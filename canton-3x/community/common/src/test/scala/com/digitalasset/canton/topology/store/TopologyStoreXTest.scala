// Copyright (c) 2023 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.canton.topology.store

import cats.syntax.option.*
import com.daml.nonempty.NonEmpty
import com.digitalasset.canton.data.CantonTimestamp
import com.digitalasset.canton.topology.processing.{EffectiveTime, SequencedTime}
import com.digitalasset.canton.topology.transaction.SignedTopologyTransactionX.GenericSignedTopologyTransactionX
import com.digitalasset.canton.topology.transaction.*
import com.digitalasset.canton.version.ProtocolVersion
import org.scalatest.wordspec.AsyncWordSpec

trait TopologyStoreXTest extends AsyncWordSpec with TopologyStoreXTestBase {

  val testData = new TopologyStoreXTestData(loggerFactory, executionContext)
  import testData.*

  // TODO(#14066): Test coverage is rudimentary - enough to convince ourselves that queries basically seem to work.
  //  Increase coverage.
  def topologyStore(mk: () => TopologyStoreX[TopologyStoreId]): Unit = {

    val bootstrapTransactions = StoredTopologyTransactionsX(
      Seq[
        (CantonTimestamp, (GenericSignedTopologyTransactionX, Option[CantonTimestamp]))
      ](
        ts1 -> (tx1_NSD_Proposal, ts3.some),
        ts2 -> (tx2_OTK, ts3.some),
        ts3 -> (tx3_IDD_Removal, ts3.some),
        ts3 -> (tx3_NSD, None),
        ts3 -> (tx3_PTP_Proposal, ts5.some),
        ts4 -> (tx4_USD, None),
        ts4 -> (tx4_OTK_Proposal, None),
        ts5 -> (tx5_PTP, None),
        ts5 -> (tx5_DTC, ts6.some),
        ts6 -> (tx6_DTC_Update, None),
      ).map { case (from, (tx, until)) =>
        StoredTopologyTransactionX(
          SequencedTime(from),
          EffectiveTime(from),
          until.map(EffectiveTime(_)),
          tx,
        )
      }
    )

    "topology store x" should {

      "deal with authorized transactions" when {

        "handle simple operations" in {
          val store = mk()

          for {
            _ <- update(store, ts1, add = Seq(tx1_NSD_Proposal))
            _ <- update(store, ts2, add = Seq(tx2_OTK))
            _ <- update(store, ts5, add = Seq(tx5_DTC))
            _ <- update(store, ts6, add = Seq(tx6_MDS))

            maxTs <- store.maxTimestamp()
            retrievedTx <- store.findStored(tx1_NSD_Proposal)
            txProtocolVersion <- store.findStoredForVersion(
              tx1_NSD_Proposal.transaction,
              ProtocolVersion.v30,
            )

            proposalTransactions <- inspect(
              store,
              TimeQueryX.Range(ts1.some, ts4.some),
              proposals = true,
            )
            positiveProposals <- findPositiveTransactions(store, ts6, isProposal = true)

            txByTxHash <- store.findProposalsByTxHash(
              EffectiveTime(ts1.immediateSuccessor), // increase since exclusive
              NonEmpty(Set, tx1_NSD_Proposal.transaction.hash),
            )
            txByMappingHash <- store.findTransactionsForMapping(
              EffectiveTime(ts2.immediateSuccessor), // increase since exclusive
              NonEmpty(Set, tx2_OTK.transaction.mapping.uniqueKey),
            )

            _ <- store.updateDispatchingWatermark(ts1)
            tsWatermark <- store.currentDispatchingWatermark

            _ <- update(
              store,
              ts4,
              removeMapping = Set(tx1_NSD_Proposal.transaction.mapping.uniqueKey),
            )
            removedByMappingHash <- store.findStored(tx1_NSD_Proposal)
            _ <- update(store, ts4, removeTxs = Set(tx2_OTK.transaction.hash))
            removedByTxHash <- store.findStored(tx2_OTK)

            mdsTx <- store.findFirstMediatorStateForMediator(
              tx6_MDS.transaction.mapping.active.headOption.getOrElse(fail())
            )

            dtsTx <- store.findFirstTrustCertificateForParticipant(
              tx5_DTC.transaction.mapping.participantId
            )

          } yield {
            store.dumpStoreContent()

            assert(maxTs.contains((SequencedTime(ts6), EffectiveTime(ts6))))
            retrievedTx.map(_.transaction) shouldBe Some(tx1_NSD_Proposal)
            txProtocolVersion.map(_.transaction) shouldBe Some(tx1_NSD_Proposal)

            expectTransactions(
              proposalTransactions,
              Seq(
                tx1_NSD_Proposal
              ), // only proposal transaction, TimeQueryX.Range is inclusive on both sides
            )
            expectTransactions(positiveProposals, Seq(tx1_NSD_Proposal))

            txByTxHash shouldBe Seq(tx1_NSD_Proposal)
            txByMappingHash shouldBe Seq(tx2_OTK)

            tsWatermark shouldBe Some(ts1)

            removedByMappingHash.flatMap(_.validUntil) shouldBe Some(EffectiveTime(ts4))
            removedByTxHash.flatMap(_.validUntil) shouldBe Some(EffectiveTime(ts4))

            mdsTx.map(_.transaction) shouldBe Some(tx6_MDS)

            dtsTx.map(_.transaction) shouldBe Some(tx5_DTC)
          }
        }

        "able to inspect" in {
          val store = mk()

          for {
            _ <- store.bootstrap(bootstrapTransactions)
            headStateTransactions <- inspect(store, TimeQueryX.HeadState)
            rangeBetweenTs2AndTs3Transactions <- inspect(
              store,
              TimeQueryX.Range(ts2.some, ts3.some),
            )
            snapshotAtTs3Transactions <- inspect(
              store,
              TimeQueryX.Snapshot(ts3),
            )
            unionspaceTransactions <- inspect(
              store,
              TimeQueryX.Range(ts1.some, ts4.some),
              typ = UnionspaceDefinitionX.code.some,
            )
            removalTransactions <- inspect(
              store,
              timeQuery = TimeQueryX.Range(ts1.some, ts4.some),
              op = TopologyChangeOpX.Remove.some,
            )
            idDaTransactions <- inspect(
              store,
              timeQuery = TimeQueryX.Range(ts1.some, ts4.some),
              idFilter = "da",
            )
            idNamespaceTransactions <- inspect(
              store,
              timeQuery = TimeQueryX.Range(ts1.some, ts4.some),
              idFilter = "unionspace",
              namespaceOnly = true,
            )
            bothParties <- inspectKnownParties(store, ts6)
            onlyFred <- inspectKnownParties(store, ts6, filterParty = "fr::can")
            fredFullySpecified <- inspectKnownParties(
              store,
              ts6,
              filterParty = fredOfCanton.uid.toProtoPrimitive,
              filterParticipant = participantId1.uid.toProtoPrimitive,
            )
            onlyParticipant2 <- inspectKnownParties(store, ts6, filterParticipant = "participant2")
            neitherParty <- inspectKnownParties(store, ts6, "fred::canton", "participant2")
          } yield {
            expectTransactions(
              headStateTransactions,
              Seq(tx3_NSD, tx4_USD, tx5_PTP, tx6_DTC_Update),
            )
            expectTransactions(
              rangeBetweenTs2AndTs3Transactions,
              Seq(tx2_OTK, tx3_IDD_Removal, tx3_NSD),
            )
            expectTransactions(
              snapshotAtTs3Transactions,
              Seq(tx2_OTK), // tx2 include as until is inclusive, tx3 missing as from exclusive
            )
            expectTransactions(unionspaceTransactions, Seq(tx4_USD))
            expectTransactions(removalTransactions, Seq(tx3_IDD_Removal))
            expectTransactions(idDaTransactions, Seq(tx3_IDD_Removal))
            expectTransactions(idNamespaceTransactions, Seq(tx4_USD))

            bothParties shouldBe Set(
              tx5_PTP.transaction.mapping.partyId,
              tx5_DTC.transaction.mapping.participantId.adminParty,
            )
            onlyFred shouldBe Set(tx5_PTP.transaction.mapping.partyId)
            fredFullySpecified shouldBe Set(tx5_PTP.transaction.mapping.partyId)
            onlyParticipant2 shouldBe Set(tx5_DTC.transaction.mapping.participantId.adminParty)
            neitherParty shouldBe Set.empty
          }
        }

        "able to find positive transactions" in {
          val store = mk()

          for {
            _ <- store.bootstrap(bootstrapTransactions)
            positiveTransactions <- findPositiveTransactions(store, ts6)
            positiveTransactionsExclusive <- findPositiveTransactions(
              store,
              ts5,
            )
            positiveTransactionsInclusive <- findPositiveTransactions(
              store,
              ts5,
              asOfInclusive = true,
            )
            selectiveMappingTransactions <- findPositiveTransactions(
              store,
              ts6,
              types =
                Seq(UnionspaceDefinitionX.code, OwnerToKeyMappingX.code, PartyToParticipantX.code),
            )
            uidFilterTransactions <- findPositiveTransactions(
              store,
              ts6,
              filterUid = Some(
                Seq(
                  tx5_PTP.transaction.mapping.partyId.uid,
                  tx5_DTC.transaction.mapping.participantId.uid,
                )
              ),
            )
            namespaceFilterTransactions <- findPositiveTransactions(
              store,
              ts6,
              filterNamespace = Some(
                Seq(tx4_USD.transaction.mapping.namespace, tx5_DTC.transaction.mapping.namespace)
              ),
            )

            essentialStateTransactions <- store.findEssentialStateForMember(
              tx2_OTK.transaction.mapping.member,
              asOfInclusive = ts5,
            )

            upcomingTransactions <- store.findUpcomingEffectiveChanges(asOfInclusive = ts4)

            dispatchingTransactionsAfter <- store.findDispatchingTransactionsAfter(
              timestampExclusive = ts1,
              limit = None,
            )

            onboardingTransactionUnlessShutdown <- store
              .findParticipantOnboardingTransactions(
                tx5_DTC.transaction.mapping.participantId,
                tx5_DTC.transaction.mapping.domainId,
              )
              .unwrap // FutureUnlessShutdown[_] -> Future[UnlessShutdown[_]]
          } yield {
            expectTransactions(positiveTransactions, Seq(tx3_NSD, tx4_USD, tx5_PTP, tx5_DTC))
            expectTransactions(positiveTransactionsExclusive, Seq(tx3_NSD, tx4_USD))
            expectTransactions(
              positiveTransactionsInclusive,
              positiveTransactions.result.map(_.transaction),
            )
            expectTransactions(
              selectiveMappingTransactions,
              Seq( /* tx2_OKM only valid until ts3 */ tx4_USD, tx5_PTP),
            )
            expectTransactions(uidFilterTransactions, Seq(tx5_PTP, tx5_DTC))
            expectTransactions(namespaceFilterTransactions, Seq(tx4_USD, tx5_DTC))

            // Essential state currently encompasses all transactions at the specified time
            expectTransactions(
              essentialStateTransactions,
              Seq(
                tx1_NSD_Proposal,
                tx2_OTK,
                tx3_IDD_Removal,
                tx3_NSD,
                tx3_PTP_Proposal,
                tx4_USD,
                tx4_OTK_Proposal,
                tx5_PTP,
                tx5_DTC,
              ),
            )

            upcomingTransactions shouldBe bootstrapTransactions.result.collect {
              case tx if tx.validFrom.value >= ts4 =>
                TopologyStore.Change.Other(tx.sequenced, tx.validFrom)
            }.distinct

            expectTransactions(
              dispatchingTransactionsAfter,
              Seq(
                tx2_OTK,
                tx3_IDD_Removal,
                tx3_NSD,
                tx4_USD,
                tx4_OTK_Proposal,
                tx5_PTP,
                tx5_DTC,
                tx6_DTC_Update,
              ),
            )

            onboardingTransactionUnlessShutdown.onShutdown(fail()) shouldBe Seq(
              tx4_USD,
              tx5_DTC,
              tx6_DTC_Update,
            )

          }
        }
      }
    }
  }
}