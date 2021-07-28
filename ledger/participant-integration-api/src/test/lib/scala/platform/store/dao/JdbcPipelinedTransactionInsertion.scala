// Copyright (c) 2021 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.platform.store.dao

import com.daml.ledger.offset.Offset
import com.daml.ledger.participant.state.{v2 => state}
import com.daml.lf.transaction.BlindingInfo
import com.daml.platform.indexer.OffsetStep
import com.daml.platform.store.entries.LedgerEntry
import org.scalatest.AsyncTestSuite

import scala.concurrent.Future

trait JdbcPipelinedTransactionInsertion {
  self: JdbcLedgerDaoSuite with AsyncTestSuite =>

  private[dao] override def store(
      completionInfo: Option[state.CompletionInfo],
      tx: LedgerEntry.Transaction,
      offsetStep: OffsetStep,
      divulgedContracts: List[state.DivulgedContract],
      blindingInfo: Option[BlindingInfo],
  ): Future[(Offset, LedgerEntry.Transaction)] = {
    val preparedTransactionInsert =
      prepareInsert(completionInfo, tx, offsetStep, divulgedContracts, blindingInfo)
    for {
      _ <- ledgerDao.storeTransactionState(preparedTransactionInsert)
      _ <- ledgerDao.storeTransactionEvents(preparedTransactionInsert)
      _ <- ledgerDao.completeTransaction(
        completionInfo = completionInfo,
        transactionId = tx.transactionId,
        recordTime = tx.recordedAt,
        offsetStep = offsetStep,
      )
    } yield offsetStep.offset -> tx
  }
}
