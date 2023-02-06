// Copyright (c) 2023 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.ledger.api.benchtool.submission

import java.util.concurrent.TimeUnit

import com.daml.ledger.api.benchtool.BenchtoolSandboxFixture
import com.daml.ledger.api.benchtool.config.WorkflowConfig
import com.daml.ledger.api.benchtool.config.WorkflowConfig.FooSubmissionConfig.ApplicationId
import com.daml.ledger.api.benchtool.services.LedgerApiServices
import com.daml.ledger.api.testing.utils.SuiteResourceManagementAroundAll
import com.daml.ledger.api.v1.ledger_offset.LedgerOffset
import com.daml.ledger.client.binding
import com.daml.scalautil.Statement.discard
import com.daml.timer.Delayed
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{AppendedClues, OptionValues}
import org.scalatest.Checkpoints

import scala.concurrent.Future
import scala.concurrent.duration.Duration

class WeightedApplicationIdsAndSubmittersITSpec
    extends AsyncFlatSpec
    with BenchtoolSandboxFixture
    with SuiteResourceManagementAroundAll
    with Matchers
    with AppendedClues
    with OptionValues
    with Checkpoints {

  it should "populate participant with contracts using specified application-ids and submitters" in {
    val submissionConfig = WorkflowConfig.FooSubmissionConfig(
      numberOfInstances = 100,
      numberOfObservers = 1,
      numberOfExtraSubmitters = 3,
      uniqueParties = false,
      instanceDistribution = List(
        WorkflowConfig.FooSubmissionConfig.ContractDescription(
          template = "Foo1",
          weight = 1,
          payloadSizeBytes = 0,
        )
      ),
      nonConsumingExercises = None,
      consumingExercises = None,
      applicationIds = List(
        ApplicationId(
          applicationId = "App-1",
          weight = 90,
        ),
        ApplicationId(
          applicationId = "App-2",
          weight = 10,
        ),
      ),
    )
    for {
      (apiServices, allocatedParties, fooSubmission) <- benchtoolFooSubmissionFixture(
        submissionConfig
      )
      _ <- fooSubmission.performSubmission()
      completionsApp1 <- observeCompletions(
        parties = List(allocatedParties.signatory),
        apiServices = apiServices,
        applicationId = "App-1",
      )
      completionsApp2 <- observeCompletions(
        parties = List(allocatedParties.signatory),
        apiServices = apiServices,
        applicationId = "App-2",
      )
      completionsApp1Submitter0 <- observeCompletions(
        parties = List(allocatedParties.extraSubmitters(0)),
        apiServices = apiServices,
        applicationId = "App-1",
      )
      completionsApp1Submitter1 <- observeCompletions(
        parties = List(allocatedParties.extraSubmitters(1)),
        apiServices = apiServices,
        applicationId = "App-1",
      )
    } yield {
      val cp = new Checkpoint
      // App only filters
      cp(discard(completionsApp1.completions.size shouldBe 91))
      cp(discard(completionsApp2.completions.size shouldBe 9))
      // App and party filters
      cp(
        discard(
          completionsApp1Submitter0.completions.size shouldBe completionsApp1.completions.size
        )
      )
      cp(discard(completionsApp1Submitter0.completions.size shouldBe 91))
      cp(discard(completionsApp1Submitter1.completions.size shouldBe 9))
      cp.reportAll()
      succeed
    }
  }

  private def observeCompletions(
      parties: List[binding.Primitive.Party],
      applicationId: String,
      apiServices: LedgerApiServices,
  ): Future[ObservedCompletions] = {
    val observer = CompletionsObserver()
    Delayed.by(t = Duration(5, TimeUnit.SECONDS))(observer.cancel())
    apiServices.commandCompletionService.completions(
      config = WorkflowConfig.StreamConfig.CompletionsStreamConfig(
        name = "dummy-name",
        parties = parties.map(_.toString),
        applicationId = applicationId,
        beginOffset = Some(LedgerOffset().withBoundary(LedgerOffset.LedgerBoundary.LEDGER_BEGIN)),
        objectives = None,
        timeoutO = None,
        maxItemCount = None,
      ),
      observer = observer,
    )
  }

}
