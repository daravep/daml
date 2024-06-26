// Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.canton.domain.sequencing.service

import cats.data.EitherT
import cats.syntax.either.*
import com.digitalasset.canton.ProtoDeserializationError.ProtoDeserializationFailure
import com.digitalasset.canton.domain.Domain.FailedToInitialiseDomainNode
import com.digitalasset.canton.domain.sequencing.admin.grpc.{
  InitializeSequencerRequest,
  InitializeSequencerResponse,
}
import com.digitalasset.canton.domain.sequencing.sequencer.OnboardingStateForSequencer
import com.digitalasset.canton.error.CantonError
import com.digitalasset.canton.lifecycle.FutureUnlessShutdown
import com.digitalasset.canton.logging.{NamedLoggerFactory, NamedLogging}
import com.digitalasset.canton.networking.grpc.CantonGrpcUtil.*
import com.digitalasset.canton.protocol.StaticDomainParameters
import com.digitalasset.canton.sequencer.admin.v30.SequencerInitializationServiceGrpc.SequencerInitializationService
import com.digitalasset.canton.sequencer.admin.v30.{
  InitializeSequencerFromGenesisStateRequest,
  InitializeSequencerFromGenesisStateResponse,
  InitializeSequencerFromOnboardingStateRequest,
  InitializeSequencerFromOnboardingStateResponse,
}
import com.digitalasset.canton.serialization.ProtoConverter
import com.digitalasset.canton.topology.processing.{EffectiveTime, SequencedTime}
import com.digitalasset.canton.topology.store.{
  StoredTopologyTransactionX,
  StoredTopologyTransactionsX,
}
import com.digitalasset.canton.topology.transaction.{
  SignedTopologyTransactionX,
  TopologyChangeOpX,
  TopologyMappingX,
}
import com.digitalasset.canton.tracing.{TraceContext, TraceContextGrpc}

import scala.concurrent.{ExecutionContext, Future}

class GrpcSequencerInitializationServiceX(
    handler: GrpcSequencerInitializationServiceX.Callback,
    val loggerFactory: NamedLoggerFactory,
)(implicit
    executionContext: ExecutionContext
) extends SequencerInitializationService
    with NamedLogging {

  override def initializeSequencerFromGenesisState(
      request: InitializeSequencerFromGenesisStateRequest
  ): Future[InitializeSequencerFromGenesisStateResponse] = {
    implicit val traceContext: TraceContext = TraceContextGrpc.fromGrpcContext

    val res: EitherT[Future, CantonError, InitializeSequencerFromGenesisStateResponse] = for {
      topologyState <- EitherT.fromEither[Future](
        StoredTopologyTransactionsX
          .fromTrustedByteString(request.topologySnapshot)
          .leftMap(ProtoDeserializationFailure.Wrap(_))
      )

      domainParameters <- EitherT.fromEither[Future](
        ProtoConverter
          .parseRequired(
            StaticDomainParameters.fromProtoV30,
            "domain_parameters",
            request.domainParameters,
          )
          .leftMap(ProtoDeserializationFailure.Wrap(_))
      )
      // TODO(i17940): Remove this when we have a method to distinguish between initialization during an upgrade and initialization during the bootstrap of a domain
      // reset effective time and sequenced time if we are initializing the sequencer from the beginning
      genesisState: StoredTopologyTransactionsX[TopologyChangeOpX, TopologyMappingX] =
        StoredTopologyTransactionsX[TopologyChangeOpX, TopologyMappingX](
          topologyState.result.map(stored =>
            StoredTopologyTransactionX(
              SequencedTime(SignedTopologyTransactionX.InitialTopologySequencingTime),
              EffectiveTime(SignedTopologyTransactionX.InitialTopologySequencingTime),
              stored.validUntil.map(_ =>
                EffectiveTime(SignedTopologyTransactionX.InitialTopologySequencingTime)
              ),
              stored.transaction,
            )
          )
        )

      initializeRequest = InitializeSequencerRequest(genesisState, domainParameters, None)
      result <- handler
        .initialize(initializeRequest)
        .leftMap(FailedToInitialiseDomainNode.Failure(_))
        .onShutdown(Left(FailedToInitialiseDomainNode.Shutdown())): EitherT[
        Future,
        CantonError,
        InitializeSequencerResponse,
      ]
    } yield InitializeSequencerFromGenesisStateResponse(result.replicated)
    mapErrNew(res)
  }

  override def initializeSequencerFromOnboardingState(
      request: InitializeSequencerFromOnboardingStateRequest
  ): Future[InitializeSequencerFromOnboardingStateResponse] = {
    implicit val traceContext: TraceContext = TraceContextGrpc.fromGrpcContext
    val res: EitherT[Future, CantonError, InitializeSequencerFromOnboardingStateResponse] = for {
      onboardingState <- EitherT.fromEither[Future](
        OnboardingStateForSequencer
          // according to @rgugliel-da, this is safe to do here.
          // the caller of this endpoint could get the onboarding state from various sequencers
          // and compare them for byte-for-byte equality, to increase the confidence that this
          // is safe to deserialize
          .fromTrustedByteString(request.onboardingState)
          .leftMap(ProtoDeserializationFailure.Wrap(_))
      )
      initializeRequest = InitializeSequencerRequest(
        onboardingState.topologySnapshot,
        onboardingState.staticDomainParameters,
        Some(onboardingState.sequencerSnapshot),
      )
      result <- handler
        .initialize(initializeRequest)
        .leftMap(FailedToInitialiseDomainNode.Failure(_))
        .onShutdown(Left(FailedToInitialiseDomainNode.Shutdown())): EitherT[
        Future,
        CantonError,
        InitializeSequencerResponse,
      ]
    } yield InitializeSequencerFromOnboardingStateResponse(result.replicated)
    mapErrNew(res)
  }

}

object GrpcSequencerInitializationServiceX {
  trait Callback {
    def initialize(request: InitializeSequencerRequest)(implicit
        traceContext: TraceContext
    ): EitherT[FutureUnlessShutdown, String, InitializeSequencerResponse]
  }
}
