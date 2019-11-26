// Copyright (c) 2019 The DAML Authors. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.platform.sandbox.auth

import org.scalatest.Assertion

import scala.concurrent.Future

trait ReadOnlyServiceCallAuthTests extends ServiceCallWithMainActorAuthTests {

  /**
    * Allows to override what is regarded as a successful response, e.g. lookup queries for
    * commands can return a NOT_FOUND, which is fine because the result is not PERMISSION_DENIED
    */
  def successfulBehavior: Future[Any] => Future[Assertion] = expectSuccess(_: Future[Any])

  it should "deny calls with an expired read-only token" in {
    expectPermissionDenied(serviceCallWithToken(canReadAsMainActorExpired))
  }
  it should "allow calls with explicitly non-expired read-only token" in {
    successfulBehavior(serviceCallWithToken(canReadAsMainActorExpiresTomorrow))
  }
  it should "allow calls with implicitly non-expired read-only token" in {
    successfulBehavior(serviceCallWithToken(canReadAsMainActor))
  }

  it should "deny calls with an expired read/write token" in {
    expectPermissionDenied(serviceCallWithToken(canActAsMainActorExpired))
  }
  it should "allow calls with explicitly non-expired read/write token" in {
    successfulBehavior(serviceCallWithToken(canActAsMainActorExpiresTomorrow))
  }
  it should "allow calls with implicitly non-expired read/write token" in {
    successfulBehavior(serviceCallWithToken(canActAsMainActor))
  }

}
