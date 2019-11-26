// Copyright (c) 2019 The DAML Authors. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.platform.sandbox.auth

trait ReadWriteServiceCallAuthTests extends ServiceCallWithMainActorAuthTests {

  it should "deny calls with an expired read/write token" in {
    expectPermissionDenied(serviceCallWithToken(canActAsMainActorExpired))
  }
  it should "allow calls with explicitly non-expired read/write token" in {
    expectSuccess(serviceCallWithToken(canActAsMainActorExpiresTomorrow))
  }
  it should "allow calls with implicitly non-expired read/write token" in {
    expectSuccess(serviceCallWithToken(canActAsMainActor))
  }

  it should "deny calls with explicitly non-expired read-only token" in {
    expectPermissionDenied(serviceCallWithToken(canReadAsMainActorExpiresTomorrow))
  }
  it should "deny calls with implicitly non-expired read-only token" in {
    expectPermissionDenied(serviceCallWithToken(canReadAsMainActor))
  }

}
