// Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.lf
package language

import scala.annotation.nowarn

final case class LanguageVersion(major: LanguageMajorVersion, minor: LanguageMinorVersion) {
  def pretty: String = s"${major.pretty}.${minor.toProtoIdentifier}"

  def isDevVersion = minor.identifier == "dev"
}

object LanguageVersion {

  type Major = LanguageMajorVersion
  val Major = LanguageMajorVersion

  type Minor = LanguageMinorVersion
  val Minor = LanguageMinorVersion

  private[this] lazy val stringToVersions = (AllV1 ++ AllV2).iterator.map(v => v.pretty -> v).toMap

  def fromString(s: String): Either[String, LanguageVersion] =
    stringToVersions.get(s).toRight(s + " is not supported")

  def assertFromString(s: String): LanguageVersion = data.assertRight(fromString(s))

  implicit val Ordering: scala.Ordering[LanguageVersion] = {
    case (LanguageVersion(Major.V1, leftMinor), LanguageVersion(Major.V1, rightMinor)) =>
      Major.V1.minorVersionOrdering.compare(leftMinor, rightMinor)
    case (LanguageVersion(Major.V2, leftMinor), LanguageVersion(Major.V2, rightMinor)) =>
      Major.V2.minorVersionOrdering.compare(leftMinor, rightMinor)
    case (v1, v2) =>
      throw new IllegalArgumentException(
        s"cannot compare version ${v1.pretty} with version ${v2.pretty}"
      )
  }

  val AllV1 = Major.V1.supportedMinorVersions.map(LanguageVersion(Major.V1, _))
  val AllV2 = Major.V2.supportedMinorVersions.map(LanguageVersion(Major.V2, _))

  val List(v1_6, v1_7, v1_8, v1_11, v1_12, v1_13, v1_14, v1_15, v1_dev) = AllV1: @nowarn(
    "msg=match may not be exhaustive"
  )
  val List(v2_1, v2_dev) = AllV2: @nowarn("msg=match may not be exhaustive")

  @deprecated("use AllV2", since = "3.1.0")
  val All = AllV2

  object Features {
    val default = v2_1
    val exceptions = v2_1
    val packageUpgrades = v2_1
    val choiceFuncs = v2_dev
    val choiceAuthority = v2_dev
    val dynamicExercise = v2_dev

    /** TYPE_REP_TYCON_NAME builtin */
    val templateTypeRepToText = v2_dev

    /** Guards in interfaces */
    val extendedInterfaces = v2_dev

    /** TextMap */
    val textMap = v2_dev

    /** BigNumeric */
    val bigNumeric = v2_dev

    val scenarios = v2_dev
    val contractKeys = v2_dev

    /** Unstable, experimental features. This should stay in x.dev forever.
      * Features implemented with this flag should be moved to a separate
      * feature flag once the decision to add them permanently has been made.
      */
    val unstable = v2_dev

  }

  private[lf] def notSupported(majorLanguageVersion: LanguageMajorVersion) =
    throw new IllegalArgumentException(s"${majorLanguageVersion.pretty} not supported")

  /** All the stable versions for a given major language version.
    * Version ranges don't make sense across major language versions because major language versions
    * break backwards compatibility. Clients of [[VersionRange]] in the codebase assume that all LF
    * versions in a range are backwards compatible with the older versions within that range. Hence
    * the majorLanguageVersion parameter.
    */
  def StableVersions(majorLanguageVersion: LanguageMajorVersion): VersionRange[LanguageVersion] =
    majorLanguageVersion match {
      case Major.V2 => VersionRange(v2_1, v2_1)
      case _ => notSupported(majorLanguageVersion)
    }

  /** All the stable and preview versions for a given major language version.
    * Equals [[StableVersions(majorLanguageVersion)]] if no preview version is available.
    */
  def EarlyAccessVersions(
      majorLanguageVersion: LanguageMajorVersion
  ): VersionRange[LanguageVersion] =
    StableVersions(majorLanguageVersion)

  /** All the supported versions for a given major language version: stable, early access and dev.
    */
  def AllVersions(majorLanguageVersion: LanguageMajorVersion): VersionRange[LanguageVersion] = {
    majorLanguageVersion match {
      case Major.V2 => VersionRange(v2_1, v2_dev)
      case _ => notSupported(majorLanguageVersion)
    }
  }

  /** The Daml-LF version used by default by the compiler if it matches the
    * provided major version, the latest non-dev version with that major version
    * otherwise. This function is meant to be used in tests who want to test the
    * closest thing to the default user experience given a major version.
    */
  def defaultOrLatestStable(majorLanguageVersion: LanguageMajorVersion): LanguageVersion = {
    majorLanguageVersion match {
      case Major.V2 => v2_1
      case _ => notSupported(majorLanguageVersion)
    }
  }

  // This refers to the default output LF version in the compiler
  val default: LanguageVersion = defaultOrLatestStable(Major.V2)
}

/** Operations on [[VersionRange]] that only make sense for ranges of [[LanguageVersion]]. */
object LanguageVersionRangeOps {
  implicit class LanguageVersionRange(val range: VersionRange[LanguageVersion]) {
    def majorVersion: LanguageMajorVersion = {
      require(
        range.min.major == range.max.major,
        s"version range ${range} spans over multiple version LF versions",
      )
      range.max.major
    }
  }
}
