// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.platform.participant.util

import com.digitalasset.daml.lf.command._
import com.digitalasset.daml.lf.data.Ref.PackageId
import com.digitalasset.daml.lf.data._
import com.digitalasset.daml.lf.engine.{Error => LfError}
import com.digitalasset.daml.lf.lfpackage.Ast.Package
import com.digitalasset.daml.lf.value
import com.digitalasset.daml.lf.value.Value._

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}

object ApiToLfEngine {

  type LfValue = value.Value[AbsoluteContractId]

  type Packages = Map[PackageId, Package]

  def collectPackages(value: LfValue): Set[PackageId] =
    collectPackagesInValue(FrontStack(value), Set.empty)

  def collectPackages(cmd: Commands): Set[PackageId] =
    collectPackagesInCommand(FrontStack(cmd.commands))

  @tailrec
  private def collectPackagesInValue(
      stack: FrontStack[LfValue] = FrontStack.empty,
      acc: Set[PackageId] = Set.empty
  ): Set[PackageId] =
    stack.pop match {
      case None => acc
      case Some((value, rest)) =>
        value match {
          case ValueUnit | _: ValueBool | _: ValueInt64 | _: ValueText | _: ValueDecimal |
              _: ValueDate | _: ValueTimestamp | _: ValueParty | ValueContractId(_) | ValueOptional(
                None) =>
            acc
          case ValueList(values) =>
            collectPackagesInValue(values.toImmArray ++: rest, acc)
          case ValueOptional(Some(x)) =>
            collectPackagesInValue(x +: stack, acc)
          case ValueMap(m) =>
            collectPackagesInValue(ImmArray(m.values.toSeq) ++: rest, acc)
          case ValueRecord(tycon, fields) =>
            collectPackagesInValue(fields.map(_._2) ++: rest, tycon.fold(acc)(acc + _.packageId))
          case ValueVariant(tycon, _, arg) =>
            collectPackagesInValue(arg +: rest, tycon.fold(acc)(acc + _.packageId))
          case ValueTuple(fields) =>
            collectPackagesInValue(fields.map(_._2) ++: rest, acc)
        }
    }

  private def collectPackagesInCommand(
      stack: FrontStack[Command],
      acc: Set[PackageId] = Set.empty
  ): Set[PackageId] =
    stack.pop match {
      case None => acc
      case Some((cmd, rest)) =>
        cmd match {
          case CreateCommand(templateId, argument) =>
            collectPackagesInValue(FrontStack(argument.value), acc + templateId.packageId)
          case ExerciseCommand(
              templateId,
              _ @contractId,
              _ @choiceId,
              _ @submitter,
              argument
              ) =>
            collectPackagesInValue(FrontStack(argument.value), acc + templateId.packageId)
        }
    }

  def checkPackages(
      pkgIds: Set[PackageId],
      pcs: PackageId => Option[Package]): Either[LfError, Unit] =
    pkgIds
      .find(pkgId => pcs(pkgId).isEmpty)
      .map(pkgId => LfError(s"Could not find package ${pkgId.underlyingString}"))
      .toLeft(())

  def checkPackagesAsync(pkgIds: Set[PackageId], pcs: PackageId => Future[Option[Package]])(
      implicit ec: ExecutionContext
  ): Future[Either[LfError, Unit]] = {
    def go(pkgIds: FrontStack[PackageId]): Future[Either[LfError, Unit]] =
      pkgIds.pop match {
        case None =>
          Future.successful(Right(()))
        case Some((pkgId, rest)) =>
          pcs(pkgId).flatMap(_.fold(go(rest))(_ =>
            Future.successful(Left(LfError(s"Could not find package $pkgId")))))
      }
    go(FrontStack(pkgIds.toSeq))
  }

}
