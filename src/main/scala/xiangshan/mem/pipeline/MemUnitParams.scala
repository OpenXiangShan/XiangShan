/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/
package xiangshan.mem

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import utility._
import xiangshan._
import xiangshan.mem.Bundles.LsPipelineBundle

sealed trait MemUnitType

case class Std() extends MemUnitType
case class Sta() extends MemUnitType
case class Ldu() extends MemUnitType
case class Hyu() extends MemUnitType
case class Amo() extends MemUnitType

object MemWB {
  def apply(name: String, issType: Int*): Map[String, Seq[Int]] = {
    require(issType.nonEmpty && issType.filter(_ < 0).isEmpty, "issType should be non-empty and non-negative")
    Map(name -> issType)
  }
}

case class MemUnitParams(
  name:             String        = "MemUnit",
  unitType:         MemUnitType,
  dataBits:         Int           = 128, // ignore it, don't change it
  issueParams:      Seq[MemIssueParams],
  wbParams:         Seq[Map[String, Seq[Int]]] = Seq(),
  exceptionOut:     Seq[Int]      = Seq(),
  replayCauseOut:   Seq[Int]      = Seq(),
  arbiter:          String        = "order",
  hasPrefetch:      Boolean       = false,
) {

  def isStd: Boolean = unitType == Std()

  def isSta: Boolean = unitType == Sta()

  def isLdu: Boolean = unitType == Ldu()

  def isHyu: Boolean = unitType == Hyu()

  def isAmo: Boolean = unitType == Amo()

  def numWBPorts: Int = wbParams.length
  require(wbParams.isEmpty || wbParams.flatMap(_.values).groupBy(identity).forall(_._2.size == 1),
    "Writeback port type should be unique")

  def hasTrigger: Boolean = issueParams.filter(_.trigger).length > 0

  def hasMisalign: Boolean = issueParams.filter(x => MemIssueType.isMb(x.issueType)).length > 0

  def hasAGen: Boolean = issueParams.filter(_.hasAGen).length > 0

  def hasLdExe: Boolean = isLdu || isHyu

  def hasStaExe: Boolean = isSta || isHyu

  def hasTlbQuery: Boolean = issueParams.filter(_.hasTlbQuery).length > 0

  def hasDCacheQuery: Boolean = issueParams.filter(_.hasDCacheQuery).length > 0

  def genLsPipelineSourceDecoupledBundle(implicit p: Parameters): MixedVec[DecoupledIO[LsPipelineBundle]] = {
    MixedVec(wbParams.map {
      case m => DecoupledIO(new LsPipelineBundle())
    })
  }

  def genLsPipelineSinkDecoupledBundle(implicit p: Parameters): MixedVec[DecoupledIO[LsPipelineBundle]] = {
    MixedVec(issueParams.map {
      case x => Flipped(DecoupledIO(new LsPipelineBundle()))
    })
  }

  def getWritebackPortByType(issType: Int): Option[Int] = {
    val wbIdx = wbParams.indexWhere(_.values.toSeq.head.contains(issType))
    if (wbIdx != -1) Some(wbIdx) else None
  }

  def unitTypeString: String = unitType match {
    case Std() => "Store Data"
    case Sta() => "Store Addr"
    case Ldu() => "Load"
    case Hyu() => "Hybrid"
    case Amo() => "Atomics"
    case _     => throw new IllegalArgumentException("Unknown unit type")
  }
}
