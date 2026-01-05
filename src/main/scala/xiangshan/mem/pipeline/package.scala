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

import chisel3._
import chisel3.util._
import scala.collection.immutable.ListMap
import utils._
import utils.EnumUtils.ChiselOHEnum
import MathUtils.IntToOH

object LoadStage extends Enumeration {
  val s0, s1, s2, s3, s4 = Value
  sealed abstract class LoadStage(val stage: Value) {
    def id: Int = stage.id
  }
  case class LoadS0() extends LoadStage(s0)
  case class LoadS1() extends LoadStage(s1)
  case class LoadS2() extends LoadStage(s2)
  case class LoadS3() extends LoadStage(s3)
  case class LoadS4() extends LoadStage(s4)
}

trait OnLoadStage {
  import LoadStage._
  implicit val s: LoadStage

  def is(sn: LoadStage): Boolean = s.id == sn.id
  def isS0: Boolean = is(LoadS0())
  def after(s1: LoadStage, s2: LoadStage): Boolean = s1.id >= s2.id
  def afterS1: Boolean = after(s, LoadS1())
  def afterS2: Boolean = after(s, LoadS2())
  def afterS3: Boolean = after(s, LoadS3())
  def lastStage: Boolean = s match {
    // case _: LoadS4 => true // TODO
    case _: LoadS4 => true
    case _ => false
  }
  def prevStage(stage: LoadStage): LoadStage = stage match {
    case LoadS0() =>
      require(false)
      LoadS0()
    case LoadS1() => LoadS0()
    case LoadS2() => LoadS1()
    case LoadS3() => LoadS2()
    case LoadS4() => LoadS3()
  }
}

object LoadEntrance extends ChiselOHEnum {
  type OHType = super.OHType

  val unalignTail = addType(name = "unalignTail")
  val replayHiPrio = addType(name = "replayHiPrio")
  val fastReplay = addType(name = "fastReplay")
  val replayLoPrio = addType(name = "replayLoPrio")
  val prefetchHiConf = addType(name = "prefetchHiConf")
  val vectorIssue = addType(name = "vectorIssue")
  val scalarIssue = addType(name = "scalarIssue")
  val prefetchLoConf = addType(name = "prefetchLoConf")

  def num = this.values.size

  def apply() = UInt(num.W)
  // def apply(): Bundle = new Record {
  //   val elements: ListMap[String, Data] = ListMap(
  //     values.toSeq.map { v => (v.getName -> Bool()) }: _*
  //   )
  // }.asInstanceOf[Bundle]

  def isUnalignTail(source: UInt): Bool = IsOneOf(source, unalignTail)
  def isReplay(source: UInt): Bool = IsOneOf(source, replayHiPrio, replayLoPrio)
  def isFastReplay(source: UInt): Bool = IsOneOf(source, fastReplay)
  def isHWPrefetch(source: UInt): Bool = IsOneOf(source, prefetchHiConf, prefetchLoConf)
  def isVectorIssue(source: UInt): Bool = IsOneOf(source, vectorIssue)
  def isScalarIssue(source: UInt): Bool = IsOneOf(source, scalarIssue)
}

class LoadAccessType extends Bundle {
  val instrType = InstrType()
  val pftType = PrefetchType() // only 
  val pftCoh = PrefetchCoh()

  import InstrType._
  def isScalar(): Bool = IsOneOf(instrType, scalar)
  def isVector(): Bool = IsOneOf(instrType, vector)
  def isPrefetch(): Bool = IsOneOf(instrType, prefetch)
  def isHwPrefetch(): Bool = isPrefetch() && PrefetchType.isHwPrefetch(pftType)
  def isSwPrefetch(): Bool = isPrefetch() && PrefetchType.isSwPrefetch(pftType)
  def isInstrPrefetch(): Bool = isPrefetch() && PrefetchType.isInstrPrefetch(pftType)
  def isDataPrefetch(): Bool = isPrefetch() && PrefetchType.isDataPrefetch(pftType)
}

object LoadAccessType {
  def apply() = new LoadAccessType
}

object InstrType extends ChiselOHEnum {
  type OHType = super.OHType

  val scalar = addType("scalar")
  val vector = addType("vector")
  val prefetch = addType("prefetch")
  def apply() = UInt(this.values.size.W)
}

object PrefetchType {
  def hw = "b0".U
  def sw = "b1".U
  def data = "b0".U
  def instr = "b1".U

  def hwData = Cat(hw, data)
  def swData = Cat(sw, data)
  def swInstr = Cat(sw, instr)

  def apply() = UInt(2.W)
  def isDataPrefetch(t: UInt): Bool = t(0) === data
  def isInstrPrefetch(t: UInt): Bool = t(0) === instr
  def isSwPrefetch(t: UInt): Bool = t(1) === sw
  def isHwPrefetch(t: UInt): Bool = t(1) === hw
}

object PrefetchCoh {
  def read = "b0".U
  def write = "b1".U
  def apply() = UInt(1.W)
}

object TlbAccessResult extends ChiselOHEnum {
  val unknown = 0
  val noQuery = addType("noQuery")
  val hit = addType("hit")
  val miss = addType("miss")

  def apply() = UInt(this.values.size.W)
  def isHit(r: UInt): Bool = IsOneOf(r, hit)
  def isMiss(r: UInt): Bool = IsOneOf(r, miss)
  def isNotMiss(r: UInt): Bool = !isMiss(r)
}