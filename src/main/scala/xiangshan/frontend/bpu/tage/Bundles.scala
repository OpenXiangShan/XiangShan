// Copyright (c) 2024-2025 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2025 Institute of Computing Technology, Chinese Academy of Sciences
// Copyright (c) 2020-2021 Peng Cheng Laboratory
//
// XiangShan is licensed under Mulan PSL v2.
// You can use this software according to the terms and conditions of the Mulan PSL v2.
// You may obtain a copy of Mulan PSL v2 at:
//          https://license.coscl.org.cn/MulanPSL2
//
// THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
// EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
// MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
//
// See the Mulan PSL v2 for more details.

package xiangshan.frontend.bpu.tage

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.XSCoreParamsKey
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.SaturateCounter
import xiangshan.frontend.bpu.SaturateCounterFactory
import xiangshan.frontend.bpu.SaturateCounterInit
import xiangshan.frontend.bpu.TageTableInfo
import xiangshan.frontend.bpu.WriteReqBundle

object TakenCounter extends SaturateCounterFactory {
  def width(implicit p: Parameters): Int =
    p(XSCoreParamsKey).frontendParameters.bpuParameters.tageParameters.TakenCtrWidth
}

object UsefulCounter extends SaturateCounterFactory {
  def width(implicit p: Parameters): Int =
    p(XSCoreParamsKey).frontendParameters.bpuParameters.tageParameters.UsefulCtrWidth

  def Init(implicit p: Parameters): SaturateCounter =
    SaturateCounterInit(width, p(XSCoreParamsKey).frontendParameters.bpuParameters.tageParameters.UsefulCtrInitValue)
}

object UsefulResetCounter extends SaturateCounterFactory {
  def width(implicit p: Parameters): Int =
    p(XSCoreParamsKey).frontendParameters.bpuParameters.tageParameters.UsefulResetCtrWidth
}

object UseAltCounter extends SaturateCounterFactory {
  def width(implicit p: Parameters): Int =
    p(XSCoreParamsKey).frontendParameters.bpuParameters.tageParameters.UseAltCtrWidth
}

class TageEntry(implicit p: Parameters) extends TageBundle {
  val valid:    Bool            = Bool()
  val tag:      UInt            = UInt(TagWidth.W)
  val takenCtr: SaturateCounter = TakenCounter()
}

class TableReadReq(implicit p: Parameters, info: TageTableInfo) extends TageBundle {
  val setIdx:   UInt = UInt(SetIdxWidth.W)
  val bankMask: UInt = UInt(NumBanks.W)
}

class TableReadResp(implicit p: Parameters, info: TageTableInfo) extends TageBundle {
  val entries:    Vec[TageEntry]       = Vec(NumWays, new TageEntry)
  val usefulCtrs: Vec[SaturateCounter] = Vec(NumWays, UsefulCounter())
}

class EntrySramWriteReq(implicit p: Parameters, info: TageTableInfo) extends WriteReqBundle
    with HasTageParameters {
  val setIdx:         UInt                    = UInt(SetIdxWidth.W)
  val entry:          TageEntry               = new TageEntry
  val usefulCtr:      SaturateCounter         = UsefulCounter()
  override def tag:   Option[UInt]            = Some(entry.tag)
  override def cnt:   Option[SaturateCounter] = Some(entry.takenCtr)
  override def taken: Option[Bool]            = Some(entry.takenCtr.isPositive) // FIXME: use actualTaken
}

class TableWriteReq(implicit p: Parameters, info: TageTableInfo) extends TageBundle {
  val setIdx:     UInt                 = UInt(SetIdxWidth.W)
  val bankMask:   UInt                 = UInt(NumBanks.W)
  val wayMask:    UInt                 = UInt(NumWays.W)
  val entries:    Vec[TageEntry]       = Vec(NumWays, new TageEntry)
  val usefulCtrs: Vec[SaturateCounter] = Vec(NumWays, UsefulCounter())
}

class TageMeta(implicit p: Parameters) extends TageBundle {
  val debug_setIdx:  Vec[UInt] = Vec(NumTables, UInt(8.W))        // TODO: remove it
  val debug_tempTag: Vec[UInt] = Vec(NumTables, UInt(TagWidth.W)) // TODO: remove it
}

class TageFoldedHist(implicit p: Parameters, info: TageTableInfo) extends TageBundle {
  val forIdx: UInt = UInt(SetIdxWidth.W)
  val forTag: UInt = UInt(TagWidth.W)
}

class TagMatchResult(implicit p: Parameters) extends TageBundle {
  val hit:          Bool            = Bool()
  val hitWayMaskOH: UInt            = UInt(MaxNumWays.W)
  val entry:        TageEntry       = new TageEntry
  val usefulCtr:    SaturateCounter = UsefulCounter()
  // perf analysis only
  val hitWayMask: UInt = UInt(MaxNumWays.W)
}

class UpdateInfo(implicit p: Parameters) extends TageBundle {
  val valid:                Bool            = Bool()
  val providerTableOH:      UInt            = UInt(NumTables.W)
  val providerWayOH:        UInt            = UInt(MaxNumWays.W)
  val providerEntry:        TageEntry       = new TageEntry
  val providerOldUsefulCtr: SaturateCounter = UsefulCounter()
  val providerNewUsefulCtr: SaturateCounter = UsefulCounter()

  val altTableOH:      UInt            = UInt(NumTables.W)
  val altWayOH:        UInt            = UInt(MaxNumWays.W)
  val altEntry:        TageEntry       = new TageEntry
  val altOldUsefulCtr: SaturateCounter = UsefulCounter()

  val useAlt:    Bool = Bool()
  val finalPred: Bool = Bool()

  val increaseUseAlt: Bool = Bool()
  val decreaseUseAlt: Bool = Bool()

  val needAllocate:  Bool = Bool()
  val notNeedUpdate: Bool = Bool()

  // perf analysis only
  val hitTableMask: UInt = UInt(NumTables.W) // all the hit tables
  val mispredicted: Bool = Bool()
}

class ConditionalBranchTrace(implicit p: Parameters) extends TageBundle {
  val startPc: PrunedAddr = PrunedAddr(VAddrBits)
  val cfiPc:   PrunedAddr = PrunedAddr(VAddrBits)

  val hasProvider:       Bool            = Bool()
  val providerTableIdx:  UInt            = UInt(TableIdxWidth.W)
  val providerSetIdx:    UInt            = UInt(16.W)
  val providerWayIdx:    UInt            = UInt(MaxWayIdxWidth.W)
  val providerTakenCtr:  SaturateCounter = TakenCounter()
  val providerUsefulCtr: SaturateCounter = UsefulCounter()

  val hasAlt:       Bool            = Bool()
  val altTableIdx:  UInt            = UInt(TableIdxWidth.W)
  val altSetIdx:    UInt            = UInt(16.W)
  val altWayIdx:    UInt            = UInt(MaxWayIdxWidth.W)
  val altTakenCtr:  SaturateCounter = TakenCounter()
  val altUsefulCtr: SaturateCounter = UsefulCounter()

  val useAlt:         Bool = Bool()
  val finalPred:      Bool = Bool()
  val actualTaken:    Bool = Bool()
  val mispredict:     Bool = Bool()
  val allocSuccess:   Bool = Bool()
  val allocTableIdx:  UInt = UInt(TableIdxWidth.W)
  val allocateSetIdx: UInt = UInt(16.W)
  val allocWayIdx:    UInt = UInt(MaxWayIdxWidth.W)
}
