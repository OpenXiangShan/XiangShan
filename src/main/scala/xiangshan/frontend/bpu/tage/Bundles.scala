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
import xiangshan.frontend.bpu.Prediction
import xiangshan.frontend.bpu.TageTableInfo
import xiangshan.frontend.bpu.WriteReqBundle
import xiangshan.frontend.bpu.counter.UnsignedSaturateCounter
import xiangshan.frontend.bpu.counter.UnsignedSaturateCounterFactory
import xiangshan.frontend.bpu.counter.UnsignedSaturateCounterInit
import xiangshan.frontend.bpu.history.phr.PhrAllFoldedHistories

object TakenCounter extends UnsignedSaturateCounterFactory {
  def width(implicit p: Parameters): Int =
    p(XSCoreParamsKey).frontendParameters.bpuParameters.tageParameters.TakenCtrWidth
}

object UsefulCounter extends UnsignedSaturateCounterFactory {
  def width(implicit p: Parameters): Int =
    p(XSCoreParamsKey).frontendParameters.bpuParameters.tageParameters.UsefulCtrWidth

  def Init(implicit p: Parameters): UnsignedSaturateCounter =
    UnsignedSaturateCounterInit(width, p(XSCoreParamsKey).frontendParameters.bpuParameters.tageParameters.UsefulCtrInitValue)
}

object UsefulResetCounter extends UnsignedSaturateCounterFactory {
  def width(implicit p: Parameters): Int =
    p(XSCoreParamsKey).frontendParameters.bpuParameters.tageParameters.UsefulResetCtrWidth
}

object UseAltOnNaCounter extends UnsignedSaturateCounterFactory {
  def width(implicit p: Parameters): Int =
    p(XSCoreParamsKey).frontendParameters.bpuParameters.tageParameters.UseAltOnNaWidth
}

class TageEntry(implicit p: Parameters) extends TageBundle {
  val valid:    Bool            = Bool()
  val tag:      UInt            = UInt(TagWidth.W)
  val takenCtr: UnsignedSaturateCounter = TakenCounter()
}

class TagePrediction(implicit p: Parameters) extends TageBundle {
  val useProvider:  Bool = Bool()
  val providerPred: Bool = Bool()
  val hasAlt:       Bool = Bool()
  val altPred:      Bool = Bool()
}

class PhrToTageIO(implicit p: Parameters) extends TageBundle {
  val foldedPathHist:         PhrAllFoldedHistories = Input(new PhrAllFoldedHistories(AllFoldedHistoryInfo))
  val foldedPathHistForTrain: PhrAllFoldedHistories = Input(new PhrAllFoldedHistories(AllFoldedHistoryInfo))
}

class MainBtbToTageIO(implicit p: Parameters) extends TageBundle {
  val result: Vec[Valid[Prediction]] = Input(Vec(NumBtbResultEntries, Valid(new Prediction)))
}

class TageToScIO(implicit p: Parameters) extends TageBundle {
  val providerTakenCtrVec: Vec[Valid[UnsignedSaturateCounter]] = Output(Vec(NumBtbResultEntries, Valid(TakenCounter())))
}

class TableReadReq(implicit p: Parameters, info: TageTableInfo) extends TageBundle {
  val setIdx:   UInt = UInt(SetIdxWidth.W)
  val bankMask: UInt = UInt(NumBanks.W)
}

class TableReadResp(implicit p: Parameters, info: TageTableInfo) extends TageBundle {
  val entries:    Vec[TageEntry]       = Vec(NumWays, new TageEntry)
  val usefulCtrs: Vec[UnsignedSaturateCounter] = Vec(NumWays, UsefulCounter())
}

class EntrySramWriteReq(implicit p: Parameters, info: TageTableInfo) extends WriteReqBundle
    with HasTageParameters {
  val setIdx:       UInt                    = UInt(SetIdxWidth.W)
  val entry:        TageEntry               = new TageEntry
  val usefulCtr:    UnsignedSaturateCounter         = UsefulCounter()
  override def tag: Option[UInt]            = Some(entry.tag)
  override def cnt: Option[UnsignedSaturateCounter] = Some(entry.takenCtr)
}

class TableWriteReq(implicit p: Parameters, info: TageTableInfo) extends TageBundle {
  val setIdx:          UInt                 = UInt(SetIdxWidth.W)
  val bankMask:        UInt                 = UInt(NumBanks.W)
  val wayMask:         UInt                 = UInt(NumWays.W)
  val actualTakenMask: Vec[Bool]            = Vec(NumWays, Bool())
  val entries:         Vec[TageEntry]       = Vec(NumWays, new TageEntry)
  val usefulCtrs:      Vec[UnsignedSaturateCounter] = Vec(NumWays, UsefulCounter())
}

class TageMetaEntry(implicit p: Parameters) extends TageBundle {
  val useProvider:       Bool            = Bool()
  val providerTableIdx:  UInt            = UInt(TableIdxWidth.W)
  val providerWayIdx:    UInt            = UInt(MaxNumWays.W)
  val providerTakenCtr:  UnsignedSaturateCounter = TakenCounter()
  val providerUsefulCtr: UnsignedSaturateCounter = UsefulCounter()
  val altOrBasePred:     Bool            = Bool()
}

class TageMeta(implicit p: Parameters) extends TageBundle {
  val entries: Vec[TageMetaEntry] = Vec(NumBtbResultEntries, new TageMetaEntry)
}

class TageFoldedHist(implicit p: Parameters, info: TageTableInfo) extends TageBundle {
  val forIdx: UInt = UInt(SetIdxWidth.W)
  val forTag: UInt = UInt(TagWidth.W)
}

class PredictTagMatchResult(implicit p: Parameters) extends TageBundle {
  val hit:          Bool            = Bool()
  val hitWayMaskOH: UInt            = UInt(MaxNumWays.W)
  val takenCtr:     UnsignedSaturateCounter = TakenCounter()
  val usefulCtr:    UnsignedSaturateCounter = UsefulCounter()
  // perf analysis only
  val hitWayMask: UInt = UInt(MaxNumWays.W)
}

class TrainTagMatchResult(implicit p: Parameters) extends TageBundle {
  val hit:          Bool            = Bool()
  val hitWayMaskOH: UInt            = UInt(MaxNumWays.W)
  val tag:          UInt            = UInt(TagWidth.W)
  val takenCtr:     UnsignedSaturateCounter = TakenCounter()
  val usefulCtr:    UnsignedSaturateCounter = UsefulCounter()
}

class TrainInfo(implicit p: Parameters) extends TageBundle {
  val valid: Bool = Bool()

  val hasProvider:          Bool            = Bool()
  val useProvider:          Bool            = Bool()
  val providerTableOH:      UInt            = UInt(NumTables.W)
  val providerWayOH:        UInt            = UInt(MaxNumWays.W)
  val providerEntry:        TageEntry       = new TageEntry
  val providerOldUsefulCtr: UnsignedSaturateCounter = UsefulCounter()
  val providerNewUsefulCtr: UnsignedSaturateCounter = UsefulCounter()

  val hasAlt:          Bool            = Bool()
  val useAlt:          Bool            = Bool()
  val altTableOH:      UInt            = UInt(NumTables.W)
  val altWayOH:        UInt            = UInt(MaxNumWays.W)
  val altEntry:        TageEntry       = new TageEntry
  val altOldUsefulCtr: UnsignedSaturateCounter = UsefulCounter()

  val needAllocate:       Bool = Bool()
  val needUpdateProvider: Bool = Bool()
  val needUpdateAlt:      Bool = Bool()

  val incUseAltOnNa: Bool = Bool()
  val decUseAltOnNa: Bool = Bool()

  val finalPred:   Bool = Bool()
  val actualTaken: Bool = Bool() // used for writeBuffer

  // perf analysis only
  val hitTableMask:     UInt = UInt(NumTables.W) // all the hit tables
  val mispredicted:     Bool = Bool()
  val newestMispredict: Bool = Bool()
}

class ConditionalBranchTrace(implicit p: Parameters) extends TageBundle {
  val isCond:  Bool = Bool()
  val mbtbHit: Bool = Bool()
  val useMeta: Bool = Bool()

  val startPc: PrunedAddr = PrunedAddr(VAddrBits)
  val cfiPc:   UInt       = UInt(VAddrBits.W)

  val hasProvider:       Bool            = Bool()
  val useProvider:       Bool            = Bool()
  val providerTableIdx:  UInt            = UInt(TableIdxWidth.W)
  val providerSetIdx:    UInt            = UInt(MaxSetIdxWidth.W)
  val providerWayIdx:    UInt            = UInt(MaxWayIdxWidth.W)
  val providerTakenCtr:  UnsignedSaturateCounter = TakenCounter()
  val providerUsefulCtr: UnsignedSaturateCounter = UsefulCounter()

  val hasAlt:       Bool            = Bool()
  val useAlt:       Bool            = Bool()
  val altTableIdx:  UInt            = UInt(TableIdxWidth.W)
  val altSetIdx:    UInt            = UInt(MaxSetIdxWidth.W)
  val altWayIdx:    UInt            = UInt(MaxWayIdxWidth.W)
  val altTakenCtr:  UnsignedSaturateCounter = TakenCounter()
  val altUsefulCtr: UnsignedSaturateCounter = UsefulCounter()

  val finalPred:   Bool = Bool()
  val actualTaken: Bool = Bool()
  val mispredict:  Bool = Bool()

  val needAllocate:     Bool = Bool()
  val allocateSuccess:  Bool = Bool()
  val allocateFailure:  Bool = Bool()
  val allocateTableIdx: UInt = UInt(TableIdxWidth.W)
  val allocateSetIdx:   UInt = UInt(MaxSetIdxWidth.W)
  val allocateWayIdx:   UInt = UInt(MaxWayIdxWidth.W)
}
