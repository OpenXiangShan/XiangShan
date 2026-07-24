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

package xiangshan.frontend.bpu.sc

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.XSCoreParamsKey
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.SaturateCounter
import xiangshan.frontend.bpu.SaturateCounterFactory
import xiangshan.frontend.bpu.SaturateCounterInit
import xiangshan.frontend.bpu.SignedSaturateCounter
import xiangshan.frontend.bpu.SignedSaturateCounterFactory
import xiangshan.frontend.bpu.WriteReqBundle
import xiangshan.frontend.bpu.history.commonhr.CommonHREntry

object Counter extends SignedSaturateCounterFactory {
  def width(implicit p: Parameters): Int =
    p(XSCoreParamsKey).frontendParameters.bpuParameters.scParameters.CtrWidth
}

object ThresholdCounter extends SaturateCounterFactory {
  def width(implicit p: Parameters): Int =
    p(XSCoreParamsKey).frontendParameters.bpuParameters.scParameters.ThresholdWidth

  def Init(implicit p: Parameters): SaturateCounter =
    SaturateCounterInit(width, p(XSCoreParamsKey).frontendParameters.bpuParameters.scParameters.ThresholdInit)
}

class ScEntry(implicit p: Parameters) extends ScBundle {
  val ctr: SignedSaturateCounter = Counter()
}

class ScTableSramWriteReq(val numSets: Int, val numWays: Int)(implicit p: Parameters) extends WriteReqBundle
    with HasScParameters {
  val setIdx:           UInt              = UInt(log2Ceil(numSets).W)
  override val wayMask: Option[Vec[Bool]] = Some(Vec(numWays, Bool()))
  override val wayData: Option[Vec[UInt]] = Some(Vec(numWays, UInt((new ScEntry).getWidth.W)))

}

class ScTableReq(val numSets: Int, val numWays: Int)(implicit p: Parameters) extends ScBundle {
  val setIdx:   UInt = UInt(log2Ceil(numSets).W)
  val bankMask: UInt = UInt(NumBanks.W)
}

class ScTableTrain(val numSets: Int, val numWays: Int)(implicit p: Parameters) extends ScBundle {
  val valid:    Bool         = Bool()
  val setIdx:   UInt         = UInt(log2Ceil(numSets).W)
  val bankMask: UInt         = UInt(NumBanks.W)
  val wayMask:  Vec[Bool]    = Vec(numWays, Bool())
  val entryVec: Vec[ScEntry] = Vec(numWays, new ScEntry())
}

class ScMeta(implicit p: Parameters) extends ScBundle with HasScParameters {
  val scBiasLowerBits: Vec[UInt] = Vec(NumWays, UInt(BiasUseTageBitWidth.W))
  val scPred:          Vec[Bool] = Vec(NumWays, Bool())
  val tagePred:        Vec[Bool] = Vec(NumBtbResultEntries, Bool())
  val tageCtr:         Vec[UInt] = Vec(NumBtbResultEntries, UInt(TageTakenCtrWidth.W))
  val tagePredValid:   Vec[Bool] = Vec(NumBtbResultEntries, Bool())
  val useScPred:       Vec[Bool] = Vec(NumWays, Bool())
  val sumAboveThres:   Vec[Bool] = Vec(NumWays, Bool())

  // for debug
  val debug_scPathTakenVec:   Option[Vec[Bool]] = Option.when(EnableScDebug)(Vec(NumWays, Bool()))
  val debug_scGlobalTakenVec: Option[Vec[Bool]] = Option.when(EnableScDebug)(Vec(NumWays, Bool()))
  val debug_scBWTakenVec:     Option[Vec[Bool]] = Option.when(EnableScDebug)(Vec(NumWays, Bool()))
  val debug_scImliTakenVec:   Option[Vec[Bool]] = Option.when(EnableScDebug)(Vec(NumWays, Bool()))
  val debug_scBiasTakenVec:   Option[Vec[Bool]] = Option.when(EnableScDebug)(Vec(NumWays, Bool()))
  val debug_predPathIdx: Option[MixedVec[UInt]] =
    Option.when(EnableScDebug)(MixedVec(PathTableInfos.map(info => UInt(log2Ceil(info.NumSets).W))))
  val debug_predGlobalIdx: Option[MixedVec[UInt]] =
    Option.when(EnableScDebug)(MixedVec(GlobalTableInfos.map(info => UInt(log2Ceil(info.NumSets).W))))
  val debug_predBWIdx: Option[MixedVec[UInt]] =
    Option.when(EnableScDebug)(MixedVec(BackwardTableInfos.map(info => UInt(log2Ceil(info.NumSets).W))))
  val debug_predImliIdx:   Option[UInt]      = Option.when(EnableScDebug)(UInt(log2Ceil(ImliTableInfo.NumSets).W))
  val debug_predBiasIdx:   Option[UInt]      = Option.when(EnableScDebug)(UInt(log2Ceil(BiasTableInfo.NumSets).W))
  val debug_totalPercsum:  Option[Vec[UInt]] = Option.when(EnableScDebug)(Vec(NumWays, UInt(ScSumWidth.W)))
  val debug_threshold:     Option[Vec[UInt]] = Option.when(EnableScDebug)(Vec(NumWays, UInt(ThresholdWidth.W)))
  val debug_commonHRValid: Option[Bool]      = Option.when(EnableScDebug)(Bool())
}

class ScConditionalBranchTrace(implicit p: Parameters) extends ScBundle with HasScParameters {
  private def ScEntryWidth = (new ScEntry).getWidth
  val startPc:     PrunedAddr = PrunedAddr(VAddrBits)
  val cfiPc:       UInt       = UInt(VAddrBits.W)
  val predSlotIdx: UInt       = UInt(log2Ceil(NumWays).W)
  val tableWayIdx: UInt       = UInt(log2Ceil(NumWays).W)

  val updateValid:    Bool = Bool()
  val trainDataValid: Bool = Bool()

  // tage provider info
  val providerValid: Bool = Bool()
  val providerTaken: Bool = Bool()
  val providerCtr:   UInt = UInt(TageTakenCtrWidth.W)

  // prediction-time context
  val predCommonHRValid: Bool      = Bool()
  val predPathSetIdx:    Vec[UInt] = Vec(NumPathTables, UInt(ScSetIdxWidth.W))
  val predGlobalSetIdx:  Vec[UInt] = Vec(NumGlobalTables, UInt(ScSetIdxWidth.W))
  val predBWSetIdx:      Vec[UInt] = Vec(NumBWTables, UInt(ScSetIdxWidth.W))
  val predImliSetIdx:    UInt      = UInt(ScSetIdxWidth.W)
  val predBiasSetIdx:    UInt      = UInt(ScSetIdxWidth.W)
  val pathPred:          Bool      = Bool()
  val globalPred:        Bool      = Bool()
  val bwPred:            Bool      = Bool()
  val imliPred:          Bool      = Bool()
  val biasPred:          Bool      = Bool()
  val totalPercsum:      UInt      = UInt(ScSumWidth.W)
  val threshold:         UInt      = UInt(ThresholdWidth.W)
  val sumAboveThres:     Bool      = Bool()
  val scPred:            Bool      = Bool()
  val useSc:             Bool      = Bool()

  // Entries are re-read at training time and are valid only when trainDataValid is set.
  val trainCommonHRValid: Bool      = Bool()
  val trainPathSetIdx:    Vec[UInt] = Vec(NumPathTables, UInt(ScSetIdxWidth.W))
  val trainGlobalSetIdx:  Vec[UInt] = Vec(NumGlobalTables, UInt(ScSetIdxWidth.W))
  val trainBWSetIdx:      Vec[UInt] = Vec(NumBWTables, UInt(ScSetIdxWidth.W))
  val trainImliSetIdx:    UInt      = UInt(ScSetIdxWidth.W)
  val trainBiasSetIdx:    UInt      = UInt(ScSetIdxWidth.W)
  val trainPathResp:      Vec[UInt] = Vec(NumPathTables, UInt(ScEntryWidth.W))
  val trainGlobalResp:    Vec[UInt] = Vec(NumGlobalTables, UInt(ScEntryWidth.W))
  val trainBWResp:        Vec[UInt] = Vec(NumBWTables, UInt(ScEntryWidth.W))
  val trainImliResp:      UInt      = UInt(ScEntryWidth.W)
  val trainBiasResp:      UInt      = UInt(ScEntryWidth.W)

  // actual
  val actualTaken:  Bool = Bool()
  val mispredict:   Bool = Bool()
  val finalPred:    Bool = Bool()
  val finalCorrect: Bool = Bool()

  val scCorrectTageWrong:   Bool = Bool()
  val scWrongTageCorrect:   Bool = Bool()
  val scCorrectTageCorrect: Bool = Bool()
  val scWrongTageWrong:     Bool = Bool()
  val scWrong:              Bool = Bool()
  val scCorrect:            Bool = Bool()
}
