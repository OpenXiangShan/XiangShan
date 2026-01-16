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
  val setIdx:       UInt         = UInt(log2Ceil(numSets).W)
  val wayMask:      Vec[Bool]    = Vec(numWays, Bool())
  val entryVec:     Vec[ScEntry] = Vec(numWays, new ScEntry())
  override def tag: Option[UInt] = Some(this.wayMask.asUInt) // use wayMask as tag
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
  // NOTE: Seems ChiselDB has problem dealing with SInt, so we do not use ScEntry for scResp here
  // FIXME: is there a better way to do this?
  private def ScEntryWidth = (new ScEntry).getWidth
  val scPathResp:      Vec[Vec[UInt]] = Vec(NumPathTables, Vec(NumWays, UInt(ScEntryWidth.W)))
  val scGlobalResp:    Vec[Vec[UInt]] = Vec(NumGlobalTables, Vec(NumWays, UInt(ScEntryWidth.W)))
  val scBWResp:        Vec[Vec[UInt]] = Vec(NumBWTables, Vec(NumWays, UInt(ScEntryWidth.W)))
  val scImliResp:      Vec[UInt]      = Vec(NumWays, UInt(ScEntryWidth.W))
  val scBiasResp:      Vec[UInt]      = Vec(BiasTableNumWays, UInt(ScEntryWidth.W))
  val scBiasLowerBits: Vec[UInt]      = Vec(NumWays, UInt(BiasUseTageBitWidth.W))
  val scCommonHR:      CommonHREntry  = new CommonHREntry
  val scImli:          UInt           = UInt(ImliWidth.W)
  val scPred:          Vec[Bool]      = Vec(NumWays, Bool())
  val tagePred:        Vec[Bool]      = Vec(NumBtbResultEntries, Bool())
  val tagePredValid:   Vec[Bool]      = Vec(NumBtbResultEntries, Bool())
  val useScPred:       Vec[Bool]      = Vec(NumWays, Bool())
  val sumAboveThres:   Vec[Bool]      = Vec(NumWays, Bool())

  // for debug
  val debug_scPathTakenVec:   Option[Vec[Bool]] = Some(Vec(NumWays, Bool()))
  val debug_scGlobalTakenVec: Option[Vec[Bool]] = Some(Vec(NumWays, Bool()))
  val debug_scBWTakenVec:     Option[Vec[Bool]] = Some(Vec(NumWays, Bool()))
  val debug_scImliTakenVec:   Option[Vec[Bool]] = Some(Vec(NumWays, Bool()))
  val debug_scBiasTakenVec:   Option[Vec[Bool]] = Some(Vec(NumWays, Bool()))
  val debug_predPathIdx: Option[Vec[UInt]] =
    Some(Vec(NumPathTables, UInt(log2Ceil(scParameters.PathTableInfos(0).Size).W)))
  val debug_predGlobalIdx: Option[Vec[UInt]] =
    Some(Vec(NumGlobalTables, UInt(log2Ceil(scParameters.GlobalTableInfos(0).Size).W)))
  val debug_predBWIdx: Option[Vec[UInt]] =
    Some(Vec(NumBWTables, UInt(log2Ceil(scParameters.BackwardTableInfos(0).Size).W)))
  val debug_predBiasIdx: Option[UInt] = Some(UInt(log2Ceil(BiasTableSize).W))
}

class ScConditionalBranchTrace(implicit p: Parameters) extends ScBundle with HasScParameters {
  private def ScEntryWidth = (new ScEntry).getWidth
  val startPc: PrunedAddr = PrunedAddr(VAddrBits)
  val cfiPc:   UInt       = UInt(VAddrBits.W)

  val providerValid: Bool = Bool()
  val providerTaken: Bool = Bool()
  val providerCtr:   UInt = UInt(TageTakenCtrWidth.W)

  val pathResp: Vec[UInt] = Vec(NumPathTables, UInt(ScEntryWidth.W))

  val globalResp: Vec[UInt] = Vec(NumGlobalTables, UInt(ScEntryWidth.W))

  val biasResp: UInt = UInt(ScEntryWidth.W)

  val sumAboveThres: Bool = Bool()
  val scPred:        Bool = Bool()
  val useSc:         Bool = Bool()

  val scCorrectTageWrong:   Bool = Bool()
  val scWrongTageCorrect:   Bool = Bool()
  val scCorrectTageCorrect: Bool = Bool()
  val scWrongTageWrong:     Bool = Bool()
}
