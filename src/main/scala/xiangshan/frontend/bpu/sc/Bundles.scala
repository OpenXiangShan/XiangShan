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
  // NOTE: Seems ChiselDB has problem dealing with SInt, so we do not use ScEntry for scResp here
  // FIXME: is there a better way to do this?
  private def scEntryWidth = (new ScEntry).getWidth
  private def reducedPercsumWidth(numTables: Int) = Counter.width + 1 + log2Ceil(scala.math.max(numTables, 1))
  private def imliPercsumWidth = Counter.width + 1
  private def biasPercsumWidth = Counter.width + 1
  private def totalPercsumWidth = reducedPercsumWidth(NumPathTables + NumGlobalTables + NumBWTables + NumImliTable + 1)
  val scBiasLowerBits: Vec[UInt] = Vec(NumWays, UInt(BiasUseTageBitWidth.W))
  val scPred:          Vec[Bool] = Vec(NumWays, Bool())
  val tagePred:        Vec[Bool] = Vec(NumBtbResultEntries, Bool())
  val tageCtr:         Vec[UInt] = Vec(NumBtbResultEntries, UInt(TageTakenCtrWidth.W))
  val tagePredValid:   Vec[Bool] = Vec(NumBtbResultEntries, Bool())
  val useScPred:       Vec[Bool] = Vec(NumWays, Bool())
  val sumAboveThres:   Vec[Bool] = Vec(NumWays, Bool())

  // for debug
  val debug_scPathTakenVec:   Option[Vec[Bool]] = Some(Vec(NumWays, Bool()))
  val debug_scGlobalTakenVec: Option[Vec[Bool]] = Some(Vec(NumWays, Bool()))
  val debug_scBWTakenVec:     Option[Vec[Bool]] = Some(Vec(NumWays, Bool()))
  val debug_scPathRespVec: Option[Vec[Vec[UInt]]] = Some(Vec(NumPathTables, Vec(NumWays, UInt(scEntryWidth.W))))
  val debug_scGlobalRespVec: Option[Vec[Vec[UInt]]] = Some(Vec(NumGlobalTables, Vec(NumWays, UInt(scEntryWidth.W))))
  val debug_scBWRespVec: Option[Vec[Vec[UInt]]] = Some(Vec(NumBWTables, Vec(NumWays, UInt(scEntryWidth.W))))
  val debug_scPathPercsumVec: Option[Vec[UInt]] = Some(Vec(NumWays, UInt(reducedPercsumWidth(NumPathTables).W)))
  val debug_scGlobalPercsumVec: Option[Vec[UInt]] =
    Some(Vec(NumWays, UInt(reducedPercsumWidth(NumGlobalTables).W)))
  val debug_scBWPercsumVec: Option[Vec[UInt]] = Some(Vec(NumWays, UInt(reducedPercsumWidth(NumBWTables).W)))
  val debug_scImliRespVec: Option[Vec[UInt]] = Some(Vec(NumWays, UInt(scEntryWidth.W)))
  val debug_scImliTakenVec:   Option[Vec[Bool]] = Some(Vec(NumWays, Bool()))
  val debug_scImliPercsumVec: Option[Vec[UInt]] = Some(Vec(NumWays, UInt(imliPercsumWidth.W)))
  val debug_scBiasRespVec: Option[Vec[UInt]] = Some(Vec(BiasTableNumWays, UInt(scEntryWidth.W)))
  val debug_scBiasTakenVec:   Option[Vec[Bool]] = Some(Vec(NumWays, Bool()))
  val debug_scBiasPercsumVec: Option[Vec[UInt]] = Some(Vec(BiasTableNumWays, UInt(biasPercsumWidth.W)))
  val debug_scTotalPercsumVec: Option[Vec[UInt]] = Some(Vec(NumWays, UInt(totalPercsumWidth.W)))
  val debug_predPathIdx: Option[MixedVec[UInt]] =
    Some(MixedVec(PathTableInfos.map(info => UInt(log2Ceil(info.NumSets).W))))
  val debug_predGlobalIdx: Option[MixedVec[UInt]] =
    Some(MixedVec(GlobalTableInfos.map(info => UInt(log2Ceil(info.NumSets).W))))
  val debug_predBWIdx: Option[MixedVec[UInt]] =
    Some(MixedVec(BackwardTableInfos.map(info => UInt(log2Ceil(info.NumSets).W))))
  val debug_predImliIdx: Option[UInt] = Some(UInt(log2Ceil(ImliTableInfo.NumSets).W))
  val debug_predBiasIdx: Option[UInt] = Some(UInt(log2Ceil(BiasTableInfo.NumSets).W))
}

class ScTraceSignedValue(width: Int) extends Bundle {
  val negative:  Bool = Bool()
  val magnitude: UInt = UInt(width.W)
}

class ScConditionalBranchTrace(implicit p: Parameters) extends ScBundle with HasScParameters {
  private def ScEntryWidth      = (new ScEntry).getWidth
  private def PathSetIdxWidth   = PathTableInfos.map(info => log2Ceil(info.NumSets)).foldLeft(0)(scala.math.max)
  private def GlobalSetIdxWidth = GlobalTableInfos.map(info => log2Ceil(info.NumSets)).foldLeft(0)(scala.math.max)
  private def BWSetIdxWidth     = BackwardTableInfos.map(info => log2Ceil(info.NumSets)).foldLeft(0)(scala.math.max)
  private def reducedPercsumWidth(numTables: Int) = Counter.width + 1 + log2Ceil(scala.math.max(numTables, 1))
  private def ImliPercsumWidth = Counter.width + 1
  private def BiasPercsumWidth = Counter.width + 1
  private def TotalPercsumWidth = reducedPercsumWidth(NumPathTables + NumGlobalTables + NumBWTables + NumImliTable + 1)
  val startPc:     PrunedAddr = PrunedAddr(VAddrBits)
  val cfiPc:       UInt       = UInt(VAddrBits.W)
  val predSlotIdx: UInt       = UInt(log2Ceil(NumWays).W)
  val cfiWayIdx:   UInt       = UInt(log2Ceil(NumWays).W)
  val pathIdx:     Vec[UInt]  = Vec(NumPathTables, UInt(PathSetIdxWidth.W))
  val globalIdx:   Vec[UInt]  = Vec(NumGlobalTables, UInt(GlobalSetIdxWidth.W))
  val bwIdx:       Vec[UInt]  = Vec(NumBWTables, UInt(BWSetIdxWidth.W))
  val imliIdx:     UInt       = UInt(log2Ceil(ImliTableInfo.NumSets).W)
  val biasIdx:     UInt       = UInt(log2Ceil(BiasTableInfo.NumSets).W)
  val biasWayIdx:  UInt       = UInt(log2Ceil(BiasTableNumWays).W)
  // tage provider info
  val providerValid: Bool = Bool()
  val providerTaken: Bool = Bool()
  val providerCtr:   UInt = UInt(TageTakenCtrWidth.W)
  // sc resp
  val pathResp:   Vec[ScTraceSignedValue] = Vec(NumPathTables, new ScTraceSignedValue(ScEntryWidth))
  val globalResp: Vec[ScTraceSignedValue] = Vec(NumGlobalTables, new ScTraceSignedValue(ScEntryWidth))
  val bwResp:     Vec[ScTraceSignedValue] = Vec(NumBWTables, new ScTraceSignedValue(ScEntryWidth))
  val imliResp:   ScTraceSignedValue      = new ScTraceSignedValue(ScEntryWidth)
  val biasResp:   ScTraceSignedValue      = new ScTraceSignedValue(ScEntryWidth)
  val pathPercsum:   ScTraceSignedValue = new ScTraceSignedValue(reducedPercsumWidth(NumPathTables))
  val globalPercsum: ScTraceSignedValue = new ScTraceSignedValue(reducedPercsumWidth(NumGlobalTables))
  val bwPercsum:     ScTraceSignedValue = new ScTraceSignedValue(reducedPercsumWidth(NumBWTables))
  val imliPercsum:   ScTraceSignedValue = new ScTraceSignedValue(ImliPercsumWidth)
  val biasPercsum:   ScTraceSignedValue = new ScTraceSignedValue(BiasPercsumWidth)
  val totalPercsum:  ScTraceSignedValue = new ScTraceSignedValue(TotalPercsumWidth)
  // sc pred
  val sumAboveThres: Bool = Bool()
  val scPred:        Bool = Bool()
  val useSc:         Bool = Bool()

  // actual
  val actualTaken: Bool = Bool()
  val mispredict:  Bool = Bool()

  val scCorrectTageWrong:   Bool = Bool()
  val scWrongTageCorrect:   Bool = Bool()
  val scCorrectTageCorrect: Bool = Bool()
  val scWrongTageWrong:     Bool = Bool()
  val scWrong:              Bool = Bool()
  val scCorrect:            Bool = Bool()
}
