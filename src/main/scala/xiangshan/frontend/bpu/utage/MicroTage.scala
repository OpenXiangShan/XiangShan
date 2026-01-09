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
package xiangshan.frontend.bpu.utage

import chisel3._
import chisel3.util._
import freechips.rocketchip.util.SeqToAugmentedSeq
import org.chipsalliance.cde.config.Parameters
import scala.math.min
import utility.ChiselDB
import utility.XSPerfAccumulate
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.BasePredictor
import xiangshan.frontend.bpu.BasePredictorIO
import xiangshan.frontend.bpu.BpuTrain
import xiangshan.frontend.bpu.CompareMatrix
import xiangshan.frontend.bpu.FoldedHistoryInfo
import xiangshan.frontend.bpu.HasFastTrainIO
import xiangshan.frontend.bpu.Prediction
import xiangshan.frontend.bpu.SaturateCounter
import xiangshan.frontend.bpu.history.phr.PhrAllFoldedHistories

/**
 * This module is the implementation of the TAGE (TAgged GEometric history length predictor).
 */
class MicroTage(implicit p: Parameters) extends BasePredictor with HasMicroTageParameters with Helpers {
  class MicroTageIO(implicit p: Parameters) extends BasePredictorIO with HasFastTrainIO {
    val foldedPathHist:         PhrAllFoldedHistories = Input(new PhrAllFoldedHistories(AllFoldedHistoryInfo))
    val foldedPathHistForTrain: PhrAllFoldedHistories = Input(new PhrAllFoldedHistories(AllFoldedHistoryInfo))
    val prediction:             MicroTagePrediction   = Output(new MicroTagePrediction)
    val meta:                   Valid[MicroTageMeta]  = Output(Valid(new MicroTageMeta))
    // val ubtbPrediction:         Valid[Prediction]      = Input(Valid(new Prediction))
    val abtbPrediction: Vec[Valid[Prediction]] = Input(Vec(NumAheadBtbPredictionEntries, Valid(new Prediction)))
  }
  val io: MicroTageIO = IO(new MicroTageIO)
  io.resetDone  := true.B
  io.trainReady := true.B

  /* *** submodules *** */
  private val tables = TableInfos.zipWithIndex.map {
    case (info, i) =>
      val t = Module(new MicroTageTable(
        numSets = info.NumSets,
        histLen = info.HistoryLength,
        tagLen = info.TagWidth,
        histBitsInTag = info.HistBitsInTag,
        tableId = i,
        numWay = NumWays
      )).io
      t
  }
  private val lowTickCounter  = RegInit(0.U((LowTickWidth + 1).W))
  private val highTickCounter = RegInit(0.U((HighTickWidth + 1).W))
  // Predict
  tables.zipWithIndex.foreach {
    case (t, idx) =>
      t.req.startPc        := io.startPc
      t.req.foldedPathHist := io.foldedPathHist
      idx match {
        case 0 => t.usefulReset := lowTickCounter(LowTickWidth)
        case 1 => t.usefulReset := highTickCounter(HighTickWidth)
        case _ => t.usefulReset := false.B
      }
  }

  private val s0_predRead = Wire(Vec(NumTables, Vec(NumWays, Valid(new MicroTageTablePred))))
  s0_predRead := tables.map(_.resps)
  private val s1_predRead       = RegEnable(s0_predRead, 0.U.asTypeOf(s0_predRead), io.stageCtrl.s0_fire)
  private val s1_abtbHitVec     = Wire(Vec(NumAheadBtbPredictionEntries, Bool()))
  private val s1_abtbTakenVec   = Wire(Vec(NumAheadBtbPredictionEntries, Bool()))
  private val s1_abtbUseTableId = Wire(Vec(NumAheadBtbPredictionEntries, UInt(log2Ceil(NumTables).W)))
  private val s1_tableIdVec     = VecInit.tabulate(NumTables)(i => i.U)
  for (i <- 0 until NumAheadBtbPredictionEntries) {
    val tableHitVec         = Wire(Vec(NumWays, Bool()))
    val tableCfiPositionVec = Wire(Vec(NumTables, UInt(CfiPositionWidth.W)))
    val tableTakenVec       = Wire(Vec(NumTables, Bool()))
    for (j <- 0 until NumTables) {
      val wayHitVec = Wire(Vec(NumWays, Bool()))
      for (k <- 0 until NumWays) {
        wayHitVec(k) := s1_predRead(j)(k).valid && s1_predRead(j)(k).bits.cfiPosition === io.abtbPrediction(
          i
        ).bits.cfiPosition
      }
      tableHitVec(j) := wayHitVec.asUInt.orR
      val priorityWayHitVec = PriorityEncoderOH(wayHitVec)
      tableCfiPositionVec(j) := Mux1H(priorityWayHitVec, s1_predRead(j).map(_.bits.cfiPosition))
      tableTakenVec(j)       := Mux1H(priorityWayHitVec, s1_predRead(j).map(_.bits.taken))
    }
    s1_abtbHitVec(i) := tableHitVec.asUInt.orR
    // Find the hit result from the highest-priority table
    val priorityTableHitVec = PriorityEncoderOH(tableHitVec.reverse)
    s1_abtbTakenVec(i)   := Mux1H(priorityTableHitVec, tableTakenVec.reverse)
    s1_abtbUseTableId(i) := Mux1H(priorityTableHitVec, s1_tableIdVec.reverse)
    // (NumTables - 1.U) - PriorityEncoder(tableHitVec.reverse)
  }

  private val s1_predMeta = Wire(Valid(new MicroTageMeta))
  s1_predMeta.valid := s1_abtbHitVec.asUInt.orR
  s1_predMeta.bits.abtbResult := 0.U.asTypeOf(Vec(
    NumAheadBtbPredictionEntries,
    new AbtbResult
  )) // no use, only for placeholder.
  for (i <- 0 until NumAheadBtbPredictionEntries) {
    s1_predMeta.bits.abtbResult(i).valid       := io.abtbPrediction(i).valid
    s1_predMeta.bits.abtbResult(i).baseTaken   := io.abtbPrediction(i).bits.taken
    s1_predMeta.bits.abtbResult(i).hit         := s1_abtbHitVec(i) && io.abtbPrediction(i).valid
    s1_predMeta.bits.abtbResult(i).taken       := s1_abtbTakenVec(i)
    s1_predMeta.bits.abtbResult(i).tableId     := s1_abtbUseTableId(i)
    s1_predMeta.bits.abtbResult(i).cfiPosition := io.abtbPrediction(i).bits.cfiPosition
  }

  io.prediction.takenVec := s1_abtbTakenVec
  // May be a false hit; needs to be combined with abtbEntry's valid signal for correctness.
  // Done here for timing/layout reasons.
  io.prediction.hitVec := s1_abtbHitVec
  io.meta              := s1_predMeta

  // ------------ MicroTage is only concerned with conditional branches ---------- //
  private val t0_fire          = io.fastTrain.get.valid && io.enable
  private val t0_trainMeta     = io.fastTrain.get.bits.utageMeta
  private val t0_trainData     = io.fastTrain.get.bits.finalPrediction
  private val t0_trainStartPc  = io.fastTrain.get.bits.startPc
  private val t0_trainOverride = io.fastTrain.get.bits.hasOverride
  private val t0_trainRead     = tables.map(_.train.read)

  private val t0_abtbResult = Wire(Vec(NumAheadBtbPredictionEntries, new AbtbResult))
  t0_abtbResult := t0_trainMeta.abtbResult
  for (i <- 0 until NumAheadBtbPredictionEntries) {
    t0_abtbResult(i).valid := t0_trainMeta.abtbResult(i).valid && (t0_abtbResult(
      i
    ).cfiPosition <= t0_trainData.cfiPosition)
  }
  private val trainHitMispredVec = Wire(Vec(NumAheadBtbPredictionEntries, Bool()))
  for (i <- 0 until NumAheadBtbPredictionEntries) {
    val brTakenDiff =
      (t0_abtbResult(i).taken =/= t0_trainData.taken) && t0_trainData.attribute.isConditional &&
        (t0_trainData.cfiPosition === t0_abtbResult(i).cfiPosition)
    val brNoTakenDiff = t0_abtbResult(i).taken && !t0_trainData.attribute.isConditional
    trainHitMispredVec(i) := t0_abtbResult(i).hit && (brTakenDiff || brNoTakenDiff) && t0_abtbResult(i).valid
  }
  // Update useful and takenCtr based on misprediction when there's a hit
  private val t0_abtbPosition      = VecInit(t0_abtbResult.map(_.cfiPosition))
  private val t0_compareMatrix     = CompareMatrix(t0_abtbPosition)
  private val t0_firstHitMisPredOH = t0_compareMatrix.getLeastElementOH(trainHitMispredVec)
  private val t0_misPredAbtbEntry  = Mux1H(t0_firstHitMisPredOH, t0_abtbResult)
  // Get the TableId used in the misprediction
  private val t0_misPredProviderOH  = UIntToOH(t0_misPredAbtbEntry.tableId)
  private val t0_misPredTaken       = t0_misPredAbtbEntry.taken
  private val t0_misPredCfiPosition = t0_misPredAbtbEntry.cfiPosition
  private val t0_hasHitMisPred      = trainHitMispredVec.asUInt.orR
  // This may also need to consider cases where microTage misses but baseTable hits.
  // It's complex; will revisit later.
  private val t0_missHitMisPred =
    !t0_hasHitMisPred && t0_trainData.attribute.isConditional && t0_trainData.taken && t0_trainOverride
  private val t0_needAlloc = t0_missHitMisPred || t0_hasHitMisPred
  private val t0_allocTaken =
    Mux(t0_hasHitMisPred, !t0_misPredTaken, t0_trainData.taken && t0_trainData.attribute.isConditional)
  private val t0_allocCfiPosition =
    Mux(
      t0_hasHitMisPred,
      t0_misPredCfiPosition,
      t0_trainData.cfiPosition
    ) // Need to reconsider cfiPosition when there's no hit—e.g., what if baseTable predicted Taken earlier?
  // Select entries eligible for allocation
  private val t0_noAllocMask = Wire(Vec(NumTables, Bool()))
  for (i <- 0 until NumTables) {
    t0_noAllocMask(i) := t0_trainRead(i).map(x => x.valid && x.bits.useful =/= 0.U).reduce(_ && _)
  }
  private val lowerFillMask =
    Mux(t0_misPredProviderOH === 0.U, 0.U, t0_misPredProviderOH | (t0_misPredProviderOH - 1.U))
  private val allocCandidateMask = ~(lowerFillMask | t0_noAllocMask.asUInt)
  private val normalAllocMask    = PriorityEncoderOH(allocCandidateMask)

  for (i <- 0 until NumTables) {
    tables(i).train.startPc                := t0_trainStartPc
    tables(i).train.foldedPathHistForTrain := io.foldedPathHistForTrain
    for (j <- 0 until NumWays) {
      val predTaken         = t0_trainRead(i)(j).bits.taken
      val predCfiPosition   = t0_trainRead(i)(j).bits.cfiPosition
      val entryHitVec       = t0_abtbResult.map(x => x.valid && x.tableId === i.U && x.cfiPosition === predCfiPosition)
      val entryBaseTakenVec = t0_abtbResult.map(_.baseTaken)
      val select            = entryHitVec.reduce(_ || _)
      val entryHitOH        = PriorityEncoderOH(entryHitVec)
      val baseTaken         = Mux1H(entryHitOH, entryBaseTakenVec)
      val updateTaken       = t0_trainData.attribute.isConditional && t0_trainData.taken
      val usefulValid       = (predTaken ^ updateTaken) || ((baseTaken ^ predTaken) && (predTaken === updateTaken))

      tables(i).train.update(j).valid              := select && t0_fire
      tables(i).train.update(j).bits.updateValid   := select
      tables(i).train.update(j).bits.updateTaken   := updateTaken
      tables(i).train.update(j).bits.usefulValid   := usefulValid
      tables(i).train.update(j).bits.usefulCorrect := (predTaken === updateTaken)
    }
    val canAllocWay = VecInit(t0_trainRead(i).map(x => !x.valid || (x.bits.useful === 0.U)))
    tables(i).train.alloc.valid            := normalAllocMask(i) && t0_needAlloc && t0_fire
    tables(i).train.alloc.bits.taken       := t0_allocTaken
    tables(i).train.alloc.bits.wayMask     := PriorityEncoderOH(canAllocWay).asUInt
    tables(i).train.alloc.bits.cfiPosition := t0_allocCfiPosition
  }

  when(lowTickCounter(LowTickWidth)) {
    lowTickCounter := 0.U
  }.elsewhen((normalAllocMask === 0.U) && t0_needAlloc && t0_fire) {
    lowTickCounter := lowTickCounter + 1.U
  }

  when(highTickCounter(HighTickWidth)) {
    highTickCounter := 0.U
  }.elsewhen((normalAllocMask === 0.U) && t0_needAlloc && t0_fire) {
    highTickCounter := highTickCounter + 1.U
  }

  // ==========================================================================
  // === PERF === Performance Counters Section
  // ==========================================================================
  val trainHitImportantConditionBranchVec     = Wire(Vec(NumAheadBtbPredictionEntries, Bool()))
  val trainHitImportantConditioanMispredVec   = Wire(Vec(NumAheadBtbPredictionEntries, Bool()))
  val trainMissHitImportantConditionBranchVec = Wire(Vec(NumAheadBtbPredictionEntries, Bool()))
  for (i <- 0 until NumAheadBtbPredictionEntries) {
    trainHitImportantConditionBranchVec(i) := t0_abtbResult(i).hit && t0_trainData.attribute.isConditional &&
      (t0_trainData.cfiPosition === t0_abtbResult(i).cfiPosition) && t0_abtbResult(i).valid
    trainHitImportantConditioanMispredVec(i) := trainHitImportantConditionBranchVec(i) &&
      (t0_abtbResult(i).taken =/= t0_trainData.taken)
    trainMissHitImportantConditionBranchVec(
      i
    ) := t0_trainData.attribute.isConditional && t0_trainData.taken && t0_trainOverride &&
      (t0_trainData.cfiPosition === t0_abtbResult(i).cfiPosition) && t0_abtbResult(i).valid && !t0_abtbResult(i).hit
  }
  // Missed important conditional branch
  XSPerfAccumulate("not_hit_important_condition_branch", trainMissHitImportantConditionBranchVec.reduce(_ || _))
  // Hit important conditional branch
  XSPerfAccumulate("hit_important_condition_branch", trainHitImportantConditionBranchVec.reduce(_ || _))
  // Hit important conditional branch but mispredicted
  XSPerfAccumulate("hit_important_condition_branch_mispred", trainHitImportantConditioanMispredVec.reduce(_ || _))
}
