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
import utility.XSPerfAccumulate
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.BasePredictor
import xiangshan.frontend.bpu.BasePredictorIO
import xiangshan.frontend.bpu.BpuTrain
import xiangshan.frontend.bpu.CompareMatrix
import xiangshan.frontend.bpu.FoldedHistoryInfo
import xiangshan.frontend.bpu.HasFastTrainIO
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
  }
  val io: MicroTageIO = IO(new MicroTageIO)
  io.resetDone := true.B
  io.train.ready := true.B

  /* *** submodules *** */
  private val tables = TableInfos.zipWithIndex.map {
    case (info, i) =>
      val t = Module(new MicroTageTable(
        numSets = info.NumSets,
        histLen = info.HistoryLength,
        tagLen = info.TagWidth,
        histBitsInTag = info.HistBitsInTag,
        tableId = i
      )).io
      t
  }
  private val tickCounter = RegInit(0.U((TickWidth + 1).W))
  // Predict
  tables.foreach { t =>
    t.req.startPc        := io.startVAddr
    t.req.foldedPathHist := io.foldedPathHist
    t.usefulReset        := tickCounter(TickWidth)
    t.usefulPenalty      := false.B
  }
  private val takenCases       = tables.reverse.map(t => t.resp.valid -> t.resp.bits.taken)
  private val cfiPositionCases = tables.reverse.map(t => t.resp.valid -> t.resp.bits.cfiPosition)
  private val usefulCase       = tables.reverse.map(t => t.resp.valid -> t.resp.bits.hitUseful)
  private val takenCtrCase     = tables.reverse.map(t => t.resp.valid -> t.resp.bits.hitTakenCtr)

  private val histTableHitMap         = tables.map(_.resp.valid)
  private val histTableTakenMap       = tables.map(_.resp.bits.taken)
  private val histTableUsefulVec      = VecInit(tables.map(_.resp.bits.useful))
  private val histTableCfiPositionVec = VecInit(tables.map(_.resp.bits.cfiPosition))
  private val choseTableTakenCtr      = MuxCase(0.U.asTypeOf(new SaturateCounter(TakenCtrWidth)), takenCtrCase)
  private val choseTableUseful        = MuxCase(0.U.asTypeOf(new SaturateCounter(UsefulWidth)), usefulCase)

  private val finalPredTaken       = MuxCase(false.B, takenCases)
  private val finalPredCfiPosition = MuxCase(0.U(CfiPositionWidth.W), cfiPositionCases)
  private val prediction           = Wire(new MicroTagePrediction)
  prediction.taken                             := false.B // finalPredTaken
  prediction.cfiPosition                       := finalPredCfiPosition
  prediction.meta.valid                        := tables.map(_.resp.valid).reduce(_ || _)
  prediction.meta.bits.histTableHitMap         := tables.map(_.resp.valid)
  prediction.meta.bits.histTableTakenMap       := tables.map(_.resp.bits.taken)
  prediction.meta.bits.histTableUsefulVec      := histTableUsefulVec
  prediction.meta.bits.histTableCfiPositionVec := histTableCfiPositionVec
  prediction.meta.bits.hitUseful               := choseTableUseful
  prediction.meta.bits.hitTakenCtr             := choseTableTakenCtr
  io.prediction := RegEnable(prediction, 0.U.asTypeOf(new MicroTagePrediction), io.stageCtrl.s0_fire)

  // ------------ MicroTage is only concerned with conditional branches ---------- //
  private val t0_trainMeta               = io.fastTrain.get.bits.utageMeta
  private val t0_trainData               = io.fastTrain.get.bits.finalPrediction
  private val t0_trainValid              = io.fastTrain.get.valid
  private val t0_histTableTakenMap       = t0_trainMeta.histTableTakenMap
  private val t0_histTableHitMap         = t0_trainMeta.histTableHitMap
  private val t0_histTableCfipositionVec = t0_trainMeta.histTableCfiPositionVec
  private val t0_takenCases = t0_histTableHitMap.zip(t0_histTableTakenMap).map { case (valid, taken) => valid -> taken }
  private val t0_cfiPositionCases = t0_histTableHitMap.zip(t0_histTableCfipositionVec).map { case (valid, position) =>
    valid -> position
  }
  private val t0_predTaken       = MuxCase(false.B, t0_takenCases.reverse)
  private val t0_predCfiPosition = MuxCase(0.U(CfiPositionWidth.W), t0_cfiPositionCases.reverse)
  private val t0_predHit         = t0_trainMeta.histTableHitMap.reduce(_ || _)

  private val t0_histHitMisPred = t0_predHit && ((!t0_trainData.attribute.isConditional && t0_predTaken) ||
    (t0_trainData.attribute.isConditional && ((t0_predTaken =/= t0_trainData.taken) || (t0_predCfiPosition =/= t0_trainData.cfiPosition))))
  private val t0_histMissHitMisPred =
    !t0_predHit && t0_trainData.attribute.isConditional && t0_trainData.taken && io.fastTrain.get.bits.hasOverride

  // 目前被选择的alt表只有
  private val t0_altHitMisPred = t0_histTableHitMap(0) && ((!t0_trainData.attribute.isConditional && t0_histTableTakenMap(0)) ||
    (t0_trainData.attribute.isConditional && ((t0_histTableTakenMap(0) =/= t0_trainData.taken) || (t0_histTableCfipositionVec(0) =/= t0_trainData.cfiPosition))))
  private val t0_isMultiHit    = t0_histTableHitMap.reduce(_ && _)
  private val t0_altUsefulPenalty = t0_altHitMisPred && t0_isMultiHit && t0_trainValid

  private val t0_misPred             = t0_histHitMisPred || t0_histMissHitMisPred
  private val t0_histTableNeedAlloc  = t0_misPred && t0_trainValid
  private val t0_histTableNeedUpdate = t0_predHit && t0_trainValid
  private val t0_updateTaken = t0_predTaken ^ t0_histHitMisPred // TODO: Perhaps more fine-grained operations are needed
  private val t0_updateCfiposition = t0_predCfiPosition
  private val t0_allocTaken        = t0_trainData.attribute.isConditional && t0_trainData.taken
  private val t0_allocCfiPosition =
    Mux(t0_trainData.attribute.isConditional, t0_trainData.cfiPosition, t0_predCfiPosition)

  private val t0_providerMask      = PriorityEncoderOH(t0_trainMeta.histTableHitMap.reverse).reverse
  private val t0_histTableNoUseful = t0_trainMeta.histTableUsefulVec.map(useful => useful === 0.U).asUInt
  private val t0_fastAllocMask     = t0_providerMask.asUInt & t0_histTableNoUseful
  private val hitMask              = t0_trainMeta.histTableHitMap.asUInt
  private val lowerFillMask        = Mux(hitMask === 0.U, 0.U, hitMask | (hitMask - 1.U))
  private val usefulMask           = t0_trainMeta.histTableUsefulVec.map(useful => useful(UsefulWidth - 1)).asUInt
  private val allocCandidateMask   = ~(lowerFillMask | usefulMask)
  private val normalAllocMask      = PriorityEncoderOH(allocCandidateMask)
  private val t0_allocMask         = Mux(t0_fastAllocMask.orR, t0_fastAllocMask, normalAllocMask)

  when(tickCounter(TickWidth)) {
    tickCounter := 0.U
  }.elsewhen((t0_allocMask === 0.U) && t0_histTableNeedAlloc && t0_trainValid) {
    tickCounter := tickCounter + 1.U
  }

// The training logic consists of two operations: updating entries and
// allocating new ones.
//
// Update behavior:
// - If train_position < table_position: entry remains unchanged.
// - If train_position === table_position: value is adjusted based on
//   prediction outcome (increment or decrement).
// - If train_position > table_position: entry remains unchanged.
//
// Allocation behavior (triggered only on misprediction):
// - A new entry is allocated to a higher-level table.
// - Target the lowest such table with an available slot (useful == 0).
// - If no slot is available, allocation fails.
//
// Update rule: To reduce noise, updates occur only when positions match.
//              The direction (inc/dec) is determined by the training result.
//
// Allocation rule: The selected entry replaces an available slot.
//                  If no free slot exists, allocation fails.
//                  Each failure is recorded; after 8 consecutive failures,
//                  all 'useful' counters are reset to 0.

  private val t0_allowAlloc = true.B
  tables.zipWithIndex.foreach { case (t, i) =>
    t.update.valid := ((t0_allocMask(i) && t0_histTableNeedAlloc && t0_allowAlloc) ||
      (t0_providerMask(i) && t0_histTableNeedUpdate)) && t0_trainValid
    t.update.bits.startPc := io.fastTrain.get.bits.startVAddr
    t.update.bits.cfiPosition := Mux(
      t0_allocMask(i) && t0_histTableNeedAlloc,
      t0_allocCfiPosition,
      t0_updateCfiposition
    )
    t.update.bits.alloc                  := t0_allocMask(i) && t0_histTableNeedAlloc
    t.update.bits.allocTaken             := t0_allocTaken
    t.update.bits.correct                := !t0_histHitMisPred
    t.update.bits.taken                  := t0_updateTaken
    t.update.bits.foldedPathHistForTrain := io.foldedPathHistForTrain
    t.update.bits.oldTakenCtr            := t0_trainMeta.hitTakenCtr
    t.update.bits.oldUseful              := t0_trainMeta.hitUseful
  }
  // tables(0).usefulPenalty   := t0_altUsefulPenalty
  // ==========================================================================
  // === PERF === Performance Counters Section
  // ==========================================================================
  // === PHR Test ===
  private val testIdxFhInfos = TableInfos.zipWithIndex.map {
    case (info, i) =>
      val t = new FoldedHistoryInfo(info.HistoryLength, min(log2Ceil(info.NumSets), info.HistoryLength))
      t
  }
  private val testTagFhInfos = TableInfos.zipWithIndex.map {
    case (info, i) =>
      val t = new FoldedHistoryInfo(info.HistoryLength, min(info.HistoryLength, info.HistBitsInTag))
      t
  }
  private val testAltTagFhInfos = TableInfos.zipWithIndex.map {
    case (info, i) =>
      val t = new FoldedHistoryInfo(info.HistoryLength, min(info.HistoryLength, info.HistBitsInTag - 1))
      t
  }

  def computeHash(startPc: UInt, allFh: PhrAllFoldedHistories, tableId: Int): (UInt, UInt) = {
    val unhashedIdx = getUnhashedIdx(startPc)
    val unhashedTag = getUnhashedTag(startPc)
    val idxFh       = allFh.getHistWithInfo(testIdxFhInfos(tableId)).foldedHist
    val tagFh       = allFh.getHistWithInfo(testTagFhInfos(tableId)).foldedHist
    val altTagFh    = allFh.getHistWithInfo(testAltTagFhInfos(tableId)).foldedHist
    val idx = if (testIdxFhInfos(tableId).FoldedLength < log2Ceil(TableInfos(tableId).NumSets)) {
      (unhashedIdx ^ Cat(idxFh, idxFh))(log2Ceil(TableInfos(tableId).NumSets) - 1, 0)
    } else {
      (unhashedIdx ^ idxFh)(log2Ceil(TableInfos(tableId).NumSets) - 1, 0)
    }
    val lowTag  = (unhashedTag ^ tagFh ^ (altTagFh << 1))(TableInfos(tableId).HistBitsInTag - 1, 0)
    val highTag = connectPcTag(unhashedIdx, tableId)
    val tag     = Cat(highTag, lowTag)(TableInfos(tableId).TagWidth - 1, 0)
    (idx, tag)
  }

  private val (s0_idxTable0, s0_tagTable0) = computeHash(io.startVAddr.toUInt, io.foldedPathHist, 0)
  private val (s0_idxTable1, s0_tagTable1) = computeHash(io.startVAddr.toUInt, io.foldedPathHist, 1)

  prediction.meta.bits.testPredIdx0 := s0_idxTable0
  prediction.meta.bits.testPredTag0 := s0_tagTable0
  prediction.meta.bits.testPredIdx1 := s0_idxTable1
  prediction.meta.bits.testPredTag1 := s0_tagTable1

  prediction.meta.bits.testPredStartAddr := io.startVAddr.toUInt

  private val (trainIdx0, trainTag0) =
    computeHash(io.fastTrain.get.bits.startVAddr.toUInt, io.foldedPathHistForTrain, 0)
  private val (trainIdx1, trainTag1) =
    computeHash(io.fastTrain.get.bits.startVAddr.toUInt, io.foldedPathHistForTrain, 1)

  XSPerfAccumulate("train_needAlloc", t0_trainValid && t0_histTableNeedAlloc)
  XSPerfAccumulate("train_needUpdate", t0_trainValid && t0_histTableNeedUpdate)
  XSPerfAccumulate("train_histHitMisPred", t0_trainValid && t0_histHitMisPred)
  XSPerfAccumulate("train_idx_hit", t0_trainValid && (t0_trainMeta.testPredIdx0 === trainIdx0))
  XSPerfAccumulate("train_tag_hit", t0_trainValid && (t0_trainMeta.testPredTag0 === trainTag0))
  XSPerfAccumulate("train_idx_miss", t0_trainValid && (t0_trainMeta.testPredIdx0 =/= trainIdx0))
  XSPerfAccumulate("train_tag_miss", t0_trainValid && (t0_trainMeta.testPredTag0 =/= trainTag0))
}
