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
import xiangshan.frontend.bpu.SaturateCounter
import xiangshan.frontend.bpu.history.phr.PhrAllFoldedHistories

/**
 * This module is the implementation of the TAGE (TAgged GEometric history length predictor).
 */
class MicroTage(implicit p: Parameters) extends BasePredictor with HasMicroTageParameters with Helpers {
  class MicroTageIO(implicit p: Parameters) extends BasePredictorIO with HasFastTrainIO {
    val foldedPathHist:         PhrAllFoldedHistories      = Input(new PhrAllFoldedHistories(AllFoldedHistoryInfo))
    val foldedPathHistForTrain: PhrAllFoldedHistories      = Input(new PhrAllFoldedHistories(AllFoldedHistoryInfo))
    val prediction:             Valid[MicroTagePrediction] = Output(Valid(new MicroTagePrediction))
    val meta:                   Valid[MicroTageMeta]       = Output(Valid(new MicroTageMeta))
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
        tableId = i
      )).io
      t
  }
  private val tickCounter = RegInit(0.U((TickWidth + 1).W))
  // Predict
  tables.foreach { t =>
    t.req.startPc        := io.startPc
    t.req.foldedPathHist := io.foldedPathHist
    t.usefulReset        := tickCounter(TickWidth)
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
  private val prediction           = Wire(Valid(new MicroTagePrediction))
  private val predMeta             = Wire(Valid(new MicroTageMeta))

// As training stabilizes, counters should approach saturation.
// Using strong saturation reduces early "overconfident" predictions before maturity,
// but it's a double-edged sword: with limited capacity, entries may be evicted
// before reaching saturation—making their unsaturated states potentially useless.
// This trade-off needs empirical validation.
  prediction.valid := io.enable && histTableHitMap.reduce(_ || _) &&
    (choseTableTakenCtr.isSaturatePositive || choseTableTakenCtr.isSaturateNegative)
  // prediction.valid            := false.B
  prediction.bits.taken       := finalPredTaken && choseTableTakenCtr.isSaturatePositive
  prediction.bits.cfiPosition := finalPredCfiPosition

  predMeta.valid                        := tables.map(_.resp.valid).reduce(_ || _)
  predMeta.bits.histTableHitMap         := tables.map(_.resp.valid)
  predMeta.bits.histTableTakenMap       := tables.map(_.resp.bits.taken)
  predMeta.bits.histTableUsefulVec      := histTableUsefulVec
  predMeta.bits.histTableCfiPositionVec := histTableCfiPositionVec
  predMeta.bits.baseTaken               := false.B // no use, only for placeholder.
  predMeta.bits.baseCfiPosition         := 0.U     // no use, only for placeholder.
  io.prediction := RegEnable(prediction, 0.U.asTypeOf(Valid(new MicroTagePrediction)), io.stageCtrl.s0_fire)
  io.meta       := RegEnable(predMeta, 0.U.asTypeOf(Valid(new MicroTageMeta)), io.stageCtrl.s0_fire)

  // ------------ MicroTage is only concerned with conditional branches ---------- //
  private val t0_fire                    = io.fastTrain.get.valid && io.enable
  private val t0_trainMeta               = io.fastTrain.get.bits.utageMeta
  private val t0_trainData               = io.fastTrain.get.bits.finalPrediction
  private val t0_trainStartPc            = io.fastTrain.get.bits.startPc
  private val t0_trainOverride           = io.fastTrain.get.bits.hasOverride
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

  private val t0_baseTaken       = t0_trainMeta.baseTaken
  private val t0_baseCfiPosition = t0_trainMeta.baseCfiPosition

  private val t0_histHitMisPred = t0_predHit && (
    (!t0_trainData.attribute.isConditional && t0_predTaken) ||
      (t0_trainData.attribute.isConditional && (
        (t0_predTaken =/= t0_trainData.taken) ||
          (t0_predCfiPosition =/= t0_trainData.cfiPosition)
      ))
  )

  private val t0_histMissHitMisPred =
    !t0_predHit && t0_trainData.attribute.isConditional &&
      t0_trainData.taken && t0_fire && io.fastTrain.get.bits.hasOverride

  private val t0_misPred             = t0_histHitMisPred || t0_histMissHitMisPred
  private val t0_histTableNeedAlloc  = t0_misPred && t0_fire
  private val t0_histTableNeedUpdate = t0_predHit && t0_fire
  private val t0_updateTaken         = (t0_predCfiPosition === t0_trainData.cfiPosition) && t0_trainData.taken
  private val t0_updateCfiPosition   = t0_predCfiPosition
  private val t0_actualTaken         = t0_trainData.attribute.isConditional && t0_trainData.taken
  private val t0_actualCfiPosition =
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
  }.elsewhen((t0_allocMask === 0.U) && t0_histTableNeedAlloc && t0_fire) {
    tickCounter := tickCounter + 1.U
  }

  // ------------------------ Consistency Check Between Base Table and Hist Table Predictions ----------------------
  private val baseEQNotMatch =
    (t0_baseCfiPosition === t0_predCfiPosition) && (t0_baseTaken ^ t0_predTaken)
  private val baseLTNotMatch =
    (t0_baseCfiPosition < t0_predCfiPosition) && ((!t0_baseTaken && t0_predTaken) || t0_baseTaken)
  private val baseGTNotMatch =
    (t0_baseCfiPosition > t0_predCfiPosition) && ((!t0_baseTaken && t0_predTaken) || (t0_baseTaken && t0_predTaken))

  private val fastTrainHasPredBr = (t0_predCfiPosition === t0_trainData.cfiPosition) ||
    ((t0_predCfiPosition < t0_trainData.cfiPosition) || t0_trainData.attribute.isConditional)
  private val baseNotMatchHistPred = baseEQNotMatch || baseLTNotMatch || baseGTNotMatch

// Allocation policy:
// - On misprediction, attempt to allocate.
// - If the victim entry has useful == 0, replace it directly.

// Update policy:
// - Only update entries on hits that lie on the executed path.
// - Entries not on the executed path are not updated (outcome unknown).

// 'useful' counter update:
// - Decrement on misprediction.
// - Increment only if prediction is correct AND base table failed to predict correctly.
  tables.zipWithIndex.foreach { case (t, i) =>
    t.update.valid := t0_fire &&
      ((t0_allocMask(i) && t0_histTableNeedAlloc) || (t0_providerMask(i) && t0_histTableNeedUpdate))
    t.update.bits.allocValid  := (t0_allocMask(i) && t0_histTableNeedAlloc)
    t.update.bits.updateValid := (t0_providerMask(i) && t0_histTableNeedUpdate) && fastTrainHasPredBr
    t.update.bits.usefulValid := (t0_providerMask(i) && t0_histTableNeedUpdate) &&
      (t0_histHitMisPred || (baseNotMatchHistPred && fastTrainHasPredBr))

    t.update.bits.startPc                := io.fastTrain.get.bits.startPc
    t.update.bits.allocTaken             := t0_actualTaken
    t.update.bits.allocCfiPosition       := t0_actualCfiPosition
    t.update.bits.updateTaken            := t0_updateTaken
    t.update.bits.updateCfiPosition      := t0_updateCfiPosition
    t.update.bits.usefulCorrect          := !t0_histHitMisPred
    t.update.bits.foldedPathHistForTrain := io.foldedPathHistForTrain
  // t.update.bits.oldTakenCtr            := t0_trainMeta.hitTakenCtr
  // t.update.bits.oldUseful              := t0_trainMeta.hitUseful
  }

  /* --------------------------------------------------------------------------------------------------------------
     MicroTage Trace
     -------------------------------------------------------------------------------------------------------------- */
  private val debug_multiHit   = PopCount(t0_histTableHitMap) > 1.U
  private val debug_tableMetas = tables.map(_.trainDebug)
  private val debug_metaCases  = t0_histTableHitMap.zip(debug_tableMetas).map { case (valid, meta) => valid -> meta }
  private val debug_tableMeta  = MuxCase(0.U.asTypeOf(new MicroTageDebug), debug_metaCases.reverse)

  private val utageTrace = Wire(Valid(new MicroTageTrace))
  utageTrace.valid            := t0_fire && (t0_histTableNeedAlloc || t0_histTableNeedUpdate)
  utageTrace.bits.startVAddr  := t0_trainStartPc.toUInt
  utageTrace.bits.branchPc    := getCfiPcFromPosition(t0_trainStartPc, t0_actualCfiPosition).toUInt
  utageTrace.bits.cfiPosition := t0_actualCfiPosition
  utageTrace.bits.hit         := t0_predHit
  utageTrace.bits.misPred     := t0_histHitMisPred
  utageTrace.bits.actualTaken := t0_actualTaken
  utageTrace.bits.tableId     := debug_tableMeta.debug_tableId
  utageTrace.bits.setIdx      := debug_tableMeta.debug_idx
  utageTrace.bits.multiHit    := debug_multiHit
  utageTrace.bits.oldUseful   := debug_tableMeta.debug_useful
  utageTrace.bits.oldTakenCtr := debug_tableMeta.debug_takenCtr
  utageTrace.bits.needUpdate  := t0_histTableNeedUpdate
  utageTrace.bits.needAlloc   := t0_histTableNeedAlloc
  utageTrace.bits.allocFailed := (t0_allocMask === 0.U) && t0_histTableNeedAlloc

  private val utageTraceDBTables = ChiselDB.createTable(s"microTageTrace", new MicroTageTrace, EnableTraceAndDebug)
  utageTraceDBTables.log(
    data = utageTrace.bits,
    en = t0_fire && utageTrace.valid,
    clock = clock,
    reset = reset
  )

  // ==========================================================================
  // === PERF === Performance Counters Section
  // ==========================================================================
  // === PHR Test ===
  if (EnableTraceAndDebug) {
    predMeta.bits.debug_startVAddr.foreach(_ := io.startPc.toUInt)
    predMeta.bits.debug_useMicroTage.foreach(_ := false.B) // no use, only for placeholder.
    predMeta.bits.debug_predIdx0.foreach(_ := tables(0).debug_predIdx)
    predMeta.bits.debug_predTag0.foreach(_ := tables(0).debug_predTag)
  }

  private val trainIdx0 = debug_tableMetas(0).debug_idx
  private val trainTag0 = debug_tableMetas(0).debug_tag

  private val positionLT = t0_predCfiPosition < t0_trainData.cfiPosition
  private val positionGT = t0_predCfiPosition > t0_trainData.cfiPosition
  private val positionEQ = t0_predCfiPosition === t0_trainData.cfiPosition
  XSPerfAccumulate("train_needAlloc", t0_fire && t0_histTableNeedAlloc)
  XSPerfAccumulate("train_needUpdate", t0_fire && t0_histTableNeedUpdate)
  XSPerfAccumulate("train_histHitMisPred", t0_fire && t0_histHitMisPred)
  XSPerfAccumulate("train_histHitMisPred_LT", t0_fire && t0_histHitMisPred && positionLT)
  XSPerfAccumulate("train_histHitMisPred_GT", t0_fire && t0_histHitMisPred && positionGT)
  XSPerfAccumulate("train_histHitMisPred_EQ", t0_fire && t0_histHitMisPred && positionEQ)
  XSPerfAccumulate("train_missHit_needAlloc", t0_fire && t0_histMissHitMisPred)
  if (EnableTraceAndDebug) {
    XSPerfAccumulate(
      "train_useMicroTage_and_override_fromFastTrain",
      t0_fire && t0_trainMeta.debug_useMicroTage.get && io.fastTrain.get.bits.hasOverride
    )
    XSPerfAccumulate("train_useMicroTage_fromFastTrain", t0_fire && t0_trainMeta.debug_useMicroTage.get)
    XSPerfAccumulate("train_idx_hit", t0_fire && (t0_trainMeta.debug_predIdx0.get === trainIdx0))
    XSPerfAccumulate("train_tag_hit", t0_fire && (t0_trainMeta.debug_predTag0.get === trainTag0))
    XSPerfAccumulate("train_idx_miss", t0_fire && (t0_trainMeta.debug_predIdx0.get =/= trainIdx0))
    XSPerfAccumulate("train_tag_miss", t0_fire && (t0_trainMeta.debug_predTag0.get =/= trainTag0))
  }
}
