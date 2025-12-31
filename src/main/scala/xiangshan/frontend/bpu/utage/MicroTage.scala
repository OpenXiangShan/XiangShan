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
import xiangshan.frontend.bpu.BpuMeta
import xiangshan.frontend.bpu.BpuTrain
import xiangshan.frontend.bpu.BranchInfo
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
    val foldedPathHist:         PhrAllFoldedHistories      = Input(new PhrAllFoldedHistories(AllFoldedHistoryInfo))
    val foldedPathHistForTrain: PhrAllFoldedHistories      = Input(new PhrAllFoldedHistories(AllFoldedHistoryInfo))
    val prediction:             Valid[MicroTagePrediction] = Output(Valid(new MicroTagePrediction))
    val meta:                   Valid[MicroTageMeta]       = Output(Valid(new MicroTageMeta))
    val abtbResult:             Vec[Valid[Prediction]]     = Input(Vec(NumAbtbResultEntries, Valid(new Prediction)))
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
  private val lowTickCounter  = RegInit(0.U((LowTickWidth + 1).W))
  private val highTickCounter = RegInit(0.U((HighTickWidth + 1).W))
  // Predict
  tables.zipWithIndex.map {
    case (t, idx) =>
      t.req.startPc        := io.startPc
      t.req.foldedPathHist := io.foldedPathHist
      idx match {
        case 0 => t.usefulReset := lowTickCounter(LowTickWidth)
        case 1 => t.usefulReset := highTickCounter(HighTickWidth)
        case _ => t.usefulReset := false.B
      }
  }

  private val startPcHitVec  = VecInit(tables.map(_.resp.valid))
  private val takenVec       = VecInit(tables.map(_.resp.bits.taken))
  private val cfiPositionVec = VecInit(tables.map(_.resp.bits.cfiPosition))
  private val usefulVec      = VecInit(tables.map(_.resp.bits.useful))
  private val takenCtrVec    = VecInit(tables.map(_.resp.bits.hitTakenCtr))

  private val s1_startPcHitVec  = RegEnable(startPcHitVec, 0.U.asTypeOf(Vec(NumTables, Bool())), io.stageCtrl.s0_fire)
  private val s1_takenVec       = RegEnable(takenVec, io.stageCtrl.s0_fire)
  private val s1_cfiPositionVec = RegEnable(cfiPositionVec, io.stageCtrl.s0_fire)
  private val s1_usefulVec      = RegEnable(usefulVec, io.stageCtrl.s0_fire)
  private val s1_takenCtrVec    = RegEnable(takenCtrVec, io.stageCtrl.s0_fire)
  private val s1_branches       = io.abtbResult

  private val s1_hitVec = Wire(Vec(NumTables, Vec(NumAbtbResultEntries, Bool())))
  for (i <- 0 until NumTables) {
    for (j <- 0 until NumAbtbResultEntries) {
      val hasBr = s1_branches(j).valid && s1_branches(j).bits.attribute.isConditional
      s1_hitVec(i)(j) := s1_startPcHitVec(i) && (s1_cfiPositionVec(i) === s1_branches(j).bits.cfiPosition) && hasBr
    }
  }

  private val s1_hitVecOH    = s1_hitVec.map(_.reduce(_ || _))
  private val s1_takenCases  = s1_hitVecOH.zip(s1_takenVec).map { case (hit, taken) => hit -> taken }.reverse
  private val s1_usefulCases = s1_hitVecOH.zip(s1_usefulVec).map { case (hit, useful) => hit -> useful }.reverse
  private val s1_cfiPositionCases = s1_hitVecOH.zip(s1_cfiPositionVec).map { case (hit, cfiPosition) =>
    hit -> cfiPosition
  }.reverse
  private val s1_takenCtrCases = s1_hitVecOH.zip(s1_takenCtrVec).map { case (hit, takenCtr) => hit -> takenCtr }.reverse
  private val s1_hitAbtbVecCase = s1_hitVecOH.zip(s1_hitVec).map { case (hit, hitAbtbVec) => hit -> hitAbtbVec }.reverse

  private val s1_histTableHitMap         = s1_hitVecOH
  private val s1_histTableTakenMap       = s1_hitVecOH.zip(s1_takenVec).map { case (hit, taken) => hit && taken }
  private val s1_histTableUsefulVec      = s1_usefulVec
  private val s1_histTableCfiPositionVec = s1_cfiPositionVec
  private val s1_choseTableTakenCtr      = MuxCase(0.U.asTypeOf(new SaturateCounter(TakenCtrWidth)), s1_takenCtrCases)

  private val finalPredTaken       = MuxCase(false.B, s1_takenCases)
  private val finalPredCfiPosition = MuxCase(0.U(CfiPositionWidth.W), s1_cfiPositionCases)
  private val prediction           = Wire(Valid(new MicroTagePrediction))
  private val predMeta             = Wire(Valid(new MicroTageMeta))

// As training stabilizes, counters should approach saturation.
// Using strong saturation reduces early "overconfident" predictions before maturity,
// but it's a double-edged sword: with limited capacity, entries may be evicted
// before reaching saturation—making their unsaturated states potentially useless.
// This trade-off needs empirical validation.
  prediction.valid := io.enable && s1_histTableHitMap.reduce(_ || _) &&
    (s1_choseTableTakenCtr.isSaturatePositive || s1_choseTableTakenCtr.isSaturateNegative)
  // prediction.valid            := false.B
  prediction.bits.taken       := finalPredTaken && s1_choseTableTakenCtr.isSaturatePositive
  prediction.bits.cfiPosition := finalPredCfiPosition
  prediction.bits.hitAbtbVec  := MuxCase(VecInit.fill(NumAbtbResultEntries)(false.B), s1_hitAbtbVecCase)

  predMeta.valid                        := s1_histTableHitMap.reduce(_ || _)
  predMeta.bits.histTableHitMap         := s1_histTableHitMap
  predMeta.bits.histTableTakenMap       := s1_histTableTakenMap
  predMeta.bits.histTableUsefulVec      := s1_histTableUsefulVec
  predMeta.bits.histTableCfiPositionVec := s1_histTableCfiPositionVec
  predMeta.bits.baseTaken               := false.B // no use, only for placeholder.
  predMeta.bits.baseCfiPosition         := 0.U     // no use, only for placeholder.
  predMeta.bits.finalTaken              := false.B
  predMeta.bits.finalCfiPosition        := 0.U
  predMeta.bits.finalIsBr               := false.B
  predMeta.bits.hasOverride             := false.B
  io.prediction                         := prediction
  io.meta                               := predMeta

  // ------------ MicroTage is only concerned with conditional branches ---------- //
  private val t0_fire                     = io.stageCtrl.t0_fire && io.enable
  private val t0_trainMeta                = io.train.meta.utage
  private val t0_trainData                = io.train
  private val t0_hasResolvedMisPred       = WireDefault(false.B)
  private val t0_hasPredBranch            = WireDefault(false.B)
  private val t0_resloveMispredPrediction = Wire(Valid(new BranchInfo))
  private val t0_hasPredBranchPrediction  = Wire(Valid(new BranchInfo))
  private val t0_trainPrediction          = Wire(new BranchInfo)
  private val canTrain                    = WireDefault(false.B)

  private val resloveMispredVec = VecInit(t0_trainData.branches.map {
    case branch => branch.valid && branch.bits.mispredict
  })
  private val hasPredBranchVec = VecInit(t0_trainData.branches.map {
    case branch =>
      branch.valid && (branch.bits.cfiPosition === t0_trainMeta.finalCfiPosition) && t0_trainMeta.finalTaken
  })
  t0_hasPredBranch            := hasPredBranchVec.reduce(_ || _)
  t0_hasResolvedMisPred       := resloveMispredVec.reduce(_ || _)
  t0_resloveMispredPrediction := Mux1H(resloveMispredVec, t0_trainData.branches)
  t0_hasPredBranchPrediction  := Mux1H(hasPredBranchVec, t0_trainData.branches)

  canTrain           := t0_hasPredBranch || t0_hasPredBranch
  t0_trainPrediction := Mux(t0_hasResolvedMisPred, t0_resloveMispredPrediction.bits, t0_hasPredBranchPrediction.bits)

  private val t0_trainStartPc            = t0_trainData.startPc
  private val t0_hasOverride             = t0_trainMeta.hasOverride
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
    (!t0_trainPrediction.attribute.isConditional && t0_predTaken) ||
      (t0_trainPrediction.attribute.isConditional && (
        (t0_predTaken =/= t0_trainPrediction.taken) ||
          (t0_predCfiPosition =/= t0_trainPrediction.cfiPosition)
      ))
  )

  private val t0_histMissHitMisPred =
    !t0_predHit && t0_trainPrediction.attribute.isConditional && t0_trainPrediction.taken && t0_hasOverride

  private val t0_misPred             = t0_histHitMisPred || t0_histMissHitMisPred
  private val t0_histTableNeedAlloc  = t0_misPred && t0_fire && canTrain
  private val t0_histTableNeedUpdate = t0_predHit && t0_fire && canTrain
  private val t0_updateTaken =
    (t0_predCfiPosition === t0_trainPrediction.cfiPosition) && t0_trainPrediction.taken && t0_trainPrediction.attribute.isConditional
  private val t0_updateCfiPosition = t0_predCfiPosition
  private val t0_actualTaken       = t0_trainPrediction.attribute.isConditional && t0_trainPrediction.taken
  private val t0_actualCfiPosition =
    Mux(t0_trainPrediction.attribute.isConditional, t0_trainPrediction.cfiPosition, t0_predCfiPosition)

  private val t0_providerMask      = PriorityEncoderOH(t0_trainMeta.histTableHitMap.reverse).reverse
  private val t0_histTableNoUseful = t0_trainMeta.histTableUsefulVec.map(useful => useful === 0.U).asUInt
  private val t0_fastAllocMask     = t0_providerMask.asUInt & t0_histTableNoUseful
  private val hitMask              = t0_trainMeta.histTableHitMap.asUInt
  private val lowerFillMask        = Mux(hitMask === 0.U, 0.U, hitMask | (hitMask - 1.U))
  private val usefulMask           = t0_trainMeta.histTableUsefulVec.map(useful => useful(UsefulWidth - 1)).asUInt
  private val allocCandidateMask   = ~(lowerFillMask | usefulMask)
  private val normalAllocMask      = PriorityEncoderOH(allocCandidateMask)
  private val t0_allocMask         = Mux(t0_fastAllocMask.orR, t0_fastAllocMask, normalAllocMask)

  when(lowTickCounter(LowTickWidth)) {
    lowTickCounter := 0.U
  }.elsewhen((t0_allocMask === 0.U) && t0_histTableNeedAlloc && t0_fire) {
    lowTickCounter := lowTickCounter + 1.U
  }

  when(highTickCounter(HighTickWidth)) {
    highTickCounter := 0.U
  }.elsewhen((t0_allocMask === 0.U) && t0_histTableNeedAlloc && t0_fire) {
    highTickCounter := highTickCounter + 1.U
  }

  // ------------------------ Consistency Check Between Base Table and Hist Table Predictions ----------------------
  private val baseEQNotMatch =
    (t0_baseCfiPosition === t0_predCfiPosition) && (t0_baseTaken ^ t0_predTaken)
  private val baseLTNotMatch =
    (t0_baseCfiPosition < t0_predCfiPosition) && ((!t0_baseTaken && t0_predTaken) || t0_baseTaken)
  private val baseGTNotMatch =
    (t0_baseCfiPosition > t0_predCfiPosition) && ((!t0_baseTaken && t0_predTaken) || (t0_baseTaken && t0_predTaken))

  private val fastTrainHasPredBr = (t0_predCfiPosition === t0_trainPrediction.cfiPosition) ||
    ((t0_predCfiPosition < t0_trainPrediction.cfiPosition) && !t0_trainPrediction.attribute.isConditional)
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
    t.update.valid := canTrain && t0_fire &&
      ((t0_allocMask(i) && t0_histTableNeedAlloc) || (t0_providerMask(i) && t0_histTableNeedUpdate))
    t.update.bits.allocValid  := (t0_allocMask(i) && t0_histTableNeedAlloc)
    t.update.bits.updateValid := (t0_providerMask(i) && t0_histTableNeedUpdate) && fastTrainHasPredBr
    t.update.bits.usefulValid := (t0_providerMask(i) && t0_histTableNeedUpdate) &&
      (t0_histHitMisPred || (baseNotMatchHistPred && fastTrainHasPredBr))

    t.update.bits.startPc                := t0_trainStartPc
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
  utageTrace.valid            := t0_fire && canTrain && (t0_histTableNeedAlloc || t0_histTableNeedUpdate)
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

  private val positionLT = t0_predCfiPosition < t0_trainPrediction.cfiPosition
  private val positionGT = t0_predCfiPosition > t0_trainPrediction.cfiPosition
  private val positionEQ = t0_predCfiPosition === t0_trainPrediction.cfiPosition
  XSPerfAccumulate("train_needAlloc", t0_fire && canTrain && t0_histTableNeedAlloc)
  XSPerfAccumulate("train_needUpdate", t0_fire && canTrain && t0_histTableNeedUpdate)
  XSPerfAccumulate("train_histHitMisPred", t0_fire && canTrain && t0_histHitMisPred)
  XSPerfAccumulate("train_histHitMisPred_LT", t0_fire && canTrain && t0_histHitMisPred && positionLT)
  XSPerfAccumulate("train_histHitMisPred_GT", t0_fire && canTrain && t0_histHitMisPred && positionGT)
  XSPerfAccumulate("train_histHitMisPred_EQ", t0_fire && canTrain && t0_histHitMisPred && positionEQ)
  XSPerfAccumulate("train_missHit_needAlloc", t0_fire && canTrain && t0_histMissHitMisPred)
  if (EnableTraceAndDebug) {
    XSPerfAccumulate(
      "train_useMicroTage_and_override_fromFastTrain",
      io.fastTrain.get.valid && io.fastTrain.get.bits.utageMeta.debug_useMicroTage.get && io.fastTrain.get.bits.hasOverride
    )
    XSPerfAccumulate(
      "train_useMicroTage_fromFastTrain",
      io.fastTrain.get.valid && io.fastTrain.get.bits.utageMeta.debug_useMicroTage.get
    )
    XSPerfAccumulate("train_idx_hit", t0_fire && canTrain && (t0_trainMeta.debug_predIdx0.get === trainIdx0))
    XSPerfAccumulate("train_tag_hit", t0_fire && canTrain && (t0_trainMeta.debug_predTag0.get === trainTag0))
    XSPerfAccumulate("train_idx_miss", t0_fire && canTrain && (t0_trainMeta.debug_predIdx0.get =/= trainIdx0))
    XSPerfAccumulate("train_tag_miss", t0_fire && canTrain && (t0_trainMeta.debug_predTag0.get =/= trainTag0))
  }
}
