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
import xiangshan.frontend.bpu.abtb.AheadBtbResult
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
    val abtbPrediction: Vec[Valid[AheadBtbResult]] = Input(Vec(NumAheadBtbPredictionEntries, Valid(new AheadBtbResult)))
    val overrideValid:  Bool                       = Input(Bool())
    val redirectValid:  Bool                       = Input(Bool())
  }
  val io: MicroTageIO = IO(new MicroTageIO)
  io.resetDone  := true.B
  io.trainReady := true.B

  private val a0_fire           = io.enable && io.stageCtrl.s0_fire
  private val a1_fire           = a0_fire
  private val a2_fire           = io.stageCtrl.s1_fire
  private val a0_startPc        = io.startPc
  private val a0_foldedPathHist = io.foldedPathHist
  private val a1_startPc        = io.startPc
  private val a1_foldedPathHist = io.foldedPathHist

  private val overrideValid = io.overrideValid

  /* *** submodules *** */
  private val tables = TableInfos.zipWithIndex.map {
    case (info, i) =>
      val t = Module(new MicroTageTable(
        numSets = info.NumSets,
        numWay = NumWays,
        tableId = i
      )).io
      t
  }
  private val lowTickCounter  = RegInit(0.U((LowTickWidth + 1).W))
  private val highTickCounter = RegInit(0.U((HighTickWidth + 1).W))
  private val a0_readIndex = VecInit.tabulate(NumTables) {
    i => computeHashIdx(a0_startPc, a0_foldedPathHist, TableInfos, i)
  }
  // Predict
  tables.zipWithIndex.foreach {
    case (t, idx) =>
      // Prepare for potential SRAM requirement changes.
      t.req.valid          := DontCare
      t.req.bits.readIndex := a0_readIndex(idx)
      idx match {
        case 0 => t.usefulReset := lowTickCounter(LowTickWidth)
        case 1 => t.usefulReset := highTickCounter(HighTickWidth)
        case _ => t.usefulReset := false.B
      }
  }
  private val a1_predEntries = tables.map(_.resps.readEntries)
  private val a1_readIndex   = RegEnable(a0_readIndex, a0_fire)
  private val a1_predRead    = Wire(Vec(NumTables, Vec(NumWays, new MicroTageTablePred)))
  for (i <- 0 until NumTables) {
    val predTag = computeHashTag(a1_startPc, a1_foldedPathHist, TableInfos, i)
    for (j <- 0 until NumWays) {
      a1_predRead(i)(j).taken       := a1_predEntries(i)(j).takenCtr.isPositive
      a1_predRead(i)(j).valid       := a1_predEntries(i)(j).valid
      a1_predRead(i)(j).tag         := a1_predEntries(i)(j).tag
      a1_predRead(i)(j).tagHit      := a1_predEntries(i)(j).tag === predTag
      a1_predRead(i)(j).cfiPosition := a1_predEntries(i)(j).cfiPosition
      a1_predRead(i)(j).posHit      := false.B
    }
  }

  private val a3_readIndex     = RegInit(0.U.asTypeOf(a1_readIndex))
  private val a3_predRead      = RegInit(0.U.asTypeOf(a1_predRead))
  private val overridePredRead = Wire(Vec(NumTables, Vec(NumWays, new MicroTageTablePred)))
  for (i <- 0 until NumTables) {
    val predTag = computeHashTag(a1_startPc, a1_foldedPathHist, TableInfos, i)
    for (j <- 0 until NumWays) {
      overridePredRead(i)(j).taken       := a3_predRead(i)(j).taken
      overridePredRead(i)(j).valid       := a3_predRead(i)(j).valid
      overridePredRead(i)(j).tag         := a3_predRead(i)(j).tag
      overridePredRead(i)(j).tagHit      := a3_predRead(i)(j).tag === predTag
      overridePredRead(i)(j).cfiPosition := a3_predRead(i)(j).cfiPosition
      overridePredRead(i)(j).posHit      := false.B
    }
  }

  private val a2_readIndex = RegEnable(Mux(overrideValid, a3_readIndex, a1_readIndex), a1_fire)
  private val a2_predRead =
    RegEnable(Mux(overrideValid, overridePredRead, a1_predRead), 0.U.asTypeOf(a1_predRead), a1_fire)
  private val a2_abtbHitVec     = Wire(Vec(NumAheadBtbPredictionEntries, Bool()))
  private val a2_abtbTakenVec   = Wire(Vec(NumAheadBtbPredictionEntries, Bool()))
  private val a2_abtbUseTableId = Wire(Vec(NumAheadBtbPredictionEntries, UInt(log2Ceil(NumTables).W)))
  private val a2_abtbUseWayId   = Wire(Vec(NumAheadBtbPredictionEntries, UInt(log2Ceil(NumWays).W)))
  private val a2_tableIdVec     = VecInit.tabulate(NumTables)(i => i.U)

  for (i <- 0 until NumAheadBtbPredictionEntries) {
    val tableHitVec         = Wire(Vec(NumTables, Bool()))
    val tableCfiPositionVec = Wire(Vec(NumTables, UInt(CfiPositionWidth.W)))
    val tableTakenVec       = Wire(Vec(NumTables, Bool()))
    val tableWayIdVec       = Wire(Vec(NumTables, UInt(log2Ceil(NumWays).W)))
    for (j <- 0 until NumTables) {
      val wayHitVec = Wire(Vec(NumWays, Bool()))
      for (k <- 0 until NumWays) {
        wayHitVec(k) := (a2_predRead(j)(k).valid && a2_predRead(j)(k).tagHit) &&
          a2_predRead(j)(k).cfiPosition === io.abtbPrediction(i).bits.cfiPosition
      }
      tableHitVec(j) := wayHitVec.asUInt.orR
      val priorityWayHitVec = PriorityEncoderOH(wayHitVec)
      tableCfiPositionVec(j) := Mux1H(priorityWayHitVec, a2_predRead(j).map(_.cfiPosition))
      tableTakenVec(j)       := Mux1H(priorityWayHitVec, a2_predRead(j).map(_.taken))
      tableWayIdVec(j)       := PriorityEncoder(wayHitVec)
    }
    a2_abtbHitVec(i) := tableHitVec.asUInt.orR
    // Find the hit result from the highest-priority table
    val priorityTableHitVec = PriorityEncoderOH(tableHitVec.reverse)
    a2_abtbTakenVec(i)   := Mux1H(priorityTableHitVec, tableTakenVec.reverse)
    a2_abtbUseTableId(i) := Mux1H(priorityTableHitVec, a2_tableIdVec.reverse)
    a2_abtbUseWayId(i)   := Mux1H(priorityTableHitVec, tableWayIdVec.reverse)
  }

  private val s1_predMeta = Wire(Valid(new MicroTageMeta))
  s1_predMeta.valid := a2_abtbHitVec.asUInt.orR
  s1_predMeta.bits.abtbResult := 0.U.asTypeOf(Vec(
    NumAheadBtbPredictionEntries,
    new AbtbResult
  )) // no use, only for placeholder.
  s1_predMeta.bits.readIndex := a2_readIndex
  for (i <- 0 until NumAheadBtbPredictionEntries) {
    s1_predMeta.bits.abtbResult(i).valid :=
      io.abtbPrediction(i).valid && io.abtbPrediction(i).bits.attribute.isConditional
    s1_predMeta.bits.abtbResult(i).baseTaken        := io.abtbPrediction(i).bits.taken
    s1_predMeta.bits.abtbResult(i).hit              := a2_abtbHitVec(i) && io.abtbPrediction(i).valid
    s1_predMeta.bits.abtbResult(i).predTaken        := a2_abtbTakenVec(i)
    s1_predMeta.bits.abtbResult(i).tableId          := a2_abtbUseTableId(i)
    s1_predMeta.bits.abtbResult(i).wayId            := a2_abtbUseWayId(i)
    s1_predMeta.bits.abtbResult(i).cfiPosition      := io.abtbPrediction(i).bits.cfiPosition
    s1_predMeta.bits.abtbResult(i).baseIsStrongBias := io.abtbPrediction(i).bits.isStrongBias
  }

  io.prediction.takenVec := a2_abtbTakenVec
  // May be a false hit; needs to be combined with abtbEntry's valid signal for correctness.
  // Done here for timing/layout reasons.
  io.prediction.hitVec := a2_abtbHitVec
  io.meta              := s1_predMeta

  when(a2_fire) {
    a3_predRead := a2_predRead
  }

  // ------------ MicroTage is only concerned with conditional branches ---------- //
  private val t0_fire                = io.stageCtrl.t0_fire && io.enable
  private val t0_trainMeta           = io.train.meta.utage
  private val t0_trainData           = io.train
  private val t0_trainBranch         = io.train.branches
  private val t0_abtbResult          = t0_trainMeta.abtbResult
  private val t0_trainRead           = VecInit(tables.map(_.train.t0_read))
  private val t0_trainBranchTakenVec = t0_trainBranch.map(x => x.bits.taken)

  private val t0_hasHitMisPredVec  = Wire(Vec(NumAheadBtbPredictionEntries, Bool()))
  private val t0_missHitMisPredVec = Wire(Vec(NumAheadBtbPredictionEntries, Bool()))
  private val t0_trainResult       = Wire(Vec(NumAheadBtbPredictionEntries, new MicroTageTrainResult))
  for (i <- 0 until NumAheadBtbPredictionEntries) {
    val hitMisPredVec = t0_trainBranch.map(x =>
      x.valid && t0_abtbResult(i).valid && t0_abtbResult(i).hit &&
        (x.bits.cfiPosition === t0_abtbResult(i).cfiPosition) && (x.bits.taken =/= t0_abtbResult(i).predTaken)
    )
    t0_hasHitMisPredVec(i) := hitMisPredVec.reduce(_ || _)
    val missHitMisPredVec = t0_trainBranch.map(x =>
      x.valid && t0_abtbResult(i).valid && !t0_abtbResult(i).hit &&
        (x.bits.cfiPosition === t0_abtbResult(i).cfiPosition) && (x.bits.taken =/= t0_abtbResult(i).baseTaken)
    )
    t0_missHitMisPredVec(i) := missHitMisPredVec.reduce(_ || _)
    val trainHasAbtbBranch = t0_trainBranch.map(x =>
      x.valid && t0_abtbResult(i).valid && (x.bits.cfiPosition === t0_abtbResult(i).cfiPosition)
    )
    t0_trainResult(i).valid            := trainHasAbtbBranch.reduce(_ || _)
    t0_trainResult(i).hit              := t0_abtbResult(i).hit
    t0_trainResult(i).baseTaken        := t0_abtbResult(i).baseTaken
    t0_trainResult(i).actualTaken      := Mux1H(trainHasAbtbBranch, t0_trainBranchTakenVec)
    t0_trainResult(i).predTaken        := t0_abtbResult(i).predTaken
    t0_trainResult(i).baseIsStrongBias := t0_abtbResult(i).baseIsStrongBias
    t0_trainResult(i).cfiPosition      := t0_abtbResult(i).cfiPosition
    t0_trainResult(i).tableId          := t0_abtbResult(i).tableId
    t0_trainResult(i).wayId            := t0_abtbResult(i).wayId
  }
  private val t0_trainMisPredVec = VecInit(t0_hasHitMisPredVec.zip(t0_missHitMisPredVec).map {
    case (hasHitMisPred, missHitMisPred) => hasHitMisPred || missHitMisPred
  })
  private val t0_abtbPosition     = VecInit(t0_abtbResult.map(_.cfiPosition))
  private val t0_compareMatrix    = CompareMatrix(t0_abtbPosition)
  private val t0_firstMisPredOH   = t0_compareMatrix.getLeastElementOH(t0_trainMisPredVec)
  private val t0_misPredAbtbEntry = Mux1H(t0_firstMisPredOH, t0_abtbResult)
  private val t0_allocTaken =
    Mux(
      t0_misPredAbtbEntry.valid && t0_misPredAbtbEntry.hit,
      !t0_misPredAbtbEntry.predTaken,
      !t0_misPredAbtbEntry.baseTaken
    )
  private val t0_hasMisPred = t0_trainMisPredVec.reduce(_ || _)
  private val t0_misPredProviderOH =
    Mux(t0_misPredAbtbEntry.valid && t0_misPredAbtbEntry.hit, UIntToOH(t0_misPredAbtbEntry.tableId), 0.U)
  private val t0_trainIdx = t0_trainMeta.readIndex

  private val t1_fire                   = RegNext(t0_fire, false.B)
  private val t1_foldedPathHistForTrain = RegEnable(io.foldedPathHistForTrain, t0_fire)
  private val t1_trainRead              = RegEnable(t0_trainRead, t0_fire)
  private val t1_trainResult            = RegEnable(t0_trainResult, t0_fire)
  private val t1_misPredProviderOH      = RegEnable(t0_misPredProviderOH, t0_fire)
  private val t1_needAlloc              = RegEnable(t0_hasMisPred, t0_fire)
  private val t1_allocTaken             = RegEnable(t0_allocTaken, t0_fire)
  private val t1_allocCfiPosition       = RegEnable(t0_misPredAbtbEntry.cfiPosition, t0_fire)
  // Select entries eligible for allocation
  private val t1_keepUseMask = Wire(Vec(NumTables, Bool()))
  for (i <- 0 until NumTables) {
    t1_keepUseMask(i) := t1_trainRead(i).map(x => x.valid && x.useful =/= 0.U).reduce(_ && _)
  }
  private val t1_lowerFillMask =
    Mux(t1_misPredProviderOH === 0.U, 0.U, t1_misPredProviderOH | (t1_misPredProviderOH - 1.U))
  private val t1_allocCandidateMask = ~(t1_lowerFillMask | t1_keepUseMask.asUInt)
  private val t1_normalAllocMask    = PriorityEncoderOH(t1_allocCandidateMask)
  private val t1_trainStartPc       = RegEnable(t0_trainData.startPc, t0_fire)

  for (i <- 0 until NumTables) {
    tables(i).train.t0_trainIndex := t0_trainIdx(i)
    val t1_trainTag = computeHashTag(t1_trainStartPc, t1_foldedPathHistForTrain, TableInfos, i)
    for (j <- 0 until NumWays) {
      val predCfiPosition = t1_trainRead(i)(j).cfiPosition
      // Use cfiPosition as an additional check to ensure the entry being updated
      // hasn't been evicted or overwritten during the update window.
      val entryHitVec = t1_trainResult.map(x =>
        x.valid && x.hit && (x.tableId === i.U) && (x.wayId === j.U) && (x.cfiPosition === predCfiPosition)
      )
      val entryBaseTakenVec   = t1_trainResult.map(_.baseTaken)
      val entryStrongBiasVec  = t1_trainResult.map(_.baseIsStrongBias)
      val entryPredTakenVec   = t1_trainResult.map(_.predTaken)
      val entryActualTakenVec = t1_trainResult.map(_.actualTaken)
      val select              = entryHitVec.reduce(_ || _)
      val entryHitOH          = PriorityEncoderOH(entryHitVec)
      val predTaken           = Mux1H(entryHitOH, entryPredTakenVec)
      val baseTaken           = Mux1H(entryHitOH, entryBaseTakenVec)
      val baseIsStrongBias    = Mux1H(entryHitOH, entryStrongBiasVec)
      val updateTaken         = Mux1H(entryHitOH, entryActualTakenVec)
      val usefulValid = (predTaken ^ updateTaken) || // the prediction is not equal actual.
        ((baseTaken ^ predTaken) && (predTaken === updateTaken)) // ||
      // ((baseTaken === updateTaken) && baseIsStrongBias) // baseTaken can predict good.

      tables(i).train.t1_update(j).valid            := select && t1_fire
      tables(i).train.t1_update(j).bits.updateValid := select
      tables(i).train.t1_update(j).bits.updateTaken := updateTaken
      tables(i).train.t1_update(j).bits.usefulValid := usefulValid
      tables(i).train.t1_update(j).bits.needUseful  := ((predTaken === updateTaken) && (baseTaken ^ predTaken))
    }
    val canAllocWay = VecInit(t1_trainRead(i).map(x => !x.valid || (x.useful === 0.U)))
    tables(i).train.t1_alloc.valid            := t1_needAlloc && t1_fire && t1_normalAllocMask(i)
    tables(i).train.t1_alloc.bits.taken       := t1_allocTaken
    tables(i).train.t1_alloc.bits.wayMask     := PriorityEncoderOH(canAllocWay).asUInt
    tables(i).train.t1_alloc.bits.cfiPosition := t1_allocCfiPosition
    tables(i).train.t1_alloc.bits.tag         := t1_trainTag
  }

  when(lowTickCounter(LowTickWidth)) {
    lowTickCounter := 0.U
  }.elsewhen((t1_normalAllocMask === 0.U) && t1_needAlloc && t1_fire) {
    lowTickCounter := lowTickCounter + 1.U
  }

  when(highTickCounter(HighTickWidth)) {
    highTickCounter := 0.U
  }.elsewhen((t1_normalAllocMask === 0.U) && t1_needAlloc && t1_fire) {
    highTickCounter := highTickCounter + 1.U
  }

  // ==========================================================================
  // === PERF === Performance Counters Section
  // ==========================================================================
  private val t0_firstHasHitMisPredOH  = t0_compareMatrix.getLeastElementOH(t0_hasHitMisPredVec)
  private val t0_firstMissHitMisPredOH = t0_compareMatrix.getLeastElementOH(t0_missHitMisPredVec)
  private val t0_hasHitMisPredEntry    = Mux1H(t0_firstHasHitMisPredOH, t0_trainResult)
  private val t0_missHitMisPredEntry   = Mux1H(t0_firstMissHitMisPredOH, t0_trainResult)
  private val t1_hasHitMisPredEntry    = RegEnable(t0_hasHitMisPredEntry, t0_fire)
  private val t1_missHitMisPredEntry   = RegEnable(t0_missHitMisPredEntry, t0_fire)
  private val t1_hasHitMisPredVec      = RegEnable(t0_hasHitMisPredVec, t0_fire)
  private val t1_missHitMisPredVec     = RegEnable(t0_missHitMisPredVec, t0_fire)
  private val t1_useMicroTage          = t1_trainResult.map(x => x.valid && x.hit).reduce(_ || _)
  private val t1_abtbBrVec             = t1_trainResult.map(x => x.valid)
  private val t1_trainIdx              = RegEnable(t0_trainIdx, t0_fire)
  XSPerfAccumulate("use_microtage", t1_useMicroTage && t1_fire)
  XSPerfAccumulate("train_hit_mispred", (t1_hasHitMisPredVec.asUInt.orR) && t1_fire)
  XSPerfAccumulate("train_miss_hit_mispred", (t1_missHitMisPredVec.asUInt.orR) && t1_fire)

  XSPerfAccumulate(
    "total_br",
    t1_fire,
    Seq(
      ("num", true.B, PopCount(t1_trainResult.map(x => x.valid))),
      ("hit_mispred", true.B, PopCount(t1_hasHitMisPredVec)),
      ("miss_hit_mispred", true.B, PopCount(t1_missHitMisPredVec))
    )
  )

  private val utageTrace = Wire(Valid(new MicroTageTrace))
  utageTrace.valid                 := t1_fire && t1_useMicroTage
  utageTrace.bits.startVAddr       := t1_trainStartPc.toUInt
  utageTrace.bits.hasHitMisPred    := (t1_hasHitMisPredVec.asUInt.orR)
  utageTrace.bits.missHitMisPred   := (t1_missHitMisPredVec.asUInt.orR)
  utageTrace.bits.hasHitMisPredBr  := t1_hasHitMisPredEntry
  utageTrace.bits.missHitMisPredBr := t1_missHitMisPredEntry
  utageTrace.bits.setIdx           := t1_trainIdx
  utageTrace.bits.branches         := t1_trainResult

  private val utageTraceDBTables = ChiselDB.createTable(s"microTageTrace", new MicroTageTrace, EnableTraceAndDebug)
  utageTraceDBTables.log(
    data = utageTrace.bits,
    en = t1_fire && utageTrace.valid,
    clock = clock,
    reset = reset
  )
}
