// Copyright (c) 2024-2026 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2026 Institute of Computing Technology, Chinese Academy of Sciences
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
import utility.ParallelPriorityMux
import utility.XSPerfAccumulate
import utility.XSPerfSeqAccumulate
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.BasePredictor
import xiangshan.frontend.bpu.BasePredictorIO
import xiangshan.frontend.bpu.BpuFastTrain
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
    val prediction: MicroTagePrediction  = Output(new MicroTagePrediction)
    val meta:       Valid[MicroTageMeta] = Output(Valid(new MicroTageMeta))
    // Send ABTB position early, pipeline registers inside the module.
    // Consideration: improve routing and enhance driving capablility.
    val abtbPosVec:     Vec[UInt]                  = Input(Vec(NumAheadBtbPredictionEntries, UInt(CfiPositionWidth.W)))
    val abtbPrediction: Vec[Valid[AheadBtbResult]] = Input(Vec(NumAheadBtbPredictionEntries, Valid(new AheadBtbResult)))
    val overrideValid:  Bool                       = Input(Bool())
    val redirectValid:  Bool                       = Input(Bool())

    val normalPathHist:   PhrAllFoldedHistories = Input(new PhrAllFoldedHistories(AllFoldedHistoryInfo))
    val s1PathHist:       PhrAllFoldedHistories = Input(new PhrAllFoldedHistories(AllFoldedHistoryInfo))
    val overridePathHist: PhrAllFoldedHistories = Input(new PhrAllFoldedHistories(AllFoldedHistoryInfo))

    val s1StartPc:       PrunedAddr = Input(new PrunedAddr(VAddrBits))
    val overrideStartPc: PrunedAddr = Input(new PrunedAddr(VAddrBits))
  }
  val io: MicroTageIO = IO(new MicroTageIO)
  io.trainReady := true.B

  // Ahead pipeline implementation. Advantage: get data one cycle earlier.
  // Disadvantage: multi-position competition for the same entry.
  // Problem scenario: the same entry accessed by different branches in different cycles.
  private val a0_fire                = io.enable && io.stageCtrl.s0_fire
  private val a1_fire                = a0_fire
  private val a2_fire                = io.stageCtrl.s1_fire
  private val overrideValid          = io.overrideValid
  private val redirectValid          = io.redirectValid
  private val a0_indexPc             = io.startPc
  private val a0_indexFoldedPathHist = io.normalPathHist

  /* *** submodules *** */
  private val tables = TableInfos.zipWithIndex.map {
    case (info, i) =>
      val t = Module(new MicroTageTable(
        numSets = info.NumSets,
        tableId = i
      )).io
      t
  }
  io.sramResetDone := tables.map(_.sramResetDone).reduce(_ && _)
  // High-order tables have longer history, better discrimination, relatively stable,
  // and lower access frequency. No need to frequently clean dead entries based on useful counters.
  private val lowTickCounter  = RegInit(0.U((LowTickWidth + 1).W))
  private val highTickCounter = RegInit(0.U((HighTickWidth + 1).W))
  private val a0_readIndex = VecInit.tabulate(NumTables) {
    i => computeHashIdx(a0_indexPc, a0_indexFoldedPathHist, TableInfos, i)
  }
  // Predict
  tables.zipWithIndex.foreach {
    case (t, idx) =>
      t.req.valid          := true.B // Power optimization related, not handled for now.
      t.req.bits.readIndex := a0_readIndex(idx)
      idx match {
        case 0 => t.usefulReset := lowTickCounter(LowTickWidth)
        case 1 => t.usefulReset := lowTickCounter(LowTickWidth)
        case 2 => t.usefulReset := highTickCounter(HighTickWidth)
        case 3 => t.usefulReset := highTickCounter(HighTickWidth)
        case _ => t.usefulReset := false.B
      }
  }
  private val a1_predEntries = tables.map(_.resps.readEntry)
  private val a1_readIndex   = RegEnable(a0_readIndex, a0_fire)
  private val a1_predRead    = Wire(Vec(NumTables, new MicroTageTablePred))
  for (i <- 0 until NumTables) {
    val predTag = computeHashTag(io.s1StartPc, io.s1PathHist, TableInfos, i)
    a1_predRead(i).taken := a1_predEntries(i).takenCtr.isPositive
    a1_predRead(i).valid := a1_predEntries(i).valid
    a1_predRead(i).tag   := a1_predEntries(i).tag
    // Timing might be tight, consider using older PHR.
    a1_predRead(i).tagHit      := a1_predEntries(i).tag === predTag
    a1_predRead(i).cfiPosition := a1_predEntries(i).cfiPosition
    a1_predRead(i).posHit      := false.B
    a1_predRead(i).takenCtr    := a1_predEntries(i).takenCtr
  }

  // Prioritize early position comparison at the cost of ABTB SRAM timing margin,
  // ensuring glitch-free valid signals for the next stage.
  private val a1_posHitVec = Wire(Vec(NumAheadBtbPredictionEntries, Vec(NumTables, Bool())))
  for (i <- 0 until NumAheadBtbPredictionEntries) {
    for (j <- 0 until NumTables) {
      a1_posHitVec(i)(j) := a1_predEntries(j).valid && (a1_predEntries(j).cfiPosition === io.abtbPosVec(i))
    }
  }
  // Get finally selected Table ID for each branch instruction of abtb.
  // (Pre-calculate timing-critical signals for BPU S1.
  // These include the Hit and Taken signals required for MicroTag and aBTB coordination.)
  private val a1_abtbTableIDVec = Wire(Vec(NumAheadBtbPredictionEntries, UInt(log2Ceil(NumTables).W)))
  private val a1_abtbTakenVec   = Wire(Vec(NumAheadBtbPredictionEntries, Bool()))
  private val a1_abtbHitVec     = Wire(Vec(NumAheadBtbPredictionEntries, Bool()))
  private val tabeIDVec         = VecInit.tabulate(NumTables)(i => i.U)
  for (i <- 0 until NumAheadBtbPredictionEntries) {
    // tmp prefix highlights the temporary scope within the loop.
    val tmpTableHitVec   = Wire(Vec(NumTables, Bool()))
    val tmpTableTakenVec = Wire(Vec(NumTables, Bool()))
    for (j <- 0 until NumTables) {
      tmpTableHitVec(j)   := a1_predRead(j).tagHit && a1_posHitVec(i)(j)
      tmpTableTakenVec(j) := a1_predRead(j).taken
    }
    a1_abtbHitVec(i)     := tmpTableHitVec.reduce(_ || _)
    a1_abtbTakenVec(i)   := ParallelPriorityMux(tmpTableHitVec.reverse, tmpTableTakenVec.reverse)
    a1_abtbTableIDVec(i) := ParallelPriorityMux(tmpTableHitVec.reverse, tabeIDVec.reverse)
  }

  private val a3_readIndex = RegInit(0.U.asTypeOf(a1_readIndex))
  private val a3_predRead  = RegInit(0.U.asTypeOf(a1_predRead))
  private val a3_posHitVec = RegInit(0.U.asTypeOf(a1_posHitVec))

  // ------------------------------  ----------------------------------------- //
  private val overridePredRead = Wire(Vec(NumTables, new MicroTageTablePred))
  for (i <- 0 until NumTables) {
    val predTag = computeHashTag(io.overrideStartPc, io.overridePathHist, TableInfos, i)
    overridePredRead(i).taken       := a3_predRead(i).taken
    overridePredRead(i).valid       := a3_predRead(i).valid
    overridePredRead(i).tag         := a3_predRead(i).tag
    overridePredRead(i).tagHit      := a3_predRead(i).tag === predTag
    overridePredRead(i).cfiPosition := a3_predRead(i).cfiPosition
    overridePredRead(i).posHit      := false.B
    overridePredRead(i).takenCtr    := a3_predRead(i).takenCtr
  }

  private val a3_abtbTableIDVec = Wire(Vec(NumAheadBtbPredictionEntries, UInt(log2Ceil(NumTables).W)))
  private val a3_abtbTakenVec   = Wire(Vec(NumAheadBtbPredictionEntries, Bool()))
  private val a3_abtbHitVec     = Wire(Vec(NumAheadBtbPredictionEntries, Bool()))
  for (i <- 0 until NumAheadBtbPredictionEntries) {
    // tmp prefix highlights the temporary scope within the loop.
    val tmpTableHitVec   = Wire(Vec(NumTables, Bool()))
    val tmpTableTakenVec = Wire(Vec(NumTables, Bool()))
    for (j <- 0 until NumTables) {
      tmpTableHitVec(j)   := overridePredRead(j).tagHit && a3_posHitVec(i)(j)
      tmpTableTakenVec(j) := overridePredRead(j).taken
    }
    a3_abtbHitVec(i)     := tmpTableHitVec.reduce(_ || _)
    a3_abtbTakenVec(i)   := ParallelPriorityMux(tmpTableHitVec.reverse, tmpTableTakenVec.reverse)
    a3_abtbTableIDVec(i) := ParallelPriorityMux(tmpTableHitVec.reverse, tabeIDVec.reverse)
  }

  private val a2_readIndex = RegEnable(Mux(overrideValid, a3_readIndex, a1_readIndex), a1_fire)
  private val a2_predRead =
    RegEnable(Mux(overrideValid, overridePredRead, a1_predRead), 0.U.asTypeOf(a1_predRead), a1_fire)
  private val a2_posHitVec =
    RegEnable(Mux(overrideValid, a3_posHitVec, a1_posHitVec), 0.U.asTypeOf(a1_posHitVec), a1_fire)
  private val a2_foldedPathHist =
    RegEnable(Mux(overrideValid, io.overridePathHist, io.s1PathHist), a1_fire)
  private val a2_fromAbtbPos = RegEnable(io.abtbPosVec, a1_fire)
  private val a2_abtbUseTableIDVec =
    RegEnable(Mux(overrideValid, a3_abtbTableIDVec, a1_abtbTableIDVec), 0.U.asTypeOf(a1_abtbTableIDVec), a1_fire)
  private val a2_abtbTakenVec =
    RegEnable(Mux(overrideValid, a3_abtbTakenVec, a1_abtbTakenVec), 0.U.asTypeOf(a1_abtbTakenVec), a1_fire)
  private val a2_abtbTakenCtrVec = Wire(Vec(NumAheadBtbPredictionEntries, TakenCounter()))

  private val a2_abtbHitVec = RegInit(VecInit.fill(NumAheadBtbPredictionEntries)(false.B))
  when(redirectValid) {
    a2_abtbHitVec := 0.U.asTypeOf(Vec(NumAheadBtbPredictionEntries, Bool()))
  }.elsewhen(overrideValid) {
    a2_abtbHitVec := a3_abtbHitVec
  }.elsewhen(a1_fire) {
    a2_abtbHitVec := a1_abtbHitVec
  }

  for (i <- 0 until NumAheadBtbPredictionEntries) {
    val tableTakenCtrVec = Wire(Vec(NumTables, TakenCounter()))
    for (j <- 0 until NumTables) {
      tableTakenCtrVec(j) := a2_predRead(j).takenCtr
    }
    a2_abtbTakenCtrVec(i) := tableTakenCtrVec(a2_abtbUseTableIDVec(i))
  }

  private val s1_predMeta = Wire(Valid(new MicroTageMeta))
  s1_predMeta.valid := a2_abtbHitVec.asUInt.orR
  s1_predMeta.bits.abtbResult := 0.U.asTypeOf(Vec(
    NumAheadBtbPredictionEntries,
    new AbtbResult
  )) // no use, only for placeholder.
  s1_predMeta.bits.readIndex              := a2_readIndex
  s1_predMeta.bits.foldedPathHistForTrain := a2_foldedPathHist
  for (i <- 0 until NumAheadBtbPredictionEntries) {
    // On the cycle following a redirect, MicroTage provides no prediction;
    // therefore, this cycle is excluded from training.
    s1_predMeta.bits.abtbResult(i).valid :=
      io.abtbPrediction(i).valid && io.abtbPrediction(i).bits.attribute.isConditional && RegNext(!redirectValid)
    s1_predMeta.bits.abtbResult(i).baseTaken        := io.abtbPrediction(i).bits.taken
    s1_predMeta.bits.abtbResult(i).hit              := a2_abtbHitVec(i) && io.abtbPrediction(i).valid
    s1_predMeta.bits.abtbResult(i).predTaken        := a2_abtbTakenVec(i)
    s1_predMeta.bits.abtbResult(i).tableId          := a2_abtbUseTableIDVec(i)
    s1_predMeta.bits.abtbResult(i).cfiPosition      := a2_fromAbtbPos(i) // io.abtbPrediction(i).bits.cfiPosition
    s1_predMeta.bits.abtbResult(i).baseIsStrongBias := io.abtbPrediction(i).bits.isStrongBias
    s1_predMeta.bits.abtbResult(i).takenCtr         := a2_abtbTakenCtrVec(i)
  }

  io.prediction.takenVec := a2_abtbTakenVec
  // May be a false hit; needs to be combined with abtbEntry's valid signal for correctness.
  // Done here for timing/layout reasons.
  io.prediction.hitVec := a2_abtbHitVec
  io.meta              := s1_predMeta

  when(a2_fire) {
    a3_predRead  := a2_predRead
    a3_readIndex := a2_readIndex
    a3_posHitVec := a2_posHitVec
  }

  // ------------ MicroTage is only concerned with conditional branches ---------- //
  private val t0_train                  = RegNext(io.fastTrain.get.bits, 0.U.asTypeOf(new BpuFastTrain))
  private val t0_fire                   = RegNext(io.fastTrain.get.valid, false.B)
  private val t0_trainMeta              = t0_train.utageMeta
  private val t0_abtbResult             = t0_trainMeta.abtbResult
  private val t0_trainRead              = VecInit(tables.map(_.train.t0_read))
  private val t0_foldedPathHistForTrain = t0_trainMeta.foldedPathHistForTrain
  private val t0_trainStartPc           = t0_train.startPc
  private val finalPrediction           = t0_train.finalPrediction

  private val t0_hasHitMisPredVec  = Wire(Vec(NumAheadBtbPredictionEntries, Bool()))
  private val t0_missHitMisPredVec = Wire(Vec(NumAheadBtbPredictionEntries, Bool()))
  private val t0_trainResult       = Wire(Vec(NumAheadBtbPredictionEntries, new MicroTageTrainResult))
  // MicroTage only predicts conditional branches that exist in ABTB.
  // If a conditional branch is not in ABTB, ignore it.
  // Rationale: MicroTage is a correction to ABTB results. ABTB is MicroTage's base table.
  // Important constraint: Do not predict branches not provided by ABTB to avoid over-generalization.
  for (i <- 0 until NumAheadBtbPredictionEntries) {
    t0_hasHitMisPredVec(i) := t0_abtbResult(i).valid && t0_abtbResult(i).hit && (
      (finalPrediction.attribute.isConditional && (finalPrediction.cfiPosition === t0_abtbResult(i).cfiPosition) &&
        (t0_abtbResult(i).predTaken =/= finalPrediction.taken)) ||
        ((finalPrediction.cfiPosition > t0_abtbResult(i).cfiPosition) && t0_abtbResult(i).predTaken)
    )

    t0_missHitMisPredVec(i) := t0_abtbResult(i).valid && !t0_abtbResult(i).hit && (
      (finalPrediction.attribute.isConditional && (finalPrediction.cfiPosition === t0_abtbResult(i).cfiPosition) &&
        (t0_abtbResult(i).baseTaken =/= finalPrediction.taken)) ||
        ((finalPrediction.cfiPosition > t0_abtbResult(i).cfiPosition) && t0_abtbResult(i).baseTaken)
    )
    val trainHasAbtbBranch = t0_abtbResult(i).valid && (finalPrediction.cfiPosition >= t0_abtbResult(i).cfiPosition)

    t0_trainResult(i).valid     := trainHasAbtbBranch
    t0_trainResult(i).hit       := t0_abtbResult(i).hit
    t0_trainResult(i).baseTaken := t0_abtbResult(i).baseTaken
    t0_trainResult(i).actualTaken := (finalPrediction.cfiPosition === t0_abtbResult(
      i
    ).cfiPosition) && finalPrediction.taken
    t0_trainResult(i).predTaken        := t0_abtbResult(i).predTaken
    t0_trainResult(i).baseIsStrongBias := t0_abtbResult(i).baseIsStrongBias
    t0_trainResult(i).cfiPosition      := t0_abtbResult(i).cfiPosition
    t0_trainResult(i).tableId          := t0_abtbResult(i).tableId
    t0_trainResult(i).takenCtr         := t0_abtbResult(i).takenCtr
  }
  private val t0_trainMisPredVec = VecInit(t0_hasHitMisPredVec.zip(t0_missHitMisPredVec).map {
    case (hasHitMisPred, missHitMisPred) => hasHitMisPred || missHitMisPred
  })
  private val t0_abtbPosition  = VecInit(t0_abtbResult.map(_.cfiPosition))
  private val t0_compareMatrix = CompareMatrix(t0_abtbPosition)
  // Within one cycle, MicroTage assumes only one conditional branch instruction is mispredicted.
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
  private val t1_foldedPathHistForTrain = RegEnable(t0_foldedPathHistForTrain, t0_fire)
  private val t1_trainRead              = RegEnable(t0_trainRead, t0_fire)
  private val t1_trainResult            = RegEnable(t0_trainResult, t0_fire)
  private val t1_misPredProviderOH      = RegEnable(t0_misPredProviderOH, t0_fire)
  private val t1_needAlloc              = RegEnable(t0_hasMisPred, t0_fire)
  private val t1_allocTaken             = RegEnable(t0_allocTaken, t0_fire)
  private val t1_allocCfiPosition       = RegEnable(t0_misPredAbtbEntry.cfiPosition, t0_fire)
  // Select entries eligible for allocation
  private val t1_keepUseMask = Wire(Vec(NumTables, Bool()))
  for (i <- 0 until NumTables) {
    t1_keepUseMask(i) := t1_trainRead(i).useful =/= 0.U
  }
  private val t1_lowerFillMask =
    Mux(t1_misPredProviderOH === 0.U, 0.U, t1_misPredProviderOH | (t1_misPredProviderOH - 1.U))
  private val t1_allocCandidateMask = ~(t1_lowerFillMask | t1_keepUseMask.asUInt)
  private val t1_normalAllocMask    = PriorityEncoderOH(t1_allocCandidateMask)
  private val t1_trainStartPc       = RegEnable(t0_trainStartPc, t0_fire)

  for (i <- 0 until NumTables) {
    tables(i).train.t0_trainIndex.valid := t0_fire
    tables(i).train.t0_trainIndex.bits  := t0_trainIdx(i)
    val t1_trainTag     = computeHashTag(t1_trainStartPc, t1_foldedPathHistForTrain, TableInfos, i)
    val predCfiPosition = t1_trainRead(i).cfiPosition
    val canGetPosition  = t1_trainRead(i).canGetPosition
    // Use cfiPosition as an additional check to ensure the entry being updated
    // hasn't been evicted or overwritten during the update window.
    // Leverages the buffering effect: within a certain time window,
    // recently evicted/updated entries are likely still in the buffer.
    // Using notHitPosition is beneficial if available, no harm otherwise.
    val entryHitVec = t1_trainResult.map { x =>
      val notHitPosition = canGetPosition && (predCfiPosition =/= x.cfiPosition)
      x.valid && x.hit && (x.tableId === i.U) && !notHitPosition
    }
    val entryBaseTakenVec   = t1_trainResult.map(_.baseTaken)
    val entryStrongBiasVec  = t1_trainResult.map(_.baseIsStrongBias)
    val entryPredTakenVec   = t1_trainResult.map(_.predTaken)
    val entryActualTakenVec = t1_trainResult.map(_.actualTaken)
    val entryCfiPositionVec = t1_trainResult.map(_.cfiPosition)
    val entryTakenCtrVec    = t1_trainResult.map(_.takenCtr)
    val select              = entryHitVec.reduce(_ || _)
    val entryHitOH          = PriorityEncoderOH(entryHitVec)
    val predTaken           = Mux1H(entryHitOH, entryPredTakenVec)
    val baseTaken           = Mux1H(entryHitOH, entryBaseTakenVec)
    val baseIsStrongBias    = Mux1H(entryHitOH, entryStrongBiasVec)
    val updateTaken         = Mux1H(entryHitOH, entryActualTakenVec)
    val updateCfiPosition   = Mux1H(entryHitOH, entryCfiPositionVec)
    val updateTakenCtr      = Mux1H(entryHitOH, entryTakenCtrVec)
    val usefulValid = (predTaken ^ updateTaken) || // the prediction is not equal actual.
      ((baseTaken ^ predTaken) && (predTaken === updateTaken)) // ||
    // ((baseTaken === updateTaken) && baseIsStrongBias) // baseTaken can predict good.

    tables(i).train.t1_update.valid                  := select && t1_fire
    tables(i).train.t1_update.bits.updateValid       := select
    tables(i).train.t1_update.bits.updateTaken       := updateTaken
    tables(i).train.t1_update.bits.usefulValid       := usefulValid
    tables(i).train.t1_update.bits.needUseful        := ((predTaken === updateTaken) && (baseTaken ^ predTaken))
    tables(i).train.t1_update.bits.updateCfiPosition := updateCfiPosition
    tables(i).train.t1_update.bits.updateTakenCtr    := updateTakenCtr
    tables(i).train.t1_alloc.valid                   := t1_needAlloc && t1_fire && t1_normalAllocMask(i)
    tables(i).train.t1_alloc.bits.taken              := t1_allocTaken
    tables(i).train.t1_alloc.bits.cfiPosition        := t1_allocCfiPosition
    // tables(i).train.t1_alloc.bits.tag         := t1_trainTag
    tables(i).train.t1_tag := t1_trainTag
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
  XSPerfAccumulate("train_alloc_failed", (t1_normalAllocMask === 0.U) && t1_needAlloc && t1_fire)
  XSPerfAccumulate("train_useful_low_reset", lowTickCounter(LowTickWidth))
  XSPerfAccumulate("train_useful_high_reset", highTickCounter(HighTickWidth))
  val debug_allocNumSets = 32 // Configurable for debugging
  for (i <- 0 until debug_allocNumSets) {
    // Bucket allocation failures by the lower log2(debug_allocNumSets) bits of the set index.
    // Helps identify hash conflicts or table capacity bottlenecks during training.
    XSPerfAccumulate(
      f"train_alloc_failed_index${i}",
      (t1_normalAllocMask === 0.U) &&
        t1_needAlloc && t1_fire && (t1_trainIdx(0)(log2Ceil(debug_allocNumSets) - 1, 0) === i.U)
    )
  }

  XSPerfSeqAccumulate(
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
