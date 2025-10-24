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
import xiangshan.frontend.bpu.FoldedHistoryInfo
import xiangshan.frontend.bpu.phr.PhrAllFoldedHistories

// import xiangshan.frontend.bpu.utage.MicroTageTable.histLen
// import xiangshan.frontend.bpu.utage.MicroTageTable.numSets

/**
 * This module is the implementation of the TAGE (TAgged GEometric history length predictor).
 */
class MicroTage(implicit p: Parameters) extends BasePredictor with HasMicroTageParameters with Helpers {
  class MicroTageIO(implicit p: Parameters) extends BasePredictorIO {
    val foldedPathHist:         PhrAllFoldedHistories = Input(new PhrAllFoldedHistories(AllFoldedHistoryInfo))
    val foldedPathHistForTrain: PhrAllFoldedHistories = Input(new PhrAllFoldedHistories(AllFoldedHistoryInfo))
    val prediction:             Valid[MicroTageMeta]  = Output(Valid(new MicroTageMeta))
  }
  val io: MicroTageIO = IO(new MicroTageIO)
  io.resetDone := true.B

  /* *** submodules *** */
  private val tables = TableInfos.zipWithIndex.map {
    case (info, i) =>
      val t = Module(new MicroTageTable(info.NumSets, info.HistoryLength, TagWidth)).io
      t
  }
  private val tickCounter = RegInit(0.U((TickWidth + 1).W))
  // Predict
  tables.foreach { t =>
    t.req.startPc        := io.startVAddr
    t.req.foldedPathHist := io.foldedPathHist
    t.usefulReset        := tickCounter(TickWidth)
  }

  io.prediction.valid          := tables.map(_.resp.valid).reduce(_ || _)
  io.prediction.bits.hitMap    := tables.map(_.resp.valid)
  io.prediction.bits.takenMap  := tables.map(_.resp.bits.taken)
  io.prediction.bits.hit       := tables.map(_.resp.valid).reduce(_ || _)
  io.prediction.bits.usefulMap := VecInit(tables.map(_.resp.bits.useful)).asUInt
  private val takenCases       = tables.reverse.map(t => t.resp.valid -> t.resp.bits.taken)
  private val cfiPositionCases = tables.reverse.map(t => t.resp.valid -> t.resp.bits.cfiPosition)
  io.prediction.bits.taken       := MuxCase(false.B, takenCases)
  io.prediction.bits.cfiPosition := MuxCase(0.U(CfiPositionWidth.W), cfiPositionCases)

  private val trainNext  = RegNext(io.train, 0.U.asTypeOf(Valid(new BpuTrain)))
  private val trainData  = trainNext.bits
  private val trainMeta  = trainNext.bits.meta.utage
  private val trainValid = trainNext.valid

  // ------------ MicroTage is only concerned with conditional branches ---------- //
  private val misPred = VecInit(trainData.branches.map(b =>
    b.valid && b.bits.attribute.isConditional &&
      (((b.bits.cfiPosition < trainMeta.cfiPosition) && b.bits.taken) ||
        ((b.bits.cfiPosition === trainMeta.cfiPosition) && (b.bits.taken ^ trainMeta.taken)) ||
        (b.bits.cfiPosition > trainMeta.cfiPosition)) && trainMeta.hit
  ))
  private val hasTaken = VecInit(trainData.branches.map(b =>
    b.valid && b.bits.attribute.isConditional && b.bits.taken
  ))
  private val missHitTaken            = hasTaken.reduce(_ || _) && !trainMeta.hit
  private val missHitTakenCfiPosition = Mux1H(PriorityEncoderOH(hasTaken), trainData.branches.map(_.bits.cfiPosition))
  private val hitMisPred              = misPred.reduce(_ || _)
  private val hitMisPredCfiPosition   = Mux1H(PriorityEncoderOH(misPred), trainData.branches.map(_.bits.cfiPosition))

  private val needAllocated    = hitMisPred || missHitTaken
  private val allocCfiPosition = Mux(missHitTaken, missHitTakenCfiPosition, hitMisPredCfiPosition)

  private val hasPredBr = VecInit(trainData.branches.map(b =>
    b.valid && b.bits.attribute.isConditional && (b.bits.cfiPosition === trainMeta.cfiPosition)
  )).reduce(_ || _)
  private val hasPredBrCorrect = VecInit(trainData.branches.map(b =>
    b.valid && b.bits.attribute.isConditional &&
      (b.bits.cfiPosition === trainMeta.cfiPosition)
      && (trainMeta.taken === b.bits.taken)
  )).reduce(_ || _)

  private val providerMask = PriorityEncoderOH(trainMeta.hitMap.reverse).reverse

  private val hitMask            = trainMeta.hitMap.asUInt
  private val lowerFillMask      = Mux(hitMask === 0.U, 0.U, hitMask | (hitMask - 1.U))
  private val usefulMask         = trainMeta.usefulMap
  private val allocCandidateMask = ~(lowerFillMask | usefulMask)
  private val allocMask          = PriorityEncoderOH(allocCandidateMask)

  when(tickCounter(TickWidth)) {
    tickCounter := 0.U
  }.elsewhen((allocMask === 0.U) && needAllocated && trainValid) {
    tickCounter := tickCounter + 1.U
  }

  // 训练逻辑分为更新项和替换项
  // 对于更新项：如果train_position < table_position, 当前项的值保持不变。
  // 对于更新项：如果train_position === table_position，当前项的值，按照预测要求进行增减。
  // 对于更新项：如果train_position > table_position，当前项的值保持不变。

  // 更新项的逻辑：去除噪音，只有position相等时更新，如何更新由训练时的对应项结果决定。
  // 分配项的逻辑：由选出的项直接进行替换，没有空项了，则分配失败。记录累计分配失败一次，8次分配失败。则清空所有useful。
  tables.zipWithIndex.foreach { case (t, i) =>
    t.update.valid            := ((allocMask(i) && needAllocated) || (providerMask(i) && hasPredBr)) && trainValid
    t.update.bits.startPc     := trainData.startVAddr
    t.update.bits.cfiPosition := Mux(allocMask(i) && needAllocated, allocCfiPosition, trainMeta.cfiPosition)
    t.update.bits.alloc       := allocMask(i) && needAllocated
    t.update.bits.correct     := providerMask(i) && hasPredBr && hasPredBrCorrect
    t.update.bits.foldedPathHistForTrain := io.foldedPathHistForTrain
  }

  // ==========================================================================
  // === PERF === Performance Counters Section
  // ==========================================================================
  private val misPredEQ = VecInit(trainData.branches.map(b =>
    b.valid && b.bits.attribute.isConditional &&
      ((b.bits.cfiPosition === trainMeta.cfiPosition) && (b.bits.taken ^ trainMeta.taken)) && trainMeta.hit
  )).reduce(_ || _)
  private val misPredLT = VecInit(trainData.branches.map(b =>
    b.valid && b.bits.attribute.isConditional &&
      ((b.bits.cfiPosition < trainMeta.cfiPosition) && b.bits.taken) && trainMeta.hit
  )).reduce(_ || _)
  private val misPredGT = VecInit(trainData.branches.map(b =>
    b.valid && b.bits.attribute.isConditional &&
      ((b.bits.cfiPosition > trainMeta.cfiPosition) && b.bits.taken) && trainMeta.hit
  )).reduce(_ || _)

  private val trainHasBr = VecInit(trainData.branches.map(b =>
    b.valid && b.bits.attribute.isConditional
  )).reduce(_ || _)

  // === Prediction structure level ===
  XSPerfAccumulate("microtage_pred_valid", io.stageCtrl.s0_fire)
  XSPerfAccumulate("microtage_pred_hit", io.stageCtrl.s0_fire && io.prediction.valid && io.prediction.bits.hit)
  XSPerfAccumulate("microtage_pred_miss", io.stageCtrl.s0_fire && !io.prediction.bits.hit)

  // === Training feedback stage ===
  XSPerfAccumulate("microtage_train_valid", trainValid)
  XSPerfAccumulate("microtage_train_br_valid", trainValid && trainHasBr)

  // train hit and correct
  private val hitPredBrCorrect   = trainValid && trainMeta.hit && hasPredBrCorrect
  private val hitPredBrWrong     = trainValid && trainMeta.hit && hitMisPred
  private val trainMiss          = trainValid && !trainMeta.hit
  private val trainMissBrCorrect = trainValid && !missHitTaken
  private val trainMissBrWrong   = trainValid && missHitTaken
  private val trainUnseenPredBr  = trainValid && !hasPredBr && trainHasBr
  XSPerfAccumulate("microtage_train_hit_predBr_correct", hitPredBrCorrect)
  XSPerfAccumulate("microtage_train_hit_predBr_wrong", hitPredBrWrong)
  XSPerfAccumulate("microtage_train_hit_EQ_predBr_wrong", trainValid && misPredEQ)
  XSPerfAccumulate("microtage_train_hit_LT_predBr_wrong", trainValid && misPredLT)
  XSPerfAccumulate("microtage_train_hit_GT_predBr_wrong", trainValid && misPredGT)

  XSPerfAccumulate("microtage_train_miss", trainMiss)
  XSPerfAccumulate("microtage_train_miss_Brcorrect", trainMissBrCorrect)
  XSPerfAccumulate("microtage_train_miss_Brwrong", trainMissBrWrong)
  XSPerfAccumulate("microtage_train_unseenPredBr", trainUnseenPredBr)

  // === Allocation / update events ===
  XSPerfAccumulate("microtage_need_alloc", trainValid && needAllocated)
  XSPerfAccumulate("microtage_alloc_triggered", trainValid && (allocMask.orR && needAllocated))
  XSPerfAccumulate("microtage_tick_reset", tickCounter(TickWidth))

  // === Debugging auxiliary statistics ===
  // Predictions with feedback, regardless of hit or miss
  XSPerfAccumulate("microtage_train_with_feedback", trainValid && hasPredBr)

  // Missed but trained (indicating the prediction structure might be saturated)
  XSPerfAccumulate("microtage_miss_trained", trainValid && trainMiss && needAllocated)

  // Per-table allocation occurrences (which table handles replacement)
  for (i <- 0 until 3) {
    XSPerfAccumulate(s"microtage_alloc_table_$i", trainValid && allocMask(i) && needAllocated)
  }

  // Multi-hit detection: more than one table valid simultaneously
  XSPerfAccumulate("microtage_table_multi_hit", io.stageCtrl.s0_fire && (PopCount(tables.map(_.resp.valid)) > 1.U))

  // No valid table response (potential predictor invalid path)
  XSPerfAccumulate("microtage_table_resp_invalid", io.stageCtrl.s0_fire && !tables.map(_.resp.valid).reduce(_ || _))

  // === PHR Test ===
  private val testIdxFhInfos = TableInfos.zipWithIndex.map {
    case (info, i) =>
      val t = new FoldedHistoryInfo(info.HistoryLength, min(log2Ceil(info.NumSets), info.HistoryLength))
      t
  }
  private val testTagFhInfos = TableInfos.zipWithIndex.map {
    case (info, i) =>
      val t = new FoldedHistoryInfo(info.HistoryLength, min(log2Ceil(info.NumSets), info.HistoryLength))
      t
  }
  private val testAltTagFhInfos = TableInfos.zipWithIndex.map {
    case (info, i) =>
      val t = new FoldedHistoryInfo(info.HistoryLength, min(log2Ceil(info.NumSets), info.HistoryLength))
      t
  }

  def getUnhashedIdx(pc: PrunedAddr): UInt = pc.toUInt(VAddrBits - 1, instOffsetBits)
  def getUnhashedTag(pc: PrunedAddr): UInt = pc.toUInt(VAddrBits - 1, log2Ceil(FetchBlockAlignSize))

  def computeHash(unhashedIdx: UInt, unhashedTag: UInt, allFh: PhrAllFoldedHistories, i: Int): (UInt, UInt) = {
    val idxFh    = allFh.getHistWithInfo(testIdxFhInfos(i)).foldedHist
    val tagFh    = allFh.getHistWithInfo(testTagFhInfos(i)).foldedHist
    val altTagFh = allFh.getHistWithInfo(testAltTagFhInfos(i)).foldedHist
    val idx      = (unhashedIdx ^ idxFh)(log2Ceil(TableInfos(i).NumSets) - 1, 0)
    val tag      = (unhashedTag ^ tagFh ^ (altTagFh << 1))(13 - 1, 0)
    (idx, tag)
  }

  private val readUnhasedIdx   = getUnhashedIdx(io.startVAddr)
  private val readUnhasedTag   = getUnhashedTag(io.startVAddr)
  private val (s0_idx, s0_tag) = computeHash(readUnhasedIdx, readUnhasedTag, io.foldedPathHist, 0)
  io.prediction.bits.testPredIdx       := s0_idx
  io.prediction.bits.testPredTag       := s0_tag
  io.prediction.bits.testPredStartAddr := io.startVAddr.toUInt

  private val trainUnhasedIdx      = getUnhashedIdx(trainData.startVAddr)
  private val trainUnhasedTag      = getUnhashedTag(trainData.startVAddr)
  private val (trainIdx, trainTag) = computeHash(trainUnhasedIdx, trainUnhasedTag, io.foldedPathHistForTrain, 0)

  XSPerfAccumulate("train_idx_hit", trainValid && (trainMeta.testPredIdx === trainIdx))
  XSPerfAccumulate("train_tag_hit", trainValid && (trainMeta.testPredTag === trainTag))
  XSPerfAccumulate("train_idx_miss", trainValid && (trainMeta.testPredIdx =/= trainIdx))
  XSPerfAccumulate("train_tag_miss", trainValid && (trainMeta.testPredTag =/= trainTag))
  XSPerfAccumulate(
    "train_idx_tag_hit",
    trainNext.valid && (trainMeta.testPredIdx === trainIdx) &&
      (trainMeta.testPredTag === trainTag) && (trainMeta.testPredStartAddr === trainNext.bits.startVAddr.toUInt)
  )
  XSPerfAccumulate(
    "train_idx_tag_miss",
    trainNext.valid && ((trainMeta.testPredIdx =/= trainIdx) ||
      (trainMeta.testPredTag =/= trainTag)) && (trainMeta.testPredStartAddr === trainNext.bits.startVAddr.toUInt)
  )
}
