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

package xiangshan.frontend.bpu.block2

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.XSPerfAccumulate
import xiangshan.frontend.GuardedPc
import xiangshan.frontend.bpu.BpuModule
import xiangshan.frontend.bpu.CompareMatrix
import xiangshan.frontend.bpu.StageCtrl
import xiangshan.frontend.bpu.Train
import xiangshan.frontend.bpu.history.phr.PhrAllFoldedHistories
import xiangshan.frontend.bpu.mbtb.MainBtb
import xiangshan.frontend.bpu.tage.Tage

/** A duplicated main btb and tage, looked up at a prediction group's *second* block.
  *
  * The s2 and s3 predictors index off where the group starts, so they answer for its first block and say nothing
  * about a second one. Only pTAGE has an opinion on the second block, which leaves it as the sole authority on a
  * block that reaches the fetch stream. This gives the second block a lookup of the same kind and the same quality
  * the first one gets, so it can be checked against a predictor that arrived at its answer independently.
  *
  * Duplication rather than a second port: both arrays take the same training stream, so they hold the same branches,
  * and each serves one read per cycle at its own address. The copies drift only in replacement and useful-bit state,
  * which each maintains for the access stream it actually sees.
  *
  * The lookup starts a cycle later than the first block's, because the address is the first block's target and that
  * is not known until pTAGE selects a provider. Every stage inside therefore runs one Bpu stage behind, and the
  * answer lands at Bpu s3, level with the s3 prediction it is compared against.
  */
class Block2Predictor(implicit p: Parameters) extends BpuModule {
  class Block2PredictorIO(implicit p: Parameters) extends Bundle {
    val enable:    Bool      = Input(Bool())
    val stageCtrl: StageCtrl = Input(new StageCtrl)

    // where the second block starts, i.e. the first block's target, valid at Bpu s1
    val startPc: GuardedPc = Input(GuardedPc())
    // the folded path history as it stands *between* the two blocks: the group's own history advanced by the first
    // block. Reading with the group's start history would index the context of the first block, not the second.
    val foldedPathHist:         PhrAllFoldedHistories = Input(new PhrAllFoldedHistories(AllFoldedHistoryInfo))
    val foldedPathHistForTrain: PhrAllFoldedHistories = Input(new PhrAllFoldedHistories(AllFoldedHistoryInfo))

    val train:      Train = Input(new Train)
    val trainReady: Bool  = Output(Bool())

    val sramResetDone: Bool = Output(Bool())

    // the second block as this predictor sees it, valid at Bpu s3
    val prediction: Block2Prediction = Output(new Block2Prediction)
  }

  val io: Block2PredictorIO = IO(new Block2PredictorIO)

  private val mbtb = Module(new MainBtb)
  private val tage = Module(new Tage)

  io.sramResetDone := mbtb.io.sramResetDone && tage.io.sramResetDone
  io.trainReady    := mbtb.io.trainReady && tage.io.trainReady

  /* *** stage control ***
   * Shifted by one Bpu stage, so that a read issued at Bpu s1 lands where a read issued at Bpu s0 would have landed
   * at Bpu s2. Training is not shifted: it is driven from the same train bundle as the main copies, at the same time,
   * which is what keeps the two arrays holding the same branches.
   */
  private val shiftedStageCtrl = Wire(new StageCtrl)
  shiftedStageCtrl.s0_fire := io.stageCtrl.s1_fire
  shiftedStageCtrl.s1_fire := io.stageCtrl.s2_fire
  shiftedStageCtrl.s2_fire := io.stageCtrl.s3_fire
  // one past Bpu's last stage, used only to touch the replacer with the final taken mask
  shiftedStageCtrl.s3_fire := RegNext(io.stageCtrl.s3_fire, init = false.B)
  shiftedStageCtrl.t0_fire := io.stageCtrl.t0_fire

  Seq(mbtb.io, tage.io).foreach { predictorIo =>
    predictorIo.enable    := io.enable
    predictorIo.stageCtrl := shiftedStageCtrl
    predictorIo.startPc   := io.startPc
    predictorIo.train     := io.train
  }

  // The replacer is left to the training path alone. A prediction-side touch would have to be qualified by whether
  // the group even had a second block, and this copy is read on every group whether or not one exists, so the touch
  // would mostly record lookups at an address nothing ever asked about.
  mbtb.io.s3_takenMask := VecInit.fill(NumBtbResultEntries)(false.B)

  tage.io.fromMainBtb.result       := mbtb.io.result
  tage.io.fromMainBtb.s1_positions := mbtb.io.s1_positions
  tage.io.fromMainBtb.baseConf     := VecInit(mbtb.io.meta.entries.flatten.map(_.counter.isSaturate))

  tage.io.fromPhr.foldedPathHist         := io.foldedPathHist
  tage.io.fromPhr.foldedPathHistForTrain := io.foldedPathHistForTrain
  tage.io.debug_trainValid               := io.stageCtrl.t0_fire

  /* *** the second block's prediction ***
   * The same selection the top level runs for the first block at s3, minus the sources a second block can never use.
   * A second block only exists behind an exit whose target the pTAGE entry itself supplied, and its own exit is only
   * ever learned as a conditional one, so neither the return stack nor ittage is consulted here: an exit this
   * predictor reports as a return or an other-indirect is by that fact a disagreement, and is reported as such.
   */
  private val result   = mbtb.io.result
  private val isJump   = VecInit(result.map(e => e.valid && (e.bits.attribute.isDirect || e.bits.attribute.isIndirect)))
  private val isCond   = VecInit(result.map(e => e.valid && e.bits.attribute.isConditional))
  private val tagePred = tage.io.prediction.takenVec

  private val takenMask = VecInit(result.zipWithIndex.map { case (entry, i) =>
    isJump(i) || (isCond(i) && Mux(tagePred(i).valid, tagePred(i).bits, entry.bits.taken))
  })
  private val taken = takenMask.reduce(_ || _)

  private val compareMatrix     = CompareMatrix(VecInit(result.map(_.bits.cfiPosition)))
  private val firstTakenBranch  = Mux1H(compareMatrix.getLeastElementOH(takenMask), result)
  private val anyEntry          = result.map(_.valid).reduce(_ || _)
  private val tageDecidedItTrue = Mux1H(compareMatrix.getLeastElementOH(takenMask), tagePred).valid

  io.prediction.hasEntry          := anyEntry
  io.prediction.taken             := taken
  io.prediction.exit              := firstTakenBranch.bits
  io.prediction.debug_tageDecided := tageDecidedItTrue
  // the address this answer belongs to, carried down the shifted pipeline so the top level can assert that it is
  // still looking at the block it asked about
  io.prediction.debug_startPc.foreach { pc =>
    pc := RegEnable(RegEnable(io.startPc, io.stageCtrl.s1_fire), io.stageCtrl.s2_fire)
  }

  private val s3_fire = io.stageCtrl.s3_fire && io.enable
  XSPerfAccumulate("lookupHasEntry", s3_fire && anyEntry)
  XSPerfAccumulate("lookupNoEntry", s3_fire && !anyEntry)
  XSPerfAccumulate("lookupTaken", s3_fire && taken)
  XSPerfAccumulate("lookupTakenByTage", s3_fire && taken && tageDecidedItTrue)
}
