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

package xiangshan.frontend.bpu.history.commonhr

import chisel3._
import chisel3.util._
import freechips.rocketchip.util.SeqToAugmentedSeq
import org.chipsalliance.cde.config.Parameters
import utility.HasCircularQueuePtrHelper
import utility.XSError
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.StageCtrl

class CommonHR(implicit p: Parameters) extends CommonHRModule with Helpers with HasCircularQueuePtrHelper {
  class CommonHRIO extends CommonHRBundle {
    val stageCtrl:      StageCtrl           = Input(new StageCtrl)
    val s1_imliTaken:   Bool                = Input(Bool())
    val s2StartPc:      PrunedAddr          = Input(PrunedAddr(VAddrBits))
    val s2CondHitMask:  Vec[Bool]           = Input(Vec(NumBtbResultEntries, Bool()))
    val s2CfiPositions: Vec[UInt]           = Input(Vec(NumBtbResultEntries, UInt(CfiPositionWidth.W)))
    val s2CfiTargets:   Vec[PrunedAddr]     = Input(Vec(NumBtbResultEntries, PrunedAddr(VAddrBits)))
    val update:         CommonHRUpdate      = Input(new CommonHRUpdate)
    val redirect:       CommonHRRedirect    = Input(new CommonHRRedirect)
    val s0_imli:        UInt                = Output(UInt(ImliHistoryLength.W))
    val s0_commonHR:    CommonHREntry       = Output(new CommonHREntry)
    val s3DedupHitMask: Vec[Bool]           = Output(Vec(NumBtbResultEntries, Bool()))
    val s3ResolveMeta:  CommonHRResolveMeta = Output(new CommonHRResolveMeta)

    val s0_startPc: Option[PrunedAddr] = Some(Input(PrunedAddr(VAddrBits))) // for debug
  }
  val io = IO(new CommonHRIO)

  // stage ctrl
  private val s0_fire = io.stageCtrl.s0_fire
  private val s1_fire = io.stageCtrl.s1_fire
  private val s2_fire = io.stageCtrl.s2_fire
  private val s3_fire = io.stageCtrl.s3_fire

  private val s3_override = io.update.s3Override

  // common history register
  private val s0_imli                = WireInit(0.U(ImliHistoryLength.W))
  private val s1_imli                = RegEnable(s0_imli, s0_fire)
  private val s2_imli                = RegEnable(s1_imli, s1_fire)
  private val s3_imli                = RegEnable(s2_imli, s2_fire)
  private val imli                   = RegInit(0.U(ImliHistoryLength.W))
  private val s0_commonHR            = WireInit(0.U.asTypeOf(new CommonHREntry))
  private val s1_commonHR            = RegEnable(s0_commonHR, 0.U.asTypeOf(new CommonHREntry), s0_fire)
  private val s2_commonHR            = RegEnable(s1_commonHR, 0.U.asTypeOf(new CommonHREntry), s1_fire)
  private val s3_commonHR            = RegEnable(s2_commonHR, 0.U.asTypeOf(new CommonHREntry), s2_fire)
  private val commonHR               = RegInit(0.U.asTypeOf(new CommonHREntry))
  private val debugCommonHR          = RegInit(0.U.asTypeOf(new CommonHREntry))
  private val s3_commonHRResolveMeta = WireInit(0.U.asTypeOf(new CommonHRResolveMeta))

  private val r1_valid    = RegNext(io.redirect.valid, false.B)
  private val r1_commonHR = WireInit(0.U.asTypeOf(new CommonHREntry))

  private val enqPtr     = RegInit(HistPtr(false.B, 0.U))
  private val predPtr    = RegInit(HistPtr(false.B, 0.U))
  private val writePtr   = RegInit(HistPtr(false.B, 0.U))
  private val recoverPtr = RegInit(HistPtr(false.B, 0.U))
  private val histQueue  = RegInit(VecInit(Seq.fill(HistQueueSize)(0.U.asTypeOf(new CommonHREntry))))

  /*
   * CommonHR train from redirect/s3_prediction
   */
  s3_commonHRResolveMeta.valid := s3_commonHR.valid
  s3_commonHRResolveMeta.ghr   := s3_commonHR.ghr
  s3_commonHRResolveMeta.bw    := s3_commonHR.bw
  s3_commonHRResolveMeta.imli  := s3_imli

  io.s0_imli       := s0_imli
  io.s3ResolveMeta := s3_commonHRResolveMeta
  io.s0_commonHR   := s0_commonHR

  /*
   * Precompute per-CFI candidate information in s2 for the s3 CommonHR update.
   */
  // Deduplicate conditional hits by position before counting older conditional branches.
  private val s2_hitMask          = dedupHitPositions(io.s2CondHitMask, io.s2CfiPositions)
  private val s2_numHit           = PopCount(s2_hitMask)
  private val s2_bwTakenCandidate = Wire(Vec(NumBtbResultEntries, Bool()))
  private val s2_numLessCandidate = Wire(Vec(NumBtbResultEntries, UInt(log2Ceil(NumBtbResultEntries + 1).W)))

  for (i <- 0 until NumBtbResultEntries) {
    val pos    = io.s2CfiPositions(i)
    val target = io.s2CfiTargets(i)
    val lessThanCurrent = VecInit(io.s2CfiPositions.zip(s2_hitMask).map { case (otherPos, hit) =>
      hit && (otherPos < pos)
    })
    val currentCfiPc = getCfiPcFromPosition(io.s2StartPc, pos)

    s2_numLessCandidate(i) := PopCount(lessThanCurrent)
    s2_bwTakenCandidate(i) := isBackwardBranch(currentCfiPc, target)
  }

  /*
   * Use the latched s2 candidate information to update CommonHR when s3 fires.
   */
  private val s3_update           = io.update // bp pipeline s3 level update
  private val s3_hitMask          = RegEnable(s2_hitMask, s2_fire)
  private val s3_taken            = s3_update.taken
  private val s3_firstTakenPos    = s3_update.firstTakenBranch.bits.cfiPosition
  private val s3_firstTakenIsCond = s3_update.firstTakenBranch.bits.attribute.isConditional
  private val s3_cfiPc            = getCfiPcFromPosition(s3_update.startPc, s3_firstTakenPos)
  private val s3_bwTakenDiff      = WireInit(false.B)
  private val s3_bwTaken          = isBackwardBranch(s3_cfiPc, s3_update.target)

  // NOTE: Usually, the maximum value of GhrShamt is NumBtbResultEntries, but in reality, the maximum value is NumBtbResultEntries+ 1
  private val s3_numLessCandidate = RegEnable(s2_numLessCandidate, s2_fire)
  private val s3_numHit           = RegEnable(s2_numHit, s2_fire)
  private val s3_newCommonHR      = WireInit(0.U.asTypeOf(new CommonHREntry))

  private val s3_defaultCommonHR  = WireInit(0.U.asTypeOf(new CommonHREntry))
  private val s3_takenCommonHR    = Wire(Vec(NumBtbResultEntries, new CommonHREntry))
  private val s3_bwTakenCandidate = RegEnable(s2_bwTakenCandidate, s2_fire)

  private val s3_selectedTakenCommonHR = Mux1H(s3_update.firstTakenBranchOH, s3_takenCommonHR)
  s3_bwTakenDiff := Mux1H(s3_update.firstTakenBranchOH, s3_bwTakenCandidate)
  XSError(
    s3_override && s3_taken && s3_firstTakenIsCond && s3_bwTaken =/= s3_bwTakenDiff,
    "s3_bwTaken is not equal s3_bwTakenDiff"
  )

  s3_takenCommonHR.foreach(_ := 0.U.asTypeOf(new CommonHREntry))
  for (i <- 0 until NumBtbResultEntries) {
    val candidate      = s3_takenCommonHR(i)
    val isCond         = s3_update.attributes(i).isConditional
    val numLessCurrent = s3_numLessCandidate(i)
    val currentBwTaken = s3_bwTakenCandidate(i)
    candidate.valid           := s3_fire
    candidate.predStartPc.get := s3_update.startPc
    candidate.ghr             := getNewHR(commonHR.ghr, numLessCurrent, s3_numHit, s3_taken, isCond)(GhrHistoryLength)
    candidate.bw := getNewHR(
      commonHR.bw,
      numLessCurrent,
      s3_numHit,
      s3_taken,
      isCond,
      Option(s3_taken && isCond && currentBwTaken)
    )(BWHistoryLength)
  }

  s3_defaultCommonHR.valid           := s3_fire
  s3_defaultCommonHR.predStartPc.get := s3_update.startPc
  s3_defaultCommonHR.ghr             := (commonHR.ghr << s3_numHit)(GhrHistoryLength - 1, 0)
  s3_defaultCommonHR.bw              := (commonHR.bw << s3_numHit)(BWHistoryLength - 1, 0)

  XSError(s3_fire && s3_taken && !s3_update.firstTakenBranchOH.reduce(_ || _), "taken but no firstTakenBranchOH")
  s3_newCommonHR := Mux(s3_taken, s3_selectedTakenCommonHR, s3_defaultCommonHR)

  /*
   * ghr/bw is not involved in prediction during redirect; used here as a placeholder
   */
  private val r0_valid    = io.redirect.valid
  private val r0_taken    = io.redirect.taken
  private val r0_isCond   = io.redirect.attribute.isConditional
  private val r0_bwTaken  = isBackwardBranch(io.redirect.cfiPc, io.redirect.target)
  private val r0_commonHR = WireInit(0.U.asTypeOf(new CommonHREntry))
  r0_commonHR.valid           := false.B
  r0_commonHR.predStartPc.get := io.redirect.target
  r0_commonHR.ghr             := io.redirect.meta.ghr
  r0_commonHR.bw              := io.redirect.meta.bw

  /*
   * Perform GHR/BW calculation one cycle after the redirect occurs
   */

  private val r1_redirect     = RegEnable(io.redirect, 0.U.asTypeOf(new CommonHRRedirect), io.redirect.valid)
  private val r1_s0StartPc    = RegEnable(io.s0_startPc.get, 0.U.asTypeOf(PrunedAddr(VAddrBits)), io.redirect.valid)
  private val r1_metaGhr      = r1_redirect.meta.ghr
  private val r1_metaBW       = r1_redirect.meta.bw
  private val r1_oldPositions = r1_redirect.meta.position
  private val r1_oldCondHits = VecInit(r1_redirect.meta.hitMask.zip(r1_redirect.meta.attribute).map {
    case (hit, attr) => hit && attr.isConditional
  })
  // TODO:Need pipeline stage for redirect update CommonHR if dedup calc skipped?
  private val r1_oldHits       = r1_redirect.meta.hitMask
  private val r1_taken         = r1_redirect.taken
  private val r1_isCond        = r1_redirect.attribute.isConditional
  private val r1_bwTaken       = isBackwardBranch(r1_redirect.cfiPc, r1_redirect.target)
  private val r1_takenPosition = getAlignedInstOffset(r1_redirect.cfiPc)
  private val r1_lessThanPc = r1_oldPositions.zip(r1_oldHits).map {
    case (pos, hit) => hit && (pos < r1_takenPosition)
  } // positions less than redirect branch pc
  private val r1_numLess = PopCount(r1_lessThanPc)
  private val r1_numHit  = PopCount(r1_oldHits)

  r1_commonHR.valid           := false.B
  r1_commonHR.predStartPc.get := r1_s0StartPc
  r1_commonHR.ghr             := getNewHR(r1_metaGhr, r1_numLess, r1_numHit, r1_taken, r1_isCond)(GhrHistoryLength)
  r1_commonHR.bw := getNewHR(r1_metaBW, r1_numLess, r1_numHit, r1_taken, r1_isCond, Option(r1_bwTaken && r1_taken))(
    BWHistoryLength
  )

  dontTouch(r1_valid)
  dontTouch(r1_commonHR)

  /*
   * Directly resume and update commonHR after redirection for debugging
   */
  private val debug_metaGhr      = io.redirect.meta.ghr
  private val debug_metaBW       = io.redirect.meta.bw
  private val debug_oldPositions = io.redirect.meta.position
  private val debug_oldCondHits = VecInit(io.redirect.meta.hitMask.zip(io.redirect.meta.attribute).map {
    case (hit, attr) => hit && attr.isConditional
  })
  private val debug_oldHits       = dedupHitPositions(debug_oldCondHits, debug_oldPositions)
  private val debug_taken         = io.redirect.taken
  private val debug_isCond        = io.redirect.attribute.isConditional
  private val debug_bwTaken       = isBackwardBranch(io.redirect.cfiPc, io.redirect.target)
  private val debug_takenPosition = getAlignedInstOffset(io.redirect.cfiPc)
  private val debug_lessThanPc = debug_oldPositions.zip(debug_oldHits).map {
    case (pos, hit) => hit && (pos < debug_takenPosition)
  } // positions less than redirect branch pc
  private val debug_numLess  = PopCount(debug_lessThanPc)
  private val debug_numHit   = PopCount(debug_oldHits)
  private val debug_commonHR = WireInit(0.U.asTypeOf(new CommonHREntry))
  debug_commonHR.valid           := false.B
  debug_commonHR.predStartPc.get := io.s0_startPc.get
  debug_commonHR.ghr := getNewHR(debug_metaGhr, debug_numLess, debug_numHit, debug_taken, debug_isCond)(
    GhrHistoryLength
  )
  debug_commonHR.bw := getNewHR(
    debug_metaBW,
    debug_numLess,
    debug_numHit,
    debug_taken,
    debug_isCond,
    Option(debug_bwTaken && debug_taken)
  )(
    BWHistoryLength
  )

  /*
   * History register update and recovery logic
   */
  when(r1_valid) {
    commonHR := r1_commonHR
  }.elsewhen(s3_fire) {
    commonHR := s3_newCommonHR
  }

  when(r0_valid) {
    debugCommonHR := debug_commonHR
  }.elsewhen(s3_fire) {
    debugCommonHR := s3_newCommonHR
  }

  // On r1 redirect recovery, overwrite s1/s2 history with the recomputed CommonHR
  when(r1_valid) {
    s1_commonHR := r1_commonHR
    s2_commonHR := r1_commonHR
  }

  // imli update
  when(r0_valid) {
    val r0_newImli = Mux(
      r0_taken && r0_bwTaken && r0_isCond,
      Mux(io.redirect.meta.imli.andR, io.redirect.meta.imli, io.redirect.meta.imli + 1.U),
      0.U
    )
    imli    := r0_newImli
    s0_imli := r0_newImli
  }.elsewhen(s3_override) {
    val s3_newImli = Mux(s3_taken && s3_bwTaken && s3_firstTakenIsCond, Mux(s3_imli.andR, s3_imli, s3_imli + 1.U), 0.U)
    imli    := s3_newImli
    s0_imli := s3_newImli
  }.elsewhen(s1_fire) {
    val s1_newImli = Mux(io.s1_imliTaken, Mux(s1_imli.andR, s1_imli, s1_imli + 1.U), 0.U)
    imli    := s1_newImli
    s0_imli := s1_newImli
  }.otherwise {
    s0_imli := imli
  }

  /*
   * NOTE:Only applicable to the current predicted flow structure
   * if s3_override,  the current s0 should use s2,
   * the next cycle should use the current s1,
   * and the next cycle should use the current s0
   * At the current flow level, a maximum of two levels of history need to be restored
   */

  private val enqEnable       = s0_fire
  private val predEnable      = s0_fire && ((predPtr + 3.U) === enqPtr)
  private val writeEnable     = s3_fire
  private val hasOverrideHist = recoverPtr + 2.U === writePtr // There is sufficient history for restoration
  private val recoverInc      = s3_fire && hasOverrideHist
  private val sync            = predPtr === writePtr
  private val initCommonHR    = WireInit(0.U.asTypeOf(new CommonHREntry))
  initCommonHR.predStartPc.get := io.s0_startPc.get

  when(r0_valid) {
    enqPtr                            := writePtr + 1.U
    recoverPtr                        := writePtr - 1.U
    predPtr                           := writePtr - 1.U
    histQueue(writePtr.value)         := initCommonHR // The queue value during redirect is used for diff
    histQueue((writePtr - 1.U).value) := r0_commonHR  // The queue value during redirect is used for diff
  }.elsewhen(s3_override) {
    val realRecoverPtr = Mux(hasOverrideHist, recoverPtr + 1.U, recoverPtr)
    histQueue(writePtr.value)         := s3_newCommonHR // update s3_fire block
    histQueue((writePtr + 1.U).value) := initCommonHR   // write new s0_block
    enqPtr                            := writePtr + 2.U
    predPtr                           := realRecoverPtr
    writePtr                          := writePtr + 1.U
    recoverPtr                        := realRecoverPtr
  }.otherwise {
    when(enqEnable) {
      histQueue(enqPtr.value) := initCommonHR
      enqPtr                  := enqPtr + 1.U
      predPtr                 := Mux(predEnable, predPtr + 1.U, predPtr)
    }
    when(writeEnable) {
      histQueue(writePtr.value) := s3_newCommonHR
      writePtr                  := writePtr + 1.U
      recoverPtr                := Mux(recoverInc, recoverPtr + 1.U, recoverPtr)
    }
  }

  when(r1_valid) {
    histQueue(recoverPtr.value) := r1_commonHR
  }

  io.s3DedupHitMask := s3_hitMask

  // Use distance-based checks for circular pointers to avoid wrap-around ordering ambiguity.
  private val writeToPredDist   = distanceBetween(writePtr, predPtr)
  private val predToRecoverDist = distanceBetween(predPtr, recoverPtr)
  XSError(
    enqEnable && (writeToPredDist > 3.U || predToRecoverDist > 2.U),
    "The predPtr exceeds the correct range"
  )
  XSError(
    writeEnable && s3_update.startPc =/= histQueue(writePtr.value).predStartPc.get,
    "update history maybe mismatched!"
  )

  s0_commonHR := MuxCase(
    0.U.asTypeOf(new CommonHREntry),
    Seq(
      r0_valid                     -> r0_commonHR,
      r1_valid                     -> r1_commonHR,
      s3_override                  -> histQueue(recoverPtr.value),
      (s0_fire && s3_fire && sync) -> s3_newCommonHR, // bypass s3_newCommonHR
      s0_fire                      -> histQueue(predPtr.value)
    )
  )

  private val diffCommonHR           = debugCommonHR.asUInt =/= commonHR.asUInt
  private val diffDebugAndR1CommonHR = debugCommonHR.asUInt =/= r1_commonHR.asUInt
  XSError(
    (!r1_valid && diffCommonHR) || (r1_valid && diffDebugAndR1CommonHR),
    "debugCommonHR is not equal commonHR!"
  )

  dontTouch(diffCommonHR)
  dontTouch(writePtr)
  dontTouch(enqPtr)
  dontTouch(predPtr)
  dontTouch(predEnable)
  dontTouch(sync)
  dontTouch(hasOverrideHist)
  dontTouch(recoverInc)

  if (EnableCommitGHistDiff) {
    val r1_lessThanPcUInt = r1_lessThanPc.asUInt
    val ghrUInt           = commonHR.ghr.asUInt
    val bwUInt            = commonHR.bw.asUInt
    dontTouch(s3_newCommonHR)
    dontTouch(r1_numLess)
    dontTouch(r1_commonHR)
    dontTouch(r1_lessThanPcUInt)
    dontTouch(ghrUInt)
    dontTouch(bwUInt)
  }
}
