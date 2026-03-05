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
import utility.CircularQueuePtr
import utility.HasCircularQueuePtrHelper
import utility.XSError
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.BpuRedirect
import xiangshan.frontend.bpu.StageCtrl

class CommonHR(implicit p: Parameters) extends CommonHRModule with Helpers with HasCircularQueuePtrHelper {
  class CommonHRIO extends CommonHRBundle {
    val stageCtrl:   StageCtrl        = Input(new StageCtrl)
    val update:      CommonHRUpdate   = Input(new CommonHRUpdate)
    val redirect:    CommonHRRedirect = Input(new CommonHRRedirect)
    val s0_commonHR: CommonHREntry    = Output(new CommonHREntry)
    val commonHR:    CommonHREntry    = Output(new CommonHREntry)

    val s0_startPc: Option[PrunedAddr] = Some(Input(PrunedAddr(VAddrBits))) // for debug
  }
  val io = IO(new CommonHRIO)

  // stage ctrl
  private val s0_fire = io.stageCtrl.s0_fire
  private val s2_fire = io.stageCtrl.s2_fire
  private val s3_fire = io.stageCtrl.s3_fire

  private val s3_override = io.update.s3Override

  // common history register
  private val s0_commonHR = WireInit(0.U.asTypeOf(new CommonHREntry))
  private val commonHR    = RegInit(0.U.asTypeOf(new CommonHREntry))

  private val enqPtr     = RegInit(HistPtr(false.B, 0.U))
  private val predPtr    = RegInit(HistPtr(false.B, 0.U))
  private val writePtr   = RegInit(HistPtr(false.B, 0.U))
  private val recoverPtr = RegInit(HistPtr(false.B, 0.U))
  private val histQueue  = RegInit(VecInit(Seq.fill(HistQueueSize)(0.U.asTypeOf(new CommonHREntry))))

  /*
   * CommonHR train from redirect/s3_prediction
   */
  io.s0_commonHR := s0_commonHR
  io.commonHR    := commonHR

  /*
   * s3_fire update CommonHR
   */
  private val s3_update = io.update // bp pipeline s3 level update
  private val s3_taken  = s3_update.taken
  // deduplicate hit positions
  private val s3_hitMask          = dedupHitPositions(s3_update.condHitMask, s3_update.position)
  private val s3_firstTakenPos    = s3_update.firstTakenBranch.bits.cfiPosition
  private val s3_firstTakenIsCond = s3_update.firstTakenBranch.bits.attribute.isConditional
  private val s3_cfiPc            = getCfiPcFromPosition(s3_update.startPc, s3_firstTakenPos)
  private val s3_bwTaken =
    s3_cfiPc.addr > s3_update.target.addr
  private val s3_lessThanFirstTaken = s3_update.position.zip(s3_hitMask).map {
    case (pos, hit) => hit && (pos < s3_firstTakenPos)
  }
  // NOTE: Usually, the maximum value of GhrShamt is NumBtbResultEntries, but in reality, the maximum value is NumBtbResultEntries+ 1
  private val s3_numLess  = PopCount(s3_lessThanFirstTaken)
  private val s3_numHit   = PopCount(s3_hitMask)
  private val s3_commonHR = WireInit(0.U.asTypeOf(new CommonHREntry))

  s3_commonHR.valid           := s3_fire
  s3_commonHR.predStartPc.get := s3_update.startPc
  s3_commonHR.ghr := getNewHR(commonHR.ghr, s3_numLess, s3_numHit, s3_taken, s3_firstTakenIsCond)(GhrHistoryLength)
  s3_commonHR.bw := getNewHR(
    commonHR.bw,
    s3_numLess,
    s3_numHit,
    s3_taken,
    s3_firstTakenIsCond,
    Option(s3_taken && s3_bwTaken)
  )(BWHistoryLength)

  /*
   * redirect recovery CommonHR
   */
  private val r0_valid        = io.redirect.valid
  private val r0_metaGhr      = io.redirect.meta.ghr
  private val r0_metaBW       = io.redirect.meta.bw
  private val r0_oldPositions = io.redirect.meta.position
  private val r0_oldCondHits = VecInit(io.redirect.meta.hitMask.zip(io.redirect.meta.attribute).map {
    case (hit, attr) => hit && attr.isConditional
  })
  private val r0_oldHits = dedupHitPositions(r0_oldCondHits, r0_oldPositions)
  private val r0_taken   = io.redirect.taken
  private val r0_isCond  = io.redirect.attribute.isConditional
  private val r0_bwTaken =
    io.redirect.cfiPc.addr > io.redirect.target.addr
  private val r0_takenPosition = getAlignedInstOffset(io.redirect.cfiPc)
  private val r0_lessThanPc = r0_oldPositions.zip(r0_oldHits).map {
    case (pos, hit) => hit && (pos < r0_takenPosition)
  } // positions less than redirect branch pc
  private val r0_numLess  = PopCount(r0_lessThanPc)
  private val r0_numHit   = PopCount(r0_oldHits)
  private val r0_commonHR = WireInit(0.U.asTypeOf(new CommonHREntry))
  r0_commonHR.valid           := false.B
  r0_commonHR.predStartPc.get := io.s0_startPc.get
  r0_commonHR.ghr             := getNewHR(r0_metaGhr, r0_numLess, r0_numHit, r0_taken, r0_isCond)(GhrHistoryLength)
  r0_commonHR.bw := getNewHR(r0_metaBW, r0_numLess, r0_numHit, r0_taken, r0_isCond, Option(r0_bwTaken && r0_taken))(
    BWHistoryLength
  )

  // update from redirect or update
  when(r0_valid) {
    commonHR := r0_commonHR // TODO: redirect commonHR recovery can delay one/two cycle
  }.elsewhen(s3_fire) {
    commonHR := s3_commonHR
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
    enqPtr                    := writePtr + 1.U
    recoverPtr                := writePtr
    predPtr                   := writePtr
    histQueue(writePtr.value) := initCommonHR // The queue value during redirect is used for diff
  }.elsewhen(s3_override) {
    val realRecoverPtr = Mux(hasOverrideHist, recoverPtr + 1.U, recoverPtr)
    histQueue(writePtr.value)         := s3_commonHR  // update s3_fire block
    histQueue((writePtr + 1.U).value) := initCommonHR // write new s0_block
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
      histQueue(writePtr.value) := s3_commonHR
      writePtr                  := writePtr + 1.U
      recoverPtr                := Mux(recoverInc, recoverPtr + 1.U, recoverPtr)
    }
  }

  XSError(enqEnable && (writePtr < predPtr || predPtr < recoverPtr), "The predPtr exceeds the correct range")
  XSError(
    writeEnable && s3_update.startPc =/= histQueue(writePtr.value).predStartPc.get,
    "update history maybe mismatched!"
  )

  s0_commonHR := MuxCase(
    0.U.asTypeOf(new CommonHREntry),
    Seq(
      r0_valid          -> r0_commonHR,
      s3_override       -> histQueue(recoverPtr.value),
      (s0_fire && sync) -> s3_commonHR, // bypass s3_commonHR
      s0_fire           -> histQueue(predPtr.value)
    )
  )

  dontTouch(writePtr)
  dontTouch(enqPtr)
  dontTouch(predPtr)
  dontTouch(predEnable)
  dontTouch(sync)
  dontTouch(hasOverrideHist)
  dontTouch(recoverInc)

  if (EnableCommitGHistDiff) {
    val s3_lessThanFirstTakenUInt = s3_lessThanFirstTaken.asUInt
    val r0_lessThanPcUInt         = r0_lessThanPc.asUInt
    val ghrUInt                   = commonHR.ghr.asUInt
    val bwUInt                    = commonHR.bw.asUInt
    dontTouch(s3_numLess)
    dontTouch(s3_commonHR)
    dontTouch(s3_lessThanFirstTakenUInt)
    dontTouch(r0_numLess)
    dontTouch(r0_commonHR)
    dontTouch(r0_lessThanPcUInt)
    dontTouch(ghrUInt)
    dontTouch(bwUInt)
  }
}
