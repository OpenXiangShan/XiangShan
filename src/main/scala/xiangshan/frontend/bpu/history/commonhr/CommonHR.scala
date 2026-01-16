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
import utility.XSError
import xiangshan.frontend.bpu.BpuRedirect
import xiangshan.frontend.bpu.StageCtrl

class CommonHR(implicit p: Parameters) extends CommonHRModule with Helpers {
  class CommonHRIO extends CommonHRBundle {
    val stageCtrl:    StageCtrl        = Input(new StageCtrl)
    val s1_imliTaken: Bool             = Input(Bool())
    val update:       CommonHRUpdate   = Input(new CommonHRUpdate)
    val redirect:     CommonHRRedirect = Input(new CommonHRRedirect)
    val s0_imli:      UInt             = Output(UInt(ImliWidth.W))
    val s3_imli:      UInt             = Output(UInt(ImliWidth.W))
    val s0_commonHR:  CommonHREntry    = Output(new CommonHREntry)
    val s3_commonHR:  CommonHREntry    = Output(new CommonHREntry)
  }
  val io = IO(new CommonHRIO)

  // stage ctrl
  private val s0_fire = io.stageCtrl.s0_fire
  private val s1_fire = io.stageCtrl.s1_fire
  private val s2_fire = io.stageCtrl.s2_fire
  private val s3_fire = io.stageCtrl.s3_fire

  // common history register
  private val s0_imli     = WireInit(0.U(ImliWidth.W))
  private val s1_imli     = RegEnable(s0_imli, s0_fire)
  private val s2_imli     = RegEnable(s1_imli, s1_fire)
  private val s3_imli     = RegEnable(s2_imli, s2_fire)
  private val imli        = RegInit(0.U(ImliWidth.W))
  private val s0_commonHR = WireInit(0.U.asTypeOf(new CommonHREntry))
  private val s3_commonHR = WireInit(0.U.asTypeOf(new CommonHREntry))
  private val commonHR    = RegInit(0.U.asTypeOf(new CommonHREntry))
  private val commonHRBuffer =
    Module(new Queue(new CommonHREntry, StallQueueSize, pipe = true, flow = true, hasFlush = true))

  /*
   * CommonHR train from redirect/s3_prediction
   */
  io.s0_imli     := s0_imli
  io.s3_imli     := s3_imli
  io.s0_commonHR := s0_commonHR
  io.s3_commonHR := s3_commonHR

  /*
   * s3_fire update CommonHR
   */
  private val s3_update   = io.update // bp pipeline s3 level update
  private val s3_taken    = s3_update.taken
  private val s3_override = s3_update.s3override

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
  private val s3_numLess = PopCount(s3_lessThanFirstTaken)
  private val s3_numHit  = PopCount(s3_hitMask)

  s3_commonHR.valid := true.B
  s3_commonHR.ghr   := getNewGhr(commonHR.ghr, s3_numLess, s3_numHit, s3_taken, s3_firstTakenIsCond)(GhrHistoryLength)
  s3_commonHR.bw := getNewBW(commonHR.bw, s3_numLess, s3_numHit, s3_taken, s3_firstTakenIsCond, s3_bwTaken)(
    BWHistoryLength
  )

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
  r0_commonHR.valid := false.B
  r0_commonHR.ghr   := getNewGhr(r0_metaGhr, r0_numLess, r0_numHit, r0_taken, r0_isCond)(GhrHistoryLength)
  r0_commonHR.bw    := getNewBW(r0_metaBW, r0_numLess, r0_numHit, r0_taken, r0_isCond, r0_bwTaken)(BWHistoryLength)

  // update from redirect or update
  when(r0_valid) {
    commonHR := r0_commonHR // TODO: redirect commonHR recovery can delay one/two cycle
  }.elsewhen(s3_fire) {
    commonHR := s3_commonHR
  }

  // imli update
  when(r0_valid) {
    val r0_newImli = Mux(r0_taken && r0_bwTaken && r0_isCond, io.redirect.meta.imli + 1.U, 0.U)
    imli    := r0_newImli
    s0_imli := r0_newImli
  }.elsewhen(s3_override) {
    val s3_newImli = Mux(s3_taken && s3_bwTaken && s3_firstTakenIsCond, s3_imli + 1.U, 0.U)
    imli    := s3_newImli
    s0_imli := s3_newImli
  }.elsewhen(s1_fire) {
    val s1_newImli = Mux(io.s1_imliTaken, s1_imli + 1.U, 0.U)
    imli    := s1_newImli
    s0_imli := s1_newImli
  }.otherwise {
    s0_imli := imli
  }

  // avoid losing updates due to !s0_fire
  commonHRBuffer.io.enq.valid := s3_fire
  commonHRBuffer.io.enq.bits  := s3_commonHR
  commonHRBuffer.io.flush.get := r0_valid
  commonHRBuffer.io.deq.ready := s0_fire

  XSError(s3_fire && !commonHRBuffer.io.enq.ready, "CommonHR stall queue overflow!\n")

  s0_commonHR := Mux(
    r0_valid,
    r0_commonHR,
    Mux(s0_fire && commonHRBuffer.io.deq.valid, commonHRBuffer.io.deq.bits, 0.U.asTypeOf(commonHR))
  )

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
