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

package xiangshan.frontend.bpu.history.ghr

import chisel3._
import chisel3.util._
import freechips.rocketchip.util.SeqToAugmentedSeq
import org.chipsalliance.cde.config.Parameters
import utility.XSError
import xiangshan.frontend.bpu.BpuRedirect
import xiangshan.frontend.bpu.StageCtrl

class Ghr(implicit p: Parameters) extends GhrModule with Helpers {
  class GhrIO extends GhrBundle {
    val stageCtrl: StageCtrl   = Input(new StageCtrl)
    val update:    GhrUpdate   = Input(new GhrUpdate)
    val redirect:  GhrRedirect = Input(new GhrRedirect)
    val s0_ghist:  GhrEntry    = Output(new GhrEntry)
    val ghist:     GhrEntry    = Output(new GhrEntry)
  }
  val io = IO(new GhrIO)

  // stage ctrl
  private val s0_fire = io.stageCtrl.s0_fire
  private val s2_fire = io.stageCtrl.s2_fire
  private val s3_fire = io.stageCtrl.s3_fire

  // global history
  private val s0_ghr    = WireInit(0.U.asTypeOf(new GhrEntry))
  private val ghr       = RegInit(0.U.asTypeOf(new GhrEntry))
  private val ghrBuffer = Module(new Queue(new GhrEntry, StallQueueSize, pipe = true, flow = true, hasFlush = true))

  /*
   * GHR train from redirect/s3_prediction
   */
  io.s0_ghist := s0_ghr
  io.ghist    := ghr

  /*
   * s3_fire update GHR
   */
  private val s3_update        = io.update // bp pipeline s3 level update
  private val s3_taken         = s3_update.taken
  private val s3_hitMask       = s3_update.hitMask
  private val s3_firstTakenIdx = OHToUInt(s3_update.firstTakenOH)
  private val s3_firstTakenPos = s3_update.position(s3_firstTakenIdx)
  private val s3_cfiPc         = getCfiPcFromPosition(s3_update.startPc, s3_firstTakenPos)
  private val s3_imliTaken =
    s3_cfiPc.addr > s3_update.target.addr // TODO: IMLI taken calculation may be wrong
  private val s3_lessThanFirstTaken = s3_update.position.zip(s3_hitMask).map {
    case (pos, hit) => hit && (pos < s3_firstTakenPos)
  }
  // NOTE: GhrShamt is NumBtbResultEntries, but numLess may be 1 larger than GhrShamt
  private val s3_numLess = PopCount(s3_lessThanFirstTaken)
  private val s3_numHit  = PopCount(s3_hitMask)
  private val s3_ghr     = WireInit(0.U.asTypeOf(new GhrEntry))
  s3_ghr.valid := true.B
  s3_ghr.ghr   := getNewGhr(ghr.ghr, s3_numLess, s3_numHit, s3_taken)(GhrHistoryLength)
  s3_ghr.imli  := getNewGhr(ghr.imli, s3_numLess, s3_numHit, s3_imliTaken)(ImliHistoryLength)
  require(isPow2(GhrShamt), "GhrShamt must be pow2")

  /*
   * redirect recovery GHR
   */
  private val r0_valid        = io.redirect.valid
  private val r0_metaGhr      = io.redirect.meta.ghr
  private val r0_metaImli     = io.redirect.meta.imli
  private val r0_oldPositions = io.redirect.meta.position
  private val r0_oldHits      = io.redirect.meta.hitMask
  private val r0_taken        = io.redirect.taken
  private val r0_imliTaken =
    io.redirect.cfiPc.addr > io.redirect.target.addr
  private val r0_takenPosition = getAlignedInstOffset(io.redirect.cfiPc)
  private val r0_lessThanPc = r0_oldPositions.zip(r0_oldHits).map {
    case (pos, hit) => hit && (pos < r0_takenPosition)
  } // positions less than redirect branch pc
  private val r0_numLess = PopCount(r0_lessThanPc)
  private val r0_numHit  = PopCount(r0_oldHits)
  // TODO: calculate the new ghr based on redirect info maybe need more cycles
  private val r0_ghr = WireInit(0.U.asTypeOf(new GhrEntry))
  r0_ghr.valid := false.B
  r0_ghr.ghr   := getNewGhr(r0_metaGhr, r0_numLess, r0_numHit, r0_taken)(GhrHistoryLength)
  r0_ghr.imli  := getNewGhr(r0_metaImli, r0_numLess, r0_numHit, r0_imliTaken)(ImliHistoryLength)

  // update from redirect or update
  when(r0_valid) {
    ghr := r0_ghr // TODO: redirect ghr recovery can delay one/two cycle
  }.elsewhen(s3_fire) {
    ghr := s3_ghr
  }

  // avoid losing updates due to !s0_fire
  ghrBuffer.io.enq.valid := s3_fire
  ghrBuffer.io.enq.bits  := s3_ghr
  ghrBuffer.io.flush.get := r0_valid
  ghrBuffer.io.deq.ready := s0_fire

  XSError(s3_fire && !ghrBuffer.io.enq.ready, "GHR stall queue overflow!\n")

  s0_ghr := Mux(r0_valid, r0_ghr, Mux(s0_fire && ghrBuffer.io.deq.valid, ghrBuffer.io.deq.bits, 0.U.asTypeOf(ghr)))

  if (EnableCommitGHistDiff) {
    val s3_lessThanFirstTakenUInt = s3_lessThanFirstTaken.asUInt
    val r0_lessThanPcUInt         = r0_lessThanPc.asUInt
    val ghrUInt                   = ghr.ghr.asUInt
    val imliUInt                  = ghr.imli.asUInt
    dontTouch(s3_numLess)
    dontTouch(s3_ghr)
    dontTouch(s3_lessThanFirstTakenUInt)
    dontTouch(r0_numLess)
    dontTouch(r0_ghr)
    dontTouch(r0_lessThanPcUInt)
    dontTouch(ghrUInt)
    dontTouch(imliUInt)
  }
}
