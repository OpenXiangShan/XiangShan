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
  private val s1_fire = io.stageCtrl.s1_fire
  private val s2_fire = io.stageCtrl.s2_fire
  private val s3_fire = io.stageCtrl.s3_fire

  // global history
  private val s0_ghr = WireInit(0.U.asTypeOf(new GhrEntry))
  private val ghr    = RegInit(0.U.asTypeOf(new GhrEntry))

  /*
   * GHR train from redirect/s3_prediction
   */
  io.s0_ghist := s0_ghr
  io.ghist    := ghr

  /*
   * s3_fire update GHR
   */
  private val s3_update             = io.update // bp pipeline s3 level update
  private val s3_taken              = s3_update.taken
  private val s3_firstTakenIdx      = OHToUInt(s3_update.firstTakenOH)
  private val s3_firstTakenPos      = s3_update.position(s3_firstTakenIdx)
  private val s3_lessThanFirstTaken = s3_update.position.map(_ < s3_firstTakenPos)
  // NOTE: GhrShamt is NumBtbResultEntries, but numLess may be 1 larger than GhrShamt
  private val s3_numLess   = PopCount(s3_lessThanFirstTaken)
  private val s3_updateGhr = getNewGhr(ghr.value, s3_numLess, s3_taken)
  require(isPow2(GhrShamt), "GhrShamt must be pow2")

  /*
   * redirect recovery GHR
   */
  private val r0_valid         = io.redirect.valid
  private val r0_metaGhr       = io.redirect.meta.ghr
  private val r0_oldPositions  = io.redirect.meta.position
  private val r0_taken         = io.redirect.taken
  private val r0_takenPosition = getAlignedInstOffset(io.redirect.startVAddr) // FIXME: position calculate maybe wrong
  private val r0_lessThanPc    = r0_oldPositions.map(_ < r0_takenPosition)    // positions less than redirect branch pc
  private val r0_numLess       = PopCount(r0_lessThanPc)
  // TODO: calculate the new ghr based on redirect info maybe need more cycles
  private val r0_updateGhr = getNewGhr(r0_metaGhr, r0_numLess, r0_taken)
  // update from redirect or update
  s0_ghr.valid := s3_fire && ghr.valid // gTable prediction can only begin when s3_fire
  s0_ghr.value := ghr.value
  when(r0_valid) {
    ghr.valid := true.B
    ghr.value := r0_updateGhr // TODO: redirect ghr recovery can delay one/two cycle
  }.elsewhen(s3_fire) {
    ghr.valid    := true.B
    ghr.value    := s3_updateGhr
    s0_ghr.valid := true.B // if s3_fire, ghr can be used
    s0_ghr.value := s3_updateGhr
  }

  if (EnableCommitGHistDiff) {
    val s3_updateGhrUInt          = s3_updateGhr.asUInt
    val s3_lessThanFirstTakenUInt = s3_lessThanFirstTaken.asUInt
    val r0_updateGhrUInt          = r0_updateGhr.asUInt
    val r0_lessThanPcUInt         = r0_lessThanPc.asUInt
    val ghrUInt                   = ghr.value.asUInt
    dontTouch(s3_numLess)
    dontTouch(s3_updateGhrUInt)
    dontTouch(s3_lessThanFirstTakenUInt)
    dontTouch(r0_numLess)
    dontTouch(r0_updateGhrUInt)
    dontTouch(r0_lessThanPcUInt)
    dontTouch(ghrUInt)
  }
}
