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
   * GHR train from redirct/s3_prediction
   */
  io.s0_ghist := s0_ghr
  io.ghist    := ghr

  // update GHR
  private val update        = io.update // bp pipeline s3 level update
  private val taken         = update.taken
  private val updateValid   = update.valid
  private val firstTakenIdx = OHToUInt(update.firstTakenOH)
  private val firstTakenPos = update.position(firstTakenIdx)

  private val lessThanFirstTaken = update.position.map(_ < firstTakenPos)
  private val numLess            = PopCount(lessThanFirstTaken)
  // FIXME: if numLess = GhrShamt maybe takenPtr error
  private val takenPtr = Mux(taken, ~numLess(log2Ceil(GhrShamt) - 1, 0), 0.U)
  require(isPow2(GhrShamt), "GhrShamt must be pow2")

  // Set High numLess bits to 0, the numLess bit is taken
  private val resultBits = VecInit(Seq.tabulate(GhrShamt)(i => Mux(i.U === takenPtr, taken, false.B)))
  private val shiftBits  = Mux(taken, resultBits, 0.U.asTypeOf(resultBits))
  private val catBits    = Cat(ghr.value.asUInt, shiftBits.asUInt)
  private val updateGhr  = VecInit(Seq.tabulate(GhrHistoryLength)(i => catBits(takenPtr + i.U)))

  // redirect ghr update
  private val oldPositions     = io.redirect.meta.position
  private val newTaken         = io.redirect.taken
  private val takenPosition    = getAlignedInstOffset(io.redirect.startVAddr) // FIXME: position calculate maybe wrong
  private val newLessThanStart = oldPositions.map(_ < takenPosition)
  private val newNumLess       = PopCount(newLessThanStart)
  private val newTakenPtr      = Mux(newTaken, ~newNumLess(log2Ceil(GhrShamt) - 1, 0), 0.U)

  // update from redirect or update
  when(io.redirect.valid) {
    ghr.valid := false.B
    ghr.value := io.redirect.meta.ghr // TODO: caclulate the new ghr based on redirect info
  }.elsewhen(updateValid) {
    ghr.valid := true.B
    ghr.value := updateGhr
  }
  s0_ghr.valid := Mux(io.redirect.valid, false.B, ghr.valid)
  s0_ghr.value := ghr.value

  private val resultBitsUInt         = resultBits.asUInt
  private val shiftBitsUInt          = shiftBits.asUInt
  private val updateGhrUInt          = updateGhr.asUInt
  private val ghrUInt                = ghr.value.asUInt
  private val lessThanFirstTakenUInt = lessThanFirstTaken.asUInt
  dontTouch(resultBitsUInt)
  dontTouch(numLess)
  dontTouch(shiftBitsUInt)
  dontTouch(updateGhrUInt)
  dontTouch(ghrUInt)
  dontTouch(lessThanFirstTakenUInt)

}
