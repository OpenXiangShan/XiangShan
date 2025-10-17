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

class Ghr(implicit p: Parameters) extends GhrModule with Helpers {
  class GhrIO extends GhrBundle {
    val update:   GhrUpdate   = Input(new GhrUpdate)
    val redirect: GhrRedirect = Input(new GhrRedirect)
    val ghist:    UInt        = Output(UInt(GhrHistoryLength.W))
    val ghrPtr:   GhrPtr      = Output(new GhrPtr)
  }
  val io          = IO(new GhrIO)
  private val ghr = RegInit(0.U.asTypeOf(Vec(GhrHistoryLength, Bool())))

  /*
   * GHR train from redirct/s3_prediction
   */
  private val ghrPtr = RegInit(0.U.asTypeOf(new GhrPtr))

  io.ghist  := getGhr(ghr.asUInt, ghrPtr)
  io.ghrPtr := ghrPtr

  // update GHR
  private val update        = io.update
  private val taken         = update.taken
  private val updateValid   = update.valid
  private val firstTakenIdx = OHToUInt(update.firstTakenOH)
  private val firstTakenPos = update.position(firstTakenIdx)

  private val lessThanFirstTaken = update.position.map(_ < firstTakenPos)
  private val numLess            = PopCount(lessThanFirstTaken)

  private val resultBits = Wire(Vec(NumBtbResultEntries, Bool()))
  for (i <- 0 until NumBtbResultEntries) {
    resultBits(i) := Mux(i.U === numLess, taken, false.B)
  }
  private val shiftBits = Mux(taken, resultBits, 0.U.asTypeOf(resultBits))

  private val updateGhr = WireInit(ghr)
  for (i <- 0 until NumBtbResultEntries) {
    updateGhr(ghrPtr.value - i.U) := shiftBits(i)
  }

  // redirect handling
  private val redirect = io.redirect
  when(redirect.valid) {
    // ghr    := getGhr(ghr.asUInt, redirect.bits.speculationMeta.ghrHistPtr)
    ghrPtr := redirect.ghrPtr
  }.elsewhen(updateValid && taken) {
    ghr    := updateGhr
    ghrPtr := ghrPtr - numLess - 1.U
  }.elsewhen(updateValid && !taken) {
    ghr    := updateGhr
    ghrPtr := ghrPtr - NumBtbResultEntries.U
  }

  private val shiftBitsUInt          = shiftBits.asUInt
  private val updateGhrUInt          = updateGhr.asUInt
  private val ghrUInt                = ghr.asUInt
  private val lessThanFirstTakenUInt = lessThanFirstTaken.asUInt
  dontTouch(shiftBitsUInt)
  dontTouch(updateGhrUInt)
  dontTouch(ghrUInt)
  dontTouch(lessThanFirstTakenUInt)

}
