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

package xiangshan.frontend.bpu.mbtb

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.ReplacementPolicy

class VictimBtbReplacer(implicit p: Parameters) extends MainBtbModule {
  class VictimBtbReplacerIO extends Bundle {
    val predTouch:  Valid[UInt] = Flipped(Valid(UInt(log2Up(NumVictimBtbWays).W)))
    val trainTouch: Valid[UInt] = Flipped(Valid(UInt(log2Up(NumVictimBtbWays).W)))
    val valids:     Vec[Bool]   = Input(Vec(NumVictimBtbWays, Bool()))
    val victim:     UInt        = Output(UInt(log2Up(NumVictimBtbWays).W))
  }

  val io: VictimBtbReplacerIO = IO(new VictimBtbReplacerIO)

  // The VBTB is fully associative, so use the same policy object as uBTB.
  // Prediction touch is applied before training touch, matching the ordering
  // used by the existing uBTB replacer for simultaneous accesses.
  private val replacer = ReplacementPolicy.fromString(VictimBtbReplacerPolicy, NumVictimBtbWays)

  replacer.access(Seq(io.predTouch, io.trainTouch))

  private val invalidMask = VecInit(io.valids.map(!_)).asUInt

  io.victim := PriorityMux(Seq(
    invalidMask.orR -> PriorityEncoder(invalidMask),
    true.B          -> replacer.way
  ))
}
