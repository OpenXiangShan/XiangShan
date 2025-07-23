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

package xiangshan.frontend.bpu.abtb

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.ReplacementPolicy

/**
  * This module implement the replacement policy for the ahead BTB.
  */
class AheadBtbReplacer(implicit p: Parameters) extends AheadBtbModule {
  val io: ReplacerIO = IO(new ReplacerIO)

  private val replacer = ReplacementPolicy.fromString(Some("setplru"), NumWays, NumSets)

  when(io.writeValid) {
    replacer.access(io.writeSetIdx, io.writeWayIdx)
  }.elsewhen(io.readValid) {
    val touchSets = Seq.fill(NumWays)(io.readSetIdx)
    val touchWays = Seq.fill(NumWays)(Wire(Valid(UInt(WayIdxWidth.W))))
    touchWays.zip(io.readWayMask).zipWithIndex.foreach { case ((t, r), i) =>
      t.valid := r
      t.bits  := i.U
    }
    replacer.access(touchSets, touchWays)
  }

  io.victimWayIdx := replacer.way(io.replaceSetIdx)
}
