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

class Replacer(implicit p: Parameters) extends AheadBtbModule {
  val io: ReplacerIO = IO(new ReplacerIO)

  private val replacer = ReplacementPolicy.fromString(Some("setplru"), NumWays, NumSets)

  // select first not-useful entry
  private val notUsefulVec = VecInit(io.usefulCounter.map(_.isSaturateNegative))
  private val notUseful    = notUsefulVec.reduce(_ || _)
  private val notUsefulIdx = PriorityEncoder(notUsefulVec)

  // if all entries are useful, select by replacement policy
  io.victimWayIdx := Mux(notUseful, notUsefulIdx, replacer.way(io.needReplaceSetIdx))

  when(io.writeValid) {
    replacer.access(io.writeSetIdx, io.writeWayIdx)
  }.elsewhen(io.readValid) {
    io.readHitMask.zipWithIndex.foreach { case (hit, i) =>
      when(hit)(replacer.access(io.readSetIdx, i.U))
    }
  }
}
