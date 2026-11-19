// Copyright (c) 2026 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2026 Institute of Computing Technology, Chinese Academy of Sciences
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

package xiangshan.frontend.icache

import chisel3._
import xiangshan.frontend.TwoPrefetchCase
import xiangshan.frontend.ftq.prefetch.PrefetchSource

/** Observation-only ownership of the two physical lines selected by TwoPrefetchCase.
  * If both logical requests cover a line, the first request owns it. This is not a usefulness metric.
  */
object PrefetchPerf {
  def sources: Seq[(String, UInt)] = Seq(
    "Fdip" -> PrefetchSource.Value.Fdip,
    "Sw"   -> PrefetchSource.Value.Sw,
    "Pdip" -> PrefetchSource.Value.Pdip
  )

  def lineSources(two: TwoPrefetchCase, firstCrossLine: Bool, sources: Vec[PrefetchSource]): Vec[PrefetchSource] = {
    val result = Wire(Vec(2, new PrefetchSource))
    result(0) := Mux(two.isOverlap2, sources(1), sources(0))
    result(1) := Mux(
      two.isInterleave || (two.isSameLine && !firstCrossLine),
      sources(1),
      sources(0)
    )
    result
  }
}
