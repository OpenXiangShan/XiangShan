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
import chisel3.simulator.EphemeralSimulator._ // scalastyle:ignore
import org.scalatest.flatspec.AnyFlatSpec
import xiangshan.frontend.TwoPrefetchCase
import xiangshan.frontend.ftq.prefetch.PrefetchSource

class PrefetchPerfTest extends AnyFlatSpec {
  private class Harness extends Module {
    val io = IO(new Bundle {
      val two = Input(new TwoPrefetchCase)
      val firstCrossLine = Input(Bool())
      val sources = Input(Vec(2, new PrefetchSource))
      val lineSources = Output(Vec(2, new PrefetchSource))
    })
    io.lineSources := PrefetchPerf.lineSources(io.two, io.firstCrossLine, io.sources)
  }

  behavior of "Prefetch physical-line attribution"

  it should "use the first covering request for all overlap shapes and source pairs" in {
    // Expected ownership is derived from virtual line coverage, independently of the mux equations.
    // (case, req0 line indices, req1 line indices, physical read-port line indices)
    val shapes = Seq(
      (0, Seq(0), Seq(4), Seq(0)),
      (0, Seq(0, 1), Seq(4), Seq(0, 1)),
      (1, Seq(0), Seq(0), Seq(0)),
      (1, Seq(0), Seq(0, 1), Seq(0, 1)),
      (1, Seq(0, 1), Seq(0), Seq(0, 1)),
      (1, Seq(0, 1), Seq(0, 1), Seq(0, 1)),
      (2, Seq(0, 1), Seq(1), Seq(0, 1)),
      (4, Seq(1), Seq(0, 1), Seq(0, 1)),
      (8, Seq(0), Seq(3), Seq(0, 3))
    )
    simulate(new Harness) { dut =>
      dut.reset.poke(true.B)
      dut.clock.step()
      dut.reset.poke(false.B)
      for {
        source0 <- 0 until 3
        source1 <- 0 until 3
        (two, first, second, lines) <- shapes
      } {
        dut.io.two.value.poke(two.U)
        dut.io.firstCrossLine.poke((first.size == 2).B)
        dut.io.sources(0).value.poke(source0.U)
        dut.io.sources(1).value.poke(source1.U)
        lines.zipWithIndex.foreach { case (line, port) =>
          assert(first.contains(line) || second.contains(line))
          val expected = if (first.contains(line)) source0 else source1
          dut.io.lineSources(port).value.expect(expected.U)
        }
      }
    }
  }
}
