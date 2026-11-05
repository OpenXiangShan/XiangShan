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

package xiangshan.frontend

import chisel3._
import chisel3.simulator.EphemeralSimulator._ // scalastyle:ignore
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import top.DefaultConfig
import xiangshan.TopDownCounters
import xiangshan.XSCoreParamsKey
import xiangshan.XSTileKey

class TopDownPipelineTest extends AnyFlatSpec with should.Matchers {
  private val defaultConfig = new DefaultConfig
  private implicit val config: Parameters = defaultConfig.alterPartial {
    case XSCoreParamsKey => defaultConfig(XSTileKey).head
  }

  private def reset(dut: Module): Unit = {
    dut.reset.poke(true.B)
    dut.clock.step()
    dut.reset.poke(false.B)
  }

  behavior of "FrontendTopDownPipeline"

  for (numStages <- Seq(1, 3); broadcast <- Seq(false, true)) {
    it should s"accept mixed reason types with $numStages stages and broadcast=$broadcast" in {
      class TestModule extends Module {
        class TestIO extends Bundle {
          val in:         TopdownBundle         = Input(new TopdownBundle)
          val enumerated: Bool                         = Input(Bool())
          val encoded:    Valid[TopdownBundle]  = Flipped(Valid(new TopdownBundle))
          val out:        TopdownBundle         = Output(new TopdownBundle)
        }
        val io: TestIO = IO(new TestIO)
        private val pipe = Module(new TopDownPipeline(numStages))
        pipe.io.in := io.in
        private val reasons = Seq(
          io.enumerated -> TopDownCounters.FetchFragBubble,
          io.encoded.valid -> io.encoded.bits.reason
        )
        if (broadcast) {
          pipe.setReason(reasons)
        } else {
          pipe.setReason(Seq.empty)
          pipe.setReason(0, reasons)
        }
        io.out := pipe.io.out
      }
      simulate(new TestModule) { dut =>
        dut.io.in.reason.poke(TopDownCounters.OverrideBubble.id.U)
        dut.io.enumerated.poke(false.B)
        dut.io.encoded.valid.poke(false.B)
        dut.io.encoded.bits.reason.poke(TopDownCounters.BTBMissBubble.id.U)
        reset(dut)
        dut.clock.step(numStages)
        dut.io.out.reason.expect(TopDownCounters.OverrideBubble.id.U)

        dut.io.enumerated.poke(true.B)
        dut.io.encoded.valid.poke(true.B)
        dut.io.out.reason.expect(TopDownCounters.OverrideBubble.id.U)
        dut.clock.step(numStages)
        dut.io.out.reason.expect(TopDownCounters.FetchFragBubble.id.U)

        dut.io.enumerated.poke(false.B)
        dut.clock.step(numStages)
        dut.io.out.reason.expect(TopDownCounters.BTBMissBubble.id.U)

        dut.io.encoded.bits.reason.poke(TopDownCounters.NoStall.id.U)
        dut.clock.step(numStages)
        dut.io.out.reason.expect(TopDownCounters.NoStall.id.U)

        dut.io.encoded.valid.poke(false.B)
        dut.clock.step(numStages)
        dut.io.out.reason.expect(TopDownCounters.OverrideBubble.id.U)
      }
    }
  }

  for (broadcast <- Seq(false, true)) {
    it should s"reject unknown types during elaboration with broadcast=$broadcast" in {
      class TestModule extends Module {
        private val pipe = Module(new TopDownPipeline(1))
        pipe.io.in.reason := TopDownCounters.NoStall.id.U
        if (broadcast) {
          pipe.setReason(Seq(123 -> "456"))
        } else {
          pipe.setReason(0, Seq(123 -> "456"))
        }
      }
      val exception = the[IllegalArgumentException] thrownBy { simulate(new TestModule) { _ => } }
      exception.getMessage should include("setReason reasons must be (Bool, UInt) or (Bool, TopDownCounters.Value)")
    }
  }

  for (numStages <- Seq(1, 2, 3)) {
    it should s"preserve cycle latency, local overrides and reset with $numStages stages" in {
      simulate(new TopDownPipeline(numStages)) { dut =>
        dut.io.in.reason.poke(TopDownCounters.NoStall.id.U)
        dut.io.over.foreach { over =>
          over.valid.poke(false.B)
          over.bits.reason.poke(TopDownCounters.BTBMissBubble.id.U)
        }
        reset(dut)
        dut.io.out.reason.expect(TopDownCounters.NoStall.id.U)

        // A single lost slot appears exactly numStages edges later.
        dut.io.in.reason.poke(TopDownCounters.OverrideBubble.id.U)
        dut.io.out.reason.expect(TopDownCounters.NoStall.id.U)
        dut.clock.step()
        dut.io.in.reason.poke(TopDownCounters.NoStall.id.U)
        if (numStages > 1) {
          dut.io.out.reason.expect(TopDownCounters.NoStall.id.U)
          dut.clock.step(numStages - 1)
        }
        dut.io.out.reason.expect(TopDownCounters.OverrideBubble.id.U)
        dut.clock.step()
        dut.io.out.reason.expect(TopDownCounters.NoStall.id.U)

        // A local event affects its own stage and subsequent stages only.
        for (stage <- 0 until numStages) {
          dut.io.over(stage).valid.poke(true.B)
          dut.io.over(stage).bits.reason.poke(TopDownCounters.ITLBMissBubble.id.U)
          dut.io.out.reason.expect(TopDownCounters.NoStall.id.U)
          dut.clock.step()
          dut.io.over(stage).valid.poke(false.B)
          val remainingEdges = numStages - stage - 1
          if (remainingEdges > 0) {
            dut.io.out.reason.expect(TopDownCounters.NoStall.id.U)
            dut.clock.step(remainingEdges)
          }
          dut.io.out.reason.expect(TopDownCounters.ITLBMissBubble.id.U)
          dut.clock.step()
          dut.io.out.reason.expect(TopDownCounters.NoStall.id.U)
        }

        // One redirect replaces every pending slot, producing numStages bubbles.
        dut.io.over.foreach { over =>
          over.valid.poke(true.B)
          over.bits.reason.poke(TopDownCounters.MemVioRedirectBubble.id.U)
        }
        dut.clock.step()
        dut.io.over.foreach(_.valid.poke(false.B))
        for (_ <- 0 until numStages) {
          dut.io.out.reason.expect(TopDownCounters.MemVioRedirectBubble.id.U)
          dut.clock.step()
        }
        dut.io.out.reason.expect(TopDownCounters.NoStall.id.U)

        // NoStall is a valid override: BPU redirects must clear old-path tags.
        dut.io.in.reason.poke(TopDownCounters.OverrideBubble.id.U)
        dut.clock.step(numStages)
        dut.io.over.foreach { over =>
          over.valid.poke(true.B)
          over.bits.reason.poke(TopDownCounters.NoStall.id.U)
        }
        dut.clock.step()
        dut.io.in.reason.poke(TopDownCounters.NoStall.id.U)
        dut.io.over.foreach(_.valid.poke(false.B))
        for (_ <- 0 until numStages) {
          dut.io.out.reason.expect(TopDownCounters.NoStall.id.U)
          dut.clock.step()
        }

        dut.io.in.reason.poke(TopDownCounters.ICacheMissBubble.id.U)
        dut.clock.step(numStages)
        dut.io.out.reason.expect(TopDownCounters.ICacheMissBubble.id.U)
        dut.reset.poke(true.B)
        dut.clock.step()
        dut.io.out.reason.expect(TopDownCounters.NoStall.id.U)
      }
    }
  }

  it should "broadcast the first matching reason and ignore inactive events" in {
    class TestModule extends Module {
      class TestIO extends Bundle {
        val in:   TopdownBundle        = Input(new TopdownBundle)
        val high: Valid[TopdownBundle]  = Flipped(Valid(new TopdownBundle))
        val low:  Valid[TopdownBundle]  = Flipped(Valid(new TopdownBundle))
        val out:  TopdownBundle        = Output(new TopdownBundle)
      }
      val io: TestIO = IO(new TestIO)
      private val pipe = Module(new TopDownPipeline(3))
      pipe.io.in := io.in
      pipe.setReason(Seq(io.high.valid -> io.high.bits.reason, io.low.valid -> io.low.bits.reason))
      io.out := pipe.io.out
    }
    simulate(new TestModule) { dut =>
      dut.io.in.reason.poke(TopDownCounters.OverrideBubble.id.U)
      dut.io.high.valid.poke(false.B)
      dut.io.high.bits.reason.poke(TopDownCounters.MemVioRedirectBubble.id.U)
      dut.io.low.valid.poke(false.B)
      dut.io.low.bits.reason.poke(TopDownCounters.ICacheMissBubble.id.U)
      reset(dut)
      dut.clock.step(3)
      dut.io.out.reason.expect(TopDownCounters.OverrideBubble.id.U)

      dut.io.high.valid.poke(true.B)
      dut.io.low.valid.poke(true.B)
      dut.io.out.reason.expect(TopDownCounters.OverrideBubble.id.U)
      dut.clock.step()
      dut.io.out.reason.expect(TopDownCounters.MemVioRedirectBubble.id.U)

      dut.io.high.valid.poke(false.B)
      dut.clock.step()
      dut.io.out.reason.expect(TopDownCounters.ICacheMissBubble.id.U)

      dut.io.low.valid.poke(false.B)
      for (_ <- 0 until 3) {
        dut.io.out.reason.expect(TopDownCounters.ICacheMissBubble.id.U)
        dut.clock.step()
      }
      dut.io.out.reason.expect(TopDownCounters.OverrideBubble.id.U)
    }
  }

  it should "prioritize later calls and fall back to earlier matching events" in {
    class TestModule extends Module {
      class TestIO extends Bundle {
        val in:    TopdownBundle        = Input(new TopdownBundle)
        val low:   Valid[TopdownBundle]  = Flipped(Valid(new TopdownBundle))
        val local: Valid[TopdownBundle]  = Flipped(Valid(new TopdownBundle))
        val high:  Valid[TopdownBundle]  = Flipped(Valid(new TopdownBundle))
        val out:   TopdownBundle        = Output(new TopdownBundle)
      }
      val io: TestIO = IO(new TestIO)
      private val pipe = Module(new TopDownPipeline(3))
      pipe.io.in := io.in
      pipe.setReason(Seq(io.low.valid -> io.low.bits.reason))
      pipe.setReason(2, Seq(io.local.valid -> io.local.bits.reason))
      pipe.setReason(Seq(io.high.valid -> io.high.bits.reason))
      pipe.setReason(2, Seq.empty)
      io.out := pipe.io.out
    }
    simulate(new TestModule) { dut =>
      dut.io.in.reason.poke(TopDownCounters.OverrideBubble.id.U)
      dut.io.low.valid.poke(false.B)
      dut.io.low.bits.reason.poke(TopDownCounters.ICacheMissBubble.id.U)
      dut.io.local.valid.poke(false.B)
      dut.io.local.bits.reason.poke(TopDownCounters.ITLBMissBubble.id.U)
      dut.io.high.valid.poke(false.B)
      dut.io.high.bits.reason.poke(TopDownCounters.MemVioRedirectBubble.id.U)
      reset(dut)
      dut.clock.step(3)
      dut.io.out.reason.expect(TopDownCounters.OverrideBubble.id.U)

      // Inactive later calls must preserve the earlier broadcast.
      dut.io.low.valid.poke(true.B)
      dut.clock.step()
      dut.io.out.reason.expect(TopDownCounters.ICacheMissBubble.id.U)

      dut.io.local.valid.poke(true.B)
      dut.clock.step()
      dut.io.out.reason.expect(TopDownCounters.ITLBMissBubble.id.U)

      dut.io.high.valid.poke(true.B)
      dut.clock.step()
      dut.io.out.reason.expect(TopDownCounters.MemVioRedirectBubble.id.U)

      dut.io.high.valid.poke(false.B)
      dut.clock.step()
      dut.io.out.reason.expect(TopDownCounters.ITLBMissBubble.id.U)

      // A matching NoStall in the last call clears all earlier reasons.
      dut.io.high.valid.poke(true.B)
      dut.io.high.bits.reason.poke(TopDownCounters.NoStall.id.U)
      dut.clock.step()
      dut.io.out.reason.expect(TopDownCounters.NoStall.id.U)
    }
  }
}
