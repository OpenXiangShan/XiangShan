// Copyright (c) 2026 Institute of Computing Technology, Chinese Academy of Sciences
// Licensed under Mulan PSL v2. See LICENSE.
package xiangshan

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import difftest.common.LogPerfControl
import org.scalatest.flatspec.AnyFlatSpec

class TeaCycleStatsTest extends AnyFlatSpec with ChiselSim {
  behavior of "TEA cycle statistics"

  class PerfSink extends Module {
    val io = IO(Output(new LogPerfControl))
    io := LogPerfControl()
  }

  class PerfHarness extends Module {
    val io = IO(new Bundle {
      val inRoi = Input(Bool())
      val clean = Input(Bool())
      val dump = Input(Bool())
      val observed = Output(new LogPerfControl)
    })
    val control = WireInit(0.U.asTypeOf(new LogPerfControl))
    control.timer := 123.U
    control.logEnable := true.B
    control.inRoi := io.inRoi
    control.clean := io.clean
    control.dump := io.dump
    LogPerfControl.setSource(control)
    val sink = Module(new PerfSink)
    io.observed := sink.io
  }

  it should "wire measurement control across modules without coupling it to counter dumps" in {
    simulate(new PerfHarness) { dut =>
      for (roi <- Seq(false, true); clean <- Seq(false, true); dump <- Seq(false, true)) {
        dut.io.inRoi.poke(roi.B)
        dut.io.clean.poke(clean.B)
        dut.io.dump.poke(dump.B)
        dut.io.observed.inRoi.expect(roi.B)
        dut.io.observed.clean.expect(clean.B)
        dut.io.observed.dump.expect(dump.B)
        dut.io.observed.timer.expect(123.U)
        dut.io.observed.logEnable.expect(true.B)
      }
    }
  }

  it should "conserve time, separate censored intervals and reset gaps across disabled windows" in {
    simulate(new TeaCycleStats) { dut =>
      dut.io.dump.poke(false.B)
      var cycle = 0
      def drive(enabled: Boolean, state: Int, retired: Int = 0): Unit = {
        dut.io.enable.poke(enabled.B)
        dut.io.cycle.poke(cycle.U)
        dut.io.state.poke(state.U)
        dut.io.retired.poke(retired.U)
        dut.io.gapThreshold.poke(8.U)
      }
      def step(enabled: Boolean, state: Int, retired: Int = 0): Unit = {
        drive(enabled, state, retired)
        dut.clock.step()
        cycle += 1
      }
      // Beginning in a stall is left-censored even if it later ends with a commit.
      step(true, 1)
      step(true, 3)
      drive(true, 0, 2)
      dut.io.interval.valid.expect(true.B)
      dut.io.interval.bits.start.expect(0.U)
      dut.io.interval.bits.endCycle.expect(2.U)
      dut.io.interval.bits.leftCensored.expect(true.B)
      step(true, 0, 2)
      for (_ <- 0 until 8) step(true, 1)
      drive(true, 0, 3)
      dut.io.interval.valid.expect(true.B)
      dut.io.interval.bits.leftCensored.expect(false.B)
      dut.io.interval.bits.states(0).expect(8.U)
      step(true, 0, 3)
      step(true, 2)
      drive(false, 0)
      dut.io.summary.valid.expect(true.B)
      dut.io.summary.bits.cycle.expect(13.U)
      dut.io.summary.bits.states(0).expect(2.U)
      dut.io.summary.bits.states(1).expect(9.U)
      dut.io.summary.bits.states(2).expect(1.U)
      dut.io.summary.bits.states(3).expect(1.U)
      dut.io.summary.bits.retired.expect(5.U)
      dut.io.summary.bits.gaps(1).expect(1.U)
      dut.io.summary.bits.censoredGaps.expect(2.U)
      dut.io.summary.bits.pending.expect(0.U)
      dut.io.interval.bits.rightCensored.expect(true.B)
      step(false, 0)
      step(false, 0)
      step(true, 0, 1)
      for (_ <- 0 until 7) step(true, 1)
      step(true, 0, 1)
      drive(false, 0)
      dut.io.summary.bits.gaps(0).expect(1.U)
      dut.io.summary.bits.censoredGaps.expect(2.U)
      dut.io.summary.bits.retired.expect(7.U)
    }
  }

  it should "exclude warmup and include the final measured cycle in a requested snapshot" in {
    simulate(new TeaCycleStats) { dut =>
      dut.io.enable.poke(false.B)
      dut.io.dump.poke(true.B)
      dut.io.state.poke(TeaState.computing)
      dut.io.retired.poke(4.U)
      dut.io.gapThreshold.poke(128.U)
      for (cycle <- 0 until 10) {
        dut.io.cycle.poke(cycle.U)
        dut.io.summary.valid.expect(false.B)
        dut.io.interval.valid.expect(false.B)
        dut.clock.step()
      }
      dut.io.enable.poke(true.B)
      dut.io.dump.poke(false.B)
      dut.io.cycle.poke(10.U)
      dut.io.state.poke(TeaState.stalled)
      dut.io.retired.poke(0.U)
      dut.io.summary.valid.expect(true.B)
      dut.io.summary.bits.states(0).expect(0.U)
      dut.io.summary.bits.states(1).expect(1.U)
      dut.io.summary.bits.retired.expect(0.U)
      dut.clock.step(3)
      dut.io.cycle.poke(13.U)
      dut.io.state.poke(TeaState.computing)
      dut.io.retired.poke(2.U)
      dut.io.dump.poke(true.B)
      dut.io.summary.valid.expect(true.B)
      dut.io.summary.bits.cycle.expect(14.U)
      dut.io.summary.bits.states(0).expect(1.U)
      dut.io.summary.bits.states(1).expect(3.U)
      dut.io.summary.bits.retired.expect(2.U)
      dut.io.interval.valid.expect(true.B)
      dut.io.interval.bits.start.expect(10.U)
      dut.io.interval.bits.leftCensored.expect(true.B)
    }
  }

  it should "include the current cycle in periodic summaries and preserve an open gap" in {
    simulate(new TeaCycleStats) { dut =>
      dut.io.enable.poke(true.B)
      dut.io.dump.poke(false.B)
      dut.io.retired.poke(0.U)
      dut.io.state.poke(1.U)
      dut.io.gapThreshold.poke(128.U)
      for (cycle <- 0 until 4096) {
        dut.io.cycle.poke(cycle.U)
        dut.io.summary.valid.expect((cycle == 0 || cycle == 4095).B)
        if (cycle == 4095) {
          dut.io.summary.bits.cycle.expect(4096.U)
          dut.io.summary.bits.states(1).expect(4096.U)
          dut.io.summary.bits.pending.expect(4096.U)
          dut.io.summary.bits.closed.expect(false.B)
        }
        dut.clock.step()
      }
    }
  }
}
