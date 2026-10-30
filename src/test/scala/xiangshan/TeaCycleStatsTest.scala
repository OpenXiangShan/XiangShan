// Copyright (c) 2026 Institute of Computing Technology, Chinese Academy of Sciences
// Licensed under Mulan PSL v2. See LICENSE.
package xiangshan

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.flatspec.AnyFlatSpec

class TeaCycleStatsTest extends AnyFlatSpec with ChiselSim {
  behavior of "TEA cycle statistics"

  it should "conserve time, separate censored intervals and reset gaps across disabled windows" in {
    simulate(new TeaCycleStats) { dut =>
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
      dut.io.interval.bits.end.expect(2.U)
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

  it should "include the current cycle in periodic summaries and preserve an open gap" in {
    simulate(new TeaCycleStats) { dut =>
      dut.io.enable.poke(true.B)
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
