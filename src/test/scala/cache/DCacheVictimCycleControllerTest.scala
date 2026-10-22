package cache

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.flatspec.AnyFlatSpec
import xiangshan.mem.prefetch._

class DCacheVictimCycleControllerTest extends AnyFlatSpec with ChiselSim {
  private def init(c: PABDepthController): Unit = {
    c.io.enabled.poke(true.B); c.io.fixedDepth.poke(64.U)
    c.io.events.insert.poke(false.B); c.io.events.late.poke(0.U); c.io.events.unused.poke(0.U)
    c.reset.poke(true.B); c.clock.step(3); c.reset.poke(false.B)
  }
  behavior of "Cycle windows with exact per-fill normalization"
  it should "check only at period boundaries, carry undersampled events and include the closing fill" in {
    simulate(new PABDepthController(PABDepthParameters(windowCycles = 8, minWindowFills = 4, settleWindows = 0))) { c =>
      init(c)
      c.io.events.late.poke(1.U); c.clock.step(); c.io.events.late.poke(0.U)
      c.clock.step(6)
      c.io.cycleCheck.expect(true.B); c.io.deferred.expect(true.B); c.io.window.valid.expect(false.B)
      c.clock.step()
      c.io.events.insert.poke(true.B); c.clock.step(3); c.io.events.insert.poke(false.B)
      c.clock.step(4); c.io.window.valid.expect(false.B)
      c.io.events.insert.poke(true.B)
      c.io.window.valid.expect(true.B); c.io.window.bits.fills.expect(4.U)
      c.io.window.bits.cycles.expect(16.U); c.io.window.bits.late.expect(1.U)
      c.io.window.bits.latePressure.expect(2.U); c.io.window.bits.depthAfter.expect(24.U)
      c.clock.step(); c.io.events.insert.poke(false.B); c.io.depth.expect(24.U); c.io.inserts.expect(0.U)
    }
  }
  it should "preserve fractional rate boundaries instead of rounding normalized counts" in {
    simulate(new PABDepthController(PABDepthParameters(windowCycles = 32, minWindowFills = 1, settleWindows = 0))) { c =>
      // The same single event means <=20, (20,40), or >=40 events per500 fills.
      for ((fills, late, unused, lp, up) <- Seq((25,1,2,0,0), (24,1,2,1,1),
        (13,1,2,1,1), (12,1,2,2,2), (10,1,1,2,1))) {
        init(c)
        for (i <- 0 until 31) {
          c.io.events.insert.poke((i < fills).B)
          c.io.events.late.poke((if (i == 0) late else 0).U)
          c.io.events.unused.poke((if (i == 0) unused else 0).U)
          c.io.window.valid.expect(false.B); c.clock.step()
        }
        c.io.events.insert.poke(false.B); c.io.events.late.poke(0.U); c.io.events.unused.poke(0.U)
        c.io.window.valid.expect(true.B); c.io.window.bits.fills.expect(fills.U)
        c.io.window.bits.latePressure.expect(lp.U); c.io.window.bits.unusedPressure.expect(up.U)
      }
    }
  }
  it should "retain credit and settling across deferred checks and honor fixed mode" in {
    simulate(new PABDepthController(PABDepthParameters(windowCycles = 32, minWindowFills = 10))) { c =>
      init(c)
      def period(fills: Int, late: Int): Unit = {
        for (i <- 0 until 32) {
          c.io.events.insert.poke((i < fills).B)
          c.io.events.late.poke((if (i == 0) late else 0).U)
          c.clock.step()
        }
        c.io.events.insert.poke(false.B); c.io.events.late.poke(0.U)
      }
      // 1/20*500 =25: two weak windows, with a no-fill check between them.
      period(20,1); c.io.depth.expect(16.U)
      period(0,0); c.io.depth.expect(16.U)
      period(20,1); c.io.depth.expect(24.U)
      period(0,0); period(20,2); c.io.depth.expect(24.U) // settling applies only on a closed window
      period(20,2); c.io.depth.expect(32.U)
      c.io.enabled.poke(false.B); period(20,2); c.io.depth.expect(64.U)
    }
  }
  it should "retain more than65535 events during sparse default-period windows" in {
    simulate(new PABDepthController(PABDepthParameters(windowCycles = 16384, minWindowFills = 128))) { c =>
      init(c)
      c.io.events.late.poke(15.U); c.clock.step(16384); c.io.window.valid.expect(false.B)
      c.io.events.late.poke(0.U); c.io.events.insert.poke(true.B); c.clock.step(128)
      c.io.events.insert.poke(false.B); c.clock.step(16384-128-1)
      c.io.window.valid.expect(true.B); c.io.window.bits.late.expect(245760.U)
      c.io.window.bits.fills.expect(128.U); c.io.window.bits.cycles.expect(32768.U)
      c.io.window.bits.latePressure.expect(2.U)
    }
  }
}
