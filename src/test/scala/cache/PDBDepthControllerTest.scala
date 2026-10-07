package cache

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.flatspec.AnyFlatSpec
import xiangshan.mem.prefetch._

class PDBDepthControllerTest extends AnyFlatSpec with ChiselSim {
  behavior of "PDB competitive depth"

  private def init(c: PDBDepthController, enabled: Boolean = true, fixed: Int = 64): Unit = {
    c.io.enabled.poke(enabled.B); c.io.fixedDepth.poke(fixed.U)
    c.io.events.refill.poke(false.B)
    c.io.events.late.poke(0.U); c.io.events.used.poke(0.U); c.io.events.unused.poke(0.U)
    c.reset.poke(true.B); c.clock.step(3); c.reset.poke(false.B)
  }

  // Events need not coincide with refills. In particular a no-refill interval
  // must accumulate pressure without closing a window or changing depth.
  private def inject(c: PDBDepthController, late: Int = 0, unused: Int = 0, used: Int = 0): Unit = {
    c.io.events.refill.poke(false.B)
    var l = late; var u = unused; var v = used
    while (l > 0 || u > 0 || v > 0) {
      val dl = l.min(15); val du = u.min(3); val dv = v.min(3)
      c.io.events.late.poke(dl.U); c.io.events.unused.poke(du.U); c.io.events.used.poke(dv.U)
      c.io.window.valid.expect(false.B); c.clock.step()
      l -= dl; u -= du; v -= dv
    }
    c.io.events.late.poke(0.U); c.io.events.unused.poke(0.U); c.io.events.used.poke(0.U)
  }

  private def window(c: PDBDepthController, late: Int = 0, unused: Int = 0, used: Int = 0): Unit = {
    inject(c, late, unused, used)
    c.io.events.refill.poke(true.B)
    c.clock.step(499)
    c.io.refills.expect(499.U); c.io.window.valid.expect(true.B)
    c.io.window.bits.late.expect(late.U)
    c.io.window.bits.unused.expect(unused.U)
    c.io.window.bits.used.expect(used.U)
    c.clock.step()
    c.io.events.refill.poke(false.B)
    c.io.refills.expect(0.U); c.io.window.valid.expect(false.B)
  }

  it should "include closing-edge events, retain partial windows and quantize exact thresholds" in {
    simulate(new PDBDepthController(PDBDepthParameters(settleWindows = 0))) { c =>
      init(c)
      inject(c, late = 39, unused = 80)
      c.io.depth.expect(16.U)
      c.io.events.refill.poke(true.B); c.clock.step(499)
      c.io.events.refill.poke(false.B); c.clock.step(10)
      c.io.refills.expect(499.U); c.io.depth.expect(16.U)
      c.io.events.refill.poke(true.B); c.io.events.late.poke(1.U)
      c.io.window.bits.late.expect(40.U)
      c.io.window.bits.latePressure.expect(2.U)
      c.io.window.bits.unusedPressure.expect(2.U)
      c.io.window.bits.depthAfter.expect(16.U)
      c.clock.step(); c.io.events.refill.poke(false.B); c.io.events.late.poke(0.U)
      c.io.depth.expect(16.U)
      for ((count, expected) <- Seq(0 -> 0, 20 -> 0, 21 -> 1, 39 -> 1, 40 -> 2, 41 -> 2)) {
        inject(c, late = count)
        c.io.events.refill.poke(true.B); c.clock.step(499)
        c.io.window.bits.latePressure.expect(expected.U)
        c.clock.step(); c.io.events.refill.poke(false.B)
      }
      for ((count, expected) <- Seq(0 -> 0, 40 -> 0, 41 -> 1, 79 -> 1, 80 -> 2, 81 -> 2)) {
        inject(c, unused = count)
        c.io.events.refill.poke(true.B); c.clock.step(499)
        c.io.window.bits.unusedPressure.expect(expected.U)
        c.clock.step(); c.io.events.refill.poke(false.B)
      }
    }
  }

  for (settle <- Seq(0, 1)) {
    it should s"walk all seven levels one step at a time with settle=$settle and saturate boundaries" in {
      simulate(new PDBDepthController(PDBDepthParameters(settleWindows = settle))) { c =>
        init(c)
        for (next <- Seq(24, 32, 48, 64)) {
          window(c, late = 40); c.io.depth.expect(next.U)
          for (_ <- 0 until settle) { window(c, late = 40); c.io.depth.expect(next.U) }
        }
        window(c, late = 40); c.io.depth.expect(64.U)
        for (next <- Seq(48, 32, 24, 16, 8, 4)) {
          window(c, unused = 80); c.io.depth.expect(next.U)
          for (_ <- 0 until settle) { window(c, unused = 80); c.io.depth.expect(next.U) }
        }
        window(c, unused = 80); c.io.depth.expect(4.U)
        window(c, late = 21); c.io.depth.expect(4.U)
        window(c, late = 21); c.io.depth.expect(8.U)
      }
    }
  }

  it should "clear credit on ties and reversals while excluding used reuse from depth pressure" in {
    simulate(new PDBDepthController(PDBDepthParameters(settleWindows = 0))) { c =>
      init(c)
      window(c, late = 21); c.io.depth.expect(16.U)
      window(c); c.io.depth.expect(16.U)
      window(c, late = 21); c.io.depth.expect(16.U)
      window(c, late = 40, unused = 80); c.io.depth.expect(16.U)
      window(c, late = 21); c.io.depth.expect(16.U)
      window(c, unused = 41); c.io.depth.expect(16.U)
      window(c, late = 21); c.io.depth.expect(16.U)
      window(c, late = 21); c.io.depth.expect(24.U)
      for (_ <- 0 until 3) { window(c, used = 100); c.io.depth.expect(24.U) }
      c.io.usedMoveShadow.expect(true.B)
      window(c, unused = 80); c.io.depth.expect(16.U)
    }
  }

  it should "observe fixed mode and implement used move hysteresis without physical moves" in {
    simulate(new PDBDepthController(PDBDepthParameters())) { c =>
      init(c, enabled = false)
      for (_ <- 0 until 2) { window(c, late = 40, used = 20); c.io.usedMoveShadow.expect(false.B) }
      window(c, used = 19); c.io.usedMoveShadow.expect(false.B)
      for (_ <- 0 until 3) window(c, unused = 80, used = 20)
      c.io.usedMoveShadow.expect(true.B); c.io.depth.expect(64.U)
      window(c, used = 5); c.io.usedMoveShadow.expect(true.B)
      window(c, used = 6); c.io.usedMoveShadow.expect(true.B)
      window(c, used = 5); c.io.usedMoveShadow.expect(true.B)
      window(c, used = 5); c.io.usedMoveShadow.expect(false.B)
      c.io.depth.expect(64.U)
    }
  }

  it should "require a full minimum-depth window and keep unused shadow from consuming pressure" in {
    simulate(new PDBDepthController(PDBDepthParameters(settleWindows = 0))) { c =>
      init(c)
      window(c, unused = 80); c.io.depth.expect(8.U)
      window(c, unused = 80); c.io.depth.expect(4.U)
      c.io.unusedMoveShadow.expect(false.B)
      window(c); c.io.unusedMoveShadow.expect(false.B)
      window(c, late = 40, unused = 80)
      c.io.unusedMoveShadow.expect(true.B); c.io.depth.expect(4.U)
      window(c, late = 40, unused = 80); c.io.depth.expect(4.U)
      window(c, late = 40); c.io.depth.expect(8.U)
      c.io.unusedMoveShadow.expect(true.B)
      for (_ <- 0 until 6) { window(c); c.io.unusedMoveShadow.expect(true.B) }
      window(c); c.io.unusedMoveShadow.expect(false.B)
    }
  }

  it should "saturate long no-refill intervals and reset all control state" in {
    simulate(new PDBDepthController(PDBDepthParameters())) { c =>
      init(c)
      c.io.events.late.poke(15.U); c.io.events.used.poke(3.U); c.io.events.unused.poke(3.U)
      c.clock.step(22000)
      c.io.depth.expect(16.U); c.io.refills.expect(0.U)
      c.io.events.late.poke(0.U); c.io.events.used.poke(0.U); c.io.events.unused.poke(0.U)
      c.io.events.refill.poke(true.B); c.clock.step(499)
      c.io.window.bits.late.expect(65535.U)
      c.io.window.bits.used.expect(65535.U)
      c.io.window.bits.unused.expect(65535.U)
      c.clock.step(); c.io.events.refill.poke(false.B)
      c.io.depth.expect(16.U)
      init(c); c.io.depth.expect(16.U)
      c.io.usedMoveShadow.expect(false.B); c.io.unusedMoveShadow.expect(false.B)
      window(c, late = 40); c.io.depth.expect(24.U)
    }
  }
}
