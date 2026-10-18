package cache

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.flatspec.AnyFlatSpec
import xiangshan.mem.prefetch._

class PABDepthControllerTest extends AnyFlatSpec with ChiselSim {
  private def init(c: PABDepthController, enabled: Boolean = true): Unit = {
    c.io.enabled.poke(enabled.B); c.io.fixedDepth.poke(64.U)
    c.io.events.insert.poke(false.B); c.io.events.late.poke(0.U); c.io.events.unused.poke(0.U)
    c.reset.poke(true.B); c.clock.step(3); c.reset.poke(false.B)
  }
  private def inject(c: PABDepthController, late: Int, unused: Int): Unit = {
    c.io.events.insert.poke(false.B)
    var l = late; var u = unused
    while (l > 0 || u > 0) {
      val dl = l.min(15); val du = u.min(3)
      c.io.events.late.poke(dl.U); c.io.events.unused.poke(du.U)
      c.clock.step(); l -= dl; u -= du
    }
    c.io.events.late.poke(0.U); c.io.events.unused.poke(0.U)
  }
  private def window(c: PABDepthController, late: Int = 0, unused: Int = 0): Unit = {
    inject(c, late, unused)
    c.io.events.insert.poke(true.B); c.clock.step(499)
    c.io.inserts.expect(499.U); c.io.window.valid.expect(true.B)
    c.io.window.bits.late.expect(late.U); c.io.window.bits.unused.expect(unused.U)
    c.clock.step(); c.io.events.insert.poke(false.B); c.io.inserts.expect(0.U)
  }
  behavior of "PDB-principle competitive depth from PAB observations"
  it should "quantize exact boundaries and include closing-edge events without a refill timeout" in {
    simulate(new PABDepthController(PABDepthParameters(settleWindows = 0))) { c =>
      init(c)
      for ((l, u, lp, up) <- Seq((20, 40, 0, 0), (21, 41, 1, 1), (39, 79, 1, 1), (40, 80, 2, 2))) {
        inject(c, l, u)
        c.io.events.insert.poke(true.B); c.clock.step(499)
        c.io.events.insert.poke(false.B); c.clock.step(100)
        c.io.window.valid.expect(false.B); c.io.depth.expect(16.U)
        c.io.events.insert.poke(true.B)
        c.io.window.bits.latePressure.expect(lp.U); c.io.window.bits.unusedPressure.expect(up.U)
        c.clock.step(); c.io.events.insert.poke(false.B)
      }
      inject(c, 39, 80); c.io.events.insert.poke(true.B); c.clock.step(499)
      c.io.events.late.poke(1.U); c.io.window.bits.late.expect(40.U)
      c.io.window.bits.depthAfter.expect(16.U)
      c.clock.step(); c.io.events.insert.poke(false.B)
    }
  }
  for (settle <- Seq(0, 1)) {
    it should s"walk every level, settle for $settle windows and saturate both limits" in {
      simulate(new PABDepthController(PABDepthParameters(settleWindows = settle))) { c =>
        init(c)
        for (next <- Seq(24, 32, 48, 64)) {
          window(c, late = 40); c.io.depth.expect(next.U)
          for (_ <- 0 until settle) { window(c, unused = 80); c.io.depth.expect(next.U) }
        }
        window(c, late = 40); c.io.depth.expect(64.U)
        for (next <- Seq(48, 32, 24, 16, 8, 4)) {
          window(c, unused = 80); c.io.depth.expect(next.U)
          for (_ <- 0 until settle) { window(c, late = 40); c.io.depth.expect(next.U) }
        }
        window(c, unused = 80); c.io.depth.expect(4.U)
        window(c, late = 21); c.io.depth.expect(4.U)
        window(c, late = 21); c.io.depth.expect(8.U)
      }
    }
  }
  it should "clear credits on ties and reversals and observe fixed mode" in {
    simulate(new PABDepthController(PABDepthParameters(settleWindows = 0))) { c =>
      init(c)
      window(c, late = 21); window(c); window(c, late = 21)
      c.io.depth.expect(16.U)
      window(c, unused = 41); window(c, late = 21); c.io.depth.expect(16.U)
      window(c, late = 21); c.io.depth.expect(24.U)
      init(c, enabled = false)
      window(c, late = 40); c.io.depth.expect(64.U)
      window(c, unused = 80); c.io.depth.expect(64.U)
    }
  }
  it should "saturate counts during long gaps and reset partial windows" in {
    simulate(new PABDepthController(PABDepthParameters())) { c =>
      init(c); c.io.events.late.poke(15.U); c.io.events.unused.poke(3.U); c.clock.step(22000)
      c.io.depth.expect(16.U); c.io.inserts.expect(0.U)
      c.io.events.late.poke(0.U); c.io.events.unused.poke(0.U)
      c.io.events.insert.poke(true.B); c.clock.step(499)
      c.io.window.bits.late.expect(65535.U); c.io.window.bits.unused.expect(65535.U)
      init(c); c.io.depth.expect(16.U); c.io.inserts.expect(0.U)
    }
  }
}
