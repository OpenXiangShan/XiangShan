package cache

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.flatspec.AnyFlatSpec
import xiangshan.mem.prefetch._

class PDBUnusedMoveDepthTop extends Module {
  val io = IO(new Bundle {
    val allowUnused = Input(Bool())
    val events = Input(new PDBDepthEvents)
    val state = Output(new PDBDepthState)
  })
  io.state := PDBDepthControl(PDBDepthParameters(settleWindows = 0), 16,
    true.B, 64.U, io.events, io.allowUnused)
}

class PDBUnusedMoveControlTest extends AnyFlatSpec with ChiselSim {
  private def init(c: PDBUnusedMoveDepthTop, permission: Boolean): Unit = {
    c.io.allowUnused.poke(permission.B); c.io.events.refill.poke(false.B)
    c.io.events.late.poke(0.U); c.io.events.used.poke(0.U); c.io.events.unused.poke(0.U)
    c.reset.poke(true.B); c.clock.step(3); c.reset.poke(false.B)
  }
  private def window(c: PDBUnusedMoveDepthTop, late: Int = 0, unused: Int = 0,
                     depthPressure: Int = -1, threshold: Int = -1): Unit = {
    var l = late; var u = unused
    while (l > 0 || u > 0) {
      val dl = l.min(15); val du = u.min(3)
      c.io.events.late.poke(dl.U); c.io.events.unused.poke(du.U); c.clock.step()
      l -= dl; u -= du
    }
    c.io.events.late.poke(0.U); c.io.events.unused.poke(0.U)
    c.io.events.refill.poke(true.B); c.clock.step(499)
    c.io.state.window.valid.expect(true.B)
    if (depthPressure >= 0) c.io.state.window.bits.depthUnusedPressure.expect(depthPressure.U)
    if (threshold >= 0) c.io.state.window.bits.upThreshold.expect(threshold.U)
    c.clock.step(); c.io.events.refill.poke(false.B)
  }
  private def activate(c: PDBUnusedMoveDepthTop): Unit = {
    window(c, unused = 80); c.io.state.depth.expect(8.U)
    window(c, unused = 80); c.io.state.depth.expect(4.U)
    c.io.state.unusedMoveShadow.expect(false.B)
    window(c, late = 40, unused = 80)
    c.io.state.depth.expect(4.U); c.io.state.unusedMoveShadow.expect(true.B)
  }

  behavior of "Unused move pressure ownership"
  it should "retain raw competition and base credit when only shadow is active" in {
    simulate(new PDBUnusedMoveDepthTop) { c =>
      init(c, permission = false); activate(c)
      window(c, late = 40, unused = 80, depthPressure = 2, threshold = 2)
      c.io.state.depth.expect(4.U)
      window(c, late = 40); c.io.state.depth.expect(8.U)
    }
  }
  it should "consume raw unused pressure only for physical move and apply 1.5 credit only at depth4" in {
    simulate(new PDBUnusedMoveDepthTop) { c =>
      init(c, permission = true); activate(c)
      window(c, late = 40, unused = 80, depthPressure = 0, threshold = 3)
      c.io.state.depth.expect(4.U)
      // Changing a permission between windows must not trip a stale-credit assertion.
      c.io.allowUnused.poke(false.B); c.clock.step()
      window(c, late = 40, unused = 80, depthPressure = 2, threshold = 2)
      c.io.state.depth.expect(4.U)
      c.io.allowUnused.poke(true.B)
      window(c, late = 40, unused = 80, depthPressure = 0, threshold = 3)
      c.io.state.depth.expect(4.U)
      window(c, late = 40, unused = 80, depthPressure = 0, threshold = 3)
      c.io.state.depth.expect(8.U)
      window(c, unused = 80, depthPressure = 0, threshold = 2)
      c.io.state.depth.expect(8.U)
      window(c, late = 40, unused = 80, threshold = 2); c.io.state.depth.expect(16.U)
      for (_ <- 0 until 7) { window(c); c.io.state.unusedMoveShadow.expect(true.B) }
      window(c); c.io.state.unusedMoveShadow.expect(false.B)
      window(c, unused = 80, depthPressure = 2); c.io.state.depth.expect(8.U)
    }
  }
}
