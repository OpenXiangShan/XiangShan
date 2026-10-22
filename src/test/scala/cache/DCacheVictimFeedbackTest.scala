package cache

import chisel3._
import chisel3.util._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.flatspec.AnyFlatSpec
import xiangshan.cache._
import xiangshan.mem.prefetch._

class DCacheVictimFeedbackTop extends Module {
  val io = IO(new Bundle {
    val victim = Input(Valid(new PABLine(20)))
    val refill = Input(Valid(UInt(20.W)))
    val demand = Input(Vec(3, Valid(UInt(20.W))))
    val fill = Input(Bool())
    val depth = Output(UInt(12.W))
    val window = Output(Valid(new PABDepthWindow))
  })
  val table = Module(new DCacheVictimTable(8, 20))
  table.io.victim := io.victim; table.io.refill := io.refill; table.io.demand := io.demand
  val controller = Module(new PABDepthController(PABDepthParameters(windowInserts = 3,
    lateLow = 0, lateHigh = 1, unusedLow = 0, unusedHigh = 1, settleWindows = 0)))
  controller.io.enabled := true.B; controller.io.fixedDepth := 64.U
  controller.io.events.insert := ShiftRegister(io.fill, 2, false.B, true.B)
  controller.io.events.late := 0.U; controller.io.events.unused := table.io.hits
  io.depth := controller.io.depth; io.window := controller.io.window
}

class DCacheVictimFeedbackTest extends AnyFlatSpec with ChiselSim {
  behavior of "Physical victim feedback alignment"
  it should "charge closing-edge reuse to its fill window and keep other records across windows" in {
    simulate(new DCacheVictimFeedbackTop) { c =>
      c.io.victim.valid.poke(false.B); c.io.victim.bits.blockAddr.poke(0.U)
      c.io.victim.bits.source.poke(3.U); c.io.victim.bits.used.poke(false.B)
      c.io.refill.valid.poke(false.B); c.io.refill.bits.poke(0.U); c.io.fill.poke(false.B)
      c.io.demand.foreach { d => d.valid.poke(false.B); d.bits.poke(0.U) }
      c.reset.poke(true.B); c.clock.step(3); c.reset.poke(false.B)
      for (a <- Seq(100,200)) {
        c.io.victim.valid.poke(true.B); c.io.victim.bits.blockAddr.poke(a.U); c.clock.step()
        c.io.victim.valid.poke(false.B); c.clock.step(3)
      }
      for ((a, expected) <- Seq((100,8),(200,4))) {
        c.io.fill.poke(true.B); c.clock.step(2)
        c.io.demand.foreach { d => d.valid.poke(true.B); d.bits.poke(a.U) }
        c.clock.step(); c.io.fill.poke(false.B); c.io.demand.foreach(_.valid.poke(false.B))
        c.clock.step(); c.io.window.valid.expect(true.B); c.io.window.bits.unused.expect(1.U)
        c.io.window.bits.depthAfter.expect(expected.U); c.clock.step()
        c.io.depth.expect(expected.U); c.clock.step(10)
      }
    }
  }
}
