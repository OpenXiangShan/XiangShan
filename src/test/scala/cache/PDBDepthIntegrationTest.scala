package cache

import chisel3._
import chisel3.util._
import chisel3.simulator.scalatest.ChiselSim
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import top.DefaultConfig
import utility._
import xiangshan.{XSCoreParamsKey, XSTileKey}
import xiangshan.cache.{PDBVictim, PDBVictimObserver}
import xiangshan.mem.prefetch._

class PDBAlignedDepthTop extends Module {
  val io = IO(new Bundle {
    val refill = Input(Bool())
    val late = Input(UInt(4.W))
    val victim = Input(Valid(new PDBVictim(20)))
    val demand = Input(Vec(3, Valid(UInt(20.W))))
    val window = Output(Valid(new PDBDepthWindow))
    val depth = Output(UInt(12.W))
    val refills = Output(UInt(9.W))
  })
  val observer = Module(new PDBVictimObserver(256, 20))
  observer.io.victim := io.victim; observer.io.demand := io.demand
  val aligner = Module(new PDBDepthEventAligner)
  aligner.io.raw.refill := io.refill; aligner.io.raw.late := io.late
  aligner.io.raw.used := observer.io.result.streamUsedHits
  aligner.io.raw.unused := observer.io.result.streamUnusedHits
  val controller = Module(new PDBDepthController(PDBDepthParameters(
    lateLow = 0, lateHigh = 1, unusedLow = 0, unusedHigh = 1, settleWindows = 0)))
  controller.io.enabled := true.B; controller.io.fixedDepth := 64.U
  controller.io.events := aligner.io.completed
  io.window := controller.io.window; io.depth := controller.io.depth; io.refills := controller.io.refills
}

class PDBControlledStreamTop(implicit p: Parameters) extends StreamDepthTestTop {
  val events = IO(Input(new PDBDepthEvents))
  val selectedDepth = IO(Output(UInt(12.W)))
  val controller = Module(new PDBDepthController(p(PDBDepthKey)))
  controller.io.enabled := true.B; controller.io.fixedDepth := 64.U
  controller.io.events := events
  stream.io.dynamic_depth := controller.io.depth
  selectedDepth := controller.io.depth
}

class PDBDepthIntegrationTest extends AnyFlatSpec with ChiselSim {
  behavior of "PDB depth integration"
  it should "align all three demand lanes with the closing refill and preserve victims across depth changes" in {
    simulate(new PDBAlignedDepthTop) { c =>
      c.io.refill.poke(false.B); c.io.late.poke(0.U)
      c.io.victim.valid.poke(false.B); c.io.victim.bits.blockAddr.poke(0.U)
      c.io.victim.bits.used.poke(false.B); c.io.victim.bits.stream.poke(true.B)
      c.io.demand.foreach { d => d.valid.poke(false.B); d.bits.poke(0.U) }
      c.reset.poke(true.B); c.clock.step(3); c.reset.poke(false.B)
      for (addr <- 10 to 12) {
        c.io.victim.valid.poke(true.B); c.io.victim.bits.blockAddr.poke(addr.U); c.clock.step()
      }
      c.io.victim.valid.poke(false.B); c.clock.step(3)
      for ((addr, late, expected) <- Seq((10, 1, 16), (11, 0, 8), (12, 0, 4))) {
        c.io.refill.poke(true.B); c.clock.step(499)
        c.io.refill.poke(false.B); c.clock.step(3)
        c.io.refills.expect(499.U)
        c.io.refill.poke(true.B); c.io.late.poke(late.U)
        c.io.demand.foreach { d => d.valid.poke(true.B); d.bits.poke(addr.U) }
        c.io.window.valid.expect(false.B); c.clock.step()
        c.io.refill.poke(false.B); c.io.late.poke(0.U)
        c.io.demand.foreach(_.valid.poke(false.B))
        c.io.window.valid.expect(false.B); c.clock.step()
        c.io.window.valid.expect(true.B)
        c.io.window.bits.late.expect(late.U); c.io.window.bits.unused.expect(1.U)
        c.io.window.bits.depthAfter.expect(expected.U)
        c.clock.step(); c.io.depth.expect(expected.U)
        c.io.refills.expect(0.U); c.io.window.valid.expect(false.B)
      }
    }
  }

  it should "feed actual Stream prefetch addresses at every controller level" in {
    val base = new DefaultConfig
    implicit val p: Parameters = base.alterPartial {
      case XSCoreParamsKey => base(XSTileKey).head
      case PerfCounterOptionsKey => PerfCounterOptions(false, false, XSPerfLevel.VERBOSE, 0)
      case LogUtilsOptionsKey => LogUtilsOptions(false, false, false, false)
    }
    simulate(new PDBControlledStreamTop) { c =>
      c.io.valid.poke(false.B); c.io.address.poke(0.U); c.io.depth.poke(16.U)
      c.events.refill.poke(false.B); c.events.late.poke(0.U)
      c.events.used.poke(0.U); c.events.unused.poke(0.U)
      c.reset.poke(true.B); c.clock.step(3); c.reset.poke(false.B)
      def window(up: Boolean): Unit = {
        for (count <- (if (up) Seq(15, 15, 10) else Seq.fill(26)(3) :+ 2)) {
          if (up) c.events.late.poke(count.U) else c.events.unused.poke(count.U)
          c.clock.step()
        }
        c.events.late.poke(0.U); c.events.unused.poke(0.U)
        c.events.refill.poke(true.B); c.clock.step(500)
        c.events.refill.poke(false.B)
        // Observe the configured full settling window without additional pressure.
        c.events.refill.poke(true.B); c.clock.step(500); c.events.refill.poke(false.B)
      }
      window(up = false); window(up = false); c.selectedDepth.expect(4.U)
      var block = BigInt("80000", 16)
      for (depth <- PDBDepthPolicy.levels) {
        if (depth != 4) window(up = true)
        c.selectedDepth.expect(depth.U)
        var observed = 0
        def check(): Unit = {
          for ((req, offset) <- Seq((c.io.l1, depth), (c.io.l2, 640), (c.io.l3, 960))) {
            if (req.valid.peek().litToBoolean) {
              val trigger = req.bits.trigger_va.peek().litValue >> 6
              val bits = req.bits.bit_vec.peek().litValue
              val first = (req.bits.region.peek().litValue << 4) + bits.lowestSetBit
              assert(first == trigger + offset, s"depth=$depth offset=$offset: $first != ${trigger + offset}")
              if (offset == depth) observed += 1
            }
          }
        }
        for (_ <- 0 until 48) {
          c.io.address.poke((block << 6).U); c.io.valid.poke(true.B)
          var wait = 0
          while (!c.io.ready.peek().litToBoolean && wait < 10) { check(); c.clock.step(); wait += 1 }
          c.io.ready.expect(true.B); check(); c.clock.step(); block += 1
        }
        c.io.valid.poke(false.B)
        for (_ <- 0 until 5) { check(); c.clock.step() }
        assert(observed > 0)
      }
    }
  }
}
