package cache

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import top.DefaultConfig
import utility._
import xiangshan.{XSCoreParamsKey, XSModule, XSTileKey}
import xiangshan.mem.prefetch._

class PDBMoveMonitorTop(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val events = Input(new PDBDepthEvents)
    val usedMove = Output(Bool())
    val unusedMove = Output(Bool())
    val depth = Output(UInt(12.W))
  })
  Constantin.init(false)
  ChiselDB.init(false)
  val monitor = Module(new PrefetcherMonitor)
  monitor.io.loadinfo := 0.U.asTypeOf(monitor.io.loadinfo)
  monitor.io.maininfo := 0.U.asTypeOf(monitor.io.maininfo)
  monitor.io.missinfo := 0.U.asTypeOf(monitor.io.missinfo)
  monitor.io.replinfo := 0.U.asTypeOf(monitor.io.replinfo)
  monitor.io.bufferinfo := 0.U.asTypeOf(monitor.io.bufferinfo)
  monitor.io.clear_flag := 0.U.asTypeOf(monitor.io.clear_flag)
  monitor.io.debugRolling := 0.U.asTypeOf(monitor.io.debugRolling)
  monitor.io.bufferinfo.stream_refill := io.events.refill
  monitor.io.bufferinfo.stream_used_victim_hits := io.events.used
  monitor.io.bufferinfo.stream_unused_victim_hits := io.events.unused
  io.usedMove := monitor.io.pdb_used_move
  io.unusedMove := monitor.io.pdb_unused_move
  io.depth := monitor.io.pf_ctrl(0).dynamic_depth
}

class PDBMoveMonitorTest extends AnyFlatSpec with ChiselSim {
  behavior of "Actual used move permission in PrefetcherMonitor"
  for (permission <- Seq(false, true)) {
    it should s"gate the unchanged three-on/two-off reuse hysteresis with permission=$permission" in {
      val base = new DefaultConfig
      implicit val p: Parameters = base.alterPartial {
        case XSCoreParamsKey => base(XSTileKey).head
        case PDBDepthKey => PDBDepthParameters(enabled = true, usedMoveEnabled = permission)
        case PerfCounterOptionsKey => PerfCounterOptions(false, false, XSPerfLevel.VERBOSE, 0)
        case LogUtilsOptionsKey => LogUtilsOptions(false, false, false, false)
      }
      simulate(new PDBMoveMonitorTop) { c =>
        c.io.events.refill.poke(false.B); c.io.events.used.poke(0.U)
        c.io.events.unused.poke(0.U); c.io.events.late.poke(0.U)
        c.reset.poke(true.B); c.clock.step(3); c.reset.poke(false.B); c.clock.step(3)
        def window(used: Int): Unit = {
          for (_ <- 0 until used) { c.io.events.used.poke(1.U); c.clock.step() }
          c.io.events.used.poke(0.U)
          c.io.events.refill.poke(true.B); c.clock.step(500)
          c.io.events.refill.poke(false.B); c.clock.step(2)
          c.io.depth.expect(16.U)
        }
        window(20); c.io.usedMove.expect(false.B)
        window(20); c.io.usedMove.expect(false.B)
        window(20); c.io.usedMove.expect(permission.B)
        window(5); c.io.usedMove.expect(permission.B)
        window(6); c.io.usedMove.expect(permission.B)
        window(5); c.io.usedMove.expect(permission.B)
        window(5); c.io.usedMove.expect(false.B)
      }
    }
  }

  for (permission <- Seq(false, true)) {
    it should s"enable unused move only after a full depth4 window and close after eight raw-zero windows with permission=$permission" in {
      val base = new DefaultConfig
      implicit val p: Parameters = base.alterPartial {
        case XSCoreParamsKey => base(XSTileKey).head
        case PDBDepthKey => PDBDepthParameters(enabled = true, unusedMoveEnabled = permission)
        case PerfCounterOptionsKey => PerfCounterOptions(false, false, XSPerfLevel.VERBOSE, 0)
        case LogUtilsOptionsKey => LogUtilsOptions(false, false, false, false)
      }
      simulate(new PDBMoveMonitorTop) { c =>
        c.io.events.refill.poke(false.B); c.io.events.used.poke(0.U)
        c.io.events.unused.poke(0.U); c.io.events.late.poke(0.U)
        c.reset.poke(true.B); c.clock.step(3); c.reset.poke(false.B); c.clock.step(3)
        def window(unused: Int): Unit = {
          for (_ <- 0 until unused) { c.io.events.unused.poke(1.U); c.clock.step() }
          c.io.events.unused.poke(0.U)
          c.io.events.refill.poke(true.B); c.clock.step(500)
          c.io.events.refill.poke(false.B); c.clock.step(2)
        }
        for (_ <- 0 until 3) { window(80); c.io.unusedMove.expect(false.B) }
        c.io.depth.expect(4.U)
        window(80); c.io.unusedMove.expect(permission.B)
        for (_ <- 0 until 7) { window(0); c.io.unusedMove.expect(permission.B) }
        window(0); c.io.unusedMove.expect(false.B)
      }
    }
  }
}
