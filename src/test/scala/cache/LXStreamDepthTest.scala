package cache

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import top.DefaultConfig
import utility._
import xiangshan.{XSCoreParamsKey, XSTileKey, XSModule}
import xiangshan.mem.prefetch._

class LXStreamDepthTop(implicit p: Parameters) extends XSModule {
  Constantin.init(false); ChiselDB.init(false)
  val io = IO(new Bundle {
    val late, useless = Input(Bool())
    val control = Output(new PrefetchControlBundle)
    val otherDepths = Output(Vec(2, UInt(12.W)))
  })
  val stream = Module(new L1PrefetchMonitor(PrefetcherMonitorParam.fromString("stream")))
  stream.io.prefetch_info := 0.U.asTypeOf(stream.io.prefetch_info)
  stream.io.prefetch_info.maininfo.total_prefetch := io.late
  stream.io.prefetch_info.maininfo.pf_source := 3.U
  stream.io.prefetch_info.missinfo.pf_late_in_mshr := io.late
  stream.io.prefetch_info.missinfo.pf_source := 3.U
  stream.io.prefetch_info.replinfo.pf_useless := io.useless
  stream.io.prefetch_info.replinfo.pf_source_useless := 3.U
  io.control := stream.io.pf_ctrl
  for ((name, i) <- Seq("stride", "berti").zipWithIndex) {
    val monitor = Module(new L1PrefetchMonitor(PrefetcherMonitorParam.fromString(name)))
    monitor.io.prefetch_info := 0.U.asTypeOf(monitor.io.prefetch_info)
    io.otherDepths(i) := monitor.io.pf_ctrl.dynamic_depth
  }
}

class LXStreamDepthTest extends AnyFlatSpec with ChiselSim {
  private val config = new DefaultConfig
  implicit private val p: Parameters = config.alterPartial {
    case XSCoreParamsKey => config(XSTileKey).head
    case PerfCounterOptionsKey => PerfCounterOptions(false, false, XSPerfLevel.VERBOSE, 0)
    case LogUtilsOptionsKey => LogUtilsOptions(false, false, false, false)
  }
  behavior of "lx Stream depth control"
  it should "start at 16, saturate at 4 and 128, and preserve upward priority and other monitors" in {
    simulate(new LXStreamDepthTop) { c =>
      c.io.late.poke(false.B); c.io.useless.poke(false.B)
      c.reset.poke(true.B); c.clock.step(3); c.reset.poke(false.B)
      c.io.control.dynamic_depth.expect(16.U)
      def check(expected: Int): Unit = {
        c.io.control.dynamic_depth.expect(expected.U)
        c.io.control.enable.expect(true.B); c.io.control.flush.expect(false.B)
        c.io.control.confidence.expect(1.U)
        c.io.otherDepths.foreach(_.expect(32.U))
      }
      def window(late: Boolean, useless: Boolean, expected: Int): Unit = {
        c.io.late.poke(late.B); c.io.useless.poke(useless.B)
        c.clock.step(1001)
        c.io.late.poke(false.B); c.io.useless.poke(false.B); c.clock.step()
        check(expected)
      }
      check(16)
      for (next <- Seq(32, 64, 128, 128, 128)) window(true, false, next)
      for (next <- Seq(64, 32, 16, 8, 4, 4, 4)) window(false, true, next)
      window(true, true, 8)
      c.reset.poke(true.B); c.clock.step(3); c.reset.poke(false.B); check(16)
    }
  }
}
