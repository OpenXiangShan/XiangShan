package cache

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import top.DefaultConfig
import utility._
import xiangshan.{XSCoreParamsKey, XSTileKey, XSModule}
import xiangshan.mem.HasL1PrefetchSourceParameter
import xiangshan.mem.prefetch._

class PrefetchShutdownDefaultsTop(implicit p: Parameters) extends XSModule with HasL1PrefetchSourceParameter {
  Constantin.init(false); ChiselDB.init(false)
  val io = IO(new Bundle {
    val pressure = Input(Bool())
    val control = Output(Vec(3, new PrefetchControlBundle))
  })
  val sources = Seq(L1_HW_PREFETCH_STREAM, L1_HW_PREFETCH_STRIDE, L1_HW_PREFETCH_BERTI)
  for (((name, source), i) <- Seq("stream", "stride", "berti").zip(sources).zipWithIndex) {
    val monitor = Module(new L1PrefetchMonitor(PrefetcherMonitorParam.fromString(name)))
    monitor.io.prefetch_info := 0.U.asTypeOf(monitor.io.prefetch_info)
    monitor.io.prefetch_info.maininfo.total_prefetch := io.pressure
    monitor.io.prefetch_info.maininfo.pf_late_in_cache := io.pressure
    monitor.io.prefetch_info.maininfo.pf_source := source
    monitor.io.prefetch_info.replinfo.pf_useless := io.pressure
    monitor.io.prefetch_info.replinfo.pf_source_useless := source
    io.control(i) := monitor.io.pf_ctrl
  }
}

class PrefetchShutdownDefaultsTest extends AnyFlatSpec with ChiselSim {
  private val config = new DefaultConfig
  implicit private val p: Parameters = config.alterPartial {
    case XSCoreParamsKey => config(XSTileKey).head
    case PerfCounterOptionsKey => PerfCounterOptions(false, false, XSPerfLevel.VERBOSE, 0)
    case LogUtilsOptionsKey => LogUtilsOptions(false, false, false, false)
  }
  behavior of "PAB prefetcher shutdown defaults"
  it should "keep Stream, Stride and Berti enabled through useless and cache-late shutdown triggers" in {
    simulate(new PrefetchShutdownDefaultsTop) { c =>
      c.io.pressure.poke(false.B)
      c.reset.poke(true.B); c.clock.step(3); c.reset.poke(false.B)
      c.io.pressure.poke(true.B)
      for (_ <- 0 until 1100) {
        for ((control, depth) <- c.io.control.zip(Seq(16, 32, 32))) {
          control.enable.expect(true.B); control.flush.expect(false.B)
          control.confidence.expect(1.U); control.dynamic_depth.expect(depth.U)
        }
        c.clock.step()
      }
    }
  }
}
