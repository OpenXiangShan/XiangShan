package xiangshan.mem.prefetch

import chisel3._
import org.chipsalliance.cde.config.Parameters
import utility.{Constantin, PerfCounterOptions, PerfCounterOptionsKey, XSPerfLevel}
import xiangshan.XSTester

// The validity window of L1PrefetchMonitor counts hit_pf_in_cache, which can rise by up to
// LoadPipelineWidth + 1 per cycle. The window must still close every VALIDITY_CHECK_INTERVAL
// events; otherwise pf_useless_cnt keeps growing and the prefetcher is disabled spuriously.
class PrefetcherMonitorTest extends XSTester {
  Constantin.init(false)
  private val monitorConfig: Parameters = config.alterPartial({
    case PerfCounterOptionsKey => PerfCounterOptions(false, false, XSPerfLevel.VERBOSE, 0)
  })

  private val STREAM = 3 // L1_HW_PREFETCH_STREAM

  private def drive(dut: L1PrefetchMonitor, hitsPerCycle: Int, useless: Boolean): Unit = {
    val info = dut.io.prefetch_info
    info.loadinfo.zipWithIndex.foreach { case (l, i) =>
      l.total_prefetch.poke(false.B)
      l.pf_late_in_cache.poke(false.B)
      l.pf_late_in_cache_source.poke(0.U)
      l.nack_prefetch.poke(false.B)
      l.pf_source.poke(0.U)
      l.hit_pf_in_cache.poke((i < hitsPerCycle).B)
      l.hit_source.poke(STREAM.U)
      l.demand_miss.poke(false.B)
      l.pollution.poke(false.B)
    }
    val m = info.maininfo
    m.total_prefetch.poke(false.B)
    m.pf_late_in_cache.poke(false.B)
    m.pf_late_in_cache_source.poke(0.U)
    m.nack_prefetch.poke(false.B)
    m.pf_source.poke(0.U)
    m.hit_pf_in_cache.poke((hitsPerCycle > info.loadinfo.length).B)
    m.hit_source.poke(0.U)
    m.demand_miss.poke(false.B)
    m.pollution.poke(false.B)
    val mi = info.missinfo
    mi.pf_late_in_mshr.poke(false.B)
    mi.pf_late_in_mshr_source.poke(0.U)
    mi.prefetch_miss.poke(false.B)
    mi.pf_source.poke(0.U)
    mi.hit_pf_in_mshr.poke(0.U)
    mi.hit_pf_in_mshr_source.foreach(_.poke(0.U))
    mi.load_miss.poke(0.U)
    val r = info.replinfo
    r.pf_useless.poke(useless.B)
    r.pf_source_useless.poke(STREAM.U)
    r.hit_pf_in_cache.poke(false.B)
    r.hit_pf_source_in_cache.poke(STREAM.U)
  }

  it should "not disable the stream prefetcher when 4 hits arrive per cycle and few are useless" in {
    simulate(new L1PrefetchMonitor(new StreamMonitorParam)(monitorConfig)) { dut =>
      dut.reset.poke(true.B)
      dut.clock.step(2)
      dut.reset.poke(false.B)
      // 4 hits per cycle plus one useless every other cycle: 9 events per 2 cycles (~11% useless),
      // so the window sum steps 0, 5, 9, 14, ... and skips 1000. The sum wraps at the counter width
      // (log2Up(interval) + 1 bits), so an exact compare misses the window end for a long time.
      for (c <- 0 until 20000) {
        drive(dut, hitsPerCycle = dut.io.prefetch_info.loadinfo.length + 1, useless = c % 2 == 0)
        dut.clock.step()
        assert(dut.io.pf_ctrl.enable.peek().litToBoolean, s"stream prefetcher disabled at cycle $c")
      }
    }
  }

  it should "still disable the stream prefetcher when nearly all prefetches are useless" in {
    simulate(new L1PrefetchMonitor(new StreamMonitorParam)(monitorConfig)) { dut =>
      dut.reset.poke(true.B)
      dut.clock.step(2)
      dut.reset.poke(false.B)
      var disabled = false
      for (_ <- 0 until 2000 if !disabled) {
        drive(dut, hitsPerCycle = 0, useless = true)
        dut.clock.step()
        disabled = dut.io.pf_ctrl.enable.peek().litToBoolean == false
      }
      assert(disabled, "stream prefetcher was not disabled by a fully useless window")
    }
  }
}
