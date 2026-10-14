package xiangshan.mem.prefetch

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import top.CHIConfig
import utility._
import xiangshan._
import xiangshan.mem.HasL1PrefetchSourceParameter

object PrefetcherMonitorHarness {
  val counterNames: Seq[String] = for {
    suffix <- Seq("", "Stream", "Stride", "Temporal")
    prefix <- Seq("l1prefetchSent", "l1prefetchHit", "l1prefetchHitInCache", "l1prefetchHitInMSHR",
      "l1prefetchLate", "l1prefetchLateInCache", "l1prefetchLateInMSHR", "l1prefetchUseless",
      "l1prefetchDropByNack", "mshr_count_Prefetch")
  } yield prefix + suffix
}

/** Exercise the production Monitor and its real XSPerf accumulation/clean/dump path. */
class PrefetcherMonitorHarness(implicit p: Parameters) extends XSModule with HasL1PrefetchSourceParameter {
  val io = IO(new Bundle {
    val info = Input(new L1PrefetchStatisticBundle)
    val clearFlag = Input(Vec(LoadPipelineWidth, Bool()))
    val clean = Input(Bool())
    val dump = Input(Bool())
    val snapshot = Input(UInt(64.W))
    val ctrl = Output(Vec(L1PrefetcherNum, new PrefetchControlBundle))
    val counters = Output(Vec(PrefetcherMonitorHarness.counterNames.size, UInt(64.W)))
  })
  private val monitor = Module(new PrefetcherMonitor)
  monitor.io.loadinfo := io.info.loadinfo
  monitor.io.missinfo := io.info.missinfo
  monitor.io.maininfo := io.info.maininfo
  monitor.io.clear_flag := io.clearFlag
  monitor.io.debugRolling.robTrueCommit := 0.U
  io.ctrl := monitor.io.pf_ctrl
  XSLog.collect(io.snapshot, true.B, io.clean, io.dump)

  // Observe the values the real XSPerf endpoint dumps, including the current cycle.
  // Chisel 7 prints to stderr, which svsim does not retain in simulation-log.txt.
  private val logged = XSLog.tapInfos.map(info => info.fmt.takeWhile(_ != ',') -> info.data).toMap
  require(!logged.contains("Temporal_disable_time"))
  io.counters.zip(PrefetcherMonitorHarness.counterNames).foreach { case (out, name) =>
    require(logged.contains(name) && logged(name).size == 1, s"Missing XSPerf counter $name")
    out := logged(name).head.asUInt
  }
}

class PrefetcherMonitorSpec extends AnyFlatSpec with Matchers with ChiselSim {
  override def buildDir: java.nio.file.Path = sys.env.get("L1_TP_LOG_DIR")
    .map(path => java.nio.file.Paths.get(path).resolve("chiselsim")).getOrElse(super.buildDir)

  private val base = new CHIConfig
  private val core = base(XSTileKey).head
  private val testParams = base.alterPartial {
    case XSCoreParamsKey => core
    case DebugOptionsKey => DebugOptions(EnablePerfDebug = true, AlwaysBasicDB = false)
    case DFTOptionsKey => DFTOptions(EnableMbist = false)
    case PerfCounterOptionsKey => PerfCounterOptions(true, false, XSPerfLevel.VERBOSE, 0)
    case LogUtilsOptionsKey => LogUtilsOptions(false, true, false, false)
  }

  private case class Counts(sent: Int = 0, cacheHits: Int = 0, mshrHits: Int = 0,
                            cacheLate: Int = 0, mshrLate: Int = 0, useless: Int = 0,
                            nack: Int = 0, misses: Int = 0) {
    def named(suffix: String): Map[String, BigInt] = Map(
      "l1prefetchSent" -> sent,
      "l1prefetchHit" -> (cacheHits + mshrHits),
      "l1prefetchHitInCache" -> cacheHits,
      "l1prefetchHitInMSHR" -> mshrHits,
      "l1prefetchLate" -> (cacheLate + mshrLate),
      "l1prefetchLateInCache" -> cacheLate,
      "l1prefetchLateInMSHR" -> mshrLate,
      "l1prefetchUseless" -> useless,
      "l1prefetchDropByNack" -> nack,
      "mshr_count_Prefetch" -> misses
    ).map { case (name, count) => (name + suffix) -> BigInt(count) }
  }

  behavior of "L1 prefetch Monitor"

  it should "recognize Temporal without adding a dynamic control channel" in {
    PrefetcherMonitorParam.fromString("TemPoRaL").name shouldBe "Temporal"
    (new HasL1PrefetchSourceParameter {}).L1PrefetcherNum shouldBe 2
  }

  it should "count Temporal cache and MSHR events without mixing prefetch sources" in {
    import chisel3.simulator.HasSimulator.simulators.verilator
    ChiselDB.init(false)
    Constantin.init(false)
    val zero = Seq("", "Stream", "Stride", "Temporal").flatMap(Counts().named).toMap
    var expected = zero
    var snapshots = 0

    simulate(new PrefetcherMonitorHarness()(testParams)) { dut =>
      val width = dut.io.info.loadinfo.size
      width shouldBe 3
      dut.io.ctrl.size shouldBe 2

      def idle(): Unit = {
        dut.io.info.loadinfo.foreach { t =>
          t.total_prefetch.poke(false.B)
          t.pf_late_in_cache.poke(false.B)
          t.nack_prefetch.poke(false.B)
          t.pf_source.poke(0.U)
          t.hit_pf_in_cache.poke(false.B)
          t.hit_source.poke(0.U)
          t.demand_miss.poke(false.B)
          t.pollution.poke(false.B)
        }
        val m = dut.io.info.missinfo
        m.pf_late_in_mshr.poke(false.B)
        m.prefetch_miss.poke(false.B)
        m.pf_source.poke(0.U)
        m.hit_pf_in_mshr.poke(false.B)
        m.hit_pf_source_in_mshr.poke(0.U)
        m.load_miss.poke(false.B)
        val main = dut.io.info.maininfo
        main.pf_useless.poke(false.B)
        main.pf_source_useless.poke(0.U)
        main.hit_pf_in_cache.poke(false.B)
        main.hit_pf_source_in_cache.poke(0.U)
        dut.io.clearFlag.foreach(_.poke(false.B))
        dut.io.clean.poke(false.B)
        dut.io.dump.poke(false.B)
      }

      // Dump on the event cycle: XSPerf must include that cycle exactly once.
      def sample(label: String, deltas: (String, Counts)*): Unit = {
        deltas.foreach { case (suffix, counts) =>
          counts.named(suffix).foreach { case (name, delta) => expected += name -> (expected(name) + delta) }
        }
        snapshots += 1
        dut.io.snapshot.poke(snapshots.U)
        dut.io.dump.poke(true.B)
        dut.io.counters.zip(PrefetcherMonitorHarness.counterNames).foreach { case (counter, name) =>
          withClue(s"snapshot $snapshots ($label), $name: ") {
            counter.expect(expected(name).U)
          }
        }
        dut.clock.step()
        dut.io.ctrl.foreach { ctrl =>
          ctrl.dynamic_depth.expect(32.U)
          ctrl.flush.expect(false.B)
          ctrl.enable.expect(true.B)
          ctrl.confidence.expect(1.U)
        }
        idle()
      }

      idle()
      dut.io.snapshot.poke(0.U)
      sample("reset state")

      // Sweep every 3-bit encoding, including demand/cleared/store/Berti/reserved sources.
      for (source <- 0 until 8) {
        val load = dut.io.info.loadinfo.head
        load.total_prefetch.poke(true.B)
        load.pf_late_in_cache.poke(true.B)
        load.nack_prefetch.poke(true.B)
        load.pf_source.poke(source.U)
        dut.io.info.loadinfo.foreach { t =>
          t.hit_pf_in_cache.poke(true.B)
          t.hit_source.poke(source.U)
        }
        val miss = dut.io.info.missinfo
        miss.pf_late_in_mshr.poke(true.B)
        miss.prefetch_miss.poke(true.B)
        miss.pf_source.poke(source.U)
        miss.hit_pf_in_mshr.poke(true.B)
        miss.hit_pf_source_in_mshr.poke(source.U)
        val main = dut.io.info.maininfo
        main.pf_useless.poke(true.B)
        main.pf_source_useless.poke(source.U)
        main.hit_pf_in_cache.poke(true.B)
        main.hit_pf_source_in_cache.poke(source.U)
        val counts = Counts(sent = 1, cacheHits = width + 1, mshrHits = 1,
          cacheLate = 1, mshrLate = 1, useless = 1, nack = 1, misses = 1)
        val suffix = Map(2 -> "Stride", 3 -> "Stream", 6 -> "Temporal").get(source)
        sample(s"source $source", (Seq("" -> counts) ++ suffix.toSeq.map(_ -> counts)): _*)
      }

      // Every load pipeline must feed the Temporal sent/nack/late/hit counters.
      for (i <- 0 until width) {
        val t = dut.io.info.loadinfo(i)
        t.total_prefetch.poke(true.B)
        t.pf_late_in_cache.poke(true.B)
        t.nack_prefetch.poke(true.B)
        t.pf_source.poke(6.U)
        t.hit_pf_in_cache.poke(true.B)
        t.hit_source.poke(6.U)
        val counts = Counts(sent = 1, cacheHits = 1, cacheLate = 1, nack = 1)
        sample(s"Temporal load pipeline $i", "" -> counts, "Temporal" -> counts)
      }

      // Requests and hits carry independent sources; mainpipe hit/useless do too.
      val load = dut.io.info.loadinfo.head
      load.total_prefetch.poke(true.B)
      load.nack_prefetch.poke(true.B)
      load.pf_late_in_cache.poke(true.B)
      load.pf_source.poke(3.U)
      dut.io.info.loadinfo.zip(Seq(6, 2, 3)).foreach { case (t, source) =>
        t.hit_pf_in_cache.poke(true.B)
        t.hit_source.poke(source.U)
      }
      val miss = dut.io.info.missinfo
      miss.pf_late_in_mshr.poke(true.B)
      miss.prefetch_miss.poke(true.B)
      miss.pf_source.poke(2.U)
      miss.hit_pf_in_mshr.poke(true.B)
      miss.hit_pf_source_in_mshr.poke(6.U)
      val main = dut.io.info.maininfo
      main.pf_useless.poke(true.B)
      main.pf_source_useless.poke(2.U)
      main.hit_pf_in_cache.poke(true.B)
      main.hit_pf_source_in_cache.poke(6.U)
      sample("independent request and hit sources",
        "" -> Counts(sent = 1, cacheHits = 4, mshrHits = 1, cacheLate = 1, mshrLate = 1, useless = 1, nack = 1, misses = 1),
        "Stream" -> Counts(sent = 1, cacheHits = 1, cacheLate = 1, nack = 1),
        "Stride" -> Counts(cacheHits = 1, mshrLate = 1, useless = 1, misses = 1),
        "Temporal" -> Counts(cacheHits = 2, mshrHits = 1))

      for (i <- 0 until width) {
        dut.io.info.loadinfo.foreach { t =>
          t.hit_pf_in_cache.poke(true.B)
          t.hit_source.poke(6.U)
        }
        dut.io.clearFlag(i).poke(true.B)
        val counts = Counts(cacheHits = width - 1)
        sample(s"clear duplicate hit on load pipeline $i", "" -> counts, "Temporal" -> counts)
      }

      dut.io.info.loadinfo.foreach { t =>
        t.hit_pf_in_cache.poke(true.B)
        t.hit_source.poke(6.U)
      }
      dut.io.clearFlag.foreach(_.poke(true.B))
      main.hit_pf_in_cache.poke(true.B)
      main.hit_pf_source_in_cache.poke(6.U)
      miss.hit_pf_in_mshr.poke(true.B)
      miss.hit_pf_source_in_mshr.poke(6.U)
      sample("load clear flags preserve mainpipe and MSHR hits",
        "" -> Counts(cacheHits = 1, mshrHits = 1), "Temporal" -> Counts(cacheHits = 1, mshrHits = 1))

      // A retained source without a valid event must not increment any counter.
      dut.io.info.loadinfo.foreach { t => t.pf_source.poke(6.U); t.hit_source.poke(6.U) }
      miss.pf_source.poke(6.U)
      miss.hit_pf_source_in_mshr.poke(6.U)
      main.pf_source_useless.poke(6.U)
      main.hit_pf_source_in_cache.poke(6.U)
      sample("Temporal sources with no events")
      dut.clock.step(7)
      sample("idle counters stay stable")

      dut.io.clean.poke(true.B)
      dut.clock.step()
      idle()
      expected = zero
      sample("XSPerf clean clears all accumulated counters")
      miss.prefetch_miss.poke(true.B)
      miss.pf_source.poke(6.U)
      sample("Temporal MSHR allocation after clean", "" -> Counts(misses = 1), "Temporal" -> Counts(misses = 1))

      dut.reset.poke(true.B)
      dut.clock.step(2)
      dut.reset.poke(false.B)
      expected = zero
      sample("hardware reset clears accumulated counters")
    }

    println(s"Monitor passed: $snapshots snapshots, ${zero.size} real XSPerf counters per snapshot")
  }
}
