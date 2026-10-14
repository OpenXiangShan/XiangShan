package xiangshan.mem.prefetch

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import chisel3.simulator.scalatest.ChiselSim
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import top.{CHIConfig, CHIMinimalConfig, CHINoTemporalConfig, TLConfig, WithL1TemporalPrefetcher}
import utility.{ChiselDB, Constantin, LogUtilsOptions, LogUtilsOptionsKey, PerfCounterOptions, PerfCounterOptionsKey, XSPerfLevel}
import utility.{ClockGate, LogPerfIO, XSLog}
import utility.mbist.{MbistInterface, MbistPipeline, MbitsStandardInterface}
import utility.sram.SramHelper
import xiangshan._
import xiangshan.mem.L1PrefetchReq

/** Drive the same S1 PC / S3 training interface used by MemBlock. */
class TemporalWrapperHarness(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val enable = Input(Bool())
    val load = Flipped(Valid(new TrainReqBundle))
    val firstIssue = Input(Bool())
    val tlbMiss = Input(Bool())
    val hwPrefetch = Input(Bool())
    val prefetch = Decoupled(new L1PrefetchReq)
    val tlbValid = Output(Bool())
    val l2Valid = Output(Bool())
    val l3Valid = Output(Bool())
    val temporalTrainValid = Output(Bool())
  })
  val wrapper = Module(new PrefetcherWrapper)
  wrapper.io.pfCtrlFromTile := 0.U.asTypeOf(wrapper.io.pfCtrlFromTile)
  wrapper.io.pfCtrlFromCSR := 0.U.asTypeOf(wrapper.io.pfCtrlFromCSR)
  wrapper.io.pfCtrlFromCSR.l1D_pf_enable := io.enable
  wrapper.io.pfCtrlFromDCache := 0.U.asTypeOf(wrapper.io.pfCtrlFromDCache)
  wrapper.io.fromDCache.sms_agt_evict_req.valid := false.B
  wrapper.io.fromDCache.sms_agt_evict_req.bits := 0.U.asTypeOf(wrapper.io.fromDCache.sms_agt_evict_req.bits)
  wrapper.io.fromDCache.refillTrain.valid := false.B
  wrapper.io.fromDCache.refillTrain.bits := 0.U.asTypeOf(wrapper.io.fromDCache.refillTrain.bits)
  wrapper.io.fromOOO.s1_loadPc.foreach(_ := io.load.bits.pc)
  wrapper.io.fromOOO.s1_storePc.foreach(_ := 0.U)
  wrapper.io.trainSource := 0.U.asTypeOf(wrapper.io.trainSource)
  wrapper.io.trainSource.s1_loadFireHint.foreach(_ := true.B)
  wrapper.io.trainSource.s2_loadFireHint.foreach(_ := true.B)

  val s3 = RegNext(RegNext(io.load.bits))
  val s3Valid = RegNext(RegNext(io.load.valid, false.B), false.B)
  val s3First = RegNext(RegNext(io.firstIssue))
  val s3TlbMiss = RegNext(RegNext(io.tlbMiss))
  val s3HwPf = RegNext(RegNext(io.hwPrefetch))
  val train = wrapper.io.trainSource.s3_load.head
  train.valid := s3Valid
  train.bits.vaddr := s3.vaddr
  train.bits.paddr := s3.paddr
  train.bits.miss := s3.miss
  train.bits.meta_prefetch := s3.metaSource
  train.bits.isFirstIssue := s3First
  train.bits.tlbMiss := s3TlbMiss
  train.bits.is_from_hw_pf := s3HwPf
  // Deliberately leave uop.pc zero: PrefetcherWrapper must recover it from S1.

  wrapper.io.tlb_req.foreach { tlb =>
    tlb.req.ready := true.B
    tlb.resp.valid := false.B
    tlb.resp.bits := 0.U.asTypeOf(tlb.resp.bits)
  }
  wrapper.io.pmp_resp.foreach(x => x := 0.U.asTypeOf(x))
  io.prefetch <> wrapper.io.l1_pf_to_l1
  io.tlbValid := wrapper.io.tlb_req.map(_.req.valid).reduce(_ || _)
  io.l2Valid := wrapper.io.l1_pf_to_l2.addr_valid
  io.l3Valid := wrapper.io.l1_pf_to_l3.addr_valid
  io.temporalTrainValid := BoringUtils.bore(wrapper.temporalOpt.get.io.ld_in.head.valid)
}

/** Collect real performance counters and terminate the default MBIST/DFT wiring. */
class TemporalWrapperIntegrationHarness(implicit p: Parameters) extends TemporalWrapperHarness
  with xiangshan.cache.HasDCacheParameters {
  val perf = IO(Input(new LogPerfIO))
  XSLog.collect(perf)
  val loggedNames = XSLog.tapInfos.map(_.fmt.takeWhile(_ != ',')).toSet
  val params = prefetcherSeq.collectFirst { case t: TemporalPrefetcherParams => t }.get
  for (i <- 0 until (1 << params.samplerTableMatchCntWidth)) {
    require(loggedNames.contains(s"l1_tp_sampler_table_matchCnt_${i}_${i + 1}"))
  }
  val maxLen = 512 / (PAddrBits - blockOffBits)
  for (i <- 0 to maxLen) {
    require(loggedNames.contains(s"l1_tp_recorder_index_${i}_${i + 1}"))
  }
  val mbistPipeline = MbistPipeline.PlaceMbistPipeline(Int.MaxValue, "MbistPipeTemporalTest", hasMbist).get
  val mbistInterface = Module(new MbistInterface(
    Seq(mbistPipeline.nodeParams), Seq(mbistPipeline.childrenIds), "MbistIntfTemporalTest", 1))
  mbistInterface.toPipeline.head <> mbistPipeline.mbist
  val mbist = IO(new MbitsStandardInterface(mbistPipeline.nodeParams))
  mbistInterface.mbist <> mbist
  val broadcast = SramHelper.genBroadCastBundleTop()
  broadcast := 0.U.asTypeOf(broadcast)
  ClockGate.genTeSrc.cgen := false.B
}

/** Generate the default CHI wrapper with performance collection and MBIST enabled. */
object L1TemporalDefaultElaboration {
  def main(args: Array[String]): Unit = {
    require(args.length == 1, "usage: L1TemporalDefaultElaboration <output-directory>")
    val base = new CHIConfig
    val core = base(XSTileKey).head
    val parameters = base.alterPartial {
      case XSCoreParamsKey => core
      case DebugOptionsKey => DebugOptions(EnablePerfDebug = true, AlwaysBasicDB = false)
      case PerfCounterOptionsKey => PerfCounterOptions(true, false, XSPerfLevel.VERBOSE, 0)
      case LogUtilsOptionsKey => LogUtilsOptions(false, true, false, false)
    }
    ChiselDB.init(false)
    Constantin.init(false)
    top.Generator.execute(
      Array("--target", "systemverilog", "--target-dir", args(0), "--split-verilog"),
      new TemporalWrapperIntegrationHarness()(parameters),
      Array("-O=release", "--disable-annotation-unknown", "--lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none")
    )
    println("Default CHI wrapper RTL generated with performance collection and MBIST; all TP histogram bins checked")
  }
}

class L1TemporalPrefetcherSpec extends AnyFlatSpec with Matchers with ChiselSim {
  override def buildDir: java.nio.file.Path = sys.env.get("L1_TP_LOG_DIR")
    .map(path => java.nio.file.Paths.get(path).resolve("chiselsim")).getOrElse(super.buildDir)
  private val small = TemporalPrefetcherParams(
    tpTableEntries = 128, tpTableAssoc = 4,
    samplerFilterEntries = 64, samplerFilterAssoc = 4,
    samplerTableEntries = 128, samplerTableAssoc = 4,
    recorderTableEntries = 64, recorderTableAssoc = 4,
    confTableEntries = 64, confTableAssoc = 4,
    trainQueueDepth = 8, tpDataQueueDepth = 8
  )
  private val base = new CHIConfig
  private val testCore = base(XSTileKey).head.copy(prefetcher = Seq(small))
  private val testParams = base.alterPartial {
    case XSCoreParamsKey => testCore
    case DebugOptionsKey => DebugOptions(EnablePerfDebug = false, AlwaysBasicDB = false)
    case DFTOptionsKey => DFTOptions(EnableMbist = false)
    case PerfCounterOptionsKey => PerfCounterOptions(false, false, XSPerfLevel.VERBOSE, 0)
    case LogUtilsOptionsKey => LogUtilsOptions(false, false, false, false)
  }

  behavior of "L1 temporal prefetcher"

  it should "register exactly once in CHI configurations and support a baseline" in {
    val chi = new CHIConfig(2)
    chi(XSTileKey).foreach { core =>
      core.prefetcher.count(_.isInstanceOf[TemporalPrefetcherParams]) shouldBe 1
      core.L2CacheParamsOpt.get.prefetch.exists(_.isInstanceOf[coupledL2.prefetch.TPParameters]) shouldBe false
    }
    (new CHIMinimalConfig)(XSTileKey).head.prefetcher.exists(_.isInstanceOf[TemporalPrefetcherParams]) shouldBe true
    (new CHINoTemporalConfig)(XSTileKey).head.prefetcher.exists(_.isInstanceOf[TemporalPrefetcherParams]) shouldBe false
    (new TLConfig)(XSTileKey).head.prefetcher.exists(_.isInstanceOf[TemporalPrefetcherParams]) shouldBe false
    val twice = new org.chipsalliance.cde.config.Config(WithL1TemporalPrefetcher() ++ new CHIConfig)
    twice(XSTileKey).head.prefetcher.count(_.isInstanceOf[TemporalPrefetcherParams]) shouldBe 1
  }

  it should "learn an irregular sequence through the wrapper and retain aliases under backpressure" in {
    import chisel3.simulator.HasSimulator.simulators.verilator
    ChiselDB.init(false)
    Constantin.init(false)
    simulate(new TemporalWrapperHarness()(testParams)) { dut =>
      // Thirteen non-stride lines, with virtual aliases deliberately different from physical ones.
      val order = Seq(0, 7, 3, 12, 5, 10, 1, 9, 4, 6, 2, 11, 8)
      val addresses = order.map(i => BigInt("80000000", 16) + i * 4096 + (i % 4) * 64)
      val aliases = addresses.zipWithIndex.map { case (addr, i) => addr -> ((i + 1) % 4) }.toMap
      val prefetched = scala.collection.mutable.Set.empty[BigInt]
      var sent = 0
      var blocked: Option[(BigInt, BigInt)] = None

      dut.io.enable.poke(true.B)
      dut.io.load.valid.poke(false.B)
      dut.io.load.bits.vaddr.poke(0.U)
      dut.io.load.bits.paddr.poke(0.U)
      dut.io.load.bits.pc.poke("h80001004".U)
      dut.io.load.bits.miss.poke(true.B)
      dut.io.load.bits.metaSource.poke(0.U)
      dut.io.load.bits.refillLatency.poke(0.U)
      dut.io.firstIssue.poke(true.B)
      dut.io.tlbMiss.poke(false.B)
      dut.io.hwPrefetch.poke(false.B)
      dut.io.prefetch.ready.poke(true.B)

      def tick(ready: Boolean = true, rejectedTraining: Boolean = false): Unit = {
        dut.io.prefetch.ready.poke(ready.B)
        if (rejectedTraining) dut.io.temporalTrainValid.expect(false.B)
        dut.io.tlbValid.expect(false.B)
        dut.io.l2Valid.expect(false.B)
        dut.io.l3Valid.expect(false.B)
        val valid = dut.io.prefetch.valid.peek().litToBoolean
        if (valid) {
          val addr = dut.io.prefetch.bits.paddr.peek().litValue
          val alias = dut.io.prefetch.bits.alias.peek().litValue
          aliases.contains(addr) shouldBe true
          alias shouldBe BigInt(aliases(addr))
          dut.io.prefetch.bits.pf_source.value.expect(6.U)
          dut.io.prefetch.bits.is_store.expect(false.B)
          dut.io.prefetch.bits.confidence.expect(0.U)
          blocked.foreach(_ shouldBe ((addr, alias)))
          if (ready) {
            sent += 1
            prefetched += addr
            blocked = None
          } else {
            blocked = Some((addr, alias))
          }
        } else {
          blocked shouldBe None
        }
        dut.clock.step()
      }
      def load(addr: BigInt, hit: Boolean): Unit = {
        val alias = aliases(addr)
        val va = (addr & ~BigInt(0x3000)) | (BigInt(alias) << 12)
        dut.io.load.valid.poke(true.B)
        dut.io.load.bits.paddr.poke(addr.U)
        dut.io.load.bits.vaddr.poke(va.U)
        dut.io.load.bits.miss.poke((!hit).B)
        dut.io.load.bits.metaSource.poke((if (hit) 6 else 0).U)
      }

      for (_ <- 0 until 200) tick()
      sent shouldBe 0
      // Rejected training must not build metadata even when it repeats a learnable pattern.
      for (rejected <- Seq("hardware prefetch", "TLB miss", "reissued load", "non-Pmem")) {
        dut.io.hwPrefetch.poke((rejected == "hardware prefetch").B)
        dut.io.tlbMiss.poke((rejected == "TLB miss").B)
        dut.io.firstIssue.poke((rejected != "reissued load").B)
        for (i <- 0 until 130) {
          load(addresses(i % addresses.size), hit = false)
          if (rejected == "non-Pmem") dut.io.load.bits.paddr.poke((i * 64).U)
          tick(rejectedTraining = true)
          dut.io.load.valid.poke(false.B)
          for (_ <- 0 until 15) tick(rejectedTraining = true)
        }
        dut.io.load.valid.poke(false.B)
        for (_ <- 0 until 200) tick(rejectedTraining = true)
        withClue(rejected + " must not train: ") { sent shouldBe 0 }
      }
      dut.io.hwPrefetch.poke(false.B)
      dut.io.tlbMiss.poke(false.B)
      dut.io.firstIssue.poke(true.B)

      for (i <- 0 until 390) {
        val addr = addresses(i % addresses.size)
        load(addr, prefetched.remove(addr))
        tick()
        dut.io.load.valid.poke(false.B)
        for (_ <- 0 until 19) tick()
      }
      sent should be > 0

      // Sustain demand training while refusing requests; bounded internal queues must stall safely.
      for (i <- 0 until 1600) {
        load(addresses(i % addresses.size), hit = false)
        tick(ready = false)
      }
      dut.io.load.valid.poke(false.B)
      for (_ <- 0 until 300) tick(ready = false)
      blocked.isDefined shouldBe true
      val beforeDrain = sent
      for (_ <- 0 until 3000) tick()
      sent should be > beforeDrain

      // Disable while a learned group is blocked, then drain accepted wrapper requests.
      // Space training so low-confidence sparse probes can reach the sender first.
      var waitForBlocked = 0
      while (blocked.isEmpty && waitForBlocked < 1300) {
        val addr = addresses(waitForBlocked % addresses.size)
        load(addr, prefetched.remove(addr))
        tick(ready = false)
        dut.io.load.valid.poke(false.B)
        for (_ <- 0 until 19) tick(ready = false)
        waitForBlocked += 1
      }
      dut.io.load.valid.poke(false.B)
      for (_ <- 0 until 300) tick(ready = false)
      blocked.isDefined shouldBe true
      dut.io.enable.poke(false.B)
      for (_ <- 0 until 20) tick(ready = false)
      for (_ <- 0 until 100) tick()
      val afterDisable = sent
      for (i <- 0 until 130) {
        load(addresses(i % addresses.size), hit = false)
        tick()
      }
      dut.io.load.valid.poke(false.B)
      for (_ <- 0 until 100) tick()
      sent shouldBe afterDisable
      dut.io.enable.poke(true.B)
      for (_ <- 0 until 20) tick()
      var waitForRestart = 0
      while (sent == afterDisable && waitForRestart < 1300) {
        val addr = addresses(waitForRestart % addresses.size)
        load(addr, prefetched.remove(addr))
        tick()
        dut.io.load.valid.poke(false.B)
        for (_ <- 0 until 19) tick()
        waitForRestart += 1
      }
      for (_ <- 0 until 500) tick()
      sent should be > afterDisable
      println(s"L1 temporal smoke passed: $sent learned requests; aliases and stalled requests checked")
    }
  }
}
