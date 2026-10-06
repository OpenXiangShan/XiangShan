package cache

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink.ClientStates
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import top.DefaultConfig
import utility._
import xiangshan.{XSCoreParamsKey, XSTileKey}
import xiangshan.cache._

class PBVictimTestTop(implicit p: Parameters) extends PBTestTop(enableMoveToDCache = false) {
  val demand = IO(Input(Vec(LoadPipelineWidth, Valid(UInt((PAddrBits - blockOffBits).W)))))
  val results = IO(new Bundle {
    val victims, usedHits, unusedHits = Output(UInt(16.W))
    val lastUsed, lastStream = Output(Bool())
  })
  val observer = Module(new PDBVictimObserver(256, PAddrBits - blockOffBits, LoadPipelineWidth))
  observer.io.victim := io.dcache.perf.capacityVictim
  observer.io.demand := demand
  val victims = RegInit(0.U(16.W))
  val usedHits = RegInit(0.U(16.W))
  val unusedHits = RegInit(0.U(16.W))
  val lastUsed = RegInit(false.B)
  val lastStream = RegInit(false.B)
  victims := victims + observer.io.result.victim.valid
  usedHits := usedHits + observer.io.result.usedHits
  unusedHits := unusedHits + observer.io.result.unusedHits
  when (observer.io.result.victim.valid) {
    lastUsed := observer.io.result.victim.bits.used
    lastStream := observer.io.result.victim.bits.stream
  }
  results.victims := victims; results.usedHits := usedHits; results.unusedHits := unusedHits
  results.lastUsed := lastUsed; results.lastStream := lastStream
}

class PDBVictimIntegrationTest extends AnyFlatSpec with PBTestDriver {
  private val base = new DefaultConfig
  implicit private val p: Parameters = base.alterPartial {
    case XSCoreParamsKey => base(XSTileKey).head
    case PerfCounterOptionsKey => PerfCounterOptions(false, false, XSPerfLevel.VERBOSE, 0)
    case LogUtilsOptionsKey => LogUtilsOptions(false, false, false, false)
  }
  def initialize(c: PBVictimTestTop): Unit = {
    c.demand.foreach { q => q.valid.poke(false.B); q.bits.poke(0.U) }
    init(c)
  }
  def query(c: PBVictimTestTop, addr: BigInt): Unit = {
    c.demand.foreach { q => q.valid.poke(true.B); q.bits.poke((addr >> 6).U) }
    c.clock.step(); c.demand.foreach(_.valid.poke(false.B)); c.clock.step(4)
  }
  def fill(c: PBVictimTestTop): Unit = {
    for (i <- c.io.mshr.status.indices) {
      val id = reserve(c, address + i * 64); refill(c, id)
    }
  }

  behavior of "PB capacity victim observation"
  it should "publish once after WBQueue acceptance and include last S2 use before slot reuse" in {
    simulate(new PBVictimTestTop) { c =>
      initialize(c); fill(c)
      val victim = awaitRelease(c)
      c.clock.step(5); c.results.victims.expect(0.U)
      // S1 authorization and Release are simultaneous; consumption is one cycle later.
      loadS1(c, addr = victim)
      c.io.releaseReq.ready.poke(true.B); c.clock.step()
      c.io.releaseReq.ready.poke(false.B); c.io.mshr.refillWait.poke(false.B)
      use(c)
      c.io.dcache.perf.capacityVictim.valid.expect(true.B)
      c.io.dcache.perf.capacityVictim.bits.used.expect(true.B)
      c.io.dcache.perf.capacityVictim.bits.blockAddr.expect((victim >> 6).U)
      c.clock.step(); c.io.load(0).s2_use.poke(false.B)
      c.clock.step(5)
      c.results.victims.expect(1.U); c.results.lastUsed.expect(true.B)
      // The same PB slot can now belong to another block without changing the victim.
      val reused = reserve(c, address + 64 * c.io.mshr.status.size); refill(c, reused)
      query(c, victim)
      c.results.usedHits.expect(1.U); c.results.unusedHits.expect(0.U)
      query(c, victim); c.results.usedHits.expect(1.U)
      c.io.pipe.s0_moveReq.valid.expect(false.B)
    }
  }

  it should "observe unused releases while excluding corrupt victims and preserving source classification" in {
    simulate(new PBVictimTestTop) { c =>
      initialize(c)
      c.io.mshr.refillReq.bits.corrupt.poke(true.B)
      val poisoned = reserve(c); refill(c, poisoned)
      c.io.mshr.refillReq.bits.corrupt.poke(false.B)
      c.io.mshr.allocReq.bits.prefetchSource.poke(2.U) // Stride, not Stream.
      for (i <- 1 until c.io.mshr.status.size) {
        val id = reserve(c, address + i * 64); refill(c, id)
      }
      assert(awaitRelease(c) == address)
      c.io.releaseReq.ready.poke(true.B); c.clock.step()
      c.io.releaseReq.ready.poke(false.B); c.io.mshr.refillWait.poke(false.B)
      c.clock.step(5); c.results.victims.expect(0.U)
      val id = reserve(c, address + 64 * c.io.mshr.status.size); refill(c, id)
      val clean = awaitRelease(c)
      c.io.releaseReq.ready.poke(true.B); c.clock.step()
      c.io.releaseReq.ready.poke(false.B); c.io.mshr.refillWait.poke(false.B)
      c.clock.step(5)
      c.results.victims.expect(1.U); c.results.lastUsed.expect(false.B)
      c.results.lastStream.expect(false.B)
      query(c, clean); c.results.unusedHits.expect(1.U); c.results.usedHits.expect(0.U)
    }
  }

  it should "exclude Probe exit, Store promotion and reservation cancellation" in {
    simulate(new PBVictimTestTop) { c =>
      initialize(c)
      val probeId = reserve(c); refill(c, probeId)
      pipeS0(c, probe = true); pipeS1(c)
      c.io.pipe.s1_paddr.valid.poke(false.B)
      c.io.pipe.s3_probeDone.valid.poke(true.B); c.io.pipe.s3_probeDone.bits.poke(probeId.U)
      c.clock.step(); c.io.pipe.s3_probeDone.valid.poke(false.B)
      val storeAddress = address + 64
      val storeId = reserve(c, storeAddress); refill(c, storeId)
      pipeS0(c, storeAddress); pipeS1(c, storeAddress, store = true)
      c.io.pipe.s2_dataResp.ready.poke(true.B); c.clock.step()
      c.io.pipe.s2_dataResp.ready.poke(false.B)
      c.io.pipe.s3_moveDone.valid.poke(true.B); c.io.pipe.s3_moveDone.bits.poke(storeId.U)
      c.clock.step(); c.io.pipe.s3_moveDone.valid.poke(false.B)
      val canceled = reserve(c, address + 128)
      c.io.mshr.cancelReq(0).valid.poke(true.B); c.io.mshr.cancelReq(0).bits.poke(canceled.U)
      c.clock.step(); c.io.mshr.cancelReq(0).valid.poke(false.B)
      c.clock.step(5); c.results.victims.expect(0.U)
      query(c, address); query(c, storeAddress)
      c.results.usedHits.expect(0.U); c.results.unusedHits.expect(0.U)
    }
  }
}
