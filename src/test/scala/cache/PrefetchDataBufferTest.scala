package cache

import chisel3._
import chisel3.util._
import chisel3.simulator.scalatest.ChiselSim
import freechips.rocketchip.tilelink.ClientStates
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import top.DefaultConfig
import utility.{LogUtilsOptions, LogUtilsOptionsKey, PerfCounterOptions, PerfCounterOptionsKey, XSPerfLevel}
import xiangshan.{XSCoreParamsKey, XSTileKey}
import xiangshan.cache._

// Test-only observation adds no production ports or control feedback.
class PBTestTop(enableMoveToDCache: Boolean = true)(implicit p: Parameters)
  extends PrefetchDataBuffer(Some(enableMoveToDCache)) {
  // Use MainPipe's alias extraction rather than assuming an indexing mode in the driver.
  val aliasQuery = IO(new Bundle {
    val vaddr = Input(UInt(VAddrBits.W))
    val alias = Output(UInt(PBAliasBits.W))
  })
  aliasQuery.alias := get_alias(aliasQuery.vaddr)
  val observed = IO(new Bundle {
    val resident = Output(Vec(PBEntries, Bool()))
    val moveLocked = Output(Vec(PBEntries, Bool()))
    val released = Output(Vec(PBEntries, Bool()))
    val used = Output(Vec(PBEntries, Bool()))
    val loadEntryId = Output(Vec(LoadPipelineWidth, UInt(PBIdBits.W)))
    val firstUses, unusedExits = Output(UInt(16.W))
  })
  val firstCount = RegInit(0.U(16.W))
  val unusedExitCount = RegInit(0.U(16.W))
  firstCount := firstCount + PopCount(io.dcache.perf.firstUse.map(_.valid))
  unusedExitCount := unusedExitCount + io.dcache.perf.unusedExit.valid
  observed.resident := VecInit(entryMeta.map(_.state === PBState.resident))
  observed.moveLocked := VecInit(entryMeta.map(_.state === PBState.moveLocked))
  observed.released := VecInit(entryMeta.map(_.state === PBState.released))
  observed.used := VecInit(entryMeta.map(_.used))
  observed.loadEntryId := loadS2EntryId
  observed.firstUses := firstCount
  observed.unusedExits := unusedExitCount
}

trait PBTestDriver extends ChiselSim { this: AnyFlatSpec =>
  protected val address: BigInt = BigInt("80001000", 16)
  protected def line(seed: Int = 0): BigInt =
    (0 until 64).foldLeft(BigInt(0))((v, b) => v | (BigInt((seed + b) & 255) << (8 * b)))

  protected def init(c: PBTestTop): Unit = {
    c.io.load.foreach { l =>
      l.s1_paddr.valid.poke(false.B); l.s1_paddr.bits.poke(0.U)
      l.s1_kill.poke(false.B); l.s2_use.poke(false.B)
    }
    c.io.mshr.allocReq.valid.poke(false.B)
    c.io.mshr.allocReq.bits.addr.poke(address.U)
    c.io.mshr.allocReq.bits.vaddr.poke(address.U)
    c.io.mshr.allocReq.bits.missEntryId.poke(0.U)
    c.io.mshr.allocReq.bits.prefetchSource.poke(3.U)
    c.io.mshr.cancelReq.foreach { r => r.valid.poke(false.B); r.bits.poke(0.U) }
    c.io.mshr.refillReq.valid.poke(false.B)
    c.io.mshr.refillReq.bits.entryId.poke(0.U)
    c.io.mshr.refillReq.bits.missEntryId.poke(0.U)
    c.io.mshr.refillReq.bits.data.poke(line().U)
    c.io.mshr.refillReq.bits.coh.state.poke(ClientStates.Trunk)
    c.io.mshr.refillReq.bits.denied.poke(false.B)
    c.io.mshr.refillReq.bits.corrupt.poke(false.B)
    c.io.mshr.refillWait.poke(false.B)
    c.io.mshr.preAcquire.foreach(_.poke(true.B))
    c.io.pipe.s0_probeReq.valid.poke(false.B); c.io.pipe.s0_probeReq.bits.poke(0.U)
    c.io.pipe.s0_moveReq.ready.poke(false.B)
    c.io.pipe.s1_paddr.valid.poke(false.B); c.io.pipe.s1_paddr.bits.poke(0.U)
    setPipeAlias(c, address)
    c.io.pipe.s1_storeReq.poke(false.B)
    c.io.pipe.s2_dataResp.ready.poke(false.B)
    c.io.pipe.s2_moveAbort.valid.poke(false.B)
    c.io.pipe.s2_moveAbort.bits.entryId.poke(0.U)
    c.io.pipe.s2_moveAbort.bits.dataBad.poke(false.B)
    c.io.pipe.s3_probeDone.valid.poke(false.B); c.io.pipe.s3_probeDone.bits.poke(0.U)
    c.io.pipe.s3_moveDone.valid.poke(false.B); c.io.pipe.s3_moveDone.bits.poke(0.U)
    c.io.releaseReq.ready.poke(false.B)
    c.io.dcache.wfi.wfiReq.poke(false.B)
    c.reset.poke(true.B); c.clock.step(2); c.reset.poke(false.B); c.clock.step()
  }

  protected def reserve(c: PBTestTop, addr: BigInt = address, owner: Int = 0): Int = {
    c.io.mshr.allocReq.bits.addr.poke(addr.U)
    c.io.mshr.allocReq.bits.vaddr.poke(addr.U)
    c.io.mshr.allocReq.bits.missEntryId.poke(owner.U)
    c.io.mshr.allocReq.valid.poke(true.B)
    c.io.mshr.allocReq.ready.expect(true.B)
    val id = c.io.mshr.allocEntryId.peek().litValue.toInt
    c.clock.step()
    c.io.mshr.allocReq.valid.poke(false.B)
    id
  }
  protected def refill(c: PBTestTop, id: Int, owner: Int = 0, seed: Int = 0): Unit = {
    c.io.mshr.refillReq.bits.entryId.poke(id.U)
    c.io.mshr.refillReq.bits.missEntryId.poke(owner.U)
    c.io.mshr.refillReq.bits.data.poke(line(seed).U)
    c.io.mshr.refillReq.valid.poke(true.B)
    c.io.mshr.refillReq.ready.expect(true.B)
    c.clock.step()
    c.io.mshr.refillReq.valid.poke(false.B)
  }
  protected def pipeS0(c: PBTestTop, addr: BigInt = address, probe: Boolean = false): Unit = {
    c.io.pipe.s0_probeReq.valid.poke(probe.B); c.io.pipe.s0_probeReq.bits.poke(addr.U)
    c.clock.step()
    c.io.pipe.s0_probeReq.valid.poke(false.B)
  }
  protected def setPipeAlias(c: PBTestTop, vaddr: BigInt): Unit = {
    c.aliasQuery.vaddr.poke(vaddr.U)
    c.io.pipe.s1_alias.poke(c.aliasQuery.alias.peek())
  }
  protected def pipeS1(c: PBTestTop, addr: BigInt = address, store: Boolean = false,
                       vaddr: Option[BigInt] = None): Unit = {
    c.io.pipe.s1_paddr.valid.poke(true.B); c.io.pipe.s1_paddr.bits.poke(addr.U)
    setPipeAlias(c, vaddr.getOrElse(addr))
    c.io.pipe.s1_storeReq.poke(store.B)
    c.io.pipe.s1_paddr.ready.expect(true.B)
    c.clock.step()
    c.io.pipe.s1_paddr.valid.poke(false.B); c.io.pipe.s1_storeReq.poke(false.B)
  }
  // Retain the cycle marker for existing scenarios; Load S0 sends no PB signal.
  protected def loadS0(c: PBTestTop, lane: Int = 0, addr: BigInt = address): Unit = ()
  protected def loadS1(c: PBTestTop, lane: Int = 0, addr: BigInt = address): Unit = {
    c.io.load(lane).s1_paddr.valid.poke(true.B)
    c.io.load(lane).s1_paddr.bits.poke(addr.U)
  }
  protected def use(c: PBTestTop, lane: Int = 0): Unit = {
    val l = c.io.load(lane)
    l.s2_dataResp.valid.expect(true.B); l.s2_dataResp.bits.hit.expect(true.B)
    l.s2_use.poke(true.B)
    l.s1_paddr.valid.poke(false.B)
  }
  protected def awaitRelease(c: PBTestTop): BigInt = {
    c.io.mshr.refillWait.poke(true.B)
    var wait = 0
    while (!c.io.releaseReq.valid.peek().litToBoolean && wait < 20) { c.clock.step(); wait += 1 }
    c.io.releaseReq.valid.expect(true.B)
    c.io.releaseReq.bits.addr.peek().litValue
  }
}

class PrefetchDataBufferTest extends AnyFlatSpec with ChiselSim with PBTestDriver {
  private val base = new DefaultConfig
  implicit private val p: Parameters = base.alterPartial {
    case XSCoreParamsKey => base(XSTileKey).head
    case PerfCounterOptionsKey => PerfCounterOptions(false, false, XSPerfLevel.VERBOSE, 0)
    case LogUtilsOptionsKey => LogUtilsOptions(false, false, false, false)
  }
  behavior of "PrefetchDataBuffer staged ownership"

  it should "elaborate the configured capacity with a matching entry ID" in {
    simulate(new PBTestTop(enableMoveToDCache = false)) { c =>
      assert(c.io.mshr.status.size == p(XSCoreParamsKey).dcacheParametersOpt.get.nPBEntries)
      assert(c.io.mshr.allocEntryId.getWidth == log2Ceil(c.io.mshr.status.size max 2))
      init(c)
      c.io.mshr.allocReq.ready.expect(true.B)
      c.io.mshr.allocEntryId.expect(0.U)
      c.io.mshr.status.head.valid.expect(false.B)
      c.io.mshr.status.last.valid.expect(false.B)
    }
  }

  it should "keep background move disabled and release only for a completed refill waiting on full capacity" in {
    simulate(new PBTestTop(enableMoveToDCache = false)) { c =>
      init(c)
      for (i <- c.io.mshr.status.indices) {
        val id = reserve(c, address + i * 64)
        refill(c, id, seed = i)
      }
      loadS0(c, addr = address)
      c.clock.step()
      loadS1(c, addr = address)
      c.io.load(0).s1_hit.expect(true.B)
      c.clock.step()
      use(c)
      c.clock.step()
      c.io.load(0).s2_use.poke(false.B)
      c.clock.step(4)
      c.io.pipe.s0_moveReq.valid.expect(false.B)
      c.io.releaseReq.valid.expect(false.B)
      c.io.mshr.refillWait.poke(true.B)
      c.clock.step()
      c.io.releaseReq.valid.expect(true.B)
    }
  }

  it should "query the current entry after refill and read corrected windows on a fresh request" in {
    simulate(new PBTestTop) { c =>
      init(c)
      val id = reserve(c)
      loadS0(c)
      refill(c, id)
      loadS1(c)
      c.io.load(0).s1_hit.expect(true.B); c.io.load(0).s1_retry.expect(false.B)
      c.clock.step()
      c.io.load(0).s1_paddr.valid.poke(false.B)
      c.io.load(0).s2_dataResp.bits.hit.expect(true.B)
      for (i <- 0 until 3) loadS0(c, i, address + i * 16)
      c.clock.step()
      for (i <- 0 until 3) loadS1(c, i, address + 32)
      c.clock.step()
      for (i <- 0 until 3) {
        c.io.load(i).s1_paddr.valid.poke(false.B)
        c.io.load(i).s2_dataResp.bits.hit.expect(true.B)
        c.io.load(i).s2_dataResp.bits.data.expect(((line() >> 256) & ((BigInt(1) << 128) - 1)).U)
        use(c, i)
      }
      c.clock.step()
      c.observed.firstUses.expect(1.U) // Three lanes, one block lifetime.
      c.io.load.foreach(_.s2_use.poke(false.B))
      loadS0(c); c.clock.step(); loadS1(c)
      c.io.load(0).s1_kill.poke(true.B)
      c.io.load(0).s1_hit.expect(false.B); c.io.load(0).s1_retry.expect(false.B)
      c.clock.step(); c.io.load(0).s2_dataResp.valid.expect(false.B)
    }
  }

  it should "respond without a use wait and keep the registered Store snapshot stable across abort" in {
    simulate(new PBTestTop) { c =>
      init(c); val id = reserve(c); refill(c, id)
      loadS0(c)
      pipeS0(c)
      loadS1(c)
      pipeS1(c, store = true)
      c.io.pipe.s2_storeResp.valid.expect(true.B)
      c.io.pipe.s2_storeResp.bits.expect(true.B)
      c.io.pipe.s2_dataResp.valid.expect(true.B)
      c.io.pipe.s2_dataResp.bits.data.expect(line().U)
      c.io.pipe.s2_dataResp.bits.used.expect(false.B)
      c.observed.resident(id).expect(false.B)
      use(c)
      // Load use updates metadata without a combinational bypass into this Store response.
      c.io.pipe.s2_dataResp.bits.used.expect(false.B)
      c.clock.step()
      c.io.load(0).s2_use.poke(false.B)
      c.clock.step(3)
      c.io.pipe.s2_dataResp.valid.expect(true.B)
      c.io.pipe.s2_dataResp.bits.used.expect(true.B)
      c.observed.firstUses.expect(1.U)
      c.io.pipe.s2_dataResp.ready.poke(true.B)
      c.io.pipe.s2_moveAbort.valid.poke(true.B); c.io.pipe.s2_moveAbort.bits.entryId.poke(id.U)
      c.clock.step()
      c.io.pipe.s2_moveAbort.valid.poke(false.B); c.io.pipe.s2_dataResp.ready.poke(false.B)
      c.observed.resident(id).expect(true.B)
      c.clock.step(4)
      c.io.pipe.s0_moveReq.valid.expect(true.B)
      c.observed.resident(id).expect(true.B) // Pending move is not a lock.
      c.io.pipe.s0_moveReq.ready.poke(true.B)
      pipeS0(c)
      c.io.pipe.s0_moveReq.ready.poke(false.B)
      c.observed.resident(id).expect(false.B)
      pipeS1(c)
      c.io.pipe.s2_dataResp.bits.used.expect(true.B)
      c.io.pipe.s2_dataResp.bits.data.expect(line().U)
      c.io.pipe.s2_dataResp.ready.poke(true.B); c.clock.step()
      c.io.pipe.s2_dataResp.ready.poke(false.B)
      c.io.pipe.s3_moveDone.valid.poke(true.B); c.io.pipe.s3_moveDone.bits.poke(id.U)
      c.clock.step()
      c.io.pipe.s3_moveDone.valid.poke(false.B)
      c.io.mshr.status(id).valid.expect(false.B)
      c.observed.firstUses.expect(1.U)
    }
  }

  it should "authorize the Probe-edge Load but block later reads and retain B or T until Probe completion" in {
    simulate(new PBTestTop) { c =>
      for (trunk <- Seq(false, true)) {
        init(c); val id = reserve(c)
        c.io.mshr.refillReq.bits.coh.state.poke(if (trunk) ClientStates.Trunk else ClientStates.Branch)
        refill(c, id)
        loadS0(c); c.clock.step(); loadS1(c)
        pipeS0(c, probe = true)
        c.io.pipe.s1_probeResp.valid.expect(true.B)
        c.io.pipe.s1_probeResp.bits.locked.expect(true.B)
        c.io.pipe.s1_probeResp.bits.coh.state.expect(if (trunk) ClientStates.Trunk else ClientStates.Branch)
        use(c)
        loadS0(c)
        pipeS1(c)
        c.io.load(0).s2_use.poke(false.B)
        c.io.pipe.s2_dataResp.valid.expect(false.B)
        loadS1(c)
        c.io.load(0).s1_hit.expect(false.B); c.io.load(0).s1_retry.expect(true.B)
        c.clock.step(); c.io.load(0).s1_paddr.valid.poke(false.B)
        c.clock.step(3)
        c.io.mshr.status(id).valid.expect(true.B)
        c.io.pipe.s3_probeDone.valid.poke(true.B); c.io.pipe.s3_probeDone.bits.poke(id.U)
        c.clock.step(); c.io.pipe.s3_probeDone.valid.poke(false.B)
        c.clock.step(3)
        c.io.mshr.status(id).valid.expect(false.B)
        c.observed.firstUses.expect(1.U); c.observed.unusedExits.expect(0.U)
      }
    }
  }

  it should "hand same-edge Release ownership to WBQ and attribute a late Load use to the retired allocation" in {
    simulate(new PBTestTop) { c =>
      init(c)
      for (i <- c.io.mshr.status.indices) { val id = reserve(c, address + i * 64); refill(c, id, seed = i) }
      val nextAddr = address + c.io.mshr.status.size * 64
      c.io.mshr.allocReq.bits.addr.poke(nextAddr.U); c.io.mshr.allocReq.bits.vaddr.poke(nextAddr.U)
      c.io.mshr.allocReq.valid.poke(true.B)
      val retiringAddr = awaitRelease(c)
      val retiringId = c.io.mshr.status.indexWhere(_.bits.peek().litValue == retiringAddr)
      loadS0(c, addr = retiringAddr); c.clock.step(); loadS1(c, addr = retiringAddr)
      c.io.releaseReq.ready.poke(true.B)
      pipeS0(c, retiringAddr, probe = true)
      c.io.releaseReq.ready.poke(false.B)
      c.observed.released(retiringId).expect(true.B)
      c.io.pipe.s1_probeResp.bits.locked.expect(false.B)
      c.io.pipe.s1_probeResp.bits.coh.state.expect(ClientStates.Nothing)
      c.io.mshr.allocReq.ready.expect(false.B)
      loadS1(c, addr = retiringAddr)
      c.io.load(0).s1_hit.expect(false.B)
      c.io.load(0).s1_retry.expect(false.B)
      c.io.load(0).s1_paddr.valid.poke(false.B)
      use(c)
      c.clock.step()
      c.observed.released(retiringId).expect(false.B)
      c.io.mshr.allocReq.ready.expect(true.B)
      pipeS1(c, retiringAddr) // Reallocate only after the old Load reports use.
      c.io.mshr.allocReq.valid.poke(false.B)
      c.io.load(0).s2_use.poke(false.B)
      c.clock.step(4)
      assert(c.observed.firstUses.peek().litValue <= 1)
      c.observed.used(retiringId).expect(false.B)
      assert(c.observed.unusedExits.peek().litValue <= 1)
      c.io.pipe.s2_dataResp.valid.expect(false.B)
      refill(c, retiringId, seed = 123)
      loadS0(c, addr = nextAddr); c.clock.step(); loadS1(c, addr = nextAddr); c.clock.step()
      c.io.load(0).s2_dataResp.bits.data.expect((line(123) & ((BigInt(1) << 128) - 1)).U)
    }
  }

  it should "allow Store takeover of an unaccepted Release without writing Store bytes" in {
    simulate(new PBTestTop) { c =>
      init(c)
      for (i <- c.io.mshr.status.indices) { val id = reserve(c, address + i * 64); refill(c, id) }
      c.io.mshr.allocReq.bits.addr.poke((address + 4096).U)
      c.io.mshr.allocReq.bits.vaddr.poke((address + 4096).U)
      c.io.mshr.allocReq.valid.poke(true.B)
      val target = awaitRelease(c)
      pipeS0(c, target)
      pipeS1(c, target, store = true)
      c.io.mshr.allocReq.valid.poke(false.B)
      c.io.pipe.s2_dataResp.valid.expect(true.B)
      c.io.pipe.s2_storeResp.valid.expect(true.B)
      c.io.pipe.s2_storeResp.bits.expect(true.B)
      c.io.pipe.s2_dataResp.bits.data.expect(line().U)
      c.io.pipe.s2_dataResp.bits.used.expect(false.B)
      // The release scheduler can offer another entry after Store takes the original candidate.
      if (c.io.releaseReq.valid.peek().litToBoolean) {
        assert(c.io.releaseReq.bits.addr.peek().litValue != target)
      }
      c.io.pipe.s2_dataResp.ready.poke(true.B); c.clock.step()
      c.io.pipe.s2_dataResp.ready.poke(false.B)
      val id = c.io.pipe.s2_dataResp.bits.entryId.peek()
      c.io.pipe.s3_moveDone.valid.poke(true.B); c.io.pipe.s3_moveDone.bits.poke(id)
      c.clock.step(); c.io.pipe.s3_moveDone.valid.poke(false.B)
    }
  }

  it should "cancel Reserved slots and exclude denied or corrupt refills from usable data" in {
    simulate(new PBTestTop) { c =>
      init(c)
      val cancelled = reserve(c, owner = 1)
      c.io.mshr.cancelReq(1).valid.poke(true.B); c.io.mshr.cancelReq(1).bits.poke(cancelled.U)
      c.clock.step(); c.io.mshr.cancelReq(1).valid.poke(false.B)
      val denied = reserve(c)
      assert(denied == cancelled)
      c.io.mshr.refillReq.bits.denied.poke(true.B); refill(c, denied)
      c.io.mshr.refillReq.bits.denied.poke(false.B)
      c.io.mshr.status(denied).valid.expect(false.B)
      val corrupt = reserve(c)
      c.io.mshr.refillReq.bits.corrupt.poke(true.B); refill(c, corrupt)
      c.io.mshr.refillReq.bits.corrupt.poke(false.B)
      loadS0(c); c.clock.step(); loadS1(c)
      c.io.load(0).s1_hit.expect(false.B); c.io.load(0).s1_retry.expect(true.B)
      c.clock.step(); c.io.load(0).s1_paddr.valid.poke(false.B)
      c.io.mshr.refillReq.bits.corrupt.poke(true.B)
      for (i <- 1 until c.io.mshr.status.size) {
        val id = reserve(c, address + i * 64, owner = i)
        refill(c, id, owner = i, seed = i)
      }
      c.io.mshr.refillReq.bits.corrupt.poke(false.B)
      awaitRelease(c)
      c.io.releaseReq.bits.hasData.expect(false.B)
      c.io.releaseReq.bits.dirty.expect(false.B)
      c.io.releaseReq.ready.poke(true.B); c.clock.step()
      c.io.releaseReq.ready.poke(false.B)
      c.clock.step(4)
      c.observed.firstUses.expect(0.U); c.observed.unusedExits.expect(0.U)
    }
  }
}
