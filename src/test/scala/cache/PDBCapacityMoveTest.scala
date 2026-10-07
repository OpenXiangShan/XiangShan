package cache

import chisel3._
import freechips.rocketchip.tilelink.ClientStates
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import top.{DefaultConfig, WithPDB}
import utility._
import xiangshan.{XSCoreParamsKey, XSTileKey}

class PDBCapacityMoveTest extends AnyFlatSpec with PBTestDriver {
  private val base = new WithPDB(4, "lru") ++ new DefaultConfig
  implicit private val p: Parameters = base.alterPartial {
    case XSCoreParamsKey => base(XSTileKey).head
    case PerfCounterOptionsKey => PerfCounterOptions(false, false, XSPerfLevel.VERBOSE, 0)
    case LogUtilsOptionsKey => LogUtilsOptions(false, false, false, false)
  }

  private def initialize(c: PBVictimTestTop, usedMove: Boolean = true): Unit = {
    c.demand.foreach { q => q.valid.poke(false.B); q.bits.poke(0.U) }
    init(c); c.io.dcache.usedMove.poke(usedMove.B)
  }
  private def consume(c: PBTestTop, addr: BigInt): Unit = {
    loadS1(c, addr = addr); c.clock.step(); use(c); c.clock.step()
    c.io.load(0).s2_use.poke(false.B)
  }
  private def fillUsedOldest(c: PBVictimTestTop): Unit = {
    for (i <- c.io.mshr.status.indices) {
      val id = reserve(c, address + i * 64); refill(c, id, seed = i)
      consume(c, address + i * 64)
    }
  }
  private def awaitMove(c: PBTestTop): Int = {
    c.io.mshr.refillWait.poke(true.B)
    var cycles = 0
    while (!c.io.pipe.s0_moveReq.valid.peek().litToBoolean && cycles < 20) {
      c.clock.step(); cycles += 1
    }
    c.io.pipe.s0_moveReq.valid.expect(true.B)
    c.io.releaseReq.valid.expect(false.B)
    c.io.pipe.s0_moveReq.bits.entryId.peek().litValue.toInt
  }
  private def acceptMove(c: PBTestTop): Unit = {
    c.io.pipe.s0_moveReq.ready.poke(true.B); c.clock.step()
    c.io.pipe.s0_moveReq.ready.poke(false.B)
    pipeS1(c)
    c.io.pipe.s2_dataResp.valid.expect(true.B)
    c.io.pipe.s2_dataResp.bits.retry.expect(false.B)
    c.io.pipe.s2_dataResp.bits.data.expect(line().U)
    c.io.pipe.s2_dataResp.bits.coh.state.expect(ClientStates.Trunk)
  }
  private def query(c: PBVictimTestTop): Unit = {
    c.demand.foreach { q => q.valid.poke(true.B); q.bits.poke((address >> 6).U) }
    c.clock.step(); c.demand.foreach(_.valid.poke(false.B)); c.clock.step(4)
  }

  behavior of "PDB capacity victim move"
  for ((usedPolicy, unusedPolicy) <- Seq((false, false), (true, false), (false, true), (true, true))) {
    it should s"route both victim classes with usedPermission=$usedPolicy unusedPermission=$unusedPolicy" in {
      simulate(new PBVictimTestTop) { c =>
        for (used <- Seq(false, true)) {
          initialize(c, usedMove = usedPolicy); c.io.dcache.unusedMove.poke(unusedPolicy.B)
          if (used) fillUsedOldest(c)
          else for (i <- c.io.mshr.status.indices) { val id = reserve(c, address + i * 64); refill(c, id) }
          if (if (used) usedPolicy else unusedPolicy) {
            val id = awaitMove(c); acceptMove(c)
            c.io.pipe.s2_dataResp.bits.used.expect(used.B)
            c.io.pipe.s2_dataResp.ready.poke(true.B); c.clock.step()
            c.io.pipe.s2_dataResp.ready.poke(false.B)
            c.io.pipe.s3_moveDone.valid.poke(true.B); c.io.pipe.s3_moveDone.bits.poke(id.U)
            c.clock.step(); c.io.pipe.s3_moveDone.valid.poke(false.B)
          } else {
            assert(awaitRelease(c) == address)
            c.io.releaseReq.ready.poke(true.B); c.clock.step(); c.io.releaseReq.ready.poke(false.B)
          }
          c.io.mshr.refillWait.poke(false.B); c.clock.step(5)
          c.results.victims.expect(1.U)
          query(c); c.results.usedHits.expect((if (used) 1 else 0).U)
          c.results.unusedHits.expect((if (used) 0 else 1).U)
        }
      }
    }
  }

  it should "include a final S2 use when an unused victim starts moving on the Load authorization edge" in {
    simulate(new PBVictimTestTop) { c =>
      initialize(c, usedMove = false); c.io.dcache.unusedMove.poke(true.B)
      for (i <- c.io.mshr.status.indices) { val id = reserve(c, address + i * 64); refill(c, id) }
      val id = awaitMove(c)
      loadS1(c); c.io.load(0).s1_hit.expect(true.B)
      c.io.pipe.s0_moveReq.ready.poke(true.B); c.clock.step()
      c.io.pipe.s0_moveReq.ready.poke(false.B)
      use(c); pipeS1(c); c.io.load(0).s2_use.poke(false.B)
      c.io.pipe.s2_dataResp.bits.used.expect(true.B)
      c.io.pipe.s2_dataResp.ready.poke(true.B); c.clock.step()
      c.io.pipe.s2_dataResp.ready.poke(false.B); c.io.mshr.refillWait.poke(false.B)
      c.io.pipe.s3_moveDone.valid.poke(true.B); c.io.pipe.s3_moveDone.bits.poke(id.U)
      c.clock.step(); c.io.pipe.s3_moveDone.valid.poke(false.B); c.clock.step(5)
      c.results.victims.expect(1.U); c.results.lastUsed.expect(true.B)
      query(c); c.results.usedHits.expect(1.U); c.results.unusedHits.expect(0.U)
    }
  }
  it should "move only the chosen used LRU victim, retain stalled ownership and publish reuse after completion" in {
    simulate(new PBVictimTestTop) { c =>
      initialize(c); fillUsedOldest(c)
      c.clock.step(5); c.io.pipe.s0_moveReq.valid.expect(false.B)
      val id = awaitMove(c)
      c.io.pipe.s0_moveReq.bits.paddr.expect(address.U)
      // Policy and recency updates cannot change an already presented request.
      c.io.dcache.usedMove.poke(false.B); consume(c, address)
      c.clock.step(5); c.io.pipe.s0_moveReq.bits.entryId.expect(id.U)
      c.results.victims.expect(0.U)
      acceptMove(c)
      c.io.pipe.s2_dataResp.bits.used.expect(true.B)
      c.observed.moveLocked(id).expect(true.B)
      c.clock.step(5)
      c.io.releaseReq.valid.expect(false.B); c.io.pipe.s0_moveReq.valid.expect(false.B)
      c.io.mshr.allocReq.ready.expect(false.B); c.results.victims.expect(0.U)
      c.io.pipe.s2_dataResp.ready.poke(true.B); c.clock.step()
      c.io.pipe.s2_dataResp.ready.poke(false.B)
      c.io.mshr.refillWait.poke(false.B)
      c.io.pipe.s3_moveDone.valid.poke(true.B); c.io.pipe.s3_moveDone.bits.poke(id.U)
      c.clock.step(); c.io.pipe.s3_moveDone.valid.poke(false.B)
      c.io.dcache.perf.capacityVictim.valid.expect(true.B)
      c.io.dcache.perf.capacityVictim.bits.used.expect(true.B)
      val reused = reserve(c, address + 256); assert(reused == id); refill(c, reused, seed = 9)
      c.clock.step(5); c.results.victims.expect(1.U)
      query(c); c.results.usedHits.expect(1.U); c.results.unusedHits.expect(0.U)
      query(c); c.results.usedHits.expect(1.U)
    }
  }

  for (used <- Seq(false, true)) {
    it should s"release a victim with used=$used when its move class is disabled and continue observing it" in {
      simulate(new PBVictimTestTop) { c =>
        initialize(c, usedMove = !used)
        if (used) fillUsedOldest(c)
        else for (i <- c.io.mshr.status.indices) { val id = reserve(c, address + i * 64); refill(c, id) }
        assert(awaitRelease(c) == address)
        c.io.pipe.s0_moveReq.valid.expect(false.B)
        c.io.releaseReq.ready.poke(true.B); c.clock.step()
        c.io.releaseReq.ready.poke(false.B); c.io.mshr.refillWait.poke(false.B)
        c.clock.step(5); c.results.victims.expect(1.U)
        query(c); c.results.usedHits.expect((if (used) 1 else 0).U)
        c.results.unusedHits.expect((if (used) 0 else 1).U)
      }
    }
  }

  for (bad <- Seq(false, true)) {
    it should s"restore an aborted capacity move without a victim event, with dataBad=$bad" in {
      simulate(new PBVictimTestTop) { c =>
        initialize(c); fillUsedOldest(c); val id = awaitMove(c); acceptMove(c)
        c.io.pipe.s2_dataResp.ready.poke(true.B)
        c.io.pipe.s2_moveAbort.valid.poke(true.B)
        c.io.pipe.s2_moveAbort.bits.entryId.poke(id.U)
        c.io.pipe.s2_moveAbort.bits.dataBad.poke(bad.B)
        c.io.mshr.refillWait.poke(false.B)
        c.clock.step(); c.io.pipe.s2_moveAbort.valid.poke(false.B)
        c.io.pipe.s2_dataResp.ready.poke(false.B); c.clock.step(4)
        c.results.victims.expect(0.U); c.observed.moveLocked(id).expect(false.B)
        if (!bad) {
          loadS1(c); c.io.load(0).s1_hit.expect(true.B); c.clock.step()
          c.io.load(0).s1_paddr.valid.poke(false.B)
          c.io.load(0).s2_dataResp.bits.data.expect((line() & ((BigInt(1) << 128) - 1)).U)
          awaitMove(c)
        } else {
          assert(awaitRelease(c) == address)
          c.io.releaseReq.ready.poke(true.B); c.clock.step()
          c.io.mshr.refillWait.poke(false.B); c.io.releaseReq.ready.poke(false.B)
          c.clock.step(5); c.results.victims.expect(0.U)
        }
      }
    }
  }

  for (store <- Seq(false, true)) {
    it should s"yield a waiting capacity move to ${if (store) "Store" else "Probe"} without publishing a capacity victim" in {
      simulate(new PBVictimTestTop) { c =>
        initialize(c); fillUsedOldest(c); val id = awaitMove(c)
        if (!store) pipeS0(c, probe = true)
        c.io.mshr.refillWait.poke(false.B)
        pipeS1(c, store = store)
        c.io.pipe.s2_dataResp.ready.poke(true.B); c.clock.step()
        c.io.pipe.s2_dataResp.ready.poke(false.B)
        if (store) {
          c.io.pipe.s3_moveDone.valid.poke(true.B); c.io.pipe.s3_moveDone.bits.poke(id.U)
        } else {
          c.io.pipe.s3_probeDone.valid.poke(true.B); c.io.pipe.s3_probeDone.bits.poke(id.U)
        }
        c.clock.step(); c.io.pipe.s3_moveDone.valid.poke(false.B); c.io.pipe.s3_probeDone.valid.poke(false.B)
        c.clock.step(5); c.results.victims.expect(0.U)
        c.io.pipe.s0_moveReq.valid.expect(false.B)
      }
    }
  }
}
