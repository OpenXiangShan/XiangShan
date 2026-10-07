package cache

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import top._
import utility._
import xiangshan.{XSCoreParamsKey, XSTileKey}
import xiangshan.cache.{PDBLRU, PDBVictimObserverKey, PDBVictimObserverParameters}
import xiangshan.mem.prefetch.{PDBDepthKey, StreamDepthKey, StreamDepthParameters}

class PDBConfigurationTest extends AnyFlatSpec {
  behavior of "PDB experiment configurations"
  it should "change only capacity and replacement in every configured core" in {
    val base = new DefaultConfig(2)
    for ((config, entries, policy) <- Seq(
      (new DefaultConfig(2), 64, "lru"),
      (new NoPDBConfig(2), 0, "rr"), (new PDB16Config(2), 16, "rr"),
      (new PDB64Config(2), 64, "rr"), (new PDB16LRUConfig(2), 16, "lru"),
      (new PDB64LRUConfig(2), 64, "lru"))) {
      config(XSTileKey).zip(base(XSTileKey)).foreach { case (actual, original) =>
        val cache = actual.dcacheParametersOpt.get
        assert(cache.nPBEntries == entries && cache.pbReplacer == policy)
        assert(actual.copy(dcacheParametersOpt = original.dcacheParametersOpt) == original)
        val originalCache = original.dcacheParametersOpt.get
        assert(cache.copy(nPBEntries = originalCache.nPBEntries,
          pbReplacer = originalCache.pbReplacer) == originalCache)
      }
      assert(config(StreamDepthKey) == StreamDepthParameters(useMonitor = config(PDBDepthKey).enabled))
    }
    assert(new PDB64MonitorDepthConfig()(StreamDepthKey).useMonitor)
    assert(!new PDB64MonitorDepthConfig()(StreamDepthKey).enableLegacyControl)
    assert(base(XSTileKey) == new PDB64LRUConfig(2)(XSTileKey))
    assert(base(StreamDepthKey) == StreamDepthParameters(useMonitor = true, fixedL1 = 64))
    assert(base(PDBDepthKey).enabled)
    assert(!new PDB64LRUConfig()(PDBDepthKey).enabled)
    assert(base(PDBVictimObserverKey) == PDBVictimObserverParameters(entries = 256, bankEntries = 32))
  }
  it should "reject invalid depth sources and unsafe legacy starting depths" in {
    intercept[IllegalArgumentException] { StreamDepthParameters(fixedL1 = 0) }
    intercept[IllegalArgumentException] { StreamDepthParameters(initial = 4096) }
    intercept[IllegalArgumentException] { StreamDepthParameters(enableLegacyControl = true) }
    intercept[IllegalArgumentException] {
      StreamDepthParameters(useMonitor = true, initial = 24, enableLegacyControl = true)
    }
    assert(StreamDepthParameters(useMonitor = true, initial = 24).initial == 24)
  }
}

class PDBLRUTest extends AnyFlatSpec with ChiselSim {
  behavior of "PDB recency selection"
  for (entries <- Seq(1, 4, 16, 64)) {
    it should s"match a software recency list under simultaneous touches and masked selection for $entries entries" in {
      simulate(new PDBLRU(entries)) { c =>
        var order = (0 until entries).toList
        val rng = new scala.util.Random(194 + entries)
        c.io.touch.foreach(_.poke(false.B))
        c.io.eligible.foreach(_.poke(false.B))
        c.reset.poke(true.B); c.clock.step(2); c.reset.poke(false.B)
        for (_ <- 0 until 1000) {
          val eligible = (0 until entries).filter(_ => rng.nextBoolean()).toSet
          val touch = (0 until entries).filter(_ => rng.nextInt(5) == 0).toSet
          for (i <- 0 until entries) {
            c.io.eligible(i).poke(eligible(i).B)
            c.io.touch(i).poke(touch(i).B)
          }
          c.io.victim.valid.expect(eligible.nonEmpty.B)
          order.find(eligible).foreach(i => c.io.victim.bits.expect(i.U))
          c.clock.step()
          order = order.filterNot(touch) ++ touch.toList.sorted
        }
      }
    }
  }
}

class PDBCapacityPolicyTest extends AnyFlatSpec with PBTestDriver {
  behavior of "PDB capacity and demand-use recency"
  for (entries <- Seq(16, 64)) {
    it should s"retain reused lines and hold a selected release while stalled at $entries entries" in {
      val base = new WithPDB(entries, "lru") ++ new DefaultConfig
      implicit val p: Parameters = base.alterPartial {
        case XSCoreParamsKey => base(XSTileKey).head
        case PerfCounterOptionsKey => PerfCounterOptions(false, false, XSPerfLevel.VERBOSE, 0)
        case LogUtilsOptionsKey => LogUtilsOptions(false, false, false, false)
      }
      simulate(new PBTestTop(enableMoveToDCache = false)) { c =>
        init(c)
        val ids = (0 until entries).map { i =>
          val id = reserve(c, address + i * 64)
          refill(c, id, seed = i)
          id
        }
        assert(ids.distinct.size == entries && ids.last == entries - 1)
        c.io.mshr.allocReq.ready.expect(false.B)
        // Three independent used lines become newer than all untouched lines.
        for (lane <- 0 until 3) loadS1(c, lane, address + lane * 64)
        c.clock.step()
        for (lane <- 0 until 3) use(c, lane)
        c.clock.step(); c.io.load.foreach(_.s2_use.poke(false.B))
        val victim = awaitRelease(c)
        assert(victim == address + 3 * 64)
        // Later use cannot change a request already held for WBQueue.
        loadS1(c, addr = victim); c.clock.step(); use(c); c.clock.step()
        c.io.load(0).s2_use.poke(false.B)
        c.io.releaseReq.bits.addr.expect(victim.U)
        c.clock.step(3)
        c.io.releaseReq.valid.expect(true.B)
        c.io.releaseReq.bits.addr.expect(victim.U)
        // Withdrawing capacity pressure cancels selection, so new recency is visible.
        c.io.mshr.refillWait.poke(false.B); c.clock.step(2)
        c.io.releaseReq.valid.expect(false.B)
        val next = awaitRelease(c)
        assert(next == address + 4 * 64)
        c.io.releaseReq.ready.poke(true.B); c.clock.step()
        c.io.releaseReq.ready.poke(false.B)
        c.io.mshr.refillWait.poke(false.B); c.clock.step(2)
        val reused = reserve(c, address + entries * 64)
        assert(reused == ids(4))
        refill(c, reused)
        assert(awaitRelease(c) == address + 5 * 64)
        c.io.pipe.s0_moveReq.valid.expect(false.B)
      }
    }
  }
}
