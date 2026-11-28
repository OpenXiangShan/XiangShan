// Copyright (c) 2026 Institute of Computing Technology, Chinese Academy of Sciences
// Licensed under Mulan PSL v2. See LICENSE for details.
package cache

import chisel3._
import chisel3.simulator.EphemeralSimulator._
import org.scalatest.flatspec.AnyFlatSpec
import top._
import xiangshan.XSTileKey
import xscache.coupledL2._

class SDBPIntegrationTest extends AnyFlatSpec {
  behavior of "SDBP configuration"
  it should "use four 256 KiB slices and a six-way sampler with PC and prefetch controls" in {
    val config = new SDBPConfig(1)
    val core = config(XSTileKey).head
    val l2 = core.L2CacheParamsOpt.get
    assert(core.L2NBanks == 4 && l2.sets == 512 && l2.ways == 8)
    assert(core.L2NBanks * l2.toCacheParams.capacity == 1024 * 1024)
    assert(l2.replacement == "sdbp" && l2.sdbp.samplerSets == 128 && l2.sdbp.ways(l2.ways) == 6)
    assert(l2.clientCaches.head.pcBitOpt.nonEmpty && l2.icacheCopy.nonEmpty)
    assert(!core.EnableCachePrefetch)
    assert(core.prefetcher.nonEmpty && l2.prefetch.nonEmpty)
    assert((new SDBPLRUConfig(1))(XSTileKey).head.L2CacheParamsOpt.get.replacement == "lru")
  }
  it should "retain configured prefetchers while selecting configuration defaults" in {
    val baseline = (new DefaultConfig(1))(XSTileKey).head
    for (name <- Seq("SDBPConfig", "SDBPLRUConfig")) {
      val (config, _, _) = ArgParser.parse(Array("--config", name, "--with-constantin"))
      val core = config(XSTileKey).head
      assert(!core.EnableCachePrefetch)
      assert(core.prefetcher == baseline.prefetcher)
      assert(core.L2CacheParamsOpt.get.prefetch.nonEmpty)
      assert(core.EnableSoftPrefetchAfterReset == baseline.EnableSoftPrefetchAfterReset)
      assert(core.EnableStorePrefetchAtIssue == baseline.EnableStorePrefetchAtIssue)
      assert(core.EnableStorePrefetchAtCommit == baseline.EnableStorePrefetchAtCommit)
      assert(core.EnableStorePrefetchSMS == baseline.EnableStorePrefetchSMS)
      assert(core.EnableStorePrefetchSPB == baseline.EnableStorePrefetchSPB)
      assert(config(xiangshan.DebugOptionsKey).EnableConstantin)
    }
    assert(baseline.EnableCachePrefetch)
  }

  it should "enable cache prefetch in the split-source performance preset" in {
    val (config, _, _) = ArgParser.parse(Array("--config", "SDBPDataPrefetchConfig"))
    val core = config(XSTileKey).head
    val baseline = (new SDBPConfig(1))(XSTileKey).head
    assert(core.EnableCachePrefetch)
    assert(core.prefetcher == baseline.prefetcher)
    assert(core.L2CacheParamsOpt == baseline.L2CacheParamsOpt)
    assert(core.L2CacheParamsOpt.get.sdbp.splitPrefetchSources)
  }

  it should "change only L2 replacement in the LRU data-prefetch preset" in {
    val (config, _, _) = ArgParser.parse(Array("--config", "SDBPLRUDataPrefetchConfig"))
    val core = config(XSTileKey).head
    val baseline = (new SDBPDataPrefetchConfig(1))(XSTileKey).head
    assert(core.EnableCachePrefetch && baseline.EnableCachePrefetch)
    assert(core.prefetcher == baseline.prefetcher)
    assert(core.L2CacheParamsOpt == baseline.L2CacheParamsOpt.map(_.copy(replacement = "lru")))
  }

  it should "change only L2 replacement in the DRRIP data-prefetch preset" in {
    val (config, _, _) = ArgParser.parse(Array("--config", "SDBPDRRIPDataPrefetchConfig"))
    val core = config(XSTileKey).head
    val baseline = (new SDBPDataPrefetchConfig(1))(XSTileKey).head
    assert(core == baseline.copy(L2CacheParamsOpt = baseline.L2CacheParamsOpt.map(_.copy(replacement = "drrip"))))
  }

  behavior of "SDBP three-stage commits"
  it should "predict by current source and train by the displaced or hit sampler entry's source" in {
    import SDBPSource.{Demand, L1Stream, L2Stream, L2SmsAgt, L2Pht, Other}
    val params = SDBPParams(samplerSets = 2, samplerWays = Some(1), predictorEntries = 64)
    val resource = scala.io.Source.fromResource("sdbp-root-hash-vectors.csv")
    val indices = try resource.getLines().next().split(',').drop(2).map(_.toInt & 63) finally resource.close()
    val counters = SDBPSource.predictorSources(params.splitPrefetchSources).map(_ -> Array(0, 0, 0)).toMap
    simulate(new SDBP(8, 4, params)) { dut =>
      dut.io.read.valid.poke(false.B); dut.io.commit.valid.poke(false.B)
      dut.io.read.bits.candidate.poke(false.B)
      dut.io.read.bits.signatureValid.poke(false.B)
      dut.reset.poke(true.B); dut.clock.step(2); dut.reset.poke(false.B)
      def access(tag: Int, accepted: Boolean, refill: Boolean, source: SDBPSource.Type = Demand,
        pcValid: Boolean = true, training: Option[(SDBPSource.Type, Boolean)] = None, set: Int = 0,
        samplerWrite: Boolean = true, candidate: Boolean = true, hintHit: Boolean = false): Unit = {
        dut.io.read.bits.set.poke(set.U); dut.io.read.bits.tag.poke(tag.U)
        dut.io.read.bits.candidate.poke(candidate.B)
        dut.io.read.bits.signature.poke(0.U) // PC zero is valid, not a missing-PC sentinel.
        dut.io.read.bits.signatureValid.poke(pcValid.B)
        dut.io.read.bits.source.poke(source)
        for (table <- 0 until 3) dut.io.read.bits.indices(table).poke(indices(table).U)
        val predicts = pcValid && source != Other
        val prediction = predicts && counters(source).sum >= 8
        dut.io.read.valid.poke(true.B); dut.clock.step()
        dut.io.read.valid.poke(false.B); dut.clock.step()
        dut.io.commit.valid.poke(accepted.B)
        dut.io.commit.bits.set.poke(set.U); dut.io.commit.bits.way.poke(2.U)
        dut.io.commit.bits.refill.poke(refill.B)
        dut.io.commit.bits.hintHit.poke(hintHit.B)
        dut.io.liveTraining.expect(training.exists(!_._2).B)
        dut.io.deadTraining.expect(training.exists(_._2).B)
        dut.io.samplerWrite.expect((accepted && predicts && samplerWrite).B)
        val deadWrite = accepted && (refill || predicts || source == Demand)
        dut.io.deadWrite.valid.expect(deadWrite.B)
        if (deadWrite) dut.io.deadWrite.bits.dead.expect(prediction.B)
        dut.clock.step(); dut.io.commit.valid.poke(false.B)
        training.foreach { case (previousSource, dead) =>
          for (table <- 0 until 3) {
            val old = counters(previousSource)(table)
            counters(previousSource)(table) =
              if (dead) math.min(3, old + 1) else if (table == 1) old >> 1 else math.max(0, old - 1)
          }
        }
      }
      access(1, accepted = false, refill = false) // Initial miss: no allocation/training.
      access(2, accepted = true, refill = true)
      access(3, accepted = false, refill = true) // Retry/denied/cancelled installation.
      access(2, accepted = true, refill = false, training = Some(Demand -> false))
      access(3, accepted = true, refill = true, training = Some(Demand -> true))
      access(4, accepted = true, refill = true, training = Some(Demand -> true))
      access(5, accepted = true, refill = true, training = Some(Demand -> true))
      access(6, accepted = true, refill = false, set = 1, samplerWrite = false) // Unsampled still predicts.
      access(5, accepted = true, refill = false, source = L1Stream, pcValid = false, hintHit = true)
      access(5, accepted = true, refill = false, source = L1Stream, training = Some(Demand -> false), hintHit = true)
      access(5, accepted = true, refill = false, source = L2Stream, training = Some(L1Stream -> false), hintHit = true)
      access(8, accepted = true, refill = true, source = L2SmsAgt, training = Some(L2Stream -> true))
      for (tag <- 9 to 12) {
        access(tag, accepted = true, refill = true, source = L2SmsAgt, training = Some(L2SmsAgt -> true))
      }
      access(12, accepted = true, refill = false, source = Other)
      access(12, accepted = true, refill = true, source = L2Pht, pcValid = false)
      access(12, accepted = true, refill = false, pcValid = false)
      for (source <- Seq(L2SmsAgt, L2Stream, L2Pht, L1Stream, Demand)) {
        access(13, accepted = true, refill = false, source = source, set = 1, samplerWrite = false)
      }
      access(12, accepted = true, refill = false, source = L2Pht, training = Some(L2SmsAgt -> false))
      access(14, accepted = false, refill = false, candidate = false, samplerWrite = false)
    }
  }
}
