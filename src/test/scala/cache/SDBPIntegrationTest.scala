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

  behavior of "SDBP three-stage commits"
  it should "train only accepted hits and installations, preserving prefetch hits" in {
    val params = SDBPParams(samplerSets = 2, samplerWays = Some(1), predictorEntries = 64)
    val resource = scala.io.Source.fromResource("sdbp-root-hash-vectors.csv")
    val indices = try resource.getLines().next().split(',').drop(2).map(_.toInt & 63) finally resource.close()
    var counters = Array(0, 0, 0)
    simulate(new SDBP(8, 4, params)) { dut =>
      dut.io.read.valid.poke(false.B); dut.io.commit.valid.poke(false.B)
      dut.io.read.bits.candidate.poke(false.B)
      dut.io.read.bits.signatureValid.poke(false.B)
      dut.reset.poke(true.B); dut.clock.step(2); dut.reset.poke(false.B)
      def access(tag: Int, accepted: Boolean, refill: Boolean, prefetch: Boolean = false,
        pcValid: Boolean = true, training: Option[Boolean] = None, set: Int = 0,
        samplerWrite: Boolean = true, candidate: Boolean = true): Unit = {
        dut.io.read.bits.set.poke(set.U); dut.io.read.bits.tag.poke(tag.U)
        dut.io.read.bits.candidate.poke(candidate.B)
        dut.io.read.bits.signature.poke(0.U) // PC zero is valid, not a missing-PC sentinel.
        dut.io.read.bits.signatureValid.poke(pcValid.B)
        dut.io.read.bits.prefetch.poke(prefetch.B)
        for (table <- 0 until 3) dut.io.read.bits.indices(table).poke(indices(table).U)
        val prediction = pcValid && !prefetch && counters.sum >= 8
        dut.io.read.valid.poke(true.B); dut.clock.step()
        dut.io.read.valid.poke(false.B); dut.clock.step()
        dut.io.commit.valid.poke(accepted.B)
        dut.io.commit.bits.set.poke(set.U); dut.io.commit.bits.way.poke(2.U)
        dut.io.commit.bits.refill.poke(refill.B)
        dut.io.liveTraining.expect(training.contains(false).B)
        dut.io.deadTraining.expect(training.contains(true).B)
        dut.io.samplerWrite.expect((accepted && samplerWrite).B)
        val deadWrite = accepted && (refill || !prefetch)
        dut.io.deadWrite.valid.expect(deadWrite.B)
        if (deadWrite) dut.io.deadWrite.bits.dead.expect(prediction.B)
        dut.clock.step(); dut.io.commit.valid.poke(false.B)
        training.foreach { dead =>
          counters = counters.zipWithIndex.map { case (old, table) =>
            if (dead) math.min(3, old + 1) else if (table == 1) old >> 1 else math.max(0, old - 1)
          }
        }
      }
      access(1, accepted = false, refill = false) // Initial miss: no allocation/training.
      access(2, accepted = true, refill = true)
      access(3, accepted = false, refill = true) // Retry/denied/cancelled installation.
      access(2, accepted = true, refill = false, training = Some(false))
      access(3, accepted = true, refill = true, training = Some(true))
      access(4, accepted = true, refill = true, training = Some(true))
      access(5, accepted = true, refill = true, training = Some(true))
      access(6, accepted = true, refill = false, set = 1, samplerWrite = false) // Unsampled still predicts.
      access(5, accepted = true, refill = false, prefetch = true, pcValid = false, samplerWrite = false)
      access(5, accepted = true, refill = false, training = Some(false)) // Prefetch did not erase old PC.
      access(5, accepted = true, refill = false, pcValid = false, training = Some(false))
      access(5, accepted = true, refill = false, pcValid = false)
      access(6, accepted = true, refill = true, prefetch = true, pcValid = false)
      access(6, accepted = true, refill = false) // Prefetch insertion's old signature is invalid.
      access(7, accepted = true, refill = true, prefetch = true, pcValid = false, training = Some(true))
      access(8, accepted = false, refill = false, candidate = false, samplerWrite = false)
    }
  }
}
