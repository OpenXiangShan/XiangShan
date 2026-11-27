// Copyright (c) 2026 Institute of Computing Technology, Chinese Academy of Sciences
// Licensed under Mulan PSL v2. See LICENSE for details.
package cache

import chisel3._
import chisel3.util._
import chisel3.simulator.EphemeralSimulator._
import org.scalatest.flatspec.AnyFlatSpec
import xscache.coupledL2._
import scala.util.Random

class SDBPHashHarness extends Module {
  val io = IO(new Bundle {
    val pc = Input(UInt(64.W))
    val set = Input(UInt(9.W))
    val signature = Output(UInt(15.W))
    val indices = Output(Vec(3, UInt(12.W)))
    val sampled = Output(Vec(4, Bool()))
  })
  io.signature := SDBPHash.signature(io.pc)
  io.indices := SDBPHash.indices(io.signature, 4096)
  io.sampled := VecInit(Seq(2, 64, 128, 512).map(sets => SDBPHash.sampled(io.set, 512, sets)))
}

class MaskedLRUHarness(ways: Int) extends Module {
  val io = IO(new Bundle {
    val touch = Flipped(Valid(UInt(log2Ceil(ways).W)))
    val mask = Input(UInt(ways.W))
    val victim = Output(UInt(ways.W))
  })
  val lru = new xscache.coupledL2.utils.TrueLRU(ways)
  when(io.touch.valid) { lru.access(io.touch.bits) }
  io.victim := lru.get_replace_OH(lru.state_read, io.mask)
}

class SDBPTest extends AnyFlatSpec {
  private val mask64 = (BigInt(1) << 64) - 1
  private def signature(pc: BigInt): BigInt = {
    var value = pc >> 1
    var folded = BigInt(0)
    while (value != 0) { folded ^= value & 32767; value >>= 15 }
    folded
  }
  private def index(sig: BigInt, table: Int, entries: Int): Int = {
    var value = sig ^ ((BigInt("9e3779b97f4a7c15", 16) * (table + 1)) & mask64)
    value = ((value ^ (value >> 30)) * BigInt("bf58476d1ce4e5b9", 16)) & mask64
    value = ((value ^ (value >> 27)) * BigInt("94d049bb133111eb", 16)) & mask64
    ((value ^ (value >> 31)) & (entries - 1)).toInt
  }
  behavior of "SDBP hashes and sampling"
  it should "match the ROOT 64-bit hash and enumerate every sampler index exactly once" in {
    simulate(new SDBPHashHarness) { dut =>
      val reference = scala.io.Source.fromResource("sdbp-root-hash-vectors.csv")
      try {
        for (line <- reference.getLines()) {
          val fields = line.split(',')
          dut.io.pc.poke(BigInt(fields(0), 16).U)
          dut.io.signature.expect(BigInt(fields(1)).U)
          for (table <- 0 until 3) dut.io.indices(table).expect(BigInt(fields(table + 2)).U)
        }
      } finally reference.close()
      val random = new Random(0x5db0)
      val pcs = Seq(BigInt(0), BigInt(1), mask64, BigInt("80000000", 16)) ++
        Seq.fill(300)(BigInt(64, random))
      for (pc <- pcs) {
        dut.io.pc.poke(pc.U)
        dut.io.signature.expect(signature(pc).U)
        for (table <- 0 until 3) dut.io.indices(table).expect(index(signature(pc), table, 4096).U)
      }
      for ((sets, port) <- Seq(2, 64, 128, 512).zipWithIndex) {
        val seen = scala.collection.mutable.Set[Int]()
        val lowBits = log2Ceil(sets)
        val compareBits = 9 - lowBits
        for (set <- 0 until 512) {
          dut.io.set.poke(set.U)
          val expected = (set >> lowBits) == (set & ((1 << compareBits) - 1))
          dut.io.sampled(port).expect(expected.B)
          if (expected) seen += set & (sets - 1)
        }
        assert(seen.size == sets)
      }
    }
  }

  behavior of "SDBP sampler"
  private val sdbpSources = SDBPSource.predictorSources(split = true)
  for (sets <- Seq(2, 64, 128, 512); ways <- Seq(1, 3, 6, 8, 12)) {
    it should s"preserve entries, true LRU, and training with $sets sets and $ways ways" in {
      val params = SDBPParams(samplerSets = sets, samplerWays = Some(ways), predictorEntries = 64)
      case class Entry(var valid: Boolean = false, var tag: Int = 0, var sig: Int = 0,
        var source: Int = 0, var dead: Boolean = false, var age: Int = 0)
      val model = Array.fill(sets, ways)(Entry())
      val random = new Random(sets * 31 + ways)
      val sampledSets = (0 until 512).filter { set =>
        (set >> log2Ceil(sets)) == (set & ((1 << (9 - log2Ceil(sets))) - 1))
      }
      simulate(new SDBPSampler(512, 8, params)) { dut =>
        dut.io.read.valid.poke(false.B)
        dut.io.commit.valid.poke(false.B)
        dut.reset.poke(true.B); dut.clock.step(2); dut.reset.poke(false.B)
        for (step <- 1 to 240) {
          val set = sampledSets(if (step < 80) 0 else random.nextInt(sets))
          val tag = if (step <= ways) step else random.nextInt(ways + 3)
          val sig = random.nextInt(32768)
          val pcValid = step % 7 != 0
          val source = step % sdbpSources.size
          val hintHit = source != 0 && step % 2 == 0
          val dead = step % 3 == 0
          val entries = model(set & (sets - 1))
          val hit = entries.indexWhere(e => e.valid && e.tag == tag)
          val invalid = entries.indexWhere(e => !e.valid)
          val deadWay = entries.indexWhere(e => e.valid && e.dead)
          val way = if (hit >= 0) hit else if (invalid >= 0) invalid else if (deadWay >= 0) deadWay
            else entries.indices.minBy(i => entries(i).age)
          val old = entries(way)
          val update = pcValid
          val train = update && old.valid
          dut.io.read.valid.poke(pcValid.B)
          dut.io.read.bits.set.poke(set.U); dut.io.read.bits.tag.poke(tag.U)
          dut.clock.step()
          dut.io.read.valid.poke(false.B); dut.clock.step()
          dut.io.commit.valid.poke(pcValid.B)
          dut.io.commit.bits.signature.poke(sig.U)
          dut.io.commit.bits.source.poke(sdbpSources(source))
          dut.io.commit.bits.dead.poke(dead.B)
          dut.io.commit.bits.hintHit.poke(hintHit.B)
          if (update) dut.io.selectedWay.expect(way.U)
          dut.io.hit.expect((update && hit >= 0).B)
          dut.io.writeEnable.expect(update.B)
          dut.io.training.valid.expect(train.B)
          if (train) {
            dut.io.training.bits.dead.expect((hit < 0).B)
            dut.io.training.bits.source.expect(sdbpSources(old.source))
            for (table <- 0 until 3) dut.io.training.bits.indices(table).expect(index(old.sig, table, 64).U)
          }
          dut.clock.step()
          dut.io.commit.valid.poke(false.B)
          if (update) {
            old.valid = true; old.tag = tag; old.sig = sig
            old.source = source; old.dead = dead
            if (!hintHit || hit < 0) old.age = step
          }
        }
      }
    }
  }
  it should "leave the Hint-hit way oldest while still writing and training the entry" in {
    import SDBPSource.{Demand, L2Stream}
    val params = SDBPParams(samplerSets = 2, samplerWays = Some(2), predictorEntries = 64)
    simulate(new SDBPSampler(4, 4, params)) { dut =>
      dut.io.read.valid.poke(false.B)
      dut.io.commit.valid.poke(false.B)
      def access(tag: Int, way: Int, hit: Boolean, source: SDBPSource.Type = Demand,
        hintHit: Boolean = false, trainedSource: Option[SDBPSource.Type] = None): Unit = {
        dut.io.read.bits.set.poke(0.U)
        dut.io.read.bits.tag.poke(tag.U)
        dut.io.read.valid.poke(true.B); dut.clock.step()
        dut.io.read.valid.poke(false.B); dut.clock.step()
        dut.io.commit.bits.signature.poke(tag.U)
        dut.io.commit.bits.source.poke(source)
        dut.io.commit.bits.dead.poke(false.B)
        dut.io.commit.bits.hintHit.poke(hintHit.B)
        dut.io.commit.valid.poke(true.B)
        dut.io.hit.expect(hit.B)
        dut.io.selectedWay.expect(way.U)
        dut.io.writeEnable.expect(true.B)
        dut.io.training.valid.expect(trainedSource.nonEmpty.B)
        trainedSource.foreach { oldSource =>
          dut.io.training.bits.source.expect(oldSource)
          dut.io.training.bits.dead.expect((!hit).B)
        }
        dut.clock.step(); dut.io.commit.valid.poke(false.B)
      }
      for ((name, source, hintHit, samplerHit, nextVictim) <- Seq(
        ("Hint hits the sampler", L2Stream, true, true, 0),
        ("demand hits the sampler", Demand, false, true, 1),
        ("prefetch refill hits the sampler", L2Stream, false, true, 1),
        ("Hint misses the sampler", L2Stream, true, false, 1)
      )) {
        withClue(s"$name: ") {
          dut.reset.poke(true.B); dut.clock.step(2); dut.reset.poke(false.B)
          access(1, 0, hit = false)
          access(2, 1, hit = false)
          access(if (samplerHit) 1 else 3, 0, samplerHit, source, hintHit, Some(Demand))
          access(4, nextVictim, hit = false, trainedSource = Some(if (nextVictim == 0) source else Demand))
        }
      }
    }
  }

  it should "reject invalid sampler geometry" in {
    for (sets <- Seq(0, 1, 3, 1024)) {
      intercept[IllegalArgumentException] { SDBPParams(samplerSets = sets).checkSets(512) }
    }
    intercept[IllegalArgumentException] { SDBPParams(samplerWays = Some(0)) }
    intercept[IllegalArgumentException] { SDBPParams(samplerWays = Some(-1)) }
  }

  it should "match sampler selections and training sources exported by GEM5" in {
    val params = SDBPParams(samplerSets = 128, samplerWays = Some(6))
    val source = scala.io.Source.fromResource("sdbp-root-sampler-vectors.csv")
    val events = try source.getLines().map(_.split(',').map(_.toInt)).toVector finally source.close()
    val banksById = sdbpSources.map(source => source.litValue.toInt -> source).toMap
    simulate(new SDBPSampler(512, 8, params)) { dut =>
      dut.io.read.valid.poke(false.B); dut.io.commit.valid.poke(false.B)
      dut.reset.poke(true.B); dut.clock.step(2); dut.reset.poke(false.B)
      for (event <- events) {
        val Array(set, tag, requestSource, pcValid, sig, prediction, sampled, hit, evicted,
          oldDead, oldValid, oldSig, oldSource) = event
        dut.io.read.valid.poke((pcValid != 0).B)
        dut.io.read.bits.set.poke(set.U); dut.io.read.bits.tag.poke(tag.U)
        dut.clock.step(); dut.io.read.valid.poke(false.B); dut.clock.step()
        dut.io.commit.valid.poke((pcValid != 0).B)
        dut.io.commit.bits.source.poke(banksById(requestSource))
        dut.io.commit.bits.signature.poke(sig.U)
        dut.io.commit.bits.dead.poke((prediction != 0).B)
        dut.io.commit.bits.hintHit.poke(false.B)
        val training = sampled != 0 && oldValid != 0 && (hit != 0 || evicted != 0)
        dut.io.writeEnable.expect((sampled != 0).B)
        dut.io.hit.expect((hit != 0).B)
        dut.io.training.valid.expect(training.B)
        if (sampled != 0 && (hit != 0 || evicted != 0)) dut.io.previousDead.expect((oldDead != 0).B)
        if (training) {
          dut.io.training.bits.dead.expect((hit == 0).B)
          dut.io.training.bits.source.expect(banksById(oldSource))
          for (table <- 0 until 3) dut.io.training.bits.indices(table).expect(index(oldSig, table, 4096).U)
        }
        dut.clock.step(); dut.io.commit.valid.poke(false.B)
      }
    }
  }

  behavior of "masked true LRU"
  it should "select the oldest free way when global LRU is occupied" in {
    simulate(new MaskedLRUHarness(8)) { dut =>
      val random = new Random(71)
      val ages = Array.tabulate(8)(i => -i)
      dut.io.touch.valid.poke(false.B)
      dut.reset.poke(true.B); dut.clock.step(2); dut.reset.poke(false.B)
      for (step <- 0 until 300) {
        val mask = random.nextInt(255) + 1
        val expected = (0 until 8).filter(i => (mask & (1 << i)) != 0).minBy(ages)
        dut.io.mask.poke(mask.U)
        dut.io.victim.expect((1 << expected).U)
        val way = random.nextInt(8)
        dut.io.touch.valid.poke(true.B); dut.io.touch.bits.poke(way.U)
        dut.clock.step(); ages(way) = step + 1
      }
    }
  }

  behavior of "SCArray"
  it should "snapshot before same-cycle training and preserve independent colliding updates" in {
    val params = SDBPParams(predictorEntries = 64)
    val golden = Array.fill(sdbpSources.size, 3, 64)(0)
    val random = new Random(108)
    simulate(new SCArray(params)) { dut =>
      dut.io.query.valid.poke(false.B); dut.io.training.valid.poke(false.B)
      dut.reset.poke(true.B); dut.clock.step(2); dut.reset.poke(false.B)
      for (cycle <- 0 until 1800) {
        // Directed saturation/decrement/shift, then dense random collisions.
        val query = cycle < 24 || random.nextInt(6) != 0
        val training = cycle < 24 || random.nextInt(5) != 0
        val indices = (0 until 3).map(t => if (cycle < 24 || random.nextBoolean()) t else random.nextInt(64))
        val trainIndices = (0 until 3).map(t => if (cycle < 24 || random.nextBoolean()) t else random.nextInt(64))
        val dead = if (cycle < 24) cycle < 8 || cycle >= 16 else random.nextInt(3) != 0
        val querySource = if (cycle < 24) 0 else random.nextInt(sdbpSources.size)
        val trainingSource = if (cycle < 24) 0 else random.nextInt(sdbpSources.size)
        dut.io.query.valid.poke(query.B)
        dut.io.query.bits.source.poke(sdbpSources(querySource))
        dut.io.training.valid.poke(training.B)
        dut.io.training.bits.dead.poke(dead.B)
        dut.io.training.bits.source.poke(sdbpSources(trainingSource))
        for (table <- 0 until 3) {
          dut.io.query.bits.indices(table).poke(indices(table).U)
          dut.io.training.bits.indices(table).poke(trainIndices(table).U)
        }
        val expected = (0 until 3).map(t => golden(querySource)(t)(indices(t)))
        dut.clock.step()
        dut.io.response.valid.expect(query.B)
        if (query) for (table <- 0 until 3) dut.io.response.bits(table).expect(expected(table).U)
        if (training) for (table <- 0 until 3) {
          val old = golden(trainingSource)(table)(trainIndices(table))
          golden(trainingSource)(table)(trainIndices(table)) = if (dead) math.min(3, old + 1)
            else if ((table & 1) != 0) old >> 1 else math.max(0, old - 1)
        }
      }
      // Read every trained entry after the last update, then reset and recheck.
      dut.io.training.valid.poke(false.B); dut.io.query.valid.poke(true.B)
      for (source <- sdbpSources.indices; entry <- 0 until 64) {
        dut.io.query.bits.source.poke(sdbpSources(source))
        dut.io.query.bits.indices.foreach(_.poke(entry.U)); dut.clock.step()
        for (table <- 0 until 3) dut.io.response.bits(table).expect(golden(source)(table)(entry).U)
      }
      dut.io.query.valid.poke(false.B)
      dut.reset.poke(true.B); dut.clock.step(2); dut.reset.poke(false.B)
      dut.io.response.valid.expect(false.B); dut.io.query.valid.poke(true.B)
      for (source <- sdbpSources; entry <- 0 until 64) {
        dut.io.query.bits.source.poke(source)
        dut.io.query.bits.indices.foreach(_.poke(entry.U)); dut.clock.step()
        dut.io.response.bits.foreach(_.expect(0.U))
      }
    }
  }

  behavior of "L1I residency tracker"
  it should "track virtual aliases, replacement, simultaneous invalidation and fence.i" in {
    val geometry = L1ICopyParams(256, 4)
    simulate(new L1ICopyTracker(geometry, 2)) { dut =>
      dut.io.update.write.valid.poke(false.B)
      dut.io.update.flushAll.poke(false.B)
      dut.io.update.invalidate.foreach(_.valid.poke(false.B))
      dut.reset.poke(true.B); dut.clock.step(2); dut.reset.poke(false.B)
      val block = BigInt("123456a5", 16)
      val other = block + 64
      val set = (3 << 6) | (block.toInt & 63)
      dut.io.query(0).poke(block.U); dut.io.query(1).poke(other.U)
      dut.io.present(0).expect(false.B)
      dut.io.update.write.valid.poke(true.B)
      dut.io.update.write.bits.set.poke(set.U)
      dut.io.update.write.bits.wayMask.poke(2.U)
      dut.io.update.write.bits.block.poke(block.U)
      dut.io.present(0).expect(true.B); dut.clock.step()
      dut.io.update.write.valid.poke(false.B)
      dut.io.present(0).expect(true.B)
      dut.io.update.write.valid.poke(true.B)
      dut.io.update.write.bits.block.poke(other.U)
      dut.io.present(0).expect(false.B); dut.io.present(1).expect(true.B)
      dut.clock.step(); dut.io.update.write.valid.poke(false.B)
      dut.io.update.invalidate(0).valid.poke(true.B)
      dut.io.update.invalidate(0).bits.set.poke(set.U)
      dut.io.update.invalidate(0).bits.wayMask.poke(2.U)
      dut.io.present(1).expect(false.B); dut.clock.step()
      dut.io.update.invalidate(0).valid.poke(false.B)
      dut.io.update.write.valid.poke(true.B)
      dut.io.update.write.bits.block.poke(block.U)
      dut.clock.step(); dut.io.update.write.valid.poke(false.B)
      dut.io.update.flushAll.poke(true.B)
      dut.io.present(0).expect(false.B); dut.clock.step()
      dut.io.update.flushAll.poke(false.B)
      dut.io.present(0).expect(false.B)
    }
  }
}
