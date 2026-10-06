package cache

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.flatspec.AnyFlatSpec
import xiangshan.cache.PDBVictimObserver

class PDBVictimObserverTest extends AnyFlatSpec with ChiselSim {
  case class Victim(address: BigInt, used: Boolean, stream: Boolean = true)
  case class Batch(victim: Option[Victim] = None, demand: Seq[Option[BigInt]] = Seq.fill(3)(None))
  case class Result(batch: Batch, used: Int = 0, unused: Int = 0,
                    streamUsed: Int = 0, streamUnused: Int = 0,
                    duplicate: Boolean = false, eviction: Option[Victim] = None, occupancy: Int = 0)

  // Logical reference: a list of live victims, with no physical slots/pipeline.
  class Model(entries: Int) {
    var fifo = Vector.empty[Victim]
    def step(batch: Batch): Result = {
      val addresses = batch.demand.flatten.toSet
      val hits = fifo.filter(v => addresses(v.address))
      val duplicate = batch.victim.exists(v => fifo.exists(_.address == v.address))
      fifo = fifo.filterNot(v => addresses(v.address))
      var eviction = Option.empty[Victim]
      batch.victim.foreach { v =>
        fifo = fifo.filterNot(_.address == v.address)
        if (fifo.size == entries) { eviction = Some(fifo.head); fifo = fifo.tail }
        fifo :+= v
      }
      Result(batch, hits.count(_.used), hits.count(v => !v.used),
        hits.count(v => v.used && v.stream), hits.count(v => !v.used && v.stream),
        duplicate, eviction, fifo.size)
    }
  }

  class Driver(c: PDBVictimObserver, entries: Int) {
    val model = new Model(entries)
    var pending = Result(Batch())
    var cycles = 0
    def poke(batch: Batch): Unit = {
      c.io.victim.valid.poke(batch.victim.nonEmpty.B)
      val v = batch.victim.getOrElse(Victim(0, false, false))
      c.io.victim.bits.blockAddr.poke(v.address.U)
      c.io.victim.bits.used.poke(v.used.B)
      c.io.victim.bits.stream.poke(v.stream.B)
      c.io.demand.zip(batch.demand).foreach { case (q, a) =>
        q.valid.poke(a.nonEmpty.B); q.bits.poke(a.getOrElse(BigInt(0)).U)
      }
    }
    poke(Batch()); c.reset.poke(true.B); c.clock.step(2); c.reset.poke(false.B)
    def check(expected: Result): Unit = {
      val r = c.io.result
      withClue(s"cycle $cycles, expected $expected: ") {
        r.usedHits.expect(expected.used.U); r.unusedHits.expect(expected.unused.U)
        r.streamUsedHits.expect(expected.streamUsed.U); r.streamUnusedHits.expect(expected.streamUnused.U)
        r.duplicate.expect(expected.duplicate.B); r.occupancy.expect(expected.occupancy.U)
        r.eviction.valid.expect(expected.eviction.nonEmpty.B)
        expected.eviction.foreach { v =>
          r.eviction.bits.blockAddr.expect(v.address.U)
          r.eviction.bits.used.expect(v.used.B); r.eviction.bits.stream.expect(v.stream.B)
        }
        r.victim.valid.expect(expected.batch.victim.nonEmpty.B)
        expected.batch.victim.foreach { v =>
          r.victim.bits.blockAddr.expect(v.address.U)
          r.victim.bits.used.expect(v.used.B); r.victim.bits.stream.expect(v.stream.B)
        }
        r.demand.zip(expected.batch.demand).foreach { case (q, a) =>
          q.valid.expect(a.nonEmpty.B); a.foreach(x => q.bits.expect(x.U))
        }
      }
    }
    def tick(batch: Batch = Batch()): Result = {
      val next = model.step(batch)
      poke(batch); c.clock.step(); cycles += 1
      check(pending); pending = next
      next
    }
    def victim(a: Int, used: Boolean = false, stream: Boolean = true): Result =
      tick(Batch(Some(Victim(a, used, stream))))
    def query(addresses: Int*): Result =
      tick(Batch(demand = addresses.map(a => Some(BigInt(a))).padTo(3, None)))
    def drain(): Unit = { tick(); tick() }
  }

  behavior of "PDB victim FIFO and ordered three-lane observation"

  it should "reuse holes, refresh duplicates and evict only the oldest remaining victim" in {
    simulate(new PDBVictimObserver(4, 42, bankEntries = 2)) { c =>
      val d = new Driver(c, 4)
      for (a <- 1 to 4) d.victim(a, used = a % 2 == 0)
      assert(d.query(2).used == 1)
      assert(d.victim(5).eviction.isEmpty) // Hole, so the oldest live entry survives.
      assert(d.victim(3, used = true, stream = false).duplicate)
      val r = d.victim(6)
      assert(r.eviction.exists(_.address == 1))
      val hits = d.query(3, 4, 5)
      assert(hits.used == 2 && hits.unused == 1 && hits.streamUsed == 1)
      assert(d.query(3, 3, 3).used == 0)
      d.drain()
    }
  }

  it should "order early queries, same-cycle replacement and repeated in-flight queries without ABA" in {
    simulate(new PDBVictimObserver(1, 42)) { c =>
      val d = new Driver(c, 1)
      d.query(10) // Must not match the next cycle's insertion.
      d.victim(10, used = false)
      val a = d.tick(Batch(Some(Victim(10, true)), Seq.fill(3)(Some(BigInt(10)))))
      assert(a.unused == 1 && a.used == 0 && a.occupancy == 1)
      assert(d.query(10, 10, 10).used == 1) // Newly inserted lifetime, once.
      assert(d.query(10).used == 0)
      d.victim(11); d.victim(12, used = true); d.query(11, 12)
      d.tick(Batch(Some(Victim(13, false)), Seq(Some(BigInt(13)), None, None)))
      assert(d.query(13).unused == 1) // Same-cycle query did not delete new entry.
      d.drain()
    }
  }

  it should "preserve all entries across idle time and accept three hits each cycle across all 256 slots" in {
    simulate(new PDBVictimObserver(256, 42)) { c =>
      val d = new Driver(c, 256)
      for (a <- 0 until 256) d.victim(a, used = a % 2 == 0)
      for (_ <- 0 until 4200) d.tick()
      for (group <- (0 until 255).grouped(3)) {
        val r = d.query(group: _*)
        assert(r.used + r.unused == 3)
      }
      assert(d.query(255, 255, 255).unused == 1)
      d.drain()
    }
  }

  for (entries <- Seq(1, 7, 64, 256)) {
    it should s"match the FIFO reference with concurrent random insertion and three-lane traffic at $entries entries" in {
      simulate(new PDBVictimObserver(entries, 42, bankEntries = 32)) { c =>
        val d = new Driver(c, entries)
        val rng = new scala.util.Random(782 + entries)
        val high = BigInt(1) << 39
        for (a <- 0 until entries * 3) {
          d.tick(Batch(Some(Victim(high + a, rng.nextBoolean(), rng.nextBoolean()))))
        }
        for (_ <- 0 until 2400) {
          def address(): BigInt = if (d.model.fifo.nonEmpty && rng.nextInt(4) == 0)
            d.model.fifo(rng.nextInt(d.model.fifo.size)).address else high + rng.nextInt(entries * 3 + 5)
          val v = if (rng.nextInt(5) != 0) Some(Victim(address(), rng.nextBoolean(), rng.nextBoolean())) else None
          val q = Seq.fill(3)(if (rng.nextInt(5) != 0) Some(address()) else None)
          d.tick(Batch(v, q))
        }
        d.model.fifo.map(_.address).grouped(3).foreach { group =>
          d.tick(Batch(demand = group.map(Some(_)).padTo(3, None)))
        }
        d.drain()
      }
    }
  }
}
