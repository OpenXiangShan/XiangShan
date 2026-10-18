package cache

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.flatspec.AnyFlatSpec
import xiangshan.cache._

class PrefetchAddrBufferTest extends AnyFlatSpec with ChiselSim {
  private def init(c: PrefetchAddrBuffer): Unit = {
    c.io.refill.valid.poke(false.B)
    c.io.refill.bits.blockAddr.poke(0.U); c.io.refill.bits.source.poke(3.U); c.io.refill.bits.used.poke(false.B)
    c.io.retire.valid.poke(false.B); c.io.retire.bits.poke(0.U)
    c.io.demand.foreach { d => d.valid.poke(false.B); d.bits.poke(0.U) }
    c.reset.poke(true.B); c.clock.step(3); c.reset.poke(false.B)
  }
  private def fill(c: PrefetchAddrBuffer, address: Int, source: Int = 3): Unit = {
    c.io.refill.valid.poke(true.B); c.io.refill.bits.blockAddr.poke(address.U)
    c.io.refill.bits.source.poke(source.U); c.clock.step(); c.io.refill.valid.poke(false.B)
  }
  behavior of "Metadata-only prefetch residency"
  it should "model 64-line capacity before physical cache eviction and preserve source and repeated-use LRU" in {
    simulate(new PrefetchAddrBuffer(64, 20)) { c =>
      init(c)
      for (a <- 0 until 64) { fill(c, a, if (a == 1) 2 else 3); c.io.victim.valid.expect(false.B) }
      c.io.occupancy.expect(64.U)
      // All three lanes can access the same virtual block, independently of cache hit/miss.
      c.io.demand.foreach { d => d.valid.poke(true.B); d.bits.poke(0.U) }
      c.clock.step(); c.io.demandHits.expect(3.U)
      c.io.demand.foreach(_.valid.poke(false.B))
      fill(c, 64)
      c.io.victim.valid.expect(true.B); c.io.victim.bits.blockAddr.expect(1.U)
      c.io.victim.bits.source.expect(2.U); c.io.victim.bits.used.expect(false.B)
      // A duplicate refill neither allocates nor refreshes the oldest line.
      fill(c, 2); c.io.inserted.valid.expect(false.B); c.io.victim.valid.expect(false.B)
      fill(c, 65); c.io.victim.bits.blockAddr.expect(2.U)
      c.io.occupancy.expect(64.U)
    }
  }
  it should "correct same-edge victim use and retire Probe or Store lines without capacity victims" in {
    simulate(new PrefetchAddrBuffer(2, 20)) { c =>
      init(c); fill(c, 10); fill(c, 11)
      c.io.demand(0).valid.poke(true.B); c.io.demand(0).bits.poke(10.U)
      fill(c, 12)
      c.io.victim.bits.blockAddr.expect(10.U); c.io.victim.bits.used.expect(true.B)
      c.io.demand(0).valid.poke(false.B)
      c.io.retire.valid.poke(true.B); c.io.retire.bits.poke(11.U)
      fill(c, 13); c.io.victim.valid.expect(false.B); c.io.occupancy.expect(2.U)
      c.io.retire.bits.poke(14.U); fill(c, 14)
      c.io.inserted.valid.expect(false.B)
      c.io.retire.bits.poke(12.U); c.clock.step(); c.io.occupancy.expect(1.U)
    }
  }
  it should "match a software LRU under repeated single-lane uses and duplicate refills" in {
    simulate(new PrefetchAddrBuffer(8, 20)) { c =>
      init(c)
      var lru = Vector.empty[Int]
      var used = Set.empty[Int]
      val rng = new scala.util.Random(1249)
      for (_ <- 0 until 800) {
        val address = rng.nextInt(24)
        val isFill = rng.nextBoolean()
        val inserted = isFill && !lru.contains(address)
        val victim = if (inserted && lru.size == 8) lru.headOption else None
        c.io.refill.valid.poke(isFill.B); c.io.refill.bits.blockAddr.poke(address.U)
        c.io.demand(0).valid.poke((!isFill).B); c.io.demand(0).bits.poke(address.U)
        val victimUsed = victim.exists(a => used(a))
        if (inserted) {
          victim.foreach { a => lru = lru.tail; used -= a }
          lru :+= address
        } else if (!isFill && lru.contains(address)) {
          lru = lru.filterNot(_ == address) :+ address; used += address
        }
        c.clock.step()
        c.io.inserted.valid.expect(inserted.B); c.io.victim.valid.expect(victim.nonEmpty.B)
        victim.foreach { a =>
          c.io.victim.bits.blockAddr.expect(a.U); c.io.victim.bits.used.expect(victimUsed.B)
        }
        c.io.occupancy.expect(lru.size.U)
      }
    }
  }
}
