package cache

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.flatspec.AnyFlatSpec
import xiangshan.cache.DCacheVictimTable

class DCacheVictimTableTest extends AnyFlatSpec with ChiselSim {
  case class Victim(address: Int, source: Int = 3, used: Boolean = false)
  behavior of "Real DCache unused Stream victim history"
  for (size <- Seq(1, 8, 256)) {
    it should s"filter admission, supersede refilled lifetimes and match a FIFO model with $size entries" in {
      simulate(new DCacheVictimTable(size, 20)) { c =>
        c.io.victim.valid.poke(false.B); c.io.victim.bits.blockAddr.poke(0.U)
        c.io.victim.bits.source.poke(0.U); c.io.victim.bits.used.poke(false.B)
        c.io.refill.valid.poke(false.B); c.io.refill.bits.poke(0.U)
        c.io.demand.foreach { d => d.valid.poke(false.B); d.bits.poke(0.U) }
        c.reset.poke(true.B); c.clock.step(3); c.reset.poke(false.B)
        var fifo = Vector.empty[Int]
        def batch(v: Option[Victim] = None, demands: Seq[Int] = Nil, refill: Option[Int] = None): Unit = {
          val hits = fifo.count(a => demands.contains(a) && !refill.contains(a))
          val removed = fifo.exists(a => refill.contains(a))
          val incoming = v.filter(x => x.source == 3 && !x.used && !refill.contains(x.address))
          val duplicate = incoming.exists(x => fifo.contains(x.address))
          fifo = fifo.filterNot(a => demands.contains(a) || refill.contains(a) || incoming.exists(_.address == a))
          val full = incoming.nonEmpty && fifo.size == size
          incoming.foreach { x => if (full) fifo = fifo.tail; fifo :+= x.address }
          c.io.victim.valid.poke(v.nonEmpty.B)
          v.foreach { x => c.io.victim.bits.blockAddr.poke(x.address.U); c.io.victim.bits.source.poke(x.source.U); c.io.victim.bits.used.poke(x.used.B) }
          c.io.refill.valid.poke(refill.nonEmpty.B); c.io.refill.bits.poke(refill.getOrElse(0).U)
          c.io.demand.zipWithIndex.foreach { case (d, i) => d.valid.poke((i < demands.size).B); d.bits.poke(demands.lift(i).getOrElse(0).U) }
          c.clock.step()
          c.io.victim.valid.poke(false.B); c.io.refill.valid.poke(false.B); c.io.demand.foreach(_.valid.poke(false.B))
          c.clock.step()
          c.io.hits.expect(hits.U); c.io.occupancy.expect(fifo.size.U)
          c.io.inserted.expect(incoming.nonEmpty.B); c.io.duplicate.expect(duplicate.B)
          c.io.fullEviction.expect(full.B); c.io.refillRemoval.expect(removed.B)
          c.clock.step(); c.io.hits.expect(0.U)
        }
        // Rejected demand/other-prefetch/used records must not displace useful history.
        batch(Some(Victim(1))); batch(Some(Victim(2, 0))); batch(Some(Victim(3, 2)))
        batch(Some(Victim(4, 3, true))); batch(demands = Seq(1, 1, 1)); batch(demands = Seq(1))
        batch(Some(Victim(5))); batch(refill = Some(5)); batch(demands = Seq(5))
        batch(Some(Victim(6))); batch(demands = Seq(6), refill = Some(6))
        batch(Some(Victim(7)), demands = Seq(7)); batch(demands = Seq(7))
        batch(Some(Victim(8)), refill = Some(8)); batch(demands = Seq(8))
        for (a <- 0 until size + 2) batch(Some(Victim(a + 100)))
        val rng = new scala.util.Random(710 + size)
        for (_ <- 0 until 500) {
          val v = if (rng.nextBoolean()) Some(Victim(rng.nextInt(size + 5), rng.nextInt(4), rng.nextBoolean())) else None
          val ds = Seq.fill(rng.nextInt(4))(rng.nextInt(size + 5))
          batch(v, ds, if (rng.nextInt(4) == 0) Some(rng.nextInt(size + 5)) else None)
        }
      }
    }
  }
}
