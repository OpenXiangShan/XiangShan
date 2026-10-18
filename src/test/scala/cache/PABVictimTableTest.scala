package cache

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.flatspec.AnyFlatSpec
import xiangshan.cache.PABVictimTable

class PABVictimTableTest extends AnyFlatSpec with ChiselSim {
  case class Victim(address: Int, used: Boolean, source: Int)
  behavior of "Persistent PAB victim history"
  for (entries <- Seq(1, 8, 256)) {
    it should s"match a FIFO reference for $entries records including duplicates, holes and simultaneous demands" in {
      simulate(new PABVictimTable(entries, 20)) { c =>
        c.io.victim.valid.poke(false.B); c.io.victim.bits.blockAddr.poke(0.U)
        c.io.victim.bits.source.poke(3.U); c.io.victim.bits.used.poke(false.B)
        c.io.demand.foreach { d => d.valid.poke(false.B); d.bits.poke(0.U) }
        c.reset.poke(true.B); c.clock.step(3); c.reset.poke(false.B)
        var fifo = Vector.empty[Victim]
        var pending = (0, 0, false, false, 0)
        val rng = new scala.util.Random(91 + entries)
        def tick(victim: Option[Victim], demand: Seq[Int]): Unit = {
          val hits = fifo.filter(v => demand.contains(v.address))
          val duplicate = victim.exists(v => fifo.exists(_.address == v.address))
          fifo = fifo.filterNot(v => demand.contains(v.address))
          var full = false
          victim.foreach { v =>
            fifo = fifo.filterNot(_.address == v.address)
            if (fifo.size == entries) { full = true; fifo = fifo.tail }
            fifo :+= v
          }
          val next = (hits.count(v => v.source == 3 && v.used),
            hits.count(v => v.source == 3 && !v.used), duplicate, full, fifo.size)
          c.io.victim.valid.poke(victim.nonEmpty.B)
          victim.foreach { v =>
            c.io.victim.bits.blockAddr.poke(v.address.U); c.io.victim.bits.used.poke(v.used.B)
            c.io.victim.bits.source.poke(v.source.U)
          }
          for (i <- 0 until 3) {
            c.io.demand(i).valid.poke((i < demand.size).B)
            c.io.demand(i).bits.poke(demand.lift(i).getOrElse(0).U)
          }
          c.clock.step()
          c.io.streamUsedHits.expect(pending._1.U); c.io.streamUnusedHits.expect(pending._2.U)
          c.io.duplicate.expect(pending._3.B); c.io.fullEviction.expect(pending._4.B)
          c.io.occupancy.expect(pending._5.U)
          pending = next
        }
        // Every slot and source class, plus idle time longer than a controller window.
        for (a <- 0 until entries) tick(Some(Victim(a, a % 2 == 0, if (a % 3 == 0) 2 else 3)), Nil)
        for (_ <- 0 until 550) tick(None, Nil)
        for (a <- (0 until entries).grouped(3)) tick(None, a)
        for (_ <- 0 until 800) {
          val victim = if (rng.nextBoolean()) Some(Victim(rng.nextInt(entries + 5), rng.nextBoolean(),
            if (rng.nextBoolean()) 3 else 2)) else None
          val demand = Seq.fill(rng.nextInt(4))(rng.nextInt(entries + 5))
          tick(victim, demand)
        }
        // Demand cannot consume a freshly inserted copy in its own batch.
        tick(Some(Victim(999, false, 3)), Seq(999, 999, 999))
        tick(None, Seq(999, 999, 999)); tick(None, Seq(999)); tick(None, Nil); tick(None, Nil)
      }
    }
  }
}
