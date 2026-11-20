package xiangshan

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class SpecMissABufferTest extends AnyFlatSpec with ChiselSim {
  behavior of "SpecMissABuffer resolution and FIFO ordering"

  private case class Record(data: BigInt, speculative: Boolean, committed: Boolean, owner: Int, id: Int)
  private case class Decision(id: Int, commit: Boolean)

  private class Driver(c: SpecMissABuffer[UInt], depth: Int, nOwners: Int) {
    var records = Vector.empty[Record]

    def tick(
      enq: Option[Record] = None,
      decisions: Map[Int, Decision] = Map.empty,
      ready: Boolean = false
    ): Unit = {
      val incoming = enq.getOrElse(Record(0, false, true, 0, 0))
      c.io.enq.valid.poke(enq.nonEmpty.B)
      c.io.enq.bits.data.poke(incoming.data.U)
      c.io.enq.bits.speculative.poke(incoming.speculative.B)
      c.io.enq.bits.committed.poke(incoming.committed.B)
      c.io.enq.bits.owner.poke(incoming.owner.U)
      c.io.enq.bits.id.poke(incoming.id.U)
      c.io.deq.ready.poke(ready.B)
      for (w <- 0 until nOwners) {
        val decision = decisions.getOrElse(w, Decision(0, false))
        c.io.resolve(w).valid.poke(decisions.contains(w).B)
        c.io.resolve(w).bits.id.poke(decision.id.U)
        c.io.resolve(w).bits.commit.poke(decision.commit.B)
      }

      def resolution(record: Record): Option[Decision] = {
        if (!record.speculative || record.committed) None
        else decisions.get(record.owner).filter(_.id == record.id)
      }

      val enqReady = records.size < depth
      val deqValid = records.headOption.exists(r =>
        !r.speculative || r.committed || resolution(r).exists(_.commit))
      val headPending = records.headOption.exists(r => resolution(r).nonEmpty)
      val headPresent = records.headOption.exists(r => !r.speculative || r.committed) || headPending
      c.io.enq.ready.expect(enqReady.B)
      c.io.preview.present.expect(headPresent.B)
      c.io.preview.pending.expect(headPending.B)
      c.io.deq.valid.expect(deqValid.B)
      if (headPresent) {
        c.io.deq.bits.expect(records.head.data.U)
        c.io.deq_speculative.expect(records.head.speculative.B)
      }
      for (w <- 0 until nOwners) {
        val matched = records.exists(r => r.owner == w && resolution(r).nonEmpty)
        c.io.matched(w).expect(matched.B)
        c.io.committed(w).expect((matched && decisions.get(w).exists(_.commit)).B)
      }

      val remaining = records.zipWithIndex.flatMap { case (record, i) =>
        val decision = resolution(record)
        if ((i == 0 && deqValid && ready) || decision.exists(d => !d.commit)) None
        else Some(record.copy(committed = record.committed || decision.exists(_.commit)))
      }
      val next = remaining ++ enq.filter(_ => enqReady)
      c.clock.step()
      records = next
    }
  }

  for ((depth, owners) <- Seq((2, 3), (3, 3), (2, 1))) {
    it should s"preserve commits, cancellations and backpressure at depth=$depth owners=$owners" in {
      simulate(new SpecMissABuffer(UInt(119.W), depth, owners, 4)) { c =>
        val d = new Driver(c, depth, owners)
        def spec(data: Int, owner: Int, id: Int): Record = Record(data, true, false, owner, id)
        def normal(data: Int): Record = Record(data, false, true, 0, 0)
        val last = owners - 1

        d.tick()
        d.tick(enq = Some(spec(11, 0, 1)))
        d.tick(decisions = Map(0 -> Decision(2, true)), ready = true) // Wrong ID cannot release the head.
        d.tick(enq = Some(spec(12, last, 2)))
        d.tick(decisions = Map(0 -> Decision(1, true))) // Commit must persist under backpressure.
        d.tick(decisions = Map(0 -> Decision(1, false))) // Committed records ignore later resolutions.
        d.tick(enq = Some(normal(13)), decisions = Map(last -> Decision(2, false)), ready = true)
        while (d.records.nonEmpty) d.tick(ready = true)

        d.tick(enq = Some(normal(14)))
        d.tick(enq = Some(spec(15, last, 4)), ready = true) // Simultaneous dequeue and enqueue.
        d.tick(decisions = Map(last -> Decision(4, true)), ready = true) // Same-cycle commit bypass.
        d.tick(enq = Some(spec(16, 0, 5)))
        d.tick(enq = Some(normal(17)))
        d.tick(decisions = Map(0 -> Decision(5, false)), ready = true) // Cancel head, preserve follower.
        d.tick(ready = true)

        if (owners > 1) {
          d.tick(enq = Some(spec(18, 0, 7)))
          d.tick(enq = Some(spec(19, last, 8)))
          d.tick(decisions = Map(0 -> Decision(7, true), last -> Decision(8, false)), ready = true)
        }
        for (i <- 0 until depth) d.tick(enq = Some(normal(20 + i)))
        d.tick(enq = Some(normal(30)), ready = true) // A full buffer retains its old-count ready policy.
        while (d.records.nonEmpty) d.tick(ready = true)

        val random = new Random(0x5a17L + depth * 10 + owners)
        for (_ <- 0 until 1000) {
          val decisions = (0 until owners).flatMap { owner =>
            if (!random.nextBoolean()) None
            else {
              val waiting = d.records.filter(r => r.speculative && !r.committed && r.owner == owner)
              val id = if (waiting.nonEmpty && random.nextInt(4) != 0) {
                waiting(random.nextInt(waiting.size)).id
              } else random.nextInt(16)
              Some(owner -> Decision(id, random.nextBoolean()))
            }
          }.toMap
          val incoming = if (random.nextInt(4) == 0) None else {
            val owner = random.nextInt(owners)
            val freeIds = (0 until 16).filterNot(id =>
              d.records.exists(r => r.speculative && r.owner == owner && r.id == id))
            val speculative = random.nextInt(4) != 0
            Some(Record(BigInt(119, random), speculative, !speculative, owner,
              freeIds(random.nextInt(freeIds.size))))
          }
          d.tick(incoming, decisions, random.nextBoolean())
        }
        while (d.records.nonEmpty) {
          val decisions = (0 until owners).flatMap { owner =>
            d.records.find(r => r.speculative && !r.committed && r.owner == owner)
              .map(r => owner -> Decision(r.id, true))
          }.toMap
          d.tick(decisions = decisions, ready = true)
        }
        d.tick(ready = true)
      }
    }
  }
}
