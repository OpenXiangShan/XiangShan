// Copyright 2016-2017 SiFive, Inc.
// See rocket-chip/LICENSE.SiFive for license details (Apache-2.0).
// Adapted from TLArbiter: XiangShan groups the priority and payload networks
// for MissQueue acquires while retaining one global multibeat owner.

package xiangshan.cache

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink.{TLArbiter, TLChannel, TLEdge}

/** Lowest-index-first acquire arbitration with no added pipeline stage.
  *
  * Groups must be contiguous in source-priority order. Group/local arbitration
  * and payload selection are combinational; only the global owner and remaining
  * beat count are registered. Independent arbiters per group would allow a
  * different group to preempt an in-flight multibeat transaction.
  */
object DCacheAcquireArbiter {
  def apply[T <: TLChannel](
    edge: TLEdge,
    sink: DecoupledIO[T],
    groupSizes: Seq[Int],
    sources: DecoupledIO[T]*
  ): Unit = {
    require(sources.nonEmpty, "DCacheAcquireArbiter requires at least one source")
    require(groupSizes.nonEmpty && groupSizes.forall(_ > 0))
    require(groupSizes.sum == sources.size)

    if (sources.size == 1) {
      sink :<>= sources.head
    } else {
      val sourcesIn = sources.toList
      val beatsIn = sourcesIn.map(s => edge.numBeats1(s.bits))

      val beatsLeft = RegInit(0.U)
      val idle = beatsLeft === 0.U
      val latch = idle && sink.ready
      val valids = sourcesIn.map(_.valid)
      val readys = VecInit(hierarchicalLowestReadys(valids, groupSizes, latch))
      val winner = VecInit((readys zip valids).map { case (r, v) => r && v })

      require(readys.size == valids.size)
      val prefixOR = winner.scanLeft(false.B)(_ || _).init
      assert((prefixOR zip winner).map { case (p, w) => !p || !w }.reduce(_ && _))
      assert(!valids.reduce(_ || _) || winner.reduce(_ || _))

      // Capture beats-minus-one on the first accepted beat. Subsequent beats
      // count down only on fire, so source bubbles and sink stalls retain owner.
      val maskedBeats = (winner zip beatsIn).map { case (w, b) => Mux(w, b, 0.U) }
      val initBeats = maskedBeats.reduce(_ | _)
      beatsLeft := Mux(latch, initBeats, beatsLeft - sink.fire)

      val state = RegInit(VecInit(Seq.fill(sources.size)(false.B)))
      val muxState = Mux(idle, winner, state)
      state := muxState

      val allowed = Mux(idle, readys, state)
      (sourcesIn zip allowed).foreach { case (s, r) =>
        s.ready := sink.ready && r
      }
      sink.valid := Mux(idle, valids.reduce(_ || _), Mux1H(state, valids))
      val muxStateSeq = (0 until sourcesIn.size).map(muxState(_))
      val selectedBits = groupedMux1H(muxStateSeq, sourcesIn.map(_.bits), groupSizes)
      sink.bits :<= selectedBits
    }
  }

  private def hierarchicalLowestReadys(
    valids: Seq[Bool],
    groupSizes: Seq[Int],
    latch: Bool
  ): Seq[Bool] = {
    val groupValid = Wire(Vec(groupSizes.size, Bool()))
    groupValid.suggestName("groupValid")
    var offset = 0
    groupSizes.zipWithIndex.foreach { case (size, groupIdx) =>
      val groupValids = valids.slice(offset, offset + size)
      groupValid(groupIdx) := groupValids.reduce(_ || _)
      offset += size
    }

    val groupReady = Wire(Vec(groupSizes.size, Bool()))
    groupReady.suggestName("groupReady")
    groupReady := TLArbiter.lowestIndexFirst(
      groupSizes.size, Cat(groupValid.toSeq.reverse), latch).asBools

    offset = 0
    groupSizes.zipWithIndex.flatMap { case (size, groupIdx) =>
      val groupValids = valids.slice(offset, offset + size)
      val localReady = Wire(Vec(size, Bool()))
      localReady.suggestName(s"localReady_$groupIdx")
      val localPolicy = TLArbiter.lowestIndexFirst(size, Cat(groupValids.reverse), latch).asBools
      localReady := VecInit(localPolicy.map(_ && groupReady(groupIdx)))
      offset += size
      localReady.toSeq
    }
  }

  private def groupedMux1H[T <: Data](
    select: Seq[Bool],
    values: Seq[T],
    groupSizes: Seq[Int]
  ): T = {
    var offset = 0
    val groupSelects = groupSizes.map { size =>
      val group = select.slice(offset, offset + size)
      offset += size
      group.reduce(_ || _)
    }

    offset = 0
    val groupValues = groupSizes.map { size =>
      val groupSelect = select.slice(offset, offset + size)
      val groupValue = values.slice(offset, offset + size)
      offset += size
      Mux1H(groupSelect, groupValue)
    }

    Mux1H(groupSelects, groupValues)
  }
}
