package cache

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.flatspec.AnyFlatSpec
import xiangshan.cache.PDBLateTracker

class PDBLateTrackerTest extends AnyFlatSpec with ChiselSim {
  behavior of "PDB MSHR late lifetime tracking"

  it should "count the first accepted demand once and each terminal PF against a demand owner" in {
    simulate(new PDBLateTracker(4)) { c =>
      for (i <- 0 until 4) {
        c.io.allocate(i).valid.poke(false.B)
        c.io.allocate(i).bits.stream.poke(false.B); c.io.allocate(i).bits.demand.poke(false.B)
        c.io.live(i).poke(false.B); c.io.demandHit(i).poke(false.B); c.io.streamHit(i).poke(false.B)
      }
      c.reset.poke(true.B); c.clock.step(2); c.reset.poke(false.B)
      c.io.allocate(0).valid.poke(true.B); c.io.allocate(0).bits.stream.poke(true.B)
      c.clock.step(); c.io.allocate(0).valid.poke(false.B); c.io.live(0).poke(true.B)
      c.io.streamHit(0).poke(true.B); c.io.prefetchLate.expect(0.U)
      c.clock.step(); c.io.streamHit(0).poke(false.B)
      c.io.demandLate.expect(0.U)
      c.io.demandHit(0).poke(true.B); c.io.demandLate.expect(1.U)
      c.clock.step(); c.io.demandLate.expect(0.U)
      c.clock.step(); c.io.demandHit(0).poke(false.B)
      c.io.streamHit(0).poke(true.B); c.io.prefetchLate.expect(1.U)
      c.clock.step(); c.io.prefetchLate.expect(1.U)
      c.io.streamHit(0).poke(false.B)
      // A new allocation in the same slot must not inherit demand ownership.
      c.io.live(0).poke(false.B); c.io.allocate(0).valid.poke(true.B)
      c.clock.step(); c.io.allocate(0).valid.poke(false.B); c.io.live(0).poke(true.B)
      c.io.streamHit(0).poke(true.B); c.io.prefetchLate.expect(0.U)
      c.io.streamHit(0).poke(false.B)
      // Two independent accepted demand merges may both be late on one edge.
      c.io.allocate(1).valid.poke(true.B); c.io.allocate(1).bits.stream.poke(true.B)
      c.clock.step(); c.io.allocate(1).valid.poke(false.B); c.io.live(1).poke(true.B)
      c.io.demandHit(0).poke(true.B); c.io.demandHit(1).poke(true.B)
      c.io.demandLate.expect(2.U); c.clock.step(); c.io.demandLate.expect(0.U)
      c.io.demandHit(0).poke(false.B); c.io.demandHit(1).poke(false.B)
      // Compressed demand in the original Stream allocation cycle is late.
      c.io.allocate(2).valid.poke(true.B); c.io.allocate(2).bits.stream.poke(true.B)
      c.io.demandHit(2).poke(true.B); c.io.demandLate.expect(1.U)
      c.clock.step(); c.io.allocate(2).valid.poke(false.B); c.io.live(2).poke(true.B)
      c.io.demandLate.expect(0.U); c.io.demandHit(2).poke(false.B)
      // An unrelated prefetch source cannot produce Stream demand lateness.
      c.io.allocate(3).valid.poke(true.B)
      c.clock.step(); c.io.allocate(3).valid.poke(false.B); c.io.live(3).poke(true.B)
      c.io.demandHit(3).poke(true.B); c.io.demandLate.expect(0.U)
      c.clock.step(); c.io.demandHit(3).poke(false.B)
      c.io.streamHit(3).poke(true.B); c.io.prefetchLate.expect(1.U)
      c.io.live(3).poke(false.B); c.io.prefetchLate.expect(0.U)
    }
  }
}
