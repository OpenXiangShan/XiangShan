package cache

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.flatspec.AnyFlatSpec
import xscache.coupledL2.FdpTaggedPollutionFilter

class FdpTaggedPollutionFilterTest extends AnyFlatSpec with ChiselSim {
  "The FDP tagged pollution filter" should "count only matching, live eviction records" in {
    simulate(new FdpTaggedPollutionFilter(blockBits = 8, hashBits = 2)) { dut =>
      val original = 1.U
      val alias = 4.U // Same XOR index as original, with a different stored tag.

      dut.io.demandMiss.valid.poke(false.B)
      dut.io.fill.valid.poke(false.B)
      dut.io.evictedDemand.valid.poke(true.B)
      dut.io.evictedDemand.bits.poke(original)
      dut.clock.step()
      dut.io.evictedDemand.valid.poke(false.B)

      dut.io.demandMiss.valid.poke(true.B)
      dut.io.demandMiss.bits.poke(alias)
      dut.io.fill.valid.poke(true.B)
      dut.io.fill.bits.poke(alias)
      dut.io.pollutionMiss.expect(false.B)
      dut.io.aliasRejected.expect(true.B)
      dut.clock.step()

      dut.io.fill.valid.poke(false.B)
      dut.io.demandMiss.bits.poke(original)
      dut.io.pollutionMiss.expect(true.B)
      dut.io.aliasRejected.expect(false.B)
      dut.clock.step()
      dut.io.pollutionMiss.expect(false.B)

      dut.io.demandMiss.valid.poke(false.B)
      dut.io.evictedDemand.valid.poke(true.B)
      dut.clock.step()
      dut.io.evictedDemand.valid.poke(false.B)
      dut.io.fill.valid.poke(true.B)
      dut.io.fill.bits.poke(original)
      dut.clock.step()
      dut.io.fill.valid.poke(false.B)
      dut.io.demandMiss.valid.poke(true.B)
      dut.io.pollutionMiss.expect(false.B)

      dut.io.demandMiss.valid.poke(false.B)
      dut.io.evictedDemand.valid.poke(true.B)
      dut.clock.step()
      // A fill retires the old record while evicting a new block into the same slot.
      dut.io.fill.valid.poke(true.B)
      dut.io.evictedDemand.bits.poke(alias)
      dut.clock.step()
      dut.io.fill.valid.poke(false.B)
      dut.io.evictedDemand.valid.poke(false.B)
      dut.io.demandMiss.valid.poke(true.B)
      dut.io.pollutionMiss.expect(false.B)
      dut.io.aliasRejected.expect(true.B)
      dut.io.demandMiss.bits.poke(alias)
      dut.io.pollutionMiss.expect(true.B)
    }
  }
}
