package cache

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.flatspec.AnyFlatSpec
import xscache.coupledL2.FdpPollutionSampler

class FdpPollutionSamplerTest extends AnyFlatSpec with ChiselSim {
  "The FDP pollution sampler" should "isolate sets and retire live records exactly once" in {
    simulate(new FdpPollutionSampler(setBits = 4, tagBits = 12)) { dut =>
      def step(
        miss: Option[(Int, Int)] = None,
        fill: Option[(Int, Int)] = None,
        victim: Option[(Int, Int)] = None,
        sampled: Boolean = false,
        polluted: Boolean = false
      ): Unit = {
        Seq(dut.io.demandMiss -> miss, dut.io.fill -> fill, dut.io.evictedDemand -> victim).foreach {
          case (port, block) =>
            val (set, tag) = block.getOrElse((0, 0))
            port.valid.poke(block.isDefined.B)
            port.bits.set.poke(set.U)
            port.bits.tag.poke(tag.U)
        }
        dut.io.sampledDemandMiss.expect(sampled.B)
        dut.io.pollutionMiss.expect(polluted.B)
        dut.clock.step()
      }

      val same = 0xf
      val complement = 0x3 // Shares its lower set bits with 0xf.
      val unsampled = 0x7
      step(victim = Some(same -> 1))
      step(victim = Some(complement -> 2))
      step(victim = Some(unsampled -> 1))
      step(miss = Some(unsampled -> 1))
      step(miss = Some(same -> 2), sampled = true)
      step(miss = Some(complement -> 1), sampled = true)
      step(miss = Some(same -> 1), sampled = true, polluted = true)
      step(miss = Some(same -> 1), fill = Some(complement -> 2), sampled = true)
      step(miss = Some(complement -> 2), sampled = true)

      for (tag <- 1 to 6) step(victim = Some(same -> tag))
      step(victim = Some(same -> 1)) // Refresh the oldest record to MRU.
      step(victim = Some(same -> 7)) // The full buffer now evicts tag 2.
      step(miss = Some(same -> 2), sampled = true)
      step(miss = Some(same -> 1), sampled = true, polluted = true)
      step(victim = Some(same -> 1))
      step(fill = Some(same -> 7), victim = Some(same -> 8)) // Reuse the way freed by this fill.
      step(miss = Some(same -> 7), sampled = true)
      step(miss = Some(same -> 3), sampled = true, polluted = true)
      step(miss = Some(same -> 8), sampled = true, polluted = true)
    }
  }

  it should "retain the DRRIP sampling rule for odd set index widths" in {
    simulate(new FdpPollutionSampler(setBits = 5, tagBits = 12)) { dut =>
      def isSampled(set: Int): Boolean = {
        val upper = set >> 2
        val lower = set & 7
        upper == lower || upper == (lower ^ 7)
      }
      dut.io.demandMiss.valid.poke(false.B)
      dut.io.fill.valid.poke(false.B)
      dut.io.evictedDemand.valid.poke(true.B)
      for (set <- 0 until 32) {
        dut.io.evictedDemand.bits.set.poke(set.U)
        dut.io.evictedDemand.bits.tag.poke(set.U)
        dut.clock.step()
      }
      dut.io.evictedDemand.valid.poke(false.B)
      dut.io.demandMiss.valid.poke(true.B)
      for (set <- 0 until 32) {
        dut.io.demandMiss.bits.set.poke(set.U)
        dut.io.demandMiss.bits.tag.poke(set.U)
        dut.io.sampledDemandMiss.expect(isSampled(set).B)
        dut.io.pollutionMiss.expect(isSampled(set).B)
        dut.clock.step()
      }
    }
  }
}
