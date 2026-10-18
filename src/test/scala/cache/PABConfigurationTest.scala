package cache

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.flatspec.AnyFlatSpec
import top._
import xiangshan.XSTileKey
import xiangshan.cache.PABMissTracker
import xiangshan.mem.prefetch._

class PABConfigurationTest extends AnyFlatSpec {
  behavior of "PAB experiment configuration"
  it should "change only its own key and preserve every core and cache configuration" in {
    val dynamic = new DefaultConfig(2)
    val fixed = new PABFixedDepthConfig(2)
    val disabled = new WithPABDepth(PABDepthParameters()) ++ new DefaultConfig(2)
    assert(dynamic(XSTileKey) == fixed(XSTileKey))
    assert(dynamic(XSTileKey) == disabled(XSTileKey))
    assert(dynamic(PABDepthKey) == PABDepthParameters(enabled = true))
    assert(fixed(PABDepthKey) == PABDepthParameters(enabled = true, autoDepth = false))
    assert(!disabled(PABDepthKey).enabled)
    for (core <- dynamic(XSTileKey)) {
      assert(core.dcacheParametersOpt.get.nWays == 4)
      assert(core.dcacheParametersOpt.get.nSets * core.dcacheParametersOpt.get.blockBytes == 16384)
    }
  }
  it should "reject unsupported capacity, depth and policy parameters" in {
    intercept[IllegalArgumentException] { PABDepthParameters(entries = 0) }
    intercept[IllegalArgumentException] { PABDepthParameters(victimEntries = 0) }
    intercept[IllegalArgumentException] { PABDepthParameters(initialDepth = 12) }
    intercept[IllegalArgumentException] { PABDepthParameters(fixedDepth = 0) }
    intercept[IllegalArgumentException] { PABDepthParameters(lateLow = 40, lateHigh = 40) }
    intercept[IllegalArgumentException] { PABDepthParameters(unusedLow = 80, unusedHigh = 40) }
    intercept[IllegalArgumentException] { PABDepthParameters(windowInserts = 0) }
    intercept[IllegalArgumentException] { PABDepthParameters(creditHalfUnits = 0) }
  }
}

class PABMissTrackerTest extends AnyFlatSpec with ChiselSim {
  behavior of "PAB observation lifetime reset"
  it should "reset consumption on slot reuse without resurrecting a retired lifetime" in {
    simulate(new PABMissTracker(2, 20)) { c =>
      c.io.active.foreach(_.poke(false.B)); c.io.demand.foreach(_.poke(false.B))
      c.io.allocationPrefetch.foreach(_.poke(true.B))
      c.io.allocation.foreach { a =>
        a.valid.poke(false.B); a.bits.blockAddr.poke(0.U); a.bits.source.poke(3.U); a.bits.eligible.poke(true.B)
      }
      c.io.streamAttempt.valid.poke(false.B); c.io.streamAttempt.bits.poke(0.U)
      c.reset.poke(true.B); c.clock.step(3); c.reset.poke(false.B)
      c.io.allocation(0).valid.poke(true.B); c.io.allocation(0).bits.blockAddr.poke(100.U)
      c.io.demand(0).poke(true.B); c.io.late.expect(1.U); c.clock.step()
      c.io.allocation(0).valid.poke(false.B); c.io.active(0).poke(true.B)
      c.io.late.expect(0.U); c.io.origin(0).eligible.expect(false.B)
      c.io.demand(0).poke(false.B); c.io.active(0).poke(false.B)
      c.io.streamAttempt.valid.poke(true.B); c.io.streamAttempt.bits.poke(100.U)
      c.io.late.expect(0.U)
      c.io.streamAttempt.valid.poke(false.B)
      c.io.allocation(0).valid.poke(true.B); c.io.allocation(0).bits.blockAddr.poke(200.U)
      c.clock.step(); c.io.allocation(0).valid.poke(false.B); c.io.active(0).poke(true.B)
      c.io.origin(0).eligible.expect(true.B)
      c.io.demand(0).poke(true.B); c.io.late.expect(1.U); c.clock.step(); c.io.late.expect(0.U)
      c.io.origin(0).blockAddr.expect(200.U)
    }
  }
  it should "distinguish NULL-tagged prefetches from demand before their first demand merge" in {
    simulate(new PABMissTracker(2, 20)) { c =>
      for (eligible <- Seq(false, true)) {
        c.io.active.foreach(_.poke(false.B)); c.io.demand.foreach(_.poke(false.B))
        c.io.allocationPrefetch.foreach(_.poke(true.B))
        c.io.allocation.foreach { a =>
          a.valid.poke(false.B); a.bits.blockAddr.poke(100.U)
          a.bits.source.poke(0.U); a.bits.eligible.poke(eligible.B)
        }
        c.io.streamAttempt.valid.poke(false.B); c.io.streamAttempt.bits.poke(100.U)
        c.reset.poke(true.B); c.clock.step(3); c.reset.poke(false.B)
        c.io.allocation(0).valid.poke(true.B); c.clock.step()
        c.io.allocation(0).valid.poke(false.B); c.io.active(0).poke(true.B)
        c.io.streamAttempt.valid.poke(true.B); c.io.late.expect(0.U)
        c.io.streamAttempt.valid.poke(false.B); c.io.demand(0).poke(true.B)
        c.io.late.expect(0.U); c.clock.step(); c.io.demand(0).poke(false.B)
        c.io.streamAttempt.valid.poke(true.B); c.io.late.expect(1.U)
      }
    }
  }
}
