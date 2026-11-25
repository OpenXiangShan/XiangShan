package xiangshan.mem.prefetch.temporal

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import top.L1LldpTemporalConfig
import utility.{PerfCounterOptions, PerfCounterOptionsKey, XSPerfLevel}
import xiangshan.{XSCoreParamsKey, XSTileKey}

class TemporalL1FeedbackTest extends AnyFlatSpec with ChiselScalatestTester {
  private val base = new L1LldpTemporalConfig()
  private val core = base(XSTileKey).head
  private implicit val config = base.alterPartial {
    case PerfCounterOptionsKey =>
      PerfCounterOptions(enablePerfPrint = false, enablePerfDB = false, perfLevel = XSPerfLevel.VERBOSE, perfDBHartID = 0)
    case XSCoreParamsKey =>
      core.copy(dcacheParametersOpt = core.dcacheParametersOpt.map(_.copy(nSets = 8, nMissEntries = 4)))
  }

  private val address = 0x80000040L

  private def idle(dut: TemporalL1Feedback): Unit = {
    dut.io.result.valid.poke(false.B)
    dut.io.clear.poke(false.B)
    dut.io.context.poke(1.U)
    dut.io.alloc.foreach(_.valid.poke(false.B))
    dut.io.sent.foreach(_.valid.poke(false.B))
    dut.io.refill.valid.poke(false.B)
    dut.io.install.valid.poke(false.B)
    dut.io.invalidate.valid.poke(false.B)
    dut.io.storeHit.valid.poke(false.B)
    dut.io.loads.foreach(_.valid.poke(false.B))
    dut.io.loadMiss.foreach(_.valid.poke(false.B))
    dut.io.feedback.ready.poke(false.B)
  }

  private def reportResult(dut: TemporalL1Feedback, generation: Int,
    allocated: Boolean, merged: Boolean = false, cacheHit: Boolean = false): Unit = {
    val result = dut.io.result
    result.bits.paddr.poke(address.U)
    result.bits.vaddr.poke(address.U)
    result.bits.cacheHit.poke(cacheHit.B)
    result.bits.allocated.poke(allocated.B)
    result.bits.merged.poke(merged.B)
    result.bits.mshr.poke(1.U)
    result.bits.owner.pc.poke(12.U)
    result.bits.owner.context.poke(1.U)
    result.bits.owner.record.slot.poke(42.U)
    result.bits.owner.record.generation.poke(generation.U)
    result.bits.owner.wrapEpoch.poke(0.U)
    result.valid.poke(true.B)
    dut.clock.step()
    result.valid.poke(false.B)
  }

  private def allocate(dut: TemporalL1Feedback, generation: Int): Unit =
    reportResult(dut, generation, allocated = true)

  private def sent(dut: TemporalL1Feedback): Unit = {
    val send = dut.io.sent.head
    send.bits.paddr.poke(address.U)
    send.bits.mshr.poke(1.U)
    send.valid.poke(true.B)
    dut.clock.step(2)
    send.valid.poke(false.B)
  }

  private def install(dut: TemporalL1Feedback): Unit = {
    dut.io.refill.bits.poke(1.U)
    dut.io.refill.valid.poke(true.B)
    dut.io.install.bits.vaddr.poke(address.U)
    dut.io.install.bits.paddr.poke(address.U)
    dut.io.install.bits.way.poke(2.U)
    dut.io.install.valid.poke(true.B)
    dut.clock.step()
    dut.io.install.valid.poke(false.B)
    dut.io.refill.valid.poke(false.B)
  }

  private def read(dut: TemporalL1Feedback, hit: Boolean): Unit = {
    val load = dut.io.loads.head
    load.bits.vaddr.poke(address.U)
    load.bits.paddr.poke(address.U)
    load.bits.way.poke(2.U)
    load.bits.hit.poke(hit.B)
    load.valid.poke(true.B)
    dut.io.loadMiss.head.bits.poke(1.U)
    dut.io.loadMiss.head.valid.poke((!hit).B)
    dut.clock.step()
    load.valid.poke(false.B)
    dut.io.loadMiss.head.valid.poke(false.B)
  }

  private def consume(dut: TemporalL1Feedback, generation: Int)(verify: TemporalFeedback => Unit): Unit = {
    val feedback = dut.io.feedback
    feedback.valid.expect(true.B)
    feedback.bits.owner.record.generation.expect(generation.U)
    verify(feedback.bits)
    feedback.ready.poke(true.B)
    dut.clock.step()
    feedback.ready.poke(false.B)
  }

  it should "credit the accepted miss, distinguish delayed read, and ignore duplicate callbacks" in {
    test(new TemporalL1Feedback) { dut =>
      idle(dut)
      allocate(dut, 17)
      sent(dut)
      consume(dut, 17)(_.sent.expect(true.B))
      read(dut, hit = false)
      consume(dut, 17) { feedback =>
        feedback.adjustRecord.expect(true.B)
        feedback.priority.expect(1.U)
        feedback.legacyUseful.expect(true.B)
        feedback.legacyTimely.expect(false.B)
      }
      read(dut, hit = false)
      dut.io.feedback.valid.expect(false.B)
      install(dut)
      read(dut, hit = true)
      consume(dut, 17) { feedback =>
        feedback.useful.expect(true.B)
        feedback.timely.expect(false.B)
        feedback.adjustRecord.expect(false.B)
      }
      read(dut, hit = true)
      dut.io.feedback.valid.expect(false.B)
    }
  }

  it should "credit a store and subsequent timely read separately and retire unused residents" in {
    test(new TemporalL1Feedback) { dut =>
      idle(dut)
      allocate(dut, 21)
      sent(dut)
      consume(dut, 21)(_.sent.expect(true.B))
      install(dut)
      val store = dut.io.storeHit
      store.valid.poke(true.B)
      store.bits.vaddr.poke(address.U)
      store.bits.paddr.poke(address.U)
      store.bits.way.poke(2.U)
      dut.clock.step()
      store.valid.poke(false.B)
      consume(dut, 21) { feedback =>
        feedback.adjustRecord.expect(true.B)
        feedback.legacyTimely.expect(true.B)
        feedback.useful.expect(false.B)
      }
      read(dut, hit = true)
      consume(dut, 21) { feedback =>
        feedback.adjustRecord.expect(false.B)
        feedback.useful.expect(true.B)
        feedback.timely.expect(true.B)
      }
      allocate(dut, 22)
      sent(dut)
      consume(dut, 22)(_.sent.expect(true.B))
      install(dut)
      val invalidate = dut.io.invalidate
      invalidate.bits.idx.poke(1.U)
      invalidate.bits.way.poke(2.U)
      invalidate.valid.poke(true.B)
      dut.clock.step()
      invalidate.valid.poke(false.B)
      consume(dut, 22) { feedback =>
        feedback.adjustRecord.expect(true.B)
        feedback.priority.expect(3.U)
      }
    }
  }

  it should "keep the allocating owner when a younger candidate finds the line in the MSHR or cache" in {
    test(new TemporalL1Feedback) { dut =>
      idle(dut)
      allocate(dut, 31)
      reportResult(dut, 32, allocated = false, merged = true)
      consume(dut, 32) { feedback =>
        feedback.legacyUseful.expect(true.B)
        feedback.legacyTimely.expect(false.B)
        feedback.adjustRecord.expect(false.B)
      }
      sent(dut)
      consume(dut, 31)(_.sent.expect(true.B))
      install(dut)
      reportResult(dut, 33, allocated = false, cacheHit = true)
      dut.io.feedback.valid.expect(false.B)
      read(dut, hit = true)
      consume(dut, 31)(_.timely.expect(true.B))
      dut.io.feedback.valid.expect(false.B)
    }
  }
}
