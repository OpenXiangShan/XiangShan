package xiangshan.mem.prefetch.temporal

import chisel3._
import chisel3.util.ValidIO
import chiseltest._
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import top.L1TemporalConfig
import utility.{PerfCounterOptions, PerfCounterOptionsKey, XSPerfLevel}
import xiangshan.{XSCoreParamsKey, XSTileKey}

class L1TemporalRequestTest extends AnyFlatSpec with ChiselScalatestTester {
  private val base = new L1TemporalConfig()
  private implicit val config: Parameters = base.alterPartial {
    case PerfCounterOptionsKey =>
      PerfCounterOptions(enablePerfPrint = false, enablePerfDB = false, perfLevel = XSPerfLevel.VERBOSE, perfDBHartID = 0)
    case XSCoreParamsKey => base(XSTileKey).head
  }

  private val pc = 0x1002
  private val context = 3
  private val baseAddress = 0x80008000L

  private def initialize(dut: L1TemporalPrefetcher): Unit = {
    dut.io.enable.poke(true.B)
    dut.io.context.poke(context.U)
    dut.io.contextChanged.poke(false.B)
    dut.io.redirect.valid.poke(false.B)
    dut.io.allocate.foreach(_.valid.poke(false.B))
    dut.io.load.foreach(_.valid.poke(false.B))
    dut.io.feedback.valid.poke(false.B)
    dut.io.role.valid.poke(false.B)
    dut.io.tlb.req.ready.poke(true.B)
    dut.io.tlb.resp.valid.poke(false.B)
    dut.io.tlb.resp.bits.miss.poke(false.B)
    dut.io.tlb.resp.bits.excp.head.pf.ld.poke(false.B)
    dut.io.tlb.resp.bits.excp.head.af.ld.poke(false.B)
    dut.io.tlb.resp.bits.excp.head.gpf.ld.poke(false.B)
    dut.io.pmp.ld.poke(false.B)
    dut.io.pmp.mmio.poke(false.B)
    dut.io.request.ready.poke(false.B)
    dut.clock.step()
  }

  private def stepWithoutTranslation(dut: L1TemporalPrefetcher, cycles: Int): Unit = {
    for (_ <- 0 until cycles) {
      dut.io.tlb.req.valid.expect(false.B)
      dut.io.request.valid.expect(false.B)
      dut.clock.step()
    }
  }

  private def allocateLoad(dut: L1TemporalPrefetcher, index: Int): Unit = {
    val allocation = dut.io.allocate.head
    allocation.bits.flag.poke(false.B)
    allocation.bits.value.poke(index.U)
    allocation.valid.poke(true.B)
    dut.clock.step()
    allocation.valid.poke(false.B)
  }

  private def driveLoad(lane: ValidIO[L1TemporalLoad], index: Int, loadPc: Int, paddr: Long): Unit = {
    val vaddr = 0x40000000L + (index % 4) * 4096 + (paddr & 4095)
    lane.bits.lqIdx.flag.poke(false.B)
    lane.bits.lqIdx.value.poke(index.U)
    lane.bits.pc.poke(loadPc.U)
    lane.bits.paddr.poke(paddr.U)
    lane.bits.vaddr.poke(vaddr.U)
    lane.bits.kind.poke(2.U)
    lane.bits.miss.poke((index % 2 == 0).B)
    lane.valid.poke(true.B)
  }

  private def observe(dut: L1TemporalPrefetcher, sample: Int): Unit = {
    val index = sample % 16
    allocateLoad(dut, index)
    // Two notifications for the same dynamic LQ allocation, plus a later repeat.
    val lanes = dut.io.load.take(2)
    for (lane <- lanes) {
      driveLoad(lane, index, pc, baseAddress + index * 64)
    }
    dut.clock.step()
    lanes.last.valid.poke(false.B)
    dut.clock.step()
    lanes.head.valid.poke(false.B)
  }

  private def checkTarget(dut: L1TemporalPrefetcher, index: Int): Unit = {
    val tlb = dut.io.tlb
    var cycles = 0
    while (!tlb.req.valid.peek().litToBoolean && cycles < 80) {
      dut.io.request.valid.expect(false.B)
      dut.clock.step()
      cycles += 1
    }
    val address = baseAddress + index * 64
    tlb.req.valid.expect(true.B)
    tlb.req.bits.no_translate.expect(true.B)
    dut.clock.step()
    tlb.req.bits.pmp_addr.expect(address.U)
    tlb.resp.valid.poke(true.B)
    dut.clock.step()
    tlb.resp.valid.poke(false.B)
    dut.io.pmp.mmio.poke((index == 2).B)
    dut.clock.step()
    dut.io.pmp.mmio.poke(false.B)
    if (index == 2) {
      dut.io.request.valid.expect(false.B)
    } else {
      val request = dut.io.request
      dut.clock.step(3)
      request.valid.expect(true.B)
      request.bits.paddr.expect(address.U)
      // The default L1D XORs virtual page-number pairs, including bit 30 of the stimulus VA.
      val expectedAlias = (index % 4) ^ 1
      request.bits.vaddr.expect(((expectedAlias * 4096) | (address & 4095)).U)
      request.bits.temporalOwner.get.pc.expect(10304.U)
      request.bits.temporalOwner.get.context.expect(context.U)
      request.ready.poke(true.B)
      dut.clock.step()
      request.ready.poke(false.B)
    }
  }

  it should "deduplicate LQ events, discard redirected samples and replay with aliases and PMP checks" in {
    test(new L1TemporalPrefetcher(L1TemporalParams())) { dut =>
      initialize(dut)
      for (mask <- Seq(1, 2)) {
        dut.io.role.bits.pc.poke(pc.U)
        dut.io.role.bits.context.poke(context.U)
        dut.io.role.bits.roles.poke(mask.U)
        dut.io.role.valid.poke(true.B)
        dut.io.role.ready.expect(true.B)
        dut.clock.step()
        dut.io.role.valid.poke(false.B)
        stepWithoutTranslation(dut, 8)
      }

      for (sample <- 0 until 32) {
        observe(dut, sample)
        stepWithoutTranslation(dut, 28)
      }
      // A higher-priority role update holds the trigger in the training FIFO.
      dut.io.role.valid.poke(true.B)
      observe(dut, 32)
      dut.io.redirect.valid.poke(true.B)
      dut.clock.step()
      dut.io.redirect.valid.poke(false.B)
      dut.io.role.valid.poke(false.B)
      stepWithoutTranslation(dut, 28)

      // Reusing the same LQ allocation can train immediately after the flush.
      observe(dut, 32)
      for (index <- 1 to 12) {
        checkTarget(dut, index)
      }
      stepWithoutTranslation(dut, 24)
      dut.io.enable.poke(false.B)
      observe(dut, 32)
      stepWithoutTranslation(dut, 28)
    }
  }

  it should "accept a repeated notification after the full training FIFO drains" in {
    test(new L1TemporalPrefetcher(L1TemporalParams())) { dut =>
      initialize(dut)
      for (sample <- 0 until 32) {
        observe(dut, sample)
        stepWithoutTranslation(dut, 28)
      }

      // Role updates have priority over training and keep the sample FIFO from draining.
      val otherPc = 0x4002
      val role = dut.io.role
      role.bits.pc.poke(otherPc.U)
      role.bits.context.poke(context.U)
      role.bits.roles.poke(1.U)
      role.valid.poke(true.B)
      val lane = dut.io.load.head
      for (index <- 0 until 16) {
        allocateLoad(dut, index)
        driveLoad(lane, index, otherPc, baseAddress + 0x4000 + index * 64)
        dut.clock.step()
        lane.valid.poke(false.B)
      }
      val triggerIndex = 16
      allocateLoad(dut, triggerIndex)
      driveLoad(lane, triggerIndex, pc, baseAddress)
      dut.clock.step()
      lane.valid.poke(false.B)

      role.valid.poke(false.B)
      stepWithoutTranslation(dut, 512)

      // Use the same LQ allocation. A dropped sample must leave it eligible for training.
      driveLoad(lane, triggerIndex, pc, baseAddress)
      dut.clock.step()
      lane.valid.poke(false.B)
      for (index <- 1 to 12) {
        checkTarget(dut, index)
      }
      stepWithoutTranslation(dut, 24)
    }
  }
}
