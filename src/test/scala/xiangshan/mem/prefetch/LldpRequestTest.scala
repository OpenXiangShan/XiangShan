package xiangshan.mem.prefetch

import chisel3._
import chiseltest._
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import top.L1LldpConfig
import utility.{PerfCounterOptions, PerfCounterOptionsKey, XSPerfLevel}
import xiangshan.{XSCoreParamsKey, XSTileKey}
import xiangshan.cache.mmu.Pbmt

class LldpRequestTest extends AnyFlatSpec with ChiselScalatestTester {
  private val base = new L1LldpConfig()
  private val core = base(XSTileKey).head
  private implicit val config: Parameters = base.alterPartial {
    case PerfCounterOptionsKey =>
      PerfCounterOptions(enablePerfPrint = false, enablePerfDB = false, perfLevel = XSPerfLevel.VERBOSE, perfDBHartID = 0)
    case XSCoreParamsKey =>
      core.copy(dcacheParametersOpt = core.dcacheParametersOpt.map(_.copy(nMissEntries = 4)))
  }

  private val producerPc = 0x210
  private val consumerPc = 0x220
  private val producerBlock = 0x80000040L
  private val producerOffset = 8
  private val consumerVa = 0x80001068L
  private val consumerPa = 0x90001068L

  private def integerLoad(dataType: LldpDataType): Unit = {
    dataType.size.poke(3.U)
    dataType.sext.poke(false.B)
    dataType.floating.poke(false.B)
  }

  private def initialize(dut: L1LldpPrefetcher): Unit = {
    dut.io.enable.poke(true.B)
    dut.io.context.poke(1.U)
    dut.io.contextChanged.poke(false.B)
    dut.io.train.foreach(_.valid.poke(false.B))
    dut.io.demand.foreach(_.valid.poke(false.B))
    dut.io.addressTrain.foreach(_.valid.poke(false.B))
    dut.io.invalidate.valid.poke(false.B)
    dut.io.redirect.valid.poke(false.B)
    dut.io.prefetch.valid.poke(false.B)
    dut.io.refill.valid.poke(false.B)
    dut.io.refill.bits.chainHit.valid.poke(false.B)
    dut.io.role.ready.poke(true.B)
    dut.io.tlb.req.ready.poke(true.B)
    dut.io.tlb.resp.valid.poke(false.B)
    dut.io.tlb.resp.bits.miss.poke(false.B)
    dut.io.tlb.resp.bits.excp.foreach { exception =>
      exception.pf.ld.poke(false.B)
      exception.af.ld.poke(false.B)
      exception.gpf.ld.poke(false.B)
    }
    dut.io.pmp.ld.poke(false.B)
    dut.io.pmp.mmio.poke(false.B)
    dut.io.request.ready.poke(false.B)
  }

  private def train(dut: L1LldpPrefetcher): Unit = {
    val sample = dut.io.train.head
    sample.bits.producer.poke(producerPc.U)
    sample.bits.consumer.poke(consumerPc.U)
    sample.bits.descriptor.length.poke(1.U)
    sample.bits.descriptor.displacement.poke((-16).S)
    integerLoad(sample.bits.consumerType)
    for (_ <- 0 until 3) {
      sample.valid.poke(true.B)
      dut.clock.step()
      sample.valid.poke(false.B)
      dut.clock.step(4)
    }
  }

  private def triggerMetadata(trigger: LldpTrigger, depth: Boolean): Unit = {
    trigger.pc.poke(producerPc.U)
    trigger.context.poke(1.U)
    trigger.offset.poke(producerOffset.U)
    trigger.depth.poke(depth.B)
    trigger.addressRef.valid.poke(false.B)
    integerLoad(trigger.dataType)
  }

  private def returnLine(dut: L1LldpPrefetcher, block: Long, chainHit: Boolean, depth: Boolean): Unit = {
    val refill = dut.io.refill
    refill.bits.paddr.poke(block.U)
    refill.bits.mshr.poke(1.U)
    refill.bits.readable.poke(true.B)
    refill.bits.data.poke((BigInt(consumerVa + 16) << (producerOffset * 8)).U)
    triggerMetadata(refill.bits.chainHit.bits, depth)
    refill.bits.chainHit.valid.poke(chainHit.B)
    refill.valid.poke(true.B)
    dut.clock.step()
    refill.valid.poke(false.B)
    refill.bits.chainHit.valid.poke(false.B)
  }

  private def trigger(
    dut: L1LldpPrefetcher,
    source: LldpValueSource.Type,
    depth: Boolean = false,
    block: Long = producerBlock
  ): Unit = {
    if (source == LldpValueSource.Demand) {
      val demand = dut.io.demand.head
      demand.bits.load.paddr.poke((block + producerOffset).U)
      demand.bits.load.mshr.get.poke(1.U)
      demand.bits.load.missAccepted.get.poke(true.B)
      demand.bits.info.pc.poke(producerPc.U)
      integerLoad(demand.bits.info.dataType)
      demand.valid.poke(true.B)
      dut.clock.step()
      demand.valid.poke(false.B)
    }
    if (source == LldpValueSource.Spatial) {
      val prefetch = dut.io.prefetch
      triggerMetadata(prefetch.bits.trigger, depth = false)
      prefetch.bits.paddr.poke(block.U)
      prefetch.bits.mshr.poke(1.U)
      prefetch.bits.allocated.poke(true.B)
      prefetch.bits.fromLldp.poke(false.B)
      prefetch.bits.dataHit.poke(false.B)
      prefetch.valid.poke(true.B)
      dut.clock.step()
      prefetch.valid.poke(false.B)
    }
    returnLine(dut, block, chainHit = source == LldpValueSource.Chain, depth = depth)
  }

  private def awaitTranslation(dut: L1LldpPrefetcher): Unit = {
    val tlb = dut.io.tlb
    var cycles = 0
    while (!tlb.req.valid.peek().litToBoolean && cycles < 20) {
      dut.clock.step()
      cycles += 1
    }
    tlb.req.valid.expect(true.B)
  }

  private def translate(dut: L1LldpPrefetcher, pbmt: UInt = Pbmt.pma, permissionDenied: Boolean = false): Unit = {
    awaitTranslation(dut)
    val tlb = dut.io.tlb
    tlb.req.bits.no_translate.expect(false.B)
    tlb.req.bits.fullva.expect(consumerVa.U)
    dut.clock.step()
    tlb.resp.bits.paddr.head.poke(consumerPa.U)
    tlb.resp.bits.pbmt.head.poke(pbmt)
    tlb.resp.bits.excp.head.pf.ld.poke(permissionDenied.B)
    tlb.resp.valid.poke(true.B)
    dut.clock.step()
    tlb.resp.valid.poke(false.B)
    dut.clock.step()
  }

  private def trainAddress(dut: L1LldpPrefetcher, block: Long): Unit = {
    val sample = dut.io.addressTrain.head
    sample.bits.producer.poke(producerPc.U)
    sample.bits.consumer.poke(consumerPc.U)
    sample.bits.producerAddress.poke((block + producerOffset).U)
    sample.bits.consumerAddress.poke(consumerPa.U)
    sample.bits.consumerVaddr.poke(consumerVa.U)
    for (_ <- 0 until 2) {
      sample.valid.poke(true.B)
      dut.clock.step()
      sample.valid.poke(false.B)
      dut.clock.step(6)
    }
  }

  private def expectNoRequest(dut: L1LldpPrefetcher): Unit = {
    for (_ <- 0 until 16) {
      dut.io.tlb.req.valid.expect(false.B)
      dut.io.request.valid.expect(false.B)
      dut.clock.step()
    }
  }

  it should "issue the translated line and preserve demand, spatial and chain sources through backpressure" in {
    test(new L1LldpPrefetcher) { dut =>
      initialize(dut)
      train(dut)
      for (source <- Seq(LldpValueSource.Demand, LldpValueSource.Spatial, LldpValueSource.Chain)) {
        trigger(dut, source)
        translate(dut)
        val request = dut.io.request
        dut.clock.step(3)
        request.valid.expect(true.B)
        request.bits.vaddr.expect((consumerVa & ~63L).U)
        request.bits.paddr.expect((consumerPa & ~63L).U)
        request.bits.lldpTrigger.get.valid.expect(true.B)
        request.bits.lldpTrigger.get.bits.source.expect(source.litValue.U)
        request.bits.lldpTrigger.get.bits.depth.expect((source == LldpValueSource.Chain).B)
        request.bits.lldpTrigger.get.bits.offset.expect((consumerPa & 63L).U)
        request.bits.lldpTrigger.get.bits.pc.expect(consumerPc.U)
        request.ready.poke(true.B)
        dut.clock.step()
        request.ready.poke(false.B)
      }
      trigger(dut, LldpValueSource.Chain, depth = true)
      expectNoRequest(dut)
    }
  }

  it should "suppress page faults, noncacheable translations, PMP rejects and disabled lookups" in {
    test(new L1LldpPrefetcher) { dut =>
      initialize(dut)
      train(dut)
      trigger(dut, LldpValueSource.Chain)
      translate(dut, permissionDenied = true)
      expectNoRequest(dut)
      trigger(dut, LldpValueSource.Chain)
      translate(dut, pbmt = Pbmt.io)
      expectNoRequest(dut)
      dut.io.pmp.ld.poke(true.B)
      trigger(dut, LldpValueSource.Chain)
      translate(dut)
      expectNoRequest(dut)
      dut.io.pmp.ld.poke(false.B)
      dut.io.enable.poke(false.B)
      trigger(dut, LldpValueSource.Chain)
      expectNoRequest(dut)
    }
  }

  it should "issue the learned byte address for all three triggers and suppress duplicate value replay" in {
    test(new L1LldpPrefetcher) { dut =>
      initialize(dut)
      train(dut)
      val sources = Seq(LldpValueSource.Demand, LldpValueSource.Spatial, LldpValueSource.Chain)
      for ((source, index) <- sources.zipWithIndex) {
        val block = producerBlock + 64L * index
        trainAddress(dut, block)
        trigger(dut, source, block = block)
        awaitTranslation(dut)
        val tlb = dut.io.tlb
        tlb.req.bits.no_translate.expect(true.B)
        tlb.req.bits.pmp_addr.expect(consumerPa.U)
        dut.clock.step()
        tlb.resp.bits.pbmt.head.poke(Pbmt.pma)
        tlb.resp.valid.poke(true.B)
        dut.clock.step()
        tlb.resp.valid.poke(false.B)
        dut.clock.step()
        val request = dut.io.request
        request.valid.expect(true.B)
        request.bits.paddr.expect((consumerPa & ~63L).U)
        val metadata = request.bits.lldpTrigger.get.bits
        metadata.addressRef.valid.expect(true.B)
        metadata.source.expect(source.litValue.U)
        metadata.offset.expect((consumerPa & 63L).U)
        request.ready.poke(true.B)
        dut.clock.step()
        request.ready.poke(false.B)
        expectNoRequest(dut)
      }
    }
  }
}
