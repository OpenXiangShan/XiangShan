package xiangshan.mem

import chisel3._
import chisel3.experimental.UnlocatableSourceInfo
import chiseltest._
import coupledL2._
import coupledL2.tl2chi._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import top.KunminghuV2WithoutLLCConfig
import xiangshan.XSTileKey

private class TestCHIOpcodes(implicit val p: Parameters) extends HasCHIOpcodes

class CHIToAXI4Test extends AnyFlatSpec with ChiselScalatestTester {
  private val config = new KunminghuV2WithoutLLCConfig
  private val clients = TLMasterPortParameters.v1(Seq(TLMasterParameters.v1(
    "test_l1", sourceId = IdRange(0, 64), supportsProbe = TransferSizes(64)
  )))
  private val managers = TLSlavePortParameters.v1(Seq(TLSlaveParameters.v1(
    address = Seq(AddressSet(0, (BigInt(1) << 48) - 1)),
    regionType = RegionType.CACHED,
    supportsAcquireT = TransferSizes(64), supportsAcquireB = TransferSizes(64),
    supportsGet = TransferSizes(1, 64)
  )), beatBytes = 32, endSinkId = 256)
  implicit private val p: Parameters = config.alterPartial {
    case CHIIssue => Issue.Eb
    case L2ParamKey => config(XSTileKey).head.L2CacheParamsOpt.get
    case BankBitsKey => 2
    case EdgeInKey => new TLEdgeIn(clients, managers, config, UnlocatableSourceInfo)
    case EdgeOutKey => new TLEdgeOut(clients, managers, config, UnlocatableSourceInfo)
  }
  private val op = new TestCHIOpcodes
  private val axi = AXI4BundleParameters(addrBits = 48, dataBits = 256, idBits = 4)
  private val address = BigInt("803c4480", 16)

  private def zero(data: Data): Unit = data match {
    case b: Bool => b.poke(false.B)
    case u: UInt => u.poke(0.U)
    case r: Record => r.elements.values.foreach(zero)
    case v: Vec[_] => v.foreach(zero)
  }
  private def init(c: CHIToAXI4): Unit = {
    zero(c.io.chi.tx.req.bits)
    zero(c.io.chi.tx.rsp.bits)
    zero(c.io.chi.tx.dat.bits)
    zero(c.io.axi.r.bits)
    zero(c.io.axi.b.bits)
    c.io.chi.tx.req.valid.poke(false.B)
    c.io.chi.tx.rsp.valid.poke(false.B)
    c.io.chi.tx.dat.valid.poke(false.B)
    c.io.chi.rx.dat.ready.poke(true.B)
    c.io.chi.rx.rsp.ready.poke(true.B)
    c.io.chi.rx.snp.ready.poke(true.B)
    c.io.axi.ar.ready.poke(false.B)
    c.io.axi.aw.ready.poke(false.B)
    c.io.axi.w.ready.poke(false.B)
    c.io.axi.r.valid.poke(false.B)
    c.io.axi.b.valid.poke(false.B)
    c.clock.step(2)
  }
  private def waitUntil(c: CHIToAXI4)(cond: => Boolean): Unit = {
    var cycles = 0
    while (!cond && cycles < 100) { c.clock.step(); cycles += 1 }
    assert(cond, "bridge handshake timed out")
  }
  private def request(c: CHIToAXI4, opcode: UInt, addr: BigInt, txn: Int): Unit = {
    c.io.chi.tx.req.bits.opcode.poke(opcode)
    c.io.chi.tx.req.bits.addr.poke(addr.U)
    c.io.chi.tx.req.bits.txnID.poke(txn.U)
    c.io.chi.tx.req.bits.size.poke(6.U)
    c.io.chi.tx.req.bits.expCompAck.poke((opcode.litValue == op.ReadUnique.litValue).B)
    c.io.chi.tx.req.valid.poke(true.B)
    waitUntil(c)(c.io.chi.tx.req.ready.peek().litToBoolean)
    c.clock.step()
    c.io.chi.tx.req.valid.poke(false.B)
  }
  private def writeback(c: CHIToAXI4, addr: BigInt, txn: Int): BigInt = {
    request(c, op.WriteBackFull, addr, txn)
    waitUntil(c)(c.io.chi.rx.rsp.valid.peek().litToBoolean)
    c.io.chi.rx.rsp.bits.opcode.expect(op.CompDBIDResp)
    c.io.chi.rx.rsp.bits.txnID.expect(txn.U)
    val id = c.io.chi.rx.rsp.bits.dbID.peek().litValue
    c.clock.step()
    id
  }
  private def writeData(c: CHIToAXI4, id: BigInt, base: Int, reverse: Boolean = false): Unit = {
    for (beat <- (if (reverse) Seq(1, 0) else Seq(0, 1))) {
      c.io.chi.tx.dat.bits.txnID.poke(id.U)
      c.io.chi.tx.dat.bits.opcode.poke(op.CopyBackWrData)
      c.io.chi.tx.dat.bits.dataID.poke((beat * 2).U)
      c.io.chi.tx.dat.bits.data.poke((base + beat).U)
      c.io.chi.tx.dat.bits.be.poke("hffffffff".U)
      c.io.chi.tx.dat.valid.poke(true.B)
      waitUntil(c)(c.io.chi.tx.dat.ready.peek().litToBoolean)
      c.clock.step()
    }
    c.io.chi.tx.dat.valid.poke(false.B)
  }
  private def drainWrite(c: CHIToAXI4, id: BigInt, base: Int): Unit = {
    waitUntil(c)(c.io.axi.aw.valid.peek().litToBoolean)
    c.io.axi.aw.bits.id.expect(id.U)
    c.io.axi.aw.ready.poke(true.B)
    c.clock.step()
    c.io.axi.aw.ready.poke(false.B)
    for (beat <- 0 until 2) {
      waitUntil(c)(c.io.axi.w.valid.peek().litToBoolean)
      c.io.axi.w.bits.data.expect((base + beat).U)
      c.io.axi.w.bits.strb.expect("hffffffff".U)
      c.io.axi.w.bits.last.expect((beat == 1).B)
      c.io.axi.w.ready.poke(true.B)
      c.clock.step()
    }
    c.io.axi.w.ready.poke(false.B)
  }
  private def b(c: CHIToAXI4, id: BigInt): Unit = {
    c.io.axi.b.bits.id.poke(id.U)
    c.io.axi.b.bits.resp.poke(0.U)
    c.io.axi.b.valid.poke(true.B)
    c.clock.step()
    c.io.axi.b.valid.poke(false.B)
    c.clock.step(2)
  }

  private def completeRead(c: CHIToAXI4, id: BigInt, txn: Int): Unit = {
    for (beat <- 0 until 2) {
      c.io.axi.r.bits.id.poke(id.U)
      c.io.axi.r.bits.data.poke((0x100 + beat).U)
      c.io.axi.r.bits.last.poke((beat == 1).B)
      c.io.axi.r.valid.poke(true.B)
      c.io.chi.rx.dat.valid.expect(true.B)
      c.io.chi.rx.dat.bits.txnID.expect(txn.U)
      c.io.chi.rx.dat.bits.dbID.expect(id.U)
      c.io.chi.rx.dat.bits.dataID.expect((beat * 2).U)
      if (beat == 1) {
        // E.b permits acknowledging after the first CompData beat.
        c.io.chi.tx.rsp.bits.opcode.poke(op.CompAck)
        c.io.chi.tx.rsp.bits.txnID.poke(id.U)
        c.io.chi.tx.rsp.valid.poke(true.B)
      }
      c.clock.step()
    }
    c.io.axi.r.valid.poke(false.B)
    c.io.chi.tx.rsp.valid.poke(false.B)
    c.clock.step(2)
  }

  behavior of "No-LLC CHI/AXI ordering"

  it should "hold a same-line read through write data and B while allowing another line to read" in {
    test(new CHIToAXI4(axi)) { c =>
      init(c)
      val writer = writeback(c, address, 0x103)
      request(c, op.ReadUnique, address, 0x204)
      request(c, op.ReadUnique, address + 64, 0x305)
      // This is a real reordering opportunity: the RAM prioritizes AR over AW.
      waitUntil(c)(c.io.axi.ar.valid.peek().litToBoolean)
      c.io.axi.ar.bits.addr.expect((address + 64).U)
      c.io.axi.ar.ready.poke(true.B)
      c.clock.step()
      c.io.axi.ar.ready.poke(false.B)
      c.io.axi.ar.valid.expect(false.B)
      writeData(c, writer, 0x100)
      c.io.axi.ar.valid.expect(false.B)
      drainWrite(c, writer, 0x100)
      c.clock.step(5)
      c.io.axi.ar.valid.expect(false.B)
      b(c, writer)
      waitUntil(c)(c.io.axi.ar.valid.peek().litToBoolean)
      c.io.axi.ar.bits.addr.expect(address.U)
    }
  }

  it should "preserve the order of two same-line writebacks even when newer data arrives first" in {
    test(new CHIToAXI4(axi)) { c =>
      init(c)
      val first = writeback(c, address, 0x103)
      val second = writeback(c, address, 0x204)
      writeData(c, second, 0x200)
      c.clock.step(3)
      c.io.axi.aw.valid.expect(false.B)
      writeData(c, first, 0x100)
      drainWrite(c, first, 0x100)
      c.clock.step(5)
      c.io.axi.aw.valid.expect(false.B)
      b(c, first)
      drainWrite(c, second, 0x200)
      b(c, second)
    }
  }

  it should "complete writeback with both data beats even when they arrive in reverse order" in {
    test(new CHIToAXI4(axi)) { c =>
      init(c)
      val writer = writeback(c, address, 0x103)
      writeData(c, writer, 0x100, reverse = true)
      drainWrite(c, writer, 0x100)
      b(c, writer)
    }
  }

  it should "hold AXI addresses stable under backpressure while independent requests arrive" in {
    test(new CHIToAXI4(axi)) { c =>
      init(c)
      request(c, op.ReadUnique, address, 0x103)
      waitUntil(c)(c.io.axi.ar.valid.peek().litToBoolean)
      val readId = c.io.axi.ar.bits.id.peek().litValue
      request(c, op.ReadUnique, address + 64, 0x204)
      for (_ <- 0 until 4) {
        c.io.axi.ar.valid.expect(true.B)
        c.io.axi.ar.bits.addr.expect(address.U)
        c.io.axi.ar.bits.id.expect(readId.U)
        c.clock.step()
      }
      val first = writeback(c, address + 128, 0x305)
      writeData(c, first, 0x100)
      waitUntil(c)(c.io.axi.aw.valid.peek().litToBoolean)
      val second = writeback(c, address + 192, 0x406)
      writeData(c, second, 0x200)
      for (_ <- 0 until 4) {
        c.io.axi.aw.valid.expect(true.B)
        c.io.axi.aw.bits.addr.expect((address + 128).U)
        c.io.axi.aw.bits.id.expect(first.U)
        c.clock.step()
      }
      drainWrite(c, first, 0x100)
      drainWrite(c, second, 0x200)
      b(c, second)
      b(c, first)
    }
  }

  it should "clear old dependencies when a retired write slot is immediately reused" in {
    test(new CHIToAXI4(axi)) { c =>
      init(c)
      val writer = writeback(c, address, 0x103)
      request(c, op.ReadUnique, address, 0x204)
      writeData(c, writer, 0x100)
      drainWrite(c, writer, 0x100)
      b(c, writer)
      // Reuse the old dependency's slot before servicing the waiting read.
      val reused = writeback(c, address + 64, 0x305)
      assert(reused == writer)
      waitUntil(c)(c.io.axi.ar.valid.peek().litToBoolean)
      c.io.axi.ar.bits.addr.expect(address.U)
      val reader = c.io.axi.ar.bits.id.peek().litValue
      c.io.axi.ar.ready.poke(true.B)
      c.clock.step()
      c.io.axi.ar.ready.poke(false.B)
      completeRead(c, reader, 0x204)
      // The reader itself must also retire after early CompAck + final data.
      val reusedReader = writeback(c, address + 128, 0x406)
      assert(reusedReader == reader)
    }
  }

  it should "fill and drain all sixteen slots with complete read and CompAck transactions" in {
    test(new CHIToAXI4(axi)) { c =>
      init(c)
      for (i <- 0 until 16) request(c, op.ReadUnique, address + i * 64, 0x100 + i)
      c.io.chi.tx.req.ready.expect(false.B)
      val seen = scala.collection.mutable.Set.empty[BigInt]
      for (_ <- 0 until 16) {
        waitUntil(c)(c.io.axi.ar.valid.peek().litToBoolean)
        val id = c.io.axi.ar.bits.id.peek().litValue
        assert(!seen(id))
        seen += id
        c.io.axi.ar.ready.poke(true.B)
        c.clock.step()
        c.io.axi.ar.ready.poke(false.B)
        completeRead(c, id, 0x100 + id.toInt)
      }
      assert(seen.size == 16)
      c.io.chi.tx.req.ready.expect(true.B)
      c.io.axi.ar.valid.expect(false.B)
    }
  }
}
