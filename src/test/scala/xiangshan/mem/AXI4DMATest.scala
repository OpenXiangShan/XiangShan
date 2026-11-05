package xiangshan.mem

import chisel3._
import chisel3.util._
import chiseltest._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import top.KunminghuV2WithoutLLCConfig
import xiangshan.{XSCoreParamsKey, XSTileKey}
import xiangshan.cache.DCacheToSbufferIO

private class DMAHarness(implicit p: Parameters) extends LazyModule {
  val master = AXI4MasterNode(Seq(AXI4MasterPortParameters(Seq(
    AXI4MasterParameters("test_dma", id = IdRange(0, 1 << 14), aligned = true, maxFlight = Some(1))
  ))))
  val bridge = LazyModule(new AXI4DMABridge)
  bridge.axiNode := AXI4Fragmenter() := AXI4Buffer() := AXI4IdIndexer(1) := master
  val memory = TLManagerNode(Seq(TLSlavePortParameters.v1(Seq(TLSlaveParameters.v1(
    address = Seq(AddressSet(0, (BigInt(1) << 48) - 1)),
    regionType = RegionType.UNCACHED,
    supportsGet = TransferSizes(1, 64),
    fifoId = Some(0)
  )), beatBytes = 32)))
  memory := bridge.readNode

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val io = IO(new Bundle {
      val axi = Flipped(new AXI4Bundle(master.out.head._2.bundle))
      val mem = new TLBundle(memory.in.head._2.bundle)
      val sbuffer = new DCacheToSbufferIO
      val dcache = Flipped(new DCacheToSbufferIO)
    })
    master.out.head._1 <> io.axi
    io.mem <> memory.in.head._1
    private val arb = Module(new DMAStoreArbiter)
    arb.io.dma <> bridge.module.io.write
    arb.io.sbuffer <> io.sbuffer
    io.dcache <> arb.io.dcache
  }
}

class AXI4DMATest extends AnyFlatSpec with ChiselScalatestTester {
  private val config = new KunminghuV2WithoutLLCConfig
  implicit private val p: Parameters = config.alterPartial {
    case XSCoreParamsKey => config(XSTileKey).head
  }

  private def waitUntil(c: DMAHarness#Impl)(cond: => Boolean): Unit = {
    var cycles = 0
    while (!cond && cycles < 200) { c.clock.step(); cycles += 1 }
    assert(cond, "DMA handshake timed out")
  }

  private def init(c: DMAHarness#Impl): Unit = {
    c.io.axi.aw.valid.poke(false.B)
    c.io.axi.w.valid.poke(false.B)
    c.io.axi.ar.valid.poke(false.B)
    c.io.axi.r.ready.poke(false.B)
    c.io.axi.b.ready.poke(false.B)
    c.io.sbuffer.req.valid.poke(false.B)
    c.io.dcache.req.ready.poke(false.B)
    c.io.dcache.main_pipe_hit_resp.valid.poke(false.B)
    c.io.dcache.replay_resp.valid.poke(false.B)
    c.io.mem.a.ready.poke(false.B)
    c.io.mem.d.valid.poke(false.B)
    c.clock.step(2)
  }

  private def address(c: DMAHarness#Impl, a: AXI4BundleA, addr: BigInt, size: Int, len: Int,
                      burst: Int, id: Int): Unit = {
    a.addr.poke(addr.U)
    a.id.poke(id.U)
    a.len.poke(len.U)
    a.size.poke(size.U)
    a.burst.poke(burst.U)
    a.lock.poke(0.U)
    a.cache.poke(0.U)
    a.prot.poke(0.U)
    a.qos.poke(0.U)
  }

  private def aw(c: DMAHarness#Impl, addr: BigInt, size: Int = 5, len: Int = 0,
                 burst: Int = 1, id: Int = 0x2a35): Unit = {
    address(c, c.io.axi.aw.bits, addr, size, len, burst, id)
    c.io.axi.aw.valid.poke(true.B)
    waitUntil(c)(c.io.axi.aw.ready.peek().litToBoolean)
    c.clock.step()
    c.io.axi.aw.valid.poke(false.B)
  }

  private def w(c: DMAHarness#Impl, data: BigInt, mask: BigInt, last: Boolean): Unit = {
    c.io.axi.w.bits.data.poke(data.U)
    c.io.axi.w.bits.strb.poke(mask.U)
    c.io.axi.w.bits.last.poke(last.B)
    c.io.axi.w.valid.poke(true.B)
    waitUntil(c)(c.io.axi.w.ready.peek().litToBoolean)
    c.clock.step()
    c.io.axi.w.valid.poke(false.B)
  }

  private def store(c: DMAHarness#Impl, addr: BigInt, data: BigInt, mask: BigInt,
                    replay: Boolean = false): Unit = {
    waitUntil(c)(c.io.dcache.req.valid.peek().litToBoolean)
    c.io.dcache.req.bits.addr.expect((addr & ~BigInt(63)).U)
    val offset = (addr & 32).toInt
    c.io.dcache.req.bits.data.expect((data << (offset * 8)).U)
    c.io.dcache.req.bits.mask.expect((mask << offset).U)
    val id = c.io.dcache.req.bits.id.peek().litValue
    c.io.dcache.req.ready.poke(true.B)
    c.clock.step()
    c.io.dcache.req.ready.poke(false.B)
    c.io.dcache.req.valid.expect(false.B)
    c.clock.step(5)
    c.io.dcache.req.valid.expect(false.B)
    c.io.axi.b.valid.expect(false.B)
    if (replay) {
      c.io.dcache.replay_resp.bits.id.poke(id.U)
      c.io.dcache.replay_resp.valid.poke(true.B)
      c.clock.step()
      c.io.dcache.replay_resp.valid.poke(false.B)
      c.io.dcache.req.valid.expect(true.B)
      c.io.dcache.req.bits.id.expect(id.U)
      c.io.dcache.req.bits.mask.expect((mask << offset).U)
      c.io.dcache.req.ready.poke(true.B)
      c.clock.step()
      c.io.dcache.req.ready.poke(false.B)
      c.io.dcache.req.valid.expect(false.B)
    }
    c.io.dcache.main_pipe_hit_resp.bits.id.poke(id.U)
    c.io.dcache.main_pipe_hit_resp.valid.poke(true.B)
    c.io.sbuffer.main_pipe_hit_resp.valid.expect(false.B)
    c.clock.step()
    c.io.dcache.main_pipe_hit_resp.valid.poke(false.B)
  }

  private def b(c: DMAHarness#Impl, id: Int = 0x2a35, resp: Int = 0): Unit = {
    waitUntil(c)(c.io.axi.b.valid.peek().litToBoolean)
    c.io.axi.b.bits.id.expect(id.U)
    c.io.axi.b.bits.resp.expect(resp.U)
    c.clock.step(3)
    c.io.axi.b.valid.expect(true.B)
    c.io.axi.b.bits.id.expect(id.U)
    c.io.axi.b.ready.poke(true.B)
    c.clock.step()
    c.io.axi.b.ready.poke(false.B)
  }

  behavior of "MemBlock AXI DMA"

  it should "serialize stores, retry on replay, and route Sbuffer responses while DMA is pending" in {
    test(LazyModule(new DMAHarness).module) { c =>
      init(c)
      aw(c, 0x80000028L, size = 3)
      w(c, BigInt(0x12345678L) << 64, 0xff00, last = true)
      waitUntil(c)(c.io.dcache.req.valid.peek().litToBoolean)
      c.clock.step(4)
      c.io.dcache.req.valid.expect(true.B)
      // A pending DMA request must not swallow an older Sbuffer completion.
      c.io.dcache.main_pipe_hit_resp.bits.id.poke(3.U)
      c.io.dcache.main_pipe_hit_resp.valid.poke(true.B)
      c.io.sbuffer.main_pipe_hit_resp.valid.expect(true.B)
      c.io.sbuffer.main_pipe_hit_resp.bits.id.expect(3.U)
      c.io.dcache.replay_resp.bits.id.poke(4.U)
      c.io.dcache.replay_resp.valid.poke(true.B)
      c.io.sbuffer.replay_resp.valid.expect(true.B)
      c.clock.step()
      c.io.dcache.main_pipe_hit_resp.valid.poke(false.B)
      c.io.dcache.replay_resp.valid.poke(false.B)
      store(c, 0x80000028L, BigInt(0x12345678L) << 64, 0xff00, replay = true)
      b(c)
      c.io.dcache.req.valid.expect(false.B)
      // Zero-strobe transfers complete without touching DCache.
      aw(c, 0x80000000L)
      w(c, 0, 0, last = true)
      b(c)
      c.io.dcache.req.valid.expect(false.B)
    }
  }

  it should "arbitrate fairly and return one response after all INCR burst writes complete" in {
    test(LazyModule(new DMAHarness).module) { c =>
      init(c)
      aw(c, 0x80000000L, len = 1)
      w(c, 0xdeadbeefL, 0xffffffffL, last = false)
      waitUntil(c)(c.io.dcache.req.valid.peek().litToBoolean)
      // RR arbitration must give both continuously valid producers a turn.
      c.io.sbuffer.req.valid.poke(true.B)
      c.io.sbuffer.req.bits.id.poke(2.U)
      c.io.dcache.req.ready.poke(true.B)
      var seenSbuffer = false
      var seenDMA = false
      var dmaId = BigInt(0)
      for (_ <- 0 until 4) {
        if (c.io.dcache.req.valid.peek().litToBoolean) {
          val id = c.io.dcache.req.bits.id.peek().litValue
          if (id == 2) seenSbuffer = true else { seenDMA = true; dmaId = id }
        }
        c.clock.step()
      }
      assert(seenSbuffer && seenDMA)
      c.io.sbuffer.req.valid.poke(false.B)
      c.io.dcache.req.ready.poke(false.B)
      c.io.dcache.main_pipe_hit_resp.valid.poke(true.B)
      c.io.dcache.main_pipe_hit_resp.bits.id.poke(dmaId.U)
      c.clock.step()
      c.io.dcache.main_pipe_hit_resp.valid.poke(false.B)
      c.clock.step(5)
      c.io.axi.b.valid.expect(false.B)
      w(c, 0xcafef00dL, 0x0f, last = true)
      store(c, 0x80000020L, 0xcafef00dL, 0x0f)
      b(c)
    }
  }

  it should "read coherent cache lines and preserve burst IDs, byte lanes, and R backpressure" in {
    test(LazyModule(new DMAHarness).module) { c =>
      init(c)
      address(c, c.io.axi.ar.bits, 0x80000038L, 3, 1, 1, 0x3abc)
      c.io.axi.ar.valid.poke(true.B)
      waitUntil(c)(c.io.axi.ar.ready.peek().litToBoolean)
      c.clock.step()
      c.io.axi.ar.valid.poke(false.B)
      for (beat <- 0 until 2) {
        waitUntil(c)(c.io.mem.a.valid.peek().litToBoolean)
        c.io.mem.a.bits.opcode.expect(TLMessages.Get)
        c.io.mem.a.bits.size.expect(6.U)
        c.io.mem.a.bits.address.expect((0x80000000L + beat * 64).U)
        c.io.mem.a.ready.poke(true.B)
        c.clock.step()
        c.io.mem.a.ready.poke(false.B)
        for (part <- 0 until 2) {
          c.io.mem.d.bits.opcode.poke(TLMessages.AccessAckData)
          c.io.mem.d.bits.param.poke(0.U)
          c.io.mem.d.bits.size.poke(6.U)
          c.io.mem.d.bits.source.poke(0.U)
          c.io.mem.d.bits.sink.poke(0.U)
          c.io.mem.d.bits.denied.poke(false.B)
          c.io.mem.d.bits.corrupt.poke(false.B)
          c.io.mem.d.bits.data.poke((BigInt(0x1234) + beat * 16 + part).U)
          c.io.mem.d.valid.poke(true.B)
          waitUntil(c)(c.io.mem.d.ready.peek().litToBoolean)
          c.clock.step()
          c.io.mem.d.valid.poke(false.B)
        }
        waitUntil(c)(c.io.axi.r.valid.peek().litToBoolean)
        c.io.axi.r.bits.id.expect(0x3abc.U)
        c.io.axi.r.bits.last.expect((beat == 1).B)
        c.io.axi.r.bits.resp.expect(0.U)
        c.io.axi.r.bits.data.expect((BigInt(0x1234) + beat * 16 + (if (beat == 0) 1 else 0)).U)
        c.clock.step(4)
        c.io.axi.r.valid.expect(true.B)
        c.io.axi.r.ready.poke(true.B)
        c.clock.step()
        c.io.axi.r.ready.poke(false.B)
      }
    }
  }

  it should "reject non-memory accesses without issuing cache requests" in {
    test(LazyModule(new DMAHarness).module) { c =>
      init(c)
      aw(c, 0x10000000L)
      w(c, 1, 1, last = true)
      b(c, resp = 3)
      c.io.dcache.req.valid.expect(false.B)
      address(c, c.io.axi.ar.bits, 0x10000000L, 5, 0, 1, 0x1234)
      c.io.axi.ar.valid.poke(true.B)
      waitUntil(c)(c.io.axi.ar.ready.peek().litToBoolean)
      c.clock.step()
      c.io.axi.ar.valid.poke(false.B)
      waitUntil(c)(c.io.axi.r.valid.peek().litToBoolean)
      c.io.axi.r.bits.id.expect(0x1234.U)
      c.io.axi.r.bits.resp.expect(3.U)
      c.io.mem.a.valid.expect(false.B)
    }
  }
}
