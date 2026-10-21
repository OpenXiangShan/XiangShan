package cache

import chisel3._
import chisel3.util._
import chisel3.simulator.ChiselSim
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config.Parameters
import top.DefaultConfig
import utility._
import xiangshan._
import xiangshan.cache.mmu._
import scala.collection.mutable.ArrayBuffer

// Drive the real L2TLB through its demand and TileLink interfaces. Memory
// responses are explicitly scheduled to exercise both walkers and late returns.
class PtwParallelHarness(implicit p: Parameters) extends LazyModule {
  val walker = LazyModule(new L2TLB)
  val memory = TLManagerNode(Seq(TLSlavePortParameters.v1(
    Seq(TLSlaveParameters.v1(address = Seq(AddressSet(0, BigInt("ffffffffffff", 16))),
      regionType = RegionType.UNCACHED, supportsGet = TransferSizes(64, 64))),
    beatBytes = 32, minLatency = 1)))
  memory := walker.node

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val (bus, _) = memory.in.head
    val io = IO(new Bundle {
      val request = Flipped(Decoupled(new Bundle {
        val vpn = UInt(36.W)
        val source = UInt(2.W)
      }))
      val acceptResponse = Input(Bool())
      val response = Vec(walker.module.io.tlb.length, Valid(new Bundle {
        val vpn = UInt(36.W)
        val ppn = UInt(44.W)
        val level = UInt(2.W)
        val pf = Bool()
        val af = Bool()
      }))
      val memRequest = Decoupled(new Bundle {
        val address = UInt(bus.a.bits.address.getWidth.W)
        val id = UInt(bus.a.bits.source.getWidth.W)
      })
      val memResponse = Flipped(Decoupled(new Bundle {
        val data = UInt(256.W)
        val id = UInt(bus.d.bits.source.getWidth.W)
      }))
      val flush = Input(Bool())
      val rootPpn = Input(UInt(44.W))
      val asid = Input(UInt(16.W))
      val satpChanged = Input(Bool())
      val csrWrite = Flipped(new DistributedCSRIO)
    })
    walker.module.io.hartId := 0.U
    walker.module.io.csr.tlb := 0.U.asTypeOf(walker.module.io.csr.tlb)
    walker.module.io.csr.tlb.satp.mode := 8.U
    walker.module.io.csr.tlb.satp.asid := io.asid
    walker.module.io.csr.tlb.satp.ppn := io.rootPpn
    walker.module.io.csr.tlb.satp.changed := io.satpChanged
    walker.module.io.csr.distribute_csr <> io.csrWrite
    walker.module.io.sfence := 0.U.asTypeOf(walker.module.io.sfence)
    walker.module.io.sfence.valid := io.flush
    walker.module.io.sfence.bits.rs1 := true.B
    walker.module.io.sfence.bits.rs2 := true.B
    walker.module.io.wfi.wfiReq := false.B
    io.request.ready := VecInit(walker.module.io.tlb.map(_.req(0).ready))(io.request.bits.source)
    for ((port, i) <- walker.module.io.tlb.zipWithIndex) {
      port.req(0).valid := io.request.valid && io.request.bits.source === i.U
      port.req(0).bits := 0.U.asTypeOf(port.req(0).bits)
      port.req(0).bits.vpn := io.request.bits.vpn
      port.resp.ready := io.acceptResponse
      io.response(i).valid := port.resp.valid
      io.response(i).bits.vpn := Cat(port.resp.bits.s1.entry.tag, port.resp.bits.s1.addr_low)
      io.response(i).bits.ppn := Cat(port.resp.bits.s1.entry.ppn,
        port.resp.bits.s1.ppn_low(port.resp.bits.s1.addr_low))
      io.response(i).bits.level := port.resp.bits.s1.entry.level.get
      io.response(i).bits.pf := port.resp.bits.s1.pf
      io.response(i).bits.af := port.resp.bits.s1.af
    }
    io.memRequest.valid := bus.a.valid
    io.memRequest.bits.address := bus.a.bits.address
    io.memRequest.bits.id := bus.a.bits.source
    bus.a.ready := io.memRequest.ready
    bus.d.valid := io.memResponse.valid
    bus.d.bits := 0.U.asTypeOf(bus.d.bits)
    bus.d.bits.opcode := TLMessages.AccessAckData
    bus.d.bits.size := 6.U
    bus.d.bits.source := io.memResponse.bits.id
    bus.d.bits.data := io.memResponse.bits.data
    io.memResponse.ready := bus.d.ready
    bus.b.valid := false.B
    bus.b.bits := 0.U.asTypeOf(bus.b.bits)
    bus.c.ready := true.B
    bus.e.ready := true.B
  }
}

object PtwParallelTest extends App with ChiselSim {
  val base = new DefaultConfig
  implicit val p: Parameters = base.alterPartial {
    case XSCoreParamsKey => base(XSTileKey).head.copy(
      l2tlbParameters = base(XSTileKey).head.l2tlbParameters.copy(enablePrefetch = false))
    case DebugOptionsKey => base(DebugOptionsKey).copy(EnableDifftest = false,
      AlwaysBasicDiff = false, EnablePerfDebug = false)
    case DFTOptionsKey => base(DFTOptionsKey).copy(EnableMbist = false)
    case PerfCounterOptionsKey => PerfCounterOptions(false, false, XSPerfLevel.NORMAL, 0)
    case LogUtilsOptionsKey => LogUtilsOptions(false, false, false, enableXMR = false)
  }
  ChiselDB.init(false)
  Constantin.init(false)
  simulate(LazyModule(new PtwParallelHarness).module) { dut =>
    case class Get(address: BigInt, id: BigInt)
    case class Response(vpn: BigInt, ppn: BigInt, level: BigInt, pf: Boolean, af: Boolean)
    val gets = ArrayBuffer.empty[Get]
    val responses = ArrayBuffer.empty[Response]
    dut.io.request.valid.poke(false.B)
    dut.io.request.bits.vpn.poke(0.U)
    dut.io.request.bits.source.poke(1.U)
    dut.io.acceptResponse.poke(true.B)
    dut.io.memRequest.ready.poke(true.B)
    dut.io.memResponse.valid.poke(false.B)
    dut.io.memResponse.bits.data.poke(0.U)
    dut.io.memResponse.bits.id.poke(0.U)
    dut.io.flush.poke(false.B)
    dut.io.rootPpn.poke("h80000".U)
    dut.io.asid.poke(1.U)
    dut.io.satpChanged.poke(false.B)
    dut.io.csrWrite.w.valid.poke(false.B)
    dut.io.csrWrite.w.bits.addr.poke(0.U)
    dut.io.csrWrite.w.bits.data.poke(0.U)

    def tick(cycles: Int = 1): Unit = for (_ <- 0 until cycles) {
      if (dut.io.memRequest.valid.peek().litToBoolean)
        gets += Get(dut.io.memRequest.bits.address.peek().litValue, dut.io.memRequest.bits.id.peek().litValue)
      if (dut.io.acceptResponse.peek().litToBoolean) {
        for (r <- dut.io.response if r.valid.peek().litToBoolean)
          responses += Response(r.bits.vpn.peek().litValue, r.bits.ppn.peek().litValue,
            r.bits.level.peek().litValue, r.bits.pf.peek().litToBoolean, r.bits.af.peek().litToBoolean)
      }
      dut.clock.step()
    }
    def until(condition: => Boolean): Unit = {
      var remaining = 400
      while (!condition && remaining > 0) { tick(); remaining -= 1 }
      assert(condition, s"timeout: gets=$gets responses=$responses")
    }
    def send(vpn: BigInt): Unit = {
      dut.io.request.bits.vpn.poke(vpn.U)
      dut.io.request.valid.poke(true.B)
      until(dut.io.request.ready.peek().litToBoolean)
      tick()
      dut.io.request.valid.poke(false.B)
    }
    def nextGet(): Get = { until(gets.nonEmpty); gets.remove(0) }
    def pte(ppn: BigInt, perm: Int = 0xcf): BigInt = (ppn << 10) | perm
    def reply(get: Get, value: BigInt): Unit = {
      val line = (0 until 8).foldLeft(BigInt(0))((a, i) => a | (value << (i * 64)))
      for (beat <- 0 until 2) {
        dut.io.memResponse.bits.id.poke(get.id.U)
        dut.io.memResponse.bits.data.poke(((line >> (beat * 256)) & ((BigInt(1) << 256) - 1)).U)
        dut.io.memResponse.valid.poke(true.B)
        until(dut.io.memResponse.ready.peek().litToBoolean)
        tick()
      }
      dut.io.memResponse.valid.poke(false.B)
    }
    def csr(address: Int, data: BigInt): Unit = {
      dut.io.csrWrite.w.valid.poke(true.B)
      dut.io.csrWrite.w.bits.addr.poke(address.U)
      dut.io.csrWrite.w.bits.data.poke(data.U)
      tick()
      dut.io.csrWrite.w.valid.poke(false.B)
      tick(2)
    }
    def expect(vpn: BigInt, ppn: BigInt, level: Int): Unit = {
      until(responses.exists(_.vpn == vpn))
      val r = responses.remove(responses.indexWhere(_.vpn == vpn))
      assert(r == Response(vpn, ppn, level, false, false), r.toString)
    }
    tick(8)
    csr(0x3b0, (BigInt(1) << 46) - 1)
    csr(0x3a0, 0x1f)

    // Prime the upper pointer, then require two memory requests before either
    // one returns. A same-page queued request must not hide the independent one.
    send(0x44000)
    val root = nextGet(); assert(root.address == BigInt("80000000", 16))
    reply(root, pte(0x80001, 1))
    reply(nextGet(), pte(0xb0000))
    expect(0x44000, 0xb0000, 1)
    send(0x40000); val a = nextGet()
    send(0x40001); send(0x41000); val b = nextGet()
    assert(a.id != b.id && a.address == BigInt("80001000", 16) &&
      b.address == BigInt("80001040", 16), s"$a $b")
    dut.io.acceptResponse.poke(false.B)
    reply(b, pte(0xa0000)); tick(5)
    reply(a, pte(0x90000)); tick(5)
    dut.io.acceptResponse.poke(true.B)
    expect(0x40000, 0x90000, 1)
    expect(0x40001, 0x90000, 1)
    expect(0x41000, 0xa0000, 1)
    assert(gets.isEmpty, s"duplicate same-page walk: $gets")

    // The second walker must accept a legal non-leaf and recheck PTC to reach
    // LLPTW, rather than reporting a fault or treating it as a 2 MiB mapping.
    send(0x43000); val hold = nextGet()
    send(0x42000); val pointer = nextGet(); assert(pointer.id == b.id)
    reply(pointer, pte(0x80002, 1))
    val leaf = nextGet(); assert(leaf.address == BigInt("80002000", 16) && leaf.id < 6)
    reply(leaf, pte(0xc0000)); reply(hold, pte(0xd0000))
    expect(0x42000, 0xc0000, 0)
    expect(0x43000, 0xd0000, 1)

    send(0x45000); val faultHold = nextGet()
    send(0x46000); val fault = nextGet(); assert(fault.id == b.id)
    reply(fault, 0)
    until(responses.exists(r => r.vpn == 0x46000 && r.pf && !r.af))
    responses.clear()
    reply(faultHold, pte(0xe0000)); expect(0x45000, 0xe0000, 1)

    // Check the second lane's independent PMP path before any memory access.
    send(0x4a000); val pmpHold = nextGet()
    csr(0x3a0, 0)
    send(0x4b000)
    until(responses.exists(r => r.vpn == 0x4b000 && r.af && !r.pf))
    responses.clear()
    assert(gets.isEmpty, "PMP-denied request reached memory")
    csr(0x3a0, 0x1f)
    reply(pmpHold, pte(0x120000)); expect(0x4a000, 0x120000, 1)

    // Flush both outstanding IDs, accept a new walk before returning the old
    // data, and prove old PTEs never produce a response or a cache refill.
    send(0x47000); val oldA = nextGet()
    send(0x48000); val oldB = nextGet()
    dut.io.flush.poke(true.B); tick(); dut.io.flush.poke(false.B); tick(8)
    responses.clear()
    send(0x49000); tick(12)
    assert(gets.isEmpty, "memory ID reused before the old return")
    reply(oldB, pte(0xf0000)); reply(oldA, pte(0xf0000)); tick(8)
    assert(responses.isEmpty, s"stale response after SFENCE: $responses")
    val newRoot = nextGet(); assert(newRoot.address == root.address)
    reply(newRoot, pte(0x80001, 1))
    reply(nextGet(), pte(0x100000)); expect(0x49000, 0x100000, 1)
    tick(20)
    assert(gets.isEmpty && responses.isEmpty)

    // Switch to a fresh ASID: SATP writes alone need not invalidate PTC entries.
    // Both in-flight lanes must flush independently of SFENCE and drop late PTEs.
    send(0x4c000); val oldRootA = nextGet()
    send(0x4d000); val oldRootB = nextGet()
    assert(oldRootA.id != oldRootB.id)
    dut.io.rootPpn.poke("h80002".U)
    dut.io.asid.poke(2.U)
    dut.io.satpChanged.poke(true.B); tick()
    dut.io.satpChanged.poke(false.B); tick(8)
    responses.clear()
    send(0x4e000); tick(12)
    assert(gets.isEmpty, "memory ID reused before the pre-SATP return")
    reply(oldRootB, pte(0xf0000)); reply(oldRootA, pte(0xf0000)); tick(8)
    assert(responses.isEmpty, s"stale response after SATP change: $responses")
    val changedRoot = nextGet()
    assert(changedRoot.address == BigInt("80002000", 16), changedRoot.toString)
    reply(changedRoot, pte(0x80003, 1))
    reply(nextGet(), pte(0x140000)); expect(0x4e000, 0x140000, 1)
    tick(20)
    assert(gets.isEmpty && responses.isEmpty)
    println("PTW_PARALLEL_PASS: overlap, reordering, same-page queue, backpressure, nonleaf/4KiB, PF, PMP/AF, SFENCE/SATP late returns")
  }
}
