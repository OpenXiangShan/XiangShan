package cache

import chisel3._
import chisel3.experimental.UnlocatableSourceInfo
import chisel3.simulator.scalatest.ChiselSim
import freechips.rocketchip.diplomacy.{AddressSet, IdRange, RegionType, TransferSizes}
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import top.DefaultConfig
import utility._
import xiangshan.{XSCoreParamsKey, XSTileKey}
import xiangshan.cache._

class PDBLateMissQueueTop(implicit p: Parameters) extends DCacheModule {
  Constantin.init(false); ChiselDB.init(false)
  val io = IO(new Bundle {
    val valid, cancel, blocked = Input(Vec(4, Bool()))
    // 0: Load, 1: Stream PF, 2: another PF source, 3: Store.
    val kind = Input(Vec(4, UInt(2.W)))
    val address = Input(Vec(4, UInt(PAddrBits.W)))
    val ready, handled = Output(Vec(4, Bool()))
    val late = Output(UInt(4.W))
  })
  val edge = new TLEdgeOut(
    TLMasterPortParameters.v1(Seq(TLMasterParameters.v1(
      "pdb-late-test", sourceId = IdRange(0, cfg.nMissEntries), supportsProbe = TransferSizes(64, 64)))),
    TLSlavePortParameters.v1(Seq(TLSlaveParameters.v1(
      address = Seq(AddressSet(0, (BigInt(1) << PAddrBits) - 1)), regionType = RegionType.TRACKED,
      supportsAcquireB = TransferSizes(64, 64), supportsAcquireT = TransferSizes(64, 64))),
      beatBytes = beatBytes, endSinkId = 1), p, UnlocatableSourceInfo)
  val mq = Module(new MissQueue(edge, 4))
  for (i <- 0 until 4) {
    val req = mq.io.queryMQ(i).req
    val pf = io.kind(i) === 1.U || io.kind(i) === 2.U
    req.valid := io.valid(i)
    req.bits := 0.U.asTypeOf(req.bits)
    req.bits.source := Mux(pf, DCACHE_PREFETCH_SOURCE.U,
      Mux(io.kind(i) === 3.U, STORE_SOURCE.U, LOAD_SOURCE.U))
    req.bits.cmd := Mux(pf, MemoryOpConstants.M_PFR,
      Mux(io.kind(i) === 3.U, MemoryOpConstants.M_XWR, MemoryOpConstants.M_XRD))
    req.bits.pf_source := Mux(io.kind(i) === 1.U, 3.U, 1.U)
    req.bits.pbEligible := pf
    req.bits.addr := io.address(i)
    req.bits.vaddr := io.address(i)
    req.bits.cancel := io.cancel(i)
    mq.io.wbq_block_miss_req(i) := io.blocked(i)
    io.ready(i) := mq.io.queryMQ(i).ready
    io.handled(i) := mq.io.resp(i).handled
  }
  mq.io.hartId := 0.U
  mq.io.pb.allocReq.ready := false.B
  mq.io.pb.allocEntryId := 0.U
  mq.io.pb.refillReq.ready := false.B
  mq.io.pb.status := 0.U.asTypeOf(mq.io.pb.status)
  mq.io.cmo_req.valid := false.B; mq.io.cmo_req.bits := 0.U.asTypeOf(mq.io.cmo_req.bits)
  mq.io.cmo_resp.ready := true.B
  mq.io.mem_acquire.foreach(_.ready := true.B)
  mq.io.mem_grant.foreach { g => g.valid := false.B; g.bits := 0.U.asTypeOf(g.bits) }
  mq.io.mem_finish.foreach(_.ready := true.B)
  mq.io.l2_hint := 0.U.asTypeOf(mq.io.l2_hint)
  mq.io.main_pipe_req.ready := false.B
  mq.io.main_pipe_resp := 0.U.asTypeOf(mq.io.main_pipe_resp)
  mq.io.mainpipe_info := 0.U.asTypeOf(mq.io.mainpipe_info)
  mq.io.probe.req := 0.U.asTypeOf(mq.io.probe.req)
  mq.io.replace.req := 0.U.asTypeOf(mq.io.replace.req)
  mq.io.evict_set := 0.U
  mq.io.occupy_set.foreach(_ := 0.U)
  mq.io.forward.foreach { f =>
    f.s0Req := 0.U.asTypeOf(f.s0Req); f.s1Req := 0.U.asTypeOf(f.s1Req); f.s1Kill := false.B
  }
  mq.io.forward_stData := 0.U.asTypeOf(mq.io.forward_stData)
  mq.io.l2_pf_store_only := false.B; mq.io.lqEmpty := true.B
  mq.io.wfi.wfiReq := false.B
  mq.io.debugTopDown.robHeadVaddr := 0.U.asTypeOf(mq.io.debugTopDown.robHeadVaddr)
  mq.io.debugTopDown.robHeadOtherReplay := false.B
  io.late := mq.io.pdbLate
}

class PDBLateMissQueueTest extends AnyFlatSpec with ChiselSim {
  private val base = new DefaultConfig
  implicit private val p: Parameters = base.alterPartial {
    case XSCoreParamsKey => base(XSTileKey).head
    case PerfCounterOptionsKey => PerfCounterOptions(false, false, XSPerfLevel.VERBOSE, 0)
    case LogUtilsOptionsKey => LogUtilsOptions(false, false, false, false)
  }
  private val address = BigInt("80001000", 16)
  private def clear(c: PDBLateMissQueueTop): Unit = {
    for (i <- 0 until 4) {
      c.io.valid(i).poke(false.B); c.io.cancel(i).poke(false.B); c.io.blocked(i).poke(false.B)
      c.io.kind(i).poke(0.U); c.io.address(i).poke(address.U)
    }
  }
  private def reset(c: PDBLateMissQueueTop): Unit = {
    clear(c); c.reset.poke(true.B); c.clock.step(3); c.reset.poke(false.B); c.clock.step(2)
  }
  private def request(c: PDBLateMissQueueTop, lane: Int, addr: BigInt, kind: Int): Unit = {
    c.io.valid(lane).poke(true.B); c.io.address(lane).poke(addr.U); c.io.kind(lane).poke(kind.U)
  }

  behavior of "Real MissQueue PDB late evidence"
  it should "handle pipe and entry merges, cancellations, compressed lanes, sources and full drops" in {
    simulate(new PDBLateMissQueueTop) { c =>
      reset(c)
      request(c, 0, address, 1); c.io.handled(0).expect(true.B); c.io.late.expect(0.U)
      c.clock.step(); clear(c)
      // Hit the allocation pipeline before the MSHR entry has become valid.
      request(c, 1, address, 0); request(c, 2, address, 0)
      c.io.handled(1).expect(true.B); c.io.handled(2).expect(true.B); c.io.late.expect(1.U)
      c.clock.step(); c.io.late.expect(0.U); clear(c); c.clock.step()
      request(c, 0, address, 1)
      c.io.ready(0).expect(false.B); c.io.late.expect(1.U)
      c.clock.step(); clear(c)
      request(c, 0, address, 2); c.io.late.expect(0.U)
      clear(c); request(c, 0, address, 1); c.io.cancel(0).poke(true.B); c.io.late.expect(0.U)

      reset(c)
      request(c, 0, address, 1); c.clock.step(); clear(c); c.clock.step(2)
      request(c, 1, address, 0); c.io.cancel(1).poke(true.B)
      c.io.handled(1).expect(false.B); c.io.late.expect(0.U); c.clock.step()
      c.io.cancel(1).poke(false.B); c.io.blocked(1).poke(true.B)
      c.io.handled(1).expect(false.B); c.io.late.expect(0.U); c.clock.step()
      // Legacy raw-query flags have already been cleared, but H3 must retain
      // the original Stream lifetime until a demand is actually accepted.
      c.io.blocked(1).poke(false.B)
      c.io.handled(1).expect(true.B); c.io.late.expect(1.U); c.clock.step()
      c.io.late.expect(0.U)

      reset(c)
      request(c, 0, address, 1); request(c, 1, address, 0); request(c, 2, address, 0)
      c.io.handled(0).expect(true.B); c.io.handled(1).expect(true.B)
      c.io.late.expect(1.U); c.clock.step(); clear(c); c.clock.step(2)
      request(c, 1, address, 0); c.io.late.expect(0.U)

      reset(c)
      for (i <- 0 until 3) {
        request(c, 0, address + i * 64, 1)
        c.io.handled(0).expect(true.B); c.clock.step(); clear(c); c.clock.step(2)
      }
      for (i <- 0 until 3) request(c, i + 1, address + i * 64, 0)
      for (i <- 1 until 4) c.io.handled(i).expect(true.B)
      c.io.late.expect(3.U); c.clock.step(); c.io.late.expect(0.U)

      reset(c)
      request(c, 1, address, 0); c.clock.step(); clear(c)
      request(c, 0, address, 1) // PF->demand in the allocation register.
      c.io.ready(0).expect(false.B); c.io.late.expect(1.U)
      c.clock.step(); clear(c)
      request(c, 0, address, 1) // Same event class in the actual entry.
      c.io.ready(0).expect(false.B); c.io.late.expect(1.U)

      reset(c)
      request(c, 0, address, 2); c.clock.step(); clear(c); c.clock.step(2)
      request(c, 1, address, 0); c.io.handled(1).expect(true.B); c.io.late.expect(0.U)

      reset(c)
      for (i <- 0 until 16) {
        request(c, 1, address + i * 64, 0)
        c.io.handled(1).expect(true.B); c.clock.step(); clear(c); c.clock.step(2)
      }
      request(c, 0, address + 32 * 64, 1)
      c.io.ready(0).expect(false.B); c.io.late.expect(0.U)
    }
  }
}
