package cache

import chisel3._
import chisel3.util._
import chisel3.simulator.scalatest.ChiselSim
import freechips.rocketchip.tilelink.{ClientStates, TLPermissions}
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import top.DefaultConfig
import utility.{Constantin, LogUtilsOptions, LogUtilsOptionsKey, PerfCounterOptions, PerfCounterOptionsKey, XSPerfLevel}
import xiangshan.{XSCoreParamsKey, XSTileKey}
import xiangshan.cache._

class PBMainTestTop(backgroundMove: Boolean = true)(implicit p: Parameters) extends DCacheModule {
  val io = IO(new Bundle {
    val alloc, fill, store, probe, dirty, block, readReady, commit = Input(Bool())
    val corrupt, btot, wbStall, otherStore = Input(Bool())
    val miss = Output(Bool())
    val storeReady, read, write, wb, replay, hit, resident, abort = Output(Bool())
    val probeReady, live, wbValid = Output(Bool())
    val wbData, wbCorrupt = Output(Bool())
    val data = Output(UInt((cfg.blockBytes * 8).W))
    val victim = Output(UInt((cfg.blockBytes * 8).W))
    val coh = Output(UInt(ClientStates.width.W))
  })
  Constantin.init(false)
  val mp = Module(new MainPipe)
  val pb = Module(new PBTestTop(backgroundMove))
  pb.aliasQuery.vaddr := 0.U

  val addr = "h80001000".U(PAddrBits.W)
  val oldAddr = "h90001000".U(PAddrBits.W)
  val bytes = VecInit((0 until cfg.blockBytes).map(_.U(8.W))).asUInt
  val oldBytes = Fill(cfg.blockBytes, "ha5".U(8.W))
  def idle[T <: Data](port: DecoupledIO[T]): Unit = {
    port.valid := false.B
    port.bits := 0.U.asTypeOf(port.bits)
  }
  idle(mp.io.probe_req)
  mp.io.probe_req.valid := io.probe
  mp.io.probe_req.bits.probe := true.B
  mp.io.probe_req.bits.probe_param := TLPermissions.toN
  mp.io.probe_req.bits.addr := addr
  mp.io.probe_req.bits.vaddr := addr
  idle(mp.io.refill_req)
  idle(mp.io.atomic_req)
  idle(mp.io.prefetch_req)
  idle(mp.io.pseudo_error)
  mp.io.miss_req.ready := true.B
  mp.io.miss_resp := 0.U.asTypeOf(mp.io.miss_resp)
  mp.io.wbq_block_miss_req := false.B
  mp.io.refill_info := 0.U.asTypeOf(mp.io.refill_info)
  mp.io.store_req.valid := io.store
  mp.io.store_req.bits := 0.U.asTypeOf(mp.io.store_req.bits)
  val storeAddr = Mux(io.otherStore, addr + 64.U, addr)
  val ordinaryStoreMiss = RegEnable(io.otherStore, mp.io.store_req.fire)
  mp.io.store_req.bits.addr := storeAddr
  mp.io.store_req.bits.vaddr := storeAddr
  mp.io.store_req.bits.cmd := MemoryOpConstants.M_XWR
  mp.io.store_req.bits.mask := Fill(cfg.blockBytes, 1.U(1.W))
  mp.io.store_req.bits.data := Fill(cfg.blockBytes, "hff".U(8.W))
  mp.io.store_req.bits.id := 3.U
  val wbArb = Module(new RRArbiter(new WritebackReq, 2))
  wbArb.io.in(0) <> mp.io.wb
  wbArb.io.in(1) <> pb.io.releaseReq
  wbArb.io.out.ready := io.commit && !io.wbStall
  mp.io.wb_ready_dup.foreach(_ := io.commit && !io.wbStall && wbArb.io.chosen === 0.U)
  mp.io.data_read.foreach(_ := false.B)
  mp.io.data_readline.ready := io.readReady
  mp.io.data_resp := 0.U.asTypeOf(mp.io.data_resp)
  for (i <- 0 until DCacheBanks) {
    mp.io.data_resp(i).raw_data := get_data_of_bank(i, oldBytes)
  }
  mp.io.readline_error := false.B
  mp.io.readline_error_delayed := false.B
  mp.io.data_write.ready := io.commit
  mp.io.data_write_ready_dup.foreach(_ := io.commit)
  mp.io.meta_read.ready := true.B
  mp.io.meta_resp := 0.U.asTypeOf(mp.io.meta_resp)
  mp.io.meta_resp(0).coh.state := Mux(io.dirty, ClientStates.Dirty, ClientStates.Nothing)
  mp.io.extra_meta_resp := 0.U.asTypeOf(mp.io.extra_meta_resp)
  mp.io.meta_write.ready := true.B
  mp.io.error_flag_write.ready := true.B
  mp.io.prefetch_flag_write.ready := true.B
  mp.io.access_flag_write.ready := true.B
  mp.io.latency_flag_write.ready := true.B
  mp.io.tag_read.ready := true.B
  mp.io.tag_resp.foreach(_ := cfg.tagCode.encode(get_tag(oldAddr)))
  mp.io.tag_write.ready := io.commit
  mp.io.tag_write_ready_dup.foreach(_ := io.commit)
  mp.io.replace_way.way := 0.U
  mp.io.btot_ways_for_set := Mux(io.btot, 1.U, 0.U)
  mp.io.replace.block := io.block
  mp.io.sms_agt_evict_req.ready := true.B
  mp.io.invalid_resv_set := false.B
  mp.io.force_write := true.B

  pb.io.pipe <> mp.io.pb
  mp.io.pbOwners := 0.U.asTypeOf(mp.io.pbOwners)
  pb.io.load.foreach { port =>
    port.s1_paddr := 0.U.asTypeOf(port.s1_paddr)
    port.s1_kill := false.B
    port.s2_use := false.B
  }
  pb.io.mshr.allocReq.valid := io.alloc
  pb.io.mshr.allocReq.bits := 0.U.asTypeOf(new PBAlloc)
  pb.io.mshr.allocReq.bits.addr := addr
  pb.io.mshr.allocReq.bits.vaddr := addr
  val entryId = RegEnable(pb.io.mshr.allocEntryId, pb.io.mshr.allocReq.fire)
  pb.io.mshr.cancelReq := 0.U.asTypeOf(pb.io.mshr.cancelReq)
  pb.io.mshr.refillReq.valid := io.fill
  pb.io.mshr.refillReq.bits := 0.U.asTypeOf(new PBRefillReq)
  pb.io.mshr.refillReq.bits.entryId := entryId
  pb.io.mshr.refillReq.bits.data := bytes
  pb.io.mshr.refillReq.bits.corrupt := io.corrupt
  pb.io.mshr.refillReq.bits.coh.state := ClientStates.Trunk
  pb.io.mshr.refillWait := false.B
  pb.io.dcache.wfi.wfiReq := false.B
  pb.io.dcache.usedMove := false.B
  pb.io.dcache.unusedMove := false.B
  pb.io.mshr.preAcquire.foreach(_ := true.B)

  io.miss := mp.io.miss_req.fire
  io.storeReady := mp.io.store_req.ready
  io.probeReady := mp.io.probe_req.ready
  io.live := pb.io.mshr.status.map(_.valid).reduce(_ || _)
  io.wbValid := wbArb.io.out.valid
  io.wbData := wbArb.io.out.bits.hasData
  io.wbCorrupt := wbArb.io.out.bits.corrupt
  io.read := mp.io.data_readline.fire
  io.write := mp.io.data_write.fire
  io.data := mp.io.data_write.bits.data.asUInt
  io.coh := mp.io.meta_write.bits.meta.coh.state
  io.wb := wbArb.io.out.fire
  io.victim := wbArb.io.out.bits.data
  io.replay := mp.io.store_replay_resp.valid
  io.hit := mp.io.store_hit_resp.valid
  io.resident := pb.observed.resident.asUInt.orR
  io.abort := mp.io.pb.s2_moveAbort.valid
  when (io.write) {
    assert(mp.io.tag_write.fire && mp.io.meta_write.fire && pb.io.pipe.s3_moveDone.valid)
    assert(mp.io.data_write.bits.wmask.andR)
    assert(mp.io.data_write_dup.map(_.valid).reduce(_ && _))
  }
  when (io.replay) { assert(mp.io.store_replay_resp.bits.id === 3.U) }
  when (mp.io.pb.s3_probeDone.valid) {
    assert(mp.io.wb.fire && !mp.io.wb.bits.voluntary && !mp.io.wb.bits.hasData)
    assert(mp.io.wb.bits.param === TLPermissions.TtoN && mp.io.wb.bits.addr === addr)
    assert(!mp.io.meta_write.valid && !mp.io.tag_write.valid && !mp.io.data_write.valid)
  }
  when (pb.io.pipe.s1_paddr.fire && pb.io.pipe.s1_storeReq || pb.io.pipe.s0_probeReq.valid) { assert(!pb.io.dcache.wfi.safe) }
  assert((ordinaryStoreMiss || !mp.io.miss_req.valid && !io.hit) && !pb.io.dcache.error.fatal)
}

class PBMainPipeTest extends AnyFlatSpec with ChiselSim {
  behavior of "PB MainPipe promotion"
  private val base = new DefaultConfig
  implicit private val p: Parameters = base.alterPartial {
    case XSCoreParamsKey => base(XSTileKey).head
    case PerfCounterOptionsKey => PerfCounterOptions(false, false, XSPerfLevel.VERBOSE, 0)
    case LogUtilsOptionsKey => LogUtilsOptions(false, false, false, false)
  }
  for (cause <- Seq("none", "replacement", "BtoT")) {
    val abort = cause != "none"
    it should s"move a full-store original line with dirty victim${if (abort) s" after $cause abort" else ""}" in {
      simulate(new PBMainTestTop) { c =>
        c.io.alloc.poke(false.B)
        c.io.fill.poke(false.B)
        c.io.corrupt.poke(false.B)
      c.io.btot.poke(false.B)
      c.io.wbStall.poke(false.B)
      c.io.otherStore.poke(false.B)
        c.io.store.poke(false.B)
        c.io.probe.poke(false.B)
        c.io.dirty.poke(true.B)
        c.io.block.poke((cause == "replacement").B)
        c.io.btot.poke((cause == "BtoT").B)
        c.io.readReady.poke(false.B)
        c.io.commit.poke(false.B)
        c.clock.step(2)
        c.io.alloc.poke(true.B)
        c.clock.step()
        c.io.alloc.poke(false.B)
        c.io.fill.poke(true.B)
        c.clock.step()
        c.io.fill.poke(false.B)
        c.io.resident.expect(true.B)
        c.io.store.poke(true.B)
        c.io.storeReady.expect(true.B)
        c.clock.step()
        c.io.store.poke(false.B)
        c.clock.step(3)
        c.io.resident.expect(true.B) // Store locks only on S1 advancement.
        c.io.write.expect(false.B)
        c.io.readReady.poke(true.B)
        c.io.read.expect(true.B)
        c.clock.step()
        if (abort) {
          c.io.abort.expect(true.B)
          c.io.write.expect(false.B)
          c.clock.step()
          c.io.replay.expect(true.B) // Failed attempt returns its Store credit now.
          c.io.block.poke(false.B)
          c.io.btot.poke(false.B)
        }
        c.clock.step(8)
        c.io.write.expect(false.B)
        c.io.wb.expect(false.B)
        c.io.replay.expect(false.B)
        c.io.commit.poke(true.B)
        c.io.wbStall.poke(true.B)
        for (_ <- 0 until 3) {
          c.io.wbValid.expect(true.B)
          c.io.write.expect(false.B)
          c.io.wb.expect(false.B)
          c.io.live.expect(true.B)
          c.clock.step()
        }
        c.io.wbStall.poke(false.B)
      c.io.otherStore.poke(false.B)
        c.io.write.expect(true.B)
        c.io.wb.expect(true.B)
        val line = (0 until 64).map(b => BigInt(b) << (8 * b)).reduce(_ | _)
        c.io.data.expect(line.U)
        c.io.victim.expect(BigInt("a5" * 64, 16).U)
        c.io.coh.expect(ClientStates.Trunk)
        c.clock.step()
        c.io.replay.expect((!abort).B)
        c.clock.step()
        c.io.replay.expect(false.B)
        c.io.resident.expect(false.B)
      }
    }
  }

  it should "retain Probe ownership through WBQ backpressure without touching cache arrays" in {
    simulate(new PBMainTestTop) { c =>
      c.io.alloc.poke(false.B)
      c.io.fill.poke(false.B)
      c.io.corrupt.poke(false.B)
      c.io.store.poke(false.B)
      c.io.probe.poke(false.B)
      c.io.dirty.poke(true.B)
      c.io.block.poke(false.B)
      c.io.readReady.poke(false.B)
      c.io.commit.poke(false.B)
      c.clock.step(2)
      c.io.alloc.poke(true.B)
      c.clock.step()
      c.io.alloc.poke(false.B)
      c.io.fill.poke(true.B)
      c.clock.step()
      c.io.fill.poke(false.B)
      c.io.resident.expect(true.B)
      c.io.probe.poke(true.B)
      c.io.probeReady.expect(true.B)
      c.clock.step()
      c.io.probe.poke(false.B)
      c.io.resident.expect(false.B)
      for (_ <- 0 until 8) {
        c.io.live.expect(true.B)
        c.io.read.expect(false.B)
        c.io.write.expect(false.B)
        c.io.wb.expect(false.B)
        c.io.replay.expect(false.B)
        c.clock.step()
      }
      c.io.wbValid.expect(true.B)
      c.io.commit.poke(true.B)
      c.io.wb.expect(true.B)
      c.io.write.expect(false.B)
      c.clock.step()
      c.io.live.expect(false.B)
      c.io.wb.expect(false.B)
      c.io.replay.expect(false.B)
    }
  }

  it should "release a poisoned PB line with corrupt data when alwaysReleaseData is enabled" in {
    val core = base(XSTileKey).head
    implicit val p: Parameters = base.alterPartial {
      case XSCoreParamsKey => core.copy(dcacheParametersOpt = core.dcacheParametersOpt.map(_.copy(alwaysReleaseData = true)))
      case PerfCounterOptionsKey => PerfCounterOptions(false, false, XSPerfLevel.VERBOSE, 0)
      case LogUtilsOptionsKey => LogUtilsOptions(false, false, false, false)
    }
    simulate(new PBMainTestTop) { c =>
      c.io.alloc.poke(false.B)
      c.io.fill.poke(false.B)
      c.io.corrupt.poke(true.B)
      c.io.btot.poke(false.B)
      c.io.wbStall.poke(false.B)
      c.io.otherStore.poke(false.B)
      c.io.store.poke(false.B)
      c.io.probe.poke(false.B)
      c.io.dirty.poke(true.B)
      c.io.block.poke(false.B)
      c.io.readReady.poke(false.B)
      c.io.commit.poke(false.B)
      c.clock.step(2)
      c.io.alloc.poke(true.B)
      c.clock.step()
      c.io.alloc.poke(false.B)
      c.io.fill.poke(true.B)
      c.clock.step()
      c.io.fill.poke(false.B)
      for (_ <- 0 until 12) {
        c.io.live.expect(true.B)
        c.io.read.expect(false.B)
        c.io.write.expect(false.B)
        c.io.wb.expect(false.B)
        c.clock.step()
      }
      c.io.wbValid.expect(true.B)
      c.io.wbData.expect(true.B)
      c.io.wbCorrupt.expect(true.B)
      val line = (0 until 64).map(b => BigInt(b) << (8 * b)).reduce(_ | _)
      c.io.victim.expect(line.U)
      c.io.commit.poke(true.B)
      c.io.wb.expect(true.B)
      c.clock.step()
      c.io.live.expect(false.B)
      c.io.replay.expect(false.B)
    }
  }
  it should "send an ordinary Store miss to MissQueue when PB owns a different block" in {
    simulate(new PBMainTestTop) { c =>
      c.io.alloc.poke(false.B); c.io.fill.poke(false.B)
      c.io.corrupt.poke(false.B); c.io.btot.poke(false.B)
      c.io.wbStall.poke(false.B); c.io.otherStore.poke(false.B)
      c.io.store.poke(false.B); c.io.probe.poke(false.B)
      c.io.dirty.poke(false.B); c.io.block.poke(false.B)
      c.io.readReady.poke(true.B); c.io.commit.poke(true.B)
      c.clock.step(2)
      c.io.alloc.poke(true.B); c.clock.step(); c.io.alloc.poke(false.B)
      c.io.fill.poke(true.B); c.clock.step(); c.io.fill.poke(false.B)
      c.io.otherStore.poke(true.B); c.io.store.poke(true.B)
      c.io.storeReady.expect(true.B); c.clock.step()
      c.io.store.poke(false.B); c.io.otherStore.poke(false.B)
      c.clock.step()
      c.io.miss.expect(true.B)
      c.io.write.expect(false.B)
      c.io.resident.expect(true.B)
      c.clock.step(4)
      c.io.replay.expect(false.B)
      c.io.miss.expect(false.B)
      c.io.live.expect(true.B)
    }
  }

}
