package cache

import chisel3._
import chisel3.util._
import chisel3.simulator.scalatest.ChiselSim
import freechips.rocketchip.tilelink.TLPermissions
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import top.DefaultConfig
import utility._
import xiangshan.{XSCoreParamsKey, XSTileKey}
import xiangshan.cache._

class DCacheVictimMainPipeTop(implicit p: Parameters) extends DCacheModule {
  Constantin.init(false); ChiselDB.init(false)
  val io = IO(new Bundle {
    val submit, commit, eligible, reuse = Input(Bool())
    val victimPresent, victimUsed, victimDirty, hitVictim, sameLine, probe = Input(Bool())
    val victimSource = Input(UInt(3.W))
    val victim = Output(Valid(new PABLine(PAddrBits - blockOffBits)))
    val install = Output(Valid(UInt((PAddrBits - blockOffBits).W)))
    val inserted = Output(Bool())
    val error = Input(UInt(2.W))
    val ready, pending = Output(Bool())
    val refill = Output(Valid(new PABRefillObservation))
  })
  val mp = Module(new MainPipe)
  val oldAddr = "h90001000".U(PAddrBits.W)
  val addr = Mux(io.sameLine, oldAddr, "h80001000".U(PAddrBits.W))
  def idle[T <: Data](port: DecoupledIO[T]): Unit = {
    port.valid := false.B; port.bits := 0.U.asTypeOf(port.bits)
  }
  idle(mp.io.probe_req); idle(mp.io.atomic_req); idle(mp.io.prefetch_req); idle(mp.io.pseudo_error)
  mp.io.store_req.valid := false.B; mp.io.store_req.bits := 0.U.asTypeOf(mp.io.store_req.bits)
  mp.io.probe_req.valid := io.submit && io.probe
  mp.io.probe_req.bits.probe := true.B
  mp.io.probe_req.bits.addr := oldAddr; mp.io.probe_req.bits.vaddr := oldAddr
  mp.io.probe_req.bits.probe_param := TLPermissions.toN
  mp.io.refill_req.valid := io.submit && !io.probe
  mp.io.refill_req.bits := 0.U.asTypeOf(mp.io.refill_req.bits)
  mp.io.refill_req.bits.miss := true.B
  mp.io.refill_req.bits.source := DCACHE_PREFETCH_SOURCE.U
  mp.io.refill_req.bits.cmd := MemoryOpConstants.M_PFR
  mp.io.refill_req.bits.addr := addr; mp.io.refill_req.bits.vaddr := addr
  mp.io.refill_req.bits.pf_source := 3.U
  mp.io.refill_info := 0.U.asTypeOf(mp.io.refill_info)
  mp.io.refill_info.valid := true.B
  mp.io.refill_info.bits.store_mask := Fill(cfg.blockBytes, 1.U(1.W))
  mp.io.refill_info.bits.store_data := Fill(cfg.blockBytes, "ha5".U(8.W))
  mp.io.refill_info.bits.miss_param := TLPermissions.toB
  mp.io.refill_info.bits.error.tl_denied := io.error(0)
  mp.io.refill_info.bits.error.tl_corrupt := io.error(1)
  mp.io.victimDemandHits := 0.U.asTypeOf(mp.io.victimDemandHits)
  mp.io.victimDemandHits(0).valid := io.hitVictim
  mp.io.victimDemandHits(0).bits := oldAddr(PAddrBits - 1, blockOffBits)
  mp.io.pabOrigins := 0.U.asTypeOf(mp.io.pabOrigins)
  mp.io.pabOrigins(0).eligible := io.eligible
  mp.io.pabOrigins(0).source := Mux(io.reuse, 2.U, 3.U)
  mp.io.pabOrigins(0).blockAddr := (addr >> blockOffBits) + io.reuse.asUInt
  mp.io.miss_req.ready := true.B; mp.io.miss_resp := 0.U.asTypeOf(mp.io.miss_resp)
  mp.io.wbq_block_miss_req := false.B
  mp.io.wb.ready := true.B; mp.io.wb_ready_dup.foreach(_ := true.B)
  mp.io.data_read.foreach(_ := false.B); mp.io.data_readline.ready := true.B
  mp.io.data_resp := 0.U.asTypeOf(mp.io.data_resp)
  mp.io.readline_error := false.B; mp.io.readline_error_delayed := false.B
  mp.io.data_write.ready := io.commit; mp.io.data_write_ready_dup.foreach(_ := io.commit)
  mp.io.meta_read.ready := true.B; mp.io.meta_resp := 0.U.asTypeOf(mp.io.meta_resp)
  mp.io.meta_resp(0).coh.state := Mux(io.victimPresent, Mux(io.victimDirty, freechips.rocketchip.tilelink.ClientStates.Dirty, freechips.rocketchip.tilelink.ClientStates.Branch), freechips.rocketchip.tilelink.ClientStates.Nothing)
  mp.io.extra_meta_resp := 0.U.asTypeOf(mp.io.extra_meta_resp)
  mp.io.extra_meta_resp(0).prefetch := io.victimSource
  mp.io.extra_meta_resp(0).access := io.victimUsed
  mp.io.meta_write.ready := true.B; mp.io.error_flag_write.ready := true.B
  mp.io.prefetch_flag_write.ready := true.B; mp.io.access_flag_write.ready := true.B
  mp.io.latency_flag_write.ready := true.B
  mp.io.tag_read.ready := true.B; mp.io.tag_resp.foreach(_ := 0.U)
  mp.io.tag_resp(0) := dcacheParameters.tagCode.encode(get_tag(oldAddr))
  mp.io.tag_write.ready := io.commit; mp.io.tag_write_ready_dup.foreach(_ := io.commit)
  mp.io.replace_way.way := 0.U; mp.io.btot_ways_for_set := 0.U
  mp.io.replace.block := false.B; mp.io.sms_agt_evict_req.ready := true.B
  mp.io.invalid_resv_set := false.B; mp.io.force_write := true.B
  io.ready := Mux(io.probe, mp.io.probe_req.ready, mp.io.refill_req.ready); io.pending := mp.io.tag_write_intend
  io.refill := mp.io.pabRefill
  io.victim := mp.io.dcacheVictim; io.install := mp.io.dcacheInstall
  val history = Module(new DCacheVictimTable(8, PAddrBits - blockOffBits))
  history.io.victim := mp.io.dcacheVictim; history.io.refill := mp.io.dcacheInstall
  history.io.demand := 0.U.asTypeOf(history.io.demand)
  io.inserted := history.io.inserted
}

class DCacheVictimMainPipeTest extends AnyFlatSpec with ChiselSim {
  private val base = new DefaultConfig
  implicit private val p: Parameters = base.alterPartial {
    case XSCoreParamsKey => base(XSTileKey).head
    case PerfCounterOptionsKey => PerfCounterOptions(false, false, XSPerfLevel.VERBOSE, 0)
    case LogUtilsOptionsKey => LogUtilsOptions(false, false, false, false)
  }
  private def issue(c: DCacheVictimMainPipeTop, present: Boolean = true, used: Boolean = false,
                    source: Int = 3, same: Boolean = false, dirty: Boolean = false, earlyUse: Int = -1): Unit = {
    c.io.submit.poke(false.B); c.io.commit.poke(false.B); c.io.eligible.poke(true.B)
    c.io.reuse.poke(false.B); c.io.error.poke(0.U); c.io.probe.poke(false.B)
    c.io.victimPresent.poke(present.B); c.io.victimUsed.poke(used.B); c.io.victimDirty.poke(dirty.B)
    c.io.victimSource.poke(source.U); c.io.hitVictim.poke(false.B); c.io.sameLine.poke(same.B)
    c.reset.poke(true.B); c.clock.step(3); c.reset.poke(false.B)
    if (earlyUse >= 0) {
      c.io.hitVictim.poke(true.B); c.clock.step(); c.io.hitVictim.poke(false.B)
      c.clock.step(earlyUse)
    }
    c.io.submit.poke(true.B)
    var wait = 0
    while (!c.io.ready.peek().litToBoolean && wait < 12) { c.clock.step(); wait += 1 }
    c.io.ready.expect(true.B); c.clock.step(); c.io.submit.poke(false.B)
    wait = 0
    while (!c.io.pending.peek().litToBoolean && wait < 12) { c.clock.step(); wait += 1 }
    c.io.pending.expect(true.B); c.io.victim.valid.expect(false.B)
  }
  behavior of "Actual DCache eviction observation"
  it should "publish the clean or dirty OLD block only at completion and admit only unused Stream victims" in {
    simulate(new DCacheVictimMainPipeTop) { c =>
      for ((present, used, source, same, dirty) <- Seq((true, false, 3, false, false),
        (true, false, 3, false, true), (true, true, 3, false, false),
        (true, false, 2, false, false), (true, false, 0, false, false),
        (false, false, 3, false, false), (true, false, 3, true, false))) {
        issue(c, present, used, source, same, dirty)
        c.clock.step(4); c.io.victim.valid.expect(false.B)
        c.io.commit.poke(true.B)
        val evict = present && !same
        c.io.victim.valid.expect(evict.B)
        if (evict) {
          c.io.victim.bits.blockAddr.expect((BigInt("90001000", 16) >> 6).U)
          c.io.victim.bits.source.expect(source.U); c.io.victim.bits.used.expect(used.B)
        }
        c.io.install.valid.expect(true.B)
        c.clock.step(); c.io.victim.valid.expect(false.B); c.clock.step()
        c.io.inserted.expect((evict && !used && source == 3).B)
        c.clock.step(); c.io.inserted.expect(false.B)
      }
    }
  }
  it should "remember a demand hit while the replacement waits in S3" in {
    simulate(new DCacheVictimMainPipeTop) { c =>
      issue(c)
      c.io.hitVictim.poke(true.B); c.clock.step(); c.io.hitVictim.poke(false.B)
      c.clock.step(4); c.io.commit.poke(true.B)
      c.io.victim.valid.expect(true.B); c.io.victim.bits.used.expect(true.B)
      c.clock.step(2); c.io.inserted.expect(false.B)
    }
  }
  it should "include first use on the eviction edge and exclude probe invalidations" in {
    simulate(new DCacheVictimMainPipeTop) { c =>
      issue(c)
      c.io.hitVictim.poke(true.B); c.io.commit.poke(true.B)
      c.io.victim.valid.expect(true.B); c.io.victim.bits.used.expect(true.B)
      c.clock.step(); c.io.hitVictim.poke(false.B); c.clock.step(); c.io.inserted.expect(false.B)
      c.io.submit.poke(false.B); c.io.commit.poke(true.B); c.io.probe.poke(true.B)
      c.reset.poke(true.B); c.clock.step(3); c.reset.poke(false.B)
      c.io.submit.poke(true.B)
      var wait = 0
      while (!c.io.ready.peek().litToBoolean && wait < 12) { c.clock.step(); wait += 1 }
      c.io.ready.expect(true.B); c.clock.step(); c.io.submit.poke(false.B)
      for (_ <- 0 until 12) {
        c.io.victim.valid.expect(false.B); c.io.inserted.expect(false.B); c.clock.step()
      }
    }
  }

  it should "cover pending access metadata updates before the victim read" in {
    simulate(new DCacheVictimMainPipeTop) { c =>
      issue(c, earlyUse = 0)
      c.io.commit.poke(true.B); c.io.victim.valid.expect(true.B); c.io.victim.bits.used.expect(true.B)
      c.clock.step(2); c.io.inserted.expect(false.B)
    }
  }

}
