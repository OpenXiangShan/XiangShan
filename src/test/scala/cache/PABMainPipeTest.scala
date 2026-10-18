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

class PABMainPipeTop(implicit p: Parameters) extends DCacheModule {
  Constantin.init(false); ChiselDB.init(false)
  val io = IO(new Bundle {
    val submit, commit, eligible, reuse = Input(Bool())
    val error = Input(UInt(2.W))
    val ready, pending = Output(Bool())
    val refill = Output(Valid(new PABRefillObservation))
  })
  val mp = Module(new MainPipe)
  val addr = "h80001000".U(PAddrBits.W)
  def idle[T <: Data](port: DecoupledIO[T]): Unit = {
    port.valid := false.B; port.bits := 0.U.asTypeOf(port.bits)
  }
  idle(mp.io.probe_req); idle(mp.io.atomic_req); idle(mp.io.prefetch_req); idle(mp.io.pseudo_error)
  mp.io.store_req.valid := false.B; mp.io.store_req.bits := 0.U.asTypeOf(mp.io.store_req.bits)
  mp.io.refill_req.valid := io.submit
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
  mp.io.extra_meta_resp := 0.U.asTypeOf(mp.io.extra_meta_resp)
  mp.io.meta_write.ready := true.B; mp.io.error_flag_write.ready := true.B
  mp.io.prefetch_flag_write.ready := true.B; mp.io.access_flag_write.ready := true.B
  mp.io.latency_flag_write.ready := true.B
  mp.io.tag_read.ready := true.B; mp.io.tag_resp.foreach(_ := 0.U)
  mp.io.tag_write.ready := io.commit; mp.io.tag_write_ready_dup.foreach(_ := io.commit)
  mp.io.replace_way.way := 0.U; mp.io.btot_ways_for_set := 0.U
  mp.io.replace.block := false.B; mp.io.sms_agt_evict_req.ready := true.B
  mp.io.invalid_resv_set := false.B; mp.io.force_write := true.B
  io.ready := mp.io.refill_req.ready; io.pending := mp.io.tag_write_intend
  io.refill := mp.io.pabRefill
}

class PABMainPipeTest extends AnyFlatSpec with ChiselSim {
  private val base = new DefaultConfig
  implicit private val p: Parameters = base.alterPartial {
    case XSCoreParamsKey => base(XSTileKey).head
    case PerfCounterOptionsKey => PerfCounterOptions(false, false, XSPerfLevel.VERBOSE, 0)
    case LogUtilsOptionsKey => LogUtilsOptions(false, false, false, false)
  }
  private def issue(c: PABMainPipeTop, eligible: Boolean = true, error: Int = 0): Unit = {
    c.io.submit.poke(false.B); c.io.commit.poke(false.B); c.io.eligible.poke(eligible.B)
    c.io.reuse.poke(false.B); c.io.error.poke(error.U)
    c.reset.poke(true.B); c.clock.step(3); c.reset.poke(false.B)
    c.io.submit.poke(true.B)
    var wait = 0
    while (!c.io.ready.peek().litToBoolean && wait < 12) { c.clock.step(); wait += 1 }
    c.io.ready.expect(true.B); c.clock.step(); c.io.submit.poke(false.B)
    wait = 0
    while (!c.io.pending.peek().litToBoolean && wait < 12) { c.clock.step(); wait += 1 }
    c.io.pending.expect(true.B); c.io.refill.valid.expect(false.B)
  }
  behavior of "Actual MainPipe PAB installation observation"
  it should "hold source across S3 backpressure and MQ retirement or reuse and publish once" in {
    simulate(new PABMainPipeTop) { c =>
      issue(c)
      c.io.eligible.poke(false.B); c.io.reuse.poke(true.B); c.clock.step(4)
      c.io.pending.expect(true.B); c.io.refill.valid.expect(false.B)
      c.io.commit.poke(true.B); c.io.refill.valid.expect(true.B)
      c.io.refill.bits.source.expect(3.U); c.io.refill.bits.blockAddr.expect((BigInt("80001000", 16) >> 6).U)
      c.clock.step(); c.io.refill.valid.expect(false.B); c.clock.step(3); c.io.refill.valid.expect(false.B)
    }
  }
  it should "exclude consumed origins and denied or corrupt refills at actual completion" in {
    simulate(new PABMainPipeTop) { c =>
      for ((eligible, error) <- Seq((false, 0), (true, 1), (true, 2))) {
        issue(c, eligible, error)
        c.io.eligible.poke(true.B); c.io.commit.poke(true.B)
        c.io.refill.valid.expect(false.B); c.clock.step()
      }
    }
  }
}
