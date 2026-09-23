package xiangshan.cache

import chisel3._
import chisel3.util._
import chisel3.simulator.scalatest.ChiselSim
import freechips.rocketchip.tilelink.ClientStates
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import top.DefaultConfig
import utility.{LogUtilsOptions, LogUtilsOptionsKey, PerfCounterOptions, PerfCounterOptionsKey, XSPerfLevel}
import xiangshan.{XSCoreParamsKey, XSTileKey}

class LoadPipeEccTagHarness(implicit p: Parameters) extends DCacheModule {
  val io = IO(new Bundle {
    val request = Input(Bool())
    val paddr = Input(UInt(PAddrBits.W))
    val tagFault = Input(UInt(encTagBits.W))
    val dataCe = Input(Bool())
    val dataUec = Input(Bool())
    val tagEvictValid = Output(Bool())
    val tagEvictAddr = Output(UInt(PAddrBits.W))
    val tagEvictWay = Output(UInt(nWays.W))
    val dataEvictValid = Output(Bool())
    val responseValid = Output(Bool())
    val responseEccReplay = Output(Bool())
    val responseError = Output(Bool())
    val errorValid = Output(Bool())
    val reportToBeu = Output(Bool())
  })

  val pipe = Module(new LoadPipe(0))
  pipe.io.lsu := DontCare
  pipe.io.dwpu := DontCare
  pipe.io.lsu.req.valid := io.request
  pipe.io.lsu.req.bits := 0.U.asTypeOf(pipe.io.lsu.req.bits)
  pipe.io.lsu.req.bits.cmd := MemoryOpConstants.M_XRD
  pipe.io.lsu.req.bits.vaddr := io.paddr
  pipe.io.lsu.req.bits.vaddr_dup := io.paddr
  pipe.io.lsu.req.bits.mask := Fill(VLEN / 8, 1.U(1.W))
  pipe.io.lsu.req.bits.instrtype := LOAD_SOURCE.U
  pipe.io.lsu.req.bits.isFirstIssue := true.B
  pipe.io.lsu.s1_kill := false.B
  pipe.io.lsu.s2_kill := false.B
  pipe.io.lsu.s0_pc := 0.U
  pipe.io.lsu.s1_pc := 0.U
  pipe.io.lsu.s2_pc := 0.U
  pipe.io.lsu.pf_source := 0.U
  pipe.io.lsu.s1_paddr_dup_lsu := io.paddr
  pipe.io.lsu.s1_paddr_dup_dcache := io.paddr
  pipe.io.lsu.resp.ready := true.B

  pipe.io.load128Req := false.B
  pipe.io.meta_read.ready := true.B
  pipe.io.tag_read.ready := true.B
  pipe.io.banked_data_read.ready := true.B
  pipe.io.vtag_update.valid := false.B
  pipe.io.vtag_update.bits := 0.U.asTypeOf(pipe.io.vtag_update.bits)
  pipe.io.miss_req.ready := true.B
  pipe.io.wbq_block_miss_req := false.B
  pipe.io.occupy_fail := false.B
  pipe.io.disable_ld_fast_wakeup := false.B
  pipe.io.bank_conflict_slow := false.B
  pipe.io.rr_bank_conflict_slow := false.B
  pipe.io.tag_evict.ready := true.B
  pipe.io.data_evict.ready := true.B
  pipe.io.access_flag_write.ready := true.B
  pipe.io.prefetch_flag_write.ready := true.B
  pipe.io.latency_flag_write.ready := true.B
  pipe.io.replace_way.way := 0.U
  pipe.io.miss_resp := 0.U.asTypeOf(pipe.io.miss_resp)
  pipe.io.bloom_filter_query.resp := 0.U.asTypeOf(pipe.io.bloom_filter_query.resp)
  pipe.io.counter_filter_query.resp := false.B

  for (way <- 0 until nWays) {
    pipe.io.meta_resp(way).coh.state := (if (way == 0) ClientStates.Trunk else ClientStates.Nothing)
    pipe.io.extra_meta_resp(way) := 0.U.asTypeOf(pipe.io.extra_meta_resp(way))
    val rawTag = if (way == 0) get_tag(io.paddr) else 0.U(tagBits.W)
    pipe.io.tag_resp(way) := cacheParams.tagCode.encode(rawTag) ^ Mux(way.U === 0.U, io.tagFault, 0.U(encTagBits.W))
  }
  for (lane <- 0 until VLEN / DCacheSRAMRowBits) {
    pipe.io.banked_data_resp(lane) := 0.U.asTypeOf(pipe.io.banked_data_resp(lane))
    pipe.io.read_error_delayed(lane) := io.dataCe || io.dataUec
    pipe.io.read_correctable_delayed(lane) := io.dataCe
    pipe.io.read_uncorrectable_delayed(lane) := io.dataUec
  }

  io.tagEvictValid := pipe.io.tag_evict.valid
  io.tagEvictAddr := pipe.io.tag_evict.bits.addr
  io.tagEvictWay := pipe.io.tag_evict.bits.way_en
  io.dataEvictValid := pipe.io.data_evict.valid
  io.responseValid := pipe.io.lsu.resp.valid
  io.responseEccReplay := pipe.io.lsu.resp.bits.ecc_replay_delayed
  io.responseError := pipe.io.lsu.resp.bits.error_delayed
  io.errorValid := pipe.io.error.valid
  io.reportToBeu := pipe.io.error.bits.report_to_beu
}

class LoadPipeEccTest extends AnyFlatSpec with ChiselSim {
  behavior of "LoadPipe ECC"

  it should "emit an evict request for a corrected tag read" in {
    val defaultConfig = new DefaultConfig
    implicit val p = defaultConfig.alterPartial({ case XSCoreParamsKey => defaultConfig(XSTileKey).head.copy() }).alterPartial({ case LogUtilsOptionsKey => LogUtilsOptions(false, false, false, false); case PerfCounterOptionsKey => PerfCounterOptions(false, false, XSPerfLevel.VERBOSE, 0) })
    simulate(new LoadPipeEccTagHarness) { d =>
      val addr = BigInt("80004000", 16)
      d.io.request.poke(false.B)
      d.io.paddr.poke(addr.U)
      d.io.tagFault.poke(1.U)
      d.io.dataCe.poke(false.B)
      d.io.dataUec.poke(false.B)
      d.reset.poke(true.B)
      d.clock.step(2)
      d.reset.poke(false.B)
      d.clock.step()
      d.io.request.poke(true.B)
      d.clock.step()
      d.io.request.poke(false.B)
      var sawEvict = false
      var seenAddr = BigInt(0)
      var seenWay = BigInt(0)
      for (_ <- 0 until 5) {
        if (d.io.tagEvictValid.peek().litToBoolean) {
          sawEvict = true
          seenAddr = d.io.tagEvictAddr.peek().litValue
          seenWay = d.io.tagEvictWay.peek().litValue
        }
        d.clock.step()
      }
      assert(sawEvict, "tag CE must request local evict")
      assert(seenAddr == addr)
      assert(seenWay == 1)
    }
  }
  it should "replay data CE and suppress BEU for load UEC" in {
    val defaultConfig = new DefaultConfig
    implicit val p = defaultConfig.alterPartial({ case XSCoreParamsKey => defaultConfig(XSTileKey).head.copy() }).alterPartial({ case LogUtilsOptionsKey => LogUtilsOptions(false, false, false, false); case PerfCounterOptionsKey => PerfCounterOptions(false, false, XSPerfLevel.VERBOSE, 0) })
    simulate(new LoadPipeEccTagHarness) { d =>
      val addr = BigInt("80008000", 16)
      d.io.request.poke(false.B)
      d.io.paddr.poke(addr.U)
      d.io.tagFault.poke(0.U)
      d.io.dataCe.poke(true.B)
      d.io.dataUec.poke(false.B)
      d.reset.poke(true.B)
      d.clock.step(2)
      d.reset.poke(false.B)
      d.clock.step()
      d.io.request.poke(true.B)
      d.clock.step()
      d.io.request.poke(false.B)
      var dataEvict = false
      var replay = false
      for (_ <- 0 until 6) {
        dataEvict ||= d.io.dataEvictValid.peek().litToBoolean
        replay ||= d.io.responseEccReplay.peek().litToBoolean
        d.clock.step()
      }
      assert(dataEvict, "data CE must request local evict")
      assert(replay, "data CE must mark replay response")

      d.io.dataCe.poke(false.B)
      d.io.dataUec.poke(true.B)
      d.io.request.poke(true.B)
      d.clock.step()
      d.io.request.poke(false.B)
      var error = false
      var errorValid = false
      var report = false
      for (_ <- 0 until 6) {
        error ||= d.io.responseError.peek().litToBoolean
        errorValid ||= d.io.errorValid.peek().litToBoolean
        report ||= d.io.reportToBeu.peek().litToBoolean
        d.clock.step()
      }
      assert(error, "data UEC must mark error response")
      assert(errorValid, "data UEC must produce load error event")
      assert(!report, "load UEC must not report to BEU")
    }
  }

}
