package xiangshan.cache

import chisel3._
import chisel3.util._
import chisel3.simulator.scalatest.ChiselSim
import freechips.rocketchip.tilelink.ClientStates
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import top.DefaultConfig
import utility.Constantin
import utility.{LogUtilsOptions, LogUtilsOptionsKey, PerfCounterOptions, PerfCounterOptionsKey, XSPerfLevel}
import xiangshan.{XSCoreParamsKey, XSTileKey}

class MainPipeStoreEccHarness(implicit p: Parameters) extends DCacheModule {
  val io = IO(new Bundle {
    val request = Input(Bool())
    val isAmo = Input(Bool())
    val paddr = Input(UInt(PAddrBits.W))
    val tagFault = Input(UInt(encTagBits.W))
    val dataUec = Input(Bool())
    val replay = Output(Bool())
    val atomicResp = Output(Bool())
    val atomicReplay = Output(Bool())
    val atomicError = Output(Bool())
    val reportToBeu = Output(Bool())
    val wbValid = Output(Bool())
    val wbCorrupt = Output(Bool())
    val wbHasData = Output(Bool())
    val dataWrite = Output(Bool())
    val tagWrite = Output(Bool())
    val metaWrite = Output(Bool())
  })

  val pipe = Module(new MainPipe)
  pipe.io.probe_req.valid := false.B
  pipe.io.probe_req.bits := 0.U.asTypeOf(pipe.io.probe_req.bits)
  pipe.io.refill_req.valid := false.B
  pipe.io.refill_req.bits := 0.U.asTypeOf(pipe.io.refill_req.bits)
  pipe.io.atomic_req.valid := io.request && io.isAmo
  pipe.io.atomic_req.bits := 0.U.asTypeOf(pipe.io.atomic_req.bits)
  pipe.io.atomic_req.bits.miss := false.B
  pipe.io.atomic_req.bits.probe := false.B
  pipe.io.atomic_req.bits.local_evict := false.B
  pipe.io.atomic_req.bits.local_evict_tag := false.B
  pipe.io.atomic_req.bits.local_evict_way_en := 0.U
  pipe.io.atomic_req.bits.probe_need_data := false.B
  pipe.io.atomic_req.bits.source := AMO_SOURCE.U
  pipe.io.atomic_req.bits.cmd := MemoryOpConstants.M_XA_ADD
  pipe.io.atomic_req.bits.addr := io.paddr
  pipe.io.atomic_req.bits.vaddr := io.paddr
  pipe.io.atomic_req.bits.word_idx := 0.U
  pipe.io.atomic_req.bits.amo_data := 1.U
  pipe.io.atomic_req.bits.amo_mask := Fill(QuadWordBytes, 1.U(1.W))
  pipe.io.atomic_req.bits.replace := false.B
  pipe.io.atomic_req.bits.error := false.B
  pipe.io.prefetch_req.valid := false.B
  pipe.io.prefetch_req.bits := 0.U.asTypeOf(pipe.io.prefetch_req.bits)
  pipe.io.store_req.valid := io.request && !io.isAmo
  pipe.io.store_req.bits := 0.U.asTypeOf(pipe.io.store_req.bits)
  pipe.io.store_req.bits.cmd := MemoryOpConstants.M_XWR
  pipe.io.store_req.bits.addr := io.paddr
  pipe.io.store_req.bits.vaddr := io.paddr
  pipe.io.store_req.bits.mask := 1.U
  pipe.io.store_req.bits.data := 0x5a.U
  pipe.io.store_req.bits.id := 3.U

  pipe.io.miss_resp := 0.U.asTypeOf(pipe.io.miss_resp)
  pipe.io.miss_req.ready := true.B
  pipe.io.wbq_block_miss_req := false.B
  pipe.io.refill_info := 0.U.asTypeOf(pipe.io.refill_info)
  pipe.io.wb.ready := true.B
  pipe.io.wb_ready_dup.foreach(_ := true.B)
  pipe.io.data_read := VecInit(Seq.fill(LoadPipelineWidth)(false.B))
  pipe.io.data_readline.ready := true.B
  pipe.io.readline_error := false.B
  pipe.io.readline_error_delayed := false.B
  pipe.io.data_write.ready := true.B
  pipe.io.data_write_ready_dup.foreach(_ := true.B)
  pipe.io.meta_read.ready := true.B
  pipe.io.meta_write.ready := true.B
  pipe.io.error_flag_write.ready := true.B
  pipe.io.prefetch_flag_write.ready := true.B
  pipe.io.access_flag_write.ready := true.B
  pipe.io.latency_flag_write.ready := true.B
  pipe.io.tag_read.ready := true.B
  pipe.io.tag_write.ready := true.B
  pipe.io.tag_write_ready_dup.foreach(_ := true.B)
  pipe.io.replace_way.way := 0.U
  pipe.io.replace.block := false.B
  pipe.io.replace.req.valid := DontCare
  pipe.io.replace.req.bits := DontCare
  pipe.io.btot_ways_for_set := 0.U
  pipe.io.sms_agt_evict_req.ready := true.B
  pipe.io.invalid_resv_set := false.B
  pipe.io.force_write := false.B

  for (way <- 0 until nWays) {
    pipe.io.meta_resp(way).coh.state := (if (way == 0) ClientStates.Dirty else ClientStates.Nothing)
    pipe.io.extra_meta_resp(way) := 0.U.asTypeOf(pipe.io.extra_meta_resp(way))
    val rawTag = if (way == 0) get_tag(io.paddr) else 0.U(tagBits.W)
    pipe.io.tag_resp(way) := cacheParams.tagCode.encode(rawTag) ^
      Mux(way.U === 0.U, io.tagFault, 0.U(encTagBits.W))
  }
  for (bank <- 0 until DCacheBanks) {
    val raw = (0x100 + bank).U(DCacheSRAMRowBits.W)
    val encoded = cacheParams.dataCode.encode(raw) ^ Mux(bank.U === 0.U && io.dataUec, 3.U(encDataBits.W), 1.U(encDataBits.W))
    pipe.io.data_resp(bank).raw_data := encoded(DCacheSRAMRowBits - 1, 0)
    pipe.io.data_resp(bank).ecc := encoded(encDataBits - 1, DCacheSRAMRowBits)
    pipe.io.data_resp(bank).error_delayed := false.B
    pipe.io.data_resp(bank).correctable_delayed := false.B
    pipe.io.data_resp(bank).uncorrectable_delayed := false.B
  }

  io.replay := Mux(io.isAmo, pipe.io.atomic_resp.valid && pipe.io.atomic_resp.bits.replay, pipe.io.store_replay_resp.valid)
  io.atomicResp := pipe.io.atomic_resp.valid
  io.atomicReplay := pipe.io.atomic_resp.bits.replay
  io.atomicError := pipe.io.atomic_resp.bits.error
  io.reportToBeu := pipe.io.error.valid && pipe.io.error.bits.report_to_beu
  io.wbValid := pipe.io.wb.valid
  io.wbCorrupt := pipe.io.wb.bits.corrupt
  io.wbHasData := pipe.io.wb.bits.hasData
  io.dataWrite := pipe.io.data_write.valid
  io.tagWrite := pipe.io.tag_write.valid
  io.metaWrite := pipe.io.meta_write.valid
}

class MainPipeEccTest extends AnyFlatSpec with ChiselSim {
  private def config = {
    val base = new DefaultConfig
    base.alterPartial({ case XSCoreParamsKey => base(XSTileKey).head.copy() }).alterPartial({
      case LogUtilsOptionsKey => LogUtilsOptions(false, false, false, false)
      case PerfCounterOptionsKey => PerfCounterOptions(false, false, XSPerfLevel.VERBOSE, 0)
    })
  }

  private def runStore(d: MainPipeStoreEccHarness, uec: Boolean, tagFault: BigInt = 0): (Boolean, Boolean, Boolean, Boolean, Boolean, Boolean) = {
    d.io.request.poke(false.B)
    d.io.isAmo.poke(false.B)
    d.io.paddr.poke("h80004000".U)
    d.io.tagFault.poke(tagFault.U)
    d.io.dataUec.poke(uec.B)
    d.reset.poke(true.B)
    d.clock.step(2)
    d.reset.poke(false.B)
    d.clock.step()
    d.io.request.poke(true.B)
    d.clock.step()
    d.io.request.poke(false.B)
    var replay = false
    var wb = false
    var corrupt = false
    var hasData = false
    var dataWrite = false
    var tagWrite = false
    for (_ <- 0 until 8) {
      replay ||= d.io.replay.peek().litToBoolean
      wb ||= d.io.wbValid.peek().litToBoolean
      corrupt ||= d.io.wbCorrupt.peek().litToBoolean
      hasData ||= d.io.wbHasData.peek().litToBoolean
      dataWrite ||= d.io.dataWrite.peek().litToBoolean
      tagWrite ||= d.io.tagWrite.peek().litToBoolean
      d.clock.step()
    }
    (replay, wb, corrupt, hasData, dataWrite, tagWrite)
  }

  private def runAmo(d: MainPipeStoreEccHarness, uec: Boolean, tagFault: BigInt = 0): (Boolean, Boolean, Boolean, Boolean, Boolean, Boolean, Boolean) = {
    d.io.request.poke(false.B)
    d.io.isAmo.poke(true.B)
    d.io.paddr.poke("h80004000".U)
    d.io.tagFault.poke(tagFault.U)
    d.io.dataUec.poke(uec.B)
    d.reset.poke(true.B)
    d.clock.step(2)
    d.reset.poke(false.B)
    d.clock.step()
    d.io.request.poke(true.B)
    d.clock.step()
    d.io.request.poke(false.B)
    var replay = false
    var response = false
    var error = false
    var reportToBeu = false
    var wb = false
    var dataWrite = false
    var tagWrite = false
    for (_ <- 0 until 8) {
      replay ||= d.io.replay.peek().litToBoolean
      response ||= d.io.atomicResp.peek().litToBoolean
      error ||= d.io.atomicError.peek().litToBoolean
      reportToBeu ||= d.io.reportToBeu.peek().litToBoolean
      wb ||= d.io.wbValid.peek().litToBoolean
      dataWrite ||= d.io.dataWrite.peek().litToBoolean
      tagWrite ||= d.io.tagWrite.peek().litToBoolean
      d.clock.step()
    }
    (replay, response, error, reportToBeu, wb, dataWrite, tagWrite)
  }

  behavior of "MainPipe ECC"

  it should "release and replay a store data CE without array writes" in {
    Constantin.init(false)
    implicit val p = config
    simulate(new MainPipeStoreEccHarness) { d =>
      val (replay, wb, corrupt, hasData, dataWrite, tagWrite) = runStore(d, uec = false)
      assert(replay, "store CE must replay")
      assert(wb, "store CE must release the cache line")
      assert(!corrupt, "store CE release must be clean")
      assert(hasData, "dirty line CE release must carry corrected data")
      assert(!dataWrite && !tagWrite, "store CE recovery must not write arrays")
    }
  }

  it should "release and replay a store tag CE without array writes" in {
    Constantin.init(false)
    implicit val p = config
    simulate(new MainPipeStoreEccHarness) { d =>
      val (replay, wb, corrupt, hasData, dataWrite, tagWrite) = runStore(d, uec = false, tagFault = 1)
      assert(replay, "store tag CE must replay")
      assert(wb && !corrupt && hasData, "store tag CE must cleanly release dirty data")
      assert(!dataWrite && !tagWrite, "store tag CE recovery must not write arrays")
    }
  }

  it should "release corrupt data and replay a store UEC" in {
    Constantin.init(false)
    implicit val p = config
    simulate(new MainPipeStoreEccHarness) { d =>
      val (replay, wb, corrupt, hasData, dataWrite, tagWrite) = runStore(d, uec = true)
      assert(replay, "store UEC must replay")
      assert(wb, "store UEC must release the cache line")
      assert(corrupt, "store UEC release must be corrupt")
      assert(hasData, "dirty UEC release must carry data")
      assert(!dataWrite && !tagWrite, "store UEC must not write arrays")
    }
  }

  it should "report a store tag UEC without release, replay, or array writes" in {
    Constantin.init(false)
    implicit val p = config
    simulate(new MainPipeStoreEccHarness) { d =>
      val (replay, wb, _, _, dataWrite, tagWrite) = runStore(d, uec = false, tagFault = 3)
      assert(!replay, "store tag UEC is not recoverable and must not replay")
      assert(!wb, "store tag UEC must not release an unrecoverable line")
      assert(!dataWrite && !tagWrite, "store tag UEC must not write arrays")
    }
  }

  it should "release and replay an AMO data CE without array writes" in {
    Constantin.init(false)
    implicit val p = config
    simulate(new MainPipeStoreEccHarness) { d =>
      val (replay, response, error, reportToBeu, wb, dataWrite, tagWrite) = runAmo(d, uec = false)
      assert(replay, "AMO CE must replay")
      assert(response, "AMO CE must produce a replay response")
      assert(!error && !reportToBeu, "correctable AMO data error must not report a hardware error")
      assert(wb, "AMO CE must release the cache line")
      assert(!dataWrite && !tagWrite, "AMO CE recovery must not write arrays")
    }
  }

  it should "release and replay an AMO tag CE without array writes" in {
    Constantin.init(false)
    implicit val p = config
    simulate(new MainPipeStoreEccHarness) { d =>
      val (replay, response, error, reportToBeu, wb, dataWrite, tagWrite) = runAmo(d, uec = false, tagFault = 1)
      assert(replay && response, "AMO tag CE must produce a replay response")
      assert(!error && !reportToBeu, "correctable AMO tag error must not report a hardware error")
      assert(wb, "AMO tag CE must release the cache line")
      assert(!dataWrite && !tagWrite, "AMO tag CE recovery must not write arrays")
    }
  }

  it should "report an AMO data UEC without array writes or BEU reporting" in {
    Constantin.init(false)
    implicit val p = config
    simulate(new MainPipeStoreEccHarness) { d =>
      val (replay, response, error, reportToBeu, wb, dataWrite, tagWrite) = runAmo(d, uec = true)
      assert(!replay, "AMO UEC must complete with a hardware-error response, not replay")
      assert(response && error, "AMO UEC must return atomic_resp.error")
      assert(!reportToBeu, "AMO UEC must be handled through the hardware-error path, not BEU")
      assert(!wb, "AMO UEC must not issue a release")
      assert(!dataWrite && !tagWrite, "AMO UEC must not write arrays")
    }
  }
  it should "report an AMO tag UEC without array writes or BEU reporting" in {
    Constantin.init(false)
    implicit val p = config
    simulate(new MainPipeStoreEccHarness) { d =>
      val (replay, response, error, reportToBeu, wb, dataWrite, tagWrite) = runAmo(d, uec = false, tagFault = 3)
      assert(!replay, "AMO tag UEC must complete with a hardware-error response, not replay")
      assert(response && error, "AMO tag UEC must return atomic_resp.error")
      assert(!reportToBeu, "AMO tag UEC must be handled through the hardware-error path, not BEU")
      assert(!wb, "AMO tag UEC must not issue a release")
      assert(!dataWrite && !tagWrite, "AMO tag UEC must not write arrays")
    }
  }

}
