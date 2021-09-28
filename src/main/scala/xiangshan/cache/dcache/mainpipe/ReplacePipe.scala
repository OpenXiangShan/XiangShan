package xiangshan.cache

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink.{ClientMetadata, ClientStates}
import utils.{OneHot, ParallelMux}

class ReplacePipeReq(implicit p: Parameters) extends DCacheBundle {
  val miss_id = UInt(log2Up(cfg.nMissEntries).W)
  val way_en = UInt(DCacheWays.W)

  // if dcache size > 32KB, vaddr is also needed for store
  // vaddr is used to get extra index bits
  val vaddr  = UInt(VAddrBits.W)

  val tag = UInt(tagBits.W) // tag of the block to be replaced
}

class ReplacePipeResp(implicit p: Parameters) extends DCacheBundle {
  val miss_id = UInt(log2Up(cfg.nMissEntries).W)
}

class ReplacePipe(implicit p: Parameters) extends  DCacheModule {
  def metaAndTagOnReset = MetaAndTag(ClientMetadata.onReset, 0.U)
  // enc bits encode both tag and meta, but is saved in meta array
  val encMetaBits = cacheParams.tagCode.width(metaAndTagOnReset.getWidth) - tagBits
  val metaBits = (new Meta).getWidth

  val io = IO(new Bundle() {
    // miss queue
    val req = Flipped(DecoupledIO(new ReplacePipeReq))
    val resp = ValidIO(new ReplacePipeResp)

    // write-back queue
    val wb = DecoupledIO(new WritebackReq)

    // read data array, invalid meta array
    val data_read = DecoupledIO(new L1BankedDataReadLineReq)
    val data_resp = Input(Vec(DCacheBanks, new L1BankedDataReadResult))
    val meta_read = DecoupledIO(new MetaReadReq)
    val meta_resp = Input(Vec(nWays, UInt(encMetaBits.W)))
    val meta_write = DecoupledIO(new MetaWriteReq)
  })

  val s0_fire = Wire(Bool())
  val s1_ready = Wire(Bool())
  val s1_valid = RegInit(false.B)
  val s1_fire = Wire(Bool())

  // meta array is made of regs, so meta write or read should always be ready
  assert(RegNext(io.meta_write.ready))
  assert(RegNext(io.meta_read.ready))
  when (s0_fire) { OneHot.checkOneHot(io.req.bits.way_en) }

  // s0: read data to be replaced
  val s0_can_go = s1_ready && io.data_read.ready && io.meta_read.ready
  s0_fire := io.req.valid && s0_can_go

  // s1: invalid meta that is going to be replaced
  val s1_req = RegEnable(io.req.bits, s0_fire)
  val s1_idx = addr_to_dcache_set(s1_req.vaddr)
  val s1_coh = Wire(new ClientMetadata)
  val s1_need_release = s1_coh.state =/= ClientStates.Nothing
  val s1_can_go = io.meta_write.ready && io.wb.ready || !s1_need_release
  s1_fire := s1_valid && s1_can_go
  s1_ready := !s1_valid || s1_fire
  when (s0_fire) {
    s1_valid := true.B
  }.elsewhen(s1_fire) {
    s1_valid := false.B
  }
  val data_resp_raw = WireInit(VecInit(io.data_resp.map(_.raw_data)))
  val s1_data = Wire(Vec(DCacheBanks, UInt(DCacheSRAMRowBits.W)))
  s1_data := Mux(RegNext(s0_fire), data_resp_raw, RegNext(s1_data))

  val meta_resp_ecc = ParallelMux(s1_req.way_en.asBools zip io.meta_resp)
  val meta_resp_raw = meta_resp_ecc(metaBits - 1, 0).asTypeOf(new Meta)
  s1_coh := Mux(RegNext(s0_fire), meta_resp_raw.coh, RegNext(s1_coh))

  io.req.ready := s0_can_go

  io.data_read.valid := io.req.valid && s1_ready
  io.data_read.bits.way_en := io.req.bits.way_en
  io.data_read.bits.addr := io.req.bits.vaddr
  io.data_read.bits.rmask := ~0.U

  io.meta_read.valid := io.req.valid && s1_ready
  io.meta_read.bits.idx := addr_to_dcache_set(io.req.bits.vaddr)
  io.meta_read.bits.way_en := io.req.bits.way_en

  io.meta_write.valid := s1_valid && io.wb.ready && s1_need_release
  io.meta_write.bits.idx := s1_idx
  io.meta_write.bits.way_en := s1_req.way_en
  io.meta_write.bits.meta.coh := ClientMetadata.onReset
  io.meta_write.bits.tag := s1_req.tag // only used to calculate ecc

  io.wb.valid := s1_valid && s1_need_release
  io.wb.bits.addr := Cat(s1_req.tag, get_untag(s1_req.vaddr))
  val (_, release_param, _) = s1_coh.onCacheControl(M_FLUSH)
  io.wb.bits.param := release_param
  io.wb.bits.voluntary := true.B
  io.wb.bits.hasData := (if (dcacheParameters.alwaysReleaseData) {
    s1_coh.state =/= ClientStates.Nothing
  } else {
    s1_coh.state === ClientStates.Dirty
  })
  io.wb.bits.dirty := s1_coh.state === ClientStates.Dirty
  io.wb.bits.data := s1_data.asUInt

  io.resp.valid := s1_fire
  io.resp.bits.miss_id := s1_req.miss_id
}
