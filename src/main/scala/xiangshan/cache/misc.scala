package xiangshan.cache

import chisel3._
import chisel3.util._

import utils.XSDebug

// this is a traditional cache pipeline:
// it handles load/store/amo/lr,sc
class MiscPipe extends DCacheModule
{
  val io = IO(new DCacheBundle{
    val lsu       = Flipped(new DCacheLoadIO)
    val data_read  = DecoupledIO(new L1DataReadReq)
    val data_resp  = Input(Vec(nWays, Vec(refillCycles, Bits(encRowBits.W))))
    val data_write = DecoupledIO(new L1DataWriteReq)
    val meta_read  = DecoupledIO(new L1MetaReadReq)
    val meta_resp  = Input(Vec(nWays, new L1Metadata))
    val inflight_req_idxes       = Output(Vec(3, Valid(UInt())))
    val inflight_req_block_addrs = Output(Vec(3, Valid(UInt())))
  })

  // LSU requests
  io.lsu.req.ready := io.meta_read.ready && io.data_read.ready
  io.meta_read.valid := io.lsu.req.valid
  io.data_read.valid := io.lsu.req.valid

  val meta_read = io.meta_read.bits
  val data_read = io.data_read.bits

  // Tag read for new requests
  meta_read.idx    := get_idx(io.lsu.req.bits.addr)
  meta_read.way_en := ~0.U(nWays.W)
  meta_read.tag    := DontCare
  // Data read for new requests
  data_read.addr   := io.lsu.req.bits.addr
  data_read.way_en := ~0.U(nWays.W)
  // only needs to read the specific beat
  data_read.rmask  := UIntToOH(get_beat(io.lsu.req.bits.addr))

  // Pipeline
  // stage 0
  val s0_valid = io.lsu.req.fire()
  val s0_req = io.lsu.req.bits

  dump_pipeline_reqs("MiscPipe s0", s0_valid, s0_req)

  // stage 1
  val s1_req = RegNext(s0_req)
  val s1_valid = RegNext(s0_valid, init = false.B)
  val s1_addr = s1_req.addr
  val s1_nack = false.B 

  dump_pipeline_reqs("MiscPipe s1", s1_valid, s1_req)

  // tag check
  val meta_resp = io.meta_resp
  def wayMap[T <: Data](f: Int => T) = VecInit((0 until nWays).map(f))
  val s1_tag_eq_way = wayMap((w: Int) => meta_resp(w).tag === (get_tag(s1_addr))).asUInt
  val s1_tag_match_way = wayMap((w: Int) => s1_tag_eq_way(w) && meta_resp(w).coh.isValid()).asUInt


  // stage 2
  val s2_req   = RegNext(s1_req)
  val s2_valid = RegNext(s1_valid && !io.lsu.s1_kill, init = false.B)

  dump_pipeline_reqs("MiscPipe s2", s2_valid, s2_req)

  val s2_tag_match_way = RegNext(s1_tag_match_way)
  val s2_tag_match     = s2_tag_match_way.orR
  val s2_hit_state     = Mux1H(s2_tag_match_way, wayMap((w: Int) => RegNext(meta_resp(w).coh)))
  val s2_has_permission = s2_hit_state.onAccess(s2_req.cmd)._1
  val s2_new_hit_state  = s2_hit_state.onAccess(s2_req.cmd)._3

  // we not only need permissions
  // we also require that state does not change on hit
  // thus we require new_hit_state === old_hit_state
  //
  // If state changes on hit,
  // we should treat it as not hit, and let mshr deal with it,
  // since we can not write meta data on the main pipeline.
  // It's possible that we had permission but state changes on hit:
  // eg: write to exclusive but clean block
  val s2_hit = s2_tag_match && s2_has_permission && s2_hit_state === s2_new_hit_state
  val s2_nack = Wire(Bool())
  val s2_data = Wire(Vec(nWays, UInt(encRowBits.W)))
  val data_resp = io.data_resp
  for (w <- 0 until nWays) {
    s2_data(w) := data_resp(w)(get_beat(s2_req.addr))
  }

  val s2_data_muxed = Mux1H(s2_tag_match_way, s2_data)
  // the index of word in a row, in case rowBits != wordBits
  val s2_word_idx   = if (rowWords == 1) 0.U else s2_req.addr(log2Up(rowWords*wordBytes)-1, log2Up(wordBytes))

  val s2_nack_hit    = RegNext(s1_nack)
  // Can't allocate MSHR for same set currently being written back
  // the same set is busy
  val s2_nack_set_busy  = s2_valid && false.B
  // Bank conflict on data arrays
  val s2_nack_data   = false.B

  s2_nack           := s2_nack_hit || s2_nack_set_busy || s2_nack_data

  // only dump these signals when they are actually valid
  dump_pipeline_valids("MiscPipe s2", "s2_hit", s2_valid && s2_hit)
  dump_pipeline_valids("MiscPipe s2", "s2_nack", s2_valid && s2_nack)
  dump_pipeline_valids("MiscPipe s2", "s2_nack_hit", s2_valid && s2_nack_hit)
  dump_pipeline_valids("MiscPipe s2", "s2_nack_set_busy", s2_valid && s2_nack_set_busy)

  // load data gen
  val s2_data_word = s2_data_muxed >> Cat(s2_word_idx, 0.U(log2Ceil(wordBits).W))

  val resp = Wire(ValidIO(new DCacheResp))
  resp.valid     := s2_valid
  resp.bits.data := s2_data_word
  resp.bits.meta := s2_req.meta
  resp.bits.miss := !s2_hit
  resp.bits.nack := s2_nack

  io.lsu.resp.valid := resp.valid
  io.lsu.resp.bits := resp.bits
  assert(!(resp.valid && !io.lsu.resp.ready))

  when (resp.valid) {
    XSDebug(s"MiscPipe resp: data: %x id: %d replay: %b miss: %b nack: %b\n",
      resp.bits.data, resp.bits.meta.id, resp.bits.meta.replay, resp.bits.miss, resp.bits.nack)
  }

  // assign default value to output signals
  io.data_write.valid := false.B
  io.data_write.bits  := DontCare

  io.inflight_req_idxes(0).valid := io.lsu.req.valid
  io.inflight_req_idxes(1).valid := s1_valid
  io.inflight_req_idxes(2).valid := s2_valid

  io.inflight_req_idxes(0).bits  := get_idx(s0_req.addr)
  io.inflight_req_idxes(1).bits  := get_idx(s1_req.addr)
  io.inflight_req_idxes(2).bits  := get_idx(s2_req.addr)

  io.inflight_req_block_addrs(0).valid := io.lsu.req.valid
  io.inflight_req_block_addrs(1).valid := s1_valid
  io.inflight_req_block_addrs(2).valid := s2_valid

  io.inflight_req_block_addrs(0).bits  := get_block_addr(s0_req.addr)
  io.inflight_req_block_addrs(1).bits  := get_block_addr(s1_req.addr)
  io.inflight_req_block_addrs(2).bits  := get_block_addr(s2_req.addr)

  // -------
  // Debug logging functions
  def dump_pipeline_reqs(pipeline_stage_name: String, valid: Bool,
    req: DCacheWordReq ) = {
      when (valid) {
        XSDebug(s"$pipeline_stage_name cmd: %x addr: %x data: %x mask: %x id: %d replay: %b\n",
          req.cmd, req.addr, req.data, req.mask, req.meta.id, req.meta.replay)
      }
  }

  def dump_pipeline_valids(pipeline_stage_name: String, signal_name: String, valid: Bool) = {
    when (valid) {
      XSDebug(s"$pipeline_stage_name $signal_name\n")
    }
  }
}
