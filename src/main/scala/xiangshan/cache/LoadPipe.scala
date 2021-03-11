package xiangshan.cache

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink.ClientMetadata

import utils.{XSDebug, XSPerf}

class LoadPipe extends DCacheModule {
  def metaBits = (new L1Metadata).getWidth
  def encMetaBits = cacheParams.tagCode.width(metaBits)
  def getMeta(encMeta: UInt): UInt = {
    require(encMeta.getWidth == encMetaBits)
    encMeta(metaBits - 1, 0)
  }

  val io = IO(new DCacheBundle {
    // incoming requests
    val lsu = Flipped(new DCacheLoadIO)
    // req got nacked in stage 0?
    val nack      = Input(Bool())

    // meta and data array read port
    val data_read = DecoupledIO(new L1DataReadReq)
    val data_resp = Input(Vec(blockRows, Bits(encRowBits.W)))
    val meta_read = DecoupledIO(new L1MetaReadReq)
    val meta_resp = Input(Vec(nWays, UInt(encMetaBits.W)))

    // send miss request to miss queue
    val miss_req    = DecoupledIO(new MissReq)

    // update state vec in replacement algo
    val replace_access = ValidIO(new ReplacementAccessBundle)

    // load fast wakeup should be disabled when data read is not ready
    val disable_ld_fast_wakeup = Input(Bool())
  })

  val s1_ready = Wire(Bool())
  val s2_ready = Wire(Bool())
  // LSU requests
  // it you got nacked, you can directly passdown
  val not_nacked_ready = io.meta_read.ready && s1_ready
  val nacked_ready     = true.B

  // ready can wait for valid
  io.lsu.req.ready := (!io.nack && not_nacked_ready) || (io.nack && nacked_ready)
  io.meta_read.valid := io.lsu.req.fire() && !io.nack

  val meta_read = io.meta_read.bits

  // Tag read for new requests
  meta_read.idx := get_idx(io.lsu.req.bits.addr)
  meta_read.way_en := ~0.U(nWays.W)
  meta_read.tag := DontCare

  // Pipeline
  // --------------------------------------------------------------------------------
  // stage 0
  val s0_valid = io.lsu.req.fire()
  val s0_req = io.lsu.req.bits
  val s0_fire = s0_valid && s1_ready

  assert(RegNext(!(s0_valid && s0_req.cmd =/= MemoryOpConstants.M_XRD)), "LoadPipe only accepts load req")

  dump_pipeline_reqs("LoadPipe s0", s0_valid, s0_req)

  // --------------------------------------------------------------------------------
  // stage 1
  val s1_valid = RegInit(false.B)
  val s1_req = RegEnable(s0_req, s0_fire)
  // in stage 1, load unit gets the physical address
  val s1_addr = io.lsu.s1_paddr
  val s1_nack = RegNext(io.nack)
  val s1_fire = s1_valid && s2_ready
  s1_ready := !s1_valid || s1_fire

  when (s0_fire) { s1_valid := true.B }
  .elsewhen (s1_fire) { s1_valid := false.B }

  dump_pipeline_reqs("LoadPipe s1", s1_valid, s1_req)

  // tag check
  val meta_resp = VecInit(io.meta_resp.map(r => getMeta(r).asTypeOf(new L1Metadata)))
  def wayMap[T <: Data](f: Int => T) = VecInit((0 until nWays).map(f))
  val s1_tag_eq_way = wayMap((w: Int) => meta_resp(w).tag === (get_tag(s1_addr))).asUInt
  val s1_tag_match_way = wayMap((w: Int) => s1_tag_eq_way(w) && meta_resp(w).coh.isValid()).asUInt
  val s1_tag_match = s1_tag_match_way.orR
  assert(RegNext(PopCount(s1_tag_match_way) <= 1.U), "tag should not match with more than 1 way")

  val s1_fake_meta = Wire(new L1Metadata)
  s1_fake_meta.tag := get_tag(s1_addr)
  s1_fake_meta.coh := ClientMetadata.onReset

  // when there are no tag match, we give it a Fake Meta
  // this simplifies our logic in s2 stage
  val s1_hit_meta = Mux(s1_tag_match, Mux1H(s1_tag_match_way, wayMap((w: Int) => meta_resp(w))), s1_fake_meta)
  val s1_hit_coh = s1_hit_meta.coh

  // data read
  val data_read = io.data_read.bits
  data_read.addr := s1_addr
  data_read.way_en := s1_tag_match_way
  // only needs to read the specific row
  data_read.rmask := UIntToOH(get_row(s1_addr))
  io.data_read.valid := s1_fire && !s1_nack

  io.replace_access.valid := RegNext(RegNext(io.meta_read.fire()) && s1_tag_match && s1_valid)
  io.replace_access.bits.set := RegNext(get_idx(s1_req.addr))
  io.replace_access.bits.way := RegNext(OHToUInt(s1_tag_match_way))

  // tag ecc check
  (0 until nWays).foreach(w => assert(!RegNext(s1_valid && s1_tag_match_way(w) && cacheParams.tagCode.decode(io.meta_resp(w)).uncorrectable)))

  // --------------------------------------------------------------------------------
  // stage 2
  // val s2_valid = RegEnable(next = s1_valid && !io.lsu.s1_kill, init = false.B, enable = s1_fire)
  val s2_valid = RegInit(false.B)
  val s2_req = RegEnable(s1_req, s1_fire)
  val s2_addr = RegEnable(s1_addr, s1_fire)
  s2_ready := true.B

  when (s1_fire) { s2_valid := !io.lsu.s1_kill }
  .elsewhen(io.lsu.resp.fire()) { s2_valid := false.B }

  dump_pipeline_reqs("LoadPipe s2", s2_valid, s2_req)

  // hit, miss, nack, permission checking
  val s2_tag_match_way = RegEnable(s1_tag_match_way, s1_fire)
  val s2_tag_match = RegEnable(s1_tag_match, s1_fire)

  val s2_hit_meta = RegEnable(s1_hit_meta, s1_fire)
  val s2_hit_coh = RegEnable(s1_hit_coh, s1_fire)
  val s2_has_permission = s2_hit_coh.onAccess(s2_req.cmd)._1
  val s2_new_hit_coh = s2_hit_coh.onAccess(s2_req.cmd)._3

  val s2_hit = s2_tag_match && s2_has_permission && s2_hit_coh === s2_new_hit_coh

  // when req got nacked, upper levels should replay this request
  // nacked or not
  val s2_nack_hit = RegEnable(s1_nack, s1_fire)
  // can no allocate mshr for load miss
  val s2_nack_no_mshr = io.miss_req.valid && !io.miss_req.ready
  // Bank conflict on data arrays
  val s2_nack_data = RegEnable(!io.data_read.ready, s1_fire)
  val s2_nack = s2_nack_hit || s2_nack_no_mshr || s2_nack_data

  // select the row we are interested in
  val data_resp = io.data_resp
  val s2_data = data_resp(get_row(s2_addr))

  // select the word
  // the index of word in a row, in case rowBits != wordBits
  val s2_word_idx = if (rowWords == 1) 0.U else s2_addr(log2Up(rowWords*wordBytes)-1, log2Up(wordBytes))

  // load data gen
  val s2_data_words = Wire(Vec(rowWords, UInt(encWordBits.W)))
  for (w <- 0 until rowWords) {
    s2_data_words(w) := s2_data(encWordBits * (w + 1) - 1, encWordBits * w)
  }
  val s2_word = s2_data_words(s2_word_idx)
  // val s2_decoded = cacheParams.dataCode.decode(s2_word)
  // val s2_word_decoded = s2_decoded.corrected
  val s2_word_decoded = s2_word(wordBits - 1, 0)
  assert(RegNext(!(s2_valid && s2_hit && !s2_nack && cacheParams.dataCode.decode(s2_word).uncorrectable)))


  // only dump these signals when they are actually valid
  dump_pipeline_valids("LoadPipe s2", "s2_hit", s2_valid && s2_hit)
  dump_pipeline_valids("LoadPipe s2", "s2_nack", s2_valid && s2_nack)
  dump_pipeline_valids("LoadPipe s2", "s2_nack_hit", s2_valid && s2_nack_hit)
  dump_pipeline_valids("LoadPipe s2", "s2_nack_no_mshr", s2_valid && s2_nack_no_mshr)

  // send load miss to miss queue
  io.miss_req.valid := s2_valid && !s2_nack_hit && !s2_nack_data && !s2_hit
  io.miss_req.bits := DontCare
  io.miss_req.bits.source := LOAD_SOURCE.U
  io.miss_req.bits.cmd := s2_req.cmd
  io.miss_req.bits.addr := get_block_addr(s2_addr)
  io.miss_req.bits.coh := s2_hit_coh

  // send back response
  val resp = Wire(ValidIO(new DCacheWordResp))
  resp.valid := s2_valid
  resp.bits := DontCare
  resp.bits.data := s2_word_decoded
  // on miss or nack, upper level should replay request
  // but if we successfully sent the request to miss queue
  // upper level does not need to replay request
  // they can sit in load queue and wait for refill
  resp.bits.miss := !s2_hit || s2_nack
  resp.bits.replay := resp.bits.miss && (!io.miss_req.fire() || s2_nack)

  io.lsu.resp.valid := resp.valid
  io.lsu.resp.bits := resp.bits
  assert(RegNext(!(resp.valid && !io.lsu.resp.ready)), "lsu should be ready in s2")

  when (resp.valid) {
    resp.bits.dump()
  }

  io.lsu.s1_hit_way := s1_tag_match_way
  io.lsu.s1_disable_fast_wakeup := io.disable_ld_fast_wakeup
  assert(RegNext(s1_ready && s2_ready), "load pipeline should never be blocked")

  // -------
  // Debug logging functions
  def dump_pipeline_reqs(pipeline_stage_name: String, valid: Bool,
    req: DCacheWordReq ) = {
      when (valid) {
        XSDebug(s"$pipeline_stage_name: ")
        req.dump()
      }
  }

  def dump_pipeline_valids(pipeline_stage_name: String, signal_name: String, valid: Bool) = {
    when (valid) {
      XSDebug(s"$pipeline_stage_name $signal_name\n")
    }
  }

  // performance counters
  XSPerf("load_req", io.lsu.req.fire())
  XSPerf("load_s1_kill", s1_fire && io.lsu.s1_kill)
  XSPerf("load_hit_way", s1_fire && s1_tag_match)
  XSPerf("load_replay", io.lsu.resp.fire() && resp.bits.replay)
  XSPerf("load_replay_for_data_nack", io.lsu.resp.fire() && resp.bits.replay && s2_nack_data)
  XSPerf("load_replay_for_no_mshr", io.lsu.resp.fire() && resp.bits.replay && s2_nack_no_mshr)
  XSPerf("load_hit", io.lsu.resp.fire() && !resp.bits.miss)
  XSPerf("load_miss", io.lsu.resp.fire() && resp.bits.miss)
  XSPerf("actual_ld_fast_wakeup", s1_fire && s1_tag_match && !io.disable_ld_fast_wakeup)
  XSPerf("ideal_ld_fast_wakeup", io.data_read.fire() && s1_tag_match)
}
