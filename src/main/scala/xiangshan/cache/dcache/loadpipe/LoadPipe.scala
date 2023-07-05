/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package xiangshan.cache

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink.ClientMetadata
import utils.{HasPerfEvents, XSDebug, XSPerfAccumulate}
import utility.ParallelPriorityMux
import xiangshan.L1CacheErrorInfo
import xiangshan.cache.dcache.{DCacheWPU, IdealWPU}

class LoadPipe(id: Int)(implicit p: Parameters) extends DCacheModule with HasPerfEvents {
  val io = IO(new DCacheBundle {
    // incoming requests
    val lsu = Flipped(new DCacheLoadIO)
    // req got nacked in stage 0?
    val nack      = Input(Bool())

    // meta and data array read port
    val meta_read = DecoupledIO(new MetaReadReq)
    val meta_resp = Input(Vec(nWays, new Meta))
    val extra_meta_resp = Input(Vec(nWays, new DCacheExtraMeta))

    val tag_read = DecoupledIO(new TagReadReq)
    val tag_resp = Input(Vec(nWays, UInt(encTagBits.W)))

    val banked_data_read = DecoupledIO(new L1BankedDataReadReq)
    val banked_data_resp = Input(new L1BankedDataReadResult())
    val read_error_delayed = Input(Bool())

    // access bit update
    val access_flag_write = DecoupledIO(new FlagMetaWriteReq)

    // banked data read conflict
    val bank_conflict_slow = Input(Bool())

    // send miss request to miss queue
    val miss_req    = DecoupledIO(new MissReq)
    val miss_resp   = Input(new MissResp)

    // update state vec in replacement algo
    val replace_access = ValidIO(new ReplacementAccessBundle)
    // find the way to be replaced
    val replace_way = new ReplacementWayReqIO

    // load fast wakeup should be disabled when data read is not ready
    val disable_ld_fast_wakeup = Input(Bool())

    // ecc error
    val error = Output(new L1CacheErrorInfo())

    // // debug_ls_info
    // val debug_s2_cache_miss = Bool()
  })

  assert(RegNext(io.meta_read.ready))

  val s1_ready = Wire(Bool())
  val s2_ready = Wire(Bool())
  // LSU requests
  // it you got nacked, you can directly passdown
  val not_nacked_ready = io.meta_read.ready && io.tag_read.ready && s1_ready
  val nacked_ready     = true.B

  // ready can wait for valid
  io.lsu.req.ready := (!io.nack && not_nacked_ready) || (io.nack && nacked_ready)
  io.meta_read.valid := io.lsu.req.fire() && !io.nack
  io.tag_read.valid := io.lsu.req.fire() && !io.nack

  val meta_read = io.meta_read.bits
  val tag_read = io.tag_read.bits

  // Tag read for new requests
  meta_read.idx := get_idx(io.lsu.req.bits.vaddr)
  meta_read.way_en := ~0.U(nWays.W)
  // meta_read.tag := DontCare

  tag_read.idx := get_idx(io.lsu.req.bits.vaddr)
  tag_read.way_en := ~0.U(nWays.W)

  // Pipeline
  // --------------------------------------------------------------------------------
  // stage 0
  // --------------------------------------------------------------------------------
  // read tag

  val s0_valid = io.lsu.req.fire()
  val s0_req = io.lsu.req.bits
  val s0_fire = s0_valid && s1_ready
  val s0_vaddr = s0_req.vaddr
  val s0_replayCarry = s0_req.replayCarry
  assert(RegNext(!(s0_valid && (s0_req.cmd =/= MemoryOpConstants.M_XRD && s0_req.cmd =/= MemoryOpConstants.M_PFR && s0_req.cmd =/= MemoryOpConstants.M_PFW))), "LoadPipe only accepts load req / softprefetch read or write!")
  dump_pipeline_reqs("LoadPipe s0", s0_valid, s0_req)

  // --------------------------------------------------------------------------------
  // stage 1
  // --------------------------------------------------------------------------------
  // tag match, read data

  val s1_valid = RegInit(false.B)
  val s1_req = RegEnable(s0_req, s0_fire)
  // in stage 1, load unit gets the physical address
  val s1_paddr_dup_lsu = io.lsu.s1_paddr_dup_lsu
  val s1_paddr_dup_dcache = io.lsu.s1_paddr_dup_dcache
  // LSU may update the address from io.lsu.s1_paddr, which affects the bank read enable only.
  val s1_vaddr = Cat(s1_req.vaddr(VAddrBits - 1, blockOffBits), io.lsu.s1_paddr_dup_lsu(blockOffBits - 1, 0))
  val s1_bank_oh = UIntToOH(addr_to_dcache_bank(s1_vaddr))
  val s1_nack = RegNext(io.nack)
  val s1_nack_data = !io.banked_data_read.ready
  val s1_fire = s1_valid && s2_ready
  s1_ready := !s1_valid || s1_fire

  when (s0_fire) { s1_valid := true.B }
  .elsewhen (s1_fire) { s1_valid := false.B }

  dump_pipeline_reqs("LoadPipe s1", s1_valid, s1_req)

  // tag check
  val meta_resp = io.meta_resp
  val tag_resp = io.tag_resp.map(r => r(tagBits - 1, 0))
  def wayMap[T <: Data](f: Int => T) = VecInit((0 until nWays).map(f))

  // dcache side tag match
  /* // just ideal situation
  val idealWPU = Module(new IdealWPU)
  val s1_vaddr_dup_dc = Wire(UInt(PAddrBits.W))
  s1_vaddr_dup_dc := RegEnable(s0_req.addr, s0_fire)
  idealWPU.io.req.bits.vaddr := s1_vaddr_dup_dc
  idealWPU.io.req.valid := true.B
  idealWPU.io.idealIf.s1_tag_resp := tag_resp
  idealWPU.io.idealIf.s1_meta_resp := meta_resp
  idealWPU.io.idealIf.s1_real_tag := get_tag(s1_paddr_dup_dcache)
  */
  // real wpu
  val wpu = Module(new DCacheWPU)
  // req in s0
  wpu.io.req.bits.vaddr := s0_vaddr
  wpu.io.req.bits.replayCarry := s0_replayCarry
  wpu.io.req.valid := s0_valid
  // check in s1
  wpu.io.check.bits.s1_tag_resp := tag_resp
  wpu.io.check.bits.s1_meta_resp := meta_resp
  wpu.io.check.bits.s1_real_tag := get_tag(s1_paddr_dup_dcache)
  wpu.io.check.valid := s1_valid
  // correct in s2
  val s2_wpu_pred_fail = wpu.io.s2_pred_fail
  val s2_real_way_en = wpu.io.s2_real_way_en

  // resp in s1
  val s1_tag_match_way_dup_dc = Wire(UInt(nWays.W))
  val s1_tag_match_way_dup_lsu = Wire(UInt(nWays.W))
  when (wpu.io.resp.valid){
    s1_tag_match_way_dup_dc := wpu.io.resp.bits.predict_way_en
    s1_tag_match_way_dup_lsu := wpu.io.resp.bits.predict_way_en
  }.otherwise {
    val s1_tag_eq_way_dup_dc = wayMap((w: Int) => tag_resp(w) === (get_tag(s1_paddr_dup_dcache))).asUInt
    s1_tag_match_way_dup_dc := wayMap((w: Int) => s1_tag_eq_way_dup_dc(w) && meta_resp(w).coh.isValid()).asUInt
    
    // lsu side tag match
    val s1_tag_eq_way_dup_lsu = wayMap((w: Int) => tag_resp(w) === (get_tag(s1_paddr_dup_lsu))).asUInt
    s1_tag_match_way_dup_lsu := wayMap((w: Int) => s1_tag_eq_way_dup_lsu(w) && meta_resp(w).coh.isValid()).asUInt
  }
  val s1_tag_match_dup_dc = s1_tag_match_way_dup_dc.orR
  val s1_tag_match_dup_lsu = s1_tag_match_way_dup_lsu.orR
  assert(RegNext(!s1_valid || PopCount(s1_tag_match_way_dup_dc) <= 1.U), "tag should not match with more than 1 way")

  val s1_fake_meta = Wire(new Meta)
  // s1_fake_meta.tag := get_tag(s1_paddr_dup_dcache)
  s1_fake_meta.coh := ClientMetadata.onReset
  val s1_fake_tag = get_tag(s1_paddr_dup_dcache)

  // when there are no tag match, we give it a Fake Meta
  // this simplifies our logic in s2 stage
  val s1_hit_meta = Mux(s1_tag_match_dup_dc, Mux1H(s1_tag_match_way_dup_dc, wayMap((w: Int) => meta_resp(w))), s1_fake_meta)
  val s1_hit_coh = s1_hit_meta.coh
  val s1_hit_error = Mux(s1_tag_match_dup_dc, Mux1H(s1_tag_match_way_dup_dc, wayMap((w: Int) => io.extra_meta_resp(w).error)), false.B)
  val s1_hit_prefetch = Mux(s1_tag_match_dup_dc, Mux1H(s1_tag_match_way_dup_dc, wayMap((w: Int) => io.extra_meta_resp(w).prefetch)), false.B)
  val s1_hit_access = Mux(s1_tag_match_dup_dc, Mux1H(s1_tag_match_way_dup_dc, wayMap((w: Int) => io.extra_meta_resp(w).access)), false.B)

  io.replace_way.set.valid := RegNext(s0_fire)
  io.replace_way.set.bits := get_idx(s1_vaddr)

  val s1_invalid_vec = wayMap(w => !meta_resp(w).coh.isValid())
  val s1_have_invalid_way = s1_invalid_vec.asUInt.orR
  val s1_invalid_way_en = ParallelPriorityMux(s1_invalid_vec.zipWithIndex.map(x => x._1 -> UIntToOH(x._2.U(nWays.W))))
  val s1_repl_way_en_oh = Mux(s1_have_invalid_way, s1_invalid_way_en, UIntToOH(io.replace_way.way))
  val s1_repl_way_en_enc = OHToUInt(s1_repl_way_en_oh)
  val s1_repl_tag = Mux1H(s1_repl_way_en_oh, wayMap(w => tag_resp(w)))
  val s1_repl_coh = Mux1H(s1_repl_way_en_oh, wayMap(w => meta_resp(w).coh))
  val s1_repl_extra_meta = Mux1H(s1_repl_way_en_oh, wayMap(w => io.extra_meta_resp(w)))

  val s1_need_replacement = !s1_tag_match_dup_dc
  val s1_way_en = Mux(s1_need_replacement, s1_repl_way_en_oh, s1_tag_match_way_dup_dc)
  val s1_coh = Mux(s1_need_replacement, s1_repl_coh, s1_hit_coh)
  val s1_tag = Mux(s1_need_replacement, s1_repl_tag, get_tag(s1_paddr_dup_dcache))

  XSPerfAccumulate("load_has_invalid_way_but_select_valid_way", io.replace_way.set.valid && wayMap(w => !meta_resp(w).coh.isValid()).asUInt.orR && s1_need_replacement && s1_repl_coh.isValid())
  XSPerfAccumulate("load_using_replacement", io.replace_way.set.valid && s1_need_replacement)

  // data read
  io.banked_data_read.valid := s1_fire && !s1_nack
  io.banked_data_read.bits.addr := s1_vaddr
  io.banked_data_read.bits.way_en := s1_tag_match_way_dup_dc

  // get s1_will_send_miss_req in lpad_s1
  val s1_has_permission = s1_hit_coh.onAccess(s1_req.cmd)._1
  val s1_new_hit_coh = s1_hit_coh.onAccess(s1_req.cmd)._3
  val s1_hit = s1_tag_match_dup_dc && s1_has_permission && s1_hit_coh === s1_new_hit_coh
  val s1_will_send_miss_req = s1_valid && !s1_nack && !s1_nack_data && !s1_hit

  // check ecc error
  val s1_encTag = Mux1H(s1_tag_match_way_dup_dc, wayMap((w: Int) => io.tag_resp(w)))
  val s1_flag_error = Mux(s1_need_replacement, false.B, s1_hit_error) // error reported by exist dcache error bit

  // --------------------------------------------------------------------------------
  // stage 2
  // --------------------------------------------------------------------------------
  // return data

  // val s2_valid = RegEnable(next = s1_valid && !io.lsu.s1_kill, init = false.B, enable = s1_fire)
  val s2_valid = RegInit(false.B)
  val s2_req = RegEnable(s1_req, s1_fire)
  val s2_paddr = RegEnable(s1_paddr_dup_dcache, s1_fire)
  val s2_vaddr = RegEnable(s1_vaddr, s1_fire)
  val s2_bank_oh = RegEnable(s1_bank_oh, s1_fire)
  val s2_bank_oh_dup_0 = RegEnable(s1_bank_oh, s1_fire)
  s2_ready := true.B

  val s2_fire = s2_valid

  when (s1_fire) { s2_valid := !io.lsu.s1_kill }
  .elsewhen(io.lsu.resp.fire()) { s2_valid := false.B }

  dump_pipeline_reqs("LoadPipe s2", s2_valid, s2_req)


  // hit, miss, nack, permission checking
  // dcache side tag match
  val s2_tag_match_way = RegEnable(s1_tag_match_way_dup_dc, s1_fire)
  val s2_tag_match = RegEnable(s1_tag_match_dup_dc, s1_fire)

  // lsu side tag match
  val s2_hit_dup_lsu = RegNext(s1_tag_match_dup_lsu)

  io.lsu.s2_hit := s2_hit_dup_lsu && !s2_wpu_pred_fail

  val s2_hit_meta = RegEnable(s1_hit_meta, s1_fire)
  val s2_hit_coh = RegEnable(s1_hit_coh, s1_fire)
  val s2_has_permission = s2_hit_coh.onAccess(s2_req.cmd)._1 // for write prefetch
  val s2_new_hit_coh = s2_hit_coh.onAccess(s2_req.cmd)._3 // for write prefetch

  val s2_way_en = RegEnable(s1_way_en, s1_fire)
  val s2_repl_coh = RegEnable(s1_repl_coh, s1_fire)
  val s2_repl_tag = RegEnable(s1_repl_tag, s1_fire)
  val s2_repl_extra_meta = RegEnable(s1_repl_extra_meta, s1_fire) // not used for now
  val s2_encTag = RegEnable(s1_encTag, s1_fire)

  // when req got nacked, upper levels should replay this request
  // nacked or not
  val s2_nack_hit = RegEnable(s1_nack, s1_fire)
  // can no allocate mshr for load miss
  val s2_nack_no_mshr = io.miss_req.valid && !io.miss_req.ready
  // Bank conflict on data arrays
  val s2_nack_data = RegEnable(!io.banked_data_read.ready, s1_fire)
  val s2_nack = s2_nack_hit || s2_nack_no_mshr || s2_nack_data
  // s2 miss merged
  val s2_miss_merged = io.miss_req.fire && !io.miss_req.bits.cancel && io.miss_resp.merged

  val s2_bank_addr = addr_to_dcache_bank(s2_paddr)
  dontTouch(s2_bank_addr)

  val s2_instrtype = s2_req.instrtype

  val s2_tag_error = dcacheParameters.tagCode.decode(s2_encTag).error // error reported by tag ecc check
  val s2_flag_error = RegEnable(s1_flag_error, s1_fire)

  val s2_hit_prefetch = RegEnable(s1_hit_prefetch, s1_fire)
  val s2_hit_access = RegEnable(s1_hit_access, s1_fire)

  val s2_hit = s2_tag_match && s2_has_permission && s2_hit_coh === s2_new_hit_coh && !s2_wpu_pred_fail

  // only dump these signals when they are actually valid
  dump_pipeline_valids("LoadPipe s2", "s2_hit", s2_valid && s2_hit)
  dump_pipeline_valids("LoadPipe s2", "s2_nack", s2_valid && s2_nack)
  dump_pipeline_valids("LoadPipe s2", "s2_nack_hit", s2_valid && s2_nack_hit)
  dump_pipeline_valids("LoadPipe s2", "s2_nack_no_mshr", s2_valid && s2_nack_no_mshr)

  val s2_can_send_miss_req = RegEnable(s1_will_send_miss_req, s1_fire)

  // send load miss to miss queue
  io.miss_req.valid := s2_valid && s2_can_send_miss_req && !s2_wpu_pred_fail
  io.miss_req.bits := DontCare
  io.miss_req.bits.source := s2_instrtype
  io.miss_req.bits.cmd := s2_req.cmd
  io.miss_req.bits.addr := get_block_addr(s2_paddr)
  io.miss_req.bits.vaddr := s2_vaddr
  io.miss_req.bits.way_en := s2_way_en
  io.miss_req.bits.req_coh := s2_hit_coh
  io.miss_req.bits.replace_coh := s2_repl_coh
  io.miss_req.bits.replace_tag := s2_repl_tag
  io.miss_req.bits.cancel := io.lsu.s2_kill || s2_tag_error
  io.miss_req.bits.pc := io.lsu.s2_pc

  // send back response
  val resp = Wire(ValidIO(new DCacheWordResp))
  resp.valid := s2_valid
  resp.bits := DontCare
  // resp.bits.data := s2_word_decoded
  // resp.bits.data := banked_data_resp_word.raw_data
  // * on miss or nack, upper level should replay request
  // but if we successfully sent the request to miss queue
  // upper level does not need to replay request
  // they can sit in load queue and wait for refill
  //
  // * report a miss if bank conflict is detected
  val real_miss = Wire(Bool())
  when (wpu.io.resp.valid){
    real_miss := !s2_real_way_en.orR
  }.otherwise{
    real_miss := !s2_hit_dup_lsu
  }
  // io.debug_s2_cache_miss := real_miss
  resp.bits.miss := real_miss || io.bank_conflict_slow || s2_wpu_pred_fail
  io.lsu.s2_first_hit := s2_req.isFirstIssue && s2_hit
  // load pipe need replay when there is a bank conflict or wpu predict fail
  resp.bits.replay := (resp.bits.miss && (!io.miss_req.fire() || s2_nack)) || io.bank_conflict_slow || s2_wpu_pred_fail
  resp.bits.replayCarry.valid := resp.bits.miss
  resp.bits.replayCarry.real_way_en := s2_real_way_en
  resp.bits.meta_prefetch := s2_hit_prefetch
  resp.bits.meta_access := s2_hit_access
  resp.bits.tag_error := s2_tag_error // report tag_error in load s2
  resp.bits.mshr_id := io.miss_resp.id
  resp.bits.handled := io.miss_req.fire && !io.miss_req.bits.cancel && io.miss_resp.handled
  resp.bits.debug_robIdx := s2_req.debug_robIdx

  XSPerfAccumulate("wpu_pred_fail", s2_wpu_pred_fail && s2_valid)
  XSPerfAccumulate("dcache_read_bank_conflict", io.bank_conflict_slow && s2_valid)
  XSPerfAccumulate("dcache_read_from_prefetched_line", s2_valid && s2_hit_prefetch && !resp.bits.miss)
  XSPerfAccumulate("dcache_first_read_from_prefetched_line", s2_valid && s2_hit_prefetch && !resp.bits.miss && !s2_hit_access)

  io.lsu.resp.valid := resp.valid
  io.lsu.resp.bits := resp.bits
  assert(RegNext(!(resp.valid && !io.lsu.resp.ready)), "lsu should be ready in s2")

  when (resp.valid) {
    resp.bits.dump()
  }

  io.lsu.debug_s1_hit_way := s1_tag_match_way_dup_dc
  io.lsu.s1_disable_fast_wakeup := io.disable_ld_fast_wakeup
  io.lsu.s2_bank_conflict := io.bank_conflict_slow
  assert(RegNext(s1_ready && s2_ready), "load pipeline should never be blocked")

  // --------------------------------------------------------------------------------
  // stage 3
  // --------------------------------------------------------------------------------
  // report ecc error and get selected dcache data

  val s3_valid = RegNext(s2_valid)
  val s3_vaddr = RegEnable(s2_vaddr, s2_fire)
  val s3_paddr = RegEnable(s2_paddr, s2_fire)
  val s3_hit = RegEnable(s2_hit, s2_fire)
  val s3_tag_match_way = RegEnable(s2_tag_match_way, s2_fire)

  val s3_banked_data_resp_word = io.banked_data_resp.raw_data
  val s3_data_error = io.read_error_delayed && s3_hit // banked_data_resp_word.error && !bank_conflict
  val s3_tag_error = RegEnable(s2_tag_error, s2_fire)
  val s3_flag_error = RegEnable(s2_flag_error, s2_fire)
  val s3_error = s3_tag_error || s3_flag_error || s3_data_error

  // error_delayed signal will be used to update uop.exception 1 cycle after load writeback
  resp.bits.error_delayed := s3_error && (s3_hit || s3_tag_error) && s3_valid
  resp.bits.data_delayed := s3_banked_data_resp_word
  resp.bits.replacementUpdated := io.replace_access.valid
  
  // report tag / data / l2 error (with paddr) to bus error unit
  io.error := 0.U.asTypeOf(new L1CacheErrorInfo())
  io.error.report_to_beu := (s3_tag_error || s3_data_error) && s3_valid
  io.error.paddr := s3_paddr
  io.error.source.tag := s3_tag_error
  io.error.source.data := s3_data_error
  io.error.source.l2 := s3_flag_error
  io.error.opType.load := true.B
  // report tag error / l2 corrupted to CACHE_ERROR csr
  io.error.valid := s3_error && s3_valid

  // update plru in s3
  val s3_miss_merged = RegNext(s2_miss_merged)
  val first_update = RegNext(RegNext(RegNext(!io.lsu.replacementUpdated)))
  val hit_update_replace_en  = RegNext(s2_valid) && RegNext(!resp.bits.miss)
  val miss_update_replace_en = RegNext(io.miss_req.fire) && RegNext(!io.miss_req.bits.cancel) && RegNext(io.miss_resp.handled)

  if (!cfg.updateReplaceOn2ndmiss) {
    // replacement is only updated on 1st miss
    // io.replace_access.valid := RegNext(RegNext(
    //   RegNext(io.meta_read.fire()) && s1_valid && !io.lsu.s1_kill) && 
    //   !s2_nack_no_mshr &&
    //   !s2_miss_merged
    // )
    io.replace_access.valid := (hit_update_replace_en || (miss_update_replace_en && !s3_miss_merged)) && first_update
    io.replace_access.bits.set := RegNext(RegNext(get_idx(s1_req.vaddr)))
    io.replace_access.bits.way := RegNext(RegNext(Mux(s1_tag_match_dup_dc, OHToUInt(s1_tag_match_way_dup_dc), s1_repl_way_en_enc)))
  } else {
    // replacement is updated on both 1st and 2nd miss
    // timing is worse than !cfg.updateReplaceOn2ndmiss
    // io.replace_access.valid := RegNext(RegNext(
    //   RegNext(io.meta_read.fire()) && s1_valid && !io.lsu.s1_kill) &&
    //   !s2_nack_no_mshr &&
    //   // replacement is updated on 2nd miss only when this req is firstly issued
    //   (!s2_miss_merged || s2_req.isFirstIssue)
    // )
    io.replace_access.valid := (hit_update_replace_en || miss_update_replace_en) && first_update
    io.replace_access.bits.set := RegNext(RegNext(get_idx(s1_req.vaddr)))
    io.replace_access.bits.way := RegNext(
      Mux(
        RegNext(s1_tag_match_dup_dc),
        RegNext(OHToUInt(s1_tag_match_way_dup_dc)), // if hit, access hit way in plru
        Mux( // if miss
          !s2_miss_merged,
          RegNext(s1_repl_way_en_enc), // 1st fire: access new selected replace way
          OHToUInt(io.miss_resp.repl_way_en) // 2nd fire: access replace way selected at miss queue allocate time
        )
      )
    )
  }

  // update access bit
  io.access_flag_write.valid := s3_valid && s3_hit
  io.access_flag_write.bits.idx := get_idx(s3_vaddr)
  io.access_flag_write.bits.way_en := s3_tag_match_way
  io.access_flag_write.bits.flag := true.B

  // --------------------------------------------------------------------------------
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
  XSPerfAccumulate("load_req", io.lsu.req.fire())
  XSPerfAccumulate("load_s1_kill", s1_fire && io.lsu.s1_kill)
  XSPerfAccumulate("load_hit_way", s1_fire && s1_tag_match_dup_dc)
  XSPerfAccumulate("load_replay", io.lsu.resp.fire() && resp.bits.replay)
  XSPerfAccumulate("load_replay_for_data_nack", io.lsu.resp.fire() && resp.bits.replay && s2_nack_data)
  XSPerfAccumulate("load_replay_for_no_mshr", io.lsu.resp.fire() && resp.bits.replay && s2_nack_no_mshr)
  XSPerfAccumulate("load_replay_for_conflict", io.lsu.resp.fire() && resp.bits.replay && io.bank_conflict_slow)
  XSPerfAccumulate("load_hit", io.lsu.resp.fire() && !real_miss)
  XSPerfAccumulate("load_miss", io.lsu.resp.fire() && real_miss)
  XSPerfAccumulate("load_succeed", io.lsu.resp.fire() && !resp.bits.miss && !resp.bits.replay)
  XSPerfAccumulate("load_miss_or_conflict", io.lsu.resp.fire() && resp.bits.miss)
  XSPerfAccumulate("actual_ld_fast_wakeup", s1_fire && s1_tag_match_dup_dc && !io.disable_ld_fast_wakeup)
  XSPerfAccumulate("ideal_ld_fast_wakeup", io.banked_data_read.fire() && s1_tag_match_dup_dc)

  val perfEvents = Seq(
    ("load_req                 ", io.lsu.req.fire()                                               ),
    ("load_replay              ", io.lsu.resp.fire() && resp.bits.replay                          ),
    ("load_replay_for_data_nack", io.lsu.resp.fire() && resp.bits.replay && s2_nack_data          ),
    ("load_replay_for_no_mshr  ", io.lsu.resp.fire() && resp.bits.replay && s2_nack_no_mshr       ),
    ("load_replay_for_conflict ", io.lsu.resp.fire() && resp.bits.replay && io.bank_conflict_slow ),
  )
  generatePerfEvent()
}
