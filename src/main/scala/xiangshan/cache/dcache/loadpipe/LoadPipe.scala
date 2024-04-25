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

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink.ClientMetadata
import utils.{HasPerfEvents, XSDebug, XSPerfAccumulate}
import utility.{ParallelPriorityMux, OneHot, ChiselDB, ParallelORR, ParallelMux}
import xiangshan.{XSCoreParamsKey, L1CacheErrorInfo}
import xiangshan.cache.wpu._
import xiangshan.mem.HasL1PrefetchSourceParameter
import xiangshan.mem.prefetch._
import xiangshan.mem.LqPtr

class LoadPfDbBundle(implicit p: Parameters) extends DCacheBundle {
  val paddr = UInt(PAddrBits.W)
}

class LoadPipe(id: Int)(implicit p: Parameters) extends DCacheModule with HasPerfEvents with HasL1PrefetchSourceParameter {
  val io = IO(new DCacheBundle {
    // incoming requests
    val lsu = Flipped(new DCacheLoadIO)
    val dwpu = Flipped(new DwpuBaseIO(nWays = nWays, nPorts = 1))
    val load128Req = Input(Bool())
    // req got nacked in stage 0?
    val nack      = Input(Bool())

    // meta and data array read port
    val meta_read = DecoupledIO(new MetaReadReq)
    val meta_resp = Input(Vec(nWays, new Meta))
    val extra_meta_resp = Input(Vec(nWays, new DCacheExtraMeta))

    val tag_read = DecoupledIO(new TagReadReq)
    val tag_resp = Input(Vec(nWays, UInt(encTagBits.W)))
    val vtag_update = Flipped(DecoupledIO(new TagWriteReq))

    val banked_data_read = DecoupledIO(new L1BankedDataReadReqWithMask)
    val is128Req = Output(Bool())
    val banked_data_resp = Input(Vec(VLEN/DCacheSRAMRowBits, new L1BankedDataReadResult()))
    val read_error_delayed = Input(Vec(VLEN/DCacheSRAMRowBits, Bool()))

    // access bit update
    val access_flag_write = DecoupledIO(new FlagMetaWriteReq)
    val prefetch_flag_write = DecoupledIO(new SourceMetaWriteReq)

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

    val prefetch_info = new Bundle {
      val naive = new Bundle {
        val total_prefetch = Output(Bool())
        val late_hit_prefetch = Output(Bool())
        val late_prefetch_hit = Output(Bool())
        val late_load_hit = Output(Bool())
        val useless_prefetch = Output(Bool())
        val useful_prefetch = Output(Bool())
        val prefetch_hit = Output(Bool())
      }

      val fdp = new Bundle {
        val useful_prefetch = Output(Bool())
        val demand_miss = Output(Bool())
        val pollution = Output(Bool())
      }
    }

    val bloom_filter_query = new Bundle {
      val query = ValidIO(new BloomQueryBundle(BLOOM_FILTER_ENTRY_NUM))
      val resp = Flipped(ValidIO(new BloomRespBundle()))
    }

    val counter_filter_query = new CounterFilterQueryBundle
    val counter_filter_enq = new ValidIO(new CounterFilterDataBundle())

    // miss queue cancel the miss request
    val mq_enq_cancel = Input(Bool())
  })

  assert(RegNext(io.meta_read.ready))

  val s1_ready = Wire(Bool())
  val s2_ready = Wire(Bool())
  // LSU requests
  // it you got nacked, you can directly passdown
  val not_nacked_ready = io.meta_read.ready && io.tag_read.ready && s1_ready
  val nacked_ready     = true.B

  // Pipeline
  // --------------------------------------------------------------------------------
  // stage 0
  // --------------------------------------------------------------------------------
  // read tag

  // ready can wait for valid
  io.lsu.req.ready := (!io.nack && not_nacked_ready) || (io.nack && nacked_ready)
  io.meta_read.valid := io.lsu.req.fire && !io.nack
  io.tag_read.valid := io.lsu.req.fire && !io.nack

  val s0_valid = io.lsu.req.fire
  val s0_req = io.lsu.req.bits
  val s0_fire = s0_valid && s1_ready
  val s0_vaddr = s0_req.vaddr
  val s0_replayCarry = s0_req.replayCarry
  val s0_load128Req = io.load128Req
  val s0_bank_oh_64 = UIntToOH(addr_to_dcache_bank(s0_vaddr))
  val s0_bank_oh_128 = (s0_bank_oh_64 << 1.U).asUInt | s0_bank_oh_64.asUInt
  val s0_bank_oh = Mux(s0_load128Req, s0_bank_oh_128, s0_bank_oh_64)
  assert(RegNext(!(s0_valid && (s0_req.cmd =/= MemoryOpConstants.M_XRD && s0_req.cmd =/= MemoryOpConstants.M_PFR && s0_req.cmd =/= MemoryOpConstants.M_PFW))), "LoadPipe only accepts load req / softprefetch read or write!")
  dump_pipeline_reqs("LoadPipe s0", s0_valid, s0_req)

  // wpu
  // val dwpu = Module(new DCacheWpuWrapper)
  // req in s0
  if(dwpuParam.enWPU){
    io.dwpu.req(0).bits.vaddr := s0_vaddr
    io.dwpu.req(0).bits.replayCarry := s0_replayCarry
    io.dwpu.req(0).valid := s0_valid
  }else{
    io.dwpu.req(0).valid := false.B
    io.dwpu.req(0).bits := DontCare
  }


  val meta_read = io.meta_read.bits
  val tag_read = io.tag_read.bits

  // Tag read for new requests
  meta_read.idx := get_idx(io.lsu.req.bits.vaddr)
  meta_read.way_en := ~0.U(nWays.W)
  // meta_read.tag := DontCare

  tag_read.idx := get_idx(io.lsu.req.bits.vaddr)
  tag_read.way_en := ~0.U(nWays.W)

  // --------------------------------------------------------------------------------
  // stage 1
  // --------------------------------------------------------------------------------
  // tag match, read data

  val s1_valid = RegInit(false.B)
  val s1_req = RegEnable(s0_req, s0_fire)
  // in stage 1, load unit gets the physical address
  val s1_paddr_dup_lsu = io.lsu.s1_paddr_dup_lsu
  val s1_paddr_dup_dcache = io.lsu.s1_paddr_dup_dcache
  val s1_load128Req = RegEnable(s0_load128Req, s0_fire)
  // LSU may update the address from io.lsu.s1_paddr, which affects the bank read enable only.
  val s1_vaddr = Cat(s1_req.vaddr(VAddrBits - 1, blockOffBits), io.lsu.s1_paddr_dup_lsu(blockOffBits - 1, 0))
  val s1_bank_oh = RegEnable(s0_bank_oh, s0_fire)
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

  // resp in s1
  val s1_tag_match_way_dup_dc = wayMap((w: Int) => tag_resp(w) === get_tag(s1_paddr_dup_dcache) && meta_resp(w).coh.isValid()).asUInt
  val s1_tag_match_way_dup_lsu = wayMap((w: Int) => tag_resp(w) === get_tag(s1_paddr_dup_lsu) && meta_resp(w).coh.isValid()).asUInt
  val s1_wpu_pred_valid = RegEnable(io.dwpu.resp(0).valid, s0_fire)
  val s1_wpu_pred_way_en = RegEnable(io.dwpu.resp(0).bits.s0_pred_way_en, s0_fire)

  // lookup update
  io.dwpu.lookup_upd(0).valid := s1_valid
  io.dwpu.lookup_upd(0).bits.vaddr := s1_vaddr
  io.dwpu.lookup_upd(0).bits.s1_real_way_en := s1_tag_match_way_dup_dc
  io.dwpu.lookup_upd(0).bits.s1_pred_way_en := s1_wpu_pred_way_en
  // replace / tag write
  io.vtag_update.ready := true.B
  // dwpu.io.tagwrite_upd.valid := io.vtag_update.valid
  // dwpu.io.tagwrite_upd.bits.vaddr := io.vtag_update.bits.vaddr
  // dwpu.io.tagwrite_upd.bits.s1_real_way_en := io.vtag_update.bits.way_en

  val s1_direct_map_way_num = get_direct_map_way(s1_req.vaddr)
  if(dwpuParam.enCfPred || !env.FPGAPlatform){
    /* method1: record the pc */
    // if (!env.FPGAPlatform){
    //    io.dwpu.cfpred(0).s0_vaddr := io.lsu.s0_pc
    //    io.dwpu.cfpred(0).s1_vaddr := io.lsu.s1_pc
    // }

    /* method2: record the vaddr */
    io.dwpu.cfpred(0).s0_vaddr := s0_vaddr
    io.dwpu.cfpred(0).s1_vaddr := s1_vaddr
    // whether direct_map_way miss with valid tag value
    io.dwpu.cfpred(0).s1_dm_hit := wayMap((w: Int) => w.U === s1_direct_map_way_num && tag_resp(w) === get_tag(s1_paddr_dup_lsu) && meta_resp(w).coh.isValid()).asUInt.orR
  }else{
    io.dwpu.cfpred(0) := DontCare
  }

  val s1_pred_tag_match_way_dup_dc = Wire(UInt(nWays.W))
  val s1_wpu_pred_fail = Wire(Bool())
  val s1_wpu_pred_fail_and_real_hit = Wire(Bool())
  if (dwpuParam.enWPU) {
    when(s1_wpu_pred_valid) {
      s1_pred_tag_match_way_dup_dc := s1_wpu_pred_way_en
    }.otherwise {
      s1_pred_tag_match_way_dup_dc := s1_tag_match_way_dup_dc
    }
    s1_wpu_pred_fail := s1_valid && s1_tag_match_way_dup_dc =/= s1_pred_tag_match_way_dup_dc
    s1_wpu_pred_fail_and_real_hit := s1_wpu_pred_fail && s1_tag_match_way_dup_dc.orR
  } else {
    s1_pred_tag_match_way_dup_dc := s1_tag_match_way_dup_dc
    s1_wpu_pred_fail := false.B
    s1_wpu_pred_fail_and_real_hit := false.B
  }

  val s1_tag_match_dup_dc = ParallelORR(s1_tag_match_way_dup_dc)
  val s1_tag_match_dup_lsu = ParallelORR(s1_tag_match_way_dup_lsu)
  assert(RegNext(!s1_valid || PopCount(s1_tag_match_way_dup_dc) <= 1.U), "tag should not match with more than 1 way")

  // when there are no tag match, we give it a Fake Meta
  // this simplifies our logic in s2 stage
  val s1_hit_meta = ParallelMux(s1_tag_match_way_dup_dc.asBools, (0 until nWays).map(w => meta_resp(w)))
  val s1_hit_coh = s1_hit_meta.coh
  val s1_hit_error = ParallelMux(s1_tag_match_way_dup_dc.asBools, (0 until nWays).map(w => io.extra_meta_resp(w).error))
  val s1_hit_prefetch = ParallelMux(s1_tag_match_way_dup_dc.asBools, (0 until nWays).map(w => io.extra_meta_resp(w).prefetch))
  val s1_hit_access = ParallelMux(s1_tag_match_way_dup_dc.asBools, (0 until nWays).map(w => io.extra_meta_resp(w).access))

  // io.replace_way.set.valid := RegNext(s0_fire)
  io.replace_way.set.valid := false.B
  io.replace_way.set.bits := get_idx(s1_vaddr)
  io.replace_way.dmWay := get_direct_map_way(s1_vaddr)
  val s1_invalid_vec = wayMap(w => !meta_resp(w).coh.isValid())
  val s1_have_invalid_way = s1_invalid_vec.asUInt.orR
  val s1_invalid_way_en = ParallelPriorityMux(s1_invalid_vec.zipWithIndex.map(x => x._1 -> UIntToOH(x._2.U(nWays.W))))

  val s1_need_replacement = !s1_tag_match_dup_dc

  XSPerfAccumulate("load_using_replacement", io.replace_way.set.valid && s1_need_replacement)

  // data read
  io.banked_data_read.valid := s1_fire && !s1_nack && !io.lsu.s1_kill
  io.banked_data_read.bits.addr := s1_vaddr
  io.banked_data_read.bits.way_en := s1_pred_tag_match_way_dup_dc
  io.banked_data_read.bits.bankMask := s1_bank_oh
  io.is128Req := s1_load128Req

  // query bloom filter
  io.bloom_filter_query.query.valid := s1_valid
  io.bloom_filter_query.query.bits.addr := io.bloom_filter_query.query.bits.get_addr(s1_paddr_dup_dcache)

  // get s1_will_send_miss_req in lpad_s1
  val s1_has_permission = s1_hit_coh.onAccess(s1_req.cmd)._1
  val s1_new_hit_coh = s1_hit_coh.onAccess(s1_req.cmd)._3
  val s1_hit = s1_tag_match_dup_dc && s1_has_permission && s1_hit_coh === s1_new_hit_coh
  val s1_will_send_miss_req = s1_valid && !s1_nack && !s1_nack_data && !s1_hit

  // check ecc error
  val s1_encTag = ParallelMux(s1_tag_match_way_dup_dc.asBools, (0 until nWays).map(w => io.tag_resp(w)))
  val s1_flag_error = Mux(s1_need_replacement, false.B, s1_hit_error) // error reported by exist dcache error bit

  // --------------------------------------------------------------------------------
  // stage 2
  // --------------------------------------------------------------------------------
  // return data

  // val s2_valid = RegEnable(next = s1_valid && !io.lsu.s1_kill, init = false.B, enable = s1_fire)
  val s2_valid = RegInit(false.B)
  val s2_req = RegEnable(s1_req, s1_fire)
  val s2_load128Req = RegEnable(s1_load128Req, s1_fire)
  val s2_paddr = RegEnable(s1_paddr_dup_dcache, s1_fire)
  val s2_vaddr = RegEnable(s1_vaddr, s1_fire)
  val s2_bank_oh = RegEnable(s1_bank_oh, s1_fire)
  val s2_bank_oh_dup_0 = RegEnable(s1_bank_oh, s1_fire)
  val s2_wpu_pred_fail = RegEnable(s1_wpu_pred_fail, s1_fire)
  val s2_real_way_en = RegEnable(s1_tag_match_way_dup_dc, s1_fire)
  val s2_pred_way_en = RegEnable(s1_pred_tag_match_way_dup_dc, s1_fire)
  val s2_dm_way_num = RegEnable(s1_direct_map_way_num, s1_fire)
  val s2_wpu_pred_fail_and_real_hit = RegEnable(s1_wpu_pred_fail_and_real_hit, s1_fire)

  s2_ready := true.B

  val s2_fire = s2_valid

  when (s1_fire) { s2_valid := !io.lsu.s1_kill }
  .elsewhen(io.lsu.resp.fire) { s2_valid := false.B }

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
  val s2_miss_merged = io.miss_req.fire && !io.mq_enq_cancel && io.miss_resp.merged

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
  io.miss_req.valid := s2_valid && s2_can_send_miss_req
  io.miss_req.bits := DontCare
  io.miss_req.bits.source := s2_instrtype
  io.miss_req.bits.pf_source := RegNext(RegNext(io.lsu.pf_source))
  io.miss_req.bits.cmd := s2_req.cmd
  io.miss_req.bits.addr := get_block_addr(s2_paddr)
  io.miss_req.bits.vaddr := s2_vaddr
  io.miss_req.bits.req_coh := s2_hit_coh
  io.miss_req.bits.cancel := io.lsu.s2_kill || s2_tag_error
  io.miss_req.bits.pc := io.lsu.s2_pc
  io.miss_req.bits.lqIdx := io.lsu.req.bits.lqIdx
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
  val real_miss = !s2_real_way_en.orR

  resp.bits.real_miss := real_miss
  resp.bits.miss := real_miss
  io.lsu.s2_first_hit := s2_req.isFirstIssue && s2_hit
  // load pipe need replay when there is a bank conflict or wpu predict fail
  resp.bits.replay := (resp.bits.miss && (!io.miss_req.fire || s2_nack || io.mq_enq_cancel)) || io.bank_conflict_slow || s2_wpu_pred_fail
  resp.bits.replayCarry.valid := (resp.bits.miss && (!io.miss_req.fire || s2_nack || io.mq_enq_cancel)) || io.bank_conflict_slow || s2_wpu_pred_fail
  resp.bits.replayCarry.real_way_en := s2_real_way_en
  resp.bits.meta_prefetch := s2_hit_prefetch
  resp.bits.meta_access := s2_hit_access
  resp.bits.tag_error := s2_tag_error // report tag_error in load s2
  resp.bits.mshr_id := io.miss_resp.id
  resp.bits.handled := io.miss_req.fire && !io.mq_enq_cancel && io.miss_resp.handled
  resp.bits.debug_robIdx := s2_req.debug_robIdx
  // debug info
  io.lsu.s2_first_hit := s2_req.isFirstIssue && s2_hit
  io.lsu.debug_s2_real_way_num := OneHot.OHToUIntStartOne(s2_real_way_en)
  if(dwpuParam.enWPU) {
    io.lsu.debug_s2_pred_way_num := OneHot.OHToUIntStartOne(s2_pred_way_en)
  }else{
    io.lsu.debug_s2_pred_way_num := 0.U
  }
  if(dwpuParam.enWPU && dwpuParam.enCfPred || !env.FPGAPlatform){
    io.lsu.debug_s2_dm_way_num :=  s2_dm_way_num + 1.U
  }else{
    io.lsu.debug_s2_dm_way_num := 0.U
  }


  XSPerfAccumulate("dcache_read_bank_conflict", io.bank_conflict_slow && s2_valid)
  XSPerfAccumulate("dcache_read_from_prefetched_line", s2_valid && isFromL1Prefetch(s2_hit_prefetch) && !resp.bits.miss)
  XSPerfAccumulate("dcache_first_read_from_prefetched_line", s2_valid && isFromL1Prefetch(s2_hit_prefetch) && !resp.bits.miss && !s2_hit_access)

  // if ldu0 and ldu1 hit the same, count for 1
  val total_prefetch = s2_valid && (s2_req.instrtype === DCACHE_PREFETCH_SOURCE.U)
  val late_hit_prefetch = s2_valid && s2_hit && (s2_req.instrtype === DCACHE_PREFETCH_SOURCE.U)
  val late_load_hit = s2_valid && s2_hit && (s2_req.instrtype === DCACHE_PREFETCH_SOURCE.U) && !isFromL1Prefetch(s2_hit_prefetch)
  val late_prefetch_hit = s2_valid && s2_hit && (s2_req.instrtype === DCACHE_PREFETCH_SOURCE.U) && isFromL1Prefetch(s2_hit_prefetch)
  val useless_prefetch = io.miss_req.valid && io.miss_req.ready && (s2_req.instrtype === DCACHE_PREFETCH_SOURCE.U)
  val useful_prefetch = s2_valid && (s2_req.instrtype === DCACHE_PREFETCH_SOURCE.U) && resp.bits.handled && !io.miss_resp.merged

  val prefetch_hit = s2_valid && (s2_req.instrtype =/= DCACHE_PREFETCH_SOURCE.U) && s2_hit && isFromL1Prefetch(s2_hit_prefetch) && s2_req.isFirstIssue

  io.prefetch_info.naive.total_prefetch := total_prefetch
  io.prefetch_info.naive.late_hit_prefetch := late_hit_prefetch
  io.prefetch_info.naive.late_load_hit := late_load_hit
  io.prefetch_info.naive.late_prefetch_hit := late_prefetch_hit
  io.prefetch_info.naive.useless_prefetch := useless_prefetch
  io.prefetch_info.naive.useful_prefetch := useful_prefetch
  io.prefetch_info.naive.prefetch_hit := prefetch_hit

  io.prefetch_info.fdp.demand_miss := s2_valid && (s2_req.instrtype =/= DCACHE_PREFETCH_SOURCE.U) && !s2_hit && s2_req.isFirstIssue
  io.prefetch_info.fdp.pollution := io.prefetch_info.fdp.demand_miss && io.bloom_filter_query.resp.valid && io.bloom_filter_query.resp.bits.res

  io.lsu.resp.valid := resp.valid
  io.lsu.resp.bits := resp.bits
  assert(RegNext(!(resp.valid && !io.lsu.resp.ready)), "lsu should be ready in s2")

  when (resp.valid) {
    resp.bits.dump()
  }

  io.lsu.debug_s1_hit_way := s1_tag_match_way_dup_dc
  io.lsu.s1_disable_fast_wakeup := io.disable_ld_fast_wakeup
  io.lsu.s2_bank_conflict := io.bank_conflict_slow
  io.lsu.s2_wpu_pred_fail := s2_wpu_pred_fail_and_real_hit
  io.lsu.s2_mq_nack       := (resp.bits.miss && (!io.miss_req.fire || s2_nack || io.mq_enq_cancel))
  assert(RegNext(s1_ready && s2_ready), "load pipeline should never be blocked")

  // --------------------------------------------------------------------------------
  // stage 3
  // --------------------------------------------------------------------------------
  // report ecc error and get selected dcache data

  val s3_valid = RegNext(s2_valid)
  val s3_load128Req = RegEnable(s2_load128Req, s2_fire)
  val s3_vaddr = RegEnable(s2_vaddr, s2_fire)
  val s3_paddr = RegEnable(s2_paddr, s2_fire)
  val s3_hit = RegEnable(s2_hit, s2_fire)
  val s3_tag_match_way = RegEnable(s2_tag_match_way, s2_fire)
  val s3_req_instrtype = RegEnable(s2_req.instrtype, s2_fire)
  val s3_is_prefetch = s3_req_instrtype === DCACHE_PREFETCH_SOURCE.U

  val s3_data128bit = Cat(io.banked_data_resp(1).raw_data, io.banked_data_resp(0).raw_data)
  val s3_data64bit = Fill(2, io.banked_data_resp(0).raw_data)
  val s3_banked_data_resp_word = Mux(s3_load128Req, s3_data128bit, s3_data64bit)
  val s3_data_error = Mux(s3_load128Req, io.read_error_delayed.asUInt.orR, io.read_error_delayed(0)) && s3_hit
  val s3_tag_error = RegEnable(s2_tag_error, s2_fire)
  val s3_flag_error = RegEnable(s2_flag_error, s2_fire)
  val s3_hit_prefetch = RegEnable(s2_hit_prefetch, s2_fire)
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
  val miss_update_replace_en = RegNext(io.miss_req.fire) && RegNext(!io.mq_enq_cancel) && RegNext(io.miss_resp.handled)

  io.replace_access.valid := s3_valid && s3_hit
  io.replace_access.bits.set := RegNext(RegNext(get_idx(s1_req.vaddr)))
  io.replace_access.bits.way := RegNext(RegNext(OHToUInt(s1_tag_match_way_dup_dc)))

  // update access bit
  io.access_flag_write.valid := s3_valid && s3_hit && !s3_is_prefetch
  io.access_flag_write.bits.idx := get_idx(s3_vaddr)
  io.access_flag_write.bits.way_en := s3_tag_match_way
  io.access_flag_write.bits.flag := true.B

  // clear prefetch source when prefetch hit
  val s3_clear_pf_flag_en = s3_valid && s3_hit && !s3_is_prefetch && isFromL1Prefetch(s3_hit_prefetch)
  io.prefetch_flag_write.valid := s3_clear_pf_flag_en && !io.counter_filter_query.resp
  io.prefetch_flag_write.bits.idx := get_idx(s3_vaddr)
  io.prefetch_flag_write.bits.way_en := s3_tag_match_way
  io.prefetch_flag_write.bits.source := L1_HW_PREFETCH_NULL

  io.counter_filter_query.req.valid := s3_clear_pf_flag_en
  io.counter_filter_query.req.bits.idx := get_idx(s3_vaddr)
  io.counter_filter_query.req.bits.way := OHToUInt(s3_tag_match_way)

  io.counter_filter_enq.valid := io.prefetch_flag_write.valid
  io.counter_filter_enq.bits.idx := get_idx(s3_vaddr)
  io.counter_filter_enq.bits.way := OHToUInt(s3_tag_match_way)

  io.prefetch_info.fdp.useful_prefetch := s3_clear_pf_flag_en && !io.counter_filter_query.resp

  XSPerfAccumulate("s3_pf_hit", s3_clear_pf_flag_en)
  XSPerfAccumulate("s3_pf_hit_filter", s3_clear_pf_flag_en && !io.counter_filter_query.resp)

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

  val load_trace = Wire(new LoadPfDbBundle)
  val pf_trace = Wire(new LoadPfDbBundle)
  val miss_trace = Wire(new LoadPfDbBundle)
  val mshr_trace = Wire(new LoadPfDbBundle)

  load_trace.paddr := get_block_addr(s2_paddr)
  pf_trace.paddr := get_block_addr(s2_paddr)
  miss_trace.paddr := get_block_addr(s2_paddr)
  mshr_trace.paddr := get_block_addr(s2_paddr)

  val table_load = ChiselDB.createTable("LoadTrace" + id.toString + "_hart"+ p(XSCoreParamsKey).HartId.toString, new LoadPfDbBundle, basicDB = false)
  val site_load = "LoadPipe_load" + id.toString
  table_load.log(load_trace, s2_valid && s2_req.isFirstIssue && (s2_req.instrtype =/= DCACHE_PREFETCH_SOURCE.U), site_load, clock, reset)

  val table_pf = ChiselDB.createTable("LoadPfTrace" + id.toString + "_hart"+ p(XSCoreParamsKey).HartId.toString, new LoadPfDbBundle, basicDB = false)
  val site_pf = "LoadPipe_pf" + id.toString
  table_pf.log(pf_trace, s2_valid && (s2_req.instrtype === DCACHE_PREFETCH_SOURCE.U), site_pf, clock, reset)

  val table_miss = ChiselDB.createTable("LoadTraceMiss" + id.toString + "_hart"+ p(XSCoreParamsKey).HartId.toString, new LoadPfDbBundle, basicDB = false)
  val site_load_miss = "LoadPipe_load_miss" + id.toString
  table_miss.log(miss_trace, s2_valid && s2_req.isFirstIssue && (s2_req.instrtype =/= DCACHE_PREFETCH_SOURCE.U) && real_miss, site_load_miss, clock, reset)

  val table_mshr = ChiselDB.createTable("LoadPfMshr" + id.toString + "_hart"+ p(XSCoreParamsKey).HartId.toString, new LoadPfDbBundle, basicDB = false)
  val site_mshr = "LoadPipe_mshr" + id.toString
  table_mshr.log(mshr_trace, s2_valid && (s2_req.instrtype === DCACHE_PREFETCH_SOURCE.U) && io.miss_req.fire, site_mshr, clock, reset)

  // performance counters
  XSPerfAccumulate("load_req", io.lsu.req.fire)
  XSPerfAccumulate("load_s1_kill", s1_fire && io.lsu.s1_kill)
  XSPerfAccumulate("load_hit_way", s1_fire && s1_tag_match_dup_dc)
  XSPerfAccumulate("load_replay", io.lsu.resp.fire && resp.bits.replay)
  XSPerfAccumulate("load_replay_for_dcache_data_nack", io.lsu.resp.fire && resp.bits.replay && s2_nack_data)
  XSPerfAccumulate("load_replay_for_dcache_no_mshr", io.lsu.resp.fire && resp.bits.replay && s2_nack_no_mshr)
  XSPerfAccumulate("load_replay_for_dcache_conflict", io.lsu.resp.fire && resp.bits.replay && io.bank_conflict_slow)
  XSPerfAccumulate("load_replay_for_dcache_wpu_pred_fail", io.lsu.resp.fire && resp.bits.replay && s2_wpu_pred_fail)
  XSPerfAccumulate("load_hit", io.lsu.resp.fire && !real_miss)
  XSPerfAccumulate("load_miss", io.lsu.resp.fire && real_miss)
  XSPerfAccumulate("load_succeed", io.lsu.resp.fire && !resp.bits.miss && !resp.bits.replay)
  XSPerfAccumulate("load_miss_or_conflict", io.lsu.resp.fire && resp.bits.miss)
  XSPerfAccumulate("actual_ld_fast_wakeup", s1_fire && s1_tag_match_dup_dc && !io.disable_ld_fast_wakeup)
  XSPerfAccumulate("ideal_ld_fast_wakeup", io.banked_data_read.fire && s1_tag_match_dup_dc)

  val perfEvents = Seq(
    ("load_req                 ", io.lsu.req.fire                                               ),
    ("load_replay              ", io.lsu.resp.fire && resp.bits.replay                          ),
    ("load_replay_for_data_nack", io.lsu.resp.fire && resp.bits.replay && s2_nack_data          ),
    ("load_replay_for_no_mshr  ", io.lsu.resp.fire && resp.bits.replay && s2_nack_no_mshr       ),
    ("load_replay_for_conflict ", io.lsu.resp.fire && resp.bits.replay && io.bank_conflict_slow ),
  )
  generatePerfEvent()
}
