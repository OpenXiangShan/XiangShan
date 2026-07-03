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

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink.TLPermissions
import org.chipsalliance.cde.config.Parameters
import utility._
import xiangshan.cache.wpu._
import xiangshan.mem.HasL1PrefetchSourceParameter
import xiangshan.mem.prefetch._
import xiangshan.{L1CacheErrorInfo, XSCoreParamsKey}

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
    val metaRead = DecoupledIO(new MetaReadReq)
    val metaResp = Input(Vec(nWays, new Meta))
    val extraMetaResp = Input(Vec(nWays, new DCacheExtraMeta))

    val tagRead = DecoupledIO(new TagReadReq)
    val tagResp = Input(Vec(nWays, UInt(encTagBits.W)))
    val vtagUpdate = Flipped(DecoupledIO(new TagWriteReq))

    val bankedDataRead = DecoupledIO(new L1BankedDataReadReqWithMask)
    val is128Req = Output(Bool())
    val bankedDataResp = Input(Vec(VLEN/DCacheSRAMRowBits, new L1BankedDataReadResult()))
    val readErrorDelayed = Input(Vec(VLEN/DCacheSRAMRowBits, Bool()))

    // access bit update
    val accessFlagWrite = DecoupledIO(new FlagMetaWriteReq)
    val prefetchFlagWrite = DecoupledIO(new SourceMetaWriteReq)
    val latencyFlagWrite = DecoupledIO(new LatencyMetaWriteReq)

    // banked data read conflict
    val bankConflictSlow = Input(Bool())

    // send miss request to miss queue
    val missReq    = DecoupledIO(new MissReq)
    val missResp   = Input(new MissResp)

    // send miss request to wbq
    val wbqConflictCheck = Valid(UInt())
    val wbqBlockMissReq = Input(Bool())

    // update state vec in replacement algo
    val replaceAccess = ValidIO(new ReplacementAccessBundle)
    // find the way to be replaced
    val replaceWay = new ReplacementWayReqIO

    // BtoT grow check
    val occupySet = Output(UInt())
    val occupyFail = Input(Bool())

    // load fast wakeup should be disabled when data read is not ready
    val disableLdFastWakeup = Input(Bool())

    // ecc error
    val error = Output(ValidIO(new L1CacheErrorInfo))
    val pseudoError = Flipped(DecoupledIO(Vec(DCacheBanks, new CtrlUnitSignalingBundle)))
    val pseudoTagErrorInjDone = Output(Bool())
    val pseudoDataErrorInjDone = Output(Bool())

    val prefetchStat = Output(new PipePrefetchStatBundle)

    val bloomFilterQuery = new Bundle {
      val query = ValidIO(new BloomQueryBundle(BLOOM_FILTER_ENTRY_NUM))
      val resp = Flipped(ValidIO(new BloomRespBundle()))
    }

    val counterFilterQuery = new CounterFilterQueryBundle
    val counterFilterEnq = new ValidIO(new CounterFilterDataBundle())
  })

  assert(RegNext(io.metaRead.ready))

  val s1_ready = Wire(Bool())
  val s2_ready = Wire(Bool())
  // LSU requests
  // it you got nacked, you can directly passdown
  val notNackedReady = io.metaRead.ready && io.tagRead.ready && s1_ready
  val nackedReady     = true.B

  // Pipeline
  // --------------------------------------------------------------------------------
  // stage 0
  // --------------------------------------------------------------------------------
  // read tag

  // ready can wait for valid
  io.lsu.req.ready := (!io.nack && notNackedReady) || (io.nack && nackedReady)
  io.metaRead.valid := io.lsu.req.fire && !io.nack
  io.tagRead.valid := io.lsu.req.fire && !io.nack

  val s0_valid = io.lsu.req.fire
  val s0_req = WireInit(io.lsu.req.bits)
  val s0_pf_source = io.lsu.pfSource
  s0_req.vaddr := Mux(io.load128Req, Cat(io.lsu.req.bits.vaddr(io.lsu.req.bits.vaddr.getWidth - 1, 4), 0.U(4.W)), io.lsu.req.bits.vaddr)
  val s0_fire = s0_valid && s1_ready
  val s0_vaddr = s0_req.vaddr
  val s0_replay_carry = s0_req.replayCarry
  val s0_load128_req = io.load128Req
  val s0_base_bank = addrToDcacheBank(s0_vaddr)
  val s0_bank_mask128b = bankMaskFromBase(s0_base_bank, DCacheVWordBankCount)
  val s0_bank_mask_normal = byteMaskToBankMask(s0_vaddr, s0_req.mask)
  val s0_bank_oh = Mux(s0_load128_req, s0_bank_mask128b, s0_bank_mask_normal)
  assert(RegNext(!(s0_valid && (s0_req.cmd =/= MemoryOpConstants.M_XRD && s0_req.cmd =/= MemoryOpConstants.M_PFR && s0_req.cmd =/= MemoryOpConstants.M_PFW))), "LoadPipe only accepts load req / softprefetch read or write!")
  dumpPipelineReqs("LoadPipe s0", s0_valid, s0_req)

  // wpu
  // val dwpu = Module(new DCacheWpuWrapper)
  // req in s0
  if(dwpuParam.enWPU){
    io.dwpu.req(0).bits.vaddr := s0_vaddr
    io.dwpu.req(0).bits.replayCarry := s0_replay_carry
    io.dwpu.req(0).valid := s0_valid
  }else{
    io.dwpu.req(0).valid := false.B
    io.dwpu.req(0).bits := DontCare
  }


  val metaRead = io.metaRead.bits
  val tagRead = io.tagRead.bits

  // Tag read for new requests
  metaRead.idx := get_dcache_idx(io.lsu.req.bits.vaddr)
  metaRead.wayEn := ~0.U(nWays.W)
  // meta_read.tag := DontCare

  tagRead.idx := get_dcache_idx(io.lsu.req.bits.vaddr)
  tagRead.wayEn := ~0.U(nWays.W)

  // --------------------------------------------------------------------------------
  // stage 1
  // --------------------------------------------------------------------------------
  // tag match, read data

  val s1_valid = RegInit(false.B)
  val s1_req = RegEnable(s0_req, s0_fire)
  val s1_pf_source = RegEnable(s0_pf_source, s0_fire)
  // in stage 1, load unit gets the physical address
  val s1_paddr_dup_lsu = io.lsu.s1_paddr_dup_lsu
  val s1_paddr_dup_dcache = io.lsu.s1_paddr_dup_dcache
  val s1_load128_req = RegEnable(s0_load128_req, s0_fire)
  val s1_is_prefetch = s1_req.instrtype === DCACHE_PREFETCH_SOURCE.U
  // LSU may update the address from io.lsu.s1_paddr, which affects the bank read enable only.
  val s1_vaddr_update = Cat(s1_req.vaddr(VAddrBits - 1, blockOffBits), io.lsu.s1_paddr_dup_lsu(blockOffBits - 1, 0))
  val s1_vaddr_update_dup = Cat(s1_req.vaddrDup(VAddrBits - 1, blockOffBits), io.lsu.s1_paddr_dup_dcache(blockOffBits - 1, 0))
  val s1_vaddr = Mux(s1_load128_req, Cat(s1_vaddr_update(VAddrBits - 1, 4), 0.U(4.W)), s1_vaddr_update)
  val s1_vaddr_dup = Mux(s1_load128_req, Cat(s1_vaddr_update_dup(VAddrBits - 1, 4), 0.U(4.W)), s1_vaddr_update_dup)
  val s1_bank_oh = RegEnable(s0_bank_oh, s0_fire)
  val s1_nack = RegNext(io.nack)
  val s1_fire = s1_valid && s2_ready
  s1_ready := !s1_valid || s1_fire

  when (s0_fire) { s1_valid := true.B }
  .elsewhen (s1_fire) { s1_valid := false.B }

  dumpPipelineReqs("LoadPipe s1", s1_valid, s1_req)

  // tag check
  def wayMap[T <: Data](f: Int => T) = VecInit((0 until nWays).map(f))
  val metaResp = io.metaResp
  // pseudo enc ecc tag
  val pseudoTagToggleMask = Mux(
                                io.pseudoError.valid && io.pseudoError.bits(0).valid,
                                io.pseudoError.bits(0).mask(tagBits - 1, 0),
                                0.U(tagBits.W)
                            )
  val s1_enc_tag_resp = Wire(io.tagResp.cloneType)
  s1_enc_tag_resp.zip(io.tagResp).map {
    case (pseudo_enc, real_enc) =>
    if (cacheCtrlParamsOpt.nonEmpty && EnableTagEcc) {
      val ecc = real_enc(encTagBits - 1, tagBits)
      val toggleTag = real_enc(tagBits - 1, 0) ^ pseudoTagToggleMask
      pseudo_enc := Cat(ecc, toggleTag)
    }  else {
      pseudo_enc := real_enc
    }
  }

  // resp in s1
  val s1_tag_resp = s1_enc_tag_resp.map(encTag => encTag(tagBits - 1, 0))
  val s1_tag_errors = wayMap((w: Int) => metaResp(w).coh.isValid() && dcacheParameters.tagCode.decode(s1_enc_tag_resp(w)).error).asUInt
  val s1_tag_match_way_dup_dc = wayMap((w: Int) => s1_tag_resp(w) === get_tag(s1_paddr_dup_dcache) && metaResp(w).coh.isValid()).asUInt
  val s1_tag_match_way_dup_lsu = wayMap((w: Int) => s1_tag_resp(w) === get_tag(s1_paddr_dup_lsu) && metaResp(w).coh.isValid()).asUInt
  val s1_wpu_pred_valid = RegEnable(io.dwpu.resp(0).valid, s0_fire)
  val s1_wpu_pred_way_en = RegEnable(io.dwpu.resp(0).bits.s0_pred_way_en, s0_fire)

  // lookup update
  io.dwpu.lookup_upd(0).valid := s1_valid
  io.dwpu.lookup_upd(0).bits.vaddr := s1_vaddr
  io.dwpu.lookup_upd(0).bits.s1_real_way_en := s1_tag_match_way_dup_dc
  io.dwpu.lookup_upd(0).bits.s1_pred_way_en := s1_wpu_pred_way_en
  // replace / tag write
  io.vtagUpdate.ready := true.B
  // dwpu.io.tagwrite_upd.valid := io.vtag_update.valid
  // dwpu.io.tagwrite_upd.bits.vaddr := io.vtag_update.bits.vaddr
  // dwpu.io.tagwrite_upd.bits.s1_real_way_en := io.vtag_update.bits.way_en

  val s1_direct_map_way_num = getDirectMapWay(s1_req.vaddr)
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
    io.dwpu.cfpred(0).s1_dm_hit := wayMap((w: Int) => w.U === s1_direct_map_way_num && s1_tag_resp(w) === get_tag(s1_paddr_dup_lsu) && metaResp(w).coh.isValid()).asUInt.orR
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
  io.pseudoTagErrorInjDone := s1_fire && wayMap((w: Int) => metaResp(w).coh.isValid()).asUInt.orR

  // when there are no tag match, we give it a Fake Meta
  // this simplifies our logic in s2 stage
  val s1_hit_meta = ParallelMux(s1_tag_match_way_dup_dc.asBools, (0 until nWays).map(w => metaResp(w)))
  val s1_hit_coh = s1_hit_meta.coh
  val s1_hit_error = ParallelMux(s1_tag_match_way_dup_dc.asBools, (0 until nWays).map(w => io.extraMetaResp(w).error))
  val s1_hit_prefetch = ParallelMux(s1_tag_match_way_dup_dc.asBools, (0 until nWays).map(w => io.extraMetaResp(w).prefetch))
  val s1_hit_access = ParallelMux(s1_tag_match_way_dup_dc.asBools, (0 until nWays).map(w => io.extraMetaResp(w).access))
  val s1_hit_refill_latency = ParallelMux(s1_tag_match_way_dup_dc.asBools, (0 until nWays).map(w => io.extraMetaResp(w).latency))

  // io.replace_way.set.valid := RegNext(s0_fire)
  io.replaceWay.set.valid := false.B
  io.replaceWay.set.bits := get_dcache_idx(s1_vaddr)
  io.replaceWay.dmWay := getDirectMapWay(s1_vaddr)
  val s1_invalid_vec = wayMap(w => !metaResp(w).coh.isValid())
  val s1_have_invalid_way = s1_invalid_vec.asUInt.orR
  val s1_invalid_way_en = ParallelPriorityMux(s1_invalid_vec.zipWithIndex.map(x => x._1 -> UIntToOH(x._2.U(nWays.W))))

  val s1_need_replacement = !s1_tag_match_dup_dc

  XSPerfAccumulate("load_using_replacement", io.replaceWay.set.valid && s1_need_replacement)

  // query bloom filter
  io.bloomFilterQuery.query.valid := s1_valid
  io.bloomFilterQuery.query.bits.addr := io.bloomFilterQuery.query.bits.get_addr(s1_paddr_dup_dcache)

  // get s1_will_send_miss_req in lpad_s1
  val (s1_has_permission, s1_shrink_perm, s1_new_hit_coh) = s1_hit_coh.onAccess(s1_req.cmd)
  val s1_hit = s1_tag_match_dup_dc && s1_has_permission && s1_hit_coh === s1_new_hit_coh
  val s1_will_send_miss_req = s1_valid && !s1_nack && !s1_hit

  // data read
  io.bankedDataRead.valid := s1_fire && !s1_nack && !s1_is_prefetch
  io.bankedDataRead.bits.addr := s1_vaddr
  io.bankedDataRead.bits.addrDup := s1_vaddr_dup
  io.bankedDataRead.bits.wayEn := s1_pred_tag_match_way_dup_dc
  io.bankedDataRead.bits.bankMask := s1_bank_oh
  io.bankedDataRead.bits.lqIdx := s1_req.lqIdx
  io.is128Req := s1_load128_req

  // check tl error
  val s1_tl_error = Mux(s1_need_replacement, 0.U.asTypeOf(new TLError()), s1_hit_error) // error reported by exist dcache error bit

  // --------------------------------------------------------------------------------
  // stage 2
  // --------------------------------------------------------------------------------
  // return data

  // val s2_valid = RegEnable(next = s1_valid && !io.lsu.s1_kill, init = false.B, enable = s1_fire)
  val s2_valid = RegInit(false.B)
  val s2_valid_dup = RegInit(false.B)
  val s2_req = RegEnable(s1_req, s1_fire)
  val s2_pf_source = RegEnable(s1_pf_source, s1_fire)
  val s2_load128_req = RegEnable(s1_load128_req, s1_fire)
  val s2_paddr = RegEnable(s1_paddr_dup_dcache, s1_fire)
  val s2_vaddr = RegEnable(s1_vaddr, s1_fire)
  val s2_bank_oh = RegEnable(s1_bank_oh, s1_fire)
  val s2_bank_oh_dup0 = RegEnable(s1_bank_oh, s1_fire)
  val s2_wpu_pred_fail = RegEnable(s1_wpu_pred_fail, s1_fire)
  val s2_real_way_en = RegEnable(s1_tag_match_way_dup_dc, s1_fire)
  val s2_pred_way_en = RegEnable(s1_pred_tag_match_way_dup_dc, s1_fire)
  val s2_dm_way_num = RegEnable(s1_direct_map_way_num, s1_fire)
  val s2_wpu_pred_fail_and_real_hit = RegEnable(s1_wpu_pred_fail_and_real_hit, s1_fire)

  // occupy set check, it will fail if the number of BtoT at same set great equal nWays - 1
  io.occupySet := addrToDcacheSet(s2_vaddr)

  s2_ready := true.B

  val s2_fire = s2_valid

  when (s1_fire) {
    s2_valid := !io.lsu.s1_kill
    s2_valid_dup := !io.lsu.s1_kill
  }
  .elsewhen(io.lsu.resp.fire) {
    s2_valid := false.B
    s2_valid_dup := false.B
  }

  dumpPipelineReqs("LoadPipe s2", s2_valid, s2_req)


  // hit, miss, nack, permission checking
  // dcache side tag match
  val s2_tag_errors = RegEnable(s1_tag_errors, s1_fire)
  val s2_tag_match_way = RegEnable(s1_tag_match_way_dup_dc, s1_fire)
  val s2_tag_match = RegEnable(s1_tag_match_dup_dc, s1_fire)

  // lsu side tag match
  val s2_hit_dup_lsu = RegNext(s1_tag_match_dup_lsu)

  io.lsu.s2_hit := s2_hit_dup_lsu && !s2_wpu_pred_fail

  val s2_hit_meta = RegEnable(s1_hit_meta, s1_fire)
  val s2_hit_coh = RegEnable(s1_hit_coh, s1_fire)
  val s2_has_permission = RegEnable(s1_has_permission, s1_fire)
  val s2_new_hit_coh = RegEnable(s1_new_hit_coh, s1_fire)
  val s2_grow_perm_btot = RegEnable(
    s1_shrink_perm === TLPermissions.BtoT && !s1_has_permission && s1_tag_match_dup_lsu && !s2_wpu_pred_fail,
    s1_fire
  )
  // BtoT occupy fail
  val s2_btot_occupy_fail = io.occupyFail && s2_grow_perm_btot

  //
  val s2_can_send_miss_req = RegEnable(s1_will_send_miss_req, s1_fire)
  val s2_can_send_miss_req_dup = RegEnable(s1_will_send_miss_req, s1_fire)

  val s2_miss_req_valid     = s2_valid && s2_can_send_miss_req
  val s2_miss_req_valid_dup = s2_valid_dup && s2_can_send_miss_req_dup
  val s2_miss_req_fire      = s2_miss_req_valid_dup && io.missReq.ready

  // when req got nacked, upper levels should replay this request
  // nacked or not
  val s2_nack_hit = RegEnable(s1_nack, s1_fire)
  // can no allocate mshr for load miss
  val s2_nack_no_mshr = s2_miss_req_valid_dup && !io.missReq.ready
  // block with a wbq valid req
  val s2_nack_wbq_conflict = s2_miss_req_valid_dup && io.wbqBlockMissReq
  // Bank conflict on data arrays
  val s2_nack_data = RegEnable(!io.bankedDataRead.ready, s1_fire)
  val s2_nack = s2_nack_hit || s2_nack_no_mshr || s2_nack_data || s2_nack_wbq_conflict
  // s2 miss merged
  val s2_miss_merged = s2_miss_req_fire && !io.missReq.bits.cancel && !io.wbqBlockMissReq && io.missResp.merged

  val s2_bank_addr = addrToDcacheBank(s2_paddr)
  dontTouch(s2_bank_addr)

  val s2_instrtype = s2_req.instrtype

  val s2_tag_error = WireInit(false.B)
  val s2_tl_error = RegEnable(s1_tl_error, s1_fire)

  val s2_hit_prefetch = RegEnable(s1_hit_prefetch, s1_fire)
  val s2_hit_access = RegEnable(s1_hit_access, s1_fire)
  val s2_hit_refill_latency = RegEnable(s1_hit_refill_latency, s1_fire)

  val s2_hit = s2_tag_match && s2_has_permission && s2_hit_coh === s2_new_hit_coh && !s2_wpu_pred_fail

  val s2_data128bit = Cat((0 until DCacheVWordBankCount).reverse.map(i => io.bankedDataResp(i).rawData))
  val s2_resp_data  = s2_data128bit

  val s2_is_prefetch = s2_req.instrtype === DCACHE_PREFETCH_SOURCE.U
  // only dump these signals when they are actually valid
  dumpPipelineValids("LoadPipe s2", "s2_hit", s2_valid && s2_hit)
  dumpPipelineValids("LoadPipe s2", "s2_nack", s2_valid && s2_nack)
  dumpPipelineValids("LoadPipe s2", "s2_nack_hit", s2_valid && s2_nack_hit)
  dumpPipelineValids("LoadPipe s2", "s2_nack_no_mshr", s2_valid && s2_nack_no_mshr)

  if(EnableTagEcc) {
    s2_tag_error := s2_tag_errors.orR // error reported by tag ecc check
  }
  io.pseudoDataErrorInjDone := s2_fire && s2_hit && !io.bankConflictSlow
  io.pseudoError.ready := false.B

  // send load miss to miss queue
  io.missReq.valid := s2_miss_req_valid
  io.missReq.bits := DontCare
  io.missReq.bits.source := s2_instrtype
  io.missReq.bits.pfSource := RegNext(RegNext(io.lsu.pfSource))  // TODO: clock gate
  io.missReq.bits.cmd := s2_req.cmd
  io.missReq.bits.addr := get_block_addr(s2_paddr)
  io.missReq.bits.vaddr := s2_vaddr
  io.missReq.bits.reqCoh := s2_hit_coh
  io.missReq.bits.cancel := io.lsu.s2_kill || s2_tag_error || s2_btot_occupy_fail
  io.missReq.bits.pc := io.lsu.s2_pc
  io.missReq.bits.lqIdx := io.lsu.req.bits.lqIdx
  io.missReq.bits.isBtoT := s2_grow_perm_btot
  io.missReq.bits.occupyWay := s2_tag_match_way

  //send load miss to wbq
  io.wbqConflictCheck.valid := s2_miss_req_valid_dup
  io.wbqConflictCheck.bits := get_block_addr(s2_paddr)

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
  val realMiss = !s2_real_way_en.orR

  resp.bits.realMiss := realMiss
  resp.bits.miss := realMiss
  resp.bits.data := s2_resp_data
  io.lsu.s2_first_hit := s2_req.isFirstIssue && s2_hit
  // load pipe need replay when there is a bank conflict or wpu predict fail
  resp.bits.replay := (resp.bits.miss && (s2_nack || io.missReq.bits.cancel)) || io.bankConflictSlow || s2_wpu_pred_fail || s2_btot_occupy_fail
  resp.bits.replayCarry.valid := (resp.bits.miss && (s2_nack || io.missReq.bits.cancel)) || io.bankConflictSlow || s2_wpu_pred_fail || s2_btot_occupy_fail
  resp.bits.replayCarry.real_way_en := s2_real_way_en
  resp.bits.metaPrefetch := s2_hit_prefetch
  resp.bits.metaAccess := s2_hit_access
  resp.bits.refillLatency := s2_hit_refill_latency
  resp.bits.tagError := false.B
  resp.bits.mshrId := io.missResp.id
  resp.bits.handled := s2_miss_req_fire && !io.missReq.bits.cancel && !io.wbqBlockMissReq && io.missResp.handled
  resp.bits.debugRobIdx := s2_req.debugRobIdx
  // debug info
  io.lsu.s2_first_hit := s2_req.isFirstIssue && s2_hit
  io.lsu.debugS2RealWayNum := OneHot.OHToUIntStartOne(s2_real_way_en)
  if(dwpuParam.enWPU) {
    io.lsu.debugS2PredWayNum := OneHot.OHToUIntStartOne(s2_pred_way_en)
  }else{
    io.lsu.debugS2PredWayNum := 0.U
  }
  if(dwpuParam.enWPU && dwpuParam.enCfPred || !env.FPGAPlatform){
    io.lsu.debugS2DmWayNum :=  s2_dm_way_num + 1.U
  }else{
    io.lsu.debugS2DmWayNum := 0.U
  }


  XSPerfAccumulate("dcache_read_bank_conflict", io.bankConflictSlow && s2_valid)
  XSPerfAccumulate("dcache_read_from_prefetched_line", s2_valid && isPrefetchRelated(s2_hit_prefetch) && !resp.bits.miss)
  XSPerfAccumulate("dcache_first_read_from_prefetched_line", s2_valid && isPrefetchRelated(s2_hit_prefetch) && !resp.bits.miss && !s2_hit_access)

  // if ldu0 and ldu1 hit the same, count for 1
  val totalPrefetch = s2_valid && (s2_req.instrtype === DCACHE_PREFETCH_SOURCE.U)
  val pfLateInCache = s2_valid && s2_hit && (s2_req.instrtype === DCACHE_PREFETCH_SOURCE.U)
  val hitPfInCache = Wire(Bool()) // assigned in s3 for filtering
  val hitSource = Wire(UInt(L1PfSourceBits.W))
  
  io.prefetchStat.total_prefetch := totalPrefetch
  io.prefetchStat.pf_late_in_cache := pfLateInCache
  io.prefetchStat.pf_late_in_cache_source := s2_hit_prefetch
  io.prefetchStat.nack_prefetch := s2_valid && s2_nack && (s2_req.instrtype === DCACHE_PREFETCH_SOURCE.U)
  io.prefetchStat.pf_source := s2_pf_source

  io.prefetchStat.hit_pf_in_cache := hitPfInCache
  io.prefetchStat.hit_source := hitSource

  io.prefetchStat.demand_miss := s2_valid && (s2_req.instrtype =/= DCACHE_PREFETCH_SOURCE.U) && !s2_hit && s2_req.isFirstIssue
  io.prefetchStat.pollution := io.prefetchStat.demand_miss && io.bloomFilterQuery.resp.valid && io.bloomFilterQuery.resp.bits.res

  io.lsu.resp.valid := resp.valid
  io.lsu.resp.bits := resp.bits
  assert(RegNext(!(resp.valid && !io.lsu.resp.ready)), "lsu should be ready in s2")

  resp.bits.dump(resp.valid)

  io.lsu.debugS1HitWay := s1_tag_match_way_dup_dc
  io.lsu.s1_disable_fast_wakeup := io.disableLdFastWakeup
  io.lsu.s2_bank_conflict := io.bankConflictSlow
  io.lsu.s2_wpu_pred_fail := s2_wpu_pred_fail_and_real_hit
  io.lsu.s2_mq_nack       := (resp.bits.miss && (s2_nack_no_mshr || io.missReq.bits.cancel || io.wbqBlockMissReq ) || s2_btot_occupy_fail)
  assert(RegNext(s1_ready && s2_ready), "load pipeline should never be blocked")

  // --------------------------------------------------------------------------------
  // stage 3
  // --------------------------------------------------------------------------------
  // report ecc error and get selected dcache data

  val s3_valid = RegNext(s2_valid)
  val s3_load128_req = RegEnable(s2_load128_req, s2_fire)
  val s3_read_error_lane_mask = RegEnable(bankMaskToReadErrorLaneMask(s2_bank_oh, addrToVWordBankBase(s2_vaddr)), s2_fire)
  val s3_vaddr = RegEnable(s2_vaddr, s2_fire)
  val s3_paddr = RegEnable(s2_paddr, s2_fire)
  val s3_hit = RegEnable(s2_hit, s2_fire)
  val s3_tag_match_way = RegEnable(s2_tag_match_way, s2_fire)
  val s3_req_instrtype = RegEnable(s2_req.instrtype, s2_fire)
  val s3_is_prefetch = s3_req_instrtype === DCACHE_PREFETCH_SOURCE.U

  val s3_banked_data_resp_word = RegEnable(s2_resp_data, s2_fire)
  val s3_data_error = Mux(
    s3_load128_req,
    io.readErrorDelayed.asUInt.orR,
    (io.readErrorDelayed.asUInt & s3_read_error_lane_mask).orR
  ) && s3_hit
  val s3_tag_error = RegEnable(s2_tag_error, s2_fire)
  val s3_tl_error = RegEnable(s2_tl_error, s2_fire)
  val s3_flag_error = s3_tl_error.asUInt.orR
  val s3_hit_prefetch = RegEnable(s2_hit_prefetch, s2_fire)
  val s3_error = s3_tag_error || s3_flag_error || s3_data_error

  // Register the S2 kill (includes load breakpoint trigger exception) so that
  // hit-side replacement/access-flag metadata updates in S3 can be suppressed
  // for loads that are architecturally killed by an exception.
  val s3_kill = RegEnable(io.lsu.s2_kill, s2_fire)

  // error_delayed signal will be used to update uop.exception 1 cycle after load writeback
  resp.bits.errorDelayed := s3_error && (s3_hit || s3_tag_error) && s3_valid
  resp.bits.tlErrorDelayed.tlDenied := s3_tl_error.tlDenied & s3_valid
  resp.bits.tlErrorDelayed.tlCorrupt := s3_tl_error.tlCorrupt & s3_valid
  resp.bits.dataDelayed := s3_banked_data_resp_word
  resp.bits.replacementUpdated := io.replaceAccess.valid

  // report tag / data / l2 error (with paddr) to bus error unit
  io.error := 0.U.asTypeOf(ValidIO(new L1CacheErrorInfo))
  io.error.bits.report_to_beu := (s3_tag_error || s3_data_error) && s3_valid
  io.error.bits.paddr := s3_paddr
  io.error.bits.source.tag := s3_tag_error
  io.error.bits.source.data := s3_data_error
  io.error.bits.source.l2 := s3_flag_error
  io.error.bits.opType.load := true.B
  // report tag error / l2 corrupted to CACHE_ERROR csr
  io.error.valid := s3_error && s3_valid

  io.replaceAccess.valid := s3_valid && s3_hit && !s3_kill
  io.replaceAccess.bits.set := RegNext(RegNext(get_dcache_idx(s1_req.vaddr)))
  io.replaceAccess.bits.way := RegNext(RegNext(OHToUInt(s1_tag_match_way_dup_dc)))

  // update access bit
  io.accessFlagWrite.valid := s3_valid && s3_hit && !s3_is_prefetch && !s3_kill
  io.accessFlagWrite.bits.idx := get_dcache_idx(s3_vaddr)
  io.accessFlagWrite.bits.wayEn := s3_tag_match_way
  io.accessFlagWrite.bits.flag := true.B

  // clear prefetch source when prefetch hit
  // so that next load to the same line won't be considered as prefetch hit
  // A prefetch block will only be counted once
  val s3_clear_pf_flag_en = s3_valid && s3_hit && !s3_is_prefetch && !s3_kill && isFromL1Prefetch(s3_hit_prefetch)
  io.prefetchFlagWrite.valid := s3_clear_pf_flag_en && !io.counterFilterQuery.resp
  io.prefetchFlagWrite.bits.idx := get_dcache_idx(s3_vaddr)
  io.prefetchFlagWrite.bits.wayEn := s3_tag_match_way
  io.prefetchFlagWrite.bits.source := L1_HW_PREFETCH_CLEAR

  // when demand request hit prefetch data, the latency is reset to 0.
  io.latencyFlagWrite.valid := s3_clear_pf_flag_en && !io.counterFilterQuery.resp
  io.latencyFlagWrite.bits.idx := get_idx(s3_vaddr)
  io.latencyFlagWrite.bits.wayEn := s3_tag_match_way
  io.latencyFlagWrite.bits.latency := 0.U

  io.counterFilterQuery.req.valid := s3_clear_pf_flag_en
  io.counterFilterQuery.req.bits.idx := get_dcache_idx(s3_vaddr)
  io.counterFilterQuery.req.bits.way := OHToUInt(s3_tag_match_way)

  io.counterFilterEnq.valid := io.prefetchFlagWrite.valid
  io.counterFilterEnq.bits.idx := get_dcache_idx(s3_vaddr)
  io.counterFilterEnq.bits.way := OHToUInt(s3_tag_match_way)

  hitPfInCache := s3_clear_pf_flag_en && !io.counterFilterQuery.resp
  hitSource := s3_hit_prefetch

  XSPerfAccumulate("s3_pf_hit", s3_clear_pf_flag_en)
  XSPerfAccumulate("s3_pf_hit_filter", s3_clear_pf_flag_en && !io.counterFilterQuery.resp)

  // --------------------------------------------------------------------------------
  // Debug logging functions
  def dumpPipelineReqs(pipeline_stage_name: String, valid: Bool,
    req: DCacheWordReq ) = {
      XSDebug(valid, s"$pipeline_stage_name: ")
      req.dump(valid)
  }

  def dumpPipelineValids(pipeline_stage_name: String, signal_name: String, valid: Bool) = {
    XSDebug(valid, s"$pipeline_stage_name $signal_name\n")
  }

  val loadTrace = Wire(new LoadPfDbBundle)
  val pfTrace = Wire(new LoadPfDbBundle)
  val missTrace = Wire(new LoadPfDbBundle)
  val mshrTrace = Wire(new LoadPfDbBundle)

  loadTrace.paddr := get_block_addr(s2_paddr)
  pfTrace.paddr := get_block_addr(s2_paddr)
  missTrace.paddr := get_block_addr(s2_paddr)
  mshrTrace.paddr := get_block_addr(s2_paddr)

  val tableLoad = ChiselDB.createTable("LoadTrace" + id.toString + "_hart"+ p(XSCoreParamsKey).HartId.toString, new LoadPfDbBundle, basicDB = false)
  val siteLoad = "LoadPipe_load" + id.toString
  tableLoad.log(loadTrace, s2_valid && s2_req.isFirstIssue && (s2_req.instrtype =/= DCACHE_PREFETCH_SOURCE.U), siteLoad, clock, reset)

  val tablePf = ChiselDB.createTable("LoadPfTrace" + id.toString + "_hart"+ p(XSCoreParamsKey).HartId.toString, new LoadPfDbBundle, basicDB = false)
  val sitePf = "LoadPipe_pf" + id.toString
  tablePf.log(pfTrace, s2_valid && (s2_req.instrtype === DCACHE_PREFETCH_SOURCE.U), sitePf, clock, reset)

  val tableMiss = ChiselDB.createTable("LoadTraceMiss" + id.toString + "_hart"+ p(XSCoreParamsKey).HartId.toString, new LoadPfDbBundle, basicDB = false)
  val siteLoadMiss = "LoadPipe_load_miss" + id.toString
  tableMiss.log(missTrace, s2_valid && s2_req.isFirstIssue && (s2_req.instrtype =/= DCACHE_PREFETCH_SOURCE.U) && realMiss, siteLoadMiss, clock, reset)

  val tableMshr = ChiselDB.createTable("LoadPfMshr" + id.toString + "_hart"+ p(XSCoreParamsKey).HartId.toString, new LoadPfDbBundle, basicDB = false)
  val siteMshr = "LoadPipe_mshr" + id.toString
  tableMshr.log(mshrTrace, s2_valid && (s2_req.instrtype === DCACHE_PREFETCH_SOURCE.U) && io.missReq.fire, siteMshr, clock, reset)

  // performance counters
  XSPerfAccumulate("load_req", io.lsu.req.fire)
  XSPerfAccumulate("load_s1_kill", s1_fire && io.lsu.s1_kill)
  XSPerfAccumulate("load_hit_way", s1_fire && s1_tag_match_dup_dc)
  XSPerfAccumulate("load_replay", io.lsu.resp.fire && resp.bits.replay)
  XSPerfAccumulate("load_replay_for_dcache_data_nack", io.lsu.resp.fire && resp.bits.replay && s2_nack_data)
  XSPerfAccumulate("load_replay_for_dcache_no_mshr", io.lsu.resp.fire && resp.bits.replay && s2_nack_no_mshr)
  XSPerfAccumulate("load_replay_for_dcache_conflict", io.lsu.resp.fire && resp.bits.replay && io.bankConflictSlow)
  XSPerfAccumulate("load_replay_for_dcache_wpu_pred_fail", io.lsu.resp.fire && resp.bits.replay && s2_wpu_pred_fail)
  XSPerfAccumulate("load_hit", io.lsu.resp.fire && !realMiss)
  XSPerfAccumulate("load_miss", io.lsu.resp.fire && realMiss)
  XSPerfAccumulate("load_succeed", io.lsu.resp.fire && !resp.bits.miss && !resp.bits.replay)
  XSPerfAccumulate("load_miss_or_conflict", io.lsu.resp.fire && resp.bits.miss)
  XSPerfAccumulate("actual_ld_fast_wakeup", s1_fire && s1_tag_match_dup_dc && !io.disableLdFastWakeup)
  XSPerfAccumulate("ideal_ld_fast_wakeup", io.bankedDataRead.fire && s1_tag_match_dup_dc)

  val perfEvents = Seq(
    ("load_req                 ", io.lsu.req.fire                                               ),
    ("load_replay              ", io.lsu.resp.fire && resp.bits.replay                          ),
    ("load_replay_for_data_nack", io.lsu.resp.fire && resp.bits.replay && s2_nack_data          ),
    ("load_replay_for_no_mshr  ", io.lsu.resp.fire && resp.bits.replay && s2_nack_no_mshr       ),
    ("load_replay_for_conflict ", io.lsu.resp.fire && resp.bits.replay && io.bankConflictSlow ),
    ("l1D_read_dcache_access   ", io.lsu.resp.fire && !s2_is_prefetch                           ),
    ("l1D_read_dcache_miss     ", io.lsu.resp.fire && !s2_is_prefetch && realMiss              )
  )
  generatePerfEvent()
}
