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
import freechips.rocketchip.tilelink.ClientStates._
import freechips.rocketchip.tilelink.MemoryOpCategories._
import freechips.rocketchip.tilelink.TLPermissions._
import freechips.rocketchip.tilelink.{ClientMetadata, ClientStates, TLPermissions}
import utils._
import utility._
import xiangshan.{L1CacheErrorInfo, XSCoreParamsKey}
import xiangshan.mem.prefetch._
import xiangshan.mem.HasL1PrefetchSourceParameter

class MainPipeReq(implicit p: Parameters) extends DCacheBundle {
  val miss = Bool() // only amo miss will refill in main pipe
  val miss_id = UInt(log2Up(cfg.nMissEntries).W)
  val miss_param = UInt(TLPermissions.bdWidth.W)
  val miss_dirty = Bool()

  val probe = Bool()
  val probe_param = UInt(TLPermissions.bdWidth.W)
  val probe_need_data = Bool()

  // request info
  // reqs from Store, AMO use this
  // probe does not use this
  val source = UInt(sourceTypeWidth.W)
  val cmd = UInt(M_SZ.W)
  // if dcache size > 32KB, vaddr is also needed for store
  // vaddr is used to get extra index bits
  val vaddr  = UInt(VAddrBits.W)
  // must be aligned to block
  val addr   = UInt(PAddrBits.W)

  // store
  val store_data = UInt((cfg.blockBytes * 8).W)
  val store_mask = UInt(cfg.blockBytes.W)

  // which word does amo work on?
  val word_idx = UInt(log2Up(cfg.blockBytes * 8 / DataBits).W)
  val amo_data   = UInt(QuadWordBits.W)
  val amo_mask   = UInt(QuadWordBytes.W)
  val amo_cmp    = UInt(QuadWordBits.W) // data to be compared in AMOCAS

  // error
  val error = Bool()

  // replace
  val replace = Bool()
  val replace_way_en = UInt(DCacheWays.W)

  // prefetch
  val pf_source = UInt(L1PfSourceBits.W)
  val access = Bool()

  val id = UInt(reqIdWidth.W)

  def isLoad: Bool = source === LOAD_SOURCE.U
  def isStore: Bool = source === STORE_SOURCE.U
  def isAMO: Bool = source === AMO_SOURCE.U

  def quad_word_idx = word_idx >> 1

  def convertStoreReq(store: DCacheLineReq): MainPipeReq = {
    val req = Wire(new MainPipeReq)
    req := DontCare
    req.miss := false.B
    req.miss_dirty := false.B
    req.probe := false.B
    req.probe_need_data := false.B
    req.source := STORE_SOURCE.U
    req.cmd := store.cmd
    req.addr := store.addr
    req.vaddr := store.vaddr
    req.store_data := store.data
    req.store_mask := store.mask
    req.replace := false.B
    req.error := false.B
    req.id := store.id
    req
  }
}

class MainPipeStatus(implicit p: Parameters) extends DCacheBundle {
  val set = UInt(idxBits.W)
  val way_en = UInt(nWays.W)
}

class MainPipeInfoToMQ(implicit p:Parameters) extends DCacheBundle {
  val s2_valid = Bool()
  val s2_miss_id = UInt(log2Up(cfg.nMissEntries).W) // For refill data selection
  val s2_replay_to_mq = Bool()
  val s3_valid = Bool()
  val s3_miss_id = UInt(log2Up(cfg.nMissEntries).W) // For mshr release
  val s3_refill_resp = Bool()
}

class MainPipe(implicit p: Parameters) extends DCacheModule with HasPerfEvents with HasL1PrefetchSourceParameter {
  val io = IO(new Bundle() {
    // probe queue
    val probe_req = Flipped(DecoupledIO(new MainPipeReq))
    // store miss go to miss queue
    val miss_req = DecoupledIO(new MissReq)
    val miss_resp = Input(new MissResp) // miss resp is used to support plru update
    val refill_req = Flipped(DecoupledIO(new MainPipeReq))
    // send miss request to wbq
    val wbq_conflict_check = Valid(UInt())
    val wbq_block_miss_req = Input(Bool())
    // store buffer
    val store_req = Flipped(DecoupledIO(new DCacheLineReq))
    val store_replay_resp = ValidIO(new DCacheLineResp)
    val store_hit_resp = ValidIO(new DCacheLineResp)
    // atmoics
    val atomic_req = Flipped(DecoupledIO(new MainPipeReq))
    val atomic_resp = ValidIO(new MainPipeResp)
    // find matched refill data in missentry
    val mainpipe_info = Output(new MainPipeInfoToMQ)
    // missqueue refill data
    val refill_info = Flipped(ValidIO(new MissQueueRefillInfo))
    // write-back queue
    val wb = DecoupledIO(new WritebackReq)
    val wb_ready_dup = Vec(nDupWbReady, Input(Bool()))

    // data sram
    val data_read = Vec(LoadPipelineWidth, Input(Bool()))
    val data_read_intend = Output(Bool())
    val data_readline = DecoupledIO(new L1BankedDataReadLineReq)
    val data_resp = Input(Vec(DCacheBanks, new L1BankedDataReadResult()))
    val readline_error_delayed = Input(Bool())
    val data_write = DecoupledIO(new L1BankedDataWriteReq)
    val data_write_dup = Vec(DCacheBanks, Valid(new L1BankedDataWriteReqCtrl))
    val data_write_ready_dup = Vec(nDupDataWriteReady, Input(Bool()))

    // meta array
    val meta_read = DecoupledIO(new MetaReadReq)
    val meta_resp = Input(Vec(nWays, new Meta))
    val meta_write = DecoupledIO(new CohMetaWriteReq)
    val extra_meta_resp = Input(Vec(nWays, new DCacheExtraMeta))
    val error_flag_write = DecoupledIO(new FlagMetaWriteReq)
    val prefetch_flag_write = DecoupledIO(new SourceMetaWriteReq)
    val access_flag_write = DecoupledIO(new FlagMetaWriteReq)

    // tag sram
    val tag_read = DecoupledIO(new TagReadReq)
    val tag_resp = Input(Vec(nWays, UInt(encTagBits.W)))
    val tag_write = DecoupledIO(new TagWriteReq)
    val tag_write_ready_dup = Vec(nDupTagWriteReady, Input(Bool()))
    val tag_write_intend = Output(new Bool())

    // update state vec in replacement algo
    val replace_access = ValidIO(new ReplacementAccessBundle)
    // find the way to be replaced
    val replace_way = new ReplacementWayReqIO

    // writeback addr to be replaced
    val replace_addr = ValidIO(UInt(PAddrBits.W))
    val replace_block = Input(Bool())

    // sms prefetch
    val sms_agt_evict_req = DecoupledIO(new AGTEvictReq)

    val status = new Bundle() {
      val s0_set = ValidIO(UInt(idxBits.W))
      val s1, s2, s3 = ValidIO(new MainPipeStatus)
    }
    val status_dup = Vec(nDupStatus, new Bundle() {
      val s1, s2, s3 = ValidIO(new MainPipeStatus)
    })

    // lrsc locked block should block probe
    val lrsc_locked_block = Output(Valid(UInt(PAddrBits.W)))
    val invalid_resv_set = Input(Bool())
    val update_resv_set = Output(Bool())
    val block_lr = Output(Bool())

    // ecc error
    val error = Output(ValidIO(new L1CacheErrorInfo))
    val pseudo_error = Flipped(DecoupledIO(Vec(DCacheBanks, new CtrlUnitSignalingBundle)))
    val pseudo_tag_error_inj_done = Output(Bool())
    val pseudo_data_error_inj_done = Output(Bool())
    // force write
    val force_write = Input(Bool())

    val bloom_filter_query = new Bundle {
      val set = ValidIO(new BloomQueryBundle(BLOOM_FILTER_ENTRY_NUM))
      val clr = ValidIO(new BloomQueryBundle(BLOOM_FILTER_ENTRY_NUM))
    }
  })

  // meta array is made of regs, so meta write or read should always be ready
  assert(RegNext(io.meta_read.ready))
  assert(RegNext(io.meta_write.ready))

  val s1_s0_set_conflict, s2_s0_set_conlict, s3_s0_set_conflict = Wire(Bool())
  val set_conflict = s1_s0_set_conflict || s2_s0_set_conlict || s3_s0_set_conflict
  // check sbuffer store req set_conflict in parallel with req arbiter
  // it will speed up the generation of store_req.ready, which is in crit. path
  val s1_s0_set_conflict_store, s2_s0_set_conlict_store, s3_s0_set_conflict_store = Wire(Bool())
  val store_set_conflict = s1_s0_set_conflict_store || s2_s0_set_conlict_store || s3_s0_set_conflict_store
  val s1_ready, s2_ready, s3_ready = Wire(Bool())

  // convert store req to main pipe req, and select a req from store and probe
  val storeWaitCycles = RegInit(0.U(4.W))
  val StoreWaitThreshold = Wire(UInt(4.W))
  StoreWaitThreshold := Constantin.createRecord(s"StoreWaitThreshold_${p(XSCoreParamsKey).HartId}", initValue = 0)
  val storeWaitTooLong = storeWaitCycles >= StoreWaitThreshold
  val loadsAreComing = io.data_read.asUInt.orR
  val storeCanAccept = storeWaitTooLong || !loadsAreComing || io.force_write

  val store_req = Wire(DecoupledIO(new MainPipeReq))
  store_req.bits := (new MainPipeReq).convertStoreReq(io.store_req.bits)
  store_req.valid := io.store_req.valid && storeCanAccept
  io.store_req.ready := store_req.ready && storeCanAccept


  when (store_req.fire) { // if wait too long and write success, reset counter.
    storeWaitCycles := 0.U
  } .elsewhen (storeWaitCycles < StoreWaitThreshold && io.store_req.valid && !store_req.ready) { // if block store, increase counter.
    storeWaitCycles := storeWaitCycles + 1.U
  }

  // s0: read meta and tag
  val req = Wire(DecoupledIO(new MainPipeReq))
  arbiter(
    in = Seq(
      io.probe_req,
      io.refill_req,
      store_req, // Note: store_req.ready is now manually assigned for better timing
      io.atomic_req,
    ),
    out = req,
    name = Some("main_pipe_req")
  )

  val store_idx = get_idx(io.store_req.bits.vaddr)
  // manually assign store_req.ready for better timing
  // now store_req set conflict check is done in parallel with req arbiter
  store_req.ready := io.meta_read.ready && io.tag_read.ready && s1_ready && !store_set_conflict &&
    !io.probe_req.valid && !io.refill_req.valid && !io.atomic_req.valid
  val s0_req = req.bits
  val s0_idx = get_idx(s0_req.vaddr)
  val s0_need_tag = io.tag_read.valid
  val s0_can_go = io.meta_read.ready && io.tag_read.ready && s1_ready && !set_conflict
  val s0_fire = req.valid && s0_can_go

  req.ready := s0_can_go

  val bank_write = VecInit((0 until DCacheBanks).map(i => get_mask_of_bank(i, s0_req.store_mask).orR)).asUInt
  val bank_full_write = VecInit((0 until DCacheBanks).map(i => get_mask_of_bank(i, s0_req.store_mask).andR)).asUInt
  val banks_full_overwrite = bank_full_write.andR

  val banked_store_rmask = bank_write & ~bank_full_write
  val banked_full_rmask = ~0.U(DCacheBanks.W)
  val banked_none_rmask = 0.U(DCacheBanks.W)

  val store_need_data = !s0_req.probe && s0_req.isStore && banked_store_rmask.orR
  val probe_need_data = s0_req.probe
  val amo_need_data = !s0_req.probe && s0_req.isAMO
  val miss_need_data = s0_req.miss
  val replace_need_data = s0_req.replace

  val banked_need_data = store_need_data || probe_need_data || amo_need_data || miss_need_data || replace_need_data

  val s0_banked_rmask = Mux(store_need_data, banked_store_rmask,
    Mux(probe_need_data || amo_need_data || miss_need_data || replace_need_data,
      banked_full_rmask,
      banked_none_rmask
    ))

  // generate wmask here and use it in stage 2
  val banked_store_wmask = bank_write
  val banked_full_wmask = ~0.U(DCacheBanks.W)
  val banked_none_wmask = 0.U(DCacheBanks.W)

  // s1: read data
  val s1_valid = RegInit(false.B)
  val s1_need_data = RegEnable(banked_need_data, s0_fire)
  val s1_req = RegEnable(s0_req, s0_fire)
  val s1_banked_rmask = RegEnable(s0_banked_rmask, s0_fire)
  val s1_banked_store_wmask = RegEnable(banked_store_wmask, s0_fire)
  val s1_need_tag = RegEnable(s0_need_tag, s0_fire)
  val s1_can_go = s2_ready && (io.data_readline.ready || !s1_need_data)
  val s1_fire = s1_valid && s1_can_go
  val s1_idx = get_idx(s1_req.vaddr)
  val s1_dmWay = RegEnable(get_direct_map_way(s0_req.vaddr), s0_fire)

  when (s0_fire) {
    s1_valid := true.B
  }.elsewhen (s1_fire) {
    s1_valid := false.B
  }
  s1_ready := !s1_valid || s1_can_go
  s1_s0_set_conflict := s1_valid && s0_idx === s1_idx
  s1_s0_set_conflict_store := s1_valid && store_idx === s1_idx

  def wayMap[T <: Data](f: Int => T) = VecInit((0 until nWays).map(f))
  val meta_resp = Wire(Vec(nWays, (new Meta).asUInt))
  meta_resp := Mux(GatedValidRegNext(s0_fire), VecInit(io.meta_resp.map(_.asUInt)), RegEnable(meta_resp, s1_valid))
  // pseudo ecc enc tag
  val pseudo_tag_toggle_mask = Mux(
                                  io.pseudo_error.valid && io.pseudo_error.bits(0).valid,
                                  io.pseudo_error.bits(0).mask(tagBits - 1, 0),
                                  0.U(tagBits.W)
                              )
  val pseudo_encTag_resp = io.tag_resp.map {
    case real_enc =>
      if (cacheCtrlParamsOpt.nonEmpty && EnableTagEcc) {
        val ecc = real_enc(encTagBits - 1, tagBits)
        val toggleTag = real_enc(tagBits - 1, 0) ^ pseudo_tag_toggle_mask
        Cat(ecc, toggleTag)
      } else {
        real_enc
      }
  }
  val encTag_resp = Wire(io.tag_resp.cloneType)
  encTag_resp := Mux(GatedValidRegNext(s0_fire), VecInit(pseudo_encTag_resp), RegEnable(encTag_resp, s1_valid))
  val tag_resp = encTag_resp.map(encTag => encTag(tagBits - 1, 0))
  val s1_meta_valids = wayMap((w: Int) => Meta(meta_resp(w)).coh.isValid()).asUInt
  val s1_tag_errors = wayMap((w: Int) => s1_meta_valids(w) && dcacheParameters.tagCode.decode(encTag_resp(w)).error).asUInt
  val s1_tag_eq_way = wayMap((w: Int) => tag_resp(w) === get_tag(s1_req.addr) && !s1_tag_errors(w)).asUInt
  val s1_tag_match_way = wayMap((w: Int) => s1_tag_eq_way(w) && s1_meta_valids(w)).asUInt
  val s1_tag_match = ParallelORR(s1_tag_match_way)

  val s1_hit_tag = get_tag(s1_req.addr)
  val s1_hit_coh = ClientMetadata(ParallelMux(s1_tag_match_way.asBools, (0 until nWays).map(w => meta_resp(w))))
  val s1_flag_error = ParallelMux(s1_tag_match_way.asBools, (0 until nWays).map(w => io.extra_meta_resp(w).error))
  val s1_extra_meta = ParallelMux(s1_tag_match_way.asBools, (0 until nWays).map(w => io.extra_meta_resp(w)))
  io.pseudo_tag_error_inj_done := s1_fire && s1_meta_valids.orR

  XSPerfAccumulate("probe_unused_prefetch", s1_req.probe && isFromL1Prefetch(s1_extra_meta.prefetch) && !s1_extra_meta.access) // may not be accurate
  XSPerfAccumulate("replace_unused_prefetch", s1_req.replace && isFromL1Prefetch(s1_extra_meta.prefetch) && !s1_extra_meta.access) // may not be accurate

  // replacement policy
  val s1_invalid_vec = wayMap(w => !meta_resp(w).asTypeOf(new Meta).coh.isValid())
  val s1_have_invalid_way = s1_invalid_vec.asUInt.orR
  val s1_invalid_way_en = ParallelPriorityMux(s1_invalid_vec.zipWithIndex.map(x => x._1 -> UIntToOH(x._2.U(nWays.W))))
  val s1_repl_way_en = WireInit(0.U(nWays.W))
  s1_repl_way_en := Mux(
    GatedValidRegNext(s0_fire),
    UIntToOH(io.replace_way.way),
    RegEnable(s1_repl_way_en, s1_valid)
  )
  val s1_repl_tag = ParallelMux(s1_repl_way_en.asBools, (0 until nWays).map(w => tag_resp(w)))
  val s1_repl_coh = ParallelMux(s1_repl_way_en.asBools, (0 until nWays).map(w => meta_resp(w))).asTypeOf(new ClientMetadata)
  val s1_repl_pf  = ParallelMux(s1_repl_way_en.asBools, (0 until nWays).map(w => io.extra_meta_resp(w).prefetch))

  val s1_repl_way_raw = WireInit(0.U(log2Up(nWays).W))
  s1_repl_way_raw := Mux(GatedValidRegNext(s0_fire), io.replace_way.way, RegEnable(s1_repl_way_raw, s1_valid))

  val s1_need_replacement = s1_req.miss && !s1_tag_match
  val s1_need_eviction = s1_req.miss && !s1_tag_match && s1_repl_coh.state =/= ClientStates.Nothing

  val s1_way_en = Mux(s1_need_replacement, s1_repl_way_en, s1_tag_match_way)
  assert(!RegNext(s1_fire && PopCount(s1_way_en) > 1.U))

  val s1_tag = s1_hit_tag
  val s1_coh = s1_hit_coh

  XSPerfAccumulate("store_has_invalid_way_but_select_valid_way", io.replace_way.set.valid && wayMap(w => !meta_resp(w).asTypeOf(new Meta).coh.isValid()).asUInt.orR && s1_need_replacement && s1_repl_coh.isValid())
  XSPerfAccumulate("store_using_replacement", io.replace_way.set.valid && s1_need_replacement)

  val s1_has_permission = s1_hit_coh.onAccess(s1_req.cmd)._1
  val s1_hit = s1_tag_match && s1_has_permission
  val s1_pregen_can_go_to_mq = !s1_req.replace && !s1_req.probe && !s1_req.miss && (s1_req.isStore || s1_req.isAMO) && !s1_hit

  // s2: select data, return resp if this is a store miss
  val s2_valid = RegInit(false.B)
  val s2_req = RegEnable(s1_req, s1_fire)
  val s2_tag_errors = RegEnable(s1_tag_errors, s1_fire)
  val s2_tag_match = RegEnable(s1_tag_match, s1_fire)
  val s2_tag_match_way = RegEnable(s1_tag_match_way, s1_fire)
  val s2_hit_coh = RegEnable(s1_hit_coh, s1_fire)
  val (s2_has_permission, _, s2_new_hit_coh) = s2_hit_coh.onAccess(s2_req.cmd)

  val s2_repl_tag = RegEnable(s1_repl_tag, s1_fire)
  val s2_repl_coh = RegEnable(s1_repl_coh, s1_fire)
  val s2_repl_pf  = RegEnable(s1_repl_pf, s1_fire)
  val s2_need_replacement = RegEnable(s1_need_replacement, s1_fire)
  val s2_need_eviction = RegEnable(s1_need_eviction, s1_fire)
  val s2_need_data = RegEnable(s1_need_data, s1_fire)
  val s2_need_tag = RegEnable(s1_need_tag, s1_fire)
  val s2_idx = get_idx(s2_req.vaddr)


  val s2_way_en = RegEnable(s1_way_en, s1_fire)
  val s2_tag = Mux(s2_need_replacement, s2_repl_tag, RegEnable(s1_tag, s1_fire))
  val s2_coh = Mux(s2_need_replacement, s2_repl_coh, RegEnable(s1_coh, s1_fire))
  val s2_banked_store_wmask = RegEnable(s1_banked_store_wmask, s1_fire)
  val s2_flag_error = RegEnable(s1_flag_error, s1_fire)
  val s2_tag_error = WireInit(false.B)
  val s2_l2_error = Mux(io.refill_info.valid, io.refill_info.bits.error, s2_req.error)
  val s2_error = s2_flag_error || s2_tag_error || s2_l2_error // data_error not included

  val s2_may_report_data_error = s2_need_data && s2_coh.state =/= ClientStates.Nothing

  val s2_hit = s2_tag_match && s2_has_permission
  val s2_amo_hit = s2_hit && !s2_req.probe && !s2_req.miss && s2_req.isAMO
  val s2_store_hit = s2_hit && !s2_req.probe && !s2_req.miss && s2_req.isStore

  if(EnableTagEcc) {
    s2_tag_error := s2_tag_errors.orR && s2_need_tag
  }

  s2_s0_set_conlict := s2_valid && s0_idx === s2_idx
  s2_s0_set_conlict_store := s2_valid && store_idx === s2_idx

  // For a store req, it either hits and goes to s3, or miss and enter miss queue immediately
  val s2_req_miss_without_data = Mux(s2_valid, s2_req.miss && !io.refill_info.valid, false.B)
  val s2_can_go_to_mq_replay = (s2_req_miss_without_data && RegEnable(s2_req_miss_without_data && !io.mainpipe_info.s2_replay_to_mq, false.B, s2_valid)) || io.replace_block // miss_req in s2 but refill data is invalid, can block 1 cycle
  val s2_can_go_to_mq = RegEnable(s1_pregen_can_go_to_mq, s1_fire)
  val s2_can_go_to_s3 = (s2_req.replace || s2_req.probe || (s2_req.miss && io.refill_info.valid && !io.replace_block) || (s2_req.isStore || s2_req.isAMO) && s2_hit) && s3_ready
  assert(RegNext(!(s2_valid && s2_can_go_to_s3 && s2_can_go_to_mq && s2_can_go_to_mq_replay)))
  val s2_can_go = s2_can_go_to_s3 || s2_can_go_to_mq || s2_can_go_to_mq_replay
  val s2_fire = s2_valid && s2_can_go
  val s2_fire_to_s3 = s2_valid && s2_can_go_to_s3
  when (s1_fire) {
    s2_valid := true.B
  }.elsewhen (s2_fire) {
    s2_valid := false.B
  }
  s2_ready := !s2_valid || s2_can_go
  val replay = !io.miss_req.ready || io.wbq_block_miss_req

  val data_resp = Wire(io.data_resp.cloneType)
  data_resp := Mux(GatedValidRegNext(s1_fire), io.data_resp, RegEnable(data_resp, s2_valid))
  val s2_store_data_merged = Wire(Vec(DCacheBanks, UInt(DCacheSRAMRowBits.W)))

  def mergePutData(old_data: UInt, new_data: UInt, wmask: UInt): UInt = {
    val full_wmask = FillInterleaved(8, wmask)
    ((~full_wmask & old_data) | (full_wmask & new_data))
  }

  val s2_data = WireInit(VecInit((0 until DCacheBanks).map(i => {
    data_resp(i).raw_data
  })))

  for (i <- 0 until DCacheBanks) {
    val old_data = s2_data(i)
    val new_data = get_data_of_bank(i, Mux(s2_req.miss, io.refill_info.bits.store_data, s2_req.store_data))
    // for amo hit, we should use read out SRAM data
    // do not merge with store data
    val wmask = Mux(s2_amo_hit, 0.U(wordBytes.W), get_mask_of_bank(i, Mux(s2_req.miss, io.refill_info.bits.store_mask, s2_req.store_mask)))
    s2_store_data_merged(i) := mergePutData(old_data, new_data, wmask)
  }

  val s2_data_word = s2_store_data_merged(s2_req.word_idx)
  val s2_data_quad_word = VecInit((0 until DCacheBanks).map(i => {
    if (i == (DCacheBanks - 1)) s2_store_data_merged(i)
    else Cat(s2_store_data_merged(i + 1), s2_store_data_merged(i))
  }))(s2_req.word_idx)

  io.pseudo_data_error_inj_done := s2_fire_to_s3 && (s2_tag_error || s2_hit) && s2_may_report_data_error
  io.pseudo_error.ready := false.B
  XSError(s2_valid && s2_can_go_to_s3 && s2_req.miss && !io.refill_info.valid, "MainPipe req can go to s3 but no refill data")

  // s3: write data, meta and tag
  val s3_valid = RegInit(false.B)
  val s3_req = RegEnable(s2_req, s2_fire_to_s3)
  val s3_miss_param = RegEnable(io.refill_info.bits.miss_param, s2_fire_to_s3)
  val s3_miss_dirty = RegEnable(io.refill_info.bits.miss_dirty, s2_fire_to_s3)
  val s3_tag = RegEnable(s2_tag, s2_fire_to_s3)
  val s3_tag_error = RegEnable(s2_tag_error, s2_fire_to_s3)
  val s3_tag_match = RegEnable(s2_tag_match, s2_fire_to_s3)
  val s3_flag_error = RegEnable(s2_flag_error, s2_fire_to_s3)
  val s3_coh = RegEnable(s2_coh, s2_fire_to_s3)
  val s3_hit = RegEnable(s2_hit, s2_fire_to_s3)
  val s3_amo_hit = RegEnable(s2_amo_hit, s2_fire_to_s3)
  val s3_store_hit = RegEnable(s2_store_hit, s2_fire_to_s3)
  val s3_hit_coh = RegEnable(s2_hit_coh, s2_fire_to_s3)
  val s3_new_hit_coh = RegEnable(s2_new_hit_coh, s2_fire_to_s3)
  val s3_way_en = RegEnable(s2_way_en, s2_fire_to_s3)
  val s3_banked_store_wmask = RegEnable(s2_banked_store_wmask, s2_fire_to_s3)
  val s3_store_data_merged = RegEnable(s2_store_data_merged, s2_fire_to_s3)
  val s3_data_word = RegEnable(s2_data_word, s2_fire_to_s3)
  val s3_data_quad_word = RegEnable(s2_data_quad_word, s2_fire_to_s3)
  val s3_data = RegEnable(s2_data, s2_fire_to_s3)
  val s3_l2_error = RegEnable(s2_l2_error, s2_fire_to_s3)
  val s3_idx = RegEnable(s2_idx, s2_fire_to_s3)
  // data_error will be reported by data array 1 cycle after data read resp
  val s3_data_error = Wire(Bool())
  s3_data_error := Mux(GatedValidRegNextN(s1_fire,2), // ecc check result is generated 2 cycle after read req
    io.readline_error_delayed && RegNext(s2_may_report_data_error),
    RegNext(s3_data_error) // do not update s3_data_error if !s1_fire
  )
  // error signal for amo inst
  // s3_error = s3_flag_error || s3_tag_error || s3_l2_error || s3_data_error
  val s3_error = RegEnable(s2_error, 0.U.asTypeOf(s2_error), s2_fire_to_s3) || s3_data_error
  val (_, probe_shrink_param, probe_new_coh) = s3_coh.onProbe(s3_req.probe_param)
  val (_, miss_shrink_param, _) = s3_coh.onCacheControl(M_FLUSH)
  val s3_need_replacement = RegEnable(s2_need_replacement, s2_fire_to_s3)

  val miss_update_meta = s3_req.miss
  val probe_update_meta = s3_req.probe && s3_tag_match && s3_coh =/= probe_new_coh
  val store_update_meta = s3_req.isStore && !s3_req.probe && s3_hit_coh =/= s3_new_hit_coh
  val amo_update_meta = s3_req.isAMO && !s3_req.probe && s3_hit_coh =/= s3_new_hit_coh
  val amo_wait_amoalu = s3_req.isAMO && s3_req.cmd =/= M_XLR && s3_req.cmd =/= M_XSC && !isAMOCAS(s3_req.cmd)
  val update_meta = (miss_update_meta || probe_update_meta || store_update_meta || amo_update_meta) && !s3_req.replace

  def missCohGen(cmd: UInt, param: UInt, dirty: Bool) = {
    val c = categorize(cmd)
    MuxLookup(Cat(c, param, dirty), Nothing)(Seq(
      //(effect param) -> (next)
      Cat(rd, toB, false.B)  -> Branch,
      Cat(rd, toB, true.B)   -> Branch,
      Cat(rd, toT, false.B)  -> Trunk,
      Cat(rd, toT, true.B)   -> Dirty,
      Cat(wi, toT, false.B)  -> Trunk,
      Cat(wi, toT, true.B)   -> Dirty,
      Cat(wr, toT, false.B)  -> Dirty,
      Cat(wr, toT, true.B)   -> Dirty))
  }

  val miss_new_coh = ClientMetadata(missCohGen(s3_req.cmd, s3_miss_param, s3_miss_dirty))

  // LR, SC and AMO
  val debug_sc_fail_addr = RegInit(0.U)
  val debug_sc_fail_cnt  = RegInit(0.U(8.W))
  val debug_sc_addr_match_fail_cnt  = RegInit(0.U(8.W))

  val lrsc_count = RegInit(0.U(log2Ceil(LRSCCycles).W))
  val lrsc_valid = lrsc_count > LRSCBackOff.U
  val lrsc_addr = Reg(UInt())

  val s3_s_amoalu = RegInit(false.B)
  val s3_lr = !s3_req.probe && s3_req.isAMO && s3_req.cmd === M_XLR
  val s3_sc = !s3_req.probe && s3_req.isAMO && s3_req.cmd === M_XSC
  val s3_cas = !s3_req.probe && s3_req.isAMO && isAMOCAS(s3_req.cmd)
  val s3_lrsc_addr_match = lrsc_valid && lrsc_addr === get_block_addr(s3_req.addr)
  val s3_sc_fail = s3_sc && !s3_lrsc_addr_match
  val debug_s3_sc_fail_addr_match = s3_sc && lrsc_addr === get_block_addr(s3_req.addr) && !lrsc_valid

  val s3_cas_fail = s3_cas && (FillInterleaved(8, s3_req.amo_mask) & (s3_req.amo_cmp ^ s3_data_quad_word)) =/= 0.U

  val s3_can_do_amo = (s3_req.miss && !s3_req.probe && s3_req.isAMO) || s3_amo_hit
  val s3_can_do_amo_write = s3_can_do_amo && isWrite(s3_req.cmd) && !s3_sc_fail && !s3_cas_fail

  when (s3_valid && (s3_lr || s3_sc)) {
    when (s3_can_do_amo && s3_lr) {
      lrsc_count := (LRSCCycles - 1).U
      lrsc_addr := get_block_addr(s3_req.addr)
    } .otherwise {
      lrsc_count := 0.U
    }
  }.elsewhen (io.invalid_resv_set) {
    // when we release this block,
    // we invalidate this reservation set
    lrsc_count := 0.U
  }.elsewhen (lrsc_valid) {
    lrsc_count := lrsc_count - 1.U
  }


  io.lrsc_locked_block.valid := lrsc_valid
  io.lrsc_locked_block.bits  := lrsc_addr
  io.block_lr := GatedValidRegNext(lrsc_valid)

  // When we update update_resv_set, block all probe req in the next cycle
  // It should give Probe reservation set addr compare an independent cycle,
  // which will lead to better timing
  io.update_resv_set := s3_valid && s3_lr && s3_can_do_amo

  when (s3_valid) {
    when (s3_req.addr === debug_sc_fail_addr) {
      when (s3_sc_fail) {
        debug_sc_fail_cnt := debug_sc_fail_cnt + 1.U
      } .elsewhen (s3_sc) {
        debug_sc_fail_cnt := 0.U
      }
    } .otherwise {
      when (s3_sc_fail) {
        debug_sc_fail_addr := s3_req.addr
        debug_sc_fail_cnt  := 1.U
      }
    }
  }
  XSWarn(debug_sc_fail_cnt > 100.U, "L1DCache failed too many SCs in a row")

  when (s3_valid) {
    when (s3_req.addr === debug_sc_fail_addr) {
      when (debug_s3_sc_fail_addr_match) {
        debug_sc_addr_match_fail_cnt := debug_sc_addr_match_fail_cnt + 1.U
      } .elsewhen (s3_sc) {
        debug_sc_addr_match_fail_cnt := 0.U
      }
    } .otherwise {
      when (s3_sc_fail) {
        debug_sc_addr_match_fail_cnt  := 1.U
      }
    }
  }
  XSError(debug_sc_addr_match_fail_cnt > 100.U, "L1DCache failed too many SCs in a row, resv set addr always match")


  val banked_amo_wmask = UIntToOH(s3_req.word_idx)
  val update_data = s3_req.miss || s3_store_hit || s3_can_do_amo_write

  // generate write data
  // AMO hits
  val do_amoalu = amo_wait_amoalu && s3_valid && !s3_s_amoalu
  val amoalu   = Module(new AMOALU(wordBits))
  amoalu.io.mask := s3_req.amo_mask
  amoalu.io.cmd  := s3_req.cmd
  amoalu.io.lhs  := s3_data_word
  amoalu.io.rhs  := s3_req.amo_data

  // merge amo write data
  val s3_amo_data_merged = Wire(Vec(DCacheBanks, UInt(DCacheSRAMRowBits.W))) // exclude AMOCAS
  val s3_sc_data_merged = Wire(Vec(DCacheBanks, UInt(DCacheSRAMRowBits.W)))
  val s3_cas_data_merged = Wire(Vec(DCacheBanks, UInt(DCacheSRAMRowBits.W)))
  for (i <- 0 until DCacheBanks) {
    val old_data = s3_store_data_merged(i)
    val new_data = amoalu.io.out
    val wmask = Mux(
      s3_req.word_idx === i.U,
      ~0.U(wordBytes.W),
      0.U(wordBytes.W)
    )
    s3_amo_data_merged(i) := mergePutData(old_data, new_data, wmask)
    s3_sc_data_merged(i) := mergePutData(old_data, s3_req.amo_data,
      Mux(s3_req.word_idx === i.U && !s3_sc_fail, s3_req.amo_mask, 0.U(wordBytes.W))
    )
    val l_select = !s3_cas_fail && s3_req.word_idx === i.U
    val h_select = !s3_cas_fail && s3_req.cmd === M_XA_CASQ &&
      (if (i % 2 == 1) s3_req.word_idx === (i - 1).U else false.B)
    s3_cas_data_merged(i) := mergePutData(
      old_data = old_data,
      new_data = Mux(h_select, s3_req.amo_data >> DataBits, s3_req.amo_data.take(DataBits)),
      wmask = Mux(
        h_select,
        s3_req.amo_mask >> wordBytes,
        Mux(
          l_select,
          s3_req.amo_mask.take(wordBytes),
          0.U(wordBytes.W)
        )
      )
    )
  }
  val s3_amo_data_merged_reg = RegEnable(s3_amo_data_merged, do_amoalu)
  val miss_wb = s3_req.miss && s3_need_replacement && s3_coh.state =/= ClientStates.Nothing
  val probe_wb = s3_req.probe
  val replace_wb = s3_req.replace
  val need_wb = miss_wb || probe_wb || replace_wb

  val writeback_param = Mux(probe_wb, probe_shrink_param, miss_shrink_param)
  val writeback_data = if (dcacheParameters.alwaysReleaseData) {
    s3_tag_match && s3_req.probe && s3_req.probe_need_data ||
      s3_coh === ClientStates.Dirty || (miss_wb || replace_wb) && s3_coh.state =/= ClientStates.Nothing
  } else {
    s3_tag_match && s3_req.probe && s3_req.probe_need_data || s3_coh === ClientStates.Dirty
  }

  val s3_probe_can_go = s3_req.probe && io.wb.ready && (io.meta_write.ready || !probe_update_meta)
  val s3_store_can_go = s3_req.source === STORE_SOURCE.U && !s3_req.probe && (io.meta_write.ready || !store_update_meta) && (io.data_write.ready || !update_data) && !s3_req.miss
  val s3_amo_can_go = s3_amo_hit && (io.meta_write.ready || !amo_update_meta) && (io.data_write.ready || !update_data) && (s3_s_amoalu || !amo_wait_amoalu)
  val s3_miss_can_go = s3_req.miss &&
    (io.meta_write.ready || !amo_update_meta) &&
    (io.data_write.ready || !update_data) &&
    (s3_s_amoalu || !amo_wait_amoalu) &&
    io.tag_write.ready &&
    io.wb.ready
  val s3_replace_nothing = s3_req.replace && s3_coh.state === ClientStates.Nothing
  val s3_replace_can_go = s3_req.replace && (s3_replace_nothing || io.wb.ready)
  val s3_can_go = s3_probe_can_go || s3_store_can_go || s3_amo_can_go || s3_miss_can_go || s3_replace_can_go
  val s3_update_data_cango = s3_store_can_go || s3_amo_can_go || s3_miss_can_go // used to speed up data_write gen
  val s3_fire = s3_valid && s3_can_go
  when (s2_fire_to_s3) {
    s3_valid := true.B
  }.elsewhen (s3_fire) {
    s3_valid := false.B
  }
  when (do_amoalu) { s3_s_amoalu := true.B }
  when (s3_fire) { s3_s_amoalu := false.B }

  val s3_probe_new_coh = probe_new_coh
  val new_coh = Mux(
    miss_update_meta,
    miss_new_coh,
    Mux(
      probe_update_meta,
      s3_probe_new_coh,
      Mux(
        store_update_meta || amo_update_meta,
        s3_new_hit_coh,
        ClientMetadata.onReset
      )
    )
  )
  val banked_wmask = Mux(
    s3_req.miss,
    banked_full_wmask,
    Mux(
      s3_store_hit,
      s3_banked_store_wmask,
      Mux(
        s3_can_do_amo_write,
        Mux(
          isAMOCASQ(s3_req.cmd),
          FillInterleaved(2, UIntToOH(s3_req.quad_word_idx)),
          UIntToOH(s3_req.word_idx)
        ),
        banked_none_wmask
      )
    )
  )
  assert(!(s3_valid && banked_wmask.orR && !update_data))

  for (i <- 0 until DCacheBanks) {
    val old_data = s3_store_data_merged(i)
    s3_sc_data_merged(i) := mergePutData(old_data, s3_req.amo_data,
      Mux(
        s3_req.word_idx === i.U && !s3_sc_fail,
        s3_req.amo_mask,
        0.U(wordBytes.W)
      )
    )
  }
  for (i <- 0 until DCacheBanks) {
    io.data_write_dup(i).valid := s3_valid && s3_update_data_cango && update_data
    io.data_write_dup(i).bits.way_en := s3_way_en
    io.data_write_dup(i).bits.addr := s3_req.vaddr
  }

  s3_ready := !s3_valid || s3_can_go
  s3_s0_set_conflict := s3_valid && s3_idx === s0_idx
  s3_s0_set_conflict_store := s3_valid && s3_idx === store_idx
  //assert(RegNext(!s3_valid || !(s3_req.source === STORE_SOURCE.U && !s3_req.probe) || s3_hit)) // miss store should never come to s3 ,fixed(reserve)

  io.meta_read.valid := req.valid && !set_conflict
  io.meta_read.bits.idx := get_idx(s0_req.vaddr)
  io.meta_read.bits.way_en := Mux(s0_req.replace, s0_req.replace_way_en, ~0.U(nWays.W))

  io.tag_read.valid := req.valid && !set_conflict && !s0_req.replace
  io.tag_read.bits.idx := get_idx(s0_req.vaddr)
  io.tag_read.bits.way_en := ~0.U(nWays.W)

  io.data_read_intend := s1_valid && s1_need_data
  io.data_readline.valid := s1_valid && s1_need_data
  io.data_readline.bits.rmask := s1_banked_rmask
  io.data_readline.bits.way_en := s1_way_en
  io.data_readline.bits.addr := s1_req.vaddr

  io.miss_req.valid := s2_valid && s2_can_go_to_mq
  val miss_req = io.miss_req.bits
  miss_req := DontCare
  miss_req.source := s2_req.source
  miss_req.pf_source := L1_HW_PREFETCH_NULL
  miss_req.cmd := s2_req.cmd
  miss_req.addr := s2_req.addr
  miss_req.vaddr := s2_req.vaddr
  miss_req.store_data := s2_req.store_data
  miss_req.store_mask := s2_req.store_mask
  miss_req.word_idx := s2_req.word_idx
  miss_req.amo_data := s2_req.amo_data
  miss_req.amo_mask := s2_req.amo_mask
  miss_req.req_coh := s2_hit_coh
  miss_req.id := s2_req.id
  miss_req.cancel := false.B
  miss_req.pc := DontCare
  miss_req.full_overwrite := s2_req.isStore && s2_req.store_mask.andR

  io.wbq_conflict_check.valid := s2_valid && s2_can_go_to_mq
  io.wbq_conflict_check.bits := s2_req.addr

  io.store_replay_resp.valid := s2_valid && s2_can_go_to_mq && replay && s2_req.isStore
  io.store_replay_resp.bits.data := DontCare
  io.store_replay_resp.bits.miss := true.B
  io.store_replay_resp.bits.replay := true.B
  io.store_replay_resp.bits.id := s2_req.id

  io.store_hit_resp.valid := s3_valid && (s3_store_can_go || (s3_miss_can_go && s3_req.isStore))
  io.store_hit_resp.bits.data := DontCare
  io.store_hit_resp.bits.miss := false.B
  io.store_hit_resp.bits.replay := false.B
  io.store_hit_resp.bits.id := s3_req.id

  val atomic_hit_resp = Wire(new MainPipeResp)
  atomic_hit_resp.source := s3_req.source
  atomic_hit_resp.data := Mux(s3_sc, s3_sc_fail.asUInt, s3_data_quad_word)
  atomic_hit_resp.miss := false.B
  atomic_hit_resp.miss_id := s3_req.miss_id
  atomic_hit_resp.error := s3_error
  atomic_hit_resp.replay := false.B
  atomic_hit_resp.ack_miss_queue := s3_req.miss
  atomic_hit_resp.id := lrsc_valid
  val atomic_replay_resp = Wire(new MainPipeResp)
  atomic_replay_resp.source := s2_req.source
  atomic_replay_resp.data := DontCare
  atomic_replay_resp.miss := true.B
  atomic_replay_resp.miss_id := DontCare
  atomic_replay_resp.error := false.B
  atomic_replay_resp.replay := true.B
  atomic_replay_resp.ack_miss_queue := false.B
  atomic_replay_resp.id := DontCare

  val atomic_replay_resp_valid = s2_valid && s2_can_go_to_mq && replay && s2_req.isAMO
  val atomic_hit_resp_valid = s3_valid && (s3_amo_can_go || s3_miss_can_go && s3_req.isAMO)

  io.atomic_resp.valid := atomic_replay_resp_valid || atomic_hit_resp_valid
  io.atomic_resp.bits := Mux(atomic_replay_resp_valid, atomic_replay_resp, atomic_hit_resp)

  // io.replace_resp.valid := s3_fire && s3_req.replace
  // io.replace_resp.bits := s3_req.miss_id

  io.meta_write.valid := s3_fire && update_meta
  io.meta_write.bits.idx := s3_idx
  io.meta_write.bits.way_en := s3_way_en
  io.meta_write.bits.meta.coh := new_coh

  io.error_flag_write.valid := s3_fire && update_meta && (s3_l2_error || s3_req.miss)
  io.error_flag_write.bits.idx := s3_idx
  io.error_flag_write.bits.way_en := s3_way_en
  io.error_flag_write.bits.flag := s3_l2_error

  // if we use (prefetch_flag && meta =/= ClientStates.Nothing) for prefetch check
  // prefetch_flag_write can be omited
  io.prefetch_flag_write.valid := s3_fire && s3_req.miss
  io.prefetch_flag_write.bits.idx := s3_idx
  io.prefetch_flag_write.bits.way_en := s3_way_en
  io.prefetch_flag_write.bits.source := s3_req.pf_source

  // regenerate repl_way & repl_coh
  io.bloom_filter_query.set.valid := s2_fire_to_s3 && s2_req.miss && !isFromL1Prefetch(s2_repl_pf) && s2_repl_coh.isValid() && isFromL1Prefetch(s2_req.pf_source)
  io.bloom_filter_query.set.bits.addr := io.bloom_filter_query.set.bits.get_addr(Cat(s2_repl_tag, get_untag(s2_req.vaddr))) // the evict block address

  io.bloom_filter_query.clr.valid := s3_fire && isFromL1Prefetch(s3_req.pf_source)
  io.bloom_filter_query.clr.bits.addr := io.bloom_filter_query.clr.bits.get_addr(s3_req.addr)

  XSPerfAccumulate("mainpipe_update_prefetchArray", io.prefetch_flag_write.valid)
  XSPerfAccumulate("mainpipe_s2_miss_req", s2_valid && s2_req.miss)
  XSPerfAccumulate("mainpipe_s2_block_penalty", s2_valid && s2_req.miss && !io.refill_info.valid)
  XSPerfAccumulate("mainpipe_s2_missqueue_replay", s2_valid && s2_can_go_to_mq_replay)
  XSPerfAccumulate("mainpipe_slot_conflict_1_2", (s1_idx === s2_idx && s1_way_en === s2_way_en && s1_req.miss && s2_req.miss && s1_valid && s2_valid ))
  XSPerfAccumulate("mainpipe_slot_conflict_1_3", (s1_idx === s3_idx && s1_way_en === s3_way_en && s1_req.miss && s3_req.miss && s1_valid && s3_valid))
  XSPerfAccumulate("mainpipe_slot_conflict_2_3", (s2_idx === s3_idx && s2_way_en === s3_way_en && s2_req.miss && s3_req.miss && s2_valid && s3_valid))
  // probe / replace will not update access bit
  io.access_flag_write.valid := s3_fire && !s3_req.probe && !s3_req.replace
  io.access_flag_write.bits.idx := s3_idx
  io.access_flag_write.bits.way_en := s3_way_en
  // io.access_flag_write.bits.flag := true.B
  io.access_flag_write.bits.flag :=Mux(s3_req.miss, s3_req.access, true.B)

  io.tag_write.valid := s3_fire && s3_req.miss
  io.tag_write.bits.idx := s3_idx
  io.tag_write.bits.way_en := s3_way_en
  io.tag_write.bits.tag := get_tag(s3_req.addr)
  io.tag_write.bits.ecc := DontCare // generate ecc code in tagArray
  io.tag_write.bits.vaddr := s3_req.vaddr

  io.tag_write_intend := s3_req.miss && s3_valid
  XSPerfAccumulate("fake_tag_write_intend", io.tag_write_intend && !io.tag_write.valid)
  XSPerfAccumulate("mainpipe_tag_write", io.tag_write.valid)

  io.replace_addr.valid := s2_valid && s2_need_eviction
  io.replace_addr.bits  := get_block_addr(Cat(s2_tag, get_untag(s2_req.vaddr)))

  assert(!RegNext(io.tag_write.valid && !io.tag_write_intend))

  io.data_write.valid := s3_valid && s3_update_data_cango && update_data
  io.data_write.bits.way_en := s3_way_en
  io.data_write.bits.addr := s3_req.vaddr
  io.data_write.bits.wmask := banked_wmask
  io.data_write.bits.data := Mux(
    amo_wait_amoalu,
    s3_amo_data_merged_reg,
    Mux(
      s3_sc,
      s3_sc_data_merged,
      Mux(
        s3_cas,
        s3_cas_data_merged,
        s3_store_data_merged
      )
    )
  )
  //assert(RegNext(!io.meta_write.valid || !s3_req.replace))
  assert(RegNext(!io.tag_write.valid || !s3_req.replace))
  assert(RegNext(!io.data_write.valid || !s3_req.replace))

  io.wb.valid := s3_valid && (
    // replace
    s3_req.replace && !s3_replace_nothing ||
    // probe can go to wbq
    s3_req.probe && (io.meta_write.ready || !probe_update_meta) ||
      // amo miss can go to wbq
      s3_req.miss &&
        (io.meta_write.ready || !amo_update_meta) &&
        (io.data_write.ready || !update_data) &&
        (s3_s_amoalu || !amo_wait_amoalu) &&
        io.tag_write.ready
    ) && need_wb

  io.wb.bits.addr := get_block_addr(Mux(s3_tag_error, s3_req.addr, Cat(s3_tag, get_untag(s3_req.vaddr))))
  io.wb.bits.param := writeback_param
  io.wb.bits.voluntary := s3_req.miss || s3_req.replace
  io.wb.bits.hasData := writeback_data && !s3_tag_error
  io.wb.bits.dirty := s3_coh === ClientStates.Dirty
  io.wb.bits.data := s3_data.asUInt
  io.wb.bits.corrupt := s3_tag_error || s3_data_error
  io.wb.bits.delay_release := s3_req.replace
  io.wb.bits.miss_id := s3_req.miss_id

  // update plru in main pipe s3
  io.replace_access.valid := GatedValidRegNext(s2_fire_to_s3) && !s3_req.probe && (s3_req.miss || ((s3_req.isAMO || s3_req.isStore) && s3_hit))
  io.replace_access.bits.set := s3_idx
  io.replace_access.bits.way := OHToUInt(s3_way_en)

  io.replace_way.set.valid := GatedValidRegNext(s0_fire)
  io.replace_way.set.bits := s1_idx
  io.replace_way.dmWay := s1_dmWay

  // send evict hint to sms
  val sms_agt_evict_valid = s2_valid && s2_req.miss && s2_fire_to_s3
  io.sms_agt_evict_req.valid := GatedValidRegNext(sms_agt_evict_valid)
  io.sms_agt_evict_req.bits.vaddr := RegEnable(Cat(s2_repl_tag(tagBits - 1, 2), s2_req.vaddr(13,12), 0.U((VAddrBits - tagBits).W)), sms_agt_evict_valid)

  // TODO: consider block policy of a finer granularity
  io.status.s0_set.valid := req.valid
  io.status.s0_set.bits := get_idx(s0_req.vaddr)
  io.status.s1.valid := s1_valid
  io.status.s1.bits.set := s1_idx
  io.status.s1.bits.way_en := s1_way_en
  io.status.s2.valid := s2_valid && !s2_req.replace
  io.status.s2.bits.set := s2_idx
  io.status.s2.bits.way_en := s2_way_en
  io.status.s3.valid := s3_valid && !s3_req.replace
  io.status.s3.bits.set := s3_idx
  io.status.s3.bits.way_en := s3_way_en

  for ((s, i) <- io.status_dup.zipWithIndex) {
    s.s1.valid := s1_valid
    s.s1.bits.set := RegEnable(get_idx(s0_req.vaddr), s0_fire)
    s.s1.bits.way_en := s1_way_en
    s.s2.valid := s2_valid && !RegEnable(s1_req.replace, s1_fire)
    s.s2.bits.set := RegEnable(get_idx(s1_req.vaddr), s1_fire)
    s.s2.bits.way_en := RegEnable(s1_way_en, s1_fire)
    s.s3.valid := s3_valid && !RegEnable(s2_req.replace, s2_fire_to_s3)
    s.s3.bits.set := RegEnable(get_idx(s2_req.vaddr), s2_fire_to_s3)
    s.s3.bits.way_en := RegEnable(s2_way_en, s2_fire_to_s3)
  }
  dontTouch(io.status_dup)

  io.mainpipe_info.s2_valid := s2_valid && s2_req.miss
  io.mainpipe_info.s2_miss_id := s2_req.miss_id
  io.mainpipe_info.s2_replay_to_mq := s2_valid && s2_can_go_to_mq_replay
  io.mainpipe_info.s3_valid := s3_valid
  io.mainpipe_info.s3_miss_id := s3_req.miss_id
  io.mainpipe_info.s3_refill_resp := RegNext(s2_valid && s2_req.miss && s2_fire_to_s3)

  // report error to beu and csr, 1 cycle after read data resp
  io.error := 0.U.asTypeOf(ValidIO(new L1CacheErrorInfo))
  // report error, update error csr
  io.error.valid := s3_error && GatedValidRegNext(s2_fire)
  // only tag_error and data_error will be reported to beu
  // l2_error should not be reported (l2 will report that)
  io.error.bits.report_to_beu := (s3_tag_error || s3_data_error) && RegNext(s2_fire)
  io.error.bits.paddr := s3_req.addr
  io.error.bits.source.tag := s3_tag_error
  io.error.bits.source.data := s3_data_error
  io.error.bits.source.l2 := s3_flag_error || s3_l2_error
  io.error.bits.opType.store := s3_req.isStore && !s3_req.probe
  io.error.bits.opType.probe := s3_req.probe
  io.error.bits.opType.release := s3_req.replace
  io.error.bits.opType.atom := s3_req.isAMO && !s3_req.probe

  val perfEvents = Seq(
    ("dcache_mp_req          ", s0_fire                                                      ),
    ("dcache_mp_total_penalty", PopCount(VecInit(Seq(s0_fire, s1_valid, s2_valid, s3_valid))))
  )
  generatePerfEvent()
}
