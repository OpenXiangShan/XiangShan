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
import freechips.rocketchip.tilelink.ClientStates._
import freechips.rocketchip.tilelink.MemoryOpCategories._
import freechips.rocketchip.tilelink.TLPermissions._
import freechips.rocketchip.tilelink.{ClientMetadata, ClientStates, TLPermissions}
import org.chipsalliance.cde.config.Parameters
import utility._
import xiangshan.mem.HasL1PrefetchSourceParameter
import xiangshan.mem.prefetch._
import xiangshan.XSCoreParamsKey
import device.RERI.{ErrorSource, RERIErrorInfo, SeverityLevel}
import xiangshan.mem.{L1PrefetchReq, skidBuffer}

class MainPipeReq(implicit p: Parameters) extends DCacheBundle {
  val miss = Bool() // only amo miss will refill in main pipe
  val miss_id = UInt(log2Up(cfg.nMissEntries).W)
  val miss_param = UInt(TLPermissions.bdWidth.W)
  val miss_dirty = Bool()
  val occupy_way = UInt(nWays.W)
  val miss_fail_cause_evict_btot = Bool()

  val probe = Bool()
  val local_evict = Bool()
  val local_evict_probe = Bool()
  val local_evict_tag = Bool()
  val local_evict_clean_only = Bool()
  val local_evict_way_en = UInt(nWays.W)
  val local_evict_enc_tag = UInt(encTagBits.W)
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
  def isPrefetch: Bool = source === DCACHE_PREFETCH_SOURCE.U

  def quad_word_idx = word_idx >> 1

  def convertStoreReq(store: DCacheLineReq): MainPipeReq = {
    val req = Wire(new MainPipeReq)
    req := DontCare
    req.miss := false.B
    req.miss_dirty := false.B
    req.probe := false.B
    req.local_evict := false.B
    req.local_evict_probe := false.B
    req.local_evict_tag := false.B
    req.local_evict_clean_only := false.B
    req.local_evict_way_en := 0.U
    req.local_evict_enc_tag := 0.U
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
    req.miss_fail_cause_evict_btot := false.B
    req
  }

  def convertPrefetchReq(prefetch: L1PrefetchReq): MainPipeReq = {
    val req = Wire(new MainPipeReq)
    req := DontCare
    req.miss := false.B
    req.miss_dirty := false.B
    req.probe := false.B
    req.local_evict := false.B
    req.local_evict_probe := false.B
    req.local_evict_tag := false.B
    req.local_evict_clean_only := false.B
    req.local_evict_way_en := 0.U
    req.local_evict_enc_tag := 0.U
    req.probe_need_data := false.B
    req.source := DCACHE_PREFETCH_SOURCE.U
    req.cmd := MemoryOpConstants.M_PFR
    req.addr := prefetch.paddr
    req.vaddr := prefetch.vaddr
    req.replace := false.B
    req.error := false.B
    req.miss_fail_cause_evict_btot := false.B
    req.pf_source := prefetch.pf_source.value
    req.access := false.B
    req.id := 0.U
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
  val s2_evict_BtoT_way = Bool()
  val s2_next_evict_way = UInt(nWays.W)
  val s3_valid = Bool()
  val s3_miss_id = UInt(log2Up(cfg.nMissEntries).W) // For mshr release
  val s3_refill_resp = Bool()
}

class MainPipe(implicit p: Parameters) extends DCacheModule with HasPerfEvents with HasL1PrefetchSourceParameter {
  val io = IO(new Bundle() {
    // probe queue
    val probe_req = Flipped(DecoupledIO(new MainPipeReq))
    val probe_resp = Valid(new ProbeResp)
    val tag_evict = Decoupled(new EccEvictReq)
    val data_evict = Decoupled(new EccEvictReq)
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
    val evict_done = Output(Valid(Bool()))
    val wb_ready_dup = Vec(nDupWbReady, Input(Bool()))
    // hardware prefetch
    val prefetch_req = Flipped(Decoupled(new L1PrefetchReq()))
    // pass to Prefetch Monitor for statistic
    val prefetch_stat = Output(new PipePrefetchStatBundle)

    // data sram
    val data_read = Vec(LoadPipelineWidth, Input(Bool()))
    val data_read_intend = Output(Bool())
    val data_readline = DecoupledIO(new L1BankedDataReadLineReq)
    val data_readline_can_go = Output(Bool())
    val data_readline_stall = Output(Bool())
    val data_readline_can_resp = Output(Bool())
    val data_resp = Input(Vec(DCacheBanks, new L1BankedDataReadResult()))
    val data_ecc = Input(Vec(DCacheBanks, new DCacheEccResult(DCacheSRAMRowBits)))
    val data_correctable = Input(Vec(DCacheBanks, Bool()))
    val data_uncorrectable = Input(Vec(DCacheBanks, Bool()))
    val data_write = DecoupledIO(new L1BankedDataWriteReq)
    val data_write_dup = Vec(DCacheBanks, Valid(new L1BankedDataWriteReqCtrl))
    val data_write_ready_dup = Vec(nDupDataWriteReady, Input(Bool()))

    // meta array
    val meta_read = DecoupledIO(new MetaReadReq)
    val meta_resp = Input(Vec(nWays, new Meta))
    val meta_write = DecoupledIO(new CohMetaWriteReq)
    val extra_meta_resp = Input(Vec(nWays, new DCacheExtraMeta))
    val error_flag_write = DecoupledIO(new ErrorMetaWriteReq)
    val prefetch_flag_write = DecoupledIO(new SourceMetaWriteReq)
    val access_flag_write = DecoupledIO(new FlagMetaWriteReq)
    val latency_flag_write = DecoupledIO(new LatencyMetaWriteReq)

    // tag sram
    val tag_read = DecoupledIO(new TagReadReq)
    val tag_resp = Input(Vec(nWays, UInt(encTagBits.W)))
    val tag_write = DecoupledIO(new TagWriteReq)
    val misstrack_install = ValidIO(new MissTrackResident)
    val tag_write_ready_dup = Vec(nDupTagWriteReady, Input(Bool()))
    val tag_write_intend = Output(new Bool())

    // update state vec in replacement algo
    val replace_access = ValidIO(new ReplacementAccessBundle)
    // find the way to be replaced
    val replace_way = new ReplacementWayReqIO

    val evict_set = Output(UInt())
    val btot_ways_for_set = Input(UInt(nWays.W))

    // writeback addr to be replaced
    val replace = new MissQueueBlockIO

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
    val error = Output(new RERIErrorInfo)
    // force write
    val force_write = Input(Bool())

    val bloom_filter_query = new Bundle {
      val set = ValidIO(new BloomQueryBundle(BLOOM_FILTER_ENTRY_NUM))
      val clr = ValidIO(new BloomQueryBundle(BLOOM_FILTER_ENTRY_NUM))
    }
  })

  private val eccCorrector = Module(new DCacheEccCorrect)

  // meta array is made of regs, so meta write or read should always be ready
  assert(RegNext(io.meta_read.ready))
  assert(RegNext(io.meta_write.ready))

  val s1_s0_set_conflict, s2_s0_set_conflict, s3_s0_set_conflict = Wire(Bool())
  val set_conflict = s1_s0_set_conflict || s2_s0_set_conflict || s3_s0_set_conflict
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

  // convert prefetch req to main pipe req
  val prefetch_req = Wire(DecoupledIO(new MainPipeReq))
  prefetch_req.bits := (new MainPipeReq).convertPrefetchReq(io.prefetch_req.bits)
  prefetch_req.valid := io.prefetch_req.valid
  io.prefetch_req.ready := prefetch_req.ready

  // s0: read meta and tag
  val req = Wire(DecoupledIO(new MainPipeReq))
  val arbiter_out = Wire(DecoupledIO(new MainPipeReq))
  arbiter(
    in = Seq(
      io.probe_req,
      io.refill_req,
      prefetch_req, // Todo: what's the best priority
      store_req,
      io.atomic_req,
    ),
    out = arbiter_out,
    name = Some("main_pipe_req")
  )
  // skidBuffer to cut timing path of ready
  skidBuffer(
    arbiter_out,
    req,
    false.B,
    "MainPipeSkidBuffer"
  )
  // Keep requestor ready driven by the arbiter so it observes the same
  // handshake as the skid buffer input.
  val s0_req = req.bits
  val s0_idx = get_dcache_idx(s0_req.vaddr)
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
  val amo_need_data = !s0_req.probe && s0_req.isAMO && !s0_req.miss
  val miss_need_data = s0_req.miss
  val replace_need_data = s0_req.replace

  val banked_need_data = store_need_data || probe_need_data || amo_need_data || miss_need_data || replace_need_data
  val banked_amo_rmask = Mux(
    isAMOCASQ(s0_req.cmd),
    bankMaskFromBase(quadWordBankBase(s0_req.quad_word_idx), DCacheQuadWordBankCount),
    bankMaskFromBase(wordBankBase(s0_req.word_idx), DCacheWordBankCount)
  )

  val s0_banked_rmask = Mux(
    store_need_data,
    banked_store_rmask,
    Mux(
      amo_need_data,
      banked_amo_rmask,
      Mux(
        probe_need_data || miss_need_data || replace_need_data,
        banked_full_rmask,
        banked_none_rmask
      )
    )
  )

  // generate wmask here and use it in stage 2
  val banked_store_wmask = bank_write
  val banked_full_wmask = ~0.U(DCacheBanks.W)
  val banked_none_wmask = 0.U(DCacheBanks.W)

  // s1: read data
  val s1_valid = RegInit(false.B)
  val s1_req = RegEnable(s0_req, s0_fire)

  val meta_resp = Wire(Vec(nWays, (new Meta).asUInt))
  val s1_repl_way_en = WireInit(0.U(nWays.W))
  val s1_repl_poison = Wire(Bool())
  val s1_repl_coh = ParallelMux(s1_repl_way_en.asBools, (0 until nWays).map(w => meta_resp(w))).asTypeOf(new ClientMetadata)
  val s1_need_data = if (dcacheParameters.alwaysReleaseData) {
    RegEnable(banked_need_data, s0_fire)
  } else {
    Mux(!s1_req.miss, RegEnable(banked_need_data, s0_fire),
      s1_repl_coh.state === ClientStates.Dirty || s1_repl_poison)
  }

  val s1_banked_rmask = RegEnable(s0_banked_rmask, s0_fire)
  val s1_banked_store_wmask = RegEnable(banked_store_wmask, s0_fire)
  val s1_need_tag = RegEnable(s0_need_tag, s0_fire)
  val s1_can_go = s2_ready && (io.data_readline.ready || !s1_need_data)
  val s1_fire = s1_valid && s1_can_go
  val s1_idx = get_dcache_idx(s1_req.vaddr)
  val s1_dmWay = RegEnable(get_direct_map_way(s0_req.vaddr), s0_fire)
  val s1_isPrefetch = !s1_req.replace && !s1_req.probe && !s1_req.miss && s1_req.isPrefetch

  when (s0_fire) {
    s1_valid := true.B
  }.elsewhen (s1_fire) {
    s1_valid := false.B
  }
  s1_ready := !s1_valid || s1_can_go
  s1_s0_set_conflict := s1_valid && s0_idx === s1_idx && !s1_isPrefetch

  def wayMap[T <: Data](f: Int => T) = VecInit((0 until nWays).map(f))
  meta_resp := Mux(GatedValidRegNext(s0_fire), VecInit(io.meta_resp.map(_.asUInt)), RegEnable(meta_resp, s1_valid))
  val encTag_resp = Wire(io.tag_resp.cloneType)
  // TagArray is synchronous.  While S1 waits for its readline response, a
  // younger S0 request may issue another tag read; retain the response paired
  // with the S1 request exactly as we do for meta_resp.
  encTag_resp := Mux(GatedValidRegNext(s0_fire), io.tag_resp, RegEnable(encTag_resp, s1_valid))
  val s1_extra_meta_resp = Wire(io.extra_meta_resp.cloneType)
  s1_extra_meta_resp := Mux(GatedValidRegNext(s0_fire), io.extra_meta_resp, RegEnable(s1_extra_meta_resp, s1_valid))
  val tag_resp = encTag_resp.map(encTag => encTag(tagBits - 1, 0))
  val s1_meta_valids = wayMap((w: Int) => Meta(meta_resp(w)).coh.isValid()).asUInt
  val s1_tag_decoders = (0 until nWays).map { w =>
    val decoder = Module(new DCacheEccDetect(tagBits, dcacheParameters.tagCode))
    decoder.io.encoded := encTag_resp(w)
    decoder.io.valid := s1_meta_valids(w) && s1_need_tag
    decoder
  }
  val s1_tag_correctable = VecInit(s1_tag_decoders.map(_.io.correctable)).asUInt
  val s1_tag_uncorrectable = VecInit(s1_tag_decoders.map(_.io.uncorrectable)).asUInt
  val s1_tag_errors = s1_tag_correctable | s1_tag_uncorrectable
  val s1_tag_eq_way = wayMap((w: Int) => tag_resp(w) === get_tag(s1_req.addr)).asUInt
  val s1_tag_ecc_eq_way = wayMap((w: Int) => s1_tag_eq_way(w) && !s1_tag_errors(w)).asUInt
  val s1_tag_ecc_match_way = wayMap((w: Int) => s1_tag_ecc_eq_way(w) && s1_meta_valids(w)).asUInt

  // Ordinary requests select their hit/replacement in s1 using uncorrected tags.
  // Local evicts instead revalidate the exact encoded tag captured by the detector.
  val s1_local_target_present = (s1_req.local_evict_way_en & s1_meta_valids).orR &&
    ParallelMux(s1_req.local_evict_way_en.asBools, encTag_resp) === s1_req.local_evict_enc_tag &&
    !(s1_tag_uncorrectable & s1_req.local_evict_way_en).orR &&
    !(s1_req.local_evict_clean_only &&
      ParallelMux(s1_req.local_evict_way_en.asBools, s1_extra_meta_resp).error.asUInt.orR)
  val s1_hit_way_en = Mux(s1_req.local_evict,
    Mux(s1_local_target_present, s1_req.local_evict_way_en, 0.U(nWays.W)), s1_tag_ecc_match_way)
  val s1_tag_match = s1_hit_way_en.orR
  val s1_hit_tag = get_tag(s1_req.addr)
  val s1_hit_coh = ClientMetadata(ParallelMux(s1_hit_way_en.asBools, (0 until nWays).map(w => meta_resp(w))))
  val s1_hit_prefetch = ParallelMux(s1_tag_ecc_match_way.asBools, s1_extra_meta_resp.map(_.prefetch))
  val s1_extra_meta = ParallelMux(s1_hit_way_en.asBools, s1_extra_meta_resp)
  val s1_flag_error = s1_extra_meta.error

  XSPerfAccumulate("probe_unused_prefetch", s1_req.probe && isFromL1Prefetch(s1_extra_meta.prefetch) && !s1_extra_meta.access) // may not be accurate
  XSPerfAccumulate("replace_unused_prefetch", s1_req.replace && isFromL1Prefetch(s1_extra_meta.prefetch) && !s1_extra_meta.access) // may not be accurate

  // replacement policy
  val s1_invalid_vec = wayMap(w => !meta_resp(w).asTypeOf(new Meta).coh.isValid())
  val s1_have_invalid_way = s1_invalid_vec.asUInt.orR
  val s1_invalid_way_en = ParallelPriorityMux(s1_invalid_vec.zipWithIndex.map(x => x._1 -> UIntToOH(x._2.U(nWays.W))))
  s1_repl_way_en := Mux(
    GatedValidRegNext(s0_fire),
    Mux(s1_req.miss_fail_cause_evict_btot, s1_req.occupy_way, UIntToOH(io.replace_way.way)),
    RegEnable(s1_repl_way_en, s1_valid)
  )
  val s1_repl_way = Wire(UInt(wayBits.W))
  s1_repl_way := Mux(
    GatedValidRegNext(s0_fire),
    Mux(s1_req.miss_fail_cause_evict_btot, OHToUInt(s1_req.occupy_way), io.replace_way.way),
    RegEnable(s1_repl_way, s1_valid)
  ) // UInt format of `s1_repl_way_en`
  val s1_repl_tag = ParallelMux(s1_repl_way_en.asBools,
                                (0 until nWays).map(w => tag_resp(w)))
  val s1_repl_pf  = ParallelMux(s1_repl_way_en.asBools, s1_extra_meta_resp.map(_.prefetch))
  val s1_repl_flag_error = ParallelMux(s1_repl_way_en.asBools, s1_extra_meta_resp).error
  s1_repl_poison := s1_repl_flag_error.asUInt.orR

  val s1_need_replacement = s1_req.miss && !s1_tag_match
  val s1_need_eviction = s1_req.miss && !s1_tag_match && s1_repl_coh.state =/= ClientStates.Nothing
  val s1_way_en = Mux(s1_need_replacement, s1_repl_way_en, s1_hit_way_en)
  val s1_way = Mux(s1_req.local_evict, OHToUInt(s1_req.local_evict_way_en),
    Mux(s1_need_replacement, s1_repl_way, OHToUInt(s1_tag_ecc_match_way)))
  val s1_tag_ce_way = PriorityEncoderOH(s1_tag_correctable)
  val s1_tag_ce_enc = ParallelMux(s1_tag_ce_way.asBools, encTag_resp)
  val s1_selected_enc_tag = ParallelMux(s1_way_en.asBools, encTag_resp)
  assert(!RegNext(s1_fire && PopCount(s1_way_en) > 1.U))
  assert(!RegNext(s1_fire && s1_req.local_evict && PopCount(s1_req.local_evict_way_en) =/= 1.U))

  val s1_tag = s1_hit_tag
  val s1_coh = s1_hit_coh

  XSPerfAccumulate("store_has_invalid_way_but_select_valid_way", io.replace_way.set.valid && wayMap(w => !meta_resp(w).asTypeOf(new Meta).coh.isValid()).asUInt.orR && s1_need_replacement && s1_repl_coh.isValid())
  XSPerfAccumulate("store_using_replacement", io.replace_way.set.valid && s1_need_replacement)

  val (s1_has_permission, s1_shrink_perm, s1_new_hit_coh) = s1_hit_coh.onAccess(s1_req.cmd)
  val s1_hit = s1_tag_match && s1_has_permission
  val s1_isStore = !s1_req.replace && !s1_req.probe && !s1_req.miss && s1_req.isStore
  val s1_isAMO = !s1_req.replace && !s1_req.probe && !s1_req.miss && s1_req.isAMO && s1_req.cmd =/= M_XSC
  val s1_pregen_can_go_to_mq = (s1_isStore || s1_isAMO || s1_isPrefetch) && !s1_hit
  val s1_grow_perm = s1_shrink_perm === BtoT && !s1_has_permission

  // Local evicts correct tags in s2; probe-path data is corrected in s3.
  eccCorrector.io.tag := VecInit(tag_resp)
  eccCorrector.io.tagErrorMask := VecInit(s1_tag_decoders.map(_.io.errorMask))
  eccCorrector.io.tagEnable := s1_fire && s1_req.local_evict

  // s2: select data, return resp if this is a store miss
  val s2_valid = RegInit(false.B)
  val s2_req = RegEnable(s1_req, s1_fire)
  val s2_vaddr_alias = RegEnable(get_alias(s1_req.vaddr), s1_fire)
  val s2_vaddr_set = RegEnable(addr_to_dcache_set(s1_req.vaddr), s1_fire)
  val s2_tag_errors = RegEnable(s1_tag_errors, s1_fire)
  val s2_local_evict_tag = ParallelMux(s2_req.local_evict_way_en.asBools, eccCorrector.io.correctedTag)
  // The evict way is fixed in s1. A delegated probe still checks that this
  // corrected line is the line L2 requested before changing its permissions.
  val s2_tag_match = RegEnable(s1_tag_match, s1_fire) &&
    (!s2_req.local_evict_probe || s2_local_evict_tag === get_tag(s2_req.addr))
  val s2_tag_ecc_match_way = RegEnable(s1_tag_ecc_match_way, s1_fire)
  val s2_hit_coh = RegEnable(s1_hit_coh, s1_fire)
  val s2_has_permission = RegEnable(s1_has_permission, s1_fire)
  val s2_new_hit_coh = RegEnable(s1_new_hit_coh, s1_fire)
  val s2_grow_perm = RegEnable(s1_grow_perm, s1_fire) && s2_tag_match
  val s2_hit_prefetch = RegEnable(s1_hit_prefetch, s1_fire)

  val s2_repl_tag = RegEnable(s1_repl_tag, s1_fire)
  val s2_repl_coh = RegEnable(s1_repl_coh, s1_fire)
  val s2_repl_pf  = RegEnable(s1_repl_pf, s1_fire)

  val s2_refill_tag_eq_way = false.B
  val s2_need_replacement = RegEnable(s1_need_replacement, s1_fire)
  val s2_need_eviction = RegEnable(s1_need_eviction, s1_fire)
  val s2_need_data = RegEnable(s1_need_data, s1_fire)
  val s2_need_tag = RegEnable(s1_need_tag, s1_fire)
  val s2_idx = get_dcache_idx(s2_req.vaddr)

  val s2_local_probe_retry = Wire(Bool())
  val s2_way_en = RegEnable(s1_way_en, s1_fire)
  val s2_tag = Mux(s2_req.local_evict && (!s2_req.local_evict_probe || s2_local_probe_retry),
    s2_local_evict_tag, Mux(s2_need_replacement, s2_repl_tag, RegEnable(s1_tag, s1_fire)))
  val s2_coh = Mux(s2_req.local_evict_probe && !s2_local_probe_retry && !s2_tag_match,
    ClientMetadata.onReset,
    Mux(s2_need_replacement, s2_repl_coh, RegEnable(s1_coh, s1_fire)))
  val s2_banked_store_wmask = RegEnable(s1_banked_store_wmask, s1_fire)
  val s2_flag_error = Mux(s2_req.local_evict_probe && !s2_local_probe_retry && !s2_tag_match,
    0.U.asTypeOf(new TLError()), RegEnable(s1_flag_error, s1_fire))
  val s2_repl_flag_error = RegEnable(s1_repl_flag_error, s1_fire)

  val s2_tag_correctable = RegEnable(s1_tag_correctable, s1_fire)
  val s2_tag_uncorrectable = RegEnable(s1_tag_uncorrectable, s1_fire)
  // Only the selected way can affect this transaction's recovery/writeback.
  // Other valid ways may carry independent ECC errors.
  val s2_selected_tag_uec = (s2_tag_uncorrectable & s2_way_en).orR
  val s2_tag_ce_way = RegEnable(s1_tag_ce_way, s1_fire)
  val s2_tag_ce_enc = RegEnable(s1_tag_ce_enc, s1_fire)
  val s2_selected_enc_tag = RegEnable(s1_selected_enc_tag, s1_fire)
  val s2_banked_rmask = RegEnable(s1_banked_rmask, s1_fire)
  val s2_probe_tag_ce_recovery = s2_req.probe && !s2_req.local_evict && !s2_tag_match &&
    s2_need_tag && s2_tag_correctable.orR && !s2_tag_uncorrectable.orR
  val s2_tag_ce_recovery = s2_probe_tag_ce_recovery || !s2_req.probe && s2_need_tag &&
    s2_tag_correctable.orR && !s2_tag_uncorrectable.orR && !s2_flag_error.asUInt.orR
  val s2_tag_error = WireInit(false.B)
  val s2_l2_error = Mux(s2_req.miss && io.refill_info.valid, io.refill_info.bits.error, 0.U.asTypeOf(new TLError()))
  val s2_refill_latency = Mux(io.refill_info.valid && isFromL1Prefetch(s2_req.pf_source), io.refill_info.bits.refill_latency, 0.U)
  val s2_error = s2_flag_error.asUInt.orR || s2_tag_error || s2_l2_error.asUInt.orR // data_error not included
  // An uncorrectable selected replacement tag has no trustworthy release
  // address. Replay the refill through its MSHR instead of committing it.
  val s2_refill_tag_uec = s2_req.miss && io.refill_info.valid && s2_tag_error

  val s2_may_report_data_error = s2_need_data && s2_coh.state =/= ClientStates.Nothing
  val s2_data_ce = (io.data_correctable.asUInt & s2_banked_rmask).orR
  val s2_data_uec = (io.data_uncorrectable.asUInt & s2_banked_rmask).orR
  // A snoop waits for local repair, then retries through its original entry.
  // The local eviction itself must reach s3 to perform correction.
  val s2_data_ce_recovery = !s2_req.local_evict && s2_may_report_data_error && s2_data_ce && !s2_data_uec &&
    !s2_selected_tag_uec && !s2_flag_error.asUInt.orR && !s2_tag_ce_recovery
  val s2_ecc_recovery = s2_tag_ce_recovery || s2_data_ce_recovery

  val s2_hit = (s2_tag_match || s2_refill_tag_eq_way) && s2_has_permission
  val s2_sc = s2_req.cmd === M_XSC
  val s2_lr = s2_req.cmd === M_XLR
  val s2_amo_hit = s2_hit && !s2_req.probe && !s2_req.miss && s2_req.isAMO
  val s2_store_hit = s2_hit && !s2_req.probe && !s2_req.miss && s2_req.isStore
  val s2_should_not_report_ecc_error = !s2_req.probe && s2_req.isAMO
  val s2_isPrefetch = !s2_req.replace && !s2_req.probe && !s2_req.miss && s2_req.isPrefetch

  if(EnableTagEcc) {
    val s2_probe_or_atomic = (s2_req.probe || s2_req.isAMO && !s2_sc) && !s2_req.miss
    val s2_probe_atomic_tag_error = s2_probe_or_atomic && !s2_tag_match && s2_tag_uncorrectable.orR
    val s2_evict_tag_error = !s2_probe_or_atomic && (s2_tag_uncorrectable & s2_way_en).orR
    s2_tag_error := Mux(s2_req.local_evict,
      (s2_tag_uncorrectable & s2_req.local_evict_way_en).orR,
      s2_probe_atomic_tag_error || s2_evict_tag_error) && s2_need_tag
  }

  // If this repaired way belongs to another line, release it first and let
  // ProbeQueue look up the original probe again. No corrected tag selects a way.
  s2_local_probe_retry := s2_req.local_evict_probe && !s2_tag_match && !s2_tag_error

  s2_s0_set_conflict := s2_valid && s0_idx === s2_idx && !s2_isPrefetch

  // BtoT grow blocked: too many in-flight BtoT occupies in this set; replay store
  val s2_has_more_then_3_ways_BtoT = PopCount(io.btot_ways_for_set) > (nWays-2).U
  val s2_grow_perm_fail = s2_has_more_then_3_ways_BtoT && s2_grow_perm
  XSError(s2_valid && s2_grow_perm && io.btot_ways_for_set.andR,
    "BtoT grow permission, but all ways are BtoT\n"
  )

  // For a store req, it either hits and goes to s3, or miss and enter miss queue immediately
  val s2_replace_block = io.replace.block && io.replace.req.valid
  val s2_req_miss_without_data = Mux(s2_valid, s2_req.miss && !io.refill_info.valid, false.B)
  val s2_can_go_to_mq_no_data = (s2_req_miss_without_data && RegEnable(s2_req_miss_without_data && !io.mainpipe_info.s2_replay_to_mq, false.B, s2_valid)) // miss_req in s2 but refill data is invalid, can block 1 cycle
  val s2_can_go_to_mq_evict_fail = s2_replace_block // refill eviction conflicts with in-flight MSHR (BtoT); replay to MQ for another evict way
  val s2_can_go_to_mq_replay = (s2_can_go_to_mq_no_data || s2_can_go_to_mq_evict_fail) &&
    !s2_ecc_recovery
  val s2_can_go_to_mq = RegEnable(s1_pregen_can_go_to_mq, s1_fire) && !(s2_req.isAMO && s2_tag_error) &&
    !s2_ecc_recovery
  // If an evict entry is busy, replay still drains this request so the pending
  // local evict can use MainPipe. The retried access will request repair again.
  val s2_can_go_ecc_replay = s2_ecc_recovery
  val s2_can_go_to_s3 = (s2_sc || s2_req.replace || s2_req.probe ||
    Mux(
      s2_req.miss,
      io.refill_info.valid && !s2_replace_block,
      (s2_req.isStore || s2_req.isAMO || s2_req.isPrefetch) && s2_hit
    ) || (s2_req.isAMO && s2_tag_error)
  ) && s3_ready && !s2_ecc_recovery
  assert(RegNext(!(s2_valid && s2_can_go_to_s3 && s2_can_go_to_mq && s2_can_go_to_mq_replay)))
  val s2_can_go = s2_can_go_to_s3 || s2_can_go_to_mq || s2_can_go_to_mq_replay || s2_can_go_ecc_replay
  val s2_fire = s2_valid && s2_can_go
  val s2_fire_to_s3 = s2_valid && s2_can_go_to_s3
  when (s1_fire) {
    s2_valid := true.B
  }.elsewhen (s2_fire) {
    s2_valid := false.B
  }
  s2_ready := !s2_valid || s2_can_go
  val s2_valid_to_s3 = s2_fire_to_s3
  val s2_ecc_replay = s2_valid && s2_can_go_ecc_replay
  val replay = !io.miss_req.ready || io.wbq_block_miss_req

  io.data_readline_can_go := GatedValidRegNext(s1_fire)
  io.data_readline_can_resp := s2_fire_to_s3
  io.data_readline_stall := s2_valid

  eccCorrector.io.data := VecInit(io.data_resp.map(_.raw_data))
  eccCorrector.io.dataErrorMask := VecInit(io.data_ecc.map(_.errorMask))

  io.tag_evict.valid := s2_ecc_replay && s2_tag_ce_recovery
  io.tag_evict.bits.addr := get_block_addr(s2_req.addr)
  io.tag_evict.bits.vaddr := get_block_addr(s2_req.vaddr)
  io.tag_evict.bits.way_en := s2_tag_ce_way
  io.tag_evict.bits.enc_tag := s2_tag_ce_enc
  io.tag_evict.bits.clean_only := false.B
  io.tag_evict.bits.probe := s2_probe_tag_ce_recovery
  io.tag_evict.bits.probe_param := s2_req.probe_param
  io.tag_evict.bits.probe_need_data := s2_req.probe_need_data
  io.tag_evict.bits.probe_id := s2_req.id

  // If allocation fails, retry the snoop; otherwise the recovery entry wakes
  // its original ProbeQueue entry only after local eviction completes.
  val s2_probe_ecc_blocked = !s2_req.local_evict &&
    (s2_tag_ce_recovery && !io.tag_evict.ready || s2_data_ce_recovery && !io.data_evict.ready)
  io.probe_resp.valid := s2_fire && s2_req.probe &&
    (!s2_req.local_evict && (!s2_ecc_recovery || s2_probe_ecc_blocked) || s2_req.local_evict_probe)
  io.probe_resp.bits.id := s2_req.id
  io.probe_resp.bits.replay := s2_local_probe_retry || s2_probe_ecc_blocked

  io.data_evict.valid := s2_ecc_replay && s2_data_ce_recovery
  io.data_evict.bits.addr := get_block_addr(Cat(s2_tag, get_untag(s2_req.vaddr)))
  io.data_evict.bits.vaddr := get_block_addr(s2_req.vaddr)
  io.data_evict.bits.way_en := s2_way_en
  io.data_evict.bits.enc_tag := s2_selected_enc_tag
  io.data_evict.bits.clean_only := false.B
  // This bit is a retry marker for the original snoop.  The repair request
  // itself is always issued as a local eviction (see EccEvictEntry).
  io.data_evict.bits.probe := s2_req.probe
  io.data_evict.bits.probe_param := 0.U
  io.data_evict.bits.probe_need_data := false.B
  io.data_evict.bits.probe_id := s2_req.id

  def mergePutData(old_data: UInt, new_data: UInt, wmask: UInt): UInt = {
    val full_wmask = FillInterleaved(8, wmask)
    ((~full_wmask & old_data) | (full_wmask & new_data))
  }
  val s2_merge_mask = Wire(Vec(DCacheBanks, UInt(DCacheSRAMRowBytes.W)))
  val s2_store_data_merged_without_cache = Wire(Vec(DCacheBanks, UInt(DCacheSRAMRowBits.W)))
  for (i <- 0 until DCacheBanks) {
    val new_data = get_data_of_bank(i, Mux(s2_req.miss, io.refill_info.bits.store_data, s2_req.store_data))
    // for amo hit, we should use read out SRAM data
    // do not merge with store data
    s2_merge_mask(i) := Mux(s2_amo_hit, 0.U(wordBytes.W), get_mask_of_bank(i, Mux(s2_req.miss, io.refill_info.bits.store_mask, s2_req.store_mask)))
    s2_store_data_merged_without_cache(i) := mergePutData(0.U(DCacheSRAMRowBits.W), new_data, s2_merge_mask(i))
  }

  XSError(s2_valid && s2_can_go_to_s3 && s2_req.miss && !io.refill_info.valid, "MainPipe req can go to s3 but no refill data")

  // s3: write data, meta and tag
  val s3_valid = RegInit(false.B)
  val s3_req = RegEnable(s2_req, s2_valid_to_s3)
  val s3_miss_param = RegEnable(io.refill_info.bits.miss_param, s2_valid_to_s3)
  val s3_miss_dirty = RegEnable(io.refill_info.bits.miss_dirty, s2_fire_to_s3)
  val s3_tag = RegEnable(s2_tag, s2_valid_to_s3)
  val s3_tag_uncorrectable = RegEnable(s2_tag_uncorrectable, s2_valid_to_s3)
  val s3_tag_match = RegEnable(s2_tag_match, s2_fire_to_s3)
  val s3_coh = RegEnable(s2_coh, s2_fire_to_s3)
  val s3_hit = RegEnable(s2_hit, s2_fire_to_s3)
  val s3_amo_hit = RegEnable(s2_amo_hit, s2_fire_to_s3)
  val s3_store_hit = RegEnable(s2_store_hit, s2_fire_to_s3)
  val s3_hit_coh = RegEnable(s2_hit_coh, s2_fire_to_s3)
  val s3_new_hit_coh = RegEnable(s2_new_hit_coh, s2_fire_to_s3)
  val s3_way_en = RegEnable(s2_way_en, s2_fire_to_s3)
  val s3_banked_store_wmask = RegEnable(s2_banked_store_wmask, s2_fire_to_s3)
  val s3_idx = RegEnable(s2_idx, s2_fire_to_s3)
  val s3_store_data_merged_without_cache = RegEnable(s2_store_data_merged_without_cache, s2_fire_to_s3)
  val s3_merge_mask = RegEnable(VecInit(s2_merge_mask.map(~_)), s2_fire_to_s3)
  val s3_isPrefetch = !s3_req.replace && !s3_req.probe && !s3_req.miss && s3_req.isPrefetch
  val s3_data_error_mask = RegEnable(Mux(s2_may_report_data_error, s2_banked_rmask, 0.U), s2_fire_to_s3)
  val s3_data_correctable = VecInit(io.data_ecc.zipWithIndex.map { case (ecc, bank) =>
    ecc.correctable && s3_data_error_mask(bank)
  }).asUInt
  val s3_data_uncorrectable = VecInit(io.data_ecc.zipWithIndex.map { case (ecc, bank) =>
    ecc.uncorrectable && s3_data_error_mask(bank)
  }).asUInt
  val s3_data_ce = s3_data_correctable.orR && !s3_data_uncorrectable.orR
  val s3_raw_data = VecInit(io.data_resp.map(_.raw_data))
  // Probes correct outgoing data; ordinary accesses replay for local eviction.
  val s3_data = WireInit(VecInit((0 until DCacheBanks).map(i => {
    Mux(s3_req.probe && !s3_data_uncorrectable(i),
      eccCorrector.io.correctedData(i), s3_raw_data(i))
  })))
  val s3_store_data_merged = Wire(Vec(DCacheBanks, UInt(DCacheSRAMRowBits.W)))
  for (i <- 0 until DCacheBanks) {
    // for amo hit, we should use read out SRAM data
    // do not merge with store data
    s3_store_data_merged(i) := mergePutData(s3_store_data_merged_without_cache(i), s3_data(i), s3_merge_mask(i))
  }

  val s3_word_bank_base = wordBankBase(s3_req.word_idx)
  val s3_quad_word_bank_base = quadWordBankBase(s3_req.quad_word_idx)
  val s3_data_words = VecInit((0 until blockWords).map(i => {
    assembleBankData(
      s3_store_data_merged,
      wordBankBase(i.U(log2Up(blockWords).W)),
      DCacheWordBankCount
    )
  }))
  val s3_data_word = s3_data_words(s3_req.word_idx)
  val s3_data_quad_word = VecInit((0 until blockWords).map(i => {
    if (i == blockWords - 1) {
      Cat(0.U(DCacheWordBits.W), s3_data_words(i))
    } else {
      Cat(s3_data_words(i + 1), s3_data_words(i))
    }
  }))(s3_req.word_idx)
  val s3_amo_resp_data = s3_data_quad_word
  val s3_data_line = Cat((0 until DCacheBanks).reverse.map(i => s3_data(i)))

  val s3_refill_latency = RegEnable(s2_refill_latency, s2_fire_to_s3)
  val s3_sc_fail  = Wire(Bool()) // miss or lr mismatch
  val s3_need_replacement = RegEnable(s2_need_replacement && !s2_refill_tag_eq_way, s2_fire_to_s3)
  val s3_tag_error_for_amo = RegEnable(s2_tag_error, false.B, s2_fire_to_s3)

  val s3_local_probe_reply = s3_req.local_evict_probe && !RegEnable(s2_local_probe_retry, s2_fire_to_s3)
  val (_, probe_shrink_param, probe_new_coh) = s3_coh.onProbe(
    Mux(s3_req.local_evict_probe && !s3_local_probe_reply, TLPermissions.toN, s3_req.probe_param))
  val (_, miss_shrink_param, _) = s3_coh.onCacheControl(M_FLUSH)

  val miss_update_meta = s3_req.miss
  val probe_update_meta = s3_req.probe &&
    (s3_tag_match || s3_req.local_evict_probe && !s3_local_probe_reply) && s3_coh =/= probe_new_coh
  val s3_selected_tag_uec = (s3_tag_uncorrectable & s3_way_en).orR
  val s3_amo_uec = s3_valid && s3_req.isAMO && !s3_req.probe && !s3_req.miss &&
    (s3_data_uncorrectable.orR || s3_selected_tag_uec || s3_tag_error_for_amo)
  val store_update_meta = s3_req.isStore && !s3_req.probe && s3_hit_coh =/= s3_new_hit_coh
  val amo_update_meta = s3_req.isAMO && !s3_req.probe && s3_hit_coh =/= s3_new_hit_coh && !s3_sc_fail
  val amo_wait_amoalu = s3_req.isAMO && s3_req.cmd =/= M_XLR && s3_req.cmd =/= M_XSC && !isAMOCAS(s3_req.cmd)
  val update_meta = (miss_update_meta || probe_update_meta || store_update_meta || amo_update_meta) &&
    !s3_req.replace && !s3_amo_uec

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

  // report ecc error
  val s2_probe_tag_uec = Mux(s2_req.local_evict,
    (s2_tag_uncorrectable & s2_req.local_evict_way_en).orR,
    !s2_tag_match && s2_tag_uncorrectable.orR) && s2_need_tag
  val s2_report_tag_uec = Mux(s2_req.probe, s2_probe_tag_uec,
    s2_selected_tag_uec && s2_need_tag)
  val s3_tag_uec_event = RegEnable(s2_report_tag_uec, s2_fire)
  val s3_tag_error_wb = RegEnable(s2_report_tag_uec, false.B, s2_fire_to_s3)

  // data_error will be reported by data array 1 cycle after data read resp
  val s3_data_uec_event = s3_data_uncorrectable.orR && GatedValidRegNext(s2_fire_to_s3)
  val s3_data_error_wb = s3_data_uncorrectable.orR

  val s3_l2_error_beu = RegEnable(s2_l2_error, s2_fire)
  val s3_l2_error_wb = RegEnable(s2_l2_error, s2_fire_to_s3)
  val s3_flag_error_beu = RegEnable(s2_flag_error, s2_fire)
  val s3_flag_error_wb = RegEnable(s2_flag_error, s2_fire_to_s3)
  val s3_repl_flag_error_wb = RegEnable(s2_repl_flag_error, s2_fire_to_s3)

  // error signal for amo inst
  val s3_error_beu = RegEnable(s2_error, 0.U.asTypeOf(s2_error), s2_fire) || s3_data_uec_event
  val s3_error_wb = RegEnable(s2_error, 0.U.asTypeOf(s2_error), s2_fire_to_s3) || s3_data_error_wb
  val s3_error_paddr_beu = RegEnable(s2_req.addr, s2_fire)

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
  val debug_s3_sc_fail_addr_match = s3_sc && lrsc_addr === get_block_addr(s3_req.addr) && !lrsc_valid

  s3_sc_fail  := s3_sc && (!s3_lrsc_addr_match || !s3_hit)
  val s3_cas_fail = s3_cas && (FillInterleaved(8, s3_req.amo_mask) & (s3_req.amo_cmp ^ s3_amo_resp_data)) =/= 0.U

  val s3_can_do_amo = ((s3_req.miss && !s3_req.probe && s3_req.isAMO) || s3_amo_hit) && !s3_amo_uec
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
  }.elsewhen (lrsc_count > 0.U) {
    lrsc_count := lrsc_count - 1.U
  }


  io.lrsc_locked_block.valid := lrsc_valid
  io.lrsc_locked_block.bits  := lrsc_addr
  io.block_lr := GatedValidRegNext(lrsc_count > 0.U)

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


  val update_data = (s3_req.miss || s3_store_hit || s3_can_do_amo_write) &&
    !s3_amo_uec

  // generate write data
  // AMO hits
  val do_amoalu = amo_wait_amoalu && s3_valid && !s3_s_amoalu && !s3_amo_uec
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
    val wordPieceSel = (0 until DCacheWordBankCount).map { offset =>
      i.U === s3_word_bank_base + offset.U
    }
    val quadPieceSel = (0 until DCacheQuadWordBankCount).map { offset =>
      i.U === s3_quad_word_bank_base + offset.U
    }
    s3_amo_data_merged(i) := mergePutData(
      old_data,
      selectDataPiece(amoalu.io.out, wordPieceSel, DCacheWordBankCount),
      selectFullMask(wordPieceSel)
    )
    s3_sc_data_merged(i) := mergePutData(
      old_data,
      selectDataPiece(s3_req.amo_data, wordPieceSel, DCacheWordBankCount),
      selectMaskPiece(s3_req.amo_mask, wordPieceSel, DCacheWordBankCount)
    )
    s3_cas_data_merged(i) := mergePutData(
      old_data = old_data,
      new_data = Mux(
        isAMOCASQ(s3_req.cmd),
        selectDataPiece(s3_req.amo_data, quadPieceSel, DCacheQuadWordBankCount),
        selectDataPiece(s3_req.amo_data, wordPieceSel, DCacheWordBankCount)
      ),
      wmask = Mux(
        !s3_cas_fail,
        Mux(
          isAMOCASQ(s3_req.cmd),
          selectMaskPiece(s3_req.amo_mask, quadPieceSel, DCacheQuadWordBankCount),
          selectMaskPiece(s3_req.amo_mask, wordPieceSel, DCacheWordBankCount)
        ),
        0.U(DCacheSRAMRowBytes.W)
      )
    )
  }
  val s3_amo_data_merged_reg = RegEnable(s3_amo_data_merged, do_amoalu)
  val miss_wb = s3_req.miss && s3_need_replacement && s3_coh.state =/= ClientStates.Nothing
  val probe_wb = s3_req.probe && (!s3_req.local_evict || s3_local_probe_reply || s3_coh.state =/= ClientStates.Nothing)
  val replace_wb = s3_req.replace
  val need_wb = (miss_wb || probe_wb || replace_wb) && !s3_amo_uec

  val writeback_param = Mux(probe_wb, probe_shrink_param, miss_shrink_param)
  // clean-only is used for a clean data UEC, where L2 owns the authoritative
  // copy.  A dirty line still has to carry its data even if the request was
  // marked clean-only by an upstream recovery path.
  val writeback_poison = Mux(s3_need_replacement,
    s3_repl_flag_error_wb.asUInt.orR, s3_flag_error_wb.asUInt.orR)
  val discard_clean_data = s3_req.local_evict_clean_only && s3_coh.state =/= ClientStates.Dirty
  // TileLink corrupt belongs to data messages. Preserve persistent poison with
  // ReleaseData, while clean-only recovery discards its local bad copy.
  val writeback_data = !discard_clean_data && ((if (dcacheParameters.alwaysReleaseData) {
    s3_tag_match && s3_req.probe && s3_req.probe_need_data ||
      s3_coh === ClientStates.Dirty || (miss_wb || replace_wb) && s3_coh.state =/= ClientStates.Nothing
  } else {
    s3_tag_match && s3_req.probe && s3_req.probe_need_data || s3_coh === ClientStates.Dirty
  }) || writeback_poison && s3_coh.state =/= ClientStates.Nothing)

  val s3_probe_can_go = s3_req.probe &&
    (io.wb.ready || s3_req.local_evict && !probe_wb) && (io.meta_write.ready || !probe_update_meta)
  val s3_store_can_go = s3_req.source === STORE_SOURCE.U && !s3_req.probe &&
    (io.meta_write.ready || !store_update_meta) && (io.data_write.ready || !update_data) &&
    !s3_req.miss
  val s3_prefetch_can_go = s3_req.isPrefetch && !s3_req.replace && !s3_req.probe && !s3_req.miss &&
    (io.meta_write.ready || !update_meta) && (io.data_write.ready || !update_data)
  val s3_amo_can_go = (s3_amo_hit && !s3_amo_uec &&
    (io.meta_write.ready || !amo_update_meta) && (io.data_write.ready || !update_data) &&
    (s3_s_amoalu || !amo_wait_amoalu)) || s3_amo_uec || s3_sc_fail
  val s3_miss_can_go = s3_req.miss &&
    (io.meta_write.ready || !amo_update_meta) &&
    (io.data_write.ready || !update_data) &&
    (s3_s_amoalu || !amo_wait_amoalu) &&
    io.tag_write.ready &&
    io.wb.ready
  val s3_replace_nothing = s3_req.replace && s3_coh.state === ClientStates.Nothing
  val s3_replace_can_go = s3_req.replace && (s3_replace_nothing || io.wb.ready)
  val s3_can_go = s3_probe_can_go || s3_store_can_go || s3_amo_can_go ||
    s3_miss_can_go || s3_replace_can_go || s3_prefetch_can_go
  val s3_update_data_cango = s3_store_can_go || s3_amo_can_go || s3_miss_can_go // used to speed up data_write gen
  val s3_fire = s3_valid && s3_can_go
  when (s2_fire_to_s3) {
    s3_valid := true.B
  }.elsewhen (s3_fire) {
    s3_valid := false.B
  }
  when (do_amoalu) { s3_s_amoalu := true.B }
  when (s3_fire) { s3_s_amoalu := false.B }

  val new_coh = Mux(
    miss_update_meta,
    miss_new_coh,
    Mux(
      probe_update_meta,
      probe_new_coh,
      Mux(store_update_meta || amo_update_meta, s3_new_hit_coh, ClientMetadata.onReset)
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
          bankMaskFromBase(quadWordBankBase(s3_req.quad_word_idx), DCacheQuadWordBankCount),
          bankMaskFromBase(wordBankBase(s3_req.word_idx), DCacheWordBankCount)
        ),
        banked_none_wmask
      )
    )
  )
  assert(!(s3_valid && !s3_amo_uec && banked_wmask.orR && !update_data))

  for (i <- 0 until DCacheBanks) {
    io.data_write_dup(i).valid := s3_valid && s3_update_data_cango && update_data
    io.data_write_dup(i).bits.way_en := s3_way_en
    io.data_write_dup(i).bits.addr := s3_req.vaddr
  }

  s3_ready := !s3_valid || s3_can_go
  s3_s0_set_conflict := s3_valid && s3_idx === s0_idx && !s3_isPrefetch
  //assert(RegNext(!s3_valid || !(s3_req.source === STORE_SOURCE.U && !s3_req.probe) || s3_hit)) // miss store should never come to s3 ,fixed(reserve)

  io.meta_read.valid := req.valid
  io.meta_read.bits.idx := get_dcache_idx(s0_req.vaddr)
  io.meta_read.bits.way_en := Mux(s0_req.replace, s0_req.replace_way_en, ~0.U(nWays.W))

  io.tag_read.valid := req.valid && !s0_req.replace
  io.tag_read.bits.idx := get_dcache_idx(s0_req.vaddr)
  io.tag_read.bits.way_en := ~0.U(nWays.W)

  io.data_read_intend := s1_valid && s1_need_data
  io.data_readline.valid := s1_valid && s1_need_data
  io.data_readline.bits.rmask := s1_banked_rmask
  io.data_readline.bits.way_en := s1_way_en
  io.data_readline.bits.way := s1_way
  io.data_readline.bits.addr := s1_req.vaddr

  io.miss_req.valid := s2_valid && s2_can_go_to_mq
  val miss_req = io.miss_req.bits
  miss_req := DontCare
  miss_req.source := s2_req.source
  miss_req.pf_source := s2_req.pf_source
  miss_req.cmd := s2_req.cmd
  miss_req.addr := s2_req.addr
  miss_req.vaddr := s2_req.vaddr
  miss_req.vaddrAlias := s2_vaddr_alias
  miss_req.vaddrSet := s2_vaddr_set
  miss_req.store_data := s2_req.store_data
  miss_req.store_mask := s2_req.store_mask
  miss_req.word_idx := s2_req.word_idx
  miss_req.amo_data := s2_req.amo_data
  miss_req.amo_mask := s2_req.amo_mask
  miss_req.amo_cmp  := s2_req.amo_cmp
  miss_req.req_coh := s2_hit_coh
  miss_req.id := s2_req.id
  miss_req.cancel := s2_grow_perm_fail
  miss_req.isSpecMiss := false.B
  miss_req.pc := 0.U // MainPipe requests (Store Buffer writeback) don't have a single corresponding PC
  miss_req.full_overwrite := s2_req.isStore && s2_req.store_mask.andR
  miss_req.isBtoT := s2_grow_perm
  miss_req.occupy_way := s2_tag_ecc_match_way

  io.wbq_conflict_check.valid := s2_valid && s2_can_go_to_mq
  io.wbq_conflict_check.bits := s2_req.addr

  /**
    * `s2_req.isStore` includes miss requests from Sbuffer sent from MissQueue,
    * while `s2_isStore`` only requests from sbuffer.
    * In the case of `BtoT` fail, only requests from sbuffer are allowed to return replay response.
    */
  val s2_isStore = RegEnable(s1_isStore, s1_fire)
  val s2_isAMO = RegEnable(s1_isAMO, s1_fire)
  io.store_replay_resp.valid := s2_valid && (s2_can_go_to_mq && replay && s2_req.isStore ||
    s2_grow_perm_fail && s2_isStore ||
    s2_can_go_ecc_replay && s2_isStore)
  io.store_replay_resp.bits.data := DontCare
  io.store_replay_resp.bits.miss := true.B // miss/recovery replay
  io.store_replay_resp.bits.replay := true.B
  io.store_replay_resp.bits.id := s2_req.id

  val mshr_handled_store_miss = s2_valid && s2_can_go_to_mq && s2_req.isStore && !io.store_replay_resp.valid
  val mshr_handled_store_miss_s3 = RegNext(mshr_handled_store_miss)
  val mshr_handled_store_miss_id_s3 = RegEnable(s2_req.id, mshr_handled_store_miss)
  // If a store is miss and accepted by mshr, tell Sbuffer it is a "hit". Sbuffer releases the entry and mshr provides corresponding st-ld forwarding data.
  //                                      (1) real hit       (2) store miss and accepted by mshr
  io.store_hit_resp.valid := s3_valid && s3_store_can_go || mshr_handled_store_miss_s3
  io.store_hit_resp.bits.data := DontCare
  io.store_hit_resp.bits.miss := mshr_handled_store_miss_s3
  io.store_hit_resp.bits.replay := false.B
  io.store_hit_resp.bits.id := Mux(mshr_handled_store_miss_s3, mshr_handled_store_miss_id_s3, s3_req.id)

  val atomic_hit_resp = Wire(new MainPipeResp)
  atomic_hit_resp.source := s3_req.source
  atomic_hit_resp.data := Mux(s3_sc, s3_sc_fail.asUInt, s3_amo_resp_data)
  atomic_hit_resp.miss := false.B
  atomic_hit_resp.miss_id := s3_req.miss_id
  atomic_hit_resp.error := Mux(s3_req.miss && s3_need_replacement,
    s3_l2_error_wb.asUInt.orR, s3_error_wb)
  atomic_hit_resp.tl_error := s3_l2_error_wb
  atomic_hit_resp.replay := false.B
  atomic_hit_resp.ack_miss_queue := s3_req.miss
  atomic_hit_resp.id := lrsc_valid
  val atomic_replay_resp = Wire(new MainPipeResp)
  atomic_replay_resp.source := s2_req.source
  atomic_replay_resp.data := DontCare
  atomic_replay_resp.miss := true.B
  atomic_replay_resp.miss_id := DontCare
  atomic_replay_resp.error := false.B
  atomic_replay_resp.tl_error := 0.U.asTypeOf(new TLError())
  atomic_replay_resp.replay := true.B
  atomic_replay_resp.ack_miss_queue := false.B
  atomic_replay_resp.id := DontCare

  val atomic_replay_resp_valid = s2_valid && (s2_can_go_to_mq && replay ||
    s2_grow_perm_fail || s2_can_go_ecc_replay) && s2_req.isAMO && !s2_req.miss && !s2_req.probe
  val atomic_hit_resp_valid = s3_valid && (s3_amo_can_go || s3_miss_can_go && s3_req.isAMO)

  io.atomic_resp.valid := atomic_replay_resp_valid || atomic_hit_resp_valid
  io.atomic_resp.bits := Mux(atomic_replay_resp_valid, atomic_replay_resp, atomic_hit_resp)

  val total_prefetch = s2_fire && s2_isPrefetch
  val pf_late_in_cache = s2_fire && s2_hit && s2_isPrefetch

  io.prefetch_stat.total_prefetch := total_prefetch
  io.prefetch_stat.pf_late_in_cache := pf_late_in_cache
  io.prefetch_stat.pf_late_in_cache_source := s2_hit_prefetch
  io.prefetch_stat.nack_prefetch := s2_valid && s2_can_go_to_mq && !io.miss_req.ready && s2_isPrefetch
  io.prefetch_stat.pf_source := s2_req.pf_source
  io.prefetch_stat.hit_pf_in_cache := DontCare
  io.prefetch_stat.hit_source := DontCare

  io.prefetch_stat.demand_miss := DontCare
  io.prefetch_stat.pollution := DontCare

  // io.replace_resp.valid := s3_fire && s3_req.replace
  // io.replace_resp.bits := s3_req.miss_id

  io.meta_write.valid := s3_fire && update_meta
  io.meta_write.bits.idx := s3_idx
  io.meta_write.bits.way_en := s3_way_en
  io.meta_write.bits.meta.coh := new_coh

  // A partial store re-encodes merged data, so preserve any observed UEC in meta.
  val store_data_error = s3_store_hit && s3_data_error_wb
  io.error_flag_write.valid := s3_fire && (store_data_error || update_meta &&
    (s3_l2_error_wb.asUInt.orR || s3_req.miss || !new_coh.isValid()))
  io.error_flag_write.bits.idx := s3_idx
  io.error_flag_write.bits.way_en := s3_way_en
  io.error_flag_write.bits.error := Mux(
    update_meta && !new_coh.isValid(),
    0.U.asTypeOf(new TLError()),
    (s3_l2_error_wb.asUInt | Mux(s3_need_replacement, 0.U, s3_flag_error_wb.asUInt)).asTypeOf(new TLError())
  )
  when (store_data_error) { io.error_flag_write.bits.error.tl_corrupt := true.B }

  // if we use (prefetch_flag && meta =/= ClientStates.Nothing) for prefetch check
  // prefetch_flag_write can be omited
  io.prefetch_flag_write.valid := s3_fire && s3_req.miss
  io.prefetch_flag_write.bits.idx := s3_idx
  io.prefetch_flag_write.bits.way_en := s3_way_en
  io.prefetch_flag_write.bits.source := s3_req.pf_source

  io.latency_flag_write.valid := s3_fire && s3_req.miss
  io.latency_flag_write.bits.idx := s3_idx
  io.latency_flag_write.bits.way_en := s3_way_en
  io.latency_flag_write.bits.latency := s3_refill_latency

  // regenerate repl_way & repl_coh
  io.bloom_filter_query.set.valid := s2_fire_to_s3 && s2_req.miss && !isFromL1Prefetch(s2_repl_pf) && s2_repl_coh.isValid() && isFromL1Prefetch(s2_req.pf_source)
  io.bloom_filter_query.set.bits.addr := io.bloom_filter_query.set.bits.get_addr(Cat(s2_repl_tag, get_untag(s2_req.vaddr))) // the evict block address

  io.bloom_filter_query.clr.valid := s3_fire && isFromL1Prefetch(s3_req.pf_source)
  io.bloom_filter_query.clr.bits.addr := io.bloom_filter_query.clr.bits.get_addr(s3_req.addr)

  XSPerfAccumulate("prefetch_write_valid", s3_fire && s3_req.miss)
  XSPerfAccumulate("prefetch_write_valid_pf", io.prefetch_flag_write.valid && isFromL1Prefetch(s3_req.pf_source))
  XSPerfAccumulate("mainpipe_update_prefetchArray", io.prefetch_flag_write.valid)
  XSPerfAccumulate("mainpipe_s2_miss_req", s2_valid && s2_req.miss)
  XSPerfAccumulate("mainpipe_s2_block_penalty", s2_valid && s2_req.miss && !io.refill_info.valid)
  XSPerfAccumulate("mainpipe_s2_missqueue_replay", s2_valid && s2_can_go_to_mq_replay)
  XSPerfAccumulate("mainpipe_slot_conflict_1_2", (s1_idx === s2_idx && s1_way_en === s2_way_en && s1_req.miss && s2_req.miss && s1_valid && s2_valid ))
  XSPerfAccumulate("mainpipe_slot_conflict_1_3", (s1_idx === s3_idx && s1_way_en === s3_way_en && s1_req.miss && s3_req.miss && s1_valid && s3_valid))
  XSPerfAccumulate("mainpipe_slot_conflict_2_3", (s2_idx === s3_idx && s2_way_en === s3_way_en && s2_req.miss && s3_req.miss && s2_valid && s3_valid))
  // probe / replace will not update access bit
  io.access_flag_write.valid := s3_fire && !s3_req.probe && !s3_req.replace && !s3_amo_uec
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

  // Only a committed, readable installation supplies a RESIDENT record.
  io.misstrack_install.valid := io.tag_write.fire && io.meta_write.fire &&
    new_coh.isValid() && !s3_l2_error_wb.asUInt.orR && !s3_error_wb
  io.misstrack_install.bits.vaddr := s3_req.vaddr
  io.misstrack_install.bits.paddr := s3_req.addr
  io.misstrack_install.bits.way := s3_way_en

  io.tag_write_intend := s3_req.miss && s3_valid
  XSPerfAccumulate("fake_tag_write_intend", io.tag_write_intend && !io.tag_write.valid)
  XSPerfAccumulate("mainpipe_tag_write", io.tag_write.valid)

  io.replace.req.valid := s2_valid && s2_need_eviction && !s2_refill_tag_eq_way && !s2_ecc_recovery
  io.replace.req.bits.addr := get_block_addr(Cat(s2_tag, get_untag(s2_req.vaddr)))
  io.replace.req.bits.vaddr := s2_req.vaddr

  io.evict_set := addr_to_dcache_set(s2_req.vaddr) // only use set index

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

  io.wb.bits.addr := get_block_addr(Cat(s3_tag, get_untag(s3_req.vaddr)))
  io.evict_done.valid := s3_fire && s3_req.local_evict && (!io.wb.valid || s3_local_probe_reply)
  io.evict_done.bits := s3_req.local_evict_tag
  io.wb.bits.param := writeback_param
  io.wb.bits.voluntary := s3_req.miss || s3_req.replace || s3_req.local_evict && !s3_local_probe_reply
  io.wb.bits.hasData := writeback_data && !s3_tag_error_wb
  // Tag UEC invalidates the victim address; a denied no-data Release must not
  // advertise a dirty payload that WBQ would require to carry data.
  io.wb.bits.dirty := s3_coh === ClientStates.Dirty && !s3_tag_error_wb
  io.wb.bits.local_evict := s3_req.local_evict && !s3_local_probe_reply
  io.wb.bits.local_evict_tag := s3_req.local_evict_tag
  io.wb.bits.local_evict_addr := get_block_addr(s3_req.addr)
  io.wb.bits.data := s3_data_line
  io.wb.bits.corrupt := s3_tag_error_wb ||
    io.wb.bits.hasData && (s3_data_error_wb || writeback_poison)
  io.wb.bits.delay_release := s3_req.replace
  io.wb.bits.miss_id := s3_req.miss_id

  // update plru in main pipe s3
  io.replace_access.valid := GatedValidRegNext(s2_fire_to_s3) && !s3_amo_uec && !s3_req.probe &&
    (s3_req.miss || ((s3_req.isAMO || s3_req.isStore) && s3_hit))
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
  io.status.s0_set.bits := get_dcache_idx(s0_req.vaddr)
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
    s.s1.bits.set := RegEnable(get_dcache_idx(s0_req.vaddr), s0_fire)
    s.s1.bits.way_en := s1_way_en
    s.s2.valid := s2_valid && !RegEnable(s1_req.replace, s1_fire)
    s.s2.bits.set := RegEnable(get_dcache_idx(s1_req.vaddr), s1_fire)
    s.s2.bits.way_en := s2_way_en
    s.s3.valid := s3_valid && !RegEnable(s2_req.replace, s2_fire_to_s3)
    s.s3.bits.set := RegEnable(get_dcache_idx(s2_req.vaddr), s2_fire_to_s3)
    s.s3.bits.way_en := RegEnable(s2_way_en, s2_fire_to_s3)
  }
  dontTouch(io.status_dup)

  io.mainpipe_info.s2_valid := s2_valid && s2_req.miss
  io.mainpipe_info.s2_miss_id := s2_req.miss_id
  io.mainpipe_info.s2_replay_to_mq := (s2_can_go_to_mq_no_data && !s2_ecc_recovery) || s2_ecc_replay
  io.mainpipe_info.s2_evict_BtoT_way := s2_can_go_to_mq_evict_fail && !s2_ecc_recovery
  io.mainpipe_info.s2_next_evict_way := PriorityEncoderOH(~io.btot_ways_for_set)
  io.mainpipe_info.s3_valid := s3_valid
  io.mainpipe_info.s3_miss_id := s3_req.miss_id
  io.mainpipe_info.s3_refill_resp := s3_fire && s3_req.miss
  XSError(s2_valid && s2_way_en.andR, "s2_way_en should not be all 1")

  // Report the error to RERI and the cache-error CSR one cycle after the data response.
  io.error := 0.U.asTypeOf(new RERIErrorInfo)
  // CE is reported when a repaired local eviction enters writeback. MainPipe is
  // the sole DCache source of RERI events; loads report failures architecturally.
  val s3_local_tag_ce = RegEnable((s2_tag_correctable & s2_way_en).orR, s2_fire_to_s3)
  val s3_local_ce_event = s3_fire && io.wb.fire && s3_req.local_evict && s3_tag_match &&
    (s3_local_tag_ce || s3_data_ce) && !s3_selected_tag_uec && !s3_data_uncorrectable.orR

  // LoadPipe handles clean-data UEC recovery locally and does not report it to RERI.
  val s3_uec_event = s3_error_beu && GatedValidRegNext(s2_fire &&
    !s2_should_not_report_ecc_error && !(s2_req.local_evict && s2_req.local_evict_clean_only))
  assert(!(s3_uec_event && s3_local_ce_event))
  io.error.errorPaddr.valid := s3_uec_event || s3_local_ce_event
  io.error.errorPaddr.bits := Mux(s3_local_ce_event,
    get_block_addr(Cat(s3_tag, get_untag(s3_req.vaddr))), s3_error_paddr_beu)
  io.error.severity := Mux(s3_uec_event, SeverityLevel.UEC,
    Mux(s3_local_ce_event, SeverityLevel.CE, SeverityLevel.NONE))
  val s3_address_error = Mux(s3_local_ce_event, s3_local_tag_ce, s3_tag_uec_event)
  io.error.errorSource := Mux(s3_address_error,
    ErrorSource.addressError, ErrorSource.dataError)

  val perfEvents = Seq(
    ("l1D_write_dcache_access", s2_fire && (s2_isStore || (s2_isAMO && isWrite(s2_req.cmd)))), // store_req (cacheline evited from Sbuffer to L1D) & amo write
    ("l1D_write_dcache_miss  ", s2_fire && (s2_isStore || (s2_isAMO && isWrite(s2_req.cmd)) && !s2_hit)),
    ("dcache_mp_req          ", s0_fire                                                      ),
    ("dcache_mp_total_penalty", PopCount(VecInit(Seq(s0_fire, s1_valid, s2_valid, s3_valid)))),
    ("s2_hw_pf_access", s2_fire && s2_isPrefetch),
    ("s2_hw_pf_miss", s2_fire && s2_isPrefetch && !s2_hit)
  )
  generatePerfEvent()
}
