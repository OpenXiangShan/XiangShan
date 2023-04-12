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
import freechips.rocketchip.tilelink.ClientStates._
import freechips.rocketchip.tilelink.MemoryOpCategories._
import freechips.rocketchip.tilelink.TLPermissions._
import freechips.rocketchip.tilelink.{ClientMetadata, ClientStates, TLPermissions}
import utils._
import utility._
import xiangshan.L1CacheErrorInfo

class MainPipeReq(implicit p: Parameters) extends DCacheBundle {
  val miss = Bool() // only amo miss will refill in main pipe
  val miss_id = UInt(log2Up(cfg.nMissEntries).W)
  val miss_param = UInt(TLPermissions.bdWidth.W)
  val miss_dirty = Bool()
  val miss_way_en = UInt(DCacheWays.W)

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
  val amo_data   = UInt(DataBits.W)
  val amo_mask   = UInt((DataBits / 8).W)

  // error
  val error = Bool()

  // replace
  val replace = Bool()
  val replace_way_en = UInt(DCacheWays.W)

  val id = UInt(reqIdWidth.W)

  def isLoad: Bool = source === LOAD_SOURCE.U
  def isStore: Bool = source === STORE_SOURCE.U
  def isAMO: Bool = source === AMO_SOURCE.U

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

class MainPipe(implicit p: Parameters) extends DCacheModule with HasPerfEvents {
  val io = IO(new Bundle() {
    // probe queue
    val probe_req = Flipped(DecoupledIO(new MainPipeReq))
    // store miss go to miss queue
    val miss_req = DecoupledIO(new MissReq)
    val miss_resp = Input(new MissResp) // miss resp is used to support plru update
    // store buffer
    val store_req = Flipped(DecoupledIO(new DCacheLineReq))
    val store_replay_resp = ValidIO(new DCacheLineResp)
    val store_hit_resp = ValidIO(new DCacheLineResp)
    val release_update = ValidIO(new ReleaseUpdate)
    // atmoics
    val atomic_req = Flipped(DecoupledIO(new MainPipeReq))
    val atomic_resp = ValidIO(new AtomicsResp)
    // replace
    val replace_req = Flipped(DecoupledIO(new MainPipeReq))
    val replace_resp = ValidIO(UInt(log2Up(cfg.nMissEntries).W))
    // write-back queue
    val wb = DecoupledIO(new WritebackReq)
    val wb_ready_dup = Vec(nDupWbReady, Input(Bool()))
    val probe_ttob_check_req = ValidIO(new ProbeToBCheckReq)
    val probe_ttob_check_resp = Flipped(ValidIO(new ProbeToBCheckResp))

    // data sram
    val data_read_intend = Output(Bool())
    val data_read = DecoupledIO(new L1BankedDataReadLineReq)
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
    val prefetch_flag_write = DecoupledIO(new FlagMetaWriteReq)
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
    val error = Output(new L1CacheErrorInfo())
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
  val store_req = Wire(DecoupledIO(new MainPipeReq))
  store_req.bits := (new MainPipeReq).convertStoreReq(io.store_req.bits)
  store_req.valid := io.store_req.valid
  io.store_req.ready := store_req.ready

  // s0: read meta and tag
  val req = Wire(DecoupledIO(new MainPipeReq))
  arbiter(
    in = Seq(
      io.probe_req,
      io.replace_req,
      store_req, // Note: store_req.ready is now manually assigned for better timing
      io.atomic_req
    ),
    out = req,
    name = Some("main_pipe_req")
  )

  val store_idx = get_idx(io.store_req.bits.vaddr)
  // manually assign store_req.ready for better timing
  // now store_req set conflict check is done in parallel with req arbiter
  store_req.ready := io.meta_read.ready && io.tag_read.ready && s1_ready && !store_set_conflict &&
    !io.probe_req.valid && !io.replace_req.valid
  val s0_req = req.bits
  val s0_idx = get_idx(s0_req.vaddr)
  val s0_need_tag = io.tag_read.valid
  val s0_can_go = io.meta_read.ready && io.tag_read.ready && s1_ready && !set_conflict
  val s0_fire = req.valid && s0_can_go

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
  val s1_can_go = s2_ready && (io.data_read.ready || !s1_need_data)
  val s1_fire = s1_valid && s1_can_go
  val s1_idx = get_idx(s1_req.vaddr)

  // duplicate regs to reduce fanout
  val s1_valid_dup = RegInit(VecInit(Seq.fill(6)(false.B)))
  val s1_req_vaddr_dup_for_data_read = RegEnable(s0_req.vaddr, s0_fire)
  val s1_idx_dup_for_replace_way = RegEnable(get_idx(s0_req.vaddr), s0_fire)
  val s1_dmWay_dup_for_replace_way = RegEnable(get_direct_map_way(s0_req.vaddr), s0_fire)

  val s1_valid_dup_for_status = RegInit(VecInit(Seq.fill(nDupStatus)(false.B)))

  when (s0_fire) {
    s1_valid := true.B
    s1_valid_dup.foreach(_ := true.B)
    s1_valid_dup_for_status.foreach(_ := true.B)
  }.elsewhen (s1_fire) {
    s1_valid := false.B
    s1_valid_dup.foreach(_ := false.B)
    s1_valid_dup_for_status.foreach(_ := false.B)
  }
  s1_ready := !s1_valid_dup(0) || s1_can_go
  s1_s0_set_conflict := s1_valid_dup(1) && s0_idx === s1_idx
  s1_s0_set_conflict_store := s1_valid_dup(2) && store_idx === s1_idx

  val meta_resp = Wire(Vec(nWays, (new Meta).asUInt()))
  val tag_resp = Wire(Vec(nWays, UInt(tagBits.W)))
  val ecc_resp = Wire(Vec(nWays, UInt(eccTagBits.W)))
  meta_resp := Mux(RegNext(s0_fire), VecInit(io.meta_resp.map(_.asUInt)), RegNext(meta_resp))
  tag_resp := Mux(RegNext(s0_fire), VecInit(io.tag_resp.map(r => r(tagBits - 1, 0))), RegNext(tag_resp))
  ecc_resp := Mux(RegNext(s0_fire), VecInit(io.tag_resp.map(r => r(encTagBits - 1, tagBits))), RegNext(ecc_resp))
  val enc_tag_resp = Wire(io.tag_resp.cloneType)
  enc_tag_resp := Mux(RegNext(s0_fire), io.tag_resp, RegNext(enc_tag_resp))

  def wayMap[T <: Data](f: Int => T) = VecInit((0 until nWays).map(f))
  val s1_tag_eq_way = wayMap((w: Int) => tag_resp(w) === get_tag(s1_req.addr)).asUInt
  val s1_tag_match_way = wayMap((w: Int) => s1_tag_eq_way(w) && Meta(meta_resp(w)).coh.isValid()).asUInt
  val s1_tag_match = s1_tag_match_way.orR

  val s1_hit_tag = Mux(s1_tag_match, Mux1H(s1_tag_match_way, wayMap(w => tag_resp(w))), get_tag(s1_req.addr))
  val s1_hit_coh = ClientMetadata(Mux(s1_tag_match, Mux1H(s1_tag_match_way, wayMap(w => meta_resp(w))), 0.U))
  val s1_encTag = Mux1H(s1_tag_match_way, wayMap((w: Int) => enc_tag_resp(w)))
  val s1_flag_error = Mux(s1_tag_match, Mux1H(s1_tag_match_way, wayMap(w => io.extra_meta_resp(w).error)), false.B)
  val s1_extra_meta = Mux1H(s1_tag_match_way, wayMap(w => io.extra_meta_resp(w)))
  val s1_l2_error = s1_req.error

  XSPerfAccumulate("probe_unused_prefetch", s1_req.probe && s1_extra_meta.prefetch && !s1_extra_meta.access) // may not be accurate
  XSPerfAccumulate("replace_unused_prefetch", s1_req.replace && s1_extra_meta.prefetch && !s1_extra_meta.access) // may not be accurate

  // replacement policy
  val s1_repl_way_en = WireInit(0.U(nWays.W))
  s1_repl_way_en := Mux(RegNext(s0_fire), UIntToOH(io.replace_way.way), RegNext(s1_repl_way_en))
  val s1_repl_tag = Mux1H(s1_repl_way_en, wayMap(w => tag_resp(w)))
  val s1_repl_coh = Mux1H(s1_repl_way_en, wayMap(w => meta_resp(w))).asTypeOf(new ClientMetadata)
  val s1_miss_tag = Mux1H(s1_req.miss_way_en, wayMap(w => tag_resp(w)))
  val s1_miss_coh = Mux1H(s1_req.miss_way_en, wayMap(w => meta_resp(w))).asTypeOf(new ClientMetadata)

  val s1_repl_way_raw = WireInit(0.U(log2Up(nWays).W))
  s1_repl_way_raw := Mux(RegNext(s0_fire), io.replace_way.way, RegNext(s1_repl_way_raw))

  val s1_need_replacement = (s1_req.miss || s1_req.isStore && !s1_req.probe) && !s1_tag_match
  val s1_way_en = Mux(
    s1_req.replace,
    s1_req.replace_way_en,
    Mux(
      s1_req.miss,
      s1_req.miss_way_en,
      Mux(
        s1_need_replacement,
        s1_repl_way_en,
        s1_tag_match_way
      )
    )
  )
  assert(!RegNext(s1_fire && PopCount(s1_way_en) > 1.U))
  val s1_tag = Mux(
    s1_req.replace,
    get_tag(s1_req.addr),
    Mux(
      s1_req.miss,
      s1_miss_tag,
      Mux(s1_need_replacement, s1_repl_tag, s1_hit_tag)
    )
  )
  val s1_coh = Mux(
    s1_req.replace,
    Mux1H(s1_req.replace_way_en, meta_resp.map(ClientMetadata(_))),
    Mux(
      s1_req.miss,
      s1_miss_coh,
      Mux(s1_need_replacement, s1_repl_coh, s1_hit_coh)
    )
  )

  val s1_has_permission = s1_hit_coh.onAccess(s1_req.cmd)._1
  val s1_hit = s1_tag_match && s1_has_permission
  val s1_pregen_can_go_to_mq = !s1_req.replace && !s1_req.probe && !s1_req.miss && (s1_req.isStore || s1_req.isAMO) && !s1_hit

  val s1_ttob_probe = s1_valid && s1_req.probe && s1_req.probe_param === TLPermissions.toB
  io.probe_ttob_check_req.valid := s1_ttob_probe
  io.probe_ttob_check_req.bits.addr := get_block_addr(Cat(s1_tag, get_untag(s1_req.vaddr)))

  // s2: select data, return resp if this is a store miss
  val s2_valid = RegInit(false.B)
  val s2_req = RegEnable(s1_req, s1_fire)
  val s2_tag_match = RegEnable(s1_tag_match, s1_fire)
  val s2_tag_match_way = RegEnable(s1_tag_match_way, s1_fire)
  val s2_hit_coh = RegEnable(s1_hit_coh, s1_fire)
  val (s2_has_permission, _, s2_new_hit_coh) = s2_hit_coh.onAccess(s2_req.cmd)

  val s2_repl_tag = RegEnable(s1_repl_tag, s1_fire)
  val s2_repl_coh = RegEnable(s1_repl_coh, s1_fire)
  val s2_repl_way_en = RegEnable(s1_repl_way_en, s1_fire)
  val s2_need_replacement = RegEnable(s1_need_replacement, s1_fire)
  val s2_need_data = RegEnable(s1_need_data, s1_fire)
  val s2_need_tag = RegEnable(s1_need_tag, s1_fire)
  val s2_encTag = RegEnable(s1_encTag, s1_fire)
  val s2_idx = get_idx(s2_req.vaddr)

  // duplicate regs to reduce fanout
  val s2_valid_dup = RegInit(VecInit(Seq.fill(8)(false.B)))
  val s2_valid_dup_for_status = RegInit(VecInit(Seq.fill(nDupStatus)(false.B)))
  val s2_req_vaddr_dup_for_miss_req = RegEnable(s1_req.vaddr, s1_fire)
  val s2_idx_dup_for_status = RegEnable(get_idx(s1_req.vaddr), s1_fire)
  val s2_idx_dup_for_replace_access = RegEnable(get_idx(s1_req.vaddr), s1_fire)

  val s2_req_replace_dup_1,
      s2_req_replace_dup_2 = RegEnable(s1_req.replace, s1_fire)
  
  val s2_can_go_to_mq_dup = (0 until 3).map(_ => RegEnable(s1_pregen_can_go_to_mq, s1_fire))

  val s2_way_en = RegEnable(s1_way_en, s1_fire)
  val s2_tag = RegEnable(s1_tag, s1_fire)
  val s2_coh = RegEnable(s1_coh, s1_fire)
  val s2_banked_store_wmask = RegEnable(s1_banked_store_wmask, s1_fire)
  val s2_flag_error = RegEnable(s1_flag_error, s1_fire)
  val s2_tag_error = dcacheParameters.tagCode.decode(s2_encTag).error && s2_need_tag
  val s2_l2_error = s2_req.error
  val s2_error = s2_flag_error || s2_tag_error || s2_l2_error // data_error not included

  val s2_may_report_data_error = s2_need_data && s2_coh.state =/= ClientStates.Nothing

  val s2_hit = s2_tag_match && s2_has_permission
  val s2_amo_hit = s2_hit && !s2_req.probe && !s2_req.miss && s2_req.isAMO
  val s2_store_hit = s2_hit && !s2_req.probe && !s2_req.miss && s2_req.isStore

  s2_s0_set_conlict := s2_valid_dup(0) && s0_idx === s2_idx
  s2_s0_set_conlict_store := s2_valid_dup(1) && store_idx === s2_idx

  // For a store req, it either hits and goes to s3, or miss and enter miss queue immediately
  val s2_can_go_to_s3 = (s2_req_replace_dup_1 || s2_req.probe || s2_req.miss || (s2_req.isStore || s2_req.isAMO) && s2_hit) && s3_ready
  val s2_can_go_to_mq = RegEnable(s1_pregen_can_go_to_mq, s1_fire)
  assert(RegNext(!(s2_valid && s2_can_go_to_s3 && s2_can_go_to_mq)))
  val s2_can_go = s2_can_go_to_s3 || s2_can_go_to_mq
  val s2_fire = s2_valid && s2_can_go
  val s2_fire_to_s3 = s2_valid_dup(2) && s2_can_go_to_s3
  when (s1_fire) {
    s2_valid := true.B
    s2_valid_dup.foreach(_ := true.B)
    s2_valid_dup_for_status.foreach(_ := true.B)
  }.elsewhen (s2_fire) {
    s2_valid := false.B
    s2_valid_dup.foreach(_ := false.B)
    s2_valid_dup_for_status.foreach(_ := false.B)
  }
  s2_ready := !s2_valid_dup(3) || s2_can_go
  val replay = !io.miss_req.ready

  val data_resp = Wire(io.data_resp.cloneType)
  data_resp := Mux(RegNext(s1_fire), io.data_resp, RegNext(data_resp))
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
    val new_data = get_data_of_bank(i, s2_req.store_data)
    // for amo hit, we should use read out SRAM data
    // do not merge with store data
    val wmask = Mux(s2_amo_hit, 0.U(wordBytes.W), get_mask_of_bank(i, s2_req.store_mask))
    s2_store_data_merged(i) := mergePutData(old_data, new_data, wmask)
  }

  val s2_data_word = s2_store_data_merged(s2_req.word_idx)

  val s2_probe_ttob_check_resp = Wire(io.probe_ttob_check_resp.cloneType)
  s2_probe_ttob_check_resp := Mux(RegNext(s1_fire), io.probe_ttob_check_resp, RegNext(s2_probe_ttob_check_resp))

  // s3: write data, meta and tag
  val s3_valid = RegInit(false.B)
  val s3_req = RegEnable(s2_req, s2_fire_to_s3)
  // val s3_idx = get_idx(s3_req.vaddr)
  val s3_tag = RegEnable(s2_tag, s2_fire_to_s3)
  val s3_tag_match = RegEnable(s2_tag_match, s2_fire_to_s3)
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
  val s3_data = RegEnable(s2_data, s2_fire_to_s3)
  val s3_l2_error = s3_req.error
  // data_error will be reported by data array 1 cycle after data read resp
  val s3_data_error = Wire(Bool())
  s3_data_error := Mux(RegNext(RegNext(s1_fire)), // ecc check result is generated 2 cycle after read req
    io.readline_error_delayed && RegNext(s2_may_report_data_error), 
    RegNext(s3_data_error) // do not update s3_data_error if !s1_fire
  )
  // error signal for amo inst
  // s3_error = s3_flag_error || s3_tag_error || s3_l2_error || s3_data_error
  val s3_error = RegEnable(s2_error, s2_fire_to_s3) || s3_data_error
  val (_, _, probe_new_coh) = s3_coh.onProbe(s3_req.probe_param)
  val s3_need_replacement = RegEnable(s2_need_replacement, s2_fire_to_s3)
  val s3_probe_ttob_check_resp = RegEnable(s2_probe_ttob_check_resp, s2_fire_to_s3)

  // duplicate regs to reduce fanout
  val s3_valid_dup = RegInit(VecInit(Seq.fill(14)(false.B)))
  val s3_valid_dup_for_status = RegInit(VecInit(Seq.fill(nDupStatus)(false.B)))
  val s3_way_en_dup = (0 until 4).map(_ => RegEnable(s2_way_en, s2_fire_to_s3))
  val s3_coh_dup = (0 until 6).map(_ => RegEnable(s2_coh, s2_fire_to_s3))
  val s3_tag_match_dup = RegEnable(s2_tag_match, s2_fire_to_s3)

  val s3_req_vaddr_dup_for_wb,
      s3_req_vaddr_dup_for_data_write = RegEnable(s2_req.vaddr, s2_fire_to_s3)
  
  val s3_idx_dup = (0 until 6).map(_ => RegEnable(get_idx(s2_req.vaddr), s2_fire_to_s3))

  val s3_req_replace_dup = (0 until 8).map(_ => RegEnable(s2_req.replace, s2_fire_to_s3))    
  val s3_req_cmd_dup = (0 until 6).map(_ => RegEnable(s2_req.cmd, s2_fire_to_s3))
  val s3_req_source_dup_1, s3_req_source_dup_2 = RegEnable(s2_req.source, s2_fire_to_s3)
  val s3_req_addr_dup = (0 until 5).map(_ => RegEnable(s2_req.addr, s2_fire_to_s3))
  val s3_req_probe_dup = (0 until 10).map(_ => RegEnable(s2_req.probe, s2_fire_to_s3))
  val s3_req_miss_dup = (0 until 10).map(_ => RegEnable(s2_req.miss, s2_fire_to_s3))
  val s3_req_word_idx_dup = (0 until DCacheBanks).map(_ => RegEnable(s2_req.word_idx, s2_fire_to_s3))

  val s3_need_replacement_dup = RegEnable(s2_need_replacement, s2_fire_to_s3)

  val s3_s_amoalu_dup = RegInit(VecInit(Seq.fill(3)(false.B)))

  val s3_hit_coh_dup = RegEnable(s2_hit_coh, s2_fire_to_s3)
  val s3_new_hit_coh_dup = (0 until 2).map(_ => RegEnable(s2_new_hit_coh, s2_fire_to_s3))
  val s3_amo_hit_dup = RegEnable(s2_amo_hit, s2_fire_to_s3)
  val s3_store_hit_dup = (0 until 2).map(_ => RegEnable(s2_store_hit, s2_fire_to_s3))

  val lrsc_count_dup = RegInit(VecInit(Seq.fill(3)(0.U(log2Ceil(LRSCCycles).W))))
  val lrsc_valid_dup = lrsc_count_dup.map { case cnt => cnt > LRSCBackOff.U }
  val lrsc_addr_dup = Reg(UInt())

  val s3_req_probe_param_dup = RegEnable(s2_req.probe_param, s2_fire_to_s3)
  val (_, probe_shrink_param, _) = s3_coh.onProbe(s3_req_probe_param_dup)


  val miss_update_meta = s3_req.miss
  val probe_update_meta = s3_req_probe_dup(0) && s3_tag_match_dup && s3_coh_dup(0) =/= probe_new_coh
  val store_update_meta = s3_req.isStore && !s3_req_probe_dup(1) && s3_hit_coh =/= s3_new_hit_coh_dup(0)
  val amo_update_meta = s3_req.isAMO && !s3_req_probe_dup(2) && s3_hit_coh_dup =/= s3_new_hit_coh_dup(1)
  val amo_wait_amoalu = s3_req.isAMO && s3_req_cmd_dup(0) =/= M_XLR && s3_req_cmd_dup(1) =/= M_XSC
  val update_meta = (miss_update_meta || probe_update_meta || store_update_meta || amo_update_meta) && !s3_req_replace_dup(0)

  def missCohGen(cmd: UInt, param: UInt, dirty: Bool) = {
    val c = categorize(cmd)
    MuxLookup(Cat(c, param, dirty), Nothing, Seq(
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
  val miss_new_coh = ClientMetadata(missCohGen(s3_req_cmd_dup(2), s3_req.miss_param, s3_req.miss_dirty))

  // LR, SC and AMO
  val debug_sc_fail_addr = RegInit(0.U)
  val debug_sc_fail_cnt  = RegInit(0.U(8.W))
  val debug_sc_addr_match_fail_cnt  = RegInit(0.U(8.W))

  val lrsc_count = RegInit(0.U(log2Ceil(LRSCCycles).W))
  // val lrsc_valid = lrsc_count > LRSCBackOff.U
  val lrsc_addr  = Reg(UInt())
  val s3_lr = !s3_req_probe_dup(3) && s3_req.isAMO && s3_req_cmd_dup(3) === M_XLR
  val s3_sc = !s3_req_probe_dup(4) && s3_req.isAMO && s3_req_cmd_dup(4) === M_XSC
  val s3_lrsc_addr_match = lrsc_valid_dup(0) && lrsc_addr === get_block_addr(s3_req.addr)
  val s3_sc_fail = s3_sc && !s3_lrsc_addr_match
  val debug_s3_sc_fail_addr_match = s3_sc && lrsc_addr === get_block_addr(s3_req.addr) && !lrsc_valid_dup(0)
  val s3_sc_resp = Mux(s3_sc_fail, 1.U, 0.U)

  val s3_can_do_amo = (s3_req_miss_dup(0) && !s3_req_probe_dup(5) && s3_req.isAMO) || s3_amo_hit
  val s3_can_do_amo_write = s3_can_do_amo && isWrite(s3_req_cmd_dup(5)) && !s3_sc_fail

  when (s3_valid_dup(0) && (s3_lr || s3_sc)) {
    when (s3_can_do_amo && s3_lr) {
      lrsc_count := (LRSCCycles - 1).U
      lrsc_count_dup.foreach(_ := (LRSCCycles - 1).U)
      lrsc_addr := get_block_addr(s3_req_addr_dup(0))
      lrsc_addr_dup := get_block_addr(s3_req_addr_dup(0))
    } .otherwise {
      lrsc_count := 0.U
      lrsc_count_dup.foreach(_ := 0.U)
    }
  }.elsewhen (io.invalid_resv_set) {
    // when we release this block,
    // we invalidate this reservation set
    lrsc_count := 0.U
    lrsc_count_dup.foreach(_ := 0.U)
  }.elsewhen (lrsc_count > 0.U) {
    lrsc_count := lrsc_count - 1.U
    lrsc_count_dup.foreach({case cnt =>
      cnt := cnt - 1.U
    })
  }

  io.lrsc_locked_block.valid := lrsc_valid_dup(1)
  io.lrsc_locked_block.bits  := lrsc_addr_dup
  io.block_lr := RegNext(lrsc_count > 0.U)

  // When we update update_resv_set, block all probe req in the next cycle
  // It should give Probe reservation set addr compare an independent cycle,
  // which will lead to better timing
  io.update_resv_set := s3_valid_dup(1) && s3_lr && s3_can_do_amo

  when (s3_valid_dup(2)) {
    when (s3_req_addr_dup(1) === debug_sc_fail_addr) {
      when (s3_sc_fail) {
        debug_sc_fail_cnt := debug_sc_fail_cnt + 1.U
      } .elsewhen (s3_sc) {
        debug_sc_fail_cnt := 0.U
      }
    } .otherwise {
      when (s3_sc_fail) {
        debug_sc_fail_addr := s3_req_addr_dup(2)
        debug_sc_fail_cnt  := 1.U
        XSWarn(s3_sc_fail === 100.U, p"L1DCache failed too many SCs in a row 0x${Hexadecimal(debug_sc_fail_addr)}, check if sth went wrong\n")
      }
    }
  }
  XSWarn(debug_sc_fail_cnt > 100.U, "L1DCache failed too many SCs in a row")

  when (s3_valid_dup(2)) {
    when (s3_req_addr_dup(1) === debug_sc_fail_addr) {
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
  val update_data = s3_req_miss_dup(2) || s3_store_hit_dup(0) || s3_can_do_amo_write

  // generate write data
  // AMO hits
  val s3_s_amoalu = RegInit(false.B)
  val do_amoalu = amo_wait_amoalu && s3_valid_dup(3) && !s3_s_amoalu
  val amoalu   = Module(new AMOALU(wordBits))
  amoalu.io.mask := s3_req.amo_mask
  amoalu.io.cmd  := s3_req.cmd
  amoalu.io.lhs  := s3_data_word
  amoalu.io.rhs  := s3_req.amo_data

  // merge amo write data
//  val amo_bitmask = FillInterleaved(8, s3_req.amo_mask)
  val s3_amo_data_merged = Wire(Vec(DCacheBanks, UInt(DCacheSRAMRowBits.W)))
  val s3_sc_data_merged = Wire(Vec(DCacheBanks, UInt(DCacheSRAMRowBits.W)))
  for (i <- 0 until DCacheBanks) {
    val old_data = s3_store_data_merged(i)
    val new_data = amoalu.io.out
    val wmask = Mux(
      s3_req_word_idx_dup(i) === i.U,
      ~0.U(wordBytes.W),
      0.U(wordBytes.W)
    )
    s3_amo_data_merged(i) := mergePutData(old_data, new_data, wmask)
    s3_sc_data_merged(i) := mergePutData(old_data, s3_req.amo_data,
      Mux(s3_req_word_idx_dup(i) === i.U && !s3_sc_fail, s3_req.amo_mask, 0.U(wordBytes.W))
    )
  }
  val s3_amo_data_merged_reg = RegEnable(s3_amo_data_merged, do_amoalu)
  when(do_amoalu){
    s3_s_amoalu := true.B
    s3_s_amoalu_dup.foreach(_ := true.B)
  }

  val miss_wb = s3_req_miss_dup(3) && s3_need_replacement && s3_coh_dup(1).state =/= ClientStates.Nothing
  val miss_wb_dup = s3_req_miss_dup(3) && s3_need_replacement_dup && s3_coh_dup(1).state =/= ClientStates.Nothing
  val probe_wb = s3_req.probe
  val replace_wb = s3_req.replace
  val need_wb = miss_wb_dup || probe_wb || replace_wb

  val (_, miss_shrink_param, _) = s3_coh_dup(2).onCacheControl(M_FLUSH)
  val writeback_param = Mux(probe_wb, probe_shrink_param, miss_shrink_param)
  val writeback_data = if (dcacheParameters.alwaysReleaseData) {
    s3_tag_match && s3_req_probe_dup(6) && s3_req.probe_need_data ||
      s3_coh_dup(3) === ClientStates.Dirty || (miss_wb || replace_wb) && s3_coh_dup(3).state =/= ClientStates.Nothing
  } else {
    s3_tag_match && s3_req_probe_dup(6) && s3_req.probe_need_data || s3_coh_dup(3) === ClientStates.Dirty
  }

  val s3_probe_can_go = s3_req_probe_dup(7) && io.wb.ready && (io.meta_write.ready || !probe_update_meta)
  val s3_store_can_go = s3_req_source_dup_1 === STORE_SOURCE.U && !s3_req_probe_dup(8) && (io.meta_write.ready || !store_update_meta) && (io.data_write.ready || !update_data)
  val s3_amo_can_go = s3_amo_hit_dup && (io.meta_write.ready || !amo_update_meta) && (io.data_write.ready || !update_data) && (s3_s_amoalu_dup(0) || !amo_wait_amoalu)
  val s3_miss_can_go = s3_req_miss_dup(4) &&
    (io.meta_write.ready || !amo_update_meta) &&
    (io.data_write.ready || !update_data) &&
    (s3_s_amoalu_dup(1) || !amo_wait_amoalu) &&
    io.tag_write.ready &&
    io.wb.ready
  val s3_replace_nothing = s3_req_replace_dup(1) && s3_coh_dup(4).state === ClientStates.Nothing
  val s3_replace_can_go = s3_req_replace_dup(2) && (s3_replace_nothing || io.wb.ready)
  val s3_can_go = s3_probe_can_go || s3_store_can_go || s3_amo_can_go || s3_miss_can_go || s3_replace_can_go
  val s3_update_data_cango = s3_store_can_go || s3_amo_can_go || s3_miss_can_go // used to speed up data_write gen

  // ---------------- duplicate regs for meta_write.valid to solve fanout ----------------
  val s3_req_miss_dup_for_meta_w_valid = RegEnable(s2_req.miss, s2_fire_to_s3)
  val s3_req_probe_dup_for_meta_w_valid = RegEnable(s2_req.probe, s2_fire_to_s3)
  val s3_tag_match_dup_for_meta_w_valid = RegEnable(s2_tag_match, s2_fire_to_s3)
  val s3_coh_dup_for_meta_w_valid = RegEnable(s2_coh, s2_fire_to_s3)
  val s3_req_probe_param_dup_for_meta_w_valid = RegEnable(s2_req.probe_param, s2_fire_to_s3)
  val (_, _, probe_new_coh_dup_for_meta_w_valid) = s3_coh_dup_for_meta_w_valid.onProbe(s3_req_probe_param_dup_for_meta_w_valid)
  val s3_req_source_dup_for_meta_w_valid = RegEnable(s2_req.source, s2_fire_to_s3)
  val s3_req_cmd_dup_for_meta_w_valid = RegEnable(s2_req.cmd, s2_fire_to_s3)
  val s3_req_replace_dup_for_meta_w_valid = RegEnable(s2_req.replace, s2_fire_to_s3)
  val s3_hit_coh_dup_for_meta_w_valid = RegEnable(s2_hit_coh, s2_fire_to_s3)
  val s3_new_hit_coh_dup_for_meta_w_valid = RegEnable(s2_new_hit_coh, s2_fire_to_s3)
  
  val miss_update_meta_dup_for_meta_w_valid = s3_req_miss_dup_for_meta_w_valid
  val probe_update_meta_dup_for_meta_w_valid = WireInit(s3_req_probe_dup_for_meta_w_valid && s3_tag_match_dup_for_meta_w_valid && s3_coh_dup_for_meta_w_valid =/= probe_new_coh_dup_for_meta_w_valid)
  val store_update_meta_dup_for_meta_w_valid = s3_req_source_dup_for_meta_w_valid === STORE_SOURCE.U &&
    !s3_req_probe_dup_for_meta_w_valid &&
    s3_hit_coh_dup_for_meta_w_valid =/= s3_new_hit_coh_dup_for_meta_w_valid
  val amo_update_meta_dup_for_meta_w_valid = s3_req_source_dup_for_meta_w_valid === AMO_SOURCE.U &&
    !s3_req_probe_dup_for_meta_w_valid &&
    s3_hit_coh_dup_for_meta_w_valid =/= s3_new_hit_coh_dup_for_meta_w_valid
  val update_meta_dup_for_meta_w_valid = (
    miss_update_meta_dup_for_meta_w_valid ||
    probe_update_meta_dup_for_meta_w_valid ||
    store_update_meta_dup_for_meta_w_valid ||
    amo_update_meta_dup_for_meta_w_valid
  ) && !s3_req_replace_dup_for_meta_w_valid

  val s3_valid_dup_for_meta_w_valid = RegInit(false.B)
  val s3_amo_hit_dup_for_meta_w_valid = RegEnable(s2_amo_hit, s2_fire_to_s3)
  val s3_s_amoalu_dup_for_meta_w_valid = RegInit(false.B)
  val amo_wait_amoalu_dup_for_meta_w_valid = s3_req_source_dup_for_meta_w_valid === AMO_SOURCE.U &&
    s3_req_cmd_dup_for_meta_w_valid =/= M_XLR &&
    s3_req_cmd_dup_for_meta_w_valid =/= M_XSC
  val do_amoalu_dup_for_meta_w_valid = amo_wait_amoalu_dup_for_meta_w_valid && s3_valid_dup_for_meta_w_valid && !s3_s_amoalu_dup_for_meta_w_valid

  val s3_store_hit_dup_for_meta_w_valid = RegEnable(s2_store_hit, s2_fire_to_s3)
  val s3_req_addr_dup_for_meta_w_valid = RegEnable(s2_req.addr, s2_fire_to_s3)
  val s3_can_do_amo_dup_for_meta_w_valid = (s3_req_miss_dup_for_meta_w_valid && !s3_req_probe_dup_for_meta_w_valid && s3_req_source_dup_for_meta_w_valid === AMO_SOURCE.U) ||
    s3_amo_hit_dup_for_meta_w_valid

  val s3_lr_dup_for_meta_w_valid = !s3_req_probe_dup_for_meta_w_valid && s3_req_source_dup_for_meta_w_valid === AMO_SOURCE.U && s3_req_cmd_dup_for_meta_w_valid === M_XLR
  val s3_sc_dup_for_meta_w_valid = !s3_req_probe_dup_for_meta_w_valid && s3_req_source_dup_for_meta_w_valid === AMO_SOURCE.U && s3_req_cmd_dup_for_meta_w_valid === M_XSC
  val lrsc_addr_dup_for_meta_w_valid = Reg(UInt())
  val lrsc_count_dup_for_meta_w_valid = RegInit(0.U(log2Ceil(LRSCCycles).W))

  when (s3_valid_dup_for_meta_w_valid && (s3_lr_dup_for_meta_w_valid || s3_sc_dup_for_meta_w_valid)) {
    when (s3_can_do_amo_dup_for_meta_w_valid && s3_lr_dup_for_meta_w_valid) {
      lrsc_count_dup_for_meta_w_valid := (LRSCCycles - 1).U
      lrsc_addr_dup_for_meta_w_valid := get_block_addr(s3_req_addr_dup_for_meta_w_valid)
    }.otherwise {
      lrsc_count_dup_for_meta_w_valid := 0.U
    }
  }.elsewhen (io.invalid_resv_set) {
    lrsc_count_dup_for_meta_w_valid := 0.U
  }.elsewhen (lrsc_count_dup_for_meta_w_valid > 0.U) {
    lrsc_count_dup_for_meta_w_valid := lrsc_count_dup_for_meta_w_valid - 1.U
  }

  val lrsc_valid_dup_for_meta_w_valid = lrsc_count_dup_for_meta_w_valid > LRSCBackOff.U
  val s3_lrsc_addr_match_dup_for_meta_w_valid = lrsc_valid_dup_for_meta_w_valid && lrsc_addr_dup_for_meta_w_valid === get_block_addr(s3_req_addr_dup_for_meta_w_valid)
  val s3_sc_fail_dup_for_meta_w_valid = s3_sc_dup_for_meta_w_valid && !s3_lrsc_addr_match_dup_for_meta_w_valid
  val s3_can_do_amo_write_dup_for_meta_w_valid = s3_can_do_amo_dup_for_meta_w_valid && isWrite(s3_req_cmd_dup_for_meta_w_valid) && !s3_sc_fail_dup_for_meta_w_valid
  val update_data_dup_for_meta_w_valid = s3_req_miss_dup_for_meta_w_valid || s3_store_hit_dup_for_meta_w_valid || s3_can_do_amo_write_dup_for_meta_w_valid

  val s3_probe_can_go_dup_for_meta_w_valid = s3_req_probe_dup_for_meta_w_valid &&
    io.wb_ready_dup(metaWritePort) &&
    (io.meta_write.ready || !probe_update_meta_dup_for_meta_w_valid)
  val s3_store_can_go_dup_for_meta_w_valid = s3_req_source_dup_for_meta_w_valid === STORE_SOURCE.U && !s3_req_probe_dup_for_meta_w_valid &&
    (io.meta_write.ready || !store_update_meta_dup_for_meta_w_valid) &&
    (io.data_write_ready_dup(metaWritePort) || !update_data_dup_for_meta_w_valid)
  val s3_amo_can_go_dup_for_meta_w_valid = s3_amo_hit_dup_for_meta_w_valid &&
    (io.meta_write.ready || !amo_update_meta_dup_for_meta_w_valid) &&
    (io.data_write_ready_dup(metaWritePort) || !update_data_dup_for_meta_w_valid) &&
    (s3_s_amoalu_dup_for_meta_w_valid || !amo_wait_amoalu_dup_for_meta_w_valid)
  val s3_miss_can_go_dup_for_meta_w_valid = s3_req_miss_dup_for_meta_w_valid &&
    (io.meta_write.ready || !amo_update_meta_dup_for_meta_w_valid) &&
    (io.data_write_ready_dup(metaWritePort) || !update_data_dup_for_meta_w_valid) &&
    (s3_s_amoalu_dup_for_meta_w_valid || !amo_wait_amoalu_dup_for_meta_w_valid) &&
    io.tag_write_ready_dup(metaWritePort) &&
    io.wb_ready_dup(metaWritePort)
  val s3_replace_can_go_dup_for_meta_w_valid = s3_req_replace_dup_for_meta_w_valid &&
    (s3_coh_dup_for_meta_w_valid.state === ClientStates.Nothing || io.wb_ready_dup(metaWritePort))
  val s3_can_go_dup_for_meta_w_valid = s3_probe_can_go_dup_for_meta_w_valid ||
    s3_store_can_go_dup_for_meta_w_valid ||
    s3_amo_can_go_dup_for_meta_w_valid ||
    s3_miss_can_go_dup_for_meta_w_valid ||
    s3_replace_can_go_dup_for_meta_w_valid

  val s3_fire_dup_for_meta_w_valid = s3_valid_dup_for_meta_w_valid && s3_can_go_dup_for_meta_w_valid
  when (do_amoalu_dup_for_meta_w_valid) { s3_s_amoalu_dup_for_meta_w_valid := true.B }
  when (s3_fire_dup_for_meta_w_valid) { s3_s_amoalu_dup_for_meta_w_valid := false.B }

  // fix probe meta change
  val s3_probe_ttob_override = s3_valid &&
    // s3_probe_ttob_check_resp.valid && 
    s3_probe_ttob_check_resp.bits.toN && 
    s3_coh_dup_for_meta_w_valid === Trunk
  val s3_probe_new_coh = Mux(
    s3_probe_ttob_override,
    ClientMetadata(Nothing),
    probe_new_coh_dup_for_meta_w_valid
  )
  when(s3_probe_ttob_override) {
    probe_update_meta_dup_for_meta_w_valid := true.B
  }

  val new_coh = Mux(
    miss_update_meta_dup_for_meta_w_valid,
    miss_new_coh,
    Mux(
      probe_update_meta,
      s3_probe_new_coh,
      Mux(
        store_update_meta_dup_for_meta_w_valid || amo_update_meta_dup_for_meta_w_valid,
        s3_new_hit_coh_dup_for_meta_w_valid,
        ClientMetadata.onReset
      )
    )
  )

  when (s2_fire_to_s3) { s3_valid_dup_for_meta_w_valid := true.B }
  .elsewhen (s3_fire_dup_for_meta_w_valid) { s3_valid_dup_for_meta_w_valid := false.B }
  // -------------------------------------------------------------------------------------

  // ---------------- duplicate regs for err_write.valid to solve fanout -----------------
  val s3_req_miss_dup_for_err_w_valid = RegEnable(s2_req.miss, s2_fire_to_s3)
  val s3_req_probe_dup_for_err_w_valid = RegEnable(s2_req.probe, s2_fire_to_s3)
  val s3_tag_match_dup_for_err_w_valid = RegEnable(s2_tag_match, s2_fire_to_s3)
  val s3_coh_dup_for_err_w_valid = RegEnable(s2_coh, s2_fire_to_s3)
  val s3_req_probe_param_dup_for_err_w_valid = RegEnable(s2_req.probe_param, s2_fire_to_s3)
  val (_, _, probe_new_coh_dup_for_err_w_valid) = s3_coh_dup_for_err_w_valid.onProbe(s3_req_probe_param_dup_for_err_w_valid)
  val s3_req_source_dup_for_err_w_valid = RegEnable(s2_req.source, s2_fire_to_s3)
  val s3_req_cmd_dup_for_err_w_valid = RegEnable(s2_req.cmd, s2_fire_to_s3)
  val s3_req_replace_dup_for_err_w_valid = RegEnable(s2_req.replace, s2_fire_to_s3)
  val s3_hit_coh_dup_for_err_w_valid = RegEnable(s2_hit_coh, s2_fire_to_s3)
  val s3_new_hit_coh_dup_for_err_w_valid = RegEnable(s2_new_hit_coh, s2_fire_to_s3)
  
  val miss_update_meta_dup_for_err_w_valid = s3_req_miss_dup_for_err_w_valid
  val probe_update_meta_dup_for_err_w_valid = s3_req_probe_dup_for_err_w_valid && s3_tag_match_dup_for_err_w_valid && s3_coh_dup_for_err_w_valid =/= probe_new_coh_dup_for_err_w_valid
  val store_update_meta_dup_for_err_w_valid = s3_req_source_dup_for_err_w_valid === STORE_SOURCE.U &&
    !s3_req_probe_dup_for_err_w_valid &&
    s3_hit_coh_dup_for_err_w_valid =/= s3_new_hit_coh_dup_for_err_w_valid
  val amo_update_meta_dup_for_err_w_valid = s3_req_source_dup_for_err_w_valid === AMO_SOURCE.U &&
    !s3_req_probe_dup_for_err_w_valid &&
    s3_hit_coh_dup_for_err_w_valid =/= s3_new_hit_coh_dup_for_err_w_valid
  val update_meta_dup_for_err_w_valid = (
    miss_update_meta_dup_for_err_w_valid ||
    probe_update_meta_dup_for_err_w_valid ||
    store_update_meta_dup_for_err_w_valid ||
    amo_update_meta_dup_for_err_w_valid
  ) && !s3_req_replace_dup_for_err_w_valid

  val s3_valid_dup_for_err_w_valid = RegInit(false.B)
  val s3_amo_hit_dup_for_err_w_valid = RegEnable(s2_amo_hit, s2_fire_to_s3)
  val s3_s_amoalu_dup_for_err_w_valid = RegInit(false.B)
  val amo_wait_amoalu_dup_for_err_w_valid = s3_req_source_dup_for_err_w_valid === AMO_SOURCE.U &&
    s3_req_cmd_dup_for_err_w_valid =/= M_XLR &&
    s3_req_cmd_dup_for_err_w_valid =/= M_XSC
  val do_amoalu_dup_for_err_w_valid = amo_wait_amoalu_dup_for_err_w_valid && s3_valid_dup_for_err_w_valid && !s3_s_amoalu_dup_for_err_w_valid

  val s3_store_hit_dup_for_err_w_valid = RegEnable(s2_store_hit, s2_fire_to_s3)
  val s3_req_addr_dup_for_err_w_valid = RegEnable(s2_req.addr, s2_fire_to_s3)
  val s3_can_do_amo_dup_for_err_w_valid = (s3_req_miss_dup_for_err_w_valid && !s3_req_probe_dup_for_err_w_valid && s3_req_source_dup_for_err_w_valid === AMO_SOURCE.U) ||
    s3_amo_hit_dup_for_err_w_valid

  val s3_lr_dup_for_err_w_valid = !s3_req_probe_dup_for_err_w_valid && s3_req_source_dup_for_err_w_valid === AMO_SOURCE.U && s3_req_cmd_dup_for_err_w_valid === M_XLR
  val s3_sc_dup_for_err_w_valid = !s3_req_probe_dup_for_err_w_valid && s3_req_source_dup_for_err_w_valid === AMO_SOURCE.U && s3_req_cmd_dup_for_err_w_valid === M_XSC
  val lrsc_addr_dup_for_err_w_valid = Reg(UInt())
  val lrsc_count_dup_for_err_w_valid = RegInit(0.U(log2Ceil(LRSCCycles).W))

  when (s3_valid_dup_for_err_w_valid && (s3_lr_dup_for_err_w_valid || s3_sc_dup_for_err_w_valid)) {
    when (s3_can_do_amo_dup_for_err_w_valid && s3_lr_dup_for_err_w_valid) {
      lrsc_count_dup_for_err_w_valid := (LRSCCycles - 1).U
      lrsc_addr_dup_for_err_w_valid := get_block_addr(s3_req_addr_dup_for_err_w_valid)
    }.otherwise {
      lrsc_count_dup_for_err_w_valid := 0.U
    }
  }.elsewhen (io.invalid_resv_set) {
    lrsc_count_dup_for_err_w_valid := 0.U
  }.elsewhen (lrsc_count_dup_for_err_w_valid > 0.U) {
    lrsc_count_dup_for_err_w_valid := lrsc_count_dup_for_err_w_valid - 1.U
  }

  val lrsc_valid_dup_for_err_w_valid = lrsc_count_dup_for_err_w_valid > LRSCBackOff.U
  val s3_lrsc_addr_match_dup_for_err_w_valid = lrsc_valid_dup_for_err_w_valid && lrsc_addr_dup_for_err_w_valid === get_block_addr(s3_req_addr_dup_for_err_w_valid)
  val s3_sc_fail_dup_for_err_w_valid = s3_sc_dup_for_err_w_valid && !s3_lrsc_addr_match_dup_for_err_w_valid
  val s3_can_do_amo_write_dup_for_err_w_valid = s3_can_do_amo_dup_for_err_w_valid && isWrite(s3_req_cmd_dup_for_err_w_valid) && !s3_sc_fail_dup_for_err_w_valid
  val update_data_dup_for_err_w_valid = s3_req_miss_dup_for_err_w_valid || s3_store_hit_dup_for_err_w_valid || s3_can_do_amo_write_dup_for_err_w_valid

  val s3_probe_can_go_dup_for_err_w_valid = s3_req_probe_dup_for_err_w_valid &&
    io.wb_ready_dup(errWritePort) &&
    (io.meta_write.ready || !probe_update_meta_dup_for_err_w_valid)
  val s3_store_can_go_dup_for_err_w_valid = s3_req_source_dup_for_err_w_valid === STORE_SOURCE.U && !s3_req_probe_dup_for_err_w_valid &&
    (io.meta_write.ready || !store_update_meta_dup_for_err_w_valid) &&
    (io.data_write_ready_dup(errWritePort) || !update_data_dup_for_err_w_valid)
  val s3_amo_can_go_dup_for_err_w_valid = s3_amo_hit_dup_for_err_w_valid &&
    (io.meta_write.ready || !amo_update_meta_dup_for_err_w_valid) &&
    (io.data_write_ready_dup(errWritePort) || !update_data_dup_for_err_w_valid) &&
    (s3_s_amoalu_dup_for_err_w_valid || !amo_wait_amoalu_dup_for_err_w_valid)
  val s3_miss_can_go_dup_for_err_w_valid = s3_req_miss_dup_for_err_w_valid &&
    (io.meta_write.ready || !amo_update_meta_dup_for_err_w_valid) &&
    (io.data_write_ready_dup(errWritePort) || !update_data_dup_for_err_w_valid) &&
    (s3_s_amoalu_dup_for_err_w_valid || !amo_wait_amoalu_dup_for_err_w_valid) &&
    io.tag_write_ready_dup(errWritePort) &&
    io.wb_ready_dup(errWritePort)
  val s3_replace_can_go_dup_for_err_w_valid = s3_req_replace_dup_for_err_w_valid &&
    (s3_coh_dup_for_err_w_valid.state === ClientStates.Nothing || io.wb_ready_dup(errWritePort))
  val s3_can_go_dup_for_err_w_valid = s3_probe_can_go_dup_for_err_w_valid ||
    s3_store_can_go_dup_for_err_w_valid ||
    s3_amo_can_go_dup_for_err_w_valid ||
    s3_miss_can_go_dup_for_err_w_valid ||
    s3_replace_can_go_dup_for_err_w_valid

  val s3_fire_dup_for_err_w_valid = s3_valid_dup_for_err_w_valid && s3_can_go_dup_for_err_w_valid
  when (do_amoalu_dup_for_err_w_valid) { s3_s_amoalu_dup_for_err_w_valid := true.B }
  when (s3_fire_dup_for_err_w_valid) { s3_s_amoalu_dup_for_err_w_valid := false.B }

  when (s2_fire_to_s3) { s3_valid_dup_for_err_w_valid := true.B }
  .elsewhen (s3_fire_dup_for_err_w_valid) { s3_valid_dup_for_err_w_valid := false.B }
  // -------------------------------------------------------------------------------------
  // ---------------- duplicate regs for tag_write.valid to solve fanout -----------------
  val s3_req_miss_dup_for_tag_w_valid = RegEnable(s2_req.miss, s2_fire_to_s3)
  val s3_req_probe_dup_for_tag_w_valid = RegEnable(s2_req.probe, s2_fire_to_s3)
  val s3_tag_match_dup_for_tag_w_valid = RegEnable(s2_tag_match, s2_fire_to_s3)
  val s3_coh_dup_for_tag_w_valid = RegEnable(s2_coh, s2_fire_to_s3)
  val s3_req_probe_param_dup_for_tag_w_valid = RegEnable(s2_req.probe_param, s2_fire_to_s3)
  val (_, _, probe_new_coh_dup_for_tag_w_valid) = s3_coh_dup_for_tag_w_valid.onProbe(s3_req_probe_param_dup_for_tag_w_valid)
  val s3_req_source_dup_for_tag_w_valid = RegEnable(s2_req.source, s2_fire_to_s3)
  val s3_req_cmd_dup_for_tag_w_valid = RegEnable(s2_req.cmd, s2_fire_to_s3)
  val s3_req_replace_dup_for_tag_w_valid = RegEnable(s2_req.replace, s2_fire_to_s3)
  val s3_hit_coh_dup_for_tag_w_valid = RegEnable(s2_hit_coh, s2_fire_to_s3)
  val s3_new_hit_coh_dup_for_tag_w_valid = RegEnable(s2_new_hit_coh, s2_fire_to_s3)
  
  val miss_update_meta_dup_for_tag_w_valid = s3_req_miss_dup_for_tag_w_valid
  val probe_update_meta_dup_for_tag_w_valid = s3_req_probe_dup_for_tag_w_valid && s3_tag_match_dup_for_tag_w_valid && s3_coh_dup_for_tag_w_valid =/= probe_new_coh_dup_for_tag_w_valid
  val store_update_meta_dup_for_tag_w_valid = s3_req_source_dup_for_tag_w_valid === STORE_SOURCE.U &&
    !s3_req_probe_dup_for_tag_w_valid &&
    s3_hit_coh_dup_for_tag_w_valid =/= s3_new_hit_coh_dup_for_tag_w_valid
  val amo_update_meta_dup_for_tag_w_valid = s3_req_source_dup_for_tag_w_valid === AMO_SOURCE.U &&
    !s3_req_probe_dup_for_tag_w_valid &&
    s3_hit_coh_dup_for_tag_w_valid =/= s3_new_hit_coh_dup_for_tag_w_valid
  val update_meta_dup_for_tag_w_valid = (
    miss_update_meta_dup_for_tag_w_valid ||
    probe_update_meta_dup_for_tag_w_valid ||
    store_update_meta_dup_for_tag_w_valid ||
    amo_update_meta_dup_for_tag_w_valid
  ) && !s3_req_replace_dup_for_tag_w_valid

  val s3_valid_dup_for_tag_w_valid = RegInit(false.B)
  val s3_amo_hit_dup_for_tag_w_valid = RegEnable(s2_amo_hit, s2_fire_to_s3)
  val s3_s_amoalu_dup_for_tag_w_valid = RegInit(false.B)
  val amo_wait_amoalu_dup_for_tag_w_valid = s3_req_source_dup_for_tag_w_valid === AMO_SOURCE.U &&
    s3_req_cmd_dup_for_tag_w_valid =/= M_XLR &&
    s3_req_cmd_dup_for_tag_w_valid =/= M_XSC
  val do_amoalu_dup_for_tag_w_valid = amo_wait_amoalu_dup_for_tag_w_valid && s3_valid_dup_for_tag_w_valid && !s3_s_amoalu_dup_for_tag_w_valid

  val s3_store_hit_dup_for_tag_w_valid = RegEnable(s2_store_hit, s2_fire_to_s3)
  val s3_req_addr_dup_for_tag_w_valid = RegEnable(s2_req.addr, s2_fire_to_s3)
  val s3_can_do_amo_dup_for_tag_w_valid = (s3_req_miss_dup_for_tag_w_valid && !s3_req_probe_dup_for_tag_w_valid && s3_req_source_dup_for_tag_w_valid === AMO_SOURCE.U) ||
    s3_amo_hit_dup_for_tag_w_valid

  val s3_lr_dup_for_tag_w_valid = !s3_req_probe_dup_for_tag_w_valid && s3_req_source_dup_for_tag_w_valid === AMO_SOURCE.U && s3_req_cmd_dup_for_tag_w_valid === M_XLR
  val s3_sc_dup_for_tag_w_valid = !s3_req_probe_dup_for_tag_w_valid && s3_req_source_dup_for_tag_w_valid === AMO_SOURCE.U && s3_req_cmd_dup_for_tag_w_valid === M_XSC
  val lrsc_addr_dup_for_tag_w_valid = Reg(UInt())
  val lrsc_count_dup_for_tag_w_valid = RegInit(0.U(log2Ceil(LRSCCycles).W))

  when (s3_valid_dup_for_tag_w_valid && (s3_lr_dup_for_tag_w_valid || s3_sc_dup_for_tag_w_valid)) {
    when (s3_can_do_amo_dup_for_tag_w_valid && s3_lr_dup_for_tag_w_valid) {
      lrsc_count_dup_for_tag_w_valid := (LRSCCycles - 1).U
      lrsc_addr_dup_for_tag_w_valid := get_block_addr(s3_req_addr_dup_for_tag_w_valid)
    }.otherwise {
      lrsc_count_dup_for_tag_w_valid := 0.U
    }
  }.elsewhen (io.invalid_resv_set) {
    lrsc_count_dup_for_tag_w_valid := 0.U
  }.elsewhen (lrsc_count_dup_for_tag_w_valid > 0.U) {
    lrsc_count_dup_for_tag_w_valid := lrsc_count_dup_for_tag_w_valid - 1.U
  }

  val lrsc_valid_dup_for_tag_w_valid = lrsc_count_dup_for_tag_w_valid > LRSCBackOff.U
  val s3_lrsc_addr_match_dup_for_tag_w_valid = lrsc_valid_dup_for_tag_w_valid && lrsc_addr_dup_for_tag_w_valid === get_block_addr(s3_req_addr_dup_for_tag_w_valid)
  val s3_sc_fail_dup_for_tag_w_valid = s3_sc_dup_for_tag_w_valid && !s3_lrsc_addr_match_dup_for_tag_w_valid
  val s3_can_do_amo_write_dup_for_tag_w_valid = s3_can_do_amo_dup_for_tag_w_valid && isWrite(s3_req_cmd_dup_for_tag_w_valid) && !s3_sc_fail_dup_for_tag_w_valid
  val update_data_dup_for_tag_w_valid = s3_req_miss_dup_for_tag_w_valid || s3_store_hit_dup_for_tag_w_valid || s3_can_do_amo_write_dup_for_tag_w_valid

  val s3_probe_can_go_dup_for_tag_w_valid = s3_req_probe_dup_for_tag_w_valid &&
    io.wb_ready_dup(tagWritePort) &&
    (io.meta_write.ready || !probe_update_meta_dup_for_tag_w_valid)
  val s3_store_can_go_dup_for_tag_w_valid = s3_req_source_dup_for_tag_w_valid === STORE_SOURCE.U && !s3_req_probe_dup_for_tag_w_valid &&
    (io.meta_write.ready || !store_update_meta_dup_for_tag_w_valid) &&
    (io.data_write_ready_dup(tagWritePort) || !update_data_dup_for_tag_w_valid)
  val s3_amo_can_go_dup_for_tag_w_valid = s3_amo_hit_dup_for_tag_w_valid &&
    (io.meta_write.ready || !amo_update_meta_dup_for_tag_w_valid) &&
    (io.data_write_ready_dup(tagWritePort) || !update_data_dup_for_tag_w_valid) &&
    (s3_s_amoalu_dup_for_tag_w_valid || !amo_wait_amoalu_dup_for_tag_w_valid)
  val s3_miss_can_go_dup_for_tag_w_valid = s3_req_miss_dup_for_tag_w_valid &&
    (io.meta_write.ready || !amo_update_meta_dup_for_tag_w_valid) &&
    (io.data_write_ready_dup(tagWritePort) || !update_data_dup_for_tag_w_valid) &&
    (s3_s_amoalu_dup_for_tag_w_valid || !amo_wait_amoalu_dup_for_tag_w_valid) &&
    io.tag_write_ready_dup(tagWritePort) &&
    io.wb_ready_dup(tagWritePort)
  val s3_replace_can_go_dup_for_tag_w_valid = s3_req_replace_dup_for_tag_w_valid &&
    (s3_coh_dup_for_tag_w_valid.state === ClientStates.Nothing || io.wb_ready_dup(tagWritePort))
  val s3_can_go_dup_for_tag_w_valid = s3_probe_can_go_dup_for_tag_w_valid ||
    s3_store_can_go_dup_for_tag_w_valid ||
    s3_amo_can_go_dup_for_tag_w_valid ||
    s3_miss_can_go_dup_for_tag_w_valid ||
    s3_replace_can_go_dup_for_tag_w_valid

  val s3_fire_dup_for_tag_w_valid = s3_valid_dup_for_tag_w_valid && s3_can_go_dup_for_tag_w_valid
  when (do_amoalu_dup_for_tag_w_valid) { s3_s_amoalu_dup_for_tag_w_valid := true.B }
  when (s3_fire_dup_for_tag_w_valid) { s3_s_amoalu_dup_for_tag_w_valid := false.B }

  when (s2_fire_to_s3) { s3_valid_dup_for_tag_w_valid := true.B }
  .elsewhen (s3_fire_dup_for_tag_w_valid) { s3_valid_dup_for_tag_w_valid := false.B }
  // -------------------------------------------------------------------------------------
  // ---------------- duplicate regs for data_write.valid to solve fanout ----------------
  val s3_req_miss_dup_for_data_w_valid = RegEnable(s2_req.miss, s2_fire_to_s3)
  val s3_req_probe_dup_for_data_w_valid = RegEnable(s2_req.probe, s2_fire_to_s3)
  val s3_tag_match_dup_for_data_w_valid = RegEnable(s2_tag_match, s2_fire_to_s3)
  val s3_coh_dup_for_data_w_valid = RegEnable(s2_coh, s2_fire_to_s3)
  val s3_req_probe_param_dup_for_data_w_valid = RegEnable(s2_req.probe_param, s2_fire_to_s3)
  val (_, _, probe_new_coh_dup_for_data_w_valid) = s3_coh_dup_for_data_w_valid.onProbe(s3_req_probe_param_dup_for_data_w_valid)
  val s3_req_source_dup_for_data_w_valid = RegEnable(s2_req.source, s2_fire_to_s3)
  val s3_req_cmd_dup_for_data_w_valid = RegEnable(s2_req.cmd, s2_fire_to_s3)
  val s3_req_replace_dup_for_data_w_valid = RegEnable(s2_req.replace, s2_fire_to_s3)
  val s3_hit_coh_dup_for_data_w_valid = RegEnable(s2_hit_coh, s2_fire_to_s3)
  val s3_new_hit_coh_dup_for_data_w_valid = RegEnable(s2_new_hit_coh, s2_fire_to_s3)
  
  val miss_update_meta_dup_for_data_w_valid = s3_req_miss_dup_for_data_w_valid
  val probe_update_meta_dup_for_data_w_valid = s3_req_probe_dup_for_data_w_valid && s3_tag_match_dup_for_data_w_valid && s3_coh_dup_for_data_w_valid =/= probe_new_coh_dup_for_data_w_valid
  val store_update_meta_dup_for_data_w_valid = s3_req_source_dup_for_data_w_valid === STORE_SOURCE.U &&
    !s3_req_probe_dup_for_data_w_valid &&
    s3_hit_coh_dup_for_data_w_valid =/= s3_new_hit_coh_dup_for_data_w_valid
  val amo_update_meta_dup_for_data_w_valid = s3_req_source_dup_for_data_w_valid === AMO_SOURCE.U &&
    !s3_req_probe_dup_for_data_w_valid &&
    s3_hit_coh_dup_for_data_w_valid =/= s3_new_hit_coh_dup_for_data_w_valid
  val update_meta_dup_for_data_w_valid = (
    miss_update_meta_dup_for_data_w_valid ||
    probe_update_meta_dup_for_data_w_valid ||
    store_update_meta_dup_for_data_w_valid ||
    amo_update_meta_dup_for_data_w_valid
  ) && !s3_req_replace_dup_for_data_w_valid

  val s3_valid_dup_for_data_w_valid = RegInit(false.B)
  val s3_amo_hit_dup_for_data_w_valid = RegEnable(s2_amo_hit, s2_fire_to_s3)
  val s3_s_amoalu_dup_for_data_w_valid = RegInit(false.B)
  val amo_wait_amoalu_dup_for_data_w_valid = s3_req_source_dup_for_data_w_valid === AMO_SOURCE.U &&
    s3_req_cmd_dup_for_data_w_valid =/= M_XLR &&
    s3_req_cmd_dup_for_data_w_valid =/= M_XSC
  val do_amoalu_dup_for_data_w_valid = amo_wait_amoalu_dup_for_data_w_valid && s3_valid_dup_for_data_w_valid && !s3_s_amoalu_dup_for_data_w_valid

  val s3_store_hit_dup_for_data_w_valid = RegEnable(s2_store_hit, s2_fire_to_s3)
  val s3_req_addr_dup_for_data_w_valid = RegEnable(s2_req.addr, s2_fire_to_s3)
  val s3_can_do_amo_dup_for_data_w_valid = (s3_req_miss_dup_for_data_w_valid && !s3_req_probe_dup_for_data_w_valid && s3_req_source_dup_for_data_w_valid === AMO_SOURCE.U) ||
    s3_amo_hit_dup_for_data_w_valid

  val s3_lr_dup_for_data_w_valid = !s3_req_probe_dup_for_data_w_valid && s3_req_source_dup_for_data_w_valid === AMO_SOURCE.U && s3_req_cmd_dup_for_data_w_valid === M_XLR
  val s3_sc_dup_for_data_w_valid = !s3_req_probe_dup_for_data_w_valid && s3_req_source_dup_for_data_w_valid === AMO_SOURCE.U && s3_req_cmd_dup_for_data_w_valid === M_XSC
  val lrsc_addr_dup_for_data_w_valid = Reg(UInt())
  val lrsc_count_dup_for_data_w_valid = RegInit(0.U(log2Ceil(LRSCCycles).W))

  when (s3_valid_dup_for_data_w_valid && (s3_lr_dup_for_data_w_valid || s3_sc_dup_for_data_w_valid)) {
    when (s3_can_do_amo_dup_for_data_w_valid && s3_lr_dup_for_data_w_valid) {
      lrsc_count_dup_for_data_w_valid := (LRSCCycles - 1).U
      lrsc_addr_dup_for_data_w_valid := get_block_addr(s3_req_addr_dup_for_data_w_valid)
    }.otherwise {
      lrsc_count_dup_for_data_w_valid := 0.U
    }
  }.elsewhen (io.invalid_resv_set) {
    lrsc_count_dup_for_data_w_valid := 0.U
  }.elsewhen (lrsc_count_dup_for_data_w_valid > 0.U) {
    lrsc_count_dup_for_data_w_valid := lrsc_count_dup_for_data_w_valid - 1.U
  }

  val lrsc_valid_dup_for_data_w_valid = lrsc_count_dup_for_data_w_valid > LRSCBackOff.U
  val s3_lrsc_addr_match_dup_for_data_w_valid = lrsc_valid_dup_for_data_w_valid && lrsc_addr_dup_for_data_w_valid === get_block_addr(s3_req_addr_dup_for_data_w_valid)
  val s3_sc_fail_dup_for_data_w_valid = s3_sc_dup_for_data_w_valid && !s3_lrsc_addr_match_dup_for_data_w_valid
  val s3_can_do_amo_write_dup_for_data_w_valid = s3_can_do_amo_dup_for_data_w_valid && isWrite(s3_req_cmd_dup_for_data_w_valid) && !s3_sc_fail_dup_for_data_w_valid
  val update_data_dup_for_data_w_valid = s3_req_miss_dup_for_data_w_valid || s3_store_hit_dup_for_data_w_valid || s3_can_do_amo_write_dup_for_data_w_valid

  val s3_probe_can_go_dup_for_data_w_valid = s3_req_probe_dup_for_data_w_valid &&
    io.wb_ready_dup(dataWritePort) &&
    (io.meta_write.ready || !probe_update_meta_dup_for_data_w_valid)
  val s3_store_can_go_dup_for_data_w_valid = s3_req_source_dup_for_data_w_valid === STORE_SOURCE.U && !s3_req_probe_dup_for_data_w_valid &&
    (io.meta_write.ready || !store_update_meta_dup_for_data_w_valid) &&
    (io.data_write_ready_dup(dataWritePort) || !update_data_dup_for_data_w_valid)
  val s3_amo_can_go_dup_for_data_w_valid = s3_amo_hit_dup_for_data_w_valid &&
    (io.meta_write.ready || !amo_update_meta_dup_for_data_w_valid) &&
    (io.data_write_ready_dup(dataWritePort) || !update_data_dup_for_data_w_valid) &&
    (s3_s_amoalu_dup_for_data_w_valid || !amo_wait_amoalu_dup_for_data_w_valid)
  val s3_miss_can_go_dup_for_data_w_valid = s3_req_miss_dup_for_data_w_valid &&
    (io.meta_write.ready || !amo_update_meta_dup_for_data_w_valid) &&
    (io.data_write_ready_dup(dataWritePort) || !update_data_dup_for_data_w_valid) &&
    (s3_s_amoalu_dup_for_data_w_valid || !amo_wait_amoalu_dup_for_data_w_valid) &&
    io.tag_write_ready_dup(dataWritePort) &&
    io.wb_ready_dup(dataWritePort)
  val s3_replace_can_go_dup_for_data_w_valid = s3_req_replace_dup_for_data_w_valid &&
    (s3_coh_dup_for_data_w_valid.state === ClientStates.Nothing || io.wb_ready_dup(dataWritePort))
  val s3_can_go_dup_for_data_w_valid = s3_probe_can_go_dup_for_data_w_valid ||
    s3_store_can_go_dup_for_data_w_valid ||
    s3_amo_can_go_dup_for_data_w_valid ||
    s3_miss_can_go_dup_for_data_w_valid ||
    s3_replace_can_go_dup_for_data_w_valid
  val s3_update_data_cango_dup_for_data_w_valid = s3_store_can_go_dup_for_data_w_valid || s3_amo_can_go_dup_for_data_w_valid || s3_miss_can_go_dup_for_data_w_valid

  val s3_fire_dup_for_data_w_valid = s3_valid_dup_for_data_w_valid && s3_can_go_dup_for_data_w_valid
  when (do_amoalu_dup_for_data_w_valid) { s3_s_amoalu_dup_for_data_w_valid := true.B }
  when (s3_fire_dup_for_data_w_valid) { s3_s_amoalu_dup_for_data_w_valid := false.B }

  val s3_banked_store_wmask_dup_for_data_w_valid = RegEnable(s2_banked_store_wmask, s2_fire_to_s3)
  val s3_req_word_idx_dup_for_data_w_valid = RegEnable(s2_req.word_idx, s2_fire_to_s3)
  val banked_wmask = Mux(
    s3_req_miss_dup_for_data_w_valid,
    banked_full_wmask,
    Mux(
      s3_store_hit_dup_for_data_w_valid,
      s3_banked_store_wmask_dup_for_data_w_valid,
      Mux(
        s3_can_do_amo_write_dup_for_data_w_valid,
        UIntToOH(s3_req_word_idx_dup_for_data_w_valid),
        banked_none_wmask
      )
    )
  )
  assert(!(s3_valid && banked_wmask.orR && !update_data))

  val s3_sc_data_merged_dup_for_data_w_valid = Wire(Vec(DCacheBanks, UInt(DCacheSRAMRowBits.W)))
  val s3_req_amo_data_dup_for_data_w_valid = RegEnable(s2_req.amo_data, s2_fire_to_s3)
  val s3_req_amo_mask_dup_for_data_w_valid = RegEnable(s2_req.amo_mask, s2_fire_to_s3)
  for (i <- 0 until DCacheBanks) {
    val old_data = s3_store_data_merged(i)
    s3_sc_data_merged_dup_for_data_w_valid(i) := mergePutData(old_data, s3_req_amo_data_dup_for_data_w_valid,
      Mux(
        s3_req_word_idx_dup_for_data_w_valid === i.U && !s3_sc_fail_dup_for_data_w_valid,
        s3_req_amo_mask_dup_for_data_w_valid,
        0.U(wordBytes.W)
      )
    )
  }

  when (s2_fire_to_s3) { s3_valid_dup_for_data_w_valid := true.B }
  .elsewhen (s3_fire_dup_for_data_w_valid) { s3_valid_dup_for_data_w_valid := false.B }

  val s3_valid_dup_for_data_w_bank = RegInit(VecInit(Seq.fill(DCacheBanks)(false.B))) // TODO
  val data_write_ready_dup_for_data_w_bank = io.data_write_ready_dup.drop(dataWritePort).take(DCacheBanks)
  val tag_write_ready_dup_for_data_w_bank = io.tag_write_ready_dup.drop(dataWritePort).take(DCacheBanks)
  val wb_ready_dup_for_data_w_bank = io.wb_ready_dup.drop(dataWritePort).take(DCacheBanks)
  for (i <- 0 until DCacheBanks) {
    val s3_req_miss_dup_for_data_w_bank = RegEnable(s2_req.miss, s2_fire_to_s3)
    val s3_req_probe_dup_for_data_w_bank = RegEnable(s2_req.probe, s2_fire_to_s3)
    val s3_tag_match_dup_for_data_w_bank = RegEnable(s2_tag_match, s2_fire_to_s3)
    val s3_coh_dup_for_data_w_bank = RegEnable(s2_coh, s2_fire_to_s3)
    val s3_req_probe_param_dup_for_data_w_bank = RegEnable(s2_req.probe_param, s2_fire_to_s3)
    val (_, _, probe_new_coh_dup_for_data_w_bank) = s3_coh_dup_for_data_w_bank.onProbe(s3_req_probe_param_dup_for_data_w_bank)
    val s3_req_source_dup_for_data_w_bank = RegEnable(s2_req.source, s2_fire_to_s3)
    val s3_req_cmd_dup_for_data_w_bank = RegEnable(s2_req.cmd, s2_fire_to_s3)
    val s3_req_replace_dup_for_data_w_bank = RegEnable(s2_req.replace, s2_fire_to_s3)
    val s3_hit_coh_dup_for_data_w_bank = RegEnable(s2_hit_coh, s2_fire_to_s3)
    val s3_new_hit_coh_dup_for_data_w_bank = RegEnable(s2_new_hit_coh, s2_fire_to_s3)
    
    val miss_update_meta_dup_for_data_w_bank = s3_req_miss_dup_for_data_w_bank
    val probe_update_meta_dup_for_data_w_bank = s3_req_probe_dup_for_data_w_bank && s3_tag_match_dup_for_data_w_bank && s3_coh_dup_for_data_w_bank =/= probe_new_coh_dup_for_data_w_bank
    val store_update_meta_dup_for_data_w_bank = s3_req_source_dup_for_data_w_bank === STORE_SOURCE.U &&
      !s3_req_probe_dup_for_data_w_bank &&
      s3_hit_coh_dup_for_data_w_bank =/= s3_new_hit_coh_dup_for_data_w_bank
    val amo_update_meta_dup_for_data_w_bank = s3_req_source_dup_for_data_w_bank === AMO_SOURCE.U &&
      !s3_req_probe_dup_for_data_w_bank &&
      s3_hit_coh_dup_for_data_w_bank =/= s3_new_hit_coh_dup_for_data_w_bank
    val update_meta_dup_for_data_w_bank = (
      miss_update_meta_dup_for_data_w_bank ||
      probe_update_meta_dup_for_data_w_bank ||
      store_update_meta_dup_for_data_w_bank ||
      amo_update_meta_dup_for_data_w_bank
    ) && !s3_req_replace_dup_for_data_w_bank

    val s3_amo_hit_dup_for_data_w_bank = RegEnable(s2_amo_hit, s2_fire_to_s3)
    val s3_s_amoalu_dup_for_data_w_bank = RegInit(false.B)
    val amo_wait_amoalu_dup_for_data_w_bank = s3_req_source_dup_for_data_w_bank === AMO_SOURCE.U &&
      s3_req_cmd_dup_for_data_w_bank =/= M_XLR &&
      s3_req_cmd_dup_for_data_w_bank =/= M_XSC
    val do_amoalu_dup_for_data_w_bank = amo_wait_amoalu_dup_for_data_w_bank && s3_valid_dup_for_data_w_bank(i) && !s3_s_amoalu_dup_for_data_w_bank

    val s3_store_hit_dup_for_data_w_bank = RegEnable(s2_store_hit, s2_fire_to_s3)
    val s3_req_addr_dup_for_data_w_bank = RegEnable(s2_req.addr, s2_fire_to_s3)
    val s3_can_do_amo_dup_for_data_w_bank = (s3_req_miss_dup_for_data_w_bank && !s3_req_probe_dup_for_data_w_bank && s3_req_source_dup_for_data_w_bank === AMO_SOURCE.U) ||
      s3_amo_hit_dup_for_data_w_bank

    val s3_lr_dup_for_data_w_bank = !s3_req_probe_dup_for_data_w_bank && s3_req_source_dup_for_data_w_bank === AMO_SOURCE.U && s3_req_cmd_dup_for_data_w_bank === M_XLR
    val s3_sc_dup_for_data_w_bank = !s3_req_probe_dup_for_data_w_bank && s3_req_source_dup_for_data_w_bank === AMO_SOURCE.U && s3_req_cmd_dup_for_data_w_bank === M_XSC
    val lrsc_addr_dup_for_data_w_bank = Reg(UInt())
    val lrsc_count_dup_for_data_w_bank = RegInit(0.U(log2Ceil(LRSCCycles).W))

    when (s3_valid_dup_for_data_w_bank(i) && (s3_lr_dup_for_data_w_bank || s3_sc_dup_for_data_w_bank)) {
      when (s3_can_do_amo_dup_for_data_w_bank && s3_lr_dup_for_data_w_bank) {
        lrsc_count_dup_for_data_w_bank := (LRSCCycles - 1).U
        lrsc_addr_dup_for_data_w_bank := get_block_addr(s3_req_addr_dup_for_data_w_bank)
      }.otherwise {
        lrsc_count_dup_for_data_w_bank := 0.U
      }
    }.elsewhen (io.invalid_resv_set) {
      lrsc_count_dup_for_data_w_bank := 0.U
    }.elsewhen (lrsc_count_dup_for_data_w_bank > 0.U) {
      lrsc_count_dup_for_data_w_bank := lrsc_count_dup_for_data_w_bank - 1.U
    }

    val lrsc_valid_dup_for_data_w_bank = lrsc_count_dup_for_data_w_bank > LRSCBackOff.U
    val s3_lrsc_addr_match_dup_for_data_w_bank = lrsc_valid_dup_for_data_w_bank && lrsc_addr_dup_for_data_w_bank === get_block_addr(s3_req_addr_dup_for_data_w_bank)
    val s3_sc_fail_dup_for_data_w_bank = s3_sc_dup_for_data_w_bank && !s3_lrsc_addr_match_dup_for_data_w_bank
    val s3_can_do_amo_write_dup_for_data_w_bank = s3_can_do_amo_dup_for_data_w_bank && isWrite(s3_req_cmd_dup_for_data_w_bank) && !s3_sc_fail_dup_for_data_w_bank
    val update_data_dup_for_data_w_bank = s3_req_miss_dup_for_data_w_bank || s3_store_hit_dup_for_data_w_bank || s3_can_do_amo_write_dup_for_data_w_bank

    val s3_probe_can_go_dup_for_data_w_bank = s3_req_probe_dup_for_data_w_bank &&
      wb_ready_dup_for_data_w_bank(i) &&
      (io.meta_write.ready || !probe_update_meta_dup_for_data_w_bank)
    val s3_store_can_go_dup_for_data_w_bank = s3_req_source_dup_for_data_w_bank === STORE_SOURCE.U && !s3_req_probe_dup_for_data_w_bank &&
      (io.meta_write.ready || !store_update_meta_dup_for_data_w_bank) &&
      (data_write_ready_dup_for_data_w_bank(i) || !update_data_dup_for_data_w_bank)
    val s3_amo_can_go_dup_for_data_w_bank = s3_amo_hit_dup_for_data_w_bank &&
      (io.meta_write.ready || !amo_update_meta_dup_for_data_w_bank) &&
      (data_write_ready_dup_for_data_w_bank(i) || !update_data_dup_for_data_w_bank) &&
      (s3_s_amoalu_dup_for_data_w_bank || !amo_wait_amoalu_dup_for_data_w_bank)
    val s3_miss_can_go_dup_for_data_w_bank = s3_req_miss_dup_for_data_w_bank &&
      (io.meta_write.ready || !amo_update_meta_dup_for_data_w_bank) &&
      (data_write_ready_dup_for_data_w_bank(i) || !update_data_dup_for_data_w_bank) &&
      (s3_s_amoalu_dup_for_data_w_bank || !amo_wait_amoalu_dup_for_data_w_bank) &&
      tag_write_ready_dup_for_data_w_bank(i) &&
      wb_ready_dup_for_data_w_bank(i)
    val s3_replace_can_go_dup_for_data_w_bank = s3_req_replace_dup_for_data_w_bank &&
      (s3_coh_dup_for_data_w_bank.state === ClientStates.Nothing || wb_ready_dup_for_data_w_bank(i))
    val s3_can_go_dup_for_data_w_bank = s3_probe_can_go_dup_for_data_w_bank ||
      s3_store_can_go_dup_for_data_w_bank ||
      s3_amo_can_go_dup_for_data_w_bank ||
      s3_miss_can_go_dup_for_data_w_bank ||
      s3_replace_can_go_dup_for_data_w_bank
    val s3_update_data_cango_dup_for_data_w_bank = s3_store_can_go_dup_for_data_w_bank || s3_amo_can_go_dup_for_data_w_bank || s3_miss_can_go_dup_for_data_w_bank

    val s3_fire_dup_for_data_w_bank = s3_valid_dup_for_data_w_bank(i) && s3_can_go_dup_for_data_w_bank

    when (do_amoalu_dup_for_data_w_bank) { s3_s_amoalu_dup_for_data_w_bank := true.B }
    when (s3_fire_dup_for_data_w_bank) { s3_s_amoalu_dup_for_data_w_bank := false.B }

    when (s2_fire_to_s3) { s3_valid_dup_for_data_w_bank(i) := true.B }
    .elsewhen (s3_fire_dup_for_data_w_bank) { s3_valid_dup_for_data_w_bank(i) := false.B }

    io.data_write_dup(i).valid := s3_valid_dup_for_data_w_bank(i) && s3_update_data_cango_dup_for_data_w_bank && update_data_dup_for_data_w_bank
    io.data_write_dup(i).bits.way_en := RegEnable(s2_way_en, s2_fire_to_s3)
    io.data_write_dup(i).bits.addr := RegEnable(s2_req.vaddr, s2_fire_to_s3)
  }
  // -------------------------------------------------------------------------------------

  // ---------------- duplicate regs for wb.valid to solve fanout ----------------
  val s3_req_miss_dup_for_wb_valid = RegEnable(s2_req.miss, s2_fire_to_s3)
  val s3_req_probe_dup_for_wb_valid = RegEnable(s2_req.probe, s2_fire_to_s3)
  val s3_tag_match_dup_for_wb_valid = RegEnable(s2_tag_match, s2_fire_to_s3)
  val s3_coh_dup_for_wb_valid = RegEnable(s2_coh, s2_fire_to_s3)
  val s3_req_probe_param_dup_for_wb_valid = RegEnable(s2_req.probe_param, s2_fire_to_s3)
  val (_, _, probe_new_coh_dup_for_wb_valid) = s3_coh_dup_for_wb_valid.onProbe(s3_req_probe_param_dup_for_wb_valid)
  val s3_req_source_dup_for_wb_valid = RegEnable(s2_req.source, s2_fire_to_s3)
  val s3_req_cmd_dup_for_wb_valid = RegEnable(s2_req.cmd, s2_fire_to_s3)
  val s3_req_replace_dup_for_wb_valid = RegEnable(s2_req.replace, s2_fire_to_s3)
  val s3_hit_coh_dup_for_wb_valid = RegEnable(s2_hit_coh, s2_fire_to_s3)
  val s3_new_hit_coh_dup_for_wb_valid = RegEnable(s2_new_hit_coh, s2_fire_to_s3)
  
  val miss_update_meta_dup_for_wb_valid = s3_req_miss_dup_for_wb_valid
  val probe_update_meta_dup_for_wb_valid = s3_req_probe_dup_for_wb_valid && s3_tag_match_dup_for_wb_valid && s3_coh_dup_for_wb_valid =/= probe_new_coh_dup_for_wb_valid
  val store_update_meta_dup_for_wb_valid = s3_req_source_dup_for_wb_valid === STORE_SOURCE.U &&
    !s3_req_probe_dup_for_wb_valid &&
    s3_hit_coh_dup_for_wb_valid =/= s3_new_hit_coh_dup_for_wb_valid
  val amo_update_meta_dup_for_wb_valid = s3_req_source_dup_for_wb_valid === AMO_SOURCE.U &&
    !s3_req_probe_dup_for_wb_valid &&
    s3_hit_coh_dup_for_wb_valid =/= s3_new_hit_coh_dup_for_wb_valid
  val update_meta_dup_for_wb_valid = (
    miss_update_meta_dup_for_wb_valid ||
    probe_update_meta_dup_for_wb_valid ||
    store_update_meta_dup_for_wb_valid ||
    amo_update_meta_dup_for_wb_valid
  ) && !s3_req_replace_dup_for_wb_valid

  val s3_valid_dup_for_wb_valid = RegInit(false.B)
  val s3_amo_hit_dup_for_wb_valid = RegEnable(s2_amo_hit, s2_fire_to_s3)
  val s3_s_amoalu_dup_for_wb_valid = RegInit(false.B)
  val amo_wait_amoalu_dup_for_wb_valid = s3_req_source_dup_for_wb_valid === AMO_SOURCE.U &&
    s3_req_cmd_dup_for_wb_valid =/= M_XLR &&
    s3_req_cmd_dup_for_wb_valid =/= M_XSC
  val do_amoalu_dup_for_wb_valid = amo_wait_amoalu_dup_for_wb_valid && s3_valid_dup_for_wb_valid && !s3_s_amoalu_dup_for_wb_valid

  val s3_store_hit_dup_for_wb_valid = RegEnable(s2_store_hit, s2_fire_to_s3)
  val s3_req_addr_dup_for_wb_valid = RegEnable(s2_req.addr, s2_fire_to_s3)
  val s3_can_do_amo_dup_for_wb_valid = (s3_req_miss_dup_for_wb_valid && !s3_req_probe_dup_for_wb_valid && s3_req_source_dup_for_wb_valid === AMO_SOURCE.U) ||
    s3_amo_hit_dup_for_wb_valid

  val s3_lr_dup_for_wb_valid = !s3_req_probe_dup_for_wb_valid && s3_req_source_dup_for_wb_valid === AMO_SOURCE.U && s3_req_cmd_dup_for_wb_valid === M_XLR
  val s3_sc_dup_for_wb_valid = !s3_req_probe_dup_for_wb_valid && s3_req_source_dup_for_wb_valid === AMO_SOURCE.U && s3_req_cmd_dup_for_wb_valid === M_XSC
  val lrsc_addr_dup_for_wb_valid = Reg(UInt())
  val lrsc_count_dup_for_wb_valid = RegInit(0.U(log2Ceil(LRSCCycles).W))

  when (s3_valid_dup_for_wb_valid && (s3_lr_dup_for_wb_valid || s3_sc_dup_for_wb_valid)) {
    when (s3_can_do_amo_dup_for_wb_valid && s3_lr_dup_for_wb_valid) {
      lrsc_count_dup_for_wb_valid := (LRSCCycles - 1).U
      lrsc_addr_dup_for_wb_valid := get_block_addr(s3_req_addr_dup_for_wb_valid)
    }.otherwise {
      lrsc_count_dup_for_wb_valid := 0.U
    }
  }.elsewhen (io.invalid_resv_set) {
    lrsc_count_dup_for_wb_valid := 0.U
  }.elsewhen (lrsc_count_dup_for_wb_valid > 0.U) {
    lrsc_count_dup_for_wb_valid := lrsc_count_dup_for_wb_valid - 1.U
  }

  val lrsc_valid_dup_for_wb_valid = lrsc_count_dup_for_wb_valid > LRSCBackOff.U
  val s3_lrsc_addr_match_dup_for_wb_valid = lrsc_valid_dup_for_wb_valid && lrsc_addr_dup_for_wb_valid === get_block_addr(s3_req_addr_dup_for_wb_valid)
  val s3_sc_fail_dup_for_wb_valid = s3_sc_dup_for_wb_valid && !s3_lrsc_addr_match_dup_for_wb_valid
  val s3_can_do_amo_write_dup_for_wb_valid = s3_can_do_amo_dup_for_wb_valid && isWrite(s3_req_cmd_dup_for_wb_valid) && !s3_sc_fail_dup_for_wb_valid
  val update_data_dup_for_wb_valid = s3_req_miss_dup_for_wb_valid || s3_store_hit_dup_for_wb_valid || s3_can_do_amo_write_dup_for_wb_valid

  val s3_probe_can_go_dup_for_wb_valid = s3_req_probe_dup_for_wb_valid &&
    io.wb_ready_dup(wbPort) &&
    (io.meta_write.ready || !probe_update_meta_dup_for_wb_valid)
  val s3_store_can_go_dup_for_wb_valid = s3_req_source_dup_for_wb_valid === STORE_SOURCE.U && !s3_req_probe_dup_for_wb_valid &&
    (io.meta_write.ready || !store_update_meta_dup_for_wb_valid) &&
    (io.data_write_ready_dup(wbPort) || !update_data_dup_for_wb_valid)
  val s3_amo_can_go_dup_for_wb_valid = s3_amo_hit_dup_for_wb_valid &&
    (io.meta_write.ready || !amo_update_meta_dup_for_wb_valid) &&
    (io.data_write_ready_dup(wbPort) || !update_data_dup_for_wb_valid) &&
    (s3_s_amoalu_dup_for_wb_valid || !amo_wait_amoalu_dup_for_wb_valid)
  val s3_miss_can_go_dup_for_wb_valid = s3_req_miss_dup_for_wb_valid &&
    (io.meta_write.ready || !amo_update_meta_dup_for_wb_valid) &&
    (io.data_write_ready_dup(wbPort) || !update_data_dup_for_wb_valid) &&
    (s3_s_amoalu_dup_for_wb_valid || !amo_wait_amoalu_dup_for_wb_valid) &&
    io.tag_write_ready_dup(wbPort) &&
    io.wb_ready_dup(wbPort)
  val s3_replace_can_go_dup_for_wb_valid = s3_req_replace_dup_for_wb_valid &&
    (s3_coh_dup_for_wb_valid.state === ClientStates.Nothing || io.wb_ready_dup(wbPort))
  val s3_can_go_dup_for_wb_valid = s3_probe_can_go_dup_for_wb_valid ||
    s3_store_can_go_dup_for_wb_valid ||
    s3_amo_can_go_dup_for_wb_valid ||
    s3_miss_can_go_dup_for_wb_valid ||
    s3_replace_can_go_dup_for_wb_valid
  val s3_update_data_cango_dup_for_wb_valid = s3_store_can_go_dup_for_wb_valid || s3_amo_can_go_dup_for_wb_valid || s3_miss_can_go_dup_for_wb_valid

  val s3_fire_dup_for_wb_valid = s3_valid_dup_for_wb_valid && s3_can_go_dup_for_wb_valid
  when (do_amoalu_dup_for_wb_valid) { s3_s_amoalu_dup_for_wb_valid := true.B }
  when (s3_fire_dup_for_wb_valid) { s3_s_amoalu_dup_for_wb_valid := false.B }

  val s3_banked_store_wmask_dup_for_wb_valid = RegEnable(s2_banked_store_wmask, s2_fire_to_s3)
  val s3_req_word_idx_dup_for_wb_valid = RegEnable(s2_req.word_idx, s2_fire_to_s3)
  val s3_replace_nothing_dup_for_wb_valid = s3_req_replace_dup_for_wb_valid && s3_coh_dup_for_wb_valid.state === ClientStates.Nothing

  val s3_sc_data_merged_dup_for_wb_valid = Wire(Vec(DCacheBanks, UInt(DCacheSRAMRowBits.W)))
  val s3_req_amo_data_dup_for_wb_valid = RegEnable(s2_req.amo_data, s2_fire_to_s3)
  val s3_req_amo_mask_dup_for_wb_valid = RegEnable(s2_req.amo_mask, s2_fire_to_s3)
  for (i <- 0 until DCacheBanks) {
    val old_data = s3_store_data_merged(i)
    s3_sc_data_merged_dup_for_wb_valid(i) := mergePutData(old_data, s3_req_amo_data_dup_for_wb_valid,
      Mux(
        s3_req_word_idx_dup_for_wb_valid === i.U && !s3_sc_fail_dup_for_wb_valid,
        s3_req_amo_mask_dup_for_wb_valid,
        0.U(wordBytes.W)
      )
    )
  }

  val s3_need_replacement_dup_for_wb_valid = RegEnable(s2_need_replacement, s2_fire_to_s3)
  val miss_wb_dup_for_wb_valid = s3_req_miss_dup_for_wb_valid && s3_need_replacement_dup_for_wb_valid &&
    s3_coh_dup_for_wb_valid.state =/= ClientStates.Nothing
  val need_wb_dup_for_wb_valid = miss_wb_dup_for_wb_valid || s3_req_probe_dup_for_wb_valid || s3_req_replace_dup_for_wb_valid

  val s3_tag_dup_for_wb_valid = RegEnable(s2_tag, s2_fire_to_s3)

  val (_, probe_shrink_param_dup_for_wb_valid, _) = s3_coh_dup_for_wb_valid.onProbe(s3_req_probe_param_dup_for_wb_valid)
  val (_, miss_shrink_param_dup_for_wb_valid, _) = s3_coh_dup_for_wb_valid.onCacheControl(M_FLUSH)
  val writeback_param_dup_for_wb_valid = Mux(
    s3_req_probe_dup_for_wb_valid,
    probe_shrink_param_dup_for_wb_valid,
    miss_shrink_param_dup_for_wb_valid
  )
  val writeback_data_dup_for_wb_valid = if (dcacheParameters.alwaysReleaseData) {
    s3_tag_match_dup_for_wb_valid && s3_req_probe_dup_for_wb_valid && RegEnable(s2_req.probe_need_data, s2_fire_to_s3) ||
      s3_coh_dup_for_wb_valid === ClientStates.Dirty || (miss_wb_dup_for_wb_valid || s3_req_replace_dup_for_wb_valid) && s3_coh_dup_for_wb_valid.state =/= ClientStates.Nothing
  } else {
    s3_tag_match_dup_for_wb_valid && s3_req_probe_dup_for_wb_valid && RegEnable(s2_req.probe_need_data, s2_fire_to_s3) || s3_coh_dup_for_wb_valid === ClientStates.Dirty
  }

  when (s2_fire_to_s3) { s3_valid_dup_for_wb_valid := true.B }
  .elsewhen (s3_fire_dup_for_wb_valid) { s3_valid_dup_for_wb_valid := false.B }
  
  // -------------------------------------------------------------------------------------

  val s3_fire = s3_valid_dup(4) && s3_can_go
  when (s2_fire_to_s3) {
    s3_valid := true.B
    s3_valid_dup.foreach(_ := true.B)
    s3_valid_dup_for_status.foreach(_ := true.B)
  }.elsewhen (s3_fire) {
    s3_valid := false.B
    s3_valid_dup.foreach(_ := false.B)
    s3_valid_dup_for_status.foreach(_ := false.B)
  }
  s3_ready := !s3_valid_dup(5) || s3_can_go
  s3_s0_set_conflict := s3_valid_dup(6) && s3_idx_dup(0) === s0_idx
  s3_s0_set_conflict_store := s3_valid_dup(7) && s3_idx_dup(1) === store_idx
  assert(RegNext(!s3_valid || !(s3_req_source_dup_2 === STORE_SOURCE.U && !s3_req.probe) || s3_hit)) // miss store should never come to s3

  when(s3_fire) {
    s3_s_amoalu := false.B
    s3_s_amoalu_dup.foreach(_ := false.B)
  }

  req.ready := s0_can_go

  io.meta_read.valid := req.valid && s1_ready && !set_conflict
  io.meta_read.bits.idx := get_idx(s0_req.vaddr)
  io.meta_read.bits.way_en := Mux(s0_req.replace, s0_req.replace_way_en, ~0.U(nWays.W))

  io.tag_read.valid := req.valid && s1_ready && !set_conflict && !s0_req.replace
  io.tag_read.bits.idx := get_idx(s0_req.vaddr)
  io.tag_read.bits.way_en := ~0.U(nWays.W)

  io.data_read_intend := s1_valid_dup(3) && s1_need_data
  io.data_read.valid := s1_valid_dup(4) && s1_need_data
  io.data_read.bits.rmask := s1_banked_rmask
  io.data_read.bits.way_en := s1_way_en
  io.data_read.bits.addr := s1_req_vaddr_dup_for_data_read

  io.miss_req.valid := s2_valid_dup(4) && s2_can_go_to_mq_dup(0)
  val miss_req = io.miss_req.bits
  miss_req := DontCare
  miss_req.source := s2_req.source
  miss_req.cmd := s2_req.cmd
  miss_req.addr := s2_req.addr
  miss_req.vaddr := s2_req_vaddr_dup_for_miss_req
  miss_req.way_en := Mux(s2_tag_match, s2_tag_match_way, s2_repl_way_en)
  miss_req.store_data := s2_req.store_data
  miss_req.store_mask := s2_req.store_mask
  miss_req.word_idx := s2_req.word_idx
  miss_req.amo_data := s2_req.amo_data
  miss_req.amo_mask := s2_req.amo_mask
  miss_req.req_coh := s2_hit_coh
  miss_req.replace_coh := s2_repl_coh
  miss_req.replace_tag := s2_repl_tag
  miss_req.id := s2_req.id
  miss_req.cancel := false.B
  miss_req.pc := DontCare

  io.store_replay_resp.valid := s2_valid_dup(5) && s2_can_go_to_mq_dup(1) && replay && s2_req.isStore
  io.store_replay_resp.bits.data := DontCare
  io.store_replay_resp.bits.miss := true.B
  io.store_replay_resp.bits.replay := true.B
  io.store_replay_resp.bits.id := s2_req.id

  io.store_hit_resp.valid := s3_valid_dup(8) && s3_store_can_go
  io.store_hit_resp.bits.data := DontCare
  io.store_hit_resp.bits.miss := false.B
  io.store_hit_resp.bits.replay := false.B
  io.store_hit_resp.bits.id := s3_req.id

  io.release_update.valid := s3_valid_dup(9) && (s3_store_can_go || s3_amo_can_go) && s3_hit && update_data
  io.release_update.bits.addr := s3_req_addr_dup(3)
  io.release_update.bits.mask := Mux(s3_store_hit_dup(1), s3_banked_store_wmask, banked_amo_wmask)
  io.release_update.bits.data := Mux(
    amo_wait_amoalu, 
    s3_amo_data_merged_reg, 
    Mux(
      s3_sc,
      s3_sc_data_merged,
      s3_store_data_merged
    )
  ).asUInt

  val atomic_hit_resp = Wire(new AtomicsResp)
  atomic_hit_resp.data := Mux(s3_sc, s3_sc_resp, s3_data_word)
  atomic_hit_resp.miss := false.B
  atomic_hit_resp.miss_id := s3_req.miss_id
  atomic_hit_resp.error := s3_error
  atomic_hit_resp.replay := false.B
  atomic_hit_resp.ack_miss_queue := s3_req_miss_dup(5)
  atomic_hit_resp.id := lrsc_valid_dup(2)
  val atomic_replay_resp = Wire(new AtomicsResp)
  atomic_replay_resp.data := DontCare
  atomic_replay_resp.miss := true.B
  atomic_replay_resp.miss_id := DontCare
  atomic_replay_resp.error := false.B
  atomic_replay_resp.replay := true.B
  atomic_replay_resp.ack_miss_queue := false.B
  atomic_replay_resp.id := DontCare
  val atomic_replay_resp_valid = s2_valid_dup(6) && s2_can_go_to_mq_dup(2) && replay && s2_req.isAMO
  val atomic_hit_resp_valid = s3_valid_dup(10) && (s3_amo_can_go || s3_miss_can_go && s3_req.isAMO)
  io.atomic_resp.valid := atomic_replay_resp_valid || atomic_hit_resp_valid
  io.atomic_resp.bits := Mux(atomic_replay_resp_valid, atomic_replay_resp, atomic_hit_resp)

  io.replace_resp.valid := s3_fire && s3_req_replace_dup(3)
  io.replace_resp.bits := s3_req.miss_id

  io.meta_write.valid := s3_fire_dup_for_meta_w_valid && update_meta_dup_for_meta_w_valid
  io.meta_write.bits.idx := s3_idx_dup(2)
  io.meta_write.bits.way_en := s3_way_en_dup(0)
  io.meta_write.bits.meta.coh := new_coh

  io.error_flag_write.valid := s3_fire_dup_for_err_w_valid && update_meta_dup_for_err_w_valid && s3_l2_error
  io.error_flag_write.bits.idx := s3_idx_dup(3)
  io.error_flag_write.bits.way_en := s3_way_en_dup(1)
  io.error_flag_write.bits.flag := s3_l2_error

  // if we use (prefetch_flag && meta =/= ClientStates.Nothing) for prefetch check
  // prefetch_flag_write can be omited
  // io.prefetch_flag_write.valid := io.meta_write.valid && new_coh === ClientStates.Nothing
  // io.prefetch_flag_write.bits.idx := s3_idx_dup(3)
  // io.prefetch_flag_write.bits.way_en := s3_way_en_dup(1)
  // io.prefetch_flag_write.bits.flag := false.B
  io.prefetch_flag_write.valid := false.B
  io.prefetch_flag_write.bits := DontCare

  // probe / replace will not update access bit
  io.access_flag_write.valid := s3_fire_dup_for_meta_w_valid && !s3_req.probe && !s3_req.replace
  io.access_flag_write.bits.idx := s3_idx_dup(3)
  io.access_flag_write.bits.way_en := s3_way_en_dup(1)
  io.access_flag_write.bits.flag := true.B

  io.tag_write.valid := s3_fire_dup_for_tag_w_valid && s3_req_miss_dup_for_tag_w_valid
  io.tag_write.bits.idx := s3_idx_dup(4)
  io.tag_write.bits.way_en := s3_way_en_dup(2)
  io.tag_write.bits.tag := get_tag(s3_req_addr_dup(4))
  io.tag_write.bits.vaddr := s3_req_vaddr_dup_for_data_write

  io.tag_write_intend := s3_req_miss_dup(7) && s3_valid_dup(11)
  XSPerfAccumulate("fake_tag_write_intend", io.tag_write_intend && !io.tag_write.valid)
  XSPerfAccumulate("mainpipe_tag_write", io.tag_write.valid)

  assert(!RegNext(io.tag_write.valid && !io.tag_write_intend))

  io.data_write.valid := s3_valid_dup_for_data_w_valid && s3_update_data_cango_dup_for_data_w_valid && update_data_dup_for_data_w_valid
  io.data_write.bits.way_en := s3_way_en_dup(3)
  io.data_write.bits.addr := s3_req_vaddr_dup_for_data_write
  io.data_write.bits.wmask := banked_wmask
  io.data_write.bits.data := Mux(
    amo_wait_amoalu_dup_for_data_w_valid, 
    s3_amo_data_merged_reg, 
    Mux(
      s3_sc_dup_for_data_w_valid,
      s3_sc_data_merged_dup_for_data_w_valid,
      s3_store_data_merged
    )
  )
  assert(RegNext(!io.meta_write.valid || !s3_req.replace))
  assert(RegNext(!io.tag_write.valid || !s3_req.replace))
  assert(RegNext(!io.data_write.valid || !s3_req.replace))

  io.wb.valid := s3_valid_dup_for_wb_valid && (
    // replace
    s3_req_replace_dup_for_wb_valid && !s3_replace_nothing_dup_for_wb_valid ||
    // probe can go to wbq
    s3_req_probe_dup_for_wb_valid && (io.meta_write.ready || !probe_update_meta_dup_for_wb_valid) ||
      // amo miss can go to wbq
      s3_req_miss_dup_for_wb_valid &&
        (io.meta_write.ready || !amo_update_meta_dup_for_wb_valid) &&
        (io.data_write_ready_dup(wbPort) || !update_data_dup_for_wb_valid) &&
        (s3_s_amoalu_dup_for_wb_valid || !amo_wait_amoalu_dup_for_wb_valid) &&
        io.tag_write_ready_dup(wbPort)
    ) && need_wb_dup_for_wb_valid

  io.wb.bits.addr := get_block_addr(Cat(s3_tag_dup_for_wb_valid, get_untag(s3_req.vaddr)))
  io.wb.bits.param := writeback_param_dup_for_wb_valid
  io.wb.bits.voluntary := s3_req_miss_dup_for_wb_valid || s3_req_replace_dup_for_wb_valid
  io.wb.bits.hasData := writeback_data_dup_for_wb_valid
  io.wb.bits.dirty := s3_coh_dup_for_wb_valid === ClientStates.Dirty
  io.wb.bits.data := s3_data.asUInt()
  io.wb.bits.delay_release := s3_req_replace_dup_for_wb_valid
  io.wb.bits.miss_id := s3_req.miss_id

  // update plru in main pipe s3
  if (!cfg.updateReplaceOn2ndmiss) {
  // replacement is only updated on 1st miss
    io.replace_access.valid := RegNext(
      // generated in mainpipe s1
      RegNext(s1_fire && (s1_req.isAMO || s1_req.isStore) && !s1_req.probe) &&
      // generated in mainpipe s2
      Mux(
        io.miss_req.valid, 
        !io.miss_resp.merged && io.miss_req.ready, // if store miss, only update plru for the first miss
        true.B // normal store access
      )
    )
    io.replace_access.bits.set := RegNext(s2_idx_dup_for_replace_access)
    io.replace_access.bits.way := RegNext(RegNext(OHToUInt(s1_way_en)))
  } else {
    // replacement is updated on both 1st and 2nd miss
    // timing is worse than !cfg.updateReplaceOn2ndmiss
    io.replace_access.valid := RegNext(
      // generated in mainpipe s1
      RegNext(s1_fire && (s1_req.isAMO || s1_req.isStore) && !s1_req.probe) &&
      // generated in mainpipe s2
      Mux(
        io.miss_req.valid, 
        io.miss_req.ready, // if store miss, do not update plru if that req needs to be replayed
        true.B // normal store access
      )
    )
    io.replace_access.bits.set := RegNext(s2_idx_dup_for_replace_access)
    io.replace_access.bits.way := RegNext(
      Mux(
        io.miss_req.valid && io.miss_resp.merged,
        // miss queue 2nd fire: access replace way selected at miss queue allocate time
        OHToUInt(io.miss_resp.repl_way_en),
        // new selected replace way or hit way
        RegNext(OHToUInt(s1_way_en))
      )
    )
  }

  io.replace_way.set.valid := RegNext(s0_fire)
  io.replace_way.set.bits := s1_idx_dup_for_replace_way
  io.replace_way.dmWay := s1_dmWay_dup_for_replace_way

  // TODO: consider block policy of a finer granularity
  io.status.s0_set.valid := req.valid
  io.status.s0_set.bits := get_idx(s0_req.vaddr)
  io.status.s1.valid := s1_valid_dup(5)
  io.status.s1.bits.set := s1_idx
  io.status.s1.bits.way_en := s1_way_en
  io.status.s2.valid := s2_valid_dup(7) && !s2_req_replace_dup_2
  io.status.s2.bits.set := s2_idx_dup_for_status
  io.status.s2.bits.way_en := s2_way_en
  io.status.s3.valid := s3_valid && !s3_req_replace_dup(7)
  io.status.s3.bits.set := s3_idx_dup(5)
  io.status.s3.bits.way_en := s3_way_en

  for ((s, i) <- io.status_dup.zipWithIndex) {
    s.s1.valid := s1_valid_dup_for_status(i)
    s.s1.bits.set := RegEnable(get_idx(s0_req.vaddr), s0_fire)
    s.s1.bits.way_en := s1_way_en
    s.s2.valid := s2_valid_dup_for_status(i) && !RegEnable(s1_req.replace, s1_fire)
    s.s2.bits.set := RegEnable(get_idx(s1_req.vaddr), s1_fire)
    s.s2.bits.way_en := RegEnable(s1_way_en, s1_fire)
    s.s3.valid := s3_valid_dup_for_status(i) && !RegEnable(s2_req.replace, s2_fire_to_s3)
    s.s3.bits.set := RegEnable(get_idx(s2_req.vaddr), s2_fire_to_s3)
    s.s3.bits.way_en := RegEnable(s2_way_en, s2_fire_to_s3)
  }
  dontTouch(io.status_dup)

  // report error to beu and csr, 1 cycle after read data resp
  io.error := 0.U.asTypeOf(new L1CacheErrorInfo())
  // report error, update error csr
  io.error.valid := s3_error && RegNext(s2_fire)
  // only tag_error and data_error will be reported to beu
  // l2_error should not be reported (l2 will report that) 
  io.error.report_to_beu := (RegEnable(s2_tag_error, s2_fire) || s3_data_error) && RegNext(s2_fire)
  io.error.paddr := RegEnable(s2_req.addr, s2_fire)
  io.error.source.tag := RegEnable(s2_tag_error, s2_fire)
  io.error.source.data := s3_data_error
  io.error.source.l2 := RegEnable(s2_flag_error || s2_l2_error, s2_fire)
  io.error.opType.store := RegEnable(s2_req.isStore && !s2_req.probe, s2_fire)
  io.error.opType.probe := RegEnable(s2_req.probe, s2_fire)
  io.error.opType.release := RegEnable(s2_req.replace, s2_fire)
  io.error.opType.atom := RegEnable(s2_req.isAMO && !s2_req.probe, s2_fire)

  val perfEvents = Seq(
    ("dcache_mp_req          ", s0_fire                                                      ),
    ("dcache_mp_total_penalty", PopCount(VecInit(Seq(s0_fire, s1_valid, s2_valid, s3_valid))))
  )
  generatePerfEvent()
}
