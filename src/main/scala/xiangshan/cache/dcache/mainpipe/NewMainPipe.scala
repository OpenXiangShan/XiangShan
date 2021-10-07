package xiangshan.cache

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.internal.firrtl.Port
import chisel3.util._
import freechips.rocketchip.tilelink.ClientStates._
import freechips.rocketchip.tilelink.MemoryOpCategories._
import freechips.rocketchip.tilelink.TLPermissions._
import freechips.rocketchip.tilelink.{ClientMetadata, ClientStates, TLPermissions}
import utils._

class NewMainPipeReq(implicit p: Parameters) extends DCacheBundle {
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

  val id = UInt(reqIdWidth.W)

  def isLoad: Bool = source === LOAD_SOURCE.U
  def isStore: Bool = source === STORE_SOURCE.U

  def convertStoreReq(store: DecoupledIO[DCacheLineReq]): DecoupledIO[NewMainPipeReq] = {
    val req = Wire(DecoupledIO(new NewMainPipeReq))
    req.valid := store.valid
    req.bits := DontCare
    req.bits.probe := false.B
    req.bits.probe_need_data := false.B
    req.bits.source := STORE_SOURCE.U
    req.bits.cmd := store.bits.cmd
    req.bits.addr := store.bits.addr
    req.bits.vaddr := store.bits.vaddr
    req.bits.store_data := store.bits.data
    req.bits.store_mask := store.bits.mask
    req.bits.id := store.bits.id
    store.ready := req.ready
    req
  }
}

class NewMainPipe(implicit p: Parameters) extends DCacheModule {
  val metaBits = (new Meta).getWidth
  val encMetaBits = cacheParams.tagCode.width((new MetaAndTag).getWidth) - tagBits

  val io = IO(new Bundle() {
    // probe queue
    val probe_req = Flipped(DecoupledIO(new NewMainPipeReq))
    // store miss go to miss queue
    val miss = DecoupledIO(new NewMissReq)
    // store buffer
    val store_req = Flipped(DecoupledIO(new DCacheLineReq))
    val store_replay_resp = ValidIO(new DCacheLineResp)
    val store_hit_resp = ValidIO(new DCacheLineResp)
    // write-back queue
    val wb = DecoupledIO(new WritebackReq)

    val data_read = DecoupledIO(new L1BankedDataReadLineReq)
    val data_resp = Input(Vec(DCacheBanks, new L1BankedDataReadResult()))
    val data_write = DecoupledIO(new L1BankedDataWriteReq)

    val meta_read = DecoupledIO(new MetaReadReq)
    val meta_resp = Input(Vec(nWays, UInt(encMetaBits.W)))
    val meta_write = DecoupledIO(new MetaWriteReq)

    val tag_read = DecoupledIO(new TagReadReq)
    val tag_resp = Input(Vec(nWays, UInt(tagBits.W)))
//    val tag_write = DecoupledIO(new TagWriteReq)

    // update state vec in replacement algo
    val replace_access = ValidIO(new ReplacementAccessBundle)
    // find the way to be replaced
    val replace_way = new ReplacementWayReqIO

    val status = new Bundle() {
      val s0_set = ValidIO(UInt(idxBits.W))
      val s1, s2, s3 = ValidIO(new Bundle() {
        val set = UInt(idxBits.W)
        val way_en = UInt(nWays.W)
      })
    }

    // load fast wakeup should be disabled when data read is not ready
    val disable_ld_fast_wakeup = Output(Vec(LoadPipelineWidth, Bool()))
  })

  // meta array is made of regs, so meta write or read should always be ready
  assert(RegNext(io.meta_read.ready))
  assert(RegNext(io.meta_write.ready))

  val s1_s0_set_conflict, s2_s0_set_conlict, s3_s0_set_conflict = Wire(Bool())
  val set_conflict = s1_s0_set_conflict || s2_s0_set_conlict || s3_s0_set_conflict
  val s1_ready, s2_ready, s3_ready = Wire(Bool())

  // convert store req to main pipe req, and select a req from store and probe
  val store_req = Wire(DecoupledIO(new NewMainPipeReq))
  store_req <> (new NewMainPipeReq).convertStoreReq(io.store_req)
  val req_arb = Module(new Arbiter(new NewMainPipeReq, 2))
  req_arb.io.in(0) <> store_req
  req_arb.io.in(1) <> io.probe_req

  // s0: read meta and tag
  val req = Wire(DecoupledIO(new NewMainPipeReq))
  req <> req_arb.io.out
  val s0_req = req.bits
  val s0_idx = get_idx(s0_req.vaddr)
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

  val banked_need_data = store_need_data || probe_need_data

  val s0_banked_rmask = Mux(store_need_data, banked_store_rmask,
    Mux(probe_need_data,
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
  val s1_can_go = s2_ready && (io.data_read.ready || !s1_need_data)
  val s1_fire = s1_valid && s1_can_go
  val s1_idx = get_idx(s1_req.vaddr)
  val s1_tag = get_tag(s1_req.addr)
  when (s0_fire) {
    s1_valid := true.B
  }.elsewhen (s1_fire) {
    s1_valid := false.B
  }
  s1_ready := !s1_valid || s1_can_go
  s1_s0_set_conflict := s1_valid && s0_idx === s1_idx

  def getMeta(encMeta: UInt): UInt = {
    require(encMeta.getWidth == encMetaBits)
    encMeta(metaBits - 1, 0)
  }

  val tag_resp = Wire(Vec(nWays, UInt(tagBits.W)))
  val ecc_meta_resp = Wire(Vec(nWays, UInt(encMetaBits.W)))
  tag_resp := Mux(RegNext(s0_fire), io.tag_resp, RegNext(tag_resp))
  ecc_meta_resp := Mux(RegNext(s0_fire), io.meta_resp, RegNext(ecc_meta_resp))
  val meta_resp = ecc_meta_resp.map(getMeta(_))

  def wayMap[T <: Data](f: Int => T) = VecInit((0 until nWays).map(f))
  val s1_tag_eq_way = wayMap((w: Int) => tag_resp(w) === s1_tag).asUInt
  val s1_tag_match_way = wayMap((w: Int) => s1_tag_eq_way(w) && Meta(meta_resp(w)).coh.isValid()).asUInt
  val s1_tag_match = s1_tag_match_way.orR

  val s1_hit_tag = Mux(s1_tag_match, Mux1H(s1_tag_match_way, wayMap(w => tag_resp(w))), get_tag(s1_req.addr))
  val s1_hit_coh = ClientMetadata(Mux(s1_tag_match, Mux1H(s1_tag_match_way, wayMap(w => meta_resp(w))), 0.U))

  // replacement policy
//  val replacer = ReplacementPolicy.fromString(cacheParams.replacer, nWays, nSets)
  val s1_repl_way_en = WireInit(0.U(nWays.W))
  s1_repl_way_en := Mux(RegNext(s0_fire), UIntToOH(io.replace_way.way), RegNext(s1_repl_way_en))
  val s1_repl_tag = Mux1H(s1_repl_way_en, wayMap(w => tag_resp(w)))
  val s1_repl_coh = Mux1H(s1_repl_way_en, wayMap(w => meta_resp(w))).asTypeOf(new ClientMetadata)

  val s1_need_replacement = s1_req.isStore && !s1_req.probe && !s1_tag_match
  val s1_way_en = Mux(s1_need_replacement, s1_repl_way_en, s1_tag_match_way)
  val s1_tag = Mux(s1_need_replacement, s1_repl_tag, s1_hit_tag)
  val s1_coh = Mux(s1_need_replacement, s1_repl_coh, s1_hit_coh)

  // s2: select data, return resp if this is a store miss
  val s2_valid = RegInit(false.B)
  val s2_req = RegEnable(s1_req, s1_fire)
  val s2_tag_match = RegEnable(s1_tag_match, s1_fire)
  val s2_hit_coh = RegEnable(s1_hit_coh, s1_fire)
  val (s2_has_permission, _, s2_new_hit_coh) = s2_hit_coh.onAccess(s2_req.cmd)
  val s2_repl_way_en = RegEnable(s1_repl_way_en, s1_fire)
  val s2_repl_tag = RegEnable(s1_repl_tag, s1_fire)
  val s2_repl_coh = RegEnable(s1_repl_coh, s1_fire)
  val s2_need_replacement = RegEnable(s1_need_replacement, s1_fire)
  val s2_idx = get_idx(s2_req.vaddr)
  val s2_way_en = RegEnable(s1_way_en, s1_fire)
  val s2_tag = RegEnable(s1_tag, s1_fire)
  val s2_coh = RegEnable(s1_coh, s1_fire)
  val s2_banked_store_wmask = RegEnable(s1_banked_store_wmask, s1_fire)

  val s2_hit = s2_tag_match && s2_has_permission

  s2_s0_set_conlict := s2_valid && s0_idx === s2_idx

  // For a store req, it either hits and goes to s3, or miss and enter miss queue immediately
  val s2_can_go_to_s3 = (s2_req.probe || s2_req.isStore && s2_hit) && s3_ready
  val s2_can_go_to_mq = !s2_req.probe && s2_req.isStore && !s2_hit
  assert(RegNext(!(s2_valid && s2_can_go_to_s3 && s2_can_go_to_mq)))
  val s2_can_go = s2_can_go_to_s3 || s2_can_go_to_mq
  val s2_fire = s2_valid && s2_can_go
  val s2_fire_to_s3 = s2_valid && s2_can_go_to_s3
  when (s1_fire) {
    s2_valid := true.B
  }.elsewhen (s2_fire) {
    s2_valid := false.B
  }
  s2_ready := !s2_valid || s2_can_go
  val replay = !io.miss.ready

  val data_resp = Wire(io.data_resp.cloneType)
  data_resp := Mux(RegNext(s1_fire), io.data_resp, RegNext(data_resp))
  val s2_store_data_merged = Wire(Vec(DCacheBanks, UInt(DCacheSRAMRowBits.W)))

  def mergePutData(old_data: UInt, new_data: UInt, wmask: UInt): UInt = {
    val full_wmask = FillInterleaved(8, wmask)
    ((~full_wmask & old_data) | (full_wmask & new_data))
  }

  val s2_data = WireInit(VecInit((0 until DCacheBanks).map(i => {
    val decoded = cacheParams.dataCode.decode(data_resp(i).asECCData())
    // assert(!RegNext(s2_valid && s2_hit && decoded.uncorrectable))
    // TODO: trigger ecc error
    data_resp(i).raw_data
  })))

  for (i <- 0 until DCacheBanks) {
    val old_data = s2_data(i)
    val new_data = get_data_of_bank(i, s2_req.store_data)
    // for amo hit, we should use read out SRAM data
    // do not merge with store data
    val wmask = get_mask_of_bank(i, s2_req.store_mask)
    s2_store_data_merged(i) := mergePutData(old_data, new_data, wmask)
  }

  // s3: write data, meta and tag
  val s3_valid = RegInit(false.B)
  val s3_req = RegEnable(s2_req, s2_fire_to_s3)
  val s3_idx = get_idx(s3_req.vaddr)
  val s3_tag = RegEnable(s2_tag, s2_fire_to_s3)
  val s3_tag_match = RegEnable(s2_tag_match, s2_fire_to_s3)
  val s3_coh = RegEnable(s2_coh, s2_fire_to_s3)
  val s3_hit = RegEnable(s2_hit, s2_fire_to_s3)
  val s3_hit_coh = RegEnable(s2_hit_coh, s2_fire_to_s3)
  val s3_new_hit_coh = RegEnable(s2_new_hit_coh, s2_fire_to_s3)
  val s3_way_en = RegEnable(s2_way_en, s2_fire_to_s3)
  val s3_banked_store_wmask = RegEnable(s2_banked_store_wmask, s2_fire_to_s3)
  val s3_store_data_merged = RegEnable(s2_store_data_merged, s2_fire_to_s3)
  val s3_data = RegEnable(s2_data, s2_fire_to_s3)
  val (probe_has_dirty_data, probe_shrink_param, probe_new_coh) = s3_coh.onProbe(s3_req.probe_param)

  val probe_update_meta = s3_req.probe && s3_tag_match && s3_coh =/= probe_new_coh
  val store_update_meta = s3_req.isStore && !s3_req.probe && s3_hit_coh =/= s3_new_hit_coh

  val banked_wmask = s3_banked_store_wmask
  val update_data = banked_wmask.orR

  val writeback_data = s3_tag_match && s3_req.probe && (s3_req.probe_need_data || s3_coh === ClientStates.Dirty)

  val s3_probe_can_go = s3_req.probe && io.wb.ready && (io.meta_write.ready || !probe_update_meta)
  val s3_store_can_go = s3_req.isStore && !s3_req.probe && (io.meta_write.ready || !store_update_meta) && (io.data_write.ready || !update_data)
  val s3_can_go = s3_probe_can_go || s3_store_can_go
  val s3_fire = s3_valid && s3_can_go
  when (s2_fire_to_s3) {
    s3_valid := true.B
  }.elsewhen (s3_fire) {
    s3_valid := false.B
  }
  s3_ready := !s3_valid || s3_can_go
  s3_s0_set_conflict := s3_valid && s3_idx === s0_idx
  assert(RegNext(!s3_valid || !(s3_req.isStore && !s3_req.probe) || s3_hit)) // miss store should never come to s3


  req.ready := s0_can_go

  io.meta_read.valid := req.valid && s1_ready && !set_conflict
  io.meta_read.bits.idx := get_idx(s0_req.vaddr)
  io.meta_read.bits.way_en := ~0.U

  io.tag_read.valid := req.valid && s1_ready && !set_conflict
  io.tag_read.bits.idx := get_idx(s0_req.vaddr)
  io.tag_read.bits.way_en := ~0.U

  io.data_read.valid := s1_valid && s1_need_data && s2_ready
  io.data_read.bits.rmask := s1_banked_rmask
  io.data_read.bits.way_en := s1_way_en
  io.data_read.bits.addr := s1_req.vaddr

  io.miss.valid := s2_valid && s2_can_go_to_mq
  val miss = io.miss.bits
  miss := DontCare
  miss.source := s2_req.source
  miss.cmd := s2_req.cmd
  miss.addr := s2_req.addr
  miss.vaddr := s2_req.vaddr
  miss.way_en := s2_way_en
  miss.store_data := s2_req.store_data
  miss.store_mask := s2_req.store_mask
  miss.req_coh := s2_coh
  miss.replace_coh := s2_repl_coh
  miss.replace_tag := s2_repl_tag
  miss.id := s2_req.id

  io.store_replay_resp.valid := s2_valid && s2_can_go_to_mq && replay
  io.store_replay_resp.bits.data := DontCare
  io.store_replay_resp.bits.miss := true.B
  io.store_replay_resp.bits.replay := true.B
  io.store_replay_resp.bits.id := s2_req.id

  io.store_hit_resp.valid := s3_valid && s3_store_can_go
  io.store_hit_resp.bits.data := DontCare
  io.store_hit_resp.bits.miss := false.B
  io.store_hit_resp.bits.replay := false.B
  io.store_hit_resp.bits.id := s3_req.id

  io.meta_write.valid := s3_valid && (s3_req.isStore && !s3_req.probe && (io.data_write.ready || !update_data) && store_update_meta ||
    s3_req.probe && io.wb.ready && probe_update_meta)
  io.meta_write.bits.idx := s3_idx
  io.meta_write.bits.way_en := s3_way_en
  io.meta_write.bits.tag := s3_tag
  io.meta_write.bits.meta.coh := s3_new_hit_coh

  io.data_write.valid := s3_valid && s3_req.isStore && !s3_req.probe && update_data && (io.meta_write.ready || !store_update_meta)
  io.data_write.bits.way_en := s3_way_en
  io.data_write.bits.addr := s3_req.vaddr
  io.data_write.bits.wmask := banked_wmask
  io.data_write.bits.data := s3_store_data_merged

  io.wb.valid := s3_valid && s3_req.probe && (io.meta_write.ready || !probe_update_meta)
  io.wb.bits.addr := get_block_addr(Cat(s3_tag, get_untag(s3_req.vaddr)))
  io.wb.bits.param := probe_shrink_param
  io.wb.bits.voluntary := false.B
  io.wb.bits.hasData := writeback_data
  io.wb.bits.dirty := s3_coh === ClientStates.Dirty
  io.wb.bits.data := s3_data.asUInt()

  io.replace_access.valid := RegNext(s1_fire && s1_req.isStore && !s1_req.probe && s1_tag_match)
  io.replace_access.bits.set := s2_idx
  io.replace_access.bits.way := RegNext(OHToUInt(s1_way_en))

  io.replace_way.set.valid := RegNext(s0_fire)
  io.replace_way.set.bits := s1_idx

  // TODO: consider block policy of a finer granularity
  io.status.s0_set.valid := req.valid
  io.status.s0_set.bits := get_idx(s0_req.vaddr)
  io.status.s1.valid := s1_valid
  io.status.s1.bits.set := s1_idx
  io.status.s1.bits.way_en := s1_way_en
  io.status.s2.valid := s2_valid
  io.status.s2.bits.set := s2_idx
  io.status.s2.bits.way_en := s2_way_en
  io.status.s3.valid := s3_valid
  io.status.s3.bits.set := s3_idx
  io.status.s3.bits.way_en := s3_way_en
}
