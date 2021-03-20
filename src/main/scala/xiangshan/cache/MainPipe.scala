package xiangshan.cache

import chisel3._
import chisel3.util._
import utils._
import freechips.rocketchip.tilelink.{ClientMetadata, ClientStates, TLPermissions}

class MainPipeReq extends DCacheBundle
{
  // for request that comes from MissQueue
  // does this req come from MissQueue
  val miss = Bool()
  // which MissQueueEntry send this req?
  val miss_id = UInt(log2Up(cfg.nMissEntries).W)
  // what permission are we granted with?
  val miss_param = UInt(TLPermissions.bdWidth.W)

  // for request that comes from MissQueue
  // does this req come from Probe
  val probe = Bool()
  val probe_param = UInt(TLPermissions.bdWidth.W)

  // request info
  // reqs from MissQueue, Store, AMO use this
  // probe does not use this
  val source = UInt(sourceTypeWidth.W)
  val cmd    = UInt(M_SZ.W)
  // must be aligned to block
  val addr   = UInt(PAddrBits.W)

  // store
  val store_data   = UInt((cfg.blockBytes * 8).W)
  val store_mask   = UInt(cfg.blockBytes.W)

  // which word does amo work on?
  val word_idx = UInt(log2Up(cfg.blockBytes * 8 / DataBits).W)
  val amo_data   = UInt(DataBits.W)
  val amo_mask   = UInt((DataBits/8).W)

  val id     = UInt(reqIdWidth.W)

  def dump() = {
    XSDebug("MainPipeReq: miss: %b miss_id: %d miss_param: %d probe: %b probe_param: %d source: %d cmd: %d addr: %x store_data: %x store_mask: %x word_idx: %d data: %x mask: %x id: %d\n",
      miss, miss_id, miss_param, probe, probe_param, source, cmd, addr, store_data, store_mask, word_idx, amo_data, amo_mask, id)
  }
}

class MainPipeResp extends DCacheBundle
{
  val id     = UInt(reqIdWidth.W)
  // AMO resp data
  val data   = UInt(DataBits.W)
  val miss   = Bool()
  val replay = Bool()
  def dump() = {
    XSDebug("MainPipeResp: id: %d data: %x miss: %b replay: %b\n",
      id, data, miss, replay)
  }
}

class MainPipe extends DCacheModule {
  def metaBits = (new L1Metadata).getWidth
  def encMetaBits = cacheParams.tagCode.width(metaBits)

  val io = IO(new DCacheBundle {
    // req and resp
    val req        = Flipped(DecoupledIO(new MainPipeReq))
    val miss_req   = DecoupledIO(new MissReq)
    val miss_resp  = ValidIO(new MainPipeResp)
    val store_resp = ValidIO(new MainPipeResp)
    val amo_resp   = ValidIO(new MainPipeResp)

    // meta/data read/write
    val data_read  = DecoupledIO(new L1DataReadReq)
    val data_resp  = Input(Vec(blockRows, Bits(encRowBits.W)))
    val data_write = DecoupledIO(new L1DataWriteReq)

    val meta_read  = DecoupledIO(new L1MetaReadReq)
    val meta_resp  = Input(Vec(nWays, UInt(encMetaBits.W)))
    val meta_write = DecoupledIO(new L1MetaWriteReq)

    // write back
    val wb_req     = DecoupledIO(new WritebackReq)

    // lrsc locked block should block probe
    val lrsc_locked_block = Output(Valid(UInt(PAddrBits.W)))

    // update state vec in replacement algo
    val replace_access = Flipped(Vec(LoadPipelineWidth, ValidIO(new ReplacementAccessBundle)))

    // load fast wakeup should be disabled when data read is not ready
    val disable_ld_fast_wakeup = Output(Vec(LoadPipelineWidth, Bool()))
  })

  def getMeta(encMeta: UInt): UInt = {
    require(encMeta.getWidth == encMetaBits)
    encMeta(metaBits - 1, 0)
  }

  // assign default value to output signals
  io.req.ready := false.B
  io.miss_req.valid := false.B
  io.miss_req.bits := DontCare
  io.miss_resp.valid := false.B
  io.store_resp.valid := false.B
  io.amo_resp.valid := false.B

  io.data_read.valid := false.B
  io.data_write.valid := false.B
  io.data_write.bits := DontCare
  io.meta_read.valid := false.B
  io.meta_write.valid := false.B
  io.meta_write.bits := DontCare

  io.wb_req.valid := false.B
  io.wb_req.bits := DontCare

  io.lrsc_locked_block.valid := false.B
  io.lrsc_locked_block.bits := DontCare

  // Pipeline
  val s1_s0_set_conflict, s2_s0_set_conflict, s3_s0_set_conflict = Wire(Bool())
  val set_conflict = s1_s0_set_conflict || s2_s0_set_conflict || s3_s0_set_conflict
  val s1_ready, s2_ready, s3_ready = Wire(Bool())
  val s3_valid = RegInit(false.B)
  val update_meta, need_write_data = Wire(Bool())

  // --------------------------------------------------------------------------------
  // stage 0
  // read meta
  val s0_valid = io.req.valid
  val s0_fire = io.req.fire()
  val s0_req = io.req.bits

  val word_mask = Wire(Vec(blockRows, Vec(rowWords, Bits(wordBytes.W))))
  for (i <- 0 until blockRows) {
    for (w <- 0 until rowWords) {
      word_mask(i)(w) := s0_req.store_mask((i + 1) * rowBytes - 1, i * rowBytes)((w + 1) * wordBytes - 1, w * wordBytes)
    }
  }

  val word_full_overwrite = Wire(Vec(blockRows, Bits(rowWords.W)))
  val word_write = Wire(Vec(blockRows, Bits(rowWords.W)))
  for (i <- 0 until blockRows) {
    word_full_overwrite(i) := VecInit((0 until rowWords).map { w => word_mask(i)(w).andR }).asUInt
    word_write(i) := VecInit((0 until rowWords).map { w => word_mask(i)(w).orR }).asUInt
  }
  val row_full_overwrite = VecInit(word_full_overwrite.map(_.andR)).asUInt
  val row_write = VecInit(word_write.map(_.orR)).asUInt
  val full_overwrite = row_full_overwrite.andR

  // sanity check
  when (s0_fire) {
    OneHot.checkOneHot(Seq(s0_req.miss, s0_req.probe))
  }
  assert(!RegNext(s0_fire && s0_req.miss && !full_overwrite), "miss req should full overwrite")

  val meta_ready = io.meta_read.ready
  val data_ready = io.data_read.ready
  io.req.ready := meta_ready && !set_conflict && s1_ready //&& !(s3_valid && update_meta)

  io.meta_read.valid := io.req.valid && !set_conflict && s1_ready
  val meta_read = io.meta_read.bits
  meta_read.idx := get_idx(s0_req.addr)
  meta_read.way_en := ~0.U(nWays.W)
  meta_read.tag := DontCare

  // generata rmask here and use it in stage 1
  // If req comes form MissQueue, it must be a full overwrite,
  //   but we still need to read data array
  //   since we may do replacement
  // If it's a store(not from MissQueue):
  //   If it's full mask, no need to read data array;
  //   If it's partial mask, no need to read full masked words.
  // If it's a AMO(not from MissQueue), only need to read the specific word.
  // If it's probe, read it all.
  val miss_need_data = s0_req.miss
  val store_need_data = !s0_req.miss && !s0_req.probe && s0_req.source === STORE_SOURCE.U && !full_overwrite
  val amo_need_data = !s0_req.miss && !s0_req.probe && s0_req.source === AMO_SOURCE.U
  val probe_need_data = s0_req.probe
  
  val need_data = miss_need_data || store_need_data || amo_need_data || probe_need_data

  def rowWordBits = log2Floor(rowWords)
  val amo_row = s0_req.word_idx >> rowWordBits
  val amo_word = if (rowWordBits == 0) 0.U else s0_req.word_idx(rowWordBits - 1, 0)
  val amo_word_addr = s0_req.addr + (s0_req.word_idx << wordOffBits)

  val store_rmask = row_write & ~row_full_overwrite
  val amo_rmask = UIntToOH(amo_row)
  val full_rmask = ~0.U(blockRows.W)
  val none_rmask = 0.U(blockRows.W)

  val s0_rmask = Mux(store_need_data, store_rmask,
    Mux(amo_need_data, amo_rmask,
    Mux(probe_need_data || miss_need_data, full_rmask, none_rmask)))

  // generate wmask here and use it in stage 2
  val store_wmask = word_write
  val amo_wmask = WireInit(VecInit((0 until blockRows).map(i => 0.U(rowWords.W))))
  amo_wmask(amo_row) := VecInit((0 until rowWords).map(w => w.U === amo_word)).asUInt
  val full_wmask = VecInit((0 until blockRows).map(i => ~0.U(rowWords.W)))
  val none_wmask = VecInit((0 until blockRows).map(i => 0.U(rowWords.W)))

  dump_pipeline_reqs("MainPipe s0", s0_valid, s0_req)

  // --------------------------------------------------------------------------------
  // stage 1
  // read data, get meta, check hit or miss
  val s1_valid = RegInit(false.B)
  val s1_need_data = RegEnable(need_data, s0_fire)
  val s1_fire = s1_valid && s2_ready && (!s1_need_data || io.data_read.ready)
  val s1_req = RegEnable(s0_req, s0_fire)
  val s1_set = get_idx(s1_req.addr)

  val s1_rmask = RegEnable(s0_rmask, s0_fire)
  val s1_store_wmask = RegEnable(store_wmask, s0_fire)
  val s1_amo_wmask = RegEnable(amo_wmask, s0_fire)

  val s1_amo_row = RegEnable(amo_row, s0_fire)
  val s1_amo_word = RegEnable(amo_word, s0_fire)
  val s1_amo_word_addr = RegEnable(amo_word_addr, s0_fire)

  s1_s0_set_conflict := s1_valid && get_idx(s1_req.addr) === get_idx(s0_req.addr)

  when (s0_fire) {
    s1_valid := true.B
  }.elsewhen (s1_fire) {
    s1_valid := false.B
  }
  s1_ready := !s1_valid || s1_fire

  // tag match
  val ecc_meta_resp = WireInit(VecInit(Seq.fill(nWays)(0.U(encMetaBits.W))))
  ecc_meta_resp := Mux(RegNext(s0_fire), io.meta_resp, RegNext(ecc_meta_resp))
  val meta_resp = ecc_meta_resp.map(m => getMeta(m).asTypeOf(new L1Metadata))
  

  def wayMap[T <: Data](f: Int => T) = VecInit((0 until nWays).map(f))
  val s1_tag_eq_way = wayMap((w: Int) => meta_resp(w).tag === (get_tag(s1_req.addr))).asUInt
  val s1_tag_match_way = wayMap((w: Int) => s1_tag_eq_way(w) && meta_resp(w).coh.isValid()).asUInt
  val s1_tag_match = s1_tag_match_way.orR

  val s1_fake_meta = Wire(new L1Metadata)
  s1_fake_meta.tag := get_tag(s1_req.addr)
  s1_fake_meta.coh := ClientMetadata.onReset

  // when there are no tag match, we give it a Fake Meta
  // this simplifies our logic in s2 stage
  val s1_hit_meta  = Mux(s1_tag_match, Mux1H(s1_tag_match_way, wayMap((w: Int) => meta_resp(w))), s1_fake_meta)
  val s1_hit_coh = s1_hit_meta.coh

  // replacement policy
  val replacer = ReplacementPolicy.fromString(cacheParams.replacer, nWays, nSets)
  val s1_repl_way_en = WireInit(0.U(nWays.W))
  s1_repl_way_en := Mux(RegNext(s0_fire), UIntToOH(replacer.way(s1_set)), RegNext(s1_repl_way_en))
  val s1_repl_meta = Mux1H(s1_repl_way_en, wayMap((w: Int) => meta_resp(w)))
  val s1_repl_coh = s1_repl_meta.coh

  // only true miss request(not permission miss) need to do replacement
  // we use repl meta when we really need to a replacement
  val s1_need_replacement = s1_req.miss && !s1_tag_match
  val s1_way_en        = Mux(s1_need_replacement, s1_repl_way_en, s1_tag_match_way)
  val s1_meta          = Mux(s1_need_replacement, s1_repl_meta,   s1_hit_meta)
  val s1_coh           = Mux(s1_need_replacement, s1_repl_coh,  s1_hit_coh)

  // read data
  // io.data_read.valid := s1_valid/* && s2_ready*/ && s1_need_data && !(s3_valid && need_write_data)
  io.data_read.valid := s1_fire && s1_need_data
  val data_read = io.data_read.bits
  data_read.rmask := s1_rmask
  data_read.way_en := s1_way_en
  data_read.addr := s1_req.addr

  // tag ecc check
  (0 until nWays).foreach(w => assert(!(s1_valid && s1_tag_match_way(w) && cacheParams.tagCode.decode(ecc_meta_resp(w)).uncorrectable)))

  dump_pipeline_reqs("MainPipe s1", s1_valid, s1_req)

  // --------------------------------------------------------------------------------
  // stage 2
  // select out data
  // to release timing pressure, we only do data selection in s2
  // all other stuff, permission checking, write/amo stuff stay in s3
  // we only change cache internal states(lr/sc counter, tag/data array) in s3
  val s2_valid = RegInit(false.B)
  val s2_fire = s2_valid && s3_ready
  val s2_req = RegEnable(s1_req, s1_fire)
  s2_ready := !s2_valid || s2_fire

  val s2_rmask = RegEnable(s1_rmask, s1_fire)
  val s2_store_wmask = RegEnable(s1_store_wmask, s1_fire)
  val s2_amo_wmask = RegEnable(s1_amo_wmask, s1_fire)

  val s2_amo_row = RegEnable(s1_amo_row, s1_fire)
  val s2_amo_word = RegEnable(s1_amo_word, s1_fire)
  val s2_amo_word_addr = RegEnable(s1_amo_word_addr, s1_fire)

  s2_s0_set_conflict := s2_valid && get_idx(s2_req.addr) === get_idx(s0_req.addr)  

  when (s1_fire) { s2_valid := true.B }
  .elsewhen(s2_fire) { s2_valid := false.B }

  val s2_tag_match_way = RegEnable(s1_tag_match_way, s1_fire)
  val s2_tag_match = RegEnable(s1_tag_match, s1_fire)
  val s2_hit_meta = RegEnable(s1_hit_meta, s1_fire)
  val s2_hit_coh = RegEnable(s1_hit_coh, s1_fire)
  val s2_has_permission = s2_hit_coh.onAccess(s2_req.cmd)._1
  val s2_new_hit_coh = s2_hit_coh.onAccess(s2_req.cmd)._3

  val s2_repl_meta = RegEnable(s1_repl_meta, s1_fire)
  val s2_repl_coh = s2_repl_meta.coh
  val s2_repl_way_en = RegEnable(s1_repl_way_en, s1_fire)

  val s2_need_replacement = RegEnable(s1_need_replacement, s1_fire)
  val s2_way_en = RegEnable(s1_way_en, s1_fire)
  val s2_meta = RegEnable(s1_meta, s1_fire)
  val s2_coh = s2_meta.coh

  // we will treat it as a hit
  // if we need to update meta from Trunk to Dirty
  // go update it
  val s2_hit = s2_tag_match && s2_has_permission
  val s2_amo_hit = s2_hit && !s2_req.miss && !s2_req.probe && s2_req.source === AMO_SOURCE.U

  when (s2_valid) {
    XSDebug("MainPipe: s2 s2_tag_match: %b s2_has_permission: %b s2_hit: %b s2_need_replacement: %b s2_way_en: %x s2_state: %d\n",
      s2_tag_match, s2_has_permission, s2_hit, s2_need_replacement, s2_way_en, s2_coh.state)
  }

  val data_resp = WireInit(VecInit(Seq.fill(blockRows)(0.U(encRowBits.W))))
  data_resp := Mux(RegNext(s1_fire), io.data_resp, RegNext(data_resp))

  // generate write data
  val s2_store_data_merged = Wire(Vec(blockRows, UInt(rowBits.W)))

  def mergePutData(old_data: UInt, new_data: UInt, wmask: UInt): UInt = {
    val full_wmask = FillInterleaved(8, wmask)
    ((~full_wmask & old_data) | (full_wmask & new_data))
  }

  val s2_data_decoded = (0 until blockRows) map { r =>
    (0 until rowWords) map { w =>
      val data = data_resp(r)(encWordBits * (w + 1) - 1, encWordBits * w)
      val decoded = cacheParams.dataCode.decode(data)
      assert(!RegNext(s2_valid && s2_hit && s2_rmask(r) && decoded.uncorrectable))
      data(wordBits - 1, 0)
    }
  }

  for (i <- 0 until blockRows) {
    s2_store_data_merged(i) := Cat((0 until rowWords).reverse map { w =>
      val old_data = s2_data_decoded(i)(w)
      val new_data = s2_req.store_data(rowBits * (i + 1) - 1, rowBits * i)(wordBits * (w + 1) - 1, wordBits * w)
      // for amo hit, we should use read out SRAM data
      // do not merge with store data
      val wmask = Mux(s2_amo_hit, 0.U(wordBytes.W),
        s2_req.store_mask(rowBytes * (i + 1) - 1, rowBytes * i)(wordBytes * (w + 1) - 1, wordBytes * w))
      val store_data = mergePutData(old_data, new_data, wmask)
      store_data
    })
  }

  // AMO hits
  val s2_amo_row_data  = s2_store_data_merged(s2_amo_row)
  val s2_amo_word_data = VecInit((0 until rowWords) map (w => s2_amo_row_data(wordBits * (w + 1) - 1, wordBits * w)))
  val s2_data_word = s2_amo_word_data(s2_amo_word)

  dump_pipeline_reqs("MainPipe s2", s2_valid, s2_req)

  // --------------------------------------------------------------------------------
  // stage 3
  // do permission checking, write/amo stuff in s3
  // we only change cache internal states(lr/sc counter, tag/data array) in s3
  val s3_fire = Wire(Bool())
  val s3_req = RegEnable(s2_req, s2_fire)
  s3_ready := !s3_valid || s3_fire

  val s3_rmask = RegEnable(s2_rmask, s2_fire)
  val s3_store_wmask = RegEnable(s2_store_wmask, s2_fire)
  val s3_amo_wmask = RegEnable(s2_amo_wmask, s2_fire)

  val s3_amo_row = RegEnable(s2_amo_row, s2_fire)
  val s3_amo_word = RegEnable(s2_amo_word, s2_fire)
  val s3_amo_word_addr = RegEnable(s2_amo_word_addr, s2_fire)
  val s3_data_word = RegEnable(s2_data_word, s2_fire)
  val s3_store_data_merged = RegEnable(s2_store_data_merged, s2_fire)
  val s3_data_decoded = RegEnable(VecInit(s2_data_decoded.flatten).asUInt, s2_fire)

  s3_s0_set_conflict := s3_valid && get_idx(s3_req.addr) === get_idx(s0_req.addr)

  when (s2_fire) { s3_valid := true.B }
  .elsewhen (s3_fire) { s3_valid := false.B }

  val s3_tag_match_way = RegEnable(s2_tag_match_way, s2_fire)
  val s3_tag_match = RegEnable(s2_tag_match, s2_fire)
  val s3_hit_meta = RegEnable(s2_hit_meta, s2_fire)
  val s3_hit_coh = RegEnable(s2_hit_coh, s2_fire)
  val s3_has_permission = s3_hit_coh.onAccess(s3_req.cmd)._1
  val s3_new_hit_coh = s3_hit_coh.onAccess(s3_req.cmd)._3

  val s3_repl_meta = RegEnable(s2_repl_meta, s2_fire)
  val s3_repl_coh = s3_repl_meta.coh
  val s3_repl_way_en = RegEnable(s2_repl_way_en, s2_fire)

  val s3_need_replacement = RegEnable(s2_need_replacement, s2_fire)
  val s3_way_en = RegEnable(s2_way_en, s2_fire)
  val s3_meta = RegEnable(s2_meta, s2_fire)
  val s3_coh = s3_meta.coh

  // --------------------------------------------------------------------------------
  // Permission checking
  val miss_new_coh = s3_coh.onGrant(s3_req.cmd, s3_req.miss_param)
  assert(!RegNext(s3_valid && s3_req.miss && !miss_new_coh.isValid()))
  assert(!RegNext(s3_valid && s3_req.miss && s3_tag_match && !(s3_hit_coh.state < miss_new_coh.state)))

  // Determine what state to go to based on Probe param
  val (probe_has_dirty_data, probe_shrink_param, probe_new_coh) = s3_coh.onProbe(s3_req.probe_param)

  // as long as we has permission
  // we will treat it as a hit
  // if we need to update meta from Trunk to Dirty
  // go update it
  val s3_hit = s3_tag_match && s3_has_permission
  val s3_store_hit = s3_hit && !s3_req.miss && !s3_req.probe && s3_req.source === STORE_SOURCE.U
  val s3_amo_hit = s3_hit && !s3_req.miss && !s3_req.probe && s3_req.source === AMO_SOURCE.U

  when (s3_valid) {
    XSDebug("MainPipe: s3 s3_tag_match: %b s3_has_permission: %b s3_hit: %b s3_need_replacement: %b s3_way_en: %x s3_state: %d\n",
      s3_tag_match, s3_has_permission, s3_hit, s3_need_replacement, s3_way_en, s3_coh.state)
  }

  dump_pipeline_reqs("MainPipe s3", s3_valid, s3_req)

  // --------------------------------------------------------------------------------
  // Write to MetaArray
  // miss should always update meta
  // store only update meta when it hits and needs to update Trunk to Dirty
  val miss_update_meta = s3_req.miss
  val probe_update_meta = s3_req.probe && s3_tag_match && s3_coh =/= probe_new_coh
  val store_update_meta = s3_store_hit && s3_hit_coh =/= s3_new_hit_coh
  val amo_update_meta = s3_amo_hit && s3_hit_coh =/= s3_new_hit_coh
  update_meta := miss_update_meta || probe_update_meta || store_update_meta || amo_update_meta

  val new_coh = Mux(miss_update_meta, miss_new_coh,
    Mux(probe_update_meta, probe_new_coh,
    Mux(store_update_meta || amo_update_meta, s3_new_hit_coh, ClientMetadata.onReset)))

  io.meta_write.valid := s3_fire && update_meta
  io.meta_write.bits.idx := get_idx(s3_req.addr)
  io.meta_write.bits.way_en := s3_way_en
  io.meta_write.bits.data.tag := get_tag(s3_req.addr)
  io.meta_write.bits.data.coh := new_coh

  // --------------------------------------------------------------------------------
  // LR, SC and AMO
  val debug_sc_fail_addr = RegInit(0.U)
  val debug_sc_fail_cnt  = RegInit(0.U(8.W))

  val lrsc_count = RegInit(0.U(log2Ceil(lrscCycles).W))
  val lrsc_valid = lrsc_count > lrscBackoff.U
  val lrsc_addr  = Reg(UInt())
  val s3_lr = !s3_req.probe && s3_req.source === AMO_SOURCE.U && s3_req.cmd === M_XLR
  val s3_sc = !s3_req.probe && s3_req.source === AMO_SOURCE.U && s3_req.cmd === M_XSC
  val s3_lrsc_addr_match = lrsc_valid && lrsc_addr === get_block_addr(s3_req.addr)
  val s3_sc_fail = s3_sc && !s3_lrsc_addr_match
  val s3_sc_resp = Mux(s3_sc_fail, 1.U, 0.U)

  val s3_can_do_amo = (s3_req.miss && !s3_req.probe && s3_req.source === AMO_SOURCE.U) || s3_amo_hit
  val s3_can_do_amo_write = s3_can_do_amo && isWrite(s3_req.cmd) && !s3_sc_fail
  when (s3_valid && (s3_lr || s3_sc)) {
    when (s3_can_do_amo && s3_lr) {
      lrsc_count := (lrscCycles - 1).U
      lrsc_addr := get_block_addr(s3_req.addr)
    } .otherwise {
      lrsc_count := 0.U
    }
  } .elsewhen (lrsc_count > 0.U) {
    lrsc_count := lrsc_count - 1.U
  }

  io.lrsc_locked_block.valid := lrsc_valid
  io.lrsc_locked_block.bits  := lrsc_addr

  // when we release this block,
  // we invalidate this reservation set
  when (io.wb_req.fire()) {
    when (io.wb_req.bits.addr === lrsc_addr) {
      lrsc_count := 0.U
    }
  }

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
  assert(debug_sc_fail_cnt < 100.U, "L1DCache failed too many SCs in a row")

  // --------------------------------------------------------------------------------
  // Write to DataArray
  // Miss:
  //   1. not store and not amo, data: store_data mask: store_mask(full_mask)
  //   2. store, data: store_data mask: store_mask(full_mask)
  //   3. amo, data: merge(store_data, amo_data, amo_mask) mask: store_mask(full_mask)
  // 
  // Probe: do not write data, DontCare
  // Store hit: data: merge(s3_data, store_data, store_mask) mask: store_mask
  // AMO hit: data: merge(s3_data, amo_data, amo_mask) mask: store_mask
  // so we can first generate store data and then merge with amo_data

  // generate write mask
  // which word do we need to write
  val wmask = Mux(s3_req.miss, full_wmask,
    Mux(s3_store_hit, s3_store_wmask,
    Mux(s3_can_do_amo_write, s3_amo_wmask,
      none_wmask)))
  need_write_data := VecInit(wmask.map(w => w.orR)).asUInt.orR

  // generate write data
  // AMO hits
  val amoalu   = Module(new AMOALU(wordBits))
  amoalu.io.mask := s3_req.amo_mask
  amoalu.io.cmd  := s3_req.cmd
  amoalu.io.lhs  := s3_data_word
  amoalu.io.rhs  := s3_req.amo_data

  // merge amo write data
  val s3_amo_data_merged = Wire(Vec(blockRows, UInt(rowBits.W)))
  for (i <- 0 until blockRows) {
    s3_amo_data_merged(i) := Cat((0 until rowWords).reverse map { w =>
      val old_data = s3_store_data_merged(i)(wordBits * (w + 1) - 1, wordBits * w)
      val new_data = amoalu.io.out
      val wmask = Mux(s3_can_do_amo_write && i.U === s3_amo_row && w.U === s3_amo_word,
        ~0.U(wordBytes.W), 0.U(wordBytes.W))
      val data = mergePutData(old_data, new_data, wmask)
      data
    })
  }

  val data_write = io.data_write.bits
  io.data_write.valid := s3_fire && need_write_data
  data_write.rmask := DontCare
  data_write.way_en := s3_way_en
  data_write.addr := s3_req.addr
  data_write.wmask := VecInit(wmask.map(_.orR)).asUInt
  data_write.data := s3_amo_data_merged

  // --------------------------------------------------------------------------------
  // Writeback
  // whether we need to write back a block
  // TODO: add support for ProbePerm
  // Now, we only deal with ProbeBlock
  val miss_writeback = s3_need_replacement && s3_coh === ClientStates.Dirty
  val probe_writeback = s3_req.probe
  val need_writeback  = miss_writeback || probe_writeback

  val writeback_addr  = Cat(s3_meta.tag, get_idx(s3_req.addr)) << blockOffBits

  val (_, miss_shrink_param, _) = s3_coh.onCacheControl(M_FLUSH)
  val writeback_param = Mux(miss_writeback, miss_shrink_param, probe_shrink_param)

  val writeback_data = s3_coh === ClientStates.Dirty

  val wb_req = io.wb_req.bits
  io.wb_req.valid := s3_fire && need_writeback
  wb_req.addr := writeback_addr
  wb_req.param := writeback_param
  wb_req.voluntary := miss_writeback
  wb_req.hasData := writeback_data
  wb_req.data := s3_data_decoded

  // for write has higher priority than read, meta/data array ready is not needed
  s3_fire := s3_valid && (!need_writeback || io.wb_req.ready)/* &&
                         (!update_meta || io.meta_write.ready) &&
                         (!need_write_data || io.data_write.ready)*/

  // Technically, load fast wakeup should be disabled when data_write.valid is true,
  // but for timing purpose, we loose the condition to s3_valid, ignoring whether wb is ready or not.
  for (i <- 0 until (LoadPipelineWidth - 1)) {
    io.disable_ld_fast_wakeup(i) := need_write_data && s3_valid
  }
  io.disable_ld_fast_wakeup(LoadPipelineWidth - 1) := need_write_data && s3_valid || s1_need_data && s1_valid

  // --------------------------------------------------------------------------------
  // update replacement policy
  val access_bundle = Wire(ValidIO(new ReplacementAccessBundle))
  access_bundle.valid := RegNext(s3_fire && (update_meta || need_write_data))
  access_bundle.bits.set := RegNext(get_idx(s3_req.addr))
  access_bundle.bits.way := RegNext(s3_way_en)
  val access_bundles = io.replace_access.toSeq ++ Seq(access_bundle)
  val sets = access_bundles.map(_.bits.set)
  val touch_ways = Seq.fill(LoadPipelineWidth + 1)(Wire(ValidIO(UInt(log2Up(nWays).W))))
  (touch_ways zip access_bundles).map{ case (w, access) =>
    w.valid := access.valid
    w.bits := access.bits.way
  }
  replacer.access(sets, touch_ways)

  // --------------------------------------------------------------------------------
  // send store/amo miss to miss queue
  val store_amo_miss = !s3_req.miss && !s3_req.probe && !s3_hit && (s3_req.source === STORE_SOURCE.U || s3_req.source === AMO_SOURCE.U)
  io.miss_req.valid := s3_fire && store_amo_miss
  io.miss_req.bits.source := s3_req.source
  io.miss_req.bits.cmd := s3_req.cmd
  io.miss_req.bits.addr := s3_req.addr
  io.miss_req.bits.store_data := s3_req.store_data
  io.miss_req.bits.store_mask := s3_req.store_mask
  io.miss_req.bits.word_idx := s3_req.word_idx
  io.miss_req.bits.amo_data := s3_req.amo_data
  io.miss_req.bits.amo_mask := s3_req.amo_mask
  io.miss_req.bits.coh := s3_coh
  io.miss_req.bits.id := s3_req.id

  // --------------------------------------------------------------------------------
  // send response
  val resp = Wire(new MainPipeResp)
  resp.data := DontCare
  resp.id := s3_req.id
  resp.miss := store_amo_miss
  resp.replay := io.miss_req.valid && !io.miss_req.ready

  io.miss_resp.valid := s3_fire && s3_req.miss
  io.miss_resp.bits := resp
  io.miss_resp.bits.id := s3_req.miss_id

  io.store_resp.valid := s3_fire && s3_req.source === STORE_SOURCE.U
  io.store_resp.bits := resp

  io.amo_resp.valid := s3_fire && s3_req.source === AMO_SOURCE.U
  io.amo_resp.bits := resp
  io.amo_resp.bits.data := Mux(s3_sc, s3_sc_resp, s3_data_word)
  // reuse this field to pass lr sc valid to commit
  // nemu use this to see whether lr sc counter is still valid
  io.amo_resp.bits.id   := lrsc_valid

  when (io.req.fire()) {
    io.req.bits.dump()
  }

  when (io.miss_req.fire()) {
    io.miss_req.bits.dump()
  }

  when (io.miss_resp.fire()) {
    io.miss_resp.bits.dump()
  }

  when (io.store_resp.fire()) {
    io.store_resp.bits.dump()
  }

  when (io.amo_resp.fire()) {
    io.amo_resp.bits.dump()
  }

  when (io.wb_req.fire()) {
    io.wb_req.bits.dump()
  }

  when (io.lrsc_locked_block.valid) {
    XSDebug("lrsc_locked_block: %x\n", io.lrsc_locked_block.bits)
  }

  // -------
  // Debug logging functions
  def dump_pipeline_reqs(pipeline_stage_name: String, valid: Bool, req: MainPipeReq) = {
    when (valid) {
      XSDebug(s"$pipeline_stage_name ")
      req.dump()
    }
  }

  // performance counters
  // penalty for each req in pipeline in average = pipe_total_penalty / pipe_req
  XSPerf("pipe_req", s0_fire)
  XSPerf("pipe_total_penalty", PopCount(VecInit(Seq(s0_fire, s1_valid, s2_valid, s3_valid))))

  XSPerf("pipe_blocked_by_wbu", s3_valid && need_writeback && !io.wb_req.ready)
  XSPerf("pipe_blocked_by_nack_data", s1_valid && s1_need_data && !io.data_read.ready)
  XSPerf("pipe_reject_req_for_nack_meta", s0_valid && !meta_ready)
  XSPerf("pipe_reject_req_for_set_conflict", s0_valid && set_conflict)

  for (i <- 0 until LoadPipelineWidth) {
    for (w <- 0 until nWays) {
      XSPerf("load_pipe_" + Integer.toString(i,10) + "_access_way_" + Integer.toString(w, 10),
        io.replace_access(i).valid && io.replace_access(i).bits.way === w.U)
    }
  }

  for (w <- 0 until nWays) {
    XSPerf("main_pipe_access_way_" + Integer.toString(w, 10),
      access_bundle.valid && access_bundle.bits.way === w.U)
    XSPerf("main_pipe_choose_way_" + Integer.toString(w, 10),
      RegNext(s0_fire) && s1_repl_way_en === UIntToOH(w.U))
  }

}
