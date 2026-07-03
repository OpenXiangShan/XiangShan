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
import xiangshan.{L1CacheErrorInfo, XSCoreParamsKey}
import xiangshan.mem.L1PrefetchReq

class MainPipeReq(implicit p: Parameters) extends DCacheBundle {
  val miss = Bool() // only amo miss will refill in main pipe
  val missId = UInt(log2Up(cfg.nMissEntries).W)
  val missParam = UInt(TLPermissions.bdWidth.W)
  val missDirty = Bool()
  val occupyWay = UInt(nWays.W)
  val missFailCauseEvictBtot = Bool()

  val probe = Bool()
  val probeParam = UInt(TLPermissions.bdWidth.W)
  val probeNeedData = Bool()

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
  val storeData = UInt((cfg.blockBytes * 8).W)
  val storeMask = UInt(cfg.blockBytes.W)

  // which word does amo work on?
  val wordIdx = UInt(log2Up(cfg.blockBytes * 8 / DataBits).W)
  val amoData   = UInt(QuadWordBits.W)
  val amoMask   = UInt(QuadWordBytes.W)
  val amoCmp    = UInt(QuadWordBits.W) // data to be compared in AMOCAS

  // error
  val error = Bool()

  // replace
  val replace = Bool()
  val replaceWayEn = UInt(DCacheWays.W)

  // prefetch
  val pfSource = UInt(L1PfSourceBits.W)
  val access = Bool()

  val id = UInt(reqIdWidth.W)

  def isLoad: Bool = source === LOAD_SOURCE.U
  def isStore: Bool = source === STORE_SOURCE.U
  def isAMO: Bool = source === AMO_SOURCE.U
  def isPrefetch: Bool = source === DCACHE_PREFETCH_SOURCE.U

  def quadWordIdx = wordIdx >> 1

  def convertStoreReq(store: DCacheLineReq): MainPipeReq = {
    val req = Wire(new MainPipeReq)
    req := DontCare
    req.miss := false.B
    req.missDirty := false.B
    req.probe := false.B
    req.probeNeedData := false.B
    req.source := STORE_SOURCE.U
    req.cmd := store.cmd
    req.addr := store.addr
    req.vaddr := store.vaddr
    req.storeData := store.data
    req.storeMask := store.mask
    req.replace := false.B
    req.error := false.B
    req.id := store.id
    req.missFailCauseEvictBtot := false.B
    req
  }

  def convertPrefetchReq(prefetch: L1PrefetchReq): MainPipeReq = {
    val req = Wire(new MainPipeReq)
    req := DontCare
    req.miss := false.B
    req.missDirty := false.B
    req.probe := false.B
    req.probeNeedData := false.B
    req.source := DCACHE_PREFETCH_SOURCE.U
    req.cmd := MemoryOpConstants.M_PFR
    req.addr := prefetch.paddr
    req.vaddr := prefetch.vaddr
    req.replace := false.B
    req.error := false.B
    req.missFailCauseEvictBtot := false.B
    req.pfSource := prefetch.pf_source.value
    req.access := false.B
    req.id := 0.U
    req
  }
}

class MainPipeStatus(implicit p: Parameters) extends DCacheBundle {
  val set = UInt(idxBits.W)
  val wayEn = UInt(nWays.W)
}

class MainPipeInfoToMQ(implicit p:Parameters) extends DCacheBundle {
  val s2_valid = Bool()
  val s2_miss_id = UInt(log2Up(cfg.nMissEntries).W) // For refill data selection
  val s2_replay_to_mq = Bool()
  val s2_evict_bto_t_way = Bool()
  val s2_next_evict_way = UInt(nWays.W)
  val s3_valid = Bool()
  val s3_miss_id = UInt(log2Up(cfg.nMissEntries).W) // For mshr release
  val s3_refill_resp = Bool()
}

class MainPipe(implicit p: Parameters) extends DCacheModule with HasPerfEvents with HasL1PrefetchSourceParameter {
  val io = IO(new Bundle() {
    // probe queue
    val probeReq = Flipped(DecoupledIO(new MainPipeReq))
    // store miss go to miss queue
    val missReq = DecoupledIO(new MissReq)
    val missResp = Input(new MissResp) // miss resp is used to support plru update
    val refillReq = Flipped(DecoupledIO(new MainPipeReq))
    // send miss request to wbq
    val wbqConflictCheck = Valid(UInt())
    val wbqBlockMissReq = Input(Bool())
    // store buffer
    val storeReq = Flipped(DecoupledIO(new DCacheLineReq))
    val storeReplayResp = ValidIO(new DCacheLineResp)
    val storeHitResp = ValidIO(new DCacheLineResp)
    // atmoics
    val atomicReq = Flipped(DecoupledIO(new MainPipeReq))
    val atomicResp = ValidIO(new MainPipeResp)
    // find matched refill data in missentry
    val mainpipeInfo = Output(new MainPipeInfoToMQ)
    // missqueue refill data
    val refillInfo = Flipped(ValidIO(new MissQueueRefillInfo))
    // write-back queue
    val wb = DecoupledIO(new WritebackReq)
    val wbReadyDup = Vec(nDupWbReady, Input(Bool()))
    // hardware prefetch
    val prefetchReq = Flipped(Decoupled(new L1PrefetchReq()))
    // pass to Prefetch Monitor for statistic
    val prefetchStat = Output(new PipePrefetchStatBundle)

    // data sram
    val dataRead = Vec(LoadPipelineWidth, Input(Bool()))
    val dataReadIntend = Output(Bool())
    val dataReadline = DecoupledIO(new L1BankedDataReadLineReq)
    val dataReadlineCanGo = Output(Bool())
    val dataReadlineStall = Output(Bool())
    val dataReadlineCanResp = Output(Bool())
    val dataResp = Input(Vec(DCacheBanks, new L1BankedDataReadResult()))
    val readlineError = Input(Bool())
    val readlineErrorDelayed = Input(Bool())
    val dataWrite = DecoupledIO(new L1BankedDataWriteReq)
    val dataWriteDup = Vec(DCacheBanks, Valid(new L1BankedDataWriteReqCtrl))
    val dataWriteReadyDup = Vec(nDupDataWriteReady, Input(Bool()))

    // meta array
    val metaRead = DecoupledIO(new MetaReadReq)
    val metaResp = Input(Vec(nWays, new Meta))
    val metaWrite = DecoupledIO(new CohMetaWriteReq)
    val extraMetaResp = Input(Vec(nWays, new DCacheExtraMeta))
    val errorFlagWrite = DecoupledIO(new ErrorMetaWriteReq)
    val prefetchFlagWrite = DecoupledIO(new SourceMetaWriteReq)
    val accessFlagWrite = DecoupledIO(new FlagMetaWriteReq)
    val latencyFlagWrite = DecoupledIO(new LatencyMetaWriteReq)

    // tag sram
    val tagRead = DecoupledIO(new TagReadReq)
    val tagResp = Input(Vec(nWays, UInt(encTagBits.W)))
    val tagWrite = DecoupledIO(new TagWriteReq)
    val tagWriteReadyDup = Vec(nDupTagWriteReady, Input(Bool()))
    val tagWriteIntend = Output(new Bool())

    // update state vec in replacement algo
    val replaceAccess = ValidIO(new ReplacementAccessBundle)
    // find the way to be replaced
    val replaceWay = new ReplacementWayReqIO

    val evictSet = Output(UInt())
    val btotWaysForSet = Input(UInt(nWays.W))

    // writeback addr to be replaced
    val replace = new MissQueueBlockIO

    // sms prefetch
    val smsAgtEvictReq = DecoupledIO(new AGTEvictReq)

    val status = new Bundle() {
      val s0_set = ValidIO(UInt(idxBits.W))
      val s1, s2, s3 = ValidIO(new MainPipeStatus)
    }
    val statusDup = Vec(nDupStatus, new Bundle() {
      val s1, s2, s3 = ValidIO(new MainPipeStatus)
    })

    // lrsc locked block should block probe
    val lrscLockedBlock = Output(Valid(UInt(PAddrBits.W)))
    val invalidResvSet = Input(Bool())
    val updateResvSet = Output(Bool())
    val blockLr = Output(Bool())

    // ecc error
    val error = Output(ValidIO(new L1CacheErrorInfo))
    val pseudoError = Flipped(DecoupledIO(Vec(DCacheBanks, new CtrlUnitSignalingBundle)))
    val pseudoTagErrorInjDone = Output(Bool())
    val pseudoDataErrorInjDone = Output(Bool())
    // force write
    val forceWrite = Input(Bool())

    val bloomFilterQuery = new Bundle {
      val set = ValidIO(new BloomQueryBundle(BLOOM_FILTER_ENTRY_NUM))
      val clr = ValidIO(new BloomQueryBundle(BLOOM_FILTER_ENTRY_NUM))
    }
  })

  // meta array is made of regs, so meta write or read should always be ready
  assert(RegNext(io.metaRead.ready))
  assert(RegNext(io.metaWrite.ready))

  val s1_s0_set_conflict, s2_s0_set_conflict, s3_s0_set_conflict = Wire(Bool())
  val setConflict = s1_s0_set_conflict || s2_s0_set_conflict || s3_s0_set_conflict
  // check sbuffer store req set_conflict in parallel with req arbiter
  // it will speed up the generation of store_req.ready, which is in crit. path
  val s1_s0_set_conflict_store, s2_s0_set_conflict_store, s3_s0_set_conflict_store = Wire(Bool())
  val storeSetConflict = s1_s0_set_conflict_store || s2_s0_set_conflict_store || s3_s0_set_conflict_store
  val s1_ready, s2_ready, s3_ready = Wire(Bool())

  // convert store req to main pipe req, and select a req from store and probe
  val storeWaitCycles = RegInit(0.U(4.W))
  val StoreWaitThreshold = Wire(UInt(4.W))
  StoreWaitThreshold := Constantin.createRecord(s"StoreWaitThreshold_${p(XSCoreParamsKey).HartId}", initValue = 0)
  val storeWaitTooLong = storeWaitCycles >= StoreWaitThreshold
  val loadsAreComing = io.dataRead.asUInt.orR
  val storeCanAccept = storeWaitTooLong || !loadsAreComing || io.forceWrite

  val storeReq = Wire(DecoupledIO(new MainPipeReq))
  storeReq.bits := (new MainPipeReq).convertStoreReq(io.storeReq.bits)
  storeReq.valid := io.storeReq.valid && storeCanAccept
  io.storeReq.ready := storeReq.ready && storeCanAccept


  when (storeReq.fire) { // if wait too long and write success, reset counter.
    storeWaitCycles := 0.U
  } .elsewhen (storeWaitCycles < StoreWaitThreshold && io.storeReq.valid && !storeReq.ready) { // if block store, increase counter.
    storeWaitCycles := storeWaitCycles + 1.U
  }

  // convert prefetch req to main pipe req
  val prefetchReq = Wire(DecoupledIO(new MainPipeReq))
  prefetchReq.bits := (new MainPipeReq).convertPrefetchReq(io.prefetchReq.bits)
  prefetchReq.valid := io.prefetchReq.valid
  io.prefetchReq.ready := prefetchReq.ready

  // s0: read meta and tag
  val req = Wire(DecoupledIO(new MainPipeReq))
  arbiter(
    in = Seq(
      io.probeReq,
      io.refillReq,
      prefetchReq, // Todo: what's the best priority
      storeReq, // Note: store_req.ready is now manually assigned for better timing
      io.atomicReq,
    ),
    out = req,
    name = Some("main_pipe_req")
  )

  val storeIdx = get_dcache_idx(io.storeReq.bits.vaddr)
  // manually assign store_req.ready for better timing
  // now store_req set conflict check is done in parallel with req arbiter
  storeReq.ready := io.metaRead.ready && io.tagRead.ready && s1_ready && !storeSetConflict &&
    !io.probeReq.valid && !io.refillReq.valid && !prefetchReq.valid
  // Prefetch request has lower priority, so it needs to check higher priority requests
  prefetchReq.ready := io.metaRead.ready && io.tagRead.ready && s1_ready && !setConflict &&
    !io.probeReq.valid && !io.refillReq.valid
  val s0_req = req.bits
  val s0_idx = get_dcache_idx(s0_req.vaddr)
  val s0_need_tag = io.tagRead.valid
  val s0_can_go = io.metaRead.ready && io.tagRead.ready && s1_ready && !setConflict
  val s0_fire = req.valid && s0_can_go

  req.ready := s0_can_go

  val bankWrite = VecInit((0 until DCacheBanks).map(i => getMaskOfBank(i, s0_req.storeMask).orR)).asUInt
  val bankFullWrite = VecInit((0 until DCacheBanks).map(i => getMaskOfBank(i, s0_req.storeMask).andR)).asUInt
  val banksFullOverwrite = bankFullWrite.andR

  val bankedStoreRmask = bankWrite & ~bankFullWrite
  val bankedFullRmask = ~0.U(DCacheBanks.W)
  val bankedNoneRmask = 0.U(DCacheBanks.W)

  val storeNeedData = !s0_req.probe && s0_req.isStore && bankedStoreRmask.orR
  val probeNeedData = s0_req.probe
  val amoNeedData = !s0_req.probe && s0_req.isAMO && !s0_req.miss
  val missNeedData = s0_req.miss
  val replaceNeedData = s0_req.replace

  val bankedNeedData = storeNeedData || probeNeedData || amoNeedData || missNeedData || replaceNeedData
  val bankedAmoRmask = Mux(
    isAMOCASQ(s0_req.cmd),
    bankMaskFromBase(quadWordBankBase(s0_req.quadWordIdx), DCacheQuadWordBankCount),
    bankMaskFromBase(wordBankBase(s0_req.wordIdx), DCacheWordBankCount)
  )

  val s0_banked_rmask = Mux(
    storeNeedData,
    bankedStoreRmask,
    Mux(
      amoNeedData,
      bankedAmoRmask,
      Mux(
        probeNeedData || missNeedData || replaceNeedData,
        bankedFullRmask,
        bankedNoneRmask
      )
    )
  )

  // generate wmask here and use it in stage 2
  val bankedStoreWmask = bankWrite
  val bankedFullWmask = ~0.U(DCacheBanks.W)
  val bankedNoneWmask = 0.U(DCacheBanks.W)

  // s1: read data
  val s1_valid = RegInit(false.B)
  val s1_req = RegEnable(s0_req, s0_fire)

  val metaResp = Wire(Vec(nWays, (new Meta).asUInt))
  val s1_repl_way_en = WireInit(0.U(nWays.W))
  val s1_repl_coh = ParallelMux(s1_repl_way_en.asBools, (0 until nWays).map(w => metaResp(w))).asTypeOf(new ClientMetadata)
  val s1_need_data = if (dcacheParameters.alwaysReleaseData) {
    RegEnable(bankedNeedData, s0_fire)
  } else {
    Mux(!s1_req.miss, RegEnable(bankedNeedData, s0_fire), s1_repl_coh.state === ClientStates.Dirty)
  }

  val s1_banked_rmask = RegEnable(s0_banked_rmask, s0_fire)
  val s1_banked_store_wmask = RegEnable(bankedStoreWmask, s0_fire)
  val s1_need_tag = RegEnable(s0_need_tag, s0_fire)
  val s1_can_go = s2_ready && (io.dataReadline.ready || !s1_need_data)
  val s1_fire = s1_valid && s1_can_go
  val s1_idx = get_dcache_idx(s1_req.vaddr)
  val s1_dm_way = RegEnable(getDirectMapWay(s0_req.vaddr), s0_fire)
  val s1_is_prefetch = !s1_req.replace && !s1_req.probe && !s1_req.miss && s1_req.isPrefetch

  when (s0_fire) {
    s1_valid := true.B
  }.elsewhen (s1_fire) {
    s1_valid := false.B
  }
  s1_ready := !s1_valid || s1_can_go
  s1_s0_set_conflict := s1_valid && s0_idx === s1_idx && !s1_is_prefetch
  s1_s0_set_conflict_store := s1_valid && storeIdx === s1_idx && !s1_is_prefetch

  def wayMap[T <: Data](f: Int => T) = VecInit((0 until nWays).map(f))
  metaResp := Mux(GatedValidRegNext(s0_fire), VecInit(io.metaResp.map(_.asUInt)), RegEnable(metaResp, s1_valid))
  // pseudo ecc enc tag
  val pseudoTagToggleMask = Mux(
                                  io.pseudoError.valid && io.pseudoError.bits(0).valid,
                                  io.pseudoError.bits(0).mask(tagBits - 1, 0),
                                  0.U(tagBits.W)
                              )
  val pseudoEncTagResp = io.tagResp.map {
    case real_enc =>
      if (cacheCtrlParamsOpt.nonEmpty && EnableTagEcc) {
        val ecc = real_enc(encTagBits - 1, tagBits)
        val toggleTag = real_enc(tagBits - 1, 0) ^ pseudoTagToggleMask
        Cat(ecc, toggleTag)
      } else {
        real_enc
      }
  }
  val encTagResp = Wire(io.tagResp.cloneType)
  encTagResp := Mux(GatedValidRegNext(s0_fire), VecInit(pseudoEncTagResp), RegEnable(encTagResp, s1_valid))
  val tagResp = encTagResp.map(encTag => encTag(tagBits - 1, 0))
  val s1_meta_valids = wayMap((w: Int) => Meta(metaResp(w)).coh.isValid()).asUInt
  val s1_tag_errors = wayMap((w: Int) => s1_meta_valids(w) && dcacheParameters.tagCode.decode(encTagResp(w)).error).asUInt
  val s1_tag_eq_way = wayMap((w: Int) => tagResp(w) === get_tag(s1_req.addr)).asUInt
  val s1_tag_ecc_eq_way = wayMap((w: Int) => s1_tag_eq_way(w) && !s1_tag_errors(w)).asUInt
  val s1_tag_ecc_match_way = wayMap((w: Int) => s1_tag_ecc_eq_way(w) && s1_meta_valids(w)).asUInt
  val s1_tag_match = ParallelORR(s1_tag_ecc_match_way)
  val s1_real_tag_eq_way = wayMap((w: Int) => io.tagResp(w)(tagBits - 1, 0) === get_tag(s1_req.addr) && s1_meta_valids(w)).asUInt
  val s1_has_real_tag_eq_way = ParallelORR(s1_real_tag_eq_way)
  val s1_real_tag_match_way_en = PriorityEncoderOH(s1_real_tag_eq_way)
  val s1_real_tag_match_way = PriorityEncoder(s1_real_tag_eq_way)

  val s1_hit_tag = get_tag(s1_req.addr)
  val s1_hit_coh = ClientMetadata(ParallelMux(s1_tag_ecc_match_way.asBools, (0 until nWays).map(w => metaResp(w))))
  val s1_hit_prefetch = ParallelMux(s1_tag_ecc_match_way.asBools, (0 until nWays).map(w => io.extraMetaResp(w).prefetch))
  val s1_extra_meta = Wire(io.extraMetaResp.head.cloneType)
  s1_extra_meta := Mux(
    GatedValidRegNext(s0_fire),
    ParallelMux(s1_tag_ecc_match_way.asBools, (0 until nWays).map(w => io.extraMetaResp(w))),
    RegEnable(s1_extra_meta, s1_valid)
  )
  val s1_flag_error = s1_extra_meta.error
  io.pseudoTagErrorInjDone := s1_fire && s1_meta_valids.orR

  XSPerfAccumulate("probe_unused_prefetch", s1_req.probe && isFromL1Prefetch(s1_extra_meta.prefetch) && !s1_extra_meta.access) // may not be accurate
  XSPerfAccumulate("replace_unused_prefetch", s1_req.replace && isFromL1Prefetch(s1_extra_meta.prefetch) && !s1_extra_meta.access) // may not be accurate

  // replacement policy
  val s1_invalid_vec = wayMap(w => !metaResp(w).asTypeOf(new Meta).coh.isValid())
  val s1_have_invalid_way = s1_invalid_vec.asUInt.orR
  val s1_invalid_way_en = ParallelPriorityMux(s1_invalid_vec.zipWithIndex.map(x => x._1 -> UIntToOH(x._2.U(nWays.W))))
  s1_repl_way_en := Mux(
    GatedValidRegNext(s0_fire),
    Mux(s1_req.missFailCauseEvictBtot, s1_req.occupyWay, UIntToOH(io.replaceWay.way)),
    RegEnable(s1_repl_way_en, s1_valid)
  )
  val s1_repl_way = Wire(UInt(wayBits.W))
  s1_repl_way := Mux(
    GatedValidRegNext(s0_fire),
    Mux(s1_req.missFailCauseEvictBtot, OHToUInt(s1_req.occupyWay), io.replaceWay.way),
    RegEnable(s1_repl_way, s1_valid)
  ) // UInt format of `s1_repl_way_en`
  val s1_repl_tag = ParallelMux(Mux(io.pseudoError.valid && s1_has_real_tag_eq_way, s1_real_tag_match_way_en, s1_repl_way_en).asBools,
                                (0 until nWays).map(w => tagResp(w)))
  val s1_repl_pf  = ParallelMux(s1_repl_way_en.asBools, (0 until nWays).map(w => io.extraMetaResp(w).prefetch))

  val s1_real_tag = ParallelMux(s1_real_tag_match_way_en.asBools, (0 until nWays).map(w => io.tagResp(w)))

  val s1_need_replacement = s1_req.miss && !s1_tag_match
  val s1_need_eviction = s1_req.miss && !s1_tag_match && s1_repl_coh.state =/= ClientStates.Nothing

  val s1_way_en = Mux(io.pseudoError.valid && s1_has_real_tag_eq_way, s1_real_tag_match_way_en, 
                      Mux(s1_need_replacement, s1_repl_way_en, s1_tag_ecc_match_way))
  val s1_way = Mux(io.pseudoError.valid && s1_has_real_tag_eq_way, s1_real_tag_match_way,
                   Mux(s1_need_replacement, s1_repl_way, OHToUInt(s1_tag_ecc_match_way)))
  assert(!RegNext(s1_fire && PopCount(s1_way_en) > 1.U))

  val s1_tag = s1_hit_tag
  val s1_coh = s1_hit_coh

  XSPerfAccumulate("store_has_invalid_way_but_select_valid_way", io.replaceWay.set.valid && wayMap(w => !metaResp(w).asTypeOf(new Meta).coh.isValid()).asUInt.orR && s1_need_replacement && s1_repl_coh.isValid())
  XSPerfAccumulate("store_using_replacement", io.replaceWay.set.valid && s1_need_replacement)

  val (s1_has_permission, s1_shrink_perm, s1_new_hit_coh) = s1_hit_coh.onAccess(s1_req.cmd)
  val s1_hit = s1_tag_match && s1_has_permission
  val s1_is_store = !s1_req.replace && !s1_req.probe && !s1_req.miss && s1_req.isStore
  val s1_is_amo = !s1_req.replace && !s1_req.probe && !s1_req.miss && s1_req.isAMO && s1_req.cmd =/= M_XSC
  val s1_pregen_can_go_to_mq = (s1_is_store || s1_is_amo || s1_is_prefetch) && !s1_hit
  val s1_grow_perm = s1_shrink_perm === BtoT && !s1_has_permission

  // s2: select data, return resp if this is a store miss
  val s2_valid = RegInit(false.B)
  val s2_req = RegEnable(s1_req, s1_fire)
  val s2_tag_errors = RegEnable(s1_tag_errors, s1_fire)
  val s2_tag_match = RegEnable(s1_tag_match, s1_fire)
  val s2_has_real_tag_eq_way = RegEnable(s1_has_real_tag_eq_way, s1_fire)
  val s2_tag_ecc_match_way = RegEnable(s1_tag_ecc_match_way, s1_fire)
  val s2_hit_coh = RegEnable(s1_hit_coh, s1_fire)
  val s2_has_permission = RegEnable(s1_has_permission, s1_fire)
  val s2_new_hit_coh = RegEnable(s1_new_hit_coh, s1_fire)
  val s2_grow_perm = RegEnable(s1_grow_perm, s1_fire) && s2_tag_match
  val s2_hit_prefetch = RegEnable(s1_hit_prefetch, s1_fire)

  val s2_repl_tag = RegEnable(s1_repl_tag, s1_fire)
  val s2_repl_coh = RegEnable(s1_repl_coh, s1_fire)
  val s2_repl_pf  = RegEnable(s1_repl_pf, s1_fire)

  val s2_has_pesudo_inj = RegEnable(io.pseudoError.valid, false.B, s1_fire)
  val s2_real_tag_has_error = dcacheParameters.tagCode.decode(RegEnable(s1_real_tag, s1_fire)).error
  val s2_refill_tag_eq_way = s2_has_pesudo_inj && s2_has_real_tag_eq_way & !s2_real_tag_has_error

  val s2_need_replacement = RegEnable(s1_need_replacement, s1_fire)
  val s2_need_eviction = RegEnable(s1_need_eviction, s1_fire)
  val s2_need_data = RegEnable(s1_need_data, s1_fire)
  val s2_need_tag = RegEnable(s1_need_tag, s1_fire)
  val s2_idx = get_dcache_idx(s2_req.vaddr)

  val s2_way_en = RegEnable(s1_way_en, s1_fire)
  val s2_tag = Mux(s2_need_replacement, s2_repl_tag, RegEnable(s1_tag, s1_fire))
  val s2_coh = Mux(s2_need_replacement, s2_repl_coh, RegEnable(s1_coh, s1_fire))
  val s2_banked_store_wmask = RegEnable(s1_banked_store_wmask, s1_fire)
  val s2_flag_error = RegEnable(s1_flag_error, s1_fire)
  val s2_tag_error = WireInit(false.B)
  val s2_l2_error = Mux(io.refillInfo.valid, io.refillInfo.bits.error, 0.U.asTypeOf(new TLError()))
  val s2_refill_latency = Mux(io.refillInfo.valid && isFromL1Prefetch(s2_req.pfSource), io.refillInfo.bits.refillLatency, 0.U)
  val s2_error = s2_flag_error.asUInt.orR || s2_tag_error || s2_l2_error.asUInt.orR // data_error not included

  val s2_may_report_data_error = s2_need_data && s2_coh.state =/= ClientStates.Nothing

  val s2_hit = (s2_tag_match || s2_refill_tag_eq_way) && s2_has_permission
  val s2_sc = s2_req.cmd === M_XSC
  val s2_lr = s2_req.cmd === M_XLR
  val s2_amo_hit = s2_hit && !s2_req.probe && !s2_req.miss && s2_req.isAMO
  val s2_store_hit = s2_hit && !s2_req.probe && !s2_req.miss && s2_req.isStore
  val s2_should_not_report_ecc_error = !s2_req.miss && (s2_req.isAMO && !s2_lr || s2_req.isStore)
  val s2_is_prefetch = !s2_req.replace && !s2_req.probe && !s2_req.miss && s2_req.isPrefetch

  if(EnableTagEcc) {
    val s2_probe_or_atomic = (s2_req.probe || s2_req.isAMO && !s2_sc) && !s2_req.miss
    val s2_probe_atomic_tag_error = s2_probe_or_atomic && !s2_tag_match && s2_tag_errors.orR
    val s2_evict_tag_error = !s2_probe_or_atomic && (s2_tag_errors & s2_way_en).orR
    s2_tag_error := (s2_probe_atomic_tag_error || s2_evict_tag_error) && s2_need_tag
  }

  s2_s0_set_conflict := s2_valid && s0_idx === s2_idx && !s2_is_prefetch
  s2_s0_set_conflict_store := s2_valid && storeIdx === s2_idx && !s2_is_prefetch

  // BtoT grow blocked: too many in-flight BtoT occupies in this set; replay store
  val s2_has_more_then3_ways_bto_t = PopCount(io.btotWaysForSet) > (nWays-2).U
  val s2_grow_perm_fail = s2_has_more_then3_ways_bto_t && s2_grow_perm
  XSError(s2_valid && s2_grow_perm && io.btotWaysForSet.andR,
    "BtoT grow permission, but all ways are BtoT\n"
  )

  // For a store req, it either hits and goes to s3, or miss and enter miss queue immediately
  val s2_replace_block = io.replace.block && io.replace.req.valid
  val s2_req_miss_without_data = Mux(s2_valid, s2_req.miss && !io.refillInfo.valid, false.B)
  val s2_can_go_to_mq_no_data = (s2_req_miss_without_data && RegEnable(s2_req_miss_without_data && !io.mainpipeInfo.s2_replay_to_mq, false.B, s2_valid)) // miss_req in s2 but refill data is invalid, can block 1 cycle
  val s2_can_go_to_mq_evict_fail = s2_replace_block // refill eviction conflicts with in-flight MSHR (BtoT); replay to MQ for another evict way
  val s2_can_go_to_mq_replay = s2_can_go_to_mq_no_data || s2_can_go_to_mq_evict_fail
  val s2_can_go_to_mq = RegEnable(s1_pregen_can_go_to_mq, s1_fire)
  val s2_can_go_to_s3 = (s2_sc || s2_req.replace || s2_req.probe ||
    Mux(
      s2_req.miss,
      io.refillInfo.valid && !s2_replace_block,
      (s2_req.isStore || s2_req.isAMO || s2_req.isPrefetch) && s2_hit
    )
  ) && s3_ready
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
  val s2_valid_to_s3 = s2_valid && s3_ready
  val replay = !io.missReq.ready || io.wbqBlockMissReq

  io.dataReadlineCanGo := GatedValidRegNext(s1_fire)
  io.dataReadlineStall := s2_valid
  io.dataReadlineCanResp := s2_fire_to_s3

  def mergePutData(oldData: UInt, newData: UInt, wmask: UInt): UInt = {
    val fullWmask = FillInterleaved(8, wmask)
    ((~fullWmask & oldData) | (fullWmask & newData))
  }
  val s2_merge_mask = Wire(Vec(DCacheBanks, UInt(DCacheSRAMRowBytes.W)))
  val s2_store_data_merged_without_cache = Wire(Vec(DCacheBanks, UInt(DCacheSRAMRowBits.W)))
  for (i <- 0 until DCacheBanks) {
    val newData = getDataOfBank(i, Mux(s2_req.miss, io.refillInfo.bits.storeData, s2_req.storeData))
    // for amo hit, we should use read out SRAM data
    // do not merge with store data
    s2_merge_mask(i) := Mux(s2_amo_hit, 0.U(wordBytes.W), getMaskOfBank(i, Mux(s2_req.miss, io.refillInfo.bits.storeMask, s2_req.storeMask)))
    s2_store_data_merged_without_cache(i) := mergePutData(0.U(DCacheSRAMRowBits.W), newData, s2_merge_mask(i))
  }

  io.pseudoDataErrorInjDone := s2_fire_to_s3 && (s2_tag_error || s2_hit) && s2_may_report_data_error
  io.pseudoError.ready := false.B
  XSError(s2_valid && s2_can_go_to_s3 && s2_req.miss && !io.refillInfo.valid, "MainPipe req can go to s3 but no refill data")

  // s3: write data, meta and tag
  val s3_valid = RegInit(false.B)
  val s3_req = RegEnable(s2_req, s2_valid_to_s3)
  val s3_miss_param = RegEnable(io.refillInfo.bits.missParam, s2_valid_to_s3)
  val s3_miss_dirty = RegEnable(io.refillInfo.bits.missDirty, s2_fire_to_s3)
  val s3_tag = RegEnable(s2_tag, s2_valid_to_s3)
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
  val s3_is_prefetch = !s3_req.replace && !s3_req.probe && !s3_req.miss && s3_req.isPrefetch

  val s3_data_resp = io.dataResp
  val s3_data = WireInit(VecInit((0 until DCacheBanks).map(i => {
    s3_data_resp(i).rawData
  })))
  val s3_store_data_merged = Wire(Vec(DCacheBanks, UInt(DCacheSRAMRowBits.W)))
  for (i <- 0 until DCacheBanks) {
    // for amo hit, we should use read out SRAM data
    // do not merge with store data
    s3_store_data_merged(i) := mergePutData(s3_store_data_merged_without_cache(i), s3_data(i), s3_merge_mask(i))
  }

  val s3_word_bank_base = wordBankBase(s3_req.wordIdx)
  val s3_quad_word_bank_base = quadWordBankBase(s3_req.quadWordIdx)
  val s3_data_words = VecInit((0 until blockWords).map(i => {
    assembleBankData(
      s3_store_data_merged,
      wordBankBase(i.U(log2Up(blockWords).W)),
      DCacheWordBankCount
    )
  }))
  val s3_data_word = s3_data_words(s3_req.wordIdx)
  val s3_data_quad_word = VecInit((0 until blockWords).map(i => {
    if (i == blockWords - 1) {
      Cat(0.U(DCacheWordBits.W), s3_data_words(i))
    } else {
      Cat(s3_data_words(i + 1), s3_data_words(i))
    }
  }))(s3_req.wordIdx)
  val s3_amo_resp_data = s3_data_quad_word
  val s3_data_line = Cat((0 until DCacheBanks).reverse.map(i => s3_data(i)))

  val s3_refill_latency = RegEnable(s2_refill_latency, s2_fire_to_s3)
  val s3_sc_fail  = Wire(Bool()) // miss or lr mismatch
  val s3_need_replacement = RegEnable(s2_need_replacement && !s2_refill_tag_eq_way, s2_fire_to_s3)

  val (_, probe_shrink_param, probe_new_coh) = s3_coh.onProbe(s3_req.probeParam)
  val (_, miss_shrink_param, _) = s3_coh.onCacheControl(M_FLUSH)

  val missUpdateMeta = s3_req.miss
  val probeUpdateMeta = s3_req.probe && s3_tag_match && s3_coh =/= probe_new_coh
  val storeUpdateMeta = s3_req.isStore && !s3_req.probe && s3_hit_coh =/= s3_new_hit_coh
  val amoUpdateMeta = s3_req.isAMO && !s3_req.probe && s3_hit_coh =/= s3_new_hit_coh && !s3_sc_fail
  val amoWaitAmoalu = s3_req.isAMO && s3_req.cmd =/= M_XLR && s3_req.cmd =/= M_XSC && !isAMOCAS(s3_req.cmd)
  val updateMeta = (missUpdateMeta || probeUpdateMeta || storeUpdateMeta || amoUpdateMeta) && !s3_req.replace

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

  val missNewCoh = ClientMetadata(missCohGen(s3_req.cmd, s3_miss_param, s3_miss_dirty))

  // report ecc error
  val s3_tag_error_beu = RegEnable(s2_tag_error, s2_fire)
  val s3_tag_error_wb = RegEnable(s2_tag_error, s2_fire_to_s3)

  // data_error will be reported by data array 1 cycle after data read resp
  val s3_data_error_beu = io.readlineErrorDelayed && GatedValidRegNext(s2_fire_to_s3) && RegEnable(s2_may_report_data_error, s2_fire)
  val s3_data_error_wb = io.readlineErrorDelayed && RegEnable(s2_may_report_data_error, s2_fire_to_s3)

  val s3_l2_error_beu = RegEnable(s2_l2_error, s2_fire)
  val s3_l2_error_wb = RegEnable(s2_l2_error, s2_fire_to_s3)
  val s3_flag_error_beu = RegEnable(s2_flag_error, s2_fire)

  // error signal for amo inst
  // s3_error_beu = s3_flag_error_beu || s3_tag_error_beu || s3_l2_error_beu || s3_data_error_beu
  val s3_error_beu = RegEnable(s2_error, 0.U.asTypeOf(s2_error), s2_fire) || s3_data_error_beu
  val s3_error_wb = RegEnable(s2_error, 0.U.asTypeOf(s2_error), s2_fire_to_s3) || s3_data_error_wb
  val s3_error_paddr_beu = get_block_addr(RegEnable(Cat(s2_tag, get_untag(s2_req.vaddr)), s2_fire))

  // LR, SC and AMO
  val debugScFailAddr = RegInit(0.U)
  val debugScFailCnt  = RegInit(0.U(8.W))
  val debugScAddrMatchFailCnt  = RegInit(0.U(8.W))

  val lrscCount = RegInit(0.U(log2Ceil(LRSCCycles).W))
  val lrscValid = lrscCount > LRSCBackOff.U
  val lrscAddr = Reg(UInt())

  val s3_s_amoalu = RegInit(false.B)
  val s3_lr = !s3_req.probe && s3_req.isAMO && s3_req.cmd === M_XLR
  val s3_sc = !s3_req.probe && s3_req.isAMO && s3_req.cmd === M_XSC
  val s3_cas = !s3_req.probe && s3_req.isAMO && isAMOCAS(s3_req.cmd)
  val s3_lrsc_addr_match = lrscValid && lrscAddr === get_block_addr(s3_req.addr)
  val debugS3ScFailAddrMatch = s3_sc && lrscAddr === get_block_addr(s3_req.addr) && !lrscValid

  s3_sc_fail  := s3_sc && (!s3_lrsc_addr_match || !s3_hit)
  val s3_cas_fail = s3_cas && (FillInterleaved(8, s3_req.amoMask) & (s3_req.amoCmp ^ s3_amo_resp_data)) =/= 0.U

  val s3_can_do_amo = (s3_req.miss && !s3_req.probe && s3_req.isAMO) || s3_amo_hit
  val s3_can_do_amo_write = s3_can_do_amo && isWrite(s3_req.cmd) && !s3_sc_fail && !s3_cas_fail

  when (s3_valid && (s3_lr || s3_sc)) {
    when (s3_can_do_amo && s3_lr) {
      lrscCount := (LRSCCycles - 1).U
      lrscAddr := get_block_addr(s3_req.addr)
    } .otherwise {
      lrscCount := 0.U
    }
  }.elsewhen (io.invalidResvSet) {
    // when we release this block,
    // we invalidate this reservation set
    lrscCount := 0.U
  }.elsewhen (lrscCount > 0.U) {
    lrscCount := lrscCount - 1.U
  }


  io.lrscLockedBlock.valid := lrscValid
  io.lrscLockedBlock.bits  := lrscAddr
  io.blockLr := GatedValidRegNext(lrscCount > 0.U)

  // When we update update_resv_set, block all probe req in the next cycle
  // It should give Probe reservation set addr compare an independent cycle,
  // which will lead to better timing
  io.updateResvSet := s3_valid && s3_lr && s3_can_do_amo

  when (s3_valid) {
    when (s3_req.addr === debugScFailAddr) {
      when (s3_sc_fail) {
        debugScFailCnt := debugScFailCnt + 1.U
      } .elsewhen (s3_sc) {
        debugScFailCnt := 0.U
      }
    } .otherwise {
      when (s3_sc_fail) {
        debugScFailAddr := s3_req.addr
        debugScFailCnt  := 1.U
      }
    }
  }
  XSWarn(debugScFailCnt > 100.U, "L1DCache failed too many SCs in a row")

  when (s3_valid) {
    when (s3_req.addr === debugScFailAddr) {
      when (debugS3ScFailAddrMatch) {
        debugScAddrMatchFailCnt := debugScAddrMatchFailCnt + 1.U
      } .elsewhen (s3_sc) {
        debugScAddrMatchFailCnt := 0.U
      }
    } .otherwise {
      when (s3_sc_fail) {
        debugScAddrMatchFailCnt  := 1.U
      }
    }
  }
  XSError(debugScAddrMatchFailCnt > 100.U, "L1DCache failed too many SCs in a row, resv set addr always match")


  val updateData = s3_req.miss || s3_store_hit || s3_can_do_amo_write

  // generate write data
  // AMO hits
  val doAmoalu = amoWaitAmoalu && s3_valid && !s3_s_amoalu
  val amoalu   = Module(new AMOALU(wordBits))
  amoalu.io.mask := s3_req.amoMask
  amoalu.io.cmd  := s3_req.cmd
  amoalu.io.lhs  := s3_data_word
  amoalu.io.rhs  := s3_req.amoData

  // merge amo write data
  val s3_amo_data_merged = Wire(Vec(DCacheBanks, UInt(DCacheSRAMRowBits.W))) // exclude AMOCAS
  val s3_sc_data_merged = Wire(Vec(DCacheBanks, UInt(DCacheSRAMRowBits.W)))
  val s3_cas_data_merged = Wire(Vec(DCacheBanks, UInt(DCacheSRAMRowBits.W)))
  for (i <- 0 until DCacheBanks) {
    val oldData = s3_store_data_merged(i)
    val wordPieceSel = (0 until DCacheWordBankCount).map { offset =>
      i.U === s3_word_bank_base + offset.U
    }
    val quadPieceSel = (0 until DCacheQuadWordBankCount).map { offset =>
      i.U === s3_quad_word_bank_base + offset.U
    }
    s3_amo_data_merged(i) := mergePutData(
      oldData,
      selectDataPiece(amoalu.io.out, wordPieceSel, DCacheWordBankCount),
      selectFullMask(wordPieceSel)
    )
    s3_sc_data_merged(i) := mergePutData(
      oldData,
      selectDataPiece(s3_req.amoData, wordPieceSel, DCacheWordBankCount),
      selectMaskPiece(s3_req.amoMask, wordPieceSel, DCacheWordBankCount)
    )
    s3_cas_data_merged(i) := mergePutData(
      oldData = oldData,
      newData = Mux(
        isAMOCASQ(s3_req.cmd),
        selectDataPiece(s3_req.amoData, quadPieceSel, DCacheQuadWordBankCount),
        selectDataPiece(s3_req.amoData, wordPieceSel, DCacheWordBankCount)
      ),
      wmask = Mux(
        !s3_cas_fail,
        Mux(
          isAMOCASQ(s3_req.cmd),
          selectMaskPiece(s3_req.amoMask, quadPieceSel, DCacheQuadWordBankCount),
          selectMaskPiece(s3_req.amoMask, wordPieceSel, DCacheWordBankCount)
        ),
        0.U(DCacheSRAMRowBytes.W)
      )
    )
  }
  val s3_amo_data_merged_reg = RegEnable(s3_amo_data_merged, doAmoalu)
  val missWb = s3_req.miss && s3_need_replacement && s3_coh.state =/= ClientStates.Nothing
  val probeWb = s3_req.probe
  val replaceWb = s3_req.replace
  val needWb = missWb || probeWb || replaceWb

  val writebackParam = Mux(probeWb, probe_shrink_param, miss_shrink_param)
  val writebackData = if (dcacheParameters.alwaysReleaseData) {
    s3_tag_match && s3_req.probe && s3_req.probeNeedData ||
      s3_coh === ClientStates.Dirty || (missWb || replaceWb) && s3_coh.state =/= ClientStates.Nothing
  } else {
    s3_tag_match && s3_req.probe && s3_req.probeNeedData || s3_coh === ClientStates.Dirty
  }

  val s3_probe_can_go = s3_req.probe && io.wb.ready && (io.metaWrite.ready || !probeUpdateMeta)
  val s3_store_can_go = s3_req.source === STORE_SOURCE.U && !s3_req.probe && (io.metaWrite.ready || !storeUpdateMeta) && (io.dataWrite.ready || !updateData) && !s3_req.miss
  val s3_prefetch_can_go = s3_req.isPrefetch && !s3_req.replace && !s3_req.probe && !s3_req.miss && (io.metaWrite.ready || !updateMeta) && (io.dataWrite.ready || !updateData)
  val s3_amo_can_go = s3_amo_hit && (io.metaWrite.ready || !amoUpdateMeta) && (io.dataWrite.ready || !updateData) && (s3_s_amoalu || !amoWaitAmoalu) || s3_sc_fail
  val s3_miss_can_go = s3_req.miss &&
    (io.metaWrite.ready || !amoUpdateMeta) &&
    (io.dataWrite.ready || !updateData) &&
    (s3_s_amoalu || !amoWaitAmoalu) &&
    io.tagWrite.ready &&
    io.wb.ready
  val s3_replace_nothing = s3_req.replace && s3_coh.state === ClientStates.Nothing
  val s3_replace_can_go = s3_req.replace && (s3_replace_nothing || io.wb.ready)
  val s3_can_go = s3_probe_can_go || s3_store_can_go || s3_amo_can_go || s3_miss_can_go || s3_replace_can_go || s3_prefetch_can_go
  val s3_update_data_cango = s3_store_can_go || s3_amo_can_go || s3_miss_can_go // used to speed up data_write gen
  val s3_fire = s3_valid && s3_can_go
  when (s2_fire_to_s3) {
    s3_valid := true.B
  }.elsewhen (s3_fire) {
    s3_valid := false.B
  }
  when (doAmoalu) { s3_s_amoalu := true.B }
  when (s3_fire) { s3_s_amoalu := false.B }

  val s3_probe_new_coh = probe_new_coh
  val newCoh = Mux(
    missUpdateMeta,
    missNewCoh,
    Mux(
      probeUpdateMeta,
      s3_probe_new_coh,
      Mux(
        storeUpdateMeta || amoUpdateMeta,
        s3_new_hit_coh,
        ClientMetadata.onReset
      )
    )
  )
  val bankedWmask = Mux(
    s3_req.miss,
    bankedFullWmask,
    Mux(
      s3_store_hit,
      s3_banked_store_wmask,
      Mux(
        s3_can_do_amo_write,
        Mux(
          isAMOCASQ(s3_req.cmd),
          bankMaskFromBase(quadWordBankBase(s3_req.quadWordIdx), DCacheQuadWordBankCount),
          bankMaskFromBase(wordBankBase(s3_req.wordIdx), DCacheWordBankCount)
        ),
        bankedNoneWmask
      )
    )
  )
  assert(!(s3_valid && bankedWmask.orR && !updateData))

  for (i <- 0 until DCacheBanks) {
    io.dataWriteDup(i).valid := s3_valid && s3_update_data_cango && updateData
    io.dataWriteDup(i).bits.wayEn := s3_way_en
    io.dataWriteDup(i).bits.addr := s3_req.vaddr
  }

  s3_ready := !s3_valid || s3_can_go
  s3_s0_set_conflict := s3_valid && s3_idx === s0_idx && !s3_is_prefetch
  s3_s0_set_conflict_store := s3_valid && s3_idx === storeIdx && !s3_is_prefetch
  //assert(RegNext(!s3_valid || !(s3_req.source === storeSource.U && !s3_req.probe) || s3_hit)) // miss store should never come to s3 ,fixed(reserve)

  io.metaRead.valid := req.valid
  io.metaRead.bits.idx := get_dcache_idx(s0_req.vaddr)
  io.metaRead.bits.wayEn := Mux(s0_req.replace, s0_req.replaceWayEn, ~0.U(nWays.W))

  io.tagRead.valid := req.valid && !s0_req.replace
  io.tagRead.bits.idx := get_dcache_idx(s0_req.vaddr)
  io.tagRead.bits.wayEn := ~0.U(nWays.W)

  io.dataReadIntend := s1_valid && s1_need_data
  io.dataReadline.valid := s1_valid && s1_need_data
  io.dataReadline.bits.rmask := s1_banked_rmask
  io.dataReadline.bits.wayEn := s1_way_en
  io.dataReadline.bits.way := s1_way
  io.dataReadline.bits.addr := s1_req.vaddr

  io.missReq.valid := s2_valid && s2_can_go_to_mq
  val missReq = io.missReq.bits
  missReq := DontCare
  missReq.source := s2_req.source
  missReq.pfSource := s2_req.pfSource
  missReq.cmd := s2_req.cmd
  missReq.addr := s2_req.addr
  missReq.vaddr := s2_req.vaddr
  missReq.storeData := s2_req.storeData
  missReq.storeMask := s2_req.storeMask
  missReq.wordIdx := s2_req.wordIdx
  missReq.amoData := s2_req.amoData
  missReq.amoMask := s2_req.amoMask
  missReq.amoCmp  := s2_req.amoCmp
  missReq.reqCoh := s2_hit_coh
  missReq.id := s2_req.id
  missReq.cancel := s2_grow_perm_fail
  missReq.pc := 0.U // MainPipe requests (Store Buffer writeback) don't have a single corresponding PC
  missReq.fullOverwrite := s2_req.isStore && s2_req.storeMask.andR
  missReq.isBtoT := s2_grow_perm
  missReq.occupyWay := s2_tag_ecc_match_way

  io.wbqConflictCheck.valid := s2_valid && s2_can_go_to_mq
  io.wbqConflictCheck.bits := s2_req.addr

  /**
    * `s2_req.isStore` includes miss requests from Sbuffer sent from MissQueue,
    * while `s2_is_store`` only requests from sbuffer.
    * In the case of `BtoT` fail, only requests from sbuffer are allowed to return replay response.
    */
  val s2_is_store = RegEnable(s1_is_store, s1_fire)
  val s2_is_amo = RegEnable(s1_is_amo, s1_fire)
  io.storeReplayResp.valid := s2_valid && (s2_can_go_to_mq && replay && s2_req.isStore || s2_grow_perm_fail && s2_is_store)
  io.storeReplayResp.bits.data := DontCare
  io.storeReplayResp.bits.miss := true.B // s2_can_go_to_mq && replay
  io.storeReplayResp.bits.replay := true.B // s2_grow_perm_fail
  io.storeReplayResp.bits.id := s2_req.id

  val mshrHandledStoreMiss = s2_valid && s2_can_go_to_mq && s2_req.isStore && !io.storeReplayResp.valid
  val mshrHandledStoreMissS3 = RegNext(mshrHandledStoreMiss)
  val mshrHandledStoreMissIdS3 = RegEnable(s2_req.id, mshrHandledStoreMiss)
  // If a store is miss and accepted by mshr, tell Sbuffer it is a "hit". Sbuffer releases the entry and mshr provides corresponding st-ld forwarding data.
  //                                      (1) real hit       (2) store miss and accepted by mshr
  io.storeHitResp.valid := s3_valid && s3_store_can_go || mshrHandledStoreMissS3
  io.storeHitResp.bits.data := DontCare
  io.storeHitResp.bits.miss := mshrHandledStoreMissS3
  io.storeHitResp.bits.replay := false.B
  io.storeHitResp.bits.id := Mux(mshrHandledStoreMissS3, mshrHandledStoreMissIdS3, s3_req.id)

  val atomicHitResp = Wire(new MainPipeResp)
  atomicHitResp.source := s3_req.source
  atomicHitResp.data := Mux(s3_sc, s3_sc_fail.asUInt, s3_amo_resp_data)
  atomicHitResp.miss := false.B
  atomicHitResp.missId := s3_req.missId
  atomicHitResp.error := s3_error_wb
  atomicHitResp.tlError := (s3_l2_error_wb.asUInt | s3_flag_error_beu.asUInt).asTypeOf(new TLError())
  atomicHitResp.replay := false.B
  atomicHitResp.ackMissQueue := s3_req.miss
  atomicHitResp.id := lrscValid
  val atomicReplayResp = Wire(new MainPipeResp)
  atomicReplayResp.source := s2_req.source
  atomicReplayResp.data := DontCare
  atomicReplayResp.miss := true.B
  atomicReplayResp.missId := DontCare
  atomicReplayResp.error := false.B
  atomicReplayResp.tlError := 0.U.asTypeOf(new TLError())
  atomicReplayResp.replay := true.B
  atomicReplayResp.ackMissQueue := false.B
  atomicReplayResp.id := DontCare

  val atomicReplayRespValid = s2_valid && (s2_can_go_to_mq && replay || s2_grow_perm_fail) && s2_req.isAMO
  val atomicHitRespValid = s3_valid && (s3_amo_can_go || s3_miss_can_go && s3_req.isAMO)

  io.atomicResp.valid := atomicReplayRespValid || atomicHitRespValid
  io.atomicResp.bits := Mux(atomicReplayRespValid, atomicReplayResp, atomicHitResp)

  val totalPrefetch = s2_fire && s2_is_prefetch
  val pfLateInCache = s2_fire && s2_hit && s2_is_prefetch

  io.prefetchStat.total_prefetch := totalPrefetch
  io.prefetchStat.pf_late_in_cache := pfLateInCache
  io.prefetchStat.pf_late_in_cache_source := s2_hit_prefetch
  io.prefetchStat.nack_prefetch := s2_valid && s2_can_go_to_mq && !io.missReq.ready && s2_is_prefetch
  io.prefetchStat.pf_source := s2_req.pfSource
  io.prefetchStat.hit_pf_in_cache := DontCare
  io.prefetchStat.hit_source := DontCare

  io.prefetchStat.demand_miss := DontCare
  io.prefetchStat.pollution := DontCare

  // io.replace_resp.valid := s3_fire && s3_req.replace
  // io.replace_resp.bits := s3_req.miss_id

  io.metaWrite.valid := s3_fire && updateMeta
  io.metaWrite.bits.idx := s3_idx
  io.metaWrite.bits.wayEn := s3_way_en
  io.metaWrite.bits.meta.coh := newCoh

  io.errorFlagWrite.valid := s3_fire && updateMeta && (s3_l2_error_wb.asUInt.orR || s3_req.miss)
  io.errorFlagWrite.bits.idx := s3_idx
  io.errorFlagWrite.bits.wayEn := s3_way_en
  io.errorFlagWrite.bits.error := s3_l2_error_wb

  // if we use (prefetch_flag && meta =/= ClientStates.Nothing) for prefetch check
  // prefetch_flag_write can be omited
  io.prefetchFlagWrite.valid := s3_fire && s3_req.miss
  io.prefetchFlagWrite.bits.idx := s3_idx
  io.prefetchFlagWrite.bits.wayEn := s3_way_en
  io.prefetchFlagWrite.bits.source := s3_req.pfSource

  io.latencyFlagWrite.valid := s3_fire && s3_req.miss
  io.latencyFlagWrite.bits.idx := s3_idx
  io.latencyFlagWrite.bits.wayEn := s3_way_en
  io.latencyFlagWrite.bits.latency := s3_refill_latency

  // regenerate repl_way & repl_coh
  io.bloomFilterQuery.set.valid := s2_fire_to_s3 && s2_req.miss && !isFromL1Prefetch(s2_repl_pf) && s2_repl_coh.isValid() && isFromL1Prefetch(s2_req.pfSource)
  io.bloomFilterQuery.set.bits.addr := io.bloomFilterQuery.set.bits.get_addr(Cat(s2_repl_tag, get_untag(s2_req.vaddr))) // the evict block address

  io.bloomFilterQuery.clr.valid := s3_fire && isFromL1Prefetch(s3_req.pfSource)
  io.bloomFilterQuery.clr.bits.addr := io.bloomFilterQuery.clr.bits.get_addr(s3_req.addr)

  XSPerfAccumulate("prefetch_write_valid", s3_fire && s3_req.miss)
  XSPerfAccumulate("prefetch_write_valid_pf", io.prefetchFlagWrite.valid && isFromL1Prefetch(s3_req.pfSource))
  XSPerfAccumulate("mainpipe_update_prefetchArray", io.prefetchFlagWrite.valid)
  XSPerfAccumulate("mainpipe_s2_miss_req", s2_valid && s2_req.miss)
  XSPerfAccumulate("mainpipe_s2_block_penalty", s2_valid && s2_req.miss && !io.refillInfo.valid)
  XSPerfAccumulate("mainpipe_s2_missqueue_replay", s2_valid && s2_can_go_to_mq_replay)
  XSPerfAccumulate("mainpipe_slot_conflict_1_2", (s1_idx === s2_idx && s1_way_en === s2_way_en && s1_req.miss && s2_req.miss && s1_valid && s2_valid ))
  XSPerfAccumulate("mainpipe_slot_conflict_1_3", (s1_idx === s3_idx && s1_way_en === s3_way_en && s1_req.miss && s3_req.miss && s1_valid && s3_valid))
  XSPerfAccumulate("mainpipe_slot_conflict_2_3", (s2_idx === s3_idx && s2_way_en === s3_way_en && s2_req.miss && s3_req.miss && s2_valid && s3_valid))
  // probe / replace will not update access bit
  io.accessFlagWrite.valid := s3_fire && !s3_req.probe && !s3_req.replace
  io.accessFlagWrite.bits.idx := s3_idx
  io.accessFlagWrite.bits.wayEn := s3_way_en
  // io.access_flag_write.bits.flag := true.B
  io.accessFlagWrite.bits.flag :=Mux(s3_req.miss, s3_req.access, true.B)

  io.tagWrite.valid := s3_fire && s3_req.miss
  io.tagWrite.bits.idx := s3_idx
  io.tagWrite.bits.wayEn := s3_way_en
  io.tagWrite.bits.tag := get_tag(s3_req.addr)
  io.tagWrite.bits.ecc := DontCare // generate ecc code in tagArray
  io.tagWrite.bits.vaddr := s3_req.vaddr

  io.tagWriteIntend := s3_req.miss && s3_valid
  XSPerfAccumulate("fake_tag_write_intend", io.tagWriteIntend && !io.tagWrite.valid)
  XSPerfAccumulate("mainpipe_tag_write", io.tagWrite.valid)

  io.replace.req.valid := s2_valid && s2_need_eviction && !s2_refill_tag_eq_way
  io.replace.req.bits.addr := get_block_addr(Cat(s2_tag, get_untag(s2_req.vaddr)))
  io.replace.req.bits.vaddr := s2_req.vaddr

  io.evictSet := addrToDcacheSet(s2_req.vaddr) // only use set index

  assert(!RegNext(io.tagWrite.valid && !io.tagWriteIntend))

  io.dataWrite.valid := s3_valid && s3_update_data_cango && updateData
  io.dataWrite.bits.wayEn := s3_way_en
  io.dataWrite.bits.addr := s3_req.vaddr
  io.dataWrite.bits.wmask := bankedWmask
  io.dataWrite.bits.data := Mux(
    amoWaitAmoalu,
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
  assert(RegNext(!io.tagWrite.valid || !s3_req.replace))
  assert(RegNext(!io.dataWrite.valid || !s3_req.replace))

  io.wb.valid := s3_valid && (
    // replace
    s3_req.replace && !s3_replace_nothing ||
    // probe can go to wbq
    s3_req.probe && (io.metaWrite.ready || !probeUpdateMeta) ||
      // amo miss can go to wbq
      s3_req.miss &&
        (io.metaWrite.ready || !amoUpdateMeta) &&
        (io.dataWrite.ready || !updateData) &&
        (s3_s_amoalu || !amoWaitAmoalu) &&
        io.tagWrite.ready
    ) && needWb

  io.wb.bits.addr := get_block_addr(Cat(s3_tag, get_untag(s3_req.vaddr)))
  io.wb.bits.param := writebackParam
  io.wb.bits.voluntary := s3_req.miss || s3_req.replace
  io.wb.bits.hasData := writebackData && !s3_tag_error_wb
  io.wb.bits.dirty := s3_coh === ClientStates.Dirty
  io.wb.bits.data := s3_data_line
  io.wb.bits.corrupt := s3_tag_error_wb || s3_data_error_wb
  io.wb.bits.delayRelease := s3_req.replace
  io.wb.bits.missId := s3_req.missId

  // update plru in main pipe s3
  io.replaceAccess.valid := GatedValidRegNext(s2_fire_to_s3) && !s3_req.probe && (s3_req.miss || ((s3_req.isAMO || s3_req.isStore) && s3_hit))
  io.replaceAccess.bits.set := s3_idx
  io.replaceAccess.bits.way := OHToUInt(s3_way_en)

  io.replaceWay.set.valid := GatedValidRegNext(s0_fire)
  io.replaceWay.set.bits := s1_idx
  io.replaceWay.dmWay := s1_dm_way

  // send evict hint to sms
  val smsAgtEvictValid = s2_valid && s2_req.miss && s2_fire_to_s3
  io.smsAgtEvictReq.valid := GatedValidRegNext(smsAgtEvictValid)
  io.smsAgtEvictReq.bits.vaddr := RegEnable(Cat(s2_repl_tag(tagBits - 1, 2), s2_req.vaddr(13,12), 0.U((VAddrBits - tagBits).W)), smsAgtEvictValid)

  // TODO: consider block policy of a finer granularity
  io.status.s0_set.valid := req.valid
  io.status.s0_set.bits := get_dcache_idx(s0_req.vaddr)
  io.status.s1.valid := s1_valid
  io.status.s1.bits.set := s1_idx
  io.status.s1.bits.wayEn := s1_way_en
  io.status.s2.valid := s2_valid && !s2_req.replace
  io.status.s2.bits.set := s2_idx
  io.status.s2.bits.wayEn := s2_way_en
  io.status.s3.valid := s3_valid && !s3_req.replace
  io.status.s3.bits.set := s3_idx
  io.status.s3.bits.wayEn := s3_way_en

  for ((s, i) <- io.statusDup.zipWithIndex) {
    s.s1.valid := s1_valid
    s.s1.bits.set := RegEnable(get_dcache_idx(s0_req.vaddr), s0_fire)
    s.s1.bits.wayEn := s1_way_en
    s.s2.valid := s2_valid && !RegEnable(s1_req.replace, s1_fire)
    s.s2.bits.set := RegEnable(get_dcache_idx(s1_req.vaddr), s1_fire)
    s.s2.bits.wayEn := s2_way_en
    s.s3.valid := s3_valid && !RegEnable(s2_req.replace, s2_fire_to_s3)
    s.s3.bits.set := RegEnable(get_dcache_idx(s2_req.vaddr), s2_fire_to_s3)
    s.s3.bits.wayEn := RegEnable(s2_way_en, s2_fire_to_s3)
  }
  dontTouch(io.statusDup)

  io.mainpipeInfo.s2_valid := s2_valid && s2_req.miss
  io.mainpipeInfo.s2_miss_id := s2_req.missId
  io.mainpipeInfo.s2_replay_to_mq := s2_can_go_to_mq_no_data
  io.mainpipeInfo.s2_evict_bto_t_way := s2_can_go_to_mq_evict_fail
  io.mainpipeInfo.s2_next_evict_way := PriorityEncoderOH(~io.btotWaysForSet)
  io.mainpipeInfo.s3_valid := s3_valid
  io.mainpipeInfo.s3_miss_id := s3_req.missId
  io.mainpipeInfo.s3_refill_resp := RegNext(s2_valid && s2_req.miss && s2_fire_to_s3)
  XSError(s2_valid && s2_way_en.andR, "s2_way_en should not be all 1")

  // report error to beu and csr, 1 cycle after read data resp
  io.error := 0.U.asTypeOf(ValidIO(new L1CacheErrorInfo))
  // report error, update error csr
  io.error.valid := s3_error_beu && GatedValidRegNext(s2_fire && !s2_should_not_report_ecc_error)
  // only tag_error and data_error will be reported to beu
  // l2_error should not be reported (l2 will report that)
  io.error.bits.report_to_beu := (s3_tag_error_beu || s3_data_error_beu) && RegNext(s2_fire)
  io.error.bits.paddr := s3_error_paddr_beu
  io.error.bits.source.tag := s3_tag_error_beu
  io.error.bits.source.data := s3_data_error_beu
  io.error.bits.source.l2 := s3_flag_error_beu.asUInt.orR || s3_l2_error_beu.asUInt.orR
  io.error.bits.opType.store := RegEnable(s2_req.isStore && !s2_req.probe, s2_fire)
  io.error.bits.opType.probe := RegEnable(s2_req.probe, s2_fire)
  io.error.bits.opType.release := RegEnable(s2_req.replace, s2_fire)
  io.error.bits.opType.atom := RegEnable(s2_req.isAMO && !s2_req.probe, s2_fire)

  val perfEvents = Seq(
    ("l1D_write_dcache_access", s2_fire && (s2_is_store || (s2_is_amo && isWrite(s2_req.cmd)))), // store_req (cacheline evited from Sbuffer to L1D) & amo write
    ("l1D_write_dcache_miss  ", s2_fire && (s2_is_store || (s2_is_amo && isWrite(s2_req.cmd)) && !s2_hit)),
    ("dcache_mp_req          ", s0_fire                                                      ),
    ("dcache_mp_total_penalty", PopCount(VecInit(Seq(s0_fire, s1_valid, s2_valid, s3_valid)))),
    ("s2_hw_pf_access", s2_fire && s2_is_prefetch),
    ("s2_hw_pf_miss", s2_fire && s2_is_prefetch && !s2_hit)
  )
  generatePerfEvent()
}
