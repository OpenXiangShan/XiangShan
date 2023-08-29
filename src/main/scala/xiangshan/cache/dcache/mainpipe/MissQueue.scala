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
import xiangshan._
import utils._
import utility._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.ClientStates._
import freechips.rocketchip.tilelink.MemoryOpCategories._
import freechips.rocketchip.tilelink.TLPermissions._
import difftest._
import coupledL2.{AliasKey, DirtyKey, PrefetchKey}
import mem.{AddPipelineReg}
import mem.trace._
import xiangshan.mem.prefetch._

class MissReqWoStoreData(implicit p: Parameters) extends DCacheBundle {
  val source = UInt(sourceTypeWidth.W)
  val pf_source = UInt(L1PfSourceBits.W)
  val cmd = UInt(M_SZ.W)
  val addr = UInt(PAddrBits.W)
  val vaddr = UInt(VAddrBits.W)
  val way_en = UInt(DCacheWays.W)
  val pc = UInt(VAddrBits.W)

  // store
  val full_overwrite = Bool()

  // which word does amo work on?
  val word_idx = UInt(log2Up(blockWords).W)
  val amo_data = UInt(DataBits.W)
  val amo_mask = UInt((DataBits / 8).W)

  val req_coh = new ClientMetadata
  val replace_coh = new ClientMetadata
  val replace_tag = UInt(tagBits.W)
  val id = UInt(reqIdWidth.W)

  val replace_pf = UInt(L1PfSourceBits.W)

  // For now, miss queue entry req is actually valid when req.valid && !cancel
  // * req.valid is fast to generate
  // * cancel is slow to generate, it will not be used until the last moment
  //
  // cancel may come from the following sources:
  // 1. miss req blocked by writeback queue: 
  //      a writeback req of the same address is in progress
  // 2. pmp check failed
  val cancel = Bool() // cancel is slow to generate, it will cancel missreq.valid

  // Req source decode
  // Note that req source is NOT cmd type
  // For instance, a req which isFromPrefetch may have R or W cmd
  def isFromLoad = source === LOAD_SOURCE.U
  def isFromStore = source === STORE_SOURCE.U
  def isFromAMO = source === AMO_SOURCE.U
  def isFromPrefetch = source >= DCACHE_PREFETCH_SOURCE.U
  def isPrefetchWrite = source === DCACHE_PREFETCH_SOURCE.U && cmd === MemoryOpConstants.M_PFW
  def isPrefetchRead = source === DCACHE_PREFETCH_SOURCE.U && cmd === MemoryOpConstants.M_PFR
  def hit = req_coh.isValid()
}

class MissReqStoreData(implicit p: Parameters) extends DCacheBundle {
  // store data and store mask will be written to miss queue entry 
  // 1 cycle after req.fire() and meta write
  val store_data = UInt((cfg.blockBytes * 8).W)
  val store_mask = UInt(cfg.blockBytes.W)
}

class MissReq(implicit p: Parameters) extends MissReqWoStoreData {
  // store data and store mask will be written to miss queue entry 
  // 1 cycle after req.fire() and meta write
  val store_data = UInt((cfg.blockBytes * 8).W)
  val store_mask = UInt(cfg.blockBytes.W)

  def toMissReqStoreData(): MissReqStoreData = {
    val out = Wire(new MissReqStoreData)
    out.store_data := store_data
    out.store_mask := store_mask
    out
  }

  def toMissReqWoStoreData(): MissReqWoStoreData = {
    val out = Wire(new MissReqWoStoreData)
    out.source := source
    out.replace_pf := replace_pf
    out.pf_source := pf_source
    out.cmd := cmd
    out.addr := addr
    out.vaddr := vaddr
    out.way_en := way_en
    out.full_overwrite := full_overwrite
    out.word_idx := word_idx
    out.amo_data := amo_data
    out.amo_mask := amo_mask
    out.req_coh := req_coh
    out.replace_coh := replace_coh
    out.replace_tag := replace_tag
    out.id := id
    out.cancel := cancel
    out.pc := pc
    out
  }
}

class MissResp(implicit p: Parameters) extends DCacheBundle {
  val id = UInt(log2Up(cfg.nMissEntries).W)
  // cache miss request is handled by miss queue, either merged or newly allocated
  val handled = Bool()
  // cache req missed, merged into one of miss queue entries
  // i.e. !miss_merged means this access is the first miss for this cacheline
  val merged = Bool()
  val repl_way_en = UInt(DCacheWays.W)
}


/** 
  * miss queue enq logic: enq is now splited into 2 cycles
  *  +---------------------------------------------------------------------+    pipeline reg  +-------------------------+
  *  +         s0: enq source arbiter, judge mshr alloc or merge           +     +-------+    + s1: real alloc or merge +
  *  +                      +-----+          primary_fire?       ->        +     | alloc |    +                         +
  *  + mainpipe  -> req0 -> |     |          secondary_fire?     ->        +     | merge |    +                         +
  *  + loadpipe0 -> req1 -> | arb | -> req                       ->        +  -> | req   | -> +                         +
  *  + loadpipe1 -> req2 -> |     |          mshr id             ->        +     | id    |    +                         +
  *  +                      +-----+                                        +     +-------+    +                         +
  *  +---------------------------------------------------------------------+                  +-------------------------+                  
  */

// a pipeline reg between MissReq and MissEntry
class MissReqPipeRegBundle(edge: TLEdgeOut)(implicit p: Parameters) extends DCacheBundle {
  val req           = new MissReq
  // this request is about to merge to an existing mshr
  val merge         = Bool()
  // this request is about to allocate a new mshr
  val alloc         = Bool()
  val mshr_id       = UInt(log2Up(cfg.nMissEntries).W)

  def reg_valid(): Bool = {
    (merge || alloc)
  }

  def matched(new_req: MissReq): Bool = {
    val block_match = get_block(req.addr) === get_block(new_req.addr)
    block_match && reg_valid() && !(req.isFromPrefetch)
  }

  def prefetch_late_en(new_req: MissReqWoStoreData, new_req_valid: Bool): Bool = {
    val block_match = get_block(req.addr) === get_block(new_req.addr)
    new_req_valid && alloc && block_match && (req.isFromPrefetch) && !(new_req.isFromPrefetch)
  }

  def reject_req(new_req: MissReq): Bool = {
    val block_match = get_block(req.addr) === get_block(new_req.addr)
    val alias_match = is_alias_match(req.vaddr, new_req.vaddr)
    val merge_load = (req.isFromLoad || req.isFromStore || req.isFromPrefetch) && new_req.isFromLoad
    // store merge to a store is disabled, sbuffer should avoid this situation, as store to same address should preserver their program order to match memory model
    val merge_store = (req.isFromLoad || req.isFromPrefetch) && new_req.isFromStore

    val set_match = addr_to_dcache_set(req.vaddr) === addr_to_dcache_set(new_req.vaddr)
    val way_match = req.way_en === new_req.way_en
    Mux(
        alloc,
        Mux(
            block_match,
            !alias_match || !(merge_load || merge_store),
            set_match && way_match
          ),
        false.B
      )
  }

  def merge_req(new_req: MissReq): Bool = {
    val block_match = get_block(req.addr) === get_block(new_req.addr)
    val alias_match = is_alias_match(req.vaddr, new_req.vaddr)
    val merge_load = (req.isFromLoad || req.isFromStore || req.isFromPrefetch) && new_req.isFromLoad
    // store merge to a store is disabled, sbuffer should avoid this situation, as store to same address should preserver their program order to match memory model
    val merge_store = (req.isFromLoad || req.isFromPrefetch) && new_req.isFromStore
    Mux(
        alloc,
        block_match && alias_match && (merge_load || merge_store),
        false.B
      )
  }
  
  // send out acquire as soon as possible
  // if a new store miss req is about to merge into this pipe reg, don't send acquire now
  def can_send_acquire(valid: Bool, new_req: MissReq): Bool = {
    alloc && !(valid && merge_req(new_req) && new_req.isFromStore)
  }

  def get_acquire(l2_pf_store_only: Bool): TLBundleA = {
    val acquire = Wire(new TLBundleA(edge.bundle))
    val grow_param = req.req_coh.onAccess(req.cmd)._2
    val acquireBlock = edge.AcquireBlock(
      fromSource = mshr_id,
      toAddress = get_block_addr(req.addr),
      lgSize = (log2Up(cfg.blockBytes)).U,
      growPermissions = grow_param
    )._2
    val acquirePerm = edge.AcquirePerm(
      fromSource = mshr_id,
      toAddress = get_block_addr(req.addr),
      lgSize = (log2Up(cfg.blockBytes)).U,
      growPermissions = grow_param
    )._2
    acquire := Mux(req.full_overwrite, acquirePerm, acquireBlock)
    // resolve cache alias by L2
    acquire.user.lift(AliasKey).foreach( _ := req.vaddr(13, 12))
    // trigger prefetch
    acquire.user.lift(PrefetchKey).foreach(_ := Mux(l2_pf_store_only, req.isFromStore, true.B))
    // req source
    when(req.isFromLoad) {
      acquire.user.lift(ReqSourceKey).foreach(_ := MemReqSource.CPULoadData.id.U)
    }.elsewhen(req.isFromStore) {
      acquire.user.lift(ReqSourceKey).foreach(_ := MemReqSource.CPUStoreData.id.U)
    }.elsewhen(req.isFromAMO) {
      acquire.user.lift(ReqSourceKey).foreach(_ := MemReqSource.CPUAtomicData.id.U)
    }.otherwise {
      acquire.user.lift(ReqSourceKey).foreach(_ := MemReqSource.L1DataPrefetch.id.U)
    }

    acquire
  }
}

class MissEntry(edge: TLEdgeOut)(implicit p: Parameters) extends DCacheModule {
  val io = IO(new Bundle() {
    val hartId = Input(UInt(8.W))
    // MSHR ID
    val id = Input(UInt(log2Up(cfg.nMissEntries).W))
    // client requests
    // MSHR update request, MSHR state and addr will be updated when req.fire()
    val req = Flipped(ValidIO(new MissReqWoStoreData))
    // pipeline reg
    val miss_req_pipe_reg = Input(new MissReqPipeRegBundle(edge))
    // allocate this entry for new req
    val primary_valid = Input(Bool())
    // this entry is free and can be allocated to new reqs
    val primary_ready = Output(Bool())
    // this entry is busy, but it can merge the new req
    val secondary_ready = Output(Bool())
    // this entry is busy and it can not merge the new req
    val secondary_reject = Output(Bool())
    // way selected for replacing, used to support plru update
    val repl_way_en = Output(UInt(DCacheWays.W))

    // bus
    val mem_acquire = DecoupledIO(new TLBundleA(edge.bundle))
    val mem_grant = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))
    val mem_finish = DecoupledIO(new TLBundleE(edge.bundle))

    // send refill info to load queue 
    val refill_to_ldq = ValidIO(new Refill)

    // refill pipe
    val refill_pipe_req = DecoupledIO(new RefillPipeReq)
    val refill_pipe_resp = Input(Bool())

    // replace pipe
    val replace_pipe_req = DecoupledIO(new MainPipeReq)
    val replace_pipe_resp = Input(Bool())

    // main pipe: amo miss
    val main_pipe_req = DecoupledIO(new MainPipeReq)
    val main_pipe_resp = Input(Bool())

    val block_addr = ValidIO(UInt(PAddrBits.W))

    val debug_early_replace = ValidIO(new Bundle() {
      // info about the block that has been replaced
      val idx = UInt(idxBits.W) // vaddr
      val tag = UInt(tagBits.W) // paddr
    })

    val req_handled_by_this_entry = Output(Bool())

    val forwardInfo = Output(new MissEntryForwardIO)
    val l2_pf_store_only = Input(Bool())

    // whether the pipeline reg has send out an acquire
    val acquire_fired_by_pipe_reg = Input(Bool())
    val memSetPattenDetected = Input(Bool())

    val perf_pending_prefetch = Output(Bool())
    val perf_pending_normal   = Output(Bool())

    val rob_head_query = new DCacheBundle {
      val vaddr = Input(UInt(VAddrBits.W))
      val query_valid = Input(Bool())

      val resp = Output(Bool())

      def hit(e_vaddr: UInt): Bool = {
        require(e_vaddr.getWidth == VAddrBits)
        query_valid && vaddr(VAddrBits - 1, DCacheLineOffset) === e_vaddr(VAddrBits - 1, DCacheLineOffset)
      }
    }

    val latency_monitor = new DCacheBundle {
      val load_miss_refilling  = Output(Bool())
      val store_miss_refilling = Output(Bool())
      val amo_miss_refilling   = Output(Bool())
      val pf_miss_refilling    = Output(Bool())
    }

    val prefetch_info = new DCacheBundle {
      val late_prefetch = Output(Bool())
    }
    val nMaxPrefetchEntry = Input(UInt(64.W))
    val matched = Output(Bool())
  })

  assert(!RegNext(io.primary_valid && !io.primary_ready))

  val req = Reg(new MissReqWoStoreData)
  val req_primary_fire = Reg(new MissReqWoStoreData) // for perf use
  val req_store_mask = Reg(UInt(cfg.blockBytes.W))
  val req_valid = RegInit(false.B)
  val set = addr_to_dcache_set(req.vaddr)

  val miss_req_pipe_reg_bits = io.miss_req_pipe_reg.req

  val input_req_is_prefetch = isPrefetch(miss_req_pipe_reg_bits.cmd)

  val s_acquire = RegInit(true.B)
  val s_grantack = RegInit(true.B)
  val s_replace_req = RegInit(true.B)
  val s_refill = RegInit(true.B)
  val s_mainpipe_req = RegInit(true.B)

  val w_grantfirst = RegInit(true.B)
  val w_grantlast = RegInit(true.B)
  val w_replace_resp = RegInit(true.B)
  val w_refill_resp = RegInit(true.B)
  val w_mainpipe_resp = RegInit(true.B)

  val release_entry = s_grantack && w_refill_resp && w_mainpipe_resp

  val acquire_not_sent = !s_acquire && !io.mem_acquire.ready
  val data_not_refilled = !w_grantfirst

  val error = RegInit(false.B)
  val prefetch = RegInit(false.B)
  val access = RegInit(false.B)

  val should_refill_data_reg =  Reg(Bool())
  val should_refill_data = WireInit(should_refill_data_reg)

  // val full_overwrite = req.isFromStore && req_store_mask.andR
  val full_overwrite = Reg(Bool())

  val (_, _, refill_done, refill_count) = edge.count(io.mem_grant)
  val grant_param = Reg(UInt(TLPermissions.bdWidth.W))

  // refill data with store data, this reg will be used to store:
  // 1. store data (if needed), before l2 refill data
  // 2. store data and l2 refill data merged result (i.e. new cacheline taht will be write to data array)
  val refill_and_store_data = Reg(Vec(blockRows, UInt(rowBits.W)))
  // raw data refilled to l1 by l2
  val refill_data_raw = Reg(Vec(blockBytes/beatBytes, UInt(beatBits.W)))

  // allocate current miss queue entry for a miss req
  val primary_fire = WireInit(io.req.valid && io.primary_ready && io.primary_valid && !io.req.bits.cancel)
  // merge miss req to current miss queue entry
  val secondary_fire = WireInit(io.req.valid && io.secondary_ready && !io.req.bits.cancel)

  val req_handled_by_this_entry = primary_fire || secondary_fire

  // for perf use
  val secondary_fired = RegInit(false.B)

  io.perf_pending_prefetch := req_valid && prefetch && !secondary_fired
  io.perf_pending_normal   := req_valid && (!prefetch || secondary_fired)

  io.rob_head_query.resp   := io.rob_head_query.hit(req.vaddr) && req_valid

  io.req_handled_by_this_entry := req_handled_by_this_entry

  when (release_entry && req_valid) {
    req_valid := false.B
  }

  when (io.miss_req_pipe_reg.alloc) {
    assert(RegNext(primary_fire), "after 1 cycle of primary_fire, entry will be allocated")
    req_valid := true.B

    req := miss_req_pipe_reg_bits.toMissReqWoStoreData()
    req_primary_fire := miss_req_pipe_reg_bits.toMissReqWoStoreData()
    req.addr := get_block_addr(miss_req_pipe_reg_bits.addr)

    s_acquire := io.acquire_fired_by_pipe_reg
    s_grantack := false.B

    w_grantfirst := false.B
    w_grantlast := false.B

    when(miss_req_pipe_reg_bits.isFromStore) {
      req_store_mask := miss_req_pipe_reg_bits.store_mask
      for (i <- 0 until blockRows) {
        refill_and_store_data(i) := miss_req_pipe_reg_bits.store_data(rowBits * (i + 1) - 1, rowBits * i)
      }
    }
    full_overwrite := miss_req_pipe_reg_bits.isFromStore && miss_req_pipe_reg_bits.full_overwrite

    when (!miss_req_pipe_reg_bits.isFromAMO) {
      s_refill := false.B
      w_refill_resp := false.B
    }

    when (!miss_req_pipe_reg_bits.hit && miss_req_pipe_reg_bits.replace_coh.isValid() && !miss_req_pipe_reg_bits.isFromAMO) {
      s_replace_req := false.B
      w_replace_resp := false.B
    }

    when (miss_req_pipe_reg_bits.isFromAMO) {
      s_mainpipe_req := false.B
      w_mainpipe_resp := false.B
    }

    should_refill_data_reg := miss_req_pipe_reg_bits.isFromLoad
    error := false.B
    prefetch := input_req_is_prefetch && !io.miss_req_pipe_reg.prefetch_late_en(io.req.bits, io.req.valid)
    access := false.B
    secondary_fired := false.B
  }

  when (io.miss_req_pipe_reg.merge) {
    assert(RegNext(secondary_fire) || RegNext(RegNext(primary_fire)), "after 1 cycle of secondary_fire or 2 cycle of primary_fire, entry will be merged")
    assert(miss_req_pipe_reg_bits.req_coh.state <= req.req_coh.state || (prefetch && !access))
    assert(!(miss_req_pipe_reg_bits.isFromAMO || req.isFromAMO))
    // use the most uptodate meta
    req.req_coh := miss_req_pipe_reg_bits.req_coh

    assert(!miss_req_pipe_reg_bits.isFromPrefetch, "can not merge a prefetch req, late prefetch should always be ignored!")

    when (miss_req_pipe_reg_bits.isFromStore) {
      req := miss_req_pipe_reg_bits
      req.addr := get_block_addr(miss_req_pipe_reg_bits.addr)
      req.way_en := req.way_en
      req.replace_coh := req.replace_coh
      req.replace_tag := req.replace_tag
      req_store_mask := miss_req_pipe_reg_bits.store_mask
      for (i <- 0 until blockRows) {
        refill_and_store_data(i) := miss_req_pipe_reg_bits.store_data(rowBits * (i + 1) - 1, rowBits * i)
      }
      full_overwrite := miss_req_pipe_reg_bits.isFromStore && miss_req_pipe_reg_bits.full_overwrite
      assert(is_alias_match(req.vaddr, miss_req_pipe_reg_bits.vaddr), "alias bits should be the same when merging store")
    }

    should_refill_data := should_refill_data_reg || miss_req_pipe_reg_bits.isFromLoad
    should_refill_data_reg := should_refill_data
    when (!input_req_is_prefetch) {
      access := true.B // when merge non-prefetch req, set access bit
    }
    secondary_fired := true.B
  }

  when (io.mem_acquire.fire()) {
    s_acquire := true.B
  }

  // merge data refilled by l2 and store data, update miss queue entry, gen refill_req
  val new_data = Wire(Vec(blockRows, UInt(rowBits.W)))
  val new_mask = Wire(Vec(blockRows, UInt(rowBytes.W)))
  // merge refilled data and store data (if needed)
  def mergePutData(old_data: UInt, new_data: UInt, wmask: UInt): UInt = {
    val full_wmask = FillInterleaved(8, wmask)
    (~full_wmask & old_data | full_wmask & new_data)
  }
  for (i <- 0 until blockRows) {
    // new_data(i) := req.store_data(rowBits * (i + 1) - 1, rowBits * i)
    new_data(i) := refill_and_store_data(i)
    // we only need to merge data for Store
    new_mask(i) := Mux(req.isFromStore, req_store_mask(rowBytes * (i + 1) - 1, rowBytes * i), 0.U)
  }

  val hasData = RegInit(true.B)
  val isDirty = RegInit(false.B)
  when (io.mem_grant.fire()) {
    w_grantfirst := true.B
    grant_param := io.mem_grant.bits.param
    when (edge.hasData(io.mem_grant.bits)) {
      // GrantData
      for (i <- 0 until beatRows) {
        val idx = (refill_count << log2Floor(beatRows)) + i.U
        val grant_row = io.mem_grant.bits.data(rowBits * (i + 1) - 1, rowBits * i)
        refill_and_store_data(idx) := mergePutData(grant_row, new_data(idx), new_mask(idx))
      }
      w_grantlast := w_grantlast || refill_done
      hasData := true.B
    }.otherwise {
      // Grant
      assert(full_overwrite)
      for (i <- 0 until blockRows) {
        refill_and_store_data(i) := new_data(i)
      }
      w_grantlast := true.B
      hasData := false.B
    }

    error := io.mem_grant.bits.denied || io.mem_grant.bits.corrupt || error

    refill_data_raw(refill_count) := io.mem_grant.bits.data
    isDirty := io.mem_grant.bits.echo.lift(DirtyKey).getOrElse(false.B)
  }

  when (io.mem_finish.fire()) {
    s_grantack := true.B
  }

  when (io.replace_pipe_req.fire()) {
    s_replace_req := true.B
  }

  when (io.replace_pipe_resp) {
    w_replace_resp := true.B
  }

  when (io.refill_pipe_req.fire()) {
    s_refill := true.B
  }

  when (io.refill_pipe_resp) {
    w_refill_resp := true.B
  }

  when (io.main_pipe_req.fire()) {
    s_mainpipe_req := true.B
  }

  when (io.main_pipe_resp) {
    w_mainpipe_resp := true.B
  }

  def before_req_sent_can_merge(new_req: MissReqWoStoreData): Bool = {
    acquire_not_sent && (req.isFromLoad || req.isFromPrefetch) && (new_req.isFromLoad || new_req.isFromStore)
  }
  
  def before_data_refill_can_merge(new_req: MissReqWoStoreData): Bool = {
    data_not_refilled && (req.isFromLoad || req.isFromStore || req.isFromPrefetch) && new_req.isFromLoad
  }
  
  // Note that late prefetch will be ignored

  def should_merge(new_req: MissReqWoStoreData): Bool = {
    val block_match = get_block(req.addr) === get_block(new_req.addr)
    val alias_match = is_alias_match(req.vaddr, new_req.vaddr)
    block_match && alias_match &&
    (
      before_req_sent_can_merge(new_req) ||
      before_data_refill_can_merge(new_req)
    )
  }

  // store can be merged before io.mem_acquire.fire()
  // store can not be merged the cycle that io.mem_acquire.fire()
  // load can be merged before io.mem_grant.fire()
  //
  // TODO: merge store if possible? mem_acquire may need to be re-issued,
  // but sbuffer entry can be freed
  def should_reject(new_req: MissReqWoStoreData): Bool = {
    val block_match = get_block(req.addr) === get_block(new_req.addr)
    val set_match = set === addr_to_dcache_set(new_req.vaddr)
    val alias_match = is_alias_match(req.vaddr, new_req.vaddr)

    req_valid &&
      Mux(
        block_match,
        (!before_req_sent_can_merge(new_req) && !before_data_refill_can_merge(new_req)) || !alias_match,
        set_match && new_req.way_en === req.way_en
      )
  }

  // req_valid will be updated 1 cycle after primary_fire, so next cycle, this entry cannot accept a new req
  when(RegNext(io.id >= ((cfg.nMissEntries).U - io.nMaxPrefetchEntry))) {
    // can accept prefetch req
    io.primary_ready := !req_valid && !RegNext(primary_fire)
  }.otherwise {
    // cannot accept prefetch req except when a memset patten is detected
    io.primary_ready := !req_valid && (!io.req.bits.isFromPrefetch || io.memSetPattenDetected) && !RegNext(primary_fire)
  }
  io.secondary_ready := should_merge(io.req.bits)
  io.secondary_reject := should_reject(io.req.bits)
  io.repl_way_en := req.way_en

  // should not allocate, merge or reject at the same time
  assert(RegNext(PopCount(Seq(io.primary_ready, io.secondary_ready, io.secondary_reject)) <= 1.U))

  val refill_data_splited = WireInit(VecInit(Seq.tabulate(cfg.blockBytes * 8 / l1BusDataWidth)(i => {
    val data = refill_and_store_data.asUInt
    data((i + 1) * l1BusDataWidth - 1, i * l1BusDataWidth)
  })))
  // when granted data is all ready, wakeup lq's miss load
  io.refill_to_ldq.valid := RegNext(!w_grantlast && io.mem_grant.fire())
  io.refill_to_ldq.bits.addr := RegNext(req.addr + (refill_count << refillOffBits))
  io.refill_to_ldq.bits.data := refill_data_splited(RegNext(refill_count))
  io.refill_to_ldq.bits.error := RegNext(io.mem_grant.bits.corrupt || io.mem_grant.bits.denied)
  io.refill_to_ldq.bits.refill_done := RegNext(refill_done && io.mem_grant.fire())
  io.refill_to_ldq.bits.hasdata := hasData
  io.refill_to_ldq.bits.data_raw := refill_data_raw.asUInt
  io.refill_to_ldq.bits.id := io.id

  // if the entry has a pending merge req, wait for it
  // Note: now, only wait for store, because store may acquire T
  io.mem_acquire.valid := !s_acquire && !(io.miss_req_pipe_reg.merge && miss_req_pipe_reg_bits.isFromStore)
  val grow_param = req.req_coh.onAccess(req.cmd)._2
  val acquireBlock = edge.AcquireBlock(
    fromSource = io.id,
    toAddress = req.addr,
    lgSize = (log2Up(cfg.blockBytes)).U,
    growPermissions = grow_param
  )._2
  val acquirePerm = edge.AcquirePerm(
    fromSource = io.id,
    toAddress = req.addr,
    lgSize = (log2Up(cfg.blockBytes)).U,
    growPermissions = grow_param
  )._2
  io.mem_acquire.bits := Mux(full_overwrite, acquirePerm, acquireBlock)
  // resolve cache alias by L2
  io.mem_acquire.bits.user.lift(AliasKey).foreach( _ := req.vaddr(13, 12))
  // trigger prefetch
  io.mem_acquire.bits.user.lift(PrefetchKey).foreach(_ := Mux(io.l2_pf_store_only, req.isFromStore, true.B))
  // req source
  when(prefetch && !secondary_fired) {
    io.mem_acquire.bits.user.lift(ReqSourceKey).foreach(_ := MemReqSource.L1DataPrefetch.id.U)
  }.otherwise {
    when(req.isFromStore) {
      io.mem_acquire.bits.user.lift(ReqSourceKey).foreach(_ := MemReqSource.CPUStoreData.id.U)
    }.elsewhen(req.isFromLoad) {
      io.mem_acquire.bits.user.lift(ReqSourceKey).foreach(_ := MemReqSource.CPULoadData.id.U)
    }.elsewhen(req.isFromAMO) {
      io.mem_acquire.bits.user.lift(ReqSourceKey).foreach(_ := MemReqSource.CPUAtomicData.id.U)
    }.otherwise {
      io.mem_acquire.bits.user.lift(ReqSourceKey).foreach(_ := MemReqSource.L1DataPrefetch.id.U)
    }
  }
  require(nSets <= 256)

  io.mem_grant.ready := !w_grantlast && s_acquire

  val grantack = RegEnable(edge.GrantAck(io.mem_grant.bits), io.mem_grant.fire())
  assert(RegNext(!io.mem_grant.fire() || edge.isRequest(io.mem_grant.bits)))
  io.mem_finish.valid := !s_grantack && w_grantfirst
  io.mem_finish.bits := grantack

  io.replace_pipe_req.valid := !s_replace_req
  val replace = io.replace_pipe_req.bits
  replace := DontCare
  replace.miss := false.B
  replace.miss_id := io.id
  replace.miss_dirty := false.B
  replace.probe := false.B
  replace.probe_need_data := false.B
  replace.source := LOAD_SOURCE.U
  replace.vaddr := req.vaddr // only untag bits are needed
  replace.addr := Cat(req.replace_tag, 0.U(pgUntagBits.W)) // only tag bits are needed
  replace.store_mask := 0.U
  replace.replace := true.B
  replace.replace_way_en := req.way_en
  replace.error := false.B

  io.refill_pipe_req.valid := !s_refill && w_replace_resp && w_grantlast
  val refill = io.refill_pipe_req.bits
  refill.source := req.source
  refill.vaddr := req.vaddr
  refill.addr := req.addr
  refill.way_en := req.way_en
  refill.wmask := Mux(
    hasData || req.isFromLoad,
    ~0.U(DCacheBanks.W),
    VecInit((0 until DCacheBanks).map(i => get_mask_of_bank(i, req_store_mask).orR)).asUInt
  )
  refill.data := refill_and_store_data.asTypeOf((new RefillPipeReq).data)
  refill.miss_id := io.id
  refill.id := req.id
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
  refill.meta.coh := ClientMetadata(missCohGen(req.cmd, grant_param, isDirty))
  refill.error := error
  refill.prefetch := req.pf_source
  refill.access := access
  refill.alias := req.vaddr(13, 12) // TODO
  assert(!io.refill_pipe_req.valid || (refill.meta.coh =/= ClientMetadata(Nothing)), "refill modifies meta to Nothing, should not happen")

  io.main_pipe_req.valid := !s_mainpipe_req && w_grantlast
  io.main_pipe_req.bits := DontCare
  io.main_pipe_req.bits.miss := true.B
  io.main_pipe_req.bits.miss_id := io.id
  io.main_pipe_req.bits.miss_param := grant_param
  io.main_pipe_req.bits.miss_dirty := isDirty
  io.main_pipe_req.bits.miss_way_en := req.way_en
  io.main_pipe_req.bits.probe := false.B
  io.main_pipe_req.bits.source := req.source
  io.main_pipe_req.bits.cmd := req.cmd
  io.main_pipe_req.bits.vaddr := req.vaddr
  io.main_pipe_req.bits.addr := req.addr
  io.main_pipe_req.bits.store_data := refill_and_store_data.asUInt
  io.main_pipe_req.bits.store_mask := ~0.U(blockBytes.W)
  io.main_pipe_req.bits.word_idx := req.word_idx
  io.main_pipe_req.bits.amo_data := req.amo_data
  io.main_pipe_req.bits.amo_mask := req.amo_mask
  io.main_pipe_req.bits.error := error
  io.main_pipe_req.bits.id := req.id

  io.block_addr.valid := req_valid && w_grantlast && !w_refill_resp
  io.block_addr.bits := req.addr

  io.debug_early_replace.valid := BoolStopWatch(io.replace_pipe_resp, io.refill_pipe_req.fire())
  io.debug_early_replace.bits.idx := addr_to_dcache_set(req.vaddr)
  io.debug_early_replace.bits.tag := req.replace_tag

  io.forwardInfo.apply(req_valid, req.addr, refill_and_store_data, w_grantfirst, w_grantlast)

  io.matched := req_valid && (get_block(req.addr) === get_block(io.req.bits.addr)) && !prefetch
  io.prefetch_info.late_prefetch := io.req.valid && !(io.req.bits.isFromPrefetch) && req_valid && (get_block(req.addr) === get_block(io.req.bits.addr)) && prefetch

  when(io.prefetch_info.late_prefetch) {
    prefetch := false.B
  }

  // refill latency monitor
  val start_counting = RegNext(io.mem_acquire.fire) || (RegNextN(primary_fire, 2) && s_acquire)
  io.latency_monitor.load_miss_refilling  := req_valid && req_primary_fire.isFromLoad     && BoolStopWatch(start_counting, io.mem_grant.fire && !refill_done, true, true)
  io.latency_monitor.store_miss_refilling := req_valid && req_primary_fire.isFromStore    && BoolStopWatch(start_counting, io.mem_grant.fire && !refill_done, true, true)
  io.latency_monitor.amo_miss_refilling   := req_valid && req_primary_fire.isFromAMO      && BoolStopWatch(start_counting, io.mem_grant.fire && !refill_done, true, true)
  io.latency_monitor.pf_miss_refilling    := req_valid && req_primary_fire.isFromPrefetch && BoolStopWatch(start_counting, io.mem_grant.fire && !refill_done, true, true)

  XSPerfAccumulate("miss_req_primary", primary_fire)
  XSPerfAccumulate("miss_req_merged", secondary_fire)
  XSPerfAccumulate("load_miss_penalty_to_use",
    should_refill_data &&
      BoolStopWatch(primary_fire, io.refill_to_ldq.valid, true)
  )
  XSPerfAccumulate("main_pipe_penalty", BoolStopWatch(io.main_pipe_req.fire(), io.main_pipe_resp))
  XSPerfAccumulate("penalty_blocked_by_channel_A", io.mem_acquire.valid && !io.mem_acquire.ready)
  XSPerfAccumulate("penalty_waiting_for_channel_D", s_acquire && !w_grantlast && !io.mem_grant.valid)
  XSPerfAccumulate("penalty_waiting_for_channel_E", io.mem_finish.valid && !io.mem_finish.ready)
  XSPerfAccumulate("penalty_from_grant_to_refill", !w_refill_resp && w_grantlast)
  XSPerfAccumulate("prefetch_req_primary", primary_fire && io.req.bits.source === DCACHE_PREFETCH_SOURCE.U)
  XSPerfAccumulate("prefetch_req_merged", secondary_fire && io.req.bits.source === DCACHE_PREFETCH_SOURCE.U)
  XSPerfAccumulate("can_not_send_acquire_because_of_merging_store", !s_acquire && io.miss_req_pipe_reg.merge && miss_req_pipe_reg_bits.isFromStore)

  val (mshr_penalty_sample, mshr_penalty) = TransactionLatencyCounter(RegNext(RegNext(primary_fire)), release_entry)
  XSPerfHistogram("miss_penalty", mshr_penalty, mshr_penalty_sample, 0, 20, 1, true, true)
  XSPerfHistogram("miss_penalty", mshr_penalty, mshr_penalty_sample, 20, 100, 10, true, false)

  val load_miss_begin = primary_fire && io.req.bits.isFromLoad
  val refill_finished = RegNext(!w_grantlast && refill_done) && should_refill_data
  val (load_miss_penalty_sample, load_miss_penalty) = TransactionLatencyCounter(load_miss_begin, refill_finished) // not real refill finish time
  XSPerfHistogram("load_miss_penalty_to_use", load_miss_penalty, load_miss_penalty_sample, 0, 20, 1, true, true)
  XSPerfHistogram("load_miss_penalty_to_use", load_miss_penalty, load_miss_penalty_sample, 20, 100, 10, true, false)

  val (a_to_d_penalty_sample, a_to_d_penalty) = TransactionLatencyCounter(start_counting, RegNext(io.mem_grant.fire() && refill_done))
  XSPerfHistogram("a_to_d_penalty", a_to_d_penalty, a_to_d_penalty_sample, 0, 20, 1, true, true)
  XSPerfHistogram("a_to_d_penalty", a_to_d_penalty, a_to_d_penalty_sample, 20, 100, 10, true, false)
}

class MissQueue(edge: TLEdgeOut)(implicit p: Parameters) extends DCacheModule with HasPerfEvents {
  val io = IO(new Bundle {
    val hartId = Input(UInt(8.W))
    val req = Flipped(DecoupledIO(new MissReq))
    val resp = Output(new MissResp)
    val refill_to_ldq = ValidIO(new Refill)

    val mem_acquire = DecoupledIO(new TLBundleA(edge.bundle))
    val mem_grant = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))
    val mem_finish = DecoupledIO(new TLBundleE(edge.bundle))

    val refill_pipe_req = DecoupledIO(new RefillPipeReq)
    val refill_pipe_req_dup = Vec(nDupStatus, DecoupledIO(new RefillPipeReqCtrl))
    val refill_pipe_resp = Flipped(ValidIO(UInt(log2Up(cfg.nMissEntries).W)))

    val replace_pipe_req = DecoupledIO(new MainPipeReq)
    val replace_pipe_resp = Flipped(ValidIO(UInt(log2Up(cfg.nMissEntries).W)))

    val main_pipe_req = DecoupledIO(new MainPipeReq)
    val main_pipe_resp = Flipped(ValidIO(new AtomicsResp))

    // block probe
    val probe_addr = Input(UInt(PAddrBits.W))
    val probe_block = Output(Bool())

    val full = Output(Bool())

    // only for performance counter
    // This is valid when an mshr has finished replacing a block (w_replace_resp),
    // but hasn't received Grant from L2 (!w_grantlast)
    val debug_early_replace = Vec(cfg.nMissEntries, ValidIO(new Bundle() {
      // info about the block that has been replaced
      val idx = UInt(idxBits.W) // vaddr
      val tag = UInt(tagBits.W) // paddr
    }))

    // forward missqueue
    val forward = Vec(LoadPipelineWidth, new LduToMissqueueForwardIO)
    val l2_pf_store_only = Input(Bool())

    val memSetPattenDetected = Output(Bool())
    val lqEmpty = Input(Bool())

    val prefetch_info = new Bundle {
      val naive = new Bundle {
        val late_miss_prefetch = Output(Bool())
      }

      val fdp = new Bundle {
        val late_miss_prefetch = Output(Bool())
        val prefetch_monitor_cnt = Output(Bool())
        val total_prefetch = Output(Bool())
      }
    }

    val bloom_filter_query = new Bundle {
      val set = ValidIO(new BloomQueryBundle(BLOOM_FILTER_ENTRY_NUM))
      val clr = ValidIO(new BloomQueryBundle(BLOOM_FILTER_ENTRY_NUM))
    }

    val mq_enq_cancel = Output(Bool())
  })
  
  // 128KBL1: FIXME: provide vaddr for l2

  val entries = Seq.fill(cfg.nMissEntries)(Module(new MissEntry(edge)))

  val miss_req_pipe_reg = RegInit(0.U.asTypeOf(new MissReqPipeRegBundle(edge)))
  val acquire_from_pipereg = Wire(chiselTypeOf(io.mem_acquire))

  val primary_ready_vec = entries.map(_.io.primary_ready)
  val secondary_ready_vec = entries.map(_.io.secondary_ready)
  val secondary_reject_vec = entries.map(_.io.secondary_reject)
  val probe_block_vec = entries.map { case e => e.io.block_addr.valid && e.io.block_addr.bits === io.probe_addr }

  val merge = Cat(secondary_ready_vec ++ Seq(miss_req_pipe_reg.merge_req(io.req.bits))).orR
  val reject = Cat(secondary_reject_vec ++ Seq(miss_req_pipe_reg.reject_req(io.req.bits))).orR
  val alloc = !reject && !merge && Cat(primary_ready_vec).orR
  val accept = alloc || merge

  val req_mshr_handled_vec = entries.map(_.io.req_handled_by_this_entry)
  // merged to pipeline reg
  val req_pipeline_reg_handled = miss_req_pipe_reg.merge_req(io.req.bits)
  assert(PopCount(Seq(req_pipeline_reg_handled, VecInit(req_mshr_handled_vec).asUInt.orR)) <= 1.U, "miss req will either go to mshr or pipeline reg")
  assert(PopCount(req_mshr_handled_vec) <= 1.U, "Only one mshr can handle a req")
  io.resp.id := Mux(!req_pipeline_reg_handled, OHToUInt(req_mshr_handled_vec), miss_req_pipe_reg.mshr_id)
  io.resp.handled := Cat(req_mshr_handled_vec).orR || req_pipeline_reg_handled
  io.resp.merged := merge
  io.resp.repl_way_en := Mux(!req_pipeline_reg_handled, Mux1H(secondary_ready_vec, entries.map(_.io.repl_way_en)), miss_req_pipe_reg.req.way_en)

  /*  MissQueue enq logic is now splitted into 2 cycles
   *
   */ 
  miss_req_pipe_reg.req     := io.req.bits
  miss_req_pipe_reg.alloc   := alloc && io.req.valid && !io.req.bits.cancel
  miss_req_pipe_reg.merge   := merge && io.req.valid && !io.req.bits.cancel
  miss_req_pipe_reg.mshr_id := io.resp.id

  assert(PopCount(Seq(alloc && io.req.valid, merge && io.req.valid)) <= 1.U, "allocate and merge a mshr in same cycle!")

  val source_except_load_cnt = RegInit(0.U(10.W))
  when(VecInit(req_mshr_handled_vec).asUInt.orR || req_pipeline_reg_handled) {
    when(io.req.bits.isFromLoad) {
      source_except_load_cnt := 0.U
    }.otherwise {
      when(io.req.bits.isFromStore) {
        source_except_load_cnt := source_except_load_cnt + 1.U
      }
    }
  }
  val Threshold = 8
  val memSetPattenDetected = RegNext((source_except_load_cnt >= Threshold.U) && io.lqEmpty)

  io.memSetPattenDetected := memSetPattenDetected

  val forwardInfo_vec = VecInit(entries.map(_.io.forwardInfo))
  (0 until LoadPipelineWidth).map(i => {
    val id = io.forward(i).mshrid
    val req_valid = io.forward(i).valid
    val paddr = io.forward(i).paddr

    val (forward_mshr, forwardData) = forwardInfo_vec(id).forward(req_valid, paddr)
    io.forward(i).forward_result_valid := forwardInfo_vec(id).check(req_valid, paddr)
    io.forward(i).forward_mshr := forward_mshr
    io.forward(i).forwardData := forwardData
  })

  assert(RegNext(PopCount(secondary_ready_vec) <= 1.U))
//  assert(RegNext(PopCount(secondary_reject_vec) <= 1.U))
  // It is possible that one mshr wants to merge a req, while another mshr wants to reject it.
  // That is, a coming req has the same paddr as that of mshr_0 (merge),
  // while it has the same set and the same way as mshr_1 (reject).
  // In this situation, the coming req should be merged by mshr_0
//  assert(RegNext(PopCount(Seq(merge, reject)) <= 1.U))

  def select_valid_one[T <: Bundle](
    in: Seq[DecoupledIO[T]],
    out: DecoupledIO[T],
    name: Option[String] = None): Unit = {

    if (name.nonEmpty) { out.suggestName(s"${name.get}_select") }
    out.valid := Cat(in.map(_.valid)).orR
    out.bits := ParallelMux(in.map(_.valid) zip in.map(_.bits))
    in.map(_.ready := out.ready) 
    assert(!RegNext(out.valid && PopCount(Cat(in.map(_.valid))) > 1.U))
  }

  io.mem_grant.ready := false.B

  val nMaxPrefetchEntry = WireInit(Constantin.createRecord("nMaxPrefetchEntry" + p(XSCoreParamsKey).HartId.toString, initValue = 14.U))
  entries.zipWithIndex.foreach {
    case (e, i) =>
      val former_primary_ready = if(i == 0)
        false.B 
      else
        Cat((0 until i).map(j => entries(j).io.primary_ready)).orR
      
      e.io.hartId := io.hartId
      e.io.id := i.U
      e.io.l2_pf_store_only := io.l2_pf_store_only
      e.io.req.valid := io.req.valid
      e.io.primary_valid := io.req.valid && 
        !merge && 
        !reject && 
        !former_primary_ready &&
        e.io.primary_ready
      e.io.req.bits := io.req.bits.toMissReqWoStoreData()

      e.io.mem_grant.valid := false.B
      e.io.mem_grant.bits := DontCare
      when (io.mem_grant.bits.source === i.U) {
        e.io.mem_grant <> io.mem_grant
      }

      when(miss_req_pipe_reg.reg_valid() && miss_req_pipe_reg.mshr_id === i.U) {
        e.io.miss_req_pipe_reg := miss_req_pipe_reg
      }.otherwise {
        e.io.miss_req_pipe_reg       := DontCare
        e.io.miss_req_pipe_reg.merge := false.B
        e.io.miss_req_pipe_reg.alloc := false.B
      }

      e.io.acquire_fired_by_pipe_reg := acquire_from_pipereg.fire

      e.io.refill_pipe_resp := io.refill_pipe_resp.valid && io.refill_pipe_resp.bits === i.U
      e.io.replace_pipe_resp := io.replace_pipe_resp.valid && io.replace_pipe_resp.bits === i.U
      e.io.main_pipe_resp := io.main_pipe_resp.valid && io.main_pipe_resp.bits.ack_miss_queue && io.main_pipe_resp.bits.miss_id === i.U

      e.io.memSetPattenDetected := memSetPattenDetected
      e.io.nMaxPrefetchEntry := nMaxPrefetchEntry

      io.debug_early_replace(i) := e.io.debug_early_replace
      e.io.main_pipe_req.ready := io.main_pipe_req.ready
  }

  io.req.ready := accept
  io.mq_enq_cancel := io.req.bits.cancel
  io.refill_to_ldq.valid := Cat(entries.map(_.io.refill_to_ldq.valid)).orR
  io.refill_to_ldq.bits := ParallelMux(entries.map(_.io.refill_to_ldq.valid) zip entries.map(_.io.refill_to_ldq.bits))

  acquire_from_pipereg.valid := miss_req_pipe_reg.can_send_acquire(io.req.valid, io.req.bits)
  acquire_from_pipereg.bits := miss_req_pipe_reg.get_acquire(io.l2_pf_store_only)

  XSPerfAccumulate("acquire_fire_from_pipereg", acquire_from_pipereg.fire)
  XSPerfAccumulate("pipereg_valid", miss_req_pipe_reg.reg_valid())

  val acquire_sources = Seq(acquire_from_pipereg) ++ entries.map(_.io.mem_acquire)
  TLArbiter.lowest(edge, io.mem_acquire, acquire_sources:_*)
  TLArbiter.lowest(edge, io.mem_finish, entries.map(_.io.mem_finish):_*)

  // arbiter_with_pipereg_N_dup(entries.map(_.io.refill_pipe_req), io.refill_pipe_req,
  // io.refill_pipe_req_dup,
  // Some("refill_pipe_req"))
  val out_refill_pipe_req = Wire(Decoupled(new RefillPipeReq))
  val out_refill_pipe_req_ctrl = Wire(Decoupled(new RefillPipeReqCtrl))
  out_refill_pipe_req_ctrl.valid := out_refill_pipe_req.valid
  out_refill_pipe_req_ctrl.bits := out_refill_pipe_req.bits.getCtrl
  out_refill_pipe_req.ready := out_refill_pipe_req_ctrl.ready
  arbiter(entries.map(_.io.refill_pipe_req), out_refill_pipe_req, Some("refill_pipe_req"))
  for (dup <- io.refill_pipe_req_dup) {
    AddPipelineReg(out_refill_pipe_req_ctrl, dup, false.B)
  }
  AddPipelineReg(out_refill_pipe_req, io.refill_pipe_req, false.B)

  arbiter_with_pipereg(entries.map(_.io.replace_pipe_req), io.replace_pipe_req, Some("replace_pipe_req"))

  // amo's main pipe req out
  val main_pipe_req_vec = entries.map(_.io.main_pipe_req)
  io.main_pipe_req.valid := VecInit(main_pipe_req_vec.map(_.valid)).asUInt.orR
  io.main_pipe_req.bits := Mux1H(main_pipe_req_vec.map(_.valid), main_pipe_req_vec.map(_.bits))
  assert(PopCount(VecInit(main_pipe_req_vec.map(_.valid))) <= 1.U, "multi main pipe req")

  io.probe_block := Cat(probe_block_vec).orR

  io.full := ~Cat(entries.map(_.io.primary_ready)).andR

  // prefetch related
  io.prefetch_info.naive.late_miss_prefetch := io.req.valid && io.req.bits.isPrefetchRead && (miss_req_pipe_reg.matched(io.req.bits) || Cat(entries.map(_.io.matched)).orR)

  io.prefetch_info.fdp.late_miss_prefetch := (miss_req_pipe_reg.prefetch_late_en(io.req.bits.toMissReqWoStoreData(), io.req.valid) || Cat(entries.map(_.io.prefetch_info.late_prefetch)).orR)
  io.prefetch_info.fdp.prefetch_monitor_cnt := io.refill_pipe_req.fire
  io.prefetch_info.fdp.total_prefetch := alloc && io.req.valid && !io.req.bits.cancel && isFromL1Prefetch(io.req.bits.pf_source)

  io.bloom_filter_query.set.valid := alloc && io.req.valid && !io.req.bits.cancel && !isFromL1Prefetch(io.req.bits.replace_pf) && io.req.bits.replace_coh.isValid() && isFromL1Prefetch(io.req.bits.pf_source)
  io.bloom_filter_query.set.bits.addr := io.bloom_filter_query.set.bits.get_addr(Cat(io.req.bits.replace_tag, get_untag(io.req.bits.vaddr))) // the evict block address

  io.bloom_filter_query.clr.valid := io.refill_pipe_req.fire && isFromL1Prefetch(io.refill_pipe_req.bits.prefetch)
  io.bloom_filter_query.clr.bits.addr := io.bloom_filter_query.clr.bits.get_addr(io.refill_pipe_req.bits.addr)

  // L1MissTrace Chisel DB
  val debug_miss_trace = Wire(new L1MissTrace)
  debug_miss_trace.vaddr := io.req.bits.vaddr
  debug_miss_trace.paddr := io.req.bits.addr
  debug_miss_trace.source := io.req.bits.source
  debug_miss_trace.pc := io.req.bits.pc

  val isWriteL1MissQMissTable = WireInit(Constantin.createRecord("isWriteL1MissQMissTable" + p(XSCoreParamsKey).HartId.toString))
  val table = ChiselDB.createTable("L1MissQMissTrace_hart"+ p(XSCoreParamsKey).HartId.toString, new L1MissTrace)
  table.log(debug_miss_trace, isWriteL1MissQMissTable.orR && io.req.valid && !io.req.bits.cancel && alloc, "MissQueue", clock, reset)

  // Difftest
  if (env.EnableDifftest) {
    val difftest = Module(new DifftestRefillEvent)
    difftest.io.clock := clock
    difftest.io.coreid := io.hartId
    difftest.io.cacheid := 1.U
    difftest.io.valid := io.refill_to_ldq.valid && io.refill_to_ldq.bits.hasdata && io.refill_to_ldq.bits.refill_done
    difftest.io.addr := io.refill_to_ldq.bits.addr
    difftest.io.data := io.refill_to_ldq.bits.data_raw.asTypeOf(difftest.io.data)
  }

  // Perf count
  XSPerfAccumulate("miss_req", io.req.fire() && !io.req.bits.cancel)
  XSPerfAccumulate("miss_req_allocate", io.req.fire() && !io.req.bits.cancel && alloc)
  XSPerfAccumulate("miss_req_load_allocate", io.req.fire() && !io.req.bits.cancel && alloc && io.req.bits.isFromLoad)
  XSPerfAccumulate("miss_req_store_allocate", io.req.fire() && !io.req.bits.cancel && alloc && io.req.bits.isFromStore)
  XSPerfAccumulate("miss_req_amo_allocate", io.req.fire() && !io.req.bits.cancel && alloc && io.req.bits.isFromAMO)
  XSPerfAccumulate("miss_req_merge_load", io.req.fire() && !io.req.bits.cancel && merge && io.req.bits.isFromLoad)
  XSPerfAccumulate("miss_req_reject_load", io.req.valid && !io.req.bits.cancel && reject && io.req.bits.isFromLoad)
  XSPerfAccumulate("probe_blocked_by_miss", io.probe_block)
  XSPerfAccumulate("prefetch_primary_fire", io.req.fire() && !io.req.bits.cancel && alloc && io.req.bits.isFromPrefetch)
  XSPerfAccumulate("prefetch_secondary_fire", io.req.fire() && !io.req.bits.cancel && merge && io.req.bits.isFromPrefetch)
  XSPerfAccumulate("memSetPattenDetected", memSetPattenDetected)
  val max_inflight = RegInit(0.U((log2Up(cfg.nMissEntries) + 1).W))
  val num_valids = PopCount(~Cat(primary_ready_vec).asUInt)
  when (num_valids > max_inflight) {
    max_inflight := num_valids
  }
  // max inflight (average) = max_inflight_total / cycle cnt
  XSPerfAccumulate("max_inflight", max_inflight)
  QueuePerf(cfg.nMissEntries, num_valids, num_valids === cfg.nMissEntries.U)
  io.full := num_valids === cfg.nMissEntries.U
  XSPerfHistogram("num_valids", num_valids, true.B, 0, cfg.nMissEntries, 1)

  XSPerfHistogram("L1DMLP_CPUData", PopCount(VecInit(entries.map(_.io.perf_pending_normal)).asUInt), true.B, 0, cfg.nMissEntries, 1)
  XSPerfHistogram("L1DMLP_Prefetch", PopCount(VecInit(entries.map(_.io.perf_pending_prefetch)).asUInt), true.B, 0, cfg.nMissEntries, 1)
  XSPerfHistogram("L1DMLP_Total", num_valids, true.B, 0, cfg.nMissEntries, 1)

  XSPerfAccumulate("miss_load_refill_latency", PopCount(entries.map(_.io.latency_monitor.load_miss_refilling)))
  XSPerfAccumulate("miss_store_refill_latency", PopCount(entries.map(_.io.latency_monitor.store_miss_refilling)))
  XSPerfAccumulate("miss_amo_refill_latency", PopCount(entries.map(_.io.latency_monitor.amo_miss_refilling)))
  XSPerfAccumulate("miss_pf_refill_latency", PopCount(entries.map(_.io.latency_monitor.pf_miss_refilling)))

  val rob_head_miss_in_dcache = VecInit(entries.map(_.io.rob_head_query.resp)).asUInt.orR
  val sourceVaddr = WireInit(0.U.asTypeOf(new Valid(UInt(VAddrBits.W))))
  val lq_doing_other_replay = WireInit(false.B)

  ExcitingUtils.addSink(sourceVaddr, s"rob_head_vaddr_${coreParams.HartId}", ExcitingUtils.Perf)
  ExcitingUtils.addSink(lq_doing_other_replay, s"rob_head_other_replay_${coreParams.HartId}", ExcitingUtils.Perf)

  entries.foreach {
    case e => {
      e.io.rob_head_query.query_valid := sourceVaddr.valid
      e.io.rob_head_query.vaddr := sourceVaddr.bits
    }
  }

  // ExcitingUtils.addSource(!rob_head_miss_in_dcache && !lq_doing_other_replay, s"load_l1_cache_stall_without_bank_conflict_${coreParams.HartId}", ExcitingUtils.Perf, true)
  ExcitingUtils.addSource(rob_head_miss_in_dcache, s"load_l1_miss_${coreParams.HartId}", ExcitingUtils.Perf, true)

  val perfValidCount = RegNext(PopCount(entries.map(entry => (!entry.io.primary_ready))))
  val perfEvents = Seq(
    ("dcache_missq_req      ", io.req.fire()),
    ("dcache_missq_1_4_valid", (perfValidCount < (cfg.nMissEntries.U/4.U))),
    ("dcache_missq_2_4_valid", (perfValidCount > (cfg.nMissEntries.U/4.U)) & (perfValidCount <= (cfg.nMissEntries.U/2.U))),
    ("dcache_missq_3_4_valid", (perfValidCount > (cfg.nMissEntries.U/2.U)) & (perfValidCount <= (cfg.nMissEntries.U*3.U/4.U))),
    ("dcache_missq_4_4_valid", (perfValidCount > (cfg.nMissEntries.U*3.U/4.U))),
  )
  generatePerfEvent()
}
