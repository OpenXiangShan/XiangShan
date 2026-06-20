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
*
*
* Acknowledgement
*
* This implementation is inspired by several key papers:
* [1] David Kroft. "[Lockup-free instruction fetch/prefetch cache organization.]
* (https://dl.acm.org/doi/10.5555/800052.801868)" 8th Annual Symposium on Computer Architecture (ISCA). 1981.
***************************************************************************************/

package xiangshan.cache

import chisel3._
import chisel3.experimental.dataview._
import chisel3.util._
import xscache.coupledL2.{IsKeywordKey, MemBackTypeMM, MemPageTypeNC, PCKey, VaddrKey}
import difftest._
import freechips.rocketchip.tilelink._
import xscache.common.{AliasKey, DirtyKey, PrefetchKey}
import org.chipsalliance.cde.config.Parameters
import utility._
import xiangshan._
import xiangshan.mem.LqPtr
import xiangshan.mem.prefetch._
import xiangshan.mem.trace._
import xiangshan.mem.Bundles.SbufferForwardReq
import freechips.rocketchip.util.UIntToAugmentedUInt
import freechips.rocketchip.util.SeqToAugmentedSeq

class MissReqWoStoreData(implicit p: Parameters) extends DCacheBundle {
  val source = UInt(sourceTypeWidth.W)
  val pf_source = UInt(L1PfSourceBits.W)
  val cmd = UInt(M_SZ.W)
  val addr = UInt(PAddrBits.W)
  val vaddr = UInt(VAddrBits.W)
  val pc = UInt(VAddrBits.W)

  val lqIdx = new LqPtr
  // store
  val full_overwrite = Bool()

  // amo
  val word_idx = UInt(log2Up(blockWords).W)
  val amo_data   = UInt(QuadWordBits.W)
  val amo_mask   = UInt(QuadWordBytes.W)
  val amo_cmp    = UInt(QuadWordBits.W) // data to be compared in AMOCAS

  val req_coh = new ClientMetadata
  val id = UInt(reqIdWidth.W)

  /**
    * isBtoT is used to mark whether the current request requires BtoT permission.
    * When the number of BtoT-occupied ways in the same set exceeds nWays-2,
    * new BtoT requests for that set are blocked (via occupy_fail -> LoadPipe cancel).
    * Non-BtoT requests are not affected.
    */
  val isBtoT = Bool()
  /**
    * The way isBtoT requests to occupy
    */
  val occupy_way = UInt(nWays.W)

  // Enqueue logic uses req.valid && !cancel && !wbq_block_miss_req
  //
  // cancel is usually ready later than req.valid and is driven by whoever builds the MissReq:
  // - LoadPipe: io.lsu.s2_kill (dcacheKill: flush, exceptions incl. PMP-related faults, uncache, ...),
  //   plus s2_tag_error and s2_btot_occupy_fail
  // - StorePipe: io.lsu.s2_kill
  // - MainPipe (miss): s2_grow_perm_fail
  val cancel = Bool()

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

class MissQueueRefillInfo(implicit p: Parameters) extends MissReqStoreData {
  // refill_info for mainpipe req awake
  val miss_param = UInt(TLPermissions.bdWidth.W)
  val miss_dirty = Bool()
  val error      = new TLError()
  val refill_latency = UInt(LATENCY_WIDTH.W)
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
    this.viewAsSupertype(new MissReqWoStoreData)
  }
}

class MissResp(implicit p: Parameters) extends DCacheBundle {
  val id = UInt(log2Up(cfg.nMissEntries).W)
  // cache miss request is handled by miss queue, either merged or newly allocated
  val handled = Bool()
  // cache req missed, merged into one of miss queue entries
  // i.e. !miss_merged means this access is the first miss for this cacheline
  val merged = Bool()
}

class MissQueueBlockReqBundle(implicit p: Parameters) extends XSBundle {
  val addr = UInt(PAddrBits.W)
  val vaddr = UInt(VAddrBits.W)
}

class MissQueueBlockIO(implicit p: Parameters) extends XSBundle {
  val req = ValidIO(new MissQueueBlockReqBundle)
  val block = Input(Bool())
}

// for manually CSE
class MatchSignals(implicit p: Parameters) extends Bundle {
  val block_match = Bool()
  val alias_match = Bool()
  val set_match   = Bool()
  val merge_load  = Bool()
  val merge_store = Bool()
}

trait HasMissReqFunction extends HasDCacheParameters
 with HasL1CacheParameters {
  // implicit val p: Parameters

  def blockMatch(req: MissReqWoStoreData, new_req: MissReqWoStoreData): Bool = {
    get_block(req.addr) === get_block(new_req.addr)
  }

  def aliasMatch(req: MissReqWoStoreData, new_req: MissReqWoStoreData): Bool = {
    is_alias_match(req.vaddr, new_req.vaddr)
  }

  def setMatch(req: MissReqWoStoreData, new_req: MissReqWoStoreData): Bool = {
    addr_to_dcache_set(req.vaddr) === addr_to_dcache_set(new_req.vaddr)
  }

  // Passing in "req" is to accommodate two different scenarios: MissEntry and MissQueue
  def computeMatchSignals(req: MissReqWoStoreData, new_req: MissReqWoStoreData): MatchSignals = {
    val signals = Wire(new MatchSignals)
    signals.block_match := blockMatch(req, new_req)
    signals.alias_match := aliasMatch(req, new_req)
    signals.set_match   := setMatch(req, new_req)
    signals.merge_load  := (req.isFromLoad || req.isFromStore || req.isFromPrefetch) && new_req.isFromLoad
    signals.merge_store := (req.isFromLoad || req.isFromPrefetch) && new_req.isFromStore
    signals
  }
}

/**
  * miss queue enq logic: enq is now splited into 2 cycles
  *  +---------------------------------------------------------------------+    pipeline reg  +-------------------------+
  *  +         s0: judge mshr alloc or merge                               +     +-------+    + s1: real alloc or merge +
  *  +                                       primary_fire?       ->        +     | alloc |    +                         +
  *  + mainpipe  -> req0 -> queryME(0)       secondary_fire?     ->        +     | merge |    +                         +
  *  + loadpipe0 -> req1 -> queryME(1)   ->  compress?           ->        +  -> |       | -> +                         +
  *  + loadpipe1 -> req2 -> queryME(2)       mshr id             ->        +     | mshrid|    +                         +
  *  + loadpipe2 -> req3 -> queryME(3)       miss_req            ->        +     | req   |    +                         +
  *  +                                                                     +     +-------+    +                         +
  *  +---------------------------------------------------------------------+                  +-------------------------+
  */

// Parallel pipeline register array for multiple enqueue ports (using reqNum)
class MissReqPipeRegArray(edge: TLEdgeOut, numPorts: Int)(implicit p: Parameters) extends Bundle {
  val regs = Vec(numPorts, new MissReqPipeRegBundle(edge))
  val valid = Vec(numPorts, Bool())

  def has_valid(): Bool = valid.asUInt.orR
  def valid_count(): UInt = PopCount(valid)
}

// Analysis result for each request in cycle 0
class ReqAnalysisResult(nReq: Int, nMissEntries: Int)(implicit p: Parameters) extends Bundle {
  // Strategy for each request: each bit represents a strategy
  // bit 0: allocate, bit 1: merge, bit 2: compress
  // Multiple bits can be set to indicate multiple applicable strategies
  val strategy = Vec(nReq, UInt(3.W))

  // Target MSHR index (for allocate and merge)
  val target_mshr = Vec(nReq, UInt(log2Up(nMissEntries).W))

  // Compression group ID (for compress strategy)
  val compress_group = Vec(nReq, UInt(log2Up(nReq).W))

  // Valid flag for each request
  val valid = Vec(nReq, Bool())
}

// a pipeline reg between MissReq and MissEntry
class MissReqPipeRegBundle(edge: TLEdgeOut)(implicit p: Parameters) extends DCacheBundle
 with HasCircularQueuePtrHelper
 with HasMissReqFunction
 {
  val req           = new MissReq
  // this request is about to merge to an existing mshr
  val merge         = Bool()
  // this request is about to allocate a new mshr
  val alloc         = Bool()
  val cancel        = Bool()
  val mshr_id       = UInt(log2Up(cfg.nMissEntries).W)

  def reg_valid(): Bool = {
    (merge || alloc)
  }

  def matched(signals: MatchSignals): Bool = {
    signals.block_match && reg_valid()
  }

  def prefetch_late_en(signals: MatchSignals, new_req: MissReqWoStoreData, new_req_valid: Bool): Bool = {
    new_req_valid && alloc && signals.block_match && req.isFromPrefetch && !new_req.isFromPrefetch
  }

  def reject_req(signals: MatchSignals): Bool = {
    Mux(
        alloc,
        signals.block_match && (!signals.alias_match || !(signals.merge_load || signals.merge_store)),
        false.B
      )
  }

  def merge_req(signals: MatchSignals): Bool = {
    Mux(
        alloc,
        signals.block_match && signals.alias_match && (signals.merge_load || signals.merge_store),
        false.B
      )
  }

  def merge_isKeyword(signals: MatchSignals, new_req: MissReq): Bool = {
    val load_merge_load  = merge_req(signals) && req.isFromLoad  && new_req.isFromLoad
    val store_merge_load = merge_req(signals) && req.isFromStore && new_req.isFromLoad
    val load_merge_load_use_new_req_isKeyword = isAfter(req.lqIdx, new_req.lqIdx)
    val use_new_req_isKeyword = (load_merge_load && load_merge_load_use_new_req_isKeyword) || store_merge_load
    Mux (
      use_new_req_isKeyword,
        new_req.vaddr(5).asBool,
        req.vaddr(5).asBool
      )
  }

  def isKeyword(): Bool= {
    alloc && req.isFromLoad && req.vaddr(5).asBool
  }
  // send out acquire as soon as possible
  // if a new store miss req is about to merge into this pipe reg, don't send acquire now
  def can_send_acquire(signals: MatchSignals, valid: Bool, new_req: MissReq): Bool = {
    alloc && !(valid && merge_req(signals) && new_req.isFromStore)
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
    acquire.user.lift(AliasKey).foreach(_ := req.vaddr(13, 12))
    // pass vaddr to l2
    acquire.user.lift(VaddrKey).foreach(_ := req.vaddr(VAddrBits - 1, blockOffBits))
    // pass pc to l2
    acquire.user.lift(PCKey).foreach(_ := req.pc) 

    // miss req pipe reg pass keyword to L2, is priority
    // acquire.echo.lift(IsKeywordKey).foreach(_ := isKeyword())
    acquire.echo.lift(IsKeywordKey).foreach(_ := false.B)

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

  def block_and_alias_match(releaseReq: MissQueueBlockReqBundle): Bool = {
    reg_valid() && get_block(req.addr) === get_block(releaseReq.addr) && is_alias_match(req.vaddr, releaseReq.vaddr)
  }

  def evict_set_match(evict_set: UInt): Bool = {
    reg_valid() && req.isBtoT && addr_to_dcache_set(req.vaddr) === evict_set
  }
}

class CMOUnit(edge: TLEdgeOut)(implicit p: Parameters) extends DCacheModule {
  val io = IO(new Bundle() {
    val req = Flipped(DecoupledIO(new CMOReq))
    val req_chanA = DecoupledIO(new TLBundleA(edge.bundle))
    val resp_chanD = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))
    val resp_to_lsq = DecoupledIO(new CMOResp)
    val wfi = Flipped(new WfiReqBundle)
    val channel_sel = Output(UInt(memChannelBits.W))  // Channel selection for dual-channel support
  })

  val s_idle :: s_sreq :: s_wresp :: s_lsq_resp :: Nil = Enum(4)
  val state = RegInit(s_idle)
  val state_next = WireInit(state)
  val req = RegEnable(io.req.bits, io.req.fire)
  val nderr = RegInit(false.B)
  val denied = RegInit(false.B)
  val corrupt = RegInit(false.B)
  val no_pending = RegInit(true.B)

  state := state_next

  switch (state) {
    is(s_idle) {
      when (io.req.fire) {
        state_next := s_sreq
        nderr := false.B
        denied := false.B
        corrupt := false.B
      }
    }
    is(s_sreq) {
      when (io.req_chanA.fire) {
        state_next := s_wresp
        no_pending := false.B
      }
    }
    is(s_wresp) {
      when (io.resp_chanD.fire) {
        state_next := s_lsq_resp
        nderr := io.resp_chanD.bits.denied || io.resp_chanD.bits.corrupt
        denied := io.resp_chanD.bits.denied
        corrupt := io.resp_chanD.bits.corrupt
        no_pending := true.B
      }
    }
    is(s_lsq_resp) {
      when (io.resp_to_lsq.fire) {
        state_next := s_idle
      }
    }
  }

  // Channel selection based on request address (same as MissEntry)
  private def selectChannel(addr: UInt): UInt = {
    if (hasDualChannel) {
      if (channelSelByAddr) get_block(addr)(memChannelBits - 1, 0)
      else 0.U(memChannelBits.W)
    } else {
      0.U(memChannelBits.W)
    }
  }
  io.channel_sel := selectChannel(req.address)

  io.req.ready := state === s_idle

  io.req_chanA.valid := state === s_sreq && !io.wfi.wfiReq
  io.req_chanA.bits := edge.CacheBlockOperation(
    fromSource = (cfg.nMissEntries + 1).U,
    toAddress = req.address,
    lgSize = (log2Up(cfg.blockBytes)).U,
    opcode = req.opcode
  )._2

  io.resp_chanD.ready := state === s_wresp
  io.wfi.wfiSafe := GatedValidRegNext(no_pending && io.wfi.wfiReq)

  io.resp_to_lsq.valid := state === s_lsq_resp
  io.resp_to_lsq.bits.address := req.address
  io.resp_to_lsq.bits.nderr   := nderr
  io.resp_to_lsq.bits.denied  := denied
  io.resp_to_lsq.bits.corrupt := corrupt

  assert(!(state =/= s_idle && io.req.valid))
  assert(!(state =/= s_wresp && io.resp_chanD.valid))
}

class MissEntry(edge: TLEdgeOut, reqNum: Int)(implicit p: Parameters) extends DCacheModule
  with HasCircularQueuePtrHelper
  with HasMissReqFunction
 {
  val io = IO(new Bundle() {
    val hartId = Input(UInt(hartIdLen.W))
    // MSHR ID
    val id = Input(UInt(log2Up(cfg.nMissEntries).W))
    // need to reject when the same block in wbq
    val wbq_block_miss_req = Input(Vec(reqNum, Bool()))
    // pipeline reg
    val miss_req_pipe_reg = Input(new MissReqPipeRegBundle(edge))
    // allocate this entry for new req
    val entry_valid = Input(Bool())
    // this entry is free and can be allocated to new reqs
    val primary_ready = Output(Bool())
    // this entry is busy, but it can merge the new req
    // Changed to Vec to support parallel enqueue: each queryMQ request gets independent judgment
    val secondary_ready = Output(Vec(reqNum, Bool()))
    // this entry is busy and it can not merge the new req
    // Changed to Vec to support parallel enqueue: each queryMQ request gets independent judgment
    val secondary_reject = Output(Vec(reqNum, Bool()))
    // way selected for replacing, used to support plru update
    // bus
    val mem_acquire = DecoupledIO(new TLBundleA(edge.bundle))
    val mem_grant = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))
    val mem_finish = DecoupledIO(new TLBundleE(edge.bundle))

    // ========== Dual-channel support ==========
    // Channel selection output
    val channel_sel = Output(UInt(memChannelBits.W))

    val queryME = Vec(reqNum, Flipped(new DCacheMEQueryIOBundle))

    // output the signals to avoid redundant computation
    // val match_signals = Output(new MatchSignals)
    val match_signals_vec = Vec(reqNum, Output(new MatchSignals))

    // send refill info to load queue, useless now
    val refill_to_ldq = ValidIO(new Refill)

    // replace pipe
    val l2_hint = Input(Valid(new L2ToL1Hint())) // Hint from L2 Cache

    // main pipe: amo miss
    val main_pipe_req = DecoupledIO(new MainPipeReq)
    val main_pipe_resp = Input(Bool())
    val main_pipe_refill_resp = Input(Bool())
    val main_pipe_replay = Input(Bool())
    val main_pipe_evict_BtoT_way = Input(Bool())
    val main_pipe_next_evict_way = Input(UInt(nWays.W))

    // for main pipe s2
    val refill_info = ValidIO(new MissQueueRefillInfo)
    val refill_train = ValidIO(new TrainReqBundle)

    val occupy_way = Output(UInt(nWays.W))

    // block probe
    val probe = Flipped(new MissQueueBlockIO)

    // block replace when release an addr valid in mshr
    val replace = Flipped(new MissQueueBlockIO)

    val req_addr = ValidIO(UInt(PAddrBits.W))
    val req_vaddr = ValidIO(UInt(VAddrBits.W))
    val req_isBtoT = Output(Bool())

    val req_hasStore = Output(Bool())

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
      val hit_prefetch = Vec(reqNum, Output(Bool()))
      val hit_pf_source = UInt(L1PfSourceBits.W)
    }
    val nMaxPrefetchEntry = Input(UInt(64.W))
    val matched = Output(Bool())
    val l1Miss = Output(Bool())

    val wfi = Flipped(new WfiReqBundle)
  })

  val req = Reg(new MissReqWoStoreData)
  val req_primary_fire = Reg(new MissReqWoStoreData) // for perf use
  val req_store_mask = Reg(UInt(cfg.blockBytes.W))
  val req_valid = RegInit(false.B)
  val set = addr_to_dcache_set(req.vaddr)
  val evict_BtoT_way = RegInit(false.B)
  val alloc_is_store = RegInit(false.B)  // The alloc (first) req is a store req
  val hasStore = RegInit(false.B)
  // initial keyword
  val isKeyword = RegInit(false.B)

  // Route each MSHR to a fixed L1-L2 channel so A/D/E traffic stays on one path.
  private def selectChannel(addr: UInt, mshrId: UInt): UInt = {
    if (hasDualChannel) {
      if (channelSelByAddr) get_block(addr)(memChannelBits - 1, 0)
      else mshrId(memChannelBits - 1, 0)
    } else {
      0.U(memChannelBits.W)
    }
  }
  val channel_sel = selectChannel(req.addr, io.id)

  // Output channel selection
  io.channel_sel := channel_sel

  val miss_req_pipe_reg_bits = io.miss_req_pipe_reg.req

  val signals_vec = WireInit(VecInit(Seq.fill(reqNum)(0.U.asTypeOf(new MatchSignals))))
  val signals_pipe_prefetch = computeMatchSignals(miss_req_pipe_reg_bits, io.queryME(0).req.bits)

  for(i <- 0 until reqNum) {
    signals_vec(i) := computeMatchSignals(req, io.queryME(i).req.bits)
  }

  val input_req_is_prefetch = isPrefetch(miss_req_pipe_reg_bits.cmd)

  val s_acquire = RegInit(true.B)
  val s_grantack = RegInit(true.B)
  val s_mainpipe_req = RegInit(true.B)

  val w_grantfirst = RegInit(true.B)
  val w_grantlast = RegInit(true.B)
  val w_mainpipe_resp = RegInit(true.B)
  val w_refill_resp = RegInit(true.B)
  val w_l2hint = RegInit(true.B)

  val no_pending = RegInit(true.B)

  val mainpipe_req_fired = RegInit(true.B)

  val release_entry = s_grantack && w_mainpipe_resp && w_refill_resp

  val acquire_not_sent = !s_acquire && !io.mem_acquire.ready
  val data_not_refilled = !w_grantfirst

  val error = RegInit(false.B)
  val denied = RegInit(false.B)
  val corrupt = RegInit(false.B)
  val prefetch = RegInit(false.B)
  val access = RegInit(false.B)

  val should_refill_data_reg =  Reg(Bool())
  val should_refill_data = WireInit(should_refill_data_reg)

  val should_replace = RegInit(false.B)

  val full_overwrite = Reg(Bool())

  val (_, _, refill_done, refill_count) = edge.count(io.mem_grant)
  val grant_param = Reg(UInt(TLPermissions.bdWidth.W))

  // refill data with store data, this reg will be used to store:
  // 1. store data (if needed), before l2 refill data
  // 2. store data and l2 refill data merged result (i.e. new cacheline taht will be write to data array)
  val refill_and_store_data = Reg(Vec(blockRows, UInt(rowBits.W)))
  // raw data refilled to l1 by l2
  val refill_data_raw = Reg(Vec(blockBytes/beatBytes, UInt(beatBits.W)))

  val refill_start_time = Reg(UInt(64.W))
  val refill_latency = Reg(UInt(LATENCY_WIDTH.W))

  // allocate current miss queue entry for a miss req
  // Use queryME instead of io.req for parallel enqueue
  val primary_fire_vec = (0 until reqNum).map{ i =>
    io.entry_valid && io.queryME(i).req.valid && io.primary_ready && !io.queryME(i).req.bits.cancel && !io.wbq_block_miss_req(i)
  }
  val primary_fire = ParallelORR(Cat(primary_fire_vec))

  val primary_accept_vec = (0 until reqNum).map{ i =>
    io.queryME(i).req.valid && io.primary_ready && !io.queryME(i).req.bits.cancel
  }
  val primary_accept = ParallelORR(Cat(primary_accept_vec))

  // merge miss req to current miss queue entry
  // Check if ANY request port can merge
  val secondary_ready_any = ParallelORR(Cat(io.secondary_ready))
  val secondary_reject_any = ParallelORR(Cat(io.secondary_reject))

  // For backward compatibility with io.req (single-port path)
  // Note: io.req.path is deprecated, use queryME instead
  val secondary_fire_vec = (0 until reqNum).map{ i =>
    io.queryME(i).req.valid && io.secondary_ready(i) && !io.queryME(i).req.bits.cancel && !io.wbq_block_miss_req(i)
  }
  val secondary_fire = ParallelORR(Cat(secondary_fire_vec))

  val secondary_accept_vec = (0 until reqNum).map{ i =>
    io.queryME(i).req.valid && io.secondary_ready(i) && !io.queryME(i).req.bits.cancel
  }
  val secondary_accept = ParallelORR(Cat(secondary_accept_vec))

  val req_handled_by_this_entry = primary_accept || secondary_accept

  for(i <- 0 until reqNum) {
    io.match_signals_vec(i).block_match := signals_vec(i).block_match && req_valid
    io.match_signals_vec(i).alias_match := signals_vec(i).alias_match && req_valid
    io.match_signals_vec(i).set_match := signals_vec(i).set_match && req_valid
    io.match_signals_vec(i).merge_load := signals_vec(i).merge_load && req_valid
    io.match_signals_vec(i).merge_store := signals_vec(i).merge_store && req_valid
  }

  // for perf use
  val secondary_fired = RegInit(false.B)

  io.perf_pending_prefetch := req_valid && prefetch && !secondary_fired
  io.perf_pending_normal   := req_valid && (!prefetch || secondary_fired)

  io.rob_head_query.resp   := io.rob_head_query.hit(req.vaddr) && req_valid

  io.req_handled_by_this_entry := req_handled_by_this_entry

  when (release_entry && req_valid) {
    req_valid := false.B
  }

  when (io.miss_req_pipe_reg.alloc && !io.miss_req_pipe_reg.cancel) {
    assert(RegNext(primary_fire), p"after 1 cycle of primary_fire, entry will be allocated:${io.id}")
    req_valid := true.B

    req := miss_req_pipe_reg_bits.toMissReqWoStoreData()
    req.isBtoT := miss_req_pipe_reg_bits.isBtoT
    req.occupy_way := miss_req_pipe_reg_bits.occupy_way
    req.addr := get_block_addr(miss_req_pipe_reg_bits.addr)
    req_primary_fire := miss_req_pipe_reg_bits.toMissReqWoStoreData()
    evict_BtoT_way := false.B
    alloc_is_store := miss_req_pipe_reg_bits.isFromStore
    hasStore := miss_req_pipe_reg_bits.isFromStore
    // remove isKeyword logic
    isKeyword := false.B

    s_acquire := io.acquire_fired_by_pipe_reg
    s_grantack := false.B
    s_mainpipe_req := false.B

    w_grantfirst := false.B
    w_grantlast := false.B
    w_l2hint := false.B
    mainpipe_req_fired := false.B

    no_pending := !io.acquire_fired_by_pipe_reg

    req_store_mask := Mux(miss_req_pipe_reg_bits.isFromStore, miss_req_pipe_reg_bits.store_mask, 0.U)

    full_overwrite := miss_req_pipe_reg_bits.isFromStore && miss_req_pipe_reg_bits.full_overwrite

    when (!miss_req_pipe_reg_bits.isFromAMO) {
      w_refill_resp := false.B
    }

    when (miss_req_pipe_reg_bits.isFromAMO) {
      w_mainpipe_resp := false.B
    }

    should_refill_data_reg := miss_req_pipe_reg_bits.isFromLoad

    error := false.B
    denied := false.B
    corrupt := false.B
    prefetch := input_req_is_prefetch && !io.miss_req_pipe_reg.prefetch_late_en(signals_pipe_prefetch, io.queryME(0).req.bits, io.queryME(0).req.valid)
    access := false.B
    secondary_fired := false.B

    refill_start_time := GTimer()
  }

  // Wire to hold updated data that has merged grant data (on grant.fire)
  val refill_and_store_data_update = Wire(Vec(blockRows, UInt(rowBits.W)))
  // All the logic of refill_and_store_data. There is a corner case to note: store-merge and grant.fire may happen at the same cycle.
  when (io.miss_req_pipe_reg.alloc && !io.miss_req_pipe_reg.cancel && miss_req_pipe_reg_bits.isFromStore) {
    refill_and_store_data := VecInit(miss_req_pipe_reg_bits.store_data.grouped(rowBits))
  }.elsewhen (io.miss_req_pipe_reg.merge && !io.miss_req_pipe_reg.cancel && miss_req_pipe_reg_bits.isFromStore) {
    for (i <- 0 until blockRows) {
      val store_mask_temp = miss_req_pipe_reg_bits.store_mask.grouped(rowBytes)(i).asBools
      val store_data_temp = (miss_req_pipe_reg_bits.store_data.grouped(8).grouped(rowBytes).toSeq)(i)
      refill_and_store_data(i) := VecInit((0 until rowBytes).map(k =>
        Mux(store_mask_temp(k), store_data_temp(k), refill_and_store_data_update(i).grouped(8)(k)))).asUInt
    }
  }.elsewhen (io.mem_grant.fire) {
    refill_and_store_data := refill_and_store_data_update
  }

  when (io.miss_req_pipe_reg.merge && !io.miss_req_pipe_reg.cancel) {
    assert(RegNext(secondary_fire) || RegNext(RegNext(primary_fire)), p"after 1 cycle of secondary_fire or 2 cycle of primary_fire, entry will be merged:${io.id}")
    assert(miss_req_pipe_reg_bits.req_coh.state <= req.req_coh.state || (prefetch && !access))
    assert(!(miss_req_pipe_reg_bits.isFromAMO || req.isFromAMO))
    // use the most uptodate meta
    req.req_coh := miss_req_pipe_reg_bits.req_coh

    isKeyword := false.B
    assert(!miss_req_pipe_reg_bits.isFromPrefetch, "can not merge a prefetch req, late prefetch should always be ignored!")

    when (miss_req_pipe_reg_bits.isFromStore) {
      req := miss_req_pipe_reg_bits
      req.isBtoT := miss_req_pipe_reg_bits.isBtoT
      req.occupy_way := miss_req_pipe_reg_bits.occupy_way
      evict_BtoT_way := false.B
      req.addr := get_block_addr(miss_req_pipe_reg_bits.addr)
      req_store_mask := req_store_mask | miss_req_pipe_reg_bits.store_mask
      hasStore := true.B
      full_overwrite := full_overwrite || miss_req_pipe_reg_bits.full_overwrite
      assert(is_alias_match(req.vaddr, miss_req_pipe_reg_bits.vaddr), "alias bits should be the same when merging store")
    }

    should_refill_data := should_refill_data_reg || miss_req_pipe_reg_bits.isFromLoad
    should_refill_data_reg := should_refill_data
    when (!input_req_is_prefetch) {
      access := true.B // when merge non-prefetch req, set access bit
    }
    secondary_fired := true.B
  }

  when (io.mem_acquire.fire) {
    s_acquire := true.B
    no_pending := false.B
  }

  // merge refilled data and store data (if needed)
  def mergePutData(old_data: UInt, new_data: UInt, wmask: UInt): UInt = {
    val full_wmask = FillInterleaved(8, wmask)
    (~full_wmask & old_data | full_wmask & new_data)
  }
  val new_mask = VecInit(req_store_mask.grouped(rowBytes))
  val grant_data_grouped = io.mem_grant.bits.data.grouped(rowBits)
  val lowHalf_refill = refill_count === 0.U && !isKeyword || refill_count =/= 0.U && isKeyword
  //---- refill_and_store_data_update: see definition above ----
  require(blockRows == 2 * beatRows, "refill_and_store_data_update: so far, only works for blockRows == 2 * beatRows")
  for (i <- 0 until beatRows) {
    refill_and_store_data_update(i) := Mux(
      io.mem_grant.fire && edge.hasData(io.mem_grant.bits) && lowHalf_refill,
      mergePutData(grant_data_grouped(i), refill_and_store_data(i), new_mask(i)),
      refill_and_store_data(i)
    )
    refill_and_store_data_update(i + beatRows) := Mux(
      io.mem_grant.fire && edge.hasData(io.mem_grant.bits) && !lowHalf_refill,
      mergePutData(grant_data_grouped(i), refill_and_store_data(i + beatRows), new_mask(i + beatRows)),
      refill_and_store_data(i + beatRows)
    )
  }

  val hasData = RegInit(true.B)
  val isDirty = RegInit(false.B)
  io.wfi.wfiSafe := GatedValidRegNext(no_pending && io.wfi.wfiReq)

  when (io.mem_grant.fire) {
    w_grantfirst := true.B
    grant_param := io.mem_grant.bits.param
    when (edge.hasData(io.mem_grant.bits)) {
      w_grantlast := w_grantlast || refill_done
      no_pending := no_pending || refill_done
      hasData := true.B
    }.otherwise {
      // Grant
      assert(full_overwrite)
      w_grantlast := true.B
      no_pending := true.B
      hasData := false.B
    }

    error := io.mem_grant.bits.denied || io.mem_grant.bits.corrupt || error
    denied := denied || io.mem_grant.bits.denied
    corrupt := corrupt || io.mem_grant.bits.corrupt

    refill_data_raw(refill_count ^ isKeyword) := io.mem_grant.bits.data
    isDirty := io.mem_grant.bits.echo.lift(DirtyKey).getOrElse(false.B)
    when(refill_done) {
      val refill_end_time = GTimer()
      val time_delta = refill_end_time - refill_start_time
      val overflow = refill_end_time < refill_start_time || (time_delta >> LATENCY_WIDTH).orR
      refill_latency := Mux(overflow, 0.U, time_delta)
    }
  }

  when (io.mem_finish.fire) {
    s_grantack := true.B
  }

  when (io.main_pipe_req.fire) {
    s_mainpipe_req := true.B
    mainpipe_req_fired := true.B
  }

  when (io.main_pipe_replay || io.main_pipe_evict_BtoT_way) {
    s_mainpipe_req := false.B
  }
  when (io.main_pipe_replay) {
    evict_BtoT_way := false.B
  } .elsewhen (io.main_pipe_evict_BtoT_way) {
    evict_BtoT_way := true.B
    req.occupy_way := io.main_pipe_next_evict_way
  }
  XSError(req_valid && req.isBtoT && io.main_pipe_evict_BtoT_way, "BtoT request will never evict a way")

  when (io.main_pipe_resp) {
    w_mainpipe_resp := true.B
  }

  when(io.main_pipe_refill_resp) {
    w_refill_resp := true.B
  }

  when (io.l2_hint.valid) {
    w_l2hint := true.B
  }

  def before_req_sent_can_merge(new_req: MissReqWoStoreData): Bool = {
    // acquire_not_sent && (new_req.isFromLoad || new_req.isFromStore)

    // Since most acquire requests have been issued from pipe_reg,
    // the number of such merge situations is currently small,
    // So dont Merge anything for better timing.
    false.B
  }

  def before_data_refill_can_merge(new_req: MissReqWoStoreData): Bool = {
    data_not_refilled && new_req.isFromLoad ||
    !io.main_pipe_refill_resp && !w_refill_resp && new_req.isFromStore && alloc_is_store
  }

  // Note that late prefetch will be ignored

  def should_merge(signals: MatchSignals, new_req: MissReqWoStoreData): Bool = {
    signals.block_match && signals.alias_match &&
    (
      before_req_sent_can_merge(new_req) ||
      before_data_refill_can_merge(new_req)
    )
  }

  def before_req_sent_merge_iskeyword(signals: MatchSignals, new_req: MissReqWoStoreData): Bool = {
    val need_check_isKeyword = acquire_not_sent && req.isFromLoad && new_req.isFromLoad && should_merge(signals, new_req)
    val use_new_req_isKeyword = isAfter(req.lqIdx, new_req.lqIdx)
    Mux(
      need_check_isKeyword,
      Mux(
        use_new_req_isKeyword,
        new_req.vaddr(5).asBool,
        req.vaddr(5).asBool
      ),
      isKeyword
      )
  }

  // load can be merged before io.mem_grant.fire
  //
  // TODO: merge store if possible? mem_acquire may need to be re-issued,
  // but sbuffer entry can be freed
  def should_reject(signals: MatchSignals, new_req: MissReqWoStoreData): Bool = {
    req_valid && Mux(
        signals.block_match,
        (!before_req_sent_can_merge(new_req) && !before_data_refill_can_merge(new_req)) || !signals.alias_match,
        false.B
      )
  }

  // req_valid will be updated 1 cycle after primary_fire, so next cycle, this entry cannot accept a new req
  when(GatedValidRegNext(io.id >= ((cfg.nMissEntries).U - io.nMaxPrefetchEntry))) {
    // can accept prefetch req
    io.primary_ready := !req_valid && !GatedValidRegNext(primary_fire)
  }.otherwise {
    // cannot accept prefetch req except when a memset patten is detected
    io.primary_ready := !req_valid && (!io.queryME.map(_.req.bits.isFromPrefetch).reduce(_&&_) || io.memSetPattenDetected) && !GatedValidRegNext(primary_fire)
  }

  // Generate vectorized secondary_ready and secondary_reject for parallel enqueue
  // Each queryMQ request gets independent judgment
  for (i <- 0 until reqNum) {
    val _signals = computeMatchSignals(req, io.queryME(i).req.bits)
    io.secondary_ready(i) := should_merge(_signals, io.queryME(i).req.bits) && !io.miss_req_pipe_reg.cancel
    io.secondary_reject(i) := should_reject(_signals, io.queryME(i).req.bits)
  }

  // For backward compatibility with io.req (single-port path)
  // Use queryME(0) to maintain compatibility
  // Note: io.secondary_ready(0) is already set by the loop above

  // generate primary_ready & secondary_(ready | reject) for each miss request
  for (i <- 0 until reqNum) {
    when(GatedValidRegNext(io.id >= ((cfg.nMissEntries).U - io.nMaxPrefetchEntry))) {
      io.queryME(i).primary_ready := !req_valid && !GatedValidRegNext(primary_fire)
    }.otherwise {
      io.queryME(i).primary_ready := !req_valid && !GatedValidRegNext(primary_fire) &&
                                    (!io.queryME(i).req.bits.isFromPrefetch || io.memSetPattenDetected)
    }
    val _signals = computeMatchSignals(req, io.queryME(i).req.bits)
    io.queryME(i).secondary_ready  := should_merge(_signals, io.queryME(i).req.bits)
    io.queryME(i).secondary_reject := should_reject(_signals, io.queryME(i).req.bits)
    io.queryME(i).block_match := _signals.block_match && req_valid
  }

  // should not allocate, merge or reject at the same time
for(i <- 0 until reqNum) {
    assert(RegNext(PopCount(Seq(io.primary_ready, io.queryME(i).secondary_ready, io.queryME(i).secondary_reject)) <= 1.U || !io.queryME(i).req.valid))
  }

  val refill_data_splited = WireInit(VecInit(Seq.tabulate(cfg.blockBytes * 8 / l1BusDataWidth)(i => {
    val data = refill_and_store_data.asUInt
    data((i + 1) * l1BusDataWidth - 1, i * l1BusDataWidth)
  })))
  // when granted data is all ready, wakeup lq's miss load
  val refill_to_ldq_en = !w_grantlast && io.mem_grant.fire
  io.refill_to_ldq.valid := GatedValidRegNext(refill_to_ldq_en)
  io.refill_to_ldq.bits.addr := RegEnable(req.addr + ((refill_count ^ isKeyword) << refillOffBits), refill_to_ldq_en)
  io.refill_to_ldq.bits.data := refill_data_splited(RegEnable(refill_count ^ isKeyword, refill_to_ldq_en))
  io.refill_to_ldq.bits.error := RegEnable(io.mem_grant.bits.corrupt || io.mem_grant.bits.denied, refill_to_ldq_en)
  io.refill_to_ldq.bits.refill_done := RegEnable(refill_done && io.mem_grant.fire, refill_to_ldq_en)
  io.refill_to_ldq.bits.hasdata := hasData
  io.refill_to_ldq.bits.data_raw := refill_data_raw.asUInt
  io.refill_to_ldq.bits.id := io.id

  // if the entry has a pending merge req, wait for it
  // Note: now, only wait for store, because store may acquire T
  io.mem_acquire.valid := !s_acquire &&
    !(io.miss_req_pipe_reg.merge && !io.miss_req_pipe_reg.cancel && miss_req_pipe_reg_bits.isFromStore) &&
    !io.wfi.wfiReq
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
  // pass vaddr to l2
  io.mem_acquire.bits.user.lift(VaddrKey).foreach( _ := req.vaddr(VAddrBits-1, blockOffBits))
  // pass pc to l2
  io.mem_acquire.bits.user.lift(PCKey).foreach(_ := req.pc)
  // pass keyword to L2
  // io.mem_acquire.bits.echo.lift(IsKeywordKey).foreach(_ := isKeyword)
  io.mem_acquire.bits.echo.lift(IsKeywordKey).foreach(_ := false.B)
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
  io.mem_acquire.bits.user.lift(MemBackTypeMM).foreach(_ := true.B)
  io.mem_acquire.bits.user.lift(MemPageTypeNC).foreach(_ := false.B)
  require(nSets <= 256)

  // io.mem_grant.ready := !w_grantlast && s_acquire
  io.mem_grant.ready := true.B
  assert(!(io.mem_grant.valid && !(!w_grantlast && s_acquire)), p"dcache should always be ready for mem_grant now:${io.id}")

  val grantack = RegEnable(edge.GrantAck(io.mem_grant.bits), io.mem_grant.fire)
  assert(RegNext(!io.mem_grant.fire || edge.isRequest(io.mem_grant.bits)))
  io.mem_finish.valid := !s_grantack && w_grantfirst
  io.mem_finish.bits := grantack

  // Send mainpipe_req when receive hint from L2 or receive data without hint
  io.main_pipe_req.valid := !s_mainpipe_req && (w_l2hint || w_grantlast)
  io.main_pipe_req.bits := DontCare
  io.main_pipe_req.bits.miss := true.B
  io.main_pipe_req.bits.miss_id := io.id
  io.main_pipe_req.bits.probe := false.B
  io.main_pipe_req.bits.source := req.source
  io.main_pipe_req.bits.cmd := req.cmd
  io.main_pipe_req.bits.vaddr := req.vaddr
  io.main_pipe_req.bits.addr := req.addr
  io.main_pipe_req.bits.word_idx := req.word_idx
  io.main_pipe_req.bits.amo_data := req.amo_data
  io.main_pipe_req.bits.amo_mask := req.amo_mask
  io.main_pipe_req.bits.amo_cmp  := req.amo_cmp
  io.main_pipe_req.bits.id := req.id
  io.main_pipe_req.bits.pf_source := req.pf_source
  io.main_pipe_req.bits.access := access
  io.main_pipe_req.bits.occupy_way := req.occupy_way
  io.main_pipe_req.bits.miss_fail_cause_evict_btot := evict_BtoT_way

  io.probe.block := req_valid && w_grantlast &&
    get_block_addr(req.addr) === get_block_addr(io.probe.req.bits.addr) &&
    is_alias_match(req.vaddr, io.probe.req.bits.vaddr)

  io.replace.block := req_valid &&
    get_block_addr(req.addr) === get_block_addr(io.replace.req.bits.addr) &&
    is_alias_match(req.vaddr, io.replace.req.bits.vaddr)

  io.req_addr.valid := req_valid
  io.req_addr.bits:= req.addr
  io.req_vaddr.valid := req_valid
  io.req_vaddr.bits := req.vaddr
  io.req_isBtoT := req.isBtoT

  io.req_hasStore := hasStore && req_valid

  io.occupy_way := req.occupy_way

  io.refill_info.valid := req_valid && w_grantlast
  io.refill_info.bits.store_data := refill_and_store_data.asUInt
  io.refill_info.bits.store_mask := ~0.U(blockBytes.W)
  io.refill_info.bits.miss_param := grant_param
  io.refill_info.bits.miss_dirty := isDirty
  io.refill_info.bits.error.tl_denied  := denied
  io.refill_info.bits.error.tl_corrupt := corrupt
  io.refill_info.bits.refill_latency := Mux(
    isFromL1Prefetch(req.pf_source),
    refill_latency,
    0.U
  )

  io.refill_train.valid := req_valid && w_grantlast
  io.refill_train.bits.pc := req.pc
  io.refill_train.bits.paddr := req.addr
  io.refill_train.bits.vaddr := req.vaddr
  io.refill_train.bits.miss := true.B
  // FIXME lyq: when mshr entry merges, req.pf_source may be cleaned.
  io.refill_train.bits.metaSource := req.pf_source
  io.refill_train.bits.refillLatency := refill_latency
  io.refill_train.bits.robIdx := DontCare
  io.refill_train.bits.isFirstIssue := DontCare
  io.refill_train.bits.isHwPrefetch := DontCare

  XSPerfAccumulate("miss_refill_mainpipe_req", io.main_pipe_req.fire)
  XSPerfAccumulate("miss_refill_without_hint", io.main_pipe_req.fire && !mainpipe_req_fired && !w_l2hint)
  XSPerfAccumulate("miss_refill_replay", io.main_pipe_replay)
  XSPerfAccumulate("miss_refill_evict_BtoT_way", io.main_pipe_evict_BtoT_way)

  val w_grantfirst_forward_info = Mux(isKeyword, w_grantlast, w_grantfirst)
  val w_grantlast_forward_info = Mux(isKeyword, w_grantfirst, w_grantlast)
  io.forwardInfo.inflight := req_valid
  io.forwardInfo.paddr := req.addr
  io.forwardInfo.raw_data := refill_and_store_data
  io.forwardInfo.isFromStore := req.isFromStore
  io.forwardInfo.store_mask := req_store_mask
  io.forwardInfo.firstbeat_valid := w_grantfirst_forward_info
  io.forwardInfo.lastbeat_valid := w_grantlast_forward_info
  io.forwardInfo.denied := denied
  io.forwardInfo.corrupt := corrupt

  // The prefetch_req only in mainPipe, now!
  // But the miss_req that hits prefetch_req is more than one!
  val hit_prefetch_vec = Wire(Vec(reqNum, Bool()))
  for(i <- 0 until reqNum) {
    hit_prefetch_vec(i) := io.queryME(i).req.valid && !io.queryME(i).req.bits.isFromPrefetch &&
                            req_valid && signals_vec(i).block_match && prefetch
  }
  io.matched := req_valid && signals_vec(0).block_match
  io.prefetch_info.hit_prefetch := hit_prefetch_vec
  io.prefetch_info.hit_pf_source := req.pf_source

  when(io.prefetch_info.hit_prefetch.asUInt.orR) {
    prefetch := false.B
    req.pf_source := L1_HW_PREFETCH_CLEAR
  }

  io.l1Miss := req_valid
  // refill latency monitor
  val start_counting = GatedValidRegNext(io.mem_acquire.fire) || (GatedValidRegNextN(primary_fire, 2) && s_acquire)
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
  XSPerfAccumulate("penalty_between_grantlast_and_release",
    BoolStopWatch(!RegNext(w_grantlast) && w_grantlast, release_entry, true)
  )
  XSPerfAccumulate("main_pipe_penalty", BoolStopWatch(io.main_pipe_req.fire, io.main_pipe_resp))
  XSPerfAccumulate("penalty_blocked_by_channel_A", io.mem_acquire.valid && !io.mem_acquire.ready)
  XSPerfAccumulate("penalty_waiting_for_channel_D", s_acquire && !w_grantlast && !io.mem_grant.valid)
  XSPerfAccumulate("penalty_waiting_for_channel_E", io.mem_finish.valid && !io.mem_finish.ready)
  XSPerfAccumulate("prefetch_req_primary", Cat((0 until reqNum).map(i=> primary_fire_vec(i) && io.queryME(i).req.bits.source === DCACHE_PREFETCH_SOURCE.U)).orR)
  XSPerfAccumulate("prefetch_req_merged", Cat((0 until reqNum).map(i=> secondary_fire_vec(i) && io.queryME(i).req.bits.source === DCACHE_PREFETCH_SOURCE.U)).orR)
  XSPerfAccumulate("can_not_send_acquire_because_of_merging_store", !s_acquire && io.miss_req_pipe_reg.merge && io.miss_req_pipe_reg.cancel && miss_req_pipe_reg_bits.isFromStore)

  val (mshr_penalty_sample, mshr_penalty) = TransactionLatencyCounter(GatedValidRegNextN(primary_fire, 2) && !release_entry, release_entry)
  XSPerfHistogram("miss_penalty", mshr_penalty, mshr_penalty_sample, 0, 20, 1, true, true)
  XSPerfHistogram("miss_penalty", mshr_penalty, mshr_penalty_sample, 20, 100, 10, true, false)

  val load_miss_begin = ParallelMux(
    primary_fire_vec
     zip
    io.queryME.map(_.req.bits.isFromLoad)
  )
  val refill_finished = GatedValidRegNext(!w_grantlast && refill_done) && should_refill_data
  val (load_miss_penalty_sample, load_miss_penalty) = TransactionLatencyCounter(load_miss_begin, refill_finished) // not real refill finish time
  XSPerfHistogram("load_miss_penalty_to_use", load_miss_penalty, load_miss_penalty_sample, 0, 20, 1, true, true)
  XSPerfHistogram("load_miss_penalty_to_use", load_miss_penalty, load_miss_penalty_sample, 20, 100, 10, true, false)
  XSPerfHistogram("load_miss_penalty_to_use", load_miss_penalty, load_miss_penalty_sample, 100, 400, 30, true, false)

  val (a_to_d_penalty_sample, a_to_d_penalty) = TransactionLatencyCounter(start_counting, GatedValidRegNext(io.mem_grant.fire && refill_done))
  XSPerfHistogram("a_to_d_penalty", a_to_d_penalty, a_to_d_penalty_sample, 0, 20, 1, true, true)
  XSPerfHistogram("a_to_d_penalty", a_to_d_penalty, a_to_d_penalty_sample, 20, 100, 10, true, false)
}

class MissQueue(edge: TLEdgeOut, reqNum: Int)(implicit p: Parameters) extends DCacheModule
  with HasPerfEvents
  with HasMissReqFunction
  {
  val io = IO(new Bundle {
    val hartId = Input(UInt(hartIdLen.W))
    val resp = Vec(reqNum, Output(new MissResp))
    val refill_to_ldq = ValidIO(new Refill)

    // cmo req
    val cmo_req = Flipped(DecoupledIO(new CMOReq))
    val cmo_resp = DecoupledIO(new CMOResp)

    val queryMQ = Vec(reqNum, Flipped(new DCacheMQQueryIOBundle))

    val mem_acquire = DecoupledIO(new TLBundleA(edge.bundle))
    val mem_grant = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))
    val mem_finish = DecoupledIO(new TLBundleE(edge.bundle))

    // ========== Dual-channel support ==========
    // Second set of TL channels for dual-channel L1-L2 interface
    val mem_acquire_1 = if (hasDualChannel) Some(DecoupledIO(new TLBundleA(edge.bundle))) else None
    val mem_grant_1 = if (hasDualChannel) Some(Flipped(DecoupledIO(new TLBundleD(edge.bundle)))) else None
    val mem_finish_1 = if (hasDualChannel) Some(DecoupledIO(new TLBundleE(edge.bundle))) else None

    val l2_hint = Input(Vec(cfg.numMemChannels, Valid(new L2ToL1Hint()))) // Hint from L2 Cache

    val main_pipe_req = DecoupledIO(new MainPipeReq)
    val main_pipe_resp = Flipped(ValidIO(new MainPipeResp))

    val mainpipe_info = Input(new MainPipeInfoToMQ)
    val refill_info = ValidIO(new MissQueueRefillInfo)
    val refill_train = ValidIO(new TrainReqBundle)

    // block probe
    val probe = Flipped(new MissQueueBlockIO)

    // block replace when release an addr valid in mshr
    val replace = Flipped(new MissQueueBlockIO)

    // block all way for set to BtoT
    val evict_set = Input(UInt())
    val btot_ways_for_set = Output(UInt(nWays.W))

    // occupy set check
    val occupy_set = Input(Vec(LoadPipelineWidth, UInt()))
    val occupy_fail = Output(Vec(LoadPipelineWidth, Bool()))

    // req blocked by wbq
    val wbq_block_miss_req = Input(Vec(reqNum, Bool()))

    val full = Output(Bool())

    // forward missqueue
    val forward = Flipped(Vec(LoadPipelineWidth, new DCacheForward))
    val forwardS1PAddrMatch = Output(Vec(LoadPipelineWidth, Bool()))
    // If a store is miss and accepted by mshr, Sbuffer releases the entry and mshr provides corresponding st-ld forwarding data.
    // Note: the resp of this st-ld forwarding is merged into io.forward.S2Resp interface
    val forward_stData = Flipped(Vec(LoadPipelineWidth, new SbufferForwardReq))
    val l2_pf_store_only = Input(Bool())

    val memSetPattenDetected = Output(Bool())
    val lqEmpty = Input(Bool())

    // sbuffer-flush must flush all store entries in mshr as well
    val mshr_store_empty = Output(Bool())

    val prefetch_stat = Output(new MissPrefetchStatBundle)

    val wfi = Flipped(new WfiReqBundle)

    val debugTopDown = new DCacheTopDownIO
    val l1Miss = Output(Bool())
  })

  // 128KBL1: FIXME: provide vaddr for l2

  val entries = Seq.fill(cfg.nMissEntries)(Module(new MissEntry(edge, reqNum)))
  val cmo_unit = Module(new CMOUnit(edge))

  // Parallel pipeline registers for queryMQ path (reqNum ports)
  val parallel_pipe_regs = RegInit(VecInit(Seq.fill(reqNum)(0.U.asTypeOf(new MissReqPipeRegBundle(edge)))))
  val parallel_regs_channel = WireInit(VecInit(Seq.fill(reqNum)(0.U(memChannelBits.W))))

  val acquire_from_pipereg_vec = Wire(Vec(reqNum, chiselTypeOf(io.mem_acquire)))

  // val signals = computeMatchSignals(miss_req_pipe_reg.req, io.req.bits)
  val signals_vec = (0 until reqNum).map {i =>
    computeMatchSignals(parallel_pipe_regs(i).req, io.queryMQ(i).req.bits)
  }

  // Store misses may reside either in MSHR entries or in the miss_req_pipe_reg.
  // sbuffer-flush should wait until both places are clear.
  val mshr_has_store = Cat(entries.map(_.io.req_hasStore)  ++ parallel_pipe_regs.map(pipe_reg => pipe_reg.reg_valid() && pipe_reg.req.isFromStore)).orR
  io.mshr_store_empty := !mshr_has_store

  val primary_ready_vec = entries.map(_.io.primary_ready)

  // secondary_ready_vec(i)(e) = entry e can merge request i
  val secondary_ready_vec = (0 until reqNum).map { i =>
    entries.map(_.io.secondary_ready(i))
  }

  // secondary_reject_vec(i)(e) = entry e will reject request i
  val secondary_reject_vec = (0 until reqNum).map { i =>
    entries.map(_.io.secondary_reject(i))
  }

  // val block_match_vec = entries.map(_.io.match_signals.block_match)

  val block_match_seqs = (0 until reqNum).map { i =>
    entries.map(_.io.match_signals_vec(i).block_match)
  }

  val probe_block_vec = entries.map {
    case e =>
      e.io.probe.req <> io.probe.req
      e.io.probe.block
  }

  val can_merge_vec = WireInit(VecInit(Seq.fill(reqNum)(false.B)))
  val match_from_pipe = WireInit(VecInit(Seq.fill(reqNum)(false.B)))
  val match_from_ith_pipe = WireInit(0.U(log2Up(reqNum).W))
  val can_merge_from_pipe = WireInit(VecInit(Seq.fill(reqNum)(false.B)))
  val can_merge_from_pipe_mshr = Wire(Vec(reqNum, UInt(log2Up(cfg.nMissEntries).W)))
  val can_merge_store_from_pipe = WireInit(VecInit(Seq.fill(reqNum)(false.B)))
  val can_allocate_vec = WireInit(VecInit(Seq.fill(reqNum)(false.B)))

  // ===== Analyze all requests =====
  // Analysis result for all queryMQ requests
  val analysis = WireInit(0.U.asTypeOf(new ReqAnalysisResult(reqNum, cfg.nMissEntries)))

  // Build free entry list for efficient allocation
  // free_entry_list(i) = the i-th free entry index
  // For example, if entries 2, 5, 7 are free, then free_entry_list = [2, 5, 7, x, x, ...]
  // Use parallel prefix sum (Kogge-Stone) for O(log n) delay

  // Initial state: each entry is 1 if free, 0 if busy
  val initial_free = VecInit(entries.map(_.io.primary_ready).map(_.asUInt))

  // Kogge-Stone parallel prefix sum
  // ps_stage[s][e] = prefix sum after stage s, for entry e
  val num_stages = log2Up(cfg.nMissEntries)
  val ps_stage = VecInit(Seq.fill(num_stages + 1)(WireInit(VecInit(Seq.fill(cfg.nMissEntries)(0.U(log2Up(cfg.nMissEntries + 1).W))))))

  // Stage 0: initial values
  for (e <- 0 until cfg.nMissEntries) {
    ps_stage(0)(e) := initial_free(e)
  }

  // Subsequent stages: parallel prefix sum
  for (stage <- 0 until num_stages) {
    val stride = 1 << stage
    for (e <- 0 until cfg.nMissEntries) {
      if (e < stride) {
        ps_stage(stage + 1)(e) := ps_stage(stage)(e)
      } else {
        ps_stage(stage + 1)(e) := ps_stage(stage)(e) + ps_stage(stage)(e - stride)
      }
    }
  }

  // Extract "count before" (shift right by 1)
  val free_count_before = WireInit(VecInit(Seq.fill(cfg.nMissEntries)(0.U(log2Up(cfg.nMissEntries + 1).W))))
  for (e <- 0 until cfg.nMissEntries) {
    if (e == 0) {
      free_count_before(0) := 0.U
    } else {
      free_count_before(e) := ps_stage(num_stages)(e - 1)
    }
  }

  // Build the free entry list using one-hot encoding
  val free_entry_onehot = WireInit(VecInit(Seq.fill(cfg.nMissEntries)(VecInit(Seq.fill(cfg.nMissEntries)(false.B)))))
  for (i <- 0 until cfg.nMissEntries) {
    for (e <- 0 until cfg.nMissEntries) {
      free_entry_onehot(i)(e) := entries(e).io.primary_ready && free_count_before(e) === i.U
    }
  }

  // Convert one-hot to entry index
  val free_entry_list = WireInit(VecInit(Seq.fill(cfg.nMissEntries)(0.U(log2Up(cfg.nMissEntries).W))))
  for (i <- 0 until cfg.nMissEntries) {
    for (e <- 0 until cfg.nMissEntries) {
      when (free_entry_onehot(i)(e)) {
        free_entry_list(i) := e.U
      }
    }
  }

  // Total count of free entries
  val free_entry_count = ps_stage(num_stages)(cfg.nMissEntries - 1)

  // Detect address conflicts (compress strategy)
  val addr_conflicts = WireInit(VecInit(Seq.fill(reqNum)(VecInit(Seq.fill(reqNum)(false.B)))))
  // block_match & not_alias_match, should not compress/alloc/merge
  val req_reject_vec = WireInit(VecInit(Seq.fill(reqNum)(false.B)))
  for (i <- 0 until reqNum) {
    for (j <- 0 until reqNum) {
      addr_conflicts(i)(j) := io.queryMQ(j).req.valid && io.queryMQ(i).req.valid && j.U =/= i.U &&
                              get_block(io.queryMQ(i).req.bits.addr) === get_block(io.queryMQ(j).req.bits.addr) &&
                              is_alias_match(io.queryMQ(i).req.bits.vaddr, io.queryMQ(j).req.bits.vaddr)
    }

    // bigger index miss_req will be reject: 
    req_reject_vec(i) :=(0 until reqNum).map{ j => 
      io.queryMQ(j).req.valid && io.queryMQ(i).req.valid && j.U < i.U &&
        get_block(io.queryMQ(i).req.bits.addr) === get_block(io.queryMQ(j).req.bits.addr) &&
        !is_alias_match(io.queryMQ(i).req.bits.vaddr, io.queryMQ(j).req.bits.vaddr)  
    }.reduce(_ || _)
  }

  for(i <- 0 until reqNum) {
    can_merge_from_pipe_mshr(i) := 0.U

    for (j <- (0 until reqNum).reverse) {
      val signals_j = computeMatchSignals(parallel_pipe_regs(j).req, io.queryMQ(i).req.bits)

      // merge from pipe_reg
      when (parallel_pipe_regs(j).merge_req(signals_j) && !parallel_pipe_regs(j).cancel) {
        can_merge_from_pipe(i) := true.B
        can_merge_from_pipe_mshr(i) := parallel_pipe_regs(j).mshr_id
      }

      // A store merging into an allocating pipe reg must be included before
      // sending Acquire, otherwise the entry cmd and Grant param can diverge.
      when(parallel_pipe_regs(j).alloc && !parallel_pipe_regs(j).cancel && parallel_pipe_regs(j).merge_req(signals_j) && io.queryMQ(i).req.bits.isFromStore && io.queryMQ(i).req.valid) {
        can_merge_store_from_pipe(j) := true.B
      }

      // do not alloc when (addr_match & alias_match)
      when(parallel_pipe_regs(j).alloc && !parallel_pipe_regs(j).cancel && signals_j.block_match) {
        match_from_pipe(i) := true.B
        match_from_ith_pipe := j.U
      }
    }
  }

  for(i <- 0 until reqNum) {
    val signals = signals_vec(i)

    can_allocate_vec(i) := free_entry_count =/= 0.U && !req_reject_vec(i) &&
                            !ParallelORR(Cat(block_match_seqs(i) ++ Seq(match_from_pipe(i))))

    val can_merge_from_entry = ParallelORR(Cat(secondary_ready_vec(i)))

    can_merge_vec(i) := can_merge_from_entry || can_merge_from_pipe(i)
  }

  // Pre-calculate which requests need allocation
  // This is used to calculate alloc_order for proper free entry assignment
  val needs_allocate = WireInit(VecInit(Seq.fill(reqNum)(false.B)))

  for (i <- 0 until reqNum) {
    val req_valid = io.queryMQ(i).req.valid

    // Quick check: will this request need allocation?
    val has_addr_conflict = addr_conflicts(i).asUInt.orR

    needs_allocate(i) := false.B
    when (req_valid) {
      when (has_addr_conflict) {
        // Compress case: needs allocation if can_allocate (and cannot merge/reject)
        needs_allocate(i) := can_allocate_vec(i) && (analysis.compress_group(i) === i.U)
      }.elsewhen (can_allocate_vec(i)) {
        // Allocate only case
        needs_allocate(i) := true.B
      }
    }
  }

  // Calculate alloc_order(i) = number of valid allocation requests with index < i
  val alloc_order = WireInit(VecInit(Seq.fill(reqNum)(0.U(log2Up(reqNum + 1).W))))
  for (i <- 0 until reqNum) {
    if (i == 0) {
      alloc_order(0) := 0.U
    } else {
      val prev_count = alloc_order(i - 1)
      alloc_order(i) := Mux(needs_allocate(i - 1), prev_count + 1.U, prev_count)
    }
  }

  // For each request, determine strategy (allocate/merge/compress)
  // Priority: compress > merge > allocate
  for (i <- 0 until reqNum) {
    val req_valid = io.queryMQ(i).req.valid
    val req_bits = io.queryMQ(i).req.bits

    val has_addr_conflict = addr_conflicts(i).asUInt.orR

    val merge_targets_entry = secondary_ready_vec(i)
    val merge_targets_pipe_reg = can_merge_from_pipe(i)

    // Manually find the first merge target
    val merge_target_id = WireInit(0.U(log2Up(cfg.nMissEntries).W))
    for (e <- (0 until cfg.nMissEntries).reverse) {
      when (merge_targets_entry(e)) {
        merge_target_id := e.U
      }.elsewhen (merge_targets_pipe_reg) {
        merge_target_id := can_merge_from_pipe_mshr(i)
      }
    }

    // Determine strategy based on priority
    // strategy, bit 0: 1.U, allocate, bit 1: 2.U, merge, bit 2: 4.U, compress
    when (req_valid) {
      when (addr_conflicts(i).asUInt.orR) {
        // Use lowest index among conflicting requests as group ID
        val conflict_group_id = WireInit(i.U(log2Up(reqNum).W))

        // Check all smaller indices for conflicts
        for (j <- (0 until i).reverse) {
          when (addr_conflicts(i)(j)) {
            conflict_group_id := j.U
          }
        }

        analysis.compress_group(i) := conflict_group_id

        // Has address conflict -> needs compression
        analysis.strategy(i) := 4.U
        analysis.target_mshr(i) := 0.U

        when (can_merge_vec(i)) {
          // compress & merge
          analysis.strategy(i) := 4.U | 2.U
          analysis.target_mshr(i) := merge_target_id
        }.elsewhen (can_allocate_vec(i)) {
          // compress & alloc
          analysis.strategy(i) := 4.U | 1.U
          // Allocate using alloc_order to account for invalid earlier requests
          val has_enough_free = alloc_order(i) < free_entry_count

          when (has_enough_free) {
            analysis.target_mshr(i) := free_entry_list(alloc_order(i))
          }.otherwise {
            // Not enough free entries, remove allocate bit
            analysis.strategy(i) := 4.U
            analysis.target_mshr(i) := 0.U
          }
        }

        // Only the first request in the group is valid for compress strategy
        analysis.valid(i) := i.U === conflict_group_id
      }.elsewhen (can_merge_vec(i)) {
        // Can merge to existing MSHR
        analysis.strategy(i) := 2.U
        analysis.valid(i) := true.B
        analysis.target_mshr(i) := merge_target_id
        analysis.compress_group(i) := i.U
      }.elsewhen (can_allocate_vec(i)) {
        // Can allocate to new MSHR
        analysis.strategy(i) := 1.U
        analysis.valid(i) := true.B

        // Allocate using alloc_order to account for invalid earlier requests
        val has_enough_free = alloc_order(i) < free_entry_count

        when (has_enough_free) {
          analysis.target_mshr(i) := free_entry_list(alloc_order(i))
          analysis.compress_group(i) := i.U
        }.otherwise {
          // Not enough free entries, mark as invalid
          analysis.strategy(i) := 0.U
          analysis.valid(i) := false.B
          analysis.target_mshr(i) := 0.U
          analysis.compress_group(i) := i.U
        }
      }.otherwise {
        // No available resource
        analysis.strategy(i) := 0.U  // none
        analysis.valid(i) := false.B
        analysis.target_mshr(i) := 0.U
        analysis.compress_group(i) := i.U
      }
    }
  }

  // ===== Generate ready signals (immediate response) =====
  // Generate ready signals for queryMQ in the same cycle
  // This allows upstream modules to know immediately if requests are accepted
  for (i <- 0 until reqNum) {
    val has_compress = (analysis.strategy(i) & 4.U) =/= 0.U
    val has_merge = (analysis.strategy(i) & 2.U) =/= 0.U
    val has_alloc = (analysis.strategy(i) & 1.U) =/= 0.U
    val is_valid = analysis.valid(i)
    val target_mshr = analysis.target_mshr(i)  // Explicit use to prevent Chisel optimization
    val target_group = analysis.compress_group(i)

    // Compress: only the first in group is master, others are slaves
    val is_first_in_group = target_group === i.U
    val compress_ready = has_compress && Mux(is_first_in_group, has_merge || has_alloc, (analysis.strategy(target_group) & 3.U) =/= 0.U)

    val merge_ready = has_merge && !has_compress && is_valid

    val alloc_ready = has_alloc && !has_compress && !has_merge && is_valid

    io.queryMQ(i).ready := (compress_ready || merge_ready || alloc_ready) && 
      !(io.wbq_block_miss_req(i) || io.wbq_block_miss_req(analysis.compress_group(i)) || io.queryMQ(analysis.compress_group(i)).req.bits.cancel)
  }

  // For each queryMQ request that was granted, connect to MissEntry or PipeReg
  val query_fire = WireInit(VecInit(Seq.fill(reqNum)(false.B)))
  for (i <- 0 until reqNum) {
    query_fire(i) := io.queryMQ(i).req.valid && io.queryMQ(i).ready
  }

  /*  MissQueue enq logic is now splitted into 2 cycles
   *
   */
  // Update parallel pipeline registers
  for (i <- 0 until reqNum) {
    when (io.queryMQ(i).req.valid) {
      parallel_pipe_regs(i).req := io.queryMQ(i).req.bits
    }
    parallel_pipe_regs(i).alloc := ((analysis.strategy(i) & 1.U) =/= 0.U) &&
                                    (analysis.compress_group(i) === i.U) &&
                                    !io.queryMQ(i).req.bits.cancel &&
                                    !io.wbq_block_miss_req(i)

    parallel_pipe_regs(i).merge := ((analysis.strategy(i) & 2.U) =/= 0.U) &&
                                    (analysis.compress_group(i) === i.U) &&
                                    !io.queryMQ(i).req.bits.cancel &&
                                    !io.wbq_block_miss_req(i)

    parallel_pipe_regs(i).mshr_id := analysis.target_mshr(i)
    parallel_pipe_regs(i).cancel := io.wbq_block_miss_req(i)
  }

  val req_mshr_handled_vec = entries.map(_.io.req_handled_by_this_entry)

  // For compressed requests, we need to return the actual MSHR that will handle them
  // This is the target_mshr of the first request in their compression group
  val actual_target_mshr_for_group = WireInit(VecInit(Seq.fill(reqNum)(0.U(log2Up(cfg.nMissEntries).W))))
  for (i <- 0 until reqNum) {
    // Find the first request in this compression group
    val group_leader = analysis.compress_group(i)
    // All requests in the same group share the same target MSHR
    actual_target_mshr_for_group(i) := Mux(
      group_leader === i.U,
      analysis.target_mshr(i),  // I am the leader, use my own target
      analysis.target_mshr(group_leader)  // I'm a follower, use leader's target
    )
  }

  for(i <- 0 until reqNum) {
    // merged to pipeline reg
    io.resp(i).id := Mux(
      can_merge_from_pipe(i),
      can_merge_from_pipe_mshr(i),
      actual_target_mshr_for_group(i)
    )
    io.resp(i).handled := (Cat(req_mshr_handled_vec).orR || can_merge_from_pipe(i)) && query_fire(i)
    io.resp(i).merged := (analysis.strategy(i) & 2.U) =/= 0.U
  }

  val source_except_load_cnt = RegInit(0.U(10.W))
  for(i <- 0 until reqNum) {
    when(VecInit(req_mshr_handled_vec).asUInt.orR || can_merge_from_pipe(i)) {
      when(io.queryMQ(i).req.bits.isFromLoad) {
        source_except_load_cnt := 0.U
      }.otherwise {
        when(io.queryMQ(i).req.bits.isFromStore) {
          source_except_load_cnt := source_except_load_cnt + 1.U
        }
      }
    }
  }
  val Threshold = 8
  val memSetPattenDetected = GatedValidRegNext((source_except_load_cnt >= Threshold.U) && io.lqEmpty)

  io.memSetPattenDetected := memSetPattenDetected

  val forwardInfo_vec = VecInit(entries.map(_.io.forwardInfo))
  val VLENB = VLEN / 8
  // Forwarding paddr CAM, shared by io.forward and io.forward_stData
  val paddrFwd = Wire(Vec(LoadPipelineWidth, UInt(PAddrBits.W)))
  val s1PaddrMatchVec = Wire(Vec(LoadPipelineWidth, Vec(cfg.nMissEntries, Bool())))
  val s1SelectOH = Wire(Vec(LoadPipelineWidth, UInt((cfg.nMissEntries).W)))
  val s1ForwardInfo = Wire(Vec(LoadPipelineWidth, new MissEntryForwardIO))
  val paddrFwd_selOH = Wire(Vec(LoadPipelineWidth, Vec(blockBytes / VLENB, Bool())))
  val s1RespDataFwd = Wire(Vec(LoadPipelineWidth, UInt(VLEN.W)))
  for (i <- 0 until LoadPipelineWidth) {
    paddrFwd(i) := Mux(RegNext(io.forward_stData(i).s0Req.valid), io.forward_stData(i).s1Req.paddr, io.forward(i).s1Req.paddr)
    s1PaddrMatchVec(i) := VecInit(forwardInfo_vec.map{ case info =>
      (paddrFwd(i) >> blockOffBits) === (info.paddr >> blockOffBits) && info.inflight})
    s1SelectOH(i) := s1PaddrMatchVec(i).asUInt
    s1ForwardInfo(i) := Mux1H(s1SelectOH(i), forwardInfo_vec)
    // Select VLEN (128-bit) data from cacheline data
    paddrFwd_selOH(i) := (0 until (blockBytes / VLENB)).map(k => paddrFwd(i)(blockOffBits - 1, log2Up(VLENB)) === k.U)
    s1RespDataFwd(i) := Mux1H(paddrFwd_selOH(i), s1ForwardInfo(i).raw_data.grouped(VLEN / rowBits).map(VecInit(_).asUInt).toSeq)
  }

  io.forward.zipWithIndex.foreach { case (forward, i) =>
    val s0ReqValid = forward.s0Req.valid
    val s0Req = forward.s0Req.bits
    val s1ReqValid = RegNext(s0ReqValid)
    val s1Req = RegEnable(s0Req, s0ReqValid)
    val mshrIdOH = UIntToOH(s1Req.mshrId)
    val s1BeatMatchVec  = VecInit(forwardInfo_vec.map{ case info =>
      Mux(paddrFwd(i)(log2Up(refillBytes)).asBool,
        info.lastbeat_valid,
        info.firstbeat_valid
    )})
    val s1RespValid = s1ReqValid && (s1SelectOH(i) & s1BeatMatchVec.asUInt).orR

    // store-load forwarding (from io.forward_stData)
    val s1ReqValid_stLdFwd = RegNext(io.forward_stData(i).s0Req.valid)
    val s1RespMask_stLdFwd = Mux1H(paddrFwd_selOH(i),
          s1ForwardInfo(i).store_mask.grouped(VLENB).map(x => Mux(s1ForwardInfo(i).isFromStore, x, 0.U)))
    val s1RespValid_stLdFwd = s1ReqValid_stLdFwd && s1SelectOH(i).orR

    val s2Resp_Valid = RegNext(s1RespValid)
    val s2Resp_stLdFwd_Valid = RegNext(s1RespValid_stLdFwd)

    forward.s2Resp.valid := s2Resp_Valid || s2Resp_stLdFwd_Valid
    forward.s2Resp.bits.matchInvalid := false.B
    forward.s2Resp.bits.forwardData := RegEnable(s1RespDataFwd(i).asTypeOf(forward.s2Resp.bits.forwardData),
                                                 s1ReqValid || s1ReqValid_stLdFwd)
    forward.s2Resp.bits.forwardMask := VecInit((0 until VLENB).map(k =>
      Mux(s2Resp_Valid, true.B, RegEnable(s1RespMask_stLdFwd(k), s1RespValid_stLdFwd) && s2Resp_stLdFwd_Valid)
    ))
    forward.s2Resp.bits.denied := RegEnable(s1ForwardInfo(i).denied, s1ReqValid) && s2Resp_Valid
    forward.s2Resp.bits.corrupt := RegEnable(s1ForwardInfo(i).corrupt, s1ReqValid) && s2Resp_Valid
    io.forwardS1PAddrMatch(i) := s1ReqValid && (mshrIdOH & s1PaddrMatchVec(i).asUInt).orR
    XSError(((s1SelectOH(i) - 1.U) & s1SelectOH(i)).orR && s1RespValid, "multi mshr hit when forward!\n")
  }

  for(i <- 0 until reqNum) {
    assert(RegNext(PopCount(secondary_ready_vec(i)) <= 1.U || !io.queryMQ(i).req.valid))
  }

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

  private def selectChannel(addr: UInt, mshrId: UInt): UInt = {
    if (hasDualChannel) {
      if (channelSelByAddr) get_block(addr)(memChannelBits - 1, 0)
      else mshrId(memChannelBits - 1, 0)
    } else {
      0.U(memChannelBits.W)
    }
  }

  private def splitDecoupledByChannel[T <: Data](source: DecoupledIO[T], channel: UInt): (DecoupledIO[T], DecoupledIO[T]) = {
    val ch0 = Wire(Decoupled(chiselTypeOf(source.bits)))
    val ch1 = Wire(Decoupled(chiselTypeOf(source.bits)))

    ch0.valid := source.valid && channel === 0.U
    ch0.bits := source.bits

    ch1.valid := source.valid && channel === 1.U
    ch1.bits := source.bits

    source.ready := Mux(channel === 1.U, ch1.ready, ch0.ready)
    (ch0, ch1)
  }

  io.mem_grant.ready := false.B
  if (hasDualChannel) {
    io.mem_grant_1.get.ready := false.B
  }

  val nMaxPrefetchEntry = Constantin.createRecord(s"nMaxPrefetchEntry${p(XSCoreParamsKey).HartId}", initValue = cfg.nMissEntries - 2)
  entries.zipWithIndex.foreach {
    case (e, i) =>
      val former_primary_ready = if(i == 0)
        false.B
      else
        Cat((0 until i).map(j => entries(j).io.primary_ready)).orR

      val former_ready_count = if(i == 0)
        0.U
      else
        PopCount((0 until i).map(j => entries(j).io.primary_ready))

      val has_n_former_ready = former_ready_count >= reqNum.U
      e.io.entry_valid := !has_n_former_ready

      e.io.hartId := io.hartId
      e.io.id := i.U
      e.io.l2_pf_store_only := io.l2_pf_store_only
      e.io.wbq_block_miss_req := io.wbq_block_miss_req

      e.io.mem_grant.valid := false.B
      e.io.mem_grant.bits := DontCare
      if (hasDualChannel) {
        when ((io.mem_grant.bits.source === i.U) && io.mem_grant.valid) {
          e.io.mem_grant <> io.mem_grant
        }
        when ((io.mem_grant_1.get.bits.source === i.U) && io.mem_grant_1.get.valid) {
          e.io.mem_grant <> io.mem_grant_1.get
        }
      } else {
        when (io.mem_grant.bits.source === i.U) {
          e.io.mem_grant <> io.mem_grant
        }
      }

      e.io.miss_req_pipe_reg       := DontCare
      e.io.miss_req_pipe_reg.merge := false.B
      e.io.miss_req_pipe_reg.alloc := false.B
      for(req <- 0 until reqNum) {
        when(parallel_pipe_regs(req).reg_valid() && parallel_pipe_regs(req).mshr_id === i.U) {
          e.io.miss_req_pipe_reg := parallel_pipe_regs(req)
        }
      }

      e.io.acquire_fired_by_pipe_reg := false.B
      for(j <- 0 until reqNum) {
        when(acquire_from_pipereg_vec(j).fire && parallel_pipe_regs(j).mshr_id === i.U) {
          e.io.acquire_fired_by_pipe_reg := true.B
        }
      }

      e.io.main_pipe_resp := io.main_pipe_resp.valid && io.main_pipe_resp.bits.ack_miss_queue && io.main_pipe_resp.bits.miss_id === i.U
      e.io.main_pipe_replay := io.mainpipe_info.s2_valid && io.mainpipe_info.s2_replay_to_mq && io.mainpipe_info.s2_miss_id === i.U
      e.io.main_pipe_evict_BtoT_way := io.mainpipe_info.s2_valid && io.mainpipe_info.s2_evict_BtoT_way && io.mainpipe_info.s2_miss_id === i.U
      e.io.main_pipe_next_evict_way := io.mainpipe_info.s2_next_evict_way
      e.io.main_pipe_refill_resp := io.mainpipe_info.s3_valid && io.mainpipe_info.s3_refill_resp && io.mainpipe_info.s3_miss_id === i.U

      e.io.memSetPattenDetected := memSetPattenDetected
      e.io.nMaxPrefetchEntry := nMaxPrefetchEntry

      e.io.main_pipe_req.ready := io.main_pipe_req.ready

      for (j <- 0 until reqNum) {
        e.io.queryME(j).req.valid := io.queryMQ(j).req.valid
        e.io.queryME(j).req.bits  := io.queryMQ(j).req.bits.toMissReqWoStoreData()
      }

      e.io.l2_hint.valid := false.B
      e.io.l2_hint.bits := DontCare
      val entryHintMatches = io.l2_hint.map(hint => hint.valid && hint.bits.sourceId === i.U)
      assert(PopCount(VecInit(entryHintMatches)) <= 1.U, "multiple l2_hint hits for one miss entry")
      for (ch <- 0 until cfg.numMemChannels) {
        when(entryHintMatches(ch)) {
          e.io.l2_hint <> io.l2_hint(ch)
        }
      }

      e.io.wfi.wfiReq := io.wfi.wfiReq
  }

  cmo_unit.io.wfi.wfiReq := io.wfi.wfiReq
  cmo_unit.io.req <> io.cmo_req
  io.cmo_resp <> cmo_unit.io.resp_to_lsq
  if (hasDualChannel) {
    cmo_unit.io.resp_chanD.valid := false.B
    cmo_unit.io.resp_chanD.bits := DontCare
    when (io.mem_grant.valid && io.mem_grant.bits.opcode === TLMessages.CBOAck) {
      cmo_unit.io.resp_chanD <> io.mem_grant
    }
    when (io.mem_grant_1.get.valid && io.mem_grant_1.get.bits.opcode === TLMessages.CBOAck) {
      cmo_unit.io.resp_chanD <> io.mem_grant_1.get
    }
  } else {
    cmo_unit.io.resp_chanD.valid := false.B
    cmo_unit.io.resp_chanD.bits := DontCare
    when (io.mem_grant.valid && io.mem_grant.bits.opcode === TLMessages.CBOAck) {
      cmo_unit.io.resp_chanD <> io.mem_grant
    }
  }
  io.wfi.wfiSafe := (Seq(cmo_unit.io.wfi.wfiSafe) ++ entries.map(_.io.wfi.wfiSafe)).reduce(_&&_)

  // io.req.ready := accept
  io.refill_to_ldq.valid := Cat(entries.map(_.io.refill_to_ldq.valid)).orR
  io.refill_to_ldq.bits := ParallelMux(entries.map(_.io.refill_to_ldq.valid) zip entries.map(_.io.refill_to_ldq.bits))

  io.refill_info.valid := VecInit(entries.zipWithIndex.map{ case(e,i) => e.io.refill_info.valid && io.mainpipe_info.s2_valid && io.mainpipe_info.s2_miss_id === i.U}).asUInt.orR
  io.refill_info.bits := Mux1H(entries.zipWithIndex.map{ case(e,i) => (io.mainpipe_info.s2_miss_id === i.U) -> e.io.refill_info.bits })

  io.refill_train.valid := VecInit(entries.zipWithIndex.map{ case(e,i) => e.io.refill_train.valid && io.mainpipe_info.s2_valid && io.mainpipe_info.s2_miss_id === i.U}).asUInt.orR
  io.refill_train.bits := Mux1H(entries.zipWithIndex.map{ case(e,i) => (io.mainpipe_info.s2_miss_id === i.U) -> e.io.refill_train.bits })

  for(i <- 0 until reqNum) {
    acquire_from_pipereg_vec(i).valid := parallel_pipe_regs(i).alloc && !can_merge_store_from_pipe(i) && !io.wfi.wfiReq
    acquire_from_pipereg_vec(i).bits := parallel_pipe_regs(i).get_acquire(io.l2_pf_store_only)

    XSPerfAccumulate(s"acquire_fire_from_pipereg_$i", acquire_from_pipereg_vec(i).fire)
    XSPerfAccumulate(s"parallel_pipe_regs_valid_$i", parallel_pipe_regs(i).reg_valid())
  }

  for(i <- 0 until reqNum) {
    parallel_regs_channel(i) := selectChannel(parallel_pipe_regs(i).req.addr, parallel_pipe_regs(i).mshr_id)
  }

  if (hasDualChannel) {
    val splitPipeAcquires = acquire_from_pipereg_vec.zipWithIndex.map {
      case (aq, i) => splitDecoupledByChannel(aq, parallel_regs_channel(i))
    }
    val splitEntryAcquires = entries.map(e => splitDecoupledByChannel(e.io.mem_acquire, e.io.channel_sel))
    val splitEntryFinishes = entries.map(e => splitDecoupledByChannel(e.io.mem_finish, e.io.channel_sel))
    val splitCmoAcquire = splitDecoupledByChannel(cmo_unit.io.req_chanA, cmo_unit.io.channel_sel)

    val (pipeAcquireCh0, pipeAcquireCh1) = splitPipeAcquires.unzip
    val (entryAcquireCh0, entryAcquireCh1) = splitEntryAcquires.unzip
    val (finishCh0, finishCh1) = splitEntryFinishes.unzip
    val (cmoAcquireCh0, cmoAcquireCh1) = (splitCmoAcquire._1, splitCmoAcquire._2)

    TLArbiter.lowest(edge, io.mem_acquire, (Seq(cmoAcquireCh0) ++ pipeAcquireCh0 ++ entryAcquireCh0):_*)
    TLArbiter.lowest(edge, io.mem_acquire_1.get, (Seq(cmoAcquireCh1) ++ pipeAcquireCh1 ++ entryAcquireCh1):_*)
    TLArbiter.lowest(edge, io.mem_finish, finishCh0:_*)
    TLArbiter.lowest(edge, io.mem_finish_1.get, finishCh1:_*)
  } else {
    val acquire_sources = Seq(cmo_unit.io.req_chanA) ++ acquire_from_pipereg_vec ++ entries.map(_.io.mem_acquire)
    TLArbiter.lowest(edge, io.mem_acquire, acquire_sources:_*)
    TLArbiter.lowest(edge, io.mem_finish, entries.map(_.io.mem_finish):_*)
  }

  // amo's main pipe req out
  arbiter(entries.map(_.io.main_pipe_req), io.main_pipe_req, Some("main_pipe_req"))

  io.probe.block := Cat(probe_block_vec).orR
  io.replace.block := Cat(
    entries.map { e =>
      e.io.replace.req <> io.replace.req
      e.io.replace.block
    } ++ parallel_pipe_regs.map(_.block_and_alias_match(io.replace.req.bits))
  ).orR

  val btot_evict_set_hit = entries.map(e => e.io.req_isBtoT && e.io.req_vaddr.valid && addr_to_dcache_set(e.io.req_vaddr.bits) === io.evict_set) ++
    parallel_pipe_regs.map(_.evict_set_match(io.evict_set))
  val btot_occupy_ways = entries.map(e => e.io.occupy_way) ++ parallel_pipe_regs.map(_.req.occupy_way)
  io.btot_ways_for_set := btot_evict_set_hit.zip(btot_occupy_ways).map {
    case (hit, way) => Fill(nWays, hit) & way
  }.reduce(_|_)

  // LoadPipe occupy check
  for (i <- 0 until LoadPipelineWidth) {
    val occupy_set_hits = entries.map(
      e => e.io.req_isBtoT && e.io.req_vaddr.valid && addr_to_dcache_set(e.io.req_vaddr.bits) === io.occupy_set(i)
    ) ++ parallel_pipe_regs.map(_.evict_set_match(io.occupy_set(i)))
    val occupy_ways = occupy_set_hits.zip(btot_occupy_ways).map {
      case (hit, way) => Fill(nWays, hit) & way
    }.reduce(_|_)
    io.occupy_fail(i) := PopCount(occupy_ways) > (nWays-2).U
  }

  io.full := ~Cat(entries.map(_.io.primary_ready)).andR

  // prefetch related. The prefetch_req only in mainPipe, Now!
  val late_in_reg = match_from_pipe(0)
  io.prefetch_stat.pf_late_in_mshr := io.queryMQ(0).req.valid && io.queryMQ(0).req.bits.isFromPrefetch && 
                                        (late_in_reg || Cat(entries.map(_.io.matched)).orR)
  io.prefetch_stat.pf_late_in_mshr_source := ParallelMux(
    Seq(late_in_reg) ++ entries.map(_.io.matched)
    zip
    Seq(parallel_pipe_regs(match_from_ith_pipe).req.pf_source) ++ entries.map(_.io.prefetch_info.hit_pf_source)
  )

  io.prefetch_stat.prefetch_miss := query_fire(0) && io.queryMQ(0).req.bits.isFromPrefetch
  io.prefetch_stat.pf_source := io.queryMQ(0).req.bits.pf_source
  io.prefetch_stat.load_miss := PopCount((0 until reqNum).map(j => query_fire(j) && io.queryMQ(j).req.bits.isFromLoad))

  // compute all miss_req hit prefetch_req or not
  val prefetch_hit_in_reg_vec = Wire(Vec(reqNum, Bool()))
  val prefetch_hit_in_mshr_vec = Wire(Vec(reqNum, Bool()))
  for(i <- 0 until reqNum) {
    val signals_ = (0 until reqNum).map(j => computeMatchSignals(parallel_pipe_regs(j).req, io.queryMQ(i).req.bits))
    val hit_in_reg = (0 until reqNum).map(j => parallel_pipe_regs(j).prefetch_late_en(signals_(j), io.queryMQ(i).req.bits.toMissReqWoStoreData(), io.queryMQ(i).req.valid))
    
    prefetch_hit_in_reg_vec(i) := hit_in_reg.asUInt.orR
    prefetch_hit_in_mshr_vec(i) := io.queryMQ(i).req.valid && !io.queryMQ(i).req.bits.isFromPrefetch && Cat(entries.map(_.io.prefetch_info.hit_prefetch(i))).orR
  }
  io.prefetch_stat.hit_pf_in_mshr := PopCount((0 until reqNum).map(i => prefetch_hit_in_reg_vec(i) || prefetch_hit_in_mshr_vec(i)))
  for(i <- 0 until reqNum) {
    io.prefetch_stat.hit_pf_in_mshr_source(i) := ParallelMux(
      Seq(prefetch_hit_in_reg_vec(i)) ++ entries.map(_.io.prefetch_info.hit_prefetch(i))
      zip
      Seq(parallel_pipe_regs(i).req.pf_source) ++ entries.map(_.io.prefetch_info.hit_pf_source)
    )
  }

  // L1MissTrace Chisel DB - support multiple enqueue ports
  val debug_miss_trace_vec = Wire(Vec(reqNum, new L1MissTrace))
  for (i <- 0 until reqNum) {
    debug_miss_trace_vec(i).vaddr := io.queryMQ(i).req.bits.vaddr
    debug_miss_trace_vec(i).paddr := io.queryMQ(i).req.bits.addr
    debug_miss_trace_vec(i).source := io.queryMQ(i).req.bits.source
    debug_miss_trace_vec(i).pc := io.queryMQ(i).req.bits.pc
  }

  val isWriteL1MissQMissTable = Constantin.createRecord(s"isWriteL1MissQMissTable${p(XSCoreParamsKey).HartId}")
  val table = ChiselDB.createTable(s"L1MissQMissTrace_hart${p(XSCoreParamsKey).HartId}", new L1MissTrace)
  for (i <- 0 until reqNum) {
    table.log(debug_miss_trace_vec(i), isWriteL1MissQMissTable.orR && query_fire(i) && !io.queryMQ(i).req.bits.cancel && ((analysis.strategy(i) & 1.U) =/= 0.U), s"MissQueue_$i", clock, reset)
  }

  // Difftest
  if (env.EnableDifftest) {
    val refillDiffValidVec = VecInit(entries.map(e =>
      e.io.refill_to_ldq.valid &&
      e.io.refill_to_ldq.bits.hasdata &&
      e.io.refill_to_ldq.bits.refill_done
    ))
    val refillDiffMask = refillDiffValidVec.asUInt
    val refillDiffFirstOH = PriorityEncoderOH(refillDiffMask)
    val refillDiffRemainMask = refillDiffMask & ~refillDiffFirstOH
    val refillDiffSecondOH = PriorityEncoderOH(refillDiffRemainMask)
    val refillDiffFirstValid = refillDiffMask.orR
    val refillDiffSecondValid = refillDiffRemainMask.orR

    class RefillDiffEntry extends Bundle {
      val addr = UInt(PAddrBits.W)
      val data = Vec(8, UInt((cfg.blockBytes).W))
      val mask = UInt((cfg.blockBytes / 8).W)
    }

    val refillDiffFirstEntry = WireDefault(0.U.asTypeOf(new RefillDiffEntry))
    val refillDiffSecondEntry = WireDefault(0.U.asTypeOf(new RefillDiffEntry))
    when(refillDiffFirstValid) {
      refillDiffFirstEntry.addr := Mux1H(refillDiffFirstOH, entries.map(_.io.refill_to_ldq.bits.addr))
      refillDiffFirstEntry.data := Mux1H(
        refillDiffFirstOH,
        entries.map(_.io.refill_to_ldq.bits.data_raw.asTypeOf(refillDiffFirstEntry.data))
      )
      refillDiffFirstEntry.mask := VecInit.fill(refillDiffFirstEntry.mask.getWidth)(true.B).asUInt
    }
    when(refillDiffSecondValid) {
      refillDiffSecondEntry.addr := Mux1H(refillDiffSecondOH, entries.map(_.io.refill_to_ldq.bits.addr))
      refillDiffSecondEntry.data := Mux1H(
        refillDiffSecondOH,
        entries.map(_.io.refill_to_ldq.bits.data_raw.asTypeOf(refillDiffSecondEntry.data))
      )
      refillDiffSecondEntry.mask := VecInit.fill(refillDiffSecondEntry.mask.getWidth)(true.B).asUInt
    }

    val refillDiffQueue = Module(new Queue(new RefillDiffEntry, entries = cfg.nMissEntries, flow = false, pipe = false))
    val queueHasPending = refillDiffQueue.io.deq.valid
    val emitCurrent = refillDiffFirstValid && (!queueHasPending || refillDiffSecondValid)
    val emitQueued = queueHasPending && !emitCurrent
    val enqueueSecondCurrent = emitCurrent && refillDiffSecondValid
    val enqueueFirstCurrent = emitQueued && refillDiffFirstValid

    refillDiffQueue.io.deq.ready := emitQueued
    refillDiffQueue.io.enq.valid := enqueueSecondCurrent || enqueueFirstCurrent
    refillDiffQueue.io.enq.bits := Mux(enqueueSecondCurrent, refillDiffSecondEntry, refillDiffFirstEntry)
    assert(!refillDiffQueue.io.enq.valid || refillDiffQueue.io.enq.ready, "refill difftest queue overflow")

    val difftest = DifftestModule(new DiffRefillEvent, dontCare = true)
    difftest.coreid := io.hartId
    difftest.index := 1.U
    difftest.valid := emitCurrent || emitQueued
    difftest.addr := Mux(emitQueued, refillDiffQueue.io.deq.bits.addr, refillDiffFirstEntry.addr)
    difftest.data := Mux(emitQueued, refillDiffQueue.io.deq.bits.data, refillDiffFirstEntry.data)
    difftest.mask := Mux(emitQueued, refillDiffQueue.io.deq.bits.mask, refillDiffFirstEntry.mask)
  }

  if (env.EnableDifftest) {
    // Store-miss refill completed in MainPipe S3: update difftest (DiffSbufferEvent).
    val mq_s3_sel_info = Mux1H(
      entries.map(e => (io.mainpipe_info.s3_miss_id === e.io.id) -> e.io.forwardInfo)
    )
    val hasStore = mq_s3_sel_info.store_mask.orR
    val difftest_store = DifftestModule(new DiffSbufferEvent, delay = 1)
    difftest_store.coreid := io.hartId
    difftest_store.index := 0.U
    difftest_store.valid := io.mainpipe_info.s3_valid && io.mainpipe_info.s3_refill_resp && hasStore
    difftest_store.addr := mq_s3_sel_info.paddr
    difftest_store.data := mq_s3_sel_info.raw_data.asUInt.asTypeOf(difftest_store.data)
    difftest_store.mask := mq_s3_sel_info.store_mask
  }

  // Perf count - adapted for multiple enqueue ports
  XSPerfAccumulate("miss_req", PopCount(query_fire))
  XSPerfAccumulate("miss_req_allocate", PopCount(Cat((0 until reqNum).map(i => query_fire(i) && ((analysis.strategy(i) & 1.U) =/= 0.U)))))
  XSPerfAccumulate("miss_req_load_allocate", PopCount(Cat((0 until reqNum).map(i => query_fire(i) && ((analysis.strategy(i) & 1.U) =/= 0.U) && io.queryMQ(i).req.bits.isFromLoad))))
  XSPerfAccumulate("miss_req_store_allocate", PopCount(Cat((0 until reqNum).map(i => query_fire(i) && ((analysis.strategy(i) & 1.U) =/= 0.U) && io.queryMQ(i).req.bits.isFromStore))))
  XSPerfAccumulate("miss_req_amo_allocate", PopCount(Cat((0 until reqNum).map(i => query_fire(i) && ((analysis.strategy(i) & 1.U) =/= 0.U) && io.queryMQ(i).req.bits.isFromAMO))))
  XSPerfAccumulate("miss_req_prefetch_allocate", PopCount(Cat((0 until reqNum).map(i => query_fire(i) && ((analysis.strategy(i) & 1.U) =/= 0.U) && io.queryMQ(i).req.bits.isFromPrefetch))))
  XSPerfAccumulate("miss_req_merge_load", PopCount(Cat((0 until reqNum).map(i => query_fire(i) && ((analysis.strategy(i) & 2.U) =/= 0.U) && io.queryMQ(i).req.bits.isFromLoad))))
  XSPerfAccumulate("miss_req_reject_load", PopCount(Cat((0 until reqNum).map(i => io.queryMQ(i).req.valid && !io.queryMQ(i).req.bits.cancel && ((analysis.strategy(i) & 3.U) === 0.U) && io.queryMQ(i).req.bits.isFromLoad))))
  XSPerfAccumulate("probe_blocked_by_miss", io.probe.block)
  XSPerfAccumulate("prefetch_primary_fire", PopCount(Cat((0 until reqNum).map(i => query_fire(i) && ((analysis.strategy(i) & 1.U) =/= 0.U) && io.queryMQ(i).req.bits.isFromPrefetch))))
  XSPerfAccumulate("prefetch_secondary_fire", PopCount(Cat((0 until reqNum).map(i => query_fire(i) && ((analysis.strategy(i) & 2.U) =/= 0.U) && io.queryMQ(i).req.bits.isFromPrefetch))))
  XSPerfAccumulate("memSetPattenDetected", memSetPattenDetected)
  XSPerfAccumulate("no_free_entry", !ParallelORR(Cat(entries.map(e => e.io.primary_ready))))
  XSPerfAccumulate("free_entry_less_reqNum", PopCount(entries.map(e => e.io.primary_ready)) < reqNum.U)
  if (hasDualChannel) {
    XSPerfAccumulate("dual_channle_acquire", io.mem_acquire.valid && io.mem_acquire_1.get.valid)
    XSPerfAccumulate("dual_channle_grant", io.mem_grant.valid && io.mem_grant_1.get.valid)
    XSPerfAccumulate("dual_channle_grantAck", io.mem_finish.valid && io.mem_finish_1.get.valid)
  }
  val max_inflight = RegInit(0.U((log2Up(cfg.nMissEntries) + 1).W))
  val num_valids = PopCount(~Cat(primary_ready_vec).asUInt)
  when (num_valids > max_inflight) {
    max_inflight := num_valids
  }
  // max inflight (average) = max_inflight_total / cycle cnt
  XSPerfAccumulate("max_inflight", max_inflight)
  QueuePerf(cfg.nMissEntries, num_valids, num_valids === cfg.nMissEntries.U)
  io.full := num_valids === cfg.nMissEntries.U
  io.l1Miss := RegNext(Cat(entries.map(_.io.l1Miss)).orR)
  XSPerfHistogram("num_valids", num_valids, true.B, 0, cfg.nMissEntries, 1)

  XSPerfHistogram("L1DMLP_CPUData", PopCount(VecInit(entries.map(_.io.perf_pending_normal)).asUInt), true.B, 0, cfg.nMissEntries, 1)
  XSPerfHistogram("L1DMLP_Prefetch", PopCount(VecInit(entries.map(_.io.perf_pending_prefetch)).asUInt), true.B, 0, cfg.nMissEntries, 1)
  XSPerfHistogram("L1DMLP_Total", num_valids, true.B, 0, cfg.nMissEntries, 1)

  XSPerfAccumulate("miss_load_refill_latency", PopCount(entries.map(_.io.latency_monitor.load_miss_refilling)))
  XSPerfAccumulate("miss_store_refill_latency", PopCount(entries.map(_.io.latency_monitor.store_miss_refilling)))
  XSPerfAccumulate("miss_amo_refill_latency", PopCount(entries.map(_.io.latency_monitor.amo_miss_refilling)))
  XSPerfAccumulate("miss_pf_refill_latency", PopCount(entries.map(_.io.latency_monitor.pf_miss_refilling)))

  val rob_head_miss_in_dcache = VecInit(entries.map(_.io.rob_head_query.resp)).asUInt.orR

  entries.foreach {
    case e => {
      e.io.rob_head_query.query_valid := io.debugTopDown.robHeadVaddr.valid
      e.io.rob_head_query.vaddr := io.debugTopDown.robHeadVaddr.bits
    }
  }

  io.debugTopDown.robHeadMissInDCache := rob_head_miss_in_dcache

  val perfValidCount = RegNext(PopCount(entries.map(entry => (!entry.io.primary_ready))))
  val query_fire_next = RegNext(query_fire)
  val perfEvents = Seq(
    ("dcache_missq_req      ", PopCount(query_fire_next)),
    ("dcache_missq_1_4_valid", (perfValidCount < (cfg.nMissEntries.U/4.U))),
    ("dcache_missq_2_4_valid", (perfValidCount > (cfg.nMissEntries.U/4.U)) & (perfValidCount <= (cfg.nMissEntries.U/2.U))),
    ("dcache_missq_3_4_valid", (perfValidCount > (cfg.nMissEntries.U/2.U)) & (perfValidCount <= (cfg.nMissEntries.U*3.U/4.U))),
    ("dcache_missq_4_4_valid", (perfValidCount > (cfg.nMissEntries.U*3.U/4.U))),
  )
  generatePerfEvent()
}
