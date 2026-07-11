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
  val pfSource = UInt(L1PfSourceBits.W)
  val cmd = UInt(M_SZ.W)
  val addr = UInt(PAddrBits.W)
  val vaddr = UInt(VAddrBits.W)
  val pc = UInt(VAddrBits.W)

  val lqIdx = new LqPtr
  // store
  val fullOverwrite = Bool()

  // amo
  val wordIdx = UInt(log2Up(blockWords).W)
  val amoData   = UInt(QuadWordBits.W)
  val amoMask   = UInt(QuadWordBytes.W)
  val amoCmp    = UInt(QuadWordBits.W) // data to be compared in AMOCAS

  val reqCoh = new ClientMetadata
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
  val occupyWay = UInt(nWays.W)

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
  def hit = reqCoh.isValid()
}

class MissReqStoreData(implicit p: Parameters) extends DCacheBundle {
  // store data and store mask will be written to miss queue entry
  // 1 cycle after req.fire() and meta write
  val storeData = UInt((cfg.blockBytes * 8).W)
  val storeMask = UInt(cfg.blockBytes.W)
}

class MissQueueRefillInfo(implicit p: Parameters) extends MissReqStoreData {
  // refill_info for mainpipe req awake
  val missParam = UInt(TLPermissions.bdWidth.W)
  val missDirty = Bool()
  val error      = new TLError()
  val refillLatency = UInt(LATENCY_WIDTH.W)
}

class MissReq(implicit p: Parameters) extends MissReqWoStoreData {
  // store data and store mask will be written to miss queue entry
  // 1 cycle after req.fire() and meta write
  val storeData = UInt((cfg.blockBytes * 8).W)
  val storeMask = UInt(cfg.blockBytes.W)

  def toMissReqStoreData(): MissReqStoreData = {
    val out = Wire(new MissReqStoreData)
    out.storeData := storeData
    out.storeMask := storeMask
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
  val blockMatch = Bool()
  val aliasMatch = Bool()
  val setMatch   = Bool()
  val mergeLoad  = Bool()
  val mergeStore = Bool()
}

trait HasMissReqFunction extends HasDCacheParameters
 with HasL1CacheParameters {
  // implicit val p: Parameters

  def blockMatch(req: MissReqWoStoreData, new_req: MissReqWoStoreData): Bool = {
    get_block(req.addr) === get_block(new_req.addr)
  }

  def aliasMatch(req: MissReqWoStoreData, new_req: MissReqWoStoreData): Bool = {
    isAliasMatch(req.vaddr, new_req.vaddr)
  }

  def setMatch(req: MissReqWoStoreData, new_req: MissReqWoStoreData): Bool = {
    addrToDcacheSet(req.vaddr) === addrToDcacheSet(new_req.vaddr)
  }

  // Passing in "req" is to accommodate two different scenarios: MissEntry and MissQueue
  def computeMatchSignals(req: MissReqWoStoreData, new_req: MissReqWoStoreData): MatchSignals = {
    val signals = Wire(new MatchSignals)
    signals.blockMatch := blockMatch(req, new_req)
    signals.aliasMatch := aliasMatch(req, new_req)
    signals.setMatch   := setMatch(req, new_req)
    signals.mergeLoad  := (req.isFromLoad || req.isFromStore || req.isFromPrefetch) && new_req.isFromLoad
    signals.mergeStore := (req.isFromLoad || req.isFromPrefetch) && new_req.isFromStore
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

  def hasValid(): Bool = valid.asUInt.orR
  def validCount(): UInt = PopCount(valid)
}

// Analysis result for each request in cycle 0
class ReqAnalysisResult(nReq: Int, nMissEntries: Int)(implicit p: Parameters) extends Bundle {
  // Strategy for each request: each bit represents a strategy
  // bit 0: allocate, bit 1: merge, bit 2: compress
  // Multiple bits can be set to indicate multiple applicable strategies
  val strategy = Vec(nReq, UInt(3.W))

  // Target MSHR index (for allocate and merge)
  val targetMshr = Vec(nReq, UInt(log2Up(nMissEntries).W))

  // Compression group ID (for compress strategy)
  val compressGroup = Vec(nReq, UInt(log2Up(nReq).W))

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
  val mshrId       = UInt(log2Up(cfg.nMissEntries).W)

  def regValid(): Bool = {
    (merge || alloc)
  }

  def matched(signals: MatchSignals): Bool = {
    signals.blockMatch && regValid()
  }

  def prefetchLateEn(signals: MatchSignals, new_req: MissReqWoStoreData, new_req_valid: Bool): Bool = {
    new_req_valid && alloc && signals.blockMatch && req.isFromPrefetch && !new_req.isFromPrefetch
  }

  def rejectReq(signals: MatchSignals): Bool = {
    Mux(
        alloc,
        signals.blockMatch && (!signals.aliasMatch || !(signals.mergeLoad || signals.mergeStore)),
        false.B
      )
  }

  def mergeReq(signals: MatchSignals): Bool = {
    Mux(
        alloc,
        signals.blockMatch && signals.aliasMatch && (signals.mergeLoad || signals.mergeStore),
        false.B
      )
  }

  def mergeIsKeyword(signals: MatchSignals, new_req: MissReq): Bool = {
    val loadMergeLoad  = mergeReq(signals) && req.isFromLoad  && new_req.isFromLoad
    val storeMergeLoad = mergeReq(signals) && req.isFromStore && new_req.isFromLoad
    val loadMergeLoadUseNewReqIsKeyword = isAfter(req.lqIdx, new_req.lqIdx)
    val useNewReqIsKeyword = (loadMergeLoad && loadMergeLoadUseNewReqIsKeyword) || storeMergeLoad
    Mux (
      useNewReqIsKeyword,
        new_req.vaddr(5).asBool,
        req.vaddr(5).asBool
      )
  }

  def isKeyword(): Bool= {
    alloc && req.isFromLoad && req.vaddr(5).asBool
  }
  // send out acquire as soon as possible
  // if a new store miss req is about to merge into this pipe reg, don't send acquire now
  def canSendAcquire(signals: MatchSignals, valid: Bool, new_req: MissReq): Bool = {
    alloc && !(valid && mergeReq(signals) && new_req.isFromStore)
  }

  def getAcquire(l2PfStoreOnly: Bool): TLBundleA = {
    val acquire = Wire(new TLBundleA(edge.bundle))
    val growParam = req.reqCoh.onAccess(req.cmd)._2
    val acquireBlock = edge.AcquireBlock(
      fromSource = mshrId,
      toAddress = get_block_addr(req.addr),
      lgSize = (log2Up(cfg.blockBytes)).U,
      growPermissions = growParam
    )._2
    val acquirePerm = edge.AcquirePerm(
      fromSource = mshrId,
      toAddress = get_block_addr(req.addr),
      lgSize = (log2Up(cfg.blockBytes)).U,
      growPermissions = growParam
    )._2
    acquire := Mux(req.fullOverwrite, acquirePerm, acquireBlock)
    // resolve cache alias by L2
    acquire.user.lift(AliasKey).foreach(_ :=  getAlias(req.vaddr))
    // pass vaddr to l2
    acquire.user.lift(VaddrKey).foreach(_ := req.vaddr(VAddrBits - 1, blockOffBits))
    // pass pc to l2
    acquire.user.lift(PCKey).foreach(_ := req.pc) 

    // miss req pipe reg pass keyword to L2, is priority
    // acquire.echo.lift(IsKeywordKey).foreach(_ := isKeyword())
    acquire.echo.lift(IsKeywordKey).foreach(_ := false.B)

    // trigger prefetch
    acquire.user.lift(PrefetchKey).foreach(_ := Mux(l2PfStoreOnly, req.isFromStore, true.B))
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

  def blockAndAliasMatch(releaseReq: MissQueueBlockReqBundle): Bool = {
    regValid() && get_block(req.addr) === get_block(releaseReq.addr) && isAliasMatch(req.vaddr, releaseReq.vaddr)
  }

  def evictSetMatch(evictSet: UInt): Bool = {
    regValid() && req.isBtoT && addrToDcacheSet(req.vaddr) === evictSet
  }
}

class CMOUnit(edge: TLEdgeOut)(implicit p: Parameters) extends DCacheModule {
  val io = IO(new Bundle() {
    val req = Flipped(DecoupledIO(new CMOReq))
    val reqChanA = DecoupledIO(new TLBundleA(edge.bundle))
    val respChanD = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))
    val respToLsq = DecoupledIO(new CMOResp)
    val wfi = Flipped(new WfiReqBundle)
  })

  val sIdle :: sSreq :: sWresp :: sLsqResp :: Nil = Enum(4)
  val state = RegInit(sIdle)
  val stateNext = WireInit(state)
  val req = RegEnable(io.req.bits, io.req.fire)
  val nderr = RegInit(false.B)
  val denied = RegInit(false.B)
  val corrupt = RegInit(false.B)
  val noPending = RegInit(true.B)

  state := stateNext

  switch (state) {
    is(sIdle) {
      when (io.req.fire) {
        stateNext := sSreq
        nderr := false.B
        denied := false.B
        corrupt := false.B
      }
    }
    is(sSreq) {
      when (io.reqChanA.fire) {
        stateNext := sWresp
        noPending := false.B
      }
    }
    is(sWresp) {
      when (io.respChanD.fire) {
        stateNext := sLsqResp
        nderr := io.respChanD.bits.denied || io.respChanD.bits.corrupt
        denied := io.respChanD.bits.denied
        corrupt := io.respChanD.bits.corrupt
        noPending := true.B
      }
    }
    is(sLsqResp) {
      when (io.respToLsq.fire) {
        stateNext := sIdle
      }
    }
  }

  io.req.ready := state === sIdle

  io.reqChanA.valid := state === sSreq && !io.wfi.wfiReq
  io.reqChanA.bits := edge.CacheBlockOperation(
    fromSource = (cfg.nMissEntries + 1).U,
    toAddress = req.address,
    lgSize = (log2Up(cfg.blockBytes)).U,
    opcode = req.opcode
  )._2

  io.respChanD.ready := state === sWresp
  io.wfi.wfiSafe := GatedValidRegNext(noPending && io.wfi.wfiReq)

  io.respToLsq.valid := state === sLsqResp
  io.respToLsq.bits.address := req.address
  io.respToLsq.bits.nderr   := nderr
  io.respToLsq.bits.denied  := denied
  io.respToLsq.bits.corrupt := corrupt

  assert(!(state =/= sIdle && io.req.valid))
  assert(!(state =/= sWresp && io.respChanD.valid))
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
    val wbqBlockMissReq = Input(Vec(reqNum, Bool()))
    // pipeline reg
    val missReqPipeReg = Input(new MissReqPipeRegBundle(edge))
    // allocate this entry for new req
    val entryValid = Input(Bool())
    // this entry is free and can be allocated to new reqs
    val primaryReady = Output(Bool())
    // this entry is busy, but it can merge the new req
    // Changed to Vec to support parallel enqueue: each queryMQ request gets independent judgment
    val secondaryReady = Output(Vec(reqNum, Bool()))
    // this entry is busy and it can not merge the new req
    // Changed to Vec to support parallel enqueue: each queryMQ request gets independent judgment
    val secondaryReject = Output(Vec(reqNum, Bool()))
    // way selected for replacing, used to support plru update
    // bus
    val memAcquire = DecoupledIO(new TLBundleA(edge.bundle))
    val memGrant = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))
    val memFinish = DecoupledIO(new TLBundleE(edge.bundle))

    // client requests, queryME receive all miss_req now
    val queryME = Vec(reqNum, Flipped(new DCacheMEQueryIOBundle))

    // output the signals to avoid redundant computation
    // val match_signals = Output(new MatchSignals)
    val matchSignalsVec = Vec(reqNum, Output(new MatchSignals))

    // send refill info to load queue, useless now
    val refillToLdq = ValidIO(new Refill)

    // replace pipe
    val l2Hint = Input(Valid(new L2ToL1Hint())) // Hint from L2 Cache

    // main pipe: amo miss
    val mainPipeReq = DecoupledIO(new MainPipeReq)
    val mainPipeResp = Input(Bool())
    val mainPipeRefillResp = Input(Bool())
    val mainPipeReplay = Input(Bool())
    val mainPipeEvictBtoTWay = Input(Bool())
    val mainPipeNextEvictWay = Input(UInt(nWays.W))

    // for main pipe s2
    val refillInfo = ValidIO(new MissQueueRefillInfo)
    val refillTrain = ValidIO(new TrainReqBundle)

    val occupyWay = Output(UInt(nWays.W))

    // block probe
    val probe = Flipped(new MissQueueBlockIO)

    // block replace when release an addr valid in mshr
    val replace = Flipped(new MissQueueBlockIO)

    val reqAddr = ValidIO(UInt(PAddrBits.W))
    val reqVaddr = ValidIO(UInt(VAddrBits.W))
    val reqIsBtoT = Output(Bool())

    val reqHasStore = Output(Bool())

    val reqHandledByThisEntry = Output(Bool())

    val forwardInfo = Output(new MissEntryForwardIO)
    val l2PfStoreOnly = Input(Bool())

    // whether the pipeline reg has send out an acquire
    val acquireFiredByPipeReg = Input(Bool())
    val memSetPattenDetected = Input(Bool())

    val perfPendingPrefetch = Output(Bool())
    val perfPendingNormal   = Output(Bool())

    val robHeadQuery = new DCacheBundle {
      val vaddr = Input(UInt(VAddrBits.W))
      val queryValid = Input(Bool())

      val resp = Output(Bool())

      def hit(e_vaddr: UInt): Bool = {
        require(e_vaddr.getWidth == VAddrBits)
        queryValid && vaddr(VAddrBits - 1, DCacheLineOffset) === e_vaddr(VAddrBits - 1, DCacheLineOffset)
      }
    }

    val latencyMonitor = new DCacheBundle {
      val loadMissRefilling  = Output(Bool())
      val storeMissRefilling = Output(Bool())
      val amoMissRefilling   = Output(Bool())
      val pfMissRefilling    = Output(Bool())
    }

    val prefetchInfo = new DCacheBundle {
      val hitPrefetch = Vec(reqNum, Output(Bool()))
      val hitPfSource = UInt(L1PfSourceBits.W)
    }
    val nMaxPrefetchEntry = Input(UInt(64.W))
    val matched = Output(Bool())
    val l1Miss = Output(Bool())

    val wfi = Flipped(new WfiReqBundle)
  })

  val req = Reg(new MissReqWoStoreData)
  val reqPrimaryFire = Reg(new MissReqWoStoreData) // for perf use
  val reqStoreMask = Reg(UInt(cfg.blockBytes.W))
  val reqValid = RegInit(false.B)
  val set = addrToDcacheSet(req.vaddr)
  val evictBtoTWay = RegInit(false.B)
  val allocIsStore = RegInit(false.B)  // The alloc (first) req is a store req
  val hasStore = RegInit(false.B)
  // initial keyword
  val isKeyword = RegInit(false.B)

  val missReqPipeRegBits = io.missReqPipeReg.req

  val signalsVec = WireInit(VecInit(Seq.fill(reqNum)(0.U.asTypeOf(new MatchSignals))))
  val signalsPipePrefetch = computeMatchSignals(missReqPipeRegBits, io.queryME(0).req.bits)

  for(i <- 0 until reqNum) {
    signalsVec(i) := computeMatchSignals(req, io.queryME(i).req.bits)
  }

  val inputReqIsPrefetch = isPrefetch(missReqPipeRegBits.cmd)

  val sAcquire = RegInit(true.B)
  val sGrantack = RegInit(true.B)
  val sMainpipeReq = RegInit(true.B)

  val wGrantfirst = RegInit(true.B)
  val wGrantlast = RegInit(true.B)
  val wMainpipeResp = RegInit(true.B)
  val wRefillResp = RegInit(true.B)
  val wL2hint = RegInit(true.B)

  val noPending = RegInit(true.B)

  val mainpipeReqFired = RegInit(true.B)

  val releaseEntry = sGrantack && wMainpipeResp && wRefillResp

  val acquireNotSent = !sAcquire && !io.memAcquire.ready
  val dataNotRefilled = !wGrantfirst

  val error = RegInit(false.B)
  val denied = RegInit(false.B)
  val corrupt = RegInit(false.B)
  val prefetch = RegInit(false.B)
  val access = RegInit(false.B)

  val shouldRefillDataReg =  Reg(Bool())
  val shouldRefillData = WireInit(shouldRefillDataReg)

  val shouldReplace = RegInit(false.B)

  val fullOverwrite = Reg(Bool())

  val (_, _, refillDone, refill_count) = edge.count(io.memGrant)
  val grantParam = Reg(UInt(TLPermissions.bdWidth.W))

  // refill data with store data, this reg will be used to store:
  // 1. store data (if needed), before l2 refill data
  // 2. store data and l2 refill data merged result (i.e. new cacheline taht will be write to data array)
  val refillAndStoreData = Reg(Vec(blockRows, UInt(rowBits.W)))
  // raw data refilled to l1 by l2
  val refillDataRaw = Reg(Vec(blockBytes/beatBytes, UInt(beatBits.W)))

  val refillStartTime = Reg(UInt(64.W))
  val refillLatency = Reg(UInt(LATENCY_WIDTH.W))

  // allocate current miss queue entry for a miss req
  // Use queryME instead of io.req for parallel enqueue
  val primaryFireVec = (0 until reqNum).map{ i =>
    io.entryValid && io.queryME(i).req.valid && io.primaryReady && !io.queryME(i).req.bits.cancel && !io.wbqBlockMissReq(i)
  }
  val primaryFire = ParallelORR(Cat(primaryFireVec))

  val primaryAcceptVec = (0 until reqNum).map{ i =>
    io.queryME(i).req.valid && io.primaryReady && !io.queryME(i).req.bits.cancel
  }
  val primaryAccept = ParallelORR(Cat(primaryAcceptVec))

  // merge miss req to current miss queue entry
  // Check if ANY request port can merge
  val secondaryReadyAny = ParallelORR(Cat(io.secondaryReady))
  val secondaryRejectAny = ParallelORR(Cat(io.secondaryReject))

  // For backward compatibility with io.req (single-port path)
  // Note: io.req.path is deprecated, use queryME instead
  val secondaryFireVec = (0 until reqNum).map{ i =>
    io.queryME(i).req.valid && io.secondaryReady(i) && !io.queryME(i).req.bits.cancel && !io.wbqBlockMissReq(i)
  }
  val secondaryFire = ParallelORR(Cat(secondaryFireVec))

  val secondaryAcceptVec = (0 until reqNum).map{ i =>
    io.queryME(i).req.valid && io.secondaryReady(i) && !io.queryME(i).req.bits.cancel
  }
  val secondaryAccept = ParallelORR(Cat(secondaryAcceptVec))

  val reqHandledByThisEntry = primaryAccept || secondaryAccept

  for(i <- 0 until reqNum) {
    io.matchSignalsVec(i).blockMatch := signalsVec(i).blockMatch && reqValid
    io.matchSignalsVec(i).aliasMatch := signalsVec(i).aliasMatch && reqValid
    io.matchSignalsVec(i).setMatch := signalsVec(i).setMatch && reqValid
    io.matchSignalsVec(i).mergeLoad := signalsVec(i).mergeLoad && reqValid
    io.matchSignalsVec(i).mergeStore := signalsVec(i).mergeStore && reqValid
  }

  // for perf use
  val secondaryFired = RegInit(false.B)

  io.perfPendingPrefetch := reqValid && prefetch && !secondaryFired
  io.perfPendingNormal   := reqValid && (!prefetch || secondaryFired)

  io.robHeadQuery.resp   := io.robHeadQuery.hit(req.vaddr) && reqValid

  io.reqHandledByThisEntry := reqHandledByThisEntry

  when (releaseEntry && reqValid) {
    reqValid := false.B
  }

  when (io.missReqPipeReg.alloc && !io.missReqPipeReg.cancel) {
    assert(RegNext(primaryFire), p"after 1 cycle of primary_fire, entry will be allocated:${io.id}")
    reqValid := true.B

    req := missReqPipeRegBits.toMissReqWoStoreData()
    req.isBtoT := missReqPipeRegBits.isBtoT
    req.occupyWay := missReqPipeRegBits.occupyWay
    req.addr := get_block_addr(missReqPipeRegBits.addr)
    reqPrimaryFire := missReqPipeRegBits.toMissReqWoStoreData()
    evictBtoTWay := false.B
    allocIsStore := missReqPipeRegBits.isFromStore
    hasStore := missReqPipeRegBits.isFromStore
    // remove isKeyword logic
    isKeyword := false.B

    sAcquire := io.acquireFiredByPipeReg
    sGrantack := false.B
    sMainpipeReq := false.B

    wGrantfirst := false.B
    wGrantlast := false.B
    wL2hint := false.B
    mainpipeReqFired := false.B

    noPending := !io.acquireFiredByPipeReg

    reqStoreMask := Mux(missReqPipeRegBits.isFromStore, missReqPipeRegBits.storeMask, 0.U)

    fullOverwrite := missReqPipeRegBits.isFromStore && missReqPipeRegBits.fullOverwrite

    when (!missReqPipeRegBits.isFromAMO) {
      wRefillResp := false.B
    }

    when (missReqPipeRegBits.isFromAMO) {
      wMainpipeResp := false.B
    }

    shouldRefillDataReg := missReqPipeRegBits.isFromLoad

    error := false.B
    denied := false.B
    corrupt := false.B
    prefetch := inputReqIsPrefetch && !io.missReqPipeReg.prefetchLateEn(signalsPipePrefetch, io.queryME(0).req.bits, io.queryME(0).req.valid)
    access := false.B
    secondaryFired := false.B

    refillStartTime := GTimer()
  }

  // Wire to hold updated data that has merged grant data (on grant.fire)
  val refillAndStoreDataUpdate = Wire(Vec(blockRows, UInt(rowBits.W)))
  // All the logic of refill_and_store_data. There is a corner case to note: store-merge and grant.fire may happen at the same cycle.
  when (io.missReqPipeReg.alloc && !io.missReqPipeReg.cancel && missReqPipeRegBits.isFromStore) {
    refillAndStoreData := VecInit(missReqPipeRegBits.storeData.grouped(rowBits))
  }.elsewhen (io.missReqPipeReg.merge && !io.missReqPipeReg.cancel && missReqPipeRegBits.isFromStore) {
    for (i <- 0 until blockRows) {
      val storeMaskTemp = missReqPipeRegBits.storeMask.grouped(rowBytes)(i).asBools
      val storeDataTemp = (missReqPipeRegBits.storeData.grouped(8).grouped(rowBytes).toSeq)(i)
      refillAndStoreData(i) := VecInit((0 until rowBytes).map(k =>
        Mux(storeMaskTemp(k), storeDataTemp(k), refillAndStoreDataUpdate(i).grouped(8)(k)))).asUInt
    }
  }.elsewhen (io.memGrant.fire) {
    refillAndStoreData := refillAndStoreDataUpdate
  }

  when (io.missReqPipeReg.merge && !io.missReqPipeReg.cancel) {
    assert(RegNext(secondaryFire) || RegNext(RegNext(primaryFire)), p"after 1 cycle of secondary_fire or 2 cycle of primary_fire, entry will be merged:${io.id}")
    assert(missReqPipeRegBits.reqCoh.state <= req.reqCoh.state || (prefetch && !access))
    assert(!(missReqPipeRegBits.isFromAMO || req.isFromAMO))
    // use the most uptodate meta
    req.reqCoh := missReqPipeRegBits.reqCoh

    isKeyword := false.B
    assert(!missReqPipeRegBits.isFromPrefetch, "can not merge a prefetch req, late prefetch should always be ignored!")

    when (missReqPipeRegBits.isFromStore) {
      req := missReqPipeRegBits
      req.isBtoT := missReqPipeRegBits.isBtoT
      req.occupyWay := missReqPipeRegBits.occupyWay
      evictBtoTWay := false.B
      req.addr := get_block_addr(missReqPipeRegBits.addr)
      reqStoreMask := reqStoreMask | missReqPipeRegBits.storeMask
      hasStore := true.B
      fullOverwrite := fullOverwrite || missReqPipeRegBits.fullOverwrite
      assert(isAliasMatch(req.vaddr, missReqPipeRegBits.vaddr), "alias bits should be the same when merging store")
    }

    shouldRefillData := shouldRefillDataReg || missReqPipeRegBits.isFromLoad
    shouldRefillDataReg := shouldRefillData
    when (!inputReqIsPrefetch) {
      access := true.B // when merge non-prefetch req, set access bit
    }
    secondaryFired := true.B
  }

  when (io.memAcquire.fire) {
    sAcquire := true.B
    noPending := false.B
  }

  // merge refilled data and store data (if needed)
  def mergePutData(oldData: UInt, newData: UInt, wmask: UInt): UInt = {
    val fullWmask = FillInterleaved(8, wmask)
    (~fullWmask & oldData | fullWmask & newData)
  }
  val newMask = VecInit(reqStoreMask.grouped(rowBytes))
  val grantDataGrouped = io.memGrant.bits.data.grouped(rowBits)
  val lowHalfRefill = refill_count === 0.U && !isKeyword || refill_count =/= 0.U && isKeyword
  //---- refill_and_store_data_update: see definition above ----
  require(blockRows == 2 * beatRows, "refill_and_store_data_update: so far, only works for blockRows == 2 * beatRows")
  for (i <- 0 until beatRows) {
    refillAndStoreDataUpdate(i) := Mux(
      io.memGrant.fire && edge.hasData(io.memGrant.bits) && lowHalfRefill,
      mergePutData(grantDataGrouped(i), refillAndStoreData(i), newMask(i)),
      refillAndStoreData(i)
    )
    refillAndStoreDataUpdate(i + beatRows) := Mux(
      io.memGrant.fire && edge.hasData(io.memGrant.bits) && !lowHalfRefill,
      mergePutData(grantDataGrouped(i), refillAndStoreData(i + beatRows), newMask(i + beatRows)),
      refillAndStoreData(i + beatRows)
    )
  }

  val hasData = RegInit(true.B)
  val isDirty = RegInit(false.B)
  io.wfi.wfiSafe := GatedValidRegNext(noPending && io.wfi.wfiReq)

  when (io.memGrant.fire) {
    wGrantfirst := true.B
    grantParam := io.memGrant.bits.param
    when (edge.hasData(io.memGrant.bits)) {
      wGrantlast := wGrantlast || refillDone
      noPending := noPending || refillDone
      hasData := true.B
    }.otherwise {
      // Grant
      assert(fullOverwrite)
      wGrantlast := true.B
      noPending := true.B
      hasData := false.B
    }

    error := io.memGrant.bits.denied || io.memGrant.bits.corrupt || error
    denied := denied || io.memGrant.bits.denied
    corrupt := corrupt || io.memGrant.bits.corrupt

    refillDataRaw(refill_count ^ isKeyword) := io.memGrant.bits.data
    isDirty := io.memGrant.bits.echo.lift(DirtyKey).getOrElse(false.B)
    when(refillDone) {
      val refillEndTime = GTimer()
      val timeDelta = refillEndTime - refillStartTime
      val overflow = refillEndTime < refillStartTime || (timeDelta >> LATENCY_WIDTH).orR
      refillLatency := Mux(overflow, 0.U, timeDelta)
    }
  }

  when (io.memFinish.fire) {
    sGrantack := true.B
  }

  when (io.mainPipeReq.fire) {
    sMainpipeReq := true.B
    mainpipeReqFired := true.B
  }

  when (io.mainPipeReplay || io.mainPipeEvictBtoTWay) {
    sMainpipeReq := false.B
  }
  when (io.mainPipeReplay) {
    evictBtoTWay := false.B
  } .elsewhen (io.mainPipeEvictBtoTWay) {
    evictBtoTWay := true.B
    req.occupyWay := io.mainPipeNextEvictWay
  }

  when (io.mainPipeResp) {
    wMainpipeResp := true.B
  }

  when(io.mainPipeRefillResp) {
    wRefillResp := true.B
  }

  when (io.l2Hint.valid) {
    wL2hint := true.B
  }

  def beforeReqSentCanMerge(new_req: MissReqWoStoreData): Bool = {
    // acquire_not_sent && (new_req.isFromLoad || new_req.isFromStore)

    // Since most acquire requests have been issued from pipe_reg,
    // the number of such merge situations is currently small,
    // So dont Merge anything for better timing.
    false.B
  }

  def beforeDataRefillCanMerge(new_req: MissReqWoStoreData): Bool = {
    dataNotRefilled && new_req.isFromLoad ||
    !io.mainPipeRefillResp && !wRefillResp && new_req.isFromStore && allocIsStore
  }

  // Note that late prefetch will be ignored

  def shouldMerge(signals: MatchSignals, new_req: MissReqWoStoreData): Bool = {
    signals.blockMatch && signals.aliasMatch &&
    (
      beforeReqSentCanMerge(new_req) ||
      beforeDataRefillCanMerge(new_req)
    )
  }

  def beforeReqSentMergeIskeyword(signals: MatchSignals, new_req: MissReqWoStoreData): Bool = {
    val needCheckIsKeyword = acquireNotSent && req.isFromLoad && new_req.isFromLoad && shouldMerge(signals, new_req)
    val useNewReqIsKeyword = isAfter(req.lqIdx, new_req.lqIdx)
    Mux(
      needCheckIsKeyword,
      Mux(
        useNewReqIsKeyword,
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
  def shouldReject(signals: MatchSignals, new_req: MissReqWoStoreData): Bool = {
    reqValid && Mux(
        signals.blockMatch,
        (!beforeReqSentCanMerge(new_req) && !beforeDataRefillCanMerge(new_req)) || !signals.aliasMatch,
        false.B
      )
  }

  // req_valid will be updated 1 cycle after primary_fire, so next cycle, this entry cannot accept a new req
  when(GatedValidRegNext(io.id >= ((cfg.nMissEntries).U - io.nMaxPrefetchEntry))) {
    // can accept prefetch req
    io.primaryReady := !reqValid && !GatedValidRegNext(primaryFire)
  }.otherwise {
    // cannot accept prefetch req except when a memset patten is detected
    // prefetch only in mainpipe, now
    io.primaryReady := !reqValid && (!(io.queryME(0).req.valid && io.queryME(0).req.bits.isFromPrefetch) || io.memSetPattenDetected) && !GatedValidRegNext(primaryFire)
  }

  // Generate vectorized secondary_ready and secondary_reject for parallel enqueue
  // Each queryMQ request gets independent judgment
  for (i <- 0 until reqNum) {
    val _signals = computeMatchSignals(req, io.queryME(i).req.bits)
    io.secondaryReady(i) := shouldMerge(_signals, io.queryME(i).req.bits) && !io.missReqPipeReg.cancel
    io.secondaryReject(i) := shouldReject(_signals, io.queryME(i).req.bits)
  }

  // For backward compatibility with io.req (single-port path)
  // Use queryME(0) to maintain compatibility
  // Note: io.secondary_ready(0) is already set by the loop above

  // generate primary_ready & secondary_(ready | reject) for each miss request
  for (i <- 0 until reqNum) {
    when(GatedValidRegNext(io.id >= ((cfg.nMissEntries).U - io.nMaxPrefetchEntry))) {
      io.queryME(i).primaryReady := !reqValid && !GatedValidRegNext(primaryFire)
    }.otherwise {
      io.queryME(i).primaryReady := !reqValid && !GatedValidRegNext(primaryFire) &&
                                    (!io.queryME(i).req.bits.isFromPrefetch || io.memSetPattenDetected)
    }
    val _signals = computeMatchSignals(req, io.queryME(i).req.bits)
    io.queryME(i).secondaryReady  := shouldMerge(_signals, io.queryME(i).req.bits)
    io.queryME(i).secondaryReject := shouldReject(_signals, io.queryME(i).req.bits)
    io.queryME(i).blockMatch := _signals.blockMatch && reqValid
  }

  // should not allocate, merge or reject at the same time
for(i <- 0 until reqNum) {
    assert(RegNext(PopCount(Seq(io.primaryReady, io.queryME(i).secondaryReady, io.queryME(i).secondaryReject)) <= 1.U || !io.queryME(i).req.valid))
  }

  val refillDataSplited = WireInit(VecInit(Seq.tabulate(cfg.blockBytes * 8 / l1BusDataWidth)(i => {
    val data = refillAndStoreData.asUInt
    data((i + 1) * l1BusDataWidth - 1, i * l1BusDataWidth)
  })))
  // when granted data is all ready, wakeup lq's miss load
  val refillToLdqEn = !wGrantlast && io.memGrant.fire
  io.refillToLdq.valid := GatedValidRegNext(refillToLdqEn)
  io.refillToLdq.bits.addr := RegEnable(req.addr + ((refill_count ^ isKeyword) << refillOffBits), refillToLdqEn)
  io.refillToLdq.bits.data := refillDataSplited(RegEnable(refill_count ^ isKeyword, refillToLdqEn))
  io.refillToLdq.bits.error := RegEnable(io.memGrant.bits.corrupt || io.memGrant.bits.denied, refillToLdqEn)
  io.refillToLdq.bits.refillDone := RegEnable(refillDone && io.memGrant.fire, refillToLdqEn)
  io.refillToLdq.bits.hasdata := hasData
  io.refillToLdq.bits.dataRaw := refillDataRaw.asUInt
  io.refillToLdq.bits.id := io.id

  // if the entry has a pending merge req, wait for it
  // Note: now, only wait for store, because store may acquire T
  io.memAcquire.valid := !sAcquire &&
    !(io.missReqPipeReg.merge && !io.missReqPipeReg.cancel && missReqPipeRegBits.isFromStore) &&
    !io.wfi.wfiReq
  val growParam = req.reqCoh.onAccess(req.cmd)._2
  val acquireBlock = edge.AcquireBlock(
    fromSource = io.id,
    toAddress = req.addr,
    lgSize = (log2Up(cfg.blockBytes)).U,
    growPermissions = growParam
  )._2
  val acquirePerm = edge.AcquirePerm(
    fromSource = io.id,
    toAddress = req.addr,
    lgSize = (log2Up(cfg.blockBytes)).U,
    growPermissions = growParam
  )._2
  io.memAcquire.bits := Mux(fullOverwrite, acquirePerm, acquireBlock)
  // resolve cache alias by L2
  io.memAcquire.bits.user.lift(AliasKey).foreach( _ := getAlias(req.vaddr))
  // pass vaddr to l2
  io.memAcquire.bits.user.lift(VaddrKey).foreach( _ := req.vaddr(VAddrBits-1, blockOffBits))
  // pass pc to l2
  io.memAcquire.bits.user.lift(PCKey).foreach(_ := req.pc)
  // pass keyword to L2
  // io.mem_acquire.bits.echo.lift(IsKeywordKey).foreach(_ := isKeyword)
  io.memAcquire.bits.echo.lift(IsKeywordKey).foreach(_ := false.B)
  // trigger prefetch
  io.memAcquire.bits.user.lift(PrefetchKey).foreach(_ := Mux(io.l2PfStoreOnly, req.isFromStore, true.B))
  // req source
  when(prefetch && !secondaryFired) {
    io.memAcquire.bits.user.lift(ReqSourceKey).foreach(_ := MemReqSource.L1DataPrefetch.id.U)
  }.otherwise {
    when(req.isFromStore) {
      io.memAcquire.bits.user.lift(ReqSourceKey).foreach(_ := MemReqSource.CPUStoreData.id.U)
    }.elsewhen(req.isFromLoad) {
      io.memAcquire.bits.user.lift(ReqSourceKey).foreach(_ := MemReqSource.CPULoadData.id.U)
    }.elsewhen(req.isFromAMO) {
      io.memAcquire.bits.user.lift(ReqSourceKey).foreach(_ := MemReqSource.CPUAtomicData.id.U)
    }.otherwise {
      io.memAcquire.bits.user.lift(ReqSourceKey).foreach(_ := MemReqSource.L1DataPrefetch.id.U)
    }
  }
  io.memAcquire.bits.user.lift(MemBackTypeMM).foreach(_ := true.B)
  io.memAcquire.bits.user.lift(MemPageTypeNC).foreach(_ := false.B)
  require(nSets <= 256)

  // io.mem_grant.ready := !w_grantlast && s_acquire
  io.memGrant.ready := true.B
  assert(!(io.memGrant.valid && !(!wGrantlast && sAcquire)), p"dcache should always be ready for mem_grant now:${io.id}")

  val grantack = RegEnable(edge.GrantAck(io.memGrant.bits), io.memGrant.fire)
  assert(RegNext(!io.memGrant.fire || edge.isRequest(io.memGrant.bits)))
  io.memFinish.valid := !sGrantack && wGrantfirst
  io.memFinish.bits := grantack

  // Send mainpipe_req when receive hint from L2 or receive data without hint
  io.mainPipeReq.valid := !sMainpipeReq && (wL2hint || wGrantlast)
  io.mainPipeReq.bits := DontCare
  io.mainPipeReq.bits.miss := true.B
  io.mainPipeReq.bits.missId := io.id
  io.mainPipeReq.bits.probe := false.B
  io.mainPipeReq.bits.source := req.source
  io.mainPipeReq.bits.cmd := req.cmd
  io.mainPipeReq.bits.vaddr := req.vaddr
  io.mainPipeReq.bits.addr := req.addr
  io.mainPipeReq.bits.wordIdx := req.wordIdx
  io.mainPipeReq.bits.amoData := req.amoData
  io.mainPipeReq.bits.amoMask := req.amoMask
  io.mainPipeReq.bits.amoCmp  := req.amoCmp
  io.mainPipeReq.bits.id := req.id
  io.mainPipeReq.bits.pfSource := req.pfSource
  io.mainPipeReq.bits.access := access
  io.mainPipeReq.bits.occupyWay := req.occupyWay
  io.mainPipeReq.bits.missFailCauseEvictBtot := evictBtoTWay

  io.probe.block := reqValid && wGrantlast &&
    get_block_addr(req.addr) === get_block_addr(io.probe.req.bits.addr) &&
    isAliasMatch(req.vaddr, io.probe.req.bits.vaddr)

  io.replace.block := reqValid &&
    get_block_addr(req.addr) === get_block_addr(io.replace.req.bits.addr) &&
    isAliasMatch(req.vaddr, io.replace.req.bits.vaddr)

  io.reqAddr.valid := reqValid
  io.reqAddr.bits:= req.addr
  io.reqVaddr.valid := reqValid
  io.reqVaddr.bits := req.vaddr
  io.reqIsBtoT := req.isBtoT

  io.reqHasStore := hasStore && reqValid

  io.occupyWay := req.occupyWay

  io.refillInfo.valid := reqValid && wGrantlast
  io.refillInfo.bits.storeData := refillAndStoreData.asUInt
  io.refillInfo.bits.storeMask := ~0.U(blockBytes.W)
  io.refillInfo.bits.missParam := grantParam
  io.refillInfo.bits.missDirty := isDirty
  io.refillInfo.bits.error.tlDenied  := denied
  io.refillInfo.bits.error.tlCorrupt := corrupt
  io.refillInfo.bits.refillLatency := Mux(
    isFromL1Prefetch(req.pfSource),
    refillLatency,
    0.U
  )

  io.refillTrain.valid := reqValid && wGrantlast
  io.refillTrain.bits.pc := req.pc
  io.refillTrain.bits.paddr := req.addr
  io.refillTrain.bits.vaddr := req.vaddr
  io.refillTrain.bits.miss := true.B
  // FIXME lyq: when mshr entry merges, req.pf_source may be cleaned.
  io.refillTrain.bits.metaSource := req.pfSource
  io.refillTrain.bits.refillLatency := refillLatency
  io.refillTrain.bits.robIdx := DontCare
  io.refillTrain.bits.isFirstIssue := DontCare
  io.refillTrain.bits.isHwPrefetch := DontCare

  XSPerfAccumulate("miss_refill_mainpipe_req", io.mainPipeReq.fire)
  XSPerfAccumulate("miss_refill_without_hint", io.mainPipeReq.fire && !mainpipeReqFired && !wL2hint)
  XSPerfAccumulate("miss_refill_replay", io.mainPipeReplay)
  XSPerfAccumulate("miss_refill_evict_BtoT_way", io.mainPipeEvictBtoTWay)

  val wGrantfirstForwardInfo = Mux(isKeyword, wGrantlast, wGrantfirst)
  val wGrantlastForwardInfo = Mux(isKeyword, wGrantfirst, wGrantlast)
  io.forwardInfo.inflight := reqValid
  io.forwardInfo.paddr := req.addr
  io.forwardInfo.rawData := refillAndStoreData
  io.forwardInfo.isFromStore := req.isFromStore
  io.forwardInfo.storeMask := reqStoreMask
  io.forwardInfo.firstbeatValid := wGrantfirstForwardInfo
  io.forwardInfo.lastbeatValid := wGrantlastForwardInfo
  io.forwardInfo.denied := denied
  io.forwardInfo.corrupt := corrupt

  // The prefetch_req only in mainPipe, now!
  // But the miss_req that hits prefetch_req is more than one!
  val hitPrefetchVec = Wire(Vec(reqNum, Bool()))
  for(i <- 0 until reqNum) {
    hitPrefetchVec(i) := io.queryME(i).req.valid && !io.queryME(i).req.bits.isFromPrefetch &&
                            reqValid && signalsVec(i).blockMatch && prefetch
  }
  io.matched := reqValid && signalsVec(0).blockMatch
  io.prefetchInfo.hitPrefetch := hitPrefetchVec
  io.prefetchInfo.hitPfSource := req.pfSource

  when(io.prefetchInfo.hitPrefetch.asUInt.orR) {
    prefetch := false.B
    req.pfSource := L1_HW_PREFETCH_CLEAR
  }

  io.l1Miss := reqValid
  // refill latency monitor
  val startCounting = GatedValidRegNext(io.memAcquire.fire) || (GatedValidRegNextN(primaryFire, 2) && sAcquire)
  io.latencyMonitor.loadMissRefilling  := reqValid && reqPrimaryFire.isFromLoad     && BoolStopWatch(startCounting, io.memGrant.fire && !refillDone, true, true)
  io.latencyMonitor.storeMissRefilling := reqValid && reqPrimaryFire.isFromStore    && BoolStopWatch(startCounting, io.memGrant.fire && !refillDone, true, true)
  io.latencyMonitor.amoMissRefilling   := reqValid && reqPrimaryFire.isFromAMO      && BoolStopWatch(startCounting, io.memGrant.fire && !refillDone, true, true)
  io.latencyMonitor.pfMissRefilling    := reqValid && reqPrimaryFire.isFromPrefetch && BoolStopWatch(startCounting, io.memGrant.fire && !refillDone, true, true)

  XSPerfAccumulate("miss_req_primary", primaryFire)
  XSPerfAccumulate("miss_req_merged", secondaryFire)
  XSPerfAccumulate("load_miss_penalty_to_use",
    shouldRefillData &&
      BoolStopWatch(primaryFire, io.refillToLdq.valid, true)
  )
  XSPerfAccumulate("penalty_between_grantlast_and_release",
    BoolStopWatch(!RegNext(wGrantlast) && wGrantlast, releaseEntry, true)
  )
  XSPerfAccumulate("main_pipe_penalty", BoolStopWatch(io.mainPipeReq.fire, io.mainPipeResp))
  XSPerfAccumulate("penalty_blocked_by_channel_A", io.memAcquire.valid && !io.memAcquire.ready)
  XSPerfAccumulate("penalty_waiting_for_channel_D", sAcquire && !wGrantlast && !io.memGrant.valid)
  XSPerfAccumulate("penalty_waiting_for_channel_E", io.memFinish.valid && !io.memFinish.ready)
  XSPerfAccumulate("prefetch_req_primary", Cat((0 until reqNum).map(i=> primaryFireVec(i) && io.queryME(i).req.bits.source === DCACHE_PREFETCH_SOURCE.U)).orR)
  XSPerfAccumulate("prefetch_req_merged", Cat((0 until reqNum).map(i=> secondaryFireVec(i) && io.queryME(i).req.bits.source === DCACHE_PREFETCH_SOURCE.U)).orR)
  XSPerfAccumulate("can_not_send_acquire_because_of_merging_store", !sAcquire && io.missReqPipeReg.merge && io.missReqPipeReg.cancel && missReqPipeRegBits.isFromStore)

  val (mshr_penalty_sample, mshr_penalty) = TransactionLatencyCounter(GatedValidRegNextN(primaryFire, 2) && !releaseEntry, releaseEntry)
  XSPerfHistogram("miss_penalty", mshr_penalty, mshr_penalty_sample, 0, 20, 1, true, true)
  XSPerfHistogram("miss_penalty", mshr_penalty, mshr_penalty_sample, 20, 100, 10, true, false)

  val loadMissBegin = ParallelMux(
    primaryFireVec
     zip
    io.queryME.map(_.req.bits.isFromLoad)
  )
  val refillFinished = GatedValidRegNext(!wGrantlast && refillDone) && shouldRefillData
  val (load_miss_penalty_sample, load_miss_penalty) = TransactionLatencyCounter(loadMissBegin, refillFinished) // not real refill finish time
  XSPerfHistogram("load_miss_penalty_to_use", load_miss_penalty, load_miss_penalty_sample, 0, 20, 1, true, true)
  XSPerfHistogram("load_miss_penalty_to_use", load_miss_penalty, load_miss_penalty_sample, 20, 100, 10, true, false)
  XSPerfHistogram("load_miss_penalty_to_use", load_miss_penalty, load_miss_penalty_sample, 100, 400, 30, true, false)

  val (a_to_d_penalty_sample, a_to_d_penalty) = TransactionLatencyCounter(startCounting, GatedValidRegNext(io.memGrant.fire && refillDone))
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
    val refillToLdq = ValidIO(new Refill)

    // cmo req
    val cmoReq = Flipped(DecoupledIO(new CMOReq))
    val cmoResp = DecoupledIO(new CMOResp)

    val queryMQ = Vec(reqNum, Flipped(new DCacheMQQueryIOBundle))

    val memAcquire = DecoupledIO(new TLBundleA(edge.bundle))
    val memGrant = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))
    val memFinish = DecoupledIO(new TLBundleE(edge.bundle))

    val l2Hint = Input(Valid(new L2ToL1Hint())) // Hint from L2 Cache

    val mainPipeReq = DecoupledIO(new MainPipeReq)
    val mainPipeResp = Flipped(ValidIO(new MainPipeResp))

    val mainpipeInfo = Input(new MainPipeInfoToMQ)
    val refillInfo = ValidIO(new MissQueueRefillInfo)
    val refillTrain = ValidIO(new TrainReqBundle)

    // block probe
    val probe = Flipped(new MissQueueBlockIO)

    // block replace when release an addr valid in mshr
    val replace = Flipped(new MissQueueBlockIO)

    // block all way for set to BtoT
    val evictSet = Input(UInt())
    val btotWaysForSet = Output(UInt(nWays.W))

    // occupy set check
    val occupySet = Input(Vec(LoadPipelineWidth, UInt()))
    val occupyFail = Output(Vec(LoadPipelineWidth, Bool()))

    // req blocked by wbq
    val wbqBlockMissReq = Input(Vec(reqNum, Bool()))

    val full = Output(Bool())

    // forward missqueue
    val forward = Flipped(Vec(LoadPipelineWidth, new DCacheForward))
    val forwardS1PAddrMatch = Output(Vec(LoadPipelineWidth, Bool()))
    // If a store is miss and accepted by mshr, Sbuffer releases the entry and mshr provides corresponding st-ld forwarding data.
    // Note: the resp of this st-ld forwarding is merged into io.forward.S2Resp interface
    val forwardStData = Flipped(Vec(LoadPipelineWidth, new SbufferForwardReq))
    val l2PfStoreOnly = Input(Bool())

    val memSetPattenDetected = Output(Bool())
    val lqEmpty = Input(Bool())

    // sbuffer-flush must flush all store entries in mshr as well
    val mshrStoreEmpty = Output(Bool())

    val prefetchStat = Output(new MissPrefetchStatBundle)

    val wfi = Flipped(new WfiReqBundle)

    val debugTopDown = new DCacheTopDownIO
    val l1Miss = Output(Bool())
  })

  // 128KBL1: FIXME: provide vaddr for l2

  val entries = Seq.fill(cfg.nMissEntries)(Module(new MissEntry(edge, reqNum)))
  val cmoUnit = Module(new CMOUnit(edge))

  // Parallel pipeline registers for queryMQ path (reqNum ports)
  val parallelPipeRegs = RegInit(VecInit(Seq.fill(reqNum)(0.U.asTypeOf(new MissReqPipeRegBundle(edge)))))

  val acquireFromPiperegVec = Wire(Vec(reqNum, chiselTypeOf(io.memAcquire)))

  // val signals = computeMatchSignals(miss_req_pipe_reg.req, io.req.bits)
  val signalsVec = (0 until reqNum).map {i =>
    computeMatchSignals(parallelPipeRegs(i).req, io.queryMQ(i).req.bits)
  }

  // Store misses may reside either in MSHR entries or in the miss_req_pipe_reg.
  // sbuffer-flush should wait until both places are clear.
  val mshrHasStore = Cat(entries.map(_.io.reqHasStore)  ++ parallelPipeRegs.map(pipe_reg => pipe_reg.regValid() && pipe_reg.req.isFromStore)).orR
  io.mshrStoreEmpty := !mshrHasStore

  val primaryReadyVec = entries.map(_.io.primaryReady)

  // secondary_ready_vec(i)(e) = entry e can merge request i
  val secondaryReadyVec = (0 until reqNum).map { i =>
    entries.map(_.io.secondaryReady(i))
  }

  // secondary_reject_vec(i)(e) = entry e will reject request i
  val secondaryRejectVec = (0 until reqNum).map { i =>
    entries.map(_.io.secondaryReject(i))
  }

  // val block_match_vec = entries.map(_.io.match_signals.block_match)

  val blockMatchSeqs = (0 until reqNum).map { i =>
    entries.map(_.io.matchSignalsVec(i).blockMatch)
  }

  val probeBlockVec = entries.map {
    case e =>
      e.io.probe.req <> io.probe.req
      e.io.probe.block
  }

  val canMergeVec = WireInit(VecInit(Seq.fill(reqNum)(false.B)))
  val matchFromPipe = WireInit(VecInit(Seq.fill(reqNum)(false.B)))
  val matchFromIthPipe = WireInit(0.U(log2Up(reqNum).W))
  val canMergeFromPipe = WireInit(VecInit(Seq.fill(reqNum)(false.B)))
  val canMergeFromPipeMshr = Wire(Vec(reqNum, UInt(log2Up(cfg.nMissEntries).W)))
  val canMergeStoreFromPipe = WireInit(VecInit(Seq.fill(reqNum)(false.B)))
  val canAllocateVec = WireInit(VecInit(Seq.fill(reqNum)(false.B)))

  // ===== Analyze all requests =====
  // Analysis result for all queryMQ requests
  val analysis = WireInit(0.U.asTypeOf(new ReqAnalysisResult(reqNum, cfg.nMissEntries)))

  // Build free entry list for efficient allocation
  // free_entry_list(i) = the i-th free entry index
  // For example, if entries 2, 5, 7 are free, then free_entry_list = [2, 5, 7, x, x, ...]
  // Use parallel prefix sum (Kogge-Stone) for O(log n) delay

  // Initial state: each entry is 1 if free, 0 if busy
  val initialFree = VecInit(entries.map(_.io.primaryReady).map(_.asUInt))

  // Kogge-Stone parallel prefix sum
  // ps_stage[s][e] = prefix sum after stage s, for entry e
  val numStages = log2Up(cfg.nMissEntries)
  val psStage = VecInit(Seq.fill(numStages + 1)(WireInit(VecInit(Seq.fill(cfg.nMissEntries)(0.U(log2Up(cfg.nMissEntries + 1).W))))))

  // Stage 0: initial values
  for (e <- 0 until cfg.nMissEntries) {
    psStage(0)(e) := initialFree(e)
  }

  // Subsequent stages: parallel prefix sum
  for (stage <- 0 until numStages) {
    val stride = 1 << stage
    for (e <- 0 until cfg.nMissEntries) {
      if (e < stride) {
        psStage(stage + 1)(e) := psStage(stage)(e)
      } else {
        psStage(stage + 1)(e) := psStage(stage)(e) + psStage(stage)(e - stride)
      }
    }
  }

  // Extract "count before" (shift right by 1)
  val freeCountBefore = WireInit(VecInit(Seq.fill(cfg.nMissEntries)(0.U(log2Up(cfg.nMissEntries + 1).W))))
  for (e <- 0 until cfg.nMissEntries) {
    if (e == 0) {
      freeCountBefore(0) := 0.U
    } else {
      freeCountBefore(e) := psStage(numStages)(e - 1)
    }
  }

  // Build the free entry list using one-hot encoding
  val freeEntryOnehot = WireInit(VecInit(Seq.fill(cfg.nMissEntries)(VecInit(Seq.fill(cfg.nMissEntries)(false.B)))))
  for (i <- 0 until cfg.nMissEntries) {
    for (e <- 0 until cfg.nMissEntries) {
      freeEntryOnehot(i)(e) := entries(e).io.primaryReady && freeCountBefore(e) === i.U
    }
  }

  // Convert one-hot to entry index
  val freeEntryList = WireInit(VecInit(Seq.fill(cfg.nMissEntries)(0.U(log2Up(cfg.nMissEntries).W))))
  for (i <- 0 until cfg.nMissEntries) {
    for (e <- 0 until cfg.nMissEntries) {
      when (freeEntryOnehot(i)(e)) {
        freeEntryList(i) := e.U
      }
    }
  }

  // Total count of free entries
  val freeEntryCount = psStage(numStages)(cfg.nMissEntries - 1)

  // Detect address conflicts (compress strategy)
  val addrConflicts = WireInit(VecInit(Seq.fill(reqNum)(VecInit(Seq.fill(reqNum)(false.B)))))
  // block_match & not_alias_match, should not compress/alloc/merge
  val reqRejectVec = WireInit(VecInit(Seq.fill(reqNum)(false.B)))
  for (i <- 0 until reqNum) {
    for (j <- 0 until reqNum) {
      addrConflicts(i)(j) := io.queryMQ(j).req.valid && io.queryMQ(i).req.valid && j.U =/= i.U &&
                              get_block(io.queryMQ(i).req.bits.addr) === get_block(io.queryMQ(j).req.bits.addr) &&
                              isAliasMatch(io.queryMQ(i).req.bits.vaddr, io.queryMQ(j).req.bits.vaddr)
    }

    // bigger index miss_req will be reject: 
    reqRejectVec(i) :=(0 until reqNum).map{ j => 
      io.queryMQ(j).req.valid && io.queryMQ(i).req.valid && j.U < i.U &&
        get_block(io.queryMQ(i).req.bits.addr) === get_block(io.queryMQ(j).req.bits.addr) &&
        !isAliasMatch(io.queryMQ(i).req.bits.vaddr, io.queryMQ(j).req.bits.vaddr)  
    }.reduce(_ || _)
  }

  for(i <- 0 until reqNum) {
    canMergeFromPipeMshr(i) := 0.U

    for (j <- (0 until reqNum).reverse) {
      val signalsJ = computeMatchSignals(parallelPipeRegs(j).req, io.queryMQ(i).req.bits)

      // merge from pipe_reg
      when (parallelPipeRegs(j).mergeReq(signalsJ) && !parallelPipeRegs(j).cancel) {
        canMergeFromPipe(i) := true.B
        canMergeFromPipeMshr(i) := parallelPipeRegs(j).mshrId
      }

      // A store merging into an allocating pipe reg must be included before
      // sending Acquire, otherwise the entry cmd and Grant param can diverge.
      when(parallelPipeRegs(j).alloc && !parallelPipeRegs(j).cancel && parallelPipeRegs(j).mergeReq(signalsJ) && io.queryMQ(i).req.bits.isFromStore && io.queryMQ(i).req.valid) {
        canMergeStoreFromPipe(j) := true.B
      }

      // do not alloc when (addr_match & alias_match)
      when(parallelPipeRegs(j).alloc && !parallelPipeRegs(j).cancel && signalsJ.blockMatch) {
        matchFromPipe(i) := true.B
        matchFromIthPipe := j.U
      }
    }
  }

  for(i <- 0 until reqNum) {
    val signals = signalsVec(i)

    canAllocateVec(i) := freeEntryCount =/= 0.U && !reqRejectVec(i) &&
                            !ParallelORR(Cat(blockMatchSeqs(i) ++ Seq(matchFromPipe(i))))

    val canMergeFromEntry = ParallelORR(Cat(secondaryReadyVec(i)))

    canMergeVec(i) := canMergeFromEntry || canMergeFromPipe(i)
  }

  // Pre-calculate which requests need allocation
  // This is used to calculate alloc_order for proper free entry assignment
  val needsAllocate = WireInit(VecInit(Seq.fill(reqNum)(false.B)))

  for (i <- 0 until reqNum) {
    val reqValid = io.queryMQ(i).req.valid

    // Quick check: will this request need allocation?
    val hasAddrConflict = addrConflicts(i).asUInt.orR

    needsAllocate(i) := false.B
    when (reqValid) {
      when (hasAddrConflict) {
        // Compress case: needs allocation if can_allocate (and cannot merge/reject)
        needsAllocate(i) := canAllocateVec(i) && (analysis.compressGroup(i) === i.U)
      }.elsewhen (canAllocateVec(i)) {
        // Allocate only case
        needsAllocate(i) := true.B
      }
    }
  }

  // Calculate alloc_order(i) = number of valid allocation requests with index < i
  val allocOrder = WireInit(VecInit(Seq.fill(reqNum)(0.U(log2Up(reqNum + 1).W))))
  for (i <- 0 until reqNum) {
    if (i == 0) {
      allocOrder(0) := 0.U
    } else {
      val prevCount = allocOrder(i - 1)
      allocOrder(i) := Mux(needsAllocate(i - 1), prevCount + 1.U, prevCount)
    }
  }

  // For each request, determine strategy (allocate/merge/compress)
  // Priority: compress > merge > allocate
  for (i <- 0 until reqNum) {
    val reqValid = io.queryMQ(i).req.valid
    val reqBits = io.queryMQ(i).req.bits

    val hasAddrConflict = addrConflicts(i).asUInt.orR

    val mergeTargetsEntry = secondaryReadyVec(i)
    val mergeTargetsPipeReg = canMergeFromPipe(i)

    // Manually find the first merge target
    val mergeTargetId = WireInit(0.U(log2Up(cfg.nMissEntries).W))
    for (e <- (0 until cfg.nMissEntries).reverse) {
      when (mergeTargetsEntry(e)) {
        mergeTargetId := e.U
      }.elsewhen (mergeTargetsPipeReg) {
        mergeTargetId := canMergeFromPipeMshr(i)
      }
    }

    // Determine strategy based on priority
    // strategy, bit 0: 1.U, allocate, bit 1: 2.U, merge, bit 2: 4.U, compress
    when (reqValid) {
      when (addrConflicts(i).asUInt.orR) {
        // Use lowest index among conflicting requests as group ID
        val conflictGroupId = WireInit(i.U(log2Up(reqNum).W))

        // Check all smaller indices for conflicts
        for (j <- (0 until i).reverse) {
          when (addrConflicts(i)(j)) {
            conflictGroupId := j.U
          }
        }

        analysis.compressGroup(i) := conflictGroupId

        // Has address conflict -> needs compression
        analysis.strategy(i) := 4.U
        analysis.targetMshr(i) := 0.U

        when (canMergeVec(i)) {
          // compress & merge
          analysis.strategy(i) := 4.U | 2.U
          analysis.targetMshr(i) := mergeTargetId
        }.elsewhen (canAllocateVec(i)) {
          // compress & alloc
          analysis.strategy(i) := 4.U | 1.U
          // Allocate using alloc_order to account for invalid earlier requests
          val hasEnoughFree = allocOrder(i) < freeEntryCount

          when (hasEnoughFree) {
            analysis.targetMshr(i) := freeEntryList(allocOrder(i))
          }.otherwise {
            // Not enough free entries, remove allocate bit
            analysis.strategy(i) := 4.U
            analysis.targetMshr(i) := 0.U
          }
        }

        // Only the first request in the group is valid for compress strategy
        analysis.valid(i) := i.U === conflictGroupId
      }.elsewhen (canMergeVec(i)) {
        // Can merge to existing MSHR
        analysis.strategy(i) := 2.U
        analysis.valid(i) := true.B
        analysis.targetMshr(i) := mergeTargetId
        analysis.compressGroup(i) := i.U
      }.elsewhen (canAllocateVec(i)) {
        // Can allocate to new MSHR
        analysis.strategy(i) := 1.U
        analysis.valid(i) := true.B

        // Allocate using alloc_order to account for invalid earlier requests
        val hasEnoughFree = allocOrder(i) < freeEntryCount

        when (hasEnoughFree) {
          analysis.targetMshr(i) := freeEntryList(allocOrder(i))
          analysis.compressGroup(i) := i.U
        }.otherwise {
          // Not enough free entries, mark as invalid
          analysis.strategy(i) := 0.U
          analysis.valid(i) := false.B
          analysis.targetMshr(i) := 0.U
          analysis.compressGroup(i) := i.U
        }
      }.otherwise {
        // No available resource
        analysis.strategy(i) := 0.U  // none
        analysis.valid(i) := false.B
        analysis.targetMshr(i) := 0.U
        analysis.compressGroup(i) := i.U
      }
    }
  }

  // ===== Generate ready signals (immediate response) =====
  // Generate ready signals for queryMQ in the same cycle
  // This allows upstream modules to know immediately if requests are accepted
  for (i <- 0 until reqNum) {
    val hasCompress = (analysis.strategy(i) & 4.U) =/= 0.U
    val hasMerge = (analysis.strategy(i) & 2.U) =/= 0.U
    val hasAlloc = (analysis.strategy(i) & 1.U) =/= 0.U
    val isValid = analysis.valid(i)
    val targetMshr = analysis.targetMshr(i)  // Explicit use to prevent Chisel optimization
    val targetGroup = analysis.compressGroup(i)

    // Compress: only the first in group is master, others are slaves
    val isFirstInGroup = targetGroup === i.U
    val compressReady = hasCompress && Mux(isFirstInGroup, hasMerge || hasAlloc, (analysis.strategy(targetGroup) & 3.U) =/= 0.U)

    val mergeReady = hasMerge && !hasCompress && isValid

    val allocReady = hasAlloc && !hasCompress && !hasMerge && isValid

    io.queryMQ(i).ready := (compressReady || mergeReady || allocReady) && 
      !(io.wbqBlockMissReq(i) || io.wbqBlockMissReq(analysis.compressGroup(i)) || io.queryMQ(analysis.compressGroup(i)).req.bits.cancel)
  }

  // For each queryMQ request that was granted, connect to MissEntry or PipeReg
  val queryFire = WireInit(VecInit(Seq.fill(reqNum)(false.B)))
  for (i <- 0 until reqNum) {
    queryFire(i) := io.queryMQ(i).req.valid && io.queryMQ(i).ready
  }

  /*  MissQueue enq logic is now splitted into 2 cycles
   *
   */
  // Update parallel pipeline registers
  for (i <- 0 until reqNum) {
    when (io.queryMQ(i).req.valid) {
      parallelPipeRegs(i).req := io.queryMQ(i).req.bits
    }
    parallelPipeRegs(i).alloc := ((analysis.strategy(i) & 1.U) =/= 0.U) &&
                                    (analysis.compressGroup(i) === i.U) &&
                                    !io.queryMQ(i).req.bits.cancel &&
                                    !io.wbqBlockMissReq(i)

    parallelPipeRegs(i).merge := ((analysis.strategy(i) & 2.U) =/= 0.U) &&
                                    (analysis.compressGroup(i) === i.U) &&
                                    !io.queryMQ(i).req.bits.cancel &&
                                    !io.wbqBlockMissReq(i)

    parallelPipeRegs(i).mshrId := analysis.targetMshr(i)
    parallelPipeRegs(i).cancel := io.wbqBlockMissReq(i)
  }

  val reqMshrHandledVec = entries.map(_.io.reqHandledByThisEntry)

  // For compressed requests, we need to return the actual MSHR that will handle them
  // This is the target_mshr of the first request in their compression group
  val actualTargetMshrForGroup = WireInit(VecInit(Seq.fill(reqNum)(0.U(log2Up(cfg.nMissEntries).W))))
  for (i <- 0 until reqNum) {
    // Find the first request in this compression group
    val groupLeader = analysis.compressGroup(i)
    // All requests in the same group share the same target MSHR
    actualTargetMshrForGroup(i) := Mux(
      groupLeader === i.U,
      analysis.targetMshr(i),  // I am the leader, use my own target
      analysis.targetMshr(groupLeader)  // I'm a follower, use leader's target
    )
  }

  for(i <- 0 until reqNum) {
    // merged to pipeline reg
    io.resp(i).id := Mux(
      canMergeFromPipe(i),
      canMergeFromPipeMshr(i),
      actualTargetMshrForGroup(i)
    )
    io.resp(i).handled := (Cat(reqMshrHandledVec).orR || canMergeFromPipe(i)) && queryFire(i)
    io.resp(i).merged := (analysis.strategy(i) & 2.U) =/= 0.U
  }

  val sourceExceptLoadCnt = RegInit(0.U(10.W))
  for(i <- 0 until reqNum) {
    when(VecInit(reqMshrHandledVec).asUInt.orR || canMergeFromPipe(i)) {
      when(io.queryMQ(i).req.bits.isFromLoad) {
        sourceExceptLoadCnt := 0.U
      }.otherwise {
        when(io.queryMQ(i).req.bits.isFromStore) {
          sourceExceptLoadCnt := sourceExceptLoadCnt + 1.U
        }
      }
    }
  }
  val Threshold = 8
  val memSetPattenDetected = GatedValidRegNext((sourceExceptLoadCnt >= Threshold.U) && io.lqEmpty)

  io.memSetPattenDetected := memSetPattenDetected

  val forwardInfoVec = VecInit(entries.map(_.io.forwardInfo))
  val VLENB = VLEN / 8
  // Forwarding paddr CAM, shared by io.forward and io.forward_stData
  val paddrFwd = Wire(Vec(LoadPipelineWidth, UInt(PAddrBits.W)))
  val s1_paddr_match_vec = Wire(Vec(LoadPipelineWidth, Vec(cfg.nMissEntries, Bool())))
  val s1_select_oh = Wire(Vec(LoadPipelineWidth, UInt((cfg.nMissEntries).W)))
  val s1_forward_info = Wire(Vec(LoadPipelineWidth, new MissEntryForwardIO))
  val paddrFwdSelOH = Wire(Vec(LoadPipelineWidth, Vec(blockBytes / VLENB, Bool())))
  val s1_resp_data_fwd = Wire(Vec(LoadPipelineWidth, UInt(VLEN.W)))
  for (i <- 0 until LoadPipelineWidth) {
    paddrFwd(i) := Mux(RegNext(io.forwardStData(i).s0Req.valid), io.forwardStData(i).s1Req.paddr, io.forward(i).s1_req.paddr)
    s1_paddr_match_vec(i) := VecInit(forwardInfoVec.map{ case info =>
      (paddrFwd(i) >> blockOffBits) === (info.paddr >> blockOffBits) && info.inflight})
    s1_select_oh(i) := s1_paddr_match_vec(i).asUInt
    s1_forward_info(i) := Mux1H(s1_select_oh(i), forwardInfoVec)
    // Select VLEN (128-bit) data from cacheline data
    paddrFwdSelOH(i) := (0 until (blockBytes / VLENB)).map(k => paddrFwd(i)(blockOffBits - 1, log2Up(VLENB)) === k.U)
    s1_resp_data_fwd(i) := Mux1H(paddrFwdSelOH(i), s1_forward_info(i).rawData.grouped(VLEN / rowBits).map(VecInit(_).asUInt).toSeq)
  }

  io.forward.zipWithIndex.foreach { case (forward, i) =>
    val s0_req_valid = forward.s0_req.valid
    val s0_req = forward.s0_req.bits
    val s1_req_valid = RegNext(s0_req_valid)
    val s1_req = RegEnable(s0_req, s0_req_valid)
    val mshrIdOH = UIntToOH(s1_req.mshrId)
    val s1_beat_match_vec  = VecInit(forwardInfoVec.map{ case info =>
      Mux(paddrFwd(i)(log2Up(refillBytes)).asBool,
        info.lastbeatValid,
        info.firstbeatValid
    )})
    val s1_resp_valid = s1_req_valid && (s1_select_oh(i) & s1_beat_match_vec.asUInt).orR

    // store-load forwarding (from io.forward_stData)
    val s1_req_valid_st_ld_fwd = RegNext(io.forwardStData(i).s0Req.valid)
    val s1_resp_mask_st_ld_fwd = Mux1H(paddrFwdSelOH(i),
          s1_forward_info(i).storeMask.grouped(VLENB).map(x => Mux(s1_forward_info(i).isFromStore, x, 0.U)))
    val s1_resp_valid_st_ld_fwd = s1_req_valid_st_ld_fwd && s1_select_oh(i).orR

    val s2_resp_valid = RegNext(s1_resp_valid)
    val s2_resp_st_ld_fwd_valid = RegNext(s1_resp_valid_st_ld_fwd)

    forward.s2_resp.valid := s2_resp_valid || s2_resp_st_ld_fwd_valid
    forward.s2_resp.bits.matchInvalid := false.B
    forward.s2_resp.bits.forwardData := RegEnable(s1_resp_data_fwd(i).asTypeOf(forward.s2_resp.bits.forwardData),
                                                 s1_req_valid || s1_req_valid_st_ld_fwd)
    forward.s2_resp.bits.forwardMask := VecInit((0 until VLENB).map(k =>
      Mux(s2_resp_valid, true.B, RegEnable(s1_resp_mask_st_ld_fwd(k), s1_resp_valid_st_ld_fwd) && s2_resp_st_ld_fwd_valid)
    ))
    forward.s2_resp.bits.denied := RegEnable(s1_forward_info(i).denied, s1_req_valid) && s2_resp_valid
    forward.s2_resp.bits.corrupt := RegEnable(s1_forward_info(i).corrupt, s1_req_valid) && s2_resp_valid
    io.forwardS1PAddrMatch(i) := s1_req_valid && (mshrIdOH & s1_paddr_match_vec(i).asUInt).orR
    XSError(((s1_select_oh(i) - 1.U) & s1_select_oh(i)).orR && s1_resp_valid, "multi mshr hit when forward!\n")
  }

  for(i <- 0 until reqNum) {
    assert(RegNext(PopCount(secondaryReadyVec(i)) <= 1.U || !io.queryMQ(i).req.valid))
  }

  def selectValidOne[T <: Bundle](
    in: Seq[DecoupledIO[T]],
    out: DecoupledIO[T],
    name: Option[String] = None): Unit = {

    if (name.nonEmpty) { out.suggestName(s"${name.get}_select") }
    out.valid := Cat(in.map(_.valid)).orR
    out.bits := ParallelMux(in.map(_.valid) zip in.map(_.bits))
    in.map(_.ready := out.ready)
    assert(!RegNext(out.valid && PopCount(Cat(in.map(_.valid))) > 1.U))
  }

  io.memGrant.ready := false.B

  val nMaxPrefetchEntry = Constantin.createRecord(s"nMaxPrefetchEntry${p(XSCoreParamsKey).HartId}", initValue = cfg.nMissEntries - 2)
  entries.zipWithIndex.foreach {
    case (e, i) =>
      val formerPrimaryReady = if(i == 0)
        false.B
      else
        Cat((0 until i).map(j => entries(j).io.primaryReady)).orR

      val formerReadyCount = if(i == 0)
        0.U
      else
        PopCount((0 until i).map(j => entries(j).io.primaryReady))

      val hasNFormerReady = formerReadyCount >= reqNum.U
      e.io.entryValid := !hasNFormerReady

      e.io.hartId := io.hartId
      e.io.id := i.U
      e.io.l2PfStoreOnly := io.l2PfStoreOnly
      e.io.wbqBlockMissReq := io.wbqBlockMissReq

      e.io.memGrant.valid := false.B
      e.io.memGrant.bits := DontCare
      when (io.memGrant.bits.source === i.U) {
        e.io.memGrant <> io.memGrant
      }

      e.io.missReqPipeReg       := DontCare
      e.io.missReqPipeReg.merge := false.B
      e.io.missReqPipeReg.alloc := false.B
      for(req <- 0 until reqNum) {
        when(parallelPipeRegs(req).regValid() && parallelPipeRegs(req).mshrId === i.U) {
          e.io.missReqPipeReg := parallelPipeRegs(req)
        }
      }

      e.io.acquireFiredByPipeReg := false.B
      for(j <- 0 until reqNum) {
        when(acquireFromPiperegVec(j).fire && parallelPipeRegs(j).mshrId === i.U) {
          e.io.acquireFiredByPipeReg := true.B
        }
      }

      e.io.mainPipeResp := io.mainPipeResp.valid && io.mainPipeResp.bits.ackMissQueue && io.mainPipeResp.bits.missId === i.U
      e.io.mainPipeReplay := io.mainpipeInfo.s2_valid && io.mainpipeInfo.s2_replay_to_mq && io.mainpipeInfo.s2_miss_id === i.U
      e.io.mainPipeEvictBtoTWay := io.mainpipeInfo.s2_valid && io.mainpipeInfo.s2_evict_bto_t_way && io.mainpipeInfo.s2_miss_id === i.U
      e.io.mainPipeNextEvictWay := io.mainpipeInfo.s2_next_evict_way
      e.io.mainPipeRefillResp := io.mainpipeInfo.s3_valid && io.mainpipeInfo.s3_refill_resp && io.mainpipeInfo.s3_miss_id === i.U

      e.io.memSetPattenDetected := memSetPattenDetected
      e.io.nMaxPrefetchEntry := nMaxPrefetchEntry

      e.io.mainPipeReq.ready := io.mainPipeReq.ready

      for (j <- 0 until reqNum) {
        e.io.queryME(j).req.valid := io.queryMQ(j).req.valid
        e.io.queryME(j).req.bits  := io.queryMQ(j).req.bits.toMissReqWoStoreData()
      }

      e.io.l2Hint.valid := false.B
      e.io.l2Hint.bits := DontCare
      when(io.l2Hint.bits.sourceId === i.U) {
        e.io.l2Hint <> io.l2Hint
      }

      e.io.wfi.wfiReq := io.wfi.wfiReq
  }

  cmoUnit.io.wfi.wfiReq := io.wfi.wfiReq
  cmoUnit.io.req <> io.cmoReq
  io.cmoResp <> cmoUnit.io.respToLsq
  when (io.memGrant.valid && io.memGrant.bits.opcode === TLMessages.CBOAck) {
    cmoUnit.io.respChanD <> io.memGrant
  } .otherwise {
    cmoUnit.io.respChanD.valid := false.B
    cmoUnit.io.respChanD.bits := DontCare
  }
  io.wfi.wfiSafe := (Seq(cmoUnit.io.wfi.wfiSafe) ++ entries.map(_.io.wfi.wfiSafe)).reduce(_&&_)

  // io.req.ready := accept
  io.refillToLdq.valid := Cat(entries.map(_.io.refillToLdq.valid)).orR
  io.refillToLdq.bits := ParallelMux(entries.map(_.io.refillToLdq.valid) zip entries.map(_.io.refillToLdq.bits))

  io.refillInfo.valid := VecInit(entries.zipWithIndex.map{ case(e,i) => e.io.refillInfo.valid && io.mainpipeInfo.s2_valid && io.mainpipeInfo.s2_miss_id === i.U}).asUInt.orR
  io.refillInfo.bits := Mux1H(entries.zipWithIndex.map{ case(e,i) => (io.mainpipeInfo.s2_miss_id === i.U) -> e.io.refillInfo.bits })

  io.refillTrain.valid := VecInit(entries.zipWithIndex.map{ case(e,i) => e.io.refillTrain.valid && io.mainpipeInfo.s2_valid && io.mainpipeInfo.s2_miss_id === i.U}).asUInt.orR
  io.refillTrain.bits := Mux1H(entries.zipWithIndex.map{ case(e,i) => (io.mainpipeInfo.s2_miss_id === i.U) -> e.io.refillTrain.bits })

  for(i <- 0 until reqNum) {
    acquireFromPiperegVec(i).valid := parallelPipeRegs(i).alloc && !canMergeStoreFromPipe(i) && !io.wfi.wfiReq
    acquireFromPiperegVec(i).bits := parallelPipeRegs(i).getAcquire(io.l2PfStoreOnly)

    XSPerfAccumulate(s"acquire_fire_from_pipereg_$i", acquireFromPiperegVec(i).fire)
    XSPerfAccumulate(s"parallel_pipe_regs_valid_$i", parallelPipeRegs(i).regValid())
  }

  val acquireSources = Seq(cmoUnit.io.reqChanA) ++ acquireFromPiperegVec ++ entries.map(_.io.memAcquire)
  TLArbiter.lowest(edge, io.memAcquire, acquireSources:_*)
  TLArbiter.lowest(edge, io.memFinish, entries.map(_.io.memFinish):_*)

  // amo's main pipe req out
  arbiter(entries.map(_.io.mainPipeReq), io.mainPipeReq, Some("main_pipe_req"))

  io.probe.block := Cat(probeBlockVec).orR
  io.replace.block := Cat(
    entries.map { e =>
      e.io.replace.req <> io.replace.req
      e.io.replace.block
    } ++ parallelPipeRegs.map(_.blockAndAliasMatch(io.replace.req.bits))
  ).orR

  val btotEvictSetHit = entries.map(e => e.io.reqIsBtoT && e.io.reqVaddr.valid && addrToDcacheSet(e.io.reqVaddr.bits) === io.evictSet) ++
    parallelPipeRegs.map(_.evictSetMatch(io.evictSet))
  val btotOccupyWays = entries.map(e => e.io.occupyWay) ++ parallelPipeRegs.map(_.req.occupyWay)
  io.btotWaysForSet := btotEvictSetHit.zip(btotOccupyWays).map {
    case (hit, way) => Fill(nWays, hit) & way
  }.reduce(_|_)

  // LoadPipe occupy check
  for (i <- 0 until LoadPipelineWidth) {
    val occupySetHits = entries.map(
      e => e.io.reqIsBtoT && e.io.reqVaddr.valid && addrToDcacheSet(e.io.reqVaddr.bits) === io.occupySet(i)
    ) ++ parallelPipeRegs.map(_.evictSetMatch(io.occupySet(i)))
    val occupyWays = occupySetHits.zip(btotOccupyWays).map {
      case (hit, way) => Fill(nWays, hit) & way
    }.reduce(_|_)
    io.occupyFail(i) := PopCount(occupyWays) > (nWays-2).U
  }

  io.full := ~Cat(entries.map(_.io.primaryReady)).andR

  // prefetch related. The prefetch_req only in mainPipe, Now!
  val lateInReg = matchFromPipe(0)
  io.prefetchStat.pf_late_in_mshr := io.queryMQ(0).req.valid && io.queryMQ(0).req.bits.isFromPrefetch &&
                                        (lateInReg || Cat(entries.map(_.io.matched)).orR)
  io.prefetchStat.pf_late_in_mshr_source := ParallelMux(
    Seq(lateInReg) ++ entries.map(_.io.matched)
    zip
    Seq(parallelPipeRegs(matchFromIthPipe).req.pfSource) ++ entries.map(_.io.prefetchInfo.hitPfSource)
  )

  io.prefetchStat.prefetch_miss := queryFire(0) && io.queryMQ(0).req.bits.isFromPrefetch
  io.prefetchStat.pf_source := io.queryMQ(0).req.bits.pfSource
  io.prefetchStat.load_miss := PopCount((0 until reqNum).map(j => queryFire(j) && io.queryMQ(j).req.bits.isFromLoad))

  // compute all miss_req hit prefetch_req or not
  val prefetchHitInRegVec = Wire(Vec(reqNum, Bool()))
  val prefetchHitInMshrVec = Wire(Vec(reqNum, Bool()))
  for(i <- 0 until reqNum) {
    val signals_ = (0 until reqNum).map(j => computeMatchSignals(parallelPipeRegs(j).req, io.queryMQ(i).req.bits))
    val hitInReg = (0 until reqNum).map(j => parallelPipeRegs(j).prefetchLateEn(signals_(j), io.queryMQ(i).req.bits.toMissReqWoStoreData(), io.queryMQ(i).req.valid))
    
    prefetchHitInRegVec(i) := hitInReg.asUInt.orR
    prefetchHitInMshrVec(i) := io.queryMQ(i).req.valid && !io.queryMQ(i).req.bits.isFromPrefetch && Cat(entries.map(_.io.prefetchInfo.hitPrefetch(i))).orR
  }
  io.prefetchStat.hit_pf_in_mshr := PopCount((0 until reqNum).map(i => prefetchHitInRegVec(i) || prefetchHitInMshrVec(i)))
  for(i <- 0 until reqNum) {
    io.prefetchStat.hit_pf_in_mshr_source(i) := ParallelMux(
      Seq(prefetchHitInRegVec(i)) ++ entries.map(_.io.prefetchInfo.hitPrefetch(i))
      zip
      Seq(parallelPipeRegs(i).req.pfSource) ++ entries.map(_.io.prefetchInfo.hitPfSource)
    )
  }

  // L1MissTrace Chisel DB - support multiple enqueue ports
  val debugMissTraceVec = Wire(Vec(reqNum, new L1MissTrace))
  for (i <- 0 until reqNum) {
    debugMissTraceVec(i).vaddr := io.queryMQ(i).req.bits.vaddr
    debugMissTraceVec(i).paddr := io.queryMQ(i).req.bits.addr
    debugMissTraceVec(i).source := io.queryMQ(i).req.bits.source
    debugMissTraceVec(i).pc := io.queryMQ(i).req.bits.pc
  }

  val isWriteL1MissQMissTable = Constantin.createRecord(s"isWriteL1MissQMissTable${p(XSCoreParamsKey).HartId}")
  val table = ChiselDB.createTable(s"L1MissQMissTrace_hart${p(XSCoreParamsKey).HartId}", new L1MissTrace)
  for (i <- 0 until reqNum) {
    table.log(debugMissTraceVec(i), isWriteL1MissQMissTable.orR && queryFire(i) && !io.queryMQ(i).req.bits.cancel && ((analysis.strategy(i) & 1.U) =/= 0.U), s"MissQueue_$i", clock, reset)
  }

  // Difftest
  if (env.EnableDifftest) {
    val difftest = DifftestModule(new DiffRefillEvent, dontCare = true)
    difftest.coreid := io.hartId
    difftest.index := 1.U
    difftest.valid := io.refillToLdq.valid && io.refillToLdq.bits.hasdata && io.refillToLdq.bits.refillDone
    difftest.addr := io.refillToLdq.bits.addr
    difftest.data := io.refillToLdq.bits.dataRaw.asTypeOf(difftest.data)
    difftest.mask := VecInit.fill(difftest.mask.getWidth)(true.B).asUInt
  }

  if (env.EnableDifftest) {
    // Store-miss refill completed in MainPipe S3: update difftest (DiffSbufferEvent).
    val mqS3SelInfo = Mux1H(
      entries.map(e => (io.mainpipeInfo.s3_miss_id === e.io.id) -> e.io.forwardInfo)
    )
    val hasStore = mqS3SelInfo.storeMask.orR
    val difftestStore = DifftestModule(new DiffSbufferEvent, delay = 1)
    difftestStore.coreid := io.hartId
    difftestStore.index := 0.U
    difftestStore.valid := io.mainpipeInfo.s3_valid && io.mainpipeInfo.s3_refill_resp && hasStore
    difftestStore.addr := mqS3SelInfo.paddr
    difftestStore.data := mqS3SelInfo.rawData.asUInt.asTypeOf(difftestStore.data)
    difftestStore.mask := mqS3SelInfo.storeMask
  }

  // Perf count - adapted for multiple enqueue ports
  XSPerfAccumulate("miss_req", PopCount(queryFire))
  XSPerfAccumulate("miss_req_allocate", PopCount(Cat((0 until reqNum).map(i => queryFire(i) && ((analysis.strategy(i) & 1.U) =/= 0.U)))))
  XSPerfAccumulate("miss_req_load_allocate", PopCount(Cat((0 until reqNum).map(i => queryFire(i) && ((analysis.strategy(i) & 1.U) =/= 0.U) && io.queryMQ(i).req.bits.isFromLoad))))
  XSPerfAccumulate("miss_req_store_allocate", PopCount(Cat((0 until reqNum).map(i => queryFire(i) && ((analysis.strategy(i) & 1.U) =/= 0.U) && io.queryMQ(i).req.bits.isFromStore))))
  XSPerfAccumulate("miss_req_amo_allocate", PopCount(Cat((0 until reqNum).map(i => queryFire(i) && ((analysis.strategy(i) & 1.U) =/= 0.U) && io.queryMQ(i).req.bits.isFromAMO))))
  XSPerfAccumulate("miss_req_prefetch_allocate", PopCount(Cat((0 until reqNum).map(i => queryFire(i) && ((analysis.strategy(i) & 1.U) =/= 0.U) && io.queryMQ(i).req.bits.isFromPrefetch))))
  XSPerfAccumulate("miss_req_merge_load", PopCount(Cat((0 until reqNum).map(i => queryFire(i) && ((analysis.strategy(i) & 2.U) =/= 0.U) && io.queryMQ(i).req.bits.isFromLoad))))
  XSPerfAccumulate("miss_req_reject_load", PopCount(Cat((0 until reqNum).map(i => io.queryMQ(i).req.valid && !io.queryMQ(i).req.bits.cancel && ((analysis.strategy(i) & 3.U) === 0.U) && io.queryMQ(i).req.bits.isFromLoad))))
  XSPerfAccumulate("probe_blocked_by_miss", io.probe.block)
  XSPerfAccumulate("prefetch_primary_fire", PopCount(Cat((0 until reqNum).map(i => queryFire(i) && ((analysis.strategy(i) & 1.U) =/= 0.U) && io.queryMQ(i).req.bits.isFromPrefetch))))
  XSPerfAccumulate("prefetch_secondary_fire", PopCount(Cat((0 until reqNum).map(i => queryFire(i) && ((analysis.strategy(i) & 2.U) =/= 0.U) && io.queryMQ(i).req.bits.isFromPrefetch))))
  XSPerfAccumulate("memSetPattenDetected", memSetPattenDetected)
  XSPerfAccumulate("no_free_entry", !ParallelORR(Cat(entries.map(e => e.io.primaryReady))))
  XSPerfAccumulate("free_entry_less_reqNum", PopCount(entries.map(e => e.io.primaryReady)) < reqNum.U)
  val maxInflight = RegInit(0.U((log2Up(cfg.nMissEntries) + 1).W))
  val numValids = PopCount(~Cat(primaryReadyVec).asUInt)
  when (numValids > maxInflight) {
    maxInflight := numValids
  }
  // max inflight (average) = max_inflight_total / cycle cnt
  XSPerfAccumulate("max_inflight", maxInflight)
  QueuePerf(cfg.nMissEntries, numValids, numValids === cfg.nMissEntries.U)
  io.full := numValids === cfg.nMissEntries.U
  io.l1Miss := RegNext(Cat(entries.map(_.io.l1Miss)).orR)
  XSPerfHistogram("num_valids", numValids, true.B, 0, cfg.nMissEntries, 1)

  XSPerfHistogram("L1DMLP_CPUData", PopCount(VecInit(entries.map(_.io.perfPendingNormal)).asUInt), true.B, 0, cfg.nMissEntries, 1)
  XSPerfHistogram("L1DMLP_Prefetch", PopCount(VecInit(entries.map(_.io.perfPendingPrefetch)).asUInt), true.B, 0, cfg.nMissEntries, 1)
  XSPerfHistogram("L1DMLP_Total", numValids, true.B, 0, cfg.nMissEntries, 1)

  XSPerfAccumulate("miss_load_refill_latency", PopCount(entries.map(_.io.latencyMonitor.loadMissRefilling)))
  XSPerfAccumulate("miss_store_refill_latency", PopCount(entries.map(_.io.latencyMonitor.storeMissRefilling)))
  XSPerfAccumulate("miss_amo_refill_latency", PopCount(entries.map(_.io.latencyMonitor.amoMissRefilling)))
  XSPerfAccumulate("miss_pf_refill_latency", PopCount(entries.map(_.io.latencyMonitor.pfMissRefilling)))

  val robHeadMissInDcache = VecInit(entries.map(_.io.robHeadQuery.resp)).asUInt.orR

  entries.foreach {
    case e => {
      e.io.robHeadQuery.queryValid := io.debugTopDown.robHeadVaddr.valid
      e.io.robHeadQuery.vaddr := io.debugTopDown.robHeadVaddr.bits
    }
  }

  io.debugTopDown.robHeadMissInDCache := robHeadMissInDcache

  val perfValidCount = RegNext(PopCount(entries.map(entry => (!entry.io.primaryReady))))
  val queryFireNext = RegNext(queryFire)
  val perfEvents = Seq(
    ("dcache_missq_req      ", PopCount(queryFireNext)),
    ("dcache_missq_1_4_valid", (perfValidCount < (cfg.nMissEntries.U/4.U))),
    ("dcache_missq_2_4_valid", (perfValidCount > (cfg.nMissEntries.U/4.U)) & (perfValidCount <= (cfg.nMissEntries.U/2.U))),
    ("dcache_missq_3_4_valid", (perfValidCount > (cfg.nMissEntries.U/2.U)) & (perfValidCount <= (cfg.nMissEntries.U*3.U/4.U))),
    ("dcache_missq_4_4_valid", (perfValidCount > (cfg.nMissEntries.U*3.U/4.U))),
  )
  generatePerfEvent()
}
