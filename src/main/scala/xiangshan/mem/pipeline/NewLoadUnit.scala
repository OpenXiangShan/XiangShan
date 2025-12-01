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

package xiangshan.mem

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import top.{ArgParser, Generator}
import utils._
import utility._
import xiangshan._
import xiangshan.ExceptionNO._
import xiangshan.backend.Bundles.{DynInst, ExuInput, ExuOutput, MemExuOutput, MemWakeUpBundle, connectSamePort}
import xiangshan.backend.fu.PMPRespBundle
import xiangshan.backend.fu.FuConfig._
import xiangshan.backend.fu.FuType
import xiangshan.backend.ctrlblock.{DebugLsInfoBundle, LsTopdownInfo}
import xiangshan.backend.rob.RobPtr
import xiangshan.backend.ctrlblock.DebugLsInfoBundle
import xiangshan.backend.fu.NewCSR._
import xiangshan.backend.fu.util.SdtrigExt
import xiangshan.backend.exu.ExeUnitParams
import xiangshan.mem.mdp._
import xiangshan.mem.Bundles._
import xiangshan.cache._
import xiangshan.cache.wpu.ReplayCarry
import xiangshan.cache.mmu._

class LoadUnitS0(val param: ExeUnitParams)(implicit p: Parameters) extends XSModule
  with HasL1PrefetchSourceParameter {
  val io = IO(new Bundle() {
    val redirect = Flipped(ValidIO(new Redirect))
    val out = DecoupledIO(new FlowSource)
    /**
      * Request sources in order of priority:
      * 0. unalign tail inject from s1
      * 1. high-priority replay from LRQ, including NC / MMIO replay
      * 2. fast replay from s3
      * 3. low-priority replay from LRQ
      * 4. high-confidence prefetch
      * 5. vector elements splited by VSplit
      * 6. loads issued from IQ
      * 7. low-confidence prefetch
      */
    val unalignTail = Flipped(DecoupledIO(new FlowSource))
    val replay = Flipped(DecoupledIO(new LsPipelineBundle))
    val fastReplay = Flipped(DecoupledIO(new LqWriteBundle))
    // TODO: canAcceptHigh/LowConfPrefetch
    val prefetchReq = Flipped(ValidIO(new L1PrefetchReq))
    val vecldin = Flipped(DecoupledIO(new VecPipeBundle))
    val ldin = Flipped(DecoupledIO(new ExuInput(param, hasCopySrc = true)))

    // Tlb request
    val tlbReq = DecoupledIO(new TlbReq)

    // DCache request
    // TODO: move pf_source, is128Req, replacementUpdated, debug info like s*_pc into req
    val dcacheReq = DecoupledIO(new DCacheWordReq)
    val is128Req = Bool()
    val replacementUpdated = Bool()
    val pfSource = Output(UInt(L1PfSourceBits.W))

    /**
      * Data forward request, including:
      * 1. LSQ / Sbuffer STLF
      * 2. UncacheBuffer
      * 3. MSHR and TileLink-D channel
      */
    val sqSbForwardReq = ValidIO(new StoreForwardReqS0)
    val uncacheForwardReq = ValidIO(new StoreForwardReqS0)
    val dcacheForwardReq = ValidIO(new DCacheForwardReqS0)

    // IQ wakeup
    val wakeup = ValidIO(new MemWakeUpBundle)

    val debugInfo = Output(new Bundle() {
      val isTlbFirstMiss = Bool()
      val isLoadToLoadForward = Bool()
      val robIdx = UInt(log2Ceil(RobSize).W)
      val vaddr = ValidIO(UInt(VAddrBits.W))
      val pc = Output(UInt(VAddrBits.W))
    })
  })

  dontTouch(io)
  io <> DontCare
}

class LoadUnitS1(val param: ExeUnitParams)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val redirect = Flipped(ValidIO(new Redirect))
    val in = Flipped(DecoupledIO(new FlowSource))
    val out = DecoupledIO(new FlowSource)

    // Tlb response
    val tlbResp = Flipped(DecoupledIO(new TlbResp(2))) // TODO: parameterize 2
    val tlbReqKill = Output(Bool()) // TODO: this is ugly
    val tlbPAddr = Output(UInt(PAddrBits.W)) // only used for no_translate

    // DCache request: paddr and s1 kill signal
    val dcachePAddr = Output(UInt(PAddrBits.W))
    val dcacheKill = Output(Bool())

    /**
      * Data forward request and kill
      */
    val storeForwardReq = Output(new StoreForwardReqS1) // including SQ, Sbuffer and Uncache
    val dcacheForwardReq = Output(new DCacheForwardReqS1)
    val sqForwardKill = Output(Bool())
    val sbufferForwardKill = Output(Bool())
    val uncacheForwardKill = Output(Bool())
    val dcacheForwardKill = Output(Bool())

    // Unalign tail inject to s0
    val unalignTail = DecoupledIO(new FlowSource)

    // Nuke check with StoreUnit
    val staNukeQueryReq = Flipped(Vec(StorePipelineWidth, ValidIO(new StoreNukeQueryReq)))

    // Software instruction prefetch
    val swInstrPrefetch = ValidIO(new SoftIfetchPrefetchBundle)

    // Load trigger
    val csrTrigger = Input(new CsrTriggerBundle)

    // Debug info
    val debugInfo = Output(new Bundle() {
      val isTlbFirstMiss = Bool()
      val isLoadToLoadForward = Bool()
      val robIdx = UInt(log2Ceil(RobSize).W)
      val vaddr = ValidIO(UInt(VAddrBits.W))
      val pc = Output(UInt(VAddrBits.W))
    })
  })

  dontTouch(io)
  io <> DontCare
}

class LoadUnitS2(val param: ExeUnitParams)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val redirect = Flipped(ValidIO(new Redirect))
    val in = Flipped(DecoupledIO(new FlowSource))
    val out = DecoupledIO(new FlowSource)

    // PMP result
    val pmp = Flipped(new PMPRespBundle)
    // TLB Hint
    val tlbHint = Flipped(new TlbHintReq)

    // DCache request: s2 kill signal
    val dcacheKill = Output(Bool())

    // DCache response
    val dcacheResp = Flipped(DecoupledIO(new DCacheWordResp))
    // TODO: move this inside of dcacheResp
    val dcacheBankConflict = Input(Bool())
    val dcacheMSHRNack = Input(Bool())

    /**
      * Data forward response
      */
    val sqForwardResp = Flipped(ValidIO(new SQForwardResp))
    val sbufferForwardResp = Flipped(ValidIO(new SbufferForwardResp))
    val uncacheForwardResp = Flipped(ValidIO(new UncacheForwardResp))
    val dcacheForwardResp = Flipped(ValidIO(new DCacheForwardResp)) // TODO: is it necessary to distinguish MSHR and TL-D?

    // Nuke query from StoreUnit
    val staNukeQueryReq = Flipped(Vec(StorePipelineWidth, ValidIO(new StoreNukeQueryReq)))
    // Nuke query to LQRAR / LQRAW
    val rarNukeQueryReq = DecoupledIO(new LoadNukeQueryReq)
    val rawNukeQueryReq = DecoupledIO(new LoadNukeQueryReq)

    // CSR control signals
    val csrCtrl = Flipped(new CustomCSRCtrlIO)

    // Debug info
    val debugInfo = Output(new Bundle() {
      val isBankConflict = Bool()
      val isDCacheMiss = Bool()
      val isDCacheFirstMiss = Bool()
      val isForwardFail = Bool()
      val robIdx = UInt(log2Ceil(RobSize).W)
      val paddr = ValidIO(UInt(PAddrBits.W))
      val pc = Output(UInt(VAddrBits.W))
    })
  })

  dontTouch(io)
  io <> DontCare
}

class LoadUnitS3(val param: ExeUnitParams)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val redirect = Flipped(ValidIO(new Redirect))
    val in = Flipped(DecoupledIO(new FlowSource))

    // DCache response
    val dcacheError = Input(Bool())

    // Writeback to Backend / LQ / VLMergeBuffer
    val ldout = DecoupledIO(new ExuOutput(param))
    val lqWrite = DecoupledIO(new LqWriteBundle)
    val vecldout = Decoupled(new VecPipelineFeedbackIO(isVStore = false))

    // Fast replay
    val fastReplay = DecoupledIO(new LqWriteBundle)

    // RAR / RAW revoke and RAR response
    val rarNukeQueryResp = Flipped(ValidIO(new LoadNukeQueryResp))
    val revoke = Output(Bool())

    /**
      * Rollback and re-fetch from IFU, including:
      * 1. RAR violation
      * 2. vaddr-paddr mismatch happens in STLF
      */
    val rollback = ValidIO(new Redirect)

    // Load cancel
    val cancel = Output(Bool())

    // Uncache data
    val uncacheData = Input(new LoadDataFromLQBundle)
    
    // Prefetch train
    // TODO: this bundle is tooooooo big, define a smaller one
    val prefetchTrain = ValidIO(new LsPrefetchTrainBundle)

    // CSR control signals
    val csrCtrl = Flipped(new CustomCSRCtrlIO)

    // Debug info
    val debugInfo = Output(new Bundle() {
      val isReplayFast = Bool()
      val isReplaySlow = Bool()
      val isReplayRS = Bool()
      val isReplay = Bool()
      val replayCause = Vec(LoadReplayCauses.allCauses, Bool())
      val replayCnt = UInt(XLEN.W)
      val robIdx = UInt(log2Ceil(RobSize).W)
    })
  })

  dontTouch(io)
  io <> DontCare
}

class LoadUnitIO(val param: ExeUnitParams)(implicit p: Parameters) extends XSBundle {
  val redirect = Flipped(ValidIO(new Redirect))
  // Request sources
  val ldin = Flipped(DecoupledIO(new ExuInput(param, hasCopySrc = true)))
  val vecldin = Flipped(DecoupledIO(new VecPipeBundle))
  val replay = Flipped(DecoupledIO(new LsPipelineBundle))
  val prefetchReq = Flipped(ValidIO(new L1PrefetchReq))
  // Writeback to Backend / LQ / VLMergeBuffer
  val ldout = DecoupledIO(new ExuOutput(param))
  val lqWrite = DecoupledIO(new LqWriteBundle)
  val vecldout = Decoupled(new VecPipelineFeedbackIO(isVStore = false))
  // TLB / PMA / PMP
  val tlb = new TlbRequestIO(2)
  val tlbHint = Flipped(new TlbHintReq)
  val pmp = Flipped(new PMPRespBundle)
  // DCache
  val dcache = new DCacheLoadIO
  // IQ wakeup and load cancel
  val wakeup = ValidIO(new MemWakeUpBundle)
  val cancel = Output(Bool())
  // Data forwarding
  val sqForward = new SQForward
  val sbufferForward = new SbufferForward
  val uncacheForward = new UncacheForward
  val dcacheForward = new DCacheForward
  // Uncache data
  val uncacheData = Input(new LoadDataFromLQBundle)
  // Nuke check with StoreUnit
  val staNukeQueryReq = Flipped(Vec(StorePipelineWidth, ValidIO(new StoreNukeQueryReq)))
  // Nuke check with RAR / RAW
  val rarNukeQuery = new LoadRARNukeQuery
  val rawNukeQuery = new LoadRAWNukeQuery
  val rollback = ValidIO(new Redirect)
  // Prefetch Train
  val prefetchTrain = ValidIO(new LsPrefetchTrainBundle)
  // Software instruction prefetch
  val swInstrPrefetch = ValidIO(new SoftIfetchPrefetchBundle)
  // CSR control signals and load trigger
  val csrCtrl = Flipped(new CustomCSRCtrlIO)
  val csrTrigger = Input(new CsrTriggerBundle)
  // Debug info
  val debugInfo = Output(new DebugLsInfoBundle)
}

class NewLoadUnit(val param: ExeUnitParams)(implicit p: Parameters) extends XSModule {
  val io = IO(new LoadUnitIO(param))

  val s0 = Module(new LoadUnitS0(param))
  val s1 = Module(new LoadUnitS1(param))
  val s2 = Module(new LoadUnitS2(param))
  val s3 = Module(new LoadUnitS3(param))

  // Internal wiring
  s1.io.in <> s0.io.out
  s2.io.in <> s1.io.out
  s3.io.in <> s2.io.out
  s0.io.unalignTail <> s1.io.unalignTail
  s0.io.fastReplay <> s3.io.fastReplay

  // IO wiring
  // S0
  s0.io.redirect := io.redirect
  s0.io.replay <> io.replay
  s0.io.prefetchReq <> io.prefetchReq
  s0.io.vecldin <> io.vecldin
  s0.io.ldin <> io.ldin
  io.tlb.req <> s0.io.tlbReq
  io.dcache.req <> s0.io.dcacheReq
  io.dcache.is128Req := s0.io.is128Req
  io.dcache.replacementUpdated := s0.io.replacementUpdated
  io.dcache.pf_source := s0.io.pfSource
  io.sqForward.s0Req := s0.io.sqSbForwardReq
  io.sbufferForward.s0Req := s0.io.sqSbForwardReq
  io.uncacheForward.s0Req := s0.io.uncacheForwardReq
  io.dcacheForward.s0Req := s0.io.dcacheForwardReq
  io.wakeup := s0.io.wakeup

  // S1
  s1.io.redirect := io.redirect
  s1.io.tlbResp <> io.tlb.resp
  io.tlb.req_kill := s1.io.tlbReqKill
  io.tlb.req.bits.pmp_addr := s1.io.tlbPAddr // TODO
  io.dcache.s1_paddr_dup_lsu := s1.io.dcachePAddr
  io.dcache.s1_paddr_dup_dcache := s1.io.dcachePAddr
  io.dcache.s1_kill := s1.io.dcacheKill
  io.sqForward.s1Req := s1.io.storeForwardReq
  io.sqForward.s1Kill := s1.io.sqForwardKill
  io.sbufferForward.s1Req := s1.io.storeForwardReq
  io.sbufferForward.s1Kill := s1.io.sbufferForwardKill
  io.uncacheForward.s1Req := s1.io.storeForwardReq
  io.uncacheForward.s1Kill := s1.io.uncacheForwardKill
  io.dcacheForward.s1Req := s1.io.dcacheForwardReq
  io.dcacheForward.s1Kill := s1.io.dcacheForwardKill
  s1.io.staNukeQueryReq := io.staNukeQueryReq
  io.swInstrPrefetch := s1.io.swInstrPrefetch
  s1.io.csrTrigger := io.csrTrigger

  // S2
  s2.io.redirect := io.redirect
  s2.io.pmp := io.pmp
  s2.io.tlbHint := io.tlbHint
  io.dcache.s2_kill := s2.io.dcacheKill
  s2.io.dcacheResp <> io.dcache.resp
  s2.io.dcacheBankConflict := io.dcache.s2_bank_conflict
  s2.io.dcacheMSHRNack := io.dcache.s2_mq_nack
  s2.io.sqForwardResp := io.sqForward.s2Resp
  s2.io.sbufferForwardResp := io.sbufferForward.s2Resp
  s2.io.uncacheForwardResp := io.uncacheForward.s2Resp
  s2.io.dcacheForwardResp := io.dcacheForward.s2Resp
  s2.io.staNukeQueryReq := io.staNukeQueryReq
  io.rarNukeQuery.req <> s2.io.rarNukeQueryReq
  io.rawNukeQuery.req <> s2.io.rawNukeQueryReq
  s2.io.csrCtrl := io.csrCtrl

  // S3
  s3.io.redirect := io.redirect
  s3.io.dcacheError := io.dcache.resp.bits.error_delayed
  io.ldout <> s3.io.ldout
  io.lqWrite <> s3.io.lqWrite
  io.vecldout <> s3.io.vecldout
  s3.io.rarNukeQueryResp := io.rarNukeQuery.resp
  io.rarNukeQuery.revoke := s3.io.revoke
  io.rawNukeQuery.revoke := s3.io.revoke
  io.rollback := s3.io.rollback
  io.cancel := s3.io.cancel
  s3.io.uncacheData := RegNextN(io.uncacheData, 3) // TODO
  io.prefetchTrain := s3.io.prefetchTrain
  s3.io.csrCtrl := io.csrCtrl

  // Debug info
  io.debugInfo.s1_isTlbFirstMiss := s1.io.debugInfo.isTlbFirstMiss
  io.debugInfo.s1_isLoadToLoadForward := s1.io.debugInfo.isLoadToLoadForward
  io.debugInfo.s2_isBankConflict := s2.io.debugInfo.isBankConflict
  io.debugInfo.s2_isDcacheFirstMiss := s2.io.debugInfo.isDCacheMiss
  io.debugInfo.s2_isForwardFail := s2.io.debugInfo.isForwardFail
  io.debugInfo.s3_isReplayFast := s3.io.debugInfo.isReplayFast
  io.debugInfo.s3_isReplaySlow := s3.io.debugInfo.isReplaySlow
  io.debugInfo.s3_isReplayRS := s3.io.debugInfo.isReplayRS
  io.debugInfo.s3_isReplay := s3.io.debugInfo.isReplay
  io.debugInfo.replayCause := s3.io.debugInfo.replayCause
  io.debugInfo.replayCnt := s3.io.debugInfo.replayCnt
  io.debugInfo.s1_robIdx := s1.io.debugInfo.robIdx
  io.debugInfo.s2_robIdx := s2.io.debugInfo.robIdx
  io.debugInfo.s3_robIdx := s3.io.debugInfo.robIdx

  io.dcache.s0_pc := s0.io.debugInfo.pc
  io.dcache.s1_pc := s1.io.debugInfo.pc
  io.dcache.s2_pc := s2.io.debugInfo.pc
}

class FlowSource(implicit p: Parameters) extends XSBundle
  with HasDCacheParameters
  with HasVLSUParameters {
  val vaddr         = UInt(VAddrBits.W)
  val mask          = UInt((VLEN/8).W)
  val uop           = new DynInst
  val has_rob_entry = Bool()
  val rep_carry     = new ReplayCarry(nWays)
  val mshrid        = UInt(log2Up(cfg.nMissEntries).W)
  val isFirstIssue  = Bool()
  val fast_rep      = Bool()
  val ld_rep        = Bool()
  val prf           = Bool()
  val prf_rd        = Bool()
  val prf_wr        = Bool()
  val prf_i         = Bool()
  val sched_idx     = UInt(log2Up(LoadQueueReplaySize+1).W)
  // Record the issue port idx of load issue queue. This signal is used by load cancel.
  val deqPortIdx    = UInt(log2Ceil(LoadPipelineWidth).W)
  val frm_mabuf     = Bool()
  // vec only
  val isvec         = Bool()
  val is128bit      = Bool()
  val uop_unit_stride_fof = Bool()
  val reg_offset    = UInt(vOffsetBits.W)
  val vecActive     = Bool() // 1: vector active element or scala mem operation, 0: vector not active element
  val is_first_ele  = Bool()
  // val flowPtr       = new VlflowPtr
  val usSecondInv   = Bool()
  val mbIndex       = UInt(vlmBindexBits.W)
  val elemIdx       = UInt(elemIdxBits.W)
  val elemIdxInsideVd = UInt(elemIdxBits.W)
  val alignedType   = UInt(alignTypeBits.W)
  val vecBaseVaddr  = UInt(VAddrBits.W)
  //for Svpbmt NC
  val isnc          = Bool()
  val paddr         = UInt(PAddrBits.W)
  val data          = UInt((VLEN+1).W)
}

/**
  * Only for compiling the module independently
  */
class NewLoadUnitTop(implicit val p: Parameters) extends Module
  with HasXSParameter
  with HasMemBlockParameters {
  val param = ldaParams.head
  param.bindBackendParam(backendParams)
  val io = IO(new LoadUnitIO(param))
  val ldu = Module(new NewLoadUnit(param))
  io <> ldu.io
}

object NewLoadUnitMain extends App {
  val (config, firrtlOpts, firtoolOpts) = ArgParser.parse(
    args :+ "--disable-always-basic-diff" :+ "--dump-fir" :+ "--fpga-platform" :+ "--target" :+ "verilog")

  val defaultConfig = config.alterPartial({
    // Get XSCoreParams and pass it to the "small module"
    case XSCoreParamsKey => config(XSTileKey).head
  })

  Generator.execute(
    firrtlOpts,
    new NewLoadUnitTop()(defaultConfig),
    firtoolOpts
  )

  println("done")
}