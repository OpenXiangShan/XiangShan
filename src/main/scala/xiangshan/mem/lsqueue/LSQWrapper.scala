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
import utils._
import utility._
import xiangshan._
import xiangshan.backend.Bundles.{DynInst, ExuOutput, IssueQueueLRQWakeUpBundle, MemExuOutput, MemToRob, UopIdx, connectSamePort}
import xiangshan.backend._
import xiangshan.backend.rob.{RobLsqIO, RobPtr}
import xiangshan.backend.fu.FuType
import xiangshan.backend.fu.vector.Bundles.NumLsElem
import xiangshan.mem.Bundles._
import xiangshan.cache._
import xiangshan.cache.{DCacheLineIO, DCacheWordIO, MemoryOpConstants}
import xiangshan.cache.{CMOReq, CMOResp}
import xiangshan.cache.mmu.{TlbHintIO, TlbRequestIO}

class ExceptionAddrIO(implicit p: Parameters) extends XSBundle {
  val isStore = Input(Bool())
  val vaddr = Output(UInt(XLEN.W))
  val vaNeedExt = Output(Bool())
  val isHyper = Output(Bool())
  val vstart = Output(UInt((log2Up(VLEN) + 1).W))
  val vl = Output(UInt((log2Up(VLEN) + 1).W))
  val gpaddr = Output(UInt(XLEN.W))
  val isForVSnonLeafPTE = Output(Bool())
}

class FwdEntry extends Bundle {
  val validFast = Bool() // validFast is generated the same cycle with query
  val valid = Bool() // valid is generated 1 cycle after query request
  val data = UInt(8.W) // data is generated 1 cycle after query request
}

// inflight miss block reqs
class InflightBlockInfo(implicit p: Parameters) extends XSBundle {
  val block_addr = UInt(PAddrBits.W)
  val valid = Bool()
}

class LsqEnqIO(implicit p: Parameters) extends MemBlockBundle {
  val canAccept = Output(Bool())
  val recoverStall = Output(Bool())
  val needAlloc = Vec(LSQEnqWidth, Input(UInt(2.W)))
  val req       = Vec(LSQEnqWidth, Flipped(ValidIO(new Bundle {
    val uop       = new DynInst
    val reqEndPtr = new LSIdx // uop end lqIdx/sqIdx
    val reqStartPtr = new LSIdx
  })))

  val iqAccept  = Input(Vec(LSQEnqWidth, Bool()))
  val resp      = Vec(LSQEnqWidth, Output(new LSIdx))
}

class LsqEnqCtrlReqIO(implicit p: Parameters) extends MemBlockBundle {
  val num       = NumLsElem()
  val fuType    = FuType()
}

class LsqPtrPreCalculateIO(implicit p: Parameters) extends MemBlockBundle {
  val req          = Vec(LSQEnqWidth, Flipped(ValidIO(new LsqEnqCtrlReqIO)))
  val respStartPtr = Vec(LSQEnqWidth, Output(new LSIdx)) // uop start lqIdx/sqIdx
  val respEndPtr   = Vec(LSQEnqWidth, Output(new LSIdx)) // uop end lqIdx/sqIdx
  val canAccept    = Output(Bool())
}

// Load / Store Queue Wrapper for XiangShan Out of Order LSU
class LsqWrapper(implicit p: Parameters) extends XSModule
  with HasDCacheParameters
  with HasMemBlockParameters
  with HasPerfEvents {
  val io = IO(new Bundle() {
    val hartId = Input(UInt(hartIdLen.W))
    val brqRedirect = Flipped(ValidIO(new Redirect))
    val stvecFeedback = Vec(VecStorePipelineWidth, Flipped(ValidIO(new FeedbackToLsqIO)))
    val ldvecFeedback = Vec(VecLoadPipelineWidth, Flipped(ValidIO(new FeedbackToLsqIO)))
    val enq = new LsqEnqIO
    val ldu = new Bundle() {
      val rawNukeQuery = Vec(LoadPipelineWidth, Flipped(new LoadRAWNukeQuery()))
      val rarNukeQuery = Vec(LoadPipelineWidth, Flipped(new LoadRARNukeQuery()))
      val ldin = Vec(LoadPipelineWidth, Flipped(Decoupled(new LqWriteBundle))) // from load_s3
    }
    val sta = new Bundle() {
      val storeAddrIn = Vec(StorePipelineWidth, Flipped(Valid(new StoreAddrIO))) // from store_s1
      val storeAddrInRe = Vec(StorePipelineWidth, Input(new StoreAddrIO)) // from store_s2
      // ready indicate unaligned queue reject this unaligned request
      val unalignQueueReq = Vec(StorePipelineWidth, Flipped(DecoupledIO(new UnalignQueueIO)))
    }
    val std = new Bundle() {
      val storeDataIn = Vec(StorePipelineWidth, Flipped(Valid(new StoreQueueDataWrite))) // from store_s0, store data, send to sq from rs
    }
    val bypass = Flipped(Vec(LoadPipelineWidth, new UncacheBypass))
    val replay = Vec(LoadPipelineWidth, Decoupled(new LoadReplayIO))
    val sbuffer = new SbufferWriteIO
    val forward = Flipped(Vec(LoadPipelineWidth, new SQForward))
    val rob = Flipped(new RobLsqIO)
    val nuke_rollback = Vec(StorePipelineWidth, Output(Valid(new Redirect)))
    val nack_rollback = Vec(1, Output(Valid(new Redirect))) // uncahce
    // mdp train io
    val mdpTrain        = ValidIO(new Redirect)
    val release = Flipped(Valid(new Release))
    val loadWakeup = Flipped(Vec(cfg.numMemChannels, ValidIO(new DCacheLoadWakeup())))
   // val refill = Flipped(Valid(new Refill))
    // val tl_d_channel  = Input(Vec(cfg.numMemChannels, new DcacheToLduForwardIO))
    // val maControl     = Flipped(new StoreMaBufToSqControlIO)
    val uncacheOutstanding = Input(Bool())
    val uncache = new UncacheWordIO
    val mmioStout = DecoupledIO(new MemToRob(staParams.head)) // writeback uncached store
    val sqEmpty = Output(Bool())
    val lq_rep_full = Output(Bool())
    val sqFull = Output(Bool())
    val lqFull = Output(Bool())
    val toLsqEnqCtrl = new ToLsqEnqCtrl(hasStore = true, hasLoad = true)
    val lqCanAccept = Output(Bool())
    val sqCanAccept = Output(Bool())
    val lqDeqPtr = Output(new LqPtr)
    val sqDeqPtr = Output(new SqPtr)
    val issuePtrExt = Output(new SqPtr)
    val l2_hint = Input(Vec(cfg.numMemChannels, Valid(new L2ToL1Hint())))
    val tlb_hint = Flipped(new TlbHintIO)
    val wakeupToLRQ = Vec(StaCnt + StdCnt, Flipped(ValidIO(new IssueQueueLRQWakeUpBundle)))
    val wakeupToLRQCancel = Input(Vec(StaCnt + StdCnt, new LRQWakeUpCancelBundle))
    val cmoOpReq  = DecoupledIO(new CMOReq)
    val cmoOpResp = Flipped(DecoupledIO(new CMOResp))
    val flushSbuffer = new SbufferFlushBundle
    val force_write = Output(Bool())
    val physicalStoreQueueFull = Output(Bool())
    val lqEmpty = Output(Bool())
    val rarValidCount = Output(UInt())
    val wfi = Flipped(new WfiReqBundle)
    val stExceptionInfo = ValidIO(new MemExceptionInfo)
    val ldExceptionInfo = ValidIO(new MemExceptionInfo)
    // top-down
    val debugTopDown = new LoadQueueTopDownIO
    val noUopsIssued = Input(Bool())

    val diffStore = OptionWrapper(debugEn, Flipped(new DiffStoreIO))
  })

  val loadQueue = Module(new LoadQueue)
  val storeQueue = Module(new StoreQueue)

  storeQueue.io.hartId := io.hartId
  storeQueue.io.wfi <> io.wfi

  if (backendParams.debugEn){ dontTouch(loadQueue.io.tlbReplayDelayCycleCtrl) }
  // Todo: imm
  val tlbReplayDelayCycleCtrl = WireInit(VecInit(Seq(14.U(ReSelectLen.W), 0.U(ReSelectLen.W), 125.U(ReSelectLen.W), 0.U(ReSelectLen.W))))
  loadQueue.io.tlbReplayDelayCycleCtrl := tlbReplayDelayCycleCtrl

  // io.enq logic
  // LSQ: send out canAccept when both load queue and store queue are ready
  // Dispatch: send instructions to LSQ only when they are ready
  io.enq.canAccept := loadQueue.io.enq.canAccept && storeQueue.io.enq.canAccept
  io.enq.recoverStall := false.B // do not used here
  io.lqCanAccept := loadQueue.io.enq.canAccept
  io.sqCanAccept := storeQueue.io.enq.canAccept
  loadQueue.io.enq.sqCanAccept := storeQueue.io.enq.canAccept
  io.lqDeqPtr := loadQueue.io.lqDeqPtr
  io.sqDeqPtr := storeQueue.io.sqDeqPtr
  io.rarValidCount := loadQueue.io.rarValidCount
  for (i <- io.enq.req.indices) {
    loadQueue.io.enq.needAlloc(i)      := io.enq.needAlloc(i)(0)
    loadQueue.io.enq.req(i).valid      := io.enq.needAlloc(i)(0) && io.enq.req(i).valid
    loadQueue.io.enq.req(i).bits       := io.enq.req(i).bits.uop
    loadQueue.io.enq.req(i).bits.sqIdx := storeQueue.io.enq.resp(i).physicalQueuePtr
    loadQueue.io.enq.req(i).bits.lqIdx := io.enq.req(i).bits.reqStartPtr.lqIdx

    storeQueue.io.enq.req(i).bits.needAlloc := io.enq.needAlloc(i)(1)
    storeQueue.io.enq.req(i).bits.reqEndPtr := io.enq.req(i).bits.reqEndPtr
    storeQueue.io.enq.req(i).bits.reqStartPtr := io.enq.req(i).bits.reqStartPtr
    storeQueue.io.enq.req(i).valid          := io.enq.needAlloc(i)(1) && io.enq.req(i).valid
    connectSamePort(storeQueue.io.enq.req(i).bits.uop, io.enq.req(i).bits.uop)
    storeQueue.io.enq.req(i).bits.uop.isVec := FuType.isVStore(io.enq.req(i).bits.uop.fuType)
    // only enable difftest, it will be used.
    if(env.EnableDifftest){
      storeQueue.io.enq.req(i).bits.debugUop.get := io.enq.req(i).bits.uop
    }
//    storeQueue.io.enq.req(i).bits.lqIdx := loadQueue.io.enq.resp(i) // TODO: need it ?

    io.enq.resp(i).lqIdx := loadQueue.io.enq.resp(i)
    io.enq.resp(i).sqIdx := storeQueue.io.enq.resp(i).physicalQueuePtr
  }

  // store queue wiring
  storeQueue.io.redirect                      <> io.brqRedirect
  storeQueue.io.fromVMergeBuffer              <> io.stvecFeedback
  storeQueue.io.fromStoreUnit.unalignQueueReq <> io.sta.unalignQueueReq
  storeQueue.io.fromStoreUnit.storeAddrIn     <> io.sta.storeAddrIn // from store_s1
  storeQueue.io.fromStoreUnit.storeAddrInRe   <> io.sta.storeAddrInRe // from store_s2
  storeQueue.io.storeDataIn                   <> io.std.storeDataIn // from store_s0
  storeQueue.io.writeToSbuffer                <> io.sbuffer
  storeQueue.io.writeBack                     <> io.mmioStout
  storeQueue.io.fromRob.robHeadPtr            := io.rob.pendingPtr
  storeQueue.io.exceptionInfo                 <> io.stExceptionInfo
  storeQueue.io.sqEmpty                       <> io.sqEmpty
  storeQueue.io.sqFull                        <> io.sqFull
  storeQueue.io.forward                       <> io.forward // overlap forwardMask & forwardData, DO NOT CHANGE SEQUENCE
  io.force_write                              := storeQueue.io.sbufferCtrl.req.forceWrite
  io.physicalStoreQueueFull                   := storeQueue.io.sbufferCtrl.req.physicalStoreQueueFull
  storeQueue.io.toDCache.req                  <> io.cmoOpReq
  storeQueue.io.toDCache.resp                 <> io.cmoOpResp
  io.flushSbuffer.valid                       := storeQueue.io.sbufferCtrl.req.flush
  io.flushSbuffer.isCmo                       := storeQueue.io.sbufferCtrl.req.flush
  storeQueue.io.sbufferCtrl.resp.empty        := io.flushSbuffer.empty
  io.toLsqEnqCtrl.sqRedirectPtr.foreach(_ := storeQueue.io.toLsqEnqCtrl.sqRedirectPtr.get)
  io.toLsqEnqCtrl.sqDeq.foreach(_ := storeQueue.io.toLsqEnqCtrl.sqDeq.get)
  io.toLsqEnqCtrl.sqRecoverStall.foreach(_ := storeQueue.io.toLsqEnqCtrl.sqRecoverStall.get)
  io.diffStore.foreach{ case sink =>
    storeQueue.io.diffStore.foreach(sink := _)
  }

  /* <------- DANGEROUS: Don't change sequence here ! -------> */

  //  load queue wiring
  loadQueue.io.redirect            <> io.brqRedirect
  loadQueue.io.vecFeedback         <> io.ldvecFeedback
  loadQueue.io.ldu                 <> io.ldu
  loadQueue.io.rob.pendingPtr      := io.rob.pendingPtr
  loadQueue.io.rob.pendingPtrNext  := io.rob.pendingPtrNext
  loadQueue.io.rob.lcommit         := io.rob.lcommit
  loadQueue.io.rob.scommit         := io.rob.scommit
  loadQueue.io.rob.commit          := io.rob.commit
  loadQueue.io.nuke_rollback       <> io.nuke_rollback
  loadQueue.io.nack_rollback       <> io.nack_rollback
  loadQueue.io.replay              <> io.replay
  loadQueue.io.loadWakeup          <> io.loadWakeup
  loadQueue.io.release             <> io.release
  loadQueue.io.exceptionInfo       <> io.ldExceptionInfo
  loadQueue.io.sq.stAddrReadySqPtr <> storeQueue.io.toLoadQueue.stAddrReadySqPtr
  loadQueue.io.sq.stAddrReadyVec   <> storeQueue.io.toLoadQueue.stAddrReadyVec
  loadQueue.io.sq.stDataReadySqPtr <> storeQueue.io.toLoadQueue.stDataReadySqPtr
  loadQueue.io.sq.stDataReadyVec   <> storeQueue.io.toLoadQueue.stDataReadyVec
  loadQueue.io.sq.sqEmpty          <> storeQueue.io.sqEmpty
  loadQueue.io.sq.sqDeqPtr         <> storeQueue.io.sqDeqPtr
  loadQueue.io.sta.storeAddrIn     <> io.sta.storeAddrIn // store_s1
  loadQueue.io.std.storeDataIn     <> io.std.storeDataIn // store_s0
  loadQueue.io.lqFull              <> io.lqFull
  loadQueue.io.lq_rep_full         <> io.lq_rep_full
  loadQueue.io.bypass              <> io.bypass
  loadQueue.io.l2_hint             <> io.l2_hint
  loadQueue.io.tlb_hint            <> io.tlb_hint
  loadQueue.io.wakeupToLRQ         <> io.wakeupToLRQ
  loadQueue.io.wakeupToLRQCancel   := io.wakeupToLRQCancel
  loadQueue.io.lqEmpty             <> io.lqEmpty
  loadQueue.io.sq.physicalUpperSqIdx <> storeQueue.io.physicalUpperSqIdx
  io.mdpTrain                      := loadQueue.io.mdpTrain

  io.toLsqEnqCtrl.lqRedirectPtr.foreach(_ := loadQueue.io.lqRedirect)
  io.toLsqEnqCtrl.lqDeq.foreach(_ := loadQueue.io.lqDeq)
  io.toLsqEnqCtrl.lqRecoverStall.foreach(_ := loadQueue.io.lqRecoverStall)

  io.issuePtrExt := storeQueue.io.toLoadQueue.stAddrReadySqPtr

  // to rob
  io.rob.mmioBusy                  := RegNext(storeQueue.io.toRob.mmioBusy || loadQueue.io.rob.mmioBusy)
  // naive uncache arbiter
  val s_idle :: s_load :: s_store :: Nil = Enum(3)
  val pendingstate = RegInit(s_idle)
  val selectLq = (loadQueue.io.uncache.req.valid && !storeQueue.io.toUncacheBuffer.req.valid) || (
    loadQueue.io.uncache.req.valid && storeQueue.io.toUncacheBuffer.req.valid &&
    loadQueue.io.uncache.req.bits.robIdx < storeQueue.io.toUncacheBuffer.req.bits.robIdx
  )

  switch(pendingstate){
    is(s_idle){
      when(io.uncache.req.fire){
        pendingstate :=
          Mux(io.uncacheOutstanding && io.uncache.req.bits.nc, s_idle,
          Mux(selectLq, s_load,
          s_store))
      }
    }
    is(s_load){
      when(io.uncache.resp.fire){
        pendingstate := s_idle
      }
    }
    is(s_store){
      when(io.uncache.resp.fire){
        pendingstate := s_idle
      }
    }
  }

  loadQueue.io.uncache := DontCare
  storeQueue.io.toUncacheBuffer := DontCare
  loadQueue.io.uncache.req.ready := false.B
  storeQueue.io.toUncacheBuffer.req.ready := false.B
  loadQueue.io.uncache.resp.valid := false.B
  loadQueue.io.uncache.idResp.valid := false.B
  storeQueue.io.toUncacheBuffer.resp.valid := false.B
  storeQueue.io.toUncacheBuffer.idResp.valid := false.B
  when(pendingstate === s_idle){
    when(selectLq){
      io.uncache.req <> loadQueue.io.uncache.req
    }.otherwise{
      io.uncache.req <> storeQueue.io.toUncacheBuffer.req
    }
  }.otherwise{
    io.uncache.req.valid := false.B
    io.uncache.req.bits := DontCare
  }
  when (io.uncache.resp.bits.is2lq) {
    io.uncache.resp <> loadQueue.io.uncache.resp
  } .otherwise {
    io.uncache.resp <> storeQueue.io.toUncacheBuffer.resp
  }
  when(io.uncache.idResp.bits.is2lq) {
    loadQueue.io.uncache.idResp <> io.uncache.idResp
  }.otherwise {
    storeQueue.io.toUncacheBuffer.idResp <> io.uncache.idResp
  }

  loadQueue.io.debugTopDown <> io.debugTopDown
  loadQueue.io.noUopsIssed := io.noUopsIssued

  assert(!(loadQueue.io.uncache.resp.valid && storeQueue.io.toUncacheBuffer.resp.valid))
  assert(!(loadQueue.io.uncache.idResp.valid && storeQueue.io.toUncacheBuffer.idResp.valid))
  when (!io.uncacheOutstanding) {
    assert(!((loadQueue.io.uncache.resp.valid || storeQueue.io.toUncacheBuffer.resp.valid) && pendingstate === s_idle))
  }

  // debug
  for (i <- 0 until io.enq.req.length) {
    XSError(io.enq.req(i).valid && io.enq.needAlloc(i)(1) && !storeQueue.io.enq.canAccept, s"storeQueue must accept $i\n")
    XSError(io.enq.req(i).valid && io.enq.needAlloc(i)(0) && !loadQueue.io.enq.canAccept, s"loadQueue must accept $i\n")
  }
  val perfEvents = Seq(loadQueue, storeQueue).flatMap(_.getPerfEvents)
  generatePerfEvent()
}

class LsqEnqCtrl(implicit p: Parameters) extends XSModule
  with HasVLSUParameters with HasMultiFlagCircularQueuePtrHelper {
  val io = IO(new Bundle {
    val redirect = Flipped(ValidIO(new Redirect))
    // from rename, pre-calculate lqPtr && sqPtr
    val fromRename = new LsqPtrPreCalculateIO
    // from dispatch
    val fromDispatch = new LsqEnqIO
    // from `memBlock.io.lqDeq
    val lqDeq = Flipped(ValidIO(UInt(log2Up(CommitWidth + 1).W)))
    // from `memBlock.io.sqDeq`
    val sqDeq = Flipped(ValidIO(UInt(log2Ceil(EnsbufferWidth + 1).W)))
    // from/tp lsq
    val lqRedirectPtr = Flipped(ValidIO(new LqPtr))
    val sqRedirectPtr = Flipped(ValidIO(new SqPtr))
    val toDispatch = Output(new LSIdx)
    val enqLsq = Flipped(new LsqEnqIO)
    val lqRecoverStall = Input(Bool())
    val sqRecoverStall = Input(Bool())
    // for topdown
    val lqStall = OptionWrapper(backendParams.debugEn, Output(Bool()))
    val sqStall = OptionWrapper(backendParams.debugEn, Output(Bool()))
  })

  protected object State extends ChiselEnum {
    val idle = Value
    val waitLsqRecover = Value // wait lsq recover pointer
  }

  private val lqDeq = Mux(io.lqDeq.valid, io.lqDeq.bits, 0.U)
  private val sqDeq = Mux(io.sqDeq.valid, io.sqDeq.bits, 0.U)

  val lqHeadPtr = RegInit(0.U.asTypeOf(new LqPtr))
  val sqHeadPtr = RegInit(0.U.asTypeOf(new SqPtr))
  val lqTailPtr = RegInit(0.U.asTypeOf(new LqPtr))
  val sqTailPtr = RegInit(0.U.asTypeOf(new SqPtr))

  val state = RegInit(State.idle)
  val stateNext = WireInit(state)

  state := stateNext

  switch(state) {
    is(State.idle) {
      when(RegNext(io.redirect.valid)) {
        stateNext := State.waitLsqRecover
      }
    }
    is(State.waitLsqRecover) {
      when(RegNext(io.redirect.valid)) {
        stateNext := State.waitLsqRecover
      }.elsewhen(!io.lqRecoverStall && !io.sqRecoverStall) { // lqRecoverStall/sqRecoverStall is state driven signal.
        stateNext := State.idle
      }
    }
  }

  XSError(!io.lqRecoverStall && io.lqRedirectPtr.valid, "loadQueue's timing of recover and stall error.\n")
  XSError(!io.sqRecoverStall && io.sqRedirectPtr.valid, "storeQueue's timing of recover and stall error.\n")

  // rename pre-calculate sqPtr/lqPtr
  val numLsElem = io.fromRename.req.map(_.bits.num)
  val needEnqLoadQueue = VecInit(io.fromRename.req.map(x => x.valid && (FuType.isLoad(x.bits.fuType) || FuType.isVNonsegLoad(x.bits.fuType))))
  val needEnqStoreQueue = VecInit(io.fromRename.req.map(x => x.valid && (FuType.isStore(x.bits.fuType) || FuType.isVNonsegStore(x.bits.fuType))))
  val loadQueueElem = needEnqLoadQueue.zip(numLsElem).map(x => Mux(x._1, x._2, 0.U))
  val storeQueueElem = needEnqStoreQueue.zip(numLsElem).map(x => Mux(x._1, x._2, 0.U))
  val loadFlowPopCount = 0.U +: loadQueueElem.zipWithIndex.map{ case (l, i) =>
    loadQueueElem.take(i + 1).reduce(_ +& _).asTypeOf(UInt(elemIdxBits.W))
  }
  val storeFlowPopCount = 0.U +: storeQueueElem.zipWithIndex.map { case (s, i) =>
    storeQueueElem.take(i + 1).reduce(_ +& _).asTypeOf(UInt(elemIdxBits.W))
  }
  val lqAllocNumber = loadQueueElem.reduce(_ +& _)
  val sqAllocNumber = storeQueueElem.reduce(_ +& _)

  io.toDispatch.lqIdx := lqHeadPtr
  io.toDispatch.sqIdx := sqHeadPtr

  for ((resp, i) <- io.fromRename.respStartPtr.zipWithIndex) { // start ptr calculate
    val lqOffset = loadFlowPopCount(i)
    resp.lqIdx := lqTailPtr.addWrapCircles(lqOffset)
    val sqOffset = storeFlowPopCount(i)
    resp.sqIdx := sqTailPtr + sqOffset
  }

  for ((resp, i) <- io.fromRename.respEndPtr.zipWithIndex) { // end ptr calculate
    if(i == io.fromRename.respEndPtr.length - 1) {
      val lqOffset = loadFlowPopCount(i)
      resp.lqIdx := lqTailPtr.addWrapCircles(lqOffset + loadQueueElem(i))
      val sqOffset = storeFlowPopCount(i)
      resp.sqIdx := sqTailPtr + sqOffset + storeQueueElem(i)
    }
    else {
      resp.lqIdx := io.fromRename.respStartPtr(i + 1).lqIdx // last's end is next's start.
      resp.sqIdx := io.fromRename.respStartPtr(i + 1).sqIdx
    }
  }

  io.fromRename.canAccept := state === State.idle

  // How to update ptr:
  // (1) by default, updated according to commit
  // (2) when redirect, update according to lsq

  // lqRedirect will arrive after 4 cycle of io.redirect.
  val lqTailPtrNext = Mux(io.lqRedirectPtr.valid,
    io.lqRedirectPtr.bits,
    Mux(io.redirect.valid,
      lqTailPtr,
      lqTailPtr.addWrapCircles(lqAllocNumber)
    )
  )
  // sqRedirect will arrive after 3 -> (3 + virtual store queue walk cycles) cycle of io.redirect.
  val sqTailPtrNext = Mux(io.sqRedirectPtr.valid,
    io.sqRedirectPtr.bits,
    Mux(io.redirect.valid,
      sqTailPtr,
      sqTailPtr + sqAllocNumber
    )
  )

  lqTailPtr := lqTailPtrNext
  sqTailPtr := sqTailPtrNext

  lqHeadPtr := lqHeadPtr + lqDeq
  sqHeadPtr := sqHeadPtr + sqDeq

  XSError(io.lqRedirectPtr.valid && io.lqRedirectPtr.bits < lqHeadPtr && !lqHeadPtr.isRotateBy(io.lqRedirectPtr.bits),
    s"loadQueue redirect error!\n")
  XSError(io.sqRedirectPtr.valid && io.sqRedirectPtr.bits < sqHeadPtr && !sqHeadPtr.isRotateBy(io.sqRedirectPtr.bits),
    s"storeQueue redirect error!\n")

  // dispatch logic
  val lqCanAccept = lqTailPtr >= lqHeadPtr && state === State.idle
  val sqCanAccept = sqTailPtr >= sqHeadPtr && state === State.idle
  val redirectUpdate = io.lqRedirectPtr.valid || io.sqRedirectPtr.valid
  io.fromDispatch.canAccept := RegNext(lqCanAccept && sqCanAccept && !redirectUpdate)

  // only for debug
  for ((resp, i) <- io.fromDispatch.resp.zipWithIndex) {
    resp.lqIdx := io.fromDispatch.req(i).bits.reqStartPtr.lqIdx
    resp.sqIdx := io.fromDispatch.req(i).bits.reqStartPtr.sqIdx
  }

  // to lsq
  io.enqLsq.needAlloc := RegNext(io.fromDispatch.needAlloc)
  io.enqLsq.iqAccept := RegNext(io.fromDispatch.iqAccept)
  io.enqLsq.req.zip(io.fromDispatch.req).zip(io.fromDispatch.resp).zipWithIndex.foreach{ case (((toLsq, enq), resp), i) =>
    val do_enq = enq.valid && !io.redirect.valid && !io.fromDispatch.recoverStall && io.fromDispatch.iqAccept(i) &&
      io.fromDispatch.needAlloc(i).orR
    toLsq.valid := RegNext(do_enq)
    toLsq.bits := RegEnable(enq.bits, do_enq)

    XSError(do_enq && resp.lqIdx < lqHeadPtr && !lqHeadPtr.isRotateBy(resp.lqIdx), s"loadQueue overflow! index: ${i}\n")
    XSError(do_enq && resp.sqIdx < sqHeadPtr && !sqHeadPtr.isRotateBy(resp.sqIdx), s"storeQueue overflow! index: ${i}\n")
  }

  io.fromDispatch.recoverStall := RegNext(state =/= State.idle || redirectUpdate)

  // for Topdown
  io.lqStall.foreach(_ := RegNext(!lqCanAccept))
  io.sqStall.foreach(_ := RegNext(!sqCanAccept))
  // debug

  if(debugEn) {
    dontTouch(sqTailPtrNext)
    dontTouch(WireInit(VecInit(loadFlowPopCount)))
    dontTouch(WireInit(VecInit(storeFlowPopCount)))
    dontTouch(needEnqStoreQueue)
    dontTouch(needEnqLoadQueue)
    dontTouch(sqAllocNumber)
    dontTouch(lqAllocNumber)
  }
}
