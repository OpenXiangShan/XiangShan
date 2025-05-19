// Copyright (c) 2024-2025 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2025 Institute of Computing Technology, Chinese Academy of Sciences
// Copyright (c) 2020-2021 Peng Cheng Laboratory
//
// XiangShan is licensed under Mulan PSL v2.
// You can use this software according to the terms and conditions of the Mulan PSL v2.
// You may obtain a copy of Mulan PSL v2 at:
//          https://license.coscl.org.cn/MulanPSL2
//
// THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
// EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
// MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
//
// See the Mulan PSL v2 for more details.
//
// Acknowledgement
// This implementation is inspired by several key papers:
// [1] Glenn Reinman, Todd Austin, and Brad Calder. "[A scalable front-end architecture for fast instruction delivery.]
// (https://doi.org/10.1109/ISCA.1999.765954)" 26th International Symposium on Computer Architecture (ISCA). 1999.

package xiangshan.frontend.ftq

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.HasCircularQueuePtrHelper
import utility.HasPerfEvents
import utility.ParallelPriorityMux
import utility.XSError
import xiangshan.Redirect
import xiangshan.RedirectLevel
import xiangshan.ValidUndirectioned
import xiangshan.backend.CtrlToFtqIO
import xiangshan.frontend.BpuToFtqIO
import xiangshan.frontend.BranchPredictionRedirect
import xiangshan.frontend.ExceptionType
import xiangshan.frontend.FtqToICacheIO
import xiangshan.frontend.FtqToIfuIO
import xiangshan.frontend.HasBPUConst
import xiangshan.frontend.IfuToFtqIO
import xiangshan.frontend.PrunedAddr

class Ftq(implicit p: Parameters) extends FtqModule
    with HasPerfEvents
    with HasCircularQueuePtrHelper
    with BackendRedirectReceiver
    with HasBPUConst {

  class FtqIO extends FtqBundle {
    val fromBpu: BpuToFtqIO = Flipped(new BpuToFtqIO)
    val toBpu:   FtqToBpuIO = new FtqToBpuIO

    val fromIfu: IfuToFtqIO = Flipped(new IfuToFtqIO)
    val toIfu:   FtqToIfuIO = new FtqToIfuIO

    val toICache: FtqToICacheIO = new FtqToICacheIO

    val fromBackend: CtrlToFtqIO = Flipped(new CtrlToFtqIO)
    val toBackend:   FtqToCtrlIO = new FtqToCtrlIO

    val bpuInfo = new Bundle {
      val bpRight: UInt = Output(UInt(XLEN.W))
      val bpWrong: UInt = Output(UInt(XLEN.W))
    }

    // for perf
    val ControlBTBMissBubble: Bool = Output(Bool())
    val TAGEMissBubble:       Bool = Output(Bool())
    val SCMissBubble:         Bool = Output(Bool())
    val ITTAGEMissBubble:     Bool = Output(Bool())
    val RASMissBubble:        Bool = Output(Bool())
  }

  val io: FtqIO = IO(new FtqIO)

  // FTQ pointers. All the pointers mean the pointed entry is *to be* processed, not already processed.
  // For example, bpuPtr points to the entry that BPU prepares to write,
  // and commitPtr points to the entry to be committed by backend.
  private val bpuPtr    = RegInit(FtqPtrVec())
  private val pfPtr     = RegInit(FtqPtrVec(2))
  private val ifuPtr    = RegInit(FtqPtrVec(3))
  private val ifuWbPtr  = RegInit(FtqPtrVec())
  private val commitPtr = RegInit(FtqPtrVec(2))

  XSError(bpuPtr < ifuPtr && !isFull(bpuPtr(0), ifuPtr(0)), "ifuPtr runs ahead of bpuPtr")
  XSError(bpuPtr < pfPtr && !isFull(bpuPtr(0), pfPtr(0)), "pfPtr runs ahead of bpuPtr")
  XSError(ifuWbPtr < commitPtr && !isFull(ifuWbPtr(0), commitPtr(0)), "ifuWbPtr runs ahead of commitPtr")

  private val validEntries = distanceBetween(bpuPtr(0), commitPtr(0))

  // entryQueue stores predictions made by BPU.
  private val entryQueue = Module(new EntryQueue)

  // ftbEntryQueue stores FTB entries of BPU, which are used to train BPU after committed.
  private val ftbEntryQueue = Module(new FtbEntryQueue)

  // metaQueue stores information needed to train BPU.
  private val metaQueue = Module(new MetaQueue)

  // pdQueue stores pre-decode information from IFU.
  private val pdQueue = Module(new PdQueue)

  private val redirectQueue = Module(new RedirectQueue)

  private val readyToCommit = Wire(Bool())
  private val canCommit     = Wire(Bool())
  private val shouldCommit  = RegInit(VecInit(Seq.fill(FtqSize)(false.B)))

  private val ifuRedirect = WireInit(0.U.asTypeOf(Valid(new BranchPredictionRedirect)))

  private val (backendRedirectFtqIdx, backendRedirect) = receiveBackendRedirect(io.fromBackend)

  // Instruction page fault and instruction access fault are sent from backend with redirect requests.
  // When IPF and IAF are sent, backendPcFaultIfuPtr points to the FTQ entry whose first instruction
  // raises IPF or IAF, which is ifuWbPtr_write or IfuPtr_write.
  // Only when IFU has written back that FTQ entry can backendIpf and backendIaf be false because this
  // makes sure that IAF and IPF are correctly raised instead of being flushed by redirect requests.
  private val backendException  = RegInit(ExceptionType.none)
  private val backendPcFaultPtr = RegInit(FtqPtr(false.B, 0.U))
  when(backendRedirect.valid) {
    backendException := ExceptionType.fromOH(
      has_pf = backendRedirect.bits.cfiUpdate.backendIPF,
      has_gpf = backendRedirect.bits.cfiUpdate.backendIGPF,
      has_af = backendRedirect.bits.cfiUpdate.backendIAF
    )
    when(ExceptionType.hasException(backendException)) {
      backendPcFaultPtr := ifuWbPtr(0)
    }
  }.elsewhen(ifuWbPtr(0) =/= backendPcFaultPtr) {
    backendException := ExceptionType.none
  }

  // --------------------------------------------------------------------------------
  // Interaction with BPU
  // --------------------------------------------------------------------------------
  // TODO: resp is a bad name
  io.fromBpu.resp.ready := validEntries < FtqSize.U || readyToCommit

  private val bpuResp = io.fromBpu.resp.bits

  private val bpuS2Redirect = bpuResp.s2.valid && bpuResp.s2.hasRedirect
  private val bpuS3Redirect = bpuResp.s3.valid && bpuResp.s3.hasRedirect

  io.toBpu.enq_ptr := bpuPtr(0)
  // TODO: Why use bpuResp.s2&3.valid for s2 and s3, but io.fromBpu.resp.fire for s1?
  private val bpuEnqueue = io.fromBpu.resp.fire && !ifuRedirect.valid && !backendRedirect.valid

  private val bpuIn = (io.fromBpu.resp.fire || bpuS2Redirect || bpuS3Redirect) &&
    !ifuRedirect.valid && !backendRedirect.valid

  private val bpuInResp    = bpuResp.selectedResp
  private val bpuInStage   = bpuResp.selectedRespIdxForFtq
  private val bpuInRespPtr = Mux(bpuInStage === BP_S1, bpuPtr(0), bpuInResp.ftq_idx)

  private val bpuInNext        = RegNext(bpuIn)
  private val bpuInRespNext    = RegEnable(bpuInResp, bpuIn)
  private val bpuInRespPtrNext = RegEnable(bpuInRespPtr, bpuIn)
  private val bpuInStageNext   = RegEnable(bpuInStage, bpuIn)

  bpuPtr := bpuPtr + bpuEnqueue
  for (stage <- 2 to 3) {
    when(bpuResp.stage(stage).valid && bpuResp.stage(stage).hasRedirect) {
      bpuPtr := bpuResp.stage(stage).ftq_idx + 1.U
    }
  }

  entryQueue.io.wen   := bpuIn
  entryQueue.io.waddr := bpuInRespPtr.value
  entryQueue.io.wdata := bpuInResp

  redirectQueue.io.wen   := bpuResp.lastStage.valid
  redirectQueue.io.waddr := bpuResp.lastStage.ftq_idx.value
  redirectQueue.io.wdata := bpuResp.last_stage_spec_info

  metaQueue.io.wen        := bpuResp.lastStage.valid
  metaQueue.io.waddr      := bpuResp.lastStage.ftq_idx.value
  metaQueue.io.wdata.meta := io.fromBpu.meta
  if (metaQueue.io.wdata.paddingBit.isDefined) {
    metaQueue.io.wdata.paddingBit.get := 0.U
  }
  metaQueue.io.wdata.ftb_entry := bpuResp.last_stage_ftb_entry

  ftbEntryQueue.io.wen   := bpuResp.lastStage.valid
  ftbEntryQueue.io.waddr := bpuResp.lastStage.ftq_idx.value
  ftbEntryQueue.io.wdata := bpuResp.last_stage_ftb_entry

  private val latestTarget           = Reg(PrunedAddr(VAddrBits))
  private val latestTargetModified   = RegInit(false.B)
  private val latestEntryPtr         = Reg(new FtqPtr)
  private val latestEntryPtrModified = RegInit(false.B)
  private val cfiIndexVec            = Reg(Vec(FtqSize, ValidUndirectioned(UInt(log2Ceil(PredictWidth).W))))
  private val mispredictVec          = Reg(Vec(FtqSize, Vec(PredictWidth, Bool())))
  private val predStage              = Reg(Vec(FtqSize, UInt(2.W)))

  private val entrySentToIfu = RegInit(VecInit(Seq.fill(FtqSize)(true.B)))

  // Modify registers one cycle later to cut critical path
  latestTargetModified   := false.B
  latestEntryPtrModified := false.B
  when(bpuInNext) {
    val ftqIdx = bpuInRespPtrNext.value

    entrySentToIfu(ftqIdx) := false.B
    cfiIndexVec(ftqIdx)    := bpuInRespNext.cfiIndex
    predStage(ftqIdx)      := bpuInStageNext

    latestTargetModified   := true.B
    latestTarget           := bpuInRespNext.getTarget
    latestEntryPtrModified := true.B
    latestEntryPtr         := bpuInRespPtrNext

    mispredictVec(ftqIdx) := WireInit(VecInit(Seq.fill(PredictWidth)(false.B)))

    shouldCommit(ftqIdx) := true.B
  }

  // TODO: Rewrite this in FSM style
  private def entryNotHit    = 0.U(2.W)
  private def entryFalseHit  = 1.U(2.W)
  private def entryHit       = 2.U(2.W)
  private val entryHitStatus = RegInit(VecInit(Seq.fill(FtqSize)(entryNotHit)))

  // Only use FTB result to modify hit status
  // TODO: Why?
  when(bpuResp.s2.valid) {
    entryHitStatus(bpuResp.s2.ftq_idx.value) := Mux(bpuResp.s2.full_pred.hit, entryHit, entryNotHit)
  }

  // --------------------------------------------------------------------------------
  // Interaction with ICache and IFU
  // --------------------------------------------------------------------------------

  when(io.toICache.prefetchReq.fire && !ifuRedirect.valid && !backendRedirect.valid) {
    pfPtr := pfPtr + 1.U
  }
  when(io.toIfu.req.fire && !ifuRedirect.valid && !backendRedirect.valid) {
    ifuPtr := ifuPtr + 1.U
  }

  for (stage <- 2 to 3) {
    val redirect = bpuResp.stage(stage).valid && bpuResp.stage(stage).hasRedirect
    val ftqIdx   = bpuResp.stage(stage).ftq_idx

    io.toICache.flushFromBpu.stage(stage).valid := redirect
    io.toICache.flushFromBpu.stage(stage).bits  := ftqIdx
    io.toIfu.flushFromBpu.stage(stage).valid    := redirect
    io.toIfu.flushFromBpu.stage(stage).bits     := ftqIdx

    when(redirect) {
      when(pfPtr >= ftqIdx) {
        pfPtr := ftqIdx
      }
      when(ifuPtr >= ftqIdx) {
        ifuPtr := ftqIdx
      }
    }
  }

  io.toIfu.req.bits.ftqIdx := ifuPtr(0)

  entryQueue.io.pfPtr.zipWithIndex.foreach { case (ptr, offset) => ptr.ptr := pfPtr(offset) }
  entryQueue.io.ifuPtr.zipWithIndex.foreach { case (ptr, offset) => ptr.ptr := ifuPtr(offset) }

  private val toPreFetchEntry     = Wire(new FtqEntry)
  private val bpuInPrefetchBypass = bpuInNext && bpuInRespPtrNext === pfPtr(0)

  when(bpuInPrefetchBypass) {
    toPreFetchEntry := bpuInRespNext
  }.otherwise {
    toPreFetchEntry := Mux(
      RegNext(io.toICache.prefetchReq.fire),
      entryQueue.io.pfPtr(1).rdata,
      entryQueue.io.pfPtr(0).rdata
    )
  }

  // TODO: why valid also uses entrySentToIfu?
  io.toICache.prefetchReq.valid := (bpuInPrefetchBypass || !entrySentToIfu(pfPtr(0).value)) && pfPtr(0) =/= bpuPtr(0)
  io.toICache.prefetchReq.bits.req.fromFtqPcBundle(toPreFetchEntry)
  io.toICache.prefetchReq.bits.req.ftqIdx := pfPtr(0)
  // TODO: why this is different from ICache?
  io.toICache.prefetchReq.bits.backendException := Mux(
    backendPcFaultPtr === pfPtr(0),
    backendException,
    ExceptionType.none
  )

  private val toIfuEntry          = Wire(new FtqEntry)
  private val toIfuEntryNextAddr  = Wire(PrunedAddr(VAddrBits))
  private val toIfuEntryFtqOffset = WireInit(cfiIndexVec(ifuPtr(0).value))
  private val bpuInIfuBypass      = bpuInNext && bpuInRespPtrNext === ifuPtr(0)

  when(bpuInIfuBypass) {
    toIfuEntry          := bpuInRespNext
    toIfuEntryNextAddr  := bpuInRespNext.getTarget
    toIfuEntryFtqOffset := bpuInRespNext.cfiIndex
  }.otherwise {
    toIfuEntry := Mux(RegNext(io.toIfu.req.fire), entryQueue.io.ifuPtr(1).rdata, entryQueue.io.ifuPtr(0).rdata)
    // TODO: potential problems here
    toIfuEntryNextAddr := Mux(
      bpuInNext && bpuInRespPtrNext === ifuPtr(1),
      bpuInRespNext.pc,
      Mux(ifuPtr(0) === latestEntryPtr, latestTarget, RegNext(entryQueue.io.ifuPtr(1).rdata.startAddr))
    )
  }

  // TODO: we should not need both fetchReq.valid and fetchReq.bits.readValid
  io.toICache.fetchReq.valid := (bpuInIfuBypass || !entrySentToIfu(ifuPtr(0).value)) && ifuPtr(0) =/= bpuPtr(0)
  io.toICache.fetchReq.bits.readValid.foreach(valid =>
    valid := (bpuInIfuBypass || !entrySentToIfu(ifuPtr(0).value)) && ifuPtr(0) =/= bpuPtr(0)
  )
  io.toICache.fetchReq.bits.req.foreach { req =>
    req.fromFtqPcBundle(toIfuEntry)
    req.ftqIdx := ifuPtr(0)
  }
  io.toICache.fetchReq.bits.isBackendException := ExceptionType.hasException(backendException) &&
    backendPcFaultPtr === ifuPtr(0)

  io.toIfu.req.valid              := (bpuInIfuBypass || !entrySentToIfu(ifuPtr(0).value)) && ifuPtr(0) =/= bpuPtr(0)
  io.toIfu.req.bits.nextStartAddr := toIfuEntryNextAddr
  io.toIfu.req.bits.ftqOffset     := toIfuEntryFtqOffset
  io.toIfu.req.bits               := toIfuEntry

  // False hit if fall through is smaller than start address
  when(toIfuEntry.fallThruError && entryHitStatus(ifuPtr(0).value) === entryHit) {
    when(io.toIfu.req.fire &&
      !(bpuS2Redirect && bpuResp.stage(2).ftq_idx === ifuPtr(0)) &&
      !(bpuS3Redirect && bpuResp.stage(3).ftq_idx === ifuPtr(0))) {
      entryHitStatus(ifuPtr(0).value) := entryFalseHit
    }
  }

  private val shouldFlushIfuReq = io.toIfu.flushFromBpu.shouldFlushByStage2(io.toIfu.req.bits.ftqIdx) ||
    io.toIfu.flushFromBpu.shouldFlushByStage3(io.toIfu.req.bits.ftqIdx)

  when(io.toIfu.req.fire && !shouldFlushIfuReq) {
    entrySentToIfu(ifuPtr(0).value) := true.B
  }

  // --------------------------------------------------------------------------------
  // Write back from IFU
  // --------------------------------------------------------------------------------
  private val pdWb = io.fromIfu.pdWb
  pdQueue.io.wen   := pdWb.valid
  pdQueue.io.waddr := pdWb.bits.ftqIdx.value
  pdQueue.io.wdata.fromPdWb(pdWb.bits)

  private val hitPdValid          = entryHitStatus(pdWb.bits.ftqIdx.value) === entryHit && pdWb.valid
  private val hitPdMispredictNext = RegNext(hitPdValid && pdWb.bits.misOffset.valid)
  private val pdNext              = RegEnable(pdWb.bits.pd, pdWb.valid)

  when(pdWb.valid) {
    ifuWbPtr := ifuWbPtr(0) + 1.U
  }

  ftbEntryQueue.io.ifuWb.ren   := pdWb.valid
  ftbEntryQueue.io.ifuWb.raddr := pdWb.bits.ftqIdx.value

  private val hasFalseHit = WireInit(false.B)
  when(RegNext(hitPdValid)) {
    val ftbEntry = ftbEntryQueue.io.ifuWb.rdata
    val brSlots  = ftbEntry.brSlots
    val tailSlot = ftbEntry.tailSlot

    val brFalseHit = brSlots.map(s => s.valid && !(pdNext(s.offset).valid && pdNext(s.offset).isBr)).reduce(_ || _) ||
      tailSlot.valid && ftbEntry.tailSlot.sharing && !(pdNext(tailSlot.offset).valid && pdNext(tailSlot.offset).isBr)

    val jmpPd = pdNext(tailSlot.offset)
    val jalFalseHit = ftbEntry.jmpValid &&
      (ftbEntry.isJal && !(jmpPd.valid && jmpPd.isJal) ||
        ftbEntry.isJalr && !(jmpPd.valid && jmpPd.isJalr) ||
        ftbEntry.isCall && !(jmpPd.valid && jmpPd.isCall) ||
        ftbEntry.isRet && !(jmpPd.valid && jmpPd.isRet))

    hasFalseHit := brFalseHit || jalFalseHit || hitPdMispredictNext
  }

  when(hasFalseHit) {
    entryHitStatus(RegNext(pdWb.bits.ftqIdx.value)) := entryFalseHit
  }

  // --------------------------------------------------------------------------------
  // Interaction with backend
  // --------------------------------------------------------------------------------

  // TODO: Is this still in use?
  io.toBackend.pc_mem_wen   := RegNext(bpuInNext)
  io.toBackend.pc_mem_waddr := RegNext(bpuInRespPtr.value)
  io.toBackend.pc_mem_wdata := RegNext(bpuInRespNext)

  io.toBackend.newest_entry_en     := RegNext(RegNext(bpuIn || backendRedirect.valid || ifuRedirect.valid))
  io.toBackend.newest_entry_ptr    := RegNext(latestEntryPtr)
  io.toBackend.newest_entry_target := RegNext(latestTarget.toUInt)

  // --------------------------------------------------------------------------------
  // Redirect from backend and IFU
  // --------------------------------------------------------------------------------
  redirectQueue.io.backendRedirect.ren   := backendRedirectFtqIdx.valid
  redirectQueue.io.backendRedirect.raddr := backendRedirectFtqIdx.bits.value
  ftbEntryQueue.io.backendRedirect.ren   := backendRedirectFtqIdx.valid
  ftbEntryQueue.io.backendRedirect.raddr := backendRedirectFtqIdx.bits.value
  pdQueue.io.backendRedirect.ren         := backendRedirectFtqIdx.valid
  pdQueue.io.backendRedirect.raddr       := backendRedirectFtqIdx.bits.value

  // TODO: bad naming
  private val redirectQueueRdata = redirectQueue.io.backendRedirect.rdata
  private val redirectPdRdata    = pdQueue.io.backendRedirect.rdata
  private val redirectFtbEntry   = ftbEntryQueue.io.backendRedirect.rdata
  private val redirectFtqOffset  = backendRedirect.bits.ftqOffset

  // TODO: why the fuck manipulate something passed by backend??? And I can not even manipulate them now!
//  private val backendRedirectCfi = backendRedirect.bits.cfiUpdate
//  backendRedirectCfi.fromFtqRedirectSram(redirectQueueRdata)
//  backendRedirectCfi.pd     := redirectPdRdata.toPd(redirectFtqOffset)
//  backendRedirectCfi.br_hit := redirectFtbEntry.brIsSaved(redirectFtqOffset)
//  backendRedirectCfi.jr_hit := redirectFtbEntry.isJalr && redirectFtbEntry.tailSlot.offset === redirectFtqOffset

//  when(entryHitStatus(backendRedirect.bits.ftqIdx.value) === entryHit) {
//    backendRedirectCfi.shift := PopCount(redirectFtbEntry.getBrMaskByOffset(redirectFtqOffset)) +&
//      (backendRedirectCfi.pd.isBr &&
//        !redirectFtbEntry.brIsSaved(redirectFtqOffset) &&
//        !redirectFtbEntry.newBrCanNotInsert(redirectFtqOffset))
//
//    backendRedirectCfi.addIntoHist := backendRedirectCfi.pd.isBr &&
//      (redirectFtbEntry.brIsSaved(redirectFtqOffset) || !redirectFtbEntry.newBrCanNotInsert(redirectFtqOffset))
//  }.otherwise {
//    backendRedirectCfi.shift       := (backendRedirectCfi.pd.isBr && backendRedirectCfi.taken).asUInt
//    backendRedirectCfi.addIntoHist := backendRedirectCfi.pd.isBr.asUInt
//  }

  ifuRedirect.valid                    := pdWb.valid && pdWb.bits.misOffset.valid && !backendRedirect.valid
  ifuRedirect.bits.ftqIdx              := pdWb.bits.ftqIdx
  ifuRedirect.bits.ftqOffset           := pdWb.bits.misOffset.bits
  ifuRedirect.bits.level               := RedirectLevel.flushAfter
  ifuRedirect.bits.cfiUpdate.pc        := pdWb.bits.pc(pdWb.bits.misOffset.bits).toUInt
  ifuRedirect.bits.cfiUpdate.pd        := pdWb.bits.pd(pdWb.bits.misOffset.bits)
  ifuRedirect.bits.cfiUpdate.predTaken := cfiIndexVec(pdWb.bits.ftqIdx.value).valid
  ifuRedirect.bits.cfiUpdate.target    := pdWb.bits.target.toUInt
  ifuRedirect.bits.cfiUpdate.taken     := pdWb.bits.cfiOffset.valid
  ifuRedirect.bits.cfiUpdate.isMisPred := pdWb.bits.misOffset.valid
  ifuRedirect.bits.cfiUpdate.fromFtqRedirectSram(redirectQueue.io.ifuRedirect.rdata)
  when(ifuRedirect.bits.cfiUpdate.pd.isRet && ifuRedirect.bits.cfiUpdate.pd.valid) {
    ifuRedirect.bits.cfiUpdate.target := ifuRedirect.bits.cfiUpdate.topAddr
  }

  redirectQueue.io.ifuRedirect.ren   := ifuRedirect.valid
  redirectQueue.io.ifuRedirect.raddr := ifuRedirect.bits.ftqIdx.value

  private def updateCfi(redirect: Valid[Redirect]): Unit = {
    val valid     = redirect.valid
    val ftqPtr    = redirect.bits.ftqIdx
    val ftqOffset = redirect.bits.ftqOffset
    val taken     = redirect.bits.cfiUpdate.taken

    val cfiIndexValidWen = valid && ftqOffset === cfiIndexVec(ftqPtr.value).bits
    val cfiIndexBitsWen  = valid && taken && ftqOffset < cfiIndexVec(ftqPtr.value).bits

    when(cfiIndexValidWen || cfiIndexBitsWen) {
      cfiIndexVec(ftqPtr.value).valid := cfiIndexValidWen && taken || cfiIndexBitsWen
    }.elsewhen(valid && !taken && ftqOffset =/= cfiIndexVec(ftqPtr.value).bits) {
      cfiIndexVec(ftqPtr.value).valid := false.B
    }

    when(cfiIndexBitsWen) {
      cfiIndexVec(ftqPtr.value).bits := ftqOffset
    }

    latestTargetModified   := true.B
    latestTarget           := redirect.bits.cfiUpdate.target
    latestEntryPtrModified := true.B
    latestEntryPtr         := ftqPtr
  }

  when(backendRedirect.valid) {
    updateCfi(backendRedirect)
    val redirect = backendRedirect.bits
    mispredictVec(redirect.ftqIdx.value)(redirect.ftqOffset) :=
      redirect.cfiUpdate.isMisPred && redirect.level === RedirectLevel.flushAfter
  }.elsewhen(ifuRedirect.valid) {
    updateCfi(ifuRedirect)
  }

  private val redirectVec   = Seq(backendRedirect, ifuRedirect)
  private val redirectValid = redirectVec.map(_.valid).reduce(_ || _)
  io.toICache.redirectFlush := redirectValid
  when(redirectValid) {
    val redirect    = PriorityMux(redirectVec.map(redirect => redirect.valid -> redirect.bits))
    val newEntryPtr = redirect.ftqIdx + 1.U
    Seq(bpuPtr, ifuPtr, pfPtr).foreach(_ := newEntryPtr)
    when(RedirectLevel.flushItself(redirect.level)) {
      shouldCommit(redirect.ftqIdx.value) := false.B
      shouldCommit(newEntryPtr.value)     := false.B
    }
  }

  io.toIfu.redirect.valid := backendRedirect.valid
  // TODO: only valid should be needed
  io.toIfu.redirect.bits := DontCare

  io.toBpu.redirctFromIFU := ifuRedirect.valid
  io.toBpu.redirect       := Mux(backendRedirect.valid, backendRedirect, ifuRedirect)

  // --------------------------------------------------------------------------------
  // MMIO fetch
  // --------------------------------------------------------------------------------
  private val mmioPtr           = io.fromIfu.mmioCommitRead.mmioFtqPtr
  private val lastMmioCommitted = commitPtr > mmioPtr || commitPtr === mmioPtr && canCommit
  io.fromIfu.mmioCommitRead.mmioLastCommit := RegNext(lastMmioCommitted)

  // --------------------------------------------------------------------------------
  // Commit and train BPU
  // --------------------------------------------------------------------------------
  // TODO: problems here, rewrite commit after BTB is merged
  // TODO: frontend does not need this many rob commit channels
  private val robCommitPtr: FtqPtr = WireInit(FtqPtr(false.B, 0.U))
  private val backendCommit = io.fromBackend.rob_commits.map(_.valid).reduce(_ | _)
  when(backendCommit) {
    robCommitPtr := ParallelPriorityMux(
      io.fromBackend.rob_commits.map(_.valid).reverse,
      io.fromBackend.rob_commits.map(_.bits.ftqIdx).reverse
    )
  }
  private val robCommitPtrReg: FtqPtr = RegEnable(robCommitPtr, FtqPtr(false.B, 0.U), backendCommit)
  private val committedPtr = Mux(backendCommit, robCommitPtr, robCommitPtrReg)
  canCommit     := commitPtr < committedPtr
  readyToCommit := canCommit && shouldCommit(commitPtr(0).value)
  when(canCommit) {
    commitPtr := commitPtr + 1.U
  }

  // frontend commit is one cycle later than backend commit because reading srams needs one cycle
  private val s2_readyToCommit = RegNext(readyToCommit, init = false.B)

  entryQueue.io.commitPtr(0).ptr := commitPtr(0)
  entryQueue.io.commitPtr(1).ptr := commitPtr(1)
  private val s2_commitEntry = RegNext(entryQueue.io.commitPtr(0).rdata)
  private val s2_commitTarget = Mux(
    RegNext(commitPtr === latestEntryPtr),
    RegEnable(latestTarget, latestTargetModified),
    RegNext(entryQueue.io.commitPtr(1).rdata.startAddr)
  )

  pdQueue.io.commit.ren   := readyToCommit
  pdQueue.io.commit.raddr := commitPtr(0).value
  private val s2_commitPd = pdQueue.io.commit.rdata

  redirectQueue.io.commit.ren   := readyToCommit
  redirectQueue.io.commit.raddr := commitPtr(0).value
  private val s2_commitSpeculativeInfo = redirectQueue.io.commit.rdata

  metaQueue.io.ren   := readyToCommit
  metaQueue.io.raddr := commitPtr(0).value
  private val s2_commitMeta = metaQueue.io.rdata.meta
  private val s2_commitFtb  = metaQueue.io.rdata.ftb_entry // TODO: why not FTB entry queue?

  private val s1_commitCfi = cfiIndexVec(commitPtr(0).value)
  private val s2_commitCfi = RegEnable(s1_commitCfi, readyToCommit)

  private val s1_commitHit   = entryHitStatus(commitPtr(0).value)
  private val s2_commitHit   = RegEnable(s1_commitHit, readyToCommit)
  private val s2_commitValid = s2_commitHit === entryHit || s2_commitCfi.valid

  private val s2_commitStage = RegEnable(predStage(commitPtr(0).value), readyToCommit)

  private val s2_commitMispredict = RegEnable(mispredictVec(commitPtr(0).value), readyToCommit)

  private val ftbGenerator = Module(new FtbEntryGen)
  ftbGenerator.io.start_addr     := s2_commitEntry.startAddr
  ftbGenerator.io.old_entry      := s2_commitFtb
  ftbGenerator.io.pd             := s2_commitPd
  ftbGenerator.io.cfiIndex       := s2_commitCfi
  ftbGenerator.io.target         := s2_commitTarget
  ftbGenerator.io.hit            := s2_commitHit === entryHit
  ftbGenerator.io.mispredict_vec := s2_commitMispredict

  io.toBpu.update                  := DontCare // TODO: ?
  io.toBpu.update.valid            := s2_commitValid && s2_readyToCommit
  io.toBpu.update.bits.false_hit   := s2_commitHit === entryFalseHit
  io.toBpu.update.bits.pc          := s2_commitEntry.startAddr
  io.toBpu.update.bits.meta        := s2_commitMeta
  io.toBpu.update.bits.cfi_idx     := s2_commitCfi
  io.toBpu.update.bits.full_target := s2_commitTarget
  io.toBpu.update.bits.from_stage  := s2_commitStage
  io.toBpu.update.bits.spec_info   := s2_commitSpeculativeInfo

  io.toBpu.update.bits.ftb_entry         := ftbGenerator.io.new_entry
  io.toBpu.update.bits.new_br_insert_pos := ftbGenerator.io.new_br_insert_pos
  io.toBpu.update.bits.mispred_mask      := ftbGenerator.io.mispred_mask
  io.toBpu.update.bits.old_entry         := ftbGenerator.io.is_old_entry
  io.toBpu.update.bits.pred_hit          := s2_commitHit === entryHit || s2_commitHit === entryFalseHit
  io.toBpu.update.bits.br_taken_mask     := ftbGenerator.io.taken_mask
  io.toBpu.update.bits.jmp_taken         := ftbGenerator.io.jmp_taken

  // --------------------------------------------------------------------------------
  // Performance monitoring
  // --------------------------------------------------------------------------------
  io.bpuInfo                    := DontCare
  io.toIfu.req.bits.topdownInfo := DontCare
  io.toIfu.topdown_redirect     := DontCare
  io.ControlBTBMissBubble       := DontCare
  io.TAGEMissBubble             := DontCare
  io.SCMissBubble               := DontCare
  io.ITTAGEMissBubble           := DontCare
  io.RASMissBubble              := DontCare

  val perfEvents: Seq[(String, UInt)] = Seq()
  generatePerfEvent()
}
