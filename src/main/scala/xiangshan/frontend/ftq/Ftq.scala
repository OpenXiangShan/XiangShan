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
import utility.DataHoldBypass
import utility.HasCircularQueuePtrHelper
import utility.HasPerfEvents
import utility.UIntToMask
import utility.XSError
import utility.XSPerfAccumulate
import utility.XSPerfHistogram
import utility.XSPerfRolling
import utility.XSPerfSeqAccumulate
import utility.XSPerfSeqRolling
import xiangshan.RedirectLevel
import xiangshan.TopDownCounters
import xiangshan.backend.CtrlToFtqIO
import xiangshan.frontend.BackendRedirectTopdown
import xiangshan.frontend.BlameBpuSource
import xiangshan.frontend.BpuToFtqIO
import xiangshan.frontend.ExceptionType
import xiangshan.frontend.FetchRequestBundle
import xiangshan.frontend.FrontendTopDownBundle
import xiangshan.frontend.FtqToBpuIO
import xiangshan.frontend.FtqToICacheIO
import xiangshan.frontend.FtqToIfuIO
import xiangshan.frontend.IfuToFtqIO
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.PrunedAddrInit
import xiangshan.frontend.TwoPrefetchCase
import xiangshan.frontend.bpu.BpuCommitMeta
import xiangshan.frontend.bpu.BpuPredictionSource
import xiangshan.frontend.bpu.BpuRedirectMeta
import xiangshan.frontend.bpu.BpuResolveMeta
import xiangshan.frontend.bpu.BpuTrain
import xiangshan.frontend.bpu.BranchAttribute
import xiangshan.frontend.bpu.BranchInfo
import xiangshan.frontend.bpu.HalfAlignHelper
import xiangshan.frontend.icache.ICacheCacheLineHelper
import xiangshan.frontend.icache.ICacheToFtqIO

class Ftq(implicit p: Parameters) extends FtqModule
    with HalfAlignHelper
    with HasPerfEvents
    with HasCircularQueuePtrHelper
    with IfuRedirectReceiver
    with BackendRedirectReceiver
    with ICacheCacheLineHelper {

  class FtqIO extends FtqBundle {
    val fromBpu: BpuToFtqIO = Flipped(new BpuToFtqIO)
    val toBpu:   FtqToBpuIO = new FtqToBpuIO

    val fromIfu: IfuToFtqIO = Flipped(new IfuToFtqIO)
    val toIfu:   FtqToIfuIO = new FtqToIfuIO

    val fromICache: ICacheToFtqIO = Flipped(new ICacheToFtqIO)
    val toICache:   FtqToICacheIO = new FtqToICacheIO

    val fromBackend: CtrlToFtqIO = Flipped(new CtrlToFtqIO)
    val toBackend:   FtqToCtrlIO = new FtqToCtrlIO

    // Topdown analysis
    val backendRedirectTopdown: BackendRedirectTopdown = Output(new BackendRedirectTopdown)
  }

  val io: FtqIO = IO(new FtqIO)

  // FTQ pointers. All the pointers mean the pointed entry is *to be* processed, not already processed.
  // For example, bpuPtr points to the entry that BPU prepares to write,
  // and commitPtr points to the entry to be committed by backend.
  private val bpuPtr    = RegInit(FtqPtrVec())
  private val pfPtr     = RegInit(FtqPtrVec(2))
  private val fetchPtr  = RegInit(FtqPtrVec(3))
  private val commitPtr = RegInit(FtqPtrVec(2))

  XSError(bpuPtr < fetchPtr && !isFull(bpuPtr(0), fetchPtr(0)), "fetchPtr runs ahead of bpuPtr")
//  XSError(bpuPtr < pfPtr && !isFull(bpuPtr(0), pfPtr(0)), "pfPtr runs ahead of bpuPtr")

  // entryQueue stores predictions made by BPU.
  private val entryQueue = Reg(Vec(FtqSize, new FtqEntry))

  // metaQueueRedirect stores speculation information needed by BPU when redirect happens.
  private val metaQueueRedirect = Reg(Vec(FtqSize, new BpuRedirectMeta))

  // metaQueue stores information needed to train BPU.
  private val metaQueueResolve = Reg(Vec(FtqSize, new BpuResolveMeta))
  private val metaQueueCommit  = Reg(Vec(FtqSize, new BpuCommitMeta))

  // resolveQueue caches branch resolve information from backend.
  private val resolveQueue = Module(new ResolveQueue)

  // commitQueue caches branch commit information from backend.
  private val commitQueue = Module(new CommitQueue)

  // perfQueue stores information for performance monitoring. These queues should not exist in hardware
  private val perfQueue = Reg(Vec(FtqSize, new PerfMeta))

  private val (backendRedirectFtqIdxInAdvance, backendRedirect) = receiveBackendRedirect(io.fromBackend)

  private val specTopAddr = metaQueueRedirect(io.fromIfu.wbRedirect.bits.ftqIdx.value).ras.topRetAddr.toUInt
  private val (ifuRedirectFtqIdxInAdvance, ifuRedirect, ifuResolve) = receiveIfuRedirect(
    io.fromIfu.wbRedirect,
    specTopAddr,
    backendRedirect.valid
  )

  // redirectFtqIdxInAdvance is always one cycle ahead of redirect
  private val redirectFtqIdxInAdvance = Mux(
    backendRedirectFtqIdxInAdvance.valid,
    backendRedirectFtqIdxInAdvance.bits,
    ifuRedirectFtqIdxInAdvance.bits
  )

  private val redirect = Mux(backendRedirect.valid, backendRedirect, ifuRedirect)

  // Instruction page fault, guest page fault, and access fault are checked by backend and sent with redirect requests.
  private val backendException    = RegInit(ExceptionType.None)
  private val backendExceptionPtr = RegInit(FtqPtr(false.B, 0.U))
  when(backendRedirect.valid) {
    val exception = ExceptionType.fromBackend(backendRedirect.bits)
    backendException := exception
    when(exception.hasException) {
      backendExceptionPtr := backendRedirect.bits.newFtqIdx
    }
  }.elsewhen(distanceBetween(fetchPtr(0), backendExceptionPtr) >= 3.U) {
    // We cannot clear backendException flag too early (e.g. once fetch fire),
    // bpu may do an override and the flag can be lost in such case.
    // Here we use a magic number 3 (the length of ifu pipeline):
    //   if fetchPtr is ahead 3 fetch blocks, the marked block should be in ibuffer and cannot be flushed by bpu.
    backendException := ExceptionType.None
  }

  // --------------------------------------------------------------------------------
  // Interaction with BPU
  // --------------------------------------------------------------------------------

  private val bpTrainStallCnt = RegInit(0.U((log2Ceil(BpTrainStallLimit) + 1).W))
  when(io.toBpu.train.valid && !io.toBpu.train.ready) {
    bpTrainStallCnt := bpTrainStallCnt + 1.U
  }.otherwise {
    bpTrainStallCnt := 0.U
  }

  // We limit the distance between BP and IF and stall counts of BP train so that branch update can be written back to
  // BPU
  io.fromBpu.prediction.ready := distanceBetween(bpuPtr(0), commitPtr(0)) < FtqSize.U &&
    distanceBetween(bpuPtr(0), fetchPtr(0)) < BpRunAheadDistance.U &&
    bpTrainStallCnt < BpTrainStallLimit.U
  io.fromBpu.meta.ready := true.B

  private val prediction = io.fromBpu.prediction

  private val bpuS3Redirect = prediction.valid && prediction.bits.s3Override

  io.toBpu.bpuPtr := bpuPtr(0)
  private val bpuEnqueue = prediction.fire && !redirect.valid

  private val predictionPtr = MuxCase(
    bpuPtr(0),
    Seq(
      prediction.bits.s3Override -> io.fromBpu.s3FtqPtr
    )
  )

  when(prediction.bits.s3Override) {
    bpuPtr := io.fromBpu.s3FtqPtr + 1.U
  }.elsewhen(bpuEnqueue) {
    bpuPtr := bpuPtr + 1.U
  }

  when((prediction.fire || bpuS3Redirect) && !redirect.valid) {
    entryQueue(predictionPtr.value).startPc        := prediction.bits.startPc
    entryQueue(predictionPtr.value).takenCfiOffset := prediction.bits.takenCfiOffset
  }

  private val s3PerfQueue = WireInit(perfQueue)
  when(io.fromBpu.meta.valid) {
    val s3BpuPtr = io.fromBpu.s3FtqPtr.value
    metaQueueRedirect(s3BpuPtr) := io.fromBpu.meta.bits.redirectMeta
    metaQueueResolve(s3BpuPtr)  := io.fromBpu.meta.bits.resolveMeta
    metaQueueCommit(s3BpuPtr)   := io.fromBpu.meta.bits.commitMeta

    s3PerfQueue(s3BpuPtr).bpuPerf := io.fromBpu.perfMeta
    s3PerfQueue(s3BpuPtr).isCfi.foreach(_ := false.B)
    s3PerfQueue(s3BpuPtr).mispredict := false.B
  }

  resolveQueue.io.bpuEnqueue    := bpuEnqueue
  resolveQueue.io.bpuEnqueuePtr := predictionPtr

  // --------------------------------------------------------------------------------
  // Interaction with ICache and IFU
  // --------------------------------------------------------------------------------

  when(io.toICache.toPrefetch.fire) {
    val twoPrefetchValid = io.toICache.toPrefetch.bits.twoPrefetchCase.valid
    pfPtr := Mux(twoPrefetchValid, pfPtr + 2.U, pfPtr + 1.U)
  }
  when(io.toICache.toWayLookup.fire) {
    fetchPtr := Mux(io.fromICache.fromWayLookup.realTwoFetchValid, fetchPtr + 2.U, fetchPtr + 1.U)
  }

  // TODO: wait for Ifu/ICache to remove bpu s2 flush
  for (stage <- 3 to 3) {
    val redirect = if (stage == 3) prediction.bits.s3Override else false.B
    val ftqIdx   = if (stage == 3) io.fromBpu.s3FtqPtr else 0.U.asTypeOf(new FtqPtr)

    io.toICache.flushFromBpu.stage(stage).valid := redirect
    io.toICache.flushFromBpu.stage(stage).bits  := ftqIdx
    io.toIfu.flushFromBpu.stage(stage).valid    := redirect
    io.toIfu.flushFromBpu.stage(stage).bits     := ftqIdx

    when(redirect) {
      when(pfPtr >= ftqIdx) {
        pfPtr := ftqIdx
      }
      when(fetchPtr >= ftqIdx) {
        fetchPtr := ftqIdx
      }
    }
  }

  // --------------------------------------------------------------------------------
  // 2-prefetch
  // --------------------------------------------------------------------------------

  private val prefetchReq = VecInit(
    Wire(new FtqPrefetchReq).fromFtqEntry(entryQueue(pfPtr(0).value)),
    Wire(new FtqPrefetchReq).fromFtqEntry(entryQueue(pfPtr(1).value))
  )

  private val canTwoPrefetch =
    // magic number 3: to simplify ICache/Ifu bpuFlush logic, we ask the second fetch block to be flushed within Ftq,
    // i.e. the following 2-prefetch (fb0/1) is safe, as fb1 had passed bpu s3 (which is the last chance of override).
    // bpu -> | fb4 | fb3 | fb2 | fb1 | fb0 | -> prefetch
    //        bpuPtr                   pfPtr
    //      bpu s1    s2    s3
    // and the following is not, we mark canTwoPrefetch=false
    // bpu -> | fb3 | fb2 | fb1 | fb0 | -> prefetch
    //        bpuPtr             pfPtr
    //      bpu s1    s2    s3
    // Therefore, we check if distanceBetween(bpuPtr(0), pfPtr(0)) (i.e. bpuPtr - pfPtr) > 3
    // NOTE: this is not portable, if we change the stage count of Bpu, we need to change this too
    distanceBetween(bpuPtr(0), pfPtr(0)) > 3.U &&
      // they also need to be on the same page, to prevent extra itlb port
      prefetchReq(0).vPageNumber === prefetchReq(1).vPageNumber &&
      // and they cannot have known exception, otherwise we'll prefetch on the wrong path
      !(backendException.hasException && (backendExceptionPtr === pfPtr(0) || backendExceptionPtr === pfPtr(1)))

  // (io.toICache.toPrefetch.fire && twoPrefetchValid) is passed to apply(..., canAssert) to prevent assert(x-state)
  private val twoPrefetchCase = TwoPrefetchCase(prefetchReq, io.toICache.toPrefetch.fire && canTwoPrefetch)

  // FIXME: backend redirect delay should be more than ITLB csr delay
  io.toICache.toPrefetch.valid := bpuPtr(0) > pfPtr(0) && !redirect.valid
  io.toICache.toPrefetch.bits.req.zipWithIndex.foreach { case (req, i) =>
    req.startVAddr       := prefetchReq(i).startVAddr
    req.nextLineVAddr    := req.startVAddr + blockBytes.U
    req.isCrossLine      := prefetchReq(i).isCrossLine
    req.ftqIdx           := pfPtr(i)
    req.backendException := Mux(backendExceptionPtr === pfPtr(i), backendException, ExceptionType.None)
    req.isSoftPrefetch   := false.B
  }
  io.toICache.toPrefetch.bits.twoPrefetchCase := Mux(canTwoPrefetch, twoPrefetchCase, TwoPrefetchCase.Conflict)

  // --------------------------------------------------------------------------------
  // 2-fetch
  // --------------------------------------------------------------------------------

  private val fetchReq = VecInit(
    Wire(new FtqFetchReq).fromFtqEntry(entryQueue(fetchPtr(0).value)),
    Wire(new FtqFetchReq).fromFtqEntry(entryQueue(fetchPtr(1).value))
  )

  private val rawTwoFetchValid = distanceBetween(bpuPtr(0), fetchPtr(0)) > 3.U &&
    (fetchReq(0).size +& fetchReq(1).size) <= FetchBlockInstNum.U && // the unit of fetchReq size is half-word
    fetchReq(0).vPageNumber === fetchReq(1).vPageNumber &&
    !(backendException.hasException && (
      backendExceptionPtr === fetchPtr(0) || backendExceptionPtr === fetchPtr(1)
    ))

  io.toICache.toWayLookup.valid := bpuPtr(0) > fetchPtr(0) && !redirect.valid &&
    distanceBetween(fetchPtr(0), commitPtr(0)) < (FtqSize - 1).U
  io.toICache.toWayLookup.bits.req.zipWithIndex.foreach { case (req, i) =>
    req.valid               := (if (i == 0) true.B else rawTwoFetchValid)
    req.startVAddr          := fetchReq(i).startVAddr
    req.nextLineVAddr       := fetchReq(i).nextLineVAddr
    req.takenCfiOffset      := entryQueue(fetchPtr(i).value).takenCfiOffset
    req.isCrossLine         := fetchReq(i).isCrossLine
    req.ftqIdx              := fetchPtr(i)
    req.bankSel             := fetchReq(i).bankSel
    req.vSetIdx             := fetchReq(i).vSetIdx
    req.hasBackendException := backendException.hasException && backendExceptionPtr === fetchPtr(i)
  }

  // --------------------------------------------------------------------------------
  // Interaction with backend
  // --------------------------------------------------------------------------------

  io.toBackend.wen     := (prediction.fire || bpuS3Redirect) && !redirect.valid
  io.toBackend.ftqIdx  := predictionPtr.value
  io.toBackend.startPc := prediction.bits.startPc

  // --------------------------------------------------------------------------------
  // Redirect from backend and IFU
  // --------------------------------------------------------------------------------

  io.toICache.redirectFlush := redirect.valid
  when(redirect.valid) {
    val newFtqIdx = redirect.bits.newFtqIdx // redirect.newFtqIdx is a def, make it a val here to prevent dup logic
    Seq(bpuPtr, pfPtr, fetchPtr).foreach(_ := newFtqIdx)
  }

  io.toIfu.redirect.valid := backendRedirect.valid
  // TODO: only valid should be needed
  io.toIfu.redirect.bits := DontCare

  io.toBpu.redirect.valid          := redirect.valid
  io.toBpu.redirect.bits.cfiPc     := getCfiPcFromOffset(PrunedAddrInit(redirect.bits.pc), redirect.bits.ftqOffset)
  io.toBpu.redirect.bits.target    := redirect.bits.target
  io.toBpu.redirect.bits.taken     := redirect.bits.taken
  io.toBpu.redirect.bits.attribute := redirect.bits.attribute
  io.toBpu.redirect.bits.meta      := RegNext(metaQueueRedirect(redirectFtqIdxInAdvance.value))
  io.toBpu.redirectFromIFU         := ifuRedirect.valid

  resolveQueue.io.backendRedirect    := backendRedirect.valid
  resolveQueue.io.backendRedirectPtr := backendRedirect.bits.ftqIdx

  // --------------------------------------------------------------------------------
  // Resolve and train BPU
  // --------------------------------------------------------------------------------

  resolveQueue.io.backendResolve := io.fromBackend.resolve
  resolveQueue.io.ifuResolve     := ifuResolve

  private val trainCache      = RegInit(0.U.asTypeOf(Valid(new BpuTrain)))
  private val trainIndexCache = RegInit(0.U.asTypeOf(new FtqPtr))

  resolveQueue.io.bpuTrain.ready := !trainCache.valid || io.toBpu.train.fire

  private val flushTrainCache =
    backendRedirect.valid && trainCache.valid && trainIndexCache > backendRedirect.bits.ftqIdx

  when(flushTrainCache) {
    trainCache.valid := false.B
  }.elsewhen(resolveQueue.io.bpuTrain.fire) {
    val needFlush = backendRedirect.valid && resolveQueue.io.bpuTrain.bits.ftqIdx > backendRedirect.bits.ftqIdx
    trainCache.valid     := !needFlush
    trainCache.bits.meta := metaQueueResolve(resolveQueue.io.bpuTrain.bits.ftqIdx.value)
    trainCache.bits.startPcVec.foreach { dup =>
      dup.zipWithIndex.foreach { case (startPc, i) =>
        if (i == 0)
          startPc := resolveQueue.io.bpuTrain.bits.startPc // do not align startPcVec.head
        else
          startPc := getAlignedPc(resolveQueue.io.bpuTrain.bits.startPc + (i << FetchBlockAlignWidth).U)
      }
    }
    trainCache.bits.branches     := resolveQueue.io.bpuTrain.bits.branches
    trainCache.bits.perfMeta     := perfQueue(resolveQueue.io.bpuTrain.bits.ftqIdx.value).bpuPerf
    trainCache.bits.debug_source := resolveQueue.io.bpuTrain.bits.debug_source
    trainIndexCache              := resolveQueue.io.bpuTrain.bits.ftqIdx
  }.elsewhen(io.toBpu.train.fire) {
    trainCache.valid := false.B
  }

  io.toBpu.train.valid := trainCache.valid && !flushTrainCache
  io.toBpu.train.bits  := trainCache.bits

  // default next state receives s3 prediction meta
  perfQueue := s3PerfQueue

  // resolve override next state
  private val lastPerfMetas = Wire(Vec(backendParams.BrhCnt, new PerfMeta))
  io.fromBackend.resolve.zipWithIndex.foreach { case (branch, i) =>
    val ftqIdx = branch.bits.ftqIdx.value
    val lastPerfMeta = WireInit(MuxCase(
      s3PerfQueue(ftqIdx),
      (0 until i).reverse.map { j =>
        val prevBranch = io.fromBackend.resolve(j)
        (prevBranch.valid && prevBranch.bits.ftqIdx.value === ftqIdx) -> lastPerfMetas(j)
      }
    ))
    val curPerfMeta = WireInit(lastPerfMeta)

    when(branch.valid) {
      val newBranchInfo = Wire(new BranchInfo)
      newBranchInfo.fromResolve(branch.bits)

      val curOH     = UIntToOH(newBranchInfo.cfiPosition, FetchBlockInstNum)
      val validMask = UIntToMask(newBranchInfo.cfiPosition +& 1.U, FetchBlockInstNum)
      val beforeKnownMispredict =
        Mux(
          lastPerfMeta.mispredict,
          newBranchInfo.cfiPosition < lastPerfMeta.mispredictBranchInfo.cfiPosition,
          true.B
        )

      when(beforeKnownMispredict) {
        curPerfMeta.isCfi(newBranchInfo.cfiPosition)   := true.B
        curPerfMeta.cfiAttr(newBranchInfo.cfiPosition) := newBranchInfo.attribute
        when(branch.bits.mispredict) {
          curPerfMeta.mispredict           := true.B
          curPerfMeta.mispredictBranchInfo := newBranchInfo
          curPerfMeta.isCfi                := ((lastPerfMeta.isCfi.asUInt | curOH) & validMask).asBools
        }
      }
      perfQueue(ftqIdx) := curPerfMeta
    }
    lastPerfMetas(i) := curPerfMeta
  }

  // --------------------------------------------------------------------------------
  // Commit and train BPU
  // --------------------------------------------------------------------------------

  // Backend may send commit for on entry multiple times, but the entry is actually committed when it is committed for
  // the first time. The rest of the commits can be ignored.
  private val robCommitPtr = DataHoldBypass(
    io.fromBackend.commit.bits,
    FtqPtr(true.B, (FtqSize - 1).U),
    io.fromBackend.commit.valid
  )
  private val commit = commitPtr <= robCommitPtr
  when(commit) {
    commitPtr := commitPtr + 1.U
  }

  commitQueue.io.backendCommit := io.fromBackend.callRetCommit

  io.toBpu.commit.valid                     := commitQueue.io.bpuTrain.valid
  io.toBpu.commit.bits.meta                 := metaQueueCommit(commitQueue.io.bpuTrain.bits.ftqPtr.value)
  io.toBpu.commit.bits.attribute.branchType := DontCare
  io.toBpu.commit.bits.attribute.rasAction  := commitQueue.io.bpuTrain.bits.rasAction

  // --------------------------------------------------------------------------------
  // Performance monitoring
  // --------------------------------------------------------------------------------

  // Topdown analysis
  io.backendRedirectTopdown.backendRedirect         := backendRedirect.valid
  io.backendRedirectTopdown.controlFlowRedirect     := backendRedirect.bits.debugIsCtrl
  io.backendRedirectTopdown.memoryViolationRedirect := backendRedirect.bits.debugIsMemVio

  io.backendRedirectTopdown.btbMissBubble    := false.B // TODO: add more info to distinguish
  io.backendRedirectTopdown.tageMissBubble   := backendRedirect.bits.attribute.isConditional
  io.backendRedirectTopdown.scMissBubble     := false.B // TODO: add SC info
  io.backendRedirectTopdown.ittageMissBubble := backendRedirect.bits.attribute.needIttage
  io.backendRedirectTopdown.rasMissBubble    := backendRedirect.bits.attribute.isReturn

  private val topdownStage = RegInit(0.U.asTypeOf(new FrontendTopDownBundle))
  // only driven by clock, not valid-ready
  topdownStage := io.fromBpu.topdownReasons
  topdownStage.backendRedirectOverride(io.backendRedirectTopdown)
  io.toIfu.topdownInfo := topdownStage

  when(!(distanceBetween(bpuPtr(0), commitPtr(0)) < FtqSize.U)) {
    topdownStage.reasons(TopDownCounters.FtqFullStall.id) := true.B
  }.elsewhen(
    !(distanceBetween(bpuPtr(0), fetchPtr(0)) < BpRunAheadDistance.U && bpTrainStallCnt < BpTrainStallLimit.U)
  ) {
    topdownStage.reasons(TopDownCounters.FtqUpdateBubble.id) := true.B
  }

  // Hardware performance monitors
  val perfEvents: Seq[(String, UInt)] = Seq()
  generatePerfEvent()

  // XSPerfCounters
  private val redirectCfiOffset = getAlignedPosition(
    PrunedAddrInit(redirect.bits.pc),
    redirect.bits.ftqOffset
  )._1
  private val redirectPerfMeta = perfQueue(backendRedirect.bits.ftqIdx.value).bpuPerf
  private val commitPerfMeta   = perfQueue(commitPtr(0).value)

  XSPerfSeqAccumulate(
    "squash_cycles_bp_wrong_redirect",
    backendRedirect.valid && backendRedirect.bits.isMisPred,
    Seq(
      ("wrong_taken", redirect.bits.taken =/= redirectPerfMeta.bpPred.taken),
      ("wrong_position", redirectCfiOffset =/= redirectPerfMeta.bpPred.cfiPosition),
      ("wrong_attribute", !(redirect.bits.attribute === redirectPerfMeta.bpPred.attribute)),
      ("wrong_target", redirect.bits.target =/= redirectPerfMeta.bpPred.target.toUInt)
    ),
    withPriority = true
  )

  XSPerfSeqAccumulate(
    "squash_cycles_bp_wrong_redirect_wrong_target",
    backendRedirect.valid && backendRedirect.bits.isMisPred &&
      redirect.bits.taken === redirectPerfMeta.bpPred.taken &&
      redirectCfiOffset === redirectPerfMeta.bpPred.cfiPosition &&
      redirect.bits.attribute === redirectPerfMeta.bpPred.attribute &&
      redirect.bits.target =/= redirectPerfMeta.bpPred.target.toUInt,
    Seq(
      ("conditional", redirect.bits.attribute.isConditional),
      ("direct", redirect.bits.attribute.isDirect),
      ("indirect", redirect.bits.attribute.isIndirect),
      ("indirect_ret_call", redirect.bits.attribute.isReturnAndCall && redirect.bits.attribute.isIndirect)
    )
  )

  private val perf_mispredS1SourceVec = BpuPredictionSource.Stage1.getValidSeq(redirectPerfMeta.bpSource.s1Source)
  private val perf_mispredS3SourceVec = BpuPredictionSource.Stage3.getValidSeq(redirectPerfMeta.bpSource.s3Source)

  XSPerfSeqAccumulate(
    "resolve_branch_mispredicts_s1_source",
    backendRedirect.valid && backendRedirect.bits.isMisPred && !redirectPerfMeta.bpSource.s3Override,
    perf_mispredS1SourceVec
  )

  XSPerfSeqAccumulate(
    "resolve_branch_mispredicts_s3_source",
    backendRedirect.valid && backendRedirect.bits.isMisPred && redirectPerfMeta.bpSource.s3Override,
    perf_mispredS3SourceVec
  )
  XSPerfAccumulate("resolve_redirects", backendRedirect.valid)
  XSPerfAccumulate("resolve_branch_mispredicts", backendRedirect.valid && backendRedirect.bits.isMisPred)
  XSPerfAccumulate("resolve_other_redirects", backendRedirect.valid && !backendRedirect.bits.isMisPred)

  // Commit-time statistics, should be correct-path only
  XSPerfSeqAccumulate(
    "commit_branch",
    commit,
    Seq(
      ("num", true.B, PopCount(commitPerfMeta.isCfi)),
      ("mispredicts", true.B, commitPerfMeta.mispredict)
    )
  )

  private def PerfNumCfiWithAttr(
      perfMeta: PerfMeta,
      withAttr: BranchAttribute => Bool
  ): UInt =
    PopCount(perfMeta.isCfi zip perfMeta.cfiAttr map { case (v, attr) => v && withAttr(attr) })

  XSPerfSeqAccumulate(
    "commit_branch_type",
    commit,
    Seq(
      ("conditional", true.B, PerfNumCfiWithAttr(commitPerfMeta, attr => attr.isConditional)),
      ("direct", true.B, PerfNumCfiWithAttr(commitPerfMeta, attr => attr.isDirect)),
      ("indirect", true.B, PerfNumCfiWithAttr(commitPerfMeta, attr => attr.isIndirect)),
      (
        "indirect_retcall",
        true.B,
        PerfNumCfiWithAttr(commitPerfMeta, attr => attr.isReturnAndCall && attr.isIndirect)
      ),
      ("call", true.B, PerfNumCfiWithAttr(commitPerfMeta, attr => attr.isCall)),
      ("ret", true.B, PerfNumCfiWithAttr(commitPerfMeta, attr => attr.isReturn))
    )
  )

  private val perf_commitHasMispredict = commit && commitPerfMeta.mispredict
  private val perf_commitHasMispredictConditional =
    perf_commitHasMispredict && commitPerfMeta.mispredictBranchInfo.attribute.isConditional

  XSPerfSeqAccumulate(
    "commit_branch_mispredicts_s1_mispred_s1_source",
    perf_commitHasMispredict && !commitPerfMeta.bpuPerf.bpSource.s3Override,
    BpuPredictionSource.Stage1.getValidSeq(commitPerfMeta.bpuPerf.bpSource.s1Source)
  )
  XSPerfSeqAccumulate(
    "commit_branch_mispredicts_s1_source",
    perf_commitHasMispredict,
    BpuPredictionSource.Stage1.getValidSeq(commitPerfMeta.bpuPerf.bpSource.s1Source)
  )
  XSPerfSeqAccumulate(
    "commit_branch_mispredicts_s3_source",
    perf_commitHasMispredict,
    BpuPredictionSource.Stage3.getValidSeq(commitPerfMeta.bpuPerf.bpSource.s3Source)
  )
  XSPerfSeqAccumulate(
    "commit_branch_mispredicts_reason",
    perf_commitHasMispredict,
    BlameBpuSource.BlameType.getValidSeq(BlameBpuSource(
      perf_commitHasMispredict,
      commitPerfMeta.bpuPerf,
      commitPerfMeta.mispredictBranchInfo
    ))
  )
  XSPerfSeqAccumulate(
    "commit_conditional_branch_mispredicts_reason",
    perf_commitHasMispredictConditional,
    BlameBpuSource.BlameType.getValidSeq(BlameBpuSource(
      perf_commitHasMispredictConditional,
      commitPerfMeta.bpuPerf,
      commitPerfMeta.mispredictBranchInfo
    ))
  )

  private val mispredictAttr = commitPerfMeta.mispredictBranchInfo.attribute
  XSPerfSeqAccumulate(
    "commit_branch_mispredicts_type",
    perf_commitHasMispredict,
    Seq(
      ("conditional", mispredictAttr.isConditional),
      ("direct", mispredictAttr.isDirect),
      ("indirect", mispredictAttr.isIndirect),
      ("indirect_retcall", mispredictAttr.isReturnAndCall && mispredictAttr.isIndirect),
      ("call", mispredictAttr.isCall),
      ("ret", mispredictAttr.isReturn)
    )
  )

  XSPerfSeqRolling(
    "rolling_commit_mispredict",
    perf_commitHasMispredict,
    Seq(
      ("conditional", mispredictAttr.isConditional),
      ("direct", mispredictAttr.isDirect),
      ("indirect", mispredictAttr.isIndirect),
      ("indirect_retcall", mispredictAttr.isReturnAndCall && mispredictAttr.isIndirect),
      ("call", mispredictAttr.isCall),
      ("ret", mispredictAttr.isReturn)
    ),
    10000,
    clock,
    reset
  )

  XSPerfRolling("rolling_ifu_redirect", io.fromIfu.wbRedirect.valid, 10000, clock, reset)

  XSPerfHistogram(
    "distance_between_bpu_commit",
    distanceBetween(bpuPtr(0), commitPtr(0)),
    true.B,
    0,
    FtqSize + 1
  )
  XSPerfHistogram(
    "distance_between_ifu_commit",
    distanceBetween(fetchPtr(0), commitPtr(0)),
    true.B,
    0,
    FtqSize + 1
  )
  XSPerfHistogram(
    "distance_between_bpu_ifu",
    distanceBetween(bpuPtr(0), fetchPtr(0)),
    true.B,
    0,
    FtqSize + 1
  )
  XSPerfAccumulate(
    "total_commits",
    commit
  )
  XSPerfSeqAccumulate(
    "2prefetch",
    io.toICache.toPrefetch.fire && io.toICache.toPrefetch.bits.twoPrefetchCase.valid,
    Seq(
      ("total", true.B)
    ) ++ io.toICache.toPrefetch.bits.twoPrefetchCase.getValidSeq
  )
  XSPerfSeqAccumulate(
    "2prefetch_fail_reason",
    io.toICache.toPrefetch.fire && !io.toICache.toPrefetch.bits.twoPrefetchCase.valid,
    Seq(
      ("fb_not_enough", distanceBetween(bpuPtr(0), pfPtr(0)) <= 3.U),
      ("fb1_exception", backendException.hasException && backendExceptionPtr === pfPtr(0)),
      ("fb2_exception", backendException.hasException && backendExceptionPtr === pfPtr(1)),
      ("page_conflict", prefetchReq(0).vPageNumber =/= prefetchReq(1).vPageNumber),
      ("sram_conflict", twoPrefetchCase.isConflict)
    ),
    withPriority = true
  )
  XSPerfAccumulate(
    "total_fetch",
    io.toICache.toWayLookup.fire
  )
  XSPerfAccumulate(
    "1fetch",
    io.toICache.toWayLookup.fire && !io.fromICache.fromWayLookup.realTwoFetchValid
  )
  XSPerfAccumulate(
    "2fetch",
    io.toICache.toWayLookup.fire && io.fromICache.fromWayLookup.realTwoFetchValid
  )
  XSPerfSeqAccumulate(
    "2fetch_fail_reason",
    io.toICache.toWayLookup.fire && !io.fromICache.fromWayLookup.realTwoFetchValid,
    Seq(
      ("fb_not_enough", distanceBetween(bpuPtr(0), fetchPtr(0)) <= 3.U),
      ("fb1_exception", backendException.hasException && backendExceptionPtr === fetchPtr(0)),
      ("fb2_exception", backendException.hasException && backendExceptionPtr === fetchPtr(1)),
      ("total_size", (fetchReq(0).size +& fetchReq(1).size) > FetchBlockInstNum.U),
      ("page_conflict", fetchReq(0).vPageNumber =/= fetchReq(1).vPageNumber),
      ("can_not_serve_two_meta", io.fromICache.fromWayLookup.perf_canNotServeTwoMeta),
      ("sram_conflict", io.fromICache.fromWayLookup.perf_dataSramReadConflict),
      ("has_mmio", io.fromICache.fromWayLookup.perf_hasMmio),
      ("has_itlb_exception", io.fromICache.fromWayLookup.perf_hasItlbException)
    ),
    withPriority = true
  )
}
