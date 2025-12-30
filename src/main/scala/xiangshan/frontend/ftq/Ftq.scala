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
import utility.XSPerfPriorityAccumulate
import xiangshan.RedirectLevel
import xiangshan.TopDownCounters
import xiangshan.backend.CtrlToFtqIO
import xiangshan.frontend.BlameBpuSource
import xiangshan.frontend.BpuPerfInfo
import xiangshan.frontend.BpuToFtqIO
import xiangshan.frontend.BpuTopDownInfo
import xiangshan.frontend.ExceptionType
import xiangshan.frontend.FetchRequestBundle
import xiangshan.frontend.FrontendTopDownBundle
import xiangshan.frontend.FtqToBpuIO
import xiangshan.frontend.FtqToICacheIO
import xiangshan.frontend.FtqToIfuIO
import xiangshan.frontend.IfuToFtqIO
import xiangshan.frontend.PrunedAddrInit
import xiangshan.frontend.bpu.BpuMeta
import xiangshan.frontend.bpu.BpuPredictionSource
import xiangshan.frontend.bpu.BpuSpeculationMeta
import xiangshan.frontend.bpu.HalfAlignHelper
import xiangshan.frontend.bpu.ras.RasMeta

class Ftq(implicit p: Parameters) extends FtqModule
    with HalfAlignHelper
    with HasPerfEvents
    with HasCircularQueuePtrHelper
    with IfuRedirectReceiver
    with BackendRedirectReceiver {

  class FtqIO extends FtqBundle {
    val fromBpu: BpuToFtqIO = Flipped(new BpuToFtqIO)
    val toBpu:   FtqToBpuIO = new FtqToBpuIO

    val fromIfu: IfuToFtqIO = Flipped(new IfuToFtqIO)
    val toIfu:   FtqToIfuIO = new FtqToIfuIO

    val toICache: FtqToICacheIO = new FtqToICacheIO

    val fromBackend: CtrlToFtqIO = Flipped(new CtrlToFtqIO)
    val toBackend:   FtqToCtrlIO = new FtqToCtrlIO

    val bpuInfo: BpuPerfInfo = Output(new BpuPerfInfo)

    // for perf
    val bpuTopDownInfo: BpuTopDownInfo = Output(new BpuTopDownInfo)
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
  // TODO: Reconsider this
//  XSError(bpuPtr < pfPtr && !isFull(bpuPtr(0), pfPtr(0)), "pfPtr runs ahead of bpuPtr")
//  XSError(ifuWbPtr < commitPtr && !isFull(ifuWbPtr(0), commitPtr(0)), "ifuWbPtr runs ahead of commitPtr")

  // entryQueue stores predictions made by BPU.
  private val entryQueue = Reg(Vec(FtqSize, new FtqEntry))

  // speculationQueue stores speculation information needed by BPU when redirect happens.
  private val speculationQueue = Reg(Vec(FtqSize, new BpuSpeculationMeta))

  // metaQueue stores information needed to train BPU.
  private val metaQueueResolve = Reg(Vec(FtqSize, new BpuMeta))
  private val metaQueueCommit  = Reg(Vec(FtqSize, new RasMeta))

  // resolveQueue caches branch resolve information from backend.
  private val resolveQueue = Module(new ResolveQueue)

  // commitQueue caches branch commit information from backend.
  private val commitQueue = Module(new CommitQueue)

  // perfQueue stores information for performance monitoring. These queues should not exist in hardware
  private val perfQueue = Reg(Vec(FtqSize, new PerfMeta))

  private val specTopAddr = speculationQueue(io.fromIfu.wbRedirect.bits.ftqIdx.value).topRetAddr.toUInt
  private val ifuRedirect = receiveIfuRedirect(io.fromIfu.wbRedirect, specTopAddr)

  private val (backendRedirectFtqIdx, backendRedirect) = receiveBackendRedirect(io.fromBackend)

  private val redirect     = Mux(backendRedirect.valid, backendRedirect, ifuRedirect)
  private val redirectNext = RegNext(redirect)

  // Instruction page fault and instruction access fault are sent from backend with redirect requests.
  // When IPF and IAF are sent, backendPcFaultIfuPtr points to the FTQ entry whose first instruction
  // raises IPF or IAF, which is ifuWbPtr_write or IfuPtr_write.
  // Only when IFU has written back that FTQ entry can backendIpf and backendIaf be false because this
  // makes sure that IAF and IPF are correctly raised instead of being flushed by redirect requests.
  private val backendException    = RegInit(ExceptionType.None)
  private val backendExceptionPtr = RegInit(FtqPtr(false.B, 0.U))
  when(backendRedirect.valid) {
    val exception = ExceptionType.fromBackend(backendRedirect.bits)
    backendException := exception
    when(exception.hasException) {
      backendExceptionPtr := ifuWbPtr(0)
    }
  }.elsewhen(ifuWbPtr(0) =/= backendExceptionPtr) {
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
    distanceBetween(bpuPtr(0), ifuPtr(0)) < BpRunAheadDistance.U &&
    bpTrainStallCnt < BpTrainStallLimit.U
  io.fromBpu.meta.ready            := true.B
  io.fromBpu.speculationMeta.ready := true.B

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

  speculationQueue(io.fromBpu.s3FtqPtr.value) := io.fromBpu.speculationMeta.bits

  when(io.fromBpu.meta.valid) {
    val s3BpuPtr = io.fromBpu.s3FtqPtr.value
    metaQueueResolve(s3BpuPtr) := io.fromBpu.meta.bits
    metaQueueCommit(s3BpuPtr)  := io.fromBpu.meta.bits.ras

    perfQueue(s3BpuPtr).bpuPerf := io.fromBpu.perfMeta
    perfQueue(s3BpuPtr).isCfi.foreach(_ := false.B)
    perfQueue(s3BpuPtr).mispredict := false.B
  }

  resolveQueue.io.bpuEnqueue    := bpuEnqueue
  resolveQueue.io.bpuEnqueuePtr := predictionPtr

  // --------------------------------------------------------------------------------
  // Interaction with ICache and IFU
  // --------------------------------------------------------------------------------

  when(io.toICache.prefetchReq.fire) {
    pfPtr := pfPtr + 1.U
  }
  when(io.toIfu.req.fire) {
    ifuPtr := ifuPtr + 1.U
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
      when(ifuPtr >= ftqIdx) {
        ifuPtr := ftqIdx
      }
    }
  }

  // FIXME: backend redirect delay should be more than ITLB csr delay
  io.toICache.prefetchReq.valid := (bpuPtr(0) > pfPtr(0) || redirectNext.valid) && !redirect.valid
  io.toICache.prefetchReq.bits.startVAddr := Mux(
    redirectNext.valid,
    PrunedAddrInit(redirectNext.bits.target),
    entryQueue(pfPtr(0).value).startPc
  )
  io.toICache.prefetchReq.bits.nextCachelineVAddr :=
    io.toICache.prefetchReq.bits.startVAddr + (CacheLineSize / 8).U
  io.toICache.prefetchReq.bits.ftqIdx := pfPtr(0)
  // we don't have takenCfiOffset after redirect
  io.toICache.prefetchReq.bits.takenCfiOffset := Mux(
    redirectNext.valid,
    (FetchBlockInstNum - 1).U, // assume maximum fetch block size
    entryQueue(pfPtr(0).value).takenCfiOffset.bits
  )
  io.toICache.prefetchReq.bits.backendException := Mux(
    backendExceptionPtr === pfPtr(0),
    backendException,
    ExceptionType.None
  )

  private val ifuReqValid = bpuPtr(0) > ifuPtr(0) && !redirect.valid &&
    distanceBetween(ifuPtr(0), commitPtr(0)) < (FtqSize - 1).U

  // TODO: consider BPU bypass
  io.toICache.fetchReq.valid                   := ifuReqValid
  io.toICache.fetchReq.bits.startVAddr         := entryQueue(ifuPtr(0).value).startPc
  io.toICache.fetchReq.bits.nextCachelineVAddr := entryQueue(ifuPtr(0).value).startPc + (CacheLineSize / 8).U
  io.toICache.fetchReq.bits.ftqIdx             := ifuPtr(0)
  io.toICache.fetchReq.bits.takenCfiOffset     := entryQueue(ifuPtr(0).value).takenCfiOffset.bits
  io.toICache.fetchReq.bits.isBackendException := backendException.hasException && backendExceptionPtr === ifuPtr(0)

  io.toIfu.req.valid                    := ifuReqValid
  io.toIfu.req.bits.fetch(0).valid      := ifuReqValid
  io.toIfu.req.bits.fetch(0).startVAddr := entryQueue(ifuPtr(0).value).startPc
  io.toIfu.req.bits.fetch(0).nextStartVAddr := MuxCase(
    entryQueue(ifuPtr(1).value).startPc,
    Seq(
      (bpuPtr(0) === ifuPtr(0)) -> prediction.bits.target,
      (bpuPtr(0) === ifuPtr(1)) -> prediction.bits.startPc
    )
  )
  io.toIfu.req.bits.fetch(0).nextCachelineVAddr := io.toIfu.req.bits.fetch(0).startVAddr + (CacheLineSize / 8).U
  io.toIfu.req.bits.fetch(0).ftqIdx             := ifuPtr(0)
  io.toIfu.req.bits.fetch(0).takenCfiOffset     := entryQueue(ifuPtr(0).value).takenCfiOffset

  io.toIfu.req.bits.fetch(1) := 0.U.asTypeOf(new FetchRequestBundle)

  // toIFU topdown counters
  val topdown_stage = RegInit(0.U.asTypeOf(new FrontendTopDownBundle()))
  // only driven by clock, not valid-ready
  topdown_stage                 := io.fromBpu.topdownReasons
  io.toIfu.req.bits.topdownInfo := topdown_stage
  when(backendRedirect.valid) {
    // TODO: reasoning back to each BP component
    when(backendRedirect.bits.debugIsMemVio) {
      topdown_stage.reasons(TopDownCounters.MemVioRedirectBubble.id)                 := true.B
      io.toIfu.req.bits.topdownInfo.reasons(TopDownCounters.MemVioRedirectBubble.id) := true.B
    }
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
    val newEntryPtr = Mux(
      RedirectLevel.flushItself(redirect.bits.level) &&
        (redirect.bits.ftqOffset === 0.U || redirect.bits.ftqOffset === 1.U && !redirect.bits.isRVC),
      redirect.bits.ftqIdx,
      redirect.bits.ftqIdx + 1.U
    )
    Seq(bpuPtr, ifuPtr, pfPtr).foreach(_ := newEntryPtr)
  }

  io.toIfu.redirect.valid := backendRedirect.valid
  // TODO: only valid should be needed
  io.toIfu.redirect.bits := DontCare

  io.toBpu.redirect.valid          := redirect.valid
  io.toBpu.redirect.bits.cfiPc     := getCfiPcFromOffset(PrunedAddrInit(redirect.bits.pc), redirect.bits.ftqOffset)
  io.toBpu.redirect.bits.target    := redirect.bits.target
  io.toBpu.redirect.bits.taken     := redirect.bits.taken
  io.toBpu.redirect.bits.attribute := redirect.bits.attribute
  io.toBpu.redirect.bits.speculationMeta := speculationQueue(redirect.bits.ftqIdx.value)
  io.toBpu.redirectFromIFU               := ifuRedirect.valid

  resolveQueue.io.backendRedirect    := backendRedirect.valid
  resolveQueue.io.backendRedirectPtr := backendRedirect.bits.ftqIdx

  // --------------------------------------------------------------------------------
  // Resolve and train BPU
  // --------------------------------------------------------------------------------

  resolveQueue.io.backendResolve := io.fromBackend.resolve

  io.toBpu.train.valid           := resolveQueue.io.bpuTrain.valid
  resolveQueue.io.bpuTrain.ready := io.toBpu.train.ready
  io.toBpu.train.bits.meta       := metaQueueResolve(resolveQueue.io.bpuTrain.bits.ftqIdx.value)
  io.toBpu.train.bits.startPc    := resolveQueue.io.bpuTrain.bits.startPc
  io.toBpu.train.bits.branches   := resolveQueue.io.bpuTrain.bits.branches
  io.toBpu.train.bits.perfMeta   := perfQueue(resolveQueue.io.bpuTrain.bits.ftqIdx.value).bpuPerf

  io.fromBackend.resolve.foreach { branch =>
    val ftqIdx      = branch.bits.ftqIdx.value
    val cfiPosition = getAlignedPosition(branch.bits.pc, branch.bits.ftqOffset)._1
    when(branch.valid) {
      perfQueue(ftqIdx).isCfi(cfiPosition) := true.B
      when(branch.bits.mispredict) {
        // Mark mispredict and flush the cfi after its position
        perfQueue(ftqIdx).mispredict := true.B
        perfQueue(ftqIdx).mispredictBranchInfo.fromResolve(branch.bits)
        val mask = UIntToMask(cfiPosition + 1.U, FetchBlockInstNum)
        perfQueue(ftqIdx).isCfi := (perfQueue(ftqIdx).isCfi.asUInt & mask).asBools // BUGGY: not really correct flush
      }
    }
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
  io.toBpu.commit.bits.rasMeta              := metaQueueCommit(commitQueue.io.bpuTrain.bits.ftqPtr.value)
  io.toBpu.commit.bits.attribute.branchType := DontCare
  io.toBpu.commit.bits.attribute.rasAction  := commitQueue.io.bpuTrain.bits.rasAction

  // --------------------------------------------------------------------------------
  // MMIO fetch
  // --------------------------------------------------------------------------------
  private val mmioPtr           = io.fromIfu.mmioCommitRead.mmioFtqPtr
  private val mmioValid         = io.fromIfu.mmioCommitRead.valid
  private val lastMmioCommitted = commitPtr > mmioPtr || commitPtr === mmioPtr && commit
  io.fromIfu.mmioCommitRead.mmioLastCommit := RegNext(lastMmioCommitted && mmioValid)

  // --------------------------------------------------------------------------------
  // Performance monitoring
  // --------------------------------------------------------------------------------
  io.bpuInfo := DontCare
  // io.toIfu.req.bits.topdownInfo is assigned above
  io.toIfu.topdownRedirect := backendRedirect

  io.bpuTopDownInfo.btbMissBubble    := false.B // TODO: add more info to distinguish
  io.bpuTopDownInfo.tageMissBubble   := RegNext(backendRedirect.valid && backendRedirect.bits.attribute.isConditional)
  io.bpuTopDownInfo.scMissBubble     := false.B // TODO: add SC info
  io.bpuTopDownInfo.ittageMissBubble := RegNext(backendRedirect.valid && backendRedirect.bits.attribute.needIttage)
  io.bpuTopDownInfo.rasMissBubble    := RegNext(backendRedirect.valid && backendRedirect.bits.attribute.isReturn)

  val perfEvents: Seq[(String, UInt)] = Seq()
  generatePerfEvent()

  // XSPerfCounters
  private val redirectCfiOffset = getAlignedPosition(
    PrunedAddrInit(redirect.bits.pc),
    redirect.bits.ftqOffset
  )._1
  private val redirectPerfMeta = perfQueue(backendRedirectFtqIdx.bits.value).bpuPerf
  private val commitPerfMeta   = perfQueue(commitPtr(0).value)

  XSPerfPriorityAccumulate(
    "squash_cycles_bp_wrong_redirect",
    backendRedirect.valid && backendRedirect.bits.isMisPred,
    Seq(
      ("wrong_taken", redirect.bits.taken =/= redirectPerfMeta.bpPred.taken),
      ("wrong_position", redirectCfiOffset =/= redirectPerfMeta.bpPred.cfiPosition),
      ("wrong_attribute", !(redirect.bits.attribute === redirectPerfMeta.bpPred.attribute)),
      ("wrong_target", redirect.bits.target =/= redirectPerfMeta.bpPred.target.toUInt)
    )
  )

  XSPerfAccumulate(
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

  XSPerfAccumulate(
    "resolve_branch_mispredicts_s1_source",
    backendRedirect.valid && backendRedirect.bits.isMisPred && !redirectPerfMeta.bpSource.s3Override,
    perf_mispredS1SourceVec
  )

  XSPerfAccumulate(
    "resolve_branch_mispredicts_s3_source",
    backendRedirect.valid && backendRedirect.bits.isMisPred && redirectPerfMeta.bpSource.s3Override,
    perf_mispredS3SourceVec
  )
  XSPerfAccumulate("resolve_redirects", backendRedirect.valid)
  XSPerfAccumulate("resolve_branch_mispredicts", backendRedirect.valid && backendRedirect.bits.isMisPred)
  XSPerfAccumulate("resolve_other_redirects", backendRedirect.valid && !backendRedirect.bits.isMisPred)

  // Commit-time statistics, should be correct-path only
  XSPerfAccumulate(
    "commit_branch",
    commit,
    Seq(
      ("num", true.B, PopCount(commitPerfMeta.isCfi)),
      ("mispredicts", true.B, commitPerfMeta.mispredict)
    )
  )
  XSPerfAccumulate(
    "commit_branch_type",
    commit,
    Seq(
      ("conditional", commitPerfMeta.mispredictBranchInfo.attribute.isConditional),
      ("direct", commitPerfMeta.mispredictBranchInfo.attribute.isDirect),
      ("indirect", commitPerfMeta.mispredictBranchInfo.attribute.isIndirect),
      (
        "indirect_retcall",
        commitPerfMeta.mispredictBranchInfo.attribute.isReturnAndCall
          && commitPerfMeta.mispredictBranchInfo.attribute.isIndirect
      ),
      ("call", commitPerfMeta.mispredictBranchInfo.attribute.isCall),
      ("ret", commitPerfMeta.mispredictBranchInfo.attribute.isReturn)
    )
  )
  XSPerfAccumulate(
    "commit_branch_mispredicts_s1_source",
    commit && commitPerfMeta.mispredict,
    BpuPredictionSource.Stage1.getValidSeq(commitPerfMeta.bpuPerf.bpSource.s1Source)
  )
  XSPerfAccumulate(
    "commit_branch_mispredicts_s3_source",
    commit && commitPerfMeta.mispredict,
    BpuPredictionSource.Stage3.getValidSeq(commitPerfMeta.bpuPerf.bpSource.s3Source)
  )
  XSPerfAccumulate(
    "commit_branch_mispredicts_reason",
    commit && commitPerfMeta.mispredict,
    BlameBpuSource.BlameType.getValidSeq(BlameBpuSource(commitPerfMeta.bpuPerf, commitPerfMeta.mispredictBranchInfo))
  )
  XSPerfAccumulate(
    "commit_conditional_branch_mispredicts_reason",
    commit && commitPerfMeta.mispredict && commitPerfMeta.mispredictBranchInfo.attribute.isConditional,
    BlameBpuSource.BlameType.getValidSeq(BlameBpuSource(commitPerfMeta.bpuPerf, commitPerfMeta.mispredictBranchInfo))
  )
  XSPerfAccumulate(
    "commit_branch_mispredicts_type",
    commit && commitPerfMeta.mispredict,
    Seq(
      ("conditional", commitPerfMeta.mispredictBranchInfo.attribute.isConditional),
      ("direct", commitPerfMeta.mispredictBranchInfo.attribute.isDirect),
      ("indirect", commitPerfMeta.mispredictBranchInfo.attribute.isIndirect),
      (
        "indirect_retcall",
        commitPerfMeta.mispredictBranchInfo.attribute.isReturnAndCall
          && commitPerfMeta.mispredictBranchInfo.attribute.isIndirect
      ),
      ("call", commitPerfMeta.mispredictBranchInfo.attribute.isCall),
      ("ret", commitPerfMeta.mispredictBranchInfo.attribute.isReturn)
    )
  )

  XSPerfHistogram(
    "distance_between_bpu_commit",
    distanceBetween(bpuPtr(0), commitPtr(0)),
    true.B,
    0,
    FtqSize + 1
  )
  XSPerfHistogram(
    "distance_between_ifu_commit",
    distanceBetween(ifuPtr(0), commitPtr(0)),
    true.B,
    0,
    FtqSize + 1
  )
  XSPerfHistogram(
    "distance_between_bpu_ifu",
    distanceBetween(bpuPtr(0), ifuPtr(0)),
    true.B,
    0,
    FtqSize + 1
  )
  XSPerfAccumulate(
    "total_commits",
    commit
  )
}
