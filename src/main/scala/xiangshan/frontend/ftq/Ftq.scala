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
import xiangshan.RedirectLevel
import xiangshan.backend.CtrlToFtqIO
import xiangshan.frontend.BpuToFtqIO
import xiangshan.frontend.ExceptionType
import xiangshan.frontend.FetchRequestBundle
import xiangshan.frontend.FtqToBpuIO
import xiangshan.frontend.FtqToICacheIO
import xiangshan.frontend.FtqToIfuIO
import xiangshan.frontend.IfuToFtqIO
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.PrunedAddrInit
import xiangshan.frontend.bpu.BpuSpeculationMeta

class Ftq(implicit p: Parameters) extends FtqModule
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
  // TODO: Reconsider this
//  XSError(bpuPtr < pfPtr && !isFull(bpuPtr(0), pfPtr(0)), "pfPtr runs ahead of bpuPtr")
//  XSError(ifuWbPtr < commitPtr && !isFull(ifuWbPtr(0), commitPtr(0)), "ifuWbPtr runs ahead of commitPtr")

  private val validEntries = distanceBetween(bpuPtr(0), commitPtr(0))

  // entryQueue stores predictions made by BPU.
  private val entryQueue = Reg(Vec(FtqSize, new FtqEntry))

  // speculationQueue stores speculation information needed by BPU when redirect happens.
  private val speculationQueue = Reg(Vec(FtqSize, new BpuSpeculationMeta))

  // metaQueue stores information needed to train BPU.
  private val metaQueue = Module(new MetaQueue)

  // resolveQueue stores branch resolve information from backend.
  private val resolveQueue = Module(new ResolveQueue)

  private val ifuRedirect = receiveIfuRedirect(io.fromIfu.pdWb(0))

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
    when(exception.hasException) {
      backendExceptionPtr := ifuWbPtr(0)
      backendException    := exception
    }
  }.elsewhen(ifuWbPtr(0) =/= backendExceptionPtr) {
    backendException := ExceptionType.None
  }

  // --------------------------------------------------------------------------------
  // Interaction with BPU
  // --------------------------------------------------------------------------------
  io.fromBpu.prediction.ready      := validEntries < FtqSize.U
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
    entryQueue(predictionPtr.value).startVAddr     := prediction.bits.startVAddr
    entryQueue(predictionPtr.value).takenCfiOffset := prediction.bits.takenCfiOffset
    entryQueue(predictionPtr.value).identifiedCfi  := prediction.bits.identifiedCfi
  }

  speculationQueue(io.fromBpu.s3FtqPtr.value) := io.fromBpu.speculationMeta.bits

  metaQueue.io.wen        := io.fromBpu.meta.valid
  metaQueue.io.waddr      := io.fromBpu.s3FtqPtr.value
  metaQueue.io.wdata.meta := io.fromBpu.meta.bits
  if (metaQueue.io.wdata.paddingBit.isDefined) {
    metaQueue.io.wdata.paddingBit.get := 0.U
  }

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
  for (stage <- 2 to 3) {
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
  io.toICache.prefetchReq.bits.req.startVAddr := Mux(
    redirectNext.valid,
    PrunedAddrInit(redirectNext.bits.target),
    entryQueue(pfPtr(0).value).startVAddr
  )
  io.toICache.prefetchReq.bits.req.nextCachelineVAddr :=
    io.toICache.prefetchReq.bits.req.startVAddr + (CacheLineSize / 8).U
  io.toICache.prefetchReq.bits.req.ftqIdx := pfPtr(0)
  io.toICache.prefetchReq.bits.backendException := Mux(
    backendExceptionPtr === pfPtr(0),
    backendException,
    ExceptionType.None
  )

  // TODO: consider BPU bypass
  io.toICache.fetchReq.valid                       := bpuPtr(0) > ifuPtr(0) && !redirect.valid
  io.toICache.fetchReq.bits.req.startVAddr         := entryQueue(ifuPtr(0).value).startVAddr
  io.toICache.fetchReq.bits.req.nextCachelineVAddr := entryQueue(ifuPtr(0).value).startVAddr + (CacheLineSize / 8).U
  io.toICache.fetchReq.bits.req.ftqIdx             := ifuPtr(0)
  io.toICache.fetchReq.bits.isBackendException     := backendException.hasException && backendExceptionPtr === ifuPtr(0)

  io.toIfu.req.valid                    := bpuPtr(0) > ifuPtr(0) && !redirect.valid
  io.toIfu.req.bits.fetch(0).valid      := bpuPtr(0) > ifuPtr(0) && !redirect.valid
  io.toIfu.req.bits.fetch(0).startVAddr := entryQueue(ifuPtr(0).value).startVAddr
  io.toIfu.req.bits.fetch(0).nextStartVAddr := MuxCase(
    entryQueue(ifuPtr(1).value).startVAddr,
    Seq(
      (bpuPtr(0) === ifuPtr(0)) -> prediction.bits.target,
      (bpuPtr(0) === ifuPtr(1)) -> prediction.bits.startVAddr
    )
  )
  io.toIfu.req.bits.fetch(0).nextCachelineVAddr := io.toIfu.req.bits.fetch(0).startVAddr + (CacheLineSize / 8).U
  io.toIfu.req.bits.fetch(0).ftqIdx             := ifuPtr(0)
  io.toIfu.req.bits.fetch(0).takenCfiOffset     := entryQueue(ifuPtr(0).value).takenCfiOffset
  io.toIfu.req.bits.fetch(0).identifiedCfi      := entryQueue(ifuPtr(0).value).identifiedCfi

  io.toIfu.req.bits.fetch(1) := 0.U.asTypeOf(new FetchRequestBundle)
  // --------------------------------------------------------------------------------
  // Interaction with backend
  // --------------------------------------------------------------------------------

  io.toBackend.pc_mem_wen   := (prediction.fire || bpuS3Redirect) && !redirect.valid
  io.toBackend.pc_mem_waddr := predictionPtr.value
  io.toBackend.pc_mem_wdata := prediction.bits.startVAddr

  // TODO: remove or reconsider this
  io.toBackend.newest_entry_en     := false.B
  io.toBackend.newest_entry_ptr    := DontCare
  io.toBackend.newest_entry_target := DontCare

  // --------------------------------------------------------------------------------
  // Redirect from backend and IFU
  // --------------------------------------------------------------------------------

  io.toICache.redirectFlush := redirect.valid
  when(redirect.valid) {
    // TODO: When can redirect information be written to original entry instead of a new entry?
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

  io.toBpu.redirect.valid := redirect.valid
  // FIXME: Modify BPU
  io.toBpu.redirect.bits.startVAddr      := redirect.bits.pc
  io.toBpu.redirect.bits.target          := redirect.bits.target
  io.toBpu.redirect.bits.isRvc           := redirect.bits.isRVC
  io.toBpu.redirect.bits.taken           := redirect.bits.taken
  io.toBpu.redirect.bits.attribute       := DontCare
  io.toBpu.redirect.bits.speculationMeta := speculationQueue(redirect.bits.ftqIdx.value)
  io.toBpu.redirectFromIFU               := ifuRedirect.valid

  // --------------------------------------------------------------------------------
  // Resolve and train BPU
  // --------------------------------------------------------------------------------
  resolveQueue.io.backendResolve := io.fromBackend.resolve

  metaQueue.io.ren   := resolveQueue.io.bpuTrain.valid
  metaQueue.io.raddr := resolveQueue.io.bpuTrain.bits.ftqIdx.value

  io.toBpu.train.valid           := RegNext(resolveQueue.io.bpuTrain.valid)
  io.toBpu.train.bits.meta       := metaQueue.io.rdata.meta
  io.toBpu.train.bits.startVAddr := RegEnable(resolveQueue.io.bpuTrain.bits.startVAddr, resolveQueue.io.bpuTrain.valid)
  io.toBpu.train.bits.branches   := RegEnable(resolveQueue.io.bpuTrain.bits.branches, resolveQueue.io.bpuTrain.valid)

  // --------------------------------------------------------------------------------
  // Commit and train BPU
  // --------------------------------------------------------------------------------
  // TODO: frontend does not need this many rob commit channels
  // Backend may send commit for on entry multiple times, but the entry is actually committed when it is committed for
  // the first time. The rest of the commits can be ignored.
  // TODO: Backend now still does not guarantee to send commit for every entry. For example, entry 0 has a flush-itself
  // redirect in the middle, and entry 1 has a flush-itself redirect on the same instruction in the beginning, entry 2
  // begins with the same instruction. Backend may not commit entry 0. This may not be totally backend's problem. Maybe
  // it is frontend's responsibility to fix it.
  private val robCommitPtr: FtqPtr = WireInit(FtqPtr(false.B, 0.U))
  private val backendCommit = io.fromBackend.rob_commits.map(_.valid).reduce(_ | _)
  when(backendCommit) {
    robCommitPtr := ParallelPriorityMux(
      io.fromBackend.rob_commits.map(_.valid).reverse,
      io.fromBackend.rob_commits.map(_.bits.ftqIdx).reverse
    )
  }
  private val robCommitPtrReg: FtqPtr = RegEnable(robCommitPtr, FtqPtr(true.B, (FtqSize - 1).U), backendCommit)
  private val committedPtr = Mux(backendCommit, robCommitPtr, robCommitPtrReg)
  private val commit       = commitPtr < committedPtr
  when(commit) {
    commitPtr := commitPtr + 1.U
  }

  // --------------------------------------------------------------------------------
  // MMIO fetch
  // --------------------------------------------------------------------------------
  private val mmioPtr           = io.fromIfu.mmioCommitRead.mmioFtqPtr
  private val lastMmioCommitted = commitPtr > mmioPtr || commitPtr === mmioPtr && commit
  io.fromIfu.mmioCommitRead.mmioLastCommit := RegNext(lastMmioCommitted)

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
