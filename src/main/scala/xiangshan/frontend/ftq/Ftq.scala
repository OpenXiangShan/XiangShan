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
import xiangshan.backend.CtrlToFtqIO
import xiangshan.frontend.BpuToFtqIO
import xiangshan.frontend.BranchPredictionRedirect
import xiangshan.frontend.ExceptionType
import xiangshan.frontend.FtqToICacheIO
import xiangshan.frontend.FtqToIfuIO
import xiangshan.frontend.IfuToFtqIO
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.PrunedAddrInit
import xiangshan.frontend.bpu.HasBPUConst

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

  // cfiQueue stores positions of control flow instructions in each request entry.
  // FIXME: Do we need cfiQueue when backend sends instruction information with commit?
  private val cfiQueue = Module(new CfiQueue)

  // metaQueue stores information needed to train BPU.
  private val metaQueue = Module(new MetaQueue)

  private val readyToCommit = Wire(Bool())
  private val canCommit     = Wire(Bool())
  private val shouldCommit  = RegInit(VecInit(Seq.fill(FtqSize)(false.B)))

  private val ifuRedirect = WireInit(0.U.asTypeOf(Valid(new BranchPredictionRedirect)))

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
    backendException := ExceptionType(
      hasPf = backendRedirect.bits.cfiUpdate.backendIPF,
      hasGpf = backendRedirect.bits.cfiUpdate.backendIGPF,
      hasAf = backendRedirect.bits.cfiUpdate.backendIAF
    )
    when(backendException.hasException) {
      backendExceptionPtr := ifuWbPtr(0)
    }
  }.elsewhen(ifuWbPtr(0) =/= backendExceptionPtr) {
    backendException := ExceptionType.None
  }

  // --------------------------------------------------------------------------------
  // Interaction with BPU
  // --------------------------------------------------------------------------------
  // TODO: resp is a bad name
  io.fromBpu.resp.ready := validEntries < FtqSize.U
  io.fromBpu.meta.ready := validEntries < FtqSize.U

  private val fromBpu = io.fromBpu.resp

  private val bpuS2Redirect = fromBpu.bits.s2Override.valid && fromBpu.valid
  private val bpuS3Redirect = fromBpu.bits.s3Override.valid && fromBpu.valid

  io.toBpu.bpuPtr := bpuPtr(0)
  private val bpuEnqueue = io.fromBpu.resp.fire && !redirect.valid

  private val fromBpuPtr = MuxCase(
    bpuPtr(0),
    Seq(
      fromBpu.bits.s3Override.valid -> fromBpu.bits.s3Override.bits.ftqPtr,
      fromBpu.bits.s2Override.valid -> fromBpu.bits.s2Override.bits.ftqPtr
    )
  )

  when(fromBpu.bits.s3Override.valid) {
    bpuPtr := fromBpu.bits.s3Override.bits.ftqPtr + 1.U
  }.elsewhen(fromBpu.bits.s2Override.valid) {
    bpuPtr := fromBpu.bits.s2Override.bits.ftqPtr + 1.U
  }.elsewhen(bpuEnqueue) {
    bpuPtr := bpuPtr + 1.U
  }

  entryQueue.io.wen   := (fromBpu.fire || bpuS2Redirect || bpuS3Redirect) && !redirect.valid
  entryQueue.io.waddr := fromBpuPtr.value
  entryQueue.io.wdata := fromBpu.bits.startVAddr

  cfiQueue.io.wen   := (fromBpu.fire || bpuS2Redirect || bpuS3Redirect) && !redirect.valid
  cfiQueue.io.waddr := fromBpuPtr.value
  cfiQueue.io.wdata := fromBpu.bits.cfiPosition

  metaQueue.io.wen             := io.fromBpu.meta.valid
  metaQueue.io.waddr           := io.fromBpu.resp.bits.s3Override.bits.ftqPtr.value
  metaQueue.io.wdata.meta      := DontCare
  metaQueue.io.wdata.newMeta   := io.fromBpu.meta.bits
  metaQueue.io.wdata.ftb_entry := DontCare
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

  for (stage <- 2 to 3) {
    val redirect = fromBpu.bits.overrideStage(stage).valid
    val ftqIdx   = fromBpu.bits.overrideStage(stage).bits.ftqPtr

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

  entryQueue.io.pfPtr.zipWithIndex.foreach { case (ptr, offset) => ptr.ptr := pfPtr(offset) }
  entryQueue.io.ifuPtr.zipWithIndex.foreach { case (ptr, offset) => ptr.ptr := ifuPtr(offset) }

  cfiQueue.io.ifuPtr.zipWithIndex.foreach { case (ptr, offset) => ptr.ptr := ifuPtr(offset) }

  // FIXME:backend redirect delay should be more than ITLB csr delay
  io.toICache.prefetchReq.valid := (bpuPtr(0) > pfPtr(0) || redirectNext.valid) && !redirect.valid
  io.toICache.prefetchReq.bits.req.startVAddr := Mux(
    redirectNext.valid,
    PrunedAddrInit(redirectNext.bits.cfiUpdate.target),
    entryQueue.io.pfPtr(0).rdata
  )
  io.toICache.prefetchReq.bits.req.nextCachelineVAddr :=
    io.toICache.prefetchReq.bits.req.startVAddr + (CacheLineSize / 8).U
  io.toICache.prefetchReq.bits.req.ftqIdx := pfPtr(0)
  io.toICache.prefetchReq.bits.backendException := Mux(
    backendExceptionPtr === pfPtr(0),
    backendException,
    ExceptionType.None
  )

  // TODO: we should not need both fetchReq.valid and fetchReq.bits.readValid
  // TODO: consider BPU bypass
  io.toICache.fetchReq.valid := bpuPtr(0) > ifuPtr(0) && !redirect.valid
  io.toICache.fetchReq.bits.readValid.foreach(valid =>
    valid := io.toICache.fetchReq.valid
  )
  io.toICache.fetchReq.bits.req.foreach { req =>
    req.startVAddr         := entryQueue.io.ifuPtr(0).rdata
    req.nextCachelineVAddr := req.startVAddr + (CacheLineSize / 8).U
    req.ftqIdx             := ifuPtr(0)
  }
  io.toICache.fetchReq.bits.isBackendException := backendException.hasException && backendExceptionPtr === ifuPtr(0)

  io.toIfu.req.valid           := bpuPtr(0) > ifuPtr(0) && !redirect.valid
  io.toIfu.req.bits.startVAddr := entryQueue.io.ifuPtr(0).rdata
  io.toIfu.req.bits.nextStartVAddr := Mux(
    bpuPtr(0) === ifuPtr(1),
    fromBpu.bits.target,
    entryQueue.io.ifuPtr(1).rdata
  )
  io.toIfu.req.bits.nextCachelineVAddr := io.toIfu.req.bits.startVAddr + (CacheLineSize / 8).U
  io.toIfu.req.bits.ftqIdx             := ifuPtr(0)
  io.toIfu.req.bits.ftqOffset          := cfiQueue.io.ifuPtr(0).rdata

  // Interaction with backend  // --------------------------------------------------------------------------------

  // --------------------------------------------------------------------------------

  io.toBackend.pc_mem_wen   := (fromBpu.fire || bpuS2Redirect || bpuS3Redirect) && !redirect.valid
  io.toBackend.pc_mem_waddr := RegNext(fromBpuPtr.value)
  io.toBackend.pc_mem_wdata := RegNext(fromBpu.bits.startVAddr)

  // TODO: remove or reconsider this
  io.toBackend.newest_entry_en     := false.B
  io.toBackend.newest_entry_ptr    := DontCare
  io.toBackend.newest_entry_target := DontCare

  // --------------------------------------------------------------------------------
  // Redirect from backend and IFU
  // --------------------------------------------------------------------------------

  private def updateCfi(redirect: Valid[Redirect]): Unit = {
    val valid     = redirect.valid
    val ftqPtr    = redirect.bits.ftqIdx
    val ftqOffset = redirect.bits.ftqOffset
    val taken     = redirect.bits.cfiUpdate.taken
  }

  when(backendRedirect.valid) {
    updateCfi(backendRedirect)
    val redirect = backendRedirect.bits
  }.elsewhen(ifuRedirect.valid) {
    updateCfi(ifuRedirect)
  }

  io.toICache.redirectFlush := redirect.valid
  when(redirect.valid) {
    val newEntryPtr = redirect.bits.ftqIdx + 1.U
    Seq(bpuPtr, ifuPtr, pfPtr).foreach(_ := newEntryPtr)
    when(RedirectLevel.flushItself(redirect.bits.level)) {
      shouldCommit(redirect.bits.ftqIdx.value) := false.B
      shouldCommit(newEntryPtr.value)          := false.B
    }
  }

  io.toIfu.redirect.valid := backendRedirect.valid
  // TODO: only valid should be needed
  io.toIfu.redirect.bits := DontCare

  io.toBpu.redirect.valid := redirect.valid
  // FIXME: Modify BPU
  io.toBpu.redirect.bits   := DontCare
  io.toBpu.redirectFromIFU := ifuRedirect.valid

  // --------------------------------------------------------------------------------
  // Commit and train BPU
  // --------------------------------------------------------------------------------
  // TODO: frontend does not need this many rob commit channels
  // Backend may send commit for on entry multiple times, but when the entry is committed for the first time,
  // it is actually committed, the rest of the commits can be ignored.
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
  canCommit     := commitPtr < committedPtr
  readyToCommit := canCommit && shouldCommit(commitPtr(0).value)
  when(canCommit) {
    commitPtr := commitPtr + 1.U
  }

  entryQueue.io.commitPtr.zipWithIndex.foreach { case (ptr, offset) => ptr.ptr := commitPtr(offset) }
  cfiQueue.io.commitPtr.zipWithIndex.foreach { case (ptr, offset) => ptr.ptr := commitPtr(offset) }

  // TODO: Wrap metaQueue as other queues
  metaQueue.io.ren   := readyToCommit
  metaQueue.io.raddr := commitPtr(0).value

  io.toBpu.update    := DontCare
  io.toBpu.newUpdate := DontCare

  // --------------------------------------------------------------------------------
  // MMIO fetch
  // --------------------------------------------------------------------------------
  private val mmioPtr           = io.fromIfu.mmioCommitRead.mmioFtqPtr
  private val lastMmioCommitted = commitPtr > mmioPtr || commitPtr === mmioPtr && canCommit
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
