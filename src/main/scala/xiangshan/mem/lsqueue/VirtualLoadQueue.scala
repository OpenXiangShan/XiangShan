/***************************************************************************************
* Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
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

import org.chipsalliance.cde.config._
import chisel3._
import chisel3.util._
import utils._
import utility._
import xiangshan._
import xiangshan.ExceptionNO._
import xiangshan.backend.rob.{RobLsqIO, RobPtr}
import xiangshan.backend.Bundles.{DynInst, MemExuOutput, UopIdx}
import xiangshan.backend.fu.FuConfig.LduCfg
import xiangshan.backend.decode.isa.bitfield.{InstVType, XSInstBitFields}
import xiangshan.backend.fu.FuType
import xiangshan.mem.Bundles._
import xiangshan.cache._

class VirtualLoadQueue(implicit p: Parameters) extends XSModule
  with HasDCacheParameters
  with HasCircularQueuePtrHelper
  with HasLoadHelper
  with HasPerfEvents
  with HasVLSUParameters {
  val io = IO(new Bundle() {
    // control
    val redirect    = Flipped(Valid(new Redirect))
    val vecCommit   = Vec(VecLoadPipelineWidth, Flipped(ValidIO(new FeedbackToLsqIO)))
    // from dispatch
    val enq         = new LqEnqIO
    // from ldu s3
    val ldin        = Vec(LoadPipelineWidth, Flipped(DecoupledIO(new LqWriteBundle)))
    // to LoadQueueReplay and LoadQueueRAR
    val ldWbPtr     = Output(new LqPtr)
    // global
    val lqFull      = Output(Bool())
    val lqEmpty     = Output(Bool())
    // to dispatch
    val lqDeq       = Output(UInt(log2Up(CommitWidth + 1).W))
    val lqCancelCnt = Output(UInt(log2Up(VirtualLoadQueueSize+1).W))
    // for topdown
    val noUopsIssued = Input(Bool())
  })

  println("VirtualLoadQueue: size: " + VirtualLoadQueueSize)
  //  VirtualLoadQueue field
  //  +-----------+---------+-------+
  //  | Allocated | MicroOp | Flags |
  //  +-----------+---------+-------+
  //  Allocated   : entry has been allocated already
  //  MicroOp     : inst's microOp
  //  Flags       : load flags
  val allocated = RegInit(VecInit(List.fill(VirtualLoadQueueSize)(false.B))) // The control signals need to explicitly indicate the initial value
  val robIdx = Reg(Vec(VirtualLoadQueueSize, new RobPtr))
  val uopIdx = Reg(Vec(VirtualLoadQueueSize, UopIdx()))
  val isvec = RegInit(VecInit(List.fill(VirtualLoadQueueSize)(false.B))) // vector load flow
  val committed = Reg(Vec(VirtualLoadQueueSize, Bool()))

  /**
   * used for debug
   */
  val debug_mmio = Reg(Vec(VirtualLoadQueueSize, Bool())) // mmio: inst is an mmio inst
  val debug_paddr = Reg(Vec(VirtualLoadQueueSize, UInt(PAddrBits.W))) // mmio: inst's paddr

  //  maintain pointers
  val enqPtrExt = RegInit(VecInit((0 until io.enq.req.length).map(_.U.asTypeOf(new LqPtr))))
  val enqPtr = enqPtrExt(0).value
  val deqPtr = Wire(new LqPtr)
  val deqPtrNext = Wire(new LqPtr)

  /**
   * update pointer
   */
  val lastCycleRedirect = RegNext(io.redirect)
  val lastLastCycleRedirect = RegNext(lastCycleRedirect)

  val validCount = distanceBetween(enqPtrExt(0), deqPtr)
  val allowEnqueue = validCount <= (VirtualLoadQueueSize - LSQLdEnqWidth).U
  val canEnqueue = io.enq.req.map(_.valid)
  val vLoadFlow = io.enq.req.map(_.bits.numLsElem.asTypeOf(UInt(elemIdxBits.W)))
  val needCancel = WireInit(VecInit((0 until VirtualLoadQueueSize).map(i => {
    robIdx(i).needFlush(io.redirect) && allocated(i)
  })))
  val lastNeedCancel = GatedValidRegNext(needCancel)
  val enqCancel = canEnqueue.zip(io.enq.req).map{case (v , x) =>
    v && x.bits.robIdx.needFlush(io.redirect)
  }
  val enqCancelNum = enqCancel.zip(vLoadFlow).map{case (v, flow) =>
    Mux(v, flow, 0.U)
  }
  val lastEnqCancel = GatedRegNext(enqCancelNum.reduce(_ + _))
  val lastCycleCancelCount = PopCount(lastNeedCancel)
  val redirectCancelCount = RegEnable(lastCycleCancelCount + lastEnqCancel, 0.U, lastCycleRedirect.valid)

  // update enqueue pointer
  val validVLoadFlow = vLoadFlow.zipWithIndex.map{case (vLoadFlowNumItem, index) => Mux(canEnqueue(index), vLoadFlowNumItem, 0.U)}
  val validVLoadOffset = vLoadFlow.zip(io.enq.needAlloc).map{case (flow, needAllocItem) => Mux(needAllocItem, flow, 0.U)}
  val validVLoadOffsetRShift = 0.U +: validVLoadOffset.take(validVLoadFlow.length - 1)

  val enqNumber = validVLoadFlow.reduce(_ + _)
  val enqPtrExtNextVec = Wire(Vec(io.enq.req.length, new LqPtr))
  val enqPtrExtNext = Wire(Vec(io.enq.req.length, new LqPtr))
  when (lastLastCycleRedirect.valid) {
    // we recover the pointers in the next cycle after redirect
    enqPtrExtNextVec := VecInit(enqPtrExt.map(_ - redirectCancelCount))
  } .otherwise {
    enqPtrExtNextVec := VecInit(enqPtrExt.map(_ + enqNumber))
  }
  assert(!(lastCycleRedirect.valid && enqNumber =/= 0.U))

  when (isAfter(enqPtrExtNextVec(0), deqPtrNext)) {
    enqPtrExtNext := enqPtrExtNextVec
  } .otherwise {
    enqPtrExtNext := VecInit((0 until io.enq.req.length).map(i => deqPtrNext + i.U))
  }
  enqPtrExt := enqPtrExtNext

  // update dequeue pointer
  val DeqPtrMoveStride = CommitWidth
  require(DeqPtrMoveStride == CommitWidth, "DeqPtrMoveStride must be equal to CommitWidth!")
  val deqLookupVec = VecInit((0 until DeqPtrMoveStride).map(deqPtr + _.U))
  val deqLookup = VecInit(deqLookupVec.map(ptr => allocated(ptr.value) && committed(ptr.value) && ptr =/= enqPtrExt(0)))
  val deqInSameRedirectCycle = VecInit(deqLookupVec.map(ptr => needCancel(ptr.value)))
  // make chisel happy
  val deqCountMask = Wire(UInt(DeqPtrMoveStride.W))
  deqCountMask := deqLookup.asUInt & (~deqInSameRedirectCycle.asUInt).asUInt
  val commitCount = PopCount(PriorityEncoderOH(~deqCountMask) - 1.U)
  val lastCommitCount = GatedRegNext(commitCount)

  // update deqPtr
  // cycle 1: generate deqPtrNext
  // cycle 2: update deqPtr
  val deqPtrUpdateEna = lastCommitCount =/= 0.U
  deqPtrNext := deqPtr + lastCommitCount
  deqPtr := RegEnable(deqPtrNext, 0.U.asTypeOf(new LqPtr), deqPtrUpdateEna)

  io.lqDeq := GatedRegNext(lastCommitCount)
  io.lqCancelCnt := redirectCancelCount
  io.ldWbPtr := deqPtr
  io.lqEmpty := RegNext(validCount === 0.U)

  /**
   * Enqueue at dispatch
   *
   * Currently, VirtualLoadQueue only allows enqueue when #emptyEntries > EnqWidth
   * Dynamic enq based on numLsElem number
   */
  io.enq.canAccept := allowEnqueue
  val enqLowBound = io.enq.req.map(_.bits.lqIdx)
  val enqUpBound  = io.enq.req.map(x => x.bits.lqIdx + x.bits.numLsElem)
  val enqCrossLoop = enqLowBound.zip(enqUpBound).map{case (low, up) => low.flag =/= up.flag}

  for(i <- 0 until VirtualLoadQueueSize) {
    val entryCanEnqSeq = (0 until io.enq.req.length).map { j =>
      val entryHitBound = Mux(
        enqCrossLoop(j),
        enqLowBound(j).value <= i.U || i.U < enqUpBound(j).value,
        enqLowBound(j).value <= i.U && i.U < enqUpBound(j).value
      )
      canEnqueue(j) && !enqCancel(j) && entryHitBound
    }
    val entryCanEnq = entryCanEnqSeq.reduce(_ || _)
    val selectBits = ParallelPriorityMux(entryCanEnqSeq, io.enq.req.map(_.bits))
    when (entryCanEnq) {
      allocated(i) := true.B
      robIdx(i) := selectBits.robIdx
      uopIdx(i) := selectBits.uopIdx
      isvec(i) :=  FuType.isVLoad(selectBits.fuType)
      committed(i) := false.B

      debug_mmio(i) := false.B
      debug_paddr(i) := 0.U
    }
  }

  for (i <- 0 until io.enq.req.length) {
    val lqIdx = enqPtrExt(0) + validVLoadOffsetRShift.take(i + 1).reduce(_ + _)
    val index = io.enq.req(i).bits.lqIdx
    XSError(canEnqueue(i) && !enqCancel(i) && (!io.enq.canAccept || !io.enq.sqCanAccept), s"must accept $i\n")
    XSError(canEnqueue(i) && !enqCancel(i) && index.value =/= lqIdx.value, s"must be the same entry $i\n")
    io.enq.resp(i) := lqIdx
  }

  /**
    * Load commits
    *
    * When load commited, mark it as !allocated and move deqPtr forward.
    */
  (0 until DeqPtrMoveStride).map(i => {
    when (commitCount > i.U) {
      allocated((deqPtr+i.U).value) := false.B
    }
    XSError(commitCount > i.U && !allocated((deqPtr+i.U).value), s"why commit invalid entry $i?\n")
  })

  // vector commit or replay
  val vecLdCommittmp = Wire(Vec(VirtualLoadQueueSize, Vec(VecLoadPipelineWidth, Bool())))
  val vecLdCommit = Wire(Vec(VirtualLoadQueueSize, Bool()))
  for (i <- 0 until VirtualLoadQueueSize) {
    val cmt = io.vecCommit
    for (j <- 0 until VecLoadPipelineWidth) {
      vecLdCommittmp(i)(j) := allocated(i) && cmt(j).valid && robIdx(i) === cmt(j).bits.robidx && uopIdx(i) === cmt(j).bits.uopidx
    }
    vecLdCommit(i) := vecLdCommittmp(i).reduce(_ || _)

    when (vecLdCommit(i) && isvec(i)) {
      committed(i) := true.B
    }
  }

  // misprediction recovery / exception redirect
  // invalidate lq term using robIdx
  for (i <- 0 until VirtualLoadQueueSize) {
    when (needCancel(i)) {
      allocated(i) := false.B
    }
  }

  XSDebug(p"(ready, valid): ${io.enq.canAccept}, ${Binary(Cat(io.enq.req.map(_.valid)))}\n")

  /**
    * Writeback load from load units
    *
    * Most load instructions writeback to regfile at the same time.
    * However,
    *   (1) For ready load instruction (no need replay), it writes back to ROB immediately.
    */
  for(i <- 0 until LoadPipelineWidth) {
    //   most lq status need to be updated immediately after load writeback to lq
    //   flag bits in lq needs to be updated accurately
    io.ldin(i).ready := true.B
    val loadWbIndex = io.ldin(i).bits.uop.lqIdx.value

    val need_rep = io.ldin(i).bits.rep_info.need_rep
    val need_valid = io.ldin(i).bits.updateAddrValid
    when (io.ldin(i).valid) {
      val hasExceptions = ExceptionNO.selectByFu(io.ldin(i).bits.uop.exceptionVec, LduCfg).asUInt.orR
      when (!need_rep && need_valid && !io.ldin(i).bits.isvec) {
        committed(loadWbIndex) := true.B
        //  Debug info
        debug_mmio(loadWbIndex) := io.ldin(i).bits.mmio
        debug_paddr(loadWbIndex) := io.ldin(i).bits.paddr
      }
    }
    XSInfo(io.ldin(i).valid && !need_rep && need_valid,
      "load hit write to lq idx %d pc 0x%x vaddr %x paddr %x mask %x forwardData %x forwardMask: %x mmio %x isvec %x\n",
      io.ldin(i).bits.uop.lqIdx.asUInt,
      io.ldin(i).bits.uop.pc,
      io.ldin(i).bits.vaddr,
      io.ldin(i).bits.paddr,
      io.ldin(i).bits.mask,
      io.ldin(i).bits.forwardData.asUInt,
      io.ldin(i).bits.forwardMask.asUInt,
      io.ldin(i).bits.mmio,
      io.ldin(i).bits.isvec
    )
  }

  //  perf counter
  QueuePerf(VirtualLoadQueueSize, validCount, !allowEnqueue)
  val vecValidVec = WireInit(VecInit((0 until VirtualLoadQueueSize).map(i => allocated(i) && isvec(i))))
  QueuePerf(VirtualLoadQueueSize, PopCount(vecValidVec), !allowEnqueue)
  io.lqFull := !allowEnqueue

  def NLoadNotCompleted = 1
  val validCountReg = RegNext(validCount)
  val noUopsIssued = io.noUopsIssued
  val stallLoad = io.noUopsIssued && (validCountReg >= NLoadNotCompleted.U)
  val memStallAnyLoad = RegNext(stallLoad)

  XSPerfAccumulate("mem_stall_anyload", memStallAnyLoad)

  val perfEvents: Seq[(String, UInt)] = Seq(
    ("MEMSTALL_ANY_LOAD", memStallAnyLoad),
  )
  generatePerfEvent()

  // debug info
  XSDebug("enqPtrExt %d:%d deqPtrExt %d:%d\n", enqPtrExt(0).flag, enqPtr, deqPtr.flag, deqPtr.value)

  def PrintFlag(flag: Bool, name: String): Unit = {
    XSDebug(false, flag, name) // when(flag)
    XSDebug(false, !flag, " ") // otherwise
  }

  for (i <- 0 until VirtualLoadQueueSize) {
    PrintFlag(allocated(i), "a")
    PrintFlag(allocated(i) && committed(i), "c")
    PrintFlag(allocated(i) && isvec(i), "v")
    XSDebug(false, true.B, "\n")
  }
  // end
}
