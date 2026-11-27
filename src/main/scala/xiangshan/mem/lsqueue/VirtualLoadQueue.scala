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
import xiangshan.backend.Bundles.{DynInst, UopIdx}
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
    // from dispatch
    val enq         = new LqEnqIO
    // from ldu s3
    val ldin        = Vec(LoadPipelineWidth, Flipped(DecoupledIO(new LqWriteBundle)))
    // to LoadQueueReplay and LoadQueueRAR
    val ldWbPtr     = Output(new LqPtr)
    val ldWbPtrByThread = OptionWrapper(EnableSMT2, Vec(SMT2ThreadCount, Output(new LqPtr)))
    // global
    val lqFull      = Output(Bool())
    val lqEmpty     = Output(Bool())
    // to dispatch
    val lqDeq       = ValidIO(UInt(log2Up(CommitWidth + 1).W))
    val lqDeqByThread = OptionWrapper(EnableSMT2,
      Vec(SMT2ThreadCount, ValidIO(UInt(log2Up(CommitWidth + 1).W))))
    // tolsqEnqCtrl
    val lqRedirect  = ValidIO(new LqPtr)
    val lqRedirectByThread = OptionWrapper(EnableSMT2, Vec(SMT2ThreadCount, ValidIO(new LqPtr)))
    val lqRecoverStall = Output(Bool())
    val lqRecoverStallByThread = OptionWrapper(EnableSMT2, Vec(SMT2ThreadCount, Output(Bool())))
    val lqEmptyByThread = OptionWrapper(EnableSMT2, Vec(SMT2ThreadCount, Output(Bool())))
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
  val allowEnqueue = enqPtrExt.head >= deqPtr
  val canEnqueue = io.enq.req.map(_.valid)
  val vLoadFlow = io.enq.req.map(_.bits.numLsElem.asTypeOf(UInt(elemIdxBits.W)))
  val requestTid = io.enq.req.zip(io.enq.tid).map { case (req, tid) =>
    Mux(EnableSMT2.B && req.valid, tid, 0.U)
  }
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

  io.lqDeq.bits := GatedRegNext(lastCommitCount)
  io.lqDeq.valid := RegNext(lastCommitCount.orR)
  io.lqRedirect.valid := RegNext(lastLastCycleRedirect.valid)
  io.lqRedirect.bits := RegEnable(enqPtrExtNext.head, lastLastCycleRedirect.valid)
  io.ldWbPtr := deqPtr
  io.lqEmpty := RegNext(validCount === 0.U)
  val lqRecoverStall = io.redirect.valid || lastCycleRedirect.valid || lastLastCycleRedirect.valid
  io.lqRecoverStall := RegNext(lqRecoverStall) // means loadqueue haven't pending redirect

  val allowEnqueueByThread = if (EnableSMT2) Some(Wire(Vec(SMT2ThreadCount, Bool()))) else None
  val smt2EnqResp = if (EnableSMT2) Some(Wire(Vec(LSQEnqWidth, new LqPtr))) else None

  if (EnableSMT2) {
    // Independent control state for the two 60-entry LoadQueue views. The existing
    // single-thread control above remains unchanged.
    val enqPtrT0 = RegInit(0.U.asTypeOf(new LqLocalPtr))
    val enqPtrT1 = RegInit(0.U.asTypeOf(new LqLocalPtr))
    val deqPtrT0 = Wire(new LqLocalPtr)
    val deqPtrT1 = Wire(new LqLocalPtr)
    val deqPtrT0Next = Wire(new LqLocalPtr)
    val deqPtrT1Next = Wire(new LqLocalPtr)

    val flowByThread = (0 until SMT2ThreadCount).map { tid =>
      vLoadFlow.zip(requestTid).map { case (flow, reqTid) => Mux(reqTid === tid.U, flow, 0.U) }
    }
    val enqNumberByThread = flowByThread.map(_.reduce(_ +& _))
    for (i <- 0 until LSQEnqWidth) {
      val tid = requestTid(i)
      val tidIdx = if (i >= LSQEnqWidth / 2) 1 else 0
      val prefix = flowByThread(tidIdx).take(i + 1).reduce(_ +& _)
      val localPtr = if (tidIdx == 0) enqPtrT0.addWrapCircles(prefix) else enqPtrT1.addWrapCircles(prefix)
      smt2EnqResp.get(i) := LqGlobalPtr.fromLocal(tid, localPtr)
    }

    val cancelByThread = (0 until SMT2ThreadCount).map { tid =>
      val bank = tid
      VecInit((0 until VirtualLoadQueueBankSize).map { index =>
        lastNeedCancel(bank * VirtualLoadQueueBankSize + index)
      })
    }
    val enqCancelByThread = (0 until SMT2ThreadCount).map { tid =>
      RegNext(enqCancelNum.zip(requestTid).map { case (num, reqTid) => Mux(reqTid === tid.U, num, 0.U) }.reduce(_ +& _))
    }
    val redirectCancelCountByThread = (0 until SMT2ThreadCount).map { tid =>
      val existing = PopCount(cancelByThread(tid))
      RegEnable(existing + enqCancelByThread(tid), 0.U, lastCycleRedirect.valid)
    }

    when(lastLastCycleRedirect.valid) {
      enqPtrT0 := enqPtrT0 - redirectCancelCountByThread(0)
      enqPtrT1 := enqPtrT1 - redirectCancelCountByThread(1)
    }.otherwise {
      enqPtrT0 := enqPtrT0 + enqNumberByThread(0)
      enqPtrT1 := enqPtrT1 + enqNumberByThread(1)
    }

    def countCommittedThread(bank: Int, base: LqLocalPtr, enq: LqLocalPtr): UInt = {
      val lookup = VecInit((0 until DeqPtrMoveStride).map(i => base.addWrapCircles(i.U)))
      val entryValid = VecInit(lookup.map { ptr =>
        allocated(bank)(ptr.value) && committed(bank)(ptr.value) && ptr =/= enq
      })
      val redirectCancel = VecInit(lookup.map(ptr => needCancel((bank * VirtualLoadQueueBankSize).U + ptr.value)))
      val countMask = Wire(UInt(DeqPtrMoveStride.W))
      countMask := entryValid.asUInt & (~redirectCancel.asUInt).asUInt
      PopCount(PriorityEncoderOH(~countMask) - 1.U)
    }

    val commitCountByThread = VecInit((0 until SMT2ThreadCount).map { tid =>
      countCommittedThread(tid, Mux(tid.U === 0.U, deqPtrT0, deqPtrT1), Mux(tid.U === 0.U, enqPtrT0, enqPtrT1))
    })
    val lastCommitCountByThread = VecInit(commitCountByThread.map(GatedRegNext(_)))

    deqPtrT0Next := deqPtrT0 + lastCommitCountByThread(0)
    deqPtrT1Next := deqPtrT1 + lastCommitCountByThread(1)
    deqPtrT0 := RegEnable(deqPtrT0Next, 0.U.asTypeOf(new LqLocalPtr), lastCommitCountByThread(0).orR)
    deqPtrT1 := RegEnable(deqPtrT1Next, 0.U.asTypeOf(new LqLocalPtr), lastCommitCountByThread(1).orR)

    val validCountByThread = VecInit((0 until SMT2ThreadCount).map { tid =>
      distanceBetween(Mux(tid.U === 0.U, enqPtrT0, enqPtrT1), Mux(tid.U === 0.U, deqPtrT0, deqPtrT1))
    })
    allowEnqueueByThread.get.zipWithIndex.foreach { case (allow, tid) =>
      val enqPtr = if (tid == 0) enqPtrT0 else enqPtrT1
      val deqPtr = if (tid == 0) deqPtrT0 else deqPtrT1
      allow := enqPtr >= deqPtr && validCountByThread(tid) +& enqNumberByThread(tid) <= VirtualLoadQueueBankSize.U
    }
    io.enq.canAcceptByThread.get := allowEnqueueByThread.get
    io.lqEmptyByThread.get.zip(validCountByThread).foreach { case (empty, count) => empty := RegNext(count === 0.U) }
    io.lqDeqByThread.get.zip(lastCommitCountByThread).foreach { case (deq, count) =>
      deq.bits := GatedRegNext(count)
      deq.valid := RegNext(count.orR)
    }

    val redirectThread = Mux(io.redirect.bits.robIdx.value >= (RobSize / 2).U, 1.U, 0.U)
    val redirectPtrByThread = (0 until SMT2ThreadCount).map { tid =>
      RegEnable(LqGlobalPtr.fromLocal(tid.U, Mux(tid.U === 0.U, enqPtrT0, enqPtrT1)),
        lastLastCycleRedirect.valid && redirectThread === tid.U)
    }
    io.lqRedirectByThread.get.zip(redirectPtrByThread).zipWithIndex.foreach { case ((redirect, ptr), tid) =>
      redirect.valid := RegNext(lastLastCycleRedirect.valid && redirectThread === tid.U)
      redirect.bits := ptr
    }

    val currentRecoverStall = (0 until SMT2ThreadCount).map { tid =>
      (io.redirect.valid && redirectThread === tid.U) ||
      (lastCycleRedirect.valid && redirectThread === tid.U) ||
      (lastLastCycleRedirect.valid && redirectThread === tid.U)
    }
    io.lqRecoverStallByThread.get.zip(currentRecoverStall).foreach { case (stall, value) => stall := RegNext(value) }
    io.ldWbPtrByThread.get.zip((0 until SMT2ThreadCount).map { tid =>
      LqGlobalPtr.fromLocal(tid.U, Mux(tid.U === 0.U, deqPtrT0, deqPtrT1))
    }).foreach { case (ptr, value) => ptr := value }
  }

  XSError((enqPtrExt.head < deqPtr) &&
  !(enqPtrExt.head.value === deqPtr.value && enqPtrExt.head.flag ^ deqPtr.flag), s"deqPtr exceed enqptr\n")

  /**
   * Enqueue at dispatch
   *
   * Currently, VirtualLoadQueue only allows enqueue when #emptyEntries > EnqWidth
   * Dynamic enq based on numLsElem number
   */
  io.enq.canAccept := (if (EnableSMT2) {
    allowEnqueueByThread.get.reduce(_ && _)
  } else {
    allowEnqueue
  })
  val enqLowBound = io.enq.req.map(_.bits.lqIdx)
  val enqUpBound  = io.enq.req.map(x => x.bits.lqIdx + x.bits.numLsElem)
  val enqCrossLoop = enqLowBound.zip(enqUpBound).map{case (low, up) => low.flag =/= up.flag}
  val smt2LowBound = if (EnableSMT2) {
    Some(io.enq.req.zip(requestTid).map { case (req, tid) =>
      Mux(tid === 0.U, req.bits.lqIdx.value, req.bits.lqIdx.value - VirtualLoadQueueBankSize.U)
    })
  } else {
    None
  }
  val smt2UpBound = if (EnableSMT2) {
    Some(io.enq.req.zip(smt2LowBound.get).map { case (req, low) => low +& req.bits.numLsElem })
  } else {
    None
  }
  val smt2CrossLoop = if (EnableSMT2) {
    Some(smt2UpBound.get.map(_ >= VirtualLoadQueueBankSize.U))
  } else {
    None
  }

  for(i <- 0 until VirtualLoadQueueSize) {
    val entryCanEnqSeq = (0 until io.enq.req.length).map { j =>
      if (EnableSMT2) {
        val bankMatches = requestTid(j) === (i / VirtualLoadQueueBankSize).U
        val localIndex = (i % VirtualLoadQueueBankSize).U
        val localStart = smt2LowBound.get(j)
        val localEnd = Mux(smt2CrossLoop.get(j),
          smt2UpBound.get(j) - VirtualLoadQueueBankSize.U,
          smt2UpBound.get(j))
        val localHit = Mux(smt2CrossLoop.get(j),
          localIndex >= localStart || localIndex < localEnd,
          localIndex >= localStart && localIndex < localEnd)
        canEnqueue(j) && !enqCancel(j) && allowEnqueueByThread.get(requestTid(j)) && bankMatches && localHit
      } else {
        val entryHitBound = Mux(
          enqCrossLoop(j),
          enqLowBound(j).value <= i.U || i.U < enqUpBound(j).value,
          enqLowBound(j).value <= i.U && i.U < enqUpBound(j).value
        )
        canEnqueue(j) && !enqCancel(j) && entryHitBound
      }
    }
    val entryCanEnq = entryCanEnqSeq.reduce(_ || _)
    val selectBits = ParallelPriorityMux(entryCanEnqSeq, io.enq.req.map(_.bits))
    when (entryCanEnq) {
      allocated(i) := true.B
      robIdx(i) := selectBits.robIdx
      isvec(i) :=  FuType.isVLoad(selectBits.fuType)
      committed(i) := false.B

      debug_mmio(i) := false.B
      debug_paddr(i) := 0.U
    }
    XSError(entryCanEnq && allocated(i), s"can't allocated entry twice! ${i}\n")
  }

  for (i <- 0 until io.enq.req.length) {
    val lqIdx = if (EnableSMT2) {
      smt2EnqResp.get(i)
    } else {
      enqPtrExt(0) + validVLoadOffsetRShift.take(i + 1).reduce(_ + _)
    }
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
      val hasExceptions = io.ldin(i).bits.uop.exceptionVec.selectByFu(LduCfg).orR
      when (!need_rep && need_valid) {
        committed(loadWbIndex) := true.B
        //  Debug info
        debug_mmio(loadWbIndex) := io.ldin(i).bits.mmio
        debug_paddr(loadWbIndex) := io.ldin(i).bits.paddr
      }
    }
    XSError((io.ldin(i).bits.uop.robIdx =/= robIdx(loadWbIndex)) && io.ldin(i).valid, s"writeback load robIdx missMatch! at pipeline ${i}\n")
    XSError((!allocated(loadWbIndex) || committed(loadWbIndex)) && io.ldin(i).valid, s"writeback load invalid! at pipeline ${i}\n")
    XSInfo(io.ldin(i).valid && !need_rep && need_valid,
      "load hit write to lq idx %d pc 0x%x vaddr %x paddr %x mask %x mmio %x isvec %x\n",
      io.ldin(i).bits.uop.lqIdx.asUInt,
      io.ldin(i).bits.uop.pc,
      io.ldin(i).bits.vaddr,
      io.ldin(i).bits.paddr,
      io.ldin(i).bits.mask,
      io.ldin(i).bits.mmio,
      io.ldin(i).bits.isvec
    )
  }

  //  perf counter
  QueuePerf(VirtualLoadQueueSize, validCount, !allowEnqueue)
  val vecValidVec = WireInit(VecInit((0 until VirtualLoadQueueSize).map(i => allocated(i) && isvec(i))))
  QueuePerf(VirtualLoadQueueSize, PopCount(vecValidVec), !allowEnqueue)
  io.lqFull := !allowEnqueue

  val perfEvents: Seq[(String, UInt)] = Seq()
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
