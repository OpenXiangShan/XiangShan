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

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config._
import xiangshan._
import xiangshan.backend.rob.{RobLsqIO, RobPtr}
import xiangshan.ExceptionNO._
import xiangshan.cache._
import utils._
import utility._
import xiangshan.backend.Bundles.DynInst
import xiangshan.backend.fu.FuConfig.LduCfg

class VirtualLoadQueue(implicit p: Parameters) extends XSModule 
  with HasDCacheParameters
  with HasCircularQueuePtrHelper
  with HasLoadHelper
  with HasPerfEvents
{
  val io = IO(new Bundle() {
    val redirect = Flipped(Valid(new Redirect)) 
    val enq = new LqEnqIO 
    val loadIn = Vec(LoadPipelineWidth, Flipped(DecoupledIO(new LqWriteBundle))) 
    val ldWbPtr = Output(new LqPtr)
    val lqFull = Output(Bool())
    val lqDeq = Output(UInt(log2Up(CommitWidth + 1).W))
    val lqCancelCnt = Output(UInt(log2Up(VirtualLoadQueueSize+1).W))   
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
  val uop = Reg(Vec(VirtualLoadQueueSize, new DynInst))
  val addrvalid = RegInit(VecInit(List.fill(VirtualLoadQueueSize)(false.B))) // non-mmio addr is valid
  val datavalid = RegInit(VecInit(List.fill(VirtualLoadQueueSize)(false.B))) // non-mmio data is valid

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
  val allowEnqueue = validCount <= (VirtualLoadQueueSize - LoadPipelineWidth).U
  val canEnqueue = io.enq.req.map(_.valid)
  val needCancel = WireInit(VecInit((0 until VirtualLoadQueueSize).map(i => {
    uop(i).robIdx.needFlush(io.redirect) && allocated(i) 
  })))
  val lastNeedCancel = RegNext(needCancel)
  val enqCancel = io.enq.req.map(_.bits.robIdx.needFlush(io.redirect))
  val lastEnqCancel = PopCount(RegNext(VecInit(canEnqueue.zip(enqCancel).map(x => x._1 && x._2))))
  val lastCycleCancelCount = PopCount(lastNeedCancel)

  // update enqueue pointer
  val enqCount = Mux(io.enq.canAccept && io.enq.sqCanAccept, PopCount(io.enq.req.map(_.valid)), 0.U)
  val enqPtrExtNextVec = Wire(Vec(io.enq.req.length, new LqPtr))
  val enqPtrExtNext = Wire(Vec(io.enq.req.length, new LqPtr))
  when (lastCycleRedirect.valid) {
    // we recover the pointers in the next cycle after redirect
    enqPtrExtNextVec := VecInit(enqPtrExt.map(_ - (lastCycleCancelCount + lastEnqCancel)))
  }.otherwise {
    enqPtrExtNextVec := VecInit(enqPtrExt.map(_ + enqCount))
  } 

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
  val deqLookup = VecInit(deqLookupVec.map(ptr => allocated(ptr.value) && datavalid(ptr.value) && addrvalid(ptr.value) && ptr =/= enqPtrExt(0)))
  val deqInSameRedirectCycle = VecInit(deqLookupVec.map(ptr => needCancel(ptr.value)))
  // make chisel happy
  val deqCountMask = Wire(UInt(DeqPtrMoveStride.W)) 
  deqCountMask := deqLookup.asUInt & ~deqInSameRedirectCycle.asUInt
  val commitCount = PopCount(PriorityEncoderOH(~deqCountMask) - 1.U)
  val lastCommitCount = RegNext(commitCount)

  // update deqPtr
  // cycle 1: generate deqPtrNext
  // cycle 2: update deqPtr
  val deqPtrUpdateEna = lastCommitCount =/= 0.U 
  deqPtrNext := deqPtr + lastCommitCount
  deqPtr := RegEnable(next = deqPtrNext, init = 0.U.asTypeOf(new LqPtr), enable = deqPtrUpdateEna)

  io.lqDeq := RegNext(lastCommitCount)
  io.lqCancelCnt := RegNext(lastCycleCancelCount + lastEnqCancel)
  io.ldWbPtr := deqPtr 

  /**
   * Enqueue at dispatch
   * 
   * Currently, VirtualLoadQueue only allows enqueue when #emptyEntries > EnqWidth
   */
  io.enq.canAccept := allowEnqueue
  for (i <- 0 until io.enq.req.length) {
    val offset = PopCount(io.enq.needAlloc.take(i))
    val lqIdx = enqPtrExt(offset)
    val index = io.enq.req(i).bits.lqIdx.value
    when (canEnqueue(i) && !enqCancel(i)) {
      allocated(index) := true.B
      uop(index) := io.enq.req(i).bits
      uop(index).lqIdx := lqIdx

      // init
      addrvalid(index) := false.B 
      datavalid(index) := false.B

      debug_mmio(index) := false.B
      debug_paddr(index) := 0.U

      XSError(!io.enq.canAccept || !io.enq.sqCanAccept, s"must accept $i\n")
      XSError(index =/= lqIdx.value, s"must be the same entry $i\n")
    } 
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
      XSError(!allocated((deqPtr+i.U).value), s"why commit invalid entry $i?\n")
    }
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
    io.loadIn(i).ready := true.B
    val loadWbIndex = io.loadIn(i).bits.uop.lqIdx.value

    when (io.loadIn(i).valid) {
      val hasExceptions = ExceptionNO.selectByFu(io.loadIn(i).bits.uop.exceptionVec, LduCfg).asUInt.orR
      val needReplay = io.loadIn(i).bits.replayInfo.needReplay()

      when (!needReplay) {
      // update control flag
        addrvalid(loadWbIndex) := hasExceptions || !io.loadIn(i).bits.tlbMiss
        datavalid(loadWbIndex) := 
          (if (EnableFastForward) {
              hasExceptions ||
              io.loadIn(i).bits.mmio ||
             !io.loadIn(i).bits.miss && // dcache miss
             !io.loadIn(i).bits.dcacheRequireReplay // do not writeback if that inst will be resend from rs
           } else {
              hasExceptions ||
              io.loadIn(i).bits.mmio ||
             !io.loadIn(i).bits.miss 
           })

        // 
        when (io.loadIn(i).bits.lqDataWenDup(1)) {
          uop(loadWbIndex) := io.loadIn(i).bits.uop
        }
        when (io.loadIn(i).bits.lqDataWenDup(4)) {
          uop(loadWbIndex).debugInfo := io.loadIn(i).bits.uop.debugInfo
        }
        uop(loadWbIndex).debugInfo := io.loadIn(i).bits.replayInfo.debug

        //  Debug info
        debug_mmio(loadWbIndex) := io.loadIn(i).bits.mmio 
        debug_paddr(loadWbIndex) := io.loadIn(i).bits.paddr

        XSInfo(io.loadIn(i).valid, "load hit write to lq idx %d pc 0x%x vaddr %x paddr %x mask %x forwardData %x forwardMask: %x mmio %x\n",
          io.loadIn(i).bits.uop.lqIdx.asUInt,
          io.loadIn(i).bits.uop.pc,
          io.loadIn(i).bits.vaddr,
          io.loadIn(i).bits.paddr,
          io.loadIn(i).bits.mask,
          io.loadIn(i).bits.forwardData.asUInt,
          io.loadIn(i).bits.forwardMask.asUInt,
          io.loadIn(i).bits.mmio
        )    
      }
    }
  }

  if (env.EnableTopDown) {
    val stall_loads_bound = WireDefault(0.B)
    ExcitingUtils.addSink(stall_loads_bound, "stall_loads_bound", ExcitingUtils.Perf)
    val have_miss_entry = (allocated zip datavalid).map(x => x._1 && !x._2).reduce(_ || _)
    val l1d_loads_bound = stall_loads_bound && !have_miss_entry
    ExcitingUtils.addSource(l1d_loads_bound, "l1d_loads_bound", ExcitingUtils.Perf)
    XSPerfAccumulate("l1d_loads_bound", l1d_loads_bound)
    val stall_l1d_load_miss = stall_loads_bound && have_miss_entry
    ExcitingUtils.addSource(stall_l1d_load_miss, "stall_l1d_load_miss", ExcitingUtils.Perf)
    ExcitingUtils.addSink(WireInit(0.U), "stall_l1d_load_miss", ExcitingUtils.Perf)
  }

  //  perf counter
  QueuePerf(VirtualLoadQueueSize, validCount, !allowEnqueue)
  io.lqFull := !allowEnqueue
  val perfEvents: Seq[(String, UInt)] = Seq()
  generatePerfEvent() 

  // debug info
  XSDebug("enqPtrExt %d:%d deqPtrExt %d:%d\n", enqPtrExt(0).flag, enqPtr, deqPtr.flag, deqPtr.value)

  def PrintFlag(flag: Bool, name: String): Unit = {
    when(flag) {
      XSDebug(false, true.B, name)
    }.otherwise {
      XSDebug(false, true.B, " ")
    }
  }

  for (i <- 0 until VirtualLoadQueueSize) {
    XSDebug(i + " pc %x pa %x ", uop(i).pc, debug_paddr(i))
    PrintFlag(allocated(i), "v")
    PrintFlag(allocated(i) && datavalid(i), "d")
    PrintFlag(allocated(i) && addrvalid(i), "a")
    PrintFlag(allocated(i) && addrvalid(i) && datavalid(i), "w")
    XSDebug(false, true.B, "\n")
  }
  // end
}