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
import xiangshan.backend.rob.{RobPtr, RobLsqIO}
import xiangshan.ExceptionNO._
import xiangshan.cache._
import utils._
import utility._

class LoadQueueFlag(implicit p: Parameters) extends XSModule 
  with HasDCacheParameters
  with HasCircularQueuePtrHelper
  with HasLoadHelper
  with HasPerfEvents
{
  val io = IO(new Bundle() {
    val redirect = Flipped(Valid(new Redirect)) 
    val enq = new LqEnqIO 
    val loadIn = Vec(LoadPipelineWidth, Flipped(Valid(new LqWriteBundle))) 
    val loadOut = Vec(LoadPipelineWidth, DecoupledIO(new ExuOutput))
    val ldRawDataOut = Vec(LoadPipelineWidth, Output(new LoadDataFromLQBundle))
    val rob = Flipped(new RobLsqIO) 
    val uncache = new UncacheWordIO 
    val trigger = Vec(LoadPipelineWidth, new LqTriggerIO)
    val exceptionAddr = new ExceptionAddrIO
    val ldIssuePtr = Output(new LqPtr)
    val lqFull = Output(Bool())
    val lqEmpty = Output(Bool())
    val lqDeq = Output(UInt(log2Up(CommitWidth + 1).W))
    val lqCancelCnt = Output(UInt(log2Up(LoadQueueFlagSize+1).W))   
  })

  println("LoadQueueFlag: size: " + LoadQueueFlagSize)
  //  LoadQueueFlag field
  //  +-----------+---------+-------+-------+------+-------+
  //  | Allocated | MicroOp | VAddr | PAddr | Mask | Flags |
  //  +-----------+---------+-------+-------+------+-------+
  //  Allocated   : entry has been allocated already
  //  MicroOp     : inst's microOp
  //  VAddr       : virtual address 
  //  PAddr       : physical address
  //  Flags       : load flags
  val allocated = RegInit(VecInit(List.fill(LoadQueueFlagSize)(false.B))) // The control signals need to explicitly indicate the initial value
  val uop = Reg(Vec(LoadQueueFlagSize, new MicroOp))
  val vaddrModule = Module(new LqVAddrModule(
    gen = UInt(VAddrBits.W), 
    numEntries = LoadQueueFlagSize, 
    numRead = 2, 
    numWrite = LoadPipelineWidth,
    numWBank = LoadQueueNWriteBanks,
    numWDelay = 2,
    numCamPort = 0))
  vaddrModule.io := DontCare 
  val paddrModule = Module(new LqPAddrModule(
    gen = UInt(PAddrBits.W), 
    numEntries = LoadQueueFlagSize, 
    numRead = 1, 
    numWrite = LoadPipelineWidth,
    numWBank = LoadQueueNWriteBanks,
    numWDelay = 2,
    numCamPort = 0))
  paddrModule.io := DontCare
  val maskModule = Module(new LqMaskModule(
    gen = UInt(8.W), 
    numEntries = LoadQueueFlagSize, 
    numRead = 1, 
    numWrite = LoadPipelineWidth, 
    numWBank = LoadQueueNWriteBanks,
    numWDelay = 2,
    numCamPort = 0))
  maskModule.io := DontCare 
  val addrvalid = RegInit(VecInit(List.fill(LoadQueueFlagSize)(false.B))) // non-mmio addr is valid
  val datavalid = RegInit(VecInit(List.fill(LoadQueueFlagSize)(false.B))) // non-mmio data is valid
  val pending = RegInit(VecInit(List.fill(LoadQueueFlagSize)(false.B))) // inst is an mmio inst
  val writebacked = RegInit(VecInit(List.fill(LoadQueueFlagSize)(false.B))) // inst writebacked

  /**
   * used for debug
   */ 
  val debug_mmio = Reg(Vec(LoadQueueFlagSize, Bool())) // mmio: inst is an mmio inst
  val debug_paddr = Reg(Vec(LoadQueueFlagSize, UInt(PAddrBits.W))) // mmio: inst's paddr

  /**
   * used for trigger
   */
  val vaddrTriggerResultModule = Module(new LqTriggerResultModule(
    gen = Vec(3, Bool()),
    numEntries = LoadQueueFlagSize, 
    numRead = LoadPipelineWidth, 
    numWrite = LoadPipelineWidth, 
    numWDelay = 2,
    numWBank = LoadQueueNWriteBanks
  ))
  vaddrTriggerResultModule.io := DontCare  

  //  maintain pointers
  val enqPtrExt = RegInit(VecInit((0 until io.enq.req.length).map(_.U.asTypeOf(new LqPtr))))
  val enqPtr = enqPtrExt(0).value
  val deqPtrExt = Wire(new LqPtr)
  val deqPtrExtNext = Wire(new LqPtr)
  val deqPtr = deqPtrExt.value 
  val issPtrExt = RegInit(0.U.asTypeOf(new LqPtr))

  /**
   * update pointer
   */
  val lastCycleRedirect = RegNext(io.redirect)
  val lastLastCycleRedirect = RegNext(lastCycleRedirect)

  val validCount = distanceBetween(enqPtrExt(0), deqPtrExt)
  val allowEnqueue = validCount <= (LoadQueueFlagSize - LoadPipelineWidth).U
  val canEnqueue = io.enq.req.map(_.valid)
  val needCancel = WireInit(VecInit((0 until LoadQueueFlagSize).map(i => {
    uop(i).robIdx.needFlush(io.redirect) && allocated(i) 
  })))
  val lastNeedCancel = RegNext(needCancel)
  val enqCancel = io.enq.req.map(_.bits.robIdx.needFlush(io.redirect))
  val lastEnqCancel = PopCount(RegNext(VecInit(canEnqueue.zip(enqCancel).map(x => x._1 && x._2))))
  val lastCycleCancelCount = PopCount(lastNeedCancel)
  val enqCount = Mux(io.enq.canAccept && io.enq.sqCanAccept, PopCount(io.enq.req.map(_.valid)), 0.U)
  val enqPtrExtNext = Wire(Vec(io.enq.req.length, new LqPtr))
  when (lastCycleRedirect.valid) {
    // we recover the pointers in the next cycle after redirect
    enqPtrExtNext := VecInit(enqPtrExt.map(_ - (lastCycleCancelCount + lastEnqCancel)))
  }.otherwise {
    enqPtrExtNext := VecInit(enqPtrExt.map(_ + enqCount))
  } 
  enqPtrExt := enqPtrExtNext

  // update dequeue ptr
  val DeqPtrMoveStride = CommitWidth
  require(DeqPtrMoveStride == CommitWidth, "DeqPtrMoveStride must be equal to CommitWidth!")
  val deqLookupVec = VecInit((0 until DeqPtrMoveStride).map(deqPtrExt + _.U))
  val deqLookup = VecInit(deqLookupVec.map(ptr => allocated(ptr.value) && datavalid(ptr.value) && addrvalid(ptr.value)  && writebacked(ptr.value) && ptr =/= enqPtrExt(0)))
  val deqInSameRedirectCycle = VecInit(deqLookupVec.map(ptr => needCancel(ptr.value)))
  // make chisel happy
  val deqCountMask = Wire(UInt(DeqPtrMoveStride.W)) 
  deqCountMask := deqLookup.asUInt & ~deqInSameRedirectCycle.asUInt
  val commitCount = PopCount(PriorityEncoderOH(~deqCountMask) - 1.U)
  val lastCommitCount = RegNext(commitCount)

  // update deqPtrExt
  // cycle 1: generate deqPtrExtNext
  // cycle 2: update deqPtrExt
  val deqPtrUpdateEna = lastCommitCount =/= 0.U 
  deqPtrExtNext := deqPtrExt + lastCommitCount
  deqPtrExt := RegEnable(next = deqPtrExtNext, init = 0.U.asTypeOf(new LqPtr), enable = deqPtrUpdateEna)

  io.lqDeq := RegNext(lastCommitCount)
  io.lqCancelCnt := RegNext(lastCycleCancelCount + lastEnqCancel)

  // update ldIssuePtr
  val IssPtrMoveStride = CommitWidth  
  val issLookupVec = VecInit((0 until IssPtrMoveStride).map(issPtrExt + _.U))
  val issLookup = VecInit(issLookupVec.map(ptr => allocated(ptr.value) && (pending(ptr.value) || datavalid(ptr.value) && addrvalid(ptr.value)) && ptr =/= enqPtrExt(0)))
  val issInSameRedirectCycle = VecInit(issLookupVec.map(ptr => needCancel(ptr.value)))
  // make chisel happy
  val issCountMask = Wire(UInt(IssPtrMoveStride.W))
  issCountMask := issLookupVec.asUInt & ~issInSameRedirectCycle.asUInt
  val issCount = PopCount(PriorityEncoderOH(~issCountMask) - 1.U)
  val lastIssCount = RegNext(issCount)

  when (lastCycleRedirect.valid) {
    issPtrExt := Mux(isAfter(enqPtrExtNext(0), issPtrExt), issPtrExt, enqPtrExtNext(0))
  } .otherwise {
    issPtrExt := issPtrExt + issCount
  }
  io.ldIssuePtr := issPtrExt

  /**
   * Enqueue at dispatch
   * 
   * Currently, LoadQueueFlag only allows enqueue when #emptyEntries > EnqWidth
   */
  io.enq.canAccept := allowEnqueue
  for (i <- 0 until io.enq.req.length) {
    val offset = if (i == 0) 0.U else PopCount(io.enq.needAlloc.take(i))
    val lqIdx = enqPtrExt(offset)
    val index = io.enq.req(i).bits.lqIdx.value
    when (canEnqueue(i) && !enqCancel(i)) {
      allocated(index) := true.B
      uop(index) := io.enq.req(i).bits
      uop(index).lqIdx := lqIdx

      // init
      addrvalid(index) := false.B 
      datavalid(index) := false.B 
      pending(index) := false.B
      writebacked(index) := false.B

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
    * When load commited, mark it as !allocated and move deqPtrExt forward.
    */
  (0 until DeqPtrMoveStride).map(i => {
    when (commitCount > i.U) {
      allocated((deqPtrExt+i.U).value) := false.B
      XSError(!allocated((deqPtrExt+i.U).value), s"why commit invalid entry $i?\n")
    }
  })

  // misprediction recovery / exception redirect
  // invalidate lq term using robIdx
  for (i <- 0 until LoadQueueFlagSize) {
    when (needCancel(i)) {
      allocated(i) := false.B
    }
  }

  XSDebug(p"(ready, valid): ${io.enq.canAccept}, ${Binary(Cat(io.enq.req.map(_.valid)))}\n")

  io.lqEmpty := RegNext(
    enqPtrExt(0).value === deqPtrExt.value && 
    enqPtrExt(0).flag === deqPtrExt.flag
  )


  /**
    * Writeback load from load units
    *
    * Most load instructions writeback to regfile at the same time.
    * However,
    *   (1) For ready load instruction (not mmio and not need replay), it writes back to ROB immediately.
    *   (2) For an mmio instruction with exceptions, it writes back to ROB immediately.
    *   (3) For an mmio instruction without exceptions, it does not write back.
    * The mmio instruction will be sent to lower level when it reaches ROB's head.
    * After uncache response, it will write back through arbiter with loadUnit.
    */  
  for   (i <- 0 until LoadPipelineWidth) {
    // IMPORTANT: control or status signals must be initialized at first.
    vaddrModule.io.wen(i) := false.B
    paddrModule.io.wen(i) := false.B
    maskModule.io.wen(i) := false.B
    vaddrTriggerResultModule.io.wen(i) := false.B

    val loadWbIndex = io.loadIn(i).bits.uop.lqIdx.value

    //   most lq status need to be updated immediately after load writeback to lq
    //   flag bits in lq needs to be updated accurately     
    when (io.loadIn(i).valid) {
      val hasExceptions = ExceptionNO.selectByFu(io.loadIn(i).bits.uop.cf.exceptionVec, lduCfg).asUInt.orR
      val needRepay = io.loadIn(i).bits.replayInfo.needReplay()

      // update control flag
      addrvalid(loadWbIndex) := !io.loadIn(i).bits.tlbMiss
      if (EnableFastForward) {
        datavalid(loadWbIndex) := !io.loadIn(i).bits.mmio && // mmio data is not valid until we finished uncache access 
                                  !io.loadIn(i).bits.dcacheRequireReplay // do not writeback if that inst will be resend from rs
      } else {
        datavalid(loadWbIndex) := !io.loadIn(i).bits.mmio // mmio data is not valid until we finished uncache access
      }
      pending(loadWbIndex) := io.loadIn(i).bits.mmio 
      writebacked(loadWbIndex) := !io.loadIn(i).bits.mmio && !hasExceptions && !needRepay

      // 
      when (io.loadIn(i).bits.lqDataWenDup(1)) {
        uop(loadWbIndex).pdest := io.loadIn(i).bits.uop.pdest 
      }
      when (io.loadIn(i).bits.lqDataWenDup(2)) {
        uop(loadWbIndex).cf := io.loadIn(i).bits.uop.cf
      }
      when (io.loadIn(i).bits.lqDataWenDup(3)) {
        uop(loadWbIndex).ctrl := io.loadIn(i).bits.uop.ctrl
      }
      when (io.loadIn(i).bits.lqDataWenDup(4)) {
        uop(loadWbIndex).debugInfo := io.loadIn(i).bits.uop.debugInfo
      }
      when (io.loadIn(i).bits.lqDataWenDup(5)) {
        vaddrTriggerResultModule.io.wen(i) := true.B
        vaddrTriggerResultModule.io.waddr(i) := loadWbIndex
        vaddrTriggerResultModule.io.wdata(i) := io.trigger(i).hitLoadAddrTriggerHitVec
      }

      uop(loadWbIndex).debugInfo := io.loadIn(i).bits.replayInfo.debug

      vaddrModule.io.wen(i) := true.B 
      vaddrModule.io.waddr(i) := loadWbIndex
      vaddrModule.io.wdata(i) := io.loadIn(i).bits.vaddr

      paddrModule.io.wen(i) := true.B 
      paddrModule.io.waddr(i) := loadWbIndex 
      paddrModule.io.wdata(i) := io.loadIn(i).bits.paddr

      maskModule.io.wen(i) := true.B 
      maskModule.io.waddr(i) := loadWbIndex
      maskModule.io.wdata(i) := io.loadIn(i).bits.mask

      //  Debug info
      debug_mmio(loadWbIndex) := io.loadIn(i).bits.mmio 
      debug_paddr(loadWbIndex) := io.loadIn(i).bits.paddr

      XSInfo(io.loadIn(i).valid, "load hit write to lq idx %d pc 0x%x vaddr %x paddr %x mask %x forwardData %x forwardMask: %x mmio %x\n",
        io.loadIn(i).bits.uop.lqIdx.asUInt,
        io.loadIn(i).bits.uop.cf.pc,
        io.loadIn(i).bits.vaddr,
        io.loadIn(i).bits.paddr,
        io.loadIn(i).bits.mask,
        io.loadIn(i).bits.forwardData.asUInt,
        io.loadIn(i).bits.forwardMask.asUInt,
        io.loadIn(i).bits.mmio
      )    
    }
    // to ROB, indicate that it's mmio instruction.
    io.rob.isMMIO(i) := RegNext(io.loadIn(i).valid && io.loadIn(i).bits.mmio)
    io.rob.uop(i) := RegNext(io.loadIn(i).bits.uop)
  }

  // Read vaddr for mem exception
  // no inst will be commited 1 cycle before tval update
  vaddrModule.io.raddr(1) := deqPtrExtNext.value 
  io.exceptionAddr.vaddr := vaddrModule.io.rdata(1)

  // read vaddr for mmio, and only port 0 is used 
  (0 until LoadPipelineWidth).map(i => {
    if (i == 0) {
      vaddrTriggerResultModule.io.raddr(i) := deqPtr
      io.trigger(i).lqLoadAddrTriggerHitVec := Mux(
        io.loadOut(i).valid,
        vaddrTriggerResultModule.io.rdata(i),
        VecInit(Seq.fill(3)(false.B))
      )
    }  else {
      vaddrTriggerResultModule.io.raddr(i) := DontCare
      io.trigger(i).lqLoadAddrTriggerHitVec := VecInit(Seq.fill(3)(false.B))
    }
  })

  /**
    * Memory mapped IO / other uncached operations
    *
    * States:
    * (1) writeback from store units: mark as pending
    * (2) when they reach ROB's head, they can be sent to uncache channel
    * (3) response from uncache channel: mark as datavalid
    * (4) writeback to ROB (and other units): mark as writebacked
    * (5) ROB commits the instruction: same as normal instructions
    */
  //(2) when they reach ROB's head, they can be sent to uncache channel
  val lqTailAllocated = WireInit(allocated(deqPtr))
  val lqTailMmioPending = WireInit(pending(deqPtr))
  val lqTailWriteback = WireInit(writebacked(deqPtr))
  val s_idle::s_req::s_resp::s_wait::Nil = Enum(4)
  val uncacheState = RegInit(s_idle)
  val uncacheCommitFired = RegInit(false.B)

  when(uncacheState === s_req) {
    uncacheCommitFired := false.B
  }

  switch (uncacheState) {
    is (s_idle) {
      when (RegNext(io.rob.pendingld && lqTailMmioPending && lqTailAllocated && !lqTailWriteback)) {
        uncacheState := s_req
      }
    }
    is (s_req) {
      when (io.uncache.req.fire) {
        uncacheState := s_resp
      }
    }
    is (s_resp) {
      when (io.uncache.resp.fire) {
        uncacheState := s_wait
      }
    }
    is (s_wait) {
      when (RegNext(io.rob.commit)) {
        uncacheState := s_idle // ready for next mmio
      }
    }
  }  

  vaddrModule.io.raddr(0) := deqPtrExtNext.value
  paddrModule.io.raddr(0) := deqPtrExtNext.value
  maskModule.io.raddr(0) := deqPtrExtNext.value

  io.uncache.req.valid := uncacheState === s_req
  io.uncache.req.bits.cmd := MemoryOpConstants.M_XRD
  io.uncache.req.bits.data := DontCare
  io.uncache.req.bits.addr := paddrModule.io.rdata(0)
  io.uncache.req.bits.mask := maskModule.io.rdata(0)
  io.uncache.req.bits.id := RegNext(deqPtrExtNext.value)
  io.uncache.req.bits.instrtype := DontCare
  io.uncache.req.bits.replayCarry := DontCare  
  io.uncache.req.bits.atomic := true.B 

  io.uncache.resp.ready := true.B

  when (io.uncache.req.fire) {
    pending(deqPtr) := false.B
    XSDebug("uncache req: pc %x addr %x data %x op %x mask %x\n",
      uop(deqPtr).cf.pc,
      io.uncache.req.bits.addr,
      io.uncache.req.bits.data,
      io.uncache.req.bits.cmd,
      io.uncache.req.bits.mask
    )
  }  

  // (3) response from uncache channel: mark as datavalid
  val uncacheData = Reg(io.uncache.resp.bits.data.cloneType)
  when (io.uncache.resp.fire) {
    datavalid(deqPtr) := true.B
    uncacheData := io.uncache.resp.bits.data
  }

  //  MMIO writeback
  val loadWbSel = deqPtr
  val selUop = uop(loadWbSel)
  val func = selUop.ctrl.fuOpType
  val raddr = paddrModule.io.rdata(0)
  val rdataSel = LookupTree(raddr(2, 0), List(
      "b000".U -> uncacheData(63,  0),
      "b001".U -> uncacheData(63,  8),
      "b010".U -> uncacheData(63, 16),
      "b011".U -> uncacheData(63, 24),
      "b100".U -> uncacheData(63, 32),
      "b101".U -> uncacheData(63, 40),
      "b110".U -> uncacheData(63, 48),
      "b111".U -> uncacheData(63, 56)
    ))
  val rdataPartialLoad = rdataHelper(selUop, rdataSel)

  io.loadOut(0).valid := (uncacheState === s_wait) && !uncacheCommitFired
  io.loadOut(0).bits := DontCare
  io.loadOut(0).bits.uop := selUop
  io.loadOut(0).bits.uop.lqIdx.flag := DontCare
  io.loadOut(0).bits.uop.lqIdx.value := loadWbSel
  io.loadOut(0).bits.data := rdataPartialLoad 
  io.loadOut(0).bits.redirectValid := false.B
  io.loadOut(0).bits.redirect := DontCare
  io.loadOut(0).bits.debug.isMMIO := true.B
  io.loadOut(0).bits.debug.paddr := debug_paddr(loadWbSel)
  io.loadOut(0).bits.debug.vaddr := vaddrModule.io.rdata(0)
  io.loadOut(0).bits.fflags := DontCare

  // just use one port
  io.loadOut(1).bits := DontCare
  io.loadOut(1).valid := false.B

  io.ldRawDataOut(0).lqData := uncacheData
  io.ldRawDataOut(0).uop := io.loadOut(0).bits.uop 
  io.ldRawDataOut(0).addrOffset := paddrModule.io.rdata(0)

  io.ldRawDataOut(1) := DontCare

  when (io.loadOut(0).fire) {
    writebacked(deqPtr) := true.B
    uncacheCommitFired := true.B

    XSInfo("int load miss write to cbd robidx %d lqidx %d pc 0x%x mmio %x\n",
      io.loadOut(0).bits.uop.robIdx.asUInt,
      io.loadOut(0).bits.uop.lqIdx.asUInt,
      io.loadOut(0).bits.uop.cf.pc,
      true.B
    )    
  }

  /**
   * misc
   */
  //  perf counter
  QueuePerf(LoadQueueFlagSize, validCount, !allowEnqueue)
  io.lqFull := !allowEnqueue

  XSPerfAccumulate("mmioCycle", uncacheState =/= s_idle)
  XSPerfAccumulate("mmioCnt", io.uncache.req.fire)
  XSPerfAccumulate("mmio_writeback_success", io.loadOut(0).fire)
  XSPerfAccumulate("mmio_writeback_blocked", io.loadOut(0).valid && !io.loadOut(0).ready)

  val perfEvents: Seq[(String, UInt)] = Seq(
    ("mmioCycle", uncacheState =/= s_idle),
    ("mmioCnt", io.uncache.req.fire),
    ("mmio_writeback_success", io.loadOut(0).fire),
    ("mmio_writeback_blocked", io.loadOut(0).valid && !io.loadOut(0).ready)
  )
  generatePerfEvent() 
  // end
}