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
import org.scalatest.Assertions.===
import utils._
import utility._
import xiangshan._
import xiangshan.cache._
import xiangshan.backend.rob.RobLsqIO
import xiangshan.backend.Bundles._
import xiangshan.backend.fu.FuConfig._

class VsFlowPtr (implicit p: Parameters) extends CircularQueuePtr[VsFlowPtr](
  p => p(XSCoreParamsKey).VsFlowL1Size
){
}

object VsFlowPtr {
  def apply (f: Bool, v: UInt)(implicit p: Parameters): VsFlowPtr = {
    val ptr = Wire(new VsFlowPtr)
    ptr.flag := f
    ptr.value := v
    ptr
  }
}

class VsFlowDataPtr (implicit p: Parameters) extends CircularQueuePtr[VsFlowDataPtr](
  p => p(XSCoreParamsKey).VsFlowL2Size
){
}

object VsFlowDataPtr {
  def apply (f: Bool, v: UInt)(implicit p: Parameters): VsFlowDataPtr = {
    val ptr = Wire(new VsFlowDataPtr)
    ptr.flag := f
    ptr.value := v
    ptr
  }
}

object VSFQFeedbackType {
  val tlbMiss = 0.U(3.W)
  val mshrFull = 1.U(3.W)
  val dataInvalid = 2.U(3.W)
  val bankConflict = 3.U(3.W)
  val ldVioCheckRedo = 4.U(3.W)
  val feedbackInvalid = 7.U(3.W)

  def apply() = UInt(3.W)
}

object GenNextSegmentFieldIdx extends VLSUConstants {
  def apply(fieldIdx: UInt, segmentIdx: UInt, nfields: UInt, offset: UInt): (UInt, UInt) = {
    assert(offset <= 2.U, "not support offset > 2 in GenNextSegmentFieldIdx")
    val newFieldIdx = Wire(UInt(fieldBits.W))
    val newSegmentIdx = Wire(UInt(elemIdxBits.W))
    
    when (nfields === 0.U) {
      newFieldIdx := fieldIdx
      newSegmentIdx := segmentIdx
    } .elsewhen (nfields === 1.U) {
      newFieldIdx := 0.U
      newSegmentIdx := segmentIdx + offset
    } .otherwise {
      val value = fieldIdx +& offset
      when (value >= nfields) {
        newFieldIdx := value - nfields
        newSegmentIdx := segmentIdx + 1.U
      } .otherwise {
        newFieldIdx := value
        newSegmentIdx := segmentIdx
      }
    }
    (newFieldIdx, newSegmentIdx)
  }
}

object GenFieldSegmentOffset extends VLSUConstants {
  def apply(fieldIdx: UInt, segmentIdx: UInt, nSegments: UInt): UInt = {
    (fieldIdx << OHToUInt(nSegments)) | segmentIdx
  }
}

class VSFQFeedback (implicit p: Parameters) extends XSBundle {
  val flowPtr = new VsFlowPtr
  val hit   = Bool()
  //val flushState = Bool()
  val sourceType = VSFQFeedbackType()
  //val dataInvalidSqIdx = new SqPtr
  val paddr = UInt(PAddrBits.W)
  val mmio = Bool()
  val atomic = Bool()
  val exceptionVec = ExceptionVec()
}

class VecStorePipeBundle(implicit p: Parameters) extends MemExuInput(isVector = true) {
  val vaddr               = UInt(VAddrBits.W)
  val mask                = UInt((VLEN/8).W)
  val uop_unit_stride_fof = Bool()
  val alignedType         = UInt(2.W) // ! MAGIC NUM: VLSUConstants.alignTypeBits
  val vecActive          = Bool()
  val flowPtr             = new VsFlowPtr
  val isLastElem          = Bool()
}

class VsFlowBundle(implicit p: Parameters) extends VecFlowBundle {
  val data = UInt(VLEN.W)
  val uopQueuePtr = new VsUopPtr
  val isLastElem = Bool()
  val nfields = UInt(fieldBits.W)
  val nSegments = UInt(elemIdxBits.W)
  val fieldIdx = UInt(fieldBits.W)
  val segmentIdx = UInt(elemIdxBits.W)
}

class VecStoreFlowEntry (implicit p: Parameters) extends VecFlowBundle {
  val uopQueuePtr = new VsUopPtr
  val paddr = UInt(PAddrBits.W)
  val isLastElem = Bool()
  val nfields = UInt(fieldBits.W)
  val nSegments = UInt(elemIdxBits.W)
  val fieldIdx = UInt(fieldBits.W)
  val segmentIdx = UInt(elemIdxBits.W)
  val writeMask = UInt((VLEN/8).W)

  def isFirstElem(): Bool = {
    this.fieldIdx === 0.U && this.segmentIdx === 0.U
  }

  def isInOrder(fieldIdx: UInt, segmentIdx: UInt): Bool = {
    this.fieldIdx === fieldIdx && this.segmentIdx === segmentIdx
  }

  def toPipeBundle(thisPtr: VsFlowPtr): VecStorePipeBundle = {
    val pipeBundle = Wire(new VecStorePipeBundle())
    pipeBundle                      := DontCare
    pipeBundle.uop                  := this.uop
    pipeBundle.src                  := DontCare
    pipeBundle.vaddr                := this.vaddr
    pipeBundle.mask                 := this.mask
    pipeBundle.uop_unit_stride_fof  := false.B
    pipeBundle.alignedType          := this.alignedType
    pipeBundle.vecActive            := this.vecActive
    pipeBundle.flowPtr              := thisPtr
    pipeBundle.isLastElem           := this.isLastElem
    pipeBundle
  }

  def needForward(forward: LoadForwardQueryIO, paddrValid: Bool): Bool = {
    // ! MAGIC NUM
    val vaddrMatch = this.vaddr(VAddrBits - 1, 4) === forward.vaddr(VAddrBits - 1, 4)
    val paddrMatch = Mux(paddrValid, this.paddr(PAddrBits - 1, 4) === forward.paddr(PAddrBits - 1, 4), true.B) // TODO: if paddr not ready, we need to set it ture, we need to fix it in feature 
    val maskMatch = (this.writeMask & forward.mask) =/= 0.U
    val isActive = this.vecActive
    vaddrMatch && paddrMatch && maskMatch && isActive
  }
}

class VecStoreFlowDataFirst (implicit p: Parameters) extends VLSUBundle {
  val data = UInt(16.W)   // 8-bit / 16-bit data, or a pointer to 32-bit / 64-bit data
}

class VsFlowQueueIOBundle(implicit p: Parameters) extends VLSUBundle {
  val redirect = Flipped(ValidIO(new Redirect))
  // receive 2 flows from uop queue each cycle at most
  val flowIn = Vec(VecStorePipelineWidth, Flipped(DecoupledIO(new VsFlowBundle())))
  // writeback 2 flows to uop queue each cycle at most
  // ? Is appropriate to use ExuOutput for writeback? We don't need writeback data.
  val flowWriteback = Vec(VecStorePipelineWidth, DecoupledIO(new VecStoreExuOutput()))

  // each issue port corresponds to an stu
  val pipeIssue = Vec(VecStorePipelineWidth, DecoupledIO(new VecStorePipeBundle()))
  // store feedback, in which `hit` indicates whether tlb misses
  val pipeFeedback = Vec(VecStorePipelineWidth, Flipped(ValidIO(new VSFQFeedback())))

  // await commit signals from rob
  val rob = Flipped(new RobLsqIO)
  // write committed stores to sbuffer
  val sbuffer = Vec(EnsbufferWidth, DecoupledIO(new DCacheWordReqWithVaddrAndPfFlag))
  // inform scalar sq to release the entry when a vector store finishes writing to sbuffer
  val sqRelease = ValidIO(new SqPtr)

  // store-to-load forward
  val forward = Vec(LoadPipelineWidth, Flipped(new LoadForwardQueryIO))

  // MMIO flows execution path
  val uncacheOutstanding = Input(Bool())
  val uncache = new UncacheWordIO

  // update tval when exception happens
  // val exceptionAddrValid = Output(Bool())
  val exceptionAddr = new ExceptionAddrIO

  // when issue last elem, need to mark vector store addrvalid
  val lsq = Vec(VecStorePipelineWidth, Valid(new LsPipelineBundle))
}

class VsExceptionBuffer(implicit p: Parameters) extends VLSUModule with HasCircularQueuePtrHelper {
  val io = IO(new Bundle() {
    val redirect = Flipped(ValidIO(new Redirect))
    val flowWriteback = Vec(VecStorePipelineWidth, Flipped(ValidIO(new VecStoreExuOutput())))
    val exceptionAddr = new ExceptionAddrIO
  })

  val req_valid = RegInit(false.B)
  val req = Reg(new VecStoreExuOutput())

  // enqueue
  // S1:
  val s1_req = VecInit(io.flowWriteback.map(_.bits))
  val s1_valid = VecInit(io.flowWriteback.map(_.valid))
  
  // S2: delay 1 cycle
  val s2_req = RegNext(s1_req)
  val s2_valid = (0 until VecStorePipelineWidth).map(i =>
    RegNext(s1_valid(i)) &&
    !s2_req(i).uop.robIdx.needFlush(RegNext(io.redirect)) && 
    !s2_req(i).uop.robIdx.needFlush(io.redirect)
  )
  val s2_has_exception = s2_req.map(x => ExceptionNO.selectByFu(x.uop.exceptionVec, VstuCfg).asUInt.orR)

  val s2_enqueue = Wire(Vec(VecStorePipelineWidth, Bool()))
  for (w <- 0 until VecStorePipelineWidth) {
    s2_enqueue(w) := s2_valid(w) && s2_has_exception(w)
  }

  when (req_valid && req.uop.robIdx.needFlush(io.redirect)) {
    req_valid := s2_enqueue.asUInt.orR
  }.elsewhen (s2_enqueue.asUInt.orR) {
    req_valid := req_valid || true.B
  }

  def selectOldest[T <: VecStoreExuOutput](valid: Seq[Bool], bits: Seq[T]): (Seq[Bool], Seq[T]) = {
    assert(valid.length == bits.length)
    if (valid.length == 0 || valid.length == 1) {
      (valid, bits)
    } else if (valid.length == 2) {
      val res = Seq.fill(2)(Wire(Valid(chiselTypeOf(bits(0)))))
      for (i <- res.indices) {
        res(i).valid := valid(i)
        res(i).bits := bits(i)
      }
      val oldest = Mux(
        valid(0) && valid(1),
        Mux(
          Cat(bits(0).segmentIdx, bits(0).fieldIdx) < Cat(bits(1).segmentIdx, bits(1).fieldIdx),
          res(0), res(1)
        ),
        Mux(valid(0), res(0), res(1))
      )
      (Seq(oldest.valid), Seq(oldest.bits))
    } else {
      val left = selectOldest(valid.take(valid.length / 2), bits.take(bits.length / 2))
      val right = selectOldest(valid.takeRight(valid.length - (valid.length / 2)), bits.takeRight(bits.length - (bits.length / 2)))
      selectOldest(left._1 ++ right._1, left._2 ++ right._2)
    }
  }

  val reqSel = selectOldest(s2_enqueue, s2_req)

  when (req_valid) {
    req := Mux(
      reqSel._1(0) && Cat(reqSel._2(0).segmentIdx, reqSel._2(0).fieldIdx) < Cat(req.segmentIdx, req.fieldIdx),
      reqSel._2(0), req
    )
  }.elsewhen (s2_enqueue.asUInt.orR) {
    req := reqSel._2(0)
  }

  io.exceptionAddr.vaddr := req.vaddr

}

class VsFlowQueue(implicit p: Parameters) extends VLSUModule with HasCircularQueuePtrHelper
{
  val io = IO(new VsFlowQueueIOBundle())

  /* Storage */

  // circuit queue for flows
  val flowQueueEntries = Reg(Vec(VsFlowL1Size, new VecStoreFlowEntry))
  // Markers
  val flowAllocated = RegInit(VecInit(List.fill(VsFlowL1Size)(false.B)))
  val flowFinished = RegInit(VecInit(List.fill(VsFlowL1Size)(false.B)))
  val flowCommitted = RegInit(VecInit(List.fill(VsFlowL1Size)(false.B)))
  val flowSecondAccess = RegInit(VecInit(List.fill(VsFlowL1Size)(false.B)))
  val mmio = RegInit(VecInit(List.fill(VsFlowL1Size)(false.B)))
  val atomic = RegInit(VecInit(List.fill(VsFlowL1Size)(false.B)))

  // 2-level queue for data
  val dataFirstQueue = Reg(Vec(VsFlowL1Size, new VecStoreFlowDataFirst))
  val dataSecondQueue = Reg(Vec(VsFlowL2Size, UInt(64.W)))

  /* Exception Buffer to save exception vaddr */
  val exceptionBuffer = Module(new VsExceptionBuffer)

  /* Queue Pointers */

  // enqueue pointers, enqPtr(0) is the exact one
  val enqPtr = RegInit(VecInit((0 until VecStorePipelineWidth).map(_.U.asTypeOf(new VsFlowPtr))))
  // issue pointers, issuePtr(0) is the exact one
  val issuePtr = RegInit(VecInit((0 until VecStorePipelineWidth).map(_.U.asTypeOf(new VsFlowPtr))))
  // issue pointers, issuePtr(0) is the exact one
  val writebackPtr = RegInit(VecInit((0 until VecStorePipelineWidth).map(_.U.asTypeOf(new VsFlowPtr))))
  // retire first pointer
  val retireFirstPtr = RegInit(0.U.asTypeOf(new VsFlowPtr))
  // retire pointers, write to sbuffer (if exp) or do nothing (if !exp)
  // this is not in queue order
  val retirePtr = RegInit(VecInit((0 until EnsbufferWidth).map(_.U.asTypeOf(new VsFlowPtr))))
  // dequeue pointers, deqPtr(0) is the exact one
  // val deqPtr = RegInit(VecInit((0 until EnsbufferWidth).map(_.U.asTypeOf(new VsFlowPtr))))

  // Data second queue enqueue pointers
  val dataSecondPtr = RegInit(VecInit((0 until VecStorePipelineWidth).map(_.U.asTypeOf(new VsFlowDataPtr))))

  /* Redirect */
  val flowNeedFlush = Wire(Vec(VsFlowL1Size, Bool()))
  val flowNeedCancel = Wire(Vec(VsFlowL1Size, Bool()))
  val flowCancelCount = PopCount(flowNeedCancel)

  flowNeedFlush := flowQueueEntries.map(_.uop.robIdx.needFlush(io.redirect))
  flowNeedCancel := ((flowNeedFlush zip flowAllocated) zip flowCommitted).map { case((flush, alloc), commited) => flush && alloc && !commited}

  /* Enqueue logic */

  // only allow enqueue when free queue terms >= VecStorePipelineWidth(=2)
  val freeCount = hasFreeEntries(enqPtr(0), retireFirstPtr)
  val allowEnqueue = !io.redirect.valid && freeCount >= VecStorePipelineWidth.U
  io.flowIn.foreach(_.ready := allowEnqueue)

  val canEnqueue = io.flowIn.map(_.valid)
  val enqueueCancel = io.flowIn.map(_.bits.uop.robIdx.needFlush(io.redirect))

  val doEnqueue = Wire(Vec(VecStorePipelineWidth, Bool()))
  val enqueueCount = PopCount(doEnqueue)

  val doDataSecondEnqueue = WireInit(VecInit(List.fill(VecStorePipelineWidth)(false.B)))
  val dataSecondEnqueueCount = PopCount(doDataSecondEnqueue)

  // handshake and do enqueue
  for (i <- 0 until VecStorePipelineWidth) {
    doEnqueue(i) := allowEnqueue && canEnqueue(i) && !enqueueCancel(i)
    // Assuming that if io.flowIn(i).valid then io.flowIn(i-1).valid
    when (doEnqueue(i)) {
      flowAllocated(enqPtr(i).value) := true.B
      mmio(enqPtr(i).value) := false.B
      atomic(enqPtr(i).value) := false.B

      val thisFlowIn = io.flowIn(i).bits
      flowQueueEntries(enqPtr(i).value) match { case x =>
        x.uopQueuePtr := thisFlowIn.uopQueuePtr
        x.vaddr := thisFlowIn.vaddr
        x.mask := thisFlowIn.mask
        x.alignedType := thisFlowIn.alignedType
        x.vecActive := thisFlowIn.vecActive
        x.elemIdx := thisFlowIn.elemIdx
        x.is_first_ele := thisFlowIn.is_first_ele
        x.uop := thisFlowIn.uop
        x.isLastElem := thisFlowIn.isLastElem
        x.nfields := thisFlowIn.nfields
        x.nSegments := thisFlowIn.nSegments
        x.fieldIdx := thisFlowIn.fieldIdx
        x.segmentIdx := thisFlowIn.segmentIdx
        x.writeMask := genVWmask(thisFlowIn.vaddr,thisFlowIn.alignedType)
      }

      // ? Is there a more elegant way?
      when (io.flowIn(i).bits.alignedType === 0.U || io.flowIn(i).bits.alignedType === 1.U) {
        doDataSecondEnqueue(i) := false.B
        flowSecondAccess(enqPtr(i).value) := false.B
        dataFirstQueue(enqPtr(i).value).data := io.flowIn(i).bits.data
      } .otherwise {
        doDataSecondEnqueue(i) := true.B
        flowSecondAccess(enqPtr(i).value) := true.B
        dataFirstQueue(enqPtr(i).value).data := dataSecondPtr(i).value
        dataSecondQueue(dataSecondPtr(i).value) := io.flowIn(i).bits.data
      }
    }
  }

  // update enqPtr
  for (i <- 0 until VecStorePipelineWidth) {
    when (io.redirect.valid) {
      enqPtr(i) := enqPtr(i) - flowCancelCount
    } .otherwise {
      enqPtr(i) := enqPtr(i) + enqueueCount
    }
  }
  // update second Ptr
  for (i <- 0 until VecStorePipelineWidth) {
    dataSecondPtr(i) := dataSecondPtr(i) + dataSecondEnqueueCount
  }

  /* Execute Logic */
  /** Issue Logic **/

  val canIssue = Wire(Vec(VecStorePipelineWidth, Bool()))
  val allowIssue = io.pipeIssue.map(_.ready)
  val doIssue = Wire(Vec(VecStorePipelineWidth, Bool()))
  val inActiveIssue = Wire(Vec(VecStorePipelineWidth,Bool()))
  val issueCount = PopCount(doIssue) + PopCount(inActiveIssue)

  // handshake
  for (i <- 0 until VecStorePipelineWidth) {
    val thisPtr = issuePtr(i).value
    val canIssueToPipline = !flowNeedCancel(thisPtr) && issuePtr(i) < enqPtr(0)
    canIssue(i) := canIssueToPipline && flowQueueEntries(thisPtr).vecActive
    inActiveIssue(i) := canIssueToPipline && !flowQueueEntries(thisPtr).vecActive
    if (i == 0) {
      doIssue(i) := canIssue(i) && allowIssue(i)
      io.pipeIssue(i).valid := canIssue(i)
    } else {
      doIssue(i) := canIssue(i) && allowIssue(i) && allowIssue(i-1)
      io.pipeIssue(i).valid := canIssue(i) && allowIssue(i-1)
    }
    // don't need issue to pipline
    when(inActiveIssue(i)){
      flowFinished(thisPtr) := true.B
    }
    // mark lsq entries addrvalid
    io.lsq(i).valid := inActiveIssue(i) && flowQueueEntries(thisPtr).isLastElem
    io.lsq(i).bits := DontCare // TODO: fix me
    io.lsq(i).bits.uop := flowQueueEntries(thisPtr).uop
  }

  // control signals
  for (i <- 0 until VecStorePipelineWidth) {
    io.pipeIssue(i).bits := flowQueueEntries(issuePtr(i).value).toPipeBundle(issuePtr(i))
  }

  /** Feedback **/
  val doFeedback = io.pipeFeedback.map(_.valid)
  val feedbackHit = io.pipeFeedback.map(_.bits.hit)
  val feedbackPtr = io.pipeFeedback.map(_.bits.flowPtr)
  val feedbackNeedReplay = WireInit(VecInit(List.fill(VecStorePipelineWidth)(false.B)))
  
  for (i <- 0 until VecStorePipelineWidth) {
    when (doFeedback(i)) {
      mmio(feedbackPtr(i).value) := io.pipeFeedback(i).bits.mmio
      atomic(feedbackPtr(i).value) := io.pipeFeedback(i).bits.atomic
      when (feedbackHit(i)) {
        flowFinished(feedbackPtr(i).value) := true.B
        flowQueueEntries(feedbackPtr(i).value).paddr := io.pipeFeedback(i).bits.paddr
        flowQueueEntries(feedbackPtr(i).value).uop.exceptionVec := io.pipeFeedback(i).bits.exceptionVec
        // ? any other need to save?
      } .otherwise {
        feedbackNeedReplay(i) := true.B
      }
    }
  }

  // get the oldest flow ptr
  // TODO: functionalize this
  val oldestReplayFlowPtr = (feedbackNeedReplay zip feedbackPtr).reduce { (a, b) => (
    a._1 || b._1,
    Mux(
      a._1 && ((b._1 && isBefore(a._2, b._2)) || !b._1),
      a._2, b._2
    )
  )}
  // update IssuePtr
  for (i <- 0 until VecStorePipelineWidth) {
    when (io.redirect.valid && flowCancelCount > distanceBetween(enqPtr(0), issuePtr(0))) {
      issuePtr(i) := enqPtr(i) - flowCancelCount
    } .otherwise {
      val issuePtrAfterIssue = issuePtr(i) + issueCount
      val issuePtrAfterReplay = oldestReplayFlowPtr._2 + i.U
      when (oldestReplayFlowPtr._1 && issuePtrAfterReplay < issuePtrAfterIssue) {
        issuePtr(i) := issuePtrAfterReplay
      }.otherwise {
        issuePtr(i) := issuePtrAfterIssue
      }
    }
  }

  /* Writeback */
  val canWriteback = Wire(Vec(VecStorePipelineWidth, Bool()))
  val allowWriteback = io.flowWriteback.map(_.ready)
  val doWriteback = Wire(Vec(VecStorePipelineWidth, Bool()))
  val writebackCount = PopCount(doWriteback)

  for (i <- 0 until VecStorePipelineWidth) {
    val thisPtr = writebackPtr(i).value
    if (i == 0) {
      canWriteback(i) := flowFinished(thisPtr) && writebackPtr(i) < issuePtr(0)
    } else {
      canWriteback(i) := flowFinished(thisPtr) && writebackPtr(i) < issuePtr(0) && canWriteback(i - 1)
    }
    io.flowWriteback(i).valid := canWriteback(i)
  }
  
  // handshake
  for (i <- 0 until VecStorePipelineWidth) {
    doWriteback(i) := canWriteback(i) && allowWriteback(i)
  }

  // update writebackPtr
  for (i <- 0 until VecStorePipelineWidth) {
    when (io.redirect.valid && flowCancelCount > distanceBetween(enqPtr(0), writebackPtr(0))) {
      writebackPtr(i) := enqPtr(i) - flowCancelCount
    } .otherwise {
      writebackPtr(i) := writebackPtr(i) + writebackCount
    }
  }

  /*
  * when redirect, flush writebacked flow entries in 1 cycle 
  * TODO: mayby need redirect in mulit cycle 
  */
  flowNeedCancel.zipWithIndex.map{
    case (enable, i) => {
      when(enable){
        flowAllocated(i) := false.B
        flowFinished(i) := false.B
      }
    }
  }

  for (i <- 0 until VecStorePipelineWidth) {
    val thisPtr = writebackPtr(i).value
    val thisEntry = flowQueueEntries(thisPtr)
    io.flowWriteback(i).bits match { case x =>
      // From XSBundleWithMicroOp
      x.uop           := thisEntry.uop
      // From ExuOutput
      x.data          := DontCare                 // No need to write back data
      // x.fflags        := DontCare
      // x.redirectValid := false.B
      // x.redirect      := DontCare
      x.debug         := DontCare
      // From VecStoreExuOutput
      x.elemIdx := thisEntry.elemIdx
      x.uopQueuePtr   := thisEntry.uopQueuePtr
      x.segmentIdx := thisEntry.segmentIdx
      x.fieldIdx := thisEntry.fieldIdx
      x.vaddr := thisEntry.vaddr
    }
  }

  /* Commit */
  io.rob.mmio := DontCare
  io.rob.uop := DontCare
  for (i <- 0 until VsFlowL1Size) {
    val thisRobIdx = flowQueueEntries(i).uop.robIdx
    when (flowAllocated(i)) {
      when (isNotAfter(io.rob.pendingPtr, thisRobIdx) && isBefore(thisRobIdx, io.rob.pendingPtrNext)) {
        flowCommitted(i) := true.B
      }
    }
  }
  // This part only change false to true.

  /* Write to Sbuffer or to uncache */
  val canEnsbuffer = Wire(Vec(EnsbufferWidth, Bool()))
  val allowEnsbuffer = io.sbuffer.map(_.ready)
  val doEnsbuffer = Wire(Vec(EnsbufferWidth, Bool()))
  val doRetire = Wire(Vec(EnsbufferWidth, Bool()))
  val retireCount = PopCount(doRetire)

  val nfields = RegInit(0.U(fieldBits.W))
  val nSegments = RegInit(0.U(elemIdxBits.W))
  
  // this three cur ptr/idx have multiple copies according to ensbufferWidth
  
  val curFieldIdx = RegInit(VecInit(List.fill(EnsbufferWidth)(0.U(fieldBits.W))))
  val curSegmentIdx = RegInit(VecInit(List.fill(EnsbufferWidth)(0.U(elemIdxBits.W))))

  val sIdle :: sDoing :: Nil = Enum(2)
  val ensbufferState = RegInit(sIdle)

  val us_idle :: us_req :: us_resp :: Nil = Enum(3)
  val uncacheState = RegInit(us_idle)

  val canEnUncache = WireInit(false.B)
  val allowEnUncache = io.rob.pendingst && uncacheState === us_idle
  val doEnUncache = WireInit(false.B)

  // handshake
  for (i <- 0 until EnsbufferWidth) {
    val thisPtr = retirePtr(i).value
    val thisEntry = flowQueueEntries(thisPtr)
    val thisVecActive = thisEntry.vecActive
    val thisInOrder = 
      thisEntry.isInOrder(curFieldIdx(i), curSegmentIdx(i)) &&
      curFieldIdx(i) < nfields && curSegmentIdx(i) < nSegments
    val isMMIO = mmio(thisPtr)
    io.sbuffer(i).valid := canEnsbuffer(i)

    canEnsbuffer(i) := false.B
    doEnsbuffer(i) := false.B
    doRetire(i) := false.B
    when (ensbufferState === sDoing && flowCommitted(thisPtr) && thisInOrder) {
      if (i == 0) {
        canEnsbuffer(i) := thisVecActive && !isMMIO && uncacheState === us_idle
        doEnsbuffer(i) := canEnsbuffer(i) && allowEnsbuffer(i)
        canEnUncache := thisVecActive && isMMIO
        doEnUncache := canEnUncache && allowEnUncache
        doRetire(i) := doEnsbuffer(i) || doEnUncache || !thisVecActive
      } else {
        canEnsbuffer(i) := thisVecActive && !isMMIO && canEnsbuffer(i - 1) && !canEnUncache
        doEnsbuffer(i) := canEnsbuffer(i) && allowEnsbuffer(i)
        doRetire(i) := doEnsbuffer(i) || (!thisVecActive && doRetire(i - 1))
      }
    }
    // Assuming that if !io.sbuffer(i).ready then !io.sbuffer(i + 1).ready
    // sbuffer need 2 cycles to write data, Therefore, we need to delay 1 cycle.
    when (RegNext(doRetire(i))) {
      flowAllocated(RegNext(thisPtr)) := false.B
      flowFinished(RegNext(thisPtr)) := false.B
      flowCommitted(RegNext(thisPtr)) := false.B
    }
  }

  val uncachePAddr = Reg(UInt(PAddrBits.W))
  val uncacheData = Reg(UInt(XLEN.W))
  val uncacheMask = Reg(UInt((XLEN/8).W))
  val uncacheAtomic = Reg(Bool())
  switch (uncacheState) {
    is (us_idle) {
      val thisPtr = retirePtr(0).value
      val thisEntry = flowQueueEntries(thisPtr)
      val thisData = Mux(
        flowSecondAccess(thisPtr),
        dataSecondQueue(dataFirstQueue(thisPtr).data),
        dataFirstQueue(thisPtr).data
      )

      when (doEnUncache && flowAllocated(thisPtr)) {
        uncachePAddr := thisEntry.paddr
        uncacheData := genWdata(thisData, thisEntry.alignedType)
        uncacheMask := genVWmask(thisEntry.paddr, thisEntry.alignedType)
        uncacheAtomic := atomic(thisPtr)

        uncacheState := us_req
      }
    }

    is (us_req) {
      when (io.uncache.req.fire) {
        when (io.uncacheOutstanding) {
          uncacheState := us_idle
        }.otherwise {
          uncacheState := us_resp
        }
      }
    }

    is (us_resp) {
      when (io.uncache.resp.fire) {
        uncacheState := us_idle
      }
    }
  }

  io.uncache.req.valid := uncacheState === us_req
  io.uncache.req.bits match { case x =>
    x := DontCare
    x.cmd := MemoryOpConstants.M_XWR
    x.addr := uncachePAddr
    x.data := uncacheData
    x.mask := uncacheMask
    x.atomic := uncacheAtomic
  }
  io.uncache.resp.ready := uncacheState === us_resp

  // update retirePtr
  for (i <- 0 until EnsbufferWidth) {
    val (newFieldIdx, newSegmentIdx) = 
      GenNextSegmentFieldIdx(curFieldIdx(i), curSegmentIdx(i), nfields, retireCount)
    val nextOffset = GenFieldSegmentOffset(newFieldIdx, newSegmentIdx, nSegments)
    curFieldIdx(i) := newFieldIdx
    curSegmentIdx(i) := newSegmentIdx
    retirePtr(i) := retireFirstPtr + nextOffset
  }

  // Update ensbuffer state
  // From idle to doing
  when (ensbufferState === sIdle) {
    val thisEntry = flowQueueEntries(retireFirstPtr.value)
    when (flowAllocated(retireFirstPtr.value) && thisEntry.isFirstElem) {
      ensbufferState := sDoing
      nfields := thisEntry.nfields
      nSegments := thisEntry.nSegments
      for (i <- 0 until EnsbufferWidth) {
        val (initFieldIdx, initSegmentIdx) = 
          GenNextSegmentFieldIdx(0.U, 0.U, thisEntry.nfields, i.U)
        val nextOffset = GenFieldSegmentOffset(initFieldIdx, initSegmentIdx, thisEntry.nSegments)
        curFieldIdx(i) := initFieldIdx
        curSegmentIdx(i) := initSegmentIdx
        retirePtr(i) := retireFirstPtr + nextOffset
      }
    }
  }
  // From doing to idle
  when (ensbufferState === sDoing) {
    for (i <- 0 until EnsbufferWidth) {
      val thisPtr = retirePtr(i).value
      val thisEntry = flowQueueEntries(thisPtr)
      when (doRetire(i) && thisEntry.isLastElem) {
        ensbufferState := sIdle
        retireFirstPtr := retirePtr(i) + 1.U
      }
    }
  }

  // ensbuffer data
  for (i <- 0 until EnsbufferWidth) {
    val thisPtr = retirePtr(i).value
    val thisEntry = flowQueueEntries(thisPtr)

    val thisData = Wire(UInt(VLEN.W))
    when (flowSecondAccess(thisPtr)) {
      thisData := dataSecondQueue(dataFirstQueue(thisPtr).data)
    } .otherwise {
      thisData := dataFirstQueue(thisPtr).data
    }

    io.sbuffer(i).bits match { case x => 
      // From DCacheWordReq
      x.cmd   := MemoryOpConstants.M_XWR
      x.vaddr := thisEntry.vaddr
      x.data  := genWdata(thisData, thisEntry.alignedType)
      x.mask  := genVWmask(thisEntry.paddr, thisEntry.alignedType)
      x.id    := 0.U                // ! Not Sure
      x.instrtype := 1.U            // ! Not Sure MAGIC NUM
      x.isFirstIssue := false.B     // ! Not Sure
      x.replayCarry := DontCare     // ! Not Sure
      // x.is128bit := false.B         // ! Not Sure
      x.debug_robIdx := thisEntry.uop.robIdx.value
      // From DCacheWordReqWithVaddr
      x.addr  := thisEntry.paddr
      x.wline := false.B            // ! Not Sure
      x.prefetch := false.B
    }
  }

  // Inform scalar sq
  io.sqRelease.valid := false.B
  io.sqRelease.bits := 0.U.asTypeOf(new SqPtr)
  for (i <- 0 until EnsbufferWidth) {
    when (doRetire(i) && flowQueueEntries(retirePtr(i).value).isLastElem) {
      io.sqRelease.valid := true.B
      io.sqRelease.bits := flowQueueEntries(retirePtr(i).value).uop.sqIdx
    }
  }

  // Forward
  for (thisForward <- io.forward) {
    // for every forward query
    // val flowNeedForward = Wire(Vec(VsFlowL1Size, Bool()))
    val flowNeedForward = ((flowQueueEntries zip flowAllocated) zip flowFinished).map{
      case ((entry,valid), paddrValid) => 
        entry.needForward(thisForward, paddrValid) && valid
    }
    val flowForwardMask = flowQueueEntries.map(_.mask & thisForward.mask)
    val doForward = flowNeedForward.reduce(_ || _)
    
    val forwardMask = MuxCase(0.U, (flowNeedForward zip flowForwardMask))

    val dataInvalid = Wire(Bool())
    val matchInvalid = Wire(Bool())
    val addrInvalid = Wire(Bool())

    when (doForward) {
      dataInvalid := true.B
      matchInvalid := false.B
      addrInvalid := false.B
    } .otherwise {
      dataInvalid := false.B
      matchInvalid := false.B
      addrInvalid := false.B
    }

    thisForward.forwardMaskFast := forwardMask.asBools
    thisForward.forwardMask := RegNext(thisForward.forwardMaskFast)
    thisForward.forwardData.map(_ := 0.U)
    thisForward.dataInvalid := RegNext(dataInvalid)
    thisForward.matchInvalid := RegNext(matchInvalid)
    thisForward.addrInvalid := RegNext(addrInvalid)
  }

  // Exception Buffer
  exceptionBuffer.io.redirect := io.redirect
  exceptionBuffer.io.flowWriteback.zipWithIndex.foreach { case (wb, i) =>
    wb.valid := io.flowWriteback(i).fire
    wb.bits := io.flowWriteback(i).bits
  }
  // io.exceptionAddrValid := (flowAllocated.asUInt & ~flowCommitted.asUInt).orR // vec store is the head of rob
  io.exceptionAddr <> exceptionBuffer.io.exceptionAddr
}