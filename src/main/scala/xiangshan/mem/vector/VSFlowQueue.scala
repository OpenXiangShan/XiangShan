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
}

class VecStorePipeBundle(implicit p: Parameters) extends MemExuInput(isVector = true) {
  val vaddr               = UInt(VAddrBits.W)
  val mask                = UInt((VLEN/8).W)
  val uop_unit_stride_fof = Bool()
  val alignedType         = UInt(2.W) // ! MAGIC NUM: VLSUConstants.alignTypeBits
  val exp                 = Bool()
  val flowPtr             = new VsFlowPtr
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
    pipeBundle.exp                  := this.exp
    pipeBundle.flowPtr              := thisPtr
    pipeBundle
  }

  def needForward(forward: LoadForwardQueryIO): Bool = {
    // ! MAGIC NUM
    val vaddrMatch = this.vaddr(VAddrBits - 1, 4) === forward.vaddr(VAddrBits - 1, 4)
    val paddrMatch = this.paddr(PAddrBits - 1, 4) === forward.paddr(PAddrBits - 1, 4)
    val maskMatch = (this.mask & forward.mask) =/= 0.U
    vaddrMatch && paddrMatch && maskMatch
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

  // 2-level queue for data
  val dataFirstQueue = Reg(Vec(VsFlowL1Size, new VecStoreFlowDataFirst))
  val dataSecondQueue = Reg(Vec(VsFlowL2Size, UInt(64.W)))

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
  flowNeedCancel := (flowNeedFlush zip flowAllocated).map { case(flush, alloc) => flush && alloc}

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

      val thisFlowIn = io.flowIn(i).bits
      flowQueueEntries(enqPtr(i).value) match { case x =>
        x.uopQueuePtr := thisFlowIn.uopQueuePtr
        x.vaddr := thisFlowIn.vaddr
        x.mask := thisFlowIn.mask
        x.alignedType := thisFlowIn.alignedType
        x.exp := thisFlowIn.exp
        x.elemIdx := thisFlowIn.elemIdx
        x.is_first_ele := thisFlowIn.is_first_ele
        x.uop := thisFlowIn.uop
        x.isLastElem := thisFlowIn.isLastElem
        x.nfields := thisFlowIn.nfields
        x.nSegments := thisFlowIn.nSegments
        x.fieldIdx := thisFlowIn.fieldIdx
        x.segmentIdx := thisFlowIn.segmentIdx
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

  val canIssue = Wire(Vec(VecLoadPipelineWidth, Bool()))
  val allowIssue = io.pipeIssue.map(_.ready)
  val doIssue = Wire(Vec(VecLoadPipelineWidth, Bool()))
  val issueCount = PopCount(doIssue)

  // handshake
  for (i <- 0 until VecStorePipelineWidth) {
    val thisPtr = issuePtr(i).value
    canIssue(i) := !flowNeedCancel(thisPtr) && issuePtr(i) < enqPtr(0)
    if (i == 0) {
      doIssue(i) := canIssue(i) && allowIssue(i)
      io.pipeIssue(i).valid := canIssue(i)
    } else {
      doIssue(i) := canIssue(i) && allowIssue(i) && allowIssue(i-1)
      io.pipeIssue(i).valid := canIssue(i) && allowIssue(i-1)
    }
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
      when (feedbackHit(i)) {
        flowFinished(feedbackPtr(i).value) := true.B
        flowQueueEntries(feedbackPtr(i).value).paddr := io.pipeFeedback(i).bits.paddr
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
    writebackPtr(i) := writebackPtr(i) + writebackCount
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

  /* Write to Sbuffer */
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

  // handshake
  for (i <- 0 until EnsbufferWidth) {
    val thisPtr = retirePtr(i).value
    val thisEntry = flowQueueEntries(thisPtr)
    val thisExp = thisEntry.exp
    val thisInOrder = 
      thisEntry.isInOrder(curFieldIdx(i), curSegmentIdx(i)) &&
      curFieldIdx(i) < nfields && curSegmentIdx(i) < nSegments
    io.sbuffer(i).valid := canEnsbuffer(i)

    canEnsbuffer(i) := false.B
    doEnsbuffer(i) := false.B
    doRetire(i) := false.B
    when (ensbufferState === sDoing && flowCommitted(thisPtr) && thisInOrder) {
      if (i == 0) {
        canEnsbuffer(i) := thisExp
        doEnsbuffer(i) := canEnsbuffer(i) && allowEnsbuffer(i)
        doRetire(i) := doEnsbuffer(i) || !thisExp
      } else {
        canEnsbuffer(i) := thisExp && canEnsbuffer(i - 1)
        doEnsbuffer(i) := canEnsbuffer(i) && allowEnsbuffer(i)
        doRetire(i) := doEnsbuffer(i) || (!thisExp && doRetire(i - 1))
      }
    }
    // Assuming that if !io.sbuffer(i).ready then !io.sbuffer(i + 1).ready

    when (doRetire(i)) {
      flowAllocated(thisPtr) := false.B
      flowFinished(thisPtr) := false.B
      flowCommitted(thisPtr) := false.B
    }
  }

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
    // val flowNeedForward = Wire(Vec(VsFlowSize, Bool()))
    val flowNeedForward = (flowQueueEntries.zipWithIndex).map{
      case (entry,i) => 
        entry.needForward(thisForward) && flowAllocated(i)
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
}