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
import utils._
import utility._
import xiangshan._
import xiangshan.backend.rob.RobPtr

class VlflowPtr(implicit p: Parameters) extends CircularQueuePtr[VlflowPtr](
  p => p(XSCoreParamsKey).VlFlowSize
){
}

object VlflowPtr {
  def apply(f: Bool, v: UInt)(implicit p: Parameters): VlflowPtr = {
    val ptr = Wire(new VlflowPtr)
    ptr.flag := f
    ptr.value := v
    ptr
  }
}

class VecLoadPipeBundle(implicit p: Parameters) extends VLSUBundleWithMicroOp {
  val vaddr               = UInt(VAddrBits.W)
  val mask                = UInt(VLENB.W)
  val uop_unit_stride_fof = Bool()
  // val rob_idx_valid       = Vec(2,Bool())
  // val rob_idx             = Vec(2,new RobPtr)
  // val inner_idx           = Vec(2,UInt(3.W))
  val reg_offset          = UInt(vOffsetBits.W)
  // val offset              = Vec(2,UInt(4.W))
  val alignedType         = UInt(alignTypeBits.W)
  val vecActive           = Bool() // 1: vector active element, 0: vector not active element
  val is_first_ele        = Bool()
  val flowPtr             = new VlflowPtr
  val isFirstIssue        = Bool()
}

class VlFlowQueueIOBundle(implicit p: Parameters) extends VLSUBundle {
  val redirect = Flipped(ValidIO(new Redirect))
  // receive 2 flows from uop queue each cycle at most
  val flowIn = Vec(VecLoadPipelineWidth, Flipped(DecoupledIO(new VlflowBundle())))
  // writeback 2 flows to uop queue each cycle at most
  val flowWriteback = Vec(VecLoadPipelineWidth, DecoupledIO(new VecExuOutput()))

  // TODO: parameterize the flow-issue-width between uop queue and flow queue instead of 2

  // each issue port corresponds to an ldu
  val pipeIssue = Vec(VecLoadPipelineWidth, Decoupled(new VecLoadPipeBundle()))
  // loads that fail and need to be replayed
  val pipeReplay = Vec(VecLoadPipelineWidth, Flipped(DecoupledIO(new LsPipelineBundle())))
  // mmio loads that fail because uncache resources are full
  val mmioReplay = Vec(VecLoadPipelineWidth, Flipped(DecoupledIO(new LsPipelineBundle())))
  // loads that succeed
  val pipeResult = Vec(VecLoadPipelineWidth, Flipped(DecoupledIO(new VecExuOutput())))
}

class VlflowBundle(implicit p: Parameters) extends VecFlowBundle {
  val reg_offset = UInt(vOffsetBits.W)
  val unit_stride_fof   = Bool()
  val uopQueuePtr = new VluopPtr
  val elemIdxInsideVd = UInt(elemIdxBits.W)
}

class VlFlowQueue(implicit p: Parameters) extends VLSUModule
  with HasCircularQueuePtrHelper
{
  val io = IO(new VlFlowQueueIOBundle())
  println("LoadFlowQueue: size:" + VlFlowSize)

  /* Storage */

  // circuit queue for flows
  val flowQueueEntries = Reg(Vec(VlFlowSize, new VlflowBundle))
  // mark whether a flow is finished
  //   1: finished and can be dequeued
  //   2: issued but not finished
  val flowFinished = RegInit(VecInit(List.fill(VlFlowSize)(false.B)))
  val flowAllocated = RegInit(VecInit(List.fill(VlFlowSize)(false.B)))
  // loaded data from load unit
  val flowLoadResult = Reg(Vec(VlFlowSize, new VecExuOutput))
  // issued is only used for debugging and perf counters, to indicate whether a valid flow is issued for the first time
  val issued = RegInit(VecInit(Seq.fill(VlFlowSize)(false.B)))


  /* Queue Pointers */

  // enqueue pointers, enqPtr(0) is the exact one
  val enqPtr = RegInit(VecInit((0 until VecLoadPipelineWidth).map(_.U.asTypeOf(new VlflowPtr))))
  // dequeue pointers, deqPtr(0) is the exact one
  val deqPtr = RegInit(VecInit((0 until VecLoadPipelineWidth).map(_.U.asTypeOf(new VlflowPtr))))
  // issue pointers, issuePtr(0) is the exact one
  val issuePtr = RegInit(VecInit((0 until VecLoadPipelineWidth).map(_.U.asTypeOf(new VlflowPtr))))

  /* Redirect */
  val flowNeedFlush = Wire(Vec(VlFlowSize, Bool()))
  val flowNeedCancel = Wire(Vec(VlFlowSize, Bool()))
  val flowCancelCount = PopCount(flowNeedCancel)

  flowNeedFlush := flowQueueEntries.map(_.uop.robIdx.needFlush(io.redirect))
  flowNeedCancel := (flowNeedFlush zip flowAllocated).map { case(flush, alloc) => flush && alloc}

  /* Enqueue logic */

  // only allow enqueue when free queue terms >= VecLoadPipelineWidth(=2)
  val freeCount = hasFreeEntries(enqPtr(0), deqPtr(0))
  val allowEnqueue = !io.redirect.valid && freeCount >= VecLoadPipelineWidth.U
  for (i <- 0 until VecLoadPipelineWidth) {
    io.flowIn(i).ready := allowEnqueue
  }

  val canEnqueue = io.flowIn.map(_.valid)
  val enqueueCancel = io.flowIn.map(_.bits.uop.robIdx.needFlush(io.redirect))

  val doEnqueue = Wire(Vec(VecLoadPipelineWidth, Bool()))
  val enqueueCount = PopCount(doEnqueue)

  // enqueue flows
  for (i <- 0 until VecLoadPipelineWidth) {
    doEnqueue(i) := allowEnqueue && canEnqueue(i) && !enqueueCancel(i)
    // Assuming that if io.flowIn(i).valid then io.flowIn(i-1).valid
    when (doEnqueue(i)) {
      flowQueueEntries(enqPtr(i).value) := io.flowIn(i).bits
      flowAllocated(enqPtr(i).value) := true.B
      issued(enqPtr(i).value) := false.B
    }
  }

  // update enqPtr
  for (i <- 0 until VecLoadPipelineWidth) {
    when (io.redirect.valid) {
      enqPtr(i) := enqPtr(i) - flowCancelCount
    } .otherwise {
      enqPtr(i) := enqPtr(i) + enqueueCount
    }
  }


  /* Dequeue logic */

  val canDequeue = Wire(Vec(VecLoadPipelineWidth, Bool()))
  val allowDequeue = io.flowWriteback.map(_.ready)
  val doDequeue = Wire(Vec(VecLoadPipelineWidth, Bool()))
  val dequeueCount = PopCount(doDequeue)

  for (i <- 0 until VecLoadPipelineWidth) {
    val thisPtr = deqPtr(i).value
    if (i == 0) {
      canDequeue(i) := flowFinished(thisPtr) && !flowNeedCancel(thisPtr) && deqPtr(i) < issuePtr(0)
    } else {
      canDequeue(i) := flowFinished(thisPtr) && !flowNeedCancel(thisPtr) && deqPtr(i) < issuePtr(0) && canDequeue(i - 1)
    }
    io.flowWriteback(i).valid := canDequeue(i)
  }

  // handshake
  for (i <- 0 until VecLoadPipelineWidth) {
    doDequeue(i) := canDequeue(i) && allowDequeue(i)
    when (doDequeue(i)) {
      flowAllocated(deqPtr(i).value) := false.B
      flowFinished(deqPtr(i).value) := false.B
      issued(deqPtr(i).value) := false.B
    }
  }
  // flowAllocated.zip(flowNeedCancel).foreach { case (v, cancel) => when (cancel) { v := false.B } }
  flowNeedCancel.zipWithIndex.foreach { case (cancel, i) =>
    when (cancel) {
      flowAllocated(i) := false.B
      flowFinished(i) := false.B
      issued(i) := false.B
    }
  }
  // update deqPtr
  for (i <- 0 until VecLoadPipelineWidth) {
    deqPtr(i) := deqPtr(i) + dequeueCount
  }
  // write back results
  for (i <- 0 until VecLoadPipelineWidth) {
    val thisLoadResult = flowLoadResult(deqPtr(i).value)
    val thisLoadEntries = flowQueueEntries(deqPtr(i).value)
    val isActiveElem = thisLoadEntries.exp

    val isvec             = Mux(isActiveElem, thisLoadResult.vec.isvec, true.B)   // ?  Can this be false ?
    val vecdata           = Mux(isActiveElem, thisLoadResult.vec.vecdata, 0.U(VLEN.W))
    val mask              = Mux(isActiveElem, thisLoadResult.vec.mask, thisLoadEntries.mask)
    val reg_offset        = Mux(isActiveElem, thisLoadResult.vec.reg_offset, thisLoadEntries.reg_offset)
    val exp               = Mux(isActiveElem, thisLoadResult.vec.vecActive, thisLoadEntries.exp)
    val is_first_ele      = Mux(isActiveElem, thisLoadResult.vec.is_first_ele, thisLoadEntries.is_first_ele)
    val elemIdx           = Mux(isActiveElem, thisLoadResult.vec.elemIdx, thisLoadEntries.elemIdx)
    val elemIdxInsideVd   = flowQueueEntries(deqPtr(i).value).elemIdxInsideVd
    val uopQueuePtr       = Mux(isActiveElem, thisLoadResult.vec.uopQueuePtr, thisLoadEntries.uopQueuePtr)
    val flowPtr           = deqPtr(i)
      // From ExuOutput
    val debug             = Mux(isActiveElem, thisLoadResult.debug, 0.U.asTypeOf(thisLoadResult.debug))
    val uop               = Mux(isActiveElem, thisLoadResult.uop, thisLoadEntries.uop)

    io.flowWriteback(i).bits match { case x =>
      // From VecExuOutput
      x.vec.isvec         := isvec
      x.vec.vecdata       := vecdata
      x.vec.mask          := mask
      x.vec.reg_offset    := reg_offset
      x.vec.exp           := exp
      x.vec.is_first_ele  := is_first_ele
      x.vec.elemIdx       := elemIdx
      x.vec.elemIdxInsideVd := elemIdxInsideVd
      x.vec.uopQueuePtr   := uopQueuePtr
      x.vec.flowPtr       := deqPtr(i)
      // From ExuOutput
      x.data              := DontCare
      // x.fflags            := thisLoadResult.fflags
      // x.redirectValid     := thisLoadResult.redirectValid
      // x.redirect          := thisLoadResult.redirect
      x.debug             := debug
      x.uop               := uop
    }
  }


  /* Execute logic */
  /** Issue **/

  val canIssue = Wire(Vec(VecLoadPipelineWidth, Bool()))
  val allowIssue = io.pipeIssue.map(_.ready)
  val doIssue = Wire(Vec(VecLoadPipelineWidth, Bool()))
  // if is inactivative elem, don't issue it to pipline, and mark it as finished.
  val inActiveIssue = Wire(Vec(VecLoadPipelineWidth, Bool())) 
  val issueCount = PopCount(doIssue) + PopCount(inActiveIssue)

  // handshake
  for (i <- 0 until VecLoadPipelineWidth) {
    val thisPtr = issuePtr(i).value
    val canIssueToPipline = !flowNeedCancel(thisPtr) && issuePtr(i) < enqPtr(0)
    // Assuming that if io.flowIn(i).ready then io.flowIn(i-1).ready
    canIssue(i) := canIssueToPipline && flowQueueEntries(thisPtr).exp
    if (i == 0) {
      doIssue(i) := canIssue(i) && allowIssue(i)
      io.pipeIssue(i).valid := canIssue(i)
      inActiveIssue(i) := canIssueToPipline && !flowQueueEntries(thisPtr).exp // first inactivative element not need to wait pipline ready
    } else {
      doIssue(i) := canIssue(i) && allowIssue(i) && allowIssue(i-1)
      io.pipeIssue(i).valid := canIssue(i) && allowIssue(i-1)
      inActiveIssue(i) := canIssueToPipline && !flowQueueEntries(thisPtr).exp && (allowIssue(i-1) || inActiveIssue(i-1)) // need to wait pipeline ready
    }
  }

  for (i <- 0 until VecLoadPipelineWidth) {
    when (doIssue(i)) {
      flowFinished(issuePtr(i).value) := false.B
      issued(issuePtr(i).value) := true.B
    }.elsewhen(inActiveIssue(i) && flowAllocated(issuePtr(i).value)){
      flowFinished(issuePtr(i).value) := true.B
      issued(issuePtr(i).value) := true.B
    }
  }
  // data
  for (i <- 0 until VecLoadPipelineWidth) {
    val thisFlow = flowQueueEntries(issuePtr(i).value)
    // It works, but it's not elegant
    io.pipeIssue(i).bits match { case x =>
      x.uop                 := thisFlow.uop
      x.vaddr               := thisFlow.vaddr
      x.mask                := thisFlow.mask
      x.uop_unit_stride_fof := thisFlow.unit_stride_fof
      x.reg_offset          := thisFlow.reg_offset
      x.alignedType         := thisFlow.alignedType
      x.vecActive                 := thisFlow.exp
      x.is_first_ele        := thisFlow.is_first_ele
      x.flowPtr             := issuePtr(i)
      x.isFirstIssue        := !issued(issuePtr(i).value)
    }
  }

  /** Replay **/
  val replays = io.pipeReplay ++ io.mmioReplay
  val requireReplay = replays.map(_.valid)
  val replayWidth = replays.length
  // It seems there isn't anything to prohibit accept a replay
  val allowReplay = WireInit(VecInit(Seq.fill(replayWidth)(true.B)))
  val doReplay = Wire(Vec(replayWidth, Bool()))
  // handshake
  for (i <- 0 until replayWidth) {
    replays(i).ready := allowReplay(i)
    doReplay(i) := requireReplay(i) && allowReplay(i)
  }
  // get the oldest flow ptr
  // TODO: functionalize this
  val oldestReplayFlowPtr = (doReplay zip replays.map(_.bits.flowPtr)).reduce { (a, b) => (
    a._1 || b._1,
    Mux(
      a._1 && ((b._1 && isBefore(a._2, b._2)) || !b._1),
      a._2, b._2
    )
  )}
  // update IssuePtr
  for (i <- 0 until VecLoadPipelineWidth) {
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

  /** Result **/
  val requireResult = io.pipeResult.map(_.valid)
  // It seems there isn't anything to prohibit accept a result
  val allowResult = WireInit(VecInit(Seq.fill(VecLoadPipelineWidth)(true.B)))
  val doResult = Wire(Vec(VecLoadPipelineWidth, Bool()))
  // handshake
  for (i <- 0 until VecLoadPipelineWidth) {
    io.pipeResult(i).ready := allowResult(i)
    doResult(i) := requireResult(i) && allowResult(i)
  }
  // update data and finished
  for (i <- 0 until VecLoadPipelineWidth) {
    val thisPipeResult = io.pipeResult(i).bits
    val thisPtr = thisPipeResult.vec.flowPtr.value
    when (doResult(i)) {
      flowFinished(thisPtr) := true.B
      flowLoadResult(thisPtr) match { case x =>
        // From VecExuOutput
        x.vec.isvec         := thisPipeResult.vec.isvec   // ?  Can this be false ?
        x.vec.vecdata       := thisPipeResult.vec.vecdata
        x.vec.mask          := thisPipeResult.vec.mask
        x.vec.reg_offset    := thisPipeResult.vec.reg_offset
        x.vec.vecActive           := thisPipeResult.vec.vecActive
        x.vec.is_first_ele  := thisPipeResult.vec.is_first_ele
        x.vec.elemIdx       := flowQueueEntries(thisPtr).elemIdx
        x.vec.uopQueuePtr   := flowQueueEntries(thisPtr).uopQueuePtr
        x.vec.flowPtr       := DontCare
        // From ExuOutput
        x.data              := DontCare
        // x.fflags            := thisPipeResult.fflags
        // x.redirectValid     := thisPipeResult.redirectValid
        // x.redirect          := thisPipeResult.redirect
        x.debug             := thisPipeResult.debug
        x.uop               := thisPipeResult.uop
      }
    }
  }

}