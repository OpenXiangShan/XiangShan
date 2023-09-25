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

import chipsalliance.rocketchip.config.Parameters
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
  val exp                 = Bool()
  val is_first_ele        = Bool()
  val flowIdx             = UInt(elemIdxBits.W)
  val flowPtr             = new VlflowPtr
}

class VlFlowQueueIOBundle(implicit p: Parameters) extends VLSUBundle {
  val redirect = Flipped(ValidIO(new Redirect))
  // receive 2 flows from uop queue each cycle at most
  val flowIn = Vec(VecLoadPipelineWidth, Flipped(DecoupledIO(new VlflowBundle())))
  // writeback 2 flows to uop queue each cycle at most
  val flowWriteback = Vec(VecLoadPipelineWidth, DecoupledIO(new VecExuOutput()))

  // TODO: parameterize the flow-issue-with between uop queue and flow queue instead of 2

  // each issue port corresponds to an ldu
  val pipeIssue = Vec(VecLoadPipelineWidth, Decoupled(new VecLoadPipeBundle()))
  // loads that fail and need to be replayed
  val pipeReplay = Vec(VecLoadPipelineWidth, Flipped(DecoupledIO(new LqWriteBundle())))
  // loads that succeed
  val pipeResult = Vec(VecLoadPipelineWidth, Flipped(DecoupledIO(new VecExuOutput())))
}

class VlflowBundle(implicit p: Parameters) extends VecFlowBundle {
  val reg_offset = UInt(vOffsetBits.W)
  val unit_stride_fof   = Bool()
  val uopQueuePtr = new VluopPtr
}

class unitStrideBundle(implicit p: Parameters) extends VLSUBundle {
  val robIdx = new RobPtr
  val counter = UInt(4.W)
}

class VlFlowQueue(implicit p: Parameters) extends VLSUModule
{
  val io = IO(new VlFlowQueueIOBundle())
  println("LoadFlowQueue: size:" + VlFlowSize)

  /* Storage */

  // circuit queue for flows
  val flowQueueBundles = Reg(Vec(VlFlowSize, new VlflowBundle))
  // mark whether a flow is finished
  //   1: finished and can be dequeued
  //   2: issued but not finished
  val flowFinished = RegInit(VecInit(List.fill(VlFlowSize)(false.B)))
  val flowAllocated = RegInit(VecInit(List.fill(VlFlowSize)(false.B)))
  // loaded data from load unit
  val flowLoadResult = Reg(Vec(VlFlowSize, new VecExuOutput))


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

  flowNeedFlush := flowQueueBundles.map(_.uop.robIdx.needFlush(io.redirect))
  flowNeedCancel := (flowNeedFlush zip flowAllocated).map { case(flush, alloc) => flush && alloc}

  /* Enqueue logic */

  // only allow enqueue when free queue terms >= VecLoadPipelineWidth(=2)
  val freeCount = distanceBetween(deqPtr(0), enqPtr(0))
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
      flowQueueBundles(enqPtr(i).value) := io.flowIn(i).bits
      flowAllocated(enqPtr(i).value) := true.B
    }
  }

  // update enqPtr
  // TODO: when redirect happens, need to subtract flushed flows
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
    }
  }
  // update deqPtr
  for (i <- 0 until VecLoadPipelineWidth) {
    deqPtr(i) := deqPtr(i) + dequeueCount
  }
  // write back results
  for (i <- 0 until VecLoadPipelineWidth) {
    io.flowWriteback(i).bits := flowLoadResult(deqPtr(i).value)
  }


  /* Execute logic */
  /** Issue **/

  val canIssue = Wire(Vec(VecLoadPipelineWidth, Bool()))
  val allowIssue = io.pipeIssue.map(_.ready)
  val doIssue = Wire(Vec(VecLoadPipelineWidth, Bool()))
  val issueCount = PopCount(doIssue)

  // handshake
  for (i <- 0 until VecLoadPipelineWidth) {
    val thisPtr = issuePtr(i).value
    canIssue(i) := !flowNeedCancel(thisPtr) && issuePtr(i) < enqPtr(0)
    io.pipeIssue(i).valid := canIssue(i)
    doIssue(i) := canIssue(i) && allowIssue(i)
  }
  // update IssuePtr and finished
  for (i <- 0 until VecLoadPipelineWidth) {
    when (io.redirect.valid && flowCancelCount > distanceBetween(enqPtr(0), issuePtr(0))) {
      issuePtr(i) := enqPtr(i) - flowCancelCount
    } .otherwise {
      issuePtr(i) := issuePtr(i) + issueCount
    }
    when (doIssue(i)) {
      flowFinished(issuePtr(i).value) := false.B
    }
  }
  // data
  for (i <- 0 until VecLoadPipelineWidth) {
    val thisFlow = flowQueueBundles(issuePtr(i).value)
    // It works, but it's not elegant
    io.pipeIssue(i).bits match { case x =>
      x.uop                 := thisFlow.uop
      x.vaddr               := thisFlow.vaddr
      x.mask                := thisFlow.mask
      x.uop_unit_stride_fof := thisFlow.unit_stride_fof
      x.reg_offset          := thisFlow.reg_offset
      x.alignedType         := thisFlow.alignedType
      x.exp                 := thisFlow.exp
      x.is_first_ele        := thisFlow.is_first_ele
      x.flowIdx             := thisFlow.flow_idx
      x.flowPtr             := issuePtr(i)
    }
  }

  /** Replay **/
  val requireReplay = io.pipeReplay.map(_.valid)
  // It seems there isn't anything to prohibit accept a replay
  val allowReplay = WireInit(VecInit(Seq.fill(VecLoadPipelineWidth)(true.B)))
  val doReplay = Wire(Vec(VecLoadPipelineWidth, Bool()))
  // handshake
  for (i <- 0 until VecLoadPipelineWidth) {
    io.pipeReplay(i).ready := allowReplay(i)
    doReplay(i) := requireReplay(i) && allowReplay(i)
  }
  // get the oldest flow ptr
  val oldestReplayFlowPtr = (doReplay zip io.pipeReplay.map(_.bits.flowPtr)).reduce { (a, b) => (
    a._1 || b._1,
    Mux(
      a._1 && ((b._1 && isBefore(a._2, b._2)) || !b._1),
      a._2, b._2
    )
  )}
  // update IssuePtr, this will overlap updating above
  for (i <- 0 until VecLoadPipelineWidth) {
    when (oldestReplayFlowPtr._1) {
      issuePtr(i) := oldestReplayFlowPtr._2 + i.U
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
    val thisPtr = io.pipeResult(i).bits.vec.flowPtr.value
    when (doResult(i)) {
      flowFinished(thisPtr) := true.B
      flowLoadResult(thisPtr) := io.pipeResult(i).bits

      // TODO: DONT use pipeline result directly for many signals are assigned to DontCare in pipeline!!!
      flowLoadResult(thisPtr).vec.uopQueuePtr := flowQueueBundles(thisPtr).uopQueuePtr
    }
  }

}
