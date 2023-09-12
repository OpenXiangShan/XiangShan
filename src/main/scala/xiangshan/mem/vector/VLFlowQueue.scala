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
// class UsQueuePtr(implicit p: Parameters) extends CircularQueuePtr[UsQueuePtr](
//   p => p(XSCoreParamsKey).UsQueueSize
// ){
// }

// object UsQueuePtr {
//   def apply(f: Bool, v: UInt)(implicit p: Parameters): UsQueuePtr = {
//     val ptr = Wire(new UsQueuePtr)
//     ptr.flag := f
//     ptr.value := v
//     ptr
//   }
// }

/**
 * (1) unit-stride instructions access to memory continously, so calculate the address by adding 16 directly (flow_inner_idx << 4.U)
 * (2) stride instructions: flow_inner_idx means the current number of UOP memory accesses,
 *     uopIdx << Log2Num(GenRealFlowNum(instType,emul,eew,sew)) means the number of all previous UOP memory accesses
 * (3) index instructions: According to flow_ inner_idx obtains immediate value from index, than Calculate address
 * (4) segment instructions: To calculate the address, segment instructions need calculate segEmulIdx and segNfIdx;
 * */
object GenVLAddr {
  def apply (instType: UInt, baseaddr: UInt, emul:UInt, lmul:UInt, uopIdx:UInt, flow_inner_idx: UInt, stride: UInt,
             index: UInt, eew: UInt, sew: UInt, nf:UInt, segNfIdx: UInt, segMulIdx: UInt): UInt = {
    (LookupTree(instType,List(
      "b000".U -> (baseaddr + (flow_inner_idx << 4.U).asUInt).asUInt,// unit-stride
      "b010".U -> (baseaddr + stride * (flow_inner_idx + (uopIdx << Log2Num(GenRealFlowNum(instType,emul,lmul,eew,sew))).asUInt)),// strided
      "b001".U -> (baseaddr + IndexAddr(index= index, flow_inner_idx = flow_inner_idx, eew = eew)), // indexed-unordered
      "b011".U -> (baseaddr + IndexAddr(index= index, flow_inner_idx = flow_inner_idx, eew = eew)), // indexed-ordered
      "b100".U -> (baseaddr +
                    (((flow_inner_idx + (segMulIdx << Log2Num(GenRealFlowNum(instType,emul,lmul,eew,sew))).asUInt).asUInt * nf) << eew(1,0)).asUInt +
                    (segNfIdx << eew(1,0)).asUInt),// segment unit-stride
      "b110".U -> (baseaddr +
                    (flow_inner_idx + (segMulIdx << Log2Num(GenRealFlowNum(instType,emul,lmul,eew,sew))).asUInt).asUInt * stride +
                    (segNfIdx << eew(1,0)).asUInt), // segment strided
      "b101".U -> (baseaddr + IndexAddr(index= index, flow_inner_idx = flow_inner_idx, eew = eew) + (segNfIdx << sew(1,0)).asUInt), // segment indexed-unordered
      "b111".U -> (baseaddr + IndexAddr(index= index, flow_inner_idx = flow_inner_idx, eew = eew) + (segNfIdx << sew(1,0)).asUInt)  // segment indexed-ordered
    )))}
}
/*
object GenRobIdx {
  def apply (instType: UInt, robIdx:UInt, startRobIdx: UInt, flow_inner_idx: UInt): UInt = {
    (LookupTree(instType,List(
      "b000".U ->  (startRobIdx + flow_inner_idx),// unit-stride, do not use
      "b010".U ->  (robIdx), // strided
      "b001".U ->  (robIdx), // indexed-unordered
      "b011".U ->  (robIdx), // indexed-ordered
      "b100".U ->  (robIdx), // segment unit-stride
      "b110".U ->  (robIdx), // segment strided
      "b101".U ->  (robIdx), // segment indexed-unordered
      "b111".U ->  (robIdx)  // segment indexed-ordered
    )))}
}*/

object VLRegOffset {
  def apply (instType: UInt, flow_inner_idx: UInt, eew: UInt, sew: UInt): UInt = {
    (LookupTree(instType,List(
      "b000".U ->  0.U                           , // unit-stride
      "b010".U ->  (flow_inner_idx << eew(1,0)).asUInt, // strided
      "b001".U ->  (flow_inner_idx << sew(1,0)).asUInt, // indexed-unordered
      "b011".U ->  (flow_inner_idx << sew(1,0)).asUInt, // indexed-ordered
      "b100".U ->  (flow_inner_idx << eew(1,0)).asUInt, // segment unit-stride
      "b110".U ->  (flow_inner_idx << eew(1,0)).asUInt, // segment strided
      "b101".U ->  (flow_inner_idx << sew(1,0)).asUInt, // segment indexed-unordered
      "b111".U ->  (flow_inner_idx << sew(1,0)).asUInt  // segment indexed-ordered
    )))}
}

object VLExpCtrl {
  def apply (vstart: UInt, vl: UInt, eleIdx: UInt):Bool = {
    val exp = Wire(Bool())
    when (vstart >= vl || vl === 0.U) {
      exp := false.B
    }.otherwise {
      when (eleIdx >= vstart && eleIdx < vl) {
        exp := true.B
      }.otherwise {
        exp := false.B
      }
    }
    exp
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

// class VlFlowQueueIOBundle(implicit p: Parameters) extends XSBundle {
//   val loadRegIn    = Vec(VecLoadPipelineWidth, Flipped(Decoupled(new ExuInput(isVpu = true))))
//   val redirect     = Flipped(ValidIO(new Redirect))
//   val flowFeedback = Vec(VecLoadPipelineWidth, ValidIO(Bool()))
//   val eew          = Vec(VecLoadPipelineWidth, Input(UInt(3.W)))
//   val sew          = Vec(VecLoadPipelineWidth, Input(UInt(3.W)))
//   val emul         = Vec(VecLoadPipelineWidth, Input(UInt(3.W)))
//   val instType     = Vec(VecLoadPipelineWidth, Input(UInt(3.W)))
//   val uop_unit_stride_fof = Vec(VecLoadPipelineWidth, Input(Bool()))
//   val whole_reg   = Vec(VecLoadPipelineWidth, Input(Bool()))
//   val uop_segment_num = Vec(VecLoadPipelineWidth, Input(UInt(3.W)))
//   val realFlowNum = Vec(VecLoadPipelineWidth, Input(UInt(5.W)))
//   val loadPipeOut = Vec(VecLoadPipelineWidth, Decoupled(new VecLoadPipeBundle))
// }

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

  /**
    * TODO @xzf
    */
  io <> DontCare

  /* Storage */

  // circuit queue for flows
  val flowQueueBundles = Reg(Vec(VlFlowSize, new VlflowBundle))
  // mark whether a flow is finished
  //   1: finished and can be dequeued
  //   2: issued but not finished
  val flowFinished = RegInit(VecInit(List.fill(VlFlowSize)(false.B)))
  // loaded data from load unit
  val flowLoadResult = Reg(Vec(VlFlowSize, new VecExuOutput))


  /* Queue Pointers */

  // enqueue pointers, enqPtr(0) is the exact one
  val enqPtr = RegInit(VecInit((0 until VecLoadPipelineWidth).map(_.U.asTypeOf(new VlflowPtr))))
  // dequeue pointers, deqPtr(0) is the exact one
  val deqPtr = RegInit(VecInit((0 until VecLoadPipelineWidth).map(_.U.asTypeOf(new VlflowPtr))))
  // issue pointers, issuePtr(0) is the exact one
  val issuePtr = RegInit(VecInit((0 until VecLoadPipelineWidth).map(_.U.asTypeOf(new VlflowPtr))))


  /* Enqueue logic */

  // only allow enqueue when free queue terms >= VecLoadPipelineWidth(=2)
  val freeCount = distanceBetween(deqPtr(0), enqPtr(0))
  val allowEnqueue = freeCount >= VecLoadPipelineWidth.U
  for (i <- 0 until VecLoadPipelineWidth) {
    io.flowIn(i).ready := allowEnqueue
  }

  val canEnqueue = io.flowIn.map(_.valid)
  val needCancel = io.flowIn.map(_.bits.uop.robIdx.needFlush(io.redirect))

  val doEnqueue = Wire(Vec(VecLoadPipelineWidth, Bool()))
  val enqueueCount = PopCount(doEnqueue)

  // enqueue flows
  for (i <- 0 until VecLoadPipelineWidth) {
    doEnqueue(i) := allowEnqueue && canEnqueue(i) && !needCancel(i)
    // Assuming that if io.flowIn(i).valid then io.flowIn(i-1).valid
    when (doEnqueue(i)) {
      flowQueueBundles(enqPtr(i).value) := io.flowIn(i).bits
    }
  }

  // update enqPtr
  // TODO: when redirect happens, need to subtract flushed flows
  for (i <- 0 until VecLoadPipelineWidth) {
    enqPtr(i) := enqPtr(i) + enqueueCount
  }


  /* Dequeue logic */

  val canDequeue = Wire(Vec(VecLoadPipelineWidth, Bool()))
  val allowDequeue = io.flowWriteback.map(_.ready)
  val doDequeue = Wire(Vec(VecLoadPipelineWidth, Bool()))
  val dequeueCount = PopCount(doDequeue)

  for (i <- 0 until VecLoadPipelineWidth) {
    if (i == 0) {
      canDequeue(i) := flowFinished(i) && deqPtr(i) < issuePtr(0)
    } else {
      canDequeue(i) := flowFinished(i) && deqPtr(i) < issuePtr(0) && canDequeue(i - 1)
    }
    io.flowWriteback(i).valid := canDequeue(i)
  }

  // handshake
  for (i <- 0 until VecLoadPipelineWidth) {
    doDequeue(i) := canDequeue(i) && allowDequeue(i)
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
    canIssue(i) := issuePtr(i) < enqPtr(0)
    io.pipeIssue(i).valid := canIssue(i)
    doIssue(i) := canIssue(i) && allowIssue(i)
  }
  // update IssuePtr and finished
  for (i <- 0 until VecLoadPipelineWidth) {
    issuePtr(i) := issuePtr(i) + issueCount
    when (doIssue(i)) {
      flowFinished(issuePtr(i).value) := false.B
    }
  }
  // data
  for (i <- 0 until VecLoadPipelineWidth) {
    val thisFlow = flowQueueBundles(issuePtr(i).value)
    // It works, but it's not elegant
    io.pipeIssue(i).bits match { case x =>
      x.vaddr               := thisFlow.vaddr
      x.mask                := thisFlow.mask
      x.uop_unit_stride_fof := thisFlow.unit_stride_fof
      x.reg_offset          := thisFlow.reg_offset
      x.alignedType         := thisFlow.alignedType
      x.exp                 := thisFlow.exp
      x.is_first_ele        := thisFlow.is_first_ele
      x.flowIdx             := thisFlow.flowIdx
      x.flowPtr             := issuePtr(i)
    }
  }

  /** Replay **/
  val requireReplay = io.pipeReplay.map(_.valid)
  // It seems there isn't anything to prohibit accept a replay
  val allowReplay = Vec(VecLoadPipelineWidth, true.B)
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
  val allowResult = Vec(VecLoadPipelineWidth, true.B)
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
    }
  }


//   dontTouch(io.loadRegIn)
//   // TODO: merge these to an FlowQueue Entry?
//   val flow_entry       = RegInit(VecInit(Seq.fill(VecLoadPipelineWidth)(VecInit(Seq.fill(VlFlowSize)(0.U.asTypeOf(new VlflowBundle))))))
//   val flow_entry_valid = RegInit(VecInit(Seq.fill(VecLoadPipelineWidth)(VecInit(Seq.fill(VlFlowSize)(false.B)))))
//   val unitStrideValid  = RegInit(VecInit(Seq.fill(VecLoadPipelineWidth)(VecInit(Seq.fill(UsQueueSize)(false.B)))))
//   val unitStrideEntry  = RegInit(VecInit(Seq.fill(VecLoadPipelineWidth)(VecInit(Seq.fill(UsQueueSize)(0.U.asTypeOf(new unitStrideBundle))))))

//   val loadRegInValid  = WireInit(VecInit(Seq.fill(VecLoadPipelineWidth)(false.B)))
//   val UsNeedFlush     = WireInit(VecInit(Seq.fill(VecLoadPipelineWidth)(VecInit(Seq.fill(UsQueueSize)(false.B)))))
//   val needFlush       = WireInit(VecInit(Seq.fill(VecLoadPipelineWidth)(VecInit(Seq.fill(VlFlowSize)(false.B)))))
//   val UsRedirectCnt   = RegInit(VecInit(Seq.fill(VecLoadPipelineWidth)(0.U(log2Up(UsQueueSize).W))))
//   val flowRedirectCnt = RegInit(VecInit(Seq.fill(VecLoadPipelineWidth)(0.U(log2Up(VsFlowSize).W))))
//   val cam             = WireInit(VecInit(Seq.fill(VecLoadPipelineWidth)(VecInit(Seq.fill(VecLoadPipelineWidth)(VecInit(Seq.fill(UsQueueSize)(false.B)))))))
//   val uSAlloc         = Wire(Vec(VecLoadPipelineWidth,Bool()))
//   val needAlloc       = Wire(Vec(VecLoadPipelineWidth, Bool()))
//   val baseAddr        = Wire(Vec(VecLoadPipelineWidth, UInt(VAddrBits.W)))
//   val dataWidth       = Wire(Vec(VecLoadPipelineWidth, UInt(8.W))) // only unit-stride use
//   val vend            = Wire(Vec(VecLoadPipelineWidth, UInt(8.W)))
//   val realFlowNum     = Wire(Vec(VecLoadPipelineWidth, UInt(5.W)))

//   val cross128    = Wire(Vec(VecLoadPipelineWidth, Bool()))
//   val uopIdx      = Wire(Vec(VecLoadPipelineWidth, UInt(6.W)))
//   val instType    = Wire(Vec(VecLoadPipelineWidth, UInt(3.W)))
//   val stride      = Wire(Vec(VecLoadPipelineWidth, UInt(XLEN.W)))
//   val index       = Wire(Vec(VecLoadPipelineWidth, UInt(VLEN.W)))
//   val eew         = Wire(Vec(VecLoadPipelineWidth, UInt(3.W)))
//   val sew         = Wire(Vec(VecLoadPipelineWidth, UInt(3.W)))
//   val emul        = Wire(Vec(VecLoadPipelineWidth, UInt(3.W)))
//   val lmul        = Wire(Vec(VecLoadPipelineWidth, UInt(3.W)))
//   val mul         = Wire(Vec(VecLoadPipelineWidth, UInt(3.W)))
//   val emulNum     = Wire(Vec(VecLoadPipelineWidth, UInt(4.W)))
//   val lmulNum     = Wire(Vec(VecLoadPipelineWidth, UInt(4.W)))
//   val vma         = Wire(Vec(VecLoadPipelineWidth, Bool()))
//   val vta         = Wire(Vec(VecLoadPipelineWidth, Bool()))
//   val vl          = Wire(Vec(VecLoadPipelineWidth, UInt(8.W)))
//   val vmask       = Wire(Vec(VecLoadPipelineWidth, UInt(VLEN.W)))
//   val vstart      = Wire(Vec(VecLoadPipelineWidth, UInt(8.W)))
//   val segMulIdx   = Wire(Vec(VecLoadPipelineWidth, UInt(6.W)))
//   val segNfIdx    = Wire(Vec(VecLoadPipelineWidth, UInt(6.W)))
//   val alignedType = Wire(Vec(VecLoadPipelineWidth, UInt(2.W)))

//   val enqPtr = RegInit(VecInit(Seq.fill(VecLoadPipelineWidth)(0.U.asTypeOf(new VlflowPtr))))
//   val deqPtr = RegInit(VecInit(Seq.fill(VecLoadPipelineWidth)(0.U.asTypeOf(new VlflowPtr))))
//   val uSEnqPtr = RegInit(VecInit(Seq.fill(VecLoadPipelineWidth)(0.U.asTypeOf(new UsQueuePtr))))
//   val uSDeqPtr = RegInit(VecInit(Seq.fill(VecLoadPipelineWidth)(0.U.asTypeOf(new UsQueuePtr))))

//   for (i <- 0 until VecLoadPipelineWidth) {
//     io.loadRegIn(i).ready := PopCount(flow_entry_valid(i)) <= 16.U && PopCount(unitStrideValid(i)) <= 7.U//TODO:
//   }

//   /**
//     * vlFlowQueue enqPtr update */
//   val lastRedirect = RegNext(io.redirect)
//   for (i <- 0 until VecLoadPipelineWidth) {
//     flowRedirectCnt(i) := RegNext(PopCount(needFlush(i)))
//     when (lastRedirect.valid) {
//       enqPtr(i).value := enqPtr(i).value - flowRedirectCnt(i)
//     }.otherwise {
//       when (needAlloc(i)) {
//         enqPtr(i).value := enqPtr(i).value + realFlowNum(i)
//       }
//     }
//   }

//   /**
//     * unitStrideQueue enqPtr update */
//   for (i <- 0 until VecLoadPipelineWidth) {
//     UsRedirectCnt(i)   := RegNext(PopCount(UsNeedFlush(i)))
//     when (lastRedirect.valid) {
//       uSEnqPtr(i).value := uSEnqPtr(i).value - UsRedirectCnt(i)
//     }.otherwise {
//       when (uSAlloc(i)) {
//         uSEnqPtr(i).value := uSEnqPtr(i).value + 1.U
//       }
//     }
//   }

//   for (i <- 0 until VecLoadPipelineWidth) {
//     when (instType(i) === "b000".U) {
//       for (j <- 0 until VecLoadPipelineWidth) {
//         for (k <- 0 until UsQueueSize) {
//           cam(i)(j)(k) := unitStrideValid(j)(k) &&
//             io.loadRegIn(i).bits.uop.robIdx.value === unitStrideEntry(j)(k).robIdx.value
//           when (cam(0)(j)(k) && cam(1)(j)(k) && io.loadRegIn(0).valid && io.loadRegIn(1).valid) {
//             unitStrideEntry(j)(k).counter := unitStrideEntry(j)(k).counter - 2.U
//           }.elsewhen (cam(i)(j)(k) && io.loadRegIn(i).valid) {
//             unitStrideEntry(j)(k).counter := unitStrideEntry(j)(k).counter - 1.U
//           }
//         }
//       }
//     }
//   }

//   for (i <- 0 until VecLoadPipelineWidth) {
//     io.flowFeedback(i).valid := io.loadRegIn(i).valid
//     io.flowFeedback(i).bits := cam(i).asUInt.orR
//   }

//   val sameInst = loadRegInValid(0) && loadRegInValid(1) && instType(0) === "b000".U && instType(1) === "b000".U &&
//     (io.loadRegIn(0).bits.uop.robIdx.value === io.loadRegIn(1).bits.uop.robIdx.value)

//   for (i <- 0 until VecLoadPipelineWidth) {
//     when (instType(i) === "b000".U) {
//       when (sameInst) {
//         when (i.U === 0.U) {
//           uSAlloc(i) := !cam(i).asUInt.orR && loadRegInValid(0)
//         }.otherwise {
//           uSAlloc(i) := false.B
//         }
//       }.otherwise {
//         uSAlloc(i) := !cam(i).asUInt.orR && loadRegInValid(i)
//       }
//     }.otherwise {
//       uSAlloc(i) := false.B
//     }
//   }

//   //queue update
//   for (i <- 0 until VecLoadPipelineWidth) {
//     when (uSAlloc(i)) {
//       unitStrideValid(i)(uSEnqPtr(i).value) := true.B
//       unitStrideEntry(i)(uSEnqPtr(i).value).robIdx := io.loadRegIn(i).bits.uop.robIdx
//       when (sameInst) {
//         unitStrideEntry(0)(uSEnqPtr(0).value).counter := io.loadRegIn(0).bits.uop.ctrl.total_num - 1.U
//       }.otherwise {
//         unitStrideEntry(i)(uSEnqPtr(i).value).counter := io.loadRegIn(i).bits.uop.ctrl.total_num
//       }
//     }
//   }

//   //deqPtr update
//   for (i <- 0 until VecLoadPipelineWidth) {
//     when (unitStrideEntry(i)(uSDeqPtr(i).value).counter === 0.U && unitStrideValid(i)(uSDeqPtr(i).value)) {
//       uSDeqPtr(i).value := uSDeqPtr(i).value + 1.U
//       unitStrideValid(i)(uSDeqPtr(i).value) := false.B
//     }
//   }
// /**
//   * Redirection occurred, flush FlowQueue and unitStrideQueue*/
//   for (i <- 0 until VecLoadPipelineWidth) {
//     for (entry <- 0 until VlFlowSize) {
//       needFlush(i)(entry) := flow_entry(i)(entry).rob_idx(0).needFlush(io.redirect) && flow_entry_valid(i)(entry)
//       when (needFlush(i)(entry)) {
//         flow_entry_valid(i)(entry) := false.B
//         flow_entry(i)(entry).mask := 0.U
//         flow_entry(i)(entry).rob_idx_valid := VecInit(Seq.fill(2)(false.B))
//       }
//     }
//     for (entry <- 0 until UsQueueSize) {
//       UsNeedFlush(i)(entry) := unitStrideEntry(i)(uSEnqPtr(i).value).robIdx.needFlush(io.redirect) && unitStrideValid(i)(entry)
//       when (UsNeedFlush(i)(entry)) {
//         unitStrideValid(i)(entry) := false.B
//       }
//     }
//     loadRegInValid(i) := !io.loadRegIn(i).bits.uop.robIdx.needFlush(io.redirect) && io.loadRegIn(i).fire
//   }

//   for (i <- 0 until VecLoadPipelineWidth) {
//     when (instType(i) === "b000".U) { // unit-stride Inst
//       needAlloc(i) := uSAlloc(i)
//     }.otherwise {
//       needAlloc(i) := loadRegInValid(i)
//     }
//   }

//   for (i <- 0 until VecLoadPipelineWidth) {
//     //loadInstDec(i).apply(io.loadRegIn(i).bits.uop.cf.instr)
//     uopIdx(i)          := io.loadRegIn(i).bits.uop.ctrl.uopIdx
//     stride(i)          := io.loadRegIn(i).bits.src(1)
//     index(i)           := io.loadRegIn(i).bits.src(1)
//     eew(i)             := io.eew(i)
//     sew(i)             := io.sew(i)
//     emul(i)            := io.emul(i)
//     lmul(i)            := io.loadRegIn(i).bits.uop.ctrl.vconfig.vtype.vlmul
//     //mul(i)             := Mux(instType(i)(1,0) === "b00".U || instType(i)(1,0) === "b10".U,emul(i),lmul(i))
//     mul(i)             := Mux(instType(i)(1,0) === "b00".U || instType(i)(1,0) === "b10".U,emul(i), Mux(emulNum(i) > lmulNum(i),emul(i),lmul(i)))
//     emulNum(i)         := MulNum(emul(i))
//     lmulNum(i)         := MulNum(lmul(i))
//     vma(i)             := Mux(io.whole_reg(i), false.B, io.loadRegIn(i).bits.uop.ctrl.vconfig.vtype.vma)
//     vta(i)             := Mux(io.whole_reg(i), false.B, io.loadRegIn(i).bits.uop.ctrl.vconfig.vtype.vta)
//     vl(i)              := Mux(io.whole_reg(i), OneRegNum(eew(i)) * io.uop_segment_num(i), io.loadRegIn(i).bits.uop.ctrl.vconfig.vl)
//     vmask(i)           := io.loadRegIn(i).bits.src(3)
//     vstart(i)          := io.loadRegIn(i).bits.uop.ctrl.vconfig.vstart
//     instType(i)        := io.instType(i)
//     baseAddr(i)        := io.loadRegIn(i).bits.src(0)
//     dataWidth(i)       := io.loadRegIn(i).bits.uop.ctrl.vconfig.vl << eew(i)(1,0)// only unit-stride use
//     vend(i)            := baseAddr(i)(3,0) + dataWidth(i)
//     alignedType(i)     := DontCare
//     segMulIdx(i)      := GenSegMulIdx(mul = mul(i), uopIdx = uopIdx(i))
//     //segNfIdx(i)        := GenSegNfIdx(mul = mul(i),uopIdx = uopIdx(i))
//     segNfIdx(i)        := Mux((instType(i) === "b101".U || instType(i) === "b111".U) && emulNum(i) > lmulNum(i),
//                                 GenSegNfIdxMul(emul = emul(i), lmul = lmul(i), uopIdx = uopIdx(i)),
//                                 GenSegNfIdx(mul = mul(i), uopIdx = uopIdx(i)))
//   }

//   for (i <- 0 until VecLoadPipelineWidth) {
//     when (instType(i) === "b000".U) { // unit-stride Inst
//       realFlowNum(i)  := vend(i)(7,4) + (vend(i)(3,0) =/= 0.U).asUInt//TODO:************
//       cross128(i)     := baseAddr(i)(3, 0) =/= 0.U(4.W)
//     }.otherwise {
//       realFlowNum(i)  := io.realFlowNum(i)
//       cross128(i)     := false.B
//     }
//   }

//   /**
//    * Update FlowQueue status bits
//    * instType(i) === "b000".U means only unit-stride instructions use this logic*/
//   for (i <- 0 until VecLoadPipelineWidth) {
//     when (needAlloc(i)) {
//       //startRobIdx(i) := io.loadRegIn(i).bits.uop.robIdx.value - io.loadRegIn(i).bits.inner_idx
//       for (j <- 0 until 16) {
//         when (j.U < realFlowNum(i)) {
//           val queueIdx = Wire(UInt(5.W))
//           val vaddr = Wire(UInt(VAddrBits.W))
//           val uop = Wire(new MicroOp)
//           val vdFlowIdx  = Wire(UInt(4.W))
//           val vs2FlowIdx = Wire(UInt(4.W))
//           val eleIdx = Wire(UInt(7.W))
//           val eleIdxUnitStride = Wire(UInt(7.W))
//           val eleIdxNonUs = Wire(UInt(7.W))
//           queueIdx := enqPtr(i).value + j.U
//           flow_entry(i)(queueIdx) := DontCare
//           uop := DontCare
//           flow_entry_valid(i)(queueIdx) := true.B
//           //flow_entry(i)(queueIdx).unit_stride_fof := io.uop_unit_stride_fof(i)
//           flow_entry(i)(queueIdx).mask := GenVecLoadMask(instType = instType(i), emul = emul(i), eew = eew(i), sew = sew(i))
//           flow_entry(i)(queueIdx).vaddr := vaddr
//           flow_entry(i)(queueIdx).alignedType := alignedType(i)
//           eleIdxNonUs := GenEleIdx(instType = instType(i), emul = emul(i), lmul = lmul(i), eew = eew(i), sew = sew(i),
//             uopIdx = uopIdx(i), flowIdx = j.U)
//           eleIdxUnitStride := (VLEN.U >>  eew(i)(1,0)).asUInt * j.U + (vstart(i) >>  eew(i)(1,0)).asUInt
//           eleIdx := Mux(instType(i) === "b000".U, eleIdxUnitStride, eleIdxNonUs)
//           val exp = VLExpCtrl(vstart = vstart(i), vl = vl(i), eleIdx = eleIdx)
//           flow_entry(i)(queueIdx).exp := exp
//           flow_entry(i)(queueIdx).is_first_ele := (eleIdx === 0.U)
//           flow_entry(i)(queueIdx).flowIdx := eleIdx

//           vaddr := GenVLAddr(instType = instType(i), baseaddr = baseAddr(i), emul = emul(i), lmul = lmul(i), uopIdx = uopIdx(i),
//             flow_inner_idx = vs2FlowIdx, stride = stride(i), index = index(i), eew = eew(i), sew = sew(i),
//             nf = io.uop_segment_num(i) + 1.U, segNfIdx = segNfIdx(i), segMulIdx = segMulIdx(i))
//           flow_entry(i)(queueIdx).reg_offset(0) := VLRegOffset(instType = instType(i), flow_inner_idx = vdFlowIdx, eew = eew(i), sew = sew(i))


//           when (instType(i)(1,0) === "b00".U || instType(i)(1,0) === "b10".U) { // (segment)
//             vdFlowIdx  := j.U
//             vs2FlowIdx := j.U
//             alignedType(i) := eew(i)(1,0)
//           }.elsewhen (instType(i) === "b001".U || instType(i) === "b011".U) {
//             vdFlowIdx  := Mux(emulNum(i) > lmulNum(i), RegFLowCnt(emulNum=emulNum(i), lmulNum=lmulNum(i), eew=eew(i), uopIdx=uopIdx(i), flowIdx=j.U), j.U)
//             vs2FlowIdx := Mux(emulNum(i) > lmulNum(i), j.U, AddrFLowCnt(emulNum=emulNum(i), lmulNum=lmulNum(i), sew=sew(i), uopIdx=uopIdx(i), flowIdx=j.U))
//             alignedType(i) := sew(i)(1,0)
//           }.otherwise {
//             vdFlowIdx  := Mux(emulNum(i) > lmulNum(i), RegFLowCnt(emulNum = emulNum(i), lmulNum = lmulNum(i), eew = eew(i), uopIdx = segMulIdx(i), flowIdx = j.U), j.U)
//             vs2FlowIdx := Mux(emulNum(i) > lmulNum(i), j.U, AddrFLowCnt(emulNum = emulNum(i), lmulNum = lmulNum(i), sew = sew(i), uopIdx = segMulIdx(i), flowIdx = j.U))
//             alignedType(i) := sew(i)(1,0)
//           }

//           when (realFlowNum(i) === 1.U && instType(i) === "b000".U) {
//             flow_entry(i)(queueIdx).uop := io.loadRegIn(i).bits.uop
//             flow_entry(i)(queueIdx).rob_idx_valid(0) := true.B
//             flow_entry(i)(queueIdx).rob_idx(0)    := io.loadRegIn(i).bits.uop.robIdx
//             flow_entry(i)(queueIdx).inner_idx(0)  := Mux(instType(i) === "b000".U,j.U,uopIdx(i))
//             flow_entry(i)(queueIdx).offset(0)     := vaddr(3, 0)
//             //flow_entry(i)(queueIdx).reg_offset(0) := VLRegOffset(instType = instType(i), flow_inner_idx = j.U, eew = eew(i), sew = sew(i))
//           }.otherwise {
//             when (j.U =/= realFlowNum(i) - 1.U) {
//               uop := io.loadRegIn(i).bits.uop
//               uop.lqIdx := io.loadRegIn(i).bits.uop.lqIdx - uopIdx(i) + j.U
//               flow_entry(i)(queueIdx).uop := Mux(instType(i) === "b000".U,uop,io.loadRegIn(i).bits.uop)
//               flow_entry(i)(queueIdx).rob_idx_valid(0) := true.B
//               flow_entry(i)(queueIdx).rob_idx(0) := io.loadRegIn(i).bits.uop.robIdx
//               flow_entry(i)(queueIdx).inner_idx(0)  := Mux(instType(i) === "b000".U,j.U,uopIdx(i))
//               flow_entry(i)(queueIdx).offset(0) := vaddr(3, 0)
//               //flow_entry(i)(queueIdx).reg_offset(0) := VLRegOffset(instType = instType(i), flow_inner_idx = j.U, eew = eew(i), sew = sew(i))
//             }.elsewhen (j.U === realFlowNum(i) - 1.U && !cross128(i)) {
//               uop := io.loadRegIn(i).bits.uop
//               uop.lqIdx := io.loadRegIn(i).bits.uop.lqIdx - uopIdx(i) + j.U
//               flow_entry(i)(queueIdx).uop := Mux(instType(i) === "b000".U,uop,io.loadRegIn(i).bits.uop)
//               flow_entry(i)(queueIdx).rob_idx_valid(0) := true.B
//               flow_entry(i)(queueIdx).rob_idx(0) := io.loadRegIn(i).bits.uop.robIdx
//               flow_entry(i)(queueIdx).inner_idx(0)  := Mux(instType(i) === "b000".U,j.U,uopIdx(i))
//               flow_entry(i)(queueIdx).offset(0) := vaddr(3, 0)
//               //flow_entry(i)(queueIdx).reg_offset(0) := VLRegOffset(instType = instType(i), flow_inner_idx = j.U, eew = eew(i), sew = sew(i))
//             }.elsewhen (j.U === realFlowNum(i) - 1.U && cross128(i)) {
//               flow_entry(i)(queueIdx).rob_idx_valid(0) := false.B
//             }

//             when (j.U =/= 0.U && instType(i) === "b000".U) {
//               flow_entry(i)(queueIdx).rob_idx_valid(1) := cross128(i)
//               flow_entry(i)(queueIdx).rob_idx(1)       := io.loadRegIn(i).bits.uop.robIdx
//               flow_entry(i)(queueIdx).inner_idx(1)     := j.U - cross128(i).asUInt
//               flow_entry(i)(queueIdx).offset(1)        := 0.U
//               flow_entry(i)(queueIdx).reg_offset(1)    := 16.U - vaddr(3, 0)
//             }.otherwise {
//               flow_entry(i)(queueIdx).rob_idx_valid(1) := false.B
//             }
//           }
//         }
//       }
//     }
//   }

//   // flow deqPtr
//   for (i <- 0 until LoadPipelineWidth) {
//     // TODO: Need to do some changes
//     //  1. DontCare?
//     //  2. Other information?
//     io.loadPipeOut(i).bits := DontCarer
//     io.loadPipeOut(i).valid := false.B
//     when (flow_entry_valid(i)(deqPtr(i).value)){
//       io.loadPipeOut(i).valid                    := true.B
//       io.loadPipeOut(i).bits.uop_unit_stride_fof := flow_entry(i)(deqPtr(i).value).unit_stride_fof
//       io.loadPipeOut(i).bits.vaddr               := flow_entry(i)(deqPtr(i).value).vaddr
//       io.loadPipeOut(i).bits.mask                := flow_entry(i)(deqPtr(i).value).mask
//       io.loadPipeOut(i).bits.rob_idx_valid       := flow_entry(i)(deqPtr(i).value).rob_idx_valid
//       io.loadPipeOut(i).bits.rob_idx             := flow_entry(i)(deqPtr(i).value).rob_idx
//       io.loadPipeOut(i).bits.inner_idx           := flow_entry(i)(deqPtr(i).value).inner_idx
//       io.loadPipeOut(i).bits.offset              := flow_entry(i)(deqPtr(i).value).offset
//       io.loadPipeOut(i).bits.reg_offset          := flow_entry(i)(deqPtr(i).value).reg_offset
//       io.loadPipeOut(i).bits.uop.lqIdx           := flow_entry(i)(deqPtr(i).value).uop.lqIdx
//       io.loadPipeOut(i).bits.alignedType         := flow_entry(i)(deqPtr(i).value).alignedType
//       io.loadPipeOut(i).bits.exp                 := flow_entry(i)(deqPtr(i).value).exp
//       io.loadPipeOut(i).bits.is_first_ele        := flow_entry(i)(deqPtr(i).value).is_first_ele
//       io.loadPipeOut(i).bits.flowIdx            := flow_entry(i)(deqPtr(i).value).flowIdx
//     }
//   }

//   for (i <- 0 until LoadPipelineWidth) {
//     when (io.loadPipeOut(i).fire) {
//       flow_entry_valid(i)(deqPtr(i).value) := false.B
//       flow_entry(i)(deqPtr(i).value).mask := 0.U
//       flow_entry(i)(deqPtr(i).value).rob_idx_valid := VecInit(Seq.fill(2)(false.B))
//       deqPtr(i) := deqPtr(i) + 1.U
//     }
//   }

}
