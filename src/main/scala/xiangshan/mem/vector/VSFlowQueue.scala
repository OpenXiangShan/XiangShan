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

object VSRegOffset {
  def apply (instType: UInt, flowIdx: UInt, eew: UInt, sew: UInt):UInt = {
    (LookupTree(instType(1,0),List(
      "b00".U -> (flowIdx << eew(1,0)).asUInt, // (segment) unit-stride(don't use),
      "b10".U -> (flowIdx << eew(1,0)).asUInt, // (segment) strided
      "b01".U -> (flowIdx << sew(1,0)).asUInt, // (segment) indexed-unordered
      "b11".U -> (flowIdx << sew(1,0)).asUInt, // (segment) indexed-ordered
    )))}
}

/**
  * (1) unit-stride instructions access to memory continously, so calculate the address by adding 16 directly (flow_inner_idx << 4.U)
  * (2) stride instructions: flow_inner_idx means the current number of UOP memory accesses,
  *     uopIdx << Log2Num(GenRealFlowNum(instType,emul,eew,sew)) means the number of all previous UOP memory accesses
  * (3) index instructions: According to flow_ inner_idx obtains immediate value from index, than Calculate address
  * (4) segment instructions: To calculate the address, segment instructions need calculate segEmulIdx and segNfIdx;
  * */
object GenVSAddr {
  def apply (instType: UInt, baseAddr: UInt, eleIdx: UInt, emul:UInt, lmul: UInt, uopIdx:UInt, flow_inner_idx: UInt, stride: UInt,
             index: UInt, eew: UInt, sew: UInt, nf:UInt, segNfIdx: UInt, segMulIdx: UInt): UInt = {
    (LookupTree(instType,List(
      "b000".U -> (baseAddr + (eleIdx << eew(1,0)).asUInt).asUInt,// unit-stride
      "b010".U -> (baseAddr + stride * eleIdx),// strided
      "b001".U -> (baseAddr + IndexAddr(index= index, flow_inner_idx = flow_inner_idx, eew = eew)), // indexed-unordered
      "b011".U -> (baseAddr + IndexAddr(index= index, flow_inner_idx = flow_inner_idx, eew = eew)), // indexed-ordered
      "b100".U -> (baseAddr +
        (((flow_inner_idx + (segMulIdx << Log2Num(GenRealFlowNum(instType,emul,lmul,eew,sew))).asUInt).asUInt * nf) << eew(1,0)).asUInt +
        (segNfIdx << eew(1,0)).asUInt),// segment unit-stride
      "b110".U -> (baseAddr +
        (flow_inner_idx + (segMulIdx << Log2Num(GenRealFlowNum(instType,emul,lmul,eew,sew))).asUInt).asUInt * stride +
        (segNfIdx << eew(1,0)).asUInt), // segment strided
      "b101".U -> (baseAddr + IndexAddr(index= index, flow_inner_idx = flow_inner_idx, eew = eew) + (segNfIdx << sew(1,0)).asUInt), // segment indexed-unordered
      "b111".U -> (baseAddr + IndexAddr(index= index, flow_inner_idx = flow_inner_idx, eew = eew) + (segNfIdx << sew(1,0)).asUInt)  // segment indexed-ordered
    )))}
}

object VSMaskCtrl {
  def apply (vstart: UInt, vl: UInt, eleIdx: UInt, vmask: UInt, mask: UInt, vma: Bool, vta: Bool) :(UInt,Bool) = {
    val vsMask = Wire(UInt(16.W))
    val exp = Wire(Bool())
    when (vstart >= vl || vl === 0.U) {
      vsMask := 0.U
      exp := false.B
    }.otherwise {
      when (eleIdx >= vstart && eleIdx < vl) {
        exp := true.B
        when(vmask === false.B && vma === false.B) {
          vsMask := 0.U
        }.otherwise {
          vsMask := mask
        }
      }.elsewhen(eleIdx >= vl) {
        exp := false.B
        when(vta === false.B) {
          vsMask := 0.U
        }.otherwise {
          vsMask := "hff".U
        }
      }.otherwise{
        vsMask := 0.U
        exp := false.B
      }
    }
    (vsMask,exp)
  }
}

object GenVSMask {
  def apply(reg_offset: UInt, offset: UInt, mask: UInt):UInt = {
    val vMask = Wire(UInt(16.W))
    when (offset <= reg_offset) {
      vMask := mask >> (reg_offset - offset)
    }.otherwise {
      vMask := mask << (offset - reg_offset)
    }
    vMask
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

class VSFQFeedback (implicit p: Parameters) extends XSBundle {
  // val fqIdx = UInt(log2Up(VsFlowSize).W)
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
  // val fqIdx               = UInt(log2Ceil(VsFlowSize).W)
  val flowPtr             = new VsFlowPtr
}

class VsFlowBundle(implicit p: Parameters) extends VecFlowBundle {
  val data = UInt(VLEN.W)
  val uopQueuePtr = new VsUopPtr
  val isLastElem = Bool()
}

class VecStoreFlowEntry (implicit p: Parameters) extends VecFlowBundle {
  val uopQueuePtr = new VsUopPtr
  val paddr = UInt(PAddrBits.W)
  val isLastElem = Bool()

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
    // result.fqIdx                := thisPtr.value
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

class VsFlowQueue(implicit p: Parameters) extends XSModule with HasCircularQueuePtrHelper
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
  // dequeue pointers, deqPtr(0) is the exact one
  val deqPtr = RegInit(VecInit((0 until EnsbufferWidth).map(_.U.asTypeOf(new VsFlowPtr))))

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
  val freeCount = hasFreeEntries(enqPtr(0), deqPtr(0))
  val allowEnqueue = !io.redirect.valid && freeCount >= VecStorePipelineWidth.U
  for (i <- 0 until VecStorePipelineWidth) {
    io.flowIn(i).ready := allowEnqueue
  }

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
        // ! This is so inelegant
        x.vaddr := thisFlowIn.vaddr
        x.mask := thisFlowIn.mask
        x.alignedType := thisFlowIn.alignedType
        x.exp := thisFlowIn.exp
        x.flow_idx := thisFlowIn.flow_idx
        x.is_first_ele := thisFlowIn.is_first_ele
        x.uop := thisFlowIn.uop
        x.isLastElem := thisFlowIn.isLastElem
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
      x.exp_ele_index := thisEntry.flow_idx       // ?
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

  /* Dequeue */   
  val canDequeue = Wire(Vec(EnsbufferWidth, Bool()))
  val allowDequeue = io.sbuffer.map(_.ready)
  val doDequeue = Wire(Vec(EnsbufferWidth, Bool()))
  val dequeueCount = PopCount(doDequeue)

  // handshake
  for (i <- 0 until EnsbufferWidth) {
    val thisPtr = deqPtr(i).value
    val exp = flowQueueEntries(thisPtr).exp
    if (i == 0) {
      canDequeue(i) := flowCommitted(thisPtr)
    } else {
      canDequeue(i) := flowCommitted(thisPtr) && doDequeue(i - 1)
    }
    io.sbuffer(i).valid := canDequeue(i) && exp
    doDequeue(i) := canDequeue(i) && (allowDequeue(i) || !exp)
    when (doDequeue(i)) {
      flowAllocated(thisPtr) := false.B
      flowFinished(thisPtr) := false.B
      flowCommitted(thisPtr) := false.B
    }
  }

  // update DequeuePtr
  for (i <- 0 until EnsbufferWidth) {
    deqPtr(i) := deqPtr(i) + dequeueCount
  }

  for (i <- 0 until EnsbufferWidth) {
    val thisPtr = deqPtr(i).value
    val thisEntry = flowQueueEntries(thisPtr)

    val thisData = Wire(UInt(VLEN.W))
    // ! Need some movement in VLEN?
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
      x.mask  := thisEntry.mask
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

  for (i <- 0 until EnsbufferWidth) {
    when (doDequeue(i) && flowQueueEntries(deqPtr(i).value).isLastElem) {
      io.sqRelease.valid := true.B
    }
    io.sqRelease.bits := flowQueueEntries(deqPtr(i).value).uop.sqIdx
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