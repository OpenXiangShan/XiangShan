/** *************************************************************************************
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
 * ************************************************************************************* */
package xiangshan.frontend.tracertl

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.{CircularQueuePtr, HasCircularQueuePtrHelper}
import utils.XSError
import xiangshan.frontend.BranchPredictionRedirect
import xiangshan.RedirectLevel

class TracePredInfoBundle extends Bundle {
  val fixTarget = UInt(64.W)
  val taken = Bool()
}

class TraceInstrBundle(implicit p: Parameters) extends TraceBundle {
  val InstID = UInt(64.W)
  val pcVA = UInt(TracePCWidth.W)
  val pcPA = UInt(TracePCWidth.W)
  val memoryAddrVA = UInt(64.W)
  val memoryAddrPA = UInt(64.W)
  val target = UInt(64.W)
  val inst = UInt(TraceInstrWidth.W)
  val memoryType = UInt(8.W)
  val memorySize = UInt(8.W)
  val branchType = UInt(8.W)
  val branchTaken = UInt(8.W)

  // use for cfi fix
  // we need to know the bpu result to know if the branch should redirect or not
//  val bpuPredInfo = new TracePredInfoBundle
}

class TraceReaderIO(implicit p: Parameters) extends TraceBundle {
  // recv.valid from f3_fire, bits.instNum from range
  val recv = Flipped(Valid(new TraceRecvInfo()))
  // BranchPredictionRedirect === Redirect with some traits
  val redirect = Flipped(Valid(new BranchPredictionRedirect()))

  // traceInst should always be valid
  val traceInsts = Output(Vec(PredictWidth, new TraceInstrBundle()))
}

class TraceBufferPtr(Size: Int)(implicit p: Parameters) extends CircularQueuePtr[TraceBufferPtr](Size)

class TraceReader(implicit p: Parameters) extends TraceModule
  with TraceParams
  with HasCircularQueuePtrHelper {
  val io = IO(new TraceReaderIO())
  dontTouch(io)

  val traceBuffer = Reg(Vec(TraceBufferSize, new TraceInstrBundle()))
  val traceReaderHelper = Module(new TraceReaderHelper(PredictWidth))
  val traceRedirecter = Module(new TraceRedirectHelper)
  val deqPtr = RegInit(0.U.asTypeOf(new TraceBufferPtr(TraceBufferSize)))
  val enqPtr = RegInit(0.U.asTypeOf(new TraceBufferPtr(TraceBufferSize)))

  if (!TraceEnableDuplicateFlush) {
    when (io.redirect.valid) {
      XSError(io.redirect.bits.traceInfo.InstID + 1.U =/= traceBuffer(deqPtr.value).InstID,
        "TraceEnableDuplicateFlush is false. Please check wrong path redirect")
    }
  }


  XSError(!isFull(enqPtr, deqPtr) && (enqPtr < deqPtr), "enqPtr should always be larger than deqPtr")
  XSError(io.recv.valid && ((deqPtr + io.recv.bits.instNum) >= enqPtr),
    "Reader should not read more than what is in the buffer. Error in ReaderHelper or Ptr logic.")

  when(io.recv.valid) {
    deqPtr := deqPtr + io.recv.bits.instNum
  }

  val isfull = isFull(enqPtr, deqPtr)
  val freeEntryNum = hasFreeEntries(enqPtr, deqPtr)
  val readTraceEnable = !isfull && (freeEntryNum >= TraceFetchWidth.U)
  when(readTraceEnable) {
    enqPtr := enqPtr + TraceFetchWidth.U

    (0 until TraceFetchWidth).foreach {
      i => bufferInsert(enqPtr + i.U, traceReaderHelper.insts(i))
    }
  }

  def bufferInsert(ptr: TraceBufferPtr, data: TraceInstrInnerBundle) = {
    (traceBuffer(ptr.value): Data).waiveAll :<= (data: Data).waiveAll
    when (data.memoryAddrPA === 0.U) {
      traceBuffer(ptr.value).memoryAddrPA := data.memoryAddrVA
    }
    when (data.pcPA === 0.U) {
      traceBuffer(ptr.value).pcPA := data.pcVA
    }
  }

  traceRedirecter.clock := clock
  traceRedirecter.reset := reset
  traceRedirecter.enable := io.redirect.valid
  traceRedirecter.InstID := io.redirect.bits.traceInfo.InstID +
    Mux(RedirectLevel.flushItself(io.redirect.bits.level), 0.U, 1.U)

  traceReaderHelper.clock := clock
  traceReaderHelper.reset := reset
  traceReaderHelper.enable := readTraceEnable && !io.redirect.valid

  io.traceInsts.zipWithIndex.foreach { case (inst, i) =>
    val ptr = (deqPtr + i.U).value
    inst := traceBuffer(ptr)
  }

  when (io.redirect.valid) {
    enqPtr := 0.U.asTypeOf(new TraceBufferPtr(TraceBufferSize))
    deqPtr := 0.U.asTypeOf(new TraceBufferPtr(TraceBufferSize))
  }

}
