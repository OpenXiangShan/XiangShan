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

class TraceReaderIO(implicit p: Parameters) extends TraceBundle {
  // traceInst should always be valid
  val traceInsts = Output(Vec(PredictWidth, new TraceInstrBundle()))
  // recv.valid from f3_fire, bits.instNum from range
  val recv = Flipped(Valid(new TraceRecvInfo()))
}

class TraceBufferPtr(Size: Int)(implicit p: Parameters) extends CircularQueuePtr[TraceBufferPtr](Size)

class TraceReader(implicit p: Parameters) extends TraceModule
  with TraceParams
  with HasCircularQueuePtrHelper {
  val io = IO(new TraceReaderIO())
  dontTouch(io)

  val traceBuffer = Reg(Vec(TraceBufferSize, new TraceInstrBundle()))
  val traceReaderHelper = Module(new TraceReaderHelper(PredictWidth))
  val deqPtr = RegInit(0.U.asTypeOf(new TraceBufferPtr(TraceBufferSize)))
  val enqPtr = RegInit(0.U.asTypeOf(new TraceBufferPtr(TraceBufferSize)))

  XSError(!isFull(enqPtr, deqPtr) && (enqPtr < deqPtr), "enqPtr should always be larger than deqPtr")
  XSError(io.recv.valid && ((deqPtr.value + io.recv.bits.instNum) >= enqPtr.value),
    "Reader should not read more than what is in the buffer. Error in ReaderHelper or Ptr logic.")

  when(io.recv.valid) {
    deqPtr := deqPtr + io.recv.bits.instNum
  }

  val readTraceEnable = !isFull(enqPtr, deqPtr) && (hasFreeEntries(enqPtr, deqPtr) >= TraceFetchWidth.U)
  when(readTraceEnable) {
    enqPtr := enqPtr + TraceFetchWidth.U
    (0 until TraceFetchWidth).foreach {
      i => bufferInsert(enqPtr + i.U, traceReaderHelper.insts(i))
    }
  }

  def bufferInsert(ptr: TraceBufferPtr, data: TraceInstrBundle) = {
    traceBuffer(ptr.value) := data
    // traceBuffer(ptr.value) := 0.U
  }

  traceReaderHelper.clock := clock
  traceReaderHelper.reset := reset
  traceReaderHelper.enable := readTraceEnable
  io.traceInsts.zipWithIndex.foreach { case (inst, i) =>
    inst := traceBuffer(deqPtr.value + i.U)
  }
}
