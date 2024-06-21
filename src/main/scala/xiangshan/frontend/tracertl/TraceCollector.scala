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

class TraceCollectBundle(implicit p: Parameters) extends TraceBundle {
  val pc = UInt(64.W)
  val inst = UInt(32.W)
  val instNum = UInt(8.W)
}

class TraceCollectorIO(implicit p: Parameters) extends TraceBundle {
  val enable = Input(Bool())
  val in = Input(Vec(CommitWidth, Valid(new TraceCollectBundle())))
}

class CommitBufferPtr(Size: Int)(implicit p: Parameters) extends CircularQueuePtr[CommitBufferPtr](Size)

class TraceCollector(implicit p: Parameters) extends TraceModule
  with TraceParams
  with HasCircularQueuePtrHelper {
  val io = IO(new TraceCollectorIO)

  val CommitBufferSize = CommitWidth * 3
  val commitBuffer = Reg(Vec(CommitBufferSize, new TraceCollectBundle()))
  val traceCollectorHelper = Module(new TraceCollectorHelper(CommitWidth))
  val deqPtr = RegInit(0.U.asTypeOf(new CommitBufferPtr(CommitBufferSize)))
  val enqPtr = RegInit(0.U.asTypeOf(new CommitBufferPtr(CommitBufferSize)))

  XSError(isFull(enqPtr, deqPtr), "Should not be full")

  val deqReady = distanceBetween(enqPtr, deqPtr) >= CommitWidth.U

  traceCollectorHelper.clock := clock
  traceCollectorHelper.reset := reset
  traceCollectorHelper.enable := deqReady
  for (i <- 0 until CommitWidth) {
    traceCollectorHelper.pc(i) := commitBuffer(deqPtr.value + i.U).pc
    traceCollectorHelper.inst(i) := commitBuffer(deqPtr.value + i.U).inst
    traceCollectorHelper.instNum(i) := commitBuffer(deqPtr.value + i.U).instNum
  }
  when (deqReady) {
    deqPtr := deqPtr + CommitWidth.U
  }

  for (i <- 0 until CommitWidth) {
    when (io.in(i).valid && io.enable) {
      commitBuffer(enqPtr.value + i.U) := io.in(i).bits
    }
  }
  when (io.enable) {
    enqPtr := enqPtr + PopCount(io.in.map(_.valid))
  }
}