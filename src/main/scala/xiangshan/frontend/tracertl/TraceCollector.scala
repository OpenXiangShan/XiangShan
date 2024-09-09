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
import utility.{CircularQueuePtr, HasCircularQueuePtrHelper, XSError}

class TraceCollectBundle(implicit p: Parameters) extends TraceBundle {
  val pc = UInt(64.W)
  val inst = UInt(32.W)
  val instNum = UInt(8.W)
}

class TraceCollectorIO(implicit p: Parameters) extends TraceBundle {
  val enable = Input(Bool())
  val in = Input(Vec(CommitWidth, Valid(new TraceCollectBundle())))
  val traceInfo = Input(Vec(CommitWidth, new TraceInstrBundle()))
}

class CommitBufferPtr(Size: Int)(implicit p: Parameters) extends CircularQueuePtr[CommitBufferPtr](Size)

class TraceCollector(implicit p: Parameters) extends TraceModule
  with TraceParams
  with HasCircularQueuePtrHelper {
  val io = IO(new TraceCollectorIO)
  dontTouch(io)

  val traceCollectorHelper = Module(new TraceCollectorHelper(CommitWidth))

  traceCollectorHelper.clock := clock
  traceCollectorHelper.reset := reset
  for (i <- 0 until CommitWidth) {
    traceCollectorHelper.enable(i) := io.enable && io.in(i).valid
    traceCollectorHelper.pc(i) := io.in(i).bits.pc
    traceCollectorHelper.inst(i) := io.in(i).bits.inst
    traceCollectorHelper.instNum(i) := io.in(i).bits.instNum
  }
}