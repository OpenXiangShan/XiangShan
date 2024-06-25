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

class TraceDriveCollectBundle(implicit p: Parameters) extends TraceBundle {
  val pc = UInt(64.W)
  val inst = UInt(32.W)
}

class TraceDriveCollectorIO(implicit p: Parameters) extends TraceBundle {
  val in = Input(Vec(DecodeWidth, Valid(new TraceDriveCollectBundle())))
}

class TraceDrivePtr(Size: Int)(implicit p: Parameters) extends CircularQueuePtr[TraceDrivePtr](Size)

class TraceDriveCollector(implicit p: Parameters) extends TraceModule
  with TraceParams
  with HasCircularQueuePtrHelper {
  val io = IO(new TraceDriveCollectorIO)

  val TraceDriveSize = DecodeWidth * 3
  val traceDrive = Reg(Vec(TraceDriveSize, new TraceDriveCollectBundle()))
  val traceDriveHelper = Module(new TraceDriveCollectorHelper(DecodeWidth))
  val deqPtr = RegInit(0.U.asTypeOf(new TraceDrivePtr(TraceDriveSize)))
  val enqPtr = RegInit(0.U.asTypeOf(new TraceDrivePtr(TraceDriveSize)))

  XSError(isFull(enqPtr, deqPtr), "TraceDriveCollector Should not be full")

  val deqReady = distanceBetween(enqPtr, deqPtr) >= DecodeWidth.U

  traceDriveHelper.clock := clock
  traceDriveHelper.reset := reset
  traceDriveHelper.enable := deqReady
  for (i <- 0 until DecodeWidth) {
    val ptr = (deqPtr + i.U).value
    traceDriveHelper.pc(i) := traceDrive(ptr).pc
    traceDriveHelper.inst(i) := traceDrive(ptr).inst
  }
  when (deqReady) {
    deqPtr := deqPtr + DecodeWidth.U
  }

  for (i <- 0 until DecodeWidth) {
    when (io.in(i).valid) {
      val sum = PopCount(io.in.map(_.valid).take(i))
      val ptr = (enqPtr + sum).value
      traceDrive(ptr) := io.in(i).bits
    }
  }
  enqPtr := enqPtr + PopCount(io.in.map(_.valid))
}
