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

  val traceDriveHelper = Module(new TraceDriveCollectorHelper(DecodeWidth))

  traceDriveHelper.clock := clock
  traceDriveHelper.reset := reset
  for (i <- 0 until DecodeWidth) {
    traceDriveHelper.enable(i) := io.in(i).valid
    traceDriveHelper.pc(i) := io.in(i).bits.pc
    traceDriveHelper.inst(i) := io.in(i).bits.inst
  }
}
