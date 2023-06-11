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

package xiangshan.backend.datapath

import chisel3._
import chisel3.util._

class NewPipelineConnectPipe[T <: Data](gen: T) extends Module {
  val io = IO(new Bundle() {
    val in = Flipped(DecoupledIO(gen.cloneType))
    val out = DecoupledIO(gen.cloneType)
    val rightOutFire = Input(Bool())
    val isFlush = Input(Bool())
  })

  NewPipelineConnect.connect(io.in, io.out, io.rightOutFire, io.isFlush)
}

object NewPipelineConnect {
  def connect[T <: Data](
                          left: DecoupledIO[T],
                          right: DecoupledIO[T],
                          rightOutFire: Bool,
                          isFlush: Bool
                        ): T = {
    val valid = RegInit(false.B)

    left.ready := right.ready || !valid
    val leftFire = left.valid && left.ready
    val data = RegEnable(left.bits, leftFire)

    when (rightOutFire) { valid := false.B }
    when (leftFire) { valid := true.B }
    when (isFlush) { valid := false.B }

    right.bits := data
    right.valid := valid

    data
  }

  def apply[T <: Data](
                        left: DecoupledIO[T],
                        right: DecoupledIO[T],
                        rightOutFire: Bool,
                        isFlush: Bool,
                        moduleName: Option[String] = None
                      ): Option[T] = {
    if (moduleName.isDefined) {
      val pipeline = Module(new NewPipelineConnectPipe(left.bits))
      pipeline.suggestName(moduleName.get)
      pipeline.io.in <> left
      pipeline.io.rightOutFire := rightOutFire
      pipeline.io.isFlush := isFlush
      pipeline.io.out <> right
      pipeline.io.out.ready := right.ready
      None
    }
    else {
      // do not use module here to please DCE
      Some(connect(left, right, rightOutFire, isFlush))
    }
  }
}