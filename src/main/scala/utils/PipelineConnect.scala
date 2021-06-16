/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
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

package utils

import chisel3._
import chisel3.util._

object PipelineConnect {

  class PipelineConnectModule[T <: Data](gen: T) extends Module {
    val io = IO(new Bundle() {
      val in = Flipped(DecoupledIO(gen.cloneType))
      val out = DecoupledIO(gen.cloneType)
      val rightOutFire = Input(Bool())
      val isFlush = Input(Bool())
    })

    val valid = RegInit(false.B)
    valid.suggestName("pipeline_valid")
    when (io.rightOutFire) { valid := false.B }
    when (io.in.valid && io.out.ready) { valid := true.B }
    when (io.isFlush) { valid := false.B }

    io.in.ready := io.out.ready
    io.out.bits := RegEnable(io.in.bits, io.in.valid && io.out.ready)
    io.out.valid := valid //&& !isFlush
  }

  def apply[T <: Data]
  (left: DecoupledIO[T], right: DecoupledIO[T], rightOutFire: Bool, isFlush: Bool,
   moduleName: Option[String] = None
  ){
    val pipelineConnect = Module(new PipelineConnectModule[T](left.bits.cloneType))
    if(moduleName.nonEmpty) pipelineConnect.suggestName(moduleName.get)
    pipelineConnect.io.in <> left
    pipelineConnect.io.rightOutFire := rightOutFire
    pipelineConnect.io.isFlush := isFlush
    right <> pipelineConnect.io.out
  }
}
