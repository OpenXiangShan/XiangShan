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
      val block = Input(Bool())
    })

    val valid = RegInit(false.B)
    valid.suggestName("pipeline_valid")
    val leftFire = io.in.valid && io.out.ready && !io.block
    when (io.rightOutFire) { valid := false.B }
    when (leftFire) { valid := true.B }
    when (io.isFlush) { valid := false.B }

    io.in.ready := io.out.ready && !io.block
    io.out.bits := RegEnable(io.in.bits, leftFire)
    io.out.valid := valid //&& !isFlush
  }

  def apply[T <: Data]
  (left: DecoupledIO[T], right: DecoupledIO[T], rightOutFire: Bool, isFlush: Bool, block: Bool = false.B,
   moduleName: Option[String] = None
  ){
    val pipelineConnect = Module(new PipelineConnectModule[T](left.bits.cloneType))
    if(moduleName.nonEmpty) pipelineConnect.suggestName(moduleName.get)
    pipelineConnect.io.in <> left
    pipelineConnect.io.block := block
    pipelineConnect.io.rightOutFire := rightOutFire
    pipelineConnect.io.isFlush := isFlush
    right <> pipelineConnect.io.out
  }
}
