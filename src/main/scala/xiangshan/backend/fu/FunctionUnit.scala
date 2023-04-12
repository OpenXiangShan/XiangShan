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

package xiangshan.backend.fu

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils.XSPerfAccumulate
import xiangshan._
import xiangshan.backend.fu.fpu._

trait HasFuLatency {
  val latencyVal: Option[Int]
}

case class CertainLatency(value: Int) extends HasFuLatency {
  override val latencyVal: Option[Int] = Some(value)
}

case class UncertainLatency() extends HasFuLatency {
  override val latencyVal: Option[Int] = None
}

class FuOutput(val len: Int)(implicit p: Parameters) extends XSBundle {
  val data = UInt(len.W)
  val uop = new MicroOp
}

class FunctionUnitInput(val len: Int)(implicit p: Parameters) extends XSBundle {
  val src = Vec(3, UInt(len.W))
  val uop = new MicroOp
}

class FunctionUnitIO(val len: Int)(implicit p: Parameters) extends XSBundle {
  val in = Flipped(DecoupledIO(new FunctionUnitInput(len)))

  val out = DecoupledIO(new FuOutput(len))

  val redirectIn = Flipped(ValidIO(new Redirect))
}

abstract class FunctionUnit(len: Int = 64)(implicit p: Parameters) extends XSModule {

  val io = IO(new FunctionUnitIO(len))

  XSPerfAccumulate("in_valid", io.in.valid)
  XSPerfAccumulate("in_fire", io.in.fire)
  XSPerfAccumulate("out_valid", io.out.valid)
  XSPerfAccumulate("out_fire", io.out.fire)

}
