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

package xiangshan.backend.exu

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import xiangshan._
import xiangshan.backend.fu.fpu._

class FmiscExeUnit(implicit p: Parameters) extends Exu(FmiscExeUnitCfg) {

  val frm = IO(Input(UInt(3.W)))

  val fus = supportedFunctionUnits.map(fu => fu.asInstanceOf[FPUSubModule])

  val input = io.fromFp
  val isRVF = input.bits.uop.ctrl.isRVF
  val instr_rm = input.bits.uop.ctrl.fpu.rm
  val (src1, src2) = (input.bits.src(0), input.bits.src(1))

  supportedFunctionUnits.foreach { module =>
    module.io.in.bits.src(0) := src1
    module.io.in.bits.src(1) := src2
    module.asInstanceOf[FPUSubModule].rm := Mux(instr_rm =/= 7.U, instr_rm, frm)
  }

  io.out.bits.fflags := MuxCase(
    0.U,
    fus.map(x => x.io.out.fire() -> x.fflags)
  )
  val fpOutCtrl = io.out.bits.uop.ctrl.fpu
  io.out.bits.data := box(arb.io.out.bits.data, fpOutCtrl.typeTagOut)
}
