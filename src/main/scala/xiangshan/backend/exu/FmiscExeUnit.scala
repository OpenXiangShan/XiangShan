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

package xiangshan.backend.exu

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import freechips.rocketchip.tile.FType
import xiangshan._
import xiangshan.backend.fu.fpu._

class FmiscExeUnit(implicit p: Parameters) extends ExeUnit(FmiscExeUnitCfg) {

  val fus = functionUnits.map(fu => fu.asInstanceOf[FPUSubModule])

  fus.foreach { module =>
    val instr_rm = module.io.in.bits.uop.ctrl.fpu.rm
    module.rm := Mux(instr_rm =/= 7.U, instr_rm, frm.get)
  }

  require(config.hasFastUopOut)
  io.out.bits.fflags := Mux1H(arbSelReg, fus.map(x => x.fflags))
  io.out.bits.data := dataReg
}
