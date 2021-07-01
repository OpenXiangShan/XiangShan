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
import xiangshan._
import xiangshan.backend.fu.fpu._

class FmacExeUnit(implicit p: Parameters) extends Exu(FmacExeUnitCfg)
{
  val frm = IO(Input(UInt(3.W)))

  val fma = supportedFunctionUnits.head.asInstanceOf[FMA]

  val input = io.fromFp.bits
  val fmaOut = fma.io.out.bits
  val isRVD = !io.fromFp.bits.uop.ctrl.isRVF
  fma.io.in.bits.src := VecInit(Seq(input.src(0), input.src(1), input.src(2)))
  val instr_rm = io.fromFp.bits.uop.ctrl.fpu.rm
  fma.rm := Mux(instr_rm =/= 7.U, instr_rm, frm)

  fma.io.redirectIn := io.redirect
  fma.io.flushIn := io.flush
  fma.io.out.ready := io.out.ready

  io.out.bits.data := box(fma.io.out.bits.data, fma.io.out.bits.uop.ctrl.fpu.typeTagOut)
  io.out.bits.fflags := fma.fflags
}
