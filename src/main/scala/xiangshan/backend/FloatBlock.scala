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

package xiangshan.backend

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.backend.regfile.Regfile
import xiangshan.backend.exu._
import xiangshan.backend.issue.ReservationStation
import xiangshan.mem.{HasFpLoadHelper, HasLoadHelper}
import difftest._

class FloatBlock()(implicit p: Parameters) extends XSModule with HasExeBlockHelper with HasFpLoadHelper {
  val io = IO(new Bundle {
    val redirect = Flipped(ValidIO(new Redirect))
    val flush = Input(Bool())
    // in
    val issue = Vec(exuParameters.FpExuCnt, Flipped(DecoupledIO(new ExuInput)))
    // out
    val writeback = Vec(exuParameters.FpExuCnt, DecoupledIO(new ExuOutput))
    // misc from csr
    val frm = Input(UInt(3.W))
  })

  val fmacExeUnits = Array.tabulate(exuParameters.FmacCnt)(_ => Module(new FmacExeUnit))
  val fmiscExeUnits = Array.tabulate(exuParameters.FmiscCnt)(_ => Module(new FmiscExeUnit))

  fmacExeUnits.foreach(_.frm := io.frm)
  fmiscExeUnits.foreach(_.frm := io.frm)

  val exeUnits = fmacExeUnits ++ fmiscExeUnits

  for ((exu, i) <- exeUnits.zipWithIndex) {
    exeUnits(i).io.redirect <> io.redirect
    exeUnits(i).io.flush <> io.flush

    // in
    exeUnits(i).io.fromFp <> io.issue(i)
    // fp instructions have three operands
    for (j <- 0 until 3) {
      // when one of the higher bits is zero, then it's not a legal single-precision number
      val isLegalSingle = io.issue(i).bits.uop.ctrl.fpu.typeTagIn === S && io.issue(i).bits.src(j)(63, 32).andR
      val single = recode(io.issue(i).bits.src(j)(31, 0), S)
      val double = recode(io.issue(i).bits.src(j)(63, 0), D)
      exeUnits(i).io.fromFp.bits.src(j) := Mux(isLegalSingle, single, double)
    }

    // out
    io.writeback(i).valid := exu.io.out.valid
    io.writeback(i).bits := exu.io.out.bits
    io.writeback(i).bits.data := Mux(exu.io.out.bits.uop.ctrl.fpWen,
      ieee(exu.io.out.bits.data),
      exu.io.out.bits.data
    )
    exu.io.out.ready := io.writeback(i).ready
  }
}
