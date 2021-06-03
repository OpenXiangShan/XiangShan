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

package xiangshan.testutils

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import chiseltest._
import xiangshan._
import xiangshan.backend._

object TestCaseGenerator {


  /*
      Generate MUL/DIV Input
   */

  def genMul(x: => ExuInput, pc: Long): ExuInput = {
    chiselTypeOf(x).Lit(
      _.uop.ctrl.fuOpType -> MDUOpType.mulw,
      _.uop.cf.pc -> pc.U
    )
  }

  def genDiv(x: => ExuInput, pc: Long): ExuInput = {
    chiselTypeOf(x).Lit(
      _.uop.ctrl.fuOpType -> MDUOpType.div,
      _.uop.cf.pc -> pc.U
    )
  }


  /*
      Generate ALU Input
   */

  def genAluInput(fuOpType: UInt)(x: => ExuInput, src1: Long, src2: Long, imm: Long): ExuInput = {
    chiselTypeOf(x).Lit(
      _.src(0) -> src1.U,
      _.src(1) -> src2.U,
      _.uop.ctrl.imm -> imm.U,
      _.uop.ctrl.fuOpType -> fuOpType
    )
  }

  def genAluAdd(x: => ExuInput, src1: Long, src2: Long) =
    genAluInput(ALUOpType.add)(x, src1, src2, imm = 0)



  /*
      Generate LSU Input (ExuInput)
   */
  def genLsuInput(fuOpType: UInt)(x: => ExuInput, base: Long, offset: Long, stData: Long): ExuInput ={
    chiselTypeOf(x).Lit(
      _.src(0) -> base.U,
      _.src(1) -> stData.U,
      _.uop.ctrl.imm -> offset.U,
      _.uop.ctrl.fuOpType -> fuOpType
    )
  }

  def genLsuLd(x: => ExuInput, base: Long, offset: Long) =
    genLsuInput(LSUOpType.ld)(x, base, offset, 0)

  def genLsuLw(x: => ExuInput, base: Long, offset: Long) =
    genLsuInput(LSUOpType.lw)(x, base, offset, 0)

  def genLsuSd(x: => ExuInput, base: Long, offset: Long, stData: Long) =
    genLsuInput(LSUOpType.sd)(x, base, offset, stData)

  def genLsuSw(x: => ExuInput, base: Long, offset: Long, stData: Long) =
    genLsuInput(LSUOpType.sw)(x, base, offset, stData)

}
