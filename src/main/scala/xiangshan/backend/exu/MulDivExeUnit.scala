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

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import utility._
import xiangshan._
import xiangshan.backend.fu._

class MulDivExeUnit(implicit p: Parameters) extends ExeUnit(MulDivExeUnitCfg) {

  val func = io.fromInt.bits.uop.ctrl.fuOpType
  val (src1, src2) = (
    io.fromInt.bits.src(0)(XLEN - 1, 0),
    io.fromInt.bits.src(1)(XLEN - 1, 0)
  )

  val mul = functionUnits.collectFirst {
    case m: ArrayMultiplier => m
  }.get

  val div = functionUnits.collectFirst {
    case d: AbstractDivider => d
  }.orNull

  // override inputs
  val op = MDUOpType.getMulOp(func)
  val signext = SignExt(_: UInt, XLEN + 1)
  val zeroext = ZeroExt(_: UInt, XLEN + 1)
  val mulInputFuncTable = List(
    MDUOpType.mul -> (zeroext, zeroext),
    MDUOpType.mulh -> (signext, signext),
    MDUOpType.mulhsu -> (signext, zeroext),
    MDUOpType.mulhu -> (zeroext, zeroext)
  )

  mul.io.in.bits.src(0) := LookupTree(
    op,
    mulInputFuncTable.map(p => (p._1(1, 0), p._2._1(src1)))
  )
  when (func(3)) {
    mul.io.in.bits.src(0) := src1(6, 0)
  }
  mul.io.in.bits.src(1) := LookupTree(
    op,
    mulInputFuncTable.map(p => (p._1(1, 0), p._2._2(src2)))
  )

  val isW = MDUOpType.isW(func)
  val isH = MDUOpType.isH(func)
  mul.ctrl.isW := isW
  mul.ctrl.isHi := isH
  mul.ctrl.sign := DontCare

  XSDebug(io.fromInt.valid, "In(%d %d) Out(%d %d) Redirect:(%d %d)\n",
    io.fromInt.valid, io.fromInt.ready,
    io.out.valid, io.out.ready,
    io.redirect.valid,
    io.redirect.bits.level
  )
  XSDebug(io.fromInt.valid, "src1:%x src2:%x pc:%x\n", src1, src2, io.fromInt.bits.uop.cf.pc)
  XSDebug(io.out.valid, "Out(%d %d) res:%x pc:%x\n",
    io.out.valid, io.out.ready, io.out.bits.data, io.out.bits.uop.cf.pc
  )
}
