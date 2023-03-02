/****************************************************************************************
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
 ****************************************************************************************
 */


package xiangshan.backend.fu.vector

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import utility._
import yunsuan.vector.VectorIntAdder
import yunsuan.{VipuType, VectorElementFormat}
import xiangshan.{SrcType, SelImm}
import xiangshan.backend.fu.FunctionUnit
import xiangshan.XSCoreParamsKey

class VIPU(implicit p: Parameters) extends FunctionUnit(p(XSCoreParamsKey).VLEN) {
  XSError(io.in.valid && io.in.bits.uop.ctrl.fuOpType === VipuType.dummy, "VIPU OpType not supported")

  val uop = io.in.bits.uop
  val ctrl = uop.ctrl
  val vtype = ctrl.vconfig.vtype

  // TODO: mv VecImmExtractor from exe stage to read rf stage(or forward stage).
  val imm = VecInit(Seq.fill(VLEN/XLEN)(VecImmExtractor(ctrl.selImm, vtype.vsew, ctrl.imm))).asUInt

  val _src1 = Mux(SrcType.isImm(ctrl.srcType(0)), imm, io.in.bits.src(0))
  val _src2 = io.in.bits.src(1)
  val src1 = Mux(VipuType.needReverse(ctrl.fuOpType), _src2, _src1)
  val src2 = Mux(VipuType.needReverse(ctrl.fuOpType), _src1, _src2)
  val src4 = io.in.bits.src(3)
  val mask = src4(7,0) // TODO 
  val carryIn = Mux(ctrl.fuOpType === VipuType.madc0, 0.U(8.W), mask)

  val AdderWidth = XLEN
  val NumAdder = VLEN / XLEN
  val adder = Seq.fill(NumAdder)(Module(new VectorIntAdder()))
  for(i <- 0 until NumAdder) {
    adder(i).io.in_0 := src1(AdderWidth*(i+1)-1, AdderWidth*i)
    adder(i).io.in_1 := src2(AdderWidth*(i+1)-1, AdderWidth*i)
    adder(i).io.int_format := vtype.vsew // TODO
    adder(i).io.op_code := ctrl.fuOpType
    adder(i).io.carry_or_borrow_in := carryIn // TODO
    adder(i).io.uop_index := DontCare // TODO
  }
  val adder_result = VecInit(adder.map(_.io.out)).asUInt
  val adder_carry = LookupTree(vtype.vsew(1,0), List(
      "b00".U -> Cat(~0.U((VLEN-16).W), VecInit(adder.map(_.io.carry_or_borrow_or_compare_out(7,0))).asUInt),
      "b01".U -> Cat(~0.U((VLEN-8).W), VecInit(adder.map(_.io.carry_or_borrow_or_compare_out(3,0))).asUInt),
      "b10".U -> Cat(~0.U((VLEN-4).W), VecInit(adder.map(_.io.carry_or_borrow_or_compare_out(1,0))).asUInt),
      "b11".U -> Cat(~0.U((VLEN-2).W), VecInit(adder.map(_.io.carry_or_borrow_or_compare_out(0))).asUInt),
    ))

  io.out.bits.data := Mux(VipuType.outIsCarry(ctrl.fuOpType), adder_carry, adder_result)
  io.out.bits.uop := io.in.bits.uop
  io.out.valid := io.in.valid
  io.in.ready := io.out.ready
}

object VecImmExtractor {
  def Imm_OPIVIS(imm: UInt): UInt = {
    SignExt(imm(4,0), 8)
  }
  def Imm_OPIVIU(imm: UInt): UInt = {
    ZeroExt(imm(4,0), 8)
  }

  def imm_sew(sew: UInt, imm: UInt): UInt = {
    val _imm = SignExt(imm(7,0), 64)
    LookupTree(sew(1,0), List(
      "b00".U -> VecInit(Seq.fill(8)(_imm(7,0))).asUInt,
      "b01".U -> VecInit(Seq.fill(4)(_imm(15,0))).asUInt,
      "b10".U -> VecInit(Seq.fill(2)(_imm(31,0))).asUInt,
      "b11".U -> _imm(63,0),
    ))
  }

  def apply(immType: UInt, sew: UInt, imm: UInt): UInt = {
    val _imm = Mux(immType === SelImm.IMM_OPIVIS, Imm_OPIVIS(imm), Imm_OPIVIU(imm))
    imm_sew(sew, _imm(7,0))
  }
}
