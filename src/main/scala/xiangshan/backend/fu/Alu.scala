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

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode.TruthTable
import utility.{LookupTree, LookupTreeDefault, ParallelMux, SignExt, XSDebug, ZeroExt}
import xiangshan._

import math.pow
import xiangshan.backend.fu.util._

class AddModule(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val src = Vec(2, Input(UInt(XLEN.W)))
    val add = Output(UInt(XLEN.W))
  })
  io.add := io.src(0) + io.src(1)
}

class AddWModule(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val src  = Input(UInt((XLEN/2).W))
    val srcw = Input(UInt((XLEN/2).W))
    val addw = Output(UInt((XLEN/2).W))
  })
  io.addw := io.srcw + io.src
}

class SubModule(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val src = Vec(2, Input(UInt(XLEN.W)))
    val sub = Output(UInt((XLEN+1).W))
  })
  io.sub := (io.src(0) +& (~io.src(1)).asUInt) + 1.U
}

class LeftShiftModule(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val shamt = Input(UInt(6.W))
    val sllSrc = Input(UInt(XLEN.W))
    val sll = Output(UInt(XLEN.W))
  })
  io.sll := doShiftLeft(io.sllSrc, io.shamt)
}

class LeftShiftWordModule(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val shamt = Input(UInt(5.W))
    val sllSrc = Input(UInt((XLEN/2).W))
    val sllw = Output(UInt((XLEN/2).W))
  })
  io.sllw := doShiftLeft(io.sllSrc, io.shamt)
}

class RotateLeftShiftModule(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val shamt = Input(UInt(6.W))
    val rolSrc = Input(UInt(XLEN.W))
    val rol = Output(UInt(XLEN.W))
  })
  io.rol := doShiftRotateLeft(io.rolSrc, io.shamt)
}

class RotateLeftShiftWordModule(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val shamt = Input(UInt(5.W))
    val rolwSrc = Input(UInt((XLEN/2).W))
    val rolw = Output(UInt(XLEN.W))
  })
  io.rolw := doShiftRotateLeftWord(io.rolwSrc, io.shamt)
}

class RightShiftModule(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val shamt = Input(UInt(6.W))
    val src = Input(UInt(XLEN.W))
    val srl, sra = Output(UInt(XLEN.W))
  })
  io.srl  := io.src >> io.shamt
  io.sra  := doShiftRightArith(io.src, io.shamt)
}

class RightShiftWordModule(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val shamt = Input(UInt(5.W))
    val src = Input(UInt((XLEN/2).W))
    val srlw, sraw = Output(UInt((XLEN/2).W))
  })

  io.srlw := io.src >> io.shamt
  io.sraw := doShiftRightArith(io.src, io.shamt)
}

class RotateRightShiftModule(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val shamt = Input(UInt(6.W))
    val rorSrc = Input(UInt(XLEN.W))
    val ror = Output(UInt(XLEN.W))
  })
  io.ror := doShiftRotateRight(io.rorSrc, io.shamt)
}

class RotateRightShiftWordModule(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val shamt = Input(UInt(5.W))
    val rorwSrc = Input(UInt((XLEN/2).W))
    val rorw = Output(UInt(XLEN.W))
  })
  io.rorw := doShiftRotateRightWord(io.rorwSrc, io.shamt)
}

class MiscResultSelect(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val func = Input(UInt(6.W))
    val and, or, xor, orcb, orh48, sextb, packh, sexth, packw, revb, rev8, pack = Input(UInt(XLEN.W))
    val src = Input(UInt(XLEN.W))
    val miscRes = Output(UInt(XLEN.W))
  })

  val logicRes = VecInit(Seq(
    io.and,
    io.or,
    io.xor,
    io.orcb
  ))(io.func(2, 1))
  val miscRes = VecInit(Seq(io.sextb, io.packh, io.sexth, io.packw))(io.func(1, 0))
  val logicBase = Mux(io.func(3), miscRes, logicRes)

  val revRes = VecInit(Seq(io.revb, io.rev8, io.pack, io.orh48))(io.func(1, 0))
  val customRes = VecInit(Seq(
    Cat(0.U(31.W), io.src(31, 0), 0.U(1.W)),
    Cat(0.U(30.W), io.src(31, 0), 0.U(2.W)),
    Cat(0.U(29.W), io.src(31, 0), 0.U(3.W)),
    Cat(0.U(56.W), io.src(15, 8))))(io.func(1, 0))
  val logicAdv = Mux(io.func(3), customRes, revRes)

  val mask = Cat(Fill(15, io.func(0)), 1.U(1.W))
  val maskedLogicRes = mask & logicRes

  io.miscRes := Mux(io.func(5), maskedLogicRes, Mux(io.func(4), logicAdv, logicBase))
}

class ConditionalZeroModule(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val condition = Input(UInt(XLEN.W))
    val value = Input(UInt(XLEN.W))
    val isNez = Input(Bool())
    val condRes = Output(UInt(XLEN.W))
  })

  val condition_zero = io.condition === 0.U
  val use_zero = !io.isNez &&  condition_zero ||
                  io.isNez && !condition_zero

  io.condRes := Mux(use_zero, 0.U, io.value)
}

class AluDataModule(val aluNeedPc: Boolean = false)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val pc = Input(UInt(XLEN.W))
    val src = Vec(2, Input(UInt(XLEN.W)))
    val func = Input(FuOpType())
    val result = Output(UInt(XLEN.W))
  })
  val (src1, src2, func, pc) = (io.src(0), io.src(1), io.func, io.pc)

  val shamt = src2(5, 0)

  // slliuw, sll
  val leftShiftModule = Module(new LeftShiftModule)
  val sll = leftShiftModule.io.sll
  leftShiftModule.io.sllSrc := Cat(Fill(32, func(0)), Fill(32, 1.U)) & src1
  leftShiftModule.io.shamt := shamt

  // bclr, bset, binv
  val bitShift = 1.U << src2(5, 0)
  val bclr = src1 & ~bitShift
  val bset = src1 | bitShift
  val binv = src1 ^ bitShift

  // srl, sra, bext
  val rightShiftModule = Module(new RightShiftModule)
  val srl = rightShiftModule.io.srl
  val sra = rightShiftModule.io.sra
  rightShiftModule.io.shamt := shamt
  rightShiftModule.io.src := src1
  val bext = srl(0)

  // rol
  val rotateLeftModule = Module(new RotateLeftShiftModule)
  val rol = rotateLeftModule.io.rol
  rotateLeftModule.io.shamt := shamt
  rotateLeftModule.io.rolSrc := src1

  // ror
  val rotateRightModule = Module(new RotateRightShiftModule)
  val ror = rotateRightModule.io.ror
  rotateRightModule.io.shamt := shamt
  rotateRightModule.io.rorSrc := src1

  // addw
  val addwModule = Module(new AddWModule)
  addwModule.io.srcw := src1(31, 0)
  addwModule.io.src  := src2(31, 0)
  val addwResultAll = VecInit(Seq(
    ZeroExt(addwModule.io.addw(0), XLEN),
    ZeroExt(addwModule.io.addw(7, 0), XLEN),
    ZeroExt(addwModule.io.addw(15, 0), XLEN),
    SignExt(addwModule.io.addw(15, 0), XLEN)
  ))
  val addw = Wire(UInt((XLEN / 2).W))
  addw := Mux(func(2), addwResultAll(func(1, 0)), addwModule.io.addw)

  // subw
  val subModule = Module(new SubModule)
  val subw = Wire(UInt((XLEN/2).W))
  subw := subModule.io.sub

  // sllw
  val leftShiftWordModule = Module(new LeftShiftWordModule)
  val sllw = leftShiftWordModule.io.sllw
  leftShiftWordModule.io.sllSrc := src1
  leftShiftWordModule.io.shamt := shamt

  val rightShiftWordModule = Module(new RightShiftWordModule)
  val srlw = rightShiftWordModule.io.srlw
  val sraw = rightShiftWordModule.io.sraw
  rightShiftWordModule.io.shamt := shamt
  rightShiftWordModule.io.src := src1

  // rolw
  val rotateLeftShiftWordModule = Module(new RotateLeftShiftWordModule)
  val rolw = Wire(UInt((XLEN/2).W))
  rolw := rotateLeftShiftWordModule.io.rolw
  rotateLeftShiftWordModule.io.shamt := shamt
  rotateLeftShiftWordModule.io.rolwSrc := src1

  // rorw
  val rotateRightShiftWordModule = Module(new RotateRightShiftWordModule)
  val rorw = Wire(UInt((XLEN/2).W))
  rorw := rotateRightShiftWordModule.io.rorw
  rotateRightShiftWordModule.io.shamt := shamt
  rotateRightShiftWordModule.io.rorwSrc := src1

  // add
  val addModule = Module(new AddModule)
  addModule.io.src(0) := src1
  addModule.io.src(1) := src2
  val add = addModule.io.add

  // sub
  val sub  = subModule.io.sub
  subModule.io.src(0) := src1
  subModule.io.src(1) := src2
  val sltu    = !sub(XLEN)
  val slt     = src1(XLEN - 1) ^ src2(XLEN - 1) ^ sltu
  val maxMin  = Mux(slt ^ func(0), src2, src1)
  val maxMinU = Mux(sltu ^ func(0), src2, src1)

  // logic
  val logicSrc2 = Mux(!func(5) && func(0), ~src2, src2)
  val and     = src1 & logicSrc2
  val or      = src1 | logicSrc2
  val xor     = src1 ^ logicSrc2
  val orcb    = Cat((0 until 8).map(i => Fill(8, src1(i * 8 + 7, i * 8).orR)).reverse)
  val orh48   = Cat(src1(63, 8), 0.U(8.W)) | src2

  val sextb = SignExt(src1(7, 0), XLEN)
  val packh = Cat(src2(7,0), src1(7,0))
  val sexth = SignExt(src1(15, 0), XLEN)
  val packw = SignExt(Cat(src2(15, 0), src1(15, 0)), XLEN)

  val revb = Cat((0 until 8).map(i => Reverse(src1(8 * i + 7, 8 * i))).reverse)
  val pack = Cat(src2(31, 0), src1(31, 0))
  val rev8 = Cat((0 until 8).map(i => src1(8 * i + 7, 8 * i)))


  // Result Select
  val miscResSel = Module(new MiscResultSelect)
  miscResSel.io.func    := func(5, 0)
  miscResSel.io.and     := and
  miscResSel.io.or      := or
  miscResSel.io.xor     := xor
  miscResSel.io.orcb    := orcb
  miscResSel.io.orh48   := orh48
  miscResSel.io.sextb   := sextb
  miscResSel.io.packh   := packh
  miscResSel.io.sexth   := sexth
  miscResSel.io.packw   := packw
  miscResSel.io.revb    := revb
  miscResSel.io.rev8    := rev8
  miscResSel.io.pack    := pack
  miscResSel.io.src     := src1
  val miscRes = miscResSel.io.miscRes

  val condModule = Module(new ConditionalZeroModule)
  condModule.io.value     := src1
  condModule.io.condition := src2
  condModule.io.isNez     := func(1)
  val condRes = condModule.io.condRes

  val jmpModule = Module(new AddModule)
  jmpModule.io.src(0) := pc
  jmpModule.io.src(1) := src2
  val jmpRes = jmpModule.io.add

  val isAdd = !func(6) & func(5) & !func(4)
  val isCompare = !func(6) & func(5) & func(4)
  val isMaxMin  = isCompare & func(2) & func(1)
  val isMaxMinU = isCompare & func(2) & !func(1)
  val isSlt  = isCompare & !func(2) & func(1)
  val isSltu = isCompare & !func(2) & !func(1) & func(0)
  val isSub  = isCompare & !func(2) & !func(1) & !func(0)
  val isShift = !func(6) & !func(5) & !func(4)
  val isSll  = isShift & !func(3) & !func(2) & !func(1)
  val isBclr = isShift & !func(3) & !func(2) &  func(1) & !func(0)
  val isBset = isShift & !func(3) & !func(2) &  func(1) &  func(0)
  val isBinv = isShift & !func(3) &  func(2) & !func(1) & !func(0)
  val isSrl  = isShift & !func(3) &  func(2) & !func(1) &  func(0)
  val isBext = isShift & !func(3) &  func(2) &  func(1) & !func(0)
  val isSra  = isShift & !func(3) &  func(2) &  func(1) &  func(0)
  val isRol  = isShift & func(3) & !func(1)
  val isRor  = isShift & func(3) &  func(1)
  val isMisc  =  func(6) & (!func(5) | !func(4))
  val isWiden = !func(6) & !func(5) &  func(4)
  val isAddw = isWiden & (!func(3) & !func(2) & (!func(1) | func(0)) | !func(3) & func(2))
  val isSubw = isWiden & !func(3) & !func(2) &  func(1) & !func(0)
  val isSllw = isWiden &  func(3) & !func(2) & !func(1) & !func(0)
  val isSrlw = isWiden &  func(3) & !func(2) &  func(0)
  val isSraw = isWiden &  func(3) & !func(2) &  func(1)
  val isRolw = isWiden &  func(3) &  func(2) & !func(0)
  val isRorw = isWiden &  func(3) &  func(2) &  func(0)
  val isZicond = ALUOpType.isZicond(func)
  val isJmp = if (aluNeedPc) ALUOpType.isJmp(func) else false.B

  val aluRes = Mux1H(Seq(
    isAdd -> add,
    isMaxMin  -> maxMin,
    isMaxMinU -> maxMinU,
    isSlt -> slt,
    isSltu -> sltu,
    isSub -> sub,
    isSll  -> sll,
    isBclr -> bclr,
    isBset -> bset,
    isBinv -> binv,
    isSrl  -> srl,
    isBext -> bext,
    isSra -> sra,
    isRol -> rol,
    isRor -> ror,
    isMisc -> miscRes,
    isAddw -> SignExt(addw, XLEN),
    isSubw -> SignExt(subw, XLEN),
    isSllw -> SignExt(sllw, XLEN),
    isSrlw -> SignExt(srlw, XLEN),
    isSraw -> SignExt(sraw, XLEN),
    isRolw -> SignExt(rolw, XLEN),
    isRorw -> SignExt(rorw, XLEN),
    isZicond -> condRes,
    isJmp -> jmpRes,
  ))

  io.result := aluRes

  XSDebug(func === ALUOpType.lui32add, p"[alu] func lui32: src1=${Hexadecimal(src1)} src2=${Hexadecimal(src2)} alures=${Hexadecimal(aluRes)}\n")
  XSDebug(func === ALUOpType.lui32add, p"[alu] func lui32: add_src1=${Hexadecimal(addModule.io.src(0))} add_src2=${Hexadecimal(addModule.io.src(1))} addres=${Hexadecimal(add)}\n")

  XSDebug(func === ALUOpType.lui32addw, p"[alu] func lui32w: src1=${Hexadecimal(src1)} src2=${Hexadecimal(src2)} alures=${Hexadecimal(aluRes)}\n")
  XSDebug(func === ALUOpType.lui32addw, p"[alu] func lui32w: add_src1=${Hexadecimal(addwModule.io.srcw)} add_src2=${Hexadecimal(addwModule.io.src)} addres=${Hexadecimal(addw)}\n")
}
