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

class ShiftResultSelect(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val func = Input(UInt(4.W))
    val sll, srl, sra, rol, ror, bclr, bset, binv, bext = Input(UInt(XLEN.W))
    val shiftRes = Output(UInt(XLEN.W))
  })

  // val leftBit  = Mux(io.func(1), io.binv, Mux(io.func(0), io.bset, io.bclr))
  // val leftRes  = Mux(io.func(2), leftBit, io.sll)
  // val rightRes = Mux(io.func(1) && io.func(0), io.sra, Mux(io.func(1), io.bext, io.srl))
  val resultSource = VecInit(Seq(
    io.sll,
    io.sll,
    io.bclr,
    io.bset,
    io.binv,
    io.srl,
    io.bext,
    io.sra
  ))
  val simple = resultSource(io.func(2, 0))

  io.shiftRes := Mux(io.func(3), Mux(io.func(1), io.ror, io.rol), simple)
}

class WordResultSelect(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val func = Input(UInt())
    val sllw, srlw, sraw, rolw, rorw, addw, subw = Input(UInt((XLEN/2).W))
    val wordRes = Output(UInt(XLEN.W))
  })

  val addsubRes = Mux(!io.func(2) && io.func(1) && !io.func(0), io.subw, io.addw)
  val shiftRes = Mux(io.func(2), Mux(io.func(0), io.rorw, io.rolw),
                  Mux(io.func(1), io.sraw, Mux(io.func(0), io.srlw, io.sllw)))
  val wordRes = Mux(io.func(3), shiftRes, addsubRes)
  io.wordRes := SignExt(wordRes, XLEN)
}


class AluResSel(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val func = Input(UInt(3.W))
    val addRes, shiftRes, miscRes, compareRes, wordRes, condRes = Input(UInt(XLEN.W))
    val aluRes = Output(UInt(XLEN.W))
  })

  val res = Mux(io.func(2, 1) === 0.U, Mux(io.func(0), io.wordRes, io.shiftRes),
                                       Mux(!io.func(2), Mux(io.func(0), io.compareRes, io.addRes),
                                                        Mux(io.func(1, 0) === 3.U, io.condRes, io.miscRes)))
  io.aluRes := res
}

class AluDataModule(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val src = Vec(2, Input(UInt(XLEN.W)))
    val func = Input(FuOpType())
    val result = Output(UInt(XLEN.W))
  })
  val (src1, src2, func) = (io.src(0), io.src(1), io.func)

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
  val addw = Mux(func(2), addwResultAll(func(1, 0)), addwModule.io.addw)

  // subw
  val subModule = Module(new SubModule)
  val subw = subModule.io.sub

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
  val rolw = rotateLeftShiftWordModule.io.rolw
  rotateLeftShiftWordModule.io.shamt := shamt
  rotateLeftShiftWordModule.io.rolwSrc := src1

  // rorw
  val rotateRightShiftWordModule = Module(new RotateRightShiftWordModule)
  val rorw = rotateRightShiftWordModule.io.rorw
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
  val compareRes = Mux(func(2), Mux(func(1), maxMin, maxMinU), Mux(func(1), slt, Mux(func(0), sltu, sub)))

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
  val shiftResSel = Module(new ShiftResultSelect)
  shiftResSel.io.func := func(3, 0)
  shiftResSel.io.sll  := sll
  shiftResSel.io.srl  := srl
  shiftResSel.io.sra  := sra
  shiftResSel.io.rol  := rol
  shiftResSel.io.ror  := ror
  shiftResSel.io.bclr := bclr
  shiftResSel.io.binv := binv
  shiftResSel.io.bset := bset
  shiftResSel.io.bext := bext
  val shiftRes = shiftResSel.io.shiftRes

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


  val wordResSel = Module(new WordResultSelect)
  wordResSel.io.func := func
  wordResSel.io.addw := addw
  wordResSel.io.subw := subw
  wordResSel.io.sllw := sllw
  wordResSel.io.srlw := srlw
  wordResSel.io.sraw := sraw
  wordResSel.io.rolw := rolw
  wordResSel.io.rorw := rorw
  val wordRes = wordResSel.io.wordRes

  val aluResSel = Module(new AluResSel)
  aluResSel.io.func := func(6, 4)
  aluResSel.io.addRes := add
  aluResSel.io.compareRes := compareRes
  aluResSel.io.shiftRes := shiftRes
  aluResSel.io.miscRes := miscRes
  aluResSel.io.wordRes := wordRes
  aluResSel.io.condRes := condRes
  val aluRes = aluResSel.io.aluRes

  io.result := aluRes

  XSDebug(func === ALUOpType.lui32add, p"[alu] func lui32: src1=${Hexadecimal(src1)} src2=${Hexadecimal(src2)} alures=${Hexadecimal(aluRes)}\n")
  XSDebug(func === ALUOpType.lui32add, p"[alu] func lui32: add_src1=${Hexadecimal(addModule.io.src(0))} add_src2=${Hexadecimal(addModule.io.src(1))} addres=${Hexadecimal(add)}\n")

  XSDebug(func === ALUOpType.lui32addw, p"[alu] func lui32w: src1=${Hexadecimal(src1)} src2=${Hexadecimal(src2)} alures=${Hexadecimal(aluRes)}\n")
  XSDebug(func === ALUOpType.lui32addw, p"[alu] func lui32w: add_src1=${Hexadecimal(addwModule.io.srcw)} add_src2=${Hexadecimal(addwModule.io.src)} addres=${Hexadecimal(addw)}\n")
}
