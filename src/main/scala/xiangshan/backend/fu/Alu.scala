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
import utility.{LookupTree, LookupTreeDefault, ParallelMux, SignExt, ZeroExt}
import xiangshan._

class AddModule(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val src = Vec(2, Input(UInt(XLEN.W)))
    val srcw = Input(UInt((XLEN/2).W))
    val add = Output(UInt(XLEN.W))
    val addw = Output(UInt((XLEN/2).W))
  })
  io.add := io.src(0) + io.src(1)
  // TODO: why this extra adder?
  io.addw := io.srcw + io.src(1)(31,0)
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
    val revShamt = Input(UInt(6.W))
    val sllSrc = Input(UInt(XLEN.W))
    val sll = Output(UInt(XLEN.W))
    val revSll = Output(UInt(XLEN.W))
  })
  io.sll := io.sllSrc << io.shamt
  io.revSll := io.sllSrc << io.revShamt
}

class LeftShiftWordModule(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val shamt = Input(UInt(5.W))
    val revShamt = Input(UInt(5.W))
    val sllSrc = Input(UInt((XLEN/2).W))
    val sllw = Output(UInt((XLEN/2).W))
    val revSllw = Output(UInt((XLEN/2).W))
  })
  io.sllw := io.sllSrc << io.shamt
  io.revSllw := io.sllSrc << io.revShamt
}

class RightShiftModule(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val shamt = Input(UInt(6.W))
    val revShamt = Input(UInt(6.W))
    val srlSrc, sraSrc = Input(UInt(XLEN.W))
    val srl, sra = Output(UInt(XLEN.W))
    val revSrl = Output(UInt(XLEN.W))
  })
  io.srl  := io.srlSrc >> io.shamt
  io.sra  := (io.sraSrc.asSInt >> io.shamt).asUInt
  io.revSrl  := io.srlSrc >> io.revShamt
}

class RightShiftWordModule(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val shamt = Input(UInt(5.W))
    val revShamt = Input(UInt(5.W))
    val srlSrc, sraSrc = Input(UInt((XLEN/2).W))
    val srlw, sraw = Output(UInt((XLEN/2).W))
    val revSrlw = Output(UInt((XLEN/2).W))
  })

  io.srlw := io.srlSrc >> io.shamt
  io.sraw := (io.sraSrc.asSInt >> io.shamt).asUInt
  io.revSrlw := io.srlSrc >> io.revShamt
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

  val addsubRes = Mux(!io.func(2) && io.func(1), io.subw, io.addw)
  val shiftRes = Mux(io.func(2), Mux(io.func(0), io.rorw, io.rolw),
                  Mux(io.func(1), io.sraw, Mux(io.func(0), io.srlw, io.sllw)))
  val wordRes = Mux(io.func(3), shiftRes, addsubRes)
  io.wordRes := SignExt(wordRes, XLEN)
}


class AluResSel(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val func = Input(UInt(3.W))
    val addRes, shiftRes, miscRes, compareRes, wordRes = Input(UInt(XLEN.W))
    val aluRes = Output(UInt(XLEN.W))
  })

  val res = Mux(io.func(2, 1) === 0.U, Mux(io.func(0), io.wordRes, io.shiftRes),
            Mux(!io.func(2), Mux(io.func(0), io.compareRes, io.addRes), io.miscRes))
  io.aluRes := res
}

class AluDataModule(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val src = Vec(2, Input(UInt(XLEN.W)))
    val func = Input(FuOpType())
    val pred_taken, isBranch = Input(Bool())
    val result = Output(UInt(XLEN.W))
    val taken, mispredict = Output(Bool())
  })
  val (src1, src2, func) = (io.src(0), io.src(1), io.func)

  val shamt = src2(5, 0)
  val revShamt = ~src2(5,0) + 1.U

  // slliuw, sll
  val leftShiftModule = Module(new LeftShiftModule)
  val sll = leftShiftModule.io.sll
  val revSll = leftShiftModule.io.revSll
  leftShiftModule.io.sllSrc := Cat(Fill(32, func(0)), Fill(32, 1.U)) & src1
  leftShiftModule.io.shamt := shamt
  leftShiftModule.io.revShamt := revShamt

  // bclr, bset, binv
  val bitShift = 1.U << src2(5, 0)
  val bclr = src1 & ~bitShift
  val bset = src1 | bitShift
  val binv = src1 ^ bitShift

  // srl, sra, bext
  val rightShiftModule = Module(new RightShiftModule)
  val srl = rightShiftModule.io.srl
  val revSrl = rightShiftModule.io.revSrl
  val sra = rightShiftModule.io.sra
  rightShiftModule.io.shamt := shamt
  rightShiftModule.io.revShamt := revShamt
  rightShiftModule.io.srlSrc := src1
  rightShiftModule.io.sraSrc := src1
  val bext = srl(0)

  val rol = revSrl | sll
  val ror = srl | revSll

  // addw
  val addModule = Module(new AddModule)
  addModule.io.srcw := Mux(!func(2) && func(0), ZeroExt(src1(0), XLEN), src1(31, 0))
  val addwResultAll = VecInit(Seq(
    ZeroExt(addModule.io.addw(0), XLEN),
    ZeroExt(addModule.io.addw(7, 0), XLEN),
    ZeroExt(addModule.io.addw(15, 0), XLEN),
    SignExt(addModule.io.addw(15, 0), XLEN)
  ))
  val addw = Mux(func(2), addwResultAll(func(1, 0)), addModule.io.addw)

  // subw
  val subModule = Module(new SubModule)
  val subw = subModule.io.sub

  // sllw
  val leftShiftWordModule = Module(new LeftShiftWordModule)
  val sllw = leftShiftWordModule.io.sllw
  val revSllw = leftShiftWordModule.io.revSllw
  leftShiftWordModule.io.sllSrc := src1
  leftShiftWordModule.io.shamt := shamt
  leftShiftWordModule.io.revShamt := revShamt

  val rightShiftWordModule = Module(new RightShiftWordModule)
  val srlw = rightShiftWordModule.io.srlw
  val revSrlw = rightShiftWordModule.io.revSrlw
  val sraw = rightShiftWordModule.io.sraw
  rightShiftWordModule.io.shamt := shamt
  rightShiftWordModule.io.revShamt := revShamt
  rightShiftWordModule.io.srlSrc := src1
  rightShiftWordModule.io.sraSrc := src1

  val rolw = revSrlw | sllw
  val rorw = srlw | revSllw

  // add
  val wordMaskAddSource = Cat(Fill(32, func(0)), Fill(32, 1.U)) & src1
  val shaddSource = VecInit(Seq(
    Cat(wordMaskAddSource(62, 0), 0.U(1.W)),
    Cat(wordMaskAddSource(61, 0), 0.U(2.W)),
    Cat(wordMaskAddSource(60, 0), 0.U(3.W)),
    Cat(wordMaskAddSource(59, 0), 0.U(4.W))
  ))
  val sraddSource = VecInit(Seq(
    ZeroExt(src1(63, 29), XLEN),
    ZeroExt(src1(63, 30), XLEN),
    ZeroExt(src1(63, 31), XLEN),
    ZeroExt(src1(63, 32), XLEN)
  ))
  // TODO: use decoder or other libraries to optimize timing
  // Now we assume shadd has the worst timing.
  addModule.io.src(0) := Mux(func(3), shaddSource(func(2, 1)),
    Mux(func(2), sraddSource(func(1, 0)),
    Mux(func(1), ZeroExt(src1(0), XLEN), wordMaskAddSource))
  )
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

  // branch
  val branchOpTable = List(
    ALUOpType.getBranchType(ALUOpType.beq)  -> !xor.orR,
    ALUOpType.getBranchType(ALUOpType.blt)  -> slt,
    ALUOpType.getBranchType(ALUOpType.bltu) -> sltu
  )
  val taken = LookupTree(ALUOpType.getBranchType(func), branchOpTable) ^ ALUOpType.isBranchInvert(func)

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
  val aluRes = aluResSel.io.aluRes

  io.result := aluRes
  io.taken := taken
  io.mispredict := (io.pred_taken ^ taken) && io.isBranch
}

class Alu(implicit p: Parameters) extends FUWithRedirect {

  val uop = io.in.bits.uop

  val isBranch = ALUOpType.isBranch(io.in.bits.uop.ctrl.fuOpType)
  val dataModule = Module(new AluDataModule)

  dataModule.io.src := io.in.bits.src.take(2)
  dataModule.io.func := io.in.bits.uop.ctrl.fuOpType
  dataModule.io.pred_taken := uop.cf.pred_taken
  dataModule.io.isBranch := isBranch

  redirectOutValid := io.out.valid && isBranch
  redirectOut := DontCare
  redirectOut.level := RedirectLevel.flushAfter
  redirectOut.robIdx := uop.robIdx
  redirectOut.ftqIdx := uop.cf.ftqPtr
  redirectOut.ftqOffset := uop.cf.ftqOffset
  redirectOut.cfiUpdate.isMisPred := dataModule.io.mispredict
  redirectOut.cfiUpdate.taken := dataModule.io.taken
  redirectOut.cfiUpdate.predTaken := uop.cf.pred_taken

  io.in.ready := io.out.ready
  io.out.valid := io.in.valid
  io.out.bits.uop <> io.in.bits.uop
  io.out.bits.data := dataModule.io.result
}
