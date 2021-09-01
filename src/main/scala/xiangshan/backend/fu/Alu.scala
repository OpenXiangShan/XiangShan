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
import utils.{LookupTreeDefault, LookupTree, ParallelMux, SignExt, ZeroExt}
import xiangshan._

class AddModule(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val src = Vec(2, Input(UInt(XLEN.W)))
    val srcw = Input(UInt((XLEN/2).W))
    val add = Output(UInt(XLEN.W))
    val addw = Output(UInt((XLEN/2).W))
  })
  io.add := io.src(0) + io.src(1)
  io.addw := io.srcw + io.src(1)(31,0)
}

class SubModule(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val src = Vec(2, Input(UInt(XLEN.W)))
    val sub = Output(UInt((XLEN+1).W))
  })
  io.sub := (io.src(0) +& (~io.src(1)).asUInt()) + 1.U
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
  io.sra  := (io.sraSrc.asSInt() >> io.shamt).asUInt()
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
  io.sraw := (io.sraSrc.asSInt() >> io.shamt).asUInt()
  io.revSrlw := io.srlSrc >> io.revShamt
}


class MiscResultSelect(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val func = Input(UInt())
    val andn, orn, xnor, and, or, xor, sextb, sexth, zexth, rev8, orcb = Input(UInt(XLEN.W))
    val miscRes = Output(UInt(XLEN.W))
  })

  val miscRes = ParallelMux(List(
    ALUOpType.andn -> io.andn,
    ALUOpType.and  -> io.and,
    ALUOpType.orn  -> io.orn,
    ALUOpType.or   -> io.or,
    ALUOpType.xnor -> io.xnor,
    ALUOpType.xor  -> io.xor,
    ALUOpType.sext_b -> io.sextb,
    ALUOpType.sext_h -> io.sexth,
    ALUOpType.zext_h -> io.zexth,
    ALUOpType.orc_b  -> io.orcb,
    ALUOpType.rev8   -> io.rev8
  ).map(x => (x._1(3, 0) === io.func(3, 0), x._2)))

  io.miscRes := miscRes
}

class ShiftResultSelect(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val func = Input(UInt())
    val sll, srl, sra, rol, ror, bclr, bset, binv, bext = Input(UInt(XLEN.W))
    val shiftRes = Output(UInt(XLEN.W))
  })
  
  val leftBit  = Mux(io.func(1), io.binv, Mux(io.func(0), io.bset, io.bclr))
  val leftRes  = Mux(io.func(2), leftBit, io.sll)
  val rightRes = Mux(io.func(2), io.sra, Mux(io.func(1), io.bext, io.srl))

  io.shiftRes := Mux(io.func(4), Mux(io.func(3), io.ror, io.rol), Mux(io.func(3), rightRes, leftRes))
}

class WordResultSelect(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val func = Input(UInt())
    val sllw, srlw, sraw, rolw, rorw, addw, subw = Input(UInt((XLEN/2).W))
    val wordRes = Output(UInt(XLEN.W))
  })

  val addsubRes = Mux(io.func(6), io.subw, io.addw)
  val shiftRes = Mux(io.func(4), 
                  Mux(io.func(3), io.rorw, io.rolw),
                  Mux(io.func(3), 
                    Mux(io.func(2), io.sraw, io.srlw), 
                    io.sllw))
  val wordRes = Mux(io.func(6,5) === 2.U, shiftRes, addsubRes)
  io.wordRes := SignExt(wordRes, XLEN)
}


class AluResSel(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val func = Input(UInt())
    val addRes, shiftRes, miscRes, compareRes, wordRes = Input(UInt(XLEN.W))
    val aluRes = Output(UInt(XLEN.W))
  })
  
  val res = Mux(io.func(7), io.wordRes, Mux(io.func(6),
    Mux(io.func(5), io.compareRes, io.shiftRes),
    Mux(io.func(5), io.addRes, io.miscRes)
  ))
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

  val isW = ALUOpType.isWordOp(func)

  val addModule = Module(new AddModule)
  val shaddShamt = func(2,1)
  val add  = addModule.io.add
  val addw = addModule.io.addw
  addModule.io.src(0) := (Cat(Fill(32, func(0)), Fill(32,1.U)) & src1) << shaddShamt
  addModule.io.src(1) := src2
  addModule.io.srcw := src1(31,0)



  val subModule = Module(new SubModule)
  val sub  = subModule.io.sub
  val subw = subModule.io.sub
  subModule.io.src(0) := src1
  subModule.io.src(1) := src2

  val shamt = src2(5, 0)
  val revShamt = ~src2(5,0) + 1.U

  val leftShiftModule = Module(new LeftShiftModule)
  val sll = leftShiftModule.io.sll
  val revSll = leftShiftModule.io.revSll
  leftShiftModule.io.sllSrc := Cat(Fill(32, func(0)), Fill(32,1.U)) & src1
  leftShiftModule.io.shamt := shamt
  leftShiftModule.io.revShamt := revShamt
  
  val leftShiftWordModule = Module(new LeftShiftWordModule)
  val sllw = leftShiftWordModule.io.sllw
  val revSllw = leftShiftWordModule.io.revSllw
  leftShiftWordModule.io.sllSrc := src1
  leftShiftWordModule.io.shamt := shamt
  leftShiftWordModule.io.revShamt := revShamt

  val rightShiftModule = Module(new RightShiftModule)
  val srl = rightShiftModule.io.srl
  val revSrl = rightShiftModule.io.revSrl
  val sra = rightShiftModule.io.sra
  rightShiftModule.io.shamt := shamt
  rightShiftModule.io.revShamt := revShamt
  rightShiftModule.io.srlSrc := src1
  rightShiftModule.io.sraSrc := src1

  val rightShiftWordModule = Module(new RightShiftWordModule)
  val srlw = rightShiftWordModule.io.srlw
  val revSrlw = rightShiftWordModule.io.revSrlw
  val sraw = rightShiftWordModule.io.sraw
  rightShiftWordModule.io.shamt := shamt
  rightShiftWordModule.io.revShamt := revShamt
  rightShiftWordModule.io.srlSrc := src1
  rightShiftWordModule.io.sraSrc := src1

  val rol = revSrl | sll
  val ror = srl | revSll
  val rolw = revSrlw | sllw
  val rorw = srlw | revSllw
  
  val bitShift = 1.U << src2(5, 0)
  val bset = src1 | bitShift
  val bclr = src1 & ~bitShift
  val binv = src1 ^ bitShift
  val bext = srl(0)

  val andn    = ~(src1 & src2)
  val orn     = ~(src1 | src2)
  val xnor    = ~(src1 ^ src2)
  val and     = ~andn
  val or      = ~orn
  val xor     = ~xnor
  val sgtu    = sub(XLEN)
  val sltu    = !sgtu
  val slt     = xor(XLEN-1) ^ sltu
  // val maxMin  = Mux(slt ^ func(0), src2, src1)
  // val maxMinU = Mux(sltu^ func(0), src2, src1)
  val maxMin  = Mux(slt ^ func(0), src2, src1)
  val maxMinU = Mux((sgtu && func(0)) || ~(sgtu && func(0)), src2, src1)
  val sextb   = SignExt(src1(7, 0), XLEN)
  val sexth   = SignExt(src1(15, 0), XLEN)
  val zexth   = ZeroExt(src1(15, 0), XLEN)
  val rev8    = Cat(src1(7,0), src1(15,8), src1(23,16), src1(31,24), 
                    src1(39,32), src1(47,40), src1(55,48), src1(63,56))
  val orcb    = Cat(Reverse(src1(63,56)), Reverse(src1(55,48)), Reverse(src1(47,40)), Reverse(src1(39,32)),
                    Reverse(src1(31,24)), Reverse(src1(23,16)), Reverse(src1(15,8)), Reverse(src1(7,0)))

  val branchOpTable = List(
    ALUOpType.getBranchType(ALUOpType.beq)  -> !xor.orR,
    ALUOpType.getBranchType(ALUOpType.blt)  -> slt,
    ALUOpType.getBranchType(ALUOpType.bltu) -> sltu
  )
  val taken = LookupTree(ALUOpType.getBranchType(func), branchOpTable) ^ ALUOpType.isBranchInvert(func)


  // Result Select

  val compareRes = Mux(func(2), Mux(func(1), maxMin, maxMinU), Mux(func(1), slt, Mux(func(0), sltu, sub)))

  val shiftResSel = Module(new ShiftResultSelect)
  shiftResSel.io.func := func(4,0)
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
  miscResSel.io.func    := func(3, 0)
  miscResSel.io.andn    := andn
  miscResSel.io.orn     := orn
  miscResSel.io.xnor    := xnor
  miscResSel.io.and     := and
  miscResSel.io.or      := or
  miscResSel.io.xor     := xor
  miscResSel.io.sextb   := sextb
  miscResSel.io.sexth   := sexth
  miscResSel.io.zexth   := zexth
  miscResSel.io.rev8    := rev8
  miscResSel.io.orcb    := orcb
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
  aluResSel.io.func := func
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

  val (src1, src2, func, pc, uop) = (
    io.in.bits.src(0),
    io.in.bits.src(1),
    io.in.bits.uop.ctrl.fuOpType,
    SignExt(io.in.bits.uop.cf.pc, AddrBits),
    io.in.bits.uop
  )

  val valid = io.in.valid
  val isBranch = ALUOpType.isBranch(func)
  val dataModule = Module(new AluDataModule)

  dataModule.io.src(0) := src1
  dataModule.io.src(1) := src2
  dataModule.io.func := func
  dataModule.io.pred_taken := uop.cf.pred_taken
  dataModule.io.isBranch := isBranch

  redirectOutValid := io.out.valid && isBranch
  redirectOut := DontCare
  redirectOut.level := RedirectLevel.flushAfter
  redirectOut.roqIdx := uop.roqIdx
  redirectOut.ftqIdx := uop.cf.ftqPtr
  redirectOut.ftqOffset := uop.cf.ftqOffset
  redirectOut.cfiUpdate.isMisPred := dataModule.io.mispredict
  redirectOut.cfiUpdate.taken := dataModule.io.taken
  redirectOut.cfiUpdate.predTaken := uop.cf.pred_taken

  io.in.ready := io.out.ready
  io.out.valid := valid
  io.out.bits.uop <> io.in.bits.uop
  io.out.bits.data := dataModule.io.result
}
