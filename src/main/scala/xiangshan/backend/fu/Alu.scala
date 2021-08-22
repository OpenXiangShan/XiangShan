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
    val add_l, add_w = Output(UInt((XLEN+1).W))
  })
  io.add_l := io.src(0) +& io.src(1)
  io.add_w := io.src(0)(31,0) +& io.src(1)
}

class SubModule(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val src = Vec(2, Input(UInt(XLEN.W)))
    val out = Output(UInt((XLEN+1).W))
  })
  io.out := (io.src(0) +& (~io.src(1)).asUInt()) + 1.U
}

class LeftShiftModule(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val shamt = Input(UInt(6.W))
    val sllSrc = Input(UInt(XLEN.W))
    val sll = Output(UInt(XLEN.W))
  })
  io.sll := (io.sllSrc << io.shamt)(XLEN - 1, 0)
}

class RightShiftModule(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val shamt = Input(UInt(6.W))
    val srlSrc, sraSrc = Input(UInt(XLEN.W))
    val srl_l, srl_w, sra_l, sra_w = Output(UInt(XLEN.W))
  })
  io.srl_l := io.srlSrc >> io.shamt
  io.srl_w := io.srlSrc(31, 0) >> io.shamt
  io.sra_l := (io.sraSrc.asSInt() >> io.shamt).asUInt()
  io.sra_w := (Cat(Fill(32, io.sraSrc(31)), io.sraSrc(31, 0)).asSInt() >> io.shamt).asUInt()
}

// class RotateShiftModule(implicit p: Parameters) extends XSModule {
//   val io = IO(new Bundle() {
//     val shamt = Input(UInt(6.W))
//     val roSrc = Input(UInt(XLEN.W))
//     val rol_l, rol_w, ror_l, ror_w = Output(UInt(XLEN.W))
//   })
//   io.rol_l := io.roSrc << io.shamt | io.roSrc >> ((~io.shamt).asUInt()+&1.U)
//   io.rol_w := (io.roSrc << io.shamt | io.roSrc >> (32.U-io.shamt))(5,0)
//   io.ror_l := io.roSrc>>io.shamt | io.roSrc << ((~io.shamt).asUInt()+&1.U)
//   io.ror_w := (io.roSrc>>io.shamt | io.roSrc << (32.U-io.shamt))(5,0)
// }

class MiscResultSelect(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val func = Input(UInt())
    val andn, orn, xnor, and, or, xor, sltu, slt, maxMin, maxMinU, sextb, sexth, zexth, rev8, orcb = Input(UInt(XLEN.W))
    val miscRes = Output(UInt(XLEN.W))
  })

  val (func, andn, orn, xnor, and, or, xor, sltu, slt, maxMin, maxMinU, sextb, sexth, zexth, rev8, orcb) =
    (io.func, io.andn, io.orn, io.xnor, io.and, io.or, io.xor, io.sltu, io.slt, io.maxMin, io.maxMinU, io.sextb, io.sexth, io.zexth, io.rev8, io.orcb)
  
  val baseMisc = ParallelMux(List(
    ALUOpType.andn -> andn,
    ALUOpType.and  -> and,
    ALUOpType.orn  -> orn,
    ALUOpType.or   -> or,
    ALUOpType.xnor -> xnor,
    ALUOpType.xor  -> xor
  ).map(x => (x._1(2, 0) === io.func(2, 0), x._2)))

  val bitMisc = ParallelMux(List(
    ALUOpType.sext_b -> sextb,
    ALUOpType.sext_h -> sexth,
    ALUOpType.zext_h -> zexth,
    ALUOpType.orc_b  -> orcb,
    ALUOpType.rev8   -> rev8
  ).map(x => (x._1(2, 0) === io.func(2, 0), x._2)))

  val compMisc = Mux(func(2), 
    Mux(func(1), maxMinU, maxMin), 
    Mux(func(1), sltu, slt))

  io.miscRes := Mux(func(4), compMisc, Mux(func(3), bitMisc, baseMisc))
}

class ShiftResultSelect(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val func = Input(UInt())
    val sll, srl, sra, rot, bclr, bset, binv, bext = Input(UInt(XLEN.W))
    val shiftRes = Output(UInt(XLEN.W))
  })

  val (func, sll, srl, sra, rot, bclr, bset, binv, bext) = 
    (io.func, io.sll, io.srl, io.sra, io.rot, io.bclr, io.bset, io.binv, io.bext)
  
  val singleBitRes = ParallelMux(List(
    ALUOpType.bclr -> bclr,
    ALUOpType.binv -> binv,
    ALUOpType.bset -> bset,
    ALUOpType.bext -> bext
  ).map(x => (x._1(1, 0) === io.func(1, 0), x._2)))

  val lrShiftRes = Mux(func(1), Mux(func(0), sra, srl), sll)
  //val rotateShiftRes = Mux(func(3), ror, rol)

  io.shiftRes := Mux(func(4), rot, Mux(func(2), singleBitRes, lrShiftRes))
}


class AluResSel(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val func = Input(UInt())
    // val addSubRes, shiftRes, miscRes, countRes = Input(UInt(XLEN.W))
    val addSubRes, shiftRes, miscRes = Input(UInt(XLEN.W))
    val aluRes = Output(UInt(XLEN.W))
  })
  
  val res = Mux(io.func(6),
    io.shiftRes,
    Mux(io.func(5), io.addSubRes, io.miscRes)
  )
  val h32 = Mux(io.func(7), Fill(32, res(31)), res(63, 32))
  io.aluRes := Cat(h32, res(31, 0))
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
  val add = Mux(isW, addModule.io.add_w, addModule.io.add_l)
  addModule.io.src(1) := src2
  addModule.io.src(0) := src1 << shaddShamt
  
  // ParallelMux(List(
  //   "b000".U -> src1,
  //   "b001".U -> src1(31,0),
  //   "b010".U -> Cat(src1(62,0), 0.U(1.W)),
  //   "b011".U -> Cat(src1(30,0), 0.U(1.W)),
  //   "b100".U -> Cat(src1(61,0), 0.U(2.W)),
  //   "b101".U -> Cat(src1(29,0), 0.U(2.W)),
  //   "b110".U -> Cat(src1(60,0), 0.U(3.W)),
  //   "b111".U -> Cat(src1(28,0), 0.U(3.W)),
  // ).map(x => (x._1(2, 0) === func(2,0), x._2)))
  

  val subModule = Module(new SubModule)
  val sub = subModule.io.out
  subModule.io.src(0) := src1
  subModule.io.src(1) := src2


 // Misc
  val andn    = ~(src1 & src2)
  val orn     = ~(src1 | src2)
  val xnor    = ~(src1 ^ src2)
  val and     = ~andn
  val or      = ~orn
  val xor     = ~xnor
  val sltu    = !sub(XLEN)
  val slt     = xor(XLEN-1) ^ sltu
  val maxMin  = Mux(slt ^ func(0), src2, src1)
  val maxMinU = Mux(sltu^ func(0), src2, src1)
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
  

  // Shift
  //val isW = ALUOpType.isWordOp(func)
  val shamt = Cat(!isW && src2(5), src2(4, 0))
  val complement = ~src2(5,0) + 1.U
  val revShamt = Cat(!isW && complement(5), complement(4, 0))

  val leftShiftModule = Module(new LeftShiftModule)
  leftShiftModule.io.sllSrc := Mux(func(2), 1.U, Mux(func(0), src1(31,0), src1))
  leftShiftModule.io.shamt := Mux(func(3), revShamt, shamt)
  val sll  = leftShiftModule.io.sll
  val bset = src1 | sll
  val bclr = src1 & ~sll
  val binv = src1 ^ sll


  val rightShiftModule = Module(new RightShiftModule)
  rightShiftModule.io.shamt := Mux(func(3), shamt, revShamt)
  rightShiftModule.io.srlSrc := src1
  rightShiftModule.io.sraSrc := src1
  val srl = Mux(isW, rightShiftModule.io.srl_w, rightShiftModule.io.srl_l)
  val sra = Mux(isW, rightShiftModule.io.sra_w, rightShiftModule.io.sra_l)
  val bext = srl(0)

  //rotateShift
  val rot = srl|sll

  // val rotateShiftModule = Module(new RotateShiftModule)
  // rotateShiftModule.io.shamt := Mux(isW, src2(4,0), src2(5,0))
  // rotateShiftModule.io.roSrc := Mux(isW, src1(31,0), src1)
  // val rol = Mux(isW, rotateShiftModule.io.rol_w, rotateShiftModule.io.rol_l)
  // val ror = Mux(isW, rotateShiftModule.io.ror_w, rotateShiftModule.io.ror_l)

  val miscResSel = Module(new MiscResultSelect)
  miscResSel.io.func    := func(4, 0)
  miscResSel.io.andn    := andn
  miscResSel.io.orn     := orn
  miscResSel.io.xnor    := xnor
  miscResSel.io.and     := and
  miscResSel.io.or      := or
  miscResSel.io.xor     := xor
  miscResSel.io.sltu    := sltu
  miscResSel.io.slt     := slt
  miscResSel.io.maxMin  := maxMin
  miscResSel.io.maxMinU := maxMinU
  miscResSel.io.sextb   := sextb
  miscResSel.io.sexth   := sexth
  miscResSel.io.zexth   := zexth
  miscResSel.io.rev8    := rev8
  miscResSel.io.orcb    := orcb
  val miscRes = miscResSel.io.miscRes

  val addSubRes = Mux(func(3), sub, add)

  val shiftResSel = Module(new ShiftResultSelect)
  shiftResSel.io.func := func(4,0)
  shiftResSel.io.sll  := sll
  shiftResSel.io.srl  := srl
  shiftResSel.io.sra  := sra
  shiftResSel.io.rot  := rot
  // shiftResSel.io.rol  := rol
  // shiftResSel.io.ror  := ror
  shiftResSel.io.bclr := bclr
  shiftResSel.io.binv := binv
  shiftResSel.io.bset := bset
  shiftResSel.io.bext := bext
  val shiftRes = shiftResSel.io.shiftRes

  val aluResSel = Module(new AluResSel)
  aluResSel.io.func := func
  aluResSel.io.addSubRes := addSubRes
  aluResSel.io.shiftRes := shiftRes
  aluResSel.io.miscRes := miscRes
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
