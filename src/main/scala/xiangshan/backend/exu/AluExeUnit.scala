package xiangshan.backend.exu

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import xiangshan._
import xiangshan.FuType._
import utils._
import xiangshan.backend._
import xiangshan.backend.fu.FunctionUnit._


class AluExeUnit extends Exu(Exu.aluExeUnitCfg) {

  val (iovalid, src1, src2, offset, func, pc, uop) = (io.in.valid, io.in.bits.src1, io.in.bits.src2, 
    io.in.bits.uop.ctrl.imm, io.in.bits.uop.ctrl.fuOpType, SignExt(io.in.bits.uop.cf.pc, AddrBits), io.in.bits.uop)

  val redirectHit = uop.roqIdx.needFlush(io.redirect)
  val valid = iovalid && !redirectHit

  val isAdderSub = (func =/= ALUOpType.add) && (func =/= ALUOpType.addw)
  val adderRes = (src1 +& (src2 ^ Fill(XLEN, isAdderSub))) + isAdderSub
  val xorRes = src1 ^ src2
  val sltu = !adderRes(XLEN)
  val slt = xorRes(XLEN-1) ^ sltu

  val shsrc1 = LookupTreeDefault(func, src1, List(
    ALUOpType.srlw -> ZeroExt(src1(31,0), 64),
    ALUOpType.sraw -> SignExt(src1(31,0), 64)
  ))
  val shamt = Mux(ALUOpType.isWordOp(func), src2(4, 0), src2(5, 0))
  val res = LookupTreeDefault(func(3, 0), adderRes, List(
    ALUOpType.sll  -> ((shsrc1  << shamt)(XLEN-1, 0)),
    ALUOpType.slt  -> ZeroExt(slt, XLEN),
    ALUOpType.sltu -> ZeroExt(sltu, XLEN),
    ALUOpType.xor  -> xorRes,
    ALUOpType.srl  -> (shsrc1  >> shamt),
    ALUOpType.or   -> (src1  |  src2),
    ALUOpType.and  -> (src1  &  src2),
    ALUOpType.sra  -> ((shsrc1.asSInt >> shamt).asUInt)
  ))
  val aluRes = Mux(ALUOpType.isWordOp(func), SignExt(res(31,0), 64), res)

  val branchOpTable = List(
    ALUOpType.getBranchType(ALUOpType.beq)  -> !xorRes.orR,
    ALUOpType.getBranchType(ALUOpType.blt)  -> slt,
    ALUOpType.getBranchType(ALUOpType.bltu) -> sltu
  )

  val isBranch = uop.cf.brUpdate.pd.isBr// ALUOpType.isBranch(func)
  val isRVC = uop.cf.brUpdate.pd.isRVC//(io.in.bits.cf.instr(1,0) =/= "b11".U)
  val taken = LookupTree(ALUOpType.getBranchType(func), branchOpTable) ^ ALUOpType.isBranchInvert(func)
  val target = Mux(isBranch, pc + offset, adderRes)(VAddrBits-1,0)
  val pcLatchSlot = Mux(isRVC, pc + 2.U, pc + 4.U)

  io.out.bits.redirectValid := io.out.valid && isBranch
  io.out.bits.redirect.pc := uop.cf.pc
  io.out.bits.redirect.target := Mux(!taken && isBranch, pcLatchSlot, target)
  io.out.bits.redirect.brTag := uop.brTag
  io.out.bits.redirect.isException := false.B
  io.out.bits.redirect.isMisPred := DontCare // check this in brq
  io.out.bits.redirect.isFlushPipe := false.B
  io.out.bits.redirect.isReplay := false.B
  io.out.bits.redirect.roqIdx := uop.roqIdx

  io.out.bits.brUpdate := uop.cf.brUpdate
  // override brUpdate
  io.out.bits.brUpdate.pc := uop.cf.pc
  io.out.bits.brUpdate.target := Mux(!taken && isBranch, pcLatchSlot, target)
  io.out.bits.brUpdate.brTarget := target
  // io.out.bits.brUpdate.btbType := "b00".U
  io.out.bits.brUpdate.taken := isBranch && taken
  // io.out.bits.brUpdate.fetchIdx := uop.cf.brUpdate.fetchOffset >> 1.U  //TODO: consider RVC
  io.out.bits.brUpdate.brTag := uop.brTag

  io.in.ready := io.out.ready
  io.out.valid := valid
  io.out.bits.uop <> io.in.bits.uop
  io.out.bits.data := aluRes

  XSDebug(io.in.valid || io.redirect.valid,
    "In(%d %d) Out(%d %d) Redirect:(%d %d %d %d) brTag:f:%d v:%d\n",
    io.in.valid,
    io.in.ready,
    io.out.valid,
    io.out.ready,
    io.redirect.valid,
    io.redirect.bits.isException,
    io.redirect.bits.isFlushPipe,
    redirectHit,
    io.redirect.bits.brTag.flag,
    io.redirect.bits.brTag.value
  )
  XSDebug(io.in.valid, p"src1:${Hexadecimal(src1)} src2:${Hexadecimal(src2)} offset:${Hexadecimal(offset)} func:${Binary(func)} pc:${Hexadecimal(pc)} roqIdx:${uop.roqIdx}\n")
  XSDebug(io.out.valid, p"res:${Hexadecimal(io.out.bits.data)} aluRes:${Hexadecimal(aluRes)} isRVC:${isRVC} isBranch:${isBranch} target:${Hexadecimal(target)} taken:${taken}\n")
}