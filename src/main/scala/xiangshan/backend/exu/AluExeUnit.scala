package xiangshan.backend.exu

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.FuType._
import xiangshan.utils._
import xiangshan.backend._
import xiangshan.backend.fu.FunctionUnit._


class AluExeUnit extends Exu(Array(aluCfg), enableBypass = false) {

  val (iovalid, src1, src2, offset, func, pc, uop) = (io.in.valid, io.in.bits.src1, io.in.bits.src2, 
    io.in.bits.uop.ctrl.imm, io.in.bits.uop.ctrl.fuOpType, SignExt(io.in.bits.uop.cf.pc, AddrBits), io.in.bits.uop)

  val redirectHit = uop.brTag.needFlush(io.redirect)
  val valid = iovalid && !redirectHit

  val isAdderSub = (func =/= ALUOpType.add) && (func =/= ALUOpType.addw) && !ALUOpType.isJump(func)
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

  val isBru = ALUOpType.isBru(func)
  // val isBranch =  io.in.bits.uop.cf.isBr// ALUOpType.isBranch(func)
  val isBranch =  ALUOpType.isBranch(func)
  val isJump = ALUOpType.isJump(func)
  val taken = LookupTree(ALUOpType.getBranchType(func), branchOpTable) ^ ALUOpType.isBranchInvert(func)
  val target = Mux(isBranch, pc + offset, adderRes)(VAddrBits-1,0)
  val isRVC = uop.cf.isRVC//(io.in.bits.cf.instr(1,0) =/= "b11".U)

  io.in.ready := io.out.ready
  val pcLatchSlot = Mux(isRVC, pc + 2.U, pc + 4.U)
  io.out.bits.redirectValid := io.out.valid && isBru//isBranch
  io.out.bits.redirect.target := Mux(!taken && isBranch, pcLatchSlot, target)
  io.out.bits.redirect.brTag := uop.brTag
  io.out.bits.redirect.isException := DontCare // false.B
  io.out.bits.redirect.roqIdx := uop.roqIdx
  io.out.bits.redirect.freelistAllocPtr := uop.freelistAllocPtr

  io.out.valid := valid
  io.out.bits.uop <> io.in.bits.uop
  io.out.bits.data := Mux(isJump, pcLatchSlot, aluRes)
  
  XSDebug(io.in.valid,
    "In(%d %d) Out(%d %d) Redirect:(%d %d %d) brTag:f:%d v:%d\n",
    io.in.valid,
    io.in.ready,
    io.out.valid,
    io.out.ready,
    io.redirect.valid,
    io.redirect.bits.isException,
    redirectHit,
    io.redirect.bits.brTag.flag,
    io.redirect.bits.brTag.value
  )
  XSDebug(io.in.valid, "src1:%x src2:%x offset:%x func:%b pc:%x\n",
    src1, src2, offset, func, pc)
  XSDebug(io.out.valid, "res:%x aluRes:%x isRVC:%d isBru:%d isBranch:%d isJump:%d target:%x taken:%d flptr:%x\n",
     io.out.bits.data, aluRes, isRVC, isBru, isBranch, isJump, target, taken, io.out.bits.uop.freelistAllocPtr.value)
}