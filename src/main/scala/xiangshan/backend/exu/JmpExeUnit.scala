package xiangshan.backend.exu

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.FuType._
import xiangshan.utils._
import xiangshan.backend.regfile.RfWritePort
import xiangshan.backend.fu.FunctionUnit._
import xiangshan.backend.BRUOpType
import xiangshan.backend.decode.isa.RV32I_BRUInstr

// NOTE: BRUOpType is at backend/package.scala

// TODO: add csr
class JmpExeUnit extends Exu(Exu.jmpExeUnitCfg) {

  val (iovalid, src1, offset, func, pc, uop) = (io.in.valid, io.in.bits.src1, io.in.bits.uop.ctrl.imm, io.in.bits.uop.ctrl.fuOpType, SignExt(io.in.bits.uop.cf.pc, AddrBits), io.in.bits.uop)

  val redirectHit = uop.brTag.needFlush(io.redirect)
  val valid = iovalid && !redirectHit

  val isCSR = BRUOpType.isCSR(func)
  val isFMV = BRUOpType.isFMV(func)
  val isMOU = BRUOpType.isMOU(func)
  val isJUMP = BRUOpType.isJUMP(func)

  // CSR

  // FMV

  // MOU

  // JUMP
  val isRVC = uop.cf.isRVC
  val pcDelaySlot = Mux(isRVC, pc + 2.U, pc + 4.U)
  val target = src1 + offset // NOTE: src1 is (pc/rf(rs1)), src2 is (offset)


  io.out.bits.redirectValid := valid && isJUMP
  io.out.bits.redirect.pc := uop.cf.pc
  io.out.bits.redirect.target := target
  io.out.bits.redirect.brTarget := target // DontCare
  io.out.bits.redirect.brTag := uop.brTag
  io.out.bits.redirect._type := LookupTree(func, RV32I_BRUInstr.bruFuncTobtbTypeTable)
  io.out.bits.redirect.taken := true.B
  io.out.bits.redirect.hist := uop.cf.hist
  io.out.bits.redirect.tageMeta := uop.cf.tageMeta
  io.out.bits.redirect.fetchIdx := uop.cf.fetchOffset >> 2.U  //TODO: consider RVC
  io.out.bits.redirect.btbVictimWay := uop.cf.btbVictimWay
  io.out.bits.redirect.btbPredCtr := uop.cf.btbPredCtr
  io.out.bits.redirect.btbHitWay := uop.cf.btbHitWay
  io.out.bits.redirect.rasSp := uop.cf.rasSp
  io.out.bits.redirect.rasTopCtr := uop.cf.rasTopCtr
  io.out.bits.redirect.isException := false.B
  io.out.bits.redirect.roqIdx := uop.roqIdx
  io.out.bits.redirect.freelistAllocPtr := uop.freelistAllocPtr

  // Output
  val resCSR = WireInit(0.U(XLEN.W)) // TODO: implement it
  val resFMV = WireInit(0.U(XLEN.W)) // TODO: implement it
  val resMOU = WireInit(0.U(XLEN.W)) // TODO: implement it
  val resJMP = pcDelaySlot
  val res = ParallelMux(
    VecInit(isCSR,  isFMV,  isMOU,  isJUMP) zip
    VecInit(resCSR, resFMV, resMOU, resJMP)
  )

  io.in.ready := io.out.ready
  io.out.valid := valid // TODO: CSR/MOU/FMV may need change it
  io.out.bits.uop <> io.in.bits.uop
  io.out.bits.data := res

  // NOTE: the debug info is for one-cycle exec, if FMV needs multi-cycle, may needs change it
  XSDebug(io.in.valid, "In(%d %d) Out(%d %d) Redirect:(%d %d %d) brTag:%x\n",
    io.in.valid, io.in.ready, io.out.valid, io.out.ready, io.redirect.valid, io.redirect.bits.isException, redirectHit, io.redirect.bits.brTag.value)
  XSDebug(io.in.valid && isCSR, "src1:%x offset:%x func:%b type:CSR pc:%x\n", src1, offset, func, pc)
  XSDebug(io.in.valid && isFMV, "src1:%x offset:%x func:%b type:FMV pc:%x\n", src1, offset, func, pc)
  XSDebug(io.in.valid && isMOU, "src1:%x offset:%x func:%b type:MOU pc:%x\n", src1, offset, func, pc)
  XSDebug(io.in.valid && isJUMP, "src1:%x offset:%x func:%b type:JUMP pc:%x\n", src1, offset, func, pc)
  XSDebug(io.in.valid, "Res:%x` CsrRes:%x FMV:%x Mou:%x Jmp:%x\n", res, resCSR, resFMV, resMOU, resJMP)
}