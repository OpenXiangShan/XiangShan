package xiangshan.backend.fu

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.utils._
import xiangshan.backend._
import xiangshan.backend.fu.FunctionUnit._

class Jump extends FunctionUnit(jmpCfg){
  val io = IO(new ExuIO)

  override def toString: String = "Bru"

  val (iovalid, src1, offset, func, pc, uop) = (io.in.valid, io.in.bits.src1, io.in.bits.uop.ctrl.imm, io.in.bits.uop.ctrl.fuOpType, SignExt(io.in.bits.uop.cf.pc, AddrBits), io.in.bits.uop)

  val redirectHit = uop.brTag.needFlush(io.redirect)
  val valid = iovalid && !redirectHit

  val isCSR = JumpOpType.isCSR(func)
  val isFMV = JumpOpType.isFMV(func)
  val isMOU = JumpOpType.isMOU(func)
  val isJUMP = JumpOpType.isJUMP(func)

  // JUMP
  val isRVC = uop.cf.isRVC
  val pcDelaySlot = Mux(isRVC, pc + 2.U, pc + 4.U)
  val target = src1 + offset // NOTE: src1 is (pc/rf(rs1)), src2 is (offset)

  io.out.bits.redirectValid := valid && isJUMP
  io.out.bits.redirect.target := target
  io.out.bits.redirect.brTag := uop.brTag
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
