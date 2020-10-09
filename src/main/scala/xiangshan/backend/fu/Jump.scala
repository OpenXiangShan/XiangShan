package xiangshan.backend.fu

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.backend._
import xiangshan.backend.fu.FunctionUnit._
import xiangshan.backend.decode.isa._

class Jump extends FunctionUnit(jmpCfg){
  val io = IO(new ExuIO)

  val (iovalid, src1, offset, func, pc, uop) = (io.in.valid, io.in.bits.src1, io.in.bits.uop.ctrl.imm, io.in.bits.uop.ctrl.fuOpType, SignExt(io.in.bits.uop.cf.pc, AddrBits), io.in.bits.uop)

  val redirectHit = uop.needFlush(io.redirect)
  val valid = iovalid && !redirectHit

  val isRVC = uop.cf.brUpdate.pd.isRVC
  val pcDelaySlot = Mux(isRVC, pc + 2.U, pc + 4.U)
  val target = src1 + offset // NOTE: src1 is (pc/rf(rs1)), src2 is (offset)

  io.out.bits.redirectValid := valid
  io.out.bits.redirect.pc := uop.cf.pc
  io.out.bits.redirect.target := target
  io.out.bits.redirect.brTag := uop.brTag
  io.out.bits.redirect.isException := false.B
  io.out.bits.redirect.isFlushPipe := false.B
  io.out.bits.redirect.isMisPred := DontCare // check this in brq
  io.out.bits.redirect.isReplay := false.B
  io.out.bits.redirect.roqIdx := uop.roqIdx

  io.out.bits.brUpdate := uop.cf.brUpdate
  io.out.bits.brUpdate.pc := uop.cf.pc
  io.out.bits.brUpdate.target := target
  io.out.bits.brUpdate.brTarget := target // DontCare
  // io.out.bits.brUpdate.btbType := LookupTree(func, RV32I_BRUInstr.bruFuncTobtbTypeTable)
  io.out.bits.brUpdate.taken := true.B
  // io.out.bits.brUpdate.fetchIdx := uop.cf.brUpdate.fetchOffset >> 1.U  //TODO: consider RVC
  io.out.bits.brUpdate.brTag := uop.brTag

  // Output
  val res = pcDelaySlot

  io.in.ready := io.out.ready
  io.out.valid := valid // TODO: CSR/MOU/FMV may need change it
  io.out.bits.uop <> io.in.bits.uop
  io.out.bits.data := res

  io.dmem <> DontCare
  io.out.bits.debug <> DontCare

  // NOTE: the debug info is for one-cycle exec, if FMV needs multi-cycle, may needs change it
  XSDebug(io.in.valid, "In(%d %d) Out(%d %d) Redirect:(%d %d %d %d) brTag:%x\n",
    io.in.valid,
    io.in.ready,
    io.out.valid,
    io.out.ready,
    io.redirect.valid,
    io.redirect.bits.isException,
    io.redirect.bits.isFlushPipe,
    redirectHit,
    io.redirect.bits.brTag.value
  )
  XSDebug(io.in.valid, "src1:%x offset:%x func:%b type:JUMP pc:%x res:%x\n", src1, offset, func, pc, res)
}
