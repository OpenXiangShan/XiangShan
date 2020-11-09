package xiangshan.backend.fu

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.backend._
import xiangshan.backend.fu.FunctionUnit._
import xiangshan.backend.decode.isa._

class RedirectOut extends XSBundle {
  val redirectValid = Bool()
  val redirect = new Redirect
  val brUpdate = new BranchUpdateInfo
}

class Jump extends FunctionUnit(jmpCfg, extOut = new RedirectOut) {

  val (iovalid, src1, offset, func, pc, uop) = (
    io.in.valid,
    io.in.bits.src(0),
    io.in.bits.uop.ctrl.imm,
    io.in.bits.uop.ctrl.fuOpType,
    SignExt(io.in.bits.uop.cf.pc, AddrBits),
    io.in.bits.uop
  )

  val redirectHit = uop.roqIdx.needFlush(io.redirectIn)
  val valid = iovalid && !redirectHit

  val isRVC = uop.cf.brUpdate.pd.isRVC
  val pcDelaySlot = Mux(isRVC, pc + 2.U, pc + 4.U)
  val target = src1 + offset // NOTE: src1 is (pc/rf(rs1)), src2 is (offset)

  val redirectOut = io.out.bits.ext.get.redirect
  val brUpdate = io.out.bits.ext.get.brUpdate

  io.out.bits.ext.get.redirectValid := valid
  redirectOut.pc := uop.cf.pc
  redirectOut.target := target
  redirectOut.brTag := uop.brTag
  redirectOut.isException := false.B
  redirectOut.isFlushPipe := false.B
  redirectOut.isMisPred := DontCare // check this in brq
  redirectOut.isReplay := false.B
  redirectOut.roqIdx := uop.roqIdx

  brUpdate := uop.cf.brUpdate
  brUpdate.pc := uop.cf.pc
  brUpdate.target := target
  brUpdate.brTarget := target // DontCare
  brUpdate.taken := true.B
  // io.out.bits.brUpdate.fetchIdx := uop.cf.brUpdate.fetchOffset >> 1.U  //TODO: consider RVC

  // Output
  val res = pcDelaySlot

  io.in.ready := io.out.ready
  io.out.valid := valid // TODO: CSR/MOU/FMV may need change it
  io.out.bits.uop <> io.in.bits.uop
  io.out.bits.data := res

  // NOTE: the debug info is for one-cycle exec, if FMV needs multi-cycle, may needs change it
  XSDebug(io.in.valid, "In(%d %d) Out(%d %d) Redirect:(%d %d %d %d) brTag:%x\n",
    io.in.valid,
    io.in.ready,
    io.out.valid,
    io.out.ready,
    io.redirectIn.valid,
    io.redirectIn.bits.isException,
    io.redirectIn.bits.isFlushPipe,
    redirectHit,
    io.redirectIn.bits.brTag.value
  )
  XSDebug(io.in.valid, "src1:%x offset:%x func:%b type:JUMP pc:%x res:%x\n", src1, offset, func, pc, res)
}
