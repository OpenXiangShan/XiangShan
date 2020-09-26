package xiangshan.backend.exu

import chisel3._
import xiangshan.{ExuOutput, FuType}
import xiangshan.backend.fu.{CSR, Jump}
import xiangshan.backend.decode.isa._
import utils._

class JmpExeUnit extends Exu(Exu.jmpExeUnitCfg) {

  val (valid, src1, src2, uop, fuType, func) = (io.in.valid, io.in.bits.src1, io.in.bits.src2, io.in.bits.uop, io.in.bits.uop.ctrl.fuType, io.in.bits.uop.ctrl.fuOpType)

  val jmp = Module(new Jump)
  val csr = Module(new CSR)
  val fence = Module(new FenceExeUnit)

  val isJmp = fuType === FuType.jmp
  val isCsr = fuType === FuType.csr
  val isFence = fuType === FuType.fence

  jmp.io.in.valid := io.in.valid && isJmp
  jmp.io.in.bits := io.in.bits
  jmp.io.out.ready := io.out.ready
  jmp.io.exception <> DontCare
  jmp.io.dmem <> DontCare
  jmp.io.mcommit := DontCare
  jmp.io.redirect := io.redirect

  csr.io.cfIn := io.in.bits.uop.cf
  csr.io.fpu_csr := DontCare
  csr.io.exception <> io.exception
  csr.io.instrValid := DontCare
  csr.io.out.ready := io.out.ready
  csr.io.in.bits.src3 := DontCare
  val csrOut = csr.access(
    valid = io.in.valid && fuType === FuType.csr,
    src1 = io.in.bits.src1,
    src2 = io.in.bits.src2,
    func = io.in.bits.uop.ctrl.fuOpType
  )
  // val uop = io.in.bits.uop
  val csrExuOut = Wire(new ExuOutput)
  csrExuOut.uop := uop
  csrExuOut.uop.cf := csr.io.cfOut
  csrExuOut.uop.ctrl.flushPipe := csr.io.flushPipe
  csrExuOut.data := csrOut
  csrExuOut.redirectValid := csr.io.redirectValid
  csrExuOut.redirect.brTag := uop.brTag
  csrExuOut.redirect.isException := false.B
  csrExuOut.redirect.isMisPred := false.B
  csrExuOut.redirect.isFlushPipe := false.B
  csrExuOut.redirect.isReplay := false.B
  csrExuOut.redirect.roqIdx := uop.roqIdx
  csrExuOut.redirect.target := csr.io.redirect.target
  csrExuOut.redirect.pc := uop.cf.pc
  csrExuOut.debug := DontCare
  csrExuOut.brUpdate := DontCare

  fence.io.in.valid := valid && isFence
  fence.io.in.bits := io.in.bits
  fence.io.redirect <> DontCare // io.redirect // No need for fence is the first instr
  fence.io.mcommit <> DontCare
  fence.io.exception <> DontCare
  fence.io.dmem <> DontCare
  fence.io.out.ready := io.out.ready

  // NOTE: just one instr in this module at the same time
  io.in.ready := jmp.io.in.ready && csr.io.in.ready && fence.io.in.ready 
  io.out.bits := Mux(jmp.io.out.valid, jmp.io.out.bits, Mux(csr.io.out.valid, csrExuOut, fence.io.out.bits))
  io.out.valid := jmp.io.out.valid || csr.io.out.valid || fence.io.out.valid

  XSDebug(io.in.valid, p"In(${io.in.valid} ${io.in.ready} ${jmp.io.in.ready}${csr.io.in.ready}${fence.io.in.ready}) pc:0x${Hexadecimal(io.in.bits.uop.cf.pc)} roqIdx:${io.in.bits.uop.roqIdx} fuType:b${Binary(io.in.bits.uop.ctrl.fuType)} fuOpType:b${Binary(io.in.bits.uop.ctrl.fuOpType)} isJmp:${isJmp} isCsr${isCsr} isFence:${isFence}\n")
  XSDebug(io.out.valid, p"Out(${io.out.valid} ${io.out.ready} ${jmp.io.out.valid}${csr.io.out.valid}${fence.io.out.valid}) pc:0x${Hexadecimal(io.out.bits.uop.cf.pc)} roqIdx:${io.out.bits.uop.roqIdx} fuType:b${Binary(io.out.bits.uop.ctrl.fuType)} fuOpType:b${Binary(io.out.bits.uop.ctrl.fuOpType)}\n")
}