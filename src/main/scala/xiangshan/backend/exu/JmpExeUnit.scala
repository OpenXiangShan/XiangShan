package xiangshan.backend.exu

import chisel3._
import chisel3.util.experimental.BoringUtils
import xiangshan.{ExuOutput, FuType}
import xiangshan.backend.fu.{CSR, Jump}
import xiangshan.backend.decode.isa._
import xiangshan.backend.fu.fpu.Fflags
import utils._

class JmpExeUnit extends Exu(Exu.jmpExeUnitCfg) {

  val (valid, src1, src2, uop, fuType, func) = (io.in.valid, io.in.bits.src1, io.in.bits.src2, io.in.bits.uop, io.in.bits.uop.ctrl.fuType, io.in.bits.uop.ctrl.fuOpType)

  val jmp = Module(new Jump)
  val csr = Module(new CSR)
  val fence = Module(new FenceExeUnit)
  val i2f = Module(new I2fExeUnit)

  fence.io.csrOnly <> DontCare
  i2f.io.csrOnly <> DontCare

  val isJmp = fuType === FuType.jmp
  val isCsr = fuType === FuType.csr
  val isFence = fuType === FuType.fence
  val isI2f = fuType === FuType.i2f

  jmp.io <> DontCare
  jmp.io.in.valid := io.in.valid && isJmp
  jmp.io.out.ready := io.out.ready
//<<<<<<< HEAD
//
  jmp.io.in.bits.connectToExuInput(io.in.bits)
  jmp.io.redirectIn := io.redirect

  val jumpExuOut = Wire(new ExuOutput)
  val jumpExtraOut = jmp.io.out.bits.ext.get

  jumpExuOut.uop := uop
  jumpExuOut.data := jmp.io.out.bits.data
  jumpExuOut.brUpdate := jumpExtraOut.brUpdate
  jumpExuOut.fflags := DontCare
  jumpExuOut.redirect := jumpExtraOut.redirect
  jumpExuOut.redirectValid := jumpExtraOut.redirectValid
  jumpExuOut.debug := DontCare


  val fflags = WireInit(0.U.asTypeOf(new Fflags))
  val dirty_fs = WireInit(false.B)
  BoringUtils.addSink(fflags, "Fflags")
  BoringUtils.addSink(dirty_fs, "DirtyFs")
  val frm = WireInit(0.U(3.W))
  frm := csr.io.fpu_csr.frm
  BoringUtils.addSource(frm, "Frm")

  csr.io.cfIn := io.in.bits.uop.cf
  csr.io.fpu_csr.fflags := fflags
  csr.io.fpu_csr.isIllegal := false.B // TODO: check illegal rounding mode
  csr.io.fpu_csr.dirty_fs := dirty_fs
  csr.io.instrValid := DontCare
  csr.io.out.ready := io.out.ready
  csr.io.in.valid  := io.in.valid && isCsr
  csr.io.in.bits.ext.get := io.in.bits.uop.ctrl.fuOpType
  csr.io.in.bits.connectToExuInput(io.in.bits)
  csr.io.redirectIn := io.redirect
  val csrOut = csr.io.out.bits.data


  csr.io.perf <> DontCare

  csr.io.exception := io.csrOnly.exception
  csr.io.memExceptionVAddr := io.csrOnly.memExceptionVAddr
  io.csrOnly.trapTarget := csr.io.trapTarget
  csr.io.mtip := io.csrOnly.externalInterrupt.mtip
  csr.io.msip := io.csrOnly.externalInterrupt.msip
  csr.io.meip := io.csrOnly.externalInterrupt.meip
  io.csrOnly.interrupt := csr.io.interrupt

  // val uop = io.in.bits.uop
  val csrExuOut = Wire(new ExuOutput)
  csrExuOut.uop := uop
  csrExuOut.uop.cf := csr.io.cfOut
  csrExuOut.uop.ctrl.flushPipe := csr.io.flushPipe
  csrExuOut.data := csrOut
  csrExuOut.fflags := DontCare
  csrExuOut.redirectValid := csr.io.redirectOutValid
  csrExuOut.redirect.brTag := uop.brTag
  csrExuOut.redirect.isException := false.B
  csrExuOut.redirect.isMisPred := false.B
  csrExuOut.redirect.isFlushPipe := false.B
  csrExuOut.redirect.isReplay := false.B
  csrExuOut.redirect.roqIdx := uop.roqIdx
  csrExuOut.redirect.target := csr.io.redirectOut.target
  csrExuOut.redirect.pc := uop.cf.pc
  csrExuOut.debug := DontCare
  csrExuOut.brUpdate := DontCare

  fence.io <> DontCare
  fence.io.in.valid := valid && isFence
  fence.io.in.bits := io.in.bits
  fence.io.redirect <> DontCare // io.redirect // No need for fence is the first instr
  fence.io.mcommit <> DontCare
  fence.io.out.ready := io.out.ready

  i2f.io.in.valid := valid && isI2f
  i2f.io.in.bits := io.in.bits
  i2f.io.redirect <> io.redirect
  i2f.io.mcommit <> DontCare
  i2f.io.out.ready := io.out.ready

  // NOTE: just one instr in this module at the same time
  io.in.ready := jmp.io.in.ready && csr.io.in.ready && fence.io.in.ready && i2f.io.in.ready
  io.out.bits := Mux(jmp.io.out.valid,
    jumpExuOut,
    Mux(csr.io.out.valid,
      csrExuOut,
      Mux(fence.io.out.valid,
        fence.io.out.bits,
        i2f.io.out.bits
      )
    )
  )
  io.out.valid := jmp.io.out.valid || csr.io.out.valid || fence.io.out.valid || i2f.io.out.valid

  XSDebug(io.in.valid,
    p"In(${io.in.valid} ${io.in.ready} ${jmp.io.in.ready}${csr.io.in.ready}${fence.io.in.ready}${i2f.io.in.ready}) " +
      p"pc:0x${Hexadecimal(io.in.bits.uop.cf.pc)} roqIdx:${io.in.bits.uop.roqIdx} " +
      p"fuType:b${Binary(io.in.bits.uop.ctrl.fuType)} fuOpType:b${Binary(io.in.bits.uop.ctrl.fuOpType)} " +
      p"isJmp:$isJmp isCsr$isCsr isFence:$isFence isI2f:$isI2f\n")
  XSDebug(io.out.valid,
    p"Out(${io.out.valid} ${io.out.ready} ${jmp.io.out.valid}${csr.io.out.valid}${fence.io.out.valid}${i2f.io.out.valid}) " +
      p"pc:0x${Hexadecimal(io.out.bits.uop.cf.pc)} roqIdx:${io.out.bits.uop.roqIdx} " +
      p"fuType:b${Binary(io.out.bits.uop.ctrl.fuType)} fuOpType:b${Binary(io.out.bits.uop.ctrl.fuOpType)}\n")
}
