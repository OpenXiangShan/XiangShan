package xiangshan.backend.exu

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import xiangshan.backend.exu.Exu.fmacExeUnitCfg
import xiangshan.backend.fu.fpu.fma.FMA
import xiangshan.backend.fu.fpu._

class FmacExeUnit extends Exu(fmacExeUnitCfg) {

  val fma = Module(new FMA)

  fma.io.in.valid := io.in.valid
  val input = io.in.bits
  val fmaOut = fma.io.out.bits
  val isRVD = !io.in.bits.uop.ctrl.isRVF
  fma.io.in.bits.src := VecInit(Seq(input.src1, input.src2, input.src3).map(src => Mux(isRVD, src, unboxF64ToF32(src))))
  fma.io.in.bits.uop := io.in.bits.uop
  val extraInput = fma.io.in.bits.ext.get
  val frm = WireInit(0.U(3.W))
  BoringUtils.addSink(frm, "Frm")
  val instr_rm = io.in.bits.uop.cf.instr(14, 12)
  extraInput.rm := Mux(instr_rm =/= 7.U, instr_rm, frm)
  extraInput.op := io.in.bits.uop.ctrl.fuOpType(2, 0)
  extraInput.isDouble := isRVD

  fma.io.redirectIn := io.redirect
  fma.io.out.ready := io.out.ready

  io.in.ready := fma.io.in.ready
  io.out.valid := fma.io.out.valid
  io.out.bits.uop := fmaOut.uop
  io.out.bits.data := Mux(fmaOut.uop.ctrl.isRVF, boxF32ToF64(fmaOut.data), fmaOut.data)
  io.out.bits.fflags := fma.io.out.bits.ext.get
  io.out.bits.redirectValid := false.B
  io.out.bits.redirect <> DontCare
  io.csrOnly <> DontCare

}
