package xiangshan.backend.exu

import chisel3._
import chisel3.util._
import xiangshan.backend.exu.Exu.fmacExeUnitCfg
import xiangshan.backend.fu.fpu._
import xiangshan.backend.fu.fpu.fma.FMA

class FmacExeUnit extends Exu(fmacExeUnitCfg)
{
  val frm = IO(Input(UInt(3.W)))

  val fma = supportedFunctionUnits.head.asInstanceOf[FMA]

  val input = io.fromFp.bits
  val fmaOut = fma.io.out.bits
  val isRVD = !io.fromFp.bits.uop.ctrl.isRVF
  fma.io.in.bits.src := VecInit(Seq(input.src1, input.src2, input.src3).map(
    src => Mux(isRVD, src, unboxF64ToF32(src))
  ))
  val instr_rm = io.fromFp.bits.uop.cf.instr(14, 12)
  fma.rm := Mux(instr_rm =/= 7.U, instr_rm, frm)

  fma.io.redirectIn := io.redirect
  fma.io.out.ready := io.toFp.ready

  io.toFp.bits.data := Mux(fmaOut.uop.ctrl.isRVF, boxF32ToF64(fmaOut.data), fmaOut.data)
  io.toFp.bits.fflags := fma.fflags
}
