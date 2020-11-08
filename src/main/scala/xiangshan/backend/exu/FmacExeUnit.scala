package xiangshan.backend.exu

import chisel3._
import chisel3.util._
import xiangshan.backend.fu.FunctionUnit
import xiangshan.backend.fu.fpu._

class FmacExeUnit extends Exu(
  exuName = "FmacExeUnit",
  fuGen = Seq(
    (FunctionUnit.fmac _, (_:FunctionUnit) => true.B)
  ),
  wbIntPriority = Int.MaxValue,
  wbFpPriority = 0
)
{
  val frm = IO(Input(UInt(3.W)))

  val fma = supportedFunctionUnits.head

  val input = io.in.bits
  val fmaOut = fma.io.out.bits
  val isRVD = !io.in.bits.uop.ctrl.isRVF
  fma.io.in.bits.src := VecInit(Seq(input.src1, input.src2, input.src3).map(
    src => Mux(isRVD, src, unboxF64ToF32(src))
  ))
  val instr_rm = io.in.bits.uop.cf.instr(14, 12)
  fma.rm := Mux(instr_rm =/= 7.U, instr_rm, frm)

  fma.io.redirectIn := io.redirect
  fma.io.out.ready := io.out.ready

  io.out.bits.data := Mux(fmaOut.uop.ctrl.isRVF, boxF32ToF64(fmaOut.data), fmaOut.data)
  io.out.bits.fflags := fma.fflags
}
