package xiangshan.backend.exu

import chisel3._
import chisel3.util._
import xiangshan.backend.exu.Exu.fmacExeUnitCfg
import xiangshan.backend.fu.fpu.fma.FMA

class FmacExeUnit extends Exu(fmacExeUnitCfg) {

  val fma = Module(new FMA)

  fma.io.in.valid := io.in.valid

  fma.io.in.bits.uop := io.in.bits.uop
  fma.io.in.bits.src(0) := io.in.bits.src1
  fma.io.in.bits.src(1) := io.in.bits.src2
  fma.io.in.bits.src(2) := io.in.bits.src3
  fma.io.in.bits.ext.get <> DontCare

  fma.io.redirectIn := io.redirect
  fma.io.out.ready := io.out.ready

  io.in.ready := fma.io.in.ready
  io.out.valid := fma.io.out.valid
  io.out.bits.uop := fma.io.out.bits.uop
  io.out.bits.data := fma.io.out.bits.data
  io.out.bits.redirectValid := false.B
  io.out.bits.redirect <> DontCare

}
