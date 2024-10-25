package xiangshan.backend.fu.fpu

import chisel3._
import chisel3.util._
import fudian.FloatPoint

object FPU {

  case class FType(expWidth: Int, precision: Int) {
    val sigWidth = precision - 1
    val len = expWidth + precision
  }

  val f16 = FType(5, 11)
  val f32 = FType(8, 24)
  val f64 = FType(11, 53)

  // Appending f16 instead of pushing it from head to avoid potential encoding conflicts.
  // Todo: use fmt field encoding in riscv FP instructions instead of customized encoding.
  val ftypes = List(f32, f64, f16)
  val ftypeWidth = log2Up(ftypes.length)

  val S = ftypes.indexOf(f32).U(ftypeWidth.W)
  val D = ftypes.indexOf(f64).U(ftypeWidth.W)
  val H = ftypes.indexOf(f16).U(ftypeWidth.W)

  // Produce zero-extended FPXX data
  def unbox(x: UInt, typeTag: UInt): UInt = {
    require(x.getWidth == 64)
    require(typeTag.getWidth == ftypeWidth)
    Mux1H(Seq(
      (typeTag === D) -> x,
      (typeTag === S) -> Mux(x.head(32).andR, x(f32.len - 1, 0), FloatPoint.defaultNaNUInt(f32.expWidth, f32.precision)),
      (typeTag === H) -> Mux(x.head(48).andR, x(f16.len - 1, 0), FloatPoint.defaultNaNUInt(f16.expWidth, f16.precision)),
    ))
  }

  def box(x: UInt, typeTag: UInt): UInt = {
    require(x.getWidth == 64)
    Mux1H(Seq(
      (typeTag === D) -> x,
      (typeTag === S) -> Cat(Fill(32, 1.U(1.W)), x(f32.len - 1, 0)),
      (typeTag === H) -> Cat(Fill(48, 1.U(1.W)), x(f16.len - 1, 0)),
    ))
  }

  def box(x: UInt, t: FType): UInt = {
    if      (t == f64) x(63, 0)
    else if (t == f32) Cat(Fill(32, 1.U(1.W)), x(31, 0))
    else if (t == f16) Cat(Fill(48, 1.U(1.W)), x(15, 0))
    else {
      assert(cond = false, "Unknown ftype!")
      0.U
    }
  }

}
