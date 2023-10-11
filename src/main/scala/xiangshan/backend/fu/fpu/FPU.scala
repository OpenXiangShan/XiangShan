package xiangshan.backend.fu.fpu

import chisel3._
import chisel3.util._
import fudian.FloatPoint

object FPU {

  case class FType(expWidth: Int, precision: Int) {
    val sigWidth = precision - 1
    val len = expWidth + precision
  }

  val f32 = FType(8, 24)
  val f64 = FType(11, 53)

  val ftypes = List(f32, f64)

  val S = ftypes.indexOf(f32).U(log2Ceil(ftypes.length).W)
  val D = ftypes.indexOf(f64).U(log2Ceil(ftypes.length).W)

  def unbox(x: UInt, typeTag: UInt): UInt = {
    require(x.getWidth == 64)
    val isBoxed = x.head(32).andR
    Mux(typeTag === D,
      x,
      Mux(isBoxed,
        x.tail(32),
        FloatPoint.defaultNaNUInt(f32.expWidth, f32.precision)
      )
    )
  }

  def box(x: UInt, typeTag: UInt): UInt = {
    require(x.getWidth == 64)
    Mux(typeTag === D, x, Cat(~0.U(32.W), x(31, 0)))
  }

  def box(x: UInt, t: FType): UInt = {
    if(t == f32){
      Cat(~0.U(32.W), x(31, 0))
    } else if(t == f64){
      x(63, 0)
    } else {
      assert(cond = false, "Unknown ftype!")
      0.U
    }
  }

}
