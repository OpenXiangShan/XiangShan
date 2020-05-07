package fpu.util

import chisel3._
import chisel3.util._

object ShiftLeftJam {
  def apply(x: UInt, shiftAmt: UInt, w:Int): UInt = {
    val xLen = if(x.getWidth < w) w else x.getWidth
    val x_shifted = Wire(UInt(xLen.W))
    x_shifted := Mux(shiftAmt > (xLen-1).U,
      0.U,
      x << shiftAmt(log2Up(xLen)-1, 0)
    )
    val sticky = ORTree(x_shifted.tail(w))
    x_shifted.head(w) | sticky
  }
}
