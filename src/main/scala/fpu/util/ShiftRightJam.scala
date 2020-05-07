package fpu.util

import chisel3._
import chisel3.util._

object ShiftRightJam {
  def apply(x: UInt, shiftAmt:UInt, w: Int): UInt ={
    val xLen = if(x.getWidth < w) w else x.getWidth
    val x_ext = Wire(UInt(xLen.W))
    x_ext := (if(x.getWidth < w) Cat(x, 0.U((w-x.getWidth).W)) else x)
    val realShiftAmt = Mux(shiftAmt > (w-1).U,
      w.U,
      shiftAmt(log2Up(w) - 1, 0)
    )
    val mask = ((-1).S(xLen.W).asUInt() >> (w.U - realShiftAmt)).asUInt()
    val sticky = ORTree(mask & x_ext)
    val x_shifted = Wire(UInt(xLen.W))
    x_shifted := x_ext >> realShiftAmt
    x_shifted.head(w) | sticky
  }
}
