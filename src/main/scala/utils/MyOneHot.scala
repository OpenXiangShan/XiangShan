package utils

import chisel3._
import chisel3.util._

object OHToUIntStartOne {
  def apply(in: UInt): UInt = {
    OHToUInt(in << 1)
  }
}

/*
object OHToUIntEndMost {
  def apply(in: UInt): UInt = {
    val res = UInt(64.W)
    when(in.orR){
      res := OHToUInt(in << 1)
    }.otherwise{
      res := 1.U << in.getWidth
    }
    res
  }
}
*/
