package utils

import chisel3._
import chisel3.util._

object WordShift {
  def apply(data: UInt, wordIndex: UInt, step: Int) = (data << (wordIndex * step.U))
}
