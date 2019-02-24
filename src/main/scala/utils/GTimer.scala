package utils

import chisel3._
import chisel3.util._

object GTimer {
  def apply() = Counter(true.B, 0x7fffffff)._1
}
