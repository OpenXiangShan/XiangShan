package utils

import chisel3._
import chisel3.util._

object BoolStopWatch {
  def apply(start: Bool, stop: Bool) = {
    val r = RegInit(false.B)
    when (start) { r := true.B }
    when (stop) { r := false.B }
    r
  }
}

