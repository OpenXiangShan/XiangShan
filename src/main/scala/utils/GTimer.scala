package utils

import chisel3._

object GTimer {
  def apply() = {
    val c = RegInit(0.U(64.W))
    c := c + 1.U
    c
  }
}
