package fpu.util

import chisel3._

object FPUDebug {
  // don't care GTimer in FPU tests
  def apply(flag: Boolean = false, cond: Bool = true.B)(body: => Unit): Any =
    if (flag) { when (cond) { body } }
}
