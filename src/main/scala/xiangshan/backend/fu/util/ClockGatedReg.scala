package xiangshan.backend.fu.util

import chisel3._
import chisel3.util._


object GatedValidSignal {
  def apply(next: Bool, init: Bool = false.B): Bool = {
    val last = Wire(Bool())
    last := RegEnable(next, init, next || last)
    last
  }
}
