package xiangshan.backend.vector.util

import chisel3._

abstract class NamedDef(w: Int) extends Bundle {
  protected val value = UInt(w.W)

  def is(f: this.type => UInt): Bool = {
    f(this) === this.value
  }
}

abstract class NamedDefObj[T <: Bundle](gen: => T) {
  def apply(): T = gen
}
