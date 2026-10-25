package xiangshan.backend.vector.util

import chisel3._

/** Compact encoded field whose subclasses provide named literals and equality predicates. */
abstract class NamedDef(w: Int) extends Bundle {
  protected val value = UInt(w.W)

  def is(f: this.type => UInt): Bool = {
    f(this) === this.value
  }
}

/** Companion-style factory; evaluate gen afresh for each hardware field instance. */
abstract class NamedDefObj[T <: Bundle](gen: => T) {
  def apply(): T = gen
}
