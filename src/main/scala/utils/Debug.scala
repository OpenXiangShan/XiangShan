package xiangshan.utils

import chisel3._
import xiangshan.XSConfig

object Debug {
  def apply(flag: Boolean = XSConfig().EnableDebug, cond: Bool = true.B)(body: => Unit): Any =
    if (flag) { when (cond) { body } }
}

object ShowType {
  def apply[T: Manifest](t: T) = println(manifest[T])
}
