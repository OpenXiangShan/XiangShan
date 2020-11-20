package xiangshan.backend.fu.fpu.util

import chisel3._

object ORTree {
  def apply(x: Seq[Bool]): Bool = {
    // Is 'x =/= 0' enough ?
    x.size match {
      case 1 => x.head
      case n => ORTree(x.take(n/2)) | ORTree(x.drop(n/2))
    }
  }
  def apply[T <: Bits](x: T): Bool = {
    apply(x.asBools())
  }
}
