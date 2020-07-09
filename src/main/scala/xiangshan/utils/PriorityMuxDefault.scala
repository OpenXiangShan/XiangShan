package xiangshan.utils

import chisel3._

object PriorityMuxDefault {
  def apply[T <: Data](in: Seq[(Bool, T)], default: T): T = {
    in.size match {
      case 1=>
        Mux(in.head._1, in.head._2, default)
      case _ =>
        Mux(in.head._1, in.head._2, PriorityMuxDefault(in.tail, default))
    }
  }
}

object PriorityEncoderDefault {
  def apply(in: Seq[Bool], default: UInt): UInt = {
    PriorityMuxDefault(in.zipWithIndex.map(x => x._1 -> x._2.U), default)
  }
}