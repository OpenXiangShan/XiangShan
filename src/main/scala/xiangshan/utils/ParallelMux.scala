package xiangshan.utils

import chisel3._
import chisel3.util._

object ParallelOperation {
  def apply(xs: Seq[UInt], func: (UInt, UInt) => UInt): UInt = {
    xs match {
      case Seq(a) => a
      case Seq(a, b) => func(a, b)
      case _ =>
        apply(Seq(apply(xs take xs.size/2, func), apply(xs drop xs.size/2, func)), func)
    }
  }
}

object ParallelOR {
  def apply(xs: Seq[UInt]): UInt = {
    ParallelOperation(xs, (a, b) => a | b)
  }
}

object ParallelAND {
  def apply(xs: Seq[UInt]): UInt = {
    ParallelOperation(xs, (a, b) => a & b)
  }
}

object ParallelMux {
  def apply[T<:Data](in: Seq[(Bool, T)]): T = {
    val xs = in map { case (cond, x) => Fill(x.getWidth, cond) & x.asUInt() }
    ParallelOR(xs).asInstanceOf[T]
  }
}

object ParallelLookUp {
  def apply[T<:Data](key: UInt, mapping:Seq[(UInt,T)]): T = {
    ParallelMux(mapping.map(m => (m._1===key) -> m._2))
  }
}