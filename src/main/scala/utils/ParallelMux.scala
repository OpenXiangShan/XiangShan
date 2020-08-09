package utils

import chisel3._
import chisel3.util._

object ParallelOperation {
  def apply[T <: Data](xs: Seq[T], func: (T, T) => T): T = {
    xs match {
      case Seq(a) => a
      case Seq(a, b) => func(a, b)
      case _ =>
        apply(Seq(apply(xs take xs.size/2, func), apply(xs drop xs.size/2, func)), func)
    }
  }
}

object ParallelOR {
  def apply[T <: Data](xs: Seq[T]): T = {
    ParallelOperation(xs, (a: T, b: T) => (a.asUInt() | b.asUInt()).asTypeOf(xs.head))
  }
}

object ParallelAND {
  def apply[T <: Data](xs: Seq[T]): T = {
    ParallelOperation(xs, (a: T, b:T) => (a.asUInt() & b.asUInt()).asTypeOf(xs.head))
  }
}

object ParallelMux {
  def apply[T<:Data](in: Seq[(Bool, T)]): T = {
    val xs = in map { case (cond, x) => (Fill(x.getWidth, cond) & x.asUInt()).asTypeOf(in.head._2) }
    ParallelOR(xs)
  }
}

object ParallelLookUp {
  def apply[T<:Data](key: UInt, mapping:Seq[(UInt,T)]): T = {
    ParallelMux(mapping.map(m => (m._1===key) -> m._2))
  }
}