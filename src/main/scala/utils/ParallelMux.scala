package utils

import chisel3._
import chisel3.util._

object ParallelOperation {
  def apply[T](xs: Seq[T], func: (T, T) => T): T = {
    require(xs.nonEmpty)
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

object ParallelORR {
  def apply(in: Seq[Bool]): Bool = ParallelOR(in)
  def apply(in: Bits): Bool = apply(in.asBools)
}

object ParallelAND {
  def apply[T <: Data](xs: Seq[T]): T = {
    ParallelOperation(xs, (a: T, b:T) => (a.asUInt() & b.asUInt()).asTypeOf(xs.head))
  }
}

object ParallelANDR {
  def apply(in: Seq[Bool]): Bool = ParallelAND(in)
  def apply(in: Bits): Bool = apply(in.asBools)
}

object ParallelXOR {
  def apply[T <: Data](xs: Seq[T]): T = {
    ParallelOperation(xs, (a: T, b:T) => (a.asUInt() ^ b.asUInt()).asTypeOf(xs.head))
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

object ParallelMax {
  def apply[T <: Data](xs: Seq[T]): T = {
    ParallelOperation(xs, (a: T, b:T) => Mux(a.asUInt() > b.asUInt(),a, b).asTypeOf(xs.head))
  }
}

object ParallelMin {
  def apply[T <: Data](xs: Seq[T]): T = {
    ParallelOperation(xs, (a: T, b:T) => Mux(a.asUInt() < b.asUInt(),a, b).asTypeOf(xs.head))
  }
}

object ParallelPriorityMux {
  def apply[T <: Data](in: Seq[(Bool, T)]): T = {
    ParallelOperation(in, (a: (Bool, T), b: (Bool, T)) => (a._1 || b._1, Mux(a._1, a._2, b._2)))._2
  }
  def apply[T <: Data](sel: Bits, in: Seq[T]): T = apply((0 until in.size).map(sel(_)), in)
  def apply[T <: Data](sel: Seq[Bool], in: Seq[T]): T = apply(sel zip in)
}

object ParallelPriorityEncoder {
  def apply(in: Seq[Bool]): UInt = ParallelPriorityMux(in, (0 until in.size).map(_.asUInt))
  def apply(in: Bits): UInt = apply(in.asBools)
}

object ParallelSingedExpandingAdd {
  def apply(in: Seq[SInt]): SInt = ParallelOperation(in, (a: SInt, b: SInt) => a +& b)
}
