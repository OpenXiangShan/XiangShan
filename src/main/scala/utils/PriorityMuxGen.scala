package utils

import chisel3._
import chisel3.util._

// this could be used to handle the situation
// in which we have mux sources at multiple
// locations, and this is same to multiple
// when clauses as below, but collect them 
// and put them into a ParallelPrioriyMux
// when (sel1) { x := in1 }
// when (sel2) { x := in2 }
class PriorityMuxGenerator[T <: Data] {
    var src: List[(Bool, T)] = List()
    def register(sel: Bool, in: T) = src = (sel, in) :: src
    def register(in: Seq[(Bool, T)]) = src = in.toList ::: src
    def register(sel: Seq[Bool], in: Seq[T]) = src = (sel zip in).toList ::: src
    def apply(): T = ParallelPriorityMux(src)
}