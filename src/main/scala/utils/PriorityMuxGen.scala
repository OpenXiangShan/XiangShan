package utils

import chisel3._
import chisel3.util._
import Chisel.experimental.chiselName

@chiselName
class PriorityMuxModule[T <: Data](val gen: T)(val names: Seq[String]) extends MultiIOModule {
    class InBundle extends Bundle {
        val sel = Bool()
        val src = gen.cloneType
    }
    class OutBundle extends Bundle {
        val res = gen.cloneType
    }
    val ins = names.map(s => {
        IO(Input(new InBundle)).suggestName(s)
    })
    val out = IO(Output(new OutBundle))
    out.res := ParallelPriorityMux(ins.map{i => (i.sel, i.src)})
}

// this could be used to handle the situation
// in which we have mux sources at multiple
// locations, and this is same to multiple
// when clauses as below, but collect them 
// and put them into a ParallelPrioriyMux
// when (sel1) { x := in1 }
// when (sel2) { x := in2 }
class PriorityMuxGenerator[T <: Data] {
    var src: List[(Bool, T, String)] = List()
    var num: Int = 0
    def genPortName(n: Option[String]): String = {
        num = num + 1
        n match {
            case Some(name) => name
            case None => {
                "in" + num.toString()
            }
        }
    }

    def register(sel: Bool, in: T, name: Option[String] = None) = {
        src = (sel, in, genPortName(name)) :: src
    }
    def register(in: Seq[(Bool, T, Option[String])]) = {
        src = in.toList.map{ case (b, t, n) => (b, t, genPortName(n)) } ::: src
    }
    def register(sel: Seq[Bool], in: Seq[T], name: Seq[Option[String]]) = {
        src = (sel,in,name.map(genPortName)).zipped.toList ::: src
    }
    def apply(): T = {
        val names = src.map(_._3)
        val ins = src.map(s => (s._1, s._2))
        // we should use this sample data to get type and width
        // ugly
        val sample = ins(0)._2
        val ppm = Module(new PriorityMuxModule(sample)(names))
        (ppm.ins zip ins).foreach {
            case (in, (b, t)) => {
                in.sel := b
                in.src := t
            }
        }
        ppm.out.res
    }
}