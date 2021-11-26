/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

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
    out.res := PriorityMux(ins.map{i => (i.sel, i.src)})
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

// this module is like PhyPriorityMuxGenerator
// but you can specify the physical priority
// by passing in an Int, usually we give
// the hightest priority to the condition
// with the largest delay.
// but their logical priority is still
// arranged in the order specified in the code
class PhyPriorityMuxGenerator[T <: Data] {
    var src: List[(Bool, T, String, Int)] = List()
    var rev_src: List[(Bool, T, String, Int)] = List()
    var sorted_src: List[(Bool, T, String, Int)] = List()
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

    def register(sel: Bool, in: T, name: Option[String] = None, phyPrio: Int = 0) = {
        src = (sel, in, genPortName(name), phyPrio) :: src
    }
    def register(in: Seq[(Bool, T, Option[String], Int)]) = {
        src = in.toList.map{ case (b, t, n, p) => (b, t, genPortName(n), p) } ::: src
    }
    def register(sel: Seq[Bool], in: Seq[T], name: Seq[Option[String]], phyPrio: Seq[Int]) = {
        src = sel.zip(in.zip(name.map(genPortName).zip(phyPrio))).map { case (s, (i, (n, p))) =>
            (s, i, n, p) }.toList ::: src
    }
    def apply(): T = {
        rev_src = src.reverse
        for (i <- 0 until rev_src.length) {
            // println(rev_src(i)._3)
            sorted_src = (rev_src(i)._1 && (if (i == rev_src.length-1) true.B else (i+1 until rev_src.length).map(j => !rev_src(j)._1).reduce(_&&_)),
                rev_src(i)._2, rev_src(i)._3, rev_src(i)._4) :: sorted_src
        }
        sorted_src = sorted_src.sortBy(_._4).reverse
        // println(sorted_src)
        val names = sorted_src.map(_._3)
        val ins = sorted_src.map(s => (s._1, s._2))
        // we should use this sample data to get type and width
        // ugly
        val sample = ins(0)._2
        // println(src)
        // println(sorted_src)
        // println(ins)
        // println(names)

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