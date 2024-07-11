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

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.{XSBundle, DebugOptionsKey, XSModule}
import utility.{ChiselMap, ParallelPriorityMux}

class DCFGKeyOuterBundle(implicit p: Parameters) extends XSBundle {
  val branchPC = UInt(VAddrBits.W)
}

class DCFGValueOuterBundle(implicit p: Parameters) extends Bundle {
  val nFused = UInt(64.W)
  val isBranch = Bool()
}

class DCFGKeyInnerBundle(implicit p: Parameters) extends XSBundle {
  val branchPC = UInt(VAddrBits.W)
}

class DCFGValueInnerBundle(implicit p: Parameters) extends Bundle {
  val instrs = UInt(64.W)
  val times = UInt(64.W)
  val cycles = UInt(64.W)
}

object DCFGCounter {
  def apply(init: Int, incre_valid: Bool, increment: UInt = 1.U, reset_valid: Bool, width: Int): UInt = {
    val counter = RegInit(init.U(width.W))
    when (incre_valid) {
      counter := counter + increment
    }
    when (reset_valid) {
      counter := init.U
    }
    counter
  }
}

object FirstValidIndex {
  def apply(valids: Seq[Bool]): (UInt, Bool) = {
    val idx = ParallelPriorityMux(valids.zipWithIndex.map{
      case (v, i) =>
        v -> i.U(log2Up(valids.size).W)
    })
    (idx, valids.reduce(_ || _))
  }
}

class XSPerfDCFGIO(width: Int)(implicit p: Parameters) extends XSBundle {
  val key = Input(Vec(width, new DCFGKeyOuterBundle))
  val value = Input(Vec(width, new DCFGValueOuterBundle))
  val commit_valid = Input(Vec(width, Bool()))
}

class XSPerfDCFG(width: Int)(implicit p: Parameters) extends XSModule {
  val io = IO(new XSPerfDCFGIO(width))

  val branch_valid = io.value.map(_.isBranch).zip(io.commit_valid).map{ case (a, b) => a && b }
  val (first_branch, first_branch_valid) = FirstValidIndex(branch_valid)
  val tail_instrs = RegInit(0.U(64.W))
  val cycle_counter = DCFGCounter(1, true.B, 1.U, branch_valid.reduce(_ || _), 64)
  val instr_counter = DCFGCounter(0, true.B, tail_instrs, first_branch_valid, 64)

  val key_inner = Wire(Vec(width, new DCFGKeyInnerBundle))
  val en_inner = Wire(Vec(width, Bool()))
  val value_inner = Wire(Vec(width, new DCFGValueInnerBundle))

  val map = ChiselMap.createTableBase("dcfg", Vec(width, new DCFGKeyInnerBundle), Vec(width, new DCFGValueInnerBundle))
  map.log(key_inner, value_inner, en_inner, "dcfg", clock, reset)

  key_inner.zip(io.key).foreach{case (inner, outer) => inner.branchPC := outer.branchPC}
  en_inner := branch_valid

  val last_reserve_instrs = Wire(Vec(width+1, UInt(64.W)))
  last_reserve_instrs(0) := tail_instrs
  value_inner.zipWithIndex.foreach{case (inner, i) =>

    inner.times := 1.U
    inner.instrs := Mux(branch_valid(i), last_reserve_instrs(i) + io.value(i).nFused, 0.U)
    // only branch_valid & first branch, set cycles to counter
    inner.cycles := 0.U

    last_reserve_instrs(i+1) := Mux(branch_valid(i),
      0.U, last_reserve_instrs(i) + Mux(io.commit_valid(i), io.value(i).nFused, 0.U))
  }

  value_inner(first_branch).cycles := cycle_counter
  tail_instrs := last_reserve_instrs(width)
}

// object XSPerfDCFG extends HasRegularPerfName {
//   def apply(perfName: String, key: Vec[DCFGKeyOuterBundle], value: Vec[DCFGValueOuterBundle], commit_valid: Vec[Bool], clock: Clock, reset: Reset)(implicit p: Parameters): Unit = {
//     val env = p(DebugOptionsKey)
//     if (env.EnablePerfDebug && !env.FPGAPlatform) {
//       val width = key.size
//       require(value.size == width && commit_valid.size == width, "key and value should have the same size")

//       val branch_valid = value.map(_.isBranch).zip(commit_valid).map{case (a, b) => a && b}
//       val (first_branch, first_branch_valid) = FirstValidIndex(branch_valid)
//       val tail_instrs = RegInit(0.U(64.W))
//       val cycle_counter = DCFGCounter(1, true.B, 1.U, branch_valid.reduce(_ || _), 64)
//       val instr_counter = DCFGCounter(0, true.B, tail_instrs, first_branch_valid, 64)

//       val key_inner = Wire(Vec(width, new DCFGKeyInnerBundle))
//       val en_inner = Wire(Vec(width, Bool()))
//       val value_inner = Wire(Vec(width, new DCFGValueInnerBundle))

//       val map = ChiselMap.createTableBase(perfName, Vec(width, new DCFGKeyInnerBundle), Vec(width, new DCFGValueInnerBundle))
//       map.log(key_inner, value_inner, en_inner, "dcfg", clock, reset)

//       key_inner.zip(key).foreach{case (inner, outer) => inner.branchPC := outer.branchPC}
//       en_inner := branch_valid

//       val last_reserve_instrs = Wire(Vec(width+1, UInt(64.W)))
//       last_reserve_instrs(0) := tail_instrs
//       value_inner.zipWithIndex.foreach{case (inner, i) =>

//         inner.times := 1.U
//         inner.instrs := Mux(branch_valid(i), last_reserve_instrs(i) + value(i).nFused, 0.U)
//         // only branch_valid & first branch, set cycles to counter
//         inner.cycles := 0.U

//         last_reserve_instrs(i+1) := Mux(branch_valid(i),
//           0.U, last_reserve_instrs(i) + Mux(commit_valid(i), value(i).nFused, 0.U))
//       }

//       value_inner(first_branch).cycles := cycle_counter
//       tail_instrs := last_reserve_instrs(width)
//     }
//   }
// }