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

package xiangshan.backend.issue

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import utility._

class AgeDetector(numEntries: Int, numEnq: Int, numDeq: Int)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    // NOTE: now we do not consider deq.
    //       keeping these old invalid entries
    //       does not affect the selection of the oldest entry that can be issued.
    val enq = Vec(numEnq, Input(UInt(numEntries.W)))
    val canIssue = Vec(numDeq, Input(UInt(numEntries.W)))
    val out = Vec(numDeq, Output(UInt(numEntries.W)))
  })

  // age(i)(j): entry i enters queue before entry j
  val age = Seq.fill(numEntries)(Seq.fill(numEntries)(RegInit(false.B)))
  val nextAge = Seq.fill(numEntries)(Seq.fill(numEntries)(Wire(Bool())))

  // to reduce reg usage, only use upper matrix
  def get_age(row: Int, col: Int): Bool = {
    if (row < col)
      age(row)(col)
    else if (row == col)
      true.B
    else
      !age(col)(row)
  }
  def get_next_age(row: Int, col: Int): Bool = if (row <= col) nextAge(row)(col) else !nextAge(col)(row)
  def isEnq(i: Int): Bool = {
    VecInit(io.enq.map(_(i))).asUInt.orR
  }
  def isEnqNport(i: Int, numPorts: Int = 0): Bool = {
    numPorts match {
      case 0 => false.B
      case n => VecInit(io.enq.take(n).map(_(i))).asUInt.orR
    }
  }

  for ((row, i) <- nextAge.zipWithIndex) {
    for ((elem, j) <- row.zipWithIndex) {
      if (i == j) {
        // an entry is always older than itself
        elem := true.B
      }
      else if (i < j) {
        when (isEnq(i) && isEnq(j)) {
          // (1) when entry i enqueues from port k,
          // (1.1) if entry j enqueues from previous ports, set to false
          // (1.2) otherwise, set to true
          val sel = io.enq.map(_(i))
          val result = (0 until numEnq).map(k => isEnqNport(j, k))
          elem := !ParallelMux(sel, result)
        }.elsewhen (isEnq(i)) {
          // (2) when entry i enqueues, set row(i) to false
          elem := false.B
        }.elsewhen (isEnq(j)) {
          // (3) when entry j enqueues, set col(j) to true
          elem := true.B
        }.otherwise {
          // default: unchanged
          elem := get_age(i, j)
        }
      }
      else {
        elem := !nextAge(j)(i)
      }
      age(i)(j) := Mux(isEnq(i) | isEnq(j), elem, age(i)(j))
    }
  }

  def getOldestCanIssue(get: (Int, Int) => Bool, canIssue: UInt): UInt = {
    VecInit((0 until numEntries).map(i => {
      (VecInit((0 until numEntries).map(j => get(i, j))).asUInt | ~canIssue).andR & canIssue(i)
    })).asUInt
  }

  io.out.zip(io.canIssue).foreach { case (out, canIssue) =>
    out := getOldestCanIssue(get_age, canIssue)
  }

  for (i <- 0 until numEnq) {
    assert(PopCount(io.enq(i)) <= 1.U, s"enq port ($i) is not ont-hot\n")
  }
}

object AgeDetector {
  def apply(numEntries: Int, enq: Vec[UInt], canIssue: Vec[UInt])(implicit p: Parameters): Vec[Valid[UInt]] = {
    val age = Module(new AgeDetector(numEntries, enq.length, canIssue.length))
    age.io.enq := enq
    age.io.canIssue := canIssue
    val outVec = Wire(Vec(canIssue.length, Valid(UInt(numEntries.W))))
    outVec.zipWithIndex.foreach { case (out, i) =>
      out.valid := canIssue(i).orR
      out.bits := age.io.out(i)
      when (out.valid) {
        assert(PopCount(out.bits) === 1.U, s"out ($i) is not ont-hot when there is at least one entry can be issued\n")
      }
    }
    outVec
  }
  def apply(numEntries: Int, enq: Vec[UInt], canIssue: UInt)(implicit p: Parameters): Valid[UInt] = {
    val age = Module(new AgeDetector(numEntries, enq.length, 1))
    age.io.enq := enq
    age.io.canIssue(0) := canIssue
    val out = Wire(Valid(UInt(numEntries.W)))
    out.valid := canIssue.orR
    out.bits := age.io.out(0)
    when (out.valid) {
      assert(PopCount(out.bits) === 1.U, "out is not ont-hot when there is at least one entry can be issued\n")
    }
    out
  }
}
