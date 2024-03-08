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

class NewAgeDetector(numEntries: Int, numEnq: Int, numDeq: Int)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val enq = Vec(numEnq, Input(Bool()))
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

  //only for row <= col
  for((row, i) <- nextAge.zipWithIndex) {
    for((elem, j) <- row.zipWithIndex) {
      if (i == j) {
        // an entry is always older than itself
        elem := true.B
      }
      else if (i < j) {
        when (io.enq(j)) {
          // (1) when entry j enqueues from port k,
          // (1.1) if entry i (<j) enqueues from previous ports, it is older than entry j
          // (1.2) if entry i does not enqueue, set col(j) older than entry j
          elem := true.B
        }.elsewhen (io.enq(i)) {
          // (2) when entry i enqueues, set row(i) to false
          elem := false.B
        }.otherwise {
          // default: unchanged
          elem := get_age(i, j)
        }
      }
      else {
        elem := !nextAge(j)(i)
      }
      age(i)(j) := Mux(io.enq(i) | io.enq(j), elem, age(i)(j))
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
}

object NewAgeDetector {
  def apply(numEntries: Int, enq: Vec[Bool], canIssue: Vec[UInt])(implicit p: Parameters): Vec[Valid[UInt]] = {
    val age = Module(new NewAgeDetector(numEntries, enq.length, canIssue.length))
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
}