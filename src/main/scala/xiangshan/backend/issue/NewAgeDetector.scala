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

class NewAgeDetector(numEntries: Int, numEnq: Int)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val enq = Vec(numEnq, Input(Bool()))
    val canIssue = Input(UInt(numEntries.W))
    val out = Output(UInt(numEntries.W))
  })

  // age(i)(j): entry i enters queue before entry j
  val age = Seq.fill(numEntries)(Seq.fill(numEntries)(RegInit(false.B)))
  val nextAge = Seq.fill(numEntries)(Seq.fill(numEntries)(Wire(Bool())))

  // to reduce reg usage, only use upper matrix
  def get_age(row: Int, col: Int): Bool = if (row <= col) age(row)(col) else !age(col)(row)
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
          // (1) enq entries are put in order
          // (1.1) when entry j enqueues from port k,
          // (1.2) entry i (<j) must enqueues from previous ports,
          // (1.3) and older than entry j
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
      age(i)(j) := elem
    }
  }

  def getOldestCanIssue(get: (Int, Int) => Bool, canIssue: UInt): UInt = {
    VecInit((0 until numEntries).map(i => {
      (VecInit((0 until numEntries).map(j => get(i, j))).asUInt | ~canIssue).andR & canIssue(i)
    })).asUInt
  }

  io.out := getOldestCanIssue(get_age, io.canIssue)
}

object NewAgeDetector {
  def apply(numEntries: Int, enq: Vec[Bool], canIssue: UInt)(implicit p: Parameters): Valid[UInt] = {
    val age = Module(new NewAgeDetector(numEntries, enq.length))
    age.io.enq := enq
    age.io.canIssue := canIssue
    val out = Wire(Valid(UInt(numEntries.W)))
    out.valid := canIssue.orR
    out.bits := age.io.out
    when (out.valid) {
      assert(PopCount(out.bits) === 1.U, "out is not ont-hot when there is at least one entry can be issued\n")
    }
    out
  }
}