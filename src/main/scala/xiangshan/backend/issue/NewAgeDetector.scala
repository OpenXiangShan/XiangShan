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

class NewAgeDetector(numEntries: Int, numEnq: Int, regOut: Boolean = true)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val enq = Vec(numEnq, Input(Bool()))
    val clear = Input(UInt(numEntries.W))
    val out = Output(UInt(numEntries.W))
  })

  // age(i)(j): entry i enters queue before entry j
  val age = Seq.fill(numEntries)(Seq.fill(numEntries)(RegInit(false.B)))
  val nextAge = Seq.fill(numEntries)(Seq.fill(numEntries)(Wire(Bool())))

  // to reduce reg usage, only use upper matrix
  def get_age(row: Int, col: Int): Bool = if (row <= col) age(row)(col) else !age(col)(row)
  def get_next_age(row: Int, col: Int): Bool = if (row <= col) nextAge(row)(col) else !nextAge(col)(row)
  def isFlushed(i: Int): Bool = io.clear(i)

  //only for row <= col
  for((row, i) <- nextAge.zipWithIndex) {
    for((elem, j) <- row.zipWithIndex) {
      if (i <= j) {
        when(io.enq(i)){
          elem := io.enq(j) || isFlushed(j) || !get_age(j, j)
        }.elsewhen(isFlushed(i)) {
          elem := false.B
        }.otherwise {
          elem := (!io.enq(j) && !isFlushed(j) && get_age(i, j)) || (!io.enq(j) && isFlushed(j)) || (io.enq(j) && get_age(i, i))
        }
      } else {
        elem := !nextAge(j)(i)
      }
      age(i)(j) := elem
    }
  }

  def getOldest(get: (Int, Int) => Bool): UInt = {
    VecInit((0 until numEntries).map(i => {
      VecInit((0 until numEntries).map(j => get(i, j))).asUInt.andR
    })).asUInt
  }

  val best = getOldest(get_age)
  val nextBest = getOldest(get_next_age)

  io.out := (if (regOut) best else nextBest)

  val ageMatrix = VecInit(age.map(v => VecInit(v).asUInt.andR)).asUInt
  val symmetricAge = RegNext(nextBest)
  XSError(ageMatrix =/= symmetricAge, p"age error between ${Hexadecimal(ageMatrix)} and ${Hexadecimal(symmetricAge)}\n")
}

object NewAgeDetector {
  def apply(numEntries: Int, enq: Vec[Bool], clear: Vec[Bool], canIssue: UInt)(implicit p: Parameters): Valid[UInt] = {
    val age = Module(new NewAgeDetector(numEntries, enq.length, regOut = true))
    age.io.enq := enq
    age.io.clear := clear.asUInt
    val out = Wire(Valid(UInt(clear.getWidth.W)))
    out.valid := (canIssue & age.io.out).orR
    out.bits := age.io.out
    out
  }
}