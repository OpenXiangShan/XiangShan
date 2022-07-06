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

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._

class SelectPolicy(params: RSParams)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    // select for enqueue
    val validVec = Input(UInt(params.numEntries.W))
    val allocate = Vec(params.numEnq, ValidIO(UInt(params.numEntries.W)))
    // select for issue
    val request = Input(UInt(params.numEntries.W))
    val grant = Vec(params.numDeq, ValidIO(UInt(params.numEntries.W)))
    val grantBalance = Output(Bool())
  })

  val enqPolicy = if (params.numEnq > 2) "oddeven" else "circ"
  val emptyVec = VecInit(io.validVec.asBools.map(v => !v))
  val allocate = SelectOne(enqPolicy, emptyVec, params.numEnq)
  for (i <- 0 until params.numEnq) {
    val sel = allocate.getNthOH(i + 1)
    io.allocate(i).valid := sel._1
    io.allocate(i).bits := sel._2.asUInt

    XSError(io.allocate(i).valid && PopCount(io.allocate(i).bits) =/= 1.U,
      p"allocate vec ${Binary(io.allocate(i).bits)} is not onehot")
    XSDebug(io.allocate(i).fire(), p"select for allocation: ${Binary(io.allocate(i).bits)}\n")
  }

  val deqPolicy = if (params.numDeq > 2 && params.numEntries > 32) "oddeven" else if (params.numDeq >= 2) "circ" else "naive"
  val request = io.request.asBools
  val select = SelectOne(deqPolicy, request, params.numDeq)
  for (i <- 0 until params.numDeq) {
    val sel = select.getNthOH(i + 1, params.needBalance)
    io.grant(i).valid := sel._1
    io.grant(i).bits := sel._2.asUInt

    XSError(io.grant(i).valid && PopCount(io.grant(i).bits.asBools) =/= 1.U,
      p"grant vec ${Binary(io.grant(i).bits)} is not onehot")
    XSDebug(io.grant(i).valid, p"select for issue request: ${Binary(io.grant(i).bits)}\n")
  }
  io.grantBalance := select.getBalance2

}

class OldestSelection(params: RSParams)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val in = Vec(params.numDeq, Flipped(ValidIO(UInt(params.numEntries.W))))
    val oldest = Flipped(ValidIO(UInt(params.numEntries.W)))
    val canOverride = Vec(params.numDeq, Input(Bool()))
    val isOverrided = Vec(params.numDeq, Output(Bool()))
  })

  val oldestMatchVec = VecInit(io.in.map(i => i.valid && OHToUInt(i.bits) === OHToUInt(io.oldest.bits)))
  io.isOverrided := io.canOverride.zipWithIndex.map{ case (canDo, i) =>
    // When the oldest is not matched with io.in(i), we always select the oldest.
    // We don't need to compare in(i) here, because we will select the oldest no matter in(i) matches or not.
    val oldestMatchIn = if (params.numDeq > 1) {
      VecInit(oldestMatchVec.zipWithIndex.filterNot(_._2 == i).map(_._1)).asUInt.orR
    } else false.B
    canDo && io.oldest.valid && !oldestMatchIn
  }
}

class AgeDetector(numEntries: Int, numEnq: Int, regOut: Boolean = true)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val enq = Vec(numEnq, Input(UInt(numEntries.W)))
    val deq = Input(UInt(numEntries.W))
    val out = Output(UInt(numEntries.W))
  })

  // age(i)(j): entry i enters queue before entry j
  val age = Seq.fill(numEntries)(Seq.fill(numEntries)(RegInit(false.B)))
  val nextAge = Seq.fill(numEntries)(Seq.fill(numEntries)(Wire(Bool())))

  // to reduce reg usage, only use upper matrix
  def get_age(row: Int, col: Int) = if (row <= col) age(row)(col) else !age(col)(row)
  def get_next_age(row: Int, col: Int) = if (row <= col) nextAge(row)(col) else !nextAge(col)(row)

  for ((row, i) <- nextAge.zipWithIndex) {
    // (1) when entry i is flushed or dequeues, set row(i) to false.B
    val thisFlushed = io.deq(i)
    val thisEnqueue = VecInit(io.enq.map(_(i))).asUInt.orR
    val thisValid = get_age(i, i) || thisEnqueue
    for ((elem, j) <- row.zipWithIndex) {
      // (2) when entry j is flushed or dequeues, set column(j) to validVec
      val otherFlushed = io.deq(j)
      when (thisFlushed) {
        elem := false.B
      }.elsewhen (otherFlushed) {
        elem := thisValid
      }.otherwise {
        elem := get_age(i, j)
        for (k <- 0 until numEnq) {
          when (io.enq(k)(i)) {
            // (3) when enqueue, set age to ~validVec or enqueueFromPreviousPorts
            elem := !get_age(j, j) && (if (k > 0) !VecInit(io.enq.take(k).map(_(j))).asUInt.orR else true.B)
          }
        }
      }
      age(i)(j) := elem
    }
  }

  val nextBest = VecInit((0 until numEntries).map(i => {
    VecInit((0 until numEntries).map(j => get_next_age(i, j))).asUInt.andR
  })).asUInt

  io.out := (if (regOut) RegNext(nextBest) else nextBest)

  val ageMatrix = VecInit(age.map(v => VecInit(v).asUInt.andR)).asUInt
  val symmetricAge = RegNext(nextBest)
  XSError(ageMatrix =/= symmetricAge, p"age error between ${Hexadecimal(ageMatrix)} and ${Hexadecimal(symmetricAge)}\n")
}

object AgeDetector {
  def apply(numEntries: Int, enq: Vec[UInt], deq: UInt, canIssue: UInt)(implicit p: Parameters): Valid[UInt] = {
    val age = Module(new AgeDetector(numEntries, enq.length, regOut = false))
    age.io.enq := enq.map(_ & (~deq).asUInt)
    age.io.deq := deq & (~enq.reduce(_ | _)).asUInt
    val out = Wire(Valid(UInt(deq.getWidth.W)))
    out.valid := (canIssue & age.io.out).orR
    out.bits := age.io.out
    out
  }
}
