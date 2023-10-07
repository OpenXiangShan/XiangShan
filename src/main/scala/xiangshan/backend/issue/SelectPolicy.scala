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

class SelectPolicy(params: RSParams)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    // select for enqueue
    val validVec = Input(UInt(params.numEntries.W))
    val allocate = Vec(params.numEnq, ValidIO(UInt(params.numEntries.W)))
    // select for issue
    val request = Input(UInt(params.numEntries.W))
    val grant = Vec(params.numDeq, ValidIO(UInt(params.numEntries.W)))
    // for load balance usage
    val balance = if (params.needBalance && params.numDeq == 2) {
      Some(new Bundle {
        val tick = Input(Bool())
        val out = Output(Bool())
      })
    } else None
  })

  val enqPolicy = if (params.numEnq > 2) "oddeven" else if (params.numEnq == 2) "center" else "circ"
  val emptyVec = VecInit(io.validVec.asBools.map(v => !v))
  val allocate = SelectOne(enqPolicy, emptyVec, params.numEnq)
  for (i <- 0 until params.numEnq) {
    val sel = allocate.getNthOH(i + 1)
    io.allocate(i).valid := sel._1
    io.allocate(i).bits := sel._2.asUInt

    XSError(io.allocate(i).valid && PopCount(io.allocate(i).bits) =/= 1.U,
      p"allocate vec ${Binary(io.allocate(i).bits)} is not onehot")
    XSDebug(io.allocate(i).fire, p"select for allocation: ${Binary(io.allocate(i).bits)}\n")
  }

  val deqPolicy = if (params.numDeq > 2 && params.numEntries > 32) "oddeven" else if (params.numDeq >= 2) "circ" else "naive"
  val request = io.request.asBools
  val select = SelectOne(deqPolicy, request, params.numDeq)
  val selected = (0 until params.numDeq).map(i => select.getNthOH(i + 1))
  for ((sel, i) <- selected.zipWithIndex) {
    io.grant(i).valid := sel._1
    io.grant(i).bits := sel._2.asUInt

    XSError(io.grant(i).valid && PopCount(io.grant(i).bits.asBools) =/= 1.U,
      p"grant vec ${Binary(io.grant(i).bits)} is not onehot")
    XSDebug(io.grant(i).valid, p"select for issue request: ${Binary(io.grant(i).bits)}\n")
  }

  if (io.balance.isDefined) {
    val balance = RegInit(false.B)
    when (io.balance.get.tick) {
      balance := !balance
    }
    io.balance.get.out := balance
    for ((sel, i) <- selected.reverse.zipWithIndex) {
      when (balance) {
        io.grant(i).valid := sel._1
        io.grant(i).bits := sel._2.asUInt
      }
    }
  }
}

class OldestSelection(params: RSParams)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val in = Vec(params.numDeq, Flipped(ValidIO(UInt(params.numEntries.W))))
    val oldest = Flipped(ValidIO(UInt(params.numEntries.W)))
    val canOverride = Vec(params.numDeq, Input(Bool()))
    val isOverrided = Vec(params.numDeq, Output(Bool()))
  })

  val oldestMatchVec = VecInit(io.in.map(i => i.valid && (i.bits & io.oldest.bits).asUInt.orR))
  io.isOverrided := io.canOverride.zipWithIndex.map{ case (canDo, i) =>
    // When the oldest is not matched with io.in(i), we always select the oldest.
    // We don't need to compare in(i) here, because we will select the oldest no matter in(i) matches or not.
    val oldestMatchIn = if (params.numDeq > 1) {
      VecInit(oldestMatchVec.zipWithIndex.filterNot(_._2 == i).map(_._1)).asUInt.orR
    } else false.B
    val isOverrided = canDo && io.oldest.valid && !oldestMatchIn
    XSPerfAccumulate(s"oldest_override_$i", isOverrided)
    XSPerfAccumulate(s"oldest_same_as_selected_$i", oldestMatchIn)
    isOverrided
  }
}

class AgeDetector(numEntries: Int, numEnq: Int, regOut: Boolean = true)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    // NOTE: deq and enq may come at the same cycle.
    val enq = Vec(numEnq, Input(UInt(numEntries.W)))
    val deq = Input(UInt(numEntries.W))
    val out = Output(UInt(numEntries.W))
  })

  // age(i)(j): entry i enters queue before entry j
  val age = Seq.fill(numEntries)(Seq.fill(numEntries)(RegInit(false.B)))
  val nextAge = Seq.fill(numEntries)(Seq.fill(numEntries)(Wire(Bool())))

  // to reduce reg usage, only use upper matrix
  def get_age(row: Int, col: Int): Bool = if (row <= col) age(row)(col) else !age(col)(row)
  def get_next_age(row: Int, col: Int): Bool = if (row <= col) nextAge(row)(col) else !nextAge(col)(row)
  def isFlushed(i: Int): Bool = io.deq(i)
  def isEnqueued(i: Int, numPorts: Int = -1): Bool = {
    val takePorts = if (numPorts == -1) io.enq.length else numPorts
    takePorts match {
      case 0 => false.B
      case 1 => io.enq.head(i) && !isFlushed(i)
      case n => VecInit(io.enq.take(n).map(_(i))).asUInt.orR && !isFlushed(i)
    }
  }

  for ((row, i) <- nextAge.zipWithIndex) {
    val thisValid = get_age(i, i) || isEnqueued(i)
    for ((elem, j) <- row.zipWithIndex) {
      when (isFlushed(i)) {
        // (1) when entry i is flushed or dequeues, set row(i) to false.B
        elem := false.B
      }.elsewhen (isFlushed(j)) {
        // (2) when entry j is flushed or dequeues, set column(j) to validVec
        elem := thisValid
      }.elsewhen (isEnqueued(i)) {
        // (3) when entry i enqueues from port k,
        // (3.1) if entry j enqueues from previous ports, set to false
        // (3.2) otherwise, set to true if and only of entry j is invalid
        // overall: !jEnqFromPreviousPorts && !jIsValid
        val sel = io.enq.map(_(i))
        val result = (0 until numEnq).map(k => isEnqueued(j, k))
        // why ParallelMux: sel must be one-hot since enq is one-hot
        elem := !get_age(j, j) && !ParallelMux(sel, result)
      }.otherwise {
        // default: unchanged
        elem := get_age(i, j)
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

object AgeDetector {
  def apply(numEntries: Int, enq: Vec[UInt], deq: UInt, canIssue: UInt)(implicit p: Parameters): Valid[UInt] = {
    val age = Module(new AgeDetector(numEntries, enq.length, regOut = true))
    age.io.enq := enq
    age.io.deq := deq
    val out = Wire(Valid(UInt(deq.getWidth.W)))
    out.valid := (canIssue & age.io.out).orR
    out.bits := age.io.out
    out
  }
}
