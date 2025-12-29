// Copyright (c) 2024-2025 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2025 Institute of Computing Technology, Chinese Academy of Sciences
// Copyright (c) 2020-2021 Peng Cheng Laboratory
//
// XiangShan is licensed under Mulan PSL v2.
// You can use this software according to the terms and conditions of the Mulan PSL v2.
// You may obtain a copy of Mulan PSL v2 at:
//          https://license.coscl.org.cn/MulanPSL2
//
// THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
// EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
// MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
//
// See the Mulan PSL v2 for more details.

// The design of this file is based on the implementation of PseudoLRU in rock-chip at:
//          https://github.com/chipsalliance/rocket-chip/blob/master/src/main/scala/util/Replacement.scala
// See LICENSE.Berkeley for license details at:
//          https://github.com/chipsalliance/rocket-chip/blob/master/LICENSE.Berkeley
// See LICENSE.SiFive for license details at:
//          https://github.com/chipsalliance/rocket-chip/blob/master/LICENSE.SiFive

package xiangshan.frontend.bpu

import chisel3._
import chisel3.util._

class LruStateGen(val n_ways: Int, val accessSize: Int = 1) extends Module {
  def nBits: Int = n_ways * (n_ways - 1) / 2

  class LruStateGenIO extends Bundle {
    val stateIn:    UInt             = Input(UInt(nBits.W))
    val touchWays:  Vec[Valid[UInt]] = Input(Vec(accessSize, Valid(UInt(log2Ceil(n_ways).W))))
    val nextState:  UInt             = Output(UInt(nBits.W))
    val replaceWay: UInt             = Output(UInt(log2Ceil(n_ways).W))
  }
  val io: LruStateGenIO = IO(new LruStateGenIO)

  private def extractMRUVec(state: UInt): Seq[UInt] = {
    // Extract per-way information about which higher-indexed ways are more recently used
    val moreRecentVec = Wire(Vec(n_ways - 1, UInt(n_ways.W)))
    var lsb           = 0
    for (i <- 0 until n_ways - 1) {
      moreRecentVec(i) := Cat(state(lsb + n_ways - i - 2, lsb), 0.U((i + 1).W))
      lsb = lsb + (n_ways - i - 1)
    }
    moreRecentVec
  }

  def getNextState(state: UInt, touchWays: Seq[Valid[UInt]]): UInt =
    touchWays.foldLeft(state)((prev, touchWay) => Mux(touchWay.valid, getNextState(prev, touchWay.bits), prev))

  def getNextState(state: UInt, touchWay: UInt): UInt = {
    val nextState     = Wire(Vec(n_ways - 1, UInt(n_ways.W)))
    val moreRecentVec = extractMRUVec(state) // reconstruct lower triangular matrix
    val wayDec        = UIntToOH(touchWay, n_ways)

    // Compute next value of triangular matrix
    // set the touched way as more recent than every other way
    nextState.zipWithIndex.foreach { case (e, i) =>
      e := Mux(i.U === touchWay, 0.U(n_ways.W), moreRecentVec(i) | wayDec)
    }

    nextState.zipWithIndex.tail.foldLeft((nextState.head.apply(n_ways - 1, 1), 0)) { case ((pe, pi), (ce, ci)) =>
      (Cat(ce.apply(n_ways - 1, ci + 1), pe), ci)
    }._1

  }

  def getReplaceWay(state: UInt): UInt = {
    val moreRecentVec = extractMRUVec(state) // reconstruct lower triangular matrix
    // For each way, determine if all other ways are more recent
    val mruWayDec = (0 until n_ways).map { i =>
      val upperMoreRecent = if (i == n_ways - 1) true.B else moreRecentVec(i).apply(n_ways - 1, i + 1).andR
      val lowerMoreRecent = if (i == 0) true.B else moreRecentVec.map(e => !e(i)).reduce(_ && _)
      upperMoreRecent && lowerMoreRecent
    }
    OHToUInt(mruWayDec)
  }

  def stateRead: UInt = if (nBits == 0) 0.U else io.stateIn

  io.replaceWay := getReplaceWay(stateRead)
  io.nextState  := getNextState(stateRead, io.touchWays)
}
