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

// The design of this file is based on the implementation of TrueLRU in rock-chip at:
//          https://github.com/chipsalliance/rocket-chip/blob/master/src/main/scala/util/Replacement.scala
// See LICENSE.Berkeley for license details at:
//          https://github.com/chipsalliance/rocket-chip/blob/master/LICENSE.Berkeley
// See LICENSE.SiFive for license details at:
//          https://github.com/chipsalliance/rocket-chip/blob/master/LICENSE.SiFive

package xiangshan.frontend.bpu.replacer

import chisel3._
import chisel3.util._
import org.scalatest.flatspec.AnyFlatSpec

class LruStateGen(NumWays: Int, AccessSize: Int = 1) extends ReplacerStateGen(NumWays, AccessSize) {
  // True LRU replacement policy, using a triangular matrix to track which sets are more recently used than others.
  // The matrix is packed into a single UInt (or Bits).  Example 4-way (6-bits):
  // [5] - 3 more recent than 2
  // [4] - 3 more recent than 1
  // [3] - 2 more recent than 1
  // [2] - 3 more recent than 0
  // [1] - 2 more recent than 0
  // [0] - 1 more recent than 0
  def StateWidth: Int = NumWays * (NumWays - 1) / 2

  private def extractMRUVec(state: UInt): Seq[UInt] = {
    // Extract per-way information about which higher-indexed ways are more recently used
    val moreRecentVec = Wire(Vec(NumWays - 1, UInt(NumWays.W)))
    var lsb           = 0
    for (i <- 0 until NumWays - 1) {
      moreRecentVec(i) := Cat(state(lsb + NumWays - i - 2, lsb), 0.U((i + 1).W))
      lsb = lsb + (NumWays - i - 1)
    }
    moreRecentVec
  }

  def getNextState(state: UInt, touchWays: Seq[Valid[UInt]]): UInt =
    touchWays.foldLeft(state)((prev, touchWay) => Mux(touchWay.valid, getNextState(prev, touchWay.bits), prev))

  def getNextState(state: UInt, touchWay: UInt): UInt = {
    val nextState     = Wire(Vec(NumWays - 1, UInt(NumWays.W)))
    val moreRecentVec = extractMRUVec(state) // reconstruct lower triangular matrix
    val wayDec        = UIntToOH(touchWay, NumWays)

    // Compute next value of triangular matrix
    // set the touched way as more recent than every other way
    nextState.zipWithIndex.foreach { case (e, i) =>
      e := Mux(i.U === touchWay, 0.U(NumWays.W), moreRecentVec(i) | wayDec)
    }

    nextState.zipWithIndex.tail.foldLeft((nextState.head.apply(NumWays - 1, 1), 0)) { case ((pe, pi), (ce, ci)) =>
      (Cat(ce.apply(NumWays - 1, ci + 1), pe), ci)
    }._1

  }

  def getVictim(state: UInt): UInt = {
    val moreRecentVec = extractMRUVec(state) // reconstruct lower triangular matrix
    // For each way, determine if all other ways are more recent
    val mruWayDec = (0 until NumWays).map { i =>
      val upperMoreRecent = if (i == NumWays - 1) true.B else moreRecentVec(i).apply(NumWays - 1, i + 1).andR
      val lowerMoreRecent = if (i == 0) true.B else moreRecentVec.map(e => !e(i)).reduce(_ && _)
      upperMoreRecent && lowerMoreRecent
    }
    OHToUInt(mruWayDec)
  }
}

class LruStateGenTest extends AnyFlatSpec {
  import chisel3.simulator.EphemeralSimulator._ // scalastyle:ignore
  import scala.util.Random

  private val initState  = Seq(0, 1, 2, 3) // here, left (lower index) is MRU
  private val random     = new Random()
  private val numWays    = 4
  private val accessSize = 2

  // compose state from lru list
  private def getState(lruList: Seq[Int]): Int = {
    require(lruList.toSet == lruList.indices.toSet, "lruList must be a permutation of all ways")
    var state = 0
    var pos   = 0
    for (i <- lruList.indices) {
      for (j <- i + 1 until lruList.length) {
        if (lruList.indexOf(i) > lruList.indexOf(j)) { // left (lower index) is more recent than right (higher index)
          state |= (1 << pos)
        }
        pos += 1
      }
    }
    state
  }

  // get next lru list after touching `touch`
  // i.e. put touched way to the lowest index (MRU)
  private def getNext(lruList: Seq[Int], touch: Int): Seq[Int] =
    touch +: lruList.filter(_ != touch)

  private def display(lruList: Seq[Int]): String = {
    val width  = numWays * (numWays - 1) / 2
    val bin    = getState(lruList).toBinaryString
    val padded = ("0" * math.max(0, width - bin.length)) + bin
    f"${padded} (${lruList.mkString(">")})"
  }

  // test for test
  assert(getState(initState) == 0)
  assert(getNext(initState, 2) == Seq(2, 0, 1, 3))

  behavior of "LruStateGen"
  it should "work correctly" in {
    simulate(new LruStateGen(numWays, accessSize)) { dut =>
      var state = initState

      def testSingle(touch: Int): Unit = {
        dut.io.state.poke(getState(state).U)
        dut.io.touches(0).valid.poke(true.B)
        dut.io.touches(0).bits.poke(touch.U)
        dut.io.touches(1).valid.poke(false.B)
        val nextState = getNext(state, touch)
        dut.io.nextState.expect(getState(nextState).U)
        print(f"${display(state)} + ${touch} -> ${display(nextState)} pass\n")
        state = nextState
      }
      for (_ <- 0 until 100) {
        testSingle(random.nextInt(numWays))
      }

      def testMulti(touch1: Int, touch2: Int): Unit = {
        dut.io.state.poke(getState(state).U)
        dut.io.touches(0).valid.poke(true.B)
        dut.io.touches(0).bits.poke(touch1.U)
        dut.io.touches(1).valid.poke(true.B)
        dut.io.touches(1).bits.poke(touch2.U)
        val nextState = getNext(getNext(state, touch1), touch2)
        dut.io.nextState.expect(getState(nextState).U)
        print(f"${display(state)} + ${touch1} + ${touch2} -> ${display(nextState)} pass\n")
        state = nextState
      }
      for (_ <- 0 until 100) {
        testMulti(random.nextInt(numWays), random.nextInt(numWays))
      }
    }
  }
}
