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

package xiangshan.frontend.bpu.replacer

import chisel3._
import chisel3.util._
import utility.LFSR64
import utility.XSError
import xiangshan.frontend.bpu.CompareMatrix

class RripStateGen(
    NumWays:              Int,
    AccessSize:           Int = 1,
    UseFrequencyPriority: Boolean = true
) extends ReplacerStateGen(NumWays, AccessSize) {
  def RrpvWidth:  Int = log2Ceil(NumWays)
  def StateWidth: Int = NumWays * RrpvWidth
  def MaxRrpv:    Int = (1 << RrpvWidth) - 1
  def InitRrpv:   Int = MaxRrpv - 1

  private lazy val random = LFSR64()
  private def genRripState(state: UInt): Vec[UInt] =
    VecInit.tabulate(NumWays)(i => state((i + 1) * RrpvWidth - 1, i * RrpvWidth))

  private def genState(rripState: Vec[UInt]): UInt =
    Cat(rripState.reverse)

  // we consider a valid touch means the way is hit
  // if no way is hit, we consider it a miss
  override def getNextState(state: UInt, touchWays: Vec[ValidIO[UInt]]): UInt = {
    val rripState = genRripState(state)
    val nextState = WireInit(rripState)

    val touchWayMask = VecInit.tabulate(NumWays) { i =>
      touchWays.map(touchWay => touchWay.valid && (touchWay.bits === i.U)).reduce(_ || _)
    }
    val miss           = !touchWayMask.reduce(_ || _)
    val victim         = getVictim(state)
    val victimRrpv     = rripState(victim)
    val incrementValue = MaxRrpv.U - victimRrpv

    nextState.zipWithIndex.foreach { case (next, i) =>
      when(miss) {
        next := Mux(
          i.U === victim,
          InitRrpv.U,
          rripState(i) + incrementValue
        )
      }.otherwise {
        if (UseFrequencyPriority) {
          next := Mux(
            touchWayMask(i),
            Mux(rripState(i) =/= 0.U, rripState(i) - 1.U, 0.U),
            rripState(i)
          )
        } else {
          next := Mux(
            touchWayMask(i),
            0.U,
            rripState(i)
          )
        }
      }
    }
    genState(nextState)
  }

  def getNextState(state: UInt, touchWay: UInt): UInt = {
    assert(false.B, "Do not use this method. Use getNextState with touchWays instead.")
    0.U
  }

  def getVictim(state: UInt): UInt = {
    val rripState    = genRripState(state)
    val victimMatrix = CompareMatrix(rripState, order = (a: UInt, b: UInt) => a > b)
    val victimWayOH  = victimMatrix.getLeastElementOH(VecInit.fill(NumWays)(true.B))
    // if all rrip states are equal, victimWay will be NumWay-1
    // so in this case we choose a random way as victim
    val isStateSame = rripState.map(_ === rripState.head).reduce(_ && _)

    Mux(!isStateSame, OHToUInt(victimWayOH), random(log2Ceil(NumWays) - 1, 0))
  }
}
