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

package xiangshan.frontend.bpu.replacer

import chisel3._
import chisel3.util._

// the abstract class for replacer state generator
abstract class ReplacerStateGen(NumWays: Int, AccessSize: Int = 1) extends Module {
  def StateWidth: Int

  class ReplacerStateGenIO extends Bundle {
    val state:     UInt             = Input(UInt(StateWidth.W))
    val touches:   Vec[Valid[UInt]] = Input(Vec(AccessSize, Valid(UInt(log2Ceil(NumWays).W))))
    val nextState: UInt             = Output(UInt(StateWidth.W))
    val victim:    UInt             = Output(UInt(log2Ceil(NumWays).W))
  }

  val io: ReplacerStateGenIO = IO(new ReplacerStateGenIO)

  def getNextState(state: UInt, touchWays: Vec[Valid[UInt]]): UInt = {
    val touchWayMask = VecInit.tabulate(NumWays) { i =>
      touchWays.map(touchWay => touchWay.valid && (touchWay.bits === i.U)).reduce(_ || _)
    }
    val miss      = !touchWayMask.reduce(_ || _)
    val victim    = getVictim(state)
    val nextState = WireInit(state)
    when(miss) {
      nextState := getNextState(state, victim)
    }.otherwise {
      nextState := touchWays.foldLeft(state)((prev, touchWay) =>
        Mux(touchWay.valid, getNextState(prev, touchWay.bits), prev)
      )
    }
    nextState
  }

  def getNextState(state: UInt, touch: UInt): UInt

  def getVictim(state: UInt): UInt

  io.nextState := getNextState(io.state, io.touches)
  io.victim    := getVictim(io.state)
}

object ReplacerStateGen {
  def apply(algorithm: String, numWays: Int, accessSize: Int = 1): ReplacerStateGen =
    algorithm match {
      case "Plru" => new PlruStateGen(numWays, accessSize)
      case "Lru"  => new LruStateGen(numWays, accessSize)
      case "Rrip" => new RripStateGen(numWays, accessSize)
      case _      => throw new Exception(s"Unsupported replacement algorithm: $algorithm")
    }
}
