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

package xiangshan.frontend.bpu.mbtb.prefetch

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.frontend.bpu.replacer.ReplacerState
import xiangshan.frontend.bpu.replacer.ReplacerStateGen

class PrefetchBtbReplacer(implicit p: Parameters) extends PrefetchBtbModule {
  class PrefetchBtbReplacerIO extends Bundle {
    class Touch extends Bundle {
      val setIdx:  UInt = UInt(SetIdxLen.W)
      val wayMask: UInt = UInt(NumWay.W)
    }
    // may has at least one victim
    class Victim extends Bundle {
      val wayMask: UInt = UInt(NumWay.W)
    }
    val writeMask: ValidIO[UInt]     = Flipped(Valid(UInt(NumWay.W)))
    val victim:    Victim            = Output(new Victim)
    val touch:     Vec[Valid[Touch]] = Vec(2, Flipped(Valid(new Touch))) // magic number 2: predict and train

    def predictTouch: Valid[Touch] = touch(0)
    def writeTouch:   Valid[Touch] = touch(1)
  }

  val io: PrefetchBtbReplacerIO = IO(new PrefetchBtbReplacerIO)

  private val predictStateGen = Module(ReplacerStateGen(Replacer, NumWay, accessSize = NumWay))
  private val writeStateGen   = Module(ReplacerStateGen(Replacer, NumWay, accessSize = NumWay))
  private val stateBank       = Module(new ReplacerState(NumSets, predictStateGen.StateWidth))

  /* *** predict *** */
  // read current state
  stateBank.io.predictReadSetIdx := io.predictTouch.bits.setIdx
  private val predictState = stateBank.io.predictReadState

  // compose touch way vec
  private val predictTouchWay = VecInit((0 until NumWay).map { i =>
    val wayValid = Wire(Valid(UInt(log2Up(NumWay).W)))
    wayValid.valid := io.predictTouch.valid && io.predictTouch.bits.wayMask(i)
    wayValid.bits  := i.U
    wayValid
  })

  // generate next state
  predictStateGen.io.state    := predictState
  predictStateGen.io.touches  := predictTouchWay
  predictStateGen.io.writeCnt := 0.U
  private val predictNextState = Mux(io.predictTouch.valid, predictStateGen.io.nextState, predictState)

  // write back next state
  stateBank.io.predictWriteValid  := io.predictTouch.valid
  stateBank.io.predictWriteSetIdx := io.predictTouch.bits.setIdx
  stateBank.io.predictWriteState  := predictNextState

  /* *** train *** */
  // read current state
  stateBank.io.trainReadSetIdx := io.writeTouch.bits.setIdx
  private val trainState = stateBank.io.trainReadState

  // compose touch way vec
  private val writeTouchWay = VecInit((0 until NumWay).map { i =>
    val wayValid = Wire(Valid(UInt(log2Up(NumWay).W)))
    wayValid.valid := io.writeTouch.valid && io.writeTouch.bits.wayMask(i)
    wayValid.bits  := i.U
    wayValid
  })

  // generate next state
  writeStateGen.io.state    := trainState
  writeStateGen.io.touches  := writeTouchWay
  writeStateGen.io.writeCnt := Mux(io.writeMask.valid, PopCount(io.writeMask.bits), 0.U)
  private val trainNextState = Mux(io.writeTouch.valid, writeStateGen.io.nextState, trainState)

  // write back next state
  stateBank.io.trainWriteValid  := io.writeTouch.valid
  stateBank.io.trainWriteSetIdx := io.writeTouch.bits.setIdx
  stateBank.io.trainWriteState  := trainNextState

  /* *** victim *** */
  io.victim.wayMask := writeStateGen.io.victimMask
}
