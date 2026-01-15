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

package xiangshan.frontend.bpu.mbtb

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.frontend.bpu.PlruStateGen
import xiangshan.frontend.bpu.ReplacerState

class MainBtbReplacer(implicit p: Parameters) extends MainBtbModule {
  class MainBtbReplacerIO extends Bundle {
    class Touch extends Bundle {
      val setIdx:  UInt = UInt(SetIdxLen.W)
      val wayMask: UInt = UInt(NumWay.W)
    }

    class Victim extends Bundle {
      val wayMask: UInt = UInt(NumWay.W)
    }

    val victim: Victim            = Output(new Victim)
    val touch:  Vec[Valid[Touch]] = Vec(2, Flipped(Valid(new Touch))) // magic number 2: predict and train

    def predictTouch: Valid[Touch] = touch(0)
    def trainTouch:   Valid[Touch] = touch(1)
  }

  val io: MainBtbReplacerIO = IO(new MainBtbReplacerIO)

  private val stateBank       = Module(new ReplacerState(NumSets, NumWay))
  private val predictStateGen = Module(new PlruStateGen(NumWay, accessSize = NumWay))
  private val trainStateGen   = Module(new PlruStateGen(NumWay, accessSize = 1))

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
  predictStateGen.io.stateIn   := predictState
  predictStateGen.io.touchWays := predictTouchWay
  private val predictNextState = Mux(io.predictTouch.valid, predictStateGen.io.nextState, predictState)

  // write back next state
  stateBank.io.predictWriteValid  := io.predictTouch.valid
  stateBank.io.predictWriteSetIdx := io.predictTouch.bits.setIdx
  stateBank.io.predictWriteState  := predictNextState

  /* *** train *** */
  // read current state
  stateBank.io.trainReadSetIdx := io.trainTouch.bits.setIdx
  private val trainState = stateBank.io.trainReadState

  // compose touch way vec
  private val trainTouchWay = Wire(Valid(UInt(log2Up(NumWay).W)))
  trainTouchWay.valid := io.trainTouch.valid
  trainTouchWay.bits  := OHToUInt(io.trainTouch.bits.wayMask) // MainBtbAlignBank ensures this is one-hot
  assert(
    !io.trainTouch.valid || PopCount(io.trainTouch.bits.wayMask) <= 1.U,
    "victim wayMask should be at-most-one-hot"
  )

  // generate next state
  trainStateGen.io.stateIn   := trainState
  trainStateGen.io.touchWays := VecInit(Seq(trainTouchWay))
  private val trainNextState = Mux(io.trainTouch.valid, trainStateGen.io.nextState, trainState)

  // write back next state
  stateBank.io.trainWriteValid  := io.trainTouch.valid
  stateBank.io.trainWriteSetIdx := io.trainTouch.bits.setIdx
  stateBank.io.trainWriteState  := trainNextState

  /* *** victim *** */
  io.victim.wayMask := UIntToOH(trainStateGen.io.replaceWay)
}
