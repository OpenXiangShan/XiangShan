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
import xiangshan.frontend.bpu.replacer.ReplacerState
import xiangshan.frontend.bpu.replacer.ReplacerStateGen

class MainBtbReplacer(implicit p: Parameters) extends MainBtbModule {
  class MainBtbReplacerIO extends Bundle {
    class Touch extends Bundle {
      val setIdx:  UInt = UInt(SetIdxLen.W)
      val wayMask: UInt = UInt(NumWay.W)
    }

    class Predict extends Bundle {
      val touch: Valid[Touch] = Flipped(Valid(new Touch))
    }

    class Train extends Bundle {
      val t0_setIdx: UInt         = Input(UInt(SetIdxLen.W))
      val t0_fire:   Bool         = Input(Bool())
      val t0_victim: UInt         = Output(UInt(NumWay.W))
      val t1_touch:  Valid[Touch] = Flipped(Valid(new Touch))
    }

    val predict: Predict = new Predict
    val train:   Train   = new Train
  }

  val io: MainBtbReplacerIO = IO(new MainBtbReplacerIO)

  private val predictStateGen = Module(ReplacerStateGen(Replacer, NumWay, accessSize = NumWay))
  private val trainStateGen   = Module(ReplacerStateGen(Replacer, NumWay))
  private val victimStateGen  = Module(ReplacerStateGen(Replacer, NumWay))
  private val stateBank       = Module(new ReplacerState(NumSets, predictStateGen.StateWidth))

  /* *** predict *** */
  // read current state
  stateBank.io.predictRead.setIdx := io.predict.touch.bits.setIdx
  private val predictState = stateBank.io.predictRead.state

  // compose touch way vec
  private val predictTouchWay = VecInit((0 until NumWay).map { i =>
    val wayValid = Wire(Valid(UInt(log2Up(NumWay).W)))
    wayValid.valid := io.predict.touch.valid && io.predict.touch.bits.wayMask(i)
    wayValid.bits  := i.U
    wayValid
  })

  // generate next state
  predictStateGen.io.state   := predictState
  predictStateGen.io.touches := predictTouchWay
  private val predictNextState = predictStateGen.io.nextState

  // write back next state
  stateBank.io.predictWrite.valid       := io.predict.touch.valid
  stateBank.io.predictWrite.bits.setIdx := io.predict.touch.bits.setIdx
  stateBank.io.predictWrite.bits.state  := predictNextState

  /* *** train t0 *** */
  // read current state
  stateBank.io.trainRead.setIdx := io.train.t0_setIdx

  // if t1 is about to write the same set as t0 read, we need to use the updated state (trainStateGen.io.nextState)
  private val trainNextState = trainStateGen.io.nextState
  private val trainSameSet   = io.train.t1_touch.valid && (io.train.t1_touch.bits.setIdx === io.train.t0_setIdx)
  private val predictSameSet = io.predict.touch.valid && (io.predict.touch.bits.setIdx === io.train.t0_setIdx)
  private val trainState = MuxCase(
    stateBank.io.trainRead.state,
    Seq(
      trainSameSet   -> trainNextState, // trainNextState has higher priority, see ReplacerState.scala
      predictSameSet -> predictNextState
    )
  )

  // generate victim
  victimStateGen.io.state   := trainState
  victimStateGen.io.touches := DontCare // we use victimStateGen just to select victim, don't care touch & nextState

  // send back to MainBtbAlignBank
  io.train.t0_victim := UIntToOH(victimStateGen.io.victim)

  /* *** train t1 *** */
  // compose touch way vec
  private val trainTouchWay = Wire(Valid(UInt(log2Up(NumWay).W)))
  trainTouchWay.valid := io.train.t1_touch.valid
  trainTouchWay.bits  := OHToUInt(io.train.t1_touch.bits.wayMask) // MainBtbAlignBank ensures this is one-hot
  assert(
    !io.train.t1_touch.valid || PopCount(io.train.t1_touch.bits.wayMask) <= 1.U,
    "victim wayMask should be at-most-one-hot"
  )

  // generate next state
  trainStateGen.io.state   := RegEnable(trainState, io.train.t0_fire) // RegEnable to sync with t1
  trainStateGen.io.touches := VecInit(Seq(trainTouchWay))

  // write back next state
  stateBank.io.trainWrite.valid       := io.train.t1_touch.valid
  stateBank.io.trainWrite.bits.setIdx := io.train.t1_touch.bits.setIdx
  stateBank.io.trainWrite.bits.state  := trainNextState

  // we use t0_setIdx to pre-read stateBank for better timing, it's a tightly-coupled design,
  // so we require t1 to touch the same set
  assert(
    !(
      io.train.t1_touch.valid && RegEnable(io.train.t0_setIdx, io.train.t0_fire) =/= io.train.t1_touch.bits.setIdx
    ),
    "pipeline mismatch: t1 touch should be for the same set as t0 train"
  )
}
