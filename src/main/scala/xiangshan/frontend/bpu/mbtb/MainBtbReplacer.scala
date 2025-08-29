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

class MainBtbReplacer(implicit p: Parameters) extends MainBtbModule {
  val io: ReplacerIO = IO(new ReplacerIO)

  private val statesBanks = Seq.tabulate(NumAlignBanks)(alignIdx => Module(new ReplacerState()))

  private val predictSetIndexVec = io.predictionSetIndxVec
  private val predictStateEntries: Vec[UInt] =
    WireInit(VecInit(Seq.fill(NumAlignBanks)(0.U((NumWay - 1).W))))
  predictSetIndexVec zip statesBanks zip predictStateEntries foreach { case ((setIdx, states), state) =>
    states.io.predictReadSetIdx := setIdx
    state                       := states.io.predictReadState
  }
  require(
    predictSetIndexVec(0).getWidth == SetIdxLen,
    s"S2 stage set index width mismatch: ${predictSetIndexVec(0).getWidth} != $SetIdxLen"
  )
  require(
    predictSetIndexVec.length == NumAlignBanks,
    s"predictSetIndexVec width mismatch: ${predictSetIndexVec.length} != $NumAlignBanks"
  )
  private val predictStateTouchs: Vec[Vec[Valid[UInt]]] = io.predictionTouchWays
  private val predictNextState: Vec[UInt] =
    WireInit(VecInit(Seq.fill(NumAlignBanks)(0.U((NumWay - 1).W))))

  private val plruStateGen = Seq.tabulate(NumAlignBanks) { alignIdx =>
    Module(new PlruStateGen(NumWay, accessSize = NumWay))
  }
  private val hits = io.predictionHitMask
  plruStateGen zip predictStateEntries zip predictStateTouchs zip predictNextState zip hits foreach {
    case ((((gen, state), touchs), nextState), hit) =>
      gen.io.stateIn   := state
      gen.io.touchWays := touchs
      nextState        := Mux(hit, gen.io.nextState, state)
  }
  predictSetIndexVec zip predictNextState zip statesBanks zip hits foreach {
    case (((setIdx, nextState), states), hit) =>
      states.io.predictWriteValid  := hit
      states.io.predictWriteSetIdx := setIdx
      states.io.predictWriteState  := nextState
  }

  private val trainSetIdx = io.trainSetIndx
  private val trainValid  = io.trainWriteValid
  require(
    trainSetIdx.getWidth == SetIdxLen,
    s"train set index width mismatch: ${trainSetIdx.getWidth} != $SetIdxLen"
  )
  private val trainAlignBankMask: Vec[Bool] = io.trainAlignBankMask
  private val trainStateEntries: Vec[UInt] =
    WireInit(VecInit(Seq.fill(NumAlignBanks)(0.U((NumWay - 1).W))))
  trainAlignBankMask zip statesBanks zip trainStateEntries foreach { case ((bankEnable, states), state) =>
    states.io.trainReadSetIdx := trainSetIdx
    state                     := states.io.trainReadState
  }

  private val trainStateEntry: UInt = Mux1H(trainAlignBankMask, trainStateEntries)
  private val writeReplacerGen = Module(new PlruStateGen(NumWay, accessSize = 1))
  private val trainTouchWay    = Wire(Valid(UInt(log2Up(NumWay).W)))
  private val trainVictimWay   = Wire(UInt(log2Up(NumWay).W))
  private val trainNextState: UInt = WireInit(0.U((NumWay - 1).W))
  writeReplacerGen.io.stateIn   := trainStateEntry
  trainVictimWay                := writeReplacerGen.io.replaceWay
  trainNextState                := Mux(trainValid, writeReplacerGen.io.nextState, trainStateEntry)
  trainTouchWay.valid           := trainValid
  trainTouchWay.bits            := trainVictimWay
  writeReplacerGen.io.touchWays := Seq(trainTouchWay)

  statesBanks zip trainAlignBankMask foreach { case (states, alignBankEnable) =>
    states.io.trainWriteValid  := trainValid && alignBankEnable
    states.io.trainWriteSetIdx := trainSetIdx
    states.io.trainWriteState  := trainNextState
  }
  require(
    trainVictimWay.getWidth == log2Ceil(NumWay),
    s"Replace way width mismatch: ${trainVictimWay.getWidth} != ${log2Ceil(NumWay)}"
  )

  io.victimWayIdx := trainVictimWay
  require(
    io.victimWayIdx.getWidth == log2Up(NumWay),
    s"Write way mask width mismatch: ${io.victimWayIdx.getWidth} != $log2Up(NumWay)"
  )

}
