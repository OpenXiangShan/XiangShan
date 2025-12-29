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

package xiangshan.frontend.bpu.abtb

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.ReplacementPolicy
import xiangshan.frontend.bpu.PlruStateGen
import xiangshan.frontend.bpu.ReplacerState

/**
  * This module implement the replacement policy for the ahead BTB.
  */
class AheadBtbReplacer(implicit p: Parameters) extends AheadBtbModule {
  val io: ReplacerIO = IO(new ReplacerIO)

  // use PlruStateGen caclulate next state and replace way
  private val states           = Module(new ReplacerState(NumSets, NumWays - 1, hasExtraReadPort = true))
  private val predReplacerGen  = Module(new PlruStateGen(NumWays, accessSize = NumWays))
  private val writeReplacerGen = Module(new PlruStateGen(NumWays))
  private val writeTouch       = Wire(Valid(UInt(WayIdxWidth.W)))
  private val touchWays        = Seq.fill(NumWays)(Wire(Valid(UInt(WayIdxWidth.W))))

  writeTouch.valid              := io.writeValid
  writeTouch.bits               := io.writeWayIdx
  states.io.trainReadSetIdx     := io.writeSetIdx
  states.io.trainWriteValid     := io.writeValid
  states.io.trainWriteSetIdx    := io.writeSetIdx
  states.io.trainWriteState     := writeReplacerGen.io.nextState
  writeReplacerGen.io.stateIn   := states.io.trainReadState
  writeReplacerGen.io.touchWays := Seq(writeTouch)

  touchWays.zip(io.readWayMask).zipWithIndex.foreach { case ((t, r), i) =>
    t.valid := r
    t.bits  := i.U
  }
  states.io.predictReadSetIdx  := io.readSetIdx
  states.io.predictWriteValid  := io.readValid && io.readWayMask.reduce(_ || _)
  states.io.predictWriteSetIdx := io.readSetIdx
  states.io.predictWriteState  := predReplacerGen.io.nextState
  predReplacerGen.io.stateIn   := states.io.predictReadState
  predReplacerGen.io.touchWays := touchWays

  states.io.readSetIdx.foreach(_ := io.replaceSetIdx)
  private val replacerState = states.io.readState.getOrElse(0.U)

  private val genReplaceWay = writeReplacerGen.way(replacerState)

  if (EnableCommitGHistDiff) {
    // use ReplacementPolicy class caclulate next state replace way
    val replacer = ReplacementPolicy.fromString(Some("setplru"), NumWays, NumSets)

    val readWriteConflict = io.readValid && io.writeValid && (io.readSetIdx === io.writeSetIdx)
    when(readWriteConflict) {
      replacer.access(io.writeSetIdx, io.writeWayIdx)
    }.otherwise {
      when(io.writeValid) {
        replacer.access(io.writeSetIdx, io.writeWayIdx)
      }
      when(io.readValid) {
        val touchSets = Seq.fill(NumWays)(io.readSetIdx)
        val touchWays = Seq.fill(NumWays)(Wire(Valid(UInt(WayIdxWidth.W))))
        touchWays.zip(io.readWayMask).zipWithIndex.foreach { case ((t, r), i) =>
          t.valid := r
          t.bits  := i.U
        }
        replacer.access(touchSets, touchWays)
      }
    }
    val replacerWay  = replacer.way(io.replaceSetIdx)
    val replacerDiff = genReplaceWay === replacerWay
    dontTouch(genReplaceWay)
    dontTouch(replacerWay)
    dontTouch(replacerDiff)
    when(io.writeValid) {
      assert(
        replacerDiff,
        "writeReplacerGen's replaceWay doesn't match replacer!"
      )
    }
  }

  io.victimWayIdx := genReplaceWay
}
