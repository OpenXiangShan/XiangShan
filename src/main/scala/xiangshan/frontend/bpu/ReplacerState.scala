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

package xiangshan.frontend.bpu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.XSBundle
import xiangshan.XSModule

// Independent replacer state management enables finer-grained clock gating
class ReplacerState(val NumSets: Int, val NumWays: Int, val hasReplacer: Boolean = false)(implicit p: Parameters)
    extends XSModule {
  class ReplacerStateIO(implicit p: Parameters) extends XSBundle {
    // Read and write for the state of the prediction table
    val predictReadSetIdx: UInt = Input(UInt(log2Ceil(NumSets).W))
    val predictReadState:  UInt = Output(UInt((NumWays - 1).W))

    val predictWriteValid:  Bool = Input(Bool())
    val predictWriteSetIdx: UInt = Input(UInt(log2Ceil(NumSets).W))
    val predictWriteState:  UInt = Input(UInt((NumWays - 1).W))

    // Read and write for the state of the training table
    val trainReadSetIdx: UInt = Input(UInt(log2Ceil(NumSets).W))
    val trainReadState:  UInt = Output(UInt((NumWays - 1).W))

    val trainWriteValid:  Bool = Input(Bool())
    val trainWriteSetIdx: UInt = Input(UInt(log2Ceil(NumSets).W))
    val trainWriteState:  UInt = Input(UInt((NumWays - 1).W))

    val replacerSetIdx: Option[UInt] = Option.when(hasReplacer) {
      Input(UInt(log2Ceil(NumSets).W))
    }
    val replacerState: Option[UInt] = Option.when(hasReplacer) {
      Output(UInt((NumWays - 1).W))
    }
  }

  val io: ReplacerStateIO = IO(new ReplacerStateIO())
  private val states = RegInit(VecInit(Seq.fill(NumSets)(0.U.asTypeOf(UInt((NumWays - 1).W)))))
  private val readWriteConflict =
    io.predictWriteValid && io.trainWriteValid && (io.predictWriteSetIdx === io.trainWriteSetIdx)

  when(readWriteConflict) {
    states(io.trainWriteSetIdx) := io.trainWriteState
  }.otherwise {
    when(io.predictWriteValid) {
      states(io.predictWriteSetIdx) := io.predictWriteState
    }
    when(io.trainWriteValid) {
      states(io.trainReadSetIdx) := io.trainWriteState
    }
  }
  io.predictReadState := states(io.predictReadSetIdx)
  io.trainReadState   := states(io.trainReadSetIdx)

  if (hasReplacer) {
    io.replacerState.get := states(io.replacerSetIdx.getOrElse(0.U))
  }
}
