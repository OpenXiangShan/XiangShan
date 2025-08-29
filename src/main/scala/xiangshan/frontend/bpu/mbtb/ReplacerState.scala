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

class ReplacerState(implicit p: Parameters) extends MainBtbModule {
  class ReplacerStateIO(implicit p: Parameters) extends MainBtbBundle {
    // Read and write for the state of the prediction table
    val predictReadSetIdx: UInt = Input(UInt(log2Ceil(NumEntries).W))
    val predictReadState:  UInt = Output(UInt((NumWay - 1).W))

    val predictWriteValid:  Bool = Input(Bool())
    val predictWriteSetIdx: UInt = Input(UInt(log2Ceil(NumEntries).W))
    val predictWriteState:  UInt = Input(UInt((NumWay - 1).W))

    // Read and write for the state of the training table
    val trainReadSetIdx: UInt = Input(UInt(log2Ceil(NumEntries).W))
    val trainReadState:  UInt = Output(UInt((NumWay - 1).W))

    val trainWriteValid:  Bool = Input(Bool())
    val trainWriteSetIdx: UInt = Input(UInt(log2Ceil(NumEntries).W))
    val trainWriteState:  UInt = Input(UInt((NumWay - 1).W))
  }

  val io: ReplacerStateIO = IO(new ReplacerStateIO())
  private val states = RegInit(VecInit(Seq.fill(NumSets)(0.U.asTypeOf(UInt((NumWay - 1).W)))))

  io.predictReadState := states(io.predictReadSetIdx)
  io.trainReadState   := states(io.trainReadSetIdx)

  when(io.predictWriteValid) {
    states(io.predictWriteSetIdx) := io.predictWriteState
  }
  when(io.trainWriteValid) {
    states(io.trainReadSetIdx) := io.trainWriteState
  }
}
