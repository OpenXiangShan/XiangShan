// Copyright (c) 2024-2025 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2025 Institute of Computing Technology, Chinese Academy of Sciences
// Copyright (c) 2020-2021 Peng Cheng Laboratory
//
// XiangShan is licensed under Mulan PSL v2.
// You can use this software according to the terms and conditions of the Mulan PSL v2.
// You may obtain a copy of Mulan PSL v2 at: https://license.coscl.org.cn/MulanPSL2
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
import xiangshan.frontend.bpu.abtb.AheadBtbHelpers
import xiangshan.frontend.bpu.abtb.HasAheadBtbParams
import xiangshan.frontend.bpu.abtb.Prediction

class DummyMicroBtbInput(implicit p: Parameters) extends XSBundle with HasAheadBtbParams
    with HasPredictorCommonSignals {}

class DummyMicroBtbOutput(implicit p: Parameters) extends XSBundle with HasAheadBtbParams {
  val prediction = new Prediction
}

class DummyMicroBtbIO(implicit p: Parameters) extends XSBundle with HasAheadBtbParams {
  val in  = Input(new DummyMicroBtbInput)
  val out = Output(new DummyMicroBtbOutput)
}

class DummyMicroBtb(implicit p: Parameters) extends XSModule with HasAheadBtbParams with AheadBtbHelpers {
  val io = IO(new DummyMicroBtbIO)

  val s1_pc = RegEnable(io.in.s0_pc, io.in.s0_fire)

  val fallThroughAddr = getAlignedPc(s1_pc) + FetchBlockSize.U

  io.out.prediction.startPc             := s1_pc
  io.out.prediction.target              := fallThroughAddr
  io.out.prediction.takenPosition.valid := false.B
  io.out.prediction.takenPosition.bits  := (PredictWidth - 1).U
  io.out.prediction.fallThroughError    := false.B
}
