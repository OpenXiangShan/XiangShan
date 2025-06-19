// Copyright (c) 2024-2025 Beijing Institute of Open Source Chip (BOSC)Add commentMore actions
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

class FallThroughPredictor(implicit p: Parameters) extends BasePredictor {
  class FallThroughPredictorIO extends BasePredictorIO {}

  val io: FallThroughPredictorIO = IO(new FallThroughPredictorIO)

  /* *** predict stage 0 *** */
  private val s0_fire = io.stageCtrl.s0_fire

  private val s0_startVAddr = io.startVAddr

  private val s0_fallThroughAddr = s0_startVAddr + FetchBlockMaxSize.U

  /* *** predict stage 1 *** */
  private val s1_fallThroughAddr = RegEnable(s0_fallThroughAddr, s0_fire)

  io.prediction.valid                  := true.B
  io.prediction.bits.cfiPosition.valid := true.B // the last inst in fetch block act as a taken branch
  io.prediction.bits.cfiPosition.bits  := (FetchBlockMaxSize - 1).U
  io.prediction.bits.target            := s1_fallThroughAddr
  io.prediction.bits.attribute         := BranchAttribute.None
}
