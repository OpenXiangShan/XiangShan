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
import utility.XSPerfAccumulate

class FallThroughPredictor(implicit p: Parameters) extends BasePredictor
    with HalfAlignHelper
    with CrossPageHelper {
  class FallThroughPredictorIO extends BasePredictorIO {
    val prediction: BranchPrediction = Output(new BranchPrediction)
  }

  val io: FallThroughPredictorIO = IO(new FallThroughPredictorIO)

  /* *** predict stage 0 *** */
  private val s0_fire = io.stageCtrl.s0_fire

  private val s0_startVAddr = io.startVAddr

  // fall-through address = startVAddr + FetchBlockSize(64B), aligned to FetchBlockAlign(32B)
  private val s0_nextBlockAlignedAddr = getAlignedAddr(s0_startVAddr + FetchBlockSize.U)

  // if cross page, we need to align fallThroughAddr to the next page
  private val s0_crossPage           = isCrossPage(s0_startVAddr, s0_nextBlockAlignedAddr) // compare LSB of Vpn
  private val s0_nextPageAlignedAddr = getPageAlignedAddr(s0_nextBlockAlignedAddr)         // clear page offset

  private val s0_fallThroughAddr = Mux(
    s0_crossPage,
    s0_nextPageAlignedAddr,
    s0_nextBlockAlignedAddr
  )

  /* *** predict stage 1 *** */
  private val s1_fallThroughAddr = RegEnable(s0_fallThroughAddr, s0_fire)

  io.prediction.taken       := false.B
  io.prediction.cfiPosition := (FetchBlockInstNum - 1).U
  io.prediction.target      := s1_fallThroughAddr
  io.prediction.attribute   := BranchAttribute.None

  XSPerfAccumulate("crossPage", s0_fire && s0_crossPage)
  XSPerfAccumulate("crossPageFixed", s0_fire && s0_crossPage && s0_nextBlockAlignedAddr =/= s0_nextPageAlignedAddr)
}
