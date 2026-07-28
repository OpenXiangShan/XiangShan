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
    with CrossPageHelper
    with CrossCacheLineHelper {
  class FallThroughPredictorIO extends BasePredictorIO {
    val prediction: Prediction = Output(new Prediction)
  }

  val io: FallThroughPredictorIO = IO(new FallThroughPredictorIO)

  io.sramResetDone := true.B

  io.trainReady := true.B

  /* *** predict stage 0 *** */
  private val s0_fire    = io.stageCtrl.s0_fire
  private val s0_startPc = io.startPc

  /* *** predict stage 1 *** */
  private val s1_fire    = io.stageCtrl.s1_fire
  private val s1_startPc = RegEnable(s0_startPc, s0_fire)

  // Limit the fall-through PC to the first ICache line. With the default 64B fetch block and
  // 32B alignment, a block starting at 0x20 ends at 0x40 instead of 0x60.
  private val s1_nextBlockAlignedPc = getAlignedPc(s1_startPc + FetchBlockSize.U)
  private val s1_nextCacheLinePc    = getNextCacheLineAlignedAddr(s1_startPc)
  private val s1_crossCacheLine     = isCrossCacheLine(s1_startPc, s1_nextBlockAlignedPc)
  private val s1_cacheLineLimitedPc = Mux(
    s1_crossCacheLine,
    s1_nextCacheLinePc,
    s1_nextBlockAlignedPc
  )

  // if cross page, we need to align fallThroughPc to the next page
  private val s1_crossPage         = isCrossPage(s1_startPc, s1_cacheLineLimitedPc) // compare LSB of Vpn
  private val s1_nextPageAlignedPc = getPageAlignedAddr(s1_cacheLineLimitedPc)      // clear page offset

  private val s1_fallThroughPc = Mux(
    s1_crossPage,
    s1_nextPageAlignedPc,
    s1_cacheLineLimitedPc
  )

  // cfiPosition is relative to the aligned fetch-block start and points to the final 2B slot.
  private val s1_alignedStartPc  = getAlignedPc(s1_startPc)
  private val s1_fallThroughSize = (s1_fallThroughPc - s1_alignedStartPc) >> instOffsetBits
  private val s1_cfiPosition     = (s1_fallThroughSize - 1.U)(CfiPositionWidth - 1, 0)

  io.prediction.taken       := false.B
  io.prediction.cfiPosition := s1_cfiPosition
  io.prediction.target      := s1_fallThroughPc
  io.prediction.attribute   := BranchAttribute.None

  XSPerfAccumulate("crossPage", s1_fire && s1_crossPage)
  XSPerfAccumulate("crossPageFixed", s1_fire && s1_crossPage && s1_cacheLineLimitedPc =/= s1_nextPageAlignedPc)
  XSPerfAccumulate(
    "crossCacheLineFixed",
    s1_fire && s1_crossCacheLine && s1_nextBlockAlignedPc =/= s1_nextCacheLinePc
  )
}
