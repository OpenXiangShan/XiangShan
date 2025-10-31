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
    val prediction: Prediction = Output(new Prediction)
  }

  val io: FallThroughPredictorIO = IO(new FallThroughPredictorIO)

  io.resetDone := true.B

  io.train.ready := true.B

  /* *** predict stage 0 *** */
  private val s0_fire       = io.stageCtrl.s0_fire
  private val s0_startVAddr = io.startVAddr

  /* *** predict stage 1 *** */
  private val s1_fire       = io.stageCtrl.s1_fire
  private val s1_startVAddr = RegEnable(s0_startVAddr, s0_fire)

  // e.g. FetchBlockSize = 64B, FetchBlockAlignSize = 32B, we have CfiPositionWidth = 5
  // start | nextBlock | crossPage | nextPage | cfiPc | cfiPosition
  // 0x000 |   0x0040  |   false   |  0x0000  | 0x03e | 0x1f // (0x03e-0x000) >> 1
  // 0x010 |   0x0040  |   false   |  0x0000  | 0x03e | 0x1f // (0x03e-0x000) >> 1
  // 0x020 |   0x0060  |   false   |  0x0000  | 0x05e | 0x1f // (0x05e-0x020) >> 1
  // 0xfc0 |   0x1000  |    true   |  0x1000  | 0xffe | 0x1f // (0xffe-0xfc0) >> 1
  // 0xfd0 |   0x1000  |    true   |  0x1000  | 0xffe | 0x1f // (0xffe-0xfc0) >> 1
  // 0xfe0 |   0x1020  |    true   |  0x1000  | 0xffe | 0x0f // (0xffe-0xfe0) >> 1
  // 0xff0 |   0x1020  |    true   |  0x1000  | 0xffe | 0x0f // (0xffe-0xfe0) >> 1

  // e.g. FetchBlockSize = 64B, FetchBlockAlignSize = 16B, we have CfiPositionWidth = 5
  // start | nextBlock | crossPage | nextPage | cfiPc | cfiPosition
  // 0x000 |   0x0040  |   false   |  0x0000  | 0x03e | 0x1f // (0x03e-0x000) >> 1
  // 0x010 |   0x0050  |   false   |  0x0000  | 0x04e | 0x1f // (0x04e-0x010) >> 1
  // 0x020 |   0x0060  |   false   |  0x0000  | 0x05e | 0x1f // (0x05e-0x020) >> 1
  // 0xfc0 |   0x1000  |    true   |  0x1000  | 0xffe | 0x1f // (0xffe-0xfc0) >> 1
  // 0xfd0 |   0x1010  |    true   |  0x1000  | 0xffe | 0x17 // (0xffe-0xfd0) >> 1
  // 0xfe0 |   0x1020  |    true   |  0x1000  | 0xffe | 0x0f // (0xffe-0xfe0) >> 1
  // 0xff0 |   0x1030  |    true   |  0x1000  | 0xffe | 0x07 // (0xffe-0xff0) >> 1

  // fall-through address = startVAddr + FetchBlockSize(64B), aligned to FetchBlockAlign(32B)
  private val s1_nextBlockAlignedAddr = getAlignedAddr(s1_startVAddr + FetchBlockSize.U)

  // if cross page, we need to align fallThroughAddr to the next page
  private val s1_crossPage           = isCrossPage(s1_startVAddr, s1_nextBlockAlignedAddr) // compare LSB of Vpn
  private val s1_nextPageAlignedAddr = getPageAlignedAddr(s1_nextBlockAlignedAddr)         // clear page offset

  // cfiPosition is the offset between alignedStart and last 2B before nextStart (nextBlock or nextPage), in instr
  // i.e. if not crossPage:
  //    cfiPosition = ((nextBlock - alignedStart) >> instOffsetBits) - 1
  //                = (FetchBlockSize >> instOffsetBits) - 1
  //                = FetchBlockInstNum - 1 // constant
  // otherwise:
  //    cfiPosition = ((nextPage - alignedStart) >> instOffsetBits) - 1
  //                = ((-(alignedStart - nextPage)) >> instOffsetBits) - 1
  //                // ignore the higher bits, alignedStart(CfiPositionWidth, 1) === nextBlock(CfiPositionWidth, 1)
  //                = ((-(nextBlock - nextPage)) >> instOffsetBits) - 1
  //                = (~(nextBlock - nextPage)) >> instOffsetBits
  private val s1_cfiPosition = Mux(
    s1_crossPage,
    ~(s1_nextBlockAlignedAddr - s1_nextPageAlignedAddr)(CfiPositionWidth + instOffsetBits - 1, instOffsetBits),
    (FetchBlockInstNum - 1).U
  )

  private val s1_fallThroughAddr = Mux(
    s1_crossPage,
    s1_nextPageAlignedAddr,
    s1_nextBlockAlignedAddr
  )

  io.prediction.taken       := false.B
  io.prediction.cfiPosition := s1_cfiPosition
  io.prediction.target      := s1_fallThroughAddr
  io.prediction.attribute   := BranchAttribute.None

  XSPerfAccumulate("crossPage", s1_fire && s1_crossPage)
  XSPerfAccumulate("crossPageFixed", s1_fire && s1_crossPage && s1_nextBlockAlignedAddr =/= s1_nextPageAlignedAddr)
}
