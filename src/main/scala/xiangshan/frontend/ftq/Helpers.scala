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

package xiangshan.frontend.ftq

import chisel3._
import chisel3.util._
import xiangshan.frontend.icache.ICacheDataHelper

trait TwoPrefetchHelper extends HasFtqParameters with ICacheDataHelper {
  def getFetchBlockInfo(entry: FtqEntry): FetchBlockInfo = {
    val blockOffset        = entry.startPc(blockOffBits - 1, 0)
    val blockEndOffsetTemp = blockOffset +& Cat(entry.takenCfiOffset.bits, 0.U(instOffsetBits.W))
    val blockEndOffset     = blockEndOffsetTemp(blockOffBits - 1, 0)
    val isCrossLine        = blockEndOffsetTemp(blockOffBits)

    val fb = Wire(new FetchBlockInfo)
    fb.startPc        := entry.startPc
    fb.takenCfiOffset := entry.takenCfiOffset
    fb.size           := (entry.takenCfiOffset.bits +& 1.U) << 1 // Bytes
    fb.isCrossLine    := isCrossLine
    fb.isMmio         := entry.twoFetchInfo.isMmio
    fb.wayMask        := entry.twoFetchInfo.wayMask
    fb.bankMask       := VecInit(getBankSel(blockOffset, blockEndOffset, fb.isCrossLine).map(_.asUInt))
    fb.vSetIdx        := VecInit(get_idx(entry.startPc), get_idx(entry.startPc) + 1.U)
    fb.vPageNumber    := entry.startPc(VAddrBits - 1, PageOffsetWidth)
    fb
  }

  def getTwoPrefetchCaseEncode(fb1: FetchBlockInfo, fb2: FetchBlockInfo): UInt = {
    // case0: two blocks' startPC are in the same cache line
    val case0 = fb1.vSetIdx(0) === fb2.vSetIdx(0)

    // case1: the first block is cross line
    //  and the second block's startPC is in the same cache line with the first block's second line
    val case1 = fb1.vSetIdx(1) === fb2.vSetIdx(0) && fb1.isCrossLine && !fb2.isCrossLine

    // case2: the second block is cross line
    //  and the first block's startPC is in the same cache line with the second block's second line
    val case2 = fb1.vSetIdx(0) === fb2.vSetIdx(1) && !fb1.isCrossLine && fb2.isCrossLine

    // case3: the two blocks' startPC are in different cache lines, and both of them are not cross line
    val case3 = fb1.vSetIdx(0) =/= fb2.vSetIdx(0) && !fb1.isCrossLine && !fb2.isCrossLine

    val isInSamePage = fb1.vPageNumber === fb2.vPageNumber

    VecInit(case0, case1, case2, case3).asUInt & Fill(NumTwoPrefetchCases, isInSamePage)
  }

}

trait TwoFetchHelper extends HasFtqParameters with ICacheDataHelper {
  def getICacheDataBankMask(fb: FetchBlockInfo): Vec[UInt] = {
    val blockOffset        = fb.startPc(blockOffBits - 1, 0)
    val blockEndOffsetTemp = blockOffset +& Cat(fb.takenCfiOffset.bits, 0.U(instOffsetBits.W))
    val blockEndOffset     = blockEndOffsetTemp(blockOffBits - 1, 0)
    VecInit(getBankSel(blockOffset, blockEndOffset, fb.isCrossLine).map(_.asUInt))
  }

  def isICacheDataSramReadConflict(fb1: FetchBlockInfo, fb2: FetchBlockInfo): Bool =
    (0 until DataBanks).map { bankIdx =>
      (0 until nWays).map { wayIdx =>
        val fb1ReadSetValid = VecInit(
          fb1.bankMask(0)(bankIdx) && fb1.wayMask(0)(wayIdx),
          fb1.bankMask(1)(bankIdx) && fb1.wayMask(1)(wayIdx)
        )

        val fb2ReadSetValid = VecInit(
          fb2.bankMask(0)(bankIdx) && fb2.wayMask(0)(wayIdx),
          fb2.bankMask(1)(bankIdx) && fb2.wayMask(1)(wayIdx)
        )

        val fb1ReadSetIdx = Mux(fb1ReadSetValid(0), fb1.vSetIdx(0), fb1.vSetIdx(1))
        val fb2ReadSetIdx = Mux(fb2ReadSetValid(0), fb2.vSetIdx(0), fb2.vSetIdx(1))

        val fb1ReadValid = fb1ReadSetValid.reduce(_ || _)
        val fb2ReadValid = fb2ReadSetValid.reduce(_ || _)

        val conflict = Mux(fb1ReadValid && fb2ReadValid, fb1ReadSetIdx === fb2ReadSetIdx, false.B)
        conflict
      }.reduce(_ || _)
    }.reduce(_ || _)
}
