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

package xiangshan.frontend.bpu.tage

import chisel3._
import chisel3.util._
import xiangshan.HasXSParameter
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.HalfAlignHelper
import xiangshan.frontend.bpu.RotateHelper
import xiangshan.frontend.bpu.history.phr.PhrAllFoldedHistories

trait Helpers extends HasTageParameters with HasXSParameter with RotateHelper with HalfAlignHelper {
  def getBaseTableSetIndex(pc: PrunedAddr): UInt =
    pc(BaseTableSetIdxWidth - 1 + BankIdxWidth + FetchBlockSizeWidth, BankIdxWidth + FetchBlockSizeWidth)

  def getBaseTableBankIndex(pc: PrunedAddr): UInt =
    pc(BankIdxWidth - 1 + FetchBlockSizeWidth, FetchBlockSizeWidth)

  def getBaseTableAlignBankIndex(pc: PrunedAddr): UInt =
    pc(FetchBlockSizeWidth - 1, FetchBlockAlignWidth)

  def getFoldedHist(allFoldedPathHist: PhrAllFoldedHistories): Vec[TageFoldedHist] =
    VecInit(TableInfos.map { tableInfo =>
      val tageFoldedHist = tableInfo.getTageFoldedHistoryInfo(NumBanks, TagWidth).map { histInfo =>
        allFoldedPathHist.getHistWithInfo(histInfo).foldedHist
      }
      val foldedHist = Wire(new TageFoldedHist(tableInfo.NumSets))
      foldedHist.forIdx := tageFoldedHist.head
      foldedHist.forTag := tageFoldedHist(1) ^ Cat(tageFoldedHist(2), 0.U(1.W))
      foldedHist
    })

  def getSetIndex(pc: PrunedAddr, hist: UInt, numSets: Int): UInt = {
    val setIdxWidth = log2Ceil(numSets / NumBanks)
    pc(setIdxWidth - 1 + BankIdxWidth + FetchBlockSizeWidth, BankIdxWidth + FetchBlockSizeWidth) ^ hist
  }

  def getTag(pc: PrunedAddr, hist: UInt): UInt =
    pc(TagWidth, 1) ^ hist

  def getBankIndex(pc: PrunedAddr): UInt =
    pc(BankIdxWidth - 1 + FetchBlockSizeWidth, FetchBlockSizeWidth)

  def getLongestHistTableOH(hitTableMask: Seq[Bool]): Seq[Bool] =
    PriorityEncoderOH(hitTableMask.reverse).reverse

  def getUseAltIndex(pc: PrunedAddr): UInt = {
    val useAltIdxWidth = log2Ceil(NumUseAltCtrs)
    pc(useAltIdxWidth - 1 + instOffsetBits, instOffsetBits)
  }
}
