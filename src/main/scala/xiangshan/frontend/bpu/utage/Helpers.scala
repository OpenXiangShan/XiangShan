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

package xiangshan.frontend.bpu.utage

import chisel3._
import chisel3.util._
import scala.math.min
import xiangshan.HasXSParameter
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.FoldedHistoryInfo
import xiangshan.frontend.bpu.HalfAlignHelper
import xiangshan.frontend.bpu.MicroTageInfo
import xiangshan.frontend.bpu.history.phr.PhrAllFoldedHistories

trait Helpers extends HasMicroTageParameters with HalfAlignHelper {
  private object TAGEHistoryType {
    val Short   = 0
    val Medium  = 1
    val Long    = 2
    val Unknown = 3
  }
  def getUnhashedIdx(pc: PrunedAddr): UInt = pc(VAddrBits - 1, instOffsetBits)
  def getUnhashedTag(pc: PrunedAddr): UInt = pc(VAddrBits - 1, PCHighTagStart)
  def connectPcTag(partPc: UInt, tableId: Int): UInt = {
    require(tableId >= 0 && tableId <= 3, s"tableId must be in [0,3], got $tableId")
    def concatBits(bits: Seq[Bool]): UInt = if (bits.isEmpty) 0.U(1.W) else bits.foldLeft(0.U(0.W))(Cat(_, _))
    val tagPC = partPc
    val hashPC = tableId match {
      case TAGEHistoryType.Short =>
        concatBits(PCTagHashBitsForShortHistory.map(tagPC(_)))

      case TAGEHistoryType.Medium =>
        concatBits(PCTagHashBitsForMediumHistory.map(tagPC(_)))

      case TAGEHistoryType.Long =>
        concatBits(PCTagHashBitsForLongHistory.map(tagPC(_)))

      case _ =>
        concatBits(PCTagHashBitsDefault.map(tagPC(_)))
    }

    hashPC
  }
  def computeSameIdx(pc: PrunedAddr, allFh: PhrAllFoldedHistories): UInt = {
    val idxFhInfo   = new FoldedHistoryInfo(log2Ceil(MaxNumSets), log2Ceil(MaxNumSets))
    val unhashedIdx = getUnhashedIdx(pc)
    val idxFh       = allFh.getHistWithInfo(idxFhInfo).foldedHist
    val idx         = (unhashedIdx ^ idxFh)(log2Ceil(MaxNumSets) - 1, 0)
    idx
  }
  def computeHashIdx(
      pc:         PrunedAddr,
      allFh:      PhrAllFoldedHistories,
      tablesInfo: Seq[MicroTageInfo],
      tableId:    Int
  ): UInt = {
    val histLen     = tablesInfo(tableId).HistoryLength
    val numSets     = tablesInfo(tableId).NumSets
    val idxFhInfo   = new FoldedHistoryInfo(histLen, min(log2Ceil(numSets), histLen))
    val unhashedIdx = getUnhashedIdx(pc)
    val idxFh       = allFh.getHistWithInfo(idxFhInfo).foldedHist
    val idx         = (unhashedIdx ^ idxFh)(log2Ceil(numSets) - 1, 0)
    idx
  }
  def computeHashTag(
      pc:         PrunedAddr,
      allFh:      PhrAllFoldedHistories,
      tablesInfo: Seq[MicroTageInfo],
      tableId:    Int
  ): UInt = {
    val histLen       = tablesInfo(tableId).HistoryLength
    val tagLen        = tablesInfo(tableId).TagWidth
    val histBitsInTag = tablesInfo(tableId).HistBitsInTag
    val tagFhInfo     = new FoldedHistoryInfo(histLen, min(histLen, histBitsInTag))
    val altTagFhInfo  = new FoldedHistoryInfo(histLen, min(histLen, histBitsInTag - 1))
    val tagFh         = allFh.getHistWithInfo(tagFhInfo).foldedHist
    val altTagFh      = allFh.getHistWithInfo(altTagFhInfo).foldedHist
    val unhashedTag   = getUnhashedTag(pc)
    val unhashedIdx   = getUnhashedIdx(pc)
    val lowTag        = (unhashedTag ^ tagFh ^ (altTagFh << 1))(histBitsInTag - 1, 0)
    val highTag       = connectPcTag(unhashedIdx, tableId)
    val tag           = Cat(highTag, lowTag)(tagLen - 1, 0)
    // Temporarily expand the bit width to be consistent for easier processing.
    // This may later be changed to use SRAM storage
    tag.pad(log2Ceil(MaxTagLen))
  }

  def getBankId(index: UInt, numBanks: Int): UInt = {
    val bankId = index(log2Ceil(numBanks) - 1, 0)
    bankId
  }
  def getBankInnerIndex(index: UInt, numBanks: Int, numSets: Int): UInt = {
    val bankInnerIndex = index(log2Ceil(numSets) - 1, log2Ceil(numBanks))
    bankInnerIndex
  }

  def findTwoZeros(dataVec: UInt): (UInt, UInt, Bool, Bool, Bool) = {
    val width = dataVec.getWidth
    require(width == 8 || width == 16, "Width must be 8 or 16")
    val zerosVec       = (~dataVec).asBools
    val firstZeroFound = zerosVec.reduce(_ || _)
    val firstZeroIdx   = PriorityEncoder(zerosVec)
    val maskVec        = Wire(Vec(width, Bool()))
    for (i <- 0 until width) {
      maskVec(i) := zerosVec(i) && (i.U =/= firstZeroIdx)
    }
    val secondZeroFound = maskVec.reduce(_ || _)
    val secondZeroIdx   = PriorityEncoder(maskVec)
    val noZeros         = !firstZeroFound
    (firstZeroIdx, secondZeroIdx, firstZeroFound, secondZeroFound, noZeros)
  }
}
