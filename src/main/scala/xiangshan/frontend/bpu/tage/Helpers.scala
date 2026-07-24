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
import utils.AddrField
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.SaturateCounter
import xiangshan.frontend.bpu.SaturateCounterInit
import xiangshan.frontend.bpu.TageTableInfo
import xiangshan.frontend.bpu.history.phr.PhrAllFoldedHistories

trait TopHelper extends HasTageParameters {
  def getFoldedHist(
      allFoldedPathHist: PhrAllFoldedHistories,
      tableConfigs:      Seq[TageTableConfig],
      runtimeConfig:     TageRuntimeConfig
  ): Vec[TageFoldedHist] = {
    require(tableConfigs.length == NumTables)
    VecInit(TableInfos.zip(tableConfigs).map { case (tableInfo, tableConfig) =>
      val physicalTableInfo = new TageTableInfo(
        MaxNumSets * tableInfo.NumWays * NumBanks,
        tableInfo.NumWays,
        tableInfo.HistoryLength
      )
      val candidates = for {
        numSets  <- SupportedNumSets
        tagWidth <- SupportedTagWidths
      } yield {
        val candidateInfo =
          new TageTableInfo(numSets * tableInfo.NumWays * NumBanks, tableInfo.NumWays, tableInfo.HistoryLength)
        val tageFoldedHist = candidateInfo.getTageFoldedHistoryInfo(NumBanks, tagWidth).map { histInfo =>
          allFoldedPathHist.getHistWithInfo(histInfo).foldedHist
        }
        val foldedHist = Wire(new TageFoldedHist()(p, physicalTableInfo))
        foldedHist.forIdx    := tageFoldedHist.head
        foldedHist.forTag(0) := tageFoldedHist(1)
        foldedHist.forTag(1) := Cat(tageFoldedHist(2), 0.U(1.W))
        (tableConfig.numSetsLog2 === log2Ceil(numSets).U && runtimeConfig.tagWidth === tagWidth.U) -> foldedHist
      }
      MuxCase(candidates.head._2, candidates)
    })
  }

  def getLongestHistTableOH(hitTableMask: Seq[Bool]): Seq[Bool] =
    PriorityEncoderOH(hitTableMask.reverse).reverse

  def getActiveTagWidth(requestedWidth: UInt): UInt =
    Mux(
      requestedWidth < tageParameters.MinTagWidth.U,
      tageParameters.MinTagWidth.U,
      Mux(
        requestedWidth > tageParameters.MaxTagWidth.U,
        tageParameters.MaxTagWidth.U,
        requestedWidth
      )
    )

  def getActiveUsefulCtrWidth(requestedWidth: UInt): UInt =
    Mux(requestedWidth < 1.U, 1.U, Mux(requestedWidth > UsefulCtrWidth.U, UsefulCtrWidth.U, requestedWidth))

  def getActiveUsefulCtrValue(counter: SaturateCounter, requestedWidth: UInt): UInt = {
    val width = getActiveUsefulCtrWidth(requestedWidth)
    counter.value & ((1.U((UsefulCtrWidth + 1).W) << width) - 1.U)(UsefulCtrWidth - 1, 0)
  }

  def normalizeUsefulCtr(counter: SaturateCounter, requestedWidth: UInt): SaturateCounter =
    SaturateCounterInit(UsefulCtrWidth, getActiveUsefulCtrValue(counter, requestedWidth))

  def usefulCtrIsSaturateNegative(counter: SaturateCounter, requestedWidth: UInt): Bool =
    !getActiveUsefulCtrValue(counter, requestedWidth).orR

  def usefulCtrIsSaturatePositive(counter: SaturateCounter, requestedWidth: UInt): Bool = {
    val value = getActiveUsefulCtrValue(counter, requestedWidth)
    val width = getActiveUsefulCtrWidth(requestedWidth)
    value === ((1.U((UsefulCtrWidth + 1).W) << width) - 1.U)(UsefulCtrWidth - 1, 0)
  }

  def getUsefulCtrIncrease(counter: SaturateCounter, requestedWidth: UInt, en: Bool): SaturateCounter = {
    val value = getActiveUsefulCtrValue(counter, requestedWidth)
    SaturateCounterInit(
      UsefulCtrWidth,
      Mux(en && !usefulCtrIsSaturatePositive(counter, requestedWidth), value + 1.U, value)
    )
  }

  def getUseAltOnNaIdx(pc: PrunedAddr): UInt = {
    val useAltOnNaIdxWidth = log2Ceil(NumUseAltOnNa)
    pc(useAltOnNaIdxWidth - 1 + instOffsetBits, instOffsetBits)
  }
}

trait TableHelper extends TopHelper { // extends TopHelper for getBankIndex
  // varies between different tables
  implicit val info: TageTableInfo

  val addrFields = AddrField(
    Seq(
      ("instOffset", instOffsetBits),
      ("bankIdx", BankIdxWidth),
      ("setIdx", SetIdxWidth),
      ("tag", TagWidth)
    ),
    maxWidth = Option(VAddrBits)
  )

  def getBankIndex(pc: PrunedAddr): UInt =
    addrFields.extract("bankIdx", pc)

  def getSetIndex(pc: PrunedAddr, hist: UInt): UInt =
    addrFields.extract("setIdx", pc) ^ hist

  def getPcTag(pc: PrunedAddr, requestedNumSetsLog2: UInt): UInt = {
    val activeSetIdxWidth = getActiveNumSetsLog2(requestedNumSetsLog2)
    (pc.toUInt >> (instOffsetBits.U + BankIdxWidth.U + activeSetIdxWidth))(TagWidth - 1, 0)
  }

  def getActiveNumSetsLog2(requestedNumSetsLog2: UInt): UInt =
    Mux(
      requestedNumSetsLog2 < MinNumSetsLog2.U,
      MinNumSetsLog2.U,
      Mux(requestedNumSetsLog2 > MaxNumSetsLog2.U, MaxNumSetsLog2.U, requestedNumSetsLog2)
    )

  def getActiveNumSets(requestedNumSetsLog2: UInt): UInt =
    UIntToOH(getActiveNumSetsLog2(requestedNumSetsLog2), SetIdxWidth + 1)

  def maskSetIndex(setIdx: UInt, requestedNumSetsLog2: UInt): UInt =
    setIdx & (getActiveNumSets(requestedNumSetsLog2) - 1.U)(SetIdxWidth - 1, 0)

  def getActiveNumWays(requestedNumWays: UInt): UInt =
    Mux(requestedNumWays < MinNumWays.U, MinNumWays.U, Mux(requestedNumWays > NumWays.U, NumWays.U, requestedNumWays))

  def getActiveWayMask(requestedNumWays: UInt): UInt =
    ((1.U((NumWays + 1).W) << getActiveNumWays(requestedNumWays)) - 1.U)(NumWays - 1, 0)

  def getActiveTagMask(requestedTagWidth: UInt): UInt =
    ((1.U((TagWidth + 1).W) << getActiveTagWidth(requestedTagWidth)) - 1.U)(TagWidth - 1, 0)

  def getRawTag(pc: PrunedAddr, hist: Vec[UInt], requestedNumSetsLog2: UInt): UInt =
    getPcTag(pc, requestedNumSetsLog2) ^ hist(0) ^ hist(1)

  def getTag(pc: PrunedAddr, hist: Vec[UInt], position: UInt, requestedNumSetsLog2: UInt): UInt =
    getRawTag(pc, hist, requestedNumSetsLog2) ^ position
}
