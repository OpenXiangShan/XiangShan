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
import xiangshan.frontend.bpu.TageTableInfo
import xiangshan.frontend.bpu.history.phr.PhrAllFoldedHistories

trait TopHelper extends HasTageParameters {
  def getFoldedHist(allFoldedPathHist: PhrAllFoldedHistories): Vec[TageFoldedHist] =
    VecInit(TableInfos.map { implicit tableInfo =>
      val tageFoldedHist = tableInfo.getTageFoldedHistoryInfo(NumBanks, TagWidth).map { histInfo =>
        allFoldedPathHist.getHistWithInfo(histInfo).foldedHist
      }
      val foldedHist = Wire(new TageFoldedHist)
      foldedHist.forIdx := tageFoldedHist.head
      foldedHist.forTag := tageFoldedHist(1) ^ Cat(tageFoldedHist(2), 0.U(1.W))
      foldedHist
    })

  def getLongestHistTableOH(hitTableMask: Seq[Bool]): Seq[Bool] =
    PriorityEncoderOH(hitTableMask.reverse).reverse

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

  def getRawTag(pc: PrunedAddr, hist: UInt): UInt =
    addrFields.extract("tag", pc) ^ hist
}
