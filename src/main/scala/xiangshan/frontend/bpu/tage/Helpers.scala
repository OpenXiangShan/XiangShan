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
import xiangshan.frontend.bpu.TageTableInfo
import xiangshan.frontend.bpu.history.phr.PhrAllFoldedHistories

trait TopHelper extends HasTageParameters {
  def getFoldedHist(allFoldedPathHist: PhrAllFoldedHistories): Vec[TageFoldedHist] =
    VecInit(TableInfos.map { implicit tableInfo =>
      val tageFoldedHist = tableInfo.getTageFoldedHistoryInfo(NumBanks, TagWidth).map { histInfo =>
        allFoldedPathHist.getHistWithInfo(histInfo).foldedHist
      }
      val foldedHist = Wire(new TageFoldedHist)
      foldedHist.forIdx    := tageFoldedHist.head
      foldedHist.forTag(0) := tageFoldedHist(1)
      foldedHist.forTag(1) := Cat(tageFoldedHist(2), 0.U(1.W))
      foldedHist
    })

  def getLongestHistTableOH(hitTableMask: Seq[Bool]): Seq[Bool] =
    PriorityEncoderOH(hitTableMask.reverse).reverse

  private def encodeLocation(
      valid:        Bool,
      tableOH:      Seq[Bool],
      wayOH:        UInt,
      numTables:    Int,
      encodedWidth: Int
  ): UInt = {
    val encoded      = Wire(UInt(encodedWidth.W))
    val numLocations = TableInfos.take(numTables).map(_.NumWays).sum
    if (numTables == 0) {
      encoded := numLocations.U
    } else {
      val wayIdx = OHToUInt(wayOH)
      val flatLocation = Mux1H(
        tableOH.take(numTables),
        TableWayOffsets.take(numTables).map(offset => offset.U(encodedWidth.W) + wayIdx)
      )
      encoded := Mux(valid, flatLocation, numLocations.U)
    }
    encoded
  }

  private def decodeLocation(encoded: UInt, numTables: Int): (Bool, UInt, UInt) = {
    val numLocations = TableInfos.take(numTables).map(_.NumWays).sum
    val valid        = encoded < numLocations.U
    val flatLocation = encoded
    val tableOH = VecInit(TableInfos.zipWithIndex.map { case (info, tableIdx) =>
      if (tableIdx < numTables) {
        val offset = TableWayOffsets(tableIdx)
        valid && flatLocation >= offset.U && flatLocation < (offset + info.NumWays).U
      } else {
        false.B
      }
    })
    val wayOH = Mux1H(TableInfos.zipWithIndex.map { case (info, tableIdx) =>
      val localWayIdx = flatLocation - TableWayOffsets(tableIdx).U
      tableOH(tableIdx) -> UIntToOH(localWayIdx, info.NumWays).pad(MaxNumWays)
    })
    (valid, tableOH.asUInt, wayOH)
  }

  def encodeProviderLocation(valid: Bool, tableOH: Seq[Bool], wayOH: UInt): UInt =
    encodeLocation(valid, tableOH, wayOH, NumTables, ProviderLocationWidth)

  def encodeAltLocation(valid: Bool, tableOH: Seq[Bool], wayOH: UInt): UInt =
    encodeLocation(valid, tableOH, wayOH, NumTables - 1, AltLocationWidth)

  def decodeProviderLocation(encoded: UInt): (Bool, UInt, UInt) =
    decodeLocation(encoded, NumTables)

  def decodeAltLocation(encoded: UInt): (Bool, UInt, UInt) =
    decodeLocation(encoded, NumTables - 1)
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

  def getRawTag(pc: PrunedAddr, hist: Vec[UInt]): UInt =
    addrFields.extract("tag", pc) ^ hist(0) ^ hist(1)

  def getTag(pc: PrunedAddr, hist: Vec[UInt], position: UInt): UInt =
    addrFields.extract("tag", pc) ^ hist(0) ^ hist(1) ^ position
}
