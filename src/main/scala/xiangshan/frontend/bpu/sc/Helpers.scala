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

package xiangshan.frontend.bpu.sc

import chisel3._
import chisel3.util._
import xiangshan.HasXSParameter
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.FoldedHistoryInfo
import xiangshan.frontend.bpu.PhrHelper
import xiangshan.frontend.bpu.history.phr.PhrAllFoldedHistories

trait Helpers extends HasScParameters with PhrHelper {
  def sign(x: SInt): Bool = x(x.getWidth - 1)
  def pos(x:  SInt): Bool = !sign(x)
  def neg(x:  SInt): Bool = sign(x)

  def getBankMask(pc: PrunedAddr): UInt =
    UIntToOH((pc >> (instOffsetBits + log2Ceil(NumWays)))(BankWidth - 1, 0))

//  def getWayIdx(cfiPosition: UInt): UInt = cfiPosition(log2Ceil(NumWays) - 1, 0)
  def getWayOH(cfiPosition: UInt): UInt =
    // hash from cfiPosition bits to NumWays bits
    // NumWays may not be 2^n
    VecInit((0 until (1 << CfiPositionWidth)).map(i => UIntToOH((i % NumWays).U, NumWays)))(cfiPosition)

  // get pc ^ foldedHist for index
  def getPathTableIdx(pc: PrunedAddr, info: FoldedHistoryInfo, allFh: PhrAllFoldedHistories, numSets: Int): UInt =
    if (info.HistoryLength > 0) {
      val idxFoldedHist = allFh.getHistWithInfo(info).foldedHist
      ((pc >> (instOffsetBits + log2Ceil(NumWays) + BankWidth)) ^ idxFoldedHist)(log2Ceil(numSets) - 1, 0)
    } else {
      (pc >> (instOffsetBits + log2Ceil(NumWays) + BankWidth))(log2Ceil(numSets) - 1, 0)
    }

  // get pc ^ foldedGhr for index
  def getGlobalTableIdx(pc: PrunedAddr, ghr: UInt, numSets: Int, ghrLen: Int): UInt = {
    val foldedGhr = computeFoldedHist(ghr, log2Ceil(numSets))(ghrLen)
    ((pc >> (instOffsetBits + log2Ceil(NumWays) + BankWidth)) ^ foldedGhr)(log2Ceil(numSets) - 1, 0)
  }

  // get pc ^ foldedBW for index
  def getBWTableIdx(pc: PrunedAddr, bw: UInt, numSets: Int, bwLen: Int): UInt = {
    val foldedBW = computeFoldedHist(bw, log2Ceil(numSets))(bwLen)
    ((pc >> (instOffsetBits + log2Ceil(NumWays) + BankWidth)) ^ foldedBW)(log2Ceil(numSets) - 1, 0)
  }

  // get bias index
  def getBiasTableIdx(pc: PrunedAddr, numSets: Int): UInt =
    (pc >> (instOffsetBits + log2Ceil(NumWays) + BankWidth))(log2Ceil(numSets) - 1, 0)

  def getPercsum(ctr: SInt): SInt = Cat(ctr, 1.U(1.W)).asSInt

  def aboveThreshold(scSum: SInt, threshold: UInt): Bool =
    (scSum > threshold.zext) && pos(scSum) || (scSum < -threshold.zext) && neg(scSum)

  // Accumulate update information for multiple branches using update methods
  def updateEntry(
      oldEntries:    Vec[ScEntry],
      writeValidVec: Vec[Bool],
      takenMask:     Vec[Bool],
//      wayIdxVec:     Vec[UInt],
      wayOHVec: Vec[UInt],
//      branchIdxVec: Vec[UInt],
      branchOHVec: Vec[UInt],
      metaData:    ScMeta
  ): Vec[ScEntry] = {
    require(
      writeValidVec.length == takenMask.length &&
        writeValidVec.length == wayOHVec.length,
      "Length of writeValidVec, takenMask and wayOHVec should be the same"
    )
    val newEntries = Wire(Vec(oldEntries.length, new ScEntry()))
    oldEntries.zip(newEntries).zipWithIndex.foreach { case ((oldEntry, newEntry), wayIdx) =>
      val newCtr = writeValidVec.zip(takenMask).zip(wayOHVec).zip(branchOHVec).foldLeft(oldEntry.ctr) {
        case (prevCtr, (((writeValid, writeTaken), writeWayOH), branchOH)) =>
          val tagePredValid = Mux1H(branchOH, metaData.tagePredValid)
          val scPred        = Mux1H(branchOH, metaData.scPred)
          val sumAboveThres = Mux1H(branchOH, metaData.sumAboveThres)
          val needUpdate = writeValid && writeWayOH === UIntToOH(wayIdx.U, NumWays) && tagePredValid &&
            (scPred =/= writeTaken || !sumAboveThres)
          prevCtr.getUpdate(
            writeTaken,
            needUpdate
          )
      }
      newEntry.ctr := WireInit(newCtr)
    }
    newEntries
  }

  def updateWayMask(
      oldEntries:    Vec[ScEntry],
      newEntries:    Vec[ScEntry],
      writeValidVec: Vec[Bool],
//      wayIdxVec:     Vec[UInt]
      wayOHVec: Vec[UInt]
  ): Vec[Bool] = {
    require(
      writeValidVec.length == wayOHVec.length,
      "Length of writeValidVec and wayIdxVec should be the same"
    )
    val updateWayMask = WireInit(VecInit.fill(NumWays)(false.B))
    writeValidVec.zip(wayOHVec).foreach {
      case (writeValid, wayOH) =>
//        when(writeValid && (Mux1H(wayOH, oldEntries).ctr.value =/= Mux1H(wayOH, newEntries).ctr.value)) {
//          updateWayMask(wayIdx) := true.B
//        }
        updateWayMask.zipWithIndex.foreach { case (bit, wayIdx) =>
          when(wayOH(wayIdx) && writeValid && (oldEntries(wayIdx).ctr.value =/= newEntries(wayIdx).ctr.value)) {
            bit := true.B
          }
        }
    }
    updateWayMask
  }
}
