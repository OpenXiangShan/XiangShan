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
import scala.math.min
import utility.ParallelXOR
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
    UIntToOH((pc >> instOffsetBits)(BankWidth - 1, 0))

  def getWayIdx(cfiPosition: UInt): UInt = {
    val nChunks = (cfiPosition.getWidth + log2Ceil(NumWays) - 1) / log2Ceil(NumWays)
    val hashChunks = (0 until nChunks) map { i =>
      cfiPosition(min((i + 1) * log2Ceil(NumWays), cfiPosition.getWidth) - 1, i * log2Ceil(NumWays))
    }
    ParallelXOR(hashChunks)
  }

  // get pc ^ foldedHist for index
  def getPathTableIdx(pc: PrunedAddr, info: FoldedHistoryInfo, allFh: PhrAllFoldedHistories, numSets: Int): UInt =
    if (info.HistoryLength > 0) {
      val idxFoldedHist = allFh.getHistWithInfo(info).foldedHist
      ((pc >> (instOffsetBits + BankWidth)) ^ idxFoldedHist)(log2Ceil(numSets) - 1, 0)
    } else {
      (pc >> (instOffsetBits + BankWidth))(log2Ceil(numSets) - 1, 0)
    }

  // get pc ^ foldedGhr for index
  def getGlobalTableIdx(pc: PrunedAddr, ghr: UInt, numSets: Int, ghrLen: Int): UInt = {
    val foldedGhr = computeFoldedHist(ghr, log2Ceil(numSets))(ghrLen)
    ((pc >> (instOffsetBits + BankWidth)) ^ foldedGhr)(log2Ceil(numSets) - 1, 0)
  }

  // get pc ^ foldedBW for index
  def getBWTableIdx(pc: PrunedAddr, bw: UInt, numSets: Int, bwLen: Int): UInt = {
    val foldedBW = computeFoldedHist(bw, log2Ceil(numSets))(bwLen)
    ((pc >> (instOffsetBits + BankWidth)) ^ foldedBW)(log2Ceil(numSets) - 1, 0)
  }

  // get pc ^ foldedImli index
  def getImliTableIdx(pc: PrunedAddr, imli: UInt, numSets: Int, imliLen: Int): UInt = {
    val foldedImli = computeFoldedHist(imli, log2Ceil(numSets))(imliLen)
    ((pc >> (instOffsetBits + BankWidth)) ^ foldedImli)(log2Ceil(numSets) - 1, 0)
  }

  // get bias index
  def getBiasTableIdx(pc: PrunedAddr, numSets: Int): UInt =
    (pc >> (instOffsetBits + BankWidth))(log2Ceil(numSets) - 1, 0)

  def getPercsum(ctr: SInt): SInt = Cat(ctr, 1.U(1.W)).asSInt

  def aboveThreshold(scSum: SInt, threshold: UInt): Bool =
    (scSum > threshold.zext) && pos(scSum) || (scSum < -threshold.zext) && neg(scSum)

  // Accumulate update information for multiple branches using update methods
  def updateEntry(
      oldEntries:    Vec[ScEntry],
      writeValidVec: Vec[Bool],
      takenMask:     Vec[Bool],
      wayIdxVec:     Vec[UInt],
      branchIdxVec:  Vec[UInt],
      metaData:      ScMeta
  ): Vec[ScEntry] = {
    require(
      writeValidVec.length == takenMask.length &&
        writeValidVec.length == wayIdxVec.length,
      "Length of writeValidVec, takenMask and wayIdxVec should be the same"
    )
    val newEntries = Wire(Vec(oldEntries.length, new ScEntry()))
    // For each reslove branch, record its update direction, whether it has been updated, and which way it has been updated to
    val writeNeedMask = VecInit(Seq.fill(writeValidVec.length)(VecInit(Seq.fill(oldEntries.length)(false.B))))
    val writeDirMask  = VecInit(Seq.fill(writeValidVec.length)(VecInit(Seq.fill(oldEntries.length)(false.B))))
    writeValidVec.zip(takenMask).zip(wayIdxVec).zip(branchIdxVec).zipWithIndex.foreach {
      case ((((valid, taken), writeIdx), oldIdx), i) =>
        val needUpdate = valid && metaData.tagePredValid(oldIdx) &&
          (metaData.scPred(oldIdx) =/= taken || !metaData.sumAboveThres(oldIdx))
        writeNeedMask(i)(writeIdx) := needUpdate
        writeDirMask(i)(writeIdx)  := taken
    }
    oldEntries.zip(newEntries).zipWithIndex.foreach { case ((oldEntry, newEntry), i) =>
      val writeHit = writeNeedMask.map(_(i))
      val writeDir = writeDirMask.map(_(i))
      val inc      = PopCount(writeHit.zip(writeDir).map { case (hit, dir) => hit && dir })
      val dec      = PopCount(writeHit.zip(writeDir).map { case (hit, dir) => hit && !dir })
      newEntry.ctr := Mux(inc >= dec, oldEntry.ctr.getIncrease(inc - dec), oldEntry.ctr.getDecrease(dec - inc))
    }
    newEntries
  }

  def updateWayMask(
      oldEntries:    Vec[ScEntry],
      newEntries:    Vec[ScEntry],
      writeValidVec: Vec[Bool],
      wayIdxVec:     Vec[UInt]
  ): Vec[Bool] = {
    require(
      writeValidVec.length == wayIdxVec.length,
      "Length of writeValidVec and wayIdxVec should be the same"
    )
    val updateWayMask = WireInit(VecInit.fill(NumWays)(false.B))
    writeValidVec.zip(wayIdxVec).foreach {
      case (writeValid, wayIdx) =>
        when(writeValid && (oldEntries(wayIdx).ctr.value =/= newEntries(wayIdx).ctr.value)) {
          updateWayMask(wayIdx) := true.B
        }
    }
    updateWayMask
  }
}
