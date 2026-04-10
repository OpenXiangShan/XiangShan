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
import utils.AddrField
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.FoldedHistoryInfo
import xiangshan.frontend.bpu.PhrHelper
import xiangshan.frontend.bpu.ScTableInfo
import xiangshan.frontend.bpu.history.phr.PhrAllFoldedHistories

trait Helpers extends HasScParameters with PhrHelper {
  def sign(x: SInt): Bool = x(x.getWidth - 1)
  def pos(x:  SInt): Bool = !sign(x)
  def neg(x:  SInt): Bool = sign(x)

  protected def generateAddrField(setIdxWidth: Option[Int] = None): AddrField = AddrField(
    Seq(
      ("shiftBit", FetchBlockAlignWidth),
      ("bankIdx", BankWidth)
    ) ++ (if (setIdxWidth.isDefined) Seq(("setIdx", setIdxWidth.get)) else Seq()),
    maxWidth = Option(VAddrBits)
  )

  lazy val addrFields = generateAddrField()

  // sc should start using startPc as setIdx from the highest bit of CfiPosition
  def getBankMask(pc: PrunedAddr): UInt =
    UIntToOH(addrFields.extract("bankIdx", pc))

  def getWayIdx(cfiPosition: UInt): UInt = {
    val nChunks = (cfiPosition.getWidth + log2Ceil(NumWays) - 1) / log2Ceil(NumWays)
    val hashChunks = (0 until nChunks) map { i =>
      cfiPosition(min((i + 1) * log2Ceil(NumWays), cfiPosition.getWidth) - 1, i * log2Ceil(NumWays))
    }
    ParallelXOR(hashChunks)
  }

  def getPercsum(ctr: SInt): SInt = Cat(ctr, 1.U(1.W)).asSInt

  def aboveThreshold(scSum: SInt, threshold: UInt): Bool =
    ((scSum > threshold.zext) && pos(scSum)) || ((scSum < -threshold.zext) && neg(scSum))

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
        writeValidVec.length == wayIdxVec.length &&
        writeValidVec.length == branchIdxVec.length,
      "Length of writeValidVec, takenMask, wayIdxVec and branchIdxVec should be the same"
    )
    val newEntries = Wire(Vec(oldEntries.length, new ScEntry()))
    // For each resolved branch, record its update direction, update requirement, and target way.
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

trait AbstractTableHelper extends Helpers {
  protected def TableInfo: ScTableInfo

  final protected def NumSets: Int = TableInfo.NumSets

  final protected def SetIdxWidth: Int = log2Ceil(NumSets)

  override lazy val addrFields = generateAddrField(Option(SetIdxWidth))
}

trait PathTableHelper extends AbstractTableHelper {

  def getPathTableIdx(pc: PrunedAddr, info: FoldedHistoryInfo, allFh: PhrAllFoldedHistories): UInt =
    if (info.HistoryLength > 0) {
      val idxFoldedHist = allFh.getHistWithInfo(info).foldedHist
      addrFields.extract("setIdx", pc) ^ idxFoldedHist
    } else {
      addrFields.extract("setIdx", pc)
    }
}

trait CommonTableHelper extends AbstractTableHelper {
  final protected def HistoryLength: Int = TableInfo.HistoryLength

  // get pc ^ foldedHist for index
  // ghr/imli/bw using getTableIdx to calculate setIdx
  def getTableIdx(pc: PrunedAddr, hist: UInt): UInt = {
    val foldedHist = computeFoldedHist(hist, SetIdxWidth)(HistoryLength)
    addrFields.extract("setIdx", pc) ^ foldedHist
  }
}

trait BiasTableHelper extends AbstractTableHelper {

  def getBiasTableIdx(pc: PrunedAddr): UInt =
    addrFields.extract("setIdx", pc)
}
