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

package xiangshan.frontend.bpu.history.phr

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.CircularQueuePtr
import utility.XSDebug
import xiangshan.XSCoreParamsKey
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.BpuRedirect
import xiangshan.frontend.bpu.FoldedHistoryInfo
import xiangshan.frontend.bpu.Prediction
import xiangshan.frontend.bpu.StageCtrl

class PhrPtr(implicit p: Parameters) extends CircularQueuePtr[PhrPtr](p =>
      p(XSCoreParamsKey).frontendParameters.getPhrHistoryLength
    ) {}

object PhrPtr {
  def apply(f: Bool, v: UInt)(implicit p: Parameters): PhrPtr = {
    val ptr = Wire(new PhrPtr)
    ptr.flag  := f
    ptr.value := v
    ptr
  }
  def inverse(ptr: PhrPtr)(implicit p: Parameters): PhrPtr =
    apply(!ptr.flag, ptr.value)
}

class S1Train(implicit p: Parameters) extends PhrBundle with HasPhrParameters {
  val valid:              Bool                   = Bool()
  val taken:              Bool                   = Bool() // actual s1_taken
  val startPc:            PrunedAddr             = PrunedAddr(VAddrBits)
  val abtbValid:          Bool                   = Bool()
  val abtbFirstTakenBrOH: Vec[Bool]              = Vec(NumAheadBtbPredictionEntries, Bool())
  val ubtbPrediction:     Valid[Prediction]      = Valid(new Prediction)
  val abtbPrediction:     Vec[Valid[Prediction]] = Vec(NumAheadBtbPredictionEntries, Valid(new Prediction))
}

class S3Train(implicit p: Parameters) extends PhrBundle with HasPhrParameters {
  val valid:          Bool                   = Bool()
  val taken:          Bool                   = Bool() // actual s3_taken
  val startPc:        PrunedAddr             = PrunedAddr(VAddrBits)
  val firstTakenBrOH: Vec[Bool]              = Vec(NumBtbResultEntries, Bool())
  val phrMeta:        PhrMeta                = new PhrMeta()
  val s3Prediction:   Vec[Valid[Prediction]] = Vec(NumBtbResultEntries, Valid(new Prediction))
}

class PhrUpdateData(implicit p: Parameters) extends PhrBundle with HasPhrParameters {
  val valid:   Bool       = Bool()
  val taken:   Bool       = Bool()
  val cfiPc:   PrunedAddr = PrunedAddr(VAddrBits)
  val target:  PrunedAddr = PrunedAddr(VAddrBits)
  val phrMeta: PhrMeta    = new PhrMeta()
}

class PhrUpdateResult(implicit p: Parameters) extends PhrBundle with HasPhrParameters {
  val phrPtr:     PhrPtr = new PhrPtr
  val phrLowBits: UInt   = UInt(PathHashHighWidth.W)
}

class PhrUpdate(implicit p: Parameters) extends PhrBundle {
  // NOTE: if the StageCtrl structure changes, it may require refactoring
  val s0_stall:  Bool               = Bool()
  val stageCtrl: StageCtrl          = new StageCtrl
  val redirect:  Valid[BpuRedirect] = Valid(new BpuRedirect)
}

class PhrMeta(implicit p: Parameters) extends PhrBundle {
  val phrPtr:     PhrPtr = new PhrPtr
  val phrLowBits: UInt   = UInt(PathHashHighWidth.W)

  // for debug
  val predFoldedHist: Option[PhrAllFoldedHistories] =
    Option.when(!env.FPGAPlatform)(new PhrAllFoldedHistories(AllFoldedHistoryInfo))
}

// NOTE: Folded history maintenance logic reuses kmh-v2 GHR folded history management logic,
// with only minor modifications made for PHR characteristics.
class PhrFoldedHistory(val info: FoldedHistoryInfo, val maxUpdateNum: Int)(implicit p: Parameters)
    extends PhrBundle with Helpers {
  // require(folded_len <= len)
  require(info.FoldedLength >= maxUpdateNum)

  val foldedHist: UInt = UInt(info.FoldedLength.W)

  def needOldestBits: Boolean = info.HistoryLength > info.FoldedLength

  def oldestBitToGetFromPhr: Seq[Int]     = (0 until maxUpdateNum).map(info.HistoryLength - _ - 1)
  def oldestBitPosInFolded:  Seq[Int]     = oldestBitToGetFromPhr.map(_ % info.FoldedLength)
  def oldestBitWrapAround:   Seq[Boolean] = oldestBitToGetFromPhr.map(_ / info.FoldedLength > 0)
  def oldestBitStart:        Int          = oldestBitPosInFolded.head

  def getOldestBitFromPhr(phr: Vec[Bool], histPtr: PhrPtr): Seq[Bool] =
    // TODO: wrap inc for histPtr value
    oldestBitToGetFromPhr.map(i => phr(i))

  // slow path, read bits from phr
  def update(phr: Vec[Bool], histPtr: PhrPtr, hashHigh: UInt, num: Int, shiftBits: UInt): PhrFoldedHistory = {
    val oldestBits = VecInit(getOldestBitFromPhr(phr, histPtr))
    update(oldestBits, num, shiftBits, hashHigh)
  }

  // fast path, use pre-read oldest bits
  def update(ob: Vec[Bool], num: Int, shiftBits: UInt, hashHigh: UInt): PhrFoldedHistory = {
    val newFoldedHist = if (needOldestBits) {
      val oldestBits = ob
      require(oldestBits.length == maxUpdateNum)
      // mask off bits that do not update
      val oldestBitsMasked = oldestBits.zipWithIndex.map {
        case (ob, i) => ob && (i < num).B
      }
      // if a bit does not wrap around, it should not be xored when it exits
      val oldestBitsSet = (0 until maxUpdateNum).filter(oldestBitWrapAround).map(i =>
        (oldestBitPosInFolded(i), oldestBitsMasked(i))
      )

      // println(f"old bits pos ${oldestBitsSet.map(_._1)}")

      // only the last bit could be 1, as we have at most one taken branch at a time
      val newestBitsMasked = shiftBits
      // val newestBitsMasked = VecInit((0 until maxUpdateNum).map(i => taken && ((i + 1) == num).B)).asUInt
      // if a bit does not wrap around, newest bits should not be xored onto it either
      val newestBitsSet = (0 until maxUpdateNum).map(i => (info.FoldedLength - 1 - i, newestBitsMasked(num - i - 1)))

      // println(f"new bits set ${newestBitsSet.map(_._1)}")
      //
      val originalBitsMasked = VecInit(foldedHist.asBools.zipWithIndex.map {
        case (fb, i) => fb && !(num >= (info.HistoryLength - i)).B
      })
      val originalBitsSet = (0 until info.FoldedLength).map(i => (i, originalBitsMasked(i)))

      // do xor then shift
      val xored = bitsetsXor(info, Seq(originalBitsSet, oldestBitsSet, newestBitsSet))
      circularShiftLeft(xored, num)
    } else {
      // histLen too short to wrap around
      ((foldedHist << num).asUInt | shiftBits)(info.FoldedLength - 1, 0).asUInt
    }

    val fh         = WireInit(this)
    val hashFolded = computeFoldedHash(Cat(hashHigh, 0.U(maxUpdateNum.W)), info.FoldedLength)(info.HistoryLength)
    dontTouch(newFoldedHist)
    dontTouch(hashFolded)
    fh.foldedHist := newFoldedHist ^ hashFolded
    fh
  }
}

class PhrFoldedHistoryOldestBits(val info: FoldedHistoryInfo, val maxUpdateNum: Int)(implicit p: Parameters)
    extends PhrBundle {
  val bits: Vec[Bool] = Vec(maxUpdateNum, Bool())

  def oldestBitToGetFromPhr: Seq[Int] = (0 until maxUpdateNum).map(info.HistoryLength - _ - 1)

  def readFromPhr(phr: Vec[Bool], histPtr: PhrPtr): Unit =
    bits := VecInit(oldestBitToGetFromPhr.map(i => phr(i)))
}

class PhrAllFoldedHistoryOldestBits(gen: Set[FoldedHistoryInfo])(implicit p: Parameters) extends PhrBundle
    with HasPhrParameters {

  val allOldestBits: MixedVec[PhrFoldedHistoryOldestBits] =
    MixedVec(gen.toSeq.sortBy(_.asTuple).map(info => new PhrFoldedHistoryOldestBits(info, Shamt)))

  def getHistWithInfo(info: FoldedHistoryInfo): PhrFoldedHistoryOldestBits = {
    val selected = allOldestBits.filter(_.info.equals(info))
    require(selected.length == 1)
    selected.head
  }

  def read(phr: Vec[Bool], ptr: PhrPtr): Unit =
    for (h <- allOldestBits) {
      h.readFromPhr(phr, ptr)
    }
}

class PhrAllFoldedHistories(gen: Set[FoldedHistoryInfo])(implicit p: Parameters) extends PhrBundle
    with HasPhrParameters with Helpers {

  val hist: MixedVec[PhrFoldedHistory] =
    MixedVec(gen.toSeq.sortBy(_.asTuple).map(info => new PhrFoldedHistory(info, Shamt)))

  def getHistWithInfo(info: FoldedHistoryInfo): PhrFoldedHistory = {
    val selected = hist.filter(_.info.equals(info))
    require(selected.length == 1)
    selected.head
  }

  def update(phr: Vec[Bool], ptr: PhrPtr, hashHigh: UInt, shift: Int, shiftBits: UInt): PhrAllFoldedHistories = {
    require(shiftBits.getWidth == shift)
    val res = WireInit(this)
    for (i <- this.hist.indices) {
      res.hist(i) := this.hist(i).update(phr, ptr, hashHigh, shift, shiftBits)
    }
    res
  }

  def update(
      oldestBits: PhrAllFoldedHistoryOldestBits,
      hashHigh:   UInt,
      shift:      Int,
      shiftBits:  UInt
  ): PhrAllFoldedHistories = {
    require(shiftBits.getWidth == shift)
    require(hist.length == oldestBits.allOldestBits.length)
    val res = WireInit(this)
    for (i <- this.hist.indices) {
      val info = this.hist(i).info
      res.hist(i) := this.hist(i).update(oldestBits.getHistWithInfo(info).bits, shift, shiftBits, hashHigh)
    }
    res
  }

  def display(cond: Bool): Unit =
    for (h <- hist) {
      XSDebug(
        cond,
        p"hist len ${h.info.HistoryLength}, folded len ${h.info.FoldedLength}, value ${Binary(h.foldedHist)}\n"
      )
    }
}
