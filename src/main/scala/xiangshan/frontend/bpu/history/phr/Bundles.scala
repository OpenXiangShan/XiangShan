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

class PhrUpdateData(implicit p: Parameters) extends PhrBundle with HasPhrParameters {
  val valid:     Bool                  = Bool()
  val taken:     Bool                  = Bool()
  val pc:        PrunedAddr            = PrunedAddr(VAddrBits)
  val phrPtr:    PhrPtr                = new PhrPtr
  val foldedPhr: PhrAllFoldedHistories = new PhrAllFoldedHistories(AllFoldedHistoryInfo)
  // val target: PrunedAddr = PrunedAddr(VAddrBits)
}

class PhrUpdate(implicit p: Parameters) extends PhrBundle {
  // NOTE: if the StageCtrl structure changes, it may require refactoring
  val s0_stall:  Bool      = Bool()
  val stageCtrl: StageCtrl = new StageCtrl

  val redirect: Valid[BpuRedirect] = Valid(new BpuRedirect)

  val s1_valid:      Bool       = Bool()
  val s1_prediction: Prediction = new Prediction()
  val s1_pc:         PrunedAddr = PrunedAddr(VAddrBits)

  val s3_override:   Bool       = Bool()
  val s3_prediction: Prediction = new Prediction()
  val s3_pc:         PrunedAddr = PrunedAddr(VAddrBits)
}

// NOTE: Folded history maintenance logic reuse kmh-v2 ghr folded history management logic,
// with only minor modifications made for phr characteristics.
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

  def getOldestBitFromGhr(phr: Vec[Bool], histPtr: PhrPtr): Seq[Bool] =
    // TODO: wrap inc for histPtr value
    oldestBitToGetFromPhr.map(i => phr(i))

  // slow path, read bits from phr
  def update(phr: Vec[Bool], histPtr: PhrPtr, num: Int, shiftBits: UInt): PhrFoldedHistory = {
    val oldestBits = VecInit(getOldestBitFromGhr(phr, histPtr))
    update(oldestBits, num, shiftBits)
  }

  // fast path, use pre-read oldest bits
  def update(ob: Vec[Bool], num: Int, shiftBits: UInt): PhrFoldedHistory = {
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

    val fh = WireInit(this)
    fh.foldedHist := newFoldedHist
    fh
  }
}

// class AheadFoldedHistoryOldestBits(val len: Int, val max_update_num: Int)(implicit p: Parameters) extends XSBundle {
//   val bits = Vec(max_update_num * 2, Bool())
//   // def info = (len, compLen)
//   def getRealOb(brNumOH: UInt): Vec[Bool] = {
//     val ob = Wire(Vec(max_update_num, Bool()))
//     for (i <- 0 until max_update_num) {
//       ob(i) := Mux1H(brNumOH, bits.drop(i).take(numBr + 1))
//     }
//     ob
//   }
// }

// class AllAheadFoldedHistoryOldestBits(val gen: Seq[Tuple2[Int, Int]])(implicit p: Parameters) extends PhrBundle {
//   val afhob = MixedVec(gen.filter(t => t._1 > t._2).map(_._1)
//     .toSet.toList.map(l => new AheadFoldedHistoryOldestBits(l, numBr))) // remove duplicates
//   require(gen.toSet.toList.equals(gen))
//   def getObWithInfo(info: Tuple2[Int, Int]) = {
//     val selected = afhob.filter(_.len == info._1)
//     require(selected.length == 1)
//     selected(0)
//   }
//   def read(ghv: Vec[Bool], ptr: PhrPtr) = {
//     val hisLens      = afhob.map(_.len)
//     val bitsToRead   = hisLens.flatMap(l => (0 until numBr * 2).map(i => l - i - 1)).toSet // remove duplicates
//     val bitsWithInfo = bitsToRead.map(pos => (pos, ghv((ptr + (pos + 1).U).value)))
//     for (ob <- afhob) {
//       for (i <- 0 until numBr * 2) {
//         val pos       = ob.len - i - 1
//         val bit_found = bitsWithInfo.filter(_._1 == pos).toList
//         require(bit_found.length == 1)
//         ob.bits(i) := bit_found(0)._2
//       }
//     }
//   }
// }

class PhrAllFoldedHistories(gen: Set[FoldedHistoryInfo])(implicit p: Parameters) extends PhrBundle
    with HasPhrParameters {

  val hist: MixedVec[PhrFoldedHistory] =
    MixedVec(gen.toSeq.sortBy(_.asTuple).map(info => new PhrFoldedHistory(info, Shamt)))

  def getHistWithInfo(info: FoldedHistoryInfo): PhrFoldedHistory = {
    val selected = hist.filter(_.info.equals(info))
    require(selected.length == 1)
    selected.head
  }

  def autoConnectFrom(that: PhrAllFoldedHistories): Unit = {
    require(this.hist.length <= that.hist.length)
    for (h <- this.hist) {
      h := that.getHistWithInfo(h.info)
    }
  }

  def update(ghv: Vec[Bool], ptr: PhrPtr, shift: Int, shiftBits: UInt): PhrAllFoldedHistories = {
    require(shiftBits.getWidth == shift)
    val res = WireInit(this)
    for (i <- this.hist.indices) {
      res.hist(i) := this.hist(i).update(ghv, ptr, shift, shiftBits)
    }
    res
  }
  // TODO: Enable ahead logic
  // def update(afhob: AllAheadFoldedHistoryOldestBits, lastBrNumOH: UInt, shift: Int, taken: Bool): AllFoldedHistories = {
  //   val res = WireInit(this)
  //   for (i <- 0 until this.hist.length) {
  //     val fh = this.hist(i)
  //     if (fh.needOldestBits) {
  //       val info          = fh.info
  //       val selectedAfhob = afhob.getObWithInfo(info)
  //       val ob            = selectedAfhob.getRealOb(lastBrNumOH)
  //       res.hist(i) := this.hist(i).update(ob, shift, taken)
  //     } else {
  //       val dumb = Wire(Vec(numBr, Bool())) // not needed
  //       dumb        := DontCare
  //       res.hist(i) := this.hist(i).update(dumb, shift, taken)
  //     }
  //   }
  //   res
  // }

  def display(cond: Bool): Unit =
    for (h <- hist) {
      XSDebug(
        cond,
        p"hist len ${h.info.HistoryLength}, folded len ${h.info.FoldedLength}, value ${Binary(h.foldedHist)}\n"
      )
    }
}
