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
import scala.math.min
import utility.ParallelXOR
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.PrunedAddrInit
import xiangshan.frontend.bpu.FoldedHistoryInfo
import xiangshan.frontend.bpu.HalfAlignHelper
import xiangshan.frontend.bpu.history.FoldedHistoryMaintenance

trait Helpers extends HasPhrParameters with HalfAlignHelper with FoldedHistoryMaintenance {
  // folded History
  def circularShiftLeft(src: UInt, shamt: Int): UInt = {
    val srcLen     = src.getWidth
    val srcDoubled = Cat(src, src)
    val shifted    = srcDoubled(srcLen * 2 - 1 - shamt, srcLen - shamt)
    shifted
  }

  // do xors for several bitsets at specified bits
  def bitsetsXor(info: FoldedHistoryInfo, bitsets: Seq[Seq[(Int, Bool)]]): UInt = {
    val res = Wire(Vec(info.FoldedLength, Bool()))
    // println(f"num bitsets: ${bitsets.length}")
    // println(f"bitsets $bitsets")
    val resArr = Array.fill(info.FoldedLength)(List[Bool]())
    for (bs <- bitsets) {
      for ((n, b) <- bs) {
        resArr(n) = b :: resArr(n)
      }
    }
    // println(f"${resArr.mkString}")
    // println(f"histLen: ${this.len}, foldedLen: $folded_len")
    for (i <- 0 until info.FoldedLength) {
      // println(f"bit[$i], ${resArr(i).mkString}")
      if (resArr(i).isEmpty) {
        println(f"[error] bits $i is not assigned in folded hist update logic! $info")
      }
      res(i) := resArr(i).foldLeft(false.B)(_ ^ _)
    }
    res.asUInt
  }

  def pathHash(pc: PrunedAddr, target: PrunedAddr): UInt = {
    val hash = Cat(pc(9, 1), 0.U(4.W)) ^ target(16, 2) // magic numbers
    hash(PathHashWidth - 1, 0)
  }

  def getPathHashComponents(pc: PrunedAddr, target: PrunedAddr): (UInt, UInt) = {
    val hash      = pathHash(pc, target)
    val shiftBits = hash(Shamt - 1, 0)
    val hashHigh  = hash(PathHashWidth - 1, Shamt)
    (shiftBits, hashHigh)
  }

  def computeFoldedHash(hashBits: UInt, compLen: Int)(histLen: Int): UInt =
    if (histLen > 0 && PathHashWidth >= histLen) {
      val effectiveBit = hashBits(histLen - 1, 0)
      val nChunks      = (histLen + compLen - 1) / compLen
      val histChunks   = (0 until nChunks) map { i => effectiveBit(min((i + 1) * compLen, histLen) - 1, i * compLen) }
      ParallelXOR(histChunks)
    } else if (histLen > 0 && PathHashWidth < histLen) {
      val nChunks    = (PathHashWidth + compLen - 1) / compLen
      val histChunks = (0 until nChunks) map { i => hashBits(min((i + 1) * compLen, PathHashWidth) - 1, i * compLen) }
      ParallelXOR(histChunks)
    } else 0.U

  def getUpdatePtrs(data: PhrUpdateData, hashHigh: UInt): PhrUpdateResult = {
    val updateResult = WireInit(0.U.asTypeOf(new PhrUpdateResult))
    when(data.valid) {
      updateResult.phrPtr     := data.phrMeta.phrPtr
      updateResult.phrLowBits := data.phrMeta.phrLowBits
      when(data.taken) {
        updateResult.phrPtr     := data.phrMeta.phrPtr - Shamt.U
        updateResult.phrLowBits := hashHigh ^ data.phrMeta.phrLowBits
      }
    }
    updateResult
  }

  // maxUpdateNum defaults to Shamt here, matching the default used by the 1-arg
  // PhrAllFoldedHistories constructor that this used to build directly
  def computeAllFoldedPhr(hist: UInt): PhrAllFoldedHistories =
    computeAllFoldedPhr(hist, AllFoldedHistoryInfo, Shamt)

  def getNextFoldedPhr(
      data:          PhrUpdateData,
      baseFoldedPhr: PhrAllFoldedHistories,
      basePhr:       UInt,
      hashHigh:      UInt,
      shiftBits:     UInt
  ): PhrAllFoldedHistories = {
    val nextFoldedPhr = WireInit(baseFoldedPhr)

    when(data.taken) {
      nextFoldedPhr := baseFoldedPhr.update(
        VecInit(basePhr.asBools),
        data.phrMeta.phrPtr,
        hashHigh,
        Shamt,
        shiftBits
      )
    }
    nextFoldedPhr
  }

  def getNextFoldedPhr(
      data:          PhrUpdateData,
      oldestBits:    PhrAllFoldedHistoryOldestBits,
      baseFoldedPhr: PhrAllFoldedHistories,
      hashHigh:      UInt,
      shiftBits:     UInt
  ): PhrAllFoldedHistories = {
    val nextFoldedPhr = WireInit(baseFoldedPhr)
    when(data.taken) {
      nextFoldedPhr := baseFoldedPhr.update(oldestBits, hashHigh, Shamt, shiftBits)
    }
    nextFoldedPhr
  }

  /** Advance every folded history by a whole group.
    *
    * Folding is linear over XOR, so the window's shift and the group's token fold apart and combine at the end. That
    * separation is what lets one step carry several blocks: the blocks' path hashes sit at different offsets within
    * the token, which a single hash-high argument could not express.
    */
  def foldGroup(
      baseFoldedPhr: PhrAllFoldedHistories,
      oldestBits:    PhrAllFoldedHistoryOldestBits,
      token:         UInt,
      insOH:         Seq[Bool]
  ): PhrAllFoldedHistories = {
    val res = WireInit(baseFoldedPhr)
    for (i <- baseFoldedPhr.hist.indices) {
      val h  = baseFoldedPhr.hist(i)
      val ob = oldestBits.hist(i).bits
      res.hist(i).foldedHist := Mux1H(
        insOH,
        Seq.tabulate(insOH.length) { n =>
          if (n == 0) h.foldedHist
          else {
            val shifted = h.update(ob, n * Shamt, 0.U((n * Shamt).W), 0.U(PathHashHighWidth.W)).foldedHist
            shifted ^ foldToken(token, h.info.FoldedLength, h.info.HistoryLength)
          }
        }
      )
    }
    res
  }
}
