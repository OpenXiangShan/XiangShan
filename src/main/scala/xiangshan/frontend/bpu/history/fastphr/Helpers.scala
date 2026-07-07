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

package xiangshan.frontend.bpu.history.fastphr

import chisel3._
import chisel3.util._
import xiangshan.frontend.bpu.history.FoldedHistoryMaintenance
import xiangshan.frontend.bpu.history.phr.PhrAllFoldedHistories

trait Helpers extends HasFastPhrParameters with FoldedHistoryMaintenance {
  // the top MaxUpdateNum bits of a table's span window, oldest bit first: ob(i) is the history
  // bit at unfolded position span - 1 - i, mirroring PhrFoldedHistory.oldestBitToGetFromPhr
  def getOldBits(phr: UInt, span: Int): Vec[Bool] = {
    require(span >= MaxUpdateNum, s"span must be >= MaxUpdateNum ($MaxUpdateNum), got $span")
    VecInit((0 until MaxUpdateNum).map(i => phr(span - 1 - i)))
  }

  // advances every table's resident folded history by 0, 1 or 2 blocks as selected by numBlocksOH.
  // for each table, 3 fold candidates are built (unchanged / advance Shamt / advance 2*Shamt) and
  // the result is an end mux over those candidates, rather than folding twice in sequence
  def foldStep(
      foldedQ:     PhrAllFoldedHistories,
      phr:         UInt,
      token:       UInt,
      numBlocksOH: Vec[Bool]
  ): PhrAllFoldedHistories = {
    require(numBlocksOH.length == 3, "numBlocksOH must be one-hot over {0, 1, 2} blocks")

    val noHashHigh = 0.U(PathHashHighWidth.W) // no path-hash mixing, only the raw history fold
    val res        = WireInit(foldedQ)
    for (i <- foldedQ.hist.indices) {
      val h  = foldedQ.hist(i)
      val ob = getOldBits(phr, h.info.HistoryLength)

      val candidate0 = h.foldedHist // 0 blocks: identity, no update() call
      val candidate1 = h.update(ob, Shamt, token(Shamt - 1, 0), noHashHigh).foldedHist
      val candidate2 = h.update(ob, 2 * Shamt, token(2 * Shamt - 1, 0), noHashHigh).foldedHist

      res.hist(i).foldedHist := Mux1H(numBlocksOH, Seq(candidate0, candidate1, candidate2))
    }
    res
  }

  // rebuilds every table's resident folded history from scratch given a raw window; used by
  // redirect recovery to reload FastPhr's state from an externally reconstructed window
  def refold(window: UInt): PhrAllFoldedHistories =
    computeAllFoldedPhr(window, FastFoldedHistoryInfo, MaxUpdateNum)
}
