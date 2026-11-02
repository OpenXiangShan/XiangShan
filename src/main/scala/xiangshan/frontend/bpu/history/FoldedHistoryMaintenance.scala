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

package xiangshan.frontend.bpu.history

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.frontend.bpu.FoldedHistoryInfo
import xiangshan.frontend.bpu.PhrHelper
import xiangshan.frontend.bpu.history.phr.HasPhrParameters
import xiangshan.frontend.bpu.history.phr.PhrAllFoldedHistories

// shared by Phr and FastPhr, which keep the same path history and must advance it identically
trait FoldedHistoryMaintenance extends PhrHelper with HasPhrParameters {
  // rebuilds a full folded-history set from scratch out of a raw path-history window, folding each table's
  // FoldedHistoryInfo independently
  def computeAllFoldedPhr(window: UInt, gen: Set[FoldedHistoryInfo])(implicit
      p: Parameters
  ): PhrAllFoldedHistories = {
    val foldedPhr = WireInit(0.U.asTypeOf(new PhrAllFoldedHistories(gen)))
    gen.foreach { info =>
      foldedPhr.getHistWithInfo(info).foldedHist :=
        computeFoldedHist(window, info.FoldedLength)(info.HistoryLength)
    }
    foldedPhr
  }

  // How much of a group's token n taken blocks reach: one whole path hash, plus one Shamt per block past the first.
  def groupToken(token: UInt, n: Int): UInt =
    if (n == 0) 0.U else token(PathHashWidth + (n - 1) * Shamt - 1, 0)

  // Shift a window by one Shamt per taken block and XOR the group's token over its newest bits, with the number of
  // taken blocks one-hot in insOH so that every shift is a fixed one.
  def advanceWindow(base: UInt, token: UInt, insOH: Seq[Bool], width: Int): UInt =
    Mux1H(
      insOH,
      Seq.tabulate(insOH.length)(n => ((base << (n * Shamt)).asUInt ^ groupToken(token, n))(width - 1, 0))
    )

  /** Advance every folded history by a whole group, as advanceWindow advances the window it folds.
    *
    * Folding is linear over XOR, so the window's shift and the group's token fold apart and combine at the end. That
    * separation is what lets one step carry several blocks: the blocks' path hashes sit at different offsets within
    * the token, which a single hash-high argument could not express. oldestBits(i) holds the bits that leave
    * baseFoldedPhr.hist(i)'s span.
    */
  def foldGroup(
      baseFoldedPhr: PhrAllFoldedHistories,
      oldestBits:    Seq[Vec[Bool]],
      token:         UInt,
      insOH:         Seq[Bool]
  ): PhrAllFoldedHistories = {
    val res = WireInit(baseFoldedPhr)
    for (i <- baseFoldedPhr.hist.indices) {
      val h = baseFoldedPhr.hist(i)
      res.hist(i).foldedHist := Mux1H(
        insOH,
        Seq.tabulate(insOH.length) { n =>
          if (n == 0) h.foldedHist
          else {
            val shifted = h.update(oldestBits(i), n * Shamt, 0.U((n * Shamt).W), 0.U(PathHashHighWidth.W)).foldedHist
            // bits past the span are dropped: they leave the window as they arrive
            val bits = groupToken(token, n)
            shifted ^ computeFoldedHist(bits, h.info.FoldedLength)(scala.math.min(bits.getWidth, h.info.HistoryLength))
          }
        }
      )
    }
    res
  }
}
