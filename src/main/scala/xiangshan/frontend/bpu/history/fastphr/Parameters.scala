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

import chisel3.util._
import scala.math.min
import xiangshan.frontend.bpu.FoldedHistoryInfo
import xiangshan.frontend.bpu.history.phr.HasPhrParameters

// FastPhr caches a short window of the predicted path history so pTAGE can read
// resident folded-history registers without waiting on the full Phr pipeline.
case class FastPhrParameters(
    // per-table history span (bits) cached in FastPhr's shift-register window
    Spans: Seq[Int] = Seq(8, 20, 40),
    // folded width for the set-index-like fold, shared by every table
    IdxWidth: Int = 8,
    // folded width for the tag-like fold; the narrower tag fold uses TagWidth - 1
    TagWidth: Int = 8
) {
  require(Spans.nonEmpty, "FastPhr requires at least one table")
  require(Spans.forall(_ > 0), "every span must be > 0")
  require(IdxWidth > 0, "IdxWidth must be > 0")
  require(TagWidth > 1, "TagWidth must be > 1")
}

trait HasFastPhrParameters extends HasPhrParameters {
  def fastPhrParameters: FastPhrParameters = bpuParameters.fastPhrParameters

  def WindowLength: Int = fastPhrParameters.Spans.max
  def MaxUpdateNum: Int = 2 * Shamt

  // per table, an index-like fold and two tag-like folds (widths IdxWidth, TagWidth, TagWidth - 1),
  // each capped at the span; idx/tag widths are direct parameters here, not derived from a set count
  def FastFoldedHistoryInfo: Set[FoldedHistoryInfo] =
    fastPhrParameters.Spans.flatMap { span =>
      Seq(
        new FoldedHistoryInfo(span, min(span, fastPhrParameters.IdxWidth)),
        new FoldedHistoryInfo(span, min(span, fastPhrParameters.TagWidth)),
        new FoldedHistoryInfo(span, min(span, fastPhrParameters.TagWidth - 1))
      )
    }.toSet

  require(
    FastFoldedHistoryInfo.forall(_.FoldedLength >= MaxUpdateNum),
    s"every FastPhr folded length must be >= MaxUpdateNum ($MaxUpdateNum)"
  )
}
