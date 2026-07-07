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
import org.chipsalliance.cde.config.Parameters
import xiangshan.frontend.bpu.FoldedHistoryInfo
import xiangshan.frontend.bpu.PhrHelper
import xiangshan.frontend.bpu.history.phr.PhrAllFoldedHistories

// shared by Phr and FastPhr: rebuilds a full folded-history set from scratch out of a raw
// path-history window, folding each table's FoldedHistoryInfo independently
trait FoldedHistoryMaintenance extends PhrHelper {
  def computeAllFoldedPhr(window: UInt, gen: Set[FoldedHistoryInfo], maxUpdateNum: Int)(implicit
      p: Parameters
  ): PhrAllFoldedHistories = {
    val foldedPhr = WireInit(0.U.asTypeOf(new PhrAllFoldedHistories(gen, maxUpdateNum)))
    gen.foreach { info =>
      foldedPhr.getHistWithInfo(info).foldedHist :=
        computeFoldedHist(window, info.FoldedLength)(info.HistoryLength)
    }
    foldedPhr
  }
}
