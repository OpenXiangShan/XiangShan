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

package xiangshan.frontend.bpu

import chisel3.util._
import math.min

class TageTableInfo(
    val Size:          Int,
    val HistoryLength: Int,
    val NumWay:        Int
) {
  def asTuple: (Int, Int, Int) =
    (Size, HistoryLength, NumWay)

  // override equals and hashCode to allow comparison and Set[TageTableInfo] de-duplication
  override def equals(obj: Any): Boolean = obj match {
    case that: TageTableInfo =>
      this.asTuple.equals(that.asTuple)
    case _ => false
  }

  override def hashCode(): Int =
    asTuple.hashCode()

  // override toString for better readability
  override def toString: String =
    s"TageTableInfo(Size=$Size, HistoryLength=$HistoryLength, NumWay=$NumWay)"

  def getFoldedHistoryInfoSet(tagWidth: Int): Set[FoldedHistoryInfo] =
    if (HistoryLength > 0)
      Set(
        new FoldedHistoryInfo(HistoryLength, min(HistoryLength, log2Ceil(Size))),
        new FoldedHistoryInfo(HistoryLength, min(HistoryLength, tagWidth)),
        new FoldedHistoryInfo(HistoryLength, min(HistoryLength, tagWidth - 1))
      )
    else
      Set[FoldedHistoryInfo]()
}

class FoldedHistoryInfo(
    val HistoryLength: Int,
    val FoldedLength:  Int
) {
  require(HistoryLength > 0, "HistoryLength must be > 0")
  require(FoldedLength > 0, "FoldedLength must be > 0")

  def asTuple: (Int, Int) =
    (HistoryLength, FoldedLength)

  // override equals and hashCode to allow comparison and Set[FoldedHistoryInfo] de-duplication
  override def equals(obj: Any): Boolean = obj match {
    case that: FoldedHistoryInfo =>
      this.asTuple.equals(that.asTuple)
    case _ => false
  }

  override def hashCode(): Int =
    asTuple.hashCode()

  // override toString for better readability
  override def toString: String =
    s"FoldedHistoryInfo(HistoryLength=$HistoryLength, FoldedLength=$FoldedLength)"
}
