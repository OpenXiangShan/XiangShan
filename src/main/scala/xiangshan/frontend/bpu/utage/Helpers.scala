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

package xiangshan.frontend.bpu.utage

import chisel3._
import chisel3.util._
import xiangshan.HasXSParameter
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.FoldedHistoryInfo
import xiangshan.frontend.bpu.HalfAlignHelper
import xiangshan.frontend.bpu.history.phr.PhrAllFoldedHistories

trait Helpers extends HasMicroTageParameters with HalfAlignHelper {
  private object TAGEHistoryType {
    val Short   = 0
    val Medium  = 1
    val Long    = 2
    val Unknown = 3
  }
  def getUnhashedIdx(pc: PrunedAddr): UInt = pc(VAddrBits - 1, instOffsetBits)
  def getUnhashedTag(pc: PrunedAddr): UInt = pc(VAddrBits - 1, log2Ceil(FetchBlockAlignSize))
  def connectPcTag(partPc: UInt, tableId: Int): UInt = {
    require(tableId >= 0 && tableId <= 3, s"tableId must be in [0,3], got $tableId")
    def concatBits(bits: Seq[Bool]): UInt = if (bits.isEmpty) 0.U(1.W) else bits.foldLeft(0.U(0.W))(Cat(_, _))
    val tagPC = partPc
    val hashPC = tableId match {
      case TAGEHistoryType.Short =>
        concatBits(PCTagHashBitsForShortHistory.map(tagPC(_)))

      case TAGEHistoryType.Medium =>
        concatBits(PCTagHashBitsForMediumHistory.map(tagPC(_)))

      case TAGEHistoryType.Long =>
        val xorBits = PCTagHashXorPairsForLongHistory.map { case (i, j) => tagPC(i) ^ tagPC(j) }
        concatBits(xorBits)

      case _ =>
        concatBits(PCTagHashBitsDefault.map(tagPC(_)))
    }

    hashPC
  }
}
