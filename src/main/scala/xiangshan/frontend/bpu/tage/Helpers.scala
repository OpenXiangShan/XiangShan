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

package xiangshan.frontend.bpu.tage

import chisel3._
import chisel3.util._
import xiangshan.HasXSParameter
import xiangshan.frontend.PrunedAddr

trait Helpers extends HasTageParameters with HasXSParameter {
  def getBaseTableSetIndex(pc: PrunedAddr): UInt =
    pc(
      BaseTableSetIdxWidth - 1 + BaseTableBankIdxWidth + FetchBlockSizeWidth,
      BaseTableBankIdxWidth + FetchBlockSizeWidth
    )

  def getBaseTableBankIndex(pc: PrunedAddr): UInt =
    pc(BaseTableBankIdxWidth - 1 + FetchBlockSizeWidth, FetchBlockSizeWidth)

  def getBaseTableAlignBankIndex(pc: PrunedAddr): UInt =
    pc(FetchBlockSizeWidth - 1, FetchBlockAlignWidth)

  def getSetIndex(pc: PrunedAddr, hist: UInt, numSets: Int): UInt = {
    val pcBits =
      pc(log2Ceil(numSets / NumBanks) - 1 + BankIdxWidth + FetchBlockSizeWidth, BankIdxWidth + FetchBlockSizeWidth)
    pcBits ^ hist
  }

  def getTag(pc: PrunedAddr, hist1: UInt, hist2: UInt): UInt = {
    val pcBits = pc(TagWidth - 1 + FetchBlockSizeWidth, FetchBlockSizeWidth)
    pcBits ^ hist1 ^ hist2
  }

  def getBankMask(pc: PrunedAddr): Seq[Bool] = {
    val bankIdx = pc(BankIdxWidth - 1 + FetchBlockSizeWidth, FetchBlockSizeWidth)
    UIntToOH(bankIdx, NumBanks).asBools
  }

  def vecRotateRight[T <: Data](vec: Vec[T], idx: UInt): Vec[T] = {
    require(isPow2(vec.length))
    require(idx.getWidth == log2Ceil(vec.length))
    val len = vec.length
    // generate all possible results of rotation
    val rotations = (0 until len).map { i =>
      val rotatedIndices = (0 until len).map(j => (j + i) % len)
      i.U -> VecInit(rotatedIndices.map(idx => vec(idx)))
    }
    MuxLookup(idx, vec)(rotations)
  }
}
