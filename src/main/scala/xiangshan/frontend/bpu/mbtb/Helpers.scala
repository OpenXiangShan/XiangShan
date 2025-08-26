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

package xiangshan.frontend.bpu.mbtb

import chisel3._
import xiangshan.HasXSParameter
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.TargetFixHelper

trait Helpers extends HasMainBtbParameters with HasXSParameter with TargetFixHelper {
  def getSetIndex(pc: PrunedAddr): UInt =
    pc(SetIdxLen + InternalBankIdxLen + FetchBlockSizeWidth - 1, InternalBankIdxLen + FetchBlockSizeWidth)

  def getReplacerSetIndex(pc: PrunedAddr): UInt =
    pc(SetIdxLen + FetchBlockSizeWidth - 1, FetchBlockSizeWidth)

  def getAlignBankIndex(pc: PrunedAddr): UInt =
    pc(FetchBlockSizeWidth - 1, FetchBlockAlignWidth)

  def getTargetUpper(pc: PrunedAddr): UInt =
    pc(VAddrBits - 1, TargetWidth + instOffsetBits)

  def getTargetLowerBits(target: PrunedAddr): UInt =
    target(TargetWidth + instOffsetBits - 1, instOffsetBits)

  def getInternalBankIndex(pc: PrunedAddr): UInt =
    pc(InternalBankIdxLen + FetchBlockSizeWidth - 1, FetchBlockSizeWidth)

  def getTag(pc: PrunedAddr): UInt =
    pc(
      TagWidth + InternalBankIdxLen + SetIdxLen + FetchBlockSizeWidth - 1,
      InternalBankIdxLen + SetIdxLen + FetchBlockSizeWidth
    )
}
