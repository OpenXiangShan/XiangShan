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
import xiangshan.frontend.bpu.CrossPageHelper
import xiangshan.frontend.bpu.HalfAlignHelper
import xiangshan.frontend.bpu.RotateHelper
import xiangshan.frontend.bpu.TargetFixHelper

trait Helpers extends HasMainBtbParameters
    with HasXSParameter with TargetFixHelper with RotateHelper with HalfAlignHelper with CrossPageHelper {
  def getSetIndex(pc: PrunedAddr): UInt =
    pc(SetIdxLen + InternalBankIdxLen + FetchBlockSizeWidth - 1, InternalBankIdxLen + FetchBlockSizeWidth)

  def getReplacerSetIndex(pc: PrunedAddr): UInt =
    pc(SetIdxLen + FetchBlockAlignWidth - 1, FetchBlockAlignWidth)

  def getAlignBankIndex(pc: PrunedAddr): UInt =
    pc(FetchBlockSizeWidth - 1, FetchBlockAlignWidth)

  def getAlignBankIndexFromPosition(cfiPosition: UInt): UInt =
    cfiPosition(CfiPositionWidth - 1, CfiPositionWidth - AlignBankIdxLen)

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

  def detectMultiHit(hitMask: IndexedSeq[Bool], position: IndexedSeq[UInt]): UInt = {
    require(hitMask.length == position.length)
    require(hitMask.length >= 2)
    val multiHitMask = VecInit(Seq.fill(NumWay)(false.B))
    for {
      i <- 0 until NumWay
      j <- i + 1 until NumWay
    } {
      val bothHit      = hitMask(i) && hitMask(j)
      val samePosition = position(i) === position(j)
      when(bothHit && samePosition) {
        multiHitMask(i) := true.B
      }
    }
    multiHitMask.asUInt
  }
}
