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

package xiangshan.frontend.bpu.abtb

import chisel3._
import chisel3.util._
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.TargetFixHelper

trait Helpers extends HasAheadBtbParameters with TargetFixHelper {
  def getSetIndex(pc: PrunedAddr): UInt =
    pc(SetIdxWidth + BankIdxWidth + instOffsetBits - 1, BankIdxWidth + instOffsetBits)

  def getBankIndex(pc: PrunedAddr): UInt =
    pc(BankIdxWidth + instOffsetBits - 1, instOffsetBits)

  def getTag(pc: PrunedAddr): UInt =
    pc(TagWidth + instOffsetBits - 1, instOffsetBits)

  def getTargetUpper(pc: PrunedAddr): UInt =
    pc(VAddrBits - 1, TargetLowerBitsWidth + instOffsetBits)

  def getTargetLowerBits(target: PrunedAddr): UInt =
    target(TargetLowerBitsWidth + instOffsetBits - 1, instOffsetBits)

  def detectMultiHit(hitMask: IndexedSeq[Bool], position: IndexedSeq[UInt]): Bool = {
    require(hitMask.length == position.length)
    require(hitMask.length >= 2)
    val n = hitMask.length
    val multiHitConditions = for {
      i <- 0 until n
      j <- i + 1 until n
    } yield {
      val bothHit      = hitMask(i) && hitMask(j)
      val samePosition = position(i) === position(j)
      bothHit && samePosition
    }
    multiHitConditions.reduce(_ || _)
  }
}
