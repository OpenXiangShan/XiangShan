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
import utils.AddrField
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.TargetFixHelper

trait Helpers extends HasAheadBtbParameters with TargetFixHelper {
  val addrFields = AddrField(
    Seq(
      ("instOffset", instOffsetBits),
      ("bankIdx", BankIdxWidth),
      ("setIdx", SetIdxWidth)
    ),
    maxWidth = Option(VAddrBits),
    extraFields = Seq(
      ("tag", instOffsetBits, TagWidth),
      ("targetLower", instOffsetBits, TargetLowerBitsWidth)
    )
  )

  def getSetIndex(pc: PrunedAddr): UInt =
    addrFields.extract("setIdx", pc)

  def getBankIndex(pc: PrunedAddr): UInt =
    addrFields.extract("bankIdx", pc)

  def getTag(pc: PrunedAddr): UInt =
    addrFields.extract("tag", pc)

  def getTargetUpper(pc: PrunedAddr): UInt =
    pc(pc.length - 1, addrFields.getEnd("targetLower") + 1)

  def getTargetLowerBits(target: PrunedAddr): UInt =
    addrFields.extract("targetLower", target)

  def detectMultiHit(hitMask: IndexedSeq[Bool], position: IndexedSeq[UInt]): (Bool, UInt) = {
    require(hitMask.length == position.length)
    require(hitMask.length >= 2)
    val isMultiHit     = WireDefault(false.B)
    val multiHitWayIdx = WireDefault(0.U(WayIdxWidth.W))
    for {
      i <- 0 until NumWays
      j <- i + 1 until NumWays
    } {
      val bothHit      = hitMask(i) && hitMask(j)
      val samePosition = position(i) === position(j)
      when(bothHit && samePosition) {
        isMultiHit     := true.B
        multiHitWayIdx := i.U
      }
    }
    (isMultiHit, multiHitWayIdx)
  }
}
