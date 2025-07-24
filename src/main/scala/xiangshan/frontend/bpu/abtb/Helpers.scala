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
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.PrunedAddrInit
import xiangshan.frontend.bpu.TargetState

trait Helpers extends HasAheadBtbParameters {
  def getSetIndex(pc: PrunedAddr): UInt =
    pc(SetIdxWidth + BankIdxWidth + instOffsetBits - 1, BankIdxWidth + instOffsetBits)

  def getBankIndex(pc: PrunedAddr): UInt =
    pc(BankIdxWidth + instOffsetBits - 1, instOffsetBits)

  def getTag(pc: PrunedAddr): UInt =
    pc(TagWidth + instOffsetBits - 1, instOffsetBits)

  def getPcUpperBits(pc: PrunedAddr): UInt =
    pc(VAddrBits - 1, TargetLowerBitsWidth + instOffsetBits)

  def getTargetLowerBits(target: PrunedAddr): UInt =
    target(TargetLowerBitsWidth + instOffsetBits - 1, instOffsetBits)

  def getTarget(entry: AheadBtbEntry, startPc: PrunedAddr): PrunedAddr = {
    val startPcUpperBits = getPcUpperBits(startPc)
    val targetUpperBits = if (EnableTargetFix)
      MuxCase(
        startPcUpperBits,
        Seq(
          entry.targetState.get.isCarry  -> (startPcUpperBits + 1.U),
          entry.targetState.get.isBorrow -> (startPcUpperBits - 1.U)
        )
      )
    else
      startPcUpperBits
    val targetLowerBits = entry.targetLowerBits
    PrunedAddrInit(Cat(targetUpperBits, targetLowerBits, 0.U(instOffsetBits.W)))
  }

  def getTargetState(startPc: PrunedAddr, target: PrunedAddr): TargetState = {
    val startPcUpperBits = getPcUpperBits(startPc)
    val targetUpperBits  = getPcUpperBits(target)
    MuxCase(
      TargetState.NoCarryAndBorrow,
      Seq(
        (targetUpperBits > startPcUpperBits) -> TargetState.Carry,
        (targetUpperBits < startPcUpperBits) -> TargetState.Borrow
      )
    )
  }

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
