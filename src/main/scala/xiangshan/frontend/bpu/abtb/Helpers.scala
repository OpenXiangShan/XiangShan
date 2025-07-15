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
import xiangshan.frontend.PrunedAddrInit
import xiangshan.frontend.bpu.TargetState

trait Helpers extends HasAheadBtbParameters {
  def getSetIndex(pc: PrunedAddr): UInt =
    pc(SetIdxLen + BankIdxLen + instOffsetBits - 1, BankIdxLen + instOffsetBits)

  def getBankIndex(pc: PrunedAddr): UInt =
    pc(BankIdxLen + instOffsetBits - 1, instOffsetBits)

  def getTag(pc: PrunedAddr): UInt =
    pc(TagLen + instOffsetBits - 1, instOffsetBits)

  def getPcUpperBits(pc: PrunedAddr): UInt =
    pc(VAddrBits - 1, TargetLowerBitsLen + instOffsetBits)

  def getTargetLowerBits(target: PrunedAddr): UInt =
    target(TargetLowerBitsLen + instOffsetBits - 1, instOffsetBits)

  def getAlignedPc(pc: PrunedAddr): PrunedAddr = {
    val shiftAmount = log2Ceil(FetchBlockAlignSize)
    val alignedPc   = (pc.toUInt >> shiftAmount) << shiftAmount
    PrunedAddrInit(alignedPc.asUInt)
  }

  def getFirstTakenEntryWayIdxOH(positions: IndexedSeq[UInt], takenMask: IndexedSeq[Bool]): IndexedSeq[Bool] = {
    val n = positions.length
    val compareMatrix = (0 until n).map(i =>
      (0 until i).map(j =>
        positions(i) < positions(j)
      )
    )
    (0 until n).map { i =>
      (0 until n).map { j =>
        if (j < i) !takenMask(j) || compareMatrix(i)(j) // positions(i) < positions(j)
        else if (j == i) takenMask(i)
        else !takenMask(j) || !compareMatrix(j)(i) // positions(i) <= positions(j)
      }.reduce(_ && _)
    }
  }

  def getFirstTakenEntry(entries: Vec[AheadBtbEntry], takenMask: IndexedSeq[Bool]): (AheadBtbEntry, UInt) = {
    val indexedEntries = VecInit(entries.zipWithIndex.zip(takenMask).map {
      case ((e, i), t) =>
        val bundle = new Bundle {
          val key   = UInt((1 + e.position.getWidth).W)
          val idx   = UInt(log2Ceil(entries.size).W)
          val entry = e.cloneType
        }
        val wire = Wire(bundle)
        wire.key   := Cat(!t, e.position)
        wire.idx   := i.U
        wire.entry := e
        wire
    })
    val firstTakenEntry = indexedEntries.reduceTree((a, b) => Mux(a.key < b.key, a, b))
    (firstTakenEntry.entry, firstTakenEntry.idx)
  }

  def getTarget(entry: AheadBtbEntry, startPc: PrunedAddr): PrunedAddr = {
    val startPcUpperBits = getPcUpperBits(startPc)
    val targetUpperBits = MuxCase(
      startPcUpperBits,
      Seq(
        entry.targetState.isCarry  -> (startPcUpperBits + 1.U),
        entry.targetState.isBorrow -> (startPcUpperBits - 1.U)
      )
    )
    val targetLowerBits = entry.targetLowerBits
    val target          = PrunedAddrInit(Cat(targetUpperBits, targetLowerBits, 0.U(instOffsetBits.W)))
    target
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

  def detectMultiHit(entries: Vec[AheadBtbEntry], tag: UInt): Bool = {
    val multiHit = WireDefault(false.B)
    for (i <- 0 until entries.length) {
      for (j <- i + 1 until entries.length) {
        val sameTag      = entries(i).tag === entries(j).tag === tag
        val samePosition = entries(i).position === entries(j).position
        multiHit := sameTag && samePosition && entries(i).valid && entries(j).valid
      }
    }
    multiHit
  }

}
