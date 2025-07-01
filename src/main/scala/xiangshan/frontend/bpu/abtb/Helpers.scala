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
    pc(TagLen, instOffsetBits)

  def getPcUpperBits(pc: PrunedAddr): UInt =
    pc(VAddrBits - 1, TargetLowerBitsLen + instOffsetBits)

  def getTargetLowerBits(target: PrunedAddr): UInt =
    target(TargetLowerBitsLen + instOffsetBits - 1, instOffsetBits)

  def getAlignedPc(pc: PrunedAddr): PrunedAddr = {
    val shiftAmount = log2Ceil(FetchBlockAlignSize)
    val alignedPc   = (pc.toUInt >> shiftAmount) << shiftAmount
    PrunedAddrInit(alignedPc.asUInt)
  }

  def getHitMask(entries: Vec[AheadBtbEntry], tag: UInt): Vec[Bool] =
    VecInit(entries.map(entry => entry.valid && entry.tag === tag))

  def getTakenMask(entries: Vec[AheadBtbEntry], hitMask: Vec[Bool], counterResult: Vec[Bool]): Vec[Bool] =
    VecInit(entries.zip(hitMask).zip(counterResult).map { case ((entry, hit), taken) =>
      hit && (taken || entry.isStaticTarget)
    })

  def getFirstTakenEntry(entries: Vec[AheadBtbEntry], takenMask: Vec[Bool]): (AheadBtbEntry, UInt) = {
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
}
