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
import org.chipsalliance.cde.config.Parameters
import scala.math.min
import utility.XSPerfAccumulate
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.FoldedHistoryInfo
import xiangshan.frontend.bpu.SaturateCounter
import xiangshan.frontend.bpu.history.phr.PhrAllFoldedHistories

class MicroTageTable(
    val numSets:       Int,
    val histLen:       Int,
    val tagLen:        Int,
    val histBitsInTag: Int,
    val tableId:       Int
)(implicit p: Parameters) extends MicroTageModule with Helpers {
  class MicroTageTableIO extends MicroTageBundle {
    class MicroTageReq extends Bundle {
      val startPc:        PrunedAddr            = new PrunedAddr(VAddrBits)
      val foldedPathHist: PhrAllFoldedHistories = Input(new PhrAllFoldedHistories(AllFoldedHistoryInfo))
    }
    class MicroTageResp extends Bundle {
      val taken:       Bool            = Bool()
      val cfiPosition: UInt            = UInt(CfiPositionWidth.W)
      val useful:      UInt            = UInt(UsefulWidth.W)
      val hitTakenCtr: SaturateCounter = new SaturateCounter(TakenCtrWidth)
      val hitUseful:   SaturateCounter = new SaturateCounter(UsefulWidth)
    }
    class MicroTageUpdate extends Bundle {
      val startPc:                PrunedAddr            = new PrunedAddr(VAddrBits)
      val allocValid:             Bool                  = Bool()
      val updateValid:            Bool                  = Bool()
      val usefulValid:            Bool                  = Bool()
      val allocTaken:             Bool                  = Bool()
      val allocCfiPosition:       UInt                  = UInt(CfiPositionWidth.W)
      val updateTaken:            Bool                  = Bool()
      val updateCfiPosition:      UInt                  = UInt(CfiPositionWidth.W)
      val usefulCorrect:          Bool                  = Bool()
      val foldedPathHistForTrain: PhrAllFoldedHistories = new PhrAllFoldedHistories(AllFoldedHistoryInfo)
    }
    val req:         MicroTageReq           = Input(new MicroTageReq)
    val resp:        Valid[MicroTageResp]   = Output(Valid(new MicroTageResp))
    val update:      Valid[MicroTageUpdate] = Input(Valid(new MicroTageUpdate))
    val usefulReset: Bool                   = Input(Bool())
    // Will be automatically optimized away when unused; no need to wrap in Option.
    val debug_predIdx: UInt           = UInt(log2Ceil(numSets).W)
    val debug_predTag: UInt           = UInt(tagLen.W)
    val trainDebug:    MicroTageDebug = Output(new MicroTageDebug)
  }
  class MicroTageEntry() extends MicroTageBundle {
    val valid:       Bool            = Bool()
    val tag:         UInt            = UInt(tagLen.W)
    val takenCtr:    SaturateCounter = new SaturateCounter(TakenCtrWidth)
    val cfiPosition: UInt            = UInt(CfiPositionWidth.W)
    // val useful:      SaturateCounter = new SaturateCounter(UsefulWidth)
  }
  val io                    = IO(new MicroTageTableIO)
  private val entries       = RegInit(VecInit(Seq.fill(numSets)(0.U.asTypeOf(new MicroTageEntry))))
  private val usefulEntries = RegInit(VecInit(Seq.fill(numSets)(0.U.asTypeOf(new SaturateCounter(UsefulWidth)))))

  val idxFhInfo    = new FoldedHistoryInfo(histLen, min(log2Ceil(numSets), histLen))
  val tagFhInfo    = new FoldedHistoryInfo(histLen, min(histLen, histBitsInTag))
  val altTagFhInfo = new FoldedHistoryInfo(histLen, min(histLen, histBitsInTag - 1))

  def computeHash(startPc: PrunedAddr, allFh: PhrAllFoldedHistories, tableId: Int): (UInt, UInt) = {
    val unhashedIdx = getUnhashedIdx(startPc)
    val unhashedTag = getUnhashedTag(startPc)
    val idxFh       = allFh.getHistWithInfo(idxFhInfo).foldedHist
    val tagFh       = allFh.getHistWithInfo(tagFhInfo).foldedHist
    val altTagFh    = allFh.getHistWithInfo(altTagFhInfo).foldedHist
    val idx = if (idxFhInfo.FoldedLength < log2Ceil(numSets)) {
      val foldShift = log2Ceil(numSets) - idxFhInfo.FoldedLength
      (unhashedIdx ^ Cat(0.U(foldShift.W), idxFh) ^ (idxFh << foldShift))(log2Ceil(numSets) - 1, 0)
    } else {
      (unhashedIdx ^ idxFh)(log2Ceil(numSets) - 1, 0)
    }
    val lowTag  = (unhashedTag ^ tagFh ^ (altTagFh << 1))(histBitsInTag - 1, 0)
    val highTag = connectPcTag(unhashedIdx, tableId)
    val tag     = Cat(highTag, lowTag)(tagLen - 1, 0)
    (idx, tag)
  }

  // predict
  private val (s0_idx, s0_tag) = computeHash(io.req.startPc, io.req.foldedPathHist, tableId)
  private val readEntry        = entries(s0_idx)
  private val readHit          = (readEntry.tag === s0_tag) && readEntry.valid
  private val usefulEntry      = usefulEntries(s0_idx)
  io.debug_predIdx := s0_idx
  io.debug_predTag := s0_tag

  io.resp.valid            := readHit
  io.resp.bits.taken       := readEntry.takenCtr.isPositive
  io.resp.bits.cfiPosition := readEntry.cfiPosition
  io.resp.bits.useful      := usefulEntry.value
  io.resp.bits.hitTakenCtr := readEntry.takenCtr
  io.resp.bits.hitUseful   := usefulEntry

  // train
  private val (trainIdx, trainTag) =
    computeHash(io.update.bits.startPc, io.update.bits.foldedPathHistForTrain, tableId)

  private val oldTakenCtr = entries(trainIdx).takenCtr
  private val oldUseful   = usefulEntries(trainIdx)
  private val updateEntry = Wire(new MicroTageEntry)
  updateEntry.valid := true.B
  updateEntry.tag   := trainTag
  updateEntry.takenCtr.value := Mux(
    io.update.bits.allocValid,
    // oldTakenCtr.getNeutral,
    Mux(io.update.bits.allocTaken, oldTakenCtr.getWeakPositive, oldTakenCtr.getWeakNegative),
    oldTakenCtr.getUpdate(io.update.bits.updateTaken)
  )

  updateEntry.cfiPosition := Mux(
    io.update.bits.allocValid,
    io.update.bits.allocCfiPosition,
    io.update.bits.updateCfiPosition
  )

  private val updateUseful = Mux(
    io.update.bits.allocValid,
    oldUseful.getWeakNegative,
    oldUseful.getUpdate(io.update.bits.usefulCorrect)
  )

  // Write back updated entry on valid update
  when(io.update.valid && (io.update.bits.allocValid || io.update.bits.updateValid)) {
    entries(trainIdx) := updateEntry
  }

  when(io.update.valid && (io.update.bits.usefulValid || io.update.bits.allocValid)) {
    usefulEntries(trainIdx).value := updateUseful // updateEntry.useful
  }

  when(io.usefulReset) {
    usefulEntries.zipWithIndex.foreach { case (entry, i) =>
      usefulEntries(i).value := entry.value >> 1.U
    }
  }

  // ------------ for debug signal ----------- //
  io.trainDebug                := 0.U.asTypeOf(new MicroTageDebug)
  io.trainDebug.debug_idx      := trainIdx
  io.trainDebug.debug_tag      := trainTag
  io.trainDebug.debug_tableId  := tableId.U
  io.trainDebug.debug_useful   := oldUseful.value
  io.trainDebug.debug_takenCtr := oldTakenCtr.value
  // Per-index access distribution
  // for (i <- 0 until numSets) {
  //   XSPerfAccumulate(f"update_idx_access_$i", (trainIdx === i.U) && io.update.valid)
  //   XSPerfAccumulate(f"alloc_idx_access_$i", (trainIdx === i.U) && io.update.valid && io.update.bits.allocValid)
  // }
}
