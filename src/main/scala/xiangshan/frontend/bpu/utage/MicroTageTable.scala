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
import yunsuan.vector.alu.VIntFixpTable.table

class MicroTageTable(
    val numSets:       Int,
    val histLen:       Int,
    val tagLen:        Int,
    val histBitsInTag: Int,
    val tableId:       Int,
    val numWay:        Int
)(implicit p: Parameters) extends MicroTageModule with Helpers {
  class MicroTageTableIO extends MicroTageBundle {
    class MicroTageReq extends Bundle {
      val startPc:        PrunedAddr            = new PrunedAddr(VAddrBits)
      val foldedPathHist: PhrAllFoldedHistories = new PhrAllFoldedHistories(AllFoldedHistoryInfo)
    }
    class MicroTageRead extends Bundle {
      val taken:       Bool = Bool()
      val cfiPosition: UInt = UInt(CfiPositionWidth.W)
      val useful:      UInt = UInt(UsefulWidth.W)
    }
    class MicroTageUpdateInfo extends Bundle {
      val updateValid: Bool = Bool()
      val updateTaken: Bool = Bool()
      val usefulValid: Bool = Bool()
      val needUseful:  Bool = Bool()
    }
    class MicroTageAllocInfo extends Bundle {
      val taken:       Bool = Bool()
      val wayMask:     UInt = UInt(numWay.W)
      val cfiPosition: UInt = UInt(CfiPositionWidth.W)
    }
    class MicroTageTrain extends Bundle {
      val t0_startPc:                PrunedAddr                = Input(new PrunedAddr(VAddrBits))
      val t0_foldedPathHistForTrain: PhrAllFoldedHistories     = Input(new PhrAllFoldedHistories(AllFoldedHistoryInfo))
      val t0_read:                   Vec[Valid[MicroTageRead]] = Output(Vec(numWay, Valid(new MicroTageRead)))
      val t1_update: Vec[Valid[MicroTageUpdateInfo]] = Input(Vec(numWay, Valid(new MicroTageUpdateInfo)))
      val t1_alloc:  Valid[MicroTageAllocInfo]       = Input(Valid(new MicroTageAllocInfo))
    }
    val req:         MicroTageReq                   = Input(new MicroTageReq)
    val resps:       Vec[Valid[MicroTageTablePred]] = Output(Vec(numWay, Valid(new MicroTageTablePred)))
    val train:       MicroTageTrain                 = new MicroTageTrain
    val usefulReset: Bool                           = Input(Bool())
    val debugIdx:    UInt                           = Output(UInt(log2Ceil(MaxNumSets).W))
  }
  class MicroTageEntry() extends MicroTageBundle {
    val valid:      Bool            = Bool()
    val startPcTag: UInt            = UInt(tagLen.W)
    val posTag:     UInt            = UInt(CfiPositionWidth.W)
    val takenCtr:   SaturateCounter = TakenCounter()
  }
  val io              = IO(new MicroTageTableIO)
  private val entries = RegInit(VecInit(Seq.fill(numSets)(VecInit(Seq.fill(numWay)(0.U.asTypeOf(new MicroTageEntry))))))
  private val usefulEntries = RegInit(VecInit(Seq.fill(numSets)(
    VecInit(Seq.fill(numWay)(0.U.asTypeOf(new SaturateCounter(UsefulWidth))))
  )))
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
  private val predReadEntries  = entries(s0_idx)
  private val predReadUseful   = usefulEntries(s0_idx)

  // Output prediction results for all ways
  for (way <- 0 until numWay) {
    val entry  = predReadEntries(way)
    val useful = predReadUseful(way)
    val hit    = entry.valid && (entry.startPcTag === s0_tag)
    io.resps(way).valid            := hit
    io.resps(way).bits.taken       := entry.takenCtr.isPositive
    io.resps(way).bits.cfiPosition := entry.posTag
  }

  // train / write logic
  private val (t0_trainIdx, t0_trainTag) = computeHash(io.train.t0_startPc, io.train.t0_foldedPathHistForTrain, tableId)
  private val t0_trainReadEntries        = entries(t0_trainIdx)
  private val t0_trainReadUseful         = usefulEntries(t0_trainIdx)
  for (way <- 0 until numWay) {
    val entry  = t0_trainReadEntries(way)
    val useful = t0_trainReadUseful(way)
    val hit    = entry.valid && (entry.startPcTag === t0_trainTag)
    io.train.t0_read(way).valid            := entry.valid
    io.train.t0_read(way).bits.taken       := entry.takenCtr.isPositive
    io.train.t0_read(way).bits.cfiPosition := entry.posTag
    io.train.t0_read(way).bits.useful      := useful.value
  }

  private val t1_trainIdx         = RegNext(t0_trainIdx)
  private val t1_trainTag         = RegNext(t0_trainTag)
  private val t1_trainReadEntries = entries(t1_trainIdx)
  private val t1_trainReadUseful  = usefulEntries(t1_trainIdx)
  // For each way, prepare updated entry and useful counter
  for (way <- 0 until numWay) {
    val oldEntry    = t1_trainReadEntries(way)
    val oldTakenCtr = oldEntry.takenCtr
    val oldUseful   = t1_trainReadUseful(way)

    // Update logic: either alloc or update
    val doAlloc  = io.train.t1_alloc.valid && io.train.t1_alloc.bits.wayMask(way)
    val doUpdate = io.train.t1_update(way).valid && io.train.t1_update(way).bits.updateValid

    // New entry values
    val newEntry = Wire(new MicroTageEntry)
    newEntry.valid      := true.B
    newEntry.startPcTag := t1_trainTag
    newEntry.posTag     := Mux(doAlloc, io.train.t1_alloc.bits.cfiPosition, oldEntry.posTag)
    newEntry.takenCtr := Mux(
      doAlloc,
      Mux(io.train.t1_alloc.bits.taken, TakenCounter.WeakPositive, TakenCounter.WeakNegative),
      oldTakenCtr.getUpdate(io.train.t1_update(way).bits.updateTaken)
    )

    // Useful counter update
    val newUseful = Mux(
      doAlloc,
      if (tableId == 0) UsefulCounter.WeakNegative else UsefulCounter.WeakPositive,
      Mux(
        io.train.t1_update(way).bits.usefulValid,
        oldUseful.getUpdate(io.train.t1_update(way).bits.needUseful),
        oldUseful
      )
    )

    when(doAlloc || doUpdate) {
      t1_trainReadEntries(way) := newEntry
    }
    when(doAlloc || (io.train.t1_update(way).valid && io.train.t1_update(way).bits.usefulValid)) {
      t1_trainReadUseful(way) := newUseful
    }
  }

  // Write back updated entry on valid update
  when(io.usefulReset) {
    for (i <- 0 until numSets) {
      for (w <- 0 until numWay) {
        val entry = usefulEntries(i)(w)
        if (tableId == 0) {
          usefulEntries(i)(w).value := Mux(entry.value === 0.U, 0.U, entry.value - 1.U)
        } else {
          usefulEntries(i)(w).value := entry.value >> 1.U
        }
      }
    }
  }

  io.debugIdx := t1_trainIdx.pad(log2Ceil(MaxNumSets))
}
