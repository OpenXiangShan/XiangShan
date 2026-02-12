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
import utility.CircularQueuePtr
import utility.HasCircularQueuePtrHelper
import utility.ParallelPriorityMux
import utility.XSPerfAccumulate
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.CompareMatrix
import xiangshan.frontend.bpu.FoldedHistoryInfo
import xiangshan.frontend.bpu.SaturateCounter
import xiangshan.frontend.bpu.history.phr.PhrAllFoldedHistories
import yunsuan.vector.alu.VIntFixpTable.table

/**
 * Bypass Shadow Buffer - Write buffer and bypass cache
 *
 * Design goals:
 * 1. Cache recent TAGE table updates to avoid frequent SRAM write-backs (write coalescing)
 * 2. Provide RAW bypass (Read-After-Write) to ensure predictions see the latest updates
 * 3. Reduce write port pressure on TAGE tables, saving area and power
 *
 * Operation principles:
 * - Circular queue structure: new writes are enqueued, forced write-back to SRAM when nearly full
 * - When buffer is not full, attempt to write-back the oldest entry every cycle
 * - Entries already written back are not cleared, but marked as allocatable
 * - Priority mask ensures the newest copy is read when multiple copies of the same address exist
 * - Banked useful registers reduce fanout during reset
 */
class BypassShadowBuffer(
    val numSets:  Int,
    val numWay:   Int,
    val numEntry: Int = 16,
    val tableId:  Int,
    val NumBanks: Int = 4
)(implicit p: Parameters) extends MicroTageModule with HasCircularQueuePtrHelper with Helpers {
  class BypassBufferIO extends MicroTageBundle {
    class Req extends MicroTageBundle {
      val readIndex: UInt = UInt(log2Ceil(MaxNumSets).W)
    }
    class Resp extends MicroTageBundle {
      val hit:         Vec[Bool]           = Vec(numWay, Bool())
      val readEntries: Vec[MicroTageEntry] = Vec(numWay, new MicroTageEntry)
    }
    class WriteReq extends MicroTageBundle {
      val writeIndex: UInt                = UInt(log2Ceil(MaxNumSets).W)
      val writeData:  Vec[MicroTageEntry] = Vec(numWay, new MicroTageEntry)
      val forceWrite: Bool                = Bool()
      val wMask:      UInt                = UInt(numWay.W)
    }
    val req:          Req             = Input(new Req)
    val resp:         Resp            = Output(new Resp)
    val train:        MicroTageTrain  = new MicroTageTrain(numWay, numSets)
    val tryWrite:     Valid[WriteReq] = Output(Valid(new WriteReq))
    val writeSuccess: Bool            = Input(Bool())
    val usefulReset:  Bool            = Input(Bool())
  }
  val io = IO(new BypassBufferIO)
  class BufferEntry extends Bundle {
    val valid:     Bool                       = Bool()
    val entryData: Vec[Valid[MicroTageEntry]] = Vec(numWay, Valid(new MicroTageEntry))
    val index:     UInt                       = UInt(log2Ceil(MaxNumSets).W)
  }

  class ReplaceItem extends Bundle {
    val valid: Bool = Bool()
    val dirty: Bool = Bool()
  }
  class BufferPtr(implicit p: Parameters) extends CircularQueuePtr[BufferPtr](numEntry) {}

  private val entries       = RegInit(VecInit(Seq.fill(numEntry)(0.U.asTypeOf(new BufferEntry))))
  private val statusEntries = RegInit(VecInit(Seq.fill(numEntry)(0.U.asTypeOf(new ReplaceItem))))
  private val enqPtr        = RegInit(0.U.asTypeOf(new BufferPtr)) // Next available position
  private val deqPtr        = RegInit(0.U.asTypeOf(new BufferPtr)) // Position ready for write-back
  private val priorityMask  = RegInit(0.U(numEntry.W))

  // Banked useful registers
  private val usefulEntries = RegInit(VecInit.tabulate(NumBanks) { bankIdx =>
    VecInit(Seq.fill(numSets / NumBanks)(
      VecInit(Seq.fill(numWay)(0.U.asTypeOf(new SaturateCounter(UsefulWidth))))
    ))
  })

  // Prediction logic
  private val entryDataVec = VecInit(entries.map(e => e.entryData))
  private val a0_entryHit  = Wire(Vec(numEntry, Bool()))
  a0_entryHit := entries.map(e => (e.index === io.req.readIndex) && e.valid)

  private val a0_chosenFirst = (a0_entryHit.asUInt & priorityMask).orR
  private val a1_chosenFirst = RegNext(a0_chosenFirst, false.B)
  private val a1_firstHit    = RegInit(VecInit(Seq.fill(numEntry)(false.B)))
  private val a1_entryHit    = RegInit(VecInit(Seq.fill(numEntry)(false.B)))
  a1_firstHit := (a0_entryHit.asUInt & priorityMask).asBools
  a1_entryHit := a0_entryHit
  private val a1_firstEntry        = ParallelPriorityMux(a1_firstHit.reverse, entryDataVec.reverse)
  private val a1_secondEntry       = ParallelPriorityMux(a1_entryHit.reverse, entryDataVec.reverse)
  private val a1_bufferEntry       = Mux(a1_chosenFirst, a1_firstEntry, a1_secondEntry)
  private val a1_hasHit            = a1_entryHit.reduce(_ || _)
  private val a1_microTageHitVec   = a1_bufferEntry.map(e => e.valid && a1_hasHit)
  private val a1_microTageEntryVec = a1_bufferEntry.map(e => e.bits)
  io.resp.hit         := a1_microTageHitVec
  io.resp.readEntries := a1_microTageEntryVec

  // Training logic - stage 0
  private val t0_fire       = io.train.t0_trainIndex.valid
  private val t0_trainIndex = io.train.t0_trainIndex.bits
  private val t0_entryHit   = Wire(Vec(numEntry, Bool()))
  t0_entryHit := entries.map(e => (e.index === t0_trainIndex) && e.valid)

  private val t0_firstHit    = Wire(Vec(numEntry, Bool()))
  private val t0_chosenFirst = (t0_entryHit.asUInt & priorityMask).orR
  t0_firstHit := (t0_entryHit.asUInt & priorityMask).asBools
  private val t0_firstEntry  = ParallelPriorityMux(t0_firstHit.reverse, entryDataVec.reverse)
  private val t0_secondEntry = ParallelPriorityMux(t0_entryHit.reverse, entryDataVec.reverse)

  private val t0_hasHit            = t0_entryHit.reduce(_ || _)
  private val t0_bufferEntry       = Mux(t0_chosenFirst, t0_firstEntry, t0_secondEntry)
  private val t0_microTageHitVec   = VecInit(t0_bufferEntry.map(e => e.valid && t0_hasHit))
  private val t0_microTageEntryVec = VecInit(t0_bufferEntry.map(e => e.bits))
  // Access useful registers for t0 stage
  private val t0_bankIdx         = getBankId(t0_trainIndex, NumBanks)
  private val t0_bankOffset      = getBankInnerIndex(t0_trainIndex, NumBanks, numSets)
  private val t0_trainReadUseful = usefulEntries(t0_bankIdx)(t0_bankOffset)
  private val t0_cleanId =
    Mux(t0_chosenFirst, ~PriorityEncoder(t0_firstHit.reverse), ~PriorityEncoder(t0_entryHit.reverse))
  private val t0_trainReadEntries = t0_microTageEntryVec

  for (way <- 0 until numWay) {
    val entry  = t0_trainReadEntries(way)
    val useful = t0_trainReadUseful(way)
    io.train.t0_read(way).canGetPosition := t0_microTageHitVec(way)
    io.train.t0_read(way).cfiPosition    := entry.cfiPosition
    io.train.t0_read(way).useful         := useful.value
  }

  // ==========================================================================
  // Bypass Logic: entries with the same readIndex are not allowed
  // ==========================================================================
  // Buffer write logic is pipelined across two cycles,
  // creating potential for writes to the same index in consecutive cycles.
  private val needBypass        = WireDefault(false.B)
  private val bypasscleanId     = WireDefault(0.U(log2Ceil(numEntry).W))
  private val bypassHasHit      = Wire(Bool())
  private val bypassHitVec      = Wire(Vec(numWay, Bool()))
  private val bypassReadEntries = Wire(Vec(numWay, new MicroTageEntry))

  private val t1_fire             = RegNext(t0_fire, false.B)
  private val t1_trainIndex       = RegNext(t0_trainIndex, 0.U(log2Ceil(MaxNumSets).W))
  private val t1_hasHit           = RegNext(Mux(needBypass, bypassHasHit, t0_hasHit), false.B)
  private val t1_microTageHitVec  = RegNext(Mux(needBypass, bypassHitVec, t0_microTageHitVec))
  private val t1_trainReadEntries = RegNext(Mux(needBypass, bypassReadEntries, t0_trainReadEntries))
  private val t1_cleanId          = RegNext(Mux(needBypass, bypasscleanId, t0_cleanId))
  // Access useful registers for t1 stage
  private val t1_bankIdx         = getBankId(t1_trainIndex, NumBanks)
  private val t1_bankOffset      = getBankInnerIndex(t1_trainIndex, NumBanks, numSets)
  private val t1_trainReadUseful = usefulEntries(t1_bankIdx)(t1_bankOffset)

  private val writeBufferValid     = WireDefault(VecInit(Seq.fill(numWay)(false.B)))
  private val newMicroTageEntryVec = WireDefault(VecInit(Seq.fill(numWay)(0.U.asTypeOf(new MicroTageEntry))))
  for (way <- 0 until numWay) {
    val oldEntry       = t1_trainReadEntries(way)
    val oldTakenCtr    = oldEntry.takenCtr
    val updateTakenCtr = io.train.t1_update(way).bits.updateTakenCtr

    // Update logic: either allocation or update
    val doAlloc  = io.train.t1_alloc.valid && io.train.t1_alloc.bits.wayMask(way)
    val doUpdate = io.train.t1_update(way).valid && io.train.t1_update(way).bits.updateValid
    writeBufferValid(way) := doAlloc || doUpdate

    // New entry values
    newMicroTageEntryVec(way).valid := true.B
    newMicroTageEntryVec(way).tag   := io.train.t1_tag
    newMicroTageEntryVec(way).cfiPosition :=
      Mux(doAlloc, io.train.t1_alloc.bits.cfiPosition, io.train.t1_update(way).bits.updateCfiPosition)
    newMicroTageEntryVec(way).takenCtr := Mux(
      doAlloc,
      Mux(io.train.t1_alloc.bits.taken, TakenCounter.WeakPositive, TakenCounter.WeakNegative),
      // updateTakenCtr.getUpdate(io.train.t1_update(way).bits.updateTaken)
      Mux(
        t1_microTageHitVec(way) && (oldEntry.tag === io.train.t1_tag),
        oldTakenCtr.getUpdate(io.train.t1_update(way).bits.updateTaken),
        updateTakenCtr.getUpdate(io.train.t1_update(way).bits.updateTaken)
      )
    )
  }

  for (way <- 0 until numWay) {
    val doAlloc   = io.train.t1_alloc.valid && io.train.t1_alloc.bits.wayMask(way)
    val oldUseful = t1_trainReadUseful(way)
    val newUseful = Mux(
      doAlloc,
      // UsefulCounter.WeakPositive,
      if (tableId < NumTables / 2) UsefulCounter.WeakNegative else UsefulCounter.WeakPositive,
      oldUseful.getUpdate(io.train.t1_update(way).bits.needUseful)
    )
    when(doAlloc || (io.train.t1_update(way).valid && io.train.t1_update(way).bits.usefulValid)) {
      t1_trainReadUseful(way) := newUseful
    }
  }

  // Useful counter reset logic
  when(io.usefulReset) {
    for (bankIdx <- 0 until NumBanks) {
      for (setIdx <- 0 until numSets / NumBanks) {
        for (wayIdx <- 0 until numWay) {
          val entry = usefulEntries(bankIdx)(setIdx)(wayIdx)
          if (tableId < NumTables / 2) {
            usefulEntries(bankIdx)(setIdx)(wayIdx).value :=
              Mux(entry.value === 0.U, 0.U, entry.value - 1.U)
          } else {
            usefulEntries(bankIdx)(setIdx)(wayIdx).value := entry.value >> 1.U
          }
          // usefulEntries(bankIdx)(setIdx)(wayIdx).value := entry.value >> 1.U
        }
      }
    }
  }

  private val newBufferEntry = Wire(new BufferEntry)
  newBufferEntry.valid := true.B
  newBufferEntry.index := t1_trainIndex
  for (i <- 0 until numWay) {
    newBufferEntry.entryData(i).valid := writeBufferValid(i)
    newBufferEntry.entryData(i).bits  := newMicroTageEntryVec(i)
  }

  private val t1_hasWrite = writeBufferValid.reduce(_ || _)
  when(t1_hasWrite) {
    entries(enqPtr.value) := newBufferEntry
    enqPtr                := enqPtr + 1.U
    priorityMask          := Fill(numEntry, 1.U(1.W)) >> ~enqPtr.value
  }

  needBypass        := (t1_trainIndex === t0_trainIndex) && t1_hasWrite
  bypassHasHit      := true.B
  bypasscleanId     := enqPtr.value
  bypassHitVec      := newBufferEntry.entryData.map(e => e.valid)
  bypassReadEntries := newBufferEntry.entryData.map(e => e.bits)

  private val isEmpty = deqPtr === enqPtr
  when(io.writeSuccess || (!statusEntries(deqPtr.value).dirty && !isEmpty)) {
    deqPtr := deqPtr + 1.U
  }

  when(io.writeSuccess) {
    statusEntries(deqPtr.value).dirty := false.B
  }

  when(t1_hasWrite) {
    statusEntries(enqPtr.value).valid := true.B
    statusEntries(enqPtr.value).dirty := true.B
  }

  when(t1_hasWrite && t1_hasHit) {
    statusEntries(t1_cleanId).dirty := false.B
  }

  private val forceWrite = RegInit(false.B)
  // Early writeback to distribute SRAM write operations and avoid write pressure concentration
  // that exacerbates read/write conflicts on individual entries.
  forceWrite                  := distanceBetween(enqPtr, deqPtr) > (numEntry - 4).U
  io.tryWrite.valid           := statusEntries(deqPtr.value).valid && statusEntries(deqPtr.value).dirty
  io.tryWrite.bits.writeIndex := entries(deqPtr.value).index
  io.tryWrite.bits.writeData  := entries(deqPtr.value).entryData.map(_.bits)
  io.tryWrite.bits.forceWrite := forceWrite && statusEntries(deqPtr.value).valid && statusEntries(deqPtr.value).dirty
  io.tryWrite.bits.wMask      := VecInit(entries(deqPtr.value).entryData.map(_.valid)).asUInt

  // ==========================================================================
  // Buffer Performance Diagnostic Counters
  // ==========================================================================
  // 1. Replacement statistics
  XSPerfAccumulate("buffer_write_total", t1_hasWrite)
  XSPerfAccumulate("buffer_train_hit_total", t1_hasWrite && t1_hasHit)

  // 2. Bypass and error statistics
  XSPerfAccumulate("need_bypass", needBypass)
  private val multihit = (PopCount(t0_entryHit) > 1.U) && t0_fire
  XSPerfAccumulate("error_multihit", multihit)
}
