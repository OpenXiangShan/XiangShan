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
import xiangshan.frontend.bpu.CompareMatrix
import xiangshan.frontend.bpu.FoldedHistoryInfo
import xiangshan.frontend.bpu.SaturateCounter
import xiangshan.frontend.bpu.history.phr.PhrAllFoldedHistories
import yunsuan.vector.alu.VIntFixpTable.table

class BypassShadowBuffer(
    val numSets:  Int,
    val numWay:   Int,
    val numEntry: Int = 16, // Fixed to 16, keeping the most recent 16 entries
    val tableId:  Int,
    val NumBanks: Int = 4
)(implicit p: Parameters) extends MicroTageModule with Helpers {

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

  // Buffer entry structure
  class BufferEntry extends Bundle {
    val valid:     Bool                       = Bool()
    val entryData: Vec[Valid[MicroTageEntry]] = Vec(numWay, Valid(new MicroTageEntry))
    val index:     UInt                       = UInt(log2Ceil(MaxNumSets).W)
  }

  // Replacement item for buffer management
  class ReplaceItem extends Bundle {
    val valid: Bool = Bool()
    val age:   UInt = UInt(log2Ceil(numEntry).W)
    val dirty: Bool = Bool()
  }

  // Buffer storage
  private val entries       = RegInit(VecInit(Seq.fill(numEntry)(0.U.asTypeOf(new BufferEntry))))
  private val statusEntries = RegInit(VecInit(Seq.fill(numEntry)(0.U.asTypeOf(new ReplaceItem))))
  private val enqPtr        = RegInit(0.U(log2Ceil(numEntry).W)) // Next available position
  private val enqMask       = RegInit(0.U(numEntry.W))
  private val deqPtr        = RegInit(0.U(log2Ceil(numEntry).W)) // Position ready for write-back

  // Banked useful counters
  private val usefulEntries = RegInit(VecInit.tabulate(NumBanks) { bankIdx =>
    VecInit(Seq.fill(numSets / NumBanks)(
      VecInit(Seq.fill(numWay)(0.U.asTypeOf(new SaturateCounter(UsefulWidth))))
    ))
  })

  // ==========================================================================
  // Prediction Logic (Stage 0 and 1)
  // ==========================================================================
  private val entryDataVec = VecInit(entries.map(e => e.entryData))

  // Stage 0: Lookup
  private val a0_entryHitOH = Wire(Vec(numEntry, Bool()))
  a0_entryHitOH := entries.map(e => (e.index === io.req.readIndex) && e.valid)

  // Stage 1: Hit determination
  private val a1_entryHitOH        = RegNext(a0_entryHitOH, 0.U.asTypeOf(Vec(numEntry, Bool())))
  private val a1_bufferEntry       = Mux1H(a1_entryHitOH, entryDataVec)
  private val a1_hasHit            = a1_entryHitOH.reduce(_ || _)
  private val a1_microTageHitVec   = a1_bufferEntry.map(e => e.valid && a1_hasHit)
  private val a1_microTageEntryVec = a1_bufferEntry.map(e => e.bits)

  io.resp.hit         := a1_microTageHitVec
  io.resp.readEntries := a1_microTageEntryVec

  // ==========================================================================
  // Training Logic - Stage 0
  // ==========================================================================
  private val t0_fire       = io.train.t0_trainIndex.valid
  private val t0_trainIndex = io.train.t0_trainIndex.bits
  private val t0_entryHitOH = Wire(Vec(numEntry, Bool()))
  t0_entryHitOH := entries.map(e => (e.index === t0_trainIndex) && e.valid)
  private val t0_hasHit            = t0_entryHitOH.reduce(_ || _)
  private val t0_bufferEntry       = Mux1H(t0_entryHitOH, entryDataVec)
  private val t0_microTageHitVec   = VecInit(t0_bufferEntry.map(e => e.valid && t0_hasHit))
  private val t0_microTageEntryVec = VecInit(t0_bufferEntry.map(e => e.bits))

  // Access useful registers for t0 stage
  private val t0_bankIdx          = getBankId(t0_trainIndex, NumBanks)
  private val t0_bankOffset       = getBankInnerIndex(t0_trainIndex, NumBanks, numSets)
  private val t0_trainReadUseful  = usefulEntries(t0_bankIdx)(t0_bankOffset)
  private val t0_hitBufferId      = OHToUInt(t0_entryHitOH)
  private val t0_trainReadEntries = t0_microTageEntryVec

  // Provide training information for stage 0
  for (way <- 0 until numWay) {
    val entry  = t0_trainReadEntries(way)
    val useful = t0_trainReadUseful(way)
    io.train.t0_read(way).canGetPosition := t0_microTageHitVec(way)
    io.train.t0_read(way).cfiPosition    := entry.cfiPosition
    io.train.t0_read(way).useful         := useful.value
  }

  // ==========================================================================
  // Bypass Logic
  // Bypass allows writing erroneous data but never allows entries with same readIndex
  // ==========================================================================
  private val needBypass        = WireDefault(false.B)
  private val bypassId          = WireDefault(0.U(log2Ceil(numEntry).W))
  private val bypassHasHit      = Wire(Bool())
  private val bypassHitVec      = Wire(Vec(numWay, Bool()))
  private val bypassReadEntries = Wire(Vec(numWay, new MicroTageEntry))

  // ==========================================================================
  // Training Logic - Stage 1
  // ==========================================================================
  private val t1_fire             = RegNext(t0_fire, false.B)
  private val t1_trainIndex       = RegNext(t0_trainIndex, 0.U(log2Ceil(MaxNumSets).W))
  private val t1_hasHit           = RegNext(Mux(needBypass, bypassHasHit, t0_hasHit), false.B)
  private val t1_microTageHitVec  = RegNext(Mux(needBypass, bypassHitVec, t0_microTageHitVec))
  private val t1_hitBufferId      = RegNext(Mux(needBypass, bypassId, t0_hitBufferId), 0.U(log2Ceil(numEntry).W))
  private val t1_trainReadEntries = RegNext(Mux(needBypass, bypassReadEntries, t0_trainReadEntries))
  private val t1_bufferWriteId    = Mux(t1_hasHit, t1_hitBufferId, enqPtr)

  // Access useful registers for t1 stage
  private val t1_bankIdx         = getBankId(t1_trainIndex, NumBanks)
  private val t1_bankOffset      = getBankInnerIndex(t1_trainIndex, NumBanks, numSets)
  private val t1_trainReadUseful = usefulEntries(t1_bankIdx)(t1_bankOffset)

  // ==========================================================================
  // Entry Update Logic
  // ==========================================================================
  private val writeBufferValid     = WireDefault(VecInit(Seq.fill(numWay)(false.B)))
  private val newMicroTageEntryVec = WireDefault(VecInit(Seq.fill(numWay)(0.U.asTypeOf(new MicroTageEntry))))
  for (way <- 0 until numWay) {
    val oldEntry       = t1_trainReadEntries(way)
    val oldTakenCtr    = oldEntry.takenCtr
    val updateTakenCtr = io.train.t1_update(way).bits.updateTakenCtr

    // Determine if this way needs update (allocation or regular update)
    val doAlloc  = io.train.t1_alloc.valid && io.train.t1_alloc.bits.wayMask(way)
    val doUpdate = io.train.t1_update(way).valid && io.train.t1_update(way).bits.updateValid
    writeBufferValid(way) := doAlloc || doUpdate

    // Calculate new entry values
    newMicroTageEntryVec(way).valid := true.B
    newMicroTageEntryVec(way).tag   := io.train.t1_tag
    newMicroTageEntryVec(way).cfiPosition :=
      Mux(doAlloc, io.train.t1_alloc.bits.cfiPosition, io.train.t1_update(way).bits.updateCfiPosition)
    newMicroTageEntryVec(way).takenCtr := Mux(
      doAlloc,
      Mux(io.train.t1_alloc.bits.taken, TakenCounter.WeakPositive, TakenCounter.WeakNegative),
      Mux(
        t1_microTageHitVec(way) && (oldEntry.tag === io.train.t1_tag),
        oldTakenCtr.getUpdate(io.train.t1_update(way).bits.updateTaken),
        updateTakenCtr.getUpdate(io.train.t1_update(way).bits.updateTaken)
      )
    )
  }

  // ==========================================================================
  // Useful Counter Update Logic
  // ==========================================================================
  for (way <- 0 until numWay) {
    val doAlloc   = io.train.t1_alloc.valid && io.train.t1_alloc.bits.wayMask(way)
    val oldUseful = t1_trainReadUseful(way)
    val newUseful = Mux(
      doAlloc,
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
        }
      }
    }
  }

  // ==========================================================================
  // Buffer Entry Update
  // ==========================================================================
  private val newBufferEntry = Wire(new BufferEntry)
  newBufferEntry.valid := true.B
  newBufferEntry.index := t1_trainIndex
  for (i <- 0 until numWay) {
    newBufferEntry.entryData(i).valid := writeBufferValid(i) || entries(t1_bufferWriteId).entryData(i).valid
    newBufferEntry.entryData(i).bits := Mux(
      writeBufferValid(i),
      newMicroTageEntryVec(i),
      entries(t1_bufferWriteId).entryData(i).bits
    )
  }

  private val t1_hasWrite = writeBufferValid.reduce(_ || _)
  when(t1_hasWrite) {
    entries(t1_bufferWriteId) := newBufferEntry
  }

  // ==========================================================================
  // Bypass Logic Implementation
  // ==========================================================================
  needBypass        := (t1_trainIndex === t0_trainIndex) && t1_hasWrite
  bypassId          := t1_bufferWriteId
  bypassHasHit      := true.B
  bypassHitVec      := newBufferEntry.entryData.map(e => e.valid)
  bypassReadEntries := newBufferEntry.entryData.map(e => e.bits)

  // ==========================================================================
  // Replacement Policy Logic
  // ==========================================================================
  private val t1_ageVec = Wire(Vec(numEntry, UInt(log2Ceil(numEntry).W)))
  t1_ageVec                   := statusEntries.map(e => e.age)
  t1_ageVec(t1_bufferWriteId) := (numEntry - 1).U
  private val writeBufferMask = UIntToOH(t1_bufferWriteId)

  // Replacement rules:
  // 1. Accessed entry: timestamp = 15 (youngest)
  // 2. Unaccessed entries: timestamp = timestamp - 1 each cycle (aging)
  // 3. Replacement selection: choose entry with smallest timestamp (oldest)
  private val t1_compareMatrix = CompareMatrix(t1_ageVec)

  // Fix: Explicitly create Bool Vec to avoid width inference issues
  private val t1_invalidEntryVec = VecInit(statusEntries.map(e => !e.valid))
  private val t1_cleanEntryVec   = VecInit(statusEntries.map(e => e.valid && !e.dirty))
  private val t1_dirtyEntryVec   = VecInit(statusEntries.map(e => e.valid && e.dirty))

  // Create masked versions
  private val t1_invalidEntryVecMasked = Wire(Vec(numEntry, Bool()))
  private val t1_cleanEntryVecMasked   = Wire(Vec(numEntry, Bool()))
  private val t1_dirtyEntryVecMasked   = Wire(Vec(numEntry, Bool()))

  for (i <- 0 until numEntry) {
    t1_invalidEntryVecMasked(i) := t1_invalidEntryVec(i) && !writeBufferMask(i)
    t1_cleanEntryVecMasked(i)   := t1_cleanEntryVec(i) && !writeBufferMask(i)
    t1_dirtyEntryVecMasked(i)   := t1_dirtyEntryVec(i) || writeBufferMask(i)
  }

  private val t1_invalidEntryOH = t1_compareMatrix.getLeastElementOH(t1_invalidEntryVecMasked)
  private val t1_cleanEntryOH   = t1_compareMatrix.getLeastElementOH(t1_cleanEntryVecMasked)
  private val t1_dirtyEntryOH   = t1_compareMatrix.getLeastElementOH(t1_dirtyEntryVecMasked)

  private val t1_invalidId  = OHToUInt(t1_invalidEntryOH)
  private val t1_cleanId    = OHToUInt(t1_cleanEntryOH)
  private val t1_dirtyId    = OHToUInt(t1_dirtyEntryOH)
  private val t1_hasInValid = t1_invalidEntryVecMasked.reduce(_ || _)
  private val t1_hasInClean = t1_cleanEntryVecMasked.reduce(_ || _)
  private val t1_hasInDirty = t1_dirtyEntryVec.reduce(_ || _)

  // Dequeue pointer update
  when(io.writeSuccess || !statusEntries(deqPtr).dirty) {
    deqPtr := t1_dirtyId
  }

  // Enqueue pointer update
  private val nexEnqPtr = Mux(t1_hasInValid, t1_invalidId, Mux(t1_hasInClean, t1_cleanId, t1_dirtyId))
  when(t1_hasWrite && (!t1_hasHit || (enqPtr === t1_bufferWriteId))) {
    enqPtr  := nexEnqPtr
    enqMask := UIntToOH(nexEnqPtr)
  }

  // Update status entries (age, valid, dirty bits)
  when(io.writeSuccess || t1_hasWrite) {
    for (i <- 0 until numEntry) {
      statusEntries(i).valid := Mux(i.U === t1_bufferWriteId && t1_hasWrite, true.B, statusEntries(i).valid)
      statusEntries(i).dirty := Mux(
        i.U === t1_bufferWriteId && t1_hasWrite,
        true.B,
        Mux(i.U === deqPtr && io.writeSuccess, false.B, statusEntries(i).dirty)
      )
      statusEntries(i).age :=
        Mux(
          t1_hasWrite,
          Mux(
            i.U === t1_bufferWriteId,
            (numEntry - 1).U,
            Mux(statusEntries(i).age === 0.U, 0.U, statusEntries(i).age - 1.U)
          ),
          statusEntries(i).age
        )
    }
  }

  // ==========================================================================
  // Write-back Control Logic
  // ==========================================================================
  private val bufferDirtyVec = VecInit(statusEntries.map(e => e.dirty && e.valid))
  private val bufferCounter  = RegNext(PopCount(bufferDirtyVec), 0.U(log2Ceil(numEntry).W))
  private val forceWrite     = bufferCounter >= (numEntry - numWay).U

  io.tryWrite.valid           := statusEntries(deqPtr).valid && statusEntries(deqPtr).dirty
  io.tryWrite.bits.writeIndex := entries(deqPtr).index
  io.tryWrite.bits.writeData  := entries(deqPtr).entryData.map(_.bits)
  io.tryWrite.bits.forceWrite := forceWrite && statusEntries(deqPtr).valid && statusEntries(deqPtr).dirty
  io.tryWrite.bits.wMask      := VecInit(entries(deqPtr).entryData.map(_.valid)).asUInt

  // ==========================================================================
  // Buffer Performance Diagnostic Counters
  // ==========================================================================

  // 1. Buffer hit rate statistics
  XSPerfAccumulate("buffer_hit_total", a1_hasHit)

  private val writeNew = t1_hasWrite && (!t1_hasHit || (enqPtr === t1_bufferWriteId))

  // 2. Replacement statistics
  XSPerfAccumulate("buffer_replace_total", writeNew)
  XSPerfAccumulate("buffer_replace_invalid", writeNew && t1_hasInValid)
  XSPerfAccumulate("buffer_replace_clean", writeNew && !t1_hasInValid && t1_hasInClean)
  XSPerfAccumulate("buffer_replace_dirty", writeNew && !t1_hasInValid && !t1_hasInClean)

  // 3. Buffer write-back analysis
  XSPerfAccumulate("buffer_writeback_total", io.tryWrite.valid)
  for (i <- 0 until 8) {
    XSPerfAccumulate(f"buffer_writeback_age${i}", io.tryWrite.valid && statusEntries(deqPtr).age === i.U)
  }

  // 4. Bypass and error statistics
  XSPerfAccumulate("need_bypass", needBypass)
  private val multihit = (PopCount(t0_entryHitOH) > 1.U) && t0_fire
  dontTouch(multihit)
  XSPerfAccumulate("error_multihit", multihit)
}
