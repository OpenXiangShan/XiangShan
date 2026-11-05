// Copyright (c) 2024-2026 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2026 Institute of Computing Technology, Chinese Academy of Sciences
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

package xiangshan.frontend.bpu.tage

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.ReplacementPolicy
import utility.XSPerfAccumulate
import utility.XSPerfHistogram
import xiangshan.frontend.bpu.TageTableInfo

class TageWriteBuffer(numEntries: Int)(implicit p: Parameters, info: TageTableInfo) extends TageModule {
  require(numEntries >= 1)

  class TageWriteBufferIO extends TageBundle {
    val write: Valid[WriteBufferWriteReq]        = Flipped(Valid(new WriteBufferWriteReq))
    val drain: DecoupledIO[TageWriteBufferDrain] = Decoupled(new TageWriteBufferDrain)

    val lookupSetIdx: UInt                         = Input(UInt(SetIdxWidth.W))
    val lookupResp:   Valid[WriteBufferLookupResp] = Output(Valid(new WriteBufferLookupResp))

    val resetUseful: Bool = Input(Bool())

    val overwrite: Bool = Output(Bool())
  }
  val io: TageWriteBufferIO = IO(new TageWriteBufferIO)

  private val entries     = RegInit(VecInit.fill(numEntries)(0.U.asTypeOf(new WriteBufferEntry)))
  private val valid       = RegInit(VecInit.fill(numEntries)(false.B))
  private val entryValid  = RegInit(VecInit.fill(numEntries)(false.B))
  private val usefulValid = RegInit(VecInit.fill(numEntries)(false.B))
  private val entryDirty  = RegInit(VecInit.fill(numEntries)(false.B))
  private val usefulDirty = RegInit(VecInit.fill(numEntries)(false.B))
  private val dirty       = VecInit(entryDirty.zip(usefulDirty).map { case (entry, useful) => entry || useful })
  private val full        = dirty.asUInt.andR

  private val nextEntries     = WireInit(entries)
  private val nextValid       = WireInit(valid)
  private val nextEntryValid  = WireInit(entryValid)
  private val nextUsefulValid = WireInit(usefulValid)
  private val nextEntryDirty  = WireInit(entryDirty)
  private val nextUsefulDirty = WireInit(usefulDirty)

  private val lookupSetIdx = RegNext(io.lookupSetIdx, 0.U)
  private val lookupHits = VecInit(entries.zip(valid).map { case (entry, valid) =>
    valid && entry.setIdx === lookupSetIdx
  })
  assert(PopCount(lookupHits) <= 1.U, "TageWriteBuffer lookup must match at most one entry")
  io.lookupResp.valid            := lookupHits.asUInt.orR
  io.lookupResp.bits.setIdx      := Mux1H(lookupHits, entries.map(_.setIdx))
  io.lookupResp.bits.tageEntry   := Mux1H(lookupHits, entries.map(_.tageEntry))
  io.lookupResp.bits.usefulCtr   := Mux1H(lookupHits, entries.map(_.usefulCtr))
  io.lookupResp.bits.entryValid  := Mux1H(lookupHits, entryValid)
  io.lookupResp.bits.usefulValid := Mux1H(lookupHits, usefulValid)

  // Drain the lowest-index dirty slot first.
  private val drainIdx = PriorityEncoder(dirty)
  io.drain.valid            := dirty.asUInt.orR
  io.drain.bits.setIdx      := entries(drainIdx).setIdx
  io.drain.bits.tageEntry   := entries(drainIdx).tageEntry
  io.drain.bits.usefulCtr   := entries(drainIdx).usefulCtr
  io.drain.bits.entryDirty  := entryDirty(drainIdx)
  io.drain.bits.usefulDirty := usefulDirty(drainIdx)

  private val drainInvalidEntry = io.drain.fire && entryValid(drainIdx) && !entries(drainIdx).tageEntry.valid

  // The drain writes the old snapshot. A simultaneous update below retains any newly changed fields.
  when(io.drain.fire) {
    nextEntryDirty(drainIdx)  := false.B
    nextUsefulDirty(drainIdx) := false.B
    when(drainInvalidEntry) {
      nextValid(drainIdx)       := false.B
      nextEntryValid(drainIdx)  := false.B
      nextUsefulValid(drainIdx) := false.B
    }
  }

  private val setHits = VecInit(entries.zip(valid).map { case (entry, valid) =>
    valid && entry.setIdx === io.write.bits.setIdx
  })
  assert(!io.write.valid || PopCount(setHits) <= 1.U, "TageWriteBuffer must retain one snapshot per set")
  private val hitIdx = OHToUInt(setHits)
  private val hit    = setHits.asUInt.orR && entries(hitIdx).tageEntry.tag === io.write.bits.tageEntry.tag

  // An invalidation may clear the tag. Reject stale updates to the same set regardless of their tag.
  private val updateInvalidEntry = io.write.bits.op === TageWriteOp.Update && setHits.asUInt.orR &&
    entryValid(hitIdx) && !entries(hitIdx).tageEntry.valid
  private val writeValid = io.write.valid && !updateInvalidEntry

  private val replacer = ReplacementPolicy.fromString("plru", numEntries)
  private val victim = Mux(
    setHits.asUInt.orR,
    hitIdx,
    // A successful drain frees its slot at this edge; reuse it before discarding another dirty entry.
    Mux(!full, PriorityEncoder(dirty.map(!_)), Mux(io.drain.fire, drainIdx, replacer.way))
  )
  private val writeAccess = Wire(Valid(UInt(log2Ceil(numEntries).W)))
  writeAccess.valid := writeValid
  writeAccess.bits  := Mux(hit, hitIdx, victim)
  replacer.access(Seq(writeAccess))

  when(writeValid) {
    // A new write to a retired invalidation slot installs a fresh snapshot.
    when(!hit || (drainInvalidEntry && drainIdx === hitIdx)) {
      // Retain the tag as the slot identity even when only useful has a valid snapshot.
      nextEntries(victim).setIdx        := io.write.bits.setIdx
      nextEntries(victim).tageEntry.tag := io.write.bits.tageEntry.tag
      when(io.write.bits.writeEntryEn) {
        nextEntries(victim).tageEntry := io.write.bits.tageEntry
      }
      when(io.write.bits.writeUsefulEn) {
        nextEntries(victim).usefulCtr := io.write.bits.usefulCtr
      }
      nextValid(victim)       := true.B
      nextEntryValid(victim)  := io.write.bits.writeEntryEn
      nextUsefulValid(victim) := io.write.bits.writeUsefulEn
      nextEntryDirty(victim)  := io.write.bits.writeEntryEn
      nextUsefulDirty(victim) := io.write.bits.writeUsefulEn
    }.otherwise {
      val old     = entries(hitIdx)
      val updated = WireInit(old)
      when(io.write.bits.writeEntryEn) {
        updated.tageEntry := io.write.bits.tageEntry
        updated.tageEntry.takenCtr := Mux(
          io.write.bits.op === TageWriteOp.Update && entryValid(hitIdx),
          old.tageEntry.takenCtr.getUpdate(io.write.bits.actualTaken),
          io.write.bits.tageEntry.takenCtr
        )
      }
      when(io.write.bits.writeUsefulEn) {
        updated.usefulCtr := Mux(
          io.write.bits.op === TageWriteOp.Update && usefulValid(hitIdx),
          old.usefulCtr.getIncrease(en = io.write.bits.incUseful),
          io.write.bits.usefulCtr
        )
      }
      val drained = io.drain.fire && drainIdx === hitIdx
      val entryChanged = io.write.bits.writeEntryEn &&
        (!entryValid(hitIdx) || old.tageEntry.asUInt =/= updated.tageEntry.asUInt)
      val usefulChanged = io.write.bits.writeUsefulEn &&
        (!usefulValid(hitIdx) || old.usefulCtr =/= updated.usefulCtr)
      nextEntries(hitIdx)     := updated
      nextEntryValid(hitIdx)  := entryValid(hitIdx) || io.write.bits.writeEntryEn
      nextUsefulValid(hitIdx) := usefulValid(hitIdx) || io.write.bits.writeUsefulEn
      nextEntryDirty(hitIdx)  := (entryDirty(hitIdx) && !drained) || entryChanged
      nextUsefulDirty(hitIdx) := (usefulDirty(hitIdx) && !drained) || usefulChanged
    }
  }

  // Useful reset takes priority over writes, preserving both the entry and its pending SRAM update.
  when(io.resetUseful) {
    for (slot <- 0 until numEntries) {
      nextEntries(slot).usefulCtr.value := 0.U
      nextUsefulValid(slot)             := nextValid(slot)
      nextUsefulDirty(slot)             := false.B
    }
  }

  entries     := nextEntries
  valid       := nextValid
  entryValid  := nextEntryValid
  usefulValid := nextUsefulValid
  entryDirty  := nextEntryDirty
  usefulDirty := nextUsefulDirty

  io.overwrite := writeValid && full && !hit && !(io.drain.fire && victim === drainIdx)

  XSPerfAccumulate("hit_not_written", io.write.valid && hit && dirty(hitIdx))
  XSPerfAccumulate("hit_written", io.write.valid && hit && !dirty(hitIdx))
  XSPerfAccumulate("hit", io.write.valid && hit)
  XSPerfAccumulate("not_hit", io.write.valid && !hit)
  XSPerfAccumulate("is_full", io.write.valid && full)
  XSPerfAccumulate("entry_dirty", PopCount(entryDirty))
  XSPerfAccumulate("useful_dirty", PopCount(usefulDirty))
  XSPerfHistogram("dirty_count", PopCount(dirty), dirty.asUInt.orR, 1, numEntries + 1)
}
