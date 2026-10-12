// Copyright (c) 2024-2026 Beijing Institute of Open Source Chip (BOSC)
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
// See the Mulan PSL v2 for more details.

package xiangshan.frontend.bpu.tage

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.XSPerfAccumulate
import xiangshan.frontend.bpu.TageTableInfo

/** Entry and useful share identity and replacement, but have independent SRAM writeback ports. */
class TageWriteBuffer(implicit p: Parameters, info: TageTableInfo) extends TageModule {
  require(WriteBufferSize > 0)

  val io = IO(new Bundle {
    val write        = Flipped(Valid(new TageSramWriteReq))
    val entryWrite   = Decoupled(new TageWriteBufferEntry)
    val usefulWrite  = Decoupled(new TageWriteBufferEntry)
    val resetUseful  = Input(Bool())
    val full         = Output(Bool())
    val dropped      = Output(Bool())
    val lookupEn     = Input(Vec(NumTableReadPorts, Bool()))
    val lookupSetIdx = Input(Vec(NumTableReadPorts, UInt(SetIdxWidth.W)))
    val lookup       = Output(Vec(NumTableReadPorts, Valid(new TageWriteBufferEntry)))
  })

  private val entries     = RegInit(VecInit.fill(WriteBufferSize)(0.U.asTypeOf(new TageWriteBufferEntry)))
  private val valid       = RegInit(VecInit.fill(WriteBufferSize)(false.B))
  private val entryDirty  = RegInit(VecInit.fill(WriteBufferSize)(false.B))
  private val usefulDirty = RegInit(VecInit.fill(WriteBufferSize)(false.B))
  private val nextEntries = WireInit(entries)
  private val nextValid   = WireInit(valid)

  private val entryIdx  = PriorityEncoder(entryDirty)
  private val usefulIdx = PriorityEncoder(usefulDirty)
  io.entryWrite.valid  := entryDirty.asUInt.orR
  io.entryWrite.bits   := entries(entryIdx)
  io.usefulWrite.valid := usefulDirty.asUInt.orR && !io.resetUseful
  io.usefulWrite.bits  := entries(usefulIdx)
  private val entryAfterDrain = VecInit((0 until WriteBufferSize).map { i =>
    entryDirty(i) && !(io.entryWrite.fire && entryIdx === i.U)
  })
  private val usefulAfterDrain = VecInit((0 until WriteBufferSize).map { i =>
    usefulDirty(i) && !(io.usefulWrite.fire && usefulIdx === i.U) && !io.resetUseful
  })
  private val nextEntryDirty  = WireInit(entryAfterDrain)
  private val nextUsefulDirty = WireInit(usefulAfterDrain)

  private val pending = VecInit(entryDirty.zip(usefulDirty).map { case (e, u) => e || (u && !io.resetUseful) })
  io.full := pending.asUInt.andR

  private val addressHit = VecInit(entries.zip(valid).map { case (entry, v) =>
    v && entry.setIdx === io.write.bits.setIdx
  })
  private val hit     = addressHit.asUInt.orR
  private val hitIdx  = OHToUInt(addressHit)
  private val sameTag = hit && entries(hitIdx).entry.tag === io.write.bits.entry.tag
  private val stale   = hit && entries(hitIdx).entryValid && !sameTag && !io.write.bits.allocate
  // A slot is reusable only after BOTH writes complete, including completion this cycle.
  private val reusable = VecInit(entryAfterDrain.zip(usefulAfterDrain).map { case (e, u) =>
    !e && !u
  })
  private val canAccept = hit || reusable.asUInt.orR
  private val index     = Mux(hit, hitIdx, PriorityEncoder(reusable))
  private val writeValid = io.write.valid &&
    (io.write.bits.writeEntryEn || (io.write.bits.writeUsefulEn && !io.resetUseful))

  io.dropped := writeValid && !canAccept
  when(writeValid && canAccept && !stale) {
    val incoming = io.write.bits
    when(!hit || !sameTag || incoming.allocate) {
      nextValid(index)               := true.B
      nextEntries(index).setIdx      := incoming.setIdx
      nextEntries(index).entry       := incoming.entry
      nextEntries(index).entryValid  := incoming.writeEntryEn
      nextEntries(index).usefulCtr   := incoming.usefulCtr
      nextEntries(index).usefulValid := incoming.usefulValid || incoming.writeUsefulEn
      nextEntryDirty(index)          := incoming.writeEntryEn
      nextUsefulDirty(index)         := incoming.writeUsefulEn && !io.resetUseful
    }.otherwise {
      val old = entries(index)
      when(incoming.writeEntryEn) {
        nextEntries(index).entry := incoming.entry
        when(old.entryValid) {
          nextEntries(index).entry.takenCtr := old.entry.takenCtr.getUpdate(incoming.actualTaken)
        }
        nextEntries(index).entryValid := true.B
        nextEntryDirty(index)         := true.B
      }
      when(incoming.writeUsefulEn && !io.resetUseful) {
        // Non-allocation useful writes are increments. Merge against the latest
        // shadow value instead of overwriting it with a stale metadata snapshot.
        nextEntries(index).usefulCtr := Mux(
          old.usefulValid,
          old.usefulCtr.getIncrease(),
          incoming.usefulCtr
        )
        nextEntries(index).usefulValid := true.B
        nextUsefulDirty(index)         := true.B
      }.elsewhen(!old.usefulValid && incoming.usefulValid) {
        nextEntries(index).usefulCtr   := incoming.usefulCtr
        nextEntries(index).usefulValid := true.B
      }
    }
  }

  // Reset has priority over drains and arrivals. Entry identity/counter and its
  // pending write survive, while no pre-reset useful write can reach SRAM later.
  when(io.resetUseful) {
    for (i <- 0 until WriteBufferSize) {
      nextEntries(i).usefulCtr   := UsefulCounter.Zero
      nextEntries(i).usefulValid := true.B
      nextUsefulDirty(i)         := false.B
    }
  }

  for (port <- 0 until NumTableReadPorts) {
    val matches = VecInit(nextEntries.zip(nextValid).map { case (entry, v) =>
      io.lookupEn(port) && v && entry.setIdx === io.lookupSetIdx(port)
    })
    io.lookup(port).valid := matches.asUInt.orR
    io.lookup(port).bits  := Mux1H(matches, nextEntries)
    assert(PopCount(matches) <= 1.U, "TAGE write buffer must have one shadow per set")
  }

  entries     := nextEntries
  valid       := nextValid
  entryDirty  := nextEntryDirty
  usefulDirty := nextUsefulDirty

  XSPerfAccumulate("full", io.full)
  XSPerfAccumulate("full_drop", io.dropped)
  XSPerfAccumulate("stale_tag_drop", writeValid && stale)
  XSPerfAccumulate("entry_writeback", io.entryWrite.fire)
  XSPerfAccumulate("useful_writeback", io.usefulWrite.fire)
}
