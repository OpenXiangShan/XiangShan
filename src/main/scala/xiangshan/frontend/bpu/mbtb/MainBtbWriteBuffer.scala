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

package xiangshan.frontend.bpu.mbtb

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.XSError
import utility.XSPerfAccumulate
import xiangshan.frontend.bpu.SaturateCounter

class MainBtbWriteBufferReq(implicit p: Parameters) extends MainBtbBundle {
  val setIdx:       UInt                 = UInt(SetIdxLen.W)
  val entryWayMask: UInt                 = UInt(NumWay.W)
  val entry:        MainBtbEntry         = new MainBtbEntry
  val counterMask:  UInt                 = UInt(NumWay.W)
  val counters:     Vec[SaturateCounter] = Vec(NumWay, TakenCounter())
}

class MainBtbWriteBufferSlot(implicit p: Parameters) extends MainBtbBundle {
  val setIdx:       UInt                 = UInt(SetIdxLen.W)
  val entryWayMask: UInt                 = UInt(NumWay.W)
  val entries:      Vec[MainBtbEntry]    = Vec(NumWay, new MainBtbEntry)
  val counterMask:  UInt                 = UInt(NumWay.W)
  val counters:     Vec[SaturateCounter] = Vec(NumWay, TakenCounter())
}

/**
 * A per-set write buffer shared by main BTB entry SRAMs and the counter SRAM.
 *
 * One slot holds one set's pending writes: an optional entry per way (selected by entryWayMask) and
 * an optional counter per way (selected by counterMask). A training write carries one entry (one-hot
 * way) plus a counter update mask; a multi-hit flush carries an entryWayMask only. Both the entry and
 * its counter are drained to SRAM in the same cycle, so a read can never observe a half-updated set.
 *
 * Slots are managed as a circular FIFO: `head` points at the oldest pending set and `tail` at the
 * next free slot, so pending sets are drained in insertion order and no slot can be starved. A write
 * that hits a pending set is merged in place, which keeps at most one slot per set (the read bypass
 * still selects a single slot) without reordering the queue. A miss on a full buffer is dropped and
 * counted; it never evicts an older, still-pending slot.
 *
 * There is a single write port. The upstream muxes training writes ahead of multi-hit flushes, so a
 * flush that loses arbitration is dropped (it is re-detected on the next access to that set). If a
 * write hits the oldest slot in the same cycle it is drained, the update is merged in place and the
 * slot is kept pending, deferring the drain by one cycle instead of allocating a second slot.
 */
class MainBtbWriteBuffer(implicit p: Parameters) extends MainBtbModule with Helpers {
  class MainBtbWriteBufferIO extends Bundle {
    class ReadBypass extends Bundle {
      val req: Valid[UInt] = Flipped(Valid(UInt(SetIdxLen.W)))

      class Resp extends Bundle {
        val valid:        Bool                 = Bool()
        val entryWayMask: UInt                 = UInt(NumWay.W)
        val entries:      Vec[MainBtbEntry]    = Vec(NumWay, new MainBtbEntry)
        val counterMask:  UInt                 = UInt(NumWay.W)
        val counters:     Vec[SaturateCounter] = Vec(NumWay, TakenCounter())
      }

      val resp: Resp = Output(new Resp)
    }

    val write:      Valid[MainBtbWriteBufferReq]        = Flipped(Valid(new MainBtbWriteBufferReq))
    val readBypass: ReadBypass                          = new ReadBypass
    val drain:      DecoupledIO[MainBtbWriteBufferSlot] = Decoupled(new MainBtbWriteBufferSlot)
    val empty:      Bool                                = Output(Bool())
    val full:       Bool                                = Output(Bool())
  }

  val io: MainBtbWriteBufferIO = IO(new MainBtbWriteBufferIO)

  private val PtrWidth = log2Ceil(WriteBufferSize).max(1)

  // valid slots always form a contiguous circular window [head, tail); a drained slot is cleared
  // before it can be hit again, and a new write only ever allocates at tail.
  private val valid = RegInit(VecInit(Seq.fill(WriteBufferSize)(false.B)))
  private val head  = RegInit(0.U(PtrWidth.W)) // oldest pending slot, drained first
  private val tail  = RegInit(0.U(PtrWidth.W)) // next free slot, allocated last

  private val setIdx       = Reg(Vec(WriteBufferSize, UInt(SetIdxLen.W)))
  private val entryWayMask = Reg(Vec(WriteBufferSize, UInt(NumWay.W)))
  private val entries      = Reg(Vec(WriteBufferSize, Vec(NumWay, new MainBtbEntry)))
  private val counterMask  = Reg(Vec(WriteBufferSize, UInt(NumWay.W)))
  private val counters     = Reg(Vec(WriteBufferSize, Vec(NumWay, TakenCounter())))

  private def wrapInc(ptr: UInt): UInt = Mux(ptr === (WriteBufferSize - 1).U, 0.U, ptr + 1.U)

  io.empty := !valid.asUInt.orR
  io.full  := valid.asUInt.andR
  XSError(!io.full && valid(tail), "MainBtbWriteBuffer tail should be free when not full")

  /* *** read bypass *** */
  private val bypassValid  = RegNext(io.readBypass.req.valid, false.B)
  private val bypassSetIdx = RegEnable(io.readBypass.req.bits, io.readBypass.req.valid)
  private val bypassHitVec = VecInit(valid.zip(setIdx).map { case (v, s) => v && s === bypassSetIdx })
  XSError(
    bypassValid && PopCount(bypassHitVec) > 1.U,
    "MainBtbWriteBuffer read bypass should hit at most one slot"
  )
  io.readBypass.resp.valid        := bypassValid && bypassHitVec.asUInt.orR
  io.readBypass.resp.entryWayMask := Mux1H(bypassHitVec, entryWayMask)
  io.readBypass.resp.entries      := Mux1H(bypassHitVec, entries)
  io.readBypass.resp.counterMask  := Mux1H(bypassHitVec, counterMask)
  io.readBypass.resp.counters     := Mux1H(bypassHitVec, counters)

  /* *** drain: always the oldest slot (head) *** */
  private val drainIdx = head
  io.drain.valid             := !io.empty
  io.drain.bits.setIdx       := setIdx(drainIdx)
  io.drain.bits.entryWayMask := entryWayMask(drainIdx)
  io.drain.bits.entries      := entries(drainIdx)
  io.drain.bits.counterMask  := counterMask(drainIdx)
  io.drain.bits.counters     := counters(drainIdx)
  XSError(io.drain.valid && !valid(drainIdx), "MainBtbWriteBuffer head should point to a valid slot")

  /* *** write decode *** */
  // Single write port (training has priority over a multi-hit flush, muxed upstream).
  private val w      = io.write
  private val hitVec = VecInit((0 until WriteBufferSize).map(s => valid(s) && setIdx(s) === w.bits.setIdx))
  private val hit    = hitVec.asUInt.orR
  private val hitIdx = OHToUInt(hitVec)
  private val isFull = valid.asUInt.andR
  private val alloc  = w.valid && !hit && !isFull

  XSError(w.valid && PopCount(hitVec) > 1.U, "MainBtbWriteBuffer should hit at most one slot")

  // A full miss normally drops the write. An entry-bearing write, however, evicts a counter-only slot
  // instead (losing that counter update), so frequent counter traffic never takes entry capacity.
  private val counterOnlyVec = VecInit((0 until WriteBufferSize).map(s => valid(s) && !entryWayMask(s).orR))
  private val replace =
    w.valid && !hit && isFull && w.bits.entryWayMask.orR && counterOnlyVec.asUInt.orR
  private val victimIdx  = PriorityEncoder(counterOnlyVec)
  private val replaceVec = VecInit((0 until WriteBufferSize).map(s => replace && victimIdx === s.U))
  private val drop       = w.valid && !hit && isFull && !replace
  XSPerfAccumulate("mbtb_writebuffer_drop", drop)
  XSPerfAccumulate("mbtb_writebuffer_replace", replace)

  // The training metadata can be stale: it may miss a branch that is already pending in this slot and
  // pick a fresh victim way, which would write the same (tag, position) into two ways. If the entry is
  // already pending in another valid way of the slot, ignore this update instead: the pending entry
  // already covers the branch, and evicting the victim way would only lose a useful entry. A flush or a
  // counter-only write carries entry = 0 (entry.valid = false), so it is never treated as a duplicate.
  private val duplicateVec = VecInit((0 until NumWay).map(j =>
    hit && entryWayMask(hitIdx)(j) && entries(hitIdx)(j).valid && !w.bits.entryWayMask(j) &&
      entries(hitIdx)(j).tag === w.bits.entry.tag &&
      entries(hitIdx)(j).position === w.bits.entry.position
  ))
  private val duplicate = w.valid && w.bits.entry.valid && duplicateVec.asUInt.orR
  XSPerfAccumulate("mbtb_writebuffer_duplicate", duplicate)

  // A write hitting the oldest slot also drains it to SRAM this cycle. Fold the update in and keep the
  // slot (do not advance head): the current SRAM write carries the older value, the merged one drains
  // next cycle.
  private val drainedHit  = io.drain.fire && w.valid && hit && hitIdx === drainIdx
  private val replaceHead = replace && victimIdx === head
  private val advanceHead = io.drain.fire && !drainedHit && !replaceHead
  private val mergeVec    = VecInit((0 until WriteBufferSize).map(s => w.valid && hitVec(s) && !duplicate))
  private val allocVec    = VecInit((0 until WriteBufferSize).map(s => alloc && tail === s.U))
  private val writeVec    = VecInit((0 until WriteBufferSize).map(s => allocVec(s) || replaceVec(s)))

  valid := VecInit((0 until WriteBufferSize).map { s =>
    Mux(advanceHead && head === s.U, false.B, Mux(writeVec(s), true.B, valid(s)))
  })
  head := Mux(advanceHead, wrapInc(head), head)
  tail := Mux(alloc, wrapInc(tail), tail)

  setIdx := VecInit((0 until WriteBufferSize).map(s => Mux(writeVec(s), w.bits.setIdx, setIdx(s))))
  entryWayMask := VecInit((0 until WriteBufferSize).map { s =>
    Mux(writeVec(s), w.bits.entryWayMask, Mux(mergeVec(s), entryWayMask(s) | w.bits.entryWayMask, entryWayMask(s)))
  })
  counterMask := VecInit((0 until WriteBufferSize).map { s =>
    Mux(writeVec(s), w.bits.counterMask, Mux(mergeVec(s), counterMask(s) | w.bits.counterMask, counterMask(s)))
  })
  entries := VecInit((0 until WriteBufferSize).map { s =>
    VecInit((0 until NumWay).map { i =>
      Mux(writeVec(s), w.bits.entry, Mux(mergeVec(s) && w.bits.entryWayMask(i), w.bits.entry, entries(s)(i)))
    })
  })
  counters := VecInit((0 until WriteBufferSize).map { s =>
    VecInit((0 until NumWay).map { i =>
      Mux(
        writeVec(s),
        w.bits.counters(i),
        Mux(mergeVec(s) && w.bits.counterMask(i), w.bits.counters(i), counters(s)(i))
      )
    })
  })
}
