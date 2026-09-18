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
import utility.XSPerfAccumulate
import utility.sram.SRAMTemplate
import utils.EnumUInt
import xiangshan.frontend.bpu.SaturateCounter
import xiangshan.frontend.bpu.WriteBuffer

class MainBtbInternalBank(
    alignIdx: Int,
    bankIdx:  Int
)(implicit p: Parameters) extends MainBtbModule with Helpers {
  class MainBtbInternalBankIO extends Bundle {
    class Read extends Bundle {
      class Req extends Bundle {
        val setIdx: UInt = UInt(SetIdxLen.W)
      }
      class Resp extends Bundle {
        val entries:  Vec[MainBtbEntry]    = Vec(NumWay, new MainBtbEntry)
        val counters: Vec[SaturateCounter] = Vec(NumWay, TakenCounter())
      }

      val req:  Valid[Req] = Flipped(Valid(new Req))
      val resp: Resp       = Output(new Resp)
    }

    class WriteEntry extends Bundle {
      class Req extends Bundle {
        val hit:     Bool         = Bool()
        val setIdx:  UInt         = UInt(SetIdxLen.W)
        val wayMask: UInt         = UInt(NumWay.W)
        val entry:   MainBtbEntry = new MainBtbEntry
      }

      val req: Valid[Req] = Flipped(Valid(new Req))
    }

    class WriteCounter extends Bundle {
      class Req extends Bundle {
        val setIdx:   UInt                 = UInt(SetIdxLen.W)
        val wayMask:  UInt                 = UInt(NumWay.W)
        val counters: Vec[SaturateCounter] = Vec(NumWay, TakenCounter())
      }

      val req: Valid[Req] = Flipped(Valid(new Req))
    }

    // flush interface for multi-hit
    class Flush extends Bundle {
      class Req extends Bundle {
        val setIdx:  UInt = UInt(SetIdxLen.W)
        val wayMask: UInt = UInt(NumWay.W)
      }

      val req: Valid[Req] = Flipped(Valid(new Req))
    }

    class Snapshot extends Bundle {
      val resp: DecoupledIO[MainBtbSnapshotResp] = Decoupled(new MainBtbSnapshotResp)
    }

    val sramResetDone: Bool = Output(Bool())

    val read:         Read         = new Read
    val writeEntry:   WriteEntry   = new WriteEntry
    val writeCounter: WriteCounter = new WriteCounter
    val flush:        Flush        = new Flush
    val snapshot:     Snapshot     = new Snapshot
  }

  val io: MainBtbInternalBankIO = IO(new MainBtbInternalBankIO)

  // alias
  private val read         = io.read
  private val writeEntry   = io.writeEntry
  private val writeCounter = io.writeCounter
  private val flush        = io.flush
  private val snapshot     = io.snapshot

  private val entrySrams = Seq.tabulate(NumWay) { wayIdx =>
    Module(
      new SRAMTemplate(
        new MainBtbEntry,
        set = NumSets,
        way = 1, // Not using way in the template, preparing for future skewed assoc
        singlePort = true,
        shouldReset = true,
        holdRead = true,
        withClockGate = true,
        hasMbist = hasMbist,
        hasSramCtl = hasSramCtl,
        suffix = Option("bpu_mbtb_entry")
      )
    ).suggestName(s"mbtb_sram_entry_align${alignIdx}_bank${bankIdx}_way${wayIdx}")
  }

  // we often need to update counter, but not the whole entry, so store counters in separate SRAMs for better power
  private val counterSram = Module(new SRAMTemplate(
    TakenCounter(),
    set = NumSets,
    way = NumWay,
    singlePort = true,
    shouldReset = true,
    holdRead = true,
    withClockGate = true,
    hasMbist = hasMbist,
    hasSramCtl = hasSramCtl,
    suffix = Option("bpu_mbtb_counter")
  )).suggestName(s"mbtb_sram_counter_align${alignIdx}_bank${bankIdx}")

  private val entryWriteBuffer = Module(new WriteBuffer(
    new MainBtbEntrySramWriteReq,
    numEntries = WriteBufferSize,
    numPorts = NumWay,
    nameSuffix = s"mbtbEntryAlign${alignIdx}_Bank${bankIdx}"
  ))

  private val counterWriteBuffer = Module(new Queue(
    new MainBtbCounterSramWriteReq,
    WriteBufferSize,
    pipe = true,
    flow = true
  ))

  io.sramResetDone := entrySrams.map(_.io.resetDone).reduce(_ && _) && counterSram.io.resetDone

  private val hitValid      = entryWriteBuffer.io.read.map(p => p.valid && p.bits.hit)
  private val pendingValid  = entryWriteBuffer.io.read.map(p => p.valid && !p.bits.hit)
  private val pendingReady  = entrySrams.map(way => way.io.w.req.ready && !way.io.r.req.valid)
  private val pendingSetIdx = entryWriteBuffer.io.read.map(_.bits.setIdx)

  // A miss write replaces the selected MainBtb way. Before overwriting it, capture the old
  // entry and keep the incoming entry until the snapshot has been accepted by the VBTB.
  // There is one independent snapshot slot per way, while the VBTB output is arbitrated
  // across all ways.
  object SnapshotState extends EnumUInt(4) {
    def Idle:      UInt = 0.U(width.W)
    def ReadSram:  UInt = 1.U(width.W)
    def SendVbtb:  UInt = 2.U(width.W)
    def WriteSram: UInt = 3.U(width.W)
  }
  private val snapshotState      = RegInit(VecInit.fill(NumWay)(SnapshotState.Idle))
  private val snapshotResp       = RegInit(VecInit.fill(NumWay)(0.U.asTypeOf(new MainBtbSnapshotResp)))
  private val snapshotWayArbiter = Module(new RRArbiter(new MainBtbSnapshotResp, NumWay, initLastGrant = true))

  snapshotState.zipWithIndex.foreach { case (state, i) =>
    switch(state) {
      is(SnapshotState.Idle) {
        when(pendingValid(i) && !read.req.valid) {
          state                    := SnapshotState.ReadSram
          snapshotResp(i).setIdx   := entryWriteBuffer.io.read(i).bits.setIdx
          snapshotResp(i).incoming := entryWriteBuffer.io.read(i).bits.entry
        }
      }
      is(SnapshotState.ReadSram) {
        state                   := SnapshotState.SendVbtb
        snapshotResp(i).evicted := entrySrams(i).io.r.resp.data.head
      }
      is(SnapshotState.SendVbtb) {
        // Do not overwrite MainBtb until VBTB accepts this snapshot.
        when(snapshotWayArbiter.io.in(i).fire) {
          state := Mux(pendingReady(i), SnapshotState.Idle, SnapshotState.WriteSram)
        }
      }
      is(SnapshotState.WriteSram) {
        when(pendingReady(i)) {
          state := SnapshotState.Idle
        }
      }
    }
  }

  snapshotWayArbiter.io.in.zipWithIndex.foreach { case (in, i) =>
    in.valid := snapshotState(i) === SnapshotState.SendVbtb
    in.bits  := snapshotResp(i)
  }

  snapshot.resp <> snapshotWayArbiter.io.out

  (entrySrams zip entryWriteBuffer.io.read).zipWithIndex.foreach { case ((way, bufRead), i) =>
    way.io.r.req.valid       := read.req.valid || (pendingValid(i) && snapshotState(i) === SnapshotState.Idle)
    way.io.r.req.bits.setIdx := Mux(read.req.valid, read.req.bits.setIdx, pendingSetIdx(i))
    // Hit writes update the selected way directly. A miss write is issued in the same cycle
    // as the VBTB handshake when possible, or in WriteSram after the VBTB handshake.
    when(snapshotState(i) === SnapshotState.Idle) {
      way.io.w.req.valid        := hitValid(i) && !way.io.r.req.valid
      way.io.w.req.bits.data(0) := bufRead.bits.entry
      way.io.w.req.bits.setIdx  := bufRead.bits.setIdx
      bufRead.ready             := Mux(bufRead.bits.hit, pendingReady(i), !read.req.valid)
    }.otherwise {
      way.io.w.req.valid := (snapshotWayArbiter.io.in(i).fire ||
        snapshotState(i) === SnapshotState.WriteSram) && !way.io.r.req.valid
      way.io.w.req.bits.data(0) := snapshotResp(i).incoming
      way.io.w.req.bits.setIdx  := snapshotResp(i).setIdx
      bufRead.ready             := false.B
    }
  }

  // each entry sram template has 1 way, so here we only read data.head
  read.resp.entries  := VecInit(entrySrams.map(_.io.r.resp.data.head))
  read.resp.counters := counterSram.io.r.resp.data

  /* *** writeBuffer -> sram *** */
  // entry

  // counter
  counterSram.io.r.req.valid            := read.req.valid
  counterSram.io.r.req.bits.setIdx      := read.req.bits.setIdx
  counterSram.io.w.req.valid            := counterWriteBuffer.io.deq.valid && !counterSram.io.r.req.valid
  counterSram.io.w.req.bits.data        := counterWriteBuffer.io.deq.bits.counters
  counterSram.io.w.req.bits.setIdx      := counterWriteBuffer.io.deq.bits.setIdx
  counterSram.io.w.req.bits.waymask.get := counterWriteBuffer.io.deq.bits.wayMask
  counterWriteBuffer.io.deq.ready       := counterSram.io.w.req.ready && !counterSram.io.r.req.valid

  /* *** io -> writeBuffer *** */
  // entry
  private val conflict =
    writeEntry.req.valid &&
      writeEntry.req.bits.setIdx === flush.req.bits.setIdx &&
      writeEntry.req.bits.entry.tag === 0.U

  entryWriteBuffer.io.write.zipWithIndex.foreach { case (bufWrite, i) =>
    val writeValid = writeEntry.req.valid && writeEntry.req.bits.wayMask(i)
    val flushValid = flush.req.valid && flush.req.bits.wayMask(i) && !conflict
    val valid      = writeValid || flushValid
    bufWrite.valid := RegNext(valid, false.B)
    // Treat a flush write as a hit write because it only clears the MainBtb entry and does
    // not evict a live entry that needs to be captured and inserted into VictimBtb.
    bufWrite.bits.hit := RegEnable(
      Mux(writeValid, writeEntry.req.bits.hit, true.B),
      valid
    )
    bufWrite.bits.setIdx := RegEnable(
      Mux(
        writeValid,
        writeEntry.req.bits.setIdx,
        flush.req.bits.setIdx
      ),
      valid
    )
    bufWrite.bits.entry := RegEnable(
      Mux(
        writeValid,
        writeEntry.req.bits.entry,
        0.U.asTypeOf(new MainBtbEntry)
      ),
      valid
    )
  }
  // counter, dont care flush (`hit` is controlled by entry)
  counterWriteBuffer.io.enq.valid         := writeCounter.req.valid
  counterWriteBuffer.io.enq.bits.setIdx   := writeCounter.req.bits.setIdx
  counterWriteBuffer.io.enq.bits.wayMask  := writeCounter.req.bits.wayMask
  counterWriteBuffer.io.enq.bits.counters := writeCounter.req.bits.counters

  private val perfEntryOverwrite = entryWriteBuffer.io.overwrite.reduce(_ || _)

  XSPerfAccumulate(
    "multihit_write_conflict",
    writeEntry.req.valid && flush.req.valid && writeEntry.req.bits.setIdx === flush.req.bits.setIdx &&
      (writeEntry.req.bits.wayMask & flush.req.bits.wayMask).orR
  )

  XSPerfAccumulate(
    "counter_writebuffer_drop_write",
    !counterWriteBuffer.io.enq.ready && counterWriteBuffer.io.enq.valid
  )
  XSPerfAccumulate(
    "entry_writebuffer_overwrite",
    perfEntryOverwrite
  )
  XSPerfAccumulate(
    "vbtb_snapshot_wait_cycle",
    snapshot.resp.valid && !snapshot.resp.ready
  )
  snapshotWayArbiter.io.in.zipWithIndex.foreach { case (in, i) =>
    XSPerfAccumulate(s"vbtb_snapshot_way_${i}_wait_cycle", in.valid && !in.ready)
    XSPerfAccumulate(s"vbtb_snapshot_way_${i}_grant", in.fire)
  }
}
