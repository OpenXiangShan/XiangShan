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
import utility.XSPerfHistogram
import utility.sram.SRAMTemplate
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
      class Resp extends Bundle {
        val setIdx:   UInt         = UInt(SetIdxLen.W)
        val evicted:  MainBtbEntry = new MainBtbEntry
        val incoming: MainBtbEntry = new MainBtbEntry
      }
      val resp: Valid[Resp] = Valid(new Resp)
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

  // Snapshot state is used only for entry writes that missed in MainBtb. A hit write updates
  // an existing way directly and does not evict another BTB entry. A miss write, however,
  // replaces the selected way, so this bank must first read the old entry and later report
  // both the incoming and evicted entries to the VictimBtb write path.
  //
  // There is one snapshot slot per way:
  // - snapshotValid(i) means way i has a miss write waiting for the old entry read and the
  //   later MainBtb write-back of the incoming entry.
  // - snapshotReq(i) keeps the buffered incoming write request.
  // - snapshotResp(i) keeps the old MainBtb entry returned by the entry SRAM read.
  //
  // snapshotValidOH serializes snapshot write-back with PriorityEncoderOH, so at most one
  // way writes the incoming entry back to MainBtb and emits a snapshot response per cycle.
  private val snapshotValid     = RegInit(VecInit.fill(NumWay)(false.B))
  private val snapshotValidNext = RegNext(snapshotValid, init = VecInit.fill(NumWay)(false.B))
  private val snapshotValidOH   = PriorityEncoderOH(snapshotValid)
  // snapshotDataValid fires one cycle after snapshotValid rises,
  //   when the old entry read response is available.
  // snapshotRespValid fires one cycle after snapshotValid falls,
  //   after the incoming entry has been accepted for write-back to MainBtb.
  private val snapshotDataValid = snapshotValid.zip(snapshotValidNext).map { case (v, n) => !n && v }
  private val snapshotRespValid = snapshotValid.zip(snapshotValidNext).map { case (v, n) => !v && n }

  private val snapshotReq  = Reg(Vec(NumWay, new MainBtbEntrySramWriteReq))
  private val snapshotResp = Reg(Vec(NumWay, new MainBtbEntry))

  Seq.tabulate(NumWay) { i =>
    switch(snapshotValid(i)) {
      is(false.B) {
        // Start a snapshot only for a pending miss write and only when the entry SRAM is not
        // serving a normal prediction read. The SRAM read issued below uses the pending set
        // index and captures the entry that will be evicted from this way.
        when(pendingValid(i) && !read.req.valid) {
          snapshotValid(i) := true.B
          snapshotReq(i)   := entryWriteBuffer.io.read(i).bits
        }
      }
      is(true.B) {
        // Once the old entry has been read and this way is selected by snapshotValidOH, write
        // the incoming entry into MainBtb. Clearing snapshotValid schedules snapshot.resp for
        // the following cycle, when the VBTB path receives a stable incoming/evicted pair.
        when(pendingReady(i) && snapshotValidOH(i)) {
          snapshotValid(i) := false.B
        }
      }
    }
    // The entry SRAM has one-cycle read latency with holdRead behavior, so the old entry is
    // captured on the cycle indicated by the valid-rise edge of snapshotValid.
    when(snapshotDataValid(i)) {
      snapshotResp(i) := entrySrams(i).io.r.resp.data.head
    }
  }
  snapshot.resp.valid         := snapshotRespValid.reduce(_ || _)
  snapshot.resp.bits.setIdx   := Mux1H(snapshotRespValid, snapshotReq.map(_.setIdx))
  snapshot.resp.bits.incoming := Mux1H(snapshotRespValid, snapshotReq.map(_.entry))
  snapshot.resp.bits.evicted  := Mux1H(snapshotRespValid, snapshotResp)

  /* *** sram -> io *** */
  entrySrams.zipWithIndex.foreach { case (s, i) =>
    // Entry SRAM reads are shared by normal prediction reads and snapshot reads. Prediction
    // reads have priority; snapshot reads are issued only when a pending miss write has not
    // already entered the snapshot state.
    s.io.r.req.valid       := read.req.valid || (pendingValid(i) && !snapshotValid(i))
    s.io.r.req.bits.setIdx := Mux(read.req.valid, read.req.bits.setIdx, pendingSetIdx(i))
  }
  counterSram.io.r.req.valid       := read.req.valid
  counterSram.io.r.req.bits.setIdx := read.req.bits.setIdx

  // each entry sram template has 1 way, so here we only read data.head
  read.resp.entries  := VecInit(entrySrams.map(_.io.r.resp.data.head))
  read.resp.counters := counterSram.io.r.resp.data

  /* *** writeBuffer -> sram *** */
  // entry
  (entrySrams zip entryWriteBuffer.io.read).zipWithIndex.foreach { case ((way, bufRead), i) =>
    // Hit writes can update the selected way directly. Miss writes are held in snapshotReq
    // and are written only when their snapshot is selected by snapshotValidOH, after the old
    // entry has been read for VBTB insertion. Reads block writes because entry SRAMs are
    // single-ported.
    way.io.w.req.valid        := Mux(snapshotValid(i), snapshotValidOH(i), hitValid(i)) && !way.io.r.req.valid
    way.io.w.req.bits.data(0) := Mux(snapshotValidOH(i), snapshotReq(i).entry, bufRead.bits.entry)
    way.io.w.req.bits.setIdx  := Mux(snapshotValidOH(i), snapshotReq(i).setIdx, bufRead.bits.setIdx)
    bufRead.ready := Mux(
      !snapshotValid(i),
      // A hit write leaves the write buffer when its direct SRAM write can proceed. A miss
      // write leaves the write buffer as soon as the snapshot read is accepted; the request
      // is then owned by snapshotReq until the incoming entry is written back.
      Mux(bufRead.bits.hit, pendingReady(i), !read.req.valid),
      false.B
    )
  }
  // counter
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
}
