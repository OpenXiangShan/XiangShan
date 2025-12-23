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
import xiangshan.frontend.bpu.FilterQueue
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
        val entries: Vec[MainBtbEntry]      = Vec(NumWay, new MainBtbEntry)
        val shareds: Vec[MainBtbSharedInfo] = Vec(NumWay, new MainBtbSharedInfo)
      }

      val req:  Valid[Req] = Flipped(Valid(new Req))
      val resp: Resp       = Output(new Resp)
    }

    class WriteEntry extends Bundle {
      class Req extends Bundle {
        val setIdx:  UInt              = UInt(SetIdxLen.W)
        val wayMask: UInt              = UInt(NumWay.W)
        val entry:   MainBtbEntry      = new MainBtbEntry
        val shared:  MainBtbSharedInfo = new MainBtbSharedInfo
      }

      val req: Valid[Req] = Flipped(Valid(new Req))
    }

    class WriteShared extends Bundle {
      class Req extends Bundle {
        val setIdx:  UInt                   = UInt(SetIdxLen.W)
        val wayMask: UInt                   = UInt(NumWay.W)
        val shareds: Vec[MainBtbSharedInfo] = Vec(NumWay, new MainBtbSharedInfo)
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

    val resetDone: Bool = Output(Bool())

    val read:        Read        = new Read
    val writeEntry:  WriteEntry  = new WriteEntry
    val writeShared: WriteShared = new WriteShared
    val flush:       Flush       = new Flush
  }

  val io: MainBtbInternalBankIO = IO(new MainBtbInternalBankIO)

  // alias
  private val read        = io.read
  private val writeEntry  = io.writeEntry
  private val writeShared = io.writeShared
  private val flush       = io.flush

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
  private val sharedSrams = Seq.tabulate(NumWay) { wayIdx =>
    Module(
      new SRAMTemplate(
        new MainBtbSharedInfo,
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
    ).suggestName(s"mbtb_sram_shared_align${alignIdx}_bank${bankIdx}_way${wayIdx}")
  }

  private val entryWriteBuffer = Module(new WriteBuffer(
    new MainBtbEntrySramWriteReq,
    numEntries = WriteBufferSize,
    numPorts = NumWay,
    nameSuffix = s"mbtbEntryAlign${alignIdx}_Bank${bankIdx}"
  ))

  private val sharedWriteBuffers = Seq.tabulate(NumWay) { _ =>
    Module(new FilterQueue(
      new MainBtbSharedSramWriteReq,
      WriteBufferSize,
      pipe = true,
      flow = false,
      hasFilter = true,
      hasOverrider = false,
      filter = (older: MainBtbSharedSramWriteReq, newer: MainBtbSharedSramWriteReq) =>
        older.setIdx === newer.setIdx
    ))
  }

  private val resetDone = RegInit(false.B)
  when((entrySrams :++ sharedSrams).map(_.io.r.req.ready).reduce(_ && _)) {
    resetDone := true.B
  }
  io.resetDone := resetDone

  /* *** sram -> io *** */
  // handle entry & shared together
  (entrySrams :++ sharedSrams).foreach { sram =>
    sram.io.r.req.valid       := read.req.valid
    sram.io.r.req.bits.setIdx := read.req.bits.setIdx
  }
  // each entry sram template has 1 way, so here we only read data.head
  read.resp.entries := VecInit(entrySrams.map(_.io.r.resp.data.head))
  read.resp.shareds := VecInit(sharedSrams.map(_.io.r.resp.data.head))

  /* *** writeBuffer -> sram *** */
  // entry
  (entrySrams zip entryWriteBuffer.io.read).foreach { case (way, bufRead) =>
    way.io.w.req.valid        := bufRead.valid && !way.io.r.req.valid
    way.io.w.req.bits.data(0) := bufRead.bits.entry
    way.io.w.req.bits.setIdx  := bufRead.bits.setIdx
    bufRead.ready             := way.io.w.req.ready && !way.io.r.req.valid
  }

  // state
  private val entryWriteShared = Wire(Vec(NumWay, Decoupled(new MainBtbSharedSramWriteReq)))
  entryWriteShared.zipWithIndex.foreach { case (shared, i) =>
    shared.valid       := entryWriteBuffer.io.read(i).fire
    shared.bits.setIdx := entryWriteBuffer.io.read(i).bits.setIdx
    shared.bits.shared := entryWriteBuffer.io.read(i).bits.shared
    assert( // when entry write fires, shared SRAM write must be ready, since they are read at the same time
      !shared.valid || sharedSrams(i).io.w.req.ready,
      s"entry fire but shared SRAM not ready, blame to align${alignIdx}_bank${bankIdx}_way${i}"
    )
  }
  // have arbiter between shared write and entry write
  // filter same setIdx writes in shared write buffer when entry write occurs
  private val sharedWriteArbiter = Seq.tabulate(NumWay)(_ => Module(new Arbiter(new MainBtbSharedSramWriteReq, 2)))
  (sharedSrams zip sharedWriteArbiter zip entryWriteShared zip sharedWriteBuffers).foreach {
    case (((way, arbiter), entry), shared) =>
      way.io.w.req.valid        := arbiter.io.out.valid && !way.io.r.req.valid
      way.io.w.req.bits.data(0) := arbiter.io.out.bits.shared
      way.io.w.req.bits.setIdx  := arbiter.io.out.bits.setIdx
      arbiter.io.out.ready      := way.io.w.req.ready && !way.io.r.req.valid

      // entry write has higher priority
      arbiter.io.in(0) <> entry
      arbiter.io.in(1) <> shared.io.deq

      // filter same setIdx writes
      shared.io.filter.valid := entry.valid
      shared.io.filter.bits  := entry.bits
  }

  /* *** io -> writeBuffer *** */
  // entry
  private val conflict =
    writeEntry.req.valid &&
      writeEntry.req.bits.setIdx === flush.req.bits.setIdx &&
      writeEntry.req.bits.entry.tagLower === 0.U

  entryWriteBuffer.io.write.zipWithIndex.foreach { case (bufWrite, i) =>
    val writeValid = writeEntry.req.valid && writeEntry.req.bits.wayMask(i)
    val flushValid = flush.req.valid && flush.req.bits.wayMask(i) && !conflict
    val valid      = writeValid || flushValid
    bufWrite.valid := RegNext(valid, false.B)
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
    bufWrite.bits.shared := RegEnable(
      Mux(
        writeValid,
        writeEntry.req.bits.shared,
        0.U.asTypeOf(new MainBtbSharedInfo)
      ),
      valid
    )
  }

  // shared
  sharedWriteBuffers.zipWithIndex.foreach { case (buf, i) =>
    buf.io.enq.valid       := writeShared.req.valid && writeShared.req.bits.wayMask(i)
    buf.io.enq.bits.setIdx := writeShared.req.bits.setIdx
    buf.io.enq.bits.shared := writeShared.req.bits.shareds(i)
  }

  /* *** perf *** */
  private val perf_entryDropWrite = (0 until NumWay).map { i =>
    writeEntry.req.valid && writeEntry.req.bits.wayMask(i) && !entryWriteBuffer.io.write(i).ready
  }.reduce(_ || _)

  private val perf_sharedDropWrite = (0 until NumWay).map { i =>
    writeShared.req.valid && writeShared.req.bits.wayMask(i) && !sharedWriteBuffers(i).io.enq.ready
  }.reduce(_ || _)

  XSPerfAccumulate(
    "multihit_write_conflict",
    writeEntry.req.valid && flush.req.valid && writeEntry.req.bits.setIdx === flush.req.bits.setIdx &&
      (writeEntry.req.bits.wayMask & flush.req.bits.wayMask).orR
  )
  XSPerfAccumulate(
    "entry_writebuffer_drop_write",
    perf_entryDropWrite
  )
  XSPerfAccumulate(
    "shared_writebuffer_drop_write",
    perf_sharedDropWrite
  )
}
