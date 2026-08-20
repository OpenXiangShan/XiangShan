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

    val sramResetDone: Bool = Output(Bool())

    // context flush handshake (not generated when HasBpuFlush is off)
    val contextFlush: Option[Bool] = Option.when(HasBpuFlush)(Input(Bool()))
    val bpuFlushing:  Option[Bool] = Option.when(HasBpuFlush)(Input(Bool()))

    val read:         Read         = new Read
    val writeEntry:   WriteEntry   = new WriteEntry
    val writeCounter: WriteCounter = new WriteCounter
    val flush:        Flush        = new Flush
  }

  val io: MainBtbInternalBankIO = IO(new MainBtbInternalBankIO)

  // Intermediate unpacked signals (SPEC 04 §4.2.1); else false.B is only a Scala
  // type placeholder: all consumers below live inside if (HasBpuFlush) guards.
  private val contextFlush = if (HasBpuFlush) io.contextFlush.get else false.B
  private val bpuFlushing  = if (HasBpuFlush) io.bpuFlushing.get  else false.B

  // alias
  private val read         = io.read
  private val writeEntry   = io.writeEntry
  private val writeCounter = io.writeCounter
  private val flush        = io.flush

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
        extraReset = HasBpuFlush, // elaboration-time constant: extra_reset port is not generated when off
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
    extraReset = HasBpuFlush, // elaboration-time constant: extra_reset port is not generated when off
    hasMbist = hasMbist,
    hasSramCtl = hasSramCtl,
    suffix = Option("bpu_mbtb_counter")
  )).suggestName(s"mbtb_sram_counter_align${alignIdx}_bank${bankIdx}")

  // Runtime context flush drives the SRAM extra_reset, triggering the 256-cycle
  // sweep; the sweep window is shared by entry and counter SRAMs (SPEC 04 §4.2.2 / §4.3.2)
  if (HasBpuFlush) {
    (entrySrams :+ counterSram).foreach { sram =>
      sram.extra_reset.get := contextFlush
    }
  }

  private val entryWriteBuffer = Module(new WriteBuffer(
    new MainBtbEntrySramWriteReq,
    numEntries = WriteBufferSize,
    numPorts = NumWay,
    hasContextFlush = HasBpuFlush, // clears dirty + shadowValid on the flush pulse (SPEC 04 §4.4.1)
    nameSuffix = s"mbtbEntryAlign${alignIdx}_Bank${bankIdx}"
  ))
  if (HasBpuFlush) {
    entryWriteBuffer.io.contextFlush.get := contextFlush
  }

  private val counterWriteBuffer = Module(new Queue(
    new MainBtbCounterSramWriteReq,
    WriteBufferSize,
    pipe = true,
    flow = true,
    hasFlush = HasBpuFlush // resets enq_ptr/deq_ptr/maybe_full on the flush pulse (SPEC 04 §4.5.1)
  ))
  if (HasBpuFlush) {
    counterWriteBuffer.io.flush.get := contextFlush
  }

  io.sramResetDone := entrySrams.map(_.io.resetDone).reduce(_ && _) && counterSram.io.resetDone

  /* *** sram -> io *** */
  // handle entry & counter together
  (entrySrams :+ counterSram).foreach { sram =>
    sram.io.r.req.valid       := read.req.valid
    sram.io.r.req.bits.setIdx := read.req.bits.setIdx
  }
  // each entry sram template has 1 way, so here we only read data.head
  read.resp.entries  := VecInit(entrySrams.map(_.io.r.resp.data.head))
  read.resp.counters := counterSram.io.r.resp.data

  /* *** writeBuffer -> sram *** */
  // entry
  (entrySrams zip entryWriteBuffer.io.read).foreach { case (way, bufRead) =>
    way.io.w.req.valid        := bufRead.valid && !way.io.r.req.valid
    way.io.w.req.bits.data(0) := bufRead.bits.entry
    way.io.w.req.bits.setIdx  := bufRead.bits.setIdx
    bufRead.ready             := way.io.w.req.ready && !way.io.r.req.valid
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
    // unified gate at the merge point: one AND term blocks both write sources
    // (train writes + multi-hit zero writes) during the whole flush window (SPEC 04 §4.4.3)
    val valid      = (writeValid || flushValid) && (if (HasBpuFlush) !bpuFlushing else true.B)
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
  }
  // counter, dont care flush (`hit` is controlled by entry)
  // single write source, gate enq.valid directly for a whole-window hard guarantee (SPEC 04 §4.5.3)
  counterWriteBuffer.io.enq.valid         := writeCounter.req.valid && (if (HasBpuFlush) !bpuFlushing else true.B)
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
