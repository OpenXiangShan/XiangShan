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
        val counters: Vec[SaturateCounter] = Vec(NumWay, new SaturateCounter(TakenCntWidth))
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
        val counters: Vec[SaturateCounter] = Vec(NumWay, new SaturateCounter(TakenCntWidth))
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

    val read:         Read         = new Read
    val writeEntry:   WriteEntry   = new WriteEntry
    val writeCounter: WriteCounter = new WriteCounter
    val flush:        Flush        = new Flush
  }

  val io: MainBtbInternalBankIO = IO(new MainBtbInternalBankIO)

  // alias
  private val r     = io.read
  private val we    = io.writeEntry
  private val wc    = io.writeCounter
  private val flush = io.flush

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
    new SaturateCounter(TakenCntWidth),
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

  private val resetDone = RegInit(false.B)
  when(entrySrams.map(_.io.r.req.ready).reduce(_ && _) && counterSram.io.r.req.ready) {
    resetDone := true.B
  }
  io.resetDone := resetDone

  /* *** sram -> io *** */
  // handle entry & counter together
  (entrySrams :+ counterSram).foreach { sram =>
    sram.io.r.req.valid       := r.req.valid
    sram.io.r.req.bits.setIdx := r.req.bits.setIdx
  }
  // each entry sram template has 1 way, so here we only read data.head
  r.resp.entries  := VecInit(entrySrams.map(_.io.r.resp.data.head))
  r.resp.counters := counterSram.io.r.resp.data

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
  private val conflict = we.req.valid && we.req.bits.setIdx === flush.req.bits.setIdx && we.req.bits.entry.tag === 0.U
  entryWriteBuffer.io.write.zipWithIndex.foreach { case (bufWrite, i) =>
    val writeValid = we.req.valid && we.req.bits.wayMask(i)
    val flushValid = flush.req.valid && flush.req.bits.wayMask(i) && !conflict
    bufWrite.valid := writeValid || flushValid
    bufWrite.bits.setIdx := Mux(
      writeValid,
      we.req.bits.setIdx,
      flush.req.bits.setIdx
    )
    bufWrite.bits.entry := Mux(
      writeValid,
      we.req.bits.entry,
      0.U.asTypeOf(new MainBtbEntry)
    )
  }
  // counter, dont care flush (`hit` is controlled by entry)
  counterWriteBuffer.io.enq.valid         := wc.req.valid
  counterWriteBuffer.io.enq.bits.setIdx   := wc.req.bits.setIdx
  counterWriteBuffer.io.enq.bits.wayMask  := wc.req.bits.wayMask
  counterWriteBuffer.io.enq.bits.counters := wc.req.bits.counters

  XSPerfAccumulate(
    "multihit_write_conflict",
    we.req.valid && flush.req.valid && we.req.bits.setIdx === flush.req.bits.setIdx &&
      (we.req.bits.wayMask & flush.req.bits.wayMask).orR
  )

  XSPerfAccumulate(
    "counter_writebuffer_drop_write",
    counterWriteBuffer.io.enq.ready && counterWriteBuffer.io.enq.valid
  )
}
