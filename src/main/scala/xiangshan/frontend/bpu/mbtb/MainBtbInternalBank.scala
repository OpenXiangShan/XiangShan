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
    alignIdx:     Int,
    bankIdx:      Int,
    hasTrainRead: Boolean
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

    val read:         Read         = new Read
    val writeEntry:   WriteEntry   = new WriteEntry
    val writeCounter: WriteCounter = new WriteCounter
    val flush:        Flush        = new Flush

    // A second read port, for looking an entry up on behalf of training. It is served by a shadow of the storage
    // rather than by the arrays above, so a look-up never has to wait for a prediction. trainReadBusy reports the one
    // cycle it can still be turned away: the shadow only stays in step with the predictor's copy if it never misses a
    // write, so a write always wins the port.
    val trainRead:     Option[Read] = Option.when(hasTrainRead)(new Read)
    val trainReadBusy: Option[Bool] = Option.when(hasTrainRead)(Output(Bool()))
  }

  val io: MainBtbInternalBankIO = IO(new MainBtbInternalBankIO)

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

  // The shadow. Same geometry, same reset, same writes on the same cycles as the two arrays above -- the only thing
  // it does not share is the read port, which is what training uses it for. There is one write path, one replacer and
  // one write buffer in this bank, so there is nothing here that could decide differently and let the copies drift.
  private val shadowEntrySrams = Option.when(hasTrainRead)(Seq.tabulate(NumWay) { wayIdx =>
    Module(
      new SRAMTemplate(
        new MainBtbEntry,
        set = NumSets,
        way = 1,
        singlePort = true,
        shouldReset = true,
        holdRead = true,
        withClockGate = true,
        hasMbist = hasMbist,
        hasSramCtl = hasSramCtl,
        suffix = Option("bpu_mbtb_entry")
      )
    ).suggestName(s"mbtb_sram_entry_shadow_align${alignIdx}_bank${bankIdx}_way${wayIdx}")
  })

  private val shadowCounterSram = Option.when(hasTrainRead)(Module(new SRAMTemplate(
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
  )).suggestName(s"mbtb_sram_counter_shadow_align${alignIdx}_bank${bankIdx}"))

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

  private val shadowResetDone =
    shadowEntrySrams.map(_.map(_.io.resetDone).reduce(_ && _)).getOrElse(true.B) &&
      shadowCounterSram.map(_.io.resetDone).getOrElse(true.B)
  io.sramResetDone := entrySrams.map(_.io.resetDone).reduce(_ && _) && counterSram.io.resetDone && shadowResetDone

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

  /* *** writeBuffer -> shadow sram, and the train read *** */
  // The shadow takes every write the array it copies takes, in the same order, but it is allowed to take them late.
  // A look-up that had to wait would nearly always be waiting on the write the previous training event had just
  // produced, so holding writes back a cycle or two is what keeps look-ups moving. The queues are sized so that a
  // look-up yields only when one is nearly full, which bounds how far behind the copy can fall.
  private val ShadowWriteQueueSize = 4
  private val shadowEntryQueues = Option.when(hasTrainRead)(Seq.fill(NumWay)(Module(new Queue(
    new MainBtbEntrySramWriteReq,
    ShadowWriteQueueSize
  ))))
  private val shadowCounterQueue = Option.when(hasTrainRead)(Module(new Queue(
    new MainBtbCounterSramWriteReq,
    ShadowWriteQueueSize
  )))

  shadowEntryQueues.foreach { queues =>
    (queues zip (entrySrams zip entryWriteBuffer.io.read)).foreach { case (q, (way, bufRead)) =>
      q.io.enq.valid := bufRead.valid && !way.io.r.req.valid
      q.io.enq.bits  := bufRead.bits
      assert(!q.io.enq.valid || q.io.enq.ready, "MainBtb shadow entry write queue overflowed")
    }
  }
  shadowCounterQueue.foreach { q =>
    q.io.enq.valid := counterWriteBuffer.io.deq.valid && !counterSram.io.r.req.valid
    q.io.enq.bits  := counterWriteBuffer.io.deq.bits
    assert(!q.io.enq.valid || q.io.enq.ready, "MainBtb shadow counter write queue overflowed")
  }

  // A look-up yields only to a queue that has no room left to absorb this cycle's write.
  io.trainReadBusy.foreach { busy =>
    val nearlyFull = ShadowWriteQueueSize - 1
    busy := (shadowEntryQueues.get.map(_.io.count) :+ shadowCounterQueue.get.io.count)
      .map(_ >= nearlyFull.U).reduce(_ || _)
  }

  io.trainRead.foreach { tr =>
    val fires = tr.req.valid && !io.trainReadBusy.get
    (shadowEntrySrams.get :+ shadowCounterSram.get).foreach { sram =>
      sram.io.r.req.valid       := fires
      sram.io.r.req.bits.setIdx := tr.req.bits.setIdx
    }
    tr.resp.entries  := VecInit(shadowEntrySrams.get.map(_.io.r.resp.data.head))
    tr.resp.counters := shadowCounterSram.get.io.r.resp.data

    (shadowEntrySrams.get zip shadowEntryQueues.get).foreach { case (shadow, q) =>
      shadow.io.w.req.valid        := q.io.deq.valid && !fires
      shadow.io.w.req.bits.data(0) := q.io.deq.bits.entry
      shadow.io.w.req.bits.setIdx  := q.io.deq.bits.setIdx
      q.io.deq.ready               := shadow.io.w.req.ready && !fires
    }
    val counterShadow = shadowCounterSram.get
    val counterQueue  = shadowCounterQueue.get
    counterShadow.io.w.req.valid            := counterQueue.io.deq.valid && !fires
    counterShadow.io.w.req.bits.data        := counterQueue.io.deq.bits.counters
    counterShadow.io.w.req.bits.setIdx      := counterQueue.io.deq.bits.setIdx
    counterShadow.io.w.req.bits.waymask.get := counterQueue.io.deq.bits.wayMask
    counterQueue.io.deq.ready               := counterShadow.io.w.req.ready && !fires
  }

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
