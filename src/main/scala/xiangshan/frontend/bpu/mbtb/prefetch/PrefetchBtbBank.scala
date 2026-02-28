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

package xiangshan.frontend.bpu.mbtb.prefetch

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.XSPerfAccumulate
import utility.sram.SplittedSRAMTemplate
import utility.sram.SRAMTemplate
import xiangshan.frontend.bpu.WriteBuffer
import xiangshan.frontend.bpu.mbtb.MainBtbCounterSramWriteReq
import xiangshan.frontend.bpu.mbtb.TakenCounter

class PrefetchBtbBank(bandIdx: Int)(implicit p: Parameters) extends PrefetchBtbModule {
  class BankIO(implicit p: Parameters) extends PrefetchBtbBundle {
    val readReq:         ValidIO[BankReadReq]         = Flipped(Valid(new BankReadReq))
    val readResp:        BankReadResp                 = Output(new BankReadResp)
    val writeEntryReq:   ValidIO[BankWriteEntryReq]   = Flipped(Valid(new BankWriteEntryReq))
    val trainCounterReq: ValidIO[BankWriteCounterReq] = Flipped(Valid(new BankWriteCounterReq))
    val trainInvalidReq: ValidIO[InvalidReq]          = Flipped(Valid(new InvalidReq))
    val ifuInvalidReq:   ValidIO[InvalidReq]          = Flipped(Valid(new InvalidReq))
    val trainUsedReq:    ValidIO[UsedReq]             = Flipped(Valid(new UsedReq()))
    val resetDone:       Bool                         = Output(Bool())
  }
  val io: BankIO = IO(new BankIO)

  private val entrySrams = Seq.tabulate(NumWay) { wayIdx =>
    Module(
      new SRAMTemplate(
        new PrefetchBtbSramEntry,
        set = NumSets,
        way = 1, // Not using way in the template, preparing for future skewed assoc
        singlePort = true,
        shouldReset = true,
        holdRead = true,
        withClockGate = true,
        hasMbist = hasMbist,
        hasSramCtl = hasSramCtl,
        suffix = Option("bpu_prefetchbtb_entry")
      )
    ).suggestName(s"prefetchbtb_sram_entry_bank${bandIdx}_way${wayIdx}")
  }

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
    suffix = Option("bpu_prefetch_counter")
  )).suggestName(s"prefetch_sram_counter${bandIdx}")

  private val valid  = RegInit(VecInit(Seq.fill(NumSets)(VecInit(Seq.fill(NumWay)(false.B)))))
  private val victim = RegInit(VecInit(Seq.fill(NumSets)(VecInit(Seq.fill(NumWay)(false.B)))))
  private val used   = RegInit(VecInit(Seq.fill(NumSets)(VecInit(Seq.fill(NumWay)(false.B)))))

  private val resetDone = RegInit(false.B)
  when(entrySrams.map(_.io.r.req.ready).reduce(_ && _) && counterSram.io.r.req.ready) {
    resetDone := true.B
  }
  io.resetDone := resetDone
  /* --------------------------------------------------------------------------------------------------------------
     read
     -------------------------------------------------------------------------------------------------------------- */
  private val entryValid  = valid(io.readReq.bits.setIdx)
  private val victimValid = victim(io.readReq.bits.setIdx)
  private val usedValid   = used(io.readReq.bits.setIdx)
  entrySrams.foreach { sram =>
    sram.io.r.req.valid       := io.readReq.valid
    sram.io.r.req.bits.setIdx := io.readReq.bits.setIdx
  }
  counterSram.io.r.req.valid       := io.readReq.valid
  counterSram.io.r.req.bits.setIdx := io.readReq.bits.setIdx
  for (i <- 0 until NumWay) {
    io.readResp.entries(i).sramData := entrySrams(i).io.r.resp.data.head
    io.readResp.entries(i).valid    := RegEnable(entryValid(i), io.readReq.fire)
    io.readResp.entries(i).victim   := RegEnable(victimValid(i), io.readReq.fire)
    io.readResp.entries(i).used     := RegEnable(usedValid(i), io.readReq.fire)
    io.readResp.entries(i).counter  := counterSram.io.r.resp.data(i)
  }

  /* --------------------------------------------------------------------------------------------------------------
     write
     -------------------------------------------------------------------------------------------------------------- */

  private val entryWriteBuffer = Module(new WriteBuffer(
    new EntryBufWriteReq,
    WriteBufferSize,
    numPorts = NumWay,
    nameSuffix = s"prefetchbtbBankEntry$bandIdx"
  ))

  private val counterWriteBuffer = Module(new Queue(
    new CounterBufWriteReq,
    WriteBufferSize,
    pipe = true,
    flow = true
  ))
  // writeReq is a ValidIO, it means that the new request will be dropped if the buffer is full

  entryWriteBuffer.io.write.zipWithIndex.foreach { case (buf, idx) =>
    val wvalid = io.writeEntryReq.valid && io.writeEntryReq.bits.wayMask(idx)
    buf.valid        := wvalid
    buf.bits.setIdx  := io.writeEntryReq.bits.setIdx
    buf.bits.entry   := io.writeEntryReq.bits.entry(idx)
    buf.bits.wayMask := io.writeEntryReq.bits.wayMask
  }

  private val trainInvalidSetIdx = io.trainInvalidReq.bits.setIdx
  private val ifuInvalidSetIdx   = io.ifuInvalidReq.bits.setIdx
  (entrySrams zip entryWriteBuffer.io.read).foreach { case (way, bufRead) =>
    way.io.w.req.valid        := bufRead.valid && !way.io.r.req.valid
    way.io.w.req.bits.data(0) := bufRead.bits.entry.sramData
    way.io.w.req.bits.setIdx  := bufRead.bits.setIdx
    bufRead.ready             := way.io.w.req.ready && !way.io.r.req.valid
  }
  for (i <- 0 until NumWay) {
    val entryWriteValid   = entryWriteBuffer.io.read(i).valid && !io.readReq.valid
    val entryWriteEntry   = entryWriteBuffer.io.read(i).bits.entry
    val entryWriteSetIdx  = entryWriteBuffer.io.read(i).bits.setIdx
    val entryWriteWayMask = entryWriteBuffer.io.read(i).bits.wayMask
    val needWrite         = entryWriteValid && entryWriteWayMask(i).asBool
    val trainNeedInvalid  = io.trainInvalidReq.valid && io.trainInvalidReq.bits.needInvalid(i)
    val ifuNeedInvalid    = io.ifuInvalidReq.valid && io.ifuInvalidReq.bits.needInvalid(i)
    val usedNeedValid     = io.trainUsedReq.valid && io.trainUsedReq.bits.wayMask(i)
    // TODO:refactor this logic
    when(needWrite) {
      valid(entryWriteSetIdx)(i)  := entryWriteEntry.valid
      victim(entryWriteSetIdx)(i) := entryWriteEntry.victim
      used(entryWriteSetIdx)(i)   := entryWriteEntry.used
    }
    when(usedNeedValid) {
      used(io.trainUsedReq.bits.setIdx)(i) := true.B
    }
    when(trainNeedInvalid) {
      valid(trainInvalidSetIdx)(i) := false.B
      used(trainInvalidSetIdx)(i)  := false.B
    }
    when(ifuNeedInvalid) {
      valid(ifuInvalidSetIdx)(i) := false.B
      used(ifuInvalidSetIdx)(i)  := false.B
    }

  }

  counterWriteBuffer.io.enq.valid         := io.trainCounterReq.valid
  counterWriteBuffer.io.enq.bits.setIdx   := io.trainCounterReq.bits.setIdx
  counterWriteBuffer.io.enq.bits.wayMask  := io.trainCounterReq.bits.wayMask
  counterWriteBuffer.io.enq.bits.counters := io.trainCounterReq.bits.counters

  counterSram.io.w.req.valid            := counterWriteBuffer.io.deq.valid && !counterSram.io.r.req.valid
  counterSram.io.w.req.bits.data        := counterWriteBuffer.io.deq.bits.counters
  counterSram.io.w.req.bits.setIdx      := counterWriteBuffer.io.deq.bits.setIdx
  counterSram.io.w.req.bits.waymask.get := counterWriteBuffer.io.deq.bits.wayMask
  counterWriteBuffer.io.deq.ready       := counterSram.io.w.req.ready && !counterSram.io.r.req.valid
//  io.writeReq.ready := writeBuffer.io.write.head.ready

  XSPerfAccumulate("read", PopCount(entrySrams.map(_.io.r.req.fire)))
  XSPerfAccumulate("write", PopCount(entrySrams.map(_.io.w.req.fire)))
  XSPerfAccumulate("entry_write_buffer_full", !entryWriteBuffer.io.write.head.ready)
  XSPerfAccumulate(
    "entry_write_buffer_full_drop_write",
    !entryWriteBuffer.io.write.head.ready && io.writeEntryReq.valid
  )

}
