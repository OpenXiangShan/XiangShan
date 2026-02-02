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

class PrefetchBtbBank(bandIdx: Int)(implicit p: Parameters) extends PrefetchBtbModule {
  class BankIO(implicit p: Parameters) extends PrefetchBtbBundle {
    val readReq:       ValidIO[BankReadReq]   = Flipped(Valid(new BankReadReq))
    val readResp:      BankReadResp           = Output(new BankReadResp)
    val writeReq:      ValidIO[BankWriteReq]  = Flipped(Valid(new BankWriteReq))
    val TrainWriteReq: ValidIO[TrainWriteReq] = Flipped(Valid(new TrainWriteReq))
    val resetDone:     Bool                   = Output(Bool())
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
  private val valid  = RegInit(VecInit(Seq.fill(NumSets)(VecInit(Seq.fill(NumWay)(false.B)))))
  private val victim = RegInit(VecInit(Seq.fill(NumSets)(VecInit(Seq.fill(NumWay)(false.B)))))

  private val resetDone = RegInit(false.B)
  when(entrySrams.map(_.io.r.req.ready).reduce(_ && _)) {
    resetDone := true.B
  }
  io.resetDone := resetDone
  /* --------------------------------------------------------------------------------------------------------------
     read
     -------------------------------------------------------------------------------------------------------------- */
  private val entryValid  = valid(io.readReq.bits.setIdx)
  private val victimValid = victim(io.readReq.bits.setIdx)
  entrySrams.foreach { sram =>
    sram.io.r.req.valid       := io.readReq.valid
    sram.io.r.req.bits.setIdx := io.readReq.bits.setIdx
  }

  for (i <- 0 until NumWay) {
    io.readResp.entries(i).sramData := entrySrams(i).io.r.resp.data.head
    io.readResp.entries(i).valid    := RegEnable(entryValid(i), io.readReq.fire)
    io.readResp.entries(i).victim   := RegEnable(victimValid(i), io.readReq.fire)
  }

  /* --------------------------------------------------------------------------------------------------------------
     write
     -------------------------------------------------------------------------------------------------------------- */

  private val writeBuffer = Module(new WriteBuffer(
    new WbufWriteReq,
    WriteBufferSize,
    numPorts = NumWay,
    nameSuffix = s"prefetchbtbBank$bandIdx"
  ))

  // writeReq is a ValidIO, it means that the new request will be dropped if the buffer is full

  writeBuffer.io.write.zipWithIndex.foreach { case (buf, idx) =>
    val wvalid = io.writeReq.valid && io.writeReq.bits.wayMask(idx)
    buf.valid        := wvalid
    buf.bits.setIdx  := io.writeReq.bits.setIdx
    buf.bits.entry   := io.writeReq.bits.entry(idx)
    buf.bits.wayMask := io.writeReq.bits.wayMask
  }

  private val writeValid   = writeBuffer.io.read.head.valid && !io.readReq.valid
  private val writeEntry   = writeBuffer.io.read.head.bits.entry
  private val writeSetIdx  = writeBuffer.io.read.head.bits.setIdx
  private val writeWayMask = writeBuffer.io.read.head.bits.wayMask

  private val invalidSetIdx = io.TrainWriteReq.bits.setIdx
  (entrySrams zip writeBuffer.io.read).foreach { case (way, bufRead) =>
    way.io.w.req.valid        := bufRead.valid && !way.io.r.req.valid
    way.io.w.req.bits.data(0) := bufRead.bits.entry.sramData
    way.io.w.req.bits.setIdx  := bufRead.bits.setIdx
    bufRead.ready             := way.io.w.req.ready && !way.io.r.req.valid
  }
  for (i <- 0 until NumWay) {
    val needWrite   = writeValid && writeWayMask(i).asBool
    val needInvalid = io.TrainWriteReq.valid && io.TrainWriteReq.bits.needInvalid(i)
    when(needInvalid) {
      valid(invalidSetIdx)(i) := false.B
    }
    when(needWrite) {
      valid(writeSetIdx)(i)  := writeEntry.valid
      victim(writeSetIdx)(i) := writeEntry.victim
    }
  }
//  io.writeReq.ready := writeBuffer.io.write.head.ready

  XSPerfAccumulate("read", PopCount(entrySrams.map(_.io.r.req.fire)))
  XSPerfAccumulate("write", PopCount(entrySrams.map(_.io.w.req.fire)))
  XSPerfAccumulate("write_buffer_full", !writeBuffer.io.write.head.ready)
  XSPerfAccumulate("write_buffer_full_drop_write", !writeBuffer.io.write.head.ready && io.writeReq.valid)

}
