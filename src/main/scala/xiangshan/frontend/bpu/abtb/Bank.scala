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

package xiangshan.frontend.bpu.abtb

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.XSPerfAccumulate
import utility.sram.SRAMTemplate
import xiangshan.frontend.bpu.WriteBuffer
import xiangshan.frontend.bpu.WriteReqBundle

/**
  * This module stores the ahead BTB entries.
  */
class Bank(implicit p: Parameters) extends AheadBtbModule {
  val io: BankIO = IO(new BankIO)

  private val sram = Module(new SRAMTemplate(
    new AheadBtbEntry,
    set = NumSets,
    way = NumWays,
    singlePort = true,
    shouldReset = true,
    withClockGate = true,
    hasMbist = hasMbist,
    hasSramCtl = hasSramCtl
  ))

  // ====================================== read ====================================== //

  sram.io.r.req.valid       := io.readReq.valid
  sram.io.r.req.bits.setIdx := io.readReq.bits.setIdx
  io.readReq.ready          := sram.io.r.req.ready

  io.readResp.entries := sram.io.r.resp.data

  // ===================================== write ======================================= //

  // single port SRAM can not be written and read at the same time
  // read has higher priority than write
  // we use a write buffer to store the write requests when read and write are both valid
  // private val writeBuffer = Module(new Queue(new BankWriteReq, WriteBufferSize, pipe = true, flow = true))
  private val writeBuffer = Module(new WriteBuffer(new BankWriteReq, WriteBufferSize, pipe = true, flow = true))


  // writeReq is a ValidIO, it means that the new request will be dropped if the buffer is full
  writeBuffer.io.enq.valid := io.writeReq.valid
  writeBuffer.io.enq.bits  := io.writeReq.bits

  writeBuffer.io.deq.ready := sram.io.w.req.ready && !io.readReq.valid

  private val writeValid   = writeBuffer.io.deq.valid && !io.readReq.valid
  private val writeEntry   = writeBuffer.io.deq.bits.entry
  private val writeSetIdx  = writeBuffer.io.deq.bits.setIdx
  private val writeWayIdx  = writeBuffer.io.deq.bits.wayIdx
  private val writeWayMask = UIntToOH(writeWayIdx)

  sram.io.w.apply(writeValid, writeEntry, writeSetIdx, writeWayMask)

  io.writeResp.valid       := sram.io.w.req.fire
  io.writeResp.bits.setIdx := writeSetIdx
  io.writeResp.bits.wayIdx := writeWayIdx


  // ===================================== write buffer test ======================================= //

  // private  val writeBufferReq = WireInit(0.U.asTypeOf(new BankWriteReqBundle()))
  // writeBufferReq.setIdx := io.writeReq.bits.setIdx
  // // writeBufferReq.writeTag := io.writeReq.bits.entry.tag
  // writeBufferReq.isNewEntry := io.writeReq.bits.isNewEntry
  // writeBufferReq.wayIdx := io.writeReq.bits.wayIdx
  // writeBufferReq.entry := io.writeReq.bits.entry

  // private val testWriteBuffer = Module(new WriteBuffer(new BankWriteReqBundle, 4))
  // testWriteBuffer.io.enq.valid := io.writeReq.valid  // only write
  // testWriteBuffer.io.enq.bits :=  writeBufferReq  // use the updated entry
  // testWriteBuffer.io.deq.ready := false.B
  // private val deqData = WireInit(0.U.asTypeOf(new BankWriteReqBundle())) // data to be written back to entries
  // when(testWriteBuffer.io.deq.valid && !io.readReq.valid) {
  //   // write back the entry from write buffer
  //   deqData := writeBuffer.io.deq.bits
  //   writeBuffer.io.deq.ready := true.B
  // }
  // dontTouch(deqData) // avoid warning about unused deqData

  XSPerfAccumulate("abtb_bank_read_write_conflict", io.readReq.valid && writeBuffer.io.deq.valid)
  XSPerfAccumulate("abtb_bank_write_buffer_full", !writeBuffer.io.enq.ready)
  XSPerfAccumulate("abtb_bank_read", io.readReq.fire)
  XSPerfAccumulate("abtb_bank_write", writeBuffer.io.deq.fire)
}
