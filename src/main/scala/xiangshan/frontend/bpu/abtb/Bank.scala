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

  /* --------------------------------------------------------------------------------------------------------------
     read
     -------------------------------------------------------------------------------------------------------------- */

  sram.io.r.req.valid       := io.readReq.valid
  sram.io.r.req.bits.setIdx := io.readReq.bits.setIdx
  io.readReq.ready          := sram.io.r.req.ready

  io.readResp.entries := sram.io.r.resp.data

  /* --------------------------------------------------------------------------------------------------------------
     write
     -------------------------------------------------------------------------------------------------------------- */

  // single port SRAM can not be written and read at the same time
  // read has higher priority than write
  // we use a write buffer to store the write requests when read and write are both valid
  private val writeBuffer = Module(new Queue(new BankWriteReq, WriteBufferSize, pipe = true, flow = true))

  // writeReq is a ValidIO, it means that the new request will be dropped if the buffer is full
  writeBuffer.io.enq.valid := io.writeReq.valid
  writeBuffer.io.enq.bits  := io.writeReq.bits

  writeBuffer.io.deq.ready := sram.io.w.req.ready && !io.readReq.valid

  private val writeValid   = writeBuffer.io.deq.valid && !io.readReq.valid
  private val writeEntry   = writeBuffer.io.deq.bits.entry
  private val writeSetIdx  = writeBuffer.io.deq.bits.setIdx
  private val writeWayMask = UIntToOH(writeBuffer.io.deq.bits.wayIdx)

  sram.io.w.apply(writeValid, writeEntry, writeSetIdx, writeWayMask)

  // when entry is written to sram, we need to notify takenCounter and replacer
  io.writeResp.valid             := writeBuffer.io.deq.fire
  io.writeResp.bits.needResetCtr := writeBuffer.io.deq.bits.needResetCtr
  io.writeResp.bits.setIdx       := writeSetIdx
  io.writeResp.bits.wayMask      := VecInit(writeWayMask.asBools)

  XSPerfAccumulate("read", sram.io.r.req.fire)
  XSPerfAccumulate("write", sram.io.w.req.fire)
  XSPerfAccumulate("write_buffer_full", !writeBuffer.io.enq.ready)
  XSPerfAccumulate("need_reset_ctr", io.writeResp.valid && io.writeResp.bits.needResetCtr)
}
