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

/**
  * This module stores the ahead BTB entries.
  */
class AheadBtbSramBank(implicit p: Parameters) extends AheadBtbModule {
  val io: BankIO = IO(new BankIO)

  private val sramBank = Module(new SRAMTemplate(
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

  sramBank.io.r.req.valid       := io.readReq.valid
  sramBank.io.r.req.bits.setIdx := io.readReq.bits.setIdx
  io.readReq.ready              := sramBank.io.r.req.ready

  io.readResp.entries := sramBank.io.r.resp.data

  /* --------------------------------------------------------------------------------------------------------------
     write
     -------------------------------------------------------------------------------------------------------------- */

  // use a write buffer to store the write requests when read and write are both valid
  private val writeBuffer = Module(new WriteBuffer(new BankWriteReq, WriteBufferSize, numPorts = 1, pipe = true))

  // writeReq is a ValidIO, it means that the new request will be dropped if the buffer is full
  writeBuffer.io.write(0).valid := io.writeReq.valid
  writeBuffer.io.write(0).bits  := io.writeReq.bits

  writeBuffer.io.read(0).ready := sramBank.io.w.req.ready && !io.readReq.valid

  private val writeValid   = writeBuffer.io.read(0).valid && !io.readReq.valid
  private val writeEntry   = writeBuffer.io.read(0).bits.entry
  private val writeSetIdx  = writeBuffer.io.read(0).bits.setIdx
  private val writeWayMask = UIntToOH(writeBuffer.io.read(0).bits.wayIdx, NumWays)

  sramBank.io.w.apply(writeValid, writeEntry, writeSetIdx, writeWayMask)

  // when entry is written to sram, we need to notify takenCounter and replacer
  io.writeResp.valid             := writeBuffer.io.read(0).fire
  io.writeResp.bits.needResetCtr := writeBuffer.io.read(0).bits.needResetCtr
  io.writeResp.bits.setIdx       := writeBuffer.io.read(0).bits.setIdx
  io.writeResp.bits.wayIdx       := writeBuffer.io.read(0).bits.wayIdx

  XSPerfAccumulate("read", sramBank.io.r.req.fire)
  XSPerfAccumulate("write", sramBank.io.w.req.fire)
  XSPerfAccumulate("write_buffer_full", !writeBuffer.io.write(0).ready)
  XSPerfAccumulate("need_reset_ctr", io.writeResp.valid && io.writeResp.bits.needResetCtr)
}
