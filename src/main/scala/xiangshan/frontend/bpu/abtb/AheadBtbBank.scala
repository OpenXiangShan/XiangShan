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
import utility.sram.SplittedSRAMTemplate
import xiangshan.frontend.bpu.WriteBuffer

/**
  * This module stores the ahead BTB entries.
  */
class AheadBtbBank(bandIdx: Int)(implicit p: Parameters) extends AheadBtbModule {
  class BankIO(implicit p: Parameters) extends AheadBtbBundle {
    val readReq:   DecoupledIO[BankReadReq] = Flipped(Decoupled(new BankReadReq))
    val readResp:  BankReadResp             = Output(new BankReadResp)
    val writeReq:  Valid[BankWriteReq]      = Flipped(Valid(new BankWriteReq))
    val writeResp: Valid[BankWriteResp]     = Valid(new BankWriteResp)
  }
  val io: BankIO = IO(new BankIO)

  private val sram = Module(new SplittedSRAMTemplate(
    new AheadBtbEntry,
    set = NumSets,
    way = NumWays,
    waySplit = NumWays / 2,
    dataSplit = 1,
    shouldReset = true,
    singlePort = true,
    withClockGate = true,
    holdRead = true,
    hasMbist = hasMbist,
    hasSramCtl = hasSramCtl,
    suffix = Option("bpu_abtb")
  ))
  /* --------------------------------------------------------------------------------------------------------------
     read
     -------------------------------------------------------------------------------------------------------------- */

  sram.io.r.apply(
    valid = io.readReq.valid,
    setIdx = io.readReq.bits.setIdx
  )
  io.readReq.ready := sram.io.r.req.ready

  io.readResp.entries := sram.io.r.resp.data

  /* --------------------------------------------------------------------------------------------------------------
     write
     -------------------------------------------------------------------------------------------------------------- */

  // single port SRAM can not be written and read at the same time
  // read has higher priority than write
  // we use a write buffer to store the write requests when read and write are both valid
  private val writeBuffer = Module(new WriteBuffer(
    new BankWriteReq,
    WriteBufferSize,
    numPorts = 1,
    nameSuffix = s"abtbBank$bandIdx"
  ))

  // writeReq is a ValidIO, it means that the new request will be dropped if the buffer is full
  writeBuffer.io.write.head.valid := io.writeReq.valid
  writeBuffer.io.write.head.bits  := io.writeReq.bits

  writeBuffer.io.read.head.ready := sram.io.w.req.ready && !io.readReq.valid

  private val writeValid   = writeBuffer.io.read.head.valid && !io.readReq.valid
  private val writeEntry   = writeBuffer.io.read.head.bits.entry
  private val writeSetIdx  = writeBuffer.io.read.head.bits.setIdx
  private val writeWayMask = UIntToOH(writeBuffer.io.read.head.bits.wayIdx)

  sram.io.w.apply(
    valid = writeValid,
    data = writeEntry,
    setIdx = writeSetIdx,
    waymask = writeWayMask
  )
  // when entry is written to sram, we need to notify takenCounter and replacer
  io.writeResp.valid             := writeBuffer.io.read.head.fire
  io.writeResp.bits.needResetCtr := writeBuffer.io.read.head.bits.needResetCtr
  io.writeResp.bits.setIdx       := writeBuffer.io.read.head.bits.setIdx
  io.writeResp.bits.wayIdx       := writeBuffer.io.read.head.bits.wayIdx

  XSPerfAccumulate("read", sram.io.r.req.fire)
  XSPerfAccumulate("write", sram.io.w.req.fire)
  XSPerfAccumulate("write_buffer_full", !writeBuffer.io.write.head.ready)
  XSPerfAccumulate("need_reset_ctr", io.writeResp.valid && io.writeResp.bits.needResetCtr)
}
