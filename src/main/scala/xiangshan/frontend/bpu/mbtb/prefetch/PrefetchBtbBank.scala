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
import chisel3.util.DecoupledIO
import org.chipsalliance.cde.config.Parameters
import utility.XSPerfAccumulate
import utility.sram.SplittedSRAMTemplate
import xiangshan.frontend.bpu.WriteBuffer

class PrefetchBtbBank(bandIdx: Int)(implicit p: Parameters) extends PrefetchBtbModule {
  class BankIO(implicit p: Parameters) extends PrefetchBtbBundle {
    val readReq:  DecoupledIO[BankReadReq]  = Flipped(Decoupled(new BankReadReq))
    val readResp: BankReadResp              = Output(new BankReadResp)
    val writeReq: DecoupledIO[BankWriteReq] = Flipped(DecoupledIO(new BankWriteReq))
  }
  val io: BankIO = IO(new BankIO)

  private val sram = Module(new SplittedSRAMTemplate(
    new PrefetchBtbSramEntry,
    set = NumSets,
    way = NumWay,
    waySplit = NumWay / 2,
    dataSplit = 1,
    shouldReset = true,
    singlePort = true,
    withClockGate = true,
    holdRead = true,
    hasMbist = hasMbist,
    hasSramCtl = hasSramCtl,
    suffix = Option("bpu_prefetchbtb")
  ))
  private val valid  = RegInit(VecInit(Seq.fill(NumSets)(VecInit(Seq.fill(NumWay)(false.B)))))
  private val victim = RegInit(VecInit(Seq.fill(NumSets)(VecInit(Seq.fill(NumWay)(false.B)))))
  /* --------------------------------------------------------------------------------------------------------------
     read
     -------------------------------------------------------------------------------------------------------------- */
  private val sramRData   = sram.io.r.resp.data
  private val entryValid  = valid(io.readReq.bits.setIdx)
  private val victimValid = victim(io.readReq.bits.setIdx)
  sram.io.r.apply(
    valid = io.readReq.valid,
    setIdx = io.readReq.bits.setIdx
  )
  io.readReq.ready := sram.io.r.req.ready
  for (i <- 0 until NumWay) {
    io.readResp.entries(i).sramData := sramRData(i)
    io.readResp.entries(i).valid    := RegEnable(entryValid(i), io.readReq.fire)
    io.readResp.entries(i).victim   := RegEnable(victimValid(i), io.readReq.fire)
  }

  /* --------------------------------------------------------------------------------------------------------------
     write
     -------------------------------------------------------------------------------------------------------------- */

  private val writeBuffer = Module(new WriteBuffer(
    new BankWriteReq,
    WriteBufferSize,
    numPorts = 1,
    nameSuffix = s"prefetchbtbBank$bandIdx"
  ))

  // writeReq is a ValidIO, it means that the new request will be dropped if the buffer is full
  writeBuffer.io.write.head.valid := io.writeReq.valid
  writeBuffer.io.write.head.bits  := io.writeReq.bits

  writeBuffer.io.read.head.ready := sram.io.w.req.ready && !io.readReq.valid

  private val writeValid   = writeBuffer.io.read.head.valid && !io.readReq.valid
  private val writeEntry   = writeBuffer.io.read.head.bits.entry
  private val writeSetIdx  = writeBuffer.io.read.head.bits.setIdx
  private val writeWayMask = writeBuffer.io.read.head.bits.wayMask

  sram.io.w.apply(
    valid = writeValid,
    data = writeEntry.sramData,
    setIdx = writeSetIdx,
    waymask = writeWayMask
  )
  for (i <- 0 until NumWay) {
    val needWrite = writeValid && writeWayMask(i).asBool
    when(needWrite) {
      valid(writeSetIdx)(i)  := writeEntry.valid(i)
      victim(writeSetIdx)(i) := writeEntry.victim(i)
    }
  }
  io.writeReq.ready := writeBuffer.io.write.head.ready

//  XSPerfAccumulate("read", sram.io.r.req.fire)
//  XSPerfAccumulate("write", sram.io.w.req.fire)
//  XSPerfAccumulate("write_buffer_full", !writeBuffer.io.write.head.ready)
//  XSPerfAccumulate("write_buffer_full_drop_write", !writeBuffer.io.write.head.ready && io.writeReq.valid)
//  XSPerfAccumulate("need_reset_ctr", io.writeResp.valid && io.writeResp.bits.needResetCtr)
}
