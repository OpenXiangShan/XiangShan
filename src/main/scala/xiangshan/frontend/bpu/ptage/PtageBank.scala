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

package xiangshan.frontend.bpu.ptage

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.XSPerfAccumulate
import utility.sram.SplittedSRAMTemplate
import xiangshan.frontend.bpu.WriteBuffer

/** One direct-mapped bank of a pTAGE table.
  *
  * Banks exist so that a prediction read and a training write can usually proceed in the same cycle by landing on
  * different banks. When they do collide the read wins, because stalling it would put a bubble in the fetch stream,
  * while a training write can wait: it goes to the write buffer and drains on a later cycle whose bank is idle.
  */
class PtageBank(tableIdx: Int, bankIdx: Int)(implicit p: Parameters) extends PtageModule {
  class BankIO(implicit p: Parameters) extends PtageBundle {
    val readReq:  DecoupledIO[BankReadReq] = Flipped(Decoupled(new BankReadReq))
    val readResp: BankReadResp             = Output(new BankReadResp)
    val writeReq: Valid[BankWriteReq]      = Flipped(Valid(new BankWriteReq))

    val sramResetDone: Bool = Output(Bool())
  }
  val io: BankIO = IO(new BankIO)

  private val sram = Module(new SplittedSRAMTemplate(
    new PtageEntry,
    set = NumSets,
    way = 1, // direct mapped: the history-tagged tables give associativity, a set does not need ways as well
    waySplit = 1,
    dataSplit = 1,
    shouldReset = true,
    singlePort = true,
    withClockGate = true,
    holdRead = true,
    hasMbist = hasMbist,
    hasSramCtl = hasSramCtl,
    suffix = Option("bpu_ptage")
  ))

  /* *** read *** */

  sram.io.r.apply(
    valid = io.readReq.valid,
    setIdx = io.readReq.bits.setIdx
  )
  io.readReq.ready := sram.io.r.req.ready

  io.readResp.entry := sram.io.r.resp.data.head

  io.sramResetDone := sram.io.resetDone

  /* *** write *** */

  private val writeBuffer = Module(new WriteBuffer(
    new BankWriteReq,
    WriteBufferSize,
    numPorts = 1,
    nameSuffix = s"ptageTable${tableIdx}Bank$bankIdx"
  ))

  // The buffer accepts every pulse and drops its oldest entry when it overflows. Training is best effort, so losing a
  // write costs a later re-learn, whereas back-pressuring it would reach into the prediction pipeline.
  writeBuffer.io.write.head.valid := io.writeReq.valid
  writeBuffer.io.write.head.bits  := io.writeReq.bits

  writeBuffer.io.read.head.ready := sram.io.w.req.ready && !io.readReq.valid

  sram.io.w.apply(
    valid = writeBuffer.io.read.head.valid && !io.readReq.valid,
    data = writeBuffer.io.read.head.bits.entry,
    setIdx = writeBuffer.io.read.head.bits.setIdx,
    waymask = 1.U
  )

  XSPerfAccumulate("read", sram.io.r.req.fire)
  XSPerfAccumulate("write", sram.io.w.req.fire)
  XSPerfAccumulate("write_blocked_by_read", writeBuffer.io.read.head.valid && io.readReq.valid)
  XSPerfAccumulate("write_buffer_full", writeBuffer.io.full.head)
  XSPerfAccumulate("write_buffer_overwrite", writeBuffer.io.overwrite.head)
}
