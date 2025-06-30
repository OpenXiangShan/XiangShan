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
import utility.DelayN
import utility.ReplacementPolicy
import utility.XSPerfAccumulate
import utility.sram.SRAMTemplate
import xiangshan.frontend.bpu.BpuBundle
import xiangshan.frontend.bpu.BpuModule

class AbtbBankReadReq(implicit p: Parameters) extends BpuBundle with HasAheadBtbParameters {
  val setIdx = UInt(SetIndexLen.W)
}

class AbtbBankReadResp(implicit p: Parameters) extends BpuBundle with HasAheadBtbParameters {
  val entries = Vec(NumWays, new AheadBtbEntry)
}

class AbtbBankWriteReq(implicit p: Parameters) extends BpuBundle with HasAheadBtbParameters {
  val isNewEntry  = Bool()
  val setIdx      = UInt(SetIndexLen.W)
  val writeWayIdx = UInt(WayIdxLen.W)
  val entry       = new AheadBtbEntry
}

class AbtbBankWriteResp(implicit p: Parameters) extends BpuBundle with HasAheadBtbParameters {
  val writeWayIdx = UInt(log2Ceil(NumWays).W)
//  private val writeDone = Bool()
}

/**
  * This module stores the ahead BTB entries.
  */
class AheadBtbBank(implicit p: Parameters) extends BpuModule with HasAheadBtbParameters {
  val io = IO(new Bundle {
    val readReq    = Flipped(Decoupled(new AbtbBankReadReq))
    val readResp   = Output(new AbtbBankReadResp)
    val readWayIdx = Input(UInt(log2Ceil(NumWays).W))
    val writeReq   = Flipped(Valid(new AbtbBankWriteReq))
    val writeResp  = Output(new AbtbBankWriteResp)
  })

  private val aBtbSram = Module(new SRAMTemplate(
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

  aBtbSram.io.r.req.valid       := io.readReq.valid
  aBtbSram.io.r.req.bits.setIdx := io.readReq.bits.setIdx
  io.readReq.ready              := aBtbSram.io.r.req.ready

  io.readResp.entries := aBtbSram.io.r.resp.data

  // ===================================== write ======================================= //

  // single port SRAM can not be written and read at the same time
  // read has higher priority than write
  // we use a write buffer to store the write requests when read and write are both valid
  private val writeBuffer = Module(new Queue(new AbtbBankWriteReq, WriteBufferSize, pipe = true, flow = true))

  // writeReq is a ValidIO, it means that the new request will be dropped if the buffer is full
  writeBuffer.io.enq.valid := io.writeReq.valid
  writeBuffer.io.enq.bits  := io.writeReq.bits

  writeBuffer.io.deq.ready := aBtbSram.io.w.req.ready && !io.readReq.valid

  private val writeValid  = writeBuffer.io.deq.valid && !io.readReq.valid
  private val writeEntry  = writeBuffer.io.deq.bits.entry
  private val writeSetIdx = writeBuffer.io.deq.bits.setIdx

  private val replacer = ReplacementPolicy.fromString(Some("setplru"), NumWays, NumSets)

  // get a way to write from replacement policy
  private val writeWayIdx =
    Mux(writeBuffer.io.deq.bits.isNewEntry, replacer.way(writeSetIdx), writeBuffer.io.deq.bits.writeWayIdx)

  private val writeWayMask = UIntToOH(writeWayIdx)
  aBtbSram.io.w.apply(writeValid, writeEntry, writeSetIdx, writeWayMask)

  io.writeResp.writeWayIdx := writeWayIdx

  // record the setIdx and wayIdx for replacement policy
  private val writeFire    = aBtbSram.io.w.req.fire
  private val accessSetIdx = Mux(writeFire, writeSetIdx, DelayN(io.readReq.bits.setIdx, 2))
  private val accessWayIdx = Mux(writeFire, writeWayIdx, io.readWayIdx)
  replacer.access(accessSetIdx, accessWayIdx)

  XSPerfAccumulate("aBtb_bank_read_write_conflict", writeBuffer.io.deq.valid && io.readReq.valid)
}
