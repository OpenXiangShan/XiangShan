// Copyright (c) 2024-2025 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2025 Institute of Computing Technology, Chinese Academy of Sciences
// Copyright (c) 2020-2021 Peng Cheng Laboratory
//
// XiangShan is licensed under Mulan PSL v2.
// You can use this software according to the terms and conditions of the Mulan PSL v2.
// You may obtain a copy of Mulan PSL v2 at: https://license.coscl.org.cn/MulanPSL2
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
import utility.sram.SRAMTemplate
import xiangshan.XSBundle
import xiangshan.XSModule

class AbtbBankReadReq(implicit p: Parameters) extends XSBundle with HasAheadBtbParams {
  val setIdx = UInt(SetIndexLen.W)
}

class AbtbBankReadResp(implicit p: Parameters) extends XSBundle with HasAheadBtbParams {
  val entries = Vec(NumWays, new AheadBtbEntry)
}

class AbtbBankWriteReq(implicit p: Parameters) extends XSBundle with HasAheadBtbParams {
  val isNewEntry  = Bool()
  val setIdx      = UInt(SetIndexLen.W)
  val writeWayIdx = UInt(WayIdxLen.W)
  val entry       = new AheadBtbEntry
}

class AbtbBankWriteResp(implicit p: Parameters) extends XSBundle with HasAheadBtbParams {
  val writeWayIdx = UInt(log2Ceil(NumWays).W)
}

/**
  * This module stores the ahead BTB entries.
  */
class AheadBtbBank(implicit p: Parameters) extends XSModule with HasAheadBtbParams {
  val io = IO(new Bundle {
    val readReq    = Flipped(Decoupled(new AbtbBankReadReq))
    val readResp   = Output(new AbtbBankReadResp)
    val readWayIdx = Input(UInt(log2Ceil(NumWays).W))
    val writeReq   = Flipped(Valid(new AbtbBankWriteReq))
    val writeResp  = Output(new AbtbBankWriteResp)
  })

  val aBtbSram = Module(new SRAMTemplate(
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
  val writeBuffer = Module(new Queue(new AbtbBankWriteReq, WriteBufferSize, pipe = true, flow = true))

  // writeReq is a ValidIO, it means that the new request will be dropped if the buffer is full
  writeBuffer.io.enq.valid := io.writeReq.valid
  writeBuffer.io.enq.bits  := io.writeReq.bits

  writeBuffer.io.deq.ready := aBtbSram.io.w.req.ready

  val writeValid  = writeBuffer.io.deq.valid && !io.readReq.valid
  val writeEntry  = writeBuffer.io.deq.bits.entry
  val writeSetIdx = writeBuffer.io.deq.bits.setIdx

  val replacer = ReplacementPolicy.fromString(Some("setplru"), NumWays, NumSets)

  // get a way to write from replacement policy
  val writeWayIdx =
    Mux(writeBuffer.io.deq.bits.isNewEntry, replacer.way(writeSetIdx), writeBuffer.io.deq.bits.writeWayIdx)

  val writeWayMask = UIntToOH(writeWayIdx)
  aBtbSram.io.w.apply(writeValid, writeEntry, writeSetIdx, writeWayMask)

  io.writeResp.writeWayIdx := writeWayIdx

  // record the setIdx and wayIdx for replacement policy
  val writeFire    = aBtbSram.io.w.req.fire
  val accessSetIdx = Mux(writeFire, writeSetIdx, DelayN(io.readReq.bits.setIdx, 2))
  val accessWayIdx = Mux(writeFire, writeWayIdx, io.readWayIdx)
  replacer.access(accessSetIdx, accessWayIdx)
}
