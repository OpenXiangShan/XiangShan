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

package xiangshan.frontend.bpu.tage

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.sram.SRAMTemplate
import xiangshan.frontend.bpu.WriteBuffer

class TageTable(val NumSets: Int, val NumWays: Int)(implicit p: Parameters) extends TageModule with Helpers {
  class TageTableIO extends TageBundle {
    val readReqValid:  Bool           = Input(Bool())
    val readSetIdx:    UInt           = Input(UInt(log2Ceil(NumSets).W))
    val readBankMask:  Vec[Bool]      = Input(Vec(NumBanks, Bool()))
    val readEntries:   Vec[TageEntry] = Output(Vec(NumWays, new TageEntry))
    val writeReqValid: Bool           = Input(Bool())
    val writeSetIdx:   UInt           = Input(UInt(log2Ceil(NumSets).W))
    val writeBankMask: Vec[Bool]      = Input(Vec(NumBanks, Bool()))
    val writeWayMask:  UInt           = Input(UInt(NumWays.W))
    val writeEntry:    TageEntry      = Input(new TageEntry)
    val resetDone:     Bool           = Output(Bool())
  }
  val io: TageTableIO = IO(new TageTableIO)

  private val sramBanks =
    Seq.fill(NumBanks)(
      Module(new SRAMTemplate(
        new TageEntry,
        set = NumSets / NumBanks,
        way = NumWays,
        singlePort = true,
        shouldReset = true,
        withClockGate = true,
        hasMbist = hasMbist,
        hasSramCtl = hasSramCtl
      ))
    )

  // use a write buffer to store the write requests when read and write are both valid
  private val writeBuffers =
    Seq.fill(NumBanks)(
      Module(new WriteBuffer(
        new TableSramWriteReq(NumSets, NumWays),
        WriteBufferSize,
        numPorts = 1,
        pipe = true,
        hasTag = true
      ))
    )

  // Connect write buffers to SRAMs
  sramBanks.zip(writeBuffers).foreach {
    case (bank, buffer) =>
      val writeValid   = buffer.io.read(0).valid && !bank.io.r.req.valid
      val writeEntry   = buffer.io.read(0).bits.entry
      val writeSetIdx  = buffer.io.read(0).bits.setIdx
      val writeWayMask = UIntToOH(buffer.io.read(0).bits.wayIdx, NumWays)
      bank.io.w.apply(writeValid, writeEntry, writeSetIdx, writeWayMask)

      buffer.io.read(0).ready := bank.io.w.req.ready && !bank.io.r.req.valid
  }

  sramBanks.zipWithIndex.foreach {
    case (bank, i) =>
      bank.io.r.req.valid       := io.readReqValid && io.readBankMask(i)
      bank.io.r.req.bits.setIdx := io.readSetIdx
  }

  private val s0_fire = io.readReqValid

  private val s1_bankMask = RegEnable(io.readBankMask, s0_fire)

  io.resetDone := sramBanks.map(_.io.r.req.ready).reduce(_ && _)

  io.readEntries := Mux1H(s1_bankMask, sramBanks.map(_.io.r.resp.data))

  writeBuffers.zipWithIndex.foreach {
    case (buffer, i) =>
      val writeValid  = io.writeReqValid && io.writeBankMask(i)
      val writeWayIdx = PriorityEncoder(io.writeWayMask)
      buffer.io.write(0).valid       := writeValid
      buffer.io.write(0).bits.setIdx := io.writeSetIdx
      buffer.io.write(0).bits.wayIdx := writeWayIdx
      buffer.io.write(0).bits.entry  := io.writeEntry
  }

}
