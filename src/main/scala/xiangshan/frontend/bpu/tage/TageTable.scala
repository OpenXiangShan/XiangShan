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
import xiangshan.frontend.bpu.SaturateCounter
import xiangshan.frontend.bpu.WriteBuffer

class TageTable(val numSets: Int)(implicit p: Parameters) extends TageModule with Helpers {
  class TageTableIO extends TageBundle {
    val readReq:                  Valid[TableReadReq]          = Flipped(Valid(new TableReadReq(numSets)))
    val readResp:                 TableReadResp                = Output(new TableReadResp)
    val writeSetIdx:              UInt                         = Input(UInt(log2Ceil(numSets / NumBanks).W))
    val writeBankMask:            UInt                         = Input(UInt(NumBanks.W))
    val updateReq:                Valid[TableUpdateEntriesReq] = Flipped(Valid(new TableUpdateEntriesReq))
    val needResetUsefulCtr:       Bool                         = Input(Bool())
    val needIncreaseAllocFailCtr: Bool                         = Input(Bool())
    val oldAllocFailCtr:          SaturateCounter              = Input(new SaturateCounter(AllocFailCtrWidth))
    val resetDone:                Bool                         = Output(Bool())
  }
  val io: TageTableIO = IO(new TageTableIO)

  private val entrySram =
    Seq.tabulate(NumBanks, NumWays) { (bankIdx, wayIdx) =>
      Module(new SRAMTemplate(
        new TageEntry,
        set = numSets / NumBanks,
        way = 1,
        singlePort = true,
        shouldReset = true,
        withClockGate = true,
        hasMbist = hasMbist,
        hasSramCtl = hasSramCtl
      )).suggestName(s"tage_entry_sram_bank${bankIdx}_way${wayIdx}")
    }

  // count allocation failure times for each set
  private val allocFailCtrSram =
    Seq.fill(NumBanks)(
      Module(new SRAMTemplate(
        new SaturateCounter(AllocFailCtrWidth),
        set = numSets / NumBanks,
        way = 1,
        singlePort = true,
        shouldReset = true,
        withClockGate = true,
        hasMbist = hasMbist,
        hasSramCtl = hasSramCtl
      ))
    )

  // use a write buffer to store a entrySram write request
  // TODO: add writeBuffer multi port simultaneous writing
  private val entryWriteBuffers =
    Seq.tabulate(NumBanks) { bankIdx =>
      Module(new WriteBuffer(
        new EntrySramWriteReq(numSets),
        WriteBufferSize,
        numPorts = NumWays,
        hasCnt = true
      )).suggestName(s"tage_entry_write_buffer_bank${bankIdx}")
    }
  private val allocFailCtrWriteBuffers =
    Seq.fill(NumBanks)(
      Module(new WriteBuffer(
        new AllocFailCtrSramWriteReq(numSets),
        WriteBufferSize,
        numPorts = 1
      ))
    )

  // read and write srams
  entrySram.zip(allocFailCtrSram).zipWithIndex.foreach {
    case ((bank, allocFailCtr), bankIdx) =>
      bank.foreach { way =>
        way.io.r.req.valid       := io.readReq.valid && io.readReq.bits.bankMask(bankIdx)
        way.io.r.req.bits.setIdx := io.readReq.bits.setIdx
      }
      allocFailCtr.io.r.req.valid       := io.readReq.valid && io.readReq.bits.bankMask(bankIdx)
      allocFailCtr.io.r.req.bits.setIdx := io.readReq.bits.setIdx
  }

  entrySram.zip(entryWriteBuffers).foreach {
    case (ways, buffer) =>
      ways zip buffer.io.read foreach {
        case (way, readPort) =>
          val valid  = readPort.valid && !way.io.r.req.valid
          val setIdx = readPort.bits.setIdx
          val entry  = readPort.bits.entry
          way.io.w.apply(valid, entry, setIdx, 1.U(1.W))
          readPort.ready := way.io.w.req.ready && !way.io.r.req.valid
      }
  }

  allocFailCtrSram.zip(allocFailCtrWriteBuffers).foreach {
    case (allocFailCtr, buffer) =>
      val valid  = buffer.io.read.head.valid && !allocFailCtr.io.r.req.valid
      val setIdx = buffer.io.read.head.bits.setIdx
      val data   = buffer.io.read.head.bits.allocFailCtr
      allocFailCtr.io.w.apply(valid, data, setIdx, 1.U(1.W))
      buffer.io.read.head.ready := allocFailCtr.io.w.req.ready && !allocFailCtr.io.r.req.valid
  }

  // write to write buffer
  private val wayMask:      Vec[Bool]      = WireInit(VecInit(Seq.fill(NumWays)(false.B)))
  private val writeEntries: Vec[TageEntry] = WireInit(VecInit(Seq.fill(NumWays)(0.U.asTypeOf(new TageEntry))))
  writeEntries := io.updateReq.bits.entries
  wayMask      := io.updateReq.bits.wayMask.asBools

  entryWriteBuffers.zip(allocFailCtrWriteBuffers).zip(io.writeBankMask.asBools).foreach {
    case ((wayBuffers, allocFailCtrBuffer), bankEnable) =>
      val writeValid = (io.updateReq.valid) && bankEnable
      // write to allocFailCtrBuffer
      allocFailCtrBuffer.io.write.head.valid       := writeValid
      allocFailCtrBuffer.io.write.head.bits.setIdx := io.writeSetIdx
      allocFailCtrBuffer.io.write.head.bits.allocFailCtr.value := Mux(
        io.needResetUsefulCtr,
        0.U,
        io.oldAllocFailCtr.getIncrease
      )
      wayBuffers.io.write.zip(wayMask).zip(writeEntries).foreach {
        case ((writePort, mask), entry) =>
          writePort.valid       := writeValid && mask
          writePort.bits.setIdx := io.writeSetIdx
          writePort.bits.entry  := entry
      }
  }

  io.resetDone :=
    entrySram.flatten.map(_.io.r.req.ready).reduce(_ && _) && allocFailCtrSram.map(_.io.r.req.ready).reduce(_ && _)

  private val readBankMaskNext = RegEnable(io.readReq.bits.bankMask, io.readReq.valid)
  io.readResp.entries :=
    Mux1H(
      readBankMaskNext,
      entrySram.map(bank => VecInit(bank.map(way => way.io.r.resp.data.head)))
    )
  io.readResp.allocFailCtr := Mux1H(readBankMaskNext, allocFailCtrSram.map(_.io.r.resp.data.head))
}
