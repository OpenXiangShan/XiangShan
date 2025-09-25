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
    val readReq:                  Valid[TableReadReq]     = Flipped(Valid(new TableReadReq(numSets)))
    val readResp:                 TableReadResp           = Output(new TableReadResp)
    val writeSetIdx:              UInt                    = Input(UInt(log2Ceil(numSets / NumBanks).W))
    val writeBankMask:            UInt                    = Input(UInt(NumBanks.W))
    val updateReq:                Valid[TableUpdateReq]   = Flipped(Valid(new TableUpdateReq))
    val allocateReq:              Valid[TableAllocateReq] = Flipped(Valid(new TableAllocateReq))
    val needResetUsefulCtr:       Bool                    = Input(Bool())
    val needIncreaseAllocFailCtr: Bool                    = Input(Bool())
    val oldAllocFailCtr:          SaturateCounter         = Input(new SaturateCounter(AllocFailCtrWidth))
    val resetDone:                Bool                    = Output(Bool())
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
        useBitmask = true,
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

  // use a write buffer to store the write requests when read and write are both valid
  private val writeBuffers =
    Seq.fill(NumBanks)(
      Module(new Queue(
        new TableSramWriteReq(numSets),
        WriteBufferSize,
        pipe = true,
        flow = true
      ))
    )

  // Connect write buffers to SRAMs
  entrySram.zip(writeBuffers).zip(allocFailCtrSram).foreach {
    case ((bank, buffer), allocFailCtr) =>
      val valid                    = buffer.io.deq.valid && !bank.head.io.r.req.valid
      val setIdx                   = buffer.io.deq.bits.setIdx
      val updateReq                = buffer.io.deq.bits.updateReq
      val allocateReq              = buffer.io.deq.bits.allocateReq
      val needResetUsefulCtr       = buffer.io.deq.bits.needResetUsefulCtr
      val needIncreaseAllocFailCtr = buffer.io.deq.bits.needIncreaseAllocFailCtr
      bank.zipWithIndex.foreach {
        case (way, wayIdx) =>
          val needAllocate = allocateReq.valid && allocateReq.bits.wayMask(wayIdx)
          val entry        = Wire(new TageEntry)
          entry.valid := true.B
          entry.tag   := allocateReq.bits.tag
          entry.takenCtr.value :=
            Mux(
              needAllocate,
              allocateReq.bits.takenCtr.value,
              updateReq.bits.newTakenCtr(wayIdx).value
            )
          entry.usefulCtr.value :=
            Mux(
              needAllocate,
              UsefulCtrInitValue.U,
              Mux(
                needResetUsefulCtr,
                0.U,
                updateReq.bits.newUsefulCtr(wayIdx).value
              )
            )
          val bitMask =
            Mux(
              needAllocate,
              Fill(TageEntryWidth, 1.U(1.W)),
              Mux(
                needResetUsefulCtr,
                (1 << UsefulCtrWidth - 1).U(TageEntryWidth.W),
                (1 << (TakenCtrWidth + UsefulCtrWidth) - 1).U(TageEntryWidth.W)
              )
            )
          way.io.w.apply(valid, entry, setIdx, 1.U(1.W), bitMask.asUInt)
      }
      buffer.io.deq.ready := bank.head.io.w.req.ready && !bank.head.io.r.req.valid

      allocFailCtr.io.w.req.valid :=
        buffer.io.deq.valid && !bank.head.io.r.req.valid && (needResetUsefulCtr || needIncreaseAllocFailCtr)
      allocFailCtr.io.w.req.bits.setIdx := setIdx
      allocFailCtr.io.w.req.bits.waymask.foreach(_ := 1.U(1.W))
      allocFailCtr.io.w.req.bits.data.head.value := Mux(
        needResetUsefulCtr,
        0.U,
        buffer.io.deq.bits.oldAllocFailCtr.getIncrease
      )
  }

  entrySram.zip(allocFailCtrSram).zipWithIndex.foreach {
    case ((bank, allocFailCtr), bankIdx) =>
      bank.foreach { way =>
        way.io.r.req.valid       := io.readReq.valid && io.readReq.bits.bankMask(bankIdx)
        way.io.r.req.bits.setIdx := io.readReq.bits.setIdx
      }
      allocFailCtr.io.r.req.valid       := io.readReq.valid && io.readReq.bits.bankMask(bankIdx)
      allocFailCtr.io.r.req.bits.setIdx := io.readReq.bits.setIdx
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

  writeBuffers.zipWithIndex.foreach {
    case (buffer, bankIdx) =>
      buffer.io.enq.valid                   := (io.updateReq.valid || io.allocateReq.valid) && io.writeBankMask(bankIdx)
      buffer.io.enq.bits.setIdx             := io.writeSetIdx
      buffer.io.enq.bits.updateReq          := io.updateReq
      buffer.io.enq.bits.allocateReq        := io.allocateReq
      buffer.io.enq.bits.needResetUsefulCtr := io.needResetUsefulCtr
      buffer.io.enq.bits.needIncreaseAllocFailCtr := io.needIncreaseAllocFailCtr
      buffer.io.enq.bits.oldAllocFailCtr          := io.oldAllocFailCtr
  }
}
