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
import utility.XSPerfAccumulate
import utility.sram.SRAMTemplate
import xiangshan.frontend.bpu.SaturateCounter
import xiangshan.frontend.bpu.TageTableInfo
import xiangshan.frontend.bpu.WriteBuffer

class TageTable(
    tableIdx:          Int,
    implicit val info: TageTableInfo // declare info as implicit val to pass it to Bundles / methods like TableReadReq
)(implicit p: Parameters) extends TageModule with TableHelper {
  class TageTableIO extends TageBundle {
    val predictReadReq:  Valid[TableReadReq]  = Flipped(Valid(new TableReadReq))
    val trainReadReq:    Valid[TableReadReq]  = Flipped(Valid(new TableReadReq))
    val predictReadResp: TableReadResp        = Output(new TableReadResp)
    val trainReadResp:   TableReadResp        = Output(new TableReadResp)
    val writeReq:        Valid[TableWriteReq] = Flipped(Valid(new TableWriteReq))
    val resetUseful:     Bool                 = Input(Bool())
    val resetDone:       Bool                 = Output(Bool())
  }

  val io: TageTableIO = IO(new TageTableIO)

  println(f"TageTable[$tableIdx]:")
  println(f"  Size(set, bank, way): $NumSets * $NumBanks * $NumWays = ${info.Size}")
  println(f"  History length: ${info.HistoryLength}")
  println(f"  Address fields:")
  addrFields.show(indent = 4)

  private val entrySram =
    Seq.tabulate(NumBanks, NumWays) { (bankIdx, wayIdx) =>
      Module(new SRAMTemplate(
        new TageEntry,
        set = NumSets,
        way = 1,
        singlePort = true,
        shouldReset = true,
        withClockGate = true,
        hasMbist = hasMbist,
        hasSramCtl = hasSramCtl,
        suffix = Option("bpu_tage")
      )).suggestName(s"tage_entry_sram_bank${bankIdx}_way${wayIdx}")
    }

  // TODO: use SRAM to implement it
  private val usefulCtrs = RegInit(
    VecInit.fill(NumBanks)(
      VecInit.fill(NumWays)(
        VecInit.fill(NumSets)(
          UsefulCounter.Zero
        )
      )
    )
  )

  // use a write buffer to store a entrySram write request
  // TODO: add writeBuffer multi port simultaneous writing
  private val entryWriteBuffers =
    Seq.tabulate(NumBanks) { bankIdx =>
      Module(new WriteBuffer(
        new EntrySramWriteReq,
        WriteBufferSize,
        numPorts = NumWays,
        hasCnt = false, // FIXME: set to true when bug fixed
        nameSuffix = s"tageTable${tableIdx}_${bankIdx}"
      )).suggestName(s"tage_entry_write_buffer_bank${bankIdx}")
    }

  // read sram
  entrySram.zipWithIndex.foreach { case (bank, bankIdx) =>
    val predictReadValid = io.predictReadReq.valid && io.predictReadReq.bits.bankMask(bankIdx)
    val trainReadValid   = io.trainReadReq.valid && io.trainReadReq.bits.bankMask(bankIdx)
    bank.foreach { way =>
      way.io.r.req.valid       := predictReadValid || trainReadValid
      way.io.r.req.bits.setIdx := Mux(predictReadValid, io.predictReadReq.bits.setIdx, io.trainReadReq.bits.setIdx)
    }
    assert(!(predictReadValid && trainReadValid), s"read conflict in tage_table_${tableIdx}_bank_${bankIdx}")
  }

  // delay one cycle for better timing
  private val writeReqValid = RegNext(io.writeReq.valid, false.B)
  private val writeReq      = RegEnable(io.writeReq.bits, io.writeReq.valid)

  // write to write buffer
  entryWriteBuffers.zipWithIndex.foreach { case (buffer, bankIdx) =>
    buffer.io.write.zipWithIndex.foreach { case (writePort, wayIdx) =>
      writePort.valid          := writeReqValid && writeReq.bankMask(bankIdx) && writeReq.wayMask(wayIdx)
      writePort.bits.setIdx    := writeReq.setIdx
      writePort.bits.entry     := writeReq.entries(wayIdx)
      writePort.bits.usefulCtr := writeReq.usefulCtrs(wayIdx)
    }
  }

  // write to sram from write buffer
  entrySram.zip(usefulCtrs).zip(entryWriteBuffers) foreach { case ((bank, ctrsPerBank), buffer) =>
    bank.zip(ctrsPerBank).zip(buffer.io.read).foreach { case ((way, ctrsPerWay), readPort) =>
      val valid  = readPort.valid && !way.io.r.req.valid
      val setIdx = readPort.bits.setIdx
      val entry  = readPort.bits.entry
      way.io.w.apply(valid, entry, setIdx, 1.U(1.W))
      readPort.ready := way.io.w.req.ready && !way.io.r.req.valid

      when(io.resetUseful) {
        ctrsPerWay.foreach(_.resetZero())
      }.elsewhen(readPort.fire) {
        ctrsPerWay(setIdx) := readPort.bits.usefulCtr
      }
    }
  }

  private val predictReadSetIdxNext   = RegEnable(io.predictReadReq.bits.setIdx, io.predictReadReq.valid)
  private val predictReadBankMaskNext = RegEnable(io.predictReadReq.bits.bankMask, io.predictReadReq.valid)
  io.predictReadResp.entries := Mux1H(
    predictReadBankMaskNext,
    entrySram.map(bank => VecInit(bank.map(way => way.io.r.resp.data.head)))
  )
  io.predictReadResp.usefulCtrs := Mux1H(
    predictReadBankMaskNext,
    usefulCtrs.map(ctrsPerBank =>
      VecInit(ctrsPerBank.map(ctrsPerWay => ctrsPerWay(predictReadSetIdxNext)))
    )
  )

  private val trainReadSetIdxNext   = RegEnable(io.trainReadReq.bits.setIdx, io.trainReadReq.valid)
  private val trainReadBankMaskNext = RegEnable(io.trainReadReq.bits.bankMask, io.trainReadReq.valid)
  io.trainReadResp.entries := Mux1H(
    trainReadBankMaskNext,
    entrySram.map(bank => VecInit(bank.map(way => way.io.r.resp.data.head)))
  )
  io.trainReadResp.usefulCtrs := Mux1H(
    trainReadBankMaskNext,
    usefulCtrs.map(ctrsPerBank =>
      VecInit(ctrsPerBank.map(ctrsPerWay => ctrsPerWay(trainReadSetIdxNext)))
    )
  )

  io.resetDone := entrySram.flatten.map(_.io.r.req.ready).reduce(_ && _)

  XSPerfAccumulate("predict_read", io.predictReadReq.valid)
  XSPerfAccumulate("train_read", io.trainReadReq.valid)
  XSPerfAccumulate("write", io.writeReq.valid)
  XSPerfAccumulate(
    "drop_write",
    PopCount(entryWriteBuffers.flatMap(writePorts => writePorts.io.write.map(p => p.valid && !p.ready)))
  )
}
