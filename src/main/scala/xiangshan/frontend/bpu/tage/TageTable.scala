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
import utility.sram.FoldedSRAMTemplate
import utility.sram.SRAMTemplate
import xiangshan.frontend.bpu.TageTableInfo
import xiangshan.frontend.bpu.WriteBuffer

class TageTable(
    tableIdx:          Int,
    implicit val info: TageTableInfo // declare info as implicit val to pass it to Bundles / methods like TableReadReq
)(implicit p: Parameters) extends TageModule with TableHelper {
  class TageTableIO extends TageBundle {
    val readReq:  Vec[Valid[TableReadReq]] = Flipped(Vec(2, Valid(new TableReadReq)))
    val readResp: Vec[TableReadResp]       = Output(Vec(2, new TableReadResp))

    val writeReq: Valid[TableWriteReq] = Flipped(Valid(new TableWriteReq))

    val usefulResetStart:    Bool = Input(Bool())
    val usefulResetInFlight: Bool = Output(Bool())

    val sramResetDone: Bool = Output(Bool())
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
        suffix = Option("bpu_tage_entry")
      )).suggestName(s"tage_entry_sram_bank${bankIdx}_way${wayIdx}")
    }

  // Folding multiple ways of an SRAM with too small a data width together results in better area efficiency.
  private val usefulCtrSram =
    Seq.tabulate(NumBanks, NumWays) { (bankIdx, wayIdx) =>
      Module(new FoldedSRAMTemplate(
        UsefulCounter(),
        set = NumSets,
        width = NumUsefulCtrSramFolds,
        way = 1,
        singlePort = true,
        shouldReset = true,
        withClockGate = true,
        hasMbist = hasMbist,
        hasSramCtl = hasSramCtl,
        suffix = Option("bpu_tage_useful")
      )).suggestName(s"tage_useful_sram_bank${bankIdx}_way${wayIdx}")
    }

  private val usefulResetSetIdx       = RegInit(VecInit.fill(NumBanks)(0.U(SetIdxWidth.W)))
  private val usefulResetInFlightMask = RegInit(VecInit.fill(NumBanks)(false.B))

  io.usefulResetInFlight := usefulResetInFlightMask.reduce(_ || _)

  private val readDuringUsefulReset = io.readReq.map { readReq =>
    readReq.valid && (io.usefulResetStart || (readReq.bits.bankMask & usefulResetInFlightMask.asUInt).orR)
  }

  // Cache the entry and its useful counter together for prediction and training bypass.
  private val writeBuffers =
    Seq.tabulate(NumBanks, NumWays) { (bankIdx, wayIdx) =>
      Module(new WriteBuffer(
        new TageWriteReq,
        WriteBufferSize,
        numPorts = 1,
        hasCnt = true,
        hasLookup = true,
        hasUsefulCnt = true,
        nameSuffix = s"tage_table_${tableIdx}_${bankIdx}_way${wayIdx}"
      ))
    }

  // read
  entrySram.zip(usefulCtrSram).zipWithIndex.foreach { case ((entryBank, usefulBank), bankIdx) =>
    val readValid = VecInit(
      io.readReq(0).valid && io.readReq(0).bits.bankMask(bankIdx),
      io.readReq(1).valid && io.readReq(1).bits.bankMask(bankIdx)
    )
    val readSetIdx = Mux(readValid(0), io.readReq(0).bits.setIdx, io.readReq(1).bits.setIdx)

    entryBank.foreach { way =>
      way.io.r.req.valid       := readValid.reduce(_ || _)
      way.io.r.req.bits.setIdx := readSetIdx
    }
    usefulBank.foreach { way =>
      way.io.r.req.valid       := readValid.reduce(_ || _) && !usefulResetInFlightMask(bankIdx)
      way.io.r.req.bits.setIdx := readSetIdx
    }

    // read write buffer
    writeBuffers(bankIdx).foreach(buffer => buffer.io.lookupSetIdx.get := readSetIdx)

    assert(!(readValid(0) && readValid(1)), s"read conflict in tage_table_${tableIdx}_bank_${bankIdx}")
  }

  // delay one cycle for better timing
  private val writeReqValid = RegNext(io.writeReq.valid, init = false.B)
  private val writeReq      = RegEnable(io.writeReq.bits, io.writeReq.valid)

  // write to write buffer
  writeBuffers.zipWithIndex.foreach { case (bankBuffers, bankIdx) =>
    bankBuffers.zipWithIndex.foreach { case (buffer, wayIdx) =>
      val writeValid =
        writeReqValid && writeReq.bankMask(bankIdx) && writeReq.wayMask(wayIdx)
      buffer.io.write.head.valid          := writeValid
      buffer.io.write.head.bits.setIdx    := writeReq.setIdx
      buffer.io.write.head.bits.data      := writeReq.entries(wayIdx)
      buffer.io.write.head.bits.incUseful := writeReq.incUsefulMask(wayIdx)
      buffer.io.write.head.bits.allocate  := writeReq.allocateMask(wayIdx)
      buffer.io.takenMask.get.head        := writeReq.actualTakenMask(wayIdx)
      buffer.io.resetUseful.get           := io.usefulResetStart || usefulResetInFlightMask(bankIdx)
    }
  }

  private val readBankMaskNext = io.readReq.map(req => RegEnable(req.bits.bankMask, req.valid))
  private val readValidNext    = io.readReq.map(req => RegNext(req.valid, init = false.B))

  // WriteBuffer registers the lookup address alongside the SRAM read request. Select
  // its following-cycle result with the matching bank mask, without another data register.
  private val readBypass = io.readReq.indices.map { i =>
    VecInit((0 until NumWays).map { wayIdx =>
      val validByBank = VecInit(writeBuffers.map(_(wayIdx).io.lookup.get.head.valid))
      val entryByBank = writeBuffers.map(_(wayIdx).io.lookup.get.head.bits.data)
      val bypass      = Wire(Valid(new TageEntryWithUseful))
      bypass.valid := readValidNext(i) && Mux1H(readBankMaskNext(i), validByBank)
      bypass.bits  := Mux1H(readBankMaskNext(i), entryByBank)
      bypass
    })
  }

  usefulCtrSram.zipWithIndex.foreach { case (bank, bankIdx) =>
    when(io.usefulResetStart) {
      usefulResetInFlightMask(bankIdx) := true.B
      usefulResetSetIdx(bankIdx)       := 0.U
    }.elsewhen(usefulResetInFlightMask(bankIdx) && bank.head.io.w.req.fire) {
      when(usefulResetSetIdx(bankIdx) === (NumSets - 1).U) {
        usefulResetInFlightMask(bankIdx) := false.B
        usefulResetSetIdx(bankIdx)       := 0.U
      }.otherwise {
        usefulResetSetIdx(bankIdx) := usefulResetSetIdx(bankIdx) + 1.U
      }
    }

    bank.zipWithIndex.foreach { case (way, wayIdx) =>
      val entryWay         = entrySram(bankIdx)(wayIdx)
      val buffer           = writeBuffers(bankIdx)(wayIdx)
      val bufferOut        = buffer.io.read.head
      val entryPending     = buffer.io.entryDirty.get.head
      val usefulPending    = buffer.io.usefulDirty.get.head
      val usefulResetValid = io.usefulResetStart || usefulResetInFlightMask(bankIdx)

      // Complete both pending writes together. An unchanged field does not occupy its SRAM port.
      val entryReady  = !entryPending || (!entryWay.io.r.req.valid && entryWay.io.w.req.ready)
      val usefulReady = !usefulPending || (!way.io.r.req.valid && way.io.w.req.ready && !usefulResetValid)
      bufferOut.ready := entryReady && usefulReady

      val entryWriteValid  = bufferOut.fire && entryPending
      val usefulWriteValid = bufferOut.fire && usefulPending
      entryWay.io.w.apply(
        entryWriteValid,
        bufferOut.bits.data.entry,
        bufferOut.bits.setIdx,
        1.U(1.W)
      )
      way.io.w.apply(
        usefulResetValid || usefulWriteValid,
        Mux(usefulResetValid, UsefulCounter.Zero, bufferOut.bits.data.usefulCtr),
        Mux(usefulResetValid, usefulResetSetIdx(bankIdx), bufferOut.bits.setIdx),
        1.U(1.W) // way mask
      )
      XSPerfAccumulate(s"entry_sram_write_bank${bankIdx}_way${wayIdx}", entryWay.io.w.req.fire)
      XSPerfAccumulate(s"useful_sram_write_bank${bankIdx}_way${wayIdx}", way.io.w.req.fire && !usefulResetValid)
    }
  }

  io.readResp.zipWithIndex.foreach { case (resp, i) =>
    // Keep this aligned with the request: the final reset write can clear the mask before its response.
    val readDuringUsefulResetNext = RegEnable(readDuringUsefulReset(i), io.readReq(i).valid)
    val sramEntries = Mux1H(
      readBankMaskNext(i),
      entrySram.map(bank => VecInit(bank.map(way => way.io.r.resp.data.head)))
    )
    val sramUsefulCtrs = Mux1H(
      readBankMaskNext(i),
      usefulCtrSram.map(bank => VecInit(bank.map(way => way.io.r.resp.data.head)))
    )

    resp.entries := VecInit(sramEntries.zip(readBypass(i)).map { case (sramEntry, bypassEntry) =>
      Mux(bypassEntry.valid, bypassEntry.bits.entry, sramEntry)
    })
    resp.usefulCtrs := Mux(
      readDuringUsefulResetNext,
      VecInit.fill(NumWays)(UsefulCounter.Zero),
      VecInit(sramUsefulCtrs.zip(readBypass(i)).map { case (sramUsefulCtr, bypassEntry) =>
        Mux(bypassEntry.valid, bypassEntry.bits.usefulCtr, sramUsefulCtr)
      })
    )
  }

  io.sramResetDone := (
    entrySram.flatten.map(_.io.resetDone) ++ usefulCtrSram.flatten.map(_.io.resetDone)
  ).reduce(_ && _)

  XSPerfAccumulate("predict_read", io.readReq(0).valid)
  XSPerfAccumulate("train_read", io.readReq(1).valid)
  XSPerfAccumulate("write", io.writeReq.valid)
  XSPerfAccumulate(s"tage_write_total_${tableIdx}", Mux(io.writeReq.valid, PopCount(io.writeReq.bits.wayMask), 0.U))
  XSPerfAccumulate("overwrite", PopCount(writeBuffers.flatMap(_.flatMap(_.io.overwrite))))
  XSPerfAccumulate("predict_read_bypass_hit", PopCount(readBypass(0).map(_.valid)))
  XSPerfAccumulate("train_read_bypass_hit", PopCount(readBypass(1).map(_.valid)))
}
