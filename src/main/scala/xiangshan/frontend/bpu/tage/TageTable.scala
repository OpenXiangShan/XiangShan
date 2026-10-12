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

class TageTable(
    tableIdx:          Int,
    implicit val info: TageTableInfo // declare info as implicit val to pass it to Bundles / methods like TableReadReq
)(implicit p: Parameters) extends TageModule with TableHelper {
  class TageTableIO extends TageBundle {
    val readReq:  Vec[Valid[TableReadReq]] = Flipped(Vec(NumTableReadPorts, Valid(new TableReadReq)))
    val readResp: Vec[TableReadResp]       = Output(Vec(NumTableReadPorts, new TableReadResp))

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
    val readBankIdx = OHToUInt(readReq.bits.bankMask)
    readReq.valid && (io.usefulResetStart || usefulResetInFlightMask(readBankIdx))
  }

  private val writeBuffers = Seq.tabulate(NumBanks, NumWays) { (bankIdx, wayIdx) =>
    Module(new TageWriteBuffer).suggestName(s"tage_write_buffer_bank${bankIdx}_way${wayIdx}")
  }

  private val predictReadGrants = Wire(Vec(NumBanks, Vec(NumWays, Bool())))

  (0 until NumBanks).foreach { case bankIdx =>
    val predictRead = io.readReq(0).valid && io.readReq(0).bits.bankMask(bankIdx)
    val trainRead   = io.readReq(1).valid && io.readReq(1).bits.bankMask(bankIdx)
    assert(
      !(predictRead && trainRead),
      s"tage_table_${tableIdx} bank_${bankIdx}: prediction read and training read use the same bank"
    )
    val readSetIdx = Mux(trainRead, io.readReq(1).bits.setIdx, io.readReq(0).bits.setIdx)

    (0 until NumWays).foreach { case wayIdx =>
      val full             = writeBuffers(bankIdx)(wayIdx).io.full
      val predictReadGrant = predictRead && !full
      val readValid        = trainRead || predictReadGrant
      predictReadGrants(bankIdx)(wayIdx) := predictReadGrant

      val entrySramIO     = entrySram(bankIdx)(wayIdx).io
      val usefulCtrSramIO = usefulCtrSram(bankIdx)(wayIdx).io
      val writerBufferIO  = writeBuffers(bankIdx)(wayIdx).io

      entrySramIO.r.req.valid       := readValid
      entrySramIO.r.req.bits.setIdx := readSetIdx

      usefulCtrSramIO.r.req.valid       := readValid && !io.usefulResetStart && !usefulResetInFlightMask(bankIdx)
      usefulCtrSramIO.r.req.bits.setIdx := readSetIdx

      writerBufferIO.lookupEn(0)     := predictRead
      writerBufferIO.lookupEn(1)     := trainRead
      writerBufferIO.lookupSetIdx(0) := io.readReq(0).bits.setIdx
      writerBufferIO.lookupSetIdx(1) := io.readReq(1).bits.setIdx
    }
  }

  // delay one cycle for better timing
  private val writeReqValid = RegNext(io.writeReq.valid, init = false.B)
  private val writeReq      = RegEnable(io.writeReq.bits, io.writeReq.valid)

  writeBuffers.zipWithIndex.foreach { case (bankBuffers, bankIdx) =>
    bankBuffers.zipWithIndex.foreach { case (buffer, wayIdx) =>
      val bufferIn = buffer.io.write
      bufferIn.valid              := writeReqValid && writeReq.bankMask(bankIdx) && writeReq.wayMask(wayIdx)
      bufferIn.bits.setIdx        := writeReq.setIdx
      bufferIn.bits.entry         := writeReq.entries(wayIdx)
      bufferIn.bits.usefulCtr     := writeReq.usefulCtrs(wayIdx)
      bufferIn.bits.usefulValid   := writeReq.usefulValid(wayIdx)
      bufferIn.bits.writeEntryEn  := writeReq.writeEntryEn(wayIdx)
      bufferIn.bits.writeUsefulEn := writeReq.writeUsefulEn(wayIdx)
      bufferIn.bits.allocate      := writeReq.allocate(wayIdx)
      bufferIn.bits.actualTaken   := writeReq.actualTakenMask(wayIdx)
      buffer.io.resetUseful       := io.usefulResetStart || usefulResetInFlightMask(bankIdx)
    }
  }

  // write entry to sram from write buffer
  entrySram.zip(writeBuffers).zipWithIndex.foreach { case ((bank, bankBuffers), bankIdx) =>
    bank.zip(bankBuffers).zipWithIndex.foreach { case ((way, buffer), wayIdx) =>
      val bufferOut   = buffer.io.entryWrite
      val predictRead = io.readReq(0).valid && io.readReq(0).bits.bankMask(bankIdx)
      val trainRead   = io.readReq(1).valid && io.readReq(1).bits.bankMask(bankIdx)
      val writeGrant  = !trainRead && (!predictRead || buffer.io.full)

      way.io.w.apply(
        bufferOut.valid && writeGrant,
        bufferOut.bits.entry,
        bufferOut.bits.setIdx,
        1.U(1.W) // way mask
      )
      bufferOut.ready := way.io.w.req.ready && writeGrant

      assert(
        !(way.io.r.req.valid && way.io.w.req.valid),
        s"tage_table_${tableIdx}_bank_${bankIdx}_way_${wayIdx}: SRAM read and write in the same cycle"
      )
    }
  }

  private val bypass = Seq.fill(NumTableReadPorts)(Wire(Vec(NumWays, Valid(new TageWriteBufferEntry))))
  private val bypassReg =
    Seq.fill(NumTableReadPorts)(RegInit(0.U.asTypeOf(Vec(NumWays, Valid(new TageWriteBufferEntry)))))

  for (i <- 0 until NumTableReadPorts) {
    bypass(i).indices.foreach { wayIdx =>
      val lookup = writeBuffers.map(_.apply(wayIdx).io.lookup(i))
      bypass(i)(wayIdx).valid := Mux1H(io.readReq(i).bits.bankMask, lookup.map(_.valid))
      bypass(i)(wayIdx).bits  := Mux1H(io.readReq(i).bits.bankMask, lookup.map(_.bits))
    }

    when(io.readReq(i).valid) {
      bypassReg(i) := bypass(i)
    }
  }

  usefulCtrSram.zip(writeBuffers).zipWithIndex.foreach { case ((bank, bankBuffer), bankIdx) =>
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

    bank.zip(bankBuffer).foreach { case (way, wayBuffer) =>
      val usefulResetValid = io.usefulResetStart || usefulResetInFlightMask(bankIdx)
      val usefulWriteValid = wayBuffer.io.usefulWrite.valid && !way.io.r.req.valid && !usefulResetValid
      way.io.w.apply(
        usefulResetValid || usefulWriteValid,
        Mux(usefulResetValid, UsefulCounter.Zero, wayBuffer.io.usefulWrite.bits.usefulCtr),
        Mux(usefulResetValid, usefulResetSetIdx(bankIdx), wayBuffer.io.usefulWrite.bits.setIdx),
        1.U(1.W) // way mask
      )
      wayBuffer.io.usefulWrite.ready := way.io.w.req.ready && !way.io.r.req.valid && !usefulResetValid
    }
  }

  io.readResp.zipWithIndex.foreach { case (resp, i) =>
    val sramGrant = VecInit((0 until NumWays).map { wayIdx =>
      val grantByBank = VecInit((0 until NumBanks).map { bankIdx =>
        if (i == 1) io.readReq(1).valid && io.readReq(1).bits.bankMask(bankIdx)
        else predictReadGrants(bankIdx)(wayIdx)
      })
      Mux1H(io.readReq(i).bits.bankMask, grantByBank)
    })
    val sramGrantNext             = RegEnable(sramGrant, io.readReq(i).valid)
    val readDuringUsefulResetNext = RegEnable(readDuringUsefulReset(i), io.readReq(i).valid)
    val readBankMaskNext          = RegEnable(io.readReq(i).bits.bankMask, io.readReq(i).valid)
    val readValidNext             = RegNext(io.readReq(i).valid, init = false.B)

    val sramEntries = Mux1H(
      readBankMaskNext,
      entrySram.map(bank => VecInit(bank.map(way => way.io.r.resp.data.head)))
    )
    val sramUsefulCtrs = Mux1H(
      readBankMaskNext,
      usefulCtrSram.map(bank => VecInit(bank.map(way => way.io.r.resp.data.head)))
    )

    (0 until NumWays).foreach { wayIdx =>
      val sramEntry        = sramEntries(wayIdx)
      val bypassEntry      = bypassReg(i)(wayIdx)
      val bypassEntryValid = bypassEntry.valid && bypassEntry.bits.entryValid
      val mergedEntry      = Mux(bypassEntryValid, bypassEntry.bits.entry, sramEntry)

      resp.entries(wayIdx)       := mergedEntry
      resp.entries(wayIdx).valid := mergedEntry.valid && readValidNext && (bypassEntryValid || sramGrantNext(wayIdx))

      val bypassUsefulValid = bypassEntryValid && bypassEntry.bits.usefulValid
      val sramUsefulValid   = sramGrantNext(wayIdx) && !bypassEntryValid

      resp.usefulCtrs(wayIdx) := MuxCase(
        UsefulCounter.Zero,
        Seq(
          readDuringUsefulResetNext -> UsefulCounter.Zero,
          bypassUsefulValid         -> bypassEntry.bits.usefulCtr,
          sramUsefulValid           -> sramUsefulCtrs(wayIdx)
        )
      )

      XSPerfAccumulate(
        s"${if (i == 0) "predict" else "train"}_useful_bypass_way${wayIdx}",
        readValidNext && !readDuringUsefulResetNext && bypassUsefulValid
      )
    }
  }

  io.sramResetDone := (
    entrySram.flatten.map(_.io.resetDone) ++ usefulCtrSram.flatten.map(_.io.resetDone)
  ).reduce(_ && _)

  XSPerfAccumulate("predict_read", io.readReq(0).valid)
  XSPerfAccumulate("train_read", io.readReq(1).valid)
  XSPerfAccumulate("write", io.writeReq.valid)
  XSPerfAccumulate(
    s"tage_write_entry_${tableIdx}",
    Mux(io.writeReq.valid, PopCount(io.writeReq.bits.writeEntryEn), 0.U)
  )
  XSPerfAccumulate(
    s"tage_write_useful_${tableIdx}",
    Mux(io.writeReq.valid, PopCount(io.writeReq.bits.writeUsefulEn), 0.U)
  )
  XSPerfAccumulate(s"tage_write_total_${tableIdx}", Mux(io.writeReq.valid, PopCount(io.writeReq.bits.wayMask), 0.U))
  XSPerfAccumulate(
    "write_buffer_full_drop",
    PopCount(writeBuffers.flatten.map(_.io.dropped))
  )
  XSPerfAccumulate("predict_read_bypass", PopCount(bypass(0).map(_.valid)))
  XSPerfAccumulate("train_read_bypass", PopCount(bypass(1).map(_.valid)))
}
