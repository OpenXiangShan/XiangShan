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

    val entryWriteBufferFullCount:             UInt = Output(UInt(log2Ceil(NumBanks * NumWays + 1).W))
    val usefulWriteBufferFullCount:            UInt = Output(UInt(log2Ceil(NumBanks * NumWays + 1).W))
    val predictionReadFullWriteCount:          UInt = Output(UInt(log2Ceil(NumBanks * NumWays + 1).W))
    val predictionReadFullWriteBypassHitCount: UInt = Output(UInt(log2Ceil(NumBanks * NumWays + 1).W))
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
    (0 until NumBanks).map { bankIdx =>
      readReq.valid && readReq.bits.bankMask(bankIdx) && usefulResetInFlightMask(bankIdx)
    }.reduce(_ || _)
  }

  // use a write buffer to store a entrySram write request
  private val entryWriteBuffers =
    Seq.tabulate(NumBanks, NumWays) { (bankIdx, wayIdx) =>
      Module(new WriteBuffer(
        new EntrySramWriteReq,
        WriteBufferSize,
        hasCnt = true,
        hasLookup = true,
        numLookupPorts = 2,
        nameSuffix = s"tageTable${tableIdx}_${bankIdx}_way${wayIdx}"
      )).suggestName(s"tage_entry_write_buffer_bank${bankIdx}_way${wayIdx}")
    }

  // use a write buffer to store a usefulCtr write request
  private val usefulCtrWriteBuffers =
    Seq.tabulate(NumBanks, NumWays) { (bankIdx, wayIdx) =>
      Module(new Queue(
        new UsefulCtrSramWriteReq,
        entries = WriteBufferSize,
        pipe = true,
        flow = true,
        hasFlush = true
      )).suggestName(s"tage_useful_write_buffer_bank${bankIdx}_way${wayIdx}")
    }

  // Count full buffers, not blocked enqueues: a full queue can still drain this cycle.
  io.entryWriteBufferFullCount  := PopCount(entryWriteBuffers.flatten.map(_.io.full.head))
  io.usefulWriteBufferFullCount := PopCount(usefulCtrWriteBuffers.flatten.map(_.io.count === WriteBufferSize.U))

  // read sram
  entrySram.zip(usefulCtrSram).zipWithIndex.foreach { case ((entryBank, usefulBank), bankIdx) =>
    val predictionRead = io.readReq(0).valid && io.readReq(0).bits.bankMask(bankIdx)
    val trainingRead   = io.readReq(1).valid && io.readReq(1).bits.bankMask(bankIdx)
    // The single-ported SRAM can only serve one read per bank, so the prediction read and the
    // training read must never ask for the same bank in the same cycle.  When they would, the BPU
    // holds the whole predict stage for one cycle (see Tage) and this block is read again next
    // cycle, which is why the prediction read can no longer be suppressed by a training read here.
    assert(
      !(predictionRead && trainingRead),
      s"tage_table_${tableIdx} bank_${bankIdx}: prediction read and training read use the same bank"
    )
    // Training reads have priority over prediction reads. The SRAM is
    // single-ported, so a simultaneous prediction read is suppressed and the
    // caller uses its write-buffer/MBTB fallback path.
    val readSetIdx = Mux(trainingRead, io.readReq(1).bits.setIdx, io.readReq(0).bits.setIdx)
    entryBank.zipWithIndex.foreach { case (way, wayIdx) =>
      val full                = entryWriteBuffers(bankIdx)(wayIdx).io.full.head
      val predictionReadGrant = predictionRead && !full
      way.io.r.req.valid                := trainingRead || predictionReadGrant
      way.io.r.req.bits.setIdx          := readSetIdx
      usefulBank(wayIdx).io.r.req.valid := (trainingRead || predictionReadGrant) && !usefulResetInFlightMask(bankIdx)
      usefulBank(wayIdx).io.r.req.bits.setIdx := readSetIdx
    }
  }

  // delay one cycle for better timing
  private val writeReqValid = RegNext(io.writeReq.valid, init = false.B)
  private val writeReq      = RegEnable(io.writeReq.bits, io.writeReq.valid)

  private val predictionReadByBank = VecInit((0 until NumBanks).map { bankIdx =>
    io.readReq(0).valid && io.readReq(0).bits.bankMask(bankIdx)
  })

  // A prediction read prevents the single-port SRAM from draining the entry
  // write buffer for the same bank.  Keep the per-bank read history so that
  // an overwrite can be attributed to a continuously reused prediction bank.
  private val previousPredictionReadByBank = RegNext(
    predictionReadByBank,
    VecInit.fill(NumBanks)(false.B)
  )
  private val consecutivePredictionReadByBank = VecInit(
    predictionReadByBank.zip(previousPredictionReadByBank).map { case (current, previous) =>
      current && previous
    }
  )

  // write to write buffer
  entryWriteBuffers.zipWithIndex.foreach { case (bankBuffers, bankIdx) =>
    bankBuffers.zipWithIndex.foreach { case (buffer, wayIdx) =>
      val writeValid =
        writeReqValid && writeReq.bankMask(bankIdx) && writeReq.wayMask(wayIdx) && writeReq.writeEntryEn(wayIdx)
      buffer.io.write.head.valid       := writeValid
      buffer.io.write.head.bits.setIdx := writeReq.setIdx
      buffer.io.write.head.bits.entry  := writeReq.entries(wayIdx)
      buffer.io.takenMask.get.head     := writeReq.actualTakenMask(wayIdx)
      buffer.io.lookupEn.get(0)        := io.readReq(0).valid && io.readReq(0).bits.bankMask(bankIdx)
      buffer.io.lookupEn.get(1)        := io.readReq(1).valid && io.readReq(1).bits.bankMask(bankIdx)
      buffer.io.lookupSetIdx.get(0)    := io.readReq(0).bits.setIdx
      buffer.io.lookupSetIdx.get(1)    := io.readReq(1).bits.setIdx
    }
  }

  usefulCtrWriteBuffers.zipWithIndex.foreach { case (bankBuffer, bankIdx) =>
    bankBuffer.zipWithIndex.foreach { case (wayBuffer, wayIdx) =>
      val writeValid =
        writeReqValid && writeReq.bankMask(bankIdx) && writeReq.wayMask(wayIdx) && writeReq.writeUsefulEn(wayIdx)
      wayBuffer.io.enq.valid          := writeValid && !io.usefulResetStart && !usefulResetInFlightMask(bankIdx)
      wayBuffer.io.enq.bits.setIdx    := writeReq.setIdx
      wayBuffer.io.enq.bits.usefulCtr := writeReq.usefulCtrs(wayIdx)
      wayBuffer.io.flush.get          := io.usefulResetStart
    }
  }

  // write entry to sram from write buffer
  entrySram.zip(entryWriteBuffers).zipWithIndex.foreach { case ((bank, bankBuffers), bankIdx) =>
    bank.zip(bankBuffers).zipWithIndex.foreach { case ((way, buffer), wayIdx) =>
      val bufferOut      = buffer.io.read.head
      val trainingRead   = io.readReq(1).valid && io.readReq(1).bits.bankMask(bankIdx)
      val predictionRead = io.readReq(0).valid && io.readReq(0).bits.bankMask(bankIdx)
      val writeGrant     = !trainingRead && (!predictionRead || buffer.io.full.head)
      way.io.w.apply(
        bufferOut.valid && writeGrant,
        bufferOut.bits.entry,
        bufferOut.bits.setIdx,
        1.U(1.W) // way mask
      )
      bufferOut.ready := way.io.w.req.ready && writeGrant
      // The SRAM is single ported: a write in the same cycle silently drops the read.  The write
      // drain therefore gives the port up whenever a read is granted, and only writes in a cycle
      // whose prediction read was suppressed by a full write buffer.
      assert(
        !(way.io.r.req.valid && way.io.w.req.valid),
        s"tage_table_${tableIdx} bank_${bankIdx} way_${wayIdx}: SRAM read and write in the same cycle"
      )
    }
  }

  // Count per physical entry-SRAM bank/way, and only when the competing full-buffer
  // write actually fires. Lookup port 0 is combinational and belongs to this same request.
  private val predictionReadFullWrites = entryWriteBuffers.zipWithIndex.flatMap { case (bank, bankIdx) =>
    bank.map(buffer => predictionReadByBank(bankIdx) && buffer.io.full.head && buffer.io.read.head.fire)
  }
  private val predictionReadFullWriteBypassHits =
    predictionReadFullWrites.zip(entryWriteBuffers.flatten).map { case (blocked, buffer) =>
      blocked && buffer.io.lookup.get(0).head.valid
    }
  io.predictionReadFullWriteCount          := PopCount(predictionReadFullWrites)
  io.predictionReadFullWriteBypassHitCount := PopCount(predictionReadFullWriteBypassHits)
  XSPerfAccumulate("prediction_read_full_write", io.predictionReadFullWriteCount)
  XSPerfAccumulate("prediction_read_full_write_bypass_hit", io.predictionReadFullWriteBypassHitCount)

  private val bypassNow = Seq.fill(2)(Wire(Vec(NumWays, Valid(new TageEntry))))
  bypassNow.indices.foreach { readIdx =>
    bypassNow(readIdx).indices.foreach { wayIdx =>
      val validByBank = VecInit(entryWriteBuffers.map(_.apply(wayIdx).io.lookup.get(readIdx).head.valid))
      val entryByBank = entryWriteBuffers.map(_.apply(wayIdx).io.lookup.get(readIdx).head.bits.entry)
      bypassNow(readIdx)(wayIdx).valid := Mux1H(io.readReq(readIdx).bits.bankMask, validByBank)
      bypassNow(readIdx)(wayIdx).bits  := Mux1H(io.readReq(readIdx).bits.bankMask, entryByBank)
    }
  }
  private val bypassReg = Seq.fill(2) {
    RegInit(VecInit(Seq.fill(NumWays)(0.U.asTypeOf(Valid(new TageEntry)))))
  }
  when(io.readReq(0).valid)(bypassReg(0) := bypassNow(0))
  when(io.readReq(1).valid)(bypassReg(1) := bypassNow(1))
  usefulCtrSram.zip(usefulCtrWriteBuffers).zipWithIndex.foreach { case ((bank, bankBuffer), bankIdx) =>
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
      val usefulWriteValid = wayBuffer.io.deq.valid && !way.io.r.req.valid && !usefulResetValid
      way.io.w.apply(
        usefulResetValid || usefulWriteValid,
        Mux(usefulResetValid, UsefulCounter.Zero, wayBuffer.io.deq.bits.usefulCtr),
        Mux(usefulResetValid, usefulResetSetIdx(bankIdx), wayBuffer.io.deq.bits.setIdx),
        1.U(1.W) // way mask
      )
      wayBuffer.io.deq.ready := way.io.w.req.ready && !way.io.r.req.valid && !usefulResetValid
    }
  }

  io.readResp.zipWithIndex.foreach { case (resp, i) =>
    val readBankMaskNext = RegEnable(io.readReq(i).bits.bankMask, io.readReq(i).valid)
    val readValidNext    = RegNext(io.readReq(i).valid, init = false.B)
    val sramGrantedNow = VecInit((0 until NumWays).map { wayIdx =>
      val grantedByBank = VecInit((0 until NumBanks).map { bankIdx =>
        val trainingRead   = io.readReq(1).valid && io.readReq(1).bits.bankMask(bankIdx)
        val predictionRead = io.readReq(0).valid && io.readReq(0).bits.bankMask(bankIdx)
        if (i == 1) trainingRead
        else predictionRead && !trainingRead && !entryWriteBuffers(bankIdx)(wayIdx).io.full.head
      })
      Mux1H(io.readReq(i).bits.bankMask, grantedByBank)
    })
    val sramGrantedNext           = RegEnable(sramGrantedNow, io.readReq(i).valid)
    val readDuringUsefulResetNext = RegEnable(readDuringUsefulReset(i), io.readReq(i).valid)
    val sramEntries = Mux1H(
      readBankMaskNext,
      entrySram.map(bank => VecInit(bank.map(way => way.io.r.resp.data.head)))
    )
    val sramUsefulCtrs = Mux1H(
      readBankMaskNext,
      usefulCtrSram.map(bank => VecInit(bank.map(way => way.io.r.resp.data.head)))
    )

    val mergedEntries = VecInit(sramEntries.zip(bypassReg(i)).map { case (sramEntry, bypassEntry) =>
      Mux(bypassEntry.valid, bypassEntry.bits, sramEntry)
    })
    resp.entryValid := VecInit((0 until NumWays).map { wayIdx =>
      readValidNext && (bypassReg(i)(wayIdx).valid || sramGrantedNext(wayIdx))
    })
    resp.entries := mergedEntries
    resp.usefulCtrs := Mux(
      readDuringUsefulResetNext,
      VecInit.fill(NumWays)(UsefulCounter.Zero),
      if (i == 0) {
        VecInit(sramUsefulCtrs.zip(bypassReg(i)).map { case (sramUsefulCtr, bypassEntry) =>
          Mux(bypassEntry.valid, UsefulCounter.Zero, sramUsefulCtr)
        })
      } else {
        sramUsefulCtrs
      }
    )
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
    "overwrite",
    PopCount(entryWriteBuffers.flatMap(_.flatMap(_.io.overwrite)))
  )
  XSPerfAccumulate(
    "train_write_buffer_overwrite_with_prediction_read",
    PopCount(entryWriteBuffers.zip(predictionReadByBank).flatMap { case (bankBuffers, predictionRead) =>
      bankBuffers.flatMap(_.io.overwrite.map(_ && predictionRead))
    })
  )
  XSPerfAccumulate(
    "train_write_buffer_overwrite_with_consecutive_prediction_read",
    PopCount(entryWriteBuffers.zip(consecutivePredictionReadByBank).flatMap {
      case (bankBuffers, consecutivePredictionRead) =>
        bankBuffers.flatMap(_.io.overwrite.map(_ && consecutivePredictionRead))
    })
  )
  XSPerfAccumulate("write_buffer_read_hit", PopCount(bypassNow(0).map(_.valid)))
}
