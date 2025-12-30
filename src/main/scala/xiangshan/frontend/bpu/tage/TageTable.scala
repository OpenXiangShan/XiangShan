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
import utility.SyncDataModuleTemplate
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
      Module(new SyncDataModuleTemplate(
        new TageEntry,
        numEntries = NumSets,
        numRead = 2,
        numWrite = 1,
        parentModule = "tage",
        perReadPortBypassEnable = Some(Seq(true, true)),
        hasRen = true
      )).suggestName(s"tage_entry_sram_bank${bankIdx}_way${wayIdx}")
    }

  // TODO: use SRAM to implement it
  private val usefulCtrs = RegInit(
    VecInit.fill(NumBanks)(
      VecInit.fill(NumWays)(
        VecInit.fill(NumSets)(
          0.U.asTypeOf(new SaturateCounter(UsefulCtrWidth))
        )
      )
    )
  )

  // read sram
  entrySram.zipWithIndex.foreach { case (bank, bankIdx) =>
    val predictReadValid = io.predictReadReq.valid && io.predictReadReq.bits.bankMask(bankIdx)
    val trainReadValid   = io.trainReadReq.valid && io.trainReadReq.bits.bankMask(bankIdx)
    bank.foreach { way =>
      if (way.io.ren.isDefined) {
        way.io.ren.get(0) := predictReadValid
        way.io.ren.get(1) := trainReadValid
      }
      way.io.raddr(0) := io.predictReadReq.bits.setIdx
      way.io.raddr(1) := io.trainReadReq.bits.setIdx
    }
  }

  // write sram
  entrySram.zipWithIndex.foreach { case (bank, bankIdx) =>
    bank.zipWithIndex.foreach { case (way, wayIdx) =>
      val writeValid = io.writeReq.valid && io.writeReq.bits.bankMask(bankIdx) && io.writeReq.bits.wayMask(wayIdx)
      val setIdx     = io.writeReq.bits.setIdx
      val entry      = io.writeReq.bits.entries(wayIdx)
      way.io.wen(0)   := writeValid
      way.io.waddr(0) := setIdx
      way.io.wdata(0) := entry

      when(io.resetUseful) {
        usefulCtrs(bankIdx)(wayIdx).foreach(_.resetZero())
      }.elsewhen(writeValid) {
        usefulCtrs(bankIdx)(wayIdx)(setIdx).value := io.writeReq.bits.usefulCtrs(wayIdx).value
      }
    }
  }


  private val predictReadSetIdxNext   = RegEnable(io.predictReadReq.bits.setIdx, io.predictReadReq.valid)
  private val predictReadBankMaskNext = RegEnable(io.predictReadReq.bits.bankMask, io.predictReadReq.valid)
  io.predictReadResp.entries := Mux1H(
    predictReadBankMaskNext,
    entrySram.map(bank => VecInit(bank.map(way => way.io.rdata(0))))
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
    entrySram.map(bank => VecInit(bank.map(way => way.io.rdata(1))))
  )
  io.trainReadResp.usefulCtrs := Mux1H(
    trainReadBankMaskNext,
    usefulCtrs.map(ctrsPerBank =>
      VecInit(ctrsPerBank.map(ctrsPerWay => ctrsPerWay(trainReadSetIdxNext)))
    )
  )

  io.resetDone := true.B

  XSPerfAccumulate("predict_read", io.predictReadReq.valid)
  XSPerfAccumulate("train_read", io.trainReadReq.valid)
  XSPerfAccumulate("write", io.writeReq.valid)
}
