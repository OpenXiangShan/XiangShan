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

package xiangshan.frontend.bpu.sc

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.XSPerfAccumulate
import utility.sram.SRAMTemplate
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.FoldedHistoryInfo
import xiangshan.frontend.bpu.WriteBuffer
import xiangshan.frontend.bpu.history.phr.PhrAllFoldedHistories

class ScTable(
    numSets:   Int,
    numWays:   Int,
    tableType: String,
    tableIdx:  Int
)(implicit p: Parameters)
    extends ScModule with HasScParameters with Helpers {
  class ScTableIO extends ScBundle {
    val predictReadReq:  Valid[ScTableReq] = Flipped(Valid(new ScTableReq(numSets, numWays)))
    val predictReadResp: Vec[ScEntry]      = Output(Vec(numWays, new ScEntry()))
    val trainReadReq:    Valid[ScTableReq] = Flipped(Valid(new ScTableReq(numSets, numWays)))
    val trainReadResp:   Vec[ScEntry]      = Output(Vec(numWays, new ScEntry()))
    val update:          ScTableTrain      = Input(new ScTableTrain(numSets, numWays))
    val sramResetDone:   Bool              = Output(Bool())
  }

  val io = IO(new ScTableIO())

  println(f"Sc$tableType[$tableIdx]:")
  println(f"  Size(set, bank, way): $numSets * $NumBanks * $numWays")
  println(f"  Address fields:")
  addrFields.show(indent = 4)

  def numRows: Int = numSets

  private val sram = Seq.fill(NumBanks)(
    Module(new SRAMTemplate(
      new ScEntry(),
      set = numRows,
      way = numWays,
      singlePort = true,
      shouldReset = true,
      holdRead = true,
      withClockGate = true,
      hasMbist = hasMbist,
      hasSramCtl = hasSramCtl,
      suffix = Option("bpu_sc")
    ))
  )

  private val writeBuffer = Seq.tabulate(NumBanks)(bankIdx =>
    Module(new WriteBuffer(
      new ScTableSramWriteReq(numRows, numWays),
      WriteBufferSize,
      numPorts = 1,
      numWays = numWays,
      hasWayMask = true,
      nameSuffix = s"sc${tableType}${tableIdx}_${bankIdx}"
    ))
  )

  // read path table by setIndex
  sram.zipWithIndex.foreach { case (bank, bankIdx) =>
    val predictReadValid = io.predictReadReq.valid && io.predictReadReq.bits.bankMask(bankIdx)
    val trainReadValid   = io.trainReadReq.valid && io.trainReadReq.bits.bankMask(bankIdx)
    bank.io.r.req.valid       := predictReadValid || trainReadValid
    bank.io.r.req.bits.setIdx := Mux(predictReadValid, io.predictReadReq.bits.setIdx, io.trainReadReq.bits.setIdx)
    assert(!(predictReadValid && trainReadValid), s"read conflict in sc${tableType}${tableIdx}_${bankIdx}")
  }

  io.sramResetDone := sram.map(_.io.resetDone).reduce(_ && _)

  private val predictReadBankMaskNext = RegEnable(io.predictReadReq.bits.bankMask, io.predictReadReq.valid)
  io.predictReadResp := Mux1H(predictReadBankMaskNext, sram.map(_.io.r.resp.data))

  private val trainReadBankMaskNext = RegEnable(io.trainReadReq.bits.bankMask, io.trainReadReq.valid)
  io.trainReadResp := Mux1H(trainReadBankMaskNext, sram.map(_.io.r.resp.data))

  // update path table
  private val updateValid    = io.update.valid && io.update.wayMask.reduce(_ || _)
  private val updateIdx      = io.update.setIdx
  private val updateWayMask  = io.update.wayMask
  private val updateBankMask = io.update.bankMask

  writeBuffer.zip(updateBankMask.asBools).foreach {
    case (buffer, bankEnable) =>
      val writeValid = updateValid && bankEnable
      buffer.io.write.head.valid       := writeValid
      buffer.io.write.head.bits.setIdx := updateIdx
      buffer.io.write.head.bits.wayMask.get := Mux(
        writeValid,
        updateWayMask,
        VecInit.fill(numWays)(false.B)
      )
      buffer.io.write.head.bits.wayData.get := Mux(
        writeValid,
        VecInit(io.update.entryVec.map(_.asUInt)),
        VecInit.fill(numWays)(0.U.asTypeOf(new ScEntry()).asUInt)
      )
  }

  sram.zip(writeBuffer).zipWithIndex.foreach {
    case ((bank, buffer), i) =>
      bank.io.w.req.valid            := buffer.io.read.head.valid && !bank.io.r.req.valid
      bank.io.w.req.bits.setIdx      := buffer.io.read.head.bits.setIdx
      bank.io.w.req.bits.waymask.get := buffer.io.read.head.bits.wayMask.get.asUInt
      bank.io.w.req.bits.data        := VecInit(buffer.io.read.head.bits.wayData.get.map(_.asTypeOf(new ScEntry())))
      buffer.io.read.head.ready      := bank.io.w.req.ready && !bank.io.r.req.valid
      buffer.io.read.head.ready      := bank.io.w.req.ready && !bank.io.r.req.valid
  }
}
