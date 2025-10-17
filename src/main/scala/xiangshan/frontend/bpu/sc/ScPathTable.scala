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

class ScPathTable(val numSets: Int, val histLen: Int)(implicit p: Parameters)
    extends ScModule with HasScParameters with Helpers {
  class ScPathTableIO extends ScBundle {
    val req:    DecoupledIO[UInt] = Flipped(Decoupled(UInt(log2Ceil(numSets / NumWays).W)))
    val resp:   Vec[ScEntry]      = Output(Vec(NumWays, new ScEntry()))
    val update: PathTableTrain    = Input(new PathTableTrain(numSets))
  }

  val io = IO(new ScPathTableIO())

  def numRows: Int = numSets / NumBanks / NumWays

  private val sram = Seq.fill(NumBanks)(
    Module(new SRAMTemplate(
      new ScEntry(),
      set = numRows,
      way = NumWays,
      shouldReset = true,
      singlePort = true,
      withClockGate = true,
      hasMbist = hasMbist,
      hasSramCtl = hasSramCtl
    ))
  )

  private val writeBuffer = Seq.fill(NumBanks)(
    Module(new WriteBuffer(
      new PathTableSramWriteReq(numRows),
      WriteBufferSize,
      numPorts = 1
    ))
  )

  // read path table by setIndex
  private val reqBankIdx  = io.req.bits(log2Ceil(NumBanks) - 1, 0)
  private val reqSetIdx   = io.req.bits >> log2Ceil(NumBanks)
  private val reqBankMask = UIntToOH(reqBankIdx, NumBanks)
  sram.zip(reqBankMask.asBools).foreach {
    case (bank, bankEnable) =>
      bank.io.r.req.valid       := io.req.valid && bankEnable
      bank.io.r.req.bits.setIdx := reqSetIdx
  }

  io.req.ready := sram.map(_.io.r.req.ready).reduce(_ && _)

  private val respBankMask = RegEnable(reqBankMask, io.req.valid)
  io.resp := Mux1H(respBankMask.asBools, sram.map(_.io.r.resp.data))

  // update path table
  private val updateValid    = io.update.valid
  private val updateIdx      = io.update.setIdx
  private val updateWayMask  = io.update.wayMask
  private val updateBankIdx  = updateIdx(log2Ceil(NumBanks) - 1, 0)
  private val updateBankMask = UIntToOH(updateBankIdx, NumBanks)

  writeBuffer.zip(updateBankMask.asBools).foreach {
    case (buffer, bankEnable) =>
      val writeValid = updateValid && bankEnable
      buffer.io.write.head.valid       := writeValid
      buffer.io.write.head.bits.setIdx := updateIdx >> log2Ceil(NumBanks)
      buffer.io.write.head.bits.wayMask := Mux(
        writeValid,
        updateWayMask,
        VecInit.fill(NumWays)(false.B)
      )
      buffer.io.write.head.bits.entryVec := Mux(
        writeValid,
        io.update.entryVec,
        VecInit.fill(ResolveEntryBranchNumber)(0.U.asTypeOf(new ScEntry()))
      )
  }

  sram.zip(writeBuffer).zipWithIndex.foreach {
    case ((bank, buffer), i) =>
      bank.io.w.req.valid            := buffer.io.read.head.valid && !bank.io.r.req.valid
      bank.io.w.req.bits.setIdx      := buffer.io.read.head.bits.setIdx
      bank.io.w.req.bits.waymask.get := buffer.io.read.head.bits.wayMask.asUInt
      bank.io.w.req.bits.data        := buffer.io.read.head.bits.entryVec
      buffer.io.read.head.ready      := bank.io.w.req.ready && !bank.io.r.req.valid
  }
}
