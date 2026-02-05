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

package xiangshan.frontend.bpu.utage

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import scala.math.min
import utility.XSPerfAccumulate
import utility.sram.SRAMTemplate
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.FoldedHistoryInfo
import xiangshan.frontend.bpu.SaturateCounter
import xiangshan.frontend.bpu.history.phr.PhrAllFoldedHistories
import yunsuan.vector.alu.VIntFixpTable.table

// MicroTage table module implementing a banked SRAM with write buffer
class MicroTageTable(
    val numSets:  Int,
    val numWay:   Int,
    val tableId:  Int,
    val NumBanks: Int = 4
)(implicit p: Parameters) extends MicroTageModule with Helpers {
  // IO bundle definition
  class MicroTageTableIO extends MicroTageBundle {
    // Request bundle for table access
    class MicroTageReq extends Bundle {
      val readIndex: UInt = UInt(log2Ceil(numSets).W)
    }
    // Response bundle for table read
    class MicroTageResp extends Bundle {
      val readEntries: Vec[MicroTageEntry] = Vec(numWay, new MicroTageEntry)
    }
    val req:         Valid[MicroTageReq] = Input(Valid(new MicroTageReq))
    val resps:       MicroTageResp       = Output(new MicroTageResp)
    val train:       MicroTageTrain      = new MicroTageTrain(numWay, numSets)
    val usefulReset: Bool                = Input(Bool())
  }
  val io = IO(new MicroTageTableIO)
  // Write buffer to handle write conflicts
  private val wbuffer = Module(new BypassShadowBuffer(numSets, numWay, 16, tableId, NumBanks))

  // Banked SRAM for storing MicroTage entries
  private val entrySram = Seq.tabulate(NumBanks) { bankIdx =>
    Module(new SRAMTemplate(
      new MicroTageEntry,
      set = numSets / NumBanks,
      way = numWay,
      singlePort = true,
      shouldReset = true,
      withClockGate = true,
      hasMbist = hasMbist,
      hasSramCtl = hasSramCtl,
      suffix = Option("bpu_utage")
    )).suggestName(s"utage_entry_sram_bank${bankIdx}")
  }

  // Calculate bank selection for read access
  private val bankOH             = UIntToOH(getBankId(io.req.bits.readIndex, NumBanks))
  private val bankReadInnerIndex = getBankInnerIndex(io.req.bits.readIndex, NumBanks, numSets)

  // Read from all SRAM banks
  entrySram.zipWithIndex.foreach { case (bank, bankIdx) =>
    bank.io.r.req.valid       := bankOH(bankIdx)
    bank.io.r.req.bits.setIdx := bankReadInnerIndex
  }

  // Pipeline stage: capture bank selection from previous cycle
  private val a1_bankOH = RegNext(bankOH)
  // Collect read responses from all banks
  private val bankReadRespVec = VecInit(entrySram.map(_.io.r.resp.data))
  // Select the appropriate bank's response based on a1_bankOH
  private val bankReadEntries = Mux1H(a1_bankOH, bankReadRespVec)

  // Check if requested data is in write buffer
  wbuffer.io.req.readIndex := io.req.bits.readIndex
  private val bufferHit         = wbuffer.io.resp.hit
  private val bufferReadEntries = wbuffer.io.resp.readEntries
  // Convert SRAM response to proper type
  private val sramReadEntries = bankReadEntries.asTypeOf(Vec(numWay, new MicroTageEntry))

  // Select data from buffer (if hit) or SRAM (if miss)
  private val readEntries = VecInit(
    (bufferHit, bufferReadEntries, sramReadEntries).zipped.map {
      case (hit, bufferEntry, sramEntry) => Mux(hit, bufferEntry, sramEntry)
    }
  )

  // Output read entries
  io.resps.readEntries := readEntries

  // Determine if write can proceed to SRAM
  // Write succeeds if accessing different banks or forceWrite is set
  private val writeSuccess =
    ((getBankId(io.req.bits.readIndex, NumBanks) =/= getBankId(wbuffer.io.tryWrite.bits.writeIndex, NumBanks)) ||
      wbuffer.io.tryWrite.bits.forceWrite) && wbuffer.io.tryWrite.valid

  // Connect training and control signals to write buffer
  wbuffer.io.train <> io.train
  wbuffer.io.writeSuccess := writeSuccess
  wbuffer.io.usefulReset  := io.usefulReset

  private val tryWrite       = wbuffer.io.tryWrite.valid
  private val writeBankId    = getBankId(wbuffer.io.tryWrite.bits.writeIndex, NumBanks)
  private val writeEntry     = wbuffer.io.tryWrite.bits.writeData
  private val bankWriteIndex = getBankInnerIndex(wbuffer.io.tryWrite.bits.writeIndex, NumBanks, numSets)
  private val forceWrite     = wbuffer.io.tryWrite.bits.forceWrite
  private val writeMask      = UIntToOH(wbuffer.io.tryWrite.bits.way)
  entrySram.zipWithIndex.foreach { case (bank, bankIdx) =>
    val writeValid = (!bank.io.r.req.valid || forceWrite) && tryWrite && (writeBankId === bankIdx.U)
    bank.io.w(writeValid, writeEntry, bankWriteIndex, writeMask)
  }
}
