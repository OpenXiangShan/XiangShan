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
    // read
    val readReqValid: Bool           = Input(Bool())
    val readSetIdx:   UInt           = Input(UInt(log2Ceil(numSets / NumBanks).W))
    val readBankMask: UInt           = Input(UInt(NumBanks.W))
    val readData:     Vec[TageEntry] = Output(Vec(NumWays, new TageEntry))
    // write
    val writeReqValid: Bool           = Input(Bool())
    val writeSetIdx:   UInt           = Input(UInt(log2Ceil(numSets / NumBanks).W))
    val writeBankMask: UInt           = Input(UInt(NumBanks.W))
    val writeWayMask:  UInt           = Input(UInt(NumWays.W))
    val writeData:     Vec[TageEntry] = Input(Vec(NumWays, new TageEntry))
    // other
    val allocFail: Bool = Input(Bool())
    val resetDone: Bool = Output(Bool())
  }
  val io: TageTableIO = IO(new TageTableIO)

  private val sramBanks =
    Seq.fill(NumBanks)(
      Module(new SRAMTemplate(
        new TageEntry,
        set = numSets / NumBanks,
        way = NumWays,
        singlePort = true,
        shouldReset = true,
        useBitmask = true,
        withClockGate = true,
        hasMbist = hasMbist,
        hasSramCtl = hasSramCtl
      ))
    )

  // use a write buffer to store the write requests when read and write are both valid
  private val writeBuffers =
    Seq.fill(NumBanks)(
      Module(new WriteBuffer(
        new TableSramWriteReq(numSets),
        WriteBufferSize,
        numPorts = 1,
        pipe = true,
        hasTag = true
      ))
    )

  // count allocation failure times for each set
  private val allocFailCtrs = RegInit(VecInit(
    Seq.fill(NumBanks)(VecInit(Seq.fill(numSets / NumBanks)(0.U.asTypeOf(new SaturateCounter(AllocFailCtrWidth)))))
  ))

  // Connect write buffers to SRAMs
  sramBanks.zip(writeBuffers).zip(allocFailCtrs).foreach {
    case ((bank, buffer), allocFailCtr) =>
      val valid              = buffer.io.read.head.valid && !bank.io.r.req.valid
      val needResetUsefulCtr = buffer.io.read.head.bits.needResetUsefulCtr
      val data               = buffer.io.read.head.bits.data
      val setIdx             = buffer.io.read.head.bits.setIdx
      val wayMask            = buffer.io.read.head.bits.wayMask.asUInt
      val bitMask =
        Mux(
          needResetUsefulCtr,
          (1 << UsefulCtrWidth - 1).U((new TageEntry).getWidth.W),
          ~0.U((new TageEntry).getWidth.W)
        ).asUInt
      bank.io.w.apply(valid, data, setIdx, wayMask, bitMask)

      buffer.io.read.head.ready := bank.io.w.req.ready && !bank.io.r.req.valid

      when(valid && needResetUsefulCtr) {
        allocFailCtr(setIdx).resetZero()
        when(setIdx =/= io.writeSetIdx && io.allocFail && !allocFailCtr(io.writeSetIdx).isPositive) {
          allocFailCtr(io.writeSetIdx).increase()
        }
      }.elsewhen(io.allocFail && !allocFailCtr(io.writeSetIdx).isPositive) {
        allocFailCtr(io.writeSetIdx).increase()
      }
  }

  sramBanks.zipWithIndex.foreach {
    case (bank, bankIdx) =>
      bank.io.r.req.valid       := io.readReqValid && io.readBankMask(bankIdx)
      bank.io.r.req.bits.setIdx := io.readSetIdx
  }

  io.resetDone := sramBanks.map(_.io.r.req.ready).reduce(_ && _)

  io.readData := Mux1H(RegEnable(io.readBankMask, io.readReqValid), sramBanks.map(_.io.r.resp.data))

  writeBuffers.zipWithIndex.foreach {
    case (buffer, bankIdx) =>
      val resetUsefulCtr = io.allocFail && allocFailCtrs(bankIdx)(io.writeSetIdx).isSaturatePositive
      buffer.io.write.head.valid                   := (io.writeReqValid || resetUsefulCtr) && io.writeBankMask(bankIdx)
      buffer.io.write.head.bits.needResetUsefulCtr := resetUsefulCtr && !io.writeReqValid
      buffer.io.write.head.bits.setIdx             := io.writeSetIdx
      buffer.io.write.head.bits.wayMask            := Mux(io.writeReqValid, io.writeWayMask, Fill(NumWays, 1.U(1.W)))
      buffer.io.write.head.bits.data               := Mux(io.writeReqValid, io.writeData, 0.U.asTypeOf(io.writeData))
  }
}
