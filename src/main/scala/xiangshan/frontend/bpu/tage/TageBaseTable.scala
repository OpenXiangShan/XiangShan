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
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.BpuTrain
import xiangshan.frontend.bpu.SaturateCounter
import xiangshan.frontend.bpu.WriteBuffer

class TageBaseTable(implicit p: Parameters) extends TageModule with Helpers {
  class TageBaseTableIO extends TageBundle {
    val readReqValid: Bool                 = Input(Bool())
    val startPc:      PrunedAddr           = Input(PrunedAddr(VAddrBits))
    val ctrs:         Vec[SaturateCounter] = Output(Vec(FetchBlockInstNum, new SaturateCounter(BaseTableTakenCtrWidth)))
    val train:        Valid[BpuTrain]      = Input(Valid(new BpuTrain))
    val resetDone:    Bool                 = Output(Bool())
  }
  val io: TageBaseTableIO = IO(new TageBaseTableIO)

  private val sramBanks =
    Seq.fill(BaseTableNumAlignBanks, BaseTableNumBanks)(
      Module(new SRAMTemplate(
        new SaturateCounter(BaseTableTakenCtrWidth),
        set = BaseTableNumSets,
        way = FetchBlockAlignInstNum,
        singlePort = true,
        shouldReset = true,
        withClockGate = true,
        hasMbist = hasMbist,
        hasSramCtl = hasSramCtl
      ))
    )

  // use a write buffer to store the write requests when read and write are both valid
  private val writeBuffers =
    Seq.fill(BaseTableNumAlignBanks, BaseTableNumBanks)(
      Module(new WriteBuffer(new BaseTableSramWriteReq, WriteBufferSize, numPorts = 1, pipe = true, hasTag = false))
    )

  // Connect write buffers to SRAMs
  sramBanks.flatten.zip(writeBuffers.flatten).foreach {
    case (bank, buffer) =>
      bank.io.w.req.valid       := buffer.io.read(0).valid && !bank.io.r.req.valid
      bank.io.w.req.bits.data   := buffer.io.read(0).bits.ctrs
      bank.io.w.req.bits.setIdx := buffer.io.read(0).bits.setIdx
      if (bank.io.w.req.bits.waymask.isDefined) {
        bank.io.w.req.bits.waymask.get := buffer.io.read(0).bits.wayMask.asUInt
      }
      buffer.io.read(0).ready := bank.io.w.req.ready && !bank.io.r.req.valid
  }

  io.resetDone := sramBanks.flatten.map(_.io.r.req.ready).reduce(_ && _)

  private val s0_fire    = io.readReqValid
  private val s0_startPc = io.startPc

  private val s0_alignBankIdx = getBaseTableAlignBankIndex(s0_startPc)
  private val s0_setIdx       = getBaseTableSetIndex(s0_startPc)
  private val s0_setIdxVec =
    Seq.tabulate(BaseTableNumAlignBanks)(bankIdx => Mux(bankIdx.U < s0_alignBankIdx, s0_setIdx + 1.U, s0_setIdx))

  private val s0_bankIdx  = getBaseTableBankIndex(s0_startPc)
  private val s0_bankMask = UIntToOH(s0_bankIdx, BaseTableNumBanks)

  dontTouch(s0_alignBankIdx)
  dontTouch(s0_setIdx)
  dontTouch(s0_bankIdx)
  dontTouch(s0_bankMask)

  sramBanks.zip(s0_setIdxVec).foreach {
    case (alignBank, setIdx) =>
      alignBank.zipWithIndex.foreach {
        case (bank, i) =>
          bank.io.r.req.valid       := s0_fire && s0_bankMask(i)
          bank.io.r.req.bits.setIdx := setIdx
      }
  }

  private val s1_alignBankIdx = RegEnable(s0_alignBankIdx, s0_fire)
  private val s1_bankMask     = RegEnable(s0_bankMask, s0_fire)

  private val s1_rawCtrs = VecInit(sramBanks.map(alignBank =>
    Mux1H(s1_bankMask, alignBank.map(_.io.r.resp.data))
  ))

  // rotate ctrs
  // for example, if BaseTableNumAlignBanks = 2, alignBankIdx = 1,
  // then io.ctrs := s1_rawCtrs(1) ++ s1_rawCtrs(0)
  // if BaseTableNumAlignBanks = 4, alignBankIdx = 1,
  // then io.ctrs := s1_rawCtrs(1) ++ s1_rawCtrs(2) ++ s1_rawCtrs(3) ++ s1_rawCtrs(0)
  io.ctrs := vecRotateRight(s1_rawCtrs, s1_alignBankIdx).flatten

  private val t1_trainValid = io.train.valid
  private val t1_train      = io.train.bits

  private val t1_alignBankIdx = getBaseTableAlignBankIndex(t1_train.startVAddr)
  private val t1_setIdx       = getBaseTableSetIndex(t1_train.startVAddr)
  private val t1_setIdxVec =
    Seq.tabulate(BaseTableNumAlignBanks)(bankIdx => Mux(bankIdx.U < t1_alignBankIdx, t1_setIdx + 1.U, t1_setIdx))

  private val t1_bankIdx  = getBaseTableBankIndex(t1_train.startVAddr)
  private val t1_bankMask = UIntToOH(t1_bankIdx, BaseTableNumBanks)

  dontTouch(t1_alignBankIdx)
  dontTouch(t1_setIdx)
  dontTouch(t1_bankIdx)
  dontTouch(t1_bankMask)

  private val t1_oldCtrs = t1_train.meta.tage.baseTableCtrs

  private val t1_newCtrs =
    Wire(Vec(BaseTableNumAlignBanks, Vec(FetchBlockAlignInstNum, new SaturateCounter(BaseTableTakenCtrWidth))))
  private val t1_wayMask = Wire(Vec(BaseTableNumAlignBanks, Vec(FetchBlockAlignInstNum, Bool())))

  t1_newCtrs.flatten.zip(t1_wayMask.flatten).zipWithIndex.foreach {
    case ((newCtr, wayMaskBit), position) =>
      when(position.U < t1_train.cfiPosition) {
        newCtr.value := t1_oldCtrs(position).getDecrease
        wayMaskBit   := true.B
      }.elsewhen(position.U === t1_train.cfiPosition) {
        newCtr.value := t1_oldCtrs(position).getUpdate(t1_train.taken)
        wayMaskBit   := true.B
      }.otherwise {
        newCtr.value := 0.U // it won't be written to SRAM
        wayMaskBit   := false.B
      }
  }

  private val t1_newCtrsVec = vecRotateRight(t1_newCtrs, t1_alignBankIdx)
  private val t1_wayMaskVec = vecRotateRight(t1_wayMask, t1_alignBankIdx)

  writeBuffers.zipWithIndex.foreach {
    case (alignBuffers, alignIdx) =>
      alignBuffers.zipWithIndex.foreach {
        case (buffer, bankIdx) =>
          buffer.io.write(0).valid        := t1_trainValid && t1_bankMask(bankIdx)
          buffer.io.write(0).bits.setIdx  := t1_setIdxVec(alignIdx)
          buffer.io.write(0).bits.ctrs    := t1_newCtrsVec(alignIdx)
          buffer.io.write(0).bits.wayMask := t1_wayMaskVec(alignIdx)
      }
  }

}
