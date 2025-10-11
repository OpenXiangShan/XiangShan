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
    val takenCtrs:    Vec[SaturateCounter] = Output(Vec(FetchBlockInstNum, new SaturateCounter(BaseTableTakenCtrWidth)))
    val train:        Valid[BpuTrain]      = Input(Valid(new BpuTrain))
    val resetDone:    Bool                 = Output(Bool())
  }
  val io: TageBaseTableIO = IO(new TageBaseTableIO)

  private val sramBanks =
    Seq.fill(BaseTableNumAlignBanks, NumBanks)(
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
    Seq.fill(BaseTableNumAlignBanks, NumBanks)(
      Module(new WriteBuffer(new BaseTableSramWriteReq, WriteBufferSize, numPorts = 1))
    )

  // Connect write buffers to SRAMs
  sramBanks.flatten.zip(writeBuffers.flatten).foreach {
    case (bank, buffer) =>
      val valid   = buffer.io.read.head.valid && !bank.io.r.req.valid
      val data    = buffer.io.read.head.bits.takenCtrs
      val setIdx  = buffer.io.read.head.bits.setIdx
      val wayMask = buffer.io.read.head.bits.wayMask
      bank.io.w.apply(valid, data, setIdx, wayMask)

      buffer.io.read.head.ready := bank.io.w.req.ready && !bank.io.r.req.valid
  }

  io.resetDone := sramBanks.flatten.map(_.io.r.req.ready).reduce(_ && _)

  /* --------------------------------------------------------------------------------------------------------------
     stage 0
     - send read request to SRAM
     -------------------------------------------------------------------------------------------------------------- */

  private val s0_fire    = io.readReqValid
  private val s0_startPc = io.startPc

  private val s0_alignBankIdx = getBaseTableAlignBankIndex(s0_startPc)
  private val s0_rawSetIdx    = getBaseTableSetIndex(s0_startPc)
  private val s0_setIdx =
    Seq.tabulate(BaseTableNumAlignBanks)(bankIdx => Mux(bankIdx.U < s0_alignBankIdx, s0_rawSetIdx + 1.U, s0_rawSetIdx))

  private val s0_bankIdx  = getBaseTableBankIndex(s0_startPc)
  private val s0_bankMask = UIntToOH(s0_bankIdx, NumBanks)

  /* --------------------------------------------------------------------------------------------------------------
     stage 1
     - get raw ctrs from SRAM
     - rotate ctrs
     -------------------------------------------------------------------------------------------------------------- */

  private val s1_alignBankIdx = RegEnable(s0_alignBankIdx, s0_fire)
  private val s1_bankMask     = RegEnable(s0_bankMask, s0_fire)

  private val s1_rawCtrs = VecInit(sramBanks.map(alignBank =>
    Mux1H(s1_bankMask, alignBank.map(_.io.r.resp.data))
  ))

  /*
   * rotate ctrs
   * for example, if BaseTableNumAlignBanks = 2, alignBankIdx = 1,
   * then io.ctrs := s1_rawCtrs(1) ++ s1_rawCtrs(0)
   * if BaseTableNumAlignBanks = 4, alignBankIdx = 1,
   * then io.ctrs := s1_rawCtrs(1) ++ s1_rawCtrs(2) ++ s1_rawCtrs(3) ++ s1_rawCtrs(0)
   */
  io.takenCtrs := vecRotateRight(s1_rawCtrs, s1_alignBankIdx).flatten

  /* --------------------------------------------------------------------------------------------------------------
   train stage 0
   - read old ctrs
   -------------------------------------------------------------------------------------------------------------- */

  private val t0_valid      = io.train.valid
  private val t0_startVAddr = io.train.bits.startVAddr
  private val t0_branches   = io.train.bits.branches

  private val t0_alignBankIdx = getBaseTableAlignBankIndex(t0_startVAddr)
  private val t0_rawSetIdx    = getBaseTableSetIndex(t0_startVAddr)
  private val t0_setIdx =
    Seq.tabulate(BaseTableNumAlignBanks)(bankIdx => Mux(bankIdx.U < t0_alignBankIdx, t0_rawSetIdx + 1.U, t0_rawSetIdx))

  private val t0_bankIdx  = getBankIndex(t0_startVAddr)
  private val t0_bankMask = UIntToOH(t0_bankIdx, NumBanks)

  sramBanks.zipWithIndex.foreach {
    case (alignBank, alignIdx) =>
      alignBank.zipWithIndex.foreach {
        case (bank, bankIdx) =>
          bank.io.r.req.valid       := s0_fire && s0_bankMask(bankIdx) || t0_valid && t0_bankMask(bankIdx)
          bank.io.r.req.bits.setIdx := Mux(t0_valid, t0_setIdx(alignIdx), s0_setIdx(alignIdx))
      }
  }

  /* --------------------------------------------------------------------------------------------------------------
   train stage 1
   - get old ctrs from SRAM
   -------------------------------------------------------------------------------------------------------------- */

  private val t1_valid    = RegNext(t0_valid)
  private val t1_branches = RegEnable(t0_branches, t0_valid)

  private val t1_alignBankIdx = RegEnable(s0_alignBankIdx, s0_fire)
  private val t1_bankMask     = RegEnable(s0_bankMask, s0_fire)
  private val t1_setIdx       = t0_setIdx.map(RegEnable(_, t0_valid))

  private val t1_rawCtrs = VecInit(sramBanks.map(alignBank =>
    Mux1H(s1_bankMask, alignBank.map(_.io.r.resp.data))
  ))

  private val t1_oldCtrs = vecRotateRight(t1_rawCtrs, t1_alignBankIdx).flatten

  /* --------------------------------------------------------------------------------------------------------------
   train stage 2
   - update ctrs
   -------------------------------------------------------------------------------------------------------------- */

  private val t2_valid    = RegNext(t1_valid)
  private val t2_branches = RegEnable(t1_branches, t1_valid)

  private val t2_alignBankIdx = RegEnable(t1_alignBankIdx, t1_valid)
  private val t2_bankMask     = RegEnable(t1_bankMask, t1_valid)
  private val t2_setIdx       = t1_setIdx.map(RegEnable(_, t1_valid))

  private val t2_oldCtrs = t1_oldCtrs.map(RegEnable(_, t1_valid))

  private val t2_updateMask = Wire(Vec(BaseTableNumAlignBanks, Vec(FetchBlockAlignInstNum, Bool())))
  private val t2_newCtrs =
    Wire(Vec(BaseTableNumAlignBanks, Vec(FetchBlockAlignInstNum, new SaturateCounter(BaseTableTakenCtrWidth))))

  t2_newCtrs.flatten.zip(t2_updateMask.flatten).zipWithIndex.foreach {
    case ((newCtr, needUpdate), position) =>
      t2_branches.foreach { branch =>
        when(position.U === branch.bits.cfiPosition) {
          needUpdate   := true.B
          newCtr.value := t2_oldCtrs(position).getUpdate(branch.bits.taken)
        }.otherwise {
          needUpdate   := false.B
          newCtr.value := 0.U
        }
      }
  }

  private val t2_rotatedNewCtrs    = vecRotateRight(t2_newCtrs, t2_alignBankIdx)
  private val t2_rotatedUpdateMask = vecRotateRight(t2_updateMask, t2_alignBankIdx)

  writeBuffers.zipWithIndex.foreach {
    case (alignBuffers, alignIdx) =>
      alignBuffers.zipWithIndex.foreach {
        case (buffer, bankIdx) =>
          buffer.io.write.head.valid          := t2_valid && t2_bankMask(bankIdx)
          buffer.io.write.head.bits.setIdx    := t2_setIdx(alignIdx)
          buffer.io.write.head.bits.takenCtrs := t2_rotatedNewCtrs(alignIdx)
          buffer.io.write.head.bits.wayMask   := t2_rotatedUpdateMask(alignIdx).asUInt
      }
  }
}
