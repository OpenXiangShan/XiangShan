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
import utility.sram.SRAMTemplate
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.BpuTrain
import xiangshan.frontend.bpu.SaturateCounter

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
        holdRead = true,
        withClockGate = true,
        hasMbist = hasMbist,
        hasSramCtl = hasSramCtl
      ))
    )

  // use a write buffer to store the write requests when read and write are both valid
  private val writeBuffers =
    Seq.fill(BaseTableNumAlignBanks, NumBanks)(
      Module(new Queue(new BaseTableSramWriteReq, WriteBufferSize, pipe = true, flow = true))
    )

  // Connect write buffers to SRAMs
  sramBanks.flatten.zip(writeBuffers.flatten).foreach { case (bank, buffer) =>
    val valid   = buffer.io.deq.valid && !bank.io.r.req.valid
    val data    = buffer.io.deq.bits.takenCtrs
    val setIdx  = buffer.io.deq.bits.setIdx
    val wayMask = buffer.io.deq.bits.wayMask
    bank.io.w.apply(valid, data, setIdx, wayMask)

    buffer.io.deq.ready := bank.io.w.req.ready && !bank.io.r.req.valid
  }

  io.resetDone := sramBanks.flatten.map(_.io.r.req.ready).reduce(_ && _)

  /* --------------------------------------------------------------------------------------------------------------
     stage 0
     - send read request to SRAM
     -------------------------------------------------------------------------------------------------------------- */

  private val s0_fire    = io.readReqValid
  private val s0_startPc = io.startPc

  private val s0_firstAlignBankIdx = getBaseTableAlignBankIndex(s0_startPc)
  private val s0_setIdx            = getBaseTableSetIndex(s0_startPc)
  private val s0_nextSetIdx        = getBaseTableSetIndex(getNextAlignedAddr(s0_startPc))
  private val s0_setIdxVec = VecInit.tabulate(BaseTableNumAlignBanks)(idx =>
    Mux(idx.U < s0_firstAlignBankIdx, s0_nextSetIdx, s0_setIdx)
  )

  private val s0_bankIdx  = getBaseTableBankIndex(s0_startPc)
  private val s0_bankMask = UIntToOH(s0_bankIdx, NumBanks)

  sramBanks.zipWithIndex.foreach { case (alignBank, alignBankIdx) =>
    alignBank.zipWithIndex.foreach { case (bank, bankIdx) =>
      bank.io.r.req.valid       := s0_fire && s0_bankMask(bankIdx)
      bank.io.r.req.bits.setIdx := s0_setIdxVec(alignBankIdx)
    }
  }

  /* --------------------------------------------------------------------------------------------------------------
     stage 1
     - get raw ctrs from SRAM
     - rotate ctrs
     -------------------------------------------------------------------------------------------------------------- */

  private val s1_firstAlignBankIdx = RegEnable(s0_firstAlignBankIdx, s0_fire)
  private val s1_bankMask          = RegEnable(s0_bankMask, s0_fire)

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
  io.takenCtrs := vecRotateRight(s1_rawCtrs, s1_firstAlignBankIdx).flatten

  /* --------------------------------------------------------------------------------------------------------------
   train stage 0
   - delay 1 cycle for better timing
   -------------------------------------------------------------------------------------------------------------- */

  private val t0_valid = io.train.valid
  private val t0_train = io.train.bits

  /* --------------------------------------------------------------------------------------------------------------
   train stage 1
   - update ctrs
   -------------------------------------------------------------------------------------------------------------- */

  private val t1_valid = RegNext(t0_valid)
  private val t1_train = RegEnable(t0_train, t0_valid)

  private val t1_startVAddr = t1_train.startVAddr
  private val t1_branches   = t1_train.branches
  private val t1_oldCtrs    = t1_train.meta.tage.baseTableCtrs

  private val t1_firstAlignBankIdx = getBaseTableAlignBankIndex(t1_startVAddr)
  private val t1_setIdx            = getBaseTableSetIndex(t1_startVAddr)
  private val t1_nextSetIdx        = getBaseTableSetIndex(getNextAlignedAddr(t1_startVAddr))
  private val t1_setIdxVec = VecInit.tabulate(BaseTableNumAlignBanks)(idx =>
    Mux(idx.U < t1_firstAlignBankIdx, t1_nextSetIdx, t1_setIdx)
  )
  private val t1_bankIdx  = getBankIndex(t1_startVAddr)
  private val t1_bankMask = UIntToOH(t1_bankIdx, NumBanks)

  private val t1_updateMask = Wire(Vec(BaseTableNumAlignBanks, Vec(FetchBlockAlignInstNum, Bool())))
  private val t1_newCtrs =
    Wire(Vec(BaseTableNumAlignBanks, Vec(FetchBlockAlignInstNum, new SaturateCounter(BaseTableTakenCtrWidth))))

  t1_newCtrs.flatten.zip(t1_updateMask.flatten).zipWithIndex.foreach { case ((newCtr, needUpdate), position) =>
    val hitMask = t1_branches.map { branch =>
      branch.valid && branch.bits.attribute.isConditional && position.U === branch.bits.cfiPosition
    }
    val taken = Mux1H(hitMask, t1_branches.map(_.bits.taken))
    needUpdate   := hitMask.reduce(_ || _)
    newCtr.value := t1_oldCtrs(position).getUpdate(taken)
  }

  private val t1_rotatedNewCtrs    = vecRotateRight(t1_newCtrs, t1_firstAlignBankIdx)
  private val t1_rotatedUpdateMask = vecRotateRight(t1_updateMask, t1_firstAlignBankIdx)

  writeBuffers.zipWithIndex.foreach { case (buffersPerAlignBank, alignBankIdx) =>
    buffersPerAlignBank.zipWithIndex.foreach { case (buffer, bankIdx) =>
      buffer.io.enq.valid          := t1_valid && t1_bankMask(bankIdx)
      buffer.io.enq.bits.setIdx    := t1_setIdxVec(alignBankIdx)
      buffer.io.enq.bits.takenCtrs := t1_rotatedNewCtrs(alignBankIdx)
      buffer.io.enq.bits.wayMask   := t1_rotatedUpdateMask(alignBankIdx).asUInt
    }
  }

  XSPerfAccumulate("train_update_ctr", t1_valid && t1_updateMask.flatten.reduce(_ || _))
  XSPerfAccumulate(
    "write_buffer_drop_write",
    PopCount(writeBuffers.flatten.map(b => !b.io.enq.ready && b.io.enq.valid))
  )
}
