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
    val startVAddr:   PrunedAddr           = Input(PrunedAddr(VAddrBits))
    val takenCtrs:    Vec[SaturateCounter] = Output(Vec(FetchBlockInstNum, new SaturateCounter(BaseTableTakenCtrWidth)))
    val train:        Valid[BpuTrain]      = Input(Valid(new BpuTrain))
    val resetDone:    Bool                 = Output(Bool())
  }
  val io: TageBaseTableIO = IO(new TageBaseTableIO)

  private val alignBanks = Seq.tabulate(BaseTableNumAlignBanks) { alignIdx =>
    Module(new TageBaseTableAlignBank(alignIdx))
  }

  io.resetDone := alignBanks.map(_.io.resetDone).reduce(_ && _)

  /* --------------------------------------------------------------------------------------------------------------
     stage 0
     - send read request to SRAM
     -------------------------------------------------------------------------------------------------------------- */

  private val s0_fire              = io.readReqValid
  private val s0_startVAddr        = io.startVAddr
  private val s0_firstAlignBankIdx = getBaseTableAlignBankIndex(s0_startVAddr)
  private val s0_startVAddrVec = vecRotateRight(
    VecInit.tabulate(BaseTableNumAlignBanks)(i => getAlignedAddr(s0_startVAddr + (i << FetchBlockAlignWidth).U)),
    s0_firstAlignBankIdx
  )

  alignBanks.zipWithIndex.foreach { case (b, i) =>
    b.io.read.req.valid           := s0_fire
    b.io.read.req.bits.startVAddr := s0_startVAddrVec(i)
  }

  /* --------------------------------------------------------------------------------------------------------------
     stage 1
     - get raw ctrs from SRAM
     - rotate ctrs
     -------------------------------------------------------------------------------------------------------------- */

  private val s1_firstAlignBankIdx = RegEnable(s0_firstAlignBankIdx, s0_fire)
  private val s1_rawCtrs           = VecInit(alignBanks.map(_.io.read.resp.takenCtrs))

  /*
   * rotate ctrs
   * for example, if BaseTableNumAlignBanks = 2, alignBankIdx = 1,
   * then io.ctrs := s1_rawCtrs(1) ++ s1_rawCtrs(0)
   * if BaseTableNumAlignBanks = 4, alignBankIdx = 1,
   * then io.ctrs := s1_rawCtrs(1) ++ s1_rawCtrs(2) ++ s1_rawCtrs(3) ++ s1_rawCtrs(0)
   */
  // FIXME: maybe rotateLeft? Not sure. But anyway results are the same for NumAlignBanks = 2
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

  private val t1_startVAddr        = t1_train.startVAddr
  private val t1_branches          = t1_train.branches
  private val t1_oldCtrs           = t1_train.meta.tage.baseTableCtrs
  private val t1_firstAlignBankIdx = getBaseTableAlignBankIndex(t1_startVAddr)
  private val t1_startVAddrVec = vecRotateRight(
    VecInit.tabulate(BaseTableNumAlignBanks)(i => getAlignedAddr(t1_startVAddr + (i << FetchBlockAlignWidth).U)),
    t1_firstAlignBankIdx
  )

  private val t1_updateMaskVec = Wire(Vec(BaseTableNumAlignBanks, Vec(FetchBlockAlignInstNum, Bool())))
  private val t1_newCtrsVec =
    Wire(Vec(BaseTableNumAlignBanks, Vec(FetchBlockAlignInstNum, new SaturateCounter(BaseTableTakenCtrWidth))))

  t1_newCtrsVec.flatten.zip(t1_updateMaskVec.flatten).zipWithIndex.foreach { case ((newCtr, needUpdate), position) =>
    val hitMask = t1_branches.map { branch =>
      branch.valid && branch.bits.attribute.isConditional && position.U === branch.bits.cfiPosition
    }
    val taken = Mux1H(hitMask, t1_branches.map(_.bits.taken))
    needUpdate   := hitMask.reduce(_ || _)
    newCtr.value := t1_oldCtrs(position).getUpdate(taken)
  }

  private val t1_rotatedNewCtrsVec    = vecRotateRight(t1_newCtrsVec, t1_firstAlignBankIdx)
  private val t1_rotatedUpdateMaskVec = vecRotateRight(t1_updateMaskVec, t1_firstAlignBankIdx)

  alignBanks.zipWithIndex.foreach { case (b, i) =>
    b.io.write.req.valid           := t1_valid
    b.io.write.req.bits.startVAddr := t1_startVAddrVec(i)
    b.io.write.req.bits.wayMask    := t1_rotatedUpdateMaskVec(i).asUInt
    b.io.write.req.bits.takenCtrs  := t1_rotatedNewCtrsVec(i)
  }
}
