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

package xiangshan.frontend.bpu.mbtb

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.XSPerfAccumulate
import utility.XSPerfHistogram
import xiangshan.frontend.bpu.BasePredictor
import xiangshan.frontend.bpu.BasePredictorIO

class MainBtb(implicit p: Parameters) extends BasePredictor with HasMainBtbParameters with Helpers {
  class MainBtbIO(implicit p: Parameters) extends BasePredictorIO {
    // prediction specific bundle
    val result: MainBtbResult = Output(new MainBtbResult)
    val meta:   MainBtbMeta   = Output(new MainBtbMeta)
  }

  val io: MainBtbIO = IO(new MainBtbIO)

  /* *** submodules *** */
  private val alignBanks = Seq.tabulate(NumAlignBanks)(alignIdx => Module(new MainBtbAlignBank(alignIdx)))

  io.resetDone := alignBanks.map(_.io.resetDone).reduce(_ && _)

  private val s0_fire, s1_fire, s2_fire = Wire(Bool())
  alignBanks.foreach { b =>
    b.io.stageCtrl.s0_fire := s0_fire
    b.io.stageCtrl.s1_fire := s1_fire
    b.io.stageCtrl.s2_fire := s2_fire
    b.io.stageCtrl.s3_fire := false.B
  }

  /* *** s0 ***
   * calculate per-bank startVAddr and posHigherBits
   * send read request to alignBanks
   */
  s0_fire := io.stageCtrl.s0_fire && io.enable
  private val s0_startVAddr        = io.startVAddr
  private val s0_firstAlignBankIdx = getAlignBankIndex(s0_startVAddr)
  private val s0_startVAddrVec = vecRotateRight(
    VecInit.tabulate(NumAlignBanks) { i =>
      if (i == 0)
        s0_startVAddr // keep lower bits for the first one
      else
        getAlignedAddr(s0_startVAddr + (i << FetchBlockAlignWidth).U) // use aligned for others
    },
    s0_firstAlignBankIdx
  )
  private val s0_posHigherBitsVec = vecRotateRight(
    VecInit.tabulate(NumAlignBanks)(i => i.U(AlignBankIdxLen.W)),
    s0_firstAlignBankIdx
  )

  alignBanks.zipWithIndex.foreach { case (b, i) =>
    b.io.read.req.startVAddr    := s0_startVAddrVec(i)
    b.io.read.req.posHigherBits := s0_posHigherBitsVec(i)
    b.io.read.req.crossPage     := isCrossPage(s0_startVAddrVec(i), s0_startVAddr)
  }

  /* *** s1 ***
   * just wait alignBanks
   */
  s1_fire := io.stageCtrl.s1_fire && io.enable

  /* *** s2 ***
   * receive read response from alignBanks
   * send out prediction result and meta info
   */
  s2_fire := io.stageCtrl.s2_fire && io.enable

  private val s2_rawHitMask = VecInit(alignBanks.flatMap(_.io.read.resp.map(_.rawHit)))
  private val s2_hitMask    = VecInit(alignBanks.flatMap(_.io.read.resp.map(_.hit)))
  private val s2_positions  = VecInit(alignBanks.flatMap(_.io.read.resp.map(_.position)))
  private val s2_targets    = VecInit(alignBanks.flatMap(_.io.read.resp.map(_.target)))
  private val s2_attributes = VecInit(alignBanks.flatMap(_.io.read.resp.map(_.attribute)))

  io.result.hitMask    := s2_hitMask
  io.result.positions  := s2_positions
  io.result.targets    := s2_targets
  io.result.attributes := s2_attributes

  io.meta.hitMask            := s2_rawHitMask
  io.meta.positions          := s2_positions
  io.meta.stronglyBiasedMask := DontCare // FIXME: add bias logic
  io.meta.attributes         := s2_attributes

  /* *** t0 ***
   * receive training data and latch
   */
  private val t0_valid = io.train.valid && io.enable
  private val t0_train = io.train.bits

  /* *** t1 ***
   * calculate write data and write to alignBanks
   */
  private val t1_valid = RegNext(t0_valid) && io.enable
  private val t1_train = RegEnable(t0_train, t0_valid)

  private val t1_startVAddr        = t1_train.startVAddr
  private val t1_firstAlignBankIdx = getAlignBankIndex(t1_startVAddr)
  private val t1_startVAddrVec = vecRotateRight(
    VecInit.tabulate(NumAlignBanks)(i => getAlignedAddr(t1_startVAddr + (i << FetchBlockAlignWidth).U)),
    t1_firstAlignBankIdx
  )
  private val t1_meta             = t1_train.meta.mbtb
  private val t1_mispredictBranch = t1_train.mispredictBranch

  private val t1_hitMispredictBranch = t1_meta.hitMask.zip(t1_meta.positions).zip(t1_meta.attributes).map {
    case ((hit, position), attribute) =>
      hit && position === t1_mispredictBranch.bits.cfiPosition && attribute === t1_mispredictBranch.bits.attribute
  }.reduce(_ || _)

  private val t1_writeValid = t1_valid && t1_mispredictBranch.valid && !t1_hitMispredictBranch

  private val t1_writeEntry = Wire(new MainBtbEntry)
  t1_writeEntry.valid           := true.B
  t1_writeEntry.tag             := getTag(t1_train.startVAddr)
  t1_writeEntry.position        := t1_mispredictBranch.bits.cfiPosition
  t1_writeEntry.targetLowerBits := getTargetLowerBits(t1_mispredictBranch.bits.target)
  t1_writeEntry.targetCarry     := getTargetCarry(t1_train.startVAddr, t1_mispredictBranch.bits.target)
  t1_writeEntry.attribute       := t1_mispredictBranch.bits.attribute
  t1_writeEntry.stronglyBiased  := false.B  // FIXME
  t1_writeEntry.replaceCnt      := DontCare // FIXME:

  private val t1_writeAlignBankIdx = getAlignBankIndexFromPosition(t1_mispredictBranch.bits.cfiPosition)
  private val t1_writeAlignBankMask = vecRotateRight(
    VecInit(UIntToOH(t1_writeAlignBankIdx).asBools),
    t1_firstAlignBankIdx
  )

  alignBanks.zipWithIndex.foreach { case (b, i) =>
    b.io.write.req.valid           := t1_writeValid && t1_writeAlignBankMask(i)
    b.io.write.req.bits.startVAddr := t1_startVAddrVec(i)
    b.io.write.req.bits.entry      := t1_writeEntry
  }

  /* *** statistics *** */
  XSPerfAccumulate("total_train", t1_valid)
  XSPerfAccumulate("pred_hit", s2_fire && s2_hitMask.reduce(_ || _))
  XSPerfHistogram("pred_hit_count", PopCount(s2_hitMask), s2_fire, 0, NumWay * NumAlignBanks)
  XSPerfAccumulate("train_write_new_entry", t1_writeValid)
  XSPerfAccumulate("train_has_mispredict", t1_valid && t1_mispredictBranch.valid)
  XSPerfAccumulate("train_hit_mispredict", t1_valid && t1_mispredictBranch.valid && t1_hitMispredictBranch)
}
