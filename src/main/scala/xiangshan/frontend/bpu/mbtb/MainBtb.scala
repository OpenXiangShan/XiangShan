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

  /* *** internal parameters *** */
  private val Alignment = FetchBlockSize / NumAlignBanks

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

  // TODO: move replacer to AlignBank
  private val replacer = Module(new MainBtbReplacer)

  /* predict stage 0
   * setup SRAM
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

  /* predict stage 1
   *
   * get result from SRAM
   * rotate SRAM result
   */
  s1_fire := io.stageCtrl.s1_fire && io.enable
  private val s1_startVAddr = RegEnable(s0_startVAddr, s0_fire)

  /* predict stage 2
   *
   * do tag compare and position compare
   * calculate target
   * map results into a per-slot vec
   * resolve multi-hit
   */
  s2_fire := io.stageCtrl.s2_fire && io.enable
  private val s2_startVAddr   = RegEnable(s1_startVAddr, s1_fire)
  private val s2_alignBankIdx = getAlignBankIndex(s2_startVAddr)
  // FIXME
  private val s2_internalBankMask = UIntToOH(getInternalBankIndex(s2_startVAddr))

  private val s2_rawHitMask = VecInit(alignBanks.flatMap(_.io.read.resp.map(_.rawHit)))
  private val s2_hitMask    = VecInit(alignBanks.flatMap(_.io.read.resp.map(_.hit)))
  private val s2_positions  = VecInit(alignBanks.flatMap(_.io.read.resp.map(_.position)))
  private val s2_targets    = VecInit(alignBanks.flatMap(_.io.read.resp.map(_.target)))
  private val s2_attributes = VecInit(alignBanks.flatMap(_.io.read.resp.map(_.attribute)))

  private val s2_thisReplacerSetIdx = getReplacerSetIndex(s2_startVAddr)
  private val s2_nextReplacerSetIdx = getNextReplacerSetIndex(s2_startVAddr)
  private val s2_replacerSetIdxVec: Vec[UInt] = VecInit.tabulate(NumAlignBanks)(bankIdx =>
    Mux(bankIdx.U < s2_alignBankIdx, s2_nextReplacerSetIdx, s2_thisReplacerSetIdx)
  )

  private val s2_stateTouchs: Vec[Vec[Valid[UInt]]] =
    Wire(Vec(NumAlignBanks, Vec(NumWay, Valid(UInt(log2Up(NumWay).W)))))
  // FIXME: this is not a good way to do this, but it works for now
  for (alignIdx <- 0 until NumAlignBanks; wayIdx <- 0 until NumWay) {
    s2_stateTouchs(alignIdx)(wayIdx).valid := s2_fire && s2_hitMask(alignIdx * NumWay + wayIdx)
    s2_stateTouchs(alignIdx)(wayIdx).bits  := wayIdx.U
  }
  replacer.io.predictionSetIndxVec := s2_replacerSetIdxVec
  replacer.io.predictionTouchWays  := s2_stateTouchs
  replacer.io.predictionHitMask    := VecInit(s2_stateTouchs.map(_.map(_.valid).reduce(_ || _) && s2_fire))

  dontTouch(s2_replacerSetIdxVec)
  dontTouch(s2_stateTouchs)
  // dontTouch(s2_nextState)

  private val (s2_multihit, s2_isHigherAlignBank, s2_multiHitWayIdx, s2_multiHitMask) =
    detectMultiHit(s2_hitMask, s2_positions)
  private val s2_multiWriteAlignBankMask: Seq[Bool] = UIntToOH(s2_isHigherAlignBank).asBools
  private val s2_multiWayIdxMask:         UInt      = UIntToOH(s2_multiHitWayIdx)

  dontTouch(s2_multihit)
  dontTouch(s2_isHigherAlignBank)
  dontTouch(s2_multiHitWayIdx)

  io.result.hitMask    := s2_hitMask
  io.result.positions  := s2_positions
  io.result.targets    := s2_targets
  io.result.attributes := s2_attributes

  io.meta.hitMask            := s2_rawHitMask
  io.meta.positions          := s2_positions
  io.meta.stronglyBiasedMask := DontCare // FIXME: add bias logic
  io.meta.attributes         := s2_attributes

  /* training stage 0 */
  private val t0_valid = io.train.valid && io.enable
  private val t0_train = io.train.bits

  /* training stage 1 */
  private val t1_valid = RegNext(t0_valid) && io.enable
  private val t1_train = RegEnable(t0_train, t0_valid)
  private val t1_startVAddrVec = vecRotateRight(
    VecInit.tabulate(NumAlignBanks)(i => getAlignedAddr(t1_train.startVAddr + (i << FetchBlockAlignWidth).U)),
    getAlignBankIndex(t1_train.startVAddr)
  )

  private val t1_internalBankIdx  = getInternalBankIndex(t1_train.startVAddr)
  private val t1_internalBankMask = UIntToOH(t1_internalBankIdx)
  private val t1_alignBankIdx     = getAlignBankIndex(t1_train.startVAddr)
  private val t1_meta             = t1_train.meta.mbtb

  private val t1_mispredictBranch = t1_train.mispredictBranch

  private val t1_hitMispredictBranch = t1_meta.hitMask.zip(t1_meta.positions).zip(t1_meta.attributes).map {
    case ((hit, position), attribute) =>
      hit && position === t1_mispredictBranch.bits.cfiPosition && attribute === t1_mispredictBranch.bits.attribute
  }.reduce(_ || _)

  private val t1_writeValid = t1_valid && t1_mispredictBranch.valid && !t1_hitMispredictBranch

  private val t1_writeEntry = Wire(new MainBtbEntry)
  t1_writeEntry.valid           := true.B   // FIXME: invalidate
  t1_writeEntry.tag             := getTag(t1_train.startVAddr)
  t1_writeEntry.position        := t1_mispredictBranch.bits.cfiPosition
  t1_writeEntry.targetLowerBits := getTargetLowerBits(t1_mispredictBranch.bits.target)
  t1_writeEntry.targetCarry     := getTargetCarry(t1_train.startVAddr, t1_mispredictBranch.bits.target)
  t1_writeEntry.attribute       := t1_mispredictBranch.bits.attribute
  t1_writeEntry.stronglyBiased  := false.B  // FIXME
  t1_writeEntry.replaceCnt      := DontCare // FIXME:

  private val t1_writeAlignBankIdx =
    t1_mispredictBranch.bits.cfiPosition(CfiPositionWidth - 1, CfiPositionWidth - log2Ceil(NumAlignBanks))
  private val t1_rawWriteAlignBankMask = VecInit(UIntToOH(t1_writeAlignBankIdx).asBools)
  private val t1_writeAlignBankMask    = vecRotateRight(t1_rawWriteAlignBankMask, t1_alignBankIdx)

  private val t1_thisReplacerSetIdx = getReplacerSetIndex(t1_train.startVAddr)
  private val t1_nextReplacerSetIdx = getNextReplacerSetIndex(t1_train.startVAddr)
  private val t1_replacerSetIdxVec: Vec[UInt] = VecInit.tabulate(NumAlignBanks)(bankIdx =>
    Mux(bankIdx.U < t1_alignBankIdx, t1_nextReplacerSetIdx, t1_thisReplacerSetIdx)
  )
  private val t1_replacerSetIdx = Mux1H(t1_writeAlignBankMask, t1_replacerSetIdxVec)
  private val t1_replacerBankMask: Vec[Bool] = t1_writeAlignBankMask
  replacer.io.trainWriteValid    := t1_writeValid
  replacer.io.trainSetIndx       := t1_replacerSetIdx
  replacer.io.trainAlignBankMask := t1_writeAlignBankMask

  dontTouch(t1_replacerSetIdxVec)
  dontTouch(t1_replacerSetIdx)
  dontTouch(t1_writeAlignBankMask)
  private val t1_writeWayMask = UIntToOH(replacer.io.victimWayIdx)
  require(t1_writeWayMask.getWidth == NumWay, s"Write way mask width mismatch: ${t1_writeWayMask.getWidth} != $NumWay")

  private val multiWriteConflict = s2_multihit && s2_fire && t1_writeValid &&
    (s2_multiWriteAlignBankMask zip t1_writeAlignBankMask map { case (a, b) => a && b }).reduce(_ || _) &&
    (s2_internalBankMask.asBools zip t1_internalBankMask.asBools map { case (a, b) => a && b }).reduce(_ || _)

  // Write to SRAM
  alignBanks zip t1_startVAddrVec zip t1_writeAlignBankMask zip s2_multiWriteAlignBankMask foreach {
    case (((alignBank, startVAddr), alignBankEnable), multiAlignBankEnable) =>
      alignBank.io.write.req.valid           := t1_writeValid && alignBankEnable
      alignBank.io.write.req.bits.startVAddr := startVAddr
      alignBank.io.write.req.bits.wayMask    := t1_writeWayMask
      alignBank.io.write.req.bits.entry      := t1_writeEntry

      alignBank.io.flush.req.valid := s2_multihit && s2_fire && multiAlignBankEnable && !multiWriteConflict
      alignBank.io.flush.req.bits.internalBankMask := s2_internalBankMask
      alignBank.io.flush.req.bits.wayMask          := s2_multiWayIdxMask
  }

  dontTouch(t1_writeValid)
  dontTouch(t1_writeAlignBankMask)
  dontTouch(t1_internalBankMask)
  dontTouch(t1_writeWayMask)

  /* ** statistics ** */

  XSPerfAccumulate("total_train", t1_valid)
  XSPerfAccumulate("pred_hit", s2_fire && s2_hitMask.reduce(_ || _))
  XSPerfHistogram("pred_hit_count", PopCount(s2_hitMask), s2_fire, 0, NumWay * NumAlignBanks)
  XSPerfAccumulate("train_write_new_entry", t1_writeValid)
  XSPerfAccumulate("train_has_mispredict", t1_valid && t1_mispredictBranch.valid)
  XSPerfAccumulate("train_hit_mispredict", t1_valid && t1_mispredictBranch.valid && t1_hitMispredictBranch)
  XSPerfAccumulate("multihit_write_conflict", multiWriteConflict)
  XSPerfHistogram("multihit_count", PopCount(s2_multiHitMask), s2_fire, 0, NumWay * NumAlignBanks)
}
