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
import utility.XSError
import utility.XSPerfAccumulate
import utility.XSPerfHistogram
import utility.sram.SRAMTemplate
import xiangshan.frontend.bpu.BasePredictor
import xiangshan.frontend.bpu.BasePredictorIO
import xiangshan.frontend.bpu.WriteBuffer

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
  private val sramBanks =
    Seq.tabulate(NumAlignBanks, NumInternalBanks) { (alignIdx, bankIdx) =>
      Module(new MainBtbInternalBank(alignIdx, bankIdx))
    }

  io.resetDone := sramBanks.flatMap(_.map(_.io.resetDone)).reduce(_ && _)

  private val replacer = Module(new MainBtbReplacer)

  /* predict stage 0
   * setup SRAM
   */
  private val s0_fire             = io.stageCtrl.s0_fire && io.enable
  private val s0_startVAddr       = io.startVAddr
  private val s0_thisSetIdx       = getSetIndex(s0_startVAddr)
  private val s0_nextSetIdx       = getNextSetIndex(s0_startVAddr)
  private val s0_internalBankIdx  = getInternalBankIndex(s0_startVAddr)
  private val s0_internalBankMask = UIntToOH(s0_internalBankIdx) & Fill(NumInternalBanks, s0_fire)
  private val s0_alignBankIdx     = getAlignBankIndex(s0_startVAddr)
  private val s0_setIdxVec: Vec[UInt] =
    VecInit.tabulate(NumAlignBanks)(bankIdx => Mux(bankIdx.U < s0_alignBankIdx, s0_nextSetIdx, s0_thisSetIdx))
  require(s0_thisSetIdx.getWidth == SetIdxLen, s"Set index width mismatch: ${s0_thisSetIdx.getWidth} != $SetIdxLen")
  XSError(
    s0_internalBankIdx >= NumInternalBanks.U,
    s"Invalid internal bank index: $s0_internalBankIdx, max: ${NumInternalBanks - 1}"
  )
  sramBanks zip s0_setIdxVec foreach { case (alignmentBank, setIdx) =>
    alignmentBank zip s0_internalBankMask.asBools foreach { case (internalBank, bankEnable) =>
      internalBank.io.read.req.valid       := bankEnable
      internalBank.io.read.req.bits.setIdx := setIdx
    }
  }

  /* predict stage 1
   *
   * get result from SRAM
   * rotate SRAM result
   */
  private val s1_fire                      = io.stageCtrl.s1_fire && io.enable
  private val s1_startVAddr                = RegEnable(s0_startVAddr, s0_fire)
  private val s1_internalBankMask          = RegEnable(s0_internalBankMask, s0_fire)
  private val s1_setIdxVec                 = RegEnable(s0_setIdxVec, s0_fire)
  private val s1_tag                       = getTag(s1_startVAddr)
  private val s1_alignBankIdx              = getAlignBankIndex(s1_startVAddr)
  private val s1_posHigherBitsPerAlignBank = vecRotateRight(VecInit.tabulate(NumAlignBanks)(i => i.U), s1_alignBankIdx)
  private val s1_posHigherBits             = VecInit(s1_posHigherBitsPerAlignBank.flatMap(Seq.fill(NumWay)(_)))

  private val s1_rawBtbEntries = VecInit(sramBanks.flatMap { alignBank =>
    Mux1H(s1_internalBankMask, alignBank.map(bank => bank.io.read.resp.entries))
  })

  private val s1_alignBankCrossPageMask = (0 until NumAlignBanks).map { i =>
    val currentAlignStartVAddr = getAlignedAddr(s1_startVAddr + (i * FetchBlockAlignSize).U)
    isCrossPage(s1_startVAddr, currentAlignStartVAddr)
  }
  private val s1_rotatedAlignBankCrossPageMask = vecRotateRight(VecInit(s1_alignBankCrossPageMask), s1_alignBankIdx)
  private val s1_crossPageMask                 = VecInit(s1_rotatedAlignBankCrossPageMask.flatMap(Seq.fill(NumWay)(_)))

  require(s1_alignBankIdx.getWidth == log2Ceil(NumAlignBanks))

  /* predict stage 2
   *
   * do tag compare and position compare
   * calculate target
   * map results into a per-slot vec
   * resolve multi-hit
   */
  private val s2_fire             = io.stageCtrl.s2_fire && io.enable
  private val s2_startVAddr       = RegEnable(s1_startVAddr, s1_fire)
  private val s2_setIdxVec        = RegEnable(s1_setIdxVec, s1_fire)
  private val s2_internalBankMask = RegEnable(s1_internalBankMask, s1_fire)
  private val s2_rawBtbEntries    = RegEnable(s1_rawBtbEntries, s1_fire)
  private val s2_tag              = RegEnable(s1_tag, s1_fire)
  private val s2_posHigherBits    = RegEnable(s1_posHigherBits, s1_fire)
  private val s2_crossPageMask    = RegEnable(s1_crossPageMask, s1_fire)
  private val s2_alignBankIdx     = RegEnable(s1_alignBankIdx, s1_fire)
  private val s2_positions = s2_posHigherBits.zip(s2_rawBtbEntries).map { case (h, entry) =>
    Cat(h, entry.position) // Add higher bits before using
  }
  private val s2_rawHitMask = s2_rawBtbEntries.map(entry => entry.valid && entry.tag === s2_tag)
  private val s2_hitMask = s2_rawHitMask.zip(s2_rawBtbEntries).zip(s2_crossPageMask).zipWithIndex.map {
    case (((hit, entry), isCrossPage), i) =>
      hit && !isCrossPage && (
        (i / NumWay).U =/= s2_alignBankIdx ||
          entry.position >= getAlignedInstOffset(s2_startVAddr)
      )
  }
  private val s2_targets =
    s2_rawBtbEntries.map(e =>
      getFullTarget(s2_startVAddr, e.targetLowerBits, Some(e.targetCarry))
    ) // FIXME: parameterize target carry

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
  private val s2_multiWriteAlignBankMask: Seq[Bool]    = UIntToOH(s2_isHigherAlignBank).asBools
  private val s2_multiWayIdxMask:         UInt         = UIntToOH(s2_multiHitWayIdx)
  private val s2_multiSetIdx:             UInt         = Mux1H(s2_multiWriteAlignBankMask, s2_setIdxVec)
  private val s2_multiWriteEntry:         MainBtbEntry = WireInit(0.U.asTypeOf(new MainBtbEntry))

  dontTouch(s2_multihit)
  dontTouch(s2_isHigherAlignBank)
  dontTouch(s2_multiHitWayIdx)

  io.result.hitMask    := s2_hitMask
  io.result.positions  := s2_positions
  io.result.targets    := s2_targets
  io.result.attributes := s2_rawBtbEntries.map(_.attribute)

  io.meta.hitMask            := s2_rawHitMask
  io.meta.positions          := s2_positions
  io.meta.stronglyBiasedMask := DontCare // FIXME: add bias logic
  io.meta.attributes         := s2_rawBtbEntries.map(_.attribute)

  /* training stage 0 */
  private val t0_valid = io.train.valid && io.enable
  private val t0_train = io.train.bits

  /* training stage 1 */
  private val t1_valid = RegNext(t0_valid) && io.enable
  private val t1_train = RegEnable(t0_train, t0_valid)

  private val t1_internalBankIdx  = getInternalBankIndex(t1_train.startVAddr)
  private val t1_internalBankMask = UIntToOH(t1_internalBankIdx)
  private val t1_thisSetIdx       = getSetIndex(t1_train.startVAddr)
  private val t1_nextSetIdx       = getNextSetIndex(t1_train.startVAddr)
  private val t1_alignBankIdx     = getAlignBankIndex(t1_train.startVAddr)
  private val t1_meta             = t1_train.meta.mbtb
  private val t1_setIdxVec =
    VecInit.tabulate(NumAlignBanks)(bankIdx => Mux(bankIdx.U < t1_alignBankIdx, t1_nextSetIdx, t1_thisSetIdx))

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
  sramBanks zip t1_setIdxVec zip t1_writeAlignBankMask zip s2_multiWriteAlignBankMask foreach {
    case (((alignmentBank, setIdx), alignBankEnable), multiAlignBankEnable) =>
      alignmentBank zip t1_internalBankMask.asBools zip s2_internalBankMask.asBools foreach {
        case ((bank, bankEnable), multiBankEnable) =>
          bank.io.write.req.valid        := t1_writeValid && alignBankEnable && bankEnable
          bank.io.write.req.bits.wayMask := t1_writeWayMask
          bank.io.write.req.bits.setIdx  := setIdx
          bank.io.write.req.bits.entry   := t1_writeEntry

          // flush
          bank.io.flush.req.valid :=
            s2_multihit && s2_fire && multiAlignBankEnable && multiBankEnable && !multiWriteConflict
          bank.io.flush.req.bits.wayMask := s2_multiWayIdxMask
          bank.io.flush.req.bits.setIdx  := s2_multiSetIdx
      }
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
