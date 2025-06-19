// Copyright (c) 2024-2025 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2025 Institute of Computing Technology, Chinese Academy of Sciences
// Copyright (c) 2020-2021 Peng Cheng Laboratory
//
// XiangShan is licensed under Mulan PSL v2.
// You can use this software according to the terms and conditions of the Mulan PSL v2.
// You may obtain a copy of Mulan PSL v2 at: https://license.coscl.org.cn/MulanPSL2
//
// THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
// EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
// MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
//
// See the Mulan PSL v2 for more details.

package xiangshan.frontend.bpu.abtb

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.XSModule
import xiangshan.frontend.bpu.SaturateCounter

/**
 * This module is the implementation of the ahead BTB (Branch Target Buffer).
 */
class AheadBtb(implicit p: Parameters) extends XSModule with HasAheadBtbParams with AheadBtbHelpers {
  val io = IO(new AheadBtbIO)

  val aBtbBanks    = Seq.fill(NumBanks)(Module(new AheadBtbBank))
  val takenCounter = VecInit.fill(NumSets)(VecInit.fill(NumWays)(Reg(new SaturateCounter(width = TakenCounterWidth))))

  val s0_fire = Wire(Bool())
  val s1_fire = Wire(Bool())
  val s2_fire = Wire(Bool())

  val s1_ready = Wire(Bool())
  val s2_ready = Wire(Bool())

  val s1_valid = RegInit(false.B)
  val s2_valid = RegInit(false.B)

  s0_fire := io.fromBpu.startPc.valid
  s1_fire := s1_valid && s2_ready && io.fromBpu.startPc.valid
  s2_fire := s2_valid

  dontTouch(s0_fire)
  dontTouch(s1_fire)
  dontTouch(s2_fire)

  s1_ready := s1_fire || !s1_valid
  s2_ready := s2_fire || !s2_valid

  dontTouch(s1_ready)
  dontTouch(s2_ready)

  val redirectValid = io.fromBpu.redirectValid

  when(s0_fire)(s1_valid := true.B)
    .elsewhen(redirectValid)(s1_valid := false.B)
    .elsewhen(s1_fire)(s1_valid := false.B)

  when(redirectValid)(s2_valid := false.B)
    .elsewhen(s1_fire)(s2_valid := !redirectValid)
    .elsewhen(s2_fire)(s2_valid := false.B)

  // ----------------------------------------------------------------------------------------
  // predict pipeline stage 0
  // - get set index and bank index
  // - send read request to selected bank
  // ----------------------------------------------------------------------------------------

  val s0_setIdx   = getSetIndex(io.fromBpu.startPc.bits)
  val s0_bankIdx  = getBankIndex(io.fromBpu.startPc.bits)
  val s0_bankMask = UIntToOH(s0_bankIdx)

  aBtbBanks.zipWithIndex.foreach { case (bank, i) =>
    bank.io.readReq.valid       := io.fromBpu.startPc.valid && s0_bankMask(i)
    bank.io.readReq.bits.setIdx := s0_setIdx
  }

  // ----------------------------------------------------------------------------------------
  // predict pipeline stage 1
  // - get taken counter result
  // - get entries from bank
  // ----------------------------------------------------------------------------------------

  val debug_s1_pc = RegEnable(io.fromBpu.startPc.bits, s0_fire)
  dontTouch(debug_s1_pc)

  val s1_setIdx        = RegEnable(s0_setIdx, s0_fire)
  val s1_bankMask      = RegEnable(s0_bankMask, s0_fire)
  val s1_counterResult = VecInit(takenCounter(s1_setIdx).map(_.isPositive))
  val s1_entries       = Mux1H(s1_bankMask, aBtbBanks.map(_.io.readResp.entries))
  val s1_newStartPc    = io.fromBpu.startPc.bits

  // ----------------------------------------------------------------------------------------
  // predict pipeline stage 2
  // - compare tag and get taken mask
  // - compare positions and find first taken entry
  // - get target from found entry
  // - output prediction
  // ----------------------------------------------------------------------------------------

  val debug_s2_pc = RegEnable(debug_s1_pc, s1_fire)
  dontTouch(debug_s2_pc)

  val s2_entries       = RegEnable(s1_entries, s1_fire)
  val s2_entriesDelay1 = RegNext(s2_entries)
  val s2_newStartPc    = RegEnable(s1_newStartPc, s1_fire)
  val s2_counterResult = RegEnable(s1_counterResult, s1_fire)

  val s2_tag         = getTag(s2_newStartPc)
  val s2_realEntries = Mux(RegNext(io.fromBpu.bpuOverride), s2_entriesDelay1, s2_entries)
  val s2_hitMask     = getHitMask(s2_realEntries, s2_tag)
  val s2_hit         = s2_hitMask.reduce(_ || _)
  val s2_takenMask   = getTakenMask(s2_realEntries, s2_hitMask, s2_counterResult)
  val s2_taken       = s2_takenMask.reduce(_ || _)

  val (s2_takenEntry, s2_takenWayIdx) = getFirstTakenEntry(s2_realEntries, s2_takenMask)

  val s2_takenPosition = Mux(s2_taken, s2_takenEntry.position, (PredictWidth - 1).U)
  val s2_target        = getTarget(s2_taken, s2_takenEntry, s2_newStartPc)

  val s2_prediction = Wire(new Prediction)
  s2_prediction.startPc             := s2_newStartPc
  s2_prediction.target              := s2_target
  s2_prediction.takenPosition.valid := s2_taken
  s2_prediction.takenPosition.bits  := s2_takenPosition
  s2_prediction.fallThroughError    := false.B // FIXME

  dontTouch(s2_prediction)

  val s2_dummyPrediction = Wire(new Prediction)
  s2_dummyPrediction.startPc             := s2_newStartPc
  s2_dummyPrediction.target              := getAlignedPc(s2_newStartPc) + FetchBlockSize.U
  s2_dummyPrediction.takenPosition.valid := false.B
  s2_dummyPrediction.takenPosition.bits  := (PredictWidth - 1).U
  s2_dummyPrediction.fallThroughError    := false.B

  dontTouch(s2_dummyPrediction)

  io.toBpu.prediction.valid := s2_valid           // FIXME
  io.toBpu.prediction.bits  := s2_dummyPrediction // FIXME

//  val meta = Wire(new AheadBtbMeta)
//  meta.hit           := s2_hit
//  meta.hitMask       := s2_hitMask
//  meta.taken         := s2_taken
//  meta.takenMask     := s2_takenMask
//  meta.takenWayIdx   := s2_takenWayIdx
//  meta.takenPosition := s2_takenPosition
//
  aBtbBanks.foreach(_.io.readWayIdx := s2_takenWayIdx) // TODO: when not hit?

  // ----------------------------------------------------------------------------------------
  // update
  // -
  // -
  // ----------------------------------------------------------------------------------------

//  val previousUpdateValid = RegInit(false.B)
//  val previousUpdate      = Reg(new AheadBtbUpdateBundle)
//
//  when(io.in.update.valid) {
//    previousUpdateValid := true.B
//    previousUpdate      := io.in.update.bits
//  }
//
//  val currentUpdateValid = io.in.update.valid
//  val currentUpdate      = io.in.update.bits
//  val updateMeta         = io.in.update.bits.meta
//
//  // FIXME: when first wrong, meta is invalid, how to solve it? just don't update for now
//  val updateValid = previousUpdateValid && currentUpdateValid && !previousUpdate.hasMispredict
//
//  // FIXME: temporarily don't read when not hit
//  val updateNeedRead = !currentUpdate.meta.hit
//
//  val updateSetIdx  = getSetIndex(currentUpdate.startPc)
//  val updateHitMask = currentUpdate.meta.hitMask
//
//  val firstMispredictBranchPosition = PriorityEncoder(currentUpdate.mispredictMask)
//
//  val hitMispredictBranchMask = VecInit(updateMeta.positions zip updateMeta.hitMask map { case (pos, hit) =>
//    pos === firstMispredictBranchPosition && hit
//  })
//
//  val hitCfiMask = VecInit(updateMeta.positions zip updateMeta.hitMask map { case (pos, hit) =>
//    pos === currentUpdate.cfiPosition.bits && hit
//  })
//
//  val hitCfi = hitCfiMask.reduce(_ || _)
//
//  val hitCfiWayIdx = PriorityEncoder(hitCfiMask)
//
//  val hasDirectionMispred = hitCfi && updateMeta.takenMask(hitCfiWayIdx) ^ currentUpdate.cfiPosition.valid
//
//  updateTakenCounter(
//    takenCounter,
//    updateSetIdx,
//    updateMeta.positions,
//    updateMeta.hitMask,
//    hasDirectionMispred,
//    hitCfiWayIdx,
//    currentUpdate.cfiPosition.valid
//  )
//
//  val needInvalidateEntry =
//    updateValid && currentUpdate.hasMispredict && !currentUpdate.cfiPosition.valid && currentUpdate.notCfiMask(
//      updateMeta.takenPosition
//    )
//
//  val needWriteNewEntry = updateValid && currentUpdate.hasMispredict && currentUpdate.cfiPosition.valid && !hitCfi
//
//  val needModifyEntry =
//    updateValid && currentUpdate.hasMispredict && currentUpdate.cfiPosition.valid && !hasDirectionMispred
//
//  val writeEntry = Wire(new AheadBtbEntry)
//  writeEntry.valid           := Mux(needInvalidateEntry, false.B, true.B)
//  writeEntry.tag             := getTag(currentUpdate.startPc)
//  writeEntry.targetLowerBits := getTargetLowerBits(currentUpdate.target)
//  writeEntry.targetState     := getTargetState(currentUpdate.startPc, currentUpdate.target)
//  writeEntry.position        := currentUpdate.cfiPosition.bits
//  writeEntry.branchAttribute := currentUpdate.cfiAttribute
//
//  val writeBankValid = needWriteNewEntry || needModifyEntry || needInvalidateEntry
//  val writeBankIdx   = getBankIndex(previousUpdate.startPc)
//  val writeBankMask  = UIntToOH(writeBankIdx)
//
//  aBtbBanks.zipWithIndex.foreach { case (bank, i) =>
//    bank.io.writeReq.valid            := writeBankValid && writeBankMask(i)
//    bank.io.writeReq.bits.isNewEntry  := needWriteNewEntry
//    bank.io.writeReq.bits.setIdx      := updateSetIdx
//    bank.io.writeReq.bits.writeWayIdx := updateMeta.takenWayIdx
//    bank.io.writeReq.bits.entry       := writeEntry
//  }

  aBtbBanks.zipWithIndex.foreach { case (bank, i) =>
    bank.io.writeReq.valid            := false.B
    bank.io.writeReq.bits.isNewEntry  := false.B
    bank.io.writeReq.bits.setIdx      := 0.U
    bank.io.writeReq.bits.writeWayIdx := 0.U
    bank.io.writeReq.bits.entry       := 0.U.asTypeOf(new AheadBtbEntry)
  }

}
