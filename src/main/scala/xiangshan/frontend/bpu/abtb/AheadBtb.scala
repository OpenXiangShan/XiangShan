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

package xiangshan.frontend.bpu.abtb

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.BasePredictor
import xiangshan.frontend.bpu.BasePredictorIO
import xiangshan.frontend.bpu.BranchPrediction
import xiangshan.frontend.bpu.SaturateCounter

/**
 * This module is the implementation of the ahead BTB (Branch Target Buffer).
 */
class AheadBtb(implicit p: Parameters) extends BasePredictor with HasAheadBtbParams with Helpers {
  class AheadBtbIO(implicit p: Parameters) extends BasePredictorIO {
    // Input
    val redirectValid: Bool                  = Input(Bool())
    val overrideValid: Bool                  = Input(Bool())
    val update:        Valid[AheadBtbUpdate] = Flipped(Valid(new AheadBtbUpdate))
    // Output
    val meta:             AheadBtbMeta = Output(new AheadBtbMeta)
    val debug_startVaddr: PrunedAddr   = Output(PrunedAddr(VAddrBits))
  }
  val io: AheadBtbIO = IO(new AheadBtbIO)

  private val aBtbBanks = Seq.fill(NumBanks)(Module(new AheadBtbBank))
  private val takenCounter = RegInit(VecInit.fill(NumSets)(VecInit.fill(NumWays)(
    (1 << (TakenCounterWidth - 1)).U.asTypeOf(new SaturateCounter(TakenCounterWidth))
  )))

  // TODO: add usefulCtr for entry
  // TODO: write ctr bypass to read
  // TODO: invliadate multi-hit entry

  private val s0_fire = Wire(Bool())
  private val s1_fire = Wire(Bool())
  private val s2_fire = Wire(Bool())

  private val s1_ready = Wire(Bool())
  private val s2_ready = Wire(Bool())

  private val s1_valid = RegInit(false.B)
  private val s2_valid = RegInit(false.B)

  private val predictReqValid = io.stageCtrl.s0_fire && io.enable

  s0_fire := predictReqValid
  s1_fire := s1_valid && s2_ready && predictReqValid
  s2_fire := s2_valid

  dontTouch(s0_fire)
  dontTouch(s1_fire)
  dontTouch(s2_fire)

  s1_ready := s1_fire || !s1_valid
  s2_ready := s2_fire || !s2_valid

  dontTouch(s1_ready)
  dontTouch(s2_ready)

  private val redirectValid = io.redirectValid

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

  private val s0_setIdx   = getSetIndex(io.startVAddr)
  private val s0_bankIdx  = getBankIndex(io.startVAddr)
  private val s0_bankMask = UIntToOH(s0_bankIdx)

  aBtbBanks.zipWithIndex.foreach { case (bank, i) =>
    bank.io.readReq.valid       := predictReqValid && s0_bankMask(i)
    bank.io.readReq.bits.setIdx := s0_setIdx
  }

  // ----------------------------------------------------------------------------------------
  // predict pipeline stage 1
  // - get entries from bank
  // - get the latest start pc for compare tag in s2
  // ----------------------------------------------------------------------------------------

  private val debug_s1_pc = RegEnable(io.startVAddr, s0_fire)
  dontTouch(debug_s1_pc)

  private val s1_setIdx     = RegEnable(s0_setIdx, s0_fire)
  private val s1_bankMask   = RegEnable(s0_bankMask, s0_fire)
  private val s1_entries    = Mux1H(s1_bankMask, aBtbBanks.map(_.io.readResp.entries))
  private val s1_newStartPc = io.startVAddr

  // ----------------------------------------------------------------------------------------
  // predict pipeline stage 2
  // - get taken counter result
  // - compare tag and get taken mask
  // - compare positions and find first taken entry
  // - get target from found entry
  // - output prediction
  // ----------------------------------------------------------------------------------------

  private val debug_s2_pc = RegEnable(debug_s1_pc, s1_fire)
  dontTouch(debug_s2_pc)

  private val s2_setIdx        = RegEnable(s1_setIdx, s0_fire)
  private val s2_entries       = RegEnable(s1_entries, s1_fire)
  private val s2_entriesDelay1 = RegNext(s2_entries)
  private val s2_newStartPc    = RegEnable(s1_newStartPc, s1_fire)
  private val s2_counterResult = VecInit(takenCounter(s2_setIdx).map(_.isPositive))

  private val s2_tag         = getTag(s2_newStartPc)
  private val s2_realEntries = Mux(RegNext(io.overrideValid), s2_entriesDelay1, s2_entries)
  private val s2_hitMask     = getHitMask(s2_realEntries, s2_tag)
  private val s2_takenMask   = getTakenMask(s2_realEntries, s2_hitMask, s2_counterResult)
  private val s2_taken       = s2_takenMask.reduce(_ || _)

  private val (s2_takenEntry, s2_takenWayIdx) = getFirstTakenEntry(s2_realEntries, s2_takenMask)

  private val s2_takenPosition = Mux(s2_taken, s2_takenEntry.position, (PredictWidth - 1).U)
  private val s2_target        = getTarget(s2_takenEntry, s2_newStartPc)

  private val s2_prediction = Wire(new BranchPrediction)
  s2_prediction.taken       := s2_valid & s2_taken
  s2_prediction.cfiPosition := s2_takenPosition
  s2_prediction.target      := s2_target
  s2_prediction.attribute   := s2_takenEntry.attribute
  dontTouch(s2_prediction)

  io.prediction := s2_prediction
  io.hit        := s2_hitMask.reduce(_ || _)

  io.debug_startVaddr := s2_newStartPc

  private val meta = Wire(new AheadBtbMeta)
  meta.valid         := s2_valid
  meta.taken         := s2_taken
  meta.takenPosition := s2_takenPosition
  meta.takenWayIdx   := s2_takenWayIdx
  meta.hitMask       := s2_hitMask
  meta.positions     := s2_realEntries.map(_.position)

  io.meta := meta

  aBtbBanks.foreach(_.io.readWayIdx := s2_takenWayIdx) // TODO: reconsider the readWayIdx

  // ----------------------------------------------------------------------------------------
  // update
  // -
  // -
  // ----------------------------------------------------------------------------------------

  private val previousUpdateValid = RegInit(false.B)
  private val previousUpdate      = Reg(new AheadBtbUpdate)

  // delay one cycle for better timing
  private val currentUpdateValid = RegNext(io.update.valid)
  private val currentUpdate      = RegEnable(io.update.bits, io.update.valid)

  when(currentUpdateValid) {
    previousUpdateValid := true.B
    previousUpdate      := currentUpdate
  }

  // FIXME: when first wrong, meta is invalid, how to solve it? just don't update for now
  private val updateValid = previousUpdateValid && currentUpdateValid &&
    !previousUpdate.hasMispredict && currentUpdate.aBtbMeta.valid
  private val updateMeta   = currentUpdate.aBtbMeta
  private val updateSetIdx = getSetIndex(previousUpdate.startVAddr)

  private val predictTaken         = updateMeta.taken
  private val predictTakenPosition = updateMeta.takenPosition
  private val predictTakenWayIdx   = updateMeta.takenWayIdx

  private val actualTaken         = currentUpdate.taken
  private val actualTakenPosition = currentUpdate.cfiPosition

  private val hitTakenBranch = VecInit(updateMeta.hitMask.zip(updateMeta.positions).map { case (hit, pos) =>
    hit && pos === actualTakenPosition && actualTaken
  }).reduce(_ || _)

  def updateCounter(cond: UInt => Bool, action: SaturateCounter => Unit): Unit =
    takenCounter(updateSetIdx).zip(updateMeta.hitMask).zip(updateMeta.positions).foreach {
      case ((ctr, hit), pos) => when(hit && cond(pos))(action(ctr))
    }

  // update taken counter
  when(!actualTaken) {
    updateCounter(_ => true.B, _.decrease())
  }.elsewhen(!predictTaken && actualTaken) {
    updateCounter(_ < actualTakenPosition, _.decrease())
    updateCounter(_ === actualTakenPosition, _.increase())
  }.otherwise { // predictTaken && actualTaken
    when(predictTakenPosition === actualTakenPosition) {
      updateCounter(_ < predictTakenPosition, _.decrease())
      takenCounter(updateSetIdx)(predictTakenWayIdx).increase()
    }.otherwise {
      updateCounter(_ <= actualTakenPosition, _.decrease())
    }
  }

  private val mispredict        = currentUpdate.hasMispredict
  private val needWriteNewEntry = mispredict && actualTaken && !hitTakenBranch
  private val needModifyEntry =
    mispredict && predictTaken && actualTaken && predictTakenPosition === actualTakenPosition
  private val needInvalidateEntry = WireDefault(false.B) // TODO: need predecode info

  private val writeEntry = Wire(new AheadBtbEntry)
  writeEntry.valid           := Mux(needInvalidateEntry, false.B, true.B)
  writeEntry.tag             := getTag(currentUpdate.startVAddr)
  writeEntry.position        := actualTakenPosition
  writeEntry.attribute       := currentUpdate.cfiAttribute
  writeEntry.targetState     := getTargetState(currentUpdate.startVAddr, currentUpdate.target)
  writeEntry.targetLowerBits := getTargetLowerBits(currentUpdate.target)
  // TODO: always taken conditional branch and indirect branch
  writeEntry.isStaticTarget := currentUpdate.cfiAttribute.isDirect // FIXME

  private val writeBankValid = needWriteNewEntry || needModifyEntry || needInvalidateEntry
  private val writeBankIdx   = getBankIndex(previousUpdate.startVAddr)
  private val writeBankMask  = UIntToOH(writeBankIdx)

  aBtbBanks.zipWithIndex.foreach { case (bank, i) =>
    bank.io.writeReq.valid            := writeBankValid && writeBankMask(i)
    bank.io.writeReq.bits.isNewEntry  := needWriteNewEntry
    bank.io.writeReq.bits.setIdx      := updateSetIdx
    bank.io.writeReq.bits.writeWayIdx := predictTakenWayIdx
    bank.io.writeReq.bits.entry       := writeEntry
  }
}
