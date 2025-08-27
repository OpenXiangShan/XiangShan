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
import utility.XSPerfAccumulate
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.BasePredictor
import xiangshan.frontend.bpu.BasePredictorIO
import xiangshan.frontend.bpu.Prediction
import xiangshan.frontend.bpu.SaturateCounter

/**
 * This module is the implementation of the ahead BTB (Branch Target Buffer).
 */
class AheadBtb(implicit p: Parameters) extends BasePredictor with Helpers {
  class AheadBtbIO(implicit p: Parameters) extends BasePredictorIO {
    val redirectValid:    Bool         = Input(Bool())
    val overrideValid:    Bool         = Input(Bool())
    val prediction:       Prediction   = Output(new Prediction)
    val meta:             AheadBtbMeta = Output(new AheadBtbMeta)
    val debug_startVAddr: PrunedAddr   = Output(PrunedAddr(VAddrBits))
  }
  val io: AheadBtbIO = IO(new AheadBtbIO)

  private val sramBanks = Seq.fill(NumBanks)(Module(new AheadBtbSramBank))
  private val replacers = Seq.fill(NumBanks)(Module(new AheadBtbReplacer))

  private val resetDone = RegInit(false.B)
  when(sramBanks.map(_.io.readReq.ready).reduce(_ && _)) {
    resetDone := true.B
  }
  io.resetDone := resetDone

  private val takenCounter = Reg(Vec(NumBanks, Vec(NumSets, Vec(NumWays, new SaturateCounter(TakenCounterWidth)))))

  // TODO: use mbtb to train abtb

  private val s0_fire = Wire(Bool())
  private val s1_fire = Wire(Bool())
  private val s2_fire = Wire(Bool())
  private val s3_fire = Wire(Bool())

  private val s1_ready = Wire(Bool())
  private val s2_ready = Wire(Bool())
  private val s3_ready = Wire(Bool())

  private val s1_flush = Wire(Bool())
  private val s2_flush = Wire(Bool())
  private val s3_flush = Wire(Bool())

  private val s1_valid = RegInit(false.B)
  private val s2_valid = RegInit(false.B)
  private val s3_valid = RegInit(false.B)

  private val predictReqValid = io.enable && io.stageCtrl.s0_fire
  private val redirectValid   = io.redirectValid
  private val overrideValid   = io.overrideValid

  s0_fire := predictReqValid
  s1_fire := s1_valid && s2_ready && predictReqValid
  s2_fire := s2_valid && s3_ready && overrideValid // only enter s3 when s2 receives an override
  s3_fire := s3_valid

  s1_ready := s1_fire || !s1_valid
  s2_ready := s2_fire || !s2_valid
  s3_ready := s3_fire || !s3_valid

  s1_flush := redirectValid || overrideValid
  s2_flush := redirectValid || overrideValid
  s3_flush := redirectValid

  when(s0_fire)(s1_valid := true.B)
    .elsewhen(s1_flush)(s1_valid := false.B)
    .elsewhen(s1_fire)(s1_valid := false.B)

  when(s2_flush)(s2_valid := false.B)
    .elsewhen(s1_fire)(s2_valid := !s2_flush)
    .elsewhen(s2_fire || (s2_valid && !overrideValid))(s2_valid := false.B)

  when(s3_flush)(s3_valid := false.B)
    .elsewhen(s2_fire)(s3_valid := !s3_flush)
    .elsewhen(s3_fire)(s3_valid := false.B)

  /* --------------------------------------------------------------------------------------------------------------
     predict pipeline stage 0
     - get set index and bank index
     - send read request to selected bank
     -------------------------------------------------------------------------------------------------------------- */

  private val s0_previousPc = io.startVAddr

  private val s0_setIdx   = getSetIndex(s0_previousPc)
  private val s0_bankIdx  = getBankIndex(s0_previousPc)
  private val s0_bankMask = UIntToOH(s0_bankIdx, NumBanks)

  sramBanks.zipWithIndex.foreach { case (b, i) =>
    b.io.readReq.valid       := predictReqValid && s0_bankMask(i)
    b.io.readReq.bits.setIdx := s0_setIdx
  }

  /* --------------------------------------------------------------------------------------------------------------
     predict pipeline stage 1
     - get the latest start pc for compare tag in s2
     - get entries from bank
     -------------------------------------------------------------------------------------------------------------- */

  private val s1_startPc = io.startVAddr
  private val s1_tag     = getTag(s1_startPc)

  private val s1_setIdx   = RegEnable(s0_setIdx, s0_fire)
  private val s1_bankIdx  = RegEnable(s0_bankIdx, s0_fire)
  private val s1_bankMask = RegEnable(s0_bankMask, s0_fire)

  private val s1_entries = Mux1H(s1_bankMask, sramBanks.map(_.io.readResp.entries))

  dontTouch(s1_tag)
  dontTouch(s1_setIdx)
  dontTouch(s1_bankIdx)
  dontTouch(s1_entries)

  /* --------------------------------------------------------------------------------------------------------------
     predict pipeline stage 2
     - get taken counter result
     - compare tag and get taken mask
     - compare positions and find first taken entry
     - get target from found entry
     - output prediction
     -------------------------------------------------------------------------------------------------------------- */

  private val s2_startPc = RegEnable(s1_startPc, s1_fire)
  private val s2_tag     = RegEnable(s1_tag, s1_fire)

  // for override
  private val s2_newStartPc = io.startVAddr

  private val s2_setIdx   = RegEnable(s1_setIdx, s1_fire)
  private val s2_bankIdx  = RegEnable(s1_bankIdx, s1_fire)
  private val s2_bankMask = RegEnable(s1_bankMask, s1_fire)

  private val s2_entries = RegEnable(s1_entries, s1_fire)

  dontTouch(s2_tag)
  dontTouch(s2_entries)

  private val s2_ctrResult = takenCounter(s2_bankIdx)(s2_setIdx).map(_.isPositive)

  private val s2_hitMask   = s2_entries.map(entry => entry.valid && entry.tag === s2_tag)
  private val s2_hit       = s2_hitMask.reduce(_ || _)
  private val s2_takenMask = s2_hitMask.zip(s2_ctrResult).map { case (hit, taken) => hit && taken }
  private val s2_taken     = s2_takenMask.reduce(_ || _)

  private val s2_positions              = s2_entries.map(_.position)
  private val s2_firstTakenEntryWayMask = getFirstTakenBranchOH(s2_positions, s2_takenMask)
  private val s2_firstTakenEntry        = Mux1H(s2_firstTakenEntryWayMask, s2_entries)

  // when detect multi-hit, we need to invalidate one entry
  private val (s2_multiHit, s2_multiHitWayIdx) = detectMultiHit(s2_hitMask, s2_positions)

  private val s2_takenPosition = s2_firstTakenEntry.position
  private val s2_target = getFullTarget(s2_startPc, s2_firstTakenEntry.targetLowerBits, s2_firstTakenEntry.targetCarry)

  private val s2_prediction = Wire(new Prediction)
  s2_prediction.taken       := s2_valid && s2_taken
  s2_prediction.cfiPosition := s2_takenPosition
  s2_prediction.target      := s2_target
  s2_prediction.attribute   := s2_firstTakenEntry.attribute

  private val s2_meta = Wire(new AheadBtbMeta)
  s2_meta.valid             := s2_valid && !overrideValid
  s2_meta.hitMask           := s2_hitMask
  s2_meta.taken             := s2_taken
  s2_meta.takenEntryWayMask := s2_firstTakenEntryWayMask
  s2_meta.attributes        := s2_entries.map(_.attribute)
  s2_meta.positions         := s2_positions
  s2_meta.targetLowerBits   := s2_firstTakenEntry.targetLowerBits
  if (s2_meta.target.isDefined) {
    s2_meta.target.get := s2_target
  }

  replacers.zipWithIndex.foreach { case (r, i) =>
    r.io.readValid   := s2_valid && s2_hit && s2_bankMask(i) && !overrideValid && !redirectValid
    r.io.readSetIdx  := s2_setIdx
    r.io.readWayMask := s2_hitMask
  }

  /* --------------------------------------------------------------------------------------------------------------
     predict pipeline stage 3
     - handle override
     -------------------------------------------------------------------------------------------------------------- */

  private val s3_entries  = RegEnable(RegEnable(s1_entries, s1_fire), s1_fire)
  private val s3_setIdx   = RegEnable(RegEnable(s1_setIdx, s1_fire), s1_fire)
  private val s3_bankIdx  = RegEnable(RegEnable(s1_bankIdx, s1_fire), s1_fire)
  private val s3_bankMask = RegEnable(RegEnable(s1_bankMask, s1_fire), s1_fire)

  private val s3_startPc = RegEnable(s2_newStartPc, s2_fire)
  private val s3_tag     = getTag(s3_startPc)

  dontTouch(s3_setIdx)
  dontTouch(s3_bankIdx)
  dontTouch(s3_tag)
  dontTouch(s3_startPc)
  dontTouch(s3_entries)

  private val s3_ctrResult = takenCounter(s3_bankIdx)(s3_setIdx).map(_.isPositive)

  private val s3_hitMask   = s3_entries.map(entry => entry.valid && entry.tag === s3_tag)
  private val s3_hit       = s3_hitMask.reduce(_ || _)
  private val s3_takenMask = s3_hitMask.zip(s3_ctrResult).map { case (hit, taken) => hit && taken }
  private val s3_taken     = s3_takenMask.reduce(_ || _)

  private val s3_positions              = s3_entries.map(_.position)
  private val s3_firstTakenEntryWayMask = getFirstTakenBranchOH(s3_positions, s3_takenMask)
  private val s3_firstTakenEntry        = Mux1H(s3_firstTakenEntryWayMask, s3_entries)

  // when detect multi-hit, we need to invalidate one entry
  private val (s3_multiHit, s3_multiHitWayIdx) = detectMultiHit(s3_hitMask, s3_positions)

  private val s3_takenPosition = s3_firstTakenEntry.position
  private val s3_target = getFullTarget(s3_startPc, s3_firstTakenEntry.targetLowerBits, s3_firstTakenEntry.targetCarry)

  private val s3_prediction = Wire(new Prediction)
  s3_prediction.taken       := s3_valid && s3_taken && !overrideValid
  s3_prediction.cfiPosition := s3_takenPosition
  s3_prediction.target      := s3_target
  s3_prediction.attribute   := s3_firstTakenEntry.attribute

  private val s3_meta = Wire(new AheadBtbMeta)
  s3_meta.valid             := s3_valid
  s3_meta.hitMask           := s3_hitMask
  s3_meta.taken             := s3_taken
  s3_meta.takenEntryWayMask := s3_firstTakenEntryWayMask
  s3_meta.attributes        := s3_entries.map(_.attribute)
  s3_meta.positions         := s3_positions
  s3_meta.targetLowerBits   := s3_firstTakenEntry.targetLowerBits
  if (s3_meta.target.isDefined) {
    s3_meta.target.get := s3_target
  }

  replacers.zipWithIndex.foreach { case (r, i) =>
    r.io.readValid   := s3_valid && s3_hit && s3_bankMask(i) && !overrideValid && !redirectValid
    r.io.readSetIdx  := s3_setIdx
    r.io.readWayMask := s3_hitMask
  }

  /* --------------------------------------------------------------------------------------------------------------
     select prediction between s2 and s3
     -------------------------------------------------------------------------------------------------------------- */

  io.prediction := Mux(s3_valid, s3_prediction, s2_prediction)

  io.meta := Mux(s3_valid, s3_meta, s2_meta)

  // used for check abtb output
  io.debug_startVAddr := Mux(s3_valid, s3_startPc, s2_startPc)

  /* --------------------------------------------------------------------------------------------------------------
     train pipeline stage 0
     - receive train request
     -------------------------------------------------------------------------------------------------------------- */

  private val t0_train = io.train

  /* --------------------------------------------------------------------------------------------------------------
     train pipeline stage 1
     - update taken counter
     - write a new entry or modify an existing entry if needed
     -------------------------------------------------------------------------------------------------------------- */

  // delay one cycle for better timing
  private val t1_trainValid = RegNext(t0_train.valid)
  private val t1_train      = RegEnable(t0_train.bits, t0_train.valid)

  private val t1_previousPcValid = RegInit(false.B)
  private val t1_bankIdx         = Reg(UInt(BankIdxWidth.W))
  private val t1_setIdx          = Reg(UInt(SetIdxWidth.W))

  when(t1_trainValid) {
    t1_previousPcValid := true.B
    t1_bankIdx         := getBankIndex(t1_train.startVAddr)
    t1_setIdx          := getSetIndex(t1_train.startVAddr)
  }

  private val t1_bankMask = UIntToOH(t1_bankIdx, NumBanks)
  private val t1_setMask  = UIntToOH(t1_setIdx, NumSets)

  private val t1_meta = t1_train.meta.abtb

  private val t1_valid = t1_previousPcValid && t1_trainValid && t1_meta.valid

  // TODO: if we want update after execution, we need FTQ send a previousPc with one update request
  //  if we want update after commit, we just ues previous update request to get bankIdx and setIdx
  //  because the order of prediction and commit is the same

  private val perf_t1_hit = t1_meta.hitMask.reduce(_ || _)

  /* --------------------------------------------------------------------------------------------------------------
     update taken counter
     -------------------------------------------------------------------------------------------------------------- */

  private val t1_positionBeforeMask = t1_meta.hitMask.zip(t1_meta.positions).zip(t1_meta.attributes).map {
    case ((hit, pos), attr) => hit && pos < t1_train.cfiPosition && attr.isConditional
  }
  private val t1_positionEqualMask = t1_meta.hitMask.zip(t1_meta.positions).zip(t1_meta.attributes).map {
    case ((hit, pos), attr) => hit && pos === t1_train.cfiPosition && attr.isConditional
  }

  private val t1_needResetCtr = takenCounter.zip(sramBanks).map { case (bankCtrs, bank) =>
    val needReset = bank.io.writeResp.valid && bank.io.writeResp.bits.needResetCtr
    val setMask   = UIntToOH(bank.io.writeResp.bits.setIdx, NumSets)
    val wayMask   = UIntToOH(bank.io.writeResp.bits.wayIdx, NumWays)
    bankCtrs.zipWithIndex.map { case (setCtrs, setIdx) =>
      setCtrs.zipWithIndex.map { case (_, wayIdx) =>
        needReset && setMask(setIdx) && wayMask(wayIdx)
      }
    }
  }

  private val t1_needIncreaseCtr = takenCounter.zipWithIndex.map { case (bankCtrs, bankIdx) =>
    bankCtrs.zipWithIndex.map { case (setCtrs, setIdx) =>
      setCtrs.zip(t1_positionEqualMask).map { case (_, equal) =>
        t1_valid && t1_bankMask(bankIdx) && t1_setMask(setIdx) && equal && t1_train.taken
      }
    }
  }

  private val t1_needDecreaseCtr = takenCounter.zipWithIndex.map { case (bankCtrs, bankIdx) =>
    bankCtrs.zipWithIndex.map { case (setCtrs, setIdx) =>
      setCtrs.zip(t1_positionBeforeMask).zip(t1_positionEqualMask).map { case ((_, before), equal) =>
        t1_valid && t1_bankMask(bankIdx) && t1_setMask(setIdx) && (before || (equal && !t1_train.taken))
      }
    }
  }

  takenCounter.zipWithIndex.foreach { case (bankCtrs, bankIdx) =>
    bankCtrs.zipWithIndex.foreach { case (setCtrs, setIdx) =>
      setCtrs.zipWithIndex.foreach { case (ctr, wayIdx) =>
        val needReset    = t1_needResetCtr(bankIdx)(setIdx)(wayIdx)
        val needDecrease = t1_needDecreaseCtr(bankIdx)(setIdx)(wayIdx)
        val needIncrease = t1_needIncreaseCtr(bankIdx)(setIdx)(wayIdx)
        when(needReset)(ctr.resetNeutral())
          .elsewhen(needDecrease)(ctr.decrease())
          .elsewhen(needIncrease)(ctr.increase())
      }
    }
  }

  /* --------------------------------------------------------------------------------------------------------------
     write a new entry or modify an existing entry if needed
     -------------------------------------------------------------------------------------------------------------- */

  // if the taken branch is not hit, we need write a new entry
  private val t1_hitTakenBranch = t1_meta.hitMask.zip(t1_meta.positions).zip(t1_meta.attributes).map {
    case ((hit, pos), attr) =>
      hit && t1_train.taken && pos === t1_train.cfiPosition && attr === t1_train.attribute
  }.reduce(_ || _)
  private val t1_needWriteNewEntry = !t1_hitTakenBranch

  private val t1_predAttribute = Mux1H(t1_meta.takenEntryWayMask, t1_meta.attributes)
  private val t1_predPosition  = Mux1H(t1_meta.takenEntryWayMask, t1_meta.positions)

  // If the target of indirect branch is wrong, we need correct it.
  // Since the entry only stores the lower bits of the target, we only need to check the lower bits.
  private val t1_needCorrectTarget = t1_meta.taken && t1_train.taken &&
    t1_predAttribute.isIndirect && t1_train.attribute.isIndirect &&
    t1_predPosition === t1_train.cfiPosition &&
    t1_meta.targetLowerBits =/= getTargetLowerBits(t1_train.target)

  // TODO: if the attribute of the taken branch is wrong, we need replace it or invalidate it

  private val t1_writeEntry = Wire(new AheadBtbEntry)
  t1_writeEntry.valid           := true.B
  t1_writeEntry.tag             := getTag(t1_train.startVAddr)
  t1_writeEntry.position        := t1_train.cfiPosition
  t1_writeEntry.attribute       := t1_train.attribute
  t1_writeEntry.targetLowerBits := getTargetLowerBits(t1_train.target)
  t1_writeEntry.targetCarry.foreach(_ := getTargetCarry(t1_train.startVAddr, t1_train.target)) // if (EnableTargetFix)

  replacers.foreach(_.io.replaceSetIdx := t1_setIdx)
  private val victimWayIdx = replacers.map(_.io.victimWayIdx)

  // TODO: the prioriority of write?
  sramBanks.zipWithIndex.foreach { case (b, i) =>
    when(t1_valid && t1_needWriteNewEntry && t1_bankMask(i)) {
      b.io.writeReq.valid             := true.B
      b.io.writeReq.bits.needResetCtr := true.B
      b.io.writeReq.bits.setIdx       := t1_setIdx
      b.io.writeReq.bits.wayIdx       := victimWayIdx(i)
      b.io.writeReq.bits.entry        := t1_writeEntry
    }.elsewhen(t1_valid && t1_needCorrectTarget && t1_bankMask(i)) {
      b.io.writeReq.valid             := true.B
      b.io.writeReq.bits.needResetCtr := false.B
      b.io.writeReq.bits.setIdx       := t1_setIdx
      b.io.writeReq.bits.wayIdx       := t1_meta.takenEntryWayMask
      b.io.writeReq.bits.entry        := t1_writeEntry
    }.elsewhen(s3_valid && s3_multiHit && s3_bankMask(i)) {
      b.io.writeReq.valid             := true.B
      b.io.writeReq.bits.needResetCtr := true.B
      b.io.writeReq.bits.setIdx       := s3_setIdx
      b.io.writeReq.bits.wayIdx       := s3_multiHitWayIdx
      b.io.writeReq.bits.entry        := 0.U.asTypeOf(new AheadBtbEntry)
    }.elsewhen(s2_valid && s2_multiHit && s2_bankMask(i)) {
      b.io.writeReq.valid             := true.B
      b.io.writeReq.bits.needResetCtr := true.B
      b.io.writeReq.bits.setIdx       := s2_setIdx
      b.io.writeReq.bits.wayIdx       := s2_multiHitWayIdx
      b.io.writeReq.bits.entry        := 0.U.asTypeOf(new AheadBtbEntry)
    }.otherwise {
      b.io.writeReq.valid := false.B
      b.io.writeReq.bits  := 0.U.asTypeOf(new BankWriteReq)
    }
  }

  replacers.zip(sramBanks).foreach { case (r, b) =>
    r.io.writeValid  := b.io.writeResp.valid
    r.io.writeSetIdx := b.io.writeResp.bits.setIdx
    r.io.writeWayIdx := b.io.writeResp.bits.wayIdx
  }

  /* --------------------------------------------------------------------------------------------------------------
     performance counter
     -------------------------------------------------------------------------------------------------------------- */

  private val perf_targetSame = if (t1_meta.target.isDefined) {
    t1_meta.target.get === t1_train.target
  } else {
    false.B
  }

  private val perf_targetOverflow = t1_valid && t1_meta.taken && t1_train.taken &&
    t1_predAttribute.isIndirect && t1_train.attribute.isIndirect &&
    t1_predPosition === t1_train.cfiPosition &&
    t1_meta.targetLowerBits =/= getTargetLowerBits(t1_train.target) &&
    !perf_targetSame

  private val perf_directionWrong = t1_valid &&
    ((!t1_meta.taken && t1_train.taken) || (t1_meta.taken && !t1_train.taken))

  private val perf_missWrong = t1_valid && !t1_meta.taken && t1_train.taken && !t1_hitTakenBranch

  private val perf_takenPositionWrong = t1_valid && t1_meta.taken && t1_train.taken &&
    t1_predPosition =/= t1_train.cfiPosition

  private val perf_targetWrong = t1_valid && t1_meta.taken && t1_train.taken &&
    t1_predAttribute === t1_train.attribute &&
    t1_predPosition === t1_train.cfiPosition &&
    !perf_targetSame

  private val perf_predictNotTakenRight = t1_valid && !t1_meta.taken && !t1_train.taken

  private val perf_predictTakenRight = t1_valid && t1_meta.taken && t1_train.taken &&
    t1_predAttribute === t1_train.attribute &&
    t1_predPosition === t1_train.cfiPosition &&
    perf_targetSame

  private val perf_condTakenRight = t1_valid && t1_meta.taken && t1_train.taken &&
    t1_predAttribute.isConditional && t1_train.attribute.isConditional &&
    t1_predPosition === t1_train.cfiPosition

  private val perf_directRight = t1_valid && t1_meta.taken && t1_train.taken &&
    t1_predAttribute.isDirect && t1_train.attribute.isDirect &&
    t1_predPosition === t1_train.cfiPosition

  private val perf_indirectRight = t1_valid && t1_meta.taken && t1_train.taken &&
    t1_predAttribute.isIndirect && t1_train.attribute.isIndirect &&
    t1_predPosition === t1_train.cfiPosition &&
    perf_targetSame

  XSPerfAccumulate("predict_req_num", predictReqValid)
  XSPerfAccumulate("predict_num", s2_valid)
  XSPerfAccumulate("predict_hit", s2_valid && s2_hit)
  XSPerfAccumulate("predict_miss", s2_valid && !s2_hit)
  XSPerfAccumulate("predict_hit_entry_num", Mux(s2_valid, PopCount(s2_hitMask), 0.U))
  XSPerfAccumulate("predict_taken", s2_valid && s2_taken)
  XSPerfAccumulate("predict_not_taken", s2_valid && s2_hit && !s2_taken)
  XSPerfAccumulate("predict_multi_hit", s2_valid && s2_multiHit)

  XSPerfAccumulate("train_req_num", io.train.valid)
  XSPerfAccumulate("train_num", t1_valid)
  XSPerfAccumulate("train_hit_path", t1_valid && perf_t1_hit)
  XSPerfAccumulate("train_hit_taken_branch", t1_valid && t1_hitTakenBranch)
  XSPerfAccumulate("train_predict_taken", t1_valid && t1_meta.taken)
  XSPerfAccumulate("train_predict_not_taken", t1_valid && perf_t1_hit && !t1_meta.taken)
  XSPerfAccumulate("train_actual_taken", t1_valid && t1_train.taken)
  XSPerfAccumulate("train_actual_not_taken", t1_valid && !t1_train.taken)

  XSPerfAccumulate("total_write", t1_valid && (t1_needWriteNewEntry || t1_needCorrectTarget) || s2_valid && s2_multiHit)
  XSPerfAccumulate("train_write_new_entry", t1_valid && t1_needWriteNewEntry)
  XSPerfAccumulate("train_correct_target", t1_valid && t1_needCorrectTarget)
  XSPerfAccumulate(
    "train_write_conflict",
    t1_valid && (t1_needWriteNewEntry || t1_needCorrectTarget) && s2_valid && s2_multiHit
  )

  XSPerfAccumulate("train_reset_ctr", VecInit(t1_needResetCtr.flatten.flatten).reduce(_ || _))
  XSPerfAccumulate("train_decrease_ctr", VecInit(t1_needDecreaseCtr.flatten.flatten).reduce(_ || _))
  XSPerfAccumulate("train_increase_ctr", VecInit(t1_needIncreaseCtr.flatten.flatten).reduce(_ || _))

  XSPerfAccumulate("train_target_overflow", perf_targetOverflow)
  XSPerfAccumulate("train_direction_wrong", perf_directionWrong)
  XSPerfAccumulate("train_miss_wrong", perf_missWrong)
  XSPerfAccumulate("train_taken_position_wrong", perf_takenPositionWrong)
  XSPerfAccumulate("train_target_wrong", perf_targetWrong)
  XSPerfAccumulate("train_predict_taken_right", perf_predictTakenRight)
  XSPerfAccumulate("train_predict_not_taken_right", perf_predictNotTakenRight)
  XSPerfAccumulate("train_cond_taken_right", perf_condTakenRight)
  XSPerfAccumulate("train_direct_right", perf_directRight)
  XSPerfAccumulate("train_indirect_right", perf_indirectRight)
}
