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
    val redirectValid:    Bool                          = Input(Bool())
    val fastPredictReq:   Valid[AheadBtbFastPredictReq] = Flipped(Valid(new AheadBtbFastPredictReq))
    val previousVAddr:    Valid[PrunedAddr]             = Flipped(Valid(PrunedAddr(VAddrBits)))
    val prediction:       Prediction                    = Output(new Prediction)
    val meta:             AheadBtbMeta                  = Output(new AheadBtbMeta)
    val debug_startVAddr: PrunedAddr                    = Output(PrunedAddr(VAddrBits))
  }
  val io: AheadBtbIO = IO(new AheadBtbIO)

  private val banks     = Seq.fill(NumBanks)(Module(new AheadBtbBank))
  private val replacers = Seq.fill(NumBanks)(Module(new AheadBtbReplacer))

  private val resetDone = RegInit(false.B)
  when(banks.map(_.io.readReq.ready).reduce(_ && _)) {
    resetDone := true.B
  }
  io.resetDone := resetDone

  private val takenCounter = RegInit(
    VecInit.fill(NumBanks)(
      VecInit.fill(NumSets)(
        VecInit.fill(NumWays)(0.U.asTypeOf(new SaturateCounter(TakenCounterWidth)))
      )
    )
  )

  // TODO: write ctr bypass to read

  private val s0_fire = Wire(Bool())
  private val s1_fire = Wire(Bool())
  private val s2_fire = Wire(Bool())

  private val s1_ready = Wire(Bool())
  private val s2_ready = Wire(Bool())

  private val s1_flush = Wire(Bool())
  private val s2_flush = Wire(Bool())

  private val s1_valid = RegInit(false.B)
  private val s2_valid = RegInit(false.B)

  private val predictReqValid = io.stageCtrl.s0_fire
  private val redirectValid   = io.redirectValid

  private val s0_fastPredictValid = io.enable && io.fastPredictReq.valid && !redirectValid

  when(io.fastPredictReq.valid)(assert(predictReqValid, "s0_fire must be true when s3_override"))

  s0_fire := io.enable && predictReqValid
  s1_fire := io.enable && s1_valid && s2_ready && predictReqValid
  s2_fire := io.enable && s2_valid

  s1_ready := s1_fire || !s1_valid
  s2_ready := s2_fire || !s2_valid

  s2_flush := redirectValid
  s1_flush := s2_flush

  when(s0_fire)(s1_valid := true.B)
    .elsewhen(s1_flush || s0_fastPredictValid)(s1_valid := false.B)
    .elsewhen(s1_fire)(s1_valid := false.B)

  when(s2_flush)(s2_valid := false.B)
    .elsewhen(s1_fire || s0_fastPredictValid)(s2_valid := !s1_flush)
    .elsewhen(s2_fire)(s2_valid := false.B)

  /* --------------------------------------------------------------------------------------------------------------
     predict pipeline stage 0
     - get set index and bank index
     - send read request to selected bank
     -------------------------------------------------------------------------------------------------------------- */

  private val s0_previousVAddr = io.startVAddr

  private val s0_setIdx   = getSetIndex(s0_previousVAddr)
  private val s0_bankIdx  = getBankIndex(s0_previousVAddr)
  private val s0_bankMask = UIntToOH(s0_bankIdx)

  banks.zipWithIndex.foreach { case (b, i) =>
    b.io.readReq.valid       := io.enable && predictReqValid && s0_bankMask(i) && !s0_fastPredictValid
    b.io.readReq.bits.setIdx := s0_setIdx
  }

  private val s0_fastPredictReq = io.fastPredictReq.bits

  /* --------------------------------------------------------------------------------------------------------------
     predict pipeline stage 1
     - get the latest startVAddr for compare tag in s2
     - get entries from bank
     -------------------------------------------------------------------------------------------------------------- */

  private val s1_startVAddr = io.startVAddr

  private val s1_setIdx   = RegEnable(s0_setIdx, s0_fire)
  private val s1_bankIdx  = RegEnable(s0_bankIdx, s0_fire)
  private val s1_bankMask = RegEnable(s0_bankMask, s0_fire)

  private val s1_entries = Mux1H(s1_bankMask, banks.map(_.io.readResp.entries))

  /* --------------------------------------------------------------------------------------------------------------
     predict pipeline stage 2
     - get taken counter result
     - compare tag and get taken mask
     - compare positions and find first taken entry
     - get target from found entry
     - output prediction
     -------------------------------------------------------------------------------------------------------------- */

  private val s2_setIdx   = RegEnable(s1_setIdx, s1_fire)
  private val s2_bankIdx  = RegEnable(s1_bankIdx, s1_fire)
  private val s2_bankMask = RegEnable(s1_bankMask, s1_fire)

  // when fast predict, s0 jump to s2
  private val s2_fastPredictValid = RegNext(s0_fastPredictValid)
  private val s2_fastPredictReq   = RegEnable(s0_fastPredictReq, s0_fastPredictValid)

  private val s2_ctrResult =
    Mux(
      s2_fastPredictValid,
      s2_fastPredictReq.ctrResult,
      VecInit(takenCounter(s2_bankIdx)(s2_setIdx).map(_.isPositive))
    )

  private val s2_startVAddr = Mux(s2_fastPredictValid, s2_fastPredictReq.startVAddr, RegEnable(s1_startVAddr, s1_fire))
  private val s2_entries    = Mux(s2_fastPredictValid, s2_fastPredictReq.entries, RegEnable(s1_entries, s1_fire))

  private val s2_tag       = getTag(s2_startVAddr)
  private val s2_hitMask   = s2_entries.map(entry => entry.valid && entry.tag === s2_tag)
  private val s2_hit       = s2_hitMask.reduce(_ || _)
  private val s2_takenMask = s2_hitMask.zip(s2_ctrResult).map { case (hit, taken) => hit && taken }
  private val s2_taken     = s2_takenMask.reduce(_ || _)

  private val s2_positions         = s2_entries.map(_.position)
  private val s2_firstTakenEntryOH = getMinimalValueOH(s2_positions, s2_takenMask)
  private val s2_firstTakenEntry   = Mux1H(s2_firstTakenEntryOH, s2_entries)

  // When detect multi-hit, we need to invalidate one entry.
  private val (s2_multiHit, s2_multiHitWayIdx) = detectMultiHit(s2_hitMask, s2_positions)

  private val s2_target =
    getFullTarget(s2_startVAddr, s2_firstTakenEntry.targetLowerBits, s2_firstTakenEntry.targetCarry)

  io.prediction.taken       := s2_valid && s2_taken
  io.prediction.cfiPosition := s2_firstTakenEntry.position
  io.prediction.attribute   := s2_firstTakenEntry.attribute
  io.prediction.target      := s2_target

  // used for check abtb output
  io.debug_startVAddr := s2_startVAddr

  io.meta.valid     := s2_valid
  io.meta.hitMask   := s2_hitMask
  io.meta.taken     := s2_taken
  io.meta.takenMask := s2_firstTakenEntryOH
  io.meta.entries   := s2_entries
  io.meta.ctrResult := s2_ctrResult

  replacers.zipWithIndex.foreach { case (r, i) =>
    r.io.readValid   := s2_valid && s2_hit && s2_bankMask(i)
    r.io.readSetIdx  := s2_setIdx
    r.io.readWayMask := s2_hitMask
  }

  /* --------------------------------------------------------------------------------------------------------------
     train pipeline stage 0
     - receive train request
     -------------------------------------------------------------------------------------------------------------- */

  private val t0_valid         = io.enable && io.train.valid && io.train.bits.meta.abtb.valid && io.previousVAddr.valid
  private val t0_train         = io.train.bits
  private val t0_previousVAddr = io.previousVAddr.bits

  /* --------------------------------------------------------------------------------------------------------------
     train pipeline stage 1
     - update taken counter
     - write a new entry or modify an existing entry if needed
     -------------------------------------------------------------------------------------------------------------- */

  private val t1_valid         = RegNext(t0_valid) && io.enable
  private val t1_train         = RegEnable(t0_train, t0_valid)
  private val t1_previousVAddr = RegEnable(t0_previousVAddr, t0_valid)

  private val t1_setIdx  = getSetIndex(t1_previousVAddr)
  private val t1_setMask = UIntToOH(t1_setIdx)

  private val t1_bankIdx  = getBankIndex(t1_previousVAddr)
  private val t1_bankMask = UIntToOH(t1_bankIdx)

  private val t1_meta            = t1_train.meta.abtb
  private val t1_positions       = t1_meta.entries.map(_.position)
  private val t1_attributes      = t1_meta.entries.map(_.attribute)
  private val t1_targetLowerBits = t1_meta.entries.map(_.targetLowerBits)

  private val t1_trainBranch = t1_train.branches(0).bits

  /* --------------------------------------------------------------------------------------------------------------
     update taken counter
     -------------------------------------------------------------------------------------------------------------- */

  private val t1_positionBeforeMask = t1_meta.hitMask.zip(t1_positions).zip(t1_attributes).map {
    case ((hit, position), attribute) =>
      hit && position < t1_trainBranch.cfiPosition && attribute.isConditional && t1_trainBranch.attribute.isConditional
  }
  private val t1_positionEqualMask = t1_meta.hitMask.zip(t1_positions).zip(t1_attributes).map {
    case ((hit, position), attribute) =>
      hit && position === t1_trainBranch.cfiPosition &&
      attribute.isConditional && t1_trainBranch.attribute.isConditional
  }

  private val t1_needResetCtr = takenCounter.zip(banks).map { case (bankCtrs, bank) =>
    val needReset = bank.io.writeResp.valid && bank.io.writeResp.bits.needResetCtr
    val setMask   = UIntToOH(bank.io.writeResp.bits.setIdx)
    val wayMask   = UIntToOH(bank.io.writeResp.bits.wayIdx)
    bankCtrs.zipWithIndex.map { case (setCtrs, setIdx) =>
      setCtrs.zipWithIndex.map { case (_, wayIdx) =>
        needReset && setMask(setIdx) && wayMask(wayIdx)
      }
    }
  }

  private val t1_needIncreaseCtr = takenCounter.zipWithIndex.map { case (bankCtrs, bankIdx) =>
    bankCtrs.zipWithIndex.map { case (setCtrs, setIdx) =>
      setCtrs.zip(t1_positionEqualMask).map { case (_, equal) =>
        t1_valid && t1_bankMask(bankIdx) && t1_setMask(setIdx) && equal && t1_trainBranch.taken
      }
    }
  }

  private val t1_needDecreaseCtr = takenCounter.zipWithIndex.map { case (bankCtrs, bankIdx) =>
    bankCtrs.zipWithIndex.map { case (setCtrs, setIdx) =>
      setCtrs.zip(t1_positionBeforeMask) map { case (_, before) =>
        t1_valid && t1_bankMask(bankIdx) && t1_setMask(setIdx) &&
        (!t1_trainBranch.taken || t1_trainBranch.taken && before)
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
  private val t1_hitTrainBranch = t1_meta.hitMask.zip(t1_positions).zip(t1_attributes).map {
    case ((hit, position), attribute) =>
      hit && t1_trainBranch.taken && position === t1_trainBranch.cfiPosition && attribute === t1_trainBranch.attribute
  }.reduce(_ || _)
  private val t1_needWriteNewEntry = !t1_hitTrainBranch

  // If the target of indirect branch is wrong, we need correct it.
  // Since the entry only stores the lower bits of the target, we only need to check the lower bits.
  private val t1_trainBranchTargetLowerBits = getTargetLowerBits(t1_trainBranch.target)
  private val t1_targetWrongMask = t1_meta.hitMask.zip(t1_positions).zip(t1_attributes).zip(t1_targetLowerBits).map {
    case (((hit, position), attribute), targetLowerBits) =>
      hit && t1_trainBranch.taken && attribute.isIndirect && t1_trainBranch.attribute.isIndirect &&
      position === t1_trainBranch.cfiPosition && targetLowerBits =/= t1_trainBranchTargetLowerBits
  }
  private val t1_needCorrectTarget = t1_targetWrongMask.reduce(_ || _)

  // TODO: if the attribute of the taken branch is wrong, we need replace it or invalidate it

  private val t1_writeEntry = Wire(new AheadBtbEntry)
  t1_writeEntry.valid           := true.B
  t1_writeEntry.tag             := getTag(t1_train.startVAddr)
  t1_writeEntry.position        := t1_trainBranch.cfiPosition
  t1_writeEntry.attribute       := t1_trainBranch.attribute
  t1_writeEntry.targetLowerBits := t1_trainBranchTargetLowerBits
  if (t1_writeEntry.targetCarry.isDefined) {
    t1_writeEntry.targetCarry.get := getTargetCarry(t1_train.startVAddr, t1_trainBranch.target) // if (EnableTargetFix)
  }

  replacers.foreach(_.io.replaceSetIdx := t1_setIdx)
  private val victimWayIdx = replacers.map(_.io.victimWayIdx)

  // TODO: the prioriority of write?
  banks.zipWithIndex.foreach { case (b, i) =>
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
      b.io.writeReq.bits.wayIdx       := PriorityEncoder(t1_targetWrongMask)
      b.io.writeReq.bits.entry        := t1_writeEntry
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

  replacers.zip(banks).foreach { case (r, b) =>
    r.io.writeValid  := b.io.writeResp.valid
    r.io.writeSetIdx := b.io.writeResp.bits.setIdx
    r.io.writeWayIdx := b.io.writeResp.bits.wayIdx
  }

  /* --------------------------------------------------------------------------------------------------------------
     performance counter
     -------------------------------------------------------------------------------------------------------------- */

  private val t1_predPosition        = Mux1H(t1_meta.hitMask, t1_positions)
  private val t1_predAttribute       = Mux1H(t1_meta.hitMask, t1_attributes)
  private val t1_predTargetLowerBits = Mux1H(t1_meta.hitMask, t1_targetLowerBits)

  private val perf_directionWrong = t1_valid &&
    ((!t1_meta.taken && t1_trainBranch.taken) || (t1_meta.taken && !t1_trainBranch.taken))

  private val perf_missWrong = t1_valid && !t1_meta.taken && t1_trainBranch.taken && !t1_hitTrainBranch

  private val perf_takenPositionWrong = t1_valid && t1_meta.taken && t1_trainBranch.taken &&
    t1_predPosition =/= t1_trainBranch.cfiPosition

  private val perf_targetWrong = t1_valid && t1_meta.taken && t1_trainBranch.taken &&
    t1_predAttribute === t1_trainBranch.attribute &&
    t1_predPosition === t1_trainBranch.cfiPosition &&
    t1_predTargetLowerBits =/= t1_trainBranchTargetLowerBits

  private val perf_predictNotTakenRight = t1_valid && !t1_meta.taken && !t1_trainBranch.taken

  private val perf_predictRight = t1_valid && ((!t1_meta.taken && !t1_trainBranch.taken) ||
    (t1_meta.taken && t1_trainBranch.taken &&
      t1_predAttribute === t1_trainBranch.attribute &&
      t1_predPosition === t1_trainBranch.cfiPosition &&
      t1_predTargetLowerBits === t1_trainBranchTargetLowerBits))

  private val perf_condTakenRight = t1_valid && t1_meta.taken && t1_trainBranch.taken &&
    t1_predAttribute.isConditional && t1_trainBranch.attribute.isConditional &&
    t1_predPosition === t1_trainBranch.cfiPosition

  private val perf_directRight = t1_valid && t1_meta.taken && t1_trainBranch.taken &&
    t1_predAttribute.isDirect && t1_trainBranch.attribute.isDirect &&
    t1_predPosition === t1_trainBranch.cfiPosition

  private val perf_indirectRight = t1_valid && t1_meta.taken && t1_trainBranch.taken &&
    t1_predAttribute.isIndirect && t1_trainBranch.attribute.isIndirect &&
    t1_predPosition === t1_trainBranch.cfiPosition &&
    t1_predTargetLowerBits === t1_trainBranchTargetLowerBits

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
  XSPerfAccumulate("train_hit_path", t1_valid && t1_meta.hitMask.reduce(_ || _))
  XSPerfAccumulate("train_hit_taken_branch", t1_valid && t1_hitTrainBranch)
  XSPerfAccumulate("train_predict_taken", t1_valid && t1_meta.taken)
  XSPerfAccumulate("train_predict_not_taken", t1_valid && t1_meta.hitMask.reduce(_ || _) && !t1_meta.taken)
  XSPerfAccumulate("train_actual_taken", t1_valid && t1_trainBranch.taken)
  XSPerfAccumulate("train_actual_not_taken", t1_valid && !t1_trainBranch.taken)

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

  XSPerfAccumulate("train_direction_wrong", perf_directionWrong)
  XSPerfAccumulate("train_miss_wrong", perf_missWrong)
  XSPerfAccumulate("train_taken_position_wrong", perf_takenPositionWrong)
  XSPerfAccumulate("train_target_wrong", perf_targetWrong)
  XSPerfAccumulate("train_predict_right", perf_predictRight)
  XSPerfAccumulate("train_predict_not_taken_right", perf_predictNotTakenRight)
  XSPerfAccumulate("train_cond_taken_right", perf_condTakenRight)
  XSPerfAccumulate("train_direct_right", perf_directRight)
  XSPerfAccumulate("train_indirect_right", perf_indirectRight)
}
