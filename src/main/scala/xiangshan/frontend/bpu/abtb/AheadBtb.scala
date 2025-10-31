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
import xiangshan.frontend.bpu.CompareMatrix
import xiangshan.frontend.bpu.HasFastTrainIO
import xiangshan.frontend.bpu.Prediction
import xiangshan.frontend.bpu.SaturateCounter

/**
 * This module is the implementation of the ahead BTB (Branch Target Buffer).
 */
class AheadBtb(implicit p: Parameters) extends BasePredictor with Helpers {
  class AheadBtbIO(implicit p: Parameters) extends BasePredictorIO with HasFastTrainIO {
    val redirectValid:    Bool              = Input(Bool())
    val overrideValid:    Bool              = Input(Bool())
    val previousVAddr:    Valid[PrunedAddr] = Flipped(Valid(PrunedAddr(VAddrBits)))
    val prediction:       Prediction        = Output(new Prediction)
    val meta:             AheadBtbMeta      = Output(new AheadBtbMeta)
    val debug_startVAddr: PrunedAddr        = Output(PrunedAddr(VAddrBits))
  }
  val io: AheadBtbIO = IO(new AheadBtbIO)

  private val banks     = Seq.fill(NumBanks)(Module(new AheadBtbBank))
  private val replacers = Seq.fill(NumBanks)(Module(new AheadBtbReplacer))

  private val resetDone = RegInit(false.B)
  when(banks.map(_.io.readReq.ready).reduce(_ && _)) {
    resetDone := true.B
  }
  io.resetDone := resetDone

  io.train.ready := true.B

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
  private val overrideValid   = io.overrideValid

  s0_fire := io.enable && predictReqValid
  s1_fire := io.enable && s1_valid && s2_ready && predictReqValid
  s2_fire := io.enable && s2_valid

  s1_ready := s1_fire || !s1_valid
  s2_ready := s2_fire || !s2_valid

  s2_flush := redirectValid || overrideValid
  s1_flush := s2_flush

  when(s0_fire)(s1_valid := true.B)
    .elsewhen(s1_flush)(s1_valid := false.B)
    .elsewhen(s1_fire)(s1_valid := false.B)

  when(s2_flush)(s2_valid := false.B)
    .elsewhen(s1_fire)(s2_valid := !s1_flush)
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
    b.io.readReq.valid       := predictReqValid && s0_bankMask(i)
    b.io.readReq.bits.setIdx := s0_setIdx
  }

  /* --------------------------------------------------------------------------------------------------------------
     predict pipeline stage 1
     - get the latest start pc for compare tag in s2
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

  private val s2_setIdx     = RegEnable(s1_setIdx, s1_fire)
  private val s2_bankIdx    = RegEnable(s1_bankIdx, s1_fire)
  private val s2_bankMask   = RegEnable(s1_bankMask, s1_fire)
  private val s2_entries    = RegEnable(s1_entries, s1_fire)
  private val s2_startVAddr = RegEnable(s1_startVAddr, s1_fire)

  //  private val s2_entriesDelay1 = RegNext(s2_entries)

  private val s2_ctrResult = takenCounter(s2_bankIdx)(s2_setIdx).map(_.isPositive)

  private val s2_tag = getTag(s2_startVAddr)
  dontTouch(s2_tag)
//  private val s2_realEntries = Mux(RegNext(io.overrideValid), s2_entriesDelay1, s2_entries)
  private val s2_realEntries = s2_entries // TODO
  private val s2_hitMask     = s2_entries.map(entry => entry.valid && entry.tag === s2_tag)
  private val s2_hit         = s2_hitMask.reduce(_ || _)
  private val s2_takenMask   = VecInit(s2_hitMask.zip(s2_ctrResult).map { case (hit, taken) => hit && taken })
  private val s2_taken       = s2_takenMask.reduce(_ || _)

  private val s2_positions         = VecInit(s2_realEntries.map(_.position))
  private val s2_compareMatrix     = CompareMatrix(s2_positions)
  private val s2_firstTakenEntryOH = s2_compareMatrix.getLeastElementOH(s2_takenMask)
  private val s2_firstTakenEntry   = Mux1H(s2_firstTakenEntryOH, s2_realEntries)

  // When detect multi-hit, we need to invalidate one entry.
  private val (s2_multiHit, s2_multiHitWayIdx) = detectMultiHit(s2_hitMask, s2_positions)

  private val s2_target =
    getFullTarget(s2_startVAddr, s2_firstTakenEntry.targetLowerBits, s2_firstTakenEntry.targetCarry)

  io.prediction.taken       := s2_valid && s2_taken
  io.prediction.target      := s2_target
  io.prediction.attribute   := s2_firstTakenEntry.attribute
  io.prediction.cfiPosition := s2_firstTakenEntry.position

  io.meta.valid           := s2_valid
  io.meta.hitMask         := s2_hitMask
  io.meta.attributes      := s2_realEntries.map(_.attribute)
  io.meta.positions       := s2_positions
  io.meta.taken           := s2_taken
  io.meta.takenMaskOH     := s2_firstTakenEntryOH
  io.meta.targetLowerBits := s2_firstTakenEntry.targetLowerBits

  // used for check abtb output
  io.debug_startVAddr := s2_startVAddr

  replacers.zipWithIndex.foreach { case (r, i) =>
    r.io.readValid   := s2_valid && s2_hit && s2_bankMask(i)
    r.io.readSetIdx  := s2_setIdx
    r.io.readWayMask := s2_hitMask
  }

  /* --------------------------------------------------------------------------------------------------------------
     train pipeline stage 0
     - receive train request
     -------------------------------------------------------------------------------------------------------------- */

  private val t0_train = io.fastTrain.get.bits

  private val t0_valid = io.enable && io.fastTrain.get.valid && t0_train.abtbMeta.valid && io.previousVAddr.valid
  private val t0_previousVAddr = io.previousVAddr.bits

  /* --------------------------------------------------------------------------------------------------------------
     train pipeline stage 1
     - update taken counter
     - write a new entry or modify an existing entry if needed
     -------------------------------------------------------------------------------------------------------------- */

  private val t1_valid         = RegNext(t0_valid) & io.enable
  private val t1_train         = RegEnable(t0_train, t0_valid)
  private val t1_previousVAddr = RegEnable(t0_previousVAddr, t0_valid)

  private val t1_setIdx  = getSetIndex(t1_previousVAddr)
  private val t1_setMask = UIntToOH(t1_setIdx)

  private val t1_bankIdx  = getBankIndex(t1_previousVAddr)
  private val t1_bankMask = UIntToOH(t1_bankIdx)

  private val t1_meta = t1_train.abtbMeta

  private val t1_predictTaken           = t1_meta.taken
  private val t1_predictPosition        = Mux1H(t1_meta.takenMaskOH, t1_meta.positions)
  private val t1_predictAttribute       = Mux1H(t1_meta.takenMaskOH, t1_meta.attributes)
  private val t1_predictTargetLowerBits = t1_meta.targetLowerBits

  // use taken branch of s3 prediction to train abtb
  private val t1_trainTaken           = t1_train.finalPrediction.taken
  private val t1_trainPosition        = t1_train.finalPrediction.cfiPosition
  private val t1_trainAttribute       = t1_train.finalPrediction.attribute
  private val t1_trainTarget          = t1_train.finalPrediction.target
  private val t1_trainTargetLowerBits = getTargetLowerBits(t1_trainTarget)

  private val t1_positionBeforeMask = t1_meta.hitMask.zip(t1_meta.positions).zip(t1_meta.attributes).map {
    case ((hit, position), attribute) => hit && position < t1_trainPosition && attribute.isConditional
  }
  private val t1_positionEqualMask = t1_meta.hitMask.zip(t1_meta.positions).zip(t1_meta.attributes).map {
    case ((hit, position), attribute) => hit && position === t1_trainPosition && attribute.isConditional
  }

  takenCounter.zip(banks).zipWithIndex.foreach { case ((ctrsPerBank, bank), bankIdx) =>
    ctrsPerBank.zipWithIndex.foreach { case (ctrsPerSet, setIdx) =>
      ctrsPerSet.zip(t1_positionBeforeMask).zip(t1_positionEqualMask).zipWithIndex.foreach {
        case (((ctr, before), equal), wayIdx) =>
          val needReset = bank.io.writeResp.valid && bank.io.writeResp.bits.needResetCtr &&
            setIdx.U === bank.io.writeResp.bits.setIdx && wayIdx.U === bank.io.writeResp.bits.wayIdx

          val updateThisSet = t1_valid && t1_bankMask(bankIdx) && t1_setMask(setIdx)
          val needDecrease  = updateThisSet && (!t1_trainTaken || t1_trainTaken && before)
          val needIncrease  = updateThisSet && t1_trainTaken && equal

          when(needReset)(ctr.resetNeutral())
            .elsewhen(needDecrease)(ctr.decrease())
            .elsewhen(needIncrease)(ctr.increase())
      }
    }
  }

  // if the taken branch is not hit, we need write a new entry
  private val t1_hitTakenBranch = t1_meta.hitMask.zip(t1_meta.positions).zip(t1_meta.attributes).map {
    case ((hit, position), attribute) =>
      hit && position === t1_trainPosition && attribute === t1_trainAttribute
  }.reduce(_ || _) && t1_trainTaken
  private val t1_needWriteNewEntry = !t1_hitTakenBranch

  // If the target of indirect branch is wrong, we need correct it.
  // Since the entry only stores the lower bits of the target, we only need to check the lower bits.
  private val t1_needCorrectTarget = t1_predictTaken && t1_trainTaken &&
    t1_predictAttribute.isIndirect && t1_trainAttribute.isIndirect &&
    t1_predictPosition === t1_trainPosition &&
    t1_predictTargetLowerBits =/= t1_trainTargetLowerBits

  // TODO: if the attribute of the taken branch is wrong, we need replace it or invalidate it

  private val t1_writeEntry = Wire(new AheadBtbEntry)
  t1_writeEntry.valid           := true.B
  t1_writeEntry.tag             := getTag(t1_train.startVAddr)
  t1_writeEntry.position        := t1_trainPosition
  t1_writeEntry.attribute       := t1_trainAttribute
  t1_writeEntry.targetLowerBits := t1_trainTargetLowerBits
  t1_writeEntry.targetCarry.foreach(_ := getTargetCarry(t1_train.startVAddr, t1_trainTarget)) // if (EnableTargetFix)

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
      b.io.writeReq.bits.wayIdx       := OHToUInt(t1_meta.takenMaskOH)
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

  private val perf_directionWrong = t1_valid &&
    ((!t1_predictTaken && t1_trainTaken) || (t1_predictTaken && !t1_trainTaken))

  private val perf_missWrong = t1_valid && !t1_predictTaken && t1_trainTaken && !t1_hitTakenBranch

  private val perf_takenPositionWrong = t1_valid && t1_predictTaken && t1_trainTaken &&
    t1_predictPosition =/= t1_trainPosition

  private val perf_targetWrong = t1_valid && t1_predictTaken && t1_trainTaken &&
    t1_predictAttribute === t1_trainAttribute && t1_predictPosition === t1_trainPosition &&
    t1_predictTargetLowerBits =/= t1_trainTargetLowerBits

  private val perf_predictNotTakenRight = t1_valid && !t1_predictTaken && !t1_trainTaken

  private val perf_predictTakenRight = t1_valid && t1_predictTaken && t1_trainTaken &&
    t1_predictAttribute === t1_trainAttribute && t1_predictPosition === t1_trainPosition &&
    t1_predictTargetLowerBits === t1_trainTargetLowerBits

  private val perf_condTakenRight = t1_valid && t1_predictTaken && t1_trainTaken &&
    t1_predictAttribute.isConditional && t1_trainAttribute.isConditional && t1_predictPosition === t1_trainPosition

  private val perf_directRight = t1_valid && t1_predictTaken && t1_trainTaken &&
    t1_predictAttribute.isDirect && t1_trainAttribute.isDirect && t1_predictPosition === t1_trainPosition

  private val perf_indirectRight = t1_valid && t1_predictTaken && t1_trainTaken &&
    t1_predictAttribute.isIndirect && t1_trainAttribute.isIndirect && t1_predictPosition === t1_trainPosition &&
    t1_predictTargetLowerBits === t1_trainTargetLowerBits

  XSPerfAccumulate("predict_req_num", predictReqValid)
  XSPerfAccumulate("predict_num", s2_valid)
  XSPerfAccumulate("predict_hit", s2_valid && s2_hit)
  XSPerfAccumulate("predict_miss", s2_valid && !s2_hit)
  XSPerfAccumulate("predict_hit_entry_num", Mux(s2_valid, PopCount(s2_hitMask), 0.U))
  XSPerfAccumulate("predict_taken", s2_valid && s2_taken)
  XSPerfAccumulate("predict_not_taken", s2_valid && s2_hit && !s2_taken)
  XSPerfAccumulate("predict_multi_hit", s2_valid && s2_multiHit)

  XSPerfAccumulate("train_req_num", io.fastTrain.get.valid)
  XSPerfAccumulate("train_num", t1_valid)
  XSPerfAccumulate("train_hit_path", t1_valid && t1_meta.hitMask.reduce(_ || _))
  XSPerfAccumulate("train_hit_taken_branch", t1_valid && t1_hitTakenBranch)
  XSPerfAccumulate("train_predict_taken", t1_valid && t1_predictTaken)
  XSPerfAccumulate("train_predict_not_taken", t1_valid && t1_meta.hitMask.reduce(_ || _) && !t1_predictTaken)
  XSPerfAccumulate("train_actual_taken", t1_valid && t1_trainTaken)
  XSPerfAccumulate("train_actual_not_taken", t1_valid && !t1_trainTaken)

  XSPerfAccumulate("total_write", t1_valid && (t1_needWriteNewEntry || t1_needCorrectTarget) || s2_valid && s2_multiHit)
  XSPerfAccumulate("train_write_new_entry", t1_valid && t1_needWriteNewEntry)
  XSPerfAccumulate("train_correct_target", t1_valid && t1_needCorrectTarget)
  XSPerfAccumulate(
    "train_write_conflict",
    t1_valid && (t1_needWriteNewEntry || t1_needCorrectTarget) && s2_valid && s2_multiHit
  )

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
