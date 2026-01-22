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
import xiangshan.frontend.bpu.HasFastTrainIO
import xiangshan.frontend.bpu.Prediction

/**
 * This module is the implementation of the ahead BTB (Branch Target Buffer).
 */
class AheadBtb(implicit p: Parameters) extends BasePredictor with Helpers {
  class AheadBtbIO(implicit p: Parameters) extends BasePredictorIO with HasFastTrainIO {
    val redirectValid: Bool                   = Input(Bool())
    val overrideValid: Bool                   = Input(Bool())
    val prediction:    Vec[Valid[Prediction]] = Output(Vec(NumAheadBtbPredictionEntries, Valid(new Prediction)))
    val meta:          AheadBtbMeta           = Output(new AheadBtbMeta)
    val debug_startPc: PrunedAddr             = Output(PrunedAddr(VAddrBits))
  }
  val io: AheadBtbIO = IO(new AheadBtbIO)

  println(f"AheadBtb:")
  println(f"  Size(set, way, bank): $NumSets * $NumWays * $NumBanks = $NumEntries")
  println(f"  Address fields:")
  addrFields.show(indent = 4)

  private val banks     = Seq.tabulate(NumBanks)(i => Module(new AheadBtbBank(i)))
  private val replacers = Seq.fill(NumBanks)(Module(new AheadBtbReplacer))

  private val resetDone = RegInit(false.B)
  when(banks.map(_.io.readReq.ready).reduce(_ && _)) {
    resetDone := true.B
  }
  io.resetDone := resetDone

  io.trainReady := true.B

  private val takenCounter = RegInit(
    VecInit.fill(NumBanks)(
      VecInit.fill(NumSets)(
        VecInit.fill(NumWays)(TakenCounter.Zero)
      )
    )
  )

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
  private val predictionSent  = io.stageCtrl.s1_fire
  private val redirectValid   = io.redirectValid
  private val overrideValid   = io.overrideValid

  s0_fire := io.enable && predictReqValid
  s1_fire := io.enable && s1_valid && s2_ready && predictReqValid
  s2_fire := io.enable && s2_valid && predictionSent

  s1_ready := s1_fire || !s1_valid
  s2_ready := s2_fire || !s2_valid || overrideValid || redirectValid

  s2_flush := redirectValid
  s1_flush := s2_flush

  when(s0_fire)(s1_valid := true.B)
    .elsewhen(s1_flush)(s1_valid := false.B)
    .elsewhen(s1_fire)(s1_valid := false.B)

  when(s1_fire)(s2_valid := true.B)
    .elsewhen(s2_flush)(s2_valid := false.B)
    .elsewhen(s2_fire)(s2_valid := false.B)

  /* --------------------------------------------------------------------------------------------------------------
     predict pipeline stage 0
     - get set index and bank index
     - send read request to selected bank
     -------------------------------------------------------------------------------------------------------------- */

  private val s0_previousStartPc = io.startPc

  private val s0_setIdx   = getSetIndex(s0_previousStartPc)
  private val s0_bankIdx  = getBankIndex(s0_previousStartPc)
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

  private val s1_startPc = io.startPc

  private val s1_setIdx   = RegEnable(s0_setIdx, s0_fire)
  private val s1_bankIdx  = RegEnable(s0_bankIdx, s0_fire)
  private val s1_bankMask = RegEnable(s0_bankMask, s0_fire)

  private val s1_entries = Mux1H(s1_bankMask, banks.map(_.io.readResp.entries))

  /* --------------------------------------------------------------------------------------------------------------
     predict pipeline stage 2 / 3
     - get taken counter result
     - compare tag and get taken mask
     - compare positions and find first taken entry
     - get target from found entry
     - output prediction
     - stage 3 is only for fast prediction when override is valid
     -------------------------------------------------------------------------------------------------------------- */

  private val s3_setIdx   = RegInit(0.U.asTypeOf(s1_setIdx))
  private val s3_bankIdx  = RegInit(0.U.asTypeOf(s1_bankIdx))
  private val s3_bankMask = RegInit(0.U.asTypeOf(s1_bankMask))
  private val s3_entries  = RegInit(0.U.asTypeOf(s1_entries))
  private val s3_startPc  = RegInit(0.U.asTypeOf(s1_startPc))

  private val s2_setIdx   = RegEnable(Mux(overrideValid, s3_setIdx, s1_setIdx), s1_fire)
  private val s2_bankIdx  = RegEnable(Mux(overrideValid, s3_bankIdx, s1_bankIdx), s1_fire)
  private val s2_bankMask = RegEnable(Mux(overrideValid, s3_bankMask, s1_bankMask), s1_fire)
  private val s2_entries  = RegEnable(Mux(overrideValid, s3_entries, s1_entries), s1_fire)
  private val s2_startPc  = RegEnable(s1_startPc, s1_fire)

  when(s2_fire) {
    s3_setIdx   := s2_setIdx
    s3_bankIdx  := s2_bankIdx
    s3_bankMask := s2_bankMask
    s3_entries  := s2_entries
    s3_startPc  := s2_startPc
  }

  private val s2_ctrResult = takenCounter(s2_bankIdx)(s2_setIdx).map(_.isPositive)

  private val s2_tag = getTag(s2_startPc)
  dontTouch(s2_tag)
  private val s2_hitMask = s2_entries.map(entry => entry.valid && entry.tag === s2_tag)
  private val s2_hit     = s2_hitMask.reduce(_ || _)

  // When detect multi-hit, we need to invalidate one entry.
  private val (s2_multiHit, s2_multiHitWayIdx) = detectMultiHit(s2_hitMask, s2_entries.map(_.position))

  io.prediction.zipWithIndex.foreach { case (pred, i) =>
    pred.valid            := s2_valid && s2_hitMask(i)
    pred.bits.taken       := s2_ctrResult(i)
    pred.bits.cfiPosition := s2_entries(i).position
    pred.bits.attribute   := s2_entries(i).attribute
    pred.bits.target      := getFullTarget(s2_startPc, s2_entries(i).targetLowerBits, s2_entries(i).targetCarry)
  }

  io.meta.valid    := s2_valid
  io.meta.setIdx   := s2_setIdx
  io.meta.bankMask := s2_bankMask
  io.meta.entries.zipWithIndex.foreach { case (e, i) =>
    e.hit             := s2_hitMask(i)
    e.attribute       := s2_entries(i).attribute
    e.position        := s2_entries(i).position
    e.targetLowerBits := s2_entries(i).targetLowerBits
  }

  // used for check abtb output
  io.debug_startPc := s2_startPc

  replacers.zipWithIndex.foreach { case (r, i) =>
    r.io.readValid   := s2_valid && s2_taken && s2_bankMask(i)
    r.io.readSetIdx  := s2_setIdx
    r.io.readWayMask := s2_takenMask
  }

  /* --------------------------------------------------------------------------------------------------------------
     train pipeline stage 0
     - receive train request
     -------------------------------------------------------------------------------------------------------------- */

  private val t0_train = io.fastTrain.get.bits

  private val t0_fire = io.enable && io.fastTrain.get.valid && t0_train.finalPrediction.taken && t0_train.abtbMeta.valid

  /* --------------------------------------------------------------------------------------------------------------
     train pipeline stage 1
     - update taken counter
     - write a new entry or modify an existing entry if needed
     -------------------------------------------------------------------------------------------------------------- */

  private val t1_fire  = RegNext(t0_fire, init = false.B)
  private val t1_train = RegEnable(t0_train, t0_fire)

  private val t1_meta = t1_train.abtbMeta

  private val t1_setIdx   = t1_meta.setIdx
  private val t1_setMask  = UIntToOH(t1_setIdx)
  private val t1_bankMask = t1_meta.bankMask

  // use taken branch of s3 prediction to train abtb
  private val t1_trainTaken           = t1_train.finalPrediction.taken
  private val t1_trainPosition        = t1_train.finalPrediction.cfiPosition
  private val t1_trainAttribute       = t1_train.finalPrediction.attribute
  private val t1_trainTarget          = t1_train.finalPrediction.target
  private val t1_trainTargetLowerBits = getTargetLowerBits(t1_trainTarget)

  private val t1_condMask           = t1_meta.entries.map(e => e.hit && e.attribute.isConditional)
  private val t1_positionBeforeMask = t1_meta.entries.map(_.position < t1_trainPosition)
  private val t1_positionEqualMask  = t1_meta.entries.map(_.position === t1_trainPosition)

  takenCounter.zip(banks).zipWithIndex.foreach { case ((ctrsPerBank, bank), bankIdx) =>
    ctrsPerBank.zipWithIndex.foreach { case (ctrsPerSet, setIdx) =>
      val updateThisSet = t1_fire && t1_bankMask(bankIdx) && t1_setMask(setIdx)
      ctrsPerSet.zipWithIndex.foreach { case (ctr, wayIdx) =>
        val isCond    = t1_condMask(wayIdx)
        val posBefore = t1_positionBeforeMask(wayIdx)
        val posEqual  = t1_positionEqualMask(wayIdx)

        val needReset = bank.io.writeResp.valid && bank.io.writeResp.bits.needResetCtr &&
          setIdx.U === bank.io.writeResp.bits.setIdx && wayIdx.U === bank.io.writeResp.bits.wayIdx
        val needDecrease = updateThisSet && isCond && (!t1_trainTaken || t1_trainTaken && posBefore)
        val needIncrease = updateThisSet && isCond && t1_trainTaken && posEqual

        when(needReset)(ctr.resetWeakPositive())
          .elsewhen(needDecrease)(ctr.selfDecrease())
          .elsewhen(needIncrease)(ctr.selfIncrease())
      }
    }
  }

  // if the taken branch is not hit, we need write a new entry
  private val t1_hitMask = t1_meta.entries.map { e =>
    e.hit && e.position === t1_trainPosition && e.attribute === t1_trainAttribute
  }
  private val t1_hit               = t1_hitMask.reduce(_ || _)
  private val t1_needWriteNewEntry = !t1_hit

  // If the target of indirect branch is wrong, we need correct it.
  // Since the entry only stores the lower bits of the target, we only need to check the lower bits.
  private val t1_hitMaskOH         = PriorityEncoderOH(t1_hitMask)
  private val t1_predictInfo       = Mux1H(t1_hitMaskOH, t1_meta.entries)
  private val t1_targetDiff        = t1_predictInfo.targetLowerBits =/= t1_trainTargetLowerBits
  private val t1_needCorrectTarget = t1_hit && t1_trainAttribute.isIndirect && t1_targetDiff

  // TODO: if the attribute of the taken branch is wrong, we need replace it or invalidate it

  private val t1_writeEntry = Wire(new AheadBtbEntry)
  t1_writeEntry.valid           := true.B
  t1_writeEntry.tag             := getTag(t1_train.startPc)
  t1_writeEntry.position        := t1_trainPosition
  t1_writeEntry.attribute       := t1_trainAttribute
  t1_writeEntry.targetLowerBits := t1_trainTargetLowerBits
  t1_writeEntry.targetCarry.foreach(_ := getTargetCarry(t1_train.startPc, t1_trainTarget)) // if (EnableTargetFix)

  replacers.foreach(_.io.replaceSetIdx := t1_setIdx)
  private val victimWayIdx = replacers.map(_.io.victimWayIdx)

  banks.zipWithIndex.foreach { case (b, i) =>
    when(t1_fire && t1_needWriteNewEntry && t1_bankMask(i)) {
      b.io.writeReq.valid             := true.B
      b.io.writeReq.bits.needResetCtr := true.B
      b.io.writeReq.bits.setIdx       := t1_setIdx
      b.io.writeReq.bits.wayIdx       := victimWayIdx(i)
      b.io.writeReq.bits.entry        := t1_writeEntry
    }.elsewhen(t1_fire && t1_needCorrectTarget && t1_bankMask(i)) {
      b.io.writeReq.valid             := true.B
      b.io.writeReq.bits.needResetCtr := false.B
      b.io.writeReq.bits.setIdx       := t1_setIdx
      b.io.writeReq.bits.wayIdx       := OHToUInt(t1_hitMaskOH)
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

  XSPerfAccumulate("predict_req_num", predictReqValid)
  XSPerfAccumulate("predict_num", s2_fire)
  XSPerfAccumulate("predict_hit", s2_fire && s2_hit)
  XSPerfAccumulate("predict_miss", s2_fire && !s2_hit)
  XSPerfAccumulate("predict_hit_entry_num", Mux(s2_fire, PopCount(s2_hitMask), 0.U))
  XSPerfAccumulate("predict_multi_hit", s2_fire && s2_multiHit)

  XSPerfAccumulate("train_req_num", io.fastTrain.get.valid)
  XSPerfAccumulate("train_num", t1_fire)
  XSPerfAccumulate("train_actual_taken", t1_fire && t1_trainTaken)
  XSPerfAccumulate("train_actual_not_taken", t1_fire && !t1_trainTaken)

  XSPerfAccumulate("total_write", t1_fire && (t1_needWriteNewEntry || t1_needCorrectTarget) || s2_valid && s2_multiHit)
  XSPerfAccumulate("train_write_new_entry", t1_fire && t1_needWriteNewEntry)
  XSPerfAccumulate("train_correct_target", t1_fire && t1_needCorrectTarget)
  XSPerfAccumulate(
    "train_write_conflict",
    t1_fire && (t1_needWriteNewEntry || t1_needCorrectTarget) && s2_valid && s2_multiHit
  )
}
