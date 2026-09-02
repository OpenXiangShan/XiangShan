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
import xiangshan.frontend.GuardedPc
import xiangshan.frontend.bpu.BasePredictor
import xiangshan.frontend.bpu.BasePredictorIO
import xiangshan.frontend.bpu.HasAheadPredictorIO
import xiangshan.frontend.bpu.HasFastTrainIO
import xiangshan.frontend.bpu.history.phr.PhrAllFoldedHistories

/**
 * This module is the implementation of the ahead BTB (Branch Target Buffer).
 */
class AheadBtb(implicit p: Parameters) extends BasePredictor with Helpers {
  class AheadBtbIO(implicit p: Parameters) extends BasePredictorIO with HasAheadPredictorIO with HasFastTrainIO {
    val normalPathHist: PhrAllFoldedHistories = Input(new PhrAllFoldedHistories(AllFoldedHistoryInfo))

    val result: AheadBtbResult = Output(new AheadBtbResult)
    val meta:   AheadBtbMeta   = Output(new AheadBtbMeta)

    val toMicroTage: AheadBtbToMicroTageIO = Output(new AheadBtbToMicroTageIO)

    val debug_bpuS2StartPc: Option[GuardedPc] = Option.when(!env.FPGAPlatform)(Input(GuardedPc()))
    val debug_bpuS3StartPc: Option[GuardedPc] = Option.when(!env.FPGAPlatform)(Input(GuardedPc()))
  }
  val io: AheadBtbIO = IO(new AheadBtbIO)

  println(f"AheadBtb:")
  println(f"  Size(set, way, bank): $NumSets * $NumWays * $NumBanks = $NumEntries")
  println(f"  Address fields:")
  addrFields.show(indent = 4)

  private val banks     = Seq.tabulate(NumBanks)(i => Module(new AheadBtbBank(i)))
  private val replacers = Seq.fill(NumBanks)(Module(new AheadBtbReplacer))

  io.sramResetDone := banks.map(_.io.sramResetDone).reduce(_ && _)

  io.trainReady := true.B

  private val takenCounter = RegInit(
    VecInit.fill(NumBanks)(
      VecInit.fill(NumSets)(
        VecInit.fill(NumWays)(TakenCounter.Zero)
      )
    )
  )

  /* --------------------------------------------------------------------------------------------------------------
     predict pipeline control
     -------------------------------------------------------------------------------------------------------------- */

  private val s0_fire = Wire(Bool())
  private val s1_fire = Wire(Bool())
  private val s2_fire = Wire(Bool())

  private val s2_ready = Wire(Bool())

  private val s1_valid = RegInit(false.B)
  private val s2_valid = RegInit(false.B)

  private val s2_state = RegInit(0.U.asTypeOf(new PipelineState))

  private val bpuS3ReplayState = RegInit(0.U.asTypeOf(Valid(new PipelineState)))

  private val bpuS0Fire = io.stageCtrl.s0_fire
  private val bpuS1Fire = io.stageCtrl.s1_fire
  private val bpuS2Fire = io.stageCtrl.s2_fire

  /* --------------------------------------------------------------------------------------------------------------
     predict pipeline stage 0
     - get set index and bank index
     - send read request to selected bank
     -------------------------------------------------------------------------------------------------------------- */

  s0_fire := io.enable && bpuS0Fire

  private val s0_simpleHash = io.normalPathHist.getHistWithInfo(AbtbHashFhInfo).foldedHist(AheadBtbHashBitWidth - 1, 0)
  private val s0_hashIndex  = io.startPc(log2Ceil(NumEntries / NumWays) - 1, 0) ^ s0_simpleHash
  private val s0_setIdx     = s0_hashIndex(log2Ceil(NumEntries / NumWays) - 1, log2Ceil(NumBanks))
  private val s0_bankIdx    = s0_hashIndex(log2Ceil(NumBanks) - 1, 0)
  private val s0_bankMask   = UIntToOH(s0_bankIdx)

  banks.zipWithIndex.foreach { case (b, i) =>
    b.io.readReq.valid       := s0_fire && s0_bankMask(i)
    b.io.readReq.bits.setIdx := s0_setIdx
  }

  /* --------------------------------------------------------------------------------------------------------------
     predict pipeline stage 1
     - receive the SRAM entries
     - use the normal-path start PC to match the SRAM entries
     - use the override target to rematch old S2 data or the BPU S3 replay state
     - select the normal or override result and prepare the complete S2 state
     -------------------------------------------------------------------------------------------------------------- */

  s1_fire := io.enable && s1_valid && s2_ready && bpuS0Fire

  when(!io.enable) {
    s1_valid := false.B
  }.elsewhen(s0_fire) {
    s1_valid := true.B
  }.elsewhen(io.redirect || s1_fire) {
    s1_valid := false.B
  }

  private val s1_setIdx       = RegEnable(s0_setIdx, s0_fire)
  private val s1_bankIdx      = RegEnable(s0_bankIdx, s0_fire)
  private val s1_bankMask     = RegEnable(s0_bankMask, s0_fire)
  private val s1_debugIndexPc = Option.when(!env.FPGAPlatform)(RegEnable(io.startPc, s0_fire))

  private val s1_entries = Mux1H(s1_bankMask, banks.map(_.io.readResp.entries))
  private val s1_ctrVec  = takenCounter(s1_bankIdx)(s1_setIdx)

  private val s1_normalState = WireInit(0.U.asTypeOf(new PipelineState))
  s1_normalState.startPc   := io.newStartPc
  s1_normalState.setIdx    := s1_setIdx
  s1_normalState.bankIdx   := s1_bankIdx
  s1_normalState.bankMask  := s1_bankMask
  s1_normalState.entries   := s1_entries
  s1_normalState.ctrVec    := s1_ctrVec
  s1_normalState.hitMask   := s1_normalState.getHitMask
  s1_normalState.isJumpVec := s1_normalState.getIsJumpVec
  s1_normalState.isCondVec := s1_normalState.getIsCondVec
  s1_normalState.debug_indexPc.zip(s1_debugIndexPc).foreach { case (statePc, indexPc) => statePc := indexPc }

  // compute hit mask for the override state, which is used to rematch the old S2 state or the BPU S3 replay state
  private val s1_overrideState = WireInit(Mux(io.bpuS3Override, bpuS3ReplayState.bits, s2_state))
  s1_overrideState.startPc   := io.overrideStartPc
  s1_overrideState.hitMask   := s1_overrideState.getHitMask
  s1_overrideState.isJumpVec := s1_overrideState.getIsJumpVec
  s1_overrideState.isCondVec := s1_overrideState.getIsCondVec

  private val s1_state = Mux(io.bpuS3Override || io.bpuS2Override, s1_overrideState, s1_normalState)

  s2_state.debug_indexPc.zip(io.debug_bpuS2StartPc).foreach { case (indexPc, bpuStartPc) =>
    when(io.enable && !io.redirect && io.bpuS2Override && !io.bpuS3Override && s2_valid) {
      assert(indexPc === bpuStartPc, "AheadBtb S2 override reuses entries indexed by the wrong PC")
    }
  }
  bpuS3ReplayState.bits.debug_indexPc.zip(io.debug_bpuS3StartPc).foreach { case (indexPc, bpuStartPc) =>
    when(io.enable && !io.redirect && io.bpuS3Override && bpuS3ReplayState.valid) {
      assert(indexPc === bpuStartPc, "AheadBtb S3 override reuses entries indexed by the wrong PC")
    }
  }

  io.toMicroTage.fastPositions := s1_state.entries.map(_.position)

  /* --------------------------------------------------------------------------------------------------------------
     predict pipeline stage 2
     - latch the normal or replay state prepared in S1
     - output the prediction
     -------------------------------------------------------------------------------------------------------------- */

  s2_ready := s2_fire || !s2_valid || io.bpuS2Override || io.bpuS3Override || io.redirect

  when(s1_fire || io.bpuS2Override || io.bpuS3Override) {
    s2_state := s1_state
  }

  when(!io.enable || io.redirect) {
    s2_valid := false.B
  }.elsewhen(io.bpuS3Override) {
    s2_valid := bpuS3ReplayState.valid
  }.elsewhen(s1_fire && !io.bpuS2Override) {
    s2_valid := true.B
  }.elsewhen(s2_fire) {
    s2_valid := false.B
  }

  s2_fire := s2_valid && bpuS1Fire && !io.bpuS3Override && !io.bpuS2Override && !io.redirect

  // When detect multi-hit, we need to invalidate one entry.
  private val (s2_multiHit, s2_multiHitWayIdx) =
    detectMultiHit(s2_state.hitMask, s2_state.entries.map(_.position))

  io.result.entries.zipWithIndex.foreach { case (pred, i) =>
    pred.valid            := s2_valid && s2_state.hitMask(i)
    pred.bits.taken       := s2_state.ctrVec(i).isPositive
    pred.bits.cfiPosition := s2_state.entries(i).position
    pred.bits.attribute   := s2_state.entries(i).attribute
    pred.bits.target :=
      getFullTarget(s2_state.startPc, s2_state.entries(i).targetLowerBits, s2_state.entries(i).targetCarry)
  }
  io.result.isJumpVec := s2_state.isJumpVec
  io.result.isCondVec := s2_state.isCondVec
  io.toMicroTage.result.zipWithIndex.foreach { case (pred, i) =>
    pred.valid             := s2_valid && s2_state.hitMask(i)
    pred.bits.taken        := s2_state.ctrVec(i).isPositive
    pred.bits.cfiPosition  := s2_state.entries(i).position
    pred.bits.attribute    := s2_state.entries(i).attribute
    pred.bits.isStrongBias := s2_state.ctrVec(i).isSaturate
  }
  io.meta.valid    := s2_valid
  io.meta.setIdx   := s2_state.setIdx
  io.meta.bankMask := s2_state.bankMask
  io.meta.entries.zipWithIndex.foreach { case (e, i) =>
    e.hit             := s2_state.hitMask(i)
    e.attribute       := s2_state.entries(i).attribute
    e.position        := s2_state.entries(i).position
    e.targetLowerBits := s2_state.entries(i).targetLowerBits
  }

  // used for check abtb output
  io.result.debug_startPc.foreach(_ := s2_state.startPc)

  /* --------------------------------------------------------------------------------------------------------------
     old state for bpu s3 override
     -------------------------------------------------------------------------------------------------------------- */

  when(!io.enable || io.redirect || io.bpuS3Override) {
    bpuS3ReplayState.valid := false.B
  }.elsewhen(bpuS2Fire) {
    bpuS3ReplayState.valid := s2_valid
    bpuS3ReplayState.bits  := s2_state
  }

  /* --------------------------------------------------------------------------------------------------------------
     train pipeline stage 0
     - receive train request
     -------------------------------------------------------------------------------------------------------------- */

  private val t0_train = io.fastTrain.get.bits

  private val t0_fire = io.enable && io.fastTrain.get.valid && t0_train.abtbMeta.valid

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

  // A taken final prediction can allocate/correct an entry. A not-taken
  // prediction only updates counters of the conditional entries that hit.
  private val t1_trainTaken           = t1_train.branch.taken
  private val t1_trainPosition        = t1_train.branch.cfiPosition
  private val t1_trainAttribute       = t1_train.branch.attribute
  private val t1_trainTarget          = t1_train.branch.target
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

        // For timing purposes, the indirect jump branch in the abtb comparison matrix relies on
        // the default CTR assignment and omits extra attribute checks.
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
  private val t1_needWriteNewEntry = !t1_hit && t1_trainTaken

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

  private val t2_fire              = RegNext(t1_fire, init = false.B)
  private val t2_victimWayIdx      = RegNext(VecInit(victimWayIdx))
  private val t2_setIdx            = RegNext(t1_setIdx)
  private val t2_bankMask          = RegNext(t1_bankMask)
  private val t2_hitMaskOH         = RegNext(VecInit(t1_hitMaskOH))
  private val t2_needWriteNewEntry = RegNext(t1_needWriteNewEntry)
  private val t2_needCorrectTarget = RegNext(t1_needCorrectTarget)
  private val t2_writeEntry        = RegNext(t1_writeEntry)
  private val t2_hit               = RegNext(t1_hit)
  private val t2_hitMask           = RegNext(VecInit(t1_hitMask))
  private val t2_trainTaken        = RegNext(t1_trainTaken)

  banks.zipWithIndex.foreach { case (b, i) =>
    when(t2_fire && t2_needWriteNewEntry && t2_bankMask(i)) {
      b.io.writeReq.valid             := true.B
      b.io.writeReq.bits.needResetCtr := true.B
      b.io.writeReq.bits.setIdx       := t2_setIdx
      b.io.writeReq.bits.wayIdx       := t2_victimWayIdx(i)
      b.io.writeReq.bits.entry        := t2_writeEntry
    }.elsewhen(t2_fire && t2_needCorrectTarget && t2_bankMask(i)) {
      b.io.writeReq.valid             := true.B
      b.io.writeReq.bits.needResetCtr := false.B
      b.io.writeReq.bits.setIdx       := t2_setIdx
      b.io.writeReq.bits.wayIdx       := OHToUInt(t2_hitMaskOH)
      b.io.writeReq.bits.entry        := t2_writeEntry
    }.elsewhen(RegNext(s2_fire && s2_multiHit && s2_state.bankMask(i))) { // delay 1 cycle for timing
      b.io.writeReq.valid             := true.B
      b.io.writeReq.bits.needResetCtr := true.B
      b.io.writeReq.bits.setIdx       := s2_state.setIdx
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
  // update replacer -- Allocation touch and training touch are triggered at different times,
  // so they cannot share the same interface.
  replacers.zipWithIndex.foreach { case (r, i) =>
    r.io.readValid   := t2_fire && t2_trainTaken && t2_bankMask(i) && t2_hit
    r.io.readSetIdx  := t2_setIdx
    r.io.readWayMask := t2_hitMask
  }

  /* --------------------------------------------------------------------------------------------------------------
     performance counter
     -------------------------------------------------------------------------------------------------------------- */

  XSPerfAccumulate("predict_req_num", s0_fire)
  XSPerfAccumulate("predict_num", s2_fire)
  XSPerfAccumulate("predict_hit", s2_fire && s2_state.hitMask.reduce(_ || _))
  XSPerfAccumulate("predict_miss", s2_fire && !s2_state.hitMask.reduce(_ || _))
  XSPerfAccumulate("predict_hit_entry_num", Mux(s2_fire, PopCount(s2_state.hitMask), 0.U))
  XSPerfAccumulate("predict_multi_hit", s2_fire && s2_multiHit)

  XSPerfAccumulate("train_req_num", io.fastTrain.get.valid)
  XSPerfAccumulate("train_num", t1_fire)
  XSPerfAccumulate("train_actual_taken", t1_fire && t1_trainTaken)
  XSPerfAccumulate("train_actual_not_taken", t1_fire && !t1_trainTaken)

  XSPerfAccumulate(
    "total_write",
    t1_fire && (t1_needWriteNewEntry || t1_needCorrectTarget) || s2_fire && s2_multiHit
  )
  XSPerfAccumulate("train_write_new_entry", t1_fire && t1_needWriteNewEntry)
  XSPerfAccumulate("train_correct_target", t1_fire && t1_needCorrectTarget)
  XSPerfAccumulate(
    "train_write_conflict",
    t1_fire && (t1_needWriteNewEntry || t1_needCorrectTarget) && s2_fire && s2_multiHit
  )
}
