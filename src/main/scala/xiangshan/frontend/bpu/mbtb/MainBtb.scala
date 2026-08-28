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
import utility.ChiselDB
import utility.XSPerfAccumulate
import utility.XSPerfHistogram
import utils.VecRotate
import xiangshan.frontend.bpu.BasePredictor
import xiangshan.frontend.bpu.BasePredictorIO
import xiangshan.frontend.bpu.Prediction

class MainBtb(implicit p: Parameters) extends BasePredictor with HasMainBtbParameters with Helpers {
  class MainBtbIO(implicit p: Parameters) extends BasePredictorIO {
    // prediction specific bundle
    val result: Vec[Valid[Prediction]] = Output(Vec(NumBtbResultEntries, Valid(new Prediction)))
    val meta:   MainBtbMeta            = Output(new MainBtbMeta)

    // timing optimization: send positions earlier to TAGE
    val s1_positions:  Vec[UInt] = Output(Vec(NumBtbResultEntries, UInt(CfiPositionWidth.W)))
    val s1_maxBankIdx: UInt      = Output(UInt(AlignBankIdxLen.W))

    // final s3_takenMask (mbtb + tage + sc), used to touch replacer accurately
    val s3_takenMask: Vec[Bool] = Input(Vec(NumBtbResultEntries, Bool()))
  }

  val io: MainBtbIO = IO(new MainBtbIO)

  // print params
  println(f"MainBtb:")
  println(f"  Size(set, way, align, internal): $NumSets * $NumWay * $NumAlignBanks * $NumInternalBanks = $NumEntries")
  println(f"  Address fields:")
  addrFields.show(indent = 4)

  /* *** submodules *** */
  private val alignBanks = Seq.tabulate(NumAlignBanks)(alignIdx => Module(new MainBtbAlignBank(alignIdx)))

  io.sramResetDone := alignBanks.map(_.io.sramResetDone).reduce(_ && _)

  io.trainReady := true.B

  private val s0_fire, s1_fire, s2_fire, s3_fire, t0_fire = Wire(Bool())
  alignBanks.foreach { b =>
    b.io.stageCtrl.s0_fire := s0_fire
    b.io.stageCtrl.s1_fire := s1_fire
    b.io.stageCtrl.s2_fire := s2_fire
    b.io.stageCtrl.s3_fire := s3_fire
    b.io.stageCtrl.t0_fire := t0_fire
  }

  /* *** s0 ***
   * calculate per-bank startPc and posHigherBits
   * send read request to alignBanks
   */
  s0_fire := io.stageCtrl.s0_fire && io.enable
  private val s0_startPc = io.startPc
  // rotate read addresses according to the first align bank index
  // e.g. if NumAlignBanks = 4, startPc locates in alignBank 1,
  // startPc + (i << FetchBlockAlignWidth) will be located in alignBank (1 + i) % 4,
  // i.e. we have VecInit.tabulate(...)'s alignBankIdx = (1, 2, 3, 0),
  // they always needs to goes to physical alignBank (0, 1, 2, 3),
  // so we need to rotate it right by 1.
  private val s0_rotator = VecRotate(getAlignBankIndex(s0_startPc))
  private val s0_startPcVec = s0_rotator.rotate(
    VecInit.tabulate(NumAlignBanks) { i =>
      if (i == 0)
        s0_startPc // keep lower bits for the first one
      else
        getAlignedPc(s0_startPc + (i << FetchBlockAlignWidth).U) // use aligned for others
    }
  )
  private val s0_posHigherBitsVec = s0_rotator.rotate(VecInit.tabulate(NumAlignBanks)(_.U(AlignBankIdxLen.W)))

  alignBanks.zipWithIndex.foreach { case (b, i) =>
    b.io.read.req.startPc       := s0_startPcVec(i)
    b.io.read.req.posHigherBits := s0_posHigherBitsVec(i)
    b.io.read.req.crossPage     := isCrossPage(s0_startPcVec(i), s0_startPc)
  }

  /* *** s1 ***
   * just wait alignBanks
   */
  s1_fire := io.stageCtrl.s1_fire && io.enable
  private val s1_rotator = RegEnable(s0_rotator, s0_fire)
  // Build the logical bank masks in S1 from s1_rotator instead of registering themselves.
  // This keeps the one-hot relation visible to synthesis. When NumAlignBanks = 2,
  // s1_maxBankIdx will be optimized to ~(alignBanks.map(_.io.read.s1_victimHit).reduce(_ || _))
  private val s1_posHigherBitsVec = s1_rotator.rotate(VecInit.tabulate(NumAlignBanks)(_.U(AlignBankIdxLen.W)))
  private val s1_logicBankMaskVec = VecInit.tabulate(NumAlignBanks) { i =>
    s1_rotator.rotate(VecInit(UIntToOH(i.U(AlignBankIdxLen.W), NumAlignBanks).asBools))
  }

  private val s1_logicBankVictimHit =
    VecInit.tabulate(NumAlignBanks)(i => Mux1H(s1_logicBankMaskVec(i), alignBanks.map(_.io.read.s1_victimHit)))
  private val s1_logicBankVictimPositions =
    VecInit.tabulate(NumAlignBanks)(i => Mux1H(s1_logicBankMaskVec(i), alignBanks.map(_.io.read.s1_victimPositions)))

  // Re-map VBTB hits from physical AlignBank order back to logical fetch-block order.
  // The override policy is defined in this logical order so that "earlier" and "later"
  // banks always mean lower and higher positions within the current fetch block, regardless
  // of the rotated physical bank mapping used by the SRAM read.
  //
  // For each logical bank with a VBTB hit, place the victim prediction from the back of
  // the fetch block toward the front. A hit in logical bank i can only override a later
  // output bank; otherwise it would move the CFI earlier than its original bank and break
  // the fetch-block ordering seen by the other predictors.
  //
  // hitCount(i) is the number of VBTB hits before logical bank i. Its bitwise inverse is
  // NumAlignBanks - 1 - hitCount(i), i.e. the next output slot allocated from the tail of
  // the fetch block. When i < overrideIdx(i), the VBTB prediction can be placed in that
  // later slot. When i >= overrideIdx(i), no later slot is available, so the current bank
  // is treated as lost and its prediction is invalidated for this fetch block.
  private val s1_logicVictimHitCount =
    VecInit.tabulate(NumAlignBanks)(i => PopCount(s1_logicBankVictimHit.take(i)).take(AlignBankIdxLen))
  private val s1_logicVictimOverrideIdx = VecInit(s1_logicVictimHitCount.map(count => (~count).asUInt))
  private val s1_logicVictimOverrideMask = VecInit.tabulate(NumAlignBanks) { i =>
    s1_logicBankVictimHit(i) && i.U < s1_logicVictimOverrideIdx(i)
  }
  private val s1_logicInvalidBankMask = VecInit.tabulate(NumAlignBanks) { i =>
    s1_logicBankVictimHit(i) && i.U >= s1_logicVictimOverrideIdx(i)
  }
  private val s1_logicOverriddenBankMask = VecInit.tabulate(NumAlignBanks) { i =>
    Mux(s1_logicVictimOverrideMask(i), UIntToOH(s1_logicVictimOverrideIdx(i), NumAlignBanks), 0.U(NumAlignBanks.W))
  }.reduce(_ | _)

  // A logical bank is lost when either its own VBTB hit cannot be placed, or its original
  // MainBtb output slot is consumed by another bank's VBTB prediction. Lost banks shrink
  // the range in which uBTB/aBTB/fall-through predictions are allowed to contribute.
  private val s1_logicLostBankMask = s1_logicInvalidBankMask.asUInt | s1_logicOverriddenBankMask

  private val s1_finalPositions               = Wire(Vec(NumAlignBanks, Vec(NumWay, UInt(CfiPositionWidth.W))))
  private val s1_logicVictimOverrideHitMatrix = Wire(Vec(NumAlignBanks, Vec(NumAlignBanks, Bool())))

  Seq.tabulate(NumAlignBanks) { i =>
    val logicIdx = s1_posHigherBitsVec(i)
    // For the physical AlignBank i, find whether any logical bank's VBTB hit decided to
    // override this bank's logical position. The hit matrix is carried to S2 so the final
    // prediction/meta selection uses exactly the same S1 placement decision as positions.
    val hitVec = VecInit.tabulate(NumAlignBanks) { j =>
      s1_logicVictimOverrideMask(j) && s1_logicVictimOverrideIdx(j) === logicIdx
    }
    val hit             = hitVec.reduce(_ || _)
    val positions       = alignBanks(i).io.read.s1_positions
    val victimPositions = Mux1H(hitVec, s1_logicBankVictimPositions)
    s1_logicVictimOverrideHitMatrix(i) := hitVec
    s1_finalPositions(i)               := Mux(hit, victimPositions, positions)
  }

  private val s1_logicLostCount = PopCount(s1_logicLostBankMask).take(AlignBankIdxLen)

  // s1_maxBankIdx tells other S1 predictors how far their predictions may be used after
  // VBTB overrides shrink the usable fetch-block range. With NumAlignBanks banks, losing
  // N banks leaves logical banks [0, NumAlignBanks - 1 - N] available; the bitwise inverse
  // of lostCount encodes that maximum index for the fixed AlignBankIdxLen width. The BPU
  // top uses this to suppress uBTB/aBTB results beyond the usable range and to recompute
  // fall-through consistently with the final MainBtb layout.
  io.s1_maxBankIdx := (~s1_logicLostCount).asUInt
  // TAGE consumes positions in S1 before final MainBtb predictions are produced in S2.
  // Therefore overridden banks expose the VBTB positions already here, keeping the S1
  // position vector consistent with the S2 prediction/meta vector selected below.
  io.s1_positions := s1_finalPositions.flatten

  /* *** s2 ***
   * receive read response from alignBanks
   * send out prediction result and meta info
   */
  s2_fire := io.stageCtrl.s2_fire && io.enable
  private val s2_logicBankMaskVec             = RegEnable(s1_logicBankMaskVec, s1_fire)
  private val s2_posHigherBitsVec             = RegEnable(s1_posHigherBitsVec, s1_fire)
  private val s2_logicVictimOverrideIdx       = RegEnable(s1_logicVictimOverrideIdx, s1_fire)
  private val s2_logicVictimOverrideMask      = RegEnable(s1_logicVictimOverrideMask, s1_fire)
  private val s2_logicVictimOverrideHitMatrix = RegEnable(s1_logicVictimOverrideHitMatrix, s1_fire)

  private val s2_logicInvalidBankMask    = RegEnable(s1_logicInvalidBankMask, s1_fire)
  private val s2_logicOverriddenBankMask = RegEnable(s1_logicOverriddenBankMask, s1_fire)

  private val s2_logicLostBankMask = s2_logicInvalidBankMask.asUInt | s2_logicOverriddenBankMask

  private val s2_logicBankResp =
    VecInit.tabulate(NumAlignBanks)(i => Mux1H(s2_logicBankMaskVec(i), alignBanks.map(_.io.read.resp)))

  private val s2_logicBankVictimPredictions = s2_logicBankResp.map(_.victimPredictions)
  private val s2_logicBankVictimMetas       = s2_logicBankResp.map(_.victimMetas)

  // we don't care about the order of alignBanks' responses,
  // (as s0_posHigherBitsVec is already computed and concatenated to each entry's posLowerBits)
  // (and we care about the full position when searching for a matching entry, not the bank it comes from)
  // so here we just flatten them, without rotating them back to the original order

  private val s2_invalidPredictions = WireInit(VecInit(alignBanks.map(_.io.read.resp.predictions)))
  private val s2_invalidMetas       = WireInit(VecInit(alignBanks.map(_.io.read.resp.metas)))
  s2_invalidPredictions.foreach(_.foreach(_.valid := false.B))
  s2_invalidMetas.foreach(_.foreach(_.rawHit := false.B))

  private val s2_finalPredictions          = Wire(Vec(NumAlignBanks, Vec(NumWay, Valid(new Prediction))))
  private val s2_finalMetas                = Wire(Vec(NumAlignBanks, Vec(NumWay, new MainBtbMetaEntry)))
  private val s2_finalLostBankMask         = Wire(Vec(NumAlignBanks, Bool()))
  private val s2_finalVictimOverride       = Wire(Vec(NumAlignBanks, Bool()))
  private val s2_finalVictimOverrideBankOH = Wire(Vec(NumAlignBanks, Vec(NumAlignBanks, Bool())))

  // Select final S2 outputs using the registered S1 VBTB override decision. S2 has both
  // the original MainBtb response and the VBTB response, but it must not recompute placement:
  // S1 already exposed the chosen positions to TAGE and the BPU top. Reusing the registered
  // override matrix keeps positions, predictions, metas, and lost-bank bookkeeping aligned.
  //
  // - overridden banks use the corresponding VBTB prediction/meta.
  // - invalid banks are cleared because their VBTB hit had no later output slot.
  // - all other banks keep the original MainBtb result.
  Seq.tabulate(NumAlignBanks) { i =>
    val logicIdx = s2_posHigherBitsVec(i)

    val victimPredictions = Mux1H(s2_logicVictimOverrideHitMatrix(i), s2_logicBankVictimPredictions)
    val invPredictions    = s2_invalidPredictions(i)
    val predictions       = alignBanks(i).io.read.resp.predictions

    val victimMetas = Mux1H(s2_logicVictimOverrideHitMatrix(i), s2_logicBankVictimMetas)
    val invMetas    = s2_invalidMetas(i)
    val metas       = alignBanks(i).io.read.resp.metas

    val overridden = s2_logicOverriddenBankMask(logicIdx)
    val invalidate = s2_logicInvalidBankMask(logicIdx)
    // Keep the lost-bank mask in physical AlignBank order for meta/training. Training later
    // masks writes for these banks, avoiding updates based on an incomplete prediction range.
    s2_finalLostBankMask(i) := s2_logicLostBankMask(logicIdx)
    s2_finalPredictions(i) := PriorityMux(Seq(
      overridden -> victimPredictions,
      invalidate -> invPredictions,
      true.B     -> predictions
    ))
    s2_finalMetas(i) := PriorityMux(Seq(
      overridden -> victimMetas,
      invalidate -> invMetas,
      true.B     -> metas
    ))

    val logicOverrideIdx = s2_logicVictimOverrideIdx(logicIdx)
    val isOverride       = s2_logicVictimOverrideMask(logicIdx)
    // If this logical bank supplied a VBTB prediction, record the physical output bank that
    // received it. S3 uses this one-hot mapping to touch the VBTB replacer with the taken
    // bit of the output slot that actually carried the victim prediction.
    val overrideOH = VecInit.tabulate(NumAlignBanks)(j => s2_posHigherBitsVec(j) === logicOverrideIdx)
    s2_finalVictimOverride(i)       := isOverride
    s2_finalVictimOverrideBankOH(i) := overrideOH
  }

  io.result := s2_finalPredictions.flatten
  // we don't need to flatten meta entries, keep the alignBank structure, anyway we just use them per alignBank
  io.meta.entries      := s2_finalMetas
  io.meta.lostBankMask := s2_finalLostBankMask

  /* *** s3 ***
   * touch replacer using final takenMask (mbtb + tage + sc)
   */
  s3_fire := io.enable && io.stageCtrl.s3_fire
  // Build separate replacement-touch masks for MainBtb and VBTB. The final taken mask comes
  // from the BPU top after MainBtb, TAGE, and SC selection, so it is indexed by final output
  // banks rather than by the original source of each prediction.
  //
  // MainBtb replacers are touched only for banks that still own their final output slot.
  // Lost banks are skipped because their original MainBtb predictions were either replaced
  // by VBTB predictions or invalidated due to lack of later output slots.
  //
  // VBTB replacers are touched only when a logical bank's VBTB hit actually overrode another
  // output bank. In that case the touch mask is taken from the overridden output bank, not
  // from the source logical bank, matching the prediction slot whose taken bit survived the
  // final BPU arbitration.
  private val s3_finalLostBankMask         = RegEnable(s2_finalLostBankMask, s2_fire)
  private val s3_finalVictimOverride       = RegEnable(s2_finalVictimOverride, s2_fire)
  private val s3_finalVictimOverrideBankOH = RegEnable(s2_finalVictimOverrideBankOH, s2_fire)
  private val s3_takenMaskVec = VecInit.tabulate(NumAlignBanks) { i =>
    VecInit(io.s3_takenMask.slice(i * NumWay, (i + 1) * NumWay))
  }
  // io.result is flattened, so is s3_takenMask from Bpu top, here we need to slice it back to alignBank structure
  alignBanks.zipWithIndex.foreach { case (b, i) =>
    b.io.s3_takenMask := Mux(
      !s3_finalLostBankMask(i),
      s3_takenMaskVec(i),
      VecInit.fill(NumWay)(false.B)
    )

    b.io.s3_victimTakenMask := Mux(
      s3_finalVictimOverride(i),
      Mux1H(s3_finalVictimOverrideBankOH(i), s3_takenMaskVec),
      VecInit.fill(NumWay)(false.B)
    )
  }

  /* *** t0 ***
   * receive training data
   * send startPc to alignBank for replacer state reading
   */
  t0_fire := io.stageCtrl.t0_fire && io.enable
  private val t0_train = io.train

  private val t0_startPc          = t0_train.startPc
  private val t0_rotator          = VecRotate(getAlignBankIndex(t0_startPc))
  private val t0_startPcVec       = t0_rotator.rotate(t0_train.startPcVec.get)
  private val t0_posHigherBitsVec = t0_rotator.rotate(VecInit.tabulate(NumAlignBanks)(_.U(AlignBankIdxLen.W)))

  alignBanks.zipWithIndex.foreach { case (b, i) =>
    b.io.t0_startPc := t0_startPcVec(i)
  }

  /* *** t1 ***
   * calculate write data and write to alignBanks
   */
  private val t1_fire  = RegNext(t0_fire, init = false.B) && io.enable
  private val t1_train = RegEnable(t0_train, t0_fire)

  private val t1_rotator          = RegEnable(t0_rotator, t0_fire)
  private val t1_startPcVec       = RegEnable(t0_startPcVec, t0_fire)
  private val t1_posHigherBitsVec = RegEnable(t0_posHigherBitsVec, t0_fire)

  private val t1_meta           = t1_train.meta.mbtb
  private val t1_mispredictInfo = t1_train.mispredictBranch

  private val t1_writeAlignBankIdx  = getAlignBankIndexFromPosition(t1_mispredictInfo.bits.cfiPosition)
  private val t1_writeAlignBankMask = t1_rotator.rotate(VecInit(UIntToOH(t1_writeAlignBankIdx).asBools))

  alignBanks.zipWithIndex.foreach { case (b, i) =>
    b.io.write.req.valid              := t1_fire && !t1_meta.lostBankMask(i)
    b.io.write.req.bits.needWrite     := t1_writeAlignBankMask(i)
    b.io.write.req.bits.startPc       := t1_startPcVec(i)
    b.io.write.req.bits.branches      := t1_train.branches
    b.io.write.req.bits.meta          := t1_meta.entries(i)
    b.io.write.req.bits.posHigherBits := t1_posHigherBitsVec(i)
    // see comments in MainBtbAlignBank.scala
    b.io.write.req.bits.mispredictInfo := t1_mispredictInfo
  }
  /* --------------------------------------------------------------------------------------------------------------
     MainBTB Trace
     -------------------------------------------------------------------------------------------------------------- */
  private val alignBankTraceVec = alignBanks.map(_.io.trace)
  private val finalTrace        = Mux1H(t1_writeAlignBankMask, alignBankTraceVec)
  private val finalTraceStartPc = Mux1H(t1_writeAlignBankMask, t1_startPcVec)
  private val mbtbTrace         = Wire(new MainBtbTrace)

  mbtbTrace.startPc      := finalTraceStartPc
  mbtbTrace.setIdx       := finalTrace.setIdx
  mbtbTrace.internalIdx  := finalTrace.bankIdx
  mbtbTrace.alignBankIdx := PriorityEncoder(t1_writeAlignBankMask)
  mbtbTrace.wayIdx       := finalTrace.wayIdx
  mbtbTrace.attribute    := finalTrace.entry.attribute
  mbtbTrace.cfiPosition  := finalTrace.entry.position

  private val mbtbTraceDBTable = ChiselDB.createTable("MBTBTrace", new MainBtbTrace(), EnableMainbtbTrace)
  mbtbTraceDBTable.log(
    data = mbtbTrace,
    en = t1_fire && finalTrace.needWrite,
    clock = clock,
    reset = reset
  )

  /* *** statistics *** */
  private val perf_s2HitMask = VecInit(alignBanks.flatMap(_.io.read.resp.predictions.map(_.valid)))
  private val perf_s2VbtbHitMask = VecInit(s2_finalPredictions.zipWithIndex.flatMap { case (predictions, i) =>
    val useVbtb = s2_logicOverriddenBankMask(s2_posHigherBitsVec(i))
    predictions.map(pred => useVbtb && pred.valid)
  })
  private val perf_s2VbtbTakenMask = VecInit(perf_s2VbtbHitMask zip io.result map { case (hit, pred) =>
    hit && pred.bits.taken
  })
  private val perf_s2VbtbNotTakenMask = VecInit(perf_s2VbtbHitMask zip io.result map { case (hit, pred) =>
    hit && !pred.bits.taken
  })
  private val perf_t1HitMispredictBranch = t1_meta.entries.flatten.map(_.hit(t1_mispredictInfo.bits)).reduce(_ || _)

  XSPerfAccumulate("pred_use_vbtb", s1_fire && s1_logicLostCount =/= 0.U)
  XSPerfAccumulate("vbtb_hit_counter_taken", Mux(s2_fire, PopCount(perf_s2VbtbTakenMask), 0.U))
  XSPerfAccumulate("vbtb_hit_counter_not_taken", Mux(s2_fire, PopCount(perf_s2VbtbNotTakenMask), 0.U))
  XSPerfAccumulate("pred_hit", s2_fire && perf_s2HitMask.reduce(_ || _))
  XSPerfAccumulate("pred_miss", s2_fire && perf_s2HitMask.reduce(!_ && !_))
  XSPerfHistogram("pred_hit_count", PopCount(perf_s2HitMask), s2_fire, 0, NumWay * NumAlignBanks + 1)

  XSPerfAccumulate("total_train", t1_fire)
  XSPerfAccumulate("train_has_mispredict", t1_fire && t1_mispredictInfo.valid)
  XSPerfAccumulate("train_hit_mispredict", t1_fire && t1_mispredictInfo.valid && perf_t1HitMispredictBranch)
}
