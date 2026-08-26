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

package xiangshan.frontend.bpu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.ChiselDB
import utility.Constantin
import utility.DelayN
import utility.XSError
import utility.XSPerfAccumulate
import utility.XSPerfHistogram
import utility.XSPerfSeqAccumulate
import utils.DuplicateInit
import xiangshan.frontend.BpuToFtqIO
import xiangshan.frontend.FrontendTopDownBundle
import xiangshan.frontend.FtqToBpuIO
import xiangshan.frontend.GuardedPcInit
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.history.commonhr.CommonHR
import xiangshan.frontend.bpu.history.commonhr.CommonHRMeta
import xiangshan.frontend.bpu.history.fastphr.FastPhr
import xiangshan.frontend.bpu.history.fastphr.HasFastPhrParameters
import xiangshan.frontend.bpu.history.phr.Phr
import xiangshan.frontend.bpu.history.phr.PhrAllFoldedHistories
import xiangshan.frontend.bpu.ittage.Ittage
import xiangshan.frontend.bpu.mbtb.MainBtb
import xiangshan.frontend.bpu.ptage.Ptage
import xiangshan.frontend.bpu.ras.MicroRas
import xiangshan.frontend.bpu.ras.Ras
import xiangshan.frontend.bpu.sc.Sc
import xiangshan.frontend.bpu.tage.Tage
import xiangshan.frontend.bpu.ubtb.MicroBtb

class Bpu(implicit p: Parameters) extends BpuModule with HalfAlignHelper with HasFastPhrParameters {
  class BpuIO extends Bundle {
    val ctrl:        BpuCtrl    = Input(new BpuCtrl)
    val resetVector: PrunedAddr = Input(PrunedAddr(PAddrBits))
    val fromFtq:     FtqToBpuIO = Flipped(new FtqToBpuIO)
    val toFtq:       BpuToFtqIO = new BpuToFtqIO
  }

  val io: BpuIO = IO(new BpuIO)

  /* *** submodules *** */
  private val fallThrough = Module(new FallThroughPredictor)
  private val ubtb        = Module(new MicroBtb)
  private val mbtb        = Module(new MainBtb)
  private val tage        = Module(new Tage)
  private val ittage      = Module(new Ittage)
  private val sc          = Module(new Sc)
  private val ras         = Module(new Ras)
  private val phr         = Module(new Phr)
  private val fastPhr     = Module(new FastPhr)
  private val commonHR    = Module(new CommonHR)
  private val uras        = Module(new MicroRas)
  private val ptage       = Module(new Ptage)

  private def predictors: Seq[BasePredictor] = Seq(
    fallThrough,
    ubtb,
    ptage,
    uras,
    mbtb,
    tage,
    sc,
    ittage,
    ras
  )

  /* *** aliases *** */
  private val commit   = io.fromFtq.commit
  private val redirect = io.fromFtq.redirect

  /* *** CSR ctrl sub-predictor enable *** */
  private val ctrl      = DelayN(io.ctrl, 2) // delay 2 cycle for timing
  private val constCtrl = Constantin.createRecord("constCtrl")

  fallThrough.io.enable := true.B // fallThrough is always enabled
  uras.io.enable        := true.B
  ptage.io.enable       := true.B
  if (env.EnableConstantin && !env.FPGAPlatform) {
    ubtb.io.enable   := Mux(constCtrl(0), constCtrl(1), ctrl.ubtbEnable)
    mbtb.io.enable   := Mux(constCtrl(0), constCtrl(3), ctrl.mbtbEnable)
    tage.io.enable   := Mux(constCtrl(0), constCtrl(4), ctrl.tageEnable)
    sc.io.enable     := Mux(constCtrl(0), constCtrl(5), ctrl.scEnable)
    ittage.io.enable := Mux(constCtrl(0), constCtrl(6), ctrl.ittageEnable)
    ras.io.enable    := Mux(constCtrl(0), constCtrl(7), ctrl.rasEnable)
  } else {
    ubtb.io.enable   := ctrl.ubtbEnable
    mbtb.io.enable   := ctrl.mbtbEnable
    tage.io.enable   := ctrl.tageEnable
    sc.io.enable     := ctrl.scEnable
    ittage.io.enable := ctrl.ittageEnable
    ras.io.enable    := ctrl.rasEnable
  }
  // For some reason s0 stalled, usually FTQ Full
  private val s0_stall = Wire(Bool())

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

  private val s3_override = WireDefault(false.B)

  private val s1_prediction = Wire(new Prediction)
  private val s3_prediction = Wire(new Prediction)

  private val debug_bpId = RegInit(0.U(XLEN.W))

  private val s0_startPc    = DuplicateInit(NumStartPcDuplicate, GuardedPcInit(0.U(GuardedVAddrBits.W)))
  private val s0_startPcReg = RegEnable(s0_startPc, !s0_stall)

  when(RegNext(RegNext(reset.asBool)) && !reset.asBool) {
    s0_startPcReg.foreach(_ := io.resetVector.zeroExt(GuardedVAddrBits))
  }

  private val s1_startPc = RegEnable(s0_startPc, s0_fire)
  private val s2_startPc = RegEnable(s1_startPc, s1_fire)
  private val s3_startPc = RegEnable(s2_startPc, s2_fire)

  // pTAGE's lookup information rides the pipeline down to s3, where the verified group trains the entry it came from
  private val s2_ptageMeta = RegEnable(ptage.io.meta, s1_fire)
  private val s3_ptageMeta = RegEnable(s2_ptageMeta, s2_fire)

  // the second block, carried to s3 so that it can be checked there. Where it starts needs no pipeline of its own:
  // it is the first block's target, which already travels down as the s1 prediction.
  private val s1_secondBlockIn = Wire(Valid(new Prediction))
  private val s2_secondBlock   = RegEnable(s1_secondBlockIn, s1_fire)
  private val s3_secondBlock   = RegEnable(s2_secondBlock, s2_fire)

  /* *** common inputs *** */
  private val stageCtrl = Wire(new StageCtrl)
  stageCtrl.s0_fire := s0_fire
  stageCtrl.s1_fire := s1_fire
  stageCtrl.s2_fire := s2_fire
  stageCtrl.s3_fire := s3_fire
  stageCtrl.t0_fire := io.fromFtq.train.fire

  private val t0_compareMatrix = CompareMatrix(VecInit(io.fromFtq.train.bits.branches.map(_.bits.cfiPosition)))
  // mark all branches after the first mispredict as invalid
  // i.e. we have (valid, position, mispredict) for each branch:
  // (1, 2, 0), (1, 5, 1), (1, 8, 0)
  // then the first mispredict branch is @5, so mask should be (1, 1, 0)
  private val t0_firstMispredictMask = t0_compareMatrix.getLowerElementMask(
    VecInit(io.fromFtq.train.bits.branches.map(b => b.valid && b.bits.mispredict))
  )

  private val train = Wire(new BpuTrain)
  train := io.fromFtq.train.bits
  train.branches.zipWithIndex.foreach { case (b, i) =>
    b.valid := io.fromFtq.train.bits.branches(i).valid && t0_firstMispredictMask(i)
  }

  private val fastTrain = Wire(Valid(new FastTrain))
  fastTrain.valid                := s3_valid
  fastTrain.bits.startPc         := s3_startPc.get.unGuard
  fastTrain.bits.finalPrediction := s3_prediction
  fastTrain.bits.ptageMeta       := s3_ptageMeta
  fastTrain.bits.hasOverride     := s3_override

  predictors.foreach { p =>
    p.io.startPc   := s0_startPc.get
    p.io.stageCtrl := stageCtrl
    // in this fromBpuTrain, we get a duplicated startPcVec, so this cannot be moved outside "predictors.foreach"
    // i.e. this is wrong: ```
    //   private val train = Wire(new Train)
    //   train.fromBpuTrain(io.fromFtq.train.bits)
    //   predictors.foreach { p => p.io.train := train }
    // ```
    p.io.train.fromBpuTrain(train)
    // fastTrain is an Option[Valid[BpuFastTrain]], we need .foreach
    p.io.fastTrain.foreach(_ := fastTrain)
  }
  io.fromFtq.train.ready := predictors.map(_.io.trainReady).reduce(_ && _)

  /* *** predictor specific inputs *** */
  // pTAGE reads its resident folded histories straight out of FastPhr; its own a0 stage is driven by the shared
  // startPc, which for an ahead-indexed predictor is the key of a group two cycles out rather than this one's.
  // Nothing selects its prediction yet, so it only observes and reports how well it would have done.
  ptage.io.foldedHist    := fastPhr.io.foldedHist
  ptage.io.redirectValid := redirect.valid
  ptage.io.overrideValid := s3_override

  // uras
  uras.io.specIn.startPc                := s1_startPc.get
  uras.io.specIn.cfiPosition            := s1_prediction.cfiPosition
  uras.io.specIn.attribute              := s1_prediction.attribute
  uras.io.hasRedirect                   := redirect.valid
  uras.io.overrideData.valid            := s3_override
  uras.io.overrideData.bits.startPc     := s3_startPc.get.toUInt
  uras.io.overrideData.bits.attribute   := s3_prediction.attribute
  uras.io.overrideData.bits.cfiPosition := s3_prediction.cfiPosition
  uras.io.fullRetAddr                   := ras.io.topRetAddr

  ras.io.redirect                := redirect
  ras.io.commit                  := commit
  ras.io.specIn.valid            := s3_fire
  ras.io.specIn.bits.startPc     := s3_startPc.get.toUInt
  ras.io.specIn.bits.attribute   := s3_prediction.attribute
  ras.io.specIn.bits.cfiPosition := s3_prediction.cfiPosition

  tage.io.fromMainBtb.result             := mbtb.io.result
  tage.io.fromMainBtb.s1_positions       := mbtb.io.s1_positions
  tage.io.fromMainBtb.baseConf           := VecInit(mbtb.io.meta.entries.flatten.map(_.counter.isSaturate))
  tage.io.fromPhr.foldedPathHist         := phr.io.s0_foldedPhr
  tage.io.fromPhr.foldedPathHistForTrain := phr.io.trainFoldedPhr
  tage.io.debug_trainValid               := io.fromFtq.train.valid // for perf counters

  ittage.io.s1_foldedPhr   := phr.io.s1_foldedPhr
  ittage.io.trainFoldedPhr := phr.io.trainFoldedPhr

  sc.io.mbtbResult          := mbtb.io.result
  sc.io.providerTakenCtrs   := tage.io.toSc.providerTakenCtrVec
  sc.io.foldedPathHist      := phr.io.s0_foldedPhr
  sc.io.imli                := commonHR.io.s0_imli
  sc.io.trainFoldedPathHist := phr.io.trainFoldedPhr
  sc.io.commonHR            := commonHR.io.s0_commonHR

  s3_flush := redirect.valid
  s2_flush := s3_flush || s3_override
  s1_flush := s2_flush

  s1_ready := s1_fire || !s1_valid || s1_flush
  s2_ready := s2_fire || !s2_valid
  s3_ready := s3_fire || !s3_valid

  private val sramResetDone = RegInit(false.B)
  when(predictors.map(_.io.sramResetDone).reduce(_ && _)) {
    sramResetDone := true.B
  }
  s0_fire := s1_ready && sramResetDone
  s1_fire := s1_valid && s2_ready && io.toFtq.prediction.ready
  s2_fire := s2_valid && s3_ready
  s3_fire := s3_valid

  when(s0_fire)(s1_valid := true.B)
    .elsewhen(s1_flush)(s1_valid := false.B)
    .elsewhen(s1_fire)(s1_valid := false.B)

  when(s2_flush)(s2_valid := false.B)
    .elsewhen(s1_fire)(s2_valid := !s1_flush)
    .elsewhen(s2_fire)(s2_valid := false.B)

  when(s3_flush)(s3_valid := false.B)
    .elsewhen(s2_fire)(s3_valid := !s2_flush)
    .elsewhen(s3_fire)(s3_valid := false.B)

  // s0_stall should be exclusive with any other PC source
  s0_stall := !(s1_valid || s3_override || redirect.valid)

  private val s1_ubtbPrediction = Wire(new Prediction)
  s1_ubtbPrediction := ubtb.io.prediction.bits
  s1_ubtbPrediction.target := Mux(
    ubtb.io.prediction.bits.attribute.isReturn && uras.io.specOut.isCanUse,
    uras.io.specOut.retTarget,
    ubtb.io.prediction.bits.target
  )
  // pTAGE answers with a whole group where it has one, so it leads; anything it does not know falls to the small btb
  // and then to running the block out. A return takes its target from the return stack whichever source found it.
  private val s1_ptageBlock  = ptage.io.prediction.blocks.head
  private val s1_ptageResult = Wire(new Prediction)
  s1_ptageResult.taken       := s1_ptageBlock.bits.taken
  s1_ptageResult.cfiPosition := s1_ptageBlock.bits.cfiPosition
  s1_ptageResult.attribute   := s1_ptageBlock.bits.attribute
  s1_ptageResult.target := Mux(
    s1_ptageBlock.bits.attribute.isReturn && uras.io.specOut.isCanUse,
    uras.io.specOut.retTarget,
    s1_ptageBlock.bits.target
  )

  s1_prediction := MuxCase(
    fallThrough.io.prediction,
    Seq(
      (s1_ptageBlock.valid && s1_ptageResult.taken) -> s1_ptageResult,
      s1_ptageBlock.valid                           -> fallThrough.io.prediction,
      s1_ubtbPrediction.taken                       -> s1_ubtbPrediction
    )
  )

  // The group s1 hands on. Everything that has to account for a whole group, the path history included, reads it from
  // here.
  //
  // A second block only exists when pTAGE supplied the first and that first block jumped somewhere the entry itself
  // named. pTAGE marks a block that cannot say where its successor starts, a return in particular, as unable to carry
  // one; and when the group came from a fallback instead, there is no second block to speak of.
  private val usePtage       = s1_ptageBlock.valid && s1_ptageResult.taken
  private val s1_secondBlock = ptage.io.prediction.blocks(1)
  // A block that moves the return stack cannot carry a successor. Every entry of a group recovers from the one
  // speculation state Ftq records for it, so a second block behind a call would read a return stack top its own
  // predecessor has already pushed past, and a return inside that block would be redirected to a stale address.
  private val s1_firstMovesRas = s1_prediction.attribute.hasPush || s1_prediction.attribute.hasPop
  private val s1_emitSecond    = usePtage && s1_secondBlock.valid && !s1_firstMovesRas

  private val s1_group = Wire(Vec(MaxPredictionNum, Valid(new Prediction)))
  s1_group(0).valid            := true.B
  s1_group(0).bits             := s1_prediction
  s1_group(1).valid            := s1_emitSecond
  s1_group(1).bits.taken       := s1_secondBlock.bits.taken
  s1_group(1).bits.cfiPosition := s1_secondBlock.bits.cfiPosition
  s1_group(1).bits.attribute   := s1_secondBlock.bits.attribute
  s1_group(1).bits.target      := s1_secondBlock.bits.target
  s1_secondBlockIn             := s1_group(1)

  private val s1_taken         = s1_prediction.taken
  private val debug_s1UsePtage = s1_taken && usePtage
  private val debug_s1UseUbtb  = s1_taken && !usePtage

  private val s2_s1Prediction = RegEnable(s1_prediction, s1_fire)

  private val s2_compareMatrix = CompareMatrix(VecInit(mbtb.io.result.map(_.bits.cfiPosition)))
  private val s2_jumpTakenVec = VecInit(mbtb.io.result.map {
    entry => entry.valid && (entry.bits.attribute.isDirect || entry.bits.attribute.isIndirect)
  })
  private val s2_isBrVec = VecInit(mbtb.io.result.map {
    entry => entry.valid && entry.bits.attribute.isConditional
  })

  /* *** s3 prediction selection *** */
  private val s3_mbtbResult     = RegEnable(mbtb.io.result, s2_fire)
  private val s3_tagePrediction = RegEnable(tage.io.prediction, s2_fire)
  private val s3_scUsed         = RegEnable(sc.io.scUsed, s2_fire)
  private val s3_scTakenMask    = RegEnable(sc.io.scTakenMask, s2_fire)
  private val s3_compareMatrix  = RegEnable(s2_compareMatrix, s2_fire)
  private val s3_s1Prediction   = RegEnable(s2_s1Prediction, s2_fire)
  private val s3_jumpTakenVec   = RegEnable(s2_jumpTakenVec, s2_fire)
  private val s3_isBrVec        = RegEnable(s2_isBrVec, s2_fire)

  // timing optimization: The comparison of predictions and the generation of the s3_taken are performed in parallel.
  private val s3_mbtbCfiPositionDiffVec = VecInit(s3_mbtbResult.map(_.bits.cfiPosition =/= s3_s1Prediction.cfiPosition))
  private val s3_mbtbAttributeDiffVec   = VecInit(s3_mbtbResult.map(_.bits.attribute =/= s3_s1Prediction.attribute))
  private val s3_mbtbTargetDiffVec      = VecInit(s3_mbtbResult.map(_.bits.target =/= s3_s1Prediction.target))
  private val s3_ittageTargetDiff       = ittage.io.prediction.target =/= s3_s1Prediction.target
  private val s3_rasTargetDiff          = ras.io.topRetAddr =/= s3_s1Prediction.target

  private val s3_takenMask = VecInit(s3_mbtbResult.zipWithIndex.map { case (entry, i) =>
    val useTage   = s3_tagePrediction.takenVec(i).valid
    val tageTaken = s3_tagePrediction.takenVec(i).bits
    val useSc     = s3_scUsed(i)
    val scTaken   = s3_scTakenMask(i)

    s3_jumpTakenVec(i) ||
    (s3_isBrVec(i) &&
      MuxCase(
        entry.bits.taken, // default: base table
        Seq(
          useSc   -> scTaken,
          useTage -> tageTaken
        )
      ))
  })
  private val s3_taken = s3_takenMask.reduce(_ || _)

  private val s3_firstTakenBranchOH = s3_compareMatrix.getLeastElementOH(s3_takenMask)
  private val s3_firstTakenBranch   = Mux1H(s3_firstTakenBranchOH, s3_mbtbResult)
  private val s3_useRas             = s3_firstTakenBranch.bits.attribute.isReturn
  private val s3_useIttage          = s3_firstTakenBranch.bits.attribute.needIttage && ittage.io.prediction.hit

  private val s2_fallThroughPrediction = RegEnable(fallThrough.io.prediction, s1_fire)
  private val s3_fallThroughPrediction = RegEnable(s2_fallThroughPrediction, s2_fire)

  // used for mainBTB replacer
  mbtb.io.s3_takenMask := s3_takenMask

  // used for ghr
  private val s3_condHitMask = VecInit(s3_mbtbResult.map(e => e.valid && e.bits.attribute.isConditional))

  s3_prediction       := Mux(s3_taken, s3_firstTakenBranch.bits, s3_fallThroughPrediction)
  s3_prediction.taken := s3_taken
  s3_prediction.target :=
    MuxCase(
      s3_fallThroughPrediction.target,
      Seq(
        (s3_taken && s3_useRas)    -> ras.io.topRetAddr,
        (s3_taken && s3_useIttage) -> ittage.io.prediction.target,
        s3_taken                   -> s3_firstTakenBranch.bits.target
      )
    )

  /* *** second block verification ***
   * The s3 predictors look up only where the group starts, so they say nothing about a second block. The micro btb
   * does: it is written from s3's own verified predictions, so an entry is an account of what happened the last time
   * control passed through, arrived at independently of whatever pTAGE stored.
   *
   * A block it has no entry for cannot be checked, and an unchecked block is not worth keeping. Dropping one costs a
   * block of width now and gets it back later: the next lookup meets that block as a first block, verifies it the
   * ordinary way, and fills the micro btb in passing, so the same pair can be checked next time round.
   */
  ubtb.io.verifyStartPc := s3_s1Prediction.target

  private val s3_secondBlockExitDiffers =
    ubtb.io.verify.bits.cfiPosition =/= s3_secondBlock.bits.cfiPosition ||
      ubtb.io.verify.bits.attribute =/= s3_secondBlock.bits.attribute ||
      ubtb.io.verify.bits.target =/= s3_secondBlock.bits.target ||
      !s3_secondBlock.bits.taken

  // An entry is positive evidence of where a block leaves; its absence is not evidence that a block runs to the end,
  // only that nothing is on record. Keeping a block on the strength of that would let one through unchecked, so a
  // block the micro btb cannot account for is treated the same as one it contradicts.
  private val s3_secondBlockUnverified =
    s3_secondBlock.valid && (!ubtb.io.verify.valid || s3_secondBlockExitDiffers)

  XSPerfAccumulate("s3SecondBlockChecked", s3_valid && s3_secondBlock.valid)
  XSPerfAccumulate("s3SecondBlockUnknown", s3_valid && s3_secondBlock.valid && !ubtb.io.verify.valid)
  XSPerfAccumulate(
    "s3SecondBlockDisagrees",
    s3_valid && s3_secondBlock.valid && ubtb.io.verify.valid && s3_secondBlockExitDiffers
  )
  // a not-taken second block can never be confirmed, since only a taken exit leaves an entry behind
  XSPerfAccumulate(
    "s3SecondBlockNotTaken",
    s3_valid && s3_secondBlock.valid && !s3_secondBlock.bits.taken
  )

  s3_override := {
    val takenDiff       = s3_taken =/= s3_s1Prediction.taken
    val cfiPositionDiff = Mux1H(s3_firstTakenBranchOH, s3_mbtbCfiPositionDiffVec)
    val attributeDiff   = Mux1H(s3_firstTakenBranchOH, s3_mbtbAttributeDiffVec)
    val targetDiff =
      MuxCase(
        false.B, // fall-through
        Seq(
          (s3_taken && s3_useRas)    -> s3_rasTargetDiff,
          (s3_taken && s3_useIttage) -> s3_ittageTargetDiff,
          s3_taken                   -> Mux1H(s3_firstTakenBranchOH, s3_mbtbTargetDiffVec)
        )
      )

    s3_valid && (takenDiff || cfiPositionDiff || attributeDiff || targetDiff || s3_secondBlockUnverified)
  }

  // Assigned here rather than with the rest of fastTrain because it depends on the override decision: a second block
  // that failed verification is dropped along with the group, so the entry that proposed it has not been vindicated.
  fastTrain.bits.hasSecondBlock := s3_secondBlock.valid && !s3_override

  private val s2_phrMeta = RegEnable(phr.io.phrMeta, s1_fire)
  private val s3_phrMeta = RegEnable(s2_phrMeta, s2_fire)

  private val s3_commonHRMeta = WireInit(0.U.asTypeOf(new CommonHRMeta))
  s3_commonHRMeta.ghr       := commonHR.io.s3ResolveMeta.ghr
  s3_commonHRMeta.bw        := commonHR.io.s3ResolveMeta.bw
  s3_commonHRMeta.imli      := commonHR.io.s3ResolveMeta.imli
  s3_commonHRMeta.hitMask   := commonHR.io.s3DedupHitMask
  s3_commonHRMeta.attribute := VecInit(s3_mbtbResult.map(_.bits.attribute))
  s3_commonHRMeta.position  := VecInit(s3_mbtbResult.map(_.bits.cfiPosition))

  private val s3_redirectMeta = Wire(new BpuRedirectMeta)
  s3_redirectMeta.phr          := s3_phrMeta
  s3_redirectMeta.commonHRMeta := s3_commonHRMeta
  s3_redirectMeta.ras          := ras.io.redirectMeta

  private val s3_resolveMeta = Wire(new BpuResolveMeta)
  s3_resolveMeta.mbtb     := RegEnable(mbtb.io.meta, s2_fire)
  s3_resolveMeta.tage     := RegEnable(tage.io.meta, s2_fire)
  s3_resolveMeta.sc       := sc.io.meta
  s3_resolveMeta.commonHR := commonHR.io.s3ResolveMeta
  s3_resolveMeta.ittage   := ittage.io.meta
  s3_resolveMeta.phr      := s3_phrMeta

  private val s3_commitMeta = Wire(new BpuCommitMeta)
  s3_commitMeta.ras := ras.io.commitMeta

  println(s"bpu redirect meta width: ${s3_redirectMeta.getWidth}")
  println(s"bpu resolve meta width: ${s3_resolveMeta.getWidth}")
  println(s"bpu commit meta width: ${s3_commitMeta.getWidth}")

  /* *** bpu to ftq io *** */
  io.toFtq.prediction.valid := s1_valid && s2_ready || s3_override

  private val firstBlock = io.toFtq.prediction.bits.blocks.head
  firstBlock.valid := true.B
  when(s3_override) {
    firstBlock.bits.fromStage(s3_startPc.get, s3_prediction)
  }.otherwise {
    firstBlock.bits.fromStage(s1_startPc.get, s1_prediction)
  }
  // The second block starts where the first jumped to, which is what lets Ftq keep reading a block's target off its
  // successor. An s3 override replaces the group with a single corrected block, so none follows it.
  private val secondBlock = io.toFtq.prediction.bits.blocks(1)
  secondBlock.valid := s1_group(1).valid && !s3_override
  secondBlock.bits.fromStage(s1_prediction.target, s1_group(1).bits)
  io.toFtq.prediction.bits.s3Override := s3_override

  // used for meta enqueue and s3 override
  private val s2_ftqPtr = RegEnable(io.fromFtq.bpuPtr, s1_fire)
  private val s3_ftqPtr = RegEnable(s2_ftqPtr, s2_fire)
  io.toFtq.s3FtqPtr := s3_ftqPtr
  // An override replaces the group with a single corrected block, so only a group that survives s3 keeps its width.
  // a group is its first block plus, where there was one, its second; an override leaves only the corrected first
  io.toFtq.s3NumBlocks := Mux(s3_override, 1.U, 1.U +& s3_secondBlock.valid.asUInt)

  io.toFtq.meta.valid             := s3_valid
  io.toFtq.meta.bits.redirectMeta := s3_redirectMeta
  io.toFtq.meta.bits.resolveMeta  := s3_resolveMeta
  io.toFtq.meta.bits.commitMeta   := s3_commitMeta

  /* *** s0_startPc selection *** */
  // A group ends where its last valid block ends, so that is where the next one starts. Feeding back the first
  // block's target instead would restart at a pc this group already covered, and the entry Ftq wrote for the second
  // block would not be followed by one starting at that block's target, which is how Ftq encodes a target at all.
  private val s1_groupTarget = Mux(s1_group(1).valid, s1_group(1).bits.target, s1_prediction.target)

  s0_startPc := MuxCase(
    s0_startPcReg.get,
    Seq(
      redirect.valid -> redirect.bits.target,
      s3_override    -> s3_prediction.target,
      s1_valid       -> s1_groupTarget
    )
  )

  // Ftq reads a block's target off its successor's startPc, so consecutive groups have to abut exactly. A redirect or
  // an override restarts the stream somewhere else, so the check resumes only once a group has been issued since.
  private val debug_lastGroupTarget = RegEnable(s1_groupTarget, s1_fire)
  private val debug_streamAbuts     = RegInit(false.B)
  when(s1_fire)(debug_streamAbuts                       := true.B)
  when(redirect.valid || s3_override)(debug_streamAbuts := false.B)
  XSError(
    s1_fire && debug_streamAbuts && s1_startPc.get =/= debug_lastGroupTarget,
    "a prediction group does not start where the previous group ended\n"
  )

  private val phrBits        = WireInit(0.U(PhrHistoryLength.W))
  private val s0_foldedPhr   = WireInit(0.U.asTypeOf(new PhrAllFoldedHistories(AllFoldedHistoryInfo)))
  private val s1_foldedPhr   = WireInit(0.U.asTypeOf(new PhrAllFoldedHistories(AllFoldedHistoryInfo)))
  private val s2_foldedPhr   = WireInit(0.U.asTypeOf(new PhrAllFoldedHistories(AllFoldedHistoryInfo)))
  private val s3_foldedPhr   = WireInit(0.U.asTypeOf(new PhrAllFoldedHistories(AllFoldedHistoryInfo)))
  private val trainFoldedPhr = WireInit(0.U.asTypeOf(new PhrAllFoldedHistories(AllFoldedHistoryInfo)))

  private val s1_ubtbPredWithURas = WireInit(ubtb.io.prediction)
  when(s1_ubtbPredWithURas.valid && s1_ubtbPredWithURas.bits.attribute.isReturn && uras.io.specOut.isCanUse) {
    s1_ubtbPredWithURas.bits.target := uras.io.specOut.retTarget
  }

  phr.io.train.s0_stall      := s0_stall
  phr.io.train.stageCtrl     := stageCtrl
  phr.io.train.redirect      := redirect
  phr.io.train.s3_override   := s3_override
  phr.io.train.s3_phrMeta    := s3_phrMeta
  phr.io.train.s3_prediction := s3_prediction
  phr.io.train.s3_startPc    := s3_startPc.get.unGuard
  phr.io.s1Train.valid       := s1_fire
  phr.io.s1Train.startPc     := s1_startPc.get.unGuard
  phr.io.s1Train.blocks      := s1_group

  phr.io.commit.valid := io.fromFtq.train.fire
  phr.io.commit.bits.fromBpuTrain(train)

  s0_foldedPhr   := phr.io.s0_foldedPhr
  s1_foldedPhr   := phr.io.s1_foldedPhr
  s2_foldedPhr   := phr.io.s2_foldedPhr
  s3_foldedPhr   := phr.io.s3_foldedPhr
  trainFoldedPhr := phr.io.trainFoldedPhr
  phrBits        := phr.io.phr.asUInt

  dontTouch(phrBits)

  /* *** fast phr ***
   * A short window of the same path history Phr maintains, giving pTAGE resident folded histories. It advances on
   * exactly the events that move Phr, with the same tokens, so the two never diverge. Only taken blocks shift the
   * path history, so a not-taken block advances neither.
   */
  private def numBlocksOH(numTaken: UInt): Vec[Bool] =
    VecInit(Seq.tabulate(MaxPredictionNum + 1)(n => numTaken === n.U))

  fastPhr.io.valid       := s1_fire
  fastPhr.io.token       := phr.io.toFastPhr.s1Token
  fastPhr.io.numBlocksOH := numBlocksOH(phr.io.toFastPhr.s1NumTaken)
  fastPhr.io.s2Fire      := s2_fire

  fastPhr.io.redirect.valid := redirect.valid
  fastPhr.io.redirect.phr   := phr.io.toFastPhr.redirectPhr

  fastPhr.io.overrideValid       := s3_override
  fastPhr.io.overrideToken       := phr.io.toFastPhr.s3PathHash
  fastPhr.io.overrideNumBlocksOH := numBlocksOH(s3_prediction.taken.asUInt)

  // FastPhr caches what Phr already holds, so the two must agree bit for bit. Checking the window directly catches a
  // divergence at its source, rather than waiting for it to surface as a mispredict through pTAGE's folded histories.
  private val fastPhrDiverged = phr.io.toFastPhr.debug_phr(WindowLength - 1, 0) =/= fastPhr.io.debug_phr
  XSPerfAccumulate("fastPhrDivergedCycles", fastPhrDiverged)
  XSError(fastPhrDiverged, "FastPhr window diverged from Phr\n")

  // ghr update
  private val s1_cfiPc = getCfiPcFromPosition(s1_startPc.get, s1_prediction.cfiPosition)
  private val s1_imliTaken =
    s1_prediction.taken && s1_prediction.attribute.isConditional &&
      (s1_cfiPc.addr(CompareAddrLowWidth - 1, 0) > s1_prediction.target.addr(CompareAddrLowWidth - 1, 0))

  commonHR.io.stageCtrl                 := stageCtrl
  commonHR.io.s0_startPc.get            := s0_startPc.get.unGuard
  commonHR.io.s1_imliTaken              := s1_imliTaken
  commonHR.io.s2StartPc                 := s2_startPc.get.unGuard
  commonHR.io.s2CondHitMask             := VecInit(mbtb.io.result.map(e => e.valid && e.bits.attribute.isConditional))
  commonHR.io.s2CfiPositions            := VecInit(mbtb.io.result.map(_.bits.cfiPosition))
  commonHR.io.s2CfiTargets              := VecInit(mbtb.io.result.map(_.bits.target.unGuard))
  commonHR.io.update.startPc            := s3_startPc.get.unGuard
  commonHR.io.update.target             := s3_prediction.target.unGuard
  commonHR.io.update.taken              := s3_taken
  commonHR.io.update.s3Override         := s3_override
  commonHR.io.update.attributes         := VecInit(s3_mbtbResult.map(_.bits.attribute))
  commonHR.io.update.targets            := VecInit(s3_mbtbResult.map(_.bits.target.unGuard))
  commonHR.io.update.firstTakenBranchOH := s3_firstTakenBranchOH
  commonHR.io.update.firstTakenBranch   := s3_firstTakenBranch
  commonHR.io.update.position           := VecInit(s3_mbtbResult.map(_.bits.cfiPosition))
  commonHR.io.update.condHitMask        := s3_condHitMask
  commonHR.io.redirect.valid            := redirect.valid
  commonHR.io.redirect.cfiPc            := redirect.bits.cfiPc
  commonHR.io.redirect.target           := redirect.bits.target.unGuard
  commonHR.io.redirect.taken            := redirect.bits.taken
  commonHR.io.redirect.attribute        := redirect.bits.attribute
  commonHR.io.redirect.meta             := redirect.bits.meta.commonHRMeta

  // Power-on reset
  private val powerOnResetState = RegInit(true.B)
  when(s0_fire) {
    // When BPU pipeline first time fire, we consider power-on reset is done
    powerOnResetState := false.B
  }
  XSError(
    !powerOnResetState && s0_stall && s0_startPc.head =/= s0_startPcReg.head,
    "s0_stall but s0_startPc is different from s0_startPcReg"
  )

  /* *** Debug Meta *** */
  // used for performance counters
  private val s3_firstTakenBlameSc = Mux1H(s3_firstTakenBranchOH, s3_scUsed)
  // see class BpuPredictionSource in bpu/Bundles.scala
  private val s1_predictionSource =
    MuxCase(
      BpuPredictionSource.Stage1.Fallthrough,
      Seq(
        debug_s1UsePtage -> BpuPredictionSource.Stage1.Ptage,
        debug_s1UseUbtb  -> BpuPredictionSource.Stage1.Ubtb
      )
    )
  private val s3_predictionSource = PriorityEncoder(Seq(
    s3_taken && s3_useRas,                                                                // RAS
    s3_taken && s3_useIttage,                                                             // ITTage
    s3_taken && s3_firstTakenBranch.bits.attribute.isConditional && s3_firstTakenBlameSc, // Sc
    s3_taken && s3_firstTakenBranch.bits.attribute.isConditional,                         // Tage
    s3_taken,                                                                             // Mbtb
    true.B                                                                                // Fallthrough
  ))

  private val s2_s1PredictionSource = RegEnable(s1_predictionSource, s1_fire)
  private val s3_s1PredictionSource = RegEnable(s2_s1PredictionSource, s2_fire)

  private val s3_perfMeta = Wire(new BpuPerfMeta)
  s3_perfMeta.startPc             := s3_startPc.head.unGuard
  s3_perfMeta.bpId                := debug_bpId
  s3_perfMeta.s1Prediction        := s3_s1Prediction
  s3_perfMeta.s3Prediction        := s3_prediction
  s3_perfMeta.bpSource.s1Source   := s3_s1PredictionSource
  s3_perfMeta.bpSource.s3Source   := s3_predictionSource
  s3_perfMeta.bpSource.s3Override := s3_override
  s3_perfMeta.mbtbMeta            := RegEnable(mbtb.io.meta, s2_fire)
  s3_perfMeta.scUsed              := s3_scUsed.asUInt

  io.toFtq.perfMeta := s3_perfMeta

  // Bpu reports no reason of its own, and does not need to: nothing inside it can stall the prediction path, which
  // fires unless Ftq back-pressures it or a flush empties it, and both of those are named where they happen. The one
  // cycle an override costs here is exactly the cycle Ftq's own stage sees, so Ftq names it.
  io.toFtq.topdownReasons := 0.U.asTypeOf(new FrontendTopDownBundle())

  /* *** BpTrace *** */
  when(io.toFtq.meta.fire) {
    debug_bpId := debug_bpId + 1.U
  }

  private class PredictionTrace extends Bundle {
    val meta     = new BpuMeta
    val perfMeta = new BpuPerfMeta
  }

  private class TrainTrace extends Bundle {
    val train = new BpuTrain
  }

  private val predictionTable = ChiselDB.createTable("BpuPredictionTrace", new PredictionTrace, EnableBpTrace)
  private val trainTable      = ChiselDB.createTable("BpuTrainTrace", new TrainTrace, EnableBpTrace)

  private val predictionTrace = Wire(new PredictionTrace)
  predictionTrace.meta     := io.toFtq.meta.bits
  predictionTrace.perfMeta := s3_perfMeta

  private val trainTrace = Wire(new TrainTrace)
  trainTrace.train := train

  predictionTable.log(
    data = predictionTrace,
    en = io.toFtq.meta.fire,
    clock = clock,
    reset = reset
  )

  trainTable.log(
    data = trainTrace,
    en = io.fromFtq.train.fire,
    clock = clock,
    reset = reset
  )

  /* *** perf pred *** */

  XSPerfAccumulate("toFtqFire", io.toFtq.prediction.fire)
  XSPerfAccumulate("s3Override", io.toFtq.prediction.fire && io.toFtq.prediction.bits.s3Override)
  XSPerfHistogram(
    "fetchBlockSize",
    Mux(
      firstBlock.bits.taken,
      getFtqOffset(firstBlock.bits.startPc, firstBlock.bits.endPosition),
      FetchBlockInstNum.U
    ),
    io.toFtq.prediction.fire,
    0,
    FetchBlockInstNum + 1
  )
  XSPerfSeqAccumulate(
    "s1_use",
    io.toFtq.prediction.fire,
    Seq(
      ("ptage", debug_s1UsePtage),
      ("ubtb", debug_s1UseUbtb),
      ("fallThrough", !s1_taken)
    )
  )
  XSPerfAccumulate("s3_use_ras", s3_fire && s3_taken && s3_useRas)
  XSPerfAccumulate("s3_use_ittage", s3_fire && s3_taken && !s3_useRas && s3_useIttage)
  XSPerfAccumulate("s3_use_mbtb_tage", s3_fire && s3_prediction.attribute.isConditional)

  XSPerfSeqAccumulate(
    "finalPred_s1",
    s3_fire && !s3_override,
    BpuPredictionSource.Stage1.getValidSeq(s3_perfMeta.bpSource.s1Source)
  )

  XSPerfSeqAccumulate(
    "finalPred_s3",
    s3_fire && s3_override,
    BpuPredictionSource.Stage3.getValidSeq(s3_perfMeta.bpSource.s3Source)
  )

  XSPerfAccumulate("s1Invalid", !s1_valid)

  // taken mismatch
  private val perf_s1TakenSourceVec = BpuPredictionSource.Stage1.getValidSeq(
    s3_perfMeta.bpSource.s1Source,
    exclude = Set("Fallthrough"),
    thisPrefix = "s1"
  )
  private val perf_s3TakenSourceVec = BpuPredictionSource.Stage3.getValidSeq(
    s3_perfMeta.bpSource.s3Source,
    exclude = Set("Fallthrough"),
    thisPrefix = "s3"
  )

  XSPerfSeqAccumulate(
    s"s3Override_takenMismatch_s1fall",
    io.toFtq.prediction.fire && s3_override && s3_perfMeta.bpSource.s1Fallthrough,
    perf_s3TakenSourceVec
  )

  XSPerfSeqAccumulate(
    s"s3Override_takenMismatch_s3fall",
    io.toFtq.prediction.fire && s3_override && s3_perfMeta.bpSource.s3Fallthrough,
    perf_s1TakenSourceVec
  )

  // position mismatch
  XSPerfSeqAccumulate(
    s"s3Override_positionMismatch",
    io.toFtq.prediction.fire && s3_override &&
      s3_prediction.taken && s3_s1Prediction.taken &&
      s3_prediction.cfiPosition =/= s3_s1Prediction.cfiPosition,
    perf_s1TakenSourceVec
  )

  // attribute mismatch
  XSPerfSeqAccumulate(
    s"s3Override_attributeMismatch",
    io.toFtq.prediction.fire && s3_override &&
      s3_prediction.taken && s3_s1Prediction.taken &&
//      s3_prediction.cfiPosition === s3_s1Prediction.cfiPosition &&
      !(s3_prediction.attribute === s3_s1Prediction.attribute),
    perf_s1TakenSourceVec
  )

  // target mismatch
  // get a cartesian product of s1 source and s3 source
  private val perf_fullTakenSourceVec = BpuPredictionSource.Stage3.getValidSeq(
    s3_perfMeta.bpSource.s3Source,
    thatSeq = perf_s1TakenSourceVec,
    exclude = Set("Fallthrough"),
    thisPrefix = "s3"
  )

  XSPerfSeqAccumulate(
    s"s3Override_targetMismatch",
    io.toFtq.prediction.fire && s3_override &&
      s3_prediction.taken && s3_s1Prediction.taken &&
      s3_prediction.cfiPosition === s3_s1Prediction.cfiPosition &&
      !(s3_prediction.target === s3_s1Prediction.target),
    perf_fullTakenSourceVec
  )

  /* *** perf train *** */
  private val t0_mispredictBranch = train.mispredictBranch
  private val t0_mbtbMeta         = train.meta.mbtb
  private val t0_branches         = train.branches
  private val t0_mbtbHit          = t0_mbtbMeta.entries.flatten.map(_.hit(t0_mispredictBranch.bits)).reduce(_ || _)

  XSPerfSeqAccumulate(
    "train",
    io.fromFtq.train.valid,
    Seq(
      ("total", io.fromFtq.train.ready),
      ("stall", !io.fromFtq.train.ready)
    )
  )
  XSPerfSeqAccumulate(
    "train_branch",
    io.fromFtq.train.fire,
    Seq(
      ("total", true.B, PopCount(t0_branches.map(_.valid))),
      ("direct", true.B, PopCount(t0_branches.map(b => b.valid && b.bits.attribute.isDirect))),
      ("otherIndirect", true.B, PopCount(t0_branches.map(b => b.valid && b.bits.attribute.isOtherIndirect))),
      ("call", true.B, PopCount(t0_branches.map(b => b.valid && b.bits.attribute.isCall))),
      ("return", true.B, PopCount(t0_branches.map(b => b.valid && b.bits.attribute.isReturn))),
      ("conditional", true.B, PopCount(t0_branches.map(b => b.valid && b.bits.attribute.isConditional)))
    )
  )
  XSPerfSeqAccumulate(
    "train_mispredict",
    io.fromFtq.train.fire && t0_mispredictBranch.valid,
    Seq(
      ("total", true.B),
      ("direct", t0_mispredictBranch.bits.attribute.isDirect),
      ("otherIndirect", t0_mispredictBranch.bits.attribute.isOtherIndirect),
      ("call", t0_mispredictBranch.bits.attribute.isCall),
      ("return", t0_mispredictBranch.bits.attribute.isReturn),
      ("conditional", t0_mispredictBranch.bits.attribute.isConditional),
      ("conditional_because_mbtb_miss", t0_mispredictBranch.bits.attribute.isConditional && !t0_mbtbHit)
    )
  )
}
