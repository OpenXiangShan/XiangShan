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
import xiangshan.frontend.BpuToFtqIO
import xiangshan.frontend.FrontendTopDownBundle
import xiangshan.frontend.FtqToBpuIO
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.abtb.AheadBtb
import xiangshan.frontend.bpu.history.commonhr.CommonHR
import xiangshan.frontend.bpu.history.commonhr.CommonHRMeta
import xiangshan.frontend.bpu.history.phr.Phr
import xiangshan.frontend.bpu.history.phr.PhrAllFoldedHistories
import xiangshan.frontend.bpu.ittage.Ittage
import xiangshan.frontend.bpu.mbtb.MainBtb
import xiangshan.frontend.bpu.ras.Ras
import xiangshan.frontend.bpu.sc.Sc
import xiangshan.frontend.bpu.tage.Tage
import xiangshan.frontend.bpu.ubtb.MicroBtb
import xiangshan.frontend.bpu.utage.MicroTage
import xiangshan.frontend.bpu.utage.MicroTageMeta

class Bpu(implicit p: Parameters) extends BpuModule with HalfAlignHelper {
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
  private val abtb        = Module(new AheadBtb)
  private val utage       = Module(new MicroTage)
  private val mbtb        = Module(new MainBtb)
  private val tage        = Module(new Tage)
  private val ittage      = Module(new Ittage)
  private val sc          = Module(new Sc)
  private val ras         = Module(new Ras)
  private val phr         = Module(new Phr)
  private val commonHR    = Module(new CommonHR)

  private def predictors: Seq[BasePredictor] = Seq(
    fallThrough,
    ubtb,
    abtb,
    utage,
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
  utage.io.enable       := true.B
  if (env.EnableConstantin && !env.FPGAPlatform) {
    ubtb.io.enable   := Mux(constCtrl(0), constCtrl(1), ctrl.ubtbEnable)
    abtb.io.enable   := Mux(constCtrl(0), constCtrl(2), ctrl.abtbEnable)
    mbtb.io.enable   := Mux(constCtrl(0), constCtrl(3), ctrl.mbtbEnable)
    tage.io.enable   := Mux(constCtrl(0), constCtrl(4), ctrl.tageEnable)
    sc.io.enable     := Mux(constCtrl(0), constCtrl(5), ctrl.scEnable)
    ittage.io.enable := Mux(constCtrl(0), constCtrl(6), ctrl.ittageEnable)
    ras.io.enable    := Mux(constCtrl(0), constCtrl(7), ctrl.rasEnable)
    // utage.io.enable  := Mux(constCtrl(0), constCtrl(8), ctrl.utageEnable)
  } else {
    ubtb.io.enable   := ctrl.ubtbEnable
    abtb.io.enable   := ctrl.abtbEnable
    mbtb.io.enable   := ctrl.mbtbEnable
    tage.io.enable   := ctrl.tageEnable
    sc.io.enable     := ctrl.scEnable
    ittage.io.enable := ctrl.ittageEnable
    ras.io.enable    := ctrl.rasEnable
    // utage.io.enable  := ctrl.utageEnable
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

  private val s0_startPc    = WireDefault(0.U.asTypeOf(PrunedAddr(VAddrBits)))
  private val s0_startPcReg = RegEnable(s0_startPc, !s0_stall)

  when(RegNext(RegNext(reset.asBool)) && !reset.asBool) {
    s0_startPcReg := io.resetVector
  }

  private val s1_startPc = RegEnable(s0_startPc, s0_fire)
  private val s2_startPc = RegEnable(s1_startPc, s1_fire)
  private val s3_startPc = RegEnable(s2_startPc, s2_fire)

  // abtb meta won't be sent to ftq, used for abtb fast train
  private val s2_abtbMeta = RegEnable(abtb.io.meta, s1_fire)
  private val s3_abtbMeta = RegEnable(s2_abtbMeta, s2_fire)

  private val s1_utageMeta = Wire(new MicroTageMeta)
  private val s2_utageMeta = RegEnable(s1_utageMeta, s1_fire)
  private val s3_utageMeta = RegEnable(s2_utageMeta, s2_fire)

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

  private val fastTrain = Wire(Valid(new BpuFastTrain))
  fastTrain.valid                := s3_valid
  fastTrain.bits.startPc         := s3_startPc
  fastTrain.bits.finalPrediction := s3_prediction
  fastTrain.bits.abtbMeta        := s3_abtbMeta
  fastTrain.bits.utageMeta       := s3_utageMeta
  fastTrain.bits.hasOverride     := s3_override

  predictors.foreach { p =>
    // TODO: duplicate pc and fire to solve high fan-out issue
    p.io.startPc   := s0_startPc
    p.io.stageCtrl := stageCtrl
    p.io.train     := train
    p.io.fastTrain.foreach(_ := fastTrain) // fastTrain is an Option[Valid[BpuFastTrain]]
  }
  io.fromFtq.train.ready := predictors.map(_.io.trainReady).reduce(_ && _)

  /* *** predictor specific inputs *** */
  abtb.io.redirectValid := redirect.valid
  abtb.io.overrideValid := s3_override

  utage.io.foldedPathHist         := phr.io.s0_foldedPhr
  utage.io.foldedPathHistForTrain := phr.io.s3_foldedPhr
  utage.io.abtbPrediction         := abtb.io.prediction

  ras.io.redirect                := redirect
  ras.io.commit                  := commit
  ras.io.specIn.valid            := s3_fire
  ras.io.specIn.bits.startPc     := s3_startPc.toUInt
  ras.io.specIn.bits.attribute   := s3_prediction.attribute
  ras.io.specIn.bits.cfiPosition := s3_prediction.cfiPosition

  tage.io.fromMainBtb.result             := mbtb.io.result
  tage.io.fromPhr.foldedPathHist         := phr.io.s0_foldedPhr
  tage.io.fromPhr.foldedPathHistForTrain := phr.io.trainFoldedPhr
  tage.io.debug_trainValid               := io.fromFtq.train.valid // for perf counters

  ittage.io.s1_foldedPhr   := phr.io.s1_foldedPhr
  ittage.io.trainFoldedPhr := phr.io.trainFoldedPhr

  sc.io.mbtbResult          := mbtb.io.result
  sc.io.providerTakenCtrs   := tage.io.toSc.providerTakenCtrVec
  sc.io.foldedPathHist      := phr.io.s0_foldedPhr
  sc.io.trainFoldedPathHist := phr.io.trainFoldedPhr
  sc.io.s3_override         := s3_override
  sc.io.commonHR            := commonHR.io.s0_commonHR

  s3_flush := redirect.valid
  s2_flush := s3_flush || s3_override
  s1_flush := s2_flush

  s1_ready := s1_fire || !s1_valid || s1_flush
  s2_ready := s2_fire || !s2_valid
  s3_ready := s3_fire || !s3_valid

  s0_fire := s1_ready && predictors.map(_.io.resetDone).reduce(_ && _)
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

  // * *** s1 prediction selection *** */
  private val s1_btbPrediction = VecInit(ubtb.io.prediction) ++ abtb.io.prediction
  private val s1_utageHitMask = VecInit(s1_btbPrediction.map { pred =>
    pred.valid && utage.io.prediction.valid && utage.io.prediction.bits.cfiPosition === pred.bits.cfiPosition
  })
  private val s1_takenMask = VecInit(s1_btbPrediction.zipWithIndex.map { case (pred, i) =>
    val utageHit   = s1_utageHitMask(i)
    val utageTaken = utage.io.prediction.bits.taken
    pred.valid && (
      pred.bits.attribute.isDirect ||
        pred.bits.attribute.isIndirect ||
        pred.bits.attribute.isConditional && Mux(utageHit, utageTaken, pred.bits.taken)
    )
  })

  // the old way to get taken mask
//  private val s1_baseTakenMask = VecInit(s1_btbPrediction.map { pred =>
//    pred.valid && (
//      pred.bits.attribute.isDirect ||
//        pred.bits.attribute.isIndirect ||
//        pred.bits.attribute.isConditional && pred.bits.taken
//    )
//  })
//  private val s1_microTageTakenMask = VecInit(s1_btbPrediction.zip(s1_utageHitMask).map { case (e, microTageHit) =>
//    e.valid && (
//      e.bits.attribute.isDirect ||
//        e.bits.attribute.isIndirect ||
//        e.bits.attribute.isConditional && Mux(microTageHit, utage.io.prediction.bits.taken, false.B)
//    )
//  })
//  private val s1_useMicroTage = s1_utageHitMask.reduce(_ || _)
//  private val s1_takenMask    = Mux(s1_useMicroTage, s1_microTageTakenMask, s1_baseTakenMask)

  private val s1_taken              = s1_takenMask.reduce(_ || _)
  private val s1_compareMatrix      = CompareMatrix(VecInit(s1_btbPrediction.map(_.bits.cfiPosition)))
  private val s1_firstTakenBranchOH = s1_compareMatrix.getLeastElementOH(s1_takenMask)
  private val s1_firstTakenBranch   = Mux1H(s1_firstTakenBranchOH, s1_btbPrediction)

  s1_prediction       := Mux(s1_taken, s1_firstTakenBranch.bits, fallThrough.io.prediction)
  s1_prediction.taken := s1_taken

  private val debug_s1UseUbtb      = s1_taken && s1_firstTakenBranchOH(0) && !s1_utageHitMask(0)
  private val debug_s1UseUbtbUtage = s1_taken && s1_firstTakenBranchOH(0) && s1_utageHitMask(0)
  private val debug_s1UseAbtb      = s1_taken && !s1_firstTakenBranchOH(0) && !s1_utageHitMask.drop(1).reduce(_ || _)
  private val debug_s1UseAbtbUtage = s1_taken && !s1_firstTakenBranchOH(0) && s1_utageHitMask.drop(1).reduce(_ || _)

  s1_utageMeta := utage.io.meta.bits
  s1_utageMeta.debug_useMicroTage.foreach(_ := s1_utageHitMask.reduce(_ || _))

  private val s2_mbtbResult  = mbtb.io.result
  private val s2_scUsed      = sc.io.scUsed
  private val s2_scTakenMask = sc.io.scTakenMask
  private val s2_scFlipTage = VecInit((tage.io.prediction zip s2_scUsed zip s2_scTakenMask).map {
    case ((p, useSc), scTaken) =>
      useSc && (p.providerPred =/= scTaken)
  }) // for bpSource counter
  private val s2_condTakenMask = VecInit((s2_mbtbResult zip tage.io.prediction zip s2_scUsed zip s2_scTakenMask).map {
    case (((e, p), useSc), scTaken) =>
      e.valid && e.bits.attribute.isConditional &&
      MuxCase(
        e.bits.taken,
        Seq(
          useSc         -> scTaken,
          p.useProvider -> p.providerPred,
          p.hasAlt      -> p.altPred
        )
      )
  })

  private val s2_jumpMask = VecInit(s2_mbtbResult.map { e =>
    e.valid && (e.bits.attribute.isDirect || e.bits.attribute.isIndirect)
  })
  private val s2_takenMask = VecInit(s2_condTakenMask.zip(s2_jumpMask).map { case (a, b) => a || b })
  private val s2_taken     = s2_takenMask.reduce(_ || _)

  private val s2_compareMatrix      = CompareMatrix(VecInit(s2_mbtbResult.map(_.bits.cfiPosition)))
  private val s2_firstTakenBranchOH = s2_compareMatrix.getLeastElementOH(s2_takenMask)

  /* *** s3 prediction selection *** */
  private val s3_taken              = RegEnable(s2_taken, s2_fire)
  private val s3_mbtbResult         = RegEnable(s2_mbtbResult, s2_fire)
  private val s3_firstTakenBranchOH = RegEnable(s2_firstTakenBranchOH, s2_fire)
  private val s3_firstTakenBranch   = Mux1H(s3_firstTakenBranchOH, s3_mbtbResult)
  private val s3_useRas             = s3_firstTakenBranch.bits.attribute.isReturn
  private val s3_useIttage          = s3_firstTakenBranch.bits.attribute.needIttage && ittage.io.prediction.hit

  private val s2_fallThroughPrediction = RegEnable(fallThrough.io.prediction, s1_fire)
  private val s3_fallThroughPrediction = RegEnable(s2_fallThroughPrediction, s2_fire)

  private val s3_takenMask = RegEnable(s2_takenMask, s2_fire)
  mbtb.io.s3_takenMask := s3_takenMask

  s3_prediction.taken       := s3_taken
  s3_prediction.cfiPosition := Mux(s3_taken, s3_firstTakenBranch.bits.cfiPosition, s3_fallThroughPrediction.cfiPosition)
  s3_prediction.attribute   := Mux(s3_taken, s3_firstTakenBranch.bits.attribute, s3_fallThroughPrediction.attribute)
  s3_prediction.target :=
    MuxCase(
      s3_fallThroughPrediction.target,
      Seq(
        (s3_taken && s3_useRas)    -> ras.io.topRetAddr,
        (s3_taken && s3_useIttage) -> ittage.io.prediction.target,
        s3_taken                   -> s3_firstTakenBranch.bits.target
      )
    )

  private val s2_s1Prediction = RegEnable(s1_prediction, s1_fire)
  private val s3_s1Prediction = RegEnable(s2_s1Prediction, s2_fire)

  s3_override := s3_valid && !(s3_prediction === s3_s1Prediction)

  private val s2_phrMeta = RegEnable(phr.io.phrMeta, s1_fire)
  private val s3_phrMeta = RegEnable(s2_phrMeta, s2_fire)

  private val s3_commonHRMeta = WireInit(0.U.asTypeOf(new CommonHRMeta))
  s3_commonHRMeta.ghr      := commonHR.io.commonHR.ghr
  s3_commonHRMeta.bw       := commonHR.io.commonHR.bw
  s3_commonHRMeta.hitMask  := VecInit(s3_mbtbResult.map(_.valid))
  s3_commonHRMeta.position := VecInit(s3_mbtbResult.map(_.bits.cfiPosition))

  private val s3_redirectMeta = Wire(new BpuRedirectMeta)
  s3_redirectMeta.phr          := s3_phrMeta
  s3_redirectMeta.commonHRMeta := s3_commonHRMeta
  s3_redirectMeta.ras          := ras.io.redirectMeta

  private val s3_resolveMeta = Wire(new BpuResolveMeta)
  s3_resolveMeta.mbtb   := RegEnable(mbtb.io.meta, s2_fire)
  s3_resolveMeta.tage   := RegEnable(tage.io.meta, s2_fire)
  s3_resolveMeta.sc     := sc.io.meta
  s3_resolveMeta.ittage := ittage.io.meta
  s3_resolveMeta.phr    := s3_phrMeta
  s3_resolveMeta.debug_utage.foreach(_ := s3_utageMeta)

  private val s3_commitMeta = Wire(new BpuCommitMeta)
  s3_commitMeta.ras := ras.io.commitMeta

  println(s"bpu redirect meta width: ${s3_redirectMeta.getWidth}")
  println(s"bpu resolve meta width: ${s3_resolveMeta.getWidth}")
  println(s"bpu commit meta width: ${s3_commitMeta.getWidth}")

  /* *** bpu to ftq io *** */
  io.toFtq.prediction.valid := s1_valid && s2_ready || s3_override
  when(s3_override) {
    io.toFtq.prediction.bits.fromStage(s3_startPc, s3_prediction)
  }.otherwise {
    io.toFtq.prediction.bits.fromStage(s1_startPc, s1_prediction)
  }
  io.toFtq.prediction.bits.s3Override := s3_override

  // used for meta enqueue and s3 override
  private val s2_ftqPtr = RegEnable(io.fromFtq.bpuPtr, s1_fire)
  private val s3_ftqPtr = RegEnable(s2_ftqPtr, s2_fire)
  io.toFtq.s3FtqPtr := s3_ftqPtr

  io.toFtq.meta.valid             := s3_valid
  io.toFtq.meta.bits.redirectMeta := s3_redirectMeta
  io.toFtq.meta.bits.resolveMeta  := s3_resolveMeta
  io.toFtq.meta.bits.commitMeta   := s3_commitMeta

  /* *** s0_startPc selection *** */
  s0_startPc := MuxCase(
    s0_startPcReg,
    Seq(
      redirect.valid -> redirect.bits.target,
      s3_override    -> s3_prediction.target,
      s1_valid       -> s1_prediction.target
    )
  )

  private val phrBits        = WireInit(0.U(PhrHistoryLength.W))
  private val s0_foldedPhr   = WireInit(0.U.asTypeOf(new PhrAllFoldedHistories(AllFoldedHistoryInfo)))
  private val s1_foldedPhr   = WireInit(0.U.asTypeOf(new PhrAllFoldedHistories(AllFoldedHistoryInfo)))
  private val s2_foldedPhr   = WireInit(0.U.asTypeOf(new PhrAllFoldedHistories(AllFoldedHistoryInfo)))
  private val s3_foldedPhr   = WireInit(0.U.asTypeOf(new PhrAllFoldedHistories(AllFoldedHistoryInfo)))
  private val trainFoldedPhr = WireInit(0.U.asTypeOf(new PhrAllFoldedHistories(AllFoldedHistoryInfo)))

  phr.io.train.s0_stall      := s0_stall
  phr.io.train.stageCtrl     := stageCtrl
  phr.io.train.redirect      := redirect
  phr.io.train.s3_override   := s3_override
  phr.io.train.s3_phrMeta    := s3_phrMeta
  phr.io.train.s3_prediction := s3_prediction
  phr.io.train.s3_startPc    := s3_startPc
  phr.io.train.s1_valid      := s1_fire
  phr.io.train.s1_prediction := s1_prediction
  phr.io.train.s1_startPc    := s1_startPc

  phr.io.commit.valid := io.fromFtq.train.fire
  phr.io.commit.bits  := train

  s0_foldedPhr   := phr.io.s0_foldedPhr
  s1_foldedPhr   := phr.io.s1_foldedPhr
  s2_foldedPhr   := phr.io.s2_foldedPhr
  s3_foldedPhr   := phr.io.s3_foldedPhr
  trainFoldedPhr := phr.io.trainFoldedPhr
  phrBits        := phr.io.phr.asUInt

  dontTouch(phrBits)

  // ghr update
  commonHR.io.stageCtrl           := stageCtrl
  commonHR.io.update.startPc      := s3_startPc
  commonHR.io.update.target       := s3_prediction.target
  commonHR.io.update.taken        := s3_taken
  commonHR.io.update.firstTakenOH := s3_firstTakenBranchOH
  commonHR.io.update.position     := VecInit(s3_mbtbResult.map(_.bits.cfiPosition))
  commonHR.io.update.hitMask      := VecInit(s3_mbtbResult.map(_.valid))
  commonHR.io.redirect.valid      := redirect.valid
  commonHR.io.redirect.cfiPc      := redirect.bits.cfiPc
  commonHR.io.redirect.target     := redirect.bits.target
  commonHR.io.redirect.taken      := redirect.bits.taken
  commonHR.io.redirect.meta       := redirect.bits.meta.commonHRMeta
  private val s0_commonHR   = commonHR.io.s0_commonHR
  private val commonHRValue = commonHR.io.commonHR
  dontTouch(s0_commonHR)
  dontTouch(commonHRValue)

  // Power-on reset
  private val powerOnResetState = RegInit(true.B)
  when(s0_fire) {
    // When BPU pipeline first time fire, we consider power-on reset is done
    powerOnResetState := false.B
  }
  XSError(
    !powerOnResetState && s0_stall && s0_startPc =/= s0_startPcReg,
    "s0_stall but s0_startPc is different from s0_startPcReg"
  )

  /* *** check abtb output *** */
  when(io.toFtq.prediction.fire && abtb.io.prediction.map(_.valid).reduce(_ || _)) {
    assert(abtb.io.debug_startPc === s1_startPc)
  }

  /* *** Debug Meta *** */
  // used for performance counters
  private val s3_condTakenMask      = RegEnable(s2_condTakenMask, s2_fire)
  private val s3_scFlipTage         = RegEnable(s2_scFlipTage, s2_fire)
  private val s3_firstTakenPosition = Mux1H(s3_firstTakenBranchOH, VecInit(s3_mbtbResult.map(_.bits.cfiPosition)))
  private val s3_firstTakenBlameSc  = Mux1H(s3_firstTakenBranchOH, s3_scFlipTage)
  // if the branch before the first take has a flipped and !s3_taken && flipped, then blamed on sc
  // first tage taken flip to not taken
  private val s3_firstT2NT = VecInit((s3_mbtbResult zip s3_scFlipTage).map {
    case (info, flip) =>
      flip && (s3_taken && info.bits.cfiPosition < s3_firstTakenPosition)
  }).reduce(_ || _) || (!s3_taken && s3_scFlipTage.reduce(_ || _))
  // see class BpuPredictionSource in bpu/Bundles.scala:137
  private val s1_predictionSource =
    MuxCase(
      BpuPredictionSource.Stage1.Fallthrough,
      Seq(
        debug_s1UseUbtb      -> BpuPredictionSource.Stage1.Ubtb,
        debug_s1UseUbtbUtage -> BpuPredictionSource.Stage1.UbtbUtage,
        debug_s1UseAbtb      -> BpuPredictionSource.Stage1.Abtb,
        debug_s1UseAbtbUtage -> BpuPredictionSource.Stage1.AbtbUtage
      )
    )
  private val s3_predictionSource = PriorityEncoder(Seq(
    s3_taken && s3_useRas,                                                                                    // RAS
    s3_taken && s3_useIttage,                                                                                 // ITTage
    s3_firstTakenBranch.bits.attribute.isConditional && ((s3_taken && s3_firstTakenBlameSc) || s3_firstT2NT), // MbtbSc
    s3_taken && s3_firstTakenBranch.bits.attribute.isConditional, // MbtbTage
    s3_taken,                                                     // Mbtb
    (s3_mbtbResult zip s3_condTakenMask).map { case (info, taken) =>
      info.valid && info.bits.attribute.isConditional && !taken
    }.reduce(_ || _), // FallthroughTage
    true.B            // Fallthrough
  ))

  private val s2_s1PredictionSource = RegEnable(s1_predictionSource, s1_fire)
  private val s3_s1PredictionSource = RegEnable(s2_s1PredictionSource, s2_fire)

  private val s3_perfMeta = Wire(new BpuPerfMeta)
  s3_perfMeta.startPc             := s3_startPc
  s3_perfMeta.bpId                := debug_bpId
  s3_perfMeta.s1Prediction        := s3_s1Prediction
  s3_perfMeta.s3Prediction        := s3_prediction
  s3_perfMeta.bpSource.s1Source   := s3_s1PredictionSource
  s3_perfMeta.bpSource.s3Source   := s3_predictionSource
  s3_perfMeta.bpSource.s3Override := s3_override

  io.toFtq.perfMeta := s3_perfMeta
  // TODO: override reason and redirect reason
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
      io.toFtq.prediction.bits.takenCfiOffset.valid,
      io.toFtq.prediction.bits.takenCfiOffset.bits,
      FetchBlockInstNum.U
    ),
    io.toFtq.prediction.fire,
    0,
    FetchBlockInstNum + 1
  )
  XSPerfAccumulate(
    "s1_use",
    io.toFtq.prediction.fire,
    Seq(
      ("ubtb", debug_s1UseUbtb),
      ("abtb", debug_s1UseAbtb),
      ("ubtb_microTage", debug_s1UseUbtbUtage),
      ("abtb_microTage", debug_s1UseAbtbUtage),
      ("fallThrough", !s1_taken)
    )
  )
  XSPerfAccumulate("s3_use_ras", s3_fire && s3_taken && s3_useRas)
  XSPerfAccumulate("s3_use_ittage", s3_fire && s3_taken && !s3_useRas && s3_useIttage)
  XSPerfAccumulate("s3_use_mbtb_tage", s3_fire && s3_prediction.attribute.isConditional)

  XSPerfAccumulate(
    "finalPred_s1",
    s3_fire && !s3_override,
    BpuPredictionSource.Stage1.getValidSeq(s3_perfMeta.bpSource.s1Source)
  )

  XSPerfAccumulate(
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
    exclude = Set("Fallthrough", "FallthroughTage"),
    thisPrefix = "s3"
  )

  XSPerfAccumulate(
    s"s3Override_takenMismatch_s1fall",
    io.toFtq.prediction.fire && s3_override && s3_perfMeta.bpSource.s1Fallthrough,
    perf_s3TakenSourceVec
  )

  XSPerfAccumulate(
    s"s3Override_takenMismatch_s3fall",
    io.toFtq.prediction.fire && s3_override && s3_perfMeta.bpSource.s3Fallthrough,
    perf_s1TakenSourceVec
  )

  XSPerfAccumulate(
    s"s3Override_takenMismatch_s3fallTage",
    io.toFtq.prediction.fire && s3_override && s3_perfMeta.bpSource.s3FallthroughTage,
    perf_s1TakenSourceVec
  )

  // position mismatch
  XSPerfAccumulate(
    s"s3Override_positionMismatch",
    io.toFtq.prediction.fire && s3_override &&
      s3_prediction.taken && s3_s1Prediction.taken &&
      s3_prediction.cfiPosition =/= s3_s1Prediction.cfiPosition,
    perf_s1TakenSourceVec
  )

  // attribute mismatch
  XSPerfAccumulate(
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
    exclude = Set("Fallthrough", "FallthroughTage"),
    thisPrefix = "s3"
  )

  XSPerfAccumulate(
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

  XSPerfAccumulate(
    "train",
    io.fromFtq.train.fire,
    Seq(
      ("total", true.B),
      ("stall", !io.fromFtq.train.ready)
    )
  )
  XSPerfAccumulate(
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
  XSPerfAccumulate(
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
