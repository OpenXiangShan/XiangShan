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
import utility.DelayN
import utility.XSError
import utility.XSPerfAccumulate
import utility.XSPerfHistogram
import xiangshan.frontend.BpuToFtqIO
import xiangshan.frontend.FtqToBpuIO
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.abtb.AheadBtb
import xiangshan.frontend.bpu.ittage.Ittage
import xiangshan.frontend.bpu.mbtb.MainBtb
import xiangshan.frontend.bpu.phr.Phr
import xiangshan.frontend.bpu.phr.PhrAllFoldedHistories
import xiangshan.frontend.bpu.ras.Ras
import xiangshan.frontend.bpu.sc.Sc
import xiangshan.frontend.bpu.tage.Tage
import xiangshan.frontend.bpu.ubtb.MicroBtb

class Bpu(implicit p: Parameters) extends BpuModule with HalfAlignHelper {
  class DummyBpuIO extends Bundle {
    val ctrl:        BpuCtrl    = Input(new BpuCtrl)
    val resetVector: PrunedAddr = Input(PrunedAddr(PAddrBits))
    val fromFtq:     FtqToBpuIO = Flipped(new FtqToBpuIO)
    val toFtq:       BpuToFtqIO = new BpuToFtqIO
  }

  val io: DummyBpuIO = IO(new DummyBpuIO)

  /* *** submodules *** */
  private val fallThrough = Module(new FallThroughPredictor)
  private val ubtb        = Module(new MicroBtb)
  private val abtb        = Module(new AheadBtb)
  private val mbtb        = Module(new MainBtb)
  private val tage        = Module(new Tage)
  private val ittage      = Module(new Ittage)
  private val sc          = Module(new Sc)
  private val ras         = Module(new Ras)
  private val phr         = Module(new Phr)

  private def predictors: Seq[BasePredictor] = Seq(
    fallThrough,
    ubtb,
    abtb,
    mbtb,
    tage,
    sc,
    ittage,
    ras
  )

  /* *** aliases *** */
  private val commitUpdate = io.fromFtq.commit
  private val redirect     = io.fromFtq.redirect

  /* *** CSR ctrl sub-predictor enable *** */
  private val ctrl = DelayN(io.ctrl, 2) // delay 2 cycle for timing
  fallThrough.io.enable := true.B // fallThrough is always enabled
  ubtb.io.enable        := ctrl.ubtbEnable
  abtb.io.enable        := ctrl.abtbEnable
  mbtb.io.enable        := ctrl.mbtbEnable
  tage.io.enable        := ctrl.tageEnable
  sc.io.enable          := ctrl.scEnable
  ittage.io.enable      := ctrl.ittageEnable
  ras.io.enable         := false.B

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
  private val s4_flush = Wire(Bool()) // only used for abtb fast train

  private val s1_valid = RegInit(false.B)
  private val s2_valid = RegInit(false.B)
  private val s3_valid = RegInit(false.B)
  private val s4_valid = RegInit(false.B) // only used for abtb fast train

  private val s3_override = WireDefault(false.B)

  private val s1_prediction = Wire(new Prediction)
  private val s3_prediction = Wire(new Prediction)

  private val s3_meta = Wire(new BpuMeta)
  println("bpu meta width: " + s3_meta.getWidth)

  private val debug_bpId = RegInit(0.U(XLEN.W))

  private val s0_pc    = WireDefault(0.U.asTypeOf(PrunedAddr(VAddrBits)))
  private val s0_pcReg = RegEnable(s0_pc, !s0_stall)

  when(RegNext(RegNext(reset.asBool)) && !reset.asBool) {
    s0_pcReg := io.resetVector
  }

  private val s1_pc = RegEnable(s0_pc, s0_fire)
  private val s2_pc = RegEnable(s1_pc, s1_fire)
  private val s3_pc = RegEnable(s2_pc, s2_fire)
  private val s4_pc = RegEnable(s3_pc, s3_fire) // only used for abtb fast train

  // abtb meta won't be sent to ftq, used for abtb fast train
  private val s2_abtbMeta = RegEnable(abtb.io.meta, s1_fire)
  private val s3_abtbMeta = RegEnable(s2_abtbMeta, s2_fire)

  /* *** common inputs *** */
  private val stageCtrl = Wire(new StageCtrl)
  stageCtrl.s0_fire := s0_fire
  stageCtrl.s1_fire := s1_fire
  stageCtrl.s2_fire := s2_fire
  stageCtrl.s3_fire := s3_fire

  private val t0_compareMatrix = CompareMatrix(VecInit(io.fromFtq.train.bits.branches.map(_.bits.cfiPosition)))
  // mark all branches after the first mispredict as invalid
  // i.e. we have (valid, position, mispredict) for each branch:
  // (1, 2, 0), (1, 5, 1), (1, 8, 0)
  // then the first mispredict branch is @5, so mask should be (1, 1, 0)
  private val t0_firstMispredictMask = t0_compareMatrix.getLowerElementMask(
    VecInit(io.fromFtq.train.bits.branches.map(b => b.valid && b.bits.mispredict))
  )

  private val fastTrain = Wire(Valid(new BpuFastTrain))
  fastTrain.valid                := s3_valid
  fastTrain.bits.startVAddr      := s3_pc
  fastTrain.bits.finalPrediction := s3_prediction
  fastTrain.bits.abtbMeta        := s3_abtbMeta

  predictors.foreach { p =>
    // TODO: duplicate pc and fire to solve high fan-out issue
    p.io.startVAddr  := s0_pc
    p.io.stageCtrl   := stageCtrl
    p.io.train.valid := io.fromFtq.train.valid
    p.io.train.bits  := io.fromFtq.train.bits
    p.io.train.bits.branches.zipWithIndex.foreach { case (b, i) =>
      b.valid := io.fromFtq.train.bits.branches(i).valid && t0_firstMispredictMask(i)
    }
    p.io.fastTrain.foreach(_ := fastTrain) // fastTrain is an Option[Valid[BpuFastTrain]]
  }
  io.fromFtq.train.ready := predictors.map(_.io.train.ready).reduce(_ && _)

  /* *** predictor specific inputs *** */
  // FIXME: should use s3_prediction to train ubtb

  // abtb
  abtb.io.redirectValid       := redirect.valid
  abtb.io.overrideValid       := s3_override
  abtb.io.previousVAddr.valid := s4_valid
  abtb.io.previousVAddr.bits  := s4_pc

  // ras
  ras.io.redirect.valid          := redirect.valid
  ras.io.redirect.bits.attribute := redirect.bits.attribute
  ras.io.redirect.bits.brPc      := redirect.bits.startVAddr
  ras.io.redirect.bits.isRvc     := redirect.bits.isRvc
  ras.io.redirect.bits.meta      := redirect.bits.speculationMeta.rasMeta
  ras.io.redirect.bits.level     := 0.U(1.W)
  ras.io.commit.valid            := commitUpdate.valid
  ras.io.commit.bits.attribute   := commitUpdate.bits.attribute
  ras.io.commit.bits.meta        := commitUpdate.bits.rasMeta
  ras.io.commit.bits.pushAddr    := commitUpdate.bits.pushAddr
  ras.io.specIn.valid            := s3_fire
  ras.io.specIn.bits.startPc     := s3_pc.toUInt
  ras.io.specIn.bits.isRvc       := false.B
  ras.io.specIn.bits.attribute   := s3_prediction.attribute
  ras.io.specIn.bits.cfiPosition := s3_prediction.cfiPosition

  // tage
  tage.io.mbtbResult             := mbtb.io.result
  tage.io.foldedPathHist         := phr.io.s0_foldedPhr
  tage.io.foldedPathHistForTrain := phr.io.trainFoldedPhr

  // ittage
  ittage.io.s1_foldedPhr   := phr.io.s1_foldedPhr
  ittage.io.trainFoldedPhr := phr.io.trainFoldedPhr

  // sc
  sc.io.mbtbResult          := mbtb.io.result
  sc.io.foldedPathHist      := phr.io.s1_foldedPhr
  sc.io.trainFoldedPathHist := phr.io.trainFoldedPhr
  private val scTakenMask = sc.io.takenMask
  dontTouch(scTakenMask)

  private val s2_ftqPtr = RegEnable(io.fromFtq.bpuPtr, s1_fire)
  private val s3_ftqPtr = RegEnable(s2_ftqPtr, s2_fire)

  s4_flush := redirect.valid
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

  when(s4_flush)(s4_valid := false.B)
    .elsewhen(s3_fire)(s4_valid := !s3_flush)

  // s0_stall should be exclusive with any other PC source
  s0_stall := !(s1_valid || s3_override || redirect.valid)

  // s1 prediction selection:
  // if ubtb or abtb find a taken branch, use the corresponding prediction
  // otherwise, use fall-through prediction
  // TODO: maybe need compare position？
  s1_prediction :=
    MuxCase(
      fallThrough.io.prediction,
      Seq(
        ubtb.io.prediction.taken -> ubtb.io.prediction
//      abtb.io.prediction.taken -> abtb.io.prediction
      )
    )

  private val s2_mbtbResult    = mbtb.io.result
  private val s2_condTakenMask = tage.io.condTakenMask
  private val s2_jumpMask = VecInit(s2_mbtbResult.hitMask.zip(s2_mbtbResult.attributes).map { case (hit, attribute) =>
    hit && (attribute.isDirect || attribute.isIndirect)
  })
  private val s2_takenMask = VecInit(s2_condTakenMask.zip(s2_jumpMask).map { case (a, b) => a || b })
  private val s2_taken     = s2_takenMask.reduce(_ || _)

  private val s2_compareMatrix      = CompareMatrix(s2_mbtbResult.positions)
  private val s2_firstTakenBranchOH = s2_compareMatrix.getLeastElementOH(s2_takenMask)

  private val s3_taken                      = RegEnable(s2_taken, s2_fire)
  private val s3_mbtbResult                 = RegEnable(s2_mbtbResult, s2_fire)
  private val s3_firstTakenBranchOH         = RegEnable(s2_firstTakenBranchOH, s2_fire)
  private val s3_takenBranchPosition        = Mux1H(s3_firstTakenBranchOH, s3_mbtbResult.positions)
  private val s3_takenBranchAttribute       = Mux1H(s3_firstTakenBranchOH, s3_mbtbResult.attributes)
  private val s3_mbtbTarget                 = Mux1H(s3_firstTakenBranchOH, s3_mbtbResult.targets)
  private val s3_firstTakenBranchIsReturn   = s3_takenBranchAttribute.isReturn
  private val s3_firstTakenBranchIsIndirect = s3_takenBranchAttribute.isOtherIndirect

  private val s2_fallThroughPrediction = RegEnable(fallThrough.io.prediction, s1_fire)
  private val s3_fallThroughPrediction = RegEnable(s2_fallThroughPrediction, s2_fire)

  s3_prediction.taken       := s3_taken
  s3_prediction.cfiPosition := Mux(s3_taken, s3_takenBranchPosition, s3_fallThroughPrediction.cfiPosition)
  s3_prediction.attribute   := Mux(s3_taken, s3_takenBranchAttribute, s3_fallThroughPrediction.attribute)
  s3_prediction.target :=
    MuxCase(
      s3_fallThroughPrediction.target,
      Seq(
//        (s3_taken && s3_firstTakenBranchIsReturn)                               -> ras.io.topRetAddr,
        (s3_taken && s3_firstTakenBranchIsIndirect && ittage.io.prediction.hit) -> ittage.io.prediction.target,
        s3_taken                                                                -> s3_mbtbTarget
      )
    )

  private val s2_s1Prediction = RegEnable(s1_prediction, s1_fire)
  private val s3_s1Prediction = RegEnable(s2_s1Prediction, s2_fire)

  s3_override := s3_valid && !s3_prediction.isIdentical(s3_s1Prediction)

  // to Ftq
  io.toFtq.prediction.valid := s1_valid && s2_ready || s3_override
  when(s3_override) {
    io.toFtq.prediction.bits.fromStage(s3_pc, s3_prediction)
  }.otherwise {
    io.toFtq.prediction.bits.fromStage(s1_pc, s1_prediction)
  }
  io.toFtq.prediction.bits.s3Override := s3_override

  // tell ftq s3 ftqPtr for meta enqueue and s3 override
  io.toFtq.s3FtqPtr := s3_ftqPtr

  // mbtb meta
  private val s3_mbtbMeta = RegEnable(mbtb.io.meta, s2_fire)

  // tage meta
  private val s3_tageMeta = RegEnable(tage.io.meta, s2_fire)

  // ittage meta
  private val s3_ittageMeta = ittage.io.meta

  // sc meta
  private val s3_scMeta = sc.io.meta

  // ras meta
  private val s3_rasMeta = ras.io.specMeta

  // phr meta
  private val s2_phrMeta = RegEnable(phr.io.phrPtr, s1_fire)
  private val s3_phrMeta = RegEnable(s2_phrMeta, s2_fire)

  private val s3_speculationMeta = Wire(new BpuSpeculationMeta)
  s3_speculationMeta.phrHistPtr := s3_phrMeta
  s3_speculationMeta.rasMeta    := s3_rasMeta
  s3_speculationMeta.topRetAddr := ras.io.topRetAddr

  s3_meta.mbtb   := s3_mbtbMeta
  s3_meta.tage   := s3_tageMeta
  s3_meta.ras    := s3_rasMeta
  s3_meta.phr    := s3_phrMeta
  s3_meta.ittage := s3_ittageMeta
  s3_meta.sc     := s3_scMeta

  s3_meta.debug_startVAddr := s3_pc
  s3_meta.debug_bpId       := debug_bpId

  io.toFtq.meta.valid := s3_valid
  io.toFtq.meta.bits  := s3_meta

  io.toFtq.speculationMeta.valid := s3_valid
  io.toFtq.speculationMeta.bits  := s3_speculationMeta

  s0_pc := MuxCase(
    s0_pcReg,
    Seq(
      redirect.valid -> redirect.bits.target,
      s3_override    -> s3_prediction.target,
      s1_valid       -> s1_prediction.target
    )
  )

  // phr train
  private val phrsWire       = WireInit(0.U.asTypeOf(Vec(PhrHistoryLength, Bool())))
  private val s0_foldedPhr   = WireInit(0.U.asTypeOf(new PhrAllFoldedHistories(AllFoldedHistoryInfo)))
  private val s1_foldedPhr   = WireInit(0.U.asTypeOf(new PhrAllFoldedHistories(AllFoldedHistoryInfo)))
  private val s2_foldedPhr   = WireInit(0.U.asTypeOf(new PhrAllFoldedHistories(AllFoldedHistoryInfo)))
  private val s3_foldedPhr   = WireInit(0.U.asTypeOf(new PhrAllFoldedHistories(AllFoldedHistoryInfo)))
  private val trainFoldedPhr = WireInit(0.U.asTypeOf(new PhrAllFoldedHistories(AllFoldedHistoryInfo)))
  phr.io.train.s0_stall      := s0_stall
  phr.io.train.stageCtrl     := stageCtrl
  phr.io.train.redirect      := redirect
  phr.io.train.s3_override   := s3_override
  phr.io.train.s3_prediction := s3_prediction
  phr.io.train.s3_pc         := s3_pc
  phr.io.train.s1_valid      := s1_fire
  phr.io.train.s1_prediction := s1_prediction
  phr.io.train.s1_pc         := s1_pc

  phr.io.commit.valid := io.fromFtq.train.fire
  phr.io.commit.bits  := io.fromFtq.train.bits

  s0_foldedPhr   := phr.io.s0_foldedPhr
  s1_foldedPhr   := phr.io.s1_foldedPhr
  s2_foldedPhr   := phr.io.s2_foldedPhr
  s3_foldedPhr   := phr.io.s3_foldedPhr
  trainFoldedPhr := phr.io.trainFoldedPhr
  phrsWire       := phr.io.phrs

  private val phrsWireValue = phrsWire.asUInt
  private val redirectPhrValue =
    (Cat(phrsWire.asUInt, phrsWire.asUInt) >> (redirect.bits.speculationMeta.phrHistPtr.value + 1.U))(
      PhrHistoryLength - 1,
      0
    )

  dontTouch(s0_foldedPhr)
  dontTouch(s1_foldedPhr)
  dontTouch(s2_foldedPhr)
  dontTouch(s3_foldedPhr)
  dontTouch(trainFoldedPhr)
  dontTouch(phrsWireValue)
  dontTouch(redirectPhrValue)

  // Power-on reset
  private val powerOnResetState = RegInit(true.B)
  when(s0_fire) {
    // When BPU pipeline first time fire, we consider power-on reset is done
    powerOnResetState := false.B
  }
  XSError(
    !powerOnResetState && s0_stall && s0_pc =/= s0_pcReg,
    "s0_stall but s0_pc is different from s0_pc_reg"
  )

  /* *** check abtb output *** */
  when(abtb.io.prediction.taken) {
    assert(abtb.io.debug_startVAddr === s1_pc)
  }

  /* *** BpTrace *** */
  when(io.toFtq.meta.fire) {
    debug_bpId := debug_bpId + 1.U
  }

  private class PredictionTrace extends Bundle {
    val s1Prediction = new Prediction
    val s3Prediction = new Prediction
    val meta         = new BpuMeta
  }

  private class TrainTrace extends Bundle {
    val train = new BpuTrain
  }

  private val predictionTable = ChiselDB.createTable("BpuPredictionTrace", new PredictionTrace, EnableBpTrace)
  private val trainTable      = ChiselDB.createTable("BpuTrainTrace", new TrainTrace, EnableBpTrace)

  private val predictionTrace = Wire(new PredictionTrace)
  predictionTrace.s1Prediction := s3_s1Prediction
  predictionTrace.s3Prediction := s3_prediction
  predictionTrace.meta         := s3_meta

  private val trainTrace = Wire(new TrainTrace)
  trainTrace.train := io.fromFtq.train.bits

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
    FetchBlockInstNum
  )
  XSPerfAccumulate("s1_use_ubtb", io.toFtq.prediction.fire && ubtb.io.prediction.taken)
  XSPerfAccumulate("s1_use_abtb", io.toFtq.prediction.fire && !ubtb.io.prediction.taken && abtb.io.prediction.taken)
  XSPerfAccumulate(
    "s1_use_fallThrough",
    io.toFtq.prediction.fire && !ubtb.io.prediction.taken && !abtb.io.prediction.taken
  )
  XSPerfAccumulate(
    "s3_use_ittage",
    s3_fire && s3_taken && s3_firstTakenBranchIsIndirect && ittage.io.prediction.hit && !s3_firstTakenBranchIsReturn
  )

  XSPerfAccumulate("s1Invalid", !s1_valid)

  /* *** perf train *** */

  private val t0_fire             = io.fromFtq.train.fire
  private val t0_mispredictBranch = io.fromFtq.train.bits.mispredictBranch
  private val t0_mbtbMeta         = io.fromFtq.train.bits.meta.mbtb
  private val t0_branches         = io.fromFtq.train.bits.branches
  private val t0_mbtbHit =
    t0_mbtbMeta.hitMask.zip(t0_mbtbMeta.positions).zip(t0_mbtbMeta.attributes).map {
      case ((hit, position), attribute) =>
        hit && position === t0_mispredictBranch.bits.cfiPosition && attribute === t0_mispredictBranch.bits.attribute
    }.reduce(_ || _)

  private val perf_conditionalMispredict =
    t0_fire && t0_mispredictBranch.valid && t0_mispredictBranch.bits.attribute.isConditional
  private val perf_directMispredict =
    t0_fire && t0_mispredictBranch.valid && t0_mispredictBranch.bits.attribute.isDirect
  private val perf_indirectMispredict =
    t0_fire && t0_mispredictBranch.valid && t0_mispredictBranch.bits.attribute.isOtherIndirect
  private val perf_callMispredict =
    t0_fire && t0_mispredictBranch.valid && t0_mispredictBranch.bits.attribute.isCall
  private val perf_returnMispredict =
    t0_fire && t0_mispredictBranch.valid && t0_mispredictBranch.bits.attribute.isReturn

  XSPerfAccumulate("train", t0_fire)
  XSPerfAccumulate("train_total_branch", Mux(t0_fire, PopCount(t0_branches.map(_.valid)), 0.U))
  XSPerfAccumulate(
    "train_total_conditional",
    Mux(t0_fire, PopCount(t0_branches.map(b => b.valid && b.bits.attribute.isConditional)), 0.U)
  )
  XSPerfAccumulate(
    "train_total_direct",
    Mux(t0_fire, PopCount(t0_branches.map(b => b.valid && b.bits.attribute.isDirect)), 0.U)
  )
  XSPerfAccumulate(
    "train_total_indirect",
    Mux(t0_fire, PopCount(t0_branches.map(b => b.valid && b.bits.attribute.isOtherIndirect)), 0.U)
  )
  XSPerfAccumulate(
    "train_total_call",
    Mux(t0_fire, PopCount(t0_branches.map(b => b.valid && b.bits.attribute.isCall)), 0.U)
  )
  XSPerfAccumulate(
    "train_total_return",
    Mux(t0_fire, PopCount(t0_branches.map(b => b.valid && b.bits.attribute.isReturn)), 0.U)
  )
  XSPerfAccumulate("train_conditional_mispredict", perf_conditionalMispredict)
  XSPerfAccumulate("train_direct_mispredict", perf_directMispredict)
  XSPerfAccumulate("train_indirect_mispredict", perf_indirectMispredict)
  XSPerfAccumulate("train_call_mispredict", perf_callMispredict)
  XSPerfAccumulate("train_return_mispredict", perf_returnMispredict)
  XSPerfAccumulate("train_conditional_mispredict_because_mbtb_miss", perf_conditionalMispredict && !t0_mbtbHit)
}
