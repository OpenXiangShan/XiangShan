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
import xiangshan.frontend.bpu.abtb.AheadBtb
import xiangshan.frontend.bpu.block2.Block2Predictor
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
import xiangshan.frontend.bpu.utage.MicroTage
import xiangshan.frontend.bpu.utage.MicroTageMeta

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
  private val abtb        = Module(new AheadBtb)
  private val utage       = Module(new MicroTage)
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
  private val block2      = Module(new Block2Predictor)

  private def predictors: Seq[BasePredictor] = Seq(
    fallThrough,
    ubtb,
    abtb,
    utage,
    uras,
    ptage,
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
  uras.io.enable        := true.B
  ptage.io.enable       := true.B
  block2.io.enable      := true.B
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

  private val s2_override = WireDefault(false.B)
  private val s3_override = WireDefault(false.B)

  private val s1_prediction = Wire(new Prediction)
  private val s2_prediction = Wire(new Prediction)
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

  // abtb meta won't be sent to ftq, used for abtb fast train
  private val s2_abtbMeta = RegEnable(abtb.io.meta, s1_fire)
  private val s3_abtbMeta = RegEnable(s2_abtbMeta, s2_fire)

  // pTAGE's lookup information rides the pipeline down to s3, where the verified group trains the entry it came from
  private val s2_ptageMeta = RegEnable(ptage.io.meta, s1_fire)
  private val s3_ptageMeta = RegEnable(s2_ptageMeta, s2_fire)

  // The second block, carried down so the later stages know how wide the group is. Where it starts needs no pipeline
  // of its own: it is the first block's target, which already travels down as the s1 prediction.
  private val s1_secondBlockIn = Wire(Valid(new Prediction))
  private val s2_secondBlock   = RegEnable(s1_secondBlockIn, s1_fire)
  // An override at s2 replaces the group with a single corrected block, so the second block s1 proposed dies with the
  // rest of that group and must not reach s3 still looking valid.
  private val s2_secondBlockOut = WireInit(s2_secondBlock)
  when(s2_override)(s2_secondBlockOut.valid := false.B)
  private val s3_secondBlock = RegEnable(s2_secondBlockOut, s2_fire)

  private val s1_utageMeta     = Wire(new MicroTageMeta)
  private val s2_utageMeta     = RegEnable(s1_utageMeta, s1_fire)
  private val s2_realUtageMeta = Wire(new MicroTageMeta)
  private val s3_utageMeta     = RegEnable(s2_realUtageMeta, s2_fire)

  /* *** common inputs *** */
  private val stageCtrl = Wire(new StageCtrl)
  stageCtrl.s0_fire := s0_fire
  stageCtrl.s1_fire := s1_fire
  stageCtrl.s2_fire := s2_fire
  stageCtrl.s3_fire := s3_fire
  stageCtrl.t0_fire := io.fromFtq.train.fire

  private val train = Wire(new BpuTrain)
  train := io.fromFtq.train.bits

  private val fastTrain = Wire(Valid(new FastTrain))
  fastTrain.valid        := s3_valid
  fastTrain.bits.startPc := s3_startPc.get.unGuard
  fastTrain.bits.branch.fromPrediction(s3_prediction, s3_override)
  fastTrain.bits.abtbMeta  := s3_abtbMeta
  fastTrain.bits.utageMeta := s3_utageMeta
  fastTrain.bits.ptageMeta := s3_ptageMeta
  // fastTrain.bits.hasSecondBlock is assigned with the second block verification below, since it depends on the
  // override decision

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

  io.fromFtq.train.ready := predictors.map(_.io.trainReady).reduce(_ && _) && block2.io.trainReady

  // Training is held back when a predictor's bank is busy serving a prediction read, and the duplicated arrays add a
  // second chance of that happening. Ftq throttles Bpu once training has been held back long enough, so this is the
  // one way the duplicate can cost prediction bandwidth rather than only area. Count who refused.
  XSPerfSeqAccumulate(
    "trainRefused",
    io.fromFtq.train.valid,
    Seq(
      ("byMainPredictors", !predictors.map(_.io.trainReady).reduce(_ && _)),
      ("byBlock2", !block2.io.trainReady),
      ("byBlock2Only", predictors.map(_.io.trainReady).reduce(_ && _) && !block2.io.trainReady)
    )
  )

  /* *** the second block's own lookup ***
   * The duplicated btb and tage are read at the second block's start pc, which is the first block's target as pTAGE
   * itself supplied it, before the return stack gets a say. That is deliberate: a block whose target comes from the
   * return stack cannot carry a successor at all, so the two never differ where a second block exists, and taking
   * pTAGE's own output keeps the address a short hop from its s1 registers rather than the end of the s1 select.
   *
   * They take the same training stream as the main copies, so they hold the same branches, and they read the path
   * history as it stands between the two blocks.
   */
  block2.io.stageCtrl              := stageCtrl
  block2.io.startPc                := ptage.io.prediction.blocks.head.bits.target
  block2.io.foldedPathHist         := phr.io.s1_midFoldedPhr
  block2.io.foldedPathHistForTrain := phr.io.trainFoldedPhr
  block2.io.train.fromBpuTrain(train)

  /* *** predictor specific inputs *** */
  // pTAGE reads its resident folded histories straight out of FastPhr; its own a0 stage is driven by the shared
  // startPc, which for an ahead-indexed predictor is the key of the group after the one entering the pipeline.
  // Nothing selects its prediction yet, so it only observes and reports how well it would have done.
  ptage.io.foldedHist := fastPhr.io.foldedHist

  abtb.io.normalPathHist := phr.io.oldFoldedPhr
  abtb.io.debug_bpuS2StartPc.foreach(_ := s2_startPc.get)
  abtb.io.debug_bpuS3StartPc.foreach(_ := s3_startPc.get)

  utage.io.fromAheadBtb     := abtb.io.toMicroTage
  utage.io.normalPathHist   := phr.io.oldFoldedPhr
  utage.io.s1PathHist       := phr.io.s1_foldedPhr
  utage.io.overridePathHist := Mux(s3_override, phr.io.s3_foldedPhr, phr.io.s2_foldedPhr)

  uras.io.specIn.startPc                  := s1_startPc.get
  uras.io.specIn.cfiPosition              := s1_prediction.cfiPosition
  uras.io.specIn.attribute                := s1_prediction.attribute
  uras.io.hasRedirect                     := redirect.valid
  uras.io.s2OverrideData.valid            := s2_override
  uras.io.s2OverrideData.bits.startPc     := s2_startPc.get
  uras.io.s2OverrideData.bits.attribute   := s2_prediction.attribute
  uras.io.s2OverrideData.bits.cfiPosition := s2_prediction.cfiPosition
  uras.io.s3OverrideData.valid            := s3_override
  uras.io.s3OverrideData.bits.startPc     := s3_startPc.get.toUInt
  uras.io.s3OverrideData.bits.attribute   := s3_prediction.attribute
  uras.io.s3OverrideData.bits.cfiPosition := s3_prediction.cfiPosition
  uras.io.fullRetAddr                     := ras.io.topRetAddr

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
  s1_flush := s2_flush || s2_override

  s1_ready := s1_fire || !s1_valid || s1_flush
  s2_ready := s2_fire || !s2_valid
  s3_ready := s3_fire || !s3_valid

  private val sramResetDone = RegInit(false.B)
  when(predictors.map(_.io.sramResetDone).reduce(_ && _) && block2.io.sramResetDone) {
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
  s0_stall := !(s1_valid || s2_override || s3_override || redirect.valid)

  private val s1_ubtbPrediction = Wire(new Prediction)
  private val s1_abtbPrediction = Wire(Vec(NumAheadBtbPredictionEntries, new Prediction))
  s1_ubtbPrediction := ubtb.io.prediction.bits
  s1_ubtbPrediction.target := Mux(
    ubtb.io.prediction.bits.attribute.isReturn && uras.io.specOut.isCanUse,
    uras.io.specOut.retTarget,
    ubtb.io.prediction.bits.target
  )
  for (i <- 0 until NumAheadBtbPredictionEntries) {
    s1_abtbPrediction(i) := abtb.io.result.entries(i).bits
  }

  private val s1_abtbPosition   = VecInit(s1_abtbPrediction.map(_.cfiPosition))
  private val s1_utageHitMask   = utage.io.prediction.hitVec
  private val s1_utageTakenMask = utage.io.prediction.takenVec

  private val s1_abtbTakenMask = VecInit(abtb.io.result.entries.zipWithIndex.map { case (pred, i) =>
    val isJump = pred.bits.attribute.isDirect || pred.bits.attribute.isIndirect
    val isCond = pred.bits.attribute.isConditional
    XSPerfAccumulate(
      s"abtb_attribute_mismatch_takenCtr${i}",
      pred.valid && isJump && !pred.bits.taken
    )
    XSPerfAccumulate(
      s"microTage_false_hit_way${i}",
      pred.valid && !isCond && s1_utageHitMask(i)
    )
    pred.valid && (
      isJump || (isCond && Mux(s1_utageHitMask(i), s1_utageTakenMask(i), pred.bits.taken))
    )
  })

  private val s1_compareMatrix      = CompareMatrix(s1_abtbPosition)
  private val s1_abtbFirstTakenBrOH = s1_compareMatrix.getLeastElementOH(s1_abtbTakenMask)
  private val s1_abtbFirstTakenBr   = Mux1H(s1_abtbFirstTakenBrOH, s1_abtbPrediction)
  private val s1_abtbValid          = abtb.io.result.entries.map(_.valid).reduce(_ || _)

  private val s1_abtbResult = Wire(new Prediction)
  s1_abtbResult       := s1_abtbFirstTakenBr
  s1_abtbResult.taken := s1_abtbTakenMask.reduce(_ || _)
  s1_abtbResult.target := Mux(
    s1_abtbFirstTakenBr.attribute.isReturn && uras.io.specOut.isCanUse,
    uras.io.specOut.retTarget,
    s1_abtbFirstTakenBr.target
  )
  // pTAGE answers with a whole group where it has one, so it leads; anything it does not know falls to the small btb
  // and then to running the block out. A return takes its target from the return stack whichever source found it.
  //
  // The ahead btb and the micro tage still look up and still train, but nothing here reads their answer. They are
  // kept so the two ahead predictors can be compared, and putting either back is a change to this MuxCase alone.
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

  // A group ends where its last valid block ends, so that is where the next one starts. Feeding back the first
  // block's target instead would restart at a pc this group already covered, and the entry Ftq wrote for the second
  // block would not be followed by one starting at that block's target, which is how Ftq encodes a target at all.
  private val s1_groupTarget = Mux(s1_group(1).valid, s1_group(1).bits.target, s1_prediction.target)

  // Where the ahead-indexed predictors pick up. They are keyed on the start of the group that follows this one, so
  // they have to be told where the whole group ends, not where its first block jumped: a group that kept a second
  // block covers past that target itself.
  Seq(abtb.io, utage.io, ptage.io).foreach { predictorIo =>
    predictorIo.redirect        := redirect.valid
    predictorIo.bpuS2Override   := s2_override
    predictorIo.bpuS3Override   := s3_override
    predictorIo.newStartPc      := s1_groupTarget
    predictorIo.overrideStartPc := Mux(s3_override, s3_prediction.target, s2_prediction.target)
  }

  private val s1_taken         = s1_prediction.taken
  private val debug_s1UsePtage = s1_taken && usePtage
  private val debug_s1UseUbtb  = s1_taken && !usePtage

  // What the ahead btb would have answered, so the two ahead predictors can be compared without either driving the
  // other's result. useAbtb is the condition that used to select it here.
  private val useAbtb = s1_abtbValid && s1_abtbResult.taken
  XSPerfAccumulate("abtbWouldTake", useAbtb)
  XSPerfAccumulate("abtbAgreesWithPtage", useAbtb && usePtage && s1_abtbResult.asUInt === s1_ptageResult.asUInt)
  XSPerfAccumulate("abtbTakesWherePtageMisses", useAbtb && !usePtage)
  XSPerfAccumulate("ptageTakesWhereAbtbMisses", usePtage && !useAbtb)

  s1_utageMeta := utage.io.meta.bits

  private val s2_s1Prediction = RegEnable(s1_prediction, s1_fire)

  s2_realUtageMeta := s2_utageMeta
  s2_realUtageMeta.abtbResult.zipWithIndex.foreach { case (result, idx) =>
    result.valid :=
      s2_utageMeta.abtbResult(idx).valid && (s2_utageMeta.abtbResult(idx).cfiPosition <= s2_s1Prediction.cfiPosition)
  }

  private val s2_compareMatrix = RegEnable(CompareMatrix(mbtb.io.s1_positions), s1_fire)

  private val s2_isJumpVec = VecInit(mbtb.io.result.map {
    entry => entry.valid && (entry.bits.attribute.isDirect || entry.bits.attribute.isIndirect)
  })
  private val s2_isCondVec = VecInit(mbtb.io.result.map {
    entry => entry.valid && entry.bits.attribute.isConditional
  })

  private val s2_takenMask = VecInit(mbtb.io.result.zipWithIndex.map { case (entry, i) =>
    val tagePredValid = tage.io.prediction.fastTakenVec(i).valid
    val tagePred      = tage.io.prediction.fastTakenVec(i).bits
    s2_isJumpVec(i) || (s2_isCondVec(i) && Mux(tagePredValid, tagePred, entry.bits.taken))
  })
  private val s2_taken              = s2_takenMask.reduce(_ || _)
  private val s2_firstTakenBranchOH = s2_compareMatrix.getLeastElementOH(s2_takenMask)
  private val s2_firstTakenBranch   = Mux1H(s2_firstTakenBranchOH, mbtb.io.result)

  private val s2_fallThroughPrediction = RegEnable(fallThrough.io.prediction, s1_fire)

  s2_prediction       := Mux(s2_taken, s2_firstTakenBranch.bits, s2_fallThroughPrediction)
  s2_prediction.taken := s2_taken

  private val s2_mbtbCfiPositionDiffVec =
    VecInit(mbtb.io.result.map(_.bits.cfiPosition =/= s2_s1Prediction.cfiPosition))
  private val s2_mbtbAttributeDiffVec = VecInit(mbtb.io.result.map(_.bits.attribute =/= s2_s1Prediction.attribute))

  s2_override := {
    val takenDiff       = s2_taken =/= s2_s1Prediction.taken
    val cfiPositionDiff = s2_taken && Mux1H(s2_firstTakenBranchOH, s2_mbtbCfiPositionDiffVec)
    val attributeDiff   = s2_taken && Mux1H(s2_firstTakenBranchOH, s2_mbtbAttributeDiffVec)

    s2_valid && (takenDiff || cfiPositionDiff || attributeDiff)
  }

  /* *** s3 prediction selection *** */
  private val s3_mbtbResult     = RegEnable(mbtb.io.result, s2_fire)
  private val s3_tagePrediction = RegEnable(tage.io.prediction, s2_fire)
  private val s3_scUsed         = RegEnable(sc.io.scUsed, s2_fire)
  private val s3_scTakenMask    = RegEnable(sc.io.scTakenMask, s2_fire)
  private val s3_compareMatrix  = RegEnable(s2_compareMatrix, s2_fire)
  private val s3_s1Prediction   = RegEnable(s2_s1Prediction, s2_fire)
  private val s3_s2Override     = RegEnable(s2_override, s2_fire)
  private val s3_s2Prediction   = RegEnable(s2_prediction, s2_fire)
  private val s3_isJumpVec      = RegEnable(s2_isJumpVec, s2_fire)
  private val s3_isCondVec      = RegEnable(s2_isCondVec, s2_fire)

  private val s3_s2FirstTakenBranchOH = RegEnable(s2_firstTakenBranchOH, s2_fire)

  // S2 does not compare targets. An S2 override already uses the selected MBTB target, so a later MBTB target
  // comparison is redundant; a changed MBTB branch is caught by firstTakenBranchDiff. Only RAS/ITTAGE need target
  // comparison after an S2 override.
  private val s3_mbtbTargetDiffVec = VecInit(s3_mbtbResult.map { entry =>
    Mux(
      s3_s2Override,
      false.B,
      entry.bits.targetLower =/= s3_s1Prediction.targetLower
    )
  })
  private val s3_ittageTargetDiff = Mux(
    s3_s2Override,
    ittage.io.prediction.target =/= s3_s2Prediction.target,
    ittage.io.prediction.target =/= s3_s1Prediction.target
  )
  private val s3_rasTargetDiff = Mux(
    s3_s2Override,
    ras.io.topRetAddr =/= s3_s2Prediction.target,
    ras.io.topRetAddr =/= s3_s1Prediction.target
  )

  private val s3_takenMask = VecInit(s3_mbtbResult.zipWithIndex.map { case (entry, i) =>
    val useTage   = s3_tagePrediction.takenVec(i).valid
    val tageTaken = s3_tagePrediction.takenVec(i).bits
    val useSc     = s3_scUsed(i)
    val scTaken   = s3_scTakenMask(i)

    s3_isJumpVec(i) ||
    (s3_isCondVec(i) &&
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

  private val s3_fallThroughPrediction = RegEnable(s2_fallThroughPrediction, s2_fire)

  // used for mainBTB replacer
  mbtb.io.s3_takenMask := s3_takenMask

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

  private val s3_targetDiff =
    MuxCase(
      false.B,
      Seq(
        (s3_taken && s3_useRas)    -> s3_rasTargetDiff,
        (s3_taken && s3_useIttage) -> s3_ittageTargetDiff,
        s3_taken                   -> Mux1H(s3_firstTakenBranchOH, s3_mbtbTargetDiffVec)
      )
    )

  /* *** second block verification ***
   * The s2 and s3 predictors look up only where the group starts, so they say nothing about a second block. Two
   * independent sources do, and a block is kept when either confirms it.
   *
   * The duplicated btb and tage give the second block a lookup of the same kind and quality the first one gets: the
   * same arrays, trained on the same stream, read at the second block's own pc with the path history as it stands
   * between the blocks. That is the authority, and the reason it exists.
   *
   * The duplicate alone decides. The micro btb still answers alongside it, but only so the two can be compared: a
   * lookup it confirms and the duplicate does not is a block the duplicate is missing, and the counters below say
   * how often that happens. Letting it override the duplicate's refusal was measured as a loss -- it keeps blocks
   * whose branches the s3 predictors never trained on, and those cost more in mispredicts than the extra width earns.
   *
   * A block the duplicate cannot account for is dropped. An entry is positive evidence of where a block leaves; its
   * absence says only that nothing is on record, and keeping a block on that basis would let one through unchecked.
   * Dropping costs a block of width now and gets it back later: the next lookup meets that block as a first block,
   * verifies it the ordinary way, and fills both arrays in passing.
   */
  ubtb.io.verifyStartPc := s3_s1Prediction.target

  private val s3_ubtbConfirms =
    ubtb.io.verify.valid &&
      s3_secondBlock.bits.taken &&
      ubtb.io.verify.bits.cfiPosition === s3_secondBlock.bits.cfiPosition &&
      !(ubtb.io.verify.bits.attribute =/= s3_secondBlock.bits.attribute) &&
      ubtb.io.verify.bits.target === s3_secondBlock.bits.target

  private val s3_block2      = block2.io.prediction
  private val s3_dupConfirms = s3_block2.confirms(s3_secondBlock.bits)

  private val s3_secondBlockUnverified =
    s3_secondBlock.valid && !s3_dupConfirms

  // The duplicated lookup is only meaningful if it is answering about the block being checked. Its address travels
  // down its own shifted pipeline, so a mismatch here means the two pipelines have come apart.
  s3_block2.debug_startPc.foreach { pc =>
    XSError(
      s3_valid && s3_secondBlock.valid && pc =/= s3_s1Prediction.target,
      "the duplicated block 2 lookup does not belong to the block being verified\n"
    )
  }

  private val s3_checking = s3_valid && s3_secondBlock.valid
  XSPerfAccumulate("s3SecondBlockChecked", s3_checking)
  XSPerfAccumulate("s3SecondBlockKept", s3_checking && !s3_secondBlockUnverified)
  XSPerfAccumulate("s3SecondBlockDropped", s3_checking && s3_secondBlockUnverified)
  // What each source would have contributed on its own, so the duplicate can be judged against the array it replaced.
  // OnlyUbtbConfirms is the duplicate's blind spot: blocks the small recent-history array still catches.
  XSPerfAccumulate("s3SecondBlockBothConfirm", s3_checking && s3_dupConfirms && s3_ubtbConfirms)
  XSPerfAccumulate("s3SecondBlockOnlyDupConfirms", s3_checking && s3_dupConfirms && !s3_ubtbConfirms)
  XSPerfAccumulate("s3SecondBlockOnlyUbtbConfirms", s3_checking && !s3_dupConfirms && s3_ubtbConfirms)
  // Why the duplicate withheld confirmation, which is what says whether growing it would help.
  XSPerfAccumulate("s3SecondBlockDupNoEntry", s3_checking && !s3_block2.hasEntry)
  XSPerfAccumulate("s3SecondBlockDupNotTaken", s3_checking && s3_block2.hasEntry && !s3_block2.taken)
  XSPerfAccumulate(
    "s3SecondBlockDupDisagrees",
    s3_checking && s3_block2.hasEntry && s3_block2.taken && !s3_dupConfirms
  )
  XSPerfAccumulate("s3SecondBlockDupDecidedByTage", s3_checking && s3_block2.debug_tageDecided)
  XSPerfAccumulate("s3SecondBlockUbtbNoEntry", s3_checking && !ubtb.io.verify.valid)
  // a not-taken second block can never be confirmed by the micro btb, since only a taken exit leaves an entry behind
  XSPerfAccumulate("s3SecondBlockNotTaken", s3_checking && !s3_secondBlock.bits.taken)

  private val s3_firstBlockWrong = {
    val takenDiff            = s3_taken =/= s3_s2Prediction.taken
    val firstTakenBranchDiff = !(s3_firstTakenBranchOH === s3_s2FirstTakenBranchOH)

    s3_valid && (takenDiff || firstTakenBranchDiff || s3_targetDiff)
  }

  // The first block was checked and it was right, so only what followed it has to be taken back. Its Ftq entry stands
  // and the fetch already in flight for it survives; the correction starts at the second block's entry. What the
  // group means afterwards is the same either way, one block ending where the first block ends, so the path history
  // and the restart pc follow the ordinary override path unchanged.
  private val s3_dropSecondOnly = s3_valid && s3_secondBlockUnverified && !s3_firstBlockWrong

  s3_override := s3_firstBlockWrong || s3_dropSecondOnly

  // A second block that failed verification is dropped along with the group, so the entry that proposed it has not
  // been vindicated and pTAGE must not reinforce the pair.
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
  // s3_resolveMeta.debug_utage.foreach(_ := s3_utageMeta)
  s3_resolveMeta.utage := s3_utageMeta

  private val s3_commitMeta = Wire(new BpuCommitMeta)
  s3_commitMeta.ras := ras.io.commitMeta

  println(s"bpu redirect meta width: ${s3_redirectMeta.getWidth}")
  println(s"bpu resolve meta width: ${s3_resolveMeta.getWidth}")
  println(s"bpu commit meta width: ${s3_commitMeta.getWidth}")

  /* *** bpu to ftq io *** */
  io.toFtq.prediction.valid := s1_valid && s2_ready || s2_override || s3_override

  private val firstBlock = io.toFtq.prediction.bits.blocks.head
  // An override that only takes back the second block writes no entry at all: the first block's is already right
  firstBlock.valid := !s3_dropSecondOnly
  when(s3_override) {
    firstBlock.bits.fromStage(s3_startPc.get, s3_prediction)
  }.elsewhen(s2_override) {
    firstBlock.bits.fromStage(s2_startPc.get, s2_prediction)
  }.otherwise {
    firstBlock.bits.fromStage(s1_startPc.get, s1_prediction)
  }
  // The second block starts where the first jumped to, which is what lets Ftq keep reading a block's target off its
  // successor. An override replaces the group with a single corrected block, so none follows it.
  private val secondBlock = io.toFtq.prediction.bits.blocks(1)
  secondBlock.valid := s1_group(1).valid && !s2_override && !s3_override
  secondBlock.bits.fromStage(s1_prediction.target, s1_group(1).bits)
  // Ftq holds back only the space this group needs, so tell it whether a second block is coming. Read from the group
  // rather than from the outgoing block, which an override has already collapsed.
  io.toFtq.predictionIsGroup          := s1_group(1).valid
  io.toFtq.prediction.bits.s2Override := s2_override
  io.toFtq.prediction.bits.s3Override := s3_override

  // used for meta enqueue and override
  private val s2_ftqPtr = RegEnable(io.fromFtq.bpuPtr, s1_fire)
  private val s3_ftqPtr = RegEnable(s2_ftqPtr, s2_fire)
  io.toFtq.s2FtqPtr := s2_ftqPtr
  io.toFtq.s3FtqPtr := s3_ftqPtr
  // An override replaces the group with a single corrected block, so only a group that survives s3 keeps its width.
  // A group is its first block plus, where there was one, its second.
  io.toFtq.s3NumBlocks          := Mux(s3_override, 1.U, 1.U +& s3_secondBlock.valid.asUInt)
  io.toFtq.s3OverrideKeptBlocks := Mux(s3_dropSecondOnly, 1.U, 0.U)

  io.toFtq.meta.valid             := s3_valid
  io.toFtq.meta.bits.redirectMeta := s3_redirectMeta
  io.toFtq.meta.bits.resolveMeta  := s3_resolveMeta
  io.toFtq.meta.bits.commitMeta   := s3_commitMeta

  /* *** s0_startPc selection *** */
  s0_startPc := MuxCase(
    s0_startPcReg.get,
    Seq(
      redirect.valid -> redirect.bits.target,
      s3_override    -> s3_prediction.target,
      s2_override    -> s2_prediction.target,
      s1_valid       -> s1_groupTarget
    )
  )

  // Ftq reads a block's target off its successor's startPc, so consecutive groups have to abut exactly. A redirect or
  // an override restarts the stream somewhere else, so the check resumes only once a group has been issued since.
  private val debug_lastGroupTarget = RegEnable(s1_groupTarget, s1_fire)
  private val debug_streamAbuts     = RegInit(false.B)
  when(s1_fire)(debug_streamAbuts                                      := true.B)
  when(redirect.valid || s2_override || s3_override)(debug_streamAbuts := false.B)
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

  private val s1_abtbPredWithURas = WireInit(abtb.io.result.entries)
  s1_abtbPredWithURas.foreach {
    case p => when(p.valid && p.bits.attribute.isReturn && uras.io.specOut.isCanUse) {
        p.bits.target := uras.io.specOut.retTarget
      }
  }

  phr.io.train.s0_stall         := s0_stall
  phr.io.train.stageCtrl        := stageCtrl
  phr.io.train.redirect         := redirect
  phr.io.train.s2.overrideValid := s2_override
  phr.io.train.s2.phrMeta       := s2_phrMeta
  phr.io.train.s2.prediction    := s2_prediction
  phr.io.train.s2.startPc       := s2_startPc.get.unGuard
  phr.io.train.s3.overrideValid := s3_override
  phr.io.train.s3.phrMeta       := s3_phrMeta
  phr.io.train.s3.prediction    := s3_prediction
  phr.io.train.s3.startPc       := s3_startPc.get.unGuard
  phr.io.s1Train.valid          := s1_fire
  phr.io.s1Train.startPc        := s1_startPc.get.unGuard
  phr.io.s1Train.blocks         := s1_group

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
   * A short window of the same path history Phr maintains, giving the ahead predictors resident folded histories. It
   * advances on exactly the events that move Phr, with the same tokens, so the two never diverge. Only taken blocks
   * shift the path history, so a group of not-taken blocks advances neither.
   */
  private def numBlocksOH(numTaken: UInt): Vec[Bool] =
    VecInit(Seq.tabulate(MaxPredictionNum + 1)(n => numTaken === n.U))

  fastPhr.io.valid       := s1_fire
  fastPhr.io.token       := phr.io.toFastPhr.s1Token
  fastPhr.io.numBlocksOH := numBlocksOH(phr.io.toFastPhr.s1NumTaken)
  fastPhr.io.s2Fire      := s2_fire

  fastPhr.io.redirect.valid := redirect.valid
  fastPhr.io.redirect.phr   := phr.io.toFastPhr.redirectPhr

  // an override replaces the group with one corrected block, so the replay carries a single block's hash
  fastPhr.io.overrideValid  := s2_override || s3_override
  fastPhr.io.overrideFromS3 := s3_override
  fastPhr.io.overrideToken :=
    Mux(s3_override, phr.io.toFastPhr.s3PathHash, phr.io.toFastPhr.s2PathHash)
  fastPhr.io.overrideNumBlocksOH :=
    numBlocksOH(Mux(s3_override, s3_prediction.taken, s2_prediction.taken).asUInt)

  // FastPhr caches what Phr already holds, so the two must agree bit for bit. Checking the window directly catches a
  // divergence at its source, rather than waiting for it to surface as a mispredict through the folded histories.
  private val fastPhrDiverged = phr.io.toFastPhr.debug_phr(WindowLength - 1, 0) =/= fastPhr.io.debug_phr
  XSPerfAccumulate("fastPhrDivergedCycles", fastPhrDiverged)
  XSError(fastPhrDiverged, "FastPhr window diverged from Phr\n")

  // ghr update
  private val s1_cfiPc = getCfiPcFromPosition(s1_startPc.get, s1_prediction.cfiPosition)
  private val s1_imliTaken =
    s1_prediction.taken && s1_prediction.attribute.isConditional &&
      (s1_cfiPc.addr(CompareAddrLowWidth - 1, 0) > s1_prediction.target.addr(CompareAddrLowWidth - 1, 0))

  commonHR.io.stageCtrl                   := stageCtrl
  commonHR.io.s0_startPc.get              := s0_startPc.get.unGuard
  commonHR.io.s1_imliTaken                := s1_imliTaken
  commonHR.io.s2Update.isOverride         := s2_override
  commonHR.io.s2Update.startPc            := s2_startPc.get.unGuard
  commonHR.io.s2Update.prediction         := s2_prediction
  commonHR.io.s2Update.condHitMask        := s2_isCondVec
  commonHR.io.s2Update.positions          := VecInit(mbtb.io.result.map(_.bits.cfiPosition))
  commonHR.io.s2Update.targets            := VecInit(mbtb.io.result.map(_.bits.target.unGuard))
  commonHR.io.s3Update.isOverride         := s3_override
  commonHR.io.s3Update.startPc            := s3_startPc.get.unGuard
  commonHR.io.s3Update.prediction         := s3_prediction
  commonHR.io.s3Update.firstTakenBranchOH := s3_firstTakenBranchOH
  commonHR.io.redirect.valid              := redirect.valid
  commonHR.io.redirect.cfiPc              := redirect.bits.cfiPc
  commonHR.io.redirect.target             := redirect.bits.target.unGuard
  commonHR.io.redirect.taken              := redirect.bits.taken
  commonHR.io.redirect.attribute          := redirect.bits.attribute
  commonHR.io.redirect.meta               := redirect.bits.meta.commonHRMeta

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

  /* *** check abtb output *** */
  abtb.io.result.debug_startPc.foreach { debug_startPc =>
    when(io.toFtq.prediction.fire && !s1_flush && abtb.io.result.entries.map(_.valid).reduce(_ || _)) {
      assert(debug_startPc === s1_startPc.head)
    }
  }

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
  s3_perfMeta.s2Prediction        := s3_s2Prediction
  s3_perfMeta.s3Prediction        := s3_prediction
  s3_perfMeta.bpSource.s1Source   := s3_s1PredictionSource
  s3_perfMeta.bpSource.s3Source   := s3_predictionSource
  s3_perfMeta.bpSource.s2Override := s3_s2Override
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
  XSPerfAccumulate("s2Override", io.toFtq.prediction.fire && io.toFtq.prediction.bits.s2Override)
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
    io.toFtq.prediction.fire && !s2_override && !s3_override,
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
    s3_fire && !s3_s2Override && !s3_override,
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
      s3_prediction.taken && s3_s2Prediction.taken &&
      s3_prediction.cfiPosition =/= s3_s2Prediction.cfiPosition,
    perf_s1TakenSourceVec
  )

  // attribute mismatch
  XSPerfSeqAccumulate(
    s"s3Override_attributeMismatch",
    io.toFtq.prediction.fire && s3_override &&
      s3_prediction.taken && s3_s2Prediction.taken &&
//      s3_prediction.cfiPosition === s3_s2Prediction.cfiPosition &&
      !(s3_prediction.attribute === s3_s2Prediction.attribute),
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
      s3_prediction.taken && s3_s2Prediction.taken &&
      s3_prediction.cfiPosition === s3_s2Prediction.cfiPosition &&
      s3_targetDiff,
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
