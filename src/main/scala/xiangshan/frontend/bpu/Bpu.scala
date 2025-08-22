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
import utility.DelayN
import utility.XSError
import utility.XSPerfAccumulate
import utility.XSPerfHistogram
import xiangshan.frontend.BpuToFtqIO
import xiangshan.frontend.FtqToBpuIO
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.abtb.AheadBtb
import xiangshan.frontend.bpu.mbtb.MainBtb
import xiangshan.frontend.bpu.phr.Phr
import xiangshan.frontend.bpu.phr.PhrAllFoldedHistories
import xiangshan.frontend.bpu.phr.PhrPtr
import xiangshan.frontend.bpu.ras.Ras
import xiangshan.frontend.bpu.ras.RasPtr
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
  private val ras         = Module(new Ras)
  private val phr         = Module(new Phr)

  private def predictors: Seq[BasePredictor] = Seq(
    fallThrough,
    ubtb,
    abtb,
    mbtb,
    tage,
    ras
  )

  /* *** aliases *** */
  private val train        = io.fromFtq.train
  private val commitUpdate = io.fromFtq.train
  private val redirect     = io.fromFtq.redirect

  /* *** CSR ctrl sub-predictor enable *** */
  private val ctrl = DelayN(io.ctrl, 2) // delay 2 cycle for timing
  fallThrough.io.enable := true.B // fallThrough is always enabled
  ubtb.io.enable        := ctrl.ubtbEnable
  abtb.io.enable        := ctrl.abtbEnable
  mbtb.io.enable        := ctrl.mbtbEnable
  tage.io.enable        := ctrl.tageEnable
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

  private val s1_valid = RegInit(false.B)
  private val s2_valid = RegInit(false.B)
  private val s3_valid = RegInit(false.B)

  private val s3_override = WireDefault(false.B)

  private val s0_pc    = WireDefault(0.U.asTypeOf(PrunedAddr(VAddrBits)))
  private val s0_pcReg = RegEnable(s0_pc, !s0_stall)

  when(RegNext(RegNext(reset.asBool)) && !reset.asBool) {
    s0_pcReg := io.resetVector
  }

  private val s1_pc = RegEnable(s0_pc, s0_fire)
  private val s2_pc = RegEnable(s1_pc, s1_fire)
  private val s3_pc = RegEnable(s2_pc, s2_fire)

  /* *** common inputs *** */
  private val stageCtrl = Wire(new StageCtrl)
  stageCtrl.s0_fire := s0_fire
  stageCtrl.s1_fire := s1_fire
  stageCtrl.s2_fire := s2_fire
  stageCtrl.s3_fire := s3_fire

  predictors.foreach { p =>
    // TODO: duplicate pc and fire to solve high fan-out issue
    p.io.startVAddr := s0_pc
    p.io.stageCtrl  := stageCtrl
    p.io.train      := train
  }

  /* *** predictor specific inputs *** */
  // FIXME: should use s3_prediction to train ubtb

  // abtb
  abtb.io.redirectValid := redirect.valid
  abtb.io.overrideValid := s3_override

  ras.io.redirect.valid          := redirect.valid
  ras.io.redirect.bits.attribute := redirect.bits.attribute
  ras.io.redirect.bits.brPc      := redirect.bits.startVAddr
  ras.io.redirect.bits.isRvc     := redirect.bits.isRvc
  ras.io.redirect.bits.meta      := redirect.bits.speculationMeta.rasMeta
  ras.io.redirect.bits.level     := 0.U(1.W)

  // ras
  ras.io.commit.valid            := commitUpdate.valid
  ras.io.commit.bits.attribute   := commitUpdate.bits.branches(0).bits.attribute
  ras.io.commit.bits.startPc     := commitUpdate.bits.startVAddr.toUInt
  ras.io.commit.bits.isRvc       := false.B // commitUpdate.bits.isRvc
  ras.io.commit.bits.meta        := commitUpdate.bits.meta.ras
  ras.io.commit.bits.cfiPosition := commitUpdate.bits.branches(0).bits.cfiPosition

  tage.io.mbtbResult := mbtb.io.result

  private val s2_ftqPtr = RegEnable(io.fromFtq.bpuPtr, s1_fire)
  private val s3_ftqPtr = RegEnable(s2_ftqPtr, s2_fire)

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

  // s1 prediction selection:
  // if ubtb or abtb find a taken branch, use the corresponding prediction
  // otherwise, use fall-through prediction
  private val s1_prediction = Wire(new Prediction)
  s1_prediction := MuxCase(
    fallThrough.io.prediction,
    Seq(
      ubtb.io.prediction.taken -> ubtb.io.prediction,
      abtb.io.prediction.taken -> abtb.io.prediction
    )
  )
  private val s2_prediction   = RegEnable(s1_prediction, s1_fire)
  private val s3_inPrediction = RegEnable(s2_prediction, s2_fire)
  // s3 prediction: TODO
  private val s3_prediction = Wire(new Prediction)
  s3_prediction                  := DontCare
  ras.io.specIn.valid            := s3_fire
  ras.io.specIn.bits.startPc     := s3_pc.toUInt
  ras.io.specIn.bits.isRvc       := false.B
  ras.io.specIn.bits.attribute   := s3_inPrediction.attribute
  ras.io.specIn.bits.cfiPosition := s3_inPrediction.cfiPosition
  ras.io.specIn.bits.target      := s3_inPrediction.target

  // to Ftq
  io.toFtq.prediction.valid := s1_valid && s2_ready || s3_fire && s3_override
  when(s3_override) {
    io.toFtq.prediction.bits.fromStage(s3_pc, s3_prediction)
  }.otherwise {
    io.toFtq.prediction.bits.fromStage(s1_pc, s1_prediction)
  }
  io.toFtq.prediction.bits.s3Override := s3_override

  // tell ftq s3 ftqptr for meta enqueue and s3 override
  io.toFtq.s3FtqPtr := s3_ftqPtr

  // abtb meta delay to s3
  private val s2_abtbMeta = RegEnable(abtb.io.meta, s1_fire)
  private val s3_abtbMeta = RegEnable(s2_abtbMeta, s2_fire)

  // mbtb meta
  private val s3_mbtbMeta = RegEnable(mbtb.io.meta, s2_fire)
  private val s3_rasMeta  = ras.io.specMeta

  // tage meta
  private val s3_tageMeta = tage.io.meta

  // phr meta
  val s2_phrMeta = RegEnable(phr.io.phrPtr, s1_fire)
  val s3_phrMeta = RegEnable(s2_phrMeta, s2_fire)

  private val s3_speculationMeta = Wire(new BpuSpeculationMeta)
  s3_speculationMeta.phrHistPtr := s3_phrMeta
  s3_speculationMeta.rasMeta    := s3_rasMeta
  s3_speculationMeta.topRetAddr := ras.io.topRetAddr

  private val s3_meta = Wire(new BpuMeta)
  s3_meta.abtb   := s3_abtbMeta
  s3_meta.mbtb   := s3_mbtbMeta
  s3_meta.ras    := s3_rasMeta
  s3_meta.phr    := s3_phrMeta
  s3_meta.tage   := s3_tageMeta
  s3_meta.ittage := DontCare // TODO: add ittage

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
  private val phrsWire     = WireInit(0.U.asTypeOf(Vec(PhrHistoryLength, Bool())))
  private val s0_foldedPhr = WireInit(0.U.asTypeOf(new PhrAllFoldedHistories(AllFoldedHistoryInfo)))
  private val s1_foldedPhr = WireInit(0.U.asTypeOf(new PhrAllFoldedHistories(AllFoldedHistoryInfo)))
  private val s2_foldedPhr = WireInit(0.U.asTypeOf(new PhrAllFoldedHistories(AllFoldedHistoryInfo)))
  private val s3_foldedPhr = WireInit(0.U.asTypeOf(new PhrAllFoldedHistories(AllFoldedHistoryInfo)))
  phr.io.train.s0_stall      := s0_stall
  phr.io.train.stageCtrl     := stageCtrl
  phr.io.train.redirect      := redirect
  phr.io.train.s3_override   := s3_override
  phr.io.train.s3_prediction := s3_prediction
  phr.io.train.s3_pc         := s3_pc
  phr.io.train.s1_valid      := s1_fire
  phr.io.train.s1_prediction := s1_prediction
  phr.io.train.s1_pc         := s1_pc

  phr.io.commit.valid := train.valid
  phr.io.commit.bits  := train.bits

  s0_foldedPhr := phr.io.s0_foldedPhr
  s1_foldedPhr := phr.io.s1_foldedPhr
  s2_foldedPhr := phr.io.s2_foldedPhr
  s3_foldedPhr := phr.io.s3_foldedPhr
  phrsWire     := phr.io.phrs

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
    assert(abtb.io.debug_startVaddr === s1_pc)
  }

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

  XSPerfAccumulate("s1Invalid", !s1_valid)

  /* *** perf train *** */
  XSPerfAccumulate("train", io.fromFtq.train.valid)
}
