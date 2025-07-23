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
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.PrunedAddrInit
import xiangshan.frontend.bpu.abtb.AheadBtb
import xiangshan.frontend.bpu.mbtb.MainBtb
import xiangshan.frontend.bpu.phr.Phr
import xiangshan.frontend.bpu.phr.PhrAllFoldedHistories
import xiangshan.frontend.bpu.phr.PhrPtr
import xiangshan.frontend.bpu.tage.Tage
import xiangshan.frontend.bpu.ubtb.MicroBtb
import xiangshan.frontend.ftq.FtqToBpuIO

class DummyBpu(implicit p: Parameters) extends BpuModule with HalfAlignHelper {
  class DummyBpuIO extends Bundle {
    val ctrl:        BPUCtrl    = Input(new BPUCtrl)
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
  private val phr         = Module(new Phr)

  private def predictors: Seq[BasePredictor] = Seq(
    fallThrough,
    ubtb,
    abtb,
    mbtb,
    tage
  )

  /* *** aliases *** */
  private val train    = io.fromFtq.update
  private val redirect = io.fromFtq.redirect

  /* *** CSR ctrl sub-predictor enable *** */
  private val ctrl = DelayN(io.ctrl, 2) // delay 2 cycle for timing
  fallThrough.io.enable := true.B // fallThrough is always enabled
  ubtb.io.enable        := ctrl.ubtb_enable
  abtb.io.enable        := true.B // FIXME
  mbtb.io.enable        := true.B
  tage.io.enable        := true.B

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

  private val resetDone = RegInit(false.B)

  private val s0_pc    = WireDefault(0.U.asTypeOf(PrunedAddr(VAddrBits)))
  private val s0_pcReg = RegEnable(s0_pc, !s0_stall)

  when(RegNext(RegNext(reset.asBool)) && !reset.asBool) {
    s0_pcReg  := io.resetVector
    resetDone := true.B
  }

  private val s1_pc = RegEnable(s0_pc, s0_fire)
  private val s2_pc = RegEnable(s1_pc, s1_fire)
  private val s3_pc = RegEnable(s2_pc, s2_fire)

  /* *** common inputs *** */
  predictors.foreach { p =>
    // TODO: duplicate pc and fire to solve high fan-out issue
    p.io.startVAddr        := s0_pc
    p.io.stageCtrl.s0_fire := s0_fire
    p.io.stageCtrl.s1_fire := s1_fire
    p.io.stageCtrl.s2_fire := s2_fire
    p.io.stageCtrl.s3_fire := s3_fire
  }

  /* *** predictor specific inputs *** */
  // fall-through and ubtb currently doesn't have
  // abtb
  abtb.io.redirectValid := redirect.valid
  abtb.io.overrideValid := s3_override

  /* *** train *** */
  private val t0_valid      = train.valid
  private val t0_startVAddr = train.bits.pc
  private val t0_taken      = train.bits.ftqOffset.valid
  private val (t0_cfiPosition, t0_cfiPositionCarry) = getAlignedPosition(
    train.bits.pc,
    train.bits.ftqOffset.bits
  )
  assert(
    !(train.valid && train.bits.ftqOffset.valid && t0_cfiPositionCarry),
    "ftqOffset exceeds 2 * 32B aligned fetch block range, cfiPosition overflow!"
  )
  private val t0_target = train.bits.full_target
  private val t0_attribute = MuxCase(
    BranchAttribute.Conditional,
    Seq(
      (train.bits.is_call && train.bits.is_jal)  -> BranchAttribute.DirectCall,
      (train.bits.is_call && train.bits.is_jalr) -> BranchAttribute.IndirectCall,
      train.bits.is_ret                          -> BranchAttribute.Return,
      train.bits.is_jal                          -> BranchAttribute.OtherDirect,
      train.bits.is_jalr                         -> BranchAttribute.OtherIndirect
    )
  )

  // FIXME: should use s3_prediction to train ubtb
  // ubtb
  ubtb.io.train.valid            := t0_valid
  ubtb.io.train.bits.startVAddr  := t0_startVAddr
  ubtb.io.train.bits.taken       := t0_taken
  ubtb.io.train.bits.cfiPosition := t0_cfiPosition
  ubtb.io.train.bits.target      := t0_target
  ubtb.io.train.bits.attribute   := t0_attribute
  // mbtb
  mbtb.io.train.valid            := t0_valid
  mbtb.io.train.bits.startVAddr  := t0_startVAddr
  mbtb.io.train.bits.taken       := t0_taken
  mbtb.io.train.bits.cfiPosition := t0_cfiPosition
  mbtb.io.train.bits.target      := t0_target
  mbtb.io.train.bits.attribute   := t0_attribute
  mbtb.io.train.bits.meta        := train.bits.newMeta.mbtbMeta
  // abtb
  abtb.io.train.valid          := t0_valid
  abtb.io.train.bits.startPc   := t0_startVAddr
  abtb.io.train.bits.target    := t0_target
  abtb.io.train.bits.taken     := t0_taken
  abtb.io.train.bits.position  := t0_cfiPosition
  abtb.io.train.bits.attribute := t0_attribute
  abtb.io.train.bits.meta      := train.bits.newMeta.abtbMeta

  private val s2_ftqPtr = RegEnable(io.fromFtq.enq_ptr, s1_fire)
  private val s3_ftqPtr = RegEnable(s2_ftqPtr, s2_fire)

  s3_flush := redirect.valid
  s2_flush := s3_flush || s3_override
  s1_flush := s2_flush

  s1_ready := s1_fire || !s1_valid || s1_flush
  s2_ready := s2_fire || !s2_valid
  s3_ready := s3_fire || !s3_valid

  private val predictorsReady = true.B // FIXME

  s0_fire := s1_ready && predictorsReady && resetDone
  s1_fire := s1_valid && s2_ready && io.toFtq.resp.ready
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
  private val s1_prediction = Wire(new BranchPrediction)
  s1_prediction := MuxCase(
    fallThrough.io.prediction,
    Seq(
      ubtb.io.prediction.taken -> ubtb.io.prediction,
      abtb.io.prediction.taken -> abtb.io.prediction
    )
  )

  // s3 prediction: TODO
  private val s3_prediction = Wire(new BranchPrediction)
  s3_prediction := DontCare

  io.toFtq.resp.valid := s1_valid && s2_ready || s3_fire && s3_override

  when(s3_override) {
    io.toFtq.resp.bits.fromStage(s3_pc, s3_prediction)
  }.otherwise {
    io.toFtq.resp.bits.fromStage(s1_pc, s1_prediction)
  }

  // override
  io.toFtq.resp.bits.s3Override.valid       := s3_override
  io.toFtq.resp.bits.s3Override.bits.ftqPtr := s3_ftqPtr

  // abtb meta delay to s3
  private val s2_abtbMeta = RegEnable(abtb.io.meta, s1_fire)
  private val s3_abtbMeta = RegEnable(s2_abtbMeta, s2_fire)

  // mbtb meta
  val s3_mbtbMeta = RegEnable(mbtb.io.meta, s2_fire)

  private val predictorMeta = Wire(new NewPredictorMeta)
  predictorMeta.abtbMeta   := s3_abtbMeta
  predictorMeta.mbtbMeta   := s3_mbtbMeta
  predictorMeta.phrHistPtr := phr.io.phrPtr
  // TODO: other meta

  io.toFtq.meta.valid := s3_valid
  io.toFtq.meta.bits  := predictorMeta

  s0_pc := MuxCase(
    s0_pcReg,
    Seq(
      redirect.valid -> PrunedAddrInit(redirect.bits.cfiUpdate.target),
      s3_override    -> s3_prediction.target,
      s1_valid       -> s1_prediction.target
    )
  )

  // phr train
  private val phrsWire     = WireInit(0.U.asTypeOf(Vec(PhrHistoryLength, Bool())))
  private val s0_foldedPhr = WireInit(0.U.asTypeOf(new PhrAllFoldedHistories(TageFoldedGHistInfos)))
  private val s1_foldedPhr = WireInit(0.U.asTypeOf(new PhrAllFoldedHistories(TageFoldedGHistInfos)))
  private val s2_foldedPhr = WireInit(0.U.asTypeOf(new PhrAllFoldedHistories(TageFoldedGHistInfos)))
  private val s3_foldedPhr = WireInit(0.U.asTypeOf(new PhrAllFoldedHistories(TageFoldedGHistInfos)))
  phr.io.train.s0_stall          := s0_stall
  phr.io.train.stageCtrl.s0_fire := s0_fire
  phr.io.train.stageCtrl.s1_fire := s1_fire
  phr.io.train.stageCtrl.s2_fire := s2_fire
  phr.io.train.stageCtrl.s3_fire := s3_fire
  phr.io.train.redirectValid     := redirect.valid
  phr.io.train.redirectPc        := PrunedAddrInit(redirect.bits.cfiUpdate.pc)
  phr.io.train.redirectTaken     := redirect.bits.cfiUpdate.taken
  phr.io.train.redirectPhrPtr    := redirect.bits.cfiUpdate.phrHistPtr
  phr.io.train.s3_override       := s3_override
  phr.io.train.s3_pc             := s3_pc
  phr.io.train.s3_taken          := s3_prediction.taken
  phr.io.train.s1_valid          := s1_valid
  phr.io.train.s1_pc             := s1_pc
  phr.io.train.s1_taken          := s1_prediction.taken

  s0_foldedPhr := phr.io.s0_foldedPhr
  s1_foldedPhr := phr.io.s1_foldedPhr
  s2_foldedPhr := phr.io.s2_foldedPhr
  s3_foldedPhr := phr.io.s3_foldedPhr
  phrsWire     := phr.io.phrs

  private val phrsWireValue = phrsWire.asUInt
  private val redirectPhrValue =
    (Cat(phrsWire.asUInt, phrsWire.asUInt) >> (redirect.bits.cfiUpdate.phrHistPtr.value + 1.U))(
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
  XSPerfAccumulate("toFtqFire", io.toFtq.resp.fire)
  XSPerfAccumulate("s3Override", io.toFtq.resp.fire && io.toFtq.resp.bits.s3Override.valid)
  XSPerfHistogram(
    "fetchBlockSize",
    Mux(
      io.toFtq.resp.bits.ftqOffset.valid,
      io.toFtq.resp.bits.ftqOffset.bits,
      FetchBlockInstNum.U
    ),
    io.toFtq.resp.fire,
    0,
    FetchBlockInstNum
  )
  XSPerfAccumulate("s1_use_ubtb", io.toFtq.resp.fire && ubtb.io.prediction.taken)
  XSPerfAccumulate("s1_use_abtb", io.toFtq.resp.fire && !ubtb.io.prediction.taken && abtb.io.prediction.taken)
  XSPerfAccumulate("s1_use_fallThrough", io.toFtq.resp.fire && !ubtb.io.prediction.taken && !abtb.io.prediction.taken)

  XSPerfAccumulate("s1Invalid", !s1_valid)

  /* *** perf train *** */
  XSPerfAccumulate("train", io.fromFtq.update.valid)
}
