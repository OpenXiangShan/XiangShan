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
import xiangshan.frontend.BpuToFtqIO
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.PrunedAddrInit
import xiangshan.frontend.bpu.abtb.AheadBtb
import xiangshan.frontend.bpu.ubtb.MicroBtb
import xiangshan.frontend.ftq.FtqToBpuIO

class DummyBpu(implicit p: Parameters) extends BpuModule {
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

  private def predictors: Seq[BasePredictor] = Seq(
    fallThrough,
    ubtb,
    abtb
  )

  /* *** CSR ctrl sub-predictor enable *** */
  private val ctrl = DelayN(io.ctrl, 2) // delay 2 cycle for timing
  fallThrough.io.enable := true.B // fallThrough is always enabled
  ubtb.io.enable        := ctrl.ubtb_enable
  abtb.io.enable        := true.B // FIXME

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

  private val redirect = io.fromFtq.redirect

  // connect common inputs
  predictors.foreach { p =>
    // TODO: duplicate pc and fire to solve high fan-out issue
    p.io.startVAddr        := s0_pc
    p.io.stageCtrl.s0_fire := s0_fire
    p.io.stageCtrl.s1_fire := s1_fire
    p.io.stageCtrl.s2_fire := s2_fire
    p.io.stageCtrl.s3_fire := s3_fire
  }

  // ubtb specific inputs
  // FIXME: should use s3_prediction to train ubtb
  ubtb.io.train.valid                  := io.fromFtq.update.valid
  ubtb.io.train.bits.startVAddr        := io.fromFtq.update.bits.pc
  ubtb.io.train.bits.cfiPosition.valid := io.fromFtq.update.bits.cfi_idx.valid
  ubtb.io.train.bits.cfiPosition.bits  := io.fromFtq.update.bits.cfi_idx.bits
  ubtb.io.train.bits.target            := io.fromFtq.update.bits.full_target
  ubtb.io.train.bits.attribute := MuxCase(
    BranchAttribute.Conditional,
    Seq(
      (io.fromFtq.update.bits.is_call && io.fromFtq.update.bits.is_jal)  -> BranchAttribute.DirectCall,
      (io.fromFtq.update.bits.is_call && io.fromFtq.update.bits.is_jalr) -> BranchAttribute.IndirectCall,
      io.fromFtq.update.bits.is_ret                                      -> BranchAttribute.Return,
      io.fromFtq.update.bits.is_jal                                      -> BranchAttribute.OtherDirect,
      io.fromFtq.update.bits.is_jalr                                     -> BranchAttribute.OtherIndirect
    )
  )

  abtb.io.redirectValid := redirect.valid
  abtb.io.overrideValid := s3_override
  abtb.io.update        := io.fromFtq.newUpdate

  dontTouch(abtb.io.hit)
  dontTouch(abtb.io.prediction)

  when(abtb.io.prediction.taken) {
    assert(abtb.io.debug_startVaddr === s1_pc)
  }

  private val s2_ftqPtr = RegEnable(io.fromFtq.enq_ptr, s1_fire)
  private val s3_ftqPtr = RegEnable(s2_ftqPtr, s2_fire)

  s3_flush := redirect.valid
  s2_flush := s3_flush || s3_override
  s1_flush := s2_flush || s2_override

  s1_ready := s1_fire || !s1_valid
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
  s0_stall := !(s1_valid || s2_override || s3_override || redirect.valid)

  // s1 prediction:
  // if ubtb hits, use meta (i.e. cfiPosition, attribute) from ubtb
  // otherwise, use fallThrough
  private val s1_prediction = Wire(new BranchPrediction)
  s1_prediction := Mux(
    ubtb.io.hit,
    ubtb.io.prediction,
    fallThrough.io.prediction
  )
  // and, if ubtb predicts a taken branch, use target from ubtb
  // otherwise, use fallThrough
  s1_prediction.target := Mux(
    ubtb.io.hit && ubtb.io.prediction.taken,
    ubtb.io.prediction.target,
    fallThrough.io.prediction.target
  )

  // s2 prediction: TODO
  private val s2_prediction = Wire(new BranchPrediction)
  s2_prediction := DontCare

  // s3 prediction: TODO
  private val s3_prediction = Wire(new BranchPrediction)
  s3_prediction := DontCare

  io.toFtq.resp.valid := s1_valid && s2_ready || s2_fire && s2_override || s3_fire && s3_override

  when(s3_override) {
    io.toFtq.resp.bits.fromStage(s3_pc, s3_prediction)
  }.elsewhen(s2_override) {
    io.toFtq.resp.bits.fromStage(s2_pc, s2_prediction)
  }.otherwise {
    io.toFtq.resp.bits.fromStage(s1_pc, s1_prediction)
  }

  // override
  io.toFtq.resp.bits.s2Override.valid       := s2_override
  io.toFtq.resp.bits.s2Override.bits.ftqPtr := s2_ftqPtr
  io.toFtq.resp.bits.s3Override.valid       := s3_override
  io.toFtq.resp.bits.s3Override.bits.ftqPtr := s3_ftqPtr

  // abtb meta delay to s3
  val s2_abtbMeta = RegEnable(abtb.io.meta, s1_fire)
  val s3_abtbMeta = RegEnable(s2_abtbMeta, s2_fire)

  private val predictorMeta = Wire(new NewPredictorMeta)
  predictorMeta.aBtbMeta := s3_abtbMeta
  // TODO: other meta

  io.toFtq.meta.valid := s3_valid
  io.toFtq.meta.bits  := predictorMeta

  s0_pc := MuxCase(
    s0_pcReg,
    Seq(
      redirect.valid -> PrunedAddrInit(redirect.bits.cfiUpdate.target),
      s3_override    -> s3_prediction.target,
      s2_override    -> s2_prediction.target,
      s1_valid       -> s1_prediction.target
    )
  )

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

  /* *** perf pred *** */
  XSPerfAccumulate("toFtqFire", io.toFtq.resp.fire)
  XSPerfAccumulate("s2Override", io.toFtq.resp.fire && io.toFtq.resp.bits.s2Override.valid)
  XSPerfAccumulate("s3Override", io.toFtq.resp.fire && io.toFtq.resp.bits.s3Override.valid)
  XSPerfAccumulate("ubtbHit", io.toFtq.resp.fire && ubtb.io.hit)

  XSPerfAccumulate("s1Invalid", !s1_valid)

  /* *** perf train *** */
  XSPerfAccumulate("train", io.fromFtq.update.valid)
}
