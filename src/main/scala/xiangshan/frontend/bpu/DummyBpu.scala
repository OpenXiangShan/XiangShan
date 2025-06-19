// Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
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
import utility.SegmentedAddrNext
import utility.XSError
import utility.XSPerfAccumulate
import xiangshan.HasXSParameter
import xiangshan.XSModule
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.PrunedAddrInit
import xiangshan.frontend.bpu.abtb.AheadBtb
import xiangshan.frontend.ftq.FtqToBpuIO

class DummyBpu(implicit p: Parameters) extends XSModule with HasXSParameter with HasBPUConst {
  val io = IO(new Bundle {
    val ctrl    = Input(new BPUCtrl)
    val resetPc = Input(PrunedAddr(PAddrBits))
    val fromFtq = Flipped(new FtqToBpuIO)
    val toFtq   = new BpuToFtqIO
  })

  val uBtb = Module(new DummyMicroBtb)
  val aBtb = Module(new AheadBtb)

  // For some reason s0 stalled, usually FTQ Full
  val s0_stall = Wire(Bool())

  val s0_fire = Wire(Bool())
  val s1_fire = Wire(Bool())
  val s2_fire = Wire(Bool())
  val s3_fire = Wire(Bool())

  val s1_ready = Wire(Bool())
  val s2_ready = Wire(Bool())
  val s3_ready = Wire(Bool())

  val s1_flush = Wire(Bool())
  val s2_flush = Wire(Bool())
  val s3_flush = Wire(Bool())

  val s1_valid = RegInit(false.B)
  val s2_valid = RegInit(false.B)
  val s3_valid = RegInit(false.B)

  val s3_override = WireDefault(false.B)

  val redirect = io.fromFtq.redirect

  val debug_redirect_startPc = redirect.bits.cfiUpdate.pc
  dontTouch(debug_redirect_startPc)

  val resetDone = RegInit(false.B)

  val s0_pc    = WireDefault(0.U.asTypeOf(PrunedAddr(VAddrBits)))
  val s0_pcReg = RegEnable(s0_pc, !s0_stall)

  when(RegNext(RegNext(reset.asBool)) && !reset.asBool) {
    s0_pcReg  := io.resetPc
    resetDone := true.B
  }

  dontTouch(s0_pc)
  dontTouch(s0_pcReg)

  // TODO: remove this？
  val s1_pc = RegEnable(s0_pc, s0_fire)
  val s2_pc = RegEnable(s1_pc, s1_fire)
  val s3_pc = RegEnable(s2_pc, s2_fire)

  val predictorsIn = Seq(uBtb.io.in)
  predictorsIn.foreach { in =>
    in.ctrl := DelayN(io.ctrl, 2)
    // TODO: duplicate pc and fire to solve high fan-out issue
    in.s0_pc   := s0_pc
    in.s0_fire := s0_fire
    in.s1_fire := s1_fire
    in.s2_fire := s2_fire
    in.s3_fire := s3_fire
    in.update  := io.fromFtq.update
    // low power: gate pc higher bits
    in.update.bits.pc := PrunedAddrInit(SegmentedAddrNext(
      io.fromFtq.update.bits.pc.toUInt,
      pcSegments,
      io.fromFtq.update.valid,
      Some("predictors_io_update_pc")
    ).getAddr())
  }

  aBtb.io.fromBpu.startPc.valid := s0_fire
  aBtb.io.fromBpu.startPc.bits  := s0_pc
  aBtb.io.fromBpu.bpuOverride   := s3_override
  aBtb.io.fromBpu.redirectValid := redirect.valid

  val s2_ftqPtr = RegEnable(io.fromFtq.enq_ptr, s1_fire)
  val s3_ftqPtr = RegEnable(s2_ftqPtr, s2_fire)

  s3_flush := redirect.valid
  s2_flush := s3_flush || s3_override
  s1_flush := s2_flush

  s1_ready := s1_fire || !s1_valid || s1_flush
  s2_ready := s2_fire || !s2_valid
  s3_ready := s3_fire || !s3_valid

  s0_fire := s1_ready && resetDone
  s1_fire := s1_valid && s2_ready && io.toFtq.resp.ready
  s2_fire := s2_valid && s3_ready
  s3_fire := s3_valid

  dontTouch(s0_fire)
  dontTouch(s1_fire)
  dontTouch(s2_fire)
  dontTouch(s3_fire)

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

  val aBtbPredictionValid = aBtb.io.toBpu.prediction.valid && aBtb.io.toBpu.prediction.bits.startPc === s1_pc

  val debug_aBtb_startPc_wrong =
    s1_valid && aBtb.io.toBpu.prediction.valid && aBtb.io.toBpu.prediction.bits.startPc =/= s1_pc
  dontTouch(debug_aBtb_startPc_wrong)

  XSPerfAccumulate("use_aBTB_prediction_num", s1_valid && aBtbPredictionValid)
  XSPerfAccumulate("use_uBTB_prediction_num", s1_valid && !aBtbPredictionValid)

  val s1_prediction = Mux(aBtbPredictionValid, aBtb.io.toBpu.prediction.bits, uBtb.io.out.prediction)

  io.toFtq.resp.valid := s1_valid && s2_ready || s3_fire && s3_override

  io.toFtq.resp.bits.prediction  := s1_prediction // FIXME
  io.toFtq.resp.bits.s3_ftqPtr   := s3_ftqPtr
  io.toFtq.resp.bits.s3_override := false.B       // FIXME

  s0_pc := MuxCase(
    s0_pcReg,
    Seq(
      redirect.valid -> PrunedAddrInit(redirect.bits.cfiUpdate.target),
      s3_override    -> s1_prediction.target, // FIXME
      s1_valid       -> s1_prediction.target
    )
  )

  // Power-on reset
  val powerOnResetState = RegInit(true.B)
  when(s0_fire) {
    // When BPU pipeline first time fire, we consider power-on reset is done
    powerOnResetState := false.B
  }
  XSError(
    !powerOnResetState && s0_stall && s0_pc =/= s0_pcReg,
    "s0_stall but s0_pc is different from s0_pcReg"
  )

}
