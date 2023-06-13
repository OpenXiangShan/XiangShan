/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package xiangshan.backend.decode

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import utility._
import xiangshan.backend.rename.RatReadPort

class DecodeStage(implicit p: Parameters) extends XSModule with HasPerfEvents {
  val io = IO(new Bundle() {
    // from Ibuffer
    val in = Vec(DecodeWidth, Flipped(DecoupledIO(new CtrlFlow)))
    // to Rename
    val out = Vec(DecodeWidth, DecoupledIO(new CfCtrl))
    // RAT read
    val intRat = Vec(RenameWidth, Vec(3, Flipped(new RatReadPort)))
    val fpRat = Vec(RenameWidth, Vec(4, Flipped(new RatReadPort)))
    // csr control
    val csrCtrl = Input(new CustomCSRCtrlIO)
    // perf only
    val fusion = Vec(DecodeWidth - 1, Input(Bool()))
    val stallReason = new Bundle {
      val in = Flipped(new StallReasonIO(DecodeWidth))
      val out = new StallReasonIO(DecodeWidth)
    }
  })

  val decoders = Seq.fill(DecodeWidth)(Module(new DecodeUnit))
  val debug_globalCounter = RegInit(0.U(XLEN.W))

  for (i <- 0 until DecodeWidth) {
    decoders(i).io.enq.ctrl_flow <> io.in(i).bits

    // csr control
    decoders(i).io.csrCtrl := io.csrCtrl

    io.out(i).bits       := DontCare
    io.out(i).valid      := io.in(i).valid
    io.out(i).bits       := decoders(i).io.deq.cf_ctrl
    io.out(i).bits.ctrl.debug_globalID := debug_globalCounter + PopCount((0 until i+1).map(io.out(_).fire))
    io.in(i).ready       := io.out(i).ready

    // We use the lsrc/ldest before fusion decoder to read RAT for better timing.
    io.intRat(i)(0).addr := decoders(i).io.deq.cf_ctrl.ctrl.lsrc(0)
    io.intRat(i)(1).addr := decoders(i).io.deq.cf_ctrl.ctrl.lsrc(1)
    io.intRat(i)(2).addr := decoders(i).io.deq.cf_ctrl.ctrl.ldest
    io.intRat(i).foreach(_.hold := !io.out(i).ready)

    // Floating-point instructions can not be fused now.
    io.fpRat(i)(0).addr := decoders(i).io.deq.cf_ctrl.ctrl.lsrc(0)
    io.fpRat(i)(1).addr := decoders(i).io.deq.cf_ctrl.ctrl.lsrc(1)
    io.fpRat(i)(2).addr := decoders(i).io.deq.cf_ctrl.ctrl.lsrc(2)
    io.fpRat(i)(3).addr := decoders(i).io.deq.cf_ctrl.ctrl.ldest
    io.fpRat(i).foreach(_.hold := !io.out(i).ready)
  }

  val hasValid = VecInit(io.in.map(_.valid)).asUInt.orR

  debug_globalCounter := debug_globalCounter + PopCount(io.out.map(_.fire))

  io.stallReason.in.backReason := io.stallReason.out.backReason
  io.stallReason.out.reason.zip(io.stallReason.in.reason).zip(io.in.map(_.valid)).foreach { case ((out, in), valid) =>
    out := Mux(io.stallReason.out.backReason.valid,
               io.stallReason.out.backReason.bits,
               Mux(valid, TopDownCounters.NoStall.id.U, in))
  }

  XSPerfAccumulate("utilization", PopCount(io.in.map(_.valid)))
  XSPerfAccumulate("waitInstr", PopCount((0 until DecodeWidth).map(i => io.in(i).valid && !io.in(i).ready)))
  XSPerfAccumulate("stall_cycle", hasValid && !io.out(0).ready)

  XSPerfHistogram("slots_fire", PopCount(io.out.map(_.fire)), true.B, 0, DecodeWidth+1, 1)
  XSPerfHistogram("slots_valid_pure", PopCount(io.in.map(_.valid)), io.out(0).fire, 0, DecodeWidth+1, 1)
  XSPerfHistogram("slots_valid_rough", PopCount(io.in.map(_.valid)), true.B, 0, DecodeWidth+1, 1)

  val fusionValid = RegNext(io.fusion)
  val inFire = io.in.map(in => RegNext(in.valid && !in.ready))
  val perfEvents = Seq(
    ("decoder_fused_instr", PopCount(fusionValid)       ),
    ("decoder_waitInstr",   PopCount(inFire)            ),
    ("decoder_stall_cycle", hasValid && !io.out(0).ready),
    ("decoder_utilization", PopCount(io.in.map(_.valid))),
  )
  generatePerfEvent()
}
