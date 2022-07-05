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
import xiangshan.backend.rename.RatReadPort

class DecodeStage(implicit p: Parameters) extends XSModule with HasPerfEvents {
  val io = IO(new Bundle() {
    // from Ibuffer
    val in = Vec(DecodeWidth, Flipped(DecoupledIO(new CtrlFlow)))
    // to Rename
    val out = Vec(DecodeWidth, DecoupledIO(new CfCtrl))
    val fusionInfo = Vec(DecodeWidth - 1, new FusionDecodeInfo)
    // RAT read
    val intRat = Vec(RenameWidth, Vec(3, Flipped(new RatReadPort)))
    val fpRat = Vec(RenameWidth, Vec(4, Flipped(new RatReadPort)))
    // csr control
    val csrCtrl = Input(new CustomCSRCtrlIO)
  })

  val decoders = Seq.fill(DecodeWidth)(Module(new DecodeUnit))

  for (i <- 0 until DecodeWidth) {
    decoders(i).io.enq.ctrl_flow <> io.in(i).bits

    // csr control
    decoders(i).io.csrCtrl := io.csrCtrl

    io.out(i).valid      := io.in(i).valid
    io.out(i).bits       := decoders(i).io.deq.cf_ctrl
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

  // instruction fusion
  val fusionDecoder = Module(new FusionDecoder())
  fusionDecoder.io.in.zip(io.in).foreach{ case (d, in) =>
    // TODO: instructions with exceptions should not be considered fusion
    d.valid := in.valid
    d.bits := in.bits.instr
  }
  fusionDecoder.io.dec := decoders.map(_.io.deq.cf_ctrl.ctrl)
  fusionDecoder.io.out.zip(io.out.dropRight(1)).zipWithIndex.foreach{ case ((d, out), i) =>
    d.ready := out.ready
    when (d.valid && !io.csrCtrl.singlestep) { // TODO && nosinglestep
      out.bits.ctrl := d.bits
      // TODO: remove this
      // Dirty code for ftq update
      val sameFtqPtr = out.bits.cf.ftqPtr.value === io.out(i + 1).bits.cf.ftqPtr.value
      val ftqOffset0 = out.bits.cf.ftqOffset
      val ftqOffset1 = io.out(i + 1).bits.cf.ftqOffset
      val ftqOffsetDiff = ftqOffset1 - ftqOffset0
      val cond1 = sameFtqPtr && ftqOffsetDiff === 1.U
      val cond2 = sameFtqPtr && ftqOffsetDiff === 2.U
      val cond3 = !sameFtqPtr && ftqOffset1 === 0.U
      val cond4 = !sameFtqPtr && ftqOffset1 === 1.U
      out.bits.ctrl.commitType := Mux(cond1, 4.U, Mux(cond2, 5.U, Mux(cond3, 6.U, 7.U)))
      XSError(!cond1 && !cond2 && !cond3 && !cond4, p"new condition $sameFtqPtr $ftqOffset0 $ftqOffset1\n")
    }
  }
  fusionDecoder.io.clear.zip(io.out.map(_.valid)).foreach{ case (c, v) =>
    when (c) {
      v := false.B
    }
  }
  io.fusionInfo := fusionDecoder.io.info

  val hasValid = VecInit(io.in.map(_.valid)).asUInt.orR
  XSPerfAccumulate("utilization", PopCount(io.in.map(_.valid)))
  XSPerfAccumulate("waitInstr", PopCount((0 until DecodeWidth).map(i => io.in(i).valid && !io.in(i).ready)))
  XSPerfAccumulate("stall_cycle", hasValid && !io.out(0).ready)

  val fusionValid = RegNext(VecInit(fusionDecoder.io.out.map(_.fire)))
  val inFire = io.in.map(in => RegNext(in.valid && !in.ready))
  val perfEvents = Seq(
    ("decoder_fused_instr", PopCount(fusionValid)       ),
    ("decoder_waitInstr",   PopCount(inFire)            ),
    ("decoder_stall_cycle", hasValid && !io.out(0).ready),
    ("decoder_utilization", PopCount(io.in.map(_.valid))),
  )
  generatePerfEvent()
}
