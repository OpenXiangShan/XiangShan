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

class DecodeStage(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    // from Ibuffer
    val in = Vec(DecodeWidth, Flipped(DecoupledIO(new CtrlFlow)))
    // from memblock
    val memPredUpdate = Vec(StorePipelineWidth, Input(new MemPredUpdateReq))
    // to DecBuffer
    val out = Vec(DecodeWidth, DecoupledIO(new CfCtrl))
    // waitable ctrl
    val csrCtrl = Input(new CustomCSRCtrlIO)
  })

  val decoders = Seq.fill(DecodeWidth)(Module(new DecodeUnit))

  // basic wait table load violation predictor (for debug only)
  val waittable = Module(new WaitTable)
  // store set load violation predictor stage 1: SSIT look up
  val ssit = Module(new SSIT)

  for (i <- 0 until DecodeWidth) {
    decoders(i).io.enq.ctrl_flow <> io.in(i).bits

    // read waittable, update loadWaitBit
    waittable.io.raddr(i) := io.in(i).bits.foldpc
    decoders(i).io.enq.ctrl_flow.loadWaitBit := waittable.io.rdata(i)

    // read SSIT, get SSID
    ssit.io.raddr(i) := io.in(i).bits.foldpc
    decoders(i).io.enq.ctrl_flow.storeSetHit := ssit.io.rdata(i).valid
    decoders(i).io.enq.ctrl_flow.loadWaitStrict := ssit.io.rdata(i).strict
    decoders(i).io.enq.ctrl_flow.ssid := ssit.io.rdata(i).ssid

    // csr control
    decoders(i).io.csrCtrl := io.csrCtrl

    io.out(i).valid      := io.in(i).valid
    io.out(i).bits       := decoders(i).io.deq.cf_ctrl
    io.in(i).ready       := io.out(i).ready
  }

  for (i <- 0 until StorePipelineWidth) {
    waittable.io.update(i) <> RegNext(io.memPredUpdate(i))
  }
  waittable.io.csrCtrl <> io.csrCtrl
  ssit.io.update <> RegNext(io.memPredUpdate(0))
  ssit.io.csrCtrl <> io.csrCtrl

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
    when (d.valid) {
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

  val loadWaitBitSet = PopCount(io.out.map(o => o.fire() && o.bits.cf.loadWaitBit))
  XSPerfAccumulate("loadWaitBitSet", loadWaitBitSet)
  val storeSetHit = PopCount(io.out.map(o => o.fire() && o.bits.cf.storeSetHit))
  XSPerfAccumulate("storeset_ssit_hit", storeSetHit)

  val hasValid = VecInit(io.in.map(_.valid)).asUInt.orR
  XSPerfAccumulate("utilization", PopCount(io.in.map(_.valid)))
  XSPerfAccumulate("waitInstr", PopCount((0 until DecodeWidth).map(i => io.in(i).valid && !io.in(i).ready)))
  XSPerfAccumulate("stall_cycle", hasValid && !io.out(0).ready)
  val perfinfo = IO(new Bundle(){
    val perfEvents = Output(new PerfEventsBundle(6))
  })
  val perfEvents = Seq(
    ("decoder_fused_instr          ", PopCount(fusionDecoder.io.out.map(_.fire))                                 ),
    ("decoder_waitInstr            ", PopCount((0 until DecodeWidth).map(i => io.in(i).valid && !io.in(i).ready))),
    ("decoder_stall_cycle          ", hasValid && !io.out(0).ready                                               ),
    ("decoder_utilization          ", PopCount(io.in.map(_.valid))                                               ),
    ("decoder_loadWaitBitSet       ", loadWaitBitSet                                                             ),
    ("decoder_storeset_ssit_hit    ", storeSetHit                                                                ),
  )

  for (((perf_out,(perf_name,perf)),i) <- perfinfo.perfEvents.perf_events.zip(perfEvents).zipWithIndex) {
    perf_out.incr_step := RegNext(perf)
  }
}
