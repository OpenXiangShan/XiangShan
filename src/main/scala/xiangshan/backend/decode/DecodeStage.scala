/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
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
    decoders(i).io.enq.ctrl_flow.ssid := ssit.io.rdata(i).ssid

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

  val loadWaitBitSet = PopCount(io.out.map(o => o.fire() && o.bits.cf.loadWaitBit))
  XSPerfAccumulate("loadWaitBitSet", loadWaitBitSet)
  val storeSetHit = PopCount(io.out.map(o => o.fire() && o.bits.cf.storeSetHit))
  XSPerfAccumulate("storeset_ssit_hit", storeSetHit)

  val hasValid = VecInit(io.in.map(_.valid)).asUInt.orR
  XSPerfAccumulate("utilization", PopCount(io.in.map(_.valid)))
  XSPerfAccumulate("waitInstr", PopCount((0 until DecodeWidth).map(i => io.in(i).valid && !io.in(i).ready)))
  XSPerfAccumulate("stall_cycle", hasValid && !io.out(0).ready)
}
