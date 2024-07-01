/** *************************************************************************************
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
 * ************************************************************************************* */
package xiangshan.frontend.tracertl

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.ParallelPosteriorityMux

class TraceDriverOutput(implicit p: Parameters) extends TraceBundle {
  // when block true, the fetch is at the wrong path, should block ifu-go, ibuffer-recv
  val block = Output(Bool())
  val recv = ValidIO(new TraceRecvInfo())

  // to reset lastHalfValid(concedate2B)
  val endWithCFI = Output(Bool())
}

class TraceDriverIO(implicit p: Parameters) extends TraceBundle {
  val fire = Input(Bool())
  val traceInsts = Input(Vec(PredictWidth, Valid(new TraceInstrBundle())))
  val traceRange = Input(UInt(PredictWidth.W))
  val predInfo = Input(new TracePredictInfo())

  val out = new TraceDriverOutput()
}

class TraceDriver(implicit p: Parameters) extends TraceModule {
  val io = IO(new TraceDriverIO())
  dontTouch(io)

  val traceValid = VecInit(io.traceInsts.map(_.valid)).asUInt
  io.out.block := io.out.recv.bits.instNum === 0.U // may be we need more precise control signal
  io.out.recv.bits.instNum := PopCount(io.traceRange & traceValid)
  io.out.recv.valid := io.fire

  io.out.endWithCFI := ParallelPosteriorityMux(io.traceRange & traceValid, io.traceInsts.map(x =>
    (x.bits.branchType =/= 0.U) && x.bits.branchTaken(0)
  ))
}
