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
import chisel3.util.experimental.BoringUtils
import org.chipsalliance.cde.config.Parameters
import utility.ParallelPosteriorityMux
import xiangshan.frontend.BranchPredictionRedirect
import utility.{ChiselMap, XSPerfAccumulate}

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
  val ifuRange = Input(UInt(PredictWidth.W)) // fixed Range
  val otherBlock = Input(Bool()) // when trace not ready(only in Sim-FPGA)

  val redirect = Input(new Bundle {
   val fromBackend = Valid(new BranchPredictionRedirect()) // backend -> ftq -> ifu
   val fromIFUBPU = Bool()
  })

  val out = new TraceDriverOutput()
}

class TraceDriver(implicit p: Parameters) extends TraceModule {
  val io = IO(new TraceDriverIO())
  dontTouch(io)

  // when fastSim instruction is drained, but fastSim memory address is not drained
  // block the fetch
  // val fastSimMemAddrFinish = TraceBoringUtils.addSink("TraceFastSimMemoryFinish").asBool
  val fastSimMemAddrFinish = WireInit(true.B)
  val firstInstFastSim = PriorityMux(io.traceInsts.map(_.valid), io.traceInsts.map(_.bits.fastSimulation(0).asBool))
  BoringUtils.addSink(fastSimMemAddrFinish, "TraceFastSimMemoryFinish")
  // BoringUtils.addSource(firstInstFastSim, "TraceFastSimInstFinish")

  val pcMismatch = io.out.recv.bits.instNum === 0.U
  io.out.block := pcMismatch || (!firstInstFastSim && !fastSimMemAddrFinish) || io.otherBlock
  XSPerfAccumulate("FastSimMemoryBlock", !firstInstFastSim && !fastSimMemAddrFinish)
  XSPerfAccumulate("FastSimFetchBlock", firstInstFastSim && fastSimMemAddrFinish)
  XSPerfAccumulate("FastSimFetchCycle", firstInstFastSim)
  XSPerfAccumulate("FastSimMemoryCycle", !fastSimMemAddrFinish)

  val finalRange = io.traceRange & io.ifuRange

  val traceValid = VecInit(io.traceInsts.map(_.valid)).asUInt
  io.out.recv.bits.instNum := PopCount(finalRange & traceValid)
  io.out.recv.valid := io.fire

  io.out.endWithCFI := ParallelPosteriorityMux(finalRange & traceValid, io.traceInsts.map(x =>
    (x.bits.branchType =/= 0.U) && x.bits.branchTaken(0)
  ))

  XSPerfAccumulate("pcMisMatch", pcMismatch)
}
