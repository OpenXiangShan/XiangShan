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
import xiangshan.frontend.BranchPredictionRedirect
import utility.ChiselMap
import utils.XSPerfAccumulate

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

  val redirect = Input(new Bundle {
   val fromBackend = Valid(new BranchPredictionRedirect()) // backend -> ftq -> ifu
   val fromIFUBPU = Bool()
  })

  val out = new TraceDriverOutput()
}

class TraceDriver(implicit p: Parameters) extends TraceModule {
  val io = IO(new TraceDriverIO())
  dontTouch(io)

  // Corner Case: wrong path, but correct pc.
  //   The path check at the TraceAligner just use pc-match.
  //   But when wrong path, the pc is correct, so we need to check the path again.
  //   For example, fetch bundle: startAddr 0xc9e, next start 0xcae. FtqOffset.valid is false
  //     At 0xca6, there is a branch to 0xcae(but predicted to not taken).
  //     correct path: fetch0: 0xc9e-0xca6 -> fetch1: 0xcae-
  //     wrong-path: fetch0: 0xc9e-0xcae -> fetch1: 0xcae-
  //     The wrong path's next fecth req's startAddr is the same with correct path.
  //   So we need to explicitly check/record the path is wrong or correct.
  // when wrong path, block ifu-go, ibuffer-recv
  val wrongPathPC = Reg(UInt(64.W))
//  val wrongPathNextPC = Reg(UInt(64.W))
  val wrongPathPCWire = Wire(UInt(64.W))
//  val wrongPathPCNextWire = Wire(UInt(64.W))
  val wrongPathState = RegInit(false.B)
  when (io.fire) {
    wrongPathState := (io.traceRange =/= io.ifuRange)
    when ((io.traceRange =/= io.ifuRange)) {
      wrongPathPC := wrongPathPCWire
//      wrongPathNextPC := wrongPathPCNextWire
    }
  }
  when (io.redirect.fromBackend.valid || io.redirect.fromIFUBPU) {
    wrongPathState := false.B
  }

  val pcMismatch = io.out.recv.bits.instNum === 0.U
  io.out.block := pcMismatch || wrongPathState

  val traceValid = VecInit(io.traceInsts.map(_.valid)).asUInt
  io.out.recv.bits.instNum := PopCount(io.traceRange & traceValid)
  io.out.recv.valid := io.fire

  wrongPathPCWire := ParallelPosteriorityMux(io.traceRange & traceValid, io.traceInsts.map(_.bits.pcVA))

  io.out.endWithCFI := ParallelPosteriorityMux(io.traceRange & traceValid, io.traceInsts.map(x =>
    (x.bits.branchType =/= 0.U) && x.bits.branchTaken(0)
  ))

  class TraceWrongPathCycle extends Bundle {
    val pcVA = UInt(64.W)
//    val nextPCVA = UInt(64.W)
  }
  val traceWrongPathCycle = ChiselMap.createTable("wrongPathCycle", Vec(1, new TraceWrongPathCycle()), basicDB =true)
  val perf_traceWPC = Wire(Vec(1, Valid(new TraceWrongPathCycle())))
  perf_traceWPC(0).valid := wrongPathState
  perf_traceWPC(0).bits.pcVA := wrongPathPC
//  perf_traceWPC(0).bits.nextPCVA := wrongPathNextPC
  traceWrongPathCycle.log(perf_traceWPC, 1.U, "wrongpathCycle", clock, reset)
  XSPerfAccumulate("pcMisMatch", pcMismatch)
  XSPerfAccumulate("wrongPathState", wrongPathState)
  XSPerfAccumulate("pcAndwrongPath", pcMismatch && wrongPathState)
  XSPerfAccumulate("pcOrwrongPath", pcMismatch || wrongPathState)
}
