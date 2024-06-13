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
import xiangshan.frontend.{PreDecodeResp, PredCheckerResp}
import utils.XSError

class TraceCheckerResp(implicit p: Parameters) extends TraceBundle {
  val traceRange = UInt(PredictWidth.W)
  val traceValid = UInt(PredictWidth.W)
}

class TraceCheckerIO(implicit p: Parameters) extends TraceBundle {
  val debug_valid = Input(Bool())
  val traceInsts = Input(Vec(PredictWidth, Valid(new TraceInstrBundle())))
  val predictInfo = Input(new TracePredictInfo)
  val preDecode = Input(new PreDecodeResp)
  val predChecker = Input(new PredCheckerResp)
  // TODO: this traceRange is alone, make it better
  val traceRange = Input(UInt(PredictWidth.W))

  val out = Output(new TraceCheckerResp)
}

class TraceChecker(implicit p: Parameters) extends TraceModule {
  val io = IO(new TraceCheckerIO)

  val predRange = io.predictInfo.instRange
  val checkRange = io.predChecker.stage1Out.fixedRange.asUInt
  io.out.traceRange := io.traceRange & checkRange
  io.out.traceValid := VecInit(io.traceInsts.map(_.valid)).asUInt

  /**
   * TraceRange
   * - 1. checkRange < traceRange && traceRange <= predRange
   * This may not happen or the jump is to seqInst
   * Just take normal ifu-redirect
   * - 2. checkRange = traceRange && traceRange <= predRange
   * predChecker finds the fault.
   * Just take normal ifu-redirect
   * - 3.
   */
  // Acutually, we don't  care about the target, we care about range
  // when (predCheckNoRemask) {
  // two case:
  // 1. checker find not fault
  // 2. checker find target fault, or notCFI fault, invalidTaken fault
  // Just do the same as the IFU: [redirect and] wait for next fetch.

  // There are may be two case:
  // 1. predRange same with traceRange: pred is right
  // Then nothing specially to do
  // val
  // 2. predRange is longer than traceRange: pred is wrong/partially wrong
  // Do we need IFU-redirect? NO, just w
  // }


  // debug
  io.traceInsts.zip(io.preDecode.pd).foreach { case (trace, pd) =>
    XSError(trace.valid =/= pd.valid,
      "traceInst should be the same with preDecode.valid")
  }
  when(io.debug_valid) {
    XSError((checkRange.asUInt & predRange.asUInt) === checkRange.asUInt,
      "checkRange should be shorter than predRange")
  }
}
