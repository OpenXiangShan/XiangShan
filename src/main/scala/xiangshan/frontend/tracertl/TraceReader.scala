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
import utility.{CircularQueuePtr, HasCircularQueuePtrHelper, GTimer}
import utils.XSError
import xiangshan.frontend.BranchPredictionRedirect
import xiangshan.RedirectLevel
import utility.ParallelPriorityMux

class TracePredInfoBundle extends Bundle {
  val fixTarget = UInt(64.W)
  val taken = Bool()
}

class TraceInstrBundle(implicit p: Parameters) extends TraceInstrInnerBundle {
  // use for cfi fix
  // we need to know the bpu result to know if the branch should redirect or not
//  val bpuPredInfo = new TracePredInfoBundle

  def hasException = exception =/= 0.U
}

object TraceInstrBundle {
  def apply(rawInst: TraceInstrInnerBundle)
           (implicit p: Parameters): TraceInstrBundle = {
    val bundle = Wire(new TraceInstrBundle)
    rawInst.elements.foreach { case (name, elt) =>
      bundle.elements(name) := elt
    }
    bundle
  }
}

class TraceReaderIO(implicit p: Parameters) extends TraceBundle {
  // recv.valid from f3_fire, bits.instNum from range
  val recv = Flipped(Valid(new TraceRecvInfo()))
  // BranchPredictionRedirect === Redirect with some traits
  val redirect = Flipped(Valid(new BranchPredictionRedirect()))
  val startSignal = Input(Bool())

  // traceInst should always be valid
  val traceInsts = Output(Vec(PredictWidth, new TraceInstrBundle()))
}

class TraceBufferPtr(Size: Int)(implicit p: Parameters) extends CircularQueuePtr[TraceBufferPtr](Size)

class TraceReader(implicit p: Parameters) extends TraceModule
  with TraceParams
  with HasCircularQueuePtrHelper {
  val io = IO(new TraceReaderIO())
  dontTouch(io)

  val redirect = WireInit(io.redirect)
  val traceBuffer = RegInit(0.U.asTypeOf(Vec(TraceBufferSize, new TraceInstrBundle())))
  val traceReaderHelper = Module(new TraceReaderHelper(TraceFetchWidth))
  val traceRedirecter = Module(new TraceRedirectHelper)
  val deqPtr = RegInit(0.U.asTypeOf(new TraceBufferPtr(TraceBufferSize)))
  val enqPtr = RegInit(0.U.asTypeOf(new TraceBufferPtr(TraceBufferSize)))
  val enqPtrVec = Wire(Vec(TraceFetchWidth, new TraceBufferPtr(TraceBufferSize)))

  val workingState = RegInit(false.B)
  val startCount = RegInit(0.U(4.W))
  when (!workingState && (startCount < 10.U)) {
    startCount := startCount + 1.U
  }
  when (startCount === 5.U) {
    workingState := true.B
  }


  val readTraceEnable = !isFull(enqPtr, deqPtr) &&
    (hasFreeEntries(enqPtr, deqPtr) >= TraceFetchWidth.U) &&
    workingState &&
    !redirect.valid
  val readTraceEnableForHelper = readTraceEnable
  val readTraceEnableForPtr = readTraceEnable
  val readTraceEnableForBuffer = readTraceEnable
  enqPtrVec.zipWithIndex.foreach { case (e, i) =>
    e := enqPtr + i.U
    // e := RegNext(enqPtr + i.U, init = 0.U.asTypeOf(new TraceBufferPtr(TraceBufferSize)))
  }

  when (readTraceEnableForBuffer) {
    (0 until TraceFetchWidth).foreach { case i =>
      traceBuffer(enqPtrVec(i).value) := TraceInstrBundle(traceReaderHelper.insts(i))
    }
  }

  traceRedirecter.clock := clock
  traceRedirecter.reset := reset
  traceRedirecter.enable := redirect.valid
  traceRedirecter.InstID := redirect.bits.traceInfo.InstID +
    Mux(RedirectLevel.flushItself(redirect.bits.level), 0.U, 1.U)

  traceReaderHelper.clock := clock
  traceReaderHelper.reset := reset
  traceReaderHelper.enable := readTraceEnableForHelper

  io.traceInsts.zipWithIndex.foreach { case (inst, i) =>
    val ptr = (deqPtr + i.U).value
    inst := traceBuffer(ptr)
  }

  when(io.recv.valid) {
    deqPtr := deqPtr + io.recv.bits.instNum
  }
  when (readTraceEnableForPtr) {
    enqPtr := enqPtr + TraceFetchWidth.U
  }

  when (redirect.valid) {
    enqPtr := 0.U.asTypeOf(new TraceBufferPtr(TraceBufferSize))
    deqPtr := 0.U.asTypeOf(new TraceBufferPtr(TraceBufferSize))
    traceBuffer.map(_ := 0.U.asTypeOf(new TraceInstrBundle))
  }

  // debug check

  // redirect check: instid should match with target
  // when redirect, record the info and check it after 4 cycle(after read from dpic)
  val debugRedirectValid = Reg(Bool())
  val debugRedirectInstID = Reg(UInt(64.W))
  val debugRedirectTarget = Reg(UInt(64.W))
  val debugRedirectDelayCounter = RegInit(0.U(3.W))
  val RedirectDelayNum = 3
  when (debugRedirectValid && (debugRedirectDelayCounter > 1.U)) {
    debugRedirectDelayCounter := debugRedirectDelayCounter - 1.U
  }
  when (debugRedirectDelayCounter === 0.U && debugRedirectValid) {
    debugRedirectValid := false.B
    val hitInstIDVec = traceBuffer.map(_.InstID === debugRedirectInstID)
    val hitTargetVec = traceBuffer.map(_.pcVA === debugRedirectTarget)
    hitInstIDVec.zip(hitTargetVec).zipWithIndex. foreach {
      case ((id, t), idx) =>
        XSError(id =/= t, s"Redirect Info(InstID/target) not match in traceBuffer($idx)")
    }
  }
  when (redirect.valid) {
    debugRedirectDelayCounter := RedirectDelayNum.U
    debugRedirectInstID := traceRedirecter.InstID
    debugRedirectTarget := redirect.bits.cfiUpdate.target
  }


  if (!TraceEnableDuplicateFlush) {
    when (redirect.valid) {
      XSError(redirect.bits.traceInfo.InstID + 1.U =/= traceBuffer(deqPtr.value).InstID,
        "TraceEnableDuplicateFlush is false. Please check wrong path redirect")
    }
  }

  XSError(!isFull(enqPtr, deqPtr) && (enqPtr < deqPtr), "enqPtr should always be larger than deqPtr")
  XSError(io.recv.valid && ((deqPtr + io.recv.bits.instNum) >= enqPtr),
    "Reader should not read more than what is in the buffer. Error in ReaderHelper or Ptr logic.")
}
