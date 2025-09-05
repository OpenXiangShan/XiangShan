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
import utility.{CircularQueuePtr, HasCircularQueuePtrHelper, GTimer, XSError}
import xiangshan.frontend.BranchPredictionRedirect
import xiangshan.RedirectLevel
import utility.ParallelPriorityMux
import chisel3.experimental.Trace

class TracePredInfoBundle extends Bundle {
  val fixTarget = UInt(64.W)
  val taken = Bool()
}

class TraceInstrBundle(implicit p: Parameters) extends TraceInstrInnerBundle {
  // use for cfi fix
  // we need to know the bpu result to know if the branch should redirect or not
//  val bpuPredInfo = new TracePredInfoBundle
  val isWrongPath = Bool()
  val hasTriggeredExuRedirect = Bool()

  def hasException = exception =/= 0.U
  def isForceJump = exception(7)

  def initMoreFromRaw() = {
    isWrongPath := false.B
    hasTriggeredExuRedirect := false.B
  }
}

object TraceInstrBundle {
  def apply(rawInst: TraceInstrInnerBundle)
           (implicit p: Parameters): TraceInstrBundle = {
    val bundle = Wire(new TraceInstrBundle)
    rawInst.elements.foreach { case (name, elt) =>
      bundle.elements(name) := elt
    }
    bundle.initMoreFromRaw()
    bundle
  }
}

class TraceReaderIO(implicit p: Parameters) extends TraceBundle {
  // recv.valid from f3_fire, bits.instNum from range
  val recv = Flipped(Valid(new TraceRecvInfo()))
  // BranchPredictionRedirect === Redirect with some traits
  val redirect = Flipped(Valid(new BranchPredictionRedirect()))

  // traceInst should always be valid
  val traceInsts = Output(Vec(PredictWidth, new TraceInstrBundle()))

  // pc match
  val pcMatch = Flipped(new TracePCMatchBundle())
}

class TraceBufferPtr(Size: Int)(implicit p: Parameters) extends CircularQueuePtr[TraceBufferPtr](Size)

class TraceReaderHelperWrapper(implicit p: Parameters) extends TraceModule
  with TraceParams
  with HasCircularQueuePtrHelper {
  val io = IO(new Bundle {
    val enable = Input(Bool())
    val insts = Output(Vec(TraceFetchWidth, new TraceInstrInnerBundle()))
    val redirect_valid = Input(Bool())
    val redirect_instID = Input(UInt(64.W))
    val workingState = Input(Bool())
  })

  if (env.TraceRTLSYNTHESIS) {
    val file_reader = Module(new TraceRTL_FileReader())
    file_reader.clock := clock
    file_reader.reset := reset
    file_reader.enable := io.enable
    file_reader.redirect_valid := io.redirect_valid
    file_reader.redirect_instID := io.redirect_instID
    file_reader.workingState := io.workingState

    io.insts.zipWithIndex.foreach { case (inst, i) =>
      inst.InstID := file_reader.index + i.U
      inst.pcVA := file_reader.instr_pc_va(i)
      inst.pcPA := file_reader.instr_pc_pa(i)
      inst.memoryAddrVA := file_reader.memory_addr_va(i);
      inst.memoryAddrPA := file_reader.memory_addr_pa(i);
      inst.target := file_reader.target(i);
      inst.inst := file_reader.instr(i);
      inst.memorySize := file_reader.memory_size(i);
      inst.memoryType := file_reader.memory_type(i);
      inst.branchTaken := file_reader.branch_taken(i);
      inst.branchType := file_reader.branch_type(i);
      inst.exception := file_reader.exception(i);
    }
  } else {
    val traceReaderHelper = Module(new TraceReaderHelper(TraceFetchWidth))
    val traceRedirecter = Module(new TraceRedirectHelper)

    traceRedirecter.clock := clock
    traceRedirecter.reset := reset
    traceRedirecter.enable := io.redirect_valid
    traceRedirecter.InstID := io.redirect_instID

    traceReaderHelper.clock := clock
    traceReaderHelper.reset := reset
    traceReaderHelper.enable := io.enable

    io.insts := traceReaderHelper.insts
  }

}


class TraceReader(implicit p: Parameters) extends TraceModule
  with TraceParams
  with HasCircularQueuePtrHelper {
  val io = IO(new TraceReaderIO())
  dontTouch(io)

  if (!env.TraceRTLMode) {
    io <> DontCare
  } else {
    val redirect = WireInit(io.redirect)
    val traceBuffer = RegInit(0.U.asTypeOf(Vec(TraceBufferSize, new TraceInstrBundle())))
    val trace_reader_helper = Module(new TraceReaderHelperWrapper())
    // val traceReaderHelper = Module(new TraceReaderHelper(TraceFetchWidth))
    // val traceRedirecter = Module(new TraceRedirectHelper)
    val deqPtr = RegInit(0.U.asTypeOf(new TraceBufferPtr(TraceBufferSize)))
    val enqPtr = RegInit(0.U.asTypeOf(new TraceBufferPtr(TraceBufferSize)))
    val enqPtrVec = Wire(Vec(TraceFetchWidth, new TraceBufferPtr(TraceBufferSize)))

    val workingState = RegInit(false.B)
    val startCount = RegInit(0.U(4.W))
    when (startCount < 10.U) {
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
        traceBuffer(enqPtrVec(i).value) := TraceInstrBundle(trace_reader_helper.io.insts(i))
      }
    }

    trace_reader_helper.io.enable := readTraceEnableForHelper
    trace_reader_helper.io.redirect_valid := redirect.valid && workingState
    trace_reader_helper.io.redirect_instID := redirect.bits.traceInfo.InstID +
      Mux(RedirectLevel.flushItself(redirect.bits.level), 0.U, 1.U)
    trace_reader_helper.io.workingState := workingState

    // traceRedirecter.clock := clock
    // traceRedirecter.reset := reset
    // traceRedirecter.enable := redirect.valid
    // traceRedirecter.InstID := redirect.bits.traceInfo.InstID +
    //   Mux(RedirectLevel.flushItself(redirect.bits.level), 0.U, 1.U)

    // traceReaderHelper.clock := clock
    // traceReaderHelper.reset := reset
    // traceReaderHelper.enable := readTraceEnableForHelper

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

    // TODO: when redirect target instID is deqPtr, no need to flush
    when (redirect.valid) {
      enqPtr := 0.U.asTypeOf(new TraceBufferPtr(TraceBufferSize))
      deqPtr := 0.U.asTypeOf(new TraceBufferPtr(TraceBufferSize))
      traceBuffer.map(_ := 0.U.asTypeOf(new TraceInstrBundle))
    }

    io.pcMatch.found  := Cat(traceBuffer.map(_.pcVA === io.pcMatch.pcVA)).orR

    // debug check
    when (redirect.valid) {
      XSError(redirect.bits.traceInfo.isWrongPath, "WrongPath instruction will not trigger redirect")
    }


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
      debugRedirectInstID := trace_reader_helper.io.redirect_instID
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
}
