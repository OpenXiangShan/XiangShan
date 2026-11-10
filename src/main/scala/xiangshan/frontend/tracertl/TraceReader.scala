package xiangshan.frontend.tracertl

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.CircularQueuePtr
import utility.HasCircularQueuePtrHelper
import utility.XSError
import utility.XSPerfAccumulate
import xiangshan.Redirect
import xiangshan.RedirectLevel

class TraceReaderIO(implicit p: Parameters) extends TraceBundle {
  val recv       = Flipped(Valid(new TraceRecvInfo()))
  val redirect   = Flipped(Valid(new Redirect()))
  val traceInsts = Output(Valid(Vec(trtl.TracePredictWidth, new TraceInstrBundle())))
}

class TraceBufferPtr(size: Int) extends CircularQueuePtr[TraceBufferPtr](size)

class TraceReader(implicit p: Parameters) extends TraceModule with HasCircularQueuePtrHelper {
  val io = IO(new TraceReaderIO())

  val traceBuffer = RegInit(0.U.asTypeOf(Vec(trtl.TraceBufferSize, new TraceInstrBundle())))
  val enqPtr      = RegInit(0.U.asTypeOf(new TraceBufferPtr(trtl.TraceBufferSize)))
  val deqPtr      = RegInit(0.U.asTypeOf(new TraceBufferPtr(trtl.TraceBufferSize)))

  val workingState = RegNext(TraceWarmUp(), false.B)

  val redirectValid = io.redirect.valid

  val readTraceEnable = hasFreeEntries(enqPtr, deqPtr) >= trtl.TraceFetchWidth.U &&
    workingState &&
    !redirectValid

  val traceReaderHelper = Module(new TraceReaderHelper(trtl.TraceFetchWidth))
  traceReaderHelper.clock  := clock
  traceReaderHelper.reset  := reset
  traceReaderHelper.enable := readTraceEnable

  // Exceptions and synthetic force-jumps transfer control after the current
  // trace instruction even when the backend redirect flushes the ROB entry.
  val redirectTrace = io.redirect.bits.traceInfo.get
  val redirectFlushItself = RedirectLevel.flushItself(io.redirect.bits.level) &&
    !redirectTrace.isForceJump &&
    !redirectTrace.hasException

  val traceRedirectHelper = Module(new TraceRedirectHelper)
  traceRedirectHelper.clock  := clock
  traceRedirectHelper.reset  := reset
  traceRedirectHelper.enable := redirectValid && workingState
  traceRedirectHelper.InstID := Mux(
    redirectFlushItself,
    redirectTrace.InstID,
    redirectTrace.InstID + 1.U
  )(trtl.TraceInstIDWidth - 1, 0)
  traceRedirectHelper.preserveDriveBefore := false.B

  when(readTraceEnable) {
    (0 until trtl.TraceFetchWidth).foreach { i =>
      traceBuffer((enqPtr + i.U).value).elements.foreach { case (name, field) =>
        val raw = traceReaderHelper.insts(name)(i)
        field := raw(field.getWidth - 1, 0)
      }
    }
    enqPtr := enqPtr + trtl.TraceFetchWidth.U
  }

  when(io.recv.valid) {
    deqPtr := deqPtr + io.recv.bits.instNum
  }

  when(redirectValid) {
    enqPtr := 0.U.asTypeOf(new TraceBufferPtr(trtl.TraceBufferSize))
    deqPtr := 0.U.asTypeOf(new TraceBufferPtr(trtl.TraceBufferSize))
    traceBuffer.foreach(_ := 0.U.asTypeOf(new TraceInstrBundle()))
  }

  // When S3 consumes the current packet, let S2 observe the following packet
  // in the same cycle.  The registered deqPtr advances at the clock edge, so
  // without this look-ahead TraceRTL has to insert a bubble between packets.
  val visibleRecvValid = io.recv.valid && !redirectValid
  val visibleDeqPtr    = deqPtr + Mux(visibleRecvValid, io.recv.bits.instNum, 0.U)
  io.traceInsts.bits.zipWithIndex.foreach { case (inst, i) =>
    inst := traceBuffer((visibleDeqPtr + i.U).value)
  }
  io.traceInsts.valid := !redirectValid &&
    distanceBetween(enqPtr, visibleDeqPtr) >= trtl.TracePredictWidth.U

  XSPerfAccumulate("TraceReaderValid", io.traceInsts.valid)
  XSPerfAccumulate("TraceReaderNotValid", !io.traceInsts.valid)

  XSError(!isFull(enqPtr, deqPtr) && enqPtr < deqPtr, "TraceReader enqPtr should not be before deqPtr")
  XSError(
    visibleRecvValid && io.recv.bits.instNum > distanceBetween(enqPtr, deqPtr),
    "TraceReader should not read more instructions than buffered"
  )

  for (i <- 0 until trtl.TracePredictWidth - 1) {
    XSError(
      io.traceInsts.valid && io.traceInsts.bits(i + 1).InstID =/= io.traceInsts.bits(i).InstID + 1.U,
      s"TraceReader InstID discontinuity at slot $i"
    )
  }

  val firstInstCheck = RegInit(true.B)
  when(io.traceInsts.valid && firstInstCheck) {
    firstInstCheck := false.B
  }
  XSError(
    io.traceInsts.valid && firstInstCheck && io.traceInsts.bits(0).pcVA =/= 0x80000000L.U,
    "TraceReader first instruction PC is not 0x80000000"
  )
  XSError(
    io.traceInsts.valid && firstInstCheck && io.traceInsts.bits(0).InstID =/= 1.U,
    "TraceReader first instruction InstID is not 1"
  )
}
