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

class TraceReaderHelperWrapper(implicit p: Parameters) extends TraceModule {
  val io = IO(new Bundle {
    val enable         = Input(Bool())
    val instsReady     = Output(Bool())
    val insts          = Output(Vec(trtl.TraceFetchWidth, new TraceInstrInnerBundle()))
    val redirectValid  = Input(Bool())
    val redirectInstID = Input(UInt(trtl.TraceInstIDWidth.W))
    val workingState   = Input(Bool())
  })

  val traceReaderHelper = Module(new TraceReaderHelper(trtl.TraceFetchWidth))
  traceReaderHelper.clock  := clock
  traceReaderHelper.reset  := reset
  traceReaderHelper.enable := io.enable && io.workingState

  val traceRedirectHelper = Module(new TraceRedirectHelper)
  traceRedirectHelper.clock               := clock
  traceRedirectHelper.reset               := reset
  traceRedirectHelper.enable              := io.redirectValid && io.workingState
  traceRedirectHelper.InstID              := io.redirectInstID
  traceRedirectHelper.preserveDriveBefore := false.B

  private def narrow(raw: UInt, width: Int): UInt = raw(width - 1, 0)

  io.insts.zipWithIndex.foreach { case (inst, i) =>
    inst.pcVA           := narrow(traceReaderHelper.insts_pcVA(i), inst.pcVA.getWidth)
    inst.pcPA           := narrow(traceReaderHelper.insts_pcPA(i), inst.pcPA.getWidth)
    inst.memoryAddrVA   := narrow(traceReaderHelper.insts_memoryAddrVA(i), inst.memoryAddrVA.getWidth)
    inst.memoryAddrPA   := narrow(traceReaderHelper.insts_memoryAddrPA(i), inst.memoryAddrPA.getWidth)
    inst.target         := narrow(traceReaderHelper.insts_target(i), inst.target.getWidth)
    inst.inst           := narrow(traceReaderHelper.insts_inst(i), inst.inst.getWidth)
    inst.memoryType     := narrow(traceReaderHelper.insts_memoryType(i), inst.memoryType.getWidth)
    inst.memorySize     := narrow(traceReaderHelper.insts_memorySize(i), inst.memorySize.getWidth)
    inst.branchType     := narrow(traceReaderHelper.insts_branchType(i), inst.branchType.getWidth)
    inst.branchTaken    := narrow(traceReaderHelper.insts_branchTaken(i), inst.branchTaken.getWidth)
    inst.exception      := narrow(traceReaderHelper.insts_exception(i), inst.exception.getWidth)
    inst.fastSimulation := narrow(traceReaderHelper.insts_fastSimulation(i), inst.fastSimulation.getWidth)
    inst.InstID         := narrow(traceReaderHelper.insts_InstID(i), inst.InstID.getWidth)
  }
  io.instsReady := true.B
}

class TraceReader(implicit p: Parameters) extends TraceModule with HasCircularQueuePtrHelper {
  val io = IO(new TraceReaderIO())

  val traceBuffer       = RegInit(0.U.asTypeOf(Vec(trtl.TraceBufferSize, new TraceInstrBundle())))
  val traceReaderHelper = Module(new TraceReaderHelperWrapper())
  val enqPtr            = RegInit(0.U.asTypeOf(new TraceBufferPtr(trtl.TraceBufferSize)))
  val deqPtr            = RegInit(0.U.asTypeOf(new TraceBufferPtr(trtl.TraceBufferSize)))

  val workingState = RegInit(false.B)
  val startCount   = RegInit(0.U(4.W))
  when(startCount < 10.U) {
    startCount := startCount + 1.U
  }
  when(startCount === 5.U) {
    workingState := true.B
  }

  val redirectValid = io.redirect.valid

  val readTraceEnable = !isFull(enqPtr, deqPtr) &&
    hasFreeEntries(enqPtr, deqPtr) >= trtl.TraceFetchWidth.U &&
    workingState &&
    !redirectValid
  val readTraceReady = traceReaderHelper.io.instsReady

  traceReaderHelper.io.enable        := readTraceEnable
  traceReaderHelper.io.redirectValid := redirectValid && workingState
  // Exceptions and synthetic force-jumps transfer control after the current
  // trace instruction even when the backend redirect flushes the ROB entry.
  val redirectFlushItself = RedirectLevel.flushItself(io.redirect.bits.level) &&
    !io.redirect.bits.traceInfo.get.isForceJump &&
    !io.redirect.bits.traceInfo.get.hasException
  val redirectInstID = Mux(
    redirectFlushItself,
    io.redirect.bits.traceInfo.get.InstID,
    io.redirect.bits.traceInfo.get.InstID + 1.U
  )(trtl.TraceInstIDWidth - 1, 0)
  traceReaderHelper.io.redirectInstID := redirectInstID
  traceReaderHelper.io.workingState   := workingState

  val enqPtrVec = Wire(Vec(trtl.TraceFetchWidth, new TraceBufferPtr(trtl.TraceBufferSize)))
  enqPtrVec.zipWithIndex.foreach { case (ptr, i) =>
    ptr := enqPtr + i.U
  }

  when(readTraceEnable && readTraceReady) {
    (0 until trtl.TraceFetchWidth).foreach { i =>
      traceBuffer(enqPtrVec(i).value) := TraceInstrBundle(traceReaderHelper.io.insts(i))
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
