package xiangshan.frontend.tracertl

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.ParallelPosteriorityMux

class TraceDriverOutput(implicit p: Parameters) extends TraceBundle {
  val block      = Output(Bool())
  val recv       = ValidIO(new TraceRecvInfo())
  val endWithCFI = Output(Bool())
}

class TraceDriverIO(implicit p: Parameters) extends TraceBundle {
  val fire           = Input(Bool())
  val decoded        = Input(Vec(IBufferEnqueueWidth, new TraceDecodedEntry))
  val fixedValid     = Input(UInt(IBufferEnqueueWidth.W))
  val traceForceJump = Input(Bool())
  val concede2Bytes  = Input(Bool())
  val out            = new TraceDriverOutput()
}

class TraceDriver(implicit p: Parameters) extends TraceModule {
  val io = IO(new TraceDriverIO())

  // The checker's fixed range only covers valid instructions; a force jump consumes just the first one.
  val compactValid = VecInit(io.decoded.map(_.compact.valid)).asUInt
  val finalValid   = Mux(io.traceForceJump, compactValid & 1.U, io.fixedValid)
  val recvInstNum  = PopCount(finalValid)
  val pcMismatch   = recvInstNum === 0.U

  io.out.block             := pcMismatch && !io.concede2Bytes
  io.out.recv.valid        := io.fire
  io.out.recv.bits.instNum := recvInstNum
  io.out.endWithCFI := Mux(
    finalValid.orR,
    ParallelPosteriorityMux(
      finalValid,
      io.decoded.map(entry => (entry.compact.traceInfo.branchType =/= 0.U) && entry.compact.traceInfo.isTaken)
    ),
    false.B
  )
}
