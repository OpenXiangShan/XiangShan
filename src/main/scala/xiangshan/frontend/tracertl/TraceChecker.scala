package xiangshan.frontend.tracertl

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters

class TraceCheckerIO(implicit p: Parameters) extends TraceBundle {
  val decoded        = Input(Vec(IBufferEnqueueWidth, new TraceDecodedEntry))
  val fixedValid     = Input(UInt(IBufferEnqueueWidth.W))
  val traceForceJump = Input(Bool())
  val consumeValid   = Output(UInt(IBufferEnqueueWidth.W))
}

class TraceChecker(implicit p: Parameters) extends TraceModule {
  val io = IO(new TraceCheckerIO())

  private val compactValid = VecInit(io.decoded.map(_.compact.valid)).asUInt
  io.consumeValid := compactValid & io.fixedValid

  when(io.traceForceJump) {
    io.consumeValid := 1.U
  }
}
