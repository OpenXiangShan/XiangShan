package xiangshan.backend.issue

import chisel3._
import chisel3.util._
import utils.PipeWithFlush

class MultiWakeupQueueIO[T <: Data, TFlush <: Data](
  gen       : T,
  flushGen  : TFlush,
  latWidth  : Int,
) extends Bundle {
  class EnqBundle extends Bundle {
    val uop = Output(gen)
    val lat = Output(UInt(latWidth.W))
  }

  val flush = Input(flushGen)
  val enq = Flipped(Valid(new EnqBundle))
  val og0IssueFail = Input(Bool())
  val og1IssueFail = Input(Bool())
  val deq = Output(Valid(gen))
}

class MultiWakeupQueue[T <: Data, TFlush <: Data](
  val gen       : T,
  val flushGen  : TFlush,
  val latencySet: Set[Int],
  flushFunc : (T, TFlush, Int) => Bool,
  modificationFunc: T => T = { x: T => x }
) extends Module {
  require(latencySet.min >= 0)

  val io = IO(new MultiWakeupQueueIO(gen, flushGen, log2Up(latencySet.max) + 1))

  val pipes = latencySet.map(x => Module(new PipeWithFlush[T, TFlush](gen, flushGen, x, flushFunc, modificationFunc))).toSeq

  pipes.zip(latencySet).foreach {
    case (pipe, lat) =>
      pipe.io.flush := io.flush
      pipe.io.enq.valid := io.enq.valid && io.enq.bits.lat === lat.U
      pipe.io.enq.bits := io.enq.bits.uop
  }

  private val pipesValidVec = VecInit(pipes.map(_.io.deq.valid).zip(latencySet).map(_ match {
    case (valid, 1) => valid && !io.og0IssueFail
    case (valid, 2) => valid && !io.og1IssueFail
    case (valid, _) => valid
  }))
  private val pipesBitsVec = VecInit(pipes.map(_.io.deq.bits))

  io.deq.valid := pipesValidVec.asUInt.orR
  io.deq.bits := Mux1H(pipesValidVec, pipesBitsVec)

  assert(PopCount(pipesValidVec) <= 1.U, "PopCount(pipesValidVec) should be no more than 1")
}
