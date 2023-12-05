package xiangshan.backend.issue

import chisel3._
import chisel3.util._
import utils.PipeWithFlush
import xiangshan.backend.Bundles.{ExuInput, connectSamePort}
import xiangshan.backend.exu.ExeUnitParams

class MultiWakeupQueueIO[T <: Bundle, TFlush <: Data](
  gen       : T,
  lastGen   : T,
  flushGen  : TFlush,
  latWidth  : Int,
) extends Bundle {
  class EnqBundle extends Bundle {
    val uop = Output(gen)
    val lat = Output(UInt(latWidth.W))
  }

  val flush = Input(flushGen)
  val enq = Flipped(Valid(new EnqBundle))
  val deq = Output(Valid(lastGen))
}

class MultiWakeupQueue[T <: Bundle, TFlush <: Data](
  val gen       : T,
  val lastGen   : T,
  val flushGen  : TFlush,
  val latencySet: Set[Int],
  flushFunc : (T, TFlush, Int) => Bool,
  modificationFunc: T => T = { x: T => x },
  lastConnectFunc: (T, T) => T,
) extends Module {
  require(latencySet.min >= 0)

  val io = IO(new MultiWakeupQueueIO(gen, lastGen, flushGen, log2Up(latencySet.max) + 1))

  val pipes = latencySet.map(x => Module(new PipeWithFlush[T, TFlush](gen, flushGen, x, flushFunc, modificationFunc))).toSeq

  val pipesOut = Wire(Valid(gen))
  val lastConnect = Reg(Valid(lastGen))

  pipes.zip(latencySet).foreach {
    case (pipe, lat) =>
      pipe.io.flush := io.flush
      pipe.io.enq.valid := io.enq.valid && io.enq.bits.lat === lat.U
      pipe.io.enq.bits := io.enq.bits.uop
  }

  private val pipesValidVec = VecInit(pipes.map(_.io.deq).zip(latencySet).map(_ match {
    case (deq, i) => deq.valid && !flushFunc(deq.bits, io.flush, i)
  }))
  private val pipesBitsVec = VecInit(pipes.map(_.io.deq.bits)).map(modificationFunc)

  pipesOut.valid := pipesValidVec.asUInt.orR
  pipesOut.bits := Mux1H(pipesValidVec, pipesBitsVec)

  lastConnect.valid := pipesOut.valid
  lastConnect.bits := lastConnectFunc(pipesOut.bits, lastConnect.bits)

  io.deq.valid := lastConnect.valid
  io.deq.bits := lastConnect.bits

  assert(PopCount(pipesValidVec) <= 1.U, "PopCount(pipesValidVec) should be no more than 1")
}
