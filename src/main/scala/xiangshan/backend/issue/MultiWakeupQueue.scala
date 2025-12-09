package xiangshan.backend.issue

import chisel3._
import chisel3.util._
import com.typesafe.scalalogging.LazyLogging
import utils.PipeWithFlush
import xiangshan.backend.Bundles.{ExuInput, connectSamePort}
import xiangshan.backend.exu.ExeUnitParams

class MultiWakeupQueueIO[T <: Bundle, TFlush <: Data](
  gen       : ExuInput,
  lastGen   : ExuInput,
  flushGen  : TFlush,
  latWidth  : Int,
) extends Bundle {
  class EnqBundle extends Bundle {
    val uop = Output(gen)
    val lat = Output(UInt(latWidth.W))
  }

  val flush = Input(flushGen)
  val enq = Flipped(Valid(new EnqBundle))
  // i2f and fdiv use enqAppend
  val enqAppend = Flipped(DecoupledIO(new EnqBundle))
  val deq = Output(Valid(lastGen))
}

class MultiWakeupQueue[T <: Bundle, TFlush <: Data](
  val exuParam  : ExeUnitParams,
  val gen       : ExuInput,
  val lastGen   : ExuInput,
  val flushGen  : TFlush,
  val latencySet: Set[Int],
  flushFunc : (ExuInput, TFlush, Int) => Bool,
  modificationFunc: ExuInput => ExuInput = { x: ExuInput => x },
  lastConnectFunc: (ExuInput, ExuInput) => ExuInput,
) extends Module with LazyLogging {
  logger.debug(s"$exuParam")
  logger.debug(s"$latencySet")
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
    case (deq, 0) => deq.valid
    case (deq, i) => deq.valid && !flushFunc(deq.bits, io.flush, i)
  }))
  private val pipesBitsVec = VecInit(pipes.map(_.io.deq.bits).zip(latencySet).map(_ match {
    case (deq, 0) => deq
    case (deq, i) => modificationFunc(deq)
  }))
  // for i2f in fp iq, donot need && !pipesValidVec.asUInt.orR, for fdiv, need !pipesValidVec.asUInt.orR
  val allValidVec = VecInit(pipesValidVec :+ (io.enqAppend.valid && !pipesValidVec.asUInt.orR))
  val allBitsVec = VecInit(pipesBitsVec :+ io.enqAppend.bits.uop)
  io.enqAppend.ready := Mux(io.enqAppend.valid, !pipesValidVec.asUInt.orR, true.B)
  pipesOut.valid := allValidVec.asUInt.orR
  pipesOut.bits := Mux1H(allValidVec, allBitsVec)
  pipesOut.bits.rfWen .foreach(_ := allValidVec.zip(allBitsVec.map(_.rfWen .get)).map{case(valid,wen) => valid && wen}.reduce(_||_))
  pipesOut.bits.fpWen .foreach(_ := allValidVec.zip(allBitsVec.map(_.fpWen .get)).map{case(valid,wen) => valid && wen}.reduce(_||_))
  pipesOut.bits.vecWen.foreach(_ := allValidVec.zip(allBitsVec.map(_.vecWen.get)).map{case(valid,wen) => valid && wen}.reduce(_||_))
  pipesOut.bits.v0Wen .foreach(_ := allValidVec.zip(allBitsVec.map(_.v0Wen .get)).map{case(valid,wen) => valid && wen}.reduce(_||_))
  pipesOut.bits.vlWen .foreach(_ := allValidVec.zip(allBitsVec.map(_.vlWen .get)).map{case(valid,wen) => valid && wen}.reduce(_||_))

  lastConnect.valid := pipesOut.valid
  lastConnect.bits := lastConnectFunc(pipesOut.bits, lastConnect.bits)

  io.deq.valid := lastConnect.valid
  io.deq.bits := lastConnect.bits

  assert(PopCount(allValidVec) <= 1.U, "PopCount(allValidVec) should be no more than 1")
}
