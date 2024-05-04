package utils

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.Bundles.ExuInput

/** Pipeline module generator parameterized by data type and latency.
  *
  * @param gen a Chisel type, used as data in pipe
  * @param flushGen a Chisel type, used as flush signal
  * @param latency the number of pipeline stages
  * @param flushFunc used to generate flush signal
  * @tparam T Type of [[io.enq.bits]] and [[io.deq.bits]]
  * @tparam TFlush Type of [[io.flush]]
  */
class PipeWithFlush[T <: Data, TFlush <: Data] (
  gen: ExuInput,
  flushGen: TFlush,
  latency: Int,
  flushFunc: (ExuInput, TFlush, Int) => Bool,
  modificationFunc: ExuInput => ExuInput = { x: ExuInput => x }
) extends Module {
  require(latency >= 0, "Pipe latency must be greater than or equal to zero!")

  class PipeIO extends Bundle {
    val flush = Input(flushGen)
    val enq = Input(Valid(gen))
    val deq = Output(Valid(gen))
  }

  val io = IO(new PipeIO)

  val valids: Seq[Bool] = io.enq.valid +: Seq.fill(latency)(RegInit(false.B))
  val bits: Seq[ExuInput] = io.enq.bits +: Seq.fill(latency)(Reg(gen))
  val modifiedBits: Seq[ExuInput] = bits.map(modificationFunc)

  for (i <- 0 until latency) {
    valids(i + 1) := (if (i==0) valids(i) else valids(i) && !flushFunc(bits(i), io.flush, i))
    when(valids(i)) {
      bits(i + 1) := (if (i==0) bits(i) else modifiedBits(i))
    }
  }
  io.deq.valid := valids.last
  io.deq.bits := bits.last
}
