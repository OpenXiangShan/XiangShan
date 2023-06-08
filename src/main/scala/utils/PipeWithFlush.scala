package utils

import chisel3._
import chisel3.util._
import top.{ArgParser, BaseConfig, DefaultConfig}
import xiangshan._
import xiangshan.backend.Bundles.DynInst

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
  val gen: T,
  val flushGen: TFlush,
  val latency: Int,
  flushFunc: (T, TFlush) => Bool
) extends Module {
  require(latency >= 0, "Pipe latency must be greater than or equal to zero!")

  class PipeIO extends Bundle {
    val flush = Flipped(flushGen)
    val enq = Input(Valid(gen))
    val deq = Output(Valid(gen))
  }

  val io = IO(new PipeIO)

  if (latency == 0) {
    io.deq := io.enq
  } else {
    val valids: Seq[Bool] = io.enq.valid +: Seq.fill(latency)(RegInit(false.B))
    val bits: Seq[T] = io.enq.bits +: Seq.fill(latency)(Reg(gen))

    for (i <- 0 until latency) {
      valids(i + 1) := valids(i) && !flushFunc(bits(i), io.flush)
      when (valids(i)) {
        bits(i + 1) := bits(i)
      }
    }
    io.deq.valid := valids.last
    io.deq.bits := bits.last
  }
}
