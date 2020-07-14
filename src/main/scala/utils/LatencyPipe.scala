// See LICENSE.Berkeley for license details.

package utils

import Chisel._

class LatencyPipe[T <: Data](typ: T, latency: Int) extends Module {
  val io = new Bundle {
    val in = Decoupled(typ).flip
    val out = Decoupled(typ)
  }

  def doN[T](n: Int, func: T => T, in: T): T =
    (0 until n).foldLeft(in)((last, _) => func(last))

  io.out <> doN(latency, (last: DecoupledIO[T]) => Queue(last, 1, pipe=true), io.in)
}

object LatencyPipe {
  def apply[T <: Data](in: DecoupledIO[T], latency: Int): DecoupledIO[T] = {
    val pipe = Module(new LatencyPipe(in.bits, latency))
    pipe.io.in <> in
    pipe.io.out
  }
}
