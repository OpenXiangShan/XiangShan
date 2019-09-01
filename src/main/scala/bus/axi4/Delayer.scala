// See LICENSE.SiFive for license details.

package bus.axi4

import chisel3._
import chisel3.util._
import utils._

class AXI4Delayer[T <: AXI4Lite](latency: Int = 0, _type: T = new AXI4) extends Module {
  val io = IO(new Bundle{
    val in = Flipped(_type)
    val out = Flipped(Flipped(_type))
  })

  io.out.ar <> LatencyPipe(io.in.ar, latency)
  io.out.aw <> LatencyPipe(io.in.aw, latency)
  io.out.w  <> io.in.w
  io.in.b   <> io.out.b
  io.in.r   <> io.out.r
}
