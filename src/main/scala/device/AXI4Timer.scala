// See LICENSE.SiFive for license details.

package device

import chisel3._
import chisel3.util._

import memory.{AXI4, AXI4Parameters}

class AXI4Timer() extends Module {
  val io = IO(new Bundle{
    val in = Flipped(new AXI4)
  })

  val in = io.in

  val clk = 50000 // 50MHz / 1000
  val tick = Counter(true.B, clk)._2
  val ms = Counter(tick, 0x40000000)._1

  in.ar.ready := true.B
  in.aw.ready := true.B
  in.w.ready := true.B

  // should deal with non-ready master
  in.b.valid := RegNext(in.aw.fire())
  in.r.valid := RegNext(in.ar.fire())

  in.r.bits.data := ms
  in.r.bits.id := RegNext(in.ar.bits.id)
  in.r.bits.user := RegNext(in.ar.bits.user)
  in.r.bits.resp := AXI4Parameters.RESP_OKAY
  in.r.bits.last := true.B
  in.b.bits.id := RegNext(in.aw.bits.id)
  in.b.bits.user := RegNext(in.aw.bits.user)
  in.b.bits.resp := AXI4Parameters.RESP_OKAY
}

object TopAXI4Timer extends App {
  Driver.execute(args, () => new AXI4Timer)
}
