// See LICENSE.SiFive for license details.

package device

import chisel3._
import chisel3.util._

import bus.axi4._
import utils._

class AXI4Timer extends Module {
  val io = IO(new Bundle{
    val in = Flipped(new AXI4Lite)
  })

  val in = io.in

  val clk = 50000 // 50MHz / 1000
  val tick = Counter(true.B, clk)._2
  val ms = Counter(tick, 0x40000000)._1

  // deal with non-rready master
  val rInflight = BoolStopWatch(in.ar.fire(), in.r.fire(), startHighPriority = true)
  in.ar.ready := in.r.ready || !rInflight
  in.r.valid := rInflight
  in.r.bits.data := ms
  in.r.bits.resp := AXI4Parameters.RESP_OKAY

  // deal with non-bready master
  val wInflight = BoolStopWatch(in.aw.fire(), in.b.fire(), startHighPriority = true)
  in.aw.ready := in.w.valid && (in.b.ready || !wInflight)
  in.w.ready := in.aw.valid && (in.b.ready || !wInflight)
  in.b.valid := wInflight
  in.b.bits.resp := AXI4Parameters.RESP_OKAY
}
