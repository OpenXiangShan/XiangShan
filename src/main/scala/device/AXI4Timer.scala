// See LICENSE.SiFive for license details.

package device

import chisel3._
import chisel3.util._

import bus.axi4.{AXI4, AXI4Parameters}

class AXI4Timer() extends Module {
  val io = IO(new Bundle{
    val in = Flipped(new AXI4)
  })

  val in = io.in

  val clk = 50000 // 50MHz / 1000
  val tick = Counter(true.B, clk)._2
  val ms = Counter(tick, 0x40000000)._1

  // deal with non-rready master
  val rInflight = RegInit(false.B)
  when (in.ar.fire()) { rInflight := true.B }
  when (in. r.fire()) { rInflight := false.B }

  val rId = RegEnable(in.ar.bits.id, in.ar.fire())
  val rUser = RegEnable(in.ar.bits.user, in.ar.fire())
  in.ar.ready := in.r.ready || !rInflight
  in.r.valid := rInflight
  in.r.bits.id := rId
  in.r.bits.user := rUser
  in.r.bits.data := ms
  in.r.bits.resp := AXI4Parameters.RESP_OKAY
  in.r.bits.last := true.B

  // deal with non-bready master
  val wInflight = RegInit(false.B)
  when (in.aw.fire()) { wInflight := true.B }
  when (in. b.fire()) { wInflight := false.B }

  val bId = RegEnable(in.aw.bits.id, in.aw.fire())
  val bUser = RegEnable(in.aw.bits.user, in.aw.fire())
  in.aw.ready := in.w.valid && (in.b.ready || !wInflight)
  in.w.ready := in.aw.valid && (in.b.ready || !wInflight)
  in.b.valid := wInflight
  in.b.bits.id := bId
  in.b.bits.user := bUser
  in.b.bits.resp := AXI4Parameters.RESP_OKAY
}

object TopAXI4Timer extends App {
  Driver.execute(args, () => new AXI4Timer)
}
