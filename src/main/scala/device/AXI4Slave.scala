package device

import chisel3._
import chisel3.util._

import bus.axi4._
import utils._

abstract class AXI4SlaveModule[T <: AXI4Lite](_type :T = new AXI4) extends Module {
  val io = IO(new Bundle{ val in = Flipped(_type) })
  val in = io.in

  val w_full = BoolStopWatch(in.aw.fire(), in.b.fire(), startHighPriority = true)
  in. b.valid := w_full
  in.aw.ready := in. w.valid && (in.b.ready || !w_full)
  in. w.ready := in.aw.valid && (in.b.ready || !w_full)
  in.b.bits.resp := AXI4Parameters.RESP_OKAY

  val r_full = BoolStopWatch(in.ar.fire(), in.r.fire(), startHighPriority = true)
  in. r.valid := r_full
  in.ar.ready := in.r.ready || !r_full
  in.r.bits.resp := AXI4Parameters.RESP_OKAY

  in match {
    case axi4: AXI4 =>
      axi4.b.bits.id   := RegEnable(axi4.aw.bits.id, axi4.aw.fire())
      axi4.b.bits.user := RegEnable(axi4.aw.bits.user, axi4.aw.fire())
      axi4.r.bits.id   := RegEnable(axi4.ar.bits.id, axi4.ar.fire())
      axi4.r.bits.user := RegEnable(axi4.ar.bits.user, axi4.ar.fire())
      axi4.r.bits.last := true.B
    case axi4lite: AXI4Lite =>
  }
}
