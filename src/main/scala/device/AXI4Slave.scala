package device

import chisel3._
import chisel3.util._

import bus.axi4._
import utils._

abstract class AXI4SlaveModule[T <: AXI4Lite](_type :T = new AXI4) extends Module {
  val io = IO(new Bundle{ val in = Flipped(_type) })
  val in = io.in

  val raddr = Wire(UInt())
  val (readBeatCnt, rLast) = in match {
    case axi4: AXI4 =>
      val c = Counter(256)
      val len = Mux(axi4.ar.fire(), axi4.ar.bits.len, RegEnable(axi4.ar.bits.len, axi4.ar.fire()))
      raddr := Mux(axi4.ar.fire(), axi4.ar.bits.addr, RegEnable(axi4.ar.bits.addr, axi4.ar.fire()))
      axi4.r.bits.last := (c.value === len)
      when (axi4.r.fire()) {
        c.inc()
        when (axi4.r.bits.last) { c.value := 0.U }
      }
      (Mux(axi4.r.fire(), c.value + 1.U, c.value), axi4.r.bits.last)

    case axi4lite: AXI4Lite =>
      raddr := axi4lite.ar.bits.addr
      (0.U, true.B)
  }

  val r_busy = BoolStopWatch(in.ar.fire(), in.r.fire() && rLast, startHighPriority = true)
  in.ar.ready := in.r.ready || !r_busy
  in.r.bits.resp := AXI4Parameters.RESP_OKAY
  val ren = in.ar.fire() || (in.r.fire() && !rLast)
  in.r.valid := BoolStopWatch(ren && (in.ar.fire() || r_busy), in.r.fire(), startHighPriority = true)


  val waddr = Wire(UInt())
  val (writeBeatCnt, wLast) = in match {
    case axi4: AXI4 =>
      val c = Counter(256)
      waddr := Mux(axi4.aw.fire(), axi4.aw.bits.addr, RegEnable(axi4.aw.bits.addr, axi4.aw.fire()))
      when (axi4.w.fire()) {
        c.inc()
        when (axi4.w.bits.last) { c.value := 0.U }
      }
      (c.value, axi4.w.bits.last)

    case axi4lite: AXI4Lite =>
      waddr := axi4lite.aw.bits.addr
      (0.U, true.B)
  }

  val w_busy = BoolStopWatch(in.aw.fire(), in.b.fire(), startHighPriority = true)
  in.aw.ready := !w_busy
  in. w.ready := in.aw.valid || (w_busy)
  in.b.bits.resp := AXI4Parameters.RESP_OKAY
  in.b.valid := BoolStopWatch(in.w.fire() && wLast, in.b.fire(), startHighPriority = true)

  in match {
    case axi4: AXI4 =>
      axi4.b.bits.id   := RegEnable(axi4.aw.bits.id, axi4.aw.fire())
      axi4.b.bits.user := RegEnable(axi4.aw.bits.user, axi4.aw.fire())
      axi4.r.bits.id   := RegEnable(axi4.ar.bits.id, axi4.ar.fire())
      axi4.r.bits.user := RegEnable(axi4.ar.bits.user, axi4.ar.fire())
    case axi4lite: AXI4Lite =>
  }
}
