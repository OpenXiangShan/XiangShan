package bus.simplebus

import chisel3._
import chisel3.util._

import bus.axi4._
import utils._

class SimpleBus2AXI4Converter[T <: AXI4Lite](_type: T = new AXI4,
  val dataBits: Int = 32, val userBits: Int = 0) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new SimpleBus(dataBits, userBits))
    val out = Flipped(Flipped(_type))
  })

  val (mem, axi) = (io.in, io.out)
  val (ar, aw, w, r, b) = (axi.ar.bits, axi.aw.bits, axi.w.bits, axi.r.bits, axi.b.bits)

  ar.addr  := mem.req.bits.addr
  ar.prot  := AXI4Parameters.PROT_PRIVILEDGED
  w.data := mem.req.bits.wdata
  w.strb := mem.req.bits.wmask

  def LineBeats = 8
  val wlast = WireInit(true.B)
  val rlast = WireInit(true.B)
  io.out match {
    case axi4: AXI4 =>
      axi4.ar.bits.id    := 0.U
      axi4.ar.bits.len   := Mux(mem.req.bits.burst, (LineBeats - 1).U, 0.U)
      axi4.ar.bits.size  := mem.req.bits.size
      axi4.ar.bits.burst := AXI4Parameters.BURST_WRAP
      axi4.ar.bits.lock  := false.B
      axi4.ar.bits.cache := 0.U
      axi4.ar.bits.qos   := 0.U
      axi4.ar.bits.user  := 0.U
      axi4.w.bits.last   := mem.req.bits.wlast
      wlast := mem.req.bits.wlast
      rlast := axi4.r.bits.last
    case axi4lite: AXI4Lite =>
  }

  aw := ar
  mem.resp.bits.rdata := r.data
  mem.resp.bits.rlast := rlast
  mem.resp.bits.user := 0.U

  val wSend = Wire(Bool())
  val awAck = BoolStopWatch(axi.aw.fire(), wSend)
  val wAck = BoolStopWatch(axi.w.fire() && wlast, wSend)
  wSend := (axi.aw.fire() && axi.w.fire() && wlast) || (awAck && wAck)
  val wen = RegEnable(mem.req.bits.isWrite(), mem.req.fire())

  axi.ar.valid := mem.isRead()
  axi.aw.valid := mem.isWrite() && !awAck
  axi.w .valid := mem.isWrite() && !wAck
  mem.req.ready  := Mux(mem.req.bits.isWrite(), !wAck && axi.w.ready, axi.ar.ready)

  axi.r.ready  := mem.resp.ready
  axi.b.ready  := mem.resp.ready
  mem.resp.valid  := Mux(wen, axi.b.valid, axi.r.valid)
}
