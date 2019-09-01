package bus.simplebus

import chisel3._
import chisel3.util._

import bus.axi4._
import utils._

class SimpleBus2AXI4Converter[IT <: SimpleBusUL, OT <: AXI4Lite]
  (inType: IT, outType: OT) extends Module {

  val ULtoAXI4Lite = (inType.getClass == classOf[SimpleBusUL]) && (outType.getClass == classOf[AXI4Lite])
  val UHtoAXI4 = (inType.getClass == classOf[SimpleBusUH]) && (outType.getClass == classOf[AXI4])
  require(ULtoAXI4Lite || UHtoAXI4)

  val io = IO(new Bundle {
    val in = Flipped(inType)
    val out = Flipped(Flipped(outType))
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
  if (UHtoAXI4) {
    val axi4 = io.out.asInstanceOf[AXI4]
    val uh = io.in.asInstanceOf[SimpleBusUH]
    axi4.ar.bits.id    := 0.U
    axi4.ar.bits.len   := Mux(uh.req.bits.burst, (LineBeats - 1).U, 0.U)
    axi4.ar.bits.size  := uh.req.bits.size
    axi4.ar.bits.burst := AXI4Parameters.BURST_WRAP
    axi4.ar.bits.lock  := false.B
    axi4.ar.bits.cache := 0.U
    axi4.ar.bits.qos   := 0.U
    axi4.ar.bits.user  := 0.U
    axi4.w.bits.last   := uh.req.bits.wlast
    uh.resp.bits.rlast := rlast
    wlast := uh.req.bits.wlast
    rlast := axi4.r.bits.last
  }

  aw := ar
  mem.resp.bits.rdata := r.data
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
