package bus.simplebus

import chisel3._
import chisel3.util._

import bus.axi4._

class SimpleBus2AXI4Converter extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new SimpleBus)
    val out = new AXI4
  })

  val mem = io.in
  val axi = io.out

  val ar = axi.ar.bits
  val aw = axi.aw.bits
  val w  = axi.w.bits
  val r  = axi.r.bits
  val b  = axi.b.bits

  ar.id    := 0.U
  ar.addr  := mem.req.bits.addr
  ar.len   := 0.U  // single beat
  ar.size  := mem.req.bits.size
  ar.burst := AXI4Parameters.BURST_INCR
  ar.lock  := false.B
  ar.cache := 0.U
  ar.prot  := AXI4Parameters.PROT_PRIVILEDGED
  ar.qos   := 0.U
  ar.user  := 0.U
  aw := ar
  w.data := mem.req.bits.wdata
  w.strb := mem.req.bits.wmask
  w.last := true.B
  mem.resp.bits.rdata := r.data

  val awAck = RegInit(false.B)
  val wAck = RegInit(false.B)

  val wen = RegEnable(mem.req.bits.wen, mem.req.fire())
  val wSend = (axi.aw.fire() && axi.w.fire()) || (awAck && wAck)
  when (wSend) {
    awAck := false.B
    wAck := false.B
  }
  .elsewhen (axi.aw.fire()) { awAck := true.B }
  .elsewhen (axi. w.fire()) {  wAck := true.B }

  axi.ar.valid := mem.isRead()
  axi.aw.valid := mem.isWrite() && !awAck
  axi.w .valid := mem.isWrite() && !wAck
  mem.req.ready  := Mux(mem.req.bits.wen, wSend, axi.ar.ready)

  axi.r.ready  := mem.resp.ready
  axi.b.ready  := mem.resp.ready
  mem.resp.valid  := Mux(wen, axi.b.valid, axi.r.valid)
}
