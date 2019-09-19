package bus.simplebus

import chisel3._
import chisel3.util._

import bus.axi4._
import utils._

class SimpleBus2AXI4Converter[OT <: AXI4Lite](outType: OT) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new SimpleBusUC)
    val out = Flipped(Flipped(outType))
  })

  val toAXI4Lite = !(io.in.req.valid && io.in.req.bits.isBurst()) && (outType.getClass == classOf[AXI4Lite]).B
  val toAXI4 = !(io.in.req.valid && !io.in.req.bits.isBurst()) && (outType.getClass == classOf[AXI4]).B
  assert(toAXI4Lite || toAXI4)

  val (mem, axi) = (io.in, io.out)
  val (ar, aw, w, r, b) = (axi.ar.bits, axi.aw.bits, axi.w.bits, axi.r.bits, axi.b.bits)

  ar.addr  := mem.req.bits.addr
  ar.prot  := AXI4Parameters.PROT_PRIVILEDGED
  w.data := mem.req.bits.wdata
  w.strb := mem.req.bits.wmask

  def LineBeats = 4 //Note: LineBeats = 8 while using rv32 inst set 
  val wlast = WireInit(true.B)
  val rlast = WireInit(true.B)
  if (outType.getClass == classOf[AXI4]) {
    val axi4 = io.out.asInstanceOf[AXI4]
    axi4.ar.bits.id    := 0.U
    axi4.ar.bits.len   := Mux(mem.req.bits.isBurst(), (LineBeats - 1).U, 0.U)
    axi4.ar.bits.size  := mem.req.bits.size
    axi4.ar.bits.burst := AXI4Parameters.BURST_WRAP
    axi4.ar.bits.lock  := false.B
    axi4.ar.bits.cache := 0.U
    axi4.ar.bits.qos   := 0.U
    axi4.ar.bits.user  := 0.U
    axi4.w.bits.last   := mem.req.bits.isWriteLast()
    wlast := axi4.w.bits.last
    rlast := axi4.r.bits.last
  }

  aw := ar
  mem.resp.bits.rdata := r.data
  mem.resp.bits.cmd  := Mux(rlast, SimpleBusCmd.readLast, 0.U)
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

  Debug(false){
    printf("[CVT] isWrite %x wAck %x wr %x arr %x addr %x\n", mem.req.bits.isWrite(), wAck, axi.w.ready, axi.ar.ready, mem.req.bits.addr)
  }

  Debug(false){
    when((ar.addr(31,4) === "h8010f00".U)&&(axi.ar.valid || axi.aw.valid)){
      printf("[AXI] TIME %d addr: %x arv %x awv %x\n", GTimer(), ar.addr, axi.ar.valid, axi.aw.valid)
    }
  }

  Debug(false){
    when((w.data(31,0) === "h18be6784".U)&& axi.w.valid){
      printf("[AXI] TIME %d wdata: %x wr: %x\n", GTimer(), w.data, axi.w.ready)
    }
    when((w.data(63,32) === "h18be6784".U)&& axi.w.valid){
      printf("[AXI] TIME %d wdata: %x wr: %x\n", GTimer(), w.data, axi.w.ready)
    }
  }

  axi.r.ready  := mem.resp.ready
  axi.b.ready  := mem.resp.ready
  mem.resp.valid  := Mux(wen, axi.b.valid, axi.r.valid)
}

object SimpleBus2AXI4Converter {
  def apply[OT <: AXI4Lite](in: SimpleBusUC, outType: OT): OT = {
    val bridge = Module(new SimpleBus2AXI4Converter(outType))
    bridge.io.in <> in
    bridge.io.out
  }
}
