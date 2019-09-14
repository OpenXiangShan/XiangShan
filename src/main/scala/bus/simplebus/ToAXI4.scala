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
    val in = Flipped(chiselTypeOf(inType))
    val out = Flipped(Flipped(outType))
  })

  val (mem, axi) = (io.in, io.out)
  val (ar, aw, w, r, b) = (axi.ar.bits, axi.aw.bits, axi.w.bits, axi.r.bits, axi.b.bits)

  ar.addr  := mem.req.bits.addr
  ar.prot  := AXI4Parameters.PROT_PRIVILEDGED
  w.data := mem.req.bits.wdata
  w.strb := mem.req.bits.wmask

  def LineBeats = 4 //Note: LineBeats = 8 while using rv32 inst set 
  val wlast = WireInit(true.B)
  val rlast = WireInit(true.B)
  if (UHtoAXI4) {
    val axi4 = io.out.asInstanceOf[AXI4]
    val uh = io.in.asInstanceOf[SimpleBusUH]

  Debug(true){
    when(axi.ar.valid && axi.ar.ready){
      printf("[AXI] araddr: %x len: %x size: %x\n", ar.addr, axi4.ar.bits.len, axi4.ar.bits.size)
    }
    when(axi.ar.valid){
      printf("[AXI] ar_req araddr: %x len: %x size: %x\n", ar.addr, axi4.ar.bits.len, axi4.ar.bits.size)
    }

    when(axi.aw.valid && axi.aw.ready){
      printf("[AXI] awaddr: %x len: %x size: %x\n", aw.addr, axi4.aw.bits.len, axi4.aw.bits.size)
    }

    when(axi.r.ready && axi.r.valid){
      printf("[AXI] rdata: %x rlast: %b\n", r.data,axi4.r.bits.last)
    }

    when(axi.w.ready && axi.w.valid){
      printf("[AXI] wdata: %x wstrb: %x wlast: %b\n", w.data, w.strb, axi4.w.bits.last)
    }
  }

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

  Debug(false){
    printf("[CVT] isWrite %x wAck %x wr %x arr %x addr %x\n", mem.req.bits.isWrite(), wAck, axi.w.ready, axi.ar.ready, mem.req.bits.addr)
  }

  Debug(true){
    when((ar.addr(31,4) === "h8010f00".U)&&(axi.ar.valid || axi.aw.valid)){
      printf("[AXI] TIME %d addr: %x arv %x awv %x\n", GTimer(), ar.addr, axi.ar.valid, axi.aw.valid)
    }
  }

  Debug(true){
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
  def apply[IT <: SimpleBusUL, OT <: AXI4Lite](inType: IT, outType: OT): OT = {
    val bridge = Module(new SimpleBus2AXI4Converter(inType, outType))
    bridge.io.in <> inType
    bridge.io.out
  }
}
