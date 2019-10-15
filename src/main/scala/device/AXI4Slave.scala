package device

import chisel3._
import chisel3.util._

import noop.HasNOOPParameter
import bus.axi4._
import utils._

abstract class AXI4SlaveModule[T <: AXI4Lite, B <: Data](_type :T = new AXI4, _extra: B = null)
  extends Module with HasNOOPParameter {
  val io = IO(new Bundle{
    val in = Flipped(_type)
    val extra = if (_extra != null) Some(Flipped(Flipped(_extra))) else None
  })
  val in = io.in

  val fullMask = MaskExpand(in.w.bits.strb)
  def genWdata(originData: UInt) = (originData & ~fullMask) | (in.w.bits.data & fullMask)

  val raddr = Wire(UInt())
  val ren = Wire(Bool())
  val (readBeatCnt, rLast) = in match {
    case axi4: AXI4 =>
      val c = Counter(256)
      val beatCnt = Counter(256)
      val len = HoldUnless(axi4.ar.bits.len, axi4.ar.fire())
      val burst = HoldUnless(axi4.ar.bits.burst, axi4.ar.fire())
      val wrapAddr = axi4.ar.bits.addr & ~(axi4.ar.bits.len.asTypeOf(UInt(AddrBits.W)) << axi4.ar.bits.size)
      raddr := HoldUnless(wrapAddr, axi4.ar.fire())
      axi4.r.bits.last := (c.value === len)
      when (ren) {
        beatCnt.inc()
        when (burst === AXI4Parameters.BURST_WRAP && beatCnt.value === len) { beatCnt.value := 0.U }
      }
      when (axi4.r.fire()) {
        c.inc()
        when (axi4.r.bits.last) { c.value := 0.U }
      }
      when (axi4.ar.fire()) {
        beatCnt.value := (axi4.ar.bits.addr >> axi4.ar.bits.size) & axi4.ar.bits.len
        when (axi4.ar.bits.len =/= 0.U && axi4.ar.bits.burst === AXI4Parameters.BURST_WRAP) {
          assert(axi4.ar.bits.len === 1.U || axi4.ar.bits.len === 3.U ||
            axi4.ar.bits.len === 7.U || axi4.ar.bits.len === 15.U)
        }
      }
      (beatCnt.value, axi4.r.bits.last)

    case axi4lite: AXI4Lite =>
      raddr := axi4lite.ar.bits.addr
      (0.U, true.B)
  }

  val r_busy = BoolStopWatch(in.ar.fire(), in.r.fire() && rLast, startHighPriority = true)
  in.ar.ready := in.r.ready || !r_busy
  in.r.bits.resp := AXI4Parameters.RESP_OKAY
  ren := RegNext(in.ar.fire()) || (in.r.fire() && !rLast)
  in.r.valid := BoolStopWatch(ren && (in.ar.fire() || r_busy), in.r.fire(), startHighPriority = true)


  val waddr = Wire(UInt())
  val (writeBeatCnt, wLast) = in match {
    case axi4: AXI4 =>
      val c = Counter(256)
      waddr := HoldUnless(axi4.aw.bits.addr, axi4.aw.fire())
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
