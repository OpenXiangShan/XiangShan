// See LICENSE.SiFive for license details.

package bus.tilelink

import chisel3._
import chisel3.util._

import xiangshan.XSModule
import bus.axi4.AXI4
import bus.axi4.AXI4Parameters
import utils.{GTimer, MuxTLookup, XSDebug}

// It's a fake tilelink LLC
// It's only deals with AcquireBlock and ReleaseData from upper layer
// and converts to AXI reads/writes
class MMIOTLToAXI4(params: TLParameters) extends XSModule
{
  val io = IO(new Bundle{
    val in = Flipped(new TLCached(params))
    val out = new AXI4 
  })

  val in = io.in
  val out = io.out

  val Y = true.B
  val N = false.B


  /* parameters */
  val innerBeatSize = in.d.bits.data.getWidth
  val outerBeatSize = out.w.bits.data.getWidth
  val outerBeatBytes = outerBeatSize / 8
  val addrWidth = in.a.bits.address.getWidth
  val innerIdWidth = in.a.bits.source.getWidth
  val outerIdWidth = out.aw.bits.id.getWidth

  assert(in.a.bits.address.getWidth == out.aw.bits.addr.getWidth)
  assert(innerBeatSize == outerBeatSize)
  val axi4_size = log2Up(outerBeatBytes).U

  val s_idle :: s_wait_awready :: s_mem_write :: s_wait_bresp :: s_wait_arready :: s_mem_read :: s_send_resp :: Nil = Enum(7)
 
  val state = RegInit(s_idle)
 
  when (in.anyFire) {
    XSDebug("tilelink in ")
    in.dump
  }
 
  when (out.anyFire) {
    XSDebug("tilelink out ")
    out.dump
  }
 
  // assign default value to signals
  in.a.ready := N
  in.b.valid := N
  in.b.bits  := DontCare
  in.c.ready := N
  in.d.valid := N
  in.d.bits  := DontCare
  in.e.ready := N
 
  out.aw.valid := N
  out.aw.bits  := DontCare
  out.w.valid  := N
  out.w.bits   := DontCare
  out.b.ready  := N
  out.ar.valid := N
  out.ar.bits  := DontCare
  out.r.ready  := N
 
  val req = Reg(chiselTypeOf(io.in.a.bits))
  when (in.a.fire()) {
    req := in.a.bits
  }
 
  val opcode = in.a.bits.opcode
  val is_read = opcode === TLMessages.Get
  val is_write = opcode === TLMessages.PutPartialData
  val read_fire = in.a.fire() && is_read
  val write_fire = in.a.fire() && is_write
  assert(!(in.a.fire() && !is_read && !is_write), "FakeTLLLC: Invalid opcode on channel A")
 
 
  // state transitions:
  // s_idle: idle state
  // capture requests
  // --------------------------------------------------------------------------------
  when (state === s_idle) {
    in.a.ready := true.B
 
    when (read_fire) {
      state := s_wait_arready
    }
 
    when (write_fire) {
      state := s_wait_awready
    }
  }
 
 
  // acquire block: do AXI read
  // s_wait_arready, s_mem_read, s_read_resp
  // --------------------------------------------------------------------------------
  when (state === s_wait_arready) {
    // AR channel
    // read address channel signals
    val out_ar = out.ar.bits
    out.ar.valid := Y
    out_ar.id := 0.asUInt(outerIdWidth.W)
    out_ar.addr := req.address
    // only read one beat
    out_ar.len := 0.asUInt(8.W)
    out_ar.size := axi4_size
    out_ar.burst := AXI4Parameters.BURST_INCR
    out_ar.lock := 0.asUInt(1.W)
    out_ar.cache := AXI4Parameters.CACHE_RALLOCATE | AXI4Parameters.CACHE_WALLOCATE | AXI4Parameters.CACHE_MODIFIABLE | AXI4Parameters.CACHE_BUFFERABLE
    out_ar.prot := 0.asUInt(3.W)
    out_ar.qos := 0.asUInt(4.W)
 
    when (out.ar.fire()) {
      state := s_mem_read
    }
  }
 
  val resp_data = Reg(UInt(outerBeatSize.W))
  when (state === s_mem_read) {
    out.r.ready := Y
    when (out.r.fire()) {
      resp_data := out.r.bits.data
      state := s_send_resp
    }
  }
 
  // deal with write
  // s_wait_awready & s_mem_write
  // --------------------------------------------------------------------------------
  when (state === s_wait_awready) {
    val out_aw = out.aw.bits
    out.aw.valid := Y
    out_aw.id := 0.U
    out_aw.addr := req.address
    out_aw.len := 0.asUInt(8.W)
    out_aw.size := axi4_size
    out_aw.burst := AXI4Parameters.BURST_INCR       // normal sequential memory
    out_aw.lock := 0.asUInt(1.W)
    out_aw.cache := AXI4Parameters.CACHE_RALLOCATE | AXI4Parameters.CACHE_WALLOCATE | AXI4Parameters.CACHE_MODIFIABLE | AXI4Parameters.CACHE_BUFFERABLE
    out_aw.prot := 0.asUInt(3.W)
    out_aw.qos := 0.asUInt(4.W)
    when (out.aw.fire()) {
      state := s_mem_write
    }
  }
 
  when (state === s_mem_write) {
    val out_w = out.w.bits
    out.w.valid := Y
    out_w.data := req.data
    out_w.strb := req.mask
    out_w.last := Y
    when (out.w.fire()) {
      state := s_wait_bresp
    }
  }
 
  when (state === s_wait_bresp) {
    out.b.ready := Y
    when (out.b.fire()) {
      state := s_send_resp
    }
  }
 
  when (state === s_send_resp) {
    in.d.valid := Y
    val accessAck      = TLSlaveUtilities.AccessAck(req)
    val accessAckData  = TLSlaveUtilities.AccessAck(req, resp_data)
    in.d.bits := Mux(req.opcode === TLMessages.Get, accessAckData, accessAck)
    when (in.d.fire()) {
      state := s_idle
    }
  }
}


object MMIOTLToAXI4
{
  def apply(params: TLParameters) = { new MMIOTLToAXI4(params) }
  def apply(in: TLCached): AXI4 = {
    val m = Module(new MMIOTLToAXI4(in.params))
    m.in <> in
    m.out
  }
}
