// See LICENSE.SiFive for license details.

package bus.tilelink

import chisel3._
import chisel3.util._

import xiangshan.XSModule
import bus.axi4.{AXI4, AXI4Parameters}
import utils.{GTimer, XSDebug}

// a simpel TileLink to AXI4 converter
// only support TileLink put and get
class NaiveTLToAXI4(params: TLParameters) extends XSModule
{
  val io = IO(new Bundle{
    val in = Flipped(new TLCached(params))
    val out = new AXI4 
  })

  io := DontCare

  val debug = true

  val in = io.in
  val out = io.out

  /* parameters */
  val Y = true.B
  val N = false.B

  val blockSize = 64 * 8
  val blockBytes = blockSize / 8
  val innerBeatSize = in.d.bits.data.getWidth
  val innerBeatBytes = innerBeatSize / 8
  val innerDataBeats = blockSize / innerBeatSize
  val innerBeatBits = log2Ceil(innerBeatBytes)
  val innerBeatIndexBits = log2Ceil(innerDataBeats)
  val innerBeatLSB = innerBeatBits
  val innerBeatMSB = innerBeatLSB + innerBeatIndexBits - 1

  val outerBeatSize = out.w.bits.data.getWidth
  val outerBeatBytes = outerBeatSize / 8
  val outerDataBeats = blockSize / outerBeatSize
  val outerBeatLen = log2Ceil(outerBeatBytes)
  val outerBurstLen = outerBeatLen + log2Ceil(outerDataBeats)
  val addrWidth = in.a.bits.address.getWidth
  val innerIdWidth = in.a.bits.source.getWidth
  val outerIdWidth = out.aw.bits.id.getWidth

  assert(in.a.bits.address.getWidth == out.aw.bits.addr.getWidth)
  assert(innerBeatSize == outerBeatSize)

  val split = innerBeatSize / outerBeatSize
  val splitBits = log2Ceil(split)
  require(isPow2(split))

  val s_idle :: s_gather_write_data :: s_wait_awready :: s_mem_write :: s_wait_bresp :: s_wait_arready :: s_mem_read :: s_read_resp :: s_write_resp :: Nil = Enum(9)

  val state = RegInit(s_idle)

  when (XSDebug.trigger) {
    XSDebug.printPrefix
    in.dump
    out.dump
  }

  val in_opcode = in.a.bits.opcode
  val in_addr = in.a.bits.address
  val in_id   = in.a.bits.source
  val in_len_shift = in.a.bits.size >= innerBeatBits.U
  val in_len  = Mux(in_len_shift, ((1.U << in.a.bits.size) >> innerBeatBits) - 1.U, 0.U)  // #word, i.e., arlen in AXI
  val in_data = in.a.bits.data
  val in_data_mask = in.a.bits.mask

  val in_recv_fire = in.a.fire()
  val in_read_req = in_recv_fire && (in_opcode === TLMessages.Get)
  val in_write_req = in_recv_fire && (in_opcode === TLMessages.PutFullData)

  val addr = Reg(UInt(addrWidth.W))
  val id = Reg(UInt(innerIdWidth.W))
  val opcode = Reg(UInt(3.W))
  val size_reg = Reg(UInt(in.a.bits.size.getWidth.W))
  
  val ren = RegInit(N)
  val wen = RegInit(N)

  val start_beat = in_addr(innerBeatMSB, innerBeatLSB)
  val inner_end_beat_reg = Reg(UInt(4.W))
  val inner_end_beat = Mux(state === s_idle, start_beat + in_len, inner_end_beat_reg)

  // gather write data beat count
  val gather_curr_beat_reg = RegInit(0.asUInt(log2Ceil(innerDataBeats).W))
  val gather_curr_beat = Mux(state === s_idle, start_beat, gather_curr_beat_reg)
  val gather_last_beat = gather_curr_beat === inner_end_beat

  // read response beat count
  val resp_curr_beat = RegInit(0.asUInt(log2Ceil(innerDataBeats).W))
  val resp_last_beat = resp_curr_beat === inner_end_beat

  // state transitions:
  // s_idle: idle state
  // capture requests
  // --------------------------------------------------------------------------------
  when (state === s_idle) {
    when (in_read_req) {
      ren := Y
      wen := N

      addr := in_addr
      id := in_id
      opcode := in_opcode
      size_reg := in.a.bits.size

      resp_curr_beat := start_beat
      inner_end_beat_reg := start_beat + in_len

      state := s_wait_arready
    } .elsewhen (in_write_req) {
      ren := N
      wen := Y
      addr := in_addr
      id := in_id
      opcode := in_opcode
      size_reg := in.a.bits.size

      resp_curr_beat := start_beat
      inner_end_beat_reg := start_beat + in_len

      state := s_gather_write_data
    } .elsewhen (in.b.fire() || in.c.fire() || in.e.fire()) {
      assert(N, "Inner tilelink Unexpected handshake")
    }
  }


  // s_gather_write_data:
  // gather write data
  // --------------------------------------------------------------------------------
  val data_buf = Reg(Vec(outerDataBeats, UInt(outerBeatSize.W)))
  val data_mask = Reg(Vec(outerDataBeats, UInt(outerBeatBytes.W)))

  // tilelink receives the first data beat when address handshake
  // which is different from axi
  val first_data_beat = state === s_idle && in_write_req
  val following_data_beat = state === s_gather_write_data && in_recv_fire
  val gather_data_beat = first_data_beat || following_data_beat

  when (first_data_beat) {
    gather_curr_beat_reg := start_beat + 1.U
  }
  
  when (following_data_beat) {
    gather_curr_beat_reg := gather_curr_beat_reg + 1.U
  }

  when (first_data_beat || following_data_beat) {
    for (i <- 0 until split) {
      data_buf((gather_curr_beat << splitBits) + i.U) := in_data(outerBeatSize * (i + 1) - 1, outerBeatSize * i)
      data_mask((gather_curr_beat << splitBits) + i.U) := in_data_mask(outerBeatBytes * (i + 1) - 1, outerBeatBytes * i)
    }
    when (gather_last_beat) {
      state := s_write_resp
    }
  }

  when (state === s_write_resp && in.d.fire()) {
    state := s_wait_awready
  }

  // s_wait_arready, s_mem_read, s_read_resp
  // deal with read
  // --------------------------------------------------------------------------------
  when (state === s_wait_arready && out.ar.fire()) {
    state := s_mem_read
  }

  val out_rdata_fire = out.r.fire()
  val (refill_cnt, refill_done) = Counter(out_rdata_fire && state === s_mem_read, outerDataBeats)
  when (state === s_mem_read && out_rdata_fire) {
    data_buf(refill_cnt) := out.r.bits.data
    when (refill_done) {
      state := s_read_resp
    }
  }

  when (state === s_read_resp && in.d.fire()) {
    resp_curr_beat := resp_curr_beat + 1.U
    when (resp_last_beat) {
      state := s_idle
    }
  }

  val resp_data = Wire(Vec(split, UInt(outerBeatSize.W)))
  for (i <- 0 until split) {
    resp_data(i) := data_buf((resp_curr_beat << splitBits) + i.U)
  }
  

  // deal with write
  // s_wait_awready & s_mem_write
  // --------------------------------------------------------------------------------
  when (state === s_wait_awready && out.aw.fire()) {
    state := s_mem_write
  }

  val (wb_cnt, wb_done) = Counter(out.w.fire() && state === s_mem_write, outerDataBeats)
  when (state === s_mem_write && wb_done) {
    state := s_wait_bresp
  }

  when (state === s_wait_bresp && out.b.fire()) {
    state := s_idle
  }

  // IO ports
  // Input tilelink channels
  // --------------------------------------------------------------------------------

  // channel A
  in.a.ready := state === s_idle || state === s_gather_write_data

  // channel B
  in.b.valid := N

  // channel C
  in.c.ready := N

  // channel D
  val in_read_resp = state === s_read_resp
  val in_write_resp = state === s_write_resp
  in.d.valid := in_write_resp || in_read_resp
  in.d.bits.opcode  := Mux(in_read_resp, TLMessages.AccessAckData, TLMessages.AccessAck)
  in.d.bits.param   := 0.U
  in.d.bits.size    := size_reg
  in.d.bits.source  := id
  in.d.bits.sink    := 0.U
  in.d.bits.denied  := N
  in.d.bits.data    := resp_data(resp_curr_beat)
  in.d.bits.corrupt := N

  // channel E
  in.e.ready := N

  // Output AXI4 channels
  // --------------------------------------------------------------------------------
  val axi4_size = log2Up(outerBeatBytes).U

  // AW channel
  // write address channel signals
  out.aw.valid := state === s_wait_awready
  out.aw.bits.id := 0.U
  out.aw.bits.addr := addr
  out.aw.bits.len := (outerDataBeats - 1).asUInt(8.W)
  out.aw.bits.size := axi4_size
  out.aw.bits.burst := AXI4Parameters.BURST_INCR       // normal sequential memory
  out.aw.bits.lock := 0.asUInt(1.W)
  out.aw.bits.cache := AXI4Parameters.CACHE_RALLOCATE | AXI4Parameters.CACHE_WALLOCATE | AXI4Parameters.CACHE_MODIFIABLE | AXI4Parameters.CACHE_BUFFERABLE
  out.aw.bits.prot := 0.asUInt(3.W)
  out.aw.bits.qos := 0.asUInt(4.W)

  // W channel
  // write data channel signals
  out.w.valid := state === s_mem_write
  out.w.bits.data := data_buf(wb_cnt)
  out.w.bits.strb := Fill(outerBeatBytes, 1.asUInt(1.W))
  out.w.bits.last := wb_cnt === (outerDataBeats - 1).U

  // B channel
  // write response channel signals
  out.b.ready := state === s_wait_bresp

  // AR channel
  // read address channel signals
  out.ar.valid := state === s_wait_arready
  out.ar.bits.id := 0.asUInt(outerIdWidth.W)
  out.ar.bits.addr := addr
  out.ar.bits.len := (outerDataBeats - 1).asUInt(8.W)
  out.ar.bits.size := axi4_size
  out.ar.bits.burst := AXI4Parameters.BURST_INCR
  out.ar.bits.lock := 0.asUInt(1.W)
  out.ar.bits.cache := AXI4Parameters.CACHE_RALLOCATE | AXI4Parameters.CACHE_WALLOCATE | AXI4Parameters.CACHE_MODIFIABLE | AXI4Parameters.CACHE_BUFFERABLE
  out.ar.bits.prot := 0.asUInt(3.W)
  out.ar.bits.qos := 0.asUInt(4.W)

  // R channel
  // read data channel signals
  out.r.ready := state === s_mem_read
}


object NaiveTLToAXI4
{
  def apply(params: TLParameters) = { new NaiveTLToAXI4(params) }
}
