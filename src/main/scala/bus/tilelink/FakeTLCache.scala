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
class FakeTLLLC(params: TLParameters) extends XSModule
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

  val axi4_size = log2Up(outerBeatBytes).U

  assert(in.a.bits.address.getWidth == out.aw.bits.addr.getWidth)
  assert(innerBeatSize == outerBeatSize)

  val split = innerBeatSize / outerBeatSize
  val splitBits = log2Ceil(split)
  require(isPow2(split))

  val s_idle :: s_gather_release_data :: s_wait_awready :: s_mem_write :: s_wait_bresp :: s_send_release_resp :: s_wait_arready :: s_mem_read :: s_send_acquire_resp :: s_wait_e :: Nil = Enum(10)

  val state = RegInit(s_idle)

  when (in.anyFire) {
    XSDebug("tilelink in ")
    in.dump
  }

  when (out.anyFire) {
    XSDebug("tilelink out ")
    out.dump
  }

  // XSDebug.exec(in.anyFire, in.dump)
  // XSDebug.exec(out.anyFire, out.dump)

  // AcquireBlock comes from TL channel A
  // ReleaseData comes from TL channel C
  // AcquireBlock converts to AXI read
  // AcqureBlock procedures:
  // 1. channel A AcquireBlock
  // 2. channel D GrantData/GrantAck
  // 3. chanenl E
  //
  // ReleaseData converts to AXI write
  // ReleaseData procedures:
  // 1. channel C ReleaseData
  // 2. channel D GrantAck

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

  // luckily, channel A and C has the same parameter
  val acquire_req = Reg(chiselTypeOf(io.in.a.bits))
  val release_req = Reg(chiselTypeOf(io.in.c.bits))
  when (in.a.fire()) {
    acquire_req := in.a.bits
  }
  when (in.c.fire()) {
    release_req := in.c.bits
  }

  val req_acquire_block :: req_release_data :: Nil = Enum(2)

  // does this request leads memory access?
  // what will the master's param be after this req
  val needs_memory_access = RegInit(N)
  val new_param = Reg(chiselTypeOf(in.a.bits.param))

  val data_buf = Reg(Vec(outerDataBeats, UInt(outerBeatSize.W)))

  val opcode = Mux(in.a.fire(), in.a.bits.opcode, in.c.bits.opcode)
  val param = Mux(in.a.fire(), in.a.bits.param, in.c.bits.param)

  val is_acquire_block = opcode === TLMessages.AcquireBlock
  val acquire_block_fire = in.a.fire() && is_acquire_block
  assert(!(in.a.fire() && !is_acquire_block), "FakeTLLLC: Invalid opcode on channel A")

  val is_release_data = opcode === TLMessages.ReleaseData
  val release_data_fire = in.c.fire && is_release_data
  assert(!(in.c.fire() && !is_release_data), "FakeTLLLC: Invalid opcode on channel C")

  // return (valid or not, needs_memory_access, new_param, next_state)
  def helper(request_type: UInt, request_param: UInt): (Bool, Bool, UInt, UInt) = {
    import bus.tilelink.TLPermissions._
    MuxTLookup(Cat(request_type, request_param), (N, N, 0.U, s_idle), Seq(
      Cat(req_acquire_block, NtoB)   -> (Y, Y, toB, s_wait_arready),
      Cat(req_acquire_block, NtoT)   -> (Y, Y, toT, s_wait_arready),
      Cat(req_acquire_block, BtoT)   -> (Y, N, toT, s_send_acquire_resp),
      Cat(req_release_data,  TtoB)   -> (Y, Y, toB, s_gather_release_data),
      Cat(req_release_data,  TtoN)   -> (Y, Y, toN, s_gather_release_data),
      Cat(req_release_data,  BtoN)   -> (Y, N, toN, s_send_release_resp)))
  }

  XSDebug("state: %d\n", state)
  
  // state transitions:
  // s_idle: idle state
  // capture requests
  // --------------------------------------------------------------------------------
  when (state === s_idle) {
    // ready can wait on valid
    // give priority to AcqurieBlock
    in.a.ready := in.a.valid
    in.c.ready := !in.a.valid && in.c.valid

    when (acquire_block_fire || release_data_fire) {
      val request_type = Mux(acquire_block_fire, req_acquire_block, req_release_data)
      val res = helper(request_type, param)
      assert(res._1, "invalid request\n")
      needs_memory_access := res._2
      new_param := res._3
      state := res._4
    } .elsewhen (in.b.fire() || in.d.fire() || in.e.fire()) {
      assert(N, "Inner tilelink Unexpected handshake")
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
    out_ar.addr := acquire_req.address
    out_ar.len := (outerDataBeats - 1).asUInt(8.W)
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

  val (refill_cnt, refill_done) = Counter(out.r.fire(), outerDataBeats)
  when (state === s_mem_read) {
    out.r.ready := Y
    when (out.r.fire()) {
      data_buf(refill_cnt) := out.r.bits.data
      when (refill_done) {
        state := s_send_acquire_resp
      }
    }
  }

  // send acquire response
  val resp_fire = in.d.fire() && needs_memory_access
  val (resp_cnt, resp_done) = Counter(resp_fire, innerDataBeats)

  val resp_data = Wire(Vec(split, UInt(outerBeatSize.W)))
  for (i <- 0 until split) {
    resp_data(i) := data_buf((resp_cnt << splitBits) + i.U)
  }

  when (state === s_send_acquire_resp) {
    in.d.valid := Y
    val grantData = TLSlaveUtilities.Grant(params, 0.U, acquire_req.source, acquire_req.size, new_param, resp_data.asUInt, N, N)
    val grant = TLSlaveUtilities.Grant(params, 0.U, acquire_req.source, acquire_req.size, new_param, N)
    in.d.bits := Mux(needs_memory_access, grantData, grant)

    val grantDataDone = in.d.fire() && needs_memory_access && resp_done
    val grantDone = in.d.fire() && !needs_memory_access

    when (grantDataDone || grantDone) {
      state := s_wait_e
    }
  }

  when (state === s_wait_e) {
    in.e.ready := Y
    when (in.e.fire()) {
      state := s_idle
    }
  }

  // s_gather_release_data:
  // gather write data
  // --------------------------------------------------------------------------------

  // tilelink receives the first data beat when address handshake
  // which is different from axi
  val gather_data_beat = (state === s_idle || state === s_gather_release_data) && release_data_fire

  val (gather_cnt, gather_done) = Counter(gather_data_beat, innerDataBeats)

  when (gather_data_beat) {
    for (i <- 0 until split) {
      data_buf((gather_cnt << splitBits) + i.U) := in.c.bits.data(outerBeatSize * (i + 1) - 1, outerBeatSize * i)
    }
  }

  when (state === s_gather_release_data) {
    in.c.ready := Y
    when (gather_done) {
      state := s_send_release_resp
    }
  }
  
  when (state === s_send_release_resp) {
    in.d.valid := Y
    in.d.bits := TLSlaveUtilities.ReleaseAck(params, release_req.source, release_req.size, false.B)
    when (in.d.fire()) {
      state := s_wait_awready
    }
  }
  

  // deal with write
  // s_wait_awready & s_mem_write
  // --------------------------------------------------------------------------------
  when (state === s_wait_awready) {
    val out_aw = out.aw.bits
    out.aw.valid := Y
    out_aw.id := 0.U
    out_aw.addr := release_req.address
    out_aw.len := (outerDataBeats - 1).asUInt(8.W)
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

  val (wb_cnt, wb_done) = Counter(out.w.fire(), outerDataBeats)
  when (state === s_mem_write) {
    val out_w = out.w.bits
    out.w.valid := Y
    out_w.data := data_buf(wb_cnt)
    out_w.strb := Fill(outerBeatBytes, 1.asUInt(1.W))
    out_w.last := wb_cnt === (outerDataBeats - 1).U
    when (wb_done) {
      state := s_wait_bresp
    }
  }

  when (state === s_wait_bresp) {
    out.b.ready := Y
    when (out.b.fire()) {
      state := s_idle
    }
  }
}


object FakeTLLLC
{
  def apply(params: TLParameters) = { new FakeTLLLC(params) }
}
