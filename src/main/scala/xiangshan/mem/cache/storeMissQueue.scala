//******************************************************************************
// Ported from Rocket-Chip
// See LICENSE.Berkeley and LICENSE.SiFive in Rocket-Chip for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package xiangshan.mem.cache

import chisel3._
import chisel3.util._

import utils.XSDebug
import bus.tilelink._

class StoreMissEntry extends DCacheModule
{
  val io = IO(new Bundle {
    val id = Input(UInt())

    val req_pri_val = Input(Bool())
    val req_pri_rdy = Output(Bool())
    val req         = Flipped(new DCacheStoreReq)
    val replay      = DecoupledIO(new DCacheStoreReq)

    val miss_req    = DecoupledIO(new MissReq)
    val miss_resp   = ValidIO(new MissResp)
    val miss_finish = Flipped(DecoupledIO(new MissFinish))

    val idx = Output(Valid(UInt()))
    val way = Output(Valid(UInt()))
    val tag = Output(Valid(UInt()))
  })

  val s_invalid :: s_miss_req :: s_miss_resp :: s_drain_rpq :: s_replay_resp :: s_miss_finish :: Nil = Enum(6)
  val state = RegInit(s_invalid)

  val req     = Reg(new DCacheStoreReq)
  val req_idx = req.addr(untagBits-1, blockOffBits)
  val req_tag = req.addr >> untagBits
  val req_block_addr = (req.addr >> blockOffBits) << blockOffBits
  val reg_miss_resp = Reg(new MissResp)

  // assign default values to output signals
  io.req_pri_rdy         := false.B

  io.replay.valid        := false.B
  io.replay.bits         := DontCare

  io.miss_req.valid      := false.B
  io.miss_req.bits       := DontCare
  io.miss_finish.valid   := false.B
  io.miss_finish.bits    := DontCare

  io.idx.valid := state =/= s_invalid
  io.tag.valid := state =/= s_invalid
  io.way.valid := state =/= s_invalid
  io.idx.bits := req_idx
  io.tag.bits := req_tag
  io.way.bits := DontCare


  XSDebug("entry: %d state: %d\n", io.id, state)
  // --------------------------------------------
  // s_invalid: receive requests
  when (state === s_invalid) {
    io.req_pri_rdy := true.B
    when (io.req_pri_val && io.req_pri_rdy) {
      assert(req.cmd === M_XRD)
      req   := io.req
      state := s_miss_req
    }
  }

  // --------------------------------------------
  when (state === s_miss_req) {
    io.miss_req.valid          := true.B
    io.miss_req.bits.cmd       := req.cmd
    io.miss_req.bits.addr      := req_block_addr
    io.miss_req.bits.client_id := io.id

    when (io.miss_req.fire()) {
      state := s_miss_resp
    }
  }

  when (state === s_miss_resp) {
    when (io.miss_resp.fire()) {
      reg_miss_resp := io.miss_resp.bits
      state         := s_drain_rpq
    }
  }

  // --------------------------------------------
  // replay
  val storePipelineLatency = 5
  val replay_resp_ctr  = Reg(UInt(log2Up(storePipelineLatency).W))
  when (state === s_drain_rpq) {
    io.replay.valid := true.B
    io.replay.bits  := req
    when (io.replay.fire()) {
      state := s_replay_resp
    }
  }

  when (state === s_replay_resp) {
    replay_resp_ctr := replay_resp_ctr + 1.U
    when (replay_resp_ctr === storePipelineLatency.U) {
      state := s_miss_finish
    }
  }

  when (state === s_miss_finish) {
    io.miss_finish.valid          := true.B
    io.miss_finish.bits.client_id := io.id
    io.miss_finish.bits.entry_id  := reg_miss_resp.entry_id
    when (io.miss_finish.fire()) {
      state := s_invalid
    }
  }
}


class StoreMissQueue extends DCacheModule
{
  val io = IO(new Bundle {
    val lsu         = Flipped(new DCacheStoreIO)
    val replay      = new DCacheStoreIO

    val miss_req    = DecoupledIO(new MissReq)
    val miss_resp   = ValidIO(new MissResp)
    val miss_finish = Flipped(DecoupledIO(new MissFinish))
  })

  val miss_req_arb   = Module(new Arbiter(new MissReq,    cfg.nStoreMissEntries))
  val miss_finish_arb    = Module(new Arbiter(new MissFinish, cfg.nStoreMissEntries))
  val replay_arb     = Module(new Arbiter(new DCacheStoreReq,   cfg.nStoreMissEntries))

  val req             =  io.lsu.req
  val entry_alloc_idx = Wire(UInt())
  val pri_rdy         = WireInit(false.B)
  val pri_val         = req.valid

  val entries = (0 until cfg.nStoreMissEntries) map { i =>
    val entry = Module(new StoreMissEntry)

    entry.io.id := i.U(log2Up(cfg.nStoreMissEntries).W)

    // entry req
    entry.io.req_pri_val := (i.U === entry_alloc_idx) && pri_val
    when (i.U === entry_alloc_idx) {
      pri_rdy := entry.io.req_pri_rdy
    }
    entry.io.req   := req.bits

    replay_arb.io.in(i)      <> entry.io.replay
    miss_req_arb.io.in(i)    <> entry.io.miss_req
    when ((i.U === io.miss_resp.bits.client_id) && io.miss_resp.valid) {
      entry.io.miss_resp.valid := true.B
      entry.io.miss_resp.bits := io.miss_resp.bits
    }
    miss_finish_arb.io.in(i) <> entry.io.miss_finish

    entry
  }

  entry_alloc_idx    := RegNext(PriorityEncoder(entries.map(m=>m.io.req_pri_rdy)))

  req.ready      := pri_rdy
  io.replay.req  <> replay_arb.io.out
  io.lsu.resp    <> io.replay.resp
  io.miss_resp   <> miss_req_arb.io.out
  io.miss_finish <> miss_finish_arb.io.out
}
