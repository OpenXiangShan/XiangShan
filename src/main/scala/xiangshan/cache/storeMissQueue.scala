package xiangshan.cache

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
    val req         = Input(new DCacheLineReq )
    val replay      = DecoupledIO(new DCacheLineReq )

    val miss_req    = DecoupledIO(new MissReq)
    val miss_resp   = Flipped(ValidIO(new MissResp))
    val miss_finish = DecoupledIO(new MissFinish)

    val idx = Output(Valid(UInt()))
    val tag = Output(Valid(UInt()))
  })

  val s_invalid :: s_miss_req :: s_miss_resp :: s_drain_rpq :: s_replay_resp :: s_miss_finish :: Nil = Enum(6)
  val state = RegInit(s_invalid)

  val req     = Reg(new DCacheLineReq )
  val req_idx = get_idx(req.addr)
  val req_tag = get_tag(req.addr)
  val req_block_addr = get_block_addr(req.addr)
  val reg_miss_resp = Reg(new MissResp)

  // assign default values to output signals
  io.req_pri_rdy         := state === s_invalid
  when (io.req_pri_val && io.req_pri_rdy) {
    assert(req.cmd === M_XWR)
  }

  io.replay.valid        := false.B
  io.replay.bits         := DontCare

  io.miss_req.valid      := false.B
  io.miss_req.bits       := DontCare
  io.miss_finish.valid   := false.B
  io.miss_finish.bits    := DontCare

  io.idx.valid := state =/= s_invalid
  io.tag.valid := state =/= s_invalid
  io.idx.bits := req_idx
  io.tag.bits := req_tag


  XSDebug("entry: %d state: %d\n", io.id, state)
  // --------------------------------------------
  // s_invalid: receive requests
  when (state === s_invalid) {
    when (io.req_pri_val && io.req_pri_rdy) {
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
    io.replay.valid            := true.B
    io.replay.bits             := req
    io.replay.bits.meta.replay := true.B
    when (io.replay.fire()) {
      replay_resp_ctr := 0.U
      state := s_replay_resp
    }
  }

  //
  // we must wait for response here,
  // if we do'not wait for response here,
  // this entry may be freed before it's response comes back
  //
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
    val miss_resp   = Flipped(ValidIO(new MissResp))
    val miss_finish = DecoupledIO(new MissFinish)
  })

  val miss_req_arb   = Module(new Arbiter(new MissReq,    cfg.nStoreMissEntries))
  val miss_finish_arb    = Module(new Arbiter(new MissFinish, cfg.nStoreMissEntries))
  val replay_arb     = Module(new Arbiter(new DCacheLineReq ,   cfg.nStoreMissEntries))

  val idx_matches = Wire(Vec(cfg.nLoadMissEntries, Bool()))
  val tag_matches = Wire(Vec(cfg.nLoadMissEntries, Bool()))

  val tag_match   = Mux1H(idx_matches, tag_matches)
  val idx_match   = idx_matches.reduce(_||_)


  val req             = io.lsu.req
  val entry_alloc_idx = Wire(UInt())
  val pri_rdy         = WireInit(false.B)
  val pri_val         = req.valid && !idx_match
  // sbuffer should not send down the same block twice
  // what's more, it should allow write into sbuffer
  // if the same block is being handled dcache
  assert(!(req.valid && tag_match))

  val entries = (0 until cfg.nStoreMissEntries) map { i =>
    val entry = Module(new StoreMissEntry)

    entry.io.id := i.U(log2Up(cfg.nStoreMissEntries).W)

    idx_matches(i) := entry.io.idx.valid && entry.io.idx.bits === get_idx(req.bits.addr)
    tag_matches(i) := entry.io.tag.valid && entry.io.tag.bits === get_tag(req.bits.addr)

    // entry req
    entry.io.req_pri_val := (i.U === entry_alloc_idx) && pri_val
    when (i.U === entry_alloc_idx) {
      pri_rdy := entry.io.req_pri_rdy
    }
    entry.io.req   := req.bits

    replay_arb.io.in(i)      <> entry.io.replay
    miss_req_arb.io.in(i)    <> entry.io.miss_req

    entry.io.miss_resp.valid := (i.U === io.miss_resp.bits.client_id) && io.miss_resp.valid
    entry.io.miss_resp.bits  := io.miss_resp.bits

    miss_finish_arb.io.in(i) <> entry.io.miss_finish

    entry
  }

  entry_alloc_idx    := PriorityEncoder(entries.map(m=>m.io.req_pri_rdy))

  // whenever index matches, do not let it in
  req.ready      := pri_rdy && !idx_match
  io.replay.req  <> replay_arb.io.out
  io.lsu.resp    <> io.replay.resp
  io.miss_req    <> miss_req_arb.io.out
  io.miss_finish <> miss_finish_arb.io.out

  // debug output
  when (req.fire()) {
    XSDebug(s"req cmd: %x addr: %x data: %x mask: %x id: %d replay: %b\n",
      req.bits.cmd, req.bits.addr, req.bits.data, req.bits.mask, req.bits.meta.id, req.bits.meta.replay)
  }

  val replay = io.replay.req
  when (replay.fire()) {
    XSDebug(s"replay cmd: %x addr: %x data: %x mask: %x id: %d replay: %b\n",
      replay.bits.cmd, replay.bits.addr, replay.bits.data, replay.bits.mask, replay.bits.meta.id, replay.bits.meta.replay)
  }

  val resp = io.lsu.resp
  when (resp.fire()) {
    XSDebug(s"resp: data: %x id: %d replay: %b miss: %b nack: %b\n",
      resp.bits.data, resp.bits.meta.id, resp.bits.meta.replay, resp.bits.miss, resp.bits.nack)
  }

  val miss_req = io.miss_req
  XSDebug(miss_req.fire(), "miss_req cmd: %x addr: %x client_id: %d\n",
    miss_req.bits.cmd, miss_req.bits.addr, miss_req.bits.client_id)

  val miss_resp = io.miss_resp
  XSDebug(miss_resp.fire(), "miss_resp client_id: %d entry_id: %d\n",
    miss_resp.bits.client_id, miss_resp.bits.entry_id)

  val miss_finish = io.miss_finish
  XSDebug(miss_finish.fire(), "miss_finish client_id: %d entry_id: %d\n",
    miss_finish.bits.client_id, miss_finish.bits.entry_id)
}
