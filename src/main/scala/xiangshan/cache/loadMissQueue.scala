package xiangshan.cache

import chisel3._
import chisel3.util._

import utils.XSDebug
import bus.tilelink._

class LoadMissEntry extends DCacheModule
{
  val io = IO(new Bundle {
    val id = Input(UInt())

    val req_pri_val = Input(Bool())
    val req_pri_rdy = Output(Bool())
    val req_sec_val = Input(Bool())
    val req_sec_rdy = Output(Bool())
    val req         = Input(new DCacheLoadReq)
    val replay      = DecoupledIO(new DCacheLoadReq)

    val miss_req    = DecoupledIO(new MissReq)
    val miss_resp   = Flipped(ValidIO(new MissResp))
    val miss_finish = DecoupledIO(new MissFinish)

    val idx = Output(Valid(UInt()))
    val tag = Output(Valid(UInt()))
  })

  val s_invalid :: s_miss_req :: s_miss_resp :: s_drain_rpq :: s_replay_resp :: s_miss_finish :: Nil = Enum(6)
  val state = RegInit(s_invalid)

  val req     = Reg(new DCacheLoadReq)
  val req_idx = get_idx(req.addr)
  val req_tag = get_tag(req.addr)
  val req_block_addr = get_block_addr(req.addr)
  val reg_miss_resp = Reg(new MissResp)

  val rpq = Module(new Queue(new DCacheLoadReq, cfg.nRPQ))

  rpq.io.enq.valid := (io.req_pri_val && io.req_pri_rdy) || (io.req_sec_val && io.req_sec_rdy)
  rpq.io.enq.bits  := io.req
  rpq.io.deq.ready := false.B

  when (rpq.io.enq.fire()) {
    assert(io.req.cmd === M_XRD)
  }

  io.req_pri_rdy := state === s_invalid
  val sec_rdy = state === s_miss_req || state === s_miss_resp
  io.req_sec_rdy := sec_rdy && rpq.io.enq.ready

  // assign default values to output signals
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
    assert(rpq.io.enq.ready)
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
  val loadPipelineLatency = 2
  val replay_resp_ctr  = Reg(UInt(log2Up(loadPipelineLatency).W))

  when (state === s_drain_rpq) {
    rpq.io.deq.ready := true.B
    io.replay <> rpq.io.deq
    when (rpq.io.count === 0.U) {
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
    when (replay_resp_ctr === loadPipelineLatency.U) {
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


class LoadMissQueue extends DCacheModule
{
  val io = IO(new Bundle {
    val lsu         = Flipped(new DCacheLoadIO)
    val replay      = new DCacheLoadIO

    val miss_req    = DecoupledIO(new MissReq)
    val miss_resp   = Flipped(ValidIO(new MissResp))
    val miss_finish = DecoupledIO(new MissFinish)
  })

  val miss_req_arb    = Module(new Arbiter(new MissReq,       cfg.nLoadMissEntries))
  val miss_finish_arb = Module(new Arbiter(new MissFinish,    cfg.nLoadMissEntries))
  val replay_arb      = Module(new Arbiter(new DCacheLoadReq, cfg.nLoadMissEntries))

  val idx_matches = Wire(Vec(cfg.nLoadMissEntries, Bool()))
  val tag_matches = Wire(Vec(cfg.nLoadMissEntries, Bool()))

  val tag_match   = Mux1H(idx_matches, tag_matches)
  val idx_match   = idx_matches.reduce(_||_)

  val req             = io.lsu.req
  val entry_alloc_idx = Wire(UInt())
  val pri_rdy         = WireInit(false.B)
  val pri_val         = req.valid && !idx_match
  var sec_rdy         = false.B

  val entries = (0 until cfg.nLoadMissEntries) map { i =>
    val entry = Module(new LoadMissEntry)

    entry.io.id := i.U(log2Up(cfg.nLoadMissEntries).W)

    idx_matches(i) := entry.io.idx.valid && entry.io.idx.bits === get_idx(req.bits.addr)
    tag_matches(i) := entry.io.tag.valid && entry.io.tag.bits === get_tag(req.bits.addr)
    when (XSDebug.trigger) {
      when (idx_matches(i)) {
        XSDebug(s"entry: $i idx_match\n")
      }
      when (tag_matches(i)) {
        XSDebug(s"entry: $i tag_match\n")
      }
    }

    // entry req
    entry.io.req_pri_val := (i.U === entry_alloc_idx) && pri_val
    when (i.U === entry_alloc_idx) {
      pri_rdy := entry.io.req_pri_rdy
    }
    entry.io.req_sec_val := req.valid && tag_match && idx_matches(i)
    sec_rdy   = sec_rdy || (entry.io.req_sec_rdy && entry.io.req_sec_val)
    entry.io.req   := req.bits

    replay_arb.io.in(i)      <> entry.io.replay
    miss_req_arb.io.in(i)    <> entry.io.miss_req

    entry.io.miss_resp.valid := (i.U === io.miss_resp.bits.client_id) && io.miss_resp.valid
    entry.io.miss_resp.bits  := io.miss_resp.bits

    miss_finish_arb.io.in(i) <> entry.io.miss_finish

    entry
  }

  entry_alloc_idx    := PriorityEncoder(entries.map(m=>m.io.req_pri_rdy))

  req.ready   := Mux(idx_match, tag_match && sec_rdy, pri_rdy)
  io.replay.req  <> replay_arb.io.out
  io.lsu.resp    <> io.replay.resp
  // replay never kills its previous request
  io.replay.s1_kill := false.B
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
