package xiangshan.cache

import chisel3._
import chisel3.util._

import utils.XSDebug
import bus.tilelink._

class StoreMissEntry extends DCacheModule
{
  val io = IO(new Bundle {
    val id = Input(UInt())

    val lsu         = Flipped(new DCacheLineIO)
    val replay      = new DCacheLineIO

    val miss_req    = DecoupledIO(new MissReq)
    val miss_resp   = Flipped(ValidIO(new MissResp))
    val miss_finish = DecoupledIO(new MissFinish)

    val idx = Output(Valid(UInt()))
    val tag = Output(Valid(UInt()))
  })

  val s_invalid :: s_replay_req :: s_replay_resp :: s_resp :: s_miss_req :: s_miss_resp :: s_miss_finish :: Nil = Enum(7)
  val state = RegInit(s_invalid)

  val req     = Reg(new DCacheLineReq )
  val resp    = Reg(new DCacheLineResp)

  val req_idx = get_idx(req.addr)
  val req_tag = get_tag(req.addr)
  val req_block_addr = get_block_addr(req.addr)
  val reg_miss_resp = Reg(new MissResp)

  // assign default values to output signals
  io.lsu.req.ready     := state === s_invalid
  io.lsu.resp.valid    := false.B
  io.lsu.resp.bits     := DontCare

  io.replay.req.valid  := false.B
  io.replay.req.bits   := DontCare
  io.replay.resp.ready := false.B

  io.miss_req.valid      := false.B
  io.miss_req.bits       := DontCare
  io.miss_finish.valid   := false.B
  io.miss_finish.bits    := DontCare

  io.idx.valid := state =/= s_invalid
  io.tag.valid := state =/= s_invalid
  io.idx.bits := req_idx
  io.tag.bits := req_tag


  when (state =/= s_invalid) {
    XSDebug("entry: %d state: %d idx: %x tag: %x\n", io.id, state, io.idx.bits, io.tag.bits)
  }

  // --------------------------------------------
  // s_invalid: receive requests
  when (state === s_invalid) {
    when (io.lsu.req.fire()) {
      assert(io.lsu.req.bits.cmd === M_XWR)
      assert(!io.lsu.req.bits.meta.replay)
      req   := io.lsu.req.bits
      state := s_replay_req
    }
  }

  // --------------------------------------------
  // replay
  when (state === s_replay_req) {
    io.replay.req.valid := true.B
    io.replay.req.bits  := req
    when (io.replay.req.fire()) {
      state := s_replay_resp
    }
  }

  when (state === s_replay_resp) {
    io.replay.resp.ready := true.B
    when (io.replay.resp.fire()) {
      when (io.replay.resp.bits.miss) {
        // replayed reqs should not miss
        assert(!req.meta.replay)
        when (!req.meta.replay) {
          state := s_miss_req
        }
      } .otherwise {
        resp := io.replay.resp.bits
        when (!req.meta.replay) {
          state := s_resp
        } .otherwise {
          state := s_miss_finish
        }
      }

      assert(!io.replay.resp.bits.nack)
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
      reg_miss_resp   := io.miss_resp.bits
      // mark req as replayed req
      req.meta.replay := true.B
      state           := s_replay_req
    }
  }

  when (state === s_miss_finish) {
    io.miss_finish.valid          := true.B
    io.miss_finish.bits.client_id := io.id
    io.miss_finish.bits.entry_id  := reg_miss_resp.entry_id
    when (io.miss_finish.fire()) {
      state := s_resp
    }
  }

  // --------------------------------------------
  when (state === s_resp) {
    io.lsu.resp.valid := true.B
    io.lsu.resp.bits  := resp

    when (io.lsu.resp.fire()) {
      state := s_invalid
    }
  }
}


class StoreMissQueue extends DCacheModule
{
  val io = IO(new Bundle {
    val lsu         = Flipped(new DCacheLineIO)
    val replay      = new DCacheLineIO

    val miss_req    = DecoupledIO(new MissReq)
    val miss_resp   = Flipped(ValidIO(new MissResp))
    val miss_finish = DecoupledIO(new MissFinish)
  })

  val miss_req_arb    = Module(new Arbiter(new MissReq,    cfg.nStoreMissEntries))
  val miss_finish_arb = Module(new Arbiter(new MissFinish, cfg.nStoreMissEntries))
  val replay_arb      = Module(new Arbiter(new DCacheLineReq,  cfg.nStoreMissEntries))
  val resp_arb        = Module(new Arbiter(new DCacheLineResp, cfg.nStoreMissEntries))

  val idx_matches = Wire(Vec(cfg.nLoadMissEntries, Bool()))
  val tag_matches = Wire(Vec(cfg.nLoadMissEntries, Bool()))

  val tag_match   = Mux1H(idx_matches, tag_matches)
  val idx_match   = idx_matches.reduce(_||_)

  XSDebug("idx_match: %b tag_match: %b\n", idx_match, tag_match)

  val req             = io.lsu.req
  val entry_alloc_idx = Wire(UInt())
  val pri_rdy         = WireInit(false.B)
  val pri_val         = req.valid && !idx_match
  // sbuffer should not send down the same block twice
  // what's more, it should allow write into sbuffer
  // if the same block is being handled dcache
  // assert(!(req.valid && tag_match))

  io.replay.resp.ready := false.B

  val entry_id_MSB = reqIdWidth - 1
  val entry_id_LSB = reqIdWidth - storeMissQueueEntryIdWidth

  val entries = (0 until cfg.nStoreMissEntries) map { i =>
    val entry = Module(new StoreMissEntry)

    entry.io.id := i.U(storeMissQueueEntryIdWidth.W)

    idx_matches(i) := entry.io.idx.valid && entry.io.idx.bits === get_idx(req.bits.addr)
    tag_matches(i) := entry.io.tag.valid && entry.io.tag.bits === get_tag(req.bits.addr)

    // lsu req and resp
    val entry_lsu = entry.io.lsu
    entry_lsu.req.valid := (i.U === entry_alloc_idx) && pri_val
    when (i.U === entry_alloc_idx) {
      pri_rdy := entry_lsu.req.ready
    }
    entry_lsu.req.bits  := req.bits

    resp_arb.io.in(i)   <> entry_lsu.resp

    // replay req and resp
    val entry_replay = entry.io.replay
    replay_arb.io.in(i) <> entry_replay.req
    replay_arb.io.in(i).bits.meta.id <> Cat(entry.io.id,
      entry_replay.req.bits.meta.id(entry_id_LSB - 1, 0))

    val resp_entry_id = io.replay.resp.bits.meta.id(entry_id_MSB, entry_id_LSB)
    entry_replay.resp.valid         := (i.U === resp_entry_id) && io.replay.resp.valid
    entry_replay.resp.bits          := io.replay.resp.bits
    entry_replay.resp.bits.meta.id  :=  Cat(0.U(storeMissQueueEntryIdWidth.W),
      io.replay.resp.bits.meta.id(entry_id_LSB - 1, 0))
    when (entry_replay.resp.valid) {
      io.replay.resp.ready := entry_replay.resp.ready
    }

    miss_req_arb.io.in(i)  <> entry.io.miss_req
    entry.io.miss_resp.valid := (i.U === io.miss_resp.bits.client_id) && io.miss_resp.valid
    entry.io.miss_resp.bits  := io.miss_resp.bits

    miss_finish_arb.io.in(i) <> entry.io.miss_finish
    entry
  }

  entry_alloc_idx    := PriorityEncoder(entries.map(m=>m.io.lsu.req.ready))

  // whenever index matches, do not let it in
  req.ready      := pri_rdy && !idx_match
  io.lsu.resp    <> resp_arb.io.out
  io.replay.req  <> replay_arb.io.out
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
