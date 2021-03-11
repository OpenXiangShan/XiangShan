package xiangshan.cache

import chisel3._
import chisel3.util._

import utils.{XSDebug, XSPerf}
import bus.tilelink._

class StoreReplayEntry extends DCacheModule
{
  val io = IO(new Bundle {
    val id = Input(UInt())

    val lsu  = Flipped(new DCacheLineIO)
    val pipe_req  = Decoupled(new MainPipeReq)
    val pipe_resp = Flipped(ValidIO(new MainPipeResp))

    val block_addr  = Output(Valid(UInt()))
  })

  val s_invalid :: s_pipe_req :: s_pipe_resp :: s_wait :: s_resp :: Nil = Enum(5)
  val state = RegInit(s_invalid)

  val req = Reg(new DCacheLineReq)

  // assign default values to output signals
  io.lsu.req.ready     := state === s_invalid
  io.lsu.resp.valid    := false.B
  io.lsu.resp.bits     := DontCare

  io.pipe_req.valid    := false.B
  io.pipe_req.bits     := DontCare

  io.block_addr.valid := state =/= s_invalid
  io.block_addr.bits  := req.addr


  when (state =/= s_invalid) {
    XSDebug("StoreReplayEntry: %d state: %d block_addr: %x\n", io.id, state, io.block_addr.bits)
  }

  // --------------------------------------------
  // s_invalid: receive requests
  when (state === s_invalid) {
    when (io.lsu.req.fire()) {
      req   := io.lsu.req.bits
      state := s_pipe_req
    }
  }

  // --------------------------------------------
  // replay
  when (state === s_pipe_req) {
    io.pipe_req.valid := true.B

    val pipe_req = io.pipe_req.bits
    pipe_req := DontCare
    pipe_req.miss := false.B
    pipe_req.probe := false.B
    pipe_req.source := STORE_SOURCE.U
    pipe_req.cmd    := req.cmd
    pipe_req.addr   := req.addr
    pipe_req.store_data  := req.data
    pipe_req.store_mask  := req.mask
    pipe_req.id := io.id

    when (io.pipe_req.fire()) {
      state := s_pipe_resp
    }
  }

  val ReplayDelayCycles = 16
  val delay_counter = Counter(ReplayDelayCycles)

  when (state === s_pipe_resp) {
    // when not miss
    // everything is OK, simply send response back to sbuffer
    // when miss and not replay
    // wait for missQueue to handling miss and replaying our request
    // when miss and replay
    // req missed and fail to enter missQueue, manually replay it later
    when (io.pipe_resp.fire()) {
      when (io.pipe_resp.bits.miss) {
        when (io.pipe_resp.bits.replay) {
          delay_counter.value := 0.U
          state := s_wait
        }
      } .otherwise {
        state := s_resp
      }
    }
  }

  when (state === s_wait) {
    delay_counter.inc()
    when (delay_counter.value === (ReplayDelayCycles - 1).U) {
      state := s_pipe_req
    }
  }

  // --------------------------------------------
  when (state === s_resp) {
    io.lsu.resp.valid := true.B
    io.lsu.resp.bits  := DontCare
    io.lsu.resp.bits.id := req.id

    when (io.lsu.resp.fire()) {
      state := s_invalid
    }
  }

  // debug output
  when (io.lsu.req.fire()) {
    XSDebug(s"StoreReplayEntryTransaction req %d\n", io.id)
  }

  when (io.lsu.resp.fire()) {
    XSDebug(s"StoreReplayEntryTransaction resp %d\n", io.id)
  }

  // performance counters
  XSPerf("store_req", io.lsu.req.fire())
  XSPerf("store_penalty", state =/= s_invalid)
  // this is useless
  // XSPerf("store_hit", state === s_pipe_resp && io.pipe_resp.fire() && !io.pipe_resp.bits.miss)
  XSPerf("store_replay", state === s_pipe_resp && io.pipe_resp.fire() && io.pipe_resp.bits.miss && io.pipe_resp.bits.replay)
  XSPerf("store_miss", state === s_pipe_resp && io.pipe_resp.fire() && io.pipe_resp.bits.miss)
}


class StoreReplayQueue extends DCacheModule
{
  val io = IO(new Bundle {
    val lsu       = Flipped(new DCacheLineIO)
    val pipe_req  = Decoupled(new MainPipeReq)
    val pipe_resp = Flipped(ValidIO(new MainPipeResp))
  })

  val pipe_req_arb = Module(new RRArbiter(new MainPipeReq, cfg.nStoreReplayEntries))
  val resp_arb     = Module(new RRArbiter(new DCacheLineResp, cfg.nStoreReplayEntries))

  // allocate a free entry for incoming request
  val primary_ready  = Wire(Vec(cfg.nStoreReplayEntries, Bool()))
  val allocate = primary_ready.asUInt.orR
  val alloc_idx = PriorityEncoder(primary_ready)

  val req = io.lsu.req
  // do not use block conflict in req allocate path
  // compare with all entries incus much latency
  req.ready := allocate

  val entries = (0 until cfg.nStoreReplayEntries) map { i =>
    val entry = Module(new StoreReplayEntry)

    entry.io.id := i.U

    // entry req
    entry.io.lsu.req.valid := (i.U === alloc_idx) && allocate && req.valid
    primary_ready(i)       := entry.io.lsu.req.ready
    entry.io.lsu.req.bits  := req.bits

    // lsu req and resp
    resp_arb.io.in(i)  <> entry.io.lsu.resp

    // replay req and resp
    pipe_req_arb.io.in(i) <> entry.io.pipe_req

    entry.io.pipe_resp.valid := (i.U === io.pipe_resp.bits.id) && io.pipe_resp.valid
    entry.io.pipe_resp.bits  := io.pipe_resp.bits

    entry
  }

  io.lsu.resp  <> resp_arb.io.out
  io.pipe_req  <> pipe_req_arb.io.out

  val block_conflict = VecInit(entries.map(e => e.io.block_addr.valid && e.io.block_addr.bits === io.lsu.req.bits.addr)).asUInt.orR

  // sanity check
  when (io.lsu.req.valid) {
    assert(io.lsu.req.bits.cmd === M_XWR)
    assert (!block_conflict)
  }

  // debug output
  when (io.lsu.req.fire()) {
    io.lsu.req.bits.dump()
  }

  when (io.lsu.resp.fire()) {
    io.lsu.resp.bits.dump()
  }

  when (io.pipe_req.fire()) {
    io.pipe_req.bits.dump()
  }

  when (io.pipe_resp.fire()) {
    io.pipe_resp.bits.dump()
  }

  // performance counters
  XSPerf("store_req", io.lsu.req.fire())
}
