package xiangshan.cache

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils.XSDebug

class AtomicsReplayEntry(implicit p: Parameters) extends DCacheModule
{
  val io = IO(new Bundle {
    val lsu  = Flipped(new DCacheWordIO)
    val pipe_req  = Decoupled(new MainPipeReq)
    val pipe_resp = Flipped(ValidIO(new MainPipeResp))

    val block_addr  = Output(Valid(UInt()))
  })

  val s_invalid :: s_pipe_req :: s_pipe_resp :: s_resp :: Nil = Enum(4)
  val state = RegInit(s_invalid)

  val req = Reg(new DCacheWordReq)

  // assign default values to output signals
  io.lsu.req.ready     := state === s_invalid
  io.lsu.resp.valid    := false.B
  io.lsu.resp.bits     := DontCare

  io.pipe_req.valid    := false.B
  io.pipe_req.bits     := DontCare

  io.block_addr.valid := state =/= s_invalid
  io.block_addr.bits  := req.addr


  when (state =/= s_invalid) {
    XSDebug("AtomicsReplayEntry: state: %d block_addr: %x\n", state, io.block_addr.bits)
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
    pipe_req.source := AMO_SOURCE.U
    pipe_req.cmd    := req.cmd
    pipe_req.addr   := get_block_addr(req.addr)
    pipe_req.word_idx  := get_word(req.addr)
    pipe_req.amo_data  := req.data
    pipe_req.amo_mask  := req.mask

    when (io.pipe_req.fire()) {
      state := s_pipe_resp
    }
  }

  val resp_data = Reg(UInt())
  val resp_id   = Reg(UInt())
  when (state === s_pipe_resp) {
    // when not miss
    // everything is OK, simply send response back to sbuffer
    // when miss and not replay
    // wait for missQueue to handling miss and replaying our request
    // when miss and replay
    // req missed and fail to enter missQueue, manually replay it later
    // TODO: add assertions:
    // 1. add a replay delay counter?
    // 2. when req gets into MissQueue, it should not miss any more
    when (io.pipe_resp.fire()) {
      when (io.pipe_resp.bits.miss) {
        when (io.pipe_resp.bits.replay) {
          state := s_pipe_req
        }
      } .otherwise {
        resp_data := io.pipe_resp.bits.data
        resp_id   := io.pipe_resp.bits.id
        state := s_resp
      }
    }
  }

  // --------------------------------------------
  when (state === s_resp) {
    io.lsu.resp.valid := true.B
    io.lsu.resp.bits  := DontCare
    io.lsu.resp.bits.data := resp_data
    io.lsu.resp.bits.id   := resp_id

    when (io.lsu.resp.fire()) {
      state := s_invalid
    }
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
}
