/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package xiangshan.cache

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utils.XSDebug

class AtomicsReplayEntry(implicit p: Parameters) extends DCacheModule
{
  val io = IO(new Bundle {
    val lsu  = Flipped(new AtomicWordIO)
    val pipe_req  = Decoupled(new MainPipeReq)
    val pipe_resp = Flipped(ValidIO(new MainPipeResp))
    val block_lr = Input(Bool())

    val block_addr  = Output(Valid(UInt()))
  })

  val s_invalid :: s_pipe_req :: s_pipe_resp :: s_resp :: Nil = Enum(4)
  val state = RegInit(s_invalid)

  val req = Reg(new DCacheWordReqWithVaddr)

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
    when (io.lsu.req.fire) {
      req   := io.lsu.req.bits
      state := s_pipe_req
    }
  }

  // --------------------------------------------
  // replay
  when (state === s_pipe_req) {
    io.pipe_req.valid := Mux(
      io.pipe_req.bits.cmd === M_XLR,
      !io.block_lr, // block lr to survive in lr storm
      true.B
    )

    val pipe_req = io.pipe_req.bits
    pipe_req := DontCare
    pipe_req.miss := false.B
    pipe_req.probe := false.B
    pipe_req.probe_need_data := false.B
    pipe_req.source := AMO_SOURCE.U
    pipe_req.cmd    := req.cmd
    pipe_req.addr   := get_block_addr(req.addr)
    pipe_req.vaddr  := get_block_addr(req.vaddr)
    pipe_req.word_idx  := get_word(req.addr)
    pipe_req.amo_data  := req.data
    pipe_req.amo_mask  := req.mask

    when (io.pipe_req.fire) {
      state := s_pipe_resp
      assert(!io.pipe_req.bits.vaddr === 0.U)
    }
  }

  val resp_data  = Reg(UInt())
  val resp_id    = Reg(UInt())
  val resp_error = Reg(Bool())
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
    when (io.pipe_resp.fire) {
      when (io.pipe_resp.bits.miss) {
        when (io.pipe_resp.bits.replay) {
          state := s_pipe_req
        }
      } .otherwise {
        resp_data  := io.pipe_resp.bits.data
        resp_id    := io.pipe_resp.bits.id
        resp_error := io.pipe_resp.bits.error
        state := s_resp
      }
    }
  }

  // --------------------------------------------
  when (state === s_resp) {
    io.lsu.resp.valid := true.B
    io.lsu.resp.bits  := DontCare
    io.lsu.resp.bits.data  := resp_data
    io.lsu.resp.bits.id    := resp_id
    io.lsu.resp.bits.error := resp_error

    when (io.lsu.resp.fire) {
      state := s_invalid
    }
  }

  // debug output
  // when (io.lsu.req.fire) {
  //   io.lsu.req.bits.dump()
  // }

  // when (io.lsu.resp.fire) {
  //   io.lsu.resp.bits.dump()
  // }

//  when (io.pipe_req.fire) {
//    io.pipe_req.bits.dump()
//  }
//
//  when (io.pipe_resp.fire) {
//    io.pipe_resp.bits.dump()
//  }
}