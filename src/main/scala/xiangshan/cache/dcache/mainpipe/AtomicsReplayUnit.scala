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
import utility.XSDebug

class AtomicsReplayEntry(implicit p: Parameters) extends DCacheModule
{
  val io = IO(new Bundle {
    val lsu  = Flipped(new AtomicWordIO)
    val pipeReq  = Decoupled(new MainPipeReq)
    val pipeResp = Flipped(ValidIO(new MainPipeResp))
    val blockLr = Input(Bool())

    val blockAddr  = Output(Valid(UInt()))
  })

  val sInvalid :: s_pipe_req :: s_pipe_resp :: s_resp :: Nil = Enum(4)
  val state = RegInit(sInvalid)

  val req = Reg(new DCacheWordReqWithVaddr)

  // assign default values to output signals
  io.lsu.req.ready     := state === sInvalid
  io.lsu.resp.valid    := false.B
  io.lsu.resp.bits     := DontCare

  io.pipeReq.valid    := false.B
  io.pipeReq.bits     := DontCare

  io.blockAddr.valid := state =/= sInvalid
  io.blockAddr.bits  := req.addr


  XSDebug(state =/= sInvalid, "AtomicsReplayEntry: state: %d block_addr: %x\n", state, io.blockAddr.bits)

  // --------------------------------------------
  // s_invalid: receive requests
  when (state === sInvalid) {
    when (io.lsu.req.fire) {
      req   := io.lsu.req.bits
      state := s_pipe_req
    }
  }

  // --------------------------------------------
  // replay
  when (state === s_pipe_req) {
    io.pipeReq.valid := Mux(
      io.pipeReq.bits.cmd === M_XLR,
      !io.blockLr, // block lr to survive in lr storm
      true.B
    )

    val pipeReq = io.pipeReq.bits
    pipeReq := DontCare
    pipeReq.miss := false.B
    pipeReq.probe := false.B
    pipeReq.probeNeedData := false.B
    pipeReq.source := AMO_SOURCE.U
    pipeReq.cmd    := req.cmd
    pipeReq.addr   := get_block_addr(req.addr)
    pipeReq.vaddr  := get_block_addr(req.vaddr)
    pipeReq.wordIdx  := get_word(req.addr)
    pipeReq.amoData  := req.data
    pipeReq.amoMask  := req.mask

    when (io.pipeReq.fire) {
      state := s_pipe_resp
      assert(!io.pipeReq.bits.vaddr === 0.U)
    }
  }

  val respData  = Reg(UInt())
  val respId    = Reg(UInt())
  val respError = Reg(Bool())
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
    when (io.pipeResp.fire) {
      when (io.pipeResp.bits.miss) {
        when (io.pipeResp.bits.replay) {
          state := s_pipe_req
        }
      } .otherwise {
        respData  := io.pipeResp.bits.data
        respId    := io.pipeResp.bits.id
        respError := io.pipeResp.bits.error
        state := s_resp
      }
    }
  }

  // --------------------------------------------
  when (state === s_resp) {
    io.lsu.resp.valid := true.B
    io.lsu.resp.bits  := DontCare
    io.lsu.resp.bits.data  := respData
    io.lsu.resp.bits.id    := respId
    io.lsu.resp.bits.error := respError

    when (io.lsu.resp.fire) {
      state := sInvalid
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
