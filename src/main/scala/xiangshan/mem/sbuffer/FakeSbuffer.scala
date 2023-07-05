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

package xiangshan.mem

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils.{XSDebug, XSInfo}
import xiangshan._
import xiangshan.cache.{DCacheLineIO, DCacheWordReq, MemoryOpConstants, DCacheWordReqWithVaddr}

// Fake Store buffer for XiangShan Out of Order LSU
//
// Note: fake store buffer is out of date, as store buffer is now
// used as extended dcache miss queue for store
class FakeSbuffer(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val in = Vec(StorePipelineWidth, Flipped(Decoupled(new DCacheWordReqWithVaddr)))
    val dcache = new DCacheLineIO
    val forward = Vec(LoadPipelineWidth, Flipped(new LoadForwardQueryIO))
  })

  assert(!(io.in(1).valid && !io.in(0).valid))

  // assign default values to signals
  io.in(1).ready := false.B

  io.dcache.req.valid := false.B
  io.dcache.req.bits := DontCare
  io.dcache.resp.ready := false.B

  val s_invalid :: s_req :: s_resp :: Nil = Enum(3)

  val state = RegInit(s_invalid)

  val req = Reg(new DCacheWordReqWithVaddr)

  XSDebug("state: %d\n", state)

  io.in(0).ready := state === s_invalid

  def word_addr(addr: UInt) = (addr >> 3) << 3
  def block_addr(addr: UInt) = (addr >> 6) << 6

  // --------------------------------------------
  // s_invalid: receive requests
  when (state === s_invalid) {
    when (io.in(0).fire()) {
      req   := io.in(0).bits
      state := s_req
    }
  }

  val wdataVec = WireInit(VecInit(Seq.fill(8)(0.U(64.W))))
  val wmaskVec = WireInit(VecInit(Seq.fill(8)(0.U(8.W))))
  wdataVec(req.addr(5,3)) := req.data
  wmaskVec(req.addr(5,3)) := req.mask

  when (state === s_req) {
    val dcache_req = io.dcache.req
    dcache_req.valid := true.B
    dcache_req.bits.cmd  := MemoryOpConstants.M_XWR
    dcache_req.bits.addr := block_addr(req.addr)
    dcache_req.bits.data := wdataVec.asUInt
    dcache_req.bits.mask := wmaskVec.asUInt
    dcache_req.bits.id   := DontCare

    when (dcache_req.fire()) {
      state := s_resp
    }
  }

  when (state === s_resp) {
    io.dcache.resp.ready := true.B
    when (io.dcache.resp.fire()) {
      state := s_invalid
    }
  }

  // do forwarding here
  for (i <- 0 until LoadPipelineWidth) {
    val addr_match = word_addr(io.forward(i).paddr) === word_addr(req.addr)
    val mask = io.forward(i).mask & req.mask(7, 0)
    val mask_match = mask =/= 0.U
    val need_forward = state =/= s_invalid && addr_match && mask_match

    io.forward(i).forwardMask := Mux(need_forward, VecInit(mask.asBools),
      VecInit(0.U(8.W).asBools))
    io.forward(i).forwardData := VecInit((0 until 8) map {i => req.data((i + 1) * 8 - 1, i * 8)})
  }

  XSInfo(io.in(0).fire(), "ensbuffer addr 0x%x wdata 0x%x mask %b\n", io.in(0).bits.addr, io.in(0).bits.data, io.in(0).bits.mask)
  XSInfo(io.in(1).fire(), "ensbuffer addr 0x%x wdata 0x%x mask %b\n", io.in(1).bits.addr, io.in(1).bits.data, io.in(0).bits.mask)
  XSInfo(io.dcache.req.fire(), "desbuffer addr 0x%x wdata 0x%x mask %b\n", io.dcache.req.bits.addr, io.dcache.req.bits.data, io.dcache.req.bits.mask)
}
