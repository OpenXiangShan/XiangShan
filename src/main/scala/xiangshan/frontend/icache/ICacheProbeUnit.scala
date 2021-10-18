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

package  xiangshan.frontend.icache

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._

import freechips.rocketchip.tilelink.{TLEdgeOut, TLBundleB, TLMessages, TLPermissions}

import utils.{HasTLDump, XSDebug, XSPerfAccumulate}

class ProbeReq(implicit p: Parameters) extends ICacheBundle
{
  val source = UInt()
  val opcode = UInt()
  val addr   = UInt(PAddrBits.W)
  // TODO: l2 should use vaddr index to probe l1
  val vaddr  = UInt(VAddrBits.W)
  val param  = UInt(TLPermissions.bdWidth.W)
  val needData = Bool()

}

class ICacheProbeEntry(implicit p: Parameters) extends ICacheModule {
  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new ProbeReq))
    val pipe_req  = DecoupledIO(new ICacheProbeReq)
  })

  val s_invalid :: s_pipe_req :: Nil = Enum(2)

  val state = RegInit(s_invalid)

  val req = Reg(new ProbeReq)

  // assign default values to signals
  io.req.ready      := false.B
  io.pipe_req.valid := false.B
  io.pipe_req.bits  := DontCare

//   io.block_addr.valid := state =/= s_invalid

  when (state === s_invalid) {
    io.req.ready := true.B
    when (io.req.fire()) {
      req := io.req.bits
      state := s_pipe_req
    }
  }

  when (state === s_pipe_req) {
    io.pipe_req.valid := true.B

    val pipe_req = io.pipe_req.bits
    pipe_req := DontCare
    pipe_req.probe_param := req.param
    pipe_req.addr   := req.addr
    pipe_req.vaddr  := req.vaddr

    when (io.pipe_req.fire()) {
      state := s_invalid
    }
  }

//  // perfoemance counters
//  XSPerfAccumulate("probe_req", state === s_invalid && io.req.fire())
//  XSPerfAccumulate("probe_penalty", state =/= s_invalid)
//  XSPerfAccumulate("probe_penalty_blocked_by_lrsc", state === s_pipe_req && io.lrsc_locked_block.valid && io.lrsc_locked_block.bits === req.addr)
//  XSPerfAccumulate("probe_penalty_blocked_by_pipeline", state === s_pipe_req && io.pipe_req.valid && !io.pipe_req.ready)
}

class ICacheProbeQueue(edge: TLEdgeOut)(implicit p: Parameters) extends ICacheModule with HasTLDump
{
  val io = IO(new Bundle {
    val mem_probe = Flipped(Decoupled(new TLBundleB(edge.bundle)))
    val pipe_req  = DecoupledIO(new ICacheProbeReq)
  })

  val pipe_req_arb = Module(new RRArbiter(new ICacheProbeReq, cacheParams.nProbeEntries))

  // allocate a free entry for incoming request
  val primary_ready  = Wire(Vec(cacheParams.nProbeEntries, Bool()))
  val allocate = primary_ready.asUInt.orR
  val alloc_idx = PriorityEncoder(primary_ready)

  // translate to inner req
  val req = Wire(new ProbeReq)
  val alias_addr_frag = io.mem_probe.bits.data(2, 1) // add extra 2 bits from vaddr to get vindex
  req.source := io.mem_probe.bits.source
  req.opcode := io.mem_probe.bits.opcode
  req.addr := io.mem_probe.bits.address
  if(ICacheAboveIndexOffset > ICacheTagOffset) {
    // have alias problem, extra alias bits needed for index
    req.vaddr := Cat(
      io.mem_probe.bits.address(VAddrBits - 1, ICacheAboveIndexOffset), // dontcare
      alias_addr_frag(ICacheAboveIndexOffset - ICacheTagOffset - 1, 0), // index
      io.mem_probe.bits.address(ICacheTagOffset - 1, 0)                 // index & others
    )
  } else { // no alias problem
    req.vaddr := io.mem_probe.bits.address
  }
  req.param := io.mem_probe.bits.param
  req.needData := io.mem_probe.bits.data(0)

  io.mem_probe.ready := allocate

  val entries = (0 until cacheParams.nProbeEntries) map { i =>
    val entry = Module(new ICacheProbeEntry)

    // entry req
    entry.io.req.valid := (i.U === alloc_idx) && allocate && io.mem_probe.valid
    primary_ready(i)   := entry.io.req.ready
    entry.io.req.bits  := req

    // pipe_req
    pipe_req_arb.io.in(i) <> entry.io.pipe_req

    entry
  }

  io.pipe_req <> pipe_req_arb.io.out

}

class ICacheProbe(implicit p: Parameters) extends ICacheModule{
  val io = IO(new Bundle{
    val req = Flipped(DecoupledIO(new ICacheProbeReq))

    val meta_read = DecoupledIO(new ICacheReadBundle)
    val data_read = DecoupledIO(new ICacheReadBundle)

    val meta_response     = Input(new ICacheMetaRespBundle)
    val data_response     = Input(new ICacheDataRespBundle)

    val meta_write = DecoupledIO(new ICacheMetaWriteBundle)

    val release_req = DecoupledIO(new RealeaseReq)
  })

  val s_idle :: s_read_array :: s_send_release :: s_write_back :: s_send_grant_ack :: s_wait_resp :: Nil = Enum(6)
  val state = RegInit(s_idle)

  val req = Reg(new ICacheProbeReq)
  val req_vidx = get_idx(req.vaddr)

  val meta_read = Wire(new ICacheMetadata)
  val data_read = Wire(UInt((blockBits).W))

  meta_read := DontCare
  data_read := DontCare

  io.req.ready := state === s_idle

  val (_, probe_shrink_param, probe_new_coh) = meta_read.coh.onProbe(req.probe_param)

  io.release_req.valid          := state === s_send_release
  io.release_req.bits.addr      := req.addr
  io.release_req.bits.param     := probe_shrink_param
  io.release_req.bits.voluntary := false.B
  io.release_req.bits.hasData   := true.B
  io.release_req.bits.data      := data_read

  io.meta_read.valid := state === s_read_array
  io.meta_read.bits.isDoubleLine := false.B
  io.meta_read.bits.vSetIdx(0) := req_vidx
  io.meta_read.bits.vSetIdx(1) := DontCare

  io.data_read.valid := state === s_read_array
  io.data_read.bits.isDoubleLine := false.B
  io.data_read.bits.vSetIdx(0) := req_vidx
  io.data_read.bits.vSetIdx(1) := DontCare

  val phy_tag = get_phy_tag(req.addr)
  val hit_vec = VecInit(io.meta_response.metaData(0).zipWithIndex.map{case(way,i) => way.tag === phy_tag && way.coh.isValid() && io.meta_response.valid(0)(i)})
  val hit_data = Mux1H(hit_vec, io.data_response.datas(0))
  val hit_coh  = Mux1H(hit_vec, VecInit(io.meta_response.metaData(0).map(way => way.coh)))

  io.meta_write.valid := (state === s_write_back)
  io.meta_write.bits.generate(tag = meta_read.tag, coh = probe_new_coh, idx = get_idx(req.vaddr), waymask = hit_vec.asUInt, bankIdx = req_vidx(0))

  //state change
  switch(state) {
    is(s_idle) {
      when(io.req.valid) {
        state := s_read_array
        req := io.req.bits
      }
    }

    // memory request
    is(s_read_array) {
      when(io.meta_read.fire() && io.data_read.fire()) {
        state := s_send_release
      }
    }

    is(s_send_release) {
      meta_read.tag := phy_tag
      meta_read.coh := hit_coh
      data_read     := hit_data
      when(io.release_req.fire()){
        state := s_write_back
      }
    }

    is(s_write_back) {
      state := Mux(io.meta_write.fire() , s_idle, s_write_back)
    }

  }
}