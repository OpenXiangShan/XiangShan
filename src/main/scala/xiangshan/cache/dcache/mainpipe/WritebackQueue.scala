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

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils.{HasTLDump, XSDebug, XSPerfAccumulate}
import freechips.rocketchip.tilelink.{TLArbiter, TLBundleC, TLBundleD, TLEdgeOut, TLPermissions}
import huancun.{DirtyField, DirtyKey}

class WritebackReq(implicit p: Parameters) extends DCacheBundle {
  val addr = UInt(PAddrBits.W)
  val param  = UInt(TLPermissions.cWidth.W)
  val voluntary = Bool()
  val hasData = Bool()
  val dirty = Bool()
  val data = UInt((cfg.blockBytes * 8).W)

  def dump() = {
    XSDebug("WritebackReq addr: %x param: %d voluntary: %b hasData: %b data: %x\n",
      addr, param, voluntary, hasData, data)
  }
}

class WritebackEntry(edge: TLEdgeOut)(implicit p: Parameters) extends DCacheModule with HasTLDump
{
  val io = IO(new Bundle {
    val id = Input(UInt())

    val req = Flipped(DecoupledIO(new WritebackReq))
    val mem_release = DecoupledIO(new TLBundleC(edge.bundle))
    val mem_grant = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))

    val block_addr  = Output(Valid(UInt()))
  })

  val s_invalid :: s_release_req :: s_release_resp :: Nil = Enum(3)
  val state = RegInit(s_invalid)

  // internal regs
  // remaining beats
  val remain = RegInit(0.U(refillCycles.W))
  val remain_set = WireInit(0.U(refillCycles.W))
  val remain_clr = WireInit(0.U(refillCycles.W))
  remain := (remain | remain_set) & ~remain_clr

  val busy = remain.orR

  val req  = Reg(new WritebackReq)

  // assign default signals to output signals
  io.req.ready := false.B
  io.mem_release.valid := false.B
  io.mem_release.bits  := DontCare
  io.mem_grant.ready   := false.B
  io.block_addr.valid  := state =/= s_invalid
  io.block_addr.bits   := req.addr


  when (state =/= s_invalid) {
    XSDebug("WritebackEntry: %d state: %d block_addr: %x\n", io.id, state, io.block_addr.bits)
  }

  // --------------------------------------------------------------------------------
  // s_invalid: receive requests
  // new req entering
  io.req.ready := state === s_invalid
  when (io.req.fire()) {
    assert (remain === 0.U)
    remain_set := Mux(io.req.bits.hasData, ~0.U(refillCycles.W), 1.U(refillCycles.W))
    req        := io.req.bits
    state      := s_release_req
  }

  // --------------------------------------------------------------------------------
  // while there beats remaining to be sent, we keep sending
  // which beat to send in this cycle?
  val beat = PriorityEncoder(remain)

  val beat_data = Wire(Vec(refillCycles, UInt(beatBits.W)))
  for (i <- 0 until refillCycles) {
    beat_data(i) := req.data((i + 1) * beatBits - 1, i * beatBits)
  }

  val probeResponse = edge.ProbeAck(
    fromSource = io.id,
    toAddress = req.addr,
    lgSize = log2Ceil(cfg.blockBytes).U,
    reportPermissions = req.param
  )

  val probeResponseData = edge.ProbeAck(
    fromSource = io.id,
    toAddress = req.addr,
    lgSize = log2Ceil(cfg.blockBytes).U,
    reportPermissions = req.param,
    data = beat_data(beat)
  )

  val voluntaryRelease = edge.Release(
    fromSource = io.id,
    toAddress = req.addr,
    lgSize = log2Ceil(cfg.blockBytes).U,
    shrinkPermissions = req.param
  )._2

  val voluntaryReleaseData = edge.Release(
    fromSource = io.id,
    toAddress = req.addr,
    lgSize = log2Ceil(cfg.blockBytes).U,
    shrinkPermissions = req.param,
    data = beat_data(beat)
  )._2

  voluntaryReleaseData.echo.lift(DirtyKey).foreach(_ := req.dirty)
  when(busy) {
    assert(!req.voluntary || req.hasData)
  }

  io.mem_release.valid := busy
  io.mem_release.bits  := Mux(req.voluntary,
    Mux(req.hasData, voluntaryReleaseData, voluntaryRelease),
    Mux(req.hasData, probeResponseData, probeResponse))

  when (io.mem_release.fire()) { remain_clr := PriorityEncoderOH(remain) }

  val (_, _, release_done, _) = edge.count(io.mem_release)

  when (state === s_release_req && release_done) {
    state := Mux(req.voluntary, s_release_resp, s_invalid)
  }

  // --------------------------------------------------------------------------------
  // receive ReleaseAck for Releases
  when (state === s_release_resp) {
    io.mem_grant.ready := true.B
    when (io.mem_grant.fire()) {
      state := s_invalid
    }
  }

  // performance counters
  XSPerfAccumulate("wb_req", io.req.fire())
  XSPerfAccumulate("wb_release", state === s_release_req && release_done && req.voluntary)
  XSPerfAccumulate("wb_probe_resp", state === s_release_req && release_done && !req.voluntary)
  XSPerfAccumulate("penalty_blocked_by_channel_C", io.mem_release.valid && !io.mem_release.ready)
  XSPerfAccumulate("penalty_waiting_for_channel_D", io.mem_grant.ready && !io.mem_grant.valid && state === s_release_resp)
}

class WritebackQueue(edge: TLEdgeOut)(implicit p: Parameters) extends DCacheModule with HasTLDump
{
  val io = IO(new Bundle {
    val req = Flipped(DecoupledIO(new WritebackReq))
    val mem_release = DecoupledIO(new TLBundleC(edge.bundle))
    val mem_grant = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))
    
    val miss_req  = Flipped(Valid(UInt()))
    val block_miss_req  = Output(Bool())
  })

  // allocate a free entry for incoming request
  val primary_ready  = Wire(Vec(cfg.nReleaseEntries, Bool()))
  val allocate = primary_ready.asUInt.orR
  val alloc_idx = PriorityEncoder(primary_ready)

  val req = io.req
  val block_conflict = Wire(Bool())
  req.ready := allocate && !block_conflict

  // assign default values to output signals
  io.mem_release.valid := false.B
  io.mem_release.bits  := DontCare
  io.mem_grant.ready   := false.B

  val entries = (0 until cfg.nReleaseEntries) map { i =>
    val entry = Module(new WritebackEntry(edge))

    entry.io.id := i.U

    // entry req
    entry.io.req.valid := (i.U === alloc_idx) && allocate && req.valid && !block_conflict
    primary_ready(i)   := entry.io.req.ready
    entry.io.req.bits  := req.bits

    entry.io.mem_grant.valid := (i.U === io.mem_grant.bits.source) && io.mem_grant.valid
    entry.io.mem_grant.bits  := io.mem_grant.bits
    when (i.U === io.mem_grant.bits.source) {
      io.mem_grant.ready := entry.io.mem_grant.ready
    }

    entry
  }

  block_conflict := VecInit(entries.map(e => e.io.block_addr.valid && e.io.block_addr.bits === io.req.bits.addr)).asUInt.orR
  val miss_req_conflict = VecInit(entries.map(e => e.io.block_addr.valid && e.io.block_addr.bits === io.miss_req.bits)).asUInt.orR
  io.block_miss_req := io.miss_req.valid && miss_req_conflict

  TLArbiter.robin(edge, io.mem_release, entries.map(_.io.mem_release):_*)

  // sanity check
  // print all input/output requests for debug purpose
  // print req
  when (io.req.fire()) {
    io.req.bits.dump()
  }

  when (io.mem_release.fire()) {
    io.mem_release.bits.dump
  }

  when (io.mem_grant.fire()) {
    io.mem_grant.bits.dump
  }

  when (io.miss_req.valid) {
    XSDebug("miss_req: addr: %x\n", io.miss_req.bits)
  }

  when (io.block_miss_req) {
    XSDebug("block_miss_req\n")
  }

  // performance counters
  XSPerfAccumulate("wb_req", io.req.fire())
}
