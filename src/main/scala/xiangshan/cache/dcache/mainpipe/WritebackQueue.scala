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

  val delay_release = Bool()
  val miss_id = UInt(log2Up(cfg.nMissEntries).W)

  def dump() = {
    XSDebug("WritebackReq addr: %x param: %d voluntary: %b hasData: %b data: %x\n",
      addr, param, voluntary, hasData, data)
  }
}

// While a Release sleeps and waits for a refill to wake it up,
// main pipe might update meta & data during this time.
// So the meta & data to be released need to be updated too.
class ReleaseUpdate(implicit p: Parameters) extends DCacheBundle {
  // only consider store here
  val addr = UInt(PAddrBits.W)
  val mask = UInt(DCacheBanks.W)
  val data = UInt((cfg.blockBytes * 8).W)
}

class WritebackEntry(edge: TLEdgeOut)(implicit p: Parameters) extends DCacheModule with HasTLDump
{
  val io = IO(new Bundle {
    val id = Input(UInt())

    val req = Flipped(DecoupledIO(new WritebackReq))
    val merge = Output(Bool())
    val mem_release = DecoupledIO(new TLBundleC(edge.bundle))
    val mem_grant = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))

    val block_addr  = Output(Valid(UInt()))

    val release_wakeup = Flipped(ValidIO(UInt(log2Up(cfg.nMissEntries).W)))
    val release_update = Flipped(ValidIO(new ReleaseUpdate))
  })

  val s_invalid :: s_sleep :: s_release_req :: s_release_resp :: Nil = Enum(4)
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

  def mergeData(old_data: UInt, new_data: UInt, wmask: UInt): UInt = {
    val full_wmask = FillInterleaved(64, wmask)
    (~full_wmask & old_data | full_wmask & new_data)
  }

  // --------------------------------------------------------------------------------
  // s_invalid: receive requests
  // new req entering
  when (io.req.fire()) {
    assert (remain === 0.U)
    req := io.req.bits
    when (io.req.bits.delay_release) {
      state := s_sleep
    }.otherwise {
      state := s_release_req
      remain_set := Mux(io.req.bits.hasData, ~0.U(refillCycles.W), 1.U(refillCycles.W))
    }
  }

  // --------------------------------------------------------------------------------
  // s_sleep: wait for refill pipe to inform me that I can keep releasing
  val merge_probe = WireInit(false.B)
  io.merge := WireInit(false.B)
  when (state === s_sleep) {
    assert (remain === 0.U)

    val update = io.release_update.valid && io.release_update.bits.addr === req.addr
    when (update) {
      req.hasData := req.hasData || io.release_update.bits.mask.orR
      req.dirty := req.dirty || io.release_update.bits.mask.orR
      req.data := mergeData(req.data, io.release_update.bits.data, io.release_update.bits.mask)
    }

    io.merge := !io.req.bits.voluntary && io.req.bits.addr === req.addr
    merge_probe := io.req.valid && io.merge
    when (merge_probe) {
      state := s_release_req
      req.voluntary := false.B
      req.hasData := req.hasData || io.req.bits.hasData
      req.dirty := req.dirty || io.req.bits.dirty
      req.data := Mux(
        io.req.bits.hasData,
        io.req.bits.data,
        req.data
      )
      req.delay_release := false.B
      remain_set := Mux(req.hasData || io.req.bits.hasData, ~0.U(refillCycles.W), 1.U(refillCycles.W))
    }.elsewhen (io.release_wakeup.valid && io.release_wakeup.bits === req.miss_id) {
      state := s_release_req
      req.delay_release := false.B
      remain_set := Mux(req.hasData || update && io.release_update.bits.mask.orR, ~0.U(refillCycles.W), 1.U(refillCycles.W))
    }
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
    assert(!req.dirty || req.hasData)
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

  // When does this entry merge a new req?
  // 1. When this entry is free
  // 2. When this entry wants to release while still waiting for release_wakeup signal,
  //    and a probe req with the same addr comes. In this case we merge probe with release,
  //    handle this probe, so we don't need another release.
  io.req.ready := state === s_invalid ||
    state === s_sleep && !io.req.bits.voluntary && io.req.bits.addr === req.addr

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

    val release_wakeup = Flipped(ValidIO(UInt(log2Up(cfg.nMissEntries).W)))
    val release_update = Flipped(ValidIO(new ReleaseUpdate))

    val miss_req  = Flipped(Valid(UInt()))
    val block_miss_req  = Output(Bool())
  })

  // allocate a free entry for incoming request
  val primary_ready  = Wire(Vec(cfg.nReleaseEntries, Bool()))
  val merge_vec = Wire(Vec(cfg.nReleaseEntries, Bool()))
  val allocate = primary_ready.asUInt.orR
  val merge = merge_vec.asUInt.orR
  val alloc_idx = PriorityEncoder(Mux(merge, merge_vec, primary_ready))

  val req = io.req
  val block_conflict = Wire(Bool())
  val accept = merge || allocate && !block_conflict
  req.ready := accept

  // assign default values to output signals
  io.mem_release.valid := false.B
  io.mem_release.bits  := DontCare
  io.mem_grant.ready   := false.B

  require(isPow2(cfg.nMissEntries))
  val grant_source = io.mem_grant.bits.source(log2Up(cfg.nReleaseEntries) - 1, 0)
  val entries = (0 until cfg.nReleaseEntries) map { i =>
    val entry = Module(new WritebackEntry(edge))

    entry.io.id := (i + releaseIdBase).U

    // entry req
    entry.io.req.valid := (i.U === alloc_idx) && req.valid && accept
    primary_ready(i)   := entry.io.req.ready
    merge_vec(i) := entry.io.merge
    entry.io.req.bits  := req.bits

    entry.io.mem_grant.valid := (i.U === grant_source) && io.mem_grant.valid
    entry.io.mem_grant.bits  := io.mem_grant.bits
    when (i.U === grant_source) {
      io.mem_grant.ready := entry.io.mem_grant.ready
    }

    entry.io.release_wakeup := io.release_wakeup
    entry.io.release_update := io.release_update

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
