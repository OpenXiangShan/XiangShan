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

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink.TLPermissions._
import freechips.rocketchip.tilelink.{TLArbiter, TLBundleC, TLBundleD, TLEdgeOut}
import org.chipsalliance.cde.config.Parameters
import utils.{HasPerfEvents, HasTLDump, XSDebug, XSPerfAccumulate}

class WritebackReqCtrl(implicit p: Parameters) extends DCacheBundle {
  val param  = UInt(cWidth.W)
  val voluntary = Bool()
  val hasData = Bool()
  val dirty = Bool()

  val delay_release = Bool()
  val miss_id = UInt(log2Up(cfg.nMissEntries).W)
}

class WritebackReqWodata(implicit p: Parameters) extends WritebackReqCtrl {
  val addr = UInt(PAddrBits.W)

  def dump() = {
    XSDebug("WritebackReq addr: %x param: %d voluntary: %b hasData: %b\n",
      addr, param, voluntary, hasData)
  }
}

class WritebackReqData(implicit p: Parameters) extends DCacheBundle {
  val data = UInt((cfg.blockBytes * 8).W)
}

class WritebackReq(implicit p: Parameters) extends WritebackReqWodata {
  val data = UInt((cfg.blockBytes * 8).W)

  override def dump() = {
    XSDebug("WritebackReq addr: %x param: %d voluntary: %b hasData: %b data: %x\n",
      addr, param, voluntary, hasData, data)
  }

  def toWritebackReqWodata(): WritebackReqWodata = {
    val out = Wire(new WritebackReqWodata)
    out.addr := addr
    out.param := param
    out.voluntary := voluntary
    out.hasData := hasData
    out.dirty := dirty
    out.delay_release := delay_release
    out.miss_id := miss_id
    out
  }

  def toWritebackReqCtrl(): WritebackReqCtrl = {
    val out = Wire(new WritebackReqCtrl)
    out.param := param
    out.voluntary := voluntary
    out.hasData := hasData
    out.dirty := dirty
    out.delay_release := delay_release
    out.miss_id := miss_id
    out
  }

  def toWritebackReqData(): WritebackReqData = {
    val out = Wire(new WritebackReqData)
    out.data := data
    out
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

// To reduce fanout, writeback queue entry data is updated 1 cycle
// after ReleaseUpdate.fire
class WBQEntryReleaseUpdate(implicit p: Parameters) extends DCacheBundle {
  // only consider store here
  val addr = UInt(PAddrBits.W)
  val mask_delayed = UInt(DCacheBanks.W)
  val data_delayed = UInt((cfg.blockBytes * 8).W)
  val mask_orr = Bool()
}

// When a probe TtoB req enter dcache main pipe, check if that cacheline
// is waiting for release. If it is so, change TtoB to TtoN, set dcache
// coh to N.
class ProbeToBCheckReq(implicit p: Parameters) extends DCacheBundle {
  val addr = UInt(PAddrBits.W) // paddr from mainpipe s1
}

class ProbeToBCheckResp(implicit p: Parameters) extends DCacheBundle {
  val toN = Bool() // need to set dcache coh to N
}

class WritebackEntry(edge: TLEdgeOut)(implicit p: Parameters) extends DCacheModule with HasTLDump
{
  val io = IO(new Bundle {
    val id = Input(UInt())

    val req = Flipped(DecoupledIO(new WritebackReqWodata))
    val req_data = Input(new WritebackReqData)

    val mem_release = DecoupledIO(new TLBundleC(edge.bundle))
    val mem_grant = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))
    val primary_valid = Input(Bool())
    val primary_ready = Output(Bool())
    val primary_ready_dup = Vec(nDupWbReady, Output(Bool()))

    val block_addr  = Output(Valid(UInt()))
  })

  val s_invalid :: s_release_req :: s_release_resp ::Nil = Enum(3)
  val state = RegInit(s_invalid)
  val state_dup_0 = RegInit(s_invalid)
  val state_dup_1 = RegInit(s_invalid)
  val state_dup_for_mp = RegInit(VecInit(Seq.fill(nDupWbReady)(s_invalid)))

  val remain = RegInit(0.U(refillCycles.W))
  val remain_dup_0 = RegInit(0.U(refillCycles.W))
  val remain_dup_1 = RegInit(0.U(refillCycles.W))
  val remain_set = WireInit(0.U(refillCycles.W))
  val remain_clr = WireInit(0.U(refillCycles.W))
  remain := (remain | remain_set) & ~remain_clr
  remain_dup_0 := (remain_dup_0 | remain_set) & ~remain_clr
  remain_dup_1 := (remain_dup_1 | remain_set) & ~remain_clr

  // writeback queue data
  val data = Reg(UInt((cfg.blockBytes * 8).W))

  // writeback queue paddr
  val paddr_dup_0 = Reg(UInt(PAddrBits.W))
  val paddr_dup_1 = Reg(UInt(PAddrBits.W))
  val paddr_dup_2 = Reg(UInt(PAddrBits.W))

  // pending data write
  // !s_data_override means there is an in-progress data write
  // val s_data_override = RegInit(true.B)
  // !s_data_merge means there is an in-progress data merge
  // val s_data_merge = RegInit(true.B)

  val busy = remain.orR

  val req = Reg(new WritebackReqWodata)

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
  val alloc = io.req.valid && io.primary_valid && io.primary_ready
  when (alloc) {
    assert (remain === 0.U)
    req := io.req.bits
    // only update paddr when allocate a new missqueue entry
    paddr_dup_0 := io.req.bits.addr
    paddr_dup_1 := io.req.bits.addr
    paddr_dup_2 := io.req.bits.addr
  
    remain_set := Mux(io.req.bits.hasData, ~0.U(refillCycles.W), 1.U(refillCycles.W))
    state      := s_release_req
    state_dup_0 := s_release_req
    state_dup_1 := s_release_req
    state_dup_for_mp.foreach(_ := s_release_req)
  }

  // --------------------------------------------------------------------------------
  // while there beats remaining to be sent, we keep sending
  // which beat to send in this cycle?
  val beat = PriorityEncoder(remain_dup_0)

  val beat_data = Wire(Vec(refillCycles, UInt(beatBits.W)))
  for (i <- 0 until refillCycles) {
    beat_data(i) := data((i + 1) * beatBits - 1, i * beatBits)
  }

  val probeResponse = edge.ProbeAck(
    fromSource = io.id,
    toAddress = paddr_dup_1,
    lgSize = log2Ceil(cfg.blockBytes).U,
    reportPermissions = req.param
  )

  val probeResponseData = edge.ProbeAck(
    fromSource = io.id,
    toAddress = paddr_dup_1,
    lgSize = log2Ceil(cfg.blockBytes).U,
    reportPermissions = req.param,
    data = beat_data(beat)
  )

  val voluntaryRelease = edge.Release(
    fromSource = io.id,
    toAddress = paddr_dup_2,
    lgSize = log2Ceil(cfg.blockBytes).U,
    shrinkPermissions = req.param
  )._2

  val voluntaryReleaseData = edge.Release(
    fromSource = io.id,
    toAddress = paddr_dup_2,
    lgSize = log2Ceil(cfg.blockBytes).U,
    shrinkPermissions = req.param,
    data = beat_data(beat)
  )._2

  // voluntaryReleaseData.echo.lift(DirtyKey).foreach(_ := req.dirty)
  when(busy) {
    assert(!req.dirty || req.hasData)
  }

  io.mem_release.valid := busy
  io.mem_release.bits  := Mux(req.voluntary,
    Mux(req.hasData, voluntaryReleaseData, voluntaryRelease),
    Mux(req.hasData, probeResponseData, probeResponse))

  
  when (io.mem_release.fire()) {remain_clr := PriorityEncoderOH(remain_dup_1)}

  val (_, _, release_done, _) = edge.count(io.mem_release)

  when(state === s_release_req && release_done){
    state := Mux(req.voluntary, s_release_resp, s_invalid)
    when(req.voluntary){
      state_dup_for_mp.foreach(_ := s_release_resp)
    } .otherwise{
      state_dup_for_mp.foreach(_ := s_invalid)
    }
  }

  io.primary_ready := state === s_invalid
  io.primary_ready_dup.zip(state_dup_for_mp).foreach { case (rdy, st) => rdy := st === s_invalid }
  // --------------------------------------------------------------------------------
  // receive ReleaseAck for Releases
  when (state === s_release_resp) {
    io.mem_grant.ready := true.B
    when (io.mem_grant.fire()) {
      state := s_invalid
      state_dup_for_mp.foreach(_ := s_invalid)
    }
  }
  when((req.hasData || RegNext(alloc))) {
    data := io.req_data.data
  }
  // performance counters
  XSPerfAccumulate("wb_req", io.req.fire)
  XSPerfAccumulate("wb_release", state === s_release_req && release_done && req.voluntary)
  XSPerfAccumulate("wb_probe_resp", state === s_release_req && release_done && !req.voluntary)
  XSPerfAccumulate("penalty_blocked_by_channel_C", io.mem_release.valid && !io.mem_release.ready)
  XSPerfAccumulate("penalty_waiting_for_channel_D", io.mem_grant.ready && !io.mem_grant.valid && state === s_release_resp)
}

class WritebackQueue(edge: TLEdgeOut)(implicit p: Parameters) extends DCacheModule with HasTLDump with HasPerfEvents
{
  val io = IO(new Bundle {
    val req = Flipped(DecoupledIO(new WritebackReq))
    val req_ready_dup = Vec(nDupWbReady, Output(Bool()))
    val mem_release = DecoupledIO(new TLBundleC(edge.bundle))
    val mem_grant = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))

    val miss_req = Flipped(Valid(UInt()))
    val block_miss_req = Output(Bool()) 
  })

  require(cfg.nReleaseEntries > cfg.nMissEntries)

  val primary_ready_vec = Wire(Vec(cfg.nReleaseEntries, Bool()))
  val alloc = Cat(primary_ready_vec).orR

  val req = io.req
  val block_conflict = Wire(Bool())

  req.ready := alloc && !block_conflict

  // assign default values to output signals
  io.mem_release.valid := false.B
  io.mem_release.bits  := DontCare
  io.mem_grant.ready   := false.B

  // delay data write in writeback req for 1 cycle
  val req_data = RegEnable(io.req.bits.toWritebackReqData(), io.req.valid)

  require(isPow2(cfg.nMissEntries))
  val grant_source = io.mem_grant.bits.source
  val entries = Seq.fill(cfg.nReleaseEntries)(Module(new WritebackEntry(edge)))
  entries.zipWithIndex.foreach {
    case (entry, i) =>
      val former_primary_ready = if(i == 0)
        false.B
      else
        Cat((0 until i).map(j => entries(j).io.primary_ready)).orR
      val entry_id = (i + releaseIdBase).U

      entry.io.id := entry_id

      // entry req
      entry.io.req.valid := req.valid && !block_conflict
      primary_ready_vec(i)   := entry.io.primary_ready
      entry.io.req.bits  := req.bits
      entry.io.req_data  := req_data

      entry.io.primary_valid := alloc &&
        !former_primary_ready &&
        entry.io.primary_ready

      entry.io.mem_grant.valid := (entry_id === grant_source) && io.mem_grant.valid
      entry.io.mem_grant.bits  := io.mem_grant.bits
      when (i.U === io.mem_grant.bits.source) {
        io.mem_grant.ready := entry.io.mem_grant.ready
      }
  }

  io.req_ready_dup.zipWithIndex.foreach { case (rdy, i) =>
    rdy := Cat(entries.map(_.io.primary_ready_dup(i))).orR
  }

  block_conflict := VecInit(entries.map(e => e.io.block_addr.valid && e.io.block_addr.bits === io.req.bits.addr)).asUInt.orR
  val miss_req_conflict = VecInit(entries.map(e => e.io.block_addr.valid && e.io.block_addr.bits === io.miss_req.bits)).asUInt.orR
  io.block_miss_req := io.miss_req.valid && miss_req_conflict

  TLArbiter.robin(edge, io.mem_release, entries.map(_.io.mem_release):_*)

  // sanity check
  // print all input/output requests for debug purpose
  // print req
  when(io.req.fire()) {
    io.req.bits.dump()
  }

  when(io.mem_release.fire()){
    io.mem_grant.bits.dump
  }

  when (io.miss_req.valid) {
    XSDebug("miss_req: addr: %x\n", io.miss_req.bits)
  }

  when (io.block_miss_req) {
    XSDebug("block_miss_req\n")
  }

  // performance counters
  XSPerfAccumulate("wb_req", io.req.fire)

  val perfValidCount = RegNext(PopCount(entries.map(e => e.io.block_addr.valid)))
  val perfEvents = Seq(
    ("dcache_wbq_req      ", io.req.fire),
    ("dcache_wbq_1_4_valid", (perfValidCount < (cfg.nReleaseEntries.U/4.U))),
    ("dcache_wbq_2_4_valid", (perfValidCount > (cfg.nReleaseEntries.U/4.U)) & (perfValidCount <= (cfg.nReleaseEntries.U/2.U))),
    ("dcache_wbq_3_4_valid", (perfValidCount > (cfg.nReleaseEntries.U/2.U)) & (perfValidCount <= (cfg.nReleaseEntries.U*3.U/4.U))),
    ("dcache_wbq_4_4_valid", (perfValidCount > (cfg.nReleaseEntries.U*3.U/4.U))),
  )
  generatePerfEvent()  

}