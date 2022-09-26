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
import freechips.rocketchip.tilelink.TLPermissions._
import freechips.rocketchip.tilelink.{TLArbiter, TLBundleC, TLBundleD, TLEdgeOut}
import huancun.DirtyKey
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
// after ReleaseUpdate.fire()
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
    // allocate this entry for new req
    val primary_valid = Input(Bool())
    // this entry is free and can be allocated to new reqs
    val primary_ready = Output(Bool())
    val primary_ready_dup = Vec(nDupWbReady, Output(Bool()))
    // this entry is busy, but it can merge the new req
    val secondary_valid = Input(Bool())
    val secondary_ready = Output(Bool())
    val req = Flipped(DecoupledIO(new WritebackReqWodata))
    val req_data = Input(new WritebackReqData)

    val mem_release = DecoupledIO(new TLBundleC(edge.bundle))
    val mem_grant = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))

    val block_addr  = Output(Valid(UInt()))

    val release_wakeup = Flipped(ValidIO(UInt(log2Up(cfg.nMissEntries).W)))
    val release_update = Flipped(ValidIO(new WBQEntryReleaseUpdate))

    val probe_ttob_check_req = Flipped(ValidIO(new ProbeToBCheckReq))
    val probe_ttob_check_resp = ValidIO(new ProbeToBCheckResp)
  })

  val s_invalid :: s_sleep :: s_release_req :: s_release_resp :: Nil = Enum(4)
  // ProbeAck:               s_invalid ->            s_release_req
  // ProbeAck merge Release: s_invalid ->            s_release_req
  // Release:                s_invalid -> s_sleep -> s_release_req -> s_release_resp
  // Release merge ProbeAck: s_invalid -> s_sleep -> s_release_req
  //                        (change Release into ProbeAck when Release is not fired)
  //                     or: s_invalid -> s_sleep -> s_release_req -> s_release_resp -> s_release_req
  //                        (send a ProbeAck after Release transaction is over)
  val state = RegInit(s_invalid)
  val state_dup_0 = RegInit(s_invalid)
  val state_dup_1 = RegInit(s_invalid)
  val state_dup_for_mp = RegInit(VecInit(Seq.fill(nDupWbReady)(s_invalid)))

  // internal regs
  // remaining beats
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
  val s_data_override = RegInit(true.B) 
  // !s_data_merge means there is an in-progress data merge
  val s_data_merge = RegInit(true.B) 

  // there are valid request that can be sent to release bus
  val busy = remain.orR && s_data_override && s_data_merge // have remain beats and data write finished

  val req  = Reg(new WritebackReqCtrl)

  // assign default signals to output signals
  io.req.ready := false.B
  io.mem_release.valid := false.B
  io.mem_release.bits  := DontCare
  io.mem_grant.ready   := false.B
  io.block_addr.valid  := state =/= s_invalid
  io.block_addr.bits   := paddr_dup_0

  s_data_override := true.B // data_override takes only 1 cycle
  s_data_merge := true.B // data_merge takes only 1 cycle


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
  when (io.req.valid && io.primary_valid && io.primary_ready) {
    assert (remain === 0.U)
    req := io.req.bits
    s_data_override := false.B
    // only update paddr when allocate a new missqueue entry
    paddr_dup_0 := io.req.bits.addr
    paddr_dup_1 := io.req.bits.addr
    paddr_dup_2 := io.req.bits.addr
    when (io.req.bits.delay_release) {
      state := s_sleep
      state_dup_0 := s_sleep
      state_dup_1 := s_sleep
      state_dup_for_mp.foreach(_ := s_sleep)
    }.otherwise {
      state := s_release_req
      state_dup_0 := s_release_req
      state_dup_1 := s_release_req
      state_dup_for_mp.foreach(_ := s_release_req)
      remain_set := Mux(io.req.bits.hasData, ~0.U(refillCycles.W), 1.U(refillCycles.W))
    }
  }

  // --------------------------------------------------------------------------------
  // s_sleep: wait for refill pipe to inform me that I can keep releasing
  val merge = io.secondary_valid && io.secondary_ready
  when (state === s_sleep) {
    assert(remain === 0.U)
    // There shouldn't be a new Release with the same addr in sleep state
    assert(!(merge && io.req.bits.voluntary))

    val update = io.release_update.valid && io.release_update.bits.addr === paddr_dup_0
    when (update) {
      req.hasData := req.hasData || io.release_update.bits.mask_orr
      req.dirty := req.dirty || io.release_update.bits.mask_orr
      s_data_merge := false.B
    }.elsewhen (merge) {
      state := s_release_req
      state_dup_0 := s_release_req
      state_dup_1 := s_release_req
      state_dup_for_mp.foreach(_ := s_release_req)
      req.voluntary := false.B
      req.param := req.param
      req.hasData := req.hasData || io.req.bits.hasData
      req.dirty := req.dirty || io.req.bits.dirty
      s_data_override := !io.req.bits.hasData // update data when io.req.bits.hasData
      req.delay_release := false.B
      remain_set := Mux(req.hasData || io.req.bits.hasData, ~0.U(refillCycles.W), 1.U(refillCycles.W))
    }

    when (io.release_wakeup.valid && io.release_wakeup.bits === req.miss_id) {
      state := s_release_req
      state_dup_0 := s_release_req
      state_dup_1 := s_release_req
      state_dup_for_mp.foreach(_ := s_release_req)
      req.delay_release := false.B
      remain_set := Mux(
        req.hasData || update && io.release_update.bits.mask_orr || merge && io.req.bits.hasData,
        ~0.U(refillCycles.W),
        1.U(refillCycles.W)
      )
    }
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

  voluntaryReleaseData.echo.lift(DirtyKey).foreach(_ := req.dirty)
  when(busy) {
    assert(!req.dirty || req.hasData)
  }

  io.mem_release.valid := busy
  io.mem_release.bits  := Mux(req.voluntary,
    Mux(req.hasData, voluntaryReleaseData, voluntaryRelease),
    Mux(req.hasData, probeResponseData, probeResponse))

  when (io.mem_release.fire()) { remain_clr := PriorityEncoderOH(remain_dup_1) }

  val (_, _, release_done, _) = edge.count(io.mem_release)

//  when (state === s_release_req && release_done) {
//    state := Mux(req.voluntary, s_release_resp, s_invalid)
//  }

  // Because now wbq merges a same-addr req unconditionally, when the req to be merged comes too late,
  // the previous req might not be able to merge. Thus we have to handle the new req later after the
  // previous one finishes.
  // TODO: initiate these
  val release_later = RegInit(false.B)
  val c_already_sent = RegInit(false.B)
  def tmp_req() = new Bundle {
    val param = UInt(cWidth.W)
    val voluntary = Bool()
    val hasData = Bool()
    val dirty = Bool()
    val delay_release = Bool()
    val miss_id = UInt(log2Up(cfg.nMissEntries).W)

    def toWritebackReqCtrl = {
      val r = Wire(new WritebackReqCtrl())
      r.param := param
      r.voluntary := voluntary
      r.hasData := hasData
      r.dirty := dirty
      r.delay_release := delay_release
      r.miss_id := miss_id
      r
    }
  }
  val req_later = Reg(tmp_req())

  when (state_dup_0 === s_release_req) {
    when (io.mem_release.fire()) {
      c_already_sent := !release_done
    }

    when (req.voluntary) {
      // The previous req is Release
      when (release_done) {
        state := s_release_resp
        state_dup_0 := s_release_resp
        state_dup_1 := s_release_resp
        state_dup_for_mp.foreach(_ := s_release_resp)
      }
      // merge a ProbeAck
      when (merge) {
        when (io.mem_release.fire() || c_already_sent) {
          // too late to merge, handle the ProbeAck later
          release_later := true.B
          req_later.param := io.req.bits.param
          req_later.voluntary := io.req.bits.voluntary
          req_later.hasData := io.req.bits.hasData
          req_later.dirty := io.req.bits.dirty
          req_later.delay_release := io.req.bits.delay_release
          req_later.miss_id := io.req.bits.miss_id
        }.otherwise {
          // Release hasn't been sent out yet, change Release to ProbeAck
          req.voluntary := false.B
          req.hasData := req.hasData || io.req.bits.hasData
          req.dirty := req.dirty || io.req.bits.dirty
          // s_data_override := false.B
          req.delay_release := false.B
          remain_set := Mux(req.hasData || io.req.bits.hasData, ~0.U(refillCycles.W), 1.U(refillCycles.W))
        }
      }
    }.otherwise {
      // The previous req is ProbeAck
      when (merge) {
        release_later := true.B
        req_later.param := io.req.bits.param
        req_later.voluntary := io.req.bits.voluntary
        req_later.hasData := io.req.bits.hasData
        req_later.dirty := io.req.bits.dirty
        req_later.delay_release := io.req.bits.delay_release
        req_later.miss_id := io.req.bits.miss_id
      }

      when (release_done) {
        when (merge) {
          // Send the Release after ProbeAck
//          state := s_release_req
//          req := Mux(merge, io.req.bits, req_later.toWritebackReqCtrl)
//          release_later := false.B
          state := s_sleep
          state_dup_0 := s_sleep
          state_dup_1 := s_sleep
          state_dup_for_mp.foreach(_ := s_sleep)
          req := io.req.bits
          release_later := false.B
        }.elsewhen (release_later) {
          state := Mux(
            io.release_wakeup.valid && io.release_wakeup.bits === req_later.miss_id || !req_later.delay_release,
            s_release_req,
            s_sleep
          )
          state_dup_0 := Mux(
            io.release_wakeup.valid && io.release_wakeup.bits === req_later.miss_id || !req_later.delay_release,
            s_release_req,
            s_sleep
          )
          state_dup_1 := Mux(
            io.release_wakeup.valid && io.release_wakeup.bits === req_later.miss_id || !req_later.delay_release,
            s_release_req,
            s_sleep
          )
          state_dup_for_mp.foreach(_ := Mux(
            io.release_wakeup.valid && io.release_wakeup.bits === req_later.miss_id || !req_later.delay_release,
            s_release_req,
            s_sleep
          ))
          req := req_later.toWritebackReqCtrl
          when (io.release_wakeup.valid && io.release_wakeup.bits === req_later.miss_id) {
            req.delay_release := false.B
          }
          release_later := false.B
        }.otherwise {
          state := s_invalid
          state_dup_0 := s_invalid
          state_dup_1 := s_invalid
          state_dup_for_mp.foreach(_ := s_invalid)
          release_later := false.B
        }
      }

      when (io.release_wakeup.valid && io.release_wakeup.bits === req_later.miss_id) {
        req_later.delay_release := false.B
      }
    }
  }

  // --------------------------------------------------------------------------------
  // receive ReleaseAck for Releases
  when (state_dup_0 === s_release_resp) {
    io.mem_grant.ready := true.B

    when (merge) {
      release_later := true.B
      req_later.param := io.req.bits.param
      req_later.voluntary := io.req.bits.voluntary
      req_later.hasData := io.req.bits.hasData
      req_later.dirty := io.req.bits.dirty
      req_later.delay_release := io.req.bits.delay_release
      req_later.miss_id := io.req.bits.miss_id
    }
    when (io.mem_grant.fire()) {
      when (merge) {
        state := s_release_req
        state_dup_0 := s_release_req
        state_dup_1 := s_release_req
        state_dup_for_mp.foreach(_ := s_release_req)
        req := io.req.bits
        remain_set := Mux(io.req.bits.hasData, ~0.U(refillCycles.W), 1.U(refillCycles.W))
        release_later := false.B
      }.elsewhen(release_later) {
        state := s_release_req
        state_dup_0 := s_release_req
        state_dup_1 := s_release_req
        state_dup_for_mp.foreach(_ := s_release_req)
        req := req_later.toWritebackReqCtrl
        remain_set := Mux(req_later.hasData, ~0.U(refillCycles.W), 1.U(refillCycles.W))
        release_later := false.B
      }.otherwise {
        state := s_invalid
        state_dup_0 := s_invalid
        state_dup_1 := s_invalid
        state_dup_for_mp.foreach(_ := s_invalid)
        release_later := false.B
      }
    }
  }

  // When does this entry merge a new req?
  // 1. When this entry is free
  // 2. When this entry wants to release while still waiting for release_wakeup signal,
  //    and a probe req with the same addr comes. In this case we merge probe with release,
  //    handle this probe, so we don't need another release.
  io.primary_ready := state_dup_1 === s_invalid
  io.primary_ready_dup.zip(state_dup_for_mp).foreach { case (rdy, st) => rdy := st === s_invalid }
  io.secondary_ready := state_dup_1 =/= s_invalid && io.req.bits.addr === paddr_dup_0

  io.probe_ttob_check_resp.valid := RegNext(io.probe_ttob_check_req.valid) // for debug only
  io.probe_ttob_check_resp.bits.toN := state_dup_1 === s_sleep && 
    RegNext(io.probe_ttob_check_req.bits.addr) === paddr_dup_0 &&
    RegNext(io.probe_ttob_check_req.valid)

  // data update logic
  when (!s_data_merge) {
    data := mergeData(data, io.release_update.bits.data_delayed, io.release_update.bits.mask_delayed)
  }

  when (!s_data_override && req.hasData) {
    data := io.req_data.data
  }

  assert(!RegNext(!s_data_merge && !s_data_override))

  // performance counters
  XSPerfAccumulate("wb_req", io.req.fire())
  XSPerfAccumulate("wb_release", state === s_release_req && release_done && req.voluntary)
  XSPerfAccumulate("wb_probe_resp", state_dup_0 === s_release_req && release_done && !req.voluntary)
  XSPerfAccumulate("wb_probe_ttob_fix", io.probe_ttob_check_resp.valid && io.probe_ttob_check_resp.bits.toN)
  XSPerfAccumulate("penalty_blocked_by_channel_C", io.mem_release.valid && !io.mem_release.ready)
  XSPerfAccumulate("penalty_waiting_for_channel_D", io.mem_grant.ready && !io.mem_grant.valid && state_dup_1 === s_release_resp)
}

class WritebackQueue(edge: TLEdgeOut)(implicit p: Parameters) extends DCacheModule with HasTLDump with HasPerfEvents {
  val io = IO(new Bundle {
    val req = Flipped(DecoupledIO(new WritebackReq))
    val req_ready_dup = Vec(nDupWbReady, Output(Bool()))
    val mem_release = DecoupledIO(new TLBundleC(edge.bundle))
    val mem_grant = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))

    val release_wakeup = Flipped(ValidIO(UInt(log2Up(cfg.nMissEntries).W)))
    val release_update = Flipped(ValidIO(new ReleaseUpdate))

    val probe_ttob_check_req = Flipped(ValidIO(new ProbeToBCheckReq))
    val probe_ttob_check_resp = ValidIO(new ProbeToBCheckResp)

    val miss_req = Flipped(Valid(UInt()))
    val block_miss_req = Output(Bool())
  })

  require(cfg.nReleaseEntries > cfg.nMissEntries)

  val primary_ready_vec = Wire(Vec(cfg.nReleaseEntries, Bool()))
  val secondary_ready_vec = Wire(Vec(cfg.nReleaseEntries, Bool()))
  val accept = Cat(primary_ready_vec).orR
  val merge = Cat(secondary_ready_vec).orR
  val alloc = accept && !merge
  // When there are empty entries, merge or allocate a new entry.
  // When there is no empty entry, reject it even if it can be merged.
  io.req.ready := accept

  // assign default values to output signals
  io.mem_release.valid := false.B
  io.mem_release.bits  := DontCare
  io.mem_grant.ready   := false.B

  // dalay data write in miss queue release update for 1 cycle
  val release_update_bits_for_entry = Wire(new WBQEntryReleaseUpdate)
  release_update_bits_for_entry.addr := io.release_update.bits.addr
  release_update_bits_for_entry.mask_delayed := RegEnable(io.release_update.bits.mask, io.release_update.valid)
  release_update_bits_for_entry.data_delayed := RegEnable(io.release_update.bits.data, io.release_update.valid)
  release_update_bits_for_entry.mask_orr := io.release_update.bits.mask.orR

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
      entry.io.req.valid := io.req.valid
      primary_ready_vec(i)   := entry.io.primary_ready
      secondary_ready_vec(i) := entry.io.secondary_ready
      entry.io.req.bits  := io.req.bits
      entry.io.req_data  := req_data

      entry.io.primary_valid := alloc &&
        !former_primary_ready &&
        entry.io.primary_ready
      entry.io.secondary_valid := io.req.valid && accept

      entry.io.mem_grant.valid := (entry_id === grant_source) && io.mem_grant.valid
      entry.io.mem_grant.bits  := io.mem_grant.bits

      entry.io.release_wakeup := io.release_wakeup
      entry.io.release_update.valid := io.release_update.valid
      entry.io.release_update.bits := release_update_bits_for_entry // data write delayed

      entry.io.probe_ttob_check_req := io.probe_ttob_check_req
  }

  io.req_ready_dup.zipWithIndex.foreach { case (rdy, i) =>
    rdy := Cat(entries.map(_.io.primary_ready_dup(i))).orR
  }

  io.probe_ttob_check_resp.valid := RegNext(io.probe_ttob_check_req.valid) // for debug only
  io.probe_ttob_check_resp.bits.toN := VecInit(entries.map(e => e.io.probe_ttob_check_resp.bits.toN)).asUInt.orR

  assert(RegNext(!(io.mem_grant.valid && !io.mem_grant.ready)))
  io.mem_grant.ready := true.B

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

  val perfValidCount = RegNext(PopCount(entries.map(e => e.io.block_addr.valid)))
  val perfEvents = Seq(
    ("dcache_wbq_req      ", io.req.fire()),
    ("dcache_wbq_1_4_valid", (perfValidCount < (cfg.nReleaseEntries.U/4.U))),
    ("dcache_wbq_2_4_valid", (perfValidCount > (cfg.nReleaseEntries.U/4.U)) & (perfValidCount <= (cfg.nReleaseEntries.U/2.U))),
    ("dcache_wbq_3_4_valid", (perfValidCount > (cfg.nReleaseEntries.U/2.U)) & (perfValidCount <= (cfg.nReleaseEntries.U*3.U/4.U))),
    ("dcache_wbq_4_4_valid", (perfValidCount > (cfg.nReleaseEntries.U*3.U/4.U))),
  )
  generatePerfEvent()
}
