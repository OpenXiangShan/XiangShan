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
import org.chipsalliance.cde.config.Parameters
import oceanus.compactchi._
import utility.{XSDebug, XSPerfAccumulate, HasPerfEvents}


class WritebackReqCtrl(implicit p: Parameters) extends DCacheBundle {
  val param  = UInt(cWidth.W)
  val voluntary = Bool()
  val hasData = Bool()
  val corrupt = Bool()
  val dirty = Bool()

  val chi_txn_id = UInt(8.W) // SNP TxnID for probe response; unused for voluntary EVT
  val trace_tag = UInt(1.W)
  val chi_channel = UInt(memChannelBits.W) // snoop resp only
}

class WritebackReqWodata(implicit p: Parameters) extends WritebackReqCtrl {
  val addr = UInt(PAddrBits.W)

  def dump(cond: Bool) = {
    XSDebug(cond, "WritebackReq addr: %x param: %d voluntary: %b hasData: %b\n",
      addr, param, voluntary, hasData)
  }
}

class WritebackReqData(implicit p: Parameters) extends DCacheBundle {
  val data = UInt((cfg.blockBytes * 8).W)
}

class WritebackReq(implicit p: Parameters) extends WritebackReqWodata {
  val data = UInt((cfg.blockBytes * 8).W)

  override def dump(cond: Bool) = {
    XSDebug(cond, "WritebackReq addr: %x param: %d voluntary: %b hasData: %b data: %x\n",
      addr, param, voluntary, hasData, data)
  }

  def toWritebackReqData(): WritebackReqData = {
    val out = Wire(new WritebackReqData)
    out.data := data
    out
  }
}

class WritebackEntry()(implicit p: Parameters) extends DCacheModule
{
  private def selectChannel(addr: UInt): UInt = {
    if (hasDualChannel) {
      if (channelSelByAddr) get_block(addr)(memChannelBits - 1, 0)
      else 0.U(memChannelBits.W)
    } else {
      0.U(memChannelBits.W)
    }
  }

  val io = IO(new Bundle {
    val id = Input(UInt())

    val req = Flipped(DecoupledIO(new WritebackReqWodata))
    val req_data = Input(new WritebackReqData)

    val txevt = DecoupledIO(new FlitEVT)
    val txrsp = DecoupledIO(new FlitUpRSP)
    val txdat = DecoupledIO(new FlitUpDAT)
    val rxrsp = Flipped(DecoupledIO(new FlitDnRSP))

    val primary_valid = Input(Bool())
    val primary_ready = Output(Bool())
    val primary_ready_dup = Vec(nDupWbReady, Output(Bool()))

    val block_addr  = Output(Valid(UInt()))
    val resp_channel = Output(UInt(memChannelBits.W))
  })

  // Entry lifecycle: invalid (free) / out (driving TX) / wait (for DnRSP).
  val s_invalid :: s_out :: s_wait :: Nil = Enum(3)

  // CHI step within one writeback; paired with state but not 1:1 (s_out covers several phases).
  val phase_idle :: phase_probe_rsp :: phase_probe_dat :: phase_evt :: phase_copyback :: phase_wait :: Nil = Enum(6)

  val state = RegInit(s_invalid)
  val state_dup_for_mp = RegInit(VecInit(Seq.fill(nDupWbReady)(s_invalid)))

  val phase = RegInit(phase_idle)
  // One-hot mask of TX beats still to send (probe data, EVT, or CopyBackWrData).
  val remain = RegInit(0.U(refillCycles.W))
  val remain_dup_0 = RegInit(0.U(refillCycles.W))
  val remain_dup_1 = RegInit(0.U(refillCycles.W))
  val remain_set = WireInit(0.U(refillCycles.W))
  val remain_clr = WireInit(0.U(refillCycles.W))
  remain := (remain | remain_set) & ~remain_clr
  remain_dup_0 := (remain_dup_0 | remain_set) & ~remain_clr
  remain_dup_1 := (remain_dup_1 | remain_set) & ~remain_clr

  val data = Reg(UInt((cfg.blockBytes * 8).W))

  // Cleared on alloc until req_data is latched into data (see below); gates TX valid.
  val s_data_override = RegInit(true.B)
  // Ready to assert txevt/txrsp/txdat: beats pending and cache line data is valid.
  val busy = remain.orR && s_data_override
  val req = Reg(new WritebackReqWodata)

  val got_dbid = RegInit(false.B)
  val got_comp = RegInit(false.B)
  val copyback_dbid = Reg(UInt(8.W))
  val snp_txn_id = Reg(UInt(8.W))
  val trace_tag = Reg(UInt(1.W))
  val resp_channel = Reg(UInt(memChannelBits.W))

  io.req.ready := false.B
  io.txevt.valid := false.B
  io.txevt.bits := DontCare
  io.txrsp.valid := false.B
  io.txrsp.bits := DontCare
  io.txdat.valid := false.B
  io.txdat.bits := DontCare
  io.rxrsp.ready := false.B
  io.block_addr.valid := state =/= s_invalid
  io.block_addr.bits := req.addr
  io.resp_channel := resp_channel

  s_data_override := true.B // default; alloc clears for one cycle to load req_data

  XSDebug(state =/= s_invalid, "WritebackEntry: %d state: %d phase: %d block_addr: %x\n",
    io.id, state, phase, io.block_addr.bits)

  io.req.ready := state === s_invalid
  val alloc = io.req.valid && io.primary_valid && io.primary_ready
  when (alloc) {
    assert(remain === 0.U)
    req := io.req.bits
    snp_txn_id := io.req.bits.chi_txn_id
    trace_tag := io.req.bits.trace_tag
    resp_channel := Mux(io.req.bits.voluntary,
      selectChannel(io.req.bits.addr),
      io.req.bits.chi_channel)
    got_dbid := false.B
    got_comp := false.B
    s_data_override := false.B

    when (!io.req.bits.voluntary) {
      phase := Mux(io.req.bits.hasData, phase_probe_dat, phase_probe_rsp)
      remain_set := Mux(io.req.bits.hasData, ~0.U(refillCycles.W), 1.U(refillCycles.W))
      state := s_out
    } .otherwise {
      phase := phase_evt
      remain_set := 1.U(refillCycles.W)
      state := s_out
    }
    state_dup_for_mp.foreach(_ := Mux(io.req.bits.voluntary, s_wait, s_invalid)) // MP backpressure: voluntary holds addr early
  }

  val beat = PriorityEncoder(remain_dup_0)
  val beat_data = Wire(Vec(refillCycles, UInt(beatBits.W)))
  for (i <- 0 until refillCycles) {
    beat_data(i) := data((i + 1) * beatBits - 1, i * beatBits)
  }

  when (busy) {
    assert(!req.dirty || req.hasData)
  }

  val tx_fire = io.txevt.fire || io.txrsp.fire || io.txdat.fire

  when (phase === phase_probe_rsp) {
    io.txrsp.valid := busy
    DCacheCCHI.Tx.snpResp(io.txrsp.bits, snp_txn_id, req.param, req.dirty, trace_tag)
    when (io.txrsp.fire) {
      remain_clr := PriorityEncoderOH(remain_dup_1)
      phase := phase_idle
      state := s_invalid
      state_dup_for_mp.foreach(_ := s_invalid)
    }
  }

  when (phase === phase_probe_dat) {
    io.txdat.valid := busy
    DCacheCCHI.Tx.snpRespData(io.txdat.bits, snp_txn_id, req.param, req.dirty, beat, beat_data(beat),
      req.corrupt, trace_tag)
    when (io.txdat.fire) {
      remain_clr := PriorityEncoderOH(remain_dup_1)
      when (PopCount(remain) === 1.U) {
        phase := phase_idle
        state := s_invalid
        state_dup_for_mp.foreach(_ := s_invalid)
      }
    }
  }

  when (phase === phase_evt) {
    io.txevt.valid := busy
    when (req.hasData) {
      DCacheCCHI.Tx.evtWriteBackFull(io.txevt.bits, io.id, req.addr)
    } .otherwise {
      DCacheCCHI.Tx.evtEvict(io.txevt.bits, io.id, req.addr)
    }
    when (io.txevt.fire) {
      remain_clr := PriorityEncoderOH(remain_dup_1)
      phase := phase_wait
      state := s_wait
      state_dup_for_mp.foreach(_ := s_wait)
    }
  }

  when (phase === phase_copyback) {
    io.txdat.valid := busy
    DCacheCCHI.Tx.copyBackWrData(io.txdat.bits, copyback_dbid, beat, beat_data(beat), req.corrupt, trace_tag)
    when (io.txdat.fire) {
      remain_clr := PriorityEncoderOH(remain_dup_1)
      when (PopCount(remain) === 1.U) {
        when (got_comp) {
          phase := phase_idle
          state := s_invalid
          state_dup_for_mp.foreach(_ := s_invalid)
        } .otherwise {
          phase := phase_wait
          state := s_wait
        }
      }
    }
  }

  val rxrsp_match = io.rxrsp.valid && io.rxrsp.bits.TxnID === io.id
  when (phase === phase_wait && rxrsp_match) {
    io.rxrsp.ready := true.B
    when (io.rxrsp.fire) {
      when (CCHIOpcode.Comp.is(io.rxrsp.bits.Opcode)) {
        got_comp := true.B
        when (!req.hasData) {
          phase := phase_idle
          state := s_invalid
          state_dup_for_mp.foreach(_ := s_invalid)
        } .elsewhen (got_dbid && remain === 0.U) {
          // WriteBackFull: Comp may arrive after CopyBackWrData (no fixed order with DBID).
          phase := phase_idle
          state := s_invalid
          state_dup_for_mp.foreach(_ := s_invalid)
        }
      }
      when (CCHIOpcode.DBIDResp.is(io.rxrsp.bits.Opcode) ||
        CCHIOpcode.CompDBIDResp.is(io.rxrsp.bits.Opcode)) {
        assert(req.hasData)
        got_dbid := true.B
        copyback_dbid := io.rxrsp.bits.DBID
        when (CCHIOpcode.CompDBIDResp.is(io.rxrsp.bits.Opcode)) {
          got_comp := true.B
        }
        phase := phase_copyback
        state := s_out
        remain_set := ~0.U(refillCycles.W)
      }
    }
  }

  io.primary_ready := state === s_invalid
  io.primary_ready_dup.zip(state_dup_for_mp).foreach { case (rdy, st) => rdy := st === s_invalid }

  // Wide data from MainPipe arrives one cycle after ctrl/addr (see WritebackQueue.req_data).
  when (!s_data_override && (req.hasData || RegNext(alloc))) {
    data := io.req_data.data
  }

  XSPerfAccumulate("wb_req", io.req.fire)
  XSPerfAccumulate("wb_release", tx_fire && req.voluntary && phase === phase_evt)
  XSPerfAccumulate("wb_probe_resp", tx_fire && !req.voluntary)
  XSPerfAccumulate("penalty_blocked_by_txevt", io.txevt.valid && !io.txevt.ready)
  XSPerfAccumulate("penalty_blocked_by_txrsp", io.txrsp.valid && !io.txrsp.ready)
  XSPerfAccumulate("penalty_blocked_by_txdat", io.txdat.valid && !io.txdat.ready)
  XSPerfAccumulate("penalty_waiting_for_dnrsp", state === s_wait && !io.rxrsp.valid)
}

class WritebackQueue()(implicit p: Parameters) extends DCacheModule with HasPerfEvents
{
  val io = IO(new Bundle {
    val req = Flipped(DecoupledIO(new WritebackReq))
    val req_ready_dup = Vec(nDupWbReady, Output(Bool()))
    val txevt = Vec(numMemChannels, DecoupledIO(new FlitEVT))
    val txrsp = Vec(numMemChannels, DecoupledIO(new FlitUpRSP))
    val txdat = Vec(numMemChannels, DecoupledIO(new FlitUpDAT))
    val rxrsp = Vec(numMemChannels, Flipped(DecoupledIO(new FlitDnRSP)))

    val miss_req_conflict_check = Vec(MissReqPortCount, Flipped(Valid(UInt())))
    val block_miss_req = Vec(MissReqPortCount, Output(Bool()))
  })

  require(cfg.nReleaseEntries > cfg.nMissEntries)

  val primary_ready_vec = Wire(Vec(cfg.nReleaseEntries, Bool()))
  val alloc = Cat(primary_ready_vec).orR

  val req = io.req
  val block_conflict = Wire(Bool())

  req.ready := alloc && !block_conflict

  for (ch <- 0 until numMemChannels) {
    io.txevt(ch).valid := false.B
    io.txevt(ch).bits := DontCare
    io.txrsp(ch).valid := false.B
    io.txrsp(ch).bits := DontCare
    io.txdat(ch).valid := false.B
    io.txdat(ch).bits := DontCare
    io.rxrsp(ch).ready := false.B
  }

  // Register full line data one cycle after io.req to ease MainPipe -> WBQ timing.
  val req_data = RegEnable(io.req.bits.toWritebackReqData(), io.req.valid)

  require(isPow2(cfg.nMissEntries))
  val entries = Seq.fill(cfg.nReleaseEntries)(Module(new WritebackEntry()))
  entries.zipWithIndex.foreach {
    case (entry, i) =>
      val former_primary_ready = if(i == 0)
        false.B
      else
        Cat((0 until i).map(j => entries(j).io.primary_ready)).orR
      val entry_id = (i + releaseIdBase).U

      entry.io.id := entry_id

      entry.io.req.valid := req.valid && !block_conflict
      primary_ready_vec(i) := entry.io.primary_ready
      entry.io.req.bits := req.bits
      entry.io.req_data := req_data

      entry.io.primary_valid := alloc &&
        !former_primary_ready &&
        entry.io.primary_ready

      entry.io.rxrsp.valid := false.B
      entry.io.rxrsp.bits := DontCare
      for (ch <- 0 until numMemChannels) {
        when (io.rxrsp(ch).valid && io.rxrsp(ch).bits.TxnID === entry_id) {
          entry.io.rxrsp <> io.rxrsp(ch)
        }
      }
  }

  for (ch <- 0 until numMemChannels) {
    io.rxrsp(ch).ready := VecInit(entries.map(_.io.rxrsp.ready)).asUInt.orR
  }

  def lowestArb[T <: Data](out: DecoupledIO[T], ins: Seq[DecoupledIO[T]]): Unit = {
    val sel = PriorityEncoderOH(ins.map(_.valid))
    out.valid := ins.map(_.valid).reduce(_ || _)
    out.bits := Mux1H(sel, ins.map(_.bits))
    ins.zip(sel).foreach { case (in, s) => in.ready := out.ready && s }
  }

  if (numMemChannels > 1) {
    val demuxedEvt = entries.map(e => demuxByChannel(e.io.txevt, e.io.resp_channel, numMemChannels))
    val demuxedRsp = entries.map(e => demuxByChannel(e.io.txrsp, e.io.resp_channel, numMemChannels))
    val demuxedDat = entries.map(e => demuxByChannel(e.io.txdat, e.io.resp_channel, numMemChannels))
    for (ch <- 0 until numMemChannels) {
      lowestArb(io.txevt(ch), demuxedEvt.map(_(ch)))
      lowestArb(io.txrsp(ch), demuxedRsp.map(_(ch)))
      lowestArb(io.txdat(ch), demuxedDat.map(_(ch)))
    }
  } else {
    lowestArb(io.txevt(0), entries.map(_.io.txevt))
    lowestArb(io.txrsp(0), entries.map(_.io.txrsp))
    lowestArb(io.txdat(0), entries.map(_.io.txdat))
  }

  io.req_ready_dup.zipWithIndex.foreach { case (rdy, i) =>
    rdy := Cat(entries.map(_.io.primary_ready_dup(i))).orR && !block_conflict
  }

  block_conflict := VecInit(entries.map(e => e.io.block_addr.valid && e.io.block_addr.bits === io.req.bits.addr)).asUInt.orR
  val miss_req_conflict = io.miss_req_conflict_check.map { r =>
    VecInit(entries.map(e => e.io.block_addr.valid && e.io.block_addr.bits === r.bits)).asUInt.orR
  }
  io.block_miss_req.zipWithIndex.foreach { case (blk, i) =>
    blk := io.miss_req_conflict_check(i).valid && miss_req_conflict(i)
  }

  io.req.bits.dump(io.req.fire)

  // XSDebug(io.miss_req.valid, "miss_req: addr: %x\n", io.miss_req.bits)
  // XSDebug(io.block_miss_req, "block_miss_req\n")

  // performance counters
  XSPerfAccumulate("wb_req", io.req.fire)
  for(i <- 0 until MissReqPortCount) {
    XSPerfAccumulate(s"block_miss_req_$i", io.block_miss_req(i))
  }

  if (numMemChannels >= 2) {
    XSPerfAccumulate("dual_channel_release", io.txevt(0).valid && io.txevt(1).valid)
    XSPerfAccumulate("dual_channel_releaseAck", io.rxrsp(0).valid && io.rxrsp(1).valid)
  }

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

