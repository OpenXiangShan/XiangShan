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

  val snp_txn_id = UInt(DCacheCCHI.dnTxnIdWidth.W) // SNP TxnID for probe response; unused for voluntary EVT
  val snp_tgt_id = UInt(DCacheCCHI.dnNodeIdWidth.W) // SNP SrcID, echoed as SnpResp/SnpRespData TgtID
  val trace_tag = UInt(1.W)
  val chi_channel = UInt(CCHIChannelBits.W) // snoop resp only
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

    val block_addr  = Output(Valid(UInt()))
    val resp_channel = Output(UInt(CCHIChannelBits.W))
  })

  // Entry lifecycle = CHI step of one writeback:
  //   s_invalid: free
  //   s_probe_rsp / s_probe_dat: SnpResp / SnpRespData beats (non-voluntary)
  //   s_evt: Evict / WriteBackFull EVT (voluntary)
  //   s_copyback: CopyBackWrData beats after DBIDResp
  //   s_wait: DnRSP pending (Comp / DBIDResp)
  val s_invalid :: s_probe_rsp :: s_probe_dat :: s_evt :: s_copyback :: s_wait :: Nil = Enum(6)

  val state = RegInit(s_invalid)

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
  val copyback_dbid = Reg(UInt(DCacheCCHI.dnTxnIdWidth.W))
  // DBIDResp SrcID / Snp SrcID, echoed as CopyBackWrData / SnpResp(SnpRespData) TgtID
  // (L2 routes UpRSP/UpDAT to slices by TgtID)
  val copyback_tgt_id = Reg(UInt(DCacheCCHI.dnNodeIdWidth.W))
  val snp_txn_id = Reg(UInt(DCacheCCHI.dnTxnIdWidth.W))
  val snp_tgt_id = Reg(UInt(DCacheCCHI.dnNodeIdWidth.W))
  val trace_tag = Reg(UInt(1.W))
  val resp_channel = Reg(UInt(CCHIChannelBits.W))

  io.req.ready := false.B
  io.txevt.valid := false.B
  io.txevt.bits := DontCare
  io.txrsp.valid := false.B
  io.txrsp.bits := DontCare
  io.txdat.valid := false.B
  io.txdat.bits := DontCare
  io.rxrsp.ready := true.B
  io.block_addr.valid := state =/= s_invalid
  io.block_addr.bits := req.addr
  io.resp_channel := resp_channel

  s_data_override := true.B // default; alloc clears for one cycle to load req_data

  XSDebug(state =/= s_invalid, "WritebackEntry: %d state: %d block_addr: %x\n",
    io.id, state, io.block_addr.bits)

  io.req.ready := state === s_invalid
  val alloc = io.req.valid && io.primary_valid && io.primary_ready
  when (alloc) {
    assert(remain === 0.U)
    req := io.req.bits
    snp_txn_id := io.req.bits.snp_txn_id
    snp_tgt_id := io.req.bits.snp_tgt_id
    trace_tag := io.req.bits.trace_tag
    resp_channel := Mux(io.req.bits.voluntary,
      selectCCHIChannel(io.req.bits.addr),
      io.req.bits.chi_channel)
    got_dbid := false.B
    got_comp := false.B
    s_data_override := false.B

    when (!io.req.bits.voluntary) {
      state := Mux(io.req.bits.hasData, s_probe_dat, s_probe_rsp)
      remain_set := Mux(io.req.bits.hasData, ~0.U(refillCycles.W), 1.U(refillCycles.W))
    } .otherwise {
      state := s_evt
      remain_set := 1.U(refillCycles.W)
    }
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
  val rxrsp_is_comp = CCHIOpcode.Comp.is(io.rxrsp.bits.Opcode, io.rxrsp.valid)
  val rxrsp_is_dbid = CCHIOpcode.DBIDResp.is(io.rxrsp.bits.Opcode, io.rxrsp.valid)
  val rxrsp_is_comp_dbid = CCHIOpcode.CompDBIDResp.is(io.rxrsp.bits.Opcode, io.rxrsp.valid)
  val comp_fire = io.rxrsp.fire && rxrsp_is_comp

  // Every legal response has reserved storage in its outstanding entry.
  when (io.rxrsp.valid) {
    assert((state === s_wait || state === s_copyback) && req.voluntary, "WBQ response must target an outstanding EVT")
    when (rxrsp_is_dbid || rxrsp_is_comp_dbid) {
      assert(state === s_wait && req.hasData && !got_dbid, "WBQ DBID response must target a WriteBackFull waiting for its first DBID")
    }
  }

  when (comp_fire) {
    got_comp := true.B
  }

  when (state === s_probe_rsp) {
    io.txrsp.valid := busy
    DCacheCCHI.Tx.snpResp(io.txrsp.bits, snp_txn_id, snp_tgt_id, req.param, req.dirty, trace_tag,
      cchiDcacheSrcId(resp_channel))
    when (io.txrsp.fire) {
      remain_clr := PriorityEncoderOH(remain_dup_1)
      state := s_invalid
    }
  }

  when (state === s_probe_dat) {
    io.txdat.valid := busy
    DCacheCCHI.Tx.snpRespData(io.txdat.bits, snp_txn_id, snp_tgt_id, req.param, req.dirty, beat, beat_data(beat),
      req.corrupt, trace_tag, cchiDcacheSrcId(resp_channel))
    when (io.txdat.fire) {
      remain_clr := PriorityEncoderOH(remain_dup_1)
      when (PopCount(remain) === 1.U) {
        state := s_invalid
      }
    }
  }

  when (state === s_evt) {
    io.txevt.valid := busy
    when (req.hasData) {
      DCacheCCHI.Tx.evtWriteBackFull(io.txevt.bits, io.id, req.addr, cchiDcacheSrcId(resp_channel))
    } .otherwise {
      DCacheCCHI.Tx.evtEvict(io.txevt.bits, io.id, req.addr, cchiDcacheSrcId(resp_channel))
    }
    when (io.txevt.fire) {
      remain_clr := PriorityEncoderOH(remain_dup_1)
      state := s_wait
    }
  }

  when (state === s_copyback) {
    io.txdat.valid := busy
    DCacheCCHI.Tx.copyBackWrData(io.txdat.bits, copyback_dbid, copyback_tgt_id, beat, beat_data(beat), req.corrupt,
      cchiDcacheSrcId(resp_channel), trace_tag)
    when (io.txdat.fire) {
      remain_clr := PriorityEncoderOH(remain_dup_1)
      when (PopCount(remain) === 1.U) {
        state := Mux(got_comp || comp_fire, s_invalid, s_wait)
      }
    }
  }

  when (state === s_wait && io.rxrsp.fire) {
    when (rxrsp_is_comp) {
      when (!req.hasData) {
        state := s_invalid
      } .elsewhen (got_dbid && remain === 0.U) {
        state := s_invalid
      }
    }
    when (rxrsp_is_dbid || rxrsp_is_comp_dbid) {
      got_dbid := true.B
      copyback_dbid := io.rxrsp.bits.DBID
      copyback_tgt_id := io.rxrsp.bits.SrcID
      state := s_copyback
      remain_set := ~0.U(refillCycles.W)
    }
    when (rxrsp_is_comp_dbid) {
      got_comp := true.B
    }
  }

  io.primary_ready := state === s_invalid

  // Wide data from MainPipe arrives one cycle after ctrl/addr (see WritebackQueue.req_data).
  when (!s_data_override && (req.hasData || RegNext(alloc))) {
    data := io.req_data.data
  }

  XSPerfAccumulate("wb_req", io.req.fire)
  XSPerfAccumulate("wb_release", tx_fire && req.voluntary && state === s_evt)
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
    val txevt = Vec(nCCHIChannels, DecoupledIO(new FlitEVT))
    val txrsp = Vec(nCCHIChannels, DecoupledIO(new FlitUpRSP))
    val txdat = Vec(nCCHIChannels, DecoupledIO(new FlitUpDAT))
    val rxrsp = Vec(nCCHIChannels, Flipped(DecoupledIO(new FlitDnRSP)))

    val miss_req_conflict_check = Vec(MissReqPortCount, Flipped(Valid(UInt())))
    val block_miss_req = Vec(MissReqPortCount, Output(Bool()))
  })

  require(cfg.nReleaseEntries > cfg.nMissEntries)

  val primary_ready_vec = Wire(Vec(cfg.nReleaseEntries, Bool()))
  val alloc = Cat(primary_ready_vec).orR

  val req = io.req
  val block_conflict = Wire(Bool())

  req.ready := alloc && !block_conflict

  for (ch <- 0 until nCCHIChannels) {
    io.txevt(ch).valid := false.B
    io.txevt(ch).bits := DontCare
    io.txrsp(ch).valid := false.B
    io.txrsp(ch).bits := DontCare
    io.txdat(ch).valid := false.B
    io.txdat(ch).bits := DontCare
    io.rxrsp(ch).ready := true.B
  }

  // Register full line data one cycle after io.req to ease MainPipe -> WBQ timing.
  val req_data = RegEnable(io.req.bits.toWritebackReqData(), io.req.valid)

  require(isPow2(cfg.nMissEntries))
  val entries = Seq.fill(cfg.nReleaseEntries)(Module(new WritebackEntry()))
  val rxrspMatches = Wire(Vec(cfg.nReleaseEntries, Vec(nCCHIChannels, Bool())))
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
      for (ch <- 0 until nCCHIChannels) {
        rxrspMatches(i)(ch) := io.rxrsp(ch).valid && io.rxrsp(ch).bits.TxnID === entry_id
        when (rxrspMatches(i)(ch)) {
          entry.io.rxrsp.valid := io.rxrsp(ch).valid
          entry.io.rxrsp.bits := io.rxrsp(ch).bits
        }
      }
  }

  if (nCCHIChannels > 1) {
    val demuxedEvt = entries.map(e => demuxByCCHIChannel(e.io.txevt, e.io.resp_channel, nCCHIChannels))
    val demuxedRsp = entries.map(e => demuxByCCHIChannel(e.io.txrsp, e.io.resp_channel, nCCHIChannels))
    val demuxedDat = entries.map(e => demuxByCCHIChannel(e.io.txdat, e.io.resp_channel, nCCHIChannels))
    for (ch <- 0 until nCCHIChannels) {
      lowestArb(io.txevt(ch), demuxedEvt.map(_(ch)))
      lowestArb(io.txrsp(ch), demuxedRsp.map(_(ch)))
      lowestArb(io.txdat(ch), demuxedDat.map(_(ch)))
    }
  } else {
    lowestArb(io.txevt(0), entries.map(_.io.txevt))
    lowestArb(io.txrsp(0), entries.map(_.io.txrsp))
    lowestArb(io.txdat(0), entries.map(_.io.txdat))
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

  if (nCCHIChannels >= 2) {
    val rxrspMatched = (0 until nCCHIChannels).map(ch => rxrspMatches.map(_(ch)).reduce(_ || _))
    XSPerfAccumulate("dual_channel_release", io.txevt(0).valid && io.txevt(1).valid)
    XSPerfAccumulate("dual_channel_releaseAck", rxrspMatched(0) && rxrspMatched(1))
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

