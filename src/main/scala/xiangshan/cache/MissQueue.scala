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
import utils._
import freechips.rocketchip.tilelink._
import bus.tilelink.TLMessages._
import difftest._

class MissReq(implicit p: Parameters) extends DCacheBundle
{
  val source = UInt(sourceTypeWidth.W)
  val cmd    = UInt(M_SZ.W)
  // must be aligned to block
  val addr   = UInt(PAddrBits.W)

  // store
  val store_data   = UInt((cfg.blockBytes * 8).W)
  val store_mask   = UInt(cfg.blockBytes.W)

  // which word does amo work on?
  val word_idx = UInt(log2Up(blockWords).W)
  val amo_data = UInt(DataBits.W)
  val amo_mask = UInt((DataBits/8).W)

  // coherence state
  val coh = new ClientMetadata
  val id  = UInt(reqIdWidth.W)

  def dump() = {
    XSDebug("MissReq source: %d cmd: %d addr: %x store_data: %x store_mask: %x word_idx: %d amo_data: %x amo_mask: %x coh: %d id: %d\n",
      source, cmd, addr, store_data, store_mask, word_idx, amo_data, amo_mask, coh.state, id)
  }

  def isLoad = source === LOAD_SOURCE.U
  def isStore = source === STORE_SOURCE.U
}

// One miss entry deals with one missed block
class MissEntry(edge: TLEdgeOut)(implicit p: Parameters) extends DCacheModule {
  val io = IO(new Bundle {
    // MSHR ID
    val id = Input(UInt())

    // client requests
    // this entry is free and can be allocated to new reqs
    val primary_ready = Output(Bool())
    // this entry is busy, but it can merge the new req
    val secondary_ready = Output(Bool())
    // this entry is busy and it can not merge the new req
    val secondary_reject = Output(Bool())
    val req    = Flipped(ValidIO(new MissReq))
    val refill = ValidIO(new Refill)

    // bus
    val mem_acquire = DecoupledIO(new TLBundleA(edge.bundle))
    val mem_grant   = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))
    val mem_finish  = DecoupledIO(new TLBundleE(edge.bundle))

    val pipe_req  = DecoupledIO(new MainPipeReq)
    val pipe_resp = Flipped(ValidIO(new MainPipeResp))

    // block probe
    val block_addr = ValidIO(UInt(PAddrBits.W))
  })
  val tma_io = IO(new Bundle {
    val req    = Output(new MissReq)
    val state  = Output(UInt(5.W))
  })

  val req = Reg(new MissReq)
  val req_valid = RegInit(false.B)

  val s_acquire = RegInit(true.B)
  val s_grantack = RegInit(true.B)
  val s_pipe_req = RegInit(true.B)
  val w_grantfirst = RegInit(true.B)
  val w_grantlast = RegInit(true.B)
  val w_pipe_resp = RegInit(true.B)

  val no_schedule = s_grantack && s_pipe_req
  val no_wait = w_pipe_resp
  val release_entry = no_schedule && no_wait

  val acquire_not_sent = !s_acquire && !io.mem_acquire.ready
  val data_not_refilled = !w_grantlast

  // should we refill the data to load queue to wake up any missed load?
  val should_refill_data_reg = Reg(Bool())
  val should_refill_data = WireInit(should_refill_data_reg)

  val full_overwrite = req.isStore && req.store_mask.andR

  val (_, _, refill_done, refill_count) = edge.count(io.mem_grant)
  val grant_param = Reg(UInt(TLPermissions.bdWidth.W))

  val grant_beats = RegInit(0.U((beatBits).W))

  when (io.req.valid && io.primary_ready) {
    req_valid := true.B
    req := io.req.bits
    req.addr := get_block_addr(io.req.bits.addr)

    s_acquire := false.B
    s_grantack := false.B
    s_pipe_req := false.B
    w_grantfirst := false.B
    w_grantlast := false.B
    w_pipe_resp := false.B

    should_refill_data_reg := io.req.bits.isLoad

    grant_beats := 0.U
  }.elsewhen (release_entry) {
    req_valid := false.B
  }

  when (io.req.valid && io.secondary_ready) {
    // The merged reqs should never have higher permissions
    // which means the cache silently upgrade the permission of our block
    // without merge with this miss queue request!
    // Either our req come in with stale meta, or the req that upgrade the permission does not merge with this req.
    // Both cases are bugs of DCache.
    //
    // DCache can silently drop permission(eg, probed or evicted)
    // it should never silently upgrade permissions.
    assert (io.req.bits.coh.state <= req.coh.state)
    // use the most uptodate meta
    req.coh := io.req.bits.coh

    // when merging with store
    // we should remember its info into our req
    // or we will not be able to replay store
    when (io.req.bits.isStore) {
      req := io.req.bits
    }

    should_refill_data := should_refill_data_reg || io.req.bits.isLoad
    should_refill_data_reg := should_refill_data
  }

  // set state regs
  when (io.mem_acquire.fire()) {
    s_acquire := true.B
  }

  val refill_data = Reg(Vec(blockRows, UInt(rowBits.W)))
  val refill_data_raw = Reg(Vec(blockBytes/beatBytes, UInt(beatBits.W)))
  val new_data = Wire(Vec(blockRows, UInt(rowBits.W)))
  val new_mask = Wire(Vec(blockRows, UInt(rowBytes.W)))
  def mergePutData(old_data: UInt, new_data: UInt, wmask: UInt): UInt = {
    val full_wmask = FillInterleaved(8, wmask)
    (~full_wmask & old_data | full_wmask & new_data)
  }
  for (i <- 0 until blockRows) {
    new_data(i) := req.store_data(rowBits * (i + 1) - 1, rowBits * i)
    // we only need to merge data for Store
    new_mask(i) := Mux(req.isStore, req.store_mask(rowBytes * (i + 1) - 1, rowBytes * i), 0.U)
  }
  val hasData = RegInit(true.B)
  when (io.mem_grant.fire()) {
    w_grantfirst := true.B
    grant_param := io.mem_grant.bits.param
    when (edge.hasData(io.mem_grant.bits)) {
      // GrantData
      for (i <- 0 until beatRows) {
        val idx = (refill_count << log2Floor(beatRows)) + i.U
        val grant_row = io.mem_grant.bits.data(rowBits * (i + 1) - 1, rowBits * i)
        refill_data(idx) := mergePutData(grant_row, new_data(idx), new_mask(idx))
      }

      w_grantlast := w_grantlast || refill_done

      hasData := true.B

      grant_beats := grant_beats + 1.U
    }.otherwise {
      // Grant

      // since we do not sync between MissQueue and WritebackQueue
      // for a AcquireBlock BtoT, we can not protect our block from being replaced by another miss and written back by WritebackQueue
      // so AcquireBlock BtoT, we need L2 to give us GrantData, not Grant.
      // So that whether our block is replaced or not, we can always refill the block with valid data
      // So, if we enters here
      // we must be a AcquirePerm, not a AcquireBlock!!!
      assert (full_overwrite)
      // when we only acquire perm, not data
      // use Store's data
      for (i <- 0 until blockRows) {
        refill_data(i) := new_data(i)
      }

      w_grantlast := true.B

      hasData := false.B
    }

    refill_data_raw(refill_count) := io.mem_grant.bits.data
  }

  when (io.mem_finish.fire()) {
    s_grantack := true.B
  }

  when (io.pipe_req.fire()) {
    s_pipe_req := true.B
  }

  when (io.pipe_resp.valid) {
    w_pipe_resp := true.B
  }

//  def can_merge(new_req: MissReq): Bool = {
//    // caution: do not merge with AMO
//    // we can not do amoalu calculation in MissQueue
//    // so, we do not know the result after AMO calculation
//    // so do not merge with AMO
//
//    // before read acquire is fired, we can merge read or write
//    val before_read_sent = acquire_not_sent && req.source === LOAD_SOURCE.U && (new_req.source === LOAD_SOURCE.U || new_req.source === STORE_SOURCE.U)
//    // before read/write refills data to LoadQueue, we can merge any read
//    val before_data_refill = data_not_refilled && (req.source === LOAD_SOURCE.U || req.source === STORE_SOURCE.U) && new_req.source === LOAD_SOURCE.U
//
//    before_read_sent || before_data_refill
//  }

  def before_read_sent_can_merge(new_req: MissReq): Bool = {
    acquire_not_sent && req.source === LOAD_SOURCE.U && (new_req.source === LOAD_SOURCE.U || new_req.source === STORE_SOURCE.U)
  }

  def before_data_refill_can_merge(new_req: MissReq): Bool = {
    data_not_refilled && (req.source === LOAD_SOURCE.U || req.source === STORE_SOURCE.U) && new_req.source === LOAD_SOURCE.U
  }

  def should_merge(new_req: MissReq): Bool = {
    val block_match = req.addr === get_block_addr(new_req.addr)
    val beat_match = new_req.addr(blockOffBits - 1, beatOffBits) >= grant_beats
    block_match && (before_read_sent_can_merge(new_req) || beat_match && before_data_refill_can_merge(new_req))
  }

  def should_reject(new_req: MissReq): Bool = {
    val block_match = req.addr === get_block_addr(new_req.addr)
    // do not reject any req when we are in s_invalid
    block_match && !should_merge(new_req) && req_valid // TODO: optimize this
  }

  io.primary_ready := !req_valid
  io.secondary_ready := should_merge(io.req.bits)
  io.secondary_reject := should_reject(io.req.bits)

  // should not allocate, merge or reject at the same time
  // one at a time
  OneHot.checkOneHot(Seq(io.primary_ready, io.secondary_ready, io.secondary_reject))

  // put should_refill_data out of RegNext
  // so that when load miss are merged at refill_done
  // we can still refill data back
  //
  // Now refill to load queue width l1BusDataWidth, to load queue refill req
  // will be issued as soon as data is ready (stored in regs in miss queue)
  val refill_data_splited = WireInit(VecInit(Seq.tabulate(cfg.blockBytes * 8 / l1BusDataWidth)(i => {
    val data = refill_data.asUInt
    data((i + 1) * l1BusDataWidth - 1, i * l1BusDataWidth)
  })))
  io.refill.valid := RegNext(!w_grantlast && s_acquire && io.mem_grant.fire()) && should_refill_data
  io.refill.bits.addr := RegNext(req.addr + (refill_count << refillOffBits))
  io.refill.bits.data := refill_data_splited(RegNext(refill_count))
  io.refill.bits.refill_done := RegNext(refill_done && io.mem_grant.fire())
  io.refill.bits.hasdata := hasData
  io.refill.bits.data_raw := refill_data_raw.asUInt

  io.mem_acquire.valid := !s_acquire
  val grow_param = req.coh.onAccess(req.cmd)._2
  val acquireBlock = edge.AcquireBlock(
    fromSource = io.id,
    toAddress = req.addr,
    lgSize = (log2Up(cfg.blockBytes)).U,
    growPermissions = grow_param
  )._2
  val acquirePerm = edge.AcquirePerm(
    fromSource = io.id,
    toAddress = req.addr,
    lgSize = (log2Up(cfg.blockBytes)).U,
    growPermissions = grow_param
  )._2
  io.mem_acquire.bits := Mux(full_overwrite, acquirePerm, acquireBlock)
  io.mem_grant.ready := !w_grantlast && s_acquire
  val grantack = RegEnable(edge.GrantAck(io.mem_grant.bits), io.mem_grant.fire())
  val is_grant = RegEnable(edge.isRequest(io.mem_grant.bits), io.mem_grant.fire())
  io.mem_finish.valid := !s_grantack && w_grantfirst && is_grant
  io.mem_finish.bits := grantack

  io.pipe_req.valid := !s_pipe_req && w_grantlast
  val pipe_req = io.pipe_req.bits
  pipe_req.miss := true.B
  pipe_req.miss_id := io.id
  pipe_req.miss_param := grant_param

  pipe_req.probe := false.B
  pipe_req.probe_param := DontCare

  pipe_req.source := req.source
  pipe_req.cmd    := req.cmd
  pipe_req.addr   := req.addr
  pipe_req.store_data := refill_data.asUInt
  // full overwrite
  pipe_req.store_mask := Fill(cfg.blockBytes, "b1".U)
  pipe_req.word_idx := req.word_idx
  pipe_req.amo_data   := req.amo_data
  pipe_req.amo_mask   := req.amo_mask
  pipe_req.id     := req.id

  io.block_addr.valid := req_valid && w_grantlast && !release_entry
  io.block_addr.bits := req.addr

  tma_io.req := req
  tma_io.state := DontCare // TODO

  XSPerfAccumulate("miss_req", io.req.valid && io.primary_ready)
  XSPerfAccumulate("miss_penalty", BoolStopWatch(io.req.valid && io.primary_ready, release_entry))
  XSPerfAccumulate("load_miss_penalty_to_use", should_refill_data && BoolStopWatch(io.req.valid && io.primary_ready, io.refill.valid, true))
  XSPerfAccumulate("pipeline_penalty", BoolStopWatch(io.pipe_req.fire(), io.pipe_resp.fire()))
  XSPerfAccumulate("penalty_blocked_by_channel_A", io.mem_acquire.valid && !io.mem_acquire.ready)
  XSPerfAccumulate("penalty_waiting_for_channel_D", s_acquire && !w_grantlast && !io.mem_grant.valid)
  XSPerfAccumulate("penalty_blocked_by_channel_E", io.mem_finish.valid && !io.mem_finish.ready)
  XSPerfAccumulate("penalty_blocked_by_pipeline", io.pipe_req.valid && !io.pipe_req.ready)

  val (mshr_penalty_sample, mshr_penalty) = TransactionLatencyCounter(RegNext(io.req.valid && io.primary_ready), release_entry)
  XSPerfHistogram("miss_penalty", mshr_penalty, mshr_penalty_sample, 0, 100, 10)

  val load_miss_begin = io.req.valid && io.primary_ready && io.req.bits.isLoad
  val refill_finished = RegNext(!w_grantlast && refill_done) && should_refill_data
  val (load_miss_penalty_sample, load_miss_penalty) = TransactionLatencyCounter(load_miss_begin, refill_finished) // not real refill finish time
  XSPerfHistogram("load_miss_penalty_to_use", load_miss_penalty, load_miss_penalty_sample, 0, 100, 10)

  val (a_to_d_penalty_sample, a_to_d_penalty) = TransactionLatencyCounter(io.mem_acquire.fire(), io.mem_grant.fire() && refill_done)
  XSPerfHistogram("a_to_d_penalty", a_to_d_penalty, a_to_d_penalty_sample, 0, 100, 10)
}

class MissQueue(edge: TLEdgeOut)(implicit p: Parameters) extends DCacheModule with HasTLDump
{
  val io = IO(new Bundle {
    val req    = Flipped(DecoupledIO(new MissReq))
    val refill = ValidIO(new Refill)

    val mem_acquire = Decoupled(new TLBundleA(edge.bundle))
    val mem_grant   = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))
    val mem_finish  = Decoupled(new TLBundleE(edge.bundle))

    val pipe_req  = DecoupledIO(new MainPipeReq)
    val pipe_resp = Flipped(ValidIO(new MainPipeResp))

    // block probe
    val probe_req = Input(UInt(PAddrBits.W))
    val probe_block = Output(Bool())

    val full = Output(Bool())
  })

  val pipe_req_arb = Module(new RRArbiter(new MainPipeReq, cfg.nMissEntries))
  val refill_arb   = Module(new Arbiter(new Refill, cfg.nMissEntries))

  // dispatch req to MSHR
  val primary_ready  = Wire(Vec(cfg.nMissEntries, Bool()))
  val secondary_ready  = Wire(Vec(cfg.nMissEntries, Bool()))
  val secondary_reject  = Wire(Vec(cfg.nMissEntries, Bool()))
  val probe_block_vec = Wire(Vec(cfg.nMissEntries, Bool()))

  // try merging with existing reqs
  val merge = secondary_ready.asUInt.orR
  val merge_idx = PriorityEncoder(secondary_ready)
  // some req says the request can not be merged
  val reject = secondary_reject.asUInt.orR
  // allocate a new entry for this req
  val allocate = !reject && !merge && primary_ready.asUInt.orR
  val alloc_idx = PriorityEncoder(primary_ready)

  // will this req be accepted
  val accept = (merge || allocate) && !reject
  // if it's accepted, which entry will it enter
  val entry_idx = Mux(allocate, alloc_idx, merge_idx)

  // for one block, their should be only one MSHR
  // one block should not be stay in multiple MSHRs
  // if we a req can not merge with existing reqs
  // block it!
  OneHot.checkOneHot(secondary_ready)
  OneHot.checkOneHot(secondary_reject)
  // should not merge and reject at the same time
  OneHot.checkOneHot(Seq(merge, reject))

  io.req.ready := accept
  io.mem_grant.ready := false.B

  val entries = (0 until cfg.nMissEntries) map { i =>
    val entry = Module(new MissEntry(edge))

    entry.io.id := i.U(log2Up(cfg.nMissEntries).W)

    // entry req
    entry.io.req.valid  := (i.U === entry_idx) && accept && io.req.valid
    primary_ready(i)    := entry.io.primary_ready
    secondary_ready(i)  := entry.io.secondary_ready
    secondary_reject(i) := entry.io.secondary_reject
    probe_block_vec(i)  := entry.io.block_addr.valid && entry.io.block_addr.bits === io.probe_req
    entry.io.req.bits   := io.req.bits

    // entry refill
    refill_arb.io.in(i).valid := entry.io.refill.valid
    refill_arb.io.in(i).bits  := entry.io.refill.bits

    // pipe_req
    pipe_req_arb.io.in(i)     <> entry.io.pipe_req

    // pipe_req
    entry.io.pipe_resp.valid  := false.B
    entry.io.pipe_resp.bits   := DontCare
    when (io.pipe_resp.bits.id === i.U) {
      entry.io.pipe_resp <> io.pipe_resp
    }

    entry.io.mem_grant.valid := false.B
    entry.io.mem_grant.bits  := DontCare
    when (io.mem_grant.bits.source === i.U) {
      entry.io.mem_grant <> io.mem_grant
    }

    /*
    XSPerf(
      "perfCntDCacheMissQueuePenaltyEntry" + Integer.toString(i, 10),
      BoolStopWatch(
        start = entry.io.req.fire(), 
        stop = entry.io.resp.fire(),
        startHighPriority = true)
    )
    */

    entry
  }

  val pendingVec = entries.map(entry => (entry.tma_io.req.source =/= STORE_SOURCE.U) && (entry.tma_io.state =/= 0.U))
  ExcitingUtils.addSource(pendingVec.reduce(_||_), "TMA_l1miss")

  io.refill.valid := refill_arb.io.out.valid
  io.refill.bits  := refill_arb.io.out.bits
  refill_arb.io.out.ready := true.B

  if (!env.FPGAPlatform) {
    val difftest = Module(new DifftestRefillEvent)
    difftest.io.clock := clock
    difftest.io.coreid := hardId.U
    difftest.io.valid := io.refill.valid && io.refill.bits.hasdata && io.refill.bits.refill_done
    difftest.io.addr := io.refill.bits.addr
    difftest.io.data := io.refill.bits.data_raw.asTypeOf(difftest.io.data)
  }

  // one refill at a time
  OneHot.checkOneHot(refill_arb.io.in.map(r => r.valid))

  TLArbiter.lowest(edge, io.mem_acquire, entries.map(_.io.mem_acquire):_*)
  TLArbiter.lowest(edge, io.mem_finish,  entries.map(_.io.mem_finish):_*)

  io.pipe_req <> pipe_req_arb.io.out

  io.probe_block := probe_block_vec.asUInt.orR

  // print all input/output requests for debug purpose

  when (io.req.fire()) {
    io.req.bits.dump()
    // sanity check
    val source = io.req.bits.source
    val cmd = io.req.bits.cmd
    when (source === LOAD_SOURCE.U) {
      assert (cmd === M_XRD)
    }
    when (source === STORE_SOURCE.U) {
      assert (cmd === M_XWR)
    }

    when (source === AMO_SOURCE.U) {
      assert (
        cmd === M_XA_SWAP ||
        cmd === M_XLR     ||
        cmd === M_XSC     ||
        cmd === M_XA_ADD  ||
        cmd === M_XA_XOR  ||
        cmd === M_XA_OR   ||
        cmd === M_XA_AND  ||
        cmd === M_XA_MIN  ||
        cmd === M_XA_MAX  ||
        cmd === M_XA_MINU ||
        cmd === M_XA_MAXU)
    }
    // req addr must be aligned to block boundary
//    assert (io.req.bits.addr(blockOffBits - 1, 0) === 0.U)
  }

  when (io.refill.fire()) {
    io.refill.bits.dump()
  }

  when (io.mem_acquire.fire()) {
    XSDebug("mem_acquire ")
    io.mem_acquire.bits.dump
  }

  when (io.mem_grant.fire()) {
    XSDebug("mem_grant ")
    io.mem_grant.bits.dump
  }

  when (io.mem_finish.fire()) {
    XSDebug("mem_finish ")
    io.mem_finish.bits.dump
  }

  when (io.probe_block) {
    XSDebug(p"block probe req ${Hexadecimal(io.probe_req)}\n")
  }

  XSPerfAccumulate("miss_req", io.req.fire())
  XSPerfAccumulate("miss_req_allocate", io.req.fire() && allocate)
  XSPerfAccumulate("miss_req_merge_load", io.req.fire() && merge && !reject && io.req.bits.isLoad)
  XSPerfAccumulate("miss_req_reject_load", io.req.valid && reject && io.req.bits.isLoad)
  XSPerfAccumulate("probe_blocked_by_miss", io.probe_block)
  val max_inflight = RegInit(0.U((log2Up(cfg.nMissEntries) + 1).W))
  val num_valids = PopCount(~primary_ready.asUInt)
  when (num_valids > max_inflight) {
    max_inflight := num_valids
  }
  // max inflight (average) = max_inflight_total / cycle cnt
  XSPerfAccumulate("max_inflight", max_inflight)
  QueuePerf(cfg.nMissEntries, num_valids, num_valids === cfg.nMissEntries.U)
  io.full := num_valids === cfg.nMissEntries.U
  XSPerfHistogram("num_valids", num_valids, true.B, 0, cfg.nMissEntries, 1)
}
