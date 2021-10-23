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
import freechips.rocketchip.tilelink.ClientStates._
import freechips.rocketchip.tilelink.MemoryOpCategories._
import freechips.rocketchip.tilelink.TLPermissions._
import bus.tilelink.TLMessages._
import difftest._
import huancun.{AliasKey, DirtyKey, PreferCacheKey, PrefetchKey}

class MissReq(implicit p: Parameters) extends DCacheBundle {
  val source = UInt(sourceTypeWidth.W)
  val cmd = UInt(M_SZ.W)
  val addr = UInt(PAddrBits.W)
  val vaddr = UInt(VAddrBits.W)
  val way_en = UInt(DCacheWays.W)

  // store
  val store_data = UInt((cfg.blockBytes * 8).W)
  val store_mask = UInt(cfg.blockBytes.W)

  // which word does amo work on?
  val word_idx = UInt(log2Up(blockWords).W)
  val amo_data = UInt(DataBits.W)
  val amo_mask = UInt((DataBits / 8).W)

  val req_coh = new ClientMetadata
  val replace_coh = new ClientMetadata
  val replace_tag = UInt(tagBits.W)
  val id = UInt(reqIdWidth.W)

  def isLoad = source === LOAD_SOURCE.U
  def isStore = source === STORE_SOURCE.U
  def isAMO = source === AMO_SOURCE.U
  def hit = req_coh.isValid()
}

class MissEntry(edge: TLEdgeOut)(implicit p: Parameters) extends DCacheModule {
  val io = IO(new Bundle() {
    // MSHR ID
    val id = Input(UInt(log2Up(cfg.nMissEntries).W))
    // client requests
    // this entry is free and can be allocated to new reqs
    val primary_ready = Output(Bool())
    // this entry is busy, but it can merge the new req
    val secondary_ready = Output(Bool())
    // this entry is busy and it can not merge the new req
    val secondary_reject = Output(Bool())
    val req    = Flipped(ValidIO(new MissReq))
    val refill_to_ldq = ValidIO(new Refill)
    // TODO: bypass refill data to load pipe

    // bus
    val mem_acquire = DecoupledIO(new TLBundleA(edge.bundle))
    val mem_grant = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))
    val mem_finish = DecoupledIO(new TLBundleE(edge.bundle))

    // refill pipe
    val refill_pipe_req = DecoupledIO(new RefillPipeReq)

    // replace pipe
    val replace_pipe_req = DecoupledIO(new ReplacePipeReq)
    val replace_pipe_resp = Input(Bool())

    // main pipe: amo miss
    val main_pipe_req = DecoupledIO(new MainPipeReq)
    val main_pipe_resp = Input(Bool())

    val block_addr = ValidIO(UInt(PAddrBits.W))

    val debug_early_replace = ValidIO(new Bundle() {
      // info about the block that has been replaced
      val idx = UInt(idxBits.W) // vaddr
      val tag = UInt(tagBits.W) // paddr
    })
  })

  val req = Reg(new MissReq)
  val req_valid = RegInit(false.B)
  val set = addr_to_dcache_set(req.vaddr)

  val s_acquire = RegInit(true.B)
  val s_grantack = RegInit(true.B)
  val s_replace_req = RegInit(true.B)
  val s_refill = RegInit(true.B)
  val s_mainpipe_req = RegInit(true.B)

  val w_grantfirst = RegInit(true.B)
  val w_grantlast = RegInit(true.B)
  val w_replace_resp = RegInit(true.B)
  val w_mainpipe_resp = RegInit(true.B)

  val release_entry = s_grantack && s_refill && w_mainpipe_resp

  val acquire_not_sent = !s_acquire && !io.mem_acquire.ready
  val data_not_refilled = !w_grantlast

  val should_refill_data_reg =  Reg(Bool())
  val should_refill_data = WireInit(should_refill_data_reg)

  val full_overwrite = req.isStore && req.store_mask.andR

  val (_, _, refill_done, refill_count) = edge.count(io.mem_grant)
  val grant_param = Reg(UInt(TLPermissions.bdWidth.W))

  val grant_beats = RegInit(0.U(beatBits.W))

  when (io.req.valid && io.primary_ready) {
    req_valid := true.B
    req := io.req.bits
    req.addr := get_block_addr(io.req.bits.addr)

    s_acquire := false.B
    s_grantack := false.B

    w_grantfirst := false.B
    w_grantlast := false.B

    when (!io.req.bits.isAMO) {
      s_refill := false.B
    }

    when (!io.req.bits.hit && io.req.bits.replace_coh.isValid() && !io.req.bits.isAMO) {
      s_replace_req := false.B
      w_replace_resp := false.B
    }

    when (io.req.bits.isAMO) {
      s_mainpipe_req := false.B
      w_mainpipe_resp := false.B
    }

    should_refill_data_reg := io.req.bits.isLoad
    grant_beats := 0.U
  }.elsewhen (release_entry) {
    req_valid := false.B
  }

  when (io.req.valid && io.secondary_ready) {
    assert(io.req.bits.req_coh.state <= req.req_coh.state)
    assert(!(io.req.bits.isAMO || req.isAMO))
    // use the most uptodate meta
    req.req_coh := io.req.bits.req_coh

    when (io.req.bits.isStore) {
      req := io.req.bits
      req.addr := get_block_addr(io.req.bits.addr)
      req.way_en := req.way_en
      req.replace_coh := req.replace_coh
      req.replace_tag := req.replace_tag
    }

    should_refill_data := should_refill_data_reg || io.req.bits.isLoad
    should_refill_data_reg := should_refill_data
  }

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
  val isDirty = RegInit(false.B)
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
      assert(full_overwrite)
      for (i <- 0 until blockRows) {
        refill_data(i) := new_data(i)
      }
      w_grantlast := true.B
      hasData := false.B
    }

    refill_data_raw(refill_count) := io.mem_grant.bits.data
    isDirty := io.mem_grant.bits.echo.lift(DirtyKey).getOrElse(false.B)
  }

  when (io.mem_finish.fire()) {
    s_grantack := true.B
  }

  when (io.replace_pipe_req.fire()) {
    s_replace_req := true.B
  }

  when (io.replace_pipe_resp) {
    w_replace_resp := true.B
  }

  when (io.refill_pipe_req.fire()) {
    s_refill := true.B
  }

  when (io.main_pipe_req.fire()) {
    s_mainpipe_req := true.B
  }

  when (io.main_pipe_resp) {
    w_mainpipe_resp := true.B
  }

  def before_read_sent_can_merge(new_req: MissReq): Bool = {
    acquire_not_sent && req.isLoad && (new_req.isLoad || new_req.isStore)
  }

  def before_data_refill_can_merge(new_req: MissReq): Bool = {
    data_not_refilled && (req.isLoad || req.isStore) && new_req.isLoad
  }

  def should_merge(new_req: MissReq): Bool = {
    val block_match = req.addr === get_block_addr(new_req.addr)
    val beat_match = new_req.addr(blockOffBits - 1, beatOffBits) >= grant_beats
    block_match &&
    (before_read_sent_can_merge(new_req) ||
      beat_match && before_data_refill_can_merge(new_req))
  }

  def should_reject(new_req: MissReq): Bool = {
    val block_match = req.addr === get_block_addr(new_req.addr)
    val beat_match = new_req.addr(blockOffBits - 1, beatOffBits) >= grant_beats
    val set_match = set === addr_to_dcache_set(new_req.vaddr)

    req_valid &&
      Mux(
        block_match,
        !before_read_sent_can_merge(new_req) &&
          !(beat_match && before_data_refill_can_merge(new_req)),
        set_match && new_req.way_en === req.way_en
      )
  }

  io.primary_ready := !req_valid
  io.secondary_ready := should_merge(io.req.bits)
  io.secondary_reject := should_reject(io.req.bits)

  // should not allocate, merge or reject at the same time
  assert(RegNext(PopCount(Seq(io.primary_ready, io.secondary_ready, io.secondary_reject)) <= 1.U))

  val refill_data_splited = WireInit(VecInit(Seq.tabulate(cfg.blockBytes * 8 / l1BusDataWidth)(i => {
    val data = refill_data.asUInt
    data((i + 1) * l1BusDataWidth - 1, i * l1BusDataWidth)
  })))
  io.refill_to_ldq.valid := RegNext(!w_grantlast && io.mem_grant.fire()) && should_refill_data
  io.refill_to_ldq.bits.addr := RegNext(req.addr + (refill_count << refillOffBits))
  io.refill_to_ldq.bits.data := refill_data_splited(RegNext(refill_count))
  io.refill_to_ldq.bits.refill_done := RegNext(refill_done && io.mem_grant.fire())
  io.refill_to_ldq.bits.hasdata := hasData
  io.refill_to_ldq.bits.data_raw := refill_data_raw.asUInt

  io.mem_acquire.valid := !s_acquire
  val grow_param = req.req_coh.onAccess(req.cmd)._2
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
  // resolve cache alias by L2
  io.mem_acquire.bits.user.lift(AliasKey).foreach( _ := req.vaddr(13, 12))
  // trigger prefetch
  io.mem_acquire.bits.user.lift(PrefetchKey).foreach(_ := true.B)
  // prefer not to cache data in L2 by default
  io.mem_acquire.bits.user.lift(PreferCacheKey).foreach(_ := false.B)
  require(nSets <= 256)

  io.mem_grant.ready := !w_grantlast && s_acquire

  val grantack = RegEnable(edge.GrantAck(io.mem_grant.bits), io.mem_grant.fire())
  assert(RegNext(!io.mem_grant.fire() || edge.isRequest(io.mem_grant.bits)))
  io.mem_finish.valid := !s_grantack && w_grantfirst
  io.mem_finish.bits := grantack

  io.replace_pipe_req.valid := !s_replace_req
  val replace = io.replace_pipe_req.bits
  replace.miss_id := io.id
  replace.way_en := req.way_en
  replace.vaddr := req.vaddr
  replace.tag := req.replace_tag

  io.refill_pipe_req.valid := !s_refill && w_replace_resp && w_grantlast
  val refill = io.refill_pipe_req.bits
  refill.source := req.source
  refill.addr := req.addr
  refill.way_en := req.way_en
  refill.wmask := Mux(
    hasData || req.isLoad,
    ~0.U(DCacheBanks.W),
    VecInit((0 until DCacheBanks).map(i => get_mask_of_bank(i, req.store_mask).orR)).asUInt
  )
  refill.data := refill_data.asTypeOf((new RefillPipeReq).data)
  refill.miss_id := io.id
  refill.id := req.id
  def missCohGen(cmd: UInt, param: UInt, dirty: Bool) = {
    val c = categorize(cmd)
    MuxLookup(Cat(c, param, dirty), Nothing, Seq(
      //(effect param) -> (next)
      Cat(rd, toB, false.B)  -> Branch,
      Cat(rd, toB, true.B)   -> Branch,
      Cat(rd, toT, false.B)  -> Trunk,
      Cat(rd, toT, true.B)   -> Dirty,
      Cat(wi, toT, false.B)  -> Trunk,
      Cat(wi, toT, true.B)   -> Dirty,
      Cat(wr, toT, false.B)  -> Dirty,
      Cat(wr, toT, true.B)   -> Dirty))
  }
  refill.meta.coh := ClientMetadata(missCohGen(req.cmd, grant_param, isDirty))
  refill.alias := req.vaddr(13, 12) // TODO

  io.main_pipe_req.valid := !s_mainpipe_req && w_grantlast
  io.main_pipe_req.bits := DontCare
  io.main_pipe_req.bits.miss := true.B
  io.main_pipe_req.bits.miss_id := io.id
  io.main_pipe_req.bits.miss_param := grant_param
  io.main_pipe_req.bits.miss_dirty := isDirty
  io.main_pipe_req.bits.probe := false.B
  io.main_pipe_req.bits.source := req.source
  io.main_pipe_req.bits.cmd := req.cmd
  io.main_pipe_req.bits.vaddr := req.vaddr
  io.main_pipe_req.bits.addr := req.addr
  io.main_pipe_req.bits.store_data := refill_data.asUInt
  io.main_pipe_req.bits.store_mask := ~0.U(blockBytes.W)
  io.main_pipe_req.bits.word_idx := req.word_idx
  io.main_pipe_req.bits.amo_data := req.amo_data
  io.main_pipe_req.bits.amo_mask := req.amo_mask
  io.main_pipe_req.bits.id := req.id

  io.block_addr.valid := req_valid && w_grantlast && !s_refill
  io.block_addr.bits := req.addr

  io.debug_early_replace.valid := BoolStopWatch(io.replace_pipe_resp, io.refill_pipe_req.fire())
  io.debug_early_replace.bits.idx := addr_to_dcache_set(req.vaddr)
  io.debug_early_replace.bits.tag := req.replace_tag

  XSPerfAccumulate("miss_req_primary", io.req.valid && io.primary_ready)
  XSPerfAccumulate("miss_req_merged", io.req.valid && io.secondary_ready)
  XSPerfAccumulate("load_miss_penalty_to_use",
    should_refill_data &&
      BoolStopWatch(io.req.valid && io.primary_ready, io.refill_to_ldq.valid, true)
  )
  XSPerfAccumulate("main_pipe_penalty", BoolStopWatch(io.main_pipe_req.fire(), io.main_pipe_resp))
  XSPerfAccumulate("penalty_blocked_by_channel_A", io.mem_acquire.valid && !io.mem_acquire.ready)
  XSPerfAccumulate("penalty_waiting_for_channel_D", s_acquire && !w_grantlast && !io.mem_grant.valid)
  XSPerfAccumulate("penalty_waiting_for_channel_E", io.mem_finish.valid && !io.mem_finish.ready)
  XSPerfAccumulate("penalty_from_grant_to_refill", !s_refill && w_grantlast)
  XSPerfAccumulate("soft_prefetch_number", io.req.valid && io.primary_ready && io.req.bits.source === SOFT_PREFETCH.U)

  val (mshr_penalty_sample, mshr_penalty) = TransactionLatencyCounter(RegNext(io.req.valid && io.primary_ready), release_entry)
  XSPerfHistogram("miss_penalty", mshr_penalty, mshr_penalty_sample, 0, 100, 10)

  val load_miss_begin = io.req.valid && io.primary_ready && io.req.bits.isLoad
  val refill_finished = RegNext(!w_grantlast && refill_done) && should_refill_data
  val (load_miss_penalty_sample, load_miss_penalty) = TransactionLatencyCounter(load_miss_begin, refill_finished) // not real refill finish time
  XSPerfHistogram("load_miss_penalty_to_use", load_miss_penalty, load_miss_penalty_sample, 0, 100, 10)

  val (a_to_d_penalty_sample, a_to_d_penalty) = TransactionLatencyCounter(io.mem_acquire.fire(), io.mem_grant.fire() && refill_done)
  XSPerfHistogram("a_to_d_penalty", a_to_d_penalty, a_to_d_penalty_sample, 0, 100, 10)
}

class MissQueue(edge: TLEdgeOut)(implicit p: Parameters) extends DCacheModule {
  val io = IO(new Bundle {
    val req = Flipped(DecoupledIO(new MissReq))
    val refill_to_ldq = ValidIO(new Refill)

    val mem_acquire = DecoupledIO(new TLBundleA(edge.bundle))
    val mem_grant = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))
    val mem_finish = DecoupledIO(new TLBundleE(edge.bundle))

    val refill_pipe_req = DecoupledIO(new RefillPipeReq)

    val replace_pipe_req = DecoupledIO(new ReplacePipeReq)
    val replace_pipe_resp = Flipped(Vec(numReplaceRespPorts, ValidIO(new ReplacePipeResp)))

    val main_pipe_req = DecoupledIO(new MainPipeReq)
    val main_pipe_resp = Flipped(ValidIO(new AtomicsResp))

    // block probe
    val probe_addr = Input(UInt(PAddrBits.W))
    val probe_block = Output(Bool())

    val full = Output(Bool())

    // only for performance counter
    // This is valid when an mshr has finished replacing a block (w_replace_resp),
    // but hasn't received Grant from L2 (!w_grantlast)
    val debug_early_replace = Vec(cfg.nMissEntries, ValidIO(new Bundle() {
      // info about the block that has been replaced
      val idx = UInt(idxBits.W) // vaddr
      val tag = UInt(tagBits.W) // paddr
    }))
  })
  
  // 128KBL1: FIXME: provide vaddr for l2

  val entries = Seq.fill(cfg.nMissEntries)(Module(new MissEntry(edge)))

  val primary_ready_vec = entries.map(_.io.primary_ready)
  val secondary_ready_vec = entries.map(_.io.secondary_ready)
  val secondary_reject_vec = entries.map(_.io.secondary_reject)
  val probe_block_vec = entries.map { case e => e.io.block_addr.valid && e.io.block_addr.bits === io.probe_addr }

  val merge = Cat(secondary_ready_vec).orR
  val merge_idx = PriorityEncoder(secondary_ready_vec)

  val reject = Cat(secondary_reject_vec).orR

  val alloc = !reject && !merge && Cat(primary_ready_vec).orR
  val alloc_idx = PriorityEncoder(primary_ready_vec)

  val accept = alloc || merge
  val entry_idx = Mux(alloc, alloc_idx, merge_idx)

  assert(RegNext(PopCount(secondary_ready_vec) <= 1.U))
//  assert(RegNext(PopCount(secondary_reject_vec) <= 1.U))
  // It is possible that one mshr wants to merge a req, while another mshr wants to reject it.
  // That is, a coming req has the same paddr as that of mshr_0 (merge),
  // while it has the same set and the same way as mshr_1 (reject).
  // In this situation, the coming req should be merged by mshr_0
//  assert(RegNext(PopCount(Seq(merge, reject)) <= 1.U))

  def rrArbiter[T <: Bundle](
    in: Seq[DecoupledIO[T]],
    out: DecoupledIO[T],
    name: Option[String] = None): Unit = {
    val arb = Module(new RRArbiter[T](chiselTypeOf(out.bits), in.size))
    if (name.nonEmpty) { arb.suggestName(s"${name.get}_arb") }
    for ((a, req) <- arb.io.in.zip(in)) {
      a <> req
    }
    out <> arb.io.out
  }

  io.mem_grant.ready := false.B

  entries.zipWithIndex.foreach {
    case (e, i) =>
      e.io.id := i.U
      e.io.req.valid := entry_idx === i.U && accept && io.req.valid
      e.io.req.bits := io.req.bits

      e.io.mem_grant.valid := false.B
      e.io.mem_grant.bits := DontCare
      when (io.mem_grant.bits.source === i.U) {
        e.io.mem_grant <> io.mem_grant
      }

      e.io.replace_pipe_resp := Cat(io.replace_pipe_resp.map { case r => r.valid && r.bits.miss_id === i.U }).orR
      e.io.main_pipe_resp := io.main_pipe_resp.valid && io.main_pipe_resp.bits.ack_miss_queue && io.main_pipe_resp.bits.miss_id === i.U

      io.debug_early_replace(i) := e.io.debug_early_replace
  }

  io.req.ready := accept
  io.refill_to_ldq.valid := Cat(entries.map(_.io.refill_to_ldq.valid)).orR
  io.refill_to_ldq.bits := ParallelMux(entries.map(_.io.refill_to_ldq.valid) zip entries.map(_.io.refill_to_ldq.bits))

  TLArbiter.lowest(edge, io.mem_acquire, entries.map(_.io.mem_acquire):_*)
  TLArbiter.lowest(edge, io.mem_finish, entries.map(_.io.mem_finish):_*)

  rrArbiter(entries.map(_.io.refill_pipe_req), io.refill_pipe_req, Some("refill_pipe_req"))
  rrArbiter(entries.map(_.io.replace_pipe_req), io.replace_pipe_req, Some("replace_pipe_req"))
  rrArbiter(entries.map(_.io.main_pipe_req), io.main_pipe_req, Some("main_pipe_req"))

  io.probe_block := Cat(probe_block_vec).orR

  io.full := ~Cat(entries.map(_.io.primary_ready)).andR

  if (!env.FPGAPlatform) {
    val difftest = Module(new DifftestRefillEvent)
    difftest.io.clock := clock
    difftest.io.coreid := hardId.U
    difftest.io.valid := io.refill_to_ldq.valid && io.refill_to_ldq.bits.hasdata && io.refill_to_ldq.bits.refill_done
    difftest.io.addr := io.refill_to_ldq.bits.addr
    difftest.io.data := io.refill_to_ldq.bits.data_raw.asTypeOf(difftest.io.data)
  }

  XSPerfAccumulate("miss_req", io.req.fire())
  XSPerfAccumulate("miss_req_allocate", io.req.fire() && alloc)
  XSPerfAccumulate("miss_req_merge_load", io.req.fire() && merge && io.req.bits.isLoad)
  XSPerfAccumulate("miss_req_reject_load", io.req.valid && reject && io.req.bits.isLoad)
  XSPerfAccumulate("probe_blocked_by_miss", io.probe_block)
  val max_inflight = RegInit(0.U((log2Up(cfg.nMissEntries) + 1).W))
  val num_valids = PopCount(~Cat(primary_ready_vec).asUInt)
  when (num_valids > max_inflight) {
    max_inflight := num_valids
  }
  // max inflight (average) = max_inflight_total / cycle cnt
  XSPerfAccumulate("max_inflight", max_inflight)
  QueuePerf(cfg.nMissEntries, num_valids, num_valids === cfg.nMissEntries.U)
  io.full := num_valids === cfg.nMissEntries.U
  XSPerfHistogram("num_valids", num_valids, true.B, 0, cfg.nMissEntries, 1)
  val perfinfo = IO(new Bundle(){
    val perfEvents = Output(new PerfEventsBundle(5))
  })
  val perfEvents = Seq(
    ("dcache_missq_req          ", io.req.fire()                                                                                                                                                                       ),
    ("dcache_missq_1/4_valid    ", (PopCount(entries.map(entry => (!entry.io.primary_ready))) < (cfg.nMissEntries.U/4.U))                                                                                              ),
    ("dcache_missq_2/4_valid    ", (PopCount(entries.map(entry => (!entry.io.primary_ready))) > (cfg.nMissEntries.U/4.U)) & (PopCount(entries.map(entry => (!entry.io.primary_ready))) <= (cfg.nMissEntries.U/2.U))    ),
    ("dcache_missq_3/4_valid    ", (PopCount(entries.map(entry => (!entry.io.primary_ready))) > (cfg.nMissEntries.U/2.U)) & (PopCount(entries.map(entry => (!entry.io.primary_ready))) <= (cfg.nMissEntries.U*3.U/4.U))),
    ("dcache_missq_4/4_valid    ", (PopCount(entries.map(entry => (!entry.io.primary_ready))) > (cfg.nMissEntries.U*3.U/4.U))                                                                                          ),
  )

  for (((perf_out,(perf_name,perf)),i) <- perfinfo.perfEvents.perf_events.zip(perfEvents).zipWithIndex) {
    perf_out.incr_step := RegNext(perf)
  }
}
