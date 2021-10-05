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

class NewMissReq(implicit p: Parameters) extends DCacheBundle {
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
  def hit = req_coh.isValid()
}

class NewMissEntry(edge: TLEdgeOut)(implicit p: Parameters) extends DCacheModule {
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
    val req    = Flipped(ValidIO(new NewMissReq))
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

    val block_addr = ValidIO(UInt(PAddrBits.W))
  })

  val req = Reg(new NewMissReq)
  val req_valid = RegInit(false.B)
  val set = addr_to_dcache_set(req.vaddr)

  val s_acquire = RegInit(true.B)
  val s_grantack = RegInit(true.B)
  val s_replace_req = RegInit(true.B)
  val s_refill = RegInit(true.B)

  val w_grantfirst = RegInit(true.B)
  val w_grantlast = RegInit(true.B)
  val w_replace_resp = RegInit(true.B)

  val release_entry = s_grantack && s_refill

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
    s_refill := false.B

    w_grantfirst := false.B
    w_grantlast := false.B

    when (!io.req.bits.hit && io.req.bits.coh.isValid) {
      s_replace_req := false.B
      w_replace_resp := false.B
    }

    should_refill_data_reg := io.req.bits.isLoad
    grant_beats := 0.U
  }.elsewhen (release_entry) {
    req_valid := false.B
  }

  when (io.req.valid && io.secondary_ready) {
    assert(io.req.bits.req_coh.state <= req.req_coh.state)
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

  def before_read_sent_can_merge(new_req: NewMissReq): Bool = {
    acquire_not_sent && req.isLoad && (new_req.isLoad || new_req.isStore)
  }

  def before_data_refill_can_merge(new_req: NewMissReq): Bool = {
    data_not_refilled && (req.isLoad || req.isStore) && new_req.isLoad
  }

  def should_merge(new_req: NewMissReq): Bool = {
    val block_match = req.addr === get_block_addr(new_req.addr)
    val beat_match = new_req.addr(blockBytes - 1, beatOffBits) >= grant_beats
    block_match &&
    (before_read_sent_can_merge(new_req) ||
      beat_match && before_data_refill_can_merge(new_req))
  }

  def should_reject(new_req: NewMissReq): Bool = {
    val block_match = req.addr === get_block_addr(new_req.addr)
    val beat_match = new_req.addr(blockBytes - 1, beatOffBits) >= grant_beats
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
  OneHot.checkOneHot(Seq(io.primary_ready, io.secondary_ready, io.secondary_reject))

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
  replace.vaddr := req.vaddr // TODO: make sure only set in vaddr is used
  replace.tag := req.replace_tag

  io.refill_pipe_req.valid := !s_refill && w_replace_resp && w_grantlast
  val refill = io.refill_pipe_req.bits
  refill.source := req.source
  refill.addr := req.addr
  refill.way_en := req.way_en
  refill.wmask := Mux(
    hasData || req.isLoad,
    ~0.U,
    VecInit((0 until DCacheBanks).map(i => get_mask_of_bank(i, req.store_mask).orR)).asUInt
  )
  refill.data := refill_data.asTypeOf((new RefillPipeReq).data)
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

  io.block_addr.valid := req_valid && w_grantlast && !s_refill
  io.block_addr.bits := req.addr
}

class NewMissQueue(edge: TLEdgeOut)(implicit p: Parameters) extends DCacheModule {
  val io = IO(new Bundle {
    val req = Flipped(DecoupledIO(new NewMissReq))
    val refill_to_ldq = ValidIO(new Refill)

    val mem_acquire = DecoupledIO(new TLBundleA(edge.bundle))
    val mem_grant = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))
    val mem_finish = DecoupledIO(new TLBundleE(edge.bundle))

    val refill_pipe_req = DecoupledIO(new RefillPipeReq)

    val replace_pipe_req = DecoupledIO(new ReplacePipeReq)
    val replace_pipe_resp = Flipped(Vec(numReplaceRespPorts, ValidIO(new ReplacePipeResp)))

    // block probe
    val probe_addr = Input(UInt(PAddrBits.W))
    val probe_block = Output(Bool())

    val full = Output(Bool())
  })
  
  // 128KBL1: FIXME: provide vaddr for l2

  val entries = Seq.fill(cfg.nMissEntries)(Module(new NewMissEntry))

  val primary_ready_vec = entries.map(_.io.primary_ready)
  val secondary_ready_vec = entries.map(_.io.secondary_ready)
  val secondary_reject_vec = entries.map(_.io.secondary_reject)
  val probe_block_vec = entries.map { case e => e.io.block_addr.valid && e.io.block_addr.bits === io.probe_addr }

  val merge = Cat(secondary_ready_vec).orR
  val merge_idx = PriorityEncoder(secondary_ready_vec)

  val reject = Cat(secondary_reject_vec).orR

  val alloc = !reject && !merge && Cat(primary_ready_vec).orR
  val alloc_idx = PriorityEncoder(primary_ready_vec)

  val accept = (merge || alloc) && !reject
  val entry_idx = Mux(alloc, alloc_idx, merge_idx)

  OneHot.checkOneHot(secondary_ready_vec)
  OneHot.checkOneHot(secondary_reject_vec)
  OneHot.checkOneHot(Seq(merge, reject))

  def arbiter[T <: Bundle](
    in: Seq[DecoupledIO[T]],
    out: DecoupledIO[T],
    roundRobin: Boolean = false,
    name: Option[String] = None): Unit = {
    val arb = if (roundRobin) {
      Module(new RRArbiter[T](chiselTypeOf(out.bits), in.size))
    } else {
      Module(new Arbiter[T](chiselTypeOf(out.bits), in.size))
    }
    if (name.nonEmpty) { arb.suggestName(s"${name.get}_arb") }
    for ((a, req) <- arb.io.in.zip(in)) {
      a <> req
    }
    out <> arb.io.out
  }

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
  }

  io.req.ready := accept
  io.refill_to_ldq.valid := Cat(entries.map(_.io.refill_to_ldq.valid)).orR
  io.refill_to_ldq.bits := ParallelMux(entries.map(_.io.refill_to_ldq.valid) zip entries.map(_.io.refill_to_ldq.bits))

  TLArbiter.lowest(edge, io.mem_acquire, entries.map(_.io.mem_acquire):_*)
  TLArbiter.lowest(edge, io.mem_finish, entries.map(_.io.mem_finish):_*)

  arbiter(entries.map(_.io.refill_pipe_req), io.refill_pipe_req, true, Some("refill_pipe_req"))
  arbiter(entries.map(_.io.replace_pipe_req), io.replace_pipe_req, true, Some("replace_pipe_req"))

  io.probe_block := Cat(probe_block_vec).orR

  io.full := ~Cat(entries.map(_.io.primary_ready)).andR
}
