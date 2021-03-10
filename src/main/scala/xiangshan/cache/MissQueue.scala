package xiangshan.cache

import chisel3._
import chisel3.util._
import chisel3.ExcitingUtils._

import freechips.rocketchip.tilelink.{TLEdgeOut, TLBundleA, TLBundleD, TLBundleE, TLPermissions, TLArbiter, ClientMetadata}
import utils.{HasTLDump, XSDebug, BoolStopWatch, OneHot, XSPerf}

class MissReq extends DCacheBundle
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
}

// One miss entry deals with one missed block
class MissEntry(edge: TLEdgeOut) extends DCacheModule
{
  val io = IO(new Bundle {
    // MSHR ID
    val id = Input(UInt())

    // client requests
    val req_valid = Input(Bool())
    // this entry is free and can be allocated to new reqs
    val primary_ready = Output(Bool())
    // this entry is busy, but it can merge the new req
    val secondary_ready = Output(Bool())
    // this entry is busy and it can not merge the new req
    val secondary_reject = Output(Bool())
    val req    = Input((new MissReq))
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

  // old MSHR:
  // 1. receive req
  // 2. send acquire req
  // 3. receive grant resp
  // 4. let main pipe do refill and replace
  // 5. wait for resp
  // 6. send finish to end the tilelink transaction
  //    We only send finish after data is written into cache.
  //    This prevents L2 from probing the block down.
  //    See Tilelink spec 1.8.1 page 69
  //    A slave should not issue a Probe if there is a pending GrantAck on the block. Once the Probe is
  //    issued, the slave should not issue further Probes on that block until it receives a ProbeAck.

  // new MSHR:
  // send finish to end the transaction before sending pipe_req
  val s_invalid :: s_refill_req :: s_refill_resp :: s_mem_finish :: s_main_pipe_req :: s_main_pipe_resp :: s_release_entry :: Nil = Enum(7)

  val state = RegInit(s_invalid)

  // --------------------------------------------
  // internal registers
  val req = Reg(new MissReq)

  // param of grant
  val grant_param = Reg(UInt(TLPermissions.bdWidth.W))

  // recording the source/sink info from Grant
  // so that we can use it grantack
  val grantack = Reg(Valid(new TLBundleE(edge.bundle)))

  // should we refill the data to load queue to wake up any missed load?
  val should_refill_data  = Reg(Bool())


  // --------------------------------------------
  // merge reqs
  // see whether we can merge requests
  // do not count s_invalid state in
  // since we can not merge request at that state
  val acquire_not_sent = state === s_refill_req && !io.mem_acquire.ready
  val data_not_refilled = state === s_refill_req || state === s_refill_resp

  def can_merge(new_req: MissReq): Bool = {
    // caution: do not merge with AMO
    // we can not do amoalu calculation in MissQueue
    // so, we do not know the result after AMO calculation
    // so do not merge with AMO

    // before read acquire is fired, we can merge read or write
    val before_read_sent = acquire_not_sent && req.source === LOAD_SOURCE.U && (new_req.source === LOAD_SOURCE.U || new_req.source === STORE_SOURCE.U)
    // before read/write refills data to LoadQueue, we can merge any read
    val before_data_refill = data_not_refilled && (req.source === LOAD_SOURCE.U || req.source === STORE_SOURCE.U) && new_req.source === LOAD_SOURCE.U

    before_read_sent || before_data_refill
  }

  def should_merge(new_req: MissReq): Bool = {
    val block_match = req.addr === new_req.addr
    block_match && can_merge(new_req)
  }

  def should_reject(new_req: MissReq): Bool = {
    val block_match = req.addr === new_req.addr
    // do not reject any req when we are in s_invalid
    block_match && !can_merge(new_req) && state =/= s_invalid
  }

  io.primary_ready    := state === s_invalid
  io.secondary_ready  := should_merge(io.req)
  io.secondary_reject := should_reject(io.req)

  // should not allocate, merge or reject at the same time
  // one at a time
  OneHot.checkOneHot(Seq(io.primary_ready, io.secondary_ready, io.secondary_reject))


  // --------------------------------------------
  // assign default values to output signals
  io.refill.valid := false.B
  io.refill.bits  := DontCare

  io.mem_acquire.valid   := false.B
  io.mem_acquire.bits    := DontCare
  io.mem_grant.ready     := false.B
  io.mem_finish.valid    := false.B
  io.mem_finish.bits     := DontCare

  io.pipe_req.valid := false.B
  io.pipe_req.bits  := DontCare

  io.block_addr.valid := state === s_mem_finish || state === s_main_pipe_req || state === s_main_pipe_resp
  io.block_addr.bits := req.addr

  when (state =/= s_invalid) {
    XSDebug("entry: %d state: %d\n", io.id, state)
    req.dump()
  }


  // --------------------------------------------
  // State Machine

  // --------------------------------------------
  // receive requests
  // primary request: allocate for a new request
  when (io.req_valid && io.primary_ready) {
    assert (state === s_invalid)

    // re init some fields
    req := io.req
    grantack.valid := false.B
    // only miss req from load needs a refill to LoadQueue
    should_refill_data := io.req.source === LOAD_SOURCE.U

    state := s_refill_req
  }

  // secondary request: merge with existing request
  when (io.req_valid && io.secondary_ready) {
    // The merged reqs should never have higher permissions
    // which means the cache silently upgrade the permission of our block
    // without merge with this miss queue request!
    // Either our req come in with stale meta, or the req that upgrade the permission does not merge with this req.
    // Both cases are bugs of DCache.
    //
    // DCache can silently drop permission(eg, probed or evicted)
    // it should never silently upgrade permissions.
    //
    // TODO: please check Tilelink Metadata.scala
    // and make sure that lower permission are encoded as smaller number
    assert (io.req.coh.state <= req.coh.state)
    // use the most uptodate meta
    req.coh := io.req.coh

    // when merging with store
    // we should remember its info into our req
    // or we will not be able to replay store
    when (io.req.source === STORE_SOURCE.U) {
      req := io.req
    }

    should_refill_data := should_refill_data || io.req.source === LOAD_SOURCE.U
  }


  // --------------------------------------------
  // refill

  // for full overwrite, we can use AcquirePerm to save memory bandwidth
  val full_overwrite = req.source === STORE_SOURCE.U && req.store_mask.andR
  when (state === s_refill_req) {

    val grow_param = req.coh.onAccess(req.cmd)._2
    val acquireBlock = edge.AcquireBlock(
      fromSource      = io.id,
      toAddress       = req.addr,
      lgSize          = (log2Up(cfg.blockBytes)).U,
      growPermissions = grow_param)._2
    val acquirePerm = edge.AcquirePerm(
      fromSource      = io.id,
      toAddress       = req.addr,
      lgSize          = (log2Up(cfg.blockBytes)).U,
      growPermissions = grow_param)._2

    io.mem_acquire.valid := true.B
    io.mem_acquire.bits := Mux(full_overwrite, acquirePerm, acquireBlock)

    when (io.mem_acquire.fire()) {
      state := s_refill_resp
    }
  }

  val (_, _, refill_done, refill_count) = edge.count(io.mem_grant)

  // raw data
  val refill_data = Reg(Vec(blockRows, UInt(rowBits.W)))
  val new_data    = Wire(Vec(blockRows, UInt(rowBits.W)))
  val new_mask    = Wire(Vec(blockRows, UInt(rowBytes.W)))

  for (i <- 0 until blockRows) {
    new_data(i) := req.store_data(rowBits * (i + 1) - 1, rowBits * i)
    // we only need to merge data for Store
    new_mask(i) := Mux(req.source === STORE_SOURCE.U,
      req.store_mask(rowBytes * (i + 1) - 1, rowBytes * i), 0.U(rowBytes.W))
  }

  def mergePutData(old_data: UInt, new_data: UInt, wmask: UInt): UInt = {
    val full_wmask = FillInterleaved(8, wmask)
    ((~full_wmask & old_data) | (full_wmask & new_data))
  }

  when (state === s_refill_resp) {
    io.mem_grant.ready := true.B
    when (io.mem_grant.fire()) {
      when (edge.hasData(io.mem_grant.bits)) {
        // GrantData
        for (i <- 0 until beatRows) {
          val idx = (refill_count << log2Floor(beatRows)) + i.U
          refill_data(idx) := mergePutData(io.mem_grant.bits.data(rowBits * (i + 1) - 1, rowBits * i), new_data(idx), new_mask(idx))
        }
      } .otherwise {
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
      }
    }

    when (refill_done) {
      grantack.valid := edge.isRequest(io.mem_grant.bits)
      grantack.bits := edge.GrantAck(io.mem_grant.bits)
      grant_param := io.mem_grant.bits.param

      state := s_mem_finish
    }
  }

  // put should_refill_data out of RegNext
  // so that when load miss are merged at refill_done
  // we can still refill data back
  io.refill.valid := RegNext(state === s_refill_resp && refill_done) && should_refill_data
  io.refill.bits.addr := req.addr
  io.refill.bits.data := refill_data.asUInt

  when (state === s_main_pipe_req) {
    io.pipe_req.valid := true.B
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

    when (io.pipe_req.fire()) {
      state := s_main_pipe_resp
    }
  }

  when (state === s_main_pipe_resp) {
    when (io.pipe_resp.fire()) {
      state := s_release_entry
    }
  }

  when (state === s_mem_finish) {
    io.mem_finish.valid := grantack.valid
    io.mem_finish.bits  := grantack.bits

    when (io.mem_finish.fire()) {
      grantack.valid := false.B
      state := s_main_pipe_req
    }
  }

  when (state === s_release_entry) {
    state := s_invalid
  }

  XSPerf("miss_req", io.req_valid && io.primary_ready)
  XSPerf("miss_penalty", BoolStopWatch(io.req_valid && io.primary_ready, state === s_release_entry))
  XSPerf("load_miss_penalty_to_use", should_refill_data && BoolStopWatch(io.req_valid && io.primary_ready, io.refill.valid, true))
  XSPerf("pipeline_penalty", BoolStopWatch(io.pipe_req.fire(), io.pipe_resp.fire()))
  XSPerf("penalty_blocked_by_channel_A", io.mem_acquire.valid && !io.mem_acquire.ready)
  XSPerf("penalty_waiting_for_channel_D", io.mem_grant.ready && !io.mem_grant.valid && state === s_refill_resp)
  XSPerf("penalty_blocked_by_channel_E", io.mem_finish.valid && !io.mem_finish.ready)
  XSPerf("penalty_blocked_by_pipeline", io.pipe_req.valid && !io.pipe_req.ready)
}


class MissQueue(edge: TLEdgeOut) extends DCacheModule with HasTLDump
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
    entry.io.req_valid  := (i.U === entry_idx) && accept && io.req.valid
    primary_ready(i)    := entry.io.primary_ready
    secondary_ready(i)  := entry.io.secondary_ready
    secondary_reject(i) := entry.io.secondary_reject
    probe_block_vec(i)  := entry.io.block_addr.valid && entry.io.block_addr.bits === io.probe_req
    entry.io.req        := io.req.bits

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

  io.refill.valid := refill_arb.io.out.valid
  io.refill.bits  := refill_arb.io.out.bits
  refill_arb.io.out.ready := true.B

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
    assert (io.req.bits.addr(blockOffBits - 1, 0) === 0.U)
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

  XSPerf("miss_req", io.req.fire())
  XSPerf("probe_blocked_by_miss", io.probe_block)
}
