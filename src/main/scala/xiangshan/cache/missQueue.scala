package xiangshan.cache

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._
import utils.{HasTLDump, XSDebug, BoolStopWatch}
import chisel3.ExcitingUtils._

class MissReq extends DCacheBundle
{
  val cmd  = UInt(M_SZ.W)
  val addr  = UInt(PAddrBits.W)
  val client_id  = UInt(missQueueClientIdWidth.W)
  val tag_match = Bool()
  val way_en    = Bits(nWays.W)
  val old_meta  = new L1Metadata
}

class MissResp extends DCacheBundle
{
  val client_id = UInt(missQueueClientIdWidth.W)
  val entry_id  = UInt(missQueueEntryIdWidth.W)
}

class MissFinish extends DCacheBundle
{
  val client_id  = UInt(missQueueClientIdWidth.W)
  val entry_id  = UInt(missQueueEntryIdWidth.W)
}


// One miss entry deals with one missed block
class MissEntry(edge: TLEdgeOut) extends DCacheModule
{
  val io = IO(new Bundle {
    // MSHR ID
    val id = Input(UInt())

    // client requests
    val req    = Flipped(DecoupledIO(new MissReq))
    val resp   = DecoupledIO(new MissResp)
    val finish = Flipped(DecoupledIO(new MissFinish))
    // refill to load queue to wake up missed requests
    val refill = ValidIO(new Refill)

    // bus
    val mem_acquire = DecoupledIO(new TLBundleA(edge.bundle))
    val mem_grant   = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))
    val mem_finish  = DecoupledIO(new TLBundleE(edge.bundle))

    // write back
    val wb_req      = DecoupledIO(new WritebackReq(edge.bundle.sourceBits))
    val wb_resp     = Input(Bool())

    // write meta and data
    val meta_write  = DecoupledIO(new L1MetaWriteReq)
    val data_write  = DecoupledIO(new L1DataWriteReq)

    // for synchronization
    val block_idx   = Output(Valid(UInt()))
    val block_addr  = Output(Valid(UInt()))

    val block_probe_idx   = Output(Valid(UInt()))
    val block_probe_addr  = Output(Valid(UInt()))

    // watch prober's write back requests
    val probe_wb_req = Flipped(ValidIO(new WritebackReq(edge.bundle.sourceBits)))
    val probe_active = Flipped(ValidIO(UInt()))
  })

  // MSHR:
  // 1. get req
  // 2. refill when necessary
  // 3. writeback when necessary
  // 4. update meta data
  // 5. send response back to client
  // 6. wait for client's finish
  // 7. done
  val s_invalid :: s_refill_req :: s_refill_resp :: s_mem_finish :: s_wait_probe_exit :: s_wb_req :: s_wb_resp :: s_data_write_req :: s_meta_write_req :: s_send_resp :: s_client_finish :: Nil = Enum(11)

  val state = RegInit(s_invalid)

  val req_reg = Reg(new MissReq)
  val req = Mux(io.req.fire(), io.req.bits, req_reg)
  val req_idx = get_idx(req.addr)
  val req_tag = get_tag(req.addr)
  val req_block_addr = get_block_addr(req.addr)

  // meta read results
  val req_tag_match = req.tag_match
  val req_old_meta = req.old_meta
  val req_way_en = req.way_en

  // what permission to release for the old block?
  val (_, shrink_param, coh_on_clear) = req_old_meta.coh.onCacheControl(M_FLUSH)

  // what permission to acquire for the new block?
  val new_coh = RegInit(ClientMetadata.onReset)
  val grow_param = new_coh.onAccess(req.cmd)._2
  val coh_on_grant = new_coh.onGrant(req.cmd, io.mem_grant.bits.param)

  val (_, _, refill_done, refill_address_inc) = edge.addr_inc(io.mem_grant)

  val grantack = Reg(Valid(new TLBundleE(edge.bundle)))
  val refill_ctr  = Reg(UInt(log2Up(refillCycles).W))
  val should_refill_data  = Reg(Bool())
  val needs_writeback  = Reg(Bool())

  // for read, we do not need to replay requests
  // just refill data to load queue, and then, we can exit
  // no need to walk through send_resp and client_finish state
  //
  // for store and amo
  // we send back response when we have finished everything
  // inform clients to replay requests
  val no_replay   = Reg(Bool())

  // assign default values to output signals
  io.req.ready           := false.B
  io.resp.valid          := false.B
  io.resp.bits           := DontCare
  io.finish.ready        := false.B

  io.refill.valid := false.B
  io.refill.bits  := DontCare

  io.mem_acquire.valid   := false.B
  io.mem_acquire.bits    := DontCare
  io.mem_grant.ready     := false.B
  io.mem_finish.valid    := false.B
  io.mem_finish.bits     := DontCare

  io.wb_req.valid        := false.B
  io.wb_req.bits         := DontCare

  io.meta_write.valid    := false.B
  io.meta_write.bits     := DontCare

  io.data_write.valid        := false.B
  io.data_write.bits         := DontCare

  io.block_idx.valid  := state =/= s_invalid
  io.block_addr.valid := state =/= s_invalid
  // break combinational loop
  io.block_idx.bits   := get_idx(req_reg.addr)
  io.block_addr.bits  := get_block_addr(req_reg.addr)

  // to preserve forward progress, we allow probe when we are dealing with acquire/grant
  io.block_probe_idx.valid  := state =/= s_invalid && state =/= s_refill_req && state =/= s_refill_resp
  io.block_probe_addr.valid := state =/= s_invalid && state =/= s_refill_req && state =/= s_refill_resp
  io.block_probe_idx.bits   := get_idx(req_reg.addr)
  io.block_probe_addr.bits  := get_block_addr(req_reg.addr)

  when (state =/= s_invalid) {
    XSDebug("entry: %d state: %d\n", io.id, state)
    XSDebug("entry: %d block_idx_valid: %b block_idx: %x block_addr_valid: %b block_addr: %x\n",
      io.id, io.block_idx.valid, io.block_idx.bits, io.block_addr.valid, io.block_addr.bits)
    XSDebug("entry: %d block_probe_idx_valid: %b block_probe_idx: %x block_probe_addr_valid: %b block_probe_addr: %x\n",
      io.id, io.block_probe_idx.valid, io.block_probe_idx.bits, io.block_probe_addr.valid, io.block_probe_addr.bits)
  }

  // --------------------------------------------
  // s_invalid: receive requests

  // decision making
  def decide_next_state(): UInt = {
    val new_state = WireInit(s_invalid)
    val old_coh   = req_old_meta.coh
    val needs_wb = old_coh.onCacheControl(M_FLUSH)._1 // does the line we are evicting need to be written back
    no_replay := req.cmd === M_XRD

    when (req_tag_match) {
      val (is_hit, _, coh_on_hit) = old_coh.onAccess(req.cmd)
      when (is_hit) { // set dirty bit
        // read should never go here
        // we get here only when we need to set dirty bit
        assert(isWrite(req.cmd))
        // go update meta
        new_coh     := coh_on_hit
        new_state   := s_meta_write_req
      } .otherwise { // upgrade permissions
        new_coh     := old_coh
        new_state   := s_refill_req
      }
    } .otherwise { // refill and writeback if necessary
      new_coh     := ClientMetadata.onReset
      should_refill_data := true.B
      needs_writeback := needs_wb
      // refill first to decrease load miss penalty
      new_state   := s_refill_req
    }
    new_state
  }

  when (state === s_invalid) {
    io.req.ready := true.B

    when (io.req.fire()) {
      grantack.valid := false.B
      refill_ctr := 0.U
      should_refill_data := false.B
      needs_writeback := false.B
      no_replay := false.B
      req_reg := io.req.bits
      state := decide_next_state()
    }
  }

  // --------------------------------------------
  // refill
  when (state === s_refill_req) {
    io.mem_acquire.valid := true.B
    // TODO: Use AcquirePerm if just doing permissions acquire
    // TODO: review this
    io.mem_acquire.bits  := edge.AcquireBlock(
      fromSource      = io.id,
      toAddress       = (Cat(req_tag, req_idx) << blockOffBits).asUInt(),
      lgSize          = (log2Up(cfg.blockBytes)).U,
      growPermissions = grow_param)._2
    when (io.mem_acquire.fire()) {
      state := s_refill_resp
    }
  }

  // ecc-encoded data
  val refill_data = Reg(Vec(blockRows, UInt(encRowBits.W)))
  // raw data
  val refill_data_raw = Reg(Vec(blockRows, UInt(rowBits.W)))
  when (state === s_refill_resp) {
    io.mem_grant.ready := true.B

    when (edge.hasData(io.mem_grant.bits)) {
      when (io.mem_grant.fire()) {
        // for AcquireBlock BtoT, we clear should_refill_data
        // and expect response with no data(Grant, not GrantData)
        // but block inclusive cache responded with a GrantData!
        // so we temporarily removed this assertion
        // we may consider using AcquirePerm BtoT for permission upgrade
        // assert(should_refill_data)
        refill_ctr := refill_ctr + 1.U
        for (i <- 0 until beatRows) {
          val row = io.mem_grant.bits.data(rowBits * (i + 1) - 1, rowBits * i)
          refill_data((refill_ctr << log2Floor(beatRows)) + i.U) := Cat((0 until rowWords).reverse map { w =>
            val word = row(wordBits * (w + 1) - 1, wordBits * w)
            val word_encoded = cacheParams.dataCode.encode(word)
            word_encoded
          })
          refill_data_raw((refill_ctr << log2Floor(beatRows)) + i.U) := row
        }

        when (refill_ctr === (refillCycles - 1).U) {
          assert(refill_done, "refill not done!")
        }
      }
    }

    when (refill_done) {
      grantack.valid := edge.isRequest(io.mem_grant.bits)
      grantack.bits := edge.GrantAck(io.mem_grant.bits)
      new_coh := coh_on_grant

      state := s_mem_finish
    }
  }

  // refill data to load queue
  io.refill.valid := RegNext(state === s_refill_resp && refill_done &&
    should_refill_data && no_replay)
  io.refill.bits.addr := req_block_addr
  io.refill.bits.data := refill_data_raw.asUInt

  when (state === s_mem_finish) {
    io.mem_finish.valid := grantack.valid
    io.mem_finish.bits  := grantack.bits

    when (io.mem_finish.fire()) {
      grantack.valid := false.B
      state := s_wait_probe_exit
    }
  }

  // --------------------------------------------
  // sync with probe
  when (state === s_wait_probe_exit) {
    // we only wait for probe, when prober is manipulating our set
    val should_wait_for_probe_exit = io.probe_active.valid && io.probe_active.bits === req_idx
    when (!should_wait_for_probe_exit) {
      when (needs_writeback) {
        // write back data
        state := s_wb_req
      } .otherwise {
        // no need to write back
        when (should_refill_data) {
          // fill data into dcache
          state := s_data_write_req
        } otherwise {
          // permission update only
          state := s_meta_write_req
        }
      }
    }
  }


  // during refill, probe may step in, it may release our blocks
  // if it releases the block we are trying to acquire, we don't care, since we will get it back eventually
  // but we need to know whether it releases the block we are trying to evict
  val prober_writeback_our_block = (state === s_refill_req || state === s_refill_resp ||
    state === s_mem_finish || state === s_wait_probe_exit) &&
    io.probe_wb_req.valid && !io.probe_wb_req.bits.voluntary &&
    io.probe_wb_req.bits.tag === req_old_meta.tag &&
    io.probe_wb_req.bits.idx === req_idx &&
    io.probe_wb_req.bits.way_en === req_way_en &&
    needs_writeback

  def onShrink(param: UInt): ClientMetadata = {
    import freechips.rocketchip.tilelink.ClientStates._
    import freechips.rocketchip.tilelink.TLPermissions._
    val state = MuxLookup(param, Nothing, Seq(
      TtoB   -> Branch,
      TtoN   -> Nothing,
      BtoN   -> Nothing))
    ClientMetadata(state)
  }

  when (prober_writeback_our_block) {
    req_reg.old_meta.coh := onShrink(io.probe_wb_req.bits.param)
  }

  // --------------------------------------------
  // write back
  when (state === s_wb_req) {
    io.wb_req.valid          := true.B

    io.wb_req.bits.tag       := req_old_meta.tag
    io.wb_req.bits.idx       := req_idx
    io.wb_req.bits.param     := shrink_param
    io.wb_req.bits.way_en    := req_way_en
    io.wb_req.bits.source    := io.id
    io.wb_req.bits.voluntary := true.B
    when (io.wb_req.fire()) {
      state := s_wb_resp
    }
  }

  when (state === s_wb_resp) {
    when (io.wb_resp) {
      state := s_data_write_req
    }
  }

  // --------------------------------------------
  // data write
  when (state === s_data_write_req) {
    io.data_write.valid        := true.B
    io.data_write.bits.addr    := req_block_addr
    io.data_write.bits.way_en  := req_way_en
    io.data_write.bits.wmask   := VecInit((0 until blockRows) map (i => ~0.U(rowWords.W)))
    io.data_write.bits.rmask   := DontCare
    io.data_write.bits.data    := refill_data

    when (io.data_write.fire()) {
      state := s_meta_write_req
    }
  }

  // --------------------------------------------
  // meta write
  when (state === s_meta_write_req) {
    io.meta_write.valid         := true.B
    io.meta_write.bits.idx      := req_idx
    io.meta_write.bits.data.coh := new_coh
    io.meta_write.bits.data.tag := req_tag
    io.meta_write.bits.way_en   := req_way_en

    when (io.meta_write.fire()) {
      when (no_replay) {
        // no need to replay, exit now
        state := s_invalid
      } .otherwise {
        state := s_send_resp
      }
    }
  }

  // --------------------------------------------
  when (state === s_send_resp) {
    io.resp.valid := true.B
    io.resp.bits.client_id := req.client_id
    io.resp.bits.entry_id := io.id

    when (io.resp.fire()) {
      // additional assertion
      val (is_hit, _, coh_on_hit) = new_coh.onAccess(req.cmd)
      assert(is_hit, "We still don't have permissions for this block")
      assert(new_coh === coh_on_hit, "Incorrect coherence meta data")

      state := s_client_finish
    }
  }

  when (state === s_client_finish) {
    io.finish.ready := true.B
    when (io.finish.fire()) {
      state := s_invalid
    }
  }
}


class MissQueue(edge: TLEdgeOut) extends DCacheModule with HasTLDump
{
  val io = IO(new Bundle {
    val req    = Flipped(DecoupledIO(new MissReq))
    val resp   = ValidIO(new MissResp)
    val finish = Flipped(DecoupledIO(new MissFinish))
    val refill = ValidIO(new Refill)

    val mem_acquire = Decoupled(new TLBundleA(edge.bundle))
    val mem_grant   = Flipped(Decoupled(new TLBundleD(edge.bundle)))
    val mem_finish  = Decoupled(new TLBundleE(edge.bundle))

    val wb_req      = Decoupled(new WritebackReq(edge.bundle.sourceBits))
    val wb_resp     = Input(Bool())

    val meta_write  = Decoupled(new L1MetaWriteReq)
    val data_write      = Decoupled(new L1DataWriteReq)

    val probe_wb_req = Flipped(ValidIO(new WritebackReq(edge.bundle.sourceBits)))
    val probe_active = Flipped(ValidIO(UInt()))

    val inflight_req_idxes       = Output(Vec(cfg.nMissEntries, Valid(UInt())))
    val inflight_req_block_addrs = Output(Vec(cfg.nMissEntries, Valid(UInt())))

    val block_probe_idxes    = Output(Vec(cfg.nMissEntries, Valid(UInt())))
    val block_probe_addrs    = Output(Vec(cfg.nMissEntries, Valid(UInt())))
  })

  val resp_arb       = Module(new Arbiter(new MissResp,         cfg.nMissEntries))
  val refill_arb     = Module(new Arbiter(new Refill,           cfg.nMissEntries))
  val meta_write_arb = Module(new Arbiter(new L1MetaWriteReq,   cfg.nMissEntries))
  val data_write_arb = Module(new Arbiter(new L1DataWriteReq,   cfg.nMissEntries))
  val wb_req_arb     = Module(new Arbiter(new WritebackReq(edge.bundle.sourceBits),     cfg.nMissEntries))

  // assign default values to output signals
  io.finish.ready := false.B
  io.mem_grant.ready := false.B

  val entry_alloc_idx = Wire(UInt())
  val req_ready = WireInit(false.B)

  val entries = (0 until cfg.nMissEntries) map { i =>
    val entry = Module(new MissEntry(edge))

    entry.io.id := i.U(log2Up(cfg.nMissEntries).W)

    // entry req
    entry.io.req.valid := (i.U === entry_alloc_idx) && io.req.valid
    entry.io.req.bits  := io.req.bits
    when (i.U === entry_alloc_idx) {
      req_ready := entry.io.req.ready
    }

    // entry resp
    resp_arb.io.in(i)       <>  entry.io.resp
    refill_arb.io.in(i).valid := entry.io.refill.valid
    refill_arb.io.in(i).bits  := entry.io.refill.bits

    // entry finish
    entry.io.finish.valid   :=  (i.U === io.finish.bits.entry_id) && io.finish.valid
    entry.io.finish.bits    :=  io.finish.bits
    when (entry.io.finish.valid) {
      io.finish.ready := entry.io.finish.ready
    }

    meta_write_arb.io.in(i) <>  entry.io.meta_write
    data_write_arb.io.in(i) <>  entry.io.data_write

    wb_req_arb.io.in(i)     <>  entry.io.wb_req
    entry.io.wb_resp        :=  io.wb_resp
    entry.io.probe_wb_req   <>  io.probe_wb_req
    entry.io.probe_active   <>  io.probe_active

    entry.io.mem_grant.valid := false.B
    entry.io.mem_grant.bits  := DontCare
    when (io.mem_grant.bits.source === i.U) {
      entry.io.mem_grant <> io.mem_grant
    }

    io.inflight_req_idxes(i)       <> entry.io.block_idx
    io.inflight_req_block_addrs(i) <> entry.io.block_addr
    io.block_probe_idxes(i)        <> entry.io.block_probe_idx
    io.block_probe_addrs(i)        <> entry.io.block_probe_addr

    if (!env.FPGAPlatform) {
      ExcitingUtils.addSource(
        BoolStopWatch(
          start = entry.io.block_idx.valid, 
          stop = !entry.io.block_idx.valid,
          startHighPriority = true),
        "perfCntDCacheMissQueuePenaltyEntry" + Integer.toString(i, 10),
        Perf
      )
    }

    entry
  }

  entry_alloc_idx    := PriorityEncoder(entries.map(m=>m.io.req.ready))

  io.req.ready  := req_ready
  io.resp.valid := resp_arb.io.out.valid
  io.resp.bits  := resp_arb.io.out.bits
  resp_arb.io.out.ready := true.B

  io.refill.valid := refill_arb.io.out.valid
  io.refill.bits  := refill_arb.io.out.bits
  refill_arb.io.out.ready := true.B

  // one refill at a time
  val refill_vec = refill_arb.io.in.map(c => c.valid)
  assert(PopCount(refill_vec) === 0.U || PopCount(refill_vec) === 1.U)

  io.meta_write <> meta_write_arb.io.out
  io.data_write <> data_write_arb.io.out
  io.wb_req     <> wb_req_arb.io.out

  TLArbiter.lowestFromSeq(edge, io.mem_acquire, entries.map(_.io.mem_acquire))
  TLArbiter.lowestFromSeq(edge, io.mem_finish,  entries.map(_.io.mem_finish))


  // print all input/output requests for debug purpose

  // print req
  val req = io.req
  XSDebug(req.fire(), "req cmd: %x addr: %x client_id: %d\n",
    req.bits.cmd, req.bits.addr, req.bits.client_id)

  val resp = io.resp
  XSDebug(resp.fire(), "resp client_id: %d entry_id: %d\n",
    resp.bits.client_id, resp.bits.entry_id)

  val finish = io.finish
  XSDebug(finish.fire(), "finish client_id: %d entry_id: %d\n",
    finish.bits.client_id, finish.bits.entry_id)

  // print refill
  XSDebug(io.refill.fire(), "refill addr %x\n", io.refill.bits.addr)

  // print data_write
  XSDebug(io.data_write.fire(), "data_write addr %x\n", io.data_write.bits.addr)

  // print meta_write
  XSDebug(io.meta_write.fire(), "meta_write idx %x way_en: %x old_tag: %x new_coh: %d new_tag: %x\n",
    io.meta_write.bits.idx, io.meta_write.bits.way_en, io.meta_write.bits.tag,
    io.meta_write.bits.data.coh.state, io.meta_write.bits.data.tag)

  // print wb_req
  XSDebug(io.wb_req.fire(), "wb_req idx %x tag: %x source: %d param: %x way_en: %x voluntary: %b\n",
    io.wb_req.bits.idx, io.wb_req.bits.tag,
    io.wb_req.bits.source, io.wb_req.bits.param,
    io.wb_req.bits.way_en, io.wb_req.bits.voluntary)

  // print tilelink messages
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

  if (!env.FPGAPlatform) {
    ExcitingUtils.addSource(io.req.fire(), "perfCntDCacheMiss", Perf)
  }
}
