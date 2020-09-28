package xiangshan.cache

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._
import utils.{HasTLDump, XSDebug}

class MissReq extends DCacheBundle
{
  val cmd  = UInt(M_SZ.W)
  val addr  = UInt(PAddrBits.W)
  val client_id  = UInt(missQueueClientIdWidth.W)
}

class MissResp extends DCacheBundle
{
  val client_id = UInt(missQueueClientIdWidth.W)
  val entry_id  = UInt(missQueueEntryIdWidth.W)
  val way_en    = Bits(nWays.W)
  val has_data  = Bool()
  val data      = UInt(blockBits.W)
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

    val block_idx   = Output(Valid(UInt()))
    val block_addr  = Output(Valid(UInt()))

    val block_probe_idx   = Output(Valid(UInt()))
    val block_probe_addr  = Output(Valid(UInt()))

    val mem_acquire = DecoupledIO(new TLBundleA(edge.bundle))
    val mem_grant   = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))
    val mem_finish  = DecoupledIO(new TLBundleE(edge.bundle))

    val meta_read   = DecoupledIO(new L1MetaReadReq)
    val meta_resp   = Input(Vec(nWays, new L1Metadata))
    val meta_write  = DecoupledIO(new L1MetaWriteReq)
    val refill      = DecoupledIO(new L1DataWriteReq)

    val wb_req      = DecoupledIO(new WritebackReq(edge.bundle.sourceBits))
    val wb_resp     = Input(Bool())
  })

  // MSHR:
  // 1. get req
  // 2. read meta data and make replacement decisions
  // 3. do writeback/refill when necessary
  // 4. send response back to client
  // 5. wait for client's finish
  // 6. update meta data
  // 7. done
  val s_invalid :: s_meta_read_req :: s_meta_read_resp :: s_decide_next_state :: s_wb_req :: s_wb_resp :: s_refill_req :: s_refill_resp :: s_data_write_req :: s_mem_finish :: s_send_resp :: s_client_finish :: s_meta_write_req :: Nil = Enum(13)

  val state = RegInit(s_invalid)

  val req     = Reg(new MissReq)
  val req_idx = get_idx(req.addr)
  val req_tag = get_tag(req.addr)
  val req_block_addr = get_block_addr(req.addr)

  // meta read results
  val req_tag_match = Reg(Bool())
  val req_old_meta = Reg(new L1Metadata)
  val req_way_en = Reg(UInt(nWays.W))

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

  // for read, to shorten latency
  // we send back response as soon as possible
  //
  // for store and amo
  // we send back response when we have finished everything
  // inform clients to replay requests
  val early_response   = Reg(Bool())

  io.block_idx.valid  := state =/= s_invalid
  io.block_addr.valid := state =/= s_invalid
  io.block_idx.bits   := req_idx
  io.block_addr.bits  := req_block_addr

  // to preserve forward progress, we allow probe when we are dealing with acquire/grant
  io.block_probe_idx.valid  := state =/= s_invalid && state =/= s_refill_req && state =/= s_refill_resp
  io.block_probe_addr.valid := state =/= s_invalid && state =/= s_refill_req && state =/= s_refill_resp
  io.block_probe_idx.bits   := req_idx
  io.block_probe_addr.bits  := req_block_addr

  // assign default values to output signals
  io.req.ready           := false.B
  io.resp.valid          := false.B
  io.resp.bits           := DontCare
  io.finish.ready        := false.B

  io.mem_acquire.valid   := false.B
  io.mem_acquire.bits    := DontCare

  io.mem_grant.ready     := false.B

  io.mem_finish.valid    := false.B
  io.mem_finish.bits     := DontCare

  io.meta_read.valid     := false.B
  io.meta_read.bits      := DontCare

  io.meta_write.valid    := false.B
  io.meta_write.bits     := DontCare

  io.refill.valid        := false.B
  io.refill.bits         := DontCare

  io.wb_req.valid        := false.B
  io.wb_req.bits         := DontCare

  when (state =/= s_invalid) {
    XSDebug("entry: %d state: %d\n", io.id, state)
    XSDebug("entry: %d block_idx_valid: %b block_idx: %x block_addr_valid: %b block_addr: %x\n",
      io.id, io.block_idx.valid, io.block_idx.bits, io.block_addr.valid, io.block_addr.bits)
    XSDebug("entry: %d block_probe_idx_valid: %b block_probe_idx: %x block_probe_addr_valid: %b block_probe_addr: %x\n",
      io.id, io.block_probe_idx.valid, io.block_probe_idx.bits, io.block_probe_addr.valid, io.block_probe_addr.bits)
  }


  // --------------------------------------------
  // s_invalid: receive requests
  when (state === s_invalid) {
    io.req.ready := true.B

    when (io.req.fire()) {
      grantack.valid := false.B
      refill_ctr := 0.U
      should_refill_data := false.B
      needs_writeback := false.B
      early_response := false.B
      req := io.req.bits
      state := s_meta_read_req
    }
  }

  // --------------------------------------------
  // s_meta_read_req: read meta data
  when (state === s_meta_read_req) {
    io.meta_read.valid := true.B
    val meta_read = io.meta_read.bits
    meta_read.idx    := req_idx
    meta_read.way_en := ~0.U(nWays.W)
    meta_read.tag    := DontCare

    when (io.meta_read.fire()) {
      state := s_meta_read_resp
    }
  }

  // s_meta_read_resp: handle meta read response
  // check hit, miss
  when (state === s_meta_read_resp) {
    // tag check
    def wayMap[T <: Data](f: Int => T) = VecInit((0 until nWays).map(f))
    val tag_eq_way = wayMap((w: Int) => io.meta_resp(w).tag === (req_tag)).asUInt
    val tag_match_way = wayMap((w: Int) => tag_eq_way(w) && io.meta_resp(w).coh.isValid()).asUInt
    val tag_match     = tag_match_way.orR
    val hit_meta     = Mux1H(tag_match_way, wayMap((w: Int) => io.meta_resp(w)))
    val hit_state     = hit_meta.coh
    val has_permission = hit_state.onAccess(req.cmd)._1
    val new_hit_state  = hit_state.onAccess(req.cmd)._3
    val hit = tag_match && has_permission && hit_state === new_hit_state

    // replacement policy
    val replacer = cacheParams.replacement
    val replaced_way_en = UIntToOH(replacer.way)
    val repl_meta = Mux1H(replaced_way_en, wayMap((w: Int) => io.meta_resp(w)))

    req_tag_match   := tag_match
    req_old_meta    := Mux(tag_match, hit_meta, repl_meta)
    req_way_en      := Mux(tag_match, tag_match_way, replaced_way_en)

    replacer.miss

    state := s_decide_next_state
  }


  // decision making
  def decide_next_state(): UInt = {
    val new_state = WireInit(s_invalid)
    val old_coh   = req_old_meta.coh
    val needs_wb = old_coh.onCacheControl(M_FLUSH)._1 // does the line we are evicting need to be written back
    early_response := req.cmd === M_XRD

    when (req_tag_match) {
      val (is_hit, _, coh_on_hit) = old_coh.onAccess(req.cmd)
      when (is_hit) { // set dirty bit
        // we do not need to assert write any more
        // read may go here as well
        // eg: when several load miss on the same block
        when (req.cmd === M_XRD) {
          // normal read
          // read hit, no need to update meta
          new_state := s_send_resp
        } .otherwise {
          assert(isWrite(req.cmd))
          new_coh     := coh_on_hit
          new_state   := s_meta_write_req
        }
      } .otherwise { // upgrade permissions
        new_coh     := old_coh
        new_state   := s_refill_req
      }
    } .otherwise { // refill and writeback if necessary
      new_coh     := ClientMetadata.onReset
      should_refill_data := true.B
      when (needs_wb) {
        new_state   := s_wb_req
        needs_writeback := true.B
      } .otherwise {
        new_state   := s_refill_req
      }
    }
    new_state
  }

  // this state is unnecessary, we can make decisions in s_meta_read_resp
  when (state === s_decide_next_state) {
    state := decide_next_state()
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
      state := s_refill_req
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

  val refill_data = Reg(Vec(blockRows, UInt(encRowBits.W)))
  // not encoded data
  val refill_data_raw = Reg(Vec(blockRows, UInt(rowBits.W)))
  when (state === s_refill_resp) {
    io.mem_grant.ready := true.B

    when (edge.hasData(io.mem_grant.bits)) {
      when (io.mem_grant.fire()) {
        assert(should_refill_data)
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

  when (state === s_mem_finish) {
    io.mem_finish.valid := grantack.valid
    io.mem_finish.bits  := grantack.bits

    when (io.mem_finish.fire()) {
      grantack.valid := false.B

      // no data
      when (!should_refill_data) {
        state := s_meta_write_req
      } .otherwise {
        when (early_response) {
          state := s_send_resp
        } .otherwise {
          state := s_data_write_req
        }
      }
    }
  }

  when (state === s_data_write_req) {
    io.refill.valid        := true.B
    io.refill.bits.addr    := req_block_addr
    io.refill.bits.way_en  := req_way_en
    io.refill.bits.wmask   := VecInit((0 until blockRows) map (i => ~0.U(rowWords.W)))
    io.refill.bits.rmask   := DontCare
    io.refill.bits.data    := refill_data

    when (io.refill.fire()) {
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
      when (early_response) {
        state := s_client_finish
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
    io.resp.bits.way_en := req_way_en
    io.resp.bits.has_data := should_refill_data
    io.resp.bits.data := refill_data_raw.asUInt

    when (io.resp.fire()) {
      // additional assertion
      val (is_hit, _, coh_on_hit) = new_coh.onAccess(req.cmd)
      assert(is_hit, "We still don't have permissions for this block")
      assert(new_coh === coh_on_hit, "Incorrect coherence meta data")

      // for read, we will write data later
      when (early_response && should_refill_data) {
        state := s_data_write_req
      } .otherwise {
        state := s_client_finish
      }
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

    val mem_acquire = Decoupled(new TLBundleA(edge.bundle))
    val mem_grant   = Flipped(Decoupled(new TLBundleD(edge.bundle)))
    val mem_finish  = Decoupled(new TLBundleE(edge.bundle))

    val meta_read   = Decoupled(new L1MetaReadReq)
    val meta_resp   = Input(Vec(nWays, new L1Metadata))
    val meta_write  = Decoupled(new L1MetaWriteReq)
    val refill      = Decoupled(new L1DataWriteReq)

    val wb_req      = Decoupled(new WritebackReq(edge.bundle.sourceBits))
    val wb_resp     = Input(Bool())

    val inflight_req_idxes       = Output(Vec(cfg.nMissEntries, Valid(UInt())))
    val inflight_req_block_addrs = Output(Vec(cfg.nMissEntries, Valid(UInt())))

    val block_probe_idxes    = Output(Vec(cfg.nMissEntries, Valid(UInt())))
    val block_probe_addrs    = Output(Vec(cfg.nMissEntries, Valid(UInt())))
  })

  val resp_arb       = Module(new Arbiter(new MissResp,         cfg.nMissEntries))
  val meta_read_arb  = Module(new Arbiter(new L1MetaReadReq,    cfg.nMissEntries))
  val meta_write_arb = Module(new Arbiter(new L1MetaWriteReq,   cfg.nMissEntries))
  val refill_arb     = Module(new Arbiter(new L1DataWriteReq,   cfg.nMissEntries))
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

    // entry finish
    entry.io.finish.valid   :=  (i.U === io.finish.bits.entry_id) && io.finish.valid
    entry.io.finish.bits    :=  io.finish.bits
    when (entry.io.finish.valid) {
      io.finish.ready := entry.io.finish.ready
    }

    meta_read_arb.io.in(i)  <>  entry.io.meta_read
    entry.io.meta_resp      :=  io.meta_resp

    meta_write_arb.io.in(i) <>  entry.io.meta_write
    refill_arb.io.in(i)     <>  entry.io.refill

    wb_req_arb.io.in(i)     <>  entry.io.wb_req
    entry.io.wb_resp        :=  io.wb_resp

    entry.io.mem_grant.valid := false.B
    entry.io.mem_grant.bits  := DontCare
    when (io.mem_grant.bits.source === i.U) {
      entry.io.mem_grant <> io.mem_grant
    }

    io.inflight_req_idxes(i)       <> entry.io.block_idx
    io.inflight_req_block_addrs(i) <> entry.io.block_addr
    io.block_probe_idxes(i)        <> entry.io.block_probe_idx
    io.block_probe_addrs(i)        <> entry.io.block_probe_addr

    entry
  }

  entry_alloc_idx    := PriorityEncoder(entries.map(m=>m.io.req.ready))

  io.req.ready  := req_ready
  io.resp.valid := resp_arb.io.out.valid
  io.resp.bits  := resp_arb.io.out.bits
  resp_arb.io.out.ready := true.B

  io.meta_read  <> meta_read_arb.io.out
  io.meta_write <> meta_write_arb.io.out
  io.refill     <> refill_arb.io.out
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
}
