//******************************************************************************
// Ported from Rocket-Chip
// See LICENSE.Berkeley and LICENSE.SiFive in Rocket-Chip for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package xiangshan.mem.cache

import chisel3._
import chisel3.util._

import utils.XSDebug
import bus.tilelink._

class MissReq extends DCacheBundle
{
  val cmd  = UInt(M_SZ.W)
  val addr  = UInt(PAddrBits.W)
  val client_id  = UInt(missQueueClientIdWidth.W)
}

class MissResp extends DCacheBundle
{
  val client_id  = UInt(missQueueClientIdWidth.W)
  val entry_id  = UInt(missQueueEntryIdWidth.W)
}

class MissFinish extends DCacheBundle
{
  val client_id  = UInt(missQueueClientIdWidth.W)
  val entry_id  = UInt(missQueueEntryIdWidth.W)
}


// One miss entry deals with one missed block
class MissEntry extends DCacheModule
{
  val io = IO(new Bundle {
    // MSHR ID
    val id = Input(UInt())

    // client requests
    val req    = Flipped(DecoupledIO(new MissReq))
    val resp   = ValidIO(new MissResp)
    val finish = Flipped(DecoupledIO(new MissFinish))

    val idx = Output(Valid(UInt()))
    val tag = Output(Valid(UInt()))

    val mem_acquire = Decoupled(new TLBundleA(cfg.busParams))
    val mem_grant   = Flipped(Decoupled(new TLBundleD(cfg.busParams)))
    val mem_finish  = Decoupled(new TLBundleE(cfg.busParams))

    val meta_read = Decoupled(new L1MetaReadReq)
    val meta_resp = Output(Vec(nWays, new L1Metadata))
    val meta_write  = Decoupled(new L1MetaWriteReq)
    val refill      = Decoupled(new L1DataWriteReq)

    val wb_req      = Decoupled(new WritebackReq)
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
  val s_invalid :: s_meta_read_req :: s_meta_read_resp :: s_decide_next_state :: s_wb_req :: s_wb_resp :: s_refill_req :: s_refill_resp :: s_mem_finish :: s_send_resp :: s_client_finish :: s_meta_write_req :: Nil = Enum(12)

  val state = RegInit(s_invalid)

  val req     = Reg(new MissReq)
  val req_idx = req.addr(untagBits-1, blockOffBits)
  val req_tag = req.addr >> untagBits
  val req_block_addr = (req.addr >> blockOffBits) << blockOffBits

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

  val (_, _, refill_done, refill_address_inc) = TLUtilities.addr_inc(io.mem_grant)

  val grantack = Reg(Valid(new TLBundleE(cfg.busParams)))
  val refill_ctr  = Reg(UInt(log2Up(cacheDataBeats).W))

  io.idx.valid := state =/= s_invalid
  io.tag.valid := state =/= s_invalid
  io.idx.bits  := req_idx
  io.tag.bits  := req_tag

  // assign default values to output signals
  io.req.ready           := false.B
  io.resp.valid          := false.B
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

  XSDebug("entry: %d state: %d\n", io.id, state)
  // --------------------------------------------
  // s_invalid: receive requests
  when (state === s_invalid) {
    io.req.ready := true.B

    when (io.req.fire()) {
      grantack.valid := false.B
      refill_ctr := 0.U
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
    when (req_tag_match) {
      val (is_hit, _, coh_on_hit) = old_coh.onAccess(req.cmd)
      when (is_hit) { // set dirty bit
        assert(isWrite(req.cmd))
        new_coh     := coh_on_hit
        new_state   := s_send_resp
      } .otherwise { // upgrade permissions
        new_coh     := old_coh
        new_state   := s_refill_req
      }
    } .otherwise { // refill and writeback if necessary
      new_coh     := ClientMetadata.onReset
      when (needs_wb) {
        new_state   := s_wb_req
      } .otherwise {
        new_state   := s_refill_req
      }
    }
    new_state
  }

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
    io.mem_acquire.bits  := TLMasterUtilities.AcquireBlock(
      params = cfg.busParams,
      fromSource      = io.id,
      toAddress       = Cat(req_tag, req_idx) << blockOffBits,
      lgSize          = (log2Up(cfg.blockBytes)).U,
      growPermissions = grow_param)._2
    when (io.mem_acquire.fire()) {
      state := s_refill_resp
    }
  }

  when (state === s_refill_resp) {
    when (TLUtilities.hasData(io.mem_grant.bits)) {
      io.mem_grant.ready      := io.refill.ready
      io.refill.valid         := io.mem_grant.valid
      io.refill.bits.addr     := req_block_addr | (refill_ctr << rowOffBits)
      io.refill.bits.way_en   := req_way_en
      io.refill.bits.wmask    := ~(0.U(rowWords.W))
      io.refill.bits.data     := io.mem_grant.bits.data

      when (io.refill.fire()) {
        refill_ctr := refill_ctr + 1.U
        when (refill_ctr === (cacheDataBeats - 1).U) {
          assert(refill_done, "refill not done!")
        }
      }
    } .otherwise {
      io.mem_grant.ready      := true.B
    }

    when (refill_done) {
      grantack.valid := TLUtilities.isRequest(io.mem_grant.bits)
      grantack.bits := TLMasterUtilities.GrantAck(io.mem_grant.bits)
      new_coh := coh_on_grant

      state := s_mem_finish
    }
  }

  when (state === s_mem_finish) {
    io.mem_finish.valid := grantack.valid
    io.mem_finish.bits  := grantack.bits

    when (io.mem_finish.fire()) {
      grantack.valid := false.B
      state := s_send_resp
    }
  }

  // --------------------------------------------
  // inform clients to replay requests
  when (state === s_send_resp) {
    io.resp.valid := true.B
    io.resp.bits.client_id := req.client_id
    io.resp.bits.entry_id := io.id

    when (io.resp.fire()) {
      when (isWrite(req.cmd)) {
        // Set dirty
        val (is_hit, _, coh_on_hit) = new_coh.onAccess(req.cmd)
        assert(is_hit, "We still don't have permissions for this store")
        new_coh := coh_on_hit
      }
      state := s_client_finish
    }
  }

  when (state === s_client_finish) {
    io.finish.ready := true.B
    when (io.finish.fire()) {
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
      state := s_invalid
    }
  }
}


class MissQueue extends DCacheModule
{
  val io = IO(new Bundle {
    val req    = Flipped(DecoupledIO(new MissReq))
    val resp   = DecoupledIO(new MissResp)
    val finish = Flipped(DecoupledIO(new MissFinish))

    val mem_acquire = Decoupled(new TLBundleA(cfg.busParams))
    val mem_grant   = Flipped(Decoupled(new TLBundleD(cfg.busParams)))
    val mem_finish  = Decoupled(new TLBundleE(cfg.busParams))

    val meta_read = Decoupled(new L1MetaReadReq)
    val meta_resp = Output(Vec(nWays, new L1Metadata))
    val meta_write  = Decoupled(new L1MetaWriteReq)
    val refill      = Decoupled(new L1DataWriteReq)

    val wb_req      = Decoupled(new WritebackReq)
    val wb_resp     = Input(Bool())
  })

  val resp_arb       = Module(new Arbiter(new MissResp,    cfg.nMissEntries))
  val finish_arb     = Module(new Arbiter(new MissFinish,  cfg.nMissEntries))
  val meta_read_arb  = Module(new Arbiter(new L1MetaReadReq,    cfg.nMissEntries))
  val meta_write_arb = Module(new Arbiter(new L1MetaWriteReq,   cfg.nMissEntries))
  val refill_arb     = Module(new Arbiter(new L1DataWriteReq,   cfg.nMissEntries))
  val wb_req_arb     = Module(new Arbiter(new WritebackReq,     cfg.nMissEntries))

  val entry_alloc_idx = Wire(UInt())
  val req_ready = WireInit(false.B)

  val entries = (0 until cfg.nMissEntries) map { i =>
    val entry = Module(new MissEntry)

    entry.io.id := i.U(log2Up(cfg.nMissEntries).W)

    // entry req
    entry.io.req.valid := (i.U === entry_alloc_idx) && io.req.valid
    entry.io.req.bits  := io.req.bits
    when (i.U === entry_alloc_idx) {
      req_ready := entry.io.req.ready
    }

    // entry resp
    resp_arb.io.in(i)     <>  entry.io.resp

    // entry finish
    entry.io.finish.valid   :=  (i.U === io.finish.bits.entry_id) && io.finish.valid
    entry.io.finish.bits    :=  io.finish.bits

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

    entry
  }

  entry_alloc_idx    := RegNext(PriorityEncoder(entries.map(m=>m.io.req.ready)))

  io.req.ready  := req_ready
  io.resp       <> resp_arb.io.out
  io.meta_read  <> meta_read_arb.io.out
  io.meta_write <> meta_write_arb.io.out
  io.refill     <> refill_arb.io.out
  io.wb_req     <> wb_req_arb.io.out

  TLArbiter.lowestFromSeq(io.mem_acquire, entries.map(_.io.mem_acquire))
  TLArbiter.lowestFromSeq(io.mem_finish,  entries.map(_.io.mem_finish))


  // print all input/output requests for debug purpose

  // print req
  val req = io.req
  XSDebug(req.fire(), "req cmd: %x addr: %x client_id: %d\n",
    req.bits.cmd, req.bits.addr, req.bits.client_id)

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
  /*
  XSDebug.exec(io.mem_acquire.fire(), io.mem_acquire.bits.dump)
  XSDebug.exec(io.mem_grant.fire(), io.mem_grant.bits.dump)
  XSDebug.exec(io.mem_finish.fire(), io.mem_finish.bits.dump)
  */
}
