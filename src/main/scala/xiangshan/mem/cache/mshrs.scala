//******************************************************************************
// Ported from Rocket-Chip
// See LICENSE.Berkeley and LICENSE.SiFive in Rocket-Chip for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package xiangshan.mem.cache

import chisel3._
import chisel3.util._

import xiangshan.mem.DCacheReq
import xiangshan.utils.XSDebug
import bus.tilelink._

class DCacheReqInternal extends DCacheReq
  with HasDCacheParameters
{
  // miss info
  val tag_match = Bool()
  val old_meta  = new L1Metadata
  val way_en    = UInt(nWays.W)

  val sdq_id    = UInt(log2Up(cfg.nSDQ).W)
}


class MSHR extends DCacheModule
{
  val io = IO(new Bundle {
    val id = Input(UInt())

    val req_pri_val = Input(Bool())
    val req_pri_rdy = Output(Bool())
    val req         = Input(new DCacheReqInternal)

    val idx = Output(Valid(UInt()))
    val way = Output(Valid(UInt()))
    val tag = Output(Valid(UInt()))

    val mem_acquire = Decoupled(new TLBundleA(cfg.busParams))
    val mem_grant   = Flipped(Decoupled(new TLBundleD(cfg.busParams)))
    val mem_finish  = Decoupled(new TLBundleE(cfg.busParams))

    val refill      = Decoupled(new L1DataWriteReq)

    val meta_write  = Decoupled(new L1MetaWriteReq)

    val wb_req      = Decoupled(new WritebackReq)
    val wb_resp     = Input(Bool())

    // Replays go through the cache pipeline again
    val replay      = Decoupled(new DCacheReqInternal)
  })

  // TODO: Optimize this. We don't want to mess with cache during speculation
  // s_refill_req      : Make a request for a new cache line
  // s_refill_resp     : Store the refill response into our buffer
  // s_drain_rpq_loads : Drain out loads from the rpq
  //                   : If miss was misspeculated, go to s_invalid
  // s_wb_req          : Write back the evicted cache line
  // s_wb_resp         : Finish writing back the evicted cache line
  // s_meta_write_req  : Write the metadata for new cache lne
  // s_meta_write_resp :

  val s_invalid :: s_refill_req :: s_refill_resp :: s_wb_req :: s_wb_resp :: s_drain_rpq :: s_meta_write_req :: s_mem_finish :: Nil = Enum(8)
  val state = RegInit(s_invalid)

  val req     = Reg(new DCacheReqInternal)
  val req_idx = req.addr(untagBits-1, blockOffBits)
  val req_tag = req.addr >> untagBits
  val req_block_addr = (req.addr >> blockOffBits) << blockOffBits

  val new_coh = RegInit(ClientMetadata.onReset)
  val (_, shrink_param, coh_on_clear) = req.old_meta.coh.onCacheControl(M_FLUSH)
  val grow_param = new_coh.onAccess(req.cmd)._2
  val coh_on_grant = new_coh.onGrant(req.cmd, io.mem_grant.bits.param)

  val (_, _, refill_done, refill_address_inc) = TLUtilities.addr_inc(io.mem_grant)

  val rpq = Module(new Queue(new DCacheReqInternal, cfg.nRPQ))

  rpq.io.enq.valid := io.req_pri_val && io.req_pri_rdy
  rpq.io.enq.bits  := io.req
  rpq.io.deq.ready := false.B


  val grantack = Reg(Valid(new TLBundleE(cfg.busParams)))
  val refill_ctr  = Reg(UInt(log2Up(cacheDataBeats).W))

  io.idx.valid := state =/= s_invalid
  io.tag.valid := state =/= s_invalid
  io.way.valid := state =/= s_invalid
  io.idx.bits := req_idx
  io.tag.bits := req_tag
  io.way.bits := req.way_en

  // assign default values to output signals
  io.req_pri_rdy         := false.B

  io.mem_acquire.valid   := false.B
  io.mem_acquire.bits    := DontCare

  io.mem_grant.ready     := false.B

  io.mem_finish.valid    := false.B
  io.mem_finish.bits     := DontCare

  io.refill.valid        := false.B
  io.refill.bits         := DontCare

  io.meta_write.valid    := false.B
  io.meta_write.bits     := DontCare

  io.wb_req.valid        := false.B
  io.wb_req.bits         := DontCare

  io.replay.valid        := false.B
  io.replay.bits         := DontCare

  def handle_pri_req(old_state: UInt): UInt = {
    val new_state = WireInit(old_state)
    grantack.valid := false.B
    refill_ctr := 0.U
    assert(rpq.io.enq.ready)
    req := io.req
    val old_coh   = io.req.old_meta.coh
    val needs_wb = old_coh.onCacheControl(M_FLUSH)._1 // does the line we are evicting need to be written back
    when (io.req.tag_match) {
      val (is_hit, _, coh_on_hit) = old_coh.onAccess(io.req.cmd)
      when (is_hit) { // set dirty bit
        assert(isWrite(io.req.cmd))
        new_coh     := coh_on_hit
        new_state   := s_drain_rpq
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

  // --------------------------------------------
  // s_invalid: receive requests
  when (state === s_invalid) {
    io.req_pri_rdy := true.B

    when (io.req_pri_val && io.req_pri_rdy) {
      state := handle_pri_req(state)
    }
  } 
  
  // --------------------------------------------
  // write back
  when (state === s_wb_req) {
    io.wb_req.valid          := true.B

    io.wb_req.bits.tag       := req.old_meta.tag
    io.wb_req.bits.idx       := req_idx
    io.wb_req.bits.param     := shrink_param
    io.wb_req.bits.way_en    := req.way_en
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
      io.refill.bits.way_en   := req.way_en
      io.refill.bits.wmask    := ~(0.U(rowWords.W))
      io.refill.bits.data     := io.mem_grant.bits.data
    } .otherwise {
      io.mem_grant.ready      := true.B
    }

    when (refill_done) {
      grantack.valid := TLUtilities.isRequest(io.mem_grant.bits)
      grantack.bits := TLMasterUtilities.GrantAck(io.mem_grant.bits)
      state := s_mem_finish
      new_coh := coh_on_grant
    }
  }
  
  when (state === s_mem_finish) {
    io.mem_finish.valid := grantack.valid
    io.mem_finish.bits  := grantack.bits

    when (io.mem_finish.fire()) {
      grantack.valid := false.B
      state := s_drain_rpq
    }
  }

  // --------------------------------------------
  // meta write
  when (state === s_meta_write_req) {
    io.meta_write.valid         := true.B
    io.meta_write.bits.idx      := req_idx
    io.meta_write.bits.data.coh := new_coh
    io.meta_write.bits.data.tag := req_tag
    io.meta_write.bits.way_en   := req.way_en

    when (io.meta_write.fire()) {
      state := s_invalid
    }
  }

  // --------------------------------------------
  // replay
  when (state === s_drain_rpq) {
    io.replay <> rpq.io.deq
    io.replay.bits.way_en    := req.way_en
    io.replay.bits.addr := Cat(req_tag, req_idx, rpq.io.deq.bits.addr(blockOffBits-1,0))
    when (io.replay.fire() && isWrite(rpq.io.deq.bits.cmd)) {
      // Set dirty bit
      val (is_hit, _, coh_on_hit) = new_coh.onAccess(rpq.io.deq.bits.cmd)
      assert(is_hit, "We still don't have permissions for this store")
      new_coh := coh_on_hit
    }
    when (rpq.io.count === 0.U) {
      state := s_meta_write_req
    }
  }
}


class MSHRFile extends DCacheModule
{
  val io = IO(new Bundle {
    val req  = Flipped(Vec(memWidth, Decoupled(new DCacheReqInternal))) // Req from s2 of DCache pipe
    val block_hit = Output(Vec(memWidth, Bool()))

    val mem_acquire  = Decoupled(new TLBundleA(cfg.busParams))
    val mem_grant    = Flipped(Decoupled(new TLBundleD(cfg.busParams)))
    val mem_finish   = Decoupled(new TLBundleE(cfg.busParams))

    val refill     = Decoupled(new L1DataWriteReq)
    val meta_write = Decoupled(new L1MetaWriteReq)
    val replay     = Decoupled(new DCacheReqInternal)
    val wb_req     = Decoupled(new WritebackReq)
    val wb_resp   = Input(Bool())
  })

  val req_idx = OHToUInt(io.req.map(_.valid))
  val req     = io.req(req_idx)

  for (w <- 0 until memWidth)
    io.req(w).ready := false.B

  val cacheable = true.B

  // --------------------
  // The MSHR SDQ
  val sdq_val      = RegInit(0.U(cfg.nSDQ.W))
  val sdq_alloc_id = PriorityEncoder(~sdq_val(cfg.nSDQ-1,0))
  val sdq_rdy      = !sdq_val.andR

  val sdq_enq      = req.fire() && cacheable && isWrite(req.bits.cmd)
  val sdq          = Mem(cfg.nSDQ, UInt(wordBits.W))

  when (sdq_enq) {
    sdq(sdq_alloc_id) := req.bits.data
  }

  // --------------------
  // The LineBuffer Data
  def widthMap[T <: Data](f: Int => T) = VecInit((0 until memWidth).map(f))

  val idx_matches = Wire(Vec(memWidth, Vec(cfg.nMSHRs, Bool())))
  val tag_matches = Wire(Vec(memWidth, Vec(cfg.nMSHRs, Bool())))
  val way_matches = Wire(Vec(memWidth, Vec(cfg.nMSHRs, Bool())))

  val tag_match   = widthMap(w => Mux1H(idx_matches(w), tag_matches(w)))
  val idx_match   = widthMap(w => idx_matches(w).reduce(_||_))
  val way_match   = widthMap(w => Mux1H(idx_matches(w), way_matches(w)))

  val wb_tag_list = Wire(Vec(cfg.nMSHRs, UInt(tagBits.W)))

  val meta_write_arb = Module(new Arbiter(new L1MetaWriteReq,    cfg.nMSHRs))
  val wb_req_arb     = Module(new Arbiter(new WritebackReq,      cfg.nMSHRs))
  val replay_arb     = Module(new Arbiter(new DCacheReqInternal, cfg.nMSHRs))
  val refill_arb     = Module(new Arbiter(new L1DataWriteReq,    cfg.nMSHRs))

  io.mem_grant.ready := false.B

  val mshr_alloc_idx = Wire(UInt())
  val pri_rdy = WireInit(false.B)
  val pri_val = req.valid && sdq_rdy && cacheable && !idx_match(req_idx)
  val mshrs = (0 until cfg.nMSHRs) map { i =>
    val mshr = Module(new MSHR)
    mshr.io.id := i.U(log2Up(cfg.nMSHRs).W)

    for (w <- 0 until memWidth) {
      idx_matches(w)(i) := mshr.io.idx.valid && mshr.io.idx.bits === io.req(w).bits.addr(untagBits-1,blockOffBits)
      tag_matches(w)(i) := mshr.io.tag.valid && mshr.io.tag.bits === io.req(w).bits.addr >> untagBits
      way_matches(w)(i) := mshr.io.way.valid && mshr.io.way.bits === io.req(w).bits.way_en
    }
    wb_tag_list(i) := mshr.io.wb_req.bits.tag

    mshr.io.req_pri_val  := (i.U === mshr_alloc_idx) && pri_val
    when (i.U === mshr_alloc_idx) {
      pri_rdy := mshr.io.req_pri_rdy
    }

    mshr.io.req          := req.bits
    mshr.io.req.sdq_id   := sdq_alloc_id

    mshr.io.wb_resp      := io.wb_resp

    meta_write_arb.io.in(i) <> mshr.io.meta_write
    wb_req_arb.io.in(i)     <> mshr.io.wb_req
    replay_arb.io.in(i)     <> mshr.io.replay
    refill_arb.io.in(i)     <> mshr.io.refill

    mshr.io.mem_grant.valid := false.B
    mshr.io.mem_grant.bits  := DontCare
    when (io.mem_grant.bits.source === i.U) {
      mshr.io.mem_grant <> io.mem_grant
    }

    mshr
  }

  mshr_alloc_idx    := RegNext(PriorityEncoder(mshrs.map(m=>m.io.req_pri_rdy)))

  io.meta_write <> meta_write_arb.io.out
  io.wb_req     <> wb_req_arb.io.out

  TLArbiter.lowestFromSeq(io.mem_acquire, mshrs.map(_.io.mem_acquire))
  TLArbiter.lowestFromSeq(io.mem_finish,  mshrs.map(_.io.mem_finish))

  val mmio_rdy = true.B

  for (w <- 0 until memWidth) {
    io.req(w).ready      := (w.U === req_idx) &&
      Mux(!cacheable, mmio_rdy, sdq_rdy && pri_rdy)
    io.block_hit(w)      := idx_match(w)
  }
  io.refill         <> refill_arb.io.out

  val free_sdq = io.replay.fire() && isWrite(io.replay.bits.cmd)

  io.replay <> replay_arb.io.out
  io.replay.bits.data := sdq(replay_arb.io.out.bits.sdq_id)

  when (io.replay.valid || sdq_enq) {
    sdq_val := sdq_val & ~(UIntToOH(replay_arb.io.out.bits.sdq_id) & Fill(cfg.nSDQ, free_sdq)) |
      PriorityEncoderOH(~sdq_val(cfg.nSDQ-1,0)) & Fill(cfg.nSDQ, sdq_enq)
  }

  // print all input/output requests for debug purpose

  // print req
  XSDebug(req.fire(), "req cmd: %x addr: %x data: %x mask: %x meta: %x tag_match: %b old_coh: %d old_tag: %x way_en: %x\n",
    req.bits.cmd, req.bits.addr, req.bits.data, req.bits.mask, req.bits.meta,
    req.bits.tag_match, req.bits.old_meta.coh.state, req.bits.old_meta.tag, req.bits.way_en)

  // block hit
  (0 until memWidth) map { w =>
    XSDebug(io.req(w).valid && io.block_hit(w), "channel %d req block hit\n", w.U)
  }

  // print refill
  XSDebug(io.refill.fire(), "refill addr %x data: %x wmask: %x way_en: %x\n",
    io.refill.bits.addr, io.refill.bits.data,
    io.refill.bits.wmask, io.refill.bits.way_en)

  // print meta_write
  XSDebug(io.meta_write.fire(), "meta_write idx %x way_en: %x old_tag: %x new_coh: %d new_tag: %x\n",
    io.meta_write.bits.idx, io.meta_write.bits.way_en,
    io.meta_write.bits.data.coh.state, io.meta_write.bits.data.tag,
    io.meta_write.bits.tag)

  // print replay
  XSDebug(io.replay.fire(), "replay cmd: %x addr: %x data: %x mask: %x meta: %x tag_match: %b old_coh: %d old_tag: %x way_en: %x\n",
    io.replay.bits.cmd, io.replay.bits.addr, io.replay.bits.data, io.replay.bits.mask, io.replay.bits.meta,
    io.replay.bits.tag_match, io.replay.bits.old_meta.coh.state, io.replay.bits.old_meta.tag, io.replay.bits.way_en)

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
