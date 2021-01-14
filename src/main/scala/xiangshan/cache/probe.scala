package xiangshan.cache

import chisel3._
import chisel3.util._

import utils.XSDebug
import freechips.rocketchip.tilelink._
import utils.{HasTLDump, XSDebug}

class ProbeUnit(edge: TLEdgeOut) extends DCacheModule with HasTLDump {
  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new TLBundleB(edge.bundle)))
    val rep = Decoupled(new TLBundleC(edge.bundle))
    val meta_read = Decoupled(new L1MetaReadReq)
    val meta_resp   = Input(Vec(nWays, new L1Metadata))
    val meta_write = Decoupled(new L1MetaWriteReq)
    val wb_req = Decoupled(new WritebackReq(edge.bundle.sourceBits))
    val wb_resp = Input(Bool())
    val inflight_req_idx        = Output(Valid(UInt()))
    val inflight_req_block_addr = Output(Valid(UInt()))
  })

  val s_invalid :: s_meta_read_req :: s_meta_read_resp :: s_decide_next_state :: s_release :: s_wb_req :: s_wb_resp :: s_meta_write_req :: Nil = Enum(8)

  val state = RegInit(s_invalid)

  val req = Reg(new TLBundleB(edge.bundle))
  val req_idx = get_idx(req.address)
  val req_tag = get_tag(req.address)
  val req_block_addr = get_block_addr(req.address)

  val req_way_en = Reg(UInt())
  val tag_matches = req_way_en.orR
  val old_coh = Reg(new ClientMetadata)
  val miss_coh = ClientMetadata.onReset
  val reply_coh = Mux(tag_matches, old_coh, miss_coh)
  val (is_dirty, report_param, new_coh) = reply_coh.onProbe(req.param)

  // assign default values to signals
  io.req.ready := false.B
  io.rep.valid := false.B
  io.rep.bits  := DontCare
  io.meta_read.valid := false.B
  io.meta_read.bits  := DontCare
  io.meta_write.valid := false.B
  io.meta_write.bits  := DontCare
  io.wb_req.valid := false.B
  io.wb_req.bits  := DontCare

  io.inflight_req_idx.valid := state =/= s_invalid
  io.inflight_req_idx.bits  := req_idx

  io.inflight_req_block_addr.valid := state =/= s_invalid
  io.inflight_req_block_addr.bits  := req_block_addr

  when (state =/= s_invalid) {
    XSDebug("state: %d\n", state)
  }

  when (state === s_invalid) {
    io.req.ready := true.B
    when (io.req.fire()) {
      req := io.req.bits
      state := s_meta_read_req
    }
  }

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

  when (state === s_meta_read_resp) {
    // tag check
    def wayMap[T <: Data](f: Int => T) = VecInit((0 until nWays).map(f))
    val tag_eq_way = wayMap((w: Int) => io.meta_resp(w).tag === (req_tag)).asUInt
    val tag_match_way = wayMap((w: Int) => tag_eq_way(w) && io.meta_resp(w).coh.isValid()).asUInt
    val hit_state = Mux1H(tag_match_way, wayMap((w: Int) => io.meta_resp(w).coh))

    old_coh := hit_state
    req_way_en := tag_match_way

    state := s_decide_next_state
  }

  when (state === s_decide_next_state) {
    // decide next state
    state := Mux(tag_matches && is_dirty, s_wb_req, s_release)
  }


  // no need to write back, just release
  when (state === s_release) {
    io.rep.valid := true.B
    io.rep.bits  := edge.ProbeAck(req, report_param)

    when (io.rep.fire()) {
      state := Mux(tag_matches, s_meta_write_req, s_invalid)
    }
  }

  when (state === s_wb_req) {
    io.wb_req.valid          := true.B

    io.wb_req.bits.tag       := req_tag
    io.wb_req.bits.idx       := req_idx
    io.wb_req.bits.param     := report_param
    io.wb_req.bits.way_en    := req_way_en
    io.wb_req.bits.source    := req.source
    io.wb_req.bits.voluntary := false.B

    when (io.wb_req.fire()) {
      state := s_wb_resp
    }
  }

  when (state === s_wb_resp) {
    when (io.wb_resp) {
      state := s_meta_write_req
    }
  }

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

  // print wb_req
  XSDebug(io.wb_req.fire(), "wb_req idx %x tag: %x source: %d param: %x way_en: %x voluntary: %b\n",
    io.wb_req.bits.idx, io.wb_req.bits.tag,
    io.wb_req.bits.source, io.wb_req.bits.param,
    io.wb_req.bits.way_en, io.wb_req.bits.voluntary)

  // print tilelink messages
  when (io.req.fire()) {
    XSDebug("mem_probe ")
    io.req.bits.dump
  }
  when (io.rep.fire()) {
    XSDebug("mem_release ")
    io.rep.bits.dump
  }
}
