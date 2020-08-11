package xiangshan.cache

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils.XSDebug
import bus.tilelink._

class WritebackReq extends DCacheBundle {
  val tag = Bits(tagBits.W)
  val idx = Bits(idxBits.W)
  // TODO: make it configurable
  val source = UInt(cfg.busParams.sourceBits.W)
  val param = UInt(TLPermissions.cWidth.W) 
  val way_en = Bits(nWays.W)
  val voluntary = Bool()
}

class WritebackUnit extends DCacheModule {
  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new WritebackReq()))
    val resp = Output(Bool())
    val data_req = Decoupled(new L1DataReadReq)
    val data_resp = Input(Vec(nWays, Bits(encRowBits.W)))
    val release = Decoupled(new TLBundleC(cfg.busParams))
    val mem_grant = Input(Bool())
  })

  val req = Reg(new WritebackReq())
  val s_invalid :: s_data_read_req :: s_data_read_resp_1 :: s_data_read_resp_2 :: s_active :: s_grant :: Nil = Enum(6)
  val state = RegInit(s_invalid)

  val data_req_cnt = RegInit(0.U(log2Up(refillCycles+1).W))

  val (_, last_beat, all_beats_done, beat_count) = TLUtilities.count(io.release)

  val wb_buffer = Reg(Vec(refillCycles, UInt(rowBits.W)))
  val acked = RegInit(false.B)

  // assign default value to signals
  io.req.ready       := false.B
  io.resp            := false.B

  io.data_req.valid  := false.B
  io.data_req.bits   := DontCare

  io.release.valid   := false.B
  io.release.bits    := DontCare

  when (state === s_invalid) {
    io.req.ready := true.B
    when (io.req.fire()) {
      state := s_data_read_req
      data_req_cnt := 0.U
      req := io.req.bits
      acked := false.B
    }
  }

  when (state === s_data_read_req) {
    // Data read for new requests
    io.data_req.valid       := true.B
    io.data_req.bits.addr   := req.idx << blockOffBits
    io.data_req.bits.way_en := req.way_en
    io.data_req.bits.rmask  := ~0.U(refillCycles.W)

    when (io.data_req.fire()) {
      state := s_data_read_resp_1
    }
  }

  when (state === s_data_read_resp_1) {
    state := s_data_read_resp_2
  }

  when (state === s_data_read_resp_2) {
    val way_idx = OHToUInt(req.way_en)
    for (i <- 0 until refillCycles) {
      wb_buffer(i) := io.data_resp(way_idx)(i)
    }

    state := s_active
  }

  // release
  val r_address = Cat(req.tag, req.idx) << blockOffBits
  val id = cfg.nMissEntries
  val probeResponse = TLMasterUtilities.ProbeAck(
                          params = cfg.busParams,
                          fromSource = id.U,
                          toAddress = r_address,
                          lgSize = log2Ceil(cfg.blockBytes).U,
                          reportPermissions = req.param,
                          data = wb_buffer(data_req_cnt))

  val voluntaryRelease = TLMasterUtilities.Release(
                          params = cfg.busParams,
                          fromSource = id.U,
                          toAddress = r_address,
                          lgSize = log2Ceil(cfg.blockBytes).U,
                          shrinkPermissions = req.param,
                          data = wb_buffer(data_req_cnt))._2

  when (state === s_active) {
    io.release.valid := data_req_cnt < refillCycles.U
    io.release.bits  := Mux(req.voluntary, voluntaryRelease, probeResponse)

    when (io.mem_grant) {
      acked := true.B
    }

    when (io.release.fire()) {
      data_req_cnt := data_req_cnt + 1.U

      when (data_req_cnt === (refillCycles-1).U) {
        state := Mux(req.voluntary, s_grant, s_invalid)
      }
    }
  }
  
  when (state === s_grant) {
    when (io.mem_grant) {
      acked := true.B
    }
    when (acked) {
      state := s_invalid
    }
  }

  // print all input/output requests for debug purpose
  // print req
  val io_req = io.req.bits
  XSDebug(io.req.fire(), "req tag: %x idx: %x source: %d param: %x way_en: %x voluntary: %b\n",
    io_req.tag, io_req.idx, io_req.source, io_req.param, io_req.way_en, io_req.voluntary)

  // print data req
  val io_data_req = io.data_req.bits
  XSDebug(io.data_req.fire(), "data_req addr: %x way_en: %x\n", io_data_req.addr, io_data_req.way_en)

  // print release
  // XSDebug.exec(io.release.fire(), io.release.bits.dump)

  // print mem_grant
  XSDebug(io.mem_grant, "mem_grant\n")
}
