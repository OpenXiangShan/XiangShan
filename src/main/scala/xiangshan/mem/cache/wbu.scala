package xiangshan.mem.cache

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import xiangshan.utils.XSDebug
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
  val s_invalid :: s_fill_buffer :: s_active :: s_grant :: Nil = Enum(4)
  val state = RegInit(s_invalid)

  val r1_data_req_fired = RegInit(false.B)
  val r2_data_req_fired = RegInit(false.B)
  val r1_data_req_cnt = Reg(UInt(log2Up(refillCycles+1).W))
  val r2_data_req_cnt = Reg(UInt(log2Up(refillCycles+1).W))
  val data_req_cnt = RegInit(0.U(log2Up(refillCycles+1).W))

  val (_, last_beat, all_beats_done, beat_count) = TLUtilities.count(io.release)

  val wb_buffer = Reg(Vec(refillCycles, UInt(encRowBits.W)))
  val acked = RegInit(false.B)

  // assign default value to signals
  io.req.ready       := false.B
  io.resp            := false.B

  io.data_req.valid  := false.B
  io.data_req.bits   := DontCare

  io.release.valid   := false.B
  io.release.bits    := DontCare

  val r_address = Cat(req.tag, req.idx) << blockOffBits
  val id = cfg.nMSHRs
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


  when (state === s_invalid) {
    io.req.ready := true.B
    when (io.req.fire()) {
      state := s_fill_buffer
      data_req_cnt := 0.U
      req := io.req.bits
      acked := false.B
    }
  }
  
  when (state === s_fill_buffer) {
    io.data_req.valid := data_req_cnt < refillCycles.U
    io.data_req.bits.way_en := req.way_en
    io.data_req.bits.addr := (if(refillCycles > 1)
                              Cat(req.idx, data_req_cnt(log2Up(refillCycles)-1,0))
                            else req.idx) << rowOffBits

    r1_data_req_fired := false.B
    r1_data_req_cnt   := 0.U
    r2_data_req_fired := r1_data_req_fired
    r2_data_req_cnt   := r1_data_req_cnt
    when (io.data_req.fire()) {
      r1_data_req_fired := true.B
      r1_data_req_cnt   := data_req_cnt
      data_req_cnt := data_req_cnt + 1.U
    }
    when (r2_data_req_fired) {
      val data = Mux1H(req.way_en, io.data_resp)
      wb_buffer(r2_data_req_cnt) := data
      when (r2_data_req_cnt === (refillCycles-1).U) {
        io.resp := true.B
        state := s_active
        data_req_cnt := 0.U
      }
      // print data resp
      XSDebug(s"data_resp cnt: %d data: %x\n", r2_data_req_cnt, data)
    }
  } .elsewhen (state === s_active) {
    io.release.valid := data_req_cnt < refillCycles.U
    io.release.bits := Mux(req.voluntary, voluntaryRelease, probeResponse)

    when (io.mem_grant) {
      acked := true.B
    }
    when (io.release.fire()) {
      data_req_cnt := data_req_cnt + 1.U
    }
    when ((data_req_cnt === (refillCycles-1).U) && io.release.fire()) {
      state := Mux(req.voluntary, s_grant, s_invalid)
    }
  } .elsewhen (state === s_grant) {
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
  when (XSDebug.trigger && io.release.fire()) {
    XSDebug.printPrefix
    io.release.bits.dump
  }

  // print mem_grant
  XSDebug(io.mem_grant, "mem_grant\n")
}
