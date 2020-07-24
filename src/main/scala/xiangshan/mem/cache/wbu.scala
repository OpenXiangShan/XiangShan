package xiangshan.mem.cache

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import bus.tilelink._

class WritebackReq extends DCacheBundle {
  val tag = Bits(tagBits.W)
  val idx = Bits(idxBits.W)
  // TODO: make it configurable
  // 问题：这个source就是mshr id吗？那假如是响应probe的请求，那又如何处理呢？
  val source = UInt(cfg.busParams.sourceBits.W)
  val param = UInt(TLPermissions.cWidth.W) 
  val way_en = Bits(nWays.W)
  // 如果是WBU下来的应该是voluntary的吧？
  val voluntary = Bool()
}

class WritebackUnit extends DCacheModule {
  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new WritebackReq()))
    // 这个是啥？
    val resp = Output(Bool())
    // 这个是干啥用的啊？
    // 暂时先简单起见，把无关的都去掉啊！
    val data_req = Decoupled(new L1DataReadReq)
    val data_resp = Input(UInt(encRowBits.W))
    val release = Decoupled(new TLBundleC(cfg.busParams))
    val mem_grant = Flipped(Decoupled(new TLBundleD(cfg.busParams)))
  })

  // 同时处理的request只能有一个
  val req = Reg(new WritebackReq())
  val s_invalid :: s_fill_buffer :: s_active :: s_grant :: Nil = Enum(4)
  val state = RegInit(s_invalid)
  // 这俩都是啥？
  // r1、r2都是啥？
  // 这边之所以要处理成r1，r2是因为数据变成读了之后，要等两拍才出来，所以才必须得搞这种幺蛾子啊。
  // 那么现在的问题是，为啥数据必须得等两拍才出来呢？why？
  // 似乎是因为bank冲突的逻辑太复杂了？
  val r1_data_req_fired = RegInit(false.B)
  val r2_data_req_fired = RegInit(false.B)
  val r1_data_req_cnt = Reg(UInt(log2Up(refillCycles+1).W))
  val r2_data_req_cnt = Reg(UInt(log2Up(refillCycles+1).W))
  val data_req_cnt = RegInit(0.U(log2Up(refillCycles+1).W))
  val (_, last_beat, all_beats_done, beat_count) = TLUtilities.count(io.release)
  // 这边怎么还搞了个wb buffer呢？
  // 使用一个buffer，而不是直接挂到总线请求行，可以让同步方便一点
  // 毕竟dcache出口是没有ready，valid的
  // 假如因为总线没有就绪，就不停地replay，反而太复杂了，所以还是先写到buffer里面好啊。
  val wb_buffer = Reg(Vec(refillCycles, UInt(encRowBits.W)))
  val acked = RegInit(false.B)

  io.release.valid   := false.B
  io.release.bits    := DontCare
  io.req.ready       := false.B
  io.data_req.valid  := false.B
  io.data_req.bits   := DontCare
  io.resp            := false.B

  val r_address = Cat(req.tag, req.idx) << blockOffBits
  val id = cfg.nMSHRs
  // 这边还要响应probe？
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
  
  // 所以根据这个时序安排的话，就是：
  // data_req_cnt: 读请求发出
  // r1_data_req_cnt: 读请求发出后的下一个周期变成valid
  // r2_data_req_cnt: 读请求发出后的下下个周期变成valid，此时开始出数据
  // 我甚至怀疑这里写的meta只是单纯为了定序？
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
      // 当fire的时候，r1就变成true了
      r1_data_req_fired := true.B
      r1_data_req_cnt   := data_req_cnt
      data_req_cnt := data_req_cnt + 1.U
    }
    when (r2_data_req_fired) {
      wb_buffer(r2_data_req_cnt) := io.data_resp
      when (r2_data_req_cnt === (refillCycles-1).U) {
        // 为啥当数据全部读上来时，就开始resp为true了呢？why？
        io.resp := true.B
        state := s_active
        data_req_cnt := 0.U
      }
    }
  } .elsewhen (state === s_active) {
    io.release.valid := data_req_cnt < refillCycles.U
    // 这两个应该就只是一些域不一样吧？
    io.release.bits := Mux(req.voluntary, voluntaryRelease, probeResponse)

    // 问题：为啥会在这里出现一个这个呢？why？
    when (io.mem_grant.fire()) {
      acked := true.B
    }
    when (io.release.fire()) {
      data_req_cnt := data_req_cnt + 1.U
    }
    when ((data_req_cnt === (refillCycles-1).U) && io.release.fire()) {
      // 似乎是voluntary的时候，就需要搞个等待master的grant，假如不是voluntary的时候，就不需要等待，就直接OK了？
      state := Mux(req.voluntary, s_grant, s_invalid)
    }
  } .elsewhen (state === s_grant) {
    when (io.mem_grant.fire()) {
      acked := true.B
    }
    when (acked) {
      state := s_invalid
    }
  }
  io.mem_grant.ready := true.B
  io.mem_grant.bits := DontCare
}
