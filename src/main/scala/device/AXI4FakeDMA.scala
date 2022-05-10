/***************************************************************************************
  * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
  * Copyright (c) 2020-2021 Peng Cheng Laboratory
  *
  * XiangShan is licensed under Mulan PSL v2.
  * You can use this software according to the terms and conditions of the Mulan PSL v2.
  * You may obtain a copy of Mulan PSL v2 at:
  *          http://license.coscl.org.cn/MulanPSL2
  *
  * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
  * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
  * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
  *
  * See the Mulan PSL v2 for more details.
  ***************************************************************************************/

package device

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import difftest.DifftestDMATransaction
import freechips.rocketchip.amba.axi4.{AXI4MasterNode, AXI4MasterPortParameters, AXI4Parameters}
import freechips.rocketchip.diplomacy.AddressSet

class DMAFakeMSHR(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val enable = Input(Bool())
    val slave = new Bundle {
      val wen   = Input(Bool())
      val addr  = Input(UInt(4.W))
      val rdata = Output(UInt(64.W))
      val wdata = Input(UInt(64.W))
    }
    val master = new Bundle {
      val req = new Bundle {
        val valid = Output(Bool())
        val ready = Input(Bool())
        val is_write = Output(Bool())
        val addr = Output(UInt(64.W))
        val mask = Output(UInt(64.W))
        val data = Output(UInt(512.W))
      }
      val resp = Flipped(ValidIO(UInt(256.W)))
    }
  })

  val state   = Reg(UInt(8.W))
  val address = Reg(UInt(64.W))
  val mask    = Reg(UInt(64.W))
  val data    = Reg(Vec(8, UInt(64.W)))

  val s_idle :: s_read :: s_write :: s_wait_resp_b :: s_wait_resp_r0 :: s_wait_resp_r1 :: Nil = Enum(6)
  when (state === s_read) {
    when (io.master.req.valid && io.master.req.ready) {
      state := s_wait_resp_r0
    }
  }.elsewhen (state === s_write) {
    when (io.master.req.valid && io.master.req.ready) {
      state := s_wait_resp_b
    }
  }.elsewhen (state === s_wait_resp_b) {
    when (io.master.resp.valid) {
      state := s_idle
    }
  }.elsewhen (state === s_wait_resp_r0) {
    when (io.master.resp.valid) {
      state := s_wait_resp_r1
    }
  }.elsewhen (state === s_wait_resp_r1) {
    when (io.master.resp.valid) {
      state := s_idle
    }
  }

  when (io.slave.wen) {
    when (io.slave.addr === 8.U) {
      state := io.slave.wdata
    }.elsewhen(io.slave.addr === 9.U) {
      address := io.slave.wdata
    }.elsewhen(io.slave.addr === 10.U) {
      mask := io.slave.wdata
    }.otherwise {
      data(io.slave.addr) := io.slave.wdata
    }
  }
  io.slave.rdata := Mux(io.slave.addr === 8.U, state,
    Mux(io.slave.addr === 9.U, address,
      Mux(io.slave.addr === 10.U, mask, data(io.slave.addr))))

  io.master.req.valid := io.enable && (state === s_read || state === s_write)
  io.master.req.is_write := state === s_write
  io.master.req.addr := address
  io.master.req.mask := mask
  io.master.req.data := data.asUInt

  when (io.master.resp.valid) {
    when (state === s_wait_resp_r0) {
      for (i <- 0 until 4) {
        data(i) := io.master.resp.bits(64 * i + 63, 64 * i)
      }
    }.elsewhen(state === s_wait_resp_r1) {
      for (i <- 0 until 4) {
        data(i + 4) := io.master.resp.bits(64 * i + 63, 64 * i)
      }
    }
  }

  val last_state = RegNext(state, init=s_idle)
  val read_valid = last_state === s_wait_resp_r1 && state === s_idle
  val write_valid = last_state === s_wait_resp_b && state === s_idle
  val difftest = Module(new DifftestDMATransaction)
  difftest.io.clock    := clock
  difftest.io.coreid   := 0.U
  difftest.io.valid    := read_valid || write_valid
  difftest.io.is_write := last_state === s_wait_resp_b
  difftest.io.address  := address
  difftest.io.mask     := mask
  difftest.io.data     := data

  def slave_read(addr: UInt): UInt = {
    io.slave.wen := false.B
    io.slave.addr := addr
    io.slave.rdata
  }
  def slave_write(addr: UInt, data: UInt): Unit = {
    io.slave.wen := true.B
    io.slave.addr := addr
    io.slave.wdata := data
  }
  def has_read_req: Bool = io.master.req.valid && !io.master.req.is_write
  def has_write_req: Bool = io.master.req.valid && io.master.req.is_write
}

class AXI4FakeDMA
(
  address: Seq[AddressSet],
  params: AXI4MasterPortParameters
)(implicit p: Parameters)
  extends AXI4SlaveModule(address, executable = true)
{
  val dma_node = AXI4MasterNode(Seq(params))

  override lazy val module = new AXI4SlaveModuleImp(this) {
    val numInflight = 64
    require(isPow2(numInflight))

    // 0x0 - (0x80 * numInflight)
    val mshr = Seq.fill(numInflight)(Module(new DMAFakeMSHR))
    val enable_addr_bit = log2Ceil(numInflight) + 4 + 3
    val enable = RegInit(0.U(numInflight.W))
    mshr.zip(enable.asBools).foreach(x => x._1.io.enable := x._2)

    // DMACtrl slave READ
    val reqReadOffset = raddr(6, 3)
    val reqReadIdx = raddr(7 + log2Ceil(numInflight + 1) - 1, 7)
    val req_r = VecInit(mshr.map(_.slave_read(reqReadOffset)))
    in.r.bits.data := Mux(raddr(enable_addr_bit), enable, req_r(reqReadIdx))

    // DMACtrl slave WRITE
    val reqWriteOffset = waddr(6, 3)
    val reqWriteIdx = waddr(7 + log2Ceil(numInflight + 1) - 1, 7)
    for ((req, i) <- mshr.zipWithIndex) {
      req.io.slave.wdata := DontCare
      when (in.w.fire && reqWriteIdx === i.U && !waddr(enable_addr_bit)) {
        req.slave_write(reqWriteOffset, in.w.bits.data)
      }
    }
    when (in.w.fire && waddr(enable_addr_bit)) {
      enable := in.w.bits.data
    }

    // DMA master
    val (out, dma_edge) = dma_node.out.head
    val dmaReqBytes = 64
    val dmaBeatBytes = dma_edge.slave.beatBytes
    val numBeats = dmaReqBytes / dmaBeatBytes
    val axi_len = numBeats - 1
    def selectByBeatIndex(i: UInt, data: UInt): UInt = {
      data.asTypeOf(Vec(numBeats, UInt((data.getWidth / numBeats).W)))(i)
    }

    // DMA master READ Request
    val has_read_req = VecInit(mshr.map(_.has_read_req))
    val out_read_index = PriorityEncoderOH(has_read_req)
    val out_read_req = Mux1H(out_read_index, mshr.map(_.io.master.req))
    out.ar.valid := has_read_req.asUInt.orR
    out.ar.bits := 0.U.asTypeOf(out.ar.bits.cloneType)
    out.ar.bits.id := OHToUInt(out_read_index)
    out.ar.bits.addr := out_read_req.addr
    out.ar.bits.len := axi_len.U
    out.ar.bits.size := log2Ceil(dmaBeatBytes).U
    out.ar.bits.burst := AXI4Parameters.BURST_INCR

    // DMA master WRIET Request
    val has_write_req = VecInit(mshr.map(_.has_write_req))
    val out_write_index = PriorityEncoderOH(has_write_req)
    val out_write_req = Mux1H(out_write_index, mshr.map(_.io.master.req))
    out.aw.valid := has_write_req.asUInt.orR
    out.aw.bits := 0.U.asTypeOf(out.aw.bits.cloneType)
    out.aw.bits.id := OHToUInt(out_write_index)
    out.aw.bits.addr := out_write_req.addr
    out.aw.bits.len := axi_len.U
    out.aw.bits.size := log2Ceil(dmaBeatBytes).U
    out.aw.bits.burst := AXI4Parameters.BURST_INCR

    // DMA master READ/WRITE handshake
    for ((req_ready, i) <- mshr.map(_.io.master.req.ready).zipWithIndex) {
      val read_fire = out.ar.fire && out_read_index(i)
      val write_fire = out.aw.fire && out_write_index(i)
      req_ready := read_fire || write_fire
    }

    // DMA master WRITE DATA
    val w_valid = RegInit(false.B)
    when (out.aw.fire) {
      w_valid := true.B
    }.elsewhen(out.w.fire && out.w.bits.last) {
      w_valid := false.B
    }
    // Only one inflight aw: disable aw.valid when !w.bits.last
    when (w_valid) {
      out.aw.valid := false.B
    }
    val beatCount = RegInit(0.U(log2Ceil(numBeats).W))
    val w_mask = RegEnable(out_write_req.mask, out.aw.fire)
    val w_data = RegEnable(out_write_req.data, out.aw.fire)
    out.w.valid := w_valid
    out.w.bits := DontCare
    out.w.bits.data := selectByBeatIndex(beatCount, w_data)
    out.w.bits.strb := selectByBeatIndex(beatCount, w_mask)
    out.w.bits.last := beatCount === (numBeats - 1).U
    when (out.w.fire) {
      beatCount := beatCount + 1.U
    }

    // DMA master READ/WRITE Response
    out.r.ready := true.B
    out.b.ready := true.B
    for ((resp, i) <- mshr.map(_.io.master.resp).zipWithIndex) {
      val read_resp_fire = out.r.fire && out.r.bits.id === i.U
      val write_resp_fire = out.b.fire && out.b.bits.id === i.U
      resp.valid := read_resp_fire || write_resp_fire
      resp.bits := out.r.bits.data
    }

  }
}
