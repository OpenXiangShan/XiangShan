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

package xiangshan.frontend.icache

import chisel3._
import chisel3.util._
import utils._
import utility._
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp, TransferSizes}
import freechips.rocketchip.tilelink.{TLArbiter, TLBundleA, TLBundleD, TLClientNode, TLEdgeOut, TLMasterParameters, TLMasterPortParameters}
import xiangshan._
import xiangshan.frontend._

class InsUncacheReq(implicit p: Parameters) extends ICacheBundle
{
    val addr = UInt(PAddrBits.W)
}

class InsUncacheResp(implicit p: Parameters) extends ICacheBundle
{
  val data = UInt(maxInstrLen.W)
}

// One miss entry deals with one mmio request
class InstrMMIOEntry(edge: TLEdgeOut)(implicit p: Parameters) extends XSModule with HasICacheParameters with HasIFUConst
{
  val io = IO(new Bundle {
    val id = Input(UInt(log2Up(cacheParams.nMMIOs).W))
    // client requests
    val req = Flipped(DecoupledIO(new InsUncacheReq))
    val resp = DecoupledIO(new InsUncacheResp)

    val mmio_acquire = DecoupledIO(new TLBundleA(edge.bundle))
    val mmio_grant   = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))

    val flush = Input(Bool())
  })


  val s_invalid :: s_refill_req :: s_refill_resp :: s_send_resp :: Nil = Enum(4)

  val state = RegInit(s_invalid)

  val req       = Reg(new InsUncacheReq )
  val respDataReg = Reg(UInt(mmioBusWidth.W))

  // assign default values to output signals
  io.req.ready           := false.B
  io.resp.valid          := false.B
  io.resp.bits           := DontCare

  io.mmio_acquire.valid   := false.B
  io.mmio_acquire.bits    := DontCare

  io.mmio_grant.ready     := false.B

  val needFlush = RegInit(false.B)

  when(io.flush && (state =/= s_invalid) && (state =/= s_send_resp)){ needFlush := true.B }
  .elsewhen((state=== s_send_resp) && needFlush){ needFlush := false.B }

  // --------------------------------------------
  // s_invalid: receive requests
  when (state === s_invalid) {
    io.req.ready := true.B

    when (io.req.fire) {
      req   := io.req.bits
      state := s_refill_req
    }
  }


  when (state === s_refill_req) {
    val address_aligned = req.addr(req.addr.getWidth - 1, log2Ceil(mmioBusBytes))
    io.mmio_acquire.valid := true.B
    io.mmio_acquire.bits  := edge.Get(
          fromSource      = io.id,
          toAddress       = Cat(address_aligned, 0.U(log2Ceil(mmioBusBytes).W)),
          lgSize          = log2Ceil(mmioBusBytes).U
        )._2

    when (io.mmio_acquire.fire) {
      state := s_refill_resp
    }
  }

  val (_, _, refill_done, _) = edge.addr_inc(io.mmio_grant)

  when (state === s_refill_resp) {
    io.mmio_grant.ready := true.B

    when (io.mmio_grant.fire) {
      respDataReg := io.mmio_grant.bits.data
      state := s_send_resp
    }
  }

  def getDataFromBus(pc: UInt) = {
    val respData = Wire(UInt(maxInstrLen.W))
    respData := Mux(pc(2,1) === "b00".U, respDataReg(31,0),
        Mux(pc(2,1) === "b01".U, respDataReg(47,16),
          Mux(pc(2,1) === "b10".U, respDataReg(63,32),
            Cat(0.U, respDataReg(63,48))
          )
        )
      )
    respData
  }

  when (state === s_send_resp) {
    io.resp.valid     := !needFlush
    io.resp.bits.data :=  getDataFromBus(req.addr)
    // meta data should go with the response
    when (io.resp.fire || needFlush) {
      state := s_invalid
    }
  }
}

class InstrUncacheIO(implicit p: Parameters) extends ICacheBundle {
    val req = Flipped(DecoupledIO(new InsUncacheReq ))
    val resp = DecoupledIO(new InsUncacheResp)
    val flush = Input(Bool())
}

class InstrUncache()(implicit p: Parameters) extends LazyModule with HasICacheParameters {
  override def shouldBeInlined: Boolean = false

  val clientParameters = TLMasterPortParameters.v1(
    clients = Seq(TLMasterParameters.v1(
      "InstrUncache",
      sourceId = IdRange(0, cacheParams.nMMIOs)
    ))
  )
  val clientNode = TLClientNode(Seq(clientParameters))

  lazy val module = new InstrUncacheImp(this)

}

class InstrUncacheImp(outer: InstrUncache)
  extends LazyModuleImp(outer)
    with HasICacheParameters
    with HasTLDump
{
  val io = IO(new InstrUncacheIO)

  val (bus, edge) = outer.clientNode.out.head

  val resp_arb = Module(new Arbiter(new InsUncacheResp, cacheParams.nMMIOs))

  val req  = io.req
  val resp = io.resp
  val mmio_acquire = bus.a
  val mmio_grant   = bus.d

  val entry_alloc_idx = Wire(UInt())
  val req_ready = WireInit(false.B)

  // assign default values to output signals
  bus.b.ready := false.B
  bus.c.valid := false.B
  bus.c.bits  := DontCare
  bus.d.ready := false.B
  bus.e.valid := false.B
  bus.e.bits  := DontCare

  val entries = (0 until cacheParams.nMMIOs) map { i =>
    val entry = Module(new InstrMMIOEntry(edge))

    entry.io.id := i.U(log2Up(cacheParams.nMMIOs).W)
    entry.io.flush := io.flush

    // entry req
    entry.io.req.valid := (i.U === entry_alloc_idx) && req.valid
    entry.io.req.bits  := req.bits
    when (i.U === entry_alloc_idx) {
      req_ready := entry.io.req.ready
    }

    // entry resp
    resp_arb.io.in(i) <> entry.io.resp

    entry.io.mmio_grant.valid := false.B
    entry.io.mmio_grant.bits  := DontCare
    when (mmio_grant.bits.source === i.U) {
      entry.io.mmio_grant <> mmio_grant
    }
    entry
  }

  entry_alloc_idx    := PriorityEncoder(entries.map(m=>m.io.req.ready))

  req.ready  := req_ready
  resp          <> resp_arb.io.out
  TLArbiter.lowestFromSeq(edge, mmio_acquire, entries.map(_.io.mmio_acquire))

}
