/***************************************************************************************
* Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
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
import freechips.rocketchip.diplomacy.IdRange
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.diplomacy.LazyModuleImp
import freechips.rocketchip.tilelink.TLArbiter
import freechips.rocketchip.tilelink.TLBundleA
import freechips.rocketchip.tilelink.TLBundleD
import freechips.rocketchip.tilelink.TLClientNode
import freechips.rocketchip.tilelink.TLEdgeOut
import freechips.rocketchip.tilelink.TLMasterParameters
import freechips.rocketchip.tilelink.TLMasterPortParameters
import org.chipsalliance.cde.config.Parameters
import utils._
import xiangshan.frontend._

class InsUncacheReq(implicit p: Parameters) extends ICacheBundle {
  val addr: UInt = UInt(PAddrBits.W)
}

class InsUncacheResp(implicit p: Parameters) extends ICacheBundle {
  val data:    UInt = UInt(maxInstrLen.W)
  val corrupt: Bool = Bool()
}

class InstrMMIOEntryIO(edge: TLEdgeOut)(implicit p: Parameters) extends ICacheBundle {
  val id: UInt = Input(UInt(log2Up(cacheParams.nMMIOs).W))
  // client requests
  val req:  DecoupledIO[InsUncacheReq]  = Flipped(DecoupledIO(new InsUncacheReq))
  val resp: DecoupledIO[InsUncacheResp] = DecoupledIO(new InsUncacheResp)

  val mmio_acquire: DecoupledIO[TLBundleA] = DecoupledIO(new TLBundleA(edge.bundle))
  val mmio_grant:   DecoupledIO[TLBundleD] = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))

  val flush: Bool = Input(Bool())
}

// One miss entry deals with one mmio request
class InstrMMIOEntry(edge: TLEdgeOut)(implicit p: Parameters) extends ICacheModule with HasIFUConst {
  val io: InstrMMIOEntryIO = IO(new InstrMMIOEntryIO(edge))

  private val s_invalid :: s_refill_req :: s_refill_resp :: s_send_resp :: Nil = Enum(4)

  private val state = RegInit(s_invalid)

  private val req            = Reg(new InsUncacheReq)
  private val respDataReg    = RegInit(0.U(mmioBusWidth.W))
  private val respCorruptReg = RegInit(false.B)

  // assign default values to output signals
  io.req.ready  := false.B
  io.resp.valid := false.B
  io.resp.bits  := DontCare

  io.mmio_acquire.valid := false.B
  io.mmio_acquire.bits  := DontCare

  io.mmio_grant.ready := false.B

  private val needFlush = RegInit(false.B)

  when(io.flush && (state =/= s_invalid) && (state =/= s_send_resp))(needFlush := true.B)
    .elsewhen((state === s_send_resp) && needFlush)(needFlush := false.B)

  // --------------------------------------------
  // s_invalid: receive requests
  when(state === s_invalid) {
    io.req.ready := true.B

    when(io.req.fire) {
      req   := io.req.bits
      state := s_refill_req
    }
  }

  when(state === s_refill_req) {
    val address_aligned = req.addr(req.addr.getWidth - 1, log2Ceil(mmioBusBytes))
    io.mmio_acquire.valid := true.B
    io.mmio_acquire.bits := edge.Get(
      fromSource = io.id,
      toAddress = Cat(address_aligned, 0.U(log2Ceil(mmioBusBytes).W)),
      lgSize = log2Ceil(mmioBusBytes).U
    )._2

    when(io.mmio_acquire.fire) {
      state := s_refill_resp
    }
  }

  val (_, _, refill_done, _) = edge.addr_inc(io.mmio_grant)

  when(state === s_refill_resp) {
    io.mmio_grant.ready := true.B

    when(io.mmio_grant.fire) {
      respDataReg    := io.mmio_grant.bits.data
      respCorruptReg := io.mmio_grant.bits.corrupt // this includes bits.denied, as tilelink spec defines
      state          := s_send_resp
    }
  }

  private def getDataFromBus(pc: UInt): UInt = {
    val respData = Wire(UInt(maxInstrLen.W))
    respData := Mux(
      pc(2, 1) === "b00".U,
      respDataReg(31, 0),
      Mux(
        pc(2, 1) === "b01".U,
        respDataReg(47, 16),
        Mux(pc(2, 1) === "b10".U, respDataReg(63, 32), Cat(0.U, respDataReg(63, 48)))
      )
    )
    respData
  }

  when(state === s_send_resp) {
    io.resp.valid        := !needFlush
    io.resp.bits.data    := getDataFromBus(req.addr)
    io.resp.bits.corrupt := respCorruptReg
    // metadata should go with the response
    when(io.resp.fire || needFlush) {
      state := s_invalid
    }
  }
}

class InstrUncacheIO(implicit p: Parameters) extends ICacheBundle {
  val req:   DecoupledIO[InsUncacheReq]  = Flipped(DecoupledIO(new InsUncacheReq))
  val resp:  DecoupledIO[InsUncacheResp] = DecoupledIO(new InsUncacheResp)
  val flush: Bool                        = Input(Bool())
}

class InstrUncache()(implicit p: Parameters) extends LazyModule with HasICacheParameters {
  override def shouldBeInlined: Boolean = false

  val clientParameters: TLMasterPortParameters = TLMasterPortParameters.v1(
    clients = Seq(TLMasterParameters.v1(
      "InstrUncache",
      sourceId = IdRange(0, cacheParams.nMMIOs)
    ))
  )
  val clientNode: TLClientNode = TLClientNode(Seq(clientParameters))

  lazy val module: InstrUncacheImp = new InstrUncacheImp(this)
}

class InstrUncacheImp(outer: InstrUncache)
    extends LazyModuleImp(outer)
    with HasICacheParameters
    with HasTLDump {
  val io: InstrUncacheIO = IO(new InstrUncacheIO)

  private val (bus, edge) = outer.clientNode.out.head

  private val resp_arb = Module(new Arbiter(new InsUncacheResp, cacheParams.nMMIOs))

  private val req          = io.req
  private val resp         = io.resp
  private val mmio_acquire = bus.a
  private val mmio_grant   = bus.d

  private val entry_alloc_idx = Wire(UInt())
  private val req_ready       = WireInit(false.B)

  // assign default values to output signals
  bus.b.ready := false.B
  bus.c.valid := false.B
  bus.c.bits  := DontCare
  bus.d.ready := false.B
  bus.e.valid := false.B
  bus.e.bits  := DontCare

  private val entries = (0 until cacheParams.nMMIOs).map { i =>
    val entry = Module(new InstrMMIOEntry(edge))

    entry.io.id    := i.U(log2Up(cacheParams.nMMIOs).W)
    entry.io.flush := io.flush

    // entry req
    entry.io.req.valid := (i.U === entry_alloc_idx) && req.valid
    entry.io.req.bits  := req.bits
    when(i.U === entry_alloc_idx) {
      req_ready := entry.io.req.ready
    }

    // entry resp
    resp_arb.io.in(i) <> entry.io.resp

    entry.io.mmio_grant.valid := false.B
    entry.io.mmio_grant.bits  := DontCare
    when(mmio_grant.bits.source === i.U) {
      entry.io.mmio_grant <> mmio_grant
    }
    entry
  }

  // override mmio_grant.ready to prevent x-propagation
  mmio_grant.ready := true.B

  entry_alloc_idx := PriorityEncoder(entries.map(m => m.io.req.ready))

  req.ready := req_ready
  resp <> resp_arb.io.out
  TLArbiter.lowestFromSeq(edge, mmio_acquire, entries.map(_.io.mmio_acquire))
}
