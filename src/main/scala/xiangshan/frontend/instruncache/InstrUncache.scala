// Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
// Copyright (c) 2020-2021 Peng Cheng Laboratory
//
// XiangShan is licensed under Mulan PSL v2.
// You can use this software according to the terms and conditions of the Mulan PSL v2.
// You may obtain a copy of Mulan PSL v2 at:
//          https://license.coscl.org.cn/MulanPSL2
//
// THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
// EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
// MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
//
// See the Mulan PSL v2 for more details.

package xiangshan.frontend.instruncache

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
import xiangshan.XSBundle
import xiangshan.frontend.PrunedAddr

class InsUncacheReq(implicit p: Parameters) extends XSBundle {
  val addr: PrunedAddr = PrunedAddr(PAddrBits)
}

class InsUncacheResp(implicit p: Parameters) extends XSBundle {
  val data:    UInt = UInt(32.W) // TODO: add a const for InstrLen, maybe in XSParameters, and use it all over the repo
  val corrupt: Bool = Bool()
}

class InstrUncacheIO(implicit p: Parameters) extends InstrUncacheBundle {
  val req:   DecoupledIO[InsUncacheReq]  = Flipped(DecoupledIO(new InsUncacheReq))
  val resp:  DecoupledIO[InsUncacheResp] = DecoupledIO(new InsUncacheResp)
  val flush: Bool                        = Input(Bool())
}

class InstrUncache()(implicit p: Parameters) extends LazyModule with HasInstrUncacheParameters {
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
    with HasInstrUncacheParameters
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
    val entry = Module(new InstrUncacheEntry(edge))

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

    entry.io.mmioGrant.valid := false.B
    entry.io.mmioGrant.bits  := DontCare
    when(mmio_grant.bits.source === i.U) {
      entry.io.mmioGrant <> mmio_grant
    }
    entry
  }

  // override mmio_grant.ready to prevent x-propagation
  mmio_grant.ready := true.B

  entry_alloc_idx := PriorityEncoder(entries.map(m => m.io.req.ready))

  req.ready := req_ready
  resp <> resp_arb.io.out
  TLArbiter.lowestFromSeq(edge, mmio_acquire, entries.map(_.io.mmioAcquire))
}
