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
import freechips.rocketchip.amba.axi4.AXI4BundleAR
import freechips.rocketchip.diplomacy.LazyModuleImp
import org.chipsalliance.cde.config.Parameters
import xiangshan.WfiReqBundle
import xiangshan.frontend.IfuToInstrUncacheIO
import xiangshan.frontend.InstrUncacheToIfuIO

class InstrUncacheImp(wrapper: InstrUncache) extends LazyModuleImp(wrapper)
    with HasInstrUncacheParameters {

  class InstrUncacheIO(implicit p: Parameters) extends InstrUncacheBundle {
    val fromIfu: IfuToInstrUncacheIO = Flipped(new IfuToInstrUncacheIO)
    val toIfu:   InstrUncacheToIfuIO = new InstrUncacheToIfuIO
    val flush:   Bool                = Input(Bool())
    val wfi:     WfiReqBundle        = Flipped(new WfiReqBundle)
  }

  val io: InstrUncacheIO = IO(new InstrUncacheIO)

  private val (axi, _) = wrapper.axiNode.out.head

  private val respArbiter = Module(new Arbiter(new InstrUncacheResp, nMmioEntry))

  private val req  = io.fromIfu.req
  private val resp = io.toIfu.resp

  private val entryAllocIdx = Wire(UInt(log2Up(nMmioEntry).W))
  private val reqReady      = WireInit(false.B)

  // read-only: tie off write channels
  axi.aw.valid := false.B
  axi.aw.bits  := DontCare
  axi.w.valid  := false.B
  axi.w.bits   := DontCare
  axi.b.ready  := true.B
  axi.r.ready  := true.B

  private val entries = (0 until nMmioEntry).map { i =>
    val entry = Module(new InstrUncacheEntry(axi.params))

    entry.io.id    := i.U(axi.params.idBits.W)
    entry.io.flush := io.flush
    entry.io.wfi.wfiReq := io.wfi.wfiReq

    // entry req
    entry.io.req.valid := (i.U === entryAllocIdx) && req.valid
    entry.io.req.bits  := req.bits
    when(i.U === entryAllocIdx) {
      reqReady := entry.io.req.ready
    }

    // entry resp
    respArbiter.io.in(i) <> entry.io.resp

    // route R to entry by AXI id
    entry.io.r.valid := false.B
    entry.io.r.bits  := DontCare
    when(axi.r.valid && axi.r.bits.id === i.U(axi.params.idBits.W)) {
      entry.io.r <> axi.r
    }
    entry
  }

  entryAllocIdx := PriorityEncoder(entries.map(_.io.req.ready))

  req.ready := reqReady
  resp <> respArbiter.io.out

  private val arArb = Module(new Arbiter(new AXI4BundleAR(axi.params), nMmioEntry))
  (arArb.io.in zip entries.map(_.io.ar)).foreach { case (in, ar) =>
    in <> ar
  }
  axi.ar.valid := arArb.io.out.valid
  axi.ar.bits  := arArb.io.out.bits
  arArb.io.out.ready := axi.ar.ready

  // we are safe to enter wfi if all entries have no pending response from L2
  io.wfi.wfiSafe := entries.map(_.io.wfi.wfiSafe).reduce(_ && _)
}
