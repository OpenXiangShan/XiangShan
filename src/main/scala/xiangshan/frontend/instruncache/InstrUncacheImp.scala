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
import freechips.rocketchip.diplomacy.LazyModuleImp
import freechips.rocketchip.tilelink.TLArbiter
import org.chipsalliance.cde.config.Parameters
import utils.HasTLDump
import xiangshan.frontend.IfuToInstrUncacheIO
import xiangshan.frontend.InstrUncacheToIfuIO

class InstrUncacheImp(wrapper: InstrUncache) extends LazyModuleImp(wrapper)
    with HasInstrUncacheParameters
    with HasTLDump {

  class InstrUncacheIO(implicit p: Parameters) extends InstrUncacheBundle {
    val fromIfu: IfuToInstrUncacheIO = Flipped(new IfuToInstrUncacheIO)
    val toIfu:   InstrUncacheToIfuIO = new InstrUncacheToIfuIO
    val flush:   Bool                = Input(Bool())
  }

  val io: InstrUncacheIO = IO(new InstrUncacheIO)

  private val (bus, edge) = wrapper.clientNode.out.head

  private val respArbiter = Module(new Arbiter(new InstrUncacheResp, cacheParams.nMMIOs))

  private val req         = io.fromIfu.req
  private val resp        = io.toIfu.resp
  private val mmioAcquire = bus.a
  private val mmioGrant   = bus.d

  private val entryAllocIdx = Wire(UInt())
  private val reqReady      = WireInit(false.B)

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
    entry.io.req.valid := (i.U === entryAllocIdx) && req.valid
    entry.io.req.bits  := req.bits
    when(i.U === entryAllocIdx) {
      reqReady := entry.io.req.ready
    }

    // entry resp
    respArbiter.io.in(i) <> entry.io.resp

    entry.io.mmioGrant.valid := false.B
    entry.io.mmioGrant.bits  := DontCare
    when(mmioGrant.bits.source === i.U) {
      entry.io.mmioGrant <> mmioGrant
    }
    entry
  }

  // override mmioGrant.ready to prevent x-propagation
  mmioGrant.ready := true.B

  entryAllocIdx := PriorityEncoder(entries.map(m => m.io.req.ready))

  req.ready := reqReady
  resp <> respArbiter.io.out
  TLArbiter.lowestFromSeq(edge, mmioAcquire, entries.map(_.io.mmioAcquire))
}
