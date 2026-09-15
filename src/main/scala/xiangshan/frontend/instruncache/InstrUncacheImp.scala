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
import oceanus.compactchi._
import org.chipsalliance.cde.config.Parameters
import xiangshan.WfiReqBundle
import xiangshan.cache.CCHIType3Port
import xiangshan.frontend.IfuToInstrUncacheIO
import xiangshan.frontend.InstrUncacheToIfuIO

class InstrUncacheImp(wrapper: InstrUncache) extends LazyModuleImp(wrapper)
    with HasInstrUncacheParameters {

  class InstrUncacheIO(implicit p: Parameters) extends InstrUncacheBundle {
    val fromIfu: IfuToInstrUncacheIO = Flipped(new IfuToInstrUncacheIO)
    val toIfu:   InstrUncacheToIfuIO = new InstrUncacheToIfuIO
    val flush:   Bool                = Input(Bool())
    val wfi:     WfiReqBundle        = Flipped(new WfiReqBundle)
    val cchi:    CCHIType3Port       = new CCHIType3Port
  }

  val io: InstrUncacheIO = IO(new InstrUncacheIO)

  private val respArbiter = Module(new Arbiter(new InstrUncacheResp, nMmioEntry))

  private val req  = io.fromIfu.req
  private val resp = io.toIfu.resp

  private val entryAllocIdx = Wire(UInt(log2Up(nMmioEntry).W))
  private val reqReady      = WireInit(false.B)

  // assign default values to output signals (read-only: no upDAT)
  io.cchi.upDAT.valid := false.B
  io.cchi.upDAT.bits  := DontCare
  io.cchi.dnRSP.ready := true.B
  io.cchi.dnDAT.ready := true.B

  private val entries = (0 until nMmioEntry).map { i =>
    val entry = Module(new InstrUncacheEntry)

    entry.io.id    := i.U(log2Up(nMmioEntry).W)
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

    // route CompData to entry by TxnID (same role as TL source)
    entry.io.compData.valid := false.B
    entry.io.compData.bits  := DontCare
    when(io.cchi.dnDAT.valid && io.cchi.dnDAT.bits.TxnID === i.U) {
      entry.io.compData <> io.cchi.dnDAT
    }
    entry
  }

  entryAllocIdx := PriorityEncoder(entries.map(_.io.req.ready))

  req.ready := reqReady
  resp <> respArbiter.io.out

  private val readReqArb = Module(new Arbiter(new FlitREQ, nMmioEntry))
  (readReqArb.io.in zip entries.map(_.io.readReq)).foreach { case (in, readReq) =>
    in <> readReq
  }
  io.cchi.upREQ <> readReqArb.io.out

  // we are safe to enter wfi if all entries have no pending response from L2
  io.wfi.wfiSafe := entries.map(_.io.wfi.wfiSafe).reduce(_ && _)
}
