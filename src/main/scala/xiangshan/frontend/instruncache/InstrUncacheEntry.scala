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
import freechips.rocketchip.tilelink.TLBundleA
import freechips.rocketchip.tilelink.TLBundleD
import freechips.rocketchip.tilelink.TLEdgeOut
import org.chipsalliance.cde.config.Parameters
import utils.NamedUInt
import xiangshan.frontend.PrunedAddr

// One miss entry deals with one mmio request
class InstrUncacheEntry(edge: TLEdgeOut)(implicit p: Parameters) extends InstrUncacheModule {
  class InstrUncacheEntryIO(edge: TLEdgeOut)(implicit p: Parameters) extends InstrUncacheBundle {
    val id: UInt = Input(UInt(log2Up(cacheParams.nMMIOs).W))
    // client requests
    val req:  DecoupledIO[InsUncacheReq]  = Flipped(DecoupledIO(new InsUncacheReq))
    val resp: DecoupledIO[InsUncacheResp] = DecoupledIO(new InsUncacheResp)

    val mmioAcquire: DecoupledIO[TLBundleA] = DecoupledIO(new TLBundleA(edge.bundle))
    val mmioGrant:   DecoupledIO[TLBundleD] = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))

    val flush: Bool = Input(Bool())
  }

  val io: InstrUncacheEntryIO = IO(new InstrUncacheEntryIO(edge))

  private def nState: Int = 4
  private object State extends NamedUInt(log2Up(nState)) {
    def Invalid:    UInt = 0.U(width.W)
    def RefillReq:  UInt = 1.U(width.W)
    def RefillResp: UInt = 2.U(width.W)
    def SendResp:   UInt = 3.U(width.W)
  }

  private val state = RegInit(State.Invalid)

  private val req            = Reg(new InsUncacheReq)
  private val respDataReg    = RegInit(0.U(MmioBusWidth.W))
  private val respCorruptReg = RegInit(false.B)

  // assign default values to output signals
  io.req.ready  := false.B
  io.resp.valid := false.B
  io.resp.bits  := DontCare

  io.mmioAcquire.valid := false.B
  io.mmioAcquire.bits  := DontCare

  io.mmioGrant.ready := false.B

  private val needFlush = RegInit(false.B)

  when(io.flush && (state =/= State.Invalid) && (state =/= State.SendResp)) {
    needFlush := true.B
  }.elsewhen((state === State.SendResp) && needFlush) {
    needFlush := false.B
  }

  // --------------------------------------------
  // State.Invalid: receive requests
  when(state === State.Invalid) {
    io.req.ready := true.B

    when(io.req.fire) {
      req   := io.req.bits
      state := State.RefillReq
    }
  }

  when(state === State.RefillReq) {
    val alignedAddr = req.addr(req.addr.getWidth - 1, log2Ceil(MmioBusBytes))
    io.mmioAcquire.valid := true.B
    io.mmioAcquire.bits := edge.Get(
      fromSource = io.id,
      toAddress = Cat(alignedAddr, 0.U(log2Ceil(MmioBusBytes).W)),
      lgSize = log2Ceil(MmioBusBytes).U
    )._2

    when(io.mmioAcquire.fire) {
      state := State.RefillResp
    }
  }

  val (_, _, refillDone, _) = edge.addr_inc(io.mmioGrant)

  when(state === State.RefillResp) {
    io.mmioGrant.ready := true.B

    when(io.mmioGrant.fire) {
      assert(refillDone)
      respDataReg    := io.mmioGrant.bits.data
      respCorruptReg := io.mmioGrant.bits.corrupt // this includes bits.denied, as tilelink spec defines
      state          := State.SendResp
    }
  }

  private def getDataFromBus(pc: PrunedAddr): UInt = {
    val respData = Wire(UInt(32.W))
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

  when(state === State.SendResp) {
    io.resp.valid        := !needFlush
    io.resp.bits.data    := getDataFromBus(req.addr)
    io.resp.bits.corrupt := respCorruptReg
    // metadata should go with the response
    when(io.resp.fire || needFlush) {
      state := State.Invalid
    }
  }
}
