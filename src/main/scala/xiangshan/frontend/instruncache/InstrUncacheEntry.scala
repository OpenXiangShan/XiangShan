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
import coupledL2.MemBackTypeMM
import coupledL2.MemPageTypeNC
import freechips.rocketchip.tilelink.TLBundleA
import freechips.rocketchip.tilelink.TLBundleD
import freechips.rocketchip.tilelink.TLEdgeOut
import org.chipsalliance.cde.config.Parameters
import utils.EnumUInt
import xiangshan.WfiReqBundle

// One miss entry deals with one mmio request
class InstrUncacheEntry(edge: TLEdgeOut)(implicit p: Parameters) extends InstrUncacheModule {
  class InstrUncacheEntryIO(edge: TLEdgeOut)(implicit p: Parameters) extends InstrUncacheBundle {
    val id: UInt = Input(UInt(log2Up(nMmioEntry).W))
    // client requests
    val req:  DecoupledIO[InstrUncacheReq]  = Flipped(DecoupledIO(new InstrUncacheReq))
    val resp: DecoupledIO[InstrUncacheResp] = DecoupledIO(new InstrUncacheResp)

    val mmioAcquire: DecoupledIO[TLBundleA] = DecoupledIO(new TLBundleA(edge.bundle))
    val mmioGrant:   DecoupledIO[TLBundleD] = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))

    val flush: Bool         = Input(Bool())
    val wfi:   WfiReqBundle = Flipped(new WfiReqBundle)
  }

  val io: InstrUncacheEntryIO = IO(new InstrUncacheEntryIO(edge))

  private def nState: Int = 4
  private object State extends EnumUInt(nState) {
    def Invalid:    UInt = 0.U(width.W)
    def RefillReq:  UInt = 1.U(width.W)
    def RefillResp: UInt = 2.U(width.W)
    def SendResp:   UInt = 3.U(width.W)
  }

  private val state = RegInit(State.Invalid)

  // hold flush state
  private val needFlush = RegInit(false.B)
  when(io.flush && (state =/= State.Invalid) && (state =/= State.SendResp)) {
    needFlush := true.B
  }.elsewhen((state === State.SendResp) && needFlush) {
    needFlush := false.B
  }

  // receive request from InstrUncache
  io.req.ready := state === State.Invalid
  private val reqReg      = RegEnable(io.req.bits, 0.U.asTypeOf(io.req.bits), io.req.fire)
  private val alignedAddr = reqReg.addr(reqReg.addr.getWidth - 1, log2Ceil(MmioBusBytes))

  // send tilelink request
  io.mmioAcquire.valid := state === State.RefillReq && !io.wfi.wfiReq // if there is a pending wfi request, we should not send new requests to L2
  io.mmioAcquire.bits := edge.Get(
    fromSource = io.id,
    toAddress = Cat(alignedAddr, 0.U(log2Ceil(MmioBusBytes).W)),
    lgSize = log2Ceil(MmioBusBytes).U
  )._2
  io.mmioAcquire.bits.user.lift(MemBackTypeMM).foreach(_ := reqReg.memBackTypeMM)
  io.mmioAcquire.bits.user.lift(MemPageTypeNC).foreach(_ := reqReg.memPageTypeNC)

  // receive tilelink response
  io.mmioGrant.ready := state === State.RefillResp

  // we are safe to enter wfi if we have no pending response from L2
  io.wfi.wfiSafe := state =/= State.RefillResp

  private val respDataReg    = RegEnable(io.mmioGrant.bits.data, 0.U(MmioBusWidth.W), io.mmioGrant.fire)
  private val respCorruptReg = RegEnable(io.mmioGrant.bits.corrupt, false.B, io.mmioGrant.fire)
  private val respDeniedReg  = RegEnable(io.mmioGrant.bits.denied, false.B, io.mmioGrant.fire)

  // send response to InstrUncache
  io.resp.valid := state === State.SendResp && !needFlush
  io.resp.bits.data := Mux1H(
    UIntToOH(reqReg.addr(2, 1)),
    Seq(
      respDataReg(31, 0),
      respDataReg(47, 16),
      respDataReg(63, 32),
      Cat(0.U(16.W), respDataReg(63, 48))
    )
  )
  io.resp.bits.corrupt := respCorruptReg
  io.resp.bits.denied  := respDeniedReg

  // state transfer
  switch(state) {
    is(State.Invalid) {
      when(io.req.fire) {
        state := State.RefillReq
      }
    }

    is(State.RefillReq) {
      when(io.mmioAcquire.fire) {
        state := State.RefillResp
      }
    }

    is(State.RefillResp) {
      when(io.mmioGrant.fire) {
        // we request size <= mmio bus width, so we should be able to get full response in one beat,
        // which means we can assert refillDone when io.mmioGrant.fire
        val (_, _, refillDone, _) = edge.addr_inc(io.mmioGrant)
        assert(refillDone)
        state := State.SendResp
      }
    }

    is(State.SendResp) {
      when(io.resp.fire || needFlush) {
        state := State.Invalid
      }
    }
  }
}
