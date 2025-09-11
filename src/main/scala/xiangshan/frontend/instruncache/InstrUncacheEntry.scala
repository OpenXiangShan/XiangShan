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
import xiangshan.frontend.ifu.PreDecodeHelper

// One miss entry deals with one mmio request
class InstrUncacheEntry(edge: TLEdgeOut)(implicit p: Parameters) extends InstrUncacheModule with PreDecodeHelper {
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

  // we need 4B of 2B-aligned data from mmio bus, but mmio bus is (MmioBusBytes)B-aligned (by default 8B-aligned),
  // so, if addr(2, 1) is 2'b11, this 4B will cross the bus boundary, we should resend request to get latter 2B of data.
  private val crossBusBoundary = reqReg.addr(log2Ceil(MmioBusBytes) - 1, 1).andR
  // though, if this 4B data crosses the page boundary, we cannot resend request, as physical page may not continuous.
  private val crossPageBoundary = reqReg.addr(PageOffsetWidth - 1, 1).andR
  private val resending         = RegInit(false.B)
  private val resendAddr        = alignedAddr + 1.U

  // send tilelink request
  // if there is a pending wfi request, we should not send new requests to L2
  io.mmioAcquire.valid := state === State.RefillReq && !io.wfi.wfiReq
  io.mmioAcquire.bits := edge.Get(
    fromSource = io.id,
    toAddress = Cat(Mux(resending, resendAddr, alignedAddr), 0.U(log2Ceil(MmioBusBytes).W)),
    lgSize = log2Ceil(MmioBusBytes).U
  )._2
  io.mmioAcquire.bits.user.lift(MemBackTypeMM).foreach(_ := reqReg.memBackTypeMM)
  io.mmioAcquire.bits.user.lift(MemPageTypeNC).foreach(_ := reqReg.memPageTypeNC)

  // receive tilelink response
  io.mmioGrant.ready := state === State.RefillResp

  // we are safe to enter wfi if we have no pending response from L2
  io.wfi.wfiSafe := state =/= State.RefillResp

  private val respDataReg    = RegInit(VecInit.fill(2)(0.U(16.W))) // FIXME: 2 * rvc, how to avoid magic number?
  private val respCorruptReg = RegInit(false.B)

  // send response to InstrUncache
  io.resp.valid           := state === State.SendResp && !needFlush
  io.resp.bits.data       := respDataReg.asUInt
  io.resp.bits.corrupt    := respCorruptReg
  io.resp.bits.incomplete := crossPageBoundary

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

        val shiftedBusData = Mux1H(
          UIntToOH(reqReg.addr(2, 1)),
          Seq(
            io.mmioGrant.bits.data(31, 0),
            io.mmioGrant.bits.data(47, 16),
            io.mmioGrant.bits.data(63, 32),
            Cat(0.U(16.W), io.mmioGrant.bits.data(63, 48))
          )
        )

        // if is corrupted, we need to raise an exception anyway, so no need to resend request
        val respCorrupt = io.mmioGrant.bits.corrupt
        // if response is rvc, we need only 2B, so no need to resend request
        val respIsRvc = isRVC(shiftedBusData)
        // also, if we are already resending, we should not resend again
        val needResend = crossBusBoundary && !crossPageBoundary && !respCorrupt && !respIsRvc && !resending

        state := Mux(needResend, State.RefillReq, State.SendResp)
        resending := needResend

        when(resending) {
          respDataReg(1)     := io.mmioGrant.bits.data(15, 0)
        }.otherwise{
          respDataReg(0)     := shiftedBusData(15, 0)
          respDataReg(1)     := shiftedBusData(31, 16)
        }
        respCorruptReg := io.mmioGrant.bits.corrupt
      }
    }

    is(State.SendResp) {
      when(io.resp.fire || needFlush) {
        state := State.Invalid
      }
    }
  }
}
