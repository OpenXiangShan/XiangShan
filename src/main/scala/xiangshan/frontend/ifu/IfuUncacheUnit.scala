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

package xiangshan.frontend.ifu
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utils.EnumUInt
import xiangshan.cache.mmu.Pbmt
import xiangshan.frontend.ExceptionType
import xiangshan.frontend.IfuToInstrUncacheIO
import xiangshan.frontend.InstrUncacheToIfuIO
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.PrunedAddrInit

class IfuUncacheUnit(implicit p: Parameters) extends IfuModule with IfuHelper {
  class IfuUncacheIO extends IfuBundle {
    class IfuUncacheReq(implicit p: Parameters) extends IfuBundle {
      val pbmt:   UInt       = UInt(Pbmt.width.W)
      val isMmio: Bool       = Bool()
      val paddr:  PrunedAddr = PrunedAddr(PAddrBits)
    }
    class IfuUncacheResp(implicit p: Parameters) extends IfuBundle {
      val uncacheData: UInt          = UInt(32.W)
      val exception:   ExceptionType = new ExceptionType
      val needResend:  Bool          = Bool() // not RVC, no exception, crossing page boundary, see InstrUncacheResp
    }
    val req          = Flipped(DecoupledIO(new IfuUncacheReq))
    val resp         = Output(ValidIO(new IfuUncacheResp))
    val isFirstInstr = Input(Bool())
    val ifuStall     = Input(Bool())
    val flush        = Input(Bool())
    val emptyAfter   = Input(Bool())
    // Uncache: mmio request / response
    val toUncache   = new IfuToInstrUncacheIO
    val fromUncache = Flipped(new InstrUncacheToIfuIO)
  }
  val io: IfuUncacheIO = IO(new IfuUncacheIO)
  private val toUncache    = io.toUncache.req
  private val fromUncache  = io.fromUncache.resp
  private val isFirstInstr = io.isFirstInstr
  private val ifuStall     = io.ifuStall
  /* *** uncache *** */
  private def nUncacheFsmState = 4
  private object UncacheFsmState extends EnumUInt(nUncacheFsmState) {
    def Idle:           UInt = 0.U(width.W)
    def WaitLastCommit: UInt = 1.U(width.W)
    def SendReq:        UInt = 2.U(width.W)
    def WaitResp:       UInt = 3.U(width.W)
  }

  private val uncacheState = RegInit(UncacheFsmState.Idle)
  def uncacheReady: Bool = uncacheState === UncacheFsmState.Idle
  def uncacheValid: Bool = uncacheState =/= UncacheFsmState.Idle

  private val uncacheData       = RegInit(0.U(32.W))
  private val uncacheException  = RegInit(ExceptionType.None)
  private val uncacheNeedResend = RegInit(false.B)
  private val uncacheFinish     = RegInit(false.B)
  private val uncachePAddr      = RegInit(PrunedAddrInit(0.U(PAddrBits.W)))
  private val isMmio            = RegInit(false.B)
  private val itlbPbmt          = RegInit(0.U(Pbmt.width.W))

  private def uncacheReset(): Unit = {
    uncacheState      := UncacheFsmState.Idle
    uncacheData       := 0.U
    uncacheException  := ExceptionType.None
    uncacheNeedResend := false.B
    uncachePAddr      := PrunedAddrInit(0.U(PAddrBits.W))
    uncacheFinish     := false.B
  }

  switch(uncacheState) {
    is(UncacheFsmState.Idle) {
      // pbmt.nc does not need to wait for last commit as it's idempotent area, while pbmt.io and (pbmt.pma && mmio) needs.
      val shouldWait = io.req.bits.isMmio || Pbmt.isIO(io.req.bits.pbmt)
      when(io.req.valid) {
        uncacheState := Mux(shouldWait, UncacheFsmState.WaitLastCommit, UncacheFsmState.SendReq)
        uncachePAddr := io.req.bits.paddr
        isMmio       := io.req.bits.isMmio
        itlbPbmt     := io.req.bits.pbmt
      }
    }

    is(UncacheFsmState.WaitLastCommit) {
      when(isFirstInstr) {
        uncacheState := UncacheFsmState.SendReq
      }.otherwise {
        uncacheState := Mux(io.emptyAfter, UncacheFsmState.SendReq, UncacheFsmState.WaitLastCommit)
      }
    }

    is(UncacheFsmState.SendReq) {
      uncacheState := Mux(toUncache.fire, UncacheFsmState.WaitResp, UncacheFsmState.SendReq)
    }

    is(UncacheFsmState.WaitResp) {
      when(fromUncache.fire) {
        val exception = ExceptionType.fromTileLink(fromUncache.bits.corrupt, fromUncache.bits.denied)
        uncacheState      := UncacheFsmState.Idle
        uncacheException  := exception
        uncacheNeedResend := fromUncache.bits.needResend
        uncacheData       := fromUncache.bits.data
      }
    }
  }

  when(uncacheState === UncacheFsmState.WaitResp && fromUncache.fire) {
    uncacheFinish := true.B
  }.otherwise {
    uncacheFinish := false.B
  }

  toUncache.valid     := (uncacheState === UncacheFsmState.SendReq) && !ifuStall
  toUncache.bits.addr := uncachePAddr
  // if !pmp_mmio, then we're actually sending a MMIO request to main memory, it must be pbmt.nc/io
  // we need to tell L2 Cache about this to make it work correctly
  toUncache.bits.memBackTypeMM := !isMmio
  toUncache.bits.memPageTypeNC := itlbPbmt === Pbmt.nc

  fromUncache.ready := true.B

  io.req.ready             := uncacheReady
  io.resp.valid            := uncacheFinish
  io.resp.bits.exception   := uncacheException
  io.resp.bits.uncacheData := uncacheData
  io.resp.bits.needResend  := uncacheNeedResend

  when(io.flush) {
    uncacheReset()
  }
}
