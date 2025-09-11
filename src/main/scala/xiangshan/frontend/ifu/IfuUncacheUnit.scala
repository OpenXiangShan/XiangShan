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
import utils.EnumUInt
import xiangshan.frontend.MmioCommitRead
import xiangshan.frontend.IfuToInstrUncacheIO
import xiangshan.frontend.InstrUncacheToIfuIO
import xiangshan.frontend.ftq.FtqPtr
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.PrunedAddrInit
import xiangshan.frontend.ExceptionType
import xiangshan.cache.mmu.Pbmt
import org.chipsalliance.cde.config.Parameters

class IfuUncacheUnit(implicit p: Parameters) extends IfuModule with IfuHelper {
  class IfuUncacheIO extends IfuBundle {
    class IfuUncacheReq(implicit p: Parameters) extends IfuBundle {
      val ftqIdx:   FtqPtr    = new FtqPtr
      val pbmt:     UInt      = UInt(Pbmt.width.W)
      val isMmio:   Bool      = Bool()
      val paddr:    PrunedAddr  = PrunedAddr(PAddrBits)
      val isInstrHead:  Bool    = Bool()
    }
    class IfuUncacheResp(implicit p: Parameters) extends IfuBundle {
      val uncacheData: UInt           = UInt(32.W)
      val exception:   ExceptionType  = new ExceptionType
      val crossPage:   Bool           = Bool()
    }
    val req            = Flipped(DecoupledIO(new IfuUncacheReq))
    val resp           = Output(ValidIO(new IfuUncacheResp))
    val isFirstInstr   = Input(Bool())
    val flush          = Input(Bool())
    val mmioCommitRead = new MmioCommitRead
    // Uncache: mmio request / response
    val toUncache      = new IfuToInstrUncacheIO
    val fromUncache    = Flipped(new InstrUncacheToIfuIO)
  }
  val io: IfuUncacheIO = IO(new IfuUncacheIO)
  private val toUncache   = io.toUncache.req
  private val fromUncache = io.fromUncache.resp
  private val isFirstInstr = io.isFirstInstr
  /* *** uncache *** */
  private def nUncacheFsmState = 11
  private object UncacheFsmState extends EnumUInt(nUncacheFsmState) {
    def Idle:           UInt = 0.U(width.W)
    def WaitLastCommit: UInt = 1.U(width.W)
    def SendReq:        UInt = 2.U(width.W)
    def WaitResp:       UInt = 3.U(width.W)
    def SendTlb:        UInt = 4.U(width.W)
    def TlbResp:        UInt = 5.U(width.W)
    def SendPmp:        UInt = 6.U(width.W)
    def ResendReq:      UInt = 7.U(width.W)
    def WaitResendResp: UInt = 8.U(width.W)
    def WaitCommit:     UInt = 9.U(width.W)
    def Commited:       UInt = 10.U(width.W)
  }

  private val uncacheState = RegInit(UncacheFsmState.Idle)

  private val uncacheData      = RegInit(0.U(32.W))
  private val uncacheException = RegInit(ExceptionType.None)
  private val uncacheCrossPage = RegInit(false.B)
  private val uncacheValid     = RegInit(false.B)
  private val uncacheReady     = RegInit(false.B)
  private val uncacheFinish    = RegInit(false.B)
  private val uncachePAddr     = RegInit(PrunedAddrInit(0.U(PAddrBits.W)))
  private val isInstrHead      = RegInit(false.B)
  private val isMmio           = RegInit(false.B)
  private val itlbPbmt         = RegInit(0.U(Pbmt.width.W))

  private def uncacheReset(): Unit = {
    uncacheState := UncacheFsmState.Idle
    uncacheData      := 0.U
    uncacheException := ExceptionType.None
    uncacheCrossPage := false.B
    isInstrHead      := false.B
    uncachePAddr     := PrunedAddrInit(0.U(PAddrBits.W))
    uncacheValid     := false.B
    uncacheReady     := true.B
    uncacheFinish    := false.B
  }

  // last instruction finish
  private val reqIsMmio    = io.req.valid && io.req.bits.isMmio

  switch(uncacheState) {
    is(UncacheFsmState.Idle) {
      uncacheReset()
      when(reqIsMmio) {
        uncacheState := Mux(reqIsMmio, UncacheFsmState.WaitLastCommit, UncacheFsmState.SendReq)
        uncacheValid := true.B
        uncachePAddr := io.req.bits.paddr
        isMmio       := io.req.bits.isMmio
        itlbPbmt     := io.req.bits.pbmt
        isInstrHead  := io.req.bits.isInstrHead
        uncacheReady := false.B
      }
    }

    is(UncacheFsmState.WaitLastCommit) {
      when(isFirstInstr) {
        uncacheState := UncacheFsmState.SendReq
      }.otherwise {
        // uncacheState := Mux(io.mmioCommitRead.mmioLastCommit, UncacheFsmState.SendReq, UncacheFsmState.WaitLastCommit)
        uncacheState := Mux(true.B, UncacheFsmState.SendReq, UncacheFsmState.WaitLastCommit)
      }
    }

    is(UncacheFsmState.SendReq) {
      uncacheState  := Mux(toUncache.fire, UncacheFsmState.WaitResp, UncacheFsmState.SendReq)
    }

    is(UncacheFsmState.WaitResp) {
      when(fromUncache.fire) {
        val exception   = ExceptionType(hasAf = fromUncache.bits.corrupt)
        val crossPage   = fromUncache.bits.incomplete
        uncacheState      := UncacheFsmState.Idle
        uncacheException  := exception
        uncacheCrossPage  := crossPage
        uncacheData       := fromUncache.bits.data
        uncacheFinish     := true.B
        uncacheReady      := true.B
      }
    }
  }

  toUncache.valid := (uncacheState === UncacheFsmState.SendReq) && uncacheValid

  toUncache.bits.addr := uncachePAddr
  // if !pmp_mmio, then we're actually sending a MMIO request to main memory, it must be pbmt.nc/io
  // we need to tell L2 Cache about this to make it work correctly
  toUncache.bits.memBackTypeMM := !isMmio
  toUncache.bits.memPageTypeNC := itlbPbmt === Pbmt.nc
  // toUncache.bits.isInstrHead   := isInstrHead

  fromUncache.ready := true.B

  io.req.ready              := uncacheReady
  io.resp.valid             := uncacheFinish
  io.resp.bits.exception    := uncacheException
  io.resp.bits.uncacheData  := uncacheData
  io.resp.bits.crossPage    := uncacheCrossPage

  // When a single MMIO instruction spans pages,
  // should the second send for confirming the oldest instruction be blocked?
  io.mmioCommitRead.valid       := uncacheValid && isMmio
  io.mmioCommitRead.mmioFtqPtr  := RegEnable(io.req.bits.ftqIdx - 1.U, io.req.valid)
  when(io.flush) {
    uncacheReset()
  }
}