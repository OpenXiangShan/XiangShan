/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
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

package xiangshan.mem

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import xiangshan._
import xiangshan.backend.decode.ImmUnion
import xiangshan.cache._
import xiangshan.cache.mmu.{TlbRequestIO, TlbReq, TlbResp, TlbCmd}

// Store Pipeline Stage 0
// Generate addr, use addr to query DCache and DTLB
class StoreUnit_S0(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val in = Flipped(Decoupled(new ExuInput))
    val rsIdx = Input(UInt(log2Up(IssQueSize).W))
    val isFirstIssue = Input(Bool())
    val out = Decoupled(new LsPipelineBundle)
    val dtlbReq = DecoupledIO(new TlbReq)
  })

  // send req to dtlb
  // val saddr = io.in.bits.src(0) + SignExt(io.in.bits.uop.ctrl.imm(11,0), VAddrBits)
  val imm12 = WireInit(io.in.bits.uop.ctrl.imm(11,0))
  val saddr_lo = io.in.bits.src(0)(11,0) + Cat(0.U(1.W), imm12)
  val saddr_hi = Mux(saddr_lo(12),
    Mux(imm12(11), io.in.bits.src(0)(VAddrBits-1, 12), io.in.bits.src(0)(VAddrBits-1, 12)+1.U),
    Mux(imm12(11), io.in.bits.src(0)(VAddrBits-1, 12)+SignExt(1.U, VAddrBits-12), io.in.bits.src(0)(VAddrBits-1, 12)),
  )
  val saddr = Cat(saddr_hi, saddr_lo(11,0))

  io.dtlbReq.bits.vaddr := saddr
  io.dtlbReq.valid := io.in.valid
  io.dtlbReq.bits.cmd := TlbCmd.write
  io.dtlbReq.bits.roqIdx := io.in.bits.uop.roqIdx
  io.dtlbReq.bits.debug.pc := io.in.bits.uop.cf.pc
  io.dtlbReq.bits.debug.isFirstIssue := io.isFirstIssue

  io.out.bits := DontCare
  io.out.bits.vaddr := saddr

  // Now data use its own io
  // io.out.bits.data := genWdata(io.in.bits.src(1), io.in.bits.uop.ctrl.fuOpType(1,0))
  io.out.bits.data := io.in.bits.src(1) // FIXME: remove data from pipeline
  io.out.bits.uop := io.in.bits.uop
  io.out.bits.miss := DontCare
  io.out.bits.rsIdx := io.rsIdx
  io.out.bits.mask := genWmask(io.out.bits.vaddr, io.in.bits.uop.ctrl.fuOpType(1,0))
  io.out.valid := io.in.valid
  io.in.ready := io.out.ready

  // exception check
  val addrAligned = LookupTree(io.in.bits.uop.ctrl.fuOpType(1,0), List(
    "b00".U   -> true.B,              //b
    "b01".U   -> (io.out.bits.vaddr(0) === 0.U),   //h
    "b10".U   -> (io.out.bits.vaddr(1,0) === 0.U), //w
    "b11".U   -> (io.out.bits.vaddr(2,0) === 0.U)  //d
  ))
  io.out.bits.uop.cf.exceptionVec(storeAddrMisaligned) := !addrAligned

  XSPerfAccumulate("addr_spec_success", io.out.fire() && saddr(VAddrBits-1, 12) === io.in.bits.src(0)(VAddrBits-1, 12))
  XSPerfAccumulate("addr_spec_failed", io.out.fire() && saddr(VAddrBits-1, 12) =/= io.in.bits.src(0)(VAddrBits-1, 12))
  XSPerfAccumulate("addr_spec_success_once", io.out.fire() && saddr(VAddrBits-1, 12) === io.in.bits.src(0)(VAddrBits-1, 12) && io.isFirstIssue)
  XSPerfAccumulate("addr_spec_failed_once", io.out.fire() && saddr(VAddrBits-1, 12) =/= io.in.bits.src(0)(VAddrBits-1, 12) && io.isFirstIssue)
}

// Load Pipeline Stage 1
// TLB resp (send paddr to dcache)
class StoreUnit_S1(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val in = Flipped(Decoupled(new LsPipelineBundle))
    val out = Decoupled(new LsPipelineBundle)
    val lsq = ValidIO(new LsPipelineBundle)
    val dtlbResp = Flipped(DecoupledIO(new TlbResp))
    val rsFeedback = ValidIO(new RSFeedback)
  })

  val s1_paddr = io.dtlbResp.bits.paddr
  val s1_tlb_miss = io.dtlbResp.bits.miss
  val s1_mmio = io.dtlbResp.bits.mmio
  val s1_exception = selectStore(io.out.bits.uop.cf.exceptionVec, false).asUInt.orR

  io.in.ready := true.B

  io.dtlbResp.ready := true.B // TODO: why dtlbResp needs a ready?

  // Send TLB feedback to store issue queue
  io.rsFeedback.valid := io.in.valid
  io.rsFeedback.bits.hit := !s1_tlb_miss
  io.rsFeedback.bits.flushState := io.dtlbResp.bits.ptwBack
  io.rsFeedback.bits.rsIdx := io.in.bits.rsIdx
  io.rsFeedback.bits.sourceType := RSFeedbackType.tlbMiss
  XSDebug(io.rsFeedback.valid,
    "S1 Store: tlbHit: %d roqIdx: %d\n",
    io.rsFeedback.bits.hit,
    io.rsFeedback.bits.rsIdx
  )


  // get paddr from dtlb, check if rollback is needed
  // writeback store inst to lsq
  io.lsq.valid := io.in.valid && !s1_tlb_miss
  io.lsq.bits := io.in.bits
  io.lsq.bits.paddr := s1_paddr
  io.lsq.bits.miss := false.B
  io.lsq.bits.mmio := s1_mmio && !s1_exception
  io.lsq.bits.uop.cf.exceptionVec(storePageFault) := io.dtlbResp.bits.excp.pf.st
  io.lsq.bits.uop.cf.exceptionVec(storeAccessFault) := io.dtlbResp.bits.excp.af.st

  // mmio inst with exception will be writebacked immediately
  io.out.valid := io.in.valid && (!io.out.bits.mmio || s1_exception) && !s1_tlb_miss
  io.out.bits := io.lsq.bits
}

class StoreUnit_S2(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val in = Flipped(Decoupled(new LsPipelineBundle))
    val out = Decoupled(new LsPipelineBundle)
  })

  io.in.ready := true.B
  io.out.bits := io.in.bits
  io.out.valid := io.in.valid

}

class StoreUnit_S3(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val in = Flipped(Decoupled(new LsPipelineBundle))
    val stout = DecoupledIO(new ExuOutput) // writeback store
  })

  io.in.ready := true.B

  io.stout.valid := io.in.valid
  io.stout.bits.uop := io.in.bits.uop
  io.stout.bits.data := DontCare
  io.stout.bits.redirectValid := false.B
  io.stout.bits.redirect := DontCare
  io.stout.bits.debug.isMMIO := io.in.bits.mmio
  io.stout.bits.debug.paddr := DontCare
  io.stout.bits.debug.isPerfCnt := false.B
  io.stout.bits.fflags := DontCare

}

class StoreUnit(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val stin = Flipped(Decoupled(new ExuInput))
    val redirect = Flipped(ValidIO(new Redirect))
    val flush = Input(Bool())
    val rsFeedback = ValidIO(new RSFeedback)
    val dtlb = new TlbRequestIO()
    val rsIdx = Input(UInt(log2Up(IssQueSize).W))
    val isFirstIssue = Input(Bool())
    val lsq = ValidIO(new LsPipelineBundle)
    val stout = DecoupledIO(new ExuOutput) // writeback store
  })

  val store_s0 = Module(new StoreUnit_S0)
  val store_s1 = Module(new StoreUnit_S1)
  val store_s2 = Module(new StoreUnit_S2)
  val store_s3 = Module(new StoreUnit_S3)

  store_s0.io.in <> io.stin
  store_s0.io.dtlbReq <> io.dtlb.req
  store_s0.io.rsIdx := io.rsIdx
  store_s0.io.isFirstIssue := io.isFirstIssue

  PipelineConnect(store_s0.io.out, store_s1.io.in, true.B, store_s0.io.out.bits.uop.roqIdx.needFlush(io.redirect, io.flush))

  store_s1.io.lsq <> io.lsq // send result to sq
  store_s1.io.dtlbResp <> io.dtlb.resp
  store_s1.io.rsFeedback <> io.rsFeedback

  PipelineConnect(store_s1.io.out, store_s2.io.in, true.B, store_s1.io.out.bits.uop.roqIdx.needFlush(io.redirect, io.flush))

  PipelineConnect(store_s2.io.out, store_s3.io.in, true.B, store_s2.io.out.bits.uop.roqIdx.needFlush(io.redirect, io.flush))

  store_s3.io.stout <> io.stout

  private def printPipeLine(pipeline: LsPipelineBundle, cond: Bool, name: String): Unit = {
    XSDebug(cond,
      p"$name" + p" pc ${Hexadecimal(pipeline.uop.cf.pc)} " +
        p"addr ${Hexadecimal(pipeline.vaddr)} -> ${Hexadecimal(pipeline.paddr)} " +
        p"op ${Binary(pipeline.uop.ctrl.fuOpType)} " +
        p"data ${Hexadecimal(pipeline.data)} " +
        p"mask ${Hexadecimal(pipeline.mask)}\n"
    )
  }

  printPipeLine(store_s0.io.out.bits, store_s0.io.out.valid, "S0")
  printPipeLine(store_s1.io.out.bits, store_s1.io.out.valid, "S1")

}
