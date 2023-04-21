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
import utility._
import xiangshan.ExceptionNO._
import xiangshan._
import xiangshan.backend.fu.PMPRespBundle
import xiangshan.backend.fu.FuConfig.StaCfg
import xiangshan.backend.rob.DebugLsInfoBundle
import xiangshan.cache.mmu.{TlbCmd, TlbReq, TlbRequestIO, TlbResp}
import xiangshan.backend.Bundles.{MemExuInput, MemExuOutput}


// Store Pipeline Stage 0
// Generate addr, use addr to query DCache and DTLB
class StoreUnit_S0(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val in = Flipped(Decoupled(new MemExuInput))
    val out = Decoupled(new LsPipelineBundle)
    val dtlbReq = DecoupledIO(new TlbReq)
  })

  // send req to dtlb
  // val saddr = io.in.bits.src(0) + SignExt(io.in.bits.uop.ctrl.imm(11,0), VAddrBits)
  val imm12 = WireInit(io.in.bits.uop.imm(11,0))
  val saddr_lo = io.in.bits.src(0)(11,0) + Cat(0.U(1.W), imm12)
  val saddr_hi = Mux(saddr_lo(12),
    Mux(imm12(11), io.in.bits.src(0)(VAddrBits-1, 12), io.in.bits.src(0)(VAddrBits-1, 12)+1.U),
    Mux(imm12(11), io.in.bits.src(0)(VAddrBits-1, 12)+SignExt(1.U, VAddrBits-12), io.in.bits.src(0)(VAddrBits-1, 12)),
  )
  val saddr = Cat(saddr_hi, saddr_lo(11,0))

  io.dtlbReq.bits.vaddr := saddr
  io.dtlbReq.valid := io.in.valid
  io.dtlbReq.bits.cmd := TlbCmd.write
  io.dtlbReq.bits.size := LSUOpType.size(io.in.bits.uop.fuOpType)
  io.dtlbReq.bits.kill := DontCare
  io.dtlbReq.bits.memidx.is_ld := false.B
  io.dtlbReq.bits.memidx.is_st := true.B
  io.dtlbReq.bits.memidx.idx := io.in.bits.uop.sqIdx.value
  io.dtlbReq.bits.debug.robIdx := io.in.bits.uop.robIdx
  io.dtlbReq.bits.no_translate := false.B
  io.dtlbReq.bits.debug.pc := io.in.bits.uop.pc
  io.dtlbReq.bits.debug.isFirstIssue := io.in.bits.isFirstIssue

  io.out.bits := DontCare
  io.out.bits.vaddr := saddr

  // Now data use its own io
  // io.out.bits.data := genWdata(io.in.bits.src(1), io.in.bits.uop.ctrl.fuOpType(1,0))
  io.out.bits.data := io.in.bits.src(1) // FIXME: remove data from pipeline
  io.out.bits.uop := io.in.bits.uop
  io.out.bits.miss := DontCare
  io.out.bits.rsIdx := io.in.bits.iqIdx // guarded by io.in.valid
  io.out.bits.mask := genWmask(io.out.bits.vaddr, io.in.bits.uop.fuOpType(1,0))
  io.out.bits.isFirstIssue := io.in.bits.isFirstIssue // guarded by io.in.valid
  io.out.bits.wlineflag := io.in.bits.uop.fuOpType === LSUOpType.cbo_zero
  io.out.valid := io.in.valid
  io.in.ready := io.out.ready
  when(io.in.valid && io.in.bits.isFirstIssue) {
    io.out.bits.uop.debugInfo.tlbFirstReqTime := GTimer()
  }

  // exception check
  val addrAligned = LookupTree(io.in.bits.uop.fuOpType(1,0), List(
    "b00".U   -> true.B,              //b
    "b01".U   -> (io.out.bits.vaddr(0) === 0.U),   //h
    "b10".U   -> (io.out.bits.vaddr(1,0) === 0.U), //w
    "b11".U   -> (io.out.bits.vaddr(2,0) === 0.U)  //d
  ))

  io.out.bits.uop.exceptionVec(storeAddrMisaligned) := !addrAligned

  XSPerfAccumulate("in_valid", io.in.valid)
  XSPerfAccumulate("in_fire", io.in.fire)
  XSPerfAccumulate("in_fire_first_issue", io.in.fire && io.in.bits.isFirstIssue)
  XSPerfAccumulate("addr_spec_success", io.out.fire() && saddr(VAddrBits-1, 12) === io.in.bits.src(0)(VAddrBits-1, 12))
  XSPerfAccumulate("addr_spec_failed", io.out.fire() && saddr(VAddrBits-1, 12) =/= io.in.bits.src(0)(VAddrBits-1, 12))
  XSPerfAccumulate("addr_spec_success_once", io.out.fire() && saddr(VAddrBits-1, 12) === io.in.bits.src(0)(VAddrBits-1, 12) && io.in.bits.isFirstIssue)
  XSPerfAccumulate("addr_spec_failed_once", io.out.fire() && saddr(VAddrBits-1, 12) =/= io.in.bits.src(0)(VAddrBits-1, 12) && io.in.bits.isFirstIssue)
}

// Store Pipeline Stage 1
// TLB resp (send paddr to dcache)
class StoreUnit_S1(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val in = Flipped(Decoupled(new LsPipelineBundle))
    val out = Decoupled(new LsPipelineBundle)
    val lsq = ValidIO(new LsPipelineBundle())
    val dtlbResp = Flipped(DecoupledIO(new TlbResp()))
    val rsFeedback = ValidIO(new RSFeedback)
    val reExecuteQuery = Valid(new LoadReExecuteQueryIO)
  })

  // mmio cbo decoder
  val is_mmio_cbo = io.in.bits.uop.fuOpType === LSUOpType.cbo_clean ||
    io.in.bits.uop.fuOpType === LSUOpType.cbo_flush ||
    io.in.bits.uop.fuOpType === LSUOpType.cbo_inval

  val s1_paddr = io.dtlbResp.bits.paddr(0)
  val s1_tlb_miss = io.dtlbResp.bits.miss

  val s1_mmio = is_mmio_cbo
  val s1_exception = ExceptionNO.selectByFu(io.out.bits.uop.exceptionVec, StaCfg).asUInt.orR

  io.in.ready := true.B

  io.dtlbResp.ready := true.B // TODO: why dtlbResp needs a ready?

  // st-ld violation dectect request.
  io.reExecuteQuery.valid := io.in.valid && !s1_tlb_miss
  io.reExecuteQuery.bits.robIdx := io.in.bits.uop.robIdx
  io.reExecuteQuery.bits.paddr := s1_paddr
  io.reExecuteQuery.bits.mask := io.in.bits.mask

  // Send TLB feedback to store issue queue
  // Store feedback is generated in store_s1, sent to RS in store_s2
  io.rsFeedback.valid := io.in.valid
  io.rsFeedback.bits.hit := !s1_tlb_miss
  io.rsFeedback.bits.flushState := io.dtlbResp.bits.ptwBack
  io.rsFeedback.bits.rsIdx := io.in.bits.rsIdx
  io.rsFeedback.bits.sourceType := RSFeedbackType.tlbMiss
  XSDebug(io.rsFeedback.valid,
    "S1 Store: tlbHit: %d robIdx: %d\n",
    io.rsFeedback.bits.hit,
    io.rsFeedback.bits.rsIdx
  )
  io.rsFeedback.bits.dataInvalidSqIdx := DontCare

  // get paddr from dtlb, check if rollback is needed
  // writeback store inst to lsq
  io.out.valid := io.in.valid && !s1_tlb_miss
  io.out.bits := io.in.bits
  io.out.bits.paddr := s1_paddr
  io.out.bits.miss := false.B
  io.out.bits.mmio := s1_mmio
  io.out.bits.atomic := s1_mmio
  io.out.bits.uop.exceptionVec(storePageFault) := io.dtlbResp.bits.excp(0).pf.st
  io.out.bits.uop.exceptionVec(storeAccessFault) := io.dtlbResp.bits.excp(0).af.st

  io.lsq.valid := io.in.valid
  io.lsq.bits := io.out.bits
  io.lsq.bits.miss := s1_tlb_miss

  // mmio inst with exception will be writebacked immediately
  // io.out.valid := io.in.valid && (!io.out.bits.mmio || s1_exception) && !s1_tlb_miss

  // write below io.out.bits assign sentence to prevent overwriting values
  val s1_tlb_memidx = io.dtlbResp.bits.memidx
  when(s1_tlb_memidx.is_st && io.dtlbResp.valid && !s1_tlb_miss && s1_tlb_memidx.idx === io.out.bits.uop.sqIdx.value) {
    // printf("Store idx = %d\n", s1_tlb_memidx.idx)
    io.out.bits.uop.debugInfo.tlbRespTime := GTimer()
  }

  XSPerfAccumulate("in_valid", io.in.valid)
  XSPerfAccumulate("in_fire", io.in.fire)
  XSPerfAccumulate("in_fire_first_issue", io.in.fire && io.in.bits.isFirstIssue)
  XSPerfAccumulate("tlb_miss", io.in.fire && s1_tlb_miss)
  XSPerfAccumulate("tlb_miss_first_issue", io.in.fire && s1_tlb_miss && io.in.bits.isFirstIssue)
}

class StoreUnit_S2(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val in = Flipped(Decoupled(new LsPipelineBundle))
    val pmpResp = Flipped(new PMPRespBundle)
    val static_pm = Input(Valid(Bool()))
    val out = Decoupled(new LsPipelineBundle)
  })
  val pmp = WireInit(io.pmpResp)
  when (io.static_pm.valid) {
    pmp.ld := false.B
    pmp.st := false.B
    pmp.instr := false.B
    pmp.mmio := io.static_pm.bits
  }

  val s2_exception = ExceptionNO.selectByFu(io.out.bits.uop.exceptionVec, StaCfg).asUInt.orR
  val is_mmio = io.in.bits.mmio || pmp.mmio

  io.in.ready := true.B
  io.out.bits := io.in.bits
  io.out.bits.mmio := is_mmio && !s2_exception
  io.out.bits.atomic := io.in.bits.atomic || pmp.atomic
  io.out.bits.uop.exceptionVec(storeAccessFault) := io.in.bits.uop.exceptionVec(storeAccessFault) || pmp.st
  io.out.valid := io.in.valid && (!is_mmio || s2_exception)
}

class StoreUnit_S3(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val in = Flipped(Decoupled(new LsPipelineBundle))
    val stout = DecoupledIO(new MemExuOutput) // writeback store
  })

  io.in.ready := true.B

  io.stout.valid := io.in.valid
  io.stout.bits.uop := io.in.bits.uop
  io.stout.bits.data := DontCare
  io.stout.bits.debug.isMMIO := io.in.bits.mmio
  io.stout.bits.debug.paddr := io.in.bits.paddr
  io.stout.bits.debug.vaddr := io.in.bits.vaddr
  io.stout.bits.debug.isPerfCnt := false.B
}

class StoreUnit(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val stin = Flipped(Decoupled(new MemExuInput))
    val redirect = Flipped(ValidIO(new Redirect))
    val feedbackSlow = ValidIO(new RSFeedback)
    val tlb = new TlbRequestIO()
    val pmp = Flipped(new PMPRespBundle())
    val lsq = ValidIO(new LsPipelineBundle)
    val lsq_replenish = Output(new LsPipelineBundle())
    val stout = DecoupledIO(new MemExuOutput) // writeback store
    // store mask, send to sq in store_s0
    val storeMaskOut = Valid(new StoreMaskBundle)
    val reExecuteQuery = Valid(new LoadReExecuteQueryIO)
    val issue = Valid(new MemExuInput)
    val debug_ls = Output(new DebugLsInfoBundle)
  })

  val store_s0 = Module(new StoreUnit_S0)
  val store_s1 = Module(new StoreUnit_S1)
  val store_s2 = Module(new StoreUnit_S2)
  val store_s3 = Module(new StoreUnit_S3)

  store_s0.io.in <> io.stin
  store_s0.io.dtlbReq <> io.tlb.req
  io.tlb.req_kill := false.B

  io.storeMaskOut.valid := store_s0.io.in.valid
  io.storeMaskOut.bits.mask := store_s0.io.out.bits.mask
  io.storeMaskOut.bits.sqIdx := store_s0.io.out.bits.uop.sqIdx

  PipelineConnect(store_s0.io.out, store_s1.io.in, true.B, store_s0.io.out.bits.uop.robIdx.needFlush(io.redirect))
  io.issue.valid := store_s1.io.in.valid && !store_s1.io.dtlbResp.bits.miss
  io.issue.bits := RegEnable(store_s0.io.in.bits, store_s0.io.in.valid)

  store_s1.io.dtlbResp <> io.tlb.resp
  io.lsq <> store_s1.io.lsq
  io.reExecuteQuery := store_s1.io.reExecuteQuery

  PipelineConnect(store_s1.io.out, store_s2.io.in, true.B, store_s1.io.out.bits.uop.robIdx.needFlush(io.redirect))

  // feedback tlb miss to RS in store_s2
  io.feedbackSlow.bits := RegNext(store_s1.io.rsFeedback.bits)
  io.feedbackSlow.valid := RegNext(store_s1.io.rsFeedback.valid && !store_s1.io.out.bits.uop.robIdx.needFlush(io.redirect))

  store_s2.io.pmpResp <> io.pmp
  store_s2.io.static_pm := RegNext(io.tlb.resp.bits.static_pm)
  io.lsq_replenish := store_s2.io.out.bits // mmio and exception
  PipelineConnect(store_s2.io.out, store_s3.io.in, true.B, store_s2.io.out.bits.uop.robIdx.needFlush(io.redirect))

  store_s3.io.stout <> io.stout

  io.debug_ls := DontCare
  io.debug_ls.s1.isTlbFirstMiss := io.tlb.resp.valid && io.tlb.resp.bits.miss && io.tlb.resp.bits.debug.isFirstIssue
  io.debug_ls.s1_robIdx := store_s1.io.in.bits.uop.robIdx.value

  private def printPipeLine(pipeline: LsPipelineBundle, cond: Bool, name: String): Unit = {
    XSDebug(cond,
      p"$name" + p" pc ${Hexadecimal(pipeline.uop.pc)} " +
        p"addr ${Hexadecimal(pipeline.vaddr)} -> ${Hexadecimal(pipeline.paddr)} " +
        p"op ${Binary(pipeline.uop.fuOpType)} " +
        p"data ${Hexadecimal(pipeline.data)} " +
        p"mask ${Hexadecimal(pipeline.mask)}\n"
    )
  }

  printPipeLine(store_s0.io.out.bits, store_s0.io.out.valid, "S0")
  printPipeLine(store_s1.io.out.bits, store_s1.io.out.valid, "S1")
}
