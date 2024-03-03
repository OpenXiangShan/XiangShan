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

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import utility._
import xiangshan.ExceptionNO._
import xiangshan._
import xiangshan.backend.fu.PMPRespBundle
import xiangshan.backend.rob.DebugLsInfoBundle
import xiangshan.cache.mmu.{TlbCmd, TlbReq, TlbRequestIO, TlbResp}
import xiangshan.cache.{DcacheStoreRequestIO, DCacheStoreIO, MemoryOpConstants, HasDCacheParameters, StorePrefetchReq}

class StoreAddrUnit(implicit p: Parameters) extends XSModule with HasDCacheParameters {
  val io = IO(new Bundle() {
    val redirect        = Flipped(ValidIO(new Redirect))
    val stin            = Flipped(Decoupled(new ExuInput))
    val issue           = Valid(new ExuInput)
    val tlb             = new TlbRequestIO()
    val dcache          = new DCacheStoreIO
    val pmp             = Flipped(new PMPRespBundle())
    val rsIdx           = Input(UInt(log2Up(IssQueSize).W))
    val isFirstIssue    = Input(Bool())
    val lsq             = ValidIO(new LsPipelineBundle)
    val lsq_replenish   = Output(new LsPipelineBundle())
    val feedback_slow   = ValidIO(new RSFeedback)
    val prefetch_req    = Flipped(DecoupledIO(new StorePrefetchReq))
    // provide prefetch info to sms
    val prefetch_train  = ValidIO(new StPrefetchTrainBundle())
    val stld_nuke_query = new StoreNukeQueryIO
    val stout           = DecoupledIO(new ExuOutput) // writeback store
    // store mask, send to sq in store_s0
    val st_mask_out     = Valid(new StoreMaskBundle)
    val debug_ls        = Output(new DebugLsInfoBundle)
  })

  val s1_ready, s2_ready, s3_ready, s4_ready = WireInit(false.B)

  // Pipeline
  // --------------------------------------------------------------------------------
  // stage 0
  // --------------------------------------------------------------------------------
  // generate addr, use addr to query DCache and DTLB
  val s0_iss_valid    = io.stin.valid
  val s0_prf_valid    = io.prefetch_req.valid && io.dcache.req.ready
  val s0_valid        = s0_iss_valid || s0_prf_valid
  val s0_use_flow_rs  = s0_iss_valid
  val s0_use_flow_prf = !s0_iss_valid && s0_prf_valid
  val s0_in           = Mux(s0_use_flow_rs, io.stin.bits, 0.U.asTypeOf(io.stin.bits))
  val s0_isFirstIssue = Mux(s0_use_flow_rs, io.isFirstIssue, false.B)
  val s0_rsIdx        = Mux(s0_use_flow_rs, io.rsIdx, 0.U)
  val s0_size         = Mux(s0_use_flow_rs, LSUOpType.size(s0_in.uop.ctrl.fuOpType), 3.U)
  val s0_mem_idx      = Mux(s0_use_flow_rs, s0_in.uop.sqIdx.value, 0.U)
  val s0_rob_idx      = Mux(s0_use_flow_rs, s0_in.uop.robIdx, 0.U.asTypeOf(s0_in.uop.robIdx))
  val s0_pc           = Mux(s0_use_flow_rs, s0_in.uop.cf.pc, 0.U)
  val s0_instr_type   = Mux(s0_use_flow_rs, STORE_SOURCE.U, DCACHE_PREFETCH_SOURCE.U)
  val s0_wlineflag    = Mux(s0_use_flow_rs, s0_in.uop.ctrl.fuOpType === LSUOpType.cbo_zero, false.B)
  val s0_can_go       = s1_ready
  val s0_fire         = s0_valid && s0_can_go

  // generate addr
  val s0_saddr = io.stin.bits.src(0) + SignExt(io.stin.bits.uop.ctrl.imm(11,0), VAddrBits)
  val s0_vaddr = Mux(s0_use_flow_rs, s0_saddr, io.prefetch_req.bits.vaddr)
  val s0_mask  = Mux(s0_use_flow_rs, genVWmask(s0_saddr, s0_in.uop.ctrl.fuOpType(1,0)), 3.U)

  io.st_mask_out.valid       := s0_use_flow_rs
  io.st_mask_out.bits.mask   := s0_mask
  io.st_mask_out.bits.sqIdx  := s0_in.uop.sqIdx

  io.stin.ready := s1_ready
  io.prefetch_req.ready := s1_ready && io.dcache.req.ready && !s0_iss_valid
  // Pipeline
  // --------------------------------------------------------------------------------
  // stage 1
  // --------------------------------------------------------------------------------
  val s1_valid  = RegInit(false.B)
  val s1_in     = RegEnable(s0_in, s0_fire)
  val s1_out    = Wire(new LsPipelineBundle)
  val s1_use_flow_rs  = RegEnable(s0_use_flow_prf, s0_fire)
  val s1_use_flow_prf = RegEnable(s0_use_flow_prf, s0_fire)
  val s1_isFirstIssue = RegEnable(s0_isFirstIssue, s0_fire)
  val s1_rsIdx        = RegEnable(s0_rsIdx, s0_fire)
  val s1_size         = RegEnable(s0_size, s0_fire)
  val s1_mem_idx      = RegEnable(s0_mem_idx, s0_fire)
  val s1_rob_idx      = RegEnable(s0_rob_idx, s0_fire)
  val s1_pc           = RegEnable(s0_pc, s0_fire)
  val s1_instr_type   = RegEnable(s0_instr_type, s0_fire)
  val s1_wlineflag    = RegEnable(s0_wlineflag, s0_fire)
  val s1_vaddr  = RegEnable(s0_vaddr, s0_fire)
  val s1_mask   = RegEnable(s0_mask, s0_fire)
  val s1_kill   = Wire(Bool())
  val s1_can_go = s2_ready
  val s1_fire   = s1_valid && !s1_kill && s1_can_go

  s1_kill := s1_in.uop.robIdx.needFlush(io.redirect) || s1_in.uop.robIdx.needFlush(RegNext(io.redirect))
  s1_ready := true.B
  io.tlb.resp.ready := true.B // TODO: why dtlbResp needs a ready?
  when (s0_fire) { s1_valid := true.B }
  .elsewhen (s1_fire) { s1_valid := false.B }
  .elsewhen (s1_kill) { s1_valid := false.B }

  io.tlb.req.valid                   := s1_valid
  io.tlb.req.bits.vaddr              := s1_vaddr
  io.tlb.req.bits.cmd                := TlbCmd.write
  io.tlb.req.bits.size               := s1_size
  io.tlb.req.bits.kill               := false.B
  io.tlb.req.bits.memidx.is_ld       := false.B
  io.tlb.req.bits.memidx.is_st       := true.B
  io.tlb.req.bits.memidx.idx         := s1_mem_idx
  io.tlb.req.bits.debug.robIdx       := s1_rob_idx
  io.tlb.req.bits.no_translate       := false.B
  io.tlb.req.bits.debug.pc           := s1_pc
  io.tlb.req.bits.debug.isFirstIssue := s1_isFirstIssue
  io.tlb.req_kill                    := s1_kill

  // Dcache access here: not **real** dcache write
  // just read meta and tag in dcache, to find out the store will hit or miss

  // NOTE: The store request does not wait for the dcache to be ready.
  //       If the dcache is not ready at this time, the dcache is not queried.
  //       But, store prefetch request will always wait for dcache to be ready to make progress.
  io.dcache.req.valid              := s1_fire
  io.dcache.req.bits.cmd           := MemoryOpConstants.M_PFW
  io.dcache.req.bits.vaddr         := s1_vaddr
  io.dcache.req.bits.instrtype     := s1_instr_type

  s1_out              := DontCare
  s1_out.vaddr        := s1_vaddr
  // Now data use its own io
  // s1_out.data := genWdata(s1_in.src(1), s1_in.uop.ctrl.fuOpType(1,0))
  s1_out.data         := s1_in.src(1) // FIXME: remove data from pipeline
  s1_out.uop          := s1_in.uop
  s1_out.miss         := false.B
  s1_out.rsIdx        := s1_rsIdx
  s1_out.mask         := s1_mask
  s1_out.isFirstIssue := s1_isFirstIssue
  s1_out.isHWPrefetch := s1_use_flow_prf
  s1_out.wlineflag    := s1_wlineflag
  when(s1_valid && s1_isFirstIssue) {
    s1_out.uop.debugInfo.tlbFirstReqTime := GTimer()
  }

  // exception check
  val s1_addr_aligned = LookupTree(s1_in.uop.ctrl.fuOpType(1,0), List(
    "b00".U   -> true.B,              //b
    "b01".U   -> (s1_out.vaddr(0) === 0.U),   //h
    "b10".U   -> (s1_out.vaddr(1,0) === 0.U), //w
    "b11".U   -> (s1_out.vaddr(2,0) === 0.U)  //d
  ))
  s1_out.uop.cf.exceptionVec(storeAddrMisaligned) := Mux(s1_use_flow_rs, !s1_addr_aligned, false.B)

  // Pipeline
  // --------------------------------------------------------------------------------
  // stage 2
  // --------------------------------------------------------------------------------
  // TLB resp (send paddr to dcache)
  val s2_valid  = RegInit(false.B)
  val s2_in     = RegEnable(s1_out, s1_fire)
  val s2_out    = Wire(new LsPipelineBundle)
  val s2_kill   = Wire(Bool())
  val s2_can_go = s3_ready
  val s2_fire   = s2_valid && !s2_kill && s2_can_go

  // mmio cbo decoder
  val s2_mmio_cbo  = s2_in.uop.ctrl.fuOpType === LSUOpType.cbo_clean ||
                     s2_in.uop.ctrl.fuOpType === LSUOpType.cbo_flush ||
                     s2_in.uop.ctrl.fuOpType === LSUOpType.cbo_inval
  val s2_paddr     = io.tlb.resp.bits.paddr(0)
  val s2_tlb_miss  = io.tlb.resp.bits.miss
  val s2_mmio      = s2_mmio_cbo
  val s2_exception = ExceptionNO.selectByFu(s2_out.uop.cf.exceptionVec, staCfg).asUInt.orR
  val s2_amo       = FuType.storeIsAMO(s2_in.uop.ctrl.fuType)
  s2_kill := s2_in.uop.robIdx.needFlush(io.redirect) || s2_tlb_miss || s2_amo

  s2_ready := true.B
  io.tlb.resp.ready := true.B // TODO: why dtlbResp needs a ready?
  when (s1_fire) { s2_valid := true.B }
  .elsewhen (s2_fire) { s2_valid := false.B }
  .elsewhen (s2_kill) { s2_valid := false.B }

  // issue
  io.issue.valid := s2_valid && !s2_tlb_miss && !s2_in.isHWPrefetch
  io.issue.bits  := RegEnable(s1_in, s1_valid)


  // Send TLB feedback to store issue queue
  // Store feedback is generated in store_s1, sent to RS in store_s2
  val s2_feedback = Wire(Valid(new RSFeedback))
  s2_feedback.valid                 := s2_valid & !s2_in.isHWPrefetch && !s2_amo
  s2_feedback.bits.hit              := !s2_tlb_miss
  s2_feedback.bits.flushState       := io.tlb.resp.bits.ptwBack
  s2_feedback.bits.rsIdx            := s2_out.rsIdx
  s2_feedback.bits.sourceType       := RSFeedbackType.tlbMiss
  s2_feedback.bits.dataInvalidSqIdx := DontCare
  XSDebug(s2_feedback.valid,
    "s2 Store: tlbHit: %d robIdx: %d\n",
    s2_feedback.bits.hit,
    s2_feedback.bits.rsIdx
  )

  io.feedback_slow := s2_feedback

  // get paddr from dtlb, check if rollback is needed
  // writeback store inst to lsq
  s2_out         := s2_in
  s2_out.paddr   := s2_paddr
  s2_out.miss    := false.B
  s2_out.mmio    := s2_mmio
  s2_out.tlbMiss := s2_tlb_miss
  s2_out.atomic  := s2_mmio
  s2_out.uop.cf.exceptionVec(storePageFault)   := io.tlb.resp.bits.excp(0).pf.st
  s2_out.uop.cf.exceptionVec(storeAccessFault) := io.tlb.resp.bits.excp(0).af.st

  io.lsq.valid     := s2_valid && !s2_in.isHWPrefetch && !s2_amo
  io.lsq.bits      := s2_out
  io.lsq.bits.miss := s2_tlb_miss

  // kill dcache write intent request when tlb miss or exception
  io.dcache.s1_kill  := (s2_kill || s2_exception || s2_mmio)
  io.dcache.s1_paddr := s2_paddr

  // write below io.out.bits assign sentence to prevent overwriting values
  val s2_tlb_memidx = io.tlb.resp.bits.memidx
  when(s2_tlb_memidx.is_st && io.tlb.resp.valid && !s2_tlb_miss && s2_tlb_memidx.idx === s2_out.uop.sqIdx.value) {
    // printf("Store idx = %d\n", s2_tlb_memidx.idx)
    s2_out.uop.debugInfo.tlbRespTime := GTimer()
  }

  // Pipeline
  // --------------------------------------------------------------------------------
  // stage 3
  // --------------------------------------------------------------------------------
  // mmio check
  val s3_valid  = RegInit(false.B)
  val s3_in     = RegEnable(s2_out, s2_fire)
  val s3_out    = Wire(new LsPipelineBundle)
  val s3_kill   = Wire(Bool())
  val s3_can_go = s4_ready
  val s3_fire   = s3_valid && !s3_kill && s3_can_go

  s3_ready := true.B
  when (s2_fire) { s3_valid := true.B }
  .elsewhen (s3_fire) { s3_valid := false.B }
  .elsewhen (s3_kill) { s3_valid := false.B }

  val s3_pmp = WireInit(io.pmp)

  val s3_exception = ExceptionNO.selectByFu(s3_out.uop.cf.exceptionVec, staCfg).asUInt.orR
  val s3_mmio = s3_in.mmio || s3_pmp.mmio
  s3_kill := (s3_mmio && !s3_exception) || s3_in.uop.robIdx.needFlush(io.redirect)

  s3_out        := s3_in
  s3_out.mmio   := s3_mmio && !s3_exception
  s3_out.atomic := s3_in.atomic || s3_pmp.atomic
  s3_out.uop.cf.exceptionVec(storeAccessFault) := s3_in.uop.cf.exceptionVec(storeAccessFault) || s3_pmp.st

  // kill dcache write intent request when mmio or exception
  io.dcache.s2_kill := (s3_mmio || s3_exception || s3_in.uop.robIdx.needFlush(io.redirect))
  io.dcache.s2_pc   := s3_out.uop.cf.pc
  // TODO: dcache resp
  io.dcache.resp.ready := true.B

  // mmio and exception
  io.lsq_replenish := s3_out

  // prefetch related
  io.lsq_replenish.miss := io.dcache.resp.fire && io.dcache.resp.bits.miss // miss info

  // st-ld violation dectect request.
  io.stld_nuke_query.valid  := s3_valid && !s3_in.isHWPrefetch
  io.stld_nuke_query.robIdx := s3_in.uop.robIdx
  io.stld_nuke_query.paddr  := s3_in.paddr
  io.stld_nuke_query.mask   := s3_in.mask

  // RegNext prefetch train for better timing
  // ** Now, prefetch train is valid at store s4 **
  io.prefetch_train.bits.fromLsPipelineBundle(s3_in, latch = true)
  // override miss bit
  io.prefetch_train.bits.miss := RegNext(io.dcache.resp.bits.miss)
  // TODO: add prefetch and access bit
  io.prefetch_train.bits.meta_prefetch := false.B
  io.prefetch_train.bits.meta_access := false.B
  if(EnableStorePrefetchSMS) {
    io.prefetch_train.valid := RegNext(s3_valid && io.dcache.resp.fire && !s3_out.mmio && !s3_in.tlbMiss && !s3_in.isHWPrefetch)
  }else {
    io.prefetch_train.valid := false.B
  }

  // Pipeline
  // --------------------------------------------------------------------------------
  // stage 4
  // --------------------------------------------------------------------------------
  // store write back
  val s4_valid  = RegInit(false.B)
  val s4_in     = RegEnable(s3_out, s3_fire)
  val s4_out    = Wire(new ExuOutput)
  val s4_kill   = s4_in.uop.robIdx.needFlush(io.redirect)
  val s4_can_go = io.stout.ready
  val s4_fire   = s4_valid && !s4_kill && s4_can_go
  val s4_bad_nuke_detected = io.stld_nuke_query.nuke

  when (s3_fire) { s4_valid := (!s3_mmio || s3_exception) && !s3_out.isHWPrefetch  }
  .elsewhen (s4_fire) { s4_valid := false.B }
  .elsewhen (s4_kill) { s4_valid := false.B }

  // wb: writeback
  s4_out                     := DontCare
  s4_out.uop                 := s4_in.uop
  s4_out.data                := DontCare
  s4_out.redirectValid       := false.B
  s4_out.redirect            := DontCare
  s4_out.debug.isMMIO        := s4_in.mmio
  s4_out.debug.paddr         := s4_in.paddr
  s4_out.debug.vaddr         := s4_in.vaddr
  s4_out.debug.isPerfCnt     := false.B
  s4_out.fflags              := DontCare

  // Pipeline
  // --------------------------------------------------------------------------------
  // stage 5 (like skid buffer)
  // --------------------------------------------------------------------------------
  // store write back
  val s5_valid  = RegInit(false.B)
  val s5_in     = RegEnable(s4_out, s4_fire)
  val s5_out    = s5_in
  val s5_kill   = s5_in.uop.robIdx.needFlush(io.redirect)
  val s5_can_go = io.stout.ready
  val s5_real_valid = s5_valid && !s5_kill
  val s5_fire   = s5_real_valid && s5_can_go

  when (s4_fire) { s5_valid := true.B & (s4_bad_nuke_detected || s5_real_valid) } // when detected bad nuke at s4 or skid buffer is full, into skip buffer
  .elsewhen (s5_fire) { s5_valid := false.B }
  .elsewhen (s5_kill) { s5_valid := false.B }

  s4_ready := Mux(s5_real_valid || s4_bad_nuke_detected, true.B, io.stout.ready)
  io.stout.valid := s5_real_valid || (s4_valid && !s4_bad_nuke_detected)
  io.stout.bits := Mux(s5_real_valid, s5_out, s4_out)


  io.debug_ls := DontCare
  io.debug_ls.s1.isTlbFirstMiss := io.tlb.resp.valid && io.tlb.resp.bits.miss && io.tlb.resp.bits.debug.isFirstIssue && !s2_in.isHWPrefetch
  io.debug_ls.s1_robIdx := s2_in.uop.robIdx.value

  private def printPipeLine(pipeline: LsPipelineBundle, cond: Bool, name: String): Unit = {
    XSDebug(cond,
      p"$name" + p" pc ${Hexadecimal(pipeline.uop.cf.pc)} " +
        p"addr ${Hexadecimal(pipeline.vaddr)} -> ${Hexadecimal(pipeline.paddr)} " +
        p"op ${Binary(pipeline.uop.ctrl.fuOpType)} " +
        p"data ${Hexadecimal(pipeline.data)} " +
        p"mask ${Hexadecimal(pipeline.mask)}\n"
    )
  }

  printPipeLine(s1_out, s1_valid, "S1")
  printPipeLine(s2_out, s2_valid, "S2")

  // perf cnt
  XSPerfAccumulate("s0_in_valid",                s0_valid)
  XSPerfAccumulate("s0_in_fire",                 s0_fire)
  XSPerfAccumulate("s0_in_fire_first_issue",     s0_fire && s0_isFirstIssue)
  XSPerfAccumulate("s0_addr_spec_success",       s0_fire && s0_saddr(VAddrBits-1, 12) === s0_in.src(0)(VAddrBits-1, 12))
  XSPerfAccumulate("s0_addr_spec_failed",        s0_fire && s0_saddr(VAddrBits-1, 12) =/= s0_in.src(0)(VAddrBits-1, 12))
  XSPerfAccumulate("s0_addr_spec_success_once",  s0_fire && s0_saddr(VAddrBits-1, 12) === s0_in.src(0)(VAddrBits-1, 12) && s0_isFirstIssue)
  XSPerfAccumulate("s0_addr_spec_failed_once",   s0_fire && s0_saddr(VAddrBits-1, 12) =/= s0_in.src(0)(VAddrBits-1, 12) && s0_isFirstIssue)

  XSPerfAccumulate("s2_in_valid",                s2_valid)
  XSPerfAccumulate("s2_in_fire",                 s2_fire)
  XSPerfAccumulate("s2_in_fire_first_issue",     s2_fire && s2_in.isFirstIssue)
  XSPerfAccumulate("s2_tlb_miss",                s2_fire && s2_tlb_miss)
  XSPerfAccumulate("s2_tlb_miss_first_issue",    s2_fire && s2_tlb_miss && s2_in.isFirstIssue)
  // end
}