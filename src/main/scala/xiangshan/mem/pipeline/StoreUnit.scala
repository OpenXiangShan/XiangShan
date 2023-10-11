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
import xiangshan.backend.Bundles.{MemExuInput, MemExuOutput}
import xiangshan.backend.fu.PMPRespBundle
import xiangshan.backend.fu.FuConfig._
import xiangshan.backend.ctrlblock.DebugLsInfoBundle
import xiangshan.cache.mmu.{TlbCmd, TlbReq, TlbRequestIO, TlbResp}
import xiangshan.cache.{DcacheStoreRequestIO, DCacheStoreIO, MemoryOpConstants, HasDCacheParameters, StorePrefetchReq}

class StoreUnit(implicit p: Parameters) extends XSModule with HasDCacheParameters {
  val io = IO(new Bundle() {
    val redirect        = Flipped(ValidIO(new Redirect))
    val stin            = Flipped(Decoupled(new MemExuInput))
    val issue           = Valid(new MemExuInput)
    val tlb             = new TlbRequestIO()
    val dcache          = new DCacheStoreIO
    val pmp             = Flipped(new PMPRespBundle())
    val lsq             = ValidIO(new LsPipelineBundle)
    val lsq_replenish   = Output(new LsPipelineBundle())
    val feedback_slow   = ValidIO(new RSFeedback)
    val prefetch_req    = Flipped(DecoupledIO(new StorePrefetchReq))
    // provide prefetch info to sms
    val prefetch_train  = ValidIO(new StPrefetchTrainBundle())
    val stld_nuke_query = Valid(new StoreNukeQueryIO)
    val stout           = DecoupledIO(new MemExuOutput) // writeback store
    // store mask, send to sq in store_s0
    val st_mask_out     = Valid(new StoreMaskBundle)
    val debug_ls        = Output(new DebugLsInfoBundle)
  })

  val s1_ready, s2_ready, s3_ready = WireInit(false.B)

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
  val s0_isFirstIssue = Mux(s0_use_flow_rs, io.stin.bits.isFirstIssue, false.B)
  val s0_rsIdx        = Mux(s0_use_flow_rs, io.stin.bits.iqIdx, 0.U)
  val s0_size         = Mux(s0_use_flow_rs, LSUOpType.size(s0_in.uop.ctrl.fuOpType), 3.U)
  val s0_mem_idx      = Mux(s0_use_flow_rs, s0_in.uop.sqIdx.value, 0.U)
  val s0_rob_idx      = Mux(s0_use_flow_rs, s0_in.uop.robIdx, 0.U.asTypeOf(s0_in.uop.robIdx))
  val s0_pc           = Mux(s0_use_flow_rs, s0_in.uop.pc, 0.U)
  val s0_instr_type   = Mux(s0_use_flow_rs, STORE_SOURCE.U, DCACHE_PREFETCH_SOURCE.U)
  val s0_wlineflag    = Mux(s0_use_flow_rs, s0_in.uop.fuOpType === LSUOpType.cbo_zero, false.B)
  val s0_out          = Wire(new LsPipelineBundle)
  val s0_kill         = s0_in.uop.robIdx.needFlush(io.redirect)
  val s0_can_go       = s1_ready
  val s0_fire         = s0_valid && !s0_kill && s0_can_go

  // generate addr
  // val saddr = s0_in.bits.src(0) + SignExt(s0_in.bits.uop.imm(11,0), VAddrBits)
  val imm12 = WireInit(s0_in.uop.imm(11,0))
  val saddr_lo = s0_in.src(0)(11,0) + Cat(0.U(1.W), imm12)
  val saddr_hi = Mux(saddr_lo(12),
    Mux(imm12(11), s0_in.src(0)(VAddrBits-1, 12), s0_in.src(0)(VAddrBits-1, 12)+1.U),
    Mux(imm12(11), s0_in.src(0)(VAddrBits-1, 12)+SignExt(1.U, VAddrBits-12), s0_in.src(0)(VAddrBits-1, 12)),
  )
  val s0_saddr = Cat(saddr_hi, saddr_lo(11,0))
  val s0_vaddr = Mux(s0_use_flow_rs, s0_saddr, io.prefetch_req.bits.vaddr)
  val s0_mask  = Mux(s0_use_flow_rs, genVWmask(s0_saddr, s0_in.uop.ctrl.fuOpType(1,0)), 3.U)

  io.tlb.req.valid                   := s0_valid
  io.tlb.req.bits.vaddr              := s0_vaddr
  io.tlb.req.bits.cmd                := TlbCmd.write
  io.tlb.req.bits.size               := s0_size
  io.tlb.req.bits.kill               := false.B
  io.tlb.req.bits.memidx.is_ld       := false.B
  io.tlb.req.bits.memidx.is_st       := true.B
  io.tlb.req.bits.memidx.idx         := s0_mem_idx
  io.tlb.req.bits.debug.robIdx       := s0_rob_idx
  io.tlb.req.bits.no_translate       := false.B
  io.tlb.req.bits.debug.pc           := s0_pc
  io.tlb.req.bits.debug.isFirstIssue := s0_isFirstIssue
  io.tlb.req_kill                    := false.B

  // Dcache access here: not **real** dcache write
  // just read meta and tag in dcache, to find out the store will hit or miss

  // NOTE: The store request does not wait for the dcache to be ready.
  //       If the dcache is not ready at this time, the dcache is not queried.
  //       But, store prefetch request will always wait for dcache to be ready to make progress.
  io.dcache.req.valid              := s0_fire
  io.dcache.req.bits.cmd           := MemoryOpConstants.M_PFW
  io.dcache.req.bits.vaddr         := s0_vaddr
  io.dcache.req.bits.instrtype     := s0_instr_type

  s0_out              := DontCare
  s0_out.vaddr        := s0_vaddr
  // Now data use its own io
  // s1_out.data := genWdata(s1_in.src(1), s1_in.uop.fuOpType(1,0))
  s0_out.data         := s0_in.src(1) // FIXME: remove data from pipeline
  s0_out.uop          := s0_in.uop
  s0_out.miss         := false.B
  s0_out.rsIdx        := s0_rsIdx
  s0_out.mask         := s0_mask
  s0_out.isFirstIssue := s0_isFirstIssue
  s0_out.isHWPrefetch := s0_use_flow_prf
  s0_out.wlineflag    := s0_wlineflag
  when(s0_valid && s0_isFirstIssue) {
    s0_out.uop.debugInfo.tlbFirstReqTime := GTimer()
  }

  // exception check
  val s0_addr_aligned = LookupTree(s0_in.uop.fuOpType(1,0), List(
    "b00".U   -> true.B,              //b
    "b01".U   -> (s0_out.vaddr(0) === 0.U),   //h
    "b10".U   -> (s0_out.vaddr(1,0) === 0.U), //w
    "b11".U   -> (s0_out.vaddr(2,0) === 0.U)  //d
  ))
  s0_out.uop.exceptionVec(storeAddrMisaligned) := Mux(s0_use_flow_rs, !s0_addr_aligned, false.B)

  io.st_mask_out.valid       := s0_use_flow_rs
  io.st_mask_out.bits.mask   := s0_out.mask
  io.st_mask_out.bits.sqIdx  := s0_out.uop.sqIdx

  io.stin.ready := s1_ready
  io.prefetch_req.ready := s1_ready && io.dcache.req.ready && !s0_iss_valid

  // Pipeline
  // --------------------------------------------------------------------------------
  // stage 1
  // --------------------------------------------------------------------------------
  // TLB resp (send paddr to dcache)
  val s1_valid  = RegInit(false.B)
  val s1_in     = RegEnable(s0_out, s0_fire)
  val s1_out    = Wire(new LsPipelineBundle)
  val s1_kill   = Wire(Bool())
  val s1_can_go = s2_ready
  val s1_fire   = s1_valid && !s1_kill && s1_can_go

  // mmio cbo decoder
  val s1_mmio_cbo  = s1_in.uop.fuOpType === LSUOpType.cbo_clean ||
                     s1_in.uop.fuOpType === LSUOpType.cbo_flush ||
                     s1_in.uop.fuOpType === LSUOpType.cbo_inval
  val s1_paddr     = io.tlb.resp.bits.paddr(0)
  val s1_tlb_miss  = io.tlb.resp.bits.miss
  val s1_mmio      = s1_mmio_cbo
  val s1_exception = ExceptionNO.selectByFu(s1_out.uop.exceptionVec, StaCfg).asUInt.orR
  s1_kill := s1_in.uop.robIdx.needFlush(io.redirect) || s1_tlb_miss

  s1_ready := !s1_valid || s1_kill || s2_ready
  io.tlb.resp.ready := true.B // TODO: why dtlbResp needs a ready?
  when (s0_fire) { s1_valid := true.B }
  .elsewhen (s1_fire) { s1_valid := false.B }
  .elsewhen (s1_kill) { s1_valid := false.B }

  // st-ld violation dectect request.
  io.stld_nuke_query.valid       := s1_valid && !s1_tlb_miss && !s1_in.isHWPrefetch
  io.stld_nuke_query.bits.robIdx := s1_in.uop.robIdx
  io.stld_nuke_query.bits.paddr  := s1_paddr
  io.stld_nuke_query.bits.mask   := s1_in.mask

  // issue
  io.issue.valid := s1_valid && !s1_tlb_miss && !s1_in.isHWPrefetch
  io.issue.bits  := RegEnable(s0_in, s0_valid)


  // Send TLB feedback to store issue queue
  // Store feedback is generated in store_s1, sent to RS in store_s2
  val s1_feedback = Wire(Valid(new RSFeedback))
  s1_feedback.valid                 := s1_valid & !s1_in.isHWPrefetch
  s1_feedback.bits.hit              := !s1_tlb_miss
  s1_feedback.bits.flushState       := io.tlb.resp.bits.ptwBack
  s1_feedback.bits.robIdx           := s1_out.uop.robIdx
  s1_feedback.bits.sourceType       := RSFeedbackType.tlbMiss
  s1_feedback.bits.dataInvalidSqIdx := DontCare

  XSDebug(s1_feedback.valid,
    "S1 Store: tlbHit: %d robIdx: %d\n",
    s1_feedback.bits.hit,
    s1_feedback.bits.robIdx.value
  )

  io.feedback_slow := s1_feedback

  // get paddr from dtlb, check if rollback is needed
  // writeback store inst to lsq
  s1_out         := s1_in
  s1_out.paddr   := s1_paddr
  s1_out.miss    := false.B
  s1_out.mmio    := s1_mmio
  s1_out.tlbMiss := s1_tlb_miss
  s1_out.atomic  := s1_mmio
  s1_out.uop.exceptionVec(storePageFault)   := io.tlb.resp.bits.excp(0).pf.st
  s1_out.uop.exceptionVec(storeAccessFault) := io.tlb.resp.bits.excp(0).af.st

  io.lsq.valid     := s1_valid && !s1_in.isHWPrefetch
  io.lsq.bits      := s1_out
  io.lsq.bits.miss := s1_tlb_miss

  // kill dcache write intent request when tlb miss or exception
  io.dcache.s1_kill  := (s1_tlb_miss || s1_exception || s1_mmio || s1_in.uop.robIdx.needFlush(io.redirect))
  io.dcache.s1_paddr := s1_paddr

  // write below io.out.bits assign sentence to prevent overwriting values
  val s1_tlb_memidx = io.tlb.resp.bits.memidx
  when(s1_tlb_memidx.is_st && io.tlb.resp.valid && !s1_tlb_miss && s1_tlb_memidx.idx === s1_out.uop.sqIdx.value) {
    // printf("Store idx = %d\n", s1_tlb_memidx.idx)
    s1_out.uop.debugInfo.tlbRespTime := GTimer()
  }

  // Pipeline
  // --------------------------------------------------------------------------------
  // stage 2
  // --------------------------------------------------------------------------------
  // mmio check
  val s2_valid  = RegInit(false.B)
  val s2_in     = RegEnable(s1_out, s1_fire)
  val s2_out    = Wire(new LsPipelineBundle)
  val s2_kill   = Wire(Bool())
  val s2_can_go = s3_ready
  val s2_fire   = s2_valid && !s2_kill && s2_can_go

  s2_ready := !s2_valid || s2_kill || s3_ready
  when (s1_fire) { s2_valid := true.B }
  .elsewhen (s2_fire) { s2_valid := false.B }
  .elsewhen (s2_kill) { s2_valid := false.B }

  val s2_pmp = WireInit(io.pmp)

  val s2_exception = ExceptionNO.selectByFu(s2_out.uop.exceptionVec, StaCfg).asUInt.orR
  val s2_mmio = s2_in.mmio || s2_pmp.mmio
  s2_kill := (s2_mmio && !s2_exception) || s2_in.uop.robIdx.needFlush(io.redirect)

  s2_out        := s2_in
  s2_out.mmio   := s2_mmio && !s2_exception
  s2_out.atomic := s2_in.atomic || s2_pmp.atomic
  s2_out.uop.exceptionVec(storeAccessFault) := s2_in.uop.exceptionVec(storeAccessFault) || s2_pmp.st

  // kill dcache write intent request when mmio or exception
  io.dcache.s2_kill := (s2_mmio || s2_exception || s2_in.uop.robIdx.needFlush(io.redirect))
  io.dcache.s2_pc   := s2_out.uop.cf.pc
  // TODO: dcache resp
  io.dcache.resp.ready := true.B

  // feedback tlb miss to RS in store_s2
  io.feedback_slow.valid := RegNext(s1_feedback.valid && !s1_out.uop.robIdx.needFlush(io.redirect))
  io.feedback_slow.bits  := RegNext(s1_feedback.bits)

  // mmio and exception
  io.lsq_replenish := s2_out

  // prefetch related
  io.lsq_replenish.miss := io.dcache.resp.fire && io.dcache.resp.bits.miss // miss info

  io.prefetch_train.bits.fromLsPipelineBundle(s2_in)
  // override miss bit
  io.prefetch_train.bits.miss := io.dcache.resp.bits.miss
  // TODO: add prefetch and access bit
  io.prefetch_train.bits.meta_prefetch := false.B
  io.prefetch_train.bits.meta_access := false.B
  if(EnableStorePrefetchSMS) {
    io.prefetch_train.valid := s2_valid && io.dcache.resp.fire && !s2_out.mmio && !s2_in.tlbMiss && !s2_in.isHWPrefetch
  }else {
    io.prefetch_train.valid := false.B
  }

  // Pipeline
  // --------------------------------------------------------------------------------
  // stage 3
  // --------------------------------------------------------------------------------
  // store write back
  val s3_valid  = RegInit(false.B)
  val s3_in     = RegEnable(s2_out, s2_fire)
  val s3_out    = Wire(new MemExuOutput)
  val s3_kill   = s3_in.uop.robIdx.needFlush(io.redirect)
  val s3_can_go = s3_ready
  val s3_fire   = s3_valid && !s3_kill && s3_can_go

  when (s2_fire) { s3_valid := (!s2_mmio || s2_exception) && !s2_out.isHWPrefetch  }
  .elsewhen (s3_fire) { s3_valid := false.B }
  .elsewhen (s3_kill) { s3_valid := false.B }

  // wb: writeback
  val SelectGroupSize   = RollbackGroupSize
  val lgSelectGroupSize = log2Ceil(SelectGroupSize)
  val TotalSelectCycles = scala.math.ceil(log2Ceil(LoadQueueRAWSize).toFloat / lgSelectGroupSize).toInt + 1

  s3_out                 := DontCare
  s3_out.uop             := s3_in.uop
  s3_out.data            := DontCare
  s3_out.debug.isMMIO    := s3_in.mmio
  s3_out.debug.paddr     := s3_in.paddr
  s3_out.debug.vaddr     := s3_in.vaddr
  s3_out.debug.isPerfCnt := false.B

  // Pipeline
  // --------------------------------------------------------------------------------
  // stage x
  // --------------------------------------------------------------------------------
  // delay TotalSelectCycles - 2 cycle(s)
  val TotalDelayCycles = TotalSelectCycles - 2
  val sx_valid = Wire(Vec(TotalDelayCycles + 1, Bool()))
  val sx_ready = Wire(Vec(TotalDelayCycles + 1, Bool()))
  val sx_in    = Wire(Vec(TotalDelayCycles + 1, new MemExuOutput))

  // backward ready signal
  s3_ready := sx_ready.head
  for (i <- 0 until TotalDelayCycles + 1) {
    if (i == 0) {
      sx_valid(i) := s3_valid
      sx_in(i)    := s3_out
      sx_ready(i) := !s3_valid(i) || sx_in(i).uop.robIdx.needFlush(io.redirect) || (if (TotalDelayCycles == 0) io.stout.ready else sx_ready(i+1))
    } else {
      val cur_kill   = sx_in(i).uop.robIdx.needFlush(io.redirect)
      val cur_can_go = (if (i == TotalDelayCycles) io.stout.ready else sx_ready(i+1))
      val cur_fire   = sx_valid(i) && !cur_kill && cur_can_go
      val prev_fire  = sx_valid(i-1) && !sx_in(i-1).uop.robIdx.needFlush(io.redirect) && sx_ready(i)

      sx_ready(i) := !sx_valid(i) || cur_kill || (if (i == TotalDelayCycles) io.stout.ready else sx_ready(i+1))
      val sx_valid_can_go = prev_fire || cur_fire || cur_kill
      sx_valid(i) := RegEnable(Mux(prev_fire, true.B, false.B), false.B, sx_valid_can_go)
      sx_in(i) := RegEnable(sx_in(i-1), prev_fire)
    }
  }
  val sx_last_valid = sx_valid.takeRight(1).head
  val sx_last_ready = sx_ready.takeRight(1).head
  val sx_last_in    = sx_in.takeRight(1).head
  sx_last_ready := !sx_last_valid || sx_last_in.uop.robIdx.needFlush(io.redirect) || io.stout.ready

  io.stout.valid := sx_last_valid && !sx_last_in.uop.robIdx.needFlush(io.redirect)
  io.stout.bits := sx_last_in
  io.stout.bits.redirectValid := false.B

  io.debug_ls := DontCare
  io.debug_ls.s1.isTlbFirstMiss := io.tlb.resp.valid && io.tlb.resp.bits.miss && io.tlb.resp.bits.debug.isFirstIssue && !s1_in.isHWPrefetch
  io.debug_ls.s1_robIdx := s1_in.uop.robIdx.value

  private def printPipeLine(pipeline: LsPipelineBundle, cond: Bool, name: String): Unit = {
    XSDebug(cond,
      p"$name" + p" pc ${Hexadecimal(pipeline.uop.pc)} " +
        p"addr ${Hexadecimal(pipeline.vaddr)} -> ${Hexadecimal(pipeline.paddr)} " +
        p"op ${Binary(pipeline.uop.fuOpType)} " +
        p"data ${Hexadecimal(pipeline.data)} " +
        p"mask ${Hexadecimal(pipeline.mask)}\n"
    )
  }

  printPipeLine(s0_out, s0_valid, "S0")
  printPipeLine(s1_out, s1_valid, "S1")

  // perf cnt
  XSPerfAccumulate("s0_in_valid",                s0_valid)
  XSPerfAccumulate("s0_in_fire",                 s0_fire)
  XSPerfAccumulate("s0_in_fire_first_issue",     s0_fire && s0_isFirstIssue)
  XSPerfAccumulate("s0_addr_spec_success",       s0_fire && s0_saddr(VAddrBits-1, 12) === s0_in.src(0)(VAddrBits-1, 12))
  XSPerfAccumulate("s0_addr_spec_failed",        s0_fire && s0_saddr(VAddrBits-1, 12) =/= s0_in.src(0)(VAddrBits-1, 12))
  XSPerfAccumulate("s0_addr_spec_success_once",  s0_fire && s0_saddr(VAddrBits-1, 12) === s0_in.src(0)(VAddrBits-1, 12) && s0_isFirstIssue)
  XSPerfAccumulate("s0_addr_spec_failed_once",   s0_fire && s0_saddr(VAddrBits-1, 12) =/= s0_in.src(0)(VAddrBits-1, 12) && s0_isFirstIssue)

  XSPerfAccumulate("s1_in_valid",                s1_valid)
  XSPerfAccumulate("s1_in_fire",                 s1_fire)
  XSPerfAccumulate("s1_in_fire_first_issue",     s1_fire && s1_in.isFirstIssue)
  XSPerfAccumulate("s1_tlb_miss",                s1_fire && s1_tlb_miss)
  XSPerfAccumulate("s1_tlb_miss_first_issue",    s1_fire && s1_tlb_miss && s1_in.isFirstIssue)
  // end
}