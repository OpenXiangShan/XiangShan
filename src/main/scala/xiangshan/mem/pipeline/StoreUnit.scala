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
import xiangshan.backend.rob.{DebugLsInfoBundle, RobPtr}
import xiangshan.cache.mmu.{TlbCmd, TlbReq, TlbRequestIO, TlbResp}

class StoreUnit(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val redirect          = Flipped(ValidIO(new Redirect))
    val stin              = Flipped(Decoupled(new ExuInput))
    val vecstin           = Flipped(Decoupled(new VecStorePipeBundle()))
    val issue             = Valid(new ExuInput)
    val tlb               = new TlbRequestIO()
    val pmp               = Flipped(new PMPRespBundle())
    val rsIdx             = Input(UInt(log2Up(IssQueSize).W))
    val isFirstIssue      = Input(Bool())
    val vec_isFirstIssue  = Input(Bool())
    val lsq               = ValidIO(new LsPipelineBundle)
    val lsq_replenish     = Output(new LsPipelineBundle())
    val feedback_slow     = ValidIO(new RSFeedback)
    val vec_feedback_slow = ValidIO(new VSFQFeedback)
    val stld_nuke_query   = Valid(new StoreNukeQueryIO)
    val stout             = DecoupledIO(new ExuOutput) // writeback store
    // store mask, send to sq in store_s0
    val st_mask_out       = Valid(new StoreMaskBundle)
    val debug_ls          = Output(new DebugLsInfoBundle)
  })

  val s1_ready, s2_ready, s3_ready = WireInit(false.B)

  // Pipeline
  // --------------------------------------------------------------------------------
  // stage 0
  // --------------------------------------------------------------------------------
  // generate addr, use addr to query DCache and DTLB
  val s0_out          = Wire(new LsPipelineBundle)
  val s0_valid        = Wire(Bool())
  val s0_vaddr        = Wire(UInt(VAddrBits.W))
  // TODO: VLSU, check it
  val s0_isFirstIssue = io.isFirstIssue || io.vec_isFirstIssue
  val s0_rsIdx        = Wire(UInt(log2Up(IssQueSize).W))
  val s0_kill         = s0_out.uop.robIdx.needFlush(io.redirect)
  val s0_can_go       = s1_ready
  val s0_fire         = s0_valid && !s0_kill && s0_can_go
  val s0_sqIdx        = WireInit(0.U.asTypeOf(new SqPtr))
  val s0_uop          = WireInit(0.U.asTypeOf(new MicroOp))
  val s0_mask         = WireInit(0.U((VLEN/8).W))
  // vector related ctrl signal
  val s0_isvec        = WireInit(false.B)
  val s0_is128bit     = WireInit(false.B)
  val s0_exp          = WireInit(true.B)
  // val s0_fqidx        = WireInit(0.U(log2Ceil(VsFlowSize).W))
  val s0_flowPtr      = WireInit(0.U.asTypeOf(new VsFlowPtr))

  val s0_int_iss_valid = io.stin.valid
  val s0_vec_iss_valid = io.vecstin.valid
  dontTouch(s0_int_iss_valid)
  dontTouch(s0_vec_iss_valid)

  val s0_int_iss_ready = WireInit(true.B)
  val s0_vec_iss_ready = !s0_int_iss_valid

  val s0_int_iss_select = s0_int_iss_ready && s0_int_iss_valid
  val s0_vec_iss_select = s0_vec_iss_ready && s0_vec_iss_valid

  s0_valid := io.stin.valid || io.vecstin.valid

  // generate addr for int store
  // val saddr = s0_in.bits.src(0) + SignExt(s0_in.bits.uop.ctrl.imm(11,0), VAddrBits)
  val imm12 = WireInit(io.stin.bits.uop.ctrl.imm(11,0))
  val saddr_lo = io.stin.bits.src(0)(11,0) + Cat(0.U(1.W), imm12)
  val saddr_hi = Mux(saddr_lo(12),
    Mux(imm12(11), io.stin.bits.src(0)(VAddrBits-1, 12), io.stin.bits.src(0)(VAddrBits-1, 12) + 1.U),
    Mux(imm12(11), io.stin.bits.src(0)(VAddrBits-1, 12) + SignExt(1.U, VAddrBits-12), io.stin.bits.src(0)(VAddrBits-1, 12)),
  )
  val s0_saddr = Cat(saddr_hi, saddr_lo(11,0))

  def fromNullSource() = {
    s0_vaddr         := 0.U
    s0_mask          := 0.U
    s0_uop           := 0.U.asTypeOf(new MicroOp)
    s0_sqIdx         := 0.U.asTypeOf(new SqPtr)
    s0_rsIdx         := 0.U
  }

  def fromIntIssueSource(src: ExuInput) = {
    s0_vaddr         := s0_saddr
    s0_mask          := genVWmask(s0_vaddr, src.uop.ctrl.fuOpType(1,0))
    s0_uop           := src.uop
    s0_sqIdx         := src.uop.sqIdx
    s0_rsIdx         := io.rsIdx
  }

  def fromVecIssueSource(src: VecStorePipeBundle) = {
    s0_vaddr        := src.vaddr
    s0_mask         := src.mask
    s0_uop          := src.uop
    s0_sqIdx        := src.uop.sqIdx
    // TODO: VLSU, implement vector feedback
    s0_rsIdx        := 0.U
    // Vector load interface
    s0_isvec        := true.B
    // TODO: VLSU, Store do not use 128 bits now?
    s0_is128bit     := false.B
    s0_exp          := src.exp
    // s0_fqidx        := src.fqIdx
    s0_flowPtr        := src.flowPtr
  }

  s0_uop := DontCare
  when (s0_int_iss_select)        { fromIntIssueSource(io.stin.bits)    }
    .elsewhen (s0_vec_iss_select) { fromVecIssueSource(io.vecstin.bits) }
    .otherwise                    { fromNullSource()                    }

  io.tlb.req.valid                   := s0_valid
  io.tlb.req.bits.vaddr              := s0_vaddr
  io.tlb.req.bits.cmd                := TlbCmd.write
  io.tlb.req.bits.size               := Mux(s0_isvec, io.vecstin.bits.alignedType, LSUOpType.size(s0_uop.ctrl.fuOpType))
  io.tlb.req.bits.kill               := DontCare
  io.tlb.req.bits.memidx.is_ld       := false.B
  io.tlb.req.bits.memidx.is_st       := true.B
  io.tlb.req.bits.memidx.idx         := s0_uop.sqIdx.value
  io.tlb.req.bits.debug.robIdx       := s0_uop.robIdx
  io.tlb.req.bits.no_translate       := false.B
  io.tlb.req.bits.debug.pc           := s0_uop.cf.pc
  io.tlb.req.bits.debug.isFirstIssue := s0_isFirstIssue
  io.tlb.req_kill                    := false.B

  s0_out              := DontCare
  s0_out.vaddr        := s0_vaddr
  s0_out.uop          := s0_uop
  s0_out.miss         := DontCare
  s0_out.rsIdx        := s0_rsIdx
  s0_out.mask         := s0_mask
  s0_out.isFirstIssue := s0_isFirstIssue
  s0_out.isHWPrefetch := false.B // TODO
  s0_out.wlineflag    := s0_uop.ctrl.fuOpType === LSUOpType.cbo_zero
  s0_out.isvec        := s0_isvec
  s0_out.is128bit     := s0_is128bit
  s0_out.exp          := s0_exp
  // s0_out.fqIdx        := s0_fqidx
  s0_out.flowPtr      := s0_flowPtr
  when(s0_valid && s0_isFirstIssue) {
    s0_out.uop.debugInfo.tlbFirstReqTime := GTimer()
  }

  // exception check
  val s0_addr_aligned = LookupTree(Mux(s0_isvec, io.vecstin.bits.alignedType, s0_uop.ctrl.fuOpType(1, 0)), List(
    "b00".U   -> true.B,                      //b
    "b01".U   -> (s0_out.vaddr(0) === 0.U),   //h
    "b10".U   -> (s0_out.vaddr(1,0) === 0.U), //w
    "b11".U   -> (s0_out.vaddr(2,0) === 0.U)  //d
  ))
  s0_out.uop.cf.exceptionVec(storeAddrMisaligned) := !s0_addr_aligned

  io.st_mask_out.valid       := s0_valid
  io.st_mask_out.bits.mask   := s0_out.mask
  io.st_mask_out.bits.sqIdx  := s0_out.uop.sqIdx

  io.stin.ready := s0_can_go && s0_int_iss_ready
  io.vecstin.ready := s0_can_go && s0_vec_iss_ready

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
  val s1_exp    = RegEnable(s0_out.exp, true.B, s0_fire)

  // mmio cbo decoder
  val s1_mmio_cbo  = s1_in.uop.ctrl.fuOpType === LSUOpType.cbo_clean ||
                     s1_in.uop.ctrl.fuOpType === LSUOpType.cbo_flush ||
                     s1_in.uop.ctrl.fuOpType === LSUOpType.cbo_inval
  val s1_paddr     = io.tlb.resp.bits.paddr(0)
  val s1_tlb_miss  = io.tlb.resp.bits.miss
  val s1_mmio      = s1_mmio_cbo
  val s1_exception = ExceptionNO.selectByFu(s1_out.uop.cf.exceptionVec, staCfg).asUInt.orR
  val s1_isvec     = RegEnable(s0_out.isvec, false.B, s0_fire)
  s1_kill := s1_in.uop.robIdx.needFlush(io.redirect) || s1_tlb_miss

  s1_ready := !s1_valid || s1_kill || s2_ready
  io.tlb.resp.ready := true.B // TODO: why dtlbResp needs a ready?
  when (s0_fire) { s1_valid := true.B }
  .elsewhen (s1_fire) { s1_valid := false.B }
  .elsewhen (s1_kill) { s1_valid := false.B }

  // st-ld violation dectect request.
  io.stld_nuke_query.valid       := s1_valid && !s1_tlb_miss
  io.stld_nuke_query.bits.robIdx := s1_in.uop.robIdx
  io.stld_nuke_query.bits.paddr  := s1_paddr
  io.stld_nuke_query.bits.mask   := s1_in.mask

  // Send TLB feedback to store issue queue
  // Store feedback is generated in store_s1, sent to RS in store_s2
  io.feedback_slow.valid           := s1_fire && !s1_isvec
  io.feedback_slow.bits.hit        := !s1_tlb_miss
  io.feedback_slow.bits.flushState := io.tlb.resp.bits.ptwBack
  io.feedback_slow.bits.rsIdx      := s1_in.rsIdx
  io.feedback_slow.bits.sourceType := RSFeedbackType.tlbMiss
  XSDebug(io.feedback_slow.valid,
    "S1 Store: tlbHit: %d robIdx: %d\n",
    io.feedback_slow.bits.hit,
    io.feedback_slow.bits.rsIdx
  )
  io.feedback_slow.bits.dataInvalidSqIdx := DontCare

  // issue
  io.issue.valid := s1_valid && !s1_tlb_miss && !s1_isvec
  io.issue.bits  := RegEnable(io.stin.bits, s0_valid)

  // rs feedback
  val s1_feedback = Wire(Valid(new RSFeedback))
  s1_feedback.valid                 := s1_valid && !s1_in.isHWPrefetch && !s1_isvec
  s1_feedback.bits.hit              := !s1_tlb_miss
  s1_feedback.bits.flushState       := io.tlb.resp.bits.ptwBack
  s1_feedback.bits.rsIdx            := s1_out.rsIdx
  s1_feedback.bits.sourceType       := RSFeedbackType.tlbMiss
  s1_feedback.bits.dataInvalidSqIdx := DontCare
  XSDebug(s1_feedback.valid,
    "S1 Store: tlbHit: %d robIdx: %d\n",
    s1_feedback.bits.hit,
    s1_feedback.bits.rsIdx
  )

  // TODO: VLSU, implement vector feedback
  val s1_vec_feedback = Wire(Valid(new VSFQFeedback))
  s1_vec_feedback.valid                 := s1_valid && !s1_in.isHWPrefetch && s1_isvec
  // s1_vec_feedback.bits.fqIdx            := s1_out.fqIdx
  s1_vec_feedback.bits.flowPtr          := s1_out.flowPtr
  s1_vec_feedback.bits.hit              := !s1_tlb_miss
  s1_vec_feedback.bits.sourceType       := RSFeedbackType.tlbMiss
  s1_vec_feedback.bits.paddr            := s1_paddr
  XSDebug(s1_vec_feedback.valid,
    "Vector S1 Store: tlbHit: %d fqIdx: %d\n",
    s1_vec_feedback.bits.hit,
    s1_vec_feedback.bits.flowPtr.value
  )

  // get paddr from dtlb, check if rollback is needed
  // writeback store inst to lsq
  s1_out        := s1_in
  s1_out.paddr  := s1_paddr
  s1_out.miss   := false.B
  s1_out.mmio   := s1_mmio
  s1_out.atomic := s1_mmio
  s1_out.uop.cf.exceptionVec(storePageFault)   := io.tlb.resp.bits.excp(0).pf.st && s1_exp
  s1_out.uop.cf.exceptionVec(storeAccessFault) := io.tlb.resp.bits.excp(0).af.st && s1_exp

  // TODO: VLSU, implement vector store queue
  io.lsq.valid     := s1_valid && !s1_isvec
  io.lsq.bits      := s1_out
  io.lsq.bits.miss := s1_tlb_miss

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
  val s2_exp    = RegEnable(s1_out.exp, true.B, s1_fire)

  s2_ready := !s2_valid || s2_kill || s3_ready
  when (s1_fire) { s2_valid := true.B }
  .elsewhen (s2_fire) { s2_valid := false.B }
  .elsewhen (s2_kill) { s2_valid := false.B }

  val s2_pmp = WireInit(io.pmp)
  val s2_static_pm = RegNext(io.tlb.resp.bits.static_pm)
  when (s2_static_pm.valid) {
    s2_pmp.ld    := false.B
    s2_pmp.st    := false.B
    s2_pmp.instr := false.B
    s2_pmp.mmio  := s2_static_pm.bits
  }

  val s2_exception = ExceptionNO.selectByFu(s2_out.uop.cf.exceptionVec, staCfg).asUInt.orR
  val s2_mmio = s2_in.mmio || s2_pmp.mmio
  s2_kill := (s2_mmio && !s2_exception) || s2_in.uop.robIdx.needFlush(io.redirect)

  s2_out        := s2_in
  s2_out.mmio   := s2_mmio && !s2_exception
  s2_out.atomic := s2_in.atomic || s2_pmp.atomic
  s2_out.uop.cf.exceptionVec(storeAccessFault) := (s2_in.uop.cf.exceptionVec(storeAccessFault) || s2_pmp.st) && s2_exp

  // feedback tlb miss to RS in store_s2
  io.feedback_slow.valid := RegNext(s1_feedback.valid && !s1_out.uop.robIdx.needFlush(io.redirect))
  io.feedback_slow.bits  := RegNext(s1_feedback.bits)

  // TODO: VLSU, implement vector feedback
  io.vec_feedback_slow.valid := RegNext(s1_vec_feedback.valid && !s1_out.uop.robIdx.needFlush(io.redirect))
  io.vec_feedback_slow.bits  := RegNext(s1_vec_feedback.bits)

  // mmio and exception
  io.lsq_replenish := s2_out

  // Pipeline
  // --------------------------------------------------------------------------------
  // stage 3
  // --------------------------------------------------------------------------------
  // store write back
  val s3_valid  = RegInit(false.B)
  val s3_in     = RegEnable(s2_out, s2_fire)
  val s3_out    = Wire(new ExuOutput)
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
  s3_out.redirectValid   := false.B
  s3_out.redirect        := DontCare
  s3_out.debug.isMMIO    := s3_in.mmio
  s3_out.debug.paddr     := s3_in.paddr
  s3_out.debug.vaddr     := s3_in.vaddr
  s3_out.debug.isPerfCnt := false.B
  s3_out.fflags          := DontCare

  // Pipeline
  // --------------------------------------------------------------------------------
  // stage x
  // --------------------------------------------------------------------------------
  // delay TotalSelectCycles - 2 cycle(s)
  val TotalDelayCycles = TotalSelectCycles - 2
  val sx_valid = Wire(Vec(TotalDelayCycles + 1, Bool()))
  val sx_ready = Wire(Vec(TotalDelayCycles + 1, Bool()))
  val sx_in    = Wire(Vec(TotalDelayCycles + 1, new ExuOutput))

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
      sx_valid(i) := RegEnable(Mux(prev_fire, true.B, false.B), sx_valid_can_go)
      sx_in(i) := RegEnable(sx_in(i-1), prev_fire)
    }
  }
  val sx_last_valid = sx_valid.takeRight(1).head
  val sx_last_ready = sx_ready.takeRight(1).head
  val sx_last_in    = sx_in.takeRight(1).head
  sx_last_ready := !sx_last_valid || sx_last_in.uop.robIdx.needFlush(io.redirect) || io.stout.ready

  io.stout.valid := sx_last_valid && !sx_last_in.uop.robIdx.needFlush(io.redirect)
  io.stout.bits := sx_last_in

  io.debug_ls := DontCare
  io.debug_ls.s1.isTlbFirstMiss := io.tlb.resp.valid && io.tlb.resp.bits.miss && io.tlb.resp.bits.debug.isFirstIssue
  io.debug_ls.s1_robIdx := s1_in.uop.robIdx.value

  private def printPipeLine(pipeline: LsPipelineBundle, cond: Bool, name: String): Unit = {
    XSDebug(cond,
      p"$name" + p" pc ${Hexadecimal(pipeline.uop.cf.pc)} " +
        p"addr ${Hexadecimal(pipeline.vaddr)} -> ${Hexadecimal(pipeline.paddr)} " +
        p"op ${Binary(pipeline.uop.ctrl.fuOpType)} " +
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
  // TODO: VLSU, implement vector addr spec
  XSPerfAccumulate("s0_addr_spec_success",       s0_fire && !s0_isvec && s0_saddr(VAddrBits-1, 12) === io.stin.bits.src(0)(VAddrBits-1, 12))
  XSPerfAccumulate("s0_addr_spec_failed",        s0_fire && !s0_isvec && s0_saddr(VAddrBits-1, 12) =/= io.stin.bits.src(0)(VAddrBits-1, 12))
  XSPerfAccumulate("s0_addr_spec_success_once",  s0_fire && !s0_isvec && s0_saddr(VAddrBits-1, 12) === io.stin.bits.src(0)(VAddrBits-1, 12) && s0_isFirstIssue)
  XSPerfAccumulate("s0_addr_spec_failed_once",   s0_fire && !s0_isvec && s0_saddr(VAddrBits-1, 12) =/= io.stin.bits.src(0)(VAddrBits-1, 12) && s0_isFirstIssue)

  XSPerfAccumulate("s1_in_valid",                s1_valid)
  XSPerfAccumulate("s1_in_fire",                 s1_fire)
  XSPerfAccumulate("s1_in_fire_first_issue",     s1_fire && s1_in.isFirstIssue)
  XSPerfAccumulate("s1_tlb_miss",                s1_fire && s1_tlb_miss)
  XSPerfAccumulate("s1_tlb_miss_first_issue",    s1_fire && s1_tlb_miss && s1_in.isFirstIssue)
  // end
}