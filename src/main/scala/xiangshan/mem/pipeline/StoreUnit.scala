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
import xiangshan._
import xiangshan.ExceptionNO._
import xiangshan.backend.Bundles.{MemExuInput, MemExuOutput, connectSamePort, UopIdx}
import xiangshan.backend.fu.PMPRespBundle
import xiangshan.backend.fu.FuConfig._
import xiangshan.backend.fu.FuType._
import xiangshan.backend.ctrlblock.DebugLsInfoBundle
import xiangshan.backend.fu.NewCSR._
import xiangshan.backend.rob.RobPtr
import xiangshan.mem.Bundles._
import xiangshan.cache.mmu.{Pbmt, TlbCmd, TlbReq, TlbRequestIO, TlbResp}
import xiangshan.cache.{DCacheStoreIO, DcacheStoreRequestIO, HasDCacheParameters, MemoryOpConstants, StorePrefetchReq}

class StoreUnit(implicit p: Parameters) extends XSModule
  with HasDCacheParameters
  with HasVLSUParameters
  {
  val io = IO(new Bundle() {
    val redirect        = Flipped(ValidIO(new Redirect))
    val csrCtrl         = Flipped(new CustomCSRCtrlIO)
    val stin            = Flipped(Decoupled(new MemExuInput))
    val issue           = Valid(new MemExuInput)
    // misalignBuffer issue path
    val misalign_stin   = Flipped(Decoupled(new LsPipelineBundle))
    val misalign_stout  = Valid(new SqWriteBundle)
    val tlb             = new TlbRequestIO()
    val dcache          = new DCacheStoreIO
    val pmp             = Flipped(new PMPRespBundle())
    val lsq             = ValidIO(new LsPipelineBundle)
    val lsq_replenish   = Output(new LsPipelineBundle())
    val feedback_slow   = ValidIO(new RSFeedback)
    val prefetch_req    = Flipped(DecoupledIO(new StorePrefetchReq))
    // provide prefetch info to sms
    val prefetch_train  = ValidIO(new LsPrefetchTrainBundle())
    // speculative for gated control
    val s1_prefetch_spec = Output(Bool())
    val s2_prefetch_spec = Output(Bool())
    val stld_nuke_query = Valid(new StoreNukeQueryBundle)
    val stout           = DecoupledIO(new MemExuOutput) // writeback store
    val vecstout        = DecoupledIO(new VecPipelineFeedbackIO(isVStore = true))
    // store mask, send to sq in store_s0
    val st_mask_out     = Valid(new StoreMaskBundle)
    val debug_ls        = Output(new DebugLsInfoBundle)
    // vector
    val vecstin           = Flipped(Decoupled(new VecPipeBundle(isVStore = true)))
    val vec_isFirstIssue  = Input(Bool())
    // writeback to misalign buffer
    val misalign_enq = new MisalignBufferEnqIO
    // trigger
    val fromCsrTrigger = Input(new CsrTriggerBundle)
    val sqDeqPtr       = Input(new SqPtr)
    val sqDeqUopIdx    = Input(UopIdx())
    val sqDeqRobIdx    = Input(new RobPtr())

    val s0_s1_s2_valid = Output(Bool())
  })

  PerfCCT.updateInstPos(io.stin.bits.uop.debug_seqNum, PerfCCT.InstPos.AtFU.id.U, io.stin.valid, clock, reset)

  val s1_ready, s2_ready, s3_ready = WireInit(false.B)

  // Pipeline
  // --------------------------------------------------------------------------------
  // stage 0
  // --------------------------------------------------------------------------------
  // generate addr, use addr to query DCache and DTLB
  val s0_iss_valid        = io.stin.valid
  val s0_prf_valid        = io.prefetch_req.valid && io.dcache.req.ready
  val s0_vec_valid        = io.vecstin.valid
  val s0_ma_st_valid      = io.misalign_stin.valid
  val s0_valid            = s0_iss_valid || s0_prf_valid || s0_vec_valid || s0_ma_st_valid
  val s0_use_flow_ma      = s0_ma_st_valid
  val s0_use_flow_vec     = s0_vec_valid && !s0_ma_st_valid
  val s0_use_flow_rs      = s0_iss_valid && !s0_vec_valid && !s0_ma_st_valid
  val s0_use_flow_prf     = s0_prf_valid && !s0_iss_valid && !s0_vec_valid && !s0_ma_st_valid
  val s0_use_non_prf_flow = s0_use_flow_rs || s0_use_flow_vec || s0_use_flow_ma
  val s0_stin             = Mux(s0_use_flow_rs, io.stin.bits, 0.U.asTypeOf(io.stin.bits))
  val s0_vecstin          = Mux(s0_use_flow_vec, io.vecstin.bits, 0.U.asTypeOf(io.vecstin.bits))
  val s0_uop              = Mux(
    s0_use_flow_ma,
    io.misalign_stin.bits.uop,
    Mux(
      s0_use_flow_rs,
      s0_stin.uop,
      s0_vecstin.uop
    )
  )
  val s0_isFirstIssue = Mux(
    s0_use_flow_ma,
    false.B,
    s0_use_flow_rs && io.stin.bits.isFirstIssue || s0_use_flow_vec && io.vec_isFirstIssue
  )
  val s0_size         = Mux(s0_use_non_prf_flow, s0_uop.fuOpType(2,0), 0.U)// may broken if use it in feature
  val s0_mem_idx      = Mux(s0_use_non_prf_flow, s0_uop.sqIdx.value, 0.U)
  val s0_rob_idx      = Mux(s0_use_non_prf_flow, s0_uop.robIdx, 0.U.asTypeOf(s0_uop.robIdx))
  val s0_pc           = Mux(s0_use_non_prf_flow, s0_uop.pc, 0.U)
  val s0_instr_type   = Mux(s0_use_non_prf_flow, STORE_SOURCE.U, DCACHE_PREFETCH_SOURCE.U)
  val s0_wlineflag    = Mux(s0_use_flow_rs, LSUOpType.isCboAll(s0_uop.fuOpType), false.B)
  val s0_out          = Wire(new LsPipelineBundle)
  val s0_kill         = s0_uop.robIdx.needFlush(io.redirect)
  val s0_can_go       = s1_ready
  val s0_fire         = s0_valid && !s0_kill && s0_can_go
  val s0_is128bit     = Wire(Bool())
  // vector
  val s0_vecActive    = !s0_use_flow_vec || s0_vecstin.vecActive
  // val s0_flowPtr      = s0_vecstin.flowPtr
  // val s0_isLastElem   = s0_vecstin.isLastElem
  val s0_secondInv    = s0_vecstin.usSecondInv
  val s0_elemIdx      = s0_vecstin.elemIdx
  val s0_alignedType  = s0_vecstin.alignedType
  val s0_mBIndex      = s0_vecstin.mBIndex
  val s0_vecBaseVaddr = s0_vecstin.basevaddr
  val s0_isFinalSplit = io.misalign_stin.valid && io.misalign_stin.bits.isFinalSplit

  // generate addr
  val s0_saddr = s0_stin.src(0) + SignExt(s0_stin.uop.imm(11,0), VAddrBits)
  val s0_fullva = Wire(UInt(XLEN.W))

  val s0_vaddr = Mux(
    s0_use_flow_ma,
    io.misalign_stin.bits.vaddr,
    Mux(
      s0_use_flow_rs,
      s0_saddr,
      Mux(
        s0_use_flow_vec,
        s0_vecstin.vaddr(VAddrBits - 1, 0),
        io.prefetch_req.bits.vaddr
      )
    )
  )

  val s0_isCbo = s0_use_flow_rs && LSUOpType.isCboAll(s0_stin.uop.fuOpType)
  val s0_isCbo_noZero = s0_use_flow_rs && LSUOpType.isCbo(s0_stin.uop.fuOpType)
  // only simulation
  val cbo_assert_flag = LSUOpType.isCboAll(s0_out.uop.fuOpType)
  XSError(!s0_use_flow_rs && cbo_assert_flag && s0_valid, "cbo instruction selection error.")

  val s0_alignType = Mux(s0_use_flow_vec, s0_vecstin.alignedType(1,0), s0_uop.fuOpType(1, 0))
  // exception check
  val s0_addr_aligned = LookupTree(s0_alignType, List(
    "b00".U   -> true.B,              //b
    "b01".U   -> (s0_vaddr(0) === 0.U),   //h
    "b10".U   -> (s0_vaddr(1,0) === 0.U), //w
    "b11".U   -> (s0_vaddr(2,0) === 0.U)  //d
  )) || s0_isCbo
  // if vector store sends 128-bit requests, its address must be 128-aligned
  XSError(s0_use_flow_vec && s0_vaddr(3, 0) =/= 0.U && s0_vecstin.alignedType(2), "unit stride 128 bit element is not aligned!")

  val s0_isMisalign = Mux(s0_use_non_prf_flow, (!s0_addr_aligned || s0_vecstin.uop.exceptionVec(storeAddrMisaligned) && s0_vecActive), false.B)
  val s0_addr_low = s0_vaddr(4, 0)
  val s0_addr_Up_low = LookupTree(s0_alignType, List(
    "b00".U -> 0.U,
    "b01".U -> 1.U,
    "b10".U -> 3.U,
    "b11".U -> 7.U
  )) + s0_addr_low
  val s0_rs_corss16Bytes = s0_addr_Up_low(4) =/= s0_addr_low(4)
  val s0_misalignWith16Byte = !s0_rs_corss16Bytes && !s0_addr_aligned && !s0_use_flow_prf
  val s0_misalignNeedReplay = (s0_use_flow_vec || s0_rs_corss16Bytes) && !(s0_uop.sqIdx === io.sqDeqPtr || s0_uop.robIdx === io.sqDeqRobIdx && s0_uop.uopIdx === io.sqDeqUopIdx)
  s0_is128bit := Mux(s0_use_flow_ma, io.misalign_stin.bits.is128bit, is128Bit(s0_vecstin.alignedType) || s0_misalignWith16Byte)

  s0_fullva := Mux(
    s0_use_flow_rs,
    s0_stin.src(0) + SignExt(s0_stin.uop.imm(11,0), XLEN),
    Mux(
      s0_use_flow_vec,
      s0_vecstin.vaddr,
      s0_vaddr
    )
  )

  val s0_mask = Mux(
    s0_use_flow_ma,
    io.misalign_stin.bits.mask,
    Mux(
      s0_use_flow_rs,
      Mux(s0_isCbo, Fill(VLEN/8, 1.U(1.W)), genVWmask128(s0_saddr, s0_uop.fuOpType(2,0))),
      Mux(
        s0_use_flow_vec,
        s0_vecstin.mask,
        // -1.asSInt.asUInt
        Fill(VLEN/8, 1.U(1.W))
      )
    )
  )

  io.tlb.req.valid                   := s0_valid
  io.tlb.req.bits.vaddr              := s0_vaddr
  io.tlb.req.bits.fullva             := s0_fullva
  io.tlb.req.bits.checkfullva        := s0_use_flow_rs || s0_use_flow_vec
  io.tlb.req.bits.cmd                := Mux(s0_isCbo_noZero, TlbCmd.read, TlbCmd.write)
  io.tlb.req.bits.isPrefetch         := s0_use_flow_prf
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
  io.tlb.req.bits.hyperinst          := LSUOpType.isHsv(s0_uop.fuOpType)
  io.tlb.req.bits.hlvx               := false.B
  io.tlb.req.bits.pmp_addr           := DontCare

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
  s0_out.fullva       := s0_fullva
  // Now data use its own io
  s0_out.data         := s0_stin.src(1)
  s0_out.uop          := s0_uop
  s0_out.miss         := false.B
  // For unaligned, we need to generate a base-aligned mask in storeunit and then do a shift split in StoreQueue.
  s0_out.mask         := Mux(s0_rs_corss16Bytes && !s0_addr_aligned, genBasemask(s0_saddr,s0_alignType(1,0)), s0_mask)
  s0_out.isFirstIssue := s0_isFirstIssue
  s0_out.isHWPrefetch := s0_use_flow_prf
  s0_out.wlineflag    := s0_wlineflag
  s0_out.isvec        := s0_use_flow_vec
  s0_out.is128bit     := s0_is128bit
  s0_out.vecActive    := s0_vecActive
  s0_out.usSecondInv  := s0_secondInv
  s0_out.elemIdx      := s0_elemIdx
  s0_out.alignedType  := s0_alignedType
  s0_out.mbIndex      := s0_mBIndex
  s0_out.misalignWith16Byte      := s0_misalignWith16Byte
  s0_out.isMisalign      := s0_isMisalign
  s0_out.vecBaseVaddr := s0_vecBaseVaddr
  when(s0_valid && s0_isFirstIssue) {
    s0_out.uop.debugInfo.tlbFirstReqTime := GTimer()
  }
  s0_out.isFrmMisAlignBuf := s0_use_flow_ma
  s0_out.isFinalSplit := s0_isFinalSplit
//  s0_out.uop.exceptionVec(storeAddrMisaligned) := Mux(s0_use_non_prf_flow, (!s0_addr_aligned || s0_vecstin.uop.exceptionVec(storeAddrMisaligned) && s0_vecActive), false.B) && !s0_misalignWith16Byte

  io.st_mask_out.valid       := s0_use_flow_rs || s0_use_flow_vec
  io.st_mask_out.bits.mask   := s0_out.mask
  io.st_mask_out.bits.sqIdx  := s0_out.uop.sqIdx

  io.stin.ready := s1_ready && s0_use_flow_rs
  io.vecstin.ready := s1_ready && s0_use_flow_vec
  io.prefetch_req.ready := s1_ready && io.dcache.req.ready && !s0_iss_valid && !s0_vec_valid && !s0_ma_st_valid
  io.misalign_stin.ready := s1_ready && s0_use_flow_ma

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
  val s1_vecActive    = RegEnable(s0_out.vecActive, true.B, s0_fire)
  val s1_frm_mabuf    = s1_in.isFrmMisAlignBuf
  val s1_is128bit     = s1_in.is128bit

  // mmio cbo decoder
  val s1_isCbo   = RegEnable(s0_isCbo, s0_fire)
  val s1_vaNeedExt = io.tlb.resp.bits.excp(0).vaNeedExt
  val s1_isHyper   = io.tlb.resp.bits.excp(0).isHyper
  val s1_paddr     = io.tlb.resp.bits.paddr(0)
  val s1_gpaddr    = io.tlb.resp.bits.gpaddr(0)
  val s1_fullva    = Mux(s1_frm_mabuf, s1_out.vaddr, io.tlb.resp.bits.fullva)
  val s1_isForVSnonLeafPTE   = io.tlb.resp.bits.isForVSnonLeafPTE
  val s1_tlb_miss  = io.tlb.resp.bits.miss && io.tlb.resp.valid && s1_valid
  val s1_tlb_hit   = !io.tlb.resp.bits.miss && io.tlb.resp.valid && s1_valid
  val s1_pbmt      = Mux(s1_tlb_hit, io.tlb.resp.bits.pbmt.head, 0.U(Pbmt.width.W))
  val s1_exception = ExceptionNO.selectByFu(s1_out.uop.exceptionVec, StaCfg).asUInt.orR
  val s1_isvec     = RegEnable(s0_out.isvec, false.B, s0_fire)
  val s1_misalignNeedReplay = RegEnable(s0_misalignNeedReplay, false.B, s0_fire)
  //We don't want `StoreUnit` to have an additional effect on the Store of vector from a `misalignBuffer,`
  //But there are places where a marker bit is needed to enable additional processing of vector instructions.
  //For example: `StoreQueue` is exceptionBuffer
  val s1_frm_mab_vec = RegEnable(s0_use_flow_ma && io.misalign_stin.bits.isvec, false.B, s0_fire)
  // val s1_isLastElem = RegEnable(s0_isLastElem, false.B, s0_fire)
  s1_kill := s1_in.uop.robIdx.needFlush(io.redirect) || (s1_tlb_miss && !s1_isvec && !s1_frm_mabuf)

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
  io.stld_nuke_query.bits.matchType := Mux(
                                          s1_isCbo,
                                          StLdNukeMatchType.CacheLine,
                                          Mux(s1_in.is128bit, StLdNukeMatchType.QuadWord, StLdNukeMatchType.Normal)
                                        )

  // issue
  io.issue.valid := s1_valid && !s1_tlb_miss && !s1_in.isHWPrefetch && !s1_isvec && !s1_frm_mabuf
  io.issue.bits  := RegEnable(s0_stin, s0_valid)

  // trigger
  val storeTrigger = Module(new MemTrigger(MemType.STORE))
  storeTrigger.io.fromCsrTrigger.tdataVec             := io.fromCsrTrigger.tdataVec
  storeTrigger.io.fromCsrTrigger.tEnableVec           := io.fromCsrTrigger.tEnableVec
  storeTrigger.io.fromCsrTrigger.triggerCanRaiseBpExp := io.fromCsrTrigger.triggerCanRaiseBpExp
  storeTrigger.io.fromCsrTrigger.debugMode            := io.fromCsrTrigger.debugMode
  storeTrigger.io.fromLoadStore.vaddr                 := s1_in.vaddr
  storeTrigger.io.fromLoadStore.isVectorUnitStride    := s1_in.isvec && s1_in.is128bit
  storeTrigger.io.fromLoadStore.mask                  := s1_in.mask
  storeTrigger.io.isCbo.get                           := s1_isCbo

  val s1_trigger_action = storeTrigger.io.toLoadStore.triggerAction
  val s1_trigger_debug_mode = TriggerAction.isDmode(s1_trigger_action)
  val s1_trigger_breakpoint = TriggerAction.isExp(s1_trigger_action)

  // Send TLB feedback to store issue queue
  // Store feedback is generated in store_s1, sent to RS in store_s2
  val s1_feedback = Wire(Valid(new RSFeedback))
  s1_feedback.valid                 := s1_valid & !s1_in.isHWPrefetch
  s1_feedback.bits.hit              := !s1_tlb_miss
  s1_feedback.bits.flushState       := io.tlb.resp.bits.ptwBack
  s1_feedback.bits.robIdx           := s1_out.uop.robIdx
  s1_feedback.bits.sourceType       := RSFeedbackType.tlbMiss
  s1_feedback.bits.dataInvalidSqIdx := DontCare
  s1_feedback.bits.sqIdx            := s1_out.uop.sqIdx
  s1_feedback.bits.lqIdx            := s1_out.uop.lqIdx

  XSDebug(s1_feedback.valid,
    "S1 Store: tlbHit: %d robIdx: %d\n",
    s1_feedback.bits.hit,
    s1_feedback.bits.robIdx.value
  )

  // io.feedback_slow := s1_feedback

  // get paddr from dtlb, check if rollback is needed
  // writeback store inst to lsq
  s1_out           := s1_in
  s1_out.paddr     := s1_paddr
  s1_out.gpaddr    := s1_gpaddr
  s1_out.fullva    := s1_fullva
  s1_out.vaNeedExt := s1_vaNeedExt
  s1_out.isHyper   := s1_isHyper
  s1_out.miss      := false.B
  s1_out.nc        := Pbmt.isNC(s1_pbmt)
  s1_out.mmio      := Pbmt.isIO(s1_pbmt)
  s1_out.tlbMiss   := s1_tlb_miss
  s1_out.isForVSnonLeafPTE := s1_isForVSnonLeafPTE
  when (RegNext(io.tlb.req.bits.checkfullva) &&
    (s1_out.uop.exceptionVec(storePageFault) ||
      s1_out.uop.exceptionVec(storeAccessFault) ||
      s1_out.uop.exceptionVec(storeGuestPageFault))) {
    s1_out.uop.exceptionVec(storeAddrMisaligned) := false.B
  }
  s1_out.uop.exceptionVec(storePageFault)      := (io.tlb.resp.bits.excp(0).pf.st  || io.tlb.resp.bits.excp(0).pf.ld ) && s1_vecActive
  s1_out.uop.exceptionVec(storeAccessFault)    := (io.tlb.resp.bits.excp(0).af.st  || io.tlb.resp.bits.excp(0).af.ld ) && s1_vecActive
  s1_out.uop.exceptionVec(storeGuestPageFault) := (io.tlb.resp.bits.excp(0).gpf.st || io.tlb.resp.bits.excp(0).gpf.ld) && s1_vecActive

  s1_out.uop.flushPipe                := false.B
  s1_out.uop.trigger                  := s1_trigger_action
  s1_out.uop.exceptionVec(breakPoint) := s1_trigger_breakpoint
  s1_out.uop.exceptionVec(storeAddrMisaligned) := s1_out.mmio && s1_in.isMisalign
  s1_out.vecVaddrOffset := Mux(
    s1_trigger_debug_mode || s1_trigger_breakpoint,
    storeTrigger.io.toLoadStore.triggerVaddr - s1_in.vecBaseVaddr,
    s1_in.vaddr + genVFirstUnmask(s1_in.mask).asUInt - s1_in.vecBaseVaddr ,
  )
  s1_out.vecTriggerMask := Mux(s1_trigger_debug_mode || s1_trigger_breakpoint, storeTrigger.io.toLoadStore.triggerMask, 0.U)

  // scalar store and scalar load nuke check, and also other purposes
  //A 128-bit aligned unaligned memory access requires changing the unaligned flag bit in sq
  io.lsq.valid     := s1_valid && !s1_in.isHWPrefetch
  io.lsq.bits      := s1_out
  io.lsq.bits.miss := s1_tlb_miss
  io.lsq.bits.isvec := s1_out.isvec || s1_frm_mab_vec
  io.lsq.bits.updateAddrValid := (!s1_in.isMisalign || s1_in.misalignWith16Byte) && (!s1_frm_mabuf || s1_in.isFinalSplit) || s1_exception
  // kill dcache write intent request when tlb miss or exception
  io.dcache.s1_kill  := (s1_tlb_miss || s1_exception || s1_out.mmio || s1_out.nc || s1_in.uop.robIdx.needFlush(io.redirect))
  io.dcache.s1_paddr := s1_paddr

  // write below io.out.bits assign sentence to prevent overwriting values
  val s1_tlb_memidx = io.tlb.resp.bits.memidx
  when(s1_tlb_memidx.is_st && io.tlb.resp.valid && !s1_tlb_miss && s1_tlb_memidx.idx === s1_out.uop.sqIdx.value) {
    // printf("Store idx = %d\n", s1_tlb_memidx.idx)
    s1_out.uop.debugInfo.tlbRespTime := GTimer()
  }
  val s1_mis_align = s1_valid && !s1_tlb_miss && !s1_in.isHWPrefetch && !s1_isCbo && !s1_out.nc && !s1_out.mmio &&
                      GatedValidRegNext(io.csrCtrl.hd_misalign_st_enable) && s1_in.isMisalign && !s1_in.misalignWith16Byte &&
                      !s1_trigger_breakpoint && !s1_trigger_debug_mode
  val s1_toMisalignBufferValid = s1_valid && !s1_tlb_miss && !s1_in.isHWPrefetch &&
    !s1_frm_mabuf && !s1_isCbo && s1_in.isMisalign && !s1_in.misalignWith16Byte &&
    GatedValidRegNext(io.csrCtrl.hd_misalign_st_enable)
  io.misalign_enq.req.valid := s1_toMisalignBufferValid && !s1_misalignNeedReplay
  io.misalign_enq.req.bits.fromLsPipelineBundle(s1_in)

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
  val s2_vecActive    = RegEnable(s1_out.vecActive, true.B, s1_fire)
  val s2_frm_mabuf    = s2_in.isFrmMisAlignBuf
  val s2_frm_mab_vec  = RegEnable(s1_frm_mab_vec, true.B, s1_fire)
  val s2_pbmt   = RegEnable(s1_pbmt, s1_fire)
  val s2_trigger_debug_mode = RegEnable(s1_trigger_debug_mode, false.B, s1_fire)
  val s2_tlb_hit = RegEnable(s1_tlb_hit, s1_fire)

  s2_ready := !s2_valid || s2_kill || s3_ready
  when (s1_fire) { s2_valid := true.B }
  .elsewhen (s2_fire) { s2_valid := false.B }
  .elsewhen (s2_kill) { s2_valid := false.B }

  val s2_pmp = WireInit(io.pmp)

  val s2_exception = RegNext(s1_feedback.bits.hit) &&
                    (s2_trigger_debug_mode || ExceptionNO.selectByFu(s2_out.uop.exceptionVec, StaCfg).asUInt.orR) && s2_vecActive
  val s2_un_misalign_exception =  RegNext(s1_feedback.bits.hit) &&
                    (s2_trigger_debug_mode || ExceptionNO.selectByFuAndUnSelect(s2_out.uop.exceptionVec, StaCfg, Seq(storeAddrMisaligned)).asUInt.orR)

  val s2_mmio = (s2_in.mmio || (Pbmt.isPMA(s2_pbmt) && s2_pmp.mmio)) && RegNext(s1_feedback.bits.hit)
  val s2_memBackTypeMM = !s2_pmp.mmio
  // The response signal of `pmp/pma` is credible only after the physical address is actually generated.
  // Therefore, the response signals of pmp/pma generated after an address translation has produced an `access fault` or a `page fault` are completely unreliable.
  val s2_un_access_exception =  s2_vecActive && (
    s2_in.uop.exceptionVec(storeAccessFault) ||
    s2_in.uop.exceptionVec(storePageFault)   ||
    s2_in.uop.exceptionVec(storeGuestPageFault)
  )
  // This real physical address is located in uncache space.
  val s2_actually_uncache = s2_tlb_hit && !s2_un_access_exception && (Pbmt.isPMA(s2_pbmt) && s2_pmp.mmio || s2_in.nc || s2_in.mmio) && RegNext(s1_feedback.bits.hit)
  val s2_isCbo  = RegEnable(s1_isCbo, s1_fire) // all cbo instr
  val s2_isCbo_noZero = LSUOpType.isCbo(s2_in.uop.fuOpType)

  s2_kill := ((s2_mmio && !s2_exception) && !s2_in.isvec && !s2_frm_mabuf) || s2_in.uop.robIdx.needFlush(io.redirect)

  s2_out        := s2_in
  s2_out.af     := s2_out.uop.exceptionVec(storeAccessFault)
  s2_out.mmio   := s2_mmio && !s2_exception
  s2_out.memBackTypeMM := s2_memBackTypeMM
  s2_out.uop.exceptionVec(storeAccessFault) := (s2_in.uop.exceptionVec(storeAccessFault) ||
                                                s2_pmp.st ||
                                                s2_pmp.ld && s2_isCbo_noZero || // cmo need read permission but produce store exception
                                                ((s2_in.isvec || s2_isCbo) && s2_actually_uncache && RegNext(s1_feedback.bits.hit))
                                                ) && s2_vecActive
  s2_out.uop.exceptionVec(storeAddrMisaligned) := s2_actually_uncache && !s2_in.isvec && (s2_in.isMisalign || s2_in.isFrmMisAlignBuf) && !s2_un_misalign_exception
  s2_out.uop.vpu.vstart     := s2_in.vecVaddrOffset >> s2_in.uop.vpu.veew

  // kill dcache write intent request when mmio or exception
  io.dcache.s2_kill := (s2_actually_uncache || s2_exception || s2_in.uop.robIdx.needFlush(io.redirect))
  io.dcache.s2_pc   := s2_out.uop.pc
  // TODO: dcache resp
  io.dcache.resp.ready := true.B

  val s2_mis_align = s2_valid && RegEnable(s1_mis_align, s1_fire) && !s2_exception
  // goto misalignBuffer
  io.misalign_enq.revoke := s2_exception
  val s2_misalignBufferNack = !io.misalign_enq.revoke &&
    RegEnable(s1_toMisalignBufferValid && (!io.misalign_enq.req.ready || s1_misalignNeedReplay), false.B, s1_fire)

  // feedback tlb miss to RS in store_s2
  val feedback_slow_valid = WireInit(false.B)

  feedback_slow_valid := s1_feedback.valid && !s1_out.uop.robIdx.needFlush(io.redirect) && !s1_out.isvec && !s1_frm_mabuf
  io.feedback_slow.valid := GatedValidRegNext(feedback_slow_valid)
  io.feedback_slow.bits  := RegEnable(s1_feedback.bits, feedback_slow_valid)
  io.feedback_slow.bits.hit  := RegEnable(s1_feedback.bits.hit, feedback_slow_valid) && !s2_misalignBufferNack

  val s2_vecFeedback = RegNext(!s1_out.uop.robIdx.needFlush(io.redirect) && s1_feedback.bits.hit && s1_feedback.valid) &&
                       !s2_misalignBufferNack && s2_in.isvec && !s2_frm_mabuf

  val s2_misalign_stout = WireInit(0.U.asTypeOf(io.misalign_stout))
  s2_misalign_stout.valid := s2_valid && s2_can_go && s2_frm_mabuf
  connectSamePort(s2_misalign_stout.bits, s2_out)
  s2_misalign_stout.bits.need_rep := RegEnable(s1_tlb_miss, s1_fire)
  io.misalign_stout := s2_misalign_stout

  val s2_misalign_cango = !s2_mis_align || s2_in.isvec && s2_misalignBufferNack

  // mmio and exception
  io.lsq_replenish := s2_out
  io.lsq_replenish.af := s2_out.af && s2_valid && !s2_kill
  io.lsq_replenish.mmio := (s2_mmio || s2_isCbo_noZero) && !s2_exception // reuse `mmiostall` logic in sq

  // prefetch related
  io.lsq_replenish.miss := io.dcache.resp.fire && io.dcache.resp.bits.miss // miss info
  io.lsq_replenish.updateAddrValid := !s2_mis_align && (!s2_frm_mabuf || s2_out.isFinalSplit) || s2_exception
  io.lsq_replenish.isvec := s2_out.isvec || s2_frm_mab_vec

  io.lsq_replenish.hasException := (ExceptionNO.selectByFu(s2_out.uop.exceptionVec, StaCfg).asUInt.orR ||
    TriggerAction.isDmode(s2_out.uop.trigger) || s2_out.af) && s2_valid && !s2_kill


  // RegNext prefetch train for better timing
  // ** Now, prefetch train is valid at store s3 **
  val s2_prefetch_train_valid = WireInit(false.B)
  s2_prefetch_train_valid := s2_valid && io.dcache.resp.fire && !s2_out.mmio && !s2_out.nc && !s2_in.tlbMiss && !s2_in.isHWPrefetch
  if(EnableStorePrefetchSMS) {
    io.s1_prefetch_spec := s1_fire
    io.s2_prefetch_spec := s2_prefetch_train_valid
    io.prefetch_train.valid := RegNext(s2_prefetch_train_valid)
    io.prefetch_train.bits.fromLsPipelineBundle(s2_in, latch = true, enable = s2_prefetch_train_valid)
  }else {
    io.s1_prefetch_spec := false.B
    io.s2_prefetch_spec := false.B
    io.prefetch_train.valid := false.B
    io.prefetch_train.bits.fromLsPipelineBundle(s2_in, latch = true, enable = false.B)
  }
  // override miss bit
  io.prefetch_train.bits.miss := RegEnable(io.dcache.resp.bits.miss, s2_prefetch_train_valid)
  // TODO: add prefetch and access bit
  io.prefetch_train.bits.meta_prefetch := false.B
  io.prefetch_train.bits.meta_access := false.B
  io.prefetch_train.bits.isFinalSplit := false.B
  io.prefetch_train.bits.misalignWith16Byte := false.B
  io.prefetch_train.bits.isMisalign := false.B
  io.prefetch_train.bits.misalignNeedWakeUp := false.B
  io.prefetch_train.bits.updateAddrValid := false.B
  io.prefetch_train.bits.hasException := false.B

  // for misalign in vsMergeBuffer
  io.s0_s1_s2_valid := s0_valid || s1_valid || s2_valid

  // Pipeline
  // --------------------------------------------------------------------------------
  // stage 3
  // --------------------------------------------------------------------------------
  // store write back
  val s3_valid  = RegInit(false.B)
  val s3_in     = RegEnable(s2_out, s2_fire)
  val s3_out    = Wire(new MemExuOutput(isVector = true))
  val s3_kill   = s3_in.uop.robIdx.needFlush(io.redirect)
  val s3_can_go = s3_ready
  val s3_fire   = s3_valid && !s3_kill && s3_can_go
  val s3_vecFeedback = RegEnable(s2_vecFeedback, s2_fire)
  val s3_exception     = RegEnable(s2_exception, s2_fire)

  // store misalign will not writeback to rob now
  when (s2_fire) { s3_valid := (!s2_mmio && !s2_isCbo || s2_exception) && !s2_out.isHWPrefetch && s2_misalign_cango && !s2_frm_mabuf }
  .elsewhen (s3_fire) { s3_valid := false.B }
  .elsewhen (s3_kill) { s3_valid := false.B }

  // wb: writeback

  s3_out                 := DontCare
  s3_out.uop             := s3_in.uop
  s3_out.data            := DontCare
  s3_out.debug.isMMIO    := s3_in.mmio
  s3_out.debug.isNCIO    := s3_in.nc && !s3_in.memBackTypeMM
  s3_out.debug.paddr     := s3_in.paddr
  s3_out.debug.vaddr     := s3_in.vaddr
  s3_out.debug.isPerfCnt := false.B

  XSError(s3_valid && s3_in.isvec && s3_in.vecActive && !s3_in.mask.orR, "In vecActive, mask complement should not be 0")
  // Pipeline
  // --------------------------------------------------------------------------------
  // stage x
  // --------------------------------------------------------------------------------
  val sx_valid = Wire(Vec(RAWTotalDelayCycles + 1, Bool()))
  val sx_ready = Wire(Vec(RAWTotalDelayCycles + 1, Bool()))
  val sx_in    = Wire(Vec(RAWTotalDelayCycles + 1, new VecMemExuOutput(isVector = true)))
  val sx_in_vec = Wire(Vec(RAWTotalDelayCycles +1, Bool()))

  // backward ready signal
  s3_ready := sx_ready.head
  for (i <- 0 until RAWTotalDelayCycles + 1) {
    if (i == 0) {
      sx_valid(i)          := s3_valid
      sx_in(i).output      := s3_out
      sx_in(i).vecFeedback := s3_vecFeedback
      sx_in(i).nc          := s3_in.nc
      sx_in(i).mmio        := s3_in.mmio
      sx_in(i).usSecondInv := s3_in.usSecondInv
      sx_in(i).elemIdx     := s3_in.elemIdx
      sx_in(i).alignedType := s3_in.alignedType
      sx_in(i).mbIndex     := s3_in.mbIndex
      sx_in(i).mask        := s3_in.mask
      sx_in(i).vaddr       := s3_in.fullva
      sx_in(i).vaNeedExt   := s3_in.vaNeedExt
      sx_in(i).gpaddr      := s3_in.gpaddr
      sx_in(i).isForVSnonLeafPTE     := s3_in.isForVSnonLeafPTE
      sx_in(i).vecTriggerMask := s3_in.vecTriggerMask
      sx_in(i).hasException := s3_exception
      sx_in_vec(i)         := s3_in.isvec
      sx_ready(i) := !s3_valid(i) || sx_in(i).output.uop.robIdx.needFlush(io.redirect) || (if (RAWTotalDelayCycles == 0) io.stout.ready else sx_ready(i+1))
    } else {
      val cur_kill   = sx_in(i).output.uop.robIdx.needFlush(io.redirect)
      val cur_can_go = (if (i == RAWTotalDelayCycles) io.stout.ready else sx_ready(i+1))
      val cur_fire   = sx_valid(i) && !cur_kill && cur_can_go
      val prev_fire  = sx_valid(i-1) && !sx_in(i-1).output.uop.robIdx.needFlush(io.redirect) && sx_ready(i)

      sx_ready(i) := !sx_valid(i) || cur_kill || (if (i == RAWTotalDelayCycles) io.stout.ready else sx_ready(i+1))
      val sx_valid_can_go = prev_fire || cur_fire || cur_kill
      sx_valid(i) := RegEnable(Mux(prev_fire, true.B, false.B), false.B, sx_valid_can_go)
      sx_in(i) := RegEnable(sx_in(i-1), prev_fire)
      sx_in_vec(i) := RegEnable(sx_in_vec(i-1), prev_fire)
    }
  }
  val sx_last_valid = sx_valid.takeRight(1).head
  val sx_last_ready = sx_ready.takeRight(1).head
  val sx_last_in    = sx_in.takeRight(1).head
  val sx_last_in_vec = sx_in_vec.takeRight(1).head
  sx_last_ready := !sx_last_valid || sx_last_in.output.uop.robIdx.needFlush(io.redirect) || io.stout.ready

  // write back: normal store, nc store
  io.stout.valid := sx_last_valid && !sx_last_in_vec //isStore(sx_last_in.output.uop.fuType)
  io.stout.bits := sx_last_in.output
  io.stout.bits.uop.exceptionVec := ExceptionNO.selectByFu(sx_last_in.output.uop.exceptionVec, StaCfg)

  io.vecstout.valid := sx_last_valid && sx_last_in_vec //isVStore(sx_last_in.output.uop.fuType)
  // TODO: implement it!
  io.vecstout.bits.mBIndex := sx_last_in.mbIndex
  io.vecstout.bits.hit := sx_last_in.vecFeedback
  io.vecstout.bits.isvec := true.B
  io.vecstout.bits.sourceType := RSFeedbackType.tlbMiss
  io.vecstout.bits.flushState := DontCare
  io.vecstout.bits.trigger    := sx_last_in.output.uop.trigger
  io.vecstout.bits.nc := sx_last_in.nc
  io.vecstout.bits.mmio := sx_last_in.mmio
  io.vecstout.bits.exceptionVec := ExceptionNO.selectByFu(sx_last_in.output.uop.exceptionVec, VstuCfg)
  io.vecstout.bits.hasException := sx_last_in.hasException
  io.vecstout.bits.usSecondInv := sx_last_in.usSecondInv
  io.vecstout.bits.vecFeedback := sx_last_in.vecFeedback
  io.vecstout.bits.elemIdx     := sx_last_in.elemIdx
  io.vecstout.bits.alignedType := sx_last_in.alignedType
  io.vecstout.bits.mask        := sx_last_in.mask
  io.vecstout.bits.vaddr       := sx_last_in.vaddr
  io.vecstout.bits.vaNeedExt   := sx_last_in.vaNeedExt
  io.vecstout.bits.gpaddr      := sx_last_in.gpaddr
  io.vecstout.bits.isForVSnonLeafPTE     := sx_last_in.isForVSnonLeafPTE
  io.vecstout.bits.vstart      := sx_last_in.output.uop.vpu.vstart
  io.vecstout.bits.vecTriggerMask := sx_last_in.vecTriggerMask
  // io.vecstout.bits.reg_offset.map(_ := DontCare)
  // io.vecstout.bits.elemIdx.map(_ := sx_last_in.elemIdx)
  // io.vecstout.bits.elemIdxInsideVd.map(_ := DontCare)
  // io.vecstout.bits.vecdata.map(_ := DontCare)
  // io.vecstout.bits.mask.map(_ := DontCare)
  // io.vecstout.bits.alignedType.map(_ := sx_last_in.alignedType)

  io.debug_ls := DontCare
  io.debug_ls.s1_robIdx := s1_in.uop.robIdx.value
  io.debug_ls.s1_isTlbFirstMiss := io.tlb.resp.valid && io.tlb.resp.bits.miss && io.tlb.resp.bits.debug.isFirstIssue && !s1_in.isHWPrefetch

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
  XSPerfAccumulate("s0_vecin_fire",              s0_fire && s0_use_flow_vec)
  XSPerfAccumulate("s0_in_fire_first_issue",     s0_fire && s0_isFirstIssue)
  XSPerfAccumulate("s0_addr_spec_success",       s0_fire && !s0_use_flow_vec && s0_saddr(VAddrBits-1, 12) === s0_stin.src(0)(VAddrBits-1, 12))
  XSPerfAccumulate("s0_addr_spec_failed",        s0_fire && !s0_use_flow_vec && s0_saddr(VAddrBits-1, 12) =/= s0_stin.src(0)(VAddrBits-1, 12))
  XSPerfAccumulate("s0_addr_spec_success_once",  s0_fire && !s0_use_flow_vec && s0_saddr(VAddrBits-1, 12) === s0_stin.src(0)(VAddrBits-1, 12) && s0_isFirstIssue)
  XSPerfAccumulate("s0_addr_spec_failed_once",   s0_fire && !s0_use_flow_vec && s0_saddr(VAddrBits-1, 12) =/= s0_stin.src(0)(VAddrBits-1, 12) && s0_isFirstIssue)

  XSPerfAccumulate("s1_in_valid",                s1_valid)
  XSPerfAccumulate("s1_in_fire",                 s1_fire)
  XSPerfAccumulate("s1_in_fire_first_issue",     s1_fire && s1_in.isFirstIssue)
  XSPerfAccumulate("s1_tlb_miss",                s1_fire && s1_tlb_miss)
  XSPerfAccumulate("s1_tlb_miss_first_issue",    s1_fire && s1_tlb_miss && s1_in.isFirstIssue)
  // end
}
