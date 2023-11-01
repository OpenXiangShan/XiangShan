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
import xiangshan.backend.rob.{DebugLsInfoBundle, LsTopdownInfo, RobPtr}
import xiangshan.cache._
import xiangshan.cache.wpu.ReplayCarry
import xiangshan.cache.mmu.{TlbCmd, TlbReq, TlbRequestIO, TlbResp}
import xiangshan.mem.mdp._

class LoadToLsqReplayIO(implicit p: Parameters) extends XSBundle with HasDCacheParameters {
  // mshr refill index
  val mshr_id         = UInt(log2Up(cfg.nMissEntries).W)
  // get full data from store queue and sbuffer
  val full_fwd        = Bool()
  // wait for data from store inst's store queue index
  val data_inv_sq_idx = new SqPtr
  // wait for address from store queue index
  val addr_inv_sq_idx = new SqPtr
  // replay carry
  val rep_carry       = new ReplayCarry(nWays)
  // data in last beat
  val last_beat       = Bool()
  // replay cause
  val cause           = Vec(LoadReplayCauses.allCauses, Bool())
  // performance debug information
  val debug           = new PerfDebugInfo

  // alias
  def mem_amb       = cause(LoadReplayCauses.C_MA)
  def tlb_miss      = cause(LoadReplayCauses.C_TM)
  def fwd_fail      = cause(LoadReplayCauses.C_FF)
  def dcache_rep    = cause(LoadReplayCauses.C_DR)
  def dcache_miss   = cause(LoadReplayCauses.C_DM)
  def wpu_fail      = cause(LoadReplayCauses.C_WF)
  def bank_conflict = cause(LoadReplayCauses.C_BC)
  def rar_nack      = cause(LoadReplayCauses.C_RAR)
  def raw_nack      = cause(LoadReplayCauses.C_RAW)
  def nuke          = cause(LoadReplayCauses.C_NK)
  def need_rep      = cause.asUInt.orR
}


class LoadToLsqIO(implicit p: Parameters) extends XSBundle {
  val ldin            = DecoupledIO(new LqWriteBundle)
  val uncache         = Flipped(DecoupledIO(new ExuOutput))
  val ld_raw_data     = Input(new LoadDataFromLQBundle)
  val forward         = new PipeLoadForwardQueryIO
  val stld_nuke_query = new LoadNukeQueryIO
  val ldld_nuke_query = new LoadNukeQueryIO
  val trigger         = Flipped(new LqTriggerIO)
}

class LoadToLoadIO(implicit p: Parameters) extends XSBundle {
  val valid      = Bool()
  val data       = UInt(XLEN.W) // load to load fast path is limited to ld (64 bit) used as vaddr src1 only
  val dly_ld_err = Bool()
}

class LoadUnitTriggerIO(implicit p: Parameters) extends XSBundle {
  val tdata2      = Input(UInt(64.W))
  val matchType   = Input(UInt(2.W))
  val tEnable     = Input(Bool()) // timing is calculated before this
  val addrHit     = Output(Bool())
  val lastDataHit = Output(Bool())
}

class LoadUnit(implicit p: Parameters) extends XSModule
  with HasLoadHelper
  with HasPerfEvents
  with HasDCacheParameters
  with HasCircularQueuePtrHelper
{
  val io = IO(new Bundle() {
    // control
    val redirect      = Flipped(ValidIO(new Redirect))
    val csrCtrl       = Flipped(new CustomCSRCtrlIO)

    // int issue path
    val ldin          = Flipped(Decoupled(new ExuInput))
    val ldout         = Decoupled(new ExuOutput)
    val rsIdx         = Input(UInt())
    val isFirstIssue  = Input(Bool())

    // data path
    val tlb           = new TlbRequestIO(2)
    val pmp           = Flipped(new PMPRespBundle()) // arrive same to tlb now
    val dcache        = new DCacheLoadIO
    val sbuffer       = new LoadForwardQueryIO
    val lsq           = new LoadToLsqIO
    val tl_d_channel  = Input(new DcacheToLduForwardIO)
    val forward_mshr  = Flipped(new LduToMissqueueForwardIO)
    val refill        = Flipped(ValidIO(new Refill))
    val l2_hint       = Input(Valid(new L2ToL1Hint))

    // fast wakeup
    val fast_uop = ValidIO(new MicroOp) // early wakeup signal generated in load_s1, send to RS in load_s2

    // trigger
    val trigger = Vec(3, new LoadUnitTriggerIO)

    // prefetch
    val prefetch_train            = ValidIO(new LdPrefetchTrainBundle()) // provide prefetch info to sms
    val prefetch_train_l1         = ValidIO(new LdPrefetchTrainBundle()) // provide prefetch info to stream & stride
    val prefetch_req              = Flipped(ValidIO(new L1PrefetchReq)) // hardware prefetch to l1 cache req
    val canAcceptLowConfPrefetch  = Output(Bool())
    val canAcceptHighConfPrefetch = Output(Bool())

    // load to load fast path
    val l2l_fwd_in    = Input(new LoadToLoadIO)
    val l2l_fwd_out   = Output(new LoadToLoadIO)

    val ld_fast_match    = Input(Bool())
    val ld_fast_fuOpType = Input(UInt())
    val ld_fast_imm      = Input(UInt(12.W))

    // rs feedback
    val feedback_fast = ValidIO(new RSFeedback) // stage 2
    val feedback_slow = ValidIO(new RSFeedback) // stage 3

    // load ecc error
    val s3_dly_ld_err = Output(Bool()) // Note that io.s3_dly_ld_err and io.lsq.s3_dly_ld_err is different

    // schedule error query
    val stld_nuke_query = Flipped(Vec(StorePipelineWidth, Valid(new StoreNukeQueryIO)))

    // queue-based replay
    val replay       = Flipped(Decoupled(new LsPipelineBundle))
    val lq_rep_full  = Input(Bool())

    // misc
    val s2_ptr_chasing = Output(Bool()) // provide right pc for hw prefetch

    // Load fast replay path
    val fast_rep_in  = Flipped(Decoupled(new LqWriteBundle))
    val fast_rep_out = Decoupled(new LqWriteBundle)

    // perf
    val debug_ls         = Output(new DebugLsInfoBundle)
    val lsTopdownInfo    = Output(new LsTopdownInfo)
    val correctMissTrain = Input(Bool())
  })

  val s1_ready, s2_ready, s3_ready = WireInit(false.B)

  // Pipeline
  // --------------------------------------------------------------------------------
  // stage 0
  // --------------------------------------------------------------------------------
  // generate addr, use addr to query DCache and DTLB
  val s0_valid         = Wire(Bool())
  val s0_kill          = Wire(Bool())
  val s0_vaddr         = Wire(UInt(VAddrBits.W))
  val s0_mask          = Wire(UInt((VLEN/8).W))
  val s0_uop           = Wire(new MicroOp)
  val s0_has_rob_entry = Wire(Bool())
  val s0_rsIdx         = Wire(UInt(log2Up(IssQueSize).W))
  val s0_mshrid        = Wire(UInt())
  val s0_try_l2l       = Wire(Bool())
  val s0_rep_carry     = Wire(new ReplayCarry(nWays))
  val s0_isFirstIssue  = Wire(Bool())
  val s0_fast_rep      = Wire(Bool())
  val s0_ld_rep        = Wire(Bool())
  val s0_l2l_fwd       = Wire(Bool())
  val s0_sched_idx     = Wire(UInt())
  val s0_can_go        = s1_ready
  val s0_fire          = s0_valid && s0_can_go
  val s0_out           = Wire(new LqWriteBundle)

  // load flow select/gen
  // src0: super load replayed by LSQ (cache miss replay) (io.replay)
  // src1: fast load replay (io.fast_rep_in)
  // src2: load replayed by LSQ (io.replay)
  // src3: hardware prefetch from prefetchor (high confidence) (io.prefetch)
  // src4: int read / software prefetch first issue from RS (io.in)
  // src5: vec read first issue from RS (TODO)
  // src6: load try pointchaising when no issued or replayed load (io.fastpath)
  // src7: hardware prefetch from prefetchor (high confidence) (io.prefetch)
  // priority: high to low
  val s0_rep_stall           = io.ldin.valid && isAfter(io.replay.bits.uop.robIdx, io.ldin.bits.uop.robIdx)
  val s0_super_ld_rep_valid  = io.replay.valid && io.replay.bits.forward_tlDchannel
  val s0_ld_fast_rep_valid   = io.fast_rep_in.valid
  val s0_ld_rep_valid        = io.replay.valid && !io.replay.bits.forward_tlDchannel && !s0_rep_stall
  val s0_high_conf_prf_valid = io.prefetch_req.valid && io.prefetch_req.bits.confidence > 0.U
  val s0_int_iss_valid       = io.ldin.valid // int flow first issue or software prefetch
  val s0_vec_iss_valid       = WireInit(false.B) // TODO
  val s0_l2l_fwd_valid       = io.l2l_fwd_in.valid && io.ld_fast_match
  val s0_low_conf_prf_valid  = io.prefetch_req.valid && io.prefetch_req.bits.confidence === 0.U
  dontTouch(s0_super_ld_rep_valid)
  dontTouch(s0_ld_fast_rep_valid)
  dontTouch(s0_ld_rep_valid)
  dontTouch(s0_high_conf_prf_valid)
  dontTouch(s0_int_iss_valid)
  dontTouch(s0_vec_iss_valid)
  dontTouch(s0_l2l_fwd_valid)
  dontTouch(s0_low_conf_prf_valid)

  // load flow source ready
  val s0_super_ld_rep_ready  = WireInit(true.B)
  val s0_ld_fast_rep_ready   = !s0_super_ld_rep_valid
  val s0_ld_rep_ready        = !s0_super_ld_rep_valid &&
                               !s0_ld_fast_rep_valid
  val s0_high_conf_prf_ready = !s0_super_ld_rep_valid &&
                               !s0_ld_fast_rep_valid &&
                               !s0_ld_rep_valid

  val s0_int_iss_ready       = !s0_super_ld_rep_valid &&
                               !s0_ld_fast_rep_valid &&
                               !s0_ld_rep_valid &&
                               !s0_high_conf_prf_valid

  val s0_vec_iss_ready       = !s0_super_ld_rep_valid &&
                               !s0_ld_fast_rep_valid &&
                               !s0_ld_rep_valid &&
                               !s0_high_conf_prf_valid &&
                               !s0_int_iss_valid

  val s0_l2l_fwd_ready       = !s0_super_ld_rep_valid &&
                               !s0_ld_fast_rep_valid &&
                               !s0_ld_rep_valid &&
                               !s0_high_conf_prf_valid &&
                               !s0_int_iss_valid &&
                               !s0_vec_iss_valid

  val s0_low_conf_prf_ready  = !s0_super_ld_rep_valid &&
                               !s0_ld_fast_rep_valid &&
                               !s0_ld_rep_valid &&
                               !s0_high_conf_prf_valid &&
                               !s0_int_iss_valid &&
                               !s0_vec_iss_valid &&
                               !s0_l2l_fwd_valid
  dontTouch(s0_super_ld_rep_ready)
  dontTouch(s0_ld_fast_rep_ready)
  dontTouch(s0_ld_rep_ready)
  dontTouch(s0_high_conf_prf_ready)
  dontTouch(s0_int_iss_ready)
  dontTouch(s0_vec_iss_ready)
  dontTouch(s0_l2l_fwd_ready)
  dontTouch(s0_low_conf_prf_ready)

  // load flow source select (OH)
  val s0_super_ld_rep_select = s0_super_ld_rep_valid && s0_super_ld_rep_ready
  val s0_ld_fast_rep_select  = s0_ld_fast_rep_valid && s0_ld_fast_rep_ready
  val s0_ld_rep_select       = s0_ld_rep_valid && s0_ld_rep_ready
  val s0_hw_prf_select       = s0_high_conf_prf_ready && s0_high_conf_prf_valid ||
                               s0_low_conf_prf_ready && s0_low_conf_prf_valid
  val s0_int_iss_select      = s0_int_iss_ready && s0_int_iss_valid
  val s0_vec_iss_select      = s0_vec_iss_ready && s0_vec_iss_valid
  val s0_l2l_fwd_select      = s0_l2l_fwd_ready && s0_l2l_fwd_valid
  assert(!s0_vec_iss_select) // to be added
  dontTouch(s0_super_ld_rep_select)
  dontTouch(s0_ld_fast_rep_select)
  dontTouch(s0_ld_rep_select)
  dontTouch(s0_hw_prf_select)
  dontTouch(s0_int_iss_select)
  dontTouch(s0_vec_iss_select)
  dontTouch(s0_l2l_fwd_select)

  s0_valid := (s0_super_ld_rep_valid ||
               s0_ld_fast_rep_valid ||
               s0_ld_rep_valid ||
               s0_high_conf_prf_valid ||
               s0_int_iss_valid ||
               s0_vec_iss_valid ||
               s0_l2l_fwd_valid ||
               s0_low_conf_prf_valid) && io.dcache.req.ready && !s0_kill

  // which is S0's out is ready and dcache is ready
  val s0_try_ptr_chasing      = s0_l2l_fwd_select
  val s0_do_try_ptr_chasing   = s0_try_ptr_chasing && s0_can_go && io.dcache.req.ready
  val s0_ptr_chasing_vaddr    = io.l2l_fwd_in.data(5, 0) +& io.ld_fast_imm(5, 0)
  val s0_ptr_chasing_canceled = WireInit(false.B)
  s0_kill := s0_ptr_chasing_canceled || (s0_out.uop.robIdx.needFlush(io.redirect) && !s0_try_ptr_chasing)

  // prefetch related ctrl signal
  val s0_prf    = Wire(Bool())
  val s0_prf_rd = Wire(Bool())
  val s0_prf_wr = Wire(Bool())
  val s0_hw_prf = s0_hw_prf_select

  io.canAcceptLowConfPrefetch  := s0_low_conf_prf_ready
  io.canAcceptHighConfPrefetch := s0_high_conf_prf_ready

  // query DTLB
  io.tlb.req.valid                   := s0_valid
  io.tlb.req.bits.cmd                := Mux(s0_prf,
                                         Mux(s0_prf_wr, TlbCmd.write, TlbCmd.read),
                                         TlbCmd.read
                                       )
  io.tlb.req.bits.vaddr              := Mux(s0_hw_prf_select, io.prefetch_req.bits.paddr, s0_vaddr)
  io.tlb.req.bits.size               := LSUOpType.size(s0_uop.ctrl.fuOpType)
  io.tlb.req.bits.kill               := s0_kill
  io.tlb.req.bits.memidx.is_ld       := true.B
  io.tlb.req.bits.memidx.is_st       := false.B
  io.tlb.req.bits.memidx.idx         := s0_uop.lqIdx.value
  io.tlb.req.bits.debug.robIdx       := s0_uop.robIdx
  io.tlb.req.bits.no_translate       := s0_hw_prf_select  // hw b.reqetch addr does not need to be translated
  io.tlb.req.bits.debug.pc           := s0_uop.cf.pc
  io.tlb.req.bits.debug.isFirstIssue := s0_isFirstIssue

  // query DCache
  io.dcache.req.valid             := s0_valid
  io.dcache.req.bits.cmd          := Mux(s0_prf_rd,
                                      MemoryOpConstants.M_PFR,
                                      Mux(s0_prf_wr, MemoryOpConstants.M_PFW, MemoryOpConstants.M_XRD)
                                    )
  io.dcache.req.bits.vaddr        := s0_vaddr
  io.dcache.req.bits.mask         := s0_mask
  io.dcache.req.bits.data         := DontCare
  io.dcache.req.bits.isFirstIssue := s0_isFirstIssue
  io.dcache.req.bits.instrtype    := Mux(s0_prf, DCACHE_PREFETCH_SOURCE.U, LOAD_SOURCE.U)
  io.dcache.req.bits.debug_robIdx := s0_uop.robIdx.value
  io.dcache.req.bits.replayCarry  := s0_rep_carry
  io.dcache.req.bits.id           := DontCare // TODO: update cache meta
  io.dcache.pf_source             := Mux(s0_hw_prf_select, io.prefetch_req.bits.pf_source.value, L1_HW_PREFETCH_NULL)

  // load flow priority mux
  def fromNullSource() = {
    s0_vaddr         := 0.U
    s0_mask          := 0.U
    s0_uop           := 0.U.asTypeOf(new MicroOp)
    s0_try_l2l       := false.B
    s0_has_rob_entry := false.B
    s0_rsIdx         := 0.U
    s0_rep_carry     := 0.U.asTypeOf(s0_rep_carry.cloneType)
    s0_mshrid        := 0.U
    s0_isFirstIssue  := false.B
    s0_fast_rep      := false.B
    s0_ld_rep        := false.B
    s0_l2l_fwd       := false.B
    s0_prf           := false.B
    s0_prf_rd        := false.B
    s0_prf_wr        := false.B
    s0_sched_idx     := 0.U
  }

  def fromFastReplaySource(src: LqWriteBundle) = {
    s0_vaddr         := src.vaddr
    s0_mask          := src.mask
    s0_uop           := src.uop
    s0_try_l2l       := false.B
    s0_has_rob_entry := src.hasROBEntry
    s0_rep_carry     := src.rep_info.rep_carry
    s0_mshrid        := src.rep_info.mshr_id
    s0_rsIdx         := src.rsIdx
    s0_isFirstIssue  := false.B
    s0_fast_rep      := true.B
    s0_ld_rep        := src.isLoadReplay
    s0_l2l_fwd       := false.B
    s0_prf           := LSUOpType.isPrefetch(src.uop.ctrl.fuOpType)
    s0_prf_rd        := src.uop.ctrl.fuOpType === LSUOpType.prefetch_r
    s0_prf_wr        := src.uop.ctrl.fuOpType === LSUOpType.prefetch_w
    s0_sched_idx     := src.schedIndex
  }

  def fromNormalReplaySource(src: LsPipelineBundle) = {
    s0_vaddr         := src.vaddr
    s0_mask          := genVWmask(src.vaddr, src.uop.ctrl.fuOpType(1, 0))
    s0_uop           := src.uop
    s0_try_l2l       := false.B
    s0_has_rob_entry := true.B
    s0_rsIdx         := src.rsIdx
    s0_rep_carry     := src.replayCarry
    s0_mshrid        := src.mshrid
    s0_isFirstIssue  := false.B
    s0_fast_rep      := false.B
    s0_ld_rep        := true.B
    s0_l2l_fwd       := false.B
    s0_prf           := LSUOpType.isPrefetch(src.uop.ctrl.fuOpType)
    s0_prf_rd        := src.uop.ctrl.fuOpType === LSUOpType.prefetch_r
    s0_prf_wr        := src.uop.ctrl.fuOpType === LSUOpType.prefetch_w
    s0_sched_idx     := src.schedIndex
  }

  def fromPrefetchSource(src: L1PrefetchReq) = {
    s0_vaddr         := src.getVaddr()
    s0_mask          := 0.U
    s0_uop           := DontCare
    s0_try_l2l       := false.B
    s0_has_rob_entry := false.B
    s0_rsIdx         := 0.U
    s0_rep_carry     := 0.U.asTypeOf(s0_rep_carry.cloneType)
    s0_mshrid        := 0.U
    s0_isFirstIssue  := false.B
    s0_fast_rep      := false.B
    s0_ld_rep        := false.B
    s0_l2l_fwd       := false.B
    s0_prf           := true.B
    s0_prf_rd        := !src.is_store
    s0_prf_wr        := src.is_store
    s0_sched_idx     := 0.U
  }

  def fromIntIssueSource(src: ExuInput) = {
    s0_vaddr         := src.src(0) + SignExt(src.uop.ctrl.imm(11, 0), VAddrBits)
    s0_mask          := genVWmask(s0_vaddr, src.uop.ctrl.fuOpType(1,0))
    s0_uop           := src.uop
    s0_try_l2l       := false.B
    s0_has_rob_entry := true.B
    s0_rsIdx         := io.rsIdx
    s0_rep_carry     := 0.U.asTypeOf(s0_rep_carry.cloneType)
    s0_mshrid        := 0.U
    s0_isFirstIssue  := true.B
    s0_fast_rep      := false.B
    s0_ld_rep        := false.B
    s0_l2l_fwd       := false.B
    s0_prf           := LSUOpType.isPrefetch(src.uop.ctrl.fuOpType)
    s0_prf_rd        := src.uop.ctrl.fuOpType === LSUOpType.prefetch_r
    s0_prf_wr        := src.uop.ctrl.fuOpType === LSUOpType.prefetch_w
    s0_sched_idx     := 0.U
  }

  def fromVecIssueSource() = {
    s0_vaddr         := 0.U
    s0_mask          := 0.U
    s0_uop           := 0.U.asTypeOf(new MicroOp)
    s0_try_l2l       := false.B
    s0_has_rob_entry := false.B
    s0_rsIdx         := 0.U
    s0_rep_carry     := 0.U.asTypeOf(s0_rep_carry.cloneType)
    s0_mshrid        := 0.U
    s0_isFirstIssue  := false.B
    s0_fast_rep      := false.B
    s0_ld_rep        := false.B
    s0_l2l_fwd       := false.B
    s0_prf           := false.B
    s0_prf_rd        := false.B
    s0_prf_wr        := false.B
    s0_sched_idx     := 0.U
  }

  def fromLoadToLoadSource(src: LoadToLoadIO) = {
    s0_vaddr              := Cat(src.data(XLEN-1, 6), s0_ptr_chasing_vaddr(5,0))
    s0_mask               := genVWmask(s0_vaddr, io.ld_fast_fuOpType(1, 0))
    // When there's no valid instruction from RS and LSQ, we try the load-to-load forwarding.
    // Assume the pointer chasing is always ld.
    s0_uop.ctrl.fuOpType  := io.ld_fast_fuOpType
    s0_try_l2l            := true.B
    // we dont care s0_isFirstIssue and s0_rsIdx and s0_sqIdx in S0 when trying pointchasing
    // because these signals will be updated in S1
    s0_has_rob_entry      := false.B
    s0_rsIdx              := 0.U
    s0_mshrid             := 0.U
    s0_rep_carry          := 0.U.asTypeOf(s0_rep_carry.cloneType)
    s0_isFirstIssue       := true.B
    s0_fast_rep           := false.B
    s0_ld_rep             := false.B
    s0_l2l_fwd            := true.B
    s0_prf                := false.B
    s0_prf_rd             := false.B
    s0_prf_wr             := false.B
    s0_sched_idx          := 0.U
  }

  // set default
  s0_uop := DontCare
  when (s0_super_ld_rep_select)      { fromNormalReplaySource(io.replay.bits)     }
  .elsewhen (s0_ld_fast_rep_select)  { fromFastReplaySource(io.fast_rep_in.bits)  }
  .elsewhen (s0_ld_rep_select)       { fromNormalReplaySource(io.replay.bits)     }
  .elsewhen (s0_hw_prf_select)       { fromPrefetchSource(io.prefetch_req.bits)   }
  .elsewhen (s0_int_iss_select)      { fromIntIssueSource(io.ldin.bits)           }
  .elsewhen (s0_vec_iss_select)      { fromVecIssueSource()                       }
  .otherwise {
    if (EnableLoadToLoadForward) {
      fromLoadToLoadSource(io.l2l_fwd_in)
    } else {
      fromNullSource()
    }
  }

  // address align check
  val s0_addr_aligned = LookupTree(s0_uop.ctrl.fuOpType(1, 0), List(
    "b00".U   -> true.B,                   //b
    "b01".U   -> (s0_vaddr(0)    === 0.U), //h
    "b10".U   -> (s0_vaddr(1, 0) === 0.U), //w
    "b11".U   -> (s0_vaddr(2, 0) === 0.U)  //d
  ))

  // accept load flow if dcache ready (tlb is always ready)
  // TODO: prefetch need writeback to loadQueueFlag
  s0_out               := DontCare
  s0_out.rsIdx         := s0_rsIdx
  s0_out.vaddr         := s0_vaddr
  s0_out.mask          := s0_mask
  s0_out.uop           := s0_uop
  s0_out.isFirstIssue  := s0_isFirstIssue
  s0_out.hasROBEntry   := s0_has_rob_entry
  s0_out.isPrefetch    := s0_prf
  s0_out.isHWPrefetch  := s0_hw_prf
  s0_out.isFastReplay  := s0_fast_rep
  s0_out.isLoadReplay  := s0_ld_rep
  s0_out.isFastPath    := s0_l2l_fwd
  s0_out.mshrid        := s0_mshrid
  s0_out.uop.cf.exceptionVec(loadAddrMisaligned) := !s0_addr_aligned
  s0_out.forward_tlDchannel := s0_super_ld_rep_select
  when(io.tlb.req.valid && s0_isFirstIssue) {
    s0_out.uop.debugInfo.tlbFirstReqTime := GTimer()
  }.otherwise{
    s0_out.uop.debugInfo.tlbFirstReqTime := s0_uop.debugInfo.tlbFirstReqTime
  }
  s0_out.schedIndex     := s0_sched_idx

  // load fast replay
  io.fast_rep_in.ready := (s0_can_go && io.dcache.req.ready && s0_ld_fast_rep_ready)

  // load flow source ready
  // cache missed load has highest priority
  // always accept cache missed load flow from load replay queue
  io.replay.ready := (s0_can_go && io.dcache.req.ready && (s0_ld_rep_ready && !s0_rep_stall || s0_super_ld_rep_select))

  // accept load flow from rs when:
  // 1) there is no lsq-replayed load
  // 2) there is no fast replayed load
  // 3) there is no high confidence prefetch request
  io.ldin.ready := (s0_can_go && io.dcache.req.ready && s0_int_iss_ready)

  // for hw prefetch load flow feedback, to be added later
  // io.prefetch_in.ready := s0_hw_prf_select

  // dcache replacement extra info
  // TODO: should prefetch load update replacement?
  io.dcache.replacementUpdated := Mux(s0_ld_rep_select || s0_super_ld_rep_select, io.replay.bits.replacementUpdated, false.B)

  XSDebug(io.dcache.req.fire,
    p"[DCACHE LOAD REQ] pc ${Hexadecimal(s0_uop.cf.pc)}, vaddr ${Hexadecimal(s0_vaddr)}\n"
  )
  XSDebug(s0_valid,
    p"S0: pc ${Hexadecimal(s0_out.uop.cf.pc)}, lId ${Hexadecimal(s0_out.uop.lqIdx.asUInt)}, " +
    p"vaddr ${Hexadecimal(s0_out.vaddr)}, mask ${Hexadecimal(s0_out.mask)}\n")

  // Pipeline
  // --------------------------------------------------------------------------------
  // stage 1
  // --------------------------------------------------------------------------------
  // TLB resp (send paddr to dcache)
  val s1_valid      = RegInit(false.B)
  val s1_in         = Wire(new LqWriteBundle)
  val s1_out        = Wire(new LqWriteBundle)
  val s1_kill       = Wire(Bool())
  val s1_can_go     = s2_ready
  val s1_fire       = s1_valid && !s1_kill && s1_can_go

  s1_ready := !s1_valid || s1_kill || s2_ready
  when (s0_fire) { s1_valid := true.B }
  .elsewhen (s1_fire) { s1_valid := false.B }
  .elsewhen (s1_kill) { s1_valid := false.B }
  s1_in   := RegEnable(s0_out, s0_fire)

  val s1_fast_rep_dly_err = RegNext(io.fast_rep_in.bits.delayedLoadError)
  val s1_fast_rep_kill    = s1_fast_rep_dly_err && s1_in.isFastReplay
  val s1_l2l_fwd_dly_err  = RegNext(io.l2l_fwd_in.dly_ld_err)
  val s1_l2l_fwd_kill     = s1_l2l_fwd_dly_err && s1_in.isFastPath
  val s1_late_kill        = s1_fast_rep_kill || s1_l2l_fwd_kill
  val s1_vaddr_hi         = Wire(UInt())
  val s1_vaddr_lo         = Wire(UInt())
  val s1_vaddr            = Wire(UInt())
  val s1_paddr_dup_lsu    = Wire(UInt())
  val s1_paddr_dup_dcache = Wire(UInt())
  val s1_exception        = ExceptionNO.selectByFu(s1_out.uop.cf.exceptionVec, lduCfg).asUInt.orR   // af & pf exception were modified below.
  val s1_tlb_miss         = io.tlb.resp.bits.miss
  val s1_prf              = s1_in.isPrefetch
  val s1_hw_prf           = s1_in.isHWPrefetch
  val s1_sw_prf           = s1_prf && !s1_hw_prf
  val s1_tlb_memidx       = io.tlb.resp.bits.memidx

  s1_vaddr_hi         := s1_in.vaddr(VAddrBits - 1, 6)
  s1_vaddr_lo         := s1_in.vaddr(5, 0)
  s1_vaddr            := Cat(s1_vaddr_hi, s1_vaddr_lo)
  s1_paddr_dup_lsu    := io.tlb.resp.bits.paddr(0)
  s1_paddr_dup_dcache := io.tlb.resp.bits.paddr(1)

  when (s1_tlb_memidx.is_ld && io.tlb.resp.valid && !s1_tlb_miss && s1_tlb_memidx.idx === s1_in.uop.lqIdx.value) {
    // printf("load idx = %d\n", s1_tlb_memidx.idx)
    s1_out.uop.debugInfo.tlbRespTime := GTimer()
  }

  io.tlb.req_kill   := s1_kill
  io.tlb.resp.ready := true.B

  io.dcache.s1_paddr_dup_lsu    <> s1_paddr_dup_lsu
  io.dcache.s1_paddr_dup_dcache <> s1_paddr_dup_dcache
  io.dcache.s1_kill             := s1_kill || s1_tlb_miss || s1_exception

  // store to load forwarding
  io.sbuffer.valid := s1_valid && !(s1_exception || s1_tlb_miss || s1_kill || s1_prf)
  io.sbuffer.vaddr := s1_vaddr
  io.sbuffer.paddr := s1_paddr_dup_lsu
  io.sbuffer.uop   := s1_in.uop
  io.sbuffer.sqIdx := s1_in.uop.sqIdx
  io.sbuffer.mask  := s1_in.mask
  io.sbuffer.pc    := s1_in.uop.cf.pc // FIXME: remove it

  io.lsq.forward.valid     := s1_valid && !(s1_exception || s1_tlb_miss || s1_kill || s1_prf)
  io.lsq.forward.vaddr     := s1_vaddr
  io.lsq.forward.paddr     := s1_paddr_dup_lsu
  io.lsq.forward.uop       := s1_in.uop
  io.lsq.forward.sqIdx     := s1_in.uop.sqIdx
  io.lsq.forward.sqIdxMask := 0.U
  io.lsq.forward.mask      := s1_in.mask
  io.lsq.forward.pc        := s1_in.uop.cf.pc // FIXME: remove it

  // st-ld violation query
  val s1_nuke = VecInit((0 until StorePipelineWidth).map(w => {
                       io.stld_nuke_query(w).valid && // query valid
                       isAfter(s1_in.uop.robIdx, io.stld_nuke_query(w).bits.robIdx) && // older store
                       // TODO: Fix me when vector instruction
                       (s1_paddr_dup_lsu(PAddrBits-1, 3) === io.stld_nuke_query(w).bits.paddr(PAddrBits-1, 3)) && // paddr match
                       (s1_in.mask & io.stld_nuke_query(w).bits.mask).orR // data mask contain
                      })).asUInt.orR && !s1_tlb_miss

  s1_out                   := s1_in
  s1_out.vaddr             := s1_vaddr
  s1_out.paddr             := s1_paddr_dup_lsu
  s1_out.tlbMiss           := s1_tlb_miss
  s1_out.ptwBack           := io.tlb.resp.bits.ptwBack
  s1_out.rsIdx             := s1_in.rsIdx
  s1_out.rep_info.debug    := s1_in.uop.debugInfo
  s1_out.rep_info.nuke     := s1_nuke && !s1_sw_prf
  s1_out.lateKill          := s1_late_kill

  when (!s1_late_kill) {
    // current ori test will cause the case of ldest == 0, below will be modifeid in the future.
    // af & pf exception were modified
    s1_out.uop.cf.exceptionVec(loadPageFault)   := io.tlb.resp.bits.excp(0).pf.ld
    s1_out.uop.cf.exceptionVec(loadAccessFault) := io.tlb.resp.bits.excp(0).af.ld
  } .otherwise {
    s1_out.uop.cf.exceptionVec(loadAddrMisaligned) := false.B
    s1_out.uop.cf.exceptionVec(loadAccessFault)    := s1_late_kill
  }

  // pointer chasing
  val s1_try_ptr_chasing       = RegNext(s0_do_try_ptr_chasing, false.B)
  val s1_ptr_chasing_vaddr     = RegEnable(s0_ptr_chasing_vaddr, s0_do_try_ptr_chasing)
  val s1_fu_op_type_not_ld     = WireInit(false.B)
  val s1_not_fast_match        = WireInit(false.B)
  val s1_addr_mismatch         = WireInit(false.B)
  val s1_addr_misaligned       = WireInit(false.B)
  val s1_ptr_chasing_canceled  = WireInit(false.B)
  val s1_cancel_ptr_chasing    = WireInit(false.B)

  s1_kill := s1_late_kill ||
             s1_cancel_ptr_chasing ||
             s1_in.uop.robIdx.needFlush(io.redirect) ||
             RegEnable(s0_kill, false.B, io.ldin.valid || io.replay.valid || io.l2l_fwd_in.valid || io.fast_rep_in.valid)

  if (EnableLoadToLoadForward) {
    // Sometimes, we need to cancel the load-load forwarding.
    // These can be put at S0 if timing is bad at S1.
    // Case 0: CACHE_SET(base + offset) != CACHE_SET(base) (lowest 6-bit addition has an overflow)
    s1_addr_mismatch      := s1_ptr_chasing_vaddr(6) || RegEnable(io.ld_fast_imm(11, 6).orR, s0_do_try_ptr_chasing)
    // Case 1: the address is misaligned, kill s1
    s1_addr_misaligned    := LookupTree(s1_in.uop.ctrl.fuOpType(1, 0), List(
                             "b00".U   -> false.B,                   //b
                             "b01".U   -> (s1_vaddr(0)    =/= 0.U), //h
                             "b10".U   -> (s1_vaddr(1, 0) =/= 0.U), //w
                             "b11".U   -> (s1_vaddr(2, 0) =/= 0.U)  //d
                          ))
    // Case 2: this load-load uop is cancelled
    s1_ptr_chasing_canceled := !io.ldin.valid

    when (s1_try_ptr_chasing) {
      s1_cancel_ptr_chasing := s1_addr_mismatch || s1_addr_misaligned || s1_ptr_chasing_canceled

      s1_in.uop           := io.ldin.bits.uop
      s1_in.rsIdx         := io.rsIdx
      s1_in.isFirstIssue  := io.isFirstIssue
      s1_vaddr_lo         := s1_ptr_chasing_vaddr(5, 0)
      s1_paddr_dup_lsu    := Cat(io.tlb.resp.bits.paddr(0)(PAddrBits - 1, 6), s1_vaddr_lo)
      s1_paddr_dup_dcache := Cat(io.tlb.resp.bits.paddr(0)(PAddrBits - 1, 6), s1_vaddr_lo)

      // recored tlb time when get the data to ensure the correctness of the latency calculation (although it should not record in here, because it does not use tlb)
      s1_in.uop.debugInfo.tlbFirstReqTime := GTimer()
      s1_in.uop.debugInfo.tlbRespTime     := GTimer()
    }
    when (!s1_cancel_ptr_chasing) {
      s0_ptr_chasing_canceled := s1_try_ptr_chasing && !io.replay.fire && !io.fast_rep_in.fire
      when (s1_try_ptr_chasing) {
        io.ldin.ready := true.B
      }
    }
  }

  // pre-calcuate sqIdx mask in s0, then send it to lsq in s1 for forwarding
  val s1_sqIdx_mask = RegNext(UIntToMask(s0_out.uop.sqIdx.value, StoreQueueSize))
  // to enable load-load, sqIdxMask must be calculated based on ldin.uop
  // If the timing here is not OK, load-load forwarding has to be disabled.
  // Or we calculate sqIdxMask at RS??
  io.lsq.forward.sqIdxMask := s1_sqIdx_mask
  if (EnableLoadToLoadForward) {
    when (s1_try_ptr_chasing) {
      io.lsq.forward.sqIdxMask := UIntToMask(io.ldin.bits.uop.sqIdx.value, StoreQueueSize)
    }
  }

  io.forward_mshr.valid  := s1_valid && s1_out.forward_tlDchannel
  io.forward_mshr.mshrid := s1_out.mshrid
  io.forward_mshr.paddr  := s1_out.paddr

  XSDebug(s1_valid,
    p"S1: pc ${Hexadecimal(s1_out.uop.cf.pc)}, lId ${Hexadecimal(s1_out.uop.lqIdx.asUInt)}, tlb_miss ${io.tlb.resp.bits.miss}, " +
    p"paddr ${Hexadecimal(s1_out.paddr)}, mmio ${s1_out.mmio}\n")

  // Pipeline
  // --------------------------------------------------------------------------------
  // stage 2
  // --------------------------------------------------------------------------------
  // s2: DCache resp
  val s2_valid  = RegInit(false.B)
  val s2_in     = Wire(new LqWriteBundle)
  val s2_out    = Wire(new LqWriteBundle)
  val s2_kill   = Wire(Bool())
  val s2_can_go = s3_ready
  val s2_fire   = s2_valid && !s2_kill && s2_can_go

  s2_kill := s2_in.uop.robIdx.needFlush(io.redirect)
  s2_ready := !s2_valid || s2_kill || s3_ready
  when (s1_fire) { s2_valid := true.B }
  .elsewhen (s2_fire) { s2_valid := false.B }
  .elsewhen (s2_kill) { s2_valid := false.B }
  s2_in := RegEnable(s1_out, s1_fire)

  val s2_pmp = WireInit(io.pmp)

  val s2_prf    = s2_in.isPrefetch
  val s2_hw_prf = s2_in.isHWPrefetch

  // exception that may cause load addr to be invalid / illegal
  // if such exception happen, that inst and its exception info
  // will be force writebacked to rob
  val s2_exception_vec = WireInit(s2_in.uop.cf.exceptionVec)
  when (!s2_in.lateKill) {
    s2_exception_vec(loadAccessFault) := s2_in.uop.cf.exceptionVec(loadAccessFault) || s2_pmp.ld
    // soft prefetch will not trigger any exception (but ecc error interrupt may be triggered)
    when (s2_prf || s2_in.tlbMiss) {
      s2_exception_vec := 0.U.asTypeOf(s2_exception_vec.cloneType)
    }
  }
  val s2_exception = ExceptionNO.selectByFu(s2_exception_vec, lduCfg).asUInt.orR

  val (s2_fwd_frm_d_chan, s2_fwd_data_frm_d_chan) = io.tl_d_channel.forward(s1_valid && s1_out.forward_tlDchannel, s1_out.mshrid, s1_out.paddr)
  val (s2_fwd_data_valid, s2_fwd_frm_mshr, s2_fwd_data_frm_mshr) = io.forward_mshr.forward()
  val s2_fwd_frm_d_chan_or_mshr = s2_fwd_data_valid && (s2_fwd_frm_d_chan || s2_fwd_frm_mshr)

  // writeback access fault caused by ecc error / bus error
  // * ecc data error is slow to generate, so we will not use it until load stage 3
  // * in load stage 3, an extra signal io.load_error will be used to
  val s2_actually_mmio = s2_pmp.mmio
  val s2_mmio          = !s2_prf &&
                          s2_actually_mmio &&
                         !s2_exception &&
                         !s2_in.tlbMiss

  val s2_full_fwd      = Wire(Bool())
  val s2_mem_amb       = s2_in.uop.cf.storeSetHit &&
                         io.lsq.forward.addrInvalid

  val s2_tlb_miss      = s2_in.tlbMiss
  val s2_fwd_fail      = io.lsq.forward.dataInvalid
  val s2_dcache_miss   = io.dcache.resp.bits.miss &&
                         !s2_fwd_frm_d_chan_or_mshr &&
                         !s2_full_fwd

  val s2_mq_nack       = io.dcache.s2_mq_nack &&
                         !s2_fwd_frm_d_chan_or_mshr &&
                         !s2_full_fwd

  val s2_bank_conflict = io.dcache.s2_bank_conflict &&
                         !s2_fwd_frm_d_chan_or_mshr &&
                         !s2_full_fwd

  val s2_wpu_pred_fail = io.dcache.s2_wpu_pred_fail &&
                        !s2_fwd_frm_d_chan_or_mshr &&
                        !s2_full_fwd

  val s2_rar_nack      = io.lsq.ldld_nuke_query.req.valid &&
                         !io.lsq.ldld_nuke_query.req.ready

  val s2_raw_nack      = io.lsq.stld_nuke_query.req.valid &&
                         !io.lsq.stld_nuke_query.req.ready
  // st-ld violation query
  //  NeedFastRecovery Valid when
  //  1. Fast recovery query request Valid.
  //  2. Load instruction is younger than requestors(store instructions).
  //  3. Physical address match.
  //  4. Data contains.
  val s2_nuke          = VecInit((0 until StorePipelineWidth).map(w => {
                          io.stld_nuke_query(w).valid && // query valid
                          isAfter(s2_in.uop.robIdx, io.stld_nuke_query(w).bits.robIdx) && // older store
                          // TODO: Fix me when vector instruction
                          (s2_in.paddr(PAddrBits-1, 3) === io.stld_nuke_query(w).bits.paddr(PAddrBits-1, 3)) && // paddr match
                          (s2_in.mask & io.stld_nuke_query(w).bits.mask).orR // data mask contain
                        })).asUInt.orR && !s2_tlb_miss || s2_in.rep_info.nuke

  val s2_cache_handled   = io.dcache.resp.bits.handled
  val s2_cache_tag_error = RegNext(io.csrCtrl.cache_error_enable) &&
                           io.dcache.resp.bits.tag_error

  val s2_troublem        = !s2_exception &&
                           !s2_mmio &&
                           !s2_prf &&
                           !s2_in.lateKill

  io.dcache.resp.ready  := true.B
  val s2_dcache_should_resp = !(s2_in.tlbMiss || s2_exception || s2_mmio || s2_prf || s2_in.lateKill)
  assert(!(s2_valid && (s2_dcache_should_resp && !io.dcache.resp.valid)), "DCache response got lost")

  // fast replay require
  val s2_dcache_fast_rep = (s2_mq_nack || !s2_dcache_miss && (s2_bank_conflict || s2_wpu_pred_fail))
  val s2_nuke_fast_rep   = !s2_mq_nack &&
                           !s2_dcache_miss &&
                           !s2_bank_conflict &&
                           !s2_wpu_pred_fail &&
                           !s2_rar_nack &&
                           !s2_raw_nack &&
                           s2_nuke

  val s2_fast_rep = !s2_mem_amb &&
                    !s2_tlb_miss &&
                    !s2_fwd_fail &&
                    (s2_dcache_fast_rep || s2_nuke_fast_rep) &&
                    s2_troublem

  // need allocate new entry
  val s2_can_query = !s2_mem_amb &&
                     !s2_tlb_miss  &&
                     !s2_fwd_fail &&
                     !s2_dcache_fast_rep &&
                     s2_troublem

  val s2_data_fwded = s2_dcache_miss && (s2_full_fwd || s2_cache_tag_error)

  // ld-ld violation require
  io.lsq.ldld_nuke_query.req.valid           := s2_valid && s2_can_query
  io.lsq.ldld_nuke_query.req.bits.uop        := s2_in.uop
  io.lsq.ldld_nuke_query.req.bits.mask       := s2_in.mask
  io.lsq.ldld_nuke_query.req.bits.paddr      := s2_in.paddr
  io.lsq.ldld_nuke_query.req.bits.data_valid := Mux(s2_full_fwd || s2_fwd_data_valid, true.B, !s2_dcache_miss)

  // st-ld violation require
  io.lsq.stld_nuke_query.req.valid           := s2_valid && s2_can_query
  io.lsq.stld_nuke_query.req.bits.uop        := s2_in.uop
  io.lsq.stld_nuke_query.req.bits.mask       := s2_in.mask
  io.lsq.stld_nuke_query.req.bits.paddr      := s2_in.paddr
  io.lsq.stld_nuke_query.req.bits.data_valid := Mux(s2_full_fwd || s2_fwd_data_valid, true.B, !s2_dcache_miss)

  // merge forward result
  // lsq has higher priority than sbuffer
  val s2_fwd_mask = Wire(Vec((VLEN/8), Bool()))
  val s2_fwd_data = Wire(Vec((VLEN/8), UInt(8.W)))
  s2_full_fwd := ((~s2_fwd_mask.asUInt).asUInt & s2_in.mask) === 0.U && !io.lsq.forward.dataInvalid
  // generate XLEN/8 Muxs
  for (i <- 0 until VLEN / 8) {
    s2_fwd_mask(i) := io.lsq.forward.forwardMask(i) || io.sbuffer.forwardMask(i)
    s2_fwd_data(i) := Mux(io.lsq.forward.forwardMask(i), io.lsq.forward.forwardData(i), io.sbuffer.forwardData(i))
  }

  XSDebug(s2_fire, "[FWD LOAD RESP] pc %x fwd %x(%b) + %x(%b)\n",
    s2_in.uop.cf.pc,
    io.lsq.forward.forwardData.asUInt, io.lsq.forward.forwardMask.asUInt,
    s2_in.forwardData.asUInt, s2_in.forwardMask.asUInt
  )

  //
  s2_out                     := s2_in
  s2_out.data                := 0.U // data will be generated in load s3
  s2_out.uop.ctrl.fpWen      := s2_in.uop.ctrl.fpWen && !s2_exception
  s2_out.mmio                := s2_mmio
  s2_out.uop.ctrl.flushPipe  := false.B
  s2_out.uop.cf.exceptionVec := s2_exception_vec
  s2_out.forwardMask         := s2_fwd_mask
  s2_out.forwardData         := s2_fwd_data
  s2_out.handledByMSHR       := s2_cache_handled
  s2_out.miss                := s2_dcache_miss && s2_troublem
  s2_out.feedbacked          := io.feedback_fast.valid

  // Generate replay signal caused by:
  // * st-ld violation check
  // * tlb miss
  // * dcache replay
  // * forward data invalid
  // * dcache miss
  s2_out.rep_info.mem_amb         := s2_mem_amb && s2_troublem
  s2_out.rep_info.tlb_miss        := s2_tlb_miss && s2_troublem
  s2_out.rep_info.fwd_fail        := s2_fwd_fail && s2_troublem
  s2_out.rep_info.dcache_rep      := s2_mq_nack && s2_troublem
  s2_out.rep_info.dcache_miss     := s2_dcache_miss && s2_troublem
  s2_out.rep_info.bank_conflict   := s2_bank_conflict && s2_troublem
  s2_out.rep_info.wpu_fail        := s2_wpu_pred_fail && s2_troublem
  s2_out.rep_info.rar_nack        := s2_rar_nack && s2_troublem
  s2_out.rep_info.raw_nack        := s2_raw_nack && s2_troublem
  s2_out.rep_info.nuke            := s2_nuke && s2_troublem
  s2_out.rep_info.full_fwd        := s2_data_fwded
  s2_out.rep_info.data_inv_sq_idx := io.lsq.forward.dataInvalidSqIdx
  s2_out.rep_info.addr_inv_sq_idx := io.lsq.forward.addrInvalidSqIdx
  s2_out.rep_info.rep_carry       := io.dcache.resp.bits.replayCarry
  s2_out.rep_info.mshr_id         := io.dcache.resp.bits.mshr_id
  s2_out.rep_info.last_beat       := s2_in.paddr(log2Up(refillBytes))
  s2_out.rep_info.debug           := s2_in.uop.debugInfo

  // if forward fail, replay this inst from fetch
  val debug_fwd_fail_rep = s2_fwd_fail && !s2_troublem && !s2_in.tlbMiss
  // if ld-ld violation is detected, replay from this inst from fetch
  val debug_ldld_nuke_rep = false.B // s2_ldld_violation && !s2_mmio && !s2_is_prefetch && !s2_in.tlbMiss
  // io.out.bits.uop.ctrl.replayInst := false.B

  // to be removed
  io.feedback_fast.valid                 := s2_valid &&                 // inst is valid
                                            !s2_in.isLoadReplay &&      // already feedbacked
                                            io.lq_rep_full &&           // LoadQueueReplay is full
                                            s2_out.rep_info.need_rep && // need replay
                                            !s2_exception &&            // no exception is triggered
                                            !s2_hw_prf                  // not hardware prefetch
  io.feedback_fast.bits.hit              := false.B
  io.feedback_fast.bits.flushState       := s2_in.ptwBack
  io.feedback_fast.bits.rsIdx            := s2_in.rsIdx
  io.feedback_fast.bits.sourceType       := RSFeedbackType.lrqFull
  io.feedback_fast.bits.dataInvalidSqIdx := DontCare

  // fast wakeup
  io.fast_uop.valid := RegNext(
    !io.dcache.s1_disable_fast_wakeup &&
    s1_valid &&
    !s1_kill &&
    !io.tlb.resp.bits.miss &&
    !io.lsq.forward.dataInvalidFast
  ) && (s2_valid && !s2_out.rep_info.need_rep && !s2_mmio)
  io.fast_uop.bits := RegNext(s1_out.uop)

  //
  io.s2_ptr_chasing                    := RegEnable(s1_try_ptr_chasing && !s1_cancel_ptr_chasing, false.B, s1_fire)

  io.prefetch_train.valid              := s2_valid && !s2_actually_mmio && !s2_in.tlbMiss
  io.prefetch_train.bits.fromLsPipelineBundle(s2_in)
  io.prefetch_train.bits.miss          := io.dcache.resp.bits.miss // TODO: use trace with bank conflict?
  io.prefetch_train.bits.meta_prefetch := io.dcache.resp.bits.meta_prefetch
  io.prefetch_train.bits.meta_access   := io.dcache.resp.bits.meta_access


  io.prefetch_train_l1.valid              := s2_valid && !s2_actually_mmio
  io.prefetch_train_l1.bits.fromLsPipelineBundle(s2_in)
  io.prefetch_train_l1.bits.miss          := io.dcache.resp.bits.miss
  io.prefetch_train_l1.bits.meta_prefetch := io.dcache.resp.bits.meta_prefetch
  io.prefetch_train_l1.bits.meta_access   := io.dcache.resp.bits.meta_access
  if (env.FPGAPlatform){
    io.dcache.s0_pc := DontCare
    io.dcache.s1_pc := DontCare
    io.dcache.s2_pc := DontCare
  }else{
    io.dcache.s0_pc := s0_out.uop.cf.pc
    io.dcache.s1_pc := s1_out.uop.cf.pc
    io.dcache.s2_pc := s2_out.uop.cf.pc
  }
  io.dcache.s2_kill := s2_pmp.ld || s2_actually_mmio || s2_kill

  val s1_ld_left_fire = s1_valid && !s1_kill && s2_ready
  val s2_ld_valid_dup = RegInit(0.U(6.W))
  s2_ld_valid_dup := 0x0.U(6.W)
  when (s1_ld_left_fire && !s1_out.isHWPrefetch) { s2_ld_valid_dup := 0x3f.U(6.W) }
  when (s1_kill || s1_out.isHWPrefetch) { s2_ld_valid_dup := 0x0.U(6.W) }
  assert(RegNext((s2_valid === s2_ld_valid_dup(0)) || RegNext(s1_out.isHWPrefetch)))

  // Pipeline
  // --------------------------------------------------------------------------------
  // stage 3
  // --------------------------------------------------------------------------------
  // writeback and update load queue
  val s3_valid        = RegNext(s2_valid && !s2_out.isHWPrefetch && !s2_out.uop.robIdx.needFlush(io.redirect))
  val s3_in           = RegEnable(s2_out, s2_fire)
  val s3_out          = Wire(Valid(new ExuOutput))
  val s3_dcache_rep   = RegEnable(s2_dcache_fast_rep && s2_troublem, false.B, s2_fire)
  val s3_ld_valid_dup = RegEnable(s2_ld_valid_dup, s2_fire)
  val s3_fast_rep     = Wire(Bool())
  val s3_troublem     = RegNext(s2_troublem)
  val s3_kill         = s3_in.uop.robIdx.needFlush(io.redirect)
  s3_ready := !s3_valid || s3_kill || io.ldout.ready

  // forwrad last beat
  val (s3_fwd_frm_d_chan, s3_fwd_data_frm_d_chan) = io.tl_d_channel.forward(s2_valid && s2_out.forward_tlDchannel, s2_out.mshrid, s2_out.paddr)
  val s3_fwd_data_valid = RegEnable(s2_fwd_data_valid, false.B, s2_valid)
  val s3_fwd_frm_d_chan_valid = (s3_fwd_frm_d_chan && s3_fwd_data_valid)

  // s3 load fast replay
  io.fast_rep_out.valid := s3_valid && s3_fast_rep && !s3_in.uop.robIdx.needFlush(io.redirect)
  io.fast_rep_out.bits := s3_in

  io.lsq.ldin.valid := s3_valid && (!s3_fast_rep || !io.fast_rep_out.ready) && !s3_in.feedbacked && !s3_in.lateKill
  io.lsq.ldin.bits := s3_in
  io.lsq.ldin.bits.miss := s3_in.miss && !s3_fwd_frm_d_chan_valid

  /* <------- DANGEROUS: Don't change sequence here ! -------> */
  io.lsq.ldin.bits.data_wen_dup := s3_ld_valid_dup.asBools
  io.lsq.ldin.bits.replacementUpdated := io.dcache.resp.bits.replacementUpdated
  io.lsq.ldin.bits.missDbUpdated := RegNext(s2_fire && s2_in.hasROBEntry && !s2_in.tlbMiss && !s2_in.missDbUpdated)

  val s3_dly_ld_err =
    if (EnableAccurateLoadError) {
      (s3_in.lateKill || io.dcache.resp.bits.error_delayed) && RegNext(io.csrCtrl.cache_error_enable)
    } else {
      WireInit(false.B)
    }
  io.s3_dly_ld_err := false.B // s3_dly_ld_err && s3_valid
  io.fast_rep_out.bits.delayedLoadError := s3_dly_ld_err
  io.lsq.ldin.bits.dcacheRequireReplay  := s3_dcache_rep

  val s3_vp_match_fail = RegNext(io.lsq.forward.matchInvalid || io.sbuffer.matchInvalid) && s3_troublem
  val s3_ldld_rep_inst =
      io.lsq.ldld_nuke_query.resp.valid &&
      io.lsq.ldld_nuke_query.resp.bits.rep_frm_fetch &&
      RegNext(io.csrCtrl.ldld_vio_check_enable)

  val s3_rep_info = WireInit(s3_in.rep_info)
  s3_rep_info.dcache_miss   := s3_in.rep_info.dcache_miss && !s3_fwd_frm_d_chan_valid && s3_troublem
  val s3_flushPipe = s3_ldld_rep_inst
  val s3_rep_frm_fetch = s3_vp_match_fail
  val s3_sel_rep_cause = PriorityEncoderOH(s3_rep_info.cause.asUInt)
  val s3_force_rep     = s3_sel_rep_cause(LoadReplayCauses.C_TM) &&
                         !s3_in.uop.cf.exceptionVec(loadAddrMisaligned) &&
                         s3_troublem

  val s3_exception = ExceptionNO.selectByFu(s3_in.uop.cf.exceptionVec, lduCfg).asUInt.orR
  when ((s3_exception || s3_dly_ld_err || s3_rep_frm_fetch) && !s3_force_rep) {
    io.lsq.ldin.bits.rep_info.cause := 0.U.asTypeOf(s3_rep_info.cause.cloneType)
  } .otherwise {
    io.lsq.ldin.bits.rep_info.cause := VecInit(s3_sel_rep_cause.asBools)
  }

  // Int load, if hit, will be writebacked at s3
  s3_out.valid                := s3_valid && !io.lsq.ldin.bits.rep_info.need_rep && !s3_in.mmio
  s3_out.bits.uop             := s3_in.uop
  s3_out.bits.uop.cf.exceptionVec(loadAccessFault) := s3_dly_ld_err  || s3_in.uop.cf.exceptionVec(loadAccessFault)
  s3_out.bits.uop.ctrl.flushPipe := s3_flushPipe
  s3_out.bits.uop.ctrl.replayInst := s3_rep_frm_fetch
  s3_out.bits.data            := s3_in.data
  s3_out.bits.redirectValid   := false.B
  s3_out.bits.redirect        := DontCare
  s3_out.bits.debug.isMMIO    := s3_in.mmio
  s3_out.bits.debug.isPerfCnt := false.B
  s3_out.bits.debug.paddr     := s3_in.paddr
  s3_out.bits.debug.vaddr     := s3_in.vaddr
  s3_out.bits.fflags          := DontCare

  when (s3_force_rep) {
    s3_out.bits.uop.cf.exceptionVec := 0.U.asTypeOf(s3_in.uop.cf.exceptionVec.cloneType)
  }

  /* <------- DANGEROUS: Don't change sequence here ! -------> */

  io.lsq.ldin.bits.uop := s3_out.bits.uop

  val s3_revoke = s3_exception || io.lsq.ldin.bits.rep_info.need_rep
  io.lsq.ldld_nuke_query.revoke := s3_revoke
  io.lsq.stld_nuke_query.revoke := s3_revoke

  // feedback slow
  s3_fast_rep := RegNext(s2_fast_rep) &&
                 !s3_in.feedbacked &&
                 !s3_in.lateKill &&
                 !s3_rep_frm_fetch &&
                 !s3_exception

  val s3_fb_no_waiting = !s3_in.isLoadReplay && !(s3_fast_rep && io.fast_rep_out.ready) && !s3_in.feedbacked

  //
  io.feedback_slow.valid                 := s3_valid && !s3_in.uop.robIdx.needFlush(io.redirect) && s3_fb_no_waiting
  io.feedback_slow.bits.hit              := !io.lsq.ldin.bits.rep_info.need_rep || io.lsq.ldin.ready
  io.feedback_slow.bits.flushState       := s3_in.ptwBack
  io.feedback_slow.bits.rsIdx            := s3_in.rsIdx
  io.feedback_slow.bits.sourceType       := RSFeedbackType.lrqFull
  io.feedback_slow.bits.dataInvalidSqIdx := DontCare

  val s3_ld_wb_meta = Mux(s3_out.valid, s3_out.bits, io.lsq.uncache.bits)

  // data from load queue refill
  val s3_ld_raw_data_frm_uncache = io.lsq.ld_raw_data
  val s3_merged_data_frm_uncache = s3_ld_raw_data_frm_uncache.mergedData()
  val s3_picked_data_frm_uncache = LookupTree(s3_ld_raw_data_frm_uncache.addrOffset, List(
    "b000".U -> s3_merged_data_frm_uncache(63,  0),
    "b001".U -> s3_merged_data_frm_uncache(63,  8),
    "b010".U -> s3_merged_data_frm_uncache(63, 16),
    "b011".U -> s3_merged_data_frm_uncache(63, 24),
    "b100".U -> s3_merged_data_frm_uncache(63, 32),
    "b101".U -> s3_merged_data_frm_uncache(63, 40),
    "b110".U -> s3_merged_data_frm_uncache(63, 48),
    "b111".U -> s3_merged_data_frm_uncache(63, 56)
  ))
  val s3_ld_data_frm_uncache = rdataHelper(s3_ld_raw_data_frm_uncache.uop, s3_picked_data_frm_uncache)

  // data from dcache hit
  val s3_ld_raw_data_frm_cache = Wire(new LoadDataFromDcacheBundle)
  s3_ld_raw_data_frm_cache.respDcacheData       := io.dcache.resp.bits.data_delayed
  s3_ld_raw_data_frm_cache.forwardMask          := RegEnable(s2_fwd_mask, s2_valid)
  s3_ld_raw_data_frm_cache.forwardData          := RegEnable(s2_fwd_data, s2_valid)
  s3_ld_raw_data_frm_cache.uop                  := RegEnable(s2_out.uop, s2_valid)
  s3_ld_raw_data_frm_cache.addrOffset           := RegEnable(s2_out.paddr(3, 0), s2_valid)
  s3_ld_raw_data_frm_cache.forward_D            := RegEnable(s2_fwd_frm_d_chan, false.B, s2_valid) || s3_fwd_frm_d_chan_valid
  s3_ld_raw_data_frm_cache.forwardData_D        := Mux(s3_fwd_frm_d_chan_valid, s3_fwd_data_frm_d_chan, RegEnable(s2_fwd_data_frm_d_chan, s2_valid))
  s3_ld_raw_data_frm_cache.forward_mshr         := RegEnable(s2_fwd_frm_mshr, false.B, s2_valid)
  s3_ld_raw_data_frm_cache.forwardData_mshr     := RegEnable(s2_fwd_data_frm_mshr, s2_valid)
  s3_ld_raw_data_frm_cache.forward_result_valid := RegEnable(s2_fwd_data_valid, false.B, s2_valid)

  val s3_merged_data_frm_cache = s3_ld_raw_data_frm_cache.mergedData()
  val s3_picked_data_frm_cache = LookupTree(s3_ld_raw_data_frm_cache.addrOffset, List(
    "b0000".U -> s3_merged_data_frm_cache(63,    0),
    "b0001".U -> s3_merged_data_frm_cache(63,    8),
    "b0010".U -> s3_merged_data_frm_cache(63,   16),
    "b0011".U -> s3_merged_data_frm_cache(63,   24),
    "b0100".U -> s3_merged_data_frm_cache(63,   32),
    "b0101".U -> s3_merged_data_frm_cache(63,   40),
    "b0110".U -> s3_merged_data_frm_cache(63,   48),
    "b0111".U -> s3_merged_data_frm_cache(63,   56),
    "b1000".U -> s3_merged_data_frm_cache(127,  64),
    "b1001".U -> s3_merged_data_frm_cache(127,  72),
    "b1010".U -> s3_merged_data_frm_cache(127,  80),
    "b1011".U -> s3_merged_data_frm_cache(127,  88),
    "b1100".U -> s3_merged_data_frm_cache(127,  96),
    "b1101".U -> s3_merged_data_frm_cache(127, 104),
    "b1110".U -> s3_merged_data_frm_cache(127, 112),
    "b1111".U -> s3_merged_data_frm_cache(127, 120)
  ))
  val s3_ld_data_frm_cache = rdataHelper(s3_ld_raw_data_frm_cache.uop, s3_picked_data_frm_cache)

  // FIXME: add 1 cycle delay ?
  io.lsq.uncache.ready := !s3_out.valid
  io.ldout.bits        := s3_ld_wb_meta
  io.ldout.bits.data   := Mux(s3_out.valid, s3_ld_data_frm_cache, s3_ld_data_frm_uncache)
  io.ldout.valid       := s3_out.valid && !s3_out.bits.uop.robIdx.needFlush(io.redirect) ||
                         io.lsq.uncache.valid && !io.lsq.uncache.bits.uop.robIdx.needFlush(io.redirect) && !s3_out.valid


  // fast load to load forward
  io.l2l_fwd_out.valid      := s3_out.valid && !s3_in.lateKill
  io.l2l_fwd_out.data       := s3_ld_data_frm_cache
  io.l2l_fwd_out.dly_ld_err := s3_dly_ld_err // ecc delayed error

   // trigger
  val last_valid_data = RegNext(RegEnable(io.ldout.bits.data, io.ldout.fire))
  val hit_ld_addr_trig_hit_vec = Wire(Vec(3, Bool()))
  val lq_ld_addr_trig_hit_vec = io.lsq.trigger.lqLoadAddrTriggerHitVec
  (0 until 3).map{i => {
    val tdata2    = RegNext(io.trigger(i).tdata2)
    val matchType = RegNext(io.trigger(i).matchType)
    val tEnable   = RegNext(io.trigger(i).tEnable)

    hit_ld_addr_trig_hit_vec(i) := TriggerCmp(RegNext(s2_out.vaddr), tdata2, matchType, tEnable)
    io.trigger(i).addrHit       := Mux(s3_out.valid, hit_ld_addr_trig_hit_vec(i), lq_ld_addr_trig_hit_vec(i))
    io.trigger(i).lastDataHit   := TriggerCmp(last_valid_data, tdata2, matchType, tEnable)
  }}
  io.lsq.trigger.hitLoadAddrTriggerHitVec := hit_ld_addr_trig_hit_vec

  // FIXME: please move this part to LoadQueueReplay
  io.debug_ls := DontCare

  // Topdown
  io.lsTopdownInfo.s1.robIdx          := s1_in.uop.robIdx.value
  io.lsTopdownInfo.s1.vaddr_valid     := s1_valid && s1_in.hasROBEntry
  io.lsTopdownInfo.s1.vaddr_bits      := s1_vaddr
  io.lsTopdownInfo.s2.robIdx          := s2_in.uop.robIdx.value
  io.lsTopdownInfo.s2.paddr_valid     := s2_fire && s2_in.hasROBEntry && !s2_in.tlbMiss
  io.lsTopdownInfo.s2.paddr_bits      := s2_in.paddr
  io.lsTopdownInfo.s2.first_real_miss := io.dcache.resp.bits.real_miss
  io.lsTopdownInfo.s2.cache_miss_en   := s2_fire && s2_in.hasROBEntry && !s2_in.tlbMiss && !s2_in.missDbUpdated

  // perf cnt
  XSPerfAccumulate("s0_in_valid",                  io.ldin.valid)
  XSPerfAccumulate("s0_in_block",                  io.ldin.valid && !io.ldin.fire)
  XSPerfAccumulate("s0_in_fire_first_issue",       s0_valid && s0_isFirstIssue)
  XSPerfAccumulate("s0_lsq_fire_first_issue",      io.replay.fire)
  XSPerfAccumulate("s0_ldu_fire_first_issue",      io.ldin.fire && s0_isFirstIssue)
  XSPerfAccumulate("s0_fast_replay_issue",         io.fast_rep_in.fire)
  XSPerfAccumulate("s0_stall_out",                 s0_valid && !s0_can_go)
  XSPerfAccumulate("s0_stall_dcache",              s0_valid && !io.dcache.req.ready)
  XSPerfAccumulate("s0_addr_spec_success",         s0_fire && s0_vaddr(VAddrBits-1, 12) === io.ldin.bits.src(0)(VAddrBits-1, 12))
  XSPerfAccumulate("s0_addr_spec_failed",          s0_fire && s0_vaddr(VAddrBits-1, 12) =/= io.ldin.bits.src(0)(VAddrBits-1, 12))
  XSPerfAccumulate("s0_addr_spec_success_once",    s0_fire && s0_vaddr(VAddrBits-1, 12) === io.ldin.bits.src(0)(VAddrBits-1, 12) && s0_isFirstIssue)
  XSPerfAccumulate("s0_addr_spec_failed_once",     s0_fire && s0_vaddr(VAddrBits-1, 12) =/= io.ldin.bits.src(0)(VAddrBits-1, 12) && s0_isFirstIssue)
  XSPerfAccumulate("s0_forward_tl_d_channel",      s0_out.forward_tlDchannel)
  XSPerfAccumulate("s0_hardware_prefetch_fire",    s0_fire && s0_hw_prf_select)
  XSPerfAccumulate("s0_software_prefetch_fire",    s0_fire && s0_prf && s0_int_iss_select)
  XSPerfAccumulate("s0_hardware_prefetch_blocked", io.prefetch_req.valid && !s0_hw_prf_select)
  XSPerfAccumulate("s0_hardware_prefetch_total",   io.prefetch_req.valid)

  XSPerfAccumulate("s1_in_valid",                  s1_valid)
  XSPerfAccumulate("s1_in_fire",                   s1_fire)
  XSPerfAccumulate("s1_in_fire_first_issue",       s1_fire && s1_in.isFirstIssue)
  XSPerfAccumulate("s1_tlb_miss",                  s1_fire && s1_tlb_miss)
  XSPerfAccumulate("s1_tlb_miss_first_issue",      s1_fire && s1_tlb_miss && s1_in.isFirstIssue)
  XSPerfAccumulate("s1_stall_out",                 s1_valid && !s1_can_go)
  XSPerfAccumulate("s1_late_kill",                 s1_valid && s1_fast_rep_kill)

  XSPerfAccumulate("s2_in_valid",                  s2_valid)
  XSPerfAccumulate("s2_in_fire",                   s2_fire)
  XSPerfAccumulate("s2_in_fire_first_issue",       s2_fire && s2_in.isFirstIssue)
  XSPerfAccumulate("s2_dcache_miss",               s2_fire && io.dcache.resp.bits.miss)
  XSPerfAccumulate("s2_dcache_miss_first_issue",   s2_fire && io.dcache.resp.bits.miss && s2_in.isFirstIssue)
  XSPerfAccumulate("s2_dcache_real_miss_first_issue",   s2_fire && io.dcache.resp.bits.miss && s2_in.isFirstIssue)
  XSPerfAccumulate("s2_full_forward",              s2_fire && s2_full_fwd)
  XSPerfAccumulate("s2_dcache_miss_full_forward",  s2_fire && s2_dcache_miss)
  XSPerfAccumulate("s2_fwd_frm_d_can",             s2_valid && s2_fwd_frm_d_chan)
  XSPerfAccumulate("s2_fwd_frm_d_chan_or_mshr",    s2_valid && s2_fwd_frm_d_chan_or_mshr)
  XSPerfAccumulate("s2_stall_out",                 s2_fire && !s2_can_go)
  XSPerfAccumulate("s2_prefetch",                  s2_fire && s2_prf)
  XSPerfAccumulate("s2_prefetch_ignored",          s2_fire && s2_prf && s2_mq_nack) // ignore prefetch for mshr full / miss req port conflict
  XSPerfAccumulate("s2_prefetch_miss",             s2_fire && s2_prf && io.dcache.resp.bits.miss) // prefetch req miss in l1
  XSPerfAccumulate("s2_prefetch_hit",              s2_fire && s2_prf && !io.dcache.resp.bits.miss) // prefetch req hit in l1
  XSPerfAccumulate("s2_prefetch_accept",           s2_fire && s2_prf && io.dcache.resp.bits.miss && !s2_mq_nack) // prefetch a missed line in l1, and l1 accepted it
  XSPerfAccumulate("s2_forward_req",               s2_fire && s2_in.forward_tlDchannel)
  XSPerfAccumulate("s2_successfully_forward_channel_D", s2_fire && s2_fwd_frm_d_chan && s2_fwd_data_valid)
  XSPerfAccumulate("s2_successfully_forward_mshr",      s2_fire && s2_fwd_frm_mshr && s2_fwd_data_valid)

  XSPerfAccumulate("s3_fwd_frm_d_chan",            s3_valid && s3_fwd_frm_d_chan_valid)

  XSPerfAccumulate("load_to_load_forward",                      s1_try_ptr_chasing && !s1_ptr_chasing_canceled)
  XSPerfAccumulate("load_to_load_forward_try",                  s1_try_ptr_chasing)
  XSPerfAccumulate("load_to_load_forward_fail",                 s1_cancel_ptr_chasing)
  XSPerfAccumulate("load_to_load_forward_fail_cancelled",       s1_cancel_ptr_chasing && s1_ptr_chasing_canceled)
  XSPerfAccumulate("load_to_load_forward_fail_wakeup_mismatch", s1_cancel_ptr_chasing && !s1_ptr_chasing_canceled && s1_not_fast_match)
  XSPerfAccumulate("load_to_load_forward_fail_op_not_ld",       s1_cancel_ptr_chasing && !s1_ptr_chasing_canceled && !s1_not_fast_match && s1_fu_op_type_not_ld)
  XSPerfAccumulate("load_to_load_forward_fail_addr_align",      s1_cancel_ptr_chasing && !s1_ptr_chasing_canceled && !s1_not_fast_match && !s1_fu_op_type_not_ld && s1_addr_misaligned)
  XSPerfAccumulate("load_to_load_forward_fail_set_mismatch",    s1_cancel_ptr_chasing && !s1_ptr_chasing_canceled && !s1_not_fast_match && !s1_fu_op_type_not_ld && !s1_addr_misaligned && s1_addr_mismatch)

  // bug lyq: some signals in perfEvents are no longer suitable for the current MemBlock design
  // hardware performance counter
  val perfEvents = Seq(
    ("load_s0_in_fire         ", s0_fire                                                        ),
    ("load_to_load_forward    ", s1_fire && s1_try_ptr_chasing && !s1_ptr_chasing_canceled      ),
    ("stall_dcache            ", s0_valid && s0_can_go && !io.dcache.req.ready                  ),
    ("load_s1_in_fire         ", s0_fire                                                        ),
    ("load_s1_tlb_miss        ", s1_fire && io.tlb.resp.bits.miss                               ),
    ("load_s2_in_fire         ", s1_fire                                                        ),
    ("load_s2_dcache_miss     ", s2_fire && io.dcache.resp.bits.miss                            ),
  )
  generatePerfEvent()

  when(io.ldout.fire){
    XSDebug("ldout %x\n", io.ldout.bits.uop.cf.pc)
  }
  // end
}