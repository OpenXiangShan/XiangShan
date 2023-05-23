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
import xiangshan.backend.rob.{DebugLsInfoBundle, LsTopdownInfo, RobPtr}
import xiangshan.cache._
import xiangshan.cache.dcache.ReplayCarry
import xiangshan.cache.mmu.{TlbCmd, TlbReq, TlbRequestIO, TlbResp}
import xiangshan.mem.mdp._

class LoadToLsqReplayIO(implicit p: Parameters) extends XSBundle with HasDCacheParameters {
  // mshr refill index
  val missMSHRId = UInt(log2Up(cfg.nMissEntries).W)
  // get full data from store queue and sbuffer
  val canForwardFullData = Bool()
  // wait for data from store inst's store queue index
  val dataInvalidSqIdx = new SqPtr
  // wait for address from store queue index
  val addrInvalidSqIdx = new SqPtr
  // replay carry
  val replayCarry = new ReplayCarry
  // data in last beat
  val dataInLastBeat = Bool()
  // replay cause
  val cause = Vec(LoadReplayCauses.allCauses, Bool())
  //
  // performance debug information
  val debug = new PerfDebugInfo

  // 
  def tlbMiss       = cause(LoadReplayCauses.tlbMiss)
  def waitStore     = cause(LoadReplayCauses.waitStore)
  def schedError    = cause(LoadReplayCauses.schedError)
  def rejectEnq     = cause(LoadReplayCauses.rejectEnq)
  def dcacheMiss    = cause(LoadReplayCauses.dcacheMiss)
  def bankConflict  = cause(LoadReplayCauses.bankConflict)
  def dcacheReplay  = cause(LoadReplayCauses.dcacheReplay)
  def forwardFail   = cause(LoadReplayCauses.forwardFail)

  def forceReplay() = rejectEnq || schedError || waitStore || tlbMiss
  def needReplay()  = cause.asUInt.orR 
}

class LoadToReplayIO(implicit p: Parameters) extends XSBundle {
  val req = ValidIO(new LqWriteBundle)
  val resp = Input(UInt(log2Up(LoadQueueReplaySize).W))
}

class LoadToLsqIO(implicit p: Parameters) extends XSBundle {
  val loadIn = DecoupledIO(new LqWriteBundle)
  val loadOut = Flipped(DecoupledIO(new ExuOutput))
  val ldRawData = Input(new LoadDataFromLQBundle)
  val forward = new PipeLoadForwardQueryIO
  val storeLoadViolationQuery = new LoadViolationQueryIO
  val loadLoadViolationQuery = new LoadViolationQueryIO
  val trigger = Flipped(new LqTriggerIO)
}

class LoadToLoadIO(implicit p: Parameters) extends XSBundle {
  // load to load fast path is limited to ld (64 bit) used as vaddr src1 only
  val data = UInt(XLEN.W)
  val valid = Bool()
}

class LoadUnitTriggerIO(implicit p: Parameters) extends XSBundle {
  val tdata2 = Input(UInt(64.W))
  val matchType = Input(UInt(2.W))
  val tEnable = Input(Bool()) // timing is calculated before this
  val addrHit = Output(Bool())
  val lastDataHit = Output(Bool())
}

// Load Pipeline Stage 0
// Generate addr, use addr to query DCache and DTLB
class LoadUnit_S0(implicit p: Parameters) extends XSModule with HasDCacheParameters with HasCircularQueuePtrHelper {
  val io = IO(new Bundle() {
    val in = Flipped(Decoupled(new ExuInput))
    val out = Decoupled(new LqWriteBundle)
    val prefetch_in = Flipped(ValidIO(new L1PrefetchReq))
    val dtlbReq = DecoupledIO(new TlbReq)
    val dcacheReq = DecoupledIO(new DCacheWordReq)
    val rsIdx = Input(UInt(log2Up(IssQueSize).W))
    val isFirstIssue = Input(Bool())
    val fastpath = Input(new LoadToLoadIO)
    val s0_kill = Input(Bool())
    // wire from lq to load pipeline
    val replay = Flipped(Decoupled(new LsPipelineBundle))
    val s0_sqIdx = Output(new SqPtr)
    // l2l
    val l2lForward_select = Output(Bool())
  })
  require(LoadPipelineWidth == exuParameters.LduCnt)

  val s0_vaddr = Wire(UInt(VAddrBits.W))
  val s0_mask = Wire(UInt(8.W))
  val s0_uop = Wire(new MicroOp)
  val s0_isFirstIssue = Wire(Bool())
  val s0_hasROBEntry = WireDefault(false.B)
  val s0_rsIdx = Wire(UInt(log2Up(IssQueSize).W))
  val s0_sqIdx = Wire(new SqPtr)
  val s0_tryFastpath = WireInit(false.B)
  val s0_replayCarry = Wire(new ReplayCarry) // way info for way predict related logic

  // default value
  s0_replayCarry.valid := false.B
  s0_replayCarry.real_way_en := 0.U
  io.s0_sqIdx := s0_sqIdx

  val s0_replayShouldWait = io.in.valid && isAfter(io.replay.bits.uop.robIdx, io.in.bits.uop.robIdx)
  // load flow select/gen
  //
  // src0: load replayed by LSQ (io.replay)
  // src1: hardware prefetch from prefetchor (high confidence) (io.prefetch)
  // src2: int read / software prefetch first issue from RS (io.in)
  // src3: vec read first issue from RS (TODO)
  // src4: load try pointchaising when no issued or replayed load (io.fastpath)
  // src5: hardware prefetch from prefetchor (high confidence) (io.prefetch)

  // load flow source valid
  val lfsrc0_loadReplay_valid = io.replay.valid && !s0_replayShouldWait 
  val lfsrc1_highconfhwPrefetch_valid = io.prefetch_in.valid && io.prefetch_in.bits.confidence > 0.U
  val lfsrc2_intloadFirstIssue_valid = io.in.valid // int flow first issue or software prefetch
  val lfsrc3_vecloadFirstIssue_valid = WireInit(false.B) // TODO
  val lfsrc4_l2lForward_valid = io.fastpath.valid
  val lfsrc5_lowconfhwPrefetch_valid = io.prefetch_in.valid && io.prefetch_in.bits.confidence === 0.U
  dontTouch(lfsrc0_loadReplay_valid)
  dontTouch(lfsrc1_highconfhwPrefetch_valid)
  dontTouch(lfsrc2_intloadFirstIssue_valid)
  dontTouch(lfsrc3_vecloadFirstIssue_valid)
  dontTouch(lfsrc4_l2lForward_valid)
  dontTouch(lfsrc5_lowconfhwPrefetch_valid)
  
  // load flow source ready
  val lfsrc_loadReplay_ready = WireInit(true.B)
  val lfsrc_highconfhwPrefetch_ready = !lfsrc0_loadReplay_valid 
  val lfsrc_intloadFirstIssue_ready = !lfsrc0_loadReplay_valid &&
    !lfsrc1_highconfhwPrefetch_valid
  val lfsrc_vecloadFirstIssue_ready = !lfsrc0_loadReplay_valid &&
    !lfsrc1_highconfhwPrefetch_valid &&
    !lfsrc2_intloadFirstIssue_valid
  val lfsrc_l2lForward_ready = !lfsrc0_loadReplay_valid &&
    !lfsrc1_highconfhwPrefetch_valid &&
    !lfsrc2_intloadFirstIssue_valid &&
    !lfsrc3_vecloadFirstIssue_valid
  val lfsrc_lowconfhwPrefetch_ready = !lfsrc0_loadReplay_valid && 
    !lfsrc1_highconfhwPrefetch_valid &&
    !lfsrc2_intloadFirstIssue_valid &&
    !lfsrc3_vecloadFirstIssue_valid &&
    !lfsrc4_l2lForward_valid
  dontTouch(lfsrc_loadReplay_ready)
  dontTouch(lfsrc_highconfhwPrefetch_ready)
  dontTouch(lfsrc_intloadFirstIssue_ready)
  dontTouch(lfsrc_vecloadFirstIssue_ready)
  dontTouch(lfsrc_l2lForward_ready)
  dontTouch(lfsrc_lowconfhwPrefetch_ready)
    
  // load flow source select (OH)
  val lfsrc_loadReplay_select = lfsrc0_loadReplay_valid && lfsrc_loadReplay_ready
  val lfsrc_hwprefetch_select = lfsrc_highconfhwPrefetch_ready && lfsrc1_highconfhwPrefetch_valid || 
    lfsrc_lowconfhwPrefetch_ready && lfsrc5_lowconfhwPrefetch_valid
  val lfsrc_intloadFirstIssue_select = lfsrc_intloadFirstIssue_ready && lfsrc2_intloadFirstIssue_valid
  val lfsrc_vecloadFirstIssue_select = lfsrc_vecloadFirstIssue_ready && lfsrc3_vecloadFirstIssue_valid
  val lfsrc_l2lForward_select = lfsrc_l2lForward_ready && lfsrc4_l2lForward_valid
  assert(!lfsrc_vecloadFirstIssue_select) // to be added
  dontTouch(lfsrc_loadReplay_select)
  dontTouch(lfsrc_hwprefetch_select)
  dontTouch(lfsrc_intloadFirstIssue_select)
  dontTouch(lfsrc_vecloadFirstIssue_select)
  dontTouch(lfsrc_l2lForward_select)

  io.l2lForward_select := lfsrc_l2lForward_select

  // s0_valid == ture iff there is a valid load flow in load_s0
  val s0_valid = lfsrc0_loadReplay_valid ||
    lfsrc1_highconfhwPrefetch_valid ||
    lfsrc2_intloadFirstIssue_valid ||
    lfsrc3_vecloadFirstIssue_valid ||
    lfsrc4_l2lForward_valid ||
    lfsrc5_lowconfhwPrefetch_valid

  // prefetch related ctrl signal
  val isPrefetch = WireInit(false.B)
  val isPrefetchRead = WireInit(s0_uop.ctrl.fuOpType === LSUOpType.prefetch_r)
  val isPrefetchWrite = WireInit(s0_uop.ctrl.fuOpType === LSUOpType.prefetch_w)
  val isHWPrefetch = lfsrc_hwprefetch_select

  // query DTLB
  io.dtlbReq.valid := s0_valid
  // hw prefetch addr does not need to be translated, give tlb paddr
  io.dtlbReq.bits.vaddr := Mux(lfsrc_hwprefetch_select, io.prefetch_in.bits.paddr, s0_vaddr) 
  io.dtlbReq.bits.cmd := Mux(isPrefetch,
    Mux(isPrefetchWrite, TlbCmd.write, TlbCmd.read),
    TlbCmd.read
  )
  io.dtlbReq.bits.size := LSUOpType.size(s0_uop.ctrl.fuOpType)
  io.dtlbReq.bits.kill := DontCare
  io.dtlbReq.bits.memidx.is_ld := true.B
  io.dtlbReq.bits.memidx.is_st := false.B
  io.dtlbReq.bits.memidx.idx := s0_uop.lqIdx.value
  io.dtlbReq.bits.debug.robIdx := s0_uop.robIdx
  // hw prefetch addr does not need to be translated
  io.dtlbReq.bits.no_translate := lfsrc_hwprefetch_select
  io.dtlbReq.bits.debug.pc := s0_uop.cf.pc
  io.dtlbReq.bits.debug.isFirstIssue := s0_isFirstIssue

  // query DCache
  io.dcacheReq.valid := s0_valid
  when (isPrefetchRead) {
    io.dcacheReq.bits.cmd  := MemoryOpConstants.M_PFR
  }.elsewhen (isPrefetchWrite) {
    io.dcacheReq.bits.cmd  := MemoryOpConstants.M_PFW
  }.otherwise {
    io.dcacheReq.bits.cmd  := MemoryOpConstants.M_XRD
  }
  io.dcacheReq.bits.vaddr := s0_vaddr
  io.dcacheReq.bits.mask := s0_mask
  io.dcacheReq.bits.data := DontCare
  io.dcacheReq.bits.isFirstIssue := s0_isFirstIssue
  when(isPrefetch) {
    io.dcacheReq.bits.instrtype := DCACHE_PREFETCH_SOURCE.U
  }.otherwise {
    io.dcacheReq.bits.instrtype := LOAD_SOURCE.U
  }
  io.dcacheReq.bits.debug_robIdx := s0_uop.robIdx.value
  io.dcacheReq.bits.replayCarry := s0_replayCarry

  // TODO: update cache meta
  io.dcacheReq.bits.id := DontCare

  // assign default value
  s0_uop := DontCare
  // load flow priority mux
  when(lfsrc_loadReplay_select) {
    s0_vaddr := io.replay.bits.vaddr
    s0_mask := genWmask(io.replay.bits.vaddr, io.replay.bits.uop.ctrl.fuOpType(1, 0))
    s0_uop := io.replay.bits.uop
    s0_isFirstIssue := io.replay.bits.isFirstIssue
    s0_sqIdx := io.replay.bits.uop.sqIdx
    s0_rsIdx := io.replay.bits.rsIdx
    s0_hasROBEntry := true.B
    s0_replayCarry := io.replay.bits.replayCarry
    val replayUopIsPrefetch = WireInit(LSUOpType.isPrefetch(io.replay.bits.uop.ctrl.fuOpType))
    when (replayUopIsPrefetch) {
      isPrefetch := true.B
    }
  }.elsewhen(lfsrc_hwprefetch_select) {
    // vaddr based index for dcache
    s0_vaddr := io.prefetch_in.bits.getVaddr()
    s0_mask := 0.U
    s0_uop := DontCare
    s0_isFirstIssue := false.B
    s0_sqIdx := DontCare
    s0_replayCarry := DontCare
    // ctrl signal
    isPrefetch := true.B
    isPrefetchRead := !io.prefetch_in.bits.is_store
    isPrefetchWrite := io.prefetch_in.bits.is_store
  }.elsewhen(lfsrc_intloadFirstIssue_select) {
    val imm12 = io.in.bits.uop.ctrl.imm(11, 0)
    s0_vaddr := io.in.bits.src(0) + SignExt(imm12, VAddrBits)
    s0_mask := genWmask(s0_vaddr, io.in.bits.uop.ctrl.fuOpType(1,0))
    s0_uop := io.in.bits.uop
    s0_isFirstIssue := true.B
    s0_hasROBEntry := true.B
    s0_rsIdx := io.rsIdx
    s0_sqIdx := io.in.bits.uop.sqIdx
    val issueUopIsPrefetch = WireInit(LSUOpType.isPrefetch(io.in.bits.uop.ctrl.fuOpType))
    when (issueUopIsPrefetch) {
      isPrefetch := true.B
    }
  }.otherwise {
    if (EnableLoadToLoadForward) {
      s0_tryFastpath := lfsrc_l2lForward_select
      // When there's no valid instruction from RS and LSQ, we try the load-to-load forwarding.
      s0_vaddr := io.fastpath.data
      // Assume the pointer chasing is always ld.
      s0_uop.ctrl.fuOpType := LSUOpType.ld
      s0_mask := genWmask(0.U, LSUOpType.ld)
      // we dont care s0_isFirstIssue and s0_rsIdx and s0_sqIdx in S0 when trying pointchasing
      // because these signals will be updated in S1
      s0_isFirstIssue := true.B
      s0_sqIdx := DontCare
    }
  }

  // address align check
  val addrAligned = LookupTree(s0_uop.ctrl.fuOpType(1, 0), List(
    "b00".U   -> true.B,                   //b
    "b01".U   -> (s0_vaddr(0)    === 0.U), //h
    "b10".U   -> (s0_vaddr(1, 0) === 0.U), //w
    "b11".U   -> (s0_vaddr(2, 0) === 0.U)  //d
  ))


  // accept load flow if dcache ready (dtlb is always ready)
  // TODO: prefetch need writeback to loadQueueFlag
  io.out.valid := s0_valid && io.dcacheReq.ready && !io.s0_kill
  io.out.bits := DontCare
  io.out.bits.rsIdx := io.rsIdx
  io.out.bits.vaddr := s0_vaddr
  io.out.bits.mask := s0_mask
  io.out.bits.uop := s0_uop
  io.out.bits.uop.cf.exceptionVec(loadAddrMisaligned) := !addrAligned
  io.out.bits.isFirstIssue := s0_isFirstIssue
  io.out.bits.hasROBEntry := s0_hasROBEntry
  io.out.bits.isPrefetch := isPrefetch
  io.out.bits.isHWPrefetch := isHWPrefetch
  io.out.bits.isLoadReplay := lfsrc_loadReplay_select
  io.out.bits.mshrid := io.replay.bits.mshrid
  io.out.bits.forward_tlDchannel := io.replay.valid && io.replay.bits.forward_tlDchannel
  when(io.dtlbReq.valid && s0_isFirstIssue) {
    io.out.bits.uop.debugInfo.tlbFirstReqTime := GTimer()
  }.otherwise{
    io.out.bits.uop.debugInfo.tlbFirstReqTime := s0_uop.debugInfo.tlbFirstReqTime
  }
  io.out.bits.sleepIndex := io.replay.bits.sleepIndex

  // load flow source ready
  // always accept load flow from load replay queue
  // io.replay has highest priority
  io.replay.ready := (io.out.ready && io.dcacheReq.ready && lfsrc_loadReplay_select && !s0_replayShouldWait)

  // accept load flow from rs when:
  // 1) there is no lsq-replayed load
  // 2) there is no high confidence prefetch request
  io.in.ready := (io.out.ready && io.dcacheReq.ready && lfsrc_intloadFirstIssue_select)

  // for hw prefetch load flow feedback, to be added later
  // io.prefetch_in.ready := lfsrc_hwprefetch_select

  XSDebug(io.dcacheReq.fire,
    p"[DCACHE LOAD REQ] pc ${Hexadecimal(s0_uop.cf.pc)}, vaddr ${Hexadecimal(s0_vaddr)}\n"
  )
  XSPerfAccumulate("in_valid", io.in.valid)
  XSPerfAccumulate("in_fire", io.in.fire)
  XSPerfAccumulate("in_fire_first_issue", s0_valid && s0_isFirstIssue)
  XSPerfAccumulate("lsq_fire_first_issue", io.replay.fire)
  XSPerfAccumulate("ldu_fire_first_issue", io.in.fire && io.isFirstIssue)
  XSPerfAccumulate("stall_out", io.out.valid && !io.out.ready && io.dcacheReq.ready)
  XSPerfAccumulate("stall_dcache", io.out.valid && io.out.ready && !io.dcacheReq.ready)
  XSPerfAccumulate("addr_spec_success", io.out.fire && s0_vaddr(VAddrBits-1, 12) === io.in.bits.src(0)(VAddrBits-1, 12))
  XSPerfAccumulate("addr_spec_failed", io.out.fire && s0_vaddr(VAddrBits-1, 12) =/= io.in.bits.src(0)(VAddrBits-1, 12))
  XSPerfAccumulate("addr_spec_success_once", io.out.fire && s0_vaddr(VAddrBits-1, 12) === io.in.bits.src(0)(VAddrBits-1, 12) && io.isFirstIssue)
  XSPerfAccumulate("addr_spec_failed_once", io.out.fire && s0_vaddr(VAddrBits-1, 12) =/= io.in.bits.src(0)(VAddrBits-1, 12) && io.isFirstIssue)
  XSPerfAccumulate("forward_tlDchannel", io.out.bits.forward_tlDchannel)
  XSPerfAccumulate("hardware_prefetch_fire", io.out.fire && lfsrc_hwprefetch_select)
  XSPerfAccumulate("software_prefetch_fire", io.out.fire && isPrefetch && lfsrc_intloadFirstIssue_select)
  XSPerfAccumulate("hardware_prefetch_blocked", io.prefetch_in.valid && !lfsrc_hwprefetch_select)
  XSPerfAccumulate("hardware_prefetch_total", io.prefetch_in.valid)
}

// Load Pipeline Stage 1
// TLB resp (send paddr to dcache)
class LoadUnit_S1(implicit p: Parameters) extends XSModule with HasCircularQueuePtrHelper {
  val io = IO(new Bundle() {
    val in = Flipped(Decoupled(new LqWriteBundle))
    val s1_kill = Input(Bool())
    val out = Decoupled(new LqWriteBundle)
    val dtlbResp = Flipped(DecoupledIO(new TlbResp(2)))
    val lsuPAddr = Output(UInt(PAddrBits.W))
    val dcachePAddr = Output(UInt(PAddrBits.W))
    val dcacheKill = Output(Bool())
    val dcacheBankConflict = Input(Bool())
    val fullForwardFast = Output(Bool())
    val sbuffer = new LoadForwardQueryIO
    val lsq = new PipeLoadForwardQueryIO
    val reExecuteQuery = Flipped(Vec(StorePipelineWidth, Valid(new LoadReExecuteQueryIO)))
    val csrCtrl = Flipped(new CustomCSRCtrlIO)
  })

  val s1_uop = io.in.bits.uop
  val s1_paddr_dup_lsu = io.dtlbResp.bits.paddr(0)
  val s1_paddr_dup_dcache = io.dtlbResp.bits.paddr(1)
  // af & pf exception were modified below.
  val s1_exception = ExceptionNO.selectByFu(io.out.bits.uop.cf.exceptionVec, lduCfg).asUInt.orR
  val s1_tlb_miss = io.dtlbResp.bits.miss
  val s1_mask = io.in.bits.mask
  val s1_is_prefetch = io.in.bits.isPrefetch
  val s1_is_hw_prefetch = io.in.bits.isHWPrefetch
  val s1_is_sw_prefetch = s1_is_prefetch && !s1_is_hw_prefetch
  val s1_bank_conflict = io.dcacheBankConflict

  io.out.bits := io.in.bits // forwardXX field will be updated in s1

  val s1_tlb_memidx = io.dtlbResp.bits.memidx
  when(s1_tlb_memidx.is_ld && io.dtlbResp.valid && !s1_tlb_miss && s1_tlb_memidx.idx === io.out.bits.uop.lqIdx.value) {
    // printf("load idx = %d\n", s1_tlb_memidx.idx)
    io.out.bits.uop.debugInfo.tlbRespTime := GTimer()
  }

  io.dtlbResp.ready := true.B

  io.lsuPAddr := s1_paddr_dup_lsu
  io.dcachePAddr := s1_paddr_dup_dcache
  //io.dcacheKill := s1_tlb_miss || s1_exception || s1_mmio
  io.dcacheKill := s1_tlb_miss || s1_exception || io.s1_kill
  // load forward query datapath
  io.sbuffer.valid := io.in.valid && !(s1_exception || s1_tlb_miss || io.s1_kill || s1_is_prefetch)
  io.sbuffer.vaddr := io.in.bits.vaddr
  io.sbuffer.paddr := s1_paddr_dup_lsu
  io.sbuffer.uop := s1_uop
  io.sbuffer.sqIdx := s1_uop.sqIdx
  io.sbuffer.mask := s1_mask
  io.sbuffer.pc := s1_uop.cf.pc // FIXME: remove it

  io.lsq.valid := io.in.valid && !(s1_exception || s1_tlb_miss || io.s1_kill || s1_is_prefetch)
  io.lsq.vaddr := io.in.bits.vaddr
  io.lsq.paddr := s1_paddr_dup_lsu
  io.lsq.uop := s1_uop
  io.lsq.sqIdx := s1_uop.sqIdx
  io.lsq.sqIdxMask := DontCare // will be overwritten by sqIdxMask pre-generated in s0
  io.lsq.mask := s1_mask
  io.lsq.pc := s1_uop.cf.pc // FIXME: remove it

  // st-ld violation query
  val s1_schedError =  VecInit((0 until StorePipelineWidth).map(w => io.reExecuteQuery(w).valid &&
                          isAfter(io.in.bits.uop.robIdx, io.reExecuteQuery(w).bits.robIdx) && 
                          (s1_paddr_dup_lsu(PAddrBits-1, 3) === io.reExecuteQuery(w).bits.paddr(PAddrBits-1, 3)) &&
                          (s1_mask & io.reExecuteQuery(w).bits.mask).orR)).asUInt.orR && !s1_tlb_miss

  // Generate forwardMaskFast to wake up insts earlier
  val forwardMaskFast = io.lsq.forwardMaskFast.asUInt | io.sbuffer.forwardMaskFast.asUInt
  io.fullForwardFast := ((~forwardMaskFast).asUInt & s1_mask) === 0.U

  io.out.valid := io.in.valid && !io.s1_kill
  io.out.bits.paddr := s1_paddr_dup_lsu
  io.out.bits.tlbMiss := s1_tlb_miss

  // Generate replay signal caused by:
  // * st-ld violation check
  // * dcache bank conflict
  io.out.bits.replayInfo.cause(LoadReplayCauses.schedError) := s1_schedError && !s1_is_sw_prefetch
  io.out.bits.replayInfo.cause(LoadReplayCauses.bankConflict) := s1_bank_conflict && !s1_is_sw_prefetch 
  io.out.bits.replayInfo.debug := io.in.bits.uop.debugInfo

  // current ori test will cause the case of ldest == 0, below will be modifeid in the future.
  // af & pf exception were modified
  io.out.bits.uop.cf.exceptionVec(loadPageFault) := io.dtlbResp.bits.excp(0).pf.ld
  io.out.bits.uop.cf.exceptionVec(loadAccessFault) := io.dtlbResp.bits.excp(0).af.ld
  io.out.bits.ptwBack := io.dtlbResp.bits.ptwBack
  io.out.bits.rsIdx := io.in.bits.rsIdx

  io.in.ready := !io.in.valid || io.out.ready

  XSPerfAccumulate("in_valid", io.in.valid)
  XSPerfAccumulate("in_fire", io.in.fire)
  XSPerfAccumulate("in_fire_first_issue", io.in.fire && io.in.bits.isFirstIssue)
  XSPerfAccumulate("tlb_miss", io.in.fire && s1_tlb_miss)
  XSPerfAccumulate("tlb_miss_first_issue", io.in.fire && s1_tlb_miss && io.in.bits.isFirstIssue)
  XSPerfAccumulate("stall_out", io.out.valid && !io.out.ready)
}

// Load Pipeline Stage 2
// DCache resp
class LoadUnit_S2(implicit p: Parameters) extends XSModule  
  with HasLoadHelper 
  with HasCircularQueuePtrHelper 
  with HasDCacheParameters 
{
  val io = IO(new Bundle() {
    val redirect = Flipped(Valid(new Redirect))
    val in = Flipped(Decoupled(new LqWriteBundle))
    val out = Decoupled(new LqWriteBundle)
    val dcacheResp = Flipped(DecoupledIO(new DCacheWordResp))
    val pmpResp = Flipped(new PMPRespBundle())
    val lsq = new LoadForwardQueryIO
    val dataInvalidSqIdx = Input(new SqPtr)
    val addrInvalidSqIdx = Input(new SqPtr)
    val sbuffer = new LoadForwardQueryIO
    val dataForwarded = Output(Bool())
    val fullForward = Output(Bool())
    val dcache_kill = Output(Bool())
    val loadLoadViolationQueryReq = DecoupledIO(new LoadViolationQueryReq)
    val storeLoadViolationQueryReq = DecoupledIO(new LoadViolationQueryReq)
    val csrCtrl = Flipped(new CustomCSRCtrlIO)
    val static_pm = Input(Valid(Bool())) // valid for static, bits for mmio
    val loadDataFromDcache = Output(new LoadDataFromDcacheBundle)
    val reExecuteQuery = Flipped(Vec(StorePipelineWidth, Valid(new LoadReExecuteQueryIO)))
    // forward tilelink D channel
    val forward_D = Input(Bool())
    val forwardData_D = Input(Vec(8, UInt(8.W)))
    val sentFastUop = Input(Bool())
    // forward mshr data
    val forward_mshr = Input(Bool())
    val forwardData_mshr = Input(Vec(8, UInt(8.W)))

    // indicate whether forward tilelink D channel or mshr data is valid
    val forward_result_valid = Input(Bool())

    val feedbackFast = ValidIO(new RSFeedback) 
    val lqReplayFull = Input(Bool())

    val s2_forward_fail = Output(Bool())
    val s2_can_replay_from_fetch = Output(Bool()) // dirty code
    val s2_dcache_require_replay = Output(Bool()) // dirty code
  })

  val pmp = WireInit(io.pmpResp)
  when (io.static_pm.valid) {
    pmp.ld := false.B
    pmp.st := false.B
    pmp.instr := false.B
    pmp.mmio := io.static_pm.bits
  }

  val s2_is_prefetch = io.in.bits.isPrefetch
  val s2_is_hw_prefetch = io.in.bits.isHWPrefetch

  val forward_D_or_mshr_valid = io.forward_result_valid && (io.forward_D || io.forward_mshr)

  // assert(!reset && io.forward_D && io.forward_mshr && io.in.valid && io.in.bits.forward_tlDchannel, "forward D and mshr at the same time")

  // exception that may cause load addr to be invalid / illegal
  //
  // if such exception happen, that inst and its exception info
  // will be force writebacked to rob
  val s2_exception_vec = WireInit(io.in.bits.uop.cf.exceptionVec)
  s2_exception_vec(loadAccessFault) := io.in.bits.uop.cf.exceptionVec(loadAccessFault) || pmp.ld
  // soft prefetch will not trigger any exception (but ecc error interrupt may be triggered)
  when (s2_is_prefetch || io.in.bits.tlbMiss) {
    s2_exception_vec := 0.U.asTypeOf(s2_exception_vec.cloneType)
  }
  val s2_exception = ExceptionNO.selectByFu(s2_exception_vec, lduCfg).asUInt.orR 

  // writeback access fault caused by ecc error / bus error
  //
  // * ecc data error is slow to generate, so we will not use it until load stage 3
  // * in load stage 3, an extra signal io.load_error will be used to

  // now cache ecc error will raise an access fault
  // at the same time, error info (including error paddr) will be write to
  // an customized CSR "CACHE_ERROR"
  // if (EnableAccurateLoadError) {
  //   io.s3_delayed_load_error := io.dcacheResp.bits.error_delayed &&
  //     io.csrCtrl.cache_error_enable &&
  //     RegNext(io.out.valid)
  // } else {
  //   io.s3_delayed_load_error := false.B
  // }

  val actually_mmio = pmp.mmio
  val s2_uop = io.in.bits.uop
  val s2_mask = io.in.bits.mask
  val s2_paddr = io.in.bits.paddr
  val s2_tlb_miss = io.in.bits.tlbMiss
  val s2_mmio = !s2_is_prefetch && actually_mmio && !s2_exception && !s2_tlb_miss
  val s2_cache_miss = io.dcacheResp.bits.miss && !forward_D_or_mshr_valid
  val s2_cache_replay = io.dcacheResp.bits.replay && !forward_D_or_mshr_valid
  val s2_cache_tag_error = RegNext(io.csrCtrl.cache_error_enable) && io.dcacheResp.bits.tag_error
  val s2_forward_fail = io.lsq.matchInvalid || io.sbuffer.matchInvalid
  val s2_wait_store = WireInit(false.B)
  val s2_data_invalid = io.lsq.dataInvalid && !s2_exception
  val s2_fullForward = WireInit(false.B)


  io.s2_forward_fail := s2_forward_fail
  io.dcache_kill := pmp.ld || pmp.mmio // move pmp resp kill to outside
  io.dcacheResp.ready := true.B
  val dcacheShouldResp = !(s2_tlb_miss || s2_exception || s2_mmio || s2_is_prefetch)
  assert(!(io.in.valid && (dcacheShouldResp && !io.dcacheResp.valid)), "DCache response got lost")

  // st-ld violation query
  //  NeedFastRecovery Valid when
  //  1. Fast recovery query request Valid.
  //  2. Load instruction is younger than requestors(store instructions).
  //  3. Physical address match.
  //  4. Data contains.
  val s2_schedError = VecInit((0 until StorePipelineWidth).map(w => io.reExecuteQuery(w).valid &&
                              isAfter(io.in.bits.uop.robIdx, io.reExecuteQuery(w).bits.robIdx) &&
                              (s2_paddr(PAddrBits-1,3) === io.reExecuteQuery(w).bits.paddr(PAddrBits-1, 3)) &&
                              (s2_mask & io.reExecuteQuery(w).bits.mask).orR)).asUInt.orR &&
                              !s2_tlb_miss 

  // need allocate new entry
  val s2_allocValid = !s2_tlb_miss &&
                      !s2_is_prefetch && 
                      !s2_exception && 
                      !s2_mmio  && 
                      !s2_wait_store && 
                      !io.in.bits.replayInfo.cause(LoadReplayCauses.schedError) 

  // ld-ld violation require
  io.loadLoadViolationQueryReq.valid := io.in.valid && s2_allocValid
  io.loadLoadViolationQueryReq.bits.uop := io.in.bits.uop
  io.loadLoadViolationQueryReq.bits.mask := s2_mask
  io.loadLoadViolationQueryReq.bits.paddr := s2_paddr
  if (EnableFastForward) {
    io.loadLoadViolationQueryReq.bits.datavalid := Mux(s2_fullForward, true.B, !s2_cache_miss) && !io.s2_dcache_require_replay
  } else {
    io.loadLoadViolationQueryReq.bits.datavalid := Mux(s2_fullForward, true.B, !s2_cache_miss)
  }

  // st-ld violation require
  io.storeLoadViolationQueryReq.valid := io.in.valid && s2_allocValid
  io.storeLoadViolationQueryReq.bits.uop := io.in.bits.uop
  io.storeLoadViolationQueryReq.bits.mask := s2_mask
  io.storeLoadViolationQueryReq.bits.paddr := s2_paddr
  io.storeLoadViolationQueryReq.bits.datavalid := io.loadLoadViolationQueryReq.bits.datavalid

  val s2_rarCanAccept = !io.loadLoadViolationQueryReq.valid || io.loadLoadViolationQueryReq.ready
  val s2_rawCanAccept = !io.storeLoadViolationQueryReq.valid || io.storeLoadViolationQueryReq.ready
  val s2_rejectEnq = !s2_rarCanAccept || !s2_rawCanAccept

  // merge forward result
  // lsq has higher priority than sbuffer
  val forwardMask = Wire(Vec(8, Bool()))
  val forwardData = Wire(Vec(8, UInt(8.W)))

  val fullForward = ((~forwardMask.asUInt).asUInt & s2_mask) === 0.U && !io.lsq.dataInvalid
  io.lsq := DontCare
  io.sbuffer := DontCare
  io.fullForward := fullForward
  s2_fullForward := fullForward

  // generate XLEN/8 Muxs
  for (i <- 0 until XLEN / 8) {
    forwardMask(i) := io.lsq.forwardMask(i) || io.sbuffer.forwardMask(i)
    forwardData(i) := Mux(io.lsq.forwardMask(i), io.lsq.forwardData(i), io.sbuffer.forwardData(i))
  }

  XSDebug(io.out.fire, "[FWD LOAD RESP] pc %x fwd %x(%b) + %x(%b)\n",
    s2_uop.cf.pc,
    io.lsq.forwardData.asUInt, io.lsq.forwardMask.asUInt,
    io.in.bits.forwardData.asUInt, io.in.bits.forwardMask.asUInt
  )

  // data merge
  // val rdataVec = VecInit((0 until XLEN / 8).map(j =>
  //   Mux(forwardMask(j), forwardData(j), io.dcacheResp.bits.data(8*(j+1)-1, 8*j))
  // )) // s2_rdataVec will be write to load queue
  // val rdata = rdataVec.asUInt
  // val rdataSel = LookupTree(s2_paddr(2, 0), List(
  //   "b000".U -> rdata(63, 0),
  //   "b001".U -> rdata(63, 8),
  //   "b010".U -> rdata(63, 16),
  //   "b011".U -> rdata(63, 24),
  //   "b100".U -> rdata(63, 32),
  //   "b101".U -> rdata(63, 40),
  //   "b110".U -> rdata(63, 48),
  //   "b111".U -> rdata(63, 56)
  // ))
  // val rdataPartialLoad = rdataHelper(s2_uop, rdataSel) // s2_rdataPartialLoad is not used
  io.feedbackFast.valid := io.in.valid && !io.in.bits.isLoadReplay && !s2_exception && io.lqReplayFull && io.out.bits.replayInfo.needReplay() && !io.out.bits.uop.robIdx.needFlush(io.redirect)
  io.feedbackFast.bits.hit := false.B 
  io.feedbackFast.bits.flushState := io.in.bits.ptwBack
  io.feedbackFast.bits.rsIdx := io.in.bits.rsIdx 
  io.feedbackFast.bits.sourceType := RSFeedbackType.lrqFull
  io.feedbackFast.bits.dataInvalidSqIdx := DontCare

  io.out.valid := io.in.valid && !io.feedbackFast.valid && !s2_is_hw_prefetch // hardware prefetch flow should not be writebacked 
  // write_lq_safe is needed by dup logic
  // io.write_lq_safe := !s2_tlb_miss && !s2_data_invalid
  // Inst will be canceled in store queue / lsq,
  // so we do not need to care about flush in load / store unit's out.valid
  io.out.bits := io.in.bits
  // io.out.bits.data := rdataPartialLoad
  io.out.bits.data := 0.U // data will be generated in load_s3
  // when exception occurs, set it to not miss and let it write back to rob (via int port)
  if (EnableFastForward) {
    io.out.bits.miss := s2_cache_miss &&
      !fullForward &&
      !s2_exception &&
      !s2_is_prefetch &&
      !s2_mmio
  } else {
    io.out.bits.miss := s2_cache_miss &&
      !s2_exception &&
      !s2_is_prefetch &&
      !s2_mmio
  }
  io.out.bits.uop.ctrl.fpWen := io.in.bits.uop.ctrl.fpWen && !s2_exception

  // val s2_loadDataFromDcache = new LoadDataFromDcacheBundle
  // s2_loadDataFromDcache.forwardMask := forwardMask
  // s2_loadDataFromDcache.forwardData := forwardData
  // s2_loadDataFromDcache.uop := io.out.bits.uop
  // s2_loadDataFromDcache.addrOffset := s2_paddr(2, 0)
  // // forward D or mshr
  // s2_loadDataFromDcache.forward_D := io.forward_D
  // s2_loadDataFromDcache.forwardData_D := io.forwardData_D
  // s2_loadDataFromDcache.forward_mshr := io.forward_mshr
  // s2_loadDataFromDcache.forwardData_mshr := io.forwardData_mshr
  // s2_loadDataFromDcache.forward_result_valid := io.forward_result_valid
  // io.loadDataFromDcache := RegEnable(s2_loadDataFromDcache, io.in.valid)
  io.loadDataFromDcache.respDcacheData := io.dcacheResp.bits.data_delayed
  io.loadDataFromDcache.forwardMask := RegEnable(forwardMask, io.in.valid)
  io.loadDataFromDcache.forwardData := RegEnable(forwardData, io.in.valid)
  io.loadDataFromDcache.uop := RegEnable(io.out.bits.uop, io.in.valid)
  io.loadDataFromDcache.addrOffset := RegEnable(s2_paddr(2, 0), io.in.valid)
  // forward D or mshr
  io.loadDataFromDcache.forward_D := RegEnable(io.forward_D, io.in.valid)
  io.loadDataFromDcache.forwardData_D := RegEnable(io.forwardData_D, io.in.valid)
  io.loadDataFromDcache.forward_mshr := RegEnable(io.forward_mshr, io.in.valid)
  io.loadDataFromDcache.forwardData_mshr := RegEnable(io.forwardData_mshr, io.in.valid)
  io.loadDataFromDcache.forward_result_valid := RegEnable(io.forward_result_valid, io.in.valid)

  io.s2_can_replay_from_fetch := !s2_mmio && !s2_is_prefetch && !s2_tlb_miss
  // if forward fail, replay this inst from fetch
  val debug_forwardFailReplay = s2_forward_fail && !s2_mmio && !s2_is_prefetch && !s2_tlb_miss
  // if ld-ld violation is detected, replay from this inst from fetch
  val debug_ldldVioReplay = false.B // s2_ldld_violation && !s2_mmio && !s2_is_prefetch && !s2_tlb_miss
  // io.out.bits.uop.ctrl.replayInst := false.B

  io.out.bits.mmio := s2_mmio
  io.out.bits.uop.ctrl.flushPipe := io.sentFastUop && s2_mmio // remove io.sentFastUop
  io.out.bits.uop.cf.exceptionVec := s2_exception_vec // cache error not included

  // For timing reasons, sometimes we can not let
  // io.out.bits.miss := s2_cache_miss && !s2_exception && !fullForward
  // We use io.dataForwarded instead. It means:
  // 1. Forward logic have prepared all data needed,
  //    and dcache query is no longer needed.
  // 2. ... or data cache tag error is detected, this kind of inst
  //    will not update miss queue. That is to say, if miss, that inst
  //    may not be refilled
  // Such inst will be writebacked from load queue.
  io.dataForwarded := s2_cache_miss && !s2_exception &&
    (fullForward || s2_cache_tag_error)
  // io.out.bits.forwardX will be send to lq
  io.out.bits.forwardMask := forwardMask
  // data from dcache is not included in io.out.bits.forwardData
  io.out.bits.forwardData := forwardData

  io.in.ready := io.out.ready || !io.in.valid

  // Generate replay signal caused by:
  // * st-ld violation check
  // * tlb miss
  // * dcache replay
  // * forward data invalid
  // * dcache miss
  io.out.bits.replayInfo.cause(LoadReplayCauses.waitStore) := s2_wait_store && !s2_mmio && !s2_is_prefetch
  io.out.bits.replayInfo.cause(LoadReplayCauses.tlbMiss) := s2_tlb_miss
  io.out.bits.replayInfo.cause(LoadReplayCauses.schedError) := (io.in.bits.replayInfo.cause(LoadReplayCauses.schedError) || s2_schedError) && !s2_mmio && !s2_is_prefetch
  io.out.bits.replayInfo.cause(LoadReplayCauses.dcacheMiss) := io.out.bits.miss 
  if (EnableFastForward) {
    io.out.bits.replayInfo.cause(LoadReplayCauses.dcacheReplay) := !(!s2_cache_replay || s2_is_prefetch || s2_mmio || s2_exception || fullForward)
  }else {
    io.out.bits.replayInfo.cause(LoadReplayCauses.dcacheReplay) := !(!s2_cache_replay || s2_is_prefetch || s2_mmio || s2_exception || io.dataForwarded) 
  }
  io.out.bits.replayInfo.cause(LoadReplayCauses.forwardFail) := s2_data_invalid && !s2_mmio && !s2_is_prefetch
  io.out.bits.replayInfo.cause(LoadReplayCauses.rejectEnq) := s2_rejectEnq && !s2_mmio && !s2_is_prefetch && !s2_exception
  io.out.bits.replayInfo.canForwardFullData := io.dataForwarded
  io.out.bits.replayInfo.dataInvalidSqIdx := io.dataInvalidSqIdx
  io.out.bits.replayInfo.addrInvalidSqIdx := io.addrInvalidSqIdx // io.in.bits.uop.sqIdx - io.oracleMDPQuery.resp.distance // io.addrInvalidSqIdx
  io.out.bits.replayInfo.replayCarry := io.dcacheResp.bits.replayCarry
  io.out.bits.replayInfo.missMSHRId := io.dcacheResp.bits.mshr_id
  io.out.bits.replayInfo.dataInLastBeat := io.in.bits.paddr(log2Up(refillBytes))
  io.out.bits.replayInfo.debug := io.in.bits.uop.debugInfo

  // To be removed
  val s2_need_replay_from_rs = WireInit(false.B)
  // s2_cache_replay is quite slow to generate, send it separately to LQ
  if (EnableFastForward) {
    io.s2_dcache_require_replay := s2_cache_replay && !fullForward
  } else {
    io.s2_dcache_require_replay := s2_cache_replay &&
      s2_need_replay_from_rs &&
      !io.dataForwarded &&
      !s2_is_prefetch &&
      io.out.bits.miss
  }

  XSPerfAccumulate("in_valid", io.in.valid)
  XSPerfAccumulate("in_fire", io.in.fire)
  XSPerfAccumulate("in_fire_first_issue", io.in.fire && io.in.bits.isFirstIssue)
  XSPerfAccumulate("dcache_miss", io.in.fire && s2_cache_miss)
  XSPerfAccumulate("dcache_miss_first_issue", io.in.fire && s2_cache_miss && io.in.bits.isFirstIssue)
  XSPerfAccumulate("full_forward", io.in.valid && fullForward)
  XSPerfAccumulate("dcache_miss_full_forward", io.in.valid && s2_cache_miss && fullForward)
  XSPerfAccumulate("stall_out", io.out.valid && !io.out.ready)
  XSPerfAccumulate("prefetch", io.in.fire && s2_is_prefetch)
  XSPerfAccumulate("prefetch_ignored", io.in.fire && s2_is_prefetch && s2_cache_replay) // ignore prefetch for mshr full / miss req port conflict
  XSPerfAccumulate("prefetch_miss", io.in.fire && s2_is_prefetch && s2_cache_miss) // prefetch req miss in l1 
  XSPerfAccumulate("prefetch_hit", io.in.fire && s2_is_prefetch && !s2_cache_miss) // prefetch req hit in l1 
  // prefetch a missed line in l1, and l1 accepted it
  XSPerfAccumulate("prefetch_accept", io.in.fire && s2_is_prefetch && s2_cache_miss && !s2_cache_replay) 
}

class LoadUnit(implicit p: Parameters) extends XSModule
  with HasLoadHelper
  with HasPerfEvents
  with HasDCacheParameters
  with HasCircularQueuePtrHelper
{
  val io = IO(new Bundle() {
    val loadIn = Flipped(Decoupled(new ExuInput))
    val loadOut = Decoupled(new ExuOutput)
    val rsIdx = Input(UInt())
    val redirect = Flipped(ValidIO(new Redirect))
    val isFirstIssue = Input(Bool())
    val dcache = new DCacheLoadIO
    val sbuffer = new LoadForwardQueryIO
    val lsq = new LoadToLsqIO
    val tlDchannel = Input(new DcacheToLduForwardIO)
    val forward_mshr = Flipped(new LduToMissqueueForwardIO)
    val refill = Flipped(ValidIO(new Refill))
    val fastUop = ValidIO(new MicroOp) // early wakeup signal generated in load_s1, send to RS in load_s2
    val trigger = Vec(3, new LoadUnitTriggerIO)

    val tlb = new TlbRequestIO(2)
    val pmp = Flipped(new PMPRespBundle()) // arrive same to tlb now

    // provide prefetch info
    val prefetch_train = ValidIO(new LdPrefetchTrainBundle())

    // hardware prefetch to l1 cache req
    val prefetch_req = Flipped(ValidIO(new L1PrefetchReq))

    // load to load fast path
    val fastpathOut = Output(new LoadToLoadIO)
    val fastpathIn = Input(new LoadToLoadIO)
    val loadFastMatch = Input(Bool())
    val loadFastImm = Input(UInt(12.W))

    // rs feedback
    val feedbackFast = ValidIO(new RSFeedback) // stage 2
    val feedbackSlow = ValidIO(new RSFeedback) // stage 3

    // load ecc
    val s3_delayedLoadError = Output(Bool()) // load ecc error
    // Note that io.s3_delayed_load_error and io.lsq.s3_delayed_load_error is different

    // load unit ctrl
    val csrCtrl = Flipped(new CustomCSRCtrlIO)

    val reExecuteQuery = Flipped(Vec(StorePipelineWidth, Valid(new LoadReExecuteQueryIO)))    // load replay
    val replay = Flipped(Decoupled(new LsPipelineBundle))
    val debug_ls = Output(new DebugLsInfoBundle)
    val lsTopdownInfo = Output(new LsTopdownInfo)
    val s2IsPointerChasing = Output(Bool()) // provide right pc for hw prefetch
    val lqReplayFull = Input(Bool())
  })

  val load_s0 = Module(new LoadUnit_S0)
  val load_s1 = Module(new LoadUnit_S1)
  val load_s2 = Module(new LoadUnit_S2)

  // load s0
  load_s0.io.in <> io.loadIn
  load_s0.io.dtlbReq <> io.tlb.req
  load_s0.io.dcacheReq <> io.dcache.req
  load_s0.io.rsIdx := io.rsIdx
  load_s0.io.isFirstIssue <> io.isFirstIssue
  load_s0.io.s0_kill := false.B
  load_s0.io.replay <> io.replay
  // hareware prefetch to l1
  load_s0.io.prefetch_in <> io.prefetch_req

  // we try pointerchasing if lfsrc_l2lForward_select condition is satisfied
  val s0_tryPointerChasing = load_s0.io.l2lForward_select
  val s0_pointerChasingVAddr = io.fastpathIn.data(5, 0) +& io.loadFastImm(5, 0)
  load_s0.io.fastpath.valid := io.fastpathIn.valid
  load_s0.io.fastpath.data := Cat(io.fastpathIn.data(XLEN-1, 6), s0_pointerChasingVAddr(5,0))

  val s1_data = PipelineConnect(load_s0.io.out, load_s1.io.in, true.B,
    load_s0.io.out.bits.uop.robIdx.needFlush(io.redirect) && !s0_tryPointerChasing).get

  // load s1
  // update s1_kill when any source has valid request
  load_s1.io.s1_kill := RegEnable(load_s0.io.s0_kill, false.B, io.loadIn.valid || io.replay.valid || io.fastpathIn.valid)
  io.tlb.req_kill := load_s1.io.s1_kill
  load_s1.io.dtlbResp <> io.tlb.resp
  load_s1.io.lsuPAddr <> io.dcache.s1_paddr_dup_lsu
  load_s1.io.dcachePAddr <> io.dcache.s1_paddr_dup_dcache
  load_s1.io.dcacheKill <> io.dcache.s1_kill
  load_s1.io.sbuffer <> io.sbuffer
  load_s1.io.lsq <> io.lsq.forward
  load_s1.io.dcacheBankConflict <> io.dcache.s1_bank_conflict
  load_s1.io.csrCtrl <> io.csrCtrl
  load_s1.io.reExecuteQuery := io.reExecuteQuery

  // when S0 has opportunity to try pointerchasing, make sure it truely goes to S1
  // which is S0's out is ready and dcache is ready
  val s0_doTryPointerChasing = s0_tryPointerChasing && load_s0.io.out.ready && load_s0.io.dcacheReq.ready
  val s1_tryPointerChasing = RegNext(s0_doTryPointerChasing, false.B)
  val s1_pointerChasingVAddr = RegEnable(s0_pointerChasingVAddr, s0_doTryPointerChasing)
  val cancelPointerChasing = WireInit(false.B)
  if (EnableLoadToLoadForward) {
    // Sometimes, we need to cancel the load-load forwarding.
    // These can be put at S0 if timing is bad at S1.
    // Case 0: CACHE_SET(base + offset) != CACHE_SET(base) (lowest 6-bit addition has an overflow)
    val addressMisMatch = s1_pointerChasingVAddr(6) || RegEnable(io.loadFastImm(11, 6).orR, s0_doTryPointerChasing)
    // Case 1: the address is not 64-bit aligned or the fuOpType is not LD
    val addressNotAligned = s1_pointerChasingVAddr(2, 0).orR
    val fuOpTypeIsNotLd = io.loadIn.bits.uop.ctrl.fuOpType =/= LSUOpType.ld
    // Case 2: this is not a valid load-load pair
    val notFastMatch = RegEnable(!io.loadFastMatch, s0_tryPointerChasing)
    // Case 3: this load-load uop is cancelled
    val isCancelled = !io.loadIn.valid
    when (s1_tryPointerChasing) {
      cancelPointerChasing := addressMisMatch || addressNotAligned || fuOpTypeIsNotLd || notFastMatch || isCancelled
      load_s1.io.in.bits.uop := io.loadIn.bits.uop
      load_s1.io.in.bits.rsIdx := io.rsIdx
      val spec_vaddr = s1_data.vaddr
      val vaddr = Cat(spec_vaddr(VAddrBits - 1, 6), s1_pointerChasingVAddr(5, 3), 0.U(3.W))
      load_s1.io.in.bits.vaddr := vaddr
      load_s1.io.in.bits.isFirstIssue := io.isFirstIssue
      // We need to replace vaddr(5, 3).
      val spec_paddr = io.tlb.resp.bits.paddr(0)
      load_s1.io.dtlbResp.bits.paddr.foreach(_ := Cat(spec_paddr(PAddrBits - 1, 6), s1_pointerChasingVAddr(5, 3), 0.U(3.W)))
      // recored tlb time when get the data to ensure the correctness of the latency calculation (although it should not record in here, because it does not use tlb)
      load_s1.io.in.bits.uop.debugInfo.tlbFirstReqTime := GTimer()
      load_s1.io.in.bits.uop.debugInfo.tlbRespTime := GTimer()
    }
    when (cancelPointerChasing) {
      load_s1.io.s1_kill := true.B
    }.otherwise {
      load_s0.io.s0_kill := s1_tryPointerChasing && !io.replay.fire
      when (s1_tryPointerChasing) {
        io.loadIn.ready := true.B
      }
    }

    XSPerfAccumulate("load_to_load_forward", s1_tryPointerChasing && !cancelPointerChasing)
    XSPerfAccumulate("load_to_load_forward_try", s1_tryPointerChasing)
    XSPerfAccumulate("load_to_load_forward_fail", cancelPointerChasing)
    XSPerfAccumulate("load_to_load_forward_fail_cancelled", cancelPointerChasing && isCancelled)
    XSPerfAccumulate("load_to_load_forward_fail_wakeup_mismatch", cancelPointerChasing && !isCancelled && notFastMatch)
    XSPerfAccumulate("load_to_load_forward_fail_op_not_ld",
      cancelPointerChasing && !isCancelled && !notFastMatch && fuOpTypeIsNotLd)
    XSPerfAccumulate("load_to_load_forward_fail_addr_align",
      cancelPointerChasing && !isCancelled && !notFastMatch && !fuOpTypeIsNotLd && addressNotAligned)
    XSPerfAccumulate("load_to_load_forward_fail_set_mismatch",
      cancelPointerChasing && !isCancelled && !notFastMatch && !fuOpTypeIsNotLd && !addressNotAligned && addressMisMatch)
  }
  PipelineConnect(load_s1.io.out, load_s2.io.in, true.B,
    load_s1.io.out.bits.uop.robIdx.needFlush(io.redirect) || cancelPointerChasing)

  val (forward_D, forwardData_D) = io.tlDchannel.forward(load_s1.io.out.valid && load_s1.io.out.bits.forward_tlDchannel, load_s1.io.out.bits.mshrid, load_s1.io.out.bits.paddr)

  io.forward_mshr.valid := load_s1.io.out.valid && load_s1.io.out.bits.forward_tlDchannel
  io.forward_mshr.mshrid := load_s1.io.out.bits.mshrid
  io.forward_mshr.paddr := load_s1.io.out.bits.paddr
  val (forward_result_valid, forward_mshr, forwardData_mshr) = io.forward_mshr.forward()

  XSPerfAccumulate("successfully_forward_channel_D", forward_D && forward_result_valid)
  XSPerfAccumulate("successfully_forward_mshr", forward_mshr && forward_result_valid)

  // load s2
  load_s2.io.redirect <> io.redirect
  load_s2.io.forward_D := forward_D
  load_s2.io.forwardData_D := forwardData_D
  load_s2.io.forward_result_valid := forward_result_valid
  load_s2.io.forward_mshr := forward_mshr
  load_s2.io.forwardData_mshr := forwardData_mshr
  io.s2IsPointerChasing := RegEnable(s1_tryPointerChasing && !cancelPointerChasing, load_s1.io.out.fire)
  io.prefetch_train.bits.fromLsPipelineBundle(load_s2.io.in.bits)
  // override miss bit
  io.prefetch_train.bits.miss := io.dcache.resp.bits.miss
  io.prefetch_train.bits.meta_prefetch := io.dcache.resp.bits.meta_prefetch
  io.prefetch_train.bits.meta_access := io.dcache.resp.bits.meta_access
  io.prefetch_train.valid := load_s2.io.in.fire && !load_s2.io.out.bits.mmio && !load_s2.io.in.bits.tlbMiss
  io.dcache.s2_kill := load_s2.io.dcache_kill // to kill mmio resp which are redirected
  if (env.FPGAPlatform)
    io.dcache.s2_pc := DontCare
  else
    io.dcache.s2_pc := load_s2.io.out.bits.uop.cf.pc
  load_s2.io.dcacheResp <> io.dcache.resp
  load_s2.io.pmpResp <> io.pmp
  load_s2.io.static_pm := RegNext(io.tlb.resp.bits.static_pm)
  load_s2.io.lsq.forwardData <> io.lsq.forward.forwardData
  load_s2.io.lsq.forwardMask <> io.lsq.forward.forwardMask
  load_s2.io.lsq.forwardMaskFast <> io.lsq.forward.forwardMaskFast // should not be used in load_s2
  load_s2.io.lsq.dataInvalid <> io.lsq.forward.dataInvalid
  load_s2.io.lsq.matchInvalid <> io.lsq.forward.matchInvalid
  load_s2.io.lsq.addrInvalid <> io.lsq.forward.addrInvalid
  load_s2.io.sbuffer.forwardData <> io.sbuffer.forwardData
  load_s2.io.sbuffer.forwardMask <> io.sbuffer.forwardMask
  load_s2.io.sbuffer.forwardMaskFast <> io.sbuffer.forwardMaskFast // should not be used in load_s2
  load_s2.io.sbuffer.dataInvalid <> io.sbuffer.dataInvalid // always false
  load_s2.io.sbuffer.matchInvalid <> io.sbuffer.matchInvalid
  load_s2.io.sbuffer.addrInvalid := DontCare // useless
  load_s2.io.dataInvalidSqIdx <> io.lsq.forward.dataInvalidSqIdx // provide dataInvalidSqIdx to make wakeup faster
  load_s2.io.addrInvalidSqIdx <> io.lsq.forward.addrInvalidSqIdx // provide addrInvalidSqIdx to make wakeup faster
  load_s2.io.csrCtrl <> io.csrCtrl
  load_s2.io.sentFastUop := io.fastUop.valid
  load_s2.io.reExecuteQuery := io.reExecuteQuery
  load_s2.io.loadLoadViolationQueryReq <> io.lsq.loadLoadViolationQuery.req
  load_s2.io.storeLoadViolationQueryReq <> io.lsq.storeLoadViolationQuery.req
  load_s2.io.feedbackFast <> io.feedbackFast
  load_s2.io.lqReplayFull <> io.lqReplayFull




  // pre-calcuate sqIdx mask in s0, then send it to lsq in s1 for forwarding
  val sqIdxMaskReg = RegNext(UIntToMask(load_s0.io.s0_sqIdx.value, StoreQueueSize))
  // to enable load-load, sqIdxMask must be calculated based on loadIn.uop
  // If the timing here is not OK, load-load forwarding has to be disabled.
  // Or we calculate sqIdxMask at RS??
  io.lsq.forward.sqIdxMask := sqIdxMaskReg
  if (EnableLoadToLoadForward) {
    when (s1_tryPointerChasing) {
      io.lsq.forward.sqIdxMask := UIntToMask(io.loadIn.bits.uop.sqIdx.value, StoreQueueSize)
    }
  }

  // // use s2_hit_way to select data received in s1
  // load_s2.io.dcacheResp.bits.data := Mux1H(RegNext(io.dcache.s1_hit_way), RegNext(io.dcache.s1_data))
  // assert(load_s2.io.dcacheResp.bits.data === io.dcache.resp.bits.data)

  // now io.fastUop.valid is sent to RS in load_s2
  // val forward_D_or_mshr_valid = forward_result_valid && (forward_D || forward_mshr)
  // val s2_dcache_hit = io.dcache.s2_hit || forward_D_or_mshr_valid // dcache hit dup in lsu side

  // never fast wakeup
  val forward_D_or_mshr_valid = forward_result_valid && (forward_D || forward_mshr)
  val s2_dcache_hit = io.dcache.s2_hit || forward_D_or_mshr_valid // dcache hit dup in lsu side

  io.fastUop.valid := RegNext(
      !io.dcache.s1_disable_fast_wakeup &&  // load fast wakeup should be disabled when dcache data read is not ready
      load_s1.io.in.valid && // valid load request
      !load_s1.io.s1_kill && // killed by load-load forwarding
      !load_s1.io.dtlbResp.bits.fast_miss && // not mmio or tlb miss, pf / af not included here
      !io.lsq.forward.dataInvalidFast // forward failed
    ) && 
    !RegNext(load_s1.io.out.bits.uop.robIdx.needFlush(io.redirect)) &&
    (load_s2.io.in.valid && s2_dcache_hit && !load_s2.io.out.bits.replayInfo.needReplay())
  io.fastUop.bits := RegNext(load_s1.io.out.bits.uop)

  XSDebug(load_s0.io.out.valid,
    p"S0: pc ${Hexadecimal(load_s0.io.out.bits.uop.cf.pc)}, lId ${Hexadecimal(load_s0.io.out.bits.uop.lqIdx.asUInt)}, " +
    p"vaddr ${Hexadecimal(load_s0.io.out.bits.vaddr)}, mask ${Hexadecimal(load_s0.io.out.bits.mask)}\n")
  XSDebug(load_s1.io.out.valid,
    p"S1: pc ${Hexadecimal(load_s1.io.out.bits.uop.cf.pc)}, lId ${Hexadecimal(load_s1.io.out.bits.uop.lqIdx.asUInt)}, tlb_miss ${io.tlb.resp.bits.miss}, " +
    p"paddr ${Hexadecimal(load_s1.io.out.bits.paddr)}, mmio ${load_s1.io.out.bits.mmio}\n")

  // load s2
  load_s2.io.out.ready := true.B
  val s2_loadOutValid = load_s2.io.out.valid 
  // generate duplicated load queue data wen
  val s2_loadValidVec = RegInit(0.U(6.W))
  val s2_loadLeftFire = load_s1.io.out.valid && load_s2.io.in.ready
  // val write_lq_safe = load_s2.io.write_lq_safe
  s2_loadValidVec := 0x0.U(6.W)
  when (s2_loadLeftFire && !load_s1.io.out.bits.isHWPrefetch) { s2_loadValidVec := 0x3f.U(6.W) } // TODO: refactor me
  when (load_s1.io.out.bits.uop.robIdx.needFlush(io.redirect)) { s2_loadValidVec := 0x0.U(6.W) }
  assert(RegNext((load_s2.io.in.valid === s2_loadValidVec(0)) || RegNext(load_s1.io.out.bits.isHWPrefetch)))

  // load s3
  // writeback to LSQ
  // Current dcache use MSHR
  // Load queue will be updated at s2 for both hit/miss int/fp load
  val s3_loadOutBits = RegEnable(load_s2.io.out.bits, s2_loadOutValid)
  val s3_loadOutValid = RegNext(s2_loadOutValid) && !RegNext(load_s2.io.out.bits.uop.robIdx.needFlush(io.redirect))
  io.lsq.loadIn.valid := s3_loadOutValid
  io.lsq.loadIn.bits := s3_loadOutBits

  /* <------- DANGEROUS: Don't change sequence here ! -------> */

  // make chisel happy
  val s3_loadValidVec = Reg(UInt(6.W))
  s3_loadValidVec := s2_loadValidVec
  io.lsq.loadIn.bits.lqDataWenDup := s3_loadValidVec.asBools

  // s2_dcache_require_replay signal will be RegNexted, then used in s3
  val s3_dcacheRequireReplay = RegNext(load_s2.io.s2_dcache_require_replay)
  val s3_delayedLoadError = 
    if (EnableAccurateLoadError) {
      io.dcache.resp.bits.error_delayed && RegNext(io.csrCtrl.cache_error_enable)
    } else {
      WireInit(false.B)
    }
  val s3_canReplayFromFetch = RegNext(load_s2.io.s2_can_replay_from_fetch)
  io.s3_delayedLoadError := false.B // s3_delayedLoadError
  io.lsq.loadIn.bits.dcacheRequireReplay := s3_dcacheRequireReplay


  val s3_vpMatchInvalid = RegNext(io.lsq.forward.matchInvalid || io.sbuffer.matchInvalid)
  val s3_ldld_replayFromFetch = 
    io.lsq.loadLoadViolationQuery.resp.valid &&
    io.lsq.loadLoadViolationQuery.resp.bits.replayFromFetch &&
    RegNext(io.csrCtrl.ldld_vio_check_enable)

  // write to rob and writeback bus
  val s3_replayInfo = s3_loadOutBits.replayInfo
  val s3_replayInst = s3_vpMatchInvalid || s3_ldld_replayFromFetch
  val s3_selReplayCause = PriorityEncoderOH(s3_replayInfo.cause.asUInt)
  dontTouch(s3_selReplayCause) // for debug
  val s3_forceReplay = s3_selReplayCause(LoadReplayCauses.schedError) || 
                       s3_selReplayCause(LoadReplayCauses.tlbMiss) ||
                       s3_selReplayCause(LoadReplayCauses.waitStore)

  val s3_exception = ExceptionNO.selectByFu(s3_loadOutBits.uop.cf.exceptionVec, lduCfg).asUInt.orR 
  when ((s3_exception || s3_delayedLoadError || s3_replayInst) && !s3_forceReplay) { 
    io.lsq.loadIn.bits.replayInfo.cause := 0.U.asTypeOf(s3_replayInfo.cause.cloneType)
  } .otherwise {
    io.lsq.loadIn.bits.replayInfo.cause := VecInit(s3_selReplayCause.asBools)
  }
  dontTouch(io.lsq.loadIn.bits.replayInfo.cause)



  // Int load, if hit, will be writebacked at s2
  val hitLoadOut = Wire(Valid(new ExuOutput))
  hitLoadOut.valid := s3_loadOutValid && !io.lsq.loadIn.bits.replayInfo.needReplay() && !s3_loadOutBits.mmio
  hitLoadOut.bits.uop := s3_loadOutBits.uop
  hitLoadOut.bits.uop.cf.exceptionVec(loadAccessFault) := s3_delayedLoadError && !s3_loadOutBits.tlbMiss  || 
                                                          s3_loadOutBits.uop.cf.exceptionVec(loadAccessFault) 
  hitLoadOut.bits.uop.ctrl.replayInst := s3_replayInst
  hitLoadOut.bits.data := s3_loadOutBits.data
  hitLoadOut.bits.redirectValid := false.B
  hitLoadOut.bits.redirect := DontCare
  hitLoadOut.bits.debug.isMMIO := s3_loadOutBits.mmio
  hitLoadOut.bits.debug.isPerfCnt := false.B
  hitLoadOut.bits.debug.paddr := s3_loadOutBits.paddr
  hitLoadOut.bits.debug.vaddr := s3_loadOutBits.vaddr
  hitLoadOut.bits.fflags := DontCare

  when (s3_forceReplay) {
    hitLoadOut.bits.uop.cf.exceptionVec := 0.U.asTypeOf(s3_loadOutBits.uop.cf.exceptionVec.cloneType)
  }

  /* <------- DANGEROUS: Don't change sequence here ! -------> */
  
  io.lsq.loadIn.bits.uop := hitLoadOut.bits.uop

  val s3_needRelease = s3_exception || io.lsq.loadIn.bits.replayInfo.needReplay()
  io.lsq.loadLoadViolationQuery.preReq := load_s1.io.out.valid
  io.lsq.loadLoadViolationQuery.release := s3_needRelease
  io.lsq.storeLoadViolationQuery.preReq := load_s1.io.out.valid
  io.lsq.storeLoadViolationQuery.release := s3_needRelease

  // feedback slow
  io.feedbackSlow.valid := s3_loadOutValid && !s3_loadOutBits.uop.robIdx.needFlush(io.redirect) && !s3_loadOutBits.isLoadReplay
  io.feedbackSlow.bits.hit := !io.lsq.loadIn.bits.replayInfo.needReplay() || io.lsq.loadIn.ready 
  io.feedbackSlow.bits.flushState := s3_loadOutBits.ptwBack
  io.feedbackSlow.bits.rsIdx := s3_loadOutBits.rsIdx
  io.feedbackSlow.bits.sourceType := RSFeedbackType.lrqFull
  io.feedbackSlow.bits.dataInvalidSqIdx := DontCare

  val s3_loadWbMeta = Mux(hitLoadOut.valid, hitLoadOut.bits, io.lsq.loadOut.bits)
  // data from load queue refill
  val s3_loadDataFromLQ = io.lsq.ldRawData
  val s3_rdataLQ = s3_loadDataFromLQ.mergedData()
  val s3_rdataSelLQ = LookupTree(s3_loadDataFromLQ.addrOffset, List(
    "b000".U -> s3_rdataLQ(63,  0),
    "b001".U -> s3_rdataLQ(63,  8),
    "b010".U -> s3_rdataLQ(63, 16),
    "b011".U -> s3_rdataLQ(63, 24),
    "b100".U -> s3_rdataLQ(63, 32),
    "b101".U -> s3_rdataLQ(63, 40),
    "b110".U -> s3_rdataLQ(63, 48),
    "b111".U -> s3_rdataLQ(63, 56)
  ))
  val s3_rdataPartialLoadLQ = rdataHelper(s3_loadDataFromLQ.uop, s3_rdataSelLQ)

  // data from dcache hit
  val s3_loadDataFromDcache = load_s2.io.loadDataFromDcache
  val s3_rdataDcache = s3_loadDataFromDcache.mergedData()
  val s3_rdataSelDcache = LookupTree(s3_loadDataFromDcache.addrOffset, List(
    "b000".U -> s3_rdataDcache(63,  0),
    "b001".U -> s3_rdataDcache(63,  8),
    "b010".U -> s3_rdataDcache(63, 16),
    "b011".U -> s3_rdataDcache(63, 24),
    "b100".U -> s3_rdataDcache(63, 32),
    "b101".U -> s3_rdataDcache(63, 40),
    "b110".U -> s3_rdataDcache(63, 48),
    "b111".U -> s3_rdataDcache(63, 56)
  ))
  val s3_rdataPartialLoadDcache = rdataHelper(s3_loadDataFromDcache.uop, s3_rdataSelDcache)

  // FIXME: add 1 cycle delay ?
  io.loadOut.bits := s3_loadWbMeta
  io.loadOut.bits.data := Mux(hitLoadOut.valid, s3_rdataPartialLoadDcache, s3_rdataPartialLoadLQ)
  io.loadOut.valid := hitLoadOut.valid && !hitLoadOut.bits.uop.robIdx.needFlush(io.redirect) ||
                    io.lsq.loadOut.valid && !io.lsq.loadOut.bits.uop.robIdx.needFlush(io.redirect) && !hitLoadOut.valid
  
  io.lsq.loadOut.ready := !hitLoadOut.valid

  // fast load to load forward
  io.fastpathOut.valid := hitLoadOut.valid // for debug only
  io.fastpathOut.data := s3_loadDataFromDcache.mergedData() // fastpath is for ld only

   // trigger
  val lastValidData = RegNext(RegEnable(io.loadOut.bits.data, io.loadOut.fire))
  val hitLoadAddrTriggerHitVec = Wire(Vec(3, Bool()))
  val lqLoadAddrTriggerHitVec = io.lsq.trigger.lqLoadAddrTriggerHitVec
  (0 until 3).map{i => {
    val tdata2 = RegNext(io.trigger(i).tdata2)
    val matchType = RegNext(io.trigger(i).matchType)
    val tEnable = RegNext(io.trigger(i).tEnable)

    hitLoadAddrTriggerHitVec(i) := TriggerCmp(RegNext(load_s2.io.out.bits.vaddr), tdata2, matchType, tEnable)
    io.trigger(i).addrHit := Mux(hitLoadOut.valid, hitLoadAddrTriggerHitVec(i), lqLoadAddrTriggerHitVec(i))
    io.trigger(i).lastDataHit := TriggerCmp(lastValidData, tdata2, matchType, tEnable)
  }}
  io.lsq.trigger.hitLoadAddrTriggerHitVec := hitLoadAddrTriggerHitVec

  // FIXME: please move this part to LoadQueueReplay 
  io.debug_ls := DontCare
  // io.debug_ls.s1.isBankConflict := load_s1.io.in.fire && (!load_s1.io.dcacheKill && load_s1.io.dcacheBankConflict)
  // io.debug_ls.s1.isLoadToLoadForward := load_s1.io.out.valid && s1_tryPointerChasing && !cancelPointerChasing
  // io.debug_ls.s1.isTlbFirstMiss := io.tlb.resp.valid && io.tlb.resp.bits.miss && io.tlb.resp.bits.debug.isFirstIssue
  // io.debug_ls.s1.isReplayFast := io.lsq.replayFast.valid && io.lsq.replayFast.needreplay
  // io.debug_ls.s1_robIdx := load_s1.io.in.bits.uop.robIdx.value
  // // s2
  // io.debug_ls.s2.isDcacheFirstMiss := load_s2.io.in.fire && load_s2.io.in.bits.isFirstIssue && load_s2.io.dcacheResp.bits.miss
  // io.debug_ls.s2.isForwardFail := load_s2.io.in.fire && load_s2.io.s2_forward_fail
  // io.debug_ls.s2.isReplaySlow := io.lsq.replaySlow.valid && io.lsq.replaySlow.needreplay
  // io.debug_ls.s2.isLoadReplayTLBMiss := io.lsq.replaySlow.valid && !io.lsq.replaySlow.tlb_hited
  // io.debug_ls.s2.isLoadReplayCacheMiss := io.lsq.replaySlow.valid && !io.lsq.replaySlow.cache_hited
  // io.debug_ls.replayCnt := DontCare
  // io.debug_ls.s2_robIdx := load_s2.io.in.bits.uop.robIdx.value

  io.lsTopdownInfo.s1.robIdx := load_s1.io.in.bits.uop.robIdx.value
  io.lsTopdownInfo.s1.vaddr_valid := load_s1.io.in.fire && load_s1.io.in.bits.hasROBEntry
  io.lsTopdownInfo.s1.vaddr_bits := load_s1.io.in.bits.vaddr
  io.lsTopdownInfo.s2.robIdx := load_s2.io.in.bits.uop.robIdx.value
  io.lsTopdownInfo.s2.paddr_valid := load_s2.io.in.fire && load_s2.io.in.bits.hasROBEntry && !load_s2.io.in.bits.tlbMiss
  io.lsTopdownInfo.s2.paddr_bits := load_s2.io.in.bits.paddr

  // bug lyq: some signals in perfEvents are no longer suitable for the current MemBlock design
  // hardware performance counter
  val perfEvents = Seq(
    ("load_s0_in_fire         ", load_s0.io.in.fire                                                                                                              ),
    ("load_to_load_forward    ", load_s1.io.out.valid && s1_tryPointerChasing && !cancelPointerChasing                                                           ),
    ("stall_dcache            ", load_s0.io.out.valid && load_s0.io.out.ready && !load_s0.io.dcacheReq.ready                                                     ),
    ("load_s1_in_fire         ", load_s1.io.in.fire                                                                                                              ),
    ("load_s1_tlb_miss        ", load_s1.io.in.fire && load_s1.io.dtlbResp.bits.miss                                                                             ),
    ("load_s2_in_fire         ", load_s2.io.in.fire                                                                                                              ),
    ("load_s2_dcache_miss     ", load_s2.io.in.fire && load_s2.io.dcacheResp.bits.miss                                                                           ),
  )
  generatePerfEvent()

  when(io.loadOut.fire){
    XSDebug("loadOut %x\n", io.loadOut.bits.uop.cf.pc)
  }
}
