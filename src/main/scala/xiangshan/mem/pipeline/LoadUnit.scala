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
import xiangshan.ExceptionNO._
import xiangshan._
import xiangshan.backend.fu.PMPRespBundle
import xiangshan.cache._
import xiangshan.cache.mmu.{TlbCmd, TlbReq, TlbRequestIO, TlbResp}

class LoadToLsqIO(implicit p: Parameters) extends XSBundle {
  val loadIn = ValidIO(new LqWriteBundle)
  val loadPaddrIn = ValidIO(new LqPaddrWriteBundle)
  val ldout = Flipped(DecoupledIO(new ExuOutput))
  val ldRawData = Input(new LoadDataFromLQBundle)
  val s2_load_data_forwarded = Output(Bool())
  val s3_delayed_load_error = Output(Bool())
  val s2_dcache_require_replay = Output(Bool())
  val s3_replay_from_fetch = Output(Bool()) // update uop.ctrl.replayInst in load queue in s3
  val forward = new PipeLoadForwardQueryIO
  val loadViolationQuery = new LoadViolationQueryIO
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
class LoadUnit_S0(implicit p: Parameters) extends XSModule with HasDCacheParameters{
  val io = IO(new Bundle() {
    val in = Flipped(Decoupled(new ExuInput))
    val out = Decoupled(new LsPipelineBundle)
    val dtlbReq = DecoupledIO(new TlbReq)
    val dcacheReq = DecoupledIO(new DCacheWordReq)
    val rsIdx = Input(UInt(log2Up(IssQueSize).W))
    val isFirstIssue = Input(Bool())
    val fastpath = Input(new LoadToLoadIO)
    val s0_kill = Input(Bool())
  })
  require(LoadPipelineWidth == exuParameters.LduCnt)

  val imm12 = io.in.bits.uop.ctrl.imm(11, 0)
  val s0_vaddr = WireInit(io.in.bits.src(0) + SignExt(imm12, VAddrBits))
  val s0_mask = WireInit(genWmask(s0_vaddr, io.in.bits.uop.ctrl.fuOpType(1,0)))
  val s0_uop = WireInit(io.in.bits.uop)

  val tryFastpath = WireInit(false.B)
  if (EnableLoadToLoadForward) {
    tryFastpath := io.fastpath.valid
    // When there's no valid instruction from RS, we try the load-to-load forwarding.
    when (!io.in.valid) {
      s0_vaddr := io.fastpath.data
      // Assume the pointer chasing is always ld.
      s0_uop.ctrl.fuOpType := LSUOpType.ld
      s0_mask := genWmask(0.U, LSUOpType.ld)
    }
  }

  val isSoftPrefetch = LSUOpType.isPrefetch(s0_uop.ctrl.fuOpType)
  val isSoftPrefetchRead = s0_uop.ctrl.fuOpType === LSUOpType.prefetch_r
  val isSoftPrefetchWrite = s0_uop.ctrl.fuOpType === LSUOpType.prefetch_w

  // query DTLB
  io.dtlbReq.valid := io.in.valid || tryFastpath
  io.dtlbReq.bits.vaddr := s0_vaddr
  io.dtlbReq.bits.cmd := TlbCmd.read
  io.dtlbReq.bits.size := LSUOpType.size(s0_uop.ctrl.fuOpType)
  io.dtlbReq.bits.robIdx := s0_uop.robIdx
  io.dtlbReq.bits.debug.pc := s0_uop.cf.pc
  io.dtlbReq.bits.debug.isFirstIssue := io.isFirstIssue

  // query DCache
  io.dcacheReq.valid := io.in.valid || tryFastpath
  when (isSoftPrefetchRead) {
    io.dcacheReq.bits.cmd  := MemoryOpConstants.M_PFR
  }.elsewhen (isSoftPrefetchWrite) {
    io.dcacheReq.bits.cmd  := MemoryOpConstants.M_PFW
  }.otherwise {
    io.dcacheReq.bits.cmd  := MemoryOpConstants.M_XRD
  }
  io.dcacheReq.bits.addr := s0_vaddr
  io.dcacheReq.bits.mask := s0_mask
  io.dcacheReq.bits.data := DontCare
  when(isSoftPrefetch) {
    io.dcacheReq.bits.instrtype := SOFT_PREFETCH.U
  }.otherwise {
    io.dcacheReq.bits.instrtype := LOAD_SOURCE.U
  }

  // TODO: update cache meta
  io.dcacheReq.bits.id   := DontCare

  val addrAligned = LookupTree(s0_uop.ctrl.fuOpType(1, 0), List(
    "b00".U   -> true.B,                   //b
    "b01".U   -> (s0_vaddr(0)    === 0.U), //h
    "b10".U   -> (s0_vaddr(1, 0) === 0.U), //w
    "b11".U   -> (s0_vaddr(2, 0) === 0.U)  //d
  ))

  io.out.valid := (io.in.valid || tryFastpath) && io.dcacheReq.ready && !io.s0_kill

  io.out.bits := DontCare
  io.out.bits.vaddr := s0_vaddr
  io.out.bits.mask := s0_mask
  io.out.bits.uop := s0_uop
  io.out.bits.uop.cf.exceptionVec(loadAddrMisaligned) := !addrAligned
  io.out.bits.rsIdx := io.rsIdx
  io.out.bits.isFirstIssue := io.isFirstIssue
  io.out.bits.isSoftPrefetch := isSoftPrefetch

  io.in.ready := !io.in.valid || (io.out.ready && io.dcacheReq.ready)

  XSDebug(io.dcacheReq.fire,
    p"[DCACHE LOAD REQ] pc ${Hexadecimal(s0_uop.cf.pc)}, vaddr ${Hexadecimal(s0_vaddr)}\n"
  )
  XSPerfAccumulate("in_valid", io.in.valid)
  XSPerfAccumulate("in_fire", io.in.fire)
  XSPerfAccumulate("in_fire_first_issue", io.in.valid && io.isFirstIssue)
  XSPerfAccumulate("stall_out", io.out.valid && !io.out.ready && io.dcacheReq.ready)
  XSPerfAccumulate("stall_dcache", io.out.valid && io.out.ready && !io.dcacheReq.ready)
  XSPerfAccumulate("addr_spec_success", io.out.fire && s0_vaddr(VAddrBits-1, 12) === io.in.bits.src(0)(VAddrBits-1, 12))
  XSPerfAccumulate("addr_spec_failed", io.out.fire && s0_vaddr(VAddrBits-1, 12) =/= io.in.bits.src(0)(VAddrBits-1, 12))
  XSPerfAccumulate("addr_spec_success_once", io.out.fire && s0_vaddr(VAddrBits-1, 12) === io.in.bits.src(0)(VAddrBits-1, 12) && io.isFirstIssue)
  XSPerfAccumulate("addr_spec_failed_once", io.out.fire && s0_vaddr(VAddrBits-1, 12) =/= io.in.bits.src(0)(VAddrBits-1, 12) && io.isFirstIssue)
}


// Load Pipeline Stage 1
// TLB resp (send paddr to dcache)
class LoadUnit_S1(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val in = Flipped(Decoupled(new LsPipelineBundle))
    val s1_kill = Input(Bool())
    val out = Decoupled(new LsPipelineBundle)
    val dtlbResp = Flipped(DecoupledIO(new TlbResp(2)))
    val lsuPAddr = Output(UInt(PAddrBits.W))
    val dcachePAddr = Output(UInt(PAddrBits.W))
    val dcacheKill = Output(Bool())
    val dcacheBankConflict = Input(Bool())
    val fullForwardFast = Output(Bool())
    val sbuffer = new LoadForwardQueryIO
    val lsq = new PipeLoadForwardQueryIO
    val loadViolationQueryReq = Decoupled(new LoadViolationQueryReq)
    val rsFeedback = ValidIO(new RSFeedback)
    val csrCtrl = Flipped(new CustomCSRCtrlIO)
    val needLdVioCheckRedo = Output(Bool())
  })

  val s1_uop = io.in.bits.uop
  val s1_paddr_dup_lsu = io.dtlbResp.bits.paddr(0)
  val s1_paddr_dup_dcache = io.dtlbResp.bits.paddr(1)
  // af & pf exception were modified below.
  val s1_exception = ExceptionNO.selectByFu(io.out.bits.uop.cf.exceptionVec, lduCfg).asUInt.orR
  val s1_tlb_miss = io.dtlbResp.bits.miss
  val s1_mask = io.in.bits.mask
  val s1_bank_conflict = io.dcacheBankConflict

  io.out.bits := io.in.bits // forwardXX field will be updated in s1

  io.dtlbResp.ready := true.B

  io.lsuPAddr := s1_paddr_dup_lsu
  io.dcachePAddr := s1_paddr_dup_dcache
  //io.dcacheKill := s1_tlb_miss || s1_exception || s1_mmio
  io.dcacheKill := s1_tlb_miss || s1_exception || io.s1_kill
  // load forward query datapath
  io.sbuffer.valid := io.in.valid && !(s1_exception || s1_tlb_miss || io.s1_kill)
  io.sbuffer.vaddr := io.in.bits.vaddr
  io.sbuffer.paddr := s1_paddr_dup_lsu
  io.sbuffer.uop := s1_uop
  io.sbuffer.sqIdx := s1_uop.sqIdx
  io.sbuffer.mask := s1_mask
  io.sbuffer.pc := s1_uop.cf.pc // FIXME: remove it

  io.lsq.valid := io.in.valid && !(s1_exception || s1_tlb_miss || io.s1_kill)
  io.lsq.vaddr := io.in.bits.vaddr
  io.lsq.paddr := s1_paddr_dup_lsu
  io.lsq.uop := s1_uop
  io.lsq.sqIdx := s1_uop.sqIdx
  io.lsq.sqIdxMask := DontCare // will be overwritten by sqIdxMask pre-generated in s0
  io.lsq.mask := s1_mask
  io.lsq.pc := s1_uop.cf.pc // FIXME: remove it

  // ld-ld violation query
  io.loadViolationQueryReq.valid := io.in.valid && !(s1_exception || s1_tlb_miss || io.s1_kill)
  io.loadViolationQueryReq.bits.paddr := s1_paddr_dup_lsu
  io.loadViolationQueryReq.bits.uop := s1_uop

  // Generate forwardMaskFast to wake up insts earlier
  val forwardMaskFast = io.lsq.forwardMaskFast.asUInt | io.sbuffer.forwardMaskFast.asUInt
  io.fullForwardFast := ((~forwardMaskFast).asUInt & s1_mask) === 0.U

  // Generate feedback signal caused by:
  // * dcache bank conflict
  // * need redo ld-ld violation check
  val needLdVioCheckRedo = io.loadViolationQueryReq.valid &&
    !io.loadViolationQueryReq.ready &&
    RegNext(io.csrCtrl.ldld_vio_check_enable)
  io.needLdVioCheckRedo := needLdVioCheckRedo
  io.rsFeedback.valid := io.in.valid && (s1_bank_conflict || needLdVioCheckRedo) && !io.s1_kill
  io.rsFeedback.bits.hit := false.B // we have found s1_bank_conflict / re do ld-ld violation check
  io.rsFeedback.bits.rsIdx := io.in.bits.rsIdx
  io.rsFeedback.bits.flushState := io.in.bits.ptwBack
  io.rsFeedback.bits.sourceType := Mux(s1_bank_conflict, RSFeedbackType.bankConflict, RSFeedbackType.ldVioCheckRedo)
  io.rsFeedback.bits.dataInvalidSqIdx := DontCare

  // if replay is detected in load_s1,
  // load inst will be canceled immediately
  io.out.valid := io.in.valid && !io.rsFeedback.valid && !io.s1_kill
  io.out.bits.paddr := s1_paddr_dup_lsu
  io.out.bits.tlbMiss := s1_tlb_miss

  // current ori test will cause the case of ldest == 0, below will be modifeid in the future.
  // af & pf exception were modified
  io.out.bits.uop.cf.exceptionVec(loadPageFault) := io.dtlbResp.bits.excp(0).pf.ld
  io.out.bits.uop.cf.exceptionVec(loadAccessFault) := io.dtlbResp.bits.excp(0).af.ld

  io.out.bits.ptwBack := io.dtlbResp.bits.ptwBack
  io.out.bits.rsIdx := io.in.bits.rsIdx

  io.out.bits.isSoftPrefetch := io.in.bits.isSoftPrefetch

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
class LoadUnit_S2(implicit p: Parameters) extends XSModule with HasLoadHelper {
  val io = IO(new Bundle() {
    val in = Flipped(Decoupled(new LsPipelineBundle))
    val out = Decoupled(new LsPipelineBundle)
    val rsFeedback = ValidIO(new RSFeedback)
    val dcacheResp = Flipped(DecoupledIO(new DCacheWordResp))
    val pmpResp = Flipped(new PMPRespBundle())
    val lsq = new LoadForwardQueryIO
    val dataInvalidSqIdx = Input(UInt())
    val sbuffer = new LoadForwardQueryIO
    val dataForwarded = Output(Bool())
    val s2_dcache_require_replay = Output(Bool())
    val fullForward = Output(Bool())
    val fastpath = Output(new LoadToLoadIO)
    val dcache_kill = Output(Bool())
    val s3_delayed_load_error = Output(Bool())
    val loadViolationQueryResp = Flipped(Valid(new LoadViolationQueryResp))
    val csrCtrl = Flipped(new CustomCSRCtrlIO)
    val sentFastUop = Input(Bool())
    val static_pm = Input(Valid(Bool())) // valid for static, bits for mmio
    val s2_can_replay_from_fetch = Output(Bool()) // dirty code
    val loadDataFromDcache = Output(new LoadDataFromDcacheBundle)
  })

  val pmp = WireInit(io.pmpResp)
  when (io.static_pm.valid) {
    pmp.ld := false.B
    pmp.st := false.B
    pmp.instr := false.B
    pmp.mmio := io.static_pm.bits
  }

  val s2_is_prefetch = io.in.bits.isSoftPrefetch

  // exception that may cause load addr to be invalid / illegal
  //
  // if such exception happen, that inst and its exception info
  // will be force writebacked to rob
  val s2_exception_vec = WireInit(io.in.bits.uop.cf.exceptionVec)
  s2_exception_vec(loadAccessFault) := io.in.bits.uop.cf.exceptionVec(loadAccessFault) || pmp.ld
  // soft prefetch will not trigger any exception (but ecc error interrupt may be triggered)
  when (s2_is_prefetch) {
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
  if (EnableAccurateLoadError) {
    io.s3_delayed_load_error := io.dcacheResp.bits.error_delayed &&
      io.csrCtrl.cache_error_enable &&
      RegNext(io.out.valid)
  } else {
    io.s3_delayed_load_error := false.B
  }

  val actually_mmio = pmp.mmio
  val s2_uop = io.in.bits.uop
  val s2_mask = io.in.bits.mask
  val s2_paddr = io.in.bits.paddr
  val s2_tlb_miss = io.in.bits.tlbMiss
  val s2_mmio = !s2_is_prefetch && actually_mmio && !s2_exception
  val s2_cache_miss = io.dcacheResp.bits.miss
  val s2_cache_replay = io.dcacheResp.bits.replay
  val s2_cache_tag_error = io.dcacheResp.bits.tag_error
  val s2_forward_fail = io.lsq.matchInvalid || io.sbuffer.matchInvalid
  val s2_ldld_violation = io.loadViolationQueryResp.valid &&
    io.loadViolationQueryResp.bits.have_violation &&
    RegNext(io.csrCtrl.ldld_vio_check_enable)
  val s2_data_invalid = io.lsq.dataInvalid && !s2_ldld_violation && !s2_exception

  io.dcache_kill := pmp.ld || pmp.mmio // move pmp resp kill to outside
  io.dcacheResp.ready := true.B
  val dcacheShouldResp = !(s2_tlb_miss || s2_exception || s2_mmio || s2_is_prefetch)
  assert(!(io.in.valid && (dcacheShouldResp && !io.dcacheResp.valid)), "DCache response got lost")

  // merge forward result
  // lsq has higher priority than sbuffer
  val forwardMask = Wire(Vec(8, Bool()))
  val forwardData = Wire(Vec(8, UInt(8.W)))

  val fullForward = ((~forwardMask.asUInt).asUInt & s2_mask) === 0.U && !io.lsq.dataInvalid
  io.lsq := DontCare
  io.sbuffer := DontCare
  io.fullForward := fullForward

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
  val rdataVec = VecInit((0 until XLEN / 8).map(j =>
    Mux(forwardMask(j), forwardData(j), io.dcacheResp.bits.data(8*(j+1)-1, 8*j))
  )) // s2_rdataVec will be write to load queue
  val rdata = rdataVec.asUInt
  val rdataSel = LookupTree(s2_paddr(2, 0), List(
    "b000".U -> rdata(63, 0),
    "b001".U -> rdata(63, 8),
    "b010".U -> rdata(63, 16),
    "b011".U -> rdata(63, 24),
    "b100".U -> rdata(63, 32),
    "b101".U -> rdata(63, 40),
    "b110".U -> rdata(63, 48),
    "b111".U -> rdata(63, 56)
  ))
  val rdataPartialLoad = rdataHelper(s2_uop, rdataSel) // s2_rdataPartialLoad is not used

  io.out.valid := io.in.valid && !s2_tlb_miss && !s2_data_invalid
  // Inst will be canceled in store queue / lsq,
  // so we do not need to care about flush in load / store unit's out.valid
  io.out.bits := io.in.bits
  // io.out.bits.data := rdataPartialLoad
  io.out.bits.data := 0.U // data will be generated in load_s3
  // when exception occurs, set it to not miss and let it write back to rob (via int port)
  if (EnableFastForward) {
    io.out.bits.miss := s2_cache_miss &&
      !s2_exception &&
      !fullForward &&
      !s2_is_prefetch
  } else {
    io.out.bits.miss := s2_cache_miss &&
      !s2_exception &&
      !s2_is_prefetch
  }
  io.out.bits.uop.ctrl.fpWen := io.in.bits.uop.ctrl.fpWen && !s2_exception

  io.loadDataFromDcache.dcacheData := io.dcacheResp.bits.data
  io.loadDataFromDcache.forwardMask := forwardMask
  io.loadDataFromDcache.forwardData := forwardData
  io.loadDataFromDcache.uop := io.out.bits.uop
  io.loadDataFromDcache.addrOffset := s2_paddr(2, 0)

  io.s2_can_replay_from_fetch := !s2_mmio && !s2_is_prefetch && !s2_tlb_miss
  // if forward fail, replay this inst from fetch
  val debug_forwardFailReplay = s2_forward_fail && !s2_mmio && !s2_is_prefetch && !s2_tlb_miss
  // if ld-ld violation is detected, replay from this inst from fetch
  val debug_ldldVioReplay = s2_ldld_violation && !s2_mmio && !s2_is_prefetch && !s2_tlb_miss
  // io.out.bits.uop.ctrl.replayInst := false.B

  io.out.bits.mmio := s2_mmio
  io.out.bits.uop.ctrl.flushPipe := s2_mmio && io.sentFastUop
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
    (fullForward || io.csrCtrl.cache_error_enable && s2_cache_tag_error)
  // io.out.bits.forwardX will be send to lq
  io.out.bits.forwardMask := forwardMask
  // data from dcache is not included in io.out.bits.forwardData
  io.out.bits.forwardData := forwardData

  io.in.ready := io.out.ready || !io.in.valid

  // feedback tlb result to RS
  io.rsFeedback.valid := io.in.valid
  val s2_need_replay_from_rs = Wire(Bool())
  if (EnableFastForward) {
    s2_need_replay_from_rs :=
      s2_tlb_miss || // replay if dtlb miss
      s2_cache_replay && !s2_is_prefetch && !s2_mmio && !s2_exception && !fullForward || // replay if dcache miss queue full / busy
      s2_data_invalid && !s2_is_prefetch // replay if store to load forward data is not ready
  } else {
    // Note that if all parts of data are available in sq / sbuffer, replay required by dcache will not be scheduled
    s2_need_replay_from_rs :=
      s2_tlb_miss || // replay if dtlb miss
      s2_cache_replay && !s2_is_prefetch && !s2_mmio && !s2_exception && !io.dataForwarded || // replay if dcache miss queue full / busy
      s2_data_invalid && !s2_is_prefetch // replay if store to load forward data is not ready
  }
  io.rsFeedback.bits.hit := !s2_need_replay_from_rs
  io.rsFeedback.bits.rsIdx := io.in.bits.rsIdx
  io.rsFeedback.bits.flushState := io.in.bits.ptwBack
  io.rsFeedback.bits.sourceType := Mux(s2_tlb_miss, RSFeedbackType.tlbMiss,
    Mux(s2_cache_replay,
      RSFeedbackType.mshrFull,
      RSFeedbackType.dataInvalid
    )
  )
  io.rsFeedback.bits.dataInvalidSqIdx.value := io.dataInvalidSqIdx
  io.rsFeedback.bits.dataInvalidSqIdx.flag := DontCare

  // s2_cache_replay is quite slow to generate, send it separately to LQ
  if (EnableFastForward) {
    io.s2_dcache_require_replay := s2_cache_replay && !fullForward
  } else {
    io.s2_dcache_require_replay := s2_cache_replay &&
      !io.rsFeedback.bits.hit &&
      !io.dataForwarded &&
      !s2_is_prefetch &&
      io.out.bits.miss
  }

  // fast load to load forward
  io.fastpath.valid := RegNext(io.out.valid) // for debug only
  io.fastpath.data := RegNext(rdata) // fastpath is for ld only

  XSDebug(io.out.fire, "[DCACHE LOAD RESP] pc %x rdata %x <- D$ %x + fwd %x(%b)\n",
    s2_uop.cf.pc, rdataPartialLoad, io.dcacheResp.bits.data,
    forwardData.asUInt, forwardMask.asUInt
  )

  XSPerfAccumulate("in_valid", io.in.valid)
  XSPerfAccumulate("in_fire", io.in.fire)
  XSPerfAccumulate("in_fire_first_issue", io.in.fire && io.in.bits.isFirstIssue)
  XSPerfAccumulate("dcache_miss", io.in.fire && s2_cache_miss)
  XSPerfAccumulate("dcache_miss_first_issue", io.in.fire && s2_cache_miss && io.in.bits.isFirstIssue)
  XSPerfAccumulate("full_forward", io.in.valid && fullForward)
  XSPerfAccumulate("dcache_miss_full_forward", io.in.valid && s2_cache_miss && fullForward)
  XSPerfAccumulate("replay",  io.rsFeedback.valid && !io.rsFeedback.bits.hit)
  XSPerfAccumulate("replay_tlb_miss", io.rsFeedback.valid && !io.rsFeedback.bits.hit && s2_tlb_miss)
  XSPerfAccumulate("replay_cache", io.rsFeedback.valid && !io.rsFeedback.bits.hit && !s2_tlb_miss && s2_cache_replay)
  XSPerfAccumulate("stall_out", io.out.valid && !io.out.ready)
  XSPerfAccumulate("replay_from_fetch_forward", io.out.valid && debug_forwardFailReplay)
  XSPerfAccumulate("replay_from_fetch_load_vio", io.out.valid && debug_ldldVioReplay)
}

class LoadUnit(implicit p: Parameters) extends XSModule with HasLoadHelper with HasPerfEvents {
  val io = IO(new Bundle() {
    val ldin = Flipped(Decoupled(new ExuInput))
    val ldout = Decoupled(new ExuOutput)
    val redirect = Flipped(ValidIO(new Redirect))
    val feedbackSlow = ValidIO(new RSFeedback)
    val feedbackFast = ValidIO(new RSFeedback)
    val rsIdx = Input(UInt(log2Up(IssQueSize).W))
    val isFirstIssue = Input(Bool())
    val dcache = new DCacheLoadIO
    val sbuffer = new LoadForwardQueryIO
    val lsq = new LoadToLsqIO
    val fastUop = ValidIO(new MicroOp) // early wakeup signal generated in load_s1, send to RS in load_s2
    val trigger = Vec(TriggerNum, new LoadUnitTriggerIO)

    val tlb = new TlbRequestIO(2)
    val pmp = Flipped(new PMPRespBundle()) // arrive same to tlb now

    // provide prefetch info
    val prefetch_train = ValidIO(new LsPipelineBundle())

    val fastpathOut = Output(new LoadToLoadIO)
    val fastpathIn = Input(new LoadToLoadIO)
    val loadFastMatch = Input(Bool())
    val loadFastImm = Input(UInt(12.W))

    val s3_delayed_load_error = Output(Bool()) // load ecc error
    // Note that io.s3_delayed_load_error and io.lsq.s3_delayed_load_error is different

    val csrCtrl = Flipped(new CustomCSRCtrlIO)
    val s2IsPointerChasing = Output(Bool())
  })

  val load_s0 = Module(new LoadUnit_S0)
  val load_s1 = Module(new LoadUnit_S1)
  val load_s2 = Module(new LoadUnit_S2)

  // load s0
  load_s0.io.in <> io.ldin
  load_s0.io.dtlbReq <> io.tlb.req
  load_s0.io.dcacheReq <> io.dcache.req
  load_s0.io.rsIdx := io.rsIdx
  load_s0.io.isFirstIssue := io.isFirstIssue
  load_s0.io.fastpath := io.fastpathIn
  load_s0.io.s0_kill := false.B
  val s0_tryPointerChasing = !io.ldin.valid && io.fastpathIn.valid
  val s0_pointerChasingVAddr = io.fastpathIn.data(5, 0) +& io.loadFastImm(5, 0)

  val s1_data = PipelineConnect(load_s0.io.out, load_s1.io.in, true.B,
    load_s0.io.out.bits.uop.robIdx.needFlush(io.redirect) && !s0_tryPointerChasing).get

  // load s1
  load_s1.io.s1_kill := RegEnable(load_s0.io.s0_kill, false.B, load_s0.io.in.valid || io.fastpathIn.valid)
  io.tlb.req_kill := load_s1.io.s1_kill
  load_s1.io.dtlbResp <> io.tlb.resp
  io.dcache.s1_paddr_dup_lsu <> load_s1.io.lsuPAddr
  io.dcache.s1_paddr_dup_dcache <> load_s1.io.dcachePAddr
  io.dcache.s1_kill <> load_s1.io.dcacheKill
  load_s1.io.sbuffer <> io.sbuffer
  load_s1.io.lsq <> io.lsq.forward
  load_s1.io.loadViolationQueryReq <> io.lsq.loadViolationQuery.req
  load_s1.io.dcacheBankConflict <> io.dcache.s1_bank_conflict
  load_s1.io.csrCtrl <> io.csrCtrl

  // provide paddr for lq
  io.lsq.loadPaddrIn.valid := load_s1.io.out.valid
  io.lsq.loadPaddrIn.bits.lqIdx := load_s1.io.out.bits.uop.lqIdx
  io.lsq.loadPaddrIn.bits.paddr := load_s1.io.lsuPAddr

  val s0_doTryPointerChasing = s0_tryPointerChasing && load_s0.io.in.ready && load_s0.io.dcacheReq.ready
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
    val fuOpTypeIsNotLd = io.ldin.bits.uop.ctrl.fuOpType =/= LSUOpType.ld
    // Case 2: this is not a valid load-load pair
    val notFastMatch = RegEnable(!io.loadFastMatch, s0_tryPointerChasing)
    // Case 3: this load-load uop is cancelled
    val isCancelled = !io.ldin.valid
    when (s1_tryPointerChasing) {
      cancelPointerChasing := addressMisMatch || addressNotAligned || fuOpTypeIsNotLd || notFastMatch || isCancelled
      load_s1.io.in.bits.uop := io.ldin.bits.uop
      val spec_vaddr = s1_data.vaddr
      val vaddr = Cat(spec_vaddr(VAddrBits - 1, 6), s1_pointerChasingVAddr(5, 3), 0.U(3.W))
      load_s1.io.in.bits.vaddr := vaddr
      load_s1.io.in.bits.rsIdx := io.rsIdx
      load_s1.io.in.bits.isFirstIssue := io.isFirstIssue
      // We need to replace vaddr(5, 3).
      val spec_paddr = io.tlb.resp.bits.paddr(0)
      load_s1.io.dtlbResp.bits.paddr.foreach(_ := Cat(spec_paddr(PAddrBits - 1, 6), s1_pointerChasingVAddr(5, 3), 0.U(3.W)))
    }
    when (cancelPointerChasing) {
      load_s1.io.s1_kill := true.B
    }.otherwise {
      load_s0.io.s0_kill := s1_tryPointerChasing
      when (s1_tryPointerChasing) {
        io.ldin.ready := true.B
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

  // load s2
  io.s2IsPointerChasing := RegEnable(s1_tryPointerChasing && !cancelPointerChasing, load_s1.io.out.fire)
  io.prefetch_train.bits := load_s2.io.in.bits
  // override miss bit
  io.prefetch_train.bits.miss := io.dcache.resp.bits.miss
  io.prefetch_train.valid := load_s2.io.in.fire && !load_s2.io.out.bits.mmio && !load_s2.io.in.bits.tlbMiss
  io.dcache.s2_kill := load_s2.io.dcache_kill // to kill mmio resp which are redirected
  load_s2.io.dcacheResp <> io.dcache.resp
  load_s2.io.pmpResp <> io.pmp
  load_s2.io.static_pm := RegNext(io.tlb.resp.bits.static_pm)
  load_s2.io.lsq.forwardData <> io.lsq.forward.forwardData
  load_s2.io.lsq.forwardMask <> io.lsq.forward.forwardMask
  load_s2.io.lsq.forwardMaskFast <> io.lsq.forward.forwardMaskFast // should not be used in load_s2
  load_s2.io.lsq.dataInvalid <> io.lsq.forward.dataInvalid
  load_s2.io.lsq.matchInvalid <> io.lsq.forward.matchInvalid
  load_s2.io.sbuffer.forwardData <> io.sbuffer.forwardData
  load_s2.io.sbuffer.forwardMask <> io.sbuffer.forwardMask
  load_s2.io.sbuffer.forwardMaskFast <> io.sbuffer.forwardMaskFast // should not be used in load_s2
  load_s2.io.sbuffer.dataInvalid <> io.sbuffer.dataInvalid // always false
  load_s2.io.sbuffer.matchInvalid <> io.sbuffer.matchInvalid
  load_s2.io.dataForwarded <> io.lsq.s2_load_data_forwarded
  load_s2.io.fastpath <> io.fastpathOut
  load_s2.io.dataInvalidSqIdx := io.lsq.forward.dataInvalidSqIdx // provide dataInvalidSqIdx to make wakeup faster
  load_s2.io.loadViolationQueryResp <> io.lsq.loadViolationQuery.resp
  load_s2.io.csrCtrl <> io.csrCtrl
  load_s2.io.sentFastUop := io.fastUop.valid

  // feedback bank conflict / ld-vio check struct hazard to rs
  io.feedbackFast.bits := RegNext(load_s1.io.rsFeedback.bits)
  io.feedbackFast.valid := RegNext(load_s1.io.rsFeedback.valid && !load_s1.io.out.bits.uop.robIdx.needFlush(io.redirect))

  // pre-calcuate sqIdx mask in s0, then send it to lsq in s1 for forwarding
  val sqIdxMaskReg = RegNext(UIntToMask(load_s0.io.in.bits.uop.sqIdx.value, StoreQueueSize))
  // to enable load-load, sqIdxMask must be calculated based on ldin.uop
  // If the timing here is not OK, load-load forwarding has to be disabled.
  // Or we calculate sqIdxMask at RS??
  io.lsq.forward.sqIdxMask := sqIdxMaskReg
  if (EnableLoadToLoadForward) {
    when (s1_tryPointerChasing) {
      io.lsq.forward.sqIdxMask := UIntToMask(io.ldin.bits.uop.sqIdx.value, StoreQueueSize)
    }
  }

  // generate io.fastUop.valid for RS
  // now io.fastUop.valid is sent to RS in load_s2
  val s2_dcache_hit = io.dcache.s2_hit // dcache hit dup in lsu side

  io.fastUop.valid := RegNext(
      !io.dcache.s1_disable_fast_wakeup &&  // load fast wakeup should be disabled when dcache data read is not ready
      load_s1.io.in.valid && // valid load request
      !load_s1.io.s1_kill && // killed by load-load forwarding
      !load_s1.io.dtlbResp.bits.fast_miss && // not mmio or tlb miss, pf / af not included here
      !io.lsq.forward.dataInvalidFast // forward failed
    ) &&
    !RegNext(load_s1.io.needLdVioCheckRedo) && // load-load violation check: load paddr cam struct hazard
    !RegNext(load_s1.io.out.bits.uop.robIdx.needFlush(io.redirect)) &&
    s2_dcache_hit // dcache hit in lsu side

  io.fastUop.bits := RegNext(load_s1.io.out.bits.uop)

  XSDebug(load_s0.io.out.valid,
    p"S0: pc ${Hexadecimal(load_s0.io.out.bits.uop.cf.pc)}, lId ${Hexadecimal(load_s0.io.out.bits.uop.lqIdx.asUInt)}, " +
    p"vaddr ${Hexadecimal(load_s0.io.out.bits.vaddr)}, mask ${Hexadecimal(load_s0.io.out.bits.mask)}\n")
  XSDebug(load_s1.io.out.valid,
    p"S1: pc ${Hexadecimal(load_s1.io.out.bits.uop.cf.pc)}, lId ${Hexadecimal(load_s1.io.out.bits.uop.lqIdx.asUInt)}, tlb_miss ${io.tlb.resp.bits.miss}, " +
    p"paddr ${Hexadecimal(load_s1.io.out.bits.paddr)}, mmio ${load_s1.io.out.bits.mmio}\n")

  // writeback to LSQ
  // Current dcache use MSHR
  // Load queue will be updated at s2 for both hit/miss int/fp load
  io.lsq.loadIn.valid := load_s2.io.out.valid
  // generate LqWriteBundle from LsPipelineBundle
  io.lsq.loadIn.bits.fromLsPipelineBundle(load_s2.io.out.bits)
  // generate duplicated load queue data wen
  val load_s2_valid_vec = RegInit(0.U(6.W))
  val load_s2_leftFire = load_s1.io.out.valid && load_s2.io.in.ready
  load_s2_valid_vec := 0x0.U(6.W)
  when (load_s2_leftFire) { load_s2_valid_vec := 0x3f.U(6.W)}
  when (load_s1.io.out.bits.uop.robIdx.needFlush(io.redirect)) { load_s2_valid_vec := 0x0.U(6.W) }
  assert(RegNext(load_s2.io.in.valid === load_s2_valid_vec(0)))
  io.lsq.loadIn.bits.lq_data_wen_dup := load_s2_valid_vec.asBools()

  // s2_dcache_require_replay signal will be RegNexted, then used in s3
  io.lsq.s2_dcache_require_replay := load_s2.io.s2_dcache_require_replay

  // write to rob and writeback bus
  val s2_wb_valid = load_s2.io.out.valid && !load_s2.io.out.bits.miss && !load_s2.io.out.bits.mmio

  // Int load, if hit, will be writebacked at s2
  val hitLoadOut = Wire(Valid(new ExuOutput))
  hitLoadOut.valid := s2_wb_valid
  hitLoadOut.bits.uop := load_s2.io.out.bits.uop
  hitLoadOut.bits.data := load_s2.io.out.bits.data
  hitLoadOut.bits.redirectValid := false.B
  hitLoadOut.bits.redirect := DontCare
  hitLoadOut.bits.debug.isMMIO := load_s2.io.out.bits.mmio
  hitLoadOut.bits.debug.isPerfCnt := false.B
  hitLoadOut.bits.debug.paddr := load_s2.io.out.bits.paddr
  hitLoadOut.bits.debug.vaddr := load_s2.io.out.bits.vaddr
  hitLoadOut.bits.fflags := DontCare

  load_s2.io.out.ready := true.B

  // load s3
  val s3_load_wb_meta_reg = RegNext(Mux(hitLoadOut.valid, hitLoadOut.bits, io.lsq.ldout.bits))

  // data from load queue refill
  val s3_loadDataFromLQ = RegEnable(io.lsq.ldRawData, io.lsq.ldout.valid)
  val s3_rdataLQ = s3_loadDataFromLQ.mergedData()
  val s3_rdataSelLQ = LookupTree(s3_loadDataFromLQ.addrOffset, List(
    "b000".U -> s3_rdataLQ(63, 0),
    "b001".U -> s3_rdataLQ(63, 8),
    "b010".U -> s3_rdataLQ(63, 16),
    "b011".U -> s3_rdataLQ(63, 24),
    "b100".U -> s3_rdataLQ(63, 32),
    "b101".U -> s3_rdataLQ(63, 40),
    "b110".U -> s3_rdataLQ(63, 48),
    "b111".U -> s3_rdataLQ(63, 56)
  ))
  val s3_rdataPartialLoadLQ = rdataHelper(s3_loadDataFromLQ.uop, s3_rdataSelLQ)

  // data from dcache hit
  val s3_loadDataFromDcache = RegEnable(load_s2.io.loadDataFromDcache, load_s2.io.in.valid)
  val s3_rdataDcache = s3_loadDataFromDcache.mergedData()
  val s3_rdataSelDcache = LookupTree(s3_loadDataFromDcache.addrOffset, List(
    "b000".U -> s3_rdataDcache(63, 0),
    "b001".U -> s3_rdataDcache(63, 8),
    "b010".U -> s3_rdataDcache(63, 16),
    "b011".U -> s3_rdataDcache(63, 24),
    "b100".U -> s3_rdataDcache(63, 32),
    "b101".U -> s3_rdataDcache(63, 40),
    "b110".U -> s3_rdataDcache(63, 48),
    "b111".U -> s3_rdataDcache(63, 56)
  ))
  val s3_rdataPartialLoadDcache = rdataHelper(s3_loadDataFromDcache.uop, s3_rdataSelDcache)

  io.ldout.bits := s3_load_wb_meta_reg
  io.ldout.bits.data := Mux(RegNext(hitLoadOut.valid), s3_rdataPartialLoadDcache, s3_rdataPartialLoadLQ)
  io.ldout.valid := RegNext(hitLoadOut.valid) && !RegNext(load_s2.io.out.bits.uop.robIdx.needFlush(io.redirect)) ||
    RegNext(io.lsq.ldout.valid) && !RegNext(io.lsq.ldout.bits.uop.robIdx.needFlush(io.redirect)) && !RegNext(hitLoadOut.valid)

  io.ldout.bits.uop.cf.exceptionVec(loadAccessFault) := s3_load_wb_meta_reg.uop.cf.exceptionVec(loadAccessFault) ||
    RegNext(hitLoadOut.valid) && load_s2.io.s3_delayed_load_error

  // feedback tlb miss / dcache miss queue full
  io.feedbackSlow.bits := RegNext(load_s2.io.rsFeedback.bits)
  io.feedbackSlow.valid := RegNext(load_s2.io.rsFeedback.valid && !load_s2.io.out.bits.uop.robIdx.needFlush(io.redirect))
  // If replay is reported at load_s1, inst will be canceled (will not enter load_s2),
  // in that case:
  // * replay should not be reported twice
  assert(!(RegNext(io.feedbackFast.valid) && io.feedbackSlow.valid))
  // * io.fastUop.valid should not be reported
  assert(!RegNext(io.feedbackFast.valid && io.fastUop.valid))

  // load forward_fail/ldld_violation check
  // check for inst in load pipeline
  val s3_forward_fail = RegNext(io.lsq.forward.matchInvalid || io.sbuffer.matchInvalid)
  val s3_ldld_violation = RegNext(
    io.lsq.loadViolationQuery.resp.valid &&
    io.lsq.loadViolationQuery.resp.bits.have_violation &&
    RegNext(io.csrCtrl.ldld_vio_check_enable)
  )
  val s3_need_replay_from_fetch = s3_forward_fail || s3_ldld_violation
  val s3_can_replay_from_fetch = RegEnable(load_s2.io.s2_can_replay_from_fetch, load_s2.io.out.valid)
  // 1) use load pipe check result generated in load_s3 iff load_hit
  when (RegNext(hitLoadOut.valid)) {
    io.ldout.bits.uop.ctrl.replayInst := s3_need_replay_from_fetch
  }
  // 2) otherwise, write check result to load queue
  io.lsq.s3_replay_from_fetch := s3_need_replay_from_fetch && s3_can_replay_from_fetch

  // s3_delayed_load_error path is not used for now, as we writeback load result in load_s3
  // but we keep this path for future use
  io.s3_delayed_load_error := false.B
  io.lsq.s3_delayed_load_error := false.B //load_s2.io.s3_delayed_load_error

  io.lsq.ldout.ready := !hitLoadOut.valid

  when(io.feedbackSlow.valid && !io.feedbackSlow.bits.hit){
    assert(RegNext(!hitLoadOut.valid))
    assert(RegNext(!io.lsq.loadIn.valid) || RegNext(load_s2.io.s2_dcache_require_replay))
  }

  val lastValidData = RegEnable(io.ldout.bits.data, io.ldout.fire)
  val hitLoadAddrTriggerHitVec = Wire(Vec(TriggerNum, Bool()))
  val lqLoadAddrTriggerHitVec = io.lsq.trigger.lqLoadAddrTriggerHitVec
  (0 until TriggerNum).map{i => {
    val tdata2 = io.trigger(i).tdata2
    val matchType = io.trigger(i).matchType
    val tEnable = io.trigger(i).tEnable

    hitLoadAddrTriggerHitVec(i) := TriggerCmp(load_s2.io.out.bits.vaddr, tdata2, matchType, tEnable)
    io.trigger(i).addrHit := RegNext(Mux(hitLoadOut.valid, hitLoadAddrTriggerHitVec(i), lqLoadAddrTriggerHitVec(i)))
    io.trigger(i).lastDataHit := TriggerCmp(lastValidData, tdata2, matchType, tEnable)
  }}
  io.lsq.trigger.hitLoadAddrTriggerHitVec := hitLoadAddrTriggerHitVec

  val perfEvents = Seq(
    ("load_s0_in_fire         ", load_s0.io.in.fire                                                                                                              ),
    ("load_to_load_forward    ", load_s1.io.out.valid && s1_tryPointerChasing && !cancelPointerChasing                                                           ),
    ("stall_dcache            ", load_s0.io.out.valid && load_s0.io.out.ready && !load_s0.io.dcacheReq.ready                                                     ),
    ("load_s1_in_fire         ", load_s1.io.in.fire                                                                                                              ),
    ("load_s1_tlb_miss        ", load_s1.io.in.fire && load_s1.io.dtlbResp.bits.miss                                                                             ),
    ("load_s2_in_fire         ", load_s2.io.in.fire                                                                                                              ),
    ("load_s2_dcache_miss     ", load_s2.io.in.fire && load_s2.io.dcacheResp.bits.miss                                                                           ),
    ("load_s2_replay          ", load_s2.io.rsFeedback.valid && !load_s2.io.rsFeedback.bits.hit                                                                  ),
    ("load_s2_replay_tlb_miss ", load_s2.io.rsFeedback.valid && !load_s2.io.rsFeedback.bits.hit && load_s2.io.in.bits.tlbMiss                                    ),
    ("load_s2_replay_cache    ", load_s2.io.rsFeedback.valid && !load_s2.io.rsFeedback.bits.hit && !load_s2.io.in.bits.tlbMiss && load_s2.io.dcacheResp.bits.miss),
  )
  generatePerfEvent()

  when(io.ldout.fire){
    XSDebug("ldout %x\n", io.ldout.bits.uop.cf.pc)
  }
}
