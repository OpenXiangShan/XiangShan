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
import xiangshan.backend.fu.PMPRespBundle
import xiangshan.backend.fu.FuType
import xiangshan.backend.Bundles.{MemExuInput, MemExuOutput}
import xiangshan.backend.fu.NewCSR.TriggerUtil
import xiangshan.backend.fu.util.SdtrigExt
import xiangshan.mem.Bundles._
import xiangshan.cache.mmu.Pbmt
import xiangshan.cache.{AtomicWordIO, HasDCacheParameters, MemoryOpConstants}
import xiangshan.cache.mmu.{TlbCmd, TlbRequestIO}
import difftest._

class AtomicsUnit(implicit p: Parameters) extends XSModule
  with MemoryOpConstants
  with HasDCacheParameters
  with SdtrigExt{

  val StdCnt  = backendParams.StdCnt

  val io = IO(new Bundle() {
    val hartId        = Input(UInt(hartIdLen.W))
    val in            = Flipped(Decoupled(new MemExuInput))
    val storeDataIn   = Flipped(Vec(StdCnt, Valid(new MemExuOutput)))
    val out           = Decoupled(new MemExuOutput)
    val dcache        = new AtomicWordIO
    val dtlb          = new TlbRequestIO(2)
    val pmpResp       = Flipped(new PMPRespBundle())
    val flush_sbuffer = new SbufferFlushBundle
    val feedbackSlow  = ValidIO(new RSFeedback)
    val redirect      = Flipped(ValidIO(new Redirect))
    val exceptionInfo = ValidIO(new Bundle {
      val vaddr = UInt(XLEN.W)
      val gpaddr = UInt(XLEN.W)
      val isForVSnonLeafPTE = Bool()
    })
    val csrCtrl       = Flipped(new CustomCSRCtrlIO)
  })

  //-------------------------------------------------------
  // Atomics Memory Accsess FSM
  //-------------------------------------------------------
  val s_invalid :: s_tlb_and_flush_sbuffer_req :: s_pm :: s_wait_flush_sbuffer_resp :: s_cache_req :: s_cache_resp :: s_cache_resp_latch :: s_finish :: s_finish2 :: Nil = Enum(9)
  val state = RegInit(s_invalid)
  val out_valid = RegInit(false.B)
  val data_valid = RegInit(false.B)

  val uop = Reg(io.in.bits.uop.cloneType)
  val isLr = LSUOpType.isLr(uop.fuOpType)
  val isSc = LSUOpType.isSc(uop.fuOpType)
  val isAMOCAS = LSUOpType.isAMOCAS(uop.fuOpType)
  val isNotLr = !isLr
  val isNotSc = !isSc
  // AMOCAS.Q needs to write two int registers, therefore backend issues two sta uops for AMOCAS.Q.
  // `pdest2` is used to record the pdest of the second uop
  val pdest1, pdest2 = Reg(UInt(PhyRegIdxWidth.W))
  val pdest1Valid, pdest2Valid = RegInit(false.B)
  /**
    * The # of std uops that an atomic instruction require:
    * (1) For AMOs (except AMOCAS) and LR/SC, 1 std uop is wanted: X(rs2) with uopIdx = 0
    * (2) For AMOCAS.W/D, 2 std uops are wanted: X(rd), X(rs2) with uopIdx = 0, 1
    * (3) For AMOCAS.Q, 4 std uops are wanted: X(rd), X(rs2), X(rd+1), X(rs2+1) with uopIdx = 0, 1, 2, 3
    * stds are not needed for write-back.
    *
    * The # of sta uops that an atomic instruction require, also the # of write-back:
    * (1) For AMOs(except AMOCAS.Q) and LR/SC, 1 sta uop is wanted: X(rs1) with uopIdx = 0
    * (2) For AMOCAS.Q, 2 sta uop is wanted: X(rs1)*2 with uopIdx = 0, 2
    */
  val rs1, rs2_l, rs2_h, rd_l, rd_h = Reg(UInt(XLEN.W))
  val stds = Seq(rd_l, rs2_l, rd_h, rs2_h)
  val rs2 = Cat(rs2_h, Mux(isAMOCAS, rs2_l, stds.head))
  val rd = Cat(rd_h, rd_l)
  val stdCnt = RegInit(0.U(log2Ceil(stds.length + 1).W))

  val exceptionVec = RegInit(0.U.asTypeOf(ExceptionVec()))
  val trigger = RegInit(TriggerAction.None)
  val atom_override_xtval = RegInit(false.B)
  val have_sent_first_tlb_req = RegInit(false.B)
  // paddr after translation
  val paddr = Reg(UInt())
  val gpaddr = Reg(UInt())
  val vaddr = rs1

  val is_mmio = Reg(Bool())
  val isForVSnonLeafPTE = Reg(Bool())

  // dcache response data
  val resp_data = Reg(UInt())
  val resp_data_wire = WireInit(0.U)
  val success = Reg(Bool())
  // sbuffer is empty or not
  val sbuffer_empty = io.flush_sbuffer.empty

  // Only the least significant AMOFuOpWidth = 6 bits of fuOpType are used,
  // therefore the MSBs are reused to identify uopIdx
  val stdUopIdxs = io.storeDataIn.map(_.bits.uop.fuOpType >> LSUOpType.AMOFuOpWidth)
  val staUopIdx = io.in.bits.uop.fuOpType >> LSUOpType.AMOFuOpWidth

  // assign default value to output signals
  io.in.ready          := false.B

  io.dcache.req.valid  := false.B
  io.dcache.req.bits   := DontCare

  io.dtlb.req.valid    := false.B
  io.dtlb.req.bits     := DontCare
  io.dtlb.req_kill     := false.B
  io.dtlb.resp.ready   := true.B

  io.flush_sbuffer.valid := false.B

  when (state === s_invalid) {
    when (io.in.fire) {
      uop := io.in.bits.uop
      rs1 := io.in.bits.src_rs1
      state := s_tlb_and_flush_sbuffer_req
      have_sent_first_tlb_req := false.B
    }
  }

  when (io.in.fire) {
    val pdest = io.in.bits.uop.pdest
    when (staUopIdx === 0.U) {
      pdest1Valid := true.B
      pdest1 := pdest
    }.elsewhen (staUopIdx === 2.U) {
      pdest2Valid := true.B
      pdest2 := pdest
    }.otherwise {
      assert(false.B, "unrecognized sta uopIdx")
    }
  }

  stds.zipWithIndex.foreach { case (data, i) =>
    val sels = io.storeDataIn.zip(stdUopIdxs).map { case (in, uopIdx) =>
      val sel = in.fire && uopIdx === i.U
      when (sel) { data := in.bits.data }
      sel
    }
    OneHot.checkOneHot(sels)
  }
  stdCnt := stdCnt + PopCount(io.storeDataIn.map(_.fire))

  val StdCntNCAS = 1 // LR/SC and AMO need only 1 src besides rs1
  val StdCntCASWD = 2 // AMOCAS.W/D needs 2 src regs (rs2 and rd) besides rs1
  val StdCntCASQ = 4 // AMOCAS.Q needs 4 src regs (rs2, rs2+1, rd, rd+1) besides rs1
  when (!data_valid) {
    data_valid := state =/= s_invalid && (
      LSUOpType.isAMOCASQ(uop.fuOpType) && stdCnt === StdCntCASQ.U ||
      LSUOpType.isAMOCASWD(uop.fuOpType) && stdCnt === StdCntCASWD.U ||
      !isAMOCAS && stdCnt === StdCntNCAS.U
    )
  }
  assert(stdCnt <= stds.length.U, "unexpected std")
  assert(!(Cat(io.storeDataIn.map(_.fire)).orR && data_valid), "atomic unit re-receive data")

  // atomic trigger
  val csrCtrl = io.csrCtrl
  val tdata = Reg(Vec(TriggerNum, new MatchTriggerIO))
  val tEnableVec = RegInit(VecInit(Seq.fill(TriggerNum)(false.B)))
  tEnableVec := csrCtrl.mem_trigger.tEnableVec
  when (csrCtrl.mem_trigger.tUpdate.valid) {
    tdata(csrCtrl.mem_trigger.tUpdate.bits.addr) := csrCtrl.mem_trigger.tUpdate.bits.tdata
  }

  val debugMode = csrCtrl.mem_trigger.debugMode
  val triggerCanRaiseBpExp = csrCtrl.mem_trigger.triggerCanRaiseBpExp
  val backendTriggerTimingVec = VecInit(tdata.map(_.timing))
  val backendTriggerChainVec = VecInit(tdata.map(_.chain))
  val backendTriggerHitVec = WireInit(VecInit(Seq.fill(TriggerNum)(false.B)))
  val backendTriggerCanFireVec = RegInit(VecInit(Seq.fill(TriggerNum)(false.B)))

  assert(state === s_invalid ||
    uop.fuOpType(1,0) === "b10".U ||
    uop.fuOpType(1,0) === "b11".U ||
    LSUOpType.isAMOCASQ(uop.fuOpType),
    "Only word or doubleword or quadword is supported"
  )

  // store trigger
  val store_hit = Wire(Vec(TriggerNum, Bool()))
  for (j <- 0 until TriggerNum) {
    store_hit(j) := !tdata(j).select && !debugMode && isNotLr && TriggerCmp(
      vaddr,
      tdata(j).tdata2,
      tdata(j).matchType,
      tEnableVec(j) && tdata(j).store
    )
  }
  // load trigger
  val load_hit = Wire(Vec(TriggerNum, Bool()))
  for (j <- 0 until TriggerNum) {
    load_hit(j) := !tdata(j).select && !debugMode && isNotSc && TriggerCmp(
      vaddr,
      tdata(j).tdata2,
      tdata(j).matchType,
      tEnableVec(j) && tdata(j).load
    )
  }
  backendTriggerHitVec := store_hit.zip(load_hit).map { case (sh, lh) => sh || lh }
  // triggerCanFireVec will update at T+1
  TriggerCheckCanFire(TriggerNum, backendTriggerCanFireVec, backendTriggerHitVec,
    backendTriggerTimingVec, backendTriggerChainVec)

  val actionVec = VecInit(tdata.map(_.action))
  val triggerAction = Wire(TriggerAction())
  TriggerUtil.triggerActionGen(triggerAction, backendTriggerCanFireVec, actionVec, triggerCanRaiseBpExp)
  val triggerDebugMode = TriggerAction.isDmode(triggerAction)
  val triggerBreakpoint = TriggerAction.isExp(triggerAction)

  // tlb translation, manipulating signals && deal with exception
  // at the same time, flush sbuffer
  when (state === s_tlb_and_flush_sbuffer_req) {
    // do not accept tlb resp in the first cycle
    // this limition is for hw prefetcher
    // when !have_sent_first_tlb_req, tlb resp may come from hw prefetch
    have_sent_first_tlb_req := true.B

    when (io.dtlb.resp.fire && have_sent_first_tlb_req) {
      paddr   := io.dtlb.resp.bits.paddr(0)
      gpaddr  := io.dtlb.resp.bits.gpaddr(0)
      vaddr   := io.dtlb.resp.bits.fullva
      isForVSnonLeafPTE := io.dtlb.resp.bits.isForVSnonLeafPTE
      // exception handling
      val addrAligned = LookupTree(uop.fuOpType(1,0), List(
        "b10".U -> (vaddr(1,0) === 0.U), // W
        "b11".U -> (vaddr(2,0) === 0.U), // D
        "b00".U -> (vaddr(3,0) === 0.U)  // Q
      ))
      exceptionVec(loadAddrMisaligned)  := !addrAligned && isLr
      exceptionVec(storeAddrMisaligned) := !addrAligned && !isLr
      exceptionVec(storePageFault)      := io.dtlb.resp.bits.excp(0).pf.st
      exceptionVec(loadPageFault)       := io.dtlb.resp.bits.excp(0).pf.ld
      exceptionVec(storeAccessFault)    := io.dtlb.resp.bits.excp(0).af.st
      exceptionVec(loadAccessFault)     := io.dtlb.resp.bits.excp(0).af.ld
      exceptionVec(storeGuestPageFault) := io.dtlb.resp.bits.excp(0).gpf.st
      exceptionVec(loadGuestPageFault)  := io.dtlb.resp.bits.excp(0).gpf.ld

      exceptionVec(breakPoint) := triggerBreakpoint
      trigger                  := triggerAction

      when (!io.dtlb.resp.bits.miss) {
        io.out.bits.uop.debugInfo.tlbRespTime := GTimer()
        when (!addrAligned || triggerDebugMode || triggerBreakpoint) {
          // NOTE: when addrAligned or trigger fire, do not need to wait tlb actually
          // check for miss aligned exceptions, tlb exception are checked next cycle for timing
          // if there are exceptions, no need to execute it
          state := s_finish
          out_valid := true.B
          atom_override_xtval := true.B
        }.otherwise {
          state := s_pm
        }
      }
    }
  }

  val pbmtReg = RegEnable(io.dtlb.resp.bits.pbmt(0), io.dtlb.resp.fire && !io.dtlb.resp.bits.miss)
  when (state === s_pm) {
    val pmp = WireInit(io.pmpResp)
    is_mmio := Pbmt.isIO(pbmtReg) || (Pbmt.isPMA(pbmtReg) && pmp.mmio)

    // NOTE: only handle load/store exception here, if other exception happens, don't send here
    val exception_va = exceptionVec(storePageFault) || exceptionVec(loadPageFault) ||
      exceptionVec(storeGuestPageFault) || exceptionVec(loadGuestPageFault) ||
      exceptionVec(storeAccessFault) || exceptionVec(loadAccessFault)
    val exception_pa_mmio_nc = pmp.mmio || Pbmt.isIO(pbmtReg) || Pbmt.isNC(pbmtReg)
    val exception_pa = pmp.st || pmp.ld || exception_pa_mmio_nc
    when (exception_va || exception_pa) {
      state := s_finish
      out_valid := true.B
      atom_override_xtval := true.B
    }.otherwise {
      // if sbuffer has been flushed, go to query dcache, otherwise wait for sbuffer.
      state := Mux(sbuffer_empty, s_cache_req, s_wait_flush_sbuffer_resp);
    }
    // update storeAccessFault bit
    exceptionVec(loadAccessFault) := exceptionVec(loadAccessFault) ||
      (pmp.ld || exception_pa_mmio_nc) && isLr
    exceptionVec(storeAccessFault) := exceptionVec(storeAccessFault) || pmp.st ||
      (pmp.ld || exception_pa_mmio_nc) && !isLr
  }

  when (state === s_wait_flush_sbuffer_resp) {
    when (sbuffer_empty) {
      state := s_cache_req
    }
  }

  def genWdataAMO(data: UInt, sizeEncode: UInt): UInt = {
    LookupTree(sizeEncode(1, 0), List(
      "b10".U -> Fill(4, data(31, 0)),
      "b11".U -> Fill(2, data(63, 0)),
      "b00".U -> data(127, 0)
    ))
  }

  def genWmaskAMO(addr: UInt, sizeEncode: UInt): UInt = {
    /**
      * `MainPipeReq` uses `word_idx` to recognize which 64-bits data bank to operate on. Double-word atomics are
      * always 8B aligned and quad-word atomics are always 16B aligned except for misaligned exception, therefore
      * `word_idx` is enough and there is no need to shift according address. Only word atomics needs LSBs of the
      * address to shift mask inside a 64-bits aligned range.
      */
    LookupTree(sizeEncode(1, 0), List(
      "b10".U -> (0xf.U << addr(2,0)), // W
      "b11".U -> 0xff.U, // D
      "b00".U -> 0xffff.U // Q
    ))
  }

  when (state === s_cache_req) {
    when (io.dcache.req.fire) {
      state := s_cache_resp
    }
  }

  val dcache_resp_data  = Reg(UInt())
  val dcache_resp_id    = Reg(UInt())
  val dcache_resp_error = Reg(Bool())

  when (state === s_cache_resp) {
    // when not miss
    // everything is OK, simply send response back to sbuffer
    // when miss and not replay
    // wait for missQueue to handling miss and replaying our request
    // when miss and replay
    // req missed and fail to enter missQueue, manually replay it later
    // TODO: add assertions:
    // 1. add a replay delay counter?
    // 2. when req gets into MissQueue, it should not miss any more
    when (io.dcache.resp.fire) {
      when (io.dcache.resp.bits.miss) {
        when (io.dcache.resp.bits.replay) {
          state := s_cache_req
        }
      }.otherwise {
        dcache_resp_data := io.dcache.resp.bits.data
        dcache_resp_id := io.dcache.resp.bits.id
        dcache_resp_error := io.dcache.resp.bits.error
        state := s_cache_resp_latch
      }
    }
  }

  when (state === s_cache_resp_latch) {
    success := dcache_resp_id
    val rdataSel = Mux(
      paddr(2, 0) === 0.U,
      dcache_resp_data,
      dcache_resp_data >> 32
    )
    assert(paddr(2, 0) === "b000".U || paddr(2, 0) === "b100".U)

    resp_data_wire := Mux(
      isSc,
      dcache_resp_data,
      LookupTree(uop.fuOpType(1,0), List(
        "b10".U -> SignExt(rdataSel(31, 0), QuadWordBits), // W
        "b11".U -> SignExt(rdataSel(63, 0), QuadWordBits), // D
        "b00".U -> rdataSel // Q
      ))
    )

    when (dcache_resp_error && io.csrCtrl.cache_error_enable) {
      exceptionVec(loadAccessFault)  := isLr
      exceptionVec(storeAccessFault) := !isLr
      assert(!exceptionVec(loadAccessFault))
      assert(!exceptionVec(storeAccessFault))
    }

    resp_data := resp_data_wire
    state := s_finish
    out_valid := true.B
  }

  when (state === s_finish) {
    when (io.out.fire) {
      when (LSUOpType.isAMOCASQ(uop.fuOpType)) {
        // enter `s_finish2` to write the 2nd uop back
        state := s_finish2
        out_valid := true.B
      }.otherwise {
        // otherwise the FSM ends here
        resetFSM()
      }
    }
  }

  when (state === s_finish2) {
    when (io.out.fire) {
      resetFSM()
    }
  }

  when (io.redirect.valid) {
    atom_override_xtval := false.B
  }

  def resetFSM(): Unit = {
    state := s_invalid
    out_valid := false.B
    data_valid := false.B
    stdCnt := 0.U
    pdest1Valid := false.B
    pdest2Valid := false.B
  }

  /**
    * IO assignment
    */
  io.exceptionInfo.valid := atom_override_xtval
  io.exceptionInfo.bits.vaddr := vaddr
  io.exceptionInfo.bits.gpaddr := gpaddr
  io.exceptionInfo.bits.isForVSnonLeafPTE := isForVSnonLeafPTE

  // Send TLB feedback to store issue queue
  // we send feedback right after we receives request
  // also, we always treat amo as tlb hit
  // since we will continue polling tlb all by ourself
  io.feedbackSlow.valid       := GatedValidRegNext(GatedValidRegNext(io.in.valid))
  io.feedbackSlow.bits.hit    := true.B
  io.feedbackSlow.bits.robIdx  := RegEnable(io.in.bits.uop.robIdx, io.in.valid)
  io.feedbackSlow.bits.sqIdx   := RegEnable(io.in.bits.uop.sqIdx, io.in.valid)
  io.feedbackSlow.bits.lqIdx   := RegEnable(io.in.bits.uop.lqIdx, io.in.valid)
  io.feedbackSlow.bits.flushState := DontCare
  io.feedbackSlow.bits.sourceType := DontCare
  io.feedbackSlow.bits.dataInvalidSqIdx := DontCare

  // send req to dtlb
  // keep firing until tlb hit
  io.dtlb.req.valid       := state === s_tlb_and_flush_sbuffer_req
  io.dtlb.req.bits.vaddr  := vaddr
  io.dtlb.req.bits.fullva := vaddr
  io.dtlb.req.bits.checkfullva := true.B
  io.dtlb.resp.ready      := true.B
  io.dtlb.req.bits.cmd    := Mux(isLr, TlbCmd.atom_read, TlbCmd.atom_write)
  io.dtlb.req.bits.debug.pc := uop.pc
  io.dtlb.req.bits.debug.robIdx := uop.robIdx
  io.dtlb.req.bits.debug.isFirstIssue := false.B
  io.out.bits.uop.debugInfo.tlbFirstReqTime := GTimer() // FIXME lyq: it will be always assigned

  // send req to sbuffer to flush it if it is not empty
  io.flush_sbuffer.valid := !sbuffer_empty && (
    state === s_tlb_and_flush_sbuffer_req ||
    state === s_pm ||
    state === s_wait_flush_sbuffer_resp
  )

  // When is sta issue port ready:
  // (1) AtomicsUnit is idle, or
  // (2) For AMOCAS.Q, the second uop with the pdest of the higher bits of rd is not received yet
  io.in.ready := state === s_invalid || LSUOpType.isAMOCASQ(uop.fuOpType) && (!pdest2Valid || !pdest1Valid)

  io.out.valid := out_valid && Mux(state === s_finish2, pdest2Valid, pdest1Valid)
  XSError((state === s_finish || state === s_finish2) =/= out_valid, "out_valid reg error\n")
  io.out.bits := DontCare
  io.out.bits.uop := uop
  io.out.bits.uop.fuType := FuType.mou.U
  io.out.bits.uop.pdest := Mux(state === s_finish2, pdest2, pdest1)
  io.out.bits.uop.exceptionVec := exceptionVec
  io.out.bits.uop.trigger := trigger
  io.out.bits.data := Mux(state === s_finish2, resp_data >> XLEN, resp_data)
  io.out.bits.debug.isMMIO := is_mmio
  io.out.bits.debug.paddr := paddr

  io.dcache.req.valid := Mux(
    io.dcache.req.bits.cmd === M_XLR,
    !io.dcache.block_lr, // block lr to survive in lr storm
    data_valid // wait until src(1) is ready
  ) && state === s_cache_req
  val pipe_req = io.dcache.req.bits
  pipe_req := DontCare
  pipe_req.cmd := LookupTree(uop.fuOpType, List(
    // TODO: optimize this
    LSUOpType.lr_w      -> M_XLR,
    LSUOpType.sc_w      -> M_XSC,
    LSUOpType.amoswap_w -> M_XA_SWAP,
    LSUOpType.amoadd_w  -> M_XA_ADD,
    LSUOpType.amoxor_w  -> M_XA_XOR,
    LSUOpType.amoand_w  -> M_XA_AND,
    LSUOpType.amoor_w   -> M_XA_OR,
    LSUOpType.amomin_w  -> M_XA_MIN,
    LSUOpType.amomax_w  -> M_XA_MAX,
    LSUOpType.amominu_w -> M_XA_MINU,
    LSUOpType.amomaxu_w -> M_XA_MAXU,
    LSUOpType.amocas_w  -> M_XA_CASW,

    LSUOpType.lr_d      -> M_XLR,
    LSUOpType.sc_d      -> M_XSC,
    LSUOpType.amoswap_d -> M_XA_SWAP,
    LSUOpType.amoadd_d  -> M_XA_ADD,
    LSUOpType.amoxor_d  -> M_XA_XOR,
    LSUOpType.amoand_d  -> M_XA_AND,
    LSUOpType.amoor_d   -> M_XA_OR,
    LSUOpType.amomin_d  -> M_XA_MIN,
    LSUOpType.amomax_d  -> M_XA_MAX,
    LSUOpType.amominu_d -> M_XA_MINU,
    LSUOpType.amomaxu_d -> M_XA_MAXU,
    LSUOpType.amocas_d  -> M_XA_CASD,

    LSUOpType.amocas_q  -> M_XA_CASQ
  ))
  pipe_req.miss := false.B
  pipe_req.probe := false.B
  pipe_req.probe_need_data := false.B
  pipe_req.source := AMO_SOURCE.U
  pipe_req.addr   := get_block_addr(paddr)
  pipe_req.vaddr  := get_block_addr(vaddr)
  pipe_req.word_idx  := get_word(paddr)
  pipe_req.amo_data := genWdataAMO(rs2, uop.fuOpType)
  pipe_req.amo_mask := genWmaskAMO(paddr, uop.fuOpType)
  pipe_req.amo_cmp  := genWdataAMO(rd, uop.fuOpType)

  if (env.EnableDifftest) {
    val difftest = DifftestModule(new DiffAtomicEvent)
    val en = io.dcache.req.fire
    difftest.coreid := io.hartId
    difftest.valid  := state === s_cache_resp_latch
    difftest.addr   := RegEnable(paddr, en)
    difftest.data   := RegEnable(io.dcache.req.bits.amo_data.asTypeOf(difftest.data), en)
    difftest.mask   := RegEnable(io.dcache.req.bits.amo_mask, en)
    difftest.cmp    := RegEnable(io.dcache.req.bits.amo_cmp.asTypeOf(difftest.cmp), en)
    difftest.fuop   := RegEnable(uop.fuOpType, en)
    difftest.out    := resp_data_wire.asTypeOf(difftest.out)
  }

  if (env.EnableDifftest || env.AlwaysBasicDiff) {
    val uop = io.out.bits.uop
    val difftest = DifftestModule(new DiffLrScEvent)
    difftest.coreid := io.hartId
    difftest.valid := io.out.fire && state === s_finish && isSc
    difftest.success := success
  }
}
