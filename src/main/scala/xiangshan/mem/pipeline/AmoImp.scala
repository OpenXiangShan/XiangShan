/***************************************************************************************
* Copyright (c) 2025 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2025 Institute of Computing Technology, Chinese Academy of Sciences
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
import xiangshan.backend.Bundles.{MemExuInput, MemExuOutput, DynInst}
import xiangshan.backend.fu.NewCSR.TriggerUtil
import xiangshan.backend.fu.util.SdtrigExt
import xiangshan.mem.Bundles._
import xiangshan.cache.mmu.Pbmt
import xiangshan.cache.{AtomicWordIO, HasDCacheParameters, MemoryOpConstants}
import xiangshan.cache.{MainPipeReq, MainPipeResp}
import xiangshan.cache.mmu.{TlbReq, TlbResp, TlbCmd, TlbRequestIO}
import difftest._

class AmoIO()(implicit p: Parameters, params: MemUnitParams) extends MemUnitIO with HasMemBlockParameters {
  // from
  val fromTlb = new Bundle() {
    val resp = Flipped(DecoupledIO(new TlbResp(2)))
  }
  val fromDCache = new Bundle  {
    val resp = Flipped(ValidIO(new MainPipeResp))
    val blockLR = Input(Bool())
  }
  val fromPmp = new Bundle() {
    val resp = Flipped(new PMPRespBundle())
  }
  val fromBackend = new Bundle() {
    val issue = Flipped(DecoupledIO(new LsPipelineBundle))
    val storeDataIn = Flipped(Vec(StdCnt, Valid(new LsPipelineBundle)))
  }

  // to
  val toTlb = new Bundle() {
    val req = DecoupledIO(new TlbReq)
    val req_kill = Output(Bool())
  }
  val toDCache = new Bundle() {
    val req = DecoupledIO(new MainPipeReq)
  }
  val toBackend = new Bundle() {
    val iqFeedback = new MemRSFeedbackIO
    val writeback = DecoupledIO(new LsPipelineBundle)
  }
  // global
  val flushSbuffer = new SbufferFlushBundle
  val exceptionInfo = ValidIO(new ExceptionAddrIO)
}

class AmoImp(override val wrapper: MemUnit)(implicit p: Parameters, params: MemUnitParams) extends MemUnitImp(wrapper)
  with MemoryOpConstants
  with HasDCacheParameters
  with SdtrigExt {

  private def genWdataAMO(data: UInt, sizeEncode: UInt): UInt = {
    LookupTree(sizeEncode(1, 0), List(
      "b10".U -> Fill(4, data(31, 0)),
      "b11".U -> Fill(2, data(63, 0)),
      "b00".U -> data(127, 0)
    ))
  }

  private def genWmaskAMO(addr: UInt, sizeEncode: UInt): UInt = {
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

  private val StdCntNCAS = 1 // LR/SC and AMO need only 1 src besides rs1
  private val StdCntCASWD = 2 // AMOCAS.W/D needs 2 src regs (rs2 and rd) besides rs1
  private val StdCntCASQ = 4 // AMOCAS.Q needs 4 src regs (rs2, rs2+1, rd, rd+1) besides rs1

  /**
    * --------------------------------------------------------------------
    * IOs
    * --------------------------------------------------------------------
    */
  io.suggestName("none")
  override lazy val io = IO(new AmoIO).suggestName("io")

  /**
    * --------------------------------------------------------------------
    * Performance Kits
    * --------------------------------------------------------------------
    */
  PerfCCT.updateInstPos(
    io.fromBackend.issue.bits.uop.debug_seqNum,
    PerfCCT.InstPos.AtFU.id.U,
    io.fromBackend.issue.valid,
    clock,
    reset
  )

  /**
    * --------------------------------------------------------------------
    * Atomics Memory Accsess FSM
    * --------------------------------------------------------------------
    */
  val s_invalid :: s_tlb_and_flush_sbuffer_req :: s_pm :: s_wait_flush_sbuffer_resp :: s_cache_req :: s_cache_resp :: s_cache_resp_latch :: s_finish :: s_finish2 :: Nil = Enum(9)
  val state = RegInit(s_invalid)
  val outValid = RegInit(false.B)
  val dataValid = RegInit(false.B)

  val uop = Reg(new DynInst)
  val isLr = LSUOpType.isLr(uop.fuOpType)
  val isSc = LSUOpType.isSc(uop.fuOpType)
  val isAMOCAS = LSUOpType.isAMOCAS(uop.fuOpType)
  val isNotLr = !isLr
  val isNotSc = !isSc

  /**
    * AMOCAS.Q needs to write two int registers, therefore backend issues two sta uops for AMOCAS.Q.
    * `pdest2` is used to record the pdest of the second uop
    */
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
  val atomOverrideXtval = RegInit(false.B)
  val haveSentFirstTlbReq = RegInit(false.B)

  // paddr after translation
  val paddr = Reg(UInt())
  val gpaddr = Reg(UInt())
  val vaddr = rs1

  val isMMIO = Reg(Bool())
  val isForVSnonLeafPTE = Reg(Bool())

  // dcache response data
  val respDataWire = WireInit(0.U)
  val respData = Reg(UInt())
  val success = Reg(Bool())

  // sbuffer is empty or not
  val sbufferEmpty = io.flushSbuffer.empty

  /**
    * Only the least significant AMOFuOpWidth = 6 bits of fuOpType are used,
    * therefore the MSBs are reused to identify uopIdx
    */
  val stdUopIdxs = io.fromBackend.storeDataIn.map(_.bits.uop.fuOpType >> LSUOpType.AMOFuOpWidth)
  val staUopIdx = io.fromBackend.issue.bits.uop.fuOpType >> LSUOpType.AMOFuOpWidth

  io.fromTlb.resp.ready   := true.B // FIXME: really need this?

  /**
    * -------------------------------------------------------------------
    * Atomic Trigger
    * -------------------------------------------------------------------
    */
  val csrCtrl = io.fromCtrl.csrCtrl
  val tdata = Reg(Vec(TriggerNum, new MatchTriggerIO))
  val tEnableVec = RegInit(VecInit(Seq.fill(TriggerNum)(false.B)))
  val triggerAction = Wire(TriggerAction())
  tEnableVec := csrCtrl.mem_trigger.tEnableVec
  when (csrCtrl.mem_trigger.tUpdate.valid) {
    tdata(csrCtrl.mem_trigger.tUpdate.bits.addr) := csrCtrl.mem_trigger.tUpdate.bits.tdata
  }

  val debugMode = csrCtrl.mem_trigger.debugMode
  val triggerCanRaiseBpExp = csrCtrl.mem_trigger.triggerCanRaiseBpExp
  val triggerTimingVec = VecInit(tdata.map(_.timing))
  val triggerChainVec = VecInit(tdata.map(_.chain))
  val triggerHitVec = WireInit(VecInit(Seq.fill(TriggerNum)(false.B)))
  val triggerCanFireVec = RegInit(VecInit(Seq.fill(TriggerNum)(false.B)))

  // store trigger
  val storeHit = Wire(Vec(TriggerNum, Bool()))
  for (j <- 0 until TriggerNum) {
    storeHit(j) := !tdata(j).select && !debugMode && isNotLr && TriggerCmp(
      vaddr,
      tdata(j).tdata2,
      tdata(j).matchType,
      tEnableVec(j) && tdata(j).store
    )
  }

  // load trigger
  val loadHit = Wire(Vec(TriggerNum, Bool()))
  for (j <- 0 until TriggerNum) {
    loadHit(j) := !tdata(j).select && !debugMode && isNotSc && TriggerCmp(
      vaddr,
      tdata(j).tdata2,
      tdata(j).matchType,
      tEnableVec(j) && tdata(j).load
    )
  }

  triggerHitVec := storeHit.zip(loadHit).map { case (sh, lh) => sh || lh }
  // triggerCanFireVec will update at T+1
  TriggerCheckCanFire(TriggerNum, triggerCanFireVec, triggerHitVec, triggerTimingVec, triggerChainVec)
  val actionVec = VecInit(tdata.map(_.action))
  TriggerUtil.triggerActionGen(triggerAction, triggerCanFireVec, actionVec, triggerCanRaiseBpExp)

  /**
    * -------------------------------------------------------------------
    * Atomics FSM
    * -------------------------------------------------------------------
    * When is sta issue port ready:
    *   (1) AtomicsUnit is idle, or
    *   (2) For AMOCAS.Q, the second uop with the pdest of the higher bits of rd is not received yet
    */
  io.fromBackend.issue.ready := state === s_invalid ||
    LSUOpType.isAMOCASQ(uop.fuOpType) && (!pdest2Valid || !pdest1Valid)

  //  Recieve the atomic uop from Backend
  when (state === s_invalid) {
    when (io.fromBackend.issue.fire) {
      uop := io.fromBackend.issue.bits.uop
      rs1 := io.fromBackend.issue.bits.src(0)
      state := s_tlb_and_flush_sbuffer_req
      haveSentFirstTlbReq := false.B
    }
  }

  assert(state === s_invalid ||
    uop.fuOpType(1,0) === "b10".U ||
    uop.fuOpType(1,0) === "b11".U ||
    LSUOpType.isAMOCASQ(uop.fuOpType),
    "Only word or doubleword or quadword is supported"
  )

  when (io.fromBackend.issue.fire) {
    val pdest = io.fromBackend.issue.bits.uop.pdest
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

  stds.zipWithIndex.foreach {
    case (data, i) =>
      val sels = io.fromBackend.storeDataIn.zip(stdUopIdxs).map {
        case (in, uopIdx) =>
          val sel = in.fire && uopIdx === i.U
          when (sel) { data := in.bits.data }
          sel
      }
      OneHot.checkOneHot(sels)
  }
  stdCnt := stdCnt + PopCount(io.fromBackend.storeDataIn.map(_.fire))

  when (!dataValid) {
    dataValid := state =/= s_invalid && (
      LSUOpType.isAMOCASQ(uop.fuOpType) && stdCnt === StdCntCASQ.U ||
      LSUOpType.isAMOCASWD(uop.fuOpType) && stdCnt === StdCntCASWD.U ||
      !isAMOCAS && stdCnt === StdCntNCAS.U
    )
  }
  assert(stdCnt <= stds.length.U, "unexpected std")
  assert(!(Cat(io.fromBackend.storeDataIn.map(_.fire)).orR && dataValid), "atomic unit re-receive data")

  /**
   * Tlb translation, manipulating signals && deal with exception
   * at the same time, flush sbuffer. Do not accept tlb resp in the first
   * cycle this limition is for hw prefetcher, when !haveSentFirstTlbReq,
   * tlb resp may come from hw prefetch.
   */
  when (state === s_tlb_and_flush_sbuffer_req) {
    haveSentFirstTlbReq := true.B

    when (io.fromTlb.resp.fire && haveSentFirstTlbReq) {
      paddr := io.fromTlb.resp.bits.paddr(0)
      gpaddr := io.fromTlb.resp.bits.gpaddr(0)
      vaddr := io.fromTlb.resp.bits.fullva
      isForVSnonLeafPTE := io.fromTlb.resp.bits.isForVSnonLeafPTE

      // exception handling
      val addrAligned = LookupTree(uop.fuOpType(1,0), List(
        "b10".U -> (vaddr(1,0) === 0.U), // W
        "b11".U -> (vaddr(2,0) === 0.U), // D
        "b00".U -> (vaddr(3,0) === 0.U)  // Q
      ))
      val triggerBreakpoint = TriggerAction.isExp(triggerAction)
      val triggerDebugMode = TriggerAction.isDmode(triggerAction)

      exceptionVec(loadAddrMisaligned) := !addrAligned && isLr
      exceptionVec(storeAddrMisaligned) := !addrAligned && !isLr
      exceptionVec(storePageFault) := io.fromTlb.resp.bits.excp(0).pf.st
      exceptionVec(loadPageFault) := io.fromTlb.resp.bits.excp(0).pf.ld
      exceptionVec(storeAccessFault) := io.fromTlb.resp.bits.excp(0).af.st
      exceptionVec(loadAccessFault) := io.fromTlb.resp.bits.excp(0).af.ld
      exceptionVec(storeGuestPageFault) := io.fromTlb.resp.bits.excp(0).gpf.st
      exceptionVec(loadGuestPageFault) := io.fromTlb.resp.bits.excp(0).gpf.ld
      exceptionVec(breakPoint) := triggerBreakpoint

      trigger := triggerAction

      when (!io.fromTlb.resp.bits.miss) {
        io.toBackend.writeback.bits.uop.debugInfo.tlbRespTime := GTimer()
        when (!addrAligned || triggerDebugMode || triggerBreakpoint) {
          /**
           * When addrAligned or trigger fire, do not need to wait tlb actually
           * check for miss aligned exceptions, tlb exception are checked next cycle for timing
           * if there are exceptions, no need to execute it.
           */
          state := s_finish
          outValid := true.B
          atomOverrideXtval := true.B
        }.otherwise {
          state := s_pm
        }
      }
    }
  }

  val pbmtReg = RegEnable(io.fromTlb.resp.bits.pbmt(0), io.fromTlb.resp.fire && !io.fromTlb.resp.bits.miss)
  when (state === s_pm) {
    val pmp = WireInit(io.fromPmp.resp)
    isMMIO := Pbmt.isIO(pbmtReg) || (Pbmt.isPMA(pbmtReg) && pmp.mmio)

    // Only handle load/store exception here, if other exception happens, don't send here
    val hasVAddrExcp = exceptionVec(storePageFault) ||
      exceptionVec(loadPageFault) ||
      exceptionVec(storeGuestPageFault) ||
      exceptionVec(loadGuestPageFault) ||
      exceptionVec(storeAccessFault) ||
      exceptionVec(loadAccessFault)

    val isMmioOrNcPAddr = pmp.mmio || Pbmt.isIO(pbmtReg) || Pbmt.isNC(pbmtReg)
    val hasPAddrExcp = pmp.st || pmp.ld || isMmioOrNcPAddr
    when (hasVAddrExcp || hasPAddrExcp) {
      state := s_finish
      outValid := true.B
      atomOverrideXtval := true.B
    }.otherwise {
      // If sbuffer has been flushed, go to query dcache, otherwise wait for sbuffer.
      state := Mux(sbufferEmpty, s_cache_req, s_wait_flush_sbuffer_resp);
    }

    // update loadAccessFault bit
    exceptionVec(loadAccessFault) := exceptionVec(loadAccessFault) ||
      (pmp.ld || isMmioOrNcPAddr) && isLr

    // update storeAccessFault bit
    exceptionVec(storeAccessFault) := exceptionVec(storeAccessFault) || pmp.st ||
      (pmp.ld || isMmioOrNcPAddr) && !isLr
  }

  when (state === s_wait_flush_sbuffer_resp) {
    when (sbufferEmpty) {
      state := s_cache_req
    }
  }

  when (state === s_cache_req) {
    when (io.toDCache.req.fire) {
      state := s_cache_resp
    }
  }

  val dcacheRespData = Reg(UInt())
  val dcacheRespId = Reg(UInt())
  val dcacheRespError = Reg(Bool())

  when (state === s_cache_resp) {
    when (io.fromDCache.resp.fire) {
      /**
        * (1) when DCache hit, everything is OK, simply send response back to sbuffer
        * (2) when DCache miss and no need replay, wait for missQueue to handling miss
        *     and replaying our request, req missed and fail to enter missQueue, manually
        *     replay it later.
        *
        * TODO: add assertions:
        * (1) add a replay delay counter?
        * (2) when req gets into MissQueue, it should not miss any more
        */
      when (io.fromDCache.resp.bits.miss) {
        when (io.fromDCache.resp.bits.replay) {
          state := s_cache_req
        }
      }.otherwise {
        dcacheRespData := io.fromDCache.resp.bits.data
        dcacheRespId := io.fromDCache.resp.bits.id
        dcacheRespError := io.fromDCache.resp.bits.error
        state := s_cache_resp_latch
      }
    }
  }

  when (state === s_cache_resp_latch) {
    success := dcacheRespId
    val rdataSel = Mux(
      paddr(2, 0) === 0.U,
      dcacheRespData,
      dcacheRespData >> 32
    )
    assert(paddr(2, 0) === "b000".U || paddr(2, 0) === "b100".U)

    respDataWire := Mux(
      isSc,
      dcacheRespData,
      LookupTree(uop.fuOpType(1,0), List(
        "b10".U -> SignExt(rdataSel(31, 0), QuadWordBits), // W
        "b11".U -> SignExt(rdataSel(63, 0), QuadWordBits), // D
        "b00".U -> rdataSel // Q
      ))
    )

    when (dcacheRespError && csrCtrl.cache_error_enable) {
      exceptionVec(loadAccessFault) := isLr
      exceptionVec(storeAccessFault) := !isLr
      assert(!exceptionVec(loadAccessFault))
      assert(!exceptionVec(storeAccessFault))
    }

    respData := respDataWire
    state := s_finish
    outValid := true.B
  }

  when (state === s_finish) {
    when (io.toBackend.writeback.fire) {
      when (LSUOpType.isAMOCASQ(uop.fuOpType)) {
        // enter `s_finish2` to write the 2nd uop back
        state := s_finish2
        outValid := true.B
      }.otherwise {
        // otherwise the FSM ends here
        state := s_invalid
        outValid := false.B
        dataValid := false.B
        stdCnt := 0.U
        pdest1Valid := false.B
        pdest2Valid := false.B
      }
    }
  }
  XSError((state === s_finish || state === s_finish2) =/= outValid, "outValid reg error\n")

  when (state === s_finish2) {
    when (io.toBackend.writeback.fire) {
      state := s_invalid
      outValid := false.B
      dataValid := false.B
      stdCnt := 0.U
      pdest1Valid := false.B
      pdest2Valid := false.B
    }
  }

  when (io.fromCtrl.redirect.valid) {
    atomOverrideXtval := false.B
  }

  /**
    * --------------------------------------------------------------------
    * TLB Request
    * --------------------------------------------------------------------
    * send req to dtlb
    * keep firing until tlb hit
    */
  io.toTlb.req.valid := state === s_tlb_and_flush_sbuffer_req
  io.toTlb.req.bits := DontCare
  io.toTlb.req.bits.vaddr := vaddr
  io.toTlb.req.bits.fullva := vaddr
  io.toTlb.req.bits.checkfullva := true.B
  io.toTlb.req.bits.cmd := Mux(isLr, TlbCmd.atom_read, TlbCmd.atom_write)
  io.toTlb.req.bits.debug.pc := uop.pc
  io.toTlb.req.bits.debug.robIdx := uop.robIdx
  io.toTlb.req.bits.debug.isFirstIssue := false.B
  io.toTlb.req_kill := false.B

  /**
    * --------------------------------------------------------------------
    * DCache Request
    * --------------------------------------------------------------------
    */
  io.toDCache.req.valid := Mux(
    io.toDCache.req.bits.cmd === M_XLR,
    !io.fromDCache.blockLR, // block lr to survive in lr storm
    dataValid // wait until src(1) is ready
  ) && state === s_cache_req
  io.toDCache.req.bits := DontCare
  io.toDCache.req.bits.cmd := LookupTree(uop.fuOpType, List(
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
  io.toDCache.req.bits.miss := false.B
  io.toDCache.req.bits.probe := false.B
  io.toDCache.req.bits.probe_need_data := false.B
  io.toDCache.req.bits.source := AMO_SOURCE.U
  io.toDCache.req.bits.addr := get_block_addr(paddr)
  io.toDCache.req.bits.vaddr := get_block_addr(vaddr)
  io.toDCache.req.bits.word_idx := get_word(paddr)
  io.toDCache.req.bits.amo_data := genWdataAMO(rs2, uop.fuOpType)
  io.toDCache.req.bits.amo_mask := genWmaskAMO(paddr, uop.fuOpType)
  io.toDCache.req.bits.amo_cmp := genWdataAMO(rd, uop.fuOpType)

  /**
    * --------------------------------------------------------------------
    * Feedback to Backend
    * --------------------------------------------------------------------
    * Send TLB feedback to store issue queue
    * we send feedback right after we receives request
    * also, we always treat amo as tlb hit
    * since we will continue polling tlb all by ourself
    */
  val iqFeedback = Wire(ValidIO(new RSFeedback()))
  iqFeedback.valid := GatedValidRegNextN(io.fromBackend.issue.valid, 2)
  iqFeedback.bits.hit := true.B
  iqFeedback.bits.robIdx := uop.robIdx
  iqFeedback.bits.sqIdx := uop.sqIdx
  iqFeedback.bits.lqIdx := uop.lqIdx
  iqFeedback.bits.flushState := DontCare
  iqFeedback.bits.sourceType := DontCare
  iqFeedback.bits.dataInvalidSqIdx := DontCare

  io.toBackend.iqFeedback.feedbackFast := DontCare
  io.toBackend.iqFeedback.feedbackSlow := iqFeedback

  /**
    * --------------------------------------------------------------------
    * Flush Sbuffer
    * --------------------------------------------------------------------
    * send req to sbuffer to flush it if it is not empty
    */
  io.flushSbuffer.valid := !sbufferEmpty && (
    state === s_tlb_and_flush_sbuffer_req ||
    state === s_pm ||
    state === s_wait_flush_sbuffer_resp
  )

  /**
    * --------------------------------------------------------------------
    * Writeback to Backend
    * --------------------------------------------------------------------
    */
  io.toBackend.writeback.valid := outValid && Mux(state === s_finish2, pdest2Valid, pdest1Valid)
  io.toBackend.writeback.bits := DontCare
  io.toBackend.writeback.bits.uop := uop
  io.toBackend.writeback.bits.uop.fuType := FuType.mou.U
  io.toBackend.writeback.bits.uop.pdest := Mux(state === s_finish2, pdest2, pdest1)
  io.toBackend.writeback.bits.uop.exceptionVec := exceptionVec
  io.toBackend.writeback.bits.uop.trigger := trigger
  io.toBackend.writeback.bits.uop.debugInfo.tlbFirstReqTime := GTimer() // FIXME lyq: it will be always assigned
  io.toBackend.writeback.bits.data := Mux(state === s_finish2, respData >> XLEN, respData)

  /**
    * --------------------------------------------------------------------
    * Report Atomics Exception Informations
    * -------------------------------------------------------------------
    */
  io.exceptionInfo.valid := atomOverrideXtval
  io.exceptionInfo.bits := DontCare
  io.exceptionInfo.bits.vaddr := vaddr
  io.exceptionInfo.bits.gpaddr := gpaddr
  io.exceptionInfo.bits.isForVSnonLeafPTE := isForVSnonLeafPTE

  /**
    * -------------------------------------------------------------------
    * Difftest
    * -------------------------------------------------------------------
    */
  if (env.EnableDifftest) {
    val difftest = DifftestModule(new DiffAtomicEvent)
    val en = io.toDCache.req.fire
    difftest.coreid := io.fromCtrl.hartId
    difftest.valid  := state === s_cache_resp_latch
    difftest.addr   := RegEnable(paddr, en)
    difftest.data   := RegEnable(io.toDCache.req.bits.amo_data.asTypeOf(difftest.data), en)
    difftest.mask   := RegEnable(io.toDCache.req.bits.amo_mask, en)
    difftest.cmp    := RegEnable(io.toDCache.req.bits.amo_cmp.asTypeOf(difftest.cmp), en)
    difftest.fuop   := RegEnable(uop.fuOpType, en)
    difftest.out    := respDataWire.asTypeOf(difftest.out)
  }

  if (env.EnableDifftest || env.AlwaysBasicDiff) {
    val uop = io.toBackend.writeback.bits.uop
    val difftest = DifftestModule(new DiffLrScEvent)
    difftest.coreid := io.fromCtrl.hartId
    difftest.valid := io.toBackend.writeback.fire && state === s_finish && isSc
    difftest.success := success
  }
}
