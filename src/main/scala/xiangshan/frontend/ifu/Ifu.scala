// Copyright (c) 2024-2025 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2025 Institute of Computing Technology, Chinese Academy of Sciences
// Copyright (c) 2020-2021 Peng Cheng Laboratory
//
// XiangShan is licensed under Mulan PSL v2.
// You can use this software according to the terms and conditions of the Mulan PSL v2.
// You may obtain a copy of Mulan PSL v2 at:
//          https://license.coscl.org.cn/MulanPSL2
//
// THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
// EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
// MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
//
// See the Mulan PSL v2 for more details.

package xiangshan.frontend.ifu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.ChiselDB
import utility.Constantin
import utility.HasCircularQueuePtrHelper
import utility.HasPerfEvents
import utility.PerfCCT
import utility.UIntToMask
import utility.ValidHold
import utility.XORFold
import utility.XSDebug
import utility.XSPerfAccumulate
import utility.XSPerfHistogram
import utils.EnumUInt
import xiangshan.FrontendTdataDistributeIO
import xiangshan.RedirectLevel
import xiangshan.RobCommitInfo
import xiangshan.TopDownCounters
import xiangshan.ValidUndirectioned
import xiangshan.XSCoreParamsKey
import xiangshan.cache.mmu.HasTlbConst
import xiangshan.cache.mmu.Pbmt
import xiangshan.cache.mmu.TlbCmd
import xiangshan.cache.mmu.TlbRequestIO
import xiangshan.frontend.ExceptionType
import xiangshan.frontend.FetchToIBuffer
import xiangshan.frontend.FrontendTopDownBundle
import xiangshan.frontend.FtqToIfuIO
import xiangshan.frontend.ICacheToIfuIO
import xiangshan.frontend.IfuToBackendIO
import xiangshan.frontend.IfuToFtqIO
import xiangshan.frontend.IfuToICacheIO
import xiangshan.frontend.IfuToInstrUncacheIO
import xiangshan.frontend.InstrUncacheToIfuIO
import xiangshan.frontend.PreDecodeInfo
import xiangshan.frontend.PredecodeWritebackBundle
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.PrunedAddrInit
import xiangshan.frontend.ibuffer.IBufPtr
import xiangshan.frontend.icache.PmpCheckBundle

class Ifu(implicit p: Parameters) extends IfuModule
    with FetchBlockHelper
    with PreDecodeHelper
    with IfuHelper
    with HasCircularQueuePtrHelper
    with HasPerfEvents
    with HasTlbConst {

  class IfuIO(implicit p: Parameters) extends IfuBundle {
    // Ftq: request / write back
    val fromFtq: FtqToIfuIO = Flipped(new FtqToIfuIO)
    val toFtq:   IfuToFtqIO = new IfuToFtqIO

    // ICache: response / stall
    val fromICache: ICacheToIfuIO = Flipped(new ICacheToIfuIO)
    val toICache:   IfuToICacheIO = new IfuToICacheIO

    // Uncache: mmio request / response
    val toUncache:   IfuToInstrUncacheIO = new IfuToInstrUncacheIO
    val fromUncache: InstrUncacheToIfuIO = Flipped(new InstrUncacheToIfuIO)

    // IBuffer: enqueue
    val toIBuffer: DecoupledIO[FetchToIBuffer] = DecoupledIO(new FetchToIBuffer)

    // Backend: gpaMem
    val toBackend: IfuToBackendIO = new IfuToBackendIO
    // Backend: commit
    val robCommits: Vec[Valid[RobCommitInfo]] = Flipped(Vec(CommitWidth, Valid(new RobCommitInfo)))

    // debug extension: frontend trigger
    val frontendTrigger: FrontendTdataDistributeIO = Flipped(new FrontendTdataDistributeIO)

    // itlb: req / resp
    val itlb: TlbRequestIO = new TlbRequestIO

    // pmp: req / resp
    val pmp: PmpCheckBundle = new PmpCheckBundle

    // Backend: csr control
    val csrFsIsOff: Bool = Input(Bool())
  }
  val io: IfuIO = IO(new IfuIO)

  // submodule

  private val preDecoder      = Module(new PreDecode)
  private val instrBoundary   = Module(new InstrBoundary)
  private val predChecker     = Module(new PredChecker)
  private val frontendTrigger = Module(new FrontendTrigger)
  private val rvcExpanders    = Seq.fill(IBufferEnqueueWidth)(Module(new RvcExpander))
  private val mmioRvcExpander = Module(new RvcExpander)

  // alias
  private val (toFtq, fromFtq)              = (io.toFtq, io.fromFtq)
  private val fromICache                    = io.fromICache.fetchResp
  private val (toUncache, fromUncache)      = (io.toUncache.req, io.fromUncache.resp)
  private val (preDecoderIn, preDecoderOut) = (preDecoder.io.req, preDecoder.io.resp)
  private val (checkerIn, checkerOutStage1, checkerOutStage2) =
    (predChecker.io.req, predChecker.io.resp.stage1Out, predChecker.io.resp.stage2Out)

  private val s1_ready, s2_ready, s3_ready, s4_ready           = WireInit(false.B)
  private val s0_fire, s1_fire, s2_fire, s3_fire, s4_fire      = WireInit(false.B)
  private val s0_flush, s1_flush, s2_flush, s3_flush, s4_flush = WireInit(false.B)

  // Top-down
  private def numOfStage = 3
  require(numOfStage > 1, "Ifu numOfStage must be greater than 1")
  private val topdownStages = RegInit(VecInit(Seq.fill(numOfStage)(0.U.asTypeOf(new FrontendTopDownBundle))))
  // bubble events in IFU, only happen in stage 1
  private val icacheMissBubble = io.fromICache.topdown.iCacheMissBubble
  private val itlbMissBubble   = io.fromICache.topdown.itlbMissBubble

  // only driven by clock, not valid-ready
  topdownStages(0) := fromFtq.req.bits.topdownInfo
  for (i <- 1 until numOfStage) {
    topdownStages(i) := topdownStages(i - 1)
  }
  when(icacheMissBubble) {
    topdownStages(1).reasons(TopDownCounters.ICacheMissBubble.id) := true.B
  }
  when(itlbMissBubble) {
    topdownStages(1).reasons(TopDownCounters.ITLBMissBubble.id) := true.B
  }
  io.toIBuffer.bits.topdownInfo := topdownStages(numOfStage - 1)
  when(fromFtq.topdownRedirect.valid) {
    // only redirect from backend, IFU redirect itself is handled elsewhere
    when(fromFtq.topdownRedirect.bits.debugIsCtrl) {
      /*
      for (i <- 0 until numOfStage) {
        topdown_stages(i).reasons(TopDownCounters.ControlRedirectBubble.id) := true.B
      }
      io.toIBuffer.bits.topdownInfo.reasons(TopDownCounters.ControlRedirectBubble.id) := true.B
       */
      // FIXME
//      when(fromFtq.topdown_redirect.bits.ControlBTBMissBubble) {
//        for (i <- 0 until numOfStage) {
//          topdownStages(i).reasons(TopDownCounters.BTBMissBubble.id) := true.B
//        }
//        io.toIBuffer.bits.topdownInfo.reasons(TopDownCounters.BTBMissBubble.id) := true.B
//      }.elsewhen(fromFtq.topdown_redirect.bits.TAGEMissBubble) {
//        for (i <- 0 until numOfStage) {
//          topdownStages(i).reasons(TopDownCounters.TAGEMissBubble.id) := true.B
//        }
//        io.toIBuffer.bits.topdownInfo.reasons(TopDownCounters.TAGEMissBubble.id) := true.B
//      }.elsewhen(fromFtq.topdown_redirect.bits.SCMissBubble) {
//        for (i <- 0 until numOfStage) {
//          topdownStages(i).reasons(TopDownCounters.SCMissBubble.id) := true.B
//        }
//        io.toIBuffer.bits.topdownInfo.reasons(TopDownCounters.SCMissBubble.id) := true.B
//      }.elsewhen(fromFtq.topdown_redirect.bits.ITTAGEMissBubble) {
//        for (i <- 0 until numOfStage) {
//          topdownStages(i).reasons(TopDownCounters.ITTAGEMissBubble.id) := true.B
//        }
//        io.toIBuffer.bits.topdownInfo.reasons(TopDownCounters.ITTAGEMissBubble.id) := true.B
//      }.elsewhen(fromFtq.topdown_redirect.bits.RASMissBubble) {
//        for (i <- 0 until numOfStage) {
//          topdownStages(i).reasons(TopDownCounters.RASMissBubble.id) := true.B
//        }
//        io.toIBuffer.bits.topdownInfo.reasons(TopDownCounters.RASMissBubble.id) := true.B
//      }
    }.elsewhen(fromFtq.topdownRedirect.bits.debugIsMemVio) {
      for (i <- 0 until numOfStage) {
        topdownStages(i).reasons(TopDownCounters.MemVioRedirectBubble.id) := true.B
      }
      io.toIBuffer.bits.topdownInfo.reasons(TopDownCounters.MemVioRedirectBubble.id) := true.B
    }.otherwise {
      for (i <- 0 until numOfStage) {
        topdownStages(i).reasons(TopDownCounters.OtherRedirectBubble.id) := true.B
      }
      io.toIBuffer.bits.topdownInfo.reasons(TopDownCounters.OtherRedirectBubble.id) := true.B
    }
  }

  /* *****************************************************************************
   * IFU Stage 0
   *
   * - Sends cacheline fetch requests to ICacheMainPipe.
   *
   * **************************************************************************** */
  private val s0_ftqFetch   = fromFtq.req.bits.fetch
  private val s0_doubleline = VecInit(fromFtq.req.bits.fetch.map(_.crossCacheline))

  s0_fire := fromFtq.req.fire

  private val s0_flushFromBpu = VecInit.tabulate(FetchPorts)(i =>
    fromFtq.flushFromBpu.shouldFlushByStage2(s0_ftqFetch(i).ftqIdx) ||
      fromFtq.flushFromBpu.shouldFlushByStage3(s0_ftqFetch(i).ftqIdx)
  )

  private val backendRedirect          = WireInit(false.B)
  private val wbRedirect, mmioRedirect = WireInit(0.U.asTypeOf(new IfuRedirectInternal))

  private val s4_wbNotFlush = WireInit(false.B)

  backendRedirect := fromFtq.redirect.valid
  s4_flush        := backendRedirect || (wbRedirect.valid && !s4_wbNotFlush)
  s3_flush        := backendRedirect || mmioRedirect.valid || wbRedirect.valid
  s2_flush        := s3_flush
  s1_flush        := s2_flush
  s0_flush        := s1_flush || s0_flushFromBpu(0)

  fromFtq.req.ready := s1_ready && io.fromICache.fetchReady

  when(wbRedirect.valid) {
    when(s4_wbNotFlush) {
      topdownStages(2).reasons(TopDownCounters.BTBMissBubble.id) := true.B
    }
    for (i <- 0 until numOfStage - 1) {
      topdownStages(i).reasons(TopDownCounters.BTBMissBubble.id) := true.B
    }
  }

  /** <PERF> f0 fetch bubble */
  XSPerfAccumulate("fetch_bubble_ftq_not_valid", !fromFtq.req.valid && fromFtq.req.ready)
  XSPerfAccumulate("fetch_flush_backend_redirect", backendRedirect)
  XSPerfAccumulate("fetch_flush_wb_redirect", wbRedirect.valid)
  XSPerfAccumulate("fetch_flush_s0_flush_from_bpu", s0_flushFromBpu(0) | s0_flushFromBpu(1))

  /* *****************************************************************************
   * IFU Stage 1
   * - calculate pc/half_pc/cut_ptr for every instruction
   * ***************************************************************************** */
  private val s1_valid      = ValidHold(s0_fire && !s0_flush, s1_fire, s1_flush)
  private val s1_firstValid = ValidHold(s0_fire && !s0_flush && s0_ftqFetch(0).valid, s1_fire, s1_flush)
  private val s1_secondValid =
    ValidHold(s0_fire && !s0_flush && s0_ftqFetch(1).valid && !s0_flushFromBpu(1), s1_fire, s1_flush)
  private val s1_ftqFetch   = RegEnable(s0_ftqFetch, s0_fire)
  private val s1_doubleline = RegEnable(s0_doubleline, s0_fire)

  s1_fire  := s1_valid && s2_ready
  s1_ready := s1_fire || !s1_valid

  assert(!(fromFtq.flushFromBpu.shouldFlushByStage3(s1_ftqFetch(0).ftqIdx) && s1_firstValid))
  assert(!(fromFtq.flushFromBpu.shouldFlushByStage3(s1_ftqFetch(1).ftqIdx) && s1_secondValid))

  private val s1_takenCfiOffset = VecInit.tabulate(FetchPorts)(i => s1_ftqFetch(i).takenCfiOffset)
  private val s1_fetchSize = VecInit.tabulate(FetchPorts) { i =>
    Mux(
      s1_takenCfiOffset(i).valid,
      s1_takenCfiOffset(i).bits + 1.U(log2Ceil(FetchBlockInstNum + 1).W),
      (s1_ftqFetch(i).nextStartVAddr - s1_ftqFetch(i).startVAddr)(log2Ceil(FetchBlockInstNum + 1) + 1, 1)
    )
  }

  private val s1_jumpRange = VecInit.tabulate(FetchPorts)(i =>
    Fill(FetchBlockInstNum, !s1_takenCfiOffset(i).valid) |
      (Fill(FetchBlockInstNum, 1.U(1.W)) >> (~s1_takenCfiOffset(i).bits).asUInt).asUInt
  )
  private val s1_ftrRange = VecInit.tabulate(FetchPorts)(i =>
    Fill(FetchBlockInstNum, s1_takenCfiOffset(i).valid) |
      (Fill(FetchBlockInstNum, 1.U(1.W)) >> (~getBasicBlockIdx(
        s1_ftqFetch(i).nextStartVAddr,
        s1_ftqFetch(i).startVAddr
      )).asUInt).asUInt
  )
  private val s1_instrRange = VecInit.tabulate(FetchPorts)(i =>
    s1_jumpRange(i) & s1_ftrRange(i)
  )

  private val s1_totalEndPos =
    Mux(s1_firstValid && s1_secondValid, s1_fetchSize(0) + s1_fetchSize(1) - 1.U, s1_fetchSize(0) - 1.U)

  private val s1_firstEndPos  = s1_fetchSize(0) - 1.U
  private val s1_secondEndPos = s1_fetchSize(1) - 1.U
  private val s1_toatalInstrRange =
    Mux(!s1_secondValid, s1_instrRange(0), (s1_instrRange(1) << s1_fetchSize(0)).asUInt | s1_instrRange(0))
  /* *****************************************************************************
   * IFU Stage 2
   * - icache response data (latched for pipeline stop)
   * - generate exception bits for every instruction (page fault/access fault/mmio)
   * - generate predicted instruction range (1 means this instruction is in this fetch packet)
   * - cut data from cachelines to packet instruction code
   * - instruction preDecode and RVC expand
   * ***************************************************************************** */
  private val icacheRespAllValid = WireInit(false.B)

  private val s2_valid             = ValidHold(s1_fire && !s1_flush, s2_fire, s2_flush)
  private val s2_firstValid        = ValidHold(s1_fire && !s1_flush && s1_firstValid, s2_fire, s2_flush)
  private val s2_secondValid       = ValidHold(s1_fire && !s1_flush && s1_secondValid, s2_fire, s2_flush)
  private val s2_ftqFetch          = RegEnable(s1_ftqFetch, s1_fire)
  private val s2_doubleline        = RegEnable(s1_doubleline, s1_fire)
  private val s2_prevLastIsHalfRvi = RegInit(false.B)
  private val s2_fetchSize         = RegEnable(s1_fetchSize, s1_fire)
  private val s2_instrRange        = RegEnable(s1_instrRange, s1_fire)
  private val s2_totalInstrRange   = RegEnable(s1_toatalInstrRange, s1_fire)
  private val s2_firstEndPos       = RegEnable(s1_firstEndPos, s1_fire)
  private val s2_totalEndPos       = RegEnable(s1_totalEndPos, s1_fire)
  private val s2_firstMaskEndPos   = RegEnable((~(1.U << s1_firstEndPos)).asUInt, s1_fire)
  private val s2_secondMaskEndPos  = RegEnable((~(1.U << s1_secondEndPos)).asUInt, s1_fire)
  private val s2_totalMaskEndPos   = RegEnable((~(1.U << s1_totalEndPos)).asUInt, s1_fire)
  private val s2_takenCfiOffset    = RegEnable(s1_takenCfiOffset, s1_fire)

  private val s2_instrPcLowerResult = WireDefault(VecInit.fill(FetchBlockInstNum)(0.U((PcCutPoint + 1).W)))
  private val s2_instrIsRvc         = WireDefault(VecInit.fill(FetchBlockInstNum)(false.B))
  private val s2_instrEndOffset     = WireDefault(VecInit.fill(FetchBlockInstNum)(0.U(FetchBlockInstOffsetWidth.W)))
  private val s2_identifiedCfi      = WireDefault(VecInit.fill(FetchBlockInstNum)(false.B))

  s2_fire  := s2_valid && s3_ready && icacheRespAllValid
  s2_ready := s2_fire || !s2_valid

  // TODO: addr compare may be timing critical
  private val s2_iCacheAllRespWire =
    fromICache.valid &&
      fromICache.bits.vAddr(0) === s2_ftqFetch(0).startVAddr &&
      (fromICache.bits.doubleline && fromICache.bits.vAddr(1) === s2_ftqFetch(0).nextCachelineVAddr ||
        !s2_doubleline(0))
  private val s2_iCacheAllRespReg = ValidHold(s2_valid && s2_iCacheAllRespWire && !s3_ready, s2_fire, s2_flush)

  icacheRespAllValid := s2_iCacheAllRespReg || s2_iCacheAllRespWire

  io.toICache.stall := !s3_ready

  private val s2_icacheInfo = WireDefault(0.U.asTypeOf(Vec(FetchPorts, new ICacheInfo)))
  s2_icacheInfo(0).exception          := fromICache.bits.exception
  s2_icacheInfo(0).pmpMmio            := fromICache.bits.pmpMmio
  s2_icacheInfo(0).itlbPbmt           := fromICache.bits.itlbPbmt
  s2_icacheInfo(0).isBackendException := fromICache.bits.isBackendException
  s2_icacheInfo(0).pAddr              := fromICache.bits.pAddr
  s2_icacheInfo(0).gpAddr             := fromICache.bits.gpAddr
  s2_icacheInfo(0).isForVSnonLeafPTE  := fromICache.bits.isForVSnonLeafPTE

  // we need only the first port, as the second is asked to be the same
  private val s2_pmpMmio  = fromICache.bits.pmpMmio
  private val s2_itlbPbmt = fromICache.bits.itlbPbmt

  private val s2_rawData  = fromICache.bits.data
  private val s2_perfInfo = io.fromICache.perf

  instrBoundary.io.req.valid                 := s2_valid && fromICache.valid
  instrBoundary.io.req.instrRange            := s2_totalInstrRange.asTypeOf(Vec(FetchBlockInstNum, Bool()))
  instrBoundary.io.req.firstFetchBlockEndPos := s2_firstEndPos
  instrBoundary.io.req.endPos                := s2_totalEndPos
  instrBoundary.io.req.firstInstrIsHalfRvi   := s2_prevLastIsHalfRvi
  instrBoundary.io.req.cacheData := (Cat(s2_rawData, s2_rawData) >> Cat(
    s2_ftqFetch(0).startVAddr(5, 0),
    0.U(3.W)
  )).asUInt

  private val s2_firstFetchEndIsHalf = instrBoundary.io.resp.firstFetchBlockLastInstrIsHalfRvi
  private val s2_fetchEndIsHalf      = instrBoundary.io.resp.lastInstrIsHalfRvi

  private val wbStage2Check = Wire(Vec(FetchPorts, new FinalPredCheckResult))

  // When invalidTaken is true, we can not flush s2_prevLastIsHalfRvi because the fetch block after it is fall-through.
  when(backendRedirect) {
    s2_prevLastIsHalfRvi := false.B
  }.elsewhen(wbRedirect.valid) {
    s2_prevLastIsHalfRvi := wbRedirect.isHalfInstr
  }.elsewhen(mmioRedirect.valid) {
    s2_prevLastIsHalfRvi := false.B
  }.elsewhen(s2_fire) {
    s2_prevLastIsHalfRvi := s2_fetchEndIsHalf
  }

// rawInstrValid(i) and instrCountBeforeCurrent(i) also handle instructions
// spanning across prediction blocks. This design aligns with the logic
// used for s3_prevLastHalfData calculation.
  private val dealInstrValid = Wire(Vec(FetchBlockInstNum, Bool()))
  dealInstrValid    := instrBoundary.io.resp.instrValid
  dealInstrValid(0) := instrBoundary.io.resp.instrValid(0) | s2_prevLastIsHalfRvi

  private val rawInstrEndVec = instrBoundary.io.resp.instrEndVec
  private val rawIsRvc       = instrBoundary.io.resp.isRvc

  /* *****************************************************************************
   * instrCountBeforeCurrent(i), not include rawInstrValid(i)
   * ***************************************************************************** */
  private val instrCountBeforeCurrent =
    WireDefault(VecInit.fill(FetchBlockInstNum + 1)(0.U(log2Ceil(FetchBlockInstNum + 1).W)))
  for (i <- 0 until FetchBlockInstNum) {
    instrCountBeforeCurrent(i) := PopCount(dealInstrValid.take(i))
  }
  instrCountBeforeCurrent(FetchBlockInstNum) := PopCount(dealInstrValid)

  private val instrIndexEntry = Wire(Vec(FetchBlockInstNum, new InstrIndexEntry))
  private val fetchBlockSelect =
    VecInit.tabulate(FetchBlockInstNum)(i =>
      Mux(s2_fetchSize(0) > i.U, false.B, true.B)
    )

  private val s2_fetchPcLowerResult = VecInit.tabulate(FetchPorts)(i =>
    VecInit((0 until FetchBlockInstNum).map(j =>
      Cat(0.U(1.W), s2_ftqFetch(i).startVAddr(PcCutPoint - 1, 0)) + (j * 2).U
    ))
  ) // cat with overflow bit

  private val s2_fetchBlockIndex = VecInit.tabulate(FetchPorts)(i =>
    VecInit.tabulate(FetchBlockInstNum)(j =>
      s2_fetchPcLowerResult(i)(j)(log2Ceil(ICacheLineBytes) - 1, 1)
    )
  )

  // FIXME: This is wrong when 2-taken is enabled
  private val twoFetchBlockIndex = VecInit.tabulate(FetchBlockInstNum)(i =>
    Mux(s2_fetchSize(0) > i.U, s2_fetchBlockIndex(0)(i), s2_fetchBlockIndex(1)(i))
  )

  private val twoFetchPcLowerResult = VecInit.tabulate(FetchBlockInstNum)(i =>
    Mux(s2_fetchSize(0) > i.U, s2_fetchPcLowerResult(0)(i), s2_fetchPcLowerResult(1)(i))
  )

  // FIXME: This is wrong when 2-taken is enabled
  private val twoFetchIdentifiedCfi = VecInit.tabulate(FetchBlockInstNum) { i =>
    // This is a dirty hack, make sure it's correct.
    val identifiedCfi = s2_ftqFetch(0).identifiedCfi
    if (i == 0) Mux(s2_prevLastIsHalfRvi | rawIsRvc(0), identifiedCfi(0), identifiedCfi(1))
    else if (i < FetchBlockInstNum - 1) Mux(rawIsRvc(i), identifiedCfi(i), identifiedCfi(i + 1))
    else identifiedCfi(i)
  }

  private val s2_rawPcLowerResult = twoFetchPcLowerResult

  private val instrSelectLowIndex   = WireDefault(VecInit.fill(FetchBlockInstNum)(true.B))
  private val instrSelectFetchBlock = WireDefault(VecInit.fill(FetchBlockInstNum)(false.B))

  // Fetch PC and index info for valid instructions based on their positions.
  instrIndexEntry.zipWithIndex.foreach {
    case (instrIndex, idx) =>
      val instrRange = idx until Math.min(2 * idx + 2, FetchBlockInstNum)

      val validOH = instrRange.map {
        i => dealInstrValid(i) & (instrCountBeforeCurrent(i) === idx.U)
      }

      val index         = instrRange.map(twoFetchBlockIndex(_))
      val select        = instrRange.map(fetchBlockSelect(_))
      val pcLowerResult = instrRange.map(twoFetchPcLowerResult(_))
      val isRvc         = instrRange.map(rawIsRvc(_))
      val instrOffset   = instrRange.map(i => Mux(rawIsRvc(i), i.U, (i + 1).U))
      // FIXME: This is wrong when 2-taken is enabled
      val identifiedCfi = instrRange.map(twoFetchIdentifiedCfi(_))

      instrIndex.valid           := validOH.reduce(_ || _)
      instrIndex.value           := Mux1H(validOH, index)
      instrSelectFetchBlock(idx) := Mux1H(validOH, select)
      s2_instrPcLowerResult(idx) := Mux1H(validOH, pcLowerResult)
      s2_instrIsRvc(idx)         := Mux1H(validOH, isRvc)
      s2_instrEndOffset(idx)     := Mux1H(validOH, instrOffset)
      s2_identifiedCfi(idx)      := Mux1H(validOH, identifiedCfi)
  }
  s2_instrEndOffset(0) := Mux(s2_prevLastIsHalfRvi, 0.U, Mux(rawIsRvc(0), 0.U, 1.U))

  private val s2_fetchTakenIdx = VecInit((0 until FetchPorts).map { i =>
    val b = Wire(new Valid(UInt(FetchBlockInstOffsetWidth.W)))
    b.valid := s2_takenCfiOffset(i).valid
    // This is the main reason for using Start — it makes index calculation easier when handling lastRvi.
    b.bits := PopCount(dealInstrValid.asUInt & s2_instrRange(i)) - 1.U
    b
  })
  s2_fetchTakenIdx(0).valid := s2_takenCfiOffset(0).valid && s2_firstValid
  s2_fetchTakenIdx(1).valid := s2_takenCfiOffset(1).valid && s2_secondValid

  private val s2_fetchBlock = VecInit((0 until FetchPorts).map { i =>
    val b = Wire(new FetchBlockInfo)
    b.ftqIdx       := s2_ftqFetch(i).ftqIdx
    b.doubleline   := s2_doubleline(i)
    b.predTakenIdx := s2_fetchTakenIdx(i)
    // This is the main reason for using End — it makes invalidTaken calculation easier when handling lastRvi.
    b.invalidTaken         := !rawInstrEndVec(s2_takenCfiOffset(i).bits) && s2_takenCfiOffset(i).valid
    b.takenCfiOffset.valid := s2_takenCfiOffset(i).valid
    b.takenCfiOffset.bits  := s2_takenCfiOffset(i).bits
    b.instrRange           := s2_instrRange(i)
    b.pcHigh               := s2_ftqFetch(i).startVAddr(VAddrBits - 1, PcCutPoint)
    b.pcHighPlus1          := s2_ftqFetch(i).startVAddr(VAddrBits - 1, PcCutPoint) + 1.U
    b.startVAddr           := s2_ftqFetch(i).startVAddr
    b.target               := s2_ftqFetch(i).nextStartVAddr
    b.fetchSize            := s2_fetchSize(i)
    b.rawInstrEndVec       := rawInstrEndVec.asUInt & s2_instrRange(i)
    b.identifiedCfi        := s2_ftqFetch(i).identifiedCfi
    b
  })
  s2_fetchBlock(0).takenCfiOffset.valid := s2_takenCfiOffset(0).valid && s2_firstValid
  s2_fetchBlock(1).takenCfiOffset.valid := s2_takenCfiOffset(1).valid && s2_secondValid
  s2_fetchBlock(0).instrRange := s2_instrRange(0) &
    Mux(s2_firstFetchEndIsHalf, s2_firstMaskEndPos, Fill(FetchBlockInstNum, 1.U(1.W)))
  s2_fetchBlock(1).instrRange := s2_instrRange(1) &
    Mux(s2_fetchEndIsHalf, s2_secondMaskEndPos, Fill(FetchBlockInstNum, 1.U(1.W)))

  // After completing the adjustment of a half prediction block, the instruction
  // valid signals at the end and beginning need to be updated.
  s2_fetchBlock(0).rawInstrEndVec := rawInstrEndVec.asUInt & s2_instrRange(0)
  s2_fetchBlock(1).rawInstrEndVec := (rawInstrEndVec.asUInt >> s2_fetchSize(0)).asUInt & s2_instrRange(1)
  private val s2_rawFirstData         = s2_rawData
  private val s2_rawSecondData        = 0.U((ICacheLineBytes * 8).W)
  private val s2_rawFirstDataDupWire  = VecInit(Seq.fill(FetchPorts)(s2_rawFirstData))
  private val s2_rawSecondDataDupWire = VecInit(Seq.fill(FetchPorts)(s2_rawSecondData))
  private val s2_firstEndIdx          = s2_fetchTakenIdx(0).bits
  // Special case for MMIO:
  // If two fetches occur and the first is non-MMIO while the second is MMIO,
  // delay the second fetch by one cycle to split into a one-fetch.

  /* *****************************************************************************
   * IFU Stage 3
   * ***************************************************************************** */
  private val s3_valid          = ValidHold(s2_fire && !s2_flush, s3_fire, s3_flush)
  private val s3_firstValid     = ValidHold(s2_fire && !s2_flush && s2_firstValid, s3_fire, s3_flush)
  private val s3_secondValid    = ValidHold(s2_fire && !s2_flush && s2_secondValid, s3_fire, s3_flush)
  private val s3_fetchBlock     = RegEnable(s2_fetchBlock, s2_fire)
  private val s3_prevIBufEnqPtr = RegInit(0.U.asTypeOf(new IBufPtr))

  private val s3_prevShiftSelect = UIntToMask(s3_prevIBufEnqPtr.value(1, 0), IfuAlignWidth)

  s3_fire  := s3_valid && s4_ready
  s3_ready := s3_fire || !s3_valid

  private val s3_instrIndex       = RegEnable(instrIndexEntry, s2_fire)
  private val s3_selectFetchBlock = RegEnable(instrSelectFetchBlock, s2_fire)
  private val s3_instrIsRvc       = RegEnable(s2_instrIsRvc, s2_fire)
  private val s3_instrCount       = RegEnable(PopCount(rawInstrEndVec), s2_fire)
  private val s3_instrValid       = RegEnable(UIntToMask(PopCount(rawInstrEndVec), FetchBlockInstNum), s2_fire)

  private val s3_rawIndex           = RegEnable(instrCountBeforeCurrent, s2_fire)
  private val s3_rawInstrEndVec     = RegEnable(rawInstrEndVec, s2_fire)
  private val s3_prevLastIsHalfRvi  = RegEnable(s2_prevLastIsHalfRvi, s2_fire)
  private val s3_prevLastHalfData   = RegInit(0.U(16.W))
  private val s3_prevLastHalfPc     = RegInit(0.U.asTypeOf(PrunedAddr(VAddrBits)))
  private val s3_perfInfo           = RegEnable(s2_perfInfo, s2_fire)
  private val s3_currentLastHalfRvi = RegEnable(s2_fetchEndIsHalf, s2_fire)

  private val s3_rawFirstDataDup  = RegEnable(s2_rawFirstDataDupWire, s2_fire)
  private val s3_rawSecondDataDup = RegEnable(s2_rawSecondDataDupWire, s2_fire)

  private val s3_doubleline       = RegEnable(s2_doubleline, s2_fire)
  private val s3_icacheInfo       = RegEnable(s2_icacheInfo, s2_fire)
  private val s3_instrEndOffset   = RegEnable(s2_instrEndOffset, s2_fire)
  private val s3_firstResendVAddr = RegEnable(s2_fetchBlock(0).startVAddr + 2.U, s2_fire)

  private val s3_instrPcLowerResult = RegEnable(s2_instrPcLowerResult, s2_fire)
  private val s3_rawPcLowerResult   = RegEnable(s2_rawPcLowerResult, s2_fire)

  private val s3_identifiedCfi = RegEnable(s2_identifiedCfi, s2_fire)

  /* NOTE: the following `Cat(_data, _data)` *is* intentional.
   * Explanation:
   * In the old design, IFU is responsible for selecting requested data from two adjacent cachelines,
   *    so IFU has to receive 2*64B (2cacheline * 64B) data from ICache, and do `Cat(_data(1), _data(0))` here.
   * However, a fetch block is 34B at max, sending 2*64B is quiet a waste of power.
   * In current design (2024.06~), ICacheDataArray is responsible for selecting data from two adjacent cachelines,
   *    so IFU only need to receive 40B (5bank * 8B) valid data, and use only one port is enough.
   * For example, when pc falls on the 6th bank in cacheline0(so this is a doubleline request):
   *                      MSB                                         LSB
   *        cacheline 1 || 1-7 | 1-6 | 1-5 | 1-4 | 1-3 | 1-2 | 1-1 | 1-0 ||
   *        cacheline 0 || 0-7 | 0-6 | 0-5 | 0-4 | 0-3 | 0-2 | 0-1 | 0-0 ||
   *    and ICacheDataArray will respond:
   *         s2_rawData || 0-7 | 0-6 | xxx | xxx | xxx | 1-2 | 1-1 | 1-0 ||
   *    therefore simply make a copy of the response and `Cat` together, and obtain the requested data from centre:
   *      s2_copiedData || 0-7 | 0-6 | xxx | xxx | xxx | 1-2 | 1-1 | 1-0 | 0-7 | 0-6 | xxx | xxx | xxx | 1-2 | 1-1 | 1-0 ||
   *                                   requested data: ^-----------------------------^
   * For another example, pc falls on the 1st bank in cacheline 0, we have:
   *         s2_rawData || xxx | xxx | 0-5 | 0-4 | 0-3 | 0-2 | 0-1 | xxx ||
   *      s2_copiedData || xxx | xxx | 0-5 | 0-4 | 0-3 | 0-2 | 0-1 | xxx | xxx | xxx | 0-5 | 0-4 | 0-3 | 0-2 | 0-1 | xxx ||
   *                                                                 requested data: ^-----------------------------^
   * Each "| x-y |" block is a 8B bank from cacheline(x).bank(y)
   * Please also refer to:
   * - DataArray selects data:
   * https://github.com/OpenXiangShan/XiangShan/blob/d4078d6edbfb4611ba58c8b0d1d8236c9115dbfc/src/main/scala/xiangshan/frontend/icache/ICache.scala#L355-L381
   * https://github.com/OpenXiangShan/XiangShan/blob/d4078d6edbfb4611ba58c8b0d1d8236c9115dbfc/src/main/scala/xiangshan/frontend/icache/ICache.scala#L149-L161
   * - ICache respond to IFU:
   * https://github.com/OpenXiangShan/XiangShan/blob/d4078d6edbfb4611ba58c8b0d1d8236c9115dbfc/src/main/scala/xiangshan/frontend/icache/ICacheMainPipe.scala#L473
   */
  // Placeholder logic for now; subject to change later
  private val s3_firstLowICacheData   = cutICacheData(s3_rawFirstDataDup(0))
  private val s3_firstHighICacheData  = cutICacheData(s3_rawFirstDataDup(1))
  private val s3_secondLowICacheData  = cutICacheData(s3_rawSecondDataDup(0))
  private val s3_secondHighICacheData = cutICacheData(s3_rawSecondDataDup(1))

  private val s3_isPredTaken = VecInit.tabulate(FetchBlockInstNum)(i =>
    ((s3_fetchBlock(0).predTakenIdx.bits === i.U && s3_fetchBlock(0).predTakenIdx.valid) &&
      !s3_selectFetchBlock(i) && s3_firstValid) ||
      ((s3_fetchBlock(1).predTakenIdx.bits === i.U && s3_fetchBlock(1).predTakenIdx.valid) &&
        s3_selectFetchBlock(i) && s3_secondValid)
  )

  private val s3_invalidTaken = WireDefault(VecInit.fill(FetchBlockInstNum)(false.B))
  s3_invalidTaken(s3_fetchBlock(0).predTakenIdx.bits) := s3_fetchBlock(0).invalidTaken && s3_firstValid

  private val s3_alignShiftNum = s3_prevIBufEnqPtr.value(1, 0)
  // Maybe it's better to move the calculation of enqBlockStartPos to the previous pipeline stage
  // — at least from a timing perspective. But it would require modifying IBufferPrevPtr.
  private val s3_alignBlockStartPos = WireDefault(VecInit.fill(IBufferEnqueueWidth)(false.B))
  s3_alignBlockStartPos(s3_alignShiftNum) := true.B
  private val s3_alignInstrPcLower =
    alignData(s3_instrPcLowerResult, s3_alignShiftNum, 0.U((PcCutPoint + 1).W))
  private val s3_alignInstrData = WireDefault(VecInit.fill(IBufferEnqueueWidth)(0.U(32.W)))
  private val s3_alignInstrIndex =
    alignData(s3_instrIndex, s3_alignShiftNum, 0.U.asTypeOf(new InstrIndexEntry))
  private val s3_alignInstrIsRvc = alignData(s3_instrIsRvc, s3_alignShiftNum, false.B)
  private val s3_alignInstrValid =
    alignData(s3_instrValid.asTypeOf(Vec(FetchBlockInstNum, Bool())), s3_alignShiftNum, false.B)
  private val s3_alignInvalidTaken = alignData(s3_invalidTaken, s3_alignShiftNum, false.B)
  private val s3_alignIsPredTaken  = alignData(s3_isPredTaken, s3_alignShiftNum, false.B)
  private val s3_alignSelectBlock  = alignData(s3_selectFetchBlock, s3_alignShiftNum, false.B)
  private val s3_alignInstrEndOffset =
    alignData(s3_instrEndOffset, s3_alignShiftNum, 0.U(FetchBlockInstOffsetWidth.W))
  private val s3_alignPc = VecInit.tabulate(IBufferEnqueueWidth)(i =>
    catPC(
      s3_alignInstrPcLower(i),
      Mux(s3_alignSelectBlock(i), s3_fetchBlock(1).pcHigh, s3_fetchBlock(0).pcHigh),
      Mux(s3_alignSelectBlock(i), s3_fetchBlock(1).pcHighPlus1, s3_fetchBlock(0).pcHighPlus1)
    )
  )
  private val s3_realAlignInstrData = WireDefault(VecInit.fill(IBufferEnqueueWidth)(0.U(32.W)))
  private val s3_realAlignPc = WireDefault(VecInit.fill(IBufferEnqueueWidth)(0.U.asTypeOf(PrunedAddr(VAddrBits))))
  private val s3_alignFoldPc = VecInit(s3_realAlignPc.map(i => XORFold(i(VAddrBits - 1, 1), MemPredPCWidth)))
  private val s3_alignIdentifiedCfi = alignData(s3_identifiedCfi, s3_alignShiftNum, false.B)

  for (i <- 0 until IBufferEnqueueWidth / 2) {
    val lowIdx     = s3_alignInstrIndex(i).value
    val highIdx    = s3_alignInstrIndex(i + IBufferEnqueueWidth / 2).value
    val j          = i + IBufferEnqueueWidth / 2
    val lowSelect  = s3_alignSelectBlock(i)
    val highSelect = s3_alignSelectBlock(j)
    s3_alignInstrData(i) := Mux(!lowSelect, s3_firstLowICacheData(lowIdx), s3_secondLowICacheData(lowIdx))
    s3_alignInstrData(j) := Mux(!highSelect, s3_firstHighICacheData(highIdx), s3_secondHighICacheData(highIdx))
  }

  for (i <- 0 until IBufferEnqueueWidth) {
    // Handling of cross-predict-block instructions in the one-fetch case.
    // This part should only be modified after the backend changes are completed.
    val adjustedBlockHeadInst =
      Mux(s3_prevLastIsHalfRvi, Cat(s3_alignInstrData(i)(15, 0), s3_prevLastHalfData), s3_alignInstrData(i))
    val adjustedBlockHeadPc =
      Mux(s3_prevLastIsHalfRvi, s3_prevLastHalfPc, s3_alignPc(i))
    s3_realAlignInstrData(i) := Mux(s3_alignBlockStartPos(i), adjustedBlockHeadInst, s3_alignInstrData(i))
    s3_realAlignPc(i)        := Mux(s3_alignBlockStartPos(i), adjustedBlockHeadPc, s3_alignPc(i))
  }

  // backendRedirect has the highest priority
  when(backendRedirect) {
    s3_prevLastHalfData := 0.U
    s3_prevLastHalfPc   := 0.U.asTypeOf(PrunedAddr(VAddrBits))
  }.elsewhen(wbRedirect.valid) {
    s3_prevLastHalfData := wbRedirect.halfData
    s3_prevLastHalfPc   := wbRedirect.halfPc
  }.elsewhen(mmioRedirect.valid) {
    s3_prevLastHalfData := 0.U
    s3_prevLastHalfPc   := 0.U.asTypeOf(PrunedAddr(VAddrBits))
  }.elsewhen(s3_fire) {
    s3_prevLastHalfData := s3_alignInstrData(s3_instrCount + s3_alignShiftNum)(15, 0)
    s3_prevLastHalfPc   := s3_alignPc(s3_instrCount + s3_alignShiftNum)
  }

  when(backendRedirect) {
    s3_prevIBufEnqPtr := 0.U.asTypeOf(new IBufPtr)
  }.elsewhen(wbRedirect.valid) {
    s3_prevIBufEnqPtr := wbRedirect.prevIBufEnqPtr + wbRedirect.instrCount
  }.elsewhen(mmioRedirect.valid) {
    s3_prevIBufEnqPtr := mmioRedirect.prevIBufEnqPtr + mmioRedirect.instrCount
  }.elsewhen(s3_fire) {
    s3_prevIBufEnqPtr := s3_prevIBufEnqPtr + s3_instrCount
  }

  // PreDecode: delimitation, does not expand RVC
  preDecoderIn.valid           := s3_valid
  preDecoderIn.bits.data       := s3_realAlignInstrData
  preDecoderIn.bits.isRvc      := s3_alignInstrIsRvc
  preDecoderIn.bits.instrValid := s3_alignInstrValid // s3_instrValid.asTypeOf(Vec(FetchBlockInstNum, Bool()))

  private val s3_alignPd         = preDecoderOut.pd
  private val s3_alignJumpOffset = preDecoderOut.jumpOffset

  XSPerfAccumulate("fetch_bubble_icache_not_resp", s3_valid && !icacheRespAllValid)

  private val s3_firstIsMmio = s3_valid && (s3_icacheInfo(0).pmpMmio || Pbmt.isUncache(s3_icacheInfo(0).itlbPbmt)) &&
    s3_icacheInfo(0).exception.isNone
  private val s3_secondIsMmio    = false.B
  private val s3_mmioResendVAddr = s3_firstResendVAddr
  private val s3_mmioFtqIdx      = s3_fetchBlock(0).ftqIdx

  assert(!s3_secondIsMmio, "MMIO and non-MMIO instructions cannot be processed in the same cycle")

  /* *****************************************************************************
   * IFU Stage 4
   * - handle MMIO instruction
   *   - send request to Uncache fetch Unit
   *   - every packet include 1 MMIO instruction
   *   - MMIO instructions will stop fetch pipeline until commiting from RoB
   *   - flush to snpc (send ifu_redirect to Ftq)
   * - IBuffer enqueue
   * - check predict result in Frontend (jalFault/retFault/notCFIFault/invalidTakenFault/targetFault)
   * - handle last half RVI instruction
   * ***************************************************************************** */

  // assign later
  private val s4_valid               = WireInit(false.B)
  private val s4_firstValid          = ValidHold(s3_fire && !s3_flush && s3_firstValid, s4_fire, s4_flush)
  private val s4_secondValid         = ValidHold(s3_fire && !s3_flush && s3_secondValid, s4_fire, s4_flush)
  private val s4_fetchBlock          = RegEnable(s3_fetchBlock, s3_fire)
  private val s4_doubleline          = RegEnable(s3_doubleline, s3_fire)
  private val s4_prevIBufEnqPtr      = RegEnable(s3_prevIBufEnqPtr, s3_fire)
  private val s4_rawIndex            = RegEnable(s3_rawIndex, s3_fire)
  private val s4_prevShiftSelect     = RegEnable(s3_prevShiftSelect, s3_fire)
  private val s4_prevLastRvi         = RegEnable(s3_prevLastIsHalfRvi, s3_fire)
  private val s4_currentLastHalfData = RegEnable(s3_alignInstrData(s3_instrCount + s3_alignShiftNum)(15, 0), s3_fire)
  private val s4_currentLastHalfRvi  = RegEnable(s3_currentLastHalfRvi, s3_fire)
  s4_fire := io.toIBuffer.fire

  private val s4_alignInvalidTaken = RegEnable(s3_alignInvalidTaken, s3_fire)
  private val s4_alignIsPredTaken  = RegEnable(s3_alignIsPredTaken, s3_fire)
  private val s4_alignSelectBlock  = RegEnable(s3_alignSelectBlock, s3_fire)
  private val s4_alignInstrData    = RegEnable(s3_realAlignInstrData, s3_fire)
  private val s4_alignInstrValid   = RegEnable(s3_alignInstrValid, s3_fire)
  private val s4_firstIsMmio       = RegEnable(s3_firstIsMmio, s3_fire)

  private val s4_mmioResendVAddr = RegEnable(s3_mmioResendVAddr, s3_fire)
  private val s4_mmioFtqIdx      = RegEnable(s3_mmioFtqIdx, s3_fire)

  private val s4_icacheInfo = RegEnable(s3_icacheInfo, s3_fire)

  rvcExpanders.zipWithIndex.foreach { case (expander, i) =>
    expander.io.in      := s4_alignInstrData(i)
    expander.io.fsIsOff := io.csrFsIsOff
  }
  // Use expanded instruction only when input is legal.
  // Otherwise, use origin illegal RVC instruction.
  private val s4_alignExpdInstr = VecInit(rvcExpanders.map { expander: RvcExpander =>
    Mux(expander.io.ill, expander.io.in, expander.io.out.bits)
  })
  private val s4_alignIll = VecInit(rvcExpanders.map(_.io.ill))

  private val s4_alignPdWire         = RegEnable(s3_alignPd, s3_fire)
  private val s4_alignPds            = WireInit(s4_alignPdWire)
  private val s4_alignJumpOffset     = RegEnable(s3_alignJumpOffset, s3_fire)
  private val s4_alignInstrPcLower   = RegEnable(s3_alignInstrPcLower, s3_fire)
  private val s4_alignInstrEndOffset = RegEnable(s3_alignInstrEndOffset, s3_fire)
  private val s4_rawPcLowerResult    = RegEnable(s3_rawPcLowerResult, s3_fire)
  private val s4_alignIdentifiedCfi  = RegEnable(s3_alignIdentifiedCfi, s3_fire)

  private val s4_alignPc = VecInit.tabulate(IBufferEnqueueWidth)(i =>
    if (i < IfuAlignWidth) {
      RegEnable(s3_realAlignPc(i), s3_fire)
    } else {
      catPC(
        s4_alignInstrPcLower(i),
        Mux(s4_alignSelectBlock(i), s4_fetchBlock(1).pcHigh, s4_fetchBlock(0).pcHigh),
        Mux(s4_alignSelectBlock(i), s4_fetchBlock(1).pcHighPlus1, s4_fetchBlock(0).pcHighPlus1)
      )
    }
  )

  private val s4_alignFoldPc        = RegEnable(s3_alignFoldPc, s3_fire)
  private val s4_rawInstrEndVec     = RegEnable(s3_rawInstrEndVec, s3_fire)
  private val s4_prevLastIsHalfRvi  = RegEnable(s3_prevLastIsHalfRvi, s3_fire)
  private val s4_mmioLowerPc        = RegEnable(s3_alignInstrPcLower(s3_alignShiftNum), s3_fire)
  private val s4_alignBlockStartPos = RegEnable(s3_alignBlockStartPos, s3_fire)
  private val s4_mmioPc             = catPC(s4_mmioLowerPc, s4_fetchBlock(0).pcHigh, s4_fetchBlock(0).pcHighPlus1)

  // Exapnd 1 bit to prevent overflow when assert
  private val s4_fetchStartAddr = VecInit.tabulate(FetchPorts)(i =>
    PrunedAddrInit(Cat(0.U(1.W), s4_fetchBlock(i).startVAddr.toUInt))
  )
  private val s4_fetchNextStartAddr = VecInit.tabulate(FetchPorts)(i =>
    PrunedAddrInit(Cat(0.U(1.W), s4_fetchBlock(i).target.toUInt))
  )
  for (i <- 0 until FetchPorts) {
    when(s4_valid && !s4_fetchBlock(i).takenCfiOffset.valid) {
      assert(
        s4_fetchStartAddr(i) + (2 * FetchBlockInstNum).U >= s4_fetchNextStartAddr(i),
        s"More than ${2 * FetchBlockInstNum} Bytes fetch is not allowed!"
      )
    }
  }

  /* *** mmio *** */
  private def nMmioFsmState = 11
  private object MmioFsmState extends EnumUInt(nMmioFsmState) {
    def Idle:           UInt = 0.U(width.W)
    def WaitLastCommit: UInt = 1.U(width.W)
    def SendReq:        UInt = 2.U(width.W)
    def WaitResp:       UInt = 3.U(width.W)
    def SendTlb:        UInt = 4.U(width.W)
    def TlbResp:        UInt = 5.U(width.W)
    def SendPmp:        UInt = 6.U(width.W)
    def ResendReq:      UInt = 7.U(width.W)
    def WaitResendResp: UInt = 8.U(width.W)
    def WaitCommit:     UInt = 9.U(width.W)
    def Commited:       UInt = 10.U(width.W)
  }

  private val mmioState = RegInit(MmioFsmState.Idle)

  private val mmioData       = RegInit(VecInit(Seq.fill(2)(0.U(16.W))))
  private val mmioException  = RegInit(ExceptionType.None)
  private val mmioIsRvc      = RegInit(false.B)
  private val mmioHasResend  = RegInit(false.B)
  private val mmioResendAddr = RegInit(PrunedAddrInit(0.U(PAddrBits.W)))
  // NOTE: we don't use GPAddrBits here, refer to ICacheMainPipe.scala L43-48 and PR#3795
  private val mmioResendGpAddr            = RegInit(PrunedAddrInit(0.U(PAddrBitsMax.W)))
  private val mmioResendIsForVSnonLeafPTE = RegInit(false.B)

  private def mmioReset(): Unit = {
    mmioState := MmioFsmState.Idle
    mmioData.foreach(_ := 0.U)
    mmioException               := ExceptionType.None
    mmioIsRvc                   := false.B
    mmioHasResend               := false.B
    mmioResendAddr              := PrunedAddrInit(0.U(PAddrBits.W))
    mmioResendGpAddr            := PrunedAddrInit(0.U(PAddrBitsMax.W))
    mmioResendIsForVSnonLeafPTE := false.B
  }

  // last instruction finish
  private val isFirstInstr = RegInit(true.B)
  private val s4_reqIsMmio = s4_firstIsMmio && s4_valid

  /* Determine whether the MMIO instruction is executable based on the previous prediction block */
  io.toFtq.mmioCommitRead.mmioFtqPtr := RegNext(s4_mmioFtqIdx - 1.U)
  private val mmioCommit =
    VecInit(io.robCommits.map(commit => commit.valid && commit.bits.ftqIdx === s4_mmioFtqIdx)).asUInt.orR
  private val s4_mmioReqCommit      = s4_reqIsMmio && mmioState === MmioFsmState.Commited
  private val s4_mmioWaitCommit     = s4_reqIsMmio && mmioState === MmioFsmState.WaitCommit
  private val s4_mmioWaitCommitNext = RegNext(s4_mmioWaitCommit)
  private val s4_mmioCanGo          = s4_mmioWaitCommit && !s4_mmioWaitCommitNext

  private val fromFtqRedirectReg = Wire(fromFtq.redirect.cloneType)
  fromFtqRedirectReg.bits := RegEnable(
    fromFtq.redirect.bits,
    0.U.asTypeOf(fromFtq.redirect.bits),
    fromFtq.redirect.valid
  )
  fromFtqRedirectReg.valid := RegNext(fromFtq.redirect.valid, init = false.B)
  private val mmioF4Flush     = RegNext(s4_flush, init = false.B)
  private val s4_ftqFlushSelf = fromFtqRedirectReg.valid && RedirectLevel.flushItself(fromFtqRedirectReg.bits.level)
  private val s4_ftqFlushByOlder =
    fromFtqRedirectReg.valid && isBefore(fromFtqRedirectReg.bits.ftqIdx, s4_mmioFtqIdx)

  private val s4_needNotFlush = s4_reqIsMmio && fromFtqRedirectReg.valid && !s4_ftqFlushSelf && !s4_ftqFlushByOlder

  /* We want to defer instruction fetching when encountering MMIO instructions
   * to ensure that the MMIO region is not negatively impacted. (no speculative fetch in MMIO region)
   * This is the exception when the first instruction is an MMIO instruction.
   */
  when(isFirstInstr && s4_fire) {
    isFirstInstr := false.B
  }

  s4_valid := ValidHold(
    // infire: s3 -> s4 fire
    s3_fire && !s3_flush,
    // outfire: if req is mmio, wait for commit, else wait for IBuffer
    Mux(s4_reqIsMmio, s4_mmioReqCommit, io.toIBuffer.fire),
    // flush: if req is mmio, check whether mmio Fsm allow flush, else flush directly
    Mux(s4_reqIsMmio, mmioF4Flush && !s4_needNotFlush, s4_flush)
  )
  dontTouch(s4_valid)

  private val (redirectFtqIdx, redirectFtqOffset) =
    (fromFtqRedirectReg.bits.ftqIdx, fromFtqRedirectReg.bits.ftqOffset)
  private val redirectMmioReq = fromFtqRedirectReg.valid && redirectFtqIdx === s4_mmioFtqIdx

  private val s4_mmioUseSnpc = ValidHold(RegNext(s3_fire && !s3_flush) && s4_reqIsMmio, redirectMmioReq)

  s4_ready := (io.toIBuffer.ready && (s4_mmioReqCommit || !s4_reqIsMmio)) || !s4_valid

  // mmio state machine
  switch(mmioState) {
    is(MmioFsmState.Idle) {
      when(s4_reqIsMmio) {
        // in idempotent spaces, we can send request directly (i.e. can do speculative fetch)
        mmioState := Mux(s4_icacheInfo(0).itlbPbmt === Pbmt.nc, MmioFsmState.SendReq, MmioFsmState.WaitLastCommit)
      }
    }

    is(MmioFsmState.WaitLastCommit) {
      when(isFirstInstr) {
        mmioState := MmioFsmState.SendReq
      }.otherwise {
        mmioState := Mux(io.toFtq.mmioCommitRead.mmioLastCommit, MmioFsmState.SendReq, MmioFsmState.WaitLastCommit)
      }
    }

    is(MmioFsmState.SendReq) {
      mmioState := Mux(toUncache.fire, MmioFsmState.WaitResp, MmioFsmState.SendReq)
    }

    is(MmioFsmState.WaitResp) {
      when(fromUncache.fire) {
        val respIsRVC = isRVC(fromUncache.bits.data(1, 0))
        val exception = ExceptionType(hasAf = fromUncache.bits.corrupt)
        // when response is not RVC, and lower bits of pAddr is 6 => request crosses 8B boundary, need resend
        val needResend = !respIsRVC && s4_icacheInfo(0).pAddr(2, 1) === 3.U && exception.isNone
        mmioState     := Mux(needResend, MmioFsmState.SendTlb, MmioFsmState.WaitCommit)
        mmioException := exception
        mmioIsRvc     := respIsRVC
        mmioHasResend := needResend
        mmioData(0)   := fromUncache.bits.data(15, 0)
        mmioData(1)   := fromUncache.bits.data(31, 16)
      }
    }

    is(MmioFsmState.SendTlb) {
      mmioState := Mux(io.itlb.req.fire, MmioFsmState.TlbResp, MmioFsmState.SendTlb)
    }

    is(MmioFsmState.TlbResp) {
      when(io.itlb.resp.fire) {
        // we are using a blocked tlb, so resp.fire must have !resp.bits.miss
        assert(!io.itlb.resp.bits.miss, "blocked mode iTLB miss when resp.fire")
        val itlbException = ExceptionType.fromTlbResp(io.itlb.resp.bits)
        // if itlb re-check respond pbmt mismatch with previous check, must be access fault
        val pbmtMismatchException = ExceptionType(hasAf = io.itlb.resp.bits.pbmt(0) =/= s4_icacheInfo(0).itlbPbmt)
        // merge, itlbException has higher priority
        val exception = itlbException || pbmtMismatchException
        // if tlb has exception, abort checking pmp, just send instr & exception to iBuffer and wait for commit
        mmioState := Mux(exception.hasException, MmioFsmState.WaitCommit, MmioFsmState.SendPmp)
        // also save itlb response
        mmioException               := exception
        mmioResendAddr              := io.itlb.resp.bits.paddr(0)
        mmioResendGpAddr            := io.itlb.resp.bits.gpaddr(0)(PAddrBitsMax - 1, 0)
        mmioResendIsForVSnonLeafPTE := io.itlb.resp.bits.isForVSnonLeafPTE(0)
      }
    }

    is(MmioFsmState.SendPmp) {
      val pmpException = ExceptionType.fromPmpResp(io.pmp.resp)
      // if pmp re-check respond mismatch with previous check, must be access fault
      val mmioMismatchException = ExceptionType(hasAf = io.pmp.resp.mmio =/= s4_icacheInfo(0).pmpMmio)
      // merge, pmpException has higher priority
      val exception = pmpException || mmioMismatchException
      // if pmp has exception, abort sending request, just send instr & exception to iBuffer and wait for commit
      mmioState := Mux(exception.hasException, MmioFsmState.WaitCommit, MmioFsmState.ResendReq)
      // also save pmp response
      mmioException := exception
    }

    is(MmioFsmState.ResendReq) {
      mmioState := Mux(toUncache.fire, MmioFsmState.WaitResendResp, MmioFsmState.ResendReq)
    }

    is(MmioFsmState.WaitResendResp) {
      when(fromUncache.fire) {
        mmioState     := MmioFsmState.WaitCommit
        mmioException := ExceptionType(hasAf = fromUncache.bits.corrupt)
        mmioData(1)   := fromUncache.bits.data(15, 0)
      }
    }

    is(MmioFsmState.WaitCommit) {
      // in idempotent spaces, we can skip waiting for commit (i.e. can do speculative fetch)
      // but we do not skip MmioFsmState.WaitCommit state, as other signals (e.g. s3_mmioCanGo relies on this)
      mmioState := Mux(
        mmioCommit || s4_icacheInfo(0).itlbPbmt === Pbmt.nc,
        MmioFsmState.Commited,
        MmioFsmState.WaitCommit
      )
    }

    // normal mmio instruction
    is(MmioFsmState.Commited) {
      mmioReset() // includes mmioState := MmioFsmState.Idle
    }
  }

  // Exception or flush by older branch prediction
  // Condition is from RegNext(fromFtq.redirect), 1 cycle after backend redirect
  when(s4_ftqFlushSelf || s4_ftqFlushByOlder) {
    mmioReset()
  }

  toUncache.valid := ((mmioState === MmioFsmState.SendReq) || (mmioState === MmioFsmState.ResendReq)) &&
    s4_reqIsMmio && s4_valid

  toUncache.bits.addr := Mux(mmioState === MmioFsmState.ResendReq, mmioResendAddr, s4_icacheInfo(0).pAddr)
  // if !pmp_mmio, then we're actually sending a MMIO request to main memory, it must be pbmt.nc/io
  // we need to tell L2 Cache about this to make it work correctly
  toUncache.bits.memBackTypeMM := !s4_icacheInfo(0).pmpMmio
  toUncache.bits.memPageTypeNC := s4_icacheInfo(0).itlbPbmt === Pbmt.nc

  fromUncache.ready := true.B

  // send itlb request in MmioFsmState.SendTlb state
  io.itlb.req.valid                   := (mmioState === MmioFsmState.SendTlb) && s4_reqIsMmio
  io.itlb.req.bits.size               := 3.U
  io.itlb.req.bits.vaddr              := s4_mmioResendVAddr.toUInt
  io.itlb.req.bits.debug.pc           := s4_mmioResendVAddr.toUInt
  io.itlb.req.bits.cmd                := TlbCmd.exec
  io.itlb.req.bits.isPrefetch         := false.B
  io.itlb.req.bits.kill               := false.B // IFU use itlb for mmio, doesn't need sync, set it to false
  io.itlb.req.bits.no_translate       := false.B
  io.itlb.req.bits.fullva             := 0.U
  io.itlb.req.bits.checkfullva        := false.B
  io.itlb.req.bits.hyperinst          := DontCare
  io.itlb.req.bits.hlvx               := DontCare
  io.itlb.req.bits.memidx             := DontCare
  io.itlb.req.bits.debug.robIdx       := DontCare
  io.itlb.req.bits.debug.isFirstIssue := DontCare
  io.itlb.req.bits.pmp_addr           := DontCare
  // what's the difference between req_kill and req.bits.kill?
  io.itlb.req_kill := false.B
  // wait for itlb response in MmioFsmState.TlbResp state
  io.itlb.resp.ready := (mmioState === MmioFsmState.TlbResp) && s4_reqIsMmio

  io.pmp.req.valid     := (mmioState === MmioFsmState.SendPmp) && s4_reqIsMmio
  io.pmp.req.bits.addr := mmioResendAddr.toUInt
  io.pmp.req.bits.size := 3.U
  io.pmp.req.bits.cmd  := TlbCmd.exec

  private val s4_mmioRange = VecInit((0 until FetchBlockInstNum).map(i => if (i == 0) true.B else false.B))
  // private val s4_alignRealInstrValid  = Wire(Vec(IBufferEnqueueWidth, Bool()))
  private val s4_ignore = s4_prevShiftSelect

  /* ** prediction result check ** */
  checkerIn.valid                := s4_valid
  checkerIn.bits.instrJumpOffset := s4_alignJumpOffset
  checkerIn.bits.instrValid      := s4_alignInstrValid.asTypeOf(Vec(IBufferEnqueueWidth, Bool()))
  checkerIn.bits.instrPds        := s4_alignPds
  checkerIn.bits.instrPc         := s4_alignPc
  checkerIn.bits.isPredTaken     := s4_alignIsPredTaken
  checkerIn.bits.ignore          := s4_ignore.asBools
  checkerIn.bits.shiftNum        := s4_prevIBufEnqPtr.value(1, 0)

  checkerIn.bits.firstPredTakenIdx.valid := s4_fetchBlock(0).predTakenIdx.valid
  checkerIn.bits.firstPredTakenIdx.bits :=
    Cat(0.U(1.W), s4_fetchBlock(0).predTakenIdx.bits) + s4_prevIBufEnqPtr.value(1, 0)
  checkerIn.bits.secondPredTakenIdx.valid := s4_fetchBlock(1).predTakenIdx.valid
  checkerIn.bits.secondPredTakenIdx.bits :=
    Cat(0.U(1.W), s4_fetchBlock(1).predTakenIdx.bits) + s4_prevIBufEnqPtr.value(1, 0)
  checkerIn.bits.firstTarget      := s4_fetchBlock(0).target
  checkerIn.bits.secondTarget     := s4_fetchBlock(1).target
  checkerIn.bits.selectFetchBlock := s4_alignSelectBlock
  checkerIn.bits.invalidTaken     := s4_alignInvalidTaken
  checkerIn.bits.instrEndOffset   := s4_alignInstrEndOffset

  /* ** frontend Trigger  ** */
  frontendTrigger.io.pds             := s4_alignPds
  frontendTrigger.io.pc              := s4_alignPc
  frontendTrigger.io.data            := 0.U.asTypeOf(Vec(IBufferEnqueueWidth + 1, UInt(16.W))) // s4_noBubbleInstrData
  frontendTrigger.io.frontendTrigger := io.frontendTrigger
  private val s4_alignTriggered = frontendTrigger.io.triggered

  /* ** send to IBuffer ** */
  private val s4_toIBufferValid = s4_valid && (!s4_reqIsMmio || s4_mmioCanGo) && !s4_flush

  private val ignoreRange = Cat(Fill(IBufferEnqueueWidth - IfuAlignWidth, 1.U(1.W)), ~s4_ignore)
  io.toIBuffer.valid          := s4_toIBufferValid
  io.toIBuffer.bits.instrs    := s4_alignExpdInstr
  io.toIBuffer.bits.valid     := s4_alignInstrValid.asUInt & ignoreRange
  io.toIBuffer.bits.enqEnable := checkerOutStage1.fixedTwoFetchRange.asUInt & s4_alignInstrValid.asUInt & ignoreRange
  io.toIBuffer.bits.pd        := s4_alignPds
  io.toIBuffer.bits.ftqPtr    := s4_fetchBlock(0).ftqIdx
  io.toIBuffer.bits.pc        := s4_alignPc
  io.toIBuffer.bits.prevIBufEnqPtr := s4_prevIBufEnqPtr
  // Find last using PriorityMux
  io.toIBuffer.bits.isLastInFtqEntry := Reverse(PriorityEncoderOH(Reverse(io.toIBuffer.bits.enqEnable))).asBools
  io.toIBuffer.bits.instrEndOffset.zipWithIndex.foreach { case (a, i) =>
    a.taken  := checkerOutStage1.fixedTwoFetchTaken(i) && !s4_reqIsMmio
    a.offset := s4_alignInstrEndOffset(i)
  }
  io.toIBuffer.bits.foldpc := s4_alignFoldPc
  // mark the exception only on first instruction
  // TODO: store only the first exception in IBuffer, instead of store in every entry
  io.toIBuffer.bits.exceptionType := (0 until IBufferEnqueueWidth).map {
    i => Mux(i.U === s4_prevIBufEnqPtr.value(1, 0), s4_icacheInfo(0).exception, ExceptionType.None)
  }
  // backendException only needs to be set for the first instruction.
  // Other instructions in the same block may have pf or af set,
  // which is a side effect of the first instruction and actually not necessary.
  io.toIBuffer.bits.backendException := (0 until IBufferEnqueueWidth).map {
    i => i.U === s4_prevIBufEnqPtr.value(1, 0) && s4_icacheInfo(0).isBackendException
  }
  // if we have last half RV-I instruction, and has exception, we need to tell backend to caculate the correct pc
  io.toIBuffer.bits.crossPageIPFFix := (0 until IBufferEnqueueWidth).map {
    i => i.U === s4_prevIBufEnqPtr.value(1, 0) && s4_icacheInfo(0).exception.hasException && s4_prevLastIsHalfRvi
  }
  io.toIBuffer.bits.illegalInstr  := s4_alignIll
  io.toIBuffer.bits.triggered     := s4_alignTriggered
  io.toIBuffer.bits.identifiedCfi := s4_alignIdentifiedCfi

  when(io.toIBuffer.valid && io.toIBuffer.ready) {
    val enqVec = io.toIBuffer.bits.enqEnable
    val allocateSeqNum = VecInit((0 until IBufferEnqueueWidth).map { i =>
      val idx  = PopCount(enqVec.take(i + 1))
      val pc   = s4_alignPc(i).toUInt
      val code = io.toIBuffer.bits.instrs(i)
      PerfCCT.createInstMetaAtFetch(idx, pc, code, enqVec(i), clock, reset)
    })
    io.toIBuffer.bits.debug_seqNum.zipWithIndex.foreach { case (a, i) =>
      a := allocateSeqNum(i)
    }
  }.otherwise {
    io.toIBuffer.bits.debug_seqNum.zipWithIndex.foreach { case (a, i) =>
      a := 0.U
    }
  }

  /** to backend */
  // s4_gpAddr is valid iff gpf is detected
  io.toBackend.gpAddrMem.wen := s4_toIBufferValid && Mux(
    s4_reqIsMmio,
    mmioException.isGpf,
    s4_icacheInfo(0).exception.isGpf
  )
  io.toBackend.gpAddrMem.waddr        := s4_fetchBlock(0).ftqIdx.value
  io.toBackend.gpAddrMem.wdata.gpaddr := Mux(s4_reqIsMmio, mmioResendGpAddr.toUInt, s4_icacheInfo(0).gpAddr.toUInt)
  io.toBackend.gpAddrMem.wdata.isForVSnonLeafPTE := Mux(
    s4_reqIsMmio,
    mmioResendIsForVSnonLeafPTE,
    s4_icacheInfo(0).isForVSnonLeafPTE
  )

  // Write back to Ftq
  private val mmioFlushWb         = Wire(Valid(new PredecodeWritebackBundle))
  private val s4_mmioMisEndOffset = Wire(ValidUndirectioned(UInt(FetchBlockInstOffsetWidth.W)))
  s4_mmioMisEndOffset.valid := s4_reqIsMmio
  s4_mmioMisEndOffset.bits  := Mux(s4_prevLastIsHalfRvi || mmioIsRvc, 0.U, 1.U)

  // Send mmioFlushWb back to FTQ 1 cycle after uncache fetch return
  // When backend redirect, mmioState reset after 1 cycle.
  // In this case, mask .valid to avoid overriding backend redirect
  mmioFlushWb.valid := (s4_reqIsMmio && mmioState === MmioFsmState.WaitCommit && RegNext(fromUncache.fire) &&
    s4_mmioUseSnpc && !s4_ftqFlushSelf && !s4_ftqFlushByOlder)
  mmioFlushWb.bits.pc := s4_mmioPc
  mmioFlushWb.bits.pd := 0.U.asTypeOf(Vec(FetchBlockInstNum, new PreDecodeInfo))
  mmioFlushWb.bits.pd.zipWithIndex.foreach { case (instr, i) => instr.valid := s4_mmioRange(i) }
  mmioFlushWb.bits.ftqIdx         := s4_fetchBlock(0).ftqIdx
  mmioFlushWb.bits.takenCfiOffset := s4_fetchBlock(0).takenCfiOffset.bits
  mmioFlushWb.bits.misEndOffset   := s4_mmioMisEndOffset
  mmioFlushWb.bits.cfiEndOffset   := DontCare
  mmioFlushWb.bits.target     := Mux(mmioIsRvc, s4_fetchBlock(0).startVAddr + 2.U, s4_fetchBlock(0).startVAddr + 4.U)
  mmioFlushWb.bits.jalTarget  := DontCare
  mmioFlushWb.bits.instrRange := s4_mmioRange

  mmioRvcExpander.io.in      := Mux(s4_reqIsMmio, Cat(mmioData(1), mmioData(0)), 0.U)
  mmioRvcExpander.io.fsIsOff := io.csrFsIsOff

  private val s4_shiftNum = s4_prevIBufEnqPtr.value(1, 0)
  /* mmio pre-decode & send to IBuffer */
  when(s4_reqIsMmio) {
    val inst = Cat(mmioData(1), mmioData(0))

    val (brType, isCall, isRet) = getBrInfo(inst)

    io.toIBuffer.bits.instrs(s4_shiftNum) := Mux(
      mmioRvcExpander.io.ill,
      mmioRvcExpander.io.in,
      mmioRvcExpander.io.out.bits
    )

    io.toIBuffer.bits.pd(s4_shiftNum).valid              := true.B
    io.toIBuffer.bits.pd(s4_shiftNum).isRVC              := mmioIsRvc
    io.toIBuffer.bits.pd(s4_shiftNum).brType             := brType
    io.toIBuffer.bits.pd(s4_shiftNum).isCall             := isCall
    io.toIBuffer.bits.pd(s4_shiftNum).isRet              := isRet
    io.toIBuffer.bits.instrEndOffset(s4_shiftNum).offset := Mux(s4_prevLastIsHalfRvi || mmioIsRvc, 0.U, 1.U)

    io.toIBuffer.bits.exceptionType(s4_shiftNum) := mmioException
    // exception can happen in next page only when resend
    io.toIBuffer.bits.crossPageIPFFix(s4_shiftNum) := mmioHasResend && mmioException.hasException
    io.toIBuffer.bits.illegalInstr(s4_shiftNum)    := mmioRvcExpander.io.ill

    io.toIBuffer.bits.enqEnable := s4_alignBlockStartPos.asUInt // s4_mmioRange.asUInt

    mmioFlushWb.bits.pd(s4_shiftNum).valid  := true.B
    mmioFlushWb.bits.pd(s4_shiftNum).isRVC  := mmioIsRvc
    mmioFlushWb.bits.pd(s4_shiftNum).brType := brType
    mmioFlushWb.bits.pd(s4_shiftNum).isCall := isCall
    mmioFlushWb.bits.pd(s4_shiftNum).isRet  := isRet
  }

  mmioRedirect.valid := s4_reqIsMmio && mmioState === MmioFsmState.WaitCommit &&
    RegNext(fromUncache.fire) && s4_mmioUseSnpc

  mmioRedirect.instrCount     := 1.U
  mmioRedirect.prevIBufEnqPtr := s4_prevIBufEnqPtr
  mmioRedirect.isHalfInstr    := false.B
  mmioRedirect.halfPc         := 0.U.asTypeOf(PrunedAddr(VAddrBits))
  mmioRedirect.halfData       := 0.U
  XSPerfAccumulate("fetch_bubble_ibuffer_not_ready", io.toIBuffer.valid && !io.toIBuffer.ready)

  /* *****************************************************************************
   * IFU Write-back Stage
   * - write back preDecode information to Ftq to update
   * - redirect if found fault prediction
   * - redirect if false hit last half(last PC is not start + 32 Bytes, but in the middle of an notCFI RVI instruction)
   * ***************************************************************************** */

  // According to the discussed version, IFU will no longer need to send predecode information to FTQ in the future.
  // Therefore, this part of the logic will not be optimized further and will be removed later.
  private val firstRawPds  = WireDefault(VecInit.fill(FetchBlockInstNum)(0.U.asTypeOf(new PreDecodeInfo)))
  private val secondRawPds = WireDefault(VecInit.fill(FetchBlockInstNum)(0.U.asTypeOf(new PreDecodeInfo)))
  // firstRawPds.zipWithIndex.foreach {
  //   case (rawPd, i) =>
  //     rawPd := Mux(
  //       s4_rawInstrValid(i),
  //       s4_alignPds(s4_rawIndex(i) + s4_prevIBufEnqPtr.value(1, 0)),
  //       0.U.asTypeOf(new PreDecodeInfo)
  //     )
  // }
  // secondRawPds.zipWithIndex.foreach {
  //   case (rawPd, i) =>
  //     rawPd := Mux(
  //       s4_rawInstrValid(i.U + s4_fetchBlock(0).fetchSize),
  //       s4_alignPds(s4_rawIndex(i.U + s4_fetchBlock(0).fetchSize) + s4_prevIBufEnqPtr.value(1, 0)),
  //       0.U.asTypeOf(new PreDecodeInfo)
  //     )
  // }

  private val wbEnable              = RegNext(s3_fire && !s3_flush) && !s4_reqIsMmio && !s4_flush
  private val wbValid               = RegNext(wbEnable, init = false.B)
  private val wbFirstValid          = RegEnable(s4_firstValid, wbEnable)
  private val wbSecondValid         = RegEnable(s4_secondValid, wbEnable)
  private val wbFetchBlock          = RegEnable(s4_fetchBlock, wbEnable)
  private val wbPrevIBufEnqPtr      = RegEnable(s4_prevIBufEnqPtr, wbEnable)
  private val wbInstrCount          = RegEnable(PopCount(io.toIBuffer.bits.enqEnable), wbEnable)
  private val wbAlignInstrEndOffset = RegEnable(s4_alignInstrEndOffset, wbEnable)
  private val wbFirstRawPds         = RegEnable(firstRawPds, wbEnable)
  private val wbSecondRawPds        = RegEnable(secondRawPds, wbEnable)

  private val wbCurrentLastHalfData = RegEnable(s4_currentLastHalfData, wbEnable)
  // private val wbPrevLastHalfPc      = RegEnable(s4_prevLastHalfPc, wbEnable)
  private val wbCurrentLastRvi = RegEnable(s4_currentLastHalfRvi, wbEnable)

  s4_wbNotFlush := wbFetchBlock(0).ftqIdx === s4_fetchBlock(0).ftqIdx && s4_valid && wbValid

  wbStage2Check(0) := checkerOutStage2.fixedFirst
  wbStage2Check(1) := checkerOutStage2.fixedSecond

  private val wbAlignInstrPcLower = RegEnable(s4_alignInstrPcLower, wbEnable)

  private val wbInstrRange = wbStage2Check.zipWithIndex.map { case (check, i) =>
    check.instrRange & RegEnable(s4_fetchBlock(i).instrRange, wbEnable)
  }

  private val checkFlushWb = VecInit((0 until FetchPorts).map { i =>
    val b       = Wire(Valid(new PredecodeWritebackBundle))
    val missIdx = wbStage2Check(i).misIdx.bits
    b.valid               := wbValid && wbFirstValid // Primarily used as a placeholder; the value will be overwritten.
    b.bits.pd             := wbFirstRawPds           // Primarily used as a placeholder; the value will be overwritten.
    b.bits.pc             := catPC(wbAlignInstrPcLower(missIdx), wbFetchBlock(i).pcHigh, wbFetchBlock(i).pcHighPlus1)
    b.bits.ftqIdx         := wbFetchBlock(i).ftqIdx
    b.bits.takenCfiOffset := wbFetchBlock(i).takenCfiOffset.bits
    b.bits.misEndOffset.valid := wbStage2Check(i).misIdx.valid
    b.bits.misEndOffset.bits  := wbAlignInstrEndOffset(wbStage2Check(i).misIdx.bits)
    b.bits.cfiEndOffset.valid := wbStage2Check(i).cfiIdx.valid
    b.bits.cfiEndOffset.bits  := wbAlignInstrEndOffset(wbStage2Check(i).cfiIdx.bits)
    b.bits.target             := wbStage2Check(i).target
    b.bits.jalTarget          := wbStage2Check(i).target
    b.bits.instrRange         := wbInstrRange(i).asTypeOf(Vec(FetchBlockInstNum, Bool()))
    b
  })

  checkFlushWb(0).valid   := wbValid && wbFirstValid
  checkFlushWb(1).valid   := wbValid && wbSecondValid
  checkFlushWb(0).bits.pd := wbFirstRawPds
  checkFlushWb(1).bits.pd := wbSecondRawPds

  toFtq.pdWb(0) := Mux(wbValid, checkFlushWb(0), mmioFlushWb)
  toFtq.pdWb(1) := checkFlushWb(1)

  wbRedirect.valid := (checkFlushWb(0).bits.misEndOffset.valid && checkFlushWb(0).valid) ||
    (checkFlushWb(1).bits.misEndOffset.valid && checkFlushWb(1).valid)
  wbRedirect.isHalfInstr    := wbCurrentLastRvi && wbStage2Check(0).invalidTaken
  wbRedirect.instrCount     := wbInstrCount
  wbRedirect.prevIBufEnqPtr := wbPrevIBufEnqPtr
  wbRedirect.halfPc := catPC(
    wbAlignInstrPcLower(wbStage2Check(0).misIdx.bits),
    wbFetchBlock(0).pcHigh,
    wbFetchBlock(0).pcHighPlus1
  )
  wbRedirect.halfData := wbCurrentLastHalfData

  /* write back flush type */
  private val checkFaultType    = checkerOutStage2.perfFaultType
  private val checkJalFault     = WireDefault(VecInit.fill(FetchPorts)(false.B))
  private val checkJalrFault    = WireDefault(VecInit.fill(FetchPorts)(false.B))
  private val checkRetFault     = WireDefault(VecInit.fill(FetchPorts)(false.B))
  private val checkTargetFault  = WireDefault(VecInit.fill(FetchPorts)(false.B))
  private val checkNotCFIFault  = WireDefault(VecInit.fill(FetchPorts)(false.B))
  private val checkInvalidTaken = WireDefault(VecInit.fill(FetchPorts)(false.B))
  private val checkFetchValid   = WireDefault(VecInit.fill(FetchPorts)(false.B))
  checkFetchValid(0) := wbFirstValid
  checkFetchValid(1) := wbSecondValid

  for (i <- 0 until FetchPorts) {
    val validFetch = wbValid && checkFetchValid(i)
    checkJalFault(i)     := validFetch && (checkFaultType(i) === PreDecodeFaultType.JalFault)
    checkJalrFault(i)    := validFetch && (checkFaultType(i) === PreDecodeFaultType.JalrFault)
    checkRetFault(i)     := validFetch && (checkFaultType(i) === PreDecodeFaultType.RetFault)
    checkNotCFIFault(i)  := validFetch && (checkFaultType(i) === PreDecodeFaultType.NotCfiFault)
    checkInvalidTaken(i) := validFetch && (checkFaultType(i) === PreDecodeFaultType.InvalidTaken)
  }

  for (i <- 0 until FetchPorts) {
    XSPerfAccumulate(f"fetch${i}_predecode_flush_jalFault", checkJalFault(i))
    XSPerfAccumulate(f"fetch${i}_predecode_flush_jalrFault", checkJalrFault(i))
    XSPerfAccumulate(f"fetch${i}_predecode_flush_retFault", checkRetFault(i))
    XSPerfAccumulate(f"fetch${i}_predecode_flush_targetFault", checkTargetFault(i))
    XSPerfAccumulate(f"fetch${i}_predecode_flush_notCFIFault", checkNotCFIFault(i))
    XSPerfAccumulate(f"fetch${i}_predecode_flush_invalidTakenFault", checkInvalidTaken(i))

    XSDebug(
      checkRetFault(i),
      "fetch:%x  startAddr:%x  nextStartAddr:%x  taken:%d    takenIdx:%d\n",
      i.U,
      wbFetchBlock(i).startVAddr.toUInt,
      wbFetchBlock(i).target.toUInt,
      wbFetchBlock(i).takenCfiOffset.valid,
      wbFetchBlock(i).takenCfiOffset.bits
    )
  }
  /* *** Perf *** */
  private val s4_perfInfo = RegEnable(s3_perfInfo, s3_fire)
  val perfEvents: Seq[(String, Bool)] = Seq(
    ("frontendFlush                ", wbRedirect.valid),
    ("ifu_req                      ", io.toIBuffer.fire),
    ("ifu_miss                     ", io.toIBuffer.fire && !s4_perfInfo.hit),
    ("ifu_req_cacheline_0          ", io.toIBuffer.fire),
    ("ifu_req_cacheline_1          ", io.toIBuffer.fire && s4_perfInfo.isDoubleLine),
    ("ifu_req_cacheline_0_hit      ", io.toIBuffer.fire && s4_perfInfo.hit0),
    ("ifu_req_cacheline_1_hit      ", io.toIBuffer.fire && s4_perfInfo.hit1),
    ("only_0_hit                   ", io.toIBuffer.fire && s4_perfInfo.hit0NoReq1),
    ("only_0_miss                  ", io.toIBuffer.fire && s4_perfInfo.miss0NoReq1),
    ("hit_0_hit_1                  ", io.toIBuffer.fire && s4_perfInfo.hit0Hit1),
    ("hit_0_miss_1                 ", io.toIBuffer.fire && s4_perfInfo.hit0Miss1),
    ("miss_0_hit_1                 ", io.toIBuffer.fire && s4_perfInfo.miss0Hit1),
    ("miss_0_miss_1                ", io.toIBuffer.fire && s4_perfInfo.miss0Miss1)
  )
  generatePerfEvent()

  XSPerfAccumulate("frontendFlush", wbRedirect.valid)
  XSPerfAccumulate("ifu_req", io.toIBuffer.fire)
  XSPerfAccumulate("ifu_miss", io.toIBuffer.fire && !s4_perfInfo.hit)
  XSPerfAccumulate("ifu_req_cacheline_0", io.toIBuffer.fire)
  XSPerfAccumulate("ifu_req_cacheline_1", io.toIBuffer.fire && s4_perfInfo.isDoubleLine)
  XSPerfAccumulate("ifu_req_cacheline_0_hit", io.toIBuffer.fire && s4_perfInfo.hit0)
  XSPerfAccumulate("ifu_req_cacheline_1_hit", io.toIBuffer.fire && s4_perfInfo.hit1)
  XSPerfAccumulate("only_0_hit", io.toIBuffer.fire && s4_perfInfo.hit0NoReq1)
  XSPerfAccumulate("only_0_miss", io.toIBuffer.fire && s4_perfInfo.miss0NoReq1)
  XSPerfAccumulate("hit_0_hit_1", io.toIBuffer.fire && s4_perfInfo.hit0Hit1)
  XSPerfAccumulate("hit_0_miss_1", io.toIBuffer.fire && s4_perfInfo.hit0Miss1)
  XSPerfAccumulate("miss_0_hit_1", io.toIBuffer.fire && s4_perfInfo.miss0Hit1)
  XSPerfAccumulate("miss_0_miss_1", io.toIBuffer.fire && s4_perfInfo.miss0Miss1)
  XSPerfAccumulate("except", io.toIBuffer.fire && s4_perfInfo.except)
  XSPerfHistogram(
    "ifu2ibuffer_validCnt",
    PopCount(io.toIBuffer.bits.valid & io.toIBuffer.bits.enqEnable),
    io.toIBuffer.fire,
    0,
    FetchBlockInstNum + 1
  )

  // DB
  private val hartId                     = p(XSCoreParamsKey).HartId
  private val isWriteFetchToIBufferTable = Constantin.createRecord(s"isWriteFetchToIBufferTable$hartId")
  private val isWriteIfuWbToFtqTable     = Constantin.createRecord(s"isWriteIfuWbToFtqTable$hartId")
  private val fetchToIBufferTable        = ChiselDB.createTable(s"FetchToIBuffer$hartId", new FetchToIBufferDB)
  private val ifuWbToFtqTable            = ChiselDB.createTable(s"IfuWbToFtq$hartId", new IfuWbToFtqDB)

  private val fetchIBufferDumpData = Wire(new FetchToIBufferDB)
  fetchIBufferDumpData.startAddr(0) := s4_fetchBlock(0).startVAddr.toUInt
  fetchIBufferDumpData.startAddr(1) := s4_fetchBlock(1).startVAddr.toUInt
  fetchIBufferDumpData.instrCount   := PopCount(io.toIBuffer.bits.enqEnable)
  fetchIBufferDumpData.exception    := io.toIBuffer.fire && s4_perfInfo.except
  fetchIBufferDumpData.isCacheHit   := io.toIBuffer.fire && s4_perfInfo.hit

  private val ifuWbToFtqDumpData = Wire(new IfuWbToFtqDB)
  for (i <- 0 until FetchPorts) {
    ifuWbToFtqDumpData.startAddr(i)      := wbFetchBlock(i).startVAddr.toUInt
    ifuWbToFtqDumpData.isMissPred(i)     := checkFlushWb(i).bits.misEndOffset.valid
    ifuWbToFtqDumpData.missPredOffset(i) := checkFlushWb(i).bits.misEndOffset.bits
  }
  ifuWbToFtqDumpData.checkJalFault     := checkJalFault(0) | checkJalFault(1)
  ifuWbToFtqDumpData.checkJalrFault    := checkJalrFault(0) | checkJalrFault(1)
  ifuWbToFtqDumpData.checkRetFault     := checkRetFault(0) | checkRetFault(1)
  ifuWbToFtqDumpData.checkNotCFIFault  := checkNotCFIFault(0) | checkNotCFIFault(1)
  ifuWbToFtqDumpData.checkInvalidTaken := checkInvalidTaken(0) | checkInvalidTaken(1)
  ifuWbToFtqDumpData.checkTargetFault  := false.B

  fetchToIBufferTable.log(
    data = fetchIBufferDumpData,
    en = isWriteFetchToIBufferTable.orR && io.toIBuffer.fire,
    site = "IFU" + p(XSCoreParamsKey).HartId.toString,
    clock = clock,
    reset = reset
  )
  ifuWbToFtqTable.log(
    data = ifuWbToFtqDumpData,
    en = isWriteIfuWbToFtqTable.orR && wbValid,
    site = "IFU" + p(XSCoreParamsKey).HartId.toString,
    clock = clock,
    reset = reset
  )
}
