// Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
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
import utility.ParallelOR
import utility.ParallelPosteriorityEncoder
import utility.ParallelPriorityEncoder
import utility.PerfCCT
import utility.UIntToMask
import utility.ValidHold
import utility.XORFold
import utility.XSDebug
import utility.XSError
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
import xiangshan.frontend.IBufPtr
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
import xiangshan.frontend.icache.HasICacheParameters
import xiangshan.frontend.icache.PmpCheckBundle

class Ifu(implicit p: Parameters) extends IfuModule
    with HasICacheParameters
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
  private val preDecoder       = Module(new PreDecode)
  private val preDecodeBounder = Module(new PreDecodeBoundary)
  private val predChecker      = Module(new PredChecker)
  private val frontendTrigger  = Module(new FrontendTrigger)
  private val rvcExpanders     = Seq.fill(PredictWidth)(Module(new RvcExpander))
  private val mmioRvcExpander  = Module(new RvcExpander)

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
  io.toIBuffer.bits.topdown_info := topdownStages(numOfStage - 1)
  when(fromFtq.topdown_redirect.valid) {
    // only redirect from backend, IFU redirect itself is handled elsewhere
    when(fromFtq.topdown_redirect.bits.debugIsCtrl) {
      /*
      for (i <- 0 until numOfStage) {
        topdown_stages(i).reasons(TopDownCounters.ControlRedirectBubble.id) := true.B
      }
      io.toIBuffer.bits.topdown_info.reasons(TopDownCounters.ControlRedirectBubble.id) := true.B
       */
      when(fromFtq.topdown_redirect.bits.ControlBTBMissBubble) {
        for (i <- 0 until numOfStage) {
          topdownStages(i).reasons(TopDownCounters.BTBMissBubble.id) := true.B
        }
        io.toIBuffer.bits.topdown_info.reasons(TopDownCounters.BTBMissBubble.id) := true.B
      }.elsewhen(fromFtq.topdown_redirect.bits.TAGEMissBubble) {
        for (i <- 0 until numOfStage) {
          topdownStages(i).reasons(TopDownCounters.TAGEMissBubble.id) := true.B
        }
        io.toIBuffer.bits.topdown_info.reasons(TopDownCounters.TAGEMissBubble.id) := true.B
      }.elsewhen(fromFtq.topdown_redirect.bits.SCMissBubble) {
        for (i <- 0 until numOfStage) {
          topdownStages(i).reasons(TopDownCounters.SCMissBubble.id) := true.B
        }
        io.toIBuffer.bits.topdown_info.reasons(TopDownCounters.SCMissBubble.id) := true.B
      }.elsewhen(fromFtq.topdown_redirect.bits.ITTAGEMissBubble) {
        for (i <- 0 until numOfStage) {
          topdownStages(i).reasons(TopDownCounters.ITTAGEMissBubble.id) := true.B
        }
        io.toIBuffer.bits.topdown_info.reasons(TopDownCounters.ITTAGEMissBubble.id) := true.B
      }.elsewhen(fromFtq.topdown_redirect.bits.RASMissBubble) {
        for (i <- 0 until numOfStage) {
          topdownStages(i).reasons(TopDownCounters.RASMissBubble.id) := true.B
        }
        io.toIBuffer.bits.topdown_info.reasons(TopDownCounters.RASMissBubble.id) := true.B
      }
    }.elsewhen(fromFtq.topdown_redirect.bits.debugIsMemVio) {
      for (i <- 0 until numOfStage) {
        topdownStages(i).reasons(TopDownCounters.MemVioRedirectBubble.id) := true.B
      }
      io.toIBuffer.bits.topdown_info.reasons(TopDownCounters.MemVioRedirectBubble.id) := true.B
    }.otherwise {
      for (i <- 0 until numOfStage) {
        topdownStages(i).reasons(TopDownCounters.OtherRedirectBubble.id) := true.B
      }
      io.toIBuffer.bits.topdown_info.reasons(TopDownCounters.OtherRedirectBubble.id) := true.B
    }
  }

  /* *****************************************************************************
   * IFU Stage 0
   * - send cacheline fetch request to ICacheMainPipe
   * ***************************************************************************** */

  private val s0_ftqReq     = fromFtq.req.bits
  private val s0_doubleline = fromFtq.req.bits.crossCacheline
  s0_fire := fromFtq.req.fire

  private val s0_flushFromBpu = fromFtq.flushFromBpu.shouldFlushByStage2(s0_ftqReq.ftqIdx) ||
    fromFtq.flushFromBpu.shouldFlushByStage3(s0_ftqReq.ftqIdx)

  private val backendRedirect          = WireInit(false.B)
  private val wbRedirect, mmioRedirect = WireInit(0.U.asTypeOf(new IfuRedirectInternal))

  private val s4_wbNotFlush = WireInit(false.B)

  backendRedirect := fromFtq.redirect.valid
  s4_flush        := backendRedirect || (wbRedirect.valid && !s4_wbNotFlush)
  s3_flush        := backendRedirect || mmioRedirect.valid || wbRedirect.valid
  s2_flush        := s3_flush
  s1_flush        := s2_flush
  s0_flush        := s1_flush || s0_flushFromBpu

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
  XSPerfAccumulate("fetch_flush_s0_flush_from_bpu", s0_flushFromBpu)

  /* *****************************************************************************
   * IFU Stage 1
   * - calculate pc/half_pc/cut_ptr for every instruction
   * ***************************************************************************** */

  private val s1_valid      = ValidHold(s0_fire && !s0_flush, s1_fire, s1_flush)
  private val s1_ftqReq     = RegEnable(s0_ftqReq, s0_fire)
  private val s1_doubleline = RegEnable(s0_doubleline, s0_fire)

  s1_fire  := s1_valid && s2_ready
  s1_ready := s1_fire || !s1_valid

  assert(!(fromFtq.flushFromBpu.shouldFlushByStage3(s1_ftqReq.ftqIdx) && s1_valid))

  private val s1_firstFetchSize = Mux(
    s1_ftqReq.ftqOffset.valid,
    s1_ftqReq.ftqOffset.bits + 1.U(log2Ceil(PredictWidth + 1).W),
    (s1_ftqReq.nextStartVAddr - s1_ftqReq.startVAddr)(log2Ceil(PredictWidth + 1) + 1, 1)
  )
  private val s1_totalFetchSize = s1_firstFetchSize
  private val s1_firstEndPos    = s1_firstFetchSize - 1.U
  private val s1_totalEndPos    = s1_totalFetchSize - 1.U
  private val s1_firstJumpRange =
    Fill(PredictWidth, !s1_ftqReq.ftqOffset.valid) | Fill(PredictWidth, 1.U(1.W)) >> ~s1_ftqReq.ftqOffset.bits
  require(
    isPow2(PredictWidth),
    "If PredictWidth is not power of 2," +
      "expression: Fill(PredictWidth, 1.U(1.W)) >> ~s0_ftqReq.ftqOffset.bits is not right !!"
  )
  private val s1_firstFtrRange =
    Fill(PredictWidth, s1_ftqReq.ftqOffset.valid) | Fill(PredictWidth, 1.U(1.W)) >> ~getBasicBlockIdx(
      s1_ftqReq.nextStartVAddr,
      s1_ftqReq.startVAddr
    )
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
  private val s2_ftqReq            = RegEnable(s1_ftqReq, s1_fire)
  private val s2_doubleline        = RegEnable(s1_doubleline, s1_fire)
  private val s2_prevLastIsHalfRvi = RegInit(false.B)
  private val s2_firstFetchSize    = RegEnable(s1_firstFetchSize, s1_fire)
  private val s2_totalFetchSize    = RegEnable(s1_totalFetchSize, s1_fire)
  private val s2_firstEndPos       = RegEnable(s1_firstEndPos, s1_fire)
  private val s2_totalEndPos       = RegEnable(s1_totalEndPos, s1_fire)
  private val s2_firstJumpRange    = RegEnable(s1_firstJumpRange, s1_fire)
  private val s2_firstFtrRange     = RegEnable(s1_firstFtrRange, s1_fire)
  // Temporary compromise for V2 compatibility; actually uses RegEnable(s1_firstJumpRange & s1_firstFtrRange, s1_fire)
  private val s2_firstInstrRange  = RegEnable(s1_firstJumpRange & s1_firstFtrRange, s1_fire)
  private val s2_secondInstrRange = 0.U(PredictWidth.W)
  private val s2_totalInstrRange  = (s2_secondInstrRange << s2_firstFetchSize) | s2_firstInstrRange

  private val s2_instrPcLowerResult  = WireDefault(VecInit.fill(PredictWidth)(0.U((PcCutPoint + 1).W)))
  private val s2_bubblePcLowerResult = WireDefault(VecInit.fill(PredictWidth)(0.U((PcCutPoint + 1).W)))
  private val s2_instrIsRvc          = WireDefault(VecInit.fill(PredictWidth)(false.B))
  private val s2_instrOffset         = WireDefault(VecInit.fill(PredictWidth)(0.U(log2Ceil(PredictWidth).W)))

  s2_fire  := s2_valid && s3_ready && icacheRespAllValid
  s2_ready := s2_fire || !s2_valid

  // TODO: addr compare may be timing critical
  private val s2_iCacheAllRespWire =
    fromICache.valid &&
      fromICache.bits.vAddr(0) === s2_ftqReq.startVAddr &&
      (fromICache.bits.doubleline && fromICache.bits.vAddr(1) === s2_ftqReq.nextCachelineVAddr || !s2_doubleline)
  private val s2_iCacheAllRespReg = ValidHold(s2_valid && s2_iCacheAllRespWire && !s3_ready, s2_fire, s2_flush)

  icacheRespAllValid := s2_iCacheAllRespReg || s2_iCacheAllRespWire

  io.toICache.stall := !s3_ready

  private val s2_exceptionIn        = fromICache.bits.exception
  private val s2_isBackendException = fromICache.bits.isBackendException
  // paddr and gpAddr of [startAddr, nextLineAddr]
  private val s2_pAddr             = fromICache.bits.pAddr
  private val s2_gpAddr            = fromICache.bits.gpAddr
  private val s2_isForVSnonLeafPTE = fromICache.bits.isForVSnonLeafPTE

  // FIXME: raise af if one fetch block crosses the cacheable/un-cacheable boundary, might not correct
  private val s2_mmioMismatchException = VecInit(Seq(
    ExceptionType.None, // mark the exception only on the second line
    ExceptionType(hasAf =
      // if not double-line, skip check
      fromICache.bits.doubleline && (
        // is double-line, ask for consistent pmp_mmio and itlb_pbmt value
        fromICache.bits.pmpMmio(0) =/= fromICache.bits.pmpMmio(1) ||
          fromICache.bits.itlbPbmt(0) =/= fromICache.bits.itlbPbmt(1)
      )
    )
  ))

  // merge exceptions
  private val s2_exception = VecInit((s2_exceptionIn zip s2_mmioMismatchException).map { case (in, m) => in || m })

  // we need only the first port, as the second is asked to be the same
  private val s2_pmpMmio  = fromICache.bits.pmpMmio(0)
  private val s2_itlbPbmt = fromICache.bits.itlbPbmt(0)

  private val s2_rawData  = fromICache.bits.data
  private val s2_perfInfo = io.fromICache.perf
  preDecodeBounder.io.req.valid            := fromICache.valid
  preDecodeBounder.io.req.bits.instrRange  := s2_totalInstrRange.asTypeOf(Vec(PredictWidth, Bool()))
  preDecodeBounder.io.req.bits.cacheData   := Cat(s2_rawData, s2_rawData) >> Cat(s2_ftqReq.startVAddr(5, 0), 0.U(3.W))
  preDecodeBounder.io.req.bits.endPosition := s2_totalEndPos
  preDecodeBounder.io.req.bits.prevLastIsHalfRvi := s2_prevLastIsHalfRvi

  val s2_fetchEndIsHalf = preDecodeBounder.io.resp.bits.isLastHalfRvi
  when(s2_fire && !s2_flush) {
    s2_prevLastIsHalfRvi := preDecodeBounder.io.resp.bits.isLastHalfRvi && !s2_ftqReq.ftqOffset.valid
  }.elsewhen(s2_flush) {
    s2_prevLastIsHalfRvi := false.B
  }

  private val bubbleInstrValid = preDecodeBounder.io.resp.bits.instrValid
  private val isRvc            = preDecodeBounder.io.resp.bits.isRvc
  private val firstFtqOffset   = s2_ftqReq.ftqOffset

  /* *****************************************************************************
   * instrCountBeforeCurrent(i), not include bubbleInstrValid(i)
   * ***************************************************************************** */
  val instrCountBeforeCurrent = WireDefault(VecInit.fill(PredictWidth + 1)(0.U(log2Ceil(PredictWidth + 1).W)))
  for (i <- 0 until PredictWidth) {
    instrCountBeforeCurrent(i) := PopCount(bubbleInstrValid.take(i))
  }
  instrCountBeforeCurrent(PredictWidth) := PopCount(bubbleInstrValid)

  val instrIndexEntry = Wire(Vec(PredictWidth, new InstrIndexEntry))
  val fetchBlockSelect =
    VecInit.tabulate(PredictWidth)(i =>
      Mux(s2_firstFetchSize > i.U, false.B, false.B)
    )

  private val s2_firstFetchBlockPcLowerResult = VecInit((0 until PredictWidth).map(i =>
    Cat(0.U(1.W), s2_ftqReq.startVAddr(PcCutPoint - 1, 0)) + (i * 2).U
  )) // cat with overflow bit

  private val firstFetchBlockIndex =
    VecInit.tabulate(PredictWidth)(i => s2_firstFetchBlockPcLowerResult(i)(log2Ceil(ICacheLineBytes) - 1, 1))

  private val fetchBlockIndex = VecInit.tabulate(PredictWidth)(i =>
    Mux(s2_firstFetchSize > i.U, firstFetchBlockIndex(i), firstFetchBlockIndex(i))
  )

  private val fetchBlockPcLowerResult = VecInit.tabulate(PredictWidth)(i =>
    Mux(
      s2_firstFetchSize > i.U,
      s2_firstFetchBlockPcLowerResult(i),
      s2_firstFetchBlockPcLowerResult(i)
    ) // Mux(firstFetchBlockSize > i.U, s1_firstFetchBlockPcLowerResult(i), s1_secondFetchBlockPcLowerResult(i))
  )

  s2_bubblePcLowerResult := fetchBlockPcLowerResult

  private val instrSelectLowIndex   = WireDefault(VecInit.fill(PredictWidth)(true.B))
  private val instrSelectFetchBlock = WireDefault(VecInit.fill(PredictWidth)(false.B))

  instrIndexEntry.zipWithIndex.foreach {
    case (index, idx) =>
      if (idx < PredictWidth / 2) {
        val validOH = Range(idx, 2 * idx + 2).map {
          i => bubbleInstrValid(i) & (instrCountBeforeCurrent(i) === idx.U)
        }
        val computeIndex = Range(idx, 2 * idx + 2).map {
          i => fetchBlockIndex(i)
        }
        val computeSelect = Range(idx, 2 * idx + 2).map {
          i => fetchBlockSelect(i)
        }
        val computePcLowerResult = Range(idx, 2 * idx + 2).map {
          i => fetchBlockPcLowerResult(i)
        }
        val computIsRvc = Range(idx, 2 * idx + 2).map {
          i => isRvc(i)
        }
        val computeInstrOffset = Range(idx, 2 * idx + 2).map {
          i => i.U
        }

        index.valid                := validOH.reduce(_ || _)
        index.value                := Mux1H(validOH, computeIndex)
        instrSelectFetchBlock(idx) := Mux1H(validOH, computeSelect)
        s2_instrPcLowerResult(idx) := Mux1H(validOH, computePcLowerResult)
        s2_instrIsRvc(idx)         := Mux1H(validOH, computIsRvc)
        s2_instrOffset(idx)        := Mux1H(validOH, computeInstrOffset)
      } else {
        val validOH = Range(idx, PredictWidth).map {
          i => bubbleInstrValid(i) && (instrCountBeforeCurrent(i) === idx.U)
        }
        val computeIndex = Range(idx, PredictWidth).map {
          i => fetchBlockIndex(i)
        }
        val computeSelect = Range(idx, PredictWidth).map {
          i => fetchBlockSelect(i)
        }
        val computePcLowerResult = Range(idx, PredictWidth).map {
          i => fetchBlockPcLowerResult(i)
        }
        val computIsRvc = Range(idx, PredictWidth).map {
          i => isRvc(i)
        }
        val computeInstrOffset = Range(idx, PredictWidth).map {
          i => i.U
        }
        index.valid                := validOH.reduce(_ || _)
        index.value                := Mux1H(validOH, computeIndex)
        instrSelectFetchBlock(idx) := Mux1H(validOH, computeSelect)
        s2_instrPcLowerResult(idx) := Mux1H(validOH, computePcLowerResult)
        s2_instrIsRvc(idx)         := Mux1H(validOH, computIsRvc)
        s2_instrOffset(idx)        := Mux1H(validOH, computeInstrOffset)
      }
  }

  val s2_firstTakenIdx = WireDefault(0.U.asTypeOf(ValidUndirectioned(UInt(log2Ceil(PredictWidth).W))))
  s2_firstTakenIdx.valid := firstFtqOffset.valid
  s2_firstTakenIdx.bits := Mux(
    firstFtqOffset.valid,
    instrCountBeforeCurrent(firstFtqOffset.bits + 1.U(5.W)) - 1.U,
    PopCount(bubbleInstrValid.asUInt & s2_firstInstrRange) - 1.U
  )

  private val s2_firstBlock  = WireDefault(0.U.asTypeOf(new FetchBlockInfo))
  private val s2_secondBlock = WireDefault(0.U.asTypeOf(new FetchBlockInfo))
  s2_firstBlock.ftqIdx       := s2_ftqReq.ftqIdx
  s2_firstBlock.predTakenIdx := s2_firstTakenIdx
  s2_firstBlock.invalidTaken := !bubbleInstrValid(firstFtqOffset.bits) && firstFtqOffset.valid

  s2_firstBlock.instrRange := s2_firstInstrRange // Temporary compromise for V2 compatibility; actually uses s1_firstJumpRange & s1_firstFtrRange
  s2_firstBlock.pcHigh           := s2_ftqReq.startVAddr(VAddrBits - 1, PcCutPoint)
  s2_firstBlock.pcHighPlus1      := s2_ftqReq.startVAddr(VAddrBits - 1, PcCutPoint) + 1.U
  s2_firstBlock.target           := s2_ftqReq.nextStartVAddr
  s2_firstBlock.fetchSize        := s2_firstFetchSize
  s2_firstBlock.bubbleInstrValid := bubbleInstrValid.asUInt & s2_firstInstrRange

  s2_secondBlock.bubbleInstrValid := (bubbleInstrValid.asUInt >> s2_firstFetchSize) & s2_secondInstrRange
  private val s2_rawDataDupWire = VecInit(Seq.fill(2)(s2_rawData))

  /* *****************************************************************************
   * IFU Stage 3
   *
   * ***************************************************************************** */
  private val s3_valid          = ValidHold(s2_fire && !s2_flush, s3_fire, s3_flush)
  private val s3_ftqReq         = RegEnable(s2_ftqReq, s2_fire)
  private val s3_doubleline     = RegEnable(s2_doubleline, s2_fire)
  private val s3_prevIBufEnqPtr = RegInit(0.U.asTypeOf(new IBufPtr))

  s3_fire  := s3_valid && s4_ready
  s3_ready := s3_fire || !s3_valid

  private val s3_instrIndex       = RegEnable(instrIndexEntry, s2_fire)
  private val s3_selectFetchBlock = RegEnable(instrSelectFetchBlock, s2_fire)
  private val s3_instrIsRvc       = RegEnable(s2_instrIsRvc, s2_fire)
  private val s3_instrCount       = RegEnable(PopCount(bubbleInstrValid), s2_fire)
  private val s3_instrValid       = RegEnable(UIntToMask(PopCount(bubbleInstrValid), PredictWidth), s2_fire)

  private val s3_firstBlock        = RegEnable(s2_firstBlock, s2_fire)
  private val s3_secondBlock       = RegEnable(s2_secondBlock, s2_fire)
  private val s3_bubbleIndex       = RegEnable(instrCountBeforeCurrent, s2_fire)
  private val s3_bubbleInstrValid  = RegEnable(bubbleInstrValid, s2_fire)
  private val s3_prevLastIsHalfRvi = RegEnable(s2_prevLastIsHalfRvi, s2_fire)
  private val s3_prevLastHalfData  = RegInit(0.U(16.W))

  private val s3_rawDataDup         = RegEnable(s2_rawDataDupWire, s2_fire)
  private val s3_exception          = RegEnable(s2_exception, s2_fire)
  private val s3_isBackendException = RegEnable(s2_isBackendException, s2_fire)
  private val s3_pAddr              = RegEnable(s2_pAddr, s2_fire)
  private val s3_gpAddr             = RegEnable(s2_gpAddr, s2_fire)
  private val s3_isForVSnonLeafPTE  = RegEnable(s2_isForVSnonLeafPTE, s2_fire)
  private val s3_perfInfo           = RegEnable(s2_perfInfo, s2_fire)
  private val s3_pmpMmio            = RegEnable(s2_pmpMmio, s2_fire)
  private val s3_itlbPbmt           = RegEnable(s2_itlbPbmt, s2_fire)
  private val s3_instrOffset        = RegEnable(s2_instrOffset, s2_fire)
  private val s3_resendVAddr        = RegEnable(s2_ftqReq.startVAddr + 2.U, s2_fire)

  private val s3_instrPcLowerResult  = RegEnable(s2_instrPcLowerResult, s2_fire)
  private val s3_bubblePcLowerResult = RegEnable(s2_bubblePcLowerResult, s2_fire)
  private val s3_pc = VecInit.tabulate(PredictWidth)(i =>
    catPC(
      s3_instrPcLowerResult(i),
      Mux(s3_selectFetchBlock(i), s3_secondBlock.pcHigh, s3_firstBlock.pcHigh),
      Mux(s3_selectFetchBlock(i), s3_secondBlock.pcHighPlus1, s3_firstBlock.pcHighPlus1)
    )
  )
  private val s3_foldPc = VecInit(s3_pc.map(i => XORFold(i(VAddrBits - 1, 1), MemPredPCWidth)))

  private val s3_exceptionVec = VecInit((0 until PredictWidth).map(i =>
    MuxCase(
      ExceptionType.None,
      Seq(
        !isNextLine(s3_pc(i), s3_ftqReq.startVAddr)                   -> s3_exception(0),
        (isNextLine(s3_pc(i), s3_ftqReq.startVAddr) && s3_doubleline) -> s3_exception(1)
      )
    )
  ))

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

  private val s3_lowICacheData  = cutICacheData(s3_rawDataDup(0))
  private val s3_highICacheData = cutICacheData(s3_rawDataDup(1))

  private val noBubbleInstrData = WireDefault(VecInit.fill(PredictWidth)(0.U(32.W)))
  for (i <- 0 until PredictWidth / 2) {
    noBubbleInstrData(i)                    := s3_lowICacheData(s3_instrIndex(i).value)
    noBubbleInstrData(i + PredictWidth / 2) := s3_highICacheData(s3_instrIndex(i + PredictWidth / 2).value)
  }
  private val s3_realInstrData = WireDefault(VecInit.fill(PredictWidth)(0.U(32.W)))
  for (i <- 0 until PredictWidth) {
    s3_realInstrData(i) := noBubbleInstrData(i)
  }
  s3_realInstrData(0) := Mux(
    s3_prevLastIsHalfRvi,
    Cat(noBubbleInstrData(0)(15, 0), s3_prevLastHalfData),
    noBubbleInstrData(0)
  )
  when(s3_fire && !s3_flush) {
    s3_prevLastHalfData := s3_realInstrData(s3_instrCount - 1.U)(15, 0)
  }.elsewhen(s4_flush) {
    s3_prevLastHalfData := 0.U
  }

  when(backendRedirect) {
    s3_prevIBufEnqPtr := 0.U.asTypeOf(new IBufPtr)
  }.elsewhen(wbRedirect.valid) {
    s3_prevIBufEnqPtr := wbRedirect.prevIBufEnqPtr + wbRedirect.instrCount
  }.elsewhen(mmioRedirect.valid) {
    s3_prevIBufEnqPtr := mmioRedirect.prevIBufEnqPtr + mmioRedirect.instrCount
  }.elsewhen(s3_fire) {
    s3_prevIBufEnqPtr := s3_prevIBufEnqPtr + s3_instrCount - s3_prevLastIsHalfRvi
  }

  // PreDecode: delimitation, does not expand RVC
  preDecoderIn.valid           := s3_valid
  preDecoderIn.bits.data       := s3_realInstrData
  preDecoderIn.bits.isRvc      := s3_instrIsRvc
  preDecoderIn.bits.instrValid := s3_instrValid.asTypeOf(Vec(PredictWidth, Bool()))

  private val s3_instr      = preDecoderOut.instr
  private val s3_pd         = preDecoderOut.pd
  private val s3_jumpOffset = preDecoderOut.jumpOffset

  /* if there is a cross-page RVI instruction, and the former page has no exception,
   * whether it has exception is actually depends on the latter page
   */
  private val s3_crossPageExceptionVec = VecInit((0 until PredictWidth).map { i =>
    Mux(
      isLastInLine(s3_pc(i)) && !s3_pd(i).isRVC && s3_doubleline && s2_exception(0).isNone,
      s3_exception(1),
      ExceptionType.None
    )
  })
  XSPerfAccumulate("fetch_bubble_icache_not_resp", s3_valid && !icacheRespAllValid)

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
  private val s4_valid = WireInit(false.B)

  private val s4_ftqReq         = RegEnable(s3_ftqReq, s3_fire)
  private val s4_doubleline     = RegEnable(s3_doubleline, s3_fire)
  private val s4_prevIBufEnqPtr = RegEnable(s3_prevIBufEnqPtr, s3_fire)
  s4_fire := io.toIBuffer.fire

  private val invalidTaken = WireDefault(VecInit.fill(PredictWidth)(false.B))
  invalidTaken(s3_firstBlock.predTakenIdx.bits)  := s3_firstBlock.invalidTaken
  invalidTaken(s3_secondBlock.predTakenIdx.bits) := s3_secondBlock.invalidTaken

  private val s4_firstBlock       = RegEnable(s3_firstBlock, s3_fire)
  private val s4_secondBlock      = RegEnable(s3_secondBlock, s3_fire)
  private val s4_invalidTaken     = RegEnable(invalidTaken, s3_fire)
  private val s4_selectFetchBlock = RegEnable(s3_selectFetchBlock, s3_fire)
  private val s4_instrData        = RegEnable(s3_realInstrData, s3_fire)
  private val s4_bubbleIndex      = RegEnable(s3_bubbleIndex, s3_fire)
  private val s4_instrValid       = RegEnable(s3_instrValid, s3_fire)

  private val s4_exception          = RegEnable(s3_exception, s3_fire)
  private val s4_pmpMmio            = RegEnable(s3_pmpMmio, s3_fire)
  private val s4_itlbPbmt           = RegEnable(s3_itlbPbmt, s3_fire)
  private val s4_isBackendException = RegEnable(s3_isBackendException, s3_fire)

  rvcExpanders.zipWithIndex.foreach { case (expander, i) =>
    expander.io.in      := s4_instrData(i)
    expander.io.fsIsOff := io.csrFsIsOff
  }
  // Use expanded instruction only when input is legal.
  // Otherwise, use origin illegal RVC instruction.
  private val s4_expdInstr = VecInit(rvcExpanders.map { expander: RvcExpander =>
    Mux(expander.io.ill, expander.io.in, expander.io.out.bits)
  })
  private val s4_ill = VecInit(rvcExpanders.map(_.io.ill))

  private val s4_pdWire                = RegEnable(s3_pd, s3_fire)
  private val s4_pd                    = WireInit(s4_pdWire)
  private val s4_jumpOffset            = RegEnable(s3_jumpOffset, s3_fire)
  private val s4_exceptionVec          = RegEnable(s3_exceptionVec, s3_fire)
  private val s4_crossPageExceptionVec = RegEnable(s3_crossPageExceptionVec, s3_fire)
  private val s4_instrPcLowerResult    = RegEnable(s3_instrPcLowerResult, s3_fire)
  private val s4_bubblePcLowerResult   = RegEnable(s3_bubblePcLowerResult, s3_fire)

  private val s4_pc = VecInit.tabulate(PredictWidth)(i =>
    catPC(
      s4_instrPcLowerResult(i),
      Mux(s4_selectFetchBlock(i), s4_secondBlock.pcHigh, s4_firstBlock.pcHigh),
      Mux(s4_selectFetchBlock(i), s4_secondBlock.pcHighPlus1, s4_firstBlock.pcHighPlus1)
    )
  )

  private val s4_foldPc            = RegEnable(s3_foldPc, s3_fire)
  private val s4_pAddr             = RegEnable(s3_pAddr, s3_fire)
  private val s4_gpAddr            = RegEnable(s3_gpAddr, s3_fire)
  private val s4_isForVSnonLeafPTE = RegEnable(s3_isForVSnonLeafPTE, s3_fire)
  private val s4_resendVAddr       = RegEnable(s3_resendVAddr, s3_fire)
  private val s4_bubbleInstrValid  = RegEnable(s3_bubbleInstrValid, s3_fire)
  private val s4_prevLastIsHalfRvi = RegEnable(s3_prevLastIsHalfRvi, s3_fire)
  private val s4_instrOffset       = RegEnable(s3_instrOffset, s3_fire)

  // Expand 1 bit to prevent overflow when assert
  private val s4_ftqReqStartAddr     = PrunedAddrInit(Cat(0.U(1.W), s4_ftqReq.startVAddr.toUInt))
  private val s4_ftqReqNextStartAddr = PrunedAddrInit(Cat(0.U(1.W), s4_ftqReq.nextStartVAddr.toUInt))

  when(s4_valid && !s4_ftqReq.ftqOffset.valid) {
    assert(
      s4_ftqReqStartAddr + (2 * PredictWidth).U >= s4_ftqReqNextStartAddr,
      s"More than ${2 * PredictWidth} Bytes fetch is not allowed!"
    )
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

  /* Determine whether the MMIO instruction is executable based on the previous prediction block */
  io.toFtq.mmioCommitRead.mmioFtqPtr := RegNext(s4_ftqReq.ftqIdx - 1.U)

  // do mmio fetch only when pmp/pbmt shows it is a un-cacheable address and no exception occurs
  private val s4_reqIsMmio =
    s4_valid && (s4_pmpMmio || Pbmt.isUncache(s4_itlbPbmt)) && s3_exception.map(_.isNone).reduce(_ && _)
  private val mmioCommit = VecInit(io.robCommits.map { commit =>
    commit.valid && commit.bits.ftqIdx === s4_ftqReq.ftqIdx && commit.bits.ftqOffset === 0.U
  }).asUInt.orR
  private val s4_mmioReqCommit = s4_reqIsMmio && mmioState === MmioFsmState.Commited

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
    fromFtqRedirectReg.valid && isBefore(fromFtqRedirectReg.bits.ftqIdx, s4_ftqReq.ftqIdx)

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
  private val redirectMmioReq =
    fromFtqRedirectReg.valid && redirectFtqIdx === s4_ftqReq.ftqIdx && redirectFtqOffset === 0.U

  private val s4_mmioUseSnpc = ValidHold(RegNext(s3_fire && !s3_flush) && s4_reqIsMmio, redirectMmioReq)

  s4_ready := (io.toIBuffer.ready && (s4_mmioReqCommit || !s4_reqIsMmio)) || !s4_valid

  // mmio state machine
  switch(mmioState) {
    is(MmioFsmState.Idle) {
      when(s4_reqIsMmio) {
        // in idempotent spaces, we can send request directly (i.e. can do speculative fetch)
        mmioState := Mux(s4_itlbPbmt === Pbmt.nc, MmioFsmState.SendReq, MmioFsmState.WaitLastCommit)
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
        val needResend = !respIsRVC && s4_pAddr(0)(2, 1) === 3.U && exception.isNone
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
        val pbmtMismatchException = ExceptionType(hasAf = io.itlb.resp.bits.pbmt(0) =/= s3_itlbPbmt)
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
      val mmioMismatchException = ExceptionType(hasAf = io.pmp.resp.mmio =/= s3_pmpMmio)
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
      mmioState := Mux(mmioCommit || s4_itlbPbmt === Pbmt.nc, MmioFsmState.Commited, MmioFsmState.WaitCommit)
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

  toUncache.valid := ((mmioState === MmioFsmState.SendReq) || (mmioState === MmioFsmState.ResendReq)) && s4_reqIsMmio
  toUncache.bits.addr := Mux(mmioState === MmioFsmState.ResendReq, mmioResendAddr, s4_pAddr(0))
  fromUncache.ready   := true.B

  // send itlb request in MmioFsmState.SendTlb state
  io.itlb.req.valid                   := (mmioState === MmioFsmState.SendTlb) && s4_reqIsMmio
  io.itlb.req.bits.size               := 3.U
  io.itlb.req.bits.vaddr              := s4_resendVAddr.toUInt
  io.itlb.req.bits.debug.pc           := s4_resendVAddr.toUInt
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

  private val s4_mmioRange      = VecInit((0 until PredictWidth).map(i => if (i == 0) true.B else false.B))
  private val s4_realInstrValid = Wire(Vec(PredictWidth, Bool()))

  /* ** prediction result check ** */
  checkerIn.valid                      := s4_valid
  checkerIn.bits.instrJumpOffset       := s4_jumpOffset
  checkerIn.bits.instrValid            := s4_instrValid.asTypeOf(Vec(PredictWidth, Bool()))
  checkerIn.bits.instrPds              := s4_pd
  checkerIn.bits.instrPc               := s4_pc
  checkerIn.valid                      := RegNext(s3_fire, init = false.B)
  checkerIn.bits.tempPrevLastIsHalfRvi := s4_prevLastIsHalfRvi

  checkerIn.bits.firstFtqPredTakenIdx  := s4_firstBlock.predTakenIdx
  checkerIn.bits.secondFtqPredTakenIdx := s4_secondBlock.predTakenIdx
  checkerIn.bits.firstTarget           := s4_firstBlock.target
  checkerIn.bits.secondTarget          := s4_secondBlock.target
  checkerIn.bits.selectFetchBlock      := s4_selectFetchBlock
  checkerIn.bits.invalidTaken          := s4_invalidTaken
  checkerIn.bits.instrOffset           := s4_instrOffset

  s4_realInstrValid.zipWithIndex.map {
    case (valid, i) =>
      valid := s4_instrValid(i) & checkerOutStage1.fixedTwoFetchRange(i)
  }
  s4_realInstrValid(0) := s4_instrValid(0) & checkerOutStage1.fixedTwoFetchRange(0) && !s4_prevLastIsHalfRvi

  /* ** frontend Trigger  ** */
  frontendTrigger.io.pds             := s4_pd
  frontendTrigger.io.pc              := s4_pc
  frontendTrigger.io.data            := 0.U.asTypeOf(Vec(IBufEnqWidth + 1, UInt(16.W))) // s4_noBubbleInstrData
  frontendTrigger.io.frontendTrigger := io.frontendTrigger
  private val s4_triggered = frontendTrigger.io.triggered

  /* ** send to IBuffer ** */
  private val s4_toIBufferValid = s4_valid && (!s4_reqIsMmio || s4_mmioCanGo) && !s4_flush

  io.toIBuffer.valid               := s4_toIBufferValid
  io.toIBuffer.bits.instrs         := s4_expdInstr
  io.toIBuffer.bits.valid          := s4_realInstrValid.asUInt
  io.toIBuffer.bits.enqEnable      := checkerOutStage1.fixedTwoFetchRange.asUInt & s4_realInstrValid.asUInt
  io.toIBuffer.bits.pd             := s4_pd
  io.toIBuffer.bits.ftqPtr         := s4_ftqReq.ftqIdx
  io.toIBuffer.bits.pc             := s4_pc
  io.toIBuffer.bits.prevIBufEnqPtr := s4_prevIBufEnqPtr
  // Find last using PriorityMux
  io.toIBuffer.bits.isLastInFtqEntry := Reverse(PriorityEncoderOH(Reverse(io.toIBuffer.bits.enqEnable))).asBools
  io.toIBuffer.bits.ftqPcOffset.zipWithIndex.foreach { case (a, i) =>
    a.bits.borrow := false.B
    a.bits.offset := s4_instrOffset(i)
    a.valid       := checkerOutStage1.fixedTwoFetchTaken(i) && !s4_reqIsMmio
  }
  io.toIBuffer.bits.foldpc := s4_foldPc
  io.toIBuffer.bits.exceptionType := VecInit((s4_exceptionVec zip s4_crossPageExceptionVec).map { case (e, ce) =>
    e || ce // merge, cross page fix has lower priority
  })
  // backendException only needs to be set for the first instruction.
  // Other instructions in the same block may have pf or af set,
  // which is a side effect of the first instruction and actually not necessary.
  io.toIBuffer.bits.backendException := (0 until IBufEnqWidth).map {
    case 0 => s4_isBackendException
    case _ => false.B
  }
  io.toIBuffer.bits.crossPageIPFFix := s4_crossPageExceptionVec.map(_.hasException)
  io.toIBuffer.bits.illegalInstr    := s4_ill
  io.toIBuffer.bits.triggered       := s4_triggered

  when(io.toIBuffer.valid && io.toIBuffer.ready) {
    val enqVec = io.toIBuffer.bits.enqEnable
    val allocateSeqNum = VecInit((0 until IBufEnqWidth).map { i =>
      val idx  = PopCount(enqVec.take(i + 1))
      val pc   = s4_pc(i).toUInt
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
  io.toBackend.gpaddrMem_wen := s4_toIBufferValid && Mux(
    s4_reqIsMmio,
    mmioException.isGpf,
    s4_exception.map(_.isGpf).reduce(_ || _)
  )
  io.toBackend.gpaddrMem_waddr        := s4_ftqReq.ftqIdx.value
  io.toBackend.gpaddrMem_wdata.gpaddr := Mux(s4_reqIsMmio, mmioResendGpAddr.toUInt, s4_gpAddr.toUInt)
  io.toBackend.gpaddrMem_wdata.isForVSnonLeafPTE := Mux(
    s4_reqIsMmio,
    mmioResendIsForVSnonLeafPTE,
    s4_isForVSnonLeafPTE
  )

  // Write back to Ftq
  private val mmioFlushWb       = Wire(Valid(new PredecodeWritebackBundle))
  private val s4_mmioMissOffset = Wire(ValidUndirectioned(UInt(log2Ceil(PredictWidth).W)))
  s4_mmioMissOffset.valid := s4_reqIsMmio
  s4_mmioMissOffset.bits  := 0.U

  // Send mmioFlushWb back to FTQ 1 cycle after uncache fetch return
  // When backend redirect, mmioState reset after 1 cycle.
  // In this case, mask .valid to avoid overriding backend redirect
  mmioFlushWb.valid := (s4_reqIsMmio && mmioState === MmioFsmState.WaitCommit && RegNext(fromUncache.fire) &&
    s4_mmioUseSnpc && !s4_ftqFlushSelf && !s4_ftqFlushByOlder)
  mmioFlushWb.bits.pc := s4_pc
  mmioFlushWb.bits.pd := s4_pd
  mmioFlushWb.bits.pd.zipWithIndex.foreach { case (instr, i) => instr.valid := s4_mmioRange(i) }
  mmioFlushWb.bits.ftqIdx     := s4_ftqReq.ftqIdx
  mmioFlushWb.bits.ftqOffset  := s4_ftqReq.ftqOffset.bits
  mmioFlushWb.bits.misOffset  := s4_mmioMissOffset
  mmioFlushWb.bits.cfiOffset  := DontCare
  mmioFlushWb.bits.target     := Mux(mmioIsRvc, s4_ftqReq.startVAddr + 2.U, s4_ftqReq.startVAddr + 4.U)
  mmioFlushWb.bits.jalTarget  := DontCare
  mmioFlushWb.bits.instrRange := s4_mmioRange

  mmioRvcExpander.io.in      := Mux(s4_reqIsMmio, Cat(mmioData(1), mmioData(0)), 0.U)
  mmioRvcExpander.io.fsIsOff := io.csrFsIsOff

  /* mmio pre-decode & send to IBuffer */
  when(s4_reqIsMmio) {
    val inst = Cat(mmioData(1), mmioData(0))

    val (brType, isCall, isRet) = getBrInfo(inst)

    io.toIBuffer.bits.instrs(0) := Mux(
      mmioRvcExpander.io.ill,
      mmioRvcExpander.io.in,
      mmioRvcExpander.io.out.bits
    )

    io.toIBuffer.bits.pd(0).valid  := true.B
    io.toIBuffer.bits.pd(0).isRVC  := mmioIsRvc
    io.toIBuffer.bits.pd(0).brType := brType
    io.toIBuffer.bits.pd(0).isCall := isCall
    io.toIBuffer.bits.pd(0).isRet  := isRet

    io.toIBuffer.bits.exceptionType(0) := mmioException
    // exception can happen in next page only when resend
    io.toIBuffer.bits.crossPageIPFFix(0) := mmioHasResend && mmioException.hasException
    io.toIBuffer.bits.illegalInstr(0)    := mmioRvcExpander.io.ill

    io.toIBuffer.bits.enqEnable := s4_mmioRange.asUInt

    mmioFlushWb.bits.pd(0).valid  := true.B
    mmioFlushWb.bits.pd(0).isRVC  := mmioIsRvc
    mmioFlushWb.bits.pd(0).brType := brType
    mmioFlushWb.bits.pd(0).isCall := isCall
    mmioFlushWb.bits.pd(0).isRet  := isRet
  }

  mmioRedirect.valid := s4_reqIsMmio && mmioState === MmioFsmState.WaitCommit && RegNext(
    fromUncache.fire
  ) && s4_mmioUseSnpc
  mmioRedirect.instrCount     := 1.U
  mmioRedirect.prevIBufEnqPtr := s4_prevIBufEnqPtr
  XSPerfAccumulate("fetch_bubble_ibuffer_not_ready", io.toIBuffer.valid && !io.toIBuffer.ready)

  /* *****************************************************************************
   * IFU Write-back Stage
   * - write back preDecode information to Ftq to update
   * - redirect if found fault prediction
   * - redirect if false hit last half (last PC is not start + 32 Bytes, but in the middle of an notCFI RVI instruction)
   * ***************************************************************************** */
  val bubblePds = WireDefault(VecInit.fill(PredictWidth)(0.U.asTypeOf(new PreDecodeInfo)))
  bubblePds.zipWithIndex.map {
    case (bubblePd, i) =>
      bubblePd := Mux(s4_bubbleInstrValid(i), s4_pd(s4_bubbleIndex(i)), 0.U.asTypeOf(new PreDecodeInfo))
  }
  private val wbEnable        = RegNext(s3_fire && !s3_flush) && !s4_reqIsMmio && !s4_flush
  private val wbValid         = RegNext(wbEnable, init = false.B)
  private val wbFtqReq        = RegEnable(s4_ftqReq, wbEnable)
  private val wbPreIBufEnqPtr = RegEnable(s4_prevIBufEnqPtr, wbEnable)
  private val wbInstrCount    = RegEnable(PopCount(io.toIBuffer.bits.enqEnable), wbEnable)
  wbRedirect.instrCount     := wbInstrCount
  wbRedirect.prevIBufEnqPtr := wbPreIBufEnqPtr

  private val wbCheckResultStage1 = RegEnable(checkerOutStage1, wbEnable)
  private val wbCheckResultStage2 = checkerOutStage2
  private val wbFirstInstrRange =
    wbCheckResultStage2.fixedFirstBubbleInstrRange & RegEnable(s4_firstBlock.instrRange, wbEnable)

  private val wbBubblePcLowerResult   = RegEnable(s4_bubblePcLowerResult, wbEnable)
  private val wbFirstPcHigh           = RegEnable(s4_firstBlock.pcHigh, wbEnable)
  private val wbFirstPcHighPlus1      = RegEnable(s4_firstBlock.pcHighPlus1, wbEnable)
  private val wbFirstBubblePc         = catPC(wbBubblePcLowerResult, wbFirstPcHigh, wbFirstPcHighPlus1)
  private val wbBubblePd              = RegEnable(bubblePds, wbEnable)
  private val wbFirstBubbleInstrValid = RegEnable(VecInit(s4_firstBlock.bubbleInstrValid.asBools), wbEnable)
  private val wbPd                    = RegEnable(s4_pd, wbEnable)
  private val wbInstrValid            = RegEnable(s4_instrValid.asTypeOf(Vec(PredictWidth, Bool())), wbEnable)
  private val wbInstrOffset           = RegEnable(s4_instrOffset, wbEnable)

  s4_wbNotFlush := wbFtqReq.ftqIdx === s4_ftqReq.ftqIdx && s4_valid && wbValid

  private val checkFlushWb = Wire(Valid(new PredecodeWritebackBundle))
  private val checkFlushWbJalTargetIdx = ParallelPriorityEncoder(VecInit(wbPd.zip(wbInstrValid).map {
    case (pd, v) =>
      v && pd.isJal
  }))
  private val checkFlushWbTargetIdx = wbCheckResultStage2.fixedFirstMispredIdx.bits
  checkFlushWb.valid   := wbValid
  checkFlushWb.bits.pc := wbFirstBubblePc
  checkFlushWb.bits.pd := wbBubblePd
  checkFlushWb.bits.pd.zipWithIndex.foreach { case (instr, i) => instr.valid := wbFirstBubbleInstrValid(i) }
  checkFlushWb.bits.ftqIdx    := wbFtqReq.ftqIdx
  checkFlushWb.bits.ftqOffset := wbFtqReq.ftqOffset.bits
  checkFlushWb.bits.misOffset.valid := wbCheckResultStage2.fixedFirstMispredIdx.valid // ParallelOR(wbCheckResultStage2.fixedMissPred)
  checkFlushWb.bits.misOffset.bits := wbInstrOffset(
    wbCheckResultStage2.fixedFirstMispredIdx.bits
  ) // ParallelPriorityEncoder(wbCheckResultStage2.fixedMissPred)
  checkFlushWb.bits.cfiOffset.valid := wbCheckResultStage2.fixedFirstTakenIdx.valid // ParallelOR(wbCheckResultStage1.fixedTaken)
  checkFlushWb.bits.cfiOffset.bits := wbInstrOffset(
    wbCheckResultStage2.fixedFirstTakenIdx.bits
  ) // ParallelPriorityEncoder(wbCheckResultStage1.fixedTaken)
  checkFlushWb.bits.target     := wbCheckResultStage2.fixedTwoFetchTarget(checkFlushWbTargetIdx)
  checkFlushWb.bits.jalTarget  := wbCheckResultStage2.twoFetchJalTarget(checkFlushWbJalTargetIdx)
  checkFlushWb.bits.instrRange := wbFirstInstrRange.asTypeOf(Vec(PredictWidth, Bool()))

  toFtq.pdWb := Mux(wbValid, checkFlushWb, mmioFlushWb)

  wbRedirect.valid := checkFlushWb.bits.misOffset.valid && wbValid

  /* write back flush type */
  private val checkFaultType    = wbCheckResultStage2.twoFetchFaultType
  private val checkJalFault     = wbValid && checkFaultType.map(_ === PreDecodeFaultType.JalFault).reduce(_ || _)
  private val checkJalrFault    = wbValid && checkFaultType.map(_ === PreDecodeFaultType.JalrFault).reduce(_ || _)
  private val checkRetFault     = wbValid && checkFaultType.map(_ === PreDecodeFaultType.RetFault).reduce(_ || _)
  private val checkTargetFault  = wbValid && checkFaultType.map(_ === PreDecodeFaultType.TargetFault).reduce(_ || _)
  private val checkNotCFIFault  = wbValid && checkFaultType.map(_ === PreDecodeFaultType.NotCfiFault).reduce(_ || _)
  private val checkInvalidTaken = wbValid && checkFaultType.map(_ === PreDecodeFaultType.InvalidTaken).reduce(_ || _)

  XSPerfAccumulate("predecode_flush_jalFault", checkJalFault)
  XSPerfAccumulate("predecode_flush_jalrFault", checkJalrFault)
  XSPerfAccumulate("predecode_flush_retFault", checkRetFault)
  XSPerfAccumulate("predecode_flush_targetFault", checkTargetFault)
  XSPerfAccumulate("predecode_flush_notCFIFault", checkNotCFIFault)
  XSPerfAccumulate("predecode_flush_invalidTakenFault", checkInvalidTaken)

  XSDebug(
    checkRetFault,
    "startAddr:%x  nextStartAddr:%x  taken:%d    takenIdx:%d\n",
    wbFtqReq.startVAddr.toUInt,
    wbFtqReq.nextStartVAddr.toUInt,
    wbFtqReq.ftqOffset.valid,
    wbFtqReq.ftqOffset.bits
  )

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
  XSPerfAccumulate("except_0", io.toIBuffer.fire && s4_perfInfo.except0)
  XSPerfAccumulate("hit_0_except_1", io.toIBuffer.fire && s4_perfInfo.hit0Except1)
  XSPerfAccumulate("miss_0_except_1", io.toIBuffer.fire && s4_perfInfo.miss0Except1)
  XSPerfHistogram(
    "ifu2ibuffer_validCnt",
    PopCount(io.toIBuffer.bits.valid & io.toIBuffer.bits.enqEnable),
    io.toIBuffer.fire,
    0,
    PredictWidth + 1,
    1
  )

  // DB
  private val hartId                     = p(XSCoreParamsKey).HartId
  private val isWriteFetchToIBufferTable = Constantin.createRecord(s"isWriteFetchToIBufferTable$hartId")
  private val isWriteIfuWbToFtqTable     = Constantin.createRecord(s"isWriteIfuWbToFtqTable$hartId")
  private val fetchToIBufferTable        = ChiselDB.createTable(s"FetchToIBuffer$hartId", new FetchToIBufferDB)
  private val ifuWbToFtqTable            = ChiselDB.createTable(s"IfuWbToFtq$hartId", new IfuWbToFtqDB)

  private val fetchIBufferDumpData = Wire(new FetchToIBufferDB)
  fetchIBufferDumpData.startAddr  := s4_ftqReq.startVAddr.toUInt
  fetchIBufferDumpData.instrCount := PopCount(io.toIBuffer.bits.enqEnable)
  fetchIBufferDumpData.exception  := io.toIBuffer.fire && s4_perfInfo.except
  fetchIBufferDumpData.isCacheHit := io.toIBuffer.fire && s4_perfInfo.hit

  private val ifuWbToFtqDumpData = Wire(new IfuWbToFtqDB)
  ifuWbToFtqDumpData.startAddr         := wbFtqReq.startVAddr.toUInt
  ifuWbToFtqDumpData.isMissPred        := checkFlushWb.bits.misOffset.valid
  ifuWbToFtqDumpData.missPredOffset    := checkFlushWb.bits.misOffset.bits
  ifuWbToFtqDumpData.checkJalFault     := checkJalFault
  ifuWbToFtqDumpData.checkJalrFault    := checkJalrFault
  ifuWbToFtqDumpData.checkRetFault     := checkRetFault
  ifuWbToFtqDumpData.checkTargetFault  := checkTargetFault
  ifuWbToFtqDumpData.checkNotCFIFault  := checkNotCFIFault
  ifuWbToFtqDumpData.checkInvalidTaken := checkInvalidTaken

  fetchToIBufferTable.log(
    data = fetchIBufferDumpData,
    en = isWriteFetchToIBufferTable.orR && io.toIBuffer.fire,
    site = "IFU" + p(XSCoreParamsKey).HartId.toString,
    clock = clock,
    reset = reset
  )
  ifuWbToFtqTable.log(
    data = ifuWbToFtqDumpData,
    en = isWriteIfuWbToFtqTable.orR && checkFlushWb.valid,
    site = "IFU" + p(XSCoreParamsKey).HartId.toString,
    clock = clock,
    reset = reset
  )
}
