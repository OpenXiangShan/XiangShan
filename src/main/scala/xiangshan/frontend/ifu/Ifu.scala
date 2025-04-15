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
import xiangshan.frontend.ICacheToIfuIO
import xiangshan.frontend.IfuToBackendIO
import xiangshan.frontend.IfuToFtqIO
import xiangshan.frontend.IfuToICacheIO
import xiangshan.frontend.IfuToInstrUncacheIO
import xiangshan.frontend.InstrUncacheToIfuIO
import xiangshan.frontend.PredecodeWritebackBundle
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.PrunedAddrInit
import xiangshan.frontend.icache.HasICacheParameters
import xiangshan.frontend.icache.PmpCheckBundle
import xiangshan.frontend.mmioCommitRead

class Ifu(implicit p: Parameters) extends IfuModule
    with HasICacheParameters
    with FetchBlockHelper
    with PreDecodeHelper
    with PcCutHelper
    with HasCircularQueuePtrHelper
    with HasPerfEvents
    with HasTlbConst {

  class IfuIO(implicit p: Parameters) extends IfuBundle {
    // Ftq: request / write back
    val fromFtq: FtqToIfuIO = Flipped(new FtqToIfuIO)
    val toFtq:   IfuToFtqIO = new IfuToFtqIO
    // ftq: mmio commit read
    val mmioCommitRead: mmioCommitRead = new mmioCommitRead

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
  private val predChecker     = Module(new PredChecker)
  private val frontendTrigger = Module(new FrontendTrigger)
  private val rvcExpanders    = Seq.fill(PredictWidth)(Module(new RvcExpander))
  private val mmioRvcExpander = Module(new RvcExpander)
  private val f3PreDecoder    = Module(new F3PreDecode)

  // alias
  private val (toFtq, fromFtq)              = (io.toFtq, io.fromFtq)
  private val fromICache                    = io.fromICache.fetchResp
  private val (toUncache, fromUncache)      = (io.toUncache.req, io.fromUncache.resp)
  private val (preDecoderIn, preDecoderOut) = (preDecoder.io.req, preDecoder.io.resp)
  private val (checkerIn, checkerOutStage1, checkerOutStage2) =
    (predChecker.io.req, predChecker.io.resp.stage1Out, predChecker.io.resp.stage2Out)

  private val s1_ready, s2_ready, s3_ready           = WireInit(false.B)
  private val s0_fire, s1_fire, s2_fire, s3_fire     = WireInit(false.B)
  private val s0_flush, s1_flush, s2_flush, s3_flush = WireInit(false.B)

  // Top-down
  private def numOfStage = 3
  require(numOfStage > 1, "Ifu numOfStage must be greater than 1")
  private val topdownStages = RegInit(VecInit(Seq.fill(numOfStage)(0.U.asTypeOf(new FrontendTopDownBundle))))
  // bubble events in IFU, only happen in stage 1
  private val icacheMissBubble = io.fromICache.topdown.icacheMiss
  private val itlbMissBubble   = io.fromICache.topdown.itlbMiss

  // only driven by clock, not valid-ready
  topdownStages(0) := fromFtq.req.bits.topdown_info
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

  private val wbRedirect, mmioRedirect, backendRedirect = WireInit(false.B)
  private val s3_wbNotFlush                             = WireInit(false.B)

  backendRedirect := fromFtq.redirect.valid
  s3_flush        := backendRedirect || (wbRedirect && !s3_wbNotFlush)
  s2_flush        := backendRedirect || mmioRedirect || wbRedirect
  s1_flush        := s2_flush
  s0_flush        := s1_flush || s0_flushFromBpu

  fromFtq.req.ready := s1_ready && io.fromICache.fetchReady

  when(wbRedirect) {
    when(s3_wbNotFlush) {
      topdownStages(2).reasons(TopDownCounters.BTBMissBubble.id) := true.B
    }
    for (i <- 0 until numOfStage - 1) {
      topdownStages(i).reasons(TopDownCounters.BTBMissBubble.id) := true.B
    }
  }

  /** <PERF> f0 fetch bubble */
  XSPerfAccumulate("fetch_bubble_ftq_not_valid", !fromFtq.req.valid && fromFtq.req.ready)
  // XSPerfAccumulate("fetch_bubble_pipe_stall",    s0_valid && toICache(0).ready && toICache(1).ready && !s1_ready )
  // XSPerfAccumulate("fetch_bubble_icache_0_busy",   s0_valid && !toICache(0).ready  )
  // XSPerfAccumulate("fetch_bubble_icache_1_busy",   s0_valid && !toICache(1).ready  )
  XSPerfAccumulate("fetch_flush_backend_redirect", backendRedirect)
  XSPerfAccumulate("fetch_flush_wb_redirect", wbRedirect)
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

  private val s1_pcHigh      = s1_ftqReq.startAddr(VAddrBits - 1, PcCutPoint)
  private val s1_pcHighPlus1 = s1_pcHigh + 1.U

  /**
   * In order to reduce power consumption, avoid calculating the full PC value in the first level.
   * code of original logic, this code has been deprecated
   * val s1_pc                 = VecInit(s1_pcLowerResult.map{ i =>
   *   Mux(
   *     i(s1_pc_adder_cut_point),
   *     Cat(s1_pcHighPlus1,i(s1_pc_adder_cut_point-1,0)),
   *     Cat(s1_pcHigh,i(s1_pc_adder_cut_point-1,0))
   *     )
   *   })
   */
  private val s1_pcLowerResult = VecInit((0 until PredictWidth).map(i =>
    Cat(0.U(1.W), s1_ftqReq.startAddr(PcCutPoint - 1, 0)) + (i * 2).U
  )) // cat with overflow bit

  private val s1_pc = catPC(s1_pcLowerResult, s1_pcHigh, s1_pcHighPlus1)

  private val s1_halfSnpcLowerResult = VecInit((0 until PredictWidth).map(i =>
    Cat(0.U(1.W), s1_ftqReq.startAddr(PcCutPoint - 1, 0)) + ((i + 2) * 2).U
  )) // cat with overflow bit
  private val s1_halfSnpc = catPC(s1_halfSnpcLowerResult, s1_pcHigh, s1_pcHighPlus1)

  if (env.FPGAPlatform) {
    val s1_pcDiff       = VecInit((0 until PredictWidth).map(i => s1_ftqReq.startAddr + (i * 2).U))
    val s1_halfSnpcDiff = VecInit((0 until PredictWidth).map(i => s1_ftqReq.startAddr + ((i + 2) * 2).U))

    XSError(
      s1_pc.zip(s1_pcDiff).map { case (a, b) => a.asUInt =/= b.asUInt }.reduce(_ || _),
      "s1_halfSnpc adder cut fail"
    )
    XSError(
      s1_halfSnpc.zip(s1_halfSnpcDiff).map { case (a, b) => a.asUInt =/= b.asUInt }.reduce(_ || _),
      "s1_halfSnpc adder cut fail"
    )
  }

  private val s1_cutPtr =
    if (HasCExtension)
      VecInit((0 until PredictWidth + 1).map(i => Cat(0.U(2.W), s1_ftqReq.startAddr(blockOffBits - 1, 1)) + i.U))
    else VecInit((0 until PredictWidth).map(i => Cat(0.U(2.W), s1_ftqReq.startAddr(blockOffBits - 1, 2)) + i.U))

  /* *****************************************************************************
   * IFU Stage 2
   * - icache response data (latched for pipeline stop)
   * - generate exception bits for every instruction (page fault/access fault/mmio)
   * - generate predicted instruction range (1 means this instruction is in this fetch packet)
   * - cut data from cachelines to packet instruction code
   * - instruction preDecode and RVC expand
   * ***************************************************************************** */

  private val icacheRespAllValid = WireInit(false.B)

  private val s2_valid      = ValidHold(s1_fire && !s1_flush, s2_fire, s2_flush)
  private val s2_ftqReq     = RegEnable(s1_ftqReq, s1_fire)
  private val s2_doubleline = RegEnable(s1_doubleline, s1_fire)

  s2_fire  := s2_valid && s3_ready && icacheRespAllValid
  s2_ready := s2_fire || !s2_valid

  // TODO: addr compare may be timing critical
  private val s2_iCacheAllRespWire =
    fromICache.valid &&
      fromICache.bits.vAddr(0) === s2_ftqReq.startAddr &&
      (fromICache.bits.doubleline && fromICache.bits.vAddr(1) === s2_ftqReq.nextlineStart || !s2_doubleline)
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

  /**
    * reduce the number of registers, origin code
    * s2_pc = RegEnable(s1_pc, s1_fire)
    */
  private val s2_pcLowerResult = RegEnable(s1_pcLowerResult, s1_fire)
  private val s2_pcHigh        = RegEnable(s1_pcHigh, s1_fire)
  private val s2_pcHighPlus1   = RegEnable(s1_pcHighPlus1, s1_fire)
  private val s2_pc            = catPC(s2_pcLowerResult, s2_pcHigh, s2_pcHighPlus1)

  private val s2_cutPtr      = RegEnable(s1_cutPtr, s1_fire)
  private val s2_resendVAddr = RegEnable(s1_ftqReq.startAddr + 2.U, s1_fire)

  private val s2_foldPc = VecInit(s2_pc.map(i => XORFold(i(VAddrBits - 1, 1), MemPredPCWidth)))
  private val s2_jumpRange =
    Fill(PredictWidth, !s2_ftqReq.ftqOffset.valid) | Fill(PredictWidth, 1.U(1.W)) >> ~s2_ftqReq.ftqOffset.bits
  require(
    isPow2(PredictWidth),
    "If PredictWidth does not satisfy the power of 2," +
      "expression: Fill(PredictWidth, 1.U(1.W)) >> ~s2_ftqReq.ftqOffset.bits is not right !!"
  )
  private val s2_ftrRange =
    Fill(PredictWidth, s2_ftqReq.ftqOffset.valid) | Fill(PredictWidth, 1.U(1.W)) >> ~getBasicBlockIdx(
      s2_ftqReq.nextStartAddr,
      s2_ftqReq.startAddr
    )
  private val s2_instrRange = s2_jumpRange & s2_ftrRange
  private val s2_exceptionVec = VecInit((0 until PredictWidth).map(i =>
    MuxCase(
      ExceptionType.None,
      Seq(
        !isNextLine(s2_pc(i), s2_ftqReq.startAddr)                   -> s2_exception(0),
        (isNextLine(s2_pc(i), s2_ftqReq.startAddr) && s2_doubleline) -> s2_exception(1)
      )
    )
  ))
  private val s2_perfInfo = io.fromICache.perf

  def cut(cacheline: UInt, cutPtr: Vec[UInt]): Vec[UInt] = {
    // FIXME: !HasCExtension
    require(HasCExtension)
    // if(HasCExtension){
    val result  = Wire(Vec(PredictWidth + 1, UInt(16.W)))
    val dataVec = cacheline.asTypeOf(Vec(blockBytes, UInt(16.W))) // 32 16-bit data vector
    (0 until PredictWidth + 1).foreach(i =>
      result(i) := dataVec(cutPtr(i)) // the max ptr is 3*blockBytes/4-1
    )
    result
    // } else {
    //   val result   = Wire(Vec(PredictWidth, UInt(32.W)) )
    //   val dataVec  = cacheline.asTypeOf(Vec(blockBytes * 2/ 4, UInt(32.W)))
    //   (0 until PredictWidth).foreach( i =>
    //     result(i) := dataVec(cutPtr(i))
    //   )
    //   result
    // }
  }

  /* NOTE: the following `Cat(_data, _data)` *is* intentional.
   * Explanation:
   * In the old design, IFU is responsible for selecting requested data from two adjacent cachelines,
   *    so IFU has to receive 2*64B (2cacheline * 64B) data from ICache, and do `Cat(_data(1), _data(0))` here.
   * However, a fetch block is 34B at max, sending 2*64B is quiet a waste of power.
   * In current design (2024.06~), ICacheDataArray is responsible for selecting data from two adjacent cachelines,
   *    so IFU only need to receive 40B (5bank * 8B) valid data, and use only one port is enough.
   * For example, when pc falls on the 6th bank in cacheline0(so this is a doubleline request):
   *                                MSB                                         LSB
   *                  cacheline 1 || 1-7 | 1-6 | 1-5 | 1-4 | 1-3 | 1-2 | 1-1 | 1-0 ||
   *                  cacheline 0 || 0-7 | 0-6 | 0-5 | 0-4 | 0-3 | 0-2 | 0-1 | 0-0 ||
   *    and ICacheDataArray will respond:
   *         fromICache.bits.data || 0-7 | 0-6 | xxx | xxx | xxx | 1-2 | 1-1 | 1-0 ||
   *    therefore simply make a copy of the response and `Cat` together, and obtain the requested data from centre:
   *                s2_copiedData || 0-7 | 0-6 | xxx | xxx | xxx | 1-2 | 1-1 | 1-0 | 0-7 | 0-6 | xxx | xxx | xxx | 1-2 | 1-1 | 1-0 ||
   *                                             requested data: ^-----------------------------^
   * For another example, pc falls on the 1st bank in cacheline 0, we have:
   *         fromICache.bits.data || xxx | xxx | 0-5 | 0-4 | 0-3 | 0-2 | 0-1 | xxx ||
   *                s2_copiedData || xxx | xxx | 0-5 | 0-4 | 0-3 | 0-2 | 0-1 | xxx | xxx | xxx | 0-5 | 0-4 | 0-3 | 0-2 | 0-1 | xxx ||
   *                                                                           requested data: ^-----------------------------^
   * Each "| x-y |" block is a 8B bank from cacheline(x).bank(y)
   * Please also refer to:
   * - DataArray selects data:
   * https://github.com/OpenXiangShan/XiangShan/blob/d4078d6edbfb4611ba58c8b0d1d8236c9115dbfc/src/main/scala/xiangshan/frontend/icache/ICache.scala#L355-L381
   * https://github.com/OpenXiangShan/XiangShan/blob/d4078d6edbfb4611ba58c8b0d1d8236c9115dbfc/src/main/scala/xiangshan/frontend/icache/ICache.scala#L149-L161
   * - ICache respond to IFU:
   * https://github.com/OpenXiangShan/XiangShan/blob/d4078d6edbfb4611ba58c8b0d1d8236c9115dbfc/src/main/scala/xiangshan/frontend/icache/ICacheMainPipe.scala#L473
   */
  private val s2_copiedData = Cat(fromICache.bits.data, fromICache.bits.data)

  private val s2_cutData = cut(s2_copiedData, s2_cutPtr)

  // PreDecode: delimitation, does not expand RVC
  preDecoderIn.valid     := s2_valid
  preDecoderIn.bits.data := s2_cutData
  preDecoderIn.bits.pc   := s2_pc

  private val s2_instr      = preDecoderOut.instr
  private val s2_pd         = preDecoderOut.pd
  private val s2_jumpOffset = preDecoderOut.jumpOffset
  private val s2_altValid   = preDecoderOut.altValid
  /* if there is a cross-page RVI instruction, and the former page has no exception,
   * whether it has exception is actually depends on the latter page
   */
  private val s2_crossPageExceptionVec = VecInit((0 until PredictWidth).map { i =>
    Mux(
      isLastInLine(s2_pc(i)) && !s2_pd(i).isRVC && s2_doubleline && s2_exception(0).isNone,
      s2_exception(1),
      ExceptionType.None
    )
  })
  XSPerfAccumulate("fetch_bubble_icache_not_resp", s2_valid && !icacheRespAllValid)

  /* *****************************************************************************
   * IFU Stage 3
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
  private val s3_valid = WireInit(false.B)

  private val s3_ftqReq     = RegEnable(s2_ftqReq, s2_fire)
  private val s3_doubleline = RegEnable(s2_doubleline, s2_fire)
  s3_fire := io.toIBuffer.fire

  private val s3_cutData = RegEnable(s2_cutData, s2_fire)

  private val s3_exception          = RegEnable(s2_exception, s2_fire)
  private val s3_pmpMmio            = RegEnable(s2_pmpMmio, s2_fire)
  private val s3_itlbPbmt           = RegEnable(s2_itlbPbmt, s2_fire)
  private val s3_isBackendException = RegEnable(s2_isBackendException, s2_fire)

  private val s3_instr = RegEnable(s2_instr, s2_fire)

  rvcExpanders.zipWithIndex.foreach { case (expander, i) =>
    expander.io.in      := s3_instr(i)
    expander.io.fsIsOff := io.csrFsIsOff
  }
  // Use expanded instruction only when input is legal.
  // Otherwise, use origin illegal RVC instruction.
  private val s3_expdInstr = VecInit(rvcExpanders.map { expander: RvcExpander =>
    Mux(expander.io.ill, expander.io.in, expander.io.out.bits)
  })
  private val s3_ill = VecInit(rvcExpanders.map(_.io.ill))

  private val s3_pdWire                = RegEnable(s2_pd, s2_fire)
  private val s3_pd                    = WireInit(s3_pdWire)
  private val s3_jumpOffset            = RegEnable(s2_jumpOffset, s2_fire)
  private val s3_exceptionVec          = RegEnable(s2_exceptionVec, s2_fire)
  private val s3_crossPageExceptionVec = RegEnable(s2_crossPageExceptionVec, s2_fire)

  private val s3_pcLowerResult = RegEnable(s2_pcLowerResult, s2_fire)
  private val s3_pcHigh        = RegEnable(s2_pcHigh, s2_fire)
  private val s3_pcHighPlus1   = RegEnable(s2_pcHighPlus1, s2_fire)
  private val s3_pc            = catPC(s3_pcLowerResult, s3_pcHigh, s3_pcHighPlus1)

  private val s3_pcLastLowerResultPlus2 = RegEnable(s2_pcLowerResult(PredictWidth - 1) + 2.U, s2_fire)
  private val s3_pcLastLowerResultPlus4 = RegEnable(s2_pcLowerResult(PredictWidth - 1) + 4.U, s2_fire)

  /**
    ***********************************************************************
    * Half snpc(i) is larger than pc(i) by 4. Using pc to calculate half snpc may be a good choice.
    ***********************************************************************
    */
  private val s3_halfSnpc = Wire(Vec(PredictWidth, PrunedAddr(VAddrBits)))
  for (i <- 0 until PredictWidth) {
    if (i == (PredictWidth - 2)) {
      s3_halfSnpc(i) := catPC(s3_pcLastLowerResultPlus2, s3_pcHigh, s3_pcHighPlus1)
    } else if (i == (PredictWidth - 1)) {
      s3_halfSnpc(i) := catPC(s3_pcLastLowerResultPlus4, s3_pcHigh, s3_pcHighPlus1)
    } else {
      s3_halfSnpc(i) := s3_pc(i + 2)
    }
  }

  private val s3_instrRange        = RegEnable(s2_instrRange, s2_fire)
  private val s3_foldPc            = RegEnable(s2_foldPc, s2_fire)
  private val s3_altValid          = RegEnable(s2_altValid, s2_fire)
  private val s3_pAddr             = RegEnable(s2_pAddr, s2_fire)
  private val s3_gpAddr            = RegEnable(s2_gpAddr, s2_fire)
  private val s3_isForVSnonLeafPTE = RegEnable(s2_isForVSnonLeafPTE, s2_fire)
  private val s3_resendVAddr       = RegEnable(s2_resendVAddr, s2_fire)

  // Expand 1 bit to prevent overflow when assert
  private val s3_ftqReqStartAddr     = PrunedAddrInit(Cat(0.U(1.W), s3_ftqReq.startAddr.toUInt))
  private val s3_ftqReqNextStartAddr = PrunedAddrInit(Cat(0.U(1.W), s3_ftqReq.nextStartAddr.toUInt))
  // brType, isCall and isRet generation is delayed to f3 stage

  f3PreDecoder.io.instr := s3_instr

  s3_pd.zipWithIndex.foreach { case (pd, i) =>
    pd.brType := f3PreDecoder.io.pd(i).brType
    pd.isCall := f3PreDecoder.io.pd(i).isCall
    pd.isRet  := f3PreDecoder.io.pd(i).isRet
  }

  private val f3PdDiff = s3_pdWire.zip(s3_pd).map { case (a, b) => a.asUInt =/= b.asUInt }.reduce(_ || _)
  XSError(s3_valid && f3PdDiff, "f3 pd diff")

  when(s3_valid && !s3_ftqReq.ftqOffset.valid) {
    assert(
      s3_ftqReqStartAddr + (2 * PredictWidth).U >= s3_ftqReqNextStartAddr,
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
  io.mmioCommitRead.mmioFtqPtr := RegNext(s3_ftqReq.ftqIdx - 1.U)

  // do mmio fetch only when pmp/pbmt shows it is a un-cacheable address and no exception occurs
  private val s3_reqIsMmio =
    s3_valid && (s3_pmpMmio || Pbmt.isUncache(s3_itlbPbmt)) && s3_exception.map(_.isNone).reduce(_ && _)
  private val mmioCommit = VecInit(io.robCommits.map { commit =>
    commit.valid && commit.bits.ftqIdx === s3_ftqReq.ftqIdx && commit.bits.ftqOffset === 0.U
  }).asUInt.orR
  private val s3_mmioReqCommit = s3_reqIsMmio && mmioState === MmioFsmState.Commited

  private val s3_mmioWaitCommit     = s3_reqIsMmio && mmioState === MmioFsmState.WaitCommit
  private val s3_mmioWaitCommitNext = RegNext(s3_mmioWaitCommit)
  private val s3_mmioCanGo          = s3_mmioWaitCommit && !s3_mmioWaitCommitNext

  private val fromFtqRedirectReg = Wire(fromFtq.redirect.cloneType)
  fromFtqRedirectReg.bits := RegEnable(
    fromFtq.redirect.bits,
    0.U.asTypeOf(fromFtq.redirect.bits),
    fromFtq.redirect.valid
  )
  fromFtqRedirectReg.valid := RegNext(fromFtq.redirect.valid, init = false.B)
  private val mmioF3Flush     = RegNext(s3_flush, init = false.B)
  private val s3_ftqFlushSelf = fromFtqRedirectReg.valid && RedirectLevel.flushItself(fromFtqRedirectReg.bits.level)
  private val s3_ftqFlushByOlder =
    fromFtqRedirectReg.valid && isBefore(fromFtqRedirectReg.bits.ftqIdx, s3_ftqReq.ftqIdx)

  private val s3_needNotFlush = s3_reqIsMmio && fromFtqRedirectReg.valid && !s3_ftqFlushSelf && !s3_ftqFlushByOlder

  /* We want to defer instruction fetching when encountering MMIO instructions
   * to ensure that the MMIO region is not negatively impacted. (no speculative fetch in MMIO region)
   * This is the exception when the first instruction is an MMIO instruction.
   */
  when(isFirstInstr && s3_fire) {
    isFirstInstr := false.B
  }

  s3_valid := ValidHold(
    // infire: s2 -> s3 fire
    s2_fire && !s2_flush,
    // outfire: if req is mmio, wait for commit, else wait for IBuffer
    Mux(s3_reqIsMmio, s3_mmioReqCommit, io.toIBuffer.fire),
    // flush: if req is mmio, check whether mmio Fsm allow flush, else flush directly
    Mux(s3_reqIsMmio, mmioF3Flush && !s3_needNotFlush, s3_flush)
  )
  dontTouch(s3_valid)

  private val (redirectFtqIdx, redirectFtqOffset) =
    (fromFtqRedirectReg.bits.ftqIdx, fromFtqRedirectReg.bits.ftqOffset)
  private val redirectMmioReq =
    fromFtqRedirectReg.valid && redirectFtqIdx === s3_ftqReq.ftqIdx && redirectFtqOffset === 0.U

  private val s3_mmioUseSnpc = ValidHold(RegNext(s2_fire && !s2_flush) && s3_reqIsMmio, redirectMmioReq)

  s3_ready := (io.toIBuffer.ready && (s3_mmioReqCommit || !s3_reqIsMmio)) || !s3_valid

  // mmio state machine
  switch(mmioState) {
    is(MmioFsmState.Idle) {
      when(s3_reqIsMmio) {
        // in idempotent spaces, we can send request directly (i.e. can do speculative fetch)
        mmioState := Mux(s3_itlbPbmt === Pbmt.nc, MmioFsmState.SendReq, MmioFsmState.WaitLastCommit)
      }
    }

    is(MmioFsmState.WaitLastCommit) {
      when(isFirstInstr) {
        mmioState := MmioFsmState.SendReq
      }.otherwise {
        mmioState := Mux(io.mmioCommitRead.mmioLastCommit, MmioFsmState.SendReq, MmioFsmState.WaitLastCommit)
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
        val needResend = !respIsRVC && s3_pAddr(0)(2, 1) === 3.U && exception.isNone
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
      mmioState := Mux(mmioCommit || s3_itlbPbmt === Pbmt.nc, MmioFsmState.Commited, MmioFsmState.WaitCommit)
    }

    // normal mmio instruction
    is(MmioFsmState.Commited) {
      mmioReset() // includes mmioState := MmioFsmState.Idle
    }
  }

  // Exception or flush by older branch prediction
  // Condition is from RegNext(fromFtq.redirect), 1 cycle after backend redirect
  when(s3_ftqFlushSelf || s3_ftqFlushByOlder) {
    mmioReset()
  }

  toUncache.valid := ((mmioState === MmioFsmState.SendReq) || (mmioState === MmioFsmState.ResendReq)) && s3_reqIsMmio
  toUncache.bits.addr := Mux(mmioState === MmioFsmState.ResendReq, mmioResendAddr, s3_pAddr(0))
  fromUncache.ready   := true.B

  // send itlb request in MmioFsmState.SendTlb state
  io.itlb.req.valid                   := (mmioState === MmioFsmState.SendTlb) && s3_reqIsMmio
  io.itlb.req.bits.size               := 3.U
  io.itlb.req.bits.vaddr              := s3_resendVAddr.toUInt
  io.itlb.req.bits.debug.pc           := s3_resendVAddr.toUInt
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
  io.itlb.resp.ready := (mmioState === MmioFsmState.TlbResp) && s3_reqIsMmio

  io.pmp.req.valid     := (mmioState === MmioFsmState.SendPmp) && s3_reqIsMmio
  io.pmp.req.bits.addr := mmioResendAddr.toUInt
  io.pmp.req.bits.size := 3.U
  io.pmp.req.bits.cmd  := TlbCmd.exec

  private val s3_lastHalf = RegInit(0.U.asTypeOf(new LastHalfEntry))

  private val s3_mmioRange  = VecInit((0 until PredictWidth).map(i => if (i == 0) true.B else false.B))
  private val s3_instrValid = Wire(Vec(PredictWidth, Bool()))

  /* ** prediction result check ** */
  checkerIn.bits.ftqOffset  := s3_ftqReq.ftqOffset
  checkerIn.bits.jumpOffset := s3_jumpOffset
  checkerIn.bits.target     := s3_ftqReq.nextStartAddr
  checkerIn.bits.instrRange := s3_instrRange.asTypeOf(Vec(PredictWidth, Bool()))
  checkerIn.bits.instrValid := s3_instrValid.asTypeOf(Vec(PredictWidth, Bool()))
  checkerIn.bits.pds        := s3_pd
  checkerIn.bits.pc         := s3_pc
  checkerIn.valid           := RegNext(s2_fire, init = false.B)

  /* ** handle half RVI in the last 2 Bytes ** */
  private def hasLastHalf(idx: UInt): Bool =
    !s3_pd(idx).isRVC && checkerOutStage1.fixedRange(idx) && s3_instrValid(idx) &&
      !checkerOutStage1.fixedTaken(idx) && !s3_reqIsMmio

  private val s3_lastValidIdx = ParallelPosteriorityEncoder(checkerOutStage1.fixedRange)

  private val s3_hasLastHalf   = hasLastHalf((PredictWidth - 1).U)
  private val s3_falseLastHalf = hasLastHalf(s3_lastValidIdx)
  private val s3_falseSnpc     = s3_halfSnpc(s3_lastValidIdx)

  private val s3_lastHalfMask     = VecInit((0 until PredictWidth).map(i => if (i == 0) false.B else true.B)).asUInt
  private val s3_lastHalfDisabled = RegInit(false.B)

  when(s3_flush || (s3_fire && s3_lastHalfDisabled)) {
    s3_lastHalfDisabled := false.B
  }

  when(s3_flush) {
    s3_lastHalf.valid := false.B
  }.elsewhen(s3_fire) {
    s3_lastHalf.valid    := s3_hasLastHalf && !s3_lastHalfDisabled
    s3_lastHalf.middlePC := s3_ftqReq.nextStartAddr
  }

  s3_instrValid := Mux(s3_lastHalf.valid, s3_altValid, VecInit(s3_pd.map(inst => inst.valid)))

  /* ** frontend Trigger  ** */
  frontendTrigger.io.pds             := s3_pd
  frontendTrigger.io.pc              := s3_pc
  frontendTrigger.io.data            := s3_cutData
  frontendTrigger.io.frontendTrigger := io.frontendTrigger
  private val s3_triggered = frontendTrigger.io.triggered

  /* ** send to IBuffer ** */
  private val s3_toIBufferValid = s3_valid && (!s3_reqIsMmio || s3_mmioCanGo) && !s3_flush
  io.toIBuffer.valid          := s3_toIBufferValid
  io.toIBuffer.bits.instrs    := s3_expdInstr
  io.toIBuffer.bits.valid     := s3_instrValid.asUInt
  io.toIBuffer.bits.enqEnable := checkerOutStage1.fixedRange.asUInt & s3_instrValid.asUInt
  io.toIBuffer.bits.pd        := s3_pd
  io.toIBuffer.bits.ftqPtr    := s3_ftqReq.ftqIdx
  io.toIBuffer.bits.pc        := s3_pc
  // Find last using PriorityMux
  io.toIBuffer.bits.isLastInFtqEntry := Reverse(PriorityEncoderOH(Reverse(io.toIBuffer.bits.enqEnable))).asBools
  io.toIBuffer.bits.ftqOffset.zipWithIndex.foreach { case (a, i) =>
    a.bits  := i.U
    a.valid := checkerOutStage1.fixedTaken(i) && !s3_reqIsMmio
  }
  io.toIBuffer.bits.foldpc := s3_foldPc
  io.toIBuffer.bits.exceptionType := VecInit((s3_exceptionVec zip s3_crossPageExceptionVec).map { case (e, ce) =>
    e || ce // merge, cross page fix has lower priority
  })
  // backendException only needs to be set for the first instruction.
  // Other instructions in the same block may have pf or af set,
  // which is a side effect of the first instruction and actually not necessary.
  io.toIBuffer.bits.backendException := (0 until PredictWidth).map {
    case 0 => s3_isBackendException
    case _ => false.B
  }
  io.toIBuffer.bits.crossPageIPFFix := s3_crossPageExceptionVec.map(_.hasException)
  io.toIBuffer.bits.illegalInstr    := s3_ill
  io.toIBuffer.bits.triggered       := s3_triggered

  when(s3_lastHalf.valid) {
    io.toIBuffer.bits.enqEnable := checkerOutStage1.fixedRange.asUInt & s3_instrValid.asUInt & s3_lastHalfMask
    io.toIBuffer.bits.valid     := s3_lastHalfMask & s3_instrValid.asUInt
  }

  when(io.toIBuffer.valid && io.toIBuffer.ready) {
    val enqVec = io.toIBuffer.bits.enqEnable
    val allocateSeqNum = VecInit((0 until PredictWidth).map { i =>
      val idx  = PopCount(enqVec.take(i + 1))
      val pc   = s3_pc(i).toUInt
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
  // s3_gpAddr is valid iff gpf is detected
  io.toBackend.gpaddrMem_wen := s3_toIBufferValid && Mux(
    s3_reqIsMmio,
    mmioException.isGpf,
    s3_exception.map(_.isGpf).reduce(_ || _)
  )
  io.toBackend.gpaddrMem_waddr        := s3_ftqReq.ftqIdx.value
  io.toBackend.gpaddrMem_wdata.gpaddr := Mux(s3_reqIsMmio, mmioResendGpAddr.toUInt, s3_gpAddr.toUInt)
  io.toBackend.gpaddrMem_wdata.isForVSnonLeafPTE := Mux(
    s3_reqIsMmio,
    mmioResendIsForVSnonLeafPTE,
    s3_isForVSnonLeafPTE
  )

  // Write back to Ftq
  private val mmioFlushWb       = Wire(Valid(new PredecodeWritebackBundle))
  private val s3_mmioMissOffset = Wire(ValidUndirectioned(UInt(log2Ceil(PredictWidth).W)))
  s3_mmioMissOffset.valid := s3_reqIsMmio
  s3_mmioMissOffset.bits  := 0.U

  // Send mmioFlushWb back to FTQ 1 cycle after uncache fetch return
  // When backend redirect, mmioState reset after 1 cycle.
  // In this case, mask .valid to avoid overriding backend redirect
  mmioFlushWb.valid := (s3_reqIsMmio && mmioState === MmioFsmState.WaitCommit && RegNext(fromUncache.fire) &&
    s3_mmioUseSnpc && !s3_ftqFlushSelf && !s3_ftqFlushByOlder)
  mmioFlushWb.bits.pc := s3_pc
  mmioFlushWb.bits.pd := s3_pd
  mmioFlushWb.bits.pd.zipWithIndex.foreach { case (instr, i) => instr.valid := s3_mmioRange(i) }
  mmioFlushWb.bits.ftqIdx     := s3_ftqReq.ftqIdx
  mmioFlushWb.bits.ftqOffset  := s3_ftqReq.ftqOffset.bits
  mmioFlushWb.bits.misOffset  := s3_mmioMissOffset
  mmioFlushWb.bits.cfiOffset  := DontCare
  mmioFlushWb.bits.target     := Mux(mmioIsRvc, s3_ftqReq.startAddr + 2.U, s3_ftqReq.startAddr + 4.U)
  mmioFlushWb.bits.jalTarget  := DontCare
  mmioFlushWb.bits.instrRange := s3_mmioRange

  mmioRvcExpander.io.in      := Mux(s3_reqIsMmio, Cat(mmioData(1), mmioData(0)), 0.U)
  mmioRvcExpander.io.fsIsOff := io.csrFsIsOff

  /* mmio pre-decode & send to IBuffer */
  when(s3_reqIsMmio) {
    val inst = Cat(mmioData(1), mmioData(0))

    val (brType, isCall, isRet) = getBrInfo(inst)

    io.toIBuffer.bits.instrs(0) := Mux(mmioRvcExpander.io.ill, mmioRvcExpander.io.in, mmioRvcExpander.io.out.bits)

    io.toIBuffer.bits.pd(0).valid  := true.B
    io.toIBuffer.bits.pd(0).isRVC  := mmioIsRvc
    io.toIBuffer.bits.pd(0).brType := brType
    io.toIBuffer.bits.pd(0).isCall := isCall
    io.toIBuffer.bits.pd(0).isRet  := isRet

    io.toIBuffer.bits.exceptionType(0) := mmioException
    // exception can happen in next page only when resend
    io.toIBuffer.bits.crossPageIPFFix(0) := mmioHasResend && mmioException.hasException
    io.toIBuffer.bits.illegalInstr(0)    := mmioRvcExpander.io.ill

    io.toIBuffer.bits.enqEnable := s3_mmioRange.asUInt

    mmioFlushWb.bits.pd(0).valid  := true.B
    mmioFlushWb.bits.pd(0).isRVC  := mmioIsRvc
    mmioFlushWb.bits.pd(0).brType := brType
    mmioFlushWb.bits.pd(0).isCall := isCall
    mmioFlushWb.bits.pd(0).isRet  := isRet
  }

  mmioRedirect := s3_reqIsMmio && mmioState === MmioFsmState.WaitCommit && RegNext(fromUncache.fire) && s3_mmioUseSnpc

  XSPerfAccumulate("fetch_bubble_ibuffer_not_ready", io.toIBuffer.valid && !io.toIBuffer.ready)

  /* *****************************************************************************
   * IFU Write-back Stage
   * - write back preDecode information to Ftq to update
   * - redirect if found fault prediction
   * - redirect if false hit last half (last PC is not start + 32 Bytes, but in the middle of an notCFI RVI instruction)
   * ***************************************************************************** */
  private val wbEnable = RegNext(s2_fire && !s2_flush) && !s3_reqIsMmio && !s3_flush
  private val wbValid  = RegNext(wbEnable, init = false.B)
  private val wbFtqReq = RegEnable(s3_ftqReq, wbEnable)

  private val wbCheckResultStage1 = RegEnable(checkerOutStage1, wbEnable)
  private val wbCheckResultStage2 = checkerOutStage2
  private val wbInstrRange        = RegEnable(io.toIBuffer.bits.enqEnable, wbEnable)

  private val wbPcLowerResult = RegEnable(s3_pcLowerResult, wbEnable)
  private val wbPcHigh        = RegEnable(s3_pcHigh, wbEnable)
  private val wbPcHighPlus1   = RegEnable(s3_pcHighPlus1, wbEnable)
  private val wbPc            = catPC(wbPcLowerResult, wbPcHigh, wbPcHighPlus1)
  private val wbPd            = RegEnable(s3_pd, wbEnable)
  private val wbInstrValid    = RegEnable(s3_instrValid, wbEnable)

  /* false hit lastHalf */
  private val wbLastIdx       = RegEnable(s3_lastValidIdx, wbEnable)
  private val wbFalseLastHalf = RegEnable(s3_falseLastHalf, wbEnable) && wbLastIdx =/= (PredictWidth - 1).U
  private val wbFalseTarget   = RegEnable(s3_falseSnpc, wbEnable)

  private val wbHalfFlush  = wbFalseLastHalf
  private val wbHalfTarget = wbFalseTarget

  /* false oversize */
//  private val lastIsRVC = wbInstrRange.asTypeOf(Vec(PredictWidth, Bool())).last && wbPd.last.isRVC
//  private val lastIsRVI =
//    wbInstrRange.asTypeOf(Vec(PredictWidth, Bool()))(PredictWidth - 2) && !wbPd(PredictWidth - 2).isRVC
//  private val lastTaken = wbCheckResultStage1.fixedTaken.last

  s3_wbNotFlush := wbFtqReq.ftqIdx === s3_ftqReq.ftqIdx && s3_valid && wbValid

  /** if a req with a last half but miss predicted enters in wb stage, and this cycle f3 stalls,
    * we set a flag to notify f3 that the last half flag need not be set.
    */
  // s3_fire is after wbValid
  when(
    wbValid && RegNext(s3_hasLastHalf, false.B) && wbCheckResultStage2.fixedMissPred(PredictWidth - 1) && !s3_fire &&
      !RegNext(s3_fire, false.B) && !s3_flush
  ) {
    s3_lastHalfDisabled := true.B
  }

  // wbValid and s3_fire are in same cycle
  when(wbValid && RegNext(s3_hasLastHalf, false.B) && wbCheckResultStage2.fixedMissPred(PredictWidth - 1) && s3_fire) {
    s3_lastHalf.valid := false.B
  }

  private val checkFlushWb = Wire(Valid(new PredecodeWritebackBundle))
  private val checkFlushWbJalTargetIdx = ParallelPriorityEncoder(VecInit(wbPd.zip(wbInstrValid).map { case (pd, v) =>
    v && pd.isJal
  }))
  private val checkFlushWbTargetIdx = ParallelPriorityEncoder(wbCheckResultStage2.fixedMissPred)
  checkFlushWb.valid   := wbValid
  checkFlushWb.bits.pc := wbPc
  checkFlushWb.bits.pd := wbPd
  checkFlushWb.bits.pd.zipWithIndex.foreach { case (instr, i) => instr.valid := wbInstrValid(i) }
  checkFlushWb.bits.ftqIdx          := wbFtqReq.ftqIdx
  checkFlushWb.bits.ftqOffset       := wbFtqReq.ftqOffset.bits
  checkFlushWb.bits.misOffset.valid := ParallelOR(wbCheckResultStage2.fixedMissPred) || wbHalfFlush
  checkFlushWb.bits.misOffset.bits := Mux(
    wbHalfFlush,
    wbLastIdx,
    ParallelPriorityEncoder(wbCheckResultStage2.fixedMissPred)
  )
  checkFlushWb.bits.cfiOffset.valid := ParallelOR(wbCheckResultStage1.fixedTaken)
  checkFlushWb.bits.cfiOffset.bits  := ParallelPriorityEncoder(wbCheckResultStage1.fixedTaken)
  checkFlushWb.bits.target := Mux(
    wbHalfFlush,
    wbHalfTarget,
    wbCheckResultStage2.fixedTarget(checkFlushWbTargetIdx)
  )
  checkFlushWb.bits.jalTarget  := wbCheckResultStage2.jalTarget(checkFlushWbJalTargetIdx)
  checkFlushWb.bits.instrRange := wbInstrRange.asTypeOf(Vec(PredictWidth, Bool()))

  toFtq.pdWb := Mux(wbValid, checkFlushWb, mmioFlushWb)

  wbRedirect := checkFlushWb.bits.misOffset.valid && wbValid

  /* write back flush type */
  private val checkFaultType    = wbCheckResultStage2.faultType
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
    wbFtqReq.startAddr.toUInt,
    wbFtqReq.nextStartAddr.toUInt,
    wbFtqReq.ftqOffset.valid,
    wbFtqReq.ftqOffset.bits
  )

  /* *** Perf *** */
  private val s3_perfInfo = RegEnable(s2_perfInfo, s2_fire)
  private val s3_req0     = io.toIBuffer.fire
  private val s3_req1     = io.toIBuffer.fire && s3_doubleline
  private val s3_hit0     = io.toIBuffer.fire && s3_perfInfo.bankHit(0)
  private val s3_hit1     = io.toIBuffer.fire && s3_doubleline & s3_perfInfo.bankHit(1)
  private val s3_hit      = s3_perfInfo.hit
  val perfEvents: Seq[(String, Bool)] = Seq(
    ("frontendFlush                ", wbRedirect),
    ("ifu_req                      ", io.toIBuffer.fire),
    ("ifu_miss                     ", io.toIBuffer.fire && !s3_perfInfo.hit),
    ("ifu_req_cacheline_0          ", s3_req0),
    ("ifu_req_cacheline_1          ", s3_req1),
    ("ifu_req_cacheline_0_hit      ", s3_hit1),
    ("ifu_req_cacheline_1_hit      ", s3_hit1),
    ("only_0_hit                   ", s3_perfInfo.only0Hit && io.toIBuffer.fire),
    ("only_0_miss                  ", s3_perfInfo.only0Miss && io.toIBuffer.fire),
    ("hit_0_hit_1                  ", s3_perfInfo.hit0Hit1 && io.toIBuffer.fire),
    ("hit_0_miss_1                 ", s3_perfInfo.hit0Miss1 && io.toIBuffer.fire),
    ("miss_0_hit_1                 ", s3_perfInfo.miss0Hit1 && io.toIBuffer.fire),
    ("miss_0_miss_1                ", s3_perfInfo.miss0Miss1 && io.toIBuffer.fire)
  )
  generatePerfEvent()

  XSPerfAccumulate("ifu_req", io.toIBuffer.fire)
  XSPerfAccumulate("ifu_miss", io.toIBuffer.fire && !s3_hit)
  XSPerfAccumulate("ifu_req_cacheline_0", s3_req0)
  XSPerfAccumulate("ifu_req_cacheline_1", s3_req1)
  XSPerfAccumulate("ifu_req_cacheline_0_hit", s3_hit0)
  XSPerfAccumulate("ifu_req_cacheline_1_hit", s3_hit1)
  XSPerfAccumulate("frontendFlush", wbRedirect)
  XSPerfAccumulate("only_0_hit", s3_perfInfo.only0Hit && io.toIBuffer.fire)
  XSPerfAccumulate("only_0_miss", s3_perfInfo.only0Miss && io.toIBuffer.fire)
  XSPerfAccumulate("hit_0_hit_1", s3_perfInfo.hit0Hit1 && io.toIBuffer.fire)
  XSPerfAccumulate("hit_0_miss_1", s3_perfInfo.hit0Miss1 && io.toIBuffer.fire)
  XSPerfAccumulate("miss_0_hit_1", s3_perfInfo.miss0Hit1 && io.toIBuffer.fire)
  XSPerfAccumulate("miss_0_miss_1", s3_perfInfo.miss0Miss1 && io.toIBuffer.fire)
  XSPerfAccumulate("hit_0_except_1", s3_perfInfo.hit0Except1 && io.toIBuffer.fire)
  XSPerfAccumulate("miss_0_except_1", s3_perfInfo.miss0Except1 && io.toIBuffer.fire)
  XSPerfAccumulate("except_0", s3_perfInfo.except0 && io.toIBuffer.fire)
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
  fetchIBufferDumpData.startAddr  := s3_ftqReq.startAddr.toUInt
  fetchIBufferDumpData.instrCount := PopCount(io.toIBuffer.bits.enqEnable)
  fetchIBufferDumpData.exception :=
    (s3_perfInfo.except0 && io.toIBuffer.fire) ||
      (s3_perfInfo.hit0Except1 && io.toIBuffer.fire) ||
      (s3_perfInfo.miss0Except1 && io.toIBuffer.fire)
  fetchIBufferDumpData.isCacheHit := s3_hit

  private val ifuWbToFtqDumpData = Wire(new IfuWbToFtqDB)
  ifuWbToFtqDumpData.startAddr         := wbFtqReq.startAddr.toUInt
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
