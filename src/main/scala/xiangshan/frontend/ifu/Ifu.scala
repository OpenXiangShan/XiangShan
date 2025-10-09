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
import xiangshan.frontend.FrontendRedirect
import xiangshan.frontend.FrontendTopDownBundle
import xiangshan.frontend.FtqToIfuIO
import xiangshan.frontend.ICacheToIfuIO
import xiangshan.frontend.IfuToBackendIO
import xiangshan.frontend.IfuToFtqIO
import xiangshan.frontend.IfuToICacheIO
import xiangshan.frontend.IfuToInstrUncacheIO
import xiangshan.frontend.InstrUncacheToIfuIO
import xiangshan.frontend.PreDecodeInfo
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.PrunedAddrInit
import xiangshan.frontend.bpu.BranchAttribute
import xiangshan.frontend.ibuffer.IBufPtr
import xiangshan.frontend.icache.PmpCheckBundle

class Ifu(implicit p: Parameters) extends IfuModule
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
  io.itlb.req.valid  := false.B
  io.itlb.req.bits   := DontCare
  io.itlb.req_kill   := false.B
  io.itlb.resp.ready := true.B
  io.pmp.req.valid   := false.B
  io.pmp.req.bits    := DontCare
  // submodule

  private val preDecoder         = Module(new PreDecode)
  private val instrBoundary      = Module(new InstrBoundary)
  private val predChecker        = Module(new PredChecker)
  private val frontendTrigger    = Module(new FrontendTrigger)
  private val rvcExpanders       = Seq.fill(IBufferEnqueueWidth)(Module(new RvcExpander))
  private val perfAnalyzer       = Module(new IfuPerfAnalysis)
  private val instrCompactor     = Module(new InstrCompact)
  private val uncacheUnit        = Module(new IfuUncacheUnit)
  private val uncacheRvcExpander = Module(new RvcExpander)

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
  private val s0_flushFromBpu, s1_flushFromBpu                 = Wire(Vec(FetchPorts, Bool()))

  /* *****************************************************************************
   * IFU Stage 0
   *
   * - Sends cacheline fetch requests to ICacheMainPipe.
   *
   * **************************************************************************** */
  private val s0_ftqFetch   = fromFtq.req.bits.fetch
  private val s0_doubleline = VecInit(fromFtq.req.bits.fetch.map(_.crossCacheline))

  s0_fire := fromFtq.req.fire

  s0_flushFromBpu := s0_ftqFetch.map(fetch =>
    fromFtq.flushFromBpu.shouldFlushByStage3(fetch.ftqIdx, fetch.valid)
  )

  private val backendRedirect             = WireInit(false.B)
  private val wbRedirect, uncacheRedirect = WireInit(0.U.asTypeOf(new IfuRedirectInternal))

  private val s4_wbNotFlush = WireInit(false.B)

  backendRedirect := fromFtq.redirect.valid
  s4_flush        := backendRedirect || (wbRedirect.valid && !s4_wbNotFlush)
  s3_flush        := backendRedirect || uncacheRedirect.valid || wbRedirect.valid
  s2_flush        := s3_flush
  s1_flush        := s2_flush || s1_flushFromBpu(0)
  s0_flush        := s1_flush || s0_flushFromBpu(0)

  fromFtq.req.ready := s1_ready && io.fromICache.fetchReady
  private val s0_fetchBlock = VecInit.tabulate(FetchPorts)(i => Wire(new FetchBlockInfo).fromFtqRequest(s0_ftqFetch(i)))

  /* *****************************************************************************
   * IFU Stage 1
   * - icache response data (latched for pipeline stop)
   * - calculate pc/half_pc/cut_ptr for every instruction
   * ***************************************************************************** */
  private val s1_valid      = ValidHold(s0_fire && !s0_flush, s1_fire, s1_flush)
  private val s1_firstValid = ValidHold(s0_fire && !s0_flush && s0_ftqFetch(0).valid, s1_fire, s1_flush)
  private val s1_secondValid =
    ValidHold(s0_fire && !s0_flush && s0_ftqFetch(1).valid && !s0_flushFromBpu(1), s1_fire, s1_flush)
  private val s1_fetchBlock = RegEnable(s0_fetchBlock, s0_fire)
  private val firstSize     = s1_fetchBlock(0).fetchSize
  private val secondSize    = s1_fetchBlock(1).fetchSize
  private val firstRange    = s1_fetchBlock(0).instrRange
  private val secondRange   = s1_fetchBlock(1).instrRange

  s1_flushFromBpu(0) := fromFtq.flushFromBpu.shouldFlushByStage3(s1_fetchBlock(0).ftqIdx, s1_firstValid)
  s1_flushFromBpu(1) := fromFtq.flushFromBpu.shouldFlushByStage3(s1_fetchBlock(1).ftqIdx, s1_secondValid)

  private val s1_iCacheRespValid = fromICache.valid

  s1_fire  := s1_valid && s2_ready && s1_iCacheRespValid
  s1_ready := s1_fire || !s1_valid

  private val s1_totalEndPos = Mux(s1_firstValid && s1_secondValid, firstSize + secondSize - 1.U, firstSize - 1.U)

  private val s1_firstEndPos     = s1_fetchBlock(0).fetchSize - 1.U
  private val s1_secondEndPos    = s1_fetchBlock(1).fetchSize - 1.U
  private val s1_totalInstrRange = mergeInstrRange(s1_secondValid, firstRange, secondRange, firstSize)

  io.toICache.stall := !s2_ready
  iCacheMatchAssert(fromICache, s1_fetchBlock)

  private val s1_icacheMeta = VecInit.tabulate(FetchPorts)(i => Wire(new ICacheMeta).fromICacheResp(fromICache.bits))
  private val s1_rawData    = fromICache.bits.data
  private val s1_perfInfo   = io.fromICache.perf
  /* *****************************************************************************
   * IFU Stage 2
   * - generate exception bits for every instruction (page fault/access fault/mmio)
   * - generate predicted instruction range (1 means this instruction is in this fetch packet)
   * - cut data from cachelines to packet instruction code
   * - instruction preDecode and RVC expand
   * ***************************************************************************** */
  private val s2_valid             = ValidHold(s1_fire && !s1_flush, s2_fire, s2_flush)
  private val s2_firstValid        = ValidHold(s1_fire && !s1_flush && s1_firstValid, s2_fire, s2_flush)
  private val s2_secondValid       = ValidHold(s1_fire && !s1_flush && s1_secondValid, s2_fire, s2_flush)
  private val s2_fetchBlock        = RegEnable(s1_fetchBlock, s1_fire)
  private val s2_prevLastIsHalfRvi = RegInit(false.B)
  private val s2_totalInstrRange   = RegEnable(s1_totalInstrRange, s1_fire)
  private val s2_firstEndPos       = RegEnable(s1_firstEndPos, s1_fire)
  private val s2_totalEndPos       = RegEnable(s1_totalEndPos, s1_fire)

  s2_fire  := s2_valid && s3_ready
  s2_ready := s2_fire || !s2_valid

  private val s2_icacheMeta = RegEnable(s1_icacheMeta, s1_fire)
  private val s2_rawData    = RegEnable(s1_rawData, s1_fire)

  instrBoundary.io.req.valid                 := s2_valid
  instrBoundary.io.req.instrRange            := s2_totalInstrRange.asTypeOf(Vec(FetchBlockInstNum, Bool()))
  instrBoundary.io.req.firstFetchBlockEndPos := s2_firstEndPos
  instrBoundary.io.req.endPos                := s2_totalEndPos
  instrBoundary.io.req.firstInstrIsHalfRvi   := s2_prevLastIsHalfRvi
  instrBoundary.io.req.cacheData := Cat(s2_rawData, s2_rawData) >> Cat(s2_fetchBlock(0).startVAddr(5, 0), 0.U(3.W))

  private val s2_firstFetchEndIsHalf = instrBoundary.io.resp.firstFetchBlockLastInstrIsHalfRvi
  private val s2_fetchEndIsHalf      = instrBoundary.io.resp.lastInstrIsHalfRvi

  // When invalidTaken is true, we can not flush s2_prevLastIsHalfRvi because the fetch block after it is fall-through.
  when(backendRedirect) {
    s2_prevLastIsHalfRvi := false.B
  }.elsewhen(wbRedirect.valid) {
    s2_prevLastIsHalfRvi := wbRedirect.isHalfInstr
  }.elsewhen(uncacheRedirect.valid) {
    s2_prevLastIsHalfRvi := false.B
  }.elsewhen(s2_fire && !s2_icacheMeta(0).isUncache) {
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
  instrCountBeforeCurrent(FetchBlockInstNum)    := PopCount(dealInstrValid)
  instrCompactor.io.req.fetchSize(0)            := s2_fetchBlock(0).fetchSize
  instrCompactor.io.req.fetchSize(1)            := s2_fetchBlock(1).fetchSize
  instrCompactor.io.req.startVAddr(0)           := s2_fetchBlock(0).startVAddr
  instrCompactor.io.req.startVAddr(1)           := s2_fetchBlock(1).startVAddr
  instrCompactor.io.req.rawInstrValid           := dealInstrValid
  instrCompactor.io.req.rawIsRvc                := rawIsRvc
  instrCompactor.io.req.instrCountBeforeCurrent := instrCountBeforeCurrent
  private val instrCompactInfo = Wire(new InstrCompactBundle(FetchBlockInstNum))
  instrCompactInfo                   := instrCompactor.io.resp
  instrCompactInfo.instrEndOffset(0) := Mux(s2_prevLastIsHalfRvi, 0.U, Mux(rawIsRvc(0), 0.U, 1.U))

  private val s2_firstRange    = s2_fetchBlock(0).instrRange
  private val s2_secondRange   = s2_fetchBlock(1).instrRange
  private val s2_firstSize     = s2_fetchBlock(0).fetchSize
  private val s2_fetchTakenIdx = Wire(Vec(FetchPorts, new Valid(UInt(FetchBlockInstOffsetWidth.W))))
  s2_fetchTakenIdx(0).bits  := PopCount(dealInstrValid.asUInt & s2_firstRange) - 1.U
  s2_fetchTakenIdx(1).bits  := PopCount(dealInstrValid.asUInt & s2_totalInstrRange) - 1.U
  s2_fetchTakenIdx(0).valid := s2_fetchBlock(0).takenCfiOffset.valid && s2_firstValid
  s2_fetchTakenIdx(1).valid := s2_fetchBlock(1).takenCfiOffset.valid && s2_secondValid

  private val s2_realFetchBlock = Wire(Vec(FetchPorts, new FetchBlockInfo))
  s2_realFetchBlock                 := s2_fetchBlock
  s2_realFetchBlock(0).predTakenIdx := s2_fetchTakenIdx(0)
  s2_realFetchBlock(1).predTakenIdx := s2_fetchTakenIdx(1)
  s2_realFetchBlock(0).invalidTaken := !rawInstrEndVec(s2_fetchBlock(0).takenCfiOffset.bits) &&
    s2_fetchBlock(0).takenCfiOffset.valid
  s2_realFetchBlock(1).invalidTaken := !rawInstrEndVec(s2_fetchBlock(1).takenCfiOffset.bits) &&
    s2_fetchBlock(1).takenCfiOffset.valid
  s2_realFetchBlock(0).takenCfiOffset.valid := s2_fetchBlock(0).takenCfiOffset.valid && s2_firstValid
  s2_realFetchBlock(1).takenCfiOffset.valid := s2_fetchBlock(1).takenCfiOffset.valid && s2_secondValid
  s2_realFetchBlock(0).rawInstrEndVec       := rawInstrEndVec.asUInt & s2_firstRange
  s2_realFetchBlock(1).rawInstrEndVec       := (rawInstrEndVec.asUInt >> s2_firstSize).asUInt & s2_secondRange
  private val s2_rawFirstData         = s2_rawData
  private val s2_rawSecondData        = 0.U((ICacheLineBytes * 8).W)
  private val s2_rawFirstDataDupWire  = VecInit(Seq.fill(FetchPorts)(s2_rawFirstData))
  private val s2_rawSecondDataDupWire = VecInit(Seq.fill(FetchPorts)(s2_rawSecondData))
  // Special case for MMIO:
  // If two fetches occur and the first is non-MMIO while the second is MMIO,
  // delay the second fetch by one cycle to split into a one-fetch.

  /* *****************************************************************************
   * IFU Stage 3
   * ***************************************************************************** */
  private val s3_valid          = ValidHold(s2_fire && !s2_flush, s3_fire, s3_flush)
  private val s3_firstValid     = ValidHold(s2_fire && !s2_flush && s2_firstValid, s3_fire, s3_flush)
  private val s3_secondValid    = ValidHold(s2_fire && !s2_flush && s2_secondValid, s3_fire, s3_flush)
  private val s3_fetchBlock     = RegEnable(s2_realFetchBlock, s2_fire)
  private val s3_prevIBufEnqPtr = RegInit(0.U.asTypeOf(new IBufPtr))

  private val s3_prevShiftSelect = UIntToMask(s3_prevIBufEnqPtr.value(1, 0), IfuAlignWidth)

  s3_fire  := s3_valid && s4_ready
  s3_ready := s3_fire || !s3_valid

  private val s3_instrCompactInfo = RegEnable(instrCompactInfo, s2_fire)
  private val s3_instrCount       = RegEnable(PopCount(rawInstrEndVec), s2_fire)
  private val s3_instrValid       = RegEnable(UIntToMask(PopCount(rawInstrEndVec), FetchBlockInstNum), s2_fire)

  private val s3_rawIndex           = RegEnable(instrCountBeforeCurrent, s2_fire)
  private val s3_rawInstrEndVec     = RegEnable(rawInstrEndVec, s2_fire)
  private val s3_prevLastIsHalfRvi  = RegEnable(s2_prevLastIsHalfRvi, s2_fire)
  private val s3_prevLastHalfData   = RegInit(0.U(16.W))
  private val s3_prevLastHalfPc     = RegInit(0.U.asTypeOf(PrunedAddr(VAddrBits)))
  private val s3_currentLastHalfRvi = RegEnable(s2_fetchEndIsHalf, s2_fire)

  private val s3_rawFirstDataDup  = RegEnable(s2_rawFirstDataDupWire, s2_fire)
  private val s3_rawSecondDataDup = RegEnable(s2_rawSecondDataDupWire, s2_fire)

  private val s3_icacheMeta = RegEnable(s2_icacheMeta, s2_fire)
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
      !s3_instrCompactInfo.selectBlock(i) && s3_firstValid) ||
      ((s3_fetchBlock(1).predTakenIdx.bits === i.U && s3_fetchBlock(1).predTakenIdx.valid) &&
        s3_instrCompactInfo.selectBlock(i) && s3_secondValid)
  )

  private val s3_invalidTaken = VecInit.tabulate(FetchBlockInstNum)(i =>
    ((s3_fetchBlock(0).predTakenIdx.bits === i.U && s3_fetchBlock(0).invalidTaken) &&
      !s3_instrCompactInfo.selectBlock(i) && s3_firstValid) ||
      ((s3_fetchBlock(1).predTakenIdx.bits === i.U && s3_fetchBlock(1).invalidTaken) &&
        s3_instrCompactInfo.selectBlock(i) && s3_secondValid)
  )

  private val s3_alignShiftNum = s3_prevIBufEnqPtr.value(1, 0)
  // Maybe it's better to move the calculation of enqBlockStartPos to the previous pipeline stage
  // — at least from a timing perspective. But it would require modifying IBufferPrevPtr.
  private val s3_alignBlockStartPos = WireDefault(VecInit.fill(IBufferEnqueueWidth)(false.B))
  s3_alignBlockStartPos(s3_alignShiftNum) := true.B
  private val s3_alignCompactInfo = alignInstrCompact(s3_instrCompactInfo, s3_alignShiftNum)
  private val s3_alignInstrData   = WireDefault(VecInit.fill(IBufferEnqueueWidth)(0.U(32.W)))
  private val s3_alignInstrValid =
    alignData(s3_instrValid.asTypeOf(Vec(FetchBlockInstNum, Bool())), s3_alignShiftNum, false.B)
  private val s3_alignInvalidTaken = alignData(s3_invalidTaken, s3_alignShiftNum, false.B)
  private val s3_alignIsPredTaken  = alignData(s3_isPredTaken, s3_alignShiftNum, false.B)
  private val s3_alignPc = VecInit.tabulate(IBufferEnqueueWidth)(i =>
    catPC(
      s3_alignCompactInfo.instrPcLower(i),
      Mux(s3_alignCompactInfo.selectBlock(i), s3_fetchBlock(1).pcHigh, s3_fetchBlock(0).pcHigh),
      Mux(s3_alignCompactInfo.selectBlock(i), s3_fetchBlock(1).pcHighPlus1, s3_fetchBlock(0).pcHighPlus1)
    )
  )
  private val s3_realAlignInstrData = WireDefault(VecInit.fill(IBufferEnqueueWidth)(0.U(32.W)))
  private val s3_realAlignPc = WireDefault(VecInit.fill(IBufferEnqueueWidth)(0.U.asTypeOf(PrunedAddr(VAddrBits))))
  private val s3_alignFoldPc = VecInit(s3_realAlignPc.map(i => XORFold(i(VAddrBits - 1, 1), MemPredPCWidth)))

  for (i <- 0 until IBufferEnqueueWidth / 2) {
    val lowIdx     = s3_alignCompactInfo.instrIndex(i).value
    val highIdx    = s3_alignCompactInfo.instrIndex(i + IBufferEnqueueWidth / 2).value
    val j          = i + IBufferEnqueueWidth / 2
    val lowSelect  = s3_alignCompactInfo.selectBlock(i)
    val highSelect = s3_alignCompactInfo.selectBlock(j)
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
  }.elsewhen(uncacheRedirect.valid) {
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
  }.elsewhen(uncacheRedirect.valid) {
    s3_prevIBufEnqPtr := uncacheRedirect.prevIBufEnqPtr + uncacheRedirect.instrCount
  }.elsewhen(s3_fire && !s3_icacheMeta(0).isUncache) {
    s3_prevIBufEnqPtr := s3_prevIBufEnqPtr + s3_instrCount
  }

  // PreDecode: delimitation, does not expand RVC
  preDecoderIn.valid           := s3_valid
  preDecoderIn.bits.data       := s3_realAlignInstrData
  preDecoderIn.bits.isRvc      := s3_alignCompactInfo.instrIsRvc
  preDecoderIn.bits.instrValid := s3_alignInstrValid // s3_instrValid.asTypeOf(Vec(FetchBlockInstNum, Bool()))

  private val s3_alignPd         = preDecoderOut.pd
  private val s3_alignJumpOffset = preDecoderOut.jumpOffset

  private val s3_reqIsUncache = s3_valid && s3_icacheMeta(0).isUncache &&
    s3_icacheMeta(0).exception.isNone
  private val s3_alignFetchBlock = Wire(Vec(FetchPorts, new FetchBlockInfo))
  s3_alignFetchBlock                      := s3_fetchBlock
  s3_alignFetchBlock(0).predTakenIdx.bits := s3_fetchBlock(0).predTakenIdx.bits + s3_prevIBufEnqPtr.value(1, 0)
  s3_alignFetchBlock(1).predTakenIdx.bits := s3_fetchBlock(1).predTakenIdx.bits + s3_prevIBufEnqPtr.value(1, 0)

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
  private val s4_alignFetchBlock     = RegEnable(s3_alignFetchBlock, s3_fire)
  private val s4_prevIBufEnqPtr      = RegEnable(s3_prevIBufEnqPtr, s3_fire)
  private val s4_rawIndex            = RegEnable(s3_rawIndex, s3_fire)
  private val s4_prevShiftSelect     = RegEnable(s3_prevShiftSelect, s3_fire)
  private val s4_prevLastRvi         = RegEnable(s3_prevLastIsHalfRvi, s3_fire)
  private val s4_currentLastHalfData = RegEnable(s3_alignInstrData(s3_instrCount + s3_alignShiftNum)(15, 0), s3_fire)
  private val s4_currentLastHalfRvi  = RegEnable(s3_currentLastHalfRvi, s3_fire)
  private val s4_currentLastHalfPC   = RegEnable(s3_realAlignPc(s3_instrCount + s3_alignShiftNum), s3_fire)
  private val s4_instrCount          = RegEnable(s3_instrCount, s3_fire)
  s4_fire := io.toIBuffer.fire

  private val s4_alignInvalidTaken = RegEnable(s3_alignInvalidTaken, s3_fire)
  private val s4_alignIsPredTaken  = RegEnable(s3_alignIsPredTaken, s3_fire)
  private val s4_alignInstrData    = RegEnable(s3_realAlignInstrData, s3_fire)
  private val s4_alignInstrValid   = RegEnable(s3_alignInstrValid, s3_fire)
  private val s4_icacheMeta        = RegEnable(s3_icacheMeta, s3_fire)
  private val s4_alignCompactInfo  = RegEnable(s3_alignCompactInfo, s3_fire)
  private val isFirstInstr         = RegInit(true.B)
  when(isFirstInstr && io.toIBuffer.fire) {
    isFirstInstr := false.B
  }

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

  private val s4_alignPdWire     = RegEnable(s3_alignPd, s3_fire)
  private val s4_alignPds        = WireInit(s4_alignPdWire)
  private val s4_alignJumpOffset = RegEnable(s3_alignJumpOffset, s3_fire)

  private val s4_alignPc = VecInit.tabulate(IBufferEnqueueWidth)(i =>
    if (i < IfuAlignWidth) {
      RegEnable(s3_realAlignPc(i), s3_fire)
    } else {
      catPC(
        s4_alignCompactInfo.instrPcLower(i),
        Mux(s4_alignCompactInfo.selectBlock(i), s4_alignFetchBlock(1).pcHigh, s4_alignFetchBlock(0).pcHigh),
        Mux(s4_alignCompactInfo.selectBlock(i), s4_alignFetchBlock(1).pcHighPlus1, s4_alignFetchBlock(0).pcHighPlus1)
      )
    }
  )

  // Exapnd 1 bit to prevent overflow when assert
  private val s4_fetchStartAddr = VecInit.tabulate(FetchPorts)(i =>
    PrunedAddrInit(Cat(0.U(1.W), s4_alignFetchBlock(i).startVAddr.toUInt))
  )
  private val s4_fetchNextStartAddr = VecInit.tabulate(FetchPorts)(i =>
    PrunedAddrInit(Cat(0.U(1.W), s4_alignFetchBlock(i).target.toUInt))
  )
  for (i <- 0 until FetchPorts) {
    when(s4_valid && !s4_alignFetchBlock(i).takenCfiOffset.valid) {
      assert(
        s4_fetchStartAddr(i) + (2 * FetchBlockInstNum).U >= s4_fetchNextStartAddr(i),
        s"More than ${2 * FetchBlockInstNum} Bytes fetch is not allowed!"
      )
    }
  }

  private val s4_alignFoldPc        = RegEnable(s3_alignFoldPc, s3_fire)
  private val s4_rawInstrEndVec     = RegEnable(s3_rawInstrEndVec, s3_fire)
  private val s4_prevLastIsHalfRvi  = RegEnable(s3_prevLastIsHalfRvi, s3_fire)
  private val s4_uncacheLowerPc     = RegEnable(s3_alignCompactInfo.instrPcLower(s3_alignShiftNum), s3_fire)
  private val s4_alignBlockStartPos = RegEnable(s3_alignBlockStartPos, s3_fire)
  private val s4_uncachePc = catPC(s4_uncacheLowerPc, s4_alignFetchBlock(0).pcHigh, s4_alignFetchBlock(0).pcHighPlus1)
  private val s4_reqIsUncache         = RegEnable(s3_reqIsUncache, s3_fire)
  private val s4_uncacheCanGo         = uncacheUnit.io.resp.valid && !uncacheUnit.io.resp.bits.crossPage
  private val s4_uncacheCrossPageMask = s4_valid && uncacheUnit.io.resp.valid && uncacheUnit.io.resp.bits.crossPage
  private val s4_toIBufferValid = s4_valid && (!s4_reqIsUncache || (s4_uncacheCanGo && s4_reqIsUncache)) && !s4_flush
  private val s4_shiftNum       = s4_prevIBufEnqPtr.value(1, 0)
  private val s4_ignore         = s4_prevShiftSelect // possibly redundant, may remove later.

  /* ** unache state handle ** */
  private val uncacheBusy = RegInit(false.B)
  // Uncache cross-page across two fetch blocks, store the prev block’s cross-page flag and data.
  private val prevUncacheCrossPage = RegInit(false.B)
  private val prevUncacheData      = RegInit(0.U(16.W))
  // For uncache cross-page instr, the real PC is in the prev fetch block.
  private val uncachePc = RegInit(0.U.asTypeOf(PrunedAddr(VAddrBits)))
  // Uncache cross-page may hit seq fetch or mispred, check required.
  private val uncacheCrossPageCheck = RegInit(false.B)
  when(s4_flush) {
    uncacheBusy           := false.B
    uncachePc             := 0.U.asTypeOf(PrunedAddr(VAddrBits))
    uncacheCrossPageCheck := false.B
  }.elsewhen(uncacheUnit.io.req.fire) {
    uncacheBusy           := true.B
    uncachePc             := Mux(prevUncacheCrossPage, uncachePc, s4_alignPc(s4_shiftNum))
    uncacheCrossPageCheck := (s4_alignFetchBlock(0).startVAddr + 2.U) === s4_alignFetchBlock(0).target
  }.elsewhen(uncacheUnit.io.resp.valid) {
    uncacheBusy := false.B
    // uncachePc := uncachePc
    uncacheCrossPageCheck := false.B
  }

  uncacheUnit.io.req.valid       := s4_valid && s4_reqIsUncache && !uncacheBusy
  uncacheUnit.io.req.bits.ftqIdx := s4_alignFetchBlock(0).ftqIdx
  uncacheUnit.io.req.bits.pbmt   := s4_icacheMeta(0).itlbPbmt
  uncacheUnit.io.req.bits.isMmio := s4_icacheMeta(0).pmpMmio
  uncacheUnit.io.req.bits.paddr  := s4_icacheMeta(0).pAddr
  uncacheUnit.io.flush           := s4_flush
  uncacheUnit.io.isFirstInstr    := isFirstInstr
  uncacheUnit.io.ifuStall        := !io.toIBuffer.ready
  io.toFtq.mmioCommitRead <> uncacheUnit.io.mmioCommitRead
  io.toUncache <> uncacheUnit.io.toUncache
  uncacheUnit.io.fromUncache <> io.fromUncache

  private val uncacheData       = uncacheUnit.io.resp.bits.uncacheData
  private val uncacheException  = uncacheUnit.io.resp.bits.exception
  private val uncacheCrossPage  = uncacheUnit.io.resp.bits.crossPage
  private val uncacheCheckFault = uncacheCrossPage && !uncacheCrossPageCheck && uncacheUnit.io.resp.valid

  when(uncacheUnit.io.resp.valid) {
    prevUncacheCrossPage := uncacheCrossPage
    prevUncacheData      := uncacheData
  }

  private val s4_uncacheData = Mux(prevUncacheCrossPage, Cat(uncacheData(15, 0), prevUncacheData), uncacheData)
  private val uncacheIsRvc   = s4_uncacheData(1, 0) =/= "b11".U
  uncacheRvcExpander.io.in      := Mux(s4_reqIsUncache, s4_uncacheData, 0.U)
  uncacheRvcExpander.io.fsIsOff := io.csrFsIsOff

  s4_valid := ValidHold(
    // infire: s3 -> s4 fire
    s3_fire && !s3_flush,
    // outfire:
    io.toIBuffer.fire || s4_uncacheCrossPageMask,
    // On flush, waiting for uncache response is handled by the channel itself.
    s4_flush
  )

  s4_ready := (io.toIBuffer.ready && (s4_uncacheCanGo || !s4_reqIsUncache)) || !s4_valid

  /* ** prediction result check ** */
  checkerIn.valid                   := s4_valid
  checkerIn.bits.instrJumpOffset    := s4_alignJumpOffset
  checkerIn.bits.instrValid         := s4_alignInstrValid.asTypeOf(Vec(IBufferEnqueueWidth, Bool()))
  checkerIn.bits.instrPds           := s4_alignPds
  checkerIn.bits.instrPc            := s4_alignPc
  checkerIn.bits.isPredTaken        := s4_alignIsPredTaken
  checkerIn.bits.ignore             := s4_ignore.asBools
  checkerIn.bits.shiftNum           := s4_prevIBufEnqPtr.value(1, 0)
  checkerIn.bits.firstPredTakenIdx  := s4_alignFetchBlock(0).predTakenIdx
  checkerIn.bits.secondPredTakenIdx := s4_alignFetchBlock(1).predTakenIdx
  checkerIn.bits.firstTarget        := s4_alignFetchBlock(0).target
  checkerIn.bits.secondTarget       := s4_alignFetchBlock(1).target
  checkerIn.bits.selectFetchBlock   := s4_alignCompactInfo.selectBlock
  checkerIn.bits.invalidTaken       := s4_alignInvalidTaken
  checkerIn.bits.instrEndOffset     := s4_alignCompactInfo.instrEndOffset

  /* ** frontend Trigger  ** */
  frontendTrigger.io.pds             := s4_alignPds
  frontendTrigger.io.pc              := s4_alignPc
  frontendTrigger.io.data            := 0.U.asTypeOf(Vec(IBufferEnqueueWidth + 1, UInt(16.W))) // s4_alignInstrData
  frontendTrigger.io.frontendTrigger := io.frontendTrigger
  private val s4_alignTriggered = frontendTrigger.io.triggered

  /* ** send to IBuffer ** */
  private val ignoreRange = Cat(Fill(IBufferEnqueueWidth - IfuAlignWidth, 1.U(1.W)), ~s4_ignore) // maybe remove
  io.toIBuffer.valid          := s4_toIBufferValid
  io.toIBuffer.bits.instrs    := s4_alignExpdInstr
  io.toIBuffer.bits.valid     := s4_alignInstrValid.asUInt & ignoreRange
  io.toIBuffer.bits.enqEnable := checkerOutStage1.fixedTwoFetchRange.asUInt & s4_alignInstrValid.asUInt & ignoreRange
  io.toIBuffer.bits.pd        := s4_alignPds
  io.toIBuffer.bits.ftqPtr    := s4_alignFetchBlock(0).ftqIdx
  io.toIBuffer.bits.pc        := s4_alignPc
  io.toIBuffer.bits.prevIBufEnqPtr := s4_prevIBufEnqPtr

  /* in s4, prevInstrCount equals to next cycle's IBuffer.numFromFetch without predChecker. "prev" means s3;
   * when s3 fire (s4_valid && s4_ready), use s3_instrCount;
   * else when s4 stall (s4_valid && !s4_ready). use s4_instrCount because prevInstrCount equals to current instrCount;
   * otherwise, we don't care about prevInstrCount because next cycle's toIBuffer.valid won't set.
   */
  io.toIBuffer.bits.prevInstrCount := Mux(
    s3_fire,
    Mux(s3_reqIsUncache, 1.U, s3_instrCount), // FIXME: consider the second fetch block
    Mux(s4_reqIsUncache, 1.U, s4_instrCount)
  )

  // Find last using PriorityMux
  io.toIBuffer.bits.isLastInFtqEntry := Reverse(PriorityEncoderOH(Reverse(io.toIBuffer.bits.enqEnable))).asBools
  io.toIBuffer.bits.instrEndOffset.zipWithIndex.foreach { case (a, i) =>
    a.taken  := checkerOutStage1.fixedTwoFetchTaken(i) && !s4_reqIsUncache
    a.offset := s4_alignCompactInfo.instrEndOffset(i)
  }
  io.toIBuffer.bits.foldpc := s4_alignFoldPc
  // mark the exception only on first instruction
  // TODO: store only the first exception in IBuffer, instead of store in every entry
  io.toIBuffer.bits.exceptionType := (0 until IBufferEnqueueWidth).map {
    i => Mux(i.U === s4_prevIBufEnqPtr.value(1, 0), s4_icacheMeta(0).exception, ExceptionType.None)
  }
  // backendException only needs to be set for the first instruction.
  // Other instructions in the same block may have pf or af set,
  // which is a side effect of the first instruction and actually not necessary.
  io.toIBuffer.bits.backendException := (0 until IBufferEnqueueWidth).map {
    i => i.U === s4_prevIBufEnqPtr.value(1, 0) && s4_icacheMeta(0).isBackendException
  }
  // if we have last half RV-I instruction, and has exception, we need to tell backend to caculate the correct pc
  io.toIBuffer.bits.crossPageIPFFix := (0 until IBufferEnqueueWidth).map {
    i => i.U === s4_prevIBufEnqPtr.value(1, 0) && s4_icacheMeta(0).exception.hasException && s4_prevLastIsHalfRvi
  }
  io.toIBuffer.bits.illegalInstr := s4_alignIll
  io.toIBuffer.bits.triggered    := s4_alignTriggered

  val enqVec = io.toIBuffer.bits.enqEnable
  val allocateSeqNum = VecInit((0 until IBufferEnqueueWidth).map { i =>
    val idx  = PopCount(enqVec.take(i + 1))
    val pc   = s4_alignPc(i).toUInt
    val code = io.toIBuffer.bits.instrs(i)
    PerfCCT.createInstMetaAtFetch(idx, pc, code, s4_fire & enqVec(i), clock, reset)
  })
  io.toIBuffer.bits.debug_seqNum.zipWithIndex.foreach { case (seqNum, i) =>
    seqNum := Mux(s4_fire, allocateSeqNum(i), 0.U)
  }

  /** to backend */
  // s4_gpAddr is valid iff gpf is detected.
  // Uncache doesn’t request iTLB; it only returns bus exceptions.
  io.toBackend.gpAddrMem.wen                     := s4_toIBufferValid && s4_icacheMeta(0).exception.isGpf
  io.toBackend.gpAddrMem.waddr                   := s4_alignFetchBlock(0).ftqIdx.value
  io.toBackend.gpAddrMem.wdata.gpaddr            := s4_icacheMeta(0).gpAddr.toUInt
  io.toBackend.gpAddrMem.wdata.isForVSnonLeafPTE := s4_icacheMeta(0).isForVSnonLeafPTE

  // Write back to Ftq
  private val s4_uncacheRange     = VecInit((0 until FetchBlockInstNum).map(i => if (i == 0) true.B else false.B))
  private val uncacheFlushWb      = Wire(Valid(new FrontendRedirect))
  private val uncachePd           = 0.U.asTypeOf(Vec(FetchBlockInstNum, new PreDecodeInfo))
  private val uncacheMisEndOffset = Wire(Valid(UInt(FetchBlockInstOffsetWidth.W)))
  uncacheMisEndOffset.valid := s4_reqIsUncache
  uncacheMisEndOffset.bits  := Mux(prevUncacheCrossPage || uncacheIsRvc || uncacheCheckFault, 0.U, 1.U)

  // Send mmioFlushWb back to FTQ 1 cycle after uncache fetch return
  // When backend redirect, mmioState reset after 1 cycle.
  // In this case, mask .valid to avoid overriding backend redirect
  private val uncacheTarget =
    Mux(
      uncacheIsRvc || prevUncacheCrossPage || uncacheCheckFault,
      s4_alignFetchBlock(0).startVAddr + 2.U,
      s4_alignFetchBlock(0).startVAddr + 4.U
    )
  uncacheFlushWb.valid          := s4_reqIsUncache && !backendRedirect && (s4_uncacheCanGo || uncacheCheckFault)
  uncacheFlushWb.bits.ftqIdx    := s4_alignFetchBlock(0).ftqIdx
  uncacheFlushWb.bits.pc        := s4_alignFetchBlock(0).startVAddr.toUInt
  uncacheFlushWb.bits.taken     := false.B
  uncacheFlushWb.bits.ftqOffset := uncacheMisEndOffset.bits
  uncacheFlushWb.bits.isRVC     := uncacheIsRvc
  uncacheFlushWb.bits.attribute := BranchAttribute.None
  uncacheFlushWb.bits.target    := uncacheTarget.toUInt

  when(s4_reqIsUncache) {
    val inst                    = s4_uncacheData
    val (brType, isCall, isRet) = getBrInfo(inst)
    io.toIBuffer.bits.instrs(s4_shiftNum) := Mux(
      uncacheRvcExpander.io.ill,
      uncacheRvcExpander.io.in,
      uncacheRvcExpander.io.out.bits
    )

    io.toIBuffer.bits.pc(s4_shiftNum)                    := uncachePc
    io.toIBuffer.bits.pd(s4_shiftNum).valid              := true.B
    io.toIBuffer.bits.pd(s4_shiftNum).isRVC              := uncacheIsRvc
    io.toIBuffer.bits.pd(s4_shiftNum).brType             := brType
    io.toIBuffer.bits.pd(s4_shiftNum).isCall             := isCall
    io.toIBuffer.bits.pd(s4_shiftNum).isRet              := isRet
    io.toIBuffer.bits.instrEndOffset(s4_shiftNum).offset := Mux(prevUncacheCrossPage || uncacheIsRvc, 0.U, 1.U)

    io.toIBuffer.bits.exceptionType(s4_shiftNum) := uncacheException
    // execption can happen in next page only when cross page.
    io.toIBuffer.bits.crossPageIPFFix(s4_shiftNum) := prevUncacheCrossPage && uncacheException.hasException
    io.toIBuffer.bits.illegalInstr(s4_shiftNum)    := uncacheRvcExpander.io.ill
    io.toIBuffer.bits.enqEnable                    := s4_alignBlockStartPos.asUInt

    uncacheFlushWb.bits.isRVC     := uncacheIsRvc
    uncacheFlushWb.bits.attribute := BranchAttribute(brType, Cat(isCall, isRet))
  }

  uncacheRedirect.valid          := s4_reqIsUncache && (s4_uncacheCanGo || uncacheCheckFault)
  uncacheRedirect.instrCount     := Mux(uncacheCheckFault, 0.U, 1.U)
  uncacheRedirect.prevIBufEnqPtr := s4_prevIBufEnqPtr
  uncacheRedirect.isHalfInstr    := false.B
  uncacheRedirect.halfPc         := 0.U.asTypeOf(PrunedAddr(VAddrBits))
  uncacheRedirect.halfData       := 0.U
  /* *****************************************************************************
   * IFU Write-back Stage
   * - write back preDecode information to Ftq to update
   * - redirect if found fault prediction
   * - redirect if false hit last half(last PC is not start + 32 Bytes, but in the middle of an notCFI RVI instruction)
   * ***************************************************************************** */
  private val wbEnable              = RegNext(s3_fire && !s3_flush) && !s4_reqIsUncache && !s4_flush
  private val wbValid               = RegNext(wbEnable, init = false.B)
  private val wbFirstValid          = RegEnable(s4_firstValid, wbEnable)
  private val wbSecondValid         = RegEnable(s4_secondValid, wbEnable)
  private val wbAlignFetchBlock     = RegEnable(s4_alignFetchBlock, wbEnable)
  private val wbPrevIBufEnqPtr      = RegEnable(s4_prevIBufEnqPtr, wbEnable)
  private val wbInstrCount          = RegEnable(PopCount(io.toIBuffer.bits.enqEnable), wbEnable)
  private val wbAlignCompactInfo    = RegEnable(s4_alignCompactInfo, wbEnable)
  private val wbAlignInstrPcLower   = wbAlignCompactInfo.instrPcLower
  private val wbAlignInstrEndOffset = wbAlignCompactInfo.instrEndOffset

  private val wbCurrentLastHalfData = RegEnable(s4_currentLastHalfData, wbEnable)
  private val wbCurrentLastHalfPc   = RegEnable(s4_currentLastHalfPC, wbEnable)
  private val wbCurrentLastRvi      = RegEnable(s4_currentLastHalfRvi, wbEnable)

  s4_wbNotFlush := wbAlignFetchBlock(0).ftqIdx === s4_alignFetchBlock(0).ftqIdx && s4_valid && wbValid

  private val checkerRedirect = checkerOutStage2.checkerRedirect
  private val checkFlushWb = {
    val b         = Wire(Valid(new FrontendRedirect))
    val missIdx   = checkerRedirect.bits.misIdx.bits
    val ftqIdx    = VecInit(wbAlignFetchBlock.map(_.ftqIdx))
    val startAddr = VecInit(wbAlignFetchBlock.map(_.startVAddr.toUInt))
    b.valid          := wbValid && checkerRedirect.valid
    b.bits.ftqIdx    := Mux(checkerRedirect.bits.selectBlock, ftqIdx(1), ftqIdx(0))
    b.bits.pc        := Mux(checkerRedirect.bits.selectBlock, startAddr(1), startAddr(0))
    b.bits.taken     := checkerRedirect.bits.taken
    b.bits.ftqOffset := checkerRedirect.bits.endOffset
    b.bits.isRVC     := checkerRedirect.bits.isRVC
    b.bits.attribute := checkerRedirect.bits.attribute
    b.bits.target    := checkerRedirect.bits.target.toUInt
    b
  }

  toFtq.wbRedirect := Mux(wbValid, checkFlushWb, uncacheFlushWb)

  wbRedirect.valid          := checkFlushWb.valid
  wbRedirect.isHalfInstr    := wbCurrentLastRvi && checkerRedirect.bits.invalidTaken
  wbRedirect.instrCount     := wbInstrCount
  wbRedirect.prevIBufEnqPtr := wbPrevIBufEnqPtr
  wbRedirect.halfPc         := checkerRedirect.bits.mispredPc
  wbRedirect.halfData       := wbCurrentLastHalfData

  private val s2_icachePerfInfo = RegEnable(io.fromICache.perf, s1_fire)
  private val s3_icachePerfInfo = RegEnable(s2_icachePerfInfo, s2_fire)
  private val s4_icachePerfInfo = RegEnable(s3_icachePerfInfo, s3_fire)

  val perfEvents: Seq[(String, Bool)] = Seq(
    ("frontendFlush                ", wbRedirect.valid),
    ("ifu_req                      ", io.toIBuffer.fire),
    ("ifu_miss                     ", io.toIBuffer.fire && !s4_icachePerfInfo.hit),
    ("ifu_req_cacheline_0          ", io.toIBuffer.fire),
    ("ifu_req_cacheline_1          ", io.toIBuffer.fire && s4_icachePerfInfo.isDoubleLine),
    ("ifu_req_cacheline_0_hit      ", io.toIBuffer.fire && s4_icachePerfInfo.hit0),
    ("ifu_req_cacheline_1_hit      ", io.toIBuffer.fire && s4_icachePerfInfo.hit1),
    ("only_0_hit                   ", io.toIBuffer.fire && s4_icachePerfInfo.hit0NoReq1),
    ("only_0_miss                  ", io.toIBuffer.fire && s4_icachePerfInfo.miss0NoReq1),
    ("hit_0_hit_1                  ", io.toIBuffer.fire && s4_icachePerfInfo.hit0Hit1),
    ("hit_0_miss_1                 ", io.toIBuffer.fire && s4_icachePerfInfo.hit0Miss1),
    ("miss_0_hit_1                 ", io.toIBuffer.fire && s4_icachePerfInfo.miss0Hit1),
    ("miss_0_miss_1                ", io.toIBuffer.fire && s4_icachePerfInfo.miss0Miss1)
  )
  generatePerfEvent()
  perfAnalyzer.io.ifuPerfCtrl.fromFtqBubble    := !fromFtq.req.valid && fromFtq.req.ready
  perfAnalyzer.io.ifuPerfCtrl.backendRedirect  := backendRedirect
  perfAnalyzer.io.ifuPerfCtrl.ifuWbRedirect    := wbRedirect.valid
  perfAnalyzer.io.ifuPerfCtrl.fromBpuFlush     := s0_flushFromBpu(0) || s1_flushFromBpu(0) // FIXME
  perfAnalyzer.io.ifuPerfCtrl.fromICacheBubble := s1_valid && !s1_iCacheRespValid

  perfAnalyzer.io.topdownIn.icacheTopdown   := io.fromICache.topdown
  perfAnalyzer.io.topdownIn.ftqTopdown      := fromFtq.req.bits.topdownInfo
  perfAnalyzer.io.topdownIn.topdownRedirect := fromFtq.topdownRedirect

  perfAnalyzer.io.perfInfo.icachePerfInfo                 := s4_icachePerfInfo
  perfAnalyzer.io.perfInfo.checkPerfInfo.valid(0)         := wbValid && wbFirstValid
  perfAnalyzer.io.perfInfo.checkPerfInfo.valid(1)         := wbValid && wbSecondValid
  perfAnalyzer.io.perfInfo.checkPerfInfo.perfFaultType(0) := checkerOutStage2.perfFaultType(0)
  perfAnalyzer.io.perfInfo.checkPerfInfo.perfFaultType(1) := checkerOutStage2.perfFaultType(1)
  perfAnalyzer.io.perfInfo.checkPerfInfo.startVAddr(0)    := wbAlignFetchBlock(0).startVAddr.toUInt
  perfAnalyzer.io.perfInfo.checkPerfInfo.startVAddr(1)    := wbAlignFetchBlock(1).startVAddr.toUInt
  perfAnalyzer.io.perfInfo.checkPerfInfo.target(0)        := wbAlignFetchBlock(0).target.toUInt
  perfAnalyzer.io.perfInfo.checkPerfInfo.target(1)        := wbAlignFetchBlock(1).target.toUInt
  perfAnalyzer.io.perfInfo.checkPerfInfo.taken(0)         := wbAlignFetchBlock(0).takenCfiOffset.valid
  perfAnalyzer.io.perfInfo.checkPerfInfo.taken(1)         := wbAlignFetchBlock(1).takenCfiOffset.valid
  perfAnalyzer.io.perfInfo.checkPerfInfo.misPred          := checkerRedirect.valid
  perfAnalyzer.io.perfInfo.checkPerfInfo.selectBlock      := checkerRedirect.bits.selectBlock
  perfAnalyzer.io.perfInfo.checkPerfInfo.misEndOffset     := checkerRedirect.bits.endOffset

  perfAnalyzer.io.perfInfo.toIBufferInfo.ibufferFire   := io.toIBuffer.fire
  perfAnalyzer.io.perfInfo.toIBufferInfo.enqEnable     := io.toIBuffer.bits.enqEnable & io.toIBuffer.bits.valid
  perfAnalyzer.io.perfInfo.toIBufferInfo.startVAddr(0) := s4_alignFetchBlock(0).startVAddr.toUInt
  perfAnalyzer.io.perfInfo.toIBufferInfo.startVAddr(1) := s4_alignFetchBlock(1).startVAddr.toUInt
  io.toIBuffer.bits.topdownInfo                        := perfAnalyzer.io.topdownOut.topdown
}
