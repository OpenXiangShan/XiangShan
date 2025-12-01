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
import utility.HasCircularQueuePtrHelper
import utility.HasPerfEvents
import utility.InstSeqNum
import utility.PerfCCT
import utility.UIntToMask
import utility.ValidHold
import utility.XORFold
import xiangshan.FrontendTdataDistributeIO
import xiangshan.cache.mmu.HasTlbConst
import xiangshan.cache.mmu.TlbRequestIO
import xiangshan.frontend.ExceptionType
import xiangshan.frontend.FetchToIBuffer
import xiangshan.frontend.FrontendRedirect
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

    // debug extension: frontend trigger
    val frontendTrigger: FrontendTdataDistributeIO = Flipped(new FrontendTdataDistributeIO)

    // Backend: csr control
    val csrFsIsOff: Bool = Input(Bool())
  }
  val io: IfuIO = IO(new IfuIO)

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

  private val s1_ready, s2_ready, s3_ready           = WireInit(false.B)
  private val s0_fire, s1_fire, s2_fire, s3_fire     = WireInit(false.B)
  private val s0_flush, s1_flush, s2_flush, s3_flush = WireInit(false.B)
  private val s0_flushFromBpu, s1_flushFromBpu       = Wire(Vec(FetchPorts, Bool()))

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

  private val s3_wbNotFlush = WireInit(false.B)

  backendRedirect := fromFtq.redirect.valid
  s3_flush        := backendRedirect || (wbRedirect.valid && !s3_wbNotFlush)
  s2_flush        := backendRedirect || uncacheRedirect.valid || wbRedirect.valid
  s1_flush        := s2_flush || s1_flushFromBpu(0)
  s0_flush        := s1_flush || s0_flushFromBpu(0)

  fromFtq.req.ready := s1_ready && io.fromICache.fetchReady
  private val s0_fetchBlock = VecInit.tabulate(FetchPorts)(i =>
    Wire(new FetchBlockInfo).fromFtqRequest(s0_ftqFetch(i), s0_flush || s0_flushFromBpu(i))
  )
  private val s0_firstSize       = s0_fetchBlock(0).fetchSize
  private val s0_firstValid      = s0_ftqFetch(0).valid
  private val s0_secondValid     = s0_ftqFetch(1).valid
  private val s0_firstRange      = s0_fetchBlock(0).instrRange
  private val s0_secondRange     = s0_fetchBlock(1).instrRange
  private val s0_totalInstrRange = mergeInstrRange(s0_secondValid, s0_firstRange, s0_secondRange, s0_firstSize)
  private val s0_firstEndPos     = s0_ftqFetch(0).takenCfiOffset.bits
  private val s0_totalEndPos = Mux(
    s0_firstValid && s0_secondValid,
    s0_firstSize + s0_ftqFetch(1).takenCfiOffset.bits - 1.U,
    s0_firstEndPos
  )

  private val s0_baseBlockPcLower = VecInit((0 until FetchBlockInstNum).map(i =>
    Cat(0.U(1.W), s0_fetchBlock(0).startVAddr(PcCutPoint - 1, 0)) + (i * 2).U
  ))
  private val s0_appendedBlockPcLower = VecInit((0 until FetchBlockInstNum).map(i =>
    Cat(0.U(1.W), s0_fetchBlock(1).startVAddr(PcCutPoint - 1, 0)) + (i * 2).U + s0_firstSize
  ))
  private val s0_appendedInstrOffset = VecInit((0 until FetchBlockInstNum).map(i =>
    i.U + s0_firstSize
  ))
  private val s0_fetchBlockSelect = VecInit.tabulate(FetchBlockInstNum)(i => Mux(s0_firstSize > i.U, false.B, true.B))
  private val s0_twoFetchPcLower = VecInit.tabulate(FetchBlockInstNum)(i =>
    Mux(s0_fetchBlockSelect(i), s0_appendedBlockPcLower(i), s0_baseBlockPcLower(i))
  )
  private val s0_twoFetchInstrOffset = VecInit.tabulate(FetchBlockInstNum)(i =>
    Mux(s0_fetchBlockSelect(i), s0_appendedInstrOffset(i), i.U)
  )
  /* *****************************************************************************
   * IFU Stage 1
   * ***************************************************************************** */
  private val s1_valid      = ValidHold(s0_fire && !s0_flush, s1_fire, s1_flush)
  private val s1_firstValid = ValidHold(s0_fire && !s0_flush && s0_ftqFetch(0).valid, s1_fire, s1_flush)
  private val s1_secondValid =
    ValidHold(s0_fire && !s0_flush && s0_ftqFetch(1).valid && !s0_flushFromBpu(1), s1_fire, s1_flush)
  private val s1_fetchBlock          = RegEnable(s0_fetchBlock, s0_fire)
  private val s1_firstSize           = s1_fetchBlock(0).fetchSize
  private val s1_prevLastIsHalfRvi   = RegInit(false.B)
  private val s1_totalInstrRange     = RegEnable(s0_totalInstrRange, s0_fire)
  private val s1_firstEndPos         = RegEnable(s0_firstEndPos, s0_fire)
  private val s1_totalEndPos         = RegEnable(s0_totalEndPos, s0_fire)
  private val s1_fetchBlockSelect    = RegEnable(s0_fetchBlockSelect, s0_fire)
  private val s1_twoFetchPcLower     = RegEnable(s0_twoFetchPcLower, s0_fire)
  private val s1_twoFetchInstrOffset = RegEnable(s0_twoFetchInstrOffset, s0_fire)

  s1_flushFromBpu(0) := fromFtq.flushFromBpu.shouldFlushByStage3(s1_fetchBlock(0).ftqIdx, s1_firstValid)
  s1_flushFromBpu(1) := fromFtq.flushFromBpu.shouldFlushByStage3(s1_fetchBlock(1).ftqIdx, s1_secondValid)

  private val s1_iCacheRespValid = fromICache.valid

  s1_fire  := s1_valid && s2_ready && s1_iCacheRespValid
  s1_ready := s1_fire || !s1_valid

  io.toICache.stall := !s2_ready
  iCacheMatchAssert(fromICache, s1_fetchBlock)

  private val s1_maybeRvcMap = fromICache.bits.maybeRvcMap
  private val s1_icacheMeta  = VecInit.tabulate(FetchPorts)(i => Wire(new ICacheMeta).fromICacheResp(fromICache.bits))
  private val s1_maybeRvc    = Cat(s1_maybeRvcMap, s1_maybeRvcMap) >> s1_fetchBlock(0).startVAddr(5, 1)
  private val s1_rawData     = fromICache.bits.data
  private val s1_perfInfo    = io.fromICache.perf

  instrBoundary.io.req.valid                 := s1_valid
  instrBoundary.io.req.instrRange            := s1_totalInstrRange.asTypeOf(Vec(FetchBlockInstNum, Bool()))
  instrBoundary.io.req.firstFetchBlockEndPos := s1_firstEndPos
  instrBoundary.io.req.endPos                := s1_totalEndPos
  instrBoundary.io.req.firstInstrIsHalfRvi   := s1_prevLastIsHalfRvi
  instrBoundary.io.req.maybeRvc              := VecInit((0 until FetchBlockInstNum).map(i => s1_maybeRvc(i)))

  private val s1_firstFetchEndIsHalf = instrBoundary.io.resp.firstFetchBlockLastInstrIsHalfRvi
  private val s1_fetchEndIsHalf      = instrBoundary.io.resp.lastInstrIsHalfRvi

  // When invalidTaken is true, we can not flush s2_prevLastIsHalfRvi because the fetch block after it is fall-through.
  when(backendRedirect) {
    s1_prevLastIsHalfRvi := false.B
  }.elsewhen(wbRedirect.valid) {
    s1_prevLastIsHalfRvi := wbRedirect.isHalfInstr
  }.elsewhen(uncacheRedirect.valid) {
    s1_prevLastIsHalfRvi := false.B
  }.elsewhen(s1_fire && !s1_icacheMeta(0).isUncache) {
    s1_prevLastIsHalfRvi := s1_fetchEndIsHalf
  }

// rawInstrValid(i) and instrCountBeforeCurrent(i) also handle instructions
// spanning across prediction blocks. This design aligns with the logic
// used for s3_prevLastHalfData calculation.
  private val dealInstrValid = Wire(Vec(FetchBlockInstNum, Bool()))
  dealInstrValid    := instrBoundary.io.resp.instrValid
  dealInstrValid(0) := instrBoundary.io.resp.instrValid(0) | s1_prevLastIsHalfRvi

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
  instrCompactor.io.req.fetchBlockSelect        := s1_fetchBlockSelect
  instrCompactor.io.req.twoFetchPcLower         := s1_twoFetchPcLower
  instrCompactor.io.req.twoFetchInstrOffset     := s1_twoFetchInstrOffset
  instrCompactor.io.req.rawInstrValid           := dealInstrValid
  instrCompactor.io.req.rawIsRvc                := rawIsRvc
  instrCompactor.io.req.instrCountBeforeCurrent := instrCountBeforeCurrent
  private val instrCompactInfo = Wire(new InstrCompactBundle(FetchBlockInstNum))
  instrCompactInfo                   := instrCompactor.io.resp
  instrCompactInfo.instrEndOffset(0) := Mux(s1_prevLastIsHalfRvi, 0.U, Mux(rawIsRvc(0), 0.U, 1.U))

  private val s1_firstRange    = s1_fetchBlock(0).instrRange
  private val s1_secondRange   = s1_fetchBlock(1).instrRange
  private val s1_fetchTakenIdx = Wire(Vec(FetchPorts, new Valid(UInt(FetchBlockInstOffsetWidth.W))))
  s1_fetchTakenIdx(0).bits  := PopCount(dealInstrValid.asUInt & s1_firstRange) - 1.U
  s1_fetchTakenIdx(1).bits  := PopCount(dealInstrValid.asUInt & s1_totalInstrRange) - 1.U
  s1_fetchTakenIdx(0).valid := s1_fetchBlock(0).takenCfiOffset.valid && s1_firstValid
  s1_fetchTakenIdx(1).valid := s1_fetchBlock(1).takenCfiOffset.valid && s1_secondValid

  private val s1_realFetchBlock = Wire(Vec(FetchPorts, new FetchBlockInfo))
  s1_realFetchBlock                 := s1_fetchBlock
  s1_realFetchBlock(0).predTakenIdx := s1_fetchTakenIdx(0)
  s1_realFetchBlock(1).predTakenIdx := s1_fetchTakenIdx(1)
  s1_realFetchBlock(0).invalidTaken := !rawInstrEndVec(s1_fetchBlock(0).takenCfiOffset.bits) &&
    s1_fetchBlock(0).takenCfiOffset.valid
  s1_realFetchBlock(1).invalidTaken := !rawInstrEndVec(s1_fetchBlock(1).takenCfiOffset.bits) &&
    s1_fetchBlock(1).takenCfiOffset.valid
  s1_realFetchBlock(0).takenCfiOffset.valid := s1_fetchBlock(0).takenCfiOffset.valid && s1_firstValid
  s1_realFetchBlock(1).takenCfiOffset.valid := s1_fetchBlock(1).takenCfiOffset.valid && s1_secondValid
  s1_realFetchBlock(0).rawInstrEndVec       := rawInstrEndVec.asUInt & s1_firstRange
  s1_realFetchBlock(1).rawInstrEndVec       := (rawInstrEndVec.asUInt >> s1_firstSize).asUInt & s1_secondRange
  private val s1_rawFirstData         = s1_rawData
  private val s1_rawSecondData        = 0.U((ICacheLineBytes * 8).W)
  private val s1_rawFirstDataDupWire  = VecInit(Seq.fill(FetchPorts)(s1_rawFirstData))
  private val s1_rawSecondDataDupWire = VecInit(Seq.fill(FetchPorts)(s1_rawSecondData))
  // Special case for MMIO:
  // If two fetches occur and the first is non-MMIO while the second is MMIO,
  // delay the second fetch by one cycle to split into a one-fetch.

  /* *****************************************************************************
   * IFU Stage 2
   * ***************************************************************************** */
  private val s2_valid      = ValidHold(s1_fire && !s1_flush, s2_fire, s2_flush)
  private val s2_firstValid = ValidHold(s1_fire && !s1_flush && s1_firstValid, s2_fire, s2_flush)
  private val s2_secondValid =
    ValidHold(s1_fire && !s1_flush && s1_secondValid && !s1_flushFromBpu(1), s2_fire, s2_flush)
  private val s2_fetchBlock     = RegEnable(s1_realFetchBlock, s1_fire)
  private val s2_prevIBufEnqPtr = RegInit(0.U.asTypeOf(new IBufPtr))

  private val s2_prevShiftSelect = UIntToMask(s2_prevIBufEnqPtr.value(1, 0), IfuAlignWidth)

  s2_fire  := s2_valid && s3_ready
  s2_ready := s2_fire || !s2_valid

  private val s2_instrCompactInfo = RegEnable(instrCompactInfo, s1_fire)
  private val s2_instrCount       = RegEnable(PopCount(rawInstrEndVec), s1_fire)
  private val s2_instrValid       = RegEnable(UIntToMask(PopCount(rawInstrEndVec), FetchBlockInstNum), s1_fire)

  private val s2_rawIndex           = RegEnable(instrCountBeforeCurrent, s1_fire)
  private val s2_rawInstrEndVec     = RegEnable(rawInstrEndVec, s1_fire)
  private val s2_prevLastIsHalfRvi  = RegEnable(s1_prevLastIsHalfRvi, s1_fire)
  private val s2_prevLastHalfData   = RegInit(0.U(16.W))
  private val s2_prevLastHalfPc     = RegInit(0.U.asTypeOf(PrunedAddr(VAddrBits)))
  private val s2_currentLastHalfRvi = RegEnable(s1_fetchEndIsHalf, s1_fire)

  private val s2_rawFirstDataDup  = RegEnable(s1_rawFirstDataDupWire, s1_fire)
  private val s2_rawSecondDataDup = RegEnable(s1_rawSecondDataDupWire, s1_fire)

  private val s2_icacheMeta = RegEnable(s1_icacheMeta, s1_fire)
  // Placeholder logic for now; subject to change later
  private val s2_firstLowICacheData   = cutICacheData(s2_rawFirstDataDup(0))
  private val s2_firstHighICacheData  = cutICacheData(s2_rawFirstDataDup(1))
  private val s2_secondLowICacheData  = cutICacheData(s2_rawSecondDataDup(0))
  private val s2_secondHighICacheData = cutICacheData(s2_rawSecondDataDup(1))

  private val s2_isPredTaken = VecInit.tabulate(FetchBlockInstNum)(i =>
    ((s2_fetchBlock(0).predTakenIdx.bits === i.U && s2_fetchBlock(0).predTakenIdx.valid) &&
      !s2_instrCompactInfo.selectBlock(i) && s2_firstValid) ||
      ((s2_fetchBlock(1).predTakenIdx.bits === i.U && s2_fetchBlock(1).predTakenIdx.valid) &&
        s2_instrCompactInfo.selectBlock(i) && s2_secondValid)
  )

  private val s2_invalidTaken = VecInit.tabulate(FetchBlockInstNum)(i =>
    ((s2_fetchBlock(0).predTakenIdx.bits === i.U && s2_fetchBlock(0).invalidTaken) &&
      !s2_instrCompactInfo.selectBlock(i) && s2_firstValid) ||
      ((s2_fetchBlock(1).predTakenIdx.bits === i.U && s2_fetchBlock(1).invalidTaken) &&
        s2_instrCompactInfo.selectBlock(i) && s2_secondValid)
  )

  private val s2_alignShiftNum = s2_prevIBufEnqPtr.value(1, 0)
  // Maybe it's better to move the calculation of enqBlockStartPos to the previous pipeline stage
  // — at least from a timing perspective. But it would require modifying IBufferPrevPtr.
  private val s2_alignBlockStartPos = WireDefault(VecInit.fill(IBufferEnqueueWidth)(false.B))
  s2_alignBlockStartPos(s2_alignShiftNum) := true.B
  private val s2_alignCompactInfo = alignInstrCompact(s2_instrCompactInfo, s2_alignShiftNum)
  private val s2_alignInstrData   = WireDefault(VecInit.fill(IBufferEnqueueWidth)(0.U(32.W)))
  private val s2_alignInstrValid =
    alignData(s2_instrValid.asTypeOf(Vec(FetchBlockInstNum, Bool())), s2_alignShiftNum, false.B)
  private val s2_alignInvalidTaken = alignData(s2_invalidTaken, s2_alignShiftNum, false.B)
  private val s2_alignIsPredTaken  = alignData(s2_isPredTaken, s2_alignShiftNum, false.B)
  private val s2_alignPc = VecInit.tabulate(IBufferEnqueueWidth)(i =>
    catPC(
      s2_alignCompactInfo.instrPcLower(i),
      Mux(s2_alignCompactInfo.selectBlock(i), s2_fetchBlock(1).pcHigh, s2_fetchBlock(0).pcHigh),
      Mux(s2_alignCompactInfo.selectBlock(i), s2_fetchBlock(1).pcHighPlus1, s2_fetchBlock(0).pcHighPlus1)
    )
  )
  private val s2_realAlignInstrData = WireDefault(VecInit.fill(IBufferEnqueueWidth)(0.U(32.W)))
  private val s2_realAlignPc = WireDefault(VecInit.fill(IBufferEnqueueWidth)(0.U.asTypeOf(PrunedAddr(VAddrBits))))
  private val s2_alignFoldPc = VecInit(s2_realAlignPc.map(i => XORFold(i(VAddrBits - 1, 1), MemPredPCWidth)))

  for (i <- 0 until IBufferEnqueueWidth / 2) {
    val lowIdx     = s2_alignCompactInfo.instrIndex(i).value
    val highIdx    = s2_alignCompactInfo.instrIndex(i + IBufferEnqueueWidth / 2).value
    val j          = i + IBufferEnqueueWidth / 2
    val lowSelect  = s2_alignCompactInfo.selectBlock(i)
    val highSelect = s2_alignCompactInfo.selectBlock(j)
    s2_alignInstrData(i) := Mux(!lowSelect, s2_firstLowICacheData(lowIdx), s2_secondLowICacheData(lowIdx))
    s2_alignInstrData(j) := Mux(!highSelect, s2_firstHighICacheData(highIdx), s2_secondHighICacheData(highIdx))
  }

  for (i <- 0 until IBufferEnqueueWidth) {
    // Handling of cross-predict-block instructions in the one-fetch case.
    // This part should only be modified after the backend changes are completed.
    val adjustedBlockHeadInst =
      Mux(s2_prevLastIsHalfRvi, Cat(s2_alignInstrData(i)(15, 0), s2_prevLastHalfData), s2_alignInstrData(i))
    val adjustedBlockHeadPc =
      Mux(s2_prevLastIsHalfRvi, s2_prevLastHalfPc, s2_alignPc(i))
    s2_realAlignInstrData(i) := Mux(s2_alignBlockStartPos(i), adjustedBlockHeadInst, s2_alignInstrData(i))
    s2_realAlignPc(i)        := Mux(s2_alignBlockStartPos(i), adjustedBlockHeadPc, s2_alignPc(i))
  }

  // backendRedirect has the highest priority
  when(backendRedirect) {
    s2_prevLastHalfData := 0.U
    s2_prevLastHalfPc   := 0.U.asTypeOf(PrunedAddr(VAddrBits))
  }.elsewhen(wbRedirect.valid) {
    s2_prevLastHalfData := wbRedirect.halfData
    s2_prevLastHalfPc   := wbRedirect.halfPc
  }.elsewhen(uncacheRedirect.valid) {
    s2_prevLastHalfData := 0.U
    s2_prevLastHalfPc   := 0.U.asTypeOf(PrunedAddr(VAddrBits))
  }.elsewhen(s2_fire) {
    s2_prevLastHalfData := s2_alignInstrData(s2_instrCount + s2_alignShiftNum)(15, 0)
    s2_prevLastHalfPc   := s2_alignPc(s2_instrCount + s2_alignShiftNum)
  }

  when(backendRedirect) {
    s2_prevIBufEnqPtr := 0.U.asTypeOf(new IBufPtr)
  }.elsewhen(wbRedirect.valid) {
    s2_prevIBufEnqPtr := wbRedirect.prevIBufEnqPtr + wbRedirect.instrCount
  }.elsewhen(uncacheRedirect.valid) {
    s2_prevIBufEnqPtr := uncacheRedirect.prevIBufEnqPtr + uncacheRedirect.instrCount
  }.elsewhen(s2_fire) {
    s2_prevIBufEnqPtr := s2_prevIBufEnqPtr + s2_instrCount
  }

  // PreDecode: delimitation, does not expand RVC
  preDecoderIn.valid           := s2_valid
  preDecoderIn.bits.data       := s2_realAlignInstrData
  preDecoderIn.bits.isRvc      := s2_alignCompactInfo.instrIsRvc
  preDecoderIn.bits.instrValid := s2_alignInstrValid // s3_instrValid.asTypeOf(Vec(FetchBlockInstNum, Bool()))

  private val s2_alignPd         = preDecoderOut.pd
  private val s2_alignJumpOffset = preDecoderOut.jumpOffset

  private val s2_reqIsUncache = s2_valid && s2_icacheMeta(0).isUncache &&
    s2_icacheMeta(0).exception.isNone
  private val s2_alignFetchBlock = Wire(Vec(FetchPorts, new FetchBlockInfo))
  s2_alignFetchBlock                      := s2_fetchBlock
  s2_alignFetchBlock(0).predTakenIdx.bits := s2_fetchBlock(0).predTakenIdx.bits + s2_prevIBufEnqPtr.value(1, 0)
  s2_alignFetchBlock(1).predTakenIdx.bits := s2_fetchBlock(1).predTakenIdx.bits + s2_prevIBufEnqPtr.value(1, 0)

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
  private val s3_valid               = WireInit(false.B)
  private val s3_firstValid          = ValidHold(s2_fire && !s2_flush && s2_firstValid, s3_fire, s3_flush)
  private val s3_secondValid         = ValidHold(s2_fire && !s2_flush && s2_secondValid, s3_fire, s3_flush)
  private val s3_alignFetchBlock     = RegEnable(s2_alignFetchBlock, s2_fire)
  private val s3_prevIBufEnqPtr      = RegEnable(s2_prevIBufEnqPtr, s2_fire)
  private val s3_rawIndex            = RegEnable(s2_rawIndex, s2_fire)
  private val s3_prevShiftSelect     = RegEnable(s2_prevShiftSelect, s2_fire)
  private val s3_prevLastRvi         = RegEnable(s2_prevLastIsHalfRvi, s2_fire)
  private val s3_currentLastHalfData = RegEnable(s2_alignInstrData(s2_instrCount + s2_alignShiftNum)(15, 0), s2_fire)
  private val s3_currentLastHalfRvi  = RegEnable(s2_currentLastHalfRvi, s2_fire)
  private val s3_currentLastHalfPC   = RegEnable(s2_realAlignPc(s2_instrCount + s2_alignShiftNum), s2_fire)
  private val s3_instrCount          = RegEnable(s2_instrCount, s2_fire)
  s3_fire := io.toIBuffer.fire

  private val s3_alignInvalidTaken = RegEnable(s2_alignInvalidTaken, s2_fire)
  private val s3_alignIsPredTaken  = RegEnable(s2_alignIsPredTaken, s2_fire)
  private val s3_alignInstrData    = RegEnable(s2_realAlignInstrData, s2_fire)
  private val s3_alignInstrValid   = RegEnable(s2_alignInstrValid, s2_fire)
  private val s3_icacheMeta        = RegEnable(s2_icacheMeta, s2_fire)
  private val s3_alignCompactInfo  = RegEnable(s2_alignCompactInfo, s2_fire)
  private val isFirstInstr         = RegInit(true.B)
  when(isFirstInstr && io.toIBuffer.fire) {
    isFirstInstr := false.B
  }

  rvcExpanders.zipWithIndex.foreach { case (expander, i) =>
    expander.io.in      := s3_alignInstrData(i)
    expander.io.fsIsOff := io.csrFsIsOff
  }
  // Use expanded instruction only when input is legal.
  // Otherwise, use origin illegal RVC instruction.
  private val s3_alignExpdInstr = VecInit(rvcExpanders.map { expander: RvcExpander =>
    Mux(expander.io.ill, expander.io.in, expander.io.out.bits)
  })
  private val s3_alignRvcIll     = VecInit(rvcExpanders.map(_.io.ill))
  private val s3_alignPds        = RegEnable(s2_alignPd, s2_fire)
  private val s3_alignJumpOffset = RegEnable(s2_alignJumpOffset, s2_fire)

  private val s3_alignPc = VecInit.tabulate(IBufferEnqueueWidth)(i =>
    if (i < IfuAlignWidth) {
      RegEnable(s2_realAlignPc(i), s2_fire)
    } else {
      catPC(
        s3_alignCompactInfo.instrPcLower(i),
        Mux(s3_alignCompactInfo.selectBlock(i), s3_alignFetchBlock(1).pcHigh, s3_alignFetchBlock(0).pcHigh),
        Mux(s3_alignCompactInfo.selectBlock(i), s3_alignFetchBlock(1).pcHighPlus1, s3_alignFetchBlock(0).pcHighPlus1)
      )
    }
  )

  // Exapnd 1 bit to prevent overflow when assert
  private val s3_fetchStartAddr = VecInit.tabulate(FetchPorts)(i =>
    PrunedAddrInit(Cat(0.U(1.W), s3_alignFetchBlock(i).startVAddr.toUInt))
  )
  private val s3_fetchNextStartAddr = VecInit.tabulate(FetchPorts)(i =>
    PrunedAddrInit(Cat(0.U(1.W), s3_alignFetchBlock(i).target.toUInt))
  )
  for (i <- 0 until FetchPorts) {
    when(s3_valid && !s3_alignFetchBlock(i).takenCfiOffset.valid) {
      assert(
        s3_fetchStartAddr(i) + (2 * FetchBlockInstNum).U >= s3_fetchNextStartAddr(i),
        s"More than ${2 * FetchBlockInstNum} Bytes fetch is not allowed!"
      )
    }
  }

  private val s3_alignFoldPc        = RegEnable(s2_alignFoldPc, s2_fire)
  private val s3_rawInstrEndVec     = RegEnable(s2_rawInstrEndVec, s2_fire)
  private val s3_prevLastIsHalfRvi  = RegEnable(s2_prevLastIsHalfRvi, s2_fire)
  private val s3_uncacheLowerPc     = RegEnable(s2_alignCompactInfo.instrPcLower(s2_alignShiftNum), s2_fire)
  private val s3_alignBlockStartPos = RegEnable(s2_alignBlockStartPos, s2_fire)
  private val s3_uncachePc = catPC(s3_uncacheLowerPc, s3_alignFetchBlock(0).pcHigh, s3_alignFetchBlock(0).pcHighPlus1)
  private val s3_reqIsUncache         = RegEnable(s2_reqIsUncache, s2_fire)
  private val s3_uncacheCanGo         = uncacheUnit.io.resp.valid && !uncacheUnit.io.resp.bits.crossPage
  private val s3_uncacheCrossPageMask = s3_valid && uncacheUnit.io.resp.valid && uncacheUnit.io.resp.bits.crossPage
  private val s3_toIBufferValid = s3_valid && (!s3_reqIsUncache || (s3_uncacheCanGo && s3_reqIsUncache)) && !s3_flush
  private val s3_shiftNum       = s3_prevIBufEnqPtr.value(1, 0)
  private val s3_ignore         = s3_prevShiftSelect // possibly redundant, may remove later.

  /* ** unache state handle ** */
  private val uncacheBusy = RegInit(false.B)
  // Uncache cross-page across two fetch blocks, store the prev block’s cross-page flag and data.
  private val prevUncacheCrossPage = RegInit(false.B)
  private val prevUncacheData      = RegInit(0.U(16.W))
  // For uncache cross-page instr, the real PC is in the prev fetch block.
  private val uncachePc = RegInit(0.U.asTypeOf(PrunedAddr(VAddrBits)))
  // Uncache cross-page may hit seq fetch or mispred, check required.
  private val uncacheCrossPageCheck = RegInit(false.B)
  when(s3_flush) {
    uncacheBusy           := false.B
    uncachePc             := 0.U.asTypeOf(PrunedAddr(VAddrBits))
    uncacheCrossPageCheck := false.B
  }.elsewhen(uncacheUnit.io.req.fire) {
    uncacheBusy           := true.B
    uncachePc             := Mux(prevUncacheCrossPage, uncachePc, s3_alignPc(s3_shiftNum))
    uncacheCrossPageCheck := (s3_alignFetchBlock(0).startVAddr + 2.U) === s3_alignFetchBlock(0).target
  }.elsewhen(uncacheUnit.io.resp.valid) {
    uncacheBusy := false.B
    // uncachePc := uncachePc
    uncacheCrossPageCheck := false.B
  }

  uncacheUnit.io.req.valid       := s3_valid && s3_reqIsUncache && !uncacheBusy
  uncacheUnit.io.req.bits.ftqIdx := s3_alignFetchBlock(0).ftqIdx
  uncacheUnit.io.req.bits.pbmt   := s3_icacheMeta(0).itlbPbmt
  uncacheUnit.io.req.bits.isMmio := s3_icacheMeta(0).pmpMmio
  uncacheUnit.io.req.bits.paddr  := s3_icacheMeta(0).pAddr
  uncacheUnit.io.flush           := s3_flush
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

  private val s3_uncacheData = Mux(prevUncacheCrossPage, Cat(uncacheData(15, 0), prevUncacheData), uncacheData)
  private val uncacheIsRvc   = s3_uncacheData(1, 0) =/= "b11".U
  uncacheRvcExpander.io.in      := Mux(s3_reqIsUncache, s3_uncacheData, 0.U)
  uncacheRvcExpander.io.fsIsOff := io.csrFsIsOff

  s3_valid := ValidHold(
    // infire: s2 -> s3 fire
    s2_fire && !s2_flush,
    // outfire:
    io.toIBuffer.fire || s3_uncacheCrossPageMask,
    // On flush, waiting for uncache response is handled by the channel itself.
    s3_flush
  )

  s3_ready := (io.toIBuffer.ready && (s3_uncacheCanGo || !s3_reqIsUncache)) || !s3_valid

  /* ** prediction result check ** */
  checkerIn.valid                   := s3_valid
  checkerIn.bits.instrJumpOffset    := s3_alignJumpOffset
  checkerIn.bits.instrValid         := s3_alignInstrValid.asTypeOf(Vec(IBufferEnqueueWidth, Bool()))
  checkerIn.bits.instrPds           := s3_alignPds
  checkerIn.bits.instrPc            := s3_alignPc
  checkerIn.bits.isPredTaken        := s3_alignIsPredTaken
  checkerIn.bits.ignore             := s3_ignore.asBools
  checkerIn.bits.shiftNum           := s3_prevIBufEnqPtr.value(1, 0)
  checkerIn.bits.firstPredTakenIdx  := s3_alignFetchBlock(0).predTakenIdx
  checkerIn.bits.secondPredTakenIdx := s3_alignFetchBlock(1).predTakenIdx
  checkerIn.bits.firstTarget        := s3_alignFetchBlock(0).target
  checkerIn.bits.secondTarget       := s3_alignFetchBlock(1).target
  checkerIn.bits.selectFetchBlock   := s3_alignCompactInfo.selectBlock
  checkerIn.bits.invalidTaken       := s3_alignInvalidTaken
  checkerIn.bits.instrEndOffset     := s3_alignCompactInfo.instrEndOffset

  /* ** frontend Trigger  ** */
  frontendTrigger.io.pds             := s3_alignPds
  frontendTrigger.io.pc              := s3_alignPc
  frontendTrigger.io.data            := 0.U.asTypeOf(Vec(IBufferEnqueueWidth + 1, UInt(16.W))) // s3_alignInstrData
  frontendTrigger.io.frontendTrigger := io.frontendTrigger
  private val s3_alignTriggered = frontendTrigger.io.triggered

  /* ** send to IBuffer ** */
  io.toIBuffer.valid               := s3_toIBufferValid
  io.toIBuffer.bits.instrs         := s3_alignExpdInstr
  io.toIBuffer.bits.valid          := s3_alignInstrValid.asUInt
  io.toIBuffer.bits.enqEnable      := checkerOutStage1.fixedTwoFetchRange.asUInt & s3_alignInstrValid.asUInt
  io.toIBuffer.bits.isRvc          := s3_alignPds.map(_.isRVC)
  io.toIBuffer.bits.pc             := s3_alignPc
  io.toIBuffer.bits.prevIBufEnqPtr := s3_prevIBufEnqPtr
  io.toIBuffer.bits.ftqPtr.zipWithIndex.foreach { case (ftqPtr, i) =>
    ftqPtr := Mux(s3_alignCompactInfo.selectBlock(i), s3_alignFetchBlock(1).ftqIdx, s3_alignFetchBlock(0).ftqIdx)
  }

  /* in s3, prevInstrCount equals to next cycle's IBuffer.numFromFetch without predChecker. "prev" means s2;
   * when s2 fire (s2_valid && s3_ready), use s2_instrCount;
   * else when s3 stall (s3_valid && !s3_ready). use s3_instrCount because prevInstrCount equals to current instrCount;
   * otherwise, we don't care about prevInstrCount because next cycle's toIBuffer.valid won't set.
   */
  io.toIBuffer.bits.prevInstrCount := Mux(
    s2_fire,
    Mux(s2_reqIsUncache, 1.U, s2_instrCount), // FIXME: consider the second fetch block
    Mux(s3_reqIsUncache, 1.U, s3_instrCount)
  )

  // Find the last entry based on the boundaries of compacted valid signals.
  private val select = s3_alignCompactInfo.selectBlock
  private val enq    = io.toIBuffer.bits.enqEnable

  private val s3_rvcException       = ExceptionType.fromRvcExpander((enq & s3_alignRvcIll.asUInt).orR, s3_valid)
  private val s3_rvcExceptionOffset = PriorityEncoder(enq & s3_alignRvcIll.asUInt)

  io.toIBuffer.bits.isLastInFtqEntry := (0 until IBufferEnqueueWidth).map { i =>
    if (i == IBufferEnqueueWidth - 1) enq(i)
    else enq(i) ^ ((select(i) === select(i + 1)) & enq(i + 1))
  }
  io.toIBuffer.bits.instrEndOffset.zipWithIndex.foreach { case (a, i) =>
    a.predTaken  := s3_alignIsPredTaken(i) && !s3_reqIsUncache
    a.fixedTaken := checkerOutStage1.fixedTwoFetchTaken(i) && !s3_reqIsUncache
    a.offset     := s3_alignCompactInfo.instrEndOffset(i)
  }
  io.toIBuffer.bits.foldpc := s3_alignFoldPc
  // mark the exception only on first instruction
  io.toIBuffer.bits.exceptionType := s3_icacheMeta(0).exception || s3_rvcException
  // backendException only needs to be set for the first instruction.
  // Other instructions in the same block may have pf or af set,
  // which is a side effect of the first instruction and actually not necessary.
  io.toIBuffer.bits.isBackendException := s3_icacheMeta(0).isBackendException
  // if we have last half RV-I instruction, and has exception, we need to tell backend to caculate the correct pc
  io.toIBuffer.bits.exceptionCrossPage := s3_icacheMeta(0).exception.hasException && s3_prevLastIsHalfRvi
  // if icache respond with exception, it's marked on entire cacheline,
  // so the first enqueued instr should be marked with exception
  // otherwise, we only have rvcException, so select its offset
  io.toIBuffer.bits.exceptionOffset := Mux(
    s3_icacheMeta(0).exception.hasException,
    0.U,
    s3_rvcExceptionOffset
  )

  io.toIBuffer.bits.triggered := s3_alignTriggered

  val enqVec = io.toIBuffer.bits.enqEnable
  val allocateSeqNum = VecInit((0 until IBufferEnqueueWidth).map { i =>
    val idx  = PopCount(enqVec.take(i + 1))
    val pc   = s3_alignPc(i).toUInt
    val code = io.toIBuffer.bits.instrs(i)
    val seq  = PerfCCT.createInstMetaAtFetch(idx, pc, code, s3_fire & enqVec(i), clock, reset)
    val res  = WireDefault(0.U.asTypeOf(new InstSeqNum))
    res.seqNum := seq
    // leave uopIdx to 0.U
    res
  })
  io.toIBuffer.bits.debug_seqNum.zipWithIndex.foreach { case (seqNum, i) =>
    seqNum := Mux(s3_fire, allocateSeqNum(i), 0.U.asTypeOf(new InstSeqNum))
  }

  /** to backend */
  // s4_gpAddr is valid iff gpf is detected.
  // Uncache doesn’t request iTLB; it only returns bus exceptions.
  io.toBackend.gpAddrMem.wen                     := s3_toIBufferValid && s3_icacheMeta(0).exception.isGpf
  io.toBackend.gpAddrMem.waddr                   := s3_alignFetchBlock(0).ftqIdx.value
  io.toBackend.gpAddrMem.wdata.gpaddr            := s3_icacheMeta(0).gpAddr.toUInt
  io.toBackend.gpAddrMem.wdata.isForVSnonLeafPTE := s3_icacheMeta(0).isForVSnonLeafPTE

  // Write back to Ftq
  private val s3_uncacheRange     = VecInit((0 until FetchBlockInstNum).map(i => if (i == 0) true.B else false.B))
  private val uncacheFlushWb      = Wire(Valid(new FrontendRedirect))
  private val uncachePd           = 0.U.asTypeOf(Vec(FetchBlockInstNum, new PreDecodeInfo))
  private val uncacheMisEndOffset = Wire(Valid(UInt(FetchBlockInstOffsetWidth.W)))
  uncacheMisEndOffset.valid := s3_reqIsUncache
  uncacheMisEndOffset.bits  := Mux(prevUncacheCrossPage || uncacheIsRvc || uncacheCheckFault, 0.U, 1.U)

  // Send mmioFlushWb back to FTQ 1 cycle after uncache fetch return
  // When backend redirect, mmioState reset after 1 cycle.
  // In this case, mask .valid to avoid overriding backend redirect
  private val uncacheTarget =
    Mux(
      uncacheIsRvc || prevUncacheCrossPage || uncacheCheckFault,
      s3_alignFetchBlock(0).startVAddr + 2.U,
      s3_alignFetchBlock(0).startVAddr + 4.U
    )
  uncacheFlushWb.valid          := s3_reqIsUncache && !backendRedirect && (s3_uncacheCanGo || uncacheCheckFault)
  uncacheFlushWb.bits.ftqIdx    := s3_alignFetchBlock(0).ftqIdx
  uncacheFlushWb.bits.pc        := s3_alignFetchBlock(0).startVAddr.toUInt
  uncacheFlushWb.bits.taken     := false.B
  uncacheFlushWb.bits.ftqOffset := uncacheMisEndOffset.bits
  uncacheFlushWb.bits.isRVC     := uncacheIsRvc
  uncacheFlushWb.bits.attribute := BranchAttribute.None
  uncacheFlushWb.bits.target    := uncacheTarget.toUInt

  when(s3_reqIsUncache) {
    val inst        = s3_uncacheData
    val brAttribute = BranchAttribute.decode(inst)

    val uncacheRvcException = ExceptionType.fromRvcExpander(uncacheRvcExpander.io.ill)
    io.toIBuffer.bits.instrs(s3_shiftNum) := Mux(
      uncacheRvcExpander.io.ill,
      uncacheRvcExpander.io.in,
      uncacheRvcExpander.io.out.bits
    )

    io.toIBuffer.bits.pc(s3_shiftNum)                    := uncachePc
    io.toIBuffer.bits.isRvc(s3_shiftNum)                 := uncacheIsRvc
    io.toIBuffer.bits.instrEndOffset(s3_shiftNum).offset := Mux(prevUncacheCrossPage || uncacheIsRvc, 0.U, 1.U)

    io.toIBuffer.bits.exceptionType := uncacheException || uncacheRvcException
    // execption can happen in next page only when cross page.
    io.toIBuffer.bits.exceptionCrossPage := prevUncacheCrossPage && uncacheException.hasException
    io.toIBuffer.bits.exceptionOffset    := 0.U

    // The s3_alignBlockStartPos vector marks the position of the first instruction.
    // In uncache scenarios, only a single instruction is allowed for execution,
    // so the valid signal enqueued into the IBuffer must align with s3_alignBlockStartPos.
    io.toIBuffer.bits.valid     := s3_alignBlockStartPos.asUInt
    io.toIBuffer.bits.enqEnable := s3_alignBlockStartPos.asUInt

    uncacheFlushWb.bits.isRVC     := uncacheIsRvc
    uncacheFlushWb.bits.attribute := brAttribute
  }

  uncacheRedirect.valid          := s3_reqIsUncache && (s3_uncacheCanGo || uncacheCheckFault)
  uncacheRedirect.instrCount     := Mux(uncacheCheckFault, 0.U, 1.U)
  uncacheRedirect.prevIBufEnqPtr := s3_prevIBufEnqPtr
  uncacheRedirect.isHalfInstr    := false.B
  uncacheRedirect.halfPc         := 0.U.asTypeOf(PrunedAddr(VAddrBits))
  uncacheRedirect.halfData       := 0.U
  /* *****************************************************************************
   * IFU Write-back Stage
   * - write back preDecode information to Ftq to update
   * - redirect if found fault prediction
   * - redirect if false hit last half(last PC is not start + 32 Bytes, but in the middle of an notCFI RVI instruction)
   * ***************************************************************************** */
  private val wbEnable              = RegNext(s2_fire && !s2_flush) && !s3_reqIsUncache && !s3_flush
  private val wbValid               = RegNext(wbEnable, init = false.B)
  private val wbFirstValid          = RegEnable(s3_firstValid, wbEnable)
  private val wbSecondValid         = RegEnable(s3_secondValid, wbEnable)
  private val wbAlignFetchBlock     = RegEnable(s3_alignFetchBlock, wbEnable)
  private val wbPrevIBufEnqPtr      = RegEnable(s3_prevIBufEnqPtr, wbEnable)
  private val wbInstrCount          = RegEnable(PopCount(io.toIBuffer.bits.enqEnable), wbEnable)
  private val wbAlignCompactInfo    = RegEnable(s3_alignCompactInfo, wbEnable)
  private val wbAlignInstrPcLower   = wbAlignCompactInfo.instrPcLower
  private val wbAlignInstrEndOffset = wbAlignCompactInfo.instrEndOffset

  private val wbCurrentLastHalfData = RegEnable(s3_currentLastHalfData, wbEnable)
  private val wbCurrentLastHalfPc   = RegEnable(s3_currentLastHalfPC, wbEnable)
  private val wbCurrentLastRvi      = RegEnable(s3_currentLastHalfRvi, wbEnable)

  s3_wbNotFlush := wbAlignFetchBlock(0).ftqIdx === s3_alignFetchBlock(0).ftqIdx && s3_valid && wbValid

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

  val perfEvents: Seq[(String, Bool)] = Seq(
    ("frontendFlush                ", wbRedirect.valid),
    ("ifu_req                      ", io.toIBuffer.fire),
    ("ifu_miss                     ", io.toIBuffer.fire && !s3_icachePerfInfo.hit),
    ("ifu_req_cacheline_0          ", io.toIBuffer.fire),
    ("ifu_req_cacheline_1          ", io.toIBuffer.fire && s3_icachePerfInfo.isDoubleLine),
    ("ifu_req_cacheline_0_hit      ", io.toIBuffer.fire && s3_icachePerfInfo.hit0),
    ("ifu_req_cacheline_1_hit      ", io.toIBuffer.fire && s3_icachePerfInfo.hit1),
    ("only_0_hit                   ", io.toIBuffer.fire && s3_icachePerfInfo.hit0NoReq1),
    ("only_0_miss                  ", io.toIBuffer.fire && s3_icachePerfInfo.miss0NoReq1),
    ("hit_0_hit_1                  ", io.toIBuffer.fire && s3_icachePerfInfo.hit0Hit1),
    ("hit_0_miss_1                 ", io.toIBuffer.fire && s3_icachePerfInfo.hit0Miss1),
    ("miss_0_hit_1                 ", io.toIBuffer.fire && s3_icachePerfInfo.miss0Hit1),
    ("miss_0_miss_1                ", io.toIBuffer.fire && s3_icachePerfInfo.miss0Miss1)
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

  perfAnalyzer.io.perfInfo.icachePerfInfo                 := s3_icachePerfInfo
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
  perfAnalyzer.io.perfInfo.checkPerfInfo.uncacheBubble    := s3_reqIsUncache && !s3_uncacheCanGo

  perfAnalyzer.io.perfInfo.toIBufferInfo.ibufferFire   := io.toIBuffer.fire
  perfAnalyzer.io.perfInfo.toIBufferInfo.enqEnable     := io.toIBuffer.bits.enqEnable & io.toIBuffer.bits.valid
  perfAnalyzer.io.perfInfo.toIBufferInfo.startVAddr(0) := s3_alignFetchBlock(0).startVAddr.toUInt
  perfAnalyzer.io.perfInfo.toIBufferInfo.startVAddr(1) := s3_alignFetchBlock(1).startVAddr.toUInt
  io.toIBuffer.bits.topdownInfo                        := perfAnalyzer.io.topdownOut.topdown
}
