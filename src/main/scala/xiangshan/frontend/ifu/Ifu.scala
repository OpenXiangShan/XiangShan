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
import utility.XSPerfAccumulate
import utils.DuplicateInit
import xiangshan.FrontendTdataDistributeIO
import xiangshan.cache.mmu.HasTlbConst
import xiangshan.cache.mmu.TlbRequestIO
import xiangshan.frontend.BackendRedirectTopdown
import xiangshan.frontend.ExceptionType
import xiangshan.frontend.FetchToIBuffer
import xiangshan.frontend.FrontendRedirect
import xiangshan.frontend.FtqToIfuIO
import xiangshan.frontend.ICacheToIfuIO
import xiangshan.frontend.IfuToBackendIO
import xiangshan.frontend.IfuToFtqIO
import xiangshan.frontend.IfuToInstrUncacheIO
import xiangshan.frontend.InstrUncacheToIfuIO
import xiangshan.frontend.PreDecodeInfo
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.PrunedAddrInit
import xiangshan.frontend.bpu.BranchAttribute
import xiangshan.frontend.ibuffer.IBufPtr

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

    // ICache: response
    val fromICache: ICacheToIfuIO = Flipped(new ICacheToIfuIO)

    // Uncache: mmio request / response
    val toUncache:   IfuToInstrUncacheIO = new IfuToInstrUncacheIO
    val fromUncache: InstrUncacheToIfuIO = Flipped(new InstrUncacheToIfuIO)

    // IBuffer: enqueue
    val toIBuffer:    DecoupledIO[FetchToIBuffer] = DecoupledIO(new FetchToIBuffer)
    val ibufferEmpty: Bool                        = Input(Bool())

    // Backend: gpaMem
    val toBackend:    IfuToBackendIO = new IfuToBackendIO
    val backendEmpty: Bool           = Input(Bool())

    // debug extension: frontend trigger
    val frontendTrigger: FrontendTdataDistributeIO = Flipped(new FrontendTdataDistributeIO)

    // Backend: csr control
    val csrFsIsOff: Bool = Input(Bool())

    // Topdown analysis
    val backendRedirectTopdown: BackendRedirectTopdown = Input(new BackendRedirectTopdown)
  }
  val io: IfuIO = IO(new IfuIO)

  // submodule
  private val instrBoundary      = Module(new InstrBoundary)
  private val predChecker        = Module(new PredChecker)
  private val frontendTrigger    = Module(new FrontendTrigger)
  private val rvcExpanders       = Seq.fill(IBufferEnqueueWidth)(Module(new RvcExpander))
  private val perfAnalyzer       = Module(new IfuPerfAnalysis)
  private val uncacheUnit        = Module(new IfuUncacheUnit)
  private val uncacheRvcExpander = Module(new RvcExpander)

  // alias
  private val (toFtq, fromFtq) = (io.toFtq, io.fromFtq)
  private val (checkerIn, checkerOutStage1, checkerOutStage2) =
    (predChecker.io.req, predChecker.io.resp.stage1Out, predChecker.io.resp.stage2Out)

  private val s0_ready, s1_ready, s2_ready = WireInit(false.B)
  private val s0_fire, s1_fire, s2_fire    = WireInit(false.B)
  private val s0_flush, s1_flush, s2_flush = WireInit(false.B)
  private val s0_flushFromBpu              = Wire(Bool())

  private val backendRedirect             = WireInit(false.B)
  private val wbRedirect, uncacheRedirect = WireInit(0.U.asTypeOf(new IfuRedirectInternal))

  private val s2_wbNotFlush = WireInit(false.B)

  backendRedirect := fromFtq.redirect.valid
  s2_flush        := backendRedirect || (wbRedirect.valid && !s2_wbNotFlush)
  s1_flush        := backendRedirect || uncacheRedirect.valid || wbRedirect.valid
  s0_flush        := s1_flush || s0_flushFromBpu

  /* --------------------------------------------------------------------------------------------------------------
     stage 0
     - get req from ICacheMainPipe
     - get instruction boundary
     - compact instructions
     -------------------------------------------------------------------------------------------------------------- */

  private val s0_valid = io.fromICache.req.valid

  s0_fire := s0_valid && s1_ready && !s0_flush

  io.fromICache.req.ready := s1_ready || s0_flush

  private val s0_fetchBlock = VecInit(io.fromICache.req.bits.info.map(req => Wire(new FetchBlock).fromICacheReq(req)))

  dontTouch(s0_fetchBlock)

  private val s0_ifuData    = Wire(new IfuData).fromICacheReq(io.fromICache.req.bits)
  private val s0_icacheMeta = VecInit(io.fromICache.req.bits.info.map(_.icacheMeta))

  s0_flushFromBpu := fromFtq.flushFromBpu.shouldFlushByStage3(s0_fetchBlock(0).ftqIdx, s0_valid)

  private val s0_prevEndIsHalfRvi = RegInit(false.B)

  private val s0_totalEndPos = Mux(
    s0_fetchBlock(1).valid,
    (s0_fetchBlock(1).takenCfiOffset.bits + s0_fetchBlock(0).size)(FetchBlockInstOffsetWidth - 1, 0),
    s0_fetchBlock(0).takenCfiOffset.bits
  )

  instrBoundary.io.req.valid               := s0_valid
  instrBoundary.io.req.fetchBlock          := s0_fetchBlock
  instrBoundary.io.req.ifuData             := s0_ifuData
  instrBoundary.io.req.firstInstrIsHalfRvi := s0_prevEndIsHalfRvi
  instrBoundary.io.req.totalEndPos         := s0_totalEndPos

  private val s0_firstEndIsHalfRvi = instrBoundary.io.resp.firstEndIsHalfRvi
  private val s0_totalEndIsHalfRvi = instrBoundary.io.resp.totalEndIsHalfRvi
  private val s0_rawInstrVec       = instrBoundary.io.resp.rawInstrVec
  private val s0_instrEndMask      = instrBoundary.io.resp.instrEndMask
  private val s0_rawInstrValid     = VecInit(s0_rawInstrVec.map(_.valid)).asUInt
  private val s0_firstRange        = s0_ifuData.firstRange
  private val s0_totalRange        = s0_ifuData.totalRange

  // When invalidTaken is true, we can not flush s2_prevLastIsHalfRvi because the fetch block after it is fall-through.
  when(backendRedirect) {
    s0_prevEndIsHalfRvi := false.B
  }.elsewhen(wbRedirect.valid) {
    s0_prevEndIsHalfRvi := wbRedirect.isHalfInstr
  }.elsewhen(uncacheRedirect.valid) {
    s0_prevEndIsHalfRvi := uncacheRedirect.isHalfInstr
  }.elsewhen(s0_fire && !s0_icacheMeta(0).isUncache) {
    s0_prevEndIsHalfRvi := s0_totalEndIsHalfRvi
  }

  // When an exception occurs, forward the exception information immediately instead of
  // waiting for instruction concatenation to complete.
  private val s0_hasException = s0_icacheMeta(0).exception.hasException
  private val s0_instrCount =
    Mux(s0_hasException, 1.U((log2Ceil(FetchBlockInstNum) + 1).W), PopCount(s0_instrEndMask.asUInt & s0_totalRange))
  private val s0_rawFirstData     = io.fromICache.req.bits.info(0).data
  private val s0_rawSecondData    = io.fromICache.req.bits.info(1).data
  private val s0_firstEndIndex    = Wire(UInt(log2Ceil(ICacheLineBytes / 2).W))
  private val s0_secondEndIndex   = Wire(UInt(log2Ceil(ICacheLineBytes / 2).W))
  private val s0_secondStartIndex = Wire(UInt(log2Ceil(ICacheLineBytes / 2).W))
  s0_firstEndIndex := io.fromICache.req.bits.info(0).startVAddr(
    log2Ceil(ICacheLineBytes / 2),
    instOffsetBits
  ) + io.fromICache.req.bits.info(0).takenCfiOffset.bits
  s0_secondEndIndex := io.fromICache.req.bits.info(1).startVAddr(
    log2Ceil(ICacheLineBytes / 2),
    instOffsetBits
  ) + io.fromICache.req.bits.info(1).takenCfiOffset.bits
  s0_secondStartIndex := io.fromICache.req.bits.info(1).startVAddr(log2Ceil(ICacheLineBytes / 2), instOffsetBits)
  /* --------------------------------------------------------------------------------------------------------------
     stage 1
     - cat half rvi instruction
     - generate instruction PC
     -------------------------------------------------------------------------------------------------------------- */

  private val s1_valid = ValidHold(s0_fire && !s0_flush, s1_fire, s1_flush)

  s1_fire  := s1_valid && s2_ready
  s1_ready := s1_fire || !s1_valid

  private val s1_fetchBlock        = RegEnable(s0_fetchBlock, s0_fire)
  private val s1_totalEndPos       = RegEnable(s0_totalEndPos, s0_fire)
  private val s1_rawInstrValid     = RegEnable(s0_rawInstrValid, s0_fire)
  private val s1_instrEndMask      = RegEnable(s0_instrEndMask, s0_fire)
  private val s1_firstRange        = RegEnable(s0_firstRange, s0_fire)
  private val s1_totalRange        = RegEnable(s0_totalRange, s0_fire)
  private val s1_compactedInstrVec = compact(s0_rawInstrVec, s0_fire)
  // The pre-calculated enqueue amount required when estimating whether the IBuffer can be enqueued.
  private val s1_specInstrCount    = RegEnable(s0_instrCount, s0_fire)
  private val s1_firstEndIsHalfRvi = RegEnable(s0_firstEndIsHalfRvi, s0_fire)
  private val s1_totalEndIsHalfRvi = RegEnable(s0_totalEndIsHalfRvi, s0_fire)
  // Used to compute the sequence number of valid instructions.
  private val s1_firstRawInstrValid = s1_rawInstrValid & s1_firstRange
  private val s1_totalRawInstrValid = s1_rawInstrValid & s1_totalRange
  // Used to compute the count of valid instructions within the fetch range.
  private val s1_firstRawInstrEndMask = s1_instrEndMask.asUInt & s1_firstRange
  private val s1_invalidTaken = VecInit(
    s1_fetchBlock(0).takenCfiOffset.valid && s1_firstEndIsHalfRvi,
    s1_fetchBlock(1).valid && s1_fetchBlock(1).takenCfiOffset.valid &&
      s1_totalEndIsHalfRvi
  )
  private val s1_predTakenIdx = VecInit(
    PopCount(s1_firstRawInstrValid) - 1.U,
    PopCount(s1_totalRawInstrValid) - 1.U
  )

  private val s1_prevIBufEnqPtrDup  = RegInit(DuplicateInit(Seq("instr", "valid"), 0.U.asTypeOf(new IBufPtr)))
  private val s1_prevEndIsHalfRvi   = RegEnable(s0_prevEndIsHalfRvi, s0_fire)
  private val s1_prevEndHalfRviData = RegInit(0.U(16.W))
  private val s1_prevEndHalfRviPc   = RegInit(0.U.asTypeOf(PrunedAddr(VAddrBits)))

  private val s1_firstICacheData  = RegEnable(s0_rawFirstData, s0_fire)
  private val s1_secondICacheData = RegEnable(s0_rawSecondData, s0_fire)
  private val s1_firstEndIndex    = RegEnable(s0_firstEndIndex, s0_fire)
  private val s1_secondEndIndex   = RegEnable(s0_secondEndIndex, s0_fire)
  private val s1_secondStartIndex = RegEnable(s0_secondStartIndex, s0_fire)
  private val s1_rawFirstData     = cutICacheData(s1_firstICacheData)
  private val s1_rawSecondData    = cutICacheData(s1_secondICacheData)

  private val s1_icacheMetaIn = RegEnable(s0_icacheMeta, s0_fire)
  private val s1_instrVec     = s1_compactedInstrVec

  // ICache mainPipe send parity check result 1 cycle after io.fromICache.req.fire, here merge into icacheMeta.
  // for better timing.
  private val s1_icacheMeta = WireDefault(s1_icacheMetaIn)
  s1_icacheMeta.zipWithIndex.foreach { case (meta, i) =>
    meta.exception := s1_icacheMetaIn(i).exception || ExceptionType.fromEcc(
      io.fromICache.corrupt(i).reduce(_ || _), // FIXME: consider which cacheline is corrupted, and modify exceptionMask
      s1_valid
    )
  }

  private val s1_firstInstrCount = PopCount(s1_firstRawInstrEndMask)
  private val s1_instrCount = Mux(
    s1_icacheMeta(0).exception.hasException,
    1.U((log2Ceil(FetchBlockInstNum) + 1).W),
    Mux(s1_invalidTaken(0), s1_firstInstrCount, s1_specInstrCount)
  )
  private val s1_instrValid =
    Mux(s1_instrCount === FetchBlockInstNum.U, ~0.U(FetchBlockInstNum.W), UIntToMask(s1_instrCount, FetchBlockInstNum))

  private val s1_predTakenMask = VecInit((0 until FetchPorts).map { i =>
    Mux(
      s1_fetchBlock(i).valid && s1_fetchBlock(i).takenCfiOffset.valid,
      UIntToOH(s1_predTakenIdx(i), FetchBlockInstNum),
      0.U(FetchBlockInstNum.W)
    )
  })
  private val s1_mergedPredTakenMask = s1_predTakenMask(0) | s1_predTakenMask(1)
  dontTouch(s1_mergedPredTakenMask)

  private val s1_invalidTakenMask = VecInit((0 until FetchPorts).map { i =>
    Mux(
      s1_fetchBlock(i).valid && s1_invalidTaken(i),
      s1_predTakenMask(i),
      0.U(FetchBlockInstNum.W)
    )
  })
  private val s1_mergedInvalidTakenMask = s1_invalidTakenMask(0) | s1_invalidTakenMask(1)
  dontTouch(s1_mergedInvalidTakenMask)

  private val s1_alignShiftInstrNum    = s1_prevIBufEnqPtrDup("instr").value(1, 0)
  private val s1_alignShiftValidNum    = s1_prevIBufEnqPtrDup("valid").value(1, 0)
  private val s1_baseAlignedInstrVec   = align(s1_instrVec, s1_alignShiftInstrNum)
  private val s1_baseAlignedInstrPcVec = VecInit(s1_baseAlignedInstrVec.map(instr => getInstrPc(instr, s1_fetchBlock)))
  private val s1_alignedInstrValid     = (s1_instrValid << s1_alignShiftValidNum).pad(IBufferEnqueueWidth)
  private val s1_alignedPredTakenMask  = (s1_mergedPredTakenMask << s1_alignShiftValidNum).pad(IBufferEnqueueWidth)
  private val s1_alignedInvalidTakenMask = (s1_mergedInvalidTakenMask << s1_alignShiftValidNum).pad(IBufferEnqueueWidth)

  private val s1_firstEndPos     = s1_fetchBlock(0).takenCfiOffset.bits
  private val s1_firstEndHalfRvi = Wire(new EndHalfRviInfo)
  s1_firstEndHalfRvi.isHalfRvi := s1_firstEndIsHalfRvi
  s1_firstEndHalfRvi.pc        := s1_fetchBlock(0).startVAddr + (s1_firstEndPos << 1).asUInt
  s1_firstEndHalfRvi.data      := s1_rawFirstData(s1_firstEndIndex)(15, 0)

  private val s1_secondEndHalfRviData = s1_rawSecondData(s1_secondEndIndex)(15, 0)
  private val s1_totalEndHalfRvi      = Wire(new EndHalfRviInfo)
  s1_totalEndHalfRvi.isHalfRvi := s1_totalEndIsHalfRvi
  s1_totalEndHalfRvi.pc := Mux(
    s1_fetchBlock(1).valid,
    s1_fetchBlock(1).startVAddr + ((s1_fetchBlock(1).takenCfiOffset.bits) << 1),
    s1_firstEndHalfRvi.pc
  )
  s1_totalEndHalfRvi.data := Mux(s1_fetchBlock(1).valid, s1_secondEndHalfRviData, s1_firstEndHalfRvi.data)
  private val s1_secondStartRviData = s1_rawSecondData(s1_secondStartIndex)(15, 0)

  private val s1_baseInstrData = genBaseInstrData(
    s1_baseAlignedInstrVec,
    s1_rawFirstData,
    s1_rawSecondData,
    s1_secondStartRviData
  )
  private val s1_alignedInstrPcVec = WireDefault(s1_baseAlignedInstrPcVec)
  private val s1_alignedInstrVec   = WireDefault(s1_baseAlignedInstrVec)

  for (i <- 0 until IBufferEnqueueWidth) {
    s1_alignedInstrVec(i).valid        := s1_alignedInstrValid(i)
    s1_alignedInstrVec(i).isPredTaken  := s1_alignedPredTakenMask(i)
    s1_alignedInstrVec(i).invalidTaken := s1_alignedInvalidTakenMask(i)
    s1_alignedInstrVec(i).data         := s1_baseInstrData(i)
    s1_alignedInstrVec(i).endOffset := Mux(
      !s1_baseAlignedInstrVec(i).blockSel && s1_baseAlignedInstrVec(i).isCrossBlockInstr,
      0.U,
      s1_baseAlignedInstrVec(i).endOffset
    )
  }

  for (i <- 0 until IfuAlignWidth) {
    when((s1_alignShiftInstrNum === i.U) && s1_prevEndIsHalfRvi) {
      s1_alignedInstrPcVec(i)                := s1_prevEndHalfRviPc
      s1_alignedInstrVec(i).data             := Cat(s1_baseInstrData(i)(15, 0), s1_prevEndHalfRviData)
      s1_alignedInstrVec(i).isPrevEndHalfRvi := s1_prevEndIsHalfRvi
      s1_alignedInstrVec(i).endOffset        := 0.U
    }
  }
  // To facilitate compilation optimization and reduce the generation of redundant data.
  s1_alignedInstrVec(IBufferEnqueueWidth - 1) := 0.U.asTypeOf(new Instruction)

  private val s1_alignedFoldPc =
    VecInit(s1_alignedInstrPcVec.map(i => XORFold(i(VAddrBits - 1, 1), MemPredPCWidth)))

  // backendRedirect has the highest priority
  when(backendRedirect) {
    s1_prevEndHalfRviData := 0.U
    s1_prevEndHalfRviPc   := 0.U.asTypeOf(PrunedAddr(VAddrBits))
  }.elsewhen(wbRedirect.valid) {
    s1_prevEndHalfRviData := wbRedirect.halfData
    s1_prevEndHalfRviPc   := wbRedirect.halfPc
  }.elsewhen(uncacheRedirect.valid) {
    s1_prevEndHalfRviData := uncacheRedirect.halfData
    s1_prevEndHalfRviPc   := uncacheRedirect.halfPc
  }.elsewhen(s1_fire) {
    s1_prevEndHalfRviData := s1_totalEndHalfRvi.data
    s1_prevEndHalfRviPc   := s1_totalEndHalfRvi.pc
  }

  when(backendRedirect) {
    s1_prevIBufEnqPtrDup := 0.U.asTypeOf(new IBufPtr)
  }.elsewhen(wbRedirect.valid) {
    s1_prevIBufEnqPtrDup := wbRedirect.prevIBufEnqPtr + wbRedirect.instrCount
  }.elsewhen(uncacheRedirect.valid) {
    s1_prevIBufEnqPtrDup := uncacheRedirect.prevIBufEnqPtr + uncacheRedirect.instrCount
  }.elsewhen(s1_fire && !s1_icacheMeta(0).isUncache) {
    s1_prevIBufEnqPtrDup := s1_prevIBufEnqPtrDup.head + s1_specInstrCount
  }

  // reqIsUncache is used to limit the number of fetch requests and enable special pre-decode configurations.
  private val s1_reqIsUncache = s1_valid && s1_icacheMeta(0).isUncache
  // useUncacheFetch controls whether the instruction fetch operation follows the uncache control logic.
  private val s1_useUncacheFetch = s1_valid && s1_icacheMeta(0).isUncache && s1_icacheMeta(0).exception.isNone

  private val s1_alignedPdInfoVec     = Wire(Vec(IBufferEnqueueWidth, new PreDecodeInfo))
  private val s1_alignedJumpOffsetVec = Wire(Vec(IBufferEnqueueWidth, PrunedAddr(VAddrBits)))
  for (i <- 0 until IBufferEnqueueWidth) {
    val alignedInstr = s1_alignedInstrVec(i)
    val alignedValid = s1_alignedInstrValid(i)
    val jalOffset    = getJalOffset(alignedInstr.data, alignedInstr.isRvc)
    val brOffset     = getBrOffset(alignedInstr.data, alignedInstr.isRvc)
    val attribute    = BranchAttribute.decode(alignedInstr.data, alignedValid && s1_valid)
    s1_alignedPdInfoVec(i).valid       := alignedValid
    s1_alignedPdInfoVec(i).isRVC       := alignedInstr.isRvc
    s1_alignedPdInfoVec(i).brAttribute := Mux(alignedValid, attribute, BranchAttribute.None)
    s1_alignedJumpOffsetVec(i)         := Mux(s1_alignedPdInfoVec(i).isBr, brOffset, jalOffset)
  }
  /* --------------------------------------------------------------------------------------------------------------
     stage 2
     - expand instructions
     - pre decode
     - check
     - process uncache req
     -------------------------------------------------------------------------------------------------------------- */

  // assign later
  private val s2_valid       = WireInit(false.B)
  private val s2_firstValid  = ValidHold(s1_fire && !s1_flush && s1_fetchBlock(0).valid, s2_fire, s2_flush)
  private val s2_secondValid = ValidHold(s1_fire && !s1_flush && s1_fetchBlock(1).valid, s2_fire, s2_flush)
  private val s2_fetchBlock  = RegEnable(s1_fetchBlock, s1_fire)
  dontTouch(s2_fetchBlock)

  private val s2_prevIBufEnqPtr = RegEnable(s1_prevIBufEnqPtrDup.head, s1_fire)

  private val s2_prevEndIsHalfRvi   = RegEnable(s1_prevEndIsHalfRvi, false.B, s1_fire)
  private val s2_prevEndHalfPc      = RegEnable(s1_prevEndHalfRviPc, s1_fire)
  private val s2_prevEndHalfRviData = RegEnable(s1_prevEndHalfRviData, s1_fire)

  private val s2_firstEndHalfRvi = RegEnable(s1_firstEndHalfRvi, s1_fire)
  private val s2_totalEndHalfRvi = RegEnable(s1_totalEndHalfRvi, s1_fire)

  private val s2_instrCount           = RegEnable(s1_instrCount, s1_fire)
  private val s2_alignedInstrValid    = RegEnable(s1_alignedInstrValid, s1_fire)
  private val s2_icacheMeta           = RegEnable(s1_icacheMeta, s1_fire)
  private val s2_alignedInstrVec      = RegEnable(s1_alignedInstrVec, s1_fire)
  private val s2_alignedInstrPcVec    = RegEnable(s1_alignedInstrPcVec, s1_fire)
  private val s2_alignedFoldPc        = RegEnable(s1_alignedFoldPc, s1_fire)
  private val s2_alignedPdInfoVec     = RegEnable(s1_alignedPdInfoVec, s1_fire)
  private val s2_alignedJumpOffsetVec = RegEnable(s1_alignedJumpOffsetVec, s1_fire)

  s2_fire := io.toIBuffer.fire
  dontTouch(s2_fire)

  private val s2_alignShiftNum = s2_prevIBufEnqPtr.value(1, 0)
  dontTouch(s2_alignShiftNum)

  rvcExpanders.zipWithIndex.foreach { case (expander, i) =>
    expander.io.in      := s2_alignedInstrVec(i).data
    expander.io.fsIsOff := io.csrFsIsOff
  }

  private val s2_expandedInstrDataVec = VecInit(rvcExpanders.map { expander =>
    Mux(expander.io.ill, expander.io.in, expander.io.out.bits)
  })
  dontTouch(s2_expandedInstrDataVec)

  private val s2_expandedInstrVec = WireDefault(s2_alignedInstrVec)
  s2_expandedInstrVec.zip(s2_expandedInstrDataVec).foreach { case (instr, expandedData) =>
    instr.data := expandedData
  }

  private val s2_blockSel          = VecInit(s2_expandedInstrVec.map(_.blockSel))
  private val s2_endOffsetVec      = VecInit(s2_expandedInstrVec.map(_.endOffset))
  private val s2_isCrossBlockInstr = VecInit(s2_expandedInstrVec.map(_.isCrossBlockInstr))
  dontTouch(s2_blockSel)

  private val s2_reqIsUncache    = RegEnable(s1_reqIsUncache, false.B, s1_fire)
  private val s2_useUncacheFetch = RegEnable(s1_useUncacheFetch, s1_fire)
  private val s2_uncacheCanGo =
    (uncacheUnit.io.resp.valid && !uncacheUnit.io.resp.bits.needResend) || !s2_useUncacheFetch
  private val s2_uncacheCrossPageMask = s2_valid && uncacheUnit.io.resp.valid && uncacheUnit.io.resp.bits.needResend
  private val s2_toIBufferValid =
    s2_valid && (!s2_reqIsUncache || (s2_uncacheCanGo && s2_reqIsUncache)) && !s2_flush

  /* ** unache state handle ** */
  private val uncacheBusy = RegInit(false.B)
  // For uncache cross-page instr, the real PC is in the prev fetch block.
  private val uncachePc = RegInit(0.U.asTypeOf(PrunedAddr(VAddrBits)))
  // Uncache cross-page may hit seq fetch or mispred, check required.
  private val uncacheResendCheck = RegInit(false.B)
  when(s2_flush) {
    uncacheBusy := false.B
    uncachePc   := 0.U.asTypeOf(PrunedAddr(VAddrBits))
  }.elsewhen(uncacheUnit.io.req.fire) {
    uncacheBusy := true.B
    uncachePc   := Mux(s2_prevEndIsHalfRvi, s2_prevEndHalfPc, s2_alignedInstrPcVec(s2_alignShiftNum))
  }.elsewhen(uncacheUnit.io.resp.valid) {
    uncacheBusy := false.B
    // uncachePc := uncachePc
  }

  private val isFirstInstr = RegInit(true.B)
  when(isFirstInstr && io.toIBuffer.fire) {
    isFirstInstr := false.B
  }

  uncacheUnit.io.req.valid       := s2_valid && s2_useUncacheFetch && !uncacheBusy
  uncacheUnit.io.req.bits.pbmt   := s2_icacheMeta(0).itlbPbmt
  uncacheUnit.io.req.bits.isMmio := s2_icacheMeta(0).pmpMmio
  uncacheUnit.io.req.bits.paddr  := s2_icacheMeta(0).pAddr
  uncacheUnit.io.flush           := s2_flush
  uncacheUnit.io.isFirstInstr    := isFirstInstr
  uncacheUnit.io.ifuStall        := !io.toIBuffer.ready
  uncacheUnit.io.emptyAfter      := io.backendEmpty && io.ibufferEmpty
  io.toUncache <> uncacheUnit.io.toUncache
  uncacheUnit.io.fromUncache <> io.fromUncache

  private val uncacheData      = uncacheUnit.io.resp.bits.uncacheData
  private val uncacheException = uncacheUnit.io.resp.bits.exception
  // not RVC, no exception, crossing page boundary
  private val uncacheNeedResend = uncacheUnit.io.resp.bits.needResend && uncacheUnit.io.resp.valid

  private val s2_uncacheData = Mux(s2_prevEndIsHalfRvi, Cat(uncacheData(15, 0), s2_prevEndHalfRviData), uncacheData)
  private val uncacheIsRvc   = s2_uncacheData(1, 0) =/= "b11".U
  uncacheRvcExpander.io.in      := Mux(s2_reqIsUncache, s2_uncacheData, 0.U)
  uncacheRvcExpander.io.fsIsOff := io.csrFsIsOff

  s2_valid := ValidHold(
    // infire: s1 -> s2 fire
    s1_fire && !s1_flush,
    // outfire: When an uncache cross-page occurs and it is not an exception,
    // this instruction fetch should end and prepare to receive the next fetch signal.
    io.toIBuffer.fire || s2_uncacheCrossPageMask,
    // On flush, waiting for uncache response is handled by the channel itself.
    s2_flush
  )

  s2_ready := (io.toIBuffer.ready && (s2_uncacheCanGo || !s2_reqIsUncache)) || !s2_valid

  /* ** prediction result check ** */
  checkerIn.valid              := s2_valid
  checkerIn.bits.jumpOffsetVec := s2_alignedJumpOffsetVec
  checkerIn.bits.pdInfoVec     := s2_alignedPdInfoVec
  checkerIn.bits.instrPcVec    := s2_alignedInstrPcVec
  checkerIn.bits.instrVec      := s2_alignedInstrVec

  private val s2_fixedInstrValid = checkerOutStage1.fixedInstrValid.asUInt
  dontTouch(s2_fixedInstrValid)

  /* ** frontend Trigger  ** */
  frontendTrigger.io.pds             := s2_alignedPdInfoVec
  frontendTrigger.io.pc              := s2_alignedInstrPcVec
  frontendTrigger.io.data            := 0.U.asTypeOf(Vec(IBufferEnqueueWidth + 1, UInt(16.W)))
  frontendTrigger.io.frontendTrigger := io.frontendTrigger
  private val s2_alignTriggered = frontendTrigger.io.triggered

  /* ** send to IBuffer ** */
  io.toIBuffer.valid               := s2_toIBufferValid
  io.toIBuffer.bits.instrs         := s2_expandedInstrDataVec
  io.toIBuffer.bits.valid          := s2_alignedInstrValid
  io.toIBuffer.bits.enqEnable      := s2_fixedInstrValid
  io.toIBuffer.bits.isRvc          := s2_expandedInstrVec.map(_.isRvc)
  io.toIBuffer.bits.pc             := s2_alignedInstrPcVec // for debug
  io.toIBuffer.bits.prevIBufEnqPtr := s2_prevIBufEnqPtr
  io.toIBuffer.bits.ftqPtr.zipWithIndex.foreach { case (ftqPtr, i) =>
    ftqPtr := Mux(s2_blockSel(i) || s2_isCrossBlockInstr(i), s2_fetchBlock(1).ftqIdx, s2_fetchBlock(0).ftqIdx)
  }

  /* in s2, prevInstrCount equals to next cycle's IBuffer.numFromFetch without predChecker. "prev" means s1;
   * when s1 fire (s1_valid && s2_ready), use s1_specInstrCount;
   * else when s2 stall (s2_valid && !s2_ready). use s2_instrCount because prevInstrCount equals to current instrCount;
   * otherwise, we don't care about prevInstrCount because next cycle's toIBuffer.valid won't set.
   */
  io.toIBuffer.bits.prevInstrCount := Mux(
    s1_fire,
    Mux(s1_reqIsUncache, 1.U, s1_specInstrCount),
    Mux(s2_reqIsUncache, 1.U, s2_instrCount)
  )

  // Find the last entry based on the boundaries of compacted valid signals.
  private val select = s2_blockSel.zip(s2_isCrossBlockInstr).map {
    case (blockSel, isCrossBlock) => blockSel || isCrossBlock
  }
  private val enq = io.toIBuffer.bits.enqEnable

  private val s2_rvcIll       = VecInit(rvcExpanders.map(_.io.ill))
  private val s2_rvcException = ExceptionType.fromRvcExpander((enq & s2_rvcIll.asUInt).orR, s2_valid)

  io.toIBuffer.bits.isLastInFtqEntry := (0 until IBufferEnqueueWidth).map { i =>
    if (i == IBufferEnqueueWidth - 1) enq(i)
    else enq(i) ^ ((select(i) === select(i + 1)) & enq(i + 1))
  }
  io.toIBuffer.bits.instrEndOffset.zipWithIndex.foreach { case (a, i) =>
    a.predTaken  := s2_expandedInstrVec(i).isPredTaken && !s2_reqIsUncache
    a.fixedTaken := checkerOutStage1.fixedTaken(i) && !s2_reqIsUncache
    a.offset     := s2_endOffsetVec(i)
  }
  io.toIBuffer.bits.foldpc := s2_alignedFoldPc
  // mark the exception only on first instruction
  io.toIBuffer.bits.exceptionType := s2_icacheMeta(0).exception || s2_rvcException
  // backendException only needs to be set for the first instruction.
  // Other instructions in the same block may have pf or af set,
  // which is a side effect of the first instruction and actually not necessary.
  io.toIBuffer.bits.isBackendException := s2_icacheMeta(0).isBackendException
  // if we have last half RV-I instruction, and has exception, we need to tell backend to calculate the correct pc
  io.toIBuffer.bits.exceptionCrossPage := s2_icacheMeta(0).exception.hasException && s2_prevEndIsHalfRvi
  // if icache respond with exception, it's marked on entire cacheline,
  // so the first enqueued instr should be marked with exception
  // otherwise, we only have rvcException, so select its offset
  io.toIBuffer.bits.exceptionMask := Mux(
    s2_icacheMeta(0).exception.hasException,
    VecInit.tabulate(IBufferEnqueueWidth)(i => if (i < IfuAlignWidth) i.U === s2_alignShiftNum else false.B),
    VecInit.tabulate(IBufferEnqueueWidth)(i => enq(i) & s2_rvcIll(i))
  )

  io.toIBuffer.bits.triggered := s2_alignTriggered

  val enqVec = io.toIBuffer.bits.enqEnable
  val allocateSeqNum = VecInit((0 until IBufferEnqueueWidth).map { i =>
    val idx  = PopCount(enqVec.take(i + 1))
    val pc   = s2_alignedInstrPcVec(i).toUInt
    val code = io.toIBuffer.bits.instrs(i)
    val seq  = PerfCCT.createInstMetaAtFetch(idx, pc, code, s2_fire & enqVec(i), clock, reset)
    val res  = WireDefault(0.U.asTypeOf(new InstSeqNum))
    res.seqNum := seq
    // leave uopIdx to 0.U
    res
  })
  io.toIBuffer.bits.debug_seqNum.zipWithIndex.foreach { case (seqNum, i) =>
    seqNum := Mux(s2_fire, allocateSeqNum(i), 0.U.asTypeOf(new InstSeqNum))
  }

  /** to backend */
  // s4_gpAddr is valid iff gpf is detected.
  // Uncache doesn’t request iTLB; it only returns bus exceptions.
  io.toBackend.gpAddrMem.wen                     := s2_toIBufferValid && s2_icacheMeta(0).exception.isGpf
  io.toBackend.gpAddrMem.waddr                   := s2_fetchBlock(0).ftqIdx.value
  io.toBackend.gpAddrMem.wdata.gpaddr            := s2_icacheMeta(0).gpAddr.toUInt
  io.toBackend.gpAddrMem.wdata.isForVSnonLeafPTE := s2_icacheMeta(0).isForVSnonLeafPTE

  // Write back to Ftq
  private val s2_uncacheRange     = VecInit((0 until FetchBlockInstNum).map(i => if (i == 0) true.B else false.B))
  private val uncacheFlushWb      = Wire(Valid(new FrontendRedirect))
  private val uncachePd           = 0.U.asTypeOf(Vec(FetchBlockInstNum, new PreDecodeInfo))
  private val uncacheMisEndOffset = Wire(Valid(UInt(FetchBlockInstOffsetWidth.W)))
  uncacheMisEndOffset.valid := s2_reqIsUncache
  uncacheMisEndOffset.bits  := Mux(uncacheIsRvc || s2_prevEndIsHalfRvi || uncacheNeedResend, 0.U, 1.U)

  // Send mmioFlushWb back to FTQ 1 cycle after uncache fetch return
  // When backend redirect, mmioState reset after 1 cycle.
  // In this case, mask .valid to avoid overriding backend redirect
  private val uncacheTarget =
    Mux(
      uncacheIsRvc || s2_prevEndIsHalfRvi || uncacheNeedResend,
      s2_fetchBlock(0).startVAddr + 2.U,
      s2_fetchBlock(0).startVAddr + 4.U
    )
  // Due to the presence of uncache requests, s2_valid && io.toIBuffer.ready is not equivalent to s2_fire.
  uncacheFlushWb.valid :=
    s2_valid && io.toIBuffer.ready && s2_reqIsUncache && !backendRedirect && (s2_uncacheCanGo || uncacheNeedResend)
  uncacheFlushWb.bits.canTrain  := false.B
  uncacheFlushWb.bits.ftqIdx    := s2_fetchBlock(0).ftqIdx
  uncacheFlushWb.bits.pc        := s2_fetchBlock(0).startVAddr.toUInt
  uncacheFlushWb.bits.taken     := false.B
  uncacheFlushWb.bits.ftqOffset := uncacheMisEndOffset.bits
  uncacheFlushWb.bits.isRVC     := uncacheIsRvc
  uncacheFlushWb.bits.attribute := BranchAttribute.None
  uncacheFlushWb.bits.target    := uncacheTarget.toUInt

  when(s2_reqIsUncache) {
    val inst        = s2_uncacheData
    val brAttribute = BranchAttribute.decode(inst)

    val uncacheRvcException = ExceptionType.fromRvcExpander(uncacheRvcExpander.io.ill)
    io.toIBuffer.bits.instrs(s2_alignShiftNum) := Mux(
      uncacheRvcExpander.io.ill,
      uncacheRvcExpander.io.in,
      uncacheRvcExpander.io.out.bits
    )

    io.toIBuffer.bits.pc(s2_alignShiftNum)                    := uncachePc
    io.toIBuffer.bits.isRvc(s2_alignShiftNum)                 := uncacheIsRvc
    io.toIBuffer.bits.instrEndOffset(s2_alignShiftNum).offset := Mux(uncacheIsRvc || s2_prevEndIsHalfRvi, 0.U, 1.U)

    io.toIBuffer.bits.exceptionType := s2_icacheMeta(0).exception || uncacheException || uncacheRvcException
    // execption can happen in next page only when cross page.
    io.toIBuffer.bits.exceptionCrossPage :=
      s2_prevEndIsHalfRvi && (s2_icacheMeta(0).exception.hasException || uncacheException.hasException)
    io.toIBuffer.bits.exceptionMask := VecInit.tabulate(IBufferEnqueueWidth) { i =>
      if (i < IfuAlignWidth) i.U === s2_alignShiftNum else false.B
    }

    // In uncache scenarios, only a single instruction is allowed for execution,
    // so the valid signal enqueued into the IBuffer must be aligned.
    io.toIBuffer.bits.valid     := Cat(0.U(FetchBlockInstNum.W), UIntToOH(s2_alignShiftNum))
    io.toIBuffer.bits.enqEnable := Cat(0.U(FetchBlockInstNum.W), UIntToOH(s2_alignShiftNum))

    uncacheFlushWb.bits.isRVC     := uncacheIsRvc
    uncacheFlushWb.bits.attribute := brAttribute
  }

  // Core change: Route cross-page uncache data to S1 for unified management.
  // S2 can now directly concatenate uncache instructions using s2_prevLastIsHalfRvi during fetch.
  // This fixes the edge case where instructions spanning both cache and uncache channels fell through
  // the cracks of the existing S1 (cache) and S2 (uncache) cross-page handling logic.
  uncacheRedirect.valid := s2_valid && io.toIBuffer.ready && s2_reqIsUncache && (s2_uncacheCanGo || uncacheNeedResend)
  uncacheRedirect.instrCount     := Mux(uncacheNeedResend, 0.U, 1.U)
  uncacheRedirect.prevIBufEnqPtr := s2_prevIBufEnqPtr
  uncacheRedirect.isHalfInstr    := uncacheNeedResend
  uncacheRedirect.halfPc         := uncachePc
  uncacheRedirect.halfData       := uncacheData(15, 0)

  /* *****************************************************************************
   * IFU Write-back Stage
   * - write back preDecode information to Ftq to update
   * - redirect if found fault prediction
   * - redirect if false hit last half(last PC is not start + 32 Bytes, but in the middle of an notCFI RVI instruction)
   * ***************************************************************************** */
  private val wbEnable          = RegNext(s1_fire && !s1_flush) && !s2_reqIsUncache && !s2_flush
  private val wbValid           = RegNext(wbEnable, init = false.B)
  private val wbFirstValid      = RegEnable(s2_firstValid, wbEnable)
  private val wbSecondValid     = RegEnable(s2_secondValid, wbEnable)
  private val wbAlignFetchBlock = RegEnable(s2_fetchBlock, wbEnable)
  private val wbPrevIBufEnqPtr  = RegEnable(s2_prevIBufEnqPtr, wbEnable)
  private val wbInstrCount      = RegEnable(PopCount(io.toIBuffer.bits.enqEnable), wbEnable)

  private val wbFirstEndHalfRvi = RegEnable(s2_firstEndHalfRvi, wbEnable)
  private val wbTotalEndHalfRvi = RegEnable(s2_totalEndHalfRvi, wbEnable)

  s2_wbNotFlush := wbAlignFetchBlock(0).ftqIdx === s2_fetchBlock(0).ftqIdx && s2_valid && wbValid

  private val checkerRedirect = checkerOutStage2.checkerRedirect
  private val checkFlushWb = {
    val b         = Wire(Valid(new FrontendRedirect))
    val ftqIdx    = VecInit(wbAlignFetchBlock.map(_.ftqIdx))
    val startAddr = VecInit(wbAlignFetchBlock.map(_.startVAddr.toUInt))
    val select    = checkerRedirect.bits.blockSel || checkerRedirect.bits.isCrossBlockInstr
    val attribute = checkerRedirect.bits.attribute
    val canTrain  = attribute.isDirect || attribute.isReturn
    b.valid          := wbValid && checkerRedirect.valid
    b.bits.canTrain  := canTrain
    b.bits.ftqIdx    := Mux(select, ftqIdx(1), ftqIdx(0))
    b.bits.pc        := Mux(select, startAddr(1), startAddr(0))
    b.bits.taken     := checkerRedirect.bits.taken
    b.bits.ftqOffset := checkerRedirect.bits.endOffset
    b.bits.isRVC     := checkerRedirect.bits.isRVC
    b.bits.attribute := checkerRedirect.bits.attribute
    b.bits.target    := checkerRedirect.bits.target.toUInt
    b
  }

  toFtq.wbRedirect := Mux(wbValid, checkFlushWb, uncacheFlushWb)

  private val wbSelectedEndHalfRvi = Mux(
    !checkerRedirect.bits.blockSel,
    wbFirstEndHalfRvi,
    wbTotalEndHalfRvi
  )

  wbRedirect.valid          := checkFlushWb.valid
  wbRedirect.isHalfInstr    := wbSelectedEndHalfRvi.isHalfRvi && checkerRedirect.bits.invalidTaken
  wbRedirect.instrCount     := wbInstrCount
  wbRedirect.prevIBufEnqPtr := wbPrevIBufEnqPtr
  wbRedirect.halfPc         := wbSelectedEndHalfRvi.pc
  wbRedirect.halfData       := wbSelectedEndHalfRvi.data

  private val s1_icachePerfInfo = RegEnable(io.fromICache.perf, s0_fire)
  private val s2_icachePerfInfo = RegEnable(s1_icachePerfInfo, s1_fire)

  val perfEvents: Seq[(String, Bool)] = Seq(
    ("frontendFlush                ", wbRedirect.valid),
    ("ifu_req                      ", io.toIBuffer.fire),
    ("ifu_miss                     ", io.toIBuffer.fire && !s2_icachePerfInfo.hit),
    ("ifu_req_cacheline_0          ", io.toIBuffer.fire),
    ("ifu_req_cacheline_1          ", io.toIBuffer.fire && s2_icachePerfInfo.isDoubleLine),
    ("ifu_req_cacheline_0_hit      ", io.toIBuffer.fire && s2_icachePerfInfo.hit0),
    ("ifu_req_cacheline_1_hit      ", io.toIBuffer.fire && s2_icachePerfInfo.hit1),
    ("only_0_hit                   ", io.toIBuffer.fire && s2_icachePerfInfo.hit0NoReq1),
    ("only_0_miss                  ", io.toIBuffer.fire && s2_icachePerfInfo.miss0NoReq1),
    ("hit_0_hit_1                  ", io.toIBuffer.fire && s2_icachePerfInfo.hit0Hit1),
    ("hit_0_miss_1                 ", io.toIBuffer.fire && s2_icachePerfInfo.hit0Miss1),
    ("miss_0_hit_1                 ", io.toIBuffer.fire && s2_icachePerfInfo.miss0Hit1),
    ("miss_0_miss_1                ", io.toIBuffer.fire && s2_icachePerfInfo.miss0Miss1)
  )
  generatePerfEvent()
  perfAnalyzer.io.ifuPerfCtrl.fromFtqBubble    := !io.fromICache.req.valid && io.fromICache.req.ready
  perfAnalyzer.io.ifuPerfCtrl.backendRedirect  := backendRedirect
  perfAnalyzer.io.ifuPerfCtrl.ifuWbRedirect    := wbRedirect.valid
  perfAnalyzer.io.ifuPerfCtrl.fromBpuFlush     := s0_flushFromBpu
  perfAnalyzer.io.ifuPerfCtrl.fromICacheBubble := false.B

  perfAnalyzer.io.topdownIn.icacheTopdown          := io.fromICache.topdown
  perfAnalyzer.io.topdownIn.ftqTopdown             := io.fromFtq.topdownInfo
  perfAnalyzer.io.topdownIn.backendRedirectTopdown := io.backendRedirectTopdown

  perfAnalyzer.io.perfInfo.icachePerfInfo                 := s2_icachePerfInfo
  perfAnalyzer.io.perfInfo.checkPerfInfo.valid(0)         := wbValid && wbFirstValid
  perfAnalyzer.io.perfInfo.checkPerfInfo.valid(1)         := wbValid && wbSecondValid
  perfAnalyzer.io.perfInfo.checkPerfInfo.perfFaultType(0) := checkerOutStage2.perfFaultType(0)
  perfAnalyzer.io.perfInfo.checkPerfInfo.perfFaultType(1) := checkerOutStage2.perfFaultType(1)
  perfAnalyzer.io.perfInfo.checkPerfInfo.startVAddr(0)    := wbAlignFetchBlock(0).startVAddr.toUInt
  perfAnalyzer.io.perfInfo.checkPerfInfo.startVAddr(1)    := wbAlignFetchBlock(1).startVAddr.toUInt
  perfAnalyzer.io.perfInfo.checkPerfInfo.taken(0)         := wbAlignFetchBlock(0).takenCfiOffset.valid
  perfAnalyzer.io.perfInfo.checkPerfInfo.taken(1)         := wbAlignFetchBlock(1).takenCfiOffset.valid
  perfAnalyzer.io.perfInfo.checkPerfInfo.misPred          := checkerRedirect.valid
  perfAnalyzer.io.perfInfo.checkPerfInfo.selectBlock      := checkerRedirect.bits.blockSel
  perfAnalyzer.io.perfInfo.checkPerfInfo.misEndOffset     := checkerRedirect.bits.endOffset
  perfAnalyzer.io.perfInfo.checkPerfInfo.uncacheBubble    := s2_reqIsUncache && !s2_uncacheCanGo

  perfAnalyzer.io.perfInfo.toIBufferInfo.ibufferFire   := io.toIBuffer.fire
  perfAnalyzer.io.perfInfo.toIBufferInfo.enqEnable     := io.toIBuffer.bits.enqEnable & io.toIBuffer.bits.valid
  perfAnalyzer.io.perfInfo.toIBufferInfo.startVAddr(0) := s2_fetchBlock(0).startVAddr.toUInt
  perfAnalyzer.io.perfInfo.toIBufferInfo.startVAddr(1) := s2_fetchBlock(1).startVAddr.toUInt
  io.toIBuffer.bits.topdownInfo                        := perfAnalyzer.io.topdownOut.topdown

  XSPerfAccumulate("2fetch_to_ibuffer", io.toIBuffer.fire && s2_secondValid)
}
