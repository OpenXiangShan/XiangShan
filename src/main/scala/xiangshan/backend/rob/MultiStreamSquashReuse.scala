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

package xiangshan.backend.rob

import chisel3._
import chisel3.util._
import utility._
import utils._
import xiangshan._
import xiangshan.backend.Bundles._
import xiangshan.backend.fu.FuType
import xiangshan.frontend.ftq.FtqPtr

trait HasMultiStreamSquashReuse { this: RobImp =>
  protected def buildMultiStreamSquashReuse(): Unit = {
    // The WPB selects a static fetch-block position before rename. The Squash Log
    // is then read only with the transported stream generation and instruction offset.
    val MsrStreamCount = MsrConfig.StreamCount
    val MsrEntriesPerStream = MsrConfig.EntriesPerStream
    val MsrTotalEntries = MsrStreamCount * MsrEntriesPerStream
    val MsrWpbEntriesPerStream = MsrConfig.WpbEntriesPerStream
    val MsrTotalWpbEntries = MsrStreamCount * MsrWpbEntriesPerStream
    val MsrStreamIdWidth = MsrConfig.StreamIdWidth
    val MsrOffsetWidth = MsrConfig.InstructionOffsetWidth
    val MsrStreamGenerationWidth = MsrConfig.StreamGenerationWidth
    val MsrWpbOffsetWidth = log2Ceil(MsrWpbEntriesPerStream + 1)
    val MsrBlockPositionCount = 1 << FetchBlockInstOffsetWidth

    val msrRobSrcUsed = RegInit(VecInit.fill(RobSize)(VecInit.fill(backendParams.numSrc)(false.B)))
    val msrRobSrcRgid = RegInit(VecInit.fill(RobSize)(
      VecInit.fill(backendParams.numSrc)(MsrRgid.Null.U(MsrRgid.Width.W))
    ))
    val msrRobDestRgid = RegInit(VecInit.fill(RobSize)(MsrRgid.Null.U(MsrRgid.Width.W)))
    val msrRobPdest = RegInit(VecInit.fill(RobSize)(0.U(PhyRegIdxWidth.W)))
    val msrRobAlu = RegInit(VecInit.fill(RobSize)(false.B))
    val msrRobReusableAlu = RegInit(VecInit.fill(RobSize)(false.B))
    val msrRobPc = RegInit(VecInit.fill(RobSize)(0.U(VAddrBits.W)))
    val msrRobInstr = RegInit(VecInit.fill(RobSize)(0.U(32.W)))
    val msrRobFtqPtr = Reg(Vec(RobSize, new FtqPtr))
    val msrRobFtqOffset = RegInit(VecInit.fill(RobSize)(0.U(FetchBlockInstOffsetWidth.W)))

    val msrEnqAllocInt = VecInit(io.enq.req.map { req =>
      io.enq.canAccept && req.valid && req.bits.firstUop && !io.redirect.valid &&
        req.bits.rfWen && !req.bits.isMove
    })
    val msrEnqReusableAlu = VecInit(io.enq.req.map { req =>
      io.enq.canAccept && req.valid && req.bits.firstUop && !io.redirect.valid &&
        FuType.isAlu(req.bits.fuType) && req.bits.rfWen && !req.bits.isMove &&
        !req.bits.hasException && !req.bits.flushPipe && !req.bits.singleStep &&
        req.bits.firstUop && req.bits.lastUop && req.bits.destRgid =/= MsrRgid.Null.U
    })

    val msrMispredSample = redirectValidReg && redirectMisPredReg
    val msrSquashedInst = PopCount(redirectNeedFlush)
    val msrRgidResetPending = RegInit(false.B)
    val msrRgidDrainCount = RegInit(0.U(log2Ceil(RobSize + 1).W))
    val msrRgidCommitCount = Mux(io.commits.isCommit, commitCnt, 0.U)
    val msrRgidDrainNext = msrRgidDrainCount +& msrRgidCommitCount
    val msrRgidReset = msrRgidResetPending && msrRgidDrainNext >= RobSize.U

    when(io.msrRgid.overflow) {
      msrRgidResetPending := true.B
      msrRgidDrainCount := 0.U
    }.elsewhen(msrRgidResetPending) {
      when(msrRgidReset) {
        msrRgidResetPending := false.B
        msrRgidDrainCount := 0.U
      }.otherwise {
        msrRgidDrainCount := msrRgidDrainNext
      }
    }
    io.msrRgid.reset := msrRgidReset

    val msrStreamAdmission = !io.msrRgid.quarantine && !msrRgidResetPending && !io.msrRgid.overflow
    val msrCreateStream = msrMispredSample && msrSquashedInst.orR && msrStreamAdmission
    val msrNextStream = RegInit(0.U(log2Ceil(MsrStreamCount).W))
    val msrNewestStream = RegInit(0.U(MsrStreamIdWidth.W))
    val msrStreamValid = RegInit(VecInit.fill(MsrStreamCount)(false.B))
    val msrStreamGeneration = RegInit(VecInit.fill(MsrStreamCount)(0.U(MsrStreamGenerationWidth.W)))
    val msrStreamLength = RegInit(VecInit.fill(MsrStreamCount)(0.U(MsrOffsetWidth.W)))
    val msrStreamWpbLength = RegInit(VecInit.fill(MsrStreamCount)(0.U(MsrWpbOffsetWidth.W)))
    val msrWpbValid = RegInit(VecInit.fill(MsrTotalWpbEntries)(false.B))
    val msrWpbBlockPc = RegInit(VecInit.fill(MsrTotalWpbEntries)(0.U(VAddrBits.W)))
    val msrWpbFirstInstructionOffset = RegInit(VecInit.fill(MsrTotalWpbEntries)(0.U(MsrOffsetWidth.W)))
    val msrWpbInstructionStartMask = RegInit(VecInit.fill(MsrTotalWpbEntries)(0.U(MsrBlockPositionCount.W)))
    val msrLogValid = RegInit(VecInit.fill(MsrTotalEntries)(false.B))
    val msrLogConsumed = RegInit(VecInit.fill(MsrTotalEntries)(false.B))
    val msrLogCompleted = Reg(Vec(MsrTotalEntries, Bool()))
    val msrLogAlu = Reg(Vec(MsrTotalEntries, Bool()))
    val msrLogPc = Reg(Vec(MsrTotalEntries, UInt(VAddrBits.W)))
    val msrLogInstr = Reg(Vec(MsrTotalEntries, UInt(32.W)))
    val msrLogSrcUsed = RegInit(VecInit.fill(MsrTotalEntries)(VecInit.fill(backendParams.numSrc)(false.B)))
    val msrLogSrcRgid = RegInit(VecInit.fill(MsrTotalEntries)(
      VecInit.fill(backendParams.numSrc)(MsrRgid.Null.U(MsrRgid.Width.W))
    ))
    val msrLogDestRgid = RegInit(VecInit.fill(MsrTotalEntries)(MsrRgid.Null.U(MsrRgid.Width.W)))
    val msrLogPdest = RegInit(VecInit.fill(MsrTotalEntries)(0.U(PhyRegIdxWidth.W)))
    // Observational hold state only: the real freelist is deliberately unchanged.
    val msrLogPdestRetained = RegInit(VecInit.fill(MsrTotalEntries)(false.B))
    val msrLogReusableAlu = RegInit(VecInit.fill(MsrTotalEntries)(false.B))
    val msrLogHasStaticHit = RegInit(VecInit.fill(MsrStreamCount)(false.B))
    val msrLogHasCompletedStaticHit = RegInit(VecInit.fill(MsrStreamCount)(false.B))
    val msrLogHasSemanticHoldHit = RegInit(VecInit.fill(MsrStreamCount)(false.B))
    val msrLogHasFullReuseHit = RegInit(VecInit.fill(MsrStreamCount)(false.B))

    val msrCandidateValid = RegInit(false.B)
    val msrCandidateStreamId = RegInit(0.U(MsrStreamIdWidth.W))
    val msrCandidateStreamGeneration = RegInit(0.U(MsrStreamGenerationWidth.W))
    val msrCandidateWpbOffset = RegInit(0.U(MsrWpbOffsetWidth.W))
    val msrLastCandidateFtqValid = RegInit(false.B)
    val msrLastCandidateFtqPtr = Reg(new FtqPtr)
    val msrLastCandidateBlockPc = RegInit(0.U(VAddrBits.W))

    // redirectBegin is the entry immediately before the first flushed instruction.
    // Walk forward in ROB age order so a bounded stream retains the closest 128 entries.
    val msrCaptureRobIdx = Wire(Vec(MsrEntriesPerStream, UInt(log2Up(RobSize).W)))
    msrCaptureRobIdx(0) := Mux(redirectBegin >= (RobSize - 1).U, 0.U, redirectBegin + 1.U)
    for (i <- 1 until MsrEntriesPerStream) {
      msrCaptureRobIdx(i) := Mux(
        msrCaptureRobIdx(i - 1) === (RobSize - 1).U,
        0.U,
        msrCaptureRobIdx(i - 1) + 1.U
      )
    }

    val msrCaptureValid = Wire(Vec(MsrEntriesPerStream, Bool()))
    val msrCaptureNewBlock = Wire(Vec(MsrEntriesPerStream, Bool()))
    val msrCaptureBlockOrdinal = Wire(Vec(MsrEntriesPerStream, UInt(MsrOffsetWidth.W)))
    val msrCaptureBlockPc = Wire(Vec(MsrEntriesPerStream, UInt(VAddrBits.W)))
    for (entry <- 0 until MsrEntriesPerStream) {
      val robIdx = msrCaptureRobIdx(entry)
      msrCaptureValid(entry) := redirectNeedFlush(robIdx)
      msrCaptureBlockPc(entry) := msrRobPc(robIdx) -
        (msrRobFtqOffset(robIdx) << instOffsetBits)
      if (entry == 0) {
        msrCaptureNewBlock(entry) := msrCaptureValid(entry)
      } else {
        val previousRobIdx = msrCaptureRobIdx(entry - 1)
        msrCaptureNewBlock(entry) := msrCaptureValid(entry) &&
          (!msrCaptureValid(entry - 1) || msrRobFtqPtr(robIdx) =/= msrRobFtqPtr(previousRobIdx))
      }
      msrCaptureBlockOrdinal(entry) := Mux(
        msrCaptureValid(entry),
        PopCount(msrCaptureNewBlock.take(entry + 1)) - 1.U,
        0.U
      )
    }
    val msrCaptureBlockCount = PopCount(msrCaptureNewBlock)
    val msrCaptureWpbLength = Mux(
      msrCaptureBlockCount > MsrWpbEntriesPerStream.U,
      MsrWpbEntriesPerStream.U,
      msrCaptureBlockCount
    )
    val msrCaptureTrackedValid = VecInit((0 until MsrEntriesPerStream).map { entry =>
      msrCaptureValid(entry) && msrCaptureBlockOrdinal(entry) < MsrWpbEntriesPerStream.U
    })
    val msrCaptureStreamLength = PopCount(msrCaptureTrackedValid)

    def msrWpbIndex(streamId: UInt, blockOffset: UInt): UInt =
      Cat(streamId, blockOffset(log2Ceil(MsrWpbEntriesPerStream) - 1, 0))

    val msrCandidateStateCurrent = msrCandidateValid &&
      msrStreamValid(msrCandidateStreamId) &&
      msrStreamGeneration(msrCandidateStreamId) === msrCandidateStreamGeneration &&
      msrCandidateWpbOffset < msrStreamWpbLength(msrCandidateStreamId)

    val msrQueryActive = Wire(Vec(RenameWidth + 1, Bool()))
    val msrQueryStreamId = Wire(Vec(RenameWidth + 1, UInt(MsrStreamIdWidth.W)))
    val msrQueryGeneration = Wire(Vec(RenameWidth + 1, UInt(MsrStreamGenerationWidth.W)))
    val msrQueryWpbOffset = Wire(Vec(RenameWidth + 1, UInt(MsrWpbOffsetWidth.W)))
    val msrQueryLastFtqValid = Wire(Vec(RenameWidth + 1, Bool()))
    val msrQueryLastFtqPtr = Wire(Vec(RenameWidth + 1, new FtqPtr))
    val msrQueryLastBlockPc = Wire(Vec(RenameWidth + 1, UInt(VAddrBits.W)))
    val msrCandidateDiscovery = Wire(Vec(RenameWidth, Bool()))
    val msrCandidateLayoutMismatch = Wire(Vec(RenameWidth, Bool()))
    val msrCandidateContextReject = Wire(Vec(RenameWidth, Bool()))

    msrQueryActive(0) := msrCandidateStateCurrent
    msrQueryStreamId(0) := msrCandidateStreamId
    msrQueryGeneration(0) := msrCandidateStreamGeneration
    msrQueryWpbOffset(0) := msrCandidateWpbOffset
    msrQueryLastFtqValid(0) := msrLastCandidateFtqValid
    msrQueryLastFtqPtr(0) := msrLastCandidateFtqPtr
    msrQueryLastBlockPc(0) := msrLastCandidateBlockPc

    for (lane <- 0 until RenameWidth) {
      val req = io.msrCandidate.req(lane)
      val blockPc = req.pc - (req.ftqOffset << instOffsetBits)
      val newBlock = !msrQueryLastFtqValid(lane) || req.ftqPtr =/= msrQueryLastFtqPtr(lane)
      val continuationOffset = Mux(
        newBlock,
        msrQueryWpbOffset(lane) + 1.U,
        msrQueryWpbOffset(lane)
      )
      val continuationIndex = msrWpbIndex(msrQueryStreamId(lane), continuationOffset)
      val continuationValid = msrQueryActive(lane) &&
        continuationOffset < msrStreamWpbLength(msrQueryStreamId(lane)) &&
        msrWpbValid(continuationIndex) && msrWpbBlockPc(continuationIndex) === blockPc

      val discoveryEntryMatch = Wire(Vec(MsrStreamCount, UInt(MsrWpbEntriesPerStream.W)))
      val discoveryStreamMatch = Wire(Vec(MsrStreamCount, Bool()))
      val contextReject = Wire(Vec(MsrStreamCount, Bool()))
      for (stream <- 0 until MsrStreamCount) {
        val rawTagMatch = VecInit((0 until MsrWpbEntriesPerStream).map { block =>
          val index = stream * MsrWpbEntriesPerStream + block
          msrStreamValid(stream) && block.U < msrStreamWpbLength(stream) &&
            msrWpbValid(index) && msrWpbBlockPc(index) === blockPc
        })
        val contextMatch = VecInit((0 until MsrWpbEntriesPerStream).map { block =>
          if (block == 0) {
            true.B
          } else {
            !msrQueryLastFtqValid(lane) ||
              msrWpbBlockPc(stream * MsrWpbEntriesPerStream + block - 1) === msrQueryLastBlockPc(lane)
          }
        })
        discoveryEntryMatch(stream) := VecInit(rawTagMatch.zip(contextMatch).map {
          case (tagMatch, predecessorMatch) => tagMatch && predecessorMatch
        }).asUInt
        discoveryStreamMatch(stream) := discoveryEntryMatch(stream).orR
        contextReject(stream) := VecInit(rawTagMatch.zip(contextMatch).map {
          case (tagMatch, predecessorMatch) => tagMatch && !predecessorMatch
        }).asUInt.orR
      }
      val otherStream = ~msrNewestStream
      val discoveryStreamId = Mux(discoveryStreamMatch(msrNewestStream), msrNewestStream, otherStream)
      val discoveryValid = req.valid && newBlock && !continuationValid && msrStreamAdmission &&
        (discoveryStreamMatch(msrNewestStream) || discoveryStreamMatch(otherStream))
      val discoveryWpbOffset = PriorityEncoder(discoveryEntryMatch(discoveryStreamId))

      val selectedValid = continuationValid || discoveryValid
      val selectedStreamId = Mux(continuationValid, msrQueryStreamId(lane), discoveryStreamId)
      val selectedGeneration = Mux(
        continuationValid,
        msrQueryGeneration(lane),
        msrStreamGeneration(discoveryStreamId)
      )
      val selectedWpbOffset = Mux(continuationValid, continuationOffset, discoveryWpbOffset)
      val selectedWpbIndex = msrWpbIndex(selectedStreamId, selectedWpbOffset)
      val instructionStartMask = msrWpbInstructionStartMask(selectedWpbIndex)
      val instructionStartPresent = instructionStartMask(req.ftqOffset)
      val instructionOrdinal = PopCount((0 until MsrBlockPositionCount).map { position =>
        instructionStartMask(position) && position.U < req.ftqOffset
      })
      val instructionOffset = msrWpbFirstInstructionOffset(selectedWpbIndex) + instructionOrdinal
      val responseValid = req.valid && selectedValid && instructionStartPresent &&
        instructionOffset < msrStreamLength(selectedStreamId)

      io.msrCandidate.resp(lane).valid := responseValid
      io.msrCandidate.resp(lane).streamId := selectedStreamId
      io.msrCandidate.resp(lane).streamGeneration := selectedGeneration
      io.msrCandidate.resp(lane).instructionOffset := instructionOffset
      msrCandidateDiscovery(lane) := req.fire && responseValid && discoveryValid
      msrCandidateLayoutMismatch(lane) := req.fire && selectedValid && !instructionStartPresent
      msrCandidateContextReject(lane) := req.fire && newBlock && !continuationValid && contextReject.asUInt.orR

      msrQueryActive(lane + 1) := Mux(req.valid, responseValid, msrQueryActive(lane))
      msrQueryStreamId(lane + 1) := Mux(req.valid, selectedStreamId, msrQueryStreamId(lane))
      msrQueryGeneration(lane + 1) := Mux(req.valid, selectedGeneration, msrQueryGeneration(lane))
      msrQueryWpbOffset(lane + 1) := Mux(req.valid, selectedWpbOffset, msrQueryWpbOffset(lane))
      msrQueryLastFtqValid(lane + 1) := Mux(req.valid, true.B, msrQueryLastFtqValid(lane))
      msrQueryLastFtqPtr(lane + 1) := Mux(req.valid, req.ftqPtr, msrQueryLastFtqPtr(lane))
      msrQueryLastBlockPc(lane + 1) := Mux(req.valid, blockPc, msrQueryLastBlockPc(lane))
    }

    val msrEnqPc = io.enq.req.map(_.bits.msrPc)
    val msrEnqInstr = io.enq.req.map(_.bits.msrInstr)

    val msrLogMatchOH = Wire(Vec(RenameWidth, UInt(MsrTotalEntries.W)))
    val msrLogMatchCompleted = Wire(Vec(RenameWidth, Bool()))
    val msrLogMatchAlu = Wire(Vec(RenameWidth, Bool()))
    val msrLogMatchReusableAlu = Wire(Vec(RenameWidth, Bool()))
    val msrLogMatchRgid = Wire(Vec(RenameWidth, Bool()))
    val msrLogMatchFullReuse = Wire(Vec(RenameWidth, Bool()))
    val msrCandidateTupleValid = Wire(Vec(RenameWidth, Bool()))
    val msrCandidateStaticGuard = Wire(Vec(RenameWidth, Bool()))
    val msrCandidateNullRgidReject = Wire(Vec(RenameWidth, Bool()))
    for (i <- 0 until RenameWidth) {
      val candidate = io.enq.req(i).bits.msrCandidate
      val instructionOffset = candidate.instructionOffset
      val offsetInRange = instructionOffset < MsrEntriesPerStream.U &&
        instructionOffset < msrStreamLength(candidate.streamId)
      val streamGenerationCurrent = msrStreamValid(candidate.streamId) &&
        msrStreamGeneration(candidate.streamId) === candidate.streamGeneration
      val entryIndex = Cat(candidate.streamId, instructionOffset(log2Ceil(MsrEntriesPerStream) - 1, 0))
      val entryAvailable = msrLogValid(entryIndex) && !msrLogConsumed(entryIndex)
      msrCandidateTupleValid(i) := instEnqValidSeq(i) && candidate.valid &&
        offsetInRange && streamGenerationCurrent && entryAvailable && msrStreamAdmission && !msrMispredSample
      msrCandidateStaticGuard(i) := msrCandidateTupleValid(i) &&
        msrLogPc(entryIndex) === msrEnqPc(i) && msrLogInstr(entryIndex) === msrEnqInstr(i)

      val allSrcRgidMatch = (0 until backendParams.numSrc).map { src =>
        val currentSrcUsed = SrcType.isXp(io.enq.req(i).bits.srcType(src)) &&
          io.enq.req(i).bits.psrc(src) =/= 0.U
        val currentRgid = io.enq.req(i).bits.srcRgid(src)
        val loggedRgid = msrLogSrcRgid(entryIndex)(src)
        msrLogSrcUsed(entryIndex)(src) === currentSrcUsed &&
          (!currentSrcUsed ||
            (currentRgid =/= MsrRgid.Null.U && loggedRgid =/= MsrRgid.Null.U && currentRgid === loggedRgid))
      }.reduce(_ && _)
      msrCandidateNullRgidReject(i) := msrCandidateStaticGuard(i) &&
        (0 until backendParams.numSrc).map { src =>
          val currentSrcUsed = SrcType.isXp(io.enq.req(i).bits.srcType(src)) &&
            io.enq.req(i).bits.psrc(src) =/= 0.U
          currentSrcUsed && (io.enq.req(i).bits.srcRgid(src) === MsrRgid.Null.U ||
            msrLogSrcRgid(entryIndex)(src) === MsrRgid.Null.U)
        }.reduce(_ || _)

      val staticMatchOH = Mux(msrCandidateStaticGuard(i), UIntToOH(entryIndex, MsrTotalEntries), 0.U)
      val completedStaticMatch = msrCandidateStaticGuard(i) && msrLogCompleted(entryIndex)
      val reusableAluMatch = completedStaticMatch && msrLogReusableAlu(entryIndex) && msrEnqReusableAlu(i)
      val rgidMatch = reusableAluMatch && allSrcRgidMatch
      val allocatedInCurrentGroup = VecInit((0 until RenameWidth).map { lane =>
        msrEnqAllocInt(lane) && io.enq.req(lane).bits.pdest === msrLogPdest(entryIndex)
      }).asUInt.orR

      msrLogMatchOH(i) := staticMatchOH
      msrLogMatchCompleted(i) := completedStaticMatch
      msrLogMatchAlu(i) := msrCandidateStaticGuard(i) && msrLogAlu(entryIndex)
      msrLogMatchReusableAlu(i) := reusableAluMatch
      msrLogMatchRgid(i) := rgidMatch
      msrLogMatchFullReuse(i) := rgidMatch && msrLogPdestRetained(entryIndex) && !allocatedInCurrentGroup

      assert(PopCount(msrLogMatchOH(i)) <= 1.U, "MSR position lookup selected more than one entry")
      when(instEnqValidSeq(i) && candidate.valid) {
        assert(candidate.instructionOffset < MsrEntriesPerStream.U,
          "transported MSR candidate offset exceeded the Squash Log bound")
      }
      when(msrLogMatchOH(i).orR) {
        assert(!msrLogConsumed(entryIndex), "MSR entry was consumed more than once")
        assert(candidate.streamGeneration === msrStreamGeneration(candidate.streamId),
          "MSR accepted a stale stream generation")
        assert(entryIndex(entryIndex.getWidth - 1) === candidate.streamId,
          "MSR candidate crossed into another stream")
      }
    }
    for (lane <- 0 until RenameWidth; prior <- 0 until lane) {
      assert(!(msrLogMatchOH(lane) & msrLogMatchOH(prior)).orR,
        "two current instructions selected the same Squash Log entry")
    }

    for (lane <- 0 until RenameWidth) {
      when(instEnqValidSeq(lane) && !io.redirect.valid) {
        val robIdx = enqRobIdxSeq(lane)
        for (src <- 0 until backendParams.numSrc) {
          msrRobSrcUsed(robIdx)(src) := SrcType.isXp(io.enq.req(lane).bits.srcType(src)) &&
            io.enq.req(lane).bits.psrc(src) =/= 0.U
          msrRobSrcRgid(robIdx)(src) := io.enq.req(lane).bits.srcRgid(src)
        }
        msrRobDestRgid(robIdx) := io.enq.req(lane).bits.destRgid
        msrRobPdest(robIdx) := io.enq.req(lane).bits.pdest
        msrRobAlu(robIdx) := FuType.isAlu(io.enq.req(lane).bits.fuType)
        msrRobReusableAlu(robIdx) := msrEnqReusableAlu(lane)
        msrRobPc(robIdx) := io.enq.req(lane).bits.msrPc
        msrRobInstr(robIdx) := io.enq.req(lane).bits.msrInstr
        msrRobFtqPtr(robIdx) := io.enq.req(lane).bits.ftqPtr
        msrRobFtqOffset(robIdx) := io.enq.req(lane).bits.ftqOffset
        for (prior <- 0 until lane) {
          when(instEnqValidSeq(prior)) {
            assert(enqRobIdxSeq(prior) =/= robIdx,
              "MSR instruction-granular capture requires one original instruction per ROB entry")
          }
        }
      }
    }

    val msrCandidateQueryFire = io.msrCandidate.req.map(_.fire).reduce(_ || _)
    val msrCandidateStaticDivergence = msrCandidateTupleValid.zip(msrCandidateStaticGuard).map {
      case (valid, guard) => valid && !guard
    }.reduce(_ || _)
    when(io.msrRgid.overflow || io.redirect.valid || msrCreateStream || msrCandidateStaticDivergence) {
      msrCandidateValid := false.B
      msrLastCandidateFtqValid := false.B
    }.elsewhen(msrCandidateQueryFire) {
      msrCandidateValid := msrQueryActive(RenameWidth)
      msrCandidateStreamId := msrQueryStreamId(RenameWidth)
      msrCandidateStreamGeneration := msrQueryGeneration(RenameWidth)
      msrCandidateWpbOffset := msrQueryWpbOffset(RenameWidth)
      msrLastCandidateFtqValid := msrQueryLastFtqValid(RenameWidth)
      msrLastCandidateFtqPtr := msrQueryLastFtqPtr(RenameWidth)
      msrLastCandidateBlockPc := msrQueryLastBlockPc(RenameWidth)
    }

    val msrStaticHitCount = PopCount(msrLogMatchOH.map(_.orR))
    val msrCompletedStaticHitCount = PopCount(msrLogMatchCompleted)
    val msrCompletedAluStaticHitCount = PopCount(msrLogMatchCompleted.zip(msrLogMatchAlu).map {
      case (completed, alu) => completed && alu
    })
    val msrCompletedReusableAluStaticHitCount = PopCount(msrLogMatchReusableAlu)
    val msrCompletedReusableAluRgidHitCount = PopCount(msrLogMatchRgid)
    val msrCompletedReusableAluSemanticHoldHitCount = msrCompletedReusableAluRgidHitCount
    val msrCompletedReusableAluRgidPRegIntactHitCount = PopCount(msrLogMatchFullReuse)
    val msrStreamStaticHit = VecInit((0 until MsrStreamCount).map { stream =>
      msrLogMatchOH.map(_((stream + 1) * MsrEntriesPerStream - 1, stream * MsrEntriesPerStream).orR)
        .reduce(_ || _)
    })
    val msrStreamCompletedStaticHit = VecInit((0 until MsrStreamCount).map { stream =>
      msrLogMatchOH.zip(msrLogMatchCompleted).map { case (matched, completed) =>
        matched((stream + 1) * MsrEntriesPerStream - 1, stream * MsrEntriesPerStream).orR && completed
      }.reduce(_ || _)
    })
    val msrStreamFullReuseHit = VecInit((0 until MsrStreamCount).map { stream =>
      msrLogMatchOH.zip(msrLogMatchFullReuse).map { case (matched, fullReuse) =>
        matched((stream + 1) * MsrEntriesPerStream - 1, stream * MsrEntriesPerStream).orR && fullReuse
      }.reduce(_ || _)
    })
    val msrStreamSemanticHoldHit = VecInit((0 until MsrStreamCount).map { stream =>
      msrLogMatchOH.zip(msrLogMatchRgid).map { case (matched, semanticHold) =>
        matched((stream + 1) * MsrEntriesPerStream - 1, stream * MsrEntriesPerStream).orR && semanticHold
      }.reduce(_ || _)
    })
    val msrNewStreamStaticHitCount = PopCount(msrStreamStaticHit.zip(msrLogHasStaticHit).map {
      case (hit, hasHit) => hit && !hasHit
    })
    val msrNewStreamCompletedStaticHitCount = PopCount(
      msrStreamCompletedStaticHit.zip(msrLogHasCompletedStaticHit).map {
        case (hit, hasHit) => hit && !hasHit
      }
    )
    val msrNewStreamFullReuseHitCount = PopCount(msrStreamFullReuseHit.zip(msrLogHasFullReuseHit).map {
      case (hit, hasHit) => hit && !hasHit
    })
    val msrNewStreamSemanticHoldHitCount = PopCount(
      msrStreamSemanticHoldHit.zip(msrLogHasSemanticHoldHit).map {
        case (hit, hasHit) => hit && !hasHit
      }
    )
    val msrReplacedStreamValid = msrStreamValid(msrNextStream)
    val msrLogOverwritten = msrCreateStream && msrReplacedStreamValid
    val msrLogOverwrittenBeforeStaticHit = msrLogOverwritten && !msrLogHasStaticHit(msrNextStream)
    val msrLogOverwrittenBeforeCompletedStaticHit =
      msrLogOverwritten && !msrLogHasCompletedStaticHit(msrNextStream)

    when(io.msrRgid.overflow) {
      msrStreamValid.foreach(_ := false.B)
      msrStreamLength.foreach(_ := 0.U)
      msrStreamWpbLength.foreach(_ := 0.U)
      msrWpbValid.foreach(_ := false.B)
      msrLogValid.foreach(_ := false.B)
      msrLogConsumed.foreach(_ := false.B)
      msrLogPdestRetained.foreach(_ := false.B)
      msrLogHasStaticHit.foreach(_ := false.B)
      msrLogHasCompletedStaticHit.foreach(_ := false.B)
      msrLogHasSemanticHoldHit.foreach(_ := false.B)
      msrLogHasFullReuseHit.foreach(_ := false.B)
    }.elsewhen(msrCreateStream) {
      for (stream <- 0 until MsrStreamCount) {
        when(msrNextStream === stream.U) {
          msrStreamValid(stream) := true.B
          msrStreamGeneration(stream) := msrStreamGeneration(stream) + 1.U
          msrStreamLength(stream) := msrCaptureStreamLength
          msrStreamWpbLength(stream) := msrCaptureWpbLength
          for (entry <- 0 until MsrEntriesPerStream) {
            val logIdx = stream * MsrEntriesPerStream + entry
            val robIdx = msrCaptureRobIdx(entry)
            val captureValid = msrCaptureTrackedValid(entry)
            msrLogValid(logIdx) := captureValid
            msrLogConsumed(logIdx) := false.B
            when(captureValid) {
              msrLogCompleted(logIdx) := robEntries(robIdx).isWritebacked
              msrLogAlu(logIdx) := msrRobAlu(robIdx)
              msrLogPc(logIdx) := msrRobPc(robIdx)
              msrLogInstr(logIdx) := msrRobInstr(robIdx)
              for (src <- 0 until backendParams.numSrc) {
                msrLogSrcUsed(logIdx)(src) := msrRobSrcUsed(robIdx)(src)
                msrLogSrcRgid(logIdx)(src) := msrRobSrcRgid(robIdx)(src)
              }
              msrLogDestRgid(logIdx) := msrRobDestRgid(robIdx)
              msrLogPdest(logIdx) := msrRobPdest(robIdx)
              msrLogPdestRetained(logIdx) :=
                robEntries(robIdx).isWritebacked && msrRobReusableAlu(robIdx)
              msrLogReusableAlu(logIdx) := msrRobReusableAlu(robIdx)
            }
          }
          for (block <- 0 until MsrWpbEntriesPerStream) {
            val wpbIdx = stream * MsrWpbEntriesPerStream + block
            val blockSelect = VecInit((0 until MsrEntriesPerStream).map { entry =>
              msrCaptureTrackedValid(entry) && msrCaptureBlockOrdinal(entry) === block.U
            })
            val blockFirstSelect = VecInit((0 until MsrEntriesPerStream).map { entry =>
              msrCaptureNewBlock(entry) && msrCaptureBlockOrdinal(entry) === block.U
            })
            val instructionMask = (0 until MsrEntriesPerStream).map { entry =>
              val robIdx = msrCaptureRobIdx(entry)
              Mux(
                blockSelect(entry),
                UIntToOH(msrRobFtqOffset(robIdx), MsrBlockPositionCount),
                0.U(MsrBlockPositionCount.W)
              )
            }.reduce(_ | _)
            msrWpbValid(wpbIdx) := blockFirstSelect.asUInt.orR
            msrWpbBlockPc(wpbIdx) := Mux1H(blockFirstSelect, msrCaptureBlockPc)
            msrWpbFirstInstructionOffset(wpbIdx) := PriorityEncoder(blockFirstSelect)
            msrWpbInstructionStartMask(wpbIdx) := instructionMask
            when(blockFirstSelect.asUInt.orR) {
              assert(PopCount(blockFirstSelect) === 1.U,
                "each WPB block must have exactly one first Squash Log instruction")
            }
          }
          msrLogHasStaticHit(stream) := false.B
          msrLogHasCompletedStaticHit(stream) := false.B
          msrLogHasSemanticHoldHit(stream) := false.B
          msrLogHasFullReuseHit(stream) := false.B
        }
      }
      msrNewestStream := msrNextStream
      msrNextStream := Mux(msrNextStream === (MsrStreamCount - 1).U, 0.U, msrNextStream + 1.U)
    }.elsewhen(!msrMispredSample) {
      for (i <- 0 until MsrTotalEntries) {
        when(msrLogMatchOH.map(_(i)).reduce(_ || _)) {
          msrLogConsumed(i) := true.B
        }
      }
      for (stream <- 0 until MsrStreamCount) {
        when(msrStreamStaticHit(stream)) {
          msrLogHasStaticHit(stream) := true.B
        }
        when(msrStreamCompletedStaticHit(stream)) {
          msrLogHasCompletedStaticHit(stream) := true.B
        }
        when(msrStreamSemanticHoldHit(stream)) {
          msrLogHasSemanticHoldHit(stream) := true.B
        }
        when(msrStreamFullReuseHit(stream)) {
          msrLogHasFullReuseHit(stream) := true.B
        }
      }
    }

    when(!msrCreateStream) {
      for (entry <- 0 until MsrTotalEntries) {
        val pdestReallocated = VecInit((0 until RenameWidth).map { lane =>
          msrEnqAllocInt(lane) && io.enq.req(lane).bits.pdest === msrLogPdest(entry)
        }).asUInt.orR
        when(pdestReallocated) {
          msrLogPdestRetained(entry) := false.B
        }
      }
    }

    assert(msrStreamLength.map(_ <= MsrEntriesPerStream.U).reduce(_ && _),
      "MSR stream length exceeded the 128-entry Squash Log")
    assert(msrStreamWpbLength.map(_ <= MsrWpbEntriesPerStream.U).reduce(_ && _),
      "MSR stream length exceeded the 32-entry WPB")
    when(msrCandidateStateCurrent) {
      assert(msrCandidateWpbOffset < msrStreamWpbLength(msrCandidateStreamId),
        "MSR candidate WPB cursor exceeded the selected stream")
    }

    val msrCompletedSquashedInst = PopCount(redirectNeedFlush.zip(robEntries).map { case (flush, entry) =>
      flush && entry.isWritebacked
    })
    val msrCompletedSquashedAluInst = PopCount(redirectNeedFlush.zip(robEntries).zip(msrRobAlu).map {
      case ((flush, entry), isAlu) => flush && entry.isWritebacked && isAlu
    })

    XSPerfAccumulate("msr_mispred_with_squashed_inst", msrMispredSample && msrSquashedInst.orR)
    XSPerfAccumulate("msr_mispred_with_completed_inst", msrMispredSample && msrCompletedSquashedInst.orR)
    XSPerfAccumulate("msr_mispred_with_completed_alu_inst", msrMispredSample && msrCompletedSquashedAluInst.orR)
    XSPerfAccumulate("msr_squashed_inst", Mux(msrMispredSample, msrSquashedInst, 0.U))
    XSPerfAccumulate("msr_completed_squashed_inst", Mux(msrMispredSample, msrCompletedSquashedInst, 0.U))
    XSPerfAccumulate("msr_completed_squashed_alu_inst", Mux(msrMispredSample, msrCompletedSquashedAluInst, 0.U))
    XSPerfAccumulate("msr_log_truncated", msrMispredSample &&
      (msrSquashedInst > MsrEntriesPerStream.U || msrCaptureBlockCount > MsrWpbEntriesPerStream.U))
    XSPerfAccumulate(
      "msr_squashed_inst_not_logged",
      Mux(msrMispredSample, msrSquashedInst - msrCaptureStreamLength, 0.U)
    )
    XSPerfAccumulate("msr_log_overwritten", msrLogOverwritten)
    XSPerfAccumulate("msr_log_overwritten_before_static_hit", msrLogOverwrittenBeforeStaticHit)
    XSPerfAccumulate(
      "msr_log_overwritten_before_completed_static_hit",
      msrLogOverwrittenBeforeCompletedStaticHit
    )
    XSPerfAccumulate("msr_static_hit_inst", msrStaticHitCount)
    XSPerfAccumulate("msr_completed_static_hit_inst", msrCompletedStaticHitCount)
    XSPerfAccumulate("msr_completed_alu_static_hit_inst", msrCompletedAluStaticHitCount)
    XSPerfAccumulate("msr_completed_reusable_alu_static_hit_inst", msrCompletedReusableAluStaticHitCount)
    XSPerfAccumulate("msr_completed_reusable_alu_rgid_hit_inst", msrCompletedReusableAluRgidHitCount)
    XSPerfAccumulate(
      "msr_completed_reusable_alu_semantic_hold_hit_inst",
      msrCompletedReusableAluSemanticHoldHitCount
    )
    XSPerfAccumulate(
      "msr_completed_reusable_alu_rgid_preg_intact_hit_inst",
      msrCompletedReusableAluRgidPRegIntactHitCount
    )
    XSPerfAccumulate("msr_log_with_static_hit", msrNewStreamStaticHitCount)
    XSPerfAccumulate("msr_log_with_completed_static_hit", msrNewStreamCompletedStaticHitCount)
    XSPerfAccumulate("msr_log_with_semantic_hold_hit", msrNewStreamSemanticHoldHitCount)
    XSPerfAccumulate("msr_log_with_full_reuse_hit", msrNewStreamFullReuseHitCount)
    XSPerfAccumulate("msr_stream_created", msrCreateStream)
    XSPerfAccumulate("msr_stream_replaced", msrLogOverwritten)
    XSPerfAccumulate(
      "msr_entry_captured",
      Mux(msrCreateStream, msrCaptureStreamLength, 0.U)
    )
    XSPerfAccumulate("msr_wpb_block_captured", Mux(msrCreateStream, msrCaptureWpbLength, 0.U))
    XSPerfAccumulate("msr_candidate_acquired", PopCount(msrCandidateDiscovery))
    XSPerfAccumulate("msr_candidate_layout_mismatch", PopCount(msrCandidateLayoutMismatch))
    XSPerfAccumulate("msr_candidate_context_reject", PopCount(msrCandidateContextReject))
    XSPerfAccumulate("msr_candidate_provenance_inst", PopCount(io.msrCandidate.req.zip(io.msrCandidate.resp).map {
      case (req, resp) => req.fire && resp.valid
    }))
    XSPerfAccumulate("msr_position_candidate_inst", PopCount(msrCandidateTupleValid))
    XSPerfAccumulate("msr_position_static_divergence", PopCount(
      msrCandidateTupleValid.zip(msrCandidateStaticGuard).map { case (valid, guard) => valid && !guard }
    ))
    XSPerfAccumulate("msr_rgid_null_reject", PopCount(msrCandidateNullRgidReject))
    XSPerfAccumulate("msr_stale_stream_generation", msrCandidateValid && !msrCandidateStateCurrent)
    XSPerfAccumulate("msr_rgid_drain_cycle", msrRgidResetPending)
    XSPerfAccumulate("msr_rgid_global_reset", msrRgidReset)
  }
}
