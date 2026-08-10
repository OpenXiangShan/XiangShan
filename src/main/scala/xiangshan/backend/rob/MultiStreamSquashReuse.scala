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
    val MsrInstSlotsPerRob = RenameWidth
    val MsrRobInstCountWidth = log2Ceil(MsrInstSlotsPerRob + 1)
    val MsrRobInstSlotWidth = log2Ceil(MsrInstSlotsPerRob)

    // ROB compression can place several original instructions in one ROB entry.
    // Preserve one metadata slot per original instruction so Squash Log offsets
    // remain instruction-granular.
    val msrRobInstCount = RegInit(VecInit.fill(RobSize)(0.U(MsrRobInstCountWidth.W)))
    val msrRobSrcUsed = RegInit(VecInit.fill(RobSize)(VecInit.fill(MsrInstSlotsPerRob)(
      VecInit.fill(backendParams.numSrc)(false.B)
    )))
    val msrRobSrcRgid = RegInit(VecInit.fill(RobSize)(VecInit.fill(MsrInstSlotsPerRob)(
      VecInit.fill(backendParams.numSrc)(MsrRgid.Null.U(MsrRgid.Width.W))
    )))
    val msrRobDestRgid = RegInit(VecInit.fill(RobSize)(VecInit.fill(MsrInstSlotsPerRob)(
      MsrRgid.Null.U(MsrRgid.Width.W)
    )))
    val msrRobPdest = RegInit(VecInit.fill(RobSize)(VecInit.fill(MsrInstSlotsPerRob)(
      0.U(PhyRegIdxWidth.W)
    )))
    val msrRobAlu = RegInit(VecInit.fill(RobSize)(VecInit.fill(MsrInstSlotsPerRob)(false.B)))
    val msrRobReusableAlu = RegInit(VecInit.fill(RobSize)(VecInit.fill(MsrInstSlotsPerRob)(false.B)))
    val msrRobOwnsIntPReg = RegInit(VecInit.fill(RobSize)(VecInit.fill(MsrInstSlotsPerRob)(false.B)))
    val msrRobReused = RegInit(VecInit.fill(RobSize)(VecInit.fill(MsrInstSlotsPerRob)(false.B)))
    val msrRobPc = RegInit(VecInit.fill(RobSize)(VecInit.fill(MsrInstSlotsPerRob)(0.U(VAddrBits.W))))
    val msrRobInstr = RegInit(VecInit.fill(RobSize)(VecInit.fill(MsrInstSlotsPerRob)(0.U(32.W))))
    val msrRobFtqPtr = Reg(Vec(RobSize, Vec(MsrInstSlotsPerRob, new FtqPtr)))
    val msrRobFtqOffset = RegInit(VecInit.fill(RobSize)(VecInit.fill(MsrInstSlotsPerRob)(
      0.U(FetchBlockInstOffsetWidth.W)
    )))

    val msrEnqAllocInt = VecInit(io.enq.req.map { req =>
      io.enq.canAccept && req.valid && req.bits.msrOriginalFirstUop && !io.redirect.valid &&
        req.bits.rfWen && !req.bits.isMove && !req.bits.msrReused
    })
    val msrEnqReusableAlu = VecInit(io.enq.req.map { req =>
      io.enq.canAccept && req.valid && req.bits.msrOriginalFirstUop && !io.redirect.valid &&
        req.bits.msrReusable && req.bits.destRgid =/= MsrRgid.Null.U
    })
    val msrEnqOwnsIntPReg = VecInit(io.enq.req.map { req =>
      io.enq.canAccept && req.valid && req.bits.msrOriginalFirstUop && !io.redirect.valid &&
        req.bits.rfWen && !req.bits.isMove
    })
    val msrInstEnqValid = VecInit(io.enq.req.map { req =>
      io.enq.canAccept && req.valid && req.bits.msrOriginalFirstUop && !io.redirect.valid
    })
    val msrInstEnqCount = VecInit(io.enq.req.zip(msrInstEnqValid).map { case (req, valid) =>
      Mux(valid, 1.U +& req.bits.msrFusionSecondValid, 0.U)
    })

    val msrMispredSample = redirectValidReg && redirectMisPredReg
    val msrRobCompletesThisCycle = VecInit((0 until RobSize).map { robIdx =>
      val writebackMatch = exuWBs.map { writeback =>
        writeback.valid && writeback.bits.robIdx.value === robIdx.U
      }
      val writebackCount = Mux1H(writebackMatch, io.writebackNums.map(_.bits))
      robEntries(robIdx).isWritebacked ||
        (robEntries(robIdx).valid && writebackCount.orR && robEntries(robIdx).uopNum === writebackCount)
    })
    val msrSquashedRobEntries = PopCount(redirectNeedFlush)
    val msrSquashedInst = (0 until RobSize).map { robIdx =>
      Mux(redirectNeedFlush(robIdx), msrRobInstCount(robIdx), 0.U)
    }.reduce(_ +& _)
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
    // True while the entry owns a PReg excluded from ordinary freelist allocation.
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

    // redirectBegin is the ROB entry immediately before the first flushed entry.
    // Walk instruction slots first, then advance to the next ROB entry, so a
    // bounded stream retains the closest 128 original instructions.
    val msrCaptureRobIdx = Wire(Vec(MsrEntriesPerStream, UInt(log2Up(RobSize).W)))
    val msrCaptureInstSlot = Wire(Vec(MsrEntriesPerStream, UInt(MsrRobInstSlotWidth.W)))
    msrCaptureRobIdx(0) := Mux(redirectBegin >= (RobSize - 1).U, 0.U, redirectBegin + 1.U)
    msrCaptureInstSlot(0) := 0.U
    for (i <- 1 until MsrEntriesPerStream) {
      val previousRobIdx = msrCaptureRobIdx(i - 1)
      val previousSlot = msrCaptureInstSlot(i - 1)
      val nextSlot = previousSlot +& 1.U
      val hasNextSlot = redirectNeedFlush(previousRobIdx) &&
        nextSlot < msrRobInstCount(previousRobIdx)
      msrCaptureRobIdx(i) := Mux(hasNextSlot, previousRobIdx, Mux(
          previousRobIdx === (RobSize - 1).U,
          0.U,
          previousRobIdx + 1.U
        ))
      msrCaptureInstSlot(i) := Mux(hasNextSlot, nextSlot, 0.U)
    }

    val msrCaptureValid = Wire(Vec(MsrEntriesPerStream, Bool()))
    val msrCaptureNewBlock = Wire(Vec(MsrEntriesPerStream, Bool()))
    val msrCaptureBlockOrdinal = Wire(Vec(MsrEntriesPerStream, UInt(MsrOffsetWidth.W)))
    val msrCaptureBlockPc = Wire(Vec(MsrEntriesPerStream, UInt(VAddrBits.W)))
    for (entry <- 0 until MsrEntriesPerStream) {
      val robIdx = msrCaptureRobIdx(entry)
      val instSlot = msrCaptureInstSlot(entry)
      msrCaptureValid(entry) := redirectNeedFlush(robIdx) && instSlot < msrRobInstCount(robIdx)
      msrCaptureBlockPc(entry) := msrRobPc(robIdx)(instSlot) -
        (msrRobFtqOffset(robIdx)(instSlot) << instOffsetBits)
      if (entry == 0) {
        msrCaptureNewBlock(entry) := msrCaptureValid(entry)
      } else {
        val previousRobIdx = msrCaptureRobIdx(entry - 1)
        val previousSlot = msrCaptureInstSlot(entry - 1)
        msrCaptureNewBlock(entry) := msrCaptureValid(entry) &&
          (!msrCaptureValid(entry - 1) ||
            msrRobFtqPtr(robIdx)(instSlot) =/= msrRobFtqPtr(previousRobIdx)(previousSlot))
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
    val msrCandidateAmbiguousPosition = Wire(Vec(RenameWidth, Bool()))
    val msrCandidateAmbiguousReject = Wire(Vec(RenameWidth, Bool()))

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
      val discoveryStreamUnique = Wire(Vec(MsrStreamCount, Bool()))
      val discoveryStreamAmbiguous = Wire(Vec(MsrStreamCount, Bool()))
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
        discoveryStreamUnique(stream) := PopCount(discoveryEntryMatch(stream)) === 1.U
        discoveryStreamAmbiguous(stream) := PopCount(discoveryEntryMatch(stream)) > 1.U
        contextReject(stream) := VecInit(rawTagMatch.zip(contextMatch).map {
          case (tagMatch, predecessorMatch) => tagMatch && !predecessorMatch
        }).asUInt.orR
      }
      val otherStream = ~msrNewestStream
      val discoveryStreamId = Mux(discoveryStreamUnique(msrNewestStream), msrNewestStream, otherStream)
      val discoveryValid = req.valid && newBlock && !continuationValid && msrStreamAdmission &&
        (discoveryStreamUnique(msrNewestStream) || discoveryStreamUnique(otherStream))
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
        instructionOffset < msrStreamLength(selectedStreamId) && msrStreamAdmission &&
        !msrMispredSample && !io.redirect.valid
      val selectedLogIndex = Cat(
        selectedStreamId,
        instructionOffset(log2Ceil(MsrEntriesPerStream) - 1, 0)
      )
      val selectedLogValid = responseValid &&
        selectedGeneration === msrStreamGeneration(selectedStreamId) &&
        msrLogValid(selectedLogIndex) && !msrLogConsumed(selectedLogIndex)

      io.msrCandidate.resp(lane).valid := responseValid
      io.msrCandidate.resp(lane).streamId := selectedStreamId
      io.msrCandidate.resp(lane).streamGeneration := selectedGeneration
      io.msrCandidate.resp(lane).instructionOffset := instructionOffset
      io.msrCandidate.reuseInfo(lane).valid := selectedLogValid
      io.msrCandidate.reuseInfo(lane).pc := msrLogPc(selectedLogIndex)
      io.msrCandidate.reuseInfo(lane).instr := msrLogInstr(selectedLogIndex)
      io.msrCandidate.reuseInfo(lane).completed := msrLogCompleted(selectedLogIndex)
      io.msrCandidate.reuseInfo(lane).reusableAlu := msrLogReusableAlu(selectedLogIndex)
      io.msrCandidate.reuseInfo(lane).srcUsed := msrLogSrcUsed(selectedLogIndex)
      io.msrCandidate.reuseInfo(lane).srcRgid := msrLogSrcRgid(selectedLogIndex)
      io.msrCandidate.reuseInfo(lane).destRgid := msrLogDestRgid(selectedLogIndex)
      io.msrCandidate.reuseInfo(lane).pdest := msrLogPdest(selectedLogIndex)
      io.msrCandidate.reuseInfo(lane).held := msrLogPdestRetained(selectedLogIndex)
      msrCandidateDiscovery(lane) := req.fire && responseValid && discoveryValid
      msrCandidateLayoutMismatch(lane) := req.fire && selectedValid && !instructionStartPresent
      msrCandidateContextReject(lane) := req.fire && newBlock && !continuationValid && contextReject.asUInt.orR
      msrCandidateAmbiguousPosition(lane) := req.fire && newBlock && !continuationValid &&
        discoveryStreamAmbiguous.asUInt.orR
      msrCandidateAmbiguousReject(lane) := req.fire && newBlock && !continuationValid &&
        discoveryStreamMatch.asUInt.orR && !discoveryStreamUnique.asUInt.orR

      when(req.fire && discoveryValid) {
        assert(PopCount(discoveryEntryMatch(discoveryStreamId)) === 1.U,
          "MSR discovery accepted an ambiguous WPB position")
      }

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
    val msrCandidateRgidMismatch = Wire(Vec(RenameWidth, Bool()))
    val msrCandidateClassReject = Wire(Vec(RenameWidth, Bool()))
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
      msrCandidateRgidMismatch(i) := reusableAluMatch && !allSrcRgidMatch && !msrCandidateNullRgidReject(i)
      msrCandidateClassReject(i) := completedStaticMatch &&
        !(msrLogReusableAlu(entryIndex) && msrEnqReusableAlu(i))
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

    val msrClaimEntryOH = Wire(Vec(RenameWidth, UInt(MsrTotalEntries.W)))
    for (lane <- 0 until RenameWidth) {
      val claim = io.msrCandidate.claim(lane)
      val candidate = claim.candidate
      val entryIndex = Cat(
        candidate.streamId,
        candidate.instructionOffset(log2Ceil(MsrEntriesPerStream) - 1, 0)
      )
      val claimCurrent = candidate.valid &&
        msrInstEnqValid(lane) && io.enq.req(lane).bits.msrReused &&
        candidate.instructionOffset < MsrEntriesPerStream.U &&
        candidate.instructionOffset < msrStreamLength(candidate.streamId) &&
        msrStreamValid(candidate.streamId) &&
        msrStreamGeneration(candidate.streamId) === candidate.streamGeneration &&
        msrLogValid(entryIndex) && !msrLogConsumed(entryIndex) &&
        msrLogPdestRetained(entryIndex) && msrLogPdest(entryIndex) === claim.pdest &&
        msrLogMatchFullReuse(lane)

      msrClaimEntryOH(lane) := Mux(claim.valid && claimCurrent, UIntToOH(entryIndex, MsrTotalEntries), 0.U)
      when(claim.valid) {
        assert(claimCurrent, "rename attempted to claim a stale or unheld Squash Log entry")
      }
    }
    for (lane <- 0 until RenameWidth; prior <- 0 until lane) {
      assert(!(msrClaimEntryOH(lane) & msrClaimEntryOH(prior)).orR,
        "two rename lanes claimed the same Squash Log entry")
    }
    val msrClaimEntryMask = msrClaimEntryOH.reduce(_ | _)
    when(io.redirect.valid) {
      assert(!io.msrCandidate.claim.map(_.valid).reduce(_ || _),
        "MSR claim must be suppressed while redirect is active")
    }
    val msrClaimStreamHit = VecInit((0 until MsrStreamCount).map { stream =>
      msrClaimEntryMask((stream + 1) * MsrEntriesPerStream - 1, stream * MsrEntriesPerStream).orR
    })

    for (robIdx <- 0 until RobSize) {
      val enqToRob = VecInit((0 until RenameWidth).map { lane =>
        msrInstEnqValid(lane) && enqRobIdxSeq(lane) === robIdx.U
      })
      val enqCountToRob = (0 until RenameWidth).map { lane =>
        Mux(enqToRob(lane), msrInstEnqCount(lane), 0.U)
      }.reduce(_ +& _)
      val oldCount = Mux(robEntries(robIdx).valid, msrRobInstCount(robIdx), 0.U)
      when(enqToRob.asUInt.orR) {
        val newCount = oldCount +& enqCountToRob
        assert(newCount <= MsrInstSlotsPerRob.U,
          "MSR ROB instruction metadata overflowed its per-entry slots")
        msrRobInstCount(robIdx) := newCount
      }
    }
    for (lane <- 0 until RenameWidth) {
      when(msrInstEnqValid(lane)) {
        val robIdx = enqRobIdxSeq(lane)
        val oldCount = Mux(robEntries(robIdx).valid, msrRobInstCount(robIdx), 0.U)
        val priorInSameRob = if (lane == 0) 0.U else (0 until lane).map { prior =>
          Mux(msrInstEnqValid(prior) && enqRobIdxSeq(prior) === robIdx, msrInstEnqCount(prior), 0.U)
        }.reduce(_ +& _)
        val instSlot = oldCount +& priorInSameRob
        assert(instSlot < MsrInstSlotsPerRob.U,
          "MSR instruction metadata selected an invalid ROB slot")
        for (src <- 0 until backendParams.numSrc) {
          msrRobSrcUsed(robIdx)(instSlot)(src) := SrcType.isXp(io.enq.req(lane).bits.srcType(src)) &&
            io.enq.req(lane).bits.psrc(src) =/= 0.U
          msrRobSrcRgid(robIdx)(instSlot)(src) := io.enq.req(lane).bits.srcRgid(src)
        }
        msrRobDestRgid(robIdx)(instSlot) := io.enq.req(lane).bits.destRgid
        msrRobPdest(robIdx)(instSlot) := io.enq.req(lane).bits.pdest
        msrRobAlu(robIdx)(instSlot) := FuType.isAlu(io.enq.req(lane).bits.fuType)
        msrRobReusableAlu(robIdx)(instSlot) := msrEnqReusableAlu(lane)
        msrRobOwnsIntPReg(robIdx)(instSlot) := msrEnqOwnsIntPReg(lane)
        msrRobReused(robIdx)(instSlot) := io.enq.req(lane).bits.msrReused
        msrRobPc(robIdx)(instSlot) := io.enq.req(lane).bits.msrPc
        msrRobInstr(robIdx)(instSlot) := io.enq.req(lane).bits.msrInstr
        msrRobFtqPtr(robIdx)(instSlot) := io.enq.req(lane).bits.ftqPtr
        msrRobFtqOffset(robIdx)(instSlot) := io.enq.req(lane).bits.ftqOffset
        when(io.enq.req(lane).bits.msrFusionSecondValid) {
          val fusionSlot = instSlot +& 1.U
          assert(fusionSlot < MsrInstSlotsPerRob.U,
            "MSR fused instruction metadata selected an invalid ROB slot")
          for (src <- 0 until backendParams.numSrc) {
            msrRobSrcUsed(robIdx)(fusionSlot)(src) := false.B
            msrRobSrcRgid(robIdx)(fusionSlot)(src) := MsrRgid.Null.U
          }
          msrRobDestRgid(robIdx)(fusionSlot) := MsrRgid.Null.U
          msrRobPdest(robIdx)(fusionSlot) := 0.U
          msrRobAlu(robIdx)(fusionSlot) := io.enq.req(lane).bits.msrFusionSecondAlu
          msrRobReusableAlu(robIdx)(fusionSlot) := false.B
          msrRobOwnsIntPReg(robIdx)(fusionSlot) := false.B
          msrRobReused(robIdx)(fusionSlot) := false.B
          msrRobPc(robIdx)(fusionSlot) := io.enq.req(lane).bits.msrFusionSecondPc
          msrRobInstr(robIdx)(fusionSlot) := io.enq.req(lane).bits.msrFusionSecondInstr
          msrRobFtqPtr(robIdx)(fusionSlot) := io.enq.req(lane).bits.msrFusionSecondFtqPtr
          msrRobFtqOffset(robIdx)(fusionSlot) := io.enq.req(lane).bits.msrFusionSecondFtqOffset
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
        .reduce(_ || _) || msrClaimStreamHit(stream)
    })
    val msrStreamCompletedStaticHit = VecInit((0 until MsrStreamCount).map { stream =>
      msrLogMatchOH.zip(msrLogMatchCompleted).map { case (matched, completed) =>
        matched((stream + 1) * MsrEntriesPerStream - 1, stream * MsrEntriesPerStream).orR && completed
      }.reduce(_ || _) || msrClaimStreamHit(stream)
    })
    val msrStreamFullReuseHit = VecInit((0 until MsrStreamCount).map { stream =>
      msrLogMatchOH.zip(msrLogMatchFullReuse).map { case (matched, fullReuse) =>
        matched((stream + 1) * MsrEntriesPerStream - 1, stream * MsrEntriesPerStream).orR && fullReuse
      }.reduce(_ || _) || msrClaimStreamHit(stream)
    })
    val msrStreamSemanticHoldHit = VecInit((0 until MsrStreamCount).map { stream =>
      msrLogMatchOH.zip(msrLogMatchRgid).map { case (matched, semanticHold) =>
        matched((stream + 1) * MsrEntriesPerStream - 1, stream * MsrEntriesPerStream).orR && semanticHold
      }.reduce(_ || _) || msrClaimStreamHit(stream)
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

    val msrRetainedPRegOH = VecInit((0 until MsrTotalEntries).map { entry =>
      Mux(msrLogPdestRetained(entry), UIntToOH(msrLogPdest(entry), IntPhyRegs), 0.U(IntPhyRegs.W))
    })
    val msrRetainedPRegMask = msrRetainedPRegOH.reduce(_ | _)
    val msrMatchedEntryMask = msrLogMatchOH.reduce(_ | _) & ~msrClaimEntryMask
    val msrMatchedPRegReleaseMask = VecInit((0 until MsrTotalEntries).map { entry =>
      Mux(
        msrMatchedEntryMask(entry) && msrLogPdestRetained(entry),
        UIntToOH(msrLogPdest(entry), IntPhyRegs),
        0.U(IntPhyRegs.W)
      )
    }).reduce(_ | _)
    val msrReplacedStreamReleaseMask = VecInit((0 until MsrTotalEntries).map { entry =>
      val entryStream = entry / MsrEntriesPerStream
      Mux(
        msrCreateStream && msrNextStream === entryStream.U && msrLogPdestRetained(entry),
        UIntToOH(msrLogPdest(entry), IntPhyRegs),
        0.U(IntPhyRegs.W)
      )
    }).reduce(_ | _)
    val msrReleaseAllHeld = io.msrRgid.overflow || msrCandidateStaticDivergence
    val msrClaimPRegMask = VecInit(io.msrCandidate.claim.map { claim =>
      Mux(claim.valid, UIntToOH(claim.pdest, IntPhyRegs), 0.U(IntPhyRegs.W))
    }).reduce(_ | _)
    val msrPRegReleaseMask = (msrMatchedPRegReleaseMask |
      Mux(msrReleaseAllHeld, msrRetainedPRegMask, msrReplacedStreamReleaseMask)) & ~msrClaimPRegMask
    val msrHeldAfterReleaseMask = io.msrPReg.held & ~msrPRegReleaseMask
    val msrHeldAfterReleaseCount = PopCount(msrHeldAfterReleaseMask)
    val msrHeldLimitCapacity = MsrConfig.MaxHeldPRegs.U - msrHeldAfterReleaseCount
    val msrSquashedIntPRegCount = PopCount((0 until RobSize).flatMap { robIdx =>
      (0 until MsrInstSlotsPerRob).map { instSlot =>
        redirectNeedFlush(robIdx) && instSlot.U < msrRobInstCount(robIdx) &&
          msrRobOwnsIntPReg(robIdx)(instSlot)
      }
    })
    val msrRecoveredFreeCount = io.msrPReg.freeCount +& PopCount(msrPRegReleaseMask) +&
      msrSquashedIntPRegCount
    val msrFreeListHoldCapacity = Mux(
      msrRecoveredFreeCount > RenameWidth.U,
      msrRecoveredFreeCount - RenameWidth.U,
      0.U
    )
    val msrHoldCapacity = Mux(
      msrHeldLimitCapacity < msrFreeListHoldCapacity,
      msrHeldLimitCapacity,
      msrFreeListHoldCapacity
    )
    val msrCaptureHoldEligible = VecInit((0 until MsrEntriesPerStream).map { entry =>
      val robIdx = msrCaptureRobIdx(entry)
      val instSlot = msrCaptureInstSlot(entry)
      msrCreateStream && msrCaptureTrackedValid(entry) && msrRobCompletesThisCycle(robIdx) &&
        msrRobReusableAlu(robIdx)(instSlot) && msrRobPdest(robIdx)(instSlot) =/= 0.U
    })
    val msrCaptureHoldAdmit = VecInit((0 until MsrEntriesPerStream).map { entry =>
      val priorEligible = if (entry == 0) 0.U else PopCount(msrCaptureHoldEligible.take(entry))
      msrCaptureHoldEligible(entry) && priorEligible < msrHoldCapacity
    })
    val msrPRegHoldMask = VecInit((0 until MsrEntriesPerStream).map { entry =>
      Mux(
        msrCaptureHoldAdmit(entry),
        UIntToOH(msrRobPdest(msrCaptureRobIdx(entry))(msrCaptureInstSlot(entry)), IntPhyRegs),
        0.U(IntPhyRegs.W)
      )
    }).reduce(_ | _)

    io.msrPReg.hold := msrPRegHoldMask
    io.msrPReg.release := msrPRegReleaseMask

    assert(PopCount(io.msrPReg.held) <= MsrConfig.MaxHeldPRegs.U,
      "MSR held-PReg occupancy exceeded its admission limit")
    assert(PopCount(msrRetainedPRegMask) === PopCount(msrLogPdestRetained),
      "two Squash Log entries own the same held PReg")
    assert(msrRetainedPRegMask === io.msrPReg.held,
      "Squash Log and integer freelist disagree on held-PReg ownership")
    assert((msrPRegHoldMask & msrHeldAfterReleaseMask).asUInt === 0.U,
      "MSR attempted to hold a PReg already owned by another Squash Log entry")
    assert(PopCount(msrPRegHoldMask) === PopCount(msrCaptureHoldAdmit),
      "two newly captured Squash Log entries attempted to hold the same PReg")
    when(msrCreateStream) {
      assert(msrRecoveredFreeCount >= PopCount(msrCaptureHoldAdmit) + RenameWidth.U,
        "MSR hold admission violated the integer freelist low watermark")
    }
    assert((msrPRegReleaseMask & msrClaimPRegMask).asUInt === 0.U,
      "MSR attempted to release and claim the same PReg in one cycle")

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
            val instSlot = msrCaptureInstSlot(entry)
            val captureValid = msrCaptureTrackedValid(entry)
            msrLogValid(logIdx) := captureValid
            msrLogConsumed(logIdx) := false.B
            msrLogPdestRetained(logIdx) := captureValid && msrCaptureHoldAdmit(entry)
            when(captureValid) {
              msrLogCompleted(logIdx) := msrRobCompletesThisCycle(robIdx)
              msrLogAlu(logIdx) := msrRobAlu(robIdx)(instSlot)
              msrLogPc(logIdx) := msrRobPc(robIdx)(instSlot)
              msrLogInstr(logIdx) := msrRobInstr(robIdx)(instSlot)
              for (src <- 0 until backendParams.numSrc) {
                msrLogSrcUsed(logIdx)(src) := msrRobSrcUsed(robIdx)(instSlot)(src)
                msrLogSrcRgid(logIdx)(src) := msrRobSrcRgid(robIdx)(instSlot)(src)
              }
              msrLogDestRgid(logIdx) := msrRobDestRgid(robIdx)(instSlot)
              msrLogPdest(logIdx) := msrRobPdest(robIdx)(instSlot)
              msrLogReusableAlu(logIdx) := msrRobReusableAlu(robIdx)(instSlot)
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
              val instSlot = msrCaptureInstSlot(entry)
              Mux(
                blockSelect(entry),
                UIntToOH(msrRobFtqOffset(robIdx)(instSlot), MsrBlockPositionCount),
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
        when(msrLogMatchOH.map(_(i)).reduce(_ || _) || msrClaimEntryMask(i)) {
          msrLogConsumed(i) := true.B
          msrLogPdestRetained(i) := false.B
        }
      }
      when(msrCandidateStaticDivergence) {
        msrLogPdestRetained.foreach(_ := false.B)
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

    for (entry <- 0 until MsrTotalEntries) {
      val pdestReallocated = VecInit((0 until RenameWidth).map { lane =>
        msrEnqAllocInt(lane) && io.enq.req(lane).bits.pdest === msrLogPdest(entry)
      }).asUInt.orR
      when(msrLogPdestRetained(entry)) {
        assert(!pdestReallocated, "integer freelist reallocated a Squash-Log-held PReg")
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

    val msrCompletedSquashedInst = (0 until RobSize).map { robIdx =>
      Mux(
        redirectNeedFlush(robIdx) && msrRobCompletesThisCycle(robIdx),
        msrRobInstCount(robIdx),
        0.U
      )
    }.reduce(_ +& _)
    val msrCompletedSquashedAluInst = PopCount((0 until RobSize).flatMap { robIdx =>
      (0 until MsrInstSlotsPerRob).map { instSlot =>
        redirectNeedFlush(robIdx) && msrRobCompletesThisCycle(robIdx) &&
          instSlot.U < msrRobInstCount(robIdx) && msrRobAlu(robIdx)(instSlot)
      }
    })
    val msrReusedThenSquashedInst = PopCount((0 until RobSize).flatMap { robIdx =>
      (0 until MsrInstSlotsPerRob).map { instSlot =>
        redirectNeedFlush(robIdx) && instSlot.U < msrRobInstCount(robIdx) &&
          msrRobReused(robIdx)(instSlot)
      }
    })
    val msrReusedCommittedInst = PopCount(io.commits.commitValid.zip(deqPtrVec).map {
      case (commitValid, deqPtr) =>
        io.commits.isCommit && commitValid && VecInit((0 until MsrInstSlotsPerRob).map { instSlot =>
          instSlot.U < msrRobInstCount(deqPtr.value) && msrRobReused(deqPtr.value)(instSlot)
        }).asUInt.orR
    })

    XSPerfAccumulate("msr_mispred_with_squashed_inst", msrMispredSample && msrSquashedInst.orR)
    XSPerfAccumulate("msr_mispred_with_completed_inst", msrMispredSample && msrCompletedSquashedInst.orR)
    XSPerfAccumulate("msr_mispred_with_completed_alu_inst", msrMispredSample && msrCompletedSquashedAluInst.orR)
    XSPerfAccumulate("msr_squashed_rob_entry", Mux(msrMispredSample, msrSquashedRobEntries, 0.U))
    XSPerfAccumulate("msr_squashed_inst", Mux(msrMispredSample, msrSquashedInst, 0.U))
    XSPerfAccumulate("msr_completed_squashed_inst", Mux(msrMispredSample, msrCompletedSquashedInst, 0.U))
    XSPerfAccumulate("msr_completed_squashed_alu_inst", Mux(msrMispredSample, msrCompletedSquashedAluInst, 0.U))
    XSPerfAccumulate("msr_reused_then_squashed_inst", Mux(redirectValidReg, msrReusedThenSquashedInst, 0.U))
    XSPerfAccumulate("msr_reused_committed_inst", msrReusedCommittedInst)
    XSPerfAccumulate("msr_redirect_writeback_completed_inst", PopCount((0 until RobSize).map { robIdx =>
      redirectNeedFlush(robIdx) && !robEntries(robIdx).isWritebacked && msrRobCompletesThisCycle(robIdx)
    }))
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
    XSPerfAccumulate("msr_entry_hold_attempt", PopCount(msrCaptureHoldEligible))
    XSPerfAccumulate("msr_entry_held", PopCount(msrCaptureHoldAdmit))
    XSPerfAccumulate(
      "msr_entry_hold_reject_pressure",
      PopCount(msrCaptureHoldEligible) - PopCount(msrCaptureHoldAdmit)
    )
    XSPerfAccumulate(
      "msr_entry_hold_reject_low_watermark",
      Mux(
        PopCount(msrCaptureHoldEligible) > msrFreeListHoldCapacity,
        PopCount(msrCaptureHoldEligible) - msrFreeListHoldCapacity,
        0.U
      )
    )
    XSPerfAccumulate("msr_entry_released", PopCount(msrPRegReleaseMask))
    XSPerfAccumulate("msr_entry_released_on_match", PopCount(msrMatchedPRegReleaseMask))
    XSPerfAccumulate("msr_entry_evicted", PopCount(msrReplacedStreamReleaseMask & ~msrClaimPRegMask))
    XSPerfAccumulate(
      "msr_entry_released_on_divergence",
      Mux(msrCandidateStaticDivergence, PopCount(msrPRegReleaseMask), 0.U)
    )
    XSPerfAccumulate(
      "msr_entry_released_on_rgid_overflow",
      Mux(io.msrRgid.overflow, PopCount(msrPRegReleaseMask), 0.U)
    )
    XSPerfAccumulate("msr_entry_claimed", PopCount(msrClaimEntryMask))
    XSPerfAccumulate("msr_wpb_block_captured", Mux(msrCreateStream, msrCaptureWpbLength, 0.U))
    XSPerfAccumulate("msr_candidate_acquired", PopCount(msrCandidateDiscovery))
    XSPerfAccumulate("msr_candidate_layout_mismatch", PopCount(msrCandidateLayoutMismatch))
    XSPerfAccumulate("msr_candidate_context_reject", PopCount(msrCandidateContextReject))
    XSPerfAccumulate("msr_candidate_ambiguous_position", PopCount(msrCandidateAmbiguousPosition))
    XSPerfAccumulate("msr_candidate_ambiguous_reject", PopCount(msrCandidateAmbiguousReject))
    XSPerfAccumulate("msr_candidate_provenance_inst", PopCount(io.msrCandidate.req.zip(io.msrCandidate.resp).map {
      case (req, resp) => req.fire && resp.valid
    }))
    XSPerfAccumulate("msr_position_candidate_inst", PopCount(msrCandidateTupleValid))
    XSPerfAccumulate("msr_position_static_divergence", PopCount(
      msrCandidateTupleValid.zip(msrCandidateStaticGuard).map { case (valid, guard) => valid && !guard }
    ))
    XSPerfAccumulate("msr_rgid_null_reject", PopCount(msrCandidateNullRgidReject))
    XSPerfAccumulate("msr_rgid_mismatch_reject", PopCount(msrCandidateRgidMismatch))
    XSPerfAccumulate("msr_instruction_class_reject", PopCount(msrCandidateClassReject))
    XSPerfAccumulate("msr_stale_stream_generation", msrCandidateValid && !msrCandidateStateCurrent)
    XSPerfAccumulate("msr_rgid_drain_cycle", msrRgidResetPending)
    XSPerfAccumulate("msr_rgid_global_reset", msrRgidReset)
  }
}
