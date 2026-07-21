/***************************************************************************************
 * Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
 * Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
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

package xiangshan.backend

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.datapath.DataConfig.IntData

class IntSparseUCAIO(implicit p: org.chipsalliance.cde.config.Parameters) extends XSBundle {
  val redirectKill = Input(Bool())

  val rename = new Bundle {
    val source = Input(Vec(RenameWidth, Vec(IntERLogicalSrcWidth, new IntERRenameSourceProbe)))
    val sourceFallback = Input(Vec(RenameWidth, Bool()))
    val sourceFallbackReason = Input(Vec(RenameWidth, UInt(IntERFallbackReason.width.W)))
    val alloc = Input(Vec(RenameWidth, ValidIO(new IntERProducerAlloc)))
    val redef = Input(Vec(RenameWidth, ValidIO(new IntERRedefProbe)))
    val redefFallback = Input(Vec(RenameWidth, Bool()))
    val redefFallbackReason = Input(Vec(RenameWidth, UInt(IntERFallbackReason.width.W)))

    val srcMatch = Output(Vec(RenameWidth, Vec(IntERLogicalSrcWidth, new IntERSrcTrack)))
    val destTrack = Output(Vec(RenameWidth, new IntERDestTrack))
    val redefTrack = Output(Vec(RenameWidth, new IntERRedefTrack))
    val fallbackMark = Output(Vec(RenameWidth, Bool()))
  }

  val producerReady = Input(Vec(backendParams.numPregWb(IntData()), ValidIO(new IntERProducerReady)))
  val readDone = Input(Vec(IntERReadDoneWidth, ValidIO(new IntERSrcValueReadDone)))
  val squash = Input(Vec(IntERReadDoneWidth, ValidIO(new IntERSquashSource)))
  val stGuardDec = Input(Vec(IntERSTWalkWidth, ValidIO(new IntERSTGuardDec)))

  val commitOldPdest = Input(Vec(RabCommitWidth, UInt(PhyRegIdxWidth.W)))
  val commitNeedFree = Input(Vec(RabCommitWidth, Bool()))
  val commitRedef = Input(Vec(RabCommitWidth, ValidIO(new IntERCommitRedef)))
  val commitSuppress = Output(Vec(RabCommitWidth, new IntERCommitSuppress))

  val earlyFree = Output(Vec(IntEREarlyFreeWidth, ValidIO(new IntEREarlyFreeReq)))
  val debug = Output(new IntERDebugBundle)
}

class IntSparseUCAEntry(implicit p: org.chipsalliance.cde.config.Parameters) extends XSBundle {
  val state = UInt(IntEREntryState.width.W)
  val pdest = UInt(PhyRegIdxWidth.W)
  val producerRobIdx = new xiangshan.backend.rob.RobPtr
  val redefinerRobIdx = new xiangshan.backend.rob.RobPtr
  val userCounter = UInt(IntERCounterWidth.W)
  val gen = UInt(IntERTrackGenBits.W)
  val fallback = Bool()
  val redefinerSeen = Bool()
  val redefinerNS = Bool()
  val producedReady = Bool()
  val earlyFreeIssued = Bool()
  val releasedReused = Bool()
}

class IntSparseUCA(implicit p: org.chipsalliance.cde.config.Parameters) extends XSModule {
  val io = IO(new IntSparseUCAIO)

  private val entryCount = IntERTrackEntries
  private val entryIdxWidth = IntERTrackIdWidth
  private val updateCountWidth = (log2Ceil(RenameWidth * IntERLogicalSrcWidth + IntERReadDoneWidth * IntERLogicalSrcWidth + IntERSTWalkWidth + 2) max (IntERCounterWidth + 1))
  private val maxCounter = ((BigInt(1) << IntERCounterWidth) - 1).U(IntERCounterWidth.W)

  private val entries = RegInit(VecInit(Seq.fill(entryCount)(0.U.asTypeOf(new IntSparseUCAEntry))))

  private def zeroSrcTrack: IntERSrcTrack = 0.U.asTypeOf(new IntERSrcTrack)
  private def zeroDestTrack: IntERDestTrack = 0.U.asTypeOf(new IntERDestTrack)
  private def zeroRedefTrack: IntERRedefTrack = 0.U.asTypeOf(new IntERRedefTrack)
  private def isCounting(entry: IntSparseUCAEntry): Bool = entry.state === IntEREntryState.counting
  private def isFallbackWait(entry: IntSparseUCAEntry): Bool = entry.state === IntEREntryState.fallbackWaitCommit
  private def isReleased(entry: IntSparseUCAEntry): Bool = entry.state === IntEREntryState.releasedWaitCommit
  private def isActive(entry: IntSparseUCAEntry): Bool = entry.state =/= IntEREntryState.invalid
  private def sameRobPtr(a: xiangshan.backend.rob.RobPtr, b: xiangshan.backend.rob.RobPtr): Bool = a.asUInt === b.asUInt
  private def entryAt(vec: Vec[IntSparseUCAEntry], idx: UInt): IntSparseUCAEntry = {
    if (entryCount == 1) vec(0) else vec(idx)
  }

  io.rename.srcMatch.foreach(_.foreach(_ := zeroSrcTrack))
  io.rename.destTrack.foreach(_ := zeroDestTrack)
  io.rename.redefTrack.foreach(_ := zeroRedefTrack)
  io.rename.fallbackMark := VecInit(Seq.fill(RenameWidth)(false.B))
  io.commitSuppress.foreach(_ := 0.U.asTypeOf(new IntERCommitSuppress))
  io.earlyFree.foreach(_ := 0.U.asTypeOf(ValidIO(new IntEREarlyFreeReq)))

  private val freeMask = Wire(Vec(RenameWidth + 1, UInt(entryCount.W)))
  private val allocIdx = Wire(Vec(RenameWidth, UInt(entryIdxWidth.W)))
  private val allocFire = Wire(Vec(RenameWidth, Bool()))
  freeMask(0) := VecInit(entries.map(_.state === IntEREntryState.invalid)).asUInt
  for (i <- 0 until RenameWidth) {
    allocFire(i) := io.rename.alloc(i).valid && !io.redirectKill && freeMask(i).orR
    allocIdx(i) := PriorityEncoder(freeMask(i))(entryIdxWidth - 1, 0)
    freeMask(i + 1) := Mux(allocFire(i), freeMask(i) & ~UIntToOH(allocIdx(i), entryCount).asUInt, freeMask(i))

    io.rename.destTrack(i).valid := allocFire(i)
    io.rename.destTrack(i).trackId := allocIdx(i)
    io.rename.destTrack(i).trackGen := entryAt(entries, allocIdx(i)).gen + 1.U
    io.rename.destTrack(i).pdest := io.rename.alloc(i).bits.pdest
  }

  private val sourceMatchOH = Wire(Vec(RenameWidth, Vec(IntERLogicalSrcWidth, UInt(entryCount.W))))
  private val sourceMatchGen = Wire(Vec(RenameWidth, Vec(IntERLogicalSrcWidth, Vec(entryCount, UInt(IntERTrackGenBits.W)))))
  private val sourceHasMatch = Wire(Vec(RenameWidth, Vec(IntERLogicalSrcWidth, Bool())))
  private val sourceDuplicate = Wire(Vec(RenameWidth, Vec(IntERLogicalSrcWidth, Bool())))
  private val sourceCounted = Wire(Vec(RenameWidth, Vec(IntERLogicalSrcWidth, Bool())))
  private val sourceFallbackHit = Wire(Vec(RenameWidth, Vec(IntERLogicalSrcWidth, Bool())))
  private val sourceFallbackMark = Wire(Vec(RenameWidth, Bool()))

  for (i <- 0 until RenameWidth) {
    for (s <- 0 until IntERLogicalSrcWidth) {
      val probe = io.rename.source(i)(s)
      val matches = VecInit((0 until entryCount).map { e =>
        val existingHit = isCounting(entries(e)) && !entries(e).fallback && entries(e).pdest === probe.psrc
        val sameCycleHits = (0 until i).map { j =>
          IntERAllowSameCycleRenameBypassMatch.B &&
            allocFire(j) && allocIdx(j) === e.U && io.rename.alloc(j).bits.pdest === probe.psrc
        }
        val sameCycleHit = sameCycleHits.foldLeft(false.B)(_ || _)
        sourceMatchGen(i)(s)(e) := Mux(sameCycleHit, entries(e).gen + 1.U, entries(e).gen)
        probe.valid && (existingHit || sameCycleHit)
      })
      sourceMatchOH(i)(s) := VecInit(PriorityEncoderOH(matches)).asUInt
      sourceHasMatch(i)(s) := matches.asUInt.orR
      sourceDuplicate(i)(s) := (0 until s).map { older =>
        sourceHasMatch(i)(older) && sourceHasMatch(i)(s) &&
          (sourceMatchOH(i)(older) & sourceMatchOH(i)(s)).orR
      }.foldLeft(false.B)(_ || _)
      sourceFallbackHit(i)(s) := io.rename.sourceFallback(i) && sourceHasMatch(i)(s)
      sourceCounted(i)(s) := sourceHasMatch(i)(s) && !sourceDuplicate(i)(s) && !sourceFallbackHit(i)(s)

      val matchIdx = OHToUInt(sourceMatchOH(i)(s))
      val matchGen = Mux1H(sourceMatchOH(i)(s), sourceMatchGen(i)(s))

      io.rename.srcMatch(i)(s).valid := sourceCounted(i)(s)
      io.rename.srcMatch(i)(s).trackId := matchIdx(entryIdxWidth - 1, 0)
      io.rename.srcMatch(i)(s).trackGen := matchGen
      io.rename.srcMatch(i)(s).srcIdx := probe.srcIdx
      io.rename.srcMatch(i)(s).psrc := probe.psrc
    }
    sourceFallbackMark(i) := sourceFallbackHit(i).asUInt.orR
  }

  private val redefMatchOH = Wire(Vec(RenameWidth, UInt(entryCount.W)))
  private val redefMatchGen = Wire(Vec(RenameWidth, Vec(entryCount, UInt(IntERTrackGenBits.W))))
  private val redefFire = Wire(Vec(RenameWidth, Bool()))
  private val redefFallbackHit = Wire(Vec(RenameWidth, Bool()))
  for (i <- 0 until RenameWidth) {
    val probe = io.rename.redef(i)
    val matches = VecInit((0 until entryCount).map { e =>
      val existingHit = (isCounting(entries(e)) || isFallbackWait(entries(e))) &&
        entries(e).pdest === probe.bits.oldPdest
      val sameCycleHits = (0 until i).map { j =>
        IntERAllowSameCycleRenameBypassMatch.B &&
          allocFire(j) && allocIdx(j) === e.U && io.rename.alloc(j).bits.pdest === probe.bits.oldPdest
      }
      val sameCycleHit = sameCycleHits.foldLeft(false.B)(_ || _)
      redefMatchGen(i)(e) := Mux(sameCycleHit, entries(e).gen + 1.U, entries(e).gen)
      probe.valid && (existingHit || sameCycleHit)
    })
    redefMatchOH(i) := VecInit(PriorityEncoderOH(matches)).asUInt
    redefFire(i) := matches.asUInt.orR
    redefFallbackHit(i) := io.rename.redefFallback(i) && redefFire(i)

    val matchIdx = OHToUInt(redefMatchOH(i))
    io.rename.redefTrack(i).valid := redefFire(i)
    io.rename.redefTrack(i).trackId := matchIdx(entryIdxWidth - 1, 0)
    io.rename.redefTrack(i).trackGen := Mux1H(redefMatchOH(i), redefMatchGen(i))
    io.rename.redefTrack(i).oldPdest := probe.bits.oldPdest
    io.rename.fallbackMark(i) := sourceFallbackMark(i) || redefFallbackHit(i)

  }

  private val commitSuppressOH = Wire(Vec(RabCommitWidth, UInt(entryCount.W)))
  private val commitSuppressFire = Wire(Vec(RabCommitWidth, Bool()))
  private val commitClearOH = Wire(Vec(RabCommitWidth, UInt(entryCount.W)))
  private val commitIdentityMismatch = Wire(Vec(RabCommitWidth, Bool()))
  for (i <- 0 until RabCommitWidth) {
    val releasedMatches = VecInit((0 until entryCount).map { e =>
      io.commitNeedFree(i) && isReleased(entries(e)) &&
        io.commitRedef(i).valid &&
        io.commitRedef(i).bits.trackId === e.U &&
        io.commitRedef(i).bits.trackGen === entries(e).gen &&
        io.commitRedef(i).bits.oldPdest === entries(e).pdest &&
        io.commitOldPdest(i) === entries(e).pdest &&
        sameRobPtr(io.commitRedef(i).bits.redefinerRobIdx, entries(e).redefinerRobIdx)
    })
    val clearMatches = VecInit((0 until entryCount).map { e =>
      io.commitNeedFree(i) && isActive(entries(e)) &&
        entries(e).redefinerSeen &&
        io.commitRedef(i).valid &&
        io.commitRedef(i).bits.trackId === e.U &&
        io.commitRedef(i).bits.trackGen === entries(e).gen &&
        io.commitRedef(i).bits.oldPdest === entries(e).pdest &&
        io.commitOldPdest(i) === entries(e).pdest &&
        sameRobPtr(io.commitRedef(i).bits.redefinerRobIdx, entries(e).redefinerRobIdx)
    })
    val releasedTrackMatches = VecInit((0 until entryCount).map { e =>
      io.commitNeedFree(i) && isReleased(entries(e)) &&
        io.commitRedef(i).valid &&
        io.commitRedef(i).bits.trackId === e.U &&
        io.commitRedef(i).bits.trackGen === entries(e).gen
    })
    commitIdentityMismatch(i) := releasedTrackMatches.asUInt.orR && !releasedMatches.asUInt.orR
    assert(!commitIdentityMismatch(i), "IntSparseUCA released commit identity mismatch")

    commitSuppressOH(i) := VecInit(PriorityEncoderOH(releasedMatches)).asUInt
    commitSuppressFire(i) := releasedMatches.asUInt.orR
    commitClearOH(i) := VecInit(PriorityEncoderOH(clearMatches)).asUInt

    val suppressIdx = OHToUInt(commitSuppressOH(i))
    io.commitSuppress(i).suppress := commitSuppressFire(i)
    io.commitSuppress(i).oldPdest := io.commitOldPdest(i)
    io.commitSuppress(i).trackId := suppressIdx(entryIdxWidth - 1, 0)
    io.commitSuppress(i).trackGen := Mux1H(commitSuppressOH(i), entries.map(_.gen))
  }

  private val incByEntry = Wire(Vec(entryCount, UInt(updateCountWidth.W)))
  private val readDecByEntry = Wire(Vec(entryCount, UInt(updateCountWidth.W)))
  private val squashDecByEntry = Wire(Vec(entryCount, UInt(updateCountWidth.W)))
  private val guardDecByEntry = Wire(Vec(entryCount, UInt(updateCountWidth.W)))
  private val setFallbackByEntry = Wire(Vec(entryCount, Bool()))
  private val setProducedReadyByEntry = Wire(Vec(entryCount, Bool()))
  private val setRedefinerByEntry = Wire(Vec(entryCount, Bool()))
  private val setRedefinerNSByEntry = Wire(Vec(entryCount, Bool()))
  private val setSaturatedFallbackByEntry = Wire(Vec(entryCount, Bool()))
  private val genMismatchByEntry = Wire(Vec(entryCount, Bool()))
  private val commitClearByEntry = Wire(Vec(entryCount, Bool()))
  private val releasedReuseByEntry = Wire(Vec(entryCount, Bool()))
  private val firstFallbackByEntry = Wire(Vec(entryCount, Bool()))
  private val firstFallbackMoveByEntry = Wire(Vec(entryCount, Bool()))
  private val firstFallbackStoreDataByEntry = Wire(Vec(entryCount, Bool()))
  private val firstFallbackUnsupportedConsumerByEntry = Wire(Vec(entryCount, Bool()))
  private val firstFallbackUnsupportedReadPathByEntry = Wire(Vec(entryCount, Bool()))
  private val firstFallbackReplayProneByEntry = Wire(Vec(entryCount, Bool()))
  private val firstFallbackUncertainByEntry = Wire(Vec(entryCount, Bool()))
  private val firstFallbackSameCycleBypassByEntry = Wire(Vec(entryCount, Bool()))
  private val firstFallbackCounterSaturatedByEntry = Wire(Vec(entryCount, Bool()))
  private val firstFallbackStaleByEntry = Wire(Vec(entryCount, Bool()))
  private val firstFallbackDuplicateByEntry = Wire(Vec(entryCount, Bool()))
  private val firstFallbackMultipleByEntry = Wire(Vec(entryCount, Bool()))
  private val firstFallbackOtherByEntry = Wire(Vec(entryCount, Bool()))

  for (e <- 0 until entryCount) {
    val entry = entries(e)
    def matchesCurrentSource(trackId: UInt, trackGen: UInt, psrc: UInt): Bool =
      trackId === e.U && trackGen === entry.gen && psrc === entry.pdest
    def matchesCurrentPdest(trackId: UInt, psrc: UInt): Bool =
      trackId === e.U && psrc === entry.pdest

    val sourceIncHits = (0 until RenameWidth).flatMap(i =>
      (0 until IntERLogicalSrcWidth).map(s => sourceCounted(i)(s) && sourceMatchOH(i)(s)(e))
    )
    val sourceFallbackHits = (0 until RenameWidth).flatMap(i =>
      (0 until IntERLogicalSrcWidth).map(s => sourceFallbackHit(i)(s) && sourceMatchOH(i)(s)(e))
    )
    val sameCycleBypassFallbackHits = if (IntERAllowSameCycleRenameBypassMatch) {
      Seq(false.B)
    } else {
      (0 until RenameWidth).flatMap { producer =>
        ((producer + 1) until RenameWidth).flatMap { consumer =>
          (0 until IntERLogicalSrcWidth).map { s =>
            allocFire(producer) && allocIdx(producer) === e.U &&
              io.rename.source(consumer)(s).valid &&
              io.rename.source(consumer)(s).psrc === io.rename.alloc(producer).bits.pdest
          }
        }
      }
    }

    val readDecHits = io.readDone.flatMap { event =>
      event.bits.src.map { src =>
        event.valid && !event.bits.fallback && src.valid &&
          matchesCurrentSource(src.trackId, src.trackGen, src.psrc) && isCounting(entry)
      }
    }
    val readFallbackHits = io.readDone.flatMap { event =>
      event.bits.src.map { src =>
        event.valid && event.bits.fallback && src.valid &&
          matchesCurrentSource(src.trackId, src.trackGen, src.psrc) && isCounting(entry)
      }
    }
    val readGenMismatchHits = io.readDone.flatMap { event =>
      event.bits.src.map { src =>
        event.valid && src.valid && matchesCurrentPdest(src.trackId, src.psrc) &&
          src.trackGen =/= entry.gen && isActive(entry)
      }
    }

    val squashDecHits = io.squash.flatMap { event =>
      event.bits.src.map { src =>
        event.valid && src.valid &&
          matchesCurrentSource(src.trackId, src.trackGen, src.psrc) && isCounting(entry)
      }
    }
    val squashGenMismatchHits = io.squash.flatMap { event =>
      event.bits.src.map { src =>
        event.valid && src.valid && matchesCurrentPdest(src.trackId, src.psrc) &&
          src.trackGen =/= entry.gen && isActive(entry)
      }
    }

    val guardDecHits = io.stGuardDec.map { event =>
      event.valid && !event.bits.fallback &&
        event.bits.trackId === e.U && event.bits.trackGen === entries(e).gen && isCounting(entries(e)) &&
        entries(e).redefinerSeen &&
        event.bits.oldPdest === entries(e).pdest &&
        sameRobPtr(event.bits.robIdx, entries(e).redefinerRobIdx)
    }
    val guardFallbackHits = io.stGuardDec.map { event =>
      event.valid && event.bits.fallback &&
        event.bits.trackId === e.U && event.bits.trackGen === entries(e).gen && isCounting(entries(e)) &&
        entries(e).redefinerSeen &&
        event.bits.oldPdest === entries(e).pdest &&
        sameRobPtr(event.bits.robIdx, entries(e).redefinerRobIdx)
    }
    val guardGenMismatchHits = io.stGuardDec.map { event =>
      event.valid && event.bits.trackId === e.U && event.bits.trackGen =/= entries(e).gen && isActive(entries(e))
    }

    val producerReadyHits = io.producerReady.map { event =>
      event.valid && event.bits.pdest === entries(e).pdest &&
        event.bits.robIdx === entries(e).producerRobIdx &&
        (isCounting(entries(e)) || isFallbackWait(entries(e)))
    }
    val redefHits = (0 until RenameWidth).map(i => redefFire(i) && redefMatchOH(i)(e))
    val redefFallbackHits = (0 until RenameWidth).map(i => redefFallbackHit(i) && redefMatchOH(i)(e))
    val commitClearHits = (0 until RabCommitWidth).map(i => commitClearOH(i)(e))
    val releasedReuseHits = (0 until RenameWidth).map { i =>
      io.rename.alloc(i).valid && isReleased(entry) && io.rename.alloc(i).bits.pdest === entry.pdest
    }

    val sourceFallbackMoveHits = (0 until RenameWidth).flatMap(i =>
      (0 until IntERLogicalSrcWidth).map(s =>
        sourceFallbackHit(i)(s) && sourceMatchOH(i)(s)(e) &&
          io.rename.sourceFallbackReason(i) === IntERFallbackReason.moveConsumer
      )
    )
    val sourceFallbackStoreDataHits = (0 until RenameWidth).flatMap(i =>
      (0 until IntERLogicalSrcWidth).map(s =>
        sourceFallbackHit(i)(s) && sourceMatchOH(i)(s)(e) &&
          io.rename.sourceFallbackReason(i) === IntERFallbackReason.storeDataConsumer
      )
    )
    val sourceFallbackUnsupportedConsumerHits = (0 until RenameWidth).flatMap(i =>
      (0 until IntERLogicalSrcWidth).map(s =>
        sourceFallbackHit(i)(s) && sourceMatchOH(i)(s)(e) &&
          io.rename.sourceFallbackReason(i) === IntERFallbackReason.unsupportedConsumer
      )
    )
    val sourceFallbackMultipleHits = (0 until RenameWidth).flatMap(i =>
      (0 until IntERLogicalSrcWidth).map(s =>
        sourceFallbackHit(i)(s) && sourceMatchOH(i)(s)(e) &&
          io.rename.sourceFallbackReason(i) === IntERFallbackReason.multiple
      )
    )
    val sourceFallbackOtherHits = (0 until RenameWidth).flatMap(i =>
      (0 until IntERLogicalSrcWidth).map(s =>
        sourceFallbackHit(i)(s) && sourceMatchOH(i)(s)(e) &&
          io.rename.sourceFallbackReason(i) === IntERFallbackReason.other
      )
    )
    val redefFallbackUnsupportedConsumerHits = (0 until RenameWidth).map(i =>
      redefFallbackHit(i) && redefMatchOH(i)(e) &&
        io.rename.redefFallbackReason(i) === IntERFallbackReason.unsupportedConsumer
    )
    val redefFallbackMultipleHits = (0 until RenameWidth).map(i =>
      redefFallbackHit(i) && redefMatchOH(i)(e) &&
        io.rename.redefFallbackReason(i) === IntERFallbackReason.multiple
    )
    val redefFallbackOtherHits = (0 until RenameWidth).map(i =>
      redefFallbackHit(i) && redefMatchOH(i)(e) &&
        io.rename.redefFallbackReason(i) === IntERFallbackReason.other
    )
    val readFallbackUnsupportedPathHits = io.readDone.flatMap { event =>
      event.bits.src.map { src =>
        event.valid && event.bits.fallback && src.valid &&
          matchesCurrentSource(src.trackId, src.trackGen, src.psrc) && isCounting(entry) &&
          event.bits.reason === IntERFallbackReason.unsupportedReadPath
      }
    }
    val readFallbackReplayProneHits = io.readDone.flatMap { event =>
      event.bits.src.map { src =>
        event.valid && event.bits.fallback && src.valid &&
          matchesCurrentSource(src.trackId, src.trackGen, src.psrc) && isCounting(entry) &&
          event.bits.reason === IntERFallbackReason.replayProneReadPath
      }
    }
    val readFallbackUncertainHits = io.readDone.flatMap { event =>
      event.bits.src.map { src =>
        event.valid && event.bits.fallback && src.valid &&
          matchesCurrentSource(src.trackId, src.trackGen, src.psrc) && isCounting(entry) &&
          event.bits.reason === IntERFallbackReason.uncertainReadPath
      }
    }
    val readFallbackMultipleHits = io.readDone.flatMap { event =>
      event.bits.src.map { src =>
        event.valid && event.bits.fallback && src.valid &&
          matchesCurrentSource(src.trackId, src.trackGen, src.psrc) && isCounting(entry) &&
          event.bits.reason === IntERFallbackReason.multiple
      }
    }
    val readFallbackStaleHits = io.readDone.flatMap { event =>
      event.bits.src.map { src =>
        event.valid && event.bits.fallback && src.valid &&
          matchesCurrentSource(src.trackId, src.trackGen, src.psrc) && isCounting(entry) &&
          event.bits.reason === IntERFallbackReason.staleEvent
      }
    }
    val readFallbackOtherHits = io.readDone.flatMap { event =>
      event.bits.src.map { src =>
        event.valid && event.bits.fallback && src.valid &&
          matchesCurrentSource(src.trackId, src.trackGen, src.psrc) && isCounting(entry) &&
          event.bits.reason === IntERFallbackReason.other
      }
    }
    val guardFallbackReasonHits = io.stGuardDec.map { event =>
      event.valid && event.bits.fallback &&
        event.bits.trackId === e.U && event.bits.trackGen === entries(e).gen && isCounting(entries(e)) &&
        entries(e).redefinerSeen &&
        event.bits.oldPdest === entries(e).pdest &&
        sameRobPtr(event.bits.robIdx, entries(e).redefinerRobIdx)
    }
    val guardFallbackUnsupportedPathHits = guardFallbackReasonHits.zip(io.stGuardDec).map {
      case (hit, event) => hit && event.bits.reason === IntERFallbackReason.unsupportedReadPath
    }
    val guardFallbackReplayProneHits = guardFallbackReasonHits.zip(io.stGuardDec).map {
      case (hit, event) => hit && event.bits.reason === IntERFallbackReason.replayProneReadPath
    }
    val guardFallbackUncertainHits = guardFallbackReasonHits.zip(io.stGuardDec).map {
      case (hit, event) => hit && event.bits.reason === IntERFallbackReason.uncertainReadPath
    }
    val guardFallbackMultipleHits = guardFallbackReasonHits.zip(io.stGuardDec).map {
      case (hit, event) => hit && event.bits.reason === IntERFallbackReason.multiple
    }
    val guardFallbackStaleHits = guardFallbackReasonHits.zip(io.stGuardDec).map {
      case (hit, event) => hit && event.bits.reason === IntERFallbackReason.staleEvent
    }
    val guardFallbackOtherHits = guardFallbackReasonHits.zip(io.stGuardDec).map {
      case (hit, event) => hit && event.bits.reason === IntERFallbackReason.other
    }

    incByEntry(e) := PopCount(sourceIncHits)
    readDecByEntry(e) := PopCount(readDecHits)
    squashDecByEntry(e) := PopCount(squashDecHits)
    guardDecByEntry(e) := PopCount(guardDecHits)
    setFallbackByEntry(e) := sourceFallbackHits.foldLeft(false.B)(_ || _) ||
      sameCycleBypassFallbackHits.foldLeft(false.B)(_ || _) ||
      readFallbackHits.foldLeft(false.B)(_ || _) ||
      guardFallbackHits.foldLeft(false.B)(_ || _) ||
      redefFallbackHits.foldLeft(false.B)(_ || _)
    setProducedReadyByEntry(e) := producerReadyHits.foldLeft(false.B)(_ || _)
    setRedefinerByEntry(e) := redefHits.foldLeft(false.B)(_ || _)
    setRedefinerNSByEntry(e) := guardDecHits.foldLeft(false.B)(_ || _)
    genMismatchByEntry(e) := readGenMismatchHits.foldLeft(false.B)(_ || _) ||
      squashGenMismatchHits.foldLeft(false.B)(_ || _) ||
      guardGenMismatchHits.foldLeft(false.B)(_ || _)
    commitClearByEntry(e) := commitClearHits.foldLeft(false.B)(_ || _)
    releasedReuseByEntry(e) := releasedReuseHits.foldLeft(false.B)(_ || _)

    val allocatedHereForFallback = VecInit((0 until RenameWidth).map(i => allocFire(i) && allocIdx(i) === e.U)).asUInt.orR
    val fallbackTransition = (isCounting(entry) || allocatedHereForFallback) &&
      (setFallbackByEntry(e) || setSaturatedFallbackByEntry(e))
    val moveReason = sourceFallbackMoveHits.foldLeft(false.B)(_ || _)
    val storeDataReason = sourceFallbackStoreDataHits.foldLeft(false.B)(_ || _)
    val unsupportedConsumerReason = sourceFallbackUnsupportedConsumerHits.foldLeft(false.B)(_ || _) ||
      redefFallbackUnsupportedConsumerHits.foldLeft(false.B)(_ || _)
    val unsupportedReadPathReason = readFallbackUnsupportedPathHits.foldLeft(false.B)(_ || _) ||
      guardFallbackUnsupportedPathHits.foldLeft(false.B)(_ || _)
    val replayProneReason = readFallbackReplayProneHits.foldLeft(false.B)(_ || _) ||
      guardFallbackReplayProneHits.foldLeft(false.B)(_ || _)
    val uncertainReason = readFallbackUncertainHits.foldLeft(false.B)(_ || _) ||
      guardFallbackUncertainHits.foldLeft(false.B)(_ || _)
    val sameCycleReason = sameCycleBypassFallbackHits.foldLeft(false.B)(_ || _)
    val counterSaturatedReason = setSaturatedFallbackByEntry(e)
    val staleReason = readFallbackStaleHits.foldLeft(false.B)(_ || _) ||
      guardFallbackStaleHits.foldLeft(false.B)(_ || _)
    val duplicateReason = false.B
    val explicitMultipleReason = sourceFallbackMultipleHits.foldLeft(false.B)(_ || _) ||
      redefFallbackMultipleHits.foldLeft(false.B)(_ || _) ||
      readFallbackMultipleHits.foldLeft(false.B)(_ || _) ||
      guardFallbackMultipleHits.foldLeft(false.B)(_ || _)
    val explicitOtherReason = sourceFallbackOtherHits.foldLeft(false.B)(_ || _) ||
      redefFallbackOtherHits.foldLeft(false.B)(_ || _) ||
      readFallbackOtherHits.foldLeft(false.B)(_ || _) ||
      guardFallbackOtherHits.foldLeft(false.B)(_ || _)
    val reasonFlags = Seq(
      moveReason,
      storeDataReason,
      unsupportedConsumerReason,
      unsupportedReadPathReason,
      replayProneReason,
      uncertainReason,
      sameCycleReason,
      counterSaturatedReason,
      staleReason,
      duplicateReason,
      explicitOtherReason
    )
    val reasonPop = PopCount(reasonFlags)
    val multiReason = explicitMultipleReason || reasonPop > 1.U
    val hasKnownReason = reasonPop.orR || explicitMultipleReason
    firstFallbackByEntry(e) := fallbackTransition
    firstFallbackMoveByEntry(e) := fallbackTransition && !multiReason && moveReason
    firstFallbackStoreDataByEntry(e) := fallbackTransition && !multiReason && storeDataReason
    firstFallbackUnsupportedConsumerByEntry(e) := fallbackTransition && !multiReason && unsupportedConsumerReason
    firstFallbackUnsupportedReadPathByEntry(e) := fallbackTransition && !multiReason && unsupportedReadPathReason
    firstFallbackReplayProneByEntry(e) := fallbackTransition && !multiReason && replayProneReason
    firstFallbackUncertainByEntry(e) := fallbackTransition && !multiReason && uncertainReason
    firstFallbackSameCycleBypassByEntry(e) := fallbackTransition && !multiReason && sameCycleReason
    firstFallbackCounterSaturatedByEntry(e) := fallbackTransition && !multiReason && counterSaturatedReason
    firstFallbackStaleByEntry(e) := fallbackTransition && !multiReason && staleReason
    firstFallbackDuplicateByEntry(e) := fallbackTransition && !multiReason && duplicateReason
    firstFallbackMultipleByEntry(e) := fallbackTransition && multiReason
    firstFallbackOtherByEntry(e) := fallbackTransition && !multiReason && (!hasKnownReason || explicitOtherReason)
  }

  private val nextBeforeEarly = Wire(Vec(entryCount, new IntSparseUCAEntry))
  private val earlyEligible = Wire(Vec(entryCount, Bool()))
  for (e <- 0 until entryCount) {
    val old = entries(e)
    val next = WireInit(old)
    val dec = readDecByEntry(e) +& squashDecByEntry(e) +& guardDecByEntry(e)
    val inc = incByEntry(e)
    val sum = old.userCounter +& inc
    val saturated = isCounting(old) && sum > maxCounter
    val underflow = isCounting(old) && dec > sum
    val updateCounter = isCounting(old) && !underflow
    val nextCounter = Mux(saturated, maxCounter, sum(IntERCounterWidth - 1, 0)) - dec(IntERCounterWidth - 1, 0)
    val allocatedHere = VecInit((0 until RenameWidth).map(i => allocFire(i) && allocIdx(i) === e.U)).asUInt.orR
    val allocLaneOH = VecInit((0 until RenameWidth).map(i => allocFire(i) && allocIdx(i) === e.U))
    val allocBits = Mux1H(allocLaneOH, io.rename.alloc.map(_.bits))
    val allocCounter = 1.U(updateCountWidth.W) + incByEntry(e)
    val allocSaturated = allocatedHere && allocCounter > maxCounter
    val fallbackSet = setFallbackByEntry(e) || saturated

    assert(!underflow, "IntSparseUCA user counter decrement underflow")
    assert(!(isCounting(old) && old.pdest === 0.U), "IntSparseUCA counting entry has x0 physical destination")

    when(io.redirectKill && !isReleased(old)) {
      next := 0.U.asTypeOf(new IntSparseUCAEntry)
      next.gen := old.gen
    }.elsewhen(commitClearByEntry(e)) {
      next := 0.U.asTypeOf(new IntSparseUCAEntry)
      next.gen := old.gen
    }.elsewhen(allocatedHere) {
      next := 0.U.asTypeOf(new IntSparseUCAEntry)
      next.state := IntEREntryState.counting
      next.pdest := allocBits.pdest
      next.producerRobIdx := allocBits.robIdx
      next.userCounter := Mux(allocSaturated, maxCounter, allocCounter(IntERCounterWidth - 1, 0))
      next.gen := old.gen + 1.U
      next.producedReady := setProducedReadyByEntry(e)
      next.redefinerNS := setRedefinerNSByEntry(e)
      when(setRedefinerByEntry(e)) {
        val redefLaneOH = VecInit((0 until RenameWidth).map(i => redefFire(i) && redefMatchOH(i)(e)))
        next.redefinerSeen := true.B
        next.redefinerRobIdx := Mux1H(redefLaneOH, io.rename.redef.map(_.bits.robIdx))
      }
      when(setFallbackByEntry(e) || allocSaturated) {
        next.state := IntEREntryState.fallbackWaitCommit
        next.fallback := true.B
      }
    }.otherwise {
      when(setProducedReadyByEntry(e)) {
        next.producedReady := true.B
      }
      when(setRedefinerByEntry(e)) {
        val redefLaneOH = VecInit((0 until RenameWidth).map(i => redefFire(i) && redefMatchOH(i)(e)))
        next.redefinerSeen := true.B
        next.redefinerRobIdx := Mux1H(redefLaneOH, io.rename.redef.map(_.bits.robIdx))
      }
      when(setRedefinerNSByEntry(e)) {
        next.redefinerNS := true.B
      }
      when(releasedReuseByEntry(e)) {
        next.releasedReused := true.B
      }
      when(updateCounter) {
        next.userCounter := nextCounter
      }
      when(fallbackSet) {
        next.state := IntEREntryState.fallbackWaitCommit
        next.fallback := true.B
      }
    }

    nextBeforeEarly(e) := next
    setSaturatedFallbackByEntry(e) := saturated || allocSaturated
    earlyEligible(e) := isCounting(next) && !next.fallback && next.producedReady &&
      next.redefinerNS && next.userCounter === 0.U && !next.earlyFreeIssued
  }

  private val earlyMask = Wire(Vec(IntEREarlyFreeWidth + 1, UInt(entryCount.W)))
  private val earlyIdx = Wire(Vec(IntEREarlyFreeWidth, UInt(entryIdxWidth.W)))
  private val earlyOpportunity = Wire(Vec(IntEREarlyFreeWidth, Bool()))
  earlyMask(0) := earlyEligible.asUInt
  for (i <- 0 until IntEREarlyFreeWidth) {
    earlyOpportunity(i) := earlyMask(i).orR
    earlyIdx(i) := PriorityEncoder(earlyMask(i))(entryIdxWidth - 1, 0)
    earlyMask(i + 1) := earlyMask(i) & ~UIntToOH(earlyIdx(i), entryCount).asUInt

    io.earlyFree(i).valid := earlyOpportunity(i) && !IntERObserveOnly.B
    io.earlyFree(i).bits.valid := earlyOpportunity(i) && !IntERObserveOnly.B
    io.earlyFree(i).bits.pdest := entryAt(nextBeforeEarly, earlyIdx(i)).pdest
    io.earlyFree(i).bits.trackId := earlyIdx(i)
    io.earlyFree(i).bits.trackGen := entryAt(nextBeforeEarly, earlyIdx(i)).gen
    io.earlyFree(i).bits.redefRobIdx := entryAt(nextBeforeEarly, earlyIdx(i)).redefinerRobIdx
  }

  private val nextEntries = Wire(Vec(entryCount, new IntSparseUCAEntry))
  for (e <- 0 until entryCount) {
    val next = WireInit(nextBeforeEarly(e))
    val earlySelected = VecInit((0 until IntEREarlyFreeWidth).map(i => earlyOpportunity(i) && earlyIdx(i) === e.U)).asUInt.orR

    assert(!(earlySelected && nextBeforeEarly(e).pdest === 0.U), "IntSparseUCA attempted to release x0 physical destination")

    when(earlySelected && !IntERObserveOnly.B) {
      next.state := IntEREntryState.releasedWaitCommit
      next.earlyFreeIssued := true.B
    }.elsewhen(earlySelected) {
      next.earlyFreeIssued := true.B
    }

    nextEntries(e) := next
  }

  entries := nextEntries

  private val countingEntryTime = entries.map(isCounting)
  private val readyEntryTime = entries.map { entry =>
    val noRedefiner = !entry.redefinerSeen
    val guardPending = entry.redefinerSeen && !entry.redefinerNS
    val producerPending = !entry.producedReady
    val outstandingGuardToken = Mux(guardPending, 1.U(IntERCounterWidth.W), 0.U(IntERCounterWidth.W))
    val consumerPending = entry.userCounter > outstandingGuardToken
    isCounting(entry) && !entry.fallback &&
      !(noRedefiner || guardPending || producerPending || consumerPending)
  }
  private val blockerNoRedefinerEntryTime = entries.map { entry =>
    val noRedefiner = !entry.redefinerSeen
    val guardPending = entry.redefinerSeen && !entry.redefinerNS
    val producerPending = !entry.producedReady
    val outstandingGuardToken = Mux(guardPending, 1.U(IntERCounterWidth.W), 0.U(IntERCounterWidth.W))
    val consumerPending = entry.userCounter > outstandingGuardToken
    isCounting(entry) && PopCount(Seq(noRedefiner, guardPending, producerPending, consumerPending)) === 1.U && noRedefiner
  }
  private val blockerGuardEntryTime = entries.map { entry =>
    val noRedefiner = !entry.redefinerSeen
    val guardPending = entry.redefinerSeen && !entry.redefinerNS
    val producerPending = !entry.producedReady
    val outstandingGuardToken = Mux(guardPending, 1.U(IntERCounterWidth.W), 0.U(IntERCounterWidth.W))
    val consumerPending = entry.userCounter > outstandingGuardToken
    isCounting(entry) && PopCount(Seq(noRedefiner, guardPending, producerPending, consumerPending)) === 1.U && guardPending
  }
  private val blockerProducerEntryTime = entries.map { entry =>
    val noRedefiner = !entry.redefinerSeen
    val guardPending = entry.redefinerSeen && !entry.redefinerNS
    val producerPending = !entry.producedReady
    val outstandingGuardToken = Mux(guardPending, 1.U(IntERCounterWidth.W), 0.U(IntERCounterWidth.W))
    val consumerPending = entry.userCounter > outstandingGuardToken
    isCounting(entry) && PopCount(Seq(noRedefiner, guardPending, producerPending, consumerPending)) === 1.U && producerPending
  }
  private val blockerConsumerEntryTime = entries.map { entry =>
    val noRedefiner = !entry.redefinerSeen
    val guardPending = entry.redefinerSeen && !entry.redefinerNS
    val producerPending = !entry.producedReady
    val outstandingGuardToken = Mux(guardPending, 1.U(IntERCounterWidth.W), 0.U(IntERCounterWidth.W))
    val consumerPending = entry.userCounter > outstandingGuardToken
    isCounting(entry) && PopCount(Seq(noRedefiner, guardPending, producerPending, consumerPending)) === 1.U && consumerPending
  }
  private val blockerMultipleEntryTime = entries.map { entry =>
    val noRedefiner = !entry.redefinerSeen
    val guardPending = entry.redefinerSeen && !entry.redefinerNS
    val producerPending = !entry.producedReady
    val outstandingGuardToken = Mux(guardPending, 1.U(IntERCounterWidth.W), 0.U(IntERCounterWidth.W))
    val consumerPending = entry.userCounter > outstandingGuardToken
    isCounting(entry) && PopCount(Seq(noRedefiner, guardPending, producerPending, consumerPending)) > 1.U
  }
  private val earlyEligibleCount = PopCount(earlyEligible)
  private val earlySelectedCount = PopCount(earlyOpportunity)
  private val earlyDeferredCount = earlyEligibleCount - earlySelectedCount
  private val earlyWidthLimitedCycle = earlyEligibleCount > IntEREarlyFreeWidth.U

  private val allocCount = RegInit(0.U(32.W))
  private val fullUntrackedCount = RegInit(0.U(32.W))
  private val sourceMatchCount = RegInit(0.U(32.W))
  private val sourceDuplicateCount = RegInit(0.U(32.W))
  private val readDoneDecCount = RegInit(0.U(32.W))
  private val squashDecCount = RegInit(0.U(32.W))
  private val guardDecCount = RegInit(0.U(32.W))
  private val fallbackCount = RegInit(0.U(32.W))
  private val saturatedFallbackCount = RegInit(0.U(32.W))
  private val producerReadyCount = RegInit(0.U(32.W))
  private val earlyFreeOpportunityCount = RegInit(0.U(32.W))
  private val earlyFreeCount = RegInit(0.U(32.W))
  private val commitSuppressCount = RegInit(0.U(32.W))
  private val commitIdentityMismatchCount = RegInit(0.U(32.W))
  private val genMismatchCount = RegInit(0.U(32.W))
  private val redirectKillCount = RegInit(0.U(32.W))
  private val countingEntryTimeCount = RegInit(0.U(32.W))
  private val readyEntryTimeCount = RegInit(0.U(32.W))
  private val blockerNoRedefinerCount = RegInit(0.U(32.W))
  private val blockerGuardCount = RegInit(0.U(32.W))
  private val blockerProducerCount = RegInit(0.U(32.W))
  private val blockerConsumerCount = RegInit(0.U(32.W))
  private val blockerMultipleCount = RegInit(0.U(32.W))
  private val earlyFreeEligibleCount = RegInit(0.U(32.W))
  private val earlyFreeDeferredCount = RegInit(0.U(32.W))
  private val earlyFreeWidthLimitedCycleCount = RegInit(0.U(32.W))
  private val firstFallbackCount = RegInit(0.U(32.W))
  private val firstFallbackMoveCount = RegInit(0.U(32.W))
  private val firstFallbackStoreDataCount = RegInit(0.U(32.W))
  private val firstFallbackUnsupportedConsumerCount = RegInit(0.U(32.W))
  private val firstFallbackUnsupportedReadPathCount = RegInit(0.U(32.W))
  private val firstFallbackReplayProneCount = RegInit(0.U(32.W))
  private val firstFallbackUncertainCount = RegInit(0.U(32.W))
  private val firstFallbackSameCycleBypassCount = RegInit(0.U(32.W))
  private val firstFallbackCounterSaturatedCount = RegInit(0.U(32.W))
  private val firstFallbackStaleCount = RegInit(0.U(32.W))
  private val firstFallbackDuplicateCount = RegInit(0.U(32.W))
  private val firstFallbackMultipleCount = RegInit(0.U(32.W))
  private val firstFallbackOtherCount = RegInit(0.U(32.W))
  private val commitClearCountingCount = RegInit(0.U(32.W))
  private val commitClearFallbackWaitCount = RegInit(0.U(32.W))
  private val commitClearReleasedWaitCount = RegInit(0.U(32.W))
  private val releasedReusedBeforeCommitCount = RegInit(0.U(32.W))
  private val releasedUnreusedAtCommitCount = RegInit(0.U(32.W))

  allocCount := allocCount + PopCount(allocFire)
  fullUntrackedCount := fullUntrackedCount + PopCount(io.rename.alloc.map(a => a.valid && !io.redirectKill)) - PopCount(allocFire)
  sourceMatchCount := sourceMatchCount + PopCount(sourceCounted.flatten)
  sourceDuplicateCount := sourceDuplicateCount + PopCount(sourceDuplicate.flatten)
  readDoneDecCount := readDoneDecCount + readDecByEntry.foldLeft(0.U(32.W))(_ + _)
  squashDecCount := squashDecCount + squashDecByEntry.foldLeft(0.U(32.W))(_ + _)
  guardDecCount := guardDecCount + guardDecByEntry.foldLeft(0.U(32.W))(_ + _)
  fallbackCount := fallbackCount + PopCount(VecInit((0 until entryCount).map(e => setFallbackByEntry(e) || setSaturatedFallbackByEntry(e))))
  saturatedFallbackCount := saturatedFallbackCount + PopCount(setSaturatedFallbackByEntry)
  producerReadyCount := producerReadyCount + PopCount(setProducedReadyByEntry)
  earlyFreeOpportunityCount := earlyFreeOpportunityCount + PopCount(earlyOpportunity)
  earlyFreeCount := earlyFreeCount + PopCount(io.earlyFree.map(_.valid))
  commitSuppressCount := commitSuppressCount + PopCount(commitSuppressFire)
  commitIdentityMismatchCount := commitIdentityMismatchCount + PopCount(commitIdentityMismatch)
  genMismatchCount := genMismatchCount + PopCount(genMismatchByEntry)
  redirectKillCount := redirectKillCount + PopCount(VecInit(entries.map(e => io.redirectKill && isActive(e) && !isReleased(e))))
  countingEntryTimeCount := countingEntryTimeCount + PopCount(countingEntryTime)
  readyEntryTimeCount := readyEntryTimeCount + PopCount(readyEntryTime)
  blockerNoRedefinerCount := blockerNoRedefinerCount + PopCount(blockerNoRedefinerEntryTime)
  blockerGuardCount := blockerGuardCount + PopCount(blockerGuardEntryTime)
  blockerProducerCount := blockerProducerCount + PopCount(blockerProducerEntryTime)
  blockerConsumerCount := blockerConsumerCount + PopCount(blockerConsumerEntryTime)
  blockerMultipleCount := blockerMultipleCount + PopCount(blockerMultipleEntryTime)
  earlyFreeEligibleCount := earlyFreeEligibleCount + earlyEligibleCount
  earlyFreeDeferredCount := earlyFreeDeferredCount + earlyDeferredCount
  earlyFreeWidthLimitedCycleCount := earlyFreeWidthLimitedCycleCount + Mux(earlyWidthLimitedCycle, 1.U, 0.U)
  firstFallbackCount := firstFallbackCount + PopCount(firstFallbackByEntry)
  firstFallbackMoveCount := firstFallbackMoveCount + PopCount(firstFallbackMoveByEntry)
  firstFallbackStoreDataCount := firstFallbackStoreDataCount + PopCount(firstFallbackStoreDataByEntry)
  firstFallbackUnsupportedConsumerCount := firstFallbackUnsupportedConsumerCount + PopCount(firstFallbackUnsupportedConsumerByEntry)
  firstFallbackUnsupportedReadPathCount := firstFallbackUnsupportedReadPathCount + PopCount(firstFallbackUnsupportedReadPathByEntry)
  firstFallbackReplayProneCount := firstFallbackReplayProneCount + PopCount(firstFallbackReplayProneByEntry)
  firstFallbackUncertainCount := firstFallbackUncertainCount + PopCount(firstFallbackUncertainByEntry)
  firstFallbackSameCycleBypassCount := firstFallbackSameCycleBypassCount + PopCount(firstFallbackSameCycleBypassByEntry)
  firstFallbackCounterSaturatedCount := firstFallbackCounterSaturatedCount + PopCount(firstFallbackCounterSaturatedByEntry)
  firstFallbackStaleCount := firstFallbackStaleCount + PopCount(firstFallbackStaleByEntry)
  firstFallbackDuplicateCount := firstFallbackDuplicateCount + PopCount(firstFallbackDuplicateByEntry)
  firstFallbackMultipleCount := firstFallbackMultipleCount + PopCount(firstFallbackMultipleByEntry)
  firstFallbackOtherCount := firstFallbackOtherCount + PopCount(firstFallbackOtherByEntry)
  commitClearCountingCount := commitClearCountingCount + PopCount(entries.zip(commitClearByEntry).map {
    case (entry, clear) => clear && isCounting(entry)
  })
  commitClearFallbackWaitCount := commitClearFallbackWaitCount + PopCount(entries.zip(commitClearByEntry).map {
    case (entry, clear) => clear && isFallbackWait(entry)
  })
  commitClearReleasedWaitCount := commitClearReleasedWaitCount + PopCount(entries.zip(commitClearByEntry).map {
    case (entry, clear) => clear && isReleased(entry)
  })
  releasedReusedBeforeCommitCount := releasedReusedBeforeCommitCount + PopCount(entries.zip(commitClearByEntry).zip(releasedReuseByEntry).map {
    case ((entry, clear), reusedThisCycle) => clear && isReleased(entry) && (entry.releasedReused || reusedThisCycle)
  })
  releasedUnreusedAtCommitCount := releasedUnreusedAtCommitCount + PopCount(entries.zip(commitClearByEntry).zip(releasedReuseByEntry).map {
    case ((entry, clear), reusedThisCycle) => clear && isReleased(entry) && !entry.releasedReused && !reusedThisCycle
  })

  for (e <- 0 until entryCount) {
    io.debug.entries(e).state := entries(e).state
    io.debug.entries(e).pdest := entries(e).pdest
    io.debug.entries(e).producerRobIdx := entries(e).producerRobIdx
    io.debug.entries(e).redefinerRobIdx := entries(e).redefinerRobIdx
    io.debug.entries(e).userCounter := entries(e).userCounter
    io.debug.entries(e).gen := entries(e).gen
    io.debug.entries(e).fallback := entries(e).fallback
    io.debug.entries(e).redefinerSeen := entries(e).redefinerSeen
    io.debug.entries(e).redefinerNS := entries(e).redefinerNS
    io.debug.entries(e).producedReady := entries(e).producedReady
    io.debug.entries(e).earlyFreeIssued := entries(e).earlyFreeIssued
    io.debug.entries(e).releasedReused := entries(e).releasedReused
  }
  io.debug.activeCount := PopCount(entries.map(isActive))
  io.debug.allocCount := allocCount
  io.debug.fullUntrackedCount := fullUntrackedCount
  io.debug.sourceMatchCount := sourceMatchCount
  io.debug.sourceDuplicateCount := sourceDuplicateCount
  io.debug.readDoneDecCount := readDoneDecCount
  io.debug.squashDecCount := squashDecCount
  io.debug.guardDecCount := guardDecCount
  io.debug.fallbackCount := fallbackCount
  io.debug.saturatedFallbackCount := saturatedFallbackCount
  io.debug.producerReadyCount := producerReadyCount
  io.debug.earlyFreeOpportunityCount := earlyFreeOpportunityCount
  io.debug.earlyFreeCount := earlyFreeCount
  io.debug.commitSuppressCount := commitSuppressCount
  io.debug.commitIdentityMismatchCount := commitIdentityMismatchCount
  io.debug.genMismatchCount := genMismatchCount
  io.debug.redirectKillCount := redirectKillCount
  io.debug.countingEntryTimeCount := countingEntryTimeCount
  io.debug.readyEntryTimeCount := readyEntryTimeCount
  io.debug.blockerNoRedefinerCount := blockerNoRedefinerCount
  io.debug.blockerGuardCount := blockerGuardCount
  io.debug.blockerProducerCount := blockerProducerCount
  io.debug.blockerConsumerCount := blockerConsumerCount
  io.debug.blockerMultipleCount := blockerMultipleCount
  io.debug.earlyFreeEligibleCount := earlyFreeEligibleCount
  io.debug.earlyFreeDeferredCount := earlyFreeDeferredCount
  io.debug.earlyFreeWidthLimitedCycleCount := earlyFreeWidthLimitedCycleCount
  io.debug.firstFallbackCount := firstFallbackCount
  io.debug.firstFallbackMoveCount := firstFallbackMoveCount
  io.debug.firstFallbackStoreDataCount := firstFallbackStoreDataCount
  io.debug.firstFallbackUnsupportedConsumerCount := firstFallbackUnsupportedConsumerCount
  io.debug.firstFallbackUnsupportedReadPathCount := firstFallbackUnsupportedReadPathCount
  io.debug.firstFallbackReplayProneCount := firstFallbackReplayProneCount
  io.debug.firstFallbackUncertainCount := firstFallbackUncertainCount
  io.debug.firstFallbackSameCycleBypassCount := firstFallbackSameCycleBypassCount
  io.debug.firstFallbackCounterSaturatedCount := firstFallbackCounterSaturatedCount
  io.debug.firstFallbackStaleCount := firstFallbackStaleCount
  io.debug.firstFallbackDuplicateCount := firstFallbackDuplicateCount
  io.debug.firstFallbackMultipleCount := firstFallbackMultipleCount
  io.debug.firstFallbackOtherCount := firstFallbackOtherCount
  io.debug.commitClearCountingCount := commitClearCountingCount
  io.debug.commitClearFallbackWaitCount := commitClearFallbackWaitCount
  io.debug.commitClearReleasedWaitCount := commitClearReleasedWaitCount
  io.debug.releasedReusedBeforeCommitCount := releasedReusedBeforeCommitCount
  io.debug.releasedUnreusedAtCommitCount := releasedUnreusedAtCommitCount
}
