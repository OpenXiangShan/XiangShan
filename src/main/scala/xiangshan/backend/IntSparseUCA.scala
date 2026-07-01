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
    val alloc = Input(Vec(RenameWidth, ValidIO(new IntERProducerAlloc)))
    val redef = Input(Vec(RenameWidth, ValidIO(new IntERRedefProbe)))
    val redefFallback = Input(Vec(RenameWidth, Bool()))

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

  for (e <- 0 until entryCount) {
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
          src.trackId === e.U && src.trackGen === entries(e).gen &&
          src.psrc === entries(e).pdest && isCounting(entries(e))
      }
    }
    val readFallbackHits = io.readDone.flatMap { event =>
      event.bits.src.map { src =>
        event.valid && event.bits.fallback && src.valid &&
          src.trackId === e.U && src.trackGen === entries(e).gen &&
          src.psrc === entries(e).pdest && isCounting(entries(e))
      }
    }
    val readGenMismatchHits = io.readDone.flatMap { event =>
      event.bits.src.map { src =>
        event.valid && src.valid && src.trackId === e.U && src.trackGen =/= entries(e).gen &&
          src.psrc === entries(e).pdest && isActive(entries(e))
      }
    }

    val squashDecHits = io.squash.flatMap { event =>
      event.bits.src.map { src =>
        event.valid && src.valid &&
          src.trackId === e.U && src.trackGen === entries(e).gen &&
          src.psrc === entries(e).pdest && isCounting(entries(e))
      }
    }
    val squashGenMismatchHits = io.squash.flatMap { event =>
      event.bits.src.map { src =>
        event.valid && src.valid && src.trackId === e.U && src.trackGen =/= entries(e).gen &&
          src.psrc === entries(e).pdest && isActive(entries(e))
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
}
