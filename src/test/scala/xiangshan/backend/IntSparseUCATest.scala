package xiangshan.backend

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import top.DefaultConfig
import xiangshan._

class IntSparseUCATest extends AnyFlatSpec with Matchers with ChiselSim {
  behavior of "IntSparseUCA"

  private def configWith(params: IntEarlyReleaseParams): Parameters = {
    val defaultConfig = new DefaultConfig
    defaultConfig.alterPartial({
      case XSCoreParamsKey => defaultConfig(XSTileKey).head.copy(
        intEarlyRelease = params
      )
    })
  }

  private def setRobPtr(ptr: xiangshan.backend.rob.RobPtr, value: Int): Unit = {
    ptr.flag.poke(false.B)
    ptr.value.poke(value.U)
  }

  private def clearInputs(dut: IntSparseUCA): Unit = {
    dut.io.redirectKill.poke(false.B)

    for (i <- dut.io.rename.source.indices) {
      dut.io.rename.sourceFallback(i).poke(false.B)
      dut.io.rename.alloc(i).valid.poke(false.B)
      dut.io.rename.alloc(i).bits.pdest.poke(0.U)
      setRobPtr(dut.io.rename.alloc(i).bits.robIdx, 0)
      dut.io.rename.redef(i).valid.poke(false.B)
      dut.io.rename.redef(i).bits.oldPdest.poke(0.U)
      setRobPtr(dut.io.rename.redef(i).bits.robIdx, 0)
      dut.io.rename.redefFallback(i).poke(false.B)
      for (s <- dut.io.rename.source(i).indices) {
        dut.io.rename.source(i)(s).valid.poke(false.B)
        dut.io.rename.source(i)(s).psrc.poke(0.U)
        dut.io.rename.source(i)(s).srcIdx.poke(s.U)
      }
    }

    for (event <- dut.io.producerReady) {
      event.valid.poke(false.B)
      event.bits.valid.poke(false.B)
      setRobPtr(event.bits.robIdx, 0)
      event.bits.pdest.poke(0.U)
    }

    for (event <- dut.io.readDone) {
      event.valid.poke(false.B)
      setRobPtr(event.bits.robIdx, 0)
      event.bits.fallback.poke(false.B)
      event.bits.reason.poke(IntERFallbackReason.none)
      for (s <- event.bits.src.indices) {
        event.bits.src(s).valid.poke(false.B)
        event.bits.src(s).trackId.poke(0.U)
        event.bits.src(s).trackGen.poke(0.U)
        event.bits.src(s).srcIdx.poke(s.U)
        event.bits.src(s).psrc.poke(0.U)
      }
    }

    for (event <- dut.io.squash) {
      event.valid.poke(false.B)
      setRobPtr(event.bits.robIdx, 0)
      for (s <- event.bits.src.indices) {
        event.bits.src(s).valid.poke(false.B)
        event.bits.src(s).trackId.poke(0.U)
        event.bits.src(s).trackGen.poke(0.U)
        event.bits.src(s).srcIdx.poke(s.U)
        event.bits.src(s).psrc.poke(0.U)
      }
    }

    for (event <- dut.io.stGuardDec) {
      event.valid.poke(false.B)
      event.bits.valid.poke(false.B)
      setRobPtr(event.bits.robIdx, 0)
      event.bits.trackId.poke(0.U)
      event.bits.trackGen.poke(0.U)
      event.bits.oldPdest.poke(0.U)
      event.bits.fallback.poke(false.B)
      event.bits.reason.poke(IntERFallbackReason.none)
    }

    for (i <- dut.io.commitNeedFree.indices) {
      dut.io.commitNeedFree(i).poke(false.B)
      dut.io.commitOldPdest(i).poke(0.U)
      dut.io.commitRedef(i).valid.poke(false.B)
      dut.io.commitRedef(i).bits.oldPdest.poke(0.U)
      dut.io.commitRedef(i).bits.trackId.poke(0.U)
      dut.io.commitRedef(i).bits.trackGen.poke(0.U)
      setRobPtr(dut.io.commitRedef(i).bits.redefinerRobIdx, 0)
    }
  }

  private def resetDut(dut: IntSparseUCA): Unit = {
    clearInputs(dut)
    dut.reset.poke(true.B)
    dut.clock.step()
    dut.reset.poke(false.B)
    dut.clock.step()
    clearInputs(dut)
  }

  private def allocate(dut: IntSparseUCA, pdest: Int, robIdx: Int, lane: Int = 0): Unit = {
    dut.io.rename.alloc(lane).valid.poke(true.B)
    dut.io.rename.alloc(lane).bits.pdest.poke(pdest.U)
    setRobPtr(dut.io.rename.alloc(lane).bits.robIdx, robIdx)
  }

  private def redef(dut: IntSparseUCA, oldPdest: Int, robIdx: Int, lane: Int = 0): Unit = {
    dut.io.rename.redef(lane).valid.poke(true.B)
    dut.io.rename.redef(lane).bits.oldPdest.poke(oldPdest.U)
    setRobPtr(dut.io.rename.redef(lane).bits.robIdx, robIdx)
  }

  private def producerReady(dut: IntSparseUCA, pdest: Int, robIdx: Int, lane: Int = 0): Unit = {
    dut.io.producerReady(lane).valid.poke(true.B)
    dut.io.producerReady(lane).bits.valid.poke(true.B)
    dut.io.producerReady(lane).bits.pdest.poke(pdest.U)
    setRobPtr(dut.io.producerReady(lane).bits.robIdx, robIdx)
  }

  private def guardDec(
    dut: IntSparseUCA,
    trackId: Int = 0,
    gen: Int = 1,
    oldPdest: Int = 5,
    robIdx: Int = 7,
    lane: Int = 0
  ): Unit = {
    dut.io.stGuardDec(lane).valid.poke(true.B)
    dut.io.stGuardDec(lane).bits.valid.poke(true.B)
    dut.io.stGuardDec(lane).bits.trackId.poke(trackId.U)
    dut.io.stGuardDec(lane).bits.trackGen.poke(gen.U)
    dut.io.stGuardDec(lane).bits.oldPdest.poke(oldPdest.U)
    setRobPtr(dut.io.stGuardDec(lane).bits.robIdx, robIdx)
  }

  private def releaseOpportunity(
    dut: IntSparseUCA,
    pdest: Int = 5,
    producerRobIdx: Int = 1,
    redefinerRobIdx: Int = 7,
    trackId: Int = 0,
    gen: Int = 1
  ): Unit = {
    redef(dut, oldPdest = pdest, robIdx = redefinerRobIdx)
    dut.clock.step()
    clearInputs(dut)
    producerReady(dut, pdest = pdest, robIdx = producerRobIdx)
    guardDec(dut, trackId = trackId, gen = gen, oldPdest = pdest, robIdx = redefinerRobIdx)
  }

  private def stepAndClear(dut: IntSparseUCA): Unit = {
    dut.clock.step()
    clearInputs(dut)
  }

  private def allocateAndSettle(dut: IntSparseUCA, pdest: Int = 5, robIdx: Int = 1, lane: Int = 0): Unit = {
    allocate(dut, pdest = pdest, robIdx = robIdx, lane = lane)
    stepAndClear(dut)
  }

  private def expectReleasedWaitCommit(dut: IntSparseUCA, entryIdx: Int = 0): Unit = {
    dut.io.debug.entries(entryIdx).state.expect(IntEREntryState.releasedWaitCommit)
  }

  private def readDone(
    dut: IntSparseUCA,
    trackId: Int = 0,
    gen: Int = 1,
    srcSlot: Int = 0,
    psrc: Int = 5,
    fallback: Boolean = false,
    reason: UInt = IntERFallbackReason.none
  ): Unit = {
    dut.io.readDone(0).valid.poke(true.B)
    dut.io.readDone(0).bits.fallback.poke(fallback.B)
    dut.io.readDone(0).bits.reason.poke(reason)
    dut.io.readDone(0).bits.src(srcSlot).valid.poke(true.B)
    dut.io.readDone(0).bits.src(srcSlot).trackId.poke(trackId.U)
    dut.io.readDone(0).bits.src(srcSlot).trackGen.poke(gen.U)
    dut.io.readDone(0).bits.src(srcSlot).srcIdx.poke(srcSlot.U)
    dut.io.readDone(0).bits.src(srcSlot).psrc.poke(psrc.U)
  }

  private def squash(dut: IntSparseUCA, trackId: Int = 0, gen: Int = 1, srcSlot: Int = 0, psrc: Int = 5): Unit = {
    dut.io.squash(0).valid.poke(true.B)
    dut.io.squash(0).bits.src(srcSlot).valid.poke(true.B)
    dut.io.squash(0).bits.src(srcSlot).trackId.poke(trackId.U)
    dut.io.squash(0).bits.src(srcSlot).trackGen.poke(gen.U)
    dut.io.squash(0).bits.src(srcSlot).srcIdx.poke(srcSlot.U)
    dut.io.squash(0).bits.src(srcSlot).psrc.poke(psrc.U)
  }

  private def commitRedef(
    dut: IntSparseUCA,
    lane: Int,
    oldPdest: Int,
    trackId: Int,
    gen: Int,
    redefinerRobIdx: Int
  ): Unit = {
    dut.io.commitNeedFree(lane).poke(true.B)
    dut.io.commitOldPdest(lane).poke(oldPdest.U)
    dut.io.commitRedef(lane).valid.poke(true.B)
    dut.io.commitRedef(lane).bits.oldPdest.poke(oldPdest.U)
    dut.io.commitRedef(lane).bits.trackId.poke(trackId.U)
    dut.io.commitRedef(lane).bits.trackGen.poke(gen.U)
    setRobPtr(dut.io.commitRedef(lane).bits.redefinerRobIdx, redefinerRobIdx)
  }

  it should "track L entries and leave extra producers untracked" in {
    val config = configWith(IntEarlyReleaseParams(trackEntries = 1))
    simulate(new IntSparseUCA()(config)) { dut =>
      resetDut(dut)

      allocate(dut, pdest = 5, robIdx = 1)
      dut.io.rename.destTrack(0).valid.expect(true.B)
      dut.io.rename.destTrack(0).trackId.expect(0.U)
      dut.io.rename.destTrack(0).trackGen.expect(1.U)
      dut.clock.step()
      clearInputs(dut)
      dut.io.debug.activeCount.expect(1.U)
      dut.io.debug.entries(0).pdest.expect(5.U)

      allocate(dut, pdest = 6, robIdx = 2)
      dut.io.rename.destTrack(0).valid.expect(false.B)
      dut.clock.step()
      clearInputs(dut)
      dut.io.debug.activeCount.expect(1.U)
      dut.io.debug.fullUntrackedCount.expect(1.U)
    }
  }

  it should "allocate a free entry for a valid nonzero rename lane" in {
    val config = configWith(IntEarlyReleaseParams(trackEntries = 1))
    simulate(new IntSparseUCA()(config)) { dut =>
      resetDut(dut)

      allocate(dut, pdest = 9, robIdx = 3, lane = 1)
      dut.io.rename.destTrack(0).valid.expect(false.B)
      dut.io.rename.destTrack(1).valid.expect(true.B)
      dut.io.rename.destTrack(1).trackId.expect(0.U)
      dut.io.rename.destTrack(1).trackGen.expect(1.U)
      dut.clock.step()
      clearInputs(dut)

      dut.io.debug.activeCount.expect(1.U)
      dut.io.debug.entries(0).pdest.expect(9.U)
      dut.io.debug.entries(0).userCounter.expect(1.U)
      dut.io.debug.fullUntrackedCount.expect(0.U)
    }
  }

  it should "count duplicate source matches only once per uop" in {
    val config = configWith(IntEarlyReleaseParams(trackEntries = 2))
    simulate(new IntSparseUCA()(config)) { dut =>
      resetDut(dut)

      allocate(dut, pdest = 5, robIdx = 1)
      dut.clock.step()
      clearInputs(dut)

      dut.io.rename.source(0)(0).valid.poke(true.B)
      dut.io.rename.source(0)(0).psrc.poke(5.U)
      dut.io.rename.source(0)(0).srcIdx.poke(0.U)
      dut.io.rename.source(0)(1).valid.poke(true.B)
      dut.io.rename.source(0)(1).psrc.poke(5.U)
      dut.io.rename.source(0)(1).srcIdx.poke(1.U)
      dut.io.rename.srcMatch(0)(0).valid.expect(true.B)
      dut.io.rename.srcMatch(0)(1).valid.expect(false.B)
      dut.clock.step()
      clearInputs(dut)

      dut.io.debug.entries(0).userCounter.expect(2.U)
      dut.io.debug.sourceMatchCount.expect(1.U)
      dut.io.debug.sourceDuplicateCount.expect(1.U)
    }
  }

  it should "count same-cycle older allocation sources and suppress same-uop duplicates" in {
    val config = configWith(IntEarlyReleaseParams(trackEntries = 2))
    simulate(new IntSparseUCA()(config)) { dut =>
      resetDut(dut)

      allocate(dut, pdest = 5, robIdx = 1, lane = 0)
      dut.io.rename.source(1)(0).valid.poke(true.B)
      dut.io.rename.source(1)(0).psrc.poke(5.U)
      dut.io.rename.source(1)(0).srcIdx.poke(0.U)
      dut.io.rename.source(1)(1).valid.poke(true.B)
      dut.io.rename.source(1)(1).psrc.poke(5.U)
      dut.io.rename.source(1)(1).srcIdx.poke(1.U)
      dut.io.rename.destTrack(0).valid.expect(true.B)
      dut.io.rename.srcMatch(1)(0).valid.expect(true.B)
      dut.io.rename.srcMatch(1)(0).trackId.expect(0.U)
      dut.io.rename.srcMatch(1)(0).trackGen.expect(1.U)
      dut.io.rename.srcMatch(1)(1).valid.expect(false.B)
      dut.clock.step()
      clearInputs(dut)

      dut.io.debug.entries(0).userCounter.expect(2.U)
      dut.io.debug.sourceMatchCount.expect(1.U)
      dut.io.debug.sourceDuplicateCount.expect(1.U)
    }
  }

  it should "fallback a same-cycle allocation when bypass matching is disabled" in {
    val config = configWith(IntEarlyReleaseParams(trackEntries = 2, allowSameCycleRenameBypassMatch = false))
    simulate(new IntSparseUCA()(config)) { dut =>
      resetDut(dut)

      allocate(dut, pdest = 5, robIdx = 1, lane = 0)
      dut.io.rename.source(1)(0).valid.poke(true.B)
      dut.io.rename.source(1)(0).psrc.poke(5.U)
      dut.io.rename.source(1)(0).srcIdx.poke(0.U)
      dut.io.rename.srcMatch(1)(0).valid.expect(false.B)
      dut.clock.step()
      clearInputs(dut)

      dut.io.debug.entries(0).state.expect(IntEREntryState.fallbackWaitCommit)
      dut.io.debug.entries(0).fallback.expect(true.B)
      dut.io.debug.entries(0).userCounter.expect(1.U)
      dut.io.debug.fallbackCount.expect(1.U)
    }
  }

  it should "decrement counted users on readDone and squash events" in {
    val config = configWith(IntEarlyReleaseParams(trackEntries = 2))
    simulate(new IntSparseUCA()(config)) { dut =>
      resetDut(dut)

      allocate(dut, pdest = 5, robIdx = 1)
      dut.clock.step()
      clearInputs(dut)

      dut.io.rename.source(0)(0).valid.poke(true.B)
      dut.io.rename.source(0)(0).psrc.poke(5.U)
      dut.io.rename.source(0)(0).srcIdx.poke(0.U)
      dut.clock.step()
      clearInputs(dut)
      dut.io.debug.entries(0).userCounter.expect(2.U)

      readDone(dut, trackId = 0, gen = 1, srcSlot = 0)
      dut.clock.step()
      clearInputs(dut)
      dut.io.debug.entries(0).userCounter.expect(1.U)
      dut.io.debug.readDoneDecCount.expect(1.U)

      dut.io.rename.source(0)(1).valid.poke(true.B)
      dut.io.rename.source(0)(1).psrc.poke(5.U)
      dut.io.rename.source(0)(1).srcIdx.poke(1.U)
      dut.clock.step()
      clearInputs(dut)
      dut.io.debug.entries(0).userCounter.expect(2.U)

      squash(dut, trackId = 0, gen = 1, srcSlot = 1)
      dut.clock.step()
      clearInputs(dut)
      dut.io.debug.entries(0).userCounter.expect(1.U)
      dut.io.debug.squashDecCount.expect(1.U)
    }
  }

  it should "ignore readDone and squash events with stale psrc identity" in {
    val config = configWith(IntEarlyReleaseParams(trackEntries = 2))
    simulate(new IntSparseUCA()(config)) { dut =>
      resetDut(dut)

      allocate(dut, pdest = 5, robIdx = 1)
      dut.clock.step()
      clearInputs(dut)

      dut.io.rename.source(0)(0).valid.poke(true.B)
      dut.io.rename.source(0)(0).psrc.poke(5.U)
      dut.io.rename.source(0)(0).srcIdx.poke(0.U)
      dut.clock.step()
      clearInputs(dut)
      dut.io.debug.entries(0).userCounter.expect(2.U)

      readDone(dut, trackId = 0, gen = 1, srcSlot = 0, psrc = 99)
      dut.clock.step()
      clearInputs(dut)
      dut.io.debug.entries(0).state.expect(IntEREntryState.counting)
      dut.io.debug.entries(0).userCounter.expect(2.U)
      dut.io.debug.readDoneDecCount.expect(0.U)

      squash(dut, trackId = 0, gen = 1, srcSlot = 0, psrc = 99)
      dut.clock.step()
      clearInputs(dut)
      dut.io.debug.entries(0).state.expect(IntEREntryState.counting)
      dut.io.debug.entries(0).userCounter.expect(2.U)
      dut.io.debug.squashDecCount.expect(0.U)

      readDone(
        dut,
        trackId = 0,
        gen = 1,
        srcSlot = 0,
        psrc = 99,
        fallback = true,
        reason = IntERFallbackReason.unsupportedReadPath
      )
      dut.clock.step()
      clearInputs(dut)
      dut.io.debug.entries(0).state.expect(IntEREntryState.counting)
      dut.io.debug.entries(0).fallback.expect(false.B)
      dut.io.debug.entries(0).userCounter.expect(2.U)
      dut.io.debug.fallbackCount.expect(0.U)

      readDone(dut, trackId = 0, gen = 2, srcSlot = 0, psrc = 99)
      dut.clock.step()
      clearInputs(dut)
      dut.io.debug.genMismatchCount.expect(0.U)
    }
  }

  it should "aggregate same-cycle source increments and validated decrements" in {
    val config = configWith(IntEarlyReleaseParams(trackEntries = 2))
    simulate(new IntSparseUCA()(config)) { dut =>
      resetDut(dut)

      allocate(dut, pdest = 5, robIdx = 1)
      dut.clock.step()
      clearInputs(dut)

      dut.io.rename.source(0)(0).valid.poke(true.B)
      dut.io.rename.source(0)(0).psrc.poke(5.U)
      dut.io.rename.source(0)(0).srcIdx.poke(0.U)
      readDone(dut, trackId = 0, gen = 1, srcSlot = 1)
      dut.clock.step()
      clearInputs(dut)

      dut.io.debug.entries(0).userCounter.expect(1.U)
      dut.io.debug.sourceMatchCount.expect(1.U)
      dut.io.debug.readDoneDecCount.expect(1.U)
    }
  }

  it should "elaborate and report debug state for sixteen tracked entries" in {
    val config = configWith(IntEarlyReleaseParams(trackEntries = 16))
    simulate(new IntSparseUCA()(config)) { dut =>
      resetDut(dut)

      for (i <- dut.io.rename.alloc.indices) {
        allocate(dut, pdest = 8 + i, robIdx = 10 + i, lane = i)
        dut.io.rename.destTrack(i).valid.expect(true.B)
        dut.io.rename.destTrack(i).trackId.expect(i.U)
      }
      dut.clock.step()
      clearInputs(dut)

      dut.io.debug.entries.length shouldBe 16
      dut.io.debug.activeCount.expect(dut.io.rename.alloc.length.U)
      for (i <- dut.io.rename.alloc.indices) {
        dut.io.debug.entries(i).pdest.expect((8 + i).U)
        dut.io.debug.entries(i).userCounter.expect(1.U)
      }
    }
  }

  it should "emit all eight early-free lanes with sixty-four tracked entries" in {
    val config = configWith(IntEarlyReleaseParams(
      enable = true,
      observeOnly = false,
      trackEntries = 64,
      earlyFreeWidth = 8,
      stWalkWidth = 8
    ))
    simulate(new IntSparseUCA()(config)) { dut =>
      resetDut(dut)

      dut.io.debug.entries.length shouldBe 64
      dut.io.earlyFree.length shouldBe 8
      dut.io.stGuardDec.length shouldBe 8

      val pdests = Seq.tabulate(8)(i => 40 + i)
      val producerRobIdxs = Seq.tabulate(8)(i => 10 + i)
      val redefinerRobIdxs = Seq.tabulate(8)(i => 100 + i)

      for (i <- pdests.indices) {
        allocate(dut, pdest = pdests(i), robIdx = producerRobIdxs(i), lane = i)
        dut.io.rename.destTrack(i).valid.expect(true.B)
        dut.io.rename.destTrack(i).trackId.expect(i.U)
        dut.io.rename.destTrack(i).trackGen.expect(1.U)
      }
      stepAndClear(dut)
      dut.io.debug.activeCount.expect(8.U)

      for (i <- pdests.indices) {
        dut.io.debug.entries(i).pdest.expect(pdests(i).U)
        dut.io.debug.entries(i).userCounter.expect(1.U)
        redef(dut, oldPdest = pdests(i), robIdx = redefinerRobIdxs(i), lane = i)
        dut.io.rename.redefTrack(i).valid.expect(true.B)
        dut.io.rename.redefTrack(i).trackId.expect(i.U)
        dut.io.rename.redefTrack(i).trackGen.expect(1.U)
      }
      stepAndClear(dut)

      val producerReadyWidth = dut.io.producerReady.length
      pdests.indices.grouped(producerReadyWidth).foreach { group =>
        group.zipWithIndex.foreach { case (entryIdx, lane) =>
          producerReady(dut, pdest = pdests(entryIdx), robIdx = producerRobIdxs(entryIdx), lane = lane)
        }
        stepAndClear(dut)
      }

      for (i <- pdests.indices) {
        guardDec(
          dut,
          trackId = i,
          gen = 1,
          oldPdest = pdests(i),
          robIdx = redefinerRobIdxs(i),
          lane = i
        )
      }
      for (i <- pdests.indices) {
        dut.io.earlyFree(i).valid.expect(true.B)
        dut.io.earlyFree(i).bits.valid.expect(true.B)
        dut.io.earlyFree(i).bits.pdest.expect(pdests(i).U)
        dut.io.earlyFree(i).bits.trackId.expect(i.U)
        dut.io.earlyFree(i).bits.trackGen.expect(1.U)
      }
      stepAndClear(dut)

      dut.io.debug.earlyFreeOpportunityCount.expect(8.U)
      dut.io.debug.earlyFreeCount.expect(8.U)
      for (i <- pdests.indices) {
        expectReleasedWaitCommit(dut, entryIdx = i)
      }
    }
  }

  it should "record observe-only opportunity without issuing early free" in {
    val config = configWith(IntEarlyReleaseParams(trackEntries = 2, observeOnly = true))
    simulate(new IntSparseUCA()(config)) { dut =>
      resetDut(dut)

      allocateAndSettle(dut)

      releaseOpportunity(dut)
      dut.io.earlyFree(0).valid.expect(false.B)
      stepAndClear(dut)

      dut.io.debug.entries(0).state.expect(IntEREntryState.counting)
      dut.io.debug.entries(0).userCounter.expect(0.U)
      dut.io.debug.entries(0).earlyFreeIssued.expect(true.B)
      dut.io.debug.earlyFreeOpportunityCount.expect(1.U)
      dut.io.debug.earlyFreeCount.expect(0.U)
    }
  }

  it should "issue early free and suppress the matching conventional free" in {
    val config = configWith(IntEarlyReleaseParams(trackEntries = 2, observeOnly = false))
    simulate(new IntSparseUCA()(config)) { dut =>
      resetDut(dut)

      allocateAndSettle(dut)

      releaseOpportunity(dut)
      dut.io.earlyFree(0).valid.expect(true.B)
      dut.io.earlyFree(0).bits.pdest.expect(5.U)
      dut.io.earlyFree(0).bits.trackId.expect(0.U)
      dut.io.earlyFree(0).bits.trackGen.expect(1.U)
      stepAndClear(dut)

      expectReleasedWaitCommit(dut)
      dut.io.rename.source(0)(0).valid.poke(true.B)
      dut.io.rename.source(0)(0).psrc.poke(5.U)
      dut.io.rename.srcMatch(0)(0).valid.expect(false.B)
      clearInputs(dut)

      commitRedef(dut, lane = 0, oldPdest = 5, trackId = 0, gen = 1, redefinerRobIdx = 7)
      dut.io.commitSuppress(0).suppress.expect(true.B)
      dut.io.commitSuppress(0).trackId.expect(0.U)
      dut.io.commitSuppress(0).trackGen.expect(1.U)
      dut.clock.step()
      clearInputs(dut)
      dut.io.debug.activeCount.expect(0.U)
      dut.io.debug.commitSuppressCount.expect(1.U)
    }
  }

  it should "not early free from a guard decrement before the redefiner is recorded" in {
    val config = configWith(IntEarlyReleaseParams(trackEntries = 2, observeOnly = false))
    simulate(new IntSparseUCA()(config)) { dut =>
      resetDut(dut)

      allocateAndSettle(dut)

      producerReady(dut, pdest = 5, robIdx = 1)
      guardDec(dut)
      dut.io.earlyFree(0).valid.expect(false.B)
      stepAndClear(dut)

      dut.io.debug.entries(0).state.expect(IntEREntryState.counting)
      dut.io.debug.entries(0).userCounter.expect(1.U)
      dut.io.debug.entries(0).redefinerSeen.expect(false.B)
      dut.io.debug.entries(0).redefinerNS.expect(false.B)
      dut.io.debug.entries(0).earlyFreeIssued.expect(false.B)
      dut.io.debug.earlyFreeCount.expect(0.U)
      dut.io.debug.guardDecCount.expect(0.U)
    }
  }

  it should "suppress only the commit lane with exact released allocation identity" in {
    val config = configWith(IntEarlyReleaseParams(trackEntries = 2, observeOnly = false))
    simulate(new IntSparseUCA()(config)) { dut =>
      resetDut(dut)

      allocateAndSettle(dut)
      releaseOpportunity(dut)
      stepAndClear(dut)
      expectReleasedWaitCommit(dut)

      allocate(dut, pdest = 5, robIdx = 2)
      stepAndClear(dut)
      dut.io.debug.entries(1).state.expect(IntEREntryState.counting)
      redef(dut, oldPdest = 5, robIdx = 11)
      dut.io.rename.redefTrack(0).valid.expect(true.B)
      dut.io.rename.redefTrack(0).trackId.expect(1.U)
      stepAndClear(dut)

      commitRedef(dut, lane = 0, oldPdest = 5, trackId = 1, gen = 1, redefinerRobIdx = 11)
      commitRedef(dut, lane = 1, oldPdest = 5, trackId = 0, gen = 1, redefinerRobIdx = 7)
      dut.io.commitSuppress(0).suppress.expect(false.B)
      dut.io.commitSuppress(1).suppress.expect(true.B)
      dut.io.commitSuppress(1).trackId.expect(0.U)
      dut.io.commitSuppress(1).trackGen.expect(1.U)
      stepAndClear(dut)

      dut.io.debug.activeCount.expect(0.U)
      dut.io.debug.commitSuppressCount.expect(1.U)
    }
  }

  it should "allow redef tracking for a reused pdest with an active newer owner" in {
    val config = configWith(IntEarlyReleaseParams(trackEntries = 2, observeOnly = false))
    noException should be thrownBy {
      simulate(new IntSparseUCA()(config)) { dut =>
        resetDut(dut)

        allocateAndSettle(dut)
        releaseOpportunity(dut)
        stepAndClear(dut)
        expectReleasedWaitCommit(dut)

        allocate(dut, pdest = 5, robIdx = 2)
        stepAndClear(dut)

        redef(dut, oldPdest = 5, robIdx = 11)
        dut.io.rename.redefTrack(0).valid.expect(true.B)
        dut.io.rename.redefTrack(0).trackId.expect(1.U)
        dut.io.rename.redefTrack(0).trackGen.expect(1.U)
        dut.clock.step()
      }
    }
  }

  it should "ignore a released pdest match when the newer owner is untracked" in {
    val config = configWith(IntEarlyReleaseParams(trackEntries = 1, observeOnly = false))
    noException should be thrownBy {
      simulate(new IntSparseUCA()(config)) { dut =>
        resetDut(dut)

        allocateAndSettle(dut)
        releaseOpportunity(dut)
        stepAndClear(dut)
        expectReleasedWaitCommit(dut)

        allocate(dut, pdest = 5, robIdx = 2)
        dut.io.rename.destTrack(0).valid.expect(false.B)
        stepAndClear(dut)
        expectReleasedWaitCommit(dut)

        redef(dut, oldPdest = 5, robIdx = 11)
        dut.io.rename.redefTrack(0).valid.expect(false.B)
        dut.clock.step()
      }
    }
  }

  it should "fail fast when a same track generation commit redef has a different released identity" in {
    val config = configWith(IntEarlyReleaseParams(trackEntries = 2, observeOnly = false))
    assertThrows[Exception] {
      simulate(new IntSparseUCA()(config)) { dut =>
        resetDut(dut)

        allocateAndSettle(dut, pdest = 0x5a, robIdx = 143)
        releaseOpportunity(dut, pdest = 0x5a, producerRobIdx = 143, redefinerRobIdx = 143)
        stepAndClear(dut)
        expectReleasedWaitCommit(dut)

        commitRedef(dut, lane = 0, oldPdest = 0x9d, trackId = 0, gen = 1, redefinerRobIdx = 171)
        dut.io.commitSuppress(0).suppress.expect(false.B)
        dut.clock.step()
      }
    }
  }

  it should "ignore pdest-only commit free and wrong released identity" in {
    val config = configWith(IntEarlyReleaseParams(trackEntries = 2, observeOnly = false))
    simulate(new IntSparseUCA()(config)) { dut =>
      resetDut(dut)

      allocateAndSettle(dut)
      releaseOpportunity(dut)
      stepAndClear(dut)
      expectReleasedWaitCommit(dut)

      dut.io.commitNeedFree(0).poke(true.B)
      dut.io.commitOldPdest(0).poke(5.U)
      dut.io.commitSuppress(0).suppress.expect(false.B)
      stepAndClear(dut)
      dut.io.debug.activeCount.expect(1.U)
    }

    assertThrows[Exception] {
      simulate(new IntSparseUCA()(config)) { dut =>
        resetDut(dut)

        allocateAndSettle(dut)
        releaseOpportunity(dut)
        stepAndClear(dut)

        commitRedef(dut, lane = 0, oldPdest = 5, trackId = 0, gen = 1, redefinerRobIdx = 8)
        dut.io.commitSuppress(0).suppress.expect(false.B)
        dut.clock.step()
      }
    }
  }

  it should "fallback on counter saturation and ignore generation-mismatched events" in {
    val config = configWith(IntEarlyReleaseParams(trackEntries = 2, counterBits = 3))
    simulate(new IntSparseUCA()(config)) { dut =>
      resetDut(dut)

      allocate(dut, pdest = 5, robIdx = 1)
      dut.clock.step()
      clearInputs(dut)

      for (i <- dut.io.rename.source.indices) {
        dut.io.rename.source(i)(0).valid.poke(true.B)
        dut.io.rename.source(i)(0).psrc.poke(5.U)
        dut.io.rename.source(i)(0).srcIdx.poke(0.U)
      }
      dut.clock.step()
      clearInputs(dut)
      dut.io.debug.entries(0).state.expect(IntEREntryState.fallbackWaitCommit)
      dut.io.debug.entries(0).fallback.expect(true.B)
      dut.io.debug.fallbackCount.expect(1.U)
      dut.io.debug.saturatedFallbackCount.expect(1.U)

      readDone(dut, trackId = 0, gen = 2)
      dut.clock.step()
      clearInputs(dut)
      dut.io.debug.entries(0).state.expect(IntEREntryState.fallbackWaitCommit)
      dut.io.debug.genMismatchCount.expect(1.U)
    }
  }

  it should "kill counting entries on redirect and keep released entries until suppress" in {
    val countingConfig = configWith(IntEarlyReleaseParams(trackEntries = 2))
    simulate(new IntSparseUCA()(countingConfig)) { dut =>
      resetDut(dut)

      allocate(dut, pdest = 5, robIdx = 1)
      dut.clock.step()
      clearInputs(dut)
      dut.io.redirectKill.poke(true.B)
      dut.clock.step()
      clearInputs(dut)
      dut.io.debug.activeCount.expect(0.U)
      dut.io.debug.redirectKillCount.expect(1.U)
    }

    val releaseConfig = configWith(IntEarlyReleaseParams(trackEntries = 2, observeOnly = false))
    simulate(new IntSparseUCA()(releaseConfig)) { dut =>
      resetDut(dut)

      allocateAndSettle(dut)
      releaseOpportunity(dut)
      stepAndClear(dut)

      dut.io.redirectKill.poke(true.B)
      stepAndClear(dut)
      dut.io.debug.activeCount.expect(1.U)
      expectReleasedWaitCommit(dut)
    }
  }

  it should "fail fast on invalid counter and destination conditions" in {
    val config = configWith(IntEarlyReleaseParams(trackEntries = 2, observeOnly = false))

    assertThrows[Exception] {
      simulate(new IntSparseUCA()(config)) { dut =>
        resetDut(dut)

        allocate(dut, pdest = 5, robIdx = 1)
        dut.clock.step()
        clearInputs(dut)

        readDone(dut)
        squash(dut)
        dut.clock.step()
      }
    }

    assertThrows[Exception] {
      simulate(new IntSparseUCA()(config)) { dut =>
        resetDut(dut)

        allocate(dut, pdest = 0, robIdx = 1)
        dut.clock.step()
        clearInputs(dut)
        dut.clock.step()
      }
    }
  }
}
