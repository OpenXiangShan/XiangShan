package xiangshan.backend

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import chisel3.util.log2Ceil
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.nio.file.{Files, Path, Paths}
import scala.io.Source
import top.DefaultConfig
import utility.{LogUtilsOptions, LogUtilsOptionsKey, PerfCounterOptions, PerfCounterOptionsKey, XSPerfLevel}
import xiangshan._
import xiangshan.backend.Bundles.EnqRobUop
import xiangshan.backend.rename.freelist.MEFreeList
import xiangshan.backend.rob.RenameBuffer
import xiangshan.backend.regfile.IntPregParams

class IntERMEFreeListProbe(size: Int)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val redirect = Input(Bool())
    val walk = Input(Bool())
    val doAllocate = Input(Bool())
    val allocateReq = Input(Vec(RenameWidth, Bool()))
    val walkReq = Input(Vec(RabCommitWidth, Bool()))
    val freeReq = Input(Vec(RabCommitWidth, Bool()))
    val freePhyReg = Input(Vec(RabCommitWidth, UInt(PhyRegIdxWidth.W)))
    val earlyFreeReq = Input(Vec(IntEREarlyFreeWidth, Bool()))
    val earlyFreePhyReg = Input(Vec(IntEREarlyFreeWidth, UInt(PhyRegIdxWidth.W)))
    val canAllocate = Output(Bool())
    val allocatePhyReg = Output(Vec(RenameWidth, UInt(PhyRegIdxWidth.W)))
  })

  val freeList = Module(new MEFreeList(size, RabCommitWidth))

  freeList.io.redirect := io.redirect
  freeList.io.walk := io.walk
  freeList.io.doAllocate := io.doAllocate
  freeList.io.allocateReq := io.allocateReq
  freeList.io.walkReq := io.walkReq
  freeList.io.freeReq := io.freeReq
  freeList.io.freePhyReg := io.freePhyReg
  freeList.io.commit.doCommit := false.B
  freeList.io.commit.archAlloc := 0.U.asTypeOf(freeList.io.commit.archAlloc)
  freeList.io.snpt := 0.U.asTypeOf(freeList.io.snpt)
  freeList.io.debug_rat.foreach(_ := 0.U.asTypeOf(freeList.io.debug_rat.get))

  val earlyReq = freeList.io.elements.get("earlyFreeReq")
  val earlyPhyReg = freeList.io.elements.get("earlyFreePhyReg")
  require(
    earlyReq.isDefined && earlyPhyReg.isDefined,
    "integer MEFreeList must expose early-free lanes when Int ER is enabled"
  )
  earlyReq.get.asInstanceOf[Vec[Bool]] := io.earlyFreeReq
  earlyPhyReg.get.asInstanceOf[Vec[UInt]] := io.earlyFreePhyReg

  io.canAllocate := freeList.io.canAllocate
  io.allocatePhyReg := freeList.io.allocatePhyReg
}

class IntERRabCommitProbe(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val enqValid = Input(Bool())
    val redefValid = Input(Bool())
    val trackId = Input(UInt(IntERTrackIdWidth.W))
    val trackGen = Input(UInt(IntERTrackGenBits.W))
    val oldPdest = Input(UInt(PhyRegIdxWidth.W))
    val redefRobIdxValue = Input(UInt(log2Ceil(RobSize).W))

    val commitValid = Output(Bool())
    val commitRedefValid = Output(Bool())
    val commitTrackId = Output(UInt(IntERTrackIdWidth.W))
    val commitTrackGen = Output(UInt(IntERTrackGenBits.W))
    val commitOldPdest = Output(UInt(PhyRegIdxWidth.W))
    val commitRedefRobIdxValue = Output(UInt(log2Ceil(RobSize).W))
  })

  val rab = Module(new RenameBuffer(RabSize))

  rab.io.redirect.valid := false.B
  rab.io.redirect.bits := 0.U.asTypeOf(rab.io.redirect.bits)
  rab.io.fromRob.walkSize := 0.U
  rab.io.fromRob.walkEnd := true.B
  rab.io.fromRob.commitSize := io.enqValid.asUInt
  rab.io.fromRob.vecLoadExcp.valid := false.B
  rab.io.fromRob.vecLoadExcp.bits := 0.U.asTypeOf(rab.io.fromRob.vecLoadExcp.bits)
  rab.io.snpt := 0.U.asTypeOf(rab.io.snpt)

  for (i <- 0 until RenameWidth) {
    rab.io.req(i).valid := io.enqValid && i.U === 0.U
    rab.io.req(i).bits := 0.U.asTypeOf(new EnqRobUop)
    rab.io.req(i).bits.ldest := 5.U
    rab.io.req(i).bits.pdest := 21.U
    rab.io.req(i).bits.rfWen := true.B
    rab.io.req(i).bits.firstUop := true.B
    rab.io.req(i).bits.lastUop := true.B
    rab.io.req(i).bits.intER.get.redef.valid := io.redefValid
    rab.io.req(i).bits.intER.get.redef.trackId := io.trackId
    rab.io.req(i).bits.intER.get.redef.trackGen := io.trackGen
    rab.io.req(i).bits.intER.get.redef.oldPdest := io.oldPdest
    rab.io.req(i).bits.robIdx.flag := false.B
    rab.io.req(i).bits.robIdx.value := io.redefRobIdxValue
  }

  io.commitValid := rab.io.commits.commitValid(0)
  io.commitRedefValid := rab.io.commits.info(0).intERCommitRedef.get.valid
  io.commitTrackId := rab.io.commits.info(0).intERCommitRedef.get.bits.trackId
  io.commitTrackGen := rab.io.commits.info(0).intERCommitRedef.get.bits.trackGen
  io.commitOldPdest := rab.io.commits.info(0).intERCommitRedef.get.bits.oldPdest
  io.commitRedefRobIdxValue := rab.io.commits.info(0).intERCommitRedef.get.bits.redefinerRobIdx.value
}

class IntERUCAFreeListProbe(size: Int)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val redirect = Input(Bool())
    val walk = Input(Bool())
    val doAllocate = Input(Bool())
    val allocateReq = Input(Vec(RenameWidth, Bool()))

    val allocValid = Input(Bool())
    val allocPdest = Input(UInt(PhyRegIdxWidth.W))
    val allocRobIdxValue = Input(UInt(log2Ceil(RobSize).W))

    val redefValid = Input(Bool())
    val redefOldPdest = Input(UInt(PhyRegIdxWidth.W))
    val redefRobIdxValue = Input(UInt(log2Ceil(RobSize).W))

    val producerReadyValid = Input(Bool())
    val producerReadyPdest = Input(UInt(PhyRegIdxWidth.W))
    val producerReadyRobIdxValue = Input(UInt(log2Ceil(RobSize).W))

    val guardDecValid = Input(Bool())
    val guardDecTrackId = Input(UInt(IntERTrackIdWidth.W))
    val guardDecTrackGen = Input(UInt(IntERTrackGenBits.W))
    val guardDecOldPdest = Input(UInt(PhyRegIdxWidth.W))
    val guardDecRobIdxValue = Input(UInt(log2Ceil(RobSize).W))

    val commitNeedFree = Input(Bool())
    val commitOldPdest = Input(UInt(PhyRegIdxWidth.W))
    val commitRedefValid = Input(Bool())
    val commitRedefTrackId = Input(UInt(IntERTrackIdWidth.W))
    val commitRedefTrackGen = Input(UInt(IntERTrackGenBits.W))
    val commitRedefOldPdest = Input(UInt(PhyRegIdxWidth.W))
    val commitRedefRobIdxValue = Input(UInt(log2Ceil(RobSize).W))

    val canAllocate = Output(Bool())
    val allocatePhyReg = Output(Vec(RenameWidth, UInt(PhyRegIdxWidth.W)))
    val earlyFreeValid = Output(Bool())
    val earlyFreePdest = Output(UInt(PhyRegIdxWidth.W))
    val suppress = Output(Bool())
    val gatedFreeReq = Output(Bool())
    val activeCount = Output(UInt(log2Ceil(IntERTrackEntries + 1).W))
    val entryState = Output(Vec(IntERTrackEntries, UInt(IntEREntryState.width.W)))
  })

  private def setRobPtr(ptr: xiangshan.backend.rob.RobPtr, value: UInt): Unit = {
    ptr.flag := false.B
    ptr.value := value
  }

  val uca = Module(new IntSparseUCA)
  val freeList = Module(new MEFreeList(size, RabCommitWidth))

  uca.io.redirectKill := false.B
  uca.io.rename.source := 0.U.asTypeOf(uca.io.rename.source)
  uca.io.rename.sourceFallback := 0.U.asTypeOf(uca.io.rename.sourceFallback)
  uca.io.rename.sourceFallbackReason := VecInit(Seq.fill(RenameWidth)(IntERFallbackReason.none))
  uca.io.rename.alloc := 0.U.asTypeOf(uca.io.rename.alloc)
  uca.io.rename.redef := 0.U.asTypeOf(uca.io.rename.redef)
  uca.io.rename.redefFallback := 0.U.asTypeOf(uca.io.rename.redefFallback)
  uca.io.rename.redefFallbackReason := VecInit(Seq.fill(RenameWidth)(IntERFallbackReason.none))
  uca.io.producerReady := 0.U.asTypeOf(uca.io.producerReady)
  uca.io.readDone := 0.U.asTypeOf(uca.io.readDone)
  uca.io.squash := 0.U.asTypeOf(uca.io.squash)
  uca.io.stGuardDec := 0.U.asTypeOf(uca.io.stGuardDec)
  uca.io.commitOldPdest := 0.U.asTypeOf(uca.io.commitOldPdest)
  uca.io.commitNeedFree := 0.U.asTypeOf(uca.io.commitNeedFree)
  uca.io.commitRedef := 0.U.asTypeOf(uca.io.commitRedef)

  uca.io.rename.alloc(0).valid := io.allocValid
  uca.io.rename.alloc(0).bits.pdest := io.allocPdest
  setRobPtr(uca.io.rename.alloc(0).bits.robIdx, io.allocRobIdxValue)
  uca.io.rename.redef(0).valid := io.redefValid
  uca.io.rename.redef(0).bits.oldPdest := io.redefOldPdest
  setRobPtr(uca.io.rename.redef(0).bits.robIdx, io.redefRobIdxValue)

  uca.io.producerReady(0).valid := io.producerReadyValid
  uca.io.producerReady(0).bits.valid := io.producerReadyValid
  uca.io.producerReady(0).bits.pdest := io.producerReadyPdest
  setRobPtr(uca.io.producerReady(0).bits.robIdx, io.producerReadyRobIdxValue)

  uca.io.stGuardDec(0).valid := io.guardDecValid
  uca.io.stGuardDec(0).bits.valid := io.guardDecValid
  uca.io.stGuardDec(0).bits.trackId := io.guardDecTrackId
  uca.io.stGuardDec(0).bits.trackGen := io.guardDecTrackGen
  uca.io.stGuardDec(0).bits.oldPdest := io.guardDecOldPdest
  setRobPtr(uca.io.stGuardDec(0).bits.robIdx, io.guardDecRobIdxValue)

  uca.io.commitNeedFree(0) := io.commitNeedFree
  uca.io.commitOldPdest(0) := io.commitOldPdest
  uca.io.commitRedef(0).valid := io.commitRedefValid
  uca.io.commitRedef(0).bits.trackId := io.commitRedefTrackId
  uca.io.commitRedef(0).bits.trackGen := io.commitRedefTrackGen
  uca.io.commitRedef(0).bits.oldPdest := io.commitRedefOldPdest
  setRobPtr(uca.io.commitRedef(0).bits.redefinerRobIdx, io.commitRedefRobIdxValue)

  freeList.io.redirect := io.redirect
  freeList.io.walk := io.walk
  freeList.io.doAllocate := io.doAllocate
  freeList.io.allocateReq := io.allocateReq
  freeList.io.walkReq := 0.U.asTypeOf(freeList.io.walkReq)
  freeList.io.freeReq := 0.U.asTypeOf(freeList.io.freeReq)
  freeList.io.freePhyReg := 0.U.asTypeOf(freeList.io.freePhyReg)
  freeList.io.commit.doCommit := false.B
  freeList.io.commit.archAlloc := 0.U.asTypeOf(freeList.io.commit.archAlloc)
  freeList.io.snpt := 0.U.asTypeOf(freeList.io.snpt)
  freeList.io.debug_rat.foreach(_ := 0.U.asTypeOf(freeList.io.debug_rat.get))

  val conventionalFree = io.commitNeedFree && !uca.io.commitSuppress(0).suppress
  freeList.io.freeReq(0) := conventionalFree
  freeList.io.freePhyReg(0) := io.commitOldPdest
  freeList.io.earlyFreeReq.get := VecInit(uca.io.earlyFree.map(_.valid))
  freeList.io.earlyFreePhyReg.get := VecInit(uca.io.earlyFree.map(_.bits.pdest))

  io.canAllocate := freeList.io.canAllocate
  io.allocatePhyReg := freeList.io.allocatePhyReg
  io.earlyFreeValid := uca.io.earlyFree(0).valid
  io.earlyFreePdest := uca.io.earlyFree(0).bits.pdest
  io.suppress := uca.io.commitSuppress(0).suppress
  io.gatedFreeReq := conventionalFree
  io.activeCount := uca.io.debug.activeCount
  io.entryState := VecInit(uca.io.debug.entries.map(_.state))
}

class IntEarlyReleaseFreeListTest extends AnyFlatSpec with Matchers with ChiselSim {
  behavior of "Int ER integer free list integration"

  private def sourceText(path: String): String = {
    val source = Source.fromFile(repoPath(path).toFile)
    try {
      source.mkString
    } finally {
      source.close()
    }
  }

  private def repoPath(path: String): Path = {
    val relative = Paths.get(path)
    Iterator.iterate(Paths.get("").toAbsolutePath)(_.getParent)
      .takeWhile(_ != null)
      .map(_.resolve(relative))
      .find(Files.exists(_))
      .getOrElse(relative)
  }

  private def configWith(params: IntEarlyReleaseParams): Parameters = {
    val defaultConfig = new DefaultConfig
    defaultConfig.alterPartial({
      case XSCoreParamsKey => defaultConfig(XSTileKey).head.copy(
        DecodeWidth = 1,
        RenameWidth = 1,
        CommitWidth = 1,
        RobCommitWidth = 1,
        RabCommitWidth = 1,
        RobSize = 16,
        RabSize = 16,
        RenameSnapshotNum = 1,
        intPreg = IntPregParams(numEntries = 64, numBank = 1, numRead = None, numWrite = None),
        intEarlyRelease = params
      )
    }).alter((site, here, up) => {
      case DebugOptionsKey => up(DebugOptionsKey).copy(
        AlwaysBasicDiff = false,
        EnableDifftest = false,
        EnablePerfDebug = false,
        EnableDebug = false
      )
      case LogUtilsOptionsKey => LogUtilsOptions(
        here(DebugOptionsKey).EnableDebug,
        here(DebugOptionsKey).EnablePerfDebug,
        here(DebugOptionsKey).FPGAPlatform,
        here(DebugOptionsKey).EnableXMR
      )
      case PerfCounterOptionsKey => PerfCounterOptions(
        enablePerfPrint = false,
        enablePerfDB = false,
        perfLevel = XSPerfLevel.withName(here(DebugOptionsKey).PerfLevel),
        perfDBHartID = 0
      )
    })
  }

  private def clearInputs(dut: IntERMEFreeListProbe): Unit = {
    dut.io.redirect.poke(false.B)
    dut.io.walk.poke(false.B)
    dut.io.doAllocate.poke(false.B)
    for (i <- dut.io.allocateReq.indices) {
      dut.io.allocateReq(i).poke(false.B)
    }
    for (i <- dut.io.walkReq.indices) {
      dut.io.walkReq(i).poke(false.B)
      dut.io.freeReq(i).poke(false.B)
      dut.io.freePhyReg(i).poke(0.U)
    }
    for (i <- dut.io.earlyFreeReq.indices) {
      dut.io.earlyFreeReq(i).poke(false.B)
      dut.io.earlyFreePhyReg(i).poke(0.U)
    }
  }

  private def clearCombinedInputs(dut: IntERUCAFreeListProbe): Unit = {
    dut.io.redirect.poke(false.B)
    dut.io.walk.poke(false.B)
    dut.io.doAllocate.poke(false.B)
    for (i <- dut.io.allocateReq.indices) {
      dut.io.allocateReq(i).poke(false.B)
    }
    dut.io.allocValid.poke(false.B)
    dut.io.allocPdest.poke(0.U)
    dut.io.allocRobIdxValue.poke(0.U)
    dut.io.redefValid.poke(false.B)
    dut.io.redefOldPdest.poke(0.U)
    dut.io.redefRobIdxValue.poke(0.U)
    dut.io.producerReadyValid.poke(false.B)
    dut.io.producerReadyPdest.poke(0.U)
    dut.io.producerReadyRobIdxValue.poke(0.U)
    dut.io.guardDecValid.poke(false.B)
    dut.io.guardDecTrackId.poke(0.U)
    dut.io.guardDecTrackGen.poke(0.U)
    dut.io.guardDecOldPdest.poke(0.U)
    dut.io.guardDecRobIdxValue.poke(0.U)
    dut.io.commitNeedFree.poke(false.B)
    dut.io.commitOldPdest.poke(0.U)
    dut.io.commitRedefValid.poke(false.B)
    dut.io.commitRedefTrackId.poke(0.U)
    dut.io.commitRedefTrackGen.poke(0.U)
    dut.io.commitRedefOldPdest.poke(0.U)
    dut.io.commitRedefRobIdxValue.poke(0.U)
  }

  private def resetCombinedDut(dut: IntERUCAFreeListProbe): Unit = {
    clearCombinedInputs(dut)
    dut.reset.poke(true.B)
    dut.clock.step(2)
    dut.reset.poke(false.B)
    dut.clock.step(2)
    clearCombinedInputs(dut)
  }

  private def resetDut(dut: IntERMEFreeListProbe): Unit = {
    clearInputs(dut)
    dut.reset.poke(true.B)
    dut.clock.step(2)
    dut.reset.poke(false.B)
    dut.clock.step(2)
    clearInputs(dut)
  }

  private def allocateUntil(dut: IntERUCAFreeListProbe, preg: Int, maxCycles: Int): Boolean = {
    var sawPreg = false
    for (_ <- 0 until maxCycles) {
      dut.io.allocateReq(0).poke(true.B)
      dut.io.doAllocate.poke(true.B)
      val allocated = dut.io.allocatePhyReg(0).peek().litValue
      sawPreg ||= dut.io.canAllocate.peek().litToBoolean && allocated == BigInt(preg)
      dut.clock.step()
      clearCombinedInputs(dut)
    }
    sawPreg
  }

  it should "expose stable MEFreeList early-free perf counter names" in {
    val source = sourceText("src/main/scala/xiangshan/backend/rename/freelist/MEFreeList.scala")

    Seq(
      "int_er_me_freelist_early_free_req",
      "int_er_me_freelist_early_free_merged",
      "int_er_me_freelist_early_free_req_under_rename_width",
      "int_er_me_freelist_early_free_req_under_2rename_width",
      "int_er_me_freelist_early_free_req_when_cannot_allocate",
      "int_er_me_freelist_early_free_cycle_under_rename_width",
      "int_er_me_freelist_early_free_cycle_under_2rename_width",
      "int_er_me_freelist_early_free_cycle_when_cannot_allocate"
    ).foreach { counterName =>
      source should include(s"""XSPerfAccumulate("$counterName"""")
    }
  }

  it should "expose stable MEFreeList capacity perf counter names" in {
    val source = sourceText("src/main/scala/xiangshan/backend/rename/freelist/MEFreeList.scala")

    Seq(
      "int_er_me_freelist_free_reg_sum",
      "int_er_me_freelist_low_rename_width_cycle",
      "int_er_me_freelist_low_2rename_width_cycle",
      "int_er_me_freelist_empty_cycle",
      "int_er_me_freelist_free_reg_count"
    ).foreach { counterName =>
      source should include(counterName)
    }
    source should include("XSPerfHistogram(\"int_er_me_freelist_free_reg_count\"")
  }

  it should "merge early-free lanes into later integer allocations" in {
    val config = configWith(IntEarlyReleaseParams(enable = true, observeOnly = false, trackEntries = 2))

    simulate(new IntERMEFreeListProbe(size = 8)(config)) { dut =>
      resetDut(dut)

      dut.io.earlyFreeReq(0).poke(true.B)
      dut.io.earlyFreePhyReg(0).poke(55.U)
      dut.clock.step()
      clearInputs(dut)

      var sawEarlyFreedReg = false
      for (_ <- 0 until 12) {
        dut.io.allocateReq(0).poke(true.B)
        dut.io.doAllocate.poke(true.B)
        val allocated = dut.io.allocatePhyReg(0).peek().litValue
        sawEarlyFreedReg ||= dut.io.canAllocate.peek().litToBoolean && allocated == BigInt(55)
        dut.clock.step()
        clearInputs(dut)
      }

      sawEarlyFreedReg shouldBe true
    }
  }

  it should "merge eight early-free lanes into later integer allocations" in {
    val config = configWith(IntEarlyReleaseParams(
      enable = true,
      observeOnly = false,
      trackEntries = 64,
      earlyFreeWidth = 8
    ))

    simulate(new IntERMEFreeListProbe(size = 16)(config)) { dut =>
      resetDut(dut)

      val releasedRegs = Seq(40, 41, 42, 43, 44, 45, 46, 47)
      releasedRegs.zipWithIndex.foreach { case (preg, lane) =>
        dut.io.earlyFreeReq(lane).poke(true.B)
        dut.io.earlyFreePhyReg(lane).poke(preg.U)
      }
      dut.clock.step()
      clearInputs(dut)

      var seen = Set.empty[BigInt]
      for (_ <- 0 until 32) {
        dut.io.allocateReq(0).poke(true.B)
        dut.io.doAllocate.poke(true.B)
        val allocated = dut.io.allocatePhyReg(0).peek().litValue
        if (dut.io.canAllocate.peek().litToBoolean) {
          seen += allocated
        }
        dut.clock.step()
        clearInputs(dut)
      }

      releasedRegs.foreach { preg =>
        seen should contain(BigInt(preg))
      }
    }
  }

  it should "fail fast on same-cycle conventional and early release of one integer preg" in {
    val config = configWith(IntEarlyReleaseParams(enable = true, observeOnly = false, trackEntries = 2))

    assertThrows[Exception] {
      simulate(new IntERMEFreeListProbe(size = 8)(config)) { dut =>
        resetDut(dut)

        dut.io.freeReq(0).poke(true.B)
        dut.io.freePhyReg(0).poke(7.U)
        dut.io.earlyFreeReq(0).poke(true.B)
        dut.io.earlyFreePhyReg(0).poke(7.U)
        dut.clock.step()
      }
    }
  }

  it should "fail fast on duplicate early release lanes" in {
    val config = configWith(IntEarlyReleaseParams(enable = true, observeOnly = false, trackEntries = 2, earlyFreeWidth = 2))

    assertThrows[Exception] {
      simulate(new IntERMEFreeListProbe(size = 8)(config)) { dut =>
        resetDut(dut)

        dut.io.earlyFreeReq(0).poke(true.B)
        dut.io.earlyFreePhyReg(0).poke(33.U)
        dut.io.earlyFreeReq(1).poke(true.B)
        dut.io.earlyFreePhyReg(1).poke(33.U)
        dut.clock.step()
      }
    }
  }

  it should "keep conventional free after ABA reuse while suppressing the original redefiner" in {
    val config = configWith(IntEarlyReleaseParams(enable = true, observeOnly = false, trackEntries = 2))

    simulate(new IntERUCAFreeListProbe(size = 8)(config)) { dut =>
      resetCombinedDut(dut)

      dut.io.allocValid.poke(true.B)
      dut.io.allocPdest.poke(55.U)
      dut.io.allocRobIdxValue.poke(1.U)
      dut.clock.step()
      clearCombinedInputs(dut)
      dut.io.activeCount.expect(1.U)

      dut.io.redefValid.poke(true.B)
      dut.io.redefOldPdest.poke(55.U)
      dut.io.redefRobIdxValue.poke(7.U)
      dut.clock.step()
      clearCombinedInputs(dut)

      dut.io.producerReadyValid.poke(true.B)
      dut.io.producerReadyPdest.poke(55.U)
      dut.io.producerReadyRobIdxValue.poke(1.U)
      dut.io.guardDecValid.poke(true.B)
      dut.io.guardDecTrackId.poke(0.U)
      dut.io.guardDecTrackGen.poke(1.U)
      dut.io.guardDecOldPdest.poke(55.U)
      dut.io.guardDecRobIdxValue.poke(7.U)
      dut.io.earlyFreeValid.expect(true.B)
      dut.io.earlyFreePdest.expect(55.U)
      dut.clock.step()
      clearCombinedInputs(dut)
      dut.io.entryState(0).expect(IntEREntryState.releasedWaitCommit)

      allocateUntil(dut, preg = 55, maxCycles = 12) shouldBe true

      dut.io.allocValid.poke(true.B)
      dut.io.allocPdest.poke(55.U)
      dut.io.allocRobIdxValue.poke(2.U)
      dut.clock.step()
      clearCombinedInputs(dut)
      dut.io.entryState(1).expect(IntEREntryState.counting)

      dut.io.redefValid.poke(true.B)
      dut.io.redefOldPdest.poke(55.U)
      dut.io.redefRobIdxValue.poke(11.U)
      dut.clock.step()
      clearCombinedInputs(dut)

      dut.io.commitNeedFree.poke(true.B)
      dut.io.commitOldPdest.poke(55.U)
      dut.io.commitRedefValid.poke(true.B)
      dut.io.commitRedefTrackId.poke(1.U)
      dut.io.commitRedefTrackGen.poke(1.U)
      dut.io.commitRedefOldPdest.poke(55.U)
      dut.io.commitRedefRobIdxValue.poke(11.U)
      dut.io.suppress.expect(false.B)
      dut.io.gatedFreeReq.expect(true.B)
      dut.clock.step()
      clearCombinedInputs(dut)
      dut.io.activeCount.expect(1.U)

      allocateUntil(dut, preg = 55, maxCycles = 12) shouldBe true

      dut.io.commitNeedFree.poke(true.B)
      dut.io.commitOldPdest.poke(55.U)
      dut.io.commitRedefValid.poke(true.B)
      dut.io.commitRedefTrackId.poke(0.U)
      dut.io.commitRedefTrackGen.poke(1.U)
      dut.io.commitRedefOldPdest.poke(55.U)
      dut.io.commitRedefRobIdxValue.poke(7.U)
      dut.io.suppress.expect(true.B)
      dut.io.gatedFreeReq.expect(false.B)
      dut.clock.step()
      clearCombinedInputs(dut)
      dut.io.activeCount.expect(0.U)
    }
  }

  it should "preserve exact commit suppress identity metadata through RAB commit info" in {
    val config = configWith(IntEarlyReleaseParams(enable = true, observeOnly = false, trackEntries = 2))

    simulate(new IntERRabCommitProbe()(config)) { dut =>
      dut.io.enqValid.poke(false.B)
      dut.io.redefValid.poke(false.B)
      dut.io.trackId.poke(0.U)
      dut.io.trackGen.poke(0.U)
      dut.io.oldPdest.poke(0.U)
      dut.io.redefRobIdxValue.poke(0.U)
      dut.reset.poke(true.B)
      dut.clock.step(2)
      dut.reset.poke(false.B)
      dut.clock.step()

      dut.io.enqValid.poke(true.B)
      dut.io.redefValid.poke(true.B)
      dut.io.trackId.poke(1.U)
      dut.io.trackGen.poke(3.U)
      dut.io.oldPdest.poke(17.U)
      dut.io.redefRobIdxValue.poke(6.U)
      dut.clock.step()

      dut.io.enqValid.poke(false.B)
      dut.io.commitValid.expect(true.B)
      dut.io.commitRedefValid.expect(true.B)
      dut.io.commitTrackId.expect(1.U)
      dut.io.commitTrackGen.expect(3.U)
      dut.io.commitOldPdest.expect(17.U)
      dut.io.commitRedefRobIdxValue.expect(6.U)
    }
  }
}
