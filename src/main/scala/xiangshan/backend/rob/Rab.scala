package xiangshan.backend.rob

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import utility._
import utility.OneHot.UIntToOHSeq
import xiangshan.backend.Bundles.EnqRobUop
import xiangshan.backend.{RabToVecExcpMod, RegWriteFromRab}
import xiangshan.backend.decode.VectorConstants
import xiangshan.backend.rename.SnapshotGenerator
import chisel3.experimental.BundleLiterals._

class RenameBufferPtr(size: Int) extends CircularQueuePtr[RenameBufferPtr](size) {
  def this()(implicit p: Parameters) = this(p(XSCoreParamsKey).RabSize)
}

object RenameBufferPtr {
  def apply(flag: Boolean = false, v: Int = 0)(implicit p: Parameters): RenameBufferPtr = {
    val ptr = Wire(new RenameBufferPtr(p(XSCoreParamsKey).RabSize))
    ptr.flag := flag.B
    ptr.value := v.U
    ptr
  }
}

class RenameBufferEntry(implicit p: Parameters) extends XSBundle {
  val info = new RabCommitInfo
  val robIdx = OptionWrapper(!env.FPGAPlatform, new RobPtr)
}

class RenameBuffer(size: Int)(implicit p: Parameters) extends XSModule with HasCircularQueuePtrHelper {
  val io = IO(new Bundle {
    val redirect = Input(ValidIO(new Bundle {
    }))

    val req = Vec(RenameWidth, Flipped(ValidIO(new EnqRobUop)))
    val fromRob = new Bundle {
      val walkSize = Input(UInt(log2Up(size).W))
      val walkEnd = Input(Bool())
      val commitSize = Input(UInt(log2Up(size).W))
      val vecLoadExcp = Input(ValidIO(new Bundle{
        val isStrided = Bool()
        val isVlm = Bool()
      }))
    }

    val snpt = Input(new SnapshotPort)

    val canEnq = Output(Bool())
    val canEnqForDispatch = Output(Bool())
    val enqPtrVec = Output(Vec(RenameWidth, new RenameBufferPtr))

    val commits = Output(new RabCommitIO)
    val diffCommits = if (backendParams.basicDebugEn) Some(Output(new DiffCommitIO)) else None

    val status = Output(new Bundle {
      val walkEnd = Bool()
      val commitEnd = Bool()
    })
    val toVecExcpMod = Output(new RabToVecExcpMod)
  })

  // alias
  private val snptSelect = io.snpt.snptSelect

  // pointer
  private val enqPtrVec = RegInit(VecInit.tabulate(RenameWidth)(idx => RenameBufferPtr(flag = false, idx)))
  private val enqPtr = enqPtrVec.head
  private val enqPtrOH = RegInit(1.U(size.W))
  private val enqPtrOHShift = CircularShift(enqPtrOH)
  // may shift [0, RenameWidth] steps
  private val enqPtrOHVec = VecInit.tabulate(RenameWidth + 1)(enqPtrOHShift.left)
  private val enqPtrVecNext = Wire(enqPtrVec.cloneType)

  private val deqPtrVec = RegInit(VecInit.tabulate(RabCommitWidth)(idx => RenameBufferPtr(flag = false, idx)))
  private val deqPtr = deqPtrVec.head
  private val deqPtrOH = RegInit(1.U(size.W))
  private val deqPtrOHShift = CircularShift(deqPtrOH)
  private val deqPtrOHVec = VecInit.tabulate(RabCommitWidth + 1)(deqPtrOHShift.left)
  private val deqPtrVecNext = Wire(deqPtrVec.cloneType)
  XSError(deqPtr.toOH =/= deqPtrOH, p"wrong one-hot reg between $deqPtr and $deqPtrOH")

  private val walkPtr = Reg(new RenameBufferPtr)
  private val walkPtrOH = UIntToOHSeq(walkPtr.value, walkPtr.entries)
  private val walkPtrOHSeq = Seq.tabulate(RabCommitWidth + 1)(CircularShift(walkPtrOH).left)
  private val walkPtrNext = Wire(new RenameBufferPtr)

  private val walkPtrSnapshots = SnapshotGenerator(enqPtr, io.snpt.snptEnq, io.snpt.snptDeq, io.redirect.valid, io.snpt.flushVec)

  val vcfgPtrOH = RegInit(1.U(size.W))
  val vcfgPtrOHShift = CircularShift(vcfgPtrOH)
  // may shift [0, 2) steps
  val vcfgPtrOHSeq = Seq.tabulate(2)(vcfgPtrOHShift.left)

  val diffPtr = RegInit(0.U.asTypeOf(new RenameBufferPtr))
  val diffPtrNext = Wire(new RenameBufferPtr)
  // Regs
  val renameBuffer = Reg(Vec(size, new RenameBufferEntry))
  val renameBufferEntries = VecInit((0 until size) map (i => renameBuffer(i)))

  val vecLoadExcp = Reg(io.fromRob.vecLoadExcp.cloneType)

  private val maxLMUL = 8
  private val vdIdxWidth = log2Up(maxLMUL + 1)
  val currentVdIdx = Reg(UInt(vdIdxWidth.W)) // store 0~8

  val s_idle :: s_special_walk :: s_walk :: Nil = Enum(3)
  val state = RegInit(s_idle)
  val stateNext = WireInit(state) // otherwise keep state value

  private val robWalkEndReg = RegInit(false.B)
  private val robWalkEnd = io.fromRob.walkEnd || robWalkEndReg

  when(io.redirect.valid) {
    robWalkEndReg := false.B
  }.elsewhen(io.fromRob.walkEnd) {
    robWalkEndReg := true.B
  }

  // only handle int, fp, vec and v0 wen, while vl is handled in VTypeBuffer
  val mayNeedAlloc = WireInit(VecInit(io.req.map(x => x.bits.needEnqRab)))
  val realNeedAlloc = io.req.zip(mayNeedAlloc).map { case (req, alloc) => req.valid && alloc }
  val enqCount    = PopCount(realNeedAlloc)
  val commitNum = Wire(UInt(RabCommitWidth.U.getWidth.W))
  val walkNum = Wire(UInt(RabCommitWidth.U.getWidth.W))
  commitNum := Mux(io.commits.commitValid(0), PriorityMux((0 until RabCommitWidth).map(
    i => io.commits.commitValid(RabCommitWidth - 1 - i) -> (RabCommitWidth - i).U
  )), 0.U)
  walkNum := Mux(io.commits.walkValid(0), PriorityMux((0 until RabCommitWidth).map(
    i => io.commits.walkValid(RabCommitWidth - 1 - i) -> (RabCommitWidth-i).U
  )), 0.U)
  val commitCount = Mux(io.commits.isCommit && !io.commits.isWalk, commitNum, 0.U)
  val walkCount   = Mux(io.commits.isWalk && !io.commits.isCommit, walkNum, 0.U)
  val specialWalkCount = Mux(io.commits.isCommit && io.commits.isWalk, walkNum, 0.U)

  // number of pair(ldest, pdest) ready to commit to arch_rat
  val commitSize = RegInit(0.U(log2Up(size).W))
  val walkSize = RegInit(0.U(log2Up(size).W))
  val specialWalkSize = RegInit(0.U(log2Up(size).W))

  val newCommitSize = io.fromRob.commitSize
  val newWalkSize = io.fromRob.walkSize

  val commitSizeNxt = commitSize + newCommitSize - commitCount
  val walkSizeNxt = walkSize + newWalkSize - walkCount

  val newSpecialWalkSize = Mux(io.redirect.valid && !io.snpt.useSnpt, commitSizeNxt, 0.U)
  val specialWalkSizeNext = specialWalkSize + newSpecialWalkSize - specialWalkCount

  commitSize := Mux(io.redirect.valid && !io.snpt.useSnpt, 0.U, commitSizeNxt)
  specialWalkSize := specialWalkSizeNext
  walkSize := Mux(io.redirect.valid, 0.U, walkSizeNxt)

  walkPtrNext := MuxCase(walkPtr, Seq(
    (state === s_idle && stateNext === s_walk) -> walkPtrSnapshots(snptSelect),
    (state === s_special_walk && stateNext === s_walk) -> deqPtrVecNext.head,
    (state === s_walk && io.snpt.useSnpt && io.redirect.valid) -> walkPtrSnapshots(snptSelect),
    (state === s_walk) -> (walkPtr + walkCount),
  ))

  walkPtr := walkPtrNext

  val walkCandidates   = VecInit(walkPtrOHSeq.map(sel => Mux1H(sel, renameBufferEntries)))
  val commitCandidates = VecInit(deqPtrOHVec.map(sel => Mux1H(sel, renameBufferEntries)))
  val vcfgCandidates   = VecInit(vcfgPtrOHSeq.map(sel => Mux1H(sel, renameBufferEntries)))

  // update diff pointer
  diffPtrNext := diffPtr + newCommitSize
  diffPtr := diffPtrNext

  // update vcfg pointer
  // TODO: do not use diffPtrNext here
  vcfgPtrOH := diffPtrNext.toOH

  // update enq pointer
  val enqPtrNext = Mux(
    state === s_walk && stateNext === s_idle,
    walkPtrNext,
    enqPtr + enqCount
  )
  val enqPtrOHNext = Mux(
    state === s_walk && stateNext === s_idle,
    walkPtrNext.toOH,
    enqPtrOHVec(enqCount)
  )
  enqPtr := enqPtrNext
  enqPtrOH := enqPtrOHNext
  enqPtrVecNext.zipWithIndex.map{ case(ptr, i) => ptr := enqPtrNext + i.U }
  enqPtrVec := enqPtrVecNext

  val deqPtrSteps = Mux1H(Seq(
    (state === s_idle) -> commitCount,
    (state === s_special_walk) -> specialWalkCount,
  ))

  // update deq pointer
  val deqPtrNext = deqPtr + deqPtrSteps
  val deqPtrOHNext = deqPtrOHVec(deqPtrSteps)
  deqPtr := deqPtrNext
  deqPtrOH := deqPtrOHNext
  deqPtrVecNext.zipWithIndex.map{ case(ptr, i) => ptr := deqPtrNext + i.U }
  deqPtrVec := deqPtrVecNext

  val allocatePtrVec = VecInit((0 until RenameWidth).map(i => enqPtrVec(PopCount(realNeedAlloc.take(i))).value))
  allocatePtrVec.zip(io.req).zip(realNeedAlloc).map{ case((allocatePtr, req), realNeedAlloc) =>
    when(realNeedAlloc){
      renameBuffer(allocatePtr).info := req.bits
      renameBuffer(allocatePtr).robIdx.foreach(_ := req.bits.robIdx)
    }
  }

  io.commits.isCommit := state === s_idle || state === s_special_walk
  io.commits.isWalk := state === s_walk || state === s_special_walk

  for(i <- 0 until RabCommitWidth) {
    io.commits.commitValid(i) := state === s_idle && i.U < commitSize || state === s_special_walk && i.U < specialWalkSize
    io.commits.walkValid(i) := state === s_walk && i.U < walkSize || state === s_special_walk && i.U < specialWalkSize
    // special walk use commitPtr
    io.commits.info(i) := Mux(state === s_idle || state === s_special_walk, commitCandidates(i).info, walkCandidates(i).info)
    io.commits.robIdx.foreach(_(i) := Mux(state === s_idle || state === s_special_walk, commitCandidates(i).robIdx.get, walkCandidates(i).robIdx.get))
  }

  private val walkEndNext = walkSizeNxt === 0.U
  private val commitEndNext = commitSizeNxt === 0.U
  private val specialWalkEndNext = specialWalkSize <= RabCommitWidth.U
  // when robWalkEndReg is 1, walkSize donot increase and decrease RabCommitWidth per Cycle
  private val walkEndNextCycle = (robWalkEndReg || io.fromRob.walkEnd && io.fromRob.walkSize === 0.U) && (walkSize <= RabCommitWidth.U)
  // change state
  state := stateNext
  when(io.redirect.valid) {
    when(io.snpt.useSnpt) {
      stateNext := s_walk
    }.otherwise {
      stateNext := s_special_walk
      vecLoadExcp := io.fromRob.vecLoadExcp
      when(io.fromRob.vecLoadExcp.valid) {
        currentVdIdx := 0.U
      }
    }
  }.otherwise {
    // change stateNext
    switch(state) {
      // this transaction is not used actually, just list all states
      is(s_idle) {
        stateNext := s_idle
      }
      is(s_special_walk) {
        currentVdIdx := currentVdIdx + specialWalkCount
        when(specialWalkEndNext) {
          stateNext := s_walk
          vecLoadExcp.valid := false.B
        }
      }
      is(s_walk) {
        when(walkEndNextCycle) {
          stateNext := s_idle
        }
      }
    }
  }

  val numValidEntries = distanceBetween(enqPtr, deqPtr)
  val allowEnqueue = GatedValidRegNext(numValidEntries + enqCount <= (size - RenameWidth).U, true.B)
  val allowEnqueueForDispatch = GatedValidRegNext(numValidEntries + enqCount <= (size - 2*RenameWidth).U, true.B)

  private val enableRabTypePerf = {
    val perfOptions = p(PerfCounterOptionsKey)
    perfOptions.enablePerfPrint || perfOptions.enablePerfDB
  }

  if (enableRabTypePerf) {
    val rabTypeCount = 4
    val rabIntType = 0
    val rabFpType = 1
    val rabVecType = 2
    val rabV0Type = 3
    val typePrefixWidth = log2Ceil(size + 1)

    def typeMatch(rfWen: Bool, fpWen: Bool, vecWen: Bool, v0Wen: Bool, typeIdx: Int): Bool = typeIdx match {
      case `rabIntType` => rfWen
      case `rabFpType`  => fpWen
      case `rabVecType` => vecWen
      case `rabV0Type`  => v0Wen
    }

    def collapsedType(info: RabCommitInfo): UInt = {
      Mux(info.rfWen, 0.U, Mux(info.fpWen, 1.U, 2.U))
    }

    val enqTypeCounts = Seq.tabulate(rabTypeCount) { typeIdx =>
      PopCount(io.req.zip(realNeedAlloc).map { case (req, valid) =>
        valid && typeMatch(req.bits.rfWen, req.bits.fpWen, req.bits.vecWen, req.bits.v0Wen, typeIdx)
      })
    }
    val enqIntMoveCount = PopCount(io.req.zip(realNeedAlloc).map { case (req, valid) =>
      valid && req.bits.rfWen && req.bits.isMove
    })

    val deqRemoveMask = (0 until RabCommitWidth).map(i => i.U < deqPtrSteps)
    val deqTypeCounts = Seq.tabulate(rabTypeCount) { typeIdx =>
      PopCount(deqRemoveMask.zip(commitCandidates).map { case (valid, entry) =>
        valid && typeMatch(entry.info.rfWen, entry.info.fpWen, entry.info.vecWen, entry.info.v0Wen, typeIdx)
      })
    }
    val commitIntMoveCount = PopCount(deqRemoveMask.zip(commitCandidates).map { case (valid, entry) =>
      valid && entry.info.rfWen && entry.info.isMove
    })

    val walkKeepMask = (0 until RabCommitWidth).map(i => i.U < walkCount)
    val walkTypeCounts = Seq.tabulate(rabTypeCount) { typeIdx =>
      PopCount(walkKeepMask.zip(walkCandidates).map { case (valid, entry) =>
        valid && typeMatch(entry.info.rfWen, entry.info.fpWen, entry.info.vecWen, entry.info.v0Wen, typeIdx)
      })
    }

    io.req.zip(realNeedAlloc).foreach { case (req, valid) =>
      when(valid) {
        assert(PopCount(VecInit(req.bits.rfWen, req.bits.fpWen, req.bits.vecWen, req.bits.v0Wen)) === 1.U)
      }
    }
    deqRemoveMask.zip(commitCandidates).foreach { case (valid, entry) =>
      when(valid) {
        assert(PopCount(VecInit(entry.info.rfWen, entry.info.fpWen, entry.info.vecWen, entry.info.v0Wen)) === 1.U)
      }
    }
    walkKeepMask.zip(walkCandidates).foreach { case (valid, entry) =>
      when(valid) {
        assert(PopCount(VecInit(entry.info.rfWen, entry.info.fpWen, entry.info.vecWen, entry.info.v0Wen)) === 1.U)
      }
    }
    assert(enqTypeCounts.reduce(_ +& _) === enqCount)
    assert(deqTypeCounts.reduce(_ +& _) === deqPtrSteps)
    assert(walkTypeCounts.reduce(_ +& _) === walkCount)

    // Prefix counters mirror the logical RAB stream. Snapshot recovery rolls the
    // enqueue prefix back in lockstep with enqPtr without scanning all entries.
    val enqTypePrefix = RegInit(VecInit.fill(rabTypeCount)(0.U(typePrefixWidth.W)))
      .suggestName("rabEnqTypePrefix")
    val deqTypePrefix = RegInit(VecInit.fill(rabTypeCount)(0.U(typePrefixWidth.W)))
      .suggestName("rabDeqTypePrefix")
    val recoveryTypePrefix = RegInit(VecInit.fill(rabTypeCount)(0.U(typePrefixWidth.W)))
      .suggestName("rabRecoveryTypePrefix")
    val enqTypePrefixSnapshots = SnapshotGenerator(
      enqTypePrefix,
      io.snpt.snptEnq,
      io.snpt.snptDeq,
      io.redirect.valid,
      io.snpt.flushVec
    )

    val enqTypePrefixAfterAlloc = Wire(Vec(rabTypeCount, UInt(typePrefixWidth.W)))
    val deqTypePrefixNext = Wire(Vec(rabTypeCount, UInt(typePrefixWidth.W)))
    val recoveryTypePrefixAfterWalk = Wire(Vec(rabTypeCount, UInt(typePrefixWidth.W)))
    for (typeIdx <- 0 until rabTypeCount) {
      enqTypePrefixAfterAlloc(typeIdx) := enqTypePrefix(typeIdx) + enqTypeCounts(typeIdx)
      deqTypePrefixNext(typeIdx) := deqTypePrefix(typeIdx) + deqTypeCounts(typeIdx)
      recoveryTypePrefixAfterWalk(typeIdx) := recoveryTypePrefix(typeIdx) + walkTypeCounts(typeIdx)
    }

    val recoveryTypePrefixNext = WireDefault(recoveryTypePrefix)
    when(io.redirect.valid) {
      when(io.snpt.useSnpt) {
        recoveryTypePrefixNext := enqTypePrefixSnapshots(snptSelect)
      }
    }.elsewhen(state === s_special_walk && stateNext === s_walk) {
      recoveryTypePrefixNext := deqTypePrefixNext
    }.elsewhen(state === s_walk) {
      recoveryTypePrefixNext := recoveryTypePrefixAfterWalk
    }

    val enqTypePrefixNext = WireDefault(enqTypePrefixAfterAlloc)
    when(state === s_walk && stateNext === s_idle) {
      enqTypePrefixNext := recoveryTypePrefixNext
    }

    enqTypePrefix := enqTypePrefixNext
    deqTypePrefix := deqTypePrefixNext
    recoveryTypePrefix := recoveryTypePrefixNext

    val typeOccupancies = VecInit((0 until rabTypeCount).map { typeIdx =>
      (enqTypePrefix(typeIdx) - deqTypePrefix(typeIdx))(typePrefixWidth - 1, 0)
    })
    val occupancySampleValid = state === s_idle
    when(occupancySampleValid) {
      assert(typeOccupancies.reduce(_ +& _) === numValidEntries)
      typeOccupancies.foreach(occupancy => assert(occupancy <= size.U))
    }

    XSPerfAccumulate("rab_enq_total", enqCount)
    XSPerfAccumulate("rab_enq_int", enqTypeCounts(rabIntType))
    XSPerfAccumulate("rab_enq_fp", enqTypeCounts(rabFpType))
    XSPerfAccumulate("rab_enq_vec", enqTypeCounts(rabVecType))
    XSPerfAccumulate("rab_enq_v0", enqTypeCounts(rabV0Type))
    XSPerfAccumulate("rab_enq_int_move", enqIntMoveCount)

    XSPerfAccumulate("rab_commit_total", deqPtrSteps)
    XSPerfAccumulate("rab_commit_int", deqTypeCounts(rabIntType))
    XSPerfAccumulate("rab_commit_fp", deqTypeCounts(rabFpType))
    XSPerfAccumulate("rab_commit_vec", deqTypeCounts(rabVecType))
    XSPerfAccumulate("rab_commit_v0", deqTypeCounts(rabV0Type))
    XSPerfAccumulate("rab_commit_int_move", commitIntMoveCount)

    XSPerfAccumulate(
      "rab_occ_total_entry_cycles",
      Mux(occupancySampleValid, numValidEntries, 0.U)
    )
    Seq("int", "fp", "vec", "v0").zipWithIndex.foreach { case (typeName, typeIdx) =>
      XSPerfAccumulate(
        s"rab_occ_${typeName}_entry_cycles",
        Mux(occupancySampleValid, typeOccupancies(typeIdx), 0.U)
      )
      XSPerfMax(s"rab_occ_${typeName}", typeOccupancies(typeIdx), occupancySampleValid)
    }

    val occupancyHistStep = 16
    val occupancyHistStop = (size / occupancyHistStep + 1) * occupancyHistStep
    Seq("int", "fp", "vec", "v0").zipWithIndex.foreach { case (typeName, typeIdx) =>
      XSPerfHistogram(
        s"rab_occ_${typeName}_hist_step16",
        typeOccupancies(typeIdx),
        occupancySampleValid,
        0,
        occupancyHistStop,
        occupancyHistStep
      )
    }

    val nearFull = occupancySampleValid && numValidEntries >= (size * 3 / 4).U
    XSPerfAccumulate("rab_nearfull_cycles", nearFull)
    XSPerfAccumulate(
      "rab_nearfull_total_entry_cycles",
      Mux(nearFull, numValidEntries, 0.U)
    )
    Seq("int", "fp", "vec", "v0").zipWithIndex.foreach { case (typeName, typeIdx) =>
      XSPerfAccumulate(
        s"rab_nearfull_${typeName}_entry_cycles",
        Mux(nearFull, typeOccupancies(typeIdx), 0.U)
      )
    }

    val vecAllOccupancy = typeOccupancies(rabVecType) +& typeOccupancies(rabV0Type)
    val totalOccupancyTimesThree = (numValidEntries << 1) +& numValidEntries
    val nearFullIntDominant = nearFull && (typeOccupancies(rabIntType) << 2) >= totalOccupancyTimesThree
    val nearFullFpDominant = nearFull && (typeOccupancies(rabFpType) << 2) >= totalOccupancyTimesThree
    val nearFullVecDominant = nearFull && (vecAllOccupancy << 2) >= totalOccupancyTimesThree
    val nearFullMixed = nearFull && !nearFullIntDominant && !nearFullFpDominant && !nearFullVecDominant
    when(nearFull) {
      assert(PopCount(VecInit(nearFullIntDominant, nearFullFpDominant, nearFullVecDominant)) <= 1.U)
    }
    XSPerfAccumulate("rab_nearfull_dominant_int_cycles", nearFullIntDominant)
    XSPerfAccumulate("rab_nearfull_dominant_fp_cycles", nearFullFpDominant)
    XSPerfAccumulate("rab_nearfull_dominant_vec_cycles", nearFullVecDominant)
    XSPerfAccumulate("rab_nearfull_dominant_mixed_cycles", nearFullMixed)

    val localWindowSize = 32
    val localWindowPositionWidth = log2Ceil(localWindowSize)
    val localWindowCountWidth = log2Ceil(localWindowSize + 1)
    require(localWindowSize >= RabCommitWidth)

    val windowPosition = RegInit(0.U(localWindowPositionWidth.W))
    val windowTypeCounts = RegInit(VecInit.fill(rabTypeCount)(0.U(localWindowCountWidth.W)))
    val collapsedTypeWidth = log2Ceil(3)
    val lastCommittedTypeValid = RegInit(false.B)
    val lastCommittedType = RegInit(0.U(collapsedTypeWidth.W))
    val transitionIncrementWidth = log2Ceil(RabCommitWidth + 1)

    val runLengthOverflow = 33
    val runLengthWidth = log2Ceil(runLengthOverflow + 1)
    val runTypeValid = RegInit(false.B)
    val runType = RegInit(0.U(collapsedTypeWidth.W))
    val runLength = RegInit(0.U(runLengthWidth.W))
    val runBucketNames = Seq("1", "2", "3_4", "5_8", "9_16", "17_32", "gt32")
    val runIncrementWidth = log2Ceil(RabCommitWidth + 1)

    assert(windowTypeCounts.reduce(_ +& _) === windowPosition)
    assert(runTypeValid === lastCommittedTypeValid)
    when(runTypeValid) {
      assert(runType === lastCommittedType)
    }

    var nextWindowPosition: UInt = windowPosition
    var nextWindowTypeCounts: Seq[UInt] = windowTypeCounts.toSeq
    var windowClose: Bool = false.B
    var closedWindowTypeCounts: Seq[UInt] = Seq.fill(rabTypeCount)(0.U(localWindowCountWidth.W))
    var nextLastCommittedTypeValid: Bool = lastCommittedTypeValid
    var nextLastCommittedType: UInt = lastCommittedType
    var transitionIncrements: Seq[UInt] = Seq.fill(9)(0.U(transitionIncrementWidth.W))
    var nextRunTypeValid: Bool = runTypeValid
    var nextRunType: UInt = runType
    var nextRunLength: UInt = runLength
    var runBucketIncrements: Seq[UInt] = Seq.fill(3 * runBucketNames.length)(0.U(runIncrementWidth.W))

    for (lane <- 0 until RabCommitWidth) {
      val entryValid = deqRemoveMask(lane)
      val entryInfo = commitCandidates(lane).info
      val entryTypeHits = Seq.tabulate(rabTypeCount)(typeIdx =>
        typeMatch(entryInfo.rfWen, entryInfo.fpWen, entryInfo.vecWen, entryInfo.v0Wen, typeIdx)
      )
      val entryCollapsedType = collapsedType(entryInfo)

      val incrementedWindowCounts = nextWindowTypeCounts.zip(entryTypeHits).map { case (count, hit) =>
        count + hit.asUInt
      }
      val closesWindow = entryValid && nextWindowPosition === (localWindowSize - 1).U
      when(closesWindow) {
        assert(!windowClose)
      }
      closedWindowTypeCounts = closedWindowTypeCounts.zip(incrementedWindowCounts).map {
        case (sample, incremented) => Mux(closesWindow, incremented, sample)
      }
      nextWindowTypeCounts = nextWindowTypeCounts.zip(incrementedWindowCounts).map {
        case (count, incremented) =>
          Mux(entryValid, Mux(closesWindow, 0.U(localWindowCountWidth.W), incremented), count)
      }
      nextWindowPosition = Mux(
        entryValid,
        Mux(closesWindow, 0.U, nextWindowPosition + 1.U),
        nextWindowPosition
      )
      windowClose = windowClose || closesWindow

      transitionIncrements = transitionIncrements.zipWithIndex.map { case (count, transitionIdx) =>
        val fromType = transitionIdx / 3
        val toType = transitionIdx % 3
        val transitionHit = entryValid && nextLastCommittedTypeValid &&
          nextLastCommittedType === fromType.U && entryCollapsedType === toType.U
        count + transitionHit.asUInt
      }
      nextLastCommittedType = Mux(entryValid, entryCollapsedType, nextLastCommittedType)
      nextLastCommittedTypeValid = nextLastCommittedTypeValid || entryValid

      val sameRunType = nextRunTypeValid && nextRunType === entryCollapsedType
      val runEnds = entryValid && nextRunTypeValid && !sameRunType
      val runBucketMatches = Seq(
        nextRunLength === 1.U,
        nextRunLength === 2.U,
        nextRunLength >= 3.U && nextRunLength <= 4.U,
        nextRunLength >= 5.U && nextRunLength <= 8.U,
        nextRunLength >= 9.U && nextRunLength <= 16.U,
        nextRunLength >= 17.U && nextRunLength <= 32.U,
        nextRunLength >= runLengthOverflow.U
      )
      runBucketIncrements = runBucketIncrements.zipWithIndex.map { case (count, bucketIdx) =>
        val typeIdx = bucketIdx / runBucketNames.length
        val lengthBucketIdx = bucketIdx % runBucketNames.length
        val bucketHit = runEnds && nextRunType === typeIdx.U && runBucketMatches(lengthBucketIdx)
        count + bucketHit.asUInt
      }
      val incrementedRunLength = Mux(
        nextRunLength < runLengthOverflow.U,
        nextRunLength + 1.U,
        nextRunLength
      )
      nextRunLength = Mux(entryValid, Mux(sameRunType, incrementedRunLength, 1.U), nextRunLength)
      nextRunType = Mux(entryValid, entryCollapsedType, nextRunType)
      nextRunTypeValid = nextRunTypeValid || entryValid
    }

    windowPosition := nextWindowPosition
    windowTypeCounts.zip(nextWindowTypeCounts).foreach { case (reg, next) => reg := next }
    lastCommittedTypeValid := nextLastCommittedTypeValid
    lastCommittedType := nextLastCommittedType
    runTypeValid := nextRunTypeValid
    runType := nextRunType
    runLength := nextRunLength

    when(windowClose) {
      assert(closedWindowTypeCounts.reduce(_ +& _) === localWindowSize.U)
    }

    val transitionCount = transitionIncrements.reduce(_ +& _)
    val expectedTransitionCount = Mux(
      deqPtrSteps === 0.U,
      0.U,
      deqPtrSteps - !lastCommittedTypeValid
    )
    assert(transitionCount === expectedTransitionCount)

    val typeChangeCount = transitionIncrements.zipWithIndex.collect {
      case (increment, transitionIdx) if transitionIdx / 3 != transitionIdx % 3 => increment
    }.reduce(_ +& _)
    assert(runBucketIncrements.reduce(_ +& _) === typeChangeCount)

    val closedWindowVecAll = closedWindowTypeCounts(rabVecType) +& closedWindowTypeCounts(rabV0Type)
    val windowDominantThreshold = localWindowSize * 3 / 4
    val windowIntDominant = windowClose && closedWindowTypeCounts(rabIntType) >= windowDominantThreshold.U
    val windowFpDominant = windowClose && closedWindowTypeCounts(rabFpType) >= windowDominantThreshold.U
    val windowVecDominant = windowClose && closedWindowVecAll >= windowDominantThreshold.U
    val windowMixed = windowClose && !windowIntDominant && !windowFpDominant && !windowVecDominant
    when(windowClose) {
      assert(PopCount(VecInit(windowIntDominant, windowFpDominant, windowVecDominant)) <= 1.U)
    }

    val localWindowHistStop = localWindowSize + 4
    XSPerfHistogram(
      "rab_commit_win32_int_hist",
      closedWindowTypeCounts(rabIntType),
      windowClose,
      0,
      localWindowHistStop,
      4
    )
    XSPerfHistogram(
      "rab_commit_win32_fp_hist",
      closedWindowTypeCounts(rabFpType),
      windowClose,
      0,
      localWindowHistStop,
      4
    )
    XSPerfHistogram(
      "rab_commit_win32_vec_all_hist",
      closedWindowVecAll,
      windowClose,
      0,
      localWindowHistStop,
      4
    )
    XSPerfAccumulate("rab_commit_win32_dominant_int", windowIntDominant)
    XSPerfAccumulate("rab_commit_win32_dominant_fp", windowFpDominant)
    XSPerfAccumulate("rab_commit_win32_dominant_vec", windowVecDominant)
    XSPerfAccumulate("rab_commit_win32_mixed", windowMixed)

    val collapsedTypeNames = Seq("i", "f", "v")
    transitionIncrements.zipWithIndex.foreach { case (increment, transitionIdx) =>
      val fromTypeName = collapsedTypeNames(transitionIdx / 3)
      val toTypeName = collapsedTypeNames(transitionIdx % 3)
      XSPerfAccumulate(s"rab_commit_trans_${fromTypeName}_${toTypeName}", increment)
    }

    runBucketIncrements.zipWithIndex.foreach { case (increment, bucketIdx) =>
      val typeName = Seq("int", "fp", "vec")(bucketIdx / runBucketNames.length)
      val bucketName = runBucketNames(bucketIdx % runBucketNames.length)
      XSPerfAccumulate(s"rab_commit_run_${typeName}_${bucketName}", increment)
    }

    val rollingGranularity = 256
    XSPerfRolling(
      "rab_commit_rolling_int",
      deqTypeCounts(rabIntType),
      eventTrigger = deqPtrSteps,
      granularity = rollingGranularity,
      clock,
      reset
    )
    XSPerfRolling(
      "rab_commit_rolling_fp",
      deqTypeCounts(rabFpType),
      eventTrigger = deqPtrSteps,
      granularity = rollingGranularity,
      clock,
      reset
    )
    XSPerfRolling(
      "rab_commit_rolling_vec",
      deqTypeCounts(rabVecType),
      eventTrigger = deqPtrSteps,
      granularity = rollingGranularity,
      clock,
      reset
    )
    XSPerfRolling(
      "rab_commit_rolling_v0",
      deqTypeCounts(rabV0Type),
      eventTrigger = deqPtrSteps,
      granularity = rollingGranularity,
      clock,
      reset
    )
  }

  io.canEnq := allowEnqueue && state === s_idle
  io.canEnqForDispatch := allowEnqueueForDispatch && state === s_idle
  io.enqPtrVec := enqPtrVec

  io.status.walkEnd := walkEndNext
  io.status.commitEnd := commitEndNext

  for (i <- 0 until RabCommitWidth) {
    val valid = (state === s_special_walk) && vecLoadExcp.valid && io.commits.commitValid(i)
    io.toVecExcpMod.logicPhyRegMap(i).valid := RegNext(valid)
    io.toVecExcpMod.logicPhyRegMap(i).bits match {
      case x =>
        x.lreg := RegEnable(io.commits.info(i).ldest, valid)
        x.preg := RegEnable(io.commits.info(i).pdest, valid)
    }
  }

  // for difftest
  io.diffCommits.foreach(_ := 0.U.asTypeOf(new DiffCommitIO))
  io.diffCommits.foreach(_.isCommit := true.B)
  for(i <- 0 until RabCommitWidth * MaxUopSize) {
    io.diffCommits.foreach(_.commitValid(i) := i.U < newCommitSize)
    io.diffCommits.foreach(_.info(i) := renameBufferEntries((diffPtr + i.U).value).info)
  }

  XSError(isBefore(enqPtr, deqPtr) && !isFull(enqPtr, deqPtr), "\ndeqPtr is older than enqPtr!\n")

  QueuePerf(RabSize, numValidEntries, numValidEntries === size.U)

  if (backendParams.debugEn) {
    dontTouch(deqPtrVec)
    dontTouch(walkPtrNext)
    dontTouch(walkSizeNxt)
    dontTouch(walkEndNext)
    dontTouch(walkEndNextCycle)
  }

  XSPerfAccumulate("s_idle_to_idle", state === s_idle         && stateNext === s_idle)
  XSPerfAccumulate("s_idle_to_swlk", state === s_idle         && stateNext === s_special_walk)
  XSPerfAccumulate("s_idle_to_walk", state === s_idle         && stateNext === s_walk)
  XSPerfAccumulate("s_swlk_to_idle", state === s_special_walk && stateNext === s_idle)
  XSPerfAccumulate("s_swlk_to_swlk", state === s_special_walk && stateNext === s_special_walk)
  XSPerfAccumulate("s_swlk_to_walk", state === s_special_walk && stateNext === s_walk)
  XSPerfAccumulate("s_walk_to_idle", state === s_walk         && stateNext === s_idle)
  XSPerfAccumulate("s_walk_to_swlk", state === s_walk         && stateNext === s_special_walk)
  XSPerfAccumulate("s_walk_to_walk", state === s_walk         && stateNext === s_walk)

  XSPerfAccumulate("disallow_enq_cycle", !allowEnqueue)
  XSPerfAccumulate("disallow_enq_full_cycle", numValidEntries + enqCount > (size - RenameWidth).U)
  XSPerfAccumulate("disallow_enq_not_idle_cycle", state =/= s_idle)
}
