package xiangshan.backend.rob

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import utility._
import xiangshan.backend.Bundles.DynInst
import xiangshan.backend.decode.VectorConstants
import xiangshan.backend.rename.SnapshotGenerator

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

    val req = Vec(RenameWidth, Flipped(ValidIO(new DynInst)))
    val fromRob = new Bundle {
      val walkSize = Input(UInt(log2Up(size).W))
      val walkEnd = Input(Bool())
      val commitSize = Input(UInt(log2Up(size).W))
    }

    val snpt = Input(new SnapshotPort)

    val canEnq = Output(Bool())
    val enqPtrVec = Output(Vec(RenameWidth, new RenameBufferPtr))

    val commits = Output(new RabCommitIO)
    val diffCommits = if (backendParams.debugEn) Some(Output(new DiffCommitIO)) else None

    val status = Output(new Bundle {
      val walkEnd = Bool()
    })
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
  private val walkPtrOH = walkPtr.toOH
  private val walkPtrOHVec = VecInit.tabulate(RabCommitWidth + 1)(CircularShift(walkPtrOH).left)
  private val walkPtrNext = Wire(new RenameBufferPtr)

  private val walkPtrSnapshots = SnapshotGenerator(enqPtr, io.snpt.snptEnq, io.snpt.snptDeq, io.redirect.valid, io.snpt.flushVec)

  val vcfgPtrOH = RegInit(1.U(size.W))
  val vcfgPtrOHShift = CircularShift(vcfgPtrOH)
  // may shift [0, 2) steps
  val vcfgPtrOHVec = VecInit.tabulate(2)(vcfgPtrOHShift.left)

  val diffPtr = RegInit(0.U.asTypeOf(new RenameBufferPtr))
  val diffPtrNext = Wire(new RenameBufferPtr)
  // Regs
  val renameBuffer = Reg(Vec(size, new RenameBufferEntry))
  val renameBufferEntries = VecInit((0 until size) map (i => renameBuffer(i)))

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

  val realNeedAlloc = io.req.map(req => req.valid && req.bits.needWriteRf)
  val enqCount    = PopCount(realNeedAlloc)
  val commitCount = Mux(io.commits.isCommit && !io.commits.isWalk, PopCount(io.commits.commitValid), 0.U)
  val walkCount   = Mux(io.commits.isWalk && !io.commits.isCommit, PopCount(io.commits.walkValid), 0.U)
  val specialWalkCount = Mux(io.commits.isCommit && io.commits.isWalk, PopCount(io.commits.walkValid), 0.U)

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

  val walkCandidates   = VecInit(walkPtrOHVec.map(sel => Mux1H(sel, renameBufferEntries)))
  val commitCandidates = VecInit(deqPtrOHVec.map(sel => Mux1H(sel, renameBufferEntries)))
  val vcfgCandidates   = VecInit(vcfgPtrOHVec.map(sel => Mux1H(sel, renameBufferEntries)))

  // update diff pointer
  diffPtrNext := Mux(state === s_idle, diffPtr + newCommitSize, diffPtr)
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
  private val specialWalkEndNext = specialWalkSizeNext === 0.U

  // change state
  state := stateNext
  when(io.redirect.valid) {
    when(io.snpt.useSnpt) {
      stateNext := s_walk
    }.otherwise {
      stateNext := s_special_walk
    }
  }.otherwise {
    // change stateNext
    switch(state) {
      // this transaction is not used actually, just list all states
      is(s_idle) {
        stateNext := s_idle
      }
      is(s_special_walk) {
        when(specialWalkEndNext) {
          stateNext := s_walk
        }
      }
      is(s_walk) {
        when(robWalkEnd && walkEndNext) {
          stateNext := s_idle
        }
      }
    }
  }

  val numValidEntries = distanceBetween(enqPtr, deqPtr)
  val allowEnqueue = GatedValidRegNext(numValidEntries + enqCount <= (size - RenameWidth).U, true.B)

  io.canEnq := allowEnqueue && state === s_idle
  io.enqPtrVec := enqPtrVec

  io.status.walkEnd := walkEndNext

  // for difftest
  io.diffCommits.foreach(_ := 0.U.asTypeOf(new DiffCommitIO))
  io.diffCommits.foreach(_.isCommit := state === s_idle || state === s_special_walk)
  for(i <- 0 until RabCommitWidth * MaxUopSize) {
    io.diffCommits.foreach(_.commitValid(i) := (state === s_idle || state === s_special_walk) && i.U < newCommitSize)
    io.diffCommits.foreach(_.info(i) := renameBufferEntries((diffPtr + i.U).value).info)
  }

  XSError(isBefore(enqPtr, deqPtr) && !isFull(enqPtr, deqPtr), "\ndeqPtr is older than enqPtr!\n")

  QueuePerf(RabSize, numValidEntries, numValidEntries === size.U)

  if (backendParams.debugEn) {
    dontTouch(deqPtrVec)
    dontTouch(walkPtrNext)
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
