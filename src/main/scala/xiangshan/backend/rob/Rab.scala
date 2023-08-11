package xiangshan.backend.rob

import chipsalliance.rocketchip.config.Parameters
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

class RenameBufferEntry(implicit p: Parameters) extends RobCommitInfo {
  val robIdx = new RobPtr
}

class RenameBuffer(size: Int)(implicit p: Parameters) extends XSModule with HasCircularQueuePtrHelper {
  val io = IO(new Bundle {
    val redirect = Input(ValidIO(new Bundle {
    }))

    val req = Vec(RenameWidth, Flipped(ValidIO(new DynInst)))

    val walkSize = Input(UInt(log2Up(size).W))
    val robWalkEnd = Input(Bool())
    val commitSize = Input(UInt(log2Up(size).W))
    val snpt = Input(new SnapshotPort)

    val canEnq = Output(Bool())
    val rabWalkEnd = Output(Bool())
    val enqPtrVec = Output(Vec(RenameWidth, new RenameBufferPtr))
    val vconfigPdest = Output(UInt(PhyRegIdxWidth.W))
    val commits = Output(new RobCommitIO)
    val diffCommits = Output(new DiffCommitIO)
  })

  // pointer
  private val enqPtrVec = RegInit(VecInit.tabulate(RenameWidth)(idx => RenameBufferPtr(flag = false, idx)))
  private val enqPtr = enqPtrVec.head
  private val enqPtrOH = RegInit(1.U(size.W))
  private val enqPtrOHShift = CircularShift(enqPtrOH)
  // may shift [0, RenameWidth] steps
  private val enqPtrOHVec = VecInit.tabulate(RenameWidth + 1)(enqPtrOHShift.left)
  private val enqPtrVecNext = Wire(enqPtrVec.cloneType)

  private val deqPtrVec = RegInit(VecInit.tabulate(CommitWidth)(idx => RenameBufferPtr(flag = false, idx)))
  private val deqPtr = deqPtrVec.head
  private val deqPtrOH = RegInit(1.U(size.W))
  private val deqPtrOHShift = CircularShift(deqPtrOH)
  private val deqPtrOHVec = VecInit.tabulate(CommitWidth + 1)(deqPtrOHShift.left)
  private val deqPtrVecNext = Wire(deqPtrVec.cloneType)
  XSError(deqPtr.toOH =/= deqPtrOH, p"wrong one-hot reg between $deqPtr and $deqPtrOH")

  private val walkPtr = Reg(new RenameBufferPtr)
  private val walkPtrOH = walkPtr.toOH
  private val walkPtrOHVec = VecInit.tabulate(CommitWidth + 1)(CircularShift(walkPtrOH).left)
  private val walkPtrNext = Wire(new RenameBufferPtr)

  private val snptEnq = io.canEnq && io.req.head.valid && io.req.head.bits.snapshot
  private val walkPtrSnapshots = SnapshotGenerator(enqPtr, snptEnq, io.snpt.snptDeq, io.redirect.valid)

  // may shift [0, CommitWidth] steps
  val headPtrOHVec2 = VecInit(Seq.tabulate(CommitWidth * MaxUopSize + 1)(_ % size).map(step => deqPtrOHShift.left(step)))

  val vcfgPtrOH = RegInit(1.U(size.W))
  val vcfgPtrOHShift = CircularShift(vcfgPtrOH)
  // may shift [0, 2) steps
  val vcfgPtrOHVec = VecInit.tabulate(2)(vcfgPtrOHShift.left)

  val diffPtrOH = RegInit(1.U(size.W))
  val diffPtrOHShift = CircularShift(diffPtrOH)
  // may shift [0, CommitWidth * MaxUopSize] steps
  val diffPtrOHVec = VecInit(Seq.tabulate(CommitWidth * MaxUopSize + 1)(_ % size).map(step => diffPtrOHShift.left(step)))

  // Regs
  val renameBuffer = RegInit(VecInit(Seq.fill(size){0.U.asTypeOf(new RenameBufferEntry)}))

  val s_idle :: s_walk :: s_cancel :: Nil = Enum(3)
  val state = RegInit(s_idle)
  val stateNxt = WireInit(s_idle)

  val robWalkEnd = RegInit(false.B)
  val rabWalkEndWire = Wire(Bool())

  when(io.redirect.valid){
    robWalkEnd := false.B
  }.elsewhen(io.robWalkEnd){
    robWalkEnd := true.B
  }

  val realNeedAlloc = io.req.map(req => req.valid && req.bits.needWriteRf)
  val enqCount    = PopCount(realNeedAlloc)
  val commitCount = Mux(io.commits.isCommit, PopCount(io.commits.commitValid), 0.U)
  val walkCount   = Mux(io.commits.isWalk, PopCount(io.commits.walkValid), 0.U)

  walkPtrNext := Mux(
    io.redirect.valid,
    Mux(
      io.snpt.useSnpt,
      walkPtrSnapshots(io.snpt.snptSelect),
      deqPtrVecNext.head
    ),
    Mux(
      state === s_walk,
      walkPtr + walkCount,
      walkPtr
    )
  )

  walkPtr := walkPtrNext

  // number of pair(ldest, pdest) ready to commit to arch_rat
  val commitSize = RegInit(0.U(log2Up(size).W))
  val walkSize = RegInit(0.U(log2Up(size).W))

  val commitSizeNxt = commitSize + io.commitSize - commitCount
  val walkSizeNxt = walkSize + io.walkSize - walkCount

  commitSize := commitSizeNxt
  walkSize := Mux(io.redirect.valid, 0.U, walkSizeNxt)

  val walkCandidates   = VecInit(walkPtrOHVec.map(sel => Mux1H(sel, renameBuffer)))
  val commitCandidates = VecInit(deqPtrOHVec.map(sel => Mux1H(sel, renameBuffer)))
  val vcfgCandidates   = VecInit(vcfgPtrOHVec.map(sel => Mux1H(sel, renameBuffer)))
  val diffCandidates   = VecInit(diffPtrOHVec.map(sel => Mux1H(sel, renameBuffer)))

  // update diff pointer
  val diffPtrOHNext = Mux(state === s_idle, diffPtrOHVec(io.commitSize), diffPtrOH)
  diffPtrOH := diffPtrOHNext

  // update vcfg pointer
  vcfgPtrOH := diffPtrOHNext

  // update enq pointer
  val enqPtrNext = Mux(
    state === s_walk && stateNxt === s_idle,
    walkPtrNext,
    enqPtr + enqCount
  )
  val enqPtrOHNext = Mux(
    state === s_walk && stateNxt === s_idle,
    walkPtrNext.toOH,
    enqPtrOHVec(enqCount)
  )
  enqPtr := enqPtrNext
  enqPtrOH := enqPtrOHNext
  enqPtrVecNext.zipWithIndex.map{ case(ptr, i) => ptr := enqPtrNext + i.U }
  enqPtrVec := enqPtrVecNext

  // update deq pointer
  val deqPtrNext = deqPtr + commitCount
  val deqPtrOHNext = deqPtrOHVec(commitCount)
  deqPtr := deqPtrNext
  deqPtrOH := deqPtrOHNext
  deqPtrVecNext.zipWithIndex.map{ case(ptr, i) => ptr := deqPtrNext + i.U }
  deqPtrVec := deqPtrVecNext

  val allocatePtrVec = VecInit((0 until RenameWidth).map(i => enqPtrVec(PopCount(realNeedAlloc.take(i))).value))
  allocatePtrVec.zip(io.req).zip(realNeedAlloc).map{ case((allocatePtr, req), realNeedAlloc) =>
    when(realNeedAlloc){
      renameBuffer(allocatePtr).ldest := req.bits.ldest
      renameBuffer(allocatePtr).pdest := req.bits.pdest
      renameBuffer(allocatePtr).rfWen := req.bits.rfWen
      renameBuffer(allocatePtr).fpWen := req.bits.fpWen
      renameBuffer(allocatePtr).vecWen := req.bits.vecWen
      renameBuffer(allocatePtr).isMove := req.bits.eliminatedMove
      renameBuffer(allocatePtr).robIdx := req.bits.robIdx
    }
  }

  io.commits.isCommit := state === s_idle
  io.commits.isWalk := state === s_walk

  for(i <- 0 until CommitWidth) {
    io.commits.commitValid(i) := state === s_idle && i.U < commitSize
    io.commits.walkValid(i) := state === s_walk && i.U < walkSize
    io.commits.info(i) := Mux(state === s_idle, commitCandidates(i), walkCandidates(i))
    io.commits.robIdx(i) := Mux(state === s_idle, commitCandidates(i).robIdx, walkCandidates(i).robIdx)
  }

  stateNxt := Mux(io.redirect.valid, s_walk,
    Mux(io.rabWalkEnd, s_idle, state))
  state := stateNxt

  rabWalkEndWire := robWalkEnd && state === s_walk && walkSizeNxt === 0.U
  io.rabWalkEnd := rabWalkEndWire

  val allowEnqueue = RegInit(true.B)
  val numValidEntries = distanceBetween(enqPtr, deqPtr)
  allowEnqueue := numValidEntries + enqCount <= (size - RenameWidth).U
  io.canEnq := allowEnqueue
  io.enqPtrVec := enqPtrVec

  io.vconfigPdest := Mux(vcfgCandidates(0).ldest === VCONFIG_IDX.U && vcfgCandidates(0).vecWen, vcfgCandidates(0).pdest, vcfgCandidates(1).pdest)

  // for difftest
  io.diffCommits := 0.U.asTypeOf(new DiffCommitIO)
  io.diffCommits.isCommit := state === s_idle
  for(i <- 0 until CommitWidth * MaxUopSize) {
    io.diffCommits.commitValid(i) := state === s_idle && i.U < io.commitSize
    io.diffCommits.info(i) := diffCandidates(i)
  }

  XSError(isBefore(enqPtr, deqPtr) && !isFull(enqPtr, deqPtr), "\ndeqPtr is older than enqPtr!\n")

  QueuePerf(RabSize, numValidEntries, !allowEnqueue)
}
