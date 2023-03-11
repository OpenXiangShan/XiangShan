package xiangshan.backend.rob

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import utility._

class RenameBuffer(size: Int)(implicit p: Parameters) extends XSModule with HasCircularQueuePtrHelper {
  val io = IO(new Bundle {
    val redirectValid = Input(Bool())

    val req = Vec(RenameWidth, Flipped(ValidIO(new MicroOp)))
    val canEnq = Output(Bool())

    val walkSize = Input(UInt(log2Up(size).W))
    val robWalkEnd = Input(Bool())
    val commitSize = Input(UInt(log2Up(size).W))
    val rabWalkEnd = Output(Bool())

    val commits = Output(new RobCommitIO)
    val diffCommits = Output(new DiffCommitIO)
  })

  class RenameBufferPtr extends CircularQueuePtr[RenameBufferPtr](size)

  object RenameBufferPtr {
    def apply(f: Boolean, v: Int): RenameBufferPtr = {
      val ptr = Wire(new RenameBufferPtr)
      ptr.flag := f.B
      ptr.value := v.U
      ptr
    }
  }

  val s_idle :: s_walk :: s_cancel :: Nil = Enum(3)
  val state = RegInit(s_idle)
  val stateNxt = WireInit(s_idle)

  val robWalkEnd = RegInit(false.B)

  when(io.redirectValid){
    robWalkEnd := false.B
  }.elsewhen(io.robWalkEnd){
    robWalkEnd := true.B
  }

  val renameBuffer = RegInit(VecInit(Seq.fill(size){0.U.asTypeOf(new RobCommitInfo)}))

  // pointer
  // tail: enq and walk
  // head: commit to arch rat
  // diff: commit to diff rat
  val headPtr = RegInit(RenameBufferPtr(false, 0))
  val headPtrOH = RegInit(1.U(size.W))
  XSError(headPtr.toOH =/= headPtrOH, p"wrong one-hot reg between $headPtr and $headPtrOH")
  val headPtrOHShift = CircularShift(headPtrOH)
  // may shift [0, CommitWidth] steps
  val headPtrOHVec = VecInit.tabulate(CommitWidth + 1)(headPtrOHShift.left)
  val headPtrOHVec2 = VecInit.tabulate(CommitWidth * MaxUopSize + 1)(headPtrOHShift.left)

  val diffPtrOH = RegInit(1.U(size.W))
  val diffPtrOHShift = CircularShift(diffPtrOH)
  // may shift [0, CommitWidth * MaxUopSize] steps
  val diffPtrOHVec = VecInit.tabulate(CommitWidth * MaxUopSize + 1)(diffPtrOHShift.left)

  val tailPtr = RegInit(RenameBufferPtr(false, 0))
  val tailPtrOH = RegInit(1.U(size.W))
  val tailPtrOHShift = CircularShift(tailPtrOH)
  // may shift [0, RenameWidth] steps
  val tailPtrOHVec = VecInit.tabulate(RenameWidth + 1)(tailPtrOHShift.left)
  val tailPtrVec = RegInit(VecInit.tabulate(RenameWidth)(_.U.asTypeOf(new RenameBufferPtr)))

  val realNeedAlloc = io.req.map(req => req.valid && req.bits.ctrl.needWriteRf)
  val enqCount    = PopCount(realNeedAlloc)
  val commitCount = PopCount(io.commits.commitValid)
  val walkCount   = PopCount(io.commits.walkValid)

  // number of pair(ldest, pdest) ready to commit to arch_rat
  val commitSize = RegInit(0.U(log2Up(size).W))
  val commitSizeNxt = Mux(state === s_idle, commitSize - Mux(io.commits.isCommit, commitCount, 0.U) + io.commitSize, commitSize)
  commitSize := commitSizeNxt
  val walkSize = RegInit(0.U(log2Up(size).W))
  val walkSizeNxt = walkSize + io.walkSize - walkCount
  walkSize := Mux(io.redirectValid, commitSizeNxt, walkSizeNxt)

  val rabWalkEnd = robWalkEnd === true.B && walkSizeNxt === 0.U

  val walkCandidates   = VecInit(tailPtrOHVec.map(sel => Mux1H(sel, renameBuffer)))
  val commitCandidates = VecInit(headPtrOHVec.map(sel => Mux1H(sel, renameBuffer)))
  val diffCandidates   = VecInit(diffPtrOHVec.map(sel => Mux1H(sel, renameBuffer)))

  // update head pointer
  val headPtrNext =  headPtr + commitCount
  val headPtrOHNext = headPtrOHVec(commitCount)
  headPtr := headPtrNext
  headPtrOH := headPtrOHNext
  // update diff pointer
  val diffPtrOHNext = Mux(state === s_idle, diffPtrOHVec(io.commitSize), diffPtrOH)
  diffPtrOH := diffPtrOHNext
  // update tail pointer
  val tailPtrEnqNext = tailPtr + PopCount(realNeedAlloc)
  val tailPtrWalkNext = tailPtr + walkCount
  val tailPtrNext = Mux(io.redirectValid, headPtrNext,
                      Mux(io.commits.isWalk, tailPtrWalkNext, tailPtrEnqNext))
  val tailPtrOHNext = Mux(io.redirectValid, headPtrOHVec2(commitCount),
                        Mux(io.commits.isWalk, tailPtrOHVec(walkCount), tailPtrOHVec(PopCount(realNeedAlloc))))
  tailPtr := tailPtrNext
  tailPtrOH := tailPtrOHNext
  tailPtrVec.zipWithIndex.map{ case(ptr, i) => ptr := tailPtrNext + i.U}

  val allocatePtrVec = VecInit((0 until RenameWidth).map(i => tailPtrVec(PopCount(realNeedAlloc.take(i))).value))
  allocatePtrVec.zip(io.req).zip(realNeedAlloc).map{ case((allocatePtr, req), realNeedAlloc) =>
    when(realNeedAlloc){
      renameBuffer(allocatePtr).ldest := req.bits.ctrl.ldest
      renameBuffer(allocatePtr).pdest := req.bits.pdest
      renameBuffer(allocatePtr).old_pdest := req.bits.old_pdest
      renameBuffer(allocatePtr).rfWen := req.bits.ctrl.rfWen
      renameBuffer(allocatePtr).fpWen := req.bits.ctrl.fpWen
      renameBuffer(allocatePtr).vecWen := req.bits.ctrl.vecWen
      renameBuffer(allocatePtr).isMove := req.bits.eliminatedMove
    }
  }

  io.commits.isCommit := state === s_idle
  io.commits.isWalk := state === s_walk

  for(i <- 0 until CommitWidth) {
    io.commits.commitValid(i) := state === s_idle && i.U < commitSize
    io.commits.walkValid(i) := state === s_walk && i.U < walkSize
    io.commits.info(i) := Mux(state === s_idle, commitCandidates(i), walkCandidates(i))
  }

  stateNxt := Mux(io.redirectValid, s_walk,
                Mux(state === s_walk && rabWalkEnd, s_idle, state))
  state := stateNxt

  io.rabWalkEnd := state === s_walk && rabWalkEnd

  val allowEnqueue = RegInit(true.B)
  val numValidEntries = distanceBetween(tailPtr, headPtr)
  allowEnqueue := numValidEntries + enqCount <= (size - RenameWidth).U
  io.canEnq := allowEnqueue

  // for difftest
  io.diffCommits := 0.U.asTypeOf(new DiffCommitIO)
  io.diffCommits.isCommit := state === s_idle
  for(i <- 0 until CommitWidth * MaxUopSize) {
    io.diffCommits.commitValid(i) := state === s_idle && i.U < io.commitSize
    io.diffCommits.info(i) := diffCandidates(i)
  }

}

