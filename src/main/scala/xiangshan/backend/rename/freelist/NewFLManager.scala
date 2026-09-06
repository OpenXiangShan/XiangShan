package xiangshan.backend.rename.freelist

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.ParallelPriorityEncoder
import xiangshan.{XSBundle, XSModule}

class NewFLManager(
  numPhyRegs: Int,
  renameWidth: Int,
  s1QueueSize: Int = NewFLManager.DefaultS1QueueSize
)(implicit p: Parameters) extends XSModule {
  require(renameWidth >= 2 && renameWidth % 2 == 0,
    s"NewFLManager rename width must be a positive even number, got $renameWidth")
  require(numPhyRegs >= renameWidth / 2,
    s"Physical register count $numPhyRegs must cover ${renameWidth / 2} allocation banks")
  require(s1QueueSize >= renameWidth,
    s"NewFLManager s1 queue size $s1QueueSize must be no smaller than rename width $renameWidth")

  val in = IO(Input(new NewFLManager.In(numPhyRegs, renameWidth)))
  val out = IO(Output(new NewFLManager.Out(numPhyRegs, renameWidth)))

  private val bankCount = renameWidth / 2
  private val phyRegIdxWidth = log2Up(numPhyRegs)
  private val s1PtrWidth = math.max(1, log2Ceil(s1QueueSize))
  private val s1CountWidth = log2Ceil(s1QueueSize + 1)

  /** Stage 1: a configurable circular queue of physical-register candidates. */
  val s1Queue = RegInit(VecInit(Seq.fill(s1QueueSize)(0.U(phyRegIdxWidth.W))))
  val s1HeadPtr = RegInit(0.U(s1PtrWidth.W))
  val s1TailPtr = RegInit(0.U(s1PtrWidth.W))
  val s1ValidCount = RegInit(0.U(s1CountWidth.W))

  private def addS1Ptr(ptr: UInt, increment: UInt): UInt = {
    val sum = ptr +& increment
    Mux(sum >= s1QueueSize.U, sum - s1QueueSize.U, sum)(s1PtrWidth - 1, 0)
  }

  // Candidates in s1 remain free in the owner's bitmap until rename really
  // consumes them, so the manager reserves them locally to prevent reselection.
  val reservedBitmap = RegInit(0.U(numPhyRegs.W))
  val s1FreeCount = s1QueueSize.U(s1CountWidth.W) - s1ValidCount

  /** Stage 0: select up to two candidates from every bank. */
  val s0CanEnqueue = !in.flush && s1ValidCount < s1QueueSize.U
  val s0AllocBitmap = Mux(s0CanEnqueue, in.freeBitmap & ~reservedBitmap, 0.U)
  val s0Candidates = Wire(Vec(renameWidth, UInt(phyRegIdxWidth.W)))
  val s0CandidateValid = Wire(Vec(renameWidth, Bool()))
  for (bankIndex <- 0 until bankCount) {
    // Match IntRegFileBank: the low-order preg bits select the bank
    // (preg % bankCount), while the remaining bits select the bank-local row.
    // Build each bank bitmap from interleaved physical-register indices rather
    // than slicing four contiguous ranges.
    val bankPRegs = (bankIndex until numPhyRegs by bankCount).toSeq
    val bankWidth = bankPRegs.size
    val bankBitmap = VecInit(bankPRegs.map(s0AllocBitmap(_))).asUInt
    val bankPRegIndices = VecInit(bankPRegs.map(_.U(phyRegIdxWidth.W)))
    val reverseBankPRegIndices = VecInit(bankPRegs.reverse.map(_.U(phyRegIdxWidth.W)))
    val firstInBank = ParallelPriorityEncoder(Cat(1.U(1.W), bankBitmap))
    val lastFromBankEnd = ParallelPriorityEncoder(Cat(1.U(1.W), Reverse(bankBitmap)))
    val firstCandidate = bankPRegIndices(firstInBank)
    val lastCandidate = reverseBankPRegIndices(lastFromBankEnd)
    val bankHasCandidate = firstInBank < bankWidth.U

    s0Candidates(bankIndex) := firstCandidate
    s0Candidates(bankIndex + bankCount) := lastCandidate
    s0CandidateValid(bankIndex) := bankHasCandidate
    s0CandidateValid(bankIndex + bankCount) := bankHasCandidate && firstCandidate =/= lastCandidate
  }

  val s0EnqueueOffset = Wire(Vec(renameWidth, UInt(log2Ceil(renameWidth + 1).W)))
  val s0EnqueueValid = Wire(Vec(renameWidth, Bool()))
  for (candidateIdx <- 0 until renameWidth) {
    s0EnqueueOffset(candidateIdx) := PopCount(s0CandidateValid.take(candidateIdx))
    s0EnqueueValid(candidateIdx) := s0CandidateValid(candidateIdx) &&
      s0EnqueueOffset(candidateIdx) < s1FreeCount
  }
  val s0EnqueueCount = PopCount(s0EnqueueValid)
  val s0EnqueueBitmap = (0 until renameWidth).map { candidateIdx =>
    Mux(
      s0EnqueueValid(candidateIdx),
      UIntToOH(s0Candidates(candidateIdx), numPhyRegs),
      0.U(numPhyRegs.W)
    )
  }.reduce(_ | _)

  // Allocation lanes consume compacted requests from the head of s1.
  val allocateCount = PopCount(in.allocateReq)
  for (laneIdx <- 0 until renameWidth) {
    val candidateOffset = PopCount(in.allocateReq.take(laneIdx))
    out.allocatePhyReg(laneIdx) := s1Queue(addS1Ptr(s1HeadPtr, candidateOffset))
  }
  val canAllocate = s1ValidCount >= allocateCount && !in.flush
  out.canAllocate := canAllocate
  val s1DoDequeue = canAllocate && in.doAllocate && !in.flush
  val s1DequeueCount = Mux(s1DoDequeue, allocateCount, 0.U)

  val selectedBitmap = (0 until renameWidth).map { laneIdx =>
    Mux(
      in.allocateReq(laneIdx),
      UIntToOH(out.allocatePhyReg(laneIdx), numPhyRegs),
      0.U(numPhyRegs.W)
    )
  }.reduce(_ | _)
  out.allocateBitmap := selectedBitmap
  val s1DequeuedBitmap = Mux(s1DoDequeue, selectedBitmap, 0.U(numPhyRegs.W))

  when(in.flush) {
    s1HeadPtr := 0.U
    s1TailPtr := 0.U
    s1ValidCount := 0.U
  }.otherwise {
    s1HeadPtr := addS1Ptr(s1HeadPtr, s1DequeueCount)
    s1TailPtr := addS1Ptr(s1TailPtr, s0EnqueueCount)
    s1ValidCount := s1ValidCount - s1DequeueCount +& s0EnqueueCount
    for (candidateIdx <- 0 until renameWidth) {
      when(s0EnqueueValid(candidateIdx)) {
        val writePtr = addS1Ptr(s1TailPtr, s0EnqueueOffset(candidateIdx))
        s1Queue(writePtr) := s0Candidates(candidateIdx)
      }
    }
  }

  when(in.flush) {
    reservedBitmap := 0.U
  }.otherwise {
    reservedBitmap := (reservedBitmap | s0EnqueueBitmap) & ~s1DequeuedBitmap
  }

  when(!in.flush) {
    assert(s1ValidCount <= s1QueueSize.U)
    assert(s1DequeueCount <= s1ValidCount)
    assert(s0EnqueueCount <= s1FreeCount)
    assert(s1TailPtr === addS1Ptr(s1HeadPtr, s1ValidCount))
    for (candidateIdx <- 0 until renameWidth) {
      when(s0EnqueueValid(candidateIdx)) {
        assert(s0Candidates(candidateIdx) < numPhyRegs.U)
        assert(s0AllocBitmap(s0Candidates(candidateIdx)))
      }
    }
  }
}

object NewFLManager {
  val DefaultS1QueueSize = 16

  class In(numPhyRegs: Int, renameWidth: Int)(implicit p: Parameters) extends XSBundle {
    val freeBitmap = UInt(numPhyRegs.W)
    val allocateReq = Vec(renameWidth, Bool())
    val doAllocate = Bool()
    val flush = Bool()
  }

  class Out(numPhyRegs: Int, renameWidth: Int)(implicit p: Parameters) extends XSBundle {
    val allocatePhyReg = Vec(renameWidth, UInt(log2Up(numPhyRegs).W))
    val allocateBitmap = UInt(numPhyRegs.W)
    val canAllocate = Bool()
  }
}
