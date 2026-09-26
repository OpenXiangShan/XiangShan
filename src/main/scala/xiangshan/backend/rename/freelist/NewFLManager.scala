package xiangshan.backend.rename.freelist

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.ParallelPriorityEncoder
import xiangshan.{XSBundle, XSModule}

class NewFLManager(
  numPhyRegs: Int,
  renameWidth: Int,
  s1QueueSize: Int = NewFLManager.DefaultS1QueueSize,
  freeReqWidth: Int = 0
)(implicit p: Parameters) extends XSModule {
  require(renameWidth >= 2 && renameWidth % 2 == 0,
    s"NewFLManager rename width must be a positive even number, got $renameWidth")
  require(numPhyRegs >= renameWidth / 2,
    s"Physical register count $numPhyRegs must cover ${renameWidth / 2} allocation banks")
  require(s1QueueSize >= renameWidth,
    s"NewFLManager s1 queue size $s1QueueSize must be no smaller than rename width $renameWidth")
  private val freeWidth = if (freeReqWidth == 0) renameWidth else freeReqWidth
  require(freeWidth > 0,
    s"NewFLManager free-request width must be positive, got $freeWidth")

  val in = IO(Input(new NewFLManager.In(numPhyRegs, renameWidth, freeWidth)))
  val out = IO(Output(new NewFLManager.Out(numPhyRegs, renameWidth)))

  private val bankCount = renameWidth / 2
  private val phyRegIdxWidth = log2Up(numPhyRegs)
  private val s1PtrWidth = math.max(1, log2Ceil(s1QueueSize))
  private val s1CountWidth = log2Ceil(s1QueueSize + 1)

  /** Stage 1: a configurable circular queue of physical-register candidates. */
  val s1Queue = RegInit(VecInit(Seq.fill(s1QueueSize)(0.U(phyRegIdxWidth.W))))
  val s1HeadPtr = RegInit(0.U(s1PtrWidth.W))
  val s1HeadPtrOH = RegInit(1.U(s1QueueSize.W))
  val s1TailPtr = RegInit(0.U(s1PtrWidth.W))
  val s1ValidCount = RegInit(0.U(s1CountWidth.W))
  val s1CanAllocateReg = RegInit(false.B)

  private def addS1Ptr(ptr: UInt, increment: UInt): UInt = {
    val sum = ptr +& increment
    Mux(sum >= s1QueueSize.U, sum - s1QueueSize.U, sum)(s1PtrWidth - 1, 0)
  }

  // Candidates in s1 remain free in the owner's bitmap until rename really
  // consumes them, so the manager reserves them locally to prevent reselection.
  val reservedBitmap = RegInit(0.U(numPhyRegs.W))
  val s1FreeCount = s1QueueSize.U(s1CountWidth.W) - s1ValidCount
  val allocateCount = PopCount(in.allocateReq)
  val s1DoDequeue = s1CanAllocateReg && in.doAllocate && !in.flush
  val s1DequeueCount = Mux(s1DoDequeue, allocateCount, 0.U)
  // Refill uses only the space visible at the beginning of the cycle.  Do not
  // feed the current allocation request back into the s0 bitmap search and
  // s1 write path; the FIFO head advances this cycle, and the newly freed
  // slots become refill capacity in the following cycle.  This mirrors the
  // timing-friendly staged FIFO scheme used by MaskFreeList.
  val s1EnqueueCapacity = s1FreeCount

  /** Stage 0: select up to two candidates from every bank. */
  val s0CanCapture = !in.flush
  // Selection does not depend on current FIFO capacity. Even when s1 is full,
  // capture a batch that can use space made visible in the following cycle.
  // A current freeReq is not present in the registered free bitmap yet, so it
  // cannot overlap a candidate selected here.
  val s0AllocBitmap = in.freeBitmap & ~reservedBitmap
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

    // Keep a fixed bank order for the first candidates, then walk the banks
    // in reverse order for the last candidates:
    // bank0_first ... bank3_first, bank3_last ... bank0_last.
    val lastCandidateIdx = renameWidth - 1 - bankIndex
    s0Candidates(bankIndex) := firstCandidate
    s0Candidates(lastCandidateIdx) := lastCandidate
    s0CandidateValid(bankIndex) := s0CanCapture && bankHasCandidate
    s0CandidateValid(lastCandidateIdx) :=
      s0CanCapture && bankHasCandidate && firstCandidate =/= lastCandidate
  }

  val s0CandidateBitmap = (0 until renameWidth).map { candidateIdx =>
    Mux(
      s0CandidateValid(candidateIdx),
      UIntToOH(s0Candidates(candidateIdx), numPhyRegs),
      0.U(numPhyRegs.W)
    )
  }.reduce(_ | _)

  /** Stage 1: hold one complete bitmap-selected batch for one cycle. */
  val pendingCandidates = RegInit(VecInit(Seq.fill(renameWidth)(0.U(phyRegIdxWidth.W))))
  val pendingCandidateValid = RegInit(VecInit(Seq.fill(renameWidth)(false.B)))
  val pendingCandidateBitmap = RegInit(0.U(numPhyRegs.W))

  // Newly released registers are not yet in this cycle's bitmap. Append
  // them after the registered bitmap candidates so they can refill s1
  // immediately. Both sources may refill during recovery; only new bitmap
  // selection is paused. As in StdFreeList, the free interface guarantees
  // that valid entries are unique and are not already free. Keep validity
  // independent of the preg value; dynamically checking reservedBitmap and
  // comparing free lanes would put freePhyReg back on the count/control path.
  val freeCandidateValid = in.freeReq

  val enqueueWidth = renameWidth + freeWidth
  val enqueueCandidates = VecInit(pendingCandidates ++ in.freePhyReg)
  val enqueueCandidateValid = VecInit(pendingCandidateValid ++ freeCandidateValid)

  // Compute each raw candidate's stable compacted position in parallel. Keep
  // count generation independent from the candidate-data crossbar: the count
  // is simply the smaller of the number of valid inputs and the FIFO capacity.
  // This avoids feeding a recursive compaction network into the count, tail,
  // canAllocate, and reserved-bitmap state updates.
  private val enqueueCountWidth = log2Ceil(enqueueWidth + 1)
  val enqueueOffset = Wire(Vec(enqueueWidth, UInt(enqueueCountWidth.W)))
  for (candidateIdx <- 0 until enqueueWidth) {
    enqueueOffset(candidateIdx) := PopCount(enqueueCandidateValid.take(candidateIdx))
  }
  val rawCandidateCount = PopCount(enqueueCandidateValid)
  val enqueueCount = Mux(
    rawCandidateCount > s1EnqueueCapacity,
    s1EnqueueCapacity,
    rawCandidateCount
  )
  val rawEnqueueValid = VecInit(Seq.tabulate(enqueueWidth) { candidateIdx =>
    enqueueCandidateValid(candidateIdx) &&
      enqueueOffset(candidateIdx) < s1EnqueueCapacity
  })
  // Pending entries were reserved when they entered the pending register.
  // Only accepted free requests add new bits to reservedBitmap here.
  val freeEnqueueBitmap = (0 until freeWidth).map { freeIdx =>
    val candidateIdx = renameWidth + freeIdx
    Mux(
      rawEnqueueValid(candidateIdx),
      UIntToOH(in.freePhyReg(freeIdx), numPhyRegs),
      0.U(numPhyRegs.W)
    )
  }.reduce(_ | _)
  val pendingAcceptedBitmap = (0 until renameWidth).map { candidateIdx =>
    Mux(
      rawEnqueueValid(candidateIdx),
      UIntToOH(pendingCandidates(candidateIdx), numPhyRegs),
      0.U(numPhyRegs.W)
    )
  }.reduce(_ | _)
  // Outside recovery, the pending stage is replaced every cycle, so release
  // candidates that FIFO capacity did not accept. During recovery, retain
  // unaccepted candidates for another attempt instead.
  val pendingUnusedBitmap = Mux(
    in.flush,
    0.U(numPhyRegs.W),
    pendingCandidateBitmap & ~pendingAcceptedBitmap
  )

  // Pending candidates remain free across rollback, so they may continue to
  // refill s1 during recovery. Remove the accepted entries to prevent the
  // same batch from being enqueued again on the next flush cycle; retain any
  // entries blocked by FIFO capacity. Outside recovery, the next s0 batch
  // replaces the previous one after the latter has had one enqueue attempt.
  when(in.flush) {
    for (candidateIdx <- 0 until renameWidth) {
      pendingCandidateValid(candidateIdx) := pendingCandidateValid(candidateIdx) &&
        !rawEnqueueValid(candidateIdx)
    }
    pendingCandidateBitmap := pendingCandidateBitmap & ~pendingAcceptedBitmap
  }.otherwise {
    pendingCandidates := s0Candidates
    pendingCandidateValid := s0CandidateValid
    pendingCandidateBitmap := s0CandidateBitmap
  }

  // Each raw candidate directly targets tail + its compacted offset. Select
  // it only once at the physical queue slot, rather than first crossing into a
  // compacted-candidate vector and then crossing again into the circular FIFO.
  val enqueueWritePtr = VecInit(Seq.tabulate(enqueueWidth) { candidateIdx =>
    addS1Ptr(s1TailPtr, enqueueOffset(candidateIdx))
  })

  // Allocation lanes consume compacted requests from the head of s1.
  // Keep a statically rotated view of the queue, as StdFreeList does, so the
  // output path only selects one already-formed RenameWidth-wide window. The
  // previous dynamic index formed an add/compare/mux chain for every lane.
  val s1QueueVec = Wire(Vec(s1QueueSize, Vec(renameWidth, UInt(phyRegIdxWidth.W))))
  for (queueStart <- 0 until s1QueueSize) {
    for (offset <- 0 until renameWidth) {
      val queueIndex = (queueStart + offset) % s1QueueSize
      s1QueueVec(queueStart)(offset) := s1Queue(queueIndex)
    }
  }
  val s1HeadCandidates = Mux1H(s1HeadPtrOH, s1QueueVec)
  for (laneIdx <- 0 until renameWidth) {
    val candidateOffset = PopCount(in.allocateReq.take(laneIdx))
    out.allocatePhyReg(laneIdx) := s1HeadCandidates(candidateOffset)
  }
  // Match StdFreeList timing: canAllocate is registered from the number of
  // candidates left after this cycle's dequeue/refill.
  out.canAllocate := s1CanAllocateReg && !in.flush
  val s1ValidCountNext = s1ValidCount - s1DequeueCount +& enqueueCount
  val s1CanAllocateNext = s1ValidCountNext >= renameWidth.U
  val s1HeadPtrNext = addS1Ptr(s1HeadPtr, s1DequeueCount)
  val s1HeadPtrOHNext = UIntToOH(s1HeadPtrNext, s1QueueSize)
  val s1TailPtrNext = addS1Ptr(s1TailPtr, enqueueCount)

  val selectedBitmap = (0 until renameWidth).map { laneIdx =>
    Mux(
      in.allocateReq(laneIdx),
      UIntToOH(out.allocatePhyReg(laneIdx), numPhyRegs),
      0.U(numPhyRegs.W)
    )
  }.reduce(_ | _)
  out.allocateBitmap := selectedBitmap
  val s1DequeuedBitmap = Mux(s1DoDequeue, selectedBitmap, 0.U(numPhyRegs.W))

  // s1 holds unallocated prefetch candidates, which remain free across
  // rollback. Keep the head fixed during recovery, but allow commit frees to
  // append at the tail. Track availability as the queue fills so allocation
  // can resume immediately when recovery ends.
  when(!in.flush) {
    s1HeadPtr := s1HeadPtrNext
    s1HeadPtrOH := Mux(s1DoDequeue, s1HeadPtrOHNext, s1HeadPtrOH)
  }
  s1CanAllocateReg := s1CanAllocateNext
  s1TailPtr := s1TailPtrNext
  s1ValidCount := s1ValidCountNext
  for (queueIdx <- 0 until s1QueueSize) {
    val writeCandidateOH = VecInit(Seq.tabulate(enqueueWidth) { candidateIdx =>
      rawEnqueueValid(candidateIdx) && enqueueWritePtr(candidateIdx) === queueIdx.U
    })
    when(writeCandidateOH.asUInt.orR) {
      s1Queue(queueIdx) := Mux1H(writeCandidateOH, enqueueCandidates)
    }
  }

  // Reserve the new s0 batch as soon as it enters the pending register. The
  // previous pending batch either becomes an s1 entry or is released here if
  // FIFO capacity could not accept it. Accepted freeReq candidates bypass the
  // pending register and are reserved through freeEnqueueBitmap.
  reservedBitmap := (reservedBitmap | s0CandidateBitmap | freeEnqueueBitmap) &
    ~(pendingUnusedBitmap | s1DequeuedBitmap)

  // when(!in.flush) {
  //   assert((reservedBitmap & ~in.freeBitmap) === 0.U,
  //     "s1 candidates must remain free after recovery")
  // }
  // assert(PopCount(reservedBitmap) === s1ValidCount +& PopCount(pendingCandidateValid))
  // assert(s1CanAllocateReg === (s1ValidCount >= renameWidth.U))
  // assert(s1ValidCount <= s1QueueSize.U)
  // assert(s1DequeueCount <= s1ValidCount)
  // assert(enqueueCount <= s1EnqueueCapacity)
  // assert(PopCount(freeEnqueueBitmap) === PopCount(rawEnqueueValid.drop(renameWidth)))
  // assert(s1HeadPtrOH === UIntToOH(s1HeadPtr, s1QueueSize))
  // assert(s1TailPtr === addS1Ptr(s1HeadPtr, s1ValidCount))
  // for (candidateIdx <- 0 until enqueueWidth) {
  //   when(rawEnqueueValid(candidateIdx)) {
  //     assert(enqueueCandidates(candidateIdx) < numPhyRegs.U)
  //   }
  // }
}

object NewFLManager {
  val DefaultS1QueueSize = 16

  class In(numPhyRegs: Int, renameWidth: Int, freeReqWidth: Int)(implicit p: Parameters) extends XSBundle {
    val freeBitmap = UInt(numPhyRegs.W)
    val allocateReq = Vec(renameWidth, Bool())
    val freeReq = Vec(freeReqWidth, Bool())
    val freePhyReg = Vec(freeReqWidth, UInt(log2Up(numPhyRegs).W))
    val doAllocate = Bool()
    // Pause bitmap selection/allocation during recovery; freeReq may fill s1.
    val flush = Bool()
  }

  class Out(numPhyRegs: Int, renameWidth: Int)(implicit p: Parameters) extends XSBundle {
    val allocatePhyReg = Vec(renameWidth, UInt(log2Up(numPhyRegs).W))
    val allocateBitmap = UInt(numPhyRegs.W)
    val canAllocate = Bool()
  }
}
