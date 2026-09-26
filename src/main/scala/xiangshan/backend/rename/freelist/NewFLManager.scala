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
  val s0CanEnqueue = !in.flush && s1EnqueueCapacity =/= 0.U
  // Keep the bitmap and bank priority encoders independent of the current
  // refill-capacity control.  Capacity gates the compacted candidate-valid
  // stream below, after the bank search, instead of fanning out through every
  // bitmap bit and priority-encoder input.
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
    s0CandidateValid(bankIndex) := s0CanEnqueue && bankHasCandidate
    s0CandidateValid(lastCandidateIdx) :=
      s0CanEnqueue && bankHasCandidate && firstCandidate =/= lastCandidate
  }

  val s0CandidateBitmap = (0 until renameWidth).map { candidateIdx =>
    Mux(
      s0CandidateValid(candidateIdx),
      UIntToOH(s0Candidates(candidateIdx), numPhyRegs),
      0.U(numPhyRegs.W)
    )
  }.reduce(_ | _)

  // Newly released registers are not yet in this cycle's bitmap. Append
  // them after the s0-selected candidates so they can refill s1 immediately.
  // Commit frees remain valid during recovery, while bitmap selection stops.
  // Filter duplicates against s1 reservations, s0 candidates, and earlier
  // free requests before compacting the combined enqueue stream.
  val freeCandidateValid = Wire(Vec(freeWidth, Bool()))
  for (freeIdx <- 0 until freeWidth) {
    val freeReg = in.freePhyReg(freeIdx)
    val duplicateFree = if (freeIdx == 0) {
      false.B
    } else {
      in.freeReq.take(freeIdx).zip(in.freePhyReg.take(freeIdx)).map {
        case (valid, previousReg) => valid && previousReg === freeReg
      }.reduce(_ || _)
    }
    freeCandidateValid(freeIdx) := in.freeReq(freeIdx) &&
      !reservedBitmap(freeReg) && !s0CandidateBitmap(freeReg) && !duplicateFree
  }

  val enqueueWidth = renameWidth + freeWidth
  val enqueueCandidates = VecInit(s0Candidates ++ in.freePhyReg)
  val enqueueCandidateValid = VecInit(s0CandidateValid ++ freeCandidateValid)

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
  private val maxEnqueueWidth = math.min(enqueueWidth, s1QueueSize)
  val compactedCandidates = VecInit(Seq.tabulate(maxEnqueueWidth) { compactedIdx =>
    val selectOH = VecInit(Seq.tabulate(enqueueWidth) { candidateIdx =>
      enqueueCandidateValid(candidateIdx) &&
        enqueueOffset(candidateIdx) === compactedIdx.U
    })
    Mux1H(selectOH, enqueueCandidates)
  })
  val enqueueValid = VecInit(Seq.tabulate(maxEnqueueWidth) { candidateIdx =>
    candidateIdx.U < enqueueCount
  })
  val enqueueBitmap = (0 until enqueueWidth).map { candidateIdx =>
    Mux(
      rawEnqueueValid(candidateIdx),
      UIntToOH(enqueueCandidates(candidateIdx), numPhyRegs),
      0.U(numPhyRegs.W)
    )
  }.reduce(_ | _)

  // Decode the existing tail pointer only on the queue-write branch. It is a
  // combinational view, not another state element, so candidate selection and
  // enqueueCount cannot create a new path ending at a tail one-hot register.
  val s1TailPtrOH = UIntToOH(s1TailPtr, s1QueueSize)
  val enqueueSlotOH = VecInit(Seq.tabulate(maxEnqueueWidth) { candidateIdx =>
    VecInit(Seq.tabulate(s1QueueSize) { queueIdx =>
      s1TailPtrOH((queueIdx - candidateIdx + s1QueueSize) % s1QueueSize)
    }).asUInt
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
    val writeLaneOH = VecInit(Seq.tabulate(maxEnqueueWidth) { candidateIdx =>
      enqueueValid(candidateIdx) && enqueueSlotOH(candidateIdx)(queueIdx)
    })
    when(writeLaneOH.asUInt.orR) {
      s1Queue(queueIdx) := Mux1H(writeLaneOH, compactedCandidates)
    }
  }

  reservedBitmap := (reservedBitmap | enqueueBitmap) & ~s1DequeuedBitmap

  // when(!in.flush) {
  //   assert((reservedBitmap & ~in.freeBitmap) === 0.U,
  //     "s1 candidates must remain free after recovery")
  // }
  // assert(PopCount(reservedBitmap) === s1ValidCount)
  // assert(s1CanAllocateReg === (s1ValidCount >= renameWidth.U))
  // assert(s1ValidCount <= s1QueueSize.U)
  // assert(s1DequeueCount <= s1ValidCount)
  // assert(enqueueCount <= s1EnqueueCapacity)
  // assert(PopCount(enqueueBitmap) === enqueueCount)
  // assert(s1HeadPtrOH === UIntToOH(s1HeadPtr, s1QueueSize))
  // assert(s1TailPtr === addS1Ptr(s1HeadPtr, s1ValidCount))
  // for (candidateIdx <- 0 until maxEnqueueWidth) {
  //   when(enqueueValid(candidateIdx)) {
  //     assert(compactedCandidates(candidateIdx) < numPhyRegs.U)
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
