package xiangshan.backend.rename.freelist

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.{ParallelPriorityEncoder, ParallelPosteriorityEncoder, ParallelSelectTwo, SelectTwoInterRes}
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
  private val bankIndexWidth = log2Ceil(bankCount)
  private val s1PtrWidth = math.max(1, log2Ceil(s1QueueSize))
  private val s1CountWidth = log2Ceil(s1QueueSize + 1)
  private val s1QueueSizeIsPow2 = (s1QueueSize & (s1QueueSize - 1)) == 0

  /** Stage 1: a configurable circular queue of physical-register candidates. */
  // Queue data are invalid while s1ValidCount is zero and are overwritten
  // before they can be consumed.  They therefore do not need a reset value;
  // keeping this as a plain Reg avoids reset/init logic on the queue payload.
  val s1Queue = Reg(Vec(s1QueueSize, UInt(phyRegIdxWidth.W)))
  val s1HeadPtr = RegInit(0.U(s1PtrWidth.W))
  private val headLowWidth = math.min(2, s1PtrWidth)
  private val headLowSpan = 1 << headLowWidth
  private val headLowCount = math.min(headLowSpan, s1QueueSize)
  private val headHighCount = (s1QueueSize + headLowSpan - 1) / headLowSpan
  val s1HeadLowOH = UIntToOH(s1HeadPtr(headLowWidth - 1, 0), headLowCount)
  val s1HeadHighOH = UIntToOH(s1HeadPtr >> headLowWidth, headHighCount)
  val s1HeadPtrOH = VecInit((0 until s1QueueSize).map { slot =>
    s1HeadLowOH(slot % headLowSpan) && s1HeadHighOH(slot / headLowSpan)
  }).asUInt
  val s1TailPtr = RegInit(0.U(s1PtrWidth.W))
  val s1FreeCount = RegInit(s1QueueSize.U(s1CountWidth.W))
  val s1ValidCount = s1QueueSize.U(s1CountWidth.W) - s1FreeCount
  val s1CanAllocateReg = RegInit(false.B)

  private def addS1Ptr(ptr: UInt, increment: UInt): UInt = {
    val sum = ptr +& increment
    if (s1QueueSizeIsPow2) {
      // The default queue has 16 entries, so truncation is exact modulo
      // arithmetic and removes a compare/subtract mux from every pointer use.
      sum(s1PtrWidth - 1, 0)
    } else {
      Mux(sum >= s1QueueSize.U, sum - s1QueueSize.U, sum)(s1PtrWidth - 1, 0)
    }
  }

  // Candidates in s1 remain free in the owner's bitmap until rename really
  // consumes them, so the manager reserves them locally to prevent reselection.
  val reservedBitmap = RegInit(0.U(numPhyRegs.W))
  val allocateCount = PopCount(in.allocateReq)
  val s1DoDequeue = s1CanAllocateReg && in.doAllocate && !in.flush
  val s1DequeueCount = Mux(s1DoDequeue, allocateCount, 0.U)
  // Head entries consumed this cycle free slots that can be reused by the
  // append stream at tail, including when the queue was full at cycle start.
  val s1EnqueueCapacity = s1FreeCount +& s1DequeueCount

  /** Stage 0: select up to two candidates from every bank. */
  // Keep flush/capacity control out of the bitmap and bank priority encoders.
  // Capacity is applied by enqueueValid after selection; flush only masks the
  // selected candidates, so it does not drive a 160-bit search network.
  val s0AllocBitmap = in.freeBitmap & ~reservedBitmap
  val s0Candidates = Wire(Vec(renameWidth, UInt(phyRegIdxWidth.W)))
  val s0CandidateValid = Wire(Vec(renameWidth, Bool()))
  for (bankIndex <- 0 until bankCount) {
    // Match IntRegFileBank: the low-order preg bits select the bank
    // (preg % bankCount), while the remaining bits select the bank-local row.
    // Build each bank bitmap from interleaved physical-register indices rather
    // than slicing four contiguous ranges.
    val bankPRegs = (bankIndex until numPhyRegs by bankCount).toSeq
    val bankBitmap = VecInit(bankPRegs.map(s0AllocBitmap(_))).asUInt.pad(1 << log2Ceil(bankPRegs.size))
    // With low-bit interleaving, preg = row << bankIndexWidth | bankIndex.
    // Form the physical index arithmetically instead of dynamically indexing
    // a 32/40-entry constant vector after priority encoding.
    val firstInBank = ParallelPriorityEncoder(bankBitmap)
    val lastInBank = ParallelPosteriorityEncoder(bankBitmap)
    val firstCandidate = (firstInBank << bankIndexWidth) | bankIndex.U
    val lastCandidate = (lastInBank << bankIndexWidth) | bankIndex.U
    // The count path only needs to know whether a second bit exists.  Avoid
    // comparing the two encoder outputs here: the posteriority encoder is
    // still needed for the last candidate, but feeding it into the count
    // path adds its full search depth to s1ValidCountNext.  A balanced
    // saturating two-entry reduction keeps this decision independent of both
    // binary encoder results.
    val bankSelect = ParallelSelectTwo(
      bankBitmap.asBools.map(bit => SelectTwoInterRes(bit, 0.U(1.W)))
    )
    val bankHasCandidate = bankSelect.hasOne
    val bankHasTwoCandidates = bankSelect.hasTwo

    // Keep a fixed bank order for the first candidates, then walk the banks
    // in reverse order for the last candidates:
    // bank0_first ... bank3_first, bank3_last ... bank0_last.
    val lastCandidateIdx = renameWidth - 1 - bankIndex
    s0Candidates(bankIndex) := firstCandidate
    s0Candidates(lastCandidateIdx) := lastCandidate
    s0CandidateValid(bankIndex) := !in.flush && bankHasCandidate
    s0CandidateValid(lastCandidateIdx) :=
      !in.flush && bankHasCandidate && bankHasTwoCandidates
  }

  // Newly released registers are not yet in this cycle's bitmap.
  // Append commit frees after the bitmap candidates. As in StdFreeList, the
  // free interface guarantees distinct, not-currently-free physical regs;
  // keep this validity path independent of the preg values.
  val freeCandidateValid = in.freeReq

  val enqueueWidth = renameWidth + freeWidth
  val enqueueCandidates = VecInit(s0Candidates ++ in.freePhyReg)
  val enqueueCandidateValid = VecInit(s0CandidateValid ++ freeCandidateValid)
  val enqueueOffset = Wire(Vec(enqueueWidth, UInt(log2Ceil(enqueueWidth + 1).W)))
  val enqueueValid = Wire(Vec(enqueueWidth, Bool()))
  for (candidateIdx <- 0 until enqueueWidth) {
    enqueueOffset(candidateIdx) := PopCount(enqueueCandidateValid.take(candidateIdx))
    enqueueValid(candidateIdx) := enqueueCandidateValid(candidateIdx) &&
      enqueueOffset(candidateIdx) < s1EnqueueCapacity
  }
  // The accepted stream is a stable prefix of the valid candidates. Count the
  // raw valids once and clamp to FIFO capacity, instead of PopCount-ing the
  // per-candidate capacity comparisons again on the tail-pointer path.
  // Share the qualified-bit reduction with enqueueOffset and the routing
  // controls instead of maintaining a separate per-bank count network.
  val s0CandidateCount = PopCount(s0CandidateValid)
  val freeCandidateCount = PopCount(freeCandidateValid)
  val rawCandidateCount = s0CandidateCount +& freeCandidateCount
  val enqueueCount = Mux(
    rawCandidateCount > s1EnqueueCapacity,
    s1EnqueueCapacity,
    rawCandidateCount
  )
  val enqueueBitmap = (0 until enqueueWidth).map { candidateIdx =>
    if (candidateIdx < renameWidth) {
      Mux(enqueueValid(candidateIdx), UIntToOH(s0Candidates(candidateIdx), numPhyRegs), 0.U(numPhyRegs.W))
    } else {
      Mux(enqueueOffset(candidateIdx) < s1EnqueueCapacity,
        in.freePhyRegOH(candidateIdx - renameWidth), 0.U(numPhyRegs.W))
    }
  }.reduce(_ | _)

  // Allocation lanes consume compacted requests from the head of s1.
  // Keep a statically rotated view of the queue, as StdFreeList does, so the
  // output path only selects one already-formed RenameWidth-wide window. The
  // previous dynamic index formed an add/compare/mux chain for every lane.
  // Split the binary head index into two shared four-way selection stages.
  val lowRotatedQueue = VecInit((0 until s1QueueSize).map { slot =>
    VecInit((0 until headLowCount).map { low =>
      s1Queue((slot + low) % s1QueueSize)
    })(s1HeadPtr(headLowWidth - 1, 0))
  })
  val s1HeadCandidates = VecInit((0 until renameWidth).map { offset =>
    VecInit((0 until headHighCount).map { high =>
      lowRotatedQueue((offset + high * headLowSpan) % s1QueueSize)
    })(s1HeadPtr >> headLowWidth)
  })
  for (laneIdx <- 0 until renameWidth) {
    val candidateOffset = PopCount(in.allocateReq.take(laneIdx))
    out.allocatePhyReg(laneIdx) := Mux1H((0 until renameWidth).map(i => candidateOffset === i.U), s1HeadCandidates.toSeq)
  }
  // Match StdFreeList timing: canAllocate is registered from the number of
  // candidates left after this cycle's dequeue/refill.
  out.canAllocate := s1CanAllocateReg && !in.flush
  val s1WillBeFull = rawCandidateCount >= s1EnqueueCapacity
  val s1FreeCountNext = Mux(s1WillBeFull, 0.U, s1EnqueueCapacity - rawCandidateCount)
  val s1CanAllocateNext = (rawCandidateCount +& (s1QueueSize - renameWidth).U) >= s1EnqueueCapacity
  val s1HeadPtrNext = addS1Ptr(s1HeadPtr, s1DequeueCount)

  // Sparse requests still consume a contiguous prefix of the queue. Decode
  // that prefix directly instead of decoding the per-lane compaction muxes.
  val selectedBitmap = (0 until renameWidth).map { offset =>
    Mux(
      offset.U < allocateCount,
      UIntToOH(s1HeadCandidates(offset), numPhyRegs),
      0.U(numPhyRegs.W)
    )
  }.reduce(_ | _)
  out.allocateBitmap := selectedBitmap
  val s1DequeuedBitmap = (0 until renameWidth).map { offset =>
    Mux(s1DoDequeue && offset.U < allocateCount,
      UIntToOH(s1HeadCandidates(offset), numPhyRegs), 0.U(numPhyRegs.W))
  }.reduce(_ | _)
  out.dequeuedBitmap := s1DequeuedBitmap

  // s1 holds unallocated prefetch candidates, which remain free across
  // rollback. Keep the head fixed during recovery, but allow commit frees to
  // append at the tail. Track availability as the queue fills so allocation
  // can resume immediately when recovery ends.
  when(!in.flush) {
    s1HeadPtr := s1HeadPtrNext
  }
  s1CanAllocateReg := s1CanAllocateNext
  // A full circular queue has tail == head. Otherwise all raw candidates
  // fit, so advance tail by the raw count without waiting for capacity clamp.
  s1TailPtr := Mux(s1WillBeFull, s1HeadPtrNext, addS1Ptr(s1TailPtr, rawCandidateCount))
  s1FreeCount := s1FreeCountNext
  // Stable candidate ranks occupy a contiguous cyclic interval. A low-bit
  // first butterfly therefore routes them without conflicts, using O(Q log Q)
  // two-input switches instead of an O(Q * enqueueWidth) data crossbar.
  class EnqueuePacket extends Bundle {
    val valid = Bool()
    val preg = UInt(phyRegIdxWidth.W)
  }
  val routedCandidates = if (s1QueueSizeIsPow2 && enqueueWidth <= s1QueueSize) {
    val initial = Wire(Vec(s1QueueSize, new EnqueuePacket))
    for (idx <- 0 until s1QueueSize) {
      initial(idx).valid := (if (idx < enqueueWidth) enqueueCandidateValid(idx) else false.B)
      initial(idx).preg := (if (idx < enqueueWidth) enqueueCandidates(idx) else 0.U)
    }
    // A block's compacted candidates occupy a contiguous cyclic interval.
    // Derive each switch choice directly from that interval, rather than
    // forwarding valid bits through every preceding routing stage.
    def firstDestination(start: Int): UInt = {
      val rank = if (start < enqueueWidth) enqueueOffset(start) else rawCandidateCount
      if (start == 0) s1TailPtr else addS1Ptr(s1TailPtr, rank)
    }
    def candidateCount(start: Int, size: Int): UInt = {
      PopCount(enqueueCandidateValid.slice(start, math.min(start + size, enqueueWidth)))
    }
    var stage = initial.toSeq
    for (bit <- 0 until s1PtrWidth) {
      val next = Wire(Vec(s1QueueSize, new EnqueuePacket))
      val stride = 1 << bit
      for (base <- 0 until s1QueueSize by 2 * stride) {
        val first = firstDestination(base)(bit, 0)
        val leftCount = candidateCount(base, stride)
        val totalCount = candidateCount(base, 2 * stride)
        def rankAt(channel: Int): UInt = (channel.U((bit + 1).W) - first)(bit, 0)
        for (offset <- 0 until stride) {
          val low = base + offset
          val high = low + stride
          val lowRank = rankAt(offset)
          val highRank = rankAt(offset + stride)
          next(low).preg := Mux(lowRank < leftCount, stage(low).preg, stage(high).preg)
          next(high).preg := Mux(highRank < leftCount, stage(low).preg, stage(high).preg)
          next(low).valid := lowRank < totalCount
          next(high).valid := highRank < totalCount
        }
      }
      stage = next.toSeq
    }
    Some(stage)
  } else {
    None
  }

  // Compare raw count and capacity in parallel to avoid a clamp mux on
  // the clock-gate enable. Keep capacity out of the packet data path.
  // Routing all raw valids is safe because the stream has at most Q entries.
  for (queueIdx <- 0 until s1QueueSize) {
    // Compute a slot's rank in the append stream once. This replaces an
    // address adder per candidate and an address comparison per slot/lane.
    val slotOffset = if (s1QueueSizeIsPow2) {
      (queueIdx.U(s1PtrWidth.W) - s1TailPtr)(s1PtrWidth - 1, 0)
    } else {
      Mux(queueIdx.U >= s1TailPtr,
        queueIdx.U - s1TailPtr,
        (queueIdx + s1QueueSize).U - s1TailPtr)(s1PtrWidth - 1, 0)
    }
    val writeData = routedCandidates match {
      case Some(candidates) =>
        when(slotOffset < rawCandidateCount && slotOffset < s1EnqueueCapacity) {
          assert(candidates(queueIdx).valid)
        }
        candidates(queueIdx).preg
      case None =>
        val writeCandidateOH = VecInit(Seq.tabulate(enqueueWidth) { candidateIdx =>
          enqueueCandidateValid(candidateIdx) && enqueueOffset(candidateIdx) === slotOffset
        })
        Mux1H(writeCandidateOH, enqueueCandidates)
    }
    when(slotOffset < rawCandidateCount && slotOffset < s1EnqueueCapacity) {
      s1Queue(queueIdx) := writeData
    }
  }

  reservedBitmap := (reservedBitmap | enqueueBitmap) & ~s1DequeuedBitmap

  when(!in.flush) {
    assert((reservedBitmap & ~in.freeBitmap) === 0.U,
      "s1 candidates must remain free after recovery")
  }
  assert(PopCount(reservedBitmap) === s1ValidCount)
  assert(s1CanAllocateReg === (s1ValidCount >= renameWidth.U))
  assert(s1ValidCount <= s1QueueSize.U)
  assert(s1DequeueCount <= s1ValidCount)
  assert(enqueueCount <= s1EnqueueCapacity)
  assert(PopCount(enqueueBitmap) === enqueueCount)
  assert(s1HeadPtrOH === UIntToOH(s1HeadPtr, s1QueueSize))
  assert(s1TailPtr === addS1Ptr(s1HeadPtr, s1ValidCount))
  for (candidateIdx <- 0 until enqueueWidth) {
    when(enqueueValid(candidateIdx)) {
      assert(enqueueCandidates(candidateIdx) < numPhyRegs.U)
      if (candidateIdx < renameWidth) {
        assert(s0AllocBitmap(enqueueCandidates(candidateIdx)))
      } else {
        assert(in.freeReq(candidateIdx - renameWidth))
      }
    }
  }
}

object NewFLManager {
  val DefaultS1QueueSize = 16

  class In(numPhyRegs: Int, renameWidth: Int, freeReqWidth: Int)(implicit p: Parameters) extends XSBundle {
    val freeBitmap = UInt(numPhyRegs.W)
    val allocateReq = Vec(renameWidth, Bool())
    val freeReq = Vec(freeReqWidth, Bool())
    val freePhyReg = Vec(freeReqWidth, UInt(log2Up(numPhyRegs).W))
    // Each one-hot is qualified by freeReq in the owner; share that
    // decode without adding another freeReq gate at this boundary.
    val freePhyRegOH = Vec(freeReqWidth, UInt(numPhyRegs.W))
    val doAllocate = Bool()
    // Pause bitmap selection/allocation during recovery; freeReq may fill s1.
    val flush = Bool()
  }

  class Out(numPhyRegs: Int, renameWidth: Int)(implicit p: Parameters) extends XSBundle {
    val allocatePhyReg = Vec(renameWidth, UInt(log2Up(numPhyRegs).W))
    val allocateBitmap = UInt(numPhyRegs.W)
    val dequeuedBitmap = UInt(numPhyRegs.W)
    val canAllocate = Bool()
  }
}
