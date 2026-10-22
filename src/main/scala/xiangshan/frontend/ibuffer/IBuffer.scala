// Copyright (c) 2024-2025 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2025 Institute of Computing Technology, Chinese Academy of Sciences
// Copyright (c) 2020-2021 Peng Cheng Laboratory
//
// XiangShan is licensed under Mulan PSL v2.
// You can use this software according to the terms and conditions of the Mulan PSL v2.
// You may obtain a copy of Mulan PSL v2 at:
//          https://license.coscl.org.cn/MulanPSL2
//
// THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
// EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
// MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
//
// See the Mulan PSL v2 for more details.

package xiangshan.frontend.ibuffer

import chisel3._
import chisel3.util._
import freechips.rocketchip.rocket.Instructions.VSETIVLI
import freechips.rocketchip.rocket.Instructions.VSETVL
import freechips.rocketchip.rocket.Instructions.VSETVLI
import org.chipsalliance.cde.config.Parameters
import utility.HasCircularQueuePtrHelper
import utility.HasPerfEvents
import utility.PriorityMuxDefault
import utility.QueuePerf
import utility.UIntToMask
import utility.XSDebug
import utility.XSError
import utility.XSPerfAccumulate
import xiangshan.CtrlFlow
import xiangshan.StallReasonIO
import xiangshan.TopDownCounters
import xiangshan.backend.BackendToIBufBundle
import xiangshan.backend.decode.VTypeGen
import xiangshan.frontend.BackendRedirectTopdown
import xiangshan.frontend.FetchToIBuffer
import xiangshan.frontend.FrontendTopDownBundle
import xiangshan.frontend.ifu.FastFetchEntry

class IBuffer(implicit p: Parameters) extends IBufferModule with HasCircularQueuePtrHelper with HasPerfEvents {
  class IBufferIO extends Bundle {
    val in:  FetchToIBuffer             = Flipped(new FetchToIBuffer)
    val out: Vec[DecoupledIO[CtrlFlow]] = Vec(DecodeWidth, DecoupledIO(new CtrlFlow))

    val flush: Bool = Input(Bool())

    val full:  Bool = Output(Bool())
    val empty: Bool = Output(Bool())

    val fromBackend: BackendToIBufBundle = Input(new BackendToIBufBundle)

    // top-down
    val backendRedirectTopdown: BackendRedirectTopdown = Input(new BackendRedirectTopdown)
    val stallReason:            StallReasonIO          = new StallReasonIO(DecodeWidth)
  }

  val io: IBufferIO = IO(new IBufferIO)

  /**
   * io alias
   * When updating vtype from rob, ibuffer should not send new instructions to outputEntries.
   * Therefore, outputEntries and related counters will not be updated when resumingVType is true.
   * Otherwise, the instructions passed to outputEntries will get wrong speculated vtype.
   */
  private val decodeCanAccept = io.fromBackend.decodeCanAccept
  private val resumingVType   = io.fromBackend.resumingVType

  // Modules
  private val vtypeGen = Module(new VTypeGen)

  // cross-module parameters check
  require(
    NumReadBank >= DecodeWidth,
    s"NumReadBank($NumReadBank) should be equal or larger than DecodeWidth($DecodeWidth)"
  )

  // Normal fetch is the only back-pressurable path.
  private val normalFetch = io.in.normalFetch
  private val normalValid = normalFetch.valid
  private val normalBits  = normalFetch.bits
  private val normalFire  = normalFetch.fire

  // Fast fetch is a one-cycle, best-effort pass-through.
  // A high fastFetch.valid guarantees that IBuffer is empty. If fastFire is low,
  // the fast entries are dropped and IBuffer keeps its state unchanged.
  private val fastFetch   = io.in.fastFetch
  private val fastEntries = fastFetch.bits.entries
  private val fastValid   = fastFetch.valid
  private val fastNum     = fastFetch.bits.instrNum
  private val fastHasVSet = fastEntries.map { entry =>
    entry.valid && Seq(VSETIVLI, VSETVL, VSETVLI).map(_ === entry.inst).reduce(_ || _)
  }.reduce(_ || _)
  private val fastReady = !io.flush && !resumingVType && !fastHasVSet && io.empty
  private val fastFire  = fastValid && fastReady

  private val hasFastFetch  = RegNext(fastFire, init = false.B)
  private val fastFetchMask = RegInit(0.U(NumWriteBank.W))
  fastFetchMask := Mux(
    fastFire,
    UIntToMask(fastNum.pad(log2Ceil(NumWriteBank)), NumWriteBank),
    0.U(NumWriteBank.W)
  )
  private val normalLaneValid     = normalBits.valid & ~fastFetchMask.pad(IBufferEnqueueWidth)
  private val normalLaneEnqEnable = normalBits.enqEnable & ~fastFetchMask.pad(IBufferEnqueueWidth)

  // IBuffer is organized as raw registers
  // This is due to IBuffer is a huge queue, read & write port logic should be precisely controlled
  //                             . + + E E E - .
  //                             . + + E E E - .
  //                             . . + E E E - .
  //                             . . + E E E E -
  // As shown above, + means enqueue, - means dequeue, E is current content
  // When dequeue, read port is organized like a banked FIFO
  // Dequeue reads no more than 1 entry from each bank sequentially, this can be exploit to reduce area
  // Enqueue writes cannot benefit from this characteristic unless use a SRAM
  // For detail see Enqueue and Dequeue below
  private val ibuf: Vec[IBufEntry] = RegInit(VecInit.fill(Size)(0.U.asTypeOf(new IBufEntry)))
  private val bankedIBufView: Vec[Vec[IBufEntry]] = VecInit.tabulate(NumReadBank)(bankID =>
    VecInit.tabulate(ReadBankSize)(inBankOffset => ibuf(bankID + inBankOffset * NumReadBank))
  )
  // Normal bypass wire
  private val normalBypassEntries = WireDefault(VecInit.fill(DecodeWidth)(0.U.asTypeOf(Valid(new IBufEntry))))
  // Normal read wire
  private val deqEntries = WireDefault(VecInit.fill(DecodeWidth)(0.U.asTypeOf(Valid(new IBufEntry))))
  // Output register
  private val outputEntries     = RegInit(VecInit.fill(DecodeWidth)(0.U.asTypeOf(Valid(new IBufOutEntry))))
  private val outputEntriesNext = Wire(outputEntries.cloneType)

  private val outputEntriesValidNum =
    PriorityMuxDefault(outputEntries.map(_.valid).zip(Seq.range(1, DecodeWidth).map(_.U)).reverse, 0.U)

  // Between Bank
  private val deqBankPtrVec     = RegInit(VecInit.tabulate(DecodeWidth)(_.U.asTypeOf(new IBufBankPtr)))
  private val deqBankPtrVecNext = Wire(deqBankPtrVec.cloneType)
  private val deqBankPtr        = deqBankPtrVec(0)
  // Inside Bank
  private val deqInBankPtr     = RegInit(VecInit.fill(NumReadBank)(0.U.asTypeOf(new IBufInBankPtr)))
  private val deqInBankPtrNext = Wire(deqInBankPtr.cloneType)

  private val deqPtrVec     = RegInit(VecInit.tabulate(DecodeWidth)(_.U.asTypeOf(new IBufPtr)))
  private val deqPtrVecNext = Wire(deqPtrVec.cloneType)
  private val deqPtr        = deqPtrVec(0)

  private val enqPtrVec = RegInit(VecInit.tabulate(EnqueueWidth)(_.U.asTypeOf(new IBufPtr)))
  private val enqPtrDup = RegInit(VecInit.fill(EnqPtrDupNum)(0.U.asTypeOf(new IBufPtr)))
  private val enqPtr    = enqPtrDup(0)
  // Use the IFU-IBuffer interaction to pre-determine the queue position for each instruction output by IFU.
  private val ifuAlignedEnqPtrVec = RegInit(VecInit.tabulate(EnqueueWidth)(_.U.asTypeOf(new IBufPtr)))

  // No bubble for ibuffer.out, so head.valid is enough
  io.empty := enqPtr === deqPtr && !io.out.head.valid
  XSError(
    !io.out.head.valid && io.out.tail.map(_.valid).reduce(_ || _),
    "Bubble in ibuffer.out"
  )

  XSError(
    normalValid && normalBits.prevIBufEnqPtr =/= enqPtr && !hasFastFetch,
    "The enqueueing behavior of the IBuffer does not match expectations."
  )

  /**
   * For better timing, only spread vtype from [[ibuf]] to [[outputEntries]]
   * Check if there is any VSET{I}VL{I} in enq insts.
   * Disable normal bypass if true.
   */
  private val enqHasVSet: Bool =
    normalValid &&
      (normalLaneValid.asBools lazyZip normalLaneEnqEnable.asBools lazyZip normalBits.instrs).map {
        case (valid, enqEnable, inst) =>
          valid && enqEnable && Seq(VSETIVLI, VSETVL, VSETVLI).map(_ === inst).reduce(_ || _)
      }.reduce(_ || _)

  private val normalNumEnq = Mux(normalFire, PopCount(normalLaneEnqEnable), 0.U)
  private val totalEnqNum  = Mux(fastFire, fastNum, normalNumEnq)

  // empty and decode can accept insts
  private val normalBypass =
    normalFire && enqPtr === deqPtr && decodeCanAccept && !resumingVType && !enqHasVSet

  // Number of normal-path instructions accepted into outputEntries this cycle.
  // Since decode promises accepting insts in order, use priority encoder to simplify the accumulation.
  private val normalNumDeq = Wire(UInt(DecodeWidth.U.getWidth.W))
  private val totalDeqNum  = Mux(fastFire, fastNum, normalNumDeq)

  // counter current number of valid
  private val numValid = distanceBetween(enqPtr, deqPtr)
  // counter next number of valid
  // Fast fetch enqueues and dequeues the same number of entries in one cycle.
  private val nextNumValid = numValid + normalNumEnq - normalNumDeq

  // TODO: Use ParallelAdder to calculate the sum of Seq(Size.U, -numValid, -totalEnqNum, normalNumDeq)
  private val nextNumInvalid = Size.U - nextNumValid
  private val allowEnq       = RegInit(true.B)

  /* prevInstrCount is equal to next cycle's numFromFetch, "prev" means ifu.s3;
   * so compare it with next cycle's number of invalid entries (i.e. nextNumInvalid);
   * the answer is next cycle's ready (NOT considering dequeue behavior and predChecker).
   */
  allowEnq := normalBits.prevInstrCount < nextNumInvalid

  private val enqOffset = VecInit.tabulate(EnqueueWidth)(i => PopCount(normalLaneValid.asBools.take(i)))
  private val enqData   = VecInit.tabulate(EnqueueWidth)(i => Wire(new IBufEntry).fromNormalFetch(normalFetch.bits, i))
  private val enqBankOffset =
    WireDefault(0.U.asTypeOf(Vec(NumWriteBank, Vec(EnqueueWidth / NumWriteBank, UInt(log2Ceil(EnqueueWidth).W)))))
  private val enqBankEntrys =
    WireDefault(0.U.asTypeOf(Vec(NumWriteBank, Vec(EnqueueWidth / NumWriteBank, new IBufEntry))))
  for (i <- 0 until NumWriteBank) {
    for (j <- 0 until EnqueueWidth / NumWriteBank) {
      enqBankOffset(i)(j) := enqOffset(i + NumWriteBank * j)
      enqBankEntrys(i)(j) := enqData(i + NumWriteBank * j)
    }
  }

  // Only one exception is stored at a time.
  private val firstExceptionIdx = RegInit(0.U.asTypeOf(new IBufPtr))
  private val firstException    = RegInit(0.U.asTypeOf(Valid(new IBufExceptionEntry)))

  private val deqHasException    = Wire(Bool())
  private val deqExceptionOffset = Wire(UInt(log2Ceil(DecodeWidth).W))

  // Current Exception Wire
  private val currentException  = Wire(new IBufExceptionEntry).fromNormalFetch(normalFetch.bits)
  private val enqExceptionIndex = PriorityEncoder(normalBits.exceptionMask)

  private val outputEntriesIsNotFull = !outputEntries(DecodeWidth - 1).valid
  private val normalNumBypass        = Wire(UInt(DecodeWidth.U.getWidth.W))
  // when using normal bypass, bypassed entries do not enqueue
  // Timing optimization: only count enqEnable up to MaxNormalBypassNum.
  // - Higher-index enqEnable bits arrive later (longer datapath).
  // - Reducing PopCount width improves timing
  private val normalMaybeBypassNum = Mux(normalFire, PopCount(normalLaneEnqEnable.take(MaxNormalBypassNum)), 0.U)
  when(normalBypass) {
    when(normalMaybeBypassNum >= DecodeWidth.U) {
      normalNumBypass := DecodeWidth.U
    }.otherwise {
      normalNumBypass := normalMaybeBypassNum
    }
  }.otherwise {
    normalNumBypass := 0.U
  }

  when(resumingVType) {
    normalNumDeq := 0.U
  }.elsewhen(decodeCanAccept) {
    normalNumDeq := Mux(normalBypass, normalNumBypass, Mux(numValid >= DecodeWidth.U, DecodeWidth.U, numValid))
  }.elsewhen(outputEntriesIsNotFull) {
    normalNumDeq := Mux(
      numValid >= DecodeWidth.U - outputEntriesValidNum,
      DecodeWidth.U - outputEntriesValidNum,
      numValid
    )
  }.otherwise {
    normalNumDeq := 0.U
  }

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Normal bypass
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // timing optimization：
  // Normal bypass selection relies on the IFU-to-IBuffer pre-alignment contract:
  // valid instructions from IFU are packed contiguously after the leading
  // bank-alignment bubbles. The low bits of enqPtr identify the write-bank
  // offset of the first valid instruction.
  //
  // Example with NumWriteBank = 4 and bankOffset = 3:
  // lane index:  0 1 2 3 4 5 6 7 8 ...
  // valid mark:  0 0 0 1 1 1 1 1 0 ...
  //
  // In general, normal bypass entry idx selects lane idx + bankOffset from the
  // sliding window [idx, idx + NumWriteBank - 1]. This avoids computing
  // enqOffset with a prefix PopCount on the normal bypass path.
  private val normalBypassExceptionMask = Wire(Vec(DecodeWidth, Bool()))
  normalBypassEntries.zipWithIndex.foreach { case (entry, idx) =>
    val bankOffset = enqPtrDup(2).value(log2Ceil(NumWriteBank) - 1, 0)
    val bankOH     = UIntToOH(bankOffset, NumWriteBank)

    val selectedValid = Mux1H(
      bankOH,
      VecInit.tabulate(NumWriteBank)(i =>
        normalLaneValid(i + idx) && normalLaneEnqEnable(i + idx)
      )
    )

    entry.valid := selectedValid && normalFire && !io.flush
    entry.bits := Mux1H(
      bankOH,
      VecInit.tabulate(NumWriteBank)(i =>
        enqData(i + idx)
      )
    )
    normalBypassExceptionMask(idx) := Mux1H(
      bankOH,
      VecInit.tabulate(NumWriteBank)(i =>
        normalBits.exceptionMask(i + idx) && normalLaneValid(i + idx) && normalLaneEnqEnable(i + idx)
      )
    )
  }

  // => Decode Output
  // clean register output
  io.out zip outputEntries foreach { case (io, reg) =>
    io.valid := reg.valid
    io.bits  := reg.bits.toCtrlFlow
  }

  // Non-normal-bypass path: deqEntries → outputEntriesFromDeqNext
  // VTypeGen is fed from this path to keep normal bypass out of the vtype timing path.
  private val outputEntriesFromDeqNext = Wire(outputEntriesNext.cloneType)
  (outputEntriesFromDeqNext lazyZip outputEntries lazyZip deqEntries).zipWithIndex.foreach {
    case ((outFromDeq, outReg, deq), i) =>
      when(decodeCanAccept && !resumingVType) {
        outFromDeq.valid := deq.valid
        outFromDeq.bits := Mux(
          deqHasException && i.U === deqExceptionOffset,
          deq.bits.toIBufOutEntry(firstException.bits),
          deq.bits.toIBufOutEntry(0.U.asTypeOf(firstException.bits))
        )
      }.elsewhen(outputEntriesIsNotFull && !resumingVType) {
        // valid mirrors bits: old entries keep outReg.valid; new entries are
        // valid iff they lie within the normalNumDeq entries dequeued this cycle.
        // normalBypass is always 0 in this branch (!decodeCanAccept), so no need to gate it.
        outFromDeq.valid := Mux(
          i.U < outputEntriesValidNum,
          outReg.valid,
          (i.U - outputEntriesValidNum) < normalNumDeq
        )
        outFromDeq.bits := Mux(
          i.U < outputEntriesValidNum,
          outReg.bits,
          Mux(
            deqHasException && (i.U - outputEntriesValidNum) === deqExceptionOffset,
            VecInit(deqEntries.take(i + 1).map(_.bits))(i.U - outputEntriesValidNum).toIBufOutEntry(
              firstException.bits
            ),
            VecInit(deqEntries.take(i + 1).map(_.bits))(i.U - outputEntriesValidNum).toIBufOutEntry(
              0.U.asTypeOf(firstException.bits)
            )
          )
        )
      }.otherwise {
        outFromDeq := outReg
      }
  }

  // Final outputEntriesNext: fast fetch overrides the normal deq-based output.
  (outputEntriesNext lazyZip outputEntriesFromDeqNext lazyZip normalBypassEntries).zipWithIndex.foreach {
    case ((outNext, outFromDeq, normalBypassEntry), i) =>
      val fastLaneValid =
        if (i < fastEntries.length) fastEntries(i).valid else false.B
      val fastLaneBits =
        if (i < fastEntries.length) Wire(new IBufOutEntry).fromFastFetch(io.in.fastFetch.bits, i)
        else 0.U.asTypeOf(new IBufOutEntry)
      when(fastFire) {
        outNext.valid := fastLaneValid
        outNext.bits  := fastLaneBits
      }.elsewhen(normalBypass) {
        outNext.valid := normalBypassEntry.valid
        outNext.bits := Mux(
          normalBypassExceptionMask(i),
          normalBypassEntry.bits.toIBufOutEntry(currentException),
          normalBypassEntry.bits.toIBufOutEntry(0.U.asTypeOf(currentException))
        )
      }.otherwise {
        outNext := outFromDeq
      }
  }

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // VTypeGen's connection
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  vtypeGen.in.canUpdateVType  := !resumingVType && (decodeCanAccept || outputEntriesIsNotFull)
  vtypeGen.in.walkToArchVType := io.fromBackend.walkToArchVType
  vtypeGen.in.walkVType       := io.fromBackend.walkVType
  vtypeGen.in.vsetvlVType     := io.fromBackend.vsetvlVType
  vtypeGen.in.commitVType     := io.fromBackend.commitVType
  for (i <- 0 until DecodeWidth) {
    when(resumingVType) {
      vtypeGen.in.insts(i).valid := false.B
    }.elsewhen(decodeCanAccept) {
      vtypeGen.in.insts(i).valid := outputEntriesFromDeqNext(i).valid
    }.elsewhen(outputEntriesIsNotFull) {
      // Only lanes carrying a genuinely new deq entry (within normalNumDeq) are fed to VTypeGen.
      vtypeGen.in.insts(i).valid := Mux(
        i.U < outputEntriesValidNum,
        false.B,
        (i.U - outputEntriesValidNum) < normalNumDeq
      )
    }.otherwise {
      vtypeGen.in.insts(i).valid := false.B
    }
    vtypeGen.in.insts(i).bits := outputEntriesFromDeqNext(i).bits.inst
  }

  // update vtype field, overwrite the assignment of outputEntriesNext above
  for (i <- outputEntriesNext.indices) {
    when(decodeCanAccept && !resumingVType) {
      // vtype can not be from normal bypass
      outputEntriesNext(i).bits.vtype     := vtypeGen.out.vtype(i)
      outputEntriesNext(i).bits.specvtype := vtypeGen.out.specvtype(i)
    }.elsewhen(outputEntriesIsNotFull && !resumingVType) {
      when(i.U < outputEntriesValidNum) {
        outputEntriesNext(i).bits.vtype     := outputEntries(i).bits.vtype
        outputEntriesNext(i).bits.specvtype := outputEntries(i).bits.specvtype
      }.otherwise {
        outputEntriesNext(i).bits.vtype     := vtypeGen.out.vtype(i)
        outputEntriesNext(i).bits.specvtype := vtypeGen.out.specvtype(i)
      }
    }
  }

  outputEntries := outputEntriesNext

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Enqueue
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  normalFetch.ready := allowEnq
  // Data
  // Rebase enqueue pointers to the IFU pre-aligned lane coordinate.
  for (bank <- 0 until NumWriteBank) {
    (0 until WriteBankSize).foreach { idx =>
      // Select
      val validOH = (0 until EnqueueWidth / NumWriteBank).map { j =>
        val normalMatch = ifuAlignedEnqPtrVec(bank + NumWriteBank * j).value === (bank + idx * NumWriteBank).asUInt
        normalLaneValid(bank + NumWriteBank * j) && normalLaneEnqEnable(bank + NumWriteBank * j) && normalMatch
      } // Should be OneHot
      val normalBypassMatch = (0 until DecodeWidth).map(k => enqPtrVec(k).value === (bank + idx * NumWriteBank).U)
      val wen = validOH.reduce(_ || _) && normalFire && !io.flush && !(normalBypassMatch.reduce(_ || _) && normalBypass)

      // Write port
      // Each IBuffer entry has a PredictWidth -> 1 Mux
      val writeEntry = Mux1H(validOH, enqBankEntrys(bank))
      ibuf(bank + idx * NumWriteBank) := Mux(wen, writeEntry, ibuf(bank + idx * NumWriteBank))
      // Debug Assertion
      XSError(normalValid && PopCount(validOH) > 1.asUInt, "validOH is not OneHot")
    }
  }

  // Pointer maintenance
  // IBuffer enqueue pointers advance sequentially. Therefore the low bits used as
  // the write-bank index cycle as 0, 1, ..., NumWriteBank-1, then wrap.
  // IFU output lanes are aligned to the same write-bank partitioning, so each IFU
  // lane's target enqueue pointer can be precomputed from the current enqPtr.
  //
  // This assumes the IBuffer size is compatible with NumWriteBank, so the pointer
  // low-bit bank cycle is preserved across wrap-around.
  private val baseAlignedPtr = Wire(new IBufPtr)
  private val alignedMask    = (~(NumWriteBank - 1).U(log2Ceil(Size).W)).asUInt
  baseAlignedPtr       := (enqPtrDup(1) + totalEnqNum)
  baseAlignedPtr.value := (enqPtrDup(1) + totalEnqNum).value & alignedMask
  when((normalFire || fastFire) && !io.flush) {
    enqPtrVec := VecInit(enqPtrVec.map(_ + totalEnqNum))
    enqPtrDup.map(_ := enqPtrDup(1) + totalEnqNum)
    ifuAlignedEnqPtrVec := VecInit((0 until EnqueueWidth).map(i => baseAlignedPtr + i.U))
  }

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Dequeue
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  private val outputEntriesValidNumNext = Wire(UInt(DecodeWidth.U.getWidth.W))
  XSError(outputEntriesValidNumNext > DecodeWidth.U, "Ibuffer: outputEntriesValidNumNext > DecodeWidth.U")
  when(fastFire) {
    outputEntriesValidNumNext := fastNum
  }.elsewhen(resumingVType) {
    outputEntriesValidNumNext := outputEntriesValidNum
  }.elsewhen(decodeCanAccept) {
    outputEntriesValidNumNext := Mux(normalBypass, normalNumBypass, normalNumDeq)
  }.elsewhen(outputEntriesIsNotFull) {
    outputEntriesValidNumNext := outputEntriesValidNum + normalNumDeq
  }.otherwise {
    outputEntriesValidNumNext := outputEntriesValidNum
  }
  // Data
  // Read port
  // 2-stage, NumReadBank * (bankSize -> 1) + NumReadBank -> 1
  // Should be better than Size -> 1 in area, with no significant latency increase
  private val readStage1: Vec[IBufEntry] =
    VecInit.tabulate(NumReadBank)(bankID => Mux1H(UIntToOH(deqInBankPtr(bankID).value), bankedIBufView(bankID)))
  for (i <- 0 until DecodeWidth) {
    // Genuine dequeue validity: only the normalNumDeq entries actually consumed this cycle are valid.
    // Under normal bypass no real data is read from ibuf, so the deq read window is invalid.
    deqEntries(i).valid := (i.U < normalNumDeq) && !normalBypass && !fastFire
    deqEntries(i).bits  := Mux1H(UIntToOH(deqBankPtrVec(i).value), readStage1)
  }
  // Pointer maintenance
  deqBankPtrVecNext := VecInit(deqBankPtrVec.map(_ + totalDeqNum))
  deqPtrVecNext     := VecInit(deqPtrVec.map(_ + totalDeqNum))
  deqInBankPtrNext.zip(deqInBankPtr).zipWithIndex.foreach { case ((ptrNext, ptr), idx) =>
    // bank n holds the (n - deqBankPtr mod NumReadBank)-th dequeued entry;
    // it advances iff that entry is within the totalDeqNum consumed this cycle
    val validIdx = Mux(
      idx.asUInt >= deqBankPtr.value,
      idx.asUInt - deqBankPtr.value,
      ((idx + NumReadBank).asUInt - deqBankPtr.value)(DecodeWidth.U.getWidth - 1, 0)
    )(DecodeWidth.U.getWidth - 1, 0)
    val bankAdvance = totalDeqNum > validIdx
    ptrNext := Mux(bankAdvance, ptr + 1.U, ptr)
  }

  // Register the first encountered exceptions into the IBuffer.
  private val receiveExceptionFire = normalFire && !io.flush && !firstException.valid
  private val nextFirstHasException =
    currentException.exceptionType.hasException &&
      (normalBits.exceptionMask & normalLaneEnqEnable).orR &&
      (!normalBypass || normalBypass && !normalBypassExceptionMask.reduce(_ || _))

  when(receiveExceptionFire && nextFirstHasException) {
    firstException.valid := true.B
  }

  when(normalFire && !io.flush && !firstException.valid) {
    firstException.bits := currentException
    firstExceptionIdx   := ifuAlignedEnqPtrVec(enqExceptionIndex)
  }

  // Dequeue the first encountered exceptions to outputEntries.
  private val deqExceptionMatchOH = VecInit((0 until DecodeWidth).map(i =>
    deqPtrVec(i) === firstExceptionIdx && firstException.valid
  ))

  private val deqHasExceptionOH = UIntToMask(normalNumDeq, DecodeWidth) & deqExceptionMatchOH.asUInt
  deqHasException    := deqHasExceptionOH.orR
  deqExceptionOffset := OHToUInt(deqHasExceptionOH)

  // When exceptions are dequeued, clear firstException.valid.
  // Dequeue has higher priority, because once an exception has been processed,
  // later exceptions are no longer relevant.
  when(deqHasException && !resumingVType && (decodeCanAccept || outputEntriesIsNotFull)) {
    firstException.valid := false.B
  }

  XSError(
    deqHasException && decodeCanAccept && normalBypass,
    "exception not dequeue, cannot use normal bypass"
  )
  XSError(PopCount(deqHasExceptionOH) > 1.U, "exception cannot multiHit")

  // Flush
  when(io.flush) {
    allowEnq  := true.B
    enqPtrVec := enqPtrVec.indices.map(_.U.asTypeOf(new IBufPtr))
    enqPtrDup.map(_ := 0.U.asTypeOf(new IBufPtr))
    ifuAlignedEnqPtrVec := ifuAlignedEnqPtrVec.indices.map(_.U.asTypeOf(new IBufPtr))
    deqBankPtrVec       := deqBankPtrVec.indices.map(_.U.asTypeOf(new IBufBankPtr))
    deqInBankPtr        := VecInit.fill(NumReadBank)(0.U.asTypeOf(new IBufInBankPtr))
    deqPtrVec           := deqPtrVec.indices.map(_.U.asTypeOf(new IBufPtr))
    outputEntries.foreach(_.valid := false.B)
    firstException.valid := false.B
  }.otherwise {
    deqPtrVec     := deqPtrVecNext
    deqInBankPtr  := deqInBankPtrNext
    deqBankPtrVec := deqBankPtrVecNext
  }
  io.full := !allowEnq

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // TopDown
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  private def numStages     = 2
  private val topdownStages = RegInit(VecInit.fill(numStages)(0.U.asTypeOf(new FrontendTopDownBundle)))

  topdownStages(0) := normalFetch.bits.topdownInfo
  for (i <- 1 until numStages) {
    topdownStages(i) := topdownStages(i - 1)
  }

  topdownStages.foreach(_.backendRedirectOverride(io.backendRedirectTopdown))

  private val deqValidCount = PopCount(io.out.map(_.valid))
  private val deqWasteCount = DecodeWidth.U - deqValidCount
  private val matchBubble = (TopDownCounters.NumStallReasons.id - 1).U - PriorityEncoder(
    topdownStages.last.reasons.reverse
  )

  io.stallReason.reason.foreach(_ := 0.U)
  for (i <- 0 until DecodeWidth) {
    when(i.U < deqWasteCount) {
      io.stallReason.reason(DecodeWidth - i - 1) := matchBubble
    }
  }

  when(!(deqWasteCount === DecodeWidth.U || topdownStages.last.reasons.asUInt.orR)) {
    // should set reason for FetchFragmentationStall
    // topdownStage.reasons(TopDownCounters.FetchFragmentationStall.id) := true.B
    for (i <- 0 until DecodeWidth) {
      when(i.U < deqWasteCount) {
        io.stallReason.reason(DecodeWidth - i - 1) := TopDownCounters.FetchFragBubble.id.U
      }
    }
  }

  // Debug info
  XSError(
    deqPtr.value =/= deqBankPtr.value + deqInBankPtr(deqBankPtr.value).value * NumReadBank.asUInt,
    "Dequeue PTR mismatch"
  )
  XSError(isBefore(enqPtr, deqPtr) && !isFull(enqPtr, deqPtr), "\ndeqPtr is older than enqPtr!\n")

  XSDebug(io.flush, "IBuffer Flushed\n")

  XSDebug(normalFire, "Enque:\n")
  XSDebug(normalFire, p"MASK=${Binary(normalLaneValid)}\n")
  for (i <- 0 until EnqueueWidth) {
    XSDebug(normalFire, p"PC=${Hexadecimal(normalBits.pc(i).toUInt)} ${Hexadecimal(normalBits.instrs(i))}\n")
  }
  XSDebug(fastFire, p"FastEnq: num=${fastNum}\n")

  for (i <- 0 until DecodeWidth) {
    XSDebug(
      io.out(i).fire,
      p"deq: ${Hexadecimal(io.out(i).bits.instr)} PC=${Hexadecimal(io.out(i).bits.pc)}" +
        p"v=${io.out(i).valid} r=${io.out(i).ready} " +
        p"excpVec=${Binary(io.out(i).bits.exceptionVec.asUInt)} crossPageIPF=${io.out(i).bits.crossPageIPFFix}\n"
    )
  }

  XSDebug(p"numValid: ${numValid}\n")
  XSDebug(p"EnqNum: ${totalEnqNum}\n")
  XSDebug(p"DeqNum: ${totalDeqNum}\n")

  private val perf_afterInit  = RegInit(false.B)
  private val perf_headBubble = RegInit(false.B)
  when(normalFire)(perf_afterInit := true.B)
  when(io.flush) {
    perf_headBubble := true.B
  }.elsewhen(numValid =/= 0.U) {
    perf_headBubble := false.B
  }

  private val perf_instrHungry = perf_afterInit && (numValid === 0.U) && !perf_headBubble

  QueuePerf(Size, numValid, !allowEnq)
  XSPerfAccumulate("flush", io.flush)
  XSPerfAccumulate("hungry", perf_instrHungry)

  // FIXME: this name is bad
  private val perf_ibufferIDWidthHvButNotFull =
    perf_afterInit && (numValid =/= 0.U) && (numValid < DecodeWidth.U) && !perf_headBubble

  XSPerfAccumulate("ibuffer_IDWidth_hvButNotFull", perf_ibufferIDWidthHvButNotFull)

  private val perf_fetchBubble = Mux(
    fastFire,
    0.U,
    Mux(decodeCanAccept && !resumingVType && !perf_headBubble, DecodeWidth.U - normalNumDeq, 0.U)
  )

  private val perf_fetchLatency =
    decodeCanAccept && !resumingVType && !perf_headBubble && normalNumDeq === 0.U && !fastFire

  XSPerfAccumulate("if_fetch_bubble", perf_fetchBubble)
  XSPerfAccumulate("if_fetch_bubble_eq_max", perf_fetchLatency)

  val perfEvents: Seq[(String, UInt)] = Seq(
    ("IBuffer_Flushed  ", io.flush),
    ("IBuffer_hungry   ", perf_instrHungry),
    ("IBuffer_1_4_valid", (numValid > (0 * (Size / 4)).U) & (numValid < (1 * (Size / 4)).U)),
    ("IBuffer_2_4_valid", (numValid >= (1 * (Size / 4)).U) & (numValid < (2 * (Size / 4)).U)),
    ("IBuffer_3_4_valid", (numValid >= (2 * (Size / 4)).U) & (numValid < (3 * (Size / 4)).U)),
    ("IBuffer_4_4_valid", (numValid >= (3 * (Size / 4)).U) & (numValid < (4 * (Size / 4)).U)),
    ("IBuffer_full     ", numValid.andR),
    ("Front_Bubble     ", perf_fetchBubble),
    ("Fetch_Latency_Bound", perf_fetchLatency)
  )
  generatePerfEvent()
}
