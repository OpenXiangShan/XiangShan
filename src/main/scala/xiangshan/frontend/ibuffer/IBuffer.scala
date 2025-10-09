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
import org.chipsalliance.cde.config.Parameters
import utility.HasCircularQueuePtrHelper
import utility.HasPerfEvents
import utility.ParallelPriorityEncoder
import utility.PriorityMuxDefault
import utility.QueuePerf
import utility.UIntToMask
import utility.XSDebug
import utility.XSError
import utility.XSPerfAccumulate
import xiangshan.CtrlFlow
import xiangshan.StallReasonIO
import xiangshan.TopDownCounters
import xiangshan.frontend.ExceptionType
import xiangshan.frontend.FetchToIBuffer
import xiangshan.frontend.FrontendTopDownBundle

class IBuffer(implicit p: Parameters) extends IBufferModule with HasCircularQueuePtrHelper with HasPerfEvents {
  class IBufferIO extends Bundle {
    val flush:           Bool                        = Input(Bool())
    val in:              DecoupledIO[FetchToIBuffer] = Flipped(DecoupledIO(new FetchToIBuffer))
    val out:             Vec[DecoupledIO[CtrlFlow]]  = Vec(DecodeWidth, DecoupledIO(new CtrlFlow))
    val full:            Bool                        = Output(Bool())
    val decodeCanAccept: Bool                        = Input(Bool())

    // FIXME: topdown, why not use a bundle?
    val ControlRedirect:      Bool          = Input(Bool())
    val ControlBTBMissBubble: Bool          = Input(Bool())
    val TAGEMissBubble:       Bool          = Input(Bool())
    val SCMissBubble:         Bool          = Input(Bool())
    val ITTAGEMissBubble:     Bool          = Input(Bool())
    val RASMissBubble:        Bool          = Input(Bool())
    val MemVioRedirect:       Bool          = Input(Bool())
    val stallReason:          StallReasonIO = new StallReasonIO(DecodeWidth)
  }

  val io: IBufferIO = IO(new IBufferIO)

  // io alias
  private val decodeCanAccept = io.decodeCanAccept

  // cross-module parameters check
  require(
    NumReadBank >= DecodeWidth,
    s"NumReadBank($NumReadBank) should be equal or larger than DecodeWidth($DecodeWidth)"
  )

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
  private val bankedIBufWriteWire: Vec[Vec[IBufEntry]] = WireDefault(VecInit.tabulate(NumWriteBank)(bankID =>
    VecInit.tabulate(WriteBankSize)(inBankOffset => 0.U.asTypeOf(new IBufEntry))
  ))

  // Bypass wire
  private val bypassEntries = WireDefault(VecInit.fill(DecodeWidth)(0.U.asTypeOf(Valid(new IBufEntry))))
  // Normal read wire
  private val deqEntries = WireDefault(VecInit.fill(DecodeWidth)(0.U.asTypeOf(Valid(new IBufEntry))))
  // Output register
  private val outputEntries = RegInit(VecInit.fill(DecodeWidth)(0.U.asTypeOf(Valid(new IBufOutEntry))))
  private val outputEntriesValidNum =
    PriorityMuxDefault(outputEntries.map(_.valid).zip(Seq.range(1, DecodeWidth).map(_.U)).reverse, 0.U)

  // Between Bank
  private val deqBankPtrVec: Vec[IBufBankPtr] = RegInit(VecInit.tabulate(DecodeWidth)(_.U.asTypeOf(new IBufBankPtr)))
  private val deqBankPtrVecNext: Vec[IBufBankPtr] = Wire(deqBankPtrVec.cloneType)
  private val deqBankPtr:        IBufBankPtr      = deqBankPtrVec(0)
  // Inside Bank
  private val deqInBankPtr:     Vec[IBufInBankPtr] = RegInit(VecInit.fill(NumReadBank)(0.U.asTypeOf(new IBufInBankPtr)))
  private val deqInBankPtrNext: Vec[IBufInBankPtr] = Wire(deqInBankPtr.cloneType)

  private val deqPtrVec:     Vec[IBufPtr] = RegInit(VecInit.tabulate(DecodeWidth)(_.U.asTypeOf(new IBufPtr)))
  private val deqPtrVecNext: Vec[IBufPtr] = Wire(deqPtrVec.cloneType)
  private val deqPtr:        IBufPtr      = deqPtrVec(0)

  private val enqPtrVec = RegInit(VecInit.tabulate(EnqueueWidth)(_.U.asTypeOf(new IBufPtr)))
  private val enqPtr    = enqPtrVec(0)

  // use Ifu.s4_shiftNum
  private val alignShiftNum = io.in.bits.prevIBufEnqPtr.value(log2Ceil(NumWriteBank) - 1, 0)
  // FIXME: only pass one exception after flush, is it correct?
  private val canReceiveException = RegInit(true.B)
  // First Exception Register
  private val firstIBufferExceptionType: UInt    = RegInit(IBufferExceptionType.None)
  private val firstBackendException:     Bool    = RegInit(false.B)
  private val firstExceptionIBufIdx:     IBufPtr = RegInit(0.U.asTypeOf(new IBufPtr))
  private val firstHasException:         Bool    = RegInit(false.B)
  private val deqHasException:           Bool    = Wire(Bool())
  private val deqExceptionOffset:        UInt    = Wire(UInt(log2Ceil(DecodeWidth).W))

  // Current Exception Wire
  private val currentExceptionType    = io.in.bits.exceptionType
  private val currentCrossPageIPFFix  = io.in.bits.crossPageIPFFix
  private val currentBackendException = io.in.bits.backendException

  private val bypassHasException:         Bool = Wire(Bool())
  private val bypassExceptionOffset:      UInt = Wire(UInt(log2Ceil(DecodeWidth).W))
  private val bypassIBufferExceptionType: UInt = Wire(IBufferExceptionType())
  private val bypassBackendException:     Bool = Wire(Bool())

  XSError(
    io.in.valid && io.in.bits.prevIBufEnqPtr =/= enqPtr,
    "The enqueueing behavior of the IBuffer does not match expectations."
  )

  private val numTryEnq = WireDefault(0.U)
  private val numEnq    = Mux(io.in.fire, numTryEnq, 0.U)

  // empty and decode can accept insts
  private val useBypass = enqPtr === deqPtr && decodeCanAccept

  // The number of decode accepted insts.
  // Since decode promises accepting insts in order, use priority encoder to simplify the accumulation.
  private val numOut = Wire(UInt(DecodeWidth.U.getWidth.W))
  private val numDeq = numOut

  // counter current number of valid
  private val numValid = distanceBetween(enqPtr, deqPtr)
  // counter next number of valid
  private val nextNumValid = numValid + numEnq - numDeq
  private val numFromFetch = Mux(io.in.valid, PopCount(io.in.bits.enqEnable), 0.U)

  // TODO: Use ParallelAdder to calculate the sum of Seq(Size.U, -numValid, -numEnq, numDeq)
  private val nextNumInvalid = Size.U - nextNumValid
  private val allowEnq       = RegInit(true.B)

  /* prevInstrCount is equal to next cycle's numFromFetch, "prev" means ifu.s3;
   * so compare it with next cycle's number of invalid entries (i.e. nextNumInvalid);
   * the answer is next cycle's ready (NOT considering dequeue behavior and predChecker).
   */
  allowEnq := io.in.bits.prevInstrCount < nextNumInvalid

  private val enqOffset = VecInit.tabulate(EnqueueWidth)(i => PopCount(io.in.bits.valid.asBools.take(i)))
  private val enqData   = VecInit.tabulate(EnqueueWidth)(i => Wire(new IBufEntry).fromFetch(io.in.bits, i))
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

  private val outputEntriesIsNotFull = !outputEntries(DecodeWidth - 1).valid
  private val numBypass              = Wire(UInt(DecodeWidth.U.getWidth.W))
  // when using bypass, bypassed entries do not enqueue
  when(useBypass) {
    when(numFromFetch >= DecodeWidth.U) {
      numBypass := DecodeWidth.U
    }.otherwise {
      numBypass := numFromFetch
    }
  }.otherwise {
    numBypass := 0.U
  }
  numTryEnq := numFromFetch

  when(decodeCanAccept) {
    numOut := Mux(useBypass, numBypass, Mux(numValid >= DecodeWidth.U, DecodeWidth.U, numValid))
  }.elsewhen(outputEntriesIsNotFull) {
    numOut := Mux(numValid >= DecodeWidth.U - outputEntriesValidNum, DecodeWidth.U - outputEntriesValidNum, numValid)
  }.otherwise {
    numOut := 0.U
  }

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Bypass
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  bypassEntries.zipWithIndex.foreach { case (entry, idx) =>
    // Select
    val validOH = (0 until MaxBypassNum).map { i =>
      io.in.bits.valid(i) &&
      io.in.bits.enqEnable(i) &&
      enqOffset(i) === idx.asUInt
    } // Should be OneHot
    entry.valid := validOH.reduce(_ || _) && io.in.fire && !io.flush
    entry.bits  := Mux1H(validOH, enqData)

    // Debug Assertion
    XSError(io.in.valid && PopCount(validOH) > 1.asUInt, "validOH is not OneHot")
  }

  // => Decode Output
  // clean register output
  io.out zip outputEntries foreach { case (io, reg) =>
    io.valid := reg.valid
    io.bits  := reg.bits.toCtrlFlow
  }
  (outputEntries zip bypassEntries).zipWithIndex.foreach { case ((out, bypass), i) =>
    when(decodeCanAccept) {
      when(useBypass && io.in.valid) {
        out.valid := bypass.valid
        // use bypass exceptions
        when(canReceiveException && bypassHasException && i.U === bypassExceptionOffset) {
          out.bits := bypass.bits.toIBufOutEntry(bypassIBufferExceptionType, bypassBackendException)
        }.otherwise {
          out.bits := bypass.bits.toIBufOutEntry(IBufferExceptionType.None, false.B)
        }
      }.otherwise {
        out.valid := deqEntries(i).valid
        // use first exceptions
        when(deqHasException && i.U === deqExceptionOffset) {
          out.bits := deqEntries(i).bits.toIBufOutEntry(firstIBufferExceptionType, firstBackendException)
        }.otherwise {
          out.bits := deqEntries(i).bits.toIBufOutEntry(IBufferExceptionType.None, false.B)
        }
      }
    }.elsewhen(outputEntriesIsNotFull) {
      out.valid := deqEntries(i).valid
      out.bits := Mux(
        i.U < outputEntriesValidNum,
        out.bits,
        // use first exceptions, match offset at i - outputEntriesValidNum
        Mux(
          deqHasException && (i.U - outputEntriesValidNum) === deqExceptionOffset,
          VecInit(deqEntries.take(i + 1).map(_.bits))(i.U - outputEntriesValidNum).toIBufOutEntry(
            firstIBufferExceptionType,
            firstBackendException
          ),
          VecInit(deqEntries.take(i + 1).map(_.bits))(i.U - outputEntriesValidNum).toIBufOutEntry(
            IBufferExceptionType.None,
            false.B
          )
        )
      )
    }
  }

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Enqueue
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  io.in.ready := allowEnq
  // Data
  for (bank <- 0 until NumWriteBank) {
    bankedIBufWriteWire(bank).zipWithIndex.foreach { case (entry, idx) =>
      // Select
      val validOH = (0 until EnqueueWidth / NumWriteBank).map { j =>
        val normalMatch = enqPtrVec(enqBankOffset(bank)(j)).value === (bank + idx * NumWriteBank).asUInt
        io.in.bits.valid(bank + NumWriteBank * j) && io.in.bits.enqEnable(bank + NumWriteBank * j) && normalMatch
      } // Should be OneHot
      val useBypassMatch = (0 until DecodeWidth).map(k => enqPtrVec(k).value === (bank + idx * NumWriteBank).U)
      val wen = validOH.reduce(_ || _) && io.in.fire && !io.flush && !(useBypassMatch.reduce(_ || _) && useBypass)

      // Write port
      // Each IBuffer entry has a PredictWidth -> 1 Mux
      val writeEntry = Mux1H(validOH, enqBankEntrys(bank))
      ibuf(bank + idx * NumWriteBank) := Mux(wen, writeEntry, ibuf(bank + idx * NumWriteBank))
      // Debug Assertion
      XSError(io.in.valid && PopCount(validOH) > 1.asUInt, "validOH is not OneHot")
    }
  }

  // Pointer maintenance
  when(io.in.fire && !io.flush) {
    enqPtrVec := VecInit(enqPtrVec.map(_ + numTryEnq))
  }

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Dequeue
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  private val outputEntriesValidNumNext = Wire(UInt(DecodeWidth.U.getWidth.W))
  XSError(outputEntriesValidNumNext > DecodeWidth.U, "Ibuffer: outputEntriesValidNumNext > DecodeWidth.U")
  private val validVec = UIntToMask(outputEntriesValidNumNext(DecodeWidth.U.getWidth - 1, 0), DecodeWidth)
  when(decodeCanAccept) {
    outputEntriesValidNumNext := Mux(useBypass, numBypass, numDeq)
  }.elsewhen(outputEntriesIsNotFull) {
    outputEntriesValidNumNext := outputEntriesValidNum + numDeq
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
    deqEntries(i).valid := validVec(i)
    deqEntries(i).bits  := Mux1H(UIntToOH(deqBankPtrVec(i).value), readStage1)
  }
  // Pointer maintenance
  deqBankPtrVecNext := VecInit(deqBankPtrVec.map(_ + numDeq))
  deqPtrVecNext     := VecInit(deqPtrVec.map(_ + numDeq))
  deqInBankPtrNext.zip(deqInBankPtr).zipWithIndex.foreach { case ((ptrNext, ptr), idx) =>
    // validVec[k] == bankValid[deqBankPtr + k]
    // So bankValid[n] == validVec[n - deqBankPtr]
    val validIdx = Mux(
      idx.asUInt >= deqBankPtr.value,
      idx.asUInt - deqBankPtr.value,
      ((idx + NumReadBank).asUInt - deqBankPtr.value)(DecodeWidth.U.getWidth - 1, 0)
    )(DecodeWidth.U.getWidth - 1, 0)
    val bankAdvance = numOut > validIdx
    ptrNext := Mux(bankAdvance, ptr + 1.U, ptr)
  }

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Exception
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  private val rawRVCII = // restore to raw mask
    (io.in.bits.illegalInstr.asUInt & io.in.bits.enqEnable) >> alignShiftNum
  // bypass the first met exception to outputEntries
  private val bypassRVCII       = rawRVCII(DecodeWidth - 1, 0)
  private val bypassRVCIIOffset = ParallelPriorityEncoder(bypassRVCII)
  private val bypassHasRVCII    = bypassRVCII.orR

  private val bypassHasExceptionWithoutRVCII =
    currentExceptionType.hasException ||
      currentBackendException || // FIXME: is this necessary?
      currentCrossPageIPFFix

  bypassHasException :=
    bypassHasExceptionWithoutRVCII ||
      bypassHasRVCII

  // when bypassExceptionWithoutRVCII happens, mark the exception only on first instruction
  // otherwise, mark the exception on rvcIll's offset
  bypassExceptionOffset  := Mux(bypassHasExceptionWithoutRVCII, 0.U, bypassRVCIIOffset)
  bypassBackendException := currentBackendException
  bypassIBufferExceptionType := Mux(
    bypassHasExceptionWithoutRVCII,
    IBufferExceptionType.cvtFromFetchExcpAndCrossPageAndRVCII(
      currentExceptionType,
      currentCrossPageIPFFix,
      bypassRVCII(0) // rvcIll may happen on offset 0
    ),
    IBufferExceptionType.cvtFromFetchExcpAndCrossPageAndRVCII(
      ExceptionType.None,
      false.B,
      bypassHasRVCII
    )
  )

  // register the first met exception
  private val nextFirstRVCII       = rawRVCII(FetchBlockInstNum - 1, 0)
  private val nextFirstRVCIIOffset = ParallelPriorityEncoder(nextFirstRVCII)
  private val nextFirstHasRVCII    = nextFirstRVCII.orR

  private val nextFirstExceptionOffset      = Mux(bypassHasExceptionWithoutRVCII, 0.U, nextFirstRVCIIOffset)
  private val nextFirstBackendException     = WireDefault(false.B)
  private val nextFirstExceptionIBufIdx     = WireDefault(firstExceptionIBufIdx)
  private val nextFirstIBufferExceptionType = WireDefault(IBufferExceptionType.None)

  private val receiveExceptionFire  = io.in.fire && !io.flush && canReceiveException
  private val nextFirstHasException = nextFirstIBufferExceptionType =/= IBufferExceptionType.None

  // exceptions are registered in IBuffer, set firstHasException
  when(receiveExceptionFire && nextFirstHasException) {
    firstHasException := true.B
  }

  // exceptions are received(bypass or registered), unset canReceiveException until flush
  when(receiveExceptionFire &&
    (nextFirstHasException || (useBypass && bypassHasException))) {
    canReceiveException := false.B
  }

  // when useBypass and no exception in range(DecodeWidth - 1, 0),
  // and rvcIll happens out of range(DecodeWidth - 1, 0),
  // rvcIll should be registered
  when(useBypass) {
    when(!bypassHasException && nextFirstHasRVCII) {
      nextFirstBackendException     := false.B
      nextFirstExceptionIBufIdx     := enqPtrVec(nextFirstRVCIIOffset)
      nextFirstIBufferExceptionType := IBufferExceptionType.RvcII
    }.otherwise {
      nextFirstBackendException     := false.B
      nextFirstExceptionIBufIdx     := firstExceptionIBufIdx
      nextFirstIBufferExceptionType := IBufferExceptionType.None
    }
  }.otherwise {
    nextFirstBackendException := currentBackendException
    nextFirstExceptionIBufIdx := enqPtrVec(nextFirstExceptionOffset)
    nextFirstIBufferExceptionType := Mux(
      bypassHasExceptionWithoutRVCII,
      IBufferExceptionType.cvtFromFetchExcpAndCrossPageAndRVCII(
        currentExceptionType,
        currentCrossPageIPFFix,
        bypassRVCII(0) // rvcIll may happen on offset 0
      ),
      IBufferExceptionType.cvtFromFetchExcpAndCrossPageAndRVCII(
        ExceptionType.None,
        false.B,
        nextFirstHasRVCII
      )
    )
  }
  when(canReceiveException) {
    firstBackendException     := nextFirstBackendException
    firstExceptionIBufIdx     := nextFirstExceptionIBufIdx
    firstIBufferExceptionType := nextFirstIBufferExceptionType
  }

  // Exceptions to outputEntries
  private val deqExceptionMatchOH = VecInit((0 until DecodeWidth).map(i =>
    deqPtrVec(i) === firstExceptionIBufIdx && firstHasException
  ))
  private val deqHasExceptionOH = UIntToMask(numDeq, DecodeWidth) & deqExceptionMatchOH.asUInt
  deqHasException    := deqHasExceptionOH.orR
  deqExceptionOffset := OHToUInt(deqHasExceptionOH)

  // exceptions dequeue, unset firstHasException
  when(deqHasException && (decodeCanAccept || outputEntriesIsNotFull)) {
    firstHasException := false.B
  }

  XSError(
    deqHasException && decodeCanAccept && useBypass,
    "exception not dequeue, cannot use bypass"
  )
  XSError(PopCount(deqHasExceptionOH) > 1.U, "exception cannot multiHit")

  // Flush
  when(io.flush) {
    allowEnq      := true.B
    enqPtrVec     := enqPtrVec.indices.map(_.U.asTypeOf(new IBufPtr))
    deqBankPtrVec := deqBankPtrVec.indices.map(_.U.asTypeOf(new IBufBankPtr))
    deqInBankPtr  := VecInit.fill(NumReadBank)(0.U.asTypeOf(new IBufInBankPtr))
    deqPtrVec     := deqPtrVec.indices.map(_.U.asTypeOf(new IBufPtr))
    outputEntries.foreach(_.valid := false.B)
    firstIBufferExceptionType := IBufferExceptionType.None
    firstExceptionIBufIdx     := 0.U.asTypeOf(new IBufPtr())
    firstBackendException     := false.B
    firstHasException         := false.B
    canReceiveException       := true.B
  }.otherwise {
    deqPtrVec     := deqPtrVecNext
    deqInBankPtr  := deqInBankPtrNext
    deqBankPtrVec := deqBankPtrVecNext
  }
  io.full := !allowEnq

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // TopDown
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  private val topdownStage = RegInit(0.U.asTypeOf(new FrontendTopDownBundle))
  topdownStage := io.in.bits.topdownInfo
  when(io.flush) {
    when(io.ControlRedirect) {
      when(io.ControlBTBMissBubble) {
        topdownStage.reasons(TopDownCounters.BTBMissBubble.id) := true.B
      }.elsewhen(io.TAGEMissBubble) {
        topdownStage.reasons(TopDownCounters.TAGEMissBubble.id) := true.B
      }.elsewhen(io.SCMissBubble) {
        topdownStage.reasons(TopDownCounters.SCMissBubble.id) := true.B
      }.elsewhen(io.ITTAGEMissBubble) {
        topdownStage.reasons(TopDownCounters.ITTAGEMissBubble.id) := true.B
      }.elsewhen(io.RASMissBubble) {
        topdownStage.reasons(TopDownCounters.RASMissBubble.id) := true.B
      }
    }.elsewhen(io.MemVioRedirect) {
      topdownStage.reasons(TopDownCounters.MemVioRedirectBubble.id) := true.B
    }.otherwise {
      topdownStage.reasons(TopDownCounters.OtherRedirectBubble.id) := true.B
    }
  }

  private val matchBubble   = Wire(UInt(log2Up(TopDownCounters.NumStallReasons.id).W))
  private val deqValidCount = PopCount(validVec.asBools)
  private val deqWasteCount = DecodeWidth.U - deqValidCount
  matchBubble := (TopDownCounters.NumStallReasons.id - 1).U - PriorityEncoder(topdownStage.reasons.reverse)

  io.stallReason.reason.foreach(_ := 0.U)
  for (i <- 0 until DecodeWidth) {
    when(i.U < deqWasteCount) {
      io.stallReason.reason(DecodeWidth - i - 1) := matchBubble
    }
  }

  when(!(deqWasteCount === DecodeWidth.U || topdownStage.reasons.asUInt.orR)) {
    // should set reason for FetchFragmentationStall
    // topdownStage.reasons(TopDownCounters.FetchFragmentationStall.id) := true.B
    for (i <- 0 until DecodeWidth) {
      when(i.U < deqWasteCount) {
        io.stallReason.reason(DecodeWidth - i - 1) := TopDownCounters.FetchFragBubble.id.U
      }
    }
  }

  when(io.stallReason.backReason.valid) {
    io.stallReason.reason.map(_ := io.stallReason.backReason.bits)
  }

  // Debug info
  XSError(
    deqPtr.value =/= deqBankPtr.value + deqInBankPtr(deqBankPtr.value).value * NumReadBank.asUInt,
    "Dequeue PTR mismatch"
  )
  XSError(isBefore(enqPtr, deqPtr) && !isFull(enqPtr, deqPtr), "\ndeqPtr is older than enqPtr!\n")

  XSDebug(io.flush, "IBuffer Flushed\n")

  XSDebug(io.in.fire, "Enque:\n")
  XSDebug(io.in.fire, p"MASK=${Binary(io.in.bits.valid)}\n")
  for (i <- 0 until EnqueueWidth) {
    XSDebug(io.in.fire, p"PC=${Hexadecimal(io.in.bits.pc(i).toUInt)} ${Hexadecimal(io.in.bits.instrs(i))}\n")
  }

  for (i <- 0 until DecodeWidth) {
    XSDebug(
      io.out(i).fire,
      p"deq: ${Hexadecimal(io.out(i).bits.instr)} PC=${Hexadecimal(io.out(i).bits.pc)}" +
        p"v=${io.out(i).valid} r=${io.out(i).ready} " +
        p"excpVec=${Binary(io.out(i).bits.exceptionVec.asUInt)} crossPageIPF=${io.out(i).bits.crossPageIPFFix}\n"
    )
  }

  XSDebug(p"numValid: ${numValid}\n")
  XSDebug(p"EnqNum: ${numEnq}\n")
  XSDebug(p"DeqNum: ${numDeq}\n")

  private val perf_afterInit  = RegInit(false.B)
  private val perf_headBubble = RegInit(false.B)
  when(io.in.fire)(perf_afterInit := true.B)
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

  private val perf_fetchBubble = Mux(decodeCanAccept && !perf_headBubble, DecodeWidth.U - numOut, 0.U)

  private val perf_fetchLatency = decodeCanAccept && !perf_headBubble && numOut === 0.U

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
