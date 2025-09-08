/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package xiangshan.frontend

import chisel3._
import chisel3.util._
import ftq.FtqPtr
import org.chipsalliance.cde.config.Parameters
import utility._
import utils._
import xiangshan._

class IBufPtr(implicit p: Parameters) extends CircularQueuePtr[IBufPtr](p => p(XSCoreParamsKey).IBufSize) {}

class IBufInBankPtr(implicit p: Parameters) extends CircularQueuePtr[IBufInBankPtr](p =>
      p(XSCoreParamsKey).IBufSize / p(XSCoreParamsKey).IBufReadBank
    ) {}

class IBufBankPtr(implicit p: Parameters) extends CircularQueuePtr[IBufBankPtr](p => p(XSCoreParamsKey).IBufReadBank) {}

class IBufferIO(implicit p: Parameters) extends XSBundle {
  val flush                = Input(Bool())
  val ControlRedirect      = Input(Bool())
  val ControlBTBMissBubble = Input(Bool())
  val TAGEMissBubble       = Input(Bool())
  val SCMissBubble         = Input(Bool())
  val ITTAGEMissBubble     = Input(Bool())
  val RASMissBubble        = Input(Bool())
  val MemVioRedirect       = Input(Bool())
  val in                   = Flipped(DecoupledIO(new FetchToIBuffer))
  val out                  = Vec(DecodeWidth, DecoupledIO(new CtrlFlow))
  val full                 = Output(Bool())
  val decodeCanAccept      = Input(Bool())
  val stallReason          = new StallReasonIO(DecodeWidth)
}

class IBufEntry(implicit p: Parameters) extends XSBundle {
  val inst:      UInt          = UInt(32.W)
  val pc:        PrunedAddr    = PrunedAddr(VAddrBits)
  val foldpc:    UInt          = UInt(MemPredPCWidth.W)
  val pd:        PreDecodeInfo = new PreDecodeInfo
  val predTaken: Bool          = Bool()
  val identifiedCfi = Bool()
  val ftqPtr:             FtqPtr        = new FtqPtr
  val instrEndOffset:     UInt          = UInt(log2Ceil(PredictWidth).W)
  val exceptionType:      ExceptionType = new ExceptionType
  val exceptionCrossPage: Bool          = Bool() // whether the exception is cross page
  val isBackendException: Bool          = Bool()
  val triggered:          UInt          = TriggerAction()
  val isLastInFtqEntry:   Bool          = Bool()
  val debug_seqNum:       UInt          = InstSeqNum()

  def fromFetch(fetch: FetchToIBuffer, i: Int): IBufEntry = {
    inst               := fetch.instrs(i)
    pc                 := fetch.pc(i)
    foldpc             := fetch.foldpc(i)
    pd                 := fetch.pd(i)
    predTaken          := fetch.instrEndOffset(i).taken
    identifiedCfi      := fetch.identifiedCfi(i)
    ftqPtr             := fetch.ftqPtr
    instrEndOffset     := fetch.instrEndOffset(i).offset
    exceptionType      := fetch.exceptionType(i)
    exceptionCrossPage := fetch.exceptionCrossPage(i)
    isBackendException := fetch.isBackendException(i)
    triggered          := fetch.triggered(i)
    isLastInFtqEntry   := fetch.isLastInFtqEntry(i)
    debug_seqNum       := fetch.debug_seqNum(i)
    this
  }

  def toIBufOutEntry: IBufOutEntry = {
    val result = Wire(new IBufOutEntry)
    result.inst               := inst
    result.pc                 := pc
    result.foldpc             := foldpc
    result.pd                 := pd
    result.predTaken          := predTaken
    result.ftqPtr             := ftqPtr
    result.exceptionType      := exceptionType
    result.exceptionCrossPage := exceptionCrossPage
    result.isBackendException := isBackendException
    result.triggered          := triggered
    result.isLastInFtqEntry   := isLastInFtqEntry
    result.debug_seqNum       := debug_seqNum
    result.instrEndOffset     := instrEndOffset
    result
  }
}

// The definition of IBufOutEntry is currently retained.
// In the future, the backend will perform certain computations
// in the IBuffer, which will be differentiated from IBufEntry.
class IBufOutEntry(implicit p: Parameters) extends XSBundle {
  val inst               = UInt(32.W)
  val pc                 = PrunedAddr(VAddrBits)
  val foldpc             = UInt(MemPredPCWidth.W)
  val pd                 = new PreDecodeInfo
  val predTaken          = Bool()
  val ftqPtr             = new FtqPtr
  val exceptionType      = new ExceptionType
  val exceptionCrossPage = Bool()
  val isBackendException = Bool()
  val triggered          = TriggerAction()
  val isLastInFtqEntry   = Bool()
  val debug_seqNum       = InstSeqNum()
  val instrEndOffset     = UInt(log2Ceil(PredictWidth).W)

  def toCtrlFlow: CtrlFlow = {
    val cf = Wire(new CtrlFlow)
    cf.instr                                         := inst
    cf.pc                                            := pc.toUInt
    cf.foldpc                                        := foldpc
    cf.exceptionVec                                  := 0.U.asTypeOf(ExceptionVec())
    cf.exceptionVec(ExceptionNO.instrPageFault)      := this.exceptionType.isPf
    cf.exceptionVec(ExceptionNO.instrGuestPageFault) := this.exceptionType.isGpf
    cf.exceptionVec(ExceptionNO.instrAccessFault)    := this.exceptionType.isAf
    cf.exceptionVec(ExceptionNO.illegalInstr)        := this.exceptionType.isIll
    cf.exceptionVec(ExceptionNO.hardwareError)       := this.exceptionType.isHwe
    cf.backendException                              := isBackendException
    cf.trigger                                       := triggered
    cf.pd                                            := pd
    cf.pred_taken                                    := predTaken
    cf.identifiedCfi                                 := instrEndOffset
    cf.crossPageIPFFix                               := this.exceptionCrossPage
    cf.storeSetHit                                   := DontCare
    cf.waitForRobIdx                                 := DontCare
    cf.loadWaitBit                                   := DontCare
    cf.loadWaitStrict                                := DontCare
    cf.ssid                                          := DontCare
    cf.ftqPtr                                        := ftqPtr
    cf.ftqOffset                                     := instrEndOffset
    cf.isLastInFtqEntry                              := isLastInFtqEntry
    cf.debug_seqNum                                  := debug_seqNum
    cf
  }
}

class IBuffer(implicit p: Parameters) extends XSModule with HasCircularQueuePtrHelper with HasPerfEvents {
  val io = IO(new IBufferIO)
  // Max IBuffer bypass = DecodeWidth + IBufWriteBank (valid instructions from new IFU are tightly packed.)
  def NEED_BYPASS_NUM: Int = DecodeWidth + IBufWriteBank
  // io alias
  private val decodeCanAccept = io.decodeCanAccept

  // Parameter Check
  private val readBankSize  = IBufSize / IBufReadBank
  private val writeBankSize = IBufSize / IBufWriteBank
  require(
    IBufSize % IBufReadBank == 0,
    s"IBufReadBank should divide IBufSize, IBufReadBank: $IBufReadBank, IBufSize: $IBufSize"
  )
  require(
    IBufSize % writeBankSize == 0,
    s"IBufWriteBank should divide IBufSize, IBufWriteBank: $IBufWriteBank, IBufSize: $IBufSize"
  )

  require(
    IBufReadBank >= DecodeWidth,
    s"IBufReadBank should be equal or larger than DecodeWidth, IBufReadBank: $IBufReadBank, DecodeWidth: $DecodeWidth"
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
  private val ibuf: Vec[IBufEntry] = RegInit(VecInit.fill(IBufSize)(0.U.asTypeOf(new IBufEntry)))
  private val bankedIBufView: Vec[Vec[IBufEntry]] = VecInit.tabulate(IBufReadBank)(bankID =>
    VecInit.tabulate(readBankSize)(inBankOffset => ibuf(bankID + inBankOffset * IBufReadBank))
  )
  private val bankedIBufWriteWire: Vec[Vec[IBufEntry]] = WireDefault(VecInit.tabulate(IBufWriteBank)(bankID =>
    VecInit.tabulate(writeBankSize)(inBankOffset => 0.U.asTypeOf(new IBufEntry))
  ))

  // Bypass wire
  private val bypassEntries = WireDefault(VecInit.fill(DecodeWidth)(0.U.asTypeOf(Valid(new IBufEntry))))
  // Normal read wire
  private val deqEntries = WireDefault(VecInit.fill(DecodeWidth)(0.U.asTypeOf(Valid(new IBufEntry))))
  // Output register
  private val outputEntries = RegInit(VecInit.fill(DecodeWidth)(0.U.asTypeOf(Valid(new IBufOutEntry))))
  private val outputEntriesValidNum =
    PriorityMuxDefault(outputEntries.map(_.valid).zip(Seq.range(1, DecodeWidth).map(_.U)).reverse.toSeq, 0.U)

  // Between Bank
  private val deqBankPtrVec: Vec[IBufBankPtr] = RegInit(VecInit.tabulate(DecodeWidth)(_.U.asTypeOf(new IBufBankPtr)))
  private val deqBankPtr:    IBufBankPtr      = deqBankPtrVec(0)
  private val deqBankPtrVecNext = Wire(deqBankPtrVec.cloneType)
  // Inside Bank
  private val deqInBankPtr: Vec[IBufInBankPtr] = RegInit(VecInit.fill(IBufReadBank)(0.U.asTypeOf(new IBufInBankPtr)))
  private val deqInBankPtrNext = Wire(deqInBankPtr.cloneType)

  val deqPtr     = RegInit(0.U.asTypeOf(new IBufPtr))
  val deqPtrNext = Wire(deqPtr.cloneType)

  val enqPtrVec = RegInit(VecInit.tabulate(IBufEnqWidth)(_.U.asTypeOf(new IBufPtr)))
  val enqPtr    = enqPtrVec(0)

  XSError(
    io.in.valid && io.in.bits.prevIBufEnqPtr =/= enqPtr,
    "The enqueueing behavior of the IBuffer does not match expectations."
  )

  val numTryEnq = WireDefault(0.U)
  val numEnq    = Mux(io.in.fire, numTryEnq, 0.U)

  // empty and decode can accept insts
  val useBypass = enqPtr === deqPtr && decodeCanAccept

  // The number of decode accepted insts.
  // Since decode promises accepting insts in order, use priority encoder to simplify the accumulation.
  private val numOut = Wire(UInt(DecodeWidth.U.getWidth.W))
  private val numDeq = numOut

  // counter current number of valid
  val numValid = distanceBetween(enqPtr, deqPtr)
  // counter next number of valid
  val numValidNext = numValid + numEnq - numDeq
  val allowEnq     = RegInit(true.B)
  val numFromFetch = Mux(io.in.valid, PopCount(io.in.bits.enqEnable), 0.U)

  allowEnq := (IBufSize - PredictWidth).U >= numValidNext // Disable when almost full

  val enqOffset = VecInit.tabulate(IBufEnqWidth)(i => PopCount(io.in.bits.valid.asBools.take(i)))
  val enqData   = VecInit.tabulate(IBufEnqWidth)(i => Wire(new IBufEntry).fromFetch(io.in.bits, i))
  val enqBankOffset =
    WireDefault(0.U.asTypeOf(Vec(IBufWriteBank, Vec(IBufEnqWidth / IBufWriteBank, UInt(log2Ceil(IBufEnqWidth).W)))))
  val enqBankEntrys =
    WireDefault(0.U.asTypeOf(Vec(IBufWriteBank, Vec(IBufEnqWidth / IBufWriteBank, new IBufEntry))))
  for (i <- 0 until IBufWriteBank) {
    for (j <- 0 until IBufEnqWidth / IBufWriteBank) {
      enqBankOffset(i)(j) := enqOffset(i + IBufWriteBank * j)
      enqBankEntrys(i)(j) := enqData(i + IBufWriteBank * j)
    }
  }

  val outputEntriesIsNotFull = !outputEntries(DecodeWidth - 1).valid
  val numBypass              = Wire(UInt(DecodeWidth.U.getWidth.W))
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
  bypassEntries.zipWithIndex.foreach {
    case (entry, idx) =>
      // Select
      val validOH = Range(0, NEED_BYPASS_NUM).map {
        i =>
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
  io.out zip outputEntries foreach {
    case (io, reg) =>
      io.valid := reg.valid
      io.bits  := reg.bits.toCtrlFlow
  }
  (outputEntries zip bypassEntries).zipWithIndex.foreach {
    case ((out, bypass), i) =>
      when(decodeCanAccept) {
        when(useBypass && io.in.valid) {
          out.valid := bypass.valid
          out.bits  := bypass.bits.toIBufOutEntry
        }.otherwise {
          out.valid := deqEntries(i).valid
          out.bits  := deqEntries(i).bits.toIBufOutEntry
        }
      }.elsewhen(outputEntriesIsNotFull) {
        out.valid := deqEntries(i).valid
        out.bits := Mux(
          i.U < outputEntriesValidNum,
          out.bits,
          VecInit(deqEntries.take(i + 1).map(_.bits))(i.U - outputEntriesValidNum).toIBufOutEntry
        )
      }
  }

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Enqueue
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  io.in.ready := allowEnq
  // Data
  for (bank <- 0 until IBufWriteBank) {
    bankedIBufWriteWire(bank).zipWithIndex.foreach {
      case (entry, idx) => {
        // Select
        val validOH = Range(0, IBufEnqWidth / IBufWriteBank).map { j =>
          val normalMatch = enqPtrVec(enqBankOffset(bank)(j)).value === (bank + idx * IBufWriteBank).asUInt
          io.in.bits.valid(bank + IBufWriteBank * j) && io.in.bits.enqEnable(bank + IBufWriteBank * j) && normalMatch
        } // Should be OneHot
        val useBypassMatch = Range(0, DecodeWidth).map(k => enqPtrVec(k).value === (bank + idx * IBufWriteBank).U)
        val wen = validOH.reduce(_ || _) && io.in.fire && !io.flush && !(useBypassMatch.reduce(_ || _) && useBypass)

        // Write port
        // Each IBuffer entry has a PredictWidth -> 1 Mux
        val writeEntry = Mux1H(validOH, enqBankEntrys(bank))
        ibuf(bank + idx * IBufWriteBank) := Mux(wen, writeEntry, ibuf(bank + idx * IBufWriteBank))
        // Debug Assertion
        XSError(io.in.valid && PopCount(validOH) > 1.asUInt, "validOH is not OneHot")
      }
    }
  }

  // Pointer maintenance
  when(io.in.fire && !io.flush) {
    enqPtrVec := VecInit(enqPtrVec.map(_ + numTryEnq))
  }

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Dequeue
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  val outputEntriesValidNumNext = Wire(UInt(DecodeWidth.U.getWidth.W))
  XSError(outputEntriesValidNumNext > DecodeWidth.U, "Ibuffer: outputEntriesValidNumNext > DecodeWidth.U")
  val validVec = UIntToMask(outputEntriesValidNumNext(DecodeWidth.U.getWidth - 1, 0), DecodeWidth)
  when(decodeCanAccept) {
    outputEntriesValidNumNext := Mux(useBypass, numBypass, numDeq)
  }.elsewhen(outputEntriesIsNotFull) {
    outputEntriesValidNumNext := outputEntriesValidNum + numDeq
  }.otherwise {
    outputEntriesValidNumNext := outputEntriesValidNum
  }
  // Data
  // Read port
  // 2-stage, IBufReadBank * (bankSize -> 1) + IBufReadBank -> 1
  // Should be better than IBufSize -> 1 in area, with no significant latency increase
  private val readStage1: Vec[IBufEntry] =
    VecInit.tabulate(IBufReadBank)(bankID => Mux1H(UIntToOH(deqInBankPtr(bankID).value), bankedIBufView(bankID)))
  for (i <- 0 until DecodeWidth) {
    deqEntries(i).valid := validVec(i)
    deqEntries(i).bits  := Mux1H(UIntToOH(deqBankPtrVec(i).value), readStage1)
  }
  // Pointer maintenance
  deqBankPtrVecNext := VecInit(deqBankPtrVec.map(_ + numDeq))
  deqPtrNext        := deqPtr + numDeq
  deqInBankPtrNext.zip(deqInBankPtr).zipWithIndex.foreach {
    case ((ptrNext, ptr), idx) => {
      // validVec[k] == bankValid[deqBankPtr + k]
      // So bankValid[n] == validVec[n - deqBankPtr]
      val validIdx = Mux(
        idx.asUInt >= deqBankPtr.value,
        idx.asUInt - deqBankPtr.value,
        ((idx + IBufReadBank).asUInt - deqBankPtr.value)(DecodeWidth.U.getWidth - 1, 0)
      )(DecodeWidth.U.getWidth - 1, 0)
      val bankAdvance = numOut > validIdx
      ptrNext := Mux(bankAdvance, ptr + 1.U, ptr)
    }
  }

  // Flush
  when(io.flush) {
    allowEnq      := true.B
    enqPtrVec     := enqPtrVec.indices.map(_.U.asTypeOf(new IBufPtr))
    deqBankPtrVec := deqBankPtrVec.indices.map(_.U.asTypeOf(new IBufBankPtr))
    deqInBankPtr  := VecInit.fill(IBufReadBank)(0.U.asTypeOf(new IBufInBankPtr))
    deqPtr        := 0.U.asTypeOf(new IBufPtr())
    outputEntries.foreach(_.valid := false.B)
  }.otherwise {
    deqPtr        := deqPtrNext
    deqInBankPtr  := deqInBankPtrNext
    deqBankPtrVec := deqBankPtrVecNext
  }
  io.full := !allowEnq

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // TopDown
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  val topdown_stage = RegInit(0.U.asTypeOf(new FrontendTopDownBundle))
  topdown_stage := io.in.bits.topdown_info
  when(io.flush) {
    when(io.ControlRedirect) {
      when(io.ControlBTBMissBubble) {
        topdown_stage.reasons(TopDownCounters.BTBMissBubble.id) := true.B
      }.elsewhen(io.TAGEMissBubble) {
        topdown_stage.reasons(TopDownCounters.TAGEMissBubble.id) := true.B
      }.elsewhen(io.SCMissBubble) {
        topdown_stage.reasons(TopDownCounters.SCMissBubble.id) := true.B
      }.elsewhen(io.ITTAGEMissBubble) {
        topdown_stage.reasons(TopDownCounters.ITTAGEMissBubble.id) := true.B
      }.elsewhen(io.RASMissBubble) {
        topdown_stage.reasons(TopDownCounters.RASMissBubble.id) := true.B
      }
    }.elsewhen(io.MemVioRedirect) {
      topdown_stage.reasons(TopDownCounters.MemVioRedirectBubble.id) := true.B
    }.otherwise {
      topdown_stage.reasons(TopDownCounters.OtherRedirectBubble.id) := true.B
    }
  }

  val matchBubble   = Wire(UInt(log2Up(TopDownCounters.NumStallReasons.id).W))
  val deqValidCount = PopCount(validVec.asBools)
  val deqWasteCount = DecodeWidth.U - deqValidCount
  matchBubble := (TopDownCounters.NumStallReasons.id - 1).U - PriorityEncoder(topdown_stage.reasons.reverse)

  io.stallReason.reason.map(_ := 0.U)
  for (i <- 0 until DecodeWidth) {
    when(i.U < deqWasteCount) {
      io.stallReason.reason(DecodeWidth - i - 1) := matchBubble
    }
  }

  when(!(deqWasteCount === DecodeWidth.U || topdown_stage.reasons.asUInt.orR)) {
    // should set reason for FetchFragmentationStall
    // topdown_stage.reasons(TopDownCounters.FetchFragmentationStall.id) := true.B
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
    deqPtr.value =/= deqBankPtr.value + deqInBankPtr(deqBankPtr.value).value * IBufReadBank.asUInt,
    "Dequeue PTR mismatch"
  )
  XSError(isBefore(enqPtr, deqPtr) && !isFull(enqPtr, deqPtr), "\ndeqPtr is older than enqPtr!\n")

  XSDebug(io.flush, "IBuffer Flushed\n")

  XSDebug(io.in.fire, "Enque:\n")
  XSDebug(io.in.fire, p"MASK=${Binary(io.in.bits.valid)}\n")
  for (i <- 0 until IBufEnqWidth) {
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

  val afterInit  = RegInit(false.B)
  val headBubble = RegInit(false.B)
  when(io.in.fire)(afterInit := true.B)
  when(io.flush) {
    headBubble := true.B
  }.elsewhen(numValid =/= 0.U) {
    headBubble := false.B
  }
  val instrHungry = afterInit && (numValid === 0.U) && !headBubble

  QueuePerf(IBufSize, numValid, !allowEnq)
  XSPerfAccumulate("flush", io.flush)
  XSPerfAccumulate("hungry", instrHungry)

  val ibuffer_IDWidth_hvButNotFull = afterInit && (numValid =/= 0.U) && (numValid < DecodeWidth.U) && !headBubble
  XSPerfAccumulate("ibuffer_IDWidth_hvButNotFull", ibuffer_IDWidth_hvButNotFull)

  val FrontBubble = Mux(decodeCanAccept && !headBubble, DecodeWidth.U - numOut, 0.U)

  val fetchLatency = decodeCanAccept && !headBubble && numOut === 0.U

  XSPerfAccumulate("if_fetch_bubble", FrontBubble)
  XSPerfAccumulate("if_fetch_bubble_eq_max", fetchLatency)

  val perfEvents = Seq(
    ("IBuffer_Flushed  ", io.flush),
    ("IBuffer_hungry   ", instrHungry),
    ("IBuffer_1_4_valid", (numValid > (0 * (IBufSize / 4)).U) & (numValid < (1 * (IBufSize / 4)).U)),
    ("IBuffer_2_4_valid", (numValid >= (1 * (IBufSize / 4)).U) & (numValid < (2 * (IBufSize / 4)).U)),
    ("IBuffer_3_4_valid", (numValid >= (2 * (IBufSize / 4)).U) & (numValid < (3 * (IBufSize / 4)).U)),
    ("IBuffer_4_4_valid", (numValid >= (3 * (IBufSize / 4)).U) & (numValid < (4 * (IBufSize / 4)).U)),
    ("IBuffer_full     ", numValid.andR),
    ("Front_Bubble     ", FrontBubble),
    ("Fetch_Latency_Bound", fetchLatency)
  )
  generatePerfEvent()
}
