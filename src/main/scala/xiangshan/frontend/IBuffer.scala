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

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import utility._
import xiangshan.ExceptionNO._

class IBufPtr(implicit p: Parameters) extends CircularQueuePtr[IBufPtr](
  p => p(XSCoreParamsKey).IBufSize
) {
}

class IBufInBankPtr(implicit p: Parameters) extends CircularQueuePtr[IBufInBankPtr](
  p => p(XSCoreParamsKey).IBufSize / p(XSCoreParamsKey).IBufNBank
) {
}

class IBufBankPtr(implicit p: Parameters) extends CircularQueuePtr[IBufBankPtr](
  p => p(XSCoreParamsKey).IBufNBank
) {
}

class IBufferIO(implicit p: Parameters) extends XSBundle {
  val flush = Input(Bool())
  val ControlRedirect = Input(Bool())
  val ControlBTBMissBubble = Input(Bool())
  val TAGEMissBubble = Input(Bool())
  val SCMissBubble = Input(Bool())
  val ITTAGEMissBubble = Input(Bool())
  val RASMissBubble = Input(Bool())
  val MemVioRedirect = Input(Bool())
  val in = Flipped(DecoupledIO(new FetchToIBuffer))
  val out = Vec(DecodeWidth, DecoupledIO(new CtrlFlow))
  val full = Output(Bool())
  val stallReason = new StallReasonIO(DecodeWidth)
}

class IBufEntry(implicit p: Parameters) extends XSBundle {
  val inst = UInt(32.W)
  val pc = UInt(VAddrBits.W)
  val foldpc = UInt(MemPredPCWidth.W)
  val pd = new PreDecodeInfo
  val pred_taken = Bool()
  val ftqPtr = new FtqPtr
  val ftqOffset = UInt(log2Ceil(PredictWidth).W)
  val ipf = Bool()
  val acf = Bool()
  val crossPageIPFFix = Bool()
  val triggered = new TriggerCf

  def fromFetch(fetch: FetchToIBuffer, i: Int): IBufEntry = {
    inst   := fetch.instrs(i)
    pc     := fetch.pc(i)
    foldpc := fetch.foldpc(i)
    pd     := fetch.pd(i)
    pred_taken := fetch.ftqOffset(i).valid
    ftqPtr := fetch.ftqPtr
    ftqOffset := fetch.ftqOffset(i).bits
    ipf := fetch.ipf(i)
    acf := fetch.acf(i)
    crossPageIPFFix := fetch.crossPageIPFFix(i)
    triggered := fetch.triggered(i)
    this
  }

  def toCtrlFlow: CtrlFlow = {
    val cf = Wire(new CtrlFlow)
    cf.instr := inst
    cf.pc := pc
    cf.foldpc := foldpc
    cf.exceptionVec := 0.U.asTypeOf(ExceptionVec())
    cf.exceptionVec(instrPageFault) := ipf
    cf.exceptionVec(instrAccessFault) := acf
    cf.trigger := triggered
    cf.pd := pd
    cf.pred_taken := pred_taken
    cf.crossPageIPFFix := crossPageIPFFix
    cf.storeSetHit := DontCare
    cf.waitForRobIdx := DontCare
    cf.loadWaitBit := DontCare
    cf.loadWaitStrict := DontCare
    cf.ssid := DontCare
    cf.ftqPtr := ftqPtr
    cf.ftqOffset := ftqOffset
    cf
  }
}

class IBuffer(implicit p: Parameters) extends XSModule with HasCircularQueuePtrHelper with HasPerfEvents {
  val io = IO(new IBufferIO)

  // Parameter Check
  private val bankSize = IBufSize / IBufNBank
  require(IBufSize % IBufNBank == 0, s"IBufNBank should divide IBufSize, IBufNBank: $IBufNBank, IBufSize: $IBufSize")
  require(IBufNBank >= DecodeWidth,
    s"IBufNBank should be equal or larger than DecodeWidth, IBufNBank: $IBufNBank, DecodeWidth: $DecodeWidth")

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
  private val bankedIBufView: Vec[Vec[IBufEntry]] = VecInit.tabulate(IBufNBank)(
    bankID => VecInit.tabulate(bankSize)(
      inBankOffset => ibuf(bankID + inBankOffset * IBufNBank)
    )
  )

  // Between Bank
  private val deqBankPtrVec: Vec[IBufBankPtr] = RegInit(VecInit.tabulate(DecodeWidth)(_.U.asTypeOf(new IBufBankPtr)))
  private val deqBankPtr: IBufBankPtr = deqBankPtrVec(0)
  // Inside Bank
  private val deqInBankPtr: Vec[IBufInBankPtr] = RegInit(VecInit.fill(IBufNBank)(0.U.asTypeOf(new IBufInBankPtr)))

  val deqPtr = RegInit(0.U.asTypeOf(new IBufPtr))

  val enqPtrVec = RegInit(VecInit.tabulate(PredictWidth)(_.U.asTypeOf(new IBufPtr)))
  val enqPtr = enqPtrVec(0)

  val validEntries = distanceBetween(enqPtr, deqPtr)
  val allowEnq = RegInit(true.B)

  val numEnq = Mux(io.in.fire, PopCount(io.in.bits.valid), 0.U)
  val numTryDeq = Mux(validEntries >= DecodeWidth.U, DecodeWidth.U, validEntries)
  val numDeq = Mux(io.out.head.ready, numTryDeq, 0.U)

  val numAfterEnq = validEntries +& numEnq
  val nextValidEntries = Mux(io.out(0).ready, numAfterEnq - numTryDeq, numAfterEnq)
  allowEnq := (IBufSize - PredictWidth).U >= nextValidEntries // Disable when almost full

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Enqueue
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  io.in.ready := allowEnq
  // Data
  val enqOffset = VecInit.tabulate(PredictWidth)(i => PopCount(io.in.bits.valid.asBools.take(i)))
  val enqData = VecInit.tabulate(PredictWidth)(i => Wire(new IBufEntry).fromFetch(io.in.bits, i))
  ibuf.zipWithIndex.foreach {
    case (entry, idx) => {
      // Select
      val validOH = Range(0, PredictWidth).map {
        i => io.in.bits.valid(i) &&
          io.in.bits.enqEnable(i) &&
          enqPtrVec(enqOffset(i)).value === idx.asUInt
      } // Should be OneHot
      val wen = validOH.reduce(_ || _) && io.in.fire && !io.flush

      // Write port
      // Each IBuffer entry has a PredictWidth -> 1 Mux
      val writeEntry = Mux1H(validOH, enqData)
      entry := Mux(wen, writeEntry, entry)

      // Debug Assertion
      XSError(PopCount(validOH) > 1.asUInt, "validOH is not OneHot")
    }
  }
  // Pointer maintenance
  when (io.in.fire && !io.flush) {
    enqPtrVec := VecInit(enqPtrVec.map(_ + PopCount(io.in.bits.enqEnable)))
  }

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Dequeue
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  val validVec = Mux(validEntries >= DecodeWidth.U,
    ((1 << DecodeWidth) - 1).U,
    UIntToMask(validEntries(log2Ceil(DecodeWidth) - 1, 0), DecodeWidth)
  )
  // Data
  for (i <- 0 until DecodeWidth) {
    io.out(i).valid := validVec(i)

    // Read port
    // 2-stage, IBufNBank * (bankSize -> 1) + IBufNBank -> 1
    // Should be better than IBufSize -> 1 in area, with no significant latency increase
    io.out(i).bits := bankedIBufView(deqBankPtrVec(i).value)(deqInBankPtr(deqBankPtrVec(i).value).value).toCtrlFlow
  }
  // Pointer maintenance
  deqBankPtrVec := Mux(io.out.head.ready, VecInit(deqBankPtrVec.map(_ + numTryDeq)), deqBankPtrVec)
  deqPtr := Mux(io.out.head.ready, deqPtr + numTryDeq, deqPtr)
  deqInBankPtr.zipWithIndex.foreach {
    case (ptr, idx) => {
      // validVec[k] == bankValid[deqBankPtr + k]
      // So bankValid[n] == validVec[n - deqBankPtr]
      val validIdx = Mux(idx.asUInt >= deqBankPtr.value,
        idx.asUInt - deqBankPtr.value,
        ((idx + IBufNBank).asUInt - deqBankPtr.value)(log2Ceil(IBufNBank) - 1, 0)
      )
      val bankAdvance = Mux(validIdx >= DecodeWidth.U,
        false.B,
        validVec(validIdx(log2Ceil(DecodeWidth) - 1, 0))
      ) && io.out.head.ready
      ptr := Mux(bankAdvance , ptr + 1.U, ptr)
    }
  }

  // Flush
  when (io.flush) {
    allowEnq := true.B
    enqPtrVec := enqPtrVec.indices.map(_.U.asTypeOf(new IBufPtr))
    deqBankPtrVec := deqBankPtrVec.indices.map(_.U.asTypeOf(new IBufBankPtr))
    deqInBankPtr := VecInit.fill(IBufNBank)(0.U.asTypeOf(new IBufInBankPtr))
    deqPtr := 0.U.asTypeOf(new IBufPtr())
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


  val dequeueInsufficient = Wire(Bool())
  val matchBubble = Wire(UInt(log2Up(TopDownCounters.NumStallReasons.id).W))
  val deqValidCount = PopCount(validVec.asBools)
  val deqWasteCount = DecodeWidth.U - deqValidCount
  dequeueInsufficient := deqValidCount < DecodeWidth.U
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
    deqPtr.value =/= deqBankPtr.value + deqInBankPtr(deqBankPtr.value).value * IBufNBank.asUInt,
    "Dequeue PTR mismatch"
  )
  XSDebug(io.flush, "IBuffer Flushed\n")

  when(io.in.fire) {
    XSDebug("Enque:\n")
    XSDebug(p"MASK=${Binary(io.in.bits.valid)}\n")
    for(i <- 0 until PredictWidth){
      XSDebug(p"PC=${Hexadecimal(io.in.bits.pc(i))} ${Hexadecimal(io.in.bits.instrs(i))}\n")
    }
  }

  for (i <- 0 until DecodeWidth) {
    XSDebug(io.out(i).fire,
      p"deq: ${Hexadecimal(io.out(i).bits.instr)} PC=${Hexadecimal(io.out(i).bits.pc)}" +
      p"v=${io.out(i).valid} r=${io.out(i).ready} " +
      p"excpVec=${Binary(io.out(i).bits.exceptionVec.asUInt)} crossPageIPF=${io.out(i).bits.crossPageIPFFix}\n")
  }

  XSDebug(p"ValidEntries: ${validEntries}\n")
  XSDebug(p"EnqNum: ${numEnq}\n")
  XSDebug(p"DeqNum: ${numDeq}\n")

  val afterInit = RegInit(false.B)
  val headBubble = RegInit(false.B)
  when (io.in.fire) { afterInit := true.B }
  when (io.flush) {
    headBubble := true.B
  } .elsewhen(validEntries =/= 0.U) {
    headBubble := false.B
  }
  val instrHungry = afterInit && (validEntries === 0.U) && !headBubble

  QueuePerf(IBufSize, validEntries, !allowEnq)
  XSPerfAccumulate("flush", io.flush)
  XSPerfAccumulate("hungry", instrHungry)

  val ibuffer_IDWidth_hvButNotFull = afterInit && (validEntries =/= 0.U) && (validEntries < DecodeWidth.U) && !headBubble
  XSPerfAccumulate("ibuffer_IDWidth_hvButNotFull", ibuffer_IDWidth_hvButNotFull)
  /*
  XSPerfAccumulate("ICacheMissBubble", Mux(matchBubbleVec(TopDownCounters.ICacheMissBubble.id), deqWasteCount, 0.U))
  XSPerfAccumulate("ITLBMissBubble", Mux(matchBubbleVec(TopDownCounters.ITLBMissBubble.id), deqWasteCount, 0.U))
  XSPerfAccumulate("ControlRedirectBubble", Mux(matchBubbleVec(TopDownCounters.ControlRedirectBubble.id), deqWasteCount, 0.U))
  XSPerfAccumulate("MemVioRedirectBubble", Mux(matchBubbleVec(TopDownCounters.MemVioRedirectBubble.id), deqWasteCount, 0.U))
  XSPerfAccumulate("OtherRedirectBubble", Mux(matchBubbleVec(TopDownCounters.OtherRedirectBubble.id), deqWasteCount, 0.U))
  XSPerfAccumulate("BTBMissBubble", Mux(matchBubbleVec(TopDownCounters.BTBMissBubble.id), deqWasteCount, 0.U))
  XSPerfAccumulate("OverrideBubble", Mux(matchBubbleVec(TopDownCounters.OverrideBubble.id), deqWasteCount, 0.U))
  XSPerfAccumulate("FtqUpdateBubble", Mux(matchBubbleVec(TopDownCounters.FtqUpdateBubble.id), deqWasteCount, 0.U))
  XSPerfAccumulate("FtqFullStall", Mux(matchBubbleVec(TopDownCounters.FtqFullStall.id), deqWasteCount, 0.U))
  XSPerfAccumulate("FetchFragmentBubble",
  Mux(deqWasteCount === DecodeWidth.U || topdown_stage.reasons.asUInt.orR, 0.U, deqWasteCount))
  XSPerfAccumulate("TAGEMissBubble", Mux(matchBubbleVec(TopDownCounters.TAGEMissBubble.id), deqWasteCount, 0.U))
  XSPerfAccumulate("SCMissBubble", Mux(matchBubbleVec(TopDownCounters.SCMissBubble.id), deqWasteCount, 0.U))
  XSPerfAccumulate("ITTAGEMissBubble", Mux(matchBubbleVec(TopDownCounters.ITTAGEMissBubble.id), deqWasteCount, 0.U))
  XSPerfAccumulate("RASMissBubble", Mux(matchBubbleVec(TopDownCounters.RASMissBubble.id), deqWasteCount, 0.U))
  */

  val perfEvents = Seq(
    ("IBuffer_Flushed  ", io.flush                                                                     ),
    ("IBuffer_hungry   ", instrHungry                                                                  ),
    ("IBuffer_1_4_valid", (validEntries >  (0*(IBufSize/4)).U) & (validEntries < (1*(IBufSize/4)).U)   ),
    ("IBuffer_2_4_valid", (validEntries >= (1*(IBufSize/4)).U) & (validEntries < (2*(IBufSize/4)).U)   ),
    ("IBuffer_3_4_valid", (validEntries >= (2*(IBufSize/4)).U) & (validEntries < (3*(IBufSize/4)).U)   ),
    ("IBuffer_4_4_valid", (validEntries >= (3*(IBufSize/4)).U) & (validEntries < (4*(IBufSize/4)).U)   ),
    ("IBuffer_full     ",  validEntries.andR                                                           ),
    ("Front_Bubble     ", PopCount((0 until DecodeWidth).map(i => io.out(i).ready && !io.out(i).valid)))
  )
  generatePerfEvent()
}
