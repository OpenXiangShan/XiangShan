package xiangshan.backend.dispatch

import chisel3._
import chisel3.util._
import utils._
import xiangshan.backend.decode.SrcType
import xiangshan._
import xiangshan.backend.roq.RoqPtr

class DispatchQueueIO(enqnum: Int, deqnum: Int) extends XSBundle {
  val enq = Vec(enqnum, Flipped(ValidIO(new MicroOp)))
  val enqReady = Output(Bool())
  val deq = Vec(deqnum, DecoupledIO(new MicroOp))
  val redirect = Flipped(ValidIO(new Redirect))
  override def cloneType: DispatchQueueIO.this.type =
    new DispatchQueueIO(enqnum, deqnum).asInstanceOf[this.type]
}

// dispatch queue: accepts at most enqnum uops from dispatch1 and dispatches deqnum uops at every clock cycle
class DispatchQueue(size: Int, enqnum: Int, deqnum: Int) extends XSModule with HasCircularQueuePtrHelper {
  val io = IO(new DispatchQueueIO(enqnum, deqnum))
  val indexWidth = log2Ceil(size)

  val s_invalid :: s_valid:: Nil = Enum(2)

  // queue data array
  val uopEntries = Mem(size, new MicroOp)
  val stateEntries = RegInit(VecInit(Seq.fill(size)(s_invalid)))

  // head: first valid entry (dispatched entry)
  val headPtr = RegInit(0.U.asTypeOf(new CircularQueuePtr(size)))
  val headPtrMask = UIntToMask(headPtr.value, size)
  // tail: first invalid entry (free entry)
  val tailPtr = RegInit(0.U.asTypeOf(new CircularQueuePtr(size)))
  val tailPtrMask = UIntToMask(tailPtr.value, size)

  // TODO: make ptr a vector to reduce latency?
  // deq: starting from head ptr
  val deqIndex = (0 until deqnum).map(i => headPtr + i.U).map(_.value)
  // enq: starting from tail ptr
  val enqIndex = (0 until enqnum).map(i => tailPtr + i.U).map(_.value)

  val validEntries = distanceBetween(tailPtr, headPtr)
  val isTrueEmpty = ~Cat((0 until size).map(i => stateEntries(i) === s_valid)).orR
  val canEnqueue = validEntries <= (size - enqnum).U
  val canActualEnqueue = canEnqueue && !(io.redirect.valid /*&& !io.redirect.bits.isReplay*/)

  /**
    * Part 1: update states and uops when enqueue, dequeue, commit, redirect/replay
    *
    * uop only changes when a new instruction enqueues.
    *
    * state changes when
    * (1) enqueue: from s_invalid to s_valid
    * (2) dequeue: from s_valid to s_dispatched
    * (3) commit: from s_dispatched to s_invalid
    * (4) redirect (branch misprediction or exception): from any state to s_invalid (flushed)
    * (5) redirect (replay): from s_dispatched to s_valid (re-dispatch)
    */
  // enqueue: from s_invalid to s_valid
  io.enqReady := canEnqueue
  for (i <- 0 until enqnum) {
    when (io.enq(i).valid && canActualEnqueue) {
      uopEntries(enqIndex(i)) := io.enq(i).bits
      stateEntries(enqIndex(i)) := s_valid
    }
  }

  // dequeue: from s_valid to s_dispatched
  for (i <- 0 until deqnum) {
    when (io.deq(i).fire() && !io.redirect.valid) {
      stateEntries(deqIndex(i)) := s_invalid

      XSError(stateEntries(deqIndex(i)) =/= s_valid, "state of the dispatch entry is not s_valid\n")
    }
  }

  // redirect: cancel uops currently in the queue
  val mispredictionValid = io.redirect.valid //&& io.redirect.bits.isMisPred
  val exceptionValid = io.redirect.valid && io.redirect.bits.isException
  val flushPipeValid = io.redirect.valid && io.redirect.bits.isFlushPipe
  val roqNeedFlush = Wire(Vec(size, Bool()))
  val needCancel = Wire(Vec(size, Bool()))
  for (i <- 0 until size) {
    roqNeedFlush(i) := uopEntries(i.U).roqIdx.needFlush(io.redirect)
    needCancel(i) := stateEntries(i) =/= s_invalid && ((roqNeedFlush(i) && mispredictionValid) || exceptionValid || flushPipeValid)

    when (needCancel(i)) {
      stateEntries(i) := s_invalid
    }

    XSInfo(needCancel(i), p"valid entry($i)(pc = ${Hexadecimal(uopEntries(i).cf.pc)}) " +
      p"roqIndex 0x${Hexadecimal(uopEntries(i).roqIdx.asUInt)} " +
      p"cancelled with redirect roqIndex 0x${Hexadecimal(io.redirect.bits.roqIdx.asUInt)}\n")
  }

  /**
    * Part 2: update indices
    *
    * tail: (1) enqueue; (2) redirect
    * head: dequeue
    */

  // dequeue
  val numDeqTry = Mux(validEntries > deqnum.U, deqnum.U, validEntries)
  val numDeqFire = PriorityEncoder(io.deq.zipWithIndex.map{case (deq, i) =>
    // For dequeue, the first entry should never be s_invalid
    // Otherwise, there should be a redirect and tail walks back
    // in this case, we set numDeq to 0
    !deq.fire() && (if (i == 0) true.B else stateEntries(deqIndex(i)) =/= s_invalid)
  } :+ true.B)
  val numDeq = Mux(numDeqTry > numDeqFire, numDeqFire, numDeqTry)
  // agreement with reservation station: don't dequeue when redirect.valid
  val headPtrNext = Mux(mispredictionValid, headPtr, headPtr + numDeq)
  headPtr := Mux(exceptionValid, 0.U.asTypeOf(new CircularQueuePtr(size)), headPtrNext)

  // For branch mis-prediction or memory violation replay,
  // we delay updating the indices for one clock cycle.
  // For now, we simply use PopCount to count #instr cancelled.
  val lastCycleMisprediction = RegNext(io.redirect.valid && !(io.redirect.bits.isException || io.redirect.bits.isFlushPipe))
  // find the last one's position, starting from headPtr and searching backwards
  val validBitVec = VecInit((0 until size).map(i => stateEntries(i) === s_valid))
  val loValidBitVec = Cat((0 until size).map(i => validBitVec(i) && headPtrMask(i)))
  val hiValidBitVec = Cat((0 until size).map(i => validBitVec(i) && ~headPtrMask(i)))
  val flippedFlag = loValidBitVec.orR
  val lastOneIndex = size.U - PriorityEncoder(Mux(loValidBitVec.orR, loValidBitVec, hiValidBitVec))
  val walkedTailPtr = Wire(new CircularQueuePtr(size))
  walkedTailPtr.flag := flippedFlag ^ headPtr.flag
  walkedTailPtr.value := lastOneIndex

  // enqueue
  val numEnq = Mux(canActualEnqueue, PriorityEncoder(io.enq.map(!_.valid) :+ true.B), 0.U)
  XSError(numEnq =/= 0.U && (mispredictionValid || exceptionValid), "should not enqueue when redirect\n")
  tailPtr := Mux(exceptionValid,
    0.U.asTypeOf(new CircularQueuePtr(size)),
    Mux(lastCycleMisprediction,
      Mux(isTrueEmpty, headPtr, walkedTailPtr),
      tailPtr + numEnq)
  )


  /**
    * Part 3: set output and input
    */
  // TODO: remove this when replay moves to roq
  for (i <- 0 until deqnum) {
    io.deq(i).bits := uopEntries(deqIndex(i))
    // do not dequeue when io.redirect valid because it may cause dispatchPtr work improperly
    io.deq(i).valid := stateEntries(deqIndex(i)) === s_valid && !lastCycleMisprediction// && !io.redirect.valid
  }

  // debug: dump dispatch queue states
  XSDebug(p"head: $headPtr, tail: $tailPtr\n")
  XSDebug(p"state: ")
  stateEntries.reverse.foreach { s =>
    XSDebug(false, s === s_invalid, "-")
    XSDebug(false, s === s_valid, "v")
  }
  XSDebug(false, true.B, "\n")
  XSDebug(p"ptr:   ")
  (0 until size).reverse.foreach { i =>
    val isPtr = i.U === headPtr.value || i.U === tailPtr.value
    XSDebug(false, isPtr, "^")
    XSDebug(false, !isPtr, " ")
  }
  XSDebug(false, true.B, "\n")

  XSError(isAfter(headPtr, tailPtr), p"assert greaterOrEqualThan(tailPtr: $tailPtr, headPtr: $headPtr) failed\n")
}
