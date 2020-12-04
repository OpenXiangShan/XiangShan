package xiangshan.backend.dispatch

import chisel3._
import chisel3.util._
import utils._
import xiangshan.backend.decode.SrcType
import xiangshan._
import xiangshan.backend.roq.RoqPtr

class DispatchQueueIO(enqnum: Int, deqnum: Int, replayWidth: Int) extends XSBundle {
  val enq = Vec(enqnum, Flipped(ValidIO(new MicroOp)))
  val enqReady = Output(Bool())
  val deq = Vec(deqnum, DecoupledIO(new MicroOp))
  val dequeueRoqIndex = Input(Valid(new RoqPtr))
  val redirect = Flipped(ValidIO(new Redirect))
  val replayPregReq = Output(Vec(replayWidth, new ReplayPregReq))
  val inReplayWalk = Output(Bool())
  val otherWalkDone = Input(Bool())

  override def cloneType: DispatchQueueIO.this.type =
    new DispatchQueueIO(enqnum, deqnum, replayWidth).asInstanceOf[this.type]
}

// dispatch queue: accepts at most enqnum uops from dispatch1 and dispatches deqnum uops at every clock cycle
class DispatchQueue(size: Int, enqnum: Int, deqnum: Int, replayWidth: Int) extends XSModule with HasCircularQueuePtrHelper {
  val io = IO(new DispatchQueueIO(enqnum, deqnum, replayWidth))
  val indexWidth = log2Ceil(size)

  val s_invalid :: s_valid :: s_dispatched :: Nil = Enum(3)

  // queue data array
  val uopEntries = Mem(size, new MicroOp)
  val stateEntries = RegInit(VecInit(Seq.fill(size)(s_invalid)))
  // head: first valid entry (dispatched entry)
  val headPtr = RegInit(0.U.asTypeOf(new CircularQueuePtr(size)))
  // dispatch: first entry that has not been dispatched
  val dispatchPtr = RegInit(0.U.asTypeOf(new CircularQueuePtr(size)))
  // tail: first invalid entry (free entry)
  val tailPtr = RegInit(0.U.asTypeOf(new CircularQueuePtr(size)))

  // TODO: make ptr a vector to reduce latency?
  // commit: starting from head ptr
  val commitIndex = (0 until CommitWidth).map(i => headPtr + i.U).map(_.value)
  // deq: starting from dispatch ptr
  val deqIndex = (0 until deqnum).map(i => dispatchPtr + i.U).map(_.value)
  // enq: starting from tail ptr
  val enqIndex = (0 until enqnum).map(i => tailPtr + i.U).map(_.value)


  val validEntries = distanceBetween(tailPtr, headPtr)
  val dispatchEntries = distanceBetween(tailPtr, dispatchPtr)
  val commitEntries = validEntries - dispatchEntries
  val emptyEntries = size.U - validEntries

  def rangeMask(start: CircularQueuePtr, end: CircularQueuePtr): UInt = {
    val startMask = (1.U((size + 1).W) << start.value).asUInt - 1.U
    val endMask = (1.U((size + 1).W) << end.value).asUInt - 1.U
    val xorMask = startMask(size - 1, 0) ^ endMask(size - 1, 0)
    Mux(start.flag === end.flag, xorMask, ~xorMask)
  }
  val dispatchedMask = rangeMask(headPtr, dispatchPtr)

  val allWalkDone = !io.inReplayWalk && io.otherWalkDone
  val canEnqueue = validEntries <= (size - enqnum).U && allWalkDone
  val canActualEnqueue = canEnqueue && !(io.redirect.valid && !io.redirect.bits.isReplay)

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
    when (io.deq(i).fire()) {
      stateEntries(deqIndex(i)) := s_dispatched

      XSError(stateEntries(deqIndex(i)) =/= s_valid, "state of the dispatch entry is not s_valid\n")
    }
  }

  // commit: from s_dispatched to s_invalid
  val needDequeue = Wire(Vec(size, Bool()))
  val deqRoqIdx = io.dequeueRoqIndex.bits
  for (i <- 0 until size) {
    needDequeue(i) := stateEntries(i)  === s_dispatched && io.dequeueRoqIndex.valid && !isAfter(uopEntries(i).roqIdx, deqRoqIdx) && dispatchedMask(i)
    when (needDequeue(i)) {
      stateEntries(i) := s_invalid
    }

    XSInfo(needDequeue(i), p"dispatched entry($i)(pc = ${Hexadecimal(uopEntries(i).cf.pc)}) " +
      p"roqIndex 0x${Hexadecimal(uopEntries(i).roqIdx.asUInt)} " +
      p"left dispatch queue with deqRoqIndex 0x${Hexadecimal(io.dequeueRoqIndex.bits.asUInt)}\n")
  }

  // redirect: cancel uops currently in the queue
  val mispredictionValid = io.redirect.valid && io.redirect.bits.isMisPred
  val exceptionValid = io.redirect.valid && io.redirect.bits.isException
  val flushPipeValid = io.redirect.valid && io.redirect.bits.isFlushPipe
  val roqNeedFlush = Wire(Vec(size, Bool()))
  val needCancel = Wire(Vec(size, Bool()))
  for (i <- 0 until size) {
    roqNeedFlush(i) := uopEntries(i.U).roqIdx.needFlush(io.redirect)
    needCancel(i) := stateEntries(i) =/= s_invalid && ((roqNeedFlush(i) && mispredictionValid) || exceptionValid || flushPipeValid) && !needDequeue(i)

    when (needCancel(i)) {
      stateEntries(i) := s_invalid
    }

    XSInfo(needCancel(i), p"valid entry($i)(pc = ${Hexadecimal(uopEntries(i).cf.pc)}) " +
      p"roqIndex 0x${Hexadecimal(uopEntries(i).roqIdx.asUInt)} " +
      p"cancelled with redirect roqIndex 0x${Hexadecimal(io.redirect.bits.roqIdx.asUInt)}\n")
  }

  // replay: from s_dispatched to s_valid
  val replayValid = io.redirect.valid && io.redirect.bits.isReplay
  val needReplay = Wire(Vec(size, Bool()))
  for (i <- 0 until size) {
    needReplay(i) := roqNeedFlush(i) && stateEntries(i) === s_dispatched && replayValid
    when (needReplay(i)) {
      stateEntries(i) := s_valid
    }

    XSInfo(needReplay(i), p"dispatched entry($i)(pc = ${Hexadecimal(uopEntries(i.U).cf.pc)}) " +
      p"replayed with roqIndex ${io.redirect.bits.roqIdx}\n")
  }

  /**
    * Part 2: walk
    *
    * Instead of keeping the walking distances, we keep the walking target position for simplicity.
    *
    * (1) replay: move dispatchPtr to the first needReplay entry
    * (2) redirect (branch misprediction): move dispatchPtr, tailPtr to the first cancelled entry
    *
    */
  // getFirstIndex: get the head index of consecutive ones
  // note that it returns the position starting from either the leftmost or the rightmost
  // 00000001 => 0
  // 00111000 => 3
  // 11000111 => 2
  // 10000000 => 1
  // 00000000 => 7
  // 11111111 => 7
  def getFirstMaskPosition(mask: Seq[Bool]) = {
    Mux(mask(size - 1),
      PriorityEncoder(mask.reverse.map(m => !m)),
      PriorityEncoder(mask)
    )
  }

  val maskedNeedReplay = Cat(needReplay.reverse) & dispatchedMask
  val allCancel = Cat(needCancel).andR
  val someReplay = Cat(maskedNeedReplay).orR
  val allReplay = Cat(maskedNeedReplay).andR
  XSDebug(replayValid, p"needReplay: ${Binary(Cat(needReplay))}\n")
  XSDebug(replayValid, p"dispatchedMask: ${Binary(dispatchedMask)}\n")
  XSDebug(replayValid, p"maskedNeedReplay: ${Binary(maskedNeedReplay)}\n")
  // when nothing or everything is cancelled or replayed, the pointers remain unchanged
  // if any uop is cancelled or replayed, the pointer should go to the first zero before all ones
  // position: target index
  //   (1) if leftmost bits are ones, count continuous ones from leftmost (target position is the last one)
  //   (2) if leftmost bit is zero, count rightmost zero btis (target position is the first one)
  // if all bits are one, we need to keep the index unchanged
  // 00000000, 11111111: unchanged
  // otherwise: firstMaskPosition
  val cancelPosition = Mux(!Cat(needCancel).orR || allCancel, tailPtr.value, getFirstMaskPosition(needCancel))
  val replayPosition = Mux(!someReplay || allReplay, dispatchPtr.value, getFirstMaskPosition(maskedNeedReplay.asBools))
  XSDebug(replayValid, p"getFirstMaskPosition: ${getFirstMaskPosition(maskedNeedReplay.asBools)}\n")
  assert(cancelPosition.getWidth == indexWidth)
  assert(replayPosition.getWidth == indexWidth)
  // If the highest bit is one, the direction flips.
  // Otherwise, the direction keeps the same.
  val tailCancelPtr = Wire(new CircularQueuePtr(size))
  tailCancelPtr.flag := Mux(needCancel(size - 1), ~tailPtr.flag, tailPtr.flag)
  tailCancelPtr.value := Mux(needCancel(size - 1) && !allCancel, size.U - cancelPosition, cancelPosition)
  // In case of branch mis-prediction:
  // If mis-prediction happens after dispatchPtr, the pointer keeps the same as before.
  // If dispatchPtr needs to be cancelled, reset dispatchPtr to tailPtr.
  val dispatchCancelPtr = Mux(needCancel(dispatchPtr.value) || dispatchEntries === 0.U, tailCancelPtr, dispatchPtr)
  // In case of replay, we need to walk back and recover preg states in the busy table.
  // We keep track of the number of entries needed to be walked instead of target position to reduce overhead
  // for 11111111, replayPosition is unuseful. We naively set Cnt to size.U
  val dispatchReplayCnt = Mux(
    allReplay, size.U,
    Mux(maskedNeedReplay(size - 1),
      // replay makes flag flipped
      dispatchPtr.value + replayPosition,
      // the new replay does not change the flag
      Mux(dispatchPtr.value <= replayPosition,
        // but we are currently in a replay that changes the flag
        dispatchPtr.value + (size.U - replayPosition),
        dispatchPtr.value - replayPosition)))
  val dispatchReplayCntReg = RegInit(0.U)
  // actually, if deqIndex points to head uops and they are replayed, there's no need for extraWalk
  // however, to simplify logic, we simply let it do extra walk now
  val needExtraReplayWalk = Cat((0 until deqnum).map(i => needReplay(deqIndex(i)))).orR
  val needExtraReplayWalkReg = RegNext(needExtraReplayWalk && replayValid, false.B)
  val inReplayWalk = dispatchReplayCntReg =/= 0.U || needExtraReplayWalkReg
  val dispatchReplayStep = Mux(needExtraReplayWalkReg, 0.U, Mux(dispatchReplayCntReg > replayWidth.U, replayWidth.U, dispatchReplayCntReg))
  when (exceptionValid) {
    dispatchReplayCntReg := 0.U
  }.elsewhen (inReplayWalk && mispredictionValid && needCancel((dispatchPtr - 1.U).value)) {
    val distance = distanceBetween(dispatchPtr, tailCancelPtr)
    dispatchReplayCntReg := Mux(dispatchReplayCntReg > distance, dispatchReplayCntReg - distance, 0.U)
  }.elsewhen (replayValid && someReplay) {
    dispatchReplayCntReg := dispatchReplayCnt - dispatchReplayStep
  }.elsewhen (!needExtraReplayWalkReg) {
    dispatchReplayCntReg := dispatchReplayCntReg - dispatchReplayStep
  }

  io.inReplayWalk := inReplayWalk
  val replayIndex = (0 until replayWidth).map(i => (dispatchPtr - (i + 1).U).value)
  for (i <- 0 until replayWidth) {
    val index =  Mux(needExtraReplayWalkReg, (if (i < deqnum) deqIndex(i) else 0.U), replayIndex(i))
    val shouldResetDest = inReplayWalk && stateEntries(index) === s_valid
    io.replayPregReq(i).isInt := shouldResetDest && uopEntries(index).ctrl.rfWen && uopEntries(index).ctrl.ldest =/= 0.U
    io.replayPregReq(i).isFp  := shouldResetDest && uopEntries(index).ctrl.fpWen
    io.replayPregReq(i).preg  := uopEntries(index).pdest

    XSDebug(shouldResetDest, p"replay $i: " +
      p"type (${uopEntries(index).ctrl.rfWen}, ${uopEntries(index).ctrl.fpWen}) " +
      p"pdest ${uopEntries(index).pdest} ldest ${uopEntries(index).ctrl.ldest}\n")
  }

  /**
    * Part 3: update indices
    *
    * tail: (1) enqueue; (2) walk in case of redirect
    * dispatch: (1) dequeue; (2) walk in case of replay; (3) walk in case of redirect
    * head: commit
    */
  // enqueue
  val numEnq = Mux(canActualEnqueue, PriorityEncoder(io.enq.map(!_.valid) :+ true.B), 0.U)
  XSError(numEnq =/= 0.U && (mispredictionValid || exceptionValid), "should not enqueue when redirect\n")
  tailPtr := Mux(exceptionValid,
    0.U.asTypeOf(new CircularQueuePtr(size)),
    Mux(mispredictionValid,
      tailCancelPtr,
      tailPtr + numEnq)
  )

  // dequeue
  val numDeqTry = Mux(dispatchEntries > deqnum.U, deqnum.U, dispatchEntries)
  val numDeqFire = PriorityEncoder(io.deq.zipWithIndex.map{case (deq, i) =>
    // For dequeue, the first entry should never be s_invalid
    // Otherwise, there should be a redirect and tail walks back
    // in this case, we set numDeq to 0
    !deq.fire() && (if (i == 0) true.B else stateEntries(deqIndex(i)) =/= s_dispatched)
  } :+ true.B)
  val numDeq = Mux(numDeqTry > numDeqFire, numDeqFire, numDeqTry)
  dispatchPtr := Mux(exceptionValid,
    0.U.asTypeOf(new CircularQueuePtr(size)),
    Mux(mispredictionValid && (!inReplayWalk || needCancel((dispatchPtr - 1.U).value)),
      dispatchCancelPtr,
      Mux(inReplayWalk, dispatchPtr - dispatchReplayStep, dispatchPtr + numDeq))
  )

  headPtr := Mux(exceptionValid, 0.U.asTypeOf(new CircularQueuePtr(size)), headPtr + PopCount(needDequeue))

  /**
    * Part 4: set output and input
    */
  // TODO: remove this when replay moves to roq
  for (i <- 0 until deqnum) {
    io.deq(i).bits := uopEntries(deqIndex(i))
    // do not dequeue when io.redirect valid because it may cause dispatchPtr work improperly
    io.deq(i).valid := stateEntries(deqIndex(i)) === s_valid && !io.redirect.valid && allWalkDone
  }

  // debug: dump dispatch queue states
  XSDebug(p"head: $headPtr, tail: $tailPtr, dispatch: $dispatchPtr, " +
    p"replayCnt: $dispatchReplayCntReg, needExtraReplayWalkReg: $needExtraReplayWalkReg\n")
  XSDebug(p"state: ")
  stateEntries.reverse.foreach { s =>
    XSDebug(false, s === s_invalid, "-")
    XSDebug(false, s === s_valid, "v")
    XSDebug(false, s === s_dispatched, "d")
  }
  XSDebug(false, true.B, "\n")
  XSDebug(p"ptr:   ")
  (0 until size).reverse.foreach { i =>
    val isPtr = i.U === headPtr.value || i.U === tailPtr.value || i.U === dispatchPtr.value
    XSDebug(false, isPtr, "^")
    XSDebug(false, !isPtr, " ")
  }
  XSDebug(false, true.B, "\n")

  XSError(isAfter(headPtr, tailPtr), p"assert greaterOrEqualThan(tailPtr: $tailPtr, headPtr: $headPtr) failed\n")
  XSError(isAfter(dispatchPtr, tailPtr) && !inReplayWalk, p"assert greaterOrEqualThan(tailPtr: $tailPtr, dispatchPtr: $dispatchPtr) failed\n")
  XSError(isAfter(headPtr, dispatchPtr), p"assert greaterOrEqualThan(dispatchPtr: $dispatchPtr, headPtr: $headPtr) failed\n")
  XSError(validEntries < dispatchEntries && !inReplayWalk, "validEntries should be less than dispatchEntries\n")
}
