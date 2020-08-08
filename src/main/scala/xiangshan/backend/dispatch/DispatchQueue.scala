package xiangshan.backend.dispatch

import chisel3._
import chisel3.util._
import utils.{XSDebug, XSError, XSInfo}
import xiangshan.backend.decode.SrcType
import xiangshan.{MicroOp, Redirect, ReplayPregReq, RoqCommit, XSBundle, XSModule}


class DispatchQueueIO(enqnum: Int, deqnum: Int, replayWidth: Int) extends XSBundle {
  val enq = Vec(enqnum, Flipped(DecoupledIO(new MicroOp)))
  val deq = Vec(deqnum, DecoupledIO(new MicroOp))
  val commits = Input(Vec(CommitWidth, Valid(new RoqCommit)))
  val redirect = Flipped(ValidIO(new Redirect))
  val replayPregReq = Output(Vec(replayWidth, new ReplayPregReq))

  override def cloneType: DispatchQueueIO.this.type =
    new DispatchQueueIO(enqnum, deqnum, replayWidth).asInstanceOf[this.type]
}

// dispatch queue: accepts at most enqnum uops from dispatch1 and dispatches deqnum uops at every clock cycle
class DispatchQueue(size: Int, enqnum: Int, deqnum: Int, replayWidth: Int) extends XSModule {
  val io = IO(new DispatchQueueIO(enqnum, deqnum, replayWidth))
  val indexWidth = log2Ceil(size)

  val s_invalid :: s_valid :: s_dispatched :: Nil = Enum(3)

  // queue data array
  val uopEntries = Mem(size, new MicroOp)//Reg(Vec(size, new MicroOp))
  val stateEntries = RegInit(VecInit(Seq.fill(size)(s_invalid)))
  // head: first valid entry (dispatched entry)
  val headPtr = RegInit(0.U((indexWidth + 1).W))
  val headIndex = headPtr(indexWidth - 1, 0)
  val headDirection = headPtr(indexWidth)
  // dispatch: first entry that has not been dispatched
  val dispatchPtr = RegInit(0.U((indexWidth + 1).W))
  val dispatchIndex = dispatchPtr(indexWidth - 1, 0)
  val dispatchDirection = dispatchPtr(indexWidth)
  // tail: first invalid entry (free entry)
  val tailPtr = RegInit(0.U((indexWidth + 1).W))
  val tailIndex = tailPtr(indexWidth - 1, 0)
  val tailDirection = tailPtr(indexWidth)

  // TODO: make ptr a vector to reduce latency?
  // commit: starting from head ptr
  val commitPtr = (0 until CommitWidth).map(i => headPtr + i.U)
  val commitIndex = commitPtr.map(ptr => ptr(indexWidth - 1, 0))
  // deq: starting from dispatch ptr
  val deqPtr = (0 until enqnum).map(i => dispatchPtr + i.U)
  val deqIndex = deqPtr.map(ptr => ptr(indexWidth - 1, 0))
  // enq: starting from tail ptr
  val enqPtr = (0 until enqnum).map(i => tailPtr + i.U)
  val enqIndex = enqPtr.map(ptr => ptr(indexWidth - 1, 0))

  def distanceBetween(left: UInt, right: UInt) = {
    Mux(left(indexWidth) === right(indexWidth),
      left(indexWidth - 1, 0) - right(indexWidth - 1, 0),
      size.U + left(indexWidth - 1, 0) - right(indexWidth - 1, 0))
  }

  val validEntries = distanceBetween(tailPtr, headPtr)
  val dispatchEntries = distanceBetween(tailPtr, dispatchPtr)
  val commitEntries = validEntries - dispatchEntries
  val emptyEntries = size.U - validEntries

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
  for (i <- 0 until enqnum) {
    when (io.enq(i).fire()) {
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
  val numCommit = PopCount(io.commits.map(commit => !commit.bits.isWalk && commit.valid))
  val commitBits = (1.U((CommitWidth+1).W) << numCommit).asUInt() - 1.U
  for (i <- 0 until CommitWidth) {
    when (commitBits(i)) {
      stateEntries(commitIndex(i)) := s_invalid

      XSError(stateEntries(commitIndex(i)) =/= s_dispatched, "state of the commit entry is not s_dispatched\n")
    }
  }

  // redirect: cancel uops currently in the queue
  val mispredictionValid = io.redirect.valid && io.redirect.bits.isMisPred
  val exceptionValid = io.redirect.valid && io.redirect.bits.isException
  val roqNeedFlush = Wire(Vec(size, Bool()))
  val needCancel = Wire(Vec(size, Bool()))
  for (i <- 0 until size) {
    roqNeedFlush(i) := uopEntries(i.U).needFlush(io.redirect)
    needCancel(i) := stateEntries(i) =/= s_invalid && ((roqNeedFlush(i) && mispredictionValid) || exceptionValid)
    when (needCancel(i)) {
      stateEntries(i) := s_invalid
    }

    XSInfo(needCancel(i), p"valid entry($i)(pc = ${Hexadecimal(uopEntries(i.U).cf.pc)})" +
      p"roqIndex 0x${Hexadecimal(uopEntries(i.U).roqIdx)} " +
      p"cancelled with redirect roqIndex 0x${Hexadecimal(io.redirect.bits.roqIdx)}\n")
  }

  // replay: from s_dispatched to s_valid
  val needReplay = Wire(Vec(size, Bool()))
  for (i <- 0 until size) {
    needReplay(i) := roqNeedFlush(i) && stateEntries(i) === s_dispatched && io.redirect.bits.isReplay
    when (needReplay(i)) {
      stateEntries(i) := s_valid
    }

    XSInfo(needReplay(i), p"dispatched entry($i)(pc = ${Hexadecimal(uopEntries(i.U).cf.pc)}) " +
      p"replayed with roqIndex ${Hexadecimal(io.redirect.bits.roqIdx)}\n")
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
  // 11000111 => 1
  // 10000000 => 0
  def getFirstMaskPosition(mask: Vec[Bool]) = {
    Mux(mask(size - 1),
      PriorityEncoder(mask.reverse.map(m => !m)),
      PriorityEncoder(mask)
    )
  }

  val cancelPosition = getFirstMaskPosition(needCancel)
  val replayPosition = getFirstMaskPosition(needReplay)
  assert(cancelPosition.getWidth == indexWidth)
  assert(replayPosition.getWidth == indexWidth)
  // If the highest bit is one, the direction flips.
  // Otherwise, the direction keeps the same.
  val tailCancelPtr = Cat(Mux(needCancel(size - 1), ~tailDirection, tailDirection), cancelPosition)
  // In case of branch mis-prediction, the last dispatched instruction must be the mis-prediction instruction.
  // Thus, we only need to reset dispatchPtr to tailPtr.
  val dispatchCancelPtr = tailCancelPtr
  // In case of replay, we need to walk back and recover preg states in the busy table.
  // We keep track of the number of entries needed to be walked instead of target position to reduce overhead
  val dispatchReplayCnt = Mux(needReplay(size - 1), dispatchIndex + replayPosition, dispatchIndex - replayPosition)
  val dispatchReplayCntReg = RegInit(0.U(indexWidth.W))
  val inReplayWalk = dispatchReplayCntReg =/= 0.U
  val dispatchReplayStep = Mux(dispatchReplayCntReg > replayWidth.U, replayWidth.U, dispatchReplayCntReg)
  when (exceptionValid) {
    dispatchReplayCntReg := 0.U
  }.elsewhen (inReplayWalk && mispredictionValid && needCancel(dispatchIndex)) {
    dispatchReplayCntReg := dispatchReplayCntReg - distanceBetween(dispatchIndex, tailCancelPtr)
  }.elsewhen (replayValid) {
    dispatchReplayCntReg := dispatchReplayCnt
  }.otherwise {
    dispatchReplayCntReg := dispatchReplayCntReg - dispatchReplayStep
  }

  val replayIndex = (0 until replayWidth).map(i => (dispatchPtr - i.U)(indexWidth - 1, 0))
  for (i <- 0 until replayWidth) {
    val replayValid = stateEntries(replayIndex(i)) === s_valid
    io.replayPregReq(i).isInt := replayValid && uopEntries(replayIndex(i)).ctrl.src1Type === SrcType.reg
    io.replayPregReq(i).isFp  := replayValid && uopEntries(replayIndex(i)).ctrl.src1Type === SrcType.fp
    io.replayPregReq(i).preg  := uopEntries(replayIndex(i)).pdest
  }

  /**
    * Part 3: update indices
    *
    * tail: (1) enqueue; (2) walk in case of redirect
    * dispatch: (1) dequeue; (2) walk in case of replay; (3) walk in case of redirect
    * head: commit
    */
  // enqueue
  val numEnqTry = Mux(emptyEntries > enqnum.U, enqnum.U, emptyEntries)
  val numEnq = PriorityEncoder(io.enq.map(!_.fire()) :+ true.B)
  XSError(numEnq =/= 0.U && (mispredictionValid || exceptionValid), "should not enqueue when redirect\n")
  tailPtr := Mux(exceptionValid,
    0.U,
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
    0.U,
    Mux(mispredictionValid && needCancel(dispatchIndex),
      dispatchCancelPtr,
      dispatchPtr + Mux(inReplayWalk, -dispatchReplayStep, numDeq))
  )

  headPtr := Mux(exceptionValid, 0.U, headPtr + numCommit)

  /**
    * Part 4: set output and input
    */
  val enqReadyBits = (1.U << numEnqTry).asUInt() - 1.U
  for (i <- 0 until enqnum) {
    io.enq(i).ready := enqReadyBits(i).asBool() && !inReplayWalk
  }

  for (i <- 0 until deqnum) {
    io.deq(i).bits := uopEntries(deqIndex(i))
    // do not dequeue when io.redirect valid because it may cause dispatchPtr work improperly
    io.deq(i).valid := stateEntries(deqIndex(i)) === s_valid && !io.redirect.valid && !inReplayWalk
  }

  // debug: dump dispatch queue states
  def greaterOrEqualThan(left: UInt, right: UInt) = {
    Mux(
      left(indexWidth) === right(indexWidth),
      left(indexWidth - 1, 0) >= right(indexWidth - 1, 0),
      left(indexWidth - 1, 0) <= right(indexWidth - 1, 0)
    )
  }
  XSError(!greaterOrEqualThan(tailPtr, headPtr), p"assert greaterOrEqualThan(tailPtr: $tailPtr, headPtr: $headPtr) failed\n")
  XSError(!greaterOrEqualThan(tailPtr, dispatchPtr) && !inReplayWalk, p"assert greaterOrEqualThan(tailPtr: $tailPtr, dispatchPtr: $dispatchPtr) failed\n")
  XSError(!greaterOrEqualThan(dispatchPtr, headPtr), p"assert greaterOrEqualThan(dispatchPtr: $dispatchPtr, headPtr: $headPtr) failed\n")
  XSError(validEntries < dispatchEntries && !inReplayWalk, "validEntries should be less than dispatchEntries\n")

  XSDebug(p"head: $headPtr, tail: $tailPtr, dispatch: $dispatchPtr\n")
  XSDebug(p"state: ")
  stateEntries.reverse.foreach { s =>
    XSDebug(false, s === s_invalid, "-")
    XSDebug(false, s === s_valid, "v")
    XSDebug(false, s === s_dispatched, "d")
  }
  XSDebug(false, true.B, "\n")
  XSDebug(p"ptr:   ")
  (0 until size).reverse.foreach { i =>
    val isPtr = i.U === headIndex || i.U === tailIndex || i.U === dispatchIndex
    XSDebug(false, isPtr, "^")
    XSDebug(false, !isPtr, " ")
  }
  XSDebug(false, true.B, "\n")

}
