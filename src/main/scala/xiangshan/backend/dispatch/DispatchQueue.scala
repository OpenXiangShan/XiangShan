package xiangshan.backend.dispatch

import chisel3._
import chisel3.util._
import utils.{XSDebug, XSError, XSInfo}
import xiangshan.{MicroOp, Redirect, RoqCommit, XSBundle, XSModule}


class DispatchQueueIO(enqnum: Int, deqnum: Int) extends XSBundle {
  val enq = Vec(enqnum, Flipped(DecoupledIO(new MicroOp)))
  val deq = Vec(deqnum, DecoupledIO(new MicroOp))
  val commits = Input(Vec(CommitWidth, Valid(new RoqCommit)))
  val redirect = Flipped(ValidIO(new Redirect))

  override def cloneType: DispatchQueueIO.this.type =
    new DispatchQueueIO(enqnum, deqnum).asInstanceOf[this.type]
}

// dispatch queue: accepts at most enqnum uops from dispatch1 and dispatches deqnum uops at every clock cycle
class DispatchQueue(size: Int, enqnum: Int, deqnum: Int) extends XSModule {
  val io = IO(new DispatchQueueIO(enqnum, deqnum))
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
  // walkDispatch: in case of redirect, walk backward
  val walkDispatchPtr =  (0 until RenameWidth).map(i => dispatchPtr - (i + 1).U)
  val walkDispatchIndex = walkDispatchPtr.map(ptr => ptr(indexWidth - 1, 0))
  // walkTail: in case of redirect, walk backward
  val walkTailPtr = (0 until RenameWidth).map(i => tailPtr - (i + 1).U)
  val walkTailIndex = walkTailPtr.map(ptr => ptr(indexWidth - 1, 0))

  // debug: dump dispatch queue states
  def greaterOrEqualThan(left: UInt, right: UInt) = {
    Mux(
      left(indexWidth) === right(indexWidth),
      left(indexWidth - 1, 0) >= right(indexWidth - 1, 0),
      left(indexWidth - 1, 0) <= right(indexWidth - 1, 0)
    )
  }
  XSError(!greaterOrEqualThan(tailPtr, headPtr), p"assert greaterOrEqualThan(tailPtr: $tailPtr, headPtr: $headPtr) failed\n")
  XSError(!greaterOrEqualThan(tailPtr, dispatchPtr), p"assert greaterOrEqualThan(tailPtr: $tailPtr, dispatchPtr: $dispatchPtr) failed\n")
  XSError(!greaterOrEqualThan(dispatchPtr, headPtr), p"assert greaterOrEqualThan(dispatchPtr: $dispatchPtr, headPtr: $headPtr) failed\n")

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

  val validEntries = Mux(headDirection === tailDirection, tailIndex - headIndex, size.U + tailIndex - headIndex)
  val dispatchEntries = Mux(dispatchDirection === tailDirection, tailIndex - dispatchIndex, size.U + tailIndex - dispatchIndex)
  XSError(validEntries < dispatchEntries, "validEntries should be less than dispatchEntries\n")
  val commitEntries = validEntries - dispatchEntries
  val emptyEntries = size.U - validEntries

  /**
    * Part 1: update states and uops when enqueue, dequeue, commit, redirect/replay
    */
  for (i <- 0 until enqnum) {
    when (io.enq(i).fire()) {
      uopEntries(enqIndex(i)) := io.enq(i).bits
      stateEntries(enqIndex(i)) := s_valid
    }
  }

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
  val exceptionValid = io.redirect.valid && io.redirect.bits.isException
  val roqNeedFlush = Wire(Vec(size, Bool()))
  for (i <- 0 until size) {
    roqNeedFlush(i) := uopEntries(i.U).needFlush(io.redirect)
    val needCancel = stateEntries(i) =/= s_invalid && ((roqNeedFlush(i) && io.redirect.bits.isMisPred) || exceptionValid)
    when (needCancel) {
      stateEntries(i) := s_invalid
    }

    XSInfo(needCancel, p"valid entry($i)(pc = ${Hexadecimal(uopEntries(i.U).cf.pc)}) " +
      p"cancelled with roqIndex ${Hexadecimal(io.redirect.bits.roqIdx)}\n")
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
    * Part 2: update indices
    *
    * tail: (1) enqueue; (2) walk in case of redirect
    * dispatch: (1) dequeue; (2) replay; (3) walk in case of redirect
    * head: commit
    */
  // enqueue
  val numEnqTry = Mux(emptyEntries > enqnum.U, enqnum.U, emptyEntries)
  val numEnq = PriorityEncoder(io.enq.map(!_.fire()) :+ true.B)
  val numWalkTailTry = PriorityEncoder(walkTailIndex.map(i => stateEntries(i) =/= s_invalid) :+ true.B)
  val numWalkTail = Mux(numWalkTailTry > validEntries, validEntries, numWalkTailTry)
  XSError(numEnq =/= 0.U && numWalkTail =/= 0.U, "should not enqueue when walk\n")
  tailPtr := Mux(exceptionValid, 0.U, tailPtr + Mux(numEnq =/= 0.U, numEnq, -numWalkTail))

  // dequeue
  val numDeqTry = Mux(dispatchEntries > deqnum.U, deqnum.U, dispatchEntries)
  val numDeqFire = PriorityEncoder(io.deq.zipWithIndex.map{case (deq, i) =>
    // For dequeue, the first entry should never be s_invalid
    // Otherwise, there should be a redirect and tail walks back
    // in this case, we set numDeq to 0
    !deq.fire() && (if (i == 0) true.B else stateEntries(deqIndex(i)) =/= s_dispatched)
  } :+ true.B)
  val numDeq = Mux(numDeqTry > numDeqFire, numDeqFire, numDeqTry)
  // TODO: this is unaccptable since it needs to add 64 bits
  val headMask = (1.U((size+1).W) << headIndex) - 1.U
  val dispatchMask = (1.U((size + 1).W) << dispatchIndex) - 1.U
  val mask = headMask ^ dispatchMask
  val replayMask = Mux(headDirection === dispatchDirection, mask, ~mask)
  val numReplay = PopCount((0 until size).map(i => needReplay(i) & replayMask(i)))
  val numWalkDispatchTry = PriorityEncoder(walkDispatchPtr.map(i => stateEntries(i) =/= s_invalid) :+ true.B)
  val numWalkDispatch = Mux(numWalkDispatchTry > commitEntries, commitEntries, numWalkDispatchTry)
  val walkCntDispatch = numWalkDispatch + numReplay
  // note that numDeq === 0.U entries after dispatch are all flushed
  // so, numDeq and walkCntDispatch cannot be nonzero at the same time
  XSError(numDeq =/= 0.U && walkCntDispatch =/= 0.U, "should not dequeue when walk\n")
  dispatchPtr := Mux(exceptionValid, 0.U, dispatchPtr + Mux(numDeq =/= 0.U, numDeq, -walkCntDispatch))

  headPtr := Mux(exceptionValid, 0.U, headPtr + numCommit)

  /**
    * Part 3: set output and input
    */
  val enqReadyBits = (1.U << numEnqTry).asUInt() - 1.U
  for (i <- 0 until enqnum) {
    io.enq(i).ready := enqReadyBits(i).asBool()
  }

  for (i <- 0 until deqnum) {
    io.deq(i).bits := uopEntries(deqIndex(i))
    // do not dequeue when io.redirect valid because it may cause dispatchPtr work improperly
    io.deq(i).valid := stateEntries(deqIndex(i)) === s_valid && !io.redirect.valid
  }

}
