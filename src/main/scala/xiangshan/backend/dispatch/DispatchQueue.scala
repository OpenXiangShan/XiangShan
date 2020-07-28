package xiangshan.backend.dispatch

import chisel3._
import chisel3.util._
import utils.{XSDebug, XSInfo}
import xiangshan.{MicroOp, Redirect, XSBundle, XSModule}


class DispatchQueueIO(enqnum: Int, deqnum: Int) extends XSBundle {
  val enq = Vec(enqnum, Flipped(DecoupledIO(new MicroOp)))
  val deq = Vec(deqnum, DecoupledIO(new MicroOp))
  val redirect = Flipped(ValidIO(new Redirect))

  override def cloneType: DispatchQueueIO.this.type =
    new DispatchQueueIO(enqnum, deqnum).asInstanceOf[this.type]
}

class DispatchQEntry extends XSBundle {
  val uop = new MicroOp
  val state = UInt(2.W)
}

// dispatch queue: accepts at most enqnum uops from dispatch1 and dispatches deqnum uops at every clock cycle
class DispatchQueue(size: Int, enqnum: Int, deqnum: Int, name: String) extends XSModule {
  val io = IO(new DispatchQueueIO(enqnum, deqnum))
  val indexWidth = log2Ceil(size)

  val s_valid :: s_dispatched :: s_invalid :: Nil = Enum(3)

  // queue data array
  val entries = Reg(Vec(size, new DispatchQEntry))
  val headPtr = RegInit(0.U((indexWidth + 1).W))
  val dispatchPtr = RegInit(0.U((indexWidth + 1).W))
  val tailPtr = RegInit(0.U((indexWidth + 1).W))
  val headIndex = headPtr(indexWidth - 1, 0)
  val dispatchIndex = dispatchPtr(indexWidth - 1, 0)
  val tailIndex = tailPtr(indexWidth - 1, 0)
  val headDirection = headPtr(indexWidth)
  val dispatchDirection = dispatchPtr(indexWidth)
  val tailDirection = tailPtr(indexWidth)
  val commitPtr = (0 until CommitWidth).map(i => headPtr + i.U)
  val enqPtr = (0 until enqnum).map(i => tailPtr + i.U)
  val deqPtr = (0 until enqnum).map(i => dispatchDirection + i.U)
  val commitIndex = commitPtr.map(ptr => ptr(indexWidth - 1, 0))
  val enqIndex = enqPtr.map(ptr => ptr(indexWidth - 1, 0))
  val deqIndex = deqPtr.map(ptr => ptr(indexWidth - 1, 0))

  val validEntries = Mux(headDirection === tailDirection, tailIndex - headIndex, size.U + tailIndex - headIndex)
  val dispatchEntries = Mux(dispatchDirection === tailDirection, tailIndex - dispatchIndex, size.U + tailIndex - dispatchIndex)
  val emptyEntries = size.U - validEntries

  // check whether valid uops are canceled
  val cancel = Wire(Vec(size, Bool()))
  for (i <- 0 until size) {
    cancel(i) := entries(i).uop.brTag.needFlush(io.redirect)
  }

  // cancelled uops should be set to invalid from enqueue input
  // we don't need to compare their brTags here
  for (i <- 0 until enqnum) {
    when (io.enq(i).fire()) {
      entries(enqIndex(i)).uop := io.enq(i).bits
      entries(enqIndex(i)).state := s_valid
    }
  }

  for (i <- 0 until deqnum) {
    when (io.deq(i).fire()) {
      entries(deqIndex(i)).state := s_dispatched
    }
  }

  // cancel uops currently in the queue
  for (i <- 0 until size) {
    val needCancel = cancel(i) && entries(i).state === s_valid
    when (needCancel) {
      entries(i).state := s_invalid
    }
    XSInfo(needCancel, p"$name: valid entry($i)(pc = ${Hexadecimal(entries(i).uop.cf.pc)})" +
      p"cancelled with brTag ${Hexadecimal(io.redirect.bits.brTag.value)}\n")
  }

  // enqueue
  val numEnqTry = PriorityEncoder(io.enq.map(!_.valid) :+ true.B)
  val numEnq = Mux(emptyEntries > numEnqTry, numEnqTry, emptyEntries)
  val enqReadyBits = (1.U << numEnq).asUInt() - 1.U
  (0 until enqnum).map(i => io.enq(i).ready := enqReadyBits(i).asBool())
  tailPtr := tailPtr + numEnq

  // dequeue
  val numDeqTry = Mux(dispatchEntries > deqnum.U, deqnum.U, dispatchEntries)
  val numDeqFire = PriorityEncoder((io.deq.zipWithIndex map { case (deq, i) =>
    !deq.fire() && entries(deqIndex(i)).state === s_valid
  }) :+ true.B)
  val numDeq = Mux(numDeqTry > numDeqFire, numDeqFire, numDeqTry)
  for (i <- 0 until deqnum) {
    io.deq(i).bits := entries(deqIndex(i)).uop
    // needs to cancel uops trying to dequeue
    io.deq(i).valid := entries(deqIndex(i)).state === s_valid && !cancel(deqIndex(i))
  }

  // replay
  val numReplay = 0.U

  dispatchPtr := dispatchPtr + numDeq - numReplay

  // commit
  val numCommit = 0.U
  val commitBits = (1.U << numCommit).asUInt() - 1.U
  for (i <- 0 until CommitWidth) {
    when (commitBits(i)) {
      entries(commitIndex(i)).state := s_invalid
    }
  }
  headPtr := headPtr + numCommit
}
