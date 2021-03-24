package xiangshan.backend.dispatch

import chisel3._
import chisel3.util._
import utils._
import xiangshan._
import xiangshan.backend.roq.RoqPtr

class DispatchQueueIO(enqnum: Int, deqnum: Int) extends XSBundle {
  val enq = new Bundle {
    // output: dispatch queue can accept new requests
    val canAccept = Output(Bool())
    // input: need to allocate new entries (for address computing)
    val needAlloc = Vec(enqnum, Input(Bool()))
    // input: actually do the allocation (for write enable)
    val req = Vec(enqnum, Flipped(ValidIO(new MicroOp)))
  }
  val deq = Vec(deqnum, DecoupledIO(new MicroOp))
  val redirect = Flipped(ValidIO(new Redirect))
  val flush = Input(Bool())
  override def cloneType: DispatchQueueIO.this.type =
    new DispatchQueueIO(enqnum, deqnum).asInstanceOf[this.type]
}

// dispatch queue: accepts at most enqnum uops from dispatch1 and dispatches deqnum uops at every clock cycle
class DispatchQueue(size: Int, enqnum: Int, deqnum: Int, name: String) extends XSModule with HasCircularQueuePtrHelper {
  val io = IO(new DispatchQueueIO(enqnum, deqnum))

  val s_invalid :: s_valid:: Nil = Enum(2)

  // queue data array
  val dataModule = Module(new SyncDataModuleTemplate(new MicroOp, size, deqnum, enqnum))
  val roqIdxEntries = Reg(Vec(size, new RoqPtr))
  val debug_uopEntries = Mem(size, new MicroOp)
  val stateEntries = RegInit(VecInit(Seq.fill(size)(s_invalid)))

  // head: first valid entry (dispatched entry)
  val headPtr = RegInit(VecInit((0 until deqnum).map(_.U.asTypeOf(new CircularQueuePtr(size)))))
  val headPtrMask = UIntToMask(headPtr(0).value, size)
  // tail: first invalid entry (free entry)
  val tailPtr = RegInit(VecInit((0 until enqnum).map(_.U.asTypeOf(new CircularQueuePtr(size)))))
  val tailPtrMask = UIntToMask(tailPtr(0).value, size)
  // valid entries counter
  val validCounter = RegInit(0.U(log2Ceil(size + 1).W))
  val allowEnqueue = RegInit(true.B)

  val isTrueEmpty = ~Cat((0 until size).map(i => stateEntries(i) === s_valid)).orR
  val canEnqueue = allowEnqueue
  val canActualEnqueue = canEnqueue && !(io.redirect.valid || io.flush)

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
  io.enq.canAccept := canEnqueue
  dataModule.io.wen   := VecInit((0 until enqnum).map(_ => false.B))
  dataModule.io.waddr := DontCare
  dataModule.io.wdata := VecInit(io.enq.req.map(_.bits))
  for (i <- 0 until enqnum) {
    when (io.enq.req(i).valid && canActualEnqueue) {
      dataModule.io.wen(i) := true.B
      val sel = if (i == 0) 0.U else PopCount(io.enq.needAlloc.take(i))
      dataModule.io.waddr(i) := tailPtr(sel).value
      roqIdxEntries(tailPtr(sel).value) := io.enq.req(i).bits.roqIdx
      debug_uopEntries(tailPtr(sel).value) := io.enq.req(i).bits
      stateEntries(tailPtr(sel).value) := s_valid
    }
  }

  // dequeue: from s_valid to s_dispatched
  for (i <- 0 until deqnum) {
    when (io.deq(i).fire() && !(io.redirect.valid || io.flush)) {
      stateEntries(headPtr(i).value) := s_invalid

//      XSError(stateEntries(headPtr(i).value) =/= s_valid, "state of the dispatch entry is not s_valid\n")
    }
  }

  // redirect: cancel uops currently in the queue
  val needCancel = Wire(Vec(size, Bool()))
  for (i <- 0 until size) {
    needCancel(i) := stateEntries(i) =/= s_invalid && (roqIdxEntries(i).needFlush(io.redirect, io.flush) || io.flush)

    when (needCancel(i)) {
      stateEntries(i) := s_invalid
    }

    XSInfo(needCancel(i), p"valid entry($i)(pc = ${Hexadecimal(debug_uopEntries(i).cf.pc)}) " +
      p"roqIndex ${roqIdxEntries(i)} " +
      p"cancelled with redirect roqIndex 0x${Hexadecimal(io.redirect.bits.roqIdx.asUInt)}\n")
  }

  /**
    * Part 2: update indices
    *
    * tail: (1) enqueue; (2) redirect
    * head: dequeue
    */

  // dequeue
  val currentValidCounter = distanceBetween(tailPtr(0), headPtr(0))
  val numDeqTry = Mux(currentValidCounter > deqnum.U, deqnum.U, currentValidCounter)
  val numDeqFire = PriorityEncoder(io.deq.zipWithIndex.map{case (deq, i) =>
    // For dequeue, the first entry should never be s_invalid
    // Otherwise, there should be a redirect and tail walks back
    // in this case, we set numDeq to 0
    !deq.fire() && (if (i == 0) true.B else stateEntries(headPtr(i).value) =/= s_invalid)
  } :+ true.B)
  val numDeq = Mux(numDeqTry > numDeqFire, numDeqFire, numDeqTry)
  // agreement with reservation station: don't dequeue when redirect.valid
  val nextHeadPtr = Wire(Vec(deqnum, new CircularQueuePtr(size)))
  for (i <- 0 until deqnum) {
    nextHeadPtr(i) := Mux(io.flush,
      i.U.asTypeOf(new CircularQueuePtr(size)),
      Mux(io.redirect.valid, headPtr(i), headPtr(i) + numDeq))
    headPtr(i) := nextHeadPtr(i)
  }

  // For branch mis-prediction or memory violation replay,
  // we delay updating the indices for one clock cycle.
  // For now, we simply use PopCount to count #instr cancelled.
  val lastCycleMisprediction = RegNext(io.redirect.valid)
  // find the last one's position, starting from headPtr and searching backwards
  val validBitVec = VecInit((0 until size).map(i => stateEntries(i) === s_valid))
  val loValidBitVec = Cat((0 until size).map(i => validBitVec(i) && headPtrMask(i)))
  val hiValidBitVec = Cat((0 until size).map(i => validBitVec(i) && ~headPtrMask(i)))
  val flippedFlag = loValidBitVec.orR || validBitVec(size - 1)
  val lastOneIndex = size.U - PriorityEncoder(Mux(loValidBitVec.orR, loValidBitVec, hiValidBitVec))
  val walkedTailPtr = Wire(new CircularQueuePtr(size))
  walkedTailPtr.flag := flippedFlag ^ headPtr(0).flag
  walkedTailPtr.value := lastOneIndex

  // enqueue
  val numEnq = Mux(io.enq.canAccept, PopCount(io.enq.req.map(_.valid)), 0.U)
  tailPtr(0) := Mux(io.flush,
    0.U.asTypeOf(new CircularQueuePtr(size)),
    Mux(io.redirect.valid,
      tailPtr(0),
      Mux(lastCycleMisprediction,
        Mux(isTrueEmpty, headPtr(0), walkedTailPtr),
        tailPtr(0) + numEnq))
  )
  val lastLastCycleMisprediction = RegNext(lastCycleMisprediction && !io.flush)
  for (i <- 1 until enqnum) {
    tailPtr(i) := Mux(io.flush,
      i.U.asTypeOf(new CircularQueuePtr(size)),
      Mux(io.redirect.valid,
        tailPtr(i),
        Mux(lastLastCycleMisprediction,
          tailPtr(0) + i.U,
          tailPtr(i) + numEnq))
      )
  }

  // update valid counter and allowEnqueue reg
  validCounter := Mux(io.flush,
    0.U,
    Mux(io.redirect.valid,
      validCounter,
      Mux(lastLastCycleMisprediction,
        currentValidCounter,
        validCounter + numEnq - numDeq)
    )
  )
  allowEnqueue := Mux(currentValidCounter > (size - enqnum).U, false.B, numEnq <= (size - enqnum).U - currentValidCounter)

  /**
    * Part 3: set output and input
    */
  // TODO: remove this when replay moves to roq
  dataModule.io.raddr := VecInit(nextHeadPtr.map(_.value))
  for (i <- 0 until deqnum) {
    io.deq(i).bits := dataModule.io.rdata(i)
    io.deq(i).bits.roqIdx := roqIdxEntries(headPtr(i).value)
    // io.deq(i).bits := debug_uopEntries(headPtr(i).value)
    // do not dequeue when io.redirect valid because it may cause dispatchPtr work improperly
    io.deq(i).valid := stateEntries(headPtr(i).value) === s_valid && !lastCycleMisprediction
  }

  // debug: dump dispatch queue states
  XSDebug(p"head: ${headPtr(0)}, tail: ${tailPtr(0)}\n")
  XSDebug(p"state: ")
  stateEntries.reverse.foreach { s =>
    XSDebug(false, s === s_invalid, "-")
    XSDebug(false, s === s_valid, "v")
  }
  XSDebug(false, true.B, "\n")
  XSDebug(p"ptr:   ")
  (0 until size).reverse.foreach { i =>
    val isPtr = i.U === headPtr(0).value || i.U === tailPtr(0).value
    XSDebug(false, isPtr, "^")
    XSDebug(false, !isPtr, " ")
  }
  XSDebug(false, true.B, "\n")

//  XSError(isAfter(headPtr(0), tailPtr(0)), p"assert greaterOrEqualThan(tailPtr: ${tailPtr(0)}, headPtr: ${headPtr(0)}) failed\n")
  QueuePerf(size, PopCount(stateEntries.map(_ =/= s_invalid)), !canEnqueue)
  XSPerf("in", numEnq)
  XSPerf("out", PopCount(io.deq.map(_.fire())))
  XSPerf("out_try", PopCount(io.deq.map(_.valid)))
}
