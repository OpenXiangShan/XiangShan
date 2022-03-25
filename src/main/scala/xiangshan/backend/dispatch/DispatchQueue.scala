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

package xiangshan.backend.dispatch

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import xiangshan._
import xiangshan.backend.rob.RobPtr

class DispatchQueueIO(enqnum: Int, deqnum: Int)(implicit p: Parameters) extends XSBundle {
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
  val dqFull = Output(Bool())
}

// dispatch queue: accepts at most enqnum uops from dispatch1 and dispatches deqnum uops at every clock cycle
class DispatchQueue(size: Int, enqnum: Int, deqnum: Int)(implicit p: Parameters)
  extends XSModule with HasCircularQueuePtrHelper with HasPerfEvents {
  val io = IO(new DispatchQueueIO(enqnum, deqnum))

  val s_invalid :: s_valid :: Nil = Enum(2)

  // queue data array
  val dataModule = Module(new SyncDataModuleTemplate(new MicroOp, size, deqnum, enqnum))
  val robIdxEntries = Reg(Vec(size, new RobPtr))
  val debug_uopEntries = Mem(size, new MicroOp)
  val stateEntries = RegInit(VecInit(Seq.fill(size)(s_invalid)))

  class DispatchQueuePtr extends CircularQueuePtr[DispatchQueuePtr](size)

  // head: first valid entry (dispatched entry)
  val headPtr = RegInit(VecInit((0 until deqnum).map(_.U.asTypeOf(new DispatchQueuePtr))))
  val headPtrMask = UIntToMask(headPtr(0).value, size)
  // tail: first invalid entry (free entry)
  val tailPtr = RegInit(VecInit((0 until enqnum).map(_.U.asTypeOf(new DispatchQueuePtr))))
  val tailPtrMask = UIntToMask(tailPtr(0).value, size)
  // valid entries counter
  val validCounter = RegInit(0.U(log2Ceil(size + 1).W))
  val allowEnqueue = RegInit(true.B)

  val isTrueEmpty = ~Cat((0 until size).map(i => stateEntries(i) === s_valid)).orR
  val canEnqueue = allowEnqueue
  val canActualEnqueue = canEnqueue && !io.redirect.valid

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
  dataModule.io.wen := VecInit((0 until enqnum).map(_ => false.B))
  dataModule.io.waddr := DontCare
  dataModule.io.wdata := VecInit(io.enq.req.map(_.bits))
  for (i <- 0 until enqnum) {
    when(io.enq.req(i).valid && canActualEnqueue) {
      dataModule.io.wen(i) := true.B
      val sel = if (i == 0) 0.U else PopCount(io.enq.needAlloc.take(i))
      dataModule.io.waddr(i) := tailPtr(sel).value
      robIdxEntries(tailPtr(sel).value) := io.enq.req(i).bits.robIdx
      debug_uopEntries(tailPtr(sel).value) := io.enq.req(i).bits
      stateEntries(tailPtr(sel).value) := s_valid
      XSError(sel =/= PopCount(io.enq.req.take(i).map(_.valid)), "why not continuous??\n")
    }
  }

  // dequeue: from s_valid to s_dispatched
  for (i <- 0 until deqnum) {
    when(io.deq(i).fire() && !io.redirect.valid) {
      stateEntries(headPtr(i).value) := s_invalid

      // XSError(stateEntries(headPtr(i).value) =/= s_valid, "state of the dispatch entry is not s_valid\n")
    }
  }

  // redirect: cancel uops currently in the queue
  val needCancel = Wire(Vec(size, Bool()))
  for (i <- 0 until size) {
    needCancel(i) := stateEntries(i) =/= s_invalid && robIdxEntries(i).needFlush(io.redirect)

    when(needCancel(i)) {
      stateEntries(i) := s_invalid
    }

    XSInfo(needCancel(i), p"valid entry($i)(pc = ${Hexadecimal(debug_uopEntries(i).cf.pc)}) " +
      p"robIndex ${robIdxEntries(i)} " +
      p"cancelled with redirect robIndex 0x${Hexadecimal(io.redirect.bits.robIdx.asUInt)}\n")
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
  val numDeqFire = PriorityEncoder(io.deq.zipWithIndex.map { case (deq, i) =>
    // For dequeue, the first entry should never be s_invalid
    // Otherwise, there should be a redirect and tail walks back
    // in this case, we set numDeq to 0
    !deq.fire() && (if (i == 0) true.B else stateEntries(headPtr(i).value) =/= s_invalid)
  } :+ true.B)
  val numDeq = Mux(numDeqTry > numDeqFire, numDeqFire, numDeqTry)
  // agreement with reservation station: don't dequeue when redirect.valid
  val nextHeadPtr = Wire(Vec(deqnum, new DispatchQueuePtr))
  for (i <- 0 until deqnum) {
    nextHeadPtr(i) := Mux(io.redirect.valid, headPtr(i), headPtr(i) + numDeq)
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
  val leadingZeros = PriorityEncoder(Mux(loValidBitVec.orR, loValidBitVec, hiValidBitVec))
  val lastOneIndex = Mux(leadingZeros === 0.U, 0.U, size.U - leadingZeros)
  val walkedTailPtr = Wire(new DispatchQueuePtr)
  walkedTailPtr.flag := flippedFlag ^ headPtr(0).flag
  walkedTailPtr.value := lastOneIndex

  // enqueue
  val numEnq = Mux(io.enq.canAccept, PopCount(io.enq.req.map(_.valid)), 0.U)
  tailPtr(0) := Mux(io.redirect.valid,
    tailPtr(0),
    Mux(lastCycleMisprediction,
      Mux(isTrueEmpty, headPtr(0), walkedTailPtr),
      tailPtr(0) + numEnq)
  )
  val lastLastCycleMisprediction = RegNext(lastCycleMisprediction)
  for (i <- 1 until enqnum) {
    tailPtr(i) := Mux(io.redirect.valid,
      tailPtr(i),
      Mux(lastLastCycleMisprediction,
        tailPtr(0) + i.U,
        tailPtr(i) + numEnq)
    )
  }

  // update valid counter and allowEnqueue reg
  validCounter := Mux(io.redirect.valid,
    validCounter,
    Mux(lastLastCycleMisprediction,
      currentValidCounter,
      validCounter + numEnq - numDeq)
  )
  allowEnqueue := Mux(currentValidCounter > (size - enqnum).U, false.B, numEnq <= (size - enqnum).U - currentValidCounter)

  /**
   * Part 3: set output and input
   */
  // TODO: remove this when replay moves to rob
  dataModule.io.raddr := VecInit(nextHeadPtr.map(_.value))
  for (i <- 0 until deqnum) {
    io.deq(i).bits := dataModule.io.rdata(i)
    io.deq(i).bits.robIdx := robIdxEntries(headPtr(i).value)
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

  // XSError(isAfter(headPtr(0), tailPtr(0)), p"assert greaterOrEqualThan(tailPtr: ${tailPtr(0)}, headPtr: ${headPtr(0)}) failed\n")
  QueuePerf(size, PopCount(stateEntries.map(_ =/= s_invalid)), !canEnqueue)
  io.dqFull := !canEnqueue
  XSPerfAccumulate("in", numEnq)
  XSPerfAccumulate("out", PopCount(io.deq.map(_.fire())))
  XSPerfAccumulate("out_try", PopCount(io.deq.map(_.valid)))
  val fake_block = currentValidCounter <= (size - enqnum).U && !canEnqueue
  XSPerfAccumulate("fake_block", fake_block)

  val perfEvents = Seq(
    ("dispatchq_in        ", numEnq),
    ("dispatchq_out       ", PopCount(io.deq.map(_.fire()))),
    ("dispatchq_out_try   ", PopCount(io.deq.map(_.valid))),
    ("dispatchq_fake_block", fake_block),
    ("dispatchq_1_4_valid ", (PopCount(stateEntries.map(_ =/= s_invalid)) < (size.U / 4.U))),
    ("dispatchq_2_4_valid ", (PopCount(stateEntries.map(_ =/= s_invalid)) > (size.U / 4.U)) & (PopCount(stateEntries.map(_ =/= s_invalid)) <= (size.U / 2.U))),
    ("dispatchq_3_4_valid ", (PopCount(stateEntries.map(_ =/= s_invalid)) > (size.U / 2.U)) & (PopCount(stateEntries.map(_ =/= s_invalid)) <= (size.U * 3.U / 4.U))),
    ("dispatchq_4_4_valid ", (PopCount(stateEntries.map(_ =/= s_invalid)) > (size.U * 3.U / 4.U))),
  )
  generatePerfEvent()
}
