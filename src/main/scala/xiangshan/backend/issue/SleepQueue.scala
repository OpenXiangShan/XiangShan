package xiangshan.backend.issue

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.backend.decode.{ImmUnion, Imm_U}
import xiangshan.backend.exu.{Exu, ExuConfig}
import xiangshan.backend.roq.RoqPtr
import xiangshan.mem.{SqPtr, StoreDataBundle}

import scala.math.max

// Memory inst sleep queue
//
// After being issued from reservation station, mem instructions may need
// to be re-issue from reservation station. The reasons include:
// 1. TLB miss
// 2. L1 dcache mshr full, "cache replay"
// 3. memory violation detected by forwarding logic
// Re-issue requires inst to stay in reservation station until inst writeback
// to common data bus.
//
// All data field for these (addr gen) instructions are ready, these instructions
// do not need to listen to common data bus result. Thus we use a light weight 
// sleep queue to store these issued but not writebacked (completed) instructions.
//
// Note that SleepQueue is for memory pipeline only. Other pipelines do not require
// insts to "feedback" and "sleep".
class SleepQueue
(
  myName : String,
  sleepQueueSize : Int,
)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val enq = Flipped(DecoupledIO(new ExuInput))
    val deq = DecoupledIO(new ExuInput)
    val redirect = Flipped(ValidIO(new Redirect))
    val flush = Input(Bool())

    val memfeedback = Flipped(ValidIO(new RSFeedback))
    val deqIdx = Output(UInt(log2Up(sleepQueueSize).W))
    val enqIdx = Output(UInt(log2Up(sleepQueueSize).W))
    val full = Output(Bool())
    val isFirstIssue = Output(Bool()) // NOTE: just use for tlb perf cnt
  })

  def widthMap[T <: Data](f: Int => T) = VecInit((0 until sleepQueueSize).map(f))

  /* state machine: the same as original reservation station
   * s_idle     : empty slot, init state, set when deq
   * s_valid    : ready to be secleted
   * s_wait     : wait for feedback
   * s_replay   : replay after some particular cycle
   */
  val s_idle :: s_valid :: s_wait :: s_replay :: Nil = Enum(4)
  val data = Reg(Vec(sleepQueueSize, new ExuInput))
  val state = RegInit(VecInit(Seq.fill(sleepQueueSize)(s_idle)))
  val deqSelectVec = widthMap(i => state(i) === s_valid)
  val enqSelectVec = widthMap(i => state(i) === s_idle)
  // TODO: opt timing
  io.deqIdx := PriorityEncoder(deqSelectVec.asUInt)
  io.enqIdx := PriorityEncoder(enqSelectVec.asUInt)
  io.full := widthMap(i => state(i) =/= s_idle).asUInt.andR // TODO: opt timing
  val isValid = widthMap(i => state(i) =/= s_idle)

  val replayDelay = VecInit(Seq(1, 1, 1, 5).map(_.U(5.W)))
  val countQueue    = RegInit(VecInit(Seq.fill(sleepQueueSize)(0.U(replayDelay(3).getWidth.W))))
  val cntCountQueue = RegInit(VecInit(Seq.fill(sleepQueueSize)(0.U(2.W))))

  // enq
  when(io.enq.fire()) {
    data(io.enqIdx) := io.enq.bits
    state(io.enqIdx) := s_wait
  }
  io.enq.ready := !io.full

  // state update / wakeup
  val flushState = io.memfeedback.bits.flushState
  (0 until sleepQueueSize).map(i => {
    when (state(i) === s_replay) {
      countQueue(i) := countQueue(i) - 1.U
      when (countQueue(i) === 0.U && !flushState) {
        cntCountQueue(i) := Mux(cntCountQueue(i)===3.U, cntCountQueue(i), cntCountQueue(i) + 1.U)
      }
      when (flushState || countQueue(i) === 0.U) {
        state(i) := s_valid
      }
    }
  })

  // deq
  when(io.deq.fire()) {
    state(io.deqIdx) := s_wait
  }
  io.deq.valid := widthMap(i => state(i) === s_valid).asUInt.orR
  io.deq.bits := data(io.deqIdx)

  // feedback
  when (io.memfeedback.valid) {
    // TODO: fix io.memfeedback.bits.feedbackIdx
    val index = io.memfeedback.bits.feedbackIdx
    when (state(index) === s_wait) {
      state(index) := Mux(io.memfeedback.bits.hit, s_idle, s_replay)
    }
    when (!io.memfeedback.bits.hit) {
      countQueue(index) := replayDelay(cntCountQueue(index))
    }
    assert(state(index) === s_wait, "mem feedback but sleep queue dont wait for it")
  }

  // redirect & flush
  // val roqIdx = widthMap(i => data(i).cf.roqIdx)
  (0 until sleepQueueSize).map(i => {
    when(data(i).uop.roqIdx.needFlush(io.redirect, io.flush) && isValid(i)) {
      state(i) := s_idle
    }
  })

  // perf counters
  io.isFirstIssue := DontCare // TODO: fixme
  // TODO

}