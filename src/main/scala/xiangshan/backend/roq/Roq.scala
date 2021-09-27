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

package xiangshan.backend.roq

import chipsalliance.rocketchip.config.Parameters
import chisel3.ExcitingUtils._
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.frontend.FtqPtr
import difftest._

class RoqPtr(implicit p: Parameters) extends CircularQueuePtr[RoqPtr](
  p => p(XSCoreParamsKey).RoqSize
) with HasCircularQueuePtrHelper {

  def needFlush(redirect: Valid[Redirect], flush: Bool): Bool = {
    val flushItself = redirect.bits.flushItself() && this === redirect.bits.roqIdx
    flush || (redirect.valid && (flushItself || isAfter(this, redirect.bits.roqIdx)))
  }

  override def cloneType = (new RoqPtr).asInstanceOf[this.type]
}

object RoqPtr {
  def apply(f: Bool, v: UInt)(implicit p: Parameters): RoqPtr = {
    val ptr = Wire(new RoqPtr)
    ptr.flag := f
    ptr.value := v
    ptr
  }
}

class RoqCSRIO(implicit p: Parameters) extends XSBundle {
  val intrBitSet = Input(Bool())
  val trapTarget = Input(UInt(VAddrBits.W))
  val isXRet = Input(Bool())

  val fflags = Output(Valid(UInt(5.W)))
  val dirty_fs = Output(Bool())
  val perfinfo = new Bundle {
    val retiredInstr = Output(UInt(3.W))
  }
}

class RoqLsqIO(implicit p: Parameters) extends XSBundle {
  val lcommit = Output(UInt(3.W))
  val scommit = Output(UInt(3.W))
  val pendingld = Output(Bool())
  val pendingst = Output(Bool())
  val commit = Output(Bool())
  val storeDataRoqWb = Input(Vec(StorePipelineWidth, Valid(new RoqPtr)))
}

class RoqEnqIO(implicit p: Parameters) extends XSBundle {
  val canAccept = Output(Bool())
  val isEmpty = Output(Bool())
  // valid vector, for roqIdx gen and walk
  val needAlloc = Vec(RenameWidth, Input(Bool()))
  val req = Vec(RenameWidth, Flipped(ValidIO(new MicroOp)))
  val resp = Vec(RenameWidth, Output(new RoqPtr))
}

class RoqDispatchData(implicit p: Parameters) extends RoqCommitInfo {
  val crossPageIPFFix = Bool()
}

class RoqDeqPtrWrapper(implicit p: Parameters) extends XSModule with HasCircularQueuePtrHelper {
  val io = IO(new Bundle {
    // for commits/flush
    val state = Input(UInt(2.W))
    val deq_v = Vec(CommitWidth, Input(Bool()))
    val deq_w = Vec(CommitWidth, Input(Bool()))
    val exception_state = Flipped(ValidIO(new RoqExceptionInfo))
    // for flush: when exception occurs, reset deqPtrs to range(0, CommitWidth)
    val intrBitSetReg = Input(Bool())
    val hasNoSpecExec = Input(Bool())
    val commitType = Input(CommitType())
    val misPredBlock = Input(Bool())
    val isReplaying = Input(Bool())
    // output: the CommitWidth deqPtr
    val out = Vec(CommitWidth, Output(new RoqPtr))
    val next_out = Vec(CommitWidth, Output(new RoqPtr))
  })

  val deqPtrVec = RegInit(VecInit((0 until CommitWidth).map(_.U.asTypeOf(new RoqPtr))))

  // for exceptions (flushPipe included) and interrupts:
  // only consider the first instruction
  val intrEnable = io.intrBitSetReg && !io.hasNoSpecExec && !CommitType.isLoadStore(io.commitType)
  val exceptionEnable = io.deq_w(0) && io.exception_state.valid && io.exception_state.bits.roqIdx === deqPtrVec(0)
  val redirectOutValid = io.state === 0.U && io.deq_v(0) && (intrEnable || exceptionEnable)

  // for normal commits: only to consider when there're no exceptions
  // we don't need to consider whether the first instruction has exceptions since it wil trigger exceptions.
  val commit_exception = io.exception_state.valid && !isAfter(io.exception_state.bits.roqIdx, deqPtrVec.last)
  val canCommit = VecInit((0 until CommitWidth).map(i => io.deq_v(i) && io.deq_w(i) && !io.misPredBlock && !io.isReplaying))
  val normalCommitCnt = PriorityEncoder(canCommit.map(c => !c) :+ true.B)
  // when io.intrBitSetReg or there're possible exceptions in these instructions, only one instruction is allowed to commit
  val allowOnlyOne = commit_exception || io.intrBitSetReg
  val commitCnt = Mux(allowOnlyOne, canCommit(0), normalCommitCnt)

  val resetDeqPtrVec = VecInit((0 until CommitWidth).map(_.U.asTypeOf(new RoqPtr)))
  val commitDeqPtrVec = VecInit(deqPtrVec.map(_ + commitCnt))
  val deqPtrVec_next = Mux(redirectOutValid, resetDeqPtrVec, Mux(io.state === 0.U, commitDeqPtrVec, deqPtrVec))

  deqPtrVec := deqPtrVec_next

  io.next_out := deqPtrVec_next
  io.out      := deqPtrVec

  when (io.state === 0.U) {
    XSInfo(io.state === 0.U && commitCnt > 0.U, "retired %d insts\n", commitCnt)
  }

}

class RoqEnqPtrWrapper(implicit p: Parameters) extends XSModule with HasCircularQueuePtrHelper {
  val io = IO(new Bundle {
    // for exceptions and interrupts
    val state = Input(UInt(2.W))
    val deq_v = Input(Bool())
    val deq_w = Input(Bool())
    val deqPtr = Input(new RoqPtr)
    val exception_state = Flipped(ValidIO(new RoqExceptionInfo))
    val intrBitSetReg = Input(Bool())
    val hasNoSpecExec = Input(Bool())
    val commitType = Input(CommitType())
    // for input redirect
    val redirect = Input(Valid(new Redirect))
    // for enqueue
    val allowEnqueue = Input(Bool())
    val hasBlockBackward = Input(Bool())
    val enq = Vec(RenameWidth, Input(Bool()))
    val out = Output(new RoqPtr)
  })

  val enqPtr = RegInit(0.U.asTypeOf(new RoqPtr))

  // for exceptions (flushPipe included) and interrupts:
  // only consider the first instruction
  val intrEnable = io.intrBitSetReg && !io.hasNoSpecExec && !CommitType.isLoadStore(io.commitType)
  val exceptionEnable = io.deq_w(0) && io.exception_state.valid && io.exception_state.bits.roqIdx === io.deqPtr
  val redirectOutValid = io.state === 0.U && io.deq_v && (intrEnable || exceptionEnable)

  // enqueue
  val canAccept = io.allowEnqueue && !io.hasBlockBackward
  val dispatchNum = Mux(canAccept && !RegNext(redirectOutValid), PopCount(io.enq), 0.U)

  when (redirectOutValid) {
    enqPtr := 0.U.asTypeOf(new RoqPtr)
  }.elsewhen (io.redirect.valid) {
    enqPtr := io.redirect.bits.roqIdx + Mux(io.redirect.bits.flushItself(), 0.U, 1.U)
  }.otherwise {
    enqPtr := enqPtr + dispatchNum
  }

  io.out := enqPtr

}

class RoqExceptionInfo(implicit p: Parameters) extends XSBundle {
  // val valid = Bool()
  val roqIdx = new RoqPtr
  val exceptionVec = ExceptionVec()
  val flushPipe = Bool()
  val replayInst = Bool() // redirect to that inst itself
  val singleStep = Bool()

  def has_exception = exceptionVec.asUInt.orR || flushPipe || singleStep || replayInst
  // only exceptions are allowed to writeback when enqueue
  def can_writeback = exceptionVec.asUInt.orR || singleStep
}

class ExceptionGen(implicit p: Parameters) extends XSModule with HasCircularQueuePtrHelper {
  val io = IO(new Bundle {
    val redirect = Input(Valid(new Redirect))
    val flush = Input(Bool())
    val enq = Vec(RenameWidth, Flipped(ValidIO(new RoqExceptionInfo)))
    val wb = Vec(5, Flipped(ValidIO(new RoqExceptionInfo)))
    val out = ValidIO(new RoqExceptionInfo)
    val state = ValidIO(new RoqExceptionInfo)
  })

  val current = Reg(Valid(new RoqExceptionInfo))

  // orR the exceptionVec
  val lastCycleFlush = RegNext(io.flush)
  val in_enq_valid = VecInit(io.enq.map(e => e.valid && e.bits.has_exception && !lastCycleFlush))
  val in_wb_valid = io.wb.map(w => w.valid && w.bits.has_exception && !lastCycleFlush)

  // s0: compare wb(1),wb(2) and wb(3),wb(4)
  val wb_valid = in_wb_valid.zip(io.wb.map(_.bits)).map{ case (v, bits) => v && !bits.roqIdx.needFlush(io.redirect, io.flush) }
  val csr_wb_bits = io.wb(0).bits
  val load_wb_bits = Mux(!in_wb_valid(2) || in_wb_valid(1) && isAfter(io.wb(2).bits.roqIdx, io.wb(1).bits.roqIdx), io.wb(1).bits, io.wb(2).bits)
  val store_wb_bits = Mux(!in_wb_valid(4) || in_wb_valid(3) && isAfter(io.wb(4).bits.roqIdx, io.wb(3).bits.roqIdx), io.wb(3).bits, io.wb(4).bits)
  val s0_out_valid = RegNext(VecInit(Seq(wb_valid(0), wb_valid(1) || wb_valid(2), wb_valid(3) || wb_valid(4))))
  val s0_out_bits = RegNext(VecInit(Seq(csr_wb_bits, load_wb_bits, store_wb_bits)))

  // s1: compare last four and current flush
  val s1_valid = VecInit(s0_out_valid.zip(s0_out_bits).map{ case (v, b) => v && !b.roqIdx.needFlush(io.redirect, io.flush) })
  val compare_01_valid = s0_out_valid(0) || s0_out_valid(1)
  val compare_01_bits = Mux(!s0_out_valid(0) || s0_out_valid(1) && isAfter(s0_out_bits(0).roqIdx, s0_out_bits(1).roqIdx), s0_out_bits(1), s0_out_bits(0))
  val compare_bits = Mux(!s0_out_valid(2) || compare_01_valid && isAfter(s0_out_bits(2).roqIdx, compare_01_bits.roqIdx), compare_01_bits, s0_out_bits(2))
  val s1_out_bits = RegNext(compare_bits)
  val s1_out_valid = RegNext(s1_valid.asUInt.orR)

  val enq_valid = RegNext(in_enq_valid.asUInt.orR && !io.redirect.valid && !io.flush)
  val enq_bits = RegNext(ParallelPriorityMux(in_enq_valid, io.enq.map(_.bits)))

  // s2: compare the input exception with the current one
  // priorities:
  // (1) system reset
  // (2) current is valid: flush, remain, merge, update
  // (3) current is not valid: s1 or enq
  val current_flush = current.bits.roqIdx.needFlush(io.redirect, io.flush)
  val s1_flush = s1_out_bits.roqIdx.needFlush(io.redirect, io.flush)
  when (reset.asBool) {
    current.valid := false.B
  }.elsewhen (current.valid) {
    when (current_flush) {
      current.valid := Mux(s1_flush, false.B, s1_out_valid)
    }
    when (s1_out_valid && !s1_flush) {
      when (isAfter(current.bits.roqIdx, s1_out_bits.roqIdx)) {
        current.bits := s1_out_bits
      }.elsewhen (current.bits.roqIdx === s1_out_bits.roqIdx) {
        current.bits.exceptionVec := (s1_out_bits.exceptionVec.asUInt | current.bits.exceptionVec.asUInt).asTypeOf(ExceptionVec())
        current.bits.flushPipe := s1_out_bits.flushPipe || current.bits.flushPipe
        current.bits.replayInst := s1_out_bits.replayInst || current.bits.replayInst
        current.bits.singleStep := s1_out_bits.singleStep || current.bits.singleStep
      }
    }
  }.elsewhen (s1_out_valid && !s1_flush) {
    current.valid := true.B
    current.bits := s1_out_bits
  }.elsewhen (enq_valid && !(io.redirect.valid || io.flush)) {
    current.valid := true.B
    current.bits := enq_bits
  }

  io.out.valid := s1_out_valid || enq_valid && enq_bits.can_writeback
  io.out.bits := Mux(s1_out_valid, s1_out_bits, enq_bits)
  io.state := current

}

class RoqFlushInfo(implicit p: Parameters) extends XSBundle {
  val ftqIdx = new FtqPtr
  val ftqOffset = UInt(log2Up(PredictWidth).W)
  val replayInst = Bool()
}

class Roq(numWbPorts: Int)(implicit p: Parameters) extends XSModule with HasCircularQueuePtrHelper {
  val io = IO(new Bundle() {
    val redirect = Input(Valid(new Redirect))
    val enq = new RoqEnqIO
    val flushOut = ValidIO(new RoqFlushInfo)
    val exception = ValidIO(new ExceptionInfo)
    // exu + brq
    val exeWbResults = Vec(numWbPorts, Flipped(ValidIO(new ExuOutput)))
    val commits = new RoqCommitIO
    val lsq = new RoqLsqIO
    val bcommit = Output(UInt(BrTagWidth.W))
    val roqDeqPtr = Output(new RoqPtr)
    val csr = new RoqCSRIO
    val roqFull = Output(Bool())
  })

  println("Roq: size:" + RoqSize + " wbports:" + numWbPorts  + " commitwidth:" + CommitWidth)

  // instvalid field
  // val valid = RegInit(VecInit(List.fill(RoqSize)(false.B)))
  val valid = Mem(RoqSize, Bool())
  // writeback status
  // val writebacked = Reg(Vec(RoqSize, Bool()))
  val writebacked = Mem(RoqSize, Bool())
  val store_data_writebacked = Mem(RoqSize, Bool())
  // data for redirect, exception, etc.
  // val flagBkup = RegInit(VecInit(List.fill(RoqSize)(false.B)))
  val flagBkup = Mem(RoqSize, Bool())
  // record move elimination info for each instruction
  val eliminatedMove = Mem(RoqSize, Bool())

  // data for debug
  // Warn: debug_* prefix should not exist in generated verilog.
  val debug_microOp = Mem(RoqSize, new MicroOp)
  val debug_exuData = Reg(Vec(RoqSize, UInt(XLEN.W)))//for debug
  val debug_exuDebug = Reg(Vec(RoqSize, new DebugBundle))//for debug

  // pointers
  // For enqueue ptr, we don't duplicate it since only enqueue needs it.
  val enqPtr = Wire(new RoqPtr)
  val deqPtrVec = Wire(Vec(CommitWidth, new RoqPtr))

  val walkPtrVec = Reg(Vec(CommitWidth, new RoqPtr))
  val validCounter = RegInit(0.U(log2Ceil(RoqSize + 1).W))
  val allowEnqueue = RegInit(true.B)

  val enqPtrVec = VecInit((0 until RenameWidth).map(i => enqPtr + PopCount(io.enq.needAlloc.take(i))))
  val deqPtr = deqPtrVec(0)
  val walkPtr = walkPtrVec(0)

  val isEmpty = enqPtr === deqPtr
  val isReplaying = io.redirect.valid && RedirectLevel.flushItself(io.redirect.bits.level)

  /**
    * states of Roq
    */
  val s_idle :: s_walk :: s_extrawalk :: Nil = Enum(3)
  val state = RegInit(s_idle)

  /**
    * Data Modules
    *
    * CommitDataModule: data from dispatch
    * (1) read: commits/walk/exception
    * (2) write: enqueue
    *
    * WritebackData: data from writeback
    * (1) read: commits/walk/exception
    * (2) write: write back from exe units
    */
  val dispatchData = Module(new SyncDataModuleTemplate(new RoqDispatchData, RoqSize, CommitWidth, RenameWidth))
  val dispatchDataRead = dispatchData.io.rdata

  val exceptionGen = Module(new ExceptionGen)
  val exceptionDataRead = exceptionGen.io.state
  val fflagsDataRead = Wire(Vec(CommitWidth, UInt(5.W)))

  io.roqDeqPtr := deqPtr

  /**
    * Enqueue (from dispatch)
    */
  // special cases
  val hasBlockBackward = RegInit(false.B)
  val hasNoSpecExec = RegInit(false.B)
  // When blockBackward instruction leaves Roq (commit or walk), hasBlockBackward should be set to false.B
  // To reduce registers usage, for hasBlockBackward cases, we allow enqueue after ROB is empty.
  when (isEmpty) { hasBlockBackward:= false.B }
  // When any instruction commits, hasNoSpecExec should be set to false.B
  when (io.commits.valid.asUInt.orR  && state =/= s_extrawalk) { hasNoSpecExec:= false.B }

  io.enq.canAccept := allowEnqueue && !hasBlockBackward
  io.enq.resp      := enqPtrVec
  val canEnqueue = VecInit(io.enq.req.map(_.valid && io.enq.canAccept))
  val timer = GTimer()
  for (i <- 0 until RenameWidth) {
    // we don't check whether io.redirect is valid here since redirect has higher priority
    when (canEnqueue(i)) {
      // store uop in data module and debug_microOp Vec
      debug_microOp(enqPtrVec(i).value) := io.enq.req(i).bits
      debug_microOp(enqPtrVec(i).value).debugInfo.dispatchTime := timer
      debug_microOp(enqPtrVec(i).value).debugInfo.enqRsTime := timer
      debug_microOp(enqPtrVec(i).value).debugInfo.selectTime := timer
      debug_microOp(enqPtrVec(i).value).debugInfo.issueTime := timer
      debug_microOp(enqPtrVec(i).value).debugInfo.writebackTime := timer
      when (io.enq.req(i).bits.ctrl.blockBackward) {
        hasBlockBackward := true.B
      }
      when (io.enq.req(i).bits.ctrl.noSpecExec) {
        hasNoSpecExec := true.B
      }
    }
  }
  val dispatchNum = Mux(io.enq.canAccept, PopCount(Cat(io.enq.req.map(_.valid))), 0.U)
  io.enq.isEmpty   := RegNext(isEmpty && dispatchNum === 0.U)

  // debug info for enqueue (dispatch)
  XSDebug(p"(ready, valid): ${io.enq.canAccept}, ${Binary(Cat(io.enq.req.map(_.valid)))}\n")
  XSInfo(dispatchNum =/= 0.U, p"dispatched $dispatchNum insts\n")


  /**
    * Writeback (from execution units)
    */
  for (i <- 0 until numWbPorts) {
    when (io.exeWbResults(i).valid) {
      val wbIdx = io.exeWbResults(i).bits.uop.roqIdx.value
      debug_microOp(wbIdx).cf.exceptionVec := io.exeWbResults(i).bits.uop.cf.exceptionVec
      debug_microOp(wbIdx).ctrl.flushPipe := io.exeWbResults(i).bits.uop.ctrl.flushPipe
      debug_microOp(wbIdx).ctrl.replayInst := io.exeWbResults(i).bits.uop.ctrl.replayInst
      debug_microOp(wbIdx).diffTestDebugLrScValid := io.exeWbResults(i).bits.uop.diffTestDebugLrScValid
      debug_exuData(wbIdx) := io.exeWbResults(i).bits.data
      debug_exuDebug(wbIdx) := io.exeWbResults(i).bits.debug
      debug_microOp(wbIdx).debugInfo.enqRsTime := io.exeWbResults(i).bits.uop.debugInfo.enqRsTime
      debug_microOp(wbIdx).debugInfo.selectTime := io.exeWbResults(i).bits.uop.debugInfo.selectTime
      debug_microOp(wbIdx).debugInfo.issueTime := io.exeWbResults(i).bits.uop.debugInfo.issueTime
      debug_microOp(wbIdx).debugInfo.writebackTime := io.exeWbResults(i).bits.uop.debugInfo.writebackTime

      val debug_Uop = debug_microOp(wbIdx)
      XSInfo(true.B,
        p"writebacked pc 0x${Hexadecimal(debug_Uop.cf.pc)} wen ${debug_Uop.ctrl.rfWen} " +
        p"data 0x${Hexadecimal(io.exeWbResults(i).bits.data)} ldst ${debug_Uop.ctrl.ldest} pdst ${debug_Uop.pdest} " +
        p"skip ${io.exeWbResults(i).bits.debug.isMMIO} roqIdx: ${io.exeWbResults(i).bits.uop.roqIdx}\n"
      )
    }
  }
  val writebackNum = PopCount(io.exeWbResults.map(_.valid))
  XSInfo(writebackNum =/= 0.U, "writebacked %d insts\n", writebackNum)


  /**
    * RedirectOut: Interrupt and Exceptions
    */
  val deqDispatchData = dispatchDataRead(0)
  val debug_deqUop = debug_microOp(deqPtr.value)

  // For MMIO instructions, they should not trigger interrupts since they may be sent to lower level before it writes back.
  // However, we cannot determine whether a load/store instruction is MMIO.
  // Thus, we don't allow load/store instructions to trigger an interrupt.
  val intrBitSetReg = RegNext(io.csr.intrBitSet)
  val intrEnable = intrBitSetReg && !hasNoSpecExec && !CommitType.isLoadStore(deqDispatchData.commitType)
  val deqHasExceptionOrFlush = exceptionDataRead.valid && exceptionDataRead.bits.roqIdx === deqPtr
  val deqHasException = deqHasExceptionOrFlush && exceptionDataRead.bits.exceptionVec.asUInt.orR
  val deqHasFlushPipe = deqHasExceptionOrFlush && exceptionDataRead.bits.flushPipe
  val deqHasReplayInst = deqHasExceptionOrFlush && exceptionDataRead.bits.replayInst
  val exceptionEnable = writebacked(deqPtr.value) && deqHasException
  val isFlushPipe = writebacked(deqPtr.value) && (deqHasFlushPipe || deqHasReplayInst)

  io.flushOut.valid := (state === s_idle) && valid(deqPtr.value) && (intrEnable || exceptionEnable || isFlushPipe)
  io.flushOut.bits.ftqIdx := deqDispatchData.ftqIdx
  io.flushOut.bits.ftqOffset := deqDispatchData.ftqOffset
  io.flushOut.bits.replayInst := deqHasReplayInst
  XSPerfAccumulate("interrupt_num", io.flushOut.valid && intrEnable)
  XSPerfAccumulate("exception_num", io.flushOut.valid && exceptionEnable)
  XSPerfAccumulate("flush_pipe_num", io.flushOut.valid && isFlushPipe)
  XSPerfAccumulate("replay_inst_num", io.flushOut.valid && isFlushPipe && deqHasReplayInst)

  val exceptionHappen = (state === s_idle) && valid(deqPtr.value) && (intrEnable || exceptionEnable)
  io.exception.valid := RegNext(exceptionHappen)
  io.exception.bits.uop := RegEnable(debug_deqUop, exceptionHappen)
  io.exception.bits.uop.ctrl.commitType := RegEnable(deqDispatchData.commitType, exceptionHappen)
  io.exception.bits.uop.cf.exceptionVec := RegEnable(exceptionDataRead.bits.exceptionVec, exceptionHappen)
  io.exception.bits.uop.ctrl.singleStep := RegEnable(exceptionDataRead.bits.singleStep, exceptionHappen)
  io.exception.bits.uop.cf.crossPageIPFFix := RegEnable(deqDispatchData.crossPageIPFFix, exceptionHappen)
  io.exception.bits.isInterrupt := RegEnable(intrEnable, exceptionHappen)

  XSDebug(io.flushOut.valid,
    p"generate redirect: pc 0x${Hexadecimal(io.exception.bits.uop.cf.pc)} intr $intrEnable " +
    p"excp $exceptionEnable flushPipe $isFlushPipe " +
    p"Trap_target 0x${Hexadecimal(io.csr.trapTarget)} exceptionVec ${Binary(exceptionDataRead.bits.exceptionVec.asUInt)}\n")


  /**
    * Commits (and walk)
    * They share the same width.
    */
  val walkCounter = Reg(UInt(log2Up(RoqSize).W))
  val shouldWalkVec = VecInit((0 until CommitWidth).map(_.U < walkCounter))
  val walkFinished = walkCounter <= CommitWidth.U

  // extra space is used when roq has no enough space, but mispredict recovery needs such info to walk regmap
  require(RenameWidth <= CommitWidth)
  val extraSpaceForMPR = Reg(Vec(RenameWidth, new RoqDispatchData))
  val usedSpaceForMPR = Reg(Vec(RenameWidth, Bool()))
  when (io.enq.needAlloc.asUInt.orR && io.redirect.valid) {
    usedSpaceForMPR := io.enq.needAlloc
    extraSpaceForMPR := dispatchData.io.wdata
    XSDebug("roq full, switched to s_extrawalk. needExtraSpaceForMPR: %b\n", io.enq.needAlloc.asUInt)
  }

  // wiring to csr
  val (wflags, fpWen) = (0 until CommitWidth).map(i => {
    val v = io.commits.valid(i)
    val info = io.commits.info(i)
    (v & info.wflags, v & info.fpWen)
  }).unzip
  val fflags = Wire(Valid(UInt(5.W)))
  fflags.valid := Mux(io.commits.isWalk, false.B, Cat(wflags).orR())
  fflags.bits := wflags.zip(fflagsDataRead).map({
    case (w, f) => Mux(w, f, 0.U)
  }).reduce(_|_)
  val dirty_fs = Mux(io.commits.isWalk, false.B, Cat(fpWen).orR())

  // when mispredict branches writeback, stop commit in the next 2 cycles
  // TODO: don't check all exu write back
  val misPredWb = Cat(VecInit((0 until numWbPorts).map(i =>
    io.exeWbResults(i).bits.redirect.cfiUpdate.isMisPred && io.exeWbResults(i).bits.redirectValid
  ))).orR()
  val misPredBlockCounter = Reg(UInt(3.W))
  misPredBlockCounter := Mux(misPredWb,
    "b111".U,
    misPredBlockCounter >> 1.U
  )
  val misPredBlock = misPredBlockCounter(0)

  io.commits.isWalk := state =/= s_idle
  val commit_v = Mux(state === s_idle, VecInit(deqPtrVec.map(ptr => valid(ptr.value))), VecInit(walkPtrVec.map(ptr => valid(ptr.value))))
  // store will be commited iff both sta & std have been writebacked  
  val commit_w = VecInit(deqPtrVec.map(ptr => writebacked(ptr.value) && store_data_writebacked(ptr.value)))
  val commit_exception = exceptionDataRead.valid && !isAfter(exceptionDataRead.bits.roqIdx, deqPtrVec.last)
  val commit_block = VecInit((0 until CommitWidth).map(i => !commit_w(i)))
  val allowOnlyOneCommit = commit_exception || intrBitSetReg
  // for instructions that may block others, we don't allow them to commit
  for (i <- 0 until CommitWidth) {
    // defaults: state === s_idle and instructions commit
    // when intrBitSetReg, allow only one instruction to commit at each clock cycle
    val isBlocked = if (i != 0) Cat(commit_block.take(i)).orR || allowOnlyOneCommit else intrEnable || deqHasException || deqHasReplayInst
    io.commits.valid(i) := commit_v(i) && commit_w(i) && !isBlocked && !misPredBlock && !isReplaying
    io.commits.info(i)  := dispatchDataRead(i)

    when (state === s_walk) {
      io.commits.valid(i) := commit_v(i) && shouldWalkVec(i)
    }.elsewhen(state === s_extrawalk) {
      io.commits.valid(i) := (if (i < RenameWidth) usedSpaceForMPR(RenameWidth-i-1) else false.B)
      io.commits.info(i)  := (if (i < RenameWidth) extraSpaceForMPR(RenameWidth-i-1) else DontCare)
    }

    XSInfo(state === s_idle && io.commits.valid(i),
      "retired pc %x wen %d ldest %d pdest %x old_pdest %x data %x fflags: %b\n",
      debug_microOp(deqPtrVec(i).value).cf.pc,
      io.commits.info(i).rfWen,
      io.commits.info(i).ldest,
      io.commits.info(i).pdest,
      io.commits.info(i).old_pdest,
      debug_exuData(deqPtrVec(i).value),
      fflagsDataRead(i)
    )
    XSInfo(state === s_walk && io.commits.valid(i), "walked pc %x wen %d ldst %d data %x\n",
      debug_microOp(walkPtrVec(i).value).cf.pc,
      io.commits.info(i).rfWen,
      io.commits.info(i).ldest,
      debug_exuData(walkPtrVec(i).value)
    )
    XSInfo(state === s_extrawalk && io.commits.valid(i), "use extra space walked wen %d ldst %d\n",
      io.commits.info(i).rfWen,
      io.commits.info(i).ldest
    )
  }
  if (!env.FPGAPlatform) {
    io.commits.info.map(info => dontTouch(info.pc))
  }

  // sync fflags/dirty_fs to csr
  io.csr.fflags := fflags
  io.csr.dirty_fs := dirty_fs

  // commit branch to brq
  val cfiCommitVec = VecInit(io.commits.valid.zip(io.commits.info.map(_.commitType)).map{case(v, t) => v && CommitType.isBranch(t)})
  io.bcommit := Mux(io.commits.isWalk, 0.U, PopCount(cfiCommitVec))

  // commit load/store to lsq
  val ldCommitVec = VecInit((0 until CommitWidth).map(i => io.commits.valid(i) && io.commits.info(i).commitType === CommitType.LOAD))
  val stCommitVec = VecInit((0 until CommitWidth).map(i => io.commits.valid(i) && io.commits.info(i).commitType === CommitType.STORE))
  io.lsq.lcommit := Mux(io.commits.isWalk, 0.U, PopCount(ldCommitVec))
  io.lsq.scommit := Mux(io.commits.isWalk, 0.U, PopCount(stCommitVec))
  io.lsq.pendingld := !io.commits.isWalk && io.commits.info(0).commitType === CommitType.LOAD && valid(deqPtr.value)
  io.lsq.pendingst := !io.commits.isWalk && io.commits.info(0).commitType === CommitType.STORE && valid(deqPtr.value)
  io.lsq.commit := !io.commits.isWalk && io.commits.valid(0)

  /**
    * state changes
    * (1) exceptions: when exception occurs, cancels all and switch to s_idle
    * (2) redirect: switch to s_walk or s_extrawalk (depends on whether there're pending instructions in dispatch1)
    * (3) walk: when walking comes to the end, switch to s_walk
    * (4) s_extrawalk to s_walk
    */
  val state_next = Mux(io.flushOut.valid,
    s_idle,
    Mux(io.redirect.valid,
      Mux(io.enq.needAlloc.asUInt.orR, s_extrawalk, s_walk),
      Mux(state === s_walk && walkFinished,
        s_idle,
        Mux(state === s_extrawalk, s_walk, state)
      )
    )
  )
  state := state_next

  /**
    * pointers and counters
    */
  val deqPtrGenModule = Module(new RoqDeqPtrWrapper)
  deqPtrGenModule.io.state := state
  deqPtrGenModule.io.deq_v := commit_v
  deqPtrGenModule.io.deq_w := commit_w
  deqPtrGenModule.io.exception_state := exceptionDataRead
  deqPtrGenModule.io.intrBitSetReg := intrBitSetReg
  deqPtrGenModule.io.hasNoSpecExec := hasNoSpecExec
  deqPtrGenModule.io.commitType := deqDispatchData.commitType

  deqPtrGenModule.io.misPredBlock := misPredBlock
  deqPtrGenModule.io.isReplaying := isReplaying
  deqPtrVec := deqPtrGenModule.io.out
  val deqPtrVec_next = deqPtrGenModule.io.next_out

  val enqPtrGenModule = Module(new RoqEnqPtrWrapper)
  enqPtrGenModule.io.state := state
  enqPtrGenModule.io.deq_v := commit_v(0)
  enqPtrGenModule.io.deq_w := commit_w(0)
  enqPtrGenModule.io.deqPtr := deqPtr
  enqPtrGenModule.io.exception_state := exceptionDataRead
  enqPtrGenModule.io.intrBitSetReg := intrBitSetReg
  enqPtrGenModule.io.hasNoSpecExec := hasNoSpecExec
  enqPtrGenModule.io.commitType := deqDispatchData.commitType
  enqPtrGenModule.io.redirect := io.redirect
  enqPtrGenModule.io.allowEnqueue := allowEnqueue
  enqPtrGenModule.io.hasBlockBackward := hasBlockBackward
  enqPtrGenModule.io.enq := VecInit(io.enq.req.map(_.valid))
  enqPtr := enqPtrGenModule.io.out

  val thisCycleWalkCount = Mux(walkFinished, walkCounter, CommitWidth.U)
  // next walkPtrVec:
  // (1) redirect occurs: update according to state
  // (2) walk: move backwards
  val walkPtrVec_next = Mux(io.redirect.valid && state =/= s_extrawalk,
    Mux(state === s_walk,
      VecInit(walkPtrVec.map(_ - thisCycleWalkCount)),
      VecInit((0 until CommitWidth).map(i => enqPtr - (i+1).U))
    ),
    Mux(state === s_walk, VecInit(walkPtrVec.map(_ - CommitWidth.U)), walkPtrVec)
  )
  walkPtrVec := walkPtrVec_next

  val lastCycleRedirect = RegNext(io.redirect.valid)
  val trueValidCounter = Mux(lastCycleRedirect, distanceBetween(enqPtr, deqPtr), validCounter)
  val commitCnt = PopCount(io.commits.valid)
  validCounter := Mux(io.flushOut.valid,
    0.U,
    Mux(state === s_idle,
      (validCounter - commitCnt) + dispatchNum,
      trueValidCounter
    )
  )

  allowEnqueue := Mux(io.flushOut.valid,
    true.B,
    Mux(state === s_idle,
      validCounter + dispatchNum <= (RoqSize - RenameWidth).U,
      trueValidCounter <= (RoqSize - RenameWidth).U
    )
  )

  val currentWalkPtr = Mux(state === s_walk || state === s_extrawalk, walkPtr, enqPtr - 1.U)
  val redirectWalkDistance = distanceBetween(currentWalkPtr, io.redirect.bits.roqIdx)
  when (io.redirect.valid) {
    walkCounter := Mux(state === s_walk,
      redirectWalkDistance + io.redirect.bits.flushItself() - commitCnt,
      redirectWalkDistance + io.redirect.bits.flushItself()
    )
  }.elsewhen (state === s_walk) {
    walkCounter := walkCounter - commitCnt
    XSInfo(p"rolling back: $enqPtr $deqPtr walk $walkPtr walkcnt $walkCounter\n")
  }


  /**
    * States
    * We put all the stage bits changes here.

    * All events: (1) enqueue (dispatch); (2) writeback; (3) cancel; (4) dequeue (commit);
    * All states: (1) valid; (2) writebacked; (3) flagBkup
    */
  val commitReadAddr = Mux(state === s_idle, VecInit(deqPtrVec.map(_.value)), VecInit(walkPtrVec.map(_.value)))

  // enqueue logic writes 6 valid
  for (i <- 0 until RenameWidth) {
    when (canEnqueue(i) && !io.redirect.valid && !RegNext(io.flushOut.valid)) {
      valid(enqPtrVec(i).value) := true.B
    }
  }
  // dequeue/walk logic writes 6 valid, dequeue and walk will not happen at the same time
  for (i <- 0 until CommitWidth) {
    when (io.commits.valid(i) && state =/= s_extrawalk) {
      valid(commitReadAddr(i)) := false.B
    }
  }
  // reset: when exception, reset all valid to false
  when (io.flushOut.valid) {
    for (i <- 0 until RoqSize) {
      valid(i) := false.B
    }
  }
  when (reset.asBool) {
    for (i <- 0 until RoqSize) {
      valid(i) := false.B
    }
  }

  // status field: writebacked
  // enqueue logic set 6 writebacked to false
  for (i <- 0 until RenameWidth) {
    when (canEnqueue(i)) {
      if (EnableIntMoveElim) {
        eliminatedMove(enqPtrVec(i).value) := io.enq.req(i).bits.eliminatedMove
        writebacked(enqPtrVec(i).value) := io.enq.req(i).bits.eliminatedMove && !io.enq.req(i).bits.cf.exceptionVec.asUInt().orR
      } else {
        writebacked(enqPtrVec(i).value) := false.B
      }
      val isStu = io.enq.req(i).bits.ctrl.fuType === FuType.stu
      store_data_writebacked(enqPtrVec(i).value) := !isStu
    }
  }
  when (exceptionGen.io.out.valid) {
    val wbIdx = exceptionGen.io.out.bits.roqIdx.value
    writebacked(wbIdx) := true.B
    store_data_writebacked(wbIdx) := true.B
  }
  // writeback logic set numWbPorts writebacked to true
  for (i <- 0 until numWbPorts) {
    when (io.exeWbResults(i).valid) {
      val wbIdx = io.exeWbResults(i).bits.uop.roqIdx.value
      val block_wb = 
        selectAll(io.exeWbResults(i).bits.uop.cf.exceptionVec, false, true).asUInt.orR || 
        io.exeWbResults(i).bits.uop.ctrl.flushPipe ||
        io.exeWbResults(i).bits.uop.ctrl.replayInst
      writebacked(wbIdx) := !block_wb
    }
  }
  // store data writeback logic mark store as data_writebacked
  for (i <- 0 until StorePipelineWidth) {
    when(io.lsq.storeDataRoqWb(i).valid) {
      store_data_writebacked(io.lsq.storeDataRoqWb(i).bits.value) := true.B
    }
  }

  // flagBkup
  // enqueue logic set 6 flagBkup at most
  for (i <- 0 until RenameWidth) {
    when (canEnqueue(i)) {
      flagBkup(enqPtrVec(i).value) := enqPtrVec(i).flag
    }
  }


  /**
    * read and write of data modules
    */
  val commitReadAddr_next = Mux(state_next === s_idle,
    VecInit(deqPtrVec_next.map(_.value)),
    VecInit(walkPtrVec_next.map(_.value))
  )
  dispatchData.io.wen := canEnqueue
  dispatchData.io.waddr := enqPtrVec.map(_.value)
  dispatchData.io.wdata.zip(io.enq.req.map(_.bits)).foreach{ case (wdata, req) =>
    wdata.ldest := req.ctrl.ldest
    wdata.rfWen := req.ctrl.rfWen
    wdata.fpWen := req.ctrl.fpWen
    wdata.wflags := req.ctrl.fpu.wflags
    wdata.commitType := req.ctrl.commitType
    if (EnableIntMoveElim) {
      wdata.eliminatedMove := req.eliminatedMove
    } else {
      wdata.eliminatedMove := DontCare
    }
    wdata.pdest := req.pdest
    wdata.old_pdest := req.old_pdest
    wdata.ftqIdx := req.cf.ftqPtr
    wdata.ftqOffset := req.cf.ftqOffset
    wdata.pc := req.cf.pc
    wdata.crossPageIPFFix := req.cf.crossPageIPFFix
    wdata.isFused := req.ctrl.isFused
    // wdata.exceptionVec := req.cf.exceptionVec
  }
  dispatchData.io.raddr := commitReadAddr_next

  exceptionGen.io.redirect <> io.redirect
  exceptionGen.io.flush := io.flushOut.valid
  for (i <- 0 until RenameWidth) {
    exceptionGen.io.enq(i).valid := canEnqueue(i)
    exceptionGen.io.enq(i).bits.roqIdx := io.enq.req(i).bits.roqIdx
    exceptionGen.io.enq(i).bits.exceptionVec := selectFrontend(io.enq.req(i).bits.cf.exceptionVec, false, true)
    exceptionGen.io.enq(i).bits.flushPipe := io.enq.req(i).bits.ctrl.flushPipe
    exceptionGen.io.enq(i).bits.replayInst := io.enq.req(i).bits.ctrl.replayInst
    assert(exceptionGen.io.enq(i).bits.replayInst === false.B)
    exceptionGen.io.enq(i).bits.singleStep := io.enq.req(i).bits.ctrl.singleStep
  }

  // TODO: don't hard code these idxes
  val numIntWbPorts = exuParameters.AluCnt + exuParameters.LduCnt + exuParameters.MduCnt
  // CSR is after Alu and Load
  def csr_wb_idx = exuParameters.AluCnt + exuParameters.LduCnt
  def atomic_wb_idx = exuParameters.AluCnt // first port for load
  def load_wb_idxes = Seq(exuParameters.AluCnt + 1) // second port for load
  def store_wb_idxes = io.exeWbResults.indices.takeRight(2)
  val all_exception_possibilities = Seq(csr_wb_idx, atomic_wb_idx) ++ load_wb_idxes ++ store_wb_idxes
  all_exception_possibilities.zipWithIndex.map{ case (p, i) => connect_exception(i, p) }
  def connect_exception(index: Int, wb_index: Int) = {
    exceptionGen.io.wb(index).valid             := io.exeWbResults(wb_index).valid
    // A temporary fix for float load writeback
    // TODO: let int/fp load use the same two wb ports
    if (wb_index == atomic_wb_idx || load_wb_idxes.contains(wb_index)) {
      when (io.exeWbResults(wb_index - exuParameters.AluCnt + numIntWbPorts + exuParameters.FmacCnt).valid) {
        exceptionGen.io.wb(index).valid := true.B
      }
    }
    exceptionGen.io.wb(index).bits.roqIdx       := io.exeWbResults(wb_index).bits.uop.roqIdx
    val selectFunc = if (wb_index == csr_wb_idx) selectCSR _
    else if (wb_index == atomic_wb_idx) selectAtomics _
    else if (load_wb_idxes.contains(wb_index)) selectLoad _
    else {
      assert(store_wb_idxes.contains(wb_index))
      selectStore _
    }
    exceptionGen.io.wb(index).bits.exceptionVec := selectFunc(io.exeWbResults(wb_index).bits.uop.cf.exceptionVec, false, true)
    exceptionGen.io.wb(index).bits.flushPipe    := io.exeWbResults(wb_index).bits.uop.ctrl.flushPipe
    exceptionGen.io.wb(index).bits.replayInst   := io.exeWbResults(wb_index).bits.uop.ctrl.replayInst
    exceptionGen.io.wb(index).bits.singleStep   := false.B
  }

  // 4 fmac + 2 fmisc + 1 i2f
  val fmacWb = (0 until exuParameters.FmacCnt).map(_ + numIntWbPorts)
  val fmiscWb = (0 until exuParameters.FmiscCnt).map(_ + numIntWbPorts + exuParameters.FmacCnt + 2)
  val i2fWb = Seq(numIntWbPorts - 1) // last port in int
  val fflags_wb = io.exeWbResults.zipWithIndex.filter(w => {
    (fmacWb ++ fmiscWb ++ i2fWb).contains(w._2)
  }).map(_._1)
  val fflagsDataModule = Module(new SyncDataModuleTemplate(
    UInt(5.W), RoqSize, CommitWidth, fflags_wb.size)
  )
  for(i <- fflags_wb.indices){
    fflagsDataModule.io.wen  (i) := fflags_wb(i).valid
    fflagsDataModule.io.waddr(i) := fflags_wb(i).bits.uop.roqIdx.value
    fflagsDataModule.io.wdata(i) := fflags_wb(i).bits.fflags
  }
  fflagsDataModule.io.raddr := VecInit(deqPtrVec_next.map(_.value))
  fflagsDataRead := fflagsDataModule.io.rdata


  val instrCnt = RegInit(0.U(64.W))
  val fuseCommitCnt = PopCount(io.commits.valid.zip(io.commits.info).map{ case (v, i) => v && i.isFused =/= 0.U })
  val trueCommitCnt = commitCnt +& fuseCommitCnt
  val retireCounter = Mux(state === s_idle, trueCommitCnt, 0.U)
  instrCnt := instrCnt + retireCounter
  io.csr.perfinfo.retiredInstr := RegNext(retireCounter)
  io.roqFull := !allowEnqueue

  /**
    * debug info
    */
  XSDebug(p"enqPtr ${enqPtr} deqPtr ${deqPtr}\n")
  XSDebug("")
  for(i <- 0 until RoqSize){
    XSDebug(false, !valid(i), "-")
    XSDebug(false, valid(i) && writebacked(i), "w")
    XSDebug(false, valid(i) && !writebacked(i), "v")
  }
  XSDebug(false, true.B, "\n")

  for(i <- 0 until RoqSize) {
    if(i % 4 == 0) XSDebug("")
    XSDebug(false, true.B, "%x ", debug_microOp(i).cf.pc)
    XSDebug(false, !valid(i), "- ")
    XSDebug(false, valid(i) && writebacked(i), "w ")
    XSDebug(false, valid(i) && !writebacked(i), "v ")
    if(i % 4 == 3) XSDebug(false, true.B, "\n")
  }

  def ifCommit(counter: UInt): UInt = Mux(io.commits.isWalk, 0.U, counter)

  val commitDebugUop = deqPtrVec.map(_.value).map(debug_microOp(_))
  XSPerfAccumulate("clock_cycle", 1.U)
  QueuePerf(RoqSize, PopCount((0 until RoqSize).map(valid(_))), !allowEnqueue)
  XSPerfAccumulate("commitUop", ifCommit(commitCnt))
  XSPerfAccumulate("commitInstr", ifCommit(trueCommitCnt))
  val commitIsMove = commitDebugUop.map(_.ctrl.isMove)
  XSPerfAccumulate("commitInstrMove", ifCommit(PopCount(io.commits.valid.zip(commitIsMove).map{ case (v, m) => v && m })))
  if (EnableIntMoveElim) {
    val commitMoveElim = commitDebugUop.map(_.debugInfo.eliminatedMove)
    XSPerfAccumulate("commitInstrMoveElim", ifCommit(PopCount(io.commits.valid zip commitMoveElim map { case (v, e) => v && e })))
  }
  XSPerfAccumulate("commitInstrFused", ifCommit(fuseCommitCnt))
  val commitIsLoad = io.commits.info.map(_.commitType).map(_ === CommitType.LOAD)
  val commitLoadValid = io.commits.valid.zip(commitIsLoad).map{ case (v, t) => v && t }
  XSPerfAccumulate("commitInstrLoad", ifCommit(PopCount(commitLoadValid)))
  val commitLoadWaitBit = commitDebugUop.map(_.cf.loadWaitBit)
  XSPerfAccumulate("commitInstrLoadWait", ifCommit(PopCount(commitLoadValid.zip(commitLoadWaitBit).map{ case (v, w) => v && w })))
  val commitIsStore = io.commits.info.map(_.commitType).map(_ === CommitType.STORE)
  XSPerfAccumulate("commitInstrStore", ifCommit(PopCount(io.commits.valid.zip(commitIsStore).map{ case (v, t) => v && t })))
  XSPerfAccumulate("writeback", PopCount((0 until RoqSize).map(i => valid(i) && writebacked(i))))
  // XSPerfAccumulate("enqInstr", PopCount(io.dp1Req.map(_.fire())))
  // XSPerfAccumulate("d2rVnR", PopCount(io.dp1Req.map(p => p.valid && !p.ready)))
  XSPerfAccumulate("walkInstr", Mux(io.commits.isWalk, PopCount(io.commits.valid), 0.U))
  XSPerfAccumulate("walkCycle", state === s_walk || state === s_extrawalk)
  val deqNotWritebacked = valid(deqPtr.value) && !writebacked(deqPtr.value)
  val deqUopCommitType = io.commits.info(0).commitType
  XSPerfAccumulate("waitNormalCycle", deqNotWritebacked && deqUopCommitType === CommitType.NORMAL)
  XSPerfAccumulate("waitBranchCycle", deqNotWritebacked && deqUopCommitType === CommitType.BRANCH)
  XSPerfAccumulate("waitLoadCycle", deqNotWritebacked && deqUopCommitType === CommitType.LOAD)
  XSPerfAccumulate("waitStoreCycle", deqNotWritebacked && deqUopCommitType === CommitType.STORE)
  XSPerfAccumulate("roqHeadPC", io.commits.info(0).pc)
  val dispatchLatency = commitDebugUop.map(uop => uop.debugInfo.dispatchTime - uop.debugInfo.renameTime)
  val enqRsLatency = commitDebugUop.map(uop => uop.debugInfo.enqRsTime - uop.debugInfo.dispatchTime)
  val selectLatency = commitDebugUop.map(uop => uop.debugInfo.selectTime - uop.debugInfo.enqRsTime)
  val issueLatency = commitDebugUop.map(uop => uop.debugInfo.issueTime - uop.debugInfo.selectTime)
  val executeLatency = commitDebugUop.map(uop => uop.debugInfo.writebackTime - uop.debugInfo.issueTime)
  val rsFuLatency = commitDebugUop.map(uop => uop.debugInfo.writebackTime - uop.debugInfo.enqRsTime)
  val commitLatency = commitDebugUop.map(uop => timer - uop.debugInfo.writebackTime)
  def latencySum(cond: Seq[Bool], latency: Seq[UInt]): UInt = {
    cond.zip(latency).map(x => Mux(x._1, x._2, 0.U)).reduce(_ +& _)
  }
  for (fuType <- FuType.functionNameMap.keys) {
    val fuName = FuType.functionNameMap(fuType)
    val commitIsFuType = io.commits.valid.zip(commitDebugUop).map(x => x._1 && x._2.ctrl.fuType === fuType.U )
    XSPerfAccumulate(s"${fuName}_instr_cnt", ifCommit(PopCount(commitIsFuType)))
    XSPerfAccumulate(s"${fuName}_latency_dispatch", ifCommit(latencySum(commitIsFuType, dispatchLatency)))
    XSPerfAccumulate(s"${fuName}_latency_enq_rs", ifCommit(latencySum(commitIsFuType, enqRsLatency)))
    XSPerfAccumulate(s"${fuName}_latency_select", ifCommit(latencySum(commitIsFuType, selectLatency)))
    XSPerfAccumulate(s"${fuName}_latency_issue", ifCommit(latencySum(commitIsFuType, issueLatency)))
    XSPerfAccumulate(s"${fuName}_latency_execute", ifCommit(latencySum(commitIsFuType, executeLatency)))
    XSPerfAccumulate(s"${fuName}_latency_enq_rs_execute", ifCommit(latencySum(commitIsFuType, rsFuLatency)))
    XSPerfAccumulate(s"${fuName}_latency_commit", ifCommit(latencySum(commitIsFuType, commitLatency)))
    if (fuType == FuType.fmac.litValue()) {
      val commitIsFma = commitIsFuType.zip(commitDebugUop).map(x => x._1 && x._2.ctrl.fpu.ren3 )
      XSPerfAccumulate(s"${fuName}_instr_cnt_fma", ifCommit(PopCount(commitIsFma)))
      XSPerfAccumulate(s"${fuName}_latency_enq_rs_execute_fma", ifCommit(latencySum(commitIsFma, rsFuLatency)))
      XSPerfAccumulate(s"${fuName}_latency_execute_fma", ifCommit(latencySum(commitIsFma, executeLatency)))
    }
  }

  val l1Miss = Wire(Bool())
  l1Miss := false.B
  ExcitingUtils.addSink(l1Miss, "TMA_l1miss")
  XSPerfAccumulate("TMA_L1miss", deqNotWritebacked && deqUopCommitType === CommitType.LOAD && l1Miss)


  //difftest signals
  val firstValidCommit = (deqPtr + PriorityMux(io.commits.valid, VecInit(List.tabulate(CommitWidth)(_.U)))).value

  val wdata = Wire(Vec(CommitWidth, UInt(XLEN.W)))
  val wpc = Wire(Vec(CommitWidth, UInt(XLEN.W)))
  val trapVec = Wire(Vec(CommitWidth, Bool()))
  for(i <- 0 until CommitWidth) {
    val idx = deqPtrVec(i).value
    wdata(i) := debug_exuData(idx)
    wpc(i) := SignExt(commitDebugUop(i).cf.pc, XLEN)
    trapVec(i) := io.commits.valid(i) && (state===s_idle) && commitDebugUop(i).ctrl.isXSTrap
  }
  val retireCounterFix = Mux(io.exception.valid, 1.U, retireCounter)
  val retirePCFix = SignExt(Mux(io.exception.valid, io.exception.bits.uop.cf.pc, debug_microOp(firstValidCommit).cf.pc), XLEN)
  val retireInstFix = Mux(io.exception.valid, io.exception.bits.uop.cf.instr, debug_microOp(firstValidCommit).cf.instr)

  val hitTrap = trapVec.reduce(_||_)
  val trapCode = PriorityMux(wdata.zip(trapVec).map(x => x._2 -> x._1))
  val trapPC = SignExt(PriorityMux(wpc.zip(trapVec).map(x => x._2 ->x._1)), XLEN)

  if (!env.FPGAPlatform) {
    for (i <- 0 until CommitWidth) {
      val difftest = Module(new DifftestInstrCommit)
      difftest.io.clock    := clock
      difftest.io.coreid   := hardId.U
      difftest.io.index    := i.U

      val ptr = deqPtrVec(i).value
      val uop = commitDebugUop(i)
      val exuOut = debug_exuDebug(ptr)
      val exuData = debug_exuData(ptr)
      difftest.io.valid    := RegNext(io.commits.valid(i) && !io.commits.isWalk)
      difftest.io.pc       := RegNext(SignExt(uop.cf.pc, XLEN))
      difftest.io.instr    := RegNext(uop.cf.instr)
      difftest.io.special  := RegNext(uop.ctrl.isFused =/= 0.U)
      if (EnableIntMoveElim) {
        // when committing an eliminated move instruction,
        // we must make sure that skip is properly set to false (output from EXU is random value)
        difftest.io.skip     := RegNext(Mux(uop.eliminatedMove, false.B, exuOut.isMMIO || exuOut.isPerfCnt))
      } else {
        difftest.io.skip     := RegNext(exuOut.isMMIO || exuOut.isPerfCnt)
      }
      difftest.io.isRVC    := RegNext(uop.cf.pd.isRVC)
      difftest.io.scFailed := RegNext(!uop.diffTestDebugLrScValid &&
        uop.ctrl.fuType === FuType.mou &&
        (uop.ctrl.fuOpType === LSUOpType.sc_d || uop.ctrl.fuOpType === LSUOpType.sc_w))
      difftest.io.wen      := RegNext(io.commits.valid(i) && uop.ctrl.rfWen && uop.ctrl.ldest =/= 0.U)
      difftest.io.wdata    := RegNext(exuData)
      difftest.io.wdest    := RegNext(uop.ctrl.ldest)

      // XSDebug(p"[difftest-instr-commit]valid:${difftest.io.valid},pc:${difftest.io.pc},instr:${difftest.io.instr},skip:${difftest.io.skip},isRVC:${difftest.io.isRVC},scFailed:${difftest.io.scFailed},wen:${difftest.io.wen},wdata:${difftest.io.wdata},wdest:${difftest.io.wdest}\n")
    }
  }

  if (!env.FPGAPlatform) {
    for (i <- 0 until CommitWidth) {
      val difftest = Module(new DifftestLoadEvent)
      difftest.io.clock  := clock
      difftest.io.coreid := hardId.U
      difftest.io.index  := i.U

      val ptr = deqPtrVec(i).value
      val uop = commitDebugUop(i)
      val exuOut = debug_exuDebug(ptr)
      difftest.io.valid  := RegNext(io.commits.valid(i) && !io.commits.isWalk)
      difftest.io.paddr  := RegNext(exuOut.paddr)
      difftest.io.opType := RegNext(uop.ctrl.fuOpType)
      difftest.io.fuType := RegNext(uop.ctrl.fuType)
    }
  }

  if (!env.FPGAPlatform) {
    val difftest = Module(new DifftestTrapEvent)
    difftest.io.clock    := clock
    difftest.io.coreid   := hardId.U
    difftest.io.valid    := hitTrap
    difftest.io.code     := trapCode
    difftest.io.pc       := trapPC
    difftest.io.cycleCnt := timer
    difftest.io.instrCnt := instrCnt
  }
}
