package xiangshan.backend.roq

import chipsalliance.rocketchip.config.Parameters
import chisel3.ExcitingUtils._
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.backend.ftq.FtqPtr
import difftest._

object roqDebugId extends Function0[Integer] {
  var x = 0
  def apply(): Integer = {
    x = x + 1
    return x
  }
}

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

  def has_exception = exceptionVec.asUInt.orR || flushPipe
  // only exceptions are allowed to writeback when enqueue
  def can_writeback = exceptionVec.asUInt.orR
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

  // instvalid field
  // val valid = RegInit(VecInit(List.fill(RoqSize)(false.B)))
  val valid = Mem(RoqSize, Bool())
  // writeback status
  // val writebacked = Reg(Vec(RoqSize, Bool()))
  val writebacked = Mem(RoqSize, Bool())
  // data for redirect, exception, etc.
  // val flagBkup = RegInit(VecInit(List.fill(RoqSize)(false.B)))
  val flagBkup = Mem(RoqSize, Bool())

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
  for (i <- 0 until RenameWidth) {
    // we don't check whether io.redirect is valid here since redirect has higher priority
    when (canEnqueue(i)) {
      // store uop in data module and debug_microOp Vec
      debug_microOp(enqPtrVec(i).value) := io.enq.req(i).bits
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
      debug_microOp(wbIdx).diffTestDebugLrScValid := io.exeWbResults(i).bits.uop.diffTestDebugLrScValid
      debug_exuData(wbIdx) := io.exeWbResults(i).bits.data
      debug_exuDebug(wbIdx) := io.exeWbResults(i).bits.debug
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
  val exceptionEnable = writebacked(deqPtr.value) && deqHasException
  val isFlushPipe = writebacked(deqPtr.value) && deqHasFlushPipe

  io.flushOut.valid := (state === s_idle) && valid(deqPtr.value) && (intrEnable || exceptionEnable || isFlushPipe)
  io.flushOut.bits.ftqIdx := deqDispatchData.ftqIdx
  io.flushOut.bits.ftqOffset := deqDispatchData.ftqOffset

  val exceptionHappen = (state === s_idle) && valid(deqPtr.value) && (intrEnable || exceptionEnable)
  io.exception.valid := RegNext(exceptionHappen)
  io.exception.bits.uop := RegEnable(debug_deqUop, exceptionHappen)
  io.exception.bits.uop.ctrl.commitType := RegEnable(deqDispatchData.commitType, exceptionHappen)
  io.exception.bits.uop.cf.exceptionVec := RegEnable(exceptionDataRead.bits.exceptionVec, exceptionHappen)
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
  val commit_w = VecInit(deqPtrVec.map(ptr => writebacked(ptr.value)))
  val commit_exception = exceptionDataRead.valid && !isAfter(exceptionDataRead.bits.roqIdx, deqPtrVec.last)
  val commit_block = VecInit((0 until CommitWidth).map(i => !commit_w(i)))
  val allowOnlyOneCommit = commit_exception || intrBitSetReg
  // for instructions that may block others, we don't allow them to commit
  for (i <- 0 until CommitWidth) {
    // defaults: state === s_idle and instructions commit
    // when intrBitSetReg, allow only one instruction to commit at each clock cycle
    val isBlocked = if (i != 0) Cat(commit_block.take(i)).orR || allowOnlyOneCommit else intrEnable || deqHasException
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
      writebacked(enqPtrVec(i).value) := false.B
    }
  }
  when (exceptionGen.io.out.valid) {
    val wbIdx = exceptionGen.io.out.bits.roqIdx.value
    writebacked(wbIdx) := true.B
  }
  // writeback logic set numWbPorts writebacked to true
  for (i <- 0 until numWbPorts) {
    when (io.exeWbResults(i).valid) {
      val wbIdx = io.exeWbResults(i).bits.uop.roqIdx.value
      val block_wb = selectAll(io.exeWbResults(i).bits.uop.cf.exceptionVec, false, true).asUInt.orR || io.exeWbResults(i).bits.uop.ctrl.flushPipe
      writebacked(wbIdx) := !block_wb
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
  dispatchData.io.wdata.zip(io.enq.req.map(_.bits)).map{ case (wdata, req) =>
    wdata.ldest := req.ctrl.ldest
    wdata.rfWen := req.ctrl.rfWen
    wdata.fpWen := req.ctrl.fpWen
    wdata.wflags := req.ctrl.fpu.wflags
    wdata.commitType := req.ctrl.commitType
    wdata.pdest := req.pdest
    wdata.old_pdest := req.old_pdest
    wdata.ftqIdx := req.cf.ftqPtr
    wdata.ftqOffset := req.cf.ftqOffset
    wdata.pc := req.cf.pc
    wdata.crossPageIPFFix := req.cf.crossPageIPFFix
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
  }

  // TODO: don't hard code these idxes
  def csr_wb_idx = 6
  def atomic_wb_idx = 4
  def load_wb_idxes = Seq(5)
  def store_wb_idxes = Seq(16, 17)
  val all_exception_possibilities = Seq(csr_wb_idx, atomic_wb_idx) ++ load_wb_idxes ++ store_wb_idxes
  all_exception_possibilities.zipWithIndex.map{ case (p, i) => connect_exception(i, p) }
  def connect_exception(index: Int, wb_index: Int) = {
    exceptionGen.io.wb(index).valid             := io.exeWbResults(wb_index).valid
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
  }

  // 4 fmac + 2 fmisc + 1 i2f
  val fflags_wb = io.exeWbResults.zipWithIndex.filter(w => {
    (Seq(8, 9, 10, 11) ++ Seq(14, 15) ++ Seq(7)).contains(w._2)
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

  XSPerfAccumulate("clock_cycle", 1.U)
  QueuePerf(RoqSize, PopCount((0 until RoqSize).map(valid(_))), !allowEnqueue)
  io.roqFull := !allowEnqueue
  XSPerfAccumulate("commitInstr", Mux(io.commits.isWalk, 0.U, PopCount(io.commits.valid)))
  val commitIsMove = deqPtrVec.map(_.value).map(ptr => debug_microOp(ptr).ctrl.isMove)
  XSPerfAccumulate("commitInstrMove", Mux(io.commits.isWalk, 0.U, PopCount(io.commits.valid.zip(commitIsMove).map{ case (v, m) => v && m })))
  val commitSrc1MoveElim = deqPtrVec.map(_.value).map(ptr => debug_microOp(ptr).debugInfo.src1MoveElim)
  XSPerfAccumulate("commitInstrSrc1MoveElim", Mux(io.commits.isWalk, 0.U, PopCount(io.commits.valid.zip(commitSrc1MoveElim).map{ case (v, e) => v && e })))
  val commitSrc2MoveElim = deqPtrVec.map(_.value).map(ptr => debug_microOp(ptr).debugInfo.src2MoveElim)
  XSPerfAccumulate("commitInstrSrc2MoveElim", Mux(io.commits.isWalk, 0.U, PopCount(io.commits.valid.zip(commitSrc2MoveElim).map{ case (v, e) => v && e })))
  val commitIsLoad = io.commits.info.map(_.commitType).map(_ === CommitType.LOAD)
  val commitLoadValid = io.commits.valid.zip(commitIsLoad).map{ case (v, t) => v && t }
  XSPerfAccumulate("commitInstrLoad", Mux(io.commits.isWalk, 0.U, PopCount(commitLoadValid)))
  val commitLoadWaitBit = deqPtrVec.map(_.value).map(ptr => debug_microOp(ptr).cf.loadWaitBit)
  XSPerfAccumulate("commitInstrLoadWait", Mux(io.commits.isWalk, 0.U, PopCount(commitLoadValid.zip(commitLoadWaitBit).map{ case (v, w) => v && w })))
  val commitIsStore = io.commits.info.map(_.commitType).map(_ === CommitType.STORE)
  XSPerfAccumulate("commitInstrStore", Mux(io.commits.isWalk, 0.U, PopCount(io.commits.valid.zip(commitIsStore).map{ case (v, t) => v && t })))
  XSPerfAccumulate("writeback", PopCount((0 until RoqSize).map(i => valid(i) && writebacked(i))))
  // XSPerfAccumulate("enqInstr", PopCount(io.dp1Req.map(_.fire())))
  // XSPerfAccumulate("d2rVnR", PopCount(io.dp1Req.map(p => p.valid && !p.ready)))
  XSPerfAccumulate("walkInstrAcc", Mux(io.commits.isWalk, PopCount(io.commits.valid), 0.U))
  XSPerfAccumulate("walkCycleAcc", state === s_walk || state === s_extrawalk)
  val deqNotWritebacked = valid(deqPtr.value) && !writebacked(deqPtr.value)
  val deqUopCommitType = io.commits.info(0).commitType
  XSPerfAccumulate("waitNormalCycleAcc", deqNotWritebacked && deqUopCommitType === CommitType.NORMAL)
  XSPerfAccumulate("waitBranchCycleAcc", deqNotWritebacked && deqUopCommitType === CommitType.BRANCH)
  XSPerfAccumulate("waitLoadCycleAcc", deqNotWritebacked && deqUopCommitType === CommitType.LOAD)
  XSPerfAccumulate("waitStoreCycleAcc", deqNotWritebacked && deqUopCommitType === CommitType.STORE)
  XSPerfAccumulate("roqHeadPC", io.commits.info(0).pc)

  val instrCnt = RegInit(0.U(64.W))
  val retireCounter = Mux(state === s_idle, commitCnt, 0.U)
  instrCnt := instrCnt + retireCounter
  io.csr.perfinfo.retiredInstr := RegNext(retireCounter)

  //difftest signals
  val firstValidCommit = (deqPtr + PriorityMux(io.commits.valid, VecInit(List.tabulate(CommitWidth)(_.U)))).value

  val skip = Wire(Vec(CommitWidth, Bool()))
  val wen = Wire(Vec(CommitWidth, Bool()))
  val wdata = Wire(Vec(CommitWidth, UInt(XLEN.W)))
  val lpaddr = Wire(Vec(CommitWidth, UInt(PAddrBits.W)))
  val ltype = Wire(Vec(CommitWidth, UInt(32.W)))
  val lfu = Wire(Vec(CommitWidth, UInt(4.W)))
  val wdst = Wire(Vec(CommitWidth, UInt(32.W)))
  val diffTestDebugLrScValid = Wire(Vec(CommitWidth, Bool()))
  val wpc = Wire(Vec(CommitWidth, UInt(XLEN.W)))
  val trapVec = Wire(Vec(CommitWidth, Bool()))
  val isRVC = Wire(Vec(CommitWidth, Bool()))
  for(i <- 0 until CommitWidth) {
    // io.commits(i).valid
    val idx = deqPtrVec(i).value
    val uop = debug_microOp(idx)
    val DifftestSkipSC = false
    if(!DifftestSkipSC){
      skip(i) := (debug_exuDebug(idx).isMMIO || debug_exuDebug(idx).isPerfCnt) && io.commits.valid(i)
    }else{
      skip(i) := (
          debug_exuDebug(idx).isMMIO ||
          debug_exuDebug(idx).isPerfCnt ||
          uop.ctrl.fuType === FuType.mou && uop.ctrl.fuOpType === LSUOpType.sc_d ||
          uop.ctrl.fuType === FuType.mou && uop.ctrl.fuOpType === LSUOpType.sc_w
        ) && io.commits.valid(i)
    }
    wen(i) := io.commits.valid(i) && uop.ctrl.rfWen && uop.ctrl.ldest =/= 0.U
    wdata(i) := debug_exuData(idx)
    lpaddr(i) := debug_exuDebug(idx).paddr
    lfu(i) := uop.ctrl.fuType
    ltype(i) := uop.ctrl.fuOpType
    wdst(i) := uop.ctrl.ldest
    diffTestDebugLrScValid(i) := uop.diffTestDebugLrScValid
    wpc(i) := SignExt(uop.cf.pc, XLEN)
    trapVec(i) := io.commits.valid(i) && (state===s_idle) && uop.ctrl.isXSTrap
    isRVC(i) := uop.cf.pd.isRVC
  }
  val retireCounterFix = Mux(io.exception.valid, 1.U, retireCounter)
  val retirePCFix = SignExt(Mux(io.exception.valid, io.exception.bits.uop.cf.pc, debug_microOp(firstValidCommit).cf.pc), XLEN)
  val retireInstFix = Mux(io.exception.valid, io.exception.bits.uop.cf.instr, debug_microOp(firstValidCommit).cf.instr)

  val scFailed = !diffTestDebugLrScValid(0) &&
    debug_deqUop.ctrl.fuType === FuType.mou &&
    (debug_deqUop.ctrl.fuOpType === LSUOpType.sc_d || debug_deqUop.ctrl.fuOpType === LSUOpType.sc_w)

  val hitTrap = trapVec.reduce(_||_)
  val trapCode = PriorityMux(wdata.zip(trapVec).map(x => x._2 -> x._1))
  val trapPC = SignExt(PriorityMux(wpc.zip(trapVec).map(x => x._2 ->x._1)), XLEN)

  if (!env.FPGAPlatform) {
    ExcitingUtils.addSource(hitTrap, "XSTRAP", ConnectionType.Debug)
  }

  if (!env.FPGAPlatform) {
    for (i <- 0 until CommitWidth) {
      val difftest = Module(new DifftestInstrCommit)
      difftest.io.clock    := clock
      difftest.io.coreid   := 0.U
      difftest.io.index    := i.U

      val ptr = deqPtrVec(i).value
      val uop = debug_microOp(ptr)
      val exuOut = debug_exuDebug(ptr)
      val exuData = debug_exuData(ptr)
      difftest.io.valid    := RegNext(io.commits.valid(i) && !io.commits.isWalk)
      difftest.io.pc       := RegNext(SignExt(uop.cf.pc, XLEN))
      difftest.io.instr    := RegNext(uop.cf.instr)
      difftest.io.skip     := RegNext(exuOut.isMMIO || exuOut.isPerfCnt)
      difftest.io.isRVC    := RegNext(uop.cf.pd.isRVC)
      difftest.io.scFailed := RegNext(!uop.diffTestDebugLrScValid &&
        uop.ctrl.fuType === FuType.mou &&
        (uop.ctrl.fuOpType === LSUOpType.sc_d || uop.ctrl.fuOpType === LSUOpType.sc_w))
      difftest.io.wen      := RegNext(io.commits.valid(i) && uop.ctrl.rfWen && uop.ctrl.ldest =/= 0.U)
      difftest.io.wdata    := RegNext(exuData)
      difftest.io.wdest    := RegNext(uop.ctrl.ldest)
    }
  }

  if (!env.FPGAPlatform) {
    for (i <- 0 until CommitWidth) {
      val difftest = Module(new DifftestLoadEvent)
      difftest.io.clock  := clock
      difftest.io.coreid := 0.U
      difftest.io.index  := i.U

      val ptr = deqPtrVec(i).value
      val uop = debug_microOp(ptr)
      val exuOut = debug_exuDebug(ptr)
      difftest.io.valid  := io.commits.valid(i) && !io.commits.isWalk
      difftest.io.paddr  := exuOut.paddr
      difftest.io.opType := uop.ctrl.fuOpType
      difftest.io.fuType := uop.ctrl.fuType
    }
  }

  if (!env.FPGAPlatform) {
    val difftest = Module(new DifftestTrapEvent)
    difftest.io.clock    := clock
    difftest.io.coreid   := 0.U
    difftest.io.valid    := hitTrap
    difftest.io.code     := trapCode
    difftest.io.pc       := trapPC
    difftest.io.cycleCnt := GTimer()
    difftest.io.instrCnt := instrCnt
  }
}
