package xiangshan.backend.roq

import chisel3.ExcitingUtils._
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.backend.LSUOpType
import xiangshan.mem.{LqPtr, SqPtr}

object roqDebugId extends Function0[Integer] {
  var x = 0
  def apply(): Integer = {
    x = x + 1
    return x
  }
}

class RoqPtr extends CircularQueuePtr(RoqPtr.RoqSize) with HasCircularQueuePtrHelper {
  def needFlush(redirect: Valid[Redirect]): Bool = {
    val flushItself = redirect.bits.flushItself() && this === redirect.bits.roqIdx
    redirect.valid && (redirect.bits.isUnconditional() || flushItself || isAfter(this, redirect.bits.roqIdx))
  }
}

object RoqPtr extends HasXSParameter {
  def apply(f: Bool, v: UInt): RoqPtr = {
    val ptr = Wire(new RoqPtr)
    ptr.flag := f
    ptr.value := v
    ptr
  }
}

class RoqCSRIO extends XSBundle {
  val intrBitSet = Input(Bool())
  val trapTarget = Input(UInt(VAddrBits.W))

  val fflags = Output(Valid(UInt(5.W)))
  val dirty_fs = Output(Bool())
  val perfinfo = new Bundle {
    val retiredInstr = Output(UInt(3.W))
  }
}

class RoqLsqIO extends XSBundle {
  val lcommit = Output(UInt(3.W))
  val scommit = Output(UInt(3.W))
  val pendingld = Output(Bool())
  val pendingst = Output(Bool())
  val commit = Output(Bool())
}

class RoqEnqIO extends XSBundle {
  val canAccept = Output(Bool())
  val isEmpty = Output(Bool())
  // valid vector, for roqIdx gen and walk
  val needAlloc = Vec(RenameWidth, Input(Bool()))
  val req = Vec(RenameWidth, Flipped(ValidIO(new MicroOp)))
  val resp = Vec(RenameWidth, Output(new RoqPtr))
}

class RoqDispatchData extends RoqCommitInfo {
  val crossPageIPFFix = Bool()
}

class RoqWbData extends XSBundle {
  val flushPipe = Bool()
}

class RoqDeqPtrWrapper extends XSModule with HasCircularQueuePtrHelper {
  val io = IO(new Bundle {
    // for commits/flush
    val state = Input(UInt(2.W))
    val deq_v = Vec(CommitWidth, Input(Bool()))
    val deq_w = Vec(CommitWidth, Input(Bool()))
    val deq_exceptionVec = Vec(CommitWidth, Input(ExceptionVec()))
    val deq_flushPipe = Vec(CommitWidth, Input(Bool()))
    // for flush: when exception occurs, reset deqPtrs to range(0, CommitWidth)
    val intrBitSetReg = Input(Bool())
    val hasNoSpecExec = Input(Bool())
    val commitType = Input(CommitType())
    // output: the CommitWidth deqPtr
    val out = Vec(CommitWidth, Output(new RoqPtr))
    val next_out = Vec(CommitWidth, Output(new RoqPtr))
  })

  val deqPtrVec = RegInit(VecInit((0 until CommitWidth).map(_.U.asTypeOf(new RoqPtr))))

  val possibleException = VecInit(io.deq_exceptionVec.map(selectAll(_, false)))
  // for exceptions (flushPipe included) and interrupts:
  // only consider the first instruction
  val intrEnable = io.intrBitSetReg && !io.hasNoSpecExec && !CommitType.isLoadStore(io.commitType)
  val exceptionEnable = io.deq_w(0) && (possibleException(0).asUInt.orR || io.deq_flushPipe(0))
  val redirectOutValid = io.state === 0.U && io.deq_v(0) && (intrEnable || exceptionEnable)

  // for normal commits: only to consider when there're no exceptions
  // we don't need to consider whether the first instruction has exceptions since it wil trigger exceptions.
  val commitBlocked = VecInit((0 until CommitWidth).map(i => if (i == 0) false.B else possibleException(i).asUInt.orR || io.deq_flushPipe(i)))
  val canCommit = VecInit((0 until CommitWidth).map(i => io.deq_v(i) && io.deq_w(i) /*&& !commitBlocked(i)*/))
  val normalCommitCnt = PriorityEncoder(canCommit.map(c => !c) :+ true.B)
  // when io.intrBitSetReg or there're possible exceptions in these instructions, only one instruction is allowed to commit
  val allowOnlyOne = VecInit(commitBlocked.drop(1)).asUInt.orR || io.intrBitSetReg
  val commitCnt = Mux(allowOnlyOne, io.deq_v(0) && io.deq_w(0), normalCommitCnt)

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

class RoqEnqPtrWrapper extends XSModule with HasCircularQueuePtrHelper {
  val io = IO(new Bundle {
    // for exceptions and interrupts
    val state = Input(UInt(2.W))
    val deq_v = Input(Bool())
    val deq_w = Input(Bool())
    val deq_exceptionVec = Input(ExceptionVec())
    val deq_flushPipe = Input(Bool())
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
  val exceptionEnable = io.deq_w && (selectAll(io.deq_exceptionVec, false).asUInt.orR || io.deq_flushPipe)
  val redirectOutValid = io.state === 0.U && io.deq_v && (intrEnable || exceptionEnable)

  // enqueue
  val canAccept = io.allowEnqueue && !io.hasBlockBackward
  val dispatchNum = Mux(canAccept, PopCount(io.enq), 0.U)

  when (redirectOutValid) {
    enqPtr := 0.U.asTypeOf(new RoqPtr)
  }.elsewhen (io.redirect.valid) {
    enqPtr := io.redirect.bits.roqIdx + Mux(io.redirect.bits.flushItself(), 0.U, 1.U)
  }.otherwise {
    enqPtr := enqPtr + dispatchNum
  }

  io.out := enqPtr

}

// class RoqStateWrapper extends XSModule with HasCircularQueuePtrHelper {
//   val io = IO(new Bundle {
//     val redirect = ValidIO(new Redirect)
//     val raddr = Vec(CommitWidth, Input(UInt(log2Up(numEntries).W)))
//     val wen = Vec(RenameWidth, Input(Bool()))
//     val waddr = Vec(RenameWidth)
//   })

//   val valid = Mme(RoqSize, Bool())
//   val flagBkup = RegInit(VecInit(List.fill(RoqSize)(false.B)))

//   for (i <- 0 until RoqSize) {
//     when (reset.asBool || io.redirectOut.valid) {
//       valid(i) := false.B
//     }.elsewhen (io.redirectOut.valid)
//   }
//   when (reset.asBool) {
//     valid(i)
//   }
//   // enqueue logic writes 6 valid
//   for (i <- 0 until RenameWidth) {
//     when (canEnqueue(i) && !io.redirect.valid) {
//       valid(enqPtrVec(i).value) := true.B
//     }
//   }
//   // dequeue/walk logic writes 6 valid, dequeue and walk will not happen at the same time
//   for (i <- 0 until CommitWidth) {
//     when (io.commits.valid(i) && state =/= s_extrawalk) {
//       valid(commitReadAddr(i)) := false.B
//     }
//   }
//   // reset: when exception, reset all valid to false
//   when (io.redirectOut.valid) {
//     for (i <- 0 until RoqSize) {
//       valid(i) := false.B
//     }
//   }

// }

class Roq(numWbPorts: Int) extends XSModule with HasCircularQueuePtrHelper {
  val io = IO(new Bundle() {
    val redirect = Input(Valid(new Redirect))
    val enq = new RoqEnqIO
    val redirectOut = Output(Valid(new Redirect))
    val exception = Output(new MicroOp)
    // exu + brq
    val exeWbResults = Vec(numWbPorts, Flipped(ValidIO(new ExuOutput)))
    val commits = new RoqCommitIO
    val lsq = new RoqLsqIO
    val bcommit = Output(UInt(BrTagWidth.W))
    val roqDeqPtr = Output(new RoqPtr)
    val csr = new RoqCSRIO
  })

  val difftestIO = IO(new Bundle() {
    val commit = Output(UInt(32.W))
    val thisPC = Output(UInt(XLEN.W))
    val thisINST = Output(UInt(32.W))
    val skip = Output(UInt(32.W))
    val wen = Output(UInt(32.W))
    val wdata = Output(Vec(CommitWidth, UInt(XLEN.W))) // set difftest width to 6
    val wdst = Output(Vec(CommitWidth, UInt(32.W))) // set difftest width to 6
    val wpc = Output(Vec(CommitWidth, UInt(XLEN.W))) // set difftest width to 6
    val isRVC = Output(UInt(32.W))
    val scFailed = Output(Bool())
  })
  difftestIO <> DontCare

  val trapIO = IO(new TrapIO())
  trapIO <> DontCare

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

  val writebackData = Module(new SyncDataModuleTemplate(new RoqWbData, RoqSize, CommitWidth, numWbPorts))
  val writebackDataRead = writebackData.io.rdata

  val exceptionDataRead = Wire(Vec(CommitWidth, ExceptionVec()))
  val fflagsDataRead = Wire(Vec(CommitWidth, UInt(5.W)))

  io.roqDeqPtr := deqPtr

  /**
    * Enqueue (from dispatch)
    */
  // special cases
  val hasBlockBackward = RegInit(false.B)
  val hasNoSpecExec = RegInit(false.B)
  // When blockBackward instruction leaves Roq (commit or walk), hasBlockBackward should be set to false.B
  // val blockBackwardLeave = Cat((0 until CommitWidth).map(i => io.commits.valid(i) && io.commits.uop(i).ctrl.blockBackward)).orR
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
  val deqWritebackData = writebackDataRead(0)
  val debug_deqUop = debug_microOp(deqPtr.value)

  val deqExceptionVec = exceptionDataRead(0)
  // For MMIO instructions, they should not trigger interrupts since they may be sent to lower level before it writes back.
  // However, we cannot determine whether a load/store instruction is MMIO.
  // Thus, we don't allow load/store instructions to trigger an interrupt.
  val intrBitSetReg = RegNext(io.csr.intrBitSet)
  val intrEnable = intrBitSetReg && !hasNoSpecExec && !CommitType.isLoadStore(deqDispatchData.commitType)
  val exceptionEnable = writebacked(deqPtr.value) && Cat(deqExceptionVec).orR()
  val isFlushPipe = writebacked(deqPtr.value) && deqWritebackData.flushPipe
  io.redirectOut := DontCare
  io.redirectOut.valid := (state === s_idle) && valid(deqPtr.value) && (intrEnable || exceptionEnable || isFlushPipe)
  io.redirectOut.bits.level := Mux(intrEnable || exceptionEnable, RedirectLevel.exception, RedirectLevel.flushAll)
  io.redirectOut.bits.interrupt := intrEnable
  io.redirectOut.bits.target := Mux(intrEnable || exceptionEnable, io.csr.trapTarget, deqDispatchData.pc + 4.U)

  io.exception := debug_deqUop
  io.exception.ctrl.commitType := deqDispatchData.commitType
  io.exception.cf.pc := deqDispatchData.pc
  io.exception.cf.exceptionVec := deqExceptionVec
  io.exception.cf.crossPageIPFFix := deqDispatchData.crossPageIPFFix

  XSDebug(io.redirectOut.valid,
    p"generate redirect: pc 0x${Hexadecimal(io.exception.cf.pc)} intr $intrEnable " +
    p"excp $exceptionEnable flushPipe $isFlushPipe target 0x${Hexadecimal(io.redirectOut.bits.target)} " +
    p"Trap_target 0x${Hexadecimal(io.csr.trapTarget)} exceptionVec ${Binary(deqExceptionVec.asUInt)}\n")


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

  io.commits.isWalk := state =/= s_idle
  val commit_v = Mux(state === s_idle, VecInit(deqPtrVec.map(ptr => valid(ptr.value))), VecInit(walkPtrVec.map(ptr => valid(ptr.value))))
  val commit_w = VecInit(deqPtrVec.map(ptr => writebacked(ptr.value)))
  val commit_exception = exceptionDataRead.zip(writebackDataRead.map(_.flushPipe)).map{ case (e, f) => e.asUInt.orR || f }
  val commit_block = VecInit((0 until CommitWidth).map(i => !commit_w(i)))
  val allowOnlyOneCommit = VecInit(commit_exception.drop(1)).asUInt.orR || intrBitSetReg
  // for instructions that may block others, we don't allow them to commit
  for (i <- 0 until CommitWidth) {
    // defaults: state === s_idle and instructions commit
    // when intrBitSetReg, allow only one instruction to commit at each clock cycle
    val isBlocked = if (i != 0) Cat(commit_block.take(i)).orR || allowOnlyOneCommit else intrEnable || commit_exception(0)
    io.commits.valid(i) := commit_v(i) && commit_w(i) && !isBlocked
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
  val state_next = Mux(io.redirectOut.valid,
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
  deqPtrGenModule.io.deq_exceptionVec := exceptionDataRead
  deqPtrGenModule.io.deq_flushPipe := writebackDataRead.map(_.flushPipe)
  deqPtrGenModule.io.intrBitSetReg := intrBitSetReg
  deqPtrGenModule.io.hasNoSpecExec := hasNoSpecExec
  deqPtrGenModule.io.commitType := deqDispatchData.commitType
  deqPtrVec := deqPtrGenModule.io.out
  val deqPtrVec_next = deqPtrGenModule.io.next_out

  val enqPtrGenModule = Module(new RoqEnqPtrWrapper)
  enqPtrGenModule.io.state := state
  enqPtrGenModule.io.deq_v := commit_v(0)
  enqPtrGenModule.io.deq_w := commit_w(0)
  enqPtrGenModule.io.deq_exceptionVec := deqExceptionVec
  enqPtrGenModule.io.deq_flushPipe := writebackDataRead(0).flushPipe
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
  validCounter := Mux(io.redirectOut.valid,
    0.U,
    Mux(state === s_idle,
      (validCounter - commitCnt) + dispatchNum,
      trueValidCounter
    )
  )

  allowEnqueue := Mux(io.redirectOut.valid,
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
    when (canEnqueue(i) && !io.redirect.valid) {
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
  when (io.redirectOut.valid) {
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
      writebacked(enqPtrVec(i).value) := selectFrontend(io.enq.req(i).bits.cf.exceptionVec, false).asUInt.orR
    }
  }
  // writeback logic set numWbPorts writebacked to true
  for (i <- 0 until numWbPorts) {
    when (io.exeWbResults(i).valid) {
      val wbIdx = io.exeWbResults(i).bits.uop.roqIdx.value
      writebacked(wbIdx) := true.B
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
    wdata.pc := req.cf.pc
    wdata.crossPageIPFFix := req.cf.crossPageIPFFix
    // wdata.exceptionVec := req.cf.exceptionVec
  }
  dispatchData.io.raddr := commitReadAddr_next

  writebackData.io.wen := io.exeWbResults.map(_.valid)
  writebackData.io.waddr := io.exeWbResults.map(_.bits.uop.roqIdx.value)
  writebackData.io.wdata.zip(io.exeWbResults.map(_.bits)).map{ case (wdata, wb) =>
    wdata.flushPipe := wb.uop.ctrl.flushPipe
  }
  writebackData.io.raddr := commitReadAddr_next

  for (i <- 0 until 16) {
    val exceptionData = Module(new SyncDataModuleTemplate(Bool(), RoqSize, CommitWidth, RenameWidth + writebackCount(i)))
    exceptionData.suggestName("exceptionData")
    var wPortIdx = 0
    for (j <- 0 until RenameWidth) {
      exceptionData.io.wen  (wPortIdx) := canEnqueue(j)
      exceptionData.io.waddr(wPortIdx) := enqPtrVec(j).value
      exceptionData.io.wdata(wPortIdx) := (if (allPossibleSet.contains(i)) io.enq.req(j).bits.cf.exceptionVec(i) else false.B)
      wPortIdx = wPortIdx + 1
    }
    if (csrWbCount(i) > 0) {
      exceptionData.io.wen  (wPortIdx) := io.exeWbResults(6).valid
      exceptionData.io.waddr(wPortIdx) := io.exeWbResults(6).bits.uop.roqIdx.value
      exceptionData.io.wdata(wPortIdx) := io.exeWbResults(6).bits.uop.cf.exceptionVec(i)
      wPortIdx = wPortIdx + 1
    }
    if (atomicsWbCount(i) > 0) {
      exceptionData.io.wen  (wPortIdx) := io.exeWbResults(4).valid
      exceptionData.io.waddr(wPortIdx) := io.exeWbResults(4).bits.uop.roqIdx.value
      exceptionData.io.wdata(wPortIdx) := io.exeWbResults(4).bits.uop.cf.exceptionVec(i)
      wPortIdx = wPortIdx + 1
    }
    if (loadWbCount(i) > 0) {
      exceptionData.io.wen  (wPortIdx) := io.exeWbResults(5).valid
      exceptionData.io.waddr(wPortIdx) := io.exeWbResults(5).bits.uop.roqIdx.value
      exceptionData.io.wdata(wPortIdx) := io.exeWbResults(5).bits.uop.cf.exceptionVec(i)
      wPortIdx = wPortIdx + 1
    }
    if (storeWbCount(i) > 0) {
      exceptionData.io.wen  (wPortIdx) := io.exeWbResults(16).valid
      exceptionData.io.waddr(wPortIdx) := io.exeWbResults(16).bits.uop.roqIdx.value
      exceptionData.io.wdata(wPortIdx) := io.exeWbResults(16).bits.uop.cf.exceptionVec(i)
      wPortIdx = wPortIdx + 1
      exceptionData.io.wen  (wPortIdx) := io.exeWbResults(17).valid
      exceptionData.io.waddr(wPortIdx) := io.exeWbResults(17).bits.uop.roqIdx.value
      exceptionData.io.wdata(wPortIdx) := io.exeWbResults(17).bits.uop.cf.exceptionVec(i)
      wPortIdx = wPortIdx + 1
    }

    exceptionData.io.raddr := VecInit(deqPtrVec_next.map(_.value))
    exceptionDataRead.zip(exceptionData.io.rdata).map{ case (d, r) => d(i) := r }
  }

  val fflagsDataModule = Module(new SyncDataModuleTemplate(UInt(5.W), RoqSize, CommitWidth, 7))
  var wPortIdx = 0
  // 4 FMACs
  for (i <- 0 until 4) {
    fflagsDataModule.io.wen  (wPortIdx) := io.exeWbResults(8+i).valid
    fflagsDataModule.io.waddr(wPortIdx) := io.exeWbResults(8+i).bits.uop.roqIdx.value
    fflagsDataModule.io.wdata(wPortIdx) := io.exeWbResults(8+i).bits.fflags
    wPortIdx = wPortIdx + 1
  }
  // 2 FMISCs (the first one includes I2F from JumpUnit)
  for (i <- 0 until 2) {
    fflagsDataModule.io.wen  (wPortIdx) := io.exeWbResults(14+i).valid
    fflagsDataModule.io.waddr(wPortIdx) := io.exeWbResults(14+i).bits.uop.roqIdx.value
    fflagsDataModule.io.wdata(wPortIdx) := io.exeWbResults(14+i).bits.fflags
    wPortIdx = wPortIdx + 1
  }
  // 1 FMISC (Int Wb)
  fflagsDataModule.io.wen  (wPortIdx) := io.exeWbResults(7).valid
  fflagsDataModule.io.waddr(wPortIdx) := io.exeWbResults(7).bits.uop.roqIdx.value
  fflagsDataModule.io.wdata(wPortIdx) := io.exeWbResults(7).bits.fflags
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

  XSPerf("utilization", PopCount((0 until RoqSize).map(valid(_))))
  XSPerf("commitInstr", Mux(io.commits.isWalk, 0.U, PopCount(io.commits.valid)))
  XSPerf("commitInstrLoad", Mux(io.commits.isWalk, 0.U, PopCount(io.commits.valid.zip(io.commits.info.map(_.commitType)).map{ case (v, t) => v && t === CommitType.LOAD})))
  XSPerf("commitInstrStore", Mux(io.commits.isWalk, 0.U, PopCount(io.commits.valid.zip(io.commits.info.map(_.commitType)).map{ case (v, t) => v && t === CommitType.STORE})))
  XSPerf("writeback", PopCount((0 until RoqSize).map(i => valid(i) && writebacked(i))))
  // XSPerf("enqInstr", PopCount(io.dp1Req.map(_.fire())))
  // XSPerf("d2rVnR", PopCount(io.dp1Req.map(p => p.valid && !p.ready)))
  XSPerf("walkInstr", Mux(io.commits.isWalk, PopCount(io.commits.valid), 0.U))
  XSPerf("walkCycle", state === s_walk || state === s_extrawalk)
  val deqNotWritebacked = valid(deqPtr.value) && !writebacked(deqPtr.value)
  val deqUopCommitType = io.commits.info(0).commitType
  XSPerf("waitNormalCycle", deqNotWritebacked && deqUopCommitType === CommitType.NORMAL)
  XSPerf("waitBranchCycle", deqNotWritebacked && deqUopCommitType === CommitType.BRANCH)
  XSPerf("waitLoadCycle", deqNotWritebacked && deqUopCommitType === CommitType.LOAD)
  XSPerf("waitStoreCycle", deqNotWritebacked && deqUopCommitType === CommitType.STORE)
  XSPerf("roqHeadPC", io.commits.info(0).pc)

  val instrCnt = RegInit(0.U(64.W))
  val retireCounter = Mux(state === s_idle, commitCnt, 0.U)
  instrCnt := instrCnt + retireCounter
  io.csr.perfinfo.retiredInstr := RegNext(retireCounter)

  //difftest signals
  val firstValidCommit = (deqPtr + PriorityMux(io.commits.valid, VecInit(List.tabulate(CommitWidth)(_.U)))).value

  val skip = Wire(Vec(CommitWidth, Bool()))
  val wen = Wire(Vec(CommitWidth, Bool()))
  val wdata = Wire(Vec(CommitWidth, UInt(XLEN.W)))
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
    wdst(i) := uop.ctrl.ldest
    diffTestDebugLrScValid(i) := uop.diffTestDebugLrScValid
    wpc(i) := SignExt(uop.cf.pc, XLEN)
    trapVec(i) := io.commits.valid(i) && (state===s_idle) && uop.ctrl.isXSTrap
    isRVC(i) := uop.cf.brUpdate.pd.isRVC
  }
  val retireCounterFix = Mux(io.redirectOut.valid, 1.U, retireCounter)
  val retirePCFix = SignExt(Mux(io.redirectOut.valid, debug_deqUop.cf.pc, debug_microOp(firstValidCommit).cf.pc), XLEN)
  val retireInstFix = Mux(io.redirectOut.valid, debug_deqUop.cf.instr, debug_microOp(firstValidCommit).cf.instr)

  val scFailed = !diffTestDebugLrScValid(0) &&
    debug_deqUop.ctrl.fuType === FuType.mou &&
    (debug_deqUop.ctrl.fuOpType === LSUOpType.sc_d || debug_deqUop.ctrl.fuOpType === LSUOpType.sc_w)

  val hitTrap = trapVec.reduce(_||_)
  val trapCode = PriorityMux(wdata.zip(trapVec).map(x => x._2 -> x._1))
  val trapPC = SignExt(PriorityMux(wpc.zip(trapVec).map(x => x._2 ->x._1)), XLEN)

  if (!env.FPGAPlatform) {

    val difftestIntrNO = WireInit(0.U(XLEN.W))
    val difftestCause = WireInit(0.U(XLEN.W))
    ExcitingUtils.addSink(difftestIntrNO, "difftestIntrNOfromCSR")
    ExcitingUtils.addSink(difftestCause, "difftestCausefromCSR")
    XSDebug(difftestIntrNO =/= 0.U, "difftest intrNO set %x\n", difftestIntrNO)

    ExcitingUtils.addSource(RegNext(retireCounterFix), "difftestCommit", ExcitingUtils.Debug)
    ExcitingUtils.addSource(RegNext(retirePCFix), "difftestThisPC", ExcitingUtils.Debug)//first valid PC
    ExcitingUtils.addSource(RegNext(retireInstFix), "difftestThisINST", ExcitingUtils.Debug)//first valid inst
    ExcitingUtils.addSource(RegNext(skip.asUInt), "difftestSkip", ExcitingUtils.Debug)
    ExcitingUtils.addSource(RegNext(isRVC.asUInt), "difftestIsRVC", ExcitingUtils.Debug)
    ExcitingUtils.addSource(RegNext(wen.asUInt), "difftestWen", ExcitingUtils.Debug)
    ExcitingUtils.addSource(RegNext(wpc), "difftestWpc", ExcitingUtils.Debug)
    ExcitingUtils.addSource(RegNext(wdata), "difftestWdata", ExcitingUtils.Debug)
    ExcitingUtils.addSource(RegNext(wdst), "difftestWdst", ExcitingUtils.Debug)
    ExcitingUtils.addSource(RegNext(scFailed), "difftestScFailed", ExcitingUtils.Debug)
    ExcitingUtils.addSource(RegNext(difftestIntrNO), "difftestIntrNO", ExcitingUtils.Debug)
    ExcitingUtils.addSource(RegNext(difftestCause), "difftestCause", ExcitingUtils.Debug)

    ExcitingUtils.addSource(RegNext(hitTrap), "trapValid")
    ExcitingUtils.addSource(RegNext(trapCode), "trapCode")
    ExcitingUtils.addSource(RegNext(trapPC), "trapPC")
    ExcitingUtils.addSource(RegNext(GTimer()), "trapCycleCnt")
    ExcitingUtils.addSource(RegNext(instrCnt), "trapInstrCnt")

    if(EnableBPU){
      ExcitingUtils.addSource(hitTrap, "XSTRAP", ConnectionType.Debug)
    }
  }

  if (env.DualCoreDifftest) {
    difftestIO.commit := RegNext(retireCounterFix)
    difftestIO.thisPC := RegNext(retirePCFix)
    difftestIO.thisINST := RegNext(retireInstFix)
    difftestIO.skip := RegNext(skip.asUInt)
    difftestIO.wen := RegNext(wen.asUInt)
    difftestIO.wdata := RegNext(wdata)
    difftestIO.wdst := RegNext(wdst)
    difftestIO.wpc := RegNext(wpc)
    difftestIO.isRVC := RegNext(isRVC.asUInt)
    difftestIO.scFailed := RegNext(scFailed)

    trapIO.valid := RegNext(hitTrap)
    trapIO.code := RegNext(trapCode)
    trapIO.pc := RegNext(trapPC)
    trapIO.cycleCnt := RegNext(GTimer())
    trapIO.instrCnt := RegNext(instrCnt)
  }
}
