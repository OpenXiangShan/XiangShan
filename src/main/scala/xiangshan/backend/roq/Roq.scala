package xiangshan.backend.roq

import chisel3.ExcitingUtils._
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.backend.LSUOpType
import xiangshan.backend.fu.fpu.Fflags
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

  val fflags = Output(new Fflags)
  val dirty_fs = Output(Bool())
}

class RoqEnqIO extends XSBundle {
  val canAccept = Output(Bool())
  val isEmpty = Output(Bool())
  // valid vector, for roqIdx gen and walk
  val needAlloc = Vec(RenameWidth, Input(Bool()))
  val req = Vec(RenameWidth, Flipped(ValidIO(new MicroOp)))
  val resp = Vec(RenameWidth, Output(new RoqPtr))
}

class RoqDispatchData extends XSBundle {
  // commit info
  val ldest = UInt(5.W)
  val rfWen = Bool()
  val fpWen = Bool()
  val commitType = CommitType()
  val pdest = UInt(PhyRegIdxWidth.W)
  val old_pdest = UInt(PhyRegIdxWidth.W)
  val lqIdx = new LqPtr
  val sqIdx = new SqPtr

  // exception info
  val pc = UInt(VAddrBits.W)
  val crossPageIPFFix = Bool()
  val exceptionVec = Vec(16, Bool())
}

class RoqWbData extends XSBundle {
  // mostly for exceptions
  val exceptionVec = Vec(16, Bool())
  val fflags = new Fflags
  val flushPipe = Bool()
}

class RoqDeqPtrWrapper extends XSModule with HasCircularQueuePtrHelper {
  val io = IO(new Bundle {
    // for commits/flush
    val state = Input(UInt(2.W))
    val deq_v = Vec(CommitWidth, Input(Bool()))
    val deq_w = Vec(CommitWidth, Input(Bool()))
    val deq_exceptionVec = Vec(CommitWidth, Input(UInt(16.W)))
    val deq_flushPipe = Vec(CommitWidth, Input(Bool()))
    // for flush: when exception occurs, reset deqPtrs to range(0, CommitWidth)
    val intrBitSetReg = Input(Bool())
    val hasNoSpecExec = Input(Bool())
    val commitType = Input(CommitType())
    // output: the CommitWidth deqPtr
    val out = Vec(CommitWidth, Output(new RoqPtr))
  })

  val deqPtrVec = RegInit(VecInit((0 until CommitWidth).map(_.U.asTypeOf(new RoqPtr))))

  // for exceptions (flushPipe included) and interrupts:
  // only consider the first instruction
  val intrEnable = io.intrBitSetReg && !io.hasNoSpecExec && !CommitType.isLoadStore(io.commitType)
  val exceptionEnable = io.deq_w(0) && (io.deq_exceptionVec(0).orR || io.deq_flushPipe(0))
  val redirectOutValid = io.state === 0.U && io.deq_v(0) && (intrEnable || exceptionEnable)

  // for normal commits: only to consider when there're no exceptions
  // we don't need to consider whether the first instruction has exceptions since it wil trigger exceptions.
  val commitBlocked = VecInit((0 until CommitWidth).map(i => if (i == 0) false.B else io.deq_exceptionVec(i).orR || io.deq_flushPipe(i)))
  val canCommit = VecInit((0 until CommitWidth).map(i => io.deq_v(i) && io.deq_w(i) && !commitBlocked(i)))
  val normalCommitCnt = PriorityEncoder(canCommit.map(c => !c) :+ true.B)
  // when io.intrBitSetReg, only one instruction is allowed to commit
  val commitCnt = Mux(io.intrBitSetReg, io.deq_v(0) && io.deq_w(0), normalCommitCnt)

  when (redirectOutValid) {
    deqPtrVec := VecInit((0 until CommitWidth).map(_.U.asTypeOf(new RoqPtr)))
  }.elsewhen (io.state === 0.U) {
    deqPtrVec := deqPtrVec.map(_ + commitCnt)
    XSInfo(io.state === 0.U && commitCnt > 0.U, "retired %d insts\n", commitCnt)
  }

  io.out := deqPtrVec

}

class RoqEnqPtrWrapper extends XSModule with HasCircularQueuePtrHelper {
  val io = IO(new Bundle {
    // for exceptions and interrupts
    val state = Input(UInt(2.W))
    val deq_v = Input(Bool())
    val deq_w = Input(Bool())
    val deq_exceptionVec = Input(UInt(16.W))
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
  val exceptionEnable = io.deq_w && (io.deq_exceptionVec.orR || io.deq_flushPipe)
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
    val bcommit = Output(UInt(BrTagWidth.W))
    val roqDeqPtr = Output(new RoqPtr)
    val csr = new RoqCSRIO
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
  val dispatchData = Module(new DataModuleTemplate(new RoqDispatchData, RoqSize, CommitWidth, RenameWidth))
  val dispatchDataRead = dispatchData.io.rdata

  val writebackData = Module(new DataModuleTemplate(new RoqWbData, RoqSize, CommitWidth, numWbPorts))
  val writebackDataRead = writebackData.io.rdata

  def mergeExceptionVec(dpData: RoqDispatchData, wbData: RoqWbData) = {
    // these exceptions can be determined before dispatch.
    // by default, let all exceptions be determined by dispatch.
    // mergeVec(instrAddrMisaligned) := dpData(instrAddrMisaligned)
    // mergeVec(instrAccessFault) := dpData(instrAccessFault)
    // mergeVec(instrPageFault) := dpData(instrPageFault)
    val mergeVec = WireInit(dpData.exceptionVec)
    // these exceptions are determined in execution units
    mergeVec(illegalInstr) := wbData.exceptionVec(illegalInstr)
    mergeVec(breakPoint) := wbData.exceptionVec(breakPoint)
    mergeVec(loadAddrMisaligned) := wbData.exceptionVec(loadAddrMisaligned)
    mergeVec(loadAccessFault) := wbData.exceptionVec(loadAccessFault)
    mergeVec(storeAddrMisaligned) := wbData.exceptionVec(storeAddrMisaligned)
    mergeVec(storeAccessFault) := wbData.exceptionVec(storeAccessFault)
    mergeVec(ecallU) := wbData.exceptionVec(ecallU)
    mergeVec(ecallS) := wbData.exceptionVec(ecallS)
    mergeVec(ecallM) := wbData.exceptionVec(ecallM)
    mergeVec(loadPageFault) := wbData.exceptionVec(loadPageFault)
    mergeVec(storePageFault) := wbData.exceptionVec(storePageFault)
    // returns the merged exception vector
    mergeVec
  }

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
  io.enq.isEmpty   := isEmpty
  io.enq.resp := enqPtrVec
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
  // debug info for enqueue (dispatch)
  val dispatchNum = Mux(io.enq.canAccept, PopCount(Cat(io.enq.req.map(_.valid))), 0.U)
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

  val deqExceptionVec = mergeExceptionVec(deqDispatchData, deqWritebackData)
  // For MMIO instructions, they should not trigger interrupts since they may be sent to lower level before it writes back.
  // However, we cannot determine whether a load/store instruction is MMIO.
  // Thus, we don't allow load/store instructions to trigger an interrupt.
  val intrBitSetReg = RegNext(io.csr.intrBitSet)
  val intrEnable = intrBitSetReg && !hasNoSpecExec && !CommitType.isLoadStore(deqDispatchData.commitType)
  val exceptionEnable = writebacked(deqPtr.value) && Cat(deqExceptionVec).orR()
  val isFlushPipe = writebacked(deqPtr.value) && deqWritebackData.flushPipe
  io.redirectOut := DontCare
  io.redirectOut.valid := (state === s_idle) && valid(deqPtr.value) && (intrEnable || exceptionEnable || isFlushPipe)
  io.redirectOut.bits.level := Mux(isFlushPipe, RedirectLevel.flushAll, RedirectLevel.exception)
  io.redirectOut.bits.interrupt := intrEnable
  io.redirectOut.bits.target := Mux(isFlushPipe, deqDispatchData.pc + 4.U, io.csr.trapTarget)

  io.exception := debug_deqUop
  io.exception.ctrl.commitType := deqDispatchData.commitType
  io.exception.lqIdx := deqDispatchData.lqIdx
  io.exception.sqIdx := deqDispatchData.sqIdx
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
  val needExtraSpaceForMPR = VecInit((0 until CommitWidth).map(i => io.redirect.valid && io.enq.needAlloc(i)))
  val extraSpaceForMPR = Reg(Vec(RenameWidth, new RoqDispatchData))
  val usedSpaceForMPR = Reg(Vec(RenameWidth, Bool()))

  // wiring to csr
  val fflags = WireInit(0.U.asTypeOf(new Fflags))
  val dirty_fs = Mux(io.commits.isWalk, false.B, Cat(io.commits.valid.zip(io.commits.info.map(_.fpWen)).map{case (v, w) => v & w}).orR)

  io.commits.isWalk := state =/= s_idle
  val commit_v = Mux(state === s_idle, VecInit(deqPtrVec.map(ptr => valid(ptr.value))), VecInit(walkPtrVec.map(ptr => valid(ptr.value))))
  val commit_w = VecInit(deqPtrVec.map(ptr => writebacked(ptr.value)))
  val commit_exception = dispatchDataRead.zip(writebackDataRead).map{ case (d, w) => mergeExceptionVec(d, w).asUInt.orR }
  val commit_block = VecInit((0 until CommitWidth).map(i => !commit_w(i) || commit_exception(i) || writebackDataRead(i).flushPipe))
  for (i <- 0 until CommitWidth) {
    // defaults: state === s_idle and instructions commit
    // when intrBitSetReg, allow only one instruction to commit at each clock cycle
    val isBlocked = if (i != 0) Cat(commit_block.take(i)).orR || intrBitSetReg else intrEnable
    io.commits.valid(i) := commit_v(i) && commit_w(i) && !isBlocked && !commit_exception(i)
    io.commits.info(i)  := dispatchDataRead(i)

    when (state === s_idle) {
      when (io.commits.valid(i) && writebackDataRead(i).fflags.asUInt.orR()) {
        fflags := writebackDataRead(i).fflags
      }
    }

    when (state === s_walk) {
      io.commits.valid(i) := commit_v(i) && shouldWalkVec(i)
    }.elsewhen(state === s_extrawalk) {
      io.commits.valid(i) := usedSpaceForMPR(RenameWidth-i-1)
      io.commits.info(i)  := extraSpaceForMPR(RenameWidth-i-1)
      state := s_walk
    }

    XSInfo(state === s_idle && io.commits.valid(i),
      "retired pc %x wen %d ldest %d pdest %x old_pdest %x data %x fflags: %b\n",
      debug_microOp(deqPtrVec(i).value).cf.pc,
      io.commits.info(i).rfWen,
      io.commits.info(i).ldest,
      io.commits.info(i).pdest,
      io.commits.info(i).old_pdest,
      debug_exuData(deqPtrVec(i).value),
      writebackDataRead(i).fflags.asUInt
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

  io.csr.fflags := fflags
  io.csr.dirty_fs := dirty_fs
  // commit branch to brq
  val cfiCommitVec = VecInit(io.commits.valid.zip(io.commits.info.map(_.commitType)).map{case(v, t) => v && CommitType.isBranch(t)})
  io.bcommit := Mux(io.commits.isWalk, 0.U, PopCount(cfiCommitVec))


  /**
    * read and write of data modules
    */
  val commitReadAddr = Mux(state === s_idle, VecInit(deqPtrVec.map(_.value)), VecInit(walkPtrVec.map(_.value)))
  dispatchData.io.wen := canEnqueue
  dispatchData.io.waddr := enqPtrVec.map(_.value)
  dispatchData.io.wdata.zip(io.enq.req.map(_.bits)).map{ case (wdata, req) =>
    wdata.ldest := req.ctrl.ldest
    wdata.rfWen := req.ctrl.rfWen
    wdata.fpWen := req.ctrl.fpWen
    wdata.commitType := req.ctrl.commitType
    wdata.pdest := req.pdest
    wdata.old_pdest := req.old_pdest
    wdata.lqIdx := req.lqIdx
    wdata.sqIdx := req.sqIdx
    wdata.pc := req.cf.pc
    wdata.crossPageIPFFix := req.cf.crossPageIPFFix
    wdata.exceptionVec := req.cf.exceptionVec
  }
  dispatchData.io.raddr := commitReadAddr

  writebackData.io.wen := io.exeWbResults.map(_.valid)
  writebackData.io.waddr := io.exeWbResults.map(_.bits.uop.roqIdx.value)
  writebackData.io.wdata.zip(io.exeWbResults.map(_.bits)).map{ case (wdata, wb) =>
    wdata.exceptionVec := wb.uop.cf.exceptionVec
    wdata.fflags := wb.fflags
    wdata.flushPipe := wb.uop.ctrl.flushPipe
  }
  writebackData.io.raddr := commitReadAddr



  /**
    * state changes
    * (1) redirect: from s_valid to s_walk or s_extrawalk (depends on whether there're pending instructions in dispatch1)
    * (2) s_extrawalk to s_walk
    * (3) s_walk to s_idle: end of walking
    */
  //exit walk state when all roq entry is commited
  when (state === s_walk && walkFinished) {
    state := s_idle
  }
  // when redirect, walk back roq entries
  when (io.redirect.valid) {
    state := s_walk
  }
  // no enough space for walk, allocate extra space
  when (needExtraSpaceForMPR.asUInt.orR && io.redirect.valid) {
    usedSpaceForMPR := needExtraSpaceForMPR
    extraSpaceForMPR := dispatchData.io.wdata

    state := s_extrawalk
    XSDebug("roq full, switched to s_extrawalk. needExtraSpaceForMPR: %b\n", needExtraSpaceForMPR.asUInt)
  }
  // when exception occurs, cancels all and switch to s_idle
  when (io.redirectOut.valid) {
    state := s_idle
  }


  /**
    * pointers and counters
    */
  val deqPtrGenModule = Module(new RoqDeqPtrWrapper)
  deqPtrGenModule.io.state := state
  deqPtrGenModule.io.deq_v := commit_v
  deqPtrGenModule.io.deq_w := commit_w
  deqPtrGenModule.io.deq_exceptionVec := VecInit(dispatchDataRead.zip(writebackDataRead).map{ case (d, w) => mergeExceptionVec(d, w).asUInt })
  deqPtrGenModule.io.deq_flushPipe := writebackDataRead.map(_.flushPipe)
  deqPtrGenModule.io.intrBitSetReg := intrBitSetReg
  deqPtrGenModule.io.hasNoSpecExec := hasNoSpecExec
  deqPtrGenModule.io.commitType := deqDispatchData.commitType
  deqPtrVec := deqPtrGenModule.io.out

  val enqPtrGenModule = Module(new RoqEnqPtrWrapper)
  enqPtrGenModule.io.state := state
  enqPtrGenModule.io.deq_v := commit_v(0)
  enqPtrGenModule.io.deq_w := commit_w(0)
  enqPtrGenModule.io.deq_exceptionVec := deqExceptionVec.asUInt
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
  when (io.redirect.valid && state =/= s_extrawalk) {
    walkPtrVec := Mux(state === s_walk,
      VecInit(walkPtrVec.map(_ - thisCycleWalkCount)),
      VecInit((0 until CommitWidth).map(i => enqPtr - (i+1).U))
    )
  }.elsewhen (state === s_walk) {
    walkPtrVec := VecInit(walkPtrVec.map(_ - CommitWidth.U))
  }

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
      writebacked(enqPtrVec(i).value) := false.B
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

  val id = roqDebugId()
  val difftestIntrNO = WireInit(0.U(XLEN.W))
  val difftestCause = WireInit(0.U(XLEN.W))
  ExcitingUtils.addSink(difftestIntrNO, s"difftestIntrNOfromCSR$id")
  ExcitingUtils.addSink(difftestCause, s"difftestCausefromCSR$id")

  if(!env.FPGAPlatform) {

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
    for(i <- 0 until CommitWidth){
      // io.commits(i).valid
      val idx = deqPtrVec(i).value
      val uop = debug_microOp(idx)
      val DifftestSkipSC = false
      if(!DifftestSkipSC){
        skip(i) := debug_exuDebug(idx).isMMIO && io.commits.valid(i)
      }else{
        skip(i) := (
            debug_exuDebug(idx).isMMIO ||
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

    val scFailed = !diffTestDebugLrScValid(0) &&
      debug_deqUop.ctrl.fuType === FuType.mou &&
      (debug_deqUop.ctrl.fuOpType === LSUOpType.sc_d || debug_deqUop.ctrl.fuOpType === LSUOpType.sc_w)

    val instrCnt = RegInit(0.U(64.W))
    val retireCounter = Mux(state === s_idle, commitCnt, 0.U)
    instrCnt := instrCnt + retireCounter

    XSDebug(difftestIntrNO =/= 0.U, "difftest intrNO set %x\n", difftestIntrNO)
    val retireCounterFix = Mux(io.redirectOut.valid, 1.U, retireCounter)
    val retirePCFix = SignExt(Mux(io.redirectOut.valid, debug_deqUop.cf.pc, debug_microOp(firstValidCommit).cf.pc), XLEN)
    val retireInstFix = Mux(io.redirectOut.valid, debug_deqUop.cf.instr, debug_microOp(firstValidCommit).cf.instr)

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

    val hitTrap = trapVec.reduce(_||_)
    val trapCode = PriorityMux(wdata.zip(trapVec).map(x => x._2 -> x._1))
    val trapPC = SignExt(PriorityMux(wpc.zip(trapVec).map(x => x._2 ->x._1)), XLEN)

    ExcitingUtils.addSource(RegNext(hitTrap), "trapValid")
    ExcitingUtils.addSource(RegNext(trapCode), "trapCode")
    ExcitingUtils.addSource(RegNext(trapPC), "trapPC")
    ExcitingUtils.addSource(RegNext(GTimer()), "trapCycleCnt")
    ExcitingUtils.addSource(RegNext(instrCnt), "trapInstrCnt")
    ExcitingUtils.addSource(state === s_walk || state === s_extrawalk, "perfCntCondRoqWalk", Perf)

    if(EnableBPU){
      ExcitingUtils.addSource(hitTrap, "XSTRAP", ConnectionType.Debug)
    }
  }
}
