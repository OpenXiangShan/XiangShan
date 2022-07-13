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

package xiangshan.backend.rob

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import difftest._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import utils._
import xiangshan._
import xiangshan.backend.exu.ExuConfig
import xiangshan.frontend.FtqPtr

class RobPtr(implicit p: Parameters) extends CircularQueuePtr[RobPtr](
  p => p(XSCoreParamsKey).RobSize
) with HasCircularQueuePtrHelper {

  def needFlush(redirect: Valid[Redirect]): Bool = {
    val flushItself = redirect.bits.flushItself() && this === redirect.bits.robIdx
    redirect.valid && (flushItself || isAfter(this, redirect.bits.robIdx))
  }

  def needFlush(redirect: Seq[Valid[Redirect]]): Bool = VecInit(redirect.map(needFlush)).asUInt.orR
}

object RobPtr {
  def apply(f: Bool, v: UInt)(implicit p: Parameters): RobPtr = {
    val ptr = Wire(new RobPtr)
    ptr.flag := f
    ptr.value := v
    ptr
  }
}

class RobCSRIO(implicit p: Parameters) extends XSBundle {
  val intrBitSet = Input(Bool())
  val trapTarget = Input(UInt(VAddrBits.W))
  val isXRet     = Input(Bool())
  val wfiEvent   = Input(Bool())

  val fflags     = Output(Valid(UInt(5.W)))
  val dirty_fs   = Output(Bool())
  val perfinfo   = new Bundle {
    val retiredInstr = Output(UInt(3.W))
  }
}

class RobLsqIO(implicit p: Parameters) extends XSBundle {
  val lcommit = Output(UInt(log2Up(CommitWidth + 1).W))
  val scommit = Output(UInt(log2Up(CommitWidth + 1).W))
  val pendingld = Output(Bool())
  val pendingst = Output(Bool())
  val commit = Output(Bool())
}

class RobEnqIO(implicit p: Parameters) extends XSBundle {
  val canAccept = Output(Bool())
  val isEmpty = Output(Bool())
  // valid vector, for robIdx gen and walk
  val needAlloc = Vec(RenameWidth, Input(Bool()))
  val req = Vec(RenameWidth, Flipped(ValidIO(new MicroOp)))
  val resp = Vec(RenameWidth, Output(new RobPtr))
}

class RobDispatchData(implicit p: Parameters) extends RobCommitInfo

class RobDeqPtrWrapper(implicit p: Parameters) extends XSModule with HasCircularQueuePtrHelper {
  val io = IO(new Bundle {
    // for commits/flush
    val state = Input(UInt(2.W))
    val deq_v = Vec(CommitWidth, Input(Bool()))
    val deq_w = Vec(CommitWidth, Input(Bool()))
    val exception_state = Flipped(ValidIO(new RobExceptionInfo))
    // for flush: when exception occurs, reset deqPtrs to range(0, CommitWidth)
    val intrBitSetReg = Input(Bool())
    val hasNoSpecExec = Input(Bool())
    val interrupt_safe = Input(Bool())
    val misPredBlock = Input(Bool())
    val isReplaying = Input(Bool())
    val hasWFI = Input(Bool())
    // output: the CommitWidth deqPtr
    val out = Vec(CommitWidth, Output(new RobPtr))
    val next_out = Vec(CommitWidth, Output(new RobPtr))
  })

  val deqPtrVec = RegInit(VecInit((0 until CommitWidth).map(_.U.asTypeOf(new RobPtr))))

  // for exceptions (flushPipe included) and interrupts:
  // only consider the first instruction
  val intrEnable = io.intrBitSetReg && !io.hasNoSpecExec && io.interrupt_safe
  val exceptionEnable = io.deq_w(0) && io.exception_state.valid && io.exception_state.bits.not_commit && io.exception_state.bits.robIdx === deqPtrVec(0)
  val redirectOutValid = io.state === 0.U && io.deq_v(0) && (intrEnable || exceptionEnable)

  // for normal commits: only to consider when there're no exceptions
  // we don't need to consider whether the first instruction has exceptions since it wil trigger exceptions.
  val commit_exception = io.exception_state.valid && !isAfter(io.exception_state.bits.robIdx, deqPtrVec.last)
  val canCommit = VecInit((0 until CommitWidth).map(i => io.deq_v(i) && io.deq_w(i) && !io.misPredBlock && !io.isReplaying && !io.hasWFI))
  val normalCommitCnt = PriorityEncoder(canCommit.map(c => !c) :+ true.B)
  // when io.intrBitSetReg or there're possible exceptions in these instructions,
  // only one instruction is allowed to commit
  val allowOnlyOne = commit_exception || io.intrBitSetReg
  val commitCnt = Mux(allowOnlyOne, canCommit(0), normalCommitCnt)

  val commitDeqPtrVec = VecInit(deqPtrVec.map(_ + commitCnt))
  val deqPtrVec_next = Mux(io.state === 0.U && !redirectOutValid, commitDeqPtrVec, deqPtrVec)

  deqPtrVec := deqPtrVec_next

  io.next_out := deqPtrVec_next
  io.out      := deqPtrVec

  when (io.state === 0.U) {
    XSInfo(io.state === 0.U && commitCnt > 0.U, "retired %d insts\n", commitCnt)
  }

}

class RobEnqPtrWrapper(implicit p: Parameters) extends XSModule with HasCircularQueuePtrHelper {
  val io = IO(new Bundle {
    // for input redirect
    val redirect = Input(Valid(new Redirect))
    // for enqueue
    val allowEnqueue = Input(Bool())
    val hasBlockBackward = Input(Bool())
    val enq = Vec(RenameWidth, Input(Bool()))
    val out = Output(Vec(RenameWidth, new RobPtr))
  })

  val enqPtrVec = RegInit(VecInit.tabulate(RenameWidth)(_.U.asTypeOf(new RobPtr)))

  // enqueue
  val canAccept = io.allowEnqueue && !io.hasBlockBackward
  val dispatchNum = Mux(canAccept, PopCount(io.enq), 0.U)

  for ((ptr, i) <- enqPtrVec.zipWithIndex) {
    when(io.redirect.valid) {
      ptr := Mux(io.redirect.bits.flushItself(), io.redirect.bits.robIdx + i.U, io.redirect.bits.robIdx + (i + 1).U)
    }.otherwise {
      ptr := ptr + dispatchNum
    }
  }

  io.out := enqPtrVec

}

class RobExceptionInfo(implicit p: Parameters) extends XSBundle {
  // val valid = Bool()
  val robIdx = new RobPtr
  val exceptionVec = ExceptionVec()
  val flushPipe = Bool()
  val replayInst = Bool() // redirect to that inst itself
  val singleStep = Bool() // TODO add frontend hit beneath
  val crossPageIPFFix = Bool()
  val trigger = new TriggerCf

//  def trigger_before = !trigger.getTimingBackend && trigger.getHitBackend
//  def trigger_after = trigger.getTimingBackend && trigger.getHitBackend
  def has_exception = exceptionVec.asUInt.orR || flushPipe || singleStep || replayInst || trigger.hit
  def not_commit = exceptionVec.asUInt.orR || singleStep || replayInst || trigger.hit
  // only exceptions are allowed to writeback when enqueue
  def can_writeback = exceptionVec.asUInt.orR || singleStep || trigger.hit
}

class ExceptionGen(implicit p: Parameters) extends XSModule with HasCircularQueuePtrHelper {
  val io = IO(new Bundle {
    val redirect = Input(Valid(new Redirect))
    val flush = Input(Bool())
    val enq = Vec(RenameWidth, Flipped(ValidIO(new RobExceptionInfo)))
    val wb = Vec(1 + LoadPipelineWidth + StorePipelineWidth, Flipped(ValidIO(new RobExceptionInfo)))
    val out = ValidIO(new RobExceptionInfo)
    val state = ValidIO(new RobExceptionInfo)
  })

  def getOldest(valid: Seq[Bool], bits: Seq[RobExceptionInfo]): (Seq[Bool], Seq[RobExceptionInfo]) = {
    assert(valid.length == bits.length)
    assert(isPow2(valid.length))
    if (valid.length == 1) {
      (valid, bits)
    } else if (valid.length == 2) {
      val res = Seq.fill(2)(Wire(ValidIO(chiselTypeOf(bits(0)))))
      for (i <- res.indices) {
        res(i).valid := valid(i)
        res(i).bits := bits(i)
      }
      val oldest = Mux(!valid(1) || valid(0) && isAfter(bits(1).robIdx, bits(0).robIdx), res(0), res(1))
      (Seq(oldest.valid), Seq(oldest.bits))
    } else {
      val left = getOldest(valid.take(valid.length / 2), bits.take(valid.length / 2))
      val right = getOldest(valid.takeRight(valid.length / 2), bits.takeRight(valid.length / 2))
      getOldest(left._1 ++ right._1, left._2 ++ right._2)
    }
  }

  val current = Reg(Valid(new RobExceptionInfo))

  // orR the exceptionVec
  val lastCycleFlush = RegNext(io.flush)
  val in_enq_valid = VecInit(io.enq.map(e => e.valid && e.bits.has_exception && !lastCycleFlush))
  val in_wb_valid = io.wb.map(w => w.valid && w.bits.has_exception && !lastCycleFlush)

  // s0: compare wb(1)~wb(LoadPipelineWidth) and wb(1 + LoadPipelineWidth)~wb(LoadPipelineWidth + StorePipelineWidth)
  val wb_valid = in_wb_valid.zip(io.wb.map(_.bits)).map{ case (v, bits) => v && !(bits.robIdx.needFlush(io.redirect) || io.flush) }
  val csr_wb_bits = io.wb(0).bits
  val load_wb_bits = getOldest(in_wb_valid.slice(1, 1 + LoadPipelineWidth), io.wb.map(_.bits).slice(1, 1 + LoadPipelineWidth))._2(0)
  val store_wb_bits = getOldest(in_wb_valid.slice(1 + LoadPipelineWidth, 1 + LoadPipelineWidth + StorePipelineWidth), io.wb.map(_.bits).slice(1 + LoadPipelineWidth, 1 + LoadPipelineWidth + StorePipelineWidth))._2(0)
  val s0_out_valid = RegNext(VecInit(Seq(wb_valid(0), wb_valid.slice(1, 1 + LoadPipelineWidth).reduce(_ || _), wb_valid.slice(1 + LoadPipelineWidth, 1 + LoadPipelineWidth + StorePipelineWidth).reduce(_ || _))))
  val s0_out_bits = RegNext(VecInit(Seq(csr_wb_bits, load_wb_bits, store_wb_bits)))

  // s1: compare last four and current flush
  val s1_valid = VecInit(s0_out_valid.zip(s0_out_bits).map{ case (v, b) => v && !(b.robIdx.needFlush(io.redirect) || io.flush) })
  val compare_01_valid = s0_out_valid(0) || s0_out_valid(1)
  val compare_01_bits = Mux(!s0_out_valid(0) || s0_out_valid(1) && isAfter(s0_out_bits(0).robIdx, s0_out_bits(1).robIdx), s0_out_bits(1), s0_out_bits(0))
  val compare_bits = Mux(!s0_out_valid(2) || compare_01_valid && isAfter(s0_out_bits(2).robIdx, compare_01_bits.robIdx), compare_01_bits, s0_out_bits(2))
  val s1_out_bits = RegNext(compare_bits)
  val s1_out_valid = RegNext(s1_valid.asUInt.orR)

  val enq_valid = RegNext(in_enq_valid.asUInt.orR && !io.redirect.valid && !io.flush)
  val enq_bits = RegNext(ParallelPriorityMux(in_enq_valid, io.enq.map(_.bits)))

  // s2: compare the input exception with the current one
  // priorities:
  // (1) system reset
  // (2) current is valid: flush, remain, merge, update
  // (3) current is not valid: s1 or enq
  val current_flush = current.bits.robIdx.needFlush(io.redirect) || io.flush
  val s1_flush = s1_out_bits.robIdx.needFlush(io.redirect) || io.flush
  when (reset.asBool) {
    current.valid := false.B
  }.elsewhen (current.valid) {
    when (current_flush) {
      current.valid := Mux(s1_flush, false.B, s1_out_valid)
    }
    when (s1_out_valid && !s1_flush) {
      when (isAfter(current.bits.robIdx, s1_out_bits.robIdx)) {
        current.bits := s1_out_bits
      }.elsewhen (current.bits.robIdx === s1_out_bits.robIdx) {
        current.bits.exceptionVec := (s1_out_bits.exceptionVec.asUInt | current.bits.exceptionVec.asUInt).asTypeOf(ExceptionVec())
        current.bits.flushPipe := s1_out_bits.flushPipe || current.bits.flushPipe
        current.bits.replayInst := s1_out_bits.replayInst || current.bits.replayInst
        current.bits.singleStep := s1_out_bits.singleStep || current.bits.singleStep
        current.bits.trigger := (s1_out_bits.trigger.asUInt | current.bits.trigger.asUInt).asTypeOf(new TriggerCf)
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

class RobFlushInfo(implicit p: Parameters) extends XSBundle {
  val ftqIdx = new FtqPtr
  val robIdx = new RobPtr
  val ftqOffset = UInt(log2Up(PredictWidth).W)
  val replayInst = Bool()
}

class Rob(implicit p: Parameters) extends LazyModule with HasWritebackSink with HasXSParameter {

  lazy val module = new RobImp(this)

  override def generateWritebackIO(
    thisMod: Option[HasWritebackSource] = None,
    thisModImp: Option[HasWritebackSourceImp] = None
  ): Unit = {
    val sources = writebackSinksImp(thisMod, thisModImp)
    module.io.writeback.zip(sources).foreach(x => x._1 := x._2)
  }
}

class RobImp(outer: Rob)(implicit p: Parameters) extends LazyModuleImp(outer)
  with HasXSParameter with HasCircularQueuePtrHelper with HasPerfEvents {
  val wbExuConfigs = outer.writebackSinksParams.map(_.exuConfigs)
  val numWbPorts = wbExuConfigs.map(_.length)

  val io = IO(new Bundle() {
    val hartId = Input(UInt(8.W))
    val redirect = Input(Valid(new Redirect))
    val enq = new RobEnqIO
    val flushOut = ValidIO(new Redirect)
    val exception = ValidIO(new ExceptionInfo)
    // exu + brq
    val writeback = MixedVec(numWbPorts.map(num => Vec(num, Flipped(ValidIO(new ExuOutput)))))
    val commits = new RobCommitIO
    val lsq = new RobLsqIO
    val robDeqPtr = Output(new RobPtr)
    val csr = new RobCSRIO
    val robFull = Output(Bool())
    val cpu_halt = Output(Bool())
  })

  def selectWb(index: Int, func: Seq[ExuConfig] => Boolean): Seq[(Seq[ExuConfig], ValidIO[ExuOutput])] = {
    wbExuConfigs(index).zip(io.writeback(index)).filter(x => func(x._1))
  }
  val exeWbSel = outer.selWritebackSinks(_.exuConfigs.length)
  val fflagsWbSel = outer.selWritebackSinks(_.exuConfigs.count(_.exists(_.writeFflags)))
  val fflagsPorts = selectWb(fflagsWbSel, _.exists(_.writeFflags))
  val exceptionWbSel = outer.selWritebackSinks(_.exuConfigs.count(_.exists(_.needExceptionGen)))
  val exceptionPorts = selectWb(fflagsWbSel, _.exists(_.needExceptionGen))
  val exuWbPorts = selectWb(exeWbSel, _.forall(_ != StdExeUnitCfg))
  val stdWbPorts = selectWb(exeWbSel, _.contains(StdExeUnitCfg))
  println(s"Rob: size $RobSize, numWbPorts: $numWbPorts, commitwidth: $CommitWidth")
  println(s"exuPorts: ${exuWbPorts.map(_._1.map(_.name))}")
  println(s"stdPorts: ${stdWbPorts.map(_._1.map(_.name))}")
  println(s"fflags: ${fflagsPorts.map(_._1.map(_.name))}")


  val exuWriteback = exuWbPorts.map(_._2)
  val stdWriteback = stdWbPorts.map(_._2)

  // instvalid field
  val valid = Mem(RobSize, Bool())
  // writeback status
  val writebacked = Mem(RobSize, Bool())
  val store_data_writebacked = Mem(RobSize, Bool())
  // data for redirect, exception, etc.
  val flagBkup = Mem(RobSize, Bool())
  // some instructions are not allowed to trigger interrupts
  // They have side effects on the states of the processor before they write back
  val interrupt_safe = Mem(RobSize, Bool())

  // data for debug
  // Warn: debug_* prefix should not exist in generated verilog.
  val debug_microOp = Mem(RobSize, new MicroOp)
  val debug_exuData = Reg(Vec(RobSize, UInt(XLEN.W)))//for debug
  val debug_exuDebug = Reg(Vec(RobSize, new DebugBundle))//for debug

  // pointers
  // For enqueue ptr, we don't duplicate it since only enqueue needs it.
  val enqPtrVec = Wire(Vec(RenameWidth, new RobPtr))
  val deqPtrVec = Wire(Vec(CommitWidth, new RobPtr))

  val walkPtrVec = Reg(Vec(CommitWidth, new RobPtr))
  val validCounter = RegInit(0.U(log2Ceil(RobSize + 1).W))
  val allowEnqueue = RegInit(true.B)

  val enqPtr = enqPtrVec.head
  val deqPtr = deqPtrVec(0)
  val walkPtr = walkPtrVec(0)

  val isEmpty = enqPtr === deqPtr
  val isReplaying = io.redirect.valid && RedirectLevel.flushItself(io.redirect.bits.level)

  /**
    * states of Rob
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
  val dispatchData = Module(new SyncDataModuleTemplate(new RobDispatchData, RobSize, CommitWidth, RenameWidth))
  val dispatchDataRead = dispatchData.io.rdata

  val exceptionGen = Module(new ExceptionGen)
  val exceptionDataRead = exceptionGen.io.state
  val fflagsDataRead = Wire(Vec(CommitWidth, UInt(5.W)))

  io.robDeqPtr := deqPtr

  /**
    * Enqueue (from dispatch)
    */
  // special cases
  val hasBlockBackward = RegInit(false.B)
  val hasNoSpecExec = RegInit(false.B)
  val doingSvinval = RegInit(false.B)
  // When blockBackward instruction leaves Rob (commit or walk), hasBlockBackward should be set to false.B
  // To reduce registers usage, for hasBlockBackward cases, we allow enqueue after ROB is empty.
  when (isEmpty) { hasBlockBackward:= false.B }
  // When any instruction commits, hasNoSpecExec should be set to false.B
  when ((io.commits.hasWalkInstr && state =/= s_extrawalk) || io.commits.hasCommitInstr) { hasNoSpecExec:= false.B }

  // The wait-for-interrupt (WFI) instruction waits in the ROB until an interrupt might need servicing.
  // io.csr.wfiEvent will be asserted if the WFI can resume execution, and we change the state to s_wfi_idle.
  // It does not affect how interrupts are serviced. Note that WFI is noSpecExec and it does not trigger interrupts.
  val hasWFI = RegInit(false.B)
  io.cpu_halt := hasWFI
  when (RegNext(RegNext(io.csr.wfiEvent))) {
    hasWFI := false.B
  }

  val allocatePtrVec = VecInit((0 until RenameWidth).map(i => enqPtrVec(PopCount(io.enq.needAlloc.take(i)))))
  io.enq.canAccept := allowEnqueue && !hasBlockBackward
  io.enq.resp      := allocatePtrVec
  val canEnqueue = VecInit(io.enq.req.map(_.valid && io.enq.canAccept))
  val timer = GTimer()
  for (i <- 0 until RenameWidth) {
    // we don't check whether io.redirect is valid here since redirect has higher priority
    when (canEnqueue(i)) {
      val enqUop = io.enq.req(i).bits
      val enqIndex = allocatePtrVec(i).value
      // store uop in data module and debug_microOp Vec
      debug_microOp(enqIndex) := enqUop
      debug_microOp(enqIndex).debugInfo.dispatchTime := timer
      debug_microOp(enqIndex).debugInfo.enqRsTime := timer
      debug_microOp(enqIndex).debugInfo.selectTime := timer
      debug_microOp(enqIndex).debugInfo.issueTime := timer
      debug_microOp(enqIndex).debugInfo.writebackTime := timer
      when (enqUop.ctrl.blockBackward) {
        hasBlockBackward := true.B
      }
      when (enqUop.ctrl.noSpecExec) {
        hasNoSpecExec := true.B
      }
      val enqHasTriggerHit = io.enq.req(i).bits.cf.trigger.getHitFrontend
      val enqHasException = ExceptionNO.selectFrontend(enqUop.cf.exceptionVec).asUInt.orR
      // the begin instruction of Svinval enqs so mark doingSvinval as true to indicate this process
      when(!enqHasTriggerHit && !enqHasException && FuType.isSvinvalBegin(enqUop.ctrl.fuType, enqUop.ctrl.fuOpType, enqUop.ctrl.flushPipe))
      {
        doingSvinval := true.B
      }
      // the end instruction of Svinval enqs so clear doingSvinval
      when(!enqHasTriggerHit && !enqHasException && FuType.isSvinvalEnd(enqUop.ctrl.fuType, enqUop.ctrl.fuOpType, enqUop.ctrl.flushPipe))
      {
        doingSvinval := false.B
      }
      // when we are in the process of Svinval software code area , only Svinval.vma and end instruction of Svinval can appear
      assert(!doingSvinval || (FuType.isSvinval(enqUop.ctrl.fuType, enqUop.ctrl.fuOpType, enqUop.ctrl.flushPipe) ||
        FuType.isSvinvalEnd(enqUop.ctrl.fuType, enqUop.ctrl.fuOpType, enqUop.ctrl.flushPipe)))
      when (enqUop.ctrl.isWFI && !enqHasException && !enqHasTriggerHit) {
        hasWFI := true.B
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
  for (wb <- exuWriteback) {
    when (wb.valid) {
      val wbIdx = wb.bits.uop.robIdx.value
      debug_exuData(wbIdx) := wb.bits.data
      debug_exuDebug(wbIdx) := wb.bits.debug
      debug_microOp(wbIdx).debugInfo.enqRsTime := wb.bits.uop.debugInfo.enqRsTime
      debug_microOp(wbIdx).debugInfo.selectTime := wb.bits.uop.debugInfo.selectTime
      debug_microOp(wbIdx).debugInfo.issueTime := wb.bits.uop.debugInfo.issueTime
      debug_microOp(wbIdx).debugInfo.writebackTime := wb.bits.uop.debugInfo.writebackTime

      val debug_Uop = debug_microOp(wbIdx)
      XSInfo(true.B,
        p"writebacked pc 0x${Hexadecimal(debug_Uop.cf.pc)} wen ${debug_Uop.ctrl.rfWen} " +
        p"data 0x${Hexadecimal(wb.bits.data)} ldst ${debug_Uop.ctrl.ldest} pdst ${debug_Uop.pdest} " +
        p"skip ${wb.bits.debug.isMMIO} robIdx: ${wb.bits.uop.robIdx}\n"
      )
    }
  }
  val writebackNum = PopCount(exuWriteback.map(_.valid))
  XSInfo(writebackNum =/= 0.U, "writebacked %d insts\n", writebackNum)


  /**
    * RedirectOut: Interrupt and Exceptions
    */
  val deqDispatchData = dispatchDataRead(0)
  val debug_deqUop = debug_microOp(deqPtr.value)

  val intrBitSetReg = RegNext(io.csr.intrBitSet)
  val intrEnable = intrBitSetReg && !hasNoSpecExec && interrupt_safe(deqPtr.value)
  val deqHasExceptionOrFlush = exceptionDataRead.valid && exceptionDataRead.bits.robIdx === deqPtr
  val deqHasException = deqHasExceptionOrFlush && (exceptionDataRead.bits.exceptionVec.asUInt.orR ||
    exceptionDataRead.bits.singleStep || exceptionDataRead.bits.trigger.hit)
  val deqHasFlushPipe = deqHasExceptionOrFlush && exceptionDataRead.bits.flushPipe
  val deqHasReplayInst = deqHasExceptionOrFlush && exceptionDataRead.bits.replayInst
  val exceptionEnable = writebacked(deqPtr.value) && deqHasException

  XSDebug(deqHasException && exceptionDataRead.bits.singleStep, "Debug Mode: Deq has singlestep exception\n")
  XSDebug(deqHasException && exceptionDataRead.bits.trigger.getHitFrontend, "Debug Mode: Deq has frontend trigger exception\n")
  XSDebug(deqHasException && exceptionDataRead.bits.trigger.getHitBackend, "Debug Mode: Deq has backend trigger exception\n")

  val isFlushPipe = writebacked(deqPtr.value) && (deqHasFlushPipe || deqHasReplayInst)

  // io.flushOut will trigger redirect at the next cycle.
  // Block any redirect or commit at the next cycle.
  val lastCycleFlush = RegNext(io.flushOut.valid)

  io.flushOut.valid := (state === s_idle) && valid(deqPtr.value) && (intrEnable || exceptionEnable || isFlushPipe) && !lastCycleFlush
  io.flushOut.bits := DontCare
  io.flushOut.bits.robIdx := deqPtr
  io.flushOut.bits.ftqIdx := deqDispatchData.ftqIdx
  io.flushOut.bits.ftqOffset := deqDispatchData.ftqOffset
  io.flushOut.bits.level := Mux(deqHasReplayInst || intrEnable || exceptionEnable, RedirectLevel.flush, RedirectLevel.flushAfter) // TODO use this to implement "exception next"
  io.flushOut.bits.interrupt := true.B
  XSPerfAccumulate("interrupt_num", io.flushOut.valid && intrEnable)
  XSPerfAccumulate("exception_num", io.flushOut.valid && exceptionEnable)
  XSPerfAccumulate("flush_pipe_num", io.flushOut.valid && isFlushPipe)
  XSPerfAccumulate("replay_inst_num", io.flushOut.valid && isFlushPipe && deqHasReplayInst)

  val exceptionHappen = (state === s_idle) && valid(deqPtr.value) && (intrEnable || exceptionEnable) && !lastCycleFlush
  io.exception.valid := RegNext(exceptionHappen)
  io.exception.bits.uop := RegEnable(debug_deqUop, exceptionHappen)
  io.exception.bits.uop.ctrl.commitType := RegEnable(deqDispatchData.commitType, exceptionHappen)
  io.exception.bits.uop.cf.exceptionVec := RegEnable(exceptionDataRead.bits.exceptionVec, exceptionHappen)
  io.exception.bits.uop.ctrl.singleStep := RegEnable(exceptionDataRead.bits.singleStep, exceptionHappen)
  io.exception.bits.uop.cf.crossPageIPFFix := RegEnable(exceptionDataRead.bits.crossPageIPFFix, exceptionHappen)
  io.exception.bits.isInterrupt := RegEnable(intrEnable, exceptionHappen)
  io.exception.bits.uop.cf.trigger := RegEnable(exceptionDataRead.bits.trigger, exceptionHappen)

  XSDebug(io.flushOut.valid,
    p"generate redirect: pc 0x${Hexadecimal(io.exception.bits.uop.cf.pc)} intr $intrEnable " +
    p"excp $exceptionEnable flushPipe $isFlushPipe " +
    p"Trap_target 0x${Hexadecimal(io.csr.trapTarget)} exceptionVec ${Binary(exceptionDataRead.bits.exceptionVec.asUInt)}\n")


  /**
    * Commits (and walk)
    * They share the same width.
    */
  val walkCounter = Reg(UInt(log2Up(RobSize + 1).W))
  val shouldWalkVec = VecInit((0 until CommitWidth).map(_.U < walkCounter))
  val walkFinished = walkCounter <= CommitWidth.U

  // extra space is used when rob has no enough space, but mispredict recovery needs such info to walk regmap
  require(RenameWidth <= CommitWidth)
  val extraSpaceForMPR = Reg(Vec(RenameWidth, new RobDispatchData))
  val usedSpaceForMPR = Reg(Vec(RenameWidth, Bool()))
  when (io.enq.needAlloc.asUInt.orR && io.redirect.valid) {
    usedSpaceForMPR := io.enq.needAlloc
    extraSpaceForMPR := dispatchData.io.wdata
    XSDebug("rob full, switched to s_extrawalk. needExtraSpaceForMPR: %b\n", io.enq.needAlloc.asUInt)
  }

  // wiring to csr
  val (wflags, fpWen) = (0 until CommitWidth).map(i => {
    val v = io.commits.commitValid(i)
    val info = io.commits.info(i)
    (v & info.wflags, v & info.fpWen)
  }).unzip
  val fflags = Wire(Valid(UInt(5.W)))
  fflags.valid := io.commits.isCommit && VecInit(wflags).asUInt.orR
  fflags.bits := wflags.zip(fflagsDataRead).map({
    case (w, f) => Mux(w, f, 0.U)
  }).reduce(_|_)
  val dirty_fs = io.commits.isCommit && VecInit(fpWen).asUInt.orR

  // when mispredict branches writeback, stop commit in the next 2 cycles
  // TODO: don't check all exu write back
  val misPredWb = Cat(VecInit(exuWriteback.map(wb =>
    wb.bits.redirect.cfiUpdate.isMisPred && wb.bits.redirectValid
  ))).orR
  val misPredBlockCounter = Reg(UInt(3.W))
  misPredBlockCounter := Mux(misPredWb,
    "b111".U,
    misPredBlockCounter >> 1.U
  )
  val misPredBlock = misPredBlockCounter(0)

  io.commits.isWalk := state =/= s_idle
  io.commits.isCommit := state === s_idle && !misPredBlock && !isReplaying && !lastCycleFlush && !hasWFI
  val walk_v = VecInit(walkPtrVec.map(ptr => valid(ptr.value)))
  val commit_v = VecInit(deqPtrVec.map(ptr => valid(ptr.value)))
  // store will be commited iff both sta & std have been writebacked
  val commit_w = VecInit(deqPtrVec.map(ptr => writebacked(ptr.value) && store_data_writebacked(ptr.value)))
  val commit_exception = exceptionDataRead.valid && !isAfter(exceptionDataRead.bits.robIdx, deqPtrVec.last)
  val commit_block = VecInit((0 until CommitWidth).map(i => !commit_w(i)))
  val allowOnlyOneCommit = commit_exception || intrBitSetReg
  // for instructions that may block others, we don't allow them to commit
  for (i <- 0 until CommitWidth) {
    // defaults: state === s_idle and instructions commit
    // when intrBitSetReg, allow only one instruction to commit at each clock cycle
    val isBlocked = if (i != 0) Cat(commit_block.take(i)).orR || allowOnlyOneCommit else intrEnable || deqHasException || deqHasReplayInst
    io.commits.commitValid(i) := commit_v(i) && commit_w(i) && !isBlocked
    io.commits.info(i)  := dispatchDataRead(i)

    io.commits.walkValid(i) := shouldWalkVec(i)
    when (io.commits.isWalk && state === s_walk && shouldWalkVec(i)) {
      XSError(!walk_v(i), s"why not $i???\n")
    }
    when (state === s_extrawalk) {
      if (i < RenameWidth) {
        io.commits.walkValid(i) := usedSpaceForMPR(RenameWidth - i - 1)
        io.commits.info(i) := extraSpaceForMPR(RenameWidth - i - 1)
      }
      else {
        io.commits.walkValid(i) := false.B
      }
    }

    XSInfo(io.commits.isCommit && io.commits.commitValid(i),
      "retired pc %x wen %d ldest %d pdest %x old_pdest %x data %x fflags: %b\n",
      debug_microOp(deqPtrVec(i).value).cf.pc,
      io.commits.info(i).rfWen,
      io.commits.info(i).ldest,
      io.commits.info(i).pdest,
      io.commits.info(i).old_pdest,
      debug_exuData(deqPtrVec(i).value),
      fflagsDataRead(i)
    )
    XSInfo(state === s_walk && io.commits.walkValid(i), "walked pc %x wen %d ldst %d data %x\n",
      debug_microOp(walkPtrVec(i).value).cf.pc,
      io.commits.info(i).rfWen,
      io.commits.info(i).ldest,
      debug_exuData(walkPtrVec(i).value)
    )
    XSInfo(state === s_extrawalk && io.commits.walkValid(i), "use extra space walked wen %d ldst %d\n",
      io.commits.info(i).rfWen,
      io.commits.info(i).ldest
    )
  }
  if (env.EnableDifftest) {
    io.commits.info.map(info => dontTouch(info.pc))
  }

  // sync fflags/dirty_fs to csr
  io.csr.fflags := RegNext(fflags)
  io.csr.dirty_fs := RegNext(dirty_fs)

  // commit load/store to lsq
  val ldCommitVec = VecInit((0 until CommitWidth).map(i => io.commits.commitValid(i) && io.commits.info(i).commitType === CommitType.LOAD))
  val stCommitVec = VecInit((0 until CommitWidth).map(i => io.commits.commitValid(i) && io.commits.info(i).commitType === CommitType.STORE))
  io.lsq.lcommit := RegNext(Mux(io.commits.isCommit, PopCount(ldCommitVec), 0.U))
  io.lsq.scommit := RegNext(Mux(io.commits.isCommit, PopCount(stCommitVec), 0.U))
  // indicate a pending load or store
  io.lsq.pendingld := RegNext(io.commits.isCommit && io.commits.info(0).commitType === CommitType.LOAD && valid(deqPtr.value))
  io.lsq.pendingst := RegNext(io.commits.isCommit && io.commits.info(0).commitType === CommitType.STORE && valid(deqPtr.value))
  io.lsq.commit := RegNext(io.commits.isCommit && io.commits.commitValid(0))

  /**
    * state changes
    * (1) exceptions: when exception occurs, cancels all and switch to s_idle
    * (2) redirect: switch to s_walk or s_extrawalk (depends on whether there're pending instructions in dispatch1)
    * (3) walk: when walking comes to the end, switch to s_walk
    * (4) s_extrawalk to s_walk
    */
  val state_next = Mux(io.redirect.valid,
    Mux(io.enq.needAlloc.asUInt.orR, s_extrawalk, s_walk),
    Mux(state === s_walk && walkFinished,
      s_idle,
      Mux(state === s_extrawalk,
        Mux(walkCounter === 0.U, s_idle, s_walk),
        state
      )
    )
  )
  state := state_next

  /**
    * pointers and counters
    */
  val deqPtrGenModule = Module(new RobDeqPtrWrapper)
  deqPtrGenModule.io.state := state
  deqPtrGenModule.io.deq_v := commit_v
  deqPtrGenModule.io.deq_w := commit_w
  deqPtrGenModule.io.exception_state := exceptionDataRead
  deqPtrGenModule.io.intrBitSetReg := intrBitSetReg
  deqPtrGenModule.io.hasNoSpecExec := hasNoSpecExec
  deqPtrGenModule.io.interrupt_safe := interrupt_safe(deqPtr.value)
  deqPtrGenModule.io.misPredBlock := misPredBlock
  deqPtrGenModule.io.isReplaying := isReplaying
  deqPtrGenModule.io.hasWFI := hasWFI
  deqPtrVec := deqPtrGenModule.io.out
  val deqPtrVec_next = deqPtrGenModule.io.next_out

  val enqPtrGenModule = Module(new RobEnqPtrWrapper)
  enqPtrGenModule.io.redirect := io.redirect
  enqPtrGenModule.io.allowEnqueue := allowEnqueue
  enqPtrGenModule.io.hasBlockBackward := hasBlockBackward
  enqPtrGenModule.io.enq := VecInit(io.enq.req.map(_.valid))
  enqPtrVec := enqPtrGenModule.io.out

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
  val commitCnt = PopCount(io.commits.commitValid)
  validCounter := Mux(io.commits.isCommit,
    (validCounter - commitCnt) + dispatchNum,
    trueValidCounter
  )

  allowEnqueue := Mux(io.commits.isCommit,
    validCounter + dispatchNum <= (RobSize - RenameWidth).U,
    trueValidCounter <= (RobSize - RenameWidth).U
  )

  val currentWalkPtr = Mux(state === s_walk || state === s_extrawalk, walkPtr, enqPtr - 1.U)
  val redirectWalkDistance = distanceBetween(currentWalkPtr, io.redirect.bits.robIdx)
  when (io.redirect.valid) {
    walkCounter := Mux(state === s_walk,
      // NOTE: +& is used here because:
      // When rob is full and the head instruction causes an exception,
      // the redirect robIdx is the deqPtr. In this case, currentWalkPtr is
      // enqPtr - 1.U and redirectWalkDistance is RobSize - 1.
      // Since exceptions flush the instruction itself, flushItSelf is true.B.
      // Previously we use `+` to count the walk distance and it causes overflows
      // when RobSize is power of 2. We change it to `+&` to allow walkCounter to be RobSize.
      // The width of walkCounter also needs to be changed.
      redirectWalkDistance - (thisCycleWalkCount - io.redirect.bits.flushItself()),
      redirectWalkDistance + io.redirect.bits.flushItself()
    )
    XSError(state === s_walk && thisCycleWalkCount < io.redirect.bits.flushItself(),
      p"walk distance error ($thisCycleWalkCount < ${io.redirect.bits.flushItself()}\n")
  }.elsewhen (state === s_walk) {
    walkCounter := walkCounter - thisCycleWalkCount
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
      valid(allocatePtrVec(i).value) := true.B
    }
  }
  // dequeue/walk logic writes 6 valid, dequeue and walk will not happen at the same time
  for (i <- 0 until CommitWidth) {
    val commitValid = io.commits.isCommit && io.commits.commitValid(i)
    val walkValid = io.commits.isWalk && io.commits.walkValid(i) && state =/= s_extrawalk
    when (commitValid || walkValid) {
      valid(commitReadAddr(i)) := false.B
    }
  }
  // reset: when exception, reset all valid to false
  when (reset.asBool) {
    for (i <- 0 until RobSize) {
      valid(i) := false.B
    }
  }

  // status field: writebacked
  // enqueue logic set 6 writebacked to false
  for (i <- 0 until RenameWidth) {
    when (canEnqueue(i)) {
      val enqHasException = ExceptionNO.selectFrontend(io.enq.req(i).bits.cf.exceptionVec).asUInt.orR
      val enqHasTriggerHit = io.enq.req(i).bits.cf.trigger.getHitFrontend
      val enqIsWritebacked = io.enq.req(i).bits.eliminatedMove
      writebacked(allocatePtrVec(i).value) := enqIsWritebacked && !enqHasException && !enqHasTriggerHit
      val isStu = io.enq.req(i).bits.ctrl.fuType === FuType.stu
      store_data_writebacked(allocatePtrVec(i).value) := !isStu
    }
  }
  when (exceptionGen.io.out.valid) {
    val wbIdx = exceptionGen.io.out.bits.robIdx.value
    writebacked(wbIdx) := true.B
    store_data_writebacked(wbIdx) := true.B
  }
  // writeback logic set numWbPorts writebacked to true
  for ((wb, cfgs) <- exuWriteback.zip(wbExuConfigs(exeWbSel))) {
    when (wb.valid) {
      val wbIdx = wb.bits.uop.robIdx.value
      val wbHasException = ExceptionNO.selectByExu(wb.bits.uop.cf.exceptionVec, cfgs).asUInt.orR
      val wbHasTriggerHit = wb.bits.uop.cf.trigger.getHitBackend
      val wbHasFlushPipe = cfgs.exists(_.flushPipe).B && wb.bits.uop.ctrl.flushPipe
      val wbHasReplayInst = cfgs.exists(_.replayInst).B && wb.bits.uop.ctrl.replayInst
      val block_wb = wbHasException || wbHasFlushPipe || wbHasReplayInst || wbHasTriggerHit
      writebacked(wbIdx) := !block_wb
    }
  }
  // store data writeback logic mark store as data_writebacked
  for (wb <- stdWriteback) {
    when(RegNext(wb.valid)) {
      store_data_writebacked(RegNext(wb.bits.uop.robIdx.value)) := true.B
    }
  }

  // flagBkup
  // enqueue logic set 6 flagBkup at most
  for (i <- 0 until RenameWidth) {
    when (canEnqueue(i)) {
      flagBkup(allocatePtrVec(i).value) := allocatePtrVec(i).flag
    }
  }

  // interrupt_safe
  for (i <- 0 until RenameWidth) {
    // We RegNext the updates for better timing.
    // Note that instructions won't change the system's states in this cycle.
    when (RegNext(canEnqueue(i))) {
      // For now, we allow non-load-store instructions to trigger interrupts
      // For MMIO instructions, they should not trigger interrupts since they may
      // be sent to lower level before it writes back.
      // However, we cannot determine whether a load/store instruction is MMIO.
      // Thus, we don't allow load/store instructions to trigger an interrupt.
      // TODO: support non-MMIO load-store instructions to trigger interrupts
      val allow_interrupts = !CommitType.isLoadStore(io.enq.req(i).bits.ctrl.commitType)
      interrupt_safe(RegNext(allocatePtrVec(i).value)) := RegNext(allow_interrupts)
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
  dispatchData.io.waddr := allocatePtrVec.map(_.value)
  dispatchData.io.wdata.zip(io.enq.req.map(_.bits)).foreach{ case (wdata, req) =>
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
  }
  dispatchData.io.raddr := commitReadAddr_next

  exceptionGen.io.redirect <> io.redirect
  exceptionGen.io.flush := io.flushOut.valid
  for (i <- 0 until RenameWidth) {
    exceptionGen.io.enq(i).valid := canEnqueue(i)
    exceptionGen.io.enq(i).bits.robIdx := io.enq.req(i).bits.robIdx
    exceptionGen.io.enq(i).bits.exceptionVec := ExceptionNO.selectFrontend(io.enq.req(i).bits.cf.exceptionVec)
    exceptionGen.io.enq(i).bits.flushPipe := io.enq.req(i).bits.ctrl.flushPipe
    exceptionGen.io.enq(i).bits.replayInst := false.B
    XSError(canEnqueue(i) && io.enq.req(i).bits.ctrl.replayInst, "enq should not set replayInst")
    exceptionGen.io.enq(i).bits.singleStep := io.enq.req(i).bits.ctrl.singleStep
    exceptionGen.io.enq(i).bits.crossPageIPFFix := io.enq.req(i).bits.cf.crossPageIPFFix
    exceptionGen.io.enq(i).bits.trigger.clear()
    exceptionGen.io.enq(i).bits.trigger.frontendHit := io.enq.req(i).bits.cf.trigger.frontendHit
  }

  println(s"ExceptionGen:")
  val exceptionCases = exceptionPorts.map(_._1.flatMap(_.exceptionOut).distinct.sorted)
  require(exceptionCases.length == exceptionGen.io.wb.length)
  for ((((configs, wb), exc_wb), i) <- exceptionPorts.zip(exceptionGen.io.wb).zipWithIndex) {
    exc_wb.valid                := wb.valid
    exc_wb.bits.robIdx          := wb.bits.uop.robIdx
    exc_wb.bits.exceptionVec    := ExceptionNO.selectByExu(wb.bits.uop.cf.exceptionVec, configs)
    exc_wb.bits.flushPipe       := configs.exists(_.flushPipe).B && wb.bits.uop.ctrl.flushPipe
    exc_wb.bits.replayInst      := configs.exists(_.replayInst).B && wb.bits.uop.ctrl.replayInst
    exc_wb.bits.singleStep      := false.B
    exc_wb.bits.crossPageIPFFix := false.B
    // TODO: make trigger configurable
    exc_wb.bits.trigger.clear()
    exc_wb.bits.trigger.backendHit := wb.bits.uop.cf.trigger.backendHit
    println(s"  [$i] ${configs.map(_.name)}: exception ${exceptionCases(i)}, " +
      s"flushPipe ${configs.exists(_.flushPipe)}, " +
      s"replayInst ${configs.exists(_.replayInst)}")
  }

  val fflags_wb = fflagsPorts.map(_._2)
  val fflagsDataModule = Module(new SyncDataModuleTemplate(
    UInt(5.W), RobSize, CommitWidth, fflags_wb.size)
  )
  for(i <- fflags_wb.indices){
    fflagsDataModule.io.wen  (i) := fflags_wb(i).valid
    fflagsDataModule.io.waddr(i) := fflags_wb(i).bits.uop.robIdx.value
    fflagsDataModule.io.wdata(i) := fflags_wb(i).bits.fflags
  }
  fflagsDataModule.io.raddr := VecInit(deqPtrVec_next.map(_.value))
  fflagsDataRead := fflagsDataModule.io.rdata


  val instrCntReg = RegInit(0.U(64.W))
  val fuseCommitCnt = PopCount(io.commits.commitValid.zip(io.commits.info).map{ case (v, i) => RegNext(v && CommitType.isFused(i.commitType)) })
  val trueCommitCnt = RegNext(commitCnt) +& fuseCommitCnt
  val retireCounter = Mux(RegNext(io.commits.isCommit), trueCommitCnt, 0.U)
  val instrCnt = instrCntReg + retireCounter
  instrCntReg := instrCnt
  io.csr.perfinfo.retiredInstr := retireCounter
  io.robFull := !allowEnqueue

  /**
    * debug info
    */
  XSDebug(p"enqPtr ${enqPtr} deqPtr ${deqPtr}\n")
  XSDebug("")
  for(i <- 0 until RobSize){
    XSDebug(false, !valid(i), "-")
    XSDebug(false, valid(i) && writebacked(i), "w")
    XSDebug(false, valid(i) && !writebacked(i), "v")
  }
  XSDebug(false, true.B, "\n")

  for(i <- 0 until RobSize) {
    if(i % 4 == 0) XSDebug("")
    XSDebug(false, true.B, "%x ", debug_microOp(i).cf.pc)
    XSDebug(false, !valid(i), "- ")
    XSDebug(false, valid(i) && writebacked(i), "w ")
    XSDebug(false, valid(i) && !writebacked(i), "v ")
    if(i % 4 == 3) XSDebug(false, true.B, "\n")
  }

  def ifCommit(counter: UInt): UInt = Mux(io.commits.isCommit, counter, 0.U)

  val commitDebugUop = deqPtrVec.map(_.value).map(debug_microOp(_))
  XSPerfAccumulate("clock_cycle", 1.U)
  QueuePerf(RobSize, PopCount((0 until RobSize).map(valid(_))), !allowEnqueue)
  XSPerfAccumulate("commitUop", ifCommit(commitCnt))
  XSPerfAccumulate("commitInstr", ifCommit(trueCommitCnt))
  val commitIsMove = commitDebugUop.map(_.ctrl.isMove)
  XSPerfAccumulate("commitInstrMove", ifCommit(PopCount(io.commits.commitValid.zip(commitIsMove).map{ case (v, m) => v && m })))
  val commitMoveElim = commitDebugUop.map(_.debugInfo.eliminatedMove)
  XSPerfAccumulate("commitInstrMoveElim", ifCommit(PopCount(io.commits.commitValid zip commitMoveElim map { case (v, e) => v && e })))
  XSPerfAccumulate("commitInstrFused", ifCommit(fuseCommitCnt))
  val commitIsLoad = io.commits.info.map(_.commitType).map(_ === CommitType.LOAD)
  val commitLoadValid = io.commits.commitValid.zip(commitIsLoad).map{ case (v, t) => v && t }
  XSPerfAccumulate("commitInstrLoad", ifCommit(PopCount(commitLoadValid)))
  val commitIsBranch = io.commits.info.map(_.commitType).map(_ === CommitType.BRANCH)
  val commitBranchValid = io.commits.commitValid.zip(commitIsBranch).map{ case (v, t) => v && t }
  XSPerfAccumulate("commitInstrBranch", ifCommit(PopCount(commitBranchValid)))
  val commitLoadWaitBit = commitDebugUop.map(_.cf.loadWaitBit)
  XSPerfAccumulate("commitInstrLoadWait", ifCommit(PopCount(commitLoadValid.zip(commitLoadWaitBit).map{ case (v, w) => v && w })))
  val commitIsStore = io.commits.info.map(_.commitType).map(_ === CommitType.STORE)
  XSPerfAccumulate("commitInstrStore", ifCommit(PopCount(io.commits.commitValid.zip(commitIsStore).map{ case (v, t) => v && t })))
  XSPerfAccumulate("writeback", PopCount((0 until RobSize).map(i => valid(i) && writebacked(i))))
  // XSPerfAccumulate("enqInstr", PopCount(io.dp1Req.map(_.fire)))
  // XSPerfAccumulate("d2rVnR", PopCount(io.dp1Req.map(p => p.valid && !p.ready)))
  XSPerfAccumulate("walkInstr", Mux(io.commits.isWalk, PopCount(io.commits.walkValid), 0.U))
  XSPerfAccumulate("walkCycle", state === s_walk || state === s_extrawalk)
  val deqNotWritebacked = valid(deqPtr.value) && !writebacked(deqPtr.value)
  val deqUopCommitType = io.commits.info(0).commitType
  XSPerfAccumulate("waitNormalCycle", deqNotWritebacked && deqUopCommitType === CommitType.NORMAL)
  XSPerfAccumulate("waitBranchCycle", deqNotWritebacked && deqUopCommitType === CommitType.BRANCH)
  XSPerfAccumulate("waitLoadCycle", deqNotWritebacked && deqUopCommitType === CommitType.LOAD)
  XSPerfAccumulate("waitStoreCycle", deqNotWritebacked && deqUopCommitType === CommitType.STORE)
  XSPerfAccumulate("robHeadPC", io.commits.info(0).pc)
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
    val commitIsFuType = io.commits.commitValid.zip(commitDebugUop).map(x => x._1 && x._2.ctrl.fuType === fuType.U )
    XSPerfAccumulate(s"${fuName}_instr_cnt", ifCommit(PopCount(commitIsFuType)))
    XSPerfAccumulate(s"${fuName}_latency_dispatch", ifCommit(latencySum(commitIsFuType, dispatchLatency)))
    XSPerfAccumulate(s"${fuName}_latency_enq_rs", ifCommit(latencySum(commitIsFuType, enqRsLatency)))
    XSPerfAccumulate(s"${fuName}_latency_select", ifCommit(latencySum(commitIsFuType, selectLatency)))
    XSPerfAccumulate(s"${fuName}_latency_issue", ifCommit(latencySum(commitIsFuType, issueLatency)))
    XSPerfAccumulate(s"${fuName}_latency_execute", ifCommit(latencySum(commitIsFuType, executeLatency)))
    XSPerfAccumulate(s"${fuName}_latency_enq_rs_execute", ifCommit(latencySum(commitIsFuType, rsFuLatency)))
    XSPerfAccumulate(s"${fuName}_latency_commit", ifCommit(latencySum(commitIsFuType, commitLatency)))
    if (fuType == FuType.fmac.litValue) {
      val commitIsFma = commitIsFuType.zip(commitDebugUop).map(x => x._1 && x._2.ctrl.fpu.ren3 )
      XSPerfAccumulate(s"${fuName}_instr_cnt_fma", ifCommit(PopCount(commitIsFma)))
      XSPerfAccumulate(s"${fuName}_latency_enq_rs_execute_fma", ifCommit(latencySum(commitIsFma, rsFuLatency)))
      XSPerfAccumulate(s"${fuName}_latency_execute_fma", ifCommit(latencySum(commitIsFma, executeLatency)))
    }
  }

  //difftest signals
  val firstValidCommit = (deqPtr + PriorityMux(io.commits.commitValid, VecInit(List.tabulate(CommitWidth)(_.U)))).value

  val wdata = Wire(Vec(CommitWidth, UInt(XLEN.W)))
  val wpc = Wire(Vec(CommitWidth, UInt(XLEN.W)))

  for(i <- 0 until CommitWidth) {
    val idx = deqPtrVec(i).value
    wdata(i) := debug_exuData(idx)
    wpc(i) := SignExt(commitDebugUop(i).cf.pc, XLEN)
  }

  if (env.EnableDifftest) {
    for (i <- 0 until CommitWidth) {
      val difftest = Module(new DifftestInstrCommit)
      difftest.io.clock    := clock
      difftest.io.coreid   := io.hartId
      difftest.io.index    := i.U

      val ptr = deqPtrVec(i).value
      val uop = commitDebugUop(i)
      val exuOut = debug_exuDebug(ptr)
      val exuData = debug_exuData(ptr)
      difftest.io.valid    := RegNext(RegNext(RegNext(io.commits.commitValid(i) && io.commits.isCommit)))
      difftest.io.pc       := RegNext(RegNext(RegNext(SignExt(uop.cf.pc, XLEN))))
      difftest.io.instr    := RegNext(RegNext(RegNext(uop.cf.instr)))
      difftest.io.special  := RegNext(RegNext(RegNext(CommitType.isFused(io.commits.info(i).commitType))))
      // when committing an eliminated move instruction,
      // we must make sure that skip is properly set to false (output from EXU is random value)
      difftest.io.skip     := RegNext(RegNext(RegNext(Mux(uop.eliminatedMove, false.B, exuOut.isMMIO || exuOut.isPerfCnt))))
      difftest.io.isRVC    := RegNext(RegNext(RegNext(uop.cf.pd.isRVC)))
      difftest.io.rfwen    := RegNext(RegNext(RegNext(io.commits.commitValid(i) && io.commits.info(i).rfWen && io.commits.info(i).ldest =/= 0.U)))
      difftest.io.fpwen    := RegNext(RegNext(RegNext(io.commits.commitValid(i) && io.commits.info(i).fpWen)))
      difftest.io.wpdest   := RegNext(RegNext(RegNext(io.commits.info(i).pdest)))
      difftest.io.wdest    := RegNext(RegNext(RegNext(io.commits.info(i).ldest)))

      // // runahead commit hint
      // val runahead_commit = Module(new DifftestRunaheadCommitEvent)
      // runahead_commit.io.clock := clock
      // runahead_commit.io.coreid := io.hartId
      // runahead_commit.io.index := i.U
      // runahead_commit.io.valid := difftest.io.valid &&
      //   (commitBranchValid(i) || commitIsStore(i))
      // // TODO: is branch or store
      // runahead_commit.io.pc    := difftest.io.pc
    }
  }
  else if (env.AlwaysBasicDiff) {
    // These are the structures used by difftest only and should be optimized after synthesis.
    val dt_eliminatedMove = Mem(RobSize, Bool())
    val dt_isRVC = Mem(RobSize, Bool())
    val dt_exuDebug = Reg(Vec(RobSize, new DebugBundle))
    for (i <- 0 until RenameWidth) {
      when (canEnqueue(i)) {
        dt_eliminatedMove(allocatePtrVec(i).value) := io.enq.req(i).bits.eliminatedMove
        dt_isRVC(allocatePtrVec(i).value) := io.enq.req(i).bits.cf.pd.isRVC
      }
    }
    for (wb <- exuWriteback) {
      when (wb.valid) {
        val wbIdx = wb.bits.uop.robIdx.value
        dt_exuDebug(wbIdx) := wb.bits.debug
      }
    }
    // Always instantiate basic difftest modules.
    for (i <- 0 until CommitWidth) {
      val commitInfo = io.commits.info(i)
      val ptr = deqPtrVec(i).value
      val exuOut = dt_exuDebug(ptr)
      val eliminatedMove = dt_eliminatedMove(ptr)
      val isRVC = dt_isRVC(ptr)

      val difftest = Module(new DifftestBasicInstrCommit)
      difftest.io.clock   := clock
      difftest.io.coreid  := io.hartId
      difftest.io.index   := i.U
      difftest.io.valid   := RegNext(RegNext(RegNext(io.commits.commitValid(i) && io.commits.isCommit)))
      difftest.io.special := RegNext(RegNext(RegNext(CommitType.isFused(commitInfo.commitType))))
      difftest.io.skip    := RegNext(RegNext(RegNext(Mux(eliminatedMove, false.B, exuOut.isMMIO || exuOut.isPerfCnt))))
      difftest.io.isRVC   := RegNext(RegNext(RegNext(isRVC)))
      difftest.io.rfwen   := RegNext(RegNext(RegNext(io.commits.commitValid(i) && commitInfo.rfWen && commitInfo.ldest =/= 0.U)))
      difftest.io.fpwen   := RegNext(RegNext(RegNext(io.commits.commitValid(i) && commitInfo.fpWen)))
      difftest.io.wpdest  := RegNext(RegNext(RegNext(commitInfo.pdest)))
      difftest.io.wdest   := RegNext(RegNext(RegNext(commitInfo.ldest)))
    }
  }

  if (env.EnableDifftest) {
    for (i <- 0 until CommitWidth) {
      val difftest = Module(new DifftestLoadEvent)
      difftest.io.clock  := clock
      difftest.io.coreid := io.hartId
      difftest.io.index  := i.U

      val ptr = deqPtrVec(i).value
      val uop = commitDebugUop(i)
      val exuOut = debug_exuDebug(ptr)
      difftest.io.valid  := RegNext(RegNext(RegNext(io.commits.commitValid(i) && io.commits.isCommit)))
      difftest.io.paddr  := RegNext(RegNext(RegNext(exuOut.paddr)))
      difftest.io.opType := RegNext(RegNext(RegNext(uop.ctrl.fuOpType)))
      difftest.io.fuType := RegNext(RegNext(RegNext(uop.ctrl.fuType)))
    }
  }

  // Always instantiate basic difftest modules.
  if (env.EnableDifftest) {
    val dt_isXSTrap = Mem(RobSize, Bool())
    for (i <- 0 until RenameWidth) {
      when (canEnqueue(i)) {
        dt_isXSTrap(allocatePtrVec(i).value) := io.enq.req(i).bits.ctrl.isXSTrap
      }
    }
    val trapVec = io.commits.commitValid.zip(deqPtrVec).map{ case (v, d) => io.commits.isCommit && v && dt_isXSTrap(d.value) }
    val hitTrap = trapVec.reduce(_||_)
    val trapCode = PriorityMux(wdata.zip(trapVec).map(x => x._2 -> x._1))
    val trapPC = SignExt(PriorityMux(wpc.zip(trapVec).map(x => x._2 ->x._1)), XLEN)
    val difftest = Module(new DifftestTrapEvent)
    difftest.io.clock    := clock
    difftest.io.coreid   := io.hartId
    difftest.io.valid    := hitTrap
    difftest.io.code     := trapCode
    difftest.io.pc       := trapPC
    difftest.io.cycleCnt := timer
    difftest.io.instrCnt := instrCnt
    difftest.io.hasWFI   := hasWFI
  }
  else if (env.AlwaysBasicDiff) {
    val dt_isXSTrap = Mem(RobSize, Bool())
    for (i <- 0 until RenameWidth) {
      when (canEnqueue(i)) {
        dt_isXSTrap(allocatePtrVec(i).value) := io.enq.req(i).bits.ctrl.isXSTrap
      }
    }
    val trapVec = io.commits.commitValid.zip(deqPtrVec).map{ case (v, d) => io.commits.isCommit && v && dt_isXSTrap(d.value) }
    val hitTrap = trapVec.reduce(_||_)
    val difftest = Module(new DifftestBasicTrapEvent)
    difftest.io.clock    := clock
    difftest.io.coreid   := io.hartId
    difftest.io.valid    := hitTrap
    difftest.io.cycleCnt := timer
    difftest.io.instrCnt := instrCnt
  }

  val perfEvents = Seq(
    ("rob_interrupt_num       ", io.flushOut.valid && intrEnable                                                                                                   ),
    ("rob_exception_num       ", io.flushOut.valid && exceptionEnable                                                                                              ),
    ("rob_flush_pipe_num      ", io.flushOut.valid && isFlushPipe                                                                                                  ),
    ("rob_replay_inst_num     ", io.flushOut.valid && isFlushPipe && deqHasReplayInst                                                                              ),
    ("rob_commitUop           ", ifCommit(commitCnt)                                                                                                               ),
    ("rob_commitInstr         ", ifCommit(trueCommitCnt)                                                                                                           ),
    ("rob_commitInstrMove     ", ifCommit(PopCount(io.commits.commitValid.zip(commitIsMove).map{ case (v, m) => v && m }))                                               ),
    ("rob_commitInstrFused    ", ifCommit(fuseCommitCnt)                                                                                                           ),
    ("rob_commitInstrLoad     ", ifCommit(PopCount(commitLoadValid))                                                                                               ),
    ("rob_commitInstrLoad     ", ifCommit(PopCount(commitBranchValid))                                                                                               ),
    ("rob_commitInstrLoadWait ", ifCommit(PopCount(commitLoadValid.zip(commitLoadWaitBit).map{ case (v, w) => v && w }))                                           ),
    ("rob_commitInstrStore    ", ifCommit(PopCount(io.commits.commitValid.zip(commitIsStore).map{ case (v, t) => v && t }))                                              ),
    ("rob_walkInstr           ", Mux(io.commits.isWalk, PopCount(io.commits.walkValid), 0.U)                                                                           ),
    ("rob_walkCycle           ", (state === s_walk || state === s_extrawalk)                                                                                       ),
    ("rob_1_4_valid           ", (PopCount((0 until RobSize).map(valid(_))) < (RobSize.U/4.U))                                                                     ),
    ("rob_2_4_valid           ", (PopCount((0 until RobSize).map(valid(_))) > (RobSize.U/4.U)) & (PopCount((0 until RobSize).map(valid(_))) <= (RobSize.U/2.U))    ),
    ("rob_3_4_valid           ", (PopCount((0 until RobSize).map(valid(_))) > (RobSize.U/2.U)) & (PopCount((0 until RobSize).map(valid(_))) <= (RobSize.U*3.U/4.U))),
    ("rob_4_4_valid           ", (PopCount((0 until RobSize).map(valid(_))) > (RobSize.U*3.U/4.U))                                                                 ),
  )
  generatePerfEvent()
}
