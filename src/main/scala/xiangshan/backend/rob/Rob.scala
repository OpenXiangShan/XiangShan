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
import utility._
import utils._
import xiangshan._
import xiangshan.backend.BackendParams
import xiangshan.backend.Bundles.{DynInst, ExceptionInfo, ExuOutput}
import xiangshan.backend.fu.FuType
import xiangshan.frontend.FtqPtr
import xiangshan.mem.{LqPtr, SqPtr}
import xiangshan.backend.Bundles.{DynInst, ExceptionInfo, ExuOutput}

class DebugMdpInfo(implicit p: Parameters) extends XSBundle{
  val ssid = UInt(SSIDWidth.W)
  val waitAllStore = Bool()
}

class DebugLsInfo(implicit p: Parameters) extends XSBundle{
  val s1 = new Bundle{
    val isTlbFirstMiss = Bool() // in s1
    val isBankConflict = Bool() // in s1
    val isLoadToLoadForward = Bool()
    val isReplayFast = Bool()
  }
  val s2 = new Bundle{
    val isDcacheFirstMiss = Bool() // in s2 (predicted result is in s1 when using WPU, real result is in s2)
    val isForwardFail = Bool() // in s2
    val isReplaySlow = Bool()
    val isLoadReplayTLBMiss = Bool()
    val isLoadReplayCacheMiss = Bool()
  }
  val replayCnt = UInt(XLEN.W)

  def s1SignalEnable(ena: DebugLsInfo) = {
    when(ena.s1.isTlbFirstMiss) { s1.isTlbFirstMiss := true.B }
    when(ena.s1.isBankConflict) { s1.isBankConflict := true.B }
    when(ena.s1.isLoadToLoadForward) { s1.isLoadToLoadForward := true.B }
    when(ena.s1.isReplayFast) {
      s1.isReplayFast := true.B
      replayCnt := replayCnt + 1.U
    }
  }

  def s2SignalEnable(ena: DebugLsInfo) = {
    when(ena.s2.isDcacheFirstMiss) { s2.isDcacheFirstMiss := true.B }
    when(ena.s2.isForwardFail) { s2.isForwardFail := true.B }
    when(ena.s2.isLoadReplayTLBMiss) { s2.isLoadReplayTLBMiss := true.B }
    when(ena.s2.isLoadReplayCacheMiss) { s2.isLoadReplayCacheMiss := true.B }
    when(ena.s2.isReplaySlow) {
      s2.isReplaySlow := true.B
      replayCnt := replayCnt + 1.U
    }
  }
}

object DebugLsInfo{
  def init(implicit p: Parameters): DebugLsInfo = {
    val lsInfo = Wire(new DebugLsInfo)
    lsInfo.s1.isTlbFirstMiss := false.B
    lsInfo.s1.isBankConflict := false.B
    lsInfo.s1.isLoadToLoadForward := false.B
    lsInfo.s1.isReplayFast := false.B
    lsInfo.s2.isDcacheFirstMiss := false.B
    lsInfo.s2.isForwardFail := false.B
    lsInfo.s2.isReplaySlow := false.B
    lsInfo.s2.isLoadReplayTLBMiss := false.B
    lsInfo.s2.isLoadReplayCacheMiss := false.B
    lsInfo.replayCnt := 0.U
    lsInfo
  }
}

class DebugLsInfoBundle(implicit p: Parameters) extends DebugLsInfo {
  // unified processing at the end stage of load/store  ==> s2  ==> bug that will write error robIdx data
  val s1_robIdx = UInt(log2Ceil(RobSize).W)
  val s2_robIdx = UInt(log2Ceil(RobSize).W)
}

class DebugLSIO(implicit p: Parameters) extends XSBundle {
  val debugLsInfo = Vec(backendParams.LduCnt + backendParams.StaCnt, Output(new DebugLsInfoBundle))
}

class RobPtr(entries: Int) extends CircularQueuePtr[RobPtr](
  entries
) with HasCircularQueuePtrHelper {

  def this()(implicit p: Parameters) = this(p(XSCoreParamsKey).RobSize)

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
  val vxsat      = Output(Valid(Bool()))
  val dirty_fs   = Output(Bool())
  val perfinfo   = new Bundle {
    val retiredInstr = Output(UInt(3.W))
  }

  val vcsrFlag   = Output(Bool())
}

class RobLsqIO(implicit p: Parameters) extends XSBundle {
  val lcommit = Output(UInt(log2Up(CommitWidth + 1).W))
  val scommit = Output(UInt(log2Up(CommitWidth + 1).W))
  val pendingld = Output(Bool())
  val pendingst = Output(Bool())
  val commit = Output(Bool())
  val pendingPtr = Output(new RobPtr)

  val mmio = Input(Vec(LoadPipelineWidth, Bool()))
  val uop = Input(Vec(LoadPipelineWidth, new DynInst))
}

class RobEnqIO(implicit p: Parameters) extends XSBundle {
  val canAccept = Output(Bool())
  val isEmpty = Output(Bool())
  // valid vector, for robIdx gen and walk
  val needAlloc = Vec(RenameWidth, Input(Bool()))
  val req = Vec(RenameWidth, Flipped(ValidIO(new DynInst)))
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
    val blockCommit = Input(Bool())
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
  val canCommit = VecInit((0 until CommitWidth).map(i => io.deq_v(i) && io.deq_w(i)))
  val normalCommitCnt = PriorityEncoder(canCommit.map(c => !c) :+ true.B)
  // when io.intrBitSetReg or there're possible exceptions in these instructions,
  // only one instruction is allowed to commit
  val allowOnlyOne = commit_exception || io.intrBitSetReg
  val commitCnt = Mux(allowOnlyOne, canCommit(0), normalCommitCnt)

  val commitDeqPtrVec = VecInit(deqPtrVec.map(_ + commitCnt))
  val deqPtrVec_next = Mux(io.state === 0.U && !redirectOutValid && !io.blockCommit, commitDeqPtrVec, deqPtrVec)

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
  val isVset = Bool()
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

class ExceptionGen(params: BackendParams)(implicit p: Parameters) extends XSModule with HasCircularQueuePtrHelper {
  val io = IO(new Bundle {
    val redirect = Input(Valid(new Redirect))
    val flush = Input(Bool())
    val enq = Vec(RenameWidth, Flipped(ValidIO(new RobExceptionInfo)))
    // csr + load + store
    val wb = Vec(params.numException, Flipped(ValidIO(new RobExceptionInfo)))
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

  val currentValid = RegInit(false.B)
  val current = Reg(new RobExceptionInfo)

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
  val current_flush = current.robIdx.needFlush(io.redirect) || io.flush
  val s1_flush = s1_out_bits.robIdx.needFlush(io.redirect) || io.flush
  when (currentValid) {
    when (current_flush) {
      currentValid := Mux(s1_flush, false.B, s1_out_valid)
    }
    when (s1_out_valid && !s1_flush) {
      when (isAfter(current.robIdx, s1_out_bits.robIdx)) {
        current := s1_out_bits
      }.elsewhen (current.robIdx === s1_out_bits.robIdx) {
        current.exceptionVec := (s1_out_bits.exceptionVec.asUInt | current.exceptionVec.asUInt).asTypeOf(ExceptionVec())
        current.flushPipe := s1_out_bits.flushPipe || current.flushPipe
        current.replayInst := s1_out_bits.replayInst || current.replayInst
        current.singleStep := s1_out_bits.singleStep || current.singleStep
        current.trigger := (s1_out_bits.trigger.asUInt | current.trigger.asUInt).asTypeOf(new TriggerCf)
      }
    }
  }.elsewhen (s1_out_valid && !s1_flush) {
    currentValid := true.B
    current := s1_out_bits
  }.elsewhen (enq_valid && !(io.redirect.valid || io.flush)) {
    currentValid := true.B
    current := enq_bits
  }

  io.out.valid   := s1_out_valid || enq_valid && enq_bits.can_writeback
  io.out.bits    := Mux(s1_out_valid, s1_out_bits, enq_bits)
  io.state.valid := currentValid
  io.state.bits  := current

}

class RobFlushInfo(implicit p: Parameters) extends XSBundle {
  val ftqIdx = new FtqPtr
  val robIdx = new RobPtr
  val ftqOffset = UInt(log2Up(PredictWidth).W)
  val replayInst = Bool()
}

class Rob(params: BackendParams)(implicit p: Parameters) extends LazyModule with HasXSParameter {

  lazy val module = new RobImp(this)(p, params)
  //
  //  override def generateWritebackIO(
  //    thisMod: Option[HasWritebackSource] = None,
  //    thisModImp: Option[HasWritebackSourceImp] = None
  //  ): Unit = {
  //    val sources = writebackSinksImp(thisMod, thisModImp)
  //    module.io.writeback.zip(sources).foreach(x => x._1 := x._2)
  //  }
  //}
}

class RobImp(override val wrapper: Rob)(implicit p: Parameters, params: BackendParams) extends LazyModuleImp(wrapper)
  with HasXSParameter with HasCircularQueuePtrHelper with HasPerfEvents {

  val io = IO(new Bundle() {
    val hartId = Input(UInt(8.W))
    val redirect = Input(Valid(new Redirect))
    val enq = new RobEnqIO
    val flushOut = ValidIO(new Redirect)
    val exception = ValidIO(new ExceptionInfo)
    // exu + brq
    val writeback: MixedVec[ValidIO[ExuOutput]] = Flipped(params.genWrite2CtrlBundles)
    val commits = Output(new RobCommitIO)
    val rabCommits = Output(new RobCommitIO)
    val diffCommits = Output(new DiffCommitIO)
    val isVsetFlushPipe = Output(Bool())
    val vconfigPdest = Output(UInt(PhyRegIdxWidth.W))
    val lsq = new RobLsqIO
    val robDeqPtr = Output(new RobPtr)
    val csr = new RobCSRIO
    val robFull = Output(Bool())
    val cpu_halt = Output(Bool())
    val wfi_enable = Input(Bool())
  })

  val exuWBs: Seq[ValidIO[ExuOutput]] = io.writeback.filter(!_.bits.params.hasStdFu)
  val stdWBs: Seq[ValidIO[ExuOutput]] = io.writeback.filter(_.bits.params.hasStdFu)
  val fflagsWBs = io.writeback.filter(x => x.bits.fflags.nonEmpty)
  val exceptionWBs = io.writeback.filter(x => x.bits.exceptionVec.nonEmpty)
  val redirectWBs = io.writeback.filter(x => x.bits.redirect.nonEmpty)

  val exuWbPorts = io.writeback.filter(!_.bits.params.hasStdFu)
  val stdWbPorts = io.writeback.filter(_.bits.params.hasStdFu)
  val fflagsPorts = io.writeback.filter(x => x.bits.fflags.nonEmpty)
  val vxsatPorts = io.writeback.filter(x => x.bits.vxsat.nonEmpty)
  val exceptionPorts = io.writeback.filter(x => x.bits.exceptionVec.nonEmpty)
  val numExuWbPorts = exuWBs.length
  val numStdWbPorts = stdWBs.length


  println(s"Rob: size $RobSize, numExuWbPorts: $numExuWbPorts, numStdWbPorts: $numStdWbPorts, commitwidth: $CommitWidth")
//  println(s"exuPorts: ${exuWbPorts.map(_._1.map(_.name))}")
//  println(s"stdPorts: ${stdWbPorts.map(_._1.map(_.name))}")
//  println(s"fflags: ${fflagsPorts.map(_._1.map(_.name))}")


  // instvalid field
  val valid = RegInit(VecInit(Seq.fill(RobSize)(false.B)))
  // writeback status

  val stdWritebacked = Reg(Vec(RobSize, Bool()))
  val uopNumVec          = RegInit(VecInit(Seq.fill(RobSize)(0.U(log2Up(MaxUopSize + 1).W))))
  val realDestSize       = RegInit(VecInit(Seq.fill(RobSize)(0.U(log2Up(MaxUopSize + 1).W))))
  val fflagsDataModule   = RegInit(VecInit(Seq.fill(RobSize)(0.U(5.W))))
  val vxsatDataModule    = RegInit(VecInit(Seq.fill(RobSize)(false.B)))

  def isWritebacked(ptr: UInt): Bool = {
    !uopNumVec(ptr).orR && stdWritebacked(ptr)
  }

  val mmio = RegInit(VecInit(Seq.fill(RobSize)(false.B)))

  // data for redirect, exception, etc.
  val flagBkup = Mem(RobSize, Bool())
  // some instructions are not allowed to trigger interrupts
  // They have side effects on the states of the processor before they write back
  val interrupt_safe = Mem(RobSize, Bool())

  // data for debug
  // Warn: debug_* prefix should not exist in generated verilog.
  val debug_microOp = Mem(RobSize, new DynInst)
  val debug_exuData = Reg(Vec(RobSize, UInt(XLEN.W)))//for debug
  val debug_exuDebug = Reg(Vec(RobSize, new DebugBundle))//for debug

  // pointers
  // For enqueue ptr, we don't duplicate it since only enqueue needs it.
  val enqPtrVec = Wire(Vec(RenameWidth, new RobPtr))
  val deqPtrVec = Wire(Vec(CommitWidth, new RobPtr))

  val walkPtrVec = Reg(Vec(CommitWidth, new RobPtr))
  val allowEnqueue = RegInit(true.B)

  val enqPtr = enqPtrVec.head
  val deqPtr = deqPtrVec(0)
  val walkPtr = walkPtrVec(0)

  val isEmpty = enqPtr === deqPtr
  val isReplaying = io.redirect.valid && RedirectLevel.flushItself(io.redirect.bits.level)

  /**
    * states of Rob
    */
  val s_idle :: s_walk :: Nil = Enum(2)
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

  val exceptionGen = Module(new ExceptionGen(params))
  val exceptionDataRead = exceptionGen.io.state
  val fflagsDataRead = Wire(Vec(CommitWidth, UInt(5.W)))
  val vxsatDataRead = Wire(Vec(CommitWidth, Bool()))

  io.robDeqPtr := deqPtr

  val rab = Module(new RenameBuffer(RabSize))
  rab.io.redirectValid := io.redirect.valid
  rab.io.req.zip(io.enq.req).map { case (dest, src) =>
    dest.bits := src.bits
    dest.valid := src.valid && io.enq.canAccept
  }

  val realDestSizeCandidates = (0 until CommitWidth).map(i => realDestSize(Mux(state === s_idle, deqPtrVec(i).value, walkPtrVec(i).value)))
  val wbSizeSeq = io.commits.commitValid.zip(io.commits.walkValid).zip(realDestSizeCandidates).map { case ((commitValid, walkValid), realDestSize) =>
    Mux(io.commits.isCommit, Mux(commitValid, realDestSize, 0.U), Mux(walkValid, realDestSize, 0.U))
  }
  val wbSizeSum = wbSizeSeq.reduce(_ + _)
  rab.io.commitSize := wbSizeSum
  rab.io.walkSize := wbSizeSum

  io.rabCommits := rab.io.commits
  io.diffCommits := rab.io.diffCommits

  /**
    * Enqueue (from dispatch)
    */
  // special cases
  val hasBlockBackward = RegInit(false.B)
  val hasWaitForward = RegInit(false.B)
  val doingSvinval = RegInit(false.B)
  // When blockBackward instruction leaves Rob (commit or walk), hasBlockBackward should be set to false.B
  // To reduce registers usage, for hasBlockBackward cases, we allow enqueue after ROB is empty.
  when (isEmpty) { hasBlockBackward:= false.B }
  // When any instruction commits, hasNoSpecExec should be set to false.B
  when (io.commits.hasWalkInstr || io.commits.hasCommitInstr) { hasWaitForward:= false.B }

  // The wait-for-interrupt (WFI) instruction waits in the ROB until an interrupt might need servicing.
  // io.csr.wfiEvent will be asserted if the WFI can resume execution, and we change the state to s_wfi_idle.
  // It does not affect how interrupts are serviced. Note that WFI is noSpecExec and it does not trigger interrupts.
  val hasWFI = RegInit(false.B)
  io.cpu_halt := hasWFI
  // WFI Timeout: 2^20 = 1M cycles
  val wfi_cycles = RegInit(0.U(20.W))
  when (hasWFI) {
    wfi_cycles := wfi_cycles + 1.U
  }.elsewhen (!hasWFI && RegNext(hasWFI)) {
    wfi_cycles := 0.U
  }
  val wfi_timeout = wfi_cycles.andR
  when (RegNext(RegNext(io.csr.wfiEvent)) || io.flushOut.valid || wfi_timeout) {
    hasWFI := false.B
  }

  val allocatePtrVec = VecInit((0 until RenameWidth).map(i => enqPtrVec(PopCount(io.enq.req.take(i).map(req => req.valid && req.bits.firstUop)))))
  io.enq.canAccept := allowEnqueue && !hasBlockBackward && rab.io.canEnq
  io.enq.resp      := allocatePtrVec
  val canEnqueue = VecInit(io.enq.req.map(req => req.valid && req.bits.firstUop && io.enq.canAccept))
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
      when (enqUop.blockBackward) {
        hasBlockBackward := true.B
      }
      when (enqUop.waitForward) {
        hasWaitForward := true.B
      }
      val enqHasTriggerHit = false.B // io.enq.req(i).bits.cf.trigger.getHitFrontend
      val enqHasException = ExceptionNO.selectFrontend(enqUop.exceptionVec).asUInt.orR
      // the begin instruction of Svinval enqs so mark doingSvinval as true to indicate this process
      when(!enqHasTriggerHit && !enqHasException && enqUop.isSvinvalBegin(enqUop.flushPipe))
      {
        doingSvinval := true.B
      }
      // the end instruction of Svinval enqs so clear doingSvinval
      when(!enqHasTriggerHit && !enqHasException && enqUop.isSvinvalEnd(enqUop.flushPipe))
      {
        doingSvinval := false.B
      }
      // when we are in the process of Svinval software code area , only Svinval.vma and end instruction of Svinval can appear
      assert(!doingSvinval || (enqUop.isSvinval(enqUop.flushPipe) || enqUop.isSvinvalEnd(enqUop.flushPipe)))
      when (enqUop.isWFI && !enqHasException && !enqHasTriggerHit) {
        hasWFI := true.B
      }

      mmio(enqIndex) := false.B
    }
  }
  val dispatchNum = Mux(io.enq.canAccept, PopCount(io.enq.req.map(req => req.valid && req.bits.firstUop)), 0.U)
  io.enq.isEmpty   := RegNext(isEmpty && !VecInit(io.enq.req.map(_.valid)).asUInt.orR)

  when (!io.wfi_enable) {
    hasWFI := false.B
  }
  // sel vsetvl's flush position
  val vs_idle :: vs_waitVinstr :: vs_waitFlush :: Nil = Enum(3)
  val vsetvlState = RegInit(vs_idle)

  val firstVInstrFtqPtr    = RegInit(0.U.asTypeOf(new FtqPtr))
  val firstVInstrFtqOffset = RegInit(0.U.asTypeOf(UInt(log2Up(PredictWidth).W)))
  val firstVInstrRobIdx    = RegInit(0.U.asTypeOf(new RobPtr))

  val enq0            = io.enq.req(0)
  val enq0IsVset      = enq0.bits.isVset && enq0.bits.lastUop && canEnqueue(0)
  val enq0IsVsetFlush = enq0IsVset && enq0.bits.flushPipe
  val enqIsVInstrVec = io.enq.req.zip(canEnqueue).map{case (req, fire) => FuType.isVpu(req.bits.fuType) && fire}
  // for vs_idle
  val firstVInstrIdle = PriorityMux(enqIsVInstrVec.zip(io.enq.req).drop(1) :+ (true.B, 0.U.asTypeOf(io.enq.req(0).cloneType)))
  // for vs_waitVinstr
  val enqIsVInstrOrVset = (enqIsVInstrVec(0) || enq0IsVset) +: enqIsVInstrVec.drop(1)
  val firstVInstrWait = PriorityMux(enqIsVInstrOrVset, io.enq.req)
  when(vsetvlState === vs_idle){
    firstVInstrFtqPtr    := firstVInstrIdle.bits.ftqPtr
    firstVInstrFtqOffset := firstVInstrIdle.bits.ftqOffset
    firstVInstrRobIdx    := firstVInstrIdle.bits.robIdx
  }.elsewhen(vsetvlState === vs_waitVinstr){
    when(Cat(enqIsVInstrOrVset).orR){
      firstVInstrFtqPtr := firstVInstrWait.bits.ftqPtr
      firstVInstrFtqOffset := firstVInstrWait.bits.ftqOffset
      firstVInstrRobIdx := firstVInstrWait.bits.robIdx
    }
  }

  val hasVInstrAfterI = Cat(enqIsVInstrVec(0)).orR
  when(vsetvlState === vs_idle && !io.redirect.valid){
    when(enq0IsVsetFlush){
      vsetvlState := Mux(hasVInstrAfterI, vs_waitFlush, vs_waitVinstr)
    }
  }.elsewhen(vsetvlState === vs_waitVinstr){
    when(io.redirect.valid){
      vsetvlState := vs_idle
    }.elsewhen(Cat(enqIsVInstrOrVset).orR){
      vsetvlState := vs_waitFlush
    }
  }.elsewhen(vsetvlState === vs_waitFlush){
    when(io.redirect.valid){
      vsetvlState := vs_idle
    }
  }

  /**
    * Writeback (from execution units)
    */
  for (wb <- exuWBs) {
    when (wb.valid) {
      val wbIdx = wb.bits.robIdx.value
      debug_exuData(wbIdx) := wb.bits.data
      debug_exuDebug(wbIdx) := wb.bits.debug
      debug_microOp(wbIdx).debugInfo.enqRsTime := wb.bits.debugInfo.enqRsTime
      debug_microOp(wbIdx).debugInfo.selectTime := wb.bits.debugInfo.selectTime
      debug_microOp(wbIdx).debugInfo.issueTime := wb.bits.debugInfo.issueTime
      debug_microOp(wbIdx).debugInfo.writebackTime := wb.bits.debugInfo.writebackTime

      // debug for lqidx and sqidx
      debug_microOp(wbIdx).lqIdx := wb.bits.lqIdx.getOrElse(0.U.asTypeOf(new LqPtr))
      debug_microOp(wbIdx).sqIdx := wb.bits.sqIdx.getOrElse(0.U.asTypeOf(new SqPtr))

      val debug_Uop = debug_microOp(wbIdx)
      XSInfo(true.B,
        p"writebacked pc 0x${Hexadecimal(debug_Uop.pc)} wen ${debug_Uop.rfWen} " +
        p"data 0x${Hexadecimal(wb.bits.data)} ldst ${debug_Uop.ldest} pdst ${debug_Uop.pdest} " +
        p"skip ${wb.bits.debug.isMMIO} robIdx: ${wb.bits.robIdx}\n"
      )
    }
  }

  val writebackNum = PopCount(exuWBs.map(_.valid))
  XSInfo(writebackNum =/= 0.U, "writebacked %d insts\n", writebackNum)

  for (i <- 0 until LoadPipelineWidth) {
    when (RegNext(io.lsq.mmio(i))) {
      mmio(RegNext(io.lsq.uop(i).robIdx).value) := true.B
    }
  }

  /**
    * RedirectOut: Interrupt and Exceptions
    */
  val deqDispatchData = dispatchDataRead(0)
  val debug_deqUop = debug_microOp(deqPtr.value)

  val intrBitSetReg = RegNext(io.csr.intrBitSet)
  val intrEnable = intrBitSetReg && !hasWaitForward && interrupt_safe(deqPtr.value)
  val deqHasExceptionOrFlush = exceptionDataRead.valid && exceptionDataRead.bits.robIdx === deqPtr
  val deqHasException = deqHasExceptionOrFlush && (exceptionDataRead.bits.exceptionVec.asUInt.orR ||
    exceptionDataRead.bits.singleStep || exceptionDataRead.bits.trigger.hit)
  val deqHasFlushPipe = deqHasExceptionOrFlush && exceptionDataRead.bits.flushPipe
  val deqHasReplayInst = deqHasExceptionOrFlush && exceptionDataRead.bits.replayInst
  val exceptionEnable = isWritebacked(deqPtr.value) && deqHasException

  XSDebug(deqHasException && exceptionDataRead.bits.singleStep, "Debug Mode: Deq has singlestep exception\n")
  XSDebug(deqHasException && exceptionDataRead.bits.trigger.getHitFrontend, "Debug Mode: Deq has frontend trigger exception\n")
  XSDebug(deqHasException && exceptionDataRead.bits.trigger.getHitBackend, "Debug Mode: Deq has backend trigger exception\n")

  val isFlushPipe = isWritebacked(deqPtr.value) && (deqHasFlushPipe || deqHasReplayInst)

  val isVsetFlushPipe = isWritebacked(deqPtr.value) && deqHasFlushPipe && exceptionDataRead.bits.isVset
//  val needModifyFtqIdxOffset = isVsetFlushPipe && (vsetvlState === vs_waitFlush)
  val needModifyFtqIdxOffset = false.B
  io.isVsetFlushPipe := isVsetFlushPipe
  io.vconfigPdest := rab.io.vconfigPdest
  // io.flushOut will trigger redirect at the next cycle.
  // Block any redirect or commit at the next cycle.
  val lastCycleFlush = RegNext(io.flushOut.valid)

  io.flushOut.valid := (state === s_idle) && valid(deqPtr.value) && (intrEnable || exceptionEnable || isFlushPipe) && !lastCycleFlush
  io.flushOut.bits := DontCare
  io.flushOut.bits.robIdx := Mux(needModifyFtqIdxOffset, firstVInstrRobIdx, deqPtr)
  io.flushOut.bits.ftqIdx := Mux(needModifyFtqIdxOffset, firstVInstrFtqPtr, deqDispatchData.ftqIdx)
  io.flushOut.bits.ftqOffset := Mux(needModifyFtqIdxOffset, firstVInstrFtqOffset, deqDispatchData.ftqOffset)
  io.flushOut.bits.level := Mux(deqHasReplayInst || intrEnable || exceptionEnable || needModifyFtqIdxOffset, RedirectLevel.flush, RedirectLevel.flushAfter) // TODO use this to implement "exception next"
  io.flushOut.bits.interrupt := true.B
  XSPerfAccumulate("interrupt_num", io.flushOut.valid && intrEnable)
  XSPerfAccumulate("exception_num", io.flushOut.valid && exceptionEnable)
  XSPerfAccumulate("flush_pipe_num", io.flushOut.valid && isFlushPipe)
  XSPerfAccumulate("replay_inst_num", io.flushOut.valid && isFlushPipe && deqHasReplayInst)

  val exceptionHappen = (state === s_idle) && valid(deqPtr.value) && (intrEnable || exceptionEnable) && !lastCycleFlush
  io.exception.valid                := RegNext(exceptionHappen)
  io.exception.bits.pc              := RegEnable(debug_deqUop.pc, exceptionHappen)
  io.exception.bits.instr           := RegEnable(debug_deqUop.instr, exceptionHappen)
  io.exception.bits.commitType      := RegEnable(deqDispatchData.commitType, exceptionHappen)
  io.exception.bits.exceptionVec    := RegEnable(exceptionDataRead.bits.exceptionVec, exceptionHappen)
  io.exception.bits.singleStep      := RegEnable(exceptionDataRead.bits.singleStep, exceptionHappen)
  io.exception.bits.crossPageIPFFix := RegEnable(exceptionDataRead.bits.crossPageIPFFix, exceptionHappen)
  io.exception.bits.isInterrupt     := RegEnable(intrEnable, exceptionHappen)
//  io.exception.bits.trigger := RegEnable(exceptionDataRead.bits.trigger, exceptionHappen)

  XSDebug(io.flushOut.valid,
    p"generate redirect: pc 0x${Hexadecimal(io.exception.bits.pc)} intr $intrEnable " +
    p"excp $exceptionEnable flushPipe $isFlushPipe " +
    p"Trap_target 0x${Hexadecimal(io.csr.trapTarget)} exceptionVec ${Binary(exceptionDataRead.bits.exceptionVec.asUInt)}\n")


  /**
    * Commits (and walk)
    * They share the same width.
    */
  val walkCounter = Reg(UInt(log2Up(RobSize + 1).W))
  val shouldWalkVec = VecInit((0 until CommitWidth).map(_.U < walkCounter))
  val walkFinished = walkCounter <= CommitWidth.U
  rab.io.robWalkEnd := state === s_walk && walkFinished

  require(RenameWidth <= CommitWidth)

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

  val vxsat = Wire(Valid(Bool()))
  vxsat.valid := io.commits.isCommit && vxsat.bits
  vxsat.bits := io.commits.commitValid.zip(vxsatDataRead).map {
    case (valid, vxsat) => valid & vxsat
  }.reduce(_ | _)

  // when mispredict branches writeback, stop commit in the next 2 cycles
  // TODO: don't check all exu write back
  val misPredWb = Cat(VecInit(redirectWBs.map(wb =>
    wb.bits.redirect.get.bits.cfiUpdate.isMisPred && wb.bits.redirect.get.valid && wb.valid
  ))).orR
  val misPredBlockCounter = Reg(UInt(3.W))
  misPredBlockCounter := Mux(misPredWb,
    "b111".U,
    misPredBlockCounter >> 1.U
  )
  val misPredBlock = misPredBlockCounter(0)
  val blockCommit = misPredBlock && !io.flushOut.valid || isReplaying || lastCycleFlush || hasWFI

  io.commits.isWalk := state === s_walk
  io.commits.isCommit := state === s_idle && !blockCommit
  val walk_v = VecInit(walkPtrVec.map(ptr => valid(ptr.value)))
  val commit_v = VecInit(deqPtrVec.map(ptr => valid(ptr.value)))
  // store will be commited iff both sta & std have been writebacked
  val commit_w = VecInit(deqPtrVec.map(ptr => isWritebacked(ptr.value)))
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

    when (state === s_walk) {
      io.commits.walkValid(i) := shouldWalkVec(i)
      when (io.commits.isWalk && state === s_walk && shouldWalkVec(i)) {
        XSError(!walk_v(i), s"why not $i???\n")
      }
    }

    XSInfo(io.commits.isCommit && io.commits.commitValid(i),
      "retired pc %x wen %d ldest %d pdest %x old_pdest %x data %x fflags: %b vxsat: %b\n",
      debug_microOp(deqPtrVec(i).value).pc,
      io.commits.info(i).rfWen,
      io.commits.info(i).ldest,
      io.commits.info(i).pdest,
      io.commits.info(i).old_pdest,
      debug_exuData(deqPtrVec(i).value),
      fflagsDataRead(i),
      vxsatDataRead(i)
    )
    XSInfo(state === s_walk && io.commits.walkValid(i), "walked pc %x wen %d ldst %d data %x\n",
      debug_microOp(walkPtrVec(i).value).pc,
      io.commits.info(i).rfWen,
      io.commits.info(i).ldest,
      debug_exuData(walkPtrVec(i).value)
    )
  }
  if (env.EnableDifftest) {
    io.commits.info.map(info => dontTouch(info.pc))
  }

  // sync fflags/dirty_fs/vxsat to csr
  io.csr.fflags := RegNext(fflags)
  io.csr.dirty_fs := RegNext(dirty_fs)
  io.csr.vxsat := RegNext(vxsat)

  // sync v csr to csr
  // for difftest
  if(env.AlwaysBasicDiff || env.EnableDifftest) {
    val isDiffWriteVconfigVec = io.diffCommits.commitValid.zip(io.diffCommits.info).map { case (valid, info) => valid && info.ldest === VCONFIG_IDX.U && info.vecWen }.reverse
    io.csr.vcsrFlag := RegNext(io.diffCommits.isCommit && Cat(isDiffWriteVconfigVec).orR)
  }
  else{
    io.csr.vcsrFlag := false.B
  }

  // commit load/store to lsq
  val ldCommitVec = VecInit((0 until CommitWidth).map(i => io.commits.commitValid(i) && io.commits.info(i).commitType === CommitType.LOAD))
  val stCommitVec = VecInit((0 until CommitWidth).map(i => io.commits.commitValid(i) && io.commits.info(i).commitType === CommitType.STORE))
  io.lsq.lcommit := RegNext(Mux(io.commits.isCommit, PopCount(ldCommitVec), 0.U))
  io.lsq.scommit := RegNext(Mux(io.commits.isCommit, PopCount(stCommitVec), 0.U))
  // indicate a pending load or store
  io.lsq.pendingld := RegNext(io.commits.isCommit && io.commits.info(0).commitType === CommitType.LOAD && valid(deqPtr.value) && mmio(deqPtr.value))
  io.lsq.pendingst := RegNext(io.commits.isCommit && io.commits.info(0).commitType === CommitType.STORE && valid(deqPtr.value))
  io.lsq.commit := RegNext(io.commits.isCommit && io.commits.commitValid(0))
  io.lsq.pendingPtr := RegNext(deqPtr)

  /**
    * state changes
    * (1) redirect: switch to s_walk
    * (2) walk: when walking comes to the end, switch to s_idle
    */
  val state_next = Mux(io.redirect.valid, s_walk, Mux(state === s_walk && walkFinished && rab.io.rabWalkEnd, s_idle, state))
  XSPerfAccumulate("s_idle_to_idle",            state === s_idle && state_next === s_idle)
  XSPerfAccumulate("s_idle_to_walk",            state === s_idle && state_next === s_walk)
  XSPerfAccumulate("s_walk_to_idle",            state === s_walk && state_next === s_idle)
  XSPerfAccumulate("s_walk_to_walk",            state === s_walk && state_next === s_walk)
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
  deqPtrGenModule.io.hasNoSpecExec := hasWaitForward
  deqPtrGenModule.io.interrupt_safe := interrupt_safe(deqPtr.value)
  deqPtrGenModule.io.blockCommit := blockCommit
  deqPtrVec := deqPtrGenModule.io.out
  val deqPtrVec_next = deqPtrGenModule.io.next_out

  val enqPtrGenModule = Module(new RobEnqPtrWrapper)
  enqPtrGenModule.io.redirect := io.redirect
  enqPtrGenModule.io.allowEnqueue := allowEnqueue
  enqPtrGenModule.io.hasBlockBackward := hasBlockBackward
  enqPtrGenModule.io.enq := VecInit(io.enq.req.map(req => req.valid && req.bits.firstUop))
  enqPtrVec := enqPtrGenModule.io.out

  val thisCycleWalkCount = Mux(walkFinished, walkCounter, CommitWidth.U)
  // next walkPtrVec:
  // (1) redirect occurs: update according to state
  // (2) walk: move forwards
  val walkPtrVec_next = Mux(io.redirect.valid,
    deqPtrVec_next,
    Mux(state === s_walk, VecInit(walkPtrVec.map(_ + CommitWidth.U)), walkPtrVec)
  )
  walkPtrVec := walkPtrVec_next

  val numValidEntries = distanceBetween(enqPtr, deqPtr)
  val commitCnt = PopCount(io.commits.commitValid)

  allowEnqueue := numValidEntries + dispatchNum <= (RobSize - RenameWidth).U

  val currentWalkPtr = Mux(state === s_walk, walkPtr, deqPtrVec_next(0))
  val redirectWalkDistance = distanceBetween(io.redirect.bits.robIdx, deqPtrVec_next(0))
  when (io.redirect.valid) {
    // full condition:
    // +& is used here because:
    // When rob is full and the tail instruction causes a misprediction,
    // the redirect robIdx is the deqPtr - 1. In this case, redirectWalkDistance
    // is RobSize - 1.
    // Since misprediction does not flush the instruction itself, flushItSelf is false.B.
    // Previously we use `+` to count the walk distance and it causes overflows
    // when RobSize is power of 2. We change it to `+&` to allow walkCounter to be RobSize.
    // The width of walkCounter also needs to be changed.
    // empty condition:
    // When the last instruction in ROB commits and causes a flush, a redirect
    // will be raised later. In such circumstances, the redirect robIdx is before
    // the deqPtrVec_next(0) and will cause underflow.
    walkCounter := Mux(isBefore(io.redirect.bits.robIdx, deqPtrVec_next(0)), 0.U,
                       redirectWalkDistance +& !io.redirect.bits.flushItself())
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

  // redirect logic writes 6 valid
  val redirectHeadVec = Reg(Vec(RenameWidth, new RobPtr))
  val redirectTail = Reg(new RobPtr)
  val redirectIdle :: redirectBusy :: Nil = Enum(2)
  val redirectState = RegInit(redirectIdle)
  val invMask = redirectHeadVec.map(redirectHead => isBefore(redirectHead, redirectTail))
  when(redirectState === redirectBusy) {
    redirectHeadVec.foreach(redirectHead => redirectHead := redirectHead + RenameWidth.U)
    redirectHeadVec zip invMask foreach {
      case (redirectHead, inv) => when(inv) {
        valid(redirectHead.value) := false.B
      }
    }
    when(!invMask.last) {
      redirectState := redirectIdle
    }
  }
  when(io.redirect.valid) {
    redirectState := redirectBusy
    when(redirectState === redirectIdle) {
      redirectTail := enqPtr
    }
    redirectHeadVec.zipWithIndex.foreach { case (redirectHead, i) =>
      redirectHead := Mux(io.redirect.bits.flushItself(), io.redirect.bits.robIdx + i.U, io.redirect.bits.robIdx + (i + 1).U)
    }
  }
  // enqueue logic writes 6 valid
  for (i <- 0 until RenameWidth) {
    when (canEnqueue(i) && !io.redirect.valid) {
      valid(allocatePtrVec(i).value) := true.B
    }
  }
  // dequeue logic writes 6 valid
  for (i <- 0 until CommitWidth) {
    val commitValid = io.commits.isCommit && io.commits.commitValid(i)
    when (commitValid) {
      valid(commitReadAddr(i)) := false.B
    }
  }

  // writeback logic set numWbPorts writebacked to true
  val blockWbSeq = Wire(Vec(exuWBs.length, Bool()))
  blockWbSeq.map(_ := false.B)
  for ((wb, blockWb) <- exuWBs.zip(blockWbSeq)) {
    when(wb.valid) {
      val wbHasException = wb.bits.exceptionVec.getOrElse(0.U).asUInt.orR
      val wbHasTriggerHit = false.B //Todo: wb.bits.trigger.getHitBackend
      val wbHasFlushPipe = wb.bits.flushPipe.getOrElse(false.B)
      val wbHasReplayInst = wb.bits.replay.getOrElse(false.B) //Todo: && wb.bits.replayInst
      blockWb := wbHasException || wbHasFlushPipe || wbHasReplayInst || wbHasTriggerHit
    }
  }

  // if the first uop of an instruction is valid , write writebackedCounter
  val uopEnqValidSeq = io.enq.req.map(req => io.enq.canAccept && req.valid)
  val instEnqValidSeq = io.enq.req.map (req => io.enq.canAccept && req.valid && req.bits.firstUop)
  val enqNeedWriteRFSeq = io.enq.req.map(_.bits.needWriteRf)
  val enqRobIdxSeq = io.enq.req.map(req => req.bits.robIdx.value)
  val enqUopNumVec = VecInit(io.enq.req.map(req => req.bits.numUops))
  val enqEliminatedMoveVec = VecInit(io.enq.req.map(req => req.bits.eliminatedMove))

  private val enqWriteStdVec: Vec[Bool] = VecInit(io.enq.req.map {
    req => FuType.isAMO(req.bits.fuType) || FuType.isStore(req.bits.fuType)
  })
  val enqWbSizeSeq = io.enq.req.map { req =>
    val enqHasException = ExceptionNO.selectFrontend(req.bits.exceptionVec).asUInt.orR
    val enqHasTriggerHit = req.bits.trigger.getHitFrontend
    Mux(req.bits.eliminatedMove, Mux(enqHasException || enqHasTriggerHit, 1.U, 0.U),
      Mux(FuType.isAMO(req.bits.fuType) || FuType.isStore(req.bits.fuType), 2.U, 1.U))
  }
  val enqWbSizeSumSeq = enqRobIdxSeq.zipWithIndex.map { case (robIdx, idx) =>
    val addend = enqRobIdxSeq.zip(enqWbSizeSeq).take(idx + 1).map { case (uopRobIdx, uopWbSize) => Mux(robIdx === uopRobIdx, uopWbSize, 0.U) }
    addend.reduce(_ +& _)
  }
  val fflags_wb = fflagsPorts
  val vxsat_wb = vxsatPorts
  for(i <- 0 until RobSize){

    val robIdxMatchSeq = io.enq.req.map(_.bits.robIdx.value === i.U)
    val uopCanEnqSeq = uopEnqValidSeq.zip(robIdxMatchSeq).map{ case(valid, isMatch) => valid && isMatch }
    val instCanEnqSeq = instEnqValidSeq.zip(robIdxMatchSeq).map{ case(valid, isMatch) => valid && isMatch }
    val instCanEnqFlag = Cat(instCanEnqSeq).orR

    realDestSize(i) := Mux(!valid(i) && instCanEnqFlag || valid(i), realDestSize(i) + PopCount(enqNeedWriteRFSeq.zip(uopCanEnqSeq).map{ case(writeFlag, valid) => writeFlag && valid }), 0.U)

    val enqUopNum = PriorityMux(instCanEnqSeq, enqUopNumVec)
    val enqEliminatedMove = PriorityMux(instCanEnqSeq, enqEliminatedMoveVec)
    val enqWriteStd = PriorityMux(instCanEnqSeq, enqWriteStdVec)

    val canWbSeq = exuWBs.map(writeback => writeback.valid && writeback.bits.robIdx.value === i.U)
    val canWbNoBlockSeq = canWbSeq.zip(blockWbSeq).map{ case(canWb, blockWb) => canWb && !blockWb }
    val canStdWbSeq = VecInit(stdWBs.map(writeback => writeback.valid && writeback.bits.robIdx.value === i.U))
    val wbCnt = PopCount(canWbNoBlockSeq)
    when (exceptionGen.io.out.valid && exceptionGen.io.out.bits.robIdx.value === i.U) {
      // exception flush
      uopNumVec(i) := 0.U
      stdWritebacked(i) := true.B
    }.elsewhen(!valid(i) && instCanEnqFlag) {
      // enq set num of uops
      uopNumVec(i) := Mux(enqEliminatedMove, 0.U, enqUopNum)
      stdWritebacked(i) := Mux(enqWriteStd, false.B, true.B)
    }.elsewhen(valid(i)) {
      // update by writing back
      uopNumVec(i) := uopNumVec(i) - wbCnt
      when (canStdWbSeq.asUInt.orR) {
        stdWritebacked(i) := true.B
      }
    }.otherwise {
      uopNumVec(i) := 0.U
    }

    val fflagsCanWbSeq = fflags_wb.map(writeback => writeback.valid && writeback.bits.robIdx.value === i.U)
    val fflagsRes = fflagsCanWbSeq.zip(fflags_wb).map { case (canWb, wb) => Mux(canWb, wb.bits.fflags.get, 0.U) }.fold(false.B)(_ | _)
    fflagsDataModule(i) := Mux(!valid(i) && instCanEnqFlag, 0.U, fflagsDataModule(i) | fflagsRes)

    val vxsatCanWbSeq = vxsat_wb.map(writeback => writeback.valid && writeback.bits.robIdx.value === i.U)
    val vxsatRes = vxsatCanWbSeq.zip(vxsat_wb).map { case (canWb, wb) => Mux(canWb, wb.bits.vxsat.get, 0.U) }.fold(false.B)(_ | _)
    vxsatDataModule(i) := Mux(!valid(i) && instCanEnqFlag, 0.U, vxsatDataModule(i) | vxsatRes)
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
      val allow_interrupts = !CommitType.isLoadStore(io.enq.req(i).bits.commitType)
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
    wdata.ldest := req.ldest
    wdata.rfWen := req.rfWen
    wdata.fpWen := req.fpWen
    wdata.vecWen := req.vecWen
    wdata.wflags := req.fpu.wflags
    wdata.commitType := req.commitType
    wdata.pdest := req.pdest
    wdata.old_pdest := req.oldPdest
    wdata.ftqIdx := req.ftqPtr
    wdata.ftqOffset := req.ftqOffset
    wdata.isMove := req.eliminatedMove
    wdata.pc := req.pc
    wdata.vtype := req.vpu.vtype
    wdata.isVset := req.isVset
  }
  dispatchData.io.raddr := commitReadAddr_next

  exceptionGen.io.redirect <> io.redirect
  exceptionGen.io.flush := io.flushOut.valid

  val canEnqueueEG = VecInit(io.enq.req.map(req => req.valid && io.enq.canAccept))
  for (i <- 0 until RenameWidth) {
    exceptionGen.io.enq(i).valid := canEnqueueEG(i)
    exceptionGen.io.enq(i).bits.robIdx := io.enq.req(i).bits.robIdx
    exceptionGen.io.enq(i).bits.exceptionVec := ExceptionNO.selectFrontend(io.enq.req(i).bits.exceptionVec)
    exceptionGen.io.enq(i).bits.flushPipe := io.enq.req(i).bits.flushPipe
    exceptionGen.io.enq(i).bits.isVset := io.enq.req(i).bits.isVset
    exceptionGen.io.enq(i).bits.replayInst := false.B
    XSError(canEnqueue(i) && io.enq.req(i).bits.replayInst, "enq should not set replayInst")
    exceptionGen.io.enq(i).bits.singleStep := io.enq.req(i).bits.singleStep
    exceptionGen.io.enq(i).bits.crossPageIPFFix := io.enq.req(i).bits.crossPageIPFFix
    exceptionGen.io.enq(i).bits.trigger.clear()
    exceptionGen.io.enq(i).bits.trigger.frontendHit := io.enq.req(i).bits.trigger.frontendHit
  }

  println(s"ExceptionGen:")
  println(s"num of exceptions: ${params.numException}")
  require(exceptionWBs.length == exceptionGen.io.wb.length,
    f"exceptionWBs.length: ${exceptionWBs.length}, " +
      f"exceptionGen.io.wb.length: ${exceptionGen.io.wb.length}")
  for (((wb, exc_wb), i) <- exceptionWBs.zip(exceptionGen.io.wb).zipWithIndex) {
    exc_wb.valid                := wb.valid
    exc_wb.bits.robIdx          := wb.bits.robIdx
    exc_wb.bits.exceptionVec    := wb.bits.exceptionVec.get
    exc_wb.bits.flushPipe       := wb.bits.flushPipe.getOrElse(false.B)
    exc_wb.bits.isVset          := false.B
    exc_wb.bits.replayInst      := wb.bits.replay.getOrElse(false.B)
    exc_wb.bits.singleStep      := false.B
    exc_wb.bits.crossPageIPFFix := false.B
    exc_wb.bits.trigger         := 0.U.asTypeOf(exc_wb.bits.trigger) // Todo
//    println(s"  [$i] ${configs.map(_.name)}: exception ${exceptionCases(i)}, " +
//      s"flushPipe ${configs.exists(_.flushPipe)}, " +
//      s"replayInst ${configs.exists(_.replayInst)}")
  }

  fflagsDataRead := (0 until CommitWidth).map(i => fflagsDataModule(deqPtrVec(i).value))
  vxsatDataRead := (0 until CommitWidth).map(i => vxsatDataModule(deqPtrVec(i).value))

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
  XSError(isBefore(enqPtr, deqPtr) && !isFull(enqPtr, deqPtr), "\ndeqPtr is older than enqPtr!\n")
  for(i <- 0 until RobSize){
    XSDebug(false, !valid(i), "-")
    XSDebug(false, valid(i) && isWritebacked(i.U), "w")
    XSDebug(false, valid(i) && !isWritebacked(i.U), "v")
  }
  XSDebug(false, true.B, "\n")

  for(i <- 0 until RobSize) {
    if(i % 4 == 0) XSDebug("")
    XSDebug(false, true.B, "%x ", debug_microOp(i).pc)
    XSDebug(false, !valid(i), "- ")
    XSDebug(false, valid(i) && isWritebacked(i.U), "w ")
    XSDebug(false, valid(i) && !isWritebacked(i.U), "v ")
    if(i % 4 == 3) XSDebug(false, true.B, "\n")
  }

  def ifCommit(counter: UInt): UInt = Mux(io.commits.isCommit, counter, 0.U)
  def ifCommitReg(counter: UInt): UInt = Mux(RegNext(io.commits.isCommit), counter, 0.U)

  val commitDebugUop = deqPtrVec.map(_.value).map(debug_microOp(_))
  XSPerfAccumulate("clock_cycle", 1.U)
  QueuePerf(RobSize, PopCount((0 until RobSize).map(valid(_))), !allowEnqueue)
  XSPerfAccumulate("commitUop", ifCommit(commitCnt))
  XSPerfAccumulate("commitInstr", ifCommitReg(trueCommitCnt))
  val commitIsMove = commitDebugUop.map(_.isMove)
  XSPerfAccumulate("commitInstrMove", ifCommit(PopCount(io.commits.commitValid.zip(commitIsMove).map{ case (v, m) => v && m })))
  val commitMoveElim = commitDebugUop.map(_.debugInfo.eliminatedMove)
  XSPerfAccumulate("commitInstrMoveElim", ifCommit(PopCount(io.commits.commitValid zip commitMoveElim map { case (v, e) => v && e })))
  XSPerfAccumulate("commitInstrFused", ifCommitReg(fuseCommitCnt))
  val commitIsLoad = io.commits.info.map(_.commitType).map(_ === CommitType.LOAD)
  val commitLoadValid = io.commits.commitValid.zip(commitIsLoad).map{ case (v, t) => v && t }
  XSPerfAccumulate("commitInstrLoad", ifCommit(PopCount(commitLoadValid)))
  val commitIsBranch = io.commits.info.map(_.commitType).map(_ === CommitType.BRANCH)
  val commitBranchValid = io.commits.commitValid.zip(commitIsBranch).map{ case (v, t) => v && t }
  XSPerfAccumulate("commitInstrBranch", ifCommit(PopCount(commitBranchValid)))
  val commitLoadWaitBit = commitDebugUop.map(_.loadWaitBit)
  XSPerfAccumulate("commitInstrLoadWait", ifCommit(PopCount(commitLoadValid.zip(commitLoadWaitBit).map{ case (v, w) => v && w })))
  val commitIsStore = io.commits.info.map(_.commitType).map(_ === CommitType.STORE)
  XSPerfAccumulate("commitInstrStore", ifCommit(PopCount(io.commits.commitValid.zip(commitIsStore).map{ case (v, t) => v && t })))
  XSPerfAccumulate("writeback", PopCount((0 until RobSize).map(i => valid(i) && isWritebacked(i.U))))
  // XSPerfAccumulate("enqInstr", PopCount(io.dp1Req.map(_.fire)))
  // XSPerfAccumulate("d2rVnR", PopCount(io.dp1Req.map(p => p.valid && !p.ready)))
  XSPerfAccumulate("walkInstr", Mux(io.commits.isWalk, PopCount(io.commits.walkValid), 0.U))
  XSPerfAccumulate("walkCycle", state === s_walk)
  val deqNotWritebacked = valid(deqPtr.value) && !isWritebacked(deqPtr.value)
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
    val commitIsFuType = io.commits.commitValid.zip(commitDebugUop).map(x => x._1 && x._2.fuType === fuType.U )
    XSPerfAccumulate(s"${fuName}_instr_cnt", ifCommit(PopCount(commitIsFuType)))
    XSPerfAccumulate(s"${fuName}_latency_dispatch", ifCommit(latencySum(commitIsFuType, dispatchLatency)))
    XSPerfAccumulate(s"${fuName}_latency_enq_rs", ifCommit(latencySum(commitIsFuType, enqRsLatency)))
    XSPerfAccumulate(s"${fuName}_latency_select", ifCommit(latencySum(commitIsFuType, selectLatency)))
    XSPerfAccumulate(s"${fuName}_latency_issue", ifCommit(latencySum(commitIsFuType, issueLatency)))
    XSPerfAccumulate(s"${fuName}_latency_execute", ifCommit(latencySum(commitIsFuType, executeLatency)))
    XSPerfAccumulate(s"${fuName}_latency_enq_rs_execute", ifCommit(latencySum(commitIsFuType, rsFuLatency)))
    XSPerfAccumulate(s"${fuName}_latency_commit", ifCommit(latencySum(commitIsFuType, commitLatency)))
    if (fuType == FuType.fmac) {
      val commitIsFma = commitIsFuType.zip(commitDebugUop).map(x => x._1 && x._2.fpu.ren3 )
      XSPerfAccumulate(s"${fuName}_instr_cnt_fma", ifCommit(PopCount(commitIsFma)))
      XSPerfAccumulate(s"${fuName}_latency_enq_rs_execute_fma", ifCommit(latencySum(commitIsFma, rsFuLatency)))
      XSPerfAccumulate(s"${fuName}_latency_execute_fma", ifCommit(latencySum(commitIsFma, executeLatency)))
    }
  }

  //difftest signals
  val firstValidCommit = (deqPtr + PriorityMux(io.commits.commitValid, VecInit(List.tabulate(CommitWidth)(_.U(log2Up(CommitWidth).W))))).value

  val wdata = Wire(Vec(CommitWidth, UInt(XLEN.W)))
  val wpc = Wire(Vec(CommitWidth, UInt(XLEN.W)))

  for(i <- 0 until CommitWidth) {
    val idx = deqPtrVec(i).value
    wdata(i) := debug_exuData(idx)
    wpc(i) := SignExt(commitDebugUop(i).pc, XLEN)
  }

  if (env.EnableDifftest) {
    for (i <- 0 until CommitWidth) {
      val difftest = Module(new DifftestInstrCommit)
      // assgin default value
      difftest.io := DontCare

      difftest.io.clock    := clock
      difftest.io.coreid   := io.hartId
      difftest.io.index    := i.U

      val ptr = deqPtrVec(i).value
      val uop = commitDebugUop(i)
      val exuOut = debug_exuDebug(ptr)
      val exuData = debug_exuData(ptr)
      difftest.io.valid    := RegNext(RegNext(RegNext(io.commits.commitValid(i) && io.commits.isCommit)))
      difftest.io.pc       := RegNext(RegNext(RegNext(SignExt(uop.pc, XLEN))))
      difftest.io.instr    := RegNext(RegNext(RegNext(uop.instr)))
      difftest.io.robIdx   := RegNext(RegNext(RegNext(ZeroExt(ptr, 10))))
      difftest.io.lqIdx    := RegNext(RegNext(RegNext(ZeroExt(uop.lqIdx.value, 7))))
      difftest.io.sqIdx    := RegNext(RegNext(RegNext(ZeroExt(uop.sqIdx.value, 7))))
      difftest.io.isLoad   := RegNext(RegNext(RegNext(io.commits.info(i).commitType === CommitType.LOAD)))
      difftest.io.isStore  := RegNext(RegNext(RegNext(io.commits.info(i).commitType === CommitType.STORE)))
      difftest.io.special  := RegNext(RegNext(RegNext(CommitType.isFused(io.commits.info(i).commitType))))
      // when committing an eliminated move instruction,
      // we must make sure that skip is properly set to false (output from EXU is random value)
      difftest.io.skip     := RegNext(RegNext(RegNext(Mux(uop.eliminatedMove, false.B, exuOut.isMMIO || exuOut.isPerfCnt))))
      difftest.io.isRVC    := RegNext(RegNext(RegNext(uop.preDecodeInfo.isRVC)))
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
        dt_isRVC(allocatePtrVec(i).value) := io.enq.req(i).bits.preDecodeInfo.isRVC
      }
    }
    for (wb <- exuWBs) {
      when (wb.valid) {
        val wbIdx = wb.bits.robIdx.value
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
      difftest.io.opType := RegNext(RegNext(RegNext(uop.fuOpType)))
      difftest.io.fuType := RegNext(RegNext(RegNext(uop.fuType)))
    }
  }

  // Always instantiate basic difftest modules.
  if (env.EnableDifftest) {
    val dt_isXSTrap = Mem(RobSize, Bool())
    for (i <- 0 until RenameWidth) {
      when (canEnqueue(i)) {
        dt_isXSTrap(allocatePtrVec(i).value) := io.enq.req(i).bits.isXSTrap
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
        dt_isXSTrap(allocatePtrVec(i).value) := io.enq.req(i).bits.isXSTrap
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

  val validEntriesBanks = (0 until (RobSize + 63) / 64).map(i => RegNext(PopCount(valid.drop(i * 64).take(64))))
  val validEntries = RegNext(ParallelOperation(validEntriesBanks, (a: UInt, b: UInt) => a +& b))
  val commitMoveVec = VecInit(io.commits.commitValid.zip(commitIsMove).map{ case (v, m) => v && m })
  val commitLoadVec = VecInit(commitLoadValid)
  val commitBranchVec = VecInit(commitBranchValid)
  val commitLoadWaitVec = VecInit(commitLoadValid.zip(commitLoadWaitBit).map{ case (v, w) => v && w })
  val commitStoreVec = VecInit(io.commits.commitValid.zip(commitIsStore).map{ case (v, t) => v && t })
  val perfEvents = Seq(
    ("rob_interrupt_num      ", io.flushOut.valid && intrEnable                                       ),
    ("rob_exception_num      ", io.flushOut.valid && exceptionEnable                                  ),
    ("rob_flush_pipe_num     ", io.flushOut.valid && isFlushPipe                                      ),
    ("rob_replay_inst_num    ", io.flushOut.valid && isFlushPipe && deqHasReplayInst                  ),
    ("rob_commitUop          ", ifCommit(commitCnt)                                                   ),
    ("rob_commitInstr        ", ifCommitReg(trueCommitCnt)                                            ),
    ("rob_commitInstrMove    ", ifCommitReg(PopCount(RegNext(commitMoveVec)))                         ),
    ("rob_commitInstrFused   ", ifCommitReg(fuseCommitCnt)                                            ),
    ("rob_commitInstrLoad    ", ifCommitReg(PopCount(RegNext(commitLoadVec)))                         ),
    ("rob_commitInstrBranch  ", ifCommitReg(PopCount(RegNext(commitBranchVec)))                       ),
    ("rob_commitInstrLoadWait", ifCommitReg(PopCount(RegNext(commitLoadWaitVec)))                     ),
    ("rob_commitInstrStore   ", ifCommitReg(PopCount(RegNext(commitStoreVec)))                        ),
    ("rob_walkInstr          ", Mux(io.commits.isWalk, PopCount(io.commits.walkValid), 0.U)           ),
    ("rob_walkCycle          ", (state === s_walk)                                                    ),
    ("rob_1_4_valid          ", validEntries <= (RobSize / 4).U                                       ),
    ("rob_2_4_valid          ", validEntries >  (RobSize / 4).U && validEntries <= (RobSize / 2).U    ),
    ("rob_3_4_valid          ", validEntries >  (RobSize / 2).U && validEntries <= (RobSize * 3 / 4).U),
    ("rob_4_4_valid          ", validEntries >  (RobSize * 3 / 4).U                                   ),
  )
  generatePerfEvent()
}
