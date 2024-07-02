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

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import difftest._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import utility._
import utils._
import xiangshan._
import xiangshan.backend.BackendParams
import xiangshan.backend.Bundles.{DynInst, ExceptionInfo, ExuOutput}
import xiangshan.backend.fu.{FuConfig, FuType}
import xiangshan.frontend.FtqPtr
import xiangshan.mem.{LqPtr, LsqEnqIO, SqPtr}
import xiangshan.backend.Bundles.{DynInst, ExceptionInfo, ExuOutput}
import xiangshan.backend.ctrlblock.{DebugLSIO, DebugLsInfo, LsTopdownInfo}
import xiangshan.backend.fu.vector.Bundles.VType
import xiangshan.backend.rename.SnapshotGenerator
import yunsuan.VfaluType
import xiangshan.backend.rob.RobBundles._
import xiangshan.backend.trace._
import xiangshan.frontend.tracertl.ChiselRecordForField._

class Rob(params: BackendParams)(implicit p: Parameters) extends LazyModule with HasXSParameter {
  override def shouldBeInlined: Boolean = false

  lazy val module = new RobImp(this)(p, params)
}

class RobImp(override val wrapper: Rob)(implicit p: Parameters, params: BackendParams) extends LazyModuleImp(wrapper)
  with HasXSParameter with HasCircularQueuePtrHelper with HasPerfEvents {

  private val LduCnt = params.LduCnt
  private val StaCnt = params.StaCnt
  private val HyuCnt = params.HyuCnt

  val io = IO(new Bundle() {
    val hartId = Input(UInt(hartIdLen.W))
    val redirect = Input(Valid(new Redirect))
    val enq = new RobEnqIO
    val flushOut = ValidIO(new Redirect)
    val exception = ValidIO(new ExceptionInfo)
    // exu + brq
    val writeback: MixedVec[ValidIO[ExuOutput]] = Flipped(params.genWrite2CtrlBundles)
    val exuWriteback: MixedVec[ValidIO[ExuOutput]] = Flipped(params.genWrite2CtrlBundles)
    val writebackNums = Flipped(Vec(writeback.size - params.StdCnt, ValidIO(UInt(writeback.size.U.getWidth.W))))
    val writebackNeedFlush = Input(Vec(params.allExuParams.filter(_.needExceptionGen).length, Bool()))
    val commits = Output(new RobCommitIO)
    val rabCommits = Output(new RabCommitIO)
    val diffCommits = if (backendParams.debugEn) Some(Output(new DiffCommitIO)) else None
    val isVsetFlushPipe = Output(Bool())
    val lsq = new RobLsqIO
    val robDeqPtr = Output(new RobPtr)
    val csr = new RobCSRIO
    val snpt = Input(new SnapshotPort)
    val robFull = Output(Bool())
    val headNotReady = Output(Bool())
    val cpu_halt = Output(Bool())
    val wfi_enable = Input(Bool())
    val toDecode = new Bundle {
      val isResumeVType = Output(Bool())
      val walkVType = ValidIO(VType())
      val commitVType = new Bundle {
        val vtype = ValidIO(VType())
        val hasVsetvl = Output(Bool())
      }
    }
    val readGPAMemAddr = ValidIO(new Bundle {
      val ftqPtr = new FtqPtr()
      val ftqOffset = UInt(log2Up(PredictWidth).W)
    })
    val readGPAMemData = Input(UInt(GPAddrBits.W))
    val vstartIsZero = Input(Bool())

    val debug_ls = Flipped(new DebugLSIO)
    val debugRobHead = Output(new DynInst)
    val debugEnqLsq = Input(new LsqEnqIO)
    val debugHeadLsIssue = Input(Bool())
    val lsTopdownInfo = Vec(LduCnt + HyuCnt, Input(new LsTopdownInfo))
    val debugTopDown = new Bundle {
      val toCore = new RobCoreTopDownIO
      val toDispatch = new RobDispatchTopDownIO
      val robHeadLqIdx = Valid(new LqPtr)
    }
    val debugRolling = new RobDebugRollingIO
  })

  val exuWBs: Seq[ValidIO[ExuOutput]] = io.exuWriteback.filter(!_.bits.params.hasStdFu).toSeq
  val stdWBs: Seq[ValidIO[ExuOutput]] = io.exuWriteback.filter(_.bits.params.hasStdFu).toSeq
  val fflagsWBs = io.exuWriteback.filter(x => x.bits.fflags.nonEmpty).toSeq
  val exceptionWBs = io.writeback.filter(x => x.bits.exceptionVec.nonEmpty).toSeq
  val redirectWBs = io.writeback.filter(x => x.bits.redirect.nonEmpty).toSeq
  val vxsatWBs = io.exuWriteback.filter(x => x.bits.vxsat.nonEmpty).toSeq
  val branchWBs = io.exuWriteback.filter(_.bits.params.hasBrhFu).toSeq
  val csrWBs = io.exuWriteback.filter(x => x.bits.params.hasCSR).toSeq

  val numExuWbPorts = exuWBs.length
  val numStdWbPorts = stdWBs.length
  val bankAddrWidth = log2Up(CommitWidth)

  println(s"Rob: size $RobSize, numExuWbPorts: $numExuWbPorts, numStdWbPorts: $numStdWbPorts, commitwidth: $CommitWidth")

  val rab = Module(new RenameBuffer(RabSize))
  val vtypeBuffer = Module(new VTypeBuffer(VTypeBufferSize))
  val bankNum = 8
  assert(RobSize % bankNum == 0, "RobSize % bankNum must be 0")
  val robEntries = Reg(Vec(RobSize, new RobEntryBundle))
  // pointers
  // For enqueue ptr, we don't duplicate it since only enqueue needs it.
  val enqPtrVec = Wire(Vec(RenameWidth, new RobPtr))
  val deqPtrVec = Wire(Vec(CommitWidth, new RobPtr))
  val walkPtrVec = Reg(Vec(CommitWidth, new RobPtr))
  val walkPtrTrue = Reg(new RobPtr)
  val lastWalkPtr = Reg(new RobPtr)
  val allowEnqueue = RegInit(true.B)

  /**
   * Enqueue (from dispatch)
   */
  // special cases
  val hasBlockBackward = RegInit(false.B)
  val hasWaitForward = RegInit(false.B)
  val doingSvinval = RegInit(false.B)
  val enqPtr = enqPtrVec(0)
  val deqPtr = deqPtrVec(0)
  val walkPtr = walkPtrVec(0)
  val allocatePtrVec = VecInit((0 until RenameWidth).map(i => enqPtrVec(PopCount(io.enq.req.take(i).map(req => req.valid && req.bits.firstUop)))))
  io.enq.canAccept := allowEnqueue && !hasBlockBackward && rab.io.canEnq && vtypeBuffer.io.canEnq
  io.enq.resp := allocatePtrVec
  val canEnqueue = VecInit(io.enq.req.map(req => req.valid && req.bits.firstUop && io.enq.canAccept))
  val timer = GTimer()
  // robEntries enqueue
  for (i <- 0 until RobSize) {
    val enqOH = VecInit(canEnqueue.zip(allocatePtrVec.map(_.value === i.U)).map(x => x._1 && x._2))
    assert(PopCount(enqOH) < 2.U, s"robEntries$i enqOH is not one hot")
    when(enqOH.asUInt.orR && !io.redirect.valid){
      connectEnq(robEntries(i), Mux1H(enqOH, io.enq.req.map(_.bits)))
    }
  }
  // robBanks0 include robidx : 0 8 16 24 32 ...
  val robBanks = VecInit((0 until bankNum).map(i => VecInit(robEntries.zipWithIndex.filter(_._2 % bankNum == i).map(_._1))))
  // each Bank has 20 Entries, read addr is one hot
  // all banks use same raddr
  val eachBankEntrieNum = robBanks(0).length
  val robBanksRaddrThisLine = RegInit(1.U(eachBankEntrieNum.W))
  val robBanksRaddrNextLine = Wire(UInt(eachBankEntrieNum.W))
  robBanksRaddrThisLine := robBanksRaddrNextLine
  val bankNumWidth = log2Up(bankNum)
  val deqPtrWidth = deqPtr.value.getWidth
  val robIdxThisLine = VecInit((0 until bankNum).map(i => Cat(deqPtr.value(deqPtrWidth - 1, bankNumWidth), i.U(bankNumWidth.W))))
  val robIdxNextLine = VecInit((0 until bankNum).map(i => Cat(deqPtr.value(deqPtrWidth - 1, bankNumWidth) + 1.U, i.U(bankNumWidth.W))))
  // robBanks read
  val robBanksRdataThisLine = VecInit(robBanks.map{ case bank =>
    Mux1H(robBanksRaddrThisLine, bank)
  })
  val robBanksRdataNextLine = VecInit(robBanks.map{ case bank =>
    val shiftBank = bank.drop(1) :+ bank(0)
    Mux1H(robBanksRaddrThisLine, shiftBank)
  })
  val robBanksRdataThisLineUpdate = Wire(Vec(CommitWidth, new RobEntryBundle))
  val robBanksRdataNextLineUpdate = Wire(Vec(CommitWidth, new RobEntryBundle))
  val commitValidThisLine = Wire(Vec(CommitWidth, Bool()))
  val hasCommitted = RegInit(VecInit(Seq.fill(CommitWidth)(false.B)))
  val donotNeedWalk = RegInit(VecInit(Seq.fill(CommitWidth)(false.B)))
  val allCommitted = Wire(Bool())

  when(allCommitted) {
    hasCommitted := 0.U.asTypeOf(hasCommitted)
  }.elsewhen(io.commits.isCommit){
    for (i <- 0 until CommitWidth){
      hasCommitted(i) := commitValidThisLine(i) || hasCommitted(i)
    }
  }
  allCommitted := io.commits.isCommit && commitValidThisLine.last
  val walkPtrHead = Wire(new RobPtr)
  val changeBankAddrToDeqPtr = (walkPtrVec.head + CommitWidth.U) > lastWalkPtr
  when(io.redirect.valid){
    robBanksRaddrNextLine := UIntToOH(walkPtrHead.value(walkPtrHead.value.getWidth-1, bankAddrWidth))
  }.elsewhen(allCommitted || io.commits.isWalk && !changeBankAddrToDeqPtr){
    robBanksRaddrNextLine := Mux(robBanksRaddrThisLine.head(1) === 1.U, 1.U, robBanksRaddrThisLine << 1)
  }.elsewhen(io.commits.isWalk && changeBankAddrToDeqPtr){
    robBanksRaddrNextLine := UIntToOH(deqPtr.value(deqPtr.value.getWidth-1, bankAddrWidth))
  }.otherwise(
    robBanksRaddrNextLine := robBanksRaddrThisLine
  )
  val robDeqGroup = Reg(Vec(bankNum, new RobCommitEntryBundle))
  val rawInfo = VecInit((0 until CommitWidth).map(i => robDeqGroup(deqPtrVec(i).value(bankAddrWidth-1, 0)))).toSeq
  val commitInfo = VecInit((0 until CommitWidth).map(i => robDeqGroup(deqPtrVec(i).value(bankAddrWidth-1,0)))).toSeq
  val walkInfo = VecInit((0 until CommitWidth).map(i => robDeqGroup(walkPtrVec(i).value(bankAddrWidth-1, 0)))).toSeq
  for (i <- 0 until CommitWidth) {
    connectCommitEntry(robDeqGroup(i), robBanksRdataThisLineUpdate(i))
    when(allCommitted){
      connectCommitEntry(robDeqGroup(i), robBanksRdataNextLineUpdate(i))
    }
  }

  // In each robentry, the ftqIdx and ftqOffset belong to the first instruction that was compressed,
  // that is Necessary when exceptions happen.
  // Update the ftqIdx and ftqOffset to correctly notify the frontend which instructions have been committed.
  for (i <- 0 until CommitWidth) {
    val lastOffset = (rawInfo(i).traceBlockInPipe.iretire - (1.U << rawInfo(i).traceBlockInPipe.ilastsize.asUInt)) +& rawInfo(i).ftqOffset
    commitInfo(i).ftqIdx := rawInfo(i).ftqIdx + lastOffset.head(1)
    commitInfo(i).ftqOffset := lastOffset.tail(1)
  }

  // data for debug
  // Warn: debug_* prefix should not exist in generated verilog.
  val debug_microOp = DebugMem(RobSize, new DynInst)
  val debug_exuData = Reg(Vec(RobSize, UInt(XLEN.W))) //for debug
  val debug_exuDebug = Reg(Vec(RobSize, new DebugBundle)) //for debug
  val debug_lsInfo = RegInit(VecInit(Seq.fill(RobSize)(DebugLsInfo.init)))
  val debug_lsTopdownInfo = RegInit(VecInit(Seq.fill(RobSize)(LsTopdownInfo.init)))
  val debug_lqIdxValid = RegInit(VecInit.fill(RobSize)(false.B))
  val debug_lsIssued = RegInit(VecInit.fill(RobSize)(false.B))

  val isEmpty = enqPtr === deqPtr
  val snptEnq = io.enq.canAccept && io.enq.req.map(x => x.valid && x.bits.snapshot).reduce(_ || _)
  val snapshotPtrVec = Wire(Vec(CommitWidth, new RobPtr))
  snapshotPtrVec(0) := io.enq.req(0).bits.robIdx
  for (i <- 1 until CommitWidth) {
    snapshotPtrVec(i) := snapshotPtrVec(0) + i.U
  }
  val snapshots = SnapshotGenerator(snapshotPtrVec, snptEnq, io.snpt.snptDeq, io.redirect.valid, io.snpt.flushVec)
  val debug_lsIssue = WireDefault(debug_lsIssued)
  debug_lsIssue(deqPtr.value) := io.debugHeadLsIssue

  /**
   * states of Rob
   */
  val s_idle :: s_walk :: Nil = Enum(2)
  val state = RegInit(s_idle)

  val tip_computing :: tip_stalled :: tip_walk :: tip_drained :: Nil = Enum(4)
  val tip_state = WireInit(0.U(4.W))
  when(!isEmpty) {  // One or more inst in ROB
    when(state === s_walk || io.redirect.valid) {
      tip_state := tip_walk
    }.elsewhen(io.commits.isCommit && PopCount(io.commits.commitValid) =/= 0.U) {
      tip_state := tip_computing
    }.otherwise {
      tip_state := tip_stalled
    }
  }.otherwise {
    tip_state := tip_drained
  }
  class TipEntry()(implicit p: Parameters) extends XSBundle {
    val state = UInt(4.W)
    val commits = new RobCommitIO()      // info of commit
    val redirect = Valid(new Redirect)   // info of redirect
    val redirect_pc = UInt(VAddrBits.W)  // PC of the redirect uop
    val debugLsInfo = new DebugLsInfo()
  }
  val tip_table = ChiselDB.createTable("Tip_" + p(XSCoreParamsKey).HartId.toString, new TipEntry)
  val tip_data = Wire(new TipEntry())
  tip_data.state := tip_state
  tip_data.commits := io.commits
  tip_data.redirect := io.redirect
  tip_data.redirect_pc := debug_microOp(io.redirect.bits.robIdx.value).pc
  tip_data.debugLsInfo := debug_lsInfo(io.commits.robIdx(0).value)
  tip_table.log(tip_data, true.B, "", clock, reset)

  val exceptionGen = Module(new ExceptionGen(params))
  val exceptionDataRead = exceptionGen.io.state
  val fflagsDataRead = Wire(Vec(CommitWidth, UInt(5.W)))
  val vxsatDataRead = Wire(Vec(CommitWidth, Bool()))
  io.robDeqPtr := deqPtr
  io.debugRobHead := debug_microOp(deqPtr.value)

  /**
   * connection of [[rab]]
   */
  rab.io.redirect.valid := io.redirect.valid

  rab.io.req.zip(io.enq.req).map { case (dest, src) =>
    dest.bits := src.bits
    dest.valid := src.valid && io.enq.canAccept
  }

  val walkDestSizeDeqGroup = RegInit(VecInit(Seq.fill(CommitWidth)(0.U(log2Up(MaxUopSize + 1).W))))
  val realDestSizeSeq = VecInit(robDeqGroup.zip(hasCommitted).map{case (r, h) => Mux(h, 0.U, r.realDestSize)})
  val walkDestSizeSeq = VecInit(robDeqGroup.zip(donotNeedWalk).map{case (r, d) => Mux(d, 0.U, r.realDestSize)})
  val commitSizeSumSeq = VecInit((0 until CommitWidth).map(i => realDestSizeSeq.take(i + 1).reduce(_ +& _)))
  val walkSizeSumSeq   = VecInit((0 until CommitWidth).map(i => walkDestSizeSeq.take(i + 1).reduce(_ +& _)))
  val commitSizeSumCond = VecInit(commitValidThisLine.zip(hasCommitted).map{case (c,h) => (c || h) && io.commits.isCommit})
  val walkSizeSumCond   = VecInit(io.commits.walkValid.zip(donotNeedWalk).map{case (w,d) => (w || d) && io.commits.isWalk})
  val commitSizeSum = PriorityMuxDefault(commitSizeSumCond.reverse.zip(commitSizeSumSeq.reverse), 0.U)
  val walkSizeSum   = PriorityMuxDefault(walkSizeSumCond.reverse.zip(walkSizeSumSeq.reverse), 0.U)

  rab.io.fromRob.commitSize := commitSizeSum
  rab.io.fromRob.walkSize := walkSizeSum
  rab.io.snpt := io.snpt
  rab.io.snpt.snptEnq := snptEnq

  io.rabCommits := rab.io.commits
  io.diffCommits.foreach(_ := rab.io.diffCommits.get)

  /**
   * connection of [[vtypeBuffer]]
   */

  vtypeBuffer.io.redirect.valid := io.redirect.valid

  vtypeBuffer.io.req.zip(io.enq.req).map { case (sink, source) =>
    sink.valid := source.valid && io.enq.canAccept
    sink.bits := source.bits
  }

  private val commitIsVTypeVec = VecInit(io.commits.commitValid.zip(io.commits.info).map { case (valid, info) => io.commits.isCommit && valid && info.isVset })
  private val walkIsVTypeVec = VecInit(io.commits.walkValid.zip(walkInfo).map { case (valid, info) => io.commits.isWalk && valid && info.isVset })
  vtypeBuffer.io.fromRob.commitSize := PopCount(commitIsVTypeVec)
  vtypeBuffer.io.fromRob.walkSize := PopCount(walkIsVTypeVec)
  vtypeBuffer.io.snpt := io.snpt
  vtypeBuffer.io.snpt.snptEnq := snptEnq
  io.toDecode.isResumeVType := vtypeBuffer.io.toDecode.isResumeVType
  io.toDecode.commitVType := vtypeBuffer.io.toDecode.commitVType
  io.toDecode.walkVType := vtypeBuffer.io.toDecode.walkVType

  // When blockBackward instruction leaves Rob (commit or walk), hasBlockBackward should be set to false.B
  // To reduce registers usage, for hasBlockBackward cases, we allow enqueue after ROB is empty.
  when(isEmpty) {
    hasBlockBackward := false.B
  }
  // When any instruction commits, hasNoSpecExec should be set to false.B
  when(io.commits.hasWalkInstr || io.commits.hasCommitInstr) {
    hasWaitForward := false.B
  }

  // The wait-for-interrupt (WFI) instruction waits in the ROB until an interrupt might need servicing.
  // io.csr.wfiEvent will be asserted if the WFI can resume execution, and we change the state to s_wfi_idle.
  // It does not affect how interrupts are serviced. Note that WFI is noSpecExec and it does not trigger interrupts.
  val hasWFI = RegInit(false.B)
  io.cpu_halt := hasWFI
  // WFI Timeout: 2^20 = 1M cycles
  val wfi_cycles = RegInit(0.U(20.W))
  when(hasWFI) {
    wfi_cycles := wfi_cycles + 1.U
  }.elsewhen(!hasWFI && RegNext(hasWFI)) {
    wfi_cycles := 0.U
  }
  val wfi_timeout = wfi_cycles.andR
  when(RegNext(RegNext(io.csr.wfiEvent)) || io.flushOut.valid || wfi_timeout) {
    hasWFI := false.B
  }

  for (i <- 0 until RenameWidth) {
    // we don't check whether io.redirect is valid here since redirect has higher priority
    when(canEnqueue(i)) {
      val enqUop = io.enq.req(i).bits
      val enqIndex = allocatePtrVec(i).value
      // store uop in data module and debug_microOp Vec
      debug_microOp(enqIndex) := enqUop
      debug_microOp(enqIndex).debugInfo.dispatchTime := timer
      debug_microOp(enqIndex).debugInfo.enqRsTime := timer
      debug_microOp(enqIndex).debugInfo.selectTime := timer
      debug_microOp(enqIndex).debugInfo.issueTime := timer
      debug_microOp(enqIndex).debugInfo.writebackTime := timer
      debug_microOp(enqIndex).debugInfo.tlbFirstReqTime := timer
      debug_microOp(enqIndex).debugInfo.tlbRespTime := timer
      debug_lsInfo(enqIndex) := DebugLsInfo.init
      debug_lsTopdownInfo(enqIndex) := LsTopdownInfo.init
      debug_lqIdxValid(enqIndex) := false.B
      debug_lsIssued(enqIndex) := false.B
      when (enqUop.waitForward) {
        hasWaitForward := true.B
      }
      val enqTriggerActionIsDebugMode = TriggerAction.isDmode(io.enq.req(i).bits.trigger)
      val enqHasException = ExceptionNO.selectFrontend(enqUop.exceptionVec).asUInt.orR
      // the begin instruction of Svinval enqs so mark doingSvinval as true to indicate this process
      when(!enqTriggerActionIsDebugMode && !enqHasException && enqUop.isSvinvalBegin(enqUop.flushPipe)) {
        doingSvinval := true.B
      }
      // the end instruction of Svinval enqs so clear doingSvinval
      when(!enqTriggerActionIsDebugMode && !enqHasException && enqUop.isSvinvalEnd(enqUop.flushPipe)) {
        doingSvinval := false.B
      }
      // when we are in the process of Svinval software code area , only Svinval.vma and end instruction of Svinval can appear
      assert(!doingSvinval || (enqUop.isSvinval(enqUop.flushPipe) || enqUop.isSvinvalEnd(enqUop.flushPipe) || enqUop.isNotSvinval))
      when(enqUop.isWFI && !enqHasException && !enqTriggerActionIsDebugMode) {
        hasWFI := true.B
      }

      robEntries(enqIndex).mmio := false.B
      robEntries(enqIndex).vls := enqUop.vlsInstr
    }
  }

  for (i <- 0 until RenameWidth) {
    val enqUop = io.enq.req(i)
    when(enqUop.valid && enqUop.bits.blockBackward && io.enq.canAccept) {
      hasBlockBackward := true.B
    }
  }

  val dispatchNum = Mux(io.enq.canAccept, PopCount(io.enq.req.map(req => req.valid && req.bits.firstUop)), 0.U)
  io.enq.isEmpty := RegNext(isEmpty && !VecInit(io.enq.req.map(_.valid)).asUInt.orR)

  when(!io.wfi_enable) {
    hasWFI := false.B
  }
  // sel vsetvl's flush position
  val vs_idle :: vs_waitVinstr :: vs_waitFlush :: Nil = Enum(3)
  val vsetvlState = RegInit(vs_idle)

  val firstVInstrFtqPtr = RegInit(0.U.asTypeOf(new FtqPtr))
  val firstVInstrFtqOffset = RegInit(0.U.asTypeOf(UInt(log2Up(PredictWidth).W)))
  val firstVInstrRobIdx = RegInit(0.U.asTypeOf(new RobPtr))

  val enq0 = io.enq.req(0)
  val enq0IsVset = enq0.bits.isVset && enq0.bits.lastUop && canEnqueue(0)
  val enq0IsVsetFlush = enq0IsVset && enq0.bits.flushPipe
  val enqIsVInstrVec = io.enq.req.zip(canEnqueue).map { case (req, fire) => FuType.isVArith(req.bits.fuType) && fire }
  // for vs_idle
  val firstVInstrIdle = PriorityMux(enqIsVInstrVec.zip(io.enq.req).drop(1) :+ (true.B, 0.U.asTypeOf(io.enq.req(0).cloneType)))
  // for vs_waitVinstr
  val enqIsVInstrOrVset = (enqIsVInstrVec(0) || enq0IsVset) +: enqIsVInstrVec.drop(1)
  val firstVInstrWait = PriorityMux(enqIsVInstrOrVset, io.enq.req)
  when(vsetvlState === vs_idle) {
    firstVInstrFtqPtr := firstVInstrIdle.bits.ftqPtr
    firstVInstrFtqOffset := firstVInstrIdle.bits.ftqOffset
    firstVInstrRobIdx := firstVInstrIdle.bits.robIdx
  }.elsewhen(vsetvlState === vs_waitVinstr) {
    when(Cat(enqIsVInstrOrVset).orR) {
      firstVInstrFtqPtr := firstVInstrWait.bits.ftqPtr
      firstVInstrFtqOffset := firstVInstrWait.bits.ftqOffset
      firstVInstrRobIdx := firstVInstrWait.bits.robIdx
    }
  }

  val hasVInstrAfterI = Cat(enqIsVInstrVec(0)).orR
  when(vsetvlState === vs_idle && !io.redirect.valid) {
    when(enq0IsVsetFlush) {
      vsetvlState := Mux(hasVInstrAfterI, vs_waitFlush, vs_waitVinstr)
    }
  }.elsewhen(vsetvlState === vs_waitVinstr) {
    when(io.redirect.valid) {
      vsetvlState := vs_idle
    }.elsewhen(Cat(enqIsVInstrOrVset).orR) {
      vsetvlState := vs_waitFlush
    }
  }.elsewhen(vsetvlState === vs_waitFlush) {
    when(io.redirect.valid) {
      vsetvlState := vs_idle
    }
  }

  // lqEnq
  io.debugEnqLsq.needAlloc.map(_(0)).zip(io.debugEnqLsq.req).foreach { case (alloc, req) =>
    when(io.debugEnqLsq.canAccept && alloc && req.valid) {
      debug_microOp(req.bits.robIdx.value).lqIdx := req.bits.lqIdx
      debug_lqIdxValid(req.bits.robIdx.value) := true.B
    }
  }

  // lsIssue
  when(io.debugHeadLsIssue) {
    debug_lsIssued(deqPtr.value) := true.B
  }

  /**
   * Writeback (from execution units)
   */
  for (wb <- exuWBs) {
    when(wb.valid) {
      val wbIdx = wb.bits.robIdx.value
      debug_exuData(wbIdx) := wb.bits.data(0)
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
          p"data 0x${Hexadecimal(wb.bits.data(0))} ldst ${debug_Uop.ldest} pdst ${debug_Uop.pdest} " +
          p"skip ${wb.bits.debug.isMMIO} robIdx: ${wb.bits.robIdx}\n"
      )
    }
  }

  val writebackNum = PopCount(exuWBs.map(_.valid))
  XSInfo(writebackNum =/= 0.U, "writebacked %d insts\n", writebackNum)

  for (i <- 0 until LoadPipelineWidth) {
    when(RegNext(io.lsq.mmio(i))) {
      robEntries(RegEnable(io.lsq.uop(i).robIdx, io.lsq.mmio(i)).value).mmio := true.B
    }
  }


  /**
   * RedirectOut: Interrupt and Exceptions
   */
  val deqDispatchData = robEntries(deqPtr.value)
  val debug_deqUop = debug_microOp(deqPtr.value)

  val deqPtrEntry = robDeqGroup(deqPtr.value(bankAddrWidth-1,0))
  val deqPtrEntryValid = deqPtrEntry.commit_v
  val intrBitSetReg = RegNext(io.csr.intrBitSet)
  val intrEnable = intrBitSetReg && !hasWaitForward && deqPtrEntry.interrupt_safe
  val deqNeedFlush = deqPtrEntry.needFlush && deqPtrEntry.commit_v && deqPtrEntry.commit_w
  val deqHitExceptionGenState = exceptionDataRead.valid && exceptionDataRead.bits.robIdx === deqPtr
  val deqNeedFlushAndHitExceptionGenState = deqNeedFlush && deqHitExceptionGenState
  val exceptionGenStateIsException = exceptionDataRead.bits.exceptionVec.asUInt.orR || exceptionDataRead.bits.singleStep || TriggerAction.isDmode(exceptionDataRead.bits.trigger)
  val deqHasException = deqNeedFlushAndHitExceptionGenState && exceptionGenStateIsException
  val deqHasFlushPipe = deqNeedFlushAndHitExceptionGenState && exceptionDataRead.bits.flushPipe
  val deqHasReplayInst = deqNeedFlushAndHitExceptionGenState && exceptionDataRead.bits.replayInst

  XSDebug(deqHasException && exceptionDataRead.bits.singleStep, "Debug Mode: Deq has singlestep exception\n")
  XSDebug(deqHasException && TriggerAction.isDmode(exceptionDataRead.bits.trigger), "Debug Mode: Deq has trigger entry debug Mode\n")

  val isFlushPipe = deqPtrEntry.commit_w && (deqHasFlushPipe || deqHasReplayInst)

  val isVsetFlushPipe = deqPtrEntry.commit_w && deqHasFlushPipe && exceptionDataRead.bits.isVset
  //  val needModifyFtqIdxOffset = isVsetFlushPipe && (vsetvlState === vs_waitFlush)
  val needModifyFtqIdxOffset = false.B
  io.isVsetFlushPipe := isVsetFlushPipe
  // io.flushOut will trigger redirect at the next cycle.
  // Block any redirect or commit at the next cycle.
  val lastCycleFlush = RegNext(io.flushOut.valid)

  io.flushOut.valid := (state === s_idle) && deqPtrEntryValid && (intrEnable || deqHasException || isFlushPipe) && !lastCycleFlush
  io.flushOut.bits := DontCare
  io.flushOut.bits.isRVC := deqDispatchData.isRVC
  io.flushOut.bits.robIdx := Mux(needModifyFtqIdxOffset, firstVInstrRobIdx, deqPtr)
  io.flushOut.bits.ftqIdx := Mux(needModifyFtqIdxOffset, firstVInstrFtqPtr, deqDispatchData.ftqIdx)
  io.flushOut.bits.ftqOffset := Mux(needModifyFtqIdxOffset, firstVInstrFtqOffset, deqDispatchData.ftqOffset)
  io.flushOut.bits.level := Mux(deqHasReplayInst || intrEnable || deqHasException || needModifyFtqIdxOffset, RedirectLevel.flush, RedirectLevel.flushAfter) // TODO use this to implement "exception next"
  io.flushOut.bits.interrupt := true.B
  io.flushOut.bits.traceInfo := robEntries(deqPtr.value).traceInfo
  XSPerfAccumulate("interrupt_num", io.flushOut.valid && intrEnable)
  XSPerfAccumulate("exception_num", io.flushOut.valid && deqHasException)
  XSPerfAccumulate("flush_pipe_num", io.flushOut.valid && isFlushPipe)
  XSPerfAccumulate("replay_inst_num", io.flushOut.valid && isFlushPipe && deqHasReplayInst)

  val exceptionHappen = (state === s_idle) && deqPtrEntryValid && (intrEnable || deqHasException) && !lastCycleFlush
  io.exception.valid := RegNext(exceptionHappen)
  io.exception.bits.pc := RegEnable(debug_deqUop.pc, exceptionHappen)
  io.exception.bits.gpaddr := io.readGPAMemData
  io.exception.bits.instr := RegEnable(debug_deqUop.instr, exceptionHappen)
  io.exception.bits.commitType := RegEnable(deqDispatchData.commitType, exceptionHappen)
  io.exception.bits.exceptionVec := RegEnable(exceptionDataRead.bits.exceptionVec, exceptionHappen)
  io.exception.bits.singleStep := RegEnable(exceptionDataRead.bits.singleStep, exceptionHappen)
  io.exception.bits.crossPageIPFFix := RegEnable(exceptionDataRead.bits.crossPageIPFFix, exceptionHappen)
  io.exception.bits.isInterrupt := RegEnable(intrEnable, exceptionHappen)
  io.exception.bits.isHls := RegEnable(deqDispatchData.isHls, exceptionHappen)
  io.exception.bits.vls := RegEnable(robEntries(deqPtr.value).vls, exceptionHappen)
  io.exception.bits.trigger := RegEnable(exceptionDataRead.bits.trigger, exceptionHappen)

  // data will be one cycle after valid
  io.readGPAMemAddr.valid := exceptionHappen
  io.readGPAMemAddr.bits.ftqPtr := exceptionDataRead.bits.ftqPtr
  io.readGPAMemAddr.bits.ftqOffset := exceptionDataRead.bits.ftqOffset

  XSDebug(io.flushOut.valid,
    p"generate redirect: pc 0x${Hexadecimal(io.exception.bits.pc)} intr $intrEnable " +
      p"excp $deqHasException flushPipe $isFlushPipe " +
      p"Trap_target 0x${Hexadecimal(io.csr.trapTarget)} exceptionVec ${Binary(exceptionDataRead.bits.exceptionVec.asUInt)}\n")


  /**
   * Commits (and walk)
   * They share the same width.
   */
  // T redirect.valid, T+1 use walkPtrVec read robEntries, T+2 start walk, shouldWalkVec used in T+2
  val shouldWalkVec = Wire(Vec(CommitWidth,Bool()))
  val walkingPtrVec = RegNext(walkPtrVec)
  when(io.redirect.valid){
    shouldWalkVec := 0.U.asTypeOf(shouldWalkVec)
  }.elsewhen(RegNext(io.redirect.valid)){
    shouldWalkVec := 0.U.asTypeOf(shouldWalkVec)
  }.elsewhen(state === s_walk){
    shouldWalkVec := VecInit(walkingPtrVec.map(_ <= lastWalkPtr).zip(donotNeedWalk).map(x => x._1 && !x._2))
  }.otherwise(
    shouldWalkVec := 0.U.asTypeOf(shouldWalkVec)
  )
  val walkFinished = walkPtrTrue > lastWalkPtr
  rab.io.fromRob.walkEnd := state === s_walk && walkFinished
  vtypeBuffer.io.fromRob.walkEnd := state === s_walk && walkFinished

  require(RenameWidth <= CommitWidth)

  // wiring to csr
  val (wflags, dirtyFs) = (0 until CommitWidth).map(i => {
    val v = io.commits.commitValid(i)
    val info = io.commits.info(i)
    (v & info.wflags, v & info.dirtyFs)
  }).unzip
  val fflags = Wire(Valid(UInt(5.W)))
  fflags.valid := io.commits.isCommit && VecInit(wflags).asUInt.orR
  fflags.bits := wflags.zip(fflagsDataRead).map({
    case (w, f) => Mux(w, f, 0.U)
  }).reduce(_ | _)
  val dirtyVs = (0 until CommitWidth).map(i => {
    val v = io.commits.commitValid(i)
    val info = io.commits.info(i)
    v & info.dirtyVs
  })
  val dirty_fs = io.commits.isCommit && VecInit(dirtyFs).asUInt.orR
  val dirty_vs = io.commits.isCommit && VecInit(dirtyVs).asUInt.orR

  val resetVstart = dirty_vs && !io.vstartIsZero

  io.csr.vstart.valid := RegNext(Mux(exceptionHappen, exceptionDataRead.bits.vstartEn, resetVstart))
  io.csr.vstart.bits := RegNext(Mux(exceptionHappen, exceptionDataRead.bits.vstart, 0.U))

  val vxsat = Wire(Valid(Bool()))
  vxsat.valid := io.commits.isCommit && vxsat.bits
  vxsat.bits := io.commits.commitValid.zip(vxsatDataRead).map {
    case (valid, vxsat) => valid & vxsat
  }.reduce(_ | _)

  // when mispredict branches writeback, stop commit in the next 2 cycles
  // TODO: don't check all exu write back
  val misPredWb = Cat(VecInit(redirectWBs.map(wb =>
    wb.bits.redirect.get.bits.cfiUpdate.isMisPred && wb.bits.redirect.get.valid && wb.valid
  ).toSeq)).orR
  val misPredBlockCounter = Reg(UInt(3.W))
  misPredBlockCounter := Mux(misPredWb,
    "b111".U,
    misPredBlockCounter >> 1.U
  )
  val misPredBlock = misPredBlockCounter(0)
  val deqFlushBlockCounter = Reg(UInt(3.W))
  val deqFlushBlock = deqFlushBlockCounter(0)
  val deqHasFlushed = RegInit(false.B)
  val deqHasCommitted = io.commits.isCommit && io.commits.commitValid(0)
  val deqHitRedirectReg = RegNext(io.redirect.valid && io.redirect.bits.robIdx === deqPtr)
  when(deqNeedFlush && deqHitRedirectReg){
    deqFlushBlockCounter := "b111".U
  }.otherwise{
    deqFlushBlockCounter := deqFlushBlockCounter >> 1.U
  }
  when(deqHasCommitted){
    deqHasFlushed := false.B
  }.elsewhen(deqNeedFlush && io.flushOut.valid && !io.flushOut.bits.flushItself()){
    deqHasFlushed := true.B
  }
  val blockCommit = misPredBlock || lastCycleFlush || hasWFI || io.redirect.valid || (deqNeedFlush && !deqHasFlushed) || deqFlushBlock

  io.commits.isWalk := state === s_walk
  io.commits.isCommit := state === s_idle && !blockCommit

  val walk_v = VecInit(walkingPtrVec.map(ptr => robEntries(ptr.value).valid))
  val commit_vDeqGroup = VecInit(robDeqGroup.map(_.commit_v))
  val commit_wDeqGroup = VecInit(robDeqGroup.map(_.commit_w))
  val realCommitLast = deqPtrVec(0).lineHeadPtr + Fill(bankAddrWidth, 1.U)
  val commit_block = VecInit((0 until CommitWidth).map(i => !commit_wDeqGroup(i) && !hasCommitted(i)))
  val allowOnlyOneCommit = VecInit(robDeqGroup.map(x => x.commit_v && x.needFlush)).asUInt.orR || intrBitSetReg
  // for instructions that may block others, we don't allow them to commit
  io.commits.commitValid := PriorityMux(commitValidThisLine, (0 until CommitWidth).map(i => (commitValidThisLine.asUInt >> i).asUInt.asTypeOf(io.commits.commitValid)))

  for (i <- 0 until CommitWidth) {
    // defaults: state === s_idle and instructions commit
    // when intrBitSetReg, allow only one instruction to commit at each clock cycle
    val isBlocked = intrEnable || (deqNeedFlush && !deqHasFlushed && !deqHasFlushPipe)
    val isBlockedByOlder = if (i != 0) commit_block.asUInt(i, 0).orR || allowOnlyOneCommit && !hasCommitted.asUInt(i - 1, 0).andR else false.B
    commitValidThisLine(i) := commit_vDeqGroup(i) && commit_wDeqGroup(i) && !isBlocked && !isBlockedByOlder && !hasCommitted(i)
    io.commits.info(i) := commitInfo(i)
    io.commits.robIdx(i) := deqPtrVec(i)

    io.commits.walkValid(i) := shouldWalkVec(i)
    when(state === s_walk) {
      when(io.commits.isWalk && state === s_walk && shouldWalkVec(i)) {
        XSError(!walk_v(i), s"The walking entry($i) should be valid\n")
      }
    }

    XSInfo(io.commits.isCommit && io.commits.commitValid(i),
      "retired pc %x wen %d ldest %d pdest %x data %x fflags: %b vxsat: %b\n",
      debug_microOp(deqPtrVec(i).value).pc,
      io.commits.info(i).rfWen,
      io.commits.info(i).debug_ldest.getOrElse(0.U),
      io.commits.info(i).debug_pdest.getOrElse(0.U),
      debug_exuData(deqPtrVec(i).value),
      fflagsDataRead(i),
      vxsatDataRead(i)
    )
    XSInfo(state === s_walk && io.commits.walkValid(i), "walked pc %x wen %d ldst %d data %x\n",
      debug_microOp(walkPtrVec(i).value).pc,
      io.commits.info(i).rfWen,
      io.commits.info(i).debug_ldest.getOrElse(0.U),
      debug_exuData(walkPtrVec(i).value)
    )
  }

  // sync fflags/dirty_fs/vxsat to csr
  io.csr.fflags   := RegNextWithEnable(fflags)
  io.csr.dirty_fs := GatedValidRegNext(dirty_fs)
  io.csr.dirty_vs := GatedValidRegNext(dirty_vs)
  io.csr.vxsat    := RegNextWithEnable(vxsat)

  // commit load/store to lsq
  val ldCommitVec = VecInit((0 until CommitWidth).map(i => io.commits.commitValid(i) && io.commits.info(i).commitType === CommitType.LOAD))
  // TODO: Check if meet the require that only set scommit when commit scala store uop
  val stCommitVec = VecInit((0 until CommitWidth).map(i => io.commits.commitValid(i) && io.commits.info(i).commitType === CommitType.STORE && !robEntries(deqPtrVec(i).value).vls ))
  val deqPtrVec_next = Wire(Vec(CommitWidth, Output(new RobPtr)))
  io.lsq.lcommit := RegNext(Mux(io.commits.isCommit, PopCount(ldCommitVec), 0.U))
  io.lsq.scommit := RegNext(Mux(io.commits.isCommit, PopCount(stCommitVec), 0.U))
  // indicate a pending load or store
  io.lsq.pendingUncacheld := RegNext(io.commits.isCommit && io.commits.info(0).commitType === CommitType.LOAD && robEntries(deqPtr.value).valid && robEntries(deqPtr.value).mmio)
  io.lsq.pendingld := RegNext(io.commits.isCommit && io.commits.info(0).commitType === CommitType.LOAD && robEntries(deqPtr.value).valid)
  // TODO: Check if need deassert pendingst when it is vst
  io.lsq.pendingst := RegNext(io.commits.isCommit && io.commits.info(0).commitType === CommitType.STORE && robEntries(deqPtr.value).valid)
  // TODO: Check if set correctly when vector store is at the head of ROB
  io.lsq.pendingVst := RegNext(io.commits.isCommit && io.commits.info(0).commitType === CommitType.STORE && robEntries(deqPtr.value).valid && robEntries(deqPtr.value).vls)
  io.lsq.commit := RegNext(io.commits.isCommit && io.commits.commitValid(0))
  io.lsq.pendingPtr := RegNext(deqPtr)
  io.lsq.pendingPtrNext := RegNext(deqPtrVec_next.head)

  /**
   * state changes
   * (1) redirect: switch to s_walk
   * (2) walk: when walking comes to the end, switch to s_idle
   */
  val state_next = Mux(
    io.redirect.valid || RegNext(io.redirect.valid), s_walk,
    Mux(
      state === s_walk && walkFinished && rab.io.status.walkEnd && vtypeBuffer.io.status.walkEnd, s_idle,
      state
    )
  )
  XSPerfAccumulate("s_idle_to_idle", state === s_idle && state_next === s_idle)
  XSPerfAccumulate("s_idle_to_walk", state === s_idle && state_next === s_walk)
  XSPerfAccumulate("s_walk_to_idle", state === s_walk && state_next === s_idle)
  XSPerfAccumulate("s_walk_to_walk", state === s_walk && state_next === s_walk)
  state := state_next

  /**
   * pointers and counters
   */
  val deqPtrGenModule = Module(new NewRobDeqPtrWrapper)
  deqPtrGenModule.io.state := state
  deqPtrGenModule.io.deq_v := commit_vDeqGroup
  deqPtrGenModule.io.deq_w := commit_wDeqGroup
  deqPtrGenModule.io.exception_state := exceptionDataRead
  deqPtrGenModule.io.intrBitSetReg := intrBitSetReg
  deqPtrGenModule.io.hasNoSpecExec := hasWaitForward
  deqPtrGenModule.io.allowOnlyOneCommit := allowOnlyOneCommit
  deqPtrGenModule.io.interrupt_safe := robDeqGroup(deqPtr.value(bankAddrWidth-1,0)).interrupt_safe
  deqPtrGenModule.io.blockCommit := blockCommit
  deqPtrGenModule.io.hasCommitted := hasCommitted
  deqPtrGenModule.io.allCommitted := allCommitted
  deqPtrVec := deqPtrGenModule.io.out
  deqPtrVec_next := deqPtrGenModule.io.next_out

  val enqPtrGenModule = Module(new RobEnqPtrWrapper)
  enqPtrGenModule.io.redirect := io.redirect
  enqPtrGenModule.io.allowEnqueue := allowEnqueue && rab.io.canEnq
  enqPtrGenModule.io.hasBlockBackward := hasBlockBackward
  enqPtrGenModule.io.enq := VecInit(io.enq.req.map(req => req.valid && req.bits.firstUop))
  enqPtrVec := enqPtrGenModule.io.out

  // next walkPtrVec:
  // (1) redirect occurs: update according to state
  // (2) walk: move forwards
  val deqPtrReadBank = deqPtrVec_next(0).lineHeadPtr
  val deqPtrVecForWalk = VecInit((0 until CommitWidth).map(i => deqPtrReadBank + i.U))
  val snapPtrReadBank = snapshots(io.snpt.snptSelect)(0).lineHeadPtr
  val snapPtrVecForWalk = VecInit((0 until CommitWidth).map(i => snapPtrReadBank + i.U))
  val walkPtrVec_next: Vec[RobPtr] = Mux(io.redirect.valid,
    Mux(io.snpt.useSnpt, snapPtrVecForWalk, deqPtrVecForWalk),
    Mux((state === s_walk) && !walkFinished, VecInit(walkPtrVec.map(_ + CommitWidth.U)), walkPtrVec)
  )
  val walkPtrTrue_next: RobPtr = Mux(io.redirect.valid,
    Mux(io.snpt.useSnpt, snapshots(io.snpt.snptSelect)(0), deqPtrVec_next(0)),
    Mux((state === s_walk) && !walkFinished, walkPtrVec_next.head, walkPtrTrue)
  )
  walkPtrHead := walkPtrVec_next.head
  walkPtrVec := walkPtrVec_next
  walkPtrTrue := walkPtrTrue_next
  // T io.redirect.valid, T+1 walkPtrLowBits update, T+2 donotNeedWalk update
  val walkPtrLowBits = Reg(UInt(bankAddrWidth.W))
  when(io.redirect.valid){
    walkPtrLowBits := Mux(io.snpt.useSnpt, snapshots(io.snpt.snptSelect)(0).value(bankAddrWidth-1, 0), deqPtrVec_next(0).value(bankAddrWidth-1, 0))
  }
  when(io.redirect.valid) {
    donotNeedWalk := Fill(donotNeedWalk.length, true.B).asTypeOf(donotNeedWalk)
  }.elsewhen(RegNext(io.redirect.valid)){
    donotNeedWalk := (0 until CommitWidth).map(i => (i.U < walkPtrLowBits))
  }.otherwise{
    donotNeedWalk := 0.U.asTypeOf(donotNeedWalk)
  }
  walkDestSizeDeqGroup.zip(walkPtrVec_next).map {
    case (reg, ptrNext) => reg := robEntries(deqPtr.value).realDestSize
  }
  val numValidEntries = distanceBetween(enqPtr, deqPtr)
  val commitCnt = PopCount(io.commits.commitValid)

  allowEnqueue := numValidEntries + dispatchNum <= (RobSize - CommitWidth).U

  val redirectWalkDistance = distanceBetween(io.redirect.bits.robIdx, deqPtrVec_next(0))
  when(io.redirect.valid) {
    lastWalkPtr := Mux(io.redirect.bits.flushItself(), io.redirect.bits.robIdx - 1.U, io.redirect.bits.robIdx)
  }


  /**
   * States
   * We put all the stage bits changes here.
   *
   * All events: (1) enqueue (dispatch); (2) writeback; (3) cancel; (4) dequeue (commit);
   * All states: (1) valid; (2) writebacked; (3) flagBkup
   */

  val deqPtrGroup = Wire(Vec(2 * CommitWidth, new RobPtr))
  deqPtrGroup.zipWithIndex.map { case (deq, i) => deq := deqPtrVec(0) + i.U }
  val commitReadAddr = Mux(state === s_idle, VecInit(deqPtrVec.map(_.value)), VecInit(walkPtrVec.map(_.value)))

  val redirectValidReg = RegNext(io.redirect.valid)
  val redirectBegin = Reg(UInt(log2Up(RobSize).W))
  val redirectEnd = Reg(UInt(log2Up(RobSize).W))
  when(io.redirect.valid){
    redirectBegin := Mux(io.redirect.bits.flushItself(), io.redirect.bits.robIdx.value - 1.U, io.redirect.bits.robIdx.value)
    redirectEnd := enqPtr.value
  }

  // update robEntries valid
  for (i <- 0 until RobSize) {
    val enqOH = VecInit(canEnqueue.zip(allocatePtrVec.map(_.value === i.U)).map(x => x._1 && x._2))
    val commitCond = io.commits.isCommit && io.commits.commitValid.zip(deqPtrVec.map(_.value === i.U)).map(x => x._1 && x._2).reduce(_ || _)
    assert(PopCount(enqOH) < 2.U, s"robEntries$i enqOH is not one hot")
    val needFlush = redirectValidReg && Mux(
      redirectEnd > redirectBegin,
      (i.U > redirectBegin) && (i.U < redirectEnd),
      (i.U > redirectBegin) || (i.U < redirectEnd)
    )
    when(reset.asBool) {
      robEntries(i).valid := false.B
    }.elsewhen(commitCond) {
      robEntries(i).valid := false.B
    }.elsewhen(enqOH.asUInt.orR && !io.redirect.valid) {
      robEntries(i).valid := true.B
    }.elsewhen(needFlush){
      robEntries(i).valid := false.B
    }
  }

  // debug_inst update
  for (i <- 0 until (LduCnt + StaCnt)) {
    debug_lsInfo(io.debug_ls.debugLsInfo(i).s1_robIdx).s1SignalEnable(io.debug_ls.debugLsInfo(i))
    debug_lsInfo(io.debug_ls.debugLsInfo(i).s2_robIdx).s2SignalEnable(io.debug_ls.debugLsInfo(i))
    debug_lsInfo(io.debug_ls.debugLsInfo(i).s3_robIdx).s3SignalEnable(io.debug_ls.debugLsInfo(i))
  }
  for (i <- 0 until LduCnt) {
    debug_lsTopdownInfo(io.lsTopdownInfo(i).s1.robIdx).s1SignalEnable(io.lsTopdownInfo(i))
    debug_lsTopdownInfo(io.lsTopdownInfo(i).s2.robIdx).s2SignalEnable(io.lsTopdownInfo(i))
  }

  // status field: writebacked
  // enqueue logic set 6 writebacked to false
  for (i <- 0 until RenameWidth) {
    when(canEnqueue(i)) {
      val enqHasException = ExceptionNO.selectFrontend(io.enq.req(i).bits.exceptionVec).asUInt.orR
      val enqTriggerActionIsDebugMode = TriggerAction.isDmode(io.enq.req(i).bits.trigger)
      val enqIsWritebacked = io.enq.req(i).bits.eliminatedMove
      val isStu = FuType.isStore(io.enq.req(i).bits.fuType)
      robEntries(allocatePtrVec(i).value).commitTrigger := enqIsWritebacked && !enqHasException && !enqTriggerActionIsDebugMode && !isStu
    }
  }
  when(exceptionGen.io.out.valid) {
    val wbIdx = exceptionGen.io.out.bits.robIdx.value
    robEntries(wbIdx).commitTrigger := true.B
  }

  // writeback logic set numWbPorts writebacked to true
  val blockWbSeq = Wire(Vec(exuWBs.length, Bool()))
  blockWbSeq.map(_ := false.B)
  for ((wb, blockWb) <- exuWBs.zip(blockWbSeq)) {
    when(wb.valid) {
      val wbIdx = wb.bits.robIdx.value
      val wbHasException = wb.bits.exceptionVec.getOrElse(0.U).asUInt.orR
      val wbTriggerActionIsDebugMode = TriggerAction.isDmode(wb.bits.trigger.getOrElse(TriggerAction.None))
      val wbHasFlushPipe = wb.bits.flushPipe.getOrElse(false.B)
      val wbHasReplayInst = wb.bits.replay.getOrElse(false.B) //Todo: && wb.bits.replayInst
      blockWb := wbHasException || wbHasFlushPipe || wbHasReplayInst || wbTriggerActionIsDebugMode
      robEntries(wbIdx).commitTrigger := !blockWb
    }
  }

  // if the first uop of an instruction is valid , write writebackedCounter
  val uopEnqValidSeq = io.enq.req.map(req => io.enq.canAccept && req.valid)
  val instEnqValidSeq = io.enq.req.map(req => io.enq.canAccept && req.valid && req.bits.firstUop)
  val enqNeedWriteRFSeq = io.enq.req.map(_.bits.needWriteRf)
  val enqRobIdxSeq = io.enq.req.map(req => req.bits.robIdx.value)
  val enqUopNumVec = VecInit(io.enq.req.map(req => req.bits.numUops))
  val enqWBNumVec = VecInit(io.enq.req.map(req => req.bits.numWB))
  val enqEliminatedMoveVec = VecInit(io.enq.req.map(req => req.bits.eliminatedMove))

  private val enqWriteStdVec: Vec[Bool] = VecInit(io.enq.req.map {
    req => FuType.isAMO(req.bits.fuType) || FuType.isStore(req.bits.fuType)
  })
  val fflags_wb = fflagsWBs
  val vxsat_wb = vxsatWBs
  for (i <- 0 until RobSize) {

    val robIdxMatchSeq = io.enq.req.map(_.bits.robIdx.value === i.U)
    val uopCanEnqSeq = uopEnqValidSeq.zip(robIdxMatchSeq).map { case (valid, isMatch) => valid && isMatch }
    val instCanEnqSeq = instEnqValidSeq.zip(robIdxMatchSeq).map { case (valid, isMatch) => valid && isMatch }
    val instCanEnqFlag = Cat(instCanEnqSeq).orR
    val realDestEnqNum = PopCount(enqNeedWriteRFSeq.zip(uopCanEnqSeq).map { case (writeFlag, valid) => writeFlag && valid })
    when(!robEntries(i).valid && instCanEnqFlag){
      robEntries(i).realDestSize := realDestEnqNum
    }.elsewhen(robEntries(i).valid && Cat(uopCanEnqSeq).orR){
      robEntries(i).realDestSize := robEntries(i).realDestSize + realDestEnqNum
    }
    val enqUopNum = PriorityMux(instCanEnqSeq, enqUopNumVec)
    val enqWBNum = PriorityMux(instCanEnqSeq, enqWBNumVec)
    val enqEliminatedMove = PriorityMux(instCanEnqSeq, enqEliminatedMoveVec)
    val enqWriteStd = PriorityMux(instCanEnqSeq, enqWriteStdVec)

    val canWbSeq = exuWBs.map(writeback => writeback.valid && writeback.bits.robIdx.value === i.U)
    val canWbNoBlockSeq = canWbSeq.zip(blockWbSeq).map { case (canWb, blockWb) => canWb && !blockWb }
    val canStdWbSeq = VecInit(stdWBs.map(writeback => writeback.valid && writeback.bits.robIdx.value === i.U))
    val wbCnt = Mux1H(canWbSeq, io.writebackNums.map(_.bits))

    val canWbExceptionSeq = exceptionWBs.map(writeback => writeback.valid && writeback.bits.robIdx.value === i.U)
    val needFlush = robEntries(i).needFlush
    val needFlushWriteBack = Wire(Bool())
    needFlushWriteBack := Mux1H(canWbExceptionSeq, io.writebackNeedFlush)
    when(robEntries(i).valid){
      needFlush := needFlush || needFlushWriteBack
    }

    when(robEntries(i).valid && (needFlush || needFlushWriteBack)) {
      // exception flush
      robEntries(i).uopNum := robEntries(i).uopNum - wbCnt
      robEntries(i).stdWritebacked := true.B
    }.elsewhen(!robEntries(i).valid && instCanEnqFlag) {
      // enq set num of uops
      robEntries(i).uopNum := enqWBNum
      robEntries(i).stdWritebacked := Mux(enqWriteStd, false.B, true.B)
    }.elsewhen(robEntries(i).valid) {
      // update by writing back
      robEntries(i).uopNum := robEntries(i).uopNum - wbCnt
      assert(!(robEntries(i).uopNum - wbCnt > robEntries(i).uopNum), s"robEntries $i uopNum is overflow!")
      when(canStdWbSeq.asUInt.orR) {
        robEntries(i).stdWritebacked := true.B
      }
    }

    val fflagsCanWbSeq = fflags_wb.map(writeback => writeback.valid && writeback.bits.robIdx.value === i.U && writeback.bits.wflags.getOrElse(false.B))
    val fflagsRes = fflagsCanWbSeq.zip(fflags_wb).map { case (canWb, wb) => Mux(canWb, wb.bits.fflags.get, 0.U) }.fold(false.B)(_ | _)
    robEntries(i).fflags := Mux(!robEntries(i).valid && instCanEnqFlag, 0.U, robEntries(i).fflags | fflagsRes)

    val vxsatCanWbSeq = vxsat_wb.map(writeback => writeback.valid && writeback.bits.robIdx.value === i.U)
    val vxsatRes = vxsatCanWbSeq.zip(vxsat_wb).map { case (canWb, wb) => Mux(canWb, wb.bits.vxsat.get, 0.U) }.fold(false.B)(_ | _)
    robEntries(i).vxsat := Mux(!robEntries(i).valid && instCanEnqFlag, 0.U, robEntries(i).vxsat | vxsatRes)

    // trace
    val taken = branchWBs.map(writeback => writeback.valid && writeback.bits.robIdx.value === i.U && writeback.bits.redirect.get.bits.cfiUpdate.taken).reduce(_ || _)
    val xret = csrWBs.map(writeback => writeback.valid && writeback.bits.robIdx.value === i.U && io.csr.isXRet).reduce(_ || _)

    when(xret){
      robEntries(i).traceBlockInPipe.itype := Itype.ExpIntReturn
    }.elsewhen(Itype.isBranchType(robEntries(i).traceBlockInPipe.itype)){
      // BranchType code(itype = 5) must be correctly replaced!
      robEntries(i).traceBlockInPipe.itype := Mux(taken, Itype.Taken, Itype.NonTaken)
    }
  }

  // begin update robBanksRdata
  val robBanksRdata = VecInit(robBanksRdataThisLine ++ robBanksRdataNextLine)
  val needUpdate = Wire(Vec(2 * CommitWidth, new RobEntryBundle))
  needUpdate := VecInit(robBanksRdataThisLine ++ robBanksRdataNextLine)
  val needUpdateRobIdx = robIdxThisLine ++ robIdxNextLine
  for (i <- 0 until 2 * CommitWidth) {
    val robIdxMatchSeq = io.enq.req.map(_.bits.robIdx.value === needUpdateRobIdx(i))
    val uopCanEnqSeq = uopEnqValidSeq.zip(robIdxMatchSeq).map { case (valid, isMatch) => valid && isMatch }
    val instCanEnqSeq = instEnqValidSeq.zip(robIdxMatchSeq).map { case (valid, isMatch) => valid && isMatch }
    val instCanEnqFlag = Cat(instCanEnqSeq).orR
    val realDestEnqNum = PopCount(enqNeedWriteRFSeq.zip(uopCanEnqSeq).map { case (writeFlag, valid) => writeFlag && valid })
    when(!needUpdate(i).valid && instCanEnqFlag) {
      needUpdate(i).realDestSize := realDestEnqNum
    }.elsewhen(needUpdate(i).valid && instCanEnqFlag) {
      needUpdate(i).realDestSize := robBanksRdata(i).realDestSize + realDestEnqNum
    }
    val enqUopNum = PriorityMux(instCanEnqSeq, enqUopNumVec)
    val enqWBNum = PriorityMux(instCanEnqSeq, enqWBNumVec)
    val enqEliminatedMove = PriorityMux(instCanEnqSeq, enqEliminatedMoveVec)
    val enqWriteStd = PriorityMux(instCanEnqSeq, enqWriteStdVec)

    val canWbSeq = exuWBs.map(writeback => writeback.valid && writeback.bits.robIdx.value === needUpdateRobIdx(i))
    val canWbNoBlockSeq = canWbSeq.zip(blockWbSeq).map { case (canWb, blockWb) => canWb && !blockWb }
    val canStdWbSeq = VecInit(stdWBs.map(writeback => writeback.valid && writeback.bits.robIdx.value === needUpdateRobIdx(i)))
    val wbCnt = Mux1H(canWbSeq, io.writebackNums.map(_.bits))

    val canWbExceptionSeq = exceptionWBs.map(writeback => writeback.valid && (writeback.bits.robIdx.value === needUpdateRobIdx(i)))
    val needFlush = robBanksRdata(i).needFlush
    val needFlushWriteBack = Wire(Bool())
    needFlushWriteBack := Mux1H(canWbExceptionSeq, io.writebackNeedFlush)
    when(needUpdate(i).valid) {
      needUpdate(i).needFlush := needFlush || needFlushWriteBack
    }

    when(needUpdate(i).valid && (needFlush || needFlushWriteBack)) {
      // exception flush
      needUpdate(i).uopNum := robBanksRdata(i).uopNum - wbCnt
      needUpdate(i).stdWritebacked := true.B
    }.elsewhen(!needUpdate(i).valid && instCanEnqFlag) {
      // enq set num of uops
      needUpdate(i).uopNum := enqWBNum
      needUpdate(i).stdWritebacked := Mux(enqWriteStd, false.B, true.B)
    }.elsewhen(needUpdate(i).valid) {
      // update by writing back
      needUpdate(i).uopNum := robBanksRdata(i).uopNum - wbCnt
      when(canStdWbSeq.asUInt.orR) {
        needUpdate(i).stdWritebacked := true.B
      }
    }

    val fflagsCanWbSeq = fflags_wb.map(writeback => writeback.valid && writeback.bits.robIdx.value === needUpdateRobIdx(i) && writeback.bits.wflags.getOrElse(false.B))
    val fflagsRes = fflagsCanWbSeq.zip(fflags_wb).map { case (canWb, wb) => Mux(canWb, wb.bits.fflags.get, 0.U) }.fold(false.B)(_ | _)
    needUpdate(i).fflags := Mux(!robBanksRdata(i).valid && instCanEnqFlag, 0.U, robBanksRdata(i).fflags | fflagsRes)

    val vxsatCanWbSeq = vxsat_wb.map(writeback => writeback.valid && writeback.bits.robIdx.value === needUpdateRobIdx(i))
    val vxsatRes = vxsatCanWbSeq.zip(vxsat_wb).map { case (canWb, wb) => Mux(canWb, wb.bits.vxsat.get, 0.U) }.fold(false.B)(_ | _)
    needUpdate(i).vxsat := Mux(!robBanksRdata(i).valid && instCanEnqFlag, 0.U, robBanksRdata(i).vxsat | vxsatRes)
  }
  robBanksRdataThisLineUpdate := VecInit(needUpdate.take(8))
  robBanksRdataNextLineUpdate := VecInit(needUpdate.drop(8))
  // end update robBanksRdata

  // interrupt_safe
  for (i <- 0 until RenameWidth) {
    // We RegNext the updates for better timing.
    // Note that instructions won't change the system's states in this cycle.
    when(RegNext(canEnqueue(i))) {
      // For now, we allow non-load-store instructions to trigger interrupts
      // For MMIO instructions, they should not trigger interrupts since they may
      // be sent to lower level before it writes back.
      // However, we cannot determine whether a load/store instruction is MMIO.
      // Thus, we don't allow load/store instructions to trigger an interrupt.
      // TODO: support non-MMIO load-store instructions to trigger interrupts
      val allow_interrupts = !CommitType.isLoadStore(io.enq.req(i).bits.commitType) && !FuType.isFence(io.enq.req(i).bits.fuType) && !FuType.isCsr(io.enq.req(i).bits.fuType)
      robEntries(RegEnable(allocatePtrVec(i).value, canEnqueue(i))).interrupt_safe := RegEnable(allow_interrupts, canEnqueue(i))
    }
  }

  /**
   * read and write of data modules
   */
  val commitReadAddr_next = Mux(state_next === s_idle,
    VecInit(deqPtrVec_next.map(_.value)),
    VecInit(walkPtrVec_next.map(_.value))
  )

  exceptionGen.io.redirect <> io.redirect
  exceptionGen.io.flush := io.flushOut.valid

  val canEnqueueEG = VecInit(io.enq.req.map(req => req.valid && io.enq.canAccept))
  for (i <- 0 until RenameWidth) {
    exceptionGen.io.enq(i).valid := canEnqueueEG(i)
    exceptionGen.io.enq(i).bits.robIdx := io.enq.req(i).bits.robIdx
    exceptionGen.io.enq(i).bits.ftqPtr := io.enq.req(i).bits.ftqPtr
    exceptionGen.io.enq(i).bits.ftqOffset := io.enq.req(i).bits.ftqOffset
    exceptionGen.io.enq(i).bits.exceptionVec := ExceptionNO.selectFrontend(io.enq.req(i).bits.exceptionVec)
    exceptionGen.io.enq(i).bits.hasException := io.enq.req(i).bits.hasException
    exceptionGen.io.enq(i).bits.flushPipe := io.enq.req(i).bits.flushPipe
    exceptionGen.io.enq(i).bits.isVset := io.enq.req(i).bits.isVset
    exceptionGen.io.enq(i).bits.replayInst := false.B
    XSError(canEnqueue(i) && io.enq.req(i).bits.replayInst, "enq should not set replayInst")
    exceptionGen.io.enq(i).bits.singleStep := io.enq.req(i).bits.singleStep
    exceptionGen.io.enq(i).bits.crossPageIPFFix := io.enq.req(i).bits.crossPageIPFFix
    exceptionGen.io.enq(i).bits.trigger := io.enq.req(i).bits.trigger
    exceptionGen.io.enq(i).bits.vstartEn := false.B //DontCare
    exceptionGen.io.enq(i).bits.vstart := 0.U //DontCare
  }

  println(s"ExceptionGen:")
  println(s"num of exceptions: ${params.numException}")
  require(exceptionWBs.length == exceptionGen.io.wb.length,
    f"exceptionWBs.length: ${exceptionWBs.length}, " +
      f"exceptionGen.io.wb.length: ${exceptionGen.io.wb.length}")
  for (((wb, exc_wb), i) <- exceptionWBs.zip(exceptionGen.io.wb).zipWithIndex) {
    exc_wb.valid       := wb.valid
    exc_wb.bits.robIdx := wb.bits.robIdx
    // only enq inst use ftqPtr to read gpa
    exc_wb.bits.ftqPtr          := 0.U.asTypeOf(exc_wb.bits.ftqPtr)
    exc_wb.bits.ftqOffset       := 0.U.asTypeOf(exc_wb.bits.ftqOffset)
    exc_wb.bits.exceptionVec    := wb.bits.exceptionVec.get
    exc_wb.bits.hasException    := wb.bits.exceptionVec.get.asUInt.orR // Todo: use io.writebackNeedFlush(i) instead
    exc_wb.bits.flushPipe       := wb.bits.flushPipe.getOrElse(false.B)
    exc_wb.bits.isVset          := false.B
    exc_wb.bits.replayInst      := wb.bits.replay.getOrElse(false.B)
    exc_wb.bits.singleStep      := false.B
    exc_wb.bits.crossPageIPFFix := false.B
    // TODO: make trigger configurable
    val trigger = wb.bits.trigger.getOrElse(0.U).asTypeOf(exc_wb.bits.trigger)
    exc_wb.bits.trigger := trigger
    exc_wb.bits.vstartEn := false.B //wb.bits.vstartEn.getOrElse(false.B) // todo need add vstart in ExuOutput
    exc_wb.bits.vstart := 0.U //wb.bits.vstart.getOrElse(0.U)
    //    println(s"  [$i] ${configs.map(_.name)}: exception ${exceptionCases(i)}, " +
    //      s"flushPipe ${configs.exists(_.flushPipe)}, " +
    //      s"replayInst ${configs.exists(_.replayInst)}")
  }

  fflagsDataRead := (0 until CommitWidth).map(i => robEntries(deqPtrVec(i).value).fflags)
  vxsatDataRead := (0 until CommitWidth).map(i => robEntries(deqPtrVec(i).value).vxsat)

  val instrCntReg = RegInit(0.U(64.W))
  val fuseCommitCnt = PopCount(io.commits.commitValid.zip(io.commits.info).map { case (v, i) => RegNext(v && CommitType.isFused(i.commitType)) })
  val trueCommitCnt = RegNext(io.commits.commitValid.zip(io.commits.info).map { case (v, i) => Mux(v, i.instrSize, 0.U) }.reduce(_ +& _)) +& fuseCommitCnt
  val retireCounter = Mux(RegNext(io.commits.isCommit), trueCommitCnt, 0.U)
  val instrCnt = instrCntReg + retireCounter
  instrCntReg := instrCnt
  io.csr.perfinfo.retiredInstr := retireCounter
  io.robFull := !allowEnqueue
  io.headNotReady := commit_vDeqGroup.head && !commit_wDeqGroup.head

  /**
   * debug info
   */
  XSDebug(p"enqPtr ${enqPtr} deqPtr ${deqPtr}\n")
  XSDebug("")
  XSError(isBefore(enqPtr, deqPtr) && !isFull(enqPtr, deqPtr), "\ndeqPtr is older than enqPtr!\n")
  for (i <- 0 until RobSize) {
    XSDebug(false, !robEntries(i).valid, "-")
    XSDebug(false, robEntries(i).valid && robEntries(i).isWritebacked, "w")
    XSDebug(false, robEntries(i).valid && !robEntries(i).isWritebacked, "v")
  }
  XSDebug(false, true.B, "\n")

  for (i <- 0 until RobSize) {
    if (i % 4 == 0) XSDebug("")
    XSDebug(false, true.B, "%x ", debug_microOp(i).pc)
    XSDebug(false, !robEntries(i).valid, "- ")
    XSDebug(false, robEntries(i).valid && robEntries(i).isWritebacked, "w ")
    XSDebug(false, robEntries(i).valid && !robEntries(i).isWritebacked, "v ")
    if (i % 4 == 3) XSDebug(false, true.B, "\n")
  }

  def ifCommit(counter: UInt): UInt = Mux(io.commits.isCommit, counter, 0.U)

  def ifCommitReg(counter: UInt): UInt = Mux(RegNext(io.commits.isCommit), counter, 0.U)

  val commitDebugUop = deqPtrVec.map(_.value).map(debug_microOp(_))
  XSPerfAccumulate("clock_cycle", 1.U)
  QueuePerf(RobSize, numValidEntries, numValidEntries === RobSize.U)
  XSPerfAccumulate("commitUop", ifCommit(commitCnt))
  XSPerfAccumulate("commitInstr", ifCommitReg(trueCommitCnt))
  XSPerfRolling("ipc", ifCommitReg(trueCommitCnt), 1000, clock, reset)
  XSPerfRolling("cpi", perfCnt = 1.U /*Cycle*/ , eventTrigger = ifCommitReg(trueCommitCnt), granularity = 1000, clock, reset)
  val commitIsMove = commitInfo.map(_.isMove)
  XSPerfAccumulate("commitInstrMove", ifCommit(PopCount(io.commits.commitValid.zip(commitIsMove).map { case (v, m) => v && m })))
  val commitMoveElim = commitDebugUop.map(_.debugInfo.eliminatedMove)
  XSPerfAccumulate("commitInstrMoveElim", ifCommit(PopCount(io.commits.commitValid zip commitMoveElim map { case (v, e) => v && e })))
  XSPerfAccumulate("commitInstrFused", ifCommitReg(fuseCommitCnt))
  val commitIsLoad = io.commits.info.map(_.commitType).map(_ === CommitType.LOAD)
  val commitLoadValid = io.commits.commitValid.zip(commitIsLoad).map { case (v, t) => v && t }
  XSPerfAccumulate("commitInstrLoad", ifCommit(PopCount(commitLoadValid)))
  val commitIsBranch = io.commits.info.map(_.commitType).map(_ === CommitType.BRANCH)
  val commitBranchValid = io.commits.commitValid.zip(commitIsBranch).map { case (v, t) => v && t }
  XSPerfAccumulate("commitInstrBranch", ifCommit(PopCount(commitBranchValid)))
  val commitLoadWaitBit = commitInfo.map(_.loadWaitBit)
  XSPerfAccumulate("commitInstrLoadWait", ifCommit(PopCount(commitLoadValid.zip(commitLoadWaitBit).map { case (v, w) => v && w })))
  val commitIsStore = io.commits.info.map(_.commitType).map(_ === CommitType.STORE)
  XSPerfAccumulate("commitInstrStore", ifCommit(PopCount(io.commits.commitValid.zip(commitIsStore).map { case (v, t) => v && t })))
  XSPerfAccumulate("writeback", PopCount((0 until RobSize).map(i => robEntries(i).valid && robEntries(i).isWritebacked)))
  // XSPerfAccumulate("enqInstr", PopCount(io.dp1Req.map(_.fire)))
  // XSPerfAccumulate("d2rVnR", PopCount(io.dp1Req.map(p => p.valid && !p.ready)))
  XSPerfAccumulate("walkInstr", Mux(io.commits.isWalk, PopCount(io.commits.walkValid), 0.U))
  XSPerfAccumulate("walkCycleTotal", state === s_walk)
  XSPerfAccumulate("waitRabWalkEnd", state === s_walk && walkFinished && !rab.io.status.walkEnd)
  private val walkCycle = RegInit(0.U(8.W))
  private val waitRabWalkCycle = RegInit(0.U(8.W))
  walkCycle := Mux(io.redirect.valid, 0.U, Mux(state === s_walk, walkCycle + 1.U, 0.U))
  waitRabWalkCycle := Mux(state === s_walk && walkFinished, 0.U, Mux(state === s_walk, walkCycle + 1.U, 0.U))

  XSPerfHistogram("walkRobCycleHist", walkCycle, state === s_walk && walkFinished, 0, 32)
  XSPerfHistogram("walkRabExtraCycleHist", waitRabWalkCycle, state === s_walk && walkFinished && rab.io.status.walkEnd, 0, 32)
  XSPerfHistogram("walkTotalCycleHist", walkCycle, state === s_walk && state_next === s_idle, 0, 32)

  private val deqNotWritebacked = robEntries(deqPtr.value).valid && !robEntries(deqPtr.value).isWritebacked
  private val deqStdNotWritebacked = robEntries(deqPtr.value).valid && !robEntries(deqPtr.value).stdWritebacked
  private val deqUopNotWritebacked = robEntries(deqPtr.value).valid && !robEntries(deqPtr.value).isUopWritebacked
  private val deqHeadInfo = debug_microOp(deqPtr.value)
  val deqUopCommitType = debug_microOp(deqPtr.value).commitType

  XSPerfAccumulate("waitAluCycle", deqNotWritebacked && deqHeadInfo.fuType === FuType.alu.U)
  XSPerfAccumulate("waitMulCycle", deqNotWritebacked && deqHeadInfo.fuType === FuType.mul.U)
  XSPerfAccumulate("waitDivCycle", deqNotWritebacked && deqHeadInfo.fuType === FuType.div.U)
  XSPerfAccumulate("waitBrhCycle", deqNotWritebacked && deqHeadInfo.fuType === FuType.brh.U)
  XSPerfAccumulate("waitJmpCycle", deqNotWritebacked && deqHeadInfo.fuType === FuType.jmp.U)
  XSPerfAccumulate("waitCsrCycle", deqNotWritebacked && deqHeadInfo.fuType === FuType.csr.U)
  XSPerfAccumulate("waitFenCycle", deqNotWritebacked && deqHeadInfo.fuType === FuType.fence.U)
  XSPerfAccumulate("waitBkuCycle", deqNotWritebacked && deqHeadInfo.fuType === FuType.bku.U)
  XSPerfAccumulate("waitLduCycle", deqNotWritebacked && deqHeadInfo.fuType === FuType.ldu.U)
  XSPerfAccumulate("waitStuCycle", deqNotWritebacked && deqHeadInfo.fuType === FuType.stu.U)
  XSPerfAccumulate("waitStaCycle", deqUopNotWritebacked && deqHeadInfo.fuType === FuType.stu.U)
  XSPerfAccumulate("waitStdCycle", deqStdNotWritebacked && deqHeadInfo.fuType === FuType.stu.U)
  XSPerfAccumulate("waitAtmCycle", deqStdNotWritebacked && deqHeadInfo.fuType === FuType.mou.U)

  XSPerfAccumulate("waitVfaluCycle", deqStdNotWritebacked && deqHeadInfo.fuType === FuType.vfalu.U)
  XSPerfAccumulate("waitVfmaCycle", deqStdNotWritebacked && deqHeadInfo.fuType === FuType.vfma.U)
  XSPerfAccumulate("waitVfdivCycle", deqStdNotWritebacked && deqHeadInfo.fuType === FuType.vfdiv.U)

  val vfalufuop = Seq(VfaluType.vfadd, VfaluType.vfwadd, VfaluType.vfwadd_w, VfaluType.vfsub, VfaluType.vfwsub, VfaluType.vfwsub_w, VfaluType.vfmin, VfaluType.vfmax,
    VfaluType.vfmerge, VfaluType.vfmv, VfaluType.vfsgnj, VfaluType.vfsgnjn, VfaluType.vfsgnjx, VfaluType.vfeq, VfaluType.vfne, VfaluType.vflt, VfaluType.vfle, VfaluType.vfgt,
    VfaluType.vfge, VfaluType.vfclass, VfaluType.vfmv_f_s, VfaluType.vfmv_s_f, VfaluType.vfredusum, VfaluType.vfredmax, VfaluType.vfredmin, VfaluType.vfredosum, VfaluType.vfwredosum)

  vfalufuop.zipWithIndex.map{
    case(fuoptype,i) =>  XSPerfAccumulate(s"waitVfalu_${i}Cycle", deqStdNotWritebacked && deqHeadInfo.fuOpType === fuoptype && deqHeadInfo.fuType === FuType.vfalu.U)
  }



  XSPerfAccumulate("waitNormalCycle", deqNotWritebacked && deqUopCommitType === CommitType.NORMAL)
  XSPerfAccumulate("waitBranchCycle", deqNotWritebacked && deqUopCommitType === CommitType.BRANCH)
  XSPerfAccumulate("waitLoadCycle", deqNotWritebacked && deqUopCommitType === CommitType.LOAD)
  XSPerfAccumulate("waitStoreCycle", deqNotWritebacked && deqUopCommitType === CommitType.STORE)
  XSPerfAccumulate("robHeadPC", io.commits.info(0).debug_pc.getOrElse(0.U))
  XSPerfAccumulate("commitCompressCntAll", PopCount(io.commits.commitValid.zip(io.commits.info).map { case (valid, info) => io.commits.isCommit && valid && info.instrSize > 1.U }))
  (2 to RenameWidth).foreach(i =>
    XSPerfAccumulate(s"commitCompressCnt${i}", PopCount(io.commits.commitValid.zip(io.commits.info).map { case (valid, info) => io.commits.isCommit && valid && info.instrSize === i.U }))
  )
  XSPerfAccumulate("compressSize", io.commits.commitValid.zip(io.commits.info).map { case (valid, info) => Mux(io.commits.isCommit && valid && info.instrSize > 1.U, info.instrSize, 0.U) }.reduce(_ +& _))
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
    val commitIsFuType = io.commits.commitValid.zip(commitDebugUop).map(x => x._1 && x._2.fuType === fuType.U)
    XSPerfRolling(s"ipc_futype_${fuName}", ifCommit(PopCount(commitIsFuType)), 1000, clock, reset)
    XSPerfAccumulate(s"${fuName}_instr_cnt", ifCommit(PopCount(commitIsFuType)))
    XSPerfAccumulate(s"${fuName}_latency_dispatch", ifCommit(latencySum(commitIsFuType, dispatchLatency)))
    XSPerfAccumulate(s"${fuName}_latency_enq_rs", ifCommit(latencySum(commitIsFuType, enqRsLatency)))
    XSPerfAccumulate(s"${fuName}_latency_select", ifCommit(latencySum(commitIsFuType, selectLatency)))
    XSPerfAccumulate(s"${fuName}_latency_issue", ifCommit(latencySum(commitIsFuType, issueLatency)))
    XSPerfAccumulate(s"${fuName}_latency_execute", ifCommit(latencySum(commitIsFuType, executeLatency)))
    XSPerfAccumulate(s"${fuName}_latency_enq_rs_execute", ifCommit(latencySum(commitIsFuType, rsFuLatency)))
    XSPerfAccumulate(s"${fuName}_latency_commit", ifCommit(latencySum(commitIsFuType, commitLatency)))
  }
  XSPerfAccumulate(s"redirect_use_snapshot", io.redirect.valid && io.snpt.useSnpt)

  // top-down info
  io.debugTopDown.toCore.robHeadVaddr.valid := debug_lsTopdownInfo(deqPtr.value).s1.vaddr_valid
  io.debugTopDown.toCore.robHeadVaddr.bits := debug_lsTopdownInfo(deqPtr.value).s1.vaddr_bits
  io.debugTopDown.toCore.robHeadPaddr.valid := debug_lsTopdownInfo(deqPtr.value).s2.paddr_valid
  io.debugTopDown.toCore.robHeadPaddr.bits := debug_lsTopdownInfo(deqPtr.value).s2.paddr_bits
  io.debugTopDown.toDispatch.robTrueCommit := ifCommitReg(trueCommitCnt)
  io.debugTopDown.toDispatch.robHeadLsIssue := debug_lsIssue(deqPtr.value)
  io.debugTopDown.robHeadLqIdx.valid := debug_lqIdxValid(deqPtr.value)
  io.debugTopDown.robHeadLqIdx.bits := debug_microOp(deqPtr.value).lqIdx

  // rolling
  io.debugRolling.robTrueCommit := ifCommitReg(trueCommitCnt)

  /**
   * DataBase info:
   * log trigger is at writeback valid
   * */

  /**
   * @todo add InstInfoEntry back
   * @author Maxpicca-Li
   */

  // TraceRTL Collect Commit Trace to check the correctness of the pipeline
  if (env.TraceRTLMode) {
    when (io.enq.canAccept) {
      io.enq.req.map{ r =>
        dontTouch(r.bits.traceInfo)
        when (r.valid) {
          XSError(r.bits.pc =/= r.bits.traceInfo.pcVA, "ROB Enq: pc should be equal to traceInfo.pcVA")
        }
      }
    }

    import xiangshan.frontend.tracertl.TraceCollector
    val traceCollector = Module(new TraceCollector)
    traceCollector.io.enable := io.commits.commitValid(0) && io.commits.isCommit
    (0 until CommitWidth).foreach { case i =>
      val uop = commitDebugUop(i)
      val commitInfo = io.commits.info(i)
      traceCollector.io.in(i).valid := io.commits.commitValid(i)
      traceCollector.io.in(i).bits.pc := SignExt(uop.pc, XLEN)
      traceCollector.io.in(i).bits.inst := uop.traceInfo.inst
      traceCollector.io.in(i).bits.instNum := CommitType.isFused(commitInfo.commitType).asUInt + commitInfo.instrSize
      traceCollector.io.traceInfo(i) := uop.traceInfo
      // TODO: do expandInst check. traceinfo.inst and uop.inst

      when (traceCollector.io.enable && traceCollector.io.in(i).valid) {
        XSError(traceCollector.io.in(i).bits.pc =/= uop.traceInfo.pcVA, "Trace ROB commit pc mismatch")
      }
    }
    (0 until CommitWidth).foreach{ case i =>
      XSError(!io.commits.commitValid(0) && io.commits.isCommit && io.commits.commitValid(i),
        "When CommitValid is true, CommitValid(0) should be true")
    }
  }

  //difftest signals
  val firstValidCommit = (deqPtr + PriorityMux(io.commits.commitValid, VecInit(List.tabulate(CommitWidth)(_.U(log2Up(CommitWidth).W))))).value

  val wdata = Wire(Vec(CommitWidth, UInt(XLEN.W)))
  val wpc = Wire(Vec(CommitWidth, UInt(XLEN.W)))

  for (i <- 0 until CommitWidth) {
    val idx = deqPtrVec(i).value
    wdata(i) := debug_exuData(idx)
    wpc(i) := SignExt(commitDebugUop(i).pc, XLEN)
  }

  if (env.EnableDifftest || env.AlwaysBasicDiff) {
    // These are the structures used by difftest only and should be optimized after synthesis.
    val dt_eliminatedMove = Mem(RobSize, Bool())
    val dt_isRVC = Mem(RobSize, Bool())
    val dt_exuDebug = Reg(Vec(RobSize, new DebugBundle))
    for (i <- 0 until RenameWidth) {
      when(canEnqueue(i)) {
        dt_eliminatedMove(allocatePtrVec(i).value) := io.enq.req(i).bits.eliminatedMove
        dt_isRVC(allocatePtrVec(i).value) := io.enq.req(i).bits.preDecodeInfo.isRVC
      }
    }
    for (wb <- exuWBs) {
      when(wb.valid) {
        val wbIdx = wb.bits.robIdx.value
        dt_exuDebug(wbIdx) := wb.bits.debug
      }
    }
    // Always instantiate basic difftest modules.
    for (i <- 0 until CommitWidth) {
      val uop = commitDebugUop(i)
      val commitInfo = io.commits.info(i)
      val ptr = deqPtrVec(i).value
      val exuOut = dt_exuDebug(ptr)
      val eliminatedMove = dt_eliminatedMove(ptr)
      val isRVC = dt_isRVC(ptr)

      val difftest = DifftestModule(new DiffInstrCommit(MaxPhyPregs), delay = 3, dontCare = true)
      val dt_skip = Mux(eliminatedMove, false.B, exuOut.isMMIO || exuOut.isPerfCnt)
      difftest.coreid := io.hartId
      difftest.index := i.U
      difftest.valid := io.commits.commitValid(i) && io.commits.isCommit
      difftest.skip := dt_skip
      difftest.isRVC := isRVC
      difftest.rfwen := io.commits.commitValid(i) && commitInfo.rfWen && commitInfo.debug_ldest.get =/= 0.U
      difftest.fpwen := io.commits.commitValid(i) && uop.fpWen
      difftest.wpdest := commitInfo.debug_pdest.get
      difftest.wdest := commitInfo.debug_ldest.get
      difftest.nFused := CommitType.isFused(commitInfo.commitType).asUInt + commitInfo.instrSize - 1.U
      when(difftest.valid) {
        assert(CommitType.isFused(commitInfo.commitType).asUInt + commitInfo.instrSize >= 1.U)
      }
      if (env.EnableDifftest) {
        val uop = commitDebugUop(i)
        difftest.pc := SignExt(uop.pc, XLEN)
        difftest.instr := uop.instr
        difftest.robIdx := ZeroExt(ptr, 10)
        difftest.lqIdx := ZeroExt(uop.lqIdx.value, 7)
        difftest.sqIdx := ZeroExt(uop.sqIdx.value, 7)
        difftest.isLoad := io.commits.info(i).commitType === CommitType.LOAD
        difftest.isStore := io.commits.info(i).commitType === CommitType.STORE
        // Check LoadEvent only when isAmo or isLoad and skip MMIO
        val difftestLoadEvent = DifftestModule(new DiffLoadEvent, delay = 3)
        difftestLoadEvent.coreid := io.hartId
        difftestLoadEvent.index := i.U
        val loadCheck = (FuType.isAMO(uop.fuType) || FuType.isLoad(uop.fuType)) && !dt_skip
        difftestLoadEvent.valid    := io.commits.commitValid(i) && io.commits.isCommit && loadCheck
        difftestLoadEvent.paddr    := exuOut.paddr
        difftestLoadEvent.opType   := uop.fuOpType
        difftestLoadEvent.isAtomic := FuType.isAMO(uop.fuType)
        difftestLoadEvent.isLoad   := FuType.isLoad(uop.fuType)
      }
    }
  }

  if (env.EnableDifftest || env.AlwaysBasicDiff) {
    val dt_isXSTrap = Mem(RobSize, Bool())
    for (i <- 0 until RenameWidth) {
      when(canEnqueue(i)) {
        dt_isXSTrap(allocatePtrVec(i).value) := io.enq.req(i).bits.isXSTrap
      }
    }
    val trapVec = io.commits.commitValid.zip(deqPtrVec).map { case (v, d) =>
      io.commits.isCommit && v && dt_isXSTrap(d.value)
    }
    val hitTrap = trapVec.reduce(_ || _)
    val difftest = DifftestModule(new DiffTrapEvent, dontCare = true)
    difftest.coreid := io.hartId
    difftest.hasTrap := hitTrap
    difftest.cycleCnt := timer
    difftest.instrCnt := instrCnt
    difftest.hasWFI := hasWFI

    if (env.EnableDifftest) {
      val trapCode = PriorityMux(wdata.zip(trapVec).map(x => x._2 -> x._1))
      val trapPC = SignExt(PriorityMux(wpc.zip(trapVec).map(x => x._2 -> x._1)), XLEN)
      difftest.code := trapCode
      difftest.pc := trapPC
    }
  }

  val validEntriesBanks = (0 until (RobSize + 31) / 32).map(i => RegNext(PopCount(robEntries.map(_.valid).drop(i * 32).take(32))))
  val validEntries = RegNext(VecInit(validEntriesBanks).reduceTree(_ +& _))
  val commitMoveVec = VecInit(io.commits.commitValid.zip(commitIsMove).map { case (v, m) => v && m })
  val commitLoadVec = VecInit(commitLoadValid)
  val commitBranchVec = VecInit(commitBranchValid)
  val commitLoadWaitVec = VecInit(commitLoadValid.zip(commitLoadWaitBit).map { case (v, w) => v && w })
  val commitStoreVec = VecInit(io.commits.commitValid.zip(commitIsStore).map { case (v, t) => v && t })
  val perfEvents = Seq(
    ("rob_interrupt_num      ", io.flushOut.valid && intrEnable),
    ("rob_exception_num      ", io.flushOut.valid && deqHasException),
    ("rob_flush_pipe_num     ", io.flushOut.valid && isFlushPipe),
    ("rob_replay_inst_num    ", io.flushOut.valid && isFlushPipe && deqHasReplayInst),
    ("rob_commitUop          ", ifCommit(commitCnt)),
    ("rob_commitInstr        ", ifCommitReg(trueCommitCnt)),
    ("rob_commitInstrMove    ", ifCommitReg(PopCount(RegNext(commitMoveVec)))),
    ("rob_commitInstrFused   ", ifCommitReg(fuseCommitCnt)),
    ("rob_commitInstrLoad    ", ifCommitReg(PopCount(RegNext(commitLoadVec)))),
    ("rob_commitInstrBranch  ", ifCommitReg(PopCount(RegNext(commitBranchVec)))),
    ("rob_commitInstrLoadWait", ifCommitReg(PopCount(RegNext(commitLoadWaitVec)))),
    ("rob_commitInstrStore   ", ifCommitReg(PopCount(RegNext(commitStoreVec)))),
    ("rob_walkInstr          ", Mux(io.commits.isWalk, PopCount(io.commits.walkValid), 0.U)),
    ("rob_walkCycle          ", (state === s_walk)),
    ("rob_1_4_valid          ", validEntries <= (RobSize / 4).U),
    ("rob_2_4_valid          ", validEntries > (RobSize / 4).U && validEntries <= (RobSize / 2).U),
    ("rob_3_4_valid          ", validEntries > (RobSize / 2).U && validEntries <= (RobSize * 3 / 4).U),
    ("rob_4_4_valid          ", validEntries > (RobSize * 3 / 4).U),
  )
  generatePerfEvent()

  // dontTouch for debug
  if (backendParams.debugEn) {
    dontTouch(enqPtrVec)
    dontTouch(deqPtrVec)
    dontTouch(robEntries)
    dontTouch(robDeqGroup)
    dontTouch(robBanks)
    dontTouch(robBanksRaddrThisLine)
    dontTouch(robBanksRaddrNextLine)
    dontTouch(robBanksRdataThisLine)
    dontTouch(robBanksRdataNextLine)
    dontTouch(robBanksRdataThisLineUpdate)
    dontTouch(robBanksRdataNextLineUpdate)
    dontTouch(needUpdate)
    val exceptionWBsVec = MixedVecInit(exceptionWBs)
    dontTouch(exceptionWBsVec)
    dontTouch(commit_wDeqGroup)
    dontTouch(commit_vDeqGroup)
    dontTouch(commitSizeSumSeq)
    dontTouch(walkSizeSumSeq)
    dontTouch(commitSizeSumCond)
    dontTouch(walkSizeSumCond)
    dontTouch(commitSizeSum)
    dontTouch(walkSizeSum)
    dontTouch(realDestSizeSeq)
    dontTouch(walkDestSizeSeq)
    dontTouch(io.commits)
    dontTouch(commitIsVTypeVec)
    dontTouch(walkIsVTypeVec)
    dontTouch(commitValidThisLine)
    dontTouch(commitReadAddr_next)
    dontTouch(donotNeedWalk)
    dontTouch(walkPtrVec_next)
    dontTouch(walkPtrVec)
    dontTouch(deqPtrVec_next)
    dontTouch(deqPtrVecForWalk)
    dontTouch(snapPtrReadBank)
    dontTouch(snapPtrVecForWalk)
    dontTouch(shouldWalkVec)
    dontTouch(walkFinished)
    dontTouch(changeBankAddrToDeqPtr)
  }
  if (env.EnableDifftest) {
    io.commits.info.map(info => dontTouch(info.debug_pc.get))
  }
}
