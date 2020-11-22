package xiangshan.backend.roq

import chisel3.ExcitingUtils._
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.backend.LSUOpType
import xiangshan.backend.fu.fpu.Fflags
object roqDebugId extends Function0[Integer] {
  var x = 0
  def apply(): Integer = {
    x = x + 1
    return x
  }
}

class RoqPtr extends CircularQueuePtr(RoqPtr.RoqSize) with HasCircularQueuePtrHelper {
  def needFlush(redirect: Valid[Redirect]): Bool = {
    redirect.valid && (redirect.bits.isException || redirect.bits.isFlushPipe || isAfter(this, redirect.bits.roqIdx))
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

class Roq(numWbPorts: Int) extends XSModule with HasCircularQueuePtrHelper {
  val io = IO(new Bundle() {
    val brqRedirect = Input(Valid(new Redirect))
    val memRedirect = Input(Valid(new Redirect))
    val dp1Req = Vec(RenameWidth, Flipped(DecoupledIO(new MicroOp)))
    val roqIdxs = Output(Vec(RenameWidth, new RoqPtr))
    val redirect = Output(Valid(new Redirect))
    val exception = Output(new MicroOp)
    // exu + brq
    val exeWbResults = Vec(numWbPorts, Flipped(ValidIO(new ExuOutput)))
    val commits = Vec(CommitWidth, Valid(new RoqCommit))
    val bcommit = Output(UInt(BrTagWidth.W))
    val commitRoqIndex = Output(Valid(new RoqPtr))
    val roqDeqPtr = Output(new RoqPtr)
    val csr = new RoqCSRIO
  })

  val microOp = Mem(RoqSize, new MicroOp)
  val valid = RegInit(VecInit(List.fill(RoqSize)(false.B)))
  val flag = RegInit(VecInit(List.fill(RoqSize)(false.B)))
  val writebacked = Reg(Vec(RoqSize, Bool()))

  val exuFflags = Mem(RoqSize, new Fflags)
  val exuData = Reg(Vec(RoqSize, UInt(XLEN.W)))//for debug
  val exuDebug = Reg(Vec(RoqSize, new DebugBundle))//for debug

  val enqPtrExt = RegInit(0.U.asTypeOf(new RoqPtr))
  val deqPtrExt = RegInit(0.U.asTypeOf(new RoqPtr))
  val walkPtrExt = Reg(new RoqPtr)
  val walkTgtExt = Reg(new RoqPtr)
  val enqPtr = enqPtrExt.value
  val deqPtr = deqPtrExt.value
  val walkPtr = walkPtrExt.value
  val isEmpty = enqPtr === deqPtr && enqPtrExt.flag ===deqPtrExt.flag
  val isFull = enqPtr === deqPtr && enqPtrExt.flag =/= deqPtrExt.flag
  val notFull = !isFull

  val s_idle :: s_walk :: s_extrawalk :: Nil = Enum(3)
  val state = RegInit(s_idle)

  io.roqDeqPtr := deqPtrExt

  // Dispatch
  val noSpecEnq = io.dp1Req.map(i => i.bits.ctrl.blockBackward)
  val hasNoSpec = RegInit(false.B)
  when(isEmpty){ hasNoSpec:= false.B }
  val validDispatch = io.dp1Req.map(_.valid)
  XSDebug("(ready, valid): ")
  for (i <- 0 until RenameWidth) {
    val offset = PopCount(validDispatch.take(i))
    val roqIdxExt = enqPtrExt + offset
    val roqIdx = roqIdxExt.value

    when(io.dp1Req(i).fire()){
      microOp(roqIdx) := io.dp1Req(i).bits
      valid(roqIdx) := true.B
      flag(roqIdx) := roqIdxExt.flag
      writebacked(roqIdx) := false.B
      when(noSpecEnq(i)){ hasNoSpec := true.B }
    }
    io.dp1Req(i).ready := (notFull && !valid(roqIdx) && state === s_idle) &&
      (!noSpecEnq(i) || isEmpty) &&
      !hasNoSpec
    io.roqIdxs(i) := roqIdxExt
    XSDebug(false, true.B, "(%d, %d) ", io.dp1Req(i).ready, io.dp1Req(i).valid)
  }
  XSDebug(false, true.B, "\n")

  val firedDispatch = Cat(io.dp1Req.map(_.fire()))
  val dispatchCnt = PopCount(firedDispatch)
  when(firedDispatch.orR){
    enqPtrExt := enqPtrExt + dispatchCnt
    XSInfo("dispatched %d insts\n", dispatchCnt)
  }

  // Writeback
  val firedWriteback = io.exeWbResults.map(_.fire())
  XSInfo(PopCount(firedWriteback) > 0.U, "writebacked %d insts\n", PopCount(firedWriteback))
  for(i <- 0 until numWbPorts){
    when(io.exeWbResults(i).fire()){
      val wbIdxExt = io.exeWbResults(i).bits.uop.roqIdx
      val wbIdx = wbIdxExt.value
      writebacked(wbIdx) := true.B
      microOp(wbIdx).cf.exceptionVec := io.exeWbResults(i).bits.uop.cf.exceptionVec
      microOp(wbIdx).lqIdx := io.exeWbResults(i).bits.uop.lqIdx
      microOp(wbIdx).sqIdx := io.exeWbResults(i).bits.uop.sqIdx
      microOp(wbIdx).ctrl.flushPipe := io.exeWbResults(i).bits.uop.ctrl.flushPipe
      microOp(wbIdx).diffTestDebugLrScValid := io.exeWbResults(i).bits.uop.diffTestDebugLrScValid
      exuData(wbIdx) := io.exeWbResults(i).bits.data
      exuFflags(wbIdx) := io.exeWbResults(i).bits.fflags
      exuDebug(wbIdx) := io.exeWbResults(i).bits.debug

      val debugUop = microOp(wbIdx)
      XSInfo(true.B,
        p"writebacked pc 0x${Hexadecimal(debugUop.cf.pc)} wen ${debugUop.ctrl.rfWen} " +
        p"data 0x${Hexadecimal(io.exeWbResults(i).bits.data)} ldst ${debugUop.ctrl.ldest} pdst ${debugUop.ctrl.ldest} " +
        p"skip ${io.exeWbResults(i).bits.debug.isMMIO} roqIdx: ${wbIdxExt}\n"
      )
    }
  }

  val deqUop = microOp(deqPtr)
  val deqPtrWritebacked = writebacked(deqPtr) && valid(deqPtr)
  val intrEnable = io.csr.intrBitSet && !isEmpty && !hasNoSpec &&
    deqUop.ctrl.commitType =/= CommitType.STORE && deqUop.ctrl.commitType =/= CommitType.LOAD// TODO: wanna check why has hasCsr(hasNoSpec)
  val exceptionEnable = deqPtrWritebacked && Cat(deqUop.cf.exceptionVec).orR()
  val isFlushPipe = deqPtrWritebacked && deqUop.ctrl.flushPipe
  io.redirect := DontCare
  io.redirect.valid := (state === s_idle) && (intrEnable || exceptionEnable || isFlushPipe)// TODO: add fence flush to flush the whole pipe
  io.redirect.bits.isException := intrEnable || exceptionEnable
  // reuse isFlushPipe to represent interrupt for CSR
  io.redirect.bits.isFlushPipe := isFlushPipe || intrEnable
  io.redirect.bits.target := Mux(isFlushPipe, deqUop.cf.pc + 4.U, io.csr.trapTarget)
  io.exception := deqUop
  XSDebug(io.redirect.valid,
    "generate redirect: pc 0x%x intr %d excp %d flushpp %d target:0x%x Traptarget 0x%x exceptionVec %b\n",
    io.exception.cf.pc, intrEnable, exceptionEnable, isFlushPipe, io.redirect.bits.target, io.csr.trapTarget,
    Cat(microOp(deqPtr).cf.exceptionVec))

  // Commit uop to Rename (walk)
  val shouldWalkVec = Wire(Vec(CommitWidth, Bool()))
  val walkPtrMatchVec  = Wire(Vec(CommitWidth, Bool()))
  val walkPtrVec = Wire(Vec(CommitWidth, new RoqPtr))
  for(i <- shouldWalkVec.indices){
    walkPtrVec(i) := walkPtrExt - i.U
    walkPtrMatchVec(i) := walkPtrVec(i) === walkTgtExt
    if(i == 0) shouldWalkVec(i) := !walkPtrMatchVec(i)
    else shouldWalkVec(i) := shouldWalkVec(i-1) && !walkPtrMatchVec(i)
  }
  val walkFinished = Cat(walkPtrMatchVec).orR()

  // extra space is used weh roq has no enough space, but mispredict recovery needs such info to walk regmap
  val needExtraSpaceForMPR = WireInit(VecInit(
    List.tabulate(RenameWidth)(i => io.brqRedirect.valid && io.dp1Req(i).valid && !io.dp1Req(i).ready)
  ))
  val extraSpaceForMPR = Reg(Vec(RenameWidth, new MicroOp))
  val usedSpaceForMPR = Reg(Vec(RenameWidth, Bool()))

  val storeCommitVec = WireInit(VecInit(Seq.fill(CommitWidth)(false.B)))
  val cfiCommitVec = WireInit(VecInit(Seq.fill(CommitWidth)(false.B)))
  // wiring to csr
  val fflags = WireInit(0.U.asTypeOf(new Fflags))
  val dirty_fs = WireInit(false.B)
  for(i <- 0 until CommitWidth){
    io.commits(i) := DontCare
    switch(state){
      is(s_idle){
        val commitIdx = deqPtr + i.U
        val commitUop = microOp(commitIdx)
        val hasException = Cat(commitUop.cf.exceptionVec).orR() || intrEnable
        val canCommit = if(i!=0) (io.commits(i-1).valid && !io.commits(i-1).bits.uop.ctrl.flushPipe) else true.B
        val v = valid(commitIdx)
        val w = writebacked(commitIdx)
        io.commits(i).valid := v && w && canCommit && !hasException
        io.commits(i).bits.uop := commitUop

        storeCommitVec(i) := io.commits(i).valid &&
          commitUop.ctrl.commitType === CommitType.STORE

        cfiCommitVec(i) := io.commits(i).valid &&
          !commitUop.cf.brUpdate.pd.notCFI

        val commitFflags = exuFflags(commitIdx)
        when(io.commits(i).valid){
          when(commitFflags.asUInt.orR()){
            // update fflags
            fflags := exuFflags(commitIdx)
          }
          when(commitUop.ctrl.fpWen){
            // set fs to dirty
            dirty_fs := true.B
          }
        }

        when(io.commits(i).valid){v := false.B}
        XSInfo(io.commits(i).valid,
          "retired pc %x wen %d ldest %d pdest %x old_pdest %x data %x fflags: %b\n",
          commitUop.cf.pc,
          commitUop.ctrl.rfWen,
          commitUop.ctrl.ldest,
          commitUop.pdest,
          commitUop.old_pdest,
          exuData(commitIdx),
          exuFflags(commitIdx).asUInt
        )
        XSInfo(io.commits(i).valid && exuDebug(commitIdx).isMMIO,
          "difftest skiped pc0x%x\n",
          commitUop.cf.pc
        )
      }

      is(s_walk){
        val idx = walkPtrVec(i).value
        val v = valid(idx)
        val walkUop = microOp(idx)
        io.commits(i).valid := v && shouldWalkVec(i)
        io.commits(i).bits.uop := walkUop
        when(shouldWalkVec(i)){
          v := false.B
        }
        XSInfo(io.commits(i).valid && shouldWalkVec(i), "walked pc %x wen %d ldst %d data %x\n",
          walkUop.cf.pc,
          walkUop.ctrl.rfWen,
          walkUop.ctrl.ldest,
          exuData(idx)
        )
      }

      is(s_extrawalk){
        val idx = RenameWidth-i-1
        val walkUop = extraSpaceForMPR(idx)
        io.commits(i).valid := usedSpaceForMPR(idx)
        io.commits(i).bits.uop := walkUop
        state := s_walk
        XSInfo(io.commits(i).valid, "use extra space walked pc %x wen %d ldst %d\n",
          walkUop.cf.pc,
          walkUop.ctrl.rfWen,
          walkUop.ctrl.ldest
        )
      }
    }
    io.commits(i).bits.isWalk := state =/= s_idle
  }

  io.csr.fflags := fflags
  io.csr.dirty_fs := dirty_fs

  val validCommit = io.commits.map(_.valid)
  when(state===s_walk) {
    //exit walk state when all roq entry is commited
    when(walkFinished) {
      state := s_idle
    }
    walkPtrExt := walkPtrExt - CommitWidth.U
    // ringBufferWalkExtended := ringBufferWalkExtended - validCommit
    XSInfo("rolling back: enqPtr %d deqPtr %d walk %d:%d\n", enqPtr, deqPtr, walkPtrExt.flag, walkPtr)
  }

  // move tail ptr
  val commitCnt = PopCount(validCommit)
  when(state === s_idle){
    deqPtrExt := deqPtrExt + commitCnt
  }
  val retireCounter = Mux(state === s_idle, commitCnt, 0.U)
  XSInfo(retireCounter > 0.U, "retired %d insts\n", retireCounter)
  val commitOffset = PriorityEncoder((validCommit :+ false.B).map(!_))
  io.commitRoqIndex.valid := state === s_idle
  io.commitRoqIndex.bits := deqPtrExt + commitOffset

  // commit branch to brq
  io.bcommit := PopCount(cfiCommitVec)

  // when redirect, walk back roq entries
  when(io.brqRedirect.valid){ // TODO: need check if consider exception redirect?
    state := s_walk
    walkPtrExt := Mux(state === s_walk && !walkFinished, walkPtrExt - CommitWidth.U, Mux(state === s_extrawalk, walkPtrExt, enqPtrExt - 1.U + dispatchCnt))
    walkTgtExt := io.brqRedirect.bits.roqIdx
    enqPtrExt := io.brqRedirect.bits.roqIdx + 1.U
  }

  // no enough space for walk, allocate extra space
  when(needExtraSpaceForMPR.asUInt.orR && io.brqRedirect.valid){
    usedSpaceForMPR := needExtraSpaceForMPR
    (0 until RenameWidth).foreach(i => extraSpaceForMPR(i) := io.dp1Req(i).bits)
    state := s_extrawalk
    XSDebug("roq full, switched to s_extrawalk. needExtraSpaceForMPR: %b\n", needExtraSpaceForMPR.asUInt)
  }

  // when rollback, reset writebacked entry to valid
  when(io.memRedirect.valid) { // TODO: opt timing
    for (i <- 0 until RoqSize) {
      val recRoqIdx = RoqPtr(flag(i), i.U)
      when (valid(i) && isAfter(recRoqIdx, io.memRedirect.bits.roqIdx)) {
        writebacked(i) := false.B
      }
    }
  }

  // when exception occurs, cancels all
  when (io.redirect.valid) { // TODO: need check for flushPipe
    enqPtrExt := 0.U.asTypeOf(new RoqPtr)
    deqPtrExt := 0.U.asTypeOf(new RoqPtr)
    for (i <- 0 until RoqSize) {
      valid(i) := false.B
    }
  }

  // debug info
  XSDebug(p"enqPtr ${enqPtrExt} deqPtr ${deqPtrExt}\n")
  XSDebug("")
  for(i <- 0 until RoqSize){
    XSDebug(false, !valid(i), "-")
    XSDebug(false, valid(i) && writebacked(i), "w")
    XSDebug(false, valid(i) && !writebacked(i), "v")
  }
  XSDebug(false, true.B, "\n")

  for(i <- 0 until RoqSize){
    if(i % 4 == 0) XSDebug("")
    XSDebug(false, true.B, "%x ", microOp(i).cf.pc)
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
  
  if(!env.FPGAPlatform){ 

    //difftest signals
    val firstValidCommit = deqPtr + PriorityMux(validCommit, VecInit(List.tabulate(CommitWidth)(_.U)))

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
      val idx = deqPtr+i.U
      val uop = io.commits(i).bits.uop
      val DifftestSkipSC = false
      if(!DifftestSkipSC){
        skip(i) := exuDebug(idx).isMMIO && io.commits(i).valid
      }else{
        skip(i) := (
            exuDebug(idx).isMMIO || 
            uop.ctrl.fuType === FuType.mou && uop.ctrl.fuOpType === LSUOpType.sc_d ||
            uop.ctrl.fuType === FuType.mou && uop.ctrl.fuOpType === LSUOpType.sc_w
          ) && io.commits(i).valid
      }
      wen(i) := io.commits(i).valid && uop.ctrl.rfWen && uop.ctrl.ldest =/= 0.U
      wdata(i) := exuData(idx)
      wdst(i) := uop.ctrl.ldest
      diffTestDebugLrScValid(i) := uop.diffTestDebugLrScValid
      wpc(i) := SignExt(uop.cf.pc, XLEN)
      trapVec(i) := io.commits(i).valid && (state===s_idle) && uop.ctrl.isXSTrap
      isRVC(i) := uop.cf.brUpdate.pd.isRVC
    }

    val scFailed = !diffTestDebugLrScValid(0) && 
      io.commits(0).bits.uop.ctrl.fuType === FuType.mou &&
      (io.commits(0).bits.uop.ctrl.fuOpType === LSUOpType.sc_d || io.commits(0).bits.uop.ctrl.fuOpType === LSUOpType.sc_w)

    val instrCnt = RegInit(0.U(64.W))
    instrCnt := instrCnt + retireCounter

    XSDebug(difftestIntrNO =/= 0.U, "difftest intrNO set %x\n", difftestIntrNO)
    val retireCounterFix = Mux(io.redirect.valid, 1.U, retireCounter)
    val retirePCFix = SignExt(Mux(io.redirect.valid, microOp(deqPtr).cf.pc, microOp(firstValidCommit).cf.pc), XLEN)
    val retireInstFix = Mux(io.redirect.valid, microOp(deqPtr).cf.instr, microOp(firstValidCommit).cf.instr)

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
    val deqNotWritebacked = valid(deqPtr) && !writebacked(deqPtr)
    val deqUopCommitType = deqUop.ctrl.commitType
    ExcitingUtils.addSource(deqNotWritebacked && deqUopCommitType === CommitType.INT,   "perfCntCondRoqWaitInt",   Perf)
    ExcitingUtils.addSource(deqNotWritebacked && deqUopCommitType === CommitType.FP,    "perfCntCondRoqWaitFp",    Perf)
    ExcitingUtils.addSource(deqNotWritebacked && deqUopCommitType === CommitType.LOAD,  "perfCntCondRoqWaitLoad",  Perf)
    ExcitingUtils.addSource(deqNotWritebacked && deqUopCommitType === CommitType.STORE, "perfCntCondRoqWaitStore", Perf)

    if(EnableBPU){
      ExcitingUtils.addSource(hitTrap, "XSTRAP", ConnectionType.Debug)
    }
  }
}
