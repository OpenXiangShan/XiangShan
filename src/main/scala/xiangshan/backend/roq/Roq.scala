package xiangshan.backend.roq

import chisel3.ExcitingUtils._
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import chisel3.util.experimental.BoringUtils
import xiangshan.backend.LSUOpType


class Roq extends XSModule {
  val io = IO(new Bundle() {
    val brqRedirect = Input(Valid(new Redirect))
    val memRedirect = Input(Valid(new Redirect))
    val dp1Req = Vec(RenameWidth, Flipped(DecoupledIO(new MicroOp)))
    val roqIdxs = Output(Vec(RenameWidth, UInt(RoqIdxWidth.W)))
    val redirect = Output(Valid(new Redirect))
    val exception = Output(new MicroOp)
    // exu + brq
    val exeWbResults = Vec(exuParameters.ExuCnt + 1, Flipped(ValidIO(new ExuOutput)))
    val commits = Vec(CommitWidth, Valid(new RoqCommit))
    val bcommit = Output(UInt(BrTagWidth.W))
  })

  val numWbPorts = io.exeWbResults.length

  val microOp = Mem(RoqSize, new MicroOp)
  val valid = RegInit(VecInit(List.fill(RoqSize)(false.B)))
  val flag = RegInit(VecInit(List.fill(RoqSize)(false.B)))
  val writebacked = Reg(Vec(RoqSize, Bool()))

  val exuData = Reg(Vec(RoqSize, UInt(XLEN.W)))//for debug
  val exuDebug = Reg(Vec(RoqSize, new DebugBundle))//for debug

  val enqPtrExt = RegInit(0.U(RoqIdxWidth.W))
  val deqPtrExt = RegInit(0.U(RoqIdxWidth.W))
  val walkPtrExt = Reg(UInt(RoqIdxWidth.W))
  val walkTgtExt = Reg(UInt(RoqIdxWidth.W))
  val enqPtr = enqPtrExt(InnerRoqIdxWidth-1,0)
  val deqPtr = deqPtrExt(InnerRoqIdxWidth-1,0)
  val walkPtr = walkPtrExt(InnerRoqIdxWidth-1,0)
  val isEmpty = enqPtr === deqPtr && enqPtrExt.head(1)===deqPtrExt.head(1)
  val isFull = enqPtr === deqPtr && enqPtrExt.head(1)=/=deqPtrExt.head(1)
  val notFull = !isFull

  val s_idle :: s_walk :: s_extrawalk :: Nil = Enum(3)
  val state = RegInit(s_idle)

  // Dispatch
  val noSpecEnq = io.dp1Req.map(i => i.bits.ctrl.noSpecExec)
  val hasNoSpec = RegInit(false.B)
  when(isEmpty){ hasNoSpec:= false.B }
  val validDispatch = io.dp1Req.map(_.valid)
  XSDebug("(ready, valid): ")
  for (i <- 0 until RenameWidth) {
    val offset = PopCount(validDispatch.take(i))
    val roqIdxExt = enqPtrExt + offset
    val roqIdx = roqIdxExt.tail(1)

    when(io.dp1Req(i).fire()){
      microOp(roqIdx) := io.dp1Req(i).bits
      valid(roqIdx) := true.B
      flag(roqIdx) := roqIdxExt.head(1).asBool()
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
      val wbIdx = wbIdxExt.tail(1)
      writebacked(wbIdx) := true.B
      microOp(wbIdx).cf.exceptionVec := io.exeWbResults(i).bits.uop.cf.exceptionVec
      microOp(wbIdx).ctrl.flushPipe := io.exeWbResults(i).bits.uop.ctrl.flushPipe
      microOp(wbIdx).diffTestDebugLrScValid := io.exeWbResults(i).bits.uop.diffTestDebugLrScValid
      exuData(wbIdx) := io.exeWbResults(i).bits.data
      exuDebug(wbIdx) := io.exeWbResults(i).bits.debug

      val debugUop = microOp(wbIdx)
      XSInfo(true.B, "writebacked pc 0x%x wen %d data 0x%x ldst %d pdst %d skip %x roqIdx: %d\n",
        debugUop.cf.pc,
        debugUop.ctrl.rfWen,
        io.exeWbResults(i).bits.data,
        debugUop.ctrl.ldest,
        io.exeWbResults(i).bits.uop.pdest,
        io.exeWbResults(i).bits.debug.isMMIO,
        wbIdxExt
      )
    }
  }

  // roq redirect only used for exception
  val intrBitSet = WireInit(false.B)
  ExcitingUtils.addSink(intrBitSet, "intrBitSetIDU")
  val trapTarget = WireInit(0.U(VAddrBits.W))
  ExcitingUtils.addSink(trapTarget, "trapTarget")

  val deqUop = microOp(deqPtr)
  val intrEnable = intrBitSet && (state === s_idle) && !isEmpty && !hasNoSpec // TODO: wanna check why has hasCsr(hasNoSpec)
  val exceptionEnable = Cat(deqUop.cf.exceptionVec).orR() && (state === s_idle) && !isEmpty
  // TODO: need check if writebacked needed
  val isEcall = deqUop.cf.exceptionVec(ecallM) ||
    deqUop.cf.exceptionVec(ecallS) ||
    deqUop.cf.exceptionVec(ecallU)
  val isFlushPipe = (deqUop.ctrl.flushPipe && writebacked(deqPtr) && valid(deqPtr) && (state === s_idle) && !isEmpty)
  io.redirect := DontCare
  io.redirect.valid := intrEnable || exceptionEnable || isFlushPipe// TODO: add fence flush to flush the whole pipe
  io.redirect.bits.isException := intrEnable || exceptionEnable
  io.redirect.bits.isFlushPipe := isFlushPipe
  io.redirect.bits.target := Mux(isFlushPipe, deqUop.cf.pc + 4.U, trapTarget)
  io.exception := deqUop
  XSDebug(io.redirect.valid, "generate redirect: pc 0x%x intr %d excp %d flushpp %d target:0x%x Traptarget 0x%x exceptionVec %b\n", io.exception.cf.pc, intrEnable, exceptionEnable, isFlushPipe, io.redirect.bits.target, trapTarget, Cat(microOp(deqPtr).cf.exceptionVec))

  // Commit uop to Rename (walk)
  val shouldWalkVec = Wire(Vec(CommitWidth, Bool()))
  val walkPtrMatchVec  = Wire(Vec(CommitWidth, Bool()))
  val walkPtrVec = Wire(Vec(CommitWidth, UInt(RoqIdxWidth.W)))
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

        when(io.commits(i).valid){v := false.B}
        XSInfo(io.commits(i).valid,
          "retired pc %x wen %d ldest %d pdest %x old_pdest %x data %x\n",
          commitUop.cf.pc,
          commitUop.ctrl.rfWen,
          commitUop.ctrl.ldest,
          commitUop.pdest,
          commitUop.old_pdest,
          exuData(commitIdx)
        )
        XSInfo(io.commits(i).valid && exuDebug(commitIdx).isMMIO,
          "difftest skiped pc0x%x\n",
          commitUop.cf.pc
        )
      }

      is(s_walk){
        val idx = walkPtrVec(i).tail(1)
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

  val validCommit = io.commits.map(_.valid)
  when(state===s_walk) {
    //exit walk state when all roq entry is commited
    when(walkFinished) {
      state := s_idle
    }
    walkPtrExt := walkPtrExt - CommitWidth.U
    // ringBufferWalkExtended := ringBufferWalkExtended - validCommit
    XSInfo("rolling back: enqPtr %d deqPtr %d walk %d:%d\n", enqPtr, deqPtr, walkPtrExt.head(1), walkPtr)
  }

  // move tail ptr
  val commitCnt = PopCount(validCommit)
  when(state === s_idle){
    deqPtrExt := deqPtrExt + commitCnt
  }
  val retireCounter = Mux(state === s_idle, commitCnt, 0.U)
  XSInfo(retireCounter > 0.U, "retired %d insts\n", retireCounter)

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
      val recRoqIdx = Wire(new XSBundle with HasRoqIdx)
      recRoqIdx.roqIdx := Cat(flag(i).asUInt, i.U((RoqIdxWidth - 1).W))
      when (valid(i) && recRoqIdx.isAfter(io.memRedirect.bits)) {
        writebacked(i) := false.B
      }
    }
  }

  // when exception occurs, cancels all
  when (io.redirect.valid) { // TODO: need check for flushPipe
    enqPtrExt := 0.U
    deqPtrExt := 0.U
    for (i <- 0 until RoqSize) {
      valid(i) := false.B
    }
  }

  // debug info
  XSDebug("enqPtr %d:%d deqPtr %d:%d\n", enqPtrExt.head(1), enqPtr, deqPtrExt.head(1), deqPtr)
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
  val instrCnt = RegInit(0.U(64.W))
  instrCnt := instrCnt + retireCounter

  val difftestIntrNO = WireInit(0.U(XLEN.W))
  ExcitingUtils.addSink(difftestIntrNO, "difftestIntrNOfromCSR")
  XSDebug(difftestIntrNO =/= 0.U, "difftest intrNO set %x\n", difftestIntrNO)
  val retireCounterFix = Mux(io.redirect.valid, 1.U, retireCounter)
  val retirePCFix = SignExt(Mux(io.redirect.valid, microOp(deqPtr).cf.pc, microOp(firstValidCommit).cf.pc), XLEN)
  val retireInstFix = Mux(io.redirect.valid, microOp(deqPtr).cf.instr, microOp(firstValidCommit).cf.instr)
  if(!env.FPGAPlatform){
    BoringUtils.addSource(RegNext(retireCounterFix), "difftestCommit")
    BoringUtils.addSource(RegNext(retirePCFix), "difftestThisPC")//first valid PC
    BoringUtils.addSource(RegNext(retireInstFix), "difftestThisINST")//first valid inst
    BoringUtils.addSource(RegNext(skip.asUInt), "difftestSkip")
    // BoringUtils.addSource(RegNext(false.B), "difftestIsRVC")//FIXIT
    BoringUtils.addSource(RegNext(isRVC.asUInt), "difftestIsRVC")
    BoringUtils.addSource(RegNext(wen.asUInt), "difftestWen")
    BoringUtils.addSource(RegNext(wpc), "difftestWpc")
    BoringUtils.addSource(RegNext(wdata), "difftestWdata")
    BoringUtils.addSource(RegNext(wdst), "difftestWdst")
    BoringUtils.addSource(RegNext(diffTestDebugLrScValid), "diffTestDebugLrScValid")
    BoringUtils.addSource(RegNext(difftestIntrNO), "difftestIntrNO")

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
