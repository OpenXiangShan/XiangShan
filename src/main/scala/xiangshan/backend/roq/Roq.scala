package xiangshan.backend.roq

import chisel3.ExcitingUtils.ConnectionType
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import chisel3.util.experimental.BoringUtils
import xiangshan.backend.LSUOpType

// A "just-enough" Roq
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
    val mcommit = Vec(CommitWidth, Valid(UInt(LsroqIdxWidth.W)))
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
  val csrEnRoq = io.dp1Req.map(i => i.bits.ctrl.fuType === FuType.csr)
  val hasCsr = RegInit(false.B)
  when(isEmpty){ hasCsr:= false.B }
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
      when(csrEnRoq(i)){ hasCsr := true.B }
    }
    io.dp1Req(i).ready := (notFull && !valid(roqIdx) && state === s_idle) &&
      (!csrEnRoq(i) || isEmpty) &&
      !hasCsr
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
      exuData(wbIdx) := io.exeWbResults(i).bits.data
      exuDebug(wbIdx) := io.exeWbResults(i).bits.debug

      val debugUop = microOp(wbIdx)
      XSInfo(true.B, "writebacked pc 0x%x wen %d data 0x%x ldst %d pdst %d skip %x roqIdx: %x\n",
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
  val intrEnable = intrBitSet && (state === s_idle) && !isEmpty && !hasCsr
  val exceptionEnable = Cat(deqUop.cf.exceptionVec).orR() && (state === s_idle) && !isEmpty

  val isEcall = deqUop.cf.exceptionVec(ecallM) ||
    deqUop.cf.exceptionVec(ecallS) ||
    deqUop.cf.exceptionVec(ecallU)
  io.redirect := DontCare
  io.redirect.valid := intrEnable || exceptionEnable
  io.redirect.bits.isException := true.B
  io.redirect.bits.target := trapTarget
  io.exception := deqUop
  XSDebug(io.redirect.valid, "generate exception: pc 0x%x target 0x%x exceptionVec %b\n", io.exception.cf.pc, trapTarget, Cat(microOp(deqPtr).cf.exceptionVec))

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
        val canCommit = if(i!=0) io.commits(i-1).valid else true.B
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
          "retired pc %x wen %d ldst %d data %x\n",
          commitUop.cf.pc,
          commitUop.ctrl.rfWen,
          commitUop.ctrl.ldest,
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

  

  // commit store to lsu, commit branch to brq
  // TODO MMIO
  (0 until CommitWidth).map(i => {
    io.mcommit(i).valid := storeCommitVec(i)
    io.mcommit(i).bits := io.commits(i).bits.uop.lsroqIdx
  })

  io.bcommit := PopCount(cfiCommitVec)

  // when redirect, walk back roq entries
  when(io.brqRedirect.valid){
    state := s_walk
    walkPtrExt := enqPtrExt - 1.U + dispatchCnt
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
  when(io.brqRedirect.valid && io.brqRedirect.bits.isReplay){ // TODO: opt timing
    for (i <- 0 until RoqSize) {
      val recRoqIdx = Cat(flag(i).asUInt, i.U)
      when(valid(i) && io.memRedirect.bits.isAfter(recRoqIdx)){
        writebacked(i) := false.B
      }
    }
  }

  // when exception occurs, cancels all
  when (io.redirect.valid) {
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
  val wpc = Wire(Vec(CommitWidth, UInt(VAddrBits.W)))
  val trapVec = Wire(Vec(CommitWidth, Bool()))
  for(i <- 0 until CommitWidth){
    // io.commits(i).valid
    val idx = deqPtr+i.U
    val uop = io.commits(i).bits.uop
    skip(i) := exuDebug(idx).isMMIO && io.commits(i).valid
    wen(i) := io.commits(i).valid && uop.ctrl.rfWen && uop.ctrl.ldest =/= 0.U
    wdata(i) := exuData(idx)
    wdst(i) := uop.ctrl.ldest
    wpc(i) := uop.cf.pc
    trapVec(i) := io.commits(i).valid && (state===s_idle) && uop.ctrl.isXSTrap
  }
  val instrCnt = RegInit(0.U(64.W))
  instrCnt := instrCnt + retireCounter

  val difftestIntrNO = WireInit(0.U(XLEN.W))
  ExcitingUtils.addSink(difftestIntrNO, "difftestIntrNOfromCSR")
  XSDebug(difftestIntrNO =/= 0.U, "difftest intrNO set %x\n", difftestIntrNO)
  val retireCounterFix = Mux(io.redirect.valid, 1.U, retireCounter)
  val retirePCFix = Mux(io.redirect.valid, microOp(deqPtr).cf.pc, microOp(firstValidCommit).cf.pc)
  val retireInstFix = Mux(io.redirect.valid, microOp(deqPtr).cf.instr, microOp(firstValidCommit).cf.instr)
  if(!env.FPGAPlatform){
    BoringUtils.addSource(RegNext(retireCounterFix), "difftestCommit")
    BoringUtils.addSource(RegNext(retirePCFix), "difftestThisPC")//first valid PC
    BoringUtils.addSource(RegNext(retireInstFix), "difftestThisINST")//first valid inst
    BoringUtils.addSource(RegNext(skip.asUInt), "difftestSkip")
    BoringUtils.addSource(RegNext(false.B), "difftestIsRVC")//FIXIT
    BoringUtils.addSource(RegNext(wen.asUInt), "difftestWen")
    BoringUtils.addSource(RegNext(wpc), "difftestWpc")
    BoringUtils.addSource(RegNext(wdata), "difftestWdata")
    BoringUtils.addSource(RegNext(wdst), "difftestWdst")
    BoringUtils.addSource(RegNext(difftestIntrNO), "difftestIntrNO")

    val hitTrap = trapVec.reduce(_||_)
    val trapCode = PriorityMux(wdata.zip(trapVec).map(x => x._2 -> x._1))
    val trapPC = PriorityMux(wpc.zip(trapVec).map(x => x._2 ->x._1))

    ExcitingUtils.addSource(RegNext(hitTrap), "trapValid")
    ExcitingUtils.addSource(RegNext(trapCode), "trapCode")
    ExcitingUtils.addSource(RegNext(trapPC), "trapPC")
    ExcitingUtils.addSource(RegNext(GTimer()), "trapCycleCnt")
    ExcitingUtils.addSource(RegNext(instrCnt), "trapInstrCnt")

    if(EnableBPU){
      ExcitingUtils.addSource(hitTrap, "XSTRAP", ConnectionType.Debug)
    }
  }
}
