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

class RoqEnqIO extends XSBundle {
  val canAccept = Output(Bool())
  val isEmpty = Output(Bool())
  // valid vector, for roqIdx gen and walk
  val needAlloc = Vec(RenameWidth, Input(Bool()))
  val req = Vec(RenameWidth, Flipped(ValidIO(new MicroOp)))
  val resp = Vec(RenameWidth, Output(new RoqPtr))
}

class RoqDataModule(numRead: Int, numWrite: Int) extends XSModule {
  val io = IO(new Bundle {
    val raddr = Vec(numRead, Input(new RoqPtr))
    val rdata = Vec(numRead, Output(new RoqCommitInfo))
    val wen = Vec(numWrite, Input(Bool()))
    val waddr = Vec(numWrite, Input(new RoqPtr))
    val wdata = Vec(numWrite, Input(new RoqCommitInfo))
  })

  val data = Mem(RoqSize, new RoqCommitInfo)
  for (i <- 0 until numRead) {
    io.rdata(i) := data(io.raddr(i).value)
  }
  for (i <- 0 until numWrite) {
    when (io.wen(i)) {
      data(io.waddr(i).value) := io.wdata(i)
    }
  }
}

class Roq(numWbPorts: Int) extends XSModule with HasCircularQueuePtrHelper {
  val io = IO(new Bundle() {
    val brqRedirect = Input(Valid(new Redirect))
    val memRedirect = Input(Valid(new Redirect))
    val enq = new RoqEnqIO
    val redirect = Output(Valid(new Redirect))
    val exception = Output(new MicroOp)
    // exu + brq
    val exeWbResults = Vec(numWbPorts, Flipped(ValidIO(new ExuOutput)))
    val commits = new RoqCommitIO
    val bcommit = Output(UInt(BrTagWidth.W))
    val roqDeqPtr = Output(new RoqPtr)
    val csr = new RoqCSRIO
  })

  // instvalid field
  val valid = RegInit(VecInit(List.fill(RoqSize)(false.B)))

  // status
  val writebacked = Reg(Vec(RoqSize, Bool()))

  // data for redirect, exception, etc.
  val flagBkup = RegInit(VecInit(List.fill(RoqSize)(false.B)))
  val exuFflags = Mem(RoqSize, new Fflags)

  // uop field used when commit
  // flushPipe (wb) (commit) (used in roq)
  // lidx (wb) (commit)
  // sidx (wb) (commit)
  // uop.ctrl.commitType (wb) (commit) (L/S)
  // exceptionVec (wb) (commit)
  // roqIdx (dispatch) (commit)
  // crossPageIPFFix (dispatch) (commit)

  // uop field used when walk
  // ctrl.fpWen (dispatch) (walk)
  // ctrl.rfWen (dispatch) (walk)
  // ldest (dispatch) (walk)

  // data for debug
  val microOp = Mem(RoqSize, new MicroOp)
  val debug_exuData = Reg(Vec(RoqSize, UInt(XLEN.W)))//for debug
  val debug_exuDebug = Reg(Vec(RoqSize, new DebugBundle))//for debug

  // ptr
  val enqPtr = RegInit(0.U.asTypeOf(new RoqPtr))
  val deqPtrVec = RegInit(VecInit((0 until CommitWidth).map(_.U.asTypeOf(new RoqPtr))))
  val walkPtrVec = Reg(Vec(CommitWidth, new RoqPtr))

  val enqPtrVec = VecInit((0 until RenameWidth).map(i => enqPtr + PopCount(io.enq.needAlloc.take(i))))
  val deqPtr = deqPtrVec(0)
  val walkPtr = walkPtrVec(0)

  val isEmpty = enqPtr === deqPtr

  val s_idle :: s_walk :: s_extrawalk :: Nil = Enum(3)
  val state = RegInit(s_idle)

  io.roqDeqPtr := deqPtr

  // For enqueue ptr, we don't duplicate it since only enqueue needs it.


  /**
    * CommitDataModule: store commit info separately
    * (1) read: commits/walk
    * (2) write: enqueue
    */
  val commitData = Module(new RoqDataModule(CommitWidth, RenameWidth))
  val deqCommitData = commitData.io.rdata(0)
  for (i <- 0 until RenameWidth) {
    commitData.io.wen(i) := false.B
    commitData.io.waddr(i) := enqPtrVec(i)
    commitData.io.wdata(i).ldest := io.enq.req(i).bits.ctrl.ldest
    commitData.io.wdata(i).rfWen := io.enq.req(i).bits.ctrl.rfWen
    commitData.io.wdata(i).fpWen := io.enq.req(i).bits.ctrl.fpWen
    commitData.io.wdata(i).commitType := io.enq.req(i).bits.ctrl.commitType
    commitData.io.wdata(i).pdest := io.enq.req(i).bits.pdest
    commitData.io.wdata(i).old_pdest := io.enq.req(i).bits.old_pdest
    commitData.io.wdata(i).lqIdx := io.enq.req(i).bits.lqIdx
    commitData.io.wdata(i).sqIdx := io.enq.req(i).bits.sqIdx
    commitData.io.wdata(i).pc := io.enq.req(i).bits.cf.pc
  }
  for (i <- 0 until CommitWidth) {
    commitData.io.raddr(i) := walkPtrVec(i)
    when (state === s_idle) {
      commitData.io.raddr(i) := deqPtrVec(i)
    }
  }

  /**
    * Enqueue (from dispatch)
    */
  // special cases
  val hasBlockBackward = RegInit(false.B)
  val hasNoSpecExec = RegInit(false.B)
  // When blockBackward instruction leaves Roq (commit or walk), hasBlockBackward should be set to false.B
  // val blockBackwardLeave = Cat((0 until CommitWidth).map(i => io.commits.valid(i) && io.commits.uop(i).ctrl.blockBackward)).orR
  when (isEmpty) { hasBlockBackward:= false.B }
  // When noSpecExec instruction commits (it should not be walked except when it has not entered Roq),
  // hasNoSpecExec should be set to false.B
  // val noSpecExecCommit = !io.commits.isWalk && Cat((0 until CommitWidth).map(i => io.commits.valid(i) && io.commits.uop(i).ctrl.noSpecExec)).orR
  when (io.commits.valid.asUInt.orR) { hasNoSpecExec:= false.B }
  // Assertion on that noSpecExec should never be walked since it's the only instruction in Roq.
  // Extra walk should be ok since noSpecExec has not enter Roq.
  // val walkNoSpecExec = io.commits.isWalk && Cat((0 until CommitWidth).map(i => io.commits.valid(i) && io.commits.uop(i).ctrl.noSpecExec)).orR
  // XSError(state =/= s_extrawalk && walkNoSpecExec, "noSpecExec should not walk\n")

  for (i <- 0 until RenameWidth) {
    when(io.enq.req(i).valid && io.enq.canAccept) {
      // store uop in data module and microOp Vec
      commitData.io.wen(i) := true.B
      microOp(enqPtrVec(i).value) := io.enq.req(i).bits
      when(io.enq.req(i).bits.ctrl.blockBackward) {
        hasBlockBackward := true.B
      }
      when(io.enq.req(i).bits.ctrl.noSpecExec) {
        hasNoSpecExec := true.B
      }
    }
    io.enq.resp(i) := enqPtrVec(i)
  }

  val validEntries = distanceBetween(enqPtr, deqPtr)
  val firedDispatch = Mux(io.enq.canAccept, PopCount(Cat(io.enq.req.map(_.valid))), 0.U)
  io.enq.canAccept := (validEntries <= (RoqSize - RenameWidth).U) && !hasBlockBackward
  io.enq.isEmpty   := isEmpty
  XSDebug(p"(ready, valid): ${io.enq.canAccept}, ${Binary(Cat(io.enq.req.map(_.valid)))}\n")

  enqPtr := enqPtr + firedDispatch
  when (firedDispatch =/= 0.U) {
    XSInfo("dispatched %d insts\n", firedDispatch)
  }

  /**
    * Writeback (from execution units)
    */
  val firedWriteback = io.exeWbResults.map(_.fire())
  XSInfo(PopCount(firedWriteback) > 0.U, "writebacked %d insts\n", PopCount(firedWriteback))
  for(i <- 0 until numWbPorts) {
    when(io.exeWbResults(i).fire()){
      val wbIdxExt = io.exeWbResults(i).bits.uop.roqIdx
      val wbIdx = wbIdxExt.value
      microOp(wbIdx).cf.exceptionVec := io.exeWbResults(i).bits.uop.cf.exceptionVec
      microOp(wbIdx).ctrl.flushPipe := io.exeWbResults(i).bits.uop.ctrl.flushPipe
      microOp(wbIdx).diffTestDebugLrScValid := io.exeWbResults(i).bits.uop.diffTestDebugLrScValid
      debug_exuData(wbIdx) := io.exeWbResults(i).bits.data
      debug_exuDebug(wbIdx) := io.exeWbResults(i).bits.debug

      val debug_Uop = microOp(wbIdx)
      XSInfo(true.B,
        p"writebacked pc 0x${Hexadecimal(debug_Uop.cf.pc)} wen ${debug_Uop.ctrl.rfWen} " +
        p"data 0x${Hexadecimal(io.exeWbResults(i).bits.data)} ldst ${debug_Uop.ctrl.ldest} pdst ${debug_Uop.pdest} " +
        p"skip ${io.exeWbResults(i).bits.debug.isMMIO} roqIdx: ${wbIdxExt}\n"
      )
    }
  }

  /**
    * Interrupt and Exceptions
    */
  val deqUop = microOp(deqPtr.value)
  val deqPtrWritebacked = writebacked(deqPtr.value) && valid(deqPtr.value)
  val intrEnable = io.csr.intrBitSet && !isEmpty && !hasNoSpecExec &&
    deqCommitData.commitType =/= CommitType.STORE && deqCommitData.commitType =/= CommitType.LOAD
  val exceptionEnable = deqPtrWritebacked && Cat(deqUop.cf.exceptionVec).orR()
  val isFlushPipe = deqPtrWritebacked && deqUop.ctrl.flushPipe
  io.redirect := DontCare
  io.redirect.valid := (state === s_idle) && (intrEnable || exceptionEnable || isFlushPipe)
  io.redirect.bits.isException := intrEnable || exceptionEnable
  // reuse isFlushPipe to represent interrupt for CSR
  io.redirect.bits.isFlushPipe := isFlushPipe || intrEnable
  io.redirect.bits.target := Mux(isFlushPipe, deqCommitData.pc + 4.U, io.csr.trapTarget)
  io.exception := deqUop
  io.exception.ctrl.commitType := deqCommitData.commitType
  io.exception.lqIdx := deqCommitData.lqIdx
  io.exception.sqIdx := deqCommitData.sqIdx
  io.exception.cf.pc := deqCommitData.pc
  XSDebug(io.redirect.valid,
    "generate redirect: pc 0x%x intr %d excp %d flushpp %d target:0x%x Traptarget 0x%x exceptionVec %b\n",
    io.exception.cf.pc, intrEnable, exceptionEnable, isFlushPipe, io.redirect.bits.target, io.csr.trapTarget,
    Cat(microOp(deqPtr.value).cf.exceptionVec))

  /**
    * Commits (and walk)
    * They share the same width.
    */
  val walkCounter = Reg(UInt(log2Up(RoqSize).W))
  val shouldWalkVec = Wire(Vec(CommitWidth, Bool()))
  for(i <- shouldWalkVec.indices){
    shouldWalkVec(i) := i.U < walkCounter
  }
  val walkFinished = walkCounter <= CommitWidth.U

  // extra space is used weh roq has no enough space, but mispredict recovery needs such info to walk regmap
  val needExtraSpaceForMPR = WireInit(VecInit(
    List.tabulate(RenameWidth)(i => io.brqRedirect.valid && io.enq.needAlloc(i))
  ))
  val extraSpaceForMPR = Reg(Vec(RenameWidth, new RoqCommitInfo))
  val usedSpaceForMPR = Reg(Vec(RenameWidth, Bool()))

  val storeCommitVec = WireInit(VecInit(Seq.fill(CommitWidth)(false.B)))
  val cfiCommitVec = WireInit(VecInit(Seq.fill(CommitWidth)(false.B)))
  // wiring to csr
  val fflags = WireInit(0.U.asTypeOf(new Fflags))
  val dirty_fs = WireInit(false.B)

  io.commits.isWalk := state =/= s_idle
  for (i <- 0 until CommitWidth) {
    io.commits.valid(i) := false.B
    val commitInfo = commitData.io.rdata(i)
    io.commits.info(i) := commitInfo
    switch (state) {
      is (s_idle) {
        val commitIdx = deqPtrVec(i).value
        val commitUop = microOp(commitIdx)

        val hasException = Cat(commitUop.cf.exceptionVec).orR() || intrEnable
        val canCommit = if(i!=0) (io.commits.valid(i-1) && !microOp(deqPtrVec(i-1).value).ctrl.flushPipe) else true.B
        val v = valid(commitIdx)
        val w = writebacked(commitIdx)
        io.commits.valid(i) := v && w && canCommit && !hasException
        storeCommitVec(i) := io.commits.valid(i) && CommitType.isLoadStore(commitInfo.commitType) && CommitType.lsInstIsStore(commitInfo.commitType)
        cfiCommitVec(i) := io.commits.valid(i) && CommitType.isBranch(commitInfo.commitType)

        val commitFflags = exuFflags(commitIdx)
        when(io.commits.valid(i)){
          when(commitFflags.asUInt.orR()){
            // update fflags
            fflags := exuFflags(commitIdx)
          }
          when(commitInfo.fpWen){
            // set fs to dirty
            dirty_fs := true.B
          }
        }

        XSInfo(io.commits.valid(i),
          "retired pc %x wen %d ldest %d pdest %x old_pdest %x data %x fflags: %b\n",
          commitUop.cf.pc,
          commitInfo.rfWen,
          commitInfo.ldest,
          commitInfo.pdest,
          commitInfo.old_pdest,
          debug_exuData(commitIdx),
          exuFflags(commitIdx).asUInt
        )
      }

      is (s_walk) {
        val idx = walkPtrVec(i).value
        val v = valid(idx)
        val walkUop = microOp(idx)
        io.commits.valid(i) := v && shouldWalkVec(i)
        when (shouldWalkVec(i)) {
          v := false.B
        }
        XSInfo(io.commits.valid(i) && shouldWalkVec(i), "walked pc %x wen %d ldst %d data %x\n",
          walkUop.cf.pc,
          commitInfo.rfWen,
          commitInfo.ldest,
          debug_exuData(idx)
        )
      }

      is (s_extrawalk) {
        val idx = RenameWidth-i-1
        val walkUop = extraSpaceForMPR(idx)
        io.commits.valid(i) := usedSpaceForMPR(idx)
        io.commits.info(i) := walkUop
        state := s_walk
        XSInfo(io.commits.valid(i), "use extra space walked wen %d ldst %d\n",
          // walkUop.cf.pc,
          commitInfo.rfWen,
          commitInfo.ldest
        )
      }
    }
  }

  io.csr.fflags := fflags
  io.csr.dirty_fs := dirty_fs

  val validCommit = io.commits.valid
  val commitCnt = PopCount(validCommit)
  when(state===s_walk) {
    //exit walk state when all roq entry is commited
    when(walkFinished) {
      state := s_idle
    }
    for (i <- 0 until CommitWidth) {
      walkPtrVec(i) := walkPtrVec(i) - CommitWidth.U
    }
    walkCounter := walkCounter - commitCnt
    XSInfo(p"rolling back: $enqPtr $deqPtr walk $walkPtr walkcnt $walkCounter\n")
  }

  // move tail ptr
  when (state === s_idle) {
    deqPtrVec := VecInit(deqPtrVec.map(_ + commitCnt))
  }
  val retireCounter = Mux(state === s_idle, commitCnt, 0.U)
  XSInfo(retireCounter > 0.U, "retired %d insts\n", retireCounter)

  // commit branch to brq
  io.bcommit := PopCount(cfiCommitVec)

  // when redirect, walk back roq entries
  when (io.brqRedirect.valid) {
    state := s_walk
    for (i <- 0 until CommitWidth) {
      walkPtrVec(i) := Mux(state === s_walk,
        walkPtrVec(i) - Mux(walkFinished, walkCounter, CommitWidth.U),
        Mux(state === s_extrawalk, walkPtrVec(i), enqPtr - (i+1).U))
    }
    val currentWalkPtr = Mux(state === s_walk || state === s_extrawalk, walkPtr, enqPtr - 1.U)
    walkCounter := distanceBetween(currentWalkPtr, io.brqRedirect.bits.roqIdx) - Mux(state === s_walk, commitCnt, 0.U)
    enqPtr := io.brqRedirect.bits.roqIdx + 1.U
  }

  // no enough space for walk, allocate extra space
  when (needExtraSpaceForMPR.asUInt.orR && io.brqRedirect.valid) {
    usedSpaceForMPR := needExtraSpaceForMPR
    (0 until RenameWidth).foreach(i => extraSpaceForMPR(i) := commitData.io.wdata(i))
    state := s_extrawalk
    XSDebug("roq full, switched to s_extrawalk. needExtraSpaceForMPR: %b\n", needExtraSpaceForMPR.asUInt)
  }

  // when exception occurs, cancels all
  when (io.redirect.valid) {
    state := s_idle
    enqPtr := 0.U.asTypeOf(new RoqPtr)
    deqPtrVec := VecInit((0 until CommitWidth).map(_.U.asTypeOf(new RoqPtr)))
  }

  /**
    * States
    * We put all the stage changes here.

    * All events: (1) enqueue (dispatch); (2) writeback; (3) cancel; (4) dequeue (commit);
    * All states: (1) valid; (2) writebacked;
    */
  // write
  // enqueue logic writes 6 valid
  for (i <- 0 until RenameWidth) {
    when(io.enq.req(i).valid && io.enq.canAccept && !io.brqRedirect.valid){
      valid(enqPtrVec(i).value) := true.B
    }
  }
  // dequeue/walk logic writes 6 valid, dequeue and walk will not happen at the same time
  for(i <- 0 until CommitWidth){
    switch(state){
      is(s_idle){
        when(io.commits.valid(i)){valid(deqPtrVec(i).value) := false.B}
      }
      is(s_walk){
        val idx = walkPtrVec(i).value
        when(shouldWalkVec(i)){
          valid(idx) := false.B
        }
      }
    }
  }

  // read
  // enqueue logic reads 6 valid
  // dequeue/walk logic reads 6 valid, dequeue and walk will not happen at the same time
  // rollback reads all valid? is it necessary?

  // reset
  // when exception, reset all valid to false
  when (io.redirect.valid) {
    for (i <- 0 until RoqSize) {
      valid(i) := false.B
    }
  }

  // status field: writebacked

  // write
  // enqueue logic set 6 writebacked to false
  for (i <- 0 until RenameWidth) {
    when(io.enq.req(i).valid && io.enq.canAccept && !io.brqRedirect.valid){
      writebacked(enqPtrVec(i).value) := false.B
    }
  }
  // writeback logic set numWbPorts writebacked to true
  for(i <- 0 until numWbPorts) {
    when(io.exeWbResults(i).fire()){
      val wbIdxExt = io.exeWbResults(i).bits.uop.roqIdx
      val wbIdx = wbIdxExt.value
      writebacked(wbIdx) := true.B
    }
  }
  // rollback: write all
  // when rollback, reset writebacked entry to valid
  // when(io.memRedirect.valid) { // TODO: opt timing
  //   for (i <- 0 until RoqSize) {
  //     val recRoqIdx = RoqPtr(flagBkup(i), i.U)
  //     when (valid(i) && isAfter(recRoqIdx, io.memRedirect.bits.roqIdx)) {
  //       writebacked(i) := false.B
  //     }
  //   }
  // }

  // read
  // deqPtrWritebacked
  // gen io.commits(i).valid read 6 (CommitWidth)

  // flagBkup
  // write: update when enqueue
  // enqueue logic set 6 flagBkup at most
  for (i <- 0 until RenameWidth) {
    when(io.enq.req(i).valid && io.enq.canAccept && !io.brqRedirect.valid){
      flagBkup(enqPtrVec(i).value) := enqPtrVec(i).flag
    }
  }
  // read: used in rollback logic
  // all flagBkup will be used

  // exuFflags
  // write: writeback logic set numWbPorts exuFflags
  for(i <- 0 until numWbPorts) {
    when(io.exeWbResults(i).fire()){
      val wbIdxExt = io.exeWbResults(i).bits.uop.roqIdx
      val wbIdx = wbIdxExt.value
      exuFflags(wbIdx) := io.exeWbResults(i).bits.fflags
    }
  }
  // read: used in commit logic
  // read CommitWidth exuFflags

  // debug info
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

  if(!env.FPGAPlatform) {

    //difftest signals
    val firstValidCommit = (deqPtr + PriorityMux(validCommit, VecInit(List.tabulate(CommitWidth)(_.U)))).value

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
      val uop = microOp(idx)
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
      microOp(deqPtr.value).ctrl.fuType === FuType.mou &&
      (microOp(deqPtr.value).ctrl.fuOpType === LSUOpType.sc_d || microOp(deqPtr.value).ctrl.fuOpType === LSUOpType.sc_w)

    val instrCnt = RegInit(0.U(64.W))
    instrCnt := instrCnt + retireCounter

    XSDebug(difftestIntrNO =/= 0.U, "difftest intrNO set %x\n", difftestIntrNO)
    val retireCounterFix = Mux(io.redirect.valid, 1.U, retireCounter)
    val retirePCFix = SignExt(Mux(io.redirect.valid, microOp(deqPtr.value).cf.pc, microOp(firstValidCommit).cf.pc), XLEN)
    val retireInstFix = Mux(io.redirect.valid, microOp(deqPtr.value).cf.instr, microOp(firstValidCommit).cf.instr)

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
