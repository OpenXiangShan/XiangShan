package xiangshan.backend.roq

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import chisel3.util.experimental.BoringUtils
import xiangshan.backend.decode.XSTrap

// A "just-enough" Roq
class Roq(implicit val p: XSConfig) extends XSModule {
  val io = IO(new Bundle() {
    val brqRedirect = Input(Valid(new Redirect))
    val dp1Req = Vec(RenameWidth, Flipped(DecoupledIO(new MicroOp)))
    val roqIdxs = Output(Vec(RenameWidth, UInt(RoqIdxWidth.W)))
    val redirect = Output(Valid(new Redirect))
    val exception = Output(new MicroOp)
    // exu + brq
    val exeWbResults = Vec(exuParameters.ExuCnt + 1, Flipped(ValidIO(new ExuOutput)))
    val commits = Vec(CommitWidth, Valid(new RoqCommit))
    val mcommit = Output(UInt(3.W))
    val bcommit = Output(UInt(BrTagWidth.W))
  })

  val numWbPorts = io.exeWbResults.length

  val microOp = Mem(RoqSize, new MicroOp)
  val valid = RegInit(VecInit(List.fill(RoqSize)(false.B)))
  val writebacked = Reg(Vec(RoqSize, Bool()))

  val exuData = Reg(Vec(RoqSize, UInt(XLEN.W)))//for debug
  val exuDebug = Reg(Vec(RoqSize, new DebugBundle))//for debug

  val ringBufferHeadExtended = RegInit(0.U(RoqIdxWidth.W))
  val ringBufferTailExtended = RegInit(0.U(RoqIdxWidth.W))
  val ringBufferWalkExtended = Reg(UInt(RoqIdxWidth.W))
  val ringBufferWalkTarget = Reg(UInt(RoqIdxWidth.W))
  val ringBufferHead = ringBufferHeadExtended(InnerRoqIdxWidth-1,0)
  val ringBufferTail = ringBufferTailExtended(InnerRoqIdxWidth-1,0)
  val ringBufferWalk = ringBufferWalkExtended(InnerRoqIdxWidth-1,0)
  val ringBufferEmpty = ringBufferHead === ringBufferTail && ringBufferHeadExtended(InnerRoqIdxWidth)===ringBufferTailExtended(InnerRoqIdxWidth)
  val ringBufferFull = ringBufferHead === ringBufferTail && ringBufferHeadExtended(InnerRoqIdxWidth)=/=ringBufferTailExtended(InnerRoqIdxWidth)
  val ringBufferAllowin = !ringBufferFull 

  val s_idle :: s_walk :: s_extrawalk :: Nil = Enum(3)
  val state = RegInit(s_idle)

  // Dispatch
  val csrEnRoq = io.dp1Req.map(i => i.bits.ctrl.fuType === FuType.csr)
  val hasCsr = RegInit(false.B)
  when(ringBufferEmpty){ hasCsr:= false.B }
  val validDispatch = VecInit((0 until RenameWidth).map(io.dp1Req(_).valid)).asUInt
  XSDebug("(ready, valid): ")
  for (i <- 0 until RenameWidth) {
    val offset = if(i==0) 0.U else PopCount(validDispatch(i-1,0))
    when(io.dp1Req(i).fire()){
      microOp(ringBufferHead+offset) := io.dp1Req(i).bits
      valid(ringBufferHead+offset) := true.B
      writebacked(ringBufferHead+offset) := false.B
      when(csrEnRoq(i)){ hasCsr := true.B }
    }
    io.dp1Req(i).ready := (ringBufferAllowin && !valid(ringBufferHead+offset) && state === s_idle) &&
      (!csrEnRoq(i) || ringBufferEmpty) &&
      !hasCsr
    io.roqIdxs(i) := ringBufferHeadExtended+offset
    XSDebug(false, true.B, "(%d, %d) ", io.dp1Req(i).ready, io.dp1Req(i).valid)
  }
  XSDebug(false, true.B, "\n")

  val firedDispatch = VecInit((0 until CommitWidth).map(io.dp1Req(_).fire())).asUInt
  when(firedDispatch.orR){
    ringBufferHeadExtended := ringBufferHeadExtended + PopCount(firedDispatch)
    XSInfo("dispatched %d insts\n", PopCount(firedDispatch))
  }

  // Writeback
  val firedWriteback = VecInit((0 until numWbPorts).map(io.exeWbResults(_).fire())).asUInt
  XSInfo(PopCount(firedWriteback) > 0.U, "writebacked %d insts\n", PopCount(firedWriteback))
  for(i <- 0 until numWbPorts){
    when(io.exeWbResults(i).fire()){
      writebacked(io.exeWbResults(i).bits.uop.roqIdx) := true.B
      microOp(io.exeWbResults(i).bits.uop.roqIdx).cf.exceptionVec := io.exeWbResults(i).bits.uop.cf.exceptionVec
      exuData(io.exeWbResults(i).bits.uop.roqIdx) := io.exeWbResults(i).bits.data
      exuDebug(io.exeWbResults(i).bits.uop.roqIdx) := io.exeWbResults(i).bits.debug
      XSInfo(io.exeWbResults(i).valid, "writebacked pc 0x%x wen %d data 0x%x ldst %d pdst %d skip %x\n", 
        microOp(io.exeWbResults(i).bits.uop.roqIdx).cf.pc,
        microOp(io.exeWbResults(i).bits.uop.roqIdx).ctrl.rfWen,
        io.exeWbResults(i).bits.data,
        microOp(io.exeWbResults(i).bits.uop.roqIdx).ctrl.ldest, 
        io.exeWbResults(i).bits.uop.pdest,
        io.exeWbResults(i).bits.debug.isMMIO
      )
    }
  }

  // roq redirect only used for exception
  val intrBitSet = WireInit(false.B)
  ExcitingUtils.addSink(intrBitSet, "intrBitSetIDU")
  val trapTarget = WireInit(0.U(VAddrBits.W))
  ExcitingUtils.addSink(trapTarget, "trapTarget")
  val intrEnable = intrBitSet && (state === s_idle) && !ringBufferEmpty && !hasCsr
  val exceptionEnable = Cat(microOp(ringBufferTail).cf.exceptionVec).orR() && (state === s_idle) && !ringBufferEmpty
  val isEcall = microOp(ringBufferTail).cf.exceptionVec(ecallM) || microOp(ringBufferTail).cf.exceptionVec(ecallS) || microOp(ringBufferTail).cf.exceptionVec(ecallU)
  io.redirect := DontCare
  io.redirect.valid := intrEnable || exceptionEnable
  io.redirect.bits.isException := true.B
  io.redirect.bits.target := trapTarget
  io.exception := microOp(ringBufferTail)
  XSDebug(io.redirect.valid, "generate exception: pc 0x%x target 0x%x exceptionVec %b\n", io.exception.cf.pc, trapTarget, Cat(microOp(ringBufferTail).cf.exceptionVec))

  // Commit uop to Rename
  val shouldWalkVec = Wire(Vec(CommitWidth, Bool()))
  shouldWalkVec(0) := ringBufferWalkExtended =/= ringBufferWalkTarget
  (1 until CommitWidth).map(i => shouldWalkVec(i) := (ringBufferWalkExtended - i.U) =/= ringBufferWalkTarget && shouldWalkVec(i - 1))
  val walkFinished = (0 until CommitWidth).map(i => (ringBufferWalkExtended - i.U) === ringBufferWalkTarget).reduce(_||_) //FIXIT!!!!!!

  // extra space is used weh roq has no enough space, but mispredict recovery needs such info to walk regmap
  val needExtraSpaceForMPR = WireInit(VecInit(List.tabulate(RenameWidth)(i => io.brqRedirect.valid && io.dp1Req(i).valid && !io.dp1Req(i).ready)))
  val extraSpaceForMPR = Reg(Vec(RenameWidth, new MicroOp))
  val usedSpaceForMPR = Reg(Vec(RenameWidth, Bool()))

  for(i <- 0 until CommitWidth){
    io.commits(i) := DontCare
    switch(state){
      is(s_idle){
        val ringBufferIndex = ringBufferTail + i.U
        val hasException = Cat(microOp(ringBufferIndex).cf.exceptionVec).orR() || intrEnable
        val canCommit = if(i!=0) io.commits(i-1).valid else true.B
        io.commits(i).valid := valid(ringBufferIndex) && writebacked(ringBufferIndex) && canCommit && !hasException
        io.commits(i).bits.uop := microOp(ringBufferIndex)
        when(io.commits(i).valid){valid(ringBufferIndex) := false.B}
        XSInfo(io.commits(i).valid,
          "retired pc %x wen %d ldst %d data %x\n",
          microOp(ringBufferIndex).cf.pc,
          microOp(ringBufferIndex).ctrl.rfWen,
          microOp(ringBufferIndex).ctrl.ldest,
          exuData(ringBufferIndex)
        )
        XSInfo(io.commits(i).valid && exuDebug(ringBufferIndex).isMMIO,
          "difftest skiped pc0x%x\n",
          microOp(ringBufferIndex).cf.pc
        )
      }

      is(s_walk){
        io.commits(i).valid := valid(ringBufferWalk-i.U) && shouldWalkVec(i)
        io.commits(i).bits.uop := microOp(ringBufferWalk-i.U)
        when(shouldWalkVec(i)){
          valid(ringBufferWalk-i.U) := false.B
        }
        XSInfo(io.commits(i).valid && shouldWalkVec(i), "walked pc %x wen %d ldst %d data %x\n", 
          microOp(ringBufferWalk-i.U).cf.pc, 
          microOp(ringBufferWalk-i.U).ctrl.rfWen, 
          microOp(ringBufferWalk-i.U).ctrl.ldest, 
          exuData(ringBufferWalk-i.U)
        )
      }

      is(s_extrawalk){
        io.commits(i).valid := usedSpaceForMPR(RenameWidth-i-1)
        io.commits(i).bits.uop := extraSpaceForMPR(RenameWidth-i-1)
        state := s_walk
        XSInfo(io.commits(i).valid, "use extra space walked pc %x wen %d ldst %d\n", 
          extraSpaceForMPR((RenameWidth-i-1).U).cf.pc, 
          extraSpaceForMPR((RenameWidth-i-1).U).ctrl.rfWen, 
          extraSpaceForMPR((RenameWidth-i-1).U).ctrl.ldest
        )
      }
    }
    io.commits(i).bits.isWalk := state =/= s_idle
  }

  val validCommit = VecInit((0 until CommitWidth).map(i => io.commits(i).valid)).asUInt
  when(state===s_walk) {
    //exit walk state when all roq entry is commited
    when(walkFinished) {
      state := s_idle
    }
    ringBufferWalkExtended := ringBufferWalkExtended - CommitWidth.U
    // ringBufferWalkExtended := ringBufferWalkExtended - validCommit
    XSInfo("rolling back: head %d tail %d walk %d:%d\n", ringBufferHead, ringBufferTail, ringBufferWalkExtended(InnerRoqIdxWidth), ringBufferWalk)
  }

  // move tail ptr
  when(state === s_idle){
    ringBufferTailExtended := ringBufferTailExtended + PopCount(validCommit)
  }
  val retireCounter = Mux(state === s_idle, PopCount(validCommit), 0.U)
  XSInfo(retireCounter > 0.U, "retired %d insts\n", retireCounter)

  // commit load & store to lsu
  val validMcommit = WireInit(VecInit((0 until CommitWidth).map(i => 
    state === s_idle && io.commits(i).valid && 
    microOp(ringBufferTail+i.U).ctrl.fuType === FuType.stu 
    // microOp(ringBufferTail+i.U).ctrl.fuOpType(3)
  ))) //FIXIT
  // val validMcommit = WireInit(VecInit((0 until CommitWidth).map(i => 
    // state === s_idle && io.commits(i).valid && 
    // microOp(ringBufferTail+i.U).ctrl.fuType === FuType.stu
  // )))
  io.mcommit := PopCount(validMcommit.asUInt)

  // TODO MMIO

  val validBcommit = WireInit(VecInit(
    (0 until CommitWidth).map(
      i => state === s_idle &&
        io.commits(i).valid &&
        microOp(ringBufferTail+i.U).cf.brUpdate.isBr
    )
  ))
  io.bcommit := PopCount(validBcommit)

  // when redirect, walk back roq entries
  when(io.brqRedirect.valid){
    state := s_walk
    ringBufferWalkExtended := ringBufferHeadExtended - 1.U + PopCount(firedDispatch)
    ringBufferWalkTarget := io.brqRedirect.bits.roqIdx
    ringBufferHeadExtended := io.brqRedirect.bits.roqIdx + 1.U
  }

  // no enough space for walk, allocate extra space
  when(needExtraSpaceForMPR.asUInt.orR && io.brqRedirect.valid){
    usedSpaceForMPR := needExtraSpaceForMPR
    (0 until RenameWidth).map(i => extraSpaceForMPR(i) := io.dp1Req(i).bits)
    state := s_extrawalk
    XSDebug("roq full, switched to s_extrawalk. needExtraSpaceForMPR: %b\n", needExtraSpaceForMPR.asUInt)
  }

  // when exception occurs, cancels all
  when (io.redirect.valid) {
    ringBufferHeadExtended := 0.U
    ringBufferTailExtended := 0.U
    for (i <- 0 until RoqSize) {
      valid(i) := false.B
    }
  }

  // debug info
  XSDebug("head %d:%d tail %d:%d\n", ringBufferHeadExtended(InnerRoqIdxWidth), ringBufferHead, ringBufferTailExtended(InnerRoqIdxWidth), ringBufferTail)
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
  val firstValidCommit = ringBufferTail + PriorityMux(validCommit, VecInit(List.tabulate(CommitWidth)(_.U)))
  val emptyCsr = WireInit(0.U(64.W))

  val skip = Wire(Vec(CommitWidth, Bool()))
  val wen = Wire(Vec(CommitWidth, Bool()))
  val wdata = Wire(Vec(CommitWidth, UInt(XLEN.W)))
  val wdst = Wire(Vec(CommitWidth, UInt(32.W)))
  val wpc = Wire(Vec(CommitWidth, UInt(VAddrBits.W)))
  val trapVec = Wire(Vec(CommitWidth, Bool()))
  for(i <- 0 until CommitWidth){
    // io.commits(i).valid
    val idx = ringBufferTail+i.U
    val uop = microOp(idx)
    skip(i) := exuDebug(idx).isMMIO && io.commits(i).valid
    wen(i) := io.commits(i).valid && uop.ctrl.rfWen && uop.ctrl.ldest =/= 0.U
    wdata(i) := exuData(idx)
    wdst(i) := uop.ctrl.ldest
    wpc(i) := uop.cf.pc
    trapVec(i) := io.commits(i).valid && (state===s_idle) && uop.ctrl.isXSTrap
  }
  val instrCnt = RegInit(0.U(64.W))
  instrCnt := instrCnt + retireCounter
  val hitTrap = trapVec.reduce(_||_)
  val trapCode = PriorityMux(wdata.zip(trapVec).map(x => x._2 -> x._1))
  val trapPC = PriorityMux(wpc.zip(trapVec).map(x => x._2 ->x._1))

  val difftestIntrNO = WireInit(0.U(XLEN.W))
  ExcitingUtils.addSink(difftestIntrNO, "difftestIntrNOfromCSR")
  XSDebug(difftestIntrNO =/= 0.U, "difftest intrNO set %x\n", difftestIntrNO)
  val retireCounterFix = Mux(io.redirect.valid, 1.U, retireCounter)
  val retirePCFix = Mux(io.redirect.valid, microOp(ringBufferTail).cf.pc, microOp(firstValidCommit).cf.pc)
  val retireInstFix = Mux(io.redirect.valid, microOp(ringBufferTail).cf.instr, microOp(firstValidCommit).cf.instr)
  if(!p.FPGAPlatform){
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

    class Monitor extends BlackBox {
      val io = IO(new Bundle {
        val clk = Input(Clock())
        val reset = Input(Reset())
        val isNoopTrap = Input(Bool())
        val trapCode = Input(UInt(32.W))
        val trapPC = Input(UInt(64.W))
        val cycleCnt = Input(UInt(64.W))
        val instrCnt = Input(UInt(64.W))
      })
    }

    val debugMonitor =  Module(new Monitor)
    debugMonitor.io.clk := this.clock
    debugMonitor.io.reset := this.reset
    debugMonitor.io.isNoopTrap := hitTrap
    debugMonitor.io.trapCode := trapCode
    debugMonitor.io.trapPC := trapPC
    debugMonitor.io.cycleCnt := GTimer()
    debugMonitor.io.instrCnt := instrCnt

    if(EnableBPU){
      BoringUtils.addSource(hitTrap, "XSTRAP")
    }
  }
}
