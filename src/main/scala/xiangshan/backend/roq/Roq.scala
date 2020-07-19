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
    // exu + brq
    val exeWbResults = Vec(exuParameters.ExuCnt + 1, Flipped(ValidIO(new ExuOutput)))
    val commits = Vec(CommitWidth, Valid(new RoqCommit))
    val scommit = Output(UInt(3.W))
  })

  val numWbPorts = io.exeWbResults.length

  val microOp = Mem(RoqSize, new MicroOp)
  // val brMask = Reg(Vec(RoqSize, UInt(BrqSize.W)))
  val valid = RegInit(VecInit(List.fill(RoqSize)(false.B)))
  val writebacked = Reg(Vec(RoqSize, Bool()))
  val redirect = Reg(Vec(RoqSize, new Redirect))

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
  XSError(!(hasCsr && state =/= s_idle), "CSR block should only happen in s_idle\n")
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
        val canCommit = if(i!=0) io.commits(i-1).valid else true.B
        io.commits(i).valid := valid(ringBufferTail+i.U) && writebacked(ringBufferTail+i.U) && canCommit
        io.commits(i).bits.uop := microOp(ringBufferTail+i.U)
        when(io.commits(i).valid){valid(ringBufferTail+i.U) := false.B}
        XSInfo(io.commits(i).valid,
          "retired pc %x wen %d ldst %d data %x\n",
          microOp(ringBufferTail+i.U).cf.pc,
          microOp(ringBufferTail+i.U).ctrl.rfWen,
          microOp(ringBufferTail+i.U).ctrl.ldest,
          exuData(ringBufferTail+i.U)
        )
        XSInfo(io.commits(i).valid && exuDebug(ringBufferTail+i.U).isMMIO,
          "difftest skiped pc0x%x\n",
          microOp(ringBufferTail+i.U).cf.pc
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

  // commit store to lsu
  val validScommit = WireInit(VecInit((0 until CommitWidth).map(i => state === s_idle && io.commits(i).valid && microOp(ringBufferTail+i.U).ctrl.fuType === FuType.stu && microOp(ringBufferTail+i.U).ctrl.fuOpType(3)))) //FIXIT
  io.scommit := PopCount(validScommit.asUInt)

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

  // roq redirect only used for exception
  io.redirect := DontCare //TODO
  io.redirect.valid := false.B //TODO

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

  if(!p.FPGAPlatform){
    BoringUtils.addSource(RegNext(retireCounter), "difftestCommit")
    BoringUtils.addSource(RegNext(microOp(firstValidCommit).cf.pc), "difftestThisPC")//first valid PC
    BoringUtils.addSource(RegNext(microOp(firstValidCommit).cf.instr), "difftestThisINST")//first valid inst
    BoringUtils.addSource(RegNext(skip.asUInt), "difftestSkip")
    BoringUtils.addSource(RegNext(false.B), "difftestIsRVC")//FIXIT
    BoringUtils.addSource(RegNext(wen.asUInt), "difftestWen")
    BoringUtils.addSource(RegNext(wpc), "difftestWpc")
    BoringUtils.addSource(RegNext(wdata), "difftestWdata")
    BoringUtils.addSource(RegNext(wdst), "difftestWdst")
    BoringUtils.addSource(RegNext(0.U), "difftestIntrNO")
    //TODO: skip insts that commited in the same cycle ahead of exception

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

    // BPU temp Perf Cnt
    if(EnableBPU){
      BoringUtils.addSource(hitTrap, "XSTRAP_BPU")
    }
  }
}
