package xiangshan.backend.roq

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.utils._
import chisel3.util.experimental.BoringUtils

// A "just-enough" Roq
class Roq(implicit val p: XSConfig) extends XSModule {
  val io = IO(new Bundle() {
    val brqRedirect = Input(Valid(new Redirect))
    val dp1Req = Vec(RenameWidth, Flipped(DecoupledIO(new MicroOp)))
    val roqIdxs = Output(Vec(RenameWidth, UInt(ExtendedRoqIdxWidth.W)))
    val redirect = Output(Valid(new Redirect))
    val exeWbResults = Vec(exuConfig.ExuCnt, Flipped(ValidIO(new ExuOutput)))
    val commits = Vec(CommitWidth, Valid(new RoqCommit))
    val scommit = Output(UInt(3.W))
  })

  val microOp = Mem(RoqSize, new MicroOp)
  // val brMask = Reg(Vec(RoqSize, UInt(BrqSize.W)))
  val valid = RegInit(VecInit(List.fill(RoqSize)(false.B)))
  val writebacked = Reg(Vec(RoqSize, Bool()))
  val redirect = Reg(Vec(RoqSize, new Redirect))

  val exuData = Reg(Vec(RoqSize, UInt(XLEN.W)))//for debug
  val exuDebug = Reg(Vec(RoqSize, new DebugBundle))//for debug
  val archRF = RegInit(VecInit(List.fill(64)(0.U(32.W))))//for debug, fp regs included

  val ringBufferHeadExtended = RegInit(0.U(ExtendedRoqIdxWidth.W))
  val ringBufferTailExtended = RegInit(0.U(ExtendedRoqIdxWidth.W))
  val ringBufferWalkExtended = Reg(UInt(ExtendedRoqIdxWidth.W))
  val ringBufferWalkTarget = Reg(UInt(ExtendedRoqIdxWidth.W))
  val ringBufferHead = ringBufferHeadExtended(RoqIdxWidth-1,0)
  val ringBufferTail = ringBufferTailExtended(RoqIdxWidth-1,0)
  val ringBufferWalk = ringBufferWalkExtended(RoqIdxWidth-1,0)
  val ringBufferEmpty = ringBufferHead === ringBufferTail && ringBufferHeadExtended(RoqIdxWidth)===ringBufferTailExtended(RoqIdxWidth)
  val ringBufferFull = ringBufferHead === ringBufferTail && ringBufferHeadExtended(RoqIdxWidth)=/=ringBufferTailExtended(RoqIdxWidth)
  val ringBufferAllowin = !ringBufferFull 

  val s_idle :: s_walk :: Nil = Enum(2)
  val state = RegInit(s_idle)

  // Dispatch
  val validDispatch = VecInit((0 until RenameWidth).map(io.dp1Req(_).valid)).asUInt
  XSDebug("(ready, valid): ")
  for(i <- 0 until RenameWidth){
    val offset = if(i==0) 0.U else PopCount(validDispatch(i-1,0))
    when(io.dp1Req(i).fire()){
      microOp(ringBufferHead+offset) := io.dp1Req(i).bits
      valid(ringBufferHead+offset) := true.B
      writebacked(ringBufferHead+offset) := false.B
    }
    io.dp1Req(i).ready := ringBufferAllowin && !valid(ringBufferHead+offset) && state === s_idle
    io.roqIdxs(i) := ringBufferHeadExtended+offset
    XSDebug(){printf("(%d, %d) ", io.dp1Req(i).ready, io.dp1Req(i).valid)}
  }
  XSDebug(){printf("\n")}

  val firedDispatch = VecInit((0 until CommitWidth).map(io.dp1Req(_).fire())).asUInt
  when(firedDispatch.orR){
    ringBufferHeadExtended := ringBufferHeadExtended + PopCount(firedDispatch)
    XSInfo("dispatched %d insts\n", PopCount(firedDispatch))
  }

  // Writeback
  for(i <- 0 until exuConfig.ExuCnt){
    when(io.exeWbResults(i).fire()){
      writebacked(io.exeWbResults(i).bits.uop.roqIdx) := true.B
      exuData(io.exeWbResults(i).bits.uop.roqIdx) := io.exeWbResults(i).bits.data
      exuDebug(io.exeWbResults(i).bits.uop.roqIdx) := io.exeWbResults(i).bits.debug
    }
  }
  val firedWriteback = VecInit((0 until exuConfig.ExuCnt).map(io.exeWbResults(_).fire())).asUInt
  when(PopCount(firedWriteback) > 0.U){
    XSInfo("writebacked %d insts\n", PopCount(firedWriteback))
  }
  for(i <- 0 until exuConfig.ExuCnt){
    XSInfo("writebacked pc %x wen %d data %d\n", microOp(io.exeWbResults(i).bits.uop.roqIdx).cf.pc, microOp(io.exeWbResults(i).bits.uop.roqIdx).ctrl.rfWen, io.exeWbResults(i).bits.data)
  }

  // Commit uop to Rename
  for(i <- 0 until CommitWidth){
    when(state === s_idle){
      val canCommit = if(i!=0) io.commits(i-1).valid else true.B
      io.commits(i).valid := valid(ringBufferTail+i.U) && writebacked(ringBufferTail+i.U) && canCommit
      io.commits(i).bits.uop := microOp(ringBufferTail+i.U)
      when(microOp(ringBufferTail+i.U).ctrl.rfWen){ archRF(microOp(ringBufferTail+i.U).ctrl.ldest) := exuData(ringBufferTail+i.U) }
      when(io.commits(i).valid){valid(ringBufferTail+i.U) := false.B}
    }.otherwise{//state === s_walk
      io.commits(i).valid := valid(ringBufferWalk+i.U) && writebacked(ringBufferWalk+i.U)
      io.commits(i).bits.uop := microOp(ringBufferWalk+i.U)
      valid(ringBufferWalk+i.U) := false.B
    }
    io.commits(i).bits.isWalk := state === s_walk
  }

  val validCommit = VecInit((0 until CommitWidth).map(i => io.commits(i).valid)).asUInt
  when(state === s_idle){
    ringBufferTailExtended := ringBufferTailExtended + PopCount(validCommit)
  }
  val retireCounter = Mux(state === s_idle, PopCount(validCommit), 0.U)

  // commit store
  val validScommit = WireInit(VecInit((0 until CommitWidth).map(i => io.commits(i).valid && microOp(ringBufferTail+i.U).ctrl.fuType === FuType.ldu && microOp(ringBufferTail+i.U).ctrl.fuOpType(3)))) //FIXIT
  io.scommit := PopCount(validScommit.asUInt)

  XSInfo(retireCounter > 0.U, "retired %d insts\n", retireCounter)
  XSInfo("")
  XSInfo(){
    printf("retired pcs are: ")
    for(i <- 0 until CommitWidth){
      when(io.commits(i).valid){ printf("%d:0x%x ", ringBufferTail+i.U, microOp(ringBufferTail+i.U).cf.pc) }
    }
    printf("\n")
  }

  val walkFinished = (0 until CommitWidth).map(i => (ringBufferWalk + i.U) === ringBufferWalkTarget).reduce(_||_)

  when(state===s_walk){
    //exit walk state when all roq entry is commited
    when(walkFinished){
      state := s_idle
    }
    ringBufferWalkExtended := ringBufferWalkExtended + CommitWidth.U
    XSInfo("rolling back: head %d tail %d walk %d\n", ringBufferHead, ringBufferTail, ringBufferWalk)
  }

  when(io.brqRedirect.valid){
    state := s_walk
    ringBufferWalkExtended := io.brqRedirect.bits.roqIdx
    ringBufferWalkTarget := ringBufferHeadExtended
    ringBufferHeadExtended := io.brqRedirect.bits.roqIdx
  }

  // roq redirect only used for exception
  io.redirect := DontCare //TODO
  io.redirect.valid := false.B //TODO

  // debug info
  XSDebug("head %d tail %d\n", ringBufferHead, ringBufferTail)
  XSDebug("")
  XSDebug(){
    for(i <- 0 until RoqSize){
      when(!valid(i)){printf("-")}
      when(valid(i) && writebacked(i)){printf("w")}
      when(valid(i) && !writebacked(i)){printf("v")}
    }
    printf("\n")
  }
  
  XSDebug(){
    for(i <- 0 until RoqSize){
      if(i % 4 == 0) XSDebug("")
      printf("%x ", microOp(i).cf.pc)
      when(!valid(i)){printf("- ")}
      when(valid(i) && writebacked(i)){printf("w ")}
      when(valid(i) && !writebacked(i)){printf("v ")}
      if(i % 4 == 3) printf("\n")
    }
  }

  //difftest signals
  val firstValidCommit = ringBufferTail + PriorityMux(validCommit, VecInit(List.tabulate(CommitWidth)(_.U)))
  val emptyCsr = WireInit(0.U(64.W))

  if(!p.FPGAPlatform){
    BoringUtils.addSource(RegNext(retireCounter), "difftestCommit")
    BoringUtils.addSource(RegNext(microOp(firstValidCommit).cf.pc), "difftestThisPC")//first valid PC
    BoringUtils.addSource(RegNext(microOp(firstValidCommit).cf.instr), "difftestThisINST")//first valid inst
    BoringUtils.addSource(archRF, "difftestRegs")//arch RegFile
    BoringUtils.addSource(RegNext(false.B), "difftestSkip")//SKIP
    BoringUtils.addSource(RegNext(false.B), "difftestIsRVC")//FIXIT
    BoringUtils.addSource(RegNext(0.U), "difftestIntrNO")
    //TODO: skip insts that commited in the same cycle ahead of exception

    //csr debug signals
    val ModeM = WireInit(0x3.U)
    BoringUtils.addSource(ModeM, "difftestMode")
    BoringUtils.addSource(emptyCsr, "difftestMstatus")
    BoringUtils.addSource(emptyCsr, "difftestSstatus") 
    BoringUtils.addSource(emptyCsr, "difftestMepc")
    BoringUtils.addSource(emptyCsr, "difftestSepc")
    BoringUtils.addSource(emptyCsr, "difftestMcause")
    BoringUtils.addSource(emptyCsr, "difftestScause")
  }
}
