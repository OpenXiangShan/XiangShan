package xiangshan.backend

import chisel3._
import chisel3.util._
import utils._
import xiangshan._
import xiangshan.backend.decode.{DecodeStage, ImmUnion}
import xiangshan.backend.rename.{BusyTable, Rename}
import xiangshan.backend.dispatch.Dispatch
import xiangshan.backend.exu._
import xiangshan.backend.exu.Exu.exuConfigs
import xiangshan.backend.ftq.{Ftq, FtqRead, GetPcByFtq}
import xiangshan.backend.regfile.RfReadPort
import xiangshan.backend.roq.{Roq, RoqCSRIO, RoqLsqIO, RoqPtr}
import xiangshan.mem.LsqEnqIO

class CtrlToIntBlockIO extends XSBundle {
  val enqIqCtrl = Vec(exuParameters.IntExuCnt, DecoupledIO(new MicroOp))
  val readRf = Vec(NRIntReadPorts, Output(UInt(PhyRegIdxWidth.W)))
  val jumpPc = Output(UInt(VAddrBits.W))
  val jalr_target = Output(UInt(VAddrBits.W))
  // int block only uses port 0~7
  val readPortIndex = Vec(exuParameters.IntExuCnt, Output(UInt(log2Ceil(8 / 2).W))) // TODO parameterize 8 here
  val redirect = ValidIO(new Redirect)
  val flush = Output(Bool())
}

class CtrlToFpBlockIO extends XSBundle {
  val enqIqCtrl = Vec(exuParameters.FpExuCnt, DecoupledIO(new MicroOp))
  val readRf = Vec(NRFpReadPorts, Output(UInt(PhyRegIdxWidth.W)))
  // fp block uses port 0~11
  val readPortIndex = Vec(exuParameters.FpExuCnt, Output(UInt(log2Ceil((NRFpReadPorts - exuParameters.StuCnt) / 3).W)))
  val redirect = ValidIO(new Redirect)
  val flush = Output(Bool())
}

class CtrlToLsBlockIO extends XSBundle {
  val enqIqCtrl = Vec(exuParameters.LsExuCnt, DecoupledIO(new MicroOp))
  val enqLsq = Flipped(new LsqEnqIO)
  val redirect = ValidIO(new Redirect)
  val flush = Output(Bool())
}

class RedirectGenerator extends XSModule with HasCircularQueuePtrHelper {
  val io = IO(new Bundle() {
    val loadRelay = Flipped(ValidIO(new Redirect))
    val exuMispredict = Vec(exuParameters.JmpCnt + exuParameters.AluCnt, Flipped(ValidIO(new ExuOutput)))
    val flush = Input(Bool())
    val stage2FtqRead = new FtqRead
    val stage2Redirect = ValidIO(new Redirect)
    val stage3Redirect = ValidIO(new Redirect)
  })
  /*
        LoadQueue  Jump  ALU0  ALU1  ALU2  ALU3   exception    Stage1
          |         |      |    |     |     |         |
          |============= reg & compare =====|         |       ========
                            |                         |
                            |                         |
                            |                         |        Stage2
                            |                         |
                    redirect (flush backend)          |
                    |                                 |
               === reg ===                            |       ========
                    |                                 |
                    |----- mux (exception first) -----|        Stage3
                            |
                redirect (send to frontend)
   */
  def selectOlderRedirect(x: Valid[Redirect], y: Valid[Redirect]): Valid[Redirect] = {
    Mux(x.valid,
      Mux(y.valid,
        Mux(isAfter(x.bits.roqIdx, y.bits.roqIdx), y, x),
        x
      ),
      y
    )
  }
  def selectOlderExuOutWithFlag(x: Valid[ExuOutput], y: Valid[ExuOutput]): (Valid[ExuOutput], Bool) = {
    val yIsOlder = Mux(x.valid,
      Mux(y.valid,
        Mux(isAfter(x.bits.redirect.roqIdx, y.bits.redirect.roqIdx), true.B, false.B),
        false.B
      ),
      true.B
    )
    val sel = Mux(yIsOlder, y, x)
    (sel, yIsOlder)
  }
  def selectOlderExuOut(x: Valid[ExuOutput], y: Valid[ExuOutput]): Valid[ExuOutput] = {
    selectOlderExuOutWithFlag(x, y)._1
  }
  val jumpOut = io.exuMispredict.head
  val oldestAluOut = ParallelOperation(io.exuMispredict.tail, selectOlderExuOut)
  val (oldestExuOut, jumpIsOlder) = selectOlderExuOutWithFlag(oldestAluOut, jumpOut) // select between jump and alu

  val oldestMispredict = selectOlderRedirect(io.loadRelay, {
    val redirect = Wire(Valid(new Redirect))
    redirect.valid := oldestExuOut.valid
    redirect.bits := oldestExuOut.bits.redirect
    redirect
  })

  XSDebug(oldestExuOut.valid, p"exuMispredict: ${Binary(Cat(io.exuMispredict.map(_.valid)))}\n")

  val s1_isJump = RegNext(jumpIsOlder, init = false.B)
  val s1_jumpTarget = RegEnable(jumpOut.bits.redirect.cfiUpdate.target, jumpOut.valid)
  val s1_imm12_reg = RegEnable(oldestExuOut.bits.uop.ctrl.imm(11, 0), oldestExuOut.valid)
  val s1_pd = RegEnable(oldestExuOut.bits.uop.cf.pd, oldestExuOut.valid)
  val s1_redirect_bits_reg = Reg(new Redirect)
  val s1_redirect_valid_reg = RegInit(false.B)

  // stage1 -> stage2
  when(oldestMispredict.valid && !oldestMispredict.bits.roqIdx.needFlush(io.stage2Redirect, io.flush)){
    s1_redirect_bits_reg := oldestMispredict.bits
    s1_redirect_valid_reg := true.B
  }.otherwise({
    s1_redirect_valid_reg := false.B
  })
  io.stage2Redirect.valid := s1_redirect_valid_reg && !io.flush
  io.stage2Redirect.bits := s1_redirect_bits_reg
  io.stage2Redirect.bits.cfiUpdate := DontCare
  // at stage2, we read ftq to get pc
  io.stage2FtqRead.ptr := s1_redirect_bits_reg.ftqIdx

  // stage3, calculate redirect target
  val s2_isJump = RegNext(s1_isJump)
  val s2_jumpTarget = RegEnable(s1_jumpTarget, s1_redirect_valid_reg)
  val s2_imm12_reg = RegEnable(s1_imm12_reg, s1_redirect_valid_reg)
  val s2_pd = RegEnable(s1_pd, s1_redirect_valid_reg)
  val s2_redirect_bits_reg = RegEnable(s1_redirect_bits_reg, enable = s1_redirect_valid_reg)
  val s2_redirect_valid_reg = RegNext(s1_redirect_valid_reg && !io.flush, init = false.B)

  val ftqRead = io.stage2FtqRead.entry
  val cfiUpdate_pc = 
    Cat(ftqRead.ftqPC.head(VAddrBits - s2_redirect_bits_reg.ftqOffset.getWidth - instOffsetBits),
        s2_redirect_bits_reg.ftqOffset,
        0.U(instOffsetBits.W))
  val real_pc = 
    GetPcByFtq(ftqRead.ftqPC, s2_redirect_bits_reg.ftqOffset,
               ftqRead.lastPacketPC.valid,
               ftqRead.lastPacketPC.bits)
  val brTarget = real_pc + SignExt(ImmUnion.B.toImm32(s2_imm12_reg), XLEN)
  val snpc = real_pc + Mux(s2_pd.isRVC, 2.U, 4.U)
  val isReplay = RedirectLevel.flushItself(s2_redirect_bits_reg.level)
  val target = Mux(isReplay,
    real_pc, // repaly from itself
    Mux(s2_redirect_bits_reg.cfiUpdate.taken,
      Mux(s2_isJump, s2_jumpTarget, brTarget),
      snpc
    )
  )
  io.stage3Redirect.valid := s2_redirect_valid_reg
  io.stage3Redirect.bits := s2_redirect_bits_reg
  val stage3CfiUpdate = io.stage3Redirect.bits.cfiUpdate
  stage3CfiUpdate.pc := cfiUpdate_pc
  stage3CfiUpdate.pd := s2_pd
  stage3CfiUpdate.rasSp := ftqRead.rasSp
  stage3CfiUpdate.rasEntry := ftqRead.rasTop
  stage3CfiUpdate.hist := ftqRead.hist
  stage3CfiUpdate.predHist := ftqRead.predHist
  stage3CfiUpdate.specCnt := ftqRead.specCnt
  stage3CfiUpdate.predTaken := s2_redirect_bits_reg.cfiUpdate.predTaken
  stage3CfiUpdate.sawNotTakenBranch := VecInit((0 until PredictWidth).map{ i =>
    if(i == 0) false.B else Cat(ftqRead.br_mask.take(i)).orR()
  })(s2_redirect_bits_reg.ftqOffset)
  stage3CfiUpdate.target := target
  stage3CfiUpdate.taken := s2_redirect_bits_reg.cfiUpdate.taken
  stage3CfiUpdate.isMisPred := s2_redirect_bits_reg.cfiUpdate.isMisPred
}

class CtrlBlock extends XSModule with HasCircularQueuePtrHelper {
  val io = IO(new Bundle {
    val frontend = Flipped(new FrontendToBackendIO)
    val fromIntBlock = Flipped(new IntBlockToCtrlIO)
    val fromFpBlock = Flipped(new FpBlockToCtrlIO)
    val fromLsBlock = Flipped(new LsBlockToCtrlIO)
    val toIntBlock = new CtrlToIntBlockIO
    val toFpBlock = new CtrlToFpBlockIO
    val toLsBlock = new CtrlToLsBlockIO
    val roqio = new Bundle {
      // to int block
      val toCSR = new RoqCSRIO
      val exception = ValidIO(new ExceptionInfo)
      // to mem block
      val lsq = new RoqLsqIO
    }
  })

  val difftestIO = IO(new Bundle() {
    val fromRoq = new Bundle() {
      val commit = Output(UInt(32.W))
      val thisPC = Output(UInt(XLEN.W))
      val thisINST = Output(UInt(32.W))
      val skip = Output(UInt(32.W))
      val wen = Output(UInt(32.W))
      val wdata = Output(Vec(CommitWidth, UInt(XLEN.W))) // set difftest width to 6
      val wdst = Output(Vec(CommitWidth, UInt(32.W))) // set difftest width to 6
      val wpc = Output(Vec(CommitWidth, UInt(XLEN.W))) // set difftest width to 6
      val isRVC = Output(UInt(32.W))
      val scFailed = Output(Bool())
      val lpaddr = Output(Vec(CommitWidth, UInt(64.W)))
      val ltype = Output(Vec(CommitWidth, UInt(32.W)))
      val lfu = Output(Vec(CommitWidth, UInt(4.W)))
    }
  })
  difftestIO <> DontCare

  val ftq = Module(new Ftq)
  val trapIO = IO(new TrapIO())
  trapIO <> DontCare

  val decode = Module(new DecodeStage)
  val rename = Module(new Rename)
  val dispatch = Module(new Dispatch)
  val intBusyTable = Module(new BusyTable(NRIntReadPorts, NRIntWritePorts))
  val fpBusyTable = Module(new BusyTable(NRFpReadPorts, NRFpWritePorts))
  val redirectGen = Module(new RedirectGenerator)

  val roqWbSize = NRIntWritePorts + NRFpWritePorts + exuParameters.StuCnt

  val roq = Module(new Roq(roqWbSize))

  val backendRedirect = redirectGen.io.stage2Redirect
  val frontendRedirect = redirectGen.io.stage3Redirect
  val flush = roq.io.flushOut.valid
  val flushReg = RegNext(flush)

  redirectGen.io.exuMispredict.zip(io.fromIntBlock.exuRedirect).map({case (x, y) =>
    val misPred = y.valid && y.bits.redirect.cfiUpdate.isMisPred
    val killedByOlder = y.bits.uop.roqIdx.needFlush(backendRedirect, flush)
    x.valid := RegNext(misPred && !killedByOlder, init = false.B)
    x.bits := RegEnable(y.bits, y.valid)
  })
  redirectGen.io.loadRelay := io.fromLsBlock.replay
  redirectGen.io.flush := flushReg

  ftq.io.enq <> io.frontend.fetchInfo
  for(i <- 0 until CommitWidth){
    ftq.io.roq_commits(i).valid := roq.io.commits.valid(i) && !roq.io.commits.isWalk
    ftq.io.roq_commits(i).bits := roq.io.commits.info(i)
  }
  ftq.io.redirect <> backendRedirect
  ftq.io.flush := flushReg
  ftq.io.flushIdx := RegNext(roq.io.flushOut.bits.ftqIdx)
  ftq.io.flushOffset := RegNext(roq.io.flushOut.bits.ftqOffset)
  ftq.io.frontendRedirect <> frontendRedirect
  ftq.io.exuWriteback <> io.fromIntBlock.exuRedirect

  ftq.io.ftqRead(1) <> redirectGen.io.stage2FtqRead
  ftq.io.ftqRead(2).ptr := roq.io.flushOut.bits.ftqIdx
  val flushPC = GetPcByFtq(
    ftq.io.ftqRead(2).entry.ftqPC,
    RegEnable(roq.io.flushOut.bits.ftqOffset, roq.io.flushOut.valid),
    ftq.io.ftqRead(2).entry.lastPacketPC.valid,
    ftq.io.ftqRead(2).entry.lastPacketPC.bits
  )

  val flushRedirect = Wire(Valid(new Redirect))
  flushRedirect.valid := flushReg
  flushRedirect.bits := DontCare
  flushRedirect.bits.ftqIdx := RegEnable(roq.io.flushOut.bits.ftqIdx, flush)
  flushRedirect.bits.interrupt := true.B
  flushRedirect.bits.cfiUpdate.target := Mux(io.roqio.toCSR.isXRet || roq.io.exception.valid,
    io.roqio.toCSR.trapTarget,
    flushPC + 4.U // flush pipe
  )

  io.frontend.redirect_cfiUpdate := Mux(flushRedirect.valid, flushRedirect, frontendRedirect)
  io.frontend.commit_cfiUpdate := ftq.io.commit_ftqEntry
  io.frontend.ftqEnqPtr := ftq.io.enqPtr
  io.frontend.ftqLeftOne := ftq.io.leftOne

  decode.io.in <> io.frontend.cfVec

  val jumpInst = dispatch.io.enqIQCtrl(0).bits
  val ftqOffsetReg = Reg(UInt(log2Up(PredictWidth).W))
  ftqOffsetReg := jumpInst.cf.ftqOffset
  ftq.io.ftqRead(0).ptr := jumpInst.cf.ftqPtr // jump
  io.toIntBlock.jumpPc := GetPcByFtq(
    ftq.io.ftqRead(0).entry.ftqPC, ftqOffsetReg,
    ftq.io.ftqRead(0).entry.lastPacketPC.valid,
    ftq.io.ftqRead(0).entry.lastPacketPC.bits
  )
  io.toIntBlock.jalr_target := ftq.io.ftqRead(0).entry.target

  // pipeline between decode and dispatch
  for (i <- 0 until RenameWidth) {
    PipelineConnect(decode.io.out(i), rename.io.in(i), rename.io.in(i).ready,
      io.frontend.redirect_cfiUpdate.valid)
  }

  rename.io.redirect <> backendRedirect
  rename.io.flush := flushReg
  rename.io.roqCommits <> roq.io.commits
  rename.io.out <> dispatch.io.fromRename
  rename.io.renameBypass <> dispatch.io.renameBypass
  rename.io.dispatchInfo <> dispatch.io.preDpInfo

  dispatch.io.redirect <> backendRedirect
  dispatch.io.flush := flushReg
  dispatch.io.enqRoq <> roq.io.enq
  dispatch.io.enqLsq <> io.toLsBlock.enqLsq
  dispatch.io.readIntRf <> io.toIntBlock.readRf
  dispatch.io.readFpRf <> io.toFpBlock.readRf
  dispatch.io.allocPregs.zipWithIndex.foreach { case (preg, i) =>
    intBusyTable.io.allocPregs(i).valid := preg.isInt
    fpBusyTable.io.allocPregs(i).valid := preg.isFp
    intBusyTable.io.allocPregs(i).bits := preg.preg
    fpBusyTable.io.allocPregs(i).bits := preg.preg
  }
  dispatch.io.numExist <> io.fromIntBlock.numExist ++ io.fromFpBlock.numExist ++ io.fromLsBlock.numExist
  dispatch.io.enqIQCtrl <> io.toIntBlock.enqIqCtrl ++ io.toFpBlock.enqIqCtrl ++ io.toLsBlock.enqIqCtrl
//  dispatch.io.enqIQData <> io.toIntBlock.enqIqData ++ io.toFpBlock.enqIqData ++ io.toLsBlock.enqIqData


  fpBusyTable.io.flush := flushReg
  intBusyTable.io.flush := flushReg
  for((wb, setPhyRegRdy) <- io.fromIntBlock.wbRegs.zip(intBusyTable.io.wbPregs)){
    setPhyRegRdy.valid := wb.valid && wb.bits.uop.ctrl.rfWen
    setPhyRegRdy.bits := wb.bits.uop.pdest
  }
  for((wb, setPhyRegRdy) <- io.fromFpBlock.wbRegs.zip(fpBusyTable.io.wbPregs)){
    setPhyRegRdy.valid := wb.valid && wb.bits.uop.ctrl.fpWen
    setPhyRegRdy.bits := wb.bits.uop.pdest
  }
  intBusyTable.io.read <> dispatch.io.readIntState
  fpBusyTable.io.read <> dispatch.io.readFpState

  roq.io.redirect <> backendRedirect
  roq.io.exeWbResults.zip(
    io.fromIntBlock.wbRegs ++ io.fromFpBlock.wbRegs ++ io.fromLsBlock.stOut
  ).foreach{
    case(x, y) =>
      x.bits := y.bits
      x.valid := y.valid
  }

  // TODO: is 'backendRedirect' necesscary?
  io.toIntBlock.redirect <> backendRedirect
  io.toIntBlock.flush <> flushReg
  io.toFpBlock.redirect <> backendRedirect
  io.toFpBlock.flush <> flushReg
  io.toLsBlock.redirect <> backendRedirect
  io.toLsBlock.flush <> flushReg

  if (!env.FPGAPlatform) {
    difftestIO.fromRoq <> roq.difftestIO
    trapIO <> roq.trapIO
  }

  dispatch.io.readPortIndex.intIndex <> io.toIntBlock.readPortIndex
  dispatch.io.readPortIndex.fpIndex <> io.toFpBlock.readPortIndex

  // roq to int block
  io.roqio.toCSR <> roq.io.csr
  io.roqio.exception := roq.io.exception
  io.roqio.exception.bits.uop.cf.pc := flushPC
  // roq to mem block
  io.roqio.lsq <> roq.io.lsq
}
