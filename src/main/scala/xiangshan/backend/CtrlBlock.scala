package xiangshan.backend

import chisel3._
import chisel3.util._
import utils._
import xiangshan._
import xiangshan.backend.decode.DecodeStage
import xiangshan.backend.rename.{BusyTable, Rename}
import xiangshan.backend.dispatch.Dispatch
import xiangshan.backend.exu._
import xiangshan.backend.exu.Exu.exuConfigs
import xiangshan.backend.ftq.{Ftq, FtqRead, GetPcByFtq}
import xiangshan.backend.regfile.RfReadPort
import xiangshan.backend.roq.{Roq, RoqCSRIO, RoqPtr}
import xiangshan.mem.LsqEnqIO

class CtrlToIntBlockIO extends XSBundle {
  val enqIqCtrl = Vec(exuParameters.IntExuCnt, DecoupledIO(new MicroOp))
  val readRf = Vec(NRIntReadPorts, Flipped(new RfReadPort(XLEN)))
  val jumpPc = Output(UInt(VAddrBits.W))
  val jalr_target = Output(UInt(VAddrBits.W))
  // int block only uses port 0~7
  val readPortIndex = Vec(exuParameters.IntExuCnt, Output(UInt(log2Ceil(8 / 2).W))) // TODO parameterize 8 here
  val redirect = ValidIO(new Redirect)
}

class CtrlToFpBlockIO extends XSBundle {
  val enqIqCtrl = Vec(exuParameters.FpExuCnt, DecoupledIO(new MicroOp))
  val readRf = Vec(NRFpReadPorts, Flipped(new RfReadPort(XLEN + 1)))
  // fp block uses port 0~11
  val readPortIndex = Vec(exuParameters.FpExuCnt, Output(UInt(log2Ceil((NRFpReadPorts - exuParameters.StuCnt) / 3).W)))
  val redirect = ValidIO(new Redirect)
}

class CtrlToLsBlockIO extends XSBundle {
  val enqIqCtrl = Vec(exuParameters.LsExuCnt, DecoupledIO(new MicroOp))
  val enqLsq = Flipped(new LsqEnqIO)
  val redirect = ValidIO(new Redirect)
}

class RedirectGenerator extends XSModule with HasCircularQueuePtrHelper {
  val io = IO(new Bundle() {
    val loadRelay = Flipped(ValidIO(new Redirect))
    val exuMispredict = Vec(exuParameters.JmpCnt + exuParameters.AluCnt, Flipped(ValidIO(new ExuOutput)))
    val roqRedirect = Flipped(ValidIO(new Redirect))
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
    Mux(isAfter(x.bits.roqIdx, y.bits.roqIdx) && y.valid, y, x)
  }
  def selectOlderExuOut(x: Valid[ExuOutput], y: Valid[ExuOutput]): Valid[ExuOutput] = {
    Mux(isAfter(x.bits.redirect.roqIdx, y.bits.redirect.roqIdx) && y.valid, y, x)
  }
  val jumpOut = io.exuMispredict.head
  val oldestAluOut = ParallelOperation(io.exuMispredict.tail, selectOlderExuOut)
  val oldestExuOut = selectOlderExuOut(oldestAluOut, jumpOut) // select between jump and alu

  val oldestMispredict = selectOlderRedirect(io.loadRelay, {
    val redirect = Wire(Valid(new Redirect))
    redirect.valid := oldestExuOut.valid
    redirect.bits := oldestExuOut.bits.redirect
    redirect
  })

  val s1_isJalr = RegEnable(JumpOpType.jumpOpisJalr(jumpOut.bits.uop.ctrl.fuOpType), jumpOut.valid)
  val s1_JalrTarget = RegEnable(jumpOut.bits.redirect.cfiUpdate.target, jumpOut.valid)
  val s1_imm12_reg = RegEnable(oldestExuOut.bits.uop.ctrl.imm(11, 0), oldestExuOut.valid)
  val s1_pd = RegEnable(oldestExuOut.bits.uop.cf.pd, oldestExuOut.valid)
  val s1_redirect_bits_reg = Reg(new Redirect)
  val s1_redirect_valid_reg = RegInit(false.B)

  // stage1 -> stage2
  when(oldestMispredict.valid && !oldestMispredict.bits.roqIdx.needFlush(io.stage2Redirect)){
    s1_redirect_bits_reg := oldestMispredict.bits
    s1_redirect_valid_reg := true.B
  }.otherwise({
    s1_redirect_valid_reg := false.B
  })
  io.stage2Redirect.valid := s1_redirect_valid_reg
  io.stage2Redirect.bits := s1_redirect_bits_reg
  io.stage2Redirect.bits.cfiUpdate := DontCare
  // at stage2, we read ftq to get pc
  io.stage2FtqRead.ptr := s1_redirect_bits_reg.ftqIdx

  // stage3, calculate redirect target
  val s2_isJalr = RegEnable(s1_isJalr, s1_redirect_valid_reg)
  val s2_JalrTarget = RegEnable(s1_JalrTarget, s1_redirect_valid_reg)
  val s2_imm12_reg = RegEnable(s1_imm12_reg, s1_redirect_valid_reg)
  val s2_pd = RegEnable(s1_pd, s1_redirect_valid_reg)
  val s2_redirect_bits_reg = Reg(new Redirect)
  val s2_redirect_valid_reg = RegInit(false.B)

  val ftqRead = io.stage2FtqRead.entry
  val pc = GetPcByFtq(ftqRead.ftqPC, s2_redirect_bits_reg.ftqOffset)
  val brTarget = pc + SignExt(s2_imm12_reg, XLEN)
  val isReplay = RedirectLevel.flushItself(s2_redirect_bits_reg.level)
  val target = Mux(isReplay,
    pc, // repaly from itself
    Mux(s2_isJalr,
      s2_JalrTarget, // jalr already save target
      brTarget // branch
    )
  )
  io.stage3Redirect.valid := s2_redirect_valid_reg
  io.stage3Redirect.bits := s2_redirect_bits_reg
  val stage3CfiUpdate = io.stage3Redirect.bits.cfiUpdate
  stage3CfiUpdate.pc := pc
  stage3CfiUpdate.pd := s2_pd
  stage3CfiUpdate.rasSp := ftqRead.rasSp
  stage3CfiUpdate.rasEntry := ftqRead.rasTop
  stage3CfiUpdate.hist := ftqRead.hist
  stage3CfiUpdate.predHist := ftqRead.predHist
  stage3CfiUpdate.specCnt := ftqRead.specCnt
  stage3CfiUpdate.predTaken := s2_redirect_bits_reg.cfiUpdate.predTaken
  stage3CfiUpdate.sawNotTakenBranch := VecInit((0 until PredictWidth).map{ i =>
    if(i == 0) false.B else Cat(ftqRead.br_mask.take(i-1)).orR()
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
      val exception = ValidIO(new MicroOp)
      val isInterrupt = Output(Bool())
      // to mem block
      val commits = new RoqCommitIO
      val roqDeqPtr = Output(new RoqPtr)
    }
  })

  val ftq = Module(new Ftq)
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

  redirectGen.io.exuMispredict.zip(io.fromIntBlock.exuRedirect).map({case (x, y) =>
    x.valid := y.valid && y.bits.redirect.cfiUpdate.isMisPred
    x.bits := y.bits
  })
  redirectGen.io.loadRelay := io.fromLsBlock.replay
  redirectGen.io.roqRedirect := roq.io.redirectOut

  ftq.io.enq <> io.frontend.fetchInfo
  for(i <- 0 until CommitWidth){
    ftq.io.roq_commits(i).valid := roq.io.commits.valid(i)
    ftq.io.roq_commits(i).bits := roq.io.commits.info(i)
  }
  ftq.io.redirect <> backendRedirect
  ftq.io.frontendRedirect <> frontendRedirect
  ftq.io.exuWriteback <> io.fromIntBlock.exuRedirect

  ftq.io.ftqRead(1) <> redirectGen.io.stage2FtqRead
  ftq.io.ftqRead(2) <> DontCare // TODO: read exception pc form here

  io.frontend.redirect_cfiUpdate := frontendRedirect
  io.frontend.commit_cfiUpdate := ftq.io.commit_ftqEntry

  decode.io.in <> io.frontend.cfVec

  val jumpInst = dispatch.io.enqIQCtrl(0).bits
  ftq.io.ftqRead(0).ptr := jumpInst.cf.ftqPtr // jump
  io.toIntBlock.jumpPc := GetPcByFtq(ftq.io.ftqRead(0).entry.ftqPC, jumpInst.cf.ftqOffset)
  io.toIntBlock.jalr_target := ftq.io.ftqRead(0).entry.jalr_target

  // pipeline between decode and dispatch
  for (i <- 0 until RenameWidth) {
    PipelineConnect(decode.io.out(i), rename.io.in(i), rename.io.in(i).ready,
      backendRedirect.valid || frontendRedirect.valid)
  }

  rename.io.redirect <> backendRedirect
  rename.io.roqCommits <> roq.io.commits
  rename.io.out <> dispatch.io.fromRename
  rename.io.renameBypass <> dispatch.io.renameBypass

  dispatch.io.redirect <> backendRedirect
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


  val flush = backendRedirect.valid && RedirectLevel.isUnconditional(backendRedirect.bits.level)
  fpBusyTable.io.flush := flush
  intBusyTable.io.flush := flush
  for((wb, setPhyRegRdy) <- io.fromIntBlock.wbRegs.zip(intBusyTable.io.wbPregs)){
    setPhyRegRdy.valid := wb.valid && wb.bits.uop.ctrl.rfWen
    setPhyRegRdy.bits := wb.bits.uop.pdest
  }
  for((wb, setPhyRegRdy) <- io.fromFpBlock.wbRegs.zip(fpBusyTable.io.wbPregs)){
    setPhyRegRdy.valid := wb.valid && wb.bits.uop.ctrl.fpWen
    setPhyRegRdy.bits := wb.bits.uop.pdest
  }
  intBusyTable.io.rfReadAddr <> dispatch.io.readIntRf.map(_.addr)
  intBusyTable.io.pregRdy <> dispatch.io.intPregRdy
  fpBusyTable.io.rfReadAddr <> dispatch.io.readFpRf.map(_.addr)
  fpBusyTable.io.pregRdy <> dispatch.io.fpPregRdy

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
  io.toFpBlock.redirect <> backendRedirect
  io.toLsBlock.redirect <> backendRedirect

  dispatch.io.readPortIndex.intIndex <> io.toIntBlock.readPortIndex
  dispatch.io.readPortIndex.fpIndex <> io.toFpBlock.readPortIndex

  // roq to int block
  io.roqio.toCSR <> roq.io.csr
  io.roqio.exception.valid := roq.io.redirectOut.valid && roq.io.redirectOut.bits.isException()
  io.roqio.exception.bits := roq.io.exception
  io.roqio.isInterrupt := roq.io.redirectOut.bits.interrupt
  // roq to mem block
  io.roqio.roqDeqPtr := roq.io.roqDeqPtr
  io.roqio.commits := roq.io.commits
}
