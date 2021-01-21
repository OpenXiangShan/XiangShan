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

class RedirectGenerator extends XSModule with NeedImpl {
  val io = IO(new Bundle() {
    val loadRelay = Flipped(ValidIO(new Redirect))
    val exuMispredict = Vec(exuParameters.JmpCnt + exuParameters.AluCnt, Flipped(ValidIO(new ExuOutput)))
    val roqRedirect = Flipped(ValidIO(new Redirect))
    val exuFtqRead = new FtqRead
    val stage2Redirect = ValidIO(new Redirect)
    val stage3CfiUpdate = Output(ValidIO(new CfiUpdateInfo))
  })
  /*
      loadReplay and roqRedirect already read cfi update info from ftq
      exus haven't read, they need to read at stage 2

        LoadQueue  Jump  ALU0  ALU1  ALU2  ALU3   exception    Stage1
          |         |      |    |     |     |         |
          |         |==== reg & compare ====|         |       ========
          |                   |                       |
          |                ftq read                   |
          |------- mux ------|                        |        Stage2
                    |                                 |
                    redirect (flush backend)          |
                    |                                 |
               === reg ===                            |       ========
                    |                                 |
                    |----- mux (exception first) -----|        Stage3
                            |
                redirect (send to frontend)
   */
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
  val frontendRedirect = redirectGen.io.stage3CfiUpdate

  ftq.io.enq <> io.frontend.fetchInfo
  for(i <- 0 until CommitWidth){
    ftq.io.roq_commits(i).valid := roq.io.commits.valid(i)
    ftq.io.roq_commits(i).bits := roq.io.commits.info(i)
  }
  ftq.io.redirect <> backendRedirect
  ftq.io.exuWriteback <> io.fromIntBlock.exuRedirect

  ftq.io.ftqRead(1) <> redirectGen.io.exuFtqRead
  ftq.io.ftqRead(2) <> DontCare // TODO: read exception pc / load replay pc form here

  io.frontend.redirect_cfiUpdate := frontendRedirect
  io.frontend.commit_cfiUpdate := ftq.io.commit_ftqEntry

  decode.io.in <> io.frontend.cfVec

  val jumpInst = dispatch.io.enqIQCtrl(0).bits
  ftq.io.ftqRead(0).ptr := jumpInst.cf.ftqPtr // jump
  io.toIntBlock.jumpPc := GetPcByFtq(ftq.io.ftqRead(0).entry.ftqPC, jumpInst.cf.ftqOffset)

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
