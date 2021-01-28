package xiangshan.backend

import chisel3._
import chisel3.util._
import utils._
import xiangshan._
import xiangshan.backend.decode.DecodeStage
import xiangshan.backend.rename.{BusyTable, Rename}
import xiangshan.backend.brq.{Brq, BrqPcRead}
import xiangshan.backend.dispatch.Dispatch
import xiangshan.backend.exu._
import xiangshan.backend.exu.Exu.exuConfigs
import xiangshan.backend.regfile.RfReadPort
import xiangshan.backend.roq.{Roq, RoqCSRIO, RoqLsqIO, RoqPtr}
import xiangshan.mem.LsqEnqIO

class CtrlToIntBlockIO extends XSBundle {
  val enqIqCtrl = Vec(exuParameters.IntExuCnt, DecoupledIO(new MicroOp))
  val readRf = Vec(NRIntReadPorts, Output(UInt(PhyRegIdxWidth.W)))
  val jumpPc = Output(UInt(VAddrBits.W))
  // int block only uses port 0~7
  val readPortIndex = Vec(exuParameters.IntExuCnt, Output(UInt(log2Ceil(8 / 2).W))) // TODO parameterize 8 here
  val redirect = ValidIO(new Redirect)
}

class CtrlToFpBlockIO extends XSBundle {
  val enqIqCtrl = Vec(exuParameters.FpExuCnt, DecoupledIO(new MicroOp))
  val readRf = Vec(NRFpReadPorts, Output(UInt(PhyRegIdxWidth.W)))
  // fp block uses port 0~11
  val readPortIndex = Vec(exuParameters.FpExuCnt, Output(UInt(log2Ceil((NRFpReadPorts - exuParameters.StuCnt) / 3).W)))
  val redirect = ValidIO(new Redirect)
}

class CtrlToLsBlockIO extends XSBundle {
  val enqIqCtrl = Vec(exuParameters.LsExuCnt, DecoupledIO(new MicroOp))
  val enqLsq = Flipped(new LsqEnqIO)
  val redirect = ValidIO(new Redirect)
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
    }
  })
  difftestIO <> DontCare

  val trapIO = IO(new TrapIO())
  trapIO <> DontCare

  val decode = Module(new DecodeStage)
  val brq = Module(new Brq)
  val rename = Module(new Rename)
  val dispatch = Module(new Dispatch)
  val intBusyTable = Module(new BusyTable(NRIntReadPorts, NRIntWritePorts))
  val fpBusyTable = Module(new BusyTable(NRFpReadPorts, NRFpWritePorts))

  val roqWbSize = NRIntWritePorts + NRFpWritePorts + exuParameters.StuCnt + 1

  val roq = Module(new Roq(roqWbSize))

  // When replay and mis-prediction have the same roqIdx,
  // mis-prediction should have higher priority, since mis-prediction flushes the load instruction.
  // Thus, only when mis-prediction roqIdx is after replay roqIdx, replay should be valid.
  val brqIsAfterLsq = isAfter(brq.io.redirectOut.bits.roqIdx, io.fromLsBlock.replay.bits.roqIdx)
  val redirectArb = Mux(io.fromLsBlock.replay.valid && (!brq.io.redirectOut.valid || brqIsAfterLsq),
    io.fromLsBlock.replay.bits, brq.io.redirectOut.bits)
  val redirectValid = roq.io.redirectOut.valid || brq.io.redirectOut.valid || io.fromLsBlock.replay.valid
  val redirect = Mux(roq.io.redirectOut.valid, roq.io.redirectOut.bits, redirectArb)

  io.frontend.redirect.valid := RegNext(redirectValid)
  io.frontend.redirect.bits := RegNext(Mux(roq.io.redirectOut.valid, roq.io.redirectOut.bits.target, redirectArb.target))
  io.frontend.cfiUpdateInfo <> brq.io.cfiInfo

  decode.io.in <> io.frontend.cfVec
  decode.io.enqBrq <> brq.io.enq

  brq.io.redirect.valid <> redirectValid
  brq.io.redirect.bits <> redirect
  brq.io.bcommit <> roq.io.bcommit
  brq.io.exuRedirectWb <> io.fromIntBlock.exuRedirect
  brq.io.pcReadReq.brqIdx := dispatch.io.enqIQCtrl(0).bits.brTag // jump
  io.toIntBlock.jumpPc := brq.io.pcReadReq.pc

  // pipeline between decode and dispatch
  val lastCycleRedirect = RegNext(redirectValid)
  for (i <- 0 until RenameWidth) {
    PipelineConnect(decode.io.out(i), rename.io.in(i), rename.io.in(i).ready, redirectValid || lastCycleRedirect)
  }

  rename.io.redirect.valid <> redirectValid
  rename.io.redirect.bits <> redirect
  rename.io.roqCommits <> roq.io.commits
  rename.io.out <> dispatch.io.fromRename
  rename.io.renameBypass <> dispatch.io.renameBypass

  dispatch.io.redirect.valid <> redirectValid
  dispatch.io.redirect.bits <> redirect
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


  val flush = redirectValid && RedirectLevel.isUnconditional(redirect.level)
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
  intBusyTable.io.read <> dispatch.io.readIntState
  fpBusyTable.io.read <> dispatch.io.readFpState

  roq.io.redirect.valid := brq.io.redirectOut.valid || io.fromLsBlock.replay.valid
  roq.io.redirect.bits <> redirectArb
  roq.io.exeWbResults.take(roqWbSize-1).zip(
    io.fromIntBlock.wbRegs ++ io.fromFpBlock.wbRegs ++ io.fromLsBlock.stOut
  ).foreach{
    case(x, y) =>
      x.bits := y.bits
      x.valid := y.valid && !y.bits.redirectValid
  }
  roq.io.exeWbResults.last := brq.io.out

  if (env.DualCoreDifftest) {
    difftestIO.fromRoq <> roq.difftestIO
    trapIO <> roq.trapIO
  }

  io.toIntBlock.redirect.valid := redirectValid
  io.toIntBlock.redirect.bits := redirect
  io.toFpBlock.redirect.valid := redirectValid
  io.toFpBlock.redirect.bits := redirect
  io.toLsBlock.redirect.valid := redirectValid
  io.toLsBlock.redirect.bits := redirect

  dispatch.io.readPortIndex.intIndex <> io.toIntBlock.readPortIndex
  dispatch.io.readPortIndex.fpIndex <> io.toFpBlock.readPortIndex

  // roq to int block
  io.roqio.toCSR <> roq.io.csr
  io.roqio.exception.valid := roq.io.redirectOut.valid && roq.io.redirectOut.bits.isException()
  io.roqio.exception.bits := roq.io.exception
  io.roqio.isInterrupt := roq.io.redirectOut.bits.interrupt
  // roq to mem block
  io.roqio.lsq <> roq.io.lsq
}
