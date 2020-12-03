package xiangshan.backend

import chisel3._
import chisel3.util._
import utils._
import xiangshan._
import xiangshan.backend.decode.{DecodeBuffer, DecodeStage}
import xiangshan.backend.rename.{Rename, BusyTable}
import xiangshan.backend.brq.Brq
import xiangshan.backend.dispatch.Dispatch
import xiangshan.backend.exu._
import xiangshan.backend.exu.Exu.exuConfigs
import xiangshan.backend.regfile.RfReadPort
import xiangshan.backend.roq.{Roq, RoqPtr, RoqCSRIO}

class CtrlToIntBlockIO extends XSBundle {
  val enqIqCtrl = Vec(exuParameters.IntExuCnt, DecoupledIO(new MicroOp))
  val enqIqData = Vec(exuParameters.IntExuCnt, Output(new ExuInput))
  val readRf = Vec(NRIntReadPorts, Flipped(new RfReadPort))
  val redirect = ValidIO(new Redirect)
}

class CtrlToFpBlockIO extends XSBundle {
  val enqIqCtrl = Vec(exuParameters.FpExuCnt, DecoupledIO(new MicroOp))
  val enqIqData = Vec(exuParameters.FpExuCnt, Output(new ExuInput))
  val readRf = Vec(NRFpReadPorts, Flipped(new RfReadPort))
  val redirect = ValidIO(new Redirect)
}

class CtrlToLsBlockIO extends XSBundle {
  val enqIqCtrl = Vec(exuParameters.LsExuCnt, DecoupledIO(new MicroOp))
  val enqIqData = Vec(exuParameters.LsExuCnt, Output(new ExuInput))
  val enqLsq = new Bundle() {
    val canAccept = Input(Bool())
    val req = Vec(RenameWidth, ValidIO(new MicroOp))
    val resp = Vec(RenameWidth, Input(new LSIdx))
  }
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
      val commits = Vec(CommitWidth, ValidIO(new RoqCommit))
      val roqDeqPtr = Output(new RoqPtr)
    }
    val oldestStore = Input(Valid(new RoqPtr))
  })

  val decode = Module(new DecodeStage)
  val brq = Module(new Brq)
  val decBuf = Module(new DecodeBuffer)
  val rename = Module(new Rename)
  val dispatch = Module(new Dispatch)
  val intBusyTable = Module(new BusyTable(NRIntReadPorts, NRIntWritePorts))
  val fpBusyTable = Module(new BusyTable(NRFpReadPorts, NRFpWritePorts))

  val roqWbSize = NRIntWritePorts + NRFpWritePorts + exuParameters.StuCnt + 1

  val roq = Module(new Roq(roqWbSize))

  val lsqIsAfterBrq = isAfter(io.fromLsBlock.replay.bits.roqIdx, brq.io.redirect.bits.roqIdx)
  val redirectArb = Mux(brq.io.redirect.valid && (!io.fromLsBlock.replay.valid || lsqIsAfterBrq),
    brq.io.redirect.bits, io.fromLsBlock.replay.bits)
  val redirectValid = roq.io.redirect.valid || brq.io.redirect.valid || io.fromLsBlock.replay.valid
  val redirect = Mux(roq.io.redirect.valid, roq.io.redirect.bits, redirectArb)

  io.frontend.redirect.valid := redirectValid
  io.frontend.redirect.bits := Mux(roq.io.redirect.valid, roq.io.redirect.bits.target, redirectArb.target)
  io.frontend.outOfOrderBrInfo <> brq.io.outOfOrderBrInfo
  io.frontend.inOrderBrInfo <> brq.io.inOrderBrInfo

  decode.io.in <> io.frontend.cfVec
  decode.io.toBrq <> brq.io.enqReqs
  decode.io.brTags <> brq.io.brTags
  decode.io.out <> decBuf.io.in

  brq.io.roqRedirect <> roq.io.redirect
  brq.io.memRedirect <> io.fromLsBlock.replay
  brq.io.bcommit <> roq.io.bcommit
  brq.io.enqReqs <> decode.io.toBrq
  brq.io.exuRedirect <> io.fromIntBlock.exuRedirect

  decBuf.io.isWalking := roq.io.commits(0).valid && roq.io.commits(0).bits.isWalk
  decBuf.io.redirect.valid <> redirectValid
  decBuf.io.redirect.bits <> redirect
  decBuf.io.out <> rename.io.in

  rename.io.redirect.valid <> redirectValid
  rename.io.redirect.bits <> redirect
  rename.io.roqCommits <> roq.io.commits
  rename.io.out <> dispatch.io.fromRename
  rename.io.renameBypass <> dispatch.io.renameBypass

  dispatch.io.redirect.valid <> redirectValid
  dispatch.io.redirect.bits <> redirect
  dispatch.io.enqRoq <> roq.io.enq
  dispatch.io.enqLsq <> io.toLsBlock.enqLsq
  dispatch.io.dequeueRoqIndex.valid := roq.io.commitRoqIndex.valid || io.oldestStore.valid
  dispatch.io.dequeueRoqIndex.bits := Mux(io.oldestStore.valid,
    io.oldestStore.bits,
    roq.io.commitRoqIndex.bits
  )
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
  dispatch.io.enqIQData <> io.toIntBlock.enqIqData ++ io.toFpBlock.enqIqData ++ io.toLsBlock.enqIqData


  val flush = redirectValid && (redirect.isException || redirect.isFlushPipe)
  fpBusyTable.io.flush := flush
  intBusyTable.io.flush := flush
  for((wb, setPhyRegRdy) <- io.fromIntBlock.wbRegs.zip(intBusyTable.io.wbPregs)){
    setPhyRegRdy.valid := wb.valid && wb.bits.uop.ctrl.rfWen && (wb.bits.uop.ctrl.ldest =/= 0.U)
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
  for(i <- 0 until ReplayWidth){
    intBusyTable.io.replayPregs(i).valid := dispatch.io.replayPregReq(i).isInt
    fpBusyTable.io.replayPregs(i).valid := dispatch.io.replayPregReq(i).isFp
    intBusyTable.io.replayPregs(i).bits := dispatch.io.replayPregReq(i).preg
    fpBusyTable.io.replayPregs(i).bits := dispatch.io.replayPregReq(i).preg
  }

  roq.io.memRedirect := DontCare
  roq.io.memRedirect.valid := false.B
  roq.io.brqRedirect.valid := brq.io.redirect.valid || io.fromLsBlock.replay.valid
  roq.io.brqRedirect.bits <> redirectArb
  roq.io.exeWbResults.take(roqWbSize-1).zip(
    io.fromIntBlock.wbRegs ++ io.fromFpBlock.wbRegs ++ io.fromLsBlock.stOut
  ).foreach{
    case(x, y) =>
      x.bits := y.bits
      x.valid := y.valid && !y.bits.redirectValid
  }
  roq.io.exeWbResults.last := brq.io.out

  io.toIntBlock.redirect.valid := redirectValid
  io.toIntBlock.redirect.bits := redirect
  io.toFpBlock.redirect.valid := redirectValid
  io.toFpBlock.redirect.bits := redirect
  io.toLsBlock.redirect.valid := redirectValid
  io.toLsBlock.redirect.bits := redirect

  // roq to int block
  io.roqio.toCSR <> roq.io.csr
  io.roqio.exception.valid := roq.io.redirect.valid && roq.io.redirect.bits.isException
  io.roqio.exception.bits := roq.io.exception
  io.roqio.isInterrupt := roq.io.redirect.bits.isFlushPipe
  // roq to mem block
  io.roqio.roqDeqPtr := roq.io.roqDeqPtr
  io.roqio.commits := roq.io.commits
}
