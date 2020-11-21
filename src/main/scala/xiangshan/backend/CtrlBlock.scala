package xiangshan.backend

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.decode.{DecodeBuffer, DecodeStage}
import xiangshan.backend.rename.Rename
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
  val lsqIdxReq = Vec(RenameWidth, DecoupledIO(new MicroOp))
  val redirect = ValidIO(new Redirect)
}

class CtrlBlock extends XSModule {
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
  // TODO: move busyTable to dispatch1
  // val fpBusyTable = Module(new BusyTable(NRFpReadPorts, NRFpWritePorts))
  // val intBusyTable = Module(new BusyTable(NRIntReadPorts, NRIntWritePorts))

  val roqWbSize = NRIntWritePorts + NRFpWritePorts + exuParameters.StuCnt + 1

  val roq = Module(new Roq(roqWbSize))

  val redirect = Mux(
    roq.io.redirect.valid,
    roq.io.redirect,
    Mux(
      brq.io.redirect.valid,
      brq.io.redirect,
      io.fromLsBlock.replay
    )
  )

  io.frontend.redirect := redirect
  io.frontend.redirect.valid := redirect.valid && !redirect.bits.isReplay
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
  decBuf.io.redirect <> redirect
  decBuf.io.out <> rename.io.in

  rename.io.redirect <> redirect
  rename.io.roqCommits <> roq.io.commits
  // they should be moved to busytables
  rename.io.wbIntResults <> io.fromIntBlock.wbRegs
  rename.io.wbFpResults <> io.fromFpBlock.wbRegs
  rename.io.intRfReadAddr <> dispatch.io.readIntRf.map(_.addr)
  rename.io.fpRfReadAddr <> dispatch.io.readFpRf.map(_.addr)
  rename.io.intPregRdy <> dispatch.io.intPregRdy
  rename.io.fpPregRdy <> dispatch.io.fpPregRdy
  rename.io.replayPregReq <> dispatch.io.replayPregReq
  rename.io.out <> dispatch.io.fromRename

  dispatch.io.redirect <> redirect
  dispatch.io.toRoq <> roq.io.dp1Req
  dispatch.io.roqIdxs <> roq.io.roqIdxs
  dispatch.io.toLsq <> io.toLsBlock.lsqIdxReq
  dispatch.io.lsIdxs <> io.fromLsBlock.lsqIdxResp
  dispatch.io.dequeueRoqIndex.valid := roq.io.commitRoqIndex.valid || io.oldestStore.valid
  dispatch.io.dequeueRoqIndex.bits := Mux(io.oldestStore.valid,
    io.oldestStore.bits,
    roq.io.commitRoqIndex.bits
  )
  dispatch.io.readIntRf <> io.toIntBlock.readRf
  dispatch.io.readFpRf <> io.toFpBlock.readRf
  dispatch.io.numExist <> io.fromIntBlock.numExist ++ io.fromFpBlock.numExist ++ io.fromLsBlock.numExist
  dispatch.io.enqIQCtrl <> io.toIntBlock.enqIqCtrl ++ io.toFpBlock.enqIqCtrl ++ io.toLsBlock.enqIqCtrl
  dispatch.io.enqIQData <> io.toIntBlock.enqIqData ++ io.toFpBlock.enqIqData ++ io.toLsBlock.enqIqData


  roq.io.memRedirect <> io.fromLsBlock.replay
  roq.io.brqRedirect <> brq.io.redirect
  roq.io.dp1Req <> dispatch.io.toRoq


  roq.io.exeWbResults.take(roqWbSize-1).zip(
    io.fromIntBlock.wbRegs ++ io.fromFpBlock.wbRegs ++ io.fromLsBlock.stOut
  ).foreach{
    case(x, y) =>
      x.bits := y.bits
      x.valid := y.valid && !y.bits.redirectValid
  }
  roq.io.exeWbResults.last := brq.io.out

  io.toIntBlock.redirect := redirect
  io.toFpBlock.redirect := redirect
  io.toLsBlock.redirect := redirect

  // roq to int block
  io.roqio.toCSR <> roq.io.csr
  io.roqio.exception.valid := roq.io.redirect.valid && roq.io.redirect.bits.isException
  io.roqio.exception.bits := roq.io.exception
  io.roqio.isInterrupt := roq.io.redirect.bits.isFlushPipe
  // roq to mem block
  io.roqio.roqDeqPtr := roq.io.roqDeqPtr
  io.roqio.commits := roq.io.commits
}
