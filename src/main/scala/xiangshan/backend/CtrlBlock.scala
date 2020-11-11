package xiangshan.backend

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.decode.{DecodeBuffer, DecodeStage}
import xiangshan.backend.rename.Rename
import xiangshan.backend.brq.Brq
import xiangshan.backend.dispatch.Dispatch
import xiangshan.backend.exu._
import xiangshan.backend.issue.ReservationStationNew
import xiangshan.backend.regfile.RfReadPort
import xiangshan.backend.roq.{Roq, RoqPtr}
import xiangshan.mem._
import xiangshan.backend.fu.FunctionUnit._


class FpBlockToCtrlIO extends XSBundle {
  // TODO: should not be FpExuCnt
  val wbIntRegs = Vec(exuParameters.FpExuCnt, Flipped(ValidIO(new ExuOutput)))
  val wbFpRegs = Vec(exuParameters.FpExuCnt, Flipped(ValidIO(new ExuOutput)))
  val numExist = Vec(exuParameters.FpExuCnt, Output(UInt(log2Ceil(IssQueSize).W)))
}

class CtrlToIntBlockIO extends XSBundle {
  val enqIqCtrl = Vec(exuParameters.IntExuCnt, DecoupledIO(new MicroOp))
  val enqIqData = Vec(exuParameters.IntExuCnt, Output(new ExuInput))
  val readIntRf = Vec(NRIntReadPorts, Flipped(new RfReadPort))
}

class CtrlToFpBlockIO extends XSBundle {
  val enqIqCtrl = Vec(exuParameters.FpExuCnt, DecoupledIO(new MicroOp))
  val enqIqData = Vec(exuParameters.FpExuCnt, Output(new ExuInput))
  val readFpRf = Vec(NRFpReadPorts, Flipped(new RfReadPort))
}

class CtrlToLsBlockIO extends XSBundle {
  val enqIqCtrl = Vec(exuParameters.LsExuCnt, DecoupledIO(new MicroOp))
  val enqIqData = Vec(exuParameters.LsExuCnt, Output(new ExuInput))
  val lsqIdxReq = Vec(RenameWidth, DecoupledIO(new MicroOp))
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
  })

  val decode = Module(new DecodeStage)
  val brq = Module(new Brq)
  val decBuf = Module(new DecodeBuffer)
  val rename = Module(new Rename)
  val dispatch = Module(new Dispatch(
    jmpExeUnit.config, aluExeUnits(0).config, mduExeUnits(0).config,
    fmacExeUnits(0).config, fmiscExeUnits(0).config,
    ldExeUnitCfg, stExeUnitCfg
  ))
  // TODO: move busyTable to dispatch1
  // val fpBusyTable = Module(new BusyTable(NRFpReadPorts, NRFpWritePorts))
  // val intBusyTable = Module(new BusyTable(NRIntReadPorts, NRIntWritePorts))
  val roq = Module(new Roq)

  val fromExeBlock = (io.fromIntBlock, io.fromFpBlock, io.fromLsBlock)
  val toExeBlock = (io.toIntBlock, io.toFpBlock, io.toLsBlock)

  val redirect = Mux(
    roq.io.redirect.valid,
    roq.io.redirect,
    Mux(
      brq.io.redirect.valid,
      brq.io.redirect,
      io.fromLsBlock.replay
    )
  )

  decode.io.in <> io.frontend.cfVec
  decode.io.toBrq <> brq.io.enqReqs
  decode.io.brTags <> brq.io.brTags
  decode.io.out <> decBuf.io.in

  decBuf.io.isWalking := roq.io.commits(0).valid && roq.io.commits(0).bits.isWalk
  decBuf.io.redirect <> redirect
  decBuf.io.out <> rename.io.in

  rename.io.redirect <> redirect
  rename.io.roqCommits <> roq.io.commits
  // they should be moved to busytables
  rename.io.wbIntResults <> io.fromIntBlock.wbIntRegs ++ io.fromFpBlock.wbIntRegs ++ io.fromLsBlock.wbIntRegs
  rename.io.wbFpResults <> io.fromIntBlock.wbFpRegs ++ io.fromFpBlock.wbFpRegs ++ io.fromLsBlock.wbFpRegs
  rename.io.intRfReadAddr <> dispatch.io.readIntRf.map(_.addr)
  rename.io.fpRfReadAddr <> dispatch.io.readFpRf.map(_.addr)
  rename.io.intPregRdy <> dispatch.io.intPregRdy
  rename.io.fpPregRdy <> dispatch.io.fpPregRdy
  rename.io.replayPregReq <> dispatch.io.replayPregReq
  rename.io.out <> dispatch.io.fromRename

  dispatch.io.redirect <> redirect
  dispatch.io.toRoq <> roq.io.dp1Req
  dispatch.io.roqIdxs <> roq.io.roqIdxs
  dispatch.io.toLsroq <> io.toLsBlock.lsqIdxReq
  dispatch.io.lsIdxs <> io.fromLsBlock.lsqIdxResp
  dispatch.io.dequeueRoqIndex.valid := roq.io.commitRoqIndex.valid || io.fromLsBlock.oldestStore.valid
  dispatch.io.dequeueRoqIndex.bits = Mux(io.fromLsBlock.oldestStore.valid, io.fromLsBlock.oldestStore.bits, roq.io.commitRoqIndex.bits)
  dispatch.io.readIntRf <> io.toIntBlock.rfReadPorts
  dispatch.io.readFpRf <> io.toFpBlock.rfReadPorts
  dispatch.io.numExist <> io.fromIntBlock.numExist ++ io.fromFpBlock.numExist ++ io.fromLsBlock.numExist
  dispatch.io.enqIQCtrl <> io.toIntBlock.enqIQCtrl ++ io.toFpBlock.enqIQCtrl ++ io.toLsBlock.enqIQCtrl
  dispatch.io.enqIqData <> io.toIntBlock.enqIqData ++ io.toFpBlock.enqIqData ++ io.toLsBlock.enqIqData

  // val flush = redirect.valid && (redirect.bits.isException || redirect.bits.isFlushPipe)
  // fpBusyTable.flush := flush
  // intBusyTable.flush := flush
  // busytable io
  // maybe update busytable in dispatch1?

}
