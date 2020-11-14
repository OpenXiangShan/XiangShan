package xiangshan.backend.dispatch

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.backend.regfile.RfReadPort
import chisel3.ExcitingUtils._
import xiangshan.backend.exu.ExuConfig
import xiangshan.backend.roq.RoqPtr

case class DispatchParameters
(
  DqEnqWidth: Int,
  IntDqSize: Int,
  FpDqSize: Int,
  LsDqSize: Int,
  IntDqDeqWidth: Int,
  FpDqDeqWidth: Int,
  LsDqDeqWidth: Int,
  IntDqReplayWidth: Int,
  FpDqReplayWidth: Int,
  LsDqReplayWidth: Int
)

class Dispatch
(
  jmpCfg: ExuConfig,
  aluCfg: ExuConfig,
  mduCfg: ExuConfig,
  fmacCfg: ExuConfig,
  fmiscCfg: ExuConfig,
  ldCfg: ExuConfig,
  stCfg: ExuConfig
) extends XSModule {
  val io = IO(new Bundle() {
    // flush or replay
    val redirect = Flipped(ValidIO(new Redirect))
    // from rename
    val fromRename = Vec(RenameWidth, Flipped(DecoupledIO(new MicroOp)))
    // enq Roq
    val toRoq =  Vec(RenameWidth, DecoupledIO(new MicroOp))
    // get RoqIdx
    val roqIdxs = Input(Vec(RenameWidth, new RoqPtr))
    // enq Lsroq
    val toLsroq =  Vec(RenameWidth, DecoupledIO(new MicroOp))
    // get LsIdx
    val lsIdxs = Input(Vec(RenameWidth, new LSIdx))
    val dequeueRoqIndex = Input(Valid(new RoqPtr))
    // read regfile
    val readIntRf = Vec(NRIntReadPorts - NRMemReadPorts, Flipped(new RfReadPort))
    val readFpRf = Vec(NRFpReadPorts - exuParameters.StuCnt, Flipped(new RfReadPort))
    // read reg status (busy/ready)
    val intPregRdy = Vec(NRIntReadPorts - NRMemReadPorts, Input(Bool()))
    val fpPregRdy = Vec(NRFpReadPorts - exuParameters.StuCnt, Input(Bool()))
    // load + store reg status (busy/ready)
    val memIntRf = Vec(NRMemReadPorts, Flipped(new RfReadPort))
    val memFpRf = Vec(exuParameters.StuCnt, Flipped(new RfReadPort))
    val intMemRegRdy = Vec(NRMemReadPorts, Input(Bool()))
    val fpMemRegRdy = Vec(exuParameters.StuCnt, Input(Bool()))
    // replay: set preg status to not ready
    val replayPregReq = Output(Vec(ReplayWidth, new ReplayPregReq))
    // to reservation stations
    val numExist = Input(Vec(exuParameters.ExuCnt, UInt(log2Ceil(IssQueSize).W)))
    val enqIQCtrl = Vec(exuParameters.ExuCnt, DecoupledIO(new MicroOp))
    val enqIQData = Vec(exuParameters.ExuCnt, Output(new ExuInput))
  })

  val dispatch1 = Module(new Dispatch1)
  val intDq = Module(new DispatchQueue(dpParams.IntDqSize, dpParams.DqEnqWidth, dpParams.IntDqDeqWidth, dpParams.IntDqReplayWidth))
  val fpDq = Module(new DispatchQueue(dpParams.FpDqSize, dpParams.DqEnqWidth, dpParams.FpDqDeqWidth, dpParams.FpDqReplayWidth))
  val lsDq = Module(new DispatchQueue(dpParams.LsDqSize, dpParams.DqEnqWidth, dpParams.LsDqDeqWidth, dpParams.LsDqReplayWidth))

  // pipeline between rename and dispatch
  // accepts all at once
  for (i <- 0 until RenameWidth) {
    PipelineConnect(io.fromRename(i), dispatch1.io.fromRename(i), dispatch1.io.recv(i), false.B)
  }

  // dispatch 1: accept uops from rename and dispatch them to the three dispatch queues
  dispatch1.io.redirect <> io.redirect
  dispatch1.io.toRoq <> io.toRoq
  dispatch1.io.roqIdxs <> io.roqIdxs
  dispatch1.io.toLsroq <> io.toLsroq
  dispatch1.io.lsIdx <> io.lsIdxs
  dispatch1.io.toIntDq <> intDq.io.enq
  dispatch1.io.toFpDq <> fpDq.io.enq
  dispatch1.io.toLsDq <> lsDq.io.enq

  // dispatch queue: queue uops and dispatch them to different reservation stations or issue queues
  // it may cancel the uops
  intDq.io.redirect <> io.redirect
  intDq.io.dequeueRoqIndex <> io.dequeueRoqIndex
  intDq.io.replayPregReq.zipWithIndex.map { case(replay, i) =>
    io.replayPregReq(i) <> replay
  }
  intDq.io.otherWalkDone := !fpDq.io.inReplayWalk && !lsDq.io.inReplayWalk

  fpDq.io.redirect <> io.redirect
  fpDq.io.dequeueRoqIndex <> io.dequeueRoqIndex
  fpDq.io.replayPregReq.zipWithIndex.map { case(replay, i) =>
    io.replayPregReq(i + dpParams.IntDqReplayWidth) <> replay
  }
  fpDq.io.otherWalkDone := !intDq.io.inReplayWalk && !lsDq.io.inReplayWalk

  lsDq.io.redirect <> io.redirect
  lsDq.io.dequeueRoqIndex <> io.dequeueRoqIndex
  lsDq.io.replayPregReq.zipWithIndex.map { case(replay, i) =>
    io.replayPregReq(i + dpParams.IntDqReplayWidth + dpParams.FpDqReplayWidth) <> replay
  }
  lsDq.io.otherWalkDone := !intDq.io.inReplayWalk && !fpDq.io.inReplayWalk

  // Int dispatch queue to Int reservation stations
  val intDispatch = Module(new Dispatch2Int(jmpCfg, aluCfg, mduCfg))
  intDispatch.io.fromDq <> intDq.io.deq
  intDispatch.io.readRf <> io.readIntRf
  intDispatch.io.regRdy := io.intPregRdy
  intDispatch.io.numExist.zipWithIndex.map({case (num, i) => num := io.numExist(i)})
  intDispatch.io.enqIQCtrl.zipWithIndex.map({case (enq, i) => enq <> io.enqIQCtrl(i)})
  intDispatch.io.enqIQData.zipWithIndex.map({case (enq, i) => enq <> io.enqIQData(i)})

  // TODO: Fp dispatch queue to Fp reservation stations
  if (exuParameters.FpExuCnt > 0) {
    val fpDispatch = Module(new Dispatch2Fp(fmacCfg, fmiscCfg))
    fpDispatch.io.fromDq <> fpDq.io.deq
    fpDispatch.io.readRf <> io.readFpRf
    fpDispatch.io.regRdy <> io.fpPregRdy
    fpDispatch.io.numExist.zipWithIndex.map({case (num, i) => num := io.numExist(i + exuParameters.IntExuCnt)})
    fpDispatch.io.enqIQCtrl.zipWithIndex.map({case (enq, i) => enq <> io.enqIQCtrl(i + exuParameters.IntExuCnt)})
    fpDispatch.io.enqIQData.zipWithIndex.map({case (enq, i) => enq <> io.enqIQData(i + exuParameters.IntExuCnt)})
  }
  else {
    fpDq.io.deq <> DontCare
    io.readFpRf <> DontCare
  }
  
  // Load/store dispatch queue to load/store issue queues
  val lsDispatch = Module(new Dispatch2Ls(ldCfg, stCfg))
  lsDispatch.io.fromDq <> lsDq.io.deq
  lsDispatch.io.readIntRf <> io.memIntRf
  lsDispatch.io.readFpRf <> io.memFpRf
  lsDispatch.io.intRegRdy <> io.intMemRegRdy
  lsDispatch.io.fpRegRdy <> io.fpMemRegRdy
  lsDispatch.io.numExist.zipWithIndex.map({case (num, i) => num := io.numExist(exuParameters.IntExuCnt + exuParameters.FpExuCnt + i)})
  lsDispatch.io.enqIQCtrl.zipWithIndex.map({case (enq, i) => enq <> io.enqIQCtrl(exuParameters.IntExuCnt + exuParameters.FpExuCnt + i)})

  val inWalk = intDq.io.inReplayWalk || fpDq.io.inReplayWalk || lsDq.io.inReplayWalk
  XSPerf("replayWalkCycle", inWalk)
  lsDispatch.io.enqIQData.zipWithIndex.map({case (enq, i) => enq <> io.enqIQData(exuParameters.IntExuCnt + exuParameters.FpExuCnt + i)})
}
