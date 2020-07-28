package xiangshan.backend.dispatch

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.exu.ExuConfig
import utils._
import xiangshan.backend.regfile.RfReadPort

class Dispatch(exuCfg: Array[ExuConfig]) extends XSModule with NeedImpl {
  val io = IO(new Bundle() {
    val redirect = Flipped(ValidIO(new Redirect))
    // from rename
    val fromRename = Vec(RenameWidth, Flipped(DecoupledIO(new MicroOp)))
    // enq Roq
    val toRoq =  Vec(RenameWidth, DecoupledIO(new MicroOp))
    // get RoqIdx
    val roqIdxs = Input(Vec(RenameWidth, UInt(RoqIdxWidth.W)))
    // enq Moq
    val toMoq =  Vec(RenameWidth, DecoupledIO(new MicroOp))
    // get MoqIdx
    val moqIdxs = Input(Vec(RenameWidth, UInt(MoqIdxWidth.W)))

    // read regfile
    val readIntRf = Vec(NRIntReadPorts, Flipped(new RfReadPort))
    val readFpRf = Vec(NRFpReadPorts - exuParameters.StuCnt, Flipped(new RfReadPort))
    // read reg status (busy/ready)
    val intPregRdy = Vec(NRIntReadPorts, Input(Bool()))
    val fpPregRdy = Vec(NRFpReadPorts - exuParameters.StuCnt, Input(Bool()))
    // load + store reg status (busy/ready)
    val intMemRegAddr = Vec(NRMemReadPorts, Output(UInt(PhyRegIdxWidth.W)))
    val fpMemRegAddr = Vec(exuParameters.StuCnt, Output(UInt(PhyRegIdxWidth.W)))
    val intMemRegRdy = Vec(NRMemReadPorts, Input(Bool()))
    val fpMemRegRdy = Vec(exuParameters.StuCnt, Input(Bool()))

    // to reservation stations
    val numExist = Input(Vec(exuParameters.ExuCnt, UInt(log2Ceil(IssQueSize).W)))
    val enqIQCtrl = Vec(exuParameters.ExuCnt, DecoupledIO(new MicroOp))
    val enqIQData = Vec(exuParameters.ExuCnt - exuParameters.LsExuCnt, ValidIO(new ExuInput))
  })
  // pipeline between rename and dispatch
//  val dispatch1 = Module(new Dispatch1)
//  for (i <- 0 until RenameWidth) {
//    PipelineConnect(io.fromRename(i), dispatch1.io.fromRename(i), dispatch1.io.recv(i), false.B)
//  }
//  val intDq = Module(new DispatchQueue(dp1Paremeters.IntDqSize, RenameWidth, IntDqDeqWidth, "IntDpQ"))
//  val fpDq = Module(new DispatchQueue(dp1Paremeters.FpDqSize, RenameWidth, FpDqDeqWidth, "FpDpQ"))
//  val lsDq = Module(new DispatchQueue(dp1Paremeters.LsDqSize, RenameWidth, LsDqDeqWidth, "LsDpQ"))
//  val dispatch2 = Module(new Dispatch2(exuCfg))
//
//  dispatch1.io.redirect <> io.redirect
//  dispatch1.io.toRoq <> io.toRoq
//  dispatch1.io.roqIdxs <> io.roqIdxs
//  dispatch1.io.toMoq <> io.toMoq
//  dispatch1.io.moqIdxs <> io.moqIdxs
//  dispatch1.io.toIntDq <> intDq.io.enq
//  dispatch1.io.toFpDq <> fpDq.io.enq
//  dispatch1.io.toLsDq <> lsDq.io.enq
//
//  // dispatch queue cancels the uops
//  intDq.io.redirect <> io.redirect
//  fpDq.io.redirect <> io.redirect
//  lsDq.io.redirect <> io.redirect
//
//  // dispatch2 only receives valid uops from dispatch queue
//  dispatch2.io.fromIntDq <> intDq.io.deq
//  dispatch2.io.fromFpDq <> fpDq.io.deq
//  dispatch2.io.fromLsDq <> lsDq.io.deq
//  dispatch2.io.readIntRf <> io.readIntRf
//  dispatch2.io.readFpRf <> io.readFpRf
//  dispatch2.io.intPregRdy <> io.intPregRdy
//  dispatch2.io.fpPregRdy <> io.fpPregRdy
//  dispatch2.io.enqIQCtrl <> io.enqIQCtrl
//  dispatch2.io.enqIQData <> io.enqIQData
//  dispatch2.io.numExist <> io.numExist
}
