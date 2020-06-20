package xiangshan.backend.dispatch

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.utils._
import xiangshan.backend.regfile.RfReadPort

class Dispatch extends XSModule {
  val io = new Bundle() {
    val redirect = Flipped(ValidIO(new Redirect))
    // from rename
    val fromRename = Vec(RenameWidth, Flipped(DecoupledIO(new MicroOp)))
    // enq Roq
    val toRoq =  Vec(RenameWidth, DecoupledIO(new MicroOp))
    // get RoqIdx
    val roqIdxs = Input(Vec(RenameWidth, UInt(RoqIdxWidth.W)))
    // read regfile
    val readIntRf = Vec(NRReadPorts, Flipped(new RfReadPort))
    val readFpRf = Vec(NRReadPorts, Flipped(new RfReadPort))
    // ro reservation stations
    val enqIQCtrl = Vec(exuConfig.ExuCnt, DecoupledIO(new MicroOp))
    val enqIQData = Vec(exuConfig.ExuCnt, ValidIO(new ExuInput))
  }
  // pipeline between rename and dispatch
  val dispatch1 = Module(new Dispatch1())
  for (i <- 0 until RenameWidth) {
    PipelineConnect(io.fromRename(i), dispatch1.io.fromRename(i), dispatch1.io.recv(i), false.B)
  }
  val intDq = Module(new DispatchQueue(new MicroOp, dp1Config.IntDqSize, RenameWidth, IntDqDeqWidth))
  val fpDq = Module(new DispatchQueue(new MicroOp, dp1Config.FpDqSize, RenameWidth, FpDqDeqWidth))
  val lsDq = Module(new DispatchQueue(new MicroOp, dp1Config.LsDqSize, RenameWidth, LsDqDeqWidth))
  val dispatch2 = Module(new Dispatch2())

  dispatch1.io.redirect <> io.redirect
  dispatch1.io.toRoq <> io.toRoq
  dispatch1.io.roqIdxs <> io.roqIdxs
  dispatch1.io.toIntDq <> intDq.io.enq
  dispatch1.io.toFpDq <> fpDq.io.enq
  dispatch1.io.toLsDq <> fpDq.io.enq

  dispatch2.io.fromIntDq <> intDq.io.deq
  dispatch2.io.fromFpDq <> fpDq.io.deq
  dispatch2.io.fromLsDq <> lsDq.io.deq
  dispatch2.io.readIntRf <> io.readIntRf
  dispatch2.io.readFpRf <> io.readFpRf
  dispatch2.io.enqIQCtrl <> io.enqIQCtrl
  dispatch2.io.enqIQData <> io.enqIQData
}
