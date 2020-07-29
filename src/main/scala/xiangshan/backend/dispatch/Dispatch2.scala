package xiangshan.backend.dispatch

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.exu.ExuConfig
import xiangshan.backend.regfile.RfReadPort
import utils.{XSDebug, XSInfo}

class Dispatch2() extends XSModule{
  val io = IO(new Bundle() {
    // from dispatch queues
    val fromIntDq = Flipped(Vec(dpParams.IntDqDeqWidth, DecoupledIO(new MicroOp)))
    val fromFpDq = Flipped(Vec(dpParams.FpDqDeqWidth, DecoupledIO(new MicroOp)))
    val fromLsDq = Flipped(Vec(dpParams.LsDqDeqWidth, DecoupledIO(new MicroOp)))
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
    val enqIQData = Vec(exuParameters.ExuCnt - exuParameters.LsExuCnt, Output(new ExuInput))
  })
  io.enqIQData <> DontCare
  io.enqIQCtrl <> DontCare

  val intDispatch = Module(new Dispatch2Int)
  intDispatch.io.fromDq <> io.fromIntDq
  intDispatch.io.readRf <> io.readIntRf
  intDispatch.io.regRdy := io.intPregRdy
  intDispatch.io.numExist.zipWithIndex.map({case (num, i) => num := io.numExist(i) })
  intDispatch.io.enqIQCtrl.zipWithIndex.map({case (enq, i) => enq <> io.enqIQCtrl(i) })
  intDispatch.io.enqIQData.zipWithIndex.map({case (enq, i) => enq <> io.enqIQData(i) })

  io.fromFpDq <> DontCare
  io.fromLsDq <> DontCare
  io.readFpRf <> DontCare
  io.intMemRegAddr <> DontCare
  io.fpMemRegAddr <> DontCare
}
