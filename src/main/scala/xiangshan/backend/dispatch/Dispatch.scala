package xiangshan.backend.dispatch

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.backend.regfile.RfReadPort
import chisel3.ExcitingUtils._
import xiangshan.backend.roq.{RoqPtr, RoqEnqIO}
import xiangshan.backend.rename.RenameBypassInfo
import xiangshan.mem.LsqEnqIO

case class DispatchParameters
(
  IntDqSize: Int,
  FpDqSize: Int,
  LsDqSize: Int,
  IntDqDeqWidth: Int,
  FpDqDeqWidth: Int,
  LsDqDeqWidth: Int
)

class Dispatch extends XSModule {
  val io = IO(new Bundle() {
    // flush or replay
    val redirect = Flipped(ValidIO(new Redirect))
    // from rename
    val fromRename = Vec(RenameWidth, Flipped(DecoupledIO(new MicroOp)))
    val renameBypass = Input(new RenameBypassInfo)
    // to busytable: set pdest to busy (not ready) when they are dispatched
    val allocPregs = Vec(RenameWidth, Output(new ReplayPregReq))
    // enq Roq
    val enqRoq = Flipped(new RoqEnqIO)
    // enq Lsq
    val enqLsq = Flipped(new LsqEnqIO)
    // read regfile
    val readIntRf = Vec(NRIntReadPorts, Flipped(new RfReadPort(XLEN)))
    val readFpRf = Vec(NRFpReadPorts, Flipped(new RfReadPort(XLEN + 1)))
    // read reg status (busy/ready)
    val intPregRdy = Vec(NRIntReadPorts, Input(Bool()))
    val fpPregRdy = Vec(NRFpReadPorts, Input(Bool()))
    // to reservation stations
    val numExist = Input(Vec(exuParameters.ExuCnt, UInt(log2Ceil(IssQueSize).W)))
    val enqIQCtrl = Vec(exuParameters.ExuCnt, DecoupledIO(new MicroOp))
    // send reg file read port index to reservation stations
    val readPortIndex = new Bundle {
      val intIndex = Vec(exuParameters.IntExuCnt, Output(UInt(log2Ceil(NRIntReadPorts).W)))
      val fpIndex = Vec(exuParameters.FpExuCnt, Output(UInt(log2Ceil(NRFpReadPorts - exuParameters.StuCnt).W)))
      // ls: hardwired to (0, 1, 2, 4)
    }
  })

  val dispatch1 = Module(new Dispatch1)
  val intDq = Module(new DispatchQueue(dpParams.IntDqSize, RenameWidth, dpParams.IntDqDeqWidth))
  val fpDq = Module(new DispatchQueue(dpParams.FpDqSize, RenameWidth, dpParams.FpDqDeqWidth))
  val lsDq = Module(new DispatchQueue(dpParams.LsDqSize, RenameWidth, dpParams.LsDqDeqWidth))

  // pipeline between rename and dispatch
  // accepts all at once
  val redirectValid = io.redirect.valid// && !io.redirect.bits.isReplay
  for (i <- 0 until RenameWidth) {
    PipelineConnect(io.fromRename(i), dispatch1.io.fromRename(i), dispatch1.io.recv(i), redirectValid)
  }

  // dispatch 1: accept uops from rename and dispatch them to the three dispatch queues
  // dispatch1.io.redirect <> io.redirect
  dispatch1.io.renameBypass := RegEnable(io.renameBypass, io.fromRename(0).valid && dispatch1.io.fromRename(0).ready)
  dispatch1.io.enqRoq <> io.enqRoq
  dispatch1.io.enqLsq <> io.enqLsq
  dispatch1.io.toIntDq <> intDq.io.enq
  dispatch1.io.toFpDq <> fpDq.io.enq
  dispatch1.io.toLsDq <> lsDq.io.enq
  dispatch1.io.allocPregs <> io.allocPregs

  // dispatch queue: queue uops and dispatch them to different reservation stations or issue queues
  // it may cancel the uops
  intDq.io.redirect <> io.redirect
  fpDq.io.redirect <> io.redirect
  lsDq.io.redirect <> io.redirect

  // Int dispatch queue to Int reservation stations
  val intDispatch = Module(new Dispatch2Int)
  intDispatch.io.fromDq <> intDq.io.deq
  intDispatch.io.readRf.zipWithIndex.map({case (r, i) => r <> io.readIntRf(i)})
  intDispatch.io.regRdy.zipWithIndex.map({case (r, i) => r <> io.intPregRdy(i)})
  intDispatch.io.numExist.zipWithIndex.map({case (num, i) => num := io.numExist(i)})
  intDispatch.io.enqIQCtrl.zipWithIndex.map({case (enq, i) => enq <> io.enqIQCtrl(i)})
//  intDispatch.io.enqIQData.zipWithIndex.map({case (enq, i) => enq <> io.enqIQData(i)})
  intDispatch.io.readPortIndex <> io.readPortIndex.intIndex

  // Fp dispatch queue to Fp reservation stations
  val fpDispatch = Module(new Dispatch2Fp)
  fpDispatch.io.fromDq <> fpDq.io.deq
  fpDispatch.io.readRf.zipWithIndex.map({case (r, i) => r <> io.readFpRf(i)})
  fpDispatch.io.regRdy.zipWithIndex.map({case (r, i) => r <> io.fpPregRdy(i)})
  fpDispatch.io.numExist.zipWithIndex.map({case (num, i) => num := io.numExist(i + exuParameters.IntExuCnt)})
  fpDispatch.io.enqIQCtrl.zipWithIndex.map({case (enq, i) => enq <> io.enqIQCtrl(i + exuParameters.IntExuCnt)})
//  fpDispatch.io.enqIQData.zipWithIndex.map({case (enq, i) => enq <> io.enqIQData(i + exuParameters.IntExuCnt)})
  fpDispatch.io.readPortIndex <> io.readPortIndex.fpIndex
  
  // Load/store dispatch queue to load/store issue queues
  val lsDispatch = Module(new Dispatch2Ls)
  lsDispatch.io.fromDq <> lsDq.io.deq
  lsDispatch.io.readIntRf.zipWithIndex.map({case (r, i) => r <> io.readIntRf(i + 8)})
  lsDispatch.io.readFpRf.zipWithIndex.map({case (r, i) => r <> io.readFpRf(i + 12)})
  lsDispatch.io.intRegRdy.zipWithIndex.map({case (r, i) => r <> io.intPregRdy(i + 8)})
  lsDispatch.io.fpRegRdy.zipWithIndex.map({case (r, i) => r <> io.fpPregRdy(i + 12)})
  lsDispatch.io.numExist.zipWithIndex.map({case (num, i) => num := io.numExist(exuParameters.IntExuCnt + exuParameters.FpExuCnt + i)})
  lsDispatch.io.enqIQCtrl.zipWithIndex.map({case (enq, i) => enq <> io.enqIQCtrl(exuParameters.IntExuCnt + exuParameters.FpExuCnt + i)})
//  lsDispatch.io.enqIQData.zipWithIndex.map({case (enq, i) => enq <> io.enqIQData(exuParameters.IntExuCnt + exuParameters.FpExuCnt + i)})
}
