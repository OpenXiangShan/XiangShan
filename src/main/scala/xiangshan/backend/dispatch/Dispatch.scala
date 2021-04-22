package xiangshan.backend.dispatch

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.backend.regfile.RfReadPort
import xiangshan.backend.roq.{RoqPtr, RoqEnqIO}
import xiangshan.backend.rename.{RenameBypassInfo, BusyTableReadIO}
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

class Dispatch(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    // flush or replay
    val redirect = Flipped(ValidIO(new Redirect))
    val flush = Input(Bool())
    // from rename
    val fromRename = Vec(RenameWidth, Flipped(DecoupledIO(new MicroOp)))
    val renameBypass = Input(new RenameBypassInfo)
    val preDpInfo = Input(new PreDispatchInfo)
    // to busytable: set pdest to busy (not ready) when they are dispatched
    val allocPregs = Vec(RenameWidth, Output(new ReplayPregReq))
    // enq Roq
    val enqRoq = Flipped(new RoqEnqIO)
    // enq Lsq
    val enqLsq = Flipped(new LsqEnqIO)
    // read regfile
    val readIntRf = Vec(NRIntReadPorts, Output(UInt(PhyRegIdxWidth.W)))
    val readFpRf = Vec(NRFpReadPorts, Output(UInt(PhyRegIdxWidth.W)))
    // to busytable: read physical registers' state (busy/ready)
    val readIntState= Vec(NRIntReadPorts, Flipped(new BusyTableReadIO))
    val readFpState = Vec(NRFpReadPorts, Flipped(new BusyTableReadIO))
    // to reservation stations
    val numExist = Input(Vec(exuParameters.ExuCnt, UInt(log2Ceil(IssQueSize).W)))
    val enqIQCtrl = Vec(exuParameters.ExuCnt, DecoupledIO(new MicroOp))
    // send reg file read port index to reservation stations
    val readPortIndex = new Bundle {
      val intIndex = Vec(exuParameters.IntExuCnt, Output(UInt(log2Ceil(8 / 2).W)))
      val fpIndex = Vec(exuParameters.FpExuCnt, Output(UInt(log2Ceil((NRFpReadPorts - exuParameters.StuCnt) / 3).W)))
      // ls: hardwired to (0, 1, 2, 4)
    }
    val ctrlInfo = new Bundle {
      val roqFull   = Output(Bool())
      val intdqFull = Output(Bool())
      val fpdqFull  = Output(Bool())
      val lsdqFull  = Output(Bool())
    }
  })

  val dispatch1 = Module(new Dispatch1)
  val intDq = Module(new DispatchQueue(dpParams.IntDqSize, RenameWidth, dpParams.IntDqDeqWidth, "int"))
  val fpDq = Module(new DispatchQueue(dpParams.FpDqSize, RenameWidth, dpParams.FpDqDeqWidth, "fp"))
  val lsDq = Module(new DispatchQueue(dpParams.LsDqSize, RenameWidth, dpParams.LsDqDeqWidth, "ls"))

  // pipeline between rename and dispatch
  // accepts all at once
  val redirectValid = io.redirect.valid || io.flush
  for (i <- 0 until RenameWidth) {
    PipelineConnect(io.fromRename(i), dispatch1.io.fromRename(i), dispatch1.io.recv(i), redirectValid)
  }

  // dispatch 1: accept uops from rename and dispatch them to the three dispatch queues
  // dispatch1.io.redirect <> io.redirect
  dispatch1.io.renameBypass := RegEnable(io.renameBypass, io.fromRename(0).valid && dispatch1.io.fromRename(0).ready)
  dispatch1.io.preDpInfo := RegEnable(io.preDpInfo, io.fromRename(0).valid && dispatch1.io.fromRename(0).ready)
  dispatch1.io.enqRoq <> io.enqRoq
  dispatch1.io.enqLsq <> io.enqLsq
  dispatch1.io.toIntDq <> intDq.io.enq
  dispatch1.io.toFpDq <> fpDq.io.enq
  dispatch1.io.toLsDq <> lsDq.io.enq
  dispatch1.io.allocPregs <> io.allocPregs

  // dispatch queue: queue uops and dispatch them to different reservation stations or issue queues
  // it may cancel the uops
  intDq.io.redirect <> io.redirect
  intDq.io.flush <> io.flush
  fpDq.io.redirect <> io.redirect
  fpDq.io.flush <> io.flush
  lsDq.io.redirect <> io.redirect
  lsDq.io.flush <> io.flush

  // Int dispatch queue to Int reservation stations
  val intDispatch = Module(new Dispatch2Int)
  intDispatch.io.fromDq <> intDq.io.deq
  intDispatch.io.readRf.zipWithIndex.map({case (r, i) => r <> io.readIntRf(i)})
  intDispatch.io.readState.zipWithIndex.map({case (r, i) => r <> io.readIntState(i)})
  intDispatch.io.numExist.zipWithIndex.map({case (num, i) => num := io.numExist(i)})
  intDispatch.io.enqIQCtrl.zipWithIndex.map({case (enq, i) => enq <> io.enqIQCtrl(i)})
//  intDispatch.io.enqIQData.zipWithIndex.map({case (enq, i) => enq <> io.enqIQData(i)})
  intDispatch.io.readPortIndex <> io.readPortIndex.intIndex

  // Fp dispatch queue to Fp reservation stations
  val fpDispatch = Module(new Dispatch2Fp)
  fpDispatch.io.fromDq <> fpDq.io.deq
  fpDispatch.io.readRf.zipWithIndex.map({case (r, i) => r <> io.readFpRf(i)})
  fpDispatch.io.readState.zipWithIndex.map({case (r, i) => r <> io.readFpState(i)})
  fpDispatch.io.numExist.zipWithIndex.map({case (num, i) => num := io.numExist(i + exuParameters.IntExuCnt)})
  fpDispatch.io.enqIQCtrl.zipWithIndex.map({case (enq, i) => enq <> io.enqIQCtrl(i + exuParameters.IntExuCnt)})
//  fpDispatch.io.enqIQData.zipWithIndex.map({case (enq, i) => enq <> io.enqIQData(i + exuParameters.IntExuCnt)})
  fpDispatch.io.readPortIndex <> io.readPortIndex.fpIndex
  
  // Load/store dispatch queue to load/store issue queues
  val lsDispatch = Module(new Dispatch2Ls)
  lsDispatch.io.fromDq <> lsDq.io.deq
  lsDispatch.io.readIntRf.zipWithIndex.map({case (r, i) => r <> io.readIntRf(i + 8)})
  lsDispatch.io.readFpRf.zipWithIndex.map({case (r, i) => r <> io.readFpRf(i + 12)})
  lsDispatch.io.readIntState.zipWithIndex.map({case (r, i) => r <> io.readIntState(i + 8)})
  lsDispatch.io.readFpState.zipWithIndex.map({case (r, i) => r <> io.readFpState(i + 12)})
  lsDispatch.io.numExist.zipWithIndex.map({case (num, i) => num := io.numExist(exuParameters.IntExuCnt + exuParameters.FpExuCnt + i)})
  lsDispatch.io.enqIQCtrl.zipWithIndex.map({case (enq, i) => enq <> io.enqIQCtrl(exuParameters.IntExuCnt + exuParameters.FpExuCnt + i)})
//  lsDispatch.io.enqIQData.zipWithIndex.map({case (enq, i) => enq <> io.enqIQData(exuParameters.IntExuCnt + exuParameters.FpExuCnt + i)})

  io.ctrlInfo <> DontCare
  io.ctrlInfo.intdqFull := intDq.io.dqFull
  io.ctrlInfo.fpdqFull := fpDq.io.dqFull
  io.ctrlInfo.lsdqFull := lsDq.io.dqFull
}
