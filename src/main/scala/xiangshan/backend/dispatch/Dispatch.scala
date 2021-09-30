/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package xiangshan.backend.dispatch

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.backend.regfile.RfReadPort
import xiangshan.backend.rob.{RobPtr, RobEnqIO}
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
    // enq Rob
    val enqRob = Flipped(new RobEnqIO)
    // enq Lsq
    val enqLsq = Flipped(new LsqEnqIO)
    // read regfile
    val readIntRf = Vec(NRIntReadPorts, Output(UInt(PhyRegIdxWidth.W)))
    val readFpRf = Vec(NRFpReadPorts, Output(UInt(PhyRegIdxWidth.W)))
    // to busytable: read physical registers' state (busy/ready)
    val readIntState= Vec(NRIntReadPorts, Flipped(new BusyTableReadIO))
    val readFpState = Vec(NRFpReadPorts, Flipped(new BusyTableReadIO))
    // to reservation stations
    val enqIQCtrl = Vec(exuParameters.CriticalExuCnt, DecoupledIO(new MicroOp))
    // send reg file read port index to reservation stations
    val csrCtrl = Input(new CustomCSRCtrlIO)
    // LFST state sync
    val storeIssue = Vec(StorePipelineWidth, Flipped(Valid(new ExuInput)))
    val ctrlInfo = new Bundle {
      val robFull   = Output(Bool())
      val intdqFull = Output(Bool())
      val fpdqFull  = Output(Bool())
      val lsdqFull  = Output(Bool())
    }
    // From CSR: to control single step execution
    val singleStep = Input(Bool())
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
  dispatch1.io.enqRob <> io.enqRob
  dispatch1.io.enqLsq <> io.enqLsq
  dispatch1.io.toIntDq <> intDq.io.enq
  dispatch1.io.toFpDq <> fpDq.io.enq
  dispatch1.io.toLsDq <> lsDq.io.enq
  dispatch1.io.allocPregs <> io.allocPregs
  dispatch1.io.csrCtrl <> io.csrCtrl
  dispatch1.io.storeIssue <> io.storeIssue
  dispatch1.io.redirect <> io.redirect
  dispatch1.io.flush <> io.flush
  dispatch1.io.singleStep := io.singleStep

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

  // Fp dispatch queue to Fp reservation stations
  val fpDispatch = Module(new Dispatch2Fp)
  fpDispatch.io.fromDq <> fpDq.io.deq

  // Load/store dispatch queue to load/store issue queues
  val lsDispatch = Module(new Dispatch2Ls)
  lsDispatch.io.fromDq <> lsDq.io.deq

  io.enqIQCtrl <> intDispatch.io.enqIQCtrl ++ fpDispatch.io.enqIQCtrl ++ lsDispatch.io.enqIQCtrl
  io.readIntRf <> intDispatch.io.readRf ++ lsDispatch.io.readIntRf
  io.readIntState <> intDispatch.io.readState ++ lsDispatch.io.readIntState
  io.readFpRf <> fpDispatch.io.readRf ++ lsDispatch.io.readFpRf
  io.readFpState <> fpDispatch.io.readState ++ lsDispatch.io.readFpState

  io.ctrlInfo <> DontCare
  io.ctrlInfo.intdqFull := intDq.io.dqFull
  io.ctrlInfo.fpdqFull := fpDq.io.dqFull
  io.ctrlInfo.lsdqFull := lsDq.io.dqFull

  val enableDetailedRegfilePortsPerf = true
  val intPortsNeeded = intDispatch.io.enqIQCtrl.map(enq => PopCount((0 until 2).map(i => enq.bits.needRfRPort(i, 0))))
  val fpPortsNeeded = fpDispatch.io.enqIQCtrl.map(enq => PopCount((0 until 3).map(i => enq.bits.needRfRPort(i, 1))))
  val lsPortsNeededInt = lsDispatch.io.enqIQCtrl.map(enq => PopCount((0 until 2).map(i => enq.bits.needRfRPort(i, 0))))
  val lsPortsNeededFp = lsDispatch.io.enqIQCtrl.map(enq => PopCount((0 until 2).map(i => enq.bits.needRfRPort(i, 1))))
  def get_active_ports(enq: Seq[Bool], ports: Seq[UInt]) = {
    enq.zip(ports).map{ case (e, p) => Mux(e, p, 0.U)}.reduce(_ +& _)
  }
  val intActivePorts = get_active_ports(intDispatch.io.enqIQCtrl.map(_.valid), intPortsNeeded)
  val fpActivePorts = get_active_ports(fpDispatch.io.enqIQCtrl.map(_.valid), fpPortsNeeded)
  val lsActivePortsInt = get_active_ports(lsDispatch.io.enqIQCtrl.map(_.valid), lsPortsNeededInt)
  val lsActivePortsFp = get_active_ports(lsDispatch.io.enqIQCtrl.map(_.valid), lsPortsNeededFp)
  val activePortsIntAll = intActivePorts + lsActivePortsInt
  val activePortsFpAll = fpActivePorts + lsActivePortsFp
  XSPerfAccumulate("int_rf_active_ports_int", intActivePorts)
  XSPerfAccumulate("int_rf_active_ports_ls", lsActivePortsInt)
  XSPerfAccumulate("int_rf_active_ports_all", activePortsIntAll)
  XSPerfAccumulate("fp_rf_active_ports_fp", fpActivePorts)
  XSPerfAccumulate("fp_rf_active_ports_ls", lsActivePortsFp)
  XSPerfAccumulate("fp_rf_active_ports_all", activePortsFpAll)
  if (enableDetailedRegfilePortsPerf) {
    XSPerfHistogram("int_rf_active_ports_all", activePortsIntAll, true.B, 0, 14+1, 1)
    XSPerfHistogram("fp_rf_active_ports_all", activePortsFpAll, true.B, 0, 14+1, 1)
    XSPerfHistogram("int_rf_active_ports_int", intActivePorts, true.B, 0, 8+1, 1)
    XSPerfHistogram("int_rf_active_ports_ls", lsActivePortsInt, true.B, 0, 6+1, 1)
    XSPerfHistogram("fp_rf_active_ports_fp", fpActivePorts, true.B, 0, 12+1, 1)
    XSPerfHistogram("fp_rf_active_ports_ls", lsActivePortsFp, true.B, 0, 2+1, 1)
  }
}
