/***************************************************************************************
  * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
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

package xiangshan.backend

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import difftest.{DifftestArchFpRegState, DifftestArchIntRegState}
import xiangshan._
import utils._
import xiangshan.backend.issue.ReservationStation
import xiangshan.backend.regfile.Regfile
import xiangshan.mem.{SqPtr, StoreDataBundle}

// TODO: parameters
class Scheduler(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    // global control
    val redirect = Flipped(ValidIO(new Redirect))
    val flush = Input(Bool())
    // dispatch and issue ports
    val allocate = Vec(12, Flipped(DecoupledIO(new MicroOp)))
    // read regfile
    val readIntRf = Vec(NRIntReadPorts, Input(UInt(PhyRegIdxWidth.W)))
    val readFpRf = Vec(NRFpReadPorts, Input(UInt(PhyRegIdxWidth.W)))
    val issue = Vec(17, DecoupledIO(new ExuInput))
    val writeback = Vec(16, Flipped(ValidIO(new ExuOutput)))
    val replay = Vec(4, Flipped(ValidIO(new RSFeedback)))
    val rsIdx = Vec(4, Output(UInt(log2Up(IssQueSize).W)))
    val isFirstIssue = Vec(4, Output(Bool()))
    val stData = Vec(2, ValidIO(new StoreDataBundle))
    // 2LOAD, data is selected from writeback ports
    val otherFastWakeup = Vec(2, Flipped(ValidIO(new MicroOp)))
    // misc
    val jumpPc = Input(UInt(VAddrBits.W))
    val jalr_target = Input(UInt(VAddrBits.W))
    val stIssuePtr = Input(new SqPtr())
    // debug
    val debug_int_rat = Vec(32, Input(UInt(PhyRegIdxWidth.W)))
    val debug_fp_rat = Vec(32, Input(UInt(PhyRegIdxWidth.W)))
  })

  // write ports: 0-3 ALU, 4-5 MUL, 6-7 LOAD
  val intRf = Module(new Regfile(
    numReadPorts = NRIntReadPorts,
    numWirtePorts = NRIntWritePorts,
    hasZero = true,
    len = XLEN
  ))
  // write ports: 0-3 FMA 4-5 FMISC, 6-7 LOAD
  val fpRf = Module(new Regfile(
    numReadPorts = NRFpReadPorts,
    numWirtePorts = NRFpWritePorts,
    hasZero = false,
    len = XLEN
  ))
  io.readIntRf <> intRf.io.readPorts.map(_.addr)
  io.readFpRf <> fpRf.io.readPorts.map(_.addr)

  val jmp_rs = Module(new ReservationStation(JumpExeUnitCfg, IssQueSize, XLEN, 6, 4, -1, false, false, 1, 1))
  val mul_rs_0 = Module(new ReservationStation(MulDivExeUnitCfg, IssQueSize, XLEN, 6, 4, 2, false, false, 2, 1))
  val mul_rs_1 = Module(new ReservationStation(MulDivExeUnitCfg, IssQueSize, XLEN, 6, 4, 2, false, false, 2, 1))
  val alu_rs_0 = Module(new ReservationStation(AluExeUnitCfg, 4*IssQueSize, XLEN,
    8, 4, 0, true, false, 4, 4
  ))

  val fmac_rs0 = Module(new ReservationStation(FmacExeUnitCfg, 4*IssQueSize, XLEN,
    4, 4, fixedDelay = 4, fastWakeup = true, feedback = false,4, 4))
  val fmisc_rs0 = Module(new ReservationStation(FmiscExeUnitCfg, IssQueSize, XLEN,
    4, 4, fixedDelay = -1, fastWakeup = false, feedback = false,2, 1))
  val fmisc_rs1 = Module(new ReservationStation(FmiscExeUnitCfg, IssQueSize, XLEN,
    4, 4, fixedDelay = -1, fastWakeup = false, feedback = false,2, 1))

  val load_rs0 = Module(new ReservationStation(LdExeUnitCfg, IssQueSize, XLEN,
    8, 4, fixedDelay = -1, fastWakeup = false, feedback = true, 1, 1))
  val load_rs1 = Module(new ReservationStation(LdExeUnitCfg, IssQueSize, XLEN,
    8, 4, fixedDelay = -1, fastWakeup = false, feedback = true, 1, 1))
  val store_rs0 = Module(new ReservationStation(StExeUnitCfg, IssQueSize, XLEN,
    6, 12, fixedDelay = -1, fastWakeup = false, feedback = true, 1, 1))
  val store_rs1 = Module(new ReservationStation(StExeUnitCfg, IssQueSize, XLEN,
    6, 12, fixedDelay = -1, fastWakeup = false, feedback = true, 1, 1))

  val intRs = Seq(jmp_rs, mul_rs_0, mul_rs_1, alu_rs_0)
  val fpRs = Seq(fmac_rs0, fmisc_rs0, fmisc_rs1)
  val lsRs = Seq(load_rs0, load_rs1, store_rs0, store_rs1)
  val reservationStations = intRs ++ fpRs ++ lsRs

  for (rs <- reservationStations) {
    rs.io.redirect <> io.redirect
    rs.io.redirect <> io.redirect
    rs.io.flush <> io.flush
  }

  val mulFastData = VecInit(io.writeback.slice(6, 8).map(_.bits.data))
  val aluFastData = VecInit(io.writeback.slice(0, 4).map(_.bits.data))
  val memFastData = VecInit(io.writeback.slice(4, 6).map(_.bits.data))
  val fmaFastData = VecInit(io.writeback.slice(8, 12).map(_.bits.data))

  jmp_rs.io.fromDispatch <> io.allocate.take(1)
  jmp_rs.io.fromDispatch(0).valid := io.allocate(0).valid && FuType.jmpCanAccept(io.allocate(0).bits.ctrl.fuType)
  jmp_rs.io.srcRegValue(0) <> VecInit(intRf.io.readPorts.take(2).map(_.data))
  jmp_rs.io.jumpPc := io.jumpPc
  jmp_rs.io.jalr_target := io.jalr_target
  jmp_rs.io.fastDatas <> mulFastData ++ aluFastData
  jmp_rs.io.deq(0) <> io.issue(0)

  mul_rs_0.io.fromDispatch <> io.allocate.slice(0, 1) ++ io.allocate.slice(2, 3)
  mul_rs_0.io.fromDispatch(0).valid := io.allocate(0).valid && FuType.mduCanAccept(io.allocate(0).bits.ctrl.fuType)
  mul_rs_0.io.fromDispatch(1).valid := io.allocate(2).valid && FuType.mduCanAccept(io.allocate(2).bits.ctrl.fuType)
  mul_rs_0.io.srcRegValue(0) <> VecInit(intRf.io.readPorts.slice(0, 2).map(_.data))
  mul_rs_0.io.srcRegValue(1) <> VecInit(intRf.io.readPorts.slice(4, 6).map(_.data))
  mul_rs_0.io.fastDatas <> mulFastData ++ aluFastData
  mul_rs_0.io.deq(0) <> io.issue(1)

  mul_rs_1.io.fromDispatch <> io.allocate.slice(1, 2) ++ io.allocate.slice(3, 4)
  mul_rs_1.io.fromDispatch(0).valid := io.allocate(1).valid && FuType.mduCanAccept(io.allocate(1).bits.ctrl.fuType)
  mul_rs_1.io.fromDispatch(1).valid := io.allocate(3).valid && FuType.mduCanAccept(io.allocate(3).bits.ctrl.fuType)
  mul_rs_1.io.srcRegValue(0) <> VecInit(intRf.io.readPorts.slice(2, 4).map(_.data))
  mul_rs_1.io.srcRegValue(1) <> VecInit(intRf.io.readPorts.slice(6, 8).map(_.data))
  mul_rs_1.io.fastDatas <> mulFastData ++ aluFastData
  mul_rs_1.io.deq(0) <> io.issue(2)

  alu_rs_0.io.fromDispatch <> VecInit(io.allocate.take(4))
  for (i <- 0 until 4) {
    alu_rs_0.io.fromDispatch(i).valid := io.allocate(i).valid && FuType.aluCanAccept(io.allocate(i).bits.ctrl.fuType)
  }
  alu_rs_0.io.srcRegValue(0) <> VecInit(intRf.io.readPorts.take(2).map(_.data))
  alu_rs_0.io.srcRegValue(1) <> VecInit(intRf.io.readPorts.slice(2, 4).map(_.data))
  alu_rs_0.io.srcRegValue(2) <> VecInit(intRf.io.readPorts.slice(4, 6).map(_.data))
  alu_rs_0.io.srcRegValue(3) <> VecInit(intRf.io.readPorts.slice(6, 8).map(_.data))
  alu_rs_0.io.fastDatas <> mulFastData ++ aluFastData ++ memFastData
  alu_rs_0.io.deq <> io.issue.slice(3, 7)

  io.allocate(0).ready := jmp_rs.io.fromDispatch(0).fire() || mul_rs_0.io.fromDispatch(0).fire() || alu_rs_0.io.fromDispatch(0).fire()
  io.allocate(1).ready := mul_rs_1.io.fromDispatch(0).fire() || alu_rs_0.io.fromDispatch(1).fire()
  io.allocate(2).ready := mul_rs_0.io.fromDispatch(1).fire() || alu_rs_0.io.fromDispatch(2).fire()
  io.allocate(3).ready := mul_rs_1.io.fromDispatch(1).fire() || alu_rs_0.io.fromDispatch(3).fire()

  fmac_rs0.io.fromDispatch <> VecInit(io.allocate.slice(4, 8))
  for (i <- 0 until 4) {
    fmac_rs0.io.fromDispatch(i).valid := io.allocate(i + 4).valid && FuType.fmacCanAccept(io.allocate(i + 4).bits.ctrl.fuType)
    fmac_rs0.io.srcRegValue(i) <> VecInit(fpRf.io.readPorts.slice(3*i, 3*i+3).map(_.data))
  }
  fmac_rs0.io.fastDatas <> fmaFastData
  fmac_rs0.io.deq <> io.issue.slice(7, 11)

  fmisc_rs0.io.fromDispatch <> VecInit(io.allocate.slice(4, 5) ++ io.allocate.slice(6, 7))
  for (i <- 0 until 2) {
    fmisc_rs0.io.fromDispatch(i).valid := io.allocate(i*2+4).valid && FuType.fmiscCanAccept(io.allocate(i*2+4).bits.ctrl.fuType)
  }
  fmisc_rs0.io.srcRegValue(0) <> VecInit(fpRf.io.readPorts.slice(0, 2).map(_.data))
  fmisc_rs0.io.srcRegValue(1) <> VecInit(fpRf.io.readPorts.slice(6, 8).map(_.data))
  fmisc_rs0.io.fastDatas <> fmaFastData
  fmisc_rs0.io.deq <> io.issue.slice(11, 12)

  fmisc_rs1.io.fromDispatch <> VecInit(io.allocate.slice(5, 6) ++ io.allocate.slice(7, 8))
  for (i <- 0 until 2) {
    fmisc_rs1.io.fromDispatch(i).valid := io.allocate(i*2+5).valid && FuType.fmiscCanAccept(io.allocate(i*2+5).bits.ctrl.fuType)
  }
  fmisc_rs1.io.srcRegValue(0) <> VecInit(fpRf.io.readPorts.slice(3, 5).map(_.data))
  fmisc_rs1.io.srcRegValue(1) <> VecInit(fpRf.io.readPorts.slice(9, 11).map(_.data))
  fmisc_rs1.io.fastDatas <> fmaFastData
  fmisc_rs1.io.deq <> io.issue.slice(12, 13)

  io.allocate(4).ready := fmisc_rs0.io.fromDispatch(0).fire() || fmac_rs0.io.fromDispatch(0).fire()
  io.allocate(5).ready := fmisc_rs1.io.fromDispatch(0).fire() || fmac_rs0.io.fromDispatch(1).fire()
  io.allocate(6).ready := fmisc_rs0.io.fromDispatch(1).fire() || fmac_rs0.io.fromDispatch(2).fire()
  io.allocate(7).ready := fmisc_rs1.io.fromDispatch(1).fire() || fmac_rs0.io.fromDispatch(3).fire()

  load_rs0.io.fromDispatch <> io.allocate.slice(8, 9)
  load_rs0.io.srcRegValue(0) <> VecInit(intRf.io.readPorts.slice(8, 9).map(_.data))
  load_rs0.io.fastDatas <> mulFastData ++ aluFastData ++ memFastData
  load_rs0.io.deq <> io.issue.slice(13, 14)

  load_rs1.io.fromDispatch <> io.allocate.slice(9, 10)
  load_rs1.io.srcRegValue(0) <> VecInit(intRf.io.readPorts.slice(9, 10).map(_.data))
  load_rs1.io.fastDatas <> mulFastData ++ aluFastData ++ memFastData
  load_rs1.io.deq <> io.issue.slice(14, 15)

  store_rs0.io.fromDispatch <> io.allocate.slice(10, 11)
  store_rs0.io.srcRegValue(0) <> VecInit(intRf.io.readPorts.slice(10, 12).map(_.data))
  when (RegNext(store_rs0.io.fromDispatch(0).bits.ctrl.srcType(1) === SrcType.fp)) {
    store_rs0.io.srcRegValue(0)(1) := fpRf.io.readPorts(12).data
  }
  store_rs0.io.fastDatas <> mulFastData ++ aluFastData
  store_rs0.io.deq <> io.issue.slice(15, 16)

  store_rs1.io.fromDispatch <> io.allocate.slice(11, 12)
  store_rs1.io.srcRegValue(0) <> VecInit(intRf.io.readPorts.slice(12, 14).map(_.data))
  when (RegNext(store_rs1.io.fromDispatch(0).bits.ctrl.srcType(1) === SrcType.fp)) {
    store_rs1.io.srcRegValue(0)(1) := fpRf.io.readPorts(13).data
  }
  store_rs1.io.fastDatas <> mulFastData ++ aluFastData
  store_rs1.io.deq <> io.issue.slice(16, 17)

  val aluFastUop = alu_rs_0.io.fastUopOut
  val mulFastUop = mul_rs_0.io.fastUopOut ++ mul_rs_1.io.fastUopOut
  val memFastUop = io.otherFastWakeup
  val fmacFastUop = fmac_rs0.io.fastUopOut

  jmp_rs.io.fastUopsIn := mulFastUop ++ aluFastUop
  mul_rs_0.io.fastUopsIn := mulFastUop ++ aluFastUop
  mul_rs_1.io.fastUopsIn := mulFastUop ++ aluFastUop
  alu_rs_0.io.fastUopsIn := mulFastUop ++ aluFastUop ++ memFastUop
  fmac_rs0.io.fastUopsIn := fmacFastUop
  fmisc_rs0.io.fastUopsIn := fmacFastUop
  fmisc_rs1.io.fastUopsIn := fmacFastUop
  load_rs0.io.fastUopsIn := mulFastUop ++ aluFastUop ++ memFastUop
  load_rs1.io.fastUopsIn := mulFastUop ++ aluFastUop ++ memFastUop
  store_rs0.io.fastUopsIn := mulFastUop ++ aluFastUop
  store_rs1.io.fastUopsIn := mulFastUop ++ aluFastUop

  jmp_rs.io.slowPorts := io.writeback.slice(4, 8)
  mul_rs_0.io.slowPorts := io.writeback.slice(4, 8)
  mul_rs_1.io.slowPorts := io.writeback.slice(4, 8)
  alu_rs_0.io.slowPorts := io.writeback.slice(4, 8)
  fmac_rs0.io.slowPorts := io.writeback.drop(12)
  fmisc_rs0.io.slowPorts := io.writeback.drop(12)
  fmisc_rs1.io.slowPorts := io.writeback.drop(12)
  load_rs0.io.slowPorts := io.writeback.slice(4, 8)
  load_rs1.io.slowPorts := io.writeback.slice(4, 8)
  store_rs0.io.slowPorts := io.writeback.drop(4)
  store_rs1.io.slowPorts := io.writeback.drop(4)

  // load-store specific connections
  load_rs0.io.memfeedback <> io.replay(0)
  load_rs1.io.memfeedback <> io.replay(1)
  store_rs0.io.memfeedback <> io.replay(2)
  store_rs1.io.memfeedback <> io.replay(3)
  load_rs0.io.rsIdx <> io.rsIdx(0)
  load_rs1.io.rsIdx <> io.rsIdx(1)
  store_rs0.io.rsIdx <> io.rsIdx(2)
  store_rs1.io.rsIdx <> io.rsIdx(3)
  load_rs0.io.isFirstIssue <> io.isFirstIssue(0)
  load_rs1.io.isFirstIssue <> io.isFirstIssue(1)
  store_rs0.io.isFirstIssue <> io.isFirstIssue(2)
  store_rs1.io.isFirstIssue <> io.isFirstIssue(3)
  store_rs0.io.stData <> io.stData(0)
  store_rs1.io.stData <> io.stData(1)
  store_rs0.io.stIssuePtr <> io.stIssuePtr
  store_rs1.io.stIssuePtr <> io.stIssuePtr
  load_rs0.io.stIssuePtr <> io.stIssuePtr
  load_rs1.io.stIssuePtr <> io.stIssuePtr

  // regfile write ports
  intRf.io.writePorts.zip(io.writeback.take(8)).foreach {
    case (rf, wb) =>
      rf.wen := wb.valid && wb.bits.uop.ctrl.rfWen
      rf.addr := wb.bits.uop.pdest
      rf.data := wb.bits.data
  }
  fpRf.io.writePorts.zip(io.writeback.drop(8)).foreach{
    case (rf, wb) =>
      rf.wen := wb.valid
      rf.addr := wb.bits.uop.pdest
      rf.data := wb.bits.data
  }

  intRf.io.debug_rports := DontCare
  fpRf.io.debug_rports := DontCare
  if (!env.FPGAPlatform) {
    for ((rport, rat) <- intRf.io.debug_rports.zip(io.debug_int_rat)) {
      rport.addr := rat
    }
    val difftest = Module(new DifftestArchIntRegState)
    difftest.io.clock  := clock
    difftest.io.coreid := hardId.U
    difftest.io.gpr    := VecInit(intRf.io.debug_rports.map(_.data))
  }

  if (!env.FPGAPlatform) {
    for ((rport, rat) <- fpRf.io.debug_rports.zip(io.debug_fp_rat)) {
      rport.addr := rat
    }
    val difftest = Module(new DifftestArchFpRegState)
    difftest.io.clock  := clock
    difftest.io.coreid := hardId.U
    difftest.io.fpr    := VecInit(fpRf.io.debug_rports.map(_.data))
  }

}
