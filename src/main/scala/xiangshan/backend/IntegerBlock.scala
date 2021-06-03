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

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import xiangshan._
import xiangshan.backend.exu._
import xiangshan.backend.issue.ReservationStation
import xiangshan.backend.fu.{FenceToSbuffer, CSRFileIO, FunctionUnit}
import xiangshan.backend.regfile.Regfile
import difftest._

class WakeUpBundle(numFast: Int, numSlow: Int)(implicit p: Parameters) extends XSBundle {
  val fastUops = Vec(numFast, Flipped(ValidIO(new MicroOp)))
  val fast = Vec(numFast, Flipped(ValidIO(new ExuOutput))) //one cycle later than fastUops
  val slow = Vec(numSlow, Flipped(DecoupledIO(new ExuOutput)))

  override def cloneType = (new WakeUpBundle(numFast, numSlow)).asInstanceOf[this.type]

}

class IntBlockToCtrlIO(implicit p: Parameters) extends XSBundle {
  // write back regfile signals after arbiter
  // used to update busytable and roq state
  val wbRegs = Vec(NRIntWritePorts, ValidIO(new ExuOutput))
  // write back to brq
  val exuRedirect = Vec(exuParameters.AluCnt + exuParameters.JmpCnt, ValidIO(new ExuOutput))
  val numExist = Vec(exuParameters.IntExuCnt, Output(UInt(log2Ceil(IssQueSize).W)))
}

trait HasExeBlockHelper {
  def fpUopValid(x: ValidIO[MicroOp]): ValidIO[MicroOp] = {
    val uop = WireInit(x)
    uop.valid := x.valid && x.bits.ctrl.fpWen
    uop
  }
  def fpOutValid(x: ValidIO[ExuOutput]): ValidIO[ExuOutput] = {
    val out = WireInit(x)
    out.valid := x.valid && x.bits.uop.ctrl.fpWen
    out
  }
  def fpOutValid(x: DecoupledIO[ExuOutput], connectReady: Boolean = false): DecoupledIO[ExuOutput] = {
    val out = WireInit(x)
    if(connectReady) x.ready := out.ready
    out.valid := x.valid && x.bits.uop.ctrl.fpWen
    out
  }
  def intUopValid(x: ValidIO[MicroOp]): ValidIO[MicroOp] = {
    val uop = WireInit(x)
    uop.valid := x.valid && x.bits.ctrl.rfWen
    uop
  }
  def intOutValid(x: ValidIO[ExuOutput]): ValidIO[ExuOutput] = {
    val out = WireInit(x)
    out.valid := x.valid && !x.bits.uop.ctrl.fpWen
    out
  }
  def intOutValid(x: DecoupledIO[ExuOutput], connectReady: Boolean = false): DecoupledIO[ExuOutput] = {
    val out = WireInit(x)
    if(connectReady) x.ready := out.ready
    out.valid := x.valid && !x.bits.uop.ctrl.fpWen
    out
  }
  def decoupledIOToValidIO[T <: Data](d: DecoupledIO[T]): Valid[T] = {
    val v = Wire(Valid(d.bits.cloneType))
    v.valid := d.valid
    v.bits := d.bits
    v
  }

  def validIOToDecoupledIO[T <: Data](v: Valid[T]): DecoupledIO[T] = {
    val d = Wire(DecoupledIO(v.bits.cloneType))
    d.valid := v.valid
    d.ready := true.B
    d.bits := v.bits
    d
  }
}

class IntegerBlock
(
  fastWakeUpIn: Seq[ExuConfig],
  slowWakeUpIn: Seq[ExuConfig],
  memFastWakeUpIn: Seq[ExuConfig],
  fastWakeUpOut: Seq[ExuConfig],
  slowWakeUpOut: Seq[ExuConfig]
)(implicit p: Parameters) extends XSModule with HasExeBlockHelper {
  val io = IO(new Bundle {
    val fromCtrlBlock = Flipped(new CtrlToIntBlockIO)
    val toCtrlBlock = new IntBlockToCtrlIO
    val toMemBlock = new IntBlockToMemBlockIO

    val wakeUpIn = new WakeUpBundle(fastWakeUpIn.size, slowWakeUpIn.size)
    val wakeUpOut = Flipped(new WakeUpBundle(fastWakeUpOut.size, slowWakeUpOut.size))
    val memFastWakeUp = new WakeUpBundle(exuParameters.LduCnt, 0)
    val intWbOut = Vec(4, ValidIO(new ExuOutput))

    val csrio = new CSRFileIO
    val fenceio = new Bundle {
      val sfence = Output(new SfenceBundle) // to front,mem
      val fencei = Output(Bool()) // to icache
      val sbuffer = new FenceToSbuffer // to mem
    }
  })
  val redirect = io.fromCtrlBlock.redirect
  val flush = io.fromCtrlBlock.flush

  val intRf = Module(new Regfile(
    numReadPorts = NRIntReadPorts,
    numWirtePorts = NRIntWritePorts,
    hasZero = true,
    len = XLEN
  ))

  val jmpExeUnit = Module(new JumpExeUnit)
  val mduExeUnits = Array.tabulate(exuParameters.MduCnt)(_ => Module(new MulDivExeUnit))
  val aluExeUnits = Array.tabulate(exuParameters.AluCnt)(_ => Module(new AluExeUnit))

  val exeUnits = jmpExeUnit +: (mduExeUnits ++ aluExeUnits)
  val intWbArbiter = Module(new Wb(
    (exeUnits.map(_.config) ++ fastWakeUpIn ++ slowWakeUpIn),
    NRIntWritePorts,
    isFp = false
  ))
  io.intWbOut := VecInit(intWbArbiter.io.out.drop(4))
  for (exe <- exeUnits) {
    exe.io.redirect <> redirect
    exe.io.flush <> flush
  }

  val jmp_rs = Module(new ReservationStation("rs_jmp", JumpExeUnitCfg, IssQueSize, XLEN, 6, 4, -1, false, false, 1, 1))
  val mul_rs_0 = Module(new ReservationStation("rs_mul_0", MulDivExeUnitCfg, IssQueSize, XLEN, 6, 4, 2, false, false, 1, 1))
  val mul_rs_1 = Module(new ReservationStation("rs_mul_1", MulDivExeUnitCfg, IssQueSize, XLEN, 6, 4, 2, false, false, 1, 1))
  val alu_rs_0 = Module(new ReservationStation("rs_alu_0", AluExeUnitCfg, 2*IssQueSize, XLEN,
    8, 4, 0, true, false, 2, 2
  ))
  val alu_rs_1 = Module(new ReservationStation("rs_alu_1", AluExeUnitCfg, 2*IssQueSize, XLEN,
    8, 4, 0, true, false, 2, 2
  ))

  val aluFastData = VecInit(exeUnits.drop(3).map(_.io.out.bits.data))
  val mulFastData = VecInit(exeUnits.drop(1).take(2).map(_.io.out.bits.data))
  val memFastData = VecInit(io.memFastWakeUp.fast.map(_.bits.data))
  val slowPorts = intWbArbiter.io.out.drop(4)

  jmp_rs.io.numExist <> io.toCtrlBlock.numExist(0)
  jmp_rs.io.fromDispatch <> VecInit(io.fromCtrlBlock.enqIqCtrl(0))
  jmp_rs.io.srcRegValue(0) <> VecInit(intRf.io.readPorts.drop(2).take(2).map(_.data))
  jmp_rs.io.jumpPc := io.fromCtrlBlock.jumpPc
  jmp_rs.io.jalr_target := io.fromCtrlBlock.jalr_target
  jmp_rs.io.fastDatas <> mulFastData ++ aluFastData
  jmp_rs.io.deq(0) <> jmpExeUnit.io.fromInt

  mul_rs_0.io.numExist <> io.toCtrlBlock.numExist(1)
  mul_rs_0.io.fromDispatch <> VecInit(io.fromCtrlBlock.enqIqCtrl(1))
  mul_rs_0.io.srcRegValue(0) <> VecInit(intRf.io.readPorts.drop(4).take(2).map(_.data))
  mul_rs_0.io.fastDatas <> mulFastData ++ aluFastData
  mul_rs_0.io.deq(0) <> mduExeUnits(0).io.fromInt

  mul_rs_1.io.numExist <> io.toCtrlBlock.numExist(2)
  mul_rs_1.io.fromDispatch <> VecInit(io.fromCtrlBlock.enqIqCtrl(2))
  mul_rs_1.io.srcRegValue(0) <> VecInit(intRf.io.readPorts.drop(6).take(2).map(_.data))
  mul_rs_1.io.fastDatas <> mulFastData ++ aluFastData
  mul_rs_1.io.deq(0) <> mduExeUnits(1).io.fromInt

  io.toCtrlBlock.numExist(3) := alu_rs_0.io.numExist >> 1
  io.toCtrlBlock.numExist(4) := alu_rs_0.io.numExist >> 1
  alu_rs_0.io.fromDispatch <> VecInit(io.fromCtrlBlock.enqIqCtrl.drop(3).take(2))
  alu_rs_0.io.srcRegValue(0) <> VecInit(intRf.io.readPorts.take(2).map(_.data))
  alu_rs_0.io.srcRegValue(1) <> VecInit(intRf.io.readPorts.drop(2).take(2).map(_.data))
  alu_rs_0.io.fastDatas <> mulFastData ++ aluFastData ++ memFastData
  alu_rs_0.io.deq(0) <> aluExeUnits(0).io.fromInt
  alu_rs_0.io.deq(1) <> aluExeUnits(1).io.fromInt

  io.toCtrlBlock.numExist(5) := alu_rs_1.io.numExist >> 1
  io.toCtrlBlock.numExist(6) := alu_rs_1.io.numExist >> 1
  alu_rs_1.io.fromDispatch <> VecInit(io.fromCtrlBlock.enqIqCtrl.drop(5))
  alu_rs_1.io.srcRegValue(0) <> VecInit(intRf.io.readPorts.drop(4).take(2).map(_.data))
  alu_rs_1.io.srcRegValue(1) <> VecInit(intRf.io.readPorts.drop(6).take(2).map(_.data))
  alu_rs_1.io.fastDatas <> mulFastData ++ aluFastData ++ memFastData
  alu_rs_1.io.deq(0) <> aluExeUnits(2).io.fromInt
  alu_rs_1.io.deq(1) <> aluExeUnits(3).io.fromInt

  val reservationStations = Seq(jmp_rs, mul_rs_0, mul_rs_1, alu_rs_0, alu_rs_1)
  val aluFastUop = Wire(Vec(4, ValidIO(new MicroOp)))
  val mulFastUop = Wire(Vec(2, ValidIO(new MicroOp)))
  val memFastUop = io.memFastWakeUp.fastUops
  aluFastUop(0) := alu_rs_0.io.fastUopOut(0)
  aluFastUop(1) := alu_rs_0.io.fastUopOut(1)
  aluFastUop(2) := alu_rs_1.io.fastUopOut(0)
  aluFastUop(3) := alu_rs_1.io.fastUopOut(1)
  mulFastUop(0) := mul_rs_0.io.fastUopOut(0)
  mulFastUop(1) := mul_rs_1.io.fastUopOut(0)

  for (rs <- reservationStations) {
    rs.io.redirect <> redirect
    rs.io.redirect <> redirect
    rs.io.flush <> flush
    rs.io.slowPorts := slowPorts
  }
  jmp_rs.io.fastUopsIn := mulFastUop ++ aluFastUop
  mul_rs_0.io.fastUopsIn := mulFastUop ++ aluFastUop
  mul_rs_1.io.fastUopsIn := mulFastUop ++ aluFastUop
  alu_rs_0.io.fastUopsIn := mulFastUop ++ aluFastUop ++ memFastUop
  alu_rs_1.io.fastUopsIn := mulFastUop ++ aluFastUop ++ memFastUop

  io.wakeUpOut.fastUops := mulFastUop ++ aluFastUop

  io.wakeUpOut.fast <> exeUnits.filter(
    x => x.config.hasCertainLatency
  ).map(_.io.out).map(decoupledIOToValidIO)

  io.wakeUpOut.slow <> exeUnits.filter(
    x => x.config.hasUncertainlatency
  ).map(x => WireInit(x.io.out))

  // send misprediction to brq
  io.toCtrlBlock.exuRedirect.zip(
    exeUnits.filter(_.config.hasRedirect).map(_.io.out)
  ).foreach {
    case (x, y) =>
      x.valid := y.fire() && y.bits.redirectValid
      x.bits := y.bits
  }

  jmpExeUnit.csrio <> io.csrio
  jmpExeUnit.csrio.perf <> RegNext(io.csrio.perf)
  // RegNext customCtrl for better timing
  io.csrio.customCtrl := RegNext(jmpExeUnit.csrio.customCtrl)
  jmpExeUnit.fenceio <> io.fenceio

  // read int rf from ctrl block
  intRf.io.readPorts.zipWithIndex.map { case (r, i) => r.addr := io.fromCtrlBlock.readRf(i) }
  (0 until NRMemReadPorts).foreach(i => io.toMemBlock.readIntRf(i).data := intRf.io.readPorts(i + 8).data)
  // write int rf arbiter

  intWbArbiter.io.in <> exeUnits.map(e => {
    val w = WireInit(e.io.out)
    if(e.config.writeFpRf){
      w.valid := e.io.out.valid && !e.io.out.bits.uop.ctrl.fpWen && io.wakeUpOut.slow(0).ready
    } else {
      w.valid := e.io.out.valid
    }
    w
  }) ++ io.wakeUpIn.slow.map(x => intOutValid(x, connectReady = true))

  XSPerfAccumulate("competition", intWbArbiter.io.in.map(i => !i.ready && i.valid).foldRight(0.U)(_+_))

  exeUnits.zip(intWbArbiter.io.in).foreach{
    case (exu, wInt) =>
      if(exu.config.writeFpRf){
        val wakeUpOut = io.wakeUpOut.slow(0) // jmpExeUnit
        val writeFpReady = wakeUpOut.fire() && wakeUpOut.bits.uop.ctrl.fpWen
        exu.io.out.ready := wInt.fire() || writeFpReady || !exu.io.out.valid
      } else {
        exu.io.out.ready := wInt.fire() || !exu.io.out.valid
      }
  }

  // set busytable and update roq
  io.toCtrlBlock.wbRegs <> intWbArbiter.io.out

  intRf.io.writePorts.zip(intWbArbiter.io.out).foreach {
    case (rf, wb) =>
      rf.wen := wb.valid && wb.bits.uop.ctrl.rfWen
      rf.addr := wb.bits.uop.pdest
      rf.data := wb.bits.data
  }
  intRf.io.debug_rports := DontCare

  if (!env.FPGAPlatform) {
    for ((rport, rat) <- intRf.io.debug_rports.zip(io.fromCtrlBlock.debug_rat)) {
      rport.addr := rat
    }
    val difftest = Module(new DifftestArchIntRegState)
    difftest.io.clock  := clock
    difftest.io.coreid := hardId.U
    difftest.io.gpr    := VecInit(intRf.io.debug_rports.map(_.data))
  }

  val rsDeqCount = PopCount(reservationStations.map(_.io.deq(0).valid))
  XSPerfAccumulate("int_rs_deq_count", rsDeqCount)
  XSPerfHistogram("int_rs_deq_count", rsDeqCount, true.B, 0, 7, 1)
}
