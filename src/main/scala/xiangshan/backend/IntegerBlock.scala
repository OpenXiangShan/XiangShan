package xiangshan.backend

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.regfile.Regfile
import xiangshan.backend.exu._
import xiangshan.backend.issue.ReservationStationNew

// wbIntRegs,wbFpRegs are used for updating busytables
class IntBlockToCtrlIO extends XSBundle {
  // TODO: should not be IntExuCnt
  val wbIntRegs = Vec(exuParameters.IntExuCnt, Flipped(ValidIO(new ExuOutput)))
  val wbFpRegs = Vec(exuParameters.IntExuCnt, Flipped(ValidIO(new ExuOutput)))
  val numExist = Vec(exuParameters.IntExuCnt, Output(UInt(log2Ceil(IssQueSize).W)))
  val sfence = Output(new SfenceBundle)
  val tlbCsrIO = Output(new TlbCsrBundle)
}

class IntegerBlock extends XSModule {
  val io = IO(new Bundle {
    val fromCtrlBlock = Flipped(new CtrlToIntBlockIO)
    val toCtrlBlock = new IntBlockToCtrlIO
    // TODO: ramdonly set 5
    val writebackData = Vec(5, Input(UInt(XLEN.W)))
    val extraListenPorts = Vec(5, Flipped(DecoupledIO(new ExuOutput)))
  })

  // integer regfile
  val regfile = Module(new Regfile(
    numReadPorts = NRIntReadPorts,
    numWirtePorts = NRIntWritePorts,
    hasZero = true
  ))

  val jmpExeUnit = Module(new JumpExeUnit)
  val mduExeUnits = Array.tabulate(exuParameters.MduCnt)(_ => Module(new MulDivExeUnit))
  val aluExeUnits = Array.tabulate(exuParameters.AluCnt)(_ => Module(new AluExeUnit))
  val exeUnits = jmpExeUnit +: (mduExeUnits ++ aluExeUnits)
  val exuConfigs = exeUnits.map(_.config)

  // generate reservation stations
  val exeWbReqs = exeUnits.map(_.io.out)
  val writebackData = exuConfigs.zip(exeWbReqs).filter(x => x._1.hasCertainLatency && x._1.writeIntRf).map(_._2.bits.data)
  val extraListenPorts = exuConfigs.zip(exeWbReqs).filter(x => x._1.hasUncertainlatency && x._1.writeIntRf).map(_._2)

  val rsConfigs = Seq(0, -1, -1, 0, 0, 0, 0)

  val reservationStations  = exuConfigs.zipWithIndex.map({ case (cfg, i) =>
    val rs = Module(new ReservationStationNew(cfg, 5, 6, fixedDelay = rsConfigs(i), feedback = false))

    rs.io.redirect <> io.fromCtrlBlock.redirect
    rs.io.numExist <> io.toCtrlBlock.numExist(i)
    rs.io.enqCtrl <> io.fromCtrlBlock.enqIqCtrl(i)
    rs.io.enqData <> io.fromCtrlBlock.enqIqData(i)

    rs.io.writeBackedData <> writebackData ++ io.writebackData
    for((x, y) <- rs.io.extraListenPorts.zip(extraListenPorts ++ io.extraListenPorts)){
      x.valid := y.fire()
      x.bits := y.bits
    }

    exeUnits(i).io.in <> rs.io.deq
    exeUnits(i).io.redirect <> io.fromCtrlBlock.redirect
    rs.io.tlbFeedback := DontCare

    rs.suggestName(s"rs_${cfg.name}")
    rs
  })

  // TODO: connect writeback
  // val wbArbiter = 
}
