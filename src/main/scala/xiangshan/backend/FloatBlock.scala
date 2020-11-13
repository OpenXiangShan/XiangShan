package xiangshan.backend

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.regfile.Regfile
import xiangshan.backend.exu._
import xiangshan.backend.issue.ReservationStationNew


class FpBlockToCtrlIO extends XSBundle {
  // TODO: should not be FpExuCnt
  val wbIntRegs = Vec(exuParameters.FpExuCnt, Flipped(ValidIO(new ExuOutput)))
  val wbFpRegs = Vec(exuParameters.FpExuCnt, Flipped(ValidIO(new ExuOutput)))
  val numExist = Vec(exuParameters.FpExuCnt, Output(UInt(log2Ceil(IssQueSize).W)))
}

class FloatBlock extends XSModule {
  val io = IO(new Bundle {
    val fromCtrlBlock = Flipped(new CtrlToFpBlockIO)
    val toCtrlBlock = new FpBlockToCtrlIO
    // TODO: ramdonly set 5
    // writeback from other blocks
    val writebackData = Vec(5, Input(UInt(XLEN.W)))
    val extraListenPorts = Vec(5, Flipped(DecoupledIO(new ExuOutput)))
    // output writeback (wakeup other blocks)
    // val 
  })

  // floating-point regfile
  val regfile = Module(new Regfile(
    numReadPorts = NRFpReadPorts,
    numWirtePorts = NRFpWritePorts,
    hasZero = false
  ))

  val fmacExeUnits = Array.tabulate(exuParameters.FmacCnt)(_ => Module(new FmacExeUnit))
  val fmiscExeUnits = Array.tabulate(exuParameters.FmiscCnt)(_ => Module(new FmiscExeUnit))
  val exeUnits = fmacExeUnits ++ fmiscExeUnits
  val exuConfigs = exeUnits.map(_.config)

  // generate reservation stations
  val exeWbReqs = exeUnits.map(_.io.out)
  val writebackData = exuConfigs.zip(exeWbReqs).filter(x => x._1.hasCertainLatency && x._1.writeIntRf).map(_._2.bits.data)
  val extraListenPorts = exuConfigs.zip(exeWbReqs).filter(x => x._1.hasUncertainlatency && x._1.writeIntRf).map(_._2)

  val rsConfigs = Seq(5, 5, 5, 5, -1, -1)
  val reservationStations = exuConfigs.zipWithIndex.map({ case (cfg, i) =>
    val rs = Module(new ReservationStationNew(cfg, 5, 4, fixedDelay = rsConfigs(i), feedback = true))

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

  // connect writeback
  // val wbArbiter = 

}
