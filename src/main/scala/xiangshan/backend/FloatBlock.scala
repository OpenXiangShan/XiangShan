package xiangshan.backend

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.regfile.Regfile
import xiangshan.backend.exu._


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
    // val writebackFromFpLs = 
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

  // connect writeback
  // val wbArbiter = 

}
