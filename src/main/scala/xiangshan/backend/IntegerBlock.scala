package xiangshan.backend

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.regfile.Regfile
import xiangshan.backend.exu._


// wbIntRegs,wbFpRegs are used for updating busytables
class IntBlockToCtrlIO extends XSBundle {
  // TODO: should not be IntExuCnt
  val wbIntRegs = Vec(exuParameters.IntExuCnt, Flipped(ValidIO(new ExuOutput)))
  val wbFpRegs = Vec(exuParameters.IntExuCnt, Flipped(ValidIO(new ExuOutput)))
  val numExist = Vec(exuParameters.IntExuCnt, Output(UInt(log2Ceil(IssQueSize).W)))
}

class IntegerBlock extends XSModule {
  val io = IO(new Bundle {
    val fromCtrlBlock = Flipped(new CtrlToIntBlockIO)
    val toCtrlBlock = new IntBlockToCtrlIO
    // val writebackFromFpLs = 
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

  // connect writeback
  // val wbArbiter = 
}
