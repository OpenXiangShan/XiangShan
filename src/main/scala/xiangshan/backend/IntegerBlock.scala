package xiangshan.backend

import chisel3._
import chisel3.util._
import xiangshan._

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
  })

  io <> DontCare

}
