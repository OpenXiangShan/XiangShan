package xiangshan.backend

import chisel3._
import chisel3.util._
import xiangshan._


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
  })

  io <> DontCare

}
