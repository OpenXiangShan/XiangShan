package xiangshan.backend

import chisel3._
import chisel3.util._
import xiangshan._


class LsBlockToCtrlIO extends XSBundle {
  // TODO: should not be LsExuCnt
  val wbIntRegs = Vec(exuParameters.LsExuCnt, Flipped(ValidIO(new ExuOutput)))
  val wbFpRegs = Vec(exuParameters.LsExuCnt, Flipped(ValidIO(new ExuOutput)))
  val numExist = Vec(exuParameters.LsExuCnt, Output(UInt(log2Ceil(IssQueSize).W)))
  val lsqIdxResp = Vec(RenameWidth, Output(new LSIdx))
  val oldestStore = Output(Valid(new RoqPtr))
  val replay = ValidIO(new Redirect)
}

class MemBlock extends XSModule {
  val io = IO(new Bundle {
    val fromCtrlBlock = Flipped(new CtrlToLsBlockIO)
    val toCtrlBlock = new LsBlockToCtrlIO
  })

  io <> DontCare

}
