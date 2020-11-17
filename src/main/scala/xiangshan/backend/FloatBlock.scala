package xiangshan.backend

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.regfile.Regfile
import xiangshan.backend.exu._
import xiangshan.backend.issue.ReservationStationNew


class FpBlockToCtrlIO extends XSBundle {
  val wbRegs = Vec(NRFpWritePorts, ValidIO(new ExuOutput))
  val numExist = Vec(exuParameters.FpExuCnt, Output(UInt(log2Ceil(IssQueSize).W)))
}

class FloatBlock
(
  fastWakeUpIn: Seq[ExuConfig],
  slowWakeUpIn: Seq[ExuConfig],
  fastFpOut: Seq[ExuConfig],
  slowFpOut: Seq[ExuConfig],
  fastIntOut: Seq[ExuConfig],
  slowIntOut: Seq[ExuConfig]
) extends XSModule with NeedImpl {
  val io = IO(new Bundle {
    val fromCtrlBlock = Flipped(new CtrlToFpBlockIO)
    val toCtrlBlock = new FpBlockToCtrlIO

    val wakeUpIn = new WakeUpBundle(fastWakeUpIn.size, slowWakeUpIn.size)
    val wakeUpFpOut = Flipped(new WakeUpBundle(fastFpOut.size, slowFpOut.size))
    val wakeUpIntOut = Flipped(new WakeUpBundle(fastIntOut.size, slowIntOut.size))
  })
}
