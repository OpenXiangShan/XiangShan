package xiangshan.backend

import chisel3._
import chisel3.util._
import xiangshan._

class WakeUpBundle(numFast: Int, numSlow: Int) extends XSBundle {
  val fastUops = Vec(numFast, Flipped(ValidIO(new MicroOp)))
  val fast = Vec(numFast, Flipped(DecoupledIO(new ExuOutput))) //one cycle later than fastUops
  val slow = Vec(numSlow, Flipped(DecoupledIO(new ExuOutput)))

  override def cloneType = (new WakeUpBundle(numFast, numSlow)).asInstanceOf[this.type]

}

class IntBlockToCtrlIO extends XSBundle {
  // write back regfile signals after arbiter
  // used to update busytable and roq state
  val wbRegs = Vec(NRIntWritePorts, ValidIO(new ExuOutput))

  // write back to brq
  val exuRedirect = Vec(exuParameters.AluCnt+exuParameters.JmpCnt, ValidIO(new ExuOutput))

  val numExist = Vec(exuParameters.IntExuCnt, Output(UInt(log2Ceil(IssQueSize).W)))
  val sfence = Output(new SfenceBundle)
  val tlbCsrIO = Output(new TlbCsrBundle)
}

class IntegerBlock
(
  fastWakeUpInCnt: Int,
  slowWakeUpInCnt: Int,
  fastFpOutCnt: Int,
  slowFpOutCnt: Int,
  fastIntOutCnt: Int,
  slowIntOutCnt: Int
) extends XSModule with NeedImpl
{
  val io = IO(new Bundle {
    val fromCtrlBlock = Flipped(new CtrlToIntBlockIO)
    val toCtrlBlock = new IntBlockToCtrlIO

    val wakeUpIn = new WakeUpBundle(fastWakeUpInCnt, slowWakeUpInCnt)
    val wakeUpFpOut = Flipped(new WakeUpBundle(fastFpOutCnt, slowFpOutCnt))
    val wakeUpIntOut = Flipped(new WakeUpBundle(fastIntOutCnt, slowIntOutCnt))

    val externalInterrupt = new ExternalInterruptIO
    val sfence = Output(new SfenceBundle)
    val fencei = Output(Bool())
    val tlbCsrIO = Output(new TlbCsrBundle)
    val csrOnly = new CSRSpecialIO
  })
}
