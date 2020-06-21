package xiangshan.backend.issue

import chisel3._
import chisel3.util._
import xiangshan._

trait IQConst{
  val iqSize = 4

}

class IssueQueue(val fuTypeInt: BigInt, wakeupCnt: Int, val bypassCnt: Int) extends XSModule with NeedImpl {

  val useBypass = bypassCnt > 0

  val io = IO(new Bundle() {
    // flush Issue Queue
    val redirect = Flipped(ValidIO(new Redirect))

    // enq Ctrl sigs at dispatch-2
    val enqCtrl = Flipped(DecoupledIO(new MicroOp))
    // enq Data at next cycle (regfile has 1 cycle latency)
    val enqData = Flipped(ValidIO(new ExuInput))

    //  broadcast selected uop to other issue queues which has bypasses
    val selectedUop = if(useBypass) DecoupledIO(new MicroOp) else null

    // send to exu
    val deq = DecoupledIO(new ExuInput)

    // listen to write back bus
    val wakeUpPorts = Vec(wakeupCnt, Flipped(DecoupledIO(new ExuOutput)))

    // use bypass uops to speculative wake-up
    val bypassUops = if(useBypass) Vec(bypassCnt, Flipped(DecoupledIO(new MicroOp))) else null
    val bypassData = if(useBypass) Vec(bypassCnt, Flipped(DecoupledIO(new ExuOutput))) else null
  })
  //---------------------------------------------------------
  // Issue Queue
  //---------------------------------------------------------
  val valid   = RegInit(VecInit(Seq.fill(iqSize)(false.B)))
  val src1Rdy = RegInit(VecInit(Seq.fill(iqSize)(false.B)))
  val src2Rdy = RegInit(VecInit(Seq.fill(iqSize)(false.B)))
  val brMask  = RegInit(VecInit(Seq.fill(iqSize)(0.U(robInstCapacity.W))))
  val prfSrc1 = Reg(Vec(iqSize, UInt(prfAddrWidth.W)))
  val prfSrc2 = Reg(Vec(iqSize, UInt(prfAddrWidth.W)))
  val src1    = Reg(Vec(iqSize, UInt(XLEN.W)))
  val src2    = Reg(Vec(iqSize, UInt(XLEN.W)))
  
}
