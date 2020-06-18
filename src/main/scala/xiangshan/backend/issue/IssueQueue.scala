package xiangshan.backend.issue

import chisel3._
import chisel3.util._
import xiangshan._

class IssueQueue(val fuTypeInt: BigInt, wakeupCnt: Int, val bypassCnt: Int) extends XSModule with NeedImpl {

  val useBypass = bypassCnt > 0

  val io = IO(new Bundle() {
    // flush Issue Queue
    val redirect = Flipped(ValidIO(new Redirect))

    // enq Ctrl sigs at dispatch-2
    val enqCtrl = Flipped(DecoupledIO(new MicroOp))
    // enq Data at next cycle (regfile has 1 cycle latency)
    val enqData = Flipped(ValidIO(new ExuInput))

    val deq = DecoupledIO(new ExuInput)
    val wakeUpPorts = Vec(wakeupCnt, Flipped(DecoupledIO(new ExuOutput)))
    val bypassPorts = if(useBypass) Vec(bypassCnt, Flipped(DecoupledIO(new ExuOutput))) else null
  })
}
