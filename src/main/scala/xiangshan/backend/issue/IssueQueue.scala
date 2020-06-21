package xiangshan.backend.issue

import chisel3._
import chisel3.util._
import xiangshan._

trait IQConst{
  val iqSize = 8

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

  //Tag Queue
  val ctrlFlow = Mem(iqSize,new CtrlFlow)
  val ctrlSig = Mem(iqSize,new CtrlSignals)
  val valid   = RegInit(VecInit(Seq.fill(iqSize)(false.B)))
  val src1Rdy = RegInit(VecInit(Seq.fill(iqSize)(false.B)))
  val src2Rdy = RegInit(VecInit(Seq.fill(iqSize)(false.B)))
  val brMask  = RegInit(VecInit(Seq.fill(iqSize)(0.U(BrqSize.W))))
  val prfSrc1 = Reg(Vec(iqSize, UInt(PhyRegIdxWidth.W)))
  val prfSrc2 = Reg(Vec(iqSize, UInt(PhyRegIdxWidth.W)))
  val roqIdx  = Reg(Vec(iqSize, UInt(RoqIdxWidth.W)))

  //tag enqueue
  val iqEmty = !valid.asUInt.orR
  val iqFull =  valid.asUInt.andR
  val iqAllowIn = !iqFull 
  io.enqCtrl.ready := iqAllowIn

  //enqueue pointer
  val emptySlot = ~valid.asUInt
  val enqueueSelect = PriorityEncoder(emptySlot)

  when(io.enqCtrl.fire()){
    ctrlFlow(enqueueSelect) := io.enqCtrl.bits.cf
    ctrlSig(enqueueSelect) := io.enqCtrl.bits.ctrl
    valid(enqueueSelect) := true.B
    prfSrc1(enqueueSelect) := io.enqCtrl.bits.psrc1
    prfSrc2(enqueueSelect) := io.enqCtrl.bits.psrc2
    src1Rdy(enqueueSelect) := io.enqCtrl.bits.src1State === SrcState.rdy
    src2Rdy(enqueueSelect) := io.enqCtrl.bits.src2State === SrcState.rdy
    brMask(enqueueSelect) := io.enqCtrl.bits.brMask
  }

  //Data Queue
  val src1Data    = Reg(Vec(iqSize, UInt(XLEN.W)))
  val src2Data    = Reg(Vec(iqSize, UInt(XLEN.W)))



  //---------------------------------------------------------
  // Select Circuit
  //---------------------------------------------------------


}
