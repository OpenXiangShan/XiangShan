package xiangshan.backend.issue

import chisel3._
import chisel3.util._
import xiangshan._

trait IQConst{
  val iqSize = 8
  val idIdxWidth = log2Up(iqSize)
  val layer1Size = iqSize
  val layer2Size = iqSize/2
}

sealed class CmpInputBundle extends XSBundle {
  val instRdy = Input(Bool())
  val roqIdx  = Input(UInt(RoqIdxWidth.W))
  val iqIdx   = Input(UInt(idIdxWidth.W))
}


sealed class CompareCircuitUnit(layer: Int, id: Int) extends XSModule with NeedImpl {
  val io = IO(new Bundle(){
    val input_1 = new CmpInputBundle
    val input_2 = new CmpInputBundle
    val output = new Flipped(CmpInputBundle)
  })

  val roqIdx1 = io.input_1.roqIdx
  val roqIdx2 = io.input_2.roqIdx
  val iqIdx1  = io.input_1.iqIdx
  val iqIdx2  = io.input_2.iqIdx

  val inst1Rdy = io.input_1.instRdy
  val inst2Rdy = io.input_2.instRdy

  val readySignal = Cat(inst1Rdy,inst2Rdy)

  switch (readySignal) {
    is ("b00".U) { 
      io.out.instRdy := false.B
      io.out.roqIdx := DontCare
      io.out.iqIdx := DontCare
    }
    is ("b01".U) { 
      io.out.instRdy := inst2Rdy
      io.out.roqIdx := roqIdx2
      io.out.iqIdx := iqIdx2
     }
    is ("b10".U) { 
      io.out.instRdy := inst1Rdy
      io.out.roqIdx := roqIdx1
      io.out.iqIdx := iqIdx1
    }
    is ("b11".U) { 
      when(roqIdx1 < roqIdx2) {
        io.out.instRdy := inst1Rdy
        io.out.roqIdx := roqIdx1
        io.out.iqIdx := iqIdx1
      } .otherwise {
        io.out.instRdy := inst2Rdy
        io.out.roqIdx := roqIdx2
        io.out.iqIdx := iqIdx2
      }
    }
  }

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
  val brMask  = RegInit(VecInit(Seq.fill(iqSize)(0.U(BrqSize.W))))
  val valid   = RegInit(VecInit(Seq.fill(iqSize)(false.B)))
  val src1Rdy = RegInit(VecInit(Seq.fill(iqSize)(false.B)))
  val src2Rdy = RegInit(VecInit(Seq.fill(iqSize)(false.B)))
  //val src3Rdy = RegInit(VecInit(Seq.fill(iqSize)(false.B)))
  val prfSrc1 = Reg(Vec(iqSize, UInt(PhyRegIdxWidth.W)))
  val prfSrc2 = Reg(Vec(iqSize, UInt(PhyRegIdxWidth.W)))
  //val prfSrc3 = Reg(Vec(iqSize, UInt(PhyRegIdxWidth.W)))
  val prfDest = Reg(Vec(iqSize, UInt(PhyRegIdxWidth.W)))
  val oldPDest = Reg(Vec(iqSize, UInt(PhyRegIdxWidth.W)))
  val freelistAllocPrt = Reg(Vec(iqSize, UInt(PhyRegIdxWidth.W)))
  val roqIdx  = Reg(Vec(iqSize, UInt(RoqIdxWidth.W)))

  val instRdy = WireInit(VecInit(List.tabulate(iqSize)(i => src1Rdy(i) && src2Rdy(i) && valid(i))))


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
    brMask(enqueueSelect) := io.enqCtrl.bits.brMask
    valid(enqueueSelect) := true.B
    src1Rdy(enqueueSelect) := io.enqCtrl.bits.src1State === SrcState.rdy
    src2Rdy(enqueueSelect) := io.enqCtrl.bits.src2State === SrcState.rdy
    // src3Rdy(enqueueSelect) := io.enqCtrl.bits.src3State === SrcState.rdy
    prfSrc1(enqueueSelect) := io.enqCtrl.bits.psrc1
    prfSrc2(enqueueSelect) := io.enqCtrl.bits.psrc2
    //prfSrc3(enqueueSelect) := io.enqCtrl.bits.psrc3
    prfDest(enqueueSelect) := io.enqCtrl.bits.pdest
    oldPDest(enqueueSelect) := io.enqCtrl.bits.old_pdest
    freelistAllocPrt(enqueueSelect) := io.enqCtrl.bits.freelistAllocPtr
    roqIdx(enqueueSelect) := io.enqCtrl.bits.roqIdx

  }

  //Data Queue
  val src1Data    = Reg(Vec(iqSize, UInt(XLEN.W)))
  val src2Data    = Reg(Vec(iqSize, UInt(XLEN.W)))


  //---------------------------------------------------------
  // Select Circuit
  //---------------------------------------------------------
  //layer 1
  val layer1CCUs = (0 to layer1Size-1 by +2) map { i =>
    val CCU_1 = Module(new CompareCircuitUnit(layer = 1, id = i))
    CCU_1.io.input_1.instRdy := instRdy(i)
    CCU_1.io.input_1.roqIdx  := roqIdx(i)
    CCU_1.io.input_1.iqIdx   := i.U

    CCU_1.io.input_2.instRdy := instRdy(i+1)
    CCU_1.io.input_2.roqIdx  := roqIdx(i+1)
    CCU_1.io.input_2.iqIdx   := (i+1).U
    
    CCU_1
  }

  //layer 2
  val layer2CCUs = (0 to layer2Size-1 by +2) map { i =>
    val CCU_2 = Module(new CompareCircuitUnit(layer = 2, id = i))
    CCU_2.io.input_1.instRdy := layer1CCUs(i).io.output.instRdy
    CCU_2.io.input_1.roqIdx  := layer1CCUs(i).io.output.roqIdx
    CCU_2.io.input_1.iqIdx   := layer1CCUs(i).io.output.iqIdx

    CCU_2.io.input_2.instRdy := layer1CCUs(i+1).io.output.instRdy
    CCU_2.io.input_2.roqIdx  := layer1CCUs(i+1).io.output.roqIdx
    CCU_2.io.input_2.iqIdx   := layer1CCUs(i+1).io.output.iqIdx
    
    CCU_2
  }

  //layer 3
  val CCU_3 = Module(new CompareCircuitUnit(layer = 2, id = i))
  CCU_3.io.input_1.instRdy := layer2CCUs(0).io.output.instRdy
  CCU_3.io.input_1.roqIdx  := layer2CCUs(0).io.output.roqIdx
  CCU_3.io.input_1.iqIdx   := layer2CCUs(0).io.output.iqIdx

  CCU_3.io.input_2.instRdy := layer2CCUs(1).io.output.instRdy
  CCU_3.io.input_2.roqIdx  := layer2CCUs(1).io.output.roqIdx
  CCU_3.io.input_2.iqIdx   := layer2CCUs(1).io.output.iqIdx

  

  


}
