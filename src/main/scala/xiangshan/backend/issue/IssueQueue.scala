package xiangshan.backend.issue

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.rename.FreeListPtr
import xiangshan.utils._

trait IQConst{
  val iqSize = 8
  val iqIdxWidth = log2Up(iqSize)
}

sealed abstract class IQBundle extends XSBundle with IQConst
sealed abstract class IQModule extends XSModule with IQConst //with NeedImpl

sealed class CmpInputBundle extends IQBundle{
  val instRdy = Input(Bool())
  val roqIdx  = Input(UInt(RoqIdxWidth.W))
  val iqIdx   = Input(UInt(iqIdxWidth.W))

  def apply(instRdy: Bool,roqIdx: UInt,iqIdx: UInt ) = {
    this.instRdy := instRdy
    this.roqIdx := roqIdx
    this.iqIdx := iqIdx
    this
  }
}


object CompareCircuitUnit{
  def apply(in1: CmpInputBundle, in2: CmpInputBundle) = {
    val out = Wire(new CmpInputBundle)
    val roqIdx1 = in1.roqIdx
    val roqIdx2 = in2.roqIdx
    val iqIdx1  = in1.iqIdx
    val iqIdx2  = in2.iqIdx

    val inst1Rdy = in1.instRdy
    val inst2Rdy = in2.instRdy

    out.instRdy := inst1Rdy | inst2Rdy
    out.roqIdx := roqIdx2
    out.iqIdx := iqIdx2

    val positionNotSame = roqIdx1.head(1) ^ roqIdx2.head(1)
    val roqIdx1lt2 = roqIdx1.tail(1) < roqIdx2.tail(1)
    val choose1 = (positionNotSame & ~roqIdx1lt2) | ((~positionNotSame) & roqIdx1lt2)

    when((inst1Rdy && !inst2Rdy) || (inst1Rdy && inst2Rdy && choose1.asBool)){
      out.roqIdx := roqIdx1
      out.iqIdx := iqIdx1
    }
    out
  }
}

object ParallelSel {
  def apply(iq: Seq[CmpInputBundle]):  CmpInputBundle = {
    iq match {
      case Seq(a) => a
      case Seq(a, b) => CompareCircuitUnit(a, b)
      case _ =>
        apply(Seq(apply(iq take iq.size/2), apply(iq drop iq.size/2)))
    }
  }
}

class IssueQueue(val fuTypeInt: BigInt, val wakeupCnt: Int, val bypassCnt: Int = 0, val fixedDelay: Int = 1) extends IQModule {

  val useBypass = bypassCnt > 0

  val io = IO(new Bundle() {
    // flush Issue Queue
    val redirect = Flipped(ValidIO(new Redirect))

    // enq Ctrl sigs at dispatch-2
    val enqCtrl = Flipped(DecoupledIO(new MicroOp))
    // enq Data at next cycle (regfile has 1 cycle latency)
    val enqData = Flipped(ValidIO(new ExuInput))

    //  broadcast selected uop to other issue queues which has bypasses
    val selectedUop = if(useBypass) ValidIO(new MicroOp) else null

    // send to exu
    val deq = DecoupledIO(new ExuInput)

    // listen to write back bus
    val wakeUpPorts = Vec(wakeupCnt, Flipped(ValidIO(new ExuOutput)))

    // use bypass uops to speculative wake-up
    val bypassUops = if(useBypass) Vec(bypassCnt, Flipped(ValidIO(new MicroOp))) else null
    val bypassData = if(useBypass) Vec(bypassCnt, Flipped(ValidIO(new ExuOutput))) else null
  })
  //---------------------------------------------------------
  // Issue Queue
  //---------------------------------------------------------

  //Tag Queue
  val ctrlFlow = Mem(iqSize,new CtrlFlow)
  val ctrlSig = Mem(iqSize,new CtrlSignals)
  val brMask  = Reg(Vec(iqSize, UInt(BrqSize.W)))
  val brTag  =  Reg(Vec(iqSize, UInt(BrTagWidth.W)))
  val validReg = RegInit(VecInit(Seq.fill(iqSize)(false.B)))
  val validWillFalse= WireInit(VecInit(Seq.fill(iqSize)(false.B)))
  val valid = validReg.asUInt & ~validWillFalse.asUInt
  val src1Rdy = Reg(Vec(iqSize, Bool()))
  val src2Rdy = Reg(Vec(iqSize, Bool()))
  val src3Rdy = Reg(Vec(iqSize, Bool()))
  val prfSrc1 = Reg(Vec(iqSize, UInt(PhyRegIdxWidth.W)))
  val prfSrc2 = Reg(Vec(iqSize, UInt(PhyRegIdxWidth.W)))
  val prfSrc3 = Reg(Vec(iqSize, UInt(PhyRegIdxWidth.W)))
  val prfDest = Reg(Vec(iqSize, UInt(PhyRegIdxWidth.W)))
  val oldPDest = Reg(Vec(iqSize, UInt(PhyRegIdxWidth.W)))
  val freelistAllocPtr = Reg(Vec(iqSize, new FreeListPtr))
  val roqIdx  = Reg(Vec(iqSize, UInt(RoqIdxWidth.W)))

  val instRdy = WireInit(VecInit(List.tabulate(iqSize)(i => src1Rdy(i) && src2Rdy(i) && src3Rdy(i)&& valid(i))))

  
  //tag enqueue
  val iqEmty = !valid.asUInt.orR
  val iqFull =  valid.asUInt.andR
  val iqAllowIn = !iqFull 
  io.enqCtrl.ready := iqAllowIn

  //enqueue pointer
  val emptySlot = ~valid.asUInt
  val enqueueSelect = PriorityEncoder(emptySlot)
  //assert(!(io.enqCtrl.valid && io.redirect.valid),"enqueue valid should be false when redirect valid")
  XSError(io.enqCtrl.valid && io.redirect.valid,"enqueue valid should be false when redirect valid")
  val srcEnqRdy = WireInit(VecInit(false.B, false.B, false.B))

  srcEnqRdy(0) := Mux(io.enqCtrl.bits.ctrl.src1Type =/= SrcType.reg , true.B ,io.enqCtrl.bits.src1State === SrcState.rdy)
  srcEnqRdy(1) := Mux(io.enqCtrl.bits.ctrl.src2Type =/= SrcType.reg , true.B ,io.enqCtrl.bits.src2State === SrcState.rdy)
  //TODO:
  if(fuTypeInt != FuType.fmac.litValue()){ srcEnqRdy(2) := true.B}
  else{srcEnqRdy(2) := Mux(io.enqCtrl.bits.ctrl.src3Type =/= SrcType.reg , true.B ,io.enqCtrl.bits.src3State === SrcState.rdy)}

  when (io.enqCtrl.fire()) {
    ctrlFlow(enqueueSelect) := io.enqCtrl.bits.cf
    ctrlSig(enqueueSelect) := io.enqCtrl.bits.ctrl
    brMask(enqueueSelect) := io.enqCtrl.bits.brMask
    brTag(enqueueSelect) := io.enqCtrl.bits.brTag
    validReg(enqueueSelect) := true.B
    src1Rdy(enqueueSelect) := srcEnqRdy(0)
    src2Rdy(enqueueSelect) := srcEnqRdy(1)
    src3Rdy(enqueueSelect) := srcEnqRdy(2)
    prfSrc1(enqueueSelect) := io.enqCtrl.bits.psrc1
    prfSrc2(enqueueSelect) := io.enqCtrl.bits.psrc2
    prfSrc3(enqueueSelect) := io.enqCtrl.bits.psrc3
    prfDest(enqueueSelect) := io.enqCtrl.bits.pdest
    oldPDest(enqueueSelect) := io.enqCtrl.bits.old_pdest
    freelistAllocPtr(enqueueSelect) := io.enqCtrl.bits.freelistAllocPtr
    roqIdx(enqueueSelect) := io.enqCtrl.bits.roqIdx
    XSDebug("[IQ enq]: enqSelect:%d | s1Rd:%d s2Rd:%d s3Rd:%d\n",enqueueSelect.asUInt,
                                                                        (io.enqCtrl.bits.src1State === SrcState.rdy),
                                                                        (io.enqCtrl.bits.src2State === SrcState.rdy),
                                                                        (io.enqCtrl.bits.src3State === SrcState.rdy))

  }

  //Data Queue
  val src1Data = Reg(Vec(iqSize, UInt(XLEN.W)))
  val src2Data = Reg(Vec(iqSize, UInt(XLEN.W)))
  val src3Data = Reg(Vec(iqSize, UInt(XLEN.W)))
  

  val enqSelNext = RegNext(enqueueSelect)
  val enqFireNext = RegNext(io.enqCtrl.fire())

  // Read RegFile
  //Ready data will written at next cycle
  when (enqFireNext) {
    when(src1Rdy(enqSelNext)){src1Data(enqSelNext) := io.enqData.bits.src1}
    when(src2Rdy(enqSelNext)){src2Data(enqSelNext) := io.enqData.bits.src2}
    when(src3Rdy(enqSelNext)){src3Data(enqSelNext) := io.enqData.bits.src3}
  }
    

  XSDebug("[Reg info-ENQ] enqSelNext:%d | enqFireNext:%d \n",enqSelNext,enqFireNext)
  XSDebug("[IQ content] valid vr vf| pc  insruction |   src1rdy  src1 |  src2Rdy  src2 |  src3Rdy  src3  |  pdest  \n")
  for(i <- 0 to (iqSize -1)) {
    val ins = ctrlFlow(i).instr
    val pc = ctrlFlow(i).pc
    XSDebug(valid(i),
      "[IQ content][%d] %d%d%d |%x  %x| %x %x | %x %x | %x %x | %d  valid|\n",
      i.asUInt, valid(i), validReg(i), validWillFalse(i), pc,ins,src1Rdy(i), src1Data(i),
      src2Rdy(i), src2Data(i),src3Rdy(i), src3Data(i),prfDest(i))
    XSDebug(validReg(i) && validWillFalse(i),
      "[IQ content][%d] %d%d%d |%x  %x| %x %x | %x %x | %x %x | %d  valid will be False|\n",
      i.asUInt, valid(i), validReg(i), validWillFalse(i),pc,ins, src1Rdy(i), src1Data(i),
      src2Rdy(i), src2Data(i),src3Rdy(i), src3Data(i),prfDest(i))
    XSDebug("[IQ content][%d] %d%d%d |%x  %x| %x %x | %x %x | %x %x | %d\n",
      i.asUInt, valid(i), validReg(i), validWillFalse(i),pc,ins, src1Rdy(i), src1Data(i),
      src2Rdy(i), src2Data(i),src3Rdy(i), src3Data(i),prfDest(i))
  }
  // From Common Data Bus(wakeUpPort)
  // chisel claims that firrtl will optimize Mux1H to and/or tree
  // TODO: ignore ALU'cdb srcRdy, for byPass has done it
  if(wakeupCnt > 0) {
    val cdbValid = List.tabulate(wakeupCnt)(i => io.wakeUpPorts(i).valid)
    val cdbData = List.tabulate(wakeupCnt)(i => io.wakeUpPorts(i).bits.data)
    val cdbPdest = List.tabulate(wakeupCnt)(i => io.wakeUpPorts(i).bits.uop.pdest)

    val srcNum = 3
    val prfSrc = List(prfSrc1, prfSrc2, prfSrc3)
    val srcRdy = List(src1Rdy, src2Rdy, src3Rdy)
    val srcData = List(src1Data, src2Data, src3Data)
    val srcHitVec = List.tabulate(srcNum)(k =>
                      List.tabulate(iqSize)(i =>
                        List.tabulate(wakeupCnt)(j =>
                          (prfSrc(k)(i) === cdbPdest(j)) && cdbValid(j))))
    val srcHit =  List.tabulate(srcNum)(k =>
                    List.tabulate(iqSize)(i =>
                      ParallelOR(srcHitVec(k)(i)).asBool()))
                      // VecInit(srcHitVec(k)(i)).asUInt.orR))
    for(k <- 0 until srcNum){
      for(i <- 0 until iqSize)( when (valid(i)) {
        when(!srcRdy(k)(i) && srcHit(k)(i)) {
          srcRdy(k)(i) := true.B
          // srcData(k)(i) := Mux1H(srcHitVec(k)(i), cdbData)
          srcData(k)(i) := ParallelMux(srcHitVec(k)(i) zip cdbData)
        }
      })
    }
    // From byPass [speculative] (just for ALU to listen to other ALU's res, include itself)
    // just need Tag(Ctrl). send out Tag when Tag is decided. other ALUIQ listen to them and decide Tag
    // byPassUops is one cycle before byPassDatas
    if (bypassCnt > 0) {
      val bypassPdest = List.tabulate(bypassCnt)(i => io.bypassUops(i).bits.pdest)
      val bypassValid = List.tabulate(bypassCnt)(i => io.bypassUops(i).valid) // may only need valid not fire()
      val bypassData = List.tabulate(bypassCnt)(i => io.bypassData(i).bits.data)
      val srcBpHitVec = List.tabulate(srcNum)(k =>
                          List.tabulate(iqSize)(i =>
                            List.tabulate(bypassCnt)(j =>
                              (prfSrc(k)(i) === bypassPdest(j)) && bypassValid(j))))
      val srcBpHit =  List.tabulate(srcNum)(k =>
                        List.tabulate(iqSize)(i =>
                          ParallelOR(srcBpHitVec(k)(i)).asBool()))
                          // VecInit(srcBpHitVec(k)(i)).asUInt.orR))
      val srcBpHitVecNext = List.tabulate(srcNum)(k =>
                              List.tabulate(iqSize)(i =>
                                List.tabulate(bypassCnt)(j => RegNext(srcBpHitVec(k)(i)(j)))))
      val srcBpHitNext = List.tabulate(srcNum)(k =>
                          List.tabulate(iqSize)(i =>
                            RegNext(srcBpHit(k)(i))))
      val srcBpData = List.tabulate(srcNum)(k =>
                        List.tabulate(iqSize)(i => 
                          ParallelMux(srcBpHitVecNext(k)(i) zip bypassData)))
                          // Mux1H(srcBpHitVecNext(k)(i), bypassData)))
      for(k <- 0 until srcNum){
        for(i <- 0 until iqSize){ when (valid(i)) {
          when(valid(i) && !srcRdy(k)(i) && srcBpHit(k)(i)) { srcRdy(k)(i) := true.B }
          when(srcBpHitNext(k)(i)) { srcData(k)(i) := srcBpData(k)(i)}
        }}
      }

      // Enqueue Bypass
      val enqBypass = WireInit(VecInit(false.B, false.B, false.B))
      val enqBypassHitVec = List(List.tabulate(bypassCnt)(j => io.enqCtrl.bits.psrc1 === bypassPdest(j) && bypassValid(j) && io.enqCtrl.fire()),
                                 List.tabulate(bypassCnt)(j => io.enqCtrl.bits.psrc2 === bypassPdest(j) && bypassValid(j) && io.enqCtrl.fire()),
                                 List.tabulate(bypassCnt)(j => io.enqCtrl.bits.psrc3 === bypassPdest(j) && bypassValid(j) && io.enqCtrl.fire()))
      val enqBypassHitVecNext = enqBypassHitVec.map(i => i.map(j => RegNext(j)))
      enqBypass(0) := ParallelOR(enqBypassHitVec(0))
      enqBypass(1) := ParallelOR(enqBypassHitVec(1))
      enqBypass(2) := ParallelOR(enqBypassHitVec(2))
      when(enqBypass(0)) { src1Rdy(enqueueSelect) := true.B }
      when(enqBypass(1)) { src2Rdy(enqueueSelect) := true.B }
      when(enqBypass(2)) { src3Rdy(enqueueSelect) := true.B }
      when(RegNext(enqBypass(0))) { src1Data(enqSelNext) := ParallelMux(enqBypassHitVecNext(0) zip bypassData)}
      when(RegNext(enqBypass(1))) { src2Data(enqSelNext) := ParallelMux(enqBypassHitVecNext(1) zip bypassData)}
      when(RegNext(enqBypass(2))) { src3Data(enqSelNext) := ParallelMux(enqBypassHitVecNext(2) zip bypassData)}
    }
    
  }


  //---------------------------------------------------------
  // Select Circuit
  //---------------------------------------------------------
  val selVec = List.tabulate(iqSize){ i =>
    Wire(new CmpInputBundle).apply(instRdy(i),roqIdx(i),i.U)
  }
  val selResult = ParallelSel(selVec)
  XSDebug("[Sel Result] ResReady:%d || ResultId:%d\n",selResult.instRdy,selResult.iqIdx.asUInt)
  //---------------------------------------------------------
  // Redirect Logic
  //---------------------------------------------------------
  val expRedirect = io.redirect.valid && io.redirect.bits.isException
  val brRedirect = io.redirect.valid && !io.redirect.bits.isException

  List.tabulate(iqSize)( i =>
    when(brRedirect && (UIntToOH(io.redirect.bits.brTag) & brMask(i)).orR && validReg(i) ){
        validReg(i) := false.B
        validWillFalse(i) := true.B

    } .elsewhen(expRedirect) {
        validReg(i) := false.B
        validWillFalse(i) := true.B
    }
  )
  //---------------------------------------------------------
  // Dequeue Logic
  //---------------------------------------------------------
  //hold the sel-index to wait for data
  val selInstIdx = RegInit(0.U(iqIdxWidth.W))
  val selInstRdy = RegInit(false.B)

  //issue the select instruction
  val dequeueSelect = Wire(UInt(iqIdxWidth.W))
  dequeueSelect := selInstIdx

  val brRedirectMaskMatch = (UIntToOH(io.redirect.bits.brTag) & brMask(dequeueSelect)).orR
  val IQreadyGo = selInstRdy && !expRedirect && (!brRedirect || !brRedirectMaskMatch)

  io.deq.valid := IQreadyGo

  io.deq.bits.uop.cf := ctrlFlow(dequeueSelect)
  io.deq.bits.uop.ctrl := ctrlSig(dequeueSelect)
  io.deq.bits.uop.brMask := brMask(dequeueSelect)
  io.deq.bits.uop.brTag := brTag(dequeueSelect)

  io.deq.bits.uop.psrc1 := prfSrc1(dequeueSelect)
  io.deq.bits.uop.psrc2 := prfSrc2(dequeueSelect)
  io.deq.bits.uop.psrc3 := prfSrc3(dequeueSelect)
  io.deq.bits.uop.pdest := prfDest(dequeueSelect)
  io.deq.bits.uop.old_pdest := oldPDest(dequeueSelect)
  io.deq.bits.uop.src1State := SrcState.rdy
  io.deq.bits.uop.src2State := SrcState.rdy
  io.deq.bits.uop.src3State := SrcState.rdy
  io.deq.bits.uop.freelistAllocPtr := freelistAllocPtr(dequeueSelect)
  io.deq.bits.uop.roqIdx := roqIdx(dequeueSelect)

  io.deq.bits.src1 := src1Data(dequeueSelect)
  io.deq.bits.src2 := src2Data(dequeueSelect)
  io.deq.bits.src3 := src3Data(dequeueSelect)
  
  XSDebug("[Reg Info-Sel] selInstRdy:%d || selIdx:%d\n",selInstRdy,selInstIdx.asUInt)
  XSDebug(IQreadyGo,"[IQ dequeue] **dequeue fire:%d** roqIdx:%d dequeueSel:%d | src1Rd:%d src1:%d | src2Rd:%d src2:%d\n", io.deq.fire(), io.deq.bits.uop.roqIdx, dequeueSelect.asUInt,
                            (io.deq.bits.uop.src1State === SrcState.rdy), io.deq.bits.uop.psrc1,
                            (io.deq.bits.uop.src2State === SrcState.rdy), io.deq.bits.uop.psrc2
                            )

  //update the index register of instruction that can be issue, unless function unit not allow in
  //then the issue will be stopped to wait the function unit 
  //clear the validBit of dequeued instruction in issuequeue
  when(io.deq.fire()){
    validReg(dequeueSelect) := false.B
    validWillFalse(dequeueSelect) := true.B
  }

  val selRegflush = expRedirect || (brRedirect && brRedirectMaskMatch)

  selInstRdy := Mux(selRegflush,false.B,selResult.instRdy)
  selInstIdx := Mux(selRegflush,0.U,selResult.iqIdx)
  // SelectedUop (bypass / speculative)
  if(useBypass) {
    assert(fixedDelay==1) // only support fixedDelay is 1 now
    def DelayPipe[T <: Data](a: T, delay: Int = 0) = {
      // println(delay)
      if(delay == 0) a
      else {
        val storage = Wire(VecInit(Seq.fill(delay+1)(a)))
        // storage(0) := a
        for(i <- 1 until delay) {
          storage(i) := RegNext(storage(i-1))
        }
        storage(delay)
      }
    }
    val sel = io.selectedUop
    val selIQIdx = selResult.iqIdx
    val delayPipe = DelayPipe(VecInit(selResult.instRdy, prfDest(selIQIdx)), fixedDelay-1)
    sel.bits := DontCare
    sel.bits.pdest := delayPipe(fixedDelay-1)(1)
    sel.valid := DelayPipe(selResult.instRdy, fixedDelay-1)
  }
}
