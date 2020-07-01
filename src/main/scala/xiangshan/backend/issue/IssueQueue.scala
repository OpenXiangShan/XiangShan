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

    when((inst1Rdy && !inst2Rdy) || (inst1Rdy && inst2Rdy && (roqIdx1 < roqIdx2))){
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
    sel.valid := selResult.instRdy
    sel.bits.pdest := delayPipe(fixedDelay-1)(1)
  }
}

class IssueQueueCpt(val fuTypeInt: BigInt, val wakeupCnt: Int, val bypassCnt: Int = 0, val fixedDelay: Int = 1) extends IQModule {

  val useBypass = bypassCnt > 0
  val src2Use = true
  val src3Use = true
  val src2Listen = true
  val src3Listen = true

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

  val srcAllNum = 3
  val srcUseNum = 1 + (if(src2Use) 1 else 0) + (if(src3Use) 1 else 0)// when src2Use is false, then src3Use must be false
  val srcListenNum = 1 + (if(src2Listen) 1 else 0) + (if(src3Listen) 1 else 0) // when src2Listen is false, then src3Listen must be false
  // when use is false, Listen must be false
  require(!(!src2Use && src2Listen))
  require(!(!src3Use && src3Listen))
  require(!(!src2Use && src3Use))
  require(!(!src2Listen && src3Listen))

  // Issue Queue
  // val issQue = IndexableMem(iqSize, new ExuInput, mem = false, init = None)
  val issQue = Mem(iqSize, new ExuInput)
  // val issQue = Reg(Vec(iqSize, new ExuInput))
  val validQue = RegInit(VecInit(Seq.fill(iqSize)(false.B)))
  val idQue = RegInit(VecInit((0 until iqSize).map(_.U(iqIdxWidth.W))))
  val idValidQue = VecInit((0 until iqSize).map(i => validQue(idQue(i)))).asUInt
  val tailAll = RegInit(0.U((iqIdxWidth+1).W))
  val tail = tailAll(iqIdxWidth-1, 0)
  val full = tailAll(iqIdxWidth)
  // alias failed, turn to independent storage(Reg)
  val psrc = VecInit(List.tabulate(iqSize)(i => VecInit(List(issQue(i.U).uop.psrc1, issQue(i.U).uop.psrc2, issQue(i.U).uop.psrc3)))) // TODO: why issQue can not use Int as index, but idQue is ok?? // NOTE: indexed by IssQue's idx
  // val srcRdyVec = Reg(Vec(iqSize, Vec(srcListenNum, Bool())))
  val srcRdyVec = Reg(Vec(iqSize, Vec(srcAllNum, Bool()))) // NOTE: indexed by IssQue's idx
  // val srcData = Reg(Vec(iqSize, Vec(srcUseNum, UInt(XLEN.W)))) // NOTE: Bundle/MicroOp need merge "src1/src2/src3" into a Vec. so that IssueQueue could have Vec
  val srcData = Reg(Vec(iqSize, Vec(srcAllNum, UInt(XLEN.W)))) // NOTE: indexed by IssQue's idx
  val srcRdy = VecInit(srcRdyVec.map(i => ParallelAND(i))) // NOTE: indexed by IssQue's idx
  val srcIdRdy = VecInit((0 until iqSize).map(i => srcRdy(idQue(i)))).asUInt // NOTE: indexed by IdQue's idx
  val srcType = List.tabulate(iqSize)(i => List(issQue(i).uop.ctrl.src1Type, issQue(i).uop.ctrl.src2Type, issQue(i).uop.ctrl.src3Type))

  val srcDataWire = srcData
  srcData := srcDataWire

  // there is three stage
  // |-------------|--------------------|--------------|
  // |Enq:get state|Deq: select/get data| fire stage   |
  // |-------------|--------------------|--------------|

  //-----------------------------------------
  // Enqueue
  //-----------------------------------------
  val enqFire = io.enqCtrl.fire()
  val deqFire = io.deq.fire()
  val popOne = Wire(Bool())
  io.enqCtrl.ready := !full || popOne
  val enqSel = idQue(tail)

  // state enq
  when (io.enqCtrl.fire()) {
    issQue(enqSel).uop := io.enqCtrl.bits
    validQue(enqSel) := true.B

    srcRdyVec(enqSel)(0) := io.enqCtrl.bits.src1State === SrcState.rdy
    if(src2Listen) { srcRdyVec(enqSel)(1) := io.enqCtrl.bits.src2State === SrcState.rdy }
    if(src3Listen) { srcRdyVec(enqSel)(2) := io.enqCtrl.bits.src3State === SrcState.rdy }
  }

  // data enq
  val enqSelNext = RegEnable(enqSel, enqFire)
  // val enqSelNext = RegNext(enqSel)
  val enqFireNext = RegInit(false.B)
  when (enqFireNext) { enqFireNext := false.B }
  when (enqFire) { enqFireNext := true.B }

  val enqDataVec = List(io.enqData.bits.src1, io.enqData.bits.src2, io.enqData.bits.src3)
  when (enqFireNext) {
    for(i <- 0 until srcUseNum) {
      srcDataWire(enqSelNext)(i) := enqDataVec(i)
    }
  }

  //-----------------------------------------
  // tail
  //-----------------------------------------
  val tailInc = enqFire
  val tailDec = popOne
  val tailKeep = tailInc === tailDec
  val tailAdd = tailAll + 1.U
  val tailSub = tailAll - 1.U
  tailAll := Mux(tailKeep, tailAll, Mux(tailInc, tailAdd, tailSub))
  assert(tailAll < 9.U)
  // Select to Dequeue
  val deqSel = PriorityEncoder(idValidQue & srcIdRdy) //may not need idx, just need oneHot
  val deqSelOH = PriorityEncoderOH(idValidQue & srcIdRdy)
  val has1Rdy = ParallelOR((validQue.asUInt & srcRdy.asUInt).asBools).asBool()

  //-----------------------------------------
  // idQue Move
  //-----------------------------------------
  def UIntToMHP(in: UInt) = {
    // UInt to Multi-Hot plus 1: 1.U -> "11".U; 2.U(2.W) -> "0111".U; 3.U(3.W) -> "00001111".W
    val a = Seq.fill(in.getWidth)(2).product
    val s = (1 << (a-1)).S
    Reverse((s(a-1,0).asSInt >> in)(a-1,0).asUInt)
  }
  def UIntToMH(in: UInt) = {
    val a = Seq.fill(in.getWidth)(2).product
    val s = (1 << (a-1)).S
    Reverse((s(a-1,0).asSInt >> in)(a-1,0).asUInt) ^ UIntToOH(in)
  }
  def PriorityDot(in: UInt) = {
    // "1100".U -> "0111".U; "1010".U -> "0011".U; "0000".U -> "0000".U
    val a = Array.fill(iqSize)(1)
    for(i <- 1 until in.getWidth) {
      a(i) = a(i-1)*2 + 1
    }
    Mux(in===0.U, 0.U(in.getWidth.W), PriorityMux(in, a.map(_.U(in.getWidth.W))))
  }
  val tailDot = Mux(full, VecInit(Seq.fill(iqSize)(true.B)).asUInt, UIntToMHP(tail))
  val tailDot2 = Mux(full, VecInit(Seq.fill(iqSize)(true.B)).asUInt, UIntToMH(tail))
  val selDot = UIntToMHP(deqSel) // FIXIT: PriorityEncoder -> UIntToMHP means long latency
  val nonValid = ~(idValidQue | ~tailDot2)
  val popDot = PriorityDot(nonValid)
  val isPop = ParallelOR(nonValid.asBools).asBool()
  val moveDot = Mux(isPop, tailDot ^ popDot, tailDot ^ selDot)

  assert(!(popOne&&moveDot(0)))
  when (popOne) {
    for(i <- 1 until iqSize) {
      when (moveDot(i)) { idQue(i-1) := idQue(i) }
    }
    val ptr_tmp = Mux(full, VecInit(Seq.fill(iqIdxWidth)(true.B)).asUInt, tail)
    idQue(ptr_tmp) := deqSel
  }
  assert(ParallelAND(List.tabulate(iqSize)(i => ParallelOR(List.tabulate(iqSize)(j => j.U === idQue(i))))).asBool)

  //-----------------------------------------
  // Redirect
  //-----------------------------------------
  // redirect enq
  val enqRedHit = io.redirect.valid && (io.redirect.bits.isException || ParallelOR((UIntToOH(io.redirect.bits.brTag) & io.enqCtrl.bits.brMask).asBools).asBool)
  when (enqRedHit) {
    validQue(enqSel) := false.B
    enqFireNext := false.B
  }

  // redirect issQue
  val redHitVec = List.tabulate(iqSize)(i => io.redirect.valid && (io.redirect.bits.isException || ParallelOR((UIntToOH(io.redirect.bits.brTag) & issQue(i).uop.brMask).asBools).asBool))
  for (i <- 0 until iqSize) {
    when (redHitVec(i)) {
      validQue(i) := false.B
    }
  }
  // reditect deq(issToExu)
  val redIdHitVec = List.tabulate(iqSize)(i => io.redirect.valid && (io.redirect.bits.isException || ParallelOR((UIntToOH(io.redirect.bits.brTag) & issQue(idQue(i)).uop.brMask).asBools).asBool))
  val selIsRed = ParallelOR((deqSelOH & VecInit(redIdHitVec).asUInt).asBools).asBool

  //-----------------------------------------
  // Dequeue (or to Issue Stage)
  //-----------------------------------------
  val issueToExu = Reg(new ExuInput)
  val issueToExuValid = RegInit(false.B)
  val deqCanIn = !issueToExuValid || deqFire
  val deqFlushHit = io.redirect.valid && (io.redirect.bits.isException ||
                 ParallelOR((issueToExu.uop.brMask & UIntToOH(io.redirect.bits.brTag)).asBools).asBool)
  val toIssFire = deqCanIn && has1Rdy && !isPop && !selIsRed
  popOne := deqCanIn && (has1Rdy || isPop) // send a empty or valid term to issueStage

  when (toIssFire) {
    issueToExu := issQue(deqSel)
    issueToExuValid := true.B

    issueToExu.src1 := srcDataWire(deqSel)(0)
    if (src2Use) { issueToExu.src2 := srcDataWire(deqSel)(1) } else { issueToExu.src2 := DontCare }
    if (src3Use) { issueToExu.src3 := srcDataWire(deqSel)(2) } else { issueToExu.src3 := DontCare }
  }
  when (deqFire || deqFlushHit) {
    issueToExuValid := false.B
  }

  io.deq.valid := issueToExuValid && !deqFlushHit
  io.deq.bits := issueToExu

  //-----------------------------------------
  // Wakeup and Bypass
  //-----------------------------------------
  if (wakeupCnt > 0) {
    val cdbValid = List.tabulate(wakeupCnt)(i => io.wakeUpPorts(i).valid)
    val cdbData = List.tabulate(wakeupCnt)(i => io.wakeUpPorts(i).bits.data)
    val cdbPdest = List.tabulate(wakeupCnt)(i => io.wakeUpPorts(i).bits.uop.pdest)
    val cdbrfWen = List.tabulate(wakeupCnt)(i => io.wakeUpPorts(i).bits.uop.ctrl.rfWen)
    val cdbfpWen = List.tabulate(wakeupCnt)(i => io.wakeUpPorts(i).bits.uop.ctrl.fpWen)

    for(i <- 0 until iqSize) {
      for(j <- 0 until srcListenNum) {
        val hitVec = List.tabulate(wakeupCnt)(k => psrc(i)(j) === cdbPdest(k) && cdbValid(k) && (srcType(i)(j)===SrcType.reg && cdbrfWen(k) || srcType(i)(j)===SrcType.fp && cdbfpWen(k)))
        val hit = ParallelOR(hitVec).asBool
        val data = ParallelMux(hitVec zip cdbData)
        when (validQue(i) && !srcRdyVec(i)(j) && hit) { 
          srcDataWire(i)(j) := data
          srcRdyVec(i)(j) := true.B
        }
        // XSDebug(validQue(i) && !srcRdyVec(i)(j) && hit, "WakeUp: Sel:%d Src:(%d|%d) Rdy:%d Hit:%d HitVec:%b Data:%x\n", i.U, j.U, psrc(i)(j), srcRdyVec(i)(j), hit, VecInit(hitVec).asUInt, data)
        for (k <- 0 until wakeupCnt) {
          XSDebug(validQue(i) && !srcRdyVec(i)(j) && hit && hitVec(k), "WakeUpHit: IQIdx:%d Src%d:%d Ports:%d Data:%x Pc:%x RoqIdx:%x\n", i.U, j.U, psrc(i)(j), k.U, cdbData(k), io.wakeUpPorts(k).bits.uop.cf.pc, io.wakeUpPorts(k).bits.uop.roqIdx)
        }
      }
    }
  }
  if (useBypass) {
    val bpPdest = List.tabulate(bypassCnt)(i => io.bypassUops(i).bits.pdest)
    val bpValid = List.tabulate(bypassCnt)(i => io.bypassUops(i).valid)
    val bpData = List.tabulate(bypassCnt)(i => io.bypassData(i).bits.data)
    val bprfWen = List.tabulate(bypassCnt)(i => io.bypassUops(i).bits.ctrl.rfWen)
    val bpfpWen = List.tabulate(bypassCnt)(i => io.bypassUops(i).bits.ctrl.fpWen)

    for (i <- 0 until iqSize) {
      for (j <- 0 until srcListenNum) {
        val hitVec = List.tabulate(bypassCnt)(k => psrc(i)(j) === bpPdest(k) && bpValid(k) && (srcType(i)(j)===SrcType.reg && bprfWen(k) || srcType(i)(j)===SrcType.fp && bpfpWen(k)))
        val hitVecNext = hitVec.map(RegNext(_))
        val hit = ParallelOR(hitVec).asBool
        when (validQue(i) && !srcRdyVec(i)(j) && hit) {
          srcRdyVec(i)(j) := true.B // FIXME: if uncomment the up comment, will cause combiantional loop, but it is Mem type??
        }
        when (RegNext(validQue(i) && !srcRdyVec(i)(j) && hit)) {
          srcDataWire(i)(j) := PriorityMux(hitVecNext zip bpData)
        }
        // XSDebug(validQue(i) && !srcRdyVec(i)(j) && hit, "BypassCtrl: Sel:%d Src:(%d|%d) Rdy:%d Hit:%d HitVec:%b\n", i.U, j.U, psrc(i)(j), srcRdyVec(i)(j), hit, VecInit(hitVec).asUInt)
        for (k <- 0 until bypassCnt) {
          XSDebug(validQue(i) && !srcRdyVec(i)(j) && hit && hitVec(k), "BypassCtrlHit: IQIdx:%d Src%d:%d Ports:%d Pc:%x RoqIdx:%x\n", i.U, j.U, psrc(i)(j), k.U, io.bypassUops(k).bits.cf.pc, io.bypassUops(k).bits.roqIdx)
        }
        // XSDebug(RegNext(validQue(i) && !srcRdyVec(i)(j) && hit), "BypassData: Sel:%d Src:(%d|%d) HitVecNext:%b Data:%x (for last cycle's Ctrl)\n", i.U, j.U, psrc(i)(j), VecInit(hitVecNext).asUInt, ParallelMux(hitVecNext zip bpData))
        for (k <- 0 until bypassCnt) {
          XSDebug(RegNext(validQue(i) && !srcRdyVec(i)(j) && hit && hitVec(k)), "BypassDataHit: IQIdx:%d Src%d:%d Ports:%d Data:%x Pc:%x RoqIdx:%x\n", i.U, j.U, psrc(i)(j), k.U, bpData(k), io.bypassUops(k).bits.cf.pc, io.bypassUops(k).bits.roqIdx)
        }
      }
    }

    // Enqueue Bypass
    val enqCtrl = io.enqCtrl
    val enqPsrc = List(enqCtrl.bits.psrc1, enqCtrl.bits.psrc2, enqCtrl.bits.psrc3)
    val enqSrcRdy = List(enqCtrl.bits.src1State===SrcState.rdy, enqCtrl.bits.src2State===SrcState.rdy, enqCtrl.bits.src3State===SrcState.rdy)
    val enqSrcType = List(enqCtrl.bits.ctrl.src1Type, enqCtrl.bits.ctrl.src2Type, enqCtrl.bits.ctrl.src3Type)
    for (i <- 0 until srcListenNum) {
      val hitVec = List.tabulate(bypassCnt)(j => enqPsrc(i)===bpPdest(j) && bpValid(j) && (enqSrcType(i)===SrcType.reg && bprfWen(j) || enqSrcType(i)===SrcType.fp && bpfpWen(j)))
      val hitVecNext = hitVec.map(RegNext(_))
      val hit = ParallelOR(hitVec).asBool
      when (enqFire && hit && !enqSrcRdy(i)) {
        srcRdyVec(enqSel)(i) := true.B
      }
      when (RegNext(enqFire && hit && !enqSrcRdy(i))) {
        srcDataWire(enqSelNext)(i) := ParallelMux(hitVecNext zip bpData)
      }
      // XSDebug(enqFire && hit, "EnqBypassCtrl: enqSel:%d Src:(%d|%d) Hit:%d HitVec:%b \n", enqSel, i.U, enqPsrc(i), hit, VecInit(hitVec).asUInt)
      for (k <- 0 until bypassCnt) {
        XSDebug(enqFire && hit && !enqSrcRdy(i) && hitVec(k), "EnqBypassCtrlHit: enqSel:%d Src%d:%d Ports:%d Pc:%x RoqIdx:%x\n", enqSel, i.U, enqPsrc(i), k.U, io.bypassUops(k).bits.cf.pc, io.bypassUops(k).bits.roqIdx)
      }
      // XSDebug(RegNext(enqFire && hit), "EnqBypassData: enqSelNext:%d Src:(%d|%d) HitVecNext:%b Data:%x (for last cycle's Ctrl)\n", enqSelNext, i.U, enqPsrc(i), VecInit(hitVecNext).asUInt, ParallelMux(hitVecNext zip bpData))
      for (k <- 0 until bypassCnt) {
        XSDebug(RegNext(enqFire && hit && !enqSrcRdy(i) && hitVec(k)), "EnqBypassDataHit: enqSel:%d Src%d:%d Ports:%d Data:%x Pc:%x RoqIdx:%x\n", enqSel, i.U, enqPsrc(i), k.U, bpData(k), io.bypassUops(k).bits.cf.pc, io.bypassUops(k).bits.roqIdx)
      }
    }

    // send out bypass
    assert(fixedDelay==1) // only support fixedDelay is 1 now
    val sel = io.selectedUop
    sel.valid := toIssFire
    sel.bits := DontCare
    sel.bits.pdest := issQue(deqSel).uop.pdest
    sel.bits.cf.pc := issQue(deqSel).uop.cf.pc
    sel.bits.roqIdx := issQue(deqSel).uop.roqIdx
    sel.bits.ctrl.rfWen := issQue(deqSel).uop.ctrl.rfWen
    sel.bits.ctrl.fpWen := issQue(deqSel).uop.ctrl.fpWen
  }
  XSInfo(io.redirect.valid, "Redirect: valid:%d isExp:%d brTag:%d redHitVec:%b redIdHitVec:%b enqHit:%d selIsRed:%d\n", io.redirect.valid, io.redirect.bits.isException, io.redirect.bits.brTag, VecInit(redHitVec).asUInt, VecInit(redIdHitVec).asUInt, enqRedHit, selIsRed)
  XSInfo(io.enqCtrl.fire(), "EnqCtrl(%d %d) enqSel:%d Psrc/Rdy(%d:%d %d:%d %d:%d) Dest:%d oldDest:%d pc:%x roqIdx:%x\n", io.enqCtrl.valid, io.enqCtrl.ready, enqSel
    , io.enqCtrl.bits.psrc1, io.enqCtrl.bits.src1State, io.enqCtrl.bits.psrc2, io.enqCtrl.bits.src2State, io.enqCtrl.bits.psrc3, io.enqCtrl.bits.src3State, io.enqCtrl.bits.pdest, io.enqCtrl.bits.old_pdest, io.enqCtrl.bits.cf.pc, io.enqCtrl.bits.roqIdx)
  XSInfo(enqFireNext, "EnqData: src1:%x src2:%x src3:%x (for last cycle's Ctrl)\n", io.enqData.bits.src1, io.enqData.bits.src2, io.enqData.bits.src3)
  XSInfo(deqFire, "Deq:(%d %d) [%d|%x][%d|%x][%d|%x] pdest:%d pc:%x roqIdx:%x\n", io.deq.valid, io.deq.ready, io.deq.bits.uop.psrc1, io.deq.bits.src1, io.deq.bits.uop.psrc2, io.deq.bits.src2, io.deq.bits.uop.psrc3, io.deq.bits.src3, io.deq.bits.uop.pdest, io.deq.bits.uop.cf.pc, io.deq.bits.uop.roqIdx)
  XSDebug("tailAll:%d KID(%d%d%d) tailDot:%b tailDot2:%b selDot:%b popDot:%b moveDot:%b\n", tailAll, tailKeep, tailInc, tailDec, tailDot, tailDot2, selDot, popDot, moveDot)
  if(useBypass) {
    XSDebug("isPop:%d popOne:%d deqCanIn:%d toIssFire:%d has1Rdy:%d selIsRed:%d nonValid:%b SelUop:(%d, %d)\n", isPop, popOne, deqCanIn, toIssFire, has1Rdy, selIsRed, nonValid, io.selectedUop.valid, io.selectedUop.bits.pdest)
  } else {
    XSDebug("isPop:%d popOne:%d deqCanIn:%d toIssFire:%d has1Rdy:%d selIsRed:%d nonValid:%b\n", isPop, popOne, deqCanIn, toIssFire, has1Rdy, selIsRed, nonValid)
  }
  XSDebug("id| v|r |psrc|r|   src1          |psrc|r|   src2          |psrc|r|   src3          |   pc   |roqIdx\n")
  for (i <- 0 until iqSize) {
    when (i.U===tail && tailAll=/=8.U) {
      XSDebug("%d|%d|%d|%d|%b|%x|%d|%b|%x|%d|%b|%x|%x| %x  <-\n", idQue(i), idValidQue(i), srcRdy(idQue(i)), psrc(idQue(i))(0), srcRdyVec(idQue(i))(0), srcData(idQue(i))(0), psrc(idQue(i))(1), srcRdyVec(idQue(i))(1), srcData(idQue(i))(1), psrc(idQue(i))(2), srcRdyVec(idQue(i))(2), srcData(idQue(i))(2), issQue(idQue(i)).uop.cf.pc, issQue(idQue(i)).uop.roqIdx)
    }.otherwise {
      XSDebug("%d|%d|%d|%d|%b|%x|%d|%b|%x|%d|%b|%x|%x| %x \n", idQue(i), idValidQue(i), srcRdy(idQue(i)), psrc(idQue(i))(0), srcRdyVec(idQue(i))(0), srcData(idQue(i))(0), psrc(idQue(i))(1), srcRdyVec(idQue(i))(1), srcData(idQue(i))(1), psrc(idQue(i))(2), srcRdyVec(idQue(i))(2), srcData(idQue(i))(2), issQue(idQue(i)).uop.cf.pc, issQue(idQue(i)).uop.roqIdx)
    }
  }

}