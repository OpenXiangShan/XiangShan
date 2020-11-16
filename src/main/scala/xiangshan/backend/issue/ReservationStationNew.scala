package xiangshan.backend.issue

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.backend.exu.{Exu, ExuConfig}
import java.rmi.registry.Registry
import java.{util => ju}

class SrcBundle extends XSBundle {
  val src = UInt(PhyRegIdxWidth.W)
  val state = SrcState()
  val srctype = SrcType()
  
  def hit(uop: MicroOp) : Bool = {
    (src === uop.pdest) && (state === SrcState.busy) &&
    ((srctype === SrcType.reg && uop.ctrl.rfWen && src=/=0.U) ||
     (srctype === SrcType.fp  && uop.ctrl.fpWen)) // TODO: check if zero map to zero when rename
  }

  override def toPrintable: Printable = {
    p"src:${src} state:${state} type:${srctype}"
  }
}

object SrcBundle {
  def apply(src: UInt, state: UInt/*SrcState*/, srctype: UInt/*SrcType*/): SrcBundle = {
    val b = Wire(new SrcBundle)
    b.src := src
    b.state := state
    b.srctype := srctype
    b
  }
  
  def stateCheck(src: SrcBundle): UInt /*SrcState*/ = {
    Mux( (src.srctype=/=SrcType.reg && src.srctype=/=SrcType.fp) ||               
      (src.srctype===SrcType.reg && src.src===0.U), SrcState.rdy, src.state)
  }

  def check(src: UInt, state: UInt, srctype: UInt): SrcBundle = {
    val b = Wire(new SrcBundle)
    b.src := src
    b.state := stateCheck(SrcBundle(src, state, srctype))
    b.srctype := srctype
    b
  }
}

class BypassQueue(number: Int) extends XSModule {
  val io = IO(new Bundle {
    val in  = Flipped(ValidIO(new MicroOp))
    val out = ValidIO(new MicroOp)
    val redirect = Flipped(ValidIO(new Redirect))
  })
  if (number < 0) {
    io.out.valid := false.B
    io.out.bits := DontCare
  } else if(number == 0) {
    io.in <> io.out
    io.out.valid := io.in.valid && !io.out.bits.roqIdx.needFlush(io.redirect) 
  } else {
    val queue = Seq.fill(number)(RegInit(0.U.asTypeOf(new Bundle{
      val valid = Bool()
      val bits = new MicroOp
    })))
    queue(0).valid := io.in.valid
    queue(0).bits  := io.in.bits
    (0 until (number-1)).map{i => 
      queue(i+1) := queue(i)
      queue(i+1).valid := queue(i).valid && !queue(i).bits.roqIdx.needFlush(io.redirect)
    }
    io.out.valid := queue(number-1).valid && !queue(number-1).bits.roqIdx.needFlush(io.redirect)
    io.out.bits := queue(number-1).bits
    for (i <- 0 until number) {
      XSDebug(queue(i).valid, p"BPQue(${i.U}): pc:${Hexadecimal(queue(i).bits.cf.pc)} roqIdx:${queue(i).bits.roqIdx} pdest:${queue(i).bits.pdest} rfWen:${queue(i).bits.ctrl.rfWen} fpWen${queue(i).bits.ctrl.fpWen}\n")
    }
  }
}

class ReservationStationCtrl
(
  val exuCfg: ExuConfig,
  wakeupCnt: Int,
  extraListenPortsCnt: Int,
  srcNum: Int = 3,
  fixedDelay: Int,
  feedback: Boolean,
  replayDelay: Int = 10
) extends XSModule {


  val iqSize = IssQueSize
  val iqIdxWidth = log2Up(iqSize)

  val io = IO(new XSBundle {
    // flush Issue Queue
    val redirect = Flipped(ValidIO(new Redirect))

    // enq Ctrl sigs at dispatch-2
    val enqCtrl = Flipped(DecoupledIO(new MicroOp))

    // broadcast selected uop to other issue queues
    val selectedUop = ValidIO(new MicroOp)

    // send to exu
    val deq = DecoupledIO(new ExuInput)

    // to DataPart
    val toData = Output(new RSCtrlDataBundle(wakeupCnt, extraListenPortsCnt))

    // recv broadcasted uops form any relative issue queue,
    // to simplify wake up logic, the uop broadcasted by this queue self
    // are also in 'boradcastedUops'
    val broadcastedUops = Vec(wakeupCnt, Flipped(ValidIO(new MicroOp)))

    // for some function units with uncertain latency,
    // we have to wake up relative uops until those function units write back
    val extraListenPorts = Vec(extraListenPortsCnt, Flipped(ValidIO(new ExuOutput)))

    // to Dispatch
    val numExist = Output(UInt(iqIdxWidth.W))

    // TODO: support replay for future use if exu is ldu/stu
    val tlbFeedback = Flipped(ValidIO(new TlbFeedback)) // TODO: change its name
  })

  // control part:

  val s_idle :: s_valid :: s_wait :: s_replay :: Nil = Enum(4)
  
  val needFeedback  = if (feedback) true.B else false.B
  val stateQueue    = RegInit(VecInit(Seq.fill(iqSize)(s_idle)))
  val validQueue    = stateQueue.map(_ === s_valid)
  val emptyQueue    = stateQueue.map(_ === s_idle)
  val srcQueue      = Reg(Vec(iqSize, Vec(srcNum, new SrcBundle)))
  val cntQueue      = Reg(Vec(iqSize, UInt(log2Up(replayDelay).W)))

  // other part:
  val uop           = Reg(Vec(iqSize, new MicroOp)) // TODO: save uop here to use roqIdx for redirect, the other domain is useless and should not be used.

  // rs queue part:
  val tailPtr       = RegInit(0.U((iqIdxWidth+1).W))
  val idxQueue      = RegInit(VecInit((0 until iqSize).map(_.U(iqIdxWidth.W))))
  val readyQueue    = VecInit(srcQueue.map(a => ParallelAND(a.map(_.state === SrcState.rdy)).asBool).
  zip(validQueue).map{ case (a,b) => a&b })

  io.toData.wback <> DontCare // NOTE: need check later
  io.toData.extra <> DontCare

  // select ready
  // for no replay, select just equal to deq (attached)
  // with   replay, select is just two stage with deq.
  val moveMask = WireInit(0.U(iqSize.W))
  val selectedIdxRegOH = Wire(UInt(iqSize.W))
  val selectMask = WireInit(VecInit(
    (0 until iqSize).map(i =>
      readyQueue(i) && !(selectedIdxRegOH(i) && io.deq.fire()) 
      // TODO: add redirect here, may cause long latency , change it
    )
  ))
  val haveBubble = Wire(Bool())
  val (selectedIdxWire, selected) = PriorityEncoderWithFlag(selectMask)
  val redSel = uop(idxQueue(selectedIdxWire)).roqIdx.needFlush(io.redirect)
  val selValid = !redSel && selected && !haveBubble
  val selReg = RegNext(selValid)
  val selectedIdxReg = RegNext(selectedIdxWire - moveMask(selectedIdxWire))
  selectedIdxRegOH := UIntToOH(selectedIdxReg)

  // sel bubble
  // TODO:
  val bubIdxRegOH = Wire(UInt(iqSize.W))
  val bubMask = WireInit(VecInit(
    (0 until iqSize).map(i => emptyQueue(i) && !bubIdxRegOH(i))
  ))
  val (firstBubble, findBubble) = PriorityEncoderWithFlag(bubMask)
  haveBubble := findBubble && (firstBubble < tailPtr)
  val bubValid = haveBubble
  val bubReg = RegNext(bubValid)
  val bubIdxReg = RegNext(firstBubble - moveMask(firstBubble))
  bubIdxRegOH := UIntToOH(bubIdxReg)

  // deq
  // TODO: divide needFeedback and not needFeedback
  val deqValid = bubReg/*fire an bubble*/ || (selReg && io.deq.ready && !needFeedback/*fire an rdy*/)
  val deqIdx = Mux(bubReg, bubIdxReg, selectedIdxReg) // TODO: may have one more cycle delay than fire slot
  moveMask := {
    (Fill(iqSize, 1.U(1.W)) << deqIdx)(iqSize-1, 0)
  } & Fill(iqSize, deqValid)

  for(i <- 0 until iqSize-1){
    when(moveMask(i)){
      idxQueue(i)   := idxQueue(i+1)
      srcQueue(i).zip(srcQueue(i+1)).map{case (a,b) => a := b}
      stateQueue(i) := stateQueue(i+1)
    }
  }
  when(deqValid){
    idxQueue.last := idxQueue(deqIdx)
    stateQueue.last := s_idle
  }

  when (selReg && io.deq.ready && needFeedback) {
    stateQueue(selectedIdxReg) := s_wait
  }

  // redirect
  val redHitVec = (0 until iqSize).map(i => uop(idxQueue(i)).roqIdx.needFlush(io.redirect))
  val fbMatchVec  = (0 until iqSize).map(i => 
    uop(idxQueue(i)).roqIdx.asUInt === io.tlbFeedback.bits.roqIdx.asUInt && io.tlbFeedback.valid && (stateQueue(i) === s_wait || stateQueue(i)===s_valid)) 
    // TODO: feedback at the same cycle now, may change later
  //redHitVec.zip(validQueue).map{ case (r,v) => when (r) { v := false.B } }
  for (i <- 0 until iqSize) {
    val cnt = cntQueue(idxQueue(i))

    if (i != 0) { // TODO: combine the two case
      val nextIdx = i.U - moveMask(i-1)
      when (stateQueue(i)===s_replay) {
        when (cnt===0.U) { stateQueue(nextIdx) := s_valid }
        .otherwise { cnt := cnt - 1.U }
      }
      when (fbMatchVec(i)) {
        stateQueue(nextIdx) := Mux(io.tlbFeedback.bits.hit, s_idle, s_replay)
        cnt := Mux(io.tlbFeedback.bits.hit, cnt, (replayDelay-1).U)
      }
      when (redHitVec(i)) { stateQueue(nextIdx) := s_idle }
    } else { when (!moveMask(i)) {
      val nextIdx = i
      when (stateQueue(i)===s_replay) {
        when (cnt===0.U) { stateQueue(nextIdx) := s_valid }
        .otherwise { cnt := cnt - 1.U }
      }
      when (fbMatchVec(i)) {
        stateQueue(nextIdx) := Mux(io.tlbFeedback.bits.hit, s_idle, s_replay)
        cnt := Mux(io.tlbFeedback.bits.hit, cnt, (replayDelay-1).U)
      }
      when (redHitVec(i)) { stateQueue(nextIdx) := s_idle }
    }}
  }

  // bypass send
  // store selected uops and send out one cycle before result back
  def bpSelCheck(uop: MicroOp): Bool = { // TODO: wanna a map from FunctionUnit.scala
    val fuType = uop.ctrl.fuType
    (fuType === FuType.alu) ||
    (fuType === FuType.jmp) ||
    (fuType === FuType.i2f) ||
    (fuType === FuType.csr) ||
    (fuType === FuType.fence) ||
    (fuType === FuType.fmac)
  }
  val bpQueue = Module(new BypassQueue(fixedDelay))
  bpQueue.io.in.valid := selValid // FIXME: error when function is blocked => fu should not be blocked
  bpQueue.io.in.bits := uop(idxQueue(selectedIdxWire))
  bpQueue.io.redirect := io.redirect
  io.selectedUop.valid := bpQueue.io.out.valid && bpSelCheck(bpQueue.io.out.bits)
  io.selectedUop.bits  := bpQueue.io.out.bits
  if(fixedDelay > 0) {
    XSDebug(io.selectedUop.valid, p"SelBypass: pc:0x${Hexadecimal(io.selectedUop.bits.cf.pc)} roqIdx:${io.selectedUop.bits.roqIdx} pdest:${io.selectedUop.bits.pdest} rfWen:${io.selectedUop.bits.ctrl.rfWen} fpWen:${io.selectedUop.bits.ctrl.fpWen}\n" )
  }

  // output
  io.deq.valid := selReg && !uop(idxQueue(selectedIdxReg)).roqIdx.needFlush(io.redirect)// TODO: read it and add assert for rdyQueue
  io.deq.bits := DontCare // NOTE: Data part

  // enq
  val tailAfterRealDeq = tailPtr - (io.deq.fire() && !needFeedback|| bubReg)
  val isFull = tailAfterRealDeq.head(1).asBool() // tailPtr===qsize.U
  tailPtr := tailAfterRealDeq + io.enqCtrl.fire()

  io.enqCtrl.ready := !isFull && !io.redirect.valid // TODO: check this redirect && need more optimization
  val enqUop = io.enqCtrl.bits
  val srcTypeSeq = Seq(enqUop.ctrl.src1Type, enqUop.ctrl.src2Type, enqUop.ctrl.src3Type)
  val srcSeq = Seq(enqUop.psrc1, enqUop.psrc2, enqUop.psrc3)
  val srcStateSeq = Seq(enqUop.src1State, enqUop.src2State, enqUop.src3State)
  // val srcDataSeq = Seq(io.enqData.src1, io.enqData.src2, io.enqData.src3)

  val enqPtr = Mux(tailPtr.head(1).asBool, deqIdx, tailPtr.tail(1))
  val enqIdx_data = idxQueue(enqPtr)
  val enqIdx_ctrl = tailAfterRealDeq.tail(1)
  val enqIdxNext = RegNext(enqIdx_data) 
  val enqBpVec = (0 until srcNum).map(i => wakeup(SrcBundle(srcSeq(i), srcStateSeq(i), srcTypeSeq(i)), true.B))

  when (io.enqCtrl.fire()) {
    uop(enqIdx_data) := enqUop
    stateQueue(enqIdx_ctrl) := s_valid
    srcQueue(enqIdx_ctrl).zipWithIndex.map{ case (s,i) =>
      s := SrcBundle.check(srcSeq(i), Mux(enqBpVec(i)._1, SrcState.rdy, srcStateSeq(i)), srcTypeSeq(i)) }
    
    XSDebug(p"EnqCtrlFire: roqIdx:${enqUop.roqIdx} pc:0x${Hexadecimal(enqUop.cf.pc)} src1:${srcSeq(0)} state:${srcStateSeq(0)} type:${srcTypeSeq(0)} src2:${srcSeq(1)} state:${srcStateSeq(1)} type:${srcTypeSeq(1)} src3:${srcSeq(2)} state:${srcStateSeq(2)} type:${srcTypeSeq(2)} enqBpHit:${enqBpVec(0)._1}${enqBpVec(1)._1}${enqBpVec(2)._1}\n")
  }
  when (RegNext(io.enqCtrl.fire())) {
    for (i <- 0 until 3) { // TODO: beautify it
      when(RegNext(enqBpVec(i)._1)) { io.toData.wback(enqIdxNext)(i) := enqBpVec(i)._2 }
    }
  }

  def extra(src: SrcBundle, valid: Bool) : (Bool, Seq[Bool]) = {
    val hitVec = io.extraListenPorts.map(port => src.hit(port.bits.uop) && port.valid && valid)
    assert(RegNext(PopCount(hitVec)===0.U || PopCount(hitVec)===1.U))

    val hit = ParallelOR(hitVec)
    (hit, hitVec)
  }

  def wakeup(src: SrcBundle, valid: Bool) : (Bool, Seq[Bool]) = {
    val hitVec = io.broadcastedUops.map(port => src.hit(port.bits) && port.valid && valid)
    assert(RegNext(PopCount(hitVec)===0.U || PopCount(hitVec)===1.U))

    val hit = ParallelOR(hitVec)
    (hit, hitVec.map(RegNext(_)))
  }

  for (i <- 0 until iqSize) {
    for (j <- 0 until srcNum) {
      val (exHit, exHitVec) = extra(srcQueue(i)(j), validQueue(i))
      val (bpHit, bpHitVecReg) = wakeup(srcQueue(i)(j), validQueue(i))
      when (exHit || bpHit) { srcQueue(i.U - moveMask(i))(j).state := SrcState.rdy }
      when (bpHit) { io.toData.wback(i)(j) := bpHitVecReg }
      when (exHit) { io.toData.extra(i)(j) := exHitVec }

      XSDebug(exHit, p"WUHit: (${i.U})(${j.U}) idx:${idxQueue(i)}\n")
      XSDebug(bpHit, p"BPHit: (${i.U})(${j.U}) Ctrl idx:${idxQueue(i)}\n")
    }
  }

  // other to Data
  io.toData.enqPtr := enqIdx_ctrl
  io.toData.deqPtr.valid  := selValid
  io.toData.deqPtr.bits   := idxQueue(selectedIdxWire)
  io.toData.enqCtrl.valid := io.enqCtrl.fire
  io.toData.enqCtrl.bits  := io.enqCtrl.bits
  io.toData.deqValid := io.deq.valid // Note: just for debug

  // other io
  io.numExist := tailPtr

  // assert
  assert(tailPtr <= iqSize.U)

  // log
  // TODO: add log
  val print = io.enqCtrl.valid || io.deq.valid || ParallelOR(validQueue) || tailPtr=/=0.U || true.B
  XSDebug(print, p"In(${io.enqCtrl.valid} ${io.enqCtrl.ready}) Out(${io.deq.valid} ${io.deq.ready}) tailPtr:${tailPtr} tailPtr.tail:${tailPtr.tail(1)} tailADeq:${tailAfterRealDeq} isFull:${isFull} validQue:b${Binary(VecInit(validQueue).asUInt)} readyQueue:${Binary(readyQueue.asUInt)} needFeedback:${needFeedback}\n")
  XSDebug(io.redirect.valid && print, p"Redirect: roqIdx:${io.redirect.bits.roqIdx} isException:${io.redirect.bits.isException} isMisPred:${io.redirect.bits.isMisPred} isReplay:${io.redirect.bits.isReplay} isFlushPipe:${io.redirect.bits.isFlushPipe} RedHitVec:b${Binary(VecInit(redHitVec).asUInt)}\n")
  XSDebug(io.tlbFeedback.valid && print, p"TlbFeedback: roqIdx:${io.tlbFeedback.bits.roqIdx} hit:${io.tlbFeedback.bits.hit} fbMatchVec:${Binary(VecInit(fbMatchVec).asUInt)}\n")
  XSDebug(print, p"SelMask:b${Binary(selectMask.asUInt)} MoveMask:b${Binary(moveMask.asUInt)} rdyQue:b${Binary(readyQueue.asUInt)} selIdxWire:${selectedIdxWire} sel:${selected} redSel:${redSel} selValid:${selValid} selIdxReg:${selectedIdxReg} selReg:${selReg} haveBubble:${haveBubble} deqValid:${deqValid} firstBubble:${firstBubble} findBubble:${findBubble} bubReg:${bubReg} bubIdxReg:${bubIdxReg} selRegOH:b${Binary(selectedIdxRegOH)}\n")
  XSDebug(io.selectedUop.valid, p"Select: roqIdx:${io.selectedUop.bits.roqIdx} pc:0x${Hexadecimal(io.selectedUop.bits.cf.pc)} fuType:b${Binary(io.selectedUop.bits.ctrl.fuType)} FuOpType:b${Binary(io.selectedUop.bits.ctrl.fuOpType)}}\n")
  XSDebug(io.deq.fire, p"Deq: SelIdxReg:${selectedIdxReg} pc:0x${Hexadecimal(io.deq.bits.uop.cf.pc)} Idx:${idxQueue(selectedIdxReg)} roqIdx:${io.deq.bits.uop.roqIdx} src1:0x${Hexadecimal(io.deq.bits.src1)} src2:0x${Hexadecimal(io.deq.bits.src2)} src3:0x${Hexadecimal(io.deq.bits.src3)}\n")
  val broadcastedUops = io.broadcastedUops
  val extraListenPorts = io.extraListenPorts
  for (i <- broadcastedUops.indices) {
    XSDebug(broadcastedUops(i).valid && print, p"BpUops(${i.U}): pc:0x${Hexadecimal(broadcastedUops(i).bits.cf.pc)} roqIdx:${broadcastedUops(i).bits.roqIdx} idxQueue:${selectedIdxWire} pdest:${broadcastedUops(i).bits.pdest} rfWen:${broadcastedUops(i).bits.ctrl.rfWen} fpWen:${broadcastedUops(i).bits.ctrl.fpWen} \n")
  }
  for (i <- extraListenPorts.indices) {
    XSDebug(extraListenPorts(i).valid && print, p"WakeUp(${i.U}): pc:0x${Hexadecimal(extraListenPorts(i).bits.uop.cf.pc)} roqIdx:${extraListenPorts(i).bits.uop.roqIdx} pdest:${extraListenPorts(i).bits.uop.pdest} rfWen:${extraListenPorts(i).bits.uop.ctrl.rfWen} fpWen:${extraListenPorts(i).bits.uop.ctrl.fpWen}\n")
  }
  XSDebug(print, " :IQ|s|r|cnt| src1 |src2 | src3|pdest(rf|fp)| roqIdx|pc\n")
  for(i <- 0 until iqSize) {
    XSDebug(print, p"${i.U}: ${idxQueue(i)}|${stateQueue(i)}|${readyQueue(i)}| ${cntQueue(idxQueue(i))}|${srcQueue(i)(0)}|${srcQueue(i)(1)} |${srcQueue(i)(2)} |${uop(idxQueue(i)).pdest}(${uop(idxQueue(i)).ctrl.rfWen}|${uop(idxQueue(i)).ctrl.fpWen})|${uop(idxQueue(i)).roqIdx}|${Hexadecimal(uop(idxQueue(i)).cf.pc)}\n")
  }
}

class RSCtrlDataBundle(wakeupCnt: Int, extraCnt: Int) extends XSBundle {
  // TODO: current: Ctrl to Data, next: Data to Ctrl
  val wback = Vec(IssQueSize, Vec(3, Vec(wakeupCnt, Bool()))) // UInt(wakeupCnt.W)
  val extra = Vec(IssQueSize, Vec(3, Vec(extraCnt, Bool()))) // UInt(extraCnt.W )
  val enqPtr = UInt(log2Up(IssQueSize).W)
  val deqPtr = Valid(UInt(log2Up(IssQueSize).W)) // one cycle earlier
  val enqCtrl = Valid(new MicroOp)

  val deqValid = Bool() // Note: just for debug

  override def cloneType: this.type = (new RSCtrlDataBundle(wakeupCnt, extraCnt)).asInstanceOf[this.type]
}

class ReservationStationData
(
  val exuCfg: ExuConfig,
  wakeupCnt: Int,
  extraListenPortsCnt: Int,
  srcNum: Int = 3
) extends XSModule {
  
  val iqSize = IssQueSize
  val iqIdxWidth = log2Up(iqSize)

  val io = IO(new XSBundle {
    // flush
    // val redirect = Flipped(ValidIO(new Redirect))

    // enq Data at next cycle (regfile has 1 cycle latency)
    val enqData = Input(new ExuInput)

    // send to exu
    val deq = Output(new ExuInput)

    // listen to RSCtrl
    val fromCtrl = Input(new RSCtrlDataBundle(wakeupCnt, extraListenPortsCnt))

    // listen to write back data bus(certain latency)
    // and extra wrtie back(uncertan latency) 
    val writeBackedData = Vec(wakeupCnt, Input(UInt(XLEN.W)))
    val extraListenPorts = Vec(extraListenPortsCnt, Input(UInt(XLEN.W)))
  })
  // TODO: may move selectUop here, check it

  val uop     = Reg(Vec(iqSize, new MicroOp))
  val data    = Reg(Vec(iqSize, Vec(3, UInt(XLEN.W))))
  // TODO: change data's number

  val wback = io.fromCtrl.wback
  val extra = io.fromCtrl.extra
  val enq   = io.fromCtrl.enqPtr
  val deq   = RegEnable(io.fromCtrl.deqPtr.bits, io.fromCtrl.deqPtr.fire())
  val enqCtrl = io.fromCtrl.enqCtrl

  val enqPtr = enq(log2Up(IssQueSize)-1,0)
  val enqPtrReg = RegEnable(enqPtr, enqCtrl.fire())
  when (enqCtrl.fire()) {
    uop(enqPtr) := enqCtrl.bits
    XSDebug(p"enqCtrlFire: enqPtr:${enqPtr} pc:0x${Hexadecimal(enqCtrl.bits.cf.pc)} roqIdx:${enqCtrl.bits.roqIdx}\n")
  }

  when (RegNext(enqCtrl.fire())) { // TODO: turn to srcNum, not the 3
    data(enqPtrReg)(0) := io.enqData.src1
    data(enqPtrReg)(1) := io.enqData.src2
    data(enqPtrReg)(2) := io.enqData.src3
    XSDebug(p"enqDataFire: enqPtrReg:${enqPtrReg} src1:${Hexadecimal(io.enqData.src1)} src2:${Hexadecimal(io.enqData.src2)} src3:${Hexadecimal(io.enqData.src2)}\n")
  }

  wback.zipWithIndex.map{ case (e,i) => {
    data(i).zipWithIndex.map{ case (s, j) => {
      when (Cat(e(j)).orR) {
        val wbdata = ParallelMux(e(j) zip io.writeBackedData)
        data(i)(j) := wbdata
        XSDebug(p"WbackData:(${i.U})(${j}): ${Hexadecimal(wbdata)}\n")
      }
    }}
  }}

  extra.zipWithIndex.map{ case (e,i) => {
    data(i).zipWithIndex.map{ case (s, j) => {
      when (Cat(e(j)).orR) { 
        val exdata = ParallelMux(e(j) zip io.extraListenPorts)
        data(i)(j) := exdata
        XSDebug(p"ExData:(${i.U})(${j}): ${Hexadecimal(exdata)}\n")
      }
    }}
  }}

  io.deq.uop  := uop(deq)
  io.deq.src1 := data(deq)(0)
  io.deq.src2 := data(deq)(1)
  io.deq.src3 := data(deq)(2)

  XSDebug(io.fromCtrl.deqValid, p"Deq: pc:${Hexadecimal(io.deq.uop.cf.pc)} roqIdx:${io.deq.uop.roqIdx} src1:${Hexadecimal(io.deq.src1)} src2:${Hexadecimal(io.deq.src2)} src3:${Hexadecimal(io.deq.src3)}\n")
  XSDebug(p"Data:  | src1 | src2 | src3| roqIdx | pc\n")
  for(i <- data.indices) {
    XSDebug(p"${i.U} ${Hexadecimal(data(i)(0))}|${Hexadecimal(data(i)(1))}|${Hexadecimal(data(i)(2))}|${uop(i).roqIdx} | ${Hexadecimal(uop(i).cf.pc)}\n")
  }
}