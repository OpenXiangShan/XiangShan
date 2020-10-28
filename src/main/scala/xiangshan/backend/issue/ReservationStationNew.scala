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
  })
  require(number >= 1)
  if(number == 1) {  io.in <> io.out }
  else {
    val queue = Seq.fill(number-1)(RegInit(0.U.asTypeOf(new Bundle{
      val valid = Bool()
      val bits = new MicroOp
    })))
    queue(0).valid := io.in.valid
    queue(0).bits  := io.in.bits
    (0 until (number-2)).map{i => queue(i+1) := queue(i) }
    io.out.valid := queue(number-1).valid
    io.out.bits := queue(number-1).bits
    // TODO: change to ptr
    // val ptr = RegInit(0.U(log2Up(number-1).W))
    // val ptrNext = ptr + 1.U
    // ptr := Mux(ptrNext === number.U, 0.U, ptrNext)
  }
}

class ReservationStationNew
(
  val exuCfg: ExuConfig,
  wakeupCnt: Int,
  extraListenPortsCnt: Int,
  srcNum: Int = 3
) extends XSModule {


  val iqSize = IssQueSize
  val iqIdxWidth = log2Up(iqSize)

  val io = IO(new XSBundle {
    // flush Issue Queue
    val redirect = Flipped(ValidIO(new Redirect))

    // enq Ctrl sigs at dispatch-2
    val enqCtrl = Flipped(DecoupledIO(new MicroOp))
    // enq Data at next cycle (regfile has 1 cycle latency)
    val enqData = Input(new ExuInput)

    // broadcast selected uop to other issue queues
    val selectedUop = ValidIO(new MicroOp)

    // send to exu
    val deq = DecoupledIO(new ExuInput)

    // recv broadcasted uops form any relative issue queue,
    // to simplify wake up logic, the uop broadcasted by this queue self
    // are also in 'boradcastedUops'
    val broadcastedUops = Vec(wakeupCnt, Flipped(ValidIO(new MicroOp)))

    // listen to write back data bus
    val writeBackedData = Vec(wakeupCnt, Input(UInt(XLEN.W)))

    // for some function units with uncertain latency,
    // we have to wake up relative uops until those function units write back
    val extraListenPorts = Vec(extraListenPortsCnt, Flipped(ValidIO(new ExuOutput)))

    // to Dispatch
    val numExist = Output(UInt(iqIdxWidth.W))

    // TODO: support replay for future use if exu is ldu/stu
  })

//  io <> DontCare

  // GOAL: 
  // 1. divide control part and data part
  // 2. store control signal in sending RS and send out when after paticular cycles
  // 3. one RS only have one paticular delay
  // 4. remove the issue stage
  // 5. support replay will cause one or two more latency for state machine change
  //    so would not support replay in current edition.

  // here is three logial part:
  // control part: psrc(5.W)*3 srcState(1.W)*3 fuOpType/Latency(3.W) roqIdx
  // data part: data(64.W)*3
  // other part: lsroqIdx and many other signal in uop. may set them to control part(close to dispatch)

  // control part:
  val validQueue    = RegInit(VecInit(Seq.fill(iqSize)(false.B)))
  val srcQueue      = Reg(Vec(iqSize, Vec(srcNum, new SrcBundle)))
  // val srcQueue      = Seq.fill(iqSize)(Seq.fill(srcNum)(Reg(new SrcBundle)))

  // data part:
  val data          = Reg(Vec(iqSize, Vec(3, UInt(XLEN.W))))

  // other part:
  val uop           = Reg(Vec(iqSize, new MicroOp))

  // rs queue part:
  val tailPtr       = RegInit(0.U((iqIdxWidth+1).W))
  val idxQueue      = RegInit(VecInit((0 until iqSize).map(_.U(iqIdxWidth.W))))
  val readyQueue    = VecInit(srcQueue.map(a => ParallelAND(a.map(_.state === SrcState.rdy)).asBool).
  zip(validQueue).map{ case (a,b) => a&b })
  
  // select
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
  val (selectedIdxWire, selected) = PriorityEncoderWithFlag(selectMask)
  val redSel = uop(idxQueue(selectedIdxWire)).roqIdx.needFlush(io.redirect)
  val selValid = !redSel && selected
  val selReg = RegNext(selValid)
  val selectedIdxReg = RegNext(selectedIdxWire - moveMask(selectedIdxWire))
  selectedIdxRegOH := UIntToOH(selectedIdxReg)

  // real deQ
  // TODO: 
  val (firstBubble, findBubble) = PriorityEncoderWithFlag(validQueue.map(!_))
  val haveBubble = findBubble && (firstBubble < tailPtr)
  val deqValid = haveBubble/*fire an bubble*/ || (selReg && io.deq.ready/*fire an rdy*/)
  val deqIdx = Mux(haveBubble, firstBubble, selectedIdxReg) // TODO: may have one more cycle delay than fire slot
  moveMask := {
    (Fill(iqSize, 1.U(1.W)) << deqIdx)(iqSize-1, 0)
  } & Fill(iqSize, deqValid)

  for(i <- 0 until iqSize-1){
    when(moveMask(i)){
      idxQueue(i)   := idxQueue(i+1)
      srcQueue(i).zip(srcQueue(i+1)).map{case (a,b) => a := b}
      validQueue(i) := validQueue(i+1)
    }
  }
  when(deqValid){
    idxQueue.last := idxQueue(deqIdx)
    validQueue.last := false.B
  }

  // wakeup and bypass
  def wakeup(src: SrcBundle, valid: Bool) : (Bool, UInt) = {
    val hitVec = io.extraListenPorts.map(port => src.hit(port.bits.uop) && port.valid)
    assert(PopCount(hitVec)===0.U || PopCount(hitVec)===1.U)
    
    val hit = ParallelOR(hitVec) && valid
    (hit, ParallelMux(hitVec zip io.extraListenPorts.map(_.bits.data)))
  }

  def bypass(src: SrcBundle, valid: Bool) : (Bool, Bool, UInt) = {
    val hitVec = io.broadcastedUops.map(port => src.hit(port.bits) && port.valid)
    assert(PopCount(hitVec)===0.U || PopCount(hitVec)===1.U)

    val hit = ParallelOR(hitVec) && valid
    (hit, RegNext(hit), ParallelMux(hitVec.map(RegNext(_)) zip io.writeBackedData))
  }
  
  for (i <- 0 until iqSize) {
    for (j <- 0 until srcNum) {
      val (wuHit, wuData) = wakeup(srcQueue(i)(j), validQueue(i))
      val (bpHit, bpHitReg, bpData) = bypass(srcQueue(i)(j), validQueue(i))
      when (wuHit || bpHit) { srcQueue(i.U - moveMask(i))(j).state := SrcState.rdy }
      when (wuHit) { data(idxQueue(i))(j) := wuData }
      when (bpHitReg) { data(RegNext(idxQueue(i)))(j) := bpData }
      
      assert(!(bpHit && wuHit))
      assert(!(bpHitReg && wuHit))
      assert(!(srcQueue(i)(j).state === SrcState.rdy && (bpHit && wuHit)))
      XSDebug(wuHit, p"WUHit: (${i.U})(${j.U}) Data:0x${Hexadecimal(wuData)} idx:${idxQueue(i)}\n")
      XSDebug(bpHit, p"BPHit: (${i.U})(${j.U}) Ctrl idx:${idxQueue(i)}\n")
      XSDebug(bpHitReg, p"BPHit: (${i.U})(${j.U}) Data:0x${Hexadecimal(bpData)} idx:${idxQueue(i)}\n")
    }
  }

  // redirect
  val redHitVec = (0 until iqSize).map(i => uop(idxQueue(i)).roqIdx.needFlush(io.redirect))
  //redHitVec.zip(validQueue).map{ case (r,v) => when (r) { v := false.B } }
  for (i <- 0 until iqSize) {
    if (i != 0) {
      when (redHitVec(i)) { validQueue(i.U - moveMask(i-1)) := false.B }
    } else {
      // Nothing to do
    }
  }

  // bypass send
  // store selected uops and send out one cycle before result back
  val fixedDelay = 1 // TODO: fix it
  val bpQueue = Module(new BypassQueue(fixedDelay))
  bpQueue.io.in.valid := selValid
  bpQueue.io.in.bits := uop(idxQueue(selectedIdxWire))
  io.selectedUop.valid := bpQueue.io.out.valid
  io.selectedUop.bits  := bpQueue.io.out.bits

  // output
  io.deq.valid := selReg && !haveBubble && !uop(idxQueue(selectedIdxReg)).roqIdx.needFlush(io.redirect)// TODO: read it and add assert for rdyQueue
  io.deq.bits.uop := uop(idxQueue(selectedIdxReg))
  io.deq.bits.src1 := data(idxQueue(selectedIdxReg))(0)
  if(srcNum > 1) { io.deq.bits.src2 := data(idxQueue(selectedIdxReg))(1) }
  if(srcNum > 2) { io.deq.bits.src3 := data(idxQueue(selectedIdxReg))(2) } // TODO: beautify it

  // enq
  val tailAfterRealDeq = tailPtr - moveMask(tailPtr.tail(1))
  val isFull = tailAfterRealDeq.head(1).asBool() // tailPtr===qsize.U
  tailPtr := tailAfterRealDeq + io.enqCtrl.fire()

  io.enqCtrl.ready := !isFull && !io.redirect.valid // TODO: check this redirect && need more optimization
  val enqUop = io.enqCtrl.bits
  val srcTypeSeq = Seq(enqUop.ctrl.src1Type, enqUop.ctrl.src2Type, enqUop.ctrl.src3Type)
  val srcSeq = Seq(enqUop.psrc1, enqUop.psrc2, enqUop.psrc3)
  val srcStateSeq = Seq(enqUop.src1State, enqUop.src2State, enqUop.src3State)
  val srcDataSeq = Seq(io.enqData.src1, io.enqData.src2, io.enqData.src3)
  val enqIdxNext = RegNext(idxQueue(tailPtr.tail(1))) 
  val enqBpVec = (0 until srcNum).map(i => bypass(SrcBundle(srcSeq(i), srcStateSeq(i), srcTypeSeq(i)), true.B))

  when (io.enqCtrl.fire()) {
    uop(idxQueue(tailPtr.tail(1))) := enqUop 
    validQueue(tailAfterRealDeq.tail(1)) := true.B
    srcQueue(tailAfterRealDeq.tail(1)).zipWithIndex.map{ case (s,i) =>
      s := SrcBundle.check(srcSeq(i), Mux(enqBpVec(i)._1, SrcState.rdy, srcStateSeq(i)), srcTypeSeq(i)) }
    
    XSDebug(p"EnqCtrlFire: roqIdx:${enqUop.roqIdx} pc:0x${Hexadecimal(enqUop.cf.pc)} src1:${srcSeq(0)} state:${srcStateSeq(0)} type:${srcTypeSeq(0)} src2:${srcSeq(1)} state:${srcStateSeq(1)} type:${srcTypeSeq(1)} src3:${srcSeq(2)} state:${srcStateSeq(2)} type:${srcTypeSeq(2)} enqBpHit:${enqBpVec(0)._1}${enqBpVec(1)._1}${enqBpVec(2)._1}\n")
  }
  when (RegNext(io.enqCtrl.fire())) {
    for(i <- data(0).indices) { data(enqIdxNext)(i) := Mux(enqBpVec(i)._2, enqBpVec(i)._3, srcDataSeq(i)) }

    XSDebug(p"EnqDataFire: idx:${enqIdxNext} src1:0x${Hexadecimal(srcDataSeq(0))} src2:0x${Hexadecimal(srcDataSeq(1))} src3:0x${Hexadecimal(srcDataSeq(2))} enqBpHit:(${enqBpVec(0)._2}|0x${Hexadecimal(enqBpVec(0)._3)})(${enqBpVec(1)._2}|0x${Hexadecimal(enqBpVec(1)._3)})(${enqBpVec(2)._2}|0x${Hexadecimal(enqBpVec(2)._3)}\n")
  }

  // other io
  io.numExist := tailPtr

  // log
  // TODO: add log
  XSDebug(io.enqCtrl.valid || io.deq.valid || ParallelOR(validQueue), p"In(${io.enqCtrl.valid} ${io.enqCtrl.ready}) Out(${io.deq.valid} ${io.deq.ready}) tailPtr:${tailPtr} tailPtr.tail:${tailPtr.tail(1)} tailADeq:${tailAfterRealDeq} isFull:${isFull} validQue:b${Binary(validQueue.asUInt)} readyQueue:${Binary(readyQueue.asUInt)}\n")
  XSDebug(io.redirect.valid && (io.enqCtrl.valid || io.deq.valid || ParallelOR(validQueue)), p"Redirect: roqIdx:${io.redirect.bits.roqIdx} isException:${io.redirect.bits.isException} isMisPred:${io.redirect.bits.isMisPred} isReplay:${io.redirect.bits.isReplay} isFlushPipe:${io.redirect.bits.isFlushPipe} RedHitVec:b${Binary(VecInit(redHitVec).asUInt)}\n")
  XSDebug(io.enqCtrl.valid || io.deq.valid || ParallelOR(validQueue), p"SelMask:b${Binary(selectMask.asUInt)} MoveMask:b${Binary(moveMask.asUInt)} rdyQue:b${Binary(readyQueue.asUInt)} selIdxWire:${selectedIdxWire} sel:${selected} redSel:${redSel} selValid:${selValid} selIdxReg:${selectedIdxReg} selReg:${selReg} haveBubble:${haveBubble} deqValid:${deqValid} firstBubble:${firstBubble} findBubble:${findBubble} selRegOH:b${Binary(selectedIdxRegOH)}\n")
  XSDebug(io.selectedUop.valid, p"Select: roqIdx:${io.selectedUop.bits.roqIdx} pc:0x${Hexadecimal(io.selectedUop.bits.cf.pc)} fuType:b${Binary(io.selectedUop.bits.ctrl.fuType)} FuOpType:b${Binary(io.selectedUop.bits.ctrl.fuOpType)} fixedDelay:${fixedDelay.U}\n")
  XSDebug(io.deq.fire, p"Deq: SelIdxReg:${selectedIdxReg} pc:0x${Hexadecimal(io.deq.bits.uop.cf.pc)} Idx:${idxQueue(selectedIdxReg)} roqIdx:${io.deq.bits.uop.roqIdx} src1:0x${Hexadecimal(io.deq.bits.src1)} src2:0x${Hexadecimal(io.deq.bits.src2)} src3:0x${Hexadecimal(io.deq.bits.src3)}\n")
  val broadcastedUops = io.broadcastedUops
  val extraListenPorts = io.extraListenPorts
  for (i <- broadcastedUops.indices) {
    XSDebug(broadcastedUops(i).valid && (io.enqCtrl.valid || io.deq.valid || ParallelOR(validQueue)), p"BpUops(${i.U}): pc:0x${Hexadecimal(broadcastedUops(i).bits.cf.pc)} roqIdx:${broadcastedUops(i).bits.roqIdx} idxQueue:${selectedIdxWire} pdest:${broadcastedUops(i).bits.pdest} rfWen:${broadcastedUops(i).bits.ctrl.rfWen} fpWen:${broadcastedUops(i).bits.ctrl.fpWen} data(last):0x${Hexadecimal(io.writeBackedData(i))}\n")
    XSDebug(RegNext(broadcastedUops(i).valid && (io.enqCtrl.valid || io.deq.valid || ParallelOR(validQueue))),  p"BpUopData(${i.U}): data(last):0x${Hexadecimal(io.writeBackedData(i))}\n")
  }
  for (i <- extraListenPorts.indices) {
    XSDebug(extraListenPorts(i).valid && (io.enqCtrl.valid || io.deq.valid || ParallelOR(validQueue)), p"WakeUp(${i.U}): pc:0x${Hexadecimal(extraListenPorts(i).bits.uop.cf.pc)} roqIdx:${extraListenPorts(i).bits.uop.roqIdx} pdest:${extraListenPorts(i).bits.uop.pdest} rfWen:${extraListenPorts(i).bits.uop.ctrl.rfWen} fpWen:${extraListenPorts(i).bits.uop.ctrl.fpWen} data:0x${Hexadecimal(extraListenPorts(i).bits.data)}\n")
  }
  XSDebug(ParallelOR(validQueue), "  : IQ|v|r| src1 |src2 | src3|pdest(rf|fp)| roqIdx|pc\n")
  for(i <- 0 until iqSize) {
    XSDebug(validQueue(i), p"${i.U}: ${idxQueue(i)}|${validQueue(i)}|${readyQueue(i)}|${srcQueue(i)(0)} 0x${Hexadecimal(data(idxQueue(i))(0))}|${srcQueue(i)(1)} 0x${Hexadecimal(data(idxQueue(i))(1))}|${srcQueue(i)(2)} 0x${Hexadecimal(data(idxQueue(i))(2))}|${uop(idxQueue(i)).pdest}(${uop(idxQueue(i)).ctrl.rfWen}|${uop(idxQueue(i)).ctrl.fpWen})|${uop(idxQueue(i)).roqIdx}|${Hexadecimal(uop(idxQueue(i)).cf.pc)}\n")
  }
}
