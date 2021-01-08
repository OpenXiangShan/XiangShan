package xiangshan.backend.issue

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.backend.exu.{Exu, ExuConfig}

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
    io.out.valid := io.in.valid
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
    io.out.valid := queue(number-1).valid
    io.out.bits := queue(number-1).bits
    for (i <- 0 until number) {
      XSDebug(queue(i).valid, p"BPQue(${i.U}): pc:${Hexadecimal(queue(i).bits.cf.pc)} roqIdx:${queue(i).bits.roqIdx}" +
        p" pdest:${queue(i).bits.pdest} rfWen:${queue(i).bits.ctrl.rfWen} fpWen${queue(i).bits.ctrl.fpWen}\n")
    }
  }
}

class RSCtrlDataIO extends XSBundle {
  // TODO: current: Ctrl to Data, next: Data to Ctrl
  val enqPtr = Output(UInt(log2Up(IssQueSize).W))
  val deqPtr = ValidIO(UInt(log2Up(IssQueSize).W)) // one cycle earlier
  val enqCtrl = ValidIO(new MicroOp)

  val fuReady   = Input(Bool())
  val srcUpdate = Input(Vec(IssQueSize+1, Vec(3, Bool()))) // Note: the last one for enq
  val redVec    = Input(UInt(IssQueSize.W))
  val feedback  = Input(Vec(IssQueSize+1, Bool())) // Note: the last one for hit
}

class ReservationStationCtrl
(
  val exuCfg: ExuConfig,
  wakeupCnt: Int,
  extraListenPortsCnt: Int,
  srcNum: Int = 3,
  feedback: Boolean,
  fixedDelay: Int,
  replayDelay: Int = 10
) extends XSModule with HasCircularQueuePtrHelper {

  val iqSize = IssQueSize
  val iqIdxWidth = log2Up(iqSize)
  val fastWakeup = fixedDelay >= 0 // NOTE: if do not enable fastWakeup(bypass), set fixedDelay to -1
  val nonBlocked = fastWakeup

  val io = IO(new XSBundle {
    // flush
    val redirect = Flipped(ValidIO(new Redirect))

    // enq Ctrl sigs at dispatch-2, only use srcState
    val enqCtrl = Flipped(DecoupledIO(new MicroOp))

    // to DataPart
    val data = new RSCtrlDataIO

    // to Dispatch
    val numExist = Output(UInt(iqIdxWidth.W))
  })

  /* there two kind of data
   * 0 : indexed by indexed queue, such as data queue
   * 1 : indexed like indexed queue
   * TODO : all the queue turn to 0 type except indexed queue
   */

  /* queue in ctrl part
   * index queue : index
   * state queue : use for replay
   * valid queue : from state queue, valid or not
   * empty queue : from state queue, empty or not(not valid and not replay)
   * src   queue : record rdy or not
   * cnt   queue : record replay cycle
   */


  val s_idle :: s_valid :: s_selected :: s_bubble :: s_wait :: s_replay :: Nil = Enum(6)
  /* state machine
   * s_idle     : empty slot, init state, set when deq
   * s_valid    : ready to be secleted
   * s_selected : the not bubble that selected
   * s_bubble   : the bubble that selected
   * s_wait     : wait for feedback
   * s_replay   : replay after some particular cycle
   */
  val stateQueue    = RegInit(VecInit(Seq.fill(iqSize)(s_idle)))
  val validQueue    = VecInit(stateQueue.map(_ === s_valid))
  val emptyQueue    = VecInit(stateQueue.map(_ === s_idle))
  val srcQueue      = Reg(Vec(iqSize, Vec(srcNum, Bool())))
  val cntQueue      = Reg(Vec(iqSize, UInt(log2Up(replayDelay).W)))

  // rs queue part:
  // val tailPtr       = RegInit(0.U((iqIdxWidth+1).W))
  val tailPtr       = RegInit(0.U.asTypeOf(new CircularQueuePtr(iqSize)))
  val idxQueue      = RegInit(VecInit((0 until iqSize).map(_.U(iqIdxWidth.W))))

  // turn to indexed index
  def widthMap[T <: Data](f: Int => T) = VecInit((0 until iqSize).map(f))
  val stateIdxQue = widthMap(i => stateQueue(idxQueue(i))) // NOTE: only use for debug, remove it later
  val validIdxQue = widthMap(i => validQueue(idxQueue(i)))
  val emptyIdxQue = widthMap(i => emptyQueue(idxQueue(i)))
  val srcIdxQue   = widthMap(i => srcQueue(idxQueue(i)))
  val cntIdxQue   = widthMap(i => cntQueue(idxQueue(i))) // NOTE: only use for debug, remove it later
  val readyIdxQue = VecInit(srcIdxQue.zip(validIdxQue).map{ case (a,b) => Cat(a).andR & b })

  // redirect
  val redVec      = io.data.redVec
  val redVecPtr   = widthMap(i => io.data.redVec(idxQueue(i)))
  val fbMatchVec = 0.U(iqSize.W)
  if (feedback) {
    val fbMatchVec = widthMap(i => io.data.feedback(i) && (stateQueue(i) === s_wait || stateQueue(i)===s_valid))
  }
  val fbHit       = io.data.feedback(IssQueSize)

  // select ready
  // for no replay, select just equal to deq (attached)
  // with   replay, select is just two stage with deq.
  val issFire = Wire(Bool())
  val moveMask = WireInit(0.U(iqSize.W))
  val selectMask = WireInit(VecInit((0 until iqSize).map(i => readyIdxQue(i))))
  val haveBubble = Wire(Bool())
  // val selIdx = ParallelMux(selectMask zip idxQueue) // NOTE: the idx in the idxQueue
  val (selPtr, haveReady) = PriorityEncoderWithFlag(selectMask) // NOTE: the idx of idxQueue
  val selIdx = idxQueue(selPtr)
  val selIdxReg = RegNext(selIdx) // NOTE: may dup with other signal, fix it later
  val redSel = redVec(selIdx)
  val selValid = !redSel && haveReady && !haveBubble
  val selReg = RegNext(selValid)
  val selPtrReg = RegNext(selPtr - moveMask(selPtr)) // TODO: deal with the long latency

  // sel bubble
  val bubMask = WireInit(VecInit((0 until iqSize).map(i => emptyIdxQue(i))))
  // val bubIdx = ParallelMux(bubMask zip idxQueue) // NOTE: the idx in the idxQueue
  val (bubPtr, findBubble) = PriorityEncoderWithFlag(bubMask) // NOTE: the idx of the idxQueue
  haveBubble := findBubble && (bubPtr < tailPtr.asUInt)
  val bubIdx = idxQueue(bubPtr)
  val bubIdxReg = RegNext(bubIdx) // NOTE: may dup with other signal, fix it later
  val bubValid = haveBubble
  val bubReg = RegNext(bubValid)
  val bubPtrReg = RegNext(bubPtr - moveMask(bubPtr)) // TODO: deal with the long latency

  // deq
  // TODO: mem's rs will issue but not deq( the bub), so just divide issue and deq
  // TODO: when need feadback, only deq when becomes bubble
  val dequeue = if (feedback) bubReg else bubReg || issFire
  val deqPtr = Mux(bubReg, bubPtrReg, selPtrReg)
  moveMask := {
    (Fill(iqSize, 1.U(1.W)) << deqPtr)(iqSize-1, 0)
  } & Fill(iqSize, dequeue)

  // move, move happens when deq
  for(i <- 0 until iqSize-1){
    when(moveMask(i)){
      idxQueue(i) := idxQueue(i+1)
    }
  }
  when(dequeue){
    idxQueue.last := idxQueue(deqPtr)
  }
  when (selValid) {
    stateQueue(selIdx) := s_selected
    // TODO: may have long latency
  }
  when (haveBubble) {
    stateQueue(bubIdx) := s_bubble
  }

  when (stateQueue(selIdxReg) === s_selected) {
    when (io.data.fuReady) {
      if (feedback) {
        stateQueue(selIdxReg) := s_wait
      } else {
        stateQueue(selIdxReg) := s_idle
      }
    }.otherwise { stateQueue(selIdxReg) := s_valid } // fu is not ready and re-select next cycle
  }
  when (stateQueue(bubIdxReg) === s_bubble) {
    stateQueue(bubIdxReg) := s_idle // move the bubble to the last positon
  }

  // redirect and feedback && wakeup
  for (i <- 0 until iqSize) {
    // replay
    val cnt = cntQueue(i)
    when (stateQueue(i) === s_replay) {
      when (cnt === 0.U) { stateQueue(i) := s_valid }
      .otherwise { cnt := cnt - 1.U }
    }
    // feedback
    when (fbMatchVec(i)) {
      stateQueue(i) := Mux(fbHit, s_idle, s_replay)
      cntQueue(i) := Mux(fbHit, cnt, (replayDelay-1).U)
    }
    // wakeup
    val hitVec = io.data.srcUpdate(i)
    for (j <- 0 until srcNum) {
      when (hitVec(j) && validQueue(i)) {
        srcQueue(i)(j) := true.B
        XSDebug(p"srcHit: i:${i.U} j:${j.U} src:${srcQueue(i)(j)}\n")
      }
    }
    // redirect
    when (redVec(i)) {
      stateQueue(i) := s_idle
    }
  }

  // output
  val issValid = selReg && !redVecPtr(selPtrReg)
  if (nonBlocked) {
    issFire := issValid
    assert(RegNext(io.data.fuReady), "if fu wanna fast wakeup, it should not block")
  } else {
    issFire := issValid && io.data.fuReady
  }

  // enq
  val isFull = tailPtr.flag && !dequeue
  // agreement with dispatch: don't fire when io.redirect.valid
  val enqueue = io.enqCtrl.fire() && !io.redirect.valid
  val tailInc = tailPtr+1.U
  val tailDec = tailPtr-1.U
  tailPtr := Mux(dequeue === enqueue, tailPtr, Mux(dequeue, tailDec, tailInc))

  io.enqCtrl.ready := !isFull
  val enqUop      = io.enqCtrl.bits
  val srcSeq      = Seq(enqUop.psrc1, enqUop.psrc2, enqUop.psrc3)
  val srcTypeSeq  = Seq(enqUop.ctrl.src1Type, enqUop.ctrl.src2Type, enqUop.ctrl.src3Type)
  val srcStateSeq = Seq(enqUop.src1State, enqUop.src2State, enqUop.src3State)

  val enqPtr = Mux(tailPtr.flag, deqPtr, tailPtr.value)
  val enqIdx = idxQueue(enqPtr)
  val enqBpVec = io.data.srcUpdate(IssQueSize)

  def stateCheck(src: UInt, srcType: UInt): Bool = {
    (srcType =/= SrcType.reg && srcType =/= SrcType.fp) ||
    (srcType === SrcType.reg && src === 0.U)
  }

  when (enqueue) {
    stateQueue(enqIdx) := s_valid
    srcQueue(enqIdx).zipWithIndex.map{ case (s, i) =>
      s := Mux(enqBpVec(i) || stateCheck(srcSeq(i), srcTypeSeq(i)), true.B,
               srcStateSeq(i)===SrcState.rdy)
    }
    XSDebug(p"EnqCtrl: roqIdx:${enqUop.roqIdx} pc:0x${Hexadecimal(enqUop.cf.pc)} " +
      p"src1:${srcSeq(0)} state:${srcStateSeq(0)} type:${srcTypeSeq(0)} src2:${srcSeq(1)} " +
      p" state:${srcStateSeq(1)} type:${srcTypeSeq(1)} src3:${srcSeq(2)} state:${srcStateSeq(2)} " +
      p"type:${srcTypeSeq(2)}\n")
  }

  // other to Data
  io.data.enqPtr := enqIdx
  io.data.deqPtr.valid  := selValid
  io.data.deqPtr.bits   := selIdx
  io.data.enqCtrl.valid := enqueue
  io.data.enqCtrl.bits  := io.enqCtrl.bits

  // other io
  io.numExist := Mux(tailPtr.flag, (iqSize-1).U, tailPtr.value) // NOTE: numExist is iqIdxWidth.W, maybe a bug

  // assert
  assert(RegNext(Mux(tailPtr.flag, tailPtr.value===0.U, true.B)))

  val print = !(tailPtr.asUInt===0.U) || io.enqCtrl.valid || enqueue || dequeue
  XSDebug(print || true.B, p"In(${io.enqCtrl.valid} ${io.enqCtrl.ready}) Out(${issValid} ${io.data.fuReady}) nonBlocked:${nonBlocked.B} needfb:${feedback.B}\n")
  XSDebug(print , p"tailPtr:${tailPtr} enq:${enqueue} deq:${dequeue} isFull:${isFull} " +
    p"vIdxQue:${Binary(validIdxQue.asUInt)} rIdxQue:${Binary(readyIdxQue.asUInt)}\n")
  XSDebug(print && Cat(redVecPtr).orR, p"Redirect: ${Hexadecimal(redVecPtr.asUInt)}\n")
  XSDebug(print && Cat(fbMatchVec).orR, p"Feedback: ${Hexadecimal(fbMatchVec.asUInt)} Hit:${fbHit}\n")
  XSDebug(print, p"moveMask:${Binary(moveMask)} selMask:${Binary(selectMask.asUInt)} haveBub:${haveBubble}\n")
  XSDebug(print, p"selIdxWire:${selPtr} haveReady:${haveReady} redSel:${redSel}" +
    p"selV:${selValid} selReg:${selReg} selPtrReg:${selPtrReg} selIdx:${selIdx} selIdxReg:${selIdxReg}\n")
  XSDebug(print, p"bubMask:${Binary(bubMask.asUInt)} bubPtr:${bubPtr} findBub:${findBubble} " +
    p"bubReg:${bubReg} bubPtrReg:${bubPtrReg} bubIdx:${bubIdx} bubIdxReg:${bubIdxReg}\n")
  XSDebug(print, p"issValid:${issValid} issueFire:${issFire} dequeue:${dequeue} deqPtr:${deqPtr}\n")
  XSDebug(p" :Idx|v|r|s |cnt|s1:s2:s3\n")
  for(i <- srcQueue.indices) {
    XSDebug(p"${i.U}: ${idxQueue(i)}|${validIdxQue(i)}|${readyIdxQue(i)}|${stateIdxQue(i)}|" +
      p"${cntIdxQue(i)}|${srcIdxQue(i)(0)}:${srcIdxQue(i)(1)}:${srcIdxQue(i)(2)}\n")
  }
}

class ReservationStationData
(
  val exuCfg: ExuConfig,
  wakeupCnt: Int,
  extraListenPortsCnt: Int,
  fixedDelay: Int,
  feedback: Boolean,
  srcNum: Int = 3
) extends XSModule {

  val iqSize = IssQueSize
  val iqIdxWidth = log2Up(iqSize)
  val fastWakeup = fixedDelay >= 0 // NOTE: if do not enable fastWakeup(bypass), set fixedDelay to -1
  val nonBlocked = fastWakeup

  val io = IO(new XSBundle {
    // flush
    val redirect = Flipped(ValidIO(new Redirect))

    // enq Data at next cycle (regfile has 1 cycle latency)
    val enqData = Input(new ExuInput)

    // send to exu
    val deq = DecoupledIO(new ExuInput)

    // listen to RSCtrl
    val ctrl = Flipped(new RSCtrlDataIO)

    // broadcast selected uop to other issue queues
    val selectedUop = ValidIO(new MicroOp)

    // recv broadcasted uops form any relative issue queue,
    // to simplify wake up logic, the uop broadcasted by this queue self
    // are also in 'boradcastedUops'
    val broadcastedUops = Vec(wakeupCnt, Flipped(ValidIO(new MicroOp)))

    // listen to write back data bus(certain latency)
    // and extra wrtie back(uncertan latency)
    val writeBackedData = Vec(wakeupCnt, Input(UInt(XLEN.W)))
    val extraListenPorts = Vec(extraListenPortsCnt, Flipped(ValidIO(new ExuOutput)))

    // tlb feedback
    val feedback = Flipped(ValidIO(new TlbFeedback))
  })

  val uop     = Reg(Vec(iqSize, new MicroOp))
  val data    = Reg(Vec(iqSize, Vec(srcNum, UInt(XLEN.W))))

  // TODO: change srcNum

  val enq   = io.ctrl.enqPtr
  val sel   = io.ctrl.deqPtr
  val deq   = RegEnable(sel.bits, sel.valid)
  val enqCtrl = io.ctrl.enqCtrl
  val enqUop = enqCtrl.bits

  // enq
  val enqPtr = enq(log2Up(IssQueSize)-1,0)
  val enqPtrReg = RegEnable(enqPtr, enqCtrl.valid)
  val enqEn  = enqCtrl.valid
  val enqEnReg = RegNext(enqEn)
  when (enqEn) {
    uop(enqPtr) := enqUop
    XSDebug(p"enqCtrl: enqPtr:${enqPtr} src1:${enqUop.psrc1}|${enqUop.src1State}|${enqUop.ctrl.src1Type}" +
      p" src2:${enqUop.psrc2}|${enqUop.src2State}|${enqUop.ctrl.src2Type} src3:${enqUop.psrc3}|" +
      p"${enqUop.src3State}|${enqUop.ctrl.src3Type} pc:0x${Hexadecimal(enqUop.cf.pc)} roqIdx:${enqUop.roqIdx}\n")
  }
  when (enqEnReg) { // TODO: turn to srcNum, not the 3
    data(enqPtrReg)(0) := io.enqData.src1
    data(enqPtrReg)(1) := io.enqData.src2
    data(enqPtrReg)(2) := io.enqData.src3
    XSDebug(p"enqData: enqPtrReg:${enqPtrReg} src1:${Hexadecimal(io.enqData.src1)}" +
            p" src2:${Hexadecimal(io.enqData.src2)} src3:${Hexadecimal(io.enqData.src2)}\n")
  }

  def wbHit(uop: MicroOp, src: UInt, srctype: UInt): Bool = {
    (src === uop.pdest) &&
    ((srctype === SrcType.reg && uop.ctrl.rfWen && src=/=0.U) ||
     (srctype === SrcType.fp  && uop.ctrl.fpWen))
  }

  // wakeup and bypass
  def wakeup(src: UInt, srcType: UInt, valid: Bool = true.B) : (Bool, UInt) = {
    val hitVec = io.extraListenPorts.map(port => wbHit(port.bits.uop, src, srcType) && port.valid && valid)
    assert(RegNext(PopCount(hitVec)===0.U || PopCount(hitVec)===1.U))

    val hit = ParallelOR(hitVec)
    (hit, ParallelMux(hitVec zip io.extraListenPorts.map(_.bits.data)))
  }

  def bypass(src: UInt, srcType: UInt, valid: Bool = true.B) : (Bool, Bool, UInt) = {
    val hitVec = io.broadcastedUops.map(port => wbHit(port.bits, src, srcType) && port.valid && valid)
    assert(RegNext(PopCount(hitVec)===0.U || PopCount(hitVec)===1.U))

    val hit = ParallelOR(hitVec)
    (hit, RegNext(hit), ParallelMux(hitVec.map(RegNext(_)) zip io.writeBackedData))
  }

  io.ctrl.srcUpdate.map(a => a.map(_ := false.B))
  for (i <- 0 until iqSize) {
    val srcSeq = Seq(uop(i).psrc1, uop(i).psrc2, uop(i).psrc3)
    val srcTypeSeq = Seq(uop(i).ctrl.src1Type, uop(i).ctrl.src2Type, uop(i).ctrl.src3Type)
    for (j <- 0 until 3) {
      val (wuHit, wuData) = wakeup(srcSeq(j), srcTypeSeq(j))
      val (bpHit, bpHitReg, bpData) = bypass(srcSeq(j), srcTypeSeq(j))
      when (wuHit || bpHit) { io.ctrl.srcUpdate(i)(j) := true.B }
      when (wuHit) { data(i)(j) := wuData }
      when (bpHitReg && !(enqPtrReg===i.U && enqEnReg)) { data(i)(j) := bpData }
      // NOTE: the hit is from data's info, so there is an erro that:
      //       when enq, hit use last instr's info not the enq info.
      //       it will be long latency to add correct here, so add it to ctrl or somewhere else
      //       enq bp is done at below
      XSDebug(wuHit, p"WUHit: (${i.U})(${j.U}) Data:0x${Hexadecimal(wuData)}\n")
      XSDebug(bpHit, p"BPHit: (${i.U})(${j.U})\n")
      XSDebug(bpHitReg, p"BPHitData: (${i.U})(${j.U}) Data:0x${Hexadecimal(bpData)}\n")
    }
  }

  // deq
  io.deq.bits.uop  := uop(deq)
  io.deq.bits.src1 := data(deq)(0)
  io.deq.bits.src2 := data(deq)(1)
  io.deq.bits.src3 := data(deq)(2)
  io.deq.valid := RegNext(sel.valid)
  if (nonBlocked) { assert(RegNext(io.deq.ready), s"${name} if fu wanna fast wakeup, it should not block")}

  // to ctrl
  val srcSeq = Seq(enqUop.psrc1, enqUop.psrc2, enqUop.psrc3)
  val srcTypeSeq = Seq(enqUop.ctrl.src1Type, enqUop.ctrl.src2Type, enqUop.ctrl.src3Type)
  io.ctrl.srcUpdate(IssQueSize).zipWithIndex.map{ case (h, i) =>
    val (bpHit, bpHitReg, bpData)= bypass(srcSeq(i), srcTypeSeq(i), enqCtrl.valid)
    when (bpHitReg) { data(enqPtrReg)(i) := bpData }
    h := bpHit
    // NOTE: enq bp is done here
    XSDebug(bpHit, p"EnqBPHit: (${i.U})\n")
    XSDebug(bpHitReg, p"EnqBPHitData: (${i.U}) data:${Hexadecimal(bpData)}\n")
  }
  if (nonBlocked) { io.ctrl.fuReady := true.B }
  else { io.ctrl.fuReady := io.deq.ready }
  io.ctrl.redVec   := VecInit(uop.map(_.roqIdx.needFlush(io.redirect))).asUInt

  io.ctrl.feedback := DontCare
  if (feedback) {
    (0 until IssQueSize).map(i =>
      io.ctrl.feedback(i) := uop(i).roqIdx.asUInt === io.feedback.bits.roqIdx.asUInt && io.feedback.valid)
    io.ctrl.feedback(IssQueSize) := io.feedback.bits.hit
  }


  // bypass send
  io.selectedUop <> DontCare
  if (fastWakeup) {
    val bpQueue = Module(new BypassQueue(fixedDelay))
    bpQueue.io.in.valid := sel.valid // FIXME: error when function is blocked => fu should not be blocked
    bpQueue.io.in.bits := uop(sel.bits)
    bpQueue.io.redirect := io.redirect
    io.selectedUop.valid := bpQueue.io.out.valid
    io.selectedUop.bits  := bpQueue.io.out.bits

    XSDebug(io.selectedUop.valid, p"SelUop: pc:0x${Hexadecimal(io.selectedUop.bits.cf.pc)}" +
      p" roqIdx:${io.selectedUop.bits.roqIdx} pdest:${io.selectedUop.bits.pdest} " +
      p"rfWen:${io.selectedUop.bits.ctrl.rfWen} fpWen:${io.selectedUop.bits.ctrl.fpWen}\n" )
  }


  // log
  XSDebug(io.ctrl.redVec.orR, p"Red: ${Binary(io.ctrl.redVec)}\n")
  XSDebug(io.feedback.valid && feedback.B, p"feedback: roqIdx:${io.feedback.bits.roqIdx} hit:${io.feedback.bits.hit}\n")
  XSDebug(true.B, p"out(${io.deq.valid} ${io.deq.ready})\n")
  XSDebug(io.deq.valid, p"Deq(${io.deq.valid} ${io.deq.ready}): deqPtr:${deq} pc:${Hexadecimal(io.deq.bits.uop.cf.pc)}" +
    p" roqIdx:${io.deq.bits.uop.roqIdx} src1:${Hexadecimal(io.deq.bits.src1)} " +
    p" src2:${Hexadecimal(io.deq.bits.src2)} src3:${Hexadecimal(io.deq.bits.src3)}\n")
  XSDebug(p"Data:  | src1:data | src2:data | src3:data |hit|pdest:rf:fp| roqIdx | pc\n")
  for(i <- data.indices) {
    XSDebug(p"${i.U}:|${uop(i).psrc1}:${Hexadecimal(data(i)(0))}|${uop(i).psrc2}:" +
      p"${Hexadecimal(data(i)(1))}|${uop(i).psrc3}:${Hexadecimal(data(i)(2))}|" +
      p"${Binary(io.ctrl.srcUpdate(i).asUInt)}|${uop(i).pdest}:${uop(i).ctrl.rfWen}:" +
      p"${uop(i).ctrl.fpWen}|${uop(i).roqIdx} |${Hexadecimal(uop(i).cf.pc)}\n")
  }
}
