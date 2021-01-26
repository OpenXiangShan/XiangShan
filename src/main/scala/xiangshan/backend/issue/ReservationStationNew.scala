package xiangshan.backend.issue

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.backend.SelImm
import xiangshan.backend.decode.{ImmUnion, Imm_U}
import xiangshan.backend.exu.{Exu, ExuConfig}
import xiangshan.backend.regfile.RfReadPort

import scala.math.max

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
    // NOTE: no delay bypass don't care redirect
  } else {
    val queue = Seq.fill(number)(RegInit(0.U.asTypeOf(new Bundle{
      val valid = Bool()
      val bits = new MicroOp
    })))
    queue(0).valid := io.in.valid && !io.in.bits.roqIdx.needFlush(io.redirect)
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

class RSCtrlDataIO(srcNum: Int) extends XSBundle {
  // TODO: current: Ctrl to Data, next: Data to Ctrl
  val enqPtr = Output(UInt(log2Up(IssQueSize).W))
  val deqPtr = ValidIO(UInt(log2Up(IssQueSize).W)) // one cycle earlier
  val enqCtrl = ValidIO(new MicroOp)
  val enqSrcReady = Output(Vec(srcNum, Bool()))

  val fuReady   = Input(Bool())
  val srcUpdate = Input(Vec(IssQueSize, Vec(srcNum, Bool()))) // Note: the last one for enq
  val redirectVec    = Input(Vec(IssQueSize, Bool()))
  val feedback  = Input(Vec(IssQueSize+1, Bool())) // Note: the last one for hit

  override def cloneType: RSCtrlDataIO.this.type = new RSCtrlDataIO(srcNum).asInstanceOf[this.type]
}

class ReservationStationCtrl
(
  val exuCfg: ExuConfig,
  wakeupCnt: Int,
  extraListenPortsCnt: Int,
  feedback: Boolean,
  fixedDelay: Int,
  replayDelay: Int = 10
) extends XSModule with HasCircularQueuePtrHelper {

  val iqSize = IssQueSize
  val iqIdxWidth = log2Up(iqSize)
  val fastWakeup = fixedDelay >= 0 // NOTE: if do not enable fastWakeup(bypass), set fixedDelay to -1
  val nonBlocked = fastWakeup
  val srcNum = max(exuCfg.intSrcCnt, exuCfg.fpSrcCnt)
  require(srcNum >= 1 && srcNum <= 3)
  println(s"[RsCtrl]  ExuConfig: ${exuCfg.name} (srcNum = $srcNum)")

  val io = IO(new XSBundle {
    // flush
    val redirect = Flipped(ValidIO(new Redirect))

    // enq Ctrl sigs at dispatch-2, only use srcState
    val enqCtrl = Flipped(DecoupledIO(new MicroOp))

    // to DataPart
    val data = new RSCtrlDataIO(srcNum)

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
   * count   queue : record replay cycle
   */


  val s_idle :: s_valid :: s_wait :: s_replay :: Nil = Enum(4)
  /* state machine
   * s_idle     : empty slot, init state, set when deq
   * s_valid    : ready to be secleted
   * s_wait     : wait for feedback
   * s_replay   : replay after some particular cycle
   */
  val stateQueue    = RegInit(VecInit(Seq.fill(iqSize)(s_idle)))
  val validQueue    = VecInit(stateQueue.map(_ === s_valid))
  val emptyQueue    = VecInit(stateQueue.map(_ === s_idle))
  val srcQueue      = Reg(Vec(iqSize, Vec(srcNum, Bool())))
  val countQueue      = Reg(Vec(iqSize, UInt(log2Up(replayDelay).W)))

  // rs queue part:
  // val tailPtr       = RegInit(0.U((iqIdxWidth+1).W))
  val tailPtr       = RegInit(0.U.asTypeOf(new CircularQueuePtr(iqSize)))
  val indexQueue      = RegInit(VecInit((0 until iqSize).map(_.U(iqIdxWidth.W))))

  // turn to indexed index
  def widthMap[T <: Data](f: Int => T) = VecInit((0 until iqSize).map(f))
  val stateIdxQue = widthMap(i => stateQueue(indexQueue(i))) // NOTE: only use for debug, remove it later
  val validIdxQue = widthMap(i => validQueue(indexQueue(i)))
  val emptyIdxQue = widthMap(i => emptyQueue(indexQueue(i)))
  val srcIdxQue   = widthMap(i => srcQueue(indexQueue(i)))
  val cntIdxQue   = widthMap(i => countQueue(indexQueue(i))) // NOTE: only use for debug, remove it later
  val readyIdxQue = VecInit(srcIdxQue.zip(validIdxQue).map{ case (a,b) => Cat(a).andR & b })

  // redirect
  val redirectVec      = io.data.redirectVec
  val redirectVecPtr   = widthMap(i => io.data.redirectVec(indexQueue(i)))
  val feedbackMatchVec = Wire(UInt(iqSize.W))
  if (feedback) {
    feedbackMatchVec := widthMap(i => io.data.feedback(i) && (stateQueue(i) === s_wait || stateQueue(i)===s_valid)).asUInt
  } else {
    feedbackMatchVec := 0.U
  }
  val feedbackHit       = io.data.feedback(IssQueSize)

  // select ready
  // for no replay, select just equal to deq (attached)
  // with   replay, select is just two stage with deq.
  val issueFire = Wire(Bool())
  val moveMask = WireInit(0.U(iqSize.W))
  val lastSelMask = Wire(UInt(iqSize.W))
  val selectMask = WireInit(VecInit((0 until iqSize).map(i => readyIdxQue(i)))).asUInt & lastSelMask
  val selectIndex = ParallelPriorityMux(selectMask.asBools zip indexQueue) // NOTE: the idx in the indexQueue
  val selectPtr = ParallelPriorityMux(selectMask.asBools.zipWithIndex.map{ case (a,i) => (a, i.U)}) // NOTE: the idx of indexQueue
  val haveReady = Cat(selectMask).orR
  val selectIndexReg = RegNext(selectIndex)
  val selectValid = haveReady
  val selectReg = RegNext(selectValid)
  val selectPtrReg = RegNext(Mux(moveMask(selectPtr), selectPtr-1.U, selectPtr))
  lastSelMask := ~Mux(selectReg, UIntToOH(selectPtrReg), 0.U)
  assert(RegNext(!(haveReady && selectPtr >= tailPtr.asUInt)), "bubble should not have valid state like s_valid or s_wait")

  // sel bubble
  val lastbubbleMask = Wire(UInt(iqSize.W))
  val bubbleMask = WireInit(VecInit((0 until iqSize).map(i => emptyIdxQue(i)))).asUInt & lastbubbleMask
  // val bubbleIndex = ParallelMux(bubbleMask zip indexQueue) // NOTE: the idx in the indexQueue
  val bubblePtr= ParallelPriorityMux(bubbleMask.asBools.zipWithIndex.map{ case (a,i) => (a, i.U)}) // NOTE: the idx of the indexQueue
  val findBubble = Cat(bubbleMask).orR
  val haveBubble = findBubble && (bubblePtr < tailPtr.asUInt)
  val bubbleIndex = indexQueue(bubblePtr)
  val bubbleValid = haveBubble && (if (feedback) true.B else !selectValid)
  val bubbleReg = RegNext(bubbleValid)
  val bubblePtrReg = RegNext(Mux(moveMask(bubblePtr), bubblePtr-1.U, bubblePtr))
  lastbubbleMask := ~Mux(bubbleReg, UIntToOH(bubblePtrReg), 0.U) & (if(feedback) ~(0.U(iqSize.W))
                                                           else         Mux(RegNext(selectValid && io.redirect.valid), 0.U, ~(0.U(iqSize.W))))

  // deq
  val dequeue = if (feedback) bubbleReg
                else          bubbleReg || issueFire
  val deqPtr =  if (feedback) bubblePtrReg
                else Mux(selectReg, selectPtrReg, bubblePtrReg)
  moveMask := {
    (Fill(iqSize, 1.U(1.W)) << deqPtr)(iqSize-1, 0)
  } & Fill(iqSize, dequeue)

  // move, move happens when deq
  for(i <- 0 until iqSize-1){
    when(moveMask(i)){
      indexQueue(i) := indexQueue(i+1)
    }
  }
  when(dequeue){
    indexQueue.last := indexQueue(deqPtr)
  }

  when (issueFire) {
    if (feedback) { when (stateQueue(selectIndexReg) === s_valid) { stateQueue(selectIndexReg) := s_wait } }
    else { stateQueue(selectIndexReg) := s_idle } // NOTE: reset the state for seclectMask timing to avoid operaion '<'
  }

  // redirect and feedback && wakeup
  for (i <- 0 until iqSize) {
    // replay
    val count = countQueue(i)
    when (stateQueue(i) === s_replay) {
      count := count - 1.U
      when (count === 0.U) { stateQueue(i) := s_valid }
    }
    // feedback
    when (feedbackMatchVec(i)) {
      stateQueue(i) := Mux(!feedbackHit && (stateQueue(i) === s_wait || stateQueue(i) === s_valid), s_replay, s_idle)
      countQueue(i) := Mux(feedbackHit, count, (replayDelay-1).U)
    }
    // redirect
    when (redirectVec(i) && stateQueue(i) =/= s_idle) {
      stateQueue(i) := s_idle
    }
  }

  // output
  val issueValid = selectReg
  if (nonBlocked) {
    issueFire := issueValid
    assert(RegNext(io.data.fuReady), "if fu wanna fast wakeup, it should not block")
  } else {
    issueFire := issueValid && io.data.fuReady
  }

  // enq
  val isFull = tailPtr.flag
  // agreement with dispatch: don't fire when io.redirect.valid
  val enqueue = io.enqCtrl.fire() && !io.redirect.valid
  val tailInc = tailPtr+1.U
  val tailDec = tailPtr-1.U
  tailPtr := Mux(dequeue === enqueue, tailPtr, Mux(dequeue, tailDec, tailInc))

  io.enqCtrl.ready := !isFull || (if(feedback || nonBlocked) dequeue else false.B)
  val enqUop      = io.enqCtrl.bits
  val srcSeq      = Seq(enqUop.psrc1, enqUop.psrc2, enqUop.psrc3)
  val srcTypeSeq  = Seq(enqUop.ctrl.src1Type, enqUop.ctrl.src2Type, enqUop.ctrl.src3Type)
  val srcStateSeq = Seq(enqUop.src1State, enqUop.src2State, enqUop.src3State)

  val enqPtr = Mux(tailPtr.flag, deqPtr, tailPtr.value)
  val enqIdx = indexQueue(enqPtr)

  def stateCheck(src: UInt, srcType: UInt): Bool = {
    (srcType =/= SrcType.reg && srcType =/= SrcType.fp) ||
    (srcType === SrcType.reg && src === 0.U)
  }

  when (enqueue) {
    stateQueue(enqIdx) := s_valid
    srcQueue(enqIdx).zipWithIndex.map{ case (s, i) =>
      s := io.data.enqSrcReady(i)
    }
    XSDebug(p"EnqCtrl: roqIdx:${enqUop.roqIdx} pc:0x${Hexadecimal(enqUop.cf.pc)} " +
      List.tabulate(srcNum)(i => p"<src$i: ${srcSeq(i)} state$i: ${srcStateSeq(i)} type$i: ${srcTypeSeq(i)}>").reduce(_ + " " + _) + "\n")
  }

  // NOTE: put wakeup below enq logic for enqueue wakeup
  for (i <- 0 until iqSize) {
    // wakeup
    val hitVec = io.data.srcUpdate(i)
    for (j <- 0 until srcNum) {
      when (hitVec(j)) {
        srcQueue(i)(j) := true.B
        XSDebug(p"srcHit: i:${i.U} j:${j.U} src:${srcQueue(i)(j)}\n")
      }
    }
  }

  // other to Data
  io.data.enqPtr := enqIdx
  io.data.deqPtr.valid  := selectValid
  io.data.deqPtr.bits   := selectIndex
  io.data.enqCtrl.valid := enqueue
  io.data.enqCtrl.bits  := io.enqCtrl.bits
  for(i <- 0 until srcNum) {
    io.data.enqSrcReady(i) := stateCheck(srcSeq(i), srcTypeSeq(i)) || (srcStateSeq(i) === SrcState.rdy)
  }
  // other io
  io.numExist := Mux(tailPtr.flag, (iqSize-1).U, tailPtr.value) // NOTE: numExist is iqIdxWidth.W, maybe a bug

  // assert
  assert(RegNext(Mux(tailPtr.flag, tailPtr.value===0.U, true.B)))

  val print = !(tailPtr.asUInt===0.U) || io.enqCtrl.valid || enqueue || dequeue
  XSDebug(print || true.B, p"In(${io.enqCtrl.valid} ${io.enqCtrl.ready}) Out(${issueValid} ${io.data.fuReady}) nonBlocked:${nonBlocked.B} needfb:${feedback.B}\n")
  XSDebug(print || true.B, p"tailPtr:${tailPtr} enq:${enqueue} deq:${dequeue} isFull:${isFull} " +
    p"vIdxQue:${Binary(validIdxQue.asUInt)} rIdxQue:${Binary(readyIdxQue.asUInt)}\n")
  XSDebug(print && Cat(redirectVecPtr).orR, p"Redirect: ${Hexadecimal(redirectVecPtr.asUInt)}\n")
  XSDebug(print && Cat(feedbackMatchVec).orR, p"Feedback: ${Hexadecimal(feedbackMatchVec.asUInt)} Hit:${feedbackHit}\n")
  XSDebug(print || true.B, p"moveMask:${Binary(moveMask)} selMask:${Binary(selectMask.asUInt)} bubbleMask:${Binary(bubbleMask.asUInt)}\n")
  XSDebug(print || true.B, p"selectPtr:${selectPtr} haveReady:${haveReady} " +
    p"selV:${selectValid} selectReg:${selectReg} selectPtrReg:${selectPtrReg} selectIndex:${selectIndex} lastSelMask:${Hexadecimal(lastSelMask)}\n")
  XSDebug(print || true.B, p"bubbleValid:${bubbleValid} haveBub:${haveBubble} bubblePtr:${bubblePtr} findBub:${findBubble} " +
    p"bubbleReg:${bubbleReg} bubblePtrReg:${bubblePtrReg} bubbleIndex:${bubbleIndex} lastbubbleMask:${Hexadecimal(lastbubbleMask)}\n")
  XSDebug(print || true.B, p"issueValid:${issueValid} issueFire:${issueFire} dequeue:${dequeue} deqPtr:${deqPtr}\n")
  XSDebug(p" :Idx|v|r|s |count|s1:s2:s3\n")
  for(i <- srcQueue.indices) {
    XSDebug(p"${i.U}: ${indexQueue(i)}|${validIdxQue(i)}|${readyIdxQue(i)}|${stateIdxQue(i)}|${cntIdxQue(i)}|" +
      List.tabulate(srcNum)(j => p"${srcIdxQue(i)(j)}").reduce(_ + ":" + _) + "\n")
  }
}

class RSDataSingleSrc(srcLen: Int, numEntries: Int, numListen: Int) extends XSModule {
  val io = IO(new Bundle {
    val r = new Bundle {
      // val valid = Bool() // NOTE: if read valid is necessary, but now it is not completed
      val addr = Input(UInt(log2Up(numEntries).W))
      val rdata = Output(UInt(srcLen.W))
    }
    val w = Input(new Bundle {
      val wen = Bool()
      val addr = UInt(log2Up(numEntries).W)
      val wdata = Input(UInt(srcLen.W))
    })
    val listen = Input(new Bundle {
      val wdata = Vec(numListen, UInt(srcLen.W))
      val wen = Vec(numEntries, Vec(numListen, Bool()))
    })
  })

  val value = Reg(Vec(numEntries, UInt(srcLen.W)))

  val wMask = Mux(io.w.wen, UIntToOH(io.w.addr)(numEntries-1, 0), 0.U(numEntries.W))
  val data = io.listen.wdata :+ io.w.wdata
  val wen = io.listen.wen.zip(wMask.asBools).map{ case (w, m) => w :+ m }
  for (i <- 0 until numEntries) {
    when (Cat(wen(i)).orR) {
      value(i) := ParallelMux(wen(i) zip data)
      assert(RegNext(PopCount(wen(i))===0.U || PopCount(wen(i))===1.U), s"${i}")
    }
  }

  io.r.rdata := value(RegNext(io.r.addr)) // NOTE: the read addr will arrive one cycle before
}

class ReservationStationData
(
  val exuCfg: ExuConfig,
  wakeupCnt: Int,
  extraListenPortsCnt: Int,
  fixedDelay: Int,
  feedback: Boolean,
) extends XSModule {
  val iqSize = IssQueSize
  val iqIdxWidth = log2Up(iqSize)
  val fastWakeup = fixedDelay >= 0 // NOTE: if do not enable fastWakeup(bypass), set fixedDelay to -1
  val nonBlocked = fastWakeup
  val srcNum = max(exuCfg.intSrcCnt, exuCfg.fpSrcCnt)
  require(srcNum >= 1 && srcNum <= 3)
  println(s"[RsData]  ExuConfig: ${exuCfg.name} (srcNum = $srcNum)")

  val io = IO(new XSBundle {
    // flush
    val redirect = Flipped(ValidIO(new Redirect))

    // send to exu
    val deq = DecoupledIO(new ExuInput)

    // listen to RSCtrl
    val ctrl = Flipped(new RSCtrlDataIO(srcNum))

    // read src op value
    val srcRegValue = Vec(srcNum, Input(UInt((XLEN + 1).W)))
    val jumpPc = if(exuCfg == Exu.jumpExeUnitCfg) Input(UInt(VAddrBits.W)) else null
    // broadcast selected uop to other issue queues
    val selectedUop = ValidIO(new MicroOp)

    // recv broadcasted uops form any relative issue queue,
    // to simplify wake up logic, the uop broadcasted by this queue self
    // are also in 'broadcastedUops'
    val broadcastedUops = Vec(wakeupCnt, Flipped(ValidIO(new MicroOp)))

    // listen to write back data bus(certain latency)
    // and extra write back(uncertain latency)
    val writeBackedData = Vec(wakeupCnt, Input(UInt((XLEN+1).W)))
    val extraListenPorts = Vec(extraListenPortsCnt, Flipped(ValidIO(new ExuOutput)))

    // tlb feedback
    val feedback = Flipped(ValidIO(new TlbFeedback))
  })

  val fastUops = io.broadcastedUops
  val fastData = io.writeBackedData
  val slowPort = io.extraListenPorts
  val lastFastUops = RegNext(fastUops)

  // Data : single read, multi write
  // ------------------------
  val data = (0 until srcNum).map{i =>
    val d = Module(new RSDataSingleSrc(XLEN + 1, iqSize, wakeupCnt + extraListenPortsCnt))
    d.suggestName(s"${this.name}_data${i}")
    d.io
  }
  data.map(src => src.listen.wen.map(a => a.map(b => b := false.B )))
  for (i <- 0 until wakeupCnt)           { data.map(_.listen.wdata(i) := fastData(i)) }
  for (i <- 0 until extraListenPortsCnt) { data.map(_.listen.wdata(i + wakeupCnt) := slowPort(i).bits.data) }

  // pdest : single write, multi read
  val psrc = Reg(Vec(iqSize, Vec(srcNum, UInt(PhyRegIdxWidth.W))))

  // other Uop : single read, single write (if fast wakeup, two read)
  // ------------------------
  val uopMem     = Module(new SyncDataModuleTemplate(new MicroOp, iqSize, iqSize, 1))
  uopMem.io <> DontCare
  uopMem.io.wen.foreach(_ := false.B)

  // uop -- read = iqSize write = 1
  // uopMem 's read ports have fixed values
  uopMem.io.raddr.zipWithIndex.foreach{ case(r, i) => r := i.U }
  def uopRead(iqIdx: UInt): MicroOp = {
    uopMem.io.rdata(iqIdx)
  }
  def uopWrite(iqIdx: UInt, wdata: MicroOp) = {
    uopMem.io.waddr(0) := iqIdx
    uopMem.io.wdata(0) := wdata
    uopMem.io.wen(0) := true.B
  }

  val uop = WireInit(VecInit((0 until iqSize).map(i => uopRead(i.U))))

  val redirectHit = WireInit(false.B)
  val enq   = io.ctrl.enqPtr
  val sel   = io.ctrl.deqPtr
  val deq   = RegEnable(sel.bits, sel.valid)
  val enqCtrl = io.ctrl.enqCtrl
  val enqUop = enqCtrl.bits
  val enqUopReg = RegEnable(enqUop, enqCtrl.fire())

  // enq
  val enqPtr = enq(log2Up(IssQueSize)-1,0)
  val enqPtrReg = RegEnable(enqPtr, enqCtrl.valid)
  val enqEn  = enqCtrl.valid
  val enqEnReg = RegNext(enqEn)
  when (enqEn) {
    uopWrite(enqPtr, enqUop)
    psrc(enqPtr)(0) := enqUop.psrc1
    if (srcNum > 1) { psrc(enqPtr)(1) := enqUop.psrc2 }
    if (srcNum > 2) { psrc(enqPtr)(2) := enqUop.psrc3 }
    XSDebug(p"enqCtrl: enqPtr:${enqPtr} src1:${enqUop.psrc1}|${enqUop.src1State}|${enqUop.ctrl.src1Type}" +
      p" src2:${enqUop.psrc2}|${enqUop.src2State}|${enqUop.ctrl.src2Type} src3:${enqUop.psrc3}|" +
      p"${enqUop.src3State}|${enqUop.ctrl.src3Type} pc:0x${Hexadecimal(enqUop.cf.pc)} roqIdx:${enqUop.roqIdx}\n")
  }

  data.map(_.w.addr  := enqPtrReg)
  data.zip(io.ctrl.enqSrcReady).map{ case (src, ready) => src.w.wen := RegNext(ready && enqEn) }

  exuCfg match {
    case Exu.jumpExeUnitCfg =>
      val src1Mux = Mux(enqUopReg.ctrl.src1Type === SrcType.pc,
                        SignExt(io.jumpPc, XLEN),
                        io.srcRegValue(0)
                    )
      // data.io.w.bits.data(0) := src1Mux
      data(0).w.wdata := src1Mux

    case Exu.aluExeUnitCfg =>
      val src1Mux = Mux(enqUopReg.ctrl.src1Type === SrcType.pc,
                      SignExt(enqUopReg.cf.pc, XLEN),
                      io.srcRegValue(0)
                    )
      data(0).w.wdata := src1Mux
      // alu only need U type and I type imm
      val imm32 = Mux(enqUopReg.ctrl.selImm === SelImm.IMM_U,
                    ImmUnion.U.toImm32(enqUopReg.ctrl.imm),
                    ImmUnion.I.toImm32(enqUopReg.ctrl.imm)
                  )
      val imm64 = SignExt(imm32, XLEN)
      val src2Mux = Mux(enqUopReg.ctrl.src2Type === SrcType.imm,
                      imm64, io.srcRegValue(1)
                    )
      data(1).w.wdata := src2Mux
    case _ =>
      (0 until srcNum).foreach(i => data(i).w.wdata := io.srcRegValue(i) )
  }
  XSDebug(enqEnReg, p"${exuCfg.name}: enqPtrReg:${enqPtrReg} pc: ${Hexadecimal(uop(enqPtrReg).cf.pc)}\n")
  XSDebug(enqEnReg, p"[srcRegValue] " + List.tabulate(srcNum)(idx =>
    p"src$idx: ${Hexadecimal(io.srcRegValue(idx))}").reduce((p1, p2) => p1 + " " + p2) + "\n")

  def listenHit(uop: MicroOp, src: UInt, srctype: UInt): Bool = {
    (src === uop.pdest) &&
    ((srctype === SrcType.reg && uop.ctrl.rfWen && src=/=0.U) ||
     (srctype === SrcType.fp  && uop.ctrl.fpWen))
  }

  io.ctrl.srcUpdate.map(a => a.map(_ := false.B))
  for (i <- 0 until iqSize) {
    val srcSeq = psrc(i)
    val srcTypeSeq = Seq(uop(i).ctrl.src1Type, uop(i).ctrl.src2Type, uop(i).ctrl.src3Type)
    for (j <- 0 until srcNum) {
      for (k <- 0 until wakeupCnt) {
        val fastHit = listenHit(fastUops(k).bits, srcSeq(j), srcTypeSeq(j)) && fastUops(k).valid
        val fastHitNoConflict = fastHit && !(enqPtr===i.U && enqEn)
        when (fastHitNoConflict) { io.ctrl.srcUpdate(i)(j) := true.B }
        when (RegNext(fastHitNoConflict) && !(enqPtr===i.U && enqEn)) { data(j).listen.wen(i)(k) := true.B }
        XSDebug(fastHit, p"FastHit: ${i.U} ${j.U} ${k.U}\n")
        XSDebug(RegNext(fastHitNoConflict) && !(enqPtr===i.U && enqEn), p"FastHit: but enq confict: ${i.U} ${j.U} ${k.U}\n")
      }
      for (k <- 0 until extraListenPortsCnt) {
        val slowHit = listenHit(slowPort(k).bits.uop, srcSeq(j), srcTypeSeq(j)) && slowPort(k).valid
        val slowHitNoConflict = slowHit && !(enqPtr===i.U && enqEn)
        when (slowHitNoConflict) { io.ctrl.srcUpdate(i)(j) := true.B }
        when (slowHitNoConflict) { data(j).listen.wen(i)(k + wakeupCnt) := true.B }
        XSDebug(slowHit, p"SlowHit: ${i.U} ${j.U} ${k.U} fastHit but enq conflict:${slowHit && (enqPtr===i.U && enqEn)}")
      }
    }
  }

  // deq
  val exuInput = io.deq.bits
  exuInput := DontCare
  exuInput.uop := uop(deq)
  exuInput.uop.cf.exceptionVec := 0.U.asTypeOf(ExceptionVec())
  data.map(_.r.addr := sel.bits)
  val regValues =  data.map(_.r.rdata)
  XSDebug(io.deq.fire(), p"[regValues] " + List.tabulate(srcNum)(idx => p"reg$idx: ${Hexadecimal(regValues(idx))}").reduce((p1, p2) => p1 + " " + p2) + "\n")
  exuInput.src1 := regValues(0)
  if (srcNum > 1) exuInput.src2 := regValues(1)
  if (srcNum > 2) exuInput.src3 := regValues(2)

  io.deq.valid := RegNext(sel.valid && ~redirectHit)
  if (nonBlocked) { assert(RegNext(io.deq.ready), s"${name} if fu wanna fast wakeup, it should not block")}

  // enq listen
  val srcSeq = Seq(enqUop.psrc1, enqUop.psrc2, enqUop.psrc3)
  val srcTypeSeq = Seq(enqUop.ctrl.src1Type, enqUop.ctrl.src2Type, enqUop.ctrl.src3Type)
  for (j <- 0 until srcNum) {
    for (k <- 0 until wakeupCnt) {
      val fastHit = listenHit(fastUops(k).bits, srcSeq(j), srcTypeSeq(j)) && enqEn && fastUops(k).valid
      val lastFastHit = listenHit(lastFastUops(k).bits, srcSeq(j), srcTypeSeq(j)) && enqEn && lastFastUops(k).valid
      when (fastHit || lastFastHit) { io.ctrl.srcUpdate(enqPtr)(j) := true.B }
      when (lastFastHit)            { data(j).listen.wen(enqPtr)(k) := true.B }
      when (RegNext(fastHit))       { data(j).listen.wen(enqPtrReg)(k) := true.B }

      XSDebug(fastHit, p"EnqFastHit: ${j.U} ${k.U}\n")
      XSDebug(lastFastHit, p"EnqLastFastHit: ${j.U} ${k.U}\n")
    }
    for (k <- 0 until extraListenPortsCnt) {
      val slowHit = listenHit(slowPort(k).bits.uop, srcSeq(j), srcTypeSeq(j)) && enqCtrl.valid && slowPort(k).valid
      when (slowHit) {
        io.ctrl.srcUpdate(enqPtr)(j) := true.B
        data(j).listen.wen(enqPtr)(k + wakeupCnt) := true.B
      }
      XSDebug(slowHit, p"EnqSlowHit: ${j.U} ${k.U}\n")
    }
  }

  if (nonBlocked) { io.ctrl.fuReady := true.B }
  else { io.ctrl.fuReady := io.deq.ready }
  io.ctrl.redirectVec   := uop.map(_.roqIdx.needFlush(io.redirect))
  redirectHit := io.ctrl.redirectVec(sel.bits)

  io.ctrl.feedback := DontCare
  if (feedback) {
    (0 until IssQueSize).foreach(i =>
      io.ctrl.feedback(i) := uop(i).roqIdx.asUInt === io.feedback.bits.roqIdx.asUInt && io.feedback.valid)
    io.ctrl.feedback(IssQueSize) := io.feedback.bits.hit
  }

  // bypass send
  io.selectedUop <> DontCare
  if (fastWakeup) {
    if (fixedDelay == 0) {
      io.selectedUop.valid := sel.valid
      io.selectedUop.bits  := uop(sel.bits)
      io.selectedUop.bits.cf.exceptionVec  := 0.U.asTypeOf(ExceptionVec())
    } else {
      val bpQueue = Module(new BypassQueue(fixedDelay))
      bpQueue.io.in.valid := sel.valid // FIXME: error when function is blocked => fu should not be blocked
      bpQueue.io.in.bits  := uop(sel.bits)
      bpQueue.io.redirect := io.redirect
      io.selectedUop.valid := bpQueue.io.out.valid
      io.selectedUop.bits  := bpQueue.io.out.bits
      io.selectedUop.bits.cf.exceptionVec  := 0.U.asTypeOf(ExceptionVec())
    }

    XSDebug(io.selectedUop.valid, p"SelUop: pc:0x${Hexadecimal(io.selectedUop.bits.cf.pc)}" +
      p" roqIdx:${io.selectedUop.bits.roqIdx} pdest:${io.selectedUop.bits.pdest} " +
      p"rfWen:${io.selectedUop.bits.ctrl.rfWen} fpWen:${io.selectedUop.bits.ctrl.fpWen}\n" )
  }

  // log
  XSDebug(Cat(io.ctrl.redirectVec).orR, p"Red: ${io.ctrl.redirectVec}\n")
  XSDebug(io.feedback.valid && feedback.B, p"feedback: roqIdx:${io.feedback.bits.roqIdx} hit:${io.feedback.bits.hit}\n")
  XSDebug(io.deq.valid, p"Deq(${io.deq.valid} ${io.deq.ready}): deqPtr:${deq} pc:${Hexadecimal(io.deq.bits.uop.cf.pc)}" +
    p" roqIdx:${io.deq.bits.uop.roqIdx} src1:${Hexadecimal(io.deq.bits.src1)} " +
    p" src2:${Hexadecimal(io.deq.bits.src2)} src3:${Hexadecimal(io.deq.bits.src3)}\n")
  XSDebug(p"Data: hit|pdest:rf:fp| roqIdx | pc\n")
  for (i <- 0 until iqSize) {
    XSDebug(p"${i.U}:" +
      p"${Binary(io.ctrl.srcUpdate(i).asUInt)}|${uop(i).pdest}:${uop(i).ctrl.rfWen}:" +
      p"${uop(i).ctrl.fpWen}|${uop(i).roqIdx} |${Hexadecimal(uop(i).cf.pc)}\n")
  }
}
