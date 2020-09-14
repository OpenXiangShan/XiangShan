package xiangshan.backend.issue

import chisel3.{util, _}
import chisel3.util._
import utils.{ParallelMux, ParallelOR, PriorityEncoderWithFlag, XSDebug, XSInfo, XSPerf}
import xiangshan._
import xiangshan.backend.exu.{Exu, ExuConfig}
import xiangshan.backend.regfile.RfReadPort

class IssueQueue
(
  val exuCfg: ExuConfig,
  val wakeupCnt: Int,
  val bypassCnt: Int = 0
) extends XSModule with HasIQConst {
  val io = IO(new Bundle() {
    val redirect = Flipped(ValidIO(new Redirect))
    val enq = Flipped(DecoupledIO(new MicroOp))
    val readIntRf = Vec(exuCfg.intSrcCnt, Flipped(new RfReadPort))
    val readFpRf = Vec(exuCfg.fpSrcCnt, Flipped(new RfReadPort))
    val deq = DecoupledIO(new ExuInput)
    val wakeUpPorts = Vec(wakeupCnt, Flipped(ValidIO(new ExuOutput)))
    val bypassUops = Vec(bypassCnt, Flipped(ValidIO(new MicroOp)))
    val bypassData = Vec(bypassCnt, Flipped(ValidIO(new ExuOutput)))
    val numExist = Output(UInt(iqIdxWidth.W))
    // tlb hit, inst can deq
    val tlbFeedback = Flipped(ValidIO(new TlbFeedback))
  })

  def qsize: Int = IssQueSize
  def idxWidth = log2Up(qsize)
  def replayDelay = 16

  require(isPow2(qsize))

  val tlbHit = io.tlbFeedback.valid && io.tlbFeedback.bits.hit
  val tlbMiss = io.tlbFeedback.valid && !io.tlbFeedback.bits.hit

  XSDebug(io.tlbFeedback.valid,
    "tlb feedback: hit: %d roqIdx: %d\n",
    io.tlbFeedback.bits.hit,
    io.tlbFeedback.bits.roqIdx
  )
  /*
      invalid --[enq]--> valid --[deq]--> wait --[tlbHit]--> invalid
                                          wait --[replay]--> replay --[cnt]--> valid
   */
  val s_invalid :: s_valid :: s_wait :: s_replay :: Nil = Enum(4)

  val idxQueue = RegInit(VecInit((0 until qsize).map(_.U(idxWidth.W))))
  val stateQueue = RegInit(VecInit(Seq.fill(qsize)(s_invalid)))

  val readyVec = Wire(Vec(qsize, Bool()))
  val uopQueue = Reg(Vec(qsize, new MicroOp))
  val cntQueue = Reg(Vec(qsize, UInt(log2Up(replayDelay).W)))

  val tailPtr = RegInit(0.U((idxWidth+1).W))

  // real deq

  /*
    example: realDeqIdx = 2       |  realDeqIdx=0
             moveMask = 11111100  |  moveMask=11111111
 */

  val (firstBubble, findBubble) = PriorityEncoderWithFlag(stateQueue.map(_ === s_invalid))
  val realDeqIdx = firstBubble
  val realDeqValid = (firstBubble < tailPtr) && findBubble
  val moveMask = {
    (Fill(qsize, 1.U(1.W)) << realDeqIdx)(qsize-1, 0)
  } & Fill(qsize, realDeqValid)

  for(i <- 0 until qsize-1){
    when(moveMask(i)){
      idxQueue(i) := idxQueue(i+1)
      stateQueue(i) := stateQueue(i+1)
    }
  }
  when(realDeqValid){
    idxQueue.last := idxQueue(realDeqIdx)
    stateQueue.last := s_invalid
  }


  // wake up
  def getSrcSeq(uop: MicroOp): Seq[UInt] = Seq(uop.psrc1, uop.psrc2, uop.psrc3)
  def getSrcTypeSeq(uop: MicroOp): Seq[UInt] = Seq(
    uop.ctrl.src1Type, uop.ctrl.src2Type, uop.ctrl.src3Type
  )
  def getSrcStateSeq(uop: MicroOp): Seq[UInt] = Seq(
    uop.src1State, uop.src2State, uop.src3State
  )

  def writeBackHit(src: UInt, srcType: UInt, wbUop: (Bool, MicroOp)): Bool = {
    val (v, uop) = wbUop
    val isSameType =
      (SrcType.isReg(srcType) && uop.ctrl.rfWen) || (SrcType.isFp(srcType) && uop.ctrl.fpWen)

    v && isSameType && (src===uop.pdest)
  }

  //TODO: opt this, do bypass select in 'select' stage not 'issue' stage
  val bypassData = RegNext(io.bypassData)
  def doBypass(src: UInt, srcType: UInt): (Bool, UInt) = {
    val hitVec = bypassData.map(p => (p.valid, p.bits.uop)).
      map(wbUop => writeBackHit(src, srcType, wbUop))
    val data = ParallelMux(hitVec.zip(bypassData.map(_.bits.data)))
    (ParallelOR(hitVec).asBool(), data)
  }

  def wakeUp(uop: MicroOp): MicroOp = {
    def getNewSrcState(i: Int): UInt = {
      val src = getSrcSeq(uop)(i)
      val srcType = getSrcTypeSeq(uop)(i)
      val srcState = getSrcStateSeq(uop)(i)
      val hitVec = (
        io.wakeUpPorts.map(w => (w.valid, w.bits.uop)) ++
        io.bypassUops.map(p => (p.valid, p.bits))
        ).map(wbUop => writeBackHit(src, srcType, wbUop))
      val hit = ParallelOR(hitVec).asBool()
      Mux(hit, SrcState.rdy, srcState)
    }
    val new_uop = WireInit(uop)
    new_uop.src1State := getNewSrcState(0)
    if(exuCfg==Exu.stExeUnitCfg) new_uop.src2State := getNewSrcState(1)
    new_uop
  }

  def uopIsRdy(uop: MicroOp): Bool = {
    def srcIsRdy(srcType: UInt, srcState: UInt): Bool = {
      SrcType.isPcImm(srcType) || srcState===SrcState.rdy
    }
    exuCfg match {
      case Exu.ldExeUnitCfg =>
        srcIsRdy(uop.ctrl.src1Type, uop.src1State)
      case Exu.stExeUnitCfg =>
        srcIsRdy(uop.ctrl.src1Type, uop.src1State) && srcIsRdy(uop.ctrl.src2Type, uop.src2State)
    }
  }


  // 1. wake up
  for(i <- 0 until qsize){
    uopQueue(i) := wakeUp(uopQueue(i))
  }

  // 2. select
  for(i <- 0 until qsize){
    readyVec(i) := uopIsRdy(uopQueue(i))
  }

  val selectedIdxRegOH = Wire(UInt(qsize.W))
  val selectMask = WireInit(VecInit(
    (0 until qsize).map(i =>
      (stateQueue(i)===s_valid) && readyVec(idxQueue(i)) && !(selectedIdxRegOH(i) && io.deq.fire())
    )
  ))
  val (selectedIdxWire, sel) = PriorityEncoderWithFlag(selectMask)
  val selReg = RegNext(sel)
  val selectedIdxReg = RegNext(selectedIdxWire - moveMask(selectedIdxWire))
  selectedIdxRegOH := UIntToOH(selectedIdxReg)
  XSDebug(
    p"selMaskWire:${Binary(selectMask.asUInt())} selected:$selectedIdxWire" +
      p" moveMask:${Binary(moveMask)} selectedIdxReg:$selectedIdxReg\n"
  )


  // read regfile
  val selectedUop = uopQueue(idxQueue(selectedIdxWire))

  exuCfg match {
    case Exu.ldExeUnitCfg =>
      io.readIntRf(0).addr := selectedUop.psrc1 // base
      XSDebug(p"src1 read addr: ${io.readIntRf(0).addr}\n")
    case Exu.stExeUnitCfg =>
      io.readIntRf(0).addr := selectedUop.psrc1 // base
      io.readIntRf(1).addr := selectedUop.psrc2 // store data (int)
      io.readFpRf(0).addr := selectedUop.psrc2  // store data (fp)
      XSDebug(
        p"src1 read addr: ${io.readIntRf(0).addr} src2 read addr: ${io.readIntRf(1).addr}\n"
      )
    case _ =>
      require(requirement = false, "Error: IssueQueue only support ldu and stu!")
  }

  // (fake) deq to Load/Store unit
  io.deq.valid := (stateQueue(selectedIdxReg)===s_valid) && selReg
  io.deq.bits.uop := uopQueue(idxQueue(selectedIdxReg))

  val src1Bypass = doBypass(io.deq.bits.uop.psrc1, io.deq.bits.uop.ctrl.src1Type)
  io.deq.bits.src1 := Mux(src1Bypass._1, src1Bypass._2, io.readIntRf(0).data)
  if(exuCfg == Exu.stExeUnitCfg){
    val src2Bypass = doBypass(io.deq.bits.uop.psrc2, io.deq.bits.uop.ctrl.src2Type)
    io.deq.bits.src2 := Mux(src2Bypass._1,
      src2Bypass._2,
      Mux(SrcType.isReg(io.deq.bits.uop.ctrl.src2Type),
        io.readIntRf(1).data,
        io.readFpRf(0).data
      )
    )
  } else {
    io.deq.bits.src2 := DontCare
  }
  io.deq.bits.src3 := DontCare

  when(io.deq.fire()){
    stateQueue(selectedIdxReg - moveMask(selectedIdxReg)) := s_wait
    assert(stateQueue(selectedIdxReg) === s_valid, "Dequeue a invalid entry to lsu!")
  }

//  assert(!(tailPtr===0.U && tlbHit), "Error: queue is empty but tlbHit is true!")

  val tailAfterRealDeq = tailPtr - moveMask(tailPtr.tail(1))
  val isFull = tailAfterRealDeq.head(1).asBool() // tailPtr===qsize.U

  // enq
  io.enq.ready := !isFull && !io.redirect.valid
  when(io.enq.fire()){
    stateQueue(tailAfterRealDeq.tail(1)) := s_valid
    val uopQIdx = idxQueue(tailPtr.tail(1))
    val new_uop = wakeUp(io.enq.bits)
    uopQueue(uopQIdx) := new_uop
  }

  tailPtr := tailAfterRealDeq + io.enq.fire()

  XSDebug(
    realDeqValid,
    p"realDeqIdx:$realDeqIdx\n"
  )

  XSDebug("State Dump: ")
  stateQueue.reverse.foreach(s =>{
    XSDebug(false, s===s_invalid, "-")
    XSDebug(false, s===s_valid, "v")
    XSDebug(false, s===s_wait, "w")
    XSDebug(false, s===s_replay, "p")
  })
  XSDebug(false, true.B, "\n")

  XSDebug("State Dump: ")
  idxQueue.reverse.foreach(id =>{
    XSDebug(false, true.B, p"$id")
  })
  XSDebug(false, true.B, "\n")

  XSDebug("State Dump: ")
  for(i <- readyVec.indices.reverse){
    val r = readyVec(idxQueue(i))
    XSDebug(false, r, p"r")
    XSDebug(false, !r, p"-")
  }
  XSDebug(false, true.B, "\n")

//  assert(!(tlbMiss && realDeqValid), "Error: realDeqValid should be false when replay valid!")
  for(i <- 0 until qsize){
    val uopQIdx = idxQueue(i)
    val uop = uopQueue(uopQIdx)
    val cnt = cntQueue(uopQIdx)
    val nextIdx = i.U - moveMask(i)
    //TODO: support replay
    val roqIdxMatch = uop.roqIdx === io.tlbFeedback.bits.roqIdx
    val notEmpty = stateQueue(i)=/=s_invalid
    val replayThis = (stateQueue(i)===s_wait) && tlbMiss && roqIdxMatch
    val tlbHitThis = notEmpty && tlbHit && roqIdxMatch
    val flushThis = notEmpty && uop.needFlush(io.redirect)

    when(replayThis){
      stateQueue(nextIdx) := s_replay
      cnt := (replayDelay-1).U
    }
    when(stateQueue(i)===s_replay){
      when(cnt === 0.U){
        stateQueue(nextIdx) := s_valid
      }.otherwise({
        cnt := cnt - 1.U
      })
    }
    when(flushThis || tlbHitThis){
      stateQueue(nextIdx) := s_invalid
    }
  }


  // assign outputs
  io.numExist := Mux(isFull, (qsize-1).U, tailPtr)

  // Debug sigs
  XSInfo(
    io.enq.fire(),
    p"enq fire: pc:${Hexadecimal(io.enq.bits.cf.pc)} roqIdx:${io.enq.bits.roqIdx} " +
      p"src1: ${io.enq.bits.psrc1} src2:${io.enq.bits.psrc2} pdst:${io.enq.bits.pdest}\n"
  )
  XSInfo(
    io.deq.fire(),
    p"deq fire: pc:${Hexadecimal(io.deq.bits.uop.cf.pc)} roqIdx:${io.deq.bits.uop.roqIdx} " +
      p"src1: ${io.deq.bits.uop.psrc1} data: ${Hexadecimal(io.deq.bits.src1)} " +
      p"src2: ${io.deq.bits.uop.psrc2} data: ${Hexadecimal(io.deq.bits.src2)} " +
      p"imm : ${Hexadecimal(io.deq.bits.uop.ctrl.imm)}\npdest: ${io.deq.bits.uop.pdest}\n"
  )
  XSDebug(p"tailPtr:$tailPtr tailAfterDeq:$tailAfterRealDeq tlbHit:$tlbHit\n")

  XSPerf("utilization", tailPtr)
}
