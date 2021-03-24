package xiangshan.backend.issue

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.backend.SelImm
import xiangshan.backend.decode.{ImmUnion, Imm_U}
import xiangshan.backend.exu.{Exu, ExuConfig}
import xiangshan.backend.regfile.RfReadPort
import xiangshan.backend.roq.RoqPtr
import xiangshan.mem.{SqPtr}

import scala.math.max

class BypassQueue(number: Int) extends XSModule {
  val io = IO(new Bundle {
    val in  = Flipped(ValidIO(new MicroOp))
    val out = ValidIO(new MicroOp)
    val redirect = Flipped(ValidIO(new Redirect))
    val flush = Input(Bool())
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
    queue(0).valid := io.in.valid && !io.in.bits.roqIdx.needFlush(io.redirect, io.flush)
    queue(0).bits  := io.in.bits
    (0 until (number-1)).map{i =>
      queue(i+1) := queue(i)
      queue(i+1).valid := queue(i).valid && !queue(i).bits.roqIdx.needFlush(io.redirect, io.flush)
    }
    io.out.valid := queue(number-1).valid
    io.out.bits := queue(number-1).bits
    for (i <- 0 until number) {
      XSDebug(queue(i).valid, p"BPQue(${i.U}): pc:${Hexadecimal(queue(i).bits.cf.pc)} roqIdx:${queue(i).bits.roqIdx}" +
        p" pdest:${queue(i).bits.pdest} rfWen:${queue(i).bits.ctrl.rfWen} fpWen${queue(i).bits.ctrl.fpWen}\n")
    }
  }
}

// multi-read && single-write
// input is data, output is hot-code(not one-hot)
class SingleSrcCAM[T <: Data](val gen: T, val set: Int, val readWidth: Int, rfZero: Boolean) extends XSModule {
  val io = IO(new Bundle {
    val r = new Bundle {
      val req = Input(Vec(readWidth, gen))
      val resp = Output(Vec(readWidth, Vec(set, Bool())))
    }
    val w = new Bundle {
      val valid = Input(Bool())
      val bits = new Bundle {
        val addr = Input(UInt(log2Up(set).W))
        val data = Input(gen)
      }
    }
    val zero = if (rfZero) Output(Vec(set, Bool())) else null
  })

  val wordType = UInt(gen.getWidth.W)
  val value = Reg(Vec(set, wordType))

  io.r.resp.zipWithIndex.map{ case (a,i) =>
    a := value.map( src => io.r.req(i).asUInt === src)
  }

  // Note: general reg file don't wakeup zero
  if (rfZero) { io.zero.zip(value).map{ case(z, a) => z := a===0.U }}

  when (io.w.valid) {
    value(io.w.bits.addr) := io.w.bits.data
  }
}

class ReservationStation
(
  myName : String,
  val exuCfg: ExuConfig,
  iqSize : Int,
  srcLen: Int,
  fastPortsCfg: Seq[ExuConfig],
  slowPortsCfg: Seq[ExuConfig],
  fixedDelay: Int,
  fastWakeup: Boolean,
  feedback: Boolean,
) extends XSModule {
  val iqIdxWidth = log2Up(iqSize)
  val nonBlocked = fixedDelay >= 0
  val srcNum = if (exuCfg == Exu.jumpExeUnitCfg) 2 else max(exuCfg.intSrcCnt, exuCfg.fpSrcCnt)
  val fastPortsCnt = fastPortsCfg.size
  val slowPortsCnt = slowPortsCfg.size
  require(nonBlocked==fastWakeup)

  val io = IO(new Bundle {
    val numExist = Output(UInt(iqIdxWidth.W))
    val fromDispatch = Flipped(DecoupledIO(new MicroOp))
    val deq = DecoupledIO(new ExuInput)
    val srcRegValue = Input(Vec(srcNum, UInt(srcLen.W)))

    val stIssuePtr = if (exuCfg == Exu.ldExeUnitCfg) Input(new SqPtr()) else null

    val fpRegValue = if (exuCfg == Exu.stExeUnitCfg) Input(UInt(srcLen.W)) else null
    val jumpPc = if(exuCfg == Exu.jumpExeUnitCfg) Input(UInt(VAddrBits.W)) else null
    val jalr_target = if(exuCfg == Exu.jumpExeUnitCfg) Input(UInt(VAddrBits.W)) else null

    val fastUopOut = ValidIO(new MicroOp)
    val fastUopsIn = Vec(fastPortsCnt, Flipped(ValidIO(new MicroOp)))
    val fastDatas = Vec(fastPortsCnt, Input(UInt(srcLen.W)))
    val slowPorts = Vec(slowPortsCnt, Flipped(ValidIO(new ExuOutput)))

    val redirect = Flipped(ValidIO(new Redirect))
    val flush = Input(Bool())

    val memfeedback = if (feedback) Flipped(ValidIO(new RSFeedback)) else null
    val rsIdx = if (feedback) Output(UInt(log2Up(IssQueSize).W)) else null
    val isFirstIssue = if (feedback) Output(Bool()) else null // NOTE: just use for tlb perf cnt
  })

  val select = Module(new ReservationStationSelect(exuCfg, iqSize, srcLen, fastPortsCfg, slowPortsCfg, fixedDelay, fastWakeup, feedback))
  val ctrl   = Module(new ReservationStationCtrl(exuCfg, iqSize, srcLen, fastPortsCfg, slowPortsCfg, fixedDelay, fastWakeup, feedback))
  val data   = Module(new ReservationStationData(exuCfg, iqSize, srcLen, fastPortsCfg, slowPortsCfg, fixedDelay, fastWakeup, feedback))

  select.suggestName(s"${myName}_select")
  ctrl.suggestName(s"${myName}_ctrl")
  data.suggestName(s"${myName}_data")

  select.io.redirect := io.redirect
  select.io.flush := io.flush
  io.numExist := select.io.numExist
  select.io.redirectVec := ctrl.io.redirectVec
  select.io.readyVec := ctrl.io.readyVec
  select.io.enq.valid := io.fromDispatch.valid
  io.fromDispatch.ready := select.io.enq.ready
  select.io.deq.ready := io.deq.ready
  if (feedback) {
    select.io.memfeedback := io.memfeedback
    select.io.flushState := io.memfeedback.bits.flushState
  }

  ctrl.io.in.valid := select.io.enq.fire()// && !(io.redirect.valid || io.flush) // NOTE: same as select
  ctrl.io.flush := io.flush
  ctrl.io.in.bits.addr := select.io.enq.bits
  ctrl.io.in.bits.uop := io.fromDispatch.bits
  ctrl.io.validVec := select.io.validVec
  ctrl.io.indexVec := select.io.indexVec
  ctrl.io.redirect := io.redirect
  ctrl.io.sel.valid := select.io.deq.valid
  ctrl.io.sel.bits  := select.io.deq.bits
  io.fastUopOut := ctrl.io.fastUopOut
  ctrl.io.fastUopsIn := io.fastUopsIn
  ctrl.io.slowUops.zip(io.slowPorts).map{ case (c, i) =>
    c.valid := i.valid
    c.bits  := i.bits.uop
  }
  if (exuCfg == Exu.ldExeUnitCfg) {
    ctrl.io.stIssuePtr := RegNext(io.stIssuePtr)
  }

  data.io.in.valid := ctrl.io.in.valid
  data.io.in.addr := select.io.enq.bits
  data.io.in.uop := io.fromDispatch.bits // NOTE: use for imm-pc src value mux
  data.io.in.enqSrcReady := ctrl.io.enqSrcReady
  data.io.srcRegValue := io.srcRegValue
  if(exuCfg == Exu.jumpExeUnitCfg) {
    data.io.jumpPc := io.jumpPc
    data.io.jalr_target := io.jalr_target
  }
  if (exuCfg == Exu.stExeUnitCfg) {
    data.io.fpRegValue := io.fpRegValue
  }
  data.io.sel := select.io.deq.bits
  data.io.listen.wen := ctrl.io.listen
  for (i <- 0 until fastPortsCnt) {
    data.io.listen.wdata(i) := io.fastDatas(i)
  }
  for (i <- 0 until slowPortsCnt) {
    data.io.listen.wdata(i + fastPortsCnt) := io.slowPorts(i).bits.data
  }

  if (feedback) {
    io.rsIdx := RegNext(select.io.deq.bits) // NOTE: just for feeback
    io.isFirstIssue := select.io.isFirstIssue
  }
  io.deq.bits := DontCare
  io.deq.bits.uop  := ctrl.io.out.bits
  io.deq.bits.uop.cf.exceptionVec := 0.U.asTypeOf(ExceptionVec())
  io.deq.valid     := ctrl.io.out.valid
  io.deq.bits.src1 := data.io.out(0)
  if (srcNum > 1) { io.deq.bits.src2 := data.io.out(1) }
  if (srcNum > 2) { io.deq.bits.src3 := data.io.out(2) }
  if (exuCfg == Exu.jumpExeUnitCfg) { io.deq.bits.uop.cf.pc := data.io.pc }
}

class ReservationStationSelect
(
  val exuCfg: ExuConfig,
  iqSize: Int,
  srcLen: Int,
  fastPortsCfg: Seq[ExuConfig],
  slowPortsCfg: Seq[ExuConfig],
  fixedDelay: Int,
  fastWakeup: Boolean,
  feedback: Boolean,
) extends XSModule with HasCircularQueuePtrHelper{
  val iqIdxWidth = log2Up(iqSize)
  val nonBlocked = fixedDelay >= 0
  val srcNum = if (exuCfg == Exu.jumpExeUnitCfg) 2 else max(exuCfg.intSrcCnt, exuCfg.fpSrcCnt)
  val fastPortsCnt = fastPortsCfg.size
  val slowPortsCnt = slowPortsCfg.size
  require(nonBlocked==fastWakeup)
  val replayDelay = VecInit(Seq(1, 1, 1, 5).map(_.U(5.W)))

  val io = IO(new Bundle {
    val redirect = Flipped(ValidIO(new Redirect))
    val flush = Input(Bool())
    val numExist = Output(UInt(iqIdxWidth.W))
    val memfeedback = if (feedback) Flipped(ValidIO(new RSFeedback)) else null

    val redirectVec = Input(Vec(IssQueSize, Bool()))
    val readyVec = Input(Vec(IssQueSize, Bool()))
    val validVec = Output(Vec(IssQueSize, Bool()))
    val indexVec = Output(Vec(IssQueSize, UInt(iqIdxWidth.W)))

    // val enq = Flipped(DecoupledIO(UInt(iqIdxWidth.W)))
    val enq = new Bundle {
      val valid = Input(Bool())
      val bits  = Output(UInt(iqIdxWidth.W))
      val ready = Output(Bool())
      def fire() = valid && ready
    }
    val deq = DecoupledIO(UInt(iqIdxWidth.W))

    val flushState = if (feedback) Input(Bool()) else null
    val isFirstIssue = if (feedback) Output(Bool()) else null
  })

  def widthMap[T <: Data](f: Int => T) = VecInit((0 until iqSize).map(f))

  /* queue in ctrl part
   * index queue : index
   * state queue : use for replay
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
  val tailPtr       = RegInit(0.U.asTypeOf(new CircularQueuePtr(iqSize)))
  val indexQueue    = RegInit(VecInit((0 until iqSize).map(_.U(iqIdxWidth.W))))
  val validQueue    = VecInit(stateQueue.map(_ === s_valid))
  val emptyQueue    = VecInit(stateQueue.map(_ === s_idle))
  val countQueue    = RegInit(VecInit(Seq.fill(iqSize)(0.U(replayDelay(3).getWidth.W))))
  val cntCountQueue = RegInit(VecInit(Seq.fill(iqSize)(0.U(2.W))))
  val validIdxQueue = widthMap(i => validQueue(indexQueue(i)))
  val readyIdxQueue = widthMap(i => validQueue(indexQueue(i)) && io.readyVec(indexQueue(i)))
  val emptyIdxQueue = widthMap(i => emptyQueue(indexQueue(i)))
  val countIdxQueue = widthMap(i => countQueue(indexQueue(i)))

  // select ready
  // for no replay, select just equal to deq (attached)
  // with   replay, select is just two stage with deq.
  val issueFire = Wire(Bool())
  val moveMask = WireInit(0.U(iqSize.W))
  val lastSelMask = Wire(UInt(iqSize.W))
  val selectMask = WireInit(VecInit((0 until iqSize).map(i => readyIdxQueue(i)))).asUInt & lastSelMask
  val selectIndex = ParallelPriorityMux(selectMask.asBools zip indexQueue) // NOTE: the idx in the indexQueue
  val selectPtr = ParallelPriorityMux(selectMask.asBools.zipWithIndex.map{ case (a,i) => (a, i.U)}) // NOTE: the idx of indexQueue
  val haveReady = Cat(selectMask).orR
  val selectIndexReg = RegNext(selectIndex, init = 0.U)
  val selectValid = haveReady
  val selectReg = RegNext(selectValid, init = false.B)
  val selectPtrReg = RegNext(Mux(moveMask(selectPtr), selectPtr-1.U, selectPtr), init = 0.U)
  lastSelMask := ~Mux(selectReg, UIntToOH(selectPtrReg), 0.U)
  assert(RegNext(!(haveReady && selectPtr >= tailPtr.asUInt)), "bubble should not have valid state like s_valid or s_wait")

  // sel bubble
  val isFull = Wire(Bool())
  val lastbubbleMask = Wire(UInt(iqSize.W))
  val bubbleMask = WireInit(VecInit((0 until iqSize).map(i => emptyIdxQueue(i)))).asUInt & lastbubbleMask
  // val bubbleIndex = ParallelMux(bubbleMask zip indexQueue) // NOTE: the idx in the indexQueue
  val bubblePtr= ParallelPriorityMux(bubbleMask.asBools.zipWithIndex.map{ case (a,i) => (a, i.U)}) // NOTE: the idx of the indexQueue
  val findBubble = Cat(bubbleMask).orR
  val haveBubble = findBubble && (bubblePtr < tailPtr.asUInt)
  val bubbleIndex = indexQueue(bubblePtr)
  val bubbleValid = haveBubble  && (if (feedback) true.B
                                    else if (nonBlocked) !selectValid
                                    else Mux(isFull, true.B, !selectValid))
  val bubbleReg = RegNext(bubbleValid, init = false.B)
  val bubblePtrReg = RegNext(Mux(moveMask(bubblePtr), bubblePtr-1.U, bubblePtr), init = 0.U)
  lastbubbleMask := ~Mux(bubbleReg, UIntToOH(bubblePtrReg), 0.U) &
                    (if(feedback) ~(0.U(iqSize.W)) else
                    Mux(RegNext(selectValid && (io.redirect.valid || io.flush)), 0.U, ~(0.U(iqSize.W))))

  // deq
  val dequeue = Mux(RegNext(io.flush), false.B,
                    if (feedback) bubbleReg else bubbleReg || issueFire)
  val deqPtr = if (feedback) bubblePtrReg
               else if (nonBlocked) Mux(selectReg, selectPtrReg, bubblePtrReg)
               else Mux(bubbleReg, bubblePtrReg, selectPtrReg)
  moveMask := {
    (Fill(iqSize, 1.U(1.W)) << deqPtr)(iqSize-1, 0)
  } & Fill(iqSize, dequeue)

  for (i <- 0 until iqSize - 1) {
    when(moveMask(i)){
      indexQueue(i) := indexQueue(i+1)
    }
  }
  when(dequeue){
    indexQueue.last := indexQueue(deqPtr)
  }

  if (feedback) {
    when (io.memfeedback.valid) {
      when (stateQueue(io.memfeedback.bits.rsIdx) === s_wait) {
        stateQueue(io.memfeedback.bits.rsIdx) := Mux(io.memfeedback.bits.hit, s_idle, s_replay)
      }
      when (!io.memfeedback.bits.hit) {
        countQueue(io.memfeedback.bits.rsIdx) := replayDelay(cntCountQueue(io.memfeedback.bits.rsIdx))
      }
    }
  }

  when (issueFire) {
    if (feedback) { when (stateQueue(selectIndexReg) === s_valid) { stateQueue(selectIndexReg) := s_wait } }
    else { stateQueue(selectIndexReg) := s_idle } // NOTE: reset the state for seclectMask timing to avoid operaion '<'
  }

  // redirect and feedback && wakeup
  for (i <- 0 until iqSize) {
    // replay
    if (feedback) {
      when (stateQueue(i) === s_replay) {
        countQueue(i) := countQueue(i) - 1.U
        when (countQueue(i) === 0.U && !io.flushState) {
          cntCountQueue(i) := Mux(cntCountQueue(i)===3.U, cntCountQueue(i), cntCountQueue(i) + 1.U)
        }
        when (io.flushState || countQueue(i) === 0.U) {
          stateQueue(i) := s_valid
        }
      }
    }

    // redirect
    when (io.redirectVec(i)) {
      stateQueue(i) := s_idle
    }
  }

  // output
  val issueValid = selectReg
  if (nonBlocked) {
    issueFire := issueValid
  } else {
    issueFire := issueValid && io.deq.ready
  }

  // enq
  isFull := tailPtr.flag
  // agreement with dispatch: don't fire when io.redirect.valid
  val enqueue = io.enq.fire() && !(io.redirect.valid || io.flush)
  val tailInc = tailPtr + 1.U
  val tailDec = tailPtr - 1.U
  val nextTailPtr = Mux(io.flush, 0.U.asTypeOf(new CircularQueuePtr(iqSize)), Mux(dequeue === enqueue, tailPtr, Mux(dequeue, tailDec, tailInc)))
  tailPtr := nextTailPtr
  assert(!(tailPtr === 0.U.asTypeOf(new CircularQueuePtr(iqSize))) || Cat(stateQueue.map(_ === s_idle)).andR)

  val enqPtr = Mux(tailPtr.flag, deqPtr, tailPtr.value)
  val enqIdx = indexQueue(enqPtr)
  when (enqueue) {
    stateQueue(enqIdx) := s_valid
    cntCountQueue(enqIdx) := 0.U
  }

  io.validVec := validIdxQueue.zip(lastSelMask.asBools).map{ case (a, b) => a & b }
  io.indexVec := indexQueue

  io.enq.ready := !isFull || (if(feedback || nonBlocked) dequeue else false.B)
  io.enq.bits  := enqIdx
  io.deq.valid := selectValid
  io.deq.bits  := selectIndex

  io.numExist := RegNext(Mux(nextTailPtr.flag, if(isPow2(iqSize)) (iqSize-1).U else iqSize.U, nextTailPtr.value), init = (iqSize - 1).U)

  assert(RegNext(Mux(tailPtr.flag, tailPtr.value===0.U, true.B)))

  XSPerf("enq", enqueue)
  XSPerf("issueFire", issueFire)
  XSPerf("issueValid", issueValid)
  XSPerf("exuBlockDeq", issueValid && !io.deq.ready)
  XSPerf("bubbleBlockEnq", haveBubble && !io.enq.ready)
  XSPerf("validButNotSel", PopCount(selectMask) - haveReady)
  
  QueuePerf(iqSize, io.numExist, !io.enq.ready)
  XSPerf("validUtil", PopCount(validQueue))
  XSPerf("emptyUtil", io.numExist - PopCount(validQueue) - PopCount(stateQueue.map(_ === s_replay)) - PopCount(stateQueue.map(_ === s_wait))) // NOTE: hard to count, use utilization - nonEmpty
  XSPerf("readyUtil", PopCount(readyIdxQueue))
  XSPerf("selectUtil", PopCount(selectMask))
  XSPerf("waitUtil", PopCount(stateQueue.map(_ === s_wait)))
  XSPerf("replayUtil", PopCount(stateQueue.map(_ === s_replay)))

  
  if (!feedback && nonBlocked) {
    XSPerf("issueValidButBubbleDeq", selectReg && bubbleReg && (deqPtr === bubblePtr))
    XSPerf("bubbleShouldNotHaveDeq", selectReg && bubbleReg && (deqPtr === bubblePtr) && io.deq.ready)
  }
  if (feedback) {
    XSPerf("ptwFlushState", io.flushState)
    XSPerf("ptwFlushEntries", Mux(io.flushState, PopCount(stateQueue.map(_ === s_replay)), 0.U))
    XSPerf("replayTimesSum", PopCount(io.memfeedback.valid && !io.memfeedback.bits.hit))
    for (i <- 0 until iqSize) {
      // NOTE: maybe useless, for logical queue and phyical queue make this no sense
      XSPerf(s"replayTimeOfEntry${i}", io.memfeedback.valid && !io.memfeedback.bits.hit && io.memfeedback.bits.rsIdx === i.U)
    }
    io.isFirstIssue := RegNext(ParallelPriorityMux(selectMask.asBools zip cntCountQueue) === 0.U) 
  }
  for(i <- 0 until iqSize) {
    if (i == 0) XSPerf("empty", io.numExist === 0.U)
    else if (i == iqSize) XSPerf("full", isFull)
    else XSPerf(s"numExistIs${i}", io.numExist === i.U)
  }
}

class ReservationStationCtrl
(
  val exuCfg: ExuConfig,
  iqSize: Int,
  srcLen: Int,
  fastPortsCfg: Seq[ExuConfig],
  slowPortsCfg: Seq[ExuConfig],
  fixedDelay: Int,
  fastWakeup: Boolean,
  feedback: Boolean,
) extends XSModule with HasCircularQueuePtrHelper {
  val iqIdxWidth = log2Up(iqSize)
  val nonBlocked = fixedDelay >= 0
  val srcNum = if (exuCfg == Exu.jumpExeUnitCfg) 2 else max(exuCfg.intSrcCnt, exuCfg.fpSrcCnt)
  val fastPortsCnt = fastPortsCfg.size
  val slowPortsCnt = slowPortsCfg.size
  require(nonBlocked==fastWakeup)

  val io = IO(new XSBundle {

    val redirect = Flipped(ValidIO(new Redirect))
    val flush = Input(Bool())

    val in = Flipped(ValidIO(new Bundle {
      val addr = UInt(iqIdxWidth.W)
      val uop  = new MicroOp
    }))
    val sel = Flipped(ValidIO(UInt(iqIdxWidth.W)))
    val out = ValidIO(new MicroOp)

    val redirectVec = Output(Vec(IssQueSize, Bool()))
    val readyVec = Output(Vec(IssQueSize, Bool()))
    val validVec = Input(Vec(IssQueSize, Bool()))
    val indexVec = Input(Vec(IssQueSize, UInt(iqIdxWidth.W)))

    val fastUopOut = ValidIO(new MicroOp)
    val fastUopsIn = Flipped(Vec(fastPortsCnt, ValidIO(new MicroOp)))
    val slowUops   = Flipped(Vec(slowPortsCnt, ValidIO(new MicroOp)))

    val listen = Output(Vec(srcNum, Vec(iqSize, Vec(fastPortsCnt + slowPortsCnt, Bool()))))
    val enqSrcReady = Output(Vec(srcNum, Bool()))

    val stIssuePtr = if (exuCfg == Exu.ldExeUnitCfg) Input(new SqPtr()) else null
  })

  val selValid = io.sel.valid
  val enqPtr = io.in.bits.addr
  val enqPtrReg = RegNext(enqPtr)
  val enqEn  = io.in.valid
  val enqEnReg = RegNext(enqEn, init = false.B)
  val enqUop = io.in.bits.uop
  val enqUopReg = RegEnable(enqUop, selValid)
  val selPtr = io.sel.bits
  val selPtrReg = RegEnable(selPtr, selValid)
  val data = io.listen
  data.map(a => a.map(b => b.map(_ := false.B)))

  val fastUops = io.fastUopsIn
  val slowUops = io.slowUops
  val lastFastUops = RegNext(fastUops)

  def stateCheck(src: UInt, srcType: UInt): Bool = {
    (srcType =/= SrcType.reg && srcType =/= SrcType.fp) ||
    (srcType === SrcType.reg && src === 0.U)
  }
  val enqSrcSeq      = Seq(enqUop.psrc1, enqUop.psrc2, enqUop.psrc3)
  val enqSrcTypeSeq  = Seq(enqUop.ctrl.src1Type, enqUop.ctrl.src2Type, enqUop.ctrl.src3Type)
  val enqSrcStateSeq = Seq(enqUop.src1State, enqUop.src2State, enqUop.src3State)
  val enqSrcReady = (0 until srcNum).map(i =>
    stateCheck(enqSrcSeq(i), enqSrcTypeSeq(i)) || (enqSrcStateSeq(i) === SrcState.rdy)
  )
  io.enqSrcReady := enqSrcReady
  val srcUpdate = Wire(Vec(iqSize, Vec(srcNum, Bool())))
  val srcUpdateListen = Wire(Vec(iqSize, Vec(srcNum, Vec(fastPortsCnt + slowPortsCnt, Bool()))))
  srcUpdateListen.map(a => a.map(b => b.map(c => c := false.B )))
  srcUpdateListen.suggestName(s"srcUpdateListen")
  val srcUpdateVecReg = RegNext(srcUpdateListen, init = 0.U.asTypeOf(srcUpdateListen.cloneType))
  for (i <- 0 until iqSize) {
    for (j <- 0 until srcNum) {
      if (exuCfg == Exu.stExeUnitCfg && j == 0) {
        srcUpdate(i)(j) := Cat(srcUpdateVecReg(i)(j).zip(fastPortsCfg ++ slowPortsCfg).filter(_._2.writeIntRf).map(_._1)).orR
      } else {
        srcUpdate(i)(j) := Cat(srcUpdateVecReg(i)(j)).orR
      }
    }
  }

  val srcQueue      = RegInit(VecInit(Seq.fill(iqSize)(VecInit(Seq.fill(srcNum)(false.B)))))
  when (enqEn) {
    srcQueue(enqPtr).zip(enqSrcReady).map{ case (s, e) => s := e }
  }
  // NOTE: delay one cycle for fp src will come one cycle later than usual
  if (exuCfg == Exu.stExeUnitCfg) {
    when (enqEn) {
      when (enqUop.ctrl.src2Type === SrcType.fp) { srcQueue(enqPtr)(1) := false.B }
    }
    when (enqEnReg && RegNext(enqUop.ctrl.src2Type === SrcType.fp && enqSrcReady(1))) {
      srcQueue(enqPtrReg)(1) := true.B
    }
  }
  val srcQueueWire = VecInit((0 until srcQueue.size).map(i => {
    VecInit((0 until srcQueue(i).size).map{j =>
      srcQueue(i)(j) || srcUpdate(i)(j)
    })
  }))
  for (i <- 0 until iqSize) {
    for (j <- 0 until srcNum) {
      when (srcQueueWire(i)(j) && !(enqPtr === i.U && io.in.valid)) { srcQueue(i)(j) := true.B }
    }
  }

  // load wait store
  io.readyVec := srcQueueWire.map(Cat(_).andR)
  if (exuCfg == Exu.ldExeUnitCfg) {
    val ldWait = Reg(Vec(iqSize, Bool()))
    val sqIdx  = Reg(Vec(iqSize, new SqPtr()))
    ldWait.zip(sqIdx).map{ case (lw, sq) =>
      when (!isAfter(sq, io.stIssuePtr)) {
        lw := true.B
      }
    }
    when (enqEn) {
      ldWait(enqPtr) := !enqUop.cf.loadWaitBit
      sqIdx(enqPtr)  := enqUop.sqIdx
    }
    ldWait.suggestName(s"${this.name}_ldWait")
    sqIdx.suggestName(s"${this.name}_sqIdx")
    io.readyVec := srcQueueWire.map(Cat(_).andR).zip(ldWait).map{ case (s, l) => s&l }
  }

  val redirectHit = io.redirectVec(selPtr)
  val uop = Module(new SyncDataModuleTemplate(new MicroOp, iqSize, 1, 1))

  uop.io.raddr(0) := selPtr
  io.out.valid    := RegNext(selValid && ~redirectHit)
  io.out.bits     := uop.io.rdata(0)
  uop.io.wen(0)   := enqEn
  uop.io.waddr(0) := enqPtr
  uop.io.wdata(0) := enqUop

  class fastSendUop extends XSBundle {
    val pdest = UInt(PhyRegIdxWidth.W)
    val rfWen = Bool()
    val fpWen = Bool()
    def apply(uop: MicroOp) = {
      this.pdest := uop.pdest
      this.rfWen := uop.ctrl.rfWen
      this.fpWen := uop.ctrl.fpWen
      this
    }
  }

  val roqIdx = Reg(Vec(IssQueSize, new RoqPtr))
  when (enqEn) {
    roqIdx(enqPtr) := enqUop.roqIdx
  }
  io.redirectVec.zip(roqIdx).map{ case (red, roq) =>
    red := roq.needFlush(io.redirect, io.flush)
  }
  io.out.bits.roqIdx := roqIdx(selPtrReg)

  io.fastUopOut := DontCare
  if (fastWakeup) {
    val asynUop = Reg(Vec(iqSize, new fastSendUop))
    when (enqEn) { asynUop(enqPtr) := (Wire(new fastSendUop)).apply(enqUop) }
    val asynIdxUop = (0 until iqSize).map(i => asynUop(io.indexVec(i)) )
    val readyIdxVec = (0 until iqSize).map(i => io.validVec(i) && Cat(srcQueueWire(io.indexVec(i))).andR )
    val fastAsynUop = ParallelPriorityMux(readyIdxVec zip asynIdxUop)
    val fastRoqIdx = ParallelPriorityMux(readyIdxVec zip (0 until iqSize).map(i => roqIdx(io.indexVec(i))))
    val fastSentUop = Wire(new MicroOp)
    fastSentUop := DontCare
    fastSentUop.pdest := fastAsynUop.pdest
    fastSentUop.ctrl.rfWen := fastAsynUop.rfWen
    fastSentUop.ctrl.fpWen := fastAsynUop.fpWen

    if (fixedDelay == 0) {
      io.fastUopOut.valid := selValid
      io.fastUopOut.bits  := fastSentUop
    } else {
      val bpQueue = Module(new BypassQueue(fixedDelay))
      bpQueue.io.in.valid := selValid
      bpQueue.io.in.bits  := fastSentUop
      bpQueue.io.in.bits.roqIdx := fastRoqIdx
      bpQueue.io.redirect := io.redirect
      bpQueue.io.flush    := io.flush
      io.fastUopOut.valid := bpQueue.io.out.valid
      io.fastUopOut.bits  := bpQueue.io.out.bits
    }

    val fastSentUopReg = RegNext(fastAsynUop)
    io.out.bits.pdest := fastSentUopReg.pdest
    io.out.bits.ctrl.rfWen := fastSentUopReg.rfWen
    io.out.bits.ctrl.fpWen := fastSentUopReg.fpWen
  }

  val psrc = (0 until srcNum).map(i => Module(new SingleSrcCAM(UInt(PhyRegIdxWidth.W), iqSize, fastPortsCnt + slowPortsCnt, true)).io)
  psrc.map(_.w.valid := false.B)
  val entryListenHit = psrc.map{src =>
    for (i <- 0 until fastPortsCnt) { src.r.req(i) := io.fastUopsIn(i).bits.pdest }
    for (i <- 0 until slowPortsCnt) { src.r.req(i + fastPortsCnt) := io.slowUops(i).bits.pdest }
    src.r.resp
  }
  val srcIsZero = psrc.map{ src => src.zero }
  psrc.map(_.w.bits.addr  := enqPtr)
  psrc.map(_.w.valid := enqEn)
  val enqSrcSeqChecked = enqSrcSeq.zip(enqSrcTypeSeq).map{ case (s, t) =>
    Mux(t === SrcType.fp || t === SrcType.reg, s, 0.U)} // NOTE: if pc/imm -> 0.U and reg (means don't hit)
  psrc.zip(enqSrcSeqChecked).map{ case (p,s) => p.w.bits.data := s }

  // TODO: later, only store will need psrcType
  val psrcType = Reg(Vec(srcNum, Vec(iqSize, Bool()))) // fp: false | other: true
  (0 until srcNum).foreach{ i =>
    when (enqEn) {
      psrcType(i)(enqPtr) := enqSrcTypeSeq(i) =/= SrcType.fp
    }
  }

  def listenHitEnq(uop: MicroOp, src: UInt, srctype: UInt): Bool = {
    (src === uop.pdest) &&
    ((srctype === SrcType.reg && uop.ctrl.rfWen && src=/=0.U) ||
     (srctype === SrcType.fp  && uop.ctrl.fpWen))
  }

  def listenHitEntry(src: Int, port: Int, addr: Int, uop: MicroOp): Bool = {
    entryListenHit(src)(port)(addr) &&
    ((psrcType(src)(addr) && uop.ctrl.rfWen && !srcIsZero(src)(addr)) ||
     (!psrcType(src)(addr)  && uop.ctrl.fpWen))
  }

  for (j <- 0 until srcNum) {
    for (i <- 0 until iqSize) {
      for (k <- 0 until fastPortsCnt) {
        val fastHit = listenHitEntry(j, k, i, fastUops(k).bits) && fastUops(k).valid
        val fastHitNoConflict = fastHit && !(enqPtr===i.U && enqEn)
        when (fastHitNoConflict) { srcUpdateListen(i)(j)(k) := true.B }
        when (RegNext(fastHitNoConflict) && !(enqPtr===i.U && enqEn)) { data(j)(i)(k) := true.B }
      }
      for (k <- 0 until slowPortsCnt) {
        val slowHit = listenHitEntry(j, k + fastPortsCnt, i, slowUops(k).bits) && slowUops(k).valid
        val slowHitNoConflict = slowHit && !(enqPtr===i.U && enqEn)
        when (slowHitNoConflict) { srcUpdateListen(i)(j)(k+fastPortsCnt) := true.B }
        when (slowHitNoConflict) { data(j)(i)(k + fastPortsCnt) := true.B }
      }
    }
  }

  // enq listen
  for (j <- 0 until srcNum) {
    for (k <- 0 until fastPortsCnt) {
      val fastHit = listenHitEnq(fastUops(k).bits, enqSrcSeq(j), enqSrcTypeSeq(j)) && enqEn && fastUops(k).valid
      val lastFastHit = listenHitEnq(lastFastUops(k).bits, enqSrcSeq(j), enqSrcTypeSeq(j)) && enqEn && lastFastUops(k).valid
      when (fastHit || lastFastHit) { srcUpdateListen(enqPtr)(j)(k) := true.B }
      when (lastFastHit)            { data(j)(enqPtr)(k) := true.B }
      when (RegNext(fastHit))       { data(j)(enqPtrReg)(k) := true.B }
    }
    for (k <- 0 until slowPortsCnt) {
      val slowHit = listenHitEnq(slowUops(k).bits, enqSrcSeq(j), enqSrcTypeSeq(j)) && enqEn && slowUops(k).valid
      when (slowHit) {
        srcUpdateListen(enqPtr)(j)(k+fastPortsCnt) := true.B
        data(j)(enqPtr)(k + fastPortsCnt) := true.B
      }
    }
  }
}

class RSDataSingleSrc(srcLen: Int, numEntries: Int, numListen: Int, writePort: Int = 1) extends XSModule {
  val io = IO(new Bundle {
    val r = new Bundle {
      // val valid = Bool() // NOTE: if read valid is necessary, but now it is not completed
      val addr = Input(UInt(log2Up(numEntries).W))
      val rdata = Output(UInt(srcLen.W))
    }
    val w = Input(Vec(writePort, new Bundle {
      val wen = Bool()
      val addr = UInt(log2Up(numEntries).W)
      val wdata = UInt(srcLen.W)
    }))
    val listen = Input(new Bundle {
      val wdata = Vec(numListen, UInt(srcLen.W))
      val wen = Vec(numEntries, Vec(numListen, Bool()))
    })
  })

  val value = Reg(Vec(numEntries, UInt(srcLen.W)))

  val wMaskT = io.w.map(w => Mux(w.wen, UIntToOH(w.addr)(numEntries-1, 0), 0.U(numEntries.W)))
  val wMask = (0 until numEntries).map(i =>
                (0 until writePort).map(j =>
                  wMaskT(j)(i)
              ))
  val wData = io.w.map(w => w.wdata)
  val data = io.listen.wdata ++ io.w.map(_.wdata)
  val wen = io.listen.wen.zip(wMask).map{ case (w, m) => w ++ m }
  for (i <- 0 until numEntries) {
    when (Cat(wen(i)).orR) {
      value(i) := ParallelMux(wen(i) zip data)
      // assert(RegNext(PopCount(wen(i))===0.U || PopCount(wen(i))===1.U), s"${i}")
    }
  }

  io.r.rdata := value(RegNext(io.r.addr)) // NOTE: the read addr will arrive one cycle before
}

class ReservationStationData
(
  val exuCfg: ExuConfig,
  iqSize: Int,
  srcLen: Int,
  fastPortsCfg: Seq[ExuConfig],
  slowPortsCfg: Seq[ExuConfig],
  fixedDelay: Int,
  fastWakeup: Boolean,
  feedback: Boolean,
) extends XSModule {
  val iqIdxWidth = log2Up(iqSize)
  val nonBlocked = fixedDelay >= 0
  val srcNum = if (exuCfg == Exu.jumpExeUnitCfg) 2 else max(exuCfg.intSrcCnt, exuCfg.fpSrcCnt)
  val fastPortsCnt = fastPortsCfg.size
  val slowPortsCnt = slowPortsCfg.size
  require(nonBlocked==fastWakeup)

  val io = IO(new XSBundle {
    val srcRegValue = Vec(srcNum, Input(UInt(srcLen.W)))
    val fpRegValue = if (exuCfg == Exu.stExeUnitCfg) Input(UInt(srcLen.W)) else null
    val jumpPc = if(exuCfg == Exu.jumpExeUnitCfg) Input(UInt(VAddrBits.W)) else null
    val jalr_target = if(exuCfg == Exu.jumpExeUnitCfg) Input(UInt(VAddrBits.W)) else null
    val in  = Input(new Bundle {
      val valid = Input(Bool())
      val addr = Input(UInt(iqIdxWidth.W))
      val uop = Input(new MicroOp)
      val enqSrcReady = Input(Vec(srcNum, Bool()))
    })

    val listen = new Bundle {
      val wen = Input(Vec(srcNum, Vec(iqSize, Vec(fastPortsCnt + slowPortsCnt, Bool()))))
      val wdata = Input(Vec(fastPortsCnt + slowPortsCnt, UInt(srcLen.W)))
    }

    val sel = Input(UInt(iqIdxWidth.W))
    val out = Output(Vec(srcNum, UInt(srcLen.W)))
    val pc = if(exuCfg == Exu.jumpExeUnitCfg) Output(UInt(VAddrBits.W)) else null
  })

  val enqUopReg = RegEnable(io.in.uop, io.in.valid)

  // Data : single read, multi write
  // ------------------------
  val data = if (exuCfg == Exu.stExeUnitCfg) {
    val baseListenWidth = (fastPortsCfg ++ slowPortsCfg).filter(_.writeIntRf).size
    val srcBase = Module(new RSDataSingleSrc(srcLen, iqSize, baseListenWidth, 1))
    val srcData = Module(new RSDataSingleSrc(srcLen, iqSize, fastPortsCnt + slowPortsCnt, 2))
    srcBase.suggestName(s"${this.name}_data0")
    srcData.suggestName(s"${this.name}_data1")
    Seq(srcBase.io, srcData.io)
  } else {
    (0 until srcNum).map{i =>
      val d = Module(new RSDataSingleSrc(srcLen, iqSize, fastPortsCnt + slowPortsCnt, 1))
      d.suggestName(s"${this.name}_data${i}")
      d.io
    }
  }
  (0 until srcNum).foreach{ i =>
    if (exuCfg == Exu.stExeUnitCfg && i == 0) {
      data(i).listen.wen := VecInit(io.listen.wen(i).map(a => VecInit(a.zip((fastPortsCfg ++ slowPortsCfg).map(_.writeIntRf)).filter(_._2).map(_._1))))
      data(i).listen.wdata := io.listen.wdata.zip((fastPortsCfg ++ slowPortsCfg).map(_.writeIntRf)).filter(_._2).map(_._1)
    } else {
      data(i).listen.wen := io.listen.wen(i)
      data(i).listen.wdata := io.listen.wdata
    }
  }

  val addrReg = RegEnable(io.in.addr, io.in.valid)
  val enqSrcReadyReg = io.in.enqSrcReady.map(r => RegNext(r && io.in.valid))
  data.map(_.w(0).addr := addrReg)
  data.zip(enqSrcReadyReg).map{ case (src, ready) => src.w(0).wen := ready }

  val pcMem = if(exuCfg == Exu.jumpExeUnitCfg)
    Some(Module(new SyncDataModuleTemplate(UInt(VAddrBits.W), iqSize, numRead = 1, numWrite = 1))) else None

  if(pcMem.nonEmpty){
    pcMem.get.io.wen(0) := RegNext(io.in.valid)
    pcMem.get.io.waddr(0) := addrReg
    pcMem.get.io.wdata(0) := io.jumpPc
  }

  exuCfg match {
    case Exu.jumpExeUnitCfg =>
      val src1Mux = Mux(enqUopReg.ctrl.src1Type === SrcType.pc,
                        SignExt(io.jumpPc, XLEN),
                        io.srcRegValue(0)
                    )
      // data.io.w.bits.data(0) := src1Mux
      data(0).w(0).wdata := src1Mux
      data(1).w(0).wdata := io.jalr_target

    case Exu.aluExeUnitCfg =>
      data(0).w(0).wdata := io.srcRegValue(0)
      // alu only need U type and I type imm
      val imm32 = Mux(enqUopReg.ctrl.selImm === SelImm.IMM_U,
                    ImmUnion.U.toImm32(enqUopReg.ctrl.imm),
                    ImmUnion.I.toImm32(enqUopReg.ctrl.imm)
                  )
      val imm64 = SignExt(imm32, XLEN)
      val src2Mux = Mux(enqUopReg.ctrl.src2Type === SrcType.imm,
                      imm64, io.srcRegValue(1)
                    )
      data(1).w(0).wdata := src2Mux

    case Exu.stExeUnitCfg =>
      (0 until srcNum).foreach(i => data(i).w(0).wdata := io.srcRegValue(i) )
      data(1).w(1).wdata := io.fpRegValue
      data(1).w(1).addr := RegNext(addrReg)
      data(1).w(1).wen   := RegNext(enqSrcReadyReg(1) && enqUopReg.ctrl.src2Type === SrcType.fp)
      data(1).w(0).wen   := enqSrcReadyReg(1) && enqUopReg.ctrl.src2Type =/= SrcType.fp

    case _ =>
      (0 until srcNum).foreach(i => data(i).w(0).wdata := io.srcRegValue(i) )
  }
  // deq
  data.map(_.r.addr := io.sel)
  io.out := data.map(_.r.rdata)
  if(pcMem.nonEmpty){
    pcMem.get.io.raddr(0) := io.sel
    io.pc := pcMem.get.io.rdata(0)
  }
}
