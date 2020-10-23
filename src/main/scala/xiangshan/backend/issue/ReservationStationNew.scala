package xiangshan.backend.issue

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.exu.{Exu, ExuConfig}
import java.rmi.registry.Registry
import java.{util => ju}

class SrcBundle extends XSBundle {
  val src = UInt(PhyRegIdxWidth.W)
  val state = SrcState()
  val srctype = SrcType()
  
  def hit(uop: MicorOp) : Bool = {
    (src === uop.pdest) && (state === SrcState.busy) &&
    ((srctype === SrcType.reg && uop.ctrl.rfWen) ||
     (srctype === SrcType.fp  && uop.ctrl.fpWen))
  }
}

class ReservationStationNew
(
  val exuCfg: ExuConfig,
  wakeupCnt: Int,
  extraListenPortsCnt: Int
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

  io <> DontCare

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
  val validQueue    = RegInit(VecInit(Seq.fill(false.B)))
  val srcQueue      = RegInit(Vec(iqSize, Seq.fill(srcNum)(new SrcBundle)))

  // data part:
  val data          = Reg(Vec(iqSize, Seq.fill(3/*srcNum*/)(UInt(XLEN.W))))

  // other part:
  val uop           = Reg(Vec(iqSize, new MicroOp))

  // rs queue part:
  val tailPtr       = RegInit(0.U(idxWidth+1).W)
  val idxQueue      = RegInit(VecInit((0 until iqSize).map(_.U(idxWidth.W))))
  val readyQueue    = srcState.map(_.andR).zip(validQueue).map(_&_)

  // real deq
  // TODO: 
  val (firstBubble, findBubble) = PriorityEncoderWithFlag(validQueue.map(!_))
  val (firstReady, findReady) = PriorityEncoderWithFlag(validQueue)
  val deqIdx = Mux(findBubble, firstBubble, findReady)
  val deqValid = ((firstBubble < tailPtr) && findBubble) || ((firstReady < tailPtr) && findReady)
  val moveMask = {
    (Fill(iqSize, 1.U(1.W)) << deqIdx)(qsize-1, 0)
  } & Fill(iqSize, deqValid)

  for(i <- 0 until qsize-1){
    when(moveMask(i)){
      idxQueue(i)   := idxQueue(i+1)
      srcQueue(i)   := srcQueue(i+1)
      validQueue(i) := validQueue(i+1)
    }
  }
  when(realDeqValid){
    idxQueue.last := idxQueue(realDeqIdx)
    validQueue.last := false.B
  }

  // wakeup and bypass and flush
  // data update and control update
  // bypass update and wakeup update -> wakeup method and bypass method may not be ok
  // for ld/st, still need send to control part, long latency
  def wakeup(src: SrcState) : (Bool, UInt) = {
    val hitVec = extraListenPorts.map(port => src.hit(port.bits.uop) && port.valid)
    assert(PopCount(hitVec)===0.U || PopCount(hitVec)===1.U)
    
    val hit = hitVec.orR
    (Mux(hit, ParallelMux(hitVec zip extraListenPorts.map(_.data)))
  }

  def bypass(src: SrcState) : (Bool, Bool, UInt) = {
    val hitVec = broadcastedUops.map(port => src.hit(port.bits) && port.valid)
    assert(PopCount(hitVec)===0.U || PopCount(hitVec)===1.U)

    val hit = hitVec.orR
    (Mux(hit, RegNext(hit), ParallelMux(RegNext(hitVec) zip writeBackedData))
  }
  
  for (i <- 0 until iqSize) {
    for (j <- 0 until srcNum) {
      val (wuHit, wuData) = wakeup(srcQueue(i)(j))
      val (bpHit, bpHitReg, bpData) = bypass(srcQueue(i)(j))
      assert(!(bpHit && wuHit))
      assert(!(bpHitReg && wuHit))

      when (wuHit || bpHit) { srcQueue(i)(j).srcState := SrcState.rdy }
      when (wuHit) { data(idxQueue(i))(j) := wuData }
      when (bpHitReg) { data(RegNext(idxQueue(i)))(j) := bpData }
      // NOTE: can not use Mem/Sram to store data, for multi-read/multi-write
    }
  }

  // select
  val selectedIdxRegOH = Wire(UInt(qsize.W))
  val selectMask = WireInit(VecInit(
    (0 until qsize).map(i =>
      readyQueue(i) && !(selectedIdxRegOH(i) && io.deq.fire()) // TODO: read it
    )
  ))
  val (selectedIdxWire, selected) = PriorityEncoderWithFlag(selectMask)
  val selReg = RegNext(selected)
  val selectedIdxReg = RegNext(selectedIdxWire - moveMask(selectedIdxWire))
  selectedIdxRegOH := UIntToOH(selectedIdxReg)

  // fake deq
  // TODO: add fake deq later
  // TODO: add deq: may have one more latency, but for replay later.
  // TODO: may change to another way to deq and select, for there is no Vec, but Seq, change to multi-in multi-out
  io.deq.valid := readyQueue(selectedIdxReg) && selReg // TODO: read it and add assert for rdyQueue
  io.deq.bits.uop := uop(idxQueue(selectedIdxReg))
  io.deq.bits.uop.src1 := data(selectedIdxReg)(0)
  if(srcNum > 1) { io.deq.bits.uop.src2 := data(selectedIdxReg)(1) }
  if(srcNum > 2) { io.deq.bits.uop.src3 := data(selectedIdxReg)(2) } // TODO: beautify it
  io.deq.bits.uop.src1State := srcQueue(selectedIdxReg)(0).state
  if(srcNum > 1) { io.deq.bits.uop.src2State := srcQueue(selectedIdxReg)(1).state }
  if(srcNum > 2) { io.deq.bits.uop.src3State := srcQueue(selectedIdxReg)(2).state }

  // enq
  val tailAfterRealDeq = tailPtr - moveMask(tailPtr.tail(1))
  val isFull = tailAfterRealDeq.head(1).asBool() // tailPtr===qsize.U
  
  io.enq.ready := !isFull && !io.redirect.valid // TODO: check this redirect
  for (i <- 0 until iqSize) { // TODO: check is it silly? may be ok for multi-in-multi-out?
    entries.in.valid := i.U === tailPtr && io.enq.valid // TODO: check if ready is needed (decoupledIO or validIO)
    entries.in.uop := io.enq.bits.uop // TODO: change to wakeup(io.enq.bits) later
    // add assert for decoupledIO or remove decoupledIO
  }
  tailPtr := tailAfterRealDeq + io.enq.fire()

  // other io
  io.numExist := tailPtr

  // log
  // TODO: add log
}

class ReservationEntryIO extends XSBundle {
  val in = Flipped(DecoupledIO(new Bundle {
    val valid = Bool()
    val uop = new MicroOp
  }))
  val out = DecoupledIO(new Bundle {
    val valid = Bool()
    val uop = new MicroOp
  })
  val replay = Flipped(ValidIO(new Bundle {
    val roqIdx = UInt()
    val miss   = Bool()
  }))
  val bypass = UInt()
  val wakeup = UInt()
  val redirect = new RedirectIO

  val debug = Output(new Bundle {
    val balabala = UInt()
  })
}

class ReservationEntry(srcNum: Int = 3, enableReplay: Boolean = false) extends XSModule {
  val io = IO(new ReservationEntryIO)

  /*
    invalid --[enq]--> valid --[deq]--> wait --[tlbHit]--> invalid
                                        wait --[replay]--> replay --[cnt]--> valid
  */
  val s_invalid :: s_valid :: s_wait :: s_replay :: Nil = Enum(4)

  val state = RegInit(s_invalid)
  // what need update: srcState && srcData, can we not store them seperately?
  val uop = Mem(new MicroOp)

  val rdy = srcState.orR && (state === s_valid || state === s_replay) // TODO: check it
  val bpCounter = Seq.fill(srcNum)(Reg(UInt(log2Up(MaxFULatency).W)))
  val rpCounter = Reg(UInt()) // TODO: check it

  // in
  assert(state === s_invalid || !io.in.valid) // when io.in.valid, state must be s_invalid now // TODO: change it
  when (io.in.valid && state === s_invalid) {
    state := s_valid
    uop   := io.in.uop
  }

  // out
  when (io.out.ready) {
    assert(state === s_valid) // TODO: decouple the handle logic
    state := s_wait
  }

  when (io.replay.valid && io.replay.roqIdx === uop.roqIdx) {
    state := Mux(io.replay.miss, s_invalid, s_replay) // Will wait for several cycle for l2tlb hit
  }

  assert(rpCounter===0.U || state===s_replay)
  rpCounter := Mux(rpCounter===L2TlbLatency.U, 0.U, rpCounter + state===s_replay)
  when (rpCounter === L2TlbLatency.U) {
    state := s_valid
  }

  // receive bypass
  when (io.bypass.valid) {
    bpCnt := 0.U// TODO: Map()
  }

  // receive wakeup
  when (io.wakeup.valid) {

  }

  // send bypass
  // TODO: may send by outer module, n
}
