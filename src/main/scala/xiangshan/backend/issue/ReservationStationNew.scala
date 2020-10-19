package xiangshan.backend.issue

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.exu.{Exu, ExuConfig}


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

  // TODO: wanna put state machine into ReservationEntry, but difficult to use if we do that, how?
  val entries = (0 until iqSize).map{val entry = Module(new ReservationEntry); entry.io}
  val tailPtr = RegInit(0.U((iqIdxWidth+1).W))
  val idxQueue = RegInit(VecInit((0 until qsize).map(_.U(idxWidth.W))))
  val emptyQueueTmp = VecInit(entries.map(_.state.empty))
  val emptyQueue = (0 until iqSize).map(emptyQueueTmp(idxQueue(i))) // TODO: may have long latency

  // real deq
  
  // TODO: can we apply multi-in multi-out style? 
  val (firstBubble, findBubble) = PriorityEncoderWithFlag(emptyQueue)
  val realDeqIdx = firstBubble
  val realDeqValid = (firstBubble < tailPtr) && findBubble
  val moveMask = {
    (Fill(qsize, 1.U(1.W)) << realDeqIdx)(qsize-1, 0)
  } & Fill(qsize, realDeqValid)

  for(i <- 0 until qsize-1){
    when(moveMask(i)){
      idxQueue(i) := idxQueue(i+1)
    }
  }
  when(realDeqValid){
    idxQueue.last := idxQueue(realDeqIdx)
  }

  // wakeup and bypass and flush
  for (i <- 0 until iqSize) {
    entries(i).bypassUops := io.boradcastedUops
    entries(i).bypassData := io.writeBackedData
    entries(i).wakeup     := io.extraListenPorts
    entries(i).redirect   := io.redirect
  }

  // select
  val rdyQueue = entries.map(_.state.ready)
  val selectedIdxRegOH = Wire(UInt(qsize.W))
  val selectMask = WireInit(VecInit(
    (0 until qsize).map(i =>
      rdyQueue(i) && !(selectedIdxRegOH(i) && io.deq.fire()) // TODO: read it
    )
  ))
  val (selectedIdxWire, sel) = PriorityEncoderWithFlag(selectMask)
  val selReg = RegNext(sel)
  val selectedIdxReg = RegNext(selectedIdxWire - moveMask(selectedIdxWire))
  selectedIdxRegOH := UIntToOH(selectedIdxReg)
  
  // fake deq
  // TODO: add fake deq later
  // TODO: add deq: may have one more latency, but for replay later.
  // TODO: may change to another way to deq and select, for there is no Vec, but Seq, change to multi-in multi-out
  io.deq.valid := rdyQueue(selectedIdxReg) && selReg // TODO: read it and add assert for rdyQueue
  io.deq.bits.uop := entries(0).io.out.uop
  for (i <- 0 until iqSize) {
    when (idxQueue(selectedIdxReg)===i.U) {
      io.deq.bits.uop := entries(i).out.uop
    }
  }

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
