package xiangshan.backend.issue

import chisel3.{util, _}
import chisel3.util._
import utils.{XSDebug, XSInfo}
import xiangshan._
import xiangshan.backend.exu.ExuConfig
import xiangshan.backend.regfile.RfReadPort

class IssueQueue
(
  val exuCfg: ExuConfig,
  val wakeupCnt: Int,
  val bypassCnt: Int = 0
) extends XSModule with HasIQConst with NeedImpl {
  val io = IO(new Bundle() {
    val redirect = Flipped(ValidIO(new Redirect))
    val enq = Flipped(DecoupledIO(new MicroOp))
    val readIntRf = Vec(exuCfg.intSrcCnt, Flipped(new RfReadPort))
    val readFpRf = Vec(exuCfg.fpSrcCnt, Flipped(new RfReadPort))
    val deq = DecoupledIO(new ExuInput)
    val wakeUpPorts = Vec(wakeupCnt, Flipped(ValidIO(new ExuOutput)))
    val bypassUops = Vec(bypassCnt, Flipped(ValidIO(new MicroOp)))
    val numExist = Output(UInt(iqIdxWidth.W))
    // tlb hit, inst can deq
    val tlbHit = Input(Bool())
    val replay = Flipped(ValidIO(UInt(RoqIdxWidth.W)))
  })

  def qsize: Int = IssQueSize
  def idxWidth = log2Up(qsize)
  def replayDelay = 16

  require(isPow2(qsize))

  /*
      invalid --[enq]--> valid --[deq]--> wait --[tlbHit]--> invalid
                                          wait --[replay]--> replay --[cnt]--> valid
   */
  val s_invalid :: s_valid :: s_wait :: s_replay :: Nil = Enum(4)

  val idxQueue = RegInit(VecInit((0 until qsize).map(_.U(idxWidth.W))))
  val stateQueue = RegInit(VecInit(Seq.fill(qsize)(s_invalid)))

  val uopQueue = Reg(Vec(qsize, new MicroOp))
  val cntQueue = Reg(Vec(qsize, UInt(log2Up(replayDelay).W)))

  val tailPtr = RegInit(0.U((idxWidth+1).W))

  // real deq

  /*
    example: realDeqIdx = 2        |  realDeqIdx=0
             moveMask = 11111100   |  moveMask=11111111
 */
  assert(!(io.tlbHit && io.replay.valid), "Error: tlbHit and replay are both true!")

  val firstWait = PriorityEncoder(stateQueue.map(_ === s_wait))
  val firstBubble = PriorityEncoder(stateQueue.map(_ === s_invalid))
  val realDeqIdx = Mux(io.tlbHit, firstWait, firstBubble)
  val realDeqValid = io.tlbHit || ((firstBubble < tailPtr.tail(1)) && !io.replay.valid)
  val moveMask = {
    (Fill(qsize, 1.U(1.W)) << realDeqIdx)(qsize-1, 0)
  } & Fill(qsize, realDeqValid)

  for(i <- 1 until qsize){
    when(moveMask(i)){
      idxQueue(i-1) := idxQueue(i)
      stateQueue(i-1) := stateQueue(i)
    }
  }
  when(realDeqValid){
    idxQueue.last := idxQueue(realDeqIdx)
    stateQueue.last := s_invalid
  }


  // wake up

  // select
  val selectedIdxRegOH = Wire(UInt(qsize.W))
  val selectMask = WireInit(VecInit(
    (0 until qsize).map(i =>
      stateQueue(i)===s_valid && !(selectedIdxRegOH(i) && io.deq.fire())
    )
  ))
  val selectedIdxWire = PriorityEncoder(selectMask)
  val selectedIdxReg = Reg(UInt(log2Up(qsize).W))
  selectedIdxReg := selectedIdxWire - moveMask(selectedIdxWire)
  selectedIdxRegOH := UIntToOH(selectedIdxReg)
  XSDebug(
    p"selMaskWire:${Binary(selectMask.asUInt())} selected:$selectedIdxWire moveMask:${Binary(moveMask)}\n"
  )

  // read data && (fake) deq
  io.deq.valid := stateQueue(selectedIdxReg)===s_valid
  io.deq.bits.uop := uopQueue(idxQueue(selectedIdxReg))
  when(io.deq.fire()){
    stateQueue(selectedIdxReg - moveMask(selectedIdxReg)) := s_wait
    assert(stateQueue(selectedIdxReg) === s_valid, "Dequeue a invalid entry to lsu!")
  }

  assert(!(tailPtr===0.U && io.tlbHit), "Error: queue is empty but tlbHit is true!")

  val tailAfterRealDeq = tailPtr - moveMask(tailPtr.tail(1))
  val isFull = tailAfterRealDeq.head(1).asBool() // tailPtr===qsize.U

  // enq
  io.enq.ready := !isFull && !io.replay.valid && !io.redirect.valid
  when(io.enq.fire()){
    stateQueue(tailAfterRealDeq.tail(1)) := s_valid
    uopQueue(idxQueue(tailPtr.tail(1))) := io.enq.bits
  }

  tailPtr := tailAfterRealDeq + io.enq.fire()

  XSDebug(
    realDeqValid,
    p"firstWait:$firstWait firstBubble:$firstBubble realDeqIdx:$realDeqIdx\n"
  )

  XSDebug("State Dump: ")
  stateQueue.reverse.foreach(s =>{
    XSDebug(false, s===s_invalid, " -")
    XSDebug(false, s===s_valid, " v")
    XSDebug(false, s===s_wait, " w")
    XSDebug(false, s===s_replay, " r")
  })
  XSDebug(false, true.B, "\n")

  XSDebug("State Dump: ")
  idxQueue.reverse.foreach(id =>{
    XSDebug(false, true.B, p"$id")
  })
  XSDebug(false, true.B, "\n")

  assert(!(io.replay.valid && realDeqValid), "Error: realDeqValid should be false when replay valid!")
  for(i <- 0 until qsize){
    val uopQIdx = idxQueue(i)
    val cnt = cntQueue(uopQIdx)
    val nextIdx = i.U - moveMask(i)
    when(
      (io.replay.valid && stateQueue(i)===s_wait) &&
        uopQueue(uopQIdx).isAfter(io.replay.bits)
    ){
      // 'i' is enough because 'realDeqValid' must be false here
      stateQueue(i) := s_replay
      cnt := (replayDelay-1).U
    }
    when(stateQueue(i)===s_replay){
      when(cnt === 0.U) {
        stateQueue(nextIdx) := s_valid
        if(i == 0) {
          assert(!moveMask(0), "Error: Attemp to delete a 's_replay' entry!")
        }
      }.otherwise({
        cnt := cnt - 1.U
      })
    }
    when(uopQueue(uopQIdx).needFlush(io.redirect)){
      stateQueue(nextIdx) := s_invalid
    }
  }


  // Debug sigs
  XSInfo(
    io.enq.fire(),
    p"enq fire: pc:${Hexadecimal(io.enq.bits.cf.pc)} roqIdx:${io.enq.bits.roqIdx}\n"
  )
  XSInfo(
    io.deq.fire(),
    p"deq fire: pc:${Hexadecimal(io.deq.bits.uop.cf.pc)} roqIdx:${io.deq.bits.uop.roqIdx}\n"
  )
  XSDebug(p"tailPtr:$tailPtr tailAfterDeq:$tailAfterRealDeq tlbHit:${io.tlbHit}\n")

  XSDebug(false, true.B, "\n")
}
