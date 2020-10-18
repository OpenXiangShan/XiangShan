package xiangshan.mem

import chisel3._
import chisel3.util._
import utils._
import xiangshan._
import xiangshan.cache._
import xiangshan.cache.{DCacheWordIO, DCacheLineIO, TlbRequestIO, MemoryOpConstants}
import xiangshan.backend.LSUOpType

// Store Queue
class StoreQueue extends XSModule with HasDCacheParameters with NeedImpl {
  val io = IO(new Bundle() {
    val dp1Req = Vec(RenameWidth, Flipped(DecoupledIO(new MicroOp)))
    val lsroqIdxs = Output(Vec(RenameWidth, UInt(LsroqIdxWidth.W)))
    val brqRedirect = Input(Valid(new Redirect))
    val storeIn = Vec(StorePipelineWidth, Flipped(Valid(new LsPipelineBundle)))
    val sbuffer = Vec(StorePipelineWidth, Decoupled(new DCacheWordReq))
    val ldout = Vec(2, DecoupledIO(new ExuOutput)) // writeback store
    val stout = Vec(2, DecoupledIO(new ExuOutput)) // writeback store
    val forward = Vec(LoadPipelineWidth, Flipped(new LoadForwardQueryIO))
    val commits = Flipped(Vec(CommitWidth, Valid(new RoqCommit)))
    val rollback = Output(Valid(new Redirect))
    val dcache = new DCacheLineIO
    val uncache = new DCacheWordIO
    val roqDeqPtr = Input(UInt(RoqIdxWidth.W))
    // val refill = Flipped(Valid(new DCacheLineReq ))
  })
  
  val uop = Reg(Vec(LsroqSize, new MicroOp))
  val data = Reg(Vec(LsroqSize, new LsRoqEntry))
  val allocated = RegInit(VecInit(List.fill(LsroqSize)(false.B))) // lsroq entry has been allocated
  val valid = RegInit(VecInit(List.fill(LsroqSize)(false.B))) // data is valid
  val writebacked = RegInit(VecInit(List.fill(LsroqSize)(false.B))) // inst has been writebacked to CDB
  val commited = Reg(Vec(LsroqSize, Bool())) // inst has been writebacked to CDB
  val miss = Reg(Vec(LsroqSize, Bool())) // load inst missed, waiting for miss queue to accept miss request
  val listening = Reg(Vec(LsroqSize, Bool())) // waiting for refill result
  val pending = Reg(Vec(LsroqSize, Bool())) // mmio pending: inst is an mmio inst, it will not be executed until it reachs the end of roq
  
  val ringBufferHeadExtended = RegInit(0.U(LsroqIdxWidth.W))
  val ringBufferTailExtended = RegInit(0.U(LsroqIdxWidth.W))
  val ringBufferHead = ringBufferHeadExtended(InnerLsroqIdxWidth - 1, 0)
  val ringBufferTail = ringBufferTailExtended(InnerLsroqIdxWidth - 1, 0)
  val ringBufferSameFlag = ringBufferHeadExtended(InnerLsroqIdxWidth) === ringBufferTailExtended(InnerLsroqIdxWidth)
  val ringBufferEmpty = ringBufferHead === ringBufferTail && ringBufferSameFlag
  val ringBufferFull = ringBufferHead === ringBufferTail && !ringBufferSameFlag
  val ringBufferAllowin = !ringBufferFull
  
  val storeCommit = (0 until CommitWidth).map(i => io.commits(i).valid && !io.commits(i).bits.isWalk && io.commits(i).bits.uop.ctrl.commitType === CommitType.STORE)
  val mcommitIdx = (0 until CommitWidth).map(i => io.commits(i).bits.uop.lsroqIdx(InnerLsroqIdxWidth-1,0))

  val tailMask = (((1.U((LsroqSize + 1).W)) << ringBufferTail).asUInt - 1.U)(LsroqSize - 1, 0)
  val headMask = (((1.U((LsroqSize + 1).W)) << ringBufferHead).asUInt - 1.U)(LsroqSize - 1, 0)
  val enqDeqMask1 = tailMask ^ headMask
  val enqDeqMask = Mux(ringBufferSameFlag, enqDeqMask1, ~enqDeqMask1)

  // TODO: misc arbitor

  // Enqueue at dispatch
  val validDispatch = VecInit((0 until RenameWidth).map(io.dp1Req(_).valid)).asUInt
  XSDebug("(ready, valid): ")
  for (i <- 0 until RenameWidth) {
    val offset = if (i == 0) 0.U else PopCount(validDispatch(i - 1, 0))
    val lsroqIdx = ringBufferHeadExtended + offset
    val index = lsroqIdx(InnerLsroqIdxWidth - 1, 0)
    when(io.dp1Req(i).fire()) {
      uop(index) := io.dp1Req(i).bits
      allocated(index) := true.B
      valid(index) := false.B
      writebacked(index) := false.B
      commited(index) := false.B
      miss(index) := false.B
      listening(index) := false.B
      pending(index) := false.B
      // data(index).bwdMask := 0.U(8.W).asBools
    }
    if (i == 0) {
      io.dp1Req(i).ready := ringBufferAllowin && !allocated(index)
    } else {
      io.dp1Req(i).ready := ringBufferAllowin && !allocated(index) && io.dp1Req(i - 1).ready
    }
    io.lsroqIdxs(i) := lsroqIdx
    XSDebug(false, true.B, "(%d, %d) ", io.dp1Req(i).ready, io.dp1Req(i).valid)
  }
  XSDebug(false, true.B, "\n")

  val firedDispatch = VecInit((0 until CommitWidth).map(io.dp1Req(_).fire())).asUInt
  when(firedDispatch.orR) {
    ringBufferHeadExtended := ringBufferHeadExtended + PopCount(firedDispatch)
    XSInfo("dispatched %d insts to lsroq\n", PopCount(firedDispatch))
  }
    
    // writeback store
    (0 until StorePipelineWidth).map(i => {
      when(io.storeIn(i).fire()) {
        valid(io.storeIn(i).bits.uop.lsroqIdx) := !io.storeIn(i).bits.mmio
        data(io.storeIn(i).bits.uop.lsroqIdx).paddr := io.storeIn(i).bits.paddr
        data(io.storeIn(i).bits.uop.lsroqIdx).vaddr := io.storeIn(i).bits.vaddr
        data(io.storeIn(i).bits.uop.lsroqIdx).mask := io.storeIn(i).bits.mask
        data(io.storeIn(i).bits.uop.lsroqIdx).data := io.storeIn(i).bits.data
        data(io.storeIn(i).bits.uop.lsroqIdx).mmio := io.storeIn(i).bits.mmio
        data(io.storeIn(i).bits.uop.lsroqIdx).exception := io.storeIn(i).bits.uop.cf.exceptionVec.asUInt
        miss(io.storeIn(i).bits.uop.lsroqIdx) := io.storeIn(i).bits.miss
        store(io.storeIn(i).bits.uop.lsroqIdx) := true.B
        pending(io.storeIn(i).bits.uop.lsroqIdx) := io.storeIn(i).bits.mmio
        XSInfo("store write to lsroq idx %d pc 0x%x vaddr %x paddr %x data %x miss %x mmio %x roll %x exc %x\n",
        io.storeIn(i).bits.uop.lsroqIdx(InnerLsroqIdxWidth - 1, 0),
        io.storeIn(i).bits.uop.cf.pc,
        io.storeIn(i).bits.vaddr,
        io.storeIn(i).bits.paddr,
        io.storeIn(i).bits.data,
        io.storeIn(i).bits.miss,
        io.storeIn(i).bits.mmio,
        io.storeIn(i).bits.rollback,
        io.storeIn(i).bits.uop.cf.exceptionVec.asUInt
        )
      }
  })

  // writeback up to 2 store insts to CDB
  // choose the first two valid store requests from deqPtr
  val storeWbSelVec = VecInit((0 until LsroqSize).map(i => {
    allocated(i) && valid(i) && !writebacked(i) && store(i)
  }))
  val storeWbSel = Wire(Vec(StorePipelineWidth, UInt(log2Up(LsroqSize).W)))
  val storeWbValid = Wire(Vec(StorePipelineWidth, Bool()))
  storeWbSel(0) := getFirstOne(storeWbSelVec, tailMask)
  val firstSelMask = UIntToOH(storeWbSel(0))
  val secondWbSelVec = VecInit((0 until LsroqSize).map(i => storeWbSelVec(i) && !firstSelMask(i)))
  storeWbSel(1) := getFirstOne(secondWbSelVec, tailMask)
  storeWbValid(0) := Cat(storeWbSelVec).orR
  storeWbValid(1) := Cat(secondWbSelVec).orR

  (0 until StorePipelineWidth).map(i => {
    io.stout(i).bits.uop := uop(storeWbSel(i))
    io.stout(i).bits.uop.lsroqIdx := storeWbSel(i)
    io.stout(i).bits.uop.cf.exceptionVec := data(storeWbSel(i)).exception.asBools
    io.stout(i).bits.data := data(storeWbSel(i)).data
    io.stout(i).bits.redirectValid := false.B
    io.stout(i).bits.redirect := DontCare
    io.stout(i).bits.brUpdate := DontCare
    io.stout(i).bits.debug.isMMIO := data(storeWbSel(i)).mmio
    io.stout(i).valid := storeWbSelVec(storeWbSel(i)) && storeWbValid(i)
    when(io.stout(i).fire()) {
      writebacked(storeWbSel(i)) := true.B
    }
  })

  // remove retired insts from lsroq, add retired store to sbuffer

  // move tailPtr
  // allocatedMask: dequeuePtr can go to the next 1-bit
  val allocatedMask = VecInit((0 until LsroqSize).map(i => allocated(i) || !enqDeqMask(i)))
  // find the first one from deqPtr (ringBufferTail)
  val nextTail1 = getFirstOneWithFlag(allocatedMask, tailMask, ringBufferTailExtended(InnerLsroqIdxWidth))
  val nextTail = Mux(Cat(allocatedMask).orR, nextTail1, ringBufferHeadExtended)
  ringBufferTailExtended := nextTail

  // TODO: FIXME: commitedStoreQueue is not necessary
  // send commited store inst to sbuffer
  // select up to 2 writebacked store insts
  // scommitPending, scommitIn, scommitOut are for debug only
  val commitedStoreQueue = Module(new MIMOQueue(
    UInt(InnerLsroqIdxWidth.W),
    entries = LsroqSize,
    inCnt = 6,
    outCnt = 2,
    mem = false,
    perf = true
  ))

  // // scommit counter for debugging
  // val scommitPending = RegInit(0.U(log2Up(LsroqSize).W))
  // val scommitIn = PopCount(VecInit(storeCommit).asUInt)
  // val scommitOut = PopCount(VecInit((0 until 2).map(i => commitedStoreQueue.io.deq(i).fire())).asUInt)
  // scommitPending := scommitPending + scommitIn - scommitOut

  commitedStoreQueue.io.flush := false.B

  // When store commited, mark it as commited (will not be influenced by redirect),
  // then add store's lsroq ptr into commitedStoreQueue
  (0 until CommitWidth).map(i => {
    when(storeCommit(i)) {
      commited(mcommitIdx(i)) := true.B
      XSDebug("store commit %d: idx %d %x\n", i.U, mcommitIdx(i), uop(mcommitIdx(i)).cf.pc)
    }
    commitedStoreQueue.io.enq(i).valid := storeCommit(i)
    commitedStoreQueue.io.enq(i).bits := mcommitIdx(i)
    // We assume commitedStoreQueue.io.enq(i).ready === true.B,
    // for commitedStoreQueue.size = 64
  })

  // get no more than 2 commited store from storeCommitedQueue
  // send selected store inst to sbuffer
  (0 until 2).map(i => {
    val ptr = commitedStoreQueue.io.deq(i).bits
    val mmio = data(ptr).mmio
    io.sbuffer(i).valid := commitedStoreQueue.io.deq(i).valid && !mmio
    io.sbuffer(i).bits.cmd  := MemoryOpConstants.M_XWR
    io.sbuffer(i).bits.addr := data(ptr).paddr
    io.sbuffer(i).bits.data := data(ptr).data
    io.sbuffer(i).bits.mask := data(ptr).mask
    io.sbuffer(i).bits.meta          := DontCare
    io.sbuffer(i).bits.meta.tlb_miss := false.B
    io.sbuffer(i).bits.meta.uop      := uop(ptr)
    io.sbuffer(i).bits.meta.mmio     := mmio
    io.sbuffer(i).bits.meta.mask     := data(ptr).mask

    commitedStoreQueue.io.deq(i).ready := io.sbuffer(i).fire() || mmio

    // update lsroq meta if store inst is send to sbuffer
    when(commitedStoreQueue.io.deq(i).valid && (mmio || io.sbuffer(i).ready)) {
      allocated(commitedStoreQueue.io.deq(i).bits) := false.B
    }
  })

  // Memory mapped IO / other uncached operations

  // setup misc mem access req
  // mask / paddr / data can be get from lsroq.data
  val commitType = io.commits(0).bits.uop.ctrl.commitType 
  io.uncache.req.valid := pending(ringBufferTail) && allocated(ringBufferTail) &&
    commitType === CommitType.STORE && 
    io.roqDeqPtr === uop(ringBufferTail).roqIdx && 
    !io.commits(0).bits.isWalk

  io.uncache.req.bits.cmd  := MemoryOpConstants.M_XWR
  io.uncache.req.bits.addr := data(ringBufferTail).paddr 
  io.uncache.req.bits.data := data(ringBufferTail).data
  io.uncache.req.bits.mask := data(ringBufferTail).mask

  io.uncache.req.bits.meta.id       := DontCare // TODO: // FIXME
  io.uncache.req.bits.meta.vaddr    := DontCare
  io.uncache.req.bits.meta.paddr    := data(ringBufferTail).paddr
  io.uncache.req.bits.meta.uop      := uop(ringBufferTail)
  io.uncache.req.bits.meta.mmio     := true.B // data(ringBufferTail).mmio
  io.uncache.req.bits.meta.tlb_miss := false.B
  io.uncache.req.bits.meta.mask     := data(ringBufferTail).mask
  io.uncache.req.bits.meta.replay   := false.B

  io.uncache.resp.ready := true.B
  io.uncache.s1_kill := false.B

  when(io.uncache.req.fire()){
    pending(ringBufferTail) := false.B
  }

  when(io.uncache.resp.fire()){
    valid(ringBufferTail) := true.B
    data(ringBufferTail).data := io.uncache.resp.bits.data(XLEN-1, 0)
    // TODO: write back exception info
  }

  when(io.uncache.req.fire()){
    XSDebug("uncache req: pc %x addr %x data %x op %x mask %x\n",
      uop(ringBufferTail).cf.pc,
      io.uncache.req.bits.addr,
      io.uncache.req.bits.data,
      io.uncache.req.bits.cmd,
      io.uncache.req.bits.mask
    )
  }

  when(io.uncache.resp.fire()){
    XSDebug("uncache resp: data %x\n", io.dcache.resp.bits.data) 
  }

  // FIXME: index by lq/sq id
  // Read vaddr for mem exception
  val mexcLsroqIdx = WireInit(0.U(LsroqIdxWidth.W))
  val memExceptionAddr = WireInit(data(mexcLsroqIdx(InnerLsroqIdxWidth - 1, 0)).vaddr)
  ExcitingUtils.addSink(mexcLsroqIdx, "EXECPTION_LSROQIDX")
  ExcitingUtils.addSource(memExceptionAddr, "EXECPTION_STORE_VADDR")

  // misprediction recovery / exception redirect
  // invalidate lsroq term using robIdx
  val needCancel = Wire(Vec(LsroqSize, Bool()))
  for (i <- 0 until LsroqSize) {
    needCancel(i) := uop(i).needFlush(io.brqRedirect) && allocated(i) && !commited(i)
    when(needCancel(i)) {
      when(io.brqRedirect.bits.isReplay){
        valid(i) := false.B
        store(i) := false.B
        writebacked(i) := false.B
        listening(i) := false.B
        miss(i) := false.B
        pending(i) := false.B
      }.otherwise{
        allocated(i) := false.B
      }
    }
  }
  when (io.brqRedirect.valid && io.brqRedirect.bits.isMisPred) {
    ringBufferHeadExtended := ringBufferHeadExtended - PopCount(needCancel)
  }

  // debug info
  XSDebug("head %d:%d tail %d:%d scommit %d\n", ringBufferHeadExtended(InnerLsroqIdxWidth), ringBufferHead, ringBufferTailExtended(InnerLsroqIdxWidth), ringBufferTail, scommitPending)

  def PrintFlag(flag: Bool, name: String): Unit = {
    when(flag) {
      XSDebug(false, true.B, name)
    }.otherwise {
      XSDebug(false, true.B, " ")
    }
  }

  for (i <- 0 until LsroqSize) {
    if (i % 4 == 0) XSDebug("")
    XSDebug(false, true.B, "%x ", uop(i).cf.pc)
    PrintFlag(allocated(i), "a")
    PrintFlag(allocated(i) && valid(i), "v")
    PrintFlag(allocated(i) && writebacked(i), "w")
    PrintFlag(allocated(i) && commited(i), "c")
    PrintFlag(allocated(i) && store(i), "s")
    PrintFlag(allocated(i) && miss(i), "m")
    PrintFlag(allocated(i) && listening(i), "l")
    PrintFlag(allocated(i) && pending(i), "p")
    XSDebug(false, true.B, " ")
    if (i % 4 == 3) XSDebug(false, true.B, "\n")
  }

}
