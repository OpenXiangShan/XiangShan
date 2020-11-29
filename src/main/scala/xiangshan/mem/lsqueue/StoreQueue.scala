package xiangshan.mem

import chisel3._
import chisel3.util._
import utils._
import xiangshan._
import xiangshan.cache._
import xiangshan.cache.{DCacheWordIO, DCacheLineIO, TlbRequestIO, MemoryOpConstants}
import xiangshan.backend.LSUOpType
import xiangshan.backend.roq.RoqPtr


class SqPtr extends CircularQueuePtr(SqPtr.StoreQueueSize) { }

object SqPtr extends HasXSParameter {
  def apply(f: Bool, v: UInt): SqPtr = {
    val ptr = Wire(new SqPtr)
    ptr.flag := f
    ptr.value := v
    ptr
  }
}

// Store Queue
class StoreQueue extends XSModule with HasDCacheParameters with HasCircularQueuePtrHelper {
  val io = IO(new Bundle() {
    val enq = new Bundle() {
      val canAccept = Output(Bool())
      val req = Vec(RenameWidth, Flipped(ValidIO(new MicroOp)))
      val resp = Vec(RenameWidth, Output(new SqPtr))
    }
    val brqRedirect = Input(Valid(new Redirect))
    val storeIn = Vec(StorePipelineWidth, Flipped(Valid(new LsPipelineBundle)))
    val sbuffer = Vec(StorePipelineWidth, Decoupled(new DCacheWordReq))
    val stout = Vec(2, DecoupledIO(new ExuOutput)) // writeback store
    val forward = Vec(LoadPipelineWidth, Flipped(new LoadForwardQueryIO))
    val commits = Flipped(Vec(CommitWidth, Valid(new RoqCommit)))
    val uncache = new DCacheWordIO
    val roqDeqPtr = Input(new RoqPtr)
    // val refill = Flipped(Valid(new DCacheLineReq ))
    val oldestStore = Output(Valid(new RoqPtr))
    val exceptionAddr = new ExceptionAddrIO
  })
  
  val uop = Reg(Vec(StoreQueueSize, new MicroOp))
  // val data = Reg(Vec(StoreQueueSize, new LsqEntry))
  val dataModule = Module(new LSQueueData(StoreQueueSize, StorePipelineWidth))
  dataModule.io := DontCare 
  val allocated = RegInit(VecInit(List.fill(StoreQueueSize)(false.B))) // sq entry has been allocated
  val datavalid = RegInit(VecInit(List.fill(StoreQueueSize)(false.B))) // non-mmio data is valid
  val writebacked = RegInit(VecInit(List.fill(StoreQueueSize)(false.B))) // inst has been writebacked to CDB
  val commited = Reg(Vec(StoreQueueSize, Bool())) // inst has been commited by roq
  val pending = Reg(Vec(StoreQueueSize, Bool())) // mmio pending: inst is an mmio inst, it will not be executed until it reachs the end of roq
  
  val ringBufferHeadExtended = RegInit(0.U.asTypeOf(new SqPtr))
  val ringBufferTailExtended = RegInit(0.U.asTypeOf(new SqPtr))
  val ringBufferHead = ringBufferHeadExtended.value
  val ringBufferTail = ringBufferTailExtended.value
  val ringBufferSameFlag = ringBufferHeadExtended.flag === ringBufferTailExtended.flag
  val ringBufferEmpty = ringBufferHead === ringBufferTail && ringBufferSameFlag
  val ringBufferFull = ringBufferHead === ringBufferTail && !ringBufferSameFlag
  val ringBufferAllowin = !ringBufferFull
  
  val storeCommit = (0 until CommitWidth).map(i => io.commits(i).valid && !io.commits(i).bits.isWalk && io.commits(i).bits.uop.ctrl.commitType === CommitType.STORE)
  val mcommitIdx = (0 until CommitWidth).map(i => io.commits(i).bits.uop.sqIdx.value)

  val tailMask = (((1.U((StoreQueueSize + 1).W)) << ringBufferTail).asUInt - 1.U)(StoreQueueSize - 1, 0)
  val headMask = (((1.U((StoreQueueSize + 1).W)) << ringBufferHead).asUInt - 1.U)(StoreQueueSize - 1, 0)
  val enqDeqMask1 = tailMask ^ headMask
  val enqDeqMask = Mux(ringBufferSameFlag, enqDeqMask1, ~enqDeqMask1)

  // Enqueue at dispatch
  val validEntries = distanceBetween(ringBufferHeadExtended, ringBufferTailExtended)
  val firedDispatch = io.enq.req.map(_.valid)
  io.enq.canAccept := validEntries <= (LoadQueueSize - RenameWidth).U
  XSDebug(p"(ready, valid): ${io.enq.canAccept}, ${Binary(Cat(firedDispatch))}\n")
  for (i <- 0 until RenameWidth) {
    val offset = if (i == 0) 0.U else PopCount((0 until i).map(firedDispatch(_)))
    val sqIdx = ringBufferHeadExtended + offset
    val index = sqIdx.value
    when(io.enq.req(i).valid) {
      uop(index) := io.enq.req(i).bits
      allocated(index) := true.B
      datavalid(index) := false.B
      writebacked(index) := false.B
      commited(index) := false.B
      pending(index) := false.B
    }
    io.enq.resp(i) := sqIdx

    XSError(!io.enq.canAccept && io.enq.req(i).valid, "should not valid when not ready\n")
  }

  when(Cat(firedDispatch).orR) {
    ringBufferHeadExtended := ringBufferHeadExtended + PopCount(firedDispatch)
    XSInfo("dispatched %d insts to sq\n", PopCount(firedDispatch))
  }
    
  // writeback store
  (0 until StorePipelineWidth).map(i => {
    dataModule.io.wb(i).wen := false.B
    when(io.storeIn(i).fire()) {
      val stWbIndex = io.storeIn(i).bits.uop.sqIdx.value
      val hasException = io.storeIn(i).bits.uop.cf.exceptionVec.asUInt.orR
      datavalid(stWbIndex) := !io.storeIn(i).bits.mmio || hasException
      pending(stWbIndex) := io.storeIn(i).bits.mmio && !hasException

      val storeWbData = Wire(new LsqEntry)
      storeWbData := DontCare
      storeWbData.paddr := io.storeIn(i).bits.paddr
      storeWbData.vaddr := io.storeIn(i).bits.vaddr
      storeWbData.mask := io.storeIn(i).bits.mask
      storeWbData.data := io.storeIn(i).bits.data
      storeWbData.mmio := io.storeIn(i).bits.mmio
      storeWbData.exception := io.storeIn(i).bits.uop.cf.exceptionVec.asUInt

      dataModule.io.wbWrite(i, stWbIndex, storeWbData)
      dataModule.io.wb(i).wen := true.B

      XSInfo("store write to sq idx %d pc 0x%x vaddr %x paddr %x data %x mmio %x roll %x exc %x\n",
        io.storeIn(i).bits.uop.sqIdx.value,
        io.storeIn(i).bits.uop.cf.pc,
        io.storeIn(i).bits.vaddr,
        io.storeIn(i).bits.paddr,
        io.storeIn(i).bits.data,
        io.storeIn(i).bits.mmio,
        io.storeIn(i).bits.rollback,
        io.storeIn(i).bits.uop.cf.exceptionVec.asUInt
        )
    }
  })

  def getFirstOne(mask: Vec[Bool], startMask: UInt) = {
    val length = mask.length
    val highBits = (0 until length).map(i => mask(i) & ~startMask(i))
    val highBitsUint = Cat(highBits.reverse)
    PriorityEncoder(Mux(highBitsUint.orR(), highBitsUint, mask.asUInt))
  }

  def getFirstOneWithFlag(mask: Vec[Bool], startMask: UInt, startFlag: Bool) = {
    val length = mask.length
    val highBits = (0 until length).map(i => mask(i) & ~startMask(i))
    val highBitsUint = Cat(highBits.reverse)
    val changeDirection = !highBitsUint.orR()
    val index = PriorityEncoder(Mux(!changeDirection, highBitsUint, mask.asUInt))
    SqPtr(startFlag ^ changeDirection, index)
  }

  def selectFirstTwo(valid: Vec[Bool], startMask: UInt) = {
    val selVec = Wire(Vec(2, UInt(log2Up(StoreQueueSize).W)))
    val selValid = Wire(Vec(2, Bool()))
    selVec(0) := getFirstOne(valid, startMask)
    val firstSelMask = UIntToOH(selVec(0))
    val secondSelVec = VecInit((0 until valid.length).map(i => valid(i) && !firstSelMask(i)))
    selVec(1) := getFirstOne(secondSelVec, startMask)
    selValid(0) := Cat(valid).orR
    selValid(1) := Cat(secondSelVec).orR
    (selValid, selVec)
  }

  def selectFirstTwoRoughly(valid: Vec[Bool]) = {
    // TODO: do not select according to seq, just select 2 valid bit randomly
    val firstSelVec = valid
    val notFirstVec = Wire(Vec(valid.length, Bool()))
    (0 until valid.length).map(i => 
      notFirstVec(i) := (if(i != 0) { valid(i) || !notFirstVec(i) } else { false.B })
    )
    val secondSelVec = VecInit((0 until valid.length).map(i => valid(i) && !notFirstVec(i)))

    val selVec = Wire(Vec(2, UInt(log2Up(valid.length).W)))
    val selValid = Wire(Vec(2, Bool()))
    selVec(0) := PriorityEncoder(firstSelVec)
    selVec(1) := PriorityEncoder(secondSelVec)
    selValid(0) := Cat(firstSelVec).orR
    selValid(1) := Cat(secondSelVec).orR
    (selValid, selVec)
  }

  // select the last writebacked instruction
  val validStoreVec = VecInit((0 until StoreQueueSize).map(i => !(allocated(i) && datavalid(i))))
  val storeNotValid = SqPtr(false.B, getFirstOne(validStoreVec, tailMask))
  val storeValidIndex = (storeNotValid - 1.U).value
  io.oldestStore.valid := allocated(ringBufferTailExtended.value) && datavalid(ringBufferTailExtended.value) && !commited(storeValidIndex)
  io.oldestStore.bits := uop(storeValidIndex).roqIdx

  // writeback up to 2 store insts to CDB
  // choose the first two valid store requests from deqPtr
  val storeWbSelVec = VecInit((0 until StoreQueueSize).map(i => allocated(i) && datavalid(i) && !writebacked(i)))
  val (storeWbValid, storeWbSel) = selectFirstTwo(storeWbSelVec, tailMask)

  (0 until StorePipelineWidth).map(i => {
    io.stout(i).bits.uop := uop(storeWbSel(i))
    io.stout(i).bits.uop.sqIdx := storeWbSel(i).asTypeOf(new SqPtr)
    io.stout(i).bits.uop.cf.exceptionVec := dataModule.io.rdata(storeWbSel(i)).exception.asBools
    io.stout(i).bits.data := dataModule.io.rdata(storeWbSel(i)).data
    io.stout(i).bits.redirectValid := false.B
    io.stout(i).bits.redirect := DontCare
    io.stout(i).bits.brUpdate := DontCare
    io.stout(i).bits.debug.isMMIO := dataModule.io.rdata(storeWbSel(i)).mmio
    io.stout(i).valid := storeWbSelVec(storeWbSel(i)) && storeWbValid(i)
    when(io.stout(i).fire()) {
      writebacked(storeWbSel(i)) := true.B
    }
    io.stout(i).bits.fflags := DontCare
  })

  // remove retired insts from sq, add retired store to sbuffer

  // move tailPtr
  // TailPtr slow recovery: recycle bubbles in store queue
  // allocatedMask: dequeuePtr can go to the next 1-bit
  val allocatedMask = VecInit((0 until StoreQueueSize).map(i => allocated(i) || !enqDeqMask(i)))
  // find the first one from deqPtr (ringBufferTail)
  val nextTail1 = getFirstOneWithFlag(allocatedMask, tailMask, ringBufferTailExtended.flag)
  val nextTail = Mux(Cat(allocatedMask).orR, nextTail1, ringBufferHeadExtended)
  ringBufferTailExtended := nextTail

  // TailPtr fast recovery
  val tailRecycle = VecInit(List(
    io.uncache.resp.fire() || io.sbuffer(0).fire(),
    io.sbuffer(1).fire()
  ))

  when(tailRecycle.asUInt.orR){
    ringBufferTailExtended := ringBufferTailExtended + PopCount(tailRecycle.asUInt)
  }

  // load forward query
  // check over all lq entries and forward data from the first matched store
  (0 until LoadPipelineWidth).map(i => {
    io.forward(i).forwardMask := 0.U(8.W).asBools
    io.forward(i).forwardData := DontCare

    // Compare ringBufferTail (deqPtr) and forward.sqIdx, we have two cases:
    // (1) if they have the same flag, we need to check range(tail, sqIdx)
    // (2) if they have different flags, we need to check range(tail, LoadQueueSize) and range(0, sqIdx)
    // Forward1: Mux(same_flag, range(tail, sqIdx), range(tail, LoadQueueSize))
    // Forward2: Mux(same_flag, 0.U,                   range(0, sqIdx)    )
    // i.e. forward1 is the target entries with the same flag bits and forward2 otherwise

    val differentFlag = ringBufferTailExtended.flag =/= io.forward(i).sqIdx.flag
    val forwardMask = ((1.U((StoreQueueSize + 1).W)) << io.forward(i).sqIdx.value).asUInt - 1.U
    val storeWritebackedVec = WireInit(VecInit(Seq.fill(StoreQueueSize)(false.B))) 
    for (j <- 0 until StoreQueueSize) {
      storeWritebackedVec(j) := datavalid(j) && allocated(j) // all datavalid terms need to be checked
    }
    val needForward1 = Mux(differentFlag, ~tailMask, tailMask ^ forwardMask) & storeWritebackedVec.asUInt
    val needForward2 = Mux(differentFlag, forwardMask, 0.U(StoreQueueSize.W)) & storeWritebackedVec.asUInt

    XSDebug("" + i + " f1 %b f2 %b sqIdx %d pa %x\n", needForward1, needForward2, io.forward(i).sqIdx.asUInt, io.forward(i).paddr)

    // do real fwd query
    dataModule.io.forwardQuery(
      channel = i,
      paddr = io.forward(i).paddr, 
      needForward1 = needForward1,
      needForward2 = needForward2
    )

    io.forward(i).forwardMask := dataModule.io.forward(i).forwardMask
    io.forward(i).forwardData := dataModule.io.forward(i).forwardData
  })

  // When store commited, mark it as commited (will not be influenced by redirect),
  (0 until CommitWidth).map(i => {
    when(storeCommit(i)) {
      commited(mcommitIdx(i)) := true.B
      XSDebug("store commit %d: idx %d %x\n", i.U, mcommitIdx(i), uop(mcommitIdx(i)).cf.pc)
    }
  })

  (0 until 2).map(i => {
    val ptr = (ringBufferTailExtended + i.U).value
    val mmio = dataModule.io.rdata(ptr).mmio
    io.sbuffer(i).valid := allocated(ptr) && commited(ptr) && !mmio
    io.sbuffer(i).bits.cmd  := MemoryOpConstants.M_XWR
    io.sbuffer(i).bits.addr := dataModule.io.rdata(ptr).paddr
    io.sbuffer(i).bits.data := dataModule.io.rdata(ptr).data
    io.sbuffer(i).bits.mask := dataModule.io.rdata(ptr).mask
    io.sbuffer(i).bits.meta          := DontCare
    io.sbuffer(i).bits.meta.tlb_miss := false.B
    io.sbuffer(i).bits.meta.uop      := DontCare
    io.sbuffer(i).bits.meta.mmio     := mmio
    io.sbuffer(i).bits.meta.mask     := dataModule.io.rdata(ptr).mask

    when(io.sbuffer(i).fire()) {
      allocated(ptr) := false.B
      XSDebug("sbuffer "+i+" fire: ptr %d\n", ptr)
    }
  })

  // Memory mapped IO / other uncached operations
  
  // setup misc mem access req
  // mask / paddr / data can be get from sq.data
  val commitType = io.commits(0).bits.uop.ctrl.commitType 
  io.uncache.req.valid := pending(ringBufferTail) && allocated(ringBufferTail) &&
    commitType === CommitType.STORE &&
    io.roqDeqPtr === uop(ringBufferTail).roqIdx &&
    !io.commits(0).bits.isWalk

  io.uncache.req.bits.cmd  := MemoryOpConstants.M_XWR
  io.uncache.req.bits.addr := dataModule.io.rdata(ringBufferTail).paddr 
  io.uncache.req.bits.data := dataModule.io.rdata(ringBufferTail).data
  io.uncache.req.bits.mask := dataModule.io.rdata(ringBufferTail).mask
  
  io.uncache.req.bits.meta.id       := DontCare // TODO: // FIXME
  io.uncache.req.bits.meta.vaddr    := DontCare
  io.uncache.req.bits.meta.paddr    := dataModule.io.rdata(ringBufferTail).paddr
  io.uncache.req.bits.meta.uop      := uop(ringBufferTail)
  io.uncache.req.bits.meta.mmio     := true.B // dataModule.io.rdata(ringBufferTail).mmio
  io.uncache.req.bits.meta.tlb_miss := false.B
  io.uncache.req.bits.meta.mask     := dataModule.io.rdata(ringBufferTail).mask
  io.uncache.req.bits.meta.replay   := false.B
  
  io.uncache.resp.ready := true.B
  
  when(io.uncache.req.fire()){
    pending(ringBufferTail) := false.B
  }
  
  when(io.uncache.resp.fire()){
    datavalid(ringBufferTail) := true.B // will be writeback to CDB in the next cycle
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

  // Read vaddr for mem exception
  io.exceptionAddr.vaddr := dataModule.io.rdata(io.exceptionAddr.lsIdx.sqIdx.value).vaddr

  // misprediction recovery / exception redirect
  // invalidate sq term using robIdx
  val needCancel = Wire(Vec(StoreQueueSize, Bool()))
  for (i <- 0 until StoreQueueSize) {
    needCancel(i) := uop(i).roqIdx.needFlush(io.brqRedirect) && allocated(i) && !commited(i)
    when(needCancel(i)) {
      when(io.brqRedirect.bits.isReplay){
        datavalid(i) := false.B
        writebacked(i) := false.B
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
  XSDebug("head %d:%d tail %d:%d\n", ringBufferHeadExtended.flag, ringBufferHead, ringBufferTailExtended.flag, ringBufferTail)

  def PrintFlag(flag: Bool, name: String): Unit = {
    when(flag) {
      XSDebug(false, true.B, name)
    }.otherwise {
      XSDebug(false, true.B, " ")
    }
  }

  for (i <- 0 until StoreQueueSize) {
    if (i % 4 == 0) XSDebug("")
    XSDebug(false, true.B, "%x [%x] ", uop(i).cf.pc, dataModule.io.rdata(i).paddr)
    PrintFlag(allocated(i), "a")
    PrintFlag(allocated(i) && datavalid(i), "v")
    PrintFlag(allocated(i) && writebacked(i), "w")
    PrintFlag(allocated(i) && commited(i), "c")
    PrintFlag(allocated(i) && pending(i), "p")
    XSDebug(false, true.B, " ")
    if (i % 4 == 3 || i == StoreQueueSize - 1) XSDebug(false, true.B, "\n")
  }

}
