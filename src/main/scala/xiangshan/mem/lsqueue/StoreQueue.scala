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
    val mmioStout = DecoupledIO(new ExuOutput) // writeback uncached store
    val forward = Vec(LoadPipelineWidth, Flipped(new LoadForwardQueryIO))
    val commits = Flipped(new RoqCommitIO)
    val uncache = new DCacheWordIO
    val roqDeqPtr = Input(new RoqPtr)
    // val refill = Flipped(Valid(new DCacheLineReq ))
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

  val enqPtrExt = RegInit(0.U.asTypeOf(new SqPtr))
  val deqPtrExt = RegInit(0.U.asTypeOf(new SqPtr))
  val enqPtr = enqPtrExt.value
  val deqPtr = deqPtrExt.value
  val sameFlag = enqPtrExt.flag === deqPtrExt.flag
  val isEmpty = enqPtr === deqPtr && sameFlag
  val isFull = enqPtr === deqPtr && !sameFlag
  val allowIn = !isFull

  val storeCommit = (0 until CommitWidth).map(i => io.commits.valid(i) && !io.commits.isWalk && io.commits.uop(i).ctrl.commitType === CommitType.STORE)
  val mcommitIdx = (0 until CommitWidth).map(i => io.commits.uop(i).sqIdx.value)

  val tailMask = UIntToMask(deqPtr, StoreQueueSize)
  val headMask = UIntToMask(enqPtr, StoreQueueSize)
  val enqDeqMask1 = tailMask ^ headMask
  val enqDeqMask = Mux(sameFlag, enqDeqMask1, ~enqDeqMask1)

  // Enqueue at dispatch
  val validEntries = distanceBetween(enqPtrExt, deqPtrExt)
  val firedDispatch = io.enq.req.map(_.valid)
  io.enq.canAccept := validEntries <= (StoreQueueSize - RenameWidth).U
  XSDebug(p"(ready, valid): ${io.enq.canAccept}, ${Binary(Cat(firedDispatch))}\n")
  for (i <- 0 until RenameWidth) {
    val offset = if (i == 0) 0.U else PopCount((0 until i).map(firedDispatch(_)))
    val sqIdx = enqPtrExt + offset
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
    enqPtrExt := enqPtrExt + PopCount(firedDispatch)
    XSInfo("dispatched %d insts to sq\n", PopCount(firedDispatch))
  }

  // writeback store
  (0 until StorePipelineWidth).map(i => {
    dataModule.io.wb(i).wen := false.B
    when(io.storeIn(i).fire()) {
      val stWbIndex = io.storeIn(i).bits.uop.sqIdx.value
      val hasException = io.storeIn(i).bits.uop.cf.exceptionVec.asUInt.orR
      val hasWritebacked = !io.storeIn(i).bits.mmio || hasException
      datavalid(stWbIndex) := hasWritebacked
      writebacked(stWbIndex) := hasWritebacked
      pending(stWbIndex) := !hasWritebacked // valid mmio require

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

  // writeback finished mmio store
  io.mmioStout.bits.uop := uop(deqPtr)
  io.mmioStout.bits.uop.sqIdx := deqPtrExt
  io.mmioStout.bits.uop.cf.exceptionVec := dataModule.io.rdata(deqPtr).exception.asBools
  io.mmioStout.bits.data := dataModule.io.rdata(deqPtr).data
  io.mmioStout.bits.redirectValid := false.B
  io.mmioStout.bits.redirect := DontCare
  io.mmioStout.bits.brUpdate := DontCare
  io.mmioStout.bits.debug.isMMIO := true.B
  io.mmioStout.bits.fflags := DontCare
  io.mmioStout.valid := allocated(deqPtr) && datavalid(deqPtr) && !writebacked(deqPtr) // finished mmio store
  when(io.mmioStout.fire()) {
    writebacked(deqPtr) := true.B
    allocated(deqPtr) := false.B // potential opt: move deqPtr immediately
    deqPtrExt := deqPtrExt + 1.U
  }

  // remove retired insts from sq, add retired store to sbuffer
  when(Cat(io.sbuffer.map(_.fire())).orR) {
    deqPtrExt := deqPtrExt + Mux(io.sbuffer(1).fire(), 2.U, 1.U)
    when (io.sbuffer(1).fire()) {
      assert(io.sbuffer(0).fire())
    }
  }

  // load forward query
  // check over all lq entries and forward data from the first matched store
  (0 until LoadPipelineWidth).map(i => {
    io.forward(i).forwardMask := 0.U(8.W).asBools
    io.forward(i).forwardData := DontCare

    // Compare deqPtr (deqPtr) and forward.sqIdx, we have two cases:
    // (1) if they have the same flag, we need to check range(tail, sqIdx)
    // (2) if they have different flags, we need to check range(tail, LoadQueueSize) and range(0, sqIdx)
    // Forward1: Mux(same_flag, range(tail, sqIdx), range(tail, LoadQueueSize))
    // Forward2: Mux(same_flag, 0.U,                   range(0, sqIdx)    )
    // i.e. forward1 is the target entries with the same flag bits and forward2 otherwise

    val differentFlag = deqPtrExt.flag =/= io.forward(i).sqIdx.flag
    val forwardMask = UIntToMask(io.forward(i).sqIdx.value, StoreQueueSize)
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
    val ptr = (deqPtrExt + i.U).value
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
  val commitType = io.commits.uop(0).ctrl.commitType
  io.uncache.req.valid := pending(deqPtr) && allocated(deqPtr) &&
    commitType === CommitType.STORE &&
    io.roqDeqPtr === uop(deqPtr).roqIdx &&
    !io.commits.isWalk

  io.uncache.req.bits.cmd  := MemoryOpConstants.M_XWR
  io.uncache.req.bits.addr := dataModule.io.rdata(deqPtr).paddr
  io.uncache.req.bits.data := dataModule.io.rdata(deqPtr).data
  io.uncache.req.bits.mask := dataModule.io.rdata(deqPtr).mask

  io.uncache.req.bits.meta.id       := DontCare // TODO: // FIXME
  io.uncache.req.bits.meta.vaddr    := DontCare
  io.uncache.req.bits.meta.paddr    := dataModule.io.rdata(deqPtr).paddr
  io.uncache.req.bits.meta.uop      := uop(deqPtr)
  io.uncache.req.bits.meta.mmio     := true.B // dataModule.io.rdata(deqPtr).mmio
  io.uncache.req.bits.meta.tlb_miss := false.B
  io.uncache.req.bits.meta.mask     := dataModule.io.rdata(deqPtr).mask
  io.uncache.req.bits.meta.replay   := false.B

  io.uncache.resp.ready := true.B

  when(io.uncache.req.fire()){
    pending(deqPtr) := false.B
  }

  when(io.uncache.resp.fire()){
    datavalid(deqPtr) := true.B // will be writeback to CDB in the next cycle
    // TODO: write back exception info
  }

  when(io.uncache.req.fire()){
    XSDebug("uncache req: pc %x addr %x data %x op %x mask %x\n",
      uop(deqPtr).cf.pc,
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
        allocated(i) := false.B
    }
  }
  val lastCycleRedirectValid = RegNext(io.brqRedirect.valid)
  val needCancelReg = RegNext(needCancel)
  when (io.brqRedirect.valid) {
    enqPtrExt := enqPtrExt
  }
  when (lastCycleRedirectValid) {
    enqPtrExt := enqPtrExt - PopCount(needCancelReg)
  }

  // debug info
  XSDebug("enqPtrExt %d:%d deqPtrExt %d:%d\n", enqPtrExt.flag, enqPtr, deqPtrExt.flag, deqPtr)

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
