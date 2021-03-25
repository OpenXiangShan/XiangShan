package xiangshan.mem

import chisel3._
import chisel3.util._
import utils._
import xiangshan._
import xiangshan.cache._
import xiangshan.cache.{DCacheWordIO, DCacheLineIO, TlbRequestIO, MemoryOpConstants}
import xiangshan.backend.LSUOpType
import xiangshan.backend.roq.RoqLsqIO


class SqPtr extends CircularQueuePtr(SqPtr.StoreQueueSize) { }

object SqPtr extends HasXSParameter {
  def apply(f: Bool, v: UInt): SqPtr = {
    val ptr = Wire(new SqPtr)
    ptr.flag := f
    ptr.value := v
    ptr
  }
}

class SqEnqIO extends XSBundle {
  val canAccept = Output(Bool())
  val lqCanAccept = Input(Bool())
  val needAlloc = Vec(RenameWidth, Input(Bool()))
  val req = Vec(RenameWidth, Flipped(ValidIO(new MicroOp)))
  val resp = Vec(RenameWidth, Output(new SqPtr))
}

// Store Queue
class StoreQueue extends XSModule with HasDCacheParameters with HasCircularQueuePtrHelper {
  val io = IO(new Bundle() {
    val enq = new SqEnqIO
    val brqRedirect = Flipped(ValidIO(new Redirect))
    val flush = Input(Bool())
    val storeIn = Vec(StorePipelineWidth, Flipped(Valid(new LsPipelineBundle)))
    val sbuffer = Vec(StorePipelineWidth, Decoupled(new DCacheWordReq))
    val mmioStout = DecoupledIO(new ExuOutput) // writeback uncached store
    val forward = Vec(LoadPipelineWidth, Flipped(new MaskedLoadForwardQueryIO))
    val roq = Flipped(new RoqLsqIO)
    val uncache = new DCacheWordIO
    // val refill = Flipped(Valid(new DCacheLineReq ))
    val exceptionAddr = new ExceptionAddrIO
    val sqempty = Output(Bool())
    val issuePtrExt = Output(new SqPtr)
    val storeIssue = Vec(StorePipelineWidth, Flipped(Valid(new ExuInput)))
  })

  val difftestIO = IO(new Bundle() {
    val storeCommit = Output(UInt(2.W))
    val storeAddr   = Output(Vec(2, UInt(64.W)))
    val storeData   = Output(Vec(2, UInt(64.W)))
    val storeMask   = Output(Vec(2, UInt(8.W)))
  })
  difftestIO <> DontCare

  // data modules
  val uop = Reg(Vec(StoreQueueSize, new MicroOp))
  // val data = Reg(Vec(StoreQueueSize, new LsqEntry))
  val dataModule = Module(new StoreQueueData(StoreQueueSize, numRead = StorePipelineWidth, numWrite = StorePipelineWidth, numForward = StorePipelineWidth))
  dataModule.io := DontCare
  val paddrModule = Module(new SQPaddrModule(StoreQueueSize, numRead = StorePipelineWidth, numWrite = StorePipelineWidth, numForward = StorePipelineWidth))
  paddrModule.io := DontCare
  val vaddrModule = Module(new SyncDataModuleTemplate(UInt(VAddrBits.W), StoreQueueSize, numRead = 1, numWrite = StorePipelineWidth))
  vaddrModule.io := DontCare

  // state & misc
  val allocated = RegInit(VecInit(List.fill(StoreQueueSize)(false.B))) // sq entry has been allocated
  val datavalid = RegInit(VecInit(List.fill(StoreQueueSize)(false.B))) // non-mmio data is valid
  val writebacked = RegInit(VecInit(List.fill(StoreQueueSize)(false.B))) // inst has been writebacked to CDB
  val issued = Reg(Vec(StoreQueueSize, Bool())) // inst has been issued by rs
  val commited = Reg(Vec(StoreQueueSize, Bool())) // inst has been commited by roq
  val pending = Reg(Vec(StoreQueueSize, Bool())) // mmio pending: inst is an mmio inst, it will not be executed until it reachs the end of roq
  val mmio = Reg(Vec(StoreQueueSize, Bool())) // mmio: inst is an mmio inst

  // ptr
  require(StoreQueueSize > RenameWidth)
  val enqPtrExt = RegInit(VecInit((0 until RenameWidth).map(_.U.asTypeOf(new SqPtr))))
  val deqPtrExt = RegInit(VecInit((0 until StorePipelineWidth).map(_.U.asTypeOf(new SqPtr))))
  val cmtPtrExt = RegInit(VecInit((0 until CommitWidth).map(_.U.asTypeOf(new SqPtr))))
  val issuePtrExt = RegInit(0.U.asTypeOf(new SqPtr))
  val validCounter = RegInit(0.U(log2Ceil(LoadQueueSize + 1).W))
  val allowEnqueue = RegInit(true.B)

  val enqPtr = enqPtrExt(0).value
  val deqPtr = deqPtrExt(0).value
  val cmtPtr = cmtPtrExt(0).value

  val deqMask = UIntToMask(deqPtr, StoreQueueSize)
  val enqMask = UIntToMask(enqPtr, StoreQueueSize)

  val commitCount = RegNext(io.roq.scommit)

  // Read dataModule
  // deqPtrExtNext and deqPtrExtNext+1 entry will be read from dataModule
  // if !sbuffer.fire(), read the same ptr
  // if sbuffer.fire(), read next
  val deqPtrExtNext = WireInit(Mux(io.sbuffer(1).fire(),
    VecInit(deqPtrExt.map(_ + 2.U)),
    Mux(io.sbuffer(0).fire() || io.mmioStout.fire(),
      VecInit(deqPtrExt.map(_ + 1.U)),
      deqPtrExt
    )
  ))
  for (i <- 0 until StorePipelineWidth) {
    dataModule.io.raddr(i) := deqPtrExtNext(i).value
    paddrModule.io.raddr(i) := deqPtrExtNext(i).value
  }

  // no inst will be commited 1 cycle before tval update
  vaddrModule.io.raddr(0) := (cmtPtrExt(0) + commitCount).value

  /**
    * Enqueue at dispatch
    *
    * Currently, StoreQueue only allows enqueue when #emptyEntries > RenameWidth(EnqWidth)
    */
  io.enq.canAccept := allowEnqueue
  for (i <- 0 until RenameWidth) {
    val offset = if (i == 0) 0.U else PopCount(io.enq.needAlloc.take(i))
    val sqIdx = enqPtrExt(offset)
    val index = sqIdx.value
    when (io.enq.req(i).valid && io.enq.canAccept && io.enq.lqCanAccept && !(io.brqRedirect.valid || io.flush)) {
      uop(index) := io.enq.req(i).bits
      allocated(index) := true.B
      datavalid(index) := false.B
      writebacked(index) := false.B
      issued(index) := false.B
      commited(index) := false.B
      pending(index) := false.B
    }
    io.enq.resp(i) := sqIdx
  }
  XSDebug(p"(ready, valid): ${io.enq.canAccept}, ${Binary(Cat(io.enq.req.map(_.valid)))}\n")

  /**
    * Update issuePtr when issue from rs
    */

  // update state bit issued
  for (i <- 0 until StorePipelineWidth) {
    when (io.storeIssue(i).valid) {
      issued(io.storeIssue(i).bits.uop.sqIdx.value) := true.B
    }
  }

  // update issuePtr
  val IssuePtrMoveStride = 4
  require(IssuePtrMoveStride >= 2)

  val issueLookupVec = (0 until IssuePtrMoveStride).map(issuePtrExt + _.U)
  val issueLookup = issueLookupVec.map(ptr => allocated(ptr.value) && issued(ptr.value) && ptr =/= enqPtrExt(0))
  val nextIssuePtr = issuePtrExt + PriorityEncoder(VecInit(issueLookup.map(!_) :+ true.B))
  issuePtrExt := nextIssuePtr

  when (io.brqRedirect.valid || io.flush) {
    issuePtrExt := Mux(
      isAfter(cmtPtrExt(0), deqPtrExt(0)),
      cmtPtrExt(0),
      deqPtrExtNext(0) // for mmio insts, deqPtr may be ahead of cmtPtr
    )
  }
  // send issuePtrExt to rs
  // io.issuePtrExt := cmtPtrExt(0)
  io.issuePtrExt := issuePtrExt

  /**
    * Writeback store from store units
    *
    * Most store instructions writeback to regfile in the previous cycle.
    * However,
    *   (1) For an mmio instruction with exceptions, we need to mark it as datavalid
    * (in this way it will trigger an exception when it reaches ROB's head)
    * instead of pending to avoid sending them to lower level.
    *   (2) For an mmio instruction without exceptions, we mark it as pending.
    * When the instruction reaches ROB's head, StoreQueue sends it to uncache channel.
    * Upon receiving the response, StoreQueue writes back the instruction
    * through arbiter with store units. It will later commit as normal.
    */
  for (i <- 0 until StorePipelineWidth) {
    dataModule.io.wen(i) := false.B
    paddrModule.io.wen(i) := false.B
    val stWbIndex = io.storeIn(i).bits.uop.sqIdx.value
    when (io.storeIn(i).fire()) {
      datavalid(stWbIndex) := !io.storeIn(i).bits.mmio
      writebacked(stWbIndex) := !io.storeIn(i).bits.mmio
      pending(stWbIndex) := io.storeIn(i).bits.mmio

      val storeWbData = Wire(new SQDataEntry)
      storeWbData := DontCare
      storeWbData.mask := io.storeIn(i).bits.mask
      storeWbData.data := io.storeIn(i).bits.data

      dataModule.io.waddr(i) := stWbIndex
      dataModule.io.wdata(i) := storeWbData
      dataModule.io.wen(i) := true.B

      paddrModule.io.waddr(i) := stWbIndex
      paddrModule.io.wdata(i) := io.storeIn(i).bits.paddr
      paddrModule.io.wen(i) := true.B


      mmio(stWbIndex) := io.storeIn(i).bits.mmio

      XSInfo("store write to sq idx %d pc 0x%x vaddr %x paddr %x data %x mmio %x\n",
        io.storeIn(i).bits.uop.sqIdx.value,
        io.storeIn(i).bits.uop.cf.pc,
        io.storeIn(i).bits.vaddr,
        io.storeIn(i).bits.paddr,
        io.storeIn(i).bits.data,
        io.storeIn(i).bits.mmio
        )
    }
    // vaddrModule write is delayed, as vaddrModule will not be read right after write
    vaddrModule.io.waddr(i) := RegNext(stWbIndex)
    vaddrModule.io.wdata(i) := RegNext(io.storeIn(i).bits.vaddr)
    vaddrModule.io.wen(i) := RegNext(io.storeIn(i).fire())
  }

  /**
    * load forward query
    *
    * Check store queue for instructions that is older than the load.
    * The response will be valid at the next cycle after req.
    */
  // check over all lq entries and forward data from the first matched store
  for (i <- 0 until LoadPipelineWidth) {
    io.forward(i).forwardMask := 0.U(8.W).asBools
    io.forward(i).forwardData := DontCare

    // Compare deqPtr (deqPtr) and forward.sqIdx, we have two cases:
    // (1) if they have the same flag, we need to check range(tail, sqIdx)
    // (2) if they have different flags, we need to check range(tail, LoadQueueSize) and range(0, sqIdx)
    // Forward1: Mux(same_flag, range(tail, sqIdx), range(tail, LoadQueueSize))
    // Forward2: Mux(same_flag, 0.U,                   range(0, sqIdx)    )
    // i.e. forward1 is the target entries with the same flag bits and forward2 otherwise
    val differentFlag = deqPtrExt(0).flag =/= io.forward(i).sqIdx.flag
    val forwardMask = io.forward(i).sqIdxMask
    val storeWritebackedVec = WireInit(VecInit(Seq.fill(StoreQueueSize)(false.B)))
    for (j <- 0 until StoreQueueSize) {
      storeWritebackedVec(j) := datavalid(j) && allocated(j) // all datavalid terms need to be checked
    }
    val needForward1 = Mux(differentFlag, ~deqMask, deqMask ^ forwardMask) & storeWritebackedVec.asUInt
    val needForward2 = Mux(differentFlag, forwardMask, 0.U(StoreQueueSize.W)) & storeWritebackedVec.asUInt

    XSDebug(p"$i f1 ${Binary(needForward1)} f2 ${Binary(needForward2)} " +
      p"sqIdx ${io.forward(i).sqIdx} pa ${Hexadecimal(io.forward(i).paddr)}\n"
    )

    // do real fwd query
    dataModule.io.needForward(i)(0) := needForward1 & paddrModule.io.forwardMmask(i).asUInt
    dataModule.io.needForward(i)(1) := needForward2 & paddrModule.io.forwardMmask(i).asUInt

    paddrModule.io.forwardMdata(i) := io.forward(i).paddr

    io.forward(i).forwardMask := dataModule.io.forwardMask(i)
    io.forward(i).forwardData := dataModule.io.forwardData(i)
  }

  /**
    * Memory mapped IO / other uncached operations
    *
    * States:
    * (1) writeback from store units: mark as pending
    * (2) when they reach ROB's head, they can be sent to uncache channel
    * (3) response from uncache channel: mark as datavalid
    * (4) writeback to ROB (and other units): mark as writebacked
    * (5) ROB commits the instruction: same as normal instructions
    */
  //(2) when they reach ROB's head, they can be sent to uncache channel
  val s_idle :: s_req :: s_resp :: s_wb :: s_wait :: Nil = Enum(5)
  val uncacheState = RegInit(s_idle)
  switch(uncacheState) {
    is(s_idle) {
      when(io.roq.pendingst && pending(deqPtr) && allocated(deqPtr)) {
        uncacheState := s_req
      }
    }
    is(s_req) {
      when(io.uncache.req.fire()) {
        uncacheState := s_resp
      }
    }
    is(s_resp) {
      when(io.uncache.resp.fire()) {
        uncacheState := s_wb
      }
    }
    is(s_wb) {
      when (io.mmioStout.fire()) {
        uncacheState := s_wait
      }
    }
    is(s_wait) {
      when(io.roq.commit) {
        uncacheState := s_idle // ready for next mmio
      }
    }
  }
  io.uncache.req.valid := uncacheState === s_req

  io.uncache.req.bits.cmd  := MemoryOpConstants.M_XWR
  io.uncache.req.bits.addr := paddrModule.io.rdata(0) // data(deqPtr) -> rdata(0)
  io.uncache.req.bits.data := dataModule.io.rdata(0).data
  io.uncache.req.bits.mask := dataModule.io.rdata(0).mask

  io.uncache.req.bits.id   := DontCare

  when(io.uncache.req.fire()){
    pending(deqPtr) := false.B

    XSDebug(
      p"uncache req: pc ${Hexadecimal(uop(deqPtr).cf.pc)} " +
      p"addr ${Hexadecimal(io.uncache.req.bits.addr)} " +
      p"data ${Hexadecimal(io.uncache.req.bits.data)} " +
      p"op ${Hexadecimal(io.uncache.req.bits.cmd)} " +
      p"mask ${Hexadecimal(io.uncache.req.bits.mask)}\n"
    )
  }

  // (3) response from uncache channel: mark as datavalid
  io.uncache.resp.ready := true.B
  when (io.uncache.resp.fire()) {
    datavalid(deqPtr) := true.B
  }

  // (4) writeback to ROB (and other units): mark as writebacked
  io.mmioStout.valid := uncacheState === s_wb // allocated(deqPtr) && datavalid(deqPtr) && !writebacked(deqPtr)
  io.mmioStout.bits.uop := uop(deqPtr)
  io.mmioStout.bits.uop.sqIdx := deqPtrExt(0)
  io.mmioStout.bits.data := dataModule.io.rdata(0).data // dataModule.io.rdata.read(deqPtr)
  io.mmioStout.bits.redirectValid := false.B
  io.mmioStout.bits.redirect := DontCare
  io.mmioStout.bits.debug.isMMIO := true.B
  io.mmioStout.bits.debug.paddr := DontCare
  io.mmioStout.bits.debug.isPerfCnt := false.B
  io.mmioStout.bits.fflags := DontCare
  when (io.mmioStout.fire()) {
    writebacked(deqPtr) := true.B
    allocated(deqPtr) := false.B
  }

  /**
    * ROB commits store instructions (mark them as commited)
    *
    * (1) When store commits, mark it as commited.
    * (2) They will not be cancelled and can be sent to lower level.
    */
  for (i <- 0 until CommitWidth) {
    when (commitCount > i.U) {
      commited(cmtPtrExt(i).value) := true.B
    }
  }
  cmtPtrExt := cmtPtrExt.map(_ + commitCount)

  // Commited stores will not be cancelled and can be sent to lower level.
  // remove retired insts from sq, add retired store to sbuffer
  for (i <- 0 until StorePipelineWidth) {
    // We use RegNext to prepare data for sbuffer
    val ptr = deqPtrExt(i).value
    // if !sbuffer.fire(), read the same ptr
    // if sbuffer.fire(), read next
    io.sbuffer(i).valid := allocated(ptr) && commited(ptr) && !mmio(ptr)
    io.sbuffer(i).bits.cmd  := MemoryOpConstants.M_XWR
    io.sbuffer(i).bits.addr := paddrModule.io.rdata(i)
    io.sbuffer(i).bits.data := dataModule.io.rdata(i).data
    io.sbuffer(i).bits.mask := dataModule.io.rdata(i).mask
    io.sbuffer(i).bits.id   := DontCare

    when (io.sbuffer(i).fire()) {
      allocated(ptr) := false.B
      XSDebug("sbuffer "+i+" fire: ptr %d\n", ptr)
    }
  }
  when (io.sbuffer(1).fire()) {
    assert(io.sbuffer(0).fire())
  }

  val storeCommit = PopCount(io.sbuffer.map(_.fire()))
  val waddr = VecInit(io.sbuffer.map(req => SignExt(req.bits.addr, 64)))
  val wdata = VecInit(io.sbuffer.map(req => req.bits.data & MaskExpand(req.bits.mask)))
  val wmask = VecInit(io.sbuffer.map(_.bits.mask))

  if (!env.FPGAPlatform) {
    difftestIO.storeCommit := RegNext(storeCommit)
    difftestIO.storeAddr   := RegNext(waddr)
    difftestIO.storeData   := RegNext(wdata)
    difftestIO.storeMask   := RegNext(wmask)
  }

  // Read vaddr for mem exception
  io.exceptionAddr.vaddr := vaddrModule.io.rdata(0)

  // misprediction recovery / exception redirect
  // invalidate sq term using robIdx
  val needCancel = Wire(Vec(StoreQueueSize, Bool()))
  for (i <- 0 until StoreQueueSize) {
    needCancel(i) := uop(i).roqIdx.needFlush(io.brqRedirect, io.flush) && allocated(i) && !commited(i)
    when (needCancel(i)) {
        allocated(i) := false.B
    }
  }

  /**
    * update pointers
    */
  val lastCycleRedirect = RegNext(io.brqRedirect.valid)
  val lastCycleFlush = RegNext(io.flush)
  val lastCycleCancelCount = PopCount(RegNext(needCancel))
  // when io.brqRedirect.valid, we don't allow eneuque even though it may fire.
  val enqNumber = Mux(io.enq.canAccept && io.enq.lqCanAccept && !(io.brqRedirect.valid || io.flush), PopCount(io.enq.req.map(_.valid)), 0.U)
  when (lastCycleRedirect || lastCycleFlush) {
    // we recover the pointers in the next cycle after redirect
    enqPtrExt := VecInit(enqPtrExt.map(_ - lastCycleCancelCount))
  }.otherwise {
    enqPtrExt := VecInit(enqPtrExt.map(_ + enqNumber))
  }

  deqPtrExt := deqPtrExtNext

  val dequeueCount = Mux(io.sbuffer(1).fire(), 2.U, Mux(io.sbuffer(0).fire() || io.mmioStout.fire(), 1.U, 0.U))
  val validCount = distanceBetween(enqPtrExt(0), deqPtrExt(0))

  allowEnqueue := validCount + enqNumber <= (StoreQueueSize - RenameWidth).U

  // io.sqempty will be used by sbuffer
  // We delay it for 1 cycle for better timing
  // When sbuffer need to check if it is empty, the pipeline is blocked, which means delay io.sqempty
  // for 1 cycle will also promise that sq is empty in that cycle
  io.sqempty := RegNext(enqPtrExt(0).value === deqPtrExt(0).value && enqPtrExt(0).flag === deqPtrExt(0).flag)

  // perf counter
  QueuePerf(StoreQueueSize, validCount, !allowEnqueue)
  XSPerf("mmioCycle", uncacheState =/= s_idle) // lq is busy dealing with uncache req
  XSPerf("mmioCnt", io.uncache.req.fire())
  XSPerf("mmio_wb_success", io.mmioStout.fire())
  XSPerf("mmio_wb_blocked", io.mmioStout.valid && !io.mmioStout.ready)
  XSPerf("validEntryCnt", distanceBetween(enqPtrExt(0), deqPtrExt(0)))
  XSPerf("cmtEntryCnt", distanceBetween(cmtPtrExt(0), deqPtrExt(0)))
  XSPerf("nCmtEntryCnt", distanceBetween(enqPtrExt(0), cmtPtrExt(0)))

  // debug info
  XSDebug("enqPtrExt %d:%d deqPtrExt %d:%d\n", enqPtrExt(0).flag, enqPtr, deqPtrExt(0).flag, deqPtr)

  def PrintFlag(flag: Bool, name: String): Unit = {
    when(flag) {
      XSDebug(false, true.B, name)
    }.otherwise {
      XSDebug(false, true.B, " ")
    }
  }

  for (i <- 0 until StoreQueueSize) {
    if (i % 4 == 0) XSDebug("")
    XSDebug(false, true.B, "%x ", uop(i).cf.pc)
    PrintFlag(allocated(i), "a")
    PrintFlag(allocated(i) && datavalid(i), "v")
    PrintFlag(allocated(i) && writebacked(i), "w")
    PrintFlag(allocated(i) && commited(i), "c")
    PrintFlag(allocated(i) && pending(i), "p")
    XSDebug(false, true.B, " ")
    if (i % 4 == 3 || i == StoreQueueSize - 1) XSDebug(false, true.B, "\n")
  }

}
