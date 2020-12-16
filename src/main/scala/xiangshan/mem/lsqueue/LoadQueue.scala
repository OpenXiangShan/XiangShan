package xiangshan.mem

import chisel3._
import chisel3.util._
import utils._
import xiangshan._
import xiangshan.cache._
import xiangshan.cache.{DCacheWordIO, DCacheLineIO, TlbRequestIO, MemoryOpConstants}
import xiangshan.backend.LSUOpType
import xiangshan.mem._
import xiangshan.backend.roq.RoqPtr
import xiangshan.backend.fu.fpu.boxF32ToF64


class LqPtr extends CircularQueuePtr(LqPtr.LoadQueueSize) { }

object LqPtr extends HasXSParameter {
  def apply(f: Bool, v: UInt): LqPtr = {
    val ptr = Wire(new LqPtr)
    ptr.flag := f
    ptr.value := v
    ptr
  }
}


// Load Queue
class LoadQueue extends XSModule with HasDCacheParameters with HasCircularQueuePtrHelper {
  val io = IO(new Bundle() {
    val enq = new Bundle() {
      val canAccept = Output(Bool())
      val req = Vec(RenameWidth, Flipped(ValidIO(new MicroOp)))
      val resp = Vec(RenameWidth, Output(new LqPtr))
    }
    val brqRedirect = Input(Valid(new Redirect))
    val loadIn = Vec(LoadPipelineWidth, Flipped(Valid(new LsPipelineBundle)))
    val storeIn = Vec(StorePipelineWidth, Flipped(Valid(new LsPipelineBundle))) // FIXME: Valid() only
    val ldout = Vec(2, DecoupledIO(new ExuOutput)) // writeback load
    val forward = Vec(LoadPipelineWidth, Flipped(new LoadForwardQueryIO))
    val commits = Flipped(new RoqCommitIO)
    val rollback = Output(Valid(new Redirect)) // replay now starts from load instead of store
    val dcache = new DCacheLineIO
    val uncache = new DCacheWordIO
    val roqDeqPtr = Input(new RoqPtr)
    val exceptionAddr = new ExceptionAddrIO
    // val refill = Flipped(Valid(new DCacheLineReq ))
  })

  val uop = Reg(Vec(LoadQueueSize, new MicroOp))
  // val data = Reg(Vec(LoadQueueSize, new LsRoqEntry))
  val dataModule = Module(new LSQueueData(LoadQueueSize, LoadPipelineWidth))
  dataModule.io := DontCare
  val allocated = RegInit(VecInit(List.fill(LoadQueueSize)(false.B))) // lq entry has been allocated
  val datavalid = RegInit(VecInit(List.fill(LoadQueueSize)(false.B))) // data is valid
  val writebacked = RegInit(VecInit(List.fill(LoadQueueSize)(false.B))) // inst has been writebacked to CDB
  val commited = Reg(Vec(LoadQueueSize, Bool())) // inst has been writebacked to CDB
  val miss = Reg(Vec(LoadQueueSize, Bool())) // load inst missed, waiting for miss queue to accept miss request
  val listening = Reg(Vec(LoadQueueSize, Bool())) // waiting for refill result
  val pending = Reg(Vec(LoadQueueSize, Bool())) // mmio pending: inst is an mmio inst, it will not be executed until it reachs the end of roq

  val enqPtrExt = RegInit(0.U.asTypeOf(new LqPtr))
  val deqPtrExt = RegInit(0.U.asTypeOf(new LqPtr))
  val enqPtr = enqPtrExt.value
  val deqPtr = deqPtrExt.value
  val sameFlag = enqPtrExt.flag === deqPtrExt.flag
  val isEmpty = enqPtr === deqPtr && sameFlag
  val isFull = enqPtr === deqPtr && !sameFlag
  val allowIn = !isFull

  val loadCommit = (0 until CommitWidth).map(i => io.commits.valid(i) && !io.commits.isWalk && io.commits.uop(i).ctrl.commitType === CommitType.LOAD)
  val mcommitIdx = (0 until CommitWidth).map(i => io.commits.uop(i).lqIdx.value)

  val deqMask = UIntToMask(deqPtr, LoadQueueSize)
  val enqMask = UIntToMask(enqPtr, LoadQueueSize)
  val enqDeqMask1 = deqMask ^ enqMask
  val enqDeqMask = Mux(sameFlag, enqDeqMask1, ~enqDeqMask1)

  // Enqueue at dispatch
  val validEntries = distanceBetween(enqPtrExt, deqPtrExt)
  val firedDispatch = io.enq.req.map(_.valid)
  io.enq.canAccept := validEntries <= (LoadQueueSize - RenameWidth).U
  XSDebug(p"(ready, valid): ${io.enq.canAccept}, ${Binary(Cat(firedDispatch))}\n")
  for (i <- 0 until RenameWidth) {
    val offset = if (i == 0) 0.U else PopCount((0 until i).map(firedDispatch(_)))
    val lqIdx = enqPtrExt + offset
    val index = lqIdx.value
    when(io.enq.req(i).valid) {
      uop(index) := io.enq.req(i).bits
      allocated(index) := true.B
      datavalid(index) := false.B
      writebacked(index) := false.B
      commited(index) := false.B
      miss(index) := false.B
      listening(index) := false.B
      pending(index) := false.B
    }
    io.enq.resp(i) := lqIdx

    XSError(!io.enq.canAccept && io.enq.req(i).valid, "should not valid when not ready\n")
  }

  when(Cat(firedDispatch).orR) {
    enqPtrExt := enqPtrExt + PopCount(firedDispatch)
    XSInfo("dispatched %d insts to lq\n", PopCount(firedDispatch))
  }

  // writeback load
  (0 until LoadPipelineWidth).map(i => {
    dataModule.io.wb(i).wen := false.B
    when(io.loadIn(i).fire()) {
      when(io.loadIn(i).bits.miss) {
        XSInfo(io.loadIn(i).valid, "load miss write to lq idx %d pc 0x%x vaddr %x paddr %x data %x mask %x forwardData %x forwardMask: %x mmio %x roll %x exc %x\n",
          io.loadIn(i).bits.uop.lqIdx.asUInt,
          io.loadIn(i).bits.uop.cf.pc,
          io.loadIn(i).bits.vaddr,
          io.loadIn(i).bits.paddr,
          io.loadIn(i).bits.data,
          io.loadIn(i).bits.mask,
          io.loadIn(i).bits.forwardData.asUInt,
          io.loadIn(i).bits.forwardMask.asUInt,
          io.loadIn(i).bits.mmio,
          io.loadIn(i).bits.rollback,
          io.loadIn(i).bits.uop.cf.exceptionVec.asUInt
          )
        }.otherwise {
          XSInfo(io.loadIn(i).valid, "load hit write to cbd lqidx %d pc 0x%x vaddr %x paddr %x data %x mask %x forwardData %x forwardMask: %x mmio %x roll %x exc %x\n",
          io.loadIn(i).bits.uop.lqIdx.asUInt,
          io.loadIn(i).bits.uop.cf.pc,
          io.loadIn(i).bits.vaddr,
          io.loadIn(i).bits.paddr,
          io.loadIn(i).bits.data,
          io.loadIn(i).bits.mask,
          io.loadIn(i).bits.forwardData.asUInt,
          io.loadIn(i).bits.forwardMask.asUInt,
          io.loadIn(i).bits.mmio,
          io.loadIn(i).bits.rollback,
          io.loadIn(i).bits.uop.cf.exceptionVec.asUInt
          )
        }
        val loadWbIndex = io.loadIn(i).bits.uop.lqIdx.value
        datavalid(loadWbIndex) := !io.loadIn(i).bits.miss && !io.loadIn(i).bits.mmio
        writebacked(loadWbIndex) := !io.loadIn(i).bits.miss && !io.loadIn(i).bits.mmio
        allocated(loadWbIndex) := !io.loadIn(i).bits.uop.cf.exceptionVec.asUInt.orR

        val loadWbData = Wire(new LsqEntry)
        loadWbData.paddr := io.loadIn(i).bits.paddr
        loadWbData.vaddr := io.loadIn(i).bits.vaddr
        loadWbData.mask := io.loadIn(i).bits.mask
        loadWbData.data := io.loadIn(i).bits.data // for mmio / misc / debug
        loadWbData.mmio := io.loadIn(i).bits.mmio
        loadWbData.fwdMask := io.loadIn(i).bits.forwardMask
        loadWbData.fwdData := io.loadIn(i).bits.forwardData
        loadWbData.exception := io.loadIn(i).bits.uop.cf.exceptionVec.asUInt
        dataModule.io.wbWrite(i, loadWbIndex, loadWbData)
        dataModule.io.wb(i).wen := true.B

        val dcacheMissed = io.loadIn(i).bits.miss && !io.loadIn(i).bits.mmio
        miss(loadWbIndex) := dcacheMissed
        listening(loadWbIndex) := dcacheMissed
        pending(loadWbIndex) := io.loadIn(i).bits.mmio
      }
    })

  // cache miss request
  val inflightReqs = RegInit(VecInit(Seq.fill(cfg.nLoadMissEntries)(0.U.asTypeOf(new InflightBlockInfo))))
  val inflightReqFull = inflightReqs.map(req => req.valid).reduce(_&&_)
  val reqBlockIndex = PriorityEncoder(~VecInit(inflightReqs.map(req => req.valid)).asUInt)

  val missRefillSelVec = VecInit(
    (0 until LoadQueueSize).map{ i =>
      val inflight = inflightReqs.map(req => req.valid && req.block_addr === get_block_addr(dataModule.io.rdata(i).paddr)).reduce(_||_)
      allocated(i) && miss(i) && !inflight
    })

  val missRefillSel = getFirstOne(missRefillSelVec, deqMask)
  val missRefillBlockAddr = get_block_addr(dataModule.io.rdata(missRefillSel).paddr)
  io.dcache.req.valid := missRefillSelVec.asUInt.orR
  io.dcache.req.bits.cmd := MemoryOpConstants.M_XRD
  io.dcache.req.bits.addr := missRefillBlockAddr
  io.dcache.req.bits.data := DontCare
  io.dcache.req.bits.mask := DontCare

  io.dcache.req.bits.meta.id       := DontCare
  io.dcache.req.bits.meta.vaddr    := DontCare // dataModule.io.rdata(missRefillSel).vaddr
  io.dcache.req.bits.meta.paddr    := missRefillBlockAddr
  io.dcache.req.bits.meta.uop      := uop(missRefillSel)
  io.dcache.req.bits.meta.mmio     := false.B // dataModule.io.rdata(missRefillSel).mmio
  io.dcache.req.bits.meta.tlb_miss := false.B
  io.dcache.req.bits.meta.mask     := DontCare
  io.dcache.req.bits.meta.replay   := false.B

  io.dcache.resp.ready := true.B

  assert(!(dataModule.io.rdata(missRefillSel).mmio && io.dcache.req.valid))

  when(io.dcache.req.fire()) {
    miss(missRefillSel) := false.B
    listening(missRefillSel) := true.B

    // mark this block as inflight
    inflightReqs(reqBlockIndex).valid := true.B
    inflightReqs(reqBlockIndex).block_addr := missRefillBlockAddr
    assert(!inflightReqs(reqBlockIndex).valid)
  }

  when(io.dcache.resp.fire()) {
    val inflight = inflightReqs.map(req => req.valid && req.block_addr === get_block_addr(io.dcache.resp.bits.meta.paddr)).reduce(_||_)
    assert(inflight)
    for (i <- 0 until cfg.nLoadMissEntries) {
      when (inflightReqs(i).valid && inflightReqs(i).block_addr === get_block_addr(io.dcache.resp.bits.meta.paddr)) {
        inflightReqs(i).valid := false.B
      }
    }
  }


  when(io.dcache.req.fire()){
    XSDebug("miss req: pc:0x%x roqIdx:%d lqIdx:%d (p)addr:0x%x vaddr:0x%x\n",
      io.dcache.req.bits.meta.uop.cf.pc, io.dcache.req.bits.meta.uop.roqIdx.asUInt, io.dcache.req.bits.meta.uop.lqIdx.asUInt,
      io.dcache.req.bits.addr, io.dcache.req.bits.meta.vaddr
    )
  }

  when(io.dcache.resp.fire()){
    XSDebug("miss resp: pc:0x%x roqIdx:%d lqIdx:%d (p)addr:0x%x data %x\n",
      io.dcache.resp.bits.meta.uop.cf.pc, io.dcache.resp.bits.meta.uop.roqIdx.asUInt, io.dcache.resp.bits.meta.uop.lqIdx.asUInt,
      io.dcache.resp.bits.meta.paddr, io.dcache.resp.bits.data
    )
  }

  // Refill 64 bit in a cycle
  // Refill data comes back from io.dcache.resp
  dataModule.io.refill.dcache := io.dcache.resp.bits

  (0 until LoadQueueSize).map(i => {
    val blockMatch = get_block_addr(dataModule.io.rdata(i).paddr) === io.dcache.resp.bits.meta.paddr
    dataModule.io.refill.wen(i) := false.B
    when(allocated(i) && listening(i) && blockMatch && io.dcache.resp.fire()) {
      dataModule.io.refill.wen(i) := true.B
      datavalid(i) := true.B
      listening(i) := false.B
    }
  })

  // writeback up to 2 missed load insts to CDB
  // just randomly pick 2 missed load (data refilled), write them back to cdb
  val loadWbSelVec = VecInit((0 until LoadQueueSize).map(i => {
    allocated(i) && datavalid(i) && !writebacked(i)
  })).asUInt() // use uint instead vec to reduce verilog lines
  val loadWbSel = Wire(Vec(StorePipelineWidth, UInt(log2Up(LoadQueueSize).W)))
  val loadWbSelV= Wire(Vec(StorePipelineWidth, Bool()))
  val lselvec0 = PriorityEncoderOH(loadWbSelVec)
  val lselvec1 = PriorityEncoderOH(loadWbSelVec & (~lselvec0).asUInt)
  loadWbSel(0) := OHToUInt(lselvec0)
  loadWbSelV(0):= lselvec0.orR
  loadWbSel(1) := OHToUInt(lselvec1)
  loadWbSelV(1) := lselvec1.orR
  (0 until StorePipelineWidth).map(i => {
    // data select
    val rdata = dataModule.io.rdata(loadWbSel(i)).data
    val func = uop(loadWbSel(i)).ctrl.fuOpType
    val raddr = dataModule.io.rdata(loadWbSel(i)).paddr
    val rdataSel = LookupTree(raddr(2, 0), List(
      "b000".U -> rdata(63, 0),
      "b001".U -> rdata(63, 8),
      "b010".U -> rdata(63, 16),
      "b011".U -> rdata(63, 24),
      "b100".U -> rdata(63, 32),
      "b101".U -> rdata(63, 40),
      "b110".U -> rdata(63, 48),
      "b111".U -> rdata(63, 56)
    ))
    val rdataPartialLoad = LookupTree(func, List(
        LSUOpType.lb   -> SignExt(rdataSel(7, 0) , XLEN),
        LSUOpType.lh   -> SignExt(rdataSel(15, 0), XLEN),
        LSUOpType.lw   -> SignExt(rdataSel(31, 0), XLEN),
        LSUOpType.ld   -> SignExt(rdataSel(63, 0), XLEN),
        LSUOpType.lbu  -> ZeroExt(rdataSel(7, 0) , XLEN),
        LSUOpType.lhu  -> ZeroExt(rdataSel(15, 0), XLEN),
        LSUOpType.lwu  -> ZeroExt(rdataSel(31, 0), XLEN),
        LSUOpType.flw  -> boxF32ToF64(rdataSel(31, 0))
    ))
    io.ldout(i).bits.uop := uop(loadWbSel(i))
    io.ldout(i).bits.uop.cf.exceptionVec := dataModule.io.rdata(loadWbSel(i)).exception.asBools
    io.ldout(i).bits.uop.lqIdx := loadWbSel(i).asTypeOf(new LqPtr)
    io.ldout(i).bits.data := rdataPartialLoad
    io.ldout(i).bits.redirectValid := false.B
    io.ldout(i).bits.redirect := DontCare
    io.ldout(i).bits.brUpdate := DontCare
    io.ldout(i).bits.debug.isMMIO := dataModule.io.rdata(loadWbSel(i)).mmio
    io.ldout(i).bits.fflags := DontCare
    io.ldout(i).valid := loadWbSelVec(loadWbSel(i)) && loadWbSelV(i)
    when(io.ldout(i).fire()) {
      writebacked(loadWbSel(i)) := true.B
      XSInfo("load miss write to cbd roqidx %d lqidx %d pc 0x%x paddr %x data %x mmio %x\n",
        io.ldout(i).bits.uop.roqIdx.asUInt,
        io.ldout(i).bits.uop.lqIdx.asUInt,
        io.ldout(i).bits.uop.cf.pc,
        dataModule.io.rdata(loadWbSel(i)).paddr,
        dataModule.io.rdata(loadWbSel(i)).data,
        dataModule.io.rdata(loadWbSel(i)).mmio
      )
    }
  })

  // move tailPtr
  // allocatedMask: dequeuePtr can go to the next 1-bit
  val allocatedMask = VecInit((0 until LoadQueueSize).map(i => allocated(i) || !enqDeqMask(i)))
  // find the first one from deqPtr (deqPtr)
  val nextTail1 = getFirstOneWithFlag(allocatedMask, deqMask, deqPtrExt.flag)
  val nextTail = Mux(Cat(allocatedMask).orR, nextTail1, enqPtrExt)
  deqPtrExt := nextTail

  // When load commited, mark it as !allocated, this entry will be recycled later
  (0 until CommitWidth).map(i => {
    when(loadCommit(i)) {
      allocated(mcommitIdx(i)) := false.B
      XSDebug("load commit %d: idx %d %x\n", i.U, mcommitIdx(i), uop(mcommitIdx(i)).cf.pc)
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
    LqPtr(startFlag ^ changeDirection, index)
  }

  def getOldestInTwo(valid: Seq[Bool], uop: Seq[MicroOp]) = {
    assert(valid.length == uop.length)
    assert(valid.length == 2)
    Mux(valid(0) && valid(1),
      Mux(isAfter(uop(0).roqIdx, uop(1).roqIdx), uop(1), uop(0)),
      Mux(valid(0) && !valid(1), uop(0), uop(1)))
  }

  def getAfterMask(valid: Seq[Bool], uop: Seq[MicroOp]) = {
    assert(valid.length == uop.length)
    val length = valid.length
    (0 until length).map(i => {
      (0 until length).map(j => {
        Mux(valid(i) && valid(j),
          isAfter(uop(i).roqIdx, uop(j).roqIdx),
          Mux(!valid(i), true.B, false.B))
      })
    })
  }

  def rangeMask(start: LqPtr, end: LqPtr): UInt = {
    val startMask = (1.U((LoadQueueSize + 1).W) << start.value).asUInt - 1.U
    val endMask = (1.U((LoadQueueSize + 1).W) << end.value).asUInt - 1.U
    val xorMask = startMask(LoadQueueSize - 1, 0) ^ endMask(LoadQueueSize - 1, 0)
    Mux(start.flag === end.flag, xorMask, ~xorMask)
  }

  // ignore data forward
  (0 until LoadPipelineWidth).foreach(i => {
    io.forward(i).forwardMask := DontCare
    io.forward(i).forwardData := DontCare
  })

  // store backward query and rollback
  def detectRollback(i: Int) = {
    val startIndex = io.storeIn(i).bits.uop.lqIdx.value
    val lqIdxMask = UIntToMask(startIndex, LoadQueueSize)
    val xorMask = lqIdxMask ^ enqMask
    val sameFlag = io.storeIn(i).bits.uop.lqIdx.flag === enqPtrExt.flag
    val toEnqPtrMask = Mux(sameFlag, xorMask, ~xorMask)

    // check if load already in lq needs to be rolledback
    val lqViolationVec = RegNext(VecInit((0 until LoadQueueSize).map(j => {
      val addrMatch = allocated(j) &&
        io.storeIn(i).bits.paddr(PAddrBits - 1, 3) === dataModule.io.rdata(j).paddr(PAddrBits - 1, 3)
      val entryNeedCheck = toEnqPtrMask(j) && addrMatch && (datavalid(j) || listening(j) || miss(j))
      // TODO: update refilled data
      val violationVec = (0 until 8).map(k => dataModule.io.rdata(j).mask(k) && io.storeIn(i).bits.mask(k))
      Cat(violationVec).orR() && entryNeedCheck
    })))
    val lqViolation = lqViolationVec.asUInt().orR()
    val lqViolationIndex = getFirstOne(lqViolationVec, RegNext(lqIdxMask))
    val lqViolationUop = uop(lqViolationIndex)
    // lqViolationUop.lqIdx.flag := deqMask(lqViolationIndex) ^ deqPtrExt.flag
    // lqViolationUop.lqIdx.value := lqViolationIndex
    XSDebug(lqViolation, p"${Binary(Cat(lqViolationVec))}, $startIndex, $lqViolationIndex\n")

    // when l/s writeback to roq together, check if rollback is needed
    val wbViolationVec = RegNext(VecInit((0 until LoadPipelineWidth).map(j => {
      io.loadIn(j).valid &&
        isAfter(io.loadIn(j).bits.uop.roqIdx, io.storeIn(i).bits.uop.roqIdx) &&
        io.storeIn(i).bits.paddr(PAddrBits - 1, 3) === io.loadIn(j).bits.paddr(PAddrBits - 1, 3) &&
        (io.storeIn(i).bits.mask & io.loadIn(j).bits.mask).orR
    })))
    val wbViolation = wbViolationVec.asUInt().orR()
    val wbViolationUop = getOldestInTwo(wbViolationVec, RegNext(VecInit(io.loadIn.map(_.bits.uop))))
    XSDebug(wbViolation, p"${Binary(Cat(wbViolationVec))}, $wbViolationUop\n")

    // check if rollback is needed for load in l1
    val l1ViolationVec = RegNext(VecInit((0 until LoadPipelineWidth).map(j => {
      io.forward(j).valid && // L1 valid
        isAfter(io.forward(j).uop.roqIdx, io.storeIn(i).bits.uop.roqIdx) &&
        io.storeIn(i).bits.paddr(PAddrBits - 1, 3) === io.forward(j).paddr(PAddrBits - 1, 3) &&
        (io.storeIn(i).bits.mask & io.forward(j).mask).orR
    })))
    val l1Violation = l1ViolationVec.asUInt().orR()
    val l1ViolationUop = getOldestInTwo(l1ViolationVec, RegNext(VecInit(io.forward.map(_.uop))))
    XSDebug(l1Violation, p"${Binary(Cat(l1ViolationVec))}, $l1ViolationUop\n")

    val rollbackValidVec = Seq(lqViolation, wbViolation, l1Violation)
    val rollbackUopVec = Seq(lqViolationUop, wbViolationUop, l1ViolationUop)

    val mask = getAfterMask(rollbackValidVec, rollbackUopVec)
    val oneAfterZero = mask(1)(0)
    val rollbackUop = Mux(oneAfterZero && mask(2)(0),
      rollbackUopVec(0),
      Mux(!oneAfterZero && mask(2)(1), rollbackUopVec(1), rollbackUopVec(2)))

    XSDebug(
      l1Violation,
      "need rollback (l4 load) pc %x roqidx %d target %x\n",
      io.storeIn(i).bits.uop.cf.pc, io.storeIn(i).bits.uop.roqIdx.asUInt, l1ViolationUop.roqIdx.asUInt
    )
    XSDebug(
      lqViolation,
      "need rollback (ld wb before store) pc %x roqidx %d target %x\n",
      io.storeIn(i).bits.uop.cf.pc, io.storeIn(i).bits.uop.roqIdx.asUInt, lqViolationUop.roqIdx.asUInt
    )
    XSDebug(
      wbViolation,
      "need rollback (ld/st wb together) pc %x roqidx %d target %x\n",
      io.storeIn(i).bits.uop.cf.pc, io.storeIn(i).bits.uop.roqIdx.asUInt, wbViolationUop.roqIdx.asUInt
    )

    (RegNext(io.storeIn(i).valid) && Cat(rollbackValidVec).orR, rollbackUop)
  }

  // rollback check
  val rollback = Wire(Vec(StorePipelineWidth, Valid(new MicroOp)))
  for (i <- 0 until StorePipelineWidth) {
    val detectedRollback = detectRollback(i)
    rollback(i).valid := detectedRollback._1
    rollback(i).bits := detectedRollback._2
  }

  def rollbackSel(a: Valid[MicroOp], b: Valid[MicroOp]): ValidIO[MicroOp] = {
    Mux(
      a.valid,
      Mux(
        b.valid,
        Mux(isAfter(a.bits.roqIdx, b.bits.roqIdx), b, a), // a,b both valid, sel oldest
        a // sel a
      ),
      b // sel b
    )
  }

  val rollbackSelected = ParallelOperation(rollback, rollbackSel)
  val lastCycleRedirect = RegNext(io.brqRedirect)

  io.rollback := DontCare
  // Note that we use roqIdx - 1.U to flush the load instruction itself.
  // Thus, here if last cycle's roqIdx equals to this cycle's roqIdx, it still triggers the redirect.
  io.rollback.valid := rollbackSelected.valid && (!lastCycleRedirect.valid || !isAfter(rollbackSelected.bits.roqIdx, lastCycleRedirect.bits.roqIdx))

  io.rollback.bits.roqIdx := rollbackSelected.bits.roqIdx - 1.U
  io.rollback.bits.isReplay := true.B
  io.rollback.bits.isMisPred := false.B
  io.rollback.bits.isException := false.B
  io.rollback.bits.isFlushPipe := false.B
  io.rollback.bits.target := rollbackSelected.bits.cf.pc
  io.rollback.bits.brTag := rollbackSelected.bits.brTag

  // Memory mapped IO / other uncached operations

  // setup misc mem access req
  // mask / paddr / data can be get from lq.data
  val commitType = io.commits.uop(0).ctrl.commitType
  io.uncache.req.valid := pending(deqPtr) && allocated(deqPtr) &&
    commitType === CommitType.LOAD &&
    io.roqDeqPtr === uop(deqPtr).roqIdx &&
    !io.commits.isWalk

  io.uncache.req.bits.cmd  := MemoryOpConstants.M_XRD
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

  when (io.uncache.req.fire()) {
    pending(deqPtr) := false.B
  }

  dataModule.io.uncache.wen := false.B
  when(io.uncache.resp.fire()){
    datavalid(deqPtr) := true.B
    dataModule.io.uncacheWrite(deqPtr, io.uncache.resp.bits.data(XLEN-1, 0))
    dataModule.io.uncache.wen := true.B
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

  when(io.uncache.resp.fire()){
    XSDebug("uncache resp: data %x\n", io.dcache.resp.bits.data)
  }

  // Read vaddr for mem exception
  io.exceptionAddr.vaddr := dataModule.io.rdata(io.exceptionAddr.lsIdx.lqIdx.value).vaddr

  // misprediction recovery / exception redirect
  // invalidate lq term using robIdx
  val needCancel = Wire(Vec(LoadQueueSize, Bool()))
  for (i <- 0 until LoadQueueSize) {
    needCancel(i) := uop(i).roqIdx.needFlush(io.brqRedirect) && allocated(i) && !commited(i)
    when(needCancel(i)) {
      // when(io.brqRedirect.bits.isReplay){
      //   valid(i) := false.B
      //   writebacked(i) := false.B
      //   listening(i) := false.B
      //   miss(i) := false.B
      //   pending(i) := false.B
      // }.otherwise{
        allocated(i) := false.B
      // }
    }
  }
  when (io.brqRedirect.valid && io.brqRedirect.bits.isMisPred) {
    enqPtrExt := enqPtrExt - PopCount(needCancel)
  }

  // assert(!io.rollback.valid)
  when(io.rollback.valid) {
    XSDebug("Mem rollback: pc %x roqidx %d\n", io.rollback.bits.pc, io.rollback.bits.roqIdx.asUInt)
  }

  // debug info
  XSDebug("head %d:%d tail %d:%d\n", enqPtrExt.flag, enqPtr, deqPtrExt.flag, deqPtr)

  def PrintFlag(flag: Bool, name: String): Unit = {
    when(flag) {
      XSDebug(false, true.B, name)
    }.otherwise {
      XSDebug(false, true.B, " ")
    }
  }

  for (i <- 0 until LoadQueueSize) {
    if (i % 4 == 0) XSDebug("")
    XSDebug(false, true.B, "%x [%x] ", uop(i).cf.pc, dataModule.io.rdata(i).paddr)
    PrintFlag(allocated(i), "a")
    PrintFlag(allocated(i) && datavalid(i), "v")
    PrintFlag(allocated(i) && writebacked(i), "w")
    PrintFlag(allocated(i) && commited(i), "c")
    PrintFlag(allocated(i) && miss(i), "m")
    PrintFlag(allocated(i) && listening(i), "l")
    PrintFlag(allocated(i) && pending(i), "p")
    XSDebug(false, true.B, " ")
    if (i % 4 == 3 || i == LoadQueueSize - 1) XSDebug(false, true.B, "\n")
  }

}
