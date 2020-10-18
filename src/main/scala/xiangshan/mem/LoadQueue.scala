package xiangshan.mem

import chisel3._
import chisel3.util._
import utils._
import xiangshan._
import xiangshan.cache._
import xiangshan.cache.{DCacheWordIO, DCacheLineIO, TlbRequestIO, MemoryOpConstants}
import xiangshan.backend.LSUOpType
import xiangshan.mem._

// Load Queue
class LoadQueue extends XSModule with HasDCacheParameters with NeedImpl {
  val io = IO(new Bundle() {
    val dp1Req = Vec(RenameWidth, Flipped(DecoupledIO(new MicroOp)))
    val lsroqIdxs = Output(Vec(RenameWidth, UInt(LsroqIdxWidth.W)))
    val brqRedirect = Input(Valid(new Redirect))
    val loadIn = Vec(LoadPipelineWidth, Flipped(Valid(new LsPipelineBundle)))
    val ldout = Vec(2, DecoupledIO(new ExuOutput)) // writeback store
    val forward = Vec(LoadPipelineWidth, Flipped(new LoadForwardQueryIO))
    val commits = Flipped(Vec(LoadPipelineWidth, Valid(new RoqCommit)))
    val rollback = Output(Valid(new Redirect)) // replay now starts from load instead of store
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
  
  val loadCommit = (0 until CommitWidth).map(i => io.commits(i).valid && !io.commits(i).bits.isWalk && io.commits(i).bits.uop.ctrl.commitType === CommitType.LOAD)
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

  // writeback load
  (0 until LoadPipelineWidth).map(i => {
    when(io.loadIn(i).fire()) {
      when(io.loadIn(i).bits.miss) {
        XSInfo(io.loadIn(i).valid, "load miss write to lsroq idx %d pc 0x%x vaddr %x paddr %x data %x mask %x forwardData %x forwardMask: %x mmio %x roll %x exc %x\n",
          io.loadIn(i).bits.uop.lsroqIdx,
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
          XSInfo(io.loadIn(i).valid, "load hit write to cbd idx %d pc 0x%x vaddr %x paddr %x data %x mask %x forwardData %x forwardMask: %x mmio %x roll %x exc %x\n",
          io.loadIn(i).bits.uop.lsroqIdx,
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
        valid(io.loadIn(i).bits.uop.lsroqIdx) := !io.loadIn(i).bits.miss && !io.loadIn(i).bits.mmio
        writebacked(io.loadIn(i).bits.uop.lsroqIdx) := !io.loadIn(i).bits.miss && !io.loadIn(i).bits.mmio
        // allocated(io.loadIn(i).bits.uop.lsroqIdx) := io.loadIn(i).bits.miss // if hit, lsroq entry can be recycled
        data(io.loadIn(i).bits.uop.lsroqIdx).paddr := io.loadIn(i).bits.paddr
        data(io.loadIn(i).bits.uop.lsroqIdx).vaddr := io.loadIn(i).bits.vaddr
        data(io.loadIn(i).bits.uop.lsroqIdx).mask := io.loadIn(i).bits.mask
        data(io.loadIn(i).bits.uop.lsroqIdx).data := io.loadIn(i).bits.data // for mmio / misc / debug
        data(io.loadIn(i).bits.uop.lsroqIdx).mmio := io.loadIn(i).bits.mmio
        data(io.loadIn(i).bits.uop.lsroqIdx).fwdMask := io.loadIn(i).bits.forwardMask
        data(io.loadIn(i).bits.uop.lsroqIdx).fwdData := io.loadIn(i).bits.forwardData
        data(io.loadIn(i).bits.uop.lsroqIdx).exception := io.loadIn(i).bits.uop.cf.exceptionVec.asUInt
        val dcacheMissed = io.loadIn(i).bits.miss && !io.loadIn(i).bits.mmio
        miss(io.loadIn(i).bits.uop.lsroqIdx) := dcacheMissed
        listening(io.loadIn(i).bits.uop.lsroqIdx) := dcacheMissed
        store(io.loadIn(i).bits.uop.lsroqIdx) := false.B
        pending(io.loadIn(i).bits.uop.lsroqIdx) := io.loadIn(i).bits.mmio
      }
    })

  // cache miss request
  val inflightReqs = RegInit(VecInit(Seq.fill(cfg.nLoadMissEntries)(0.U.asTypeOf(new InflightBlockInfo))))
  val inflightReqFull = inflightReqs.map(req => req.valid).reduce(_&&_)
  val reqBlockIndex = PriorityEncoder(~VecInit(inflightReqs.map(req => req.valid)).asUInt)

  val missRefillSelVec = VecInit(
    (0 until LsroqSize).map{ i =>
      val inflight = inflightReqs.map(req => req.valid && req.block_addr === get_block_addr(data(i).paddr)).reduce(_||_)
      allocated(i) && miss(i) && !inflight
    })

  val missRefillSel = getFirstOne(missRefillSelVec, tailMask)
  val missRefillBlockAddr = get_block_addr(data(missRefillSel).paddr)
  io.dcache.req.valid := missRefillSelVec.asUInt.orR
  io.dcache.req.bits.cmd := MemoryOpConstants.M_XRD
  io.dcache.req.bits.addr := missRefillBlockAddr
  io.dcache.req.bits.data := DontCare
  io.dcache.req.bits.mask := DontCare

  io.dcache.req.bits.meta.id       := DontCare // TODO: // FIXME
  io.dcache.req.bits.meta.vaddr    := DontCare // data(missRefillSel).vaddr
  io.dcache.req.bits.meta.paddr    := missRefillBlockAddr
  io.dcache.req.bits.meta.uop      := uop(missRefillSel)
  io.dcache.req.bits.meta.mmio     := false.B // data(missRefillSel).mmio
  io.dcache.req.bits.meta.tlb_miss := false.B
  io.dcache.req.bits.meta.mask     := DontCare
  io.dcache.req.bits.meta.replay   := false.B

  io.dcache.resp.ready := true.B

  assert(!(data(missRefillSel).mmio && io.dcache.req.valid))

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
    XSDebug("miss req: pc:0x%x roqIdx:%d lsroqIdx:%d (p)addr:0x%x vaddr:0x%x\n", io.dcache.req.bits.meta.uop.cf.pc, io.dcache.req.bits.meta.uop.roqIdx, io.dcache.req.bits.meta.uop.lsroqIdx, io.dcache.req.bits.addr, io.dcache.req.bits.meta.vaddr) 
  }

  when(io.dcache.resp.fire()){
    XSDebug("miss resp: pc:0x%x roqIdx:%d lsroqIdx:%d (p)addr:0x%x data %x\n", io.dcache.resp.bits.meta.uop.cf.pc, io.dcache.resp.bits.meta.uop.roqIdx, io.dcache.resp.bits.meta.uop.lsroqIdx, io.dcache.resp.bits.meta.paddr, io.dcache.resp.bits.data) 
  }

  // Refill 64 bit in a cycle
  // Refill data comes back from io.dcache.resp
  def mergeRefillData(refill: UInt, fwd: UInt, fwdMask: UInt): UInt = {
    val res = Wire(Vec(8, UInt(8.W)))
    (0 until 8).foreach(i => {
      res(i) := Mux(fwdMask(i), fwd(8 * (i + 1) - 1, 8 * i), refill(8 * (i + 1) - 1, 8 * i))
    })
    res.asUInt
  }

  (0 until LsroqSize).map(i => {
    val blockMatch = get_block_addr(data(i).paddr) === io.dcache.resp.bits.meta.paddr
    when(allocated(i) && listening(i) && blockMatch && io.dcache.resp.fire()) {
      // split them into words
      val words = VecInit((0 until blockWords) map { i =>
        io.dcache.resp.bits.data(DataBits * (i + 1) - 1, DataBits * i)
      })

      val refillData = words(get_word(data(i).paddr))
      data(i).data := mergeRefillData(refillData, data(i).fwdData.asUInt, data(i).fwdMask.asUInt)
      valid(i) := true.B
      listening(i) := false.B
      XSDebug("miss resp: pos %d addr %x data %x + %x(%b)\n", i.U, data(i).paddr, refillData, data(i).fwdData.asUInt, data(i).fwdMask.asUInt) 
    }
  })

  // writeback up to 2 missed load insts to CDB
  // just randomly pick 2 missed load (data refilled), write them back to cdb
  val loadWbSelVec = VecInit((0 until LsroqSize).map(i => {
    allocated(i) && valid(i) && !writebacked(i) && !store(i)
  })).asUInt() // use uint instead vec to reduce verilog lines
  val loadWbSel = Wire(Vec(StorePipelineWidth, UInt(log2Up(LsroqSize).W)))
  val lselvec0 = PriorityEncoderOH(loadWbSelVec)
  val lselvec1 = PriorityEncoderOH(loadWbSelVec & (~lselvec0).asUInt)
  loadWbSel(0) := OHToUInt(lselvec0)
  loadWbSel(1) := OHToUInt(lselvec1)
  (0 until StorePipelineWidth).map(i => {
    // data select
    val rdata = data(loadWbSel(i)).data
    val func = uop(loadWbSel(i)).ctrl.fuOpType
    val raddr = data(loadWbSel(i)).paddr
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
        LSUOpType.lwu  -> ZeroExt(rdataSel(31, 0), XLEN)
    ))
    io.ldout(i).bits.uop := uop(loadWbSel(i))
    io.ldout(i).bits.uop.cf.exceptionVec := data(loadWbSel(i)).exception.asBools
    io.ldout(i).bits.uop.lsroqIdx := loadWbSel(i)
    io.ldout(i).bits.data := rdataPartialLoad
    io.ldout(i).bits.redirectValid := false.B
    io.ldout(i).bits.redirect := DontCare
    io.ldout(i).bits.brUpdate := DontCare
    io.ldout(i).bits.debug.isMMIO := data(loadWbSel(i)).mmio
    io.ldout(i).valid := loadWbSelVec(loadWbSel(i))
    when(io.ldout(i).fire()) {
      writebacked(loadWbSel(i)) := true.B
      XSInfo(io.loadIn(i).valid, "load miss write to cbd idx %d pc 0x%x paddr %x data %x mmio %x\n",
        io.ldout(i).bits.uop.lsroqIdx,
        io.ldout(i).bits.uop.cf.pc,
        data(loadWbSel(i)).paddr,
        data(loadWbSel(i)).data,
        data(loadWbSel(i)).mmio
      )
    }
  })

  // move tailPtr
  // allocatedMask: dequeuePtr can go to the next 1-bit
  val allocatedMask = VecInit((0 until LsroqSize).map(i => allocated(i) || !enqDeqMask(i)))
  // find the first one from deqPtr (ringBufferTail)
  val nextTail1 = getFirstOneWithFlag(allocatedMask, tailMask, ringBufferTailExtended(InnerLsroqIdxWidth))
  val nextTail = Mux(Cat(allocatedMask).orR, nextTail1, ringBufferHeadExtended)
  ringBufferTailExtended := nextTail

  // When load commited, mark it as !allocated, this entry will be recycled later
  (0 until CommitWidth).map(i => {
    when(loadCommit(i)) {
      allocated(mcommitIdx(i)) := false.B
      XSDebug("load commit %d: idx %d %x\n", i.U, mcommitIdx(i), uop(mcommitIdx(i)).cf.pc)
    }
  })

  // load forward query
  // check over all lsroq entries and forward data from the first matched store
  // TODO: FIXME
  (0 until LoadPipelineWidth).map(i => {
    io.forward(i).forwardMask := 0.U(8.W).asBools
    io.forward(i).forwardData := DontCare

    // Compare ringBufferTail (deqPtr) and forward.lsroqIdx, we have two cases:
    // (1) if they have the same flag, we need to check range(tail, lsroqIdx)
    // (2) if they have different flags, we need to check range(tail, lsroqSize) and range(0, lsroqIdx)
    // Forward1: Mux(same_flag, range(tail, lsroqIdx), range(tail, lsroqSize))
    // Forward2: Mux(same_flag, 0.U,                   range(0, lsroqIdx)    )
    // i.e. forward1 is the target entries with the same flag bits and forward2 otherwise
    val forwardMask1 = WireInit(VecInit(Seq.fill(8)(false.B)))
    val forwardData1 = WireInit(VecInit(Seq.fill(8)(0.U(8.W))))
    val forwardMask2 = WireInit(VecInit(Seq.fill(8)(false.B)))
    val forwardData2 = WireInit(VecInit(Seq.fill(8)(0.U(8.W))))

    val differentFlag = ringBufferTailExtended(InnerLsroqIdxWidth) =/= io.forward(i).lsroqIdx(InnerLsroqIdxWidth)
    val forwardMask = ((1.U((LsroqSize + 1).W)) << io.forward(i).lsroqIdx(InnerLsroqIdxWidth - 1, 0)).asUInt - 1.U
    val needForward1 = Mux(differentFlag, ~tailMask, tailMask ^ forwardMask)
    val needForward2 = Mux(differentFlag, forwardMask, 0.U(LsroqSize.W))

    // entry with larger index should have higher priority since it's data is younger
    for (j <- 0 until LsroqSize) {
      val needCheck = valid(j) && allocated(j) && store(j) &&
        io.forward(i).paddr(PAddrBits - 1, 3) === data(j).paddr(PAddrBits - 1, 3)
      (0 until XLEN / 8).foreach(k => {
        when (needCheck && data(j).mask(k)) {
          when (needForward1(j)) {
            forwardMask1(k) := true.B
            forwardData1(k) := data(j).data(8 * (k + 1) - 1, 8 * k)
          }
          when (needForward2(j)) {
            forwardMask2(k) := true.B
            forwardData2(k) := data(j).data(8 * (k + 1) - 1, 8 * k)
          }
          XSDebug(needForward1(j) || needForward2(j),
            p"forwarding $k-th byte ${Hexadecimal(data(j).data(8 * (k + 1) - 1, 8 * k))} " +
            p"from ptr $j pc ${Hexadecimal(uop(j).cf.pc)}\n")
        }
      })
    }

    // merge forward lookup results
    // forward2 is younger than forward1 and should have higher priority
    (0 until XLEN / 8).map(k => {
      io.forward(i).forwardMask(k) := forwardMask1(k) || forwardMask2(k)
      io.forward(i).forwardData(k) := Mux(forwardMask2(k), forwardData2(k), forwardData1(k))
    })
  })

  // rollback check
  val rollback = Wire(Vec(StorePipelineWidth, Valid(new Redirect)))

  def getFirstOne(mask: Vec[Bool], startMask: UInt) = {
    val length = mask.length
    val highBits = (0 until length).map(i => mask(i) & ~startMask(i))
    val highBitsUint = Cat(highBits.reverse)
    PriorityEncoder(Mux(highBitsUint.orR(), highBitsUint, mask.asUInt))
  }

  def getFirstOneWithFlag(mask: Vec[Bool], startMask: UInt, startFlag: UInt) = {
    val length = mask.length
    val highBits = (0 until length).map(i => mask(i) & ~startMask(i))
    val highBitsUint = Cat(highBits.reverse)
    val changeDirection = !highBitsUint.orR()
    val index = PriorityEncoder(Mux(!changeDirection, highBitsUint, mask.asUInt))
    Cat(startFlag ^ changeDirection, index)
  }

  def getOldestInTwo(valid: Seq[Bool], uop: Seq[MicroOp]) = {
    assert(valid.length == uop.length)
    assert(valid.length == 2)
    Mux(valid(0) && valid(1),
      Mux(uop(0).isAfter(uop(1)), uop(1), uop(0)),
      Mux(valid(0) && !valid(1), uop(0), uop(1)))
  }

  def getAfterMask(valid: Seq[Bool], uop: Seq[MicroOp]) = {
    assert(valid.length == uop.length)
    val length = valid.length
    (0 until length).map(i => {
      (0 until length).map(j => {
        Mux(valid(i) && valid(j),
          uop(i).isAfter(uop(j)),
          Mux(!valid(i), true.B, false.B))
      })
    })
  }

  def rangeMask(start: UInt, end: UInt): UInt = {
    val startMask = (1.U((LsroqSize + 1).W) << start(InnerLsroqIdxWidth - 1, 0)).asUInt - 1.U
    val endMask = (1.U((LsroqSize + 1).W) << end(InnerLsroqIdxWidth - 1, 0)).asUInt - 1.U
    val xorMask = startMask(LsroqSize - 1, 0) ^ endMask(LsroqSize - 1, 0)
    Mux(start(InnerLsroqIdxWidth) === end(InnerLsroqIdxWidth), xorMask, ~xorMask)
  }

  // store backward query and rollback
  //  val needCheck = Seq.fill(8)(WireInit(true.B))
  (0 until StorePipelineWidth).foreach(i => {
    rollback(i) := DontCare

    when(io.storeIn(i).valid) {
      val startIndex = io.storeIn(i).bits.uop.lsroqIdx(InnerLsroqIdxWidth - 1, 0)
      val lsroqIdxMask = ((1.U((LsroqSize + 1).W) << startIndex).asUInt - 1.U)(LsroqSize - 1, 0)
      val xorMask = lsroqIdxMask ^ headMask
      val sameFlag = io.storeIn(i).bits.uop.lsroqIdx(InnerLsroqIdxWidth) === ringBufferHeadExtended(InnerLsroqIdxWidth)
      val toEnqPtrMask = Mux(sameFlag, xorMask, ~xorMask)
      val lsroqViolationVec = VecInit((0 until LsroqSize).map(j => {
        val addrMatch = allocated(j) &&
          io.storeIn(i).bits.paddr(PAddrBits - 1, 3) === data(j).paddr(PAddrBits - 1, 3)
        val entryNeedCheck = toEnqPtrMask(j) && addrMatch && !store(j) && (valid(j) || listening(j) || miss(j))
        // TODO: update refilled data
        val violationVec = (0 until 8).map(k => data(j).mask(k) && io.storeIn(i).bits.mask(k))
        Cat(violationVec).orR() && entryNeedCheck
      }))
      val lsroqViolation = lsroqViolationVec.asUInt().orR()
      val lsroqViolationIndex = getFirstOne(lsroqViolationVec, lsroqIdxMask)
      val lsroqViolationUop = uop(lsroqViolationIndex)
      XSDebug(lsroqViolation, p"${Binary(Cat(lsroqViolationVec))}, $startIndex, $lsroqViolationIndex\n")

      // when l/s writeback to roq together, check if rollback is needed
      val wbViolationVec = VecInit((0 until LoadPipelineWidth).map(j => {
        io.loadIn(j).valid &&
          io.loadIn(j).bits.uop.isAfter(io.storeIn(i).bits.uop) &&
          io.storeIn(i).bits.paddr(PAddrBits - 1, 3) === io.loadIn(j).bits.paddr(PAddrBits - 1, 3) &&
          (io.storeIn(i).bits.mask & io.loadIn(j).bits.mask).orR
      }))
      val wbViolation = wbViolationVec.asUInt().orR()
      val wbViolationUop = getOldestInTwo(wbViolationVec, io.loadIn.map(_.bits.uop))
      XSDebug(wbViolation, p"${Binary(Cat(wbViolationVec))}, $wbViolationUop\n")

      // check if rollback is needed for load in l4
      val l4ViolationVec = VecInit((0 until LoadPipelineWidth).map(j => {
        io.forward(j).valid && // L4 valid\
          io.forward(j).uop.isAfter(io.storeIn(i).bits.uop) &&
          io.storeIn(i).bits.paddr(PAddrBits - 1, 3) === io.forward(j).paddr(PAddrBits - 1, 3) &&
          (io.storeIn(i).bits.mask & io.forward(j).mask).orR
      }))
      val l4Violation = l4ViolationVec.asUInt().orR()
      val l4ViolationUop = getOldestInTwo(l4ViolationVec, io.forward.map(_.uop))

      val rollbackValidVec = Seq(lsroqViolation, wbViolation, l4Violation)
      val rollbackUopVec = Seq(lsroqViolationUop, wbViolationUop, l4ViolationUop)
      rollback(i).valid := Cat(rollbackValidVec).orR
      val mask = getAfterMask(rollbackValidVec, rollbackUopVec)
      val oneAfterZero = mask(1)(0)
      val rollbackUop = Mux(oneAfterZero && mask(2)(0),
        rollbackUopVec(0),
        Mux(!oneAfterZero && mask(2)(1), rollbackUopVec(1), rollbackUopVec(2)))
      rollback(i).bits.roqIdx := rollbackUop.roqIdx - 1.U

      rollback(i).bits.isReplay := true.B
      rollback(i).bits.isMisPred := false.B
      rollback(i).bits.isException := false.B
      rollback(i).bits.isFlushPipe := false.B

      XSDebug(
        lsroqViolation,
        "need rollback (ld wb before store) pc %x roqidx %d target %x\n",
        io.storeIn(i).bits.uop.cf.pc, io.storeIn(i).bits.uop.roqIdx, lsroqViolationUop.roqIdx
      )
      XSDebug(
        wbViolation,
        "need rollback (ld/st wb together) pc %x roqidx %d target %x\n",
        io.storeIn(i).bits.uop.cf.pc, io.storeIn(i).bits.uop.roqIdx, wbViolationUop.roqIdx
      )
      XSDebug(
        l4Violation,
        "need rollback (l4 load) pc %x roqidx %d target %x\n",
        io.storeIn(i).bits.uop.cf.pc, io.storeIn(i).bits.uop.roqIdx, l4ViolationUop.roqIdx
      )
    }.otherwise {
      rollback(i).valid := false.B
    }
  })

  def rollbackSel(a: Valid[Redirect], b: Valid[Redirect]): ValidIO[Redirect] = {
    Mux(
      a.valid,
      Mux(
        b.valid,
        Mux(a.bits.isAfter(b.bits), b, a), // a,b both valid, sel oldest
        a // sel a
      ),
      b // sel b
    )
  }

  io.rollback := ParallelOperation(rollback, rollbackSel)

  // Memory mapped IO / other uncached operations

  // setup misc mem access req
  // mask / paddr / data can be get from lsroq.data
  val commitType = io.commits(0).bits.uop.ctrl.commitType 
  io.uncache.req.valid := pending(ringBufferTail) && allocated(ringBufferTail) &&
    (commitType === CommitType.STORE || commitType === CommitType.LOAD) && 
    io.roqDeqPtr === uop(ringBufferTail).roqIdx && 
    !io.commits(0).bits.isWalk

  io.uncache.req.bits.cmd  := Mux(store(ringBufferTail), MemoryOpConstants.M_XWR, MemoryOpConstants.M_XRD)
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

  // Read vaddr for mem exception
  val mexcLsroqIdx = WireInit(0.U(LsroqIdxWidth.W))
  val memExceptionAddr = WireInit(data(mexcLsroqIdx(InnerLsroqIdxWidth - 1, 0)).vaddr)
  ExcitingUtils.addSink(mexcLsroqIdx, "EXECPTION_LSROQIDX")
  ExcitingUtils.addSource(memExceptionAddr, "EXECPTION_VADDR")

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

  // assert(!io.rollback.valid)
  when(io.rollback.valid) {
    XSDebug("Mem rollback: pc %x roqidx %d\n", io.rollback.bits.pc, io.rollback.bits.roqIdx)
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
