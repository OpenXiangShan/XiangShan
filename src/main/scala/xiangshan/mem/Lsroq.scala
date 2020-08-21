package xiangshan.mem

import chisel3._
import chisel3.util._
import utils._
import xiangshan._
import xiangshan.cache._
import xiangshan.cache.{DCacheLoadIO, TlbRequestIO, MemoryOpConstants}

class LsRoqEntry extends XSBundle {
  val paddr = UInt(PAddrBits.W)
  val op = UInt(6.W)
  val mask = UInt(8.W)
  val data = UInt(XLEN.W)
  val exception = UInt(8.W)
  // val miss = Bool()
  val mmio = Bool()
  // val store = Bool()
  // val bwdMask = Vec(8, Bool()) // UInt(8.W)
  // val bwdData = Vec(8, UInt(8.W))
  val fwdMask = Vec(8, Bool())
  val fwdData = Vec(8, UInt(8.W))
}

// Load/Store Roq (Lsroq) for XiangShan Out of Order LSU
class Lsroq extends XSModule {
  val io = IO(new Bundle() {
    val dp1Req = Vec(RenameWidth, Flipped(DecoupledIO(new MicroOp)))
    val lsroqIdxs = Output(Vec(RenameWidth, UInt(LsroqIdxWidth.W)))
    val brqRedirect = Input(Valid(new Redirect))
    val loadIn = Vec(LoadPipelineWidth, Flipped(Valid(new LsPipelineBundle)))
    val storeIn = Vec(StorePipelineWidth, Flipped(Valid(new LsPipelineBundle)))
    val sbuffer = Vec(StorePipelineWidth, Decoupled(new DCacheWordReq))
    val ldout = Vec(2, DecoupledIO(new ExuOutput)) // writeback store
    val stout = Vec(2, DecoupledIO(new ExuOutput)) // writeback store
    val forward = Vec(LoadPipelineWidth, Flipped(new LoadForwardQueryIO))
    val commits = Flipped(Vec(CommitWidth, Valid(new RoqCommit)))
    val rollback = Output(Valid(new Redirect))
    val dcache = new DCacheLoadIO
    val uncache = new DCacheLoadIO
    // val refill = Flipped(Valid(new DCacheLineReq ))
  })
  
  val uop = Reg(Vec(LsroqSize, new MicroOp))
  val data = Reg(Vec(LsroqSize, new LsRoqEntry))
  val allocated = RegInit(VecInit(List.fill(LsroqSize)(false.B))) // lsroq entry has been allocated
  val valid = RegInit(VecInit(List.fill(LsroqSize)(false.B))) // data is valid
  val writebacked = RegInit(VecInit(List.fill(LsroqSize)(false.B))) // inst has been writebacked to CDB
  val commited = Reg(Vec(LsroqSize, Bool())) // inst has been writebacked to CDB
  val store = Reg(Vec(LsroqSize, Bool())) // inst is a store inst
  val miss = Reg(Vec(LsroqSize, Bool())) // load inst missed, waiting for miss queue to accept miss request
  val listening = Reg(Vec(LsroqSize, Bool())) // waiting for refill result
  val pending = Reg(Vec(LsroqSize, Bool())) // mmio pending: inst is an mmio inst, it will not be executed until it reachs the end of roq
  
  val ringBufferHeadExtended = RegInit(0.U(LsroqIdxWidth.W))
  val ringBufferTailExtended = RegInit(0.U(LsroqIdxWidth.W))
  val ringBufferHead = ringBufferHeadExtended(InnerLsroqIdxWidth - 1, 0)
  val ringBufferTail = ringBufferTailExtended(InnerLsroqIdxWidth - 1, 0)
  val ringBufferEmpty = ringBufferHead === ringBufferTail && ringBufferHeadExtended(InnerLsroqIdxWidth) === ringBufferTailExtended(InnerLsroqIdxWidth)
  val ringBufferFull = ringBufferHead === ringBufferTail && ringBufferHeadExtended(InnerLsroqIdxWidth) =/= ringBufferTailExtended(InnerLsroqIdxWidth)
  val ringBufferAllowin = !ringBufferFull
  
  val storeCommit = (0 until CommitWidth).map(i => io.commits(i).valid && !io.commits(i).bits.isWalk && io.commits(i).bits.uop.ctrl.commitType === CommitType.STORE)
  val loadCommit = (0 until CommitWidth).map(i => io.commits(i).valid && !io.commits(i).bits.isWalk && io.commits(i).bits.uop.ctrl.commitType === CommitType.LOAD)
  val mcommitIdx = (0 until CommitWidth).map(i => io.commits(i).bits.uop.lsroqIdx(InnerLsroqIdxWidth-1,0))
  
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
      store(index) := false.B
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
        XSInfo(io.loadIn(i).valid, "load miss write to lsroq idx %d pc 0x%x vaddr %x paddr %x data %x mask %x forwardData %x forwardMask: %x mmio %x roll %x\n",
          io.loadIn(i).bits.uop.lsroqIdx,
          io.loadIn(i).bits.uop.cf.pc,
          io.loadIn(i).bits.vaddr,
          io.loadIn(i).bits.paddr,
          io.loadIn(i).bits.data,
          io.loadIn(i).bits.mask,
          io.loadIn(i).bits.forwardData.asUInt,
          io.loadIn(i).bits.forwardMask.asUInt,
          io.loadIn(i).bits.mmio,
          io.loadIn(i).bits.rollback
        )
      }.otherwise {
        XSInfo(io.loadIn(i).valid, "load hit write to cbd idx %d pc 0x%x vaddr %x paddr %x data %x mask %x forwardData %x forwardMask: %x mmio %x roll %x\n",
          io.loadIn(i).bits.uop.lsroqIdx,
          io.loadIn(i).bits.uop.cf.pc,
          io.loadIn(i).bits.vaddr,
          io.loadIn(i).bits.paddr,
          io.loadIn(i).bits.data,
          io.loadIn(i).bits.mask,
          io.loadIn(i).bits.forwardData.asUInt,
          io.loadIn(i).bits.forwardMask.asUInt,
          io.loadIn(i).bits.mmio,
          io.loadIn(i).bits.rollback
        )
      }
      valid(io.loadIn(i).bits.uop.lsroqIdx) := !io.loadIn(i).bits.miss && !io.loadIn(i).bits.mmio
      writebacked(io.loadIn(i).bits.uop.lsroqIdx) := !io.loadIn(i).bits.miss && !io.loadIn(i).bits.mmio
      // allocated(io.loadIn(i).bits.uop.lsroqIdx) := io.loadIn(i).bits.miss // if hit, lsroq entry can be recycled
      data(io.loadIn(i).bits.uop.lsroqIdx).paddr := io.loadIn(i).bits.paddr
      data(io.loadIn(i).bits.uop.lsroqIdx).mask := io.loadIn(i).bits.mask
      data(io.loadIn(i).bits.uop.lsroqIdx).data := io.loadIn(i).bits.data // for mmio / misc / debug
      data(io.loadIn(i).bits.uop.lsroqIdx).mmio := io.loadIn(i).bits.mmio
      data(io.loadIn(i).bits.uop.lsroqIdx).fwdMask := io.loadIn(i).bits.forwardMask
      data(io.loadIn(i).bits.uop.lsroqIdx).fwdData := io.loadIn(i).bits.forwardData
      miss(io.loadIn(i).bits.uop.lsroqIdx) := io.loadIn(i).bits.miss && !io.loadIn(i).bits.mmio
      store(io.loadIn(i).bits.uop.lsroqIdx) := false.B
      pending(io.loadIn(i).bits.uop.lsroqIdx) := io.loadIn(i).bits.mmio
    }
  })

  // writeback store
  (0 until StorePipelineWidth).map(i => {
    when(io.storeIn(i).fire()) {
      valid(io.storeIn(i).bits.uop.lsroqIdx) := !io.storeIn(i).bits.mmio
      data(io.storeIn(i).bits.uop.lsroqIdx).paddr := io.storeIn(i).bits.paddr
      data(io.storeIn(i).bits.uop.lsroqIdx).mask := io.storeIn(i).bits.mask
      data(io.storeIn(i).bits.uop.lsroqIdx).data := io.storeIn(i).bits.data
      data(io.storeIn(i).bits.uop.lsroqIdx).mmio := io.storeIn(i).bits.mmio
      miss(io.storeIn(i).bits.uop.lsroqIdx) := io.storeIn(i).bits.miss
      store(io.storeIn(i).bits.uop.lsroqIdx) := true.B
      pending(io.storeIn(i).bits.uop.lsroqIdx) := io.storeIn(i).bits.mmio
      XSInfo("store write to lsroq idx %d pc 0x%x vaddr %x paddr %x data %x miss %x mmio %x roll %x\n",
        io.storeIn(i).bits.uop.lsroqIdx(InnerLsroqIdxWidth - 1, 0),
        io.storeIn(i).bits.uop.cf.pc,
        io.storeIn(i).bits.vaddr,
        io.storeIn(i).bits.paddr,
        io.storeIn(i).bits.data,
        io.storeIn(i).bits.miss,
        io.storeIn(i).bits.mmio,
        io.storeIn(i).bits.rollback
      )
    }
  })

  // cache miss request
  val missRefillSelVec = VecInit(
    (0 until LsroqSize).map(i => allocated(i) && miss(i))
  )
  val missRefillSel = PriorityEncoder(missRefillSelVec.asUInt)
  io.dcache.req.valid := missRefillSelVec.asUInt.orR
  io.dcache.req.bits.cmd := MemoryOpConstants.M_XRD
  io.dcache.req.bits.addr := data(missRefillSel).paddr
  io.dcache.req.bits.data := DontCare
  io.dcache.req.bits.mask := data(missRefillSel).mask

  io.dcache.req.bits.meta.id       := DCacheMiscType.miss
  io.dcache.req.bits.meta.vaddr    := DontCare // data(missRefillSel).vaddr
  io.dcache.req.bits.meta.paddr    := data(missRefillSel).paddr
  io.dcache.req.bits.meta.uop      := uop(missRefillSel)
  io.dcache.req.bits.meta.mmio     := false.B // data(missRefillSel).mmio
  io.dcache.req.bits.meta.tlb_miss := false.B
  io.dcache.req.bits.meta.mask     := data(missRefillSel).mask
  io.dcache.req.bits.meta.replay   := false.B

  io.dcache.resp.ready := true.B
  io.dcache.s1_kill := false.B

  assert(!(data(missRefillSel).mmio && io.dcache.req.valid))

  when(io.dcache.req.fire()) {
    miss(missRefillSel) := false.B
    listening(missRefillSel) := true.B
  }

  when(io.dcache.req.fire()){
    XSDebug("miss req: pc:0x%x roqIdx:%d lsroqIdx:%d (p)addr:0x%x vaddr:0x%x\n", io.dcache.req.bits.meta.uop.cf.pc, io.dcache.req.bits.meta.uop.roqIdx, io.dcache.req.bits.meta.uop.lsroqIdx, io.dcache.req.bits.addr, io.dcache.req.bits.meta.vaddr) 
  }

  when(io.dcache.resp.fire()){
    XSDebug("miss resp: pc:0x%x roqIdx:%d lsroqIdx:%d (p)addr:0x%x data %x\n", io.dcache.resp.bits.meta.uop.cf.pc, io.dcache.resp.bits.meta.uop.roqIdx, io.dcache.resp.bits.meta.uop.lsroqIdx, io.dcache.resp.bits.meta.paddr, io.dcache.resp.bits.data) 
  }

  // get load result from refill resp
  // Refill a line in 1 cycle
  // def refillDataSel(data: UInt, offset: UInt): UInt = {
  //   Mux1H((0 until 8).map(p => (data(5, 3) === p.U, data(64 * (p + 1) - 1, 64 * p))))
  // }

  // def mergeRefillData(refill: UInt, fwd: UInt, fwdMask: UInt): UInt = {
  //   val res = Wire(Vec(8, UInt(8.W)))
  //   (0 until 8).foreach(i => {
  //     res(i) := Mux(fwdMask(i), fwd(8 * (i + 1) - 1, 8 * i), refill(8 * (i + 1) - 1, 8 * i))
  //   })
  //   res.asUInt
  // }

  // (0 until LsroqSize).map(i => {
  //   val addrMatch = data(i).paddr(PAddrBits - 1, 6) === io.refill.bits.meta.paddr
  //   when(allocated(i) && listening(i) && addrMatch && io.dcache.resp.fire()) {
  //     // TODO: merge data
  //     // val refillData = refillDataSel(io.refill.bits.data, data(i).paddr(5, 0))
  //     // data(i).data := mergeRefillData(refillData, data(i).data, data(i).mask)
  //     data(i).data := refillDataSel(io.refill.bits.data, data(i).paddr(5, 0)) // TODO: forward refill data
  //     valid(i) := true.B
  //     listening(i) := false.B
  //   }
  // })

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
    val addrMatch = data(i).paddr(PAddrBits - 1, 3) === io.dcache.resp.bits.meta.paddr(PAddrBits - 1, 3)
    when(allocated(i) && listening(i) && addrMatch && io.dcache.resp.fire()) {
      val refillData = io.dcache.resp.bits.data
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
        LSUOpType.lwu  -> ZeroExt(rdataSel(31, 0), XLEN),
        LSUOpType.ldu  -> ZeroExt(rdataSel(63, 0), XLEN)
    ))
    io.ldout(i).bits.uop := uop(loadWbSel(i))
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

  // writeback up to 2 store insts to CDB
  // just randomly pick 2 stores, write them back to cdb
  val storeWbSelVec = VecInit((0 until LsroqSize).map(i => {
    allocated(i) && valid(i) && !writebacked(i) && store(i)
  })).asUInt()
  val storeWbSel = Wire(Vec(StorePipelineWidth, UInt(log2Up(LsroqSize).W)))
  val storeWbValid = Wire(Vec(StorePipelineWidth, Bool()))
  val sselvec0 = PriorityEncoderOH(storeWbSelVec)
  val sselvec1 = PriorityEncoderOH(storeWbSelVec & (~sselvec0).asUInt)
  storeWbSel(0) := OHToUInt(sselvec0)
  storeWbSel(1) := OHToUInt(sselvec1)
  storeWbValid(0) := sselvec0.orR
  storeWbValid(1) := sselvec1.orR

  (0 until StorePipelineWidth).map(i => {
    io.stout(i).bits.uop := uop(storeWbSel(i))
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
  // FIXME: opt size using OH -> Mask
  val dequeueMask = Wire(Vec(LsroqSize * 2, Bool()))
  (0 until LsroqSize * 2).foreach(i => {
    val ptr = i.U(InnerLsroqIdxWidth - 1, 0)
    if (i == 0) {
      dequeueMask(i) := ringBufferTail === i.U && !ringBufferEmpty && !allocated(ptr) // beginning of dequeuemask
    } else {
      dequeueMask(i) := (
        dequeueMask(i - 1) && !allocated(ptr) && ringBufferHead =/= i.U(InnerLsroqIdxWidth - 1, 0) ||
          ringBufferTail === i.U && !ringBufferEmpty && !allocated(ptr) // beginning of dequeuemask
        // TODO: opt timing
        )
    }
  })
  ringBufferTailExtended := ringBufferTailExtended + PopCount(dequeueMask.asUInt)

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

  // scommit counter for debugging
  val scommitPending = RegInit(0.U(log2Up(LsroqSize).W))
  val scommitIn = PopCount(VecInit(storeCommit).asUInt)
  val scommitOut = PopCount(VecInit((0 until 2).map(i => commitedStoreQueue.io.deq(i).fire())).asUInt)
  scommitPending := scommitPending + scommitIn - scommitOut

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

  // When load commited, mark it as !allocated, this entry will be recycled later
  (0 until CommitWidth).map(i => {
    when(loadCommit(i)) {
      allocated(mcommitIdx(i)) := false.B
      XSDebug("load commit %d: idx %d %x\n", i.U, mcommitIdx(i), uop(mcommitIdx(i)).cf.pc)
    }
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

  // load forward query
  (0 until LoadPipelineWidth).map(i => {
    io.forward(i).forwardMask := 0.U(8.W).asBools
    io.forward(i).forwardData := DontCare
    // Just for functional simulation

    // forward
    val needForward1 = WireInit(VecInit((0 until LsroqSize).map(j => {
      io.forward(i).lsroqIdx(InnerLsroqIdxWidth - 1, 0) > j.U &&
        (
          ringBufferTail <= j.U ||
            ringBufferTailExtended(InnerLsroqIdxWidth) =/= io.forward(i).lsroqIdx(InnerLsroqIdxWidth)
          )
    })))
    val needForward2 = WireInit(VecInit((0 until LsroqSize).map(j => {
      ringBufferTail <= j.U &&
        ringBufferTailExtended(InnerLsroqIdxWidth) =/= io.forward(i).lsroqIdx(InnerLsroqIdxWidth)
    })))
    val forwardMask1 = WireInit(VecInit(Seq.fill(8)(false.B)))
    val forwardData1 = WireInit(VecInit(Seq.fill(8)(0.U(8.W))))
    val forwardMask2 = WireInit(VecInit(Seq.fill(8)(false.B)))
    val forwardData2 = WireInit(VecInit(Seq.fill(8)(0.U(8.W))))

    // forward lookup vec2
    (0 until LsroqSize).map(j => {
      when(
        needForward2(j) &&
          valid(j) && allocated(j) && store(j) &&
          io.forward(i).paddr(PAddrBits - 1, 3) === data(j).paddr(PAddrBits - 1, 3)
      ) {
        (0 until 8).map(k => {
          when(data(j).mask(k)) {
            forwardMask2(k) := true.B
            forwardData2(k) := data(j).data(8 * (k + 1) - 1, 8 * k)
            XSDebug("forwarding " + k + "th byte %x from ptr %d pc %x\n",
              data(j).data(8 * (k + 1) - 1, 8 * k), j.U, uop(j).cf.pc
            )
          }
        })
      }
    })
    // forward lookup vec1
    (0 until LsroqSize).map(j => {
      when(
        needForward1(j) &&
          valid(j) && allocated(j) && store(j) &&
          io.forward(i).paddr(PAddrBits - 1, 3) === data(j).paddr(PAddrBits - 1, 3)
      ) {
        (0 until 8).map(k => {
          when(data(j).mask(k)) {
            forwardMask1(k) := true.B
            forwardData1(k) := data(j).data(8 * (k + 1) - 1, 8 * k)
            XSDebug("forwarding " + k + "th byte %x from ptr %d pc %x, idx %d pc %x\n",
              data(j).data(8 * (k + 1) - 1, 8 * k), j.U, uop(j).cf.pc, io.forward(i).lsroqIdx, uop(io.forward(i).lsroqIdx(InnerLsroqIdxWidth - 1, 0)).cf.pc
            )
          }
        })
      }
    })
    // merge forward lookup results
    (0 until 8).map(k => {
      io.forward(i).forwardMask(k) := forwardMask1(k) || forwardMask2(k)
      io.forward(i).forwardData(k) := Mux(forwardMask1(k), forwardData1(k), forwardData2(k))
    })

    // (1 until LsroqSize).map(j => {
    //   val ptr = io.forward(i).lsroqIdx - j.U
    //   when(
    //     lsroqIdxOlderThan(ptr, io.forward(i).lsroqIdx) &&
    //     valid(ptr) && allocated(ptr) && store(ptr) &&
    //     io.forward(i).paddr(PAddrBits-1, 3) === data(ptr).paddr(PAddrBits-1, 3)
    //   ){
    //     (0 until 8).map(k => {
    //       // when(data(ptr).mask(k) && io.forward(i).mask(k)){
    //         when(data(ptr).mask(k)){
    //           io.forward(i).forwardMask(k) := true.B
    //           io.forward(i).forwardData(k) := data(ptr).data(8*(k+1)-1, 8*k)
    //           XSDebug("forwarding "+k+"th byte %x from ptr %d pc %x\n",
    //           io.forward(i).forwardData(k), ptr, uop(ptr).cf.pc
    //           )
    //         }
    //       })
    //     }
    //   })

    // backward
    // (0 until 8).map(k => {
    //   when(data(io.forward(i).lsroqIdx).bwdMask(k)) {
    //     io.forward(i).forwardMask(k) := true.B
    //     io.forward(i).forwardData(k) := data(io.forward(i).lsroqIdx).bwdData(k)
    //     XSDebug("backwarding " + k + "th byte %x, idx %d pc %x\n",
    //       io.forward(i).forwardData(k), io.forward(i).lsroqIdx(InnerLsroqIdxWidth - 1, 0), uop(io.forward(i).lsroqIdx).cf.pc
    //     )
    //   }
    // })
  })

  // rollback check
  val rollback = Wire(Vec(StorePipelineWidth, Valid(new Redirect)))

  def getFirstOne(mask: Vec[Bool], start: UInt) = {
    val length = mask.length
    val lowMask = (1.U((length + 1).W) << start).asUInt() - 1.U
    val highBits = (0 until length).map(i => mask(i) & ~lowMask(i))
    val highBitsUint = Cat(highBits.reverse)
    PriorityEncoder(Mux(highBitsUint.orR(), highBitsUint, mask.asUInt))
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
      val toEnqPtrMask = rangeMask(io.storeIn(i).bits.uop.lsroqIdx, ringBufferHeadExtended)
      val lsroqViolationVec = VecInit((0 until LsroqSize).map(j => {
        val addrMatch = allocated(j) &&
          io.storeIn(i).bits.paddr(PAddrBits - 1, 3) === data(j).paddr(PAddrBits - 1, 3)
        val entryNeedCheck = toEnqPtrMask(j) && addrMatch && !store(j) && valid(j)
        // TODO: update refilled data
        val violationVec = (0 until 8).map(k => data(j).mask(k) && io.storeIn(i).bits.mask(k))
        Cat(violationVec).orR() && entryNeedCheck
      }))
      val lsroqViolation = lsroqViolationVec.asUInt().orR()
      val lsroqViolationIndex = getFirstOne(lsroqViolationVec, startIndex)
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
  io.uncache.req.valid := pending(ringBufferTail) && allocated(ringBufferTail) &&
    io.commits(0).bits.uop.lsroqIdx === ringBufferTailExtended && 
    !io.commits(0).bits.isWalk

  io.uncache.req.bits.cmd  := Mux(store(ringBufferTail), MemoryOpConstants.M_XWR, MemoryOpConstants.M_XRD)
  io.uncache.req.bits.addr := data(ringBufferTail).paddr 
  io.uncache.req.bits.data := data(ringBufferTail).data
  io.uncache.req.bits.mask := data(ringBufferTail).mask

  io.uncache.req.bits.meta.id       := DCacheMiscType.mmio
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
    XSDebug("uncache req: pc %x addr %x data %x op %x mask %x\n", uop(missRefillSel).cf.pc, io.dcache.req.bits.addr, io.uncache.req.bits.data, io.uncache.req.bits.cmd, io.uncache.req.bits.mask) 
  }

  when(io.uncache.resp.fire()){
    XSDebug("uncache resp: data %x\n", io.dcache.resp.bits.data) 
  }

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
