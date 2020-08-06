package xiangshan.mem

import chisel3._
import chisel3.util._
import utils._
import xiangshan._
import xiangshan.cache._

class LsRoqEntry extends XSBundle {
  val paddr = UInt(PAddrBits.W)
  val op = UInt(6.W)
  val mask = UInt(8.W)
  val data = UInt(XLEN.W)
  val exception = UInt(8.W)
  // val miss = Bool()
  val mmio = Bool()
  // val store = Bool()
  val bwdMask = Vec(8, Bool()) // UInt(8.W)
  val bwdData = Vec(8, UInt(8.W))
}

// Load/Store Roq (Moq) for XiangShan Out of Order LSU
class Lsroq extends XSModule {
  val io = IO(new Bundle() {
    val dp1Req = Vec(RenameWidth, Flipped(DecoupledIO(new MicroOp)))
    val moqIdxs = Output(Vec(RenameWidth, UInt(MoqIdxWidth.W)))
    val brqRedirect = Input(Valid(new Redirect))
    val loadIn = Vec(LoadPipelineWidth, Flipped(Valid(new LsPipelineBundle)))
    val storeIn = Vec(StorePipelineWidth, Flipped(Valid(new LsPipelineBundle)))
    val sbuffer = Vec(StorePipelineWidth, Decoupled(new DCacheStoreReq))
    val ldout = Vec(2, DecoupledIO(new ExuOutput)) // writeback store
    val stout = Vec(2, DecoupledIO(new ExuOutput)) // writeback store
    val mcommit = Flipped(Vec(CommitWidth, Valid(UInt(MoqIdxWidth.W))))
    val forward = Vec(LoadPipelineWidth, Flipped(new LoadForwardQueryIO))
    val rollback = Output(Valid(new Redirect))
    val miss = Decoupled(new MissReqIO)
    val refill = Flipped(Valid(new DCacheStoreReq))
  })

  val uop = Reg(Vec(MoqSize, new MicroOp))
  val data = Reg(Vec(MoqSize, new LsRoqEntry))
  val allocated = RegInit(VecInit(List.fill(MoqSize)(false.B))) // lsroq entry has been allocated
  val valid = RegInit(VecInit(List.fill(MoqSize)(false.B))) // data is valid
  val writebacked = RegInit(VecInit(List.fill(MoqSize)(false.B))) // inst has been writebacked to CDB
  val commited = Reg(Vec(MoqSize, Bool())) // inst has been writebacked to CDB
  val store = Reg(Vec(MoqSize, Bool())) // inst is a store inst
  val miss = Reg(Vec(MoqSize, Bool())) // load inst missed, waiting for miss queue to accept miss request
  val listening = Reg(Vec(MoqSize, Bool())) // waiting foe refill result

  val ringBufferHeadExtended = RegInit(0.U(MoqIdxWidth.W))
  val ringBufferTailExtended = RegInit(0.U(MoqIdxWidth.W))
  val ringBufferHead = ringBufferHeadExtended(InnerMoqIdxWidth - 1, 0)
  val ringBufferTail = ringBufferTailExtended(InnerMoqIdxWidth - 1, 0)
  val ringBufferEmpty = ringBufferHead === ringBufferTail && ringBufferHeadExtended(InnerMoqIdxWidth) === ringBufferTailExtended(InnerMoqIdxWidth)
  val ringBufferFull = ringBufferHead === ringBufferTail && ringBufferHeadExtended(InnerMoqIdxWidth) =/= ringBufferTailExtended(InnerMoqIdxWidth)
  val ringBufferAllowin = !ringBufferFull

  // Enqueue at dispatch
  val validDispatch = VecInit((0 until RenameWidth).map(io.dp1Req(_).valid)).asUInt
  XSDebug("(ready, valid): ")
  for (i <- 0 until RenameWidth) {
    val offset = if (i == 0) 0.U else PopCount(validDispatch(i - 1, 0))
    when(io.dp1Req(i).fire()) {
      uop(ringBufferHead + offset) := io.dp1Req(i).bits
      allocated(ringBufferHead + offset) := true.B
      valid(ringBufferHead + offset) := false.B
      writebacked(ringBufferHead + offset) := false.B
      commited(ringBufferHead + offset) := false.B
      store(ringBufferHead + offset) := false.B
      miss(ringBufferHead + offset) := false.B
      listening(ringBufferHead + offset) := false.B
      data(ringBufferHead + offset).bwdMask := 0.U(8.W).asBools
    }
    if (i == 0) {
      io.dp1Req(i).ready := ringBufferAllowin && !allocated(ringBufferHead + offset)
    } else {
      io.dp1Req(i).ready := ringBufferAllowin && !allocated(ringBufferHead + offset) && io.dp1Req(i - 1).ready
    }
    io.moqIdxs(i) := ringBufferHeadExtended + offset
    XSDebug(false, true.B, "(%d, %d) ", io.dp1Req(i).ready, io.dp1Req(i).valid)
  }
  XSDebug(false, true.B, "\n")

  val firedDispatch = VecInit((0 until CommitWidth).map(io.dp1Req(_).fire())).asUInt
  when(firedDispatch.orR) {
    ringBufferHeadExtended := ringBufferHeadExtended + PopCount(firedDispatch)
    XSInfo("dispatched %d insts to moq\n", PopCount(firedDispatch))
  }

  // writeback load
  (0 until LoadPipelineWidth).map(i => {
    assert(!io.loadIn(i).bits.miss)
    when(io.loadIn(i).fire()) {
      when(io.loadIn(i).bits.miss) {
        XSInfo(io.loadIn(i).valid, "load miss write to cbd idx %d pc 0x%x vaddr %x paddr %x data %x mmio %x roll %x\n",
          io.loadIn(i).bits.uop.moqIdx,
          io.loadIn(i).bits.uop.cf.pc,
          io.loadIn(i).bits.vaddr,
          io.loadIn(i).bits.paddr,
          io.loadIn(i).bits.data,
          io.loadIn(i).bits.mmio,
          io.loadIn(i).bits.rollback
        )
      }.otherwise {
        XSInfo(io.loadIn(i).valid, "load hit write to cbd idx %d pc 0x%x vaddr %x paddr %x data %x mmio %x roll %x\n",
          io.loadIn(i).bits.uop.moqIdx,
          io.loadIn(i).bits.uop.cf.pc,
          io.loadIn(i).bits.vaddr,
          io.loadIn(i).bits.paddr,
          io.loadIn(i).bits.data,
          io.loadIn(i).bits.mmio,
          io.loadIn(i).bits.rollback
        )
      }
      valid(io.loadIn(i).bits.uop.moqIdx) := !io.loadIn(i).bits.miss
      writebacked(io.loadIn(i).bits.uop.moqIdx) := !io.loadIn(i).bits.miss
      allocated(io.loadIn(i).bits.uop.moqIdx) := io.loadIn(i).bits.miss // if hit, lsroq entry can be recycled
      data(io.loadIn(i).bits.uop.moqIdx).paddr := io.loadIn(i).bits.paddr
      data(io.loadIn(i).bits.uop.moqIdx).mask := io.loadIn(i).bits.forwardMask.asUInt // load's "data" field is used to save bypassMask/Data for missed load
      data(io.loadIn(i).bits.uop.moqIdx).data := io.loadIn(i).bits.forwardData.asUInt // load's "data" field is used to save bypassMask/Data for missed load
      data(io.loadIn(i).bits.uop.moqIdx).mmio := io.loadIn(i).bits.mmio
      miss(io.loadIn(i).bits.uop.moqIdx) := io.loadIn(i).bits.miss
      store(io.loadIn(i).bits.uop.moqIdx) := false.B
    }
  })

  // writeback store
  (0 until StorePipelineWidth).map(i => {
    when(io.storeIn(i).fire()) {
      valid(io.storeIn(i).bits.uop.moqIdx) := true.B
      data(io.storeIn(i).bits.uop.moqIdx).paddr := io.storeIn(i).bits.paddr
      data(io.storeIn(i).bits.uop.moqIdx).mask := io.storeIn(i).bits.mask
      data(io.storeIn(i).bits.uop.moqIdx).data := io.storeIn(i).bits.data
      data(io.storeIn(i).bits.uop.moqIdx).mmio := io.storeIn(i).bits.mmio
      miss(io.storeIn(i).bits.uop.moqIdx) := io.storeIn(i).bits.miss
      store(io.storeIn(i).bits.uop.moqIdx) := true.B
      XSInfo("store write to lsroq idx %d pc 0x%x vaddr %x paddr %x data %x miss %x mmio %x roll %x\n",
        io.storeIn(i).bits.uop.moqIdx(InnerMoqIdxWidth - 1, 0),
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
    (0 until MoqSize).map(i => allocated(i) && miss(i))
  )
  val missRefillSel = OHToUInt(missRefillSelVec.asUInt)
  io.miss.valid := missRefillSelVec.asUInt.orR
  io.miss.bits.paddr := data(missRefillSel).paddr
  when(io.miss.fire()) {
    miss(missRefillSel) := false.B
    listening(missRefillSel) := true.B
  }

  // get load result from refill resp
  def refillDataSel(data: UInt, offset: UInt): UInt = {
    Mux1H((0 until 8).map(p => (data(5, 3) === p.U, data(64 * (p + 1) - 1, 64 * p))))
  }

  def mergeRefillData(refill: UInt, fwd: UInt, fwdMask: UInt): UInt = {
    val res = Wire(Vec(8, UInt(8.W)))
    (0 until 8).foreach(i => {
      res(i) := Mux(fwdMask(i), fwd(8 * (i + 1) - 1, 8 * i), refill(8 * (i + 1) - 1, 8 * i))
    })
    res.asUInt
  }

  (0 until MoqSize).map(i => {
    val addrMatch = data(i).paddr(PAddrBits - 1, 6) === io.refill.bits.paddr
    when(allocated(i) && listening(i)) {
      // TODO: merge data
      val refillData = refillDataSel(io.refill.bits.data, data(i).paddr(5, 0))
      data(i).data := mergeRefillData(refillData, data(i).data, data(i).mask)
      valid(i) := true.B
      listening(i) := false.B
    }
  })

  // writeback up to 2 missed load insts to CDB
  // just randomly pick 2 missed load (data refilled), write them back to cdb
  val loadWbSelVec = VecInit((0 until MoqSize).map(i => {
    allocated(i) && valid(i) && !writebacked(i) && !store(i)
  })).asUInt() // use uint instead vec to reduce verilog lines
  val loadWbSel = Wire(Vec(StorePipelineWidth, UInt(log2Up(MoqSize).W)))
  val lselvec0 = PriorityEncoderOH(loadWbSelVec)
  val lselvec1 = PriorityEncoderOH(loadWbSelVec & (~lselvec0).asUInt)
  loadWbSel(0) := OHToUInt(lselvec0)
  loadWbSel(1) := OHToUInt(lselvec1)
  (0 until StorePipelineWidth).map(i => {
    io.ldout(i).bits.uop := uop(loadWbSel(i))
    io.ldout(i).bits.data := data(loadWbSel(i)).data
    io.ldout(i).bits.redirectValid := false.B
    io.ldout(i).bits.redirect := DontCare
    io.ldout(i).bits.brUpdate := DontCare
    io.ldout(i).bits.debug.isMMIO := data(loadWbSel(i)).mmio
    io.ldout(i).valid := loadWbSelVec(loadWbSel(i))
    when(io.ldout(i).fire()) {
      // writebacked(loadWbSel(i)) := true.B
      allocated(loadWbSel(i)) := false.B
    }
  })

  // writeback up to 2 store insts to CDB
  // just randomly pick 2 stores, write them back to cdb
  val storeWbSelVec = VecInit((0 until MoqSize).map(i => {
    allocated(i) && valid(i) && !writebacked(i) && store(i)
  })).asUInt()
  val storeWbSel = Wire(Vec(StorePipelineWidth, UInt(log2Up(MoqSize).W)))
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
  val dequeueMask = Wire(Vec(MoqSize * 2, Bool()))
  (0 until MoqSize * 2).foreach(i => {
    val ptr = i.U(InnerMoqIdxWidth - 1, 0)
    if (i == 0) {
      dequeueMask(i) := ringBufferTail === i.U && !ringBufferEmpty && !allocated(ptr) // beginning of dequeuemask
    } else {
      dequeueMask(i) := (
        dequeueMask(i - 1) && !allocated(ptr) && ringBufferHead =/= i.U(InnerMoqIdxWidth - 1, 0) ||
          ringBufferTail === i.U && !ringBufferEmpty && !allocated(ptr) // beginning of dequeuemask
        // TODO: opt timing
        )
    }
  })
  ringBufferTailExtended := ringBufferTailExtended + PopCount(dequeueMask.asUInt)

  // send commited store inst to sbuffer
  // select up to 2 writebacked store insts
  // scommitPending, scommitIn, scommitOut are for debug only
  val scommitPending = RegInit(0.U(log2Up(MoqSize).W))
  val scommitIn = PopCount(VecInit((0 until CommitWidth).map(i => io.mcommit(i).valid)).asUInt)
  val scommitOut = PopCount(VecInit((0 until 2).map(i => io.sbuffer(i).fire())).asUInt)
  scommitPending := scommitPending + scommitIn - scommitOut

  val commitedStoreQueue = Module(new MIMOQueue(
    UInt(InnerMoqIdxWidth.W),
    entries = MoqSize,
    inCnt = 6,
    outCnt = 2,
    mem = false,
    perf = true
  ))

  commitedStoreQueue.io.flush := false.B

  // When store commited, mark it as commited (will not be influenced by redirect),
  // then add store's moq ptr into commitedStoreQueue
  (0 until CommitWidth).map(i => {
    when(io.mcommit(i).valid) {
      commited(io.mcommit(i).bits) := true.B
    }
    commitedStoreQueue.io.enq(i).valid := io.mcommit(i).valid
    commitedStoreQueue.io.enq(i).bits := io.mcommit(i).bits(InnerMoqIdxWidth - 1, 0)
    // We assume commitedStoreQueue.io.enq(i).ready === true.B, 
    // for commitedStoreQueue.size = 64
  })

  // get no more than 2 commited store from storeCommitedQueue
  (0 until 2).map(i => {
    commitedStoreQueue.io.deq(i).ready := io.sbuffer(i).fire()
  })

  // send selected store inst to sbuffer
  (0 until 2).map(i => {
    val ptr = commitedStoreQueue.io.deq(i).bits
    io.sbuffer(i).valid := commitedStoreQueue.io.deq(i).valid
    io.sbuffer(i).bits.paddr := data(ptr).paddr
    io.sbuffer(i).bits.data := data(ptr).data
    io.sbuffer(i).bits.mask := data(ptr).mask
    io.sbuffer(i).bits.miss := false.B
    io.sbuffer(i).bits.user.uop := uop(ptr)
    io.sbuffer(i).bits.user.mmio := data(ptr).mmio
    io.sbuffer(i).bits.user.mask := data(ptr).mask
    io.sbuffer(i).bits.user.id := DontCare // always store
    io.sbuffer(i).bits.user.paddr := DontCare
  })

  // update lsroq meta if store inst is send to sbuffer
  (0 until 2).map(i => {
    when(io.sbuffer(i).fire()) {
      allocated(commitedStoreQueue.io.deq(i).bits) := false.B
    }
  })

  // load forward query
  (0 until LoadPipelineWidth).map(i => {
    io.forward(i).forwardMask := 0.U(8.W).asBools
    io.forward(i).forwardData := DontCare
    // Just for functional simulation

    // forward
    val needForward1 = WireInit(VecInit((0 until MoqSize).map(j => {
      io.forward(i).moqIdx(InnerMoqIdxWidth - 1, 0) > j.U &&
        (
          ringBufferTail <= j.U ||
            ringBufferTailExtended(InnerMoqIdxWidth) =/= io.forward(i).moqIdx(InnerMoqIdxWidth)
          )
    })))
    val needForward2 = WireInit(VecInit((0 until MoqSize).map(j => {
      ringBufferTail <= j.U &&
        ringBufferTailExtended(InnerMoqIdxWidth) =/= io.forward(i).moqIdx(InnerMoqIdxWidth)
    })))
    val forwardMask1 = WireInit(VecInit(Seq.fill(8)(false.B)))
    val forwardData1 = WireInit(VecInit(Seq.fill(8)(0.U(8.W))))
    val forwardMask2 = WireInit(VecInit(Seq.fill(8)(false.B)))
    val forwardData2 = WireInit(VecInit(Seq.fill(8)(0.U(8.W))))

    // forward lookup vec2
    (0 until MoqSize).map(j => {
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
    (0 until MoqSize).map(j => {
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
              data(j).data(8 * (k + 1) - 1, 8 * k), j.U, uop(j).cf.pc, io.forward(i).moqIdx, uop(io.forward(i).moqIdx(InnerMoqIdxWidth - 1, 0)).cf.pc
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

    // (1 until MoqSize).map(j => {
    //   val ptr = io.forward(i).moqIdx - j.U
    //   when(
    //     moqIdxOlderThan(ptr, io.forward(i).moqIdx) &&
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
    (0 until 8).map(k => {
      when(data(io.forward(i).moqIdx).bwdMask(k)) {
        io.forward(i).forwardMask(k) := true.B
        io.forward(i).forwardData(k) := data(io.forward(i).moqIdx).bwdData(k)
        XSDebug("backwarding " + k + "th byte %x, idx %d pc %x\n",
          io.forward(i).forwardData(k), io.forward(i).moqIdx(InnerMoqIdxWidth - 1, 0), uop(io.forward(i).moqIdx).cf.pc
        )
      }
    })
  })

  // rollback check
  val rollback = Wire(Vec(StorePipelineWidth, Valid(new Redirect)))


  // store backward query and rollback
  //  val needCheck = Seq.fill(8)(WireInit(true.B))
  (0 until StorePipelineWidth).foreach(i => {
    rollback(i) := DontCare
    when(io.storeIn(i).valid) {
      val needCheck = Seq.fill(MoqSize + 1)(Seq.fill(8)(WireInit(true.B))) // TODO: refactor

      val moqViolation = VecInit((0 until MoqSize).map(j => {
        val ptr = io.storeIn(i).bits.uop.moqIdx + j.U
        val reachHead = ptr === ringBufferHeadExtended
        val addrMatch = allocated(ptr) &&
          io.storeIn(i).bits.paddr(PAddrBits - 1, 3) === data(ptr).paddr(PAddrBits - 1, 3)
        val mask = data(ptr).mask
        val s = store(ptr)
        val w = writebacked(ptr)
        val violationVec = (0 until 8) map (k => {
          needCheck(j+1)(k) := needCheck(j)(k) && !(addrMatch && s) && !reachHead
          needCheck(j)(k) && addrMatch && mask(k) && io.storeIn(i).bits.mask(k) && !s && w
        })
        Cat(violationVec).orR()
      })).asUInt().orR()

      // when l/s writeback to roq together, check if rollback is needed
      val wbViolation = VecInit((0 until LoadPipelineWidth).map(j => {
        io.loadIn(j).valid &&
          io.loadIn(j).bits.uop.isAfter(io.storeIn(i).bits.uop) &&
          io.storeIn(i).bits.paddr(PAddrBits - 1, 3) === io.loadIn(j).bits.paddr(PAddrBits - 1, 3) &&
          (io.storeIn(i).bits.mask & io.loadIn(j).bits.mask).orR
      })).asUInt().orR()

      // check if rollback is needed for load in l4
      val l4Violation = VecInit((0 until LoadPipelineWidth).map(j => {
        // TODO: consider load store order
        io.forward(j).valid && // L4 valid
          io.storeIn(i).bits.paddr(PAddrBits - 1, 3) === io.forward(j).paddr(PAddrBits - 1, 3) &&
          (io.storeIn(i).bits.mask & io.forward(j).mask).orR
      })).asUInt().orR()

      rollback(i).valid := moqViolation || wbViolation || l4Violation


      XSDebug(
        moqViolation,
        "need rollback (ld wb before store) pc %x roqidx %d\n",
        io.storeIn(i).bits.uop.cf.pc, io.storeIn(i).bits.uop.roqIdx
      )
      XSDebug(
        wbViolation,
        "need rollback (ld/st wb together) pc %x roqidx %d\n",
        io.storeIn(i).bits.uop.cf.pc, io.storeIn(i).bits.uop.roqIdx
      )
      XSDebug(
        l4Violation,
        "need rollback (l4 load) pc %x roqidx %d\n",
        io.storeIn(i).bits.uop.cf.pc, io.storeIn(i).bits.uop.roqIdx
      )
    }.otherwise({
      rollback(i).valid := false.B
    })
    rollback(i).bits.isReplay := true.B
    rollback(i).bits.isMisPred := false.B
    rollback(i).bits.isException := false.B
    rollback(i).bits.target := io.storeIn(i).bits.uop.cf.pc
    rollback(i).bits.roqIdx := io.storeIn(i).bits.uop.roqIdx
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

  // misprediction recovery / exception redirect
  // invalidate lsroq term using robIdx
  // TODO: check exception redirect implementation
  (0 until MoqSize).map(i => {
    when(uop(i).needFlush(io.brqRedirect) && allocated(i) && !commited(i)) {
      when(io.brqRedirect.bits.isReplay){
        valid(i) := false.B
        store(i) := false.B
        writebacked(i) := false.B
        listening(i) := false.B
        miss(i) := false.B
      }.otherwise{
        allocated(i) := false.B
      }
    }
  })

  // assert(!io.rollback.valid)
  when(io.rollback.valid) {
    XSDebug("Mem rollback: pc %x roqidx %d\n", io.rollback.bits.pc, io.rollback.bits.roqIdx)
  }

  // debug info
  XSDebug("head %d:%d tail %d:%d scommit %d\n", ringBufferHeadExtended(InnerMoqIdxWidth), ringBufferHead, ringBufferTailExtended(InnerMoqIdxWidth), ringBufferTail, scommitPending)

  def PrintFlag(flag: Bool, name: String): Unit = {
    when(flag) {
      XSDebug(false, true.B, name)
    }.otherwise {
      XSDebug(false, true.B, " ")
    }
  }

  for (i <- 0 until MoqSize) {
    if (i % 4 == 0) XSDebug("")
    XSDebug(false, true.B, "%x ", uop(i).cf.pc)
    PrintFlag(allocated(i), "a")
    PrintFlag(allocated(i) && valid(i), "v")
    PrintFlag(allocated(i) && writebacked(i), "w")
    PrintFlag(allocated(i) && commited(i), "c")
    PrintFlag(allocated(i) && store(i), "s")
    PrintFlag(allocated(i) && miss(i), "m")
    PrintFlag(allocated(i) && listening(i), "l")
    XSDebug(false, true.B, " ")
    if (i % 4 == 3) XSDebug(false, true.B, "\n")
  }

}
