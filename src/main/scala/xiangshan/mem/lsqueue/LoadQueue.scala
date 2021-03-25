package xiangshan.mem

import chisel3._
import chisel3.util._
import freechips.rocketchip.tile.HasFPUParameters
import utils._
import xiangshan._
import xiangshan.cache._
import xiangshan.cache.{DCacheLineIO, DCacheWordIO, MemoryOpConstants, TlbRequestIO}
import xiangshan.backend.LSUOpType
import xiangshan.mem._
import xiangshan.backend.roq.RoqLsqIO
import xiangshan.backend.fu.HasExceptionNO


class LqPtr extends CircularQueuePtr(LqPtr.LoadQueueSize) { }

object LqPtr extends HasXSParameter {
  def apply(f: Bool, v: UInt): LqPtr = {
    val ptr = Wire(new LqPtr)
    ptr.flag := f
    ptr.value := v
    ptr
  }
}

trait HasFpLoadHelper { this: HasFPUParameters =>
  def fpRdataHelper(uop: MicroOp, rdata: UInt): UInt = {
    LookupTree(uop.ctrl.fuOpType, List(
      LSUOpType.lw   -> recode(rdata(31, 0), S),
      LSUOpType.ld   -> recode(rdata(63, 0), D)
    ))
  }
}
trait HasLoadHelper { this: XSModule =>
  def rdataHelper(uop: MicroOp, rdata: UInt): UInt = {
    val fpWen = uop.ctrl.fpWen
    LookupTree(uop.ctrl.fuOpType, List(
      LSUOpType.lb   -> SignExt(rdata(7, 0) , XLEN),
      LSUOpType.lh   -> SignExt(rdata(15, 0), XLEN),
      LSUOpType.lw   -> Mux(fpWen, Cat(Fill(32, 1.U(1.W)), rdata(31, 0)), SignExt(rdata(31, 0), XLEN)),
      LSUOpType.ld   -> Mux(fpWen, rdata, SignExt(rdata(63, 0), XLEN)),
      LSUOpType.lbu  -> ZeroExt(rdata(7, 0) , XLEN),
      LSUOpType.lhu  -> ZeroExt(rdata(15, 0), XLEN),
      LSUOpType.lwu  -> ZeroExt(rdata(31, 0), XLEN),
    ))
  }
}

class LqEnqIO extends XSBundle {
  val canAccept = Output(Bool())
  val sqCanAccept = Input(Bool())
  val needAlloc = Vec(RenameWidth, Input(Bool()))
  val req = Vec(RenameWidth, Flipped(ValidIO(new MicroOp)))
  val resp = Vec(RenameWidth, Output(new LqPtr))
}

// Load Queue
class LoadQueue extends XSModule
  with HasDCacheParameters
  with HasCircularQueuePtrHelper
  with HasLoadHelper
  with HasExceptionNO
{
  val io = IO(new Bundle() {
    val enq = new LqEnqIO
    val brqRedirect = Flipped(ValidIO(new Redirect))
    val flush = Input(Bool())
    val loadIn = Vec(LoadPipelineWidth, Flipped(Valid(new LsPipelineBundle)))
    val storeIn = Vec(StorePipelineWidth, Flipped(Valid(new LsPipelineBundle)))
    val loadDataForwarded = Vec(LoadPipelineWidth, Input(Bool()))
    val needReplayFromRS = Vec(LoadPipelineWidth, Input(Bool()))
    val ldout = Vec(2, DecoupledIO(new ExuOutput)) // writeback int load
    val load_s1 = Vec(LoadPipelineWidth, Flipped(new MaskedLoadForwardQueryIO))
    val roq = Flipped(new RoqLsqIO)
    val rollback = Output(Valid(new Redirect)) // replay now starts from load instead of store
    val dcache = Flipped(ValidIO(new Refill))
    val uncache = new DCacheWordIO
    val exceptionAddr = new ExceptionAddrIO
  })

  val uop = Reg(Vec(LoadQueueSize, new MicroOp))
  // val data = Reg(Vec(LoadQueueSize, new LsRoqEntry))
  val dataModule = Module(new LoadQueueData(LoadQueueSize, wbNumRead = LoadPipelineWidth, wbNumWrite = LoadPipelineWidth))
  dataModule.io := DontCare
  val vaddrModule = Module(new SyncDataModuleTemplate(UInt(VAddrBits.W), LoadQueueSize, numRead = 1, numWrite = LoadPipelineWidth))
  vaddrModule.io := DontCare
  val allocated = RegInit(VecInit(List.fill(LoadQueueSize)(false.B))) // lq entry has been allocated
  val datavalid = RegInit(VecInit(List.fill(LoadQueueSize)(false.B))) // data is valid
  val writebacked = RegInit(VecInit(List.fill(LoadQueueSize)(false.B))) // inst has been writebacked to CDB
  val miss = Reg(Vec(LoadQueueSize, Bool())) // load inst missed, waiting for miss queue to accept miss request
  // val listening = Reg(Vec(LoadQueueSize, Bool())) // waiting for refill result
  val pending = Reg(Vec(LoadQueueSize, Bool())) // mmio pending: inst is an mmio inst, it will not be executed until it reachs the end of roq

  val debug_mmio = Reg(Vec(LoadQueueSize, Bool())) // mmio: inst is an mmio inst
  val debug_paddr = Reg(Vec(LoadQueueSize, UInt(PAddrBits.W))) // mmio: inst is an mmio inst

  val enqPtrExt = RegInit(VecInit((0 until RenameWidth).map(_.U.asTypeOf(new LqPtr))))
  val deqPtrExt = RegInit(0.U.asTypeOf(new LqPtr))
  val deqPtrExtNext = Wire(new LqPtr)
  val allowEnqueue = RegInit(true.B)

  val enqPtr = enqPtrExt(0).value
  val deqPtr = deqPtrExt.value

  val deqMask = UIntToMask(deqPtr, LoadQueueSize)
  val enqMask = UIntToMask(enqPtr, LoadQueueSize)

  val commitCount = RegNext(io.roq.lcommit)

  /**
    * Enqueue at dispatch
    *
    * Currently, LoadQueue only allows enqueue when #emptyEntries > RenameWidth(EnqWidth)
    */
  io.enq.canAccept := allowEnqueue

  for (i <- 0 until RenameWidth) {
    val offset = if (i == 0) 0.U else PopCount(io.enq.needAlloc.take(i))
    val lqIdx = enqPtrExt(offset)
    val index = lqIdx.value
    when (io.enq.req(i).valid && io.enq.canAccept && io.enq.sqCanAccept && !(io.brqRedirect.valid || io.flush)) {
      uop(index) := io.enq.req(i).bits
      allocated(index) := true.B
      datavalid(index) := false.B
      writebacked(index) := false.B
      miss(index) := false.B
      // listening(index) := false.B
      pending(index) := false.B
    }
    io.enq.resp(i) := lqIdx
  }
  XSDebug(p"(ready, valid): ${io.enq.canAccept}, ${Binary(Cat(io.enq.req.map(_.valid)))}\n")

  /**
    * Writeback load from load units
    *
    * Most load instructions writeback to regfile at the same time.
    * However,
    *   (1) For an mmio instruction with exceptions, it writes back to ROB immediately.
    *   (2) For an mmio instruction without exceptions, it does not write back.
    * The mmio instruction will be sent to lower level when it reaches ROB's head.
    * After uncache response, it will write back through arbiter with loadUnit.
    *   (3) For cache misses, it is marked miss and sent to dcache later.
    * After cache refills, it will write back through arbiter with loadUnit.
    */
  for (i <- 0 until LoadPipelineWidth) {
    dataModule.io.wb.wen(i) := false.B
    val loadWbIndex = io.loadIn(i).bits.uop.lqIdx.value
    when(io.loadIn(i).fire()) {
      when(io.loadIn(i).bits.miss) {
        XSInfo(io.loadIn(i).valid, "load miss write to lq idx %d pc 0x%x vaddr %x paddr %x data %x mask %x forwardData %x forwardMask: %x mmio %x\n",
          io.loadIn(i).bits.uop.lqIdx.asUInt,
          io.loadIn(i).bits.uop.cf.pc,
          io.loadIn(i).bits.vaddr,
          io.loadIn(i).bits.paddr,
          io.loadIn(i).bits.data,
          io.loadIn(i).bits.mask,
          io.loadIn(i).bits.forwardData.asUInt,
          io.loadIn(i).bits.forwardMask.asUInt,
          io.loadIn(i).bits.mmio
        )
      }.otherwise {
        XSInfo(io.loadIn(i).valid, "load hit write to cbd lqidx %d pc 0x%x vaddr %x paddr %x data %x mask %x forwardData %x forwardMask: %x mmio %x\n",
        io.loadIn(i).bits.uop.lqIdx.asUInt,
        io.loadIn(i).bits.uop.cf.pc,
        io.loadIn(i).bits.vaddr,
        io.loadIn(i).bits.paddr,
        io.loadIn(i).bits.data,
        io.loadIn(i).bits.mask,
        io.loadIn(i).bits.forwardData.asUInt,
        io.loadIn(i).bits.forwardMask.asUInt,
        io.loadIn(i).bits.mmio
      )}
      datavalid(loadWbIndex) := (!io.loadIn(i).bits.miss || io.loadDataForwarded(i)) && 
        !io.loadIn(i).bits.mmio && // mmio data is not valid until we finished uncache access
        !io.needReplayFromRS(i) // do not writeback if that inst will be resend from rs
      writebacked(loadWbIndex) := !io.loadIn(i).bits.miss && !io.loadIn(i).bits.mmio

      val loadWbData = Wire(new LQDataEntry)
      loadWbData.paddr := io.loadIn(i).bits.paddr
      loadWbData.mask := io.loadIn(i).bits.mask
      loadWbData.data := io.loadIn(i).bits.forwardData.asUInt // fwd data
      loadWbData.fwdMask := io.loadIn(i).bits.forwardMask
      dataModule.io.wbWrite(i, loadWbIndex, loadWbData)
      dataModule.io.wb.wen(i) := true.B


      debug_mmio(loadWbIndex) := io.loadIn(i).bits.mmio
      debug_paddr(loadWbIndex) := io.loadIn(i).bits.paddr

      val dcacheMissed = io.loadIn(i).bits.miss && !io.loadIn(i).bits.mmio
      miss(loadWbIndex) := dcacheMissed && !io.loadDataForwarded(i) && !io.needReplayFromRS(i)
      pending(loadWbIndex) := io.loadIn(i).bits.mmio
      uop(loadWbIndex).debugInfo.issueTime := io.loadIn(i).bits.uop.debugInfo.issueTime
    }
    // vaddrModule write is delayed, as vaddrModule will not be read right after write
    vaddrModule.io.waddr(i) := RegNext(loadWbIndex)
    vaddrModule.io.wdata(i) := RegNext(io.loadIn(i).bits.vaddr)
    vaddrModule.io.wen(i) := RegNext(io.loadIn(i).fire())
  }

  when(io.dcache.valid) {
    XSDebug("miss resp: paddr:0x%x data %x\n", io.dcache.bits.addr, io.dcache.bits.data)
  }

  // Refill 64 bit in a cycle
  // Refill data comes back from io.dcache.resp
  dataModule.io.refill.valid := io.dcache.valid
  dataModule.io.refill.paddr := io.dcache.bits.addr
  dataModule.io.refill.data := io.dcache.bits.data

  (0 until LoadQueueSize).map(i => {
    dataModule.io.refill.refillMask(i) := allocated(i) && miss(i)
    when(dataModule.io.refill.valid && dataModule.io.refill.refillMask(i) && dataModule.io.refill.matchMask(i)) {
      datavalid(i) := true.B
      miss(i) := false.B
    }
  })

  // Writeback up to 2 missed load insts to CDB
  //
  // Pick 2 missed load (data refilled), write them back to cdb
  // 2 refilled load will be selected from even/odd entry, separately

  // Stage 0
  // Generate writeback indexes

  def getEvenBits(input: UInt): UInt = {
    require(input.getWidth == LoadQueueSize)
    VecInit((0 until LoadQueueSize/2).map(i => {input(2*i)})).asUInt
  }
  def getOddBits(input: UInt): UInt = {
    require(input.getWidth == LoadQueueSize)
    VecInit((0 until LoadQueueSize/2).map(i => {input(2*i+1)})).asUInt
  }

  val loadWbSel = Wire(Vec(LoadPipelineWidth, UInt(log2Up(LoadQueueSize).W))) // index selected last cycle
  val loadWbSelV = Wire(Vec(LoadPipelineWidth, Bool())) // index selected in last cycle is valid

  val loadWbSelVec = VecInit((0 until LoadQueueSize).map(i => {
    allocated(i) && !writebacked(i) && datavalid(i)
  })).asUInt() // use uint instead vec to reduce verilog lines
  val evenDeqMask = getEvenBits(deqMask)
  val oddDeqMask = getOddBits(deqMask)
  // generate lastCycleSelect mask
  val evenSelectMask = Mux(io.ldout(0).fire(), getEvenBits(UIntToOH(loadWbSel(0))), 0.U)
  val oddSelectMask = Mux(io.ldout(1).fire(), getOddBits(UIntToOH(loadWbSel(1))), 0.U)
  // generate real select vec
  val loadEvenSelVec = getEvenBits(loadWbSelVec) & ~evenSelectMask
  val loadOddSelVec = getOddBits(loadWbSelVec) & ~oddSelectMask

  def toVec(a: UInt): Vec[Bool] = {
    VecInit(a.asBools)
  }

  val loadWbSelGen = Wire(Vec(LoadPipelineWidth, UInt(log2Up(LoadQueueSize).W)))
  val loadWbSelVGen = Wire(Vec(LoadPipelineWidth, Bool()))
  loadWbSelGen(0) := Cat(getFirstOne(toVec(loadEvenSelVec), evenDeqMask), 0.U(1.W))
  loadWbSelVGen(0):= loadEvenSelVec.asUInt.orR
  loadWbSelGen(1) := Cat(getFirstOne(toVec(loadOddSelVec), oddDeqMask), 1.U(1.W))
  loadWbSelVGen(1) := loadOddSelVec.asUInt.orR

  (0 until LoadPipelineWidth).map(i => {
    loadWbSel(i) := RegNext(loadWbSelGen(i))
    loadWbSelV(i) := RegNext(loadWbSelVGen(i), init = false.B)
    when(io.ldout(i).fire()){
      // Mark them as writebacked, so they will not be selected in the next cycle
      writebacked(loadWbSel(i)) := true.B
    }
  })

  // Stage 1
  // Use indexes generated in cycle 0 to read data
  // writeback data to cdb
  (0 until LoadPipelineWidth).map(i => {
    // data select
    dataModule.io.wb.raddr(i) := loadWbSelGen(i)
    val rdata = dataModule.io.wb.rdata(i).data
    val seluop = uop(loadWbSel(i))
    val func = seluop.ctrl.fuOpType
    val raddr = dataModule.io.wb.rdata(i).paddr
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
    val rdataPartialLoad = rdataHelper(seluop, rdataSel)

    // writeback missed int/fp load
    //
    // Int load writeback will finish (if not blocked) in one cycle
    io.ldout(i).bits.uop := seluop
    io.ldout(i).bits.uop.lqIdx := loadWbSel(i).asTypeOf(new LqPtr)
    io.ldout(i).bits.data := rdataPartialLoad
    io.ldout(i).bits.redirectValid := false.B
    io.ldout(i).bits.redirect := DontCare
    io.ldout(i).bits.debug.isMMIO := debug_mmio(loadWbSel(i))
    io.ldout(i).bits.debug.isPerfCnt := false.B
    io.ldout(i).bits.debug.paddr := debug_paddr(loadWbSel(i))
    io.ldout(i).bits.fflags := DontCare
    io.ldout(i).valid := loadWbSelV(i)

    when(io.ldout(i).fire()) {
      XSInfo("int load miss write to cbd roqidx %d lqidx %d pc 0x%x mmio %x\n",
        io.ldout(i).bits.uop.roqIdx.asUInt,
        io.ldout(i).bits.uop.lqIdx.asUInt,
        io.ldout(i).bits.uop.cf.pc,
        debug_mmio(loadWbSel(i))
      )
    }

  })

  /**
    * Load commits
    *
    * When load commited, mark it as !allocated and move deqPtrExt forward.
    */
  (0 until CommitWidth).map(i => {
    when(commitCount > i.U){
      allocated(deqPtr+i.U) := false.B
    }
  })

  def getFirstOne(mask: Vec[Bool], startMask: UInt) = {
    val length = mask.length
    val highBits = (0 until length).map(i => mask(i) & ~startMask(i))
    val highBitsUint = Cat(highBits.reverse)
    PriorityEncoder(Mux(highBitsUint.orR(), highBitsUint, mask.asUInt))
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

  /**
    * Memory violation detection
    *
    * When store writes back, it searches LoadQueue for younger load instructions
    * with the same load physical address. They loaded wrong data and need re-execution.
    *
    * Cycle 0: Store Writeback
    *   Generate match vector for store address with rangeMask(stPtr, enqPtr).
    *   Besides, load instructions in LoadUnit_S1 and S2 are also checked.
    * Cycle 1: Redirect Generation
    *   There're three possible types of violations, up to 6 possible redirect requests.
    *   Choose the oldest load (part 1). (4 + 2) -> (1 + 2)
    * Cycle 2: Redirect Fire
    *   Choose the oldest load (part 2). (3 -> 1)
    *   Prepare redirect request according to the detected violation.
    *   Fire redirect request (if valid)
    */

  // stage 0:        lq l1 wb     l1 wb lq
  //                 |  |  |      |  |  |  (paddr match)
  // stage 1:        lq l1 wb     l1 wb lq
  //                 |  |  |      |  |  |
  //                 |  |------------|  |
  //                 |        |         |
  // stage 2:        lq      l1wb       lq
  //                 |        |         |
  //                 --------------------
  //                          |
  //                      rollback req
  io.load_s1 := DontCare
  def detectRollback(i: Int) = {
    val startIndex = io.storeIn(i).bits.uop.lqIdx.value
    val lqIdxMask = UIntToMask(startIndex, LoadQueueSize)
    val xorMask = lqIdxMask ^ enqMask
    val sameFlag = io.storeIn(i).bits.uop.lqIdx.flag === enqPtrExt(0).flag
    val toEnqPtrMask = Mux(sameFlag, xorMask, ~xorMask)

    // check if load already in lq needs to be rolledback
    dataModule.io.violation(i).paddr := io.storeIn(i).bits.paddr
    dataModule.io.violation(i).mask := io.storeIn(i).bits.mask
    val addrMaskMatch = RegNext(dataModule.io.violation(i).violationMask)
    val entryNeedCheck = RegNext(VecInit((0 until LoadQueueSize).map(j => {
      allocated(j) && toEnqPtrMask(j) && (datavalid(j) || miss(j))
    })))
    val lqViolationVec = VecInit((0 until LoadQueueSize).map(j => {
      addrMaskMatch(j) && entryNeedCheck(j)
    }))
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
      io.load_s1(j).valid && // L1 valid
        isAfter(io.load_s1(j).uop.roqIdx, io.storeIn(i).bits.uop.roqIdx) &&
        io.storeIn(i).bits.paddr(PAddrBits - 1, 3) === io.load_s1(j).paddr(PAddrBits - 1, 3) &&
        (io.storeIn(i).bits.mask & io.load_s1(j).mask).orR
    })))
    val l1Violation = l1ViolationVec.asUInt().orR()
    val l1ViolationUop = getOldestInTwo(l1ViolationVec, RegNext(VecInit(io.load_s1.map(_.uop))))
    XSDebug(l1Violation, p"${Binary(Cat(l1ViolationVec))}, $l1ViolationUop\n")

    XSDebug(
      l1Violation,
      "need rollback (l1 load) pc %x roqidx %d target %x\n",
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

    ((lqViolation, lqViolationUop), (wbViolation, wbViolationUop), (l1Violation, l1ViolationUop))
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
  val lastCycleRedirect = RegNext(io.brqRedirect)
  val lastlastCycleRedirect = RegNext(lastCycleRedirect)
  val lastCycleFlush = RegNext(io.flush)
  val lastlastCycleFlush = RegNext(lastCycleFlush)

  // S2: select rollback (part1) and generate rollback request
  // rollback check
  // Wb/L1 rollback seq check is done in s2
  val rollbackWb = Wire(Vec(StorePipelineWidth, Valid(new MicroOp)))
  val rollbackL1 = Wire(Vec(StorePipelineWidth, Valid(new MicroOp)))
  val rollbackL1Wb = Wire(Vec(StorePipelineWidth*2, Valid(new MicroOp)))
  // Lq rollback seq check is done in s3 (next stage), as getting rollbackLq MicroOp is slow
  val rollbackLq = Wire(Vec(StorePipelineWidth, Valid(new MicroOp)))
  for (i <- 0 until StorePipelineWidth) {
    val detectedRollback = detectRollback(i)
    rollbackLq(i).valid := detectedRollback._1._1 && RegNext(io.storeIn(i).valid)
    rollbackLq(i).bits := detectedRollback._1._2
    rollbackWb(i).valid := detectedRollback._2._1 && RegNext(io.storeIn(i).valid)
    rollbackWb(i).bits := detectedRollback._2._2
    rollbackL1(i).valid := detectedRollback._3._1 && RegNext(io.storeIn(i).valid)
    rollbackL1(i).bits := detectedRollback._3._2
    rollbackL1Wb(2*i) := rollbackL1(i)
    rollbackL1Wb(2*i+1) := rollbackWb(i)
  }

  val rollbackL1WbSelected = ParallelOperation(rollbackL1Wb, rollbackSel)
  val rollbackL1WbVReg = RegNext(rollbackL1WbSelected.valid)
  val rollbackL1WbReg = RegEnable(rollbackL1WbSelected.bits, rollbackL1WbSelected.valid)
  val rollbackLq0VReg = RegNext(rollbackLq(0).valid)
  val rollbackLq0Reg = RegEnable(rollbackLq(0).bits, rollbackLq(0).valid)
  val rollbackLq1VReg = RegNext(rollbackLq(1).valid)
  val rollbackLq1Reg = RegEnable(rollbackLq(1).bits, rollbackLq(1).valid)

  // S3: select rollback (part2), generate rollback request, then fire rollback request
  // Note that we use roqIdx - 1.U to flush the load instruction itself.
  // Thus, here if last cycle's roqIdx equals to this cycle's roqIdx, it still triggers the redirect.

  // FIXME: this is ugly
  val rollbackValidVec = Seq(rollbackL1WbVReg, rollbackLq0VReg, rollbackLq1VReg)
  val rollbackUopVec = Seq(rollbackL1WbReg, rollbackLq0Reg, rollbackLq1Reg)

  // select uop in parallel
  val mask = getAfterMask(rollbackValidVec, rollbackUopVec)
  val oneAfterZero = mask(1)(0)
  val rollbackUop = Mux(oneAfterZero && mask(2)(0),
    rollbackUopVec(0),
    Mux(!oneAfterZero && mask(2)(1), rollbackUopVec(1), rollbackUopVec(2)))

  // check if rollback request is still valid in parallel
  val rollbackValidVecChecked = Wire(Vec(3, Bool()))
  for(((v, uop), idx) <- rollbackValidVec.zip(rollbackUopVec).zipWithIndex) {
    rollbackValidVecChecked(idx) := v && 
      (!lastCycleRedirect.valid || isBefore(uop.roqIdx, lastCycleRedirect.bits.roqIdx)) &&
      (!lastlastCycleRedirect.valid || isBefore(uop.roqIdx, lastlastCycleRedirect.bits.roqIdx))
  }

  io.rollback.bits.roqIdx := rollbackUop.roqIdx
  io.rollback.bits.ftqIdx := rollbackUop.cf.ftqPtr
  io.rollback.bits.ftqOffset := rollbackUop.cf.ftqOffset
  io.rollback.bits.level := RedirectLevel.flush
  io.rollback.bits.interrupt := DontCare
  io.rollback.bits.cfiUpdate := DontCare
  io.rollback.bits.cfiUpdate.target := rollbackUop.cf.pc
  // io.rollback.bits.pc := DontCare

  io.rollback.valid := rollbackValidVecChecked.asUInt.orR && !lastCycleFlush && !lastlastCycleFlush

  when(io.rollback.valid) {
    // XSDebug("Mem rollback: pc %x roqidx %d\n", io.rollback.bits.cfi, io.rollback.bits.roqIdx.asUInt)
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
  val lqTailMmioPending = WireInit(pending(deqPtr))
  val lqTailAllocated = WireInit(allocated(deqPtr))
  val s_idle :: s_req :: s_resp :: s_wait :: Nil = Enum(4)
  val uncacheState = RegInit(s_idle)
  switch(uncacheState) {
    is(s_idle) {
      when(io.roq.pendingld && lqTailMmioPending && lqTailAllocated) {
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

  dataModule.io.uncache.raddr := deqPtrExtNext.value

  io.uncache.req.bits.cmd  := MemoryOpConstants.M_XRD
  io.uncache.req.bits.addr := dataModule.io.uncache.rdata.paddr
  io.uncache.req.bits.data := dataModule.io.uncache.rdata.data
  io.uncache.req.bits.mask := dataModule.io.uncache.rdata.mask

  io.uncache.req.bits.id   := DontCare

  io.uncache.resp.ready := true.B

  when (io.uncache.req.fire()) {
    pending(deqPtr) := false.B

    XSDebug("uncache req: pc %x addr %x data %x op %x mask %x\n",
      uop(deqPtr).cf.pc,
      io.uncache.req.bits.addr,
      io.uncache.req.bits.data,
      io.uncache.req.bits.cmd,
      io.uncache.req.bits.mask
    )
  }

  // (3) response from uncache channel: mark as datavalid
  dataModule.io.uncache.wen := false.B
  when(io.uncache.resp.fire()){
    datavalid(deqPtr) := true.B
    dataModule.io.uncacheWrite(deqPtr, io.uncache.resp.bits.data(XLEN-1, 0))
    dataModule.io.uncache.wen := true.B

    XSDebug("uncache resp: data %x\n", io.dcache.bits.data)
  }

  // Read vaddr for mem exception
  // no inst will be commited 1 cycle before tval update
  vaddrModule.io.raddr(0) := (deqPtrExt + commitCount).value 
  io.exceptionAddr.vaddr := vaddrModule.io.rdata(0)

  // misprediction recovery / exception redirect
  // invalidate lq term using robIdx
  val needCancel = Wire(Vec(LoadQueueSize, Bool()))
  for (i <- 0 until LoadQueueSize) {
    needCancel(i) := uop(i).roqIdx.needFlush(io.brqRedirect, io.flush) && allocated(i)
    when (needCancel(i)) {
        allocated(i) := false.B
    }
  }

  /**
    * update pointers
    */
  val lastCycleCancelCount = PopCount(RegNext(needCancel))
  // when io.brqRedirect.valid, we don't allow eneuque even though it may fire.
  val enqNumber = Mux(io.enq.canAccept && io.enq.sqCanAccept && !(io.brqRedirect.valid || io.flush), PopCount(io.enq.req.map(_.valid)), 0.U)
  when (lastCycleRedirect.valid || lastCycleFlush) {
    // we recover the pointers in the next cycle after redirect
    enqPtrExt := VecInit(enqPtrExt.map(_ - lastCycleCancelCount))
  }.otherwise {
    enqPtrExt := VecInit(enqPtrExt.map(_ + enqNumber))
  }

  deqPtrExtNext := deqPtrExt + commitCount
  deqPtrExt := deqPtrExtNext

  val validCount = distanceBetween(enqPtrExt(0), deqPtrExt)

  allowEnqueue := validCount + enqNumber <= (LoadQueueSize - RenameWidth).U

  // perf counter
  QueuePerf(LoadQueueSize, validCount, !allowEnqueue)
  XSPerf("rollback", io.rollback.valid) // rollback redirect generated
  XSPerf("mmioCycle", uncacheState =/= s_idle) // lq is busy dealing with uncache req
  XSPerf("mmioCnt", io.uncache.req.fire())
  XSPerf("refill", io.dcache.valid)
  XSPerf("writeback_success", PopCount(VecInit(io.ldout.map(i => i.fire()))))
  XSPerf("writeback_blocked", PopCount(VecInit(io.ldout.map(i => i.valid && !i.ready))))
  XSPerf("utilization_miss", PopCount((0 until LoadQueueSize).map(i => allocated(i) && miss(i))))

  // debug info
  XSDebug("enqPtrExt %d:%d deqPtrExt %d:%d\n", enqPtrExt(0).flag, enqPtr, deqPtrExt.flag, deqPtr)

  def PrintFlag(flag: Bool, name: String): Unit = {
    when(flag) {
      XSDebug(false, true.B, name)
    }.otherwise {
      XSDebug(false, true.B, " ")
    }
  }

  for (i <- 0 until LoadQueueSize) {
    if (i % 4 == 0) XSDebug("")
    XSDebug(false, true.B, "%x [%x] ", uop(i).cf.pc, dataModule.io.debug(i).paddr)
    PrintFlag(allocated(i), "a")
    PrintFlag(allocated(i) && datavalid(i), "v")
    PrintFlag(allocated(i) && writebacked(i), "w")
    PrintFlag(allocated(i) && miss(i), "m")
    // PrintFlag(allocated(i) && listening(i), "l")
    PrintFlag(allocated(i) && pending(i), "p")
    XSDebug(false, true.B, " ")
    if (i % 4 == 3 || i == LoadQueueSize - 1) XSDebug(false, true.B, "\n")
  }

}
