/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package xiangshan.mem

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import xiangshan._
import xiangshan.cache._
import xiangshan.cache.{DCacheWordIO, DCacheLineIO, MemoryOpConstants}
import xiangshan.backend.rob.{RobLsqIO, RobPtr}
import difftest._
import device.RAMHelper

class SqPtr(implicit p: Parameters) extends CircularQueuePtr[SqPtr](
  p => p(XSCoreParamsKey).StoreQueueSize
){
  override def cloneType = (new SqPtr).asInstanceOf[this.type]
}

object SqPtr {
  def apply(f: Bool, v: UInt)(implicit p: Parameters): SqPtr = {
    val ptr = Wire(new SqPtr)
    ptr.flag := f
    ptr.value := v
    ptr
  }
}

class SqEnqIO(implicit p: Parameters) extends XSBundle {
  val canAccept = Output(Bool())
  val lqCanAccept = Input(Bool())
  val needAlloc = Vec(exuParameters.LsExuCnt, Input(Bool()))
  val req = Vec(exuParameters.LsExuCnt, Flipped(ValidIO(new MicroOp)))
  val resp = Vec(exuParameters.LsExuCnt, Output(new SqPtr))
}

// Store Queue
class StoreQueue(implicit p: Parameters) extends XSModule with HasDCacheParameters with HasCircularQueuePtrHelper {
  val io = IO(new Bundle() {
    val enq = new SqEnqIO
    val brqRedirect = Flipped(ValidIO(new Redirect))
    val storeIn = Vec(StorePipelineWidth, Flipped(Valid(new LsPipelineBundle))) // store addr, data is not included
    val storeInRe = Vec(StorePipelineWidth, Input(new LsPipelineBundle())) // store more mmio and exception
    val storeDataIn = Vec(StorePipelineWidth, Flipped(Valid(new StoreDataBundle))) // store data, send to sq from rs
    val sbuffer = Vec(StorePipelineWidth, Decoupled(new DCacheWordReqWithVaddr)) // write commited store to sbuffer
    val mmioStout = DecoupledIO(new ExuOutput) // writeback uncached store
    val forward = Vec(LoadPipelineWidth, Flipped(new PipeLoadForwardQueryIO))
    val rob = Flipped(new RobLsqIO)
    val uncache = new DCacheWordIO
    // val refill = Flipped(Valid(new DCacheLineReq ))
    val exceptionAddr = new ExceptionAddrIO
    val sqempty = Output(Bool())
    val issuePtrExt = Output(new SqPtr) // used to wake up delayed load/store
    val sqFull = Output(Bool())
  })

  println("StoreQueue: size:" + StoreQueueSize)

  // data modules
  val uop = Reg(Vec(StoreQueueSize, new MicroOp))
  // val data = Reg(Vec(StoreQueueSize, new LsqEntry))
  val dataModule = Module(new SQDataModule(
    numEntries = StoreQueueSize,
    numRead = StorePipelineWidth,
    numWrite = StorePipelineWidth,
    numForward = StorePipelineWidth
  ))
  dataModule.io := DontCare
  val paddrModule = Module(new SQAddrModule(
    dataWidth = PAddrBits,
    numEntries = StoreQueueSize,
    numRead = StorePipelineWidth,
    numWrite = StorePipelineWidth,
    numForward = StorePipelineWidth
  ))
  paddrModule.io := DontCare
  val vaddrModule = Module(new SQAddrModule(
    dataWidth = VAddrBits,
    numEntries = StoreQueueSize,
    numRead = StorePipelineWidth + 1, // sbuffer 2 + badvaddr 1 (TODO)
    numWrite = StorePipelineWidth,
    numForward = StorePipelineWidth
  ))
  vaddrModule.io := DontCare
  val debug_paddr = Reg(Vec(StoreQueueSize, UInt((PAddrBits).W)))
  val debug_vaddr = Reg(Vec(StoreQueueSize, UInt((VAddrBits).W)))
  val debug_data = Reg(Vec(StoreQueueSize, UInt((XLEN).W)))

  // state & misc
  val allocated = RegInit(VecInit(List.fill(StoreQueueSize)(false.B))) // sq entry has been allocated
  val addrvalid = RegInit(VecInit(List.fill(StoreQueueSize)(false.B))) // non-mmio addr is valid
  val datavalid = RegInit(VecInit(List.fill(StoreQueueSize)(false.B))) // non-mmio data is valid
  val allvalid  = VecInit((0 until StoreQueueSize).map(i => addrvalid(i) && datavalid(i))) // non-mmio data & addr is valid
  val commited = Reg(Vec(StoreQueueSize, Bool())) // inst has been commited by rob
  val pending = Reg(Vec(StoreQueueSize, Bool())) // mmio pending: inst is an mmio inst, it will not be executed until it reachs the end of rob
  val mmio = Reg(Vec(StoreQueueSize, Bool())) // mmio: inst is an mmio inst

  // ptr
  val enqPtrExt = RegInit(VecInit((0 until io.enq.req.length).map(_.U.asTypeOf(new SqPtr))))
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

  val commitCount = RegNext(io.rob.scommit)

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
    vaddrModule.io.raddr(i) := deqPtrExtNext(i).value
  }

  // no inst will be commited 1 cycle before tval update
  vaddrModule.io.raddr(StorePipelineWidth) := (cmtPtrExt(0) + commitCount).value

  /**
    * Enqueue at dispatch
    *
    * Currently, StoreQueue only allows enqueue when #emptyEntries > EnqWidth
    */
  io.enq.canAccept := allowEnqueue
  for (i <- 0 until io.enq.req.length) {
    val offset = if (i == 0) 0.U else PopCount(io.enq.needAlloc.take(i))
    val sqIdx = enqPtrExt(offset)
    val index = sqIdx.value
    when (io.enq.req(i).valid && io.enq.canAccept && io.enq.lqCanAccept && !io.brqRedirect.valid) {
      uop(index) := io.enq.req(i).bits
      allocated(index) := true.B
      datavalid(index) := false.B
      addrvalid(index) := false.B
      commited(index) := false.B
      pending(index) := false.B
    }
    io.enq.resp(i) := sqIdx
  }
  XSDebug(p"(ready, valid): ${io.enq.canAccept}, ${Binary(Cat(io.enq.req.map(_.valid)))}\n")

  /**
    * Update issuePtr when issue from rs
    */
  // update issuePtr
  val IssuePtrMoveStride = 4
  require(IssuePtrMoveStride >= 2)

  val issueLookupVec = (0 until IssuePtrMoveStride).map(issuePtrExt + _.U)
  val issueLookup = issueLookupVec.map(ptr => allocated(ptr.value) && addrvalid(ptr.value) && datavalid(ptr.value) && ptr =/= enqPtrExt(0))
  val nextIssuePtr = issuePtrExt + PriorityEncoder(VecInit(issueLookup.map(!_) :+ true.B))
  issuePtrExt := nextIssuePtr

  when (io.brqRedirect.valid) {
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
    *   (1) For an mmio instruction with exceptions, we need to mark it as addrvalid
    * (in this way it will trigger an exception when it reaches ROB's head)
    * instead of pending to avoid sending them to lower level.
    *   (2) For an mmio instruction without exceptions, we mark it as pending.
    * When the instruction reaches ROB's head, StoreQueue sends it to uncache channel.
    * Upon receiving the response, StoreQueue writes back the instruction
    * through arbiter with store units. It will later commit as normal.
    */

  // Write addr to sq
  for (i <- 0 until StorePipelineWidth) {
    paddrModule.io.wen(i) := false.B
    vaddrModule.io.wen(i) := false.B
    dataModule.io.mask.wen(i) := false.B
    val stWbIndex = io.storeIn(i).bits.uop.sqIdx.value
    when (io.storeIn(i).fire()) {
      addrvalid(stWbIndex) := true.B//!io.storeIn(i).bits.mmio
      // pending(stWbIndex) := io.storeIn(i).bits.mmio

      dataModule.io.mask.waddr(i) := stWbIndex
      dataModule.io.mask.wdata(i) := io.storeIn(i).bits.mask
      dataModule.io.mask.wen(i) := true.B

      paddrModule.io.waddr(i) := stWbIndex
      paddrModule.io.wdata(i) := io.storeIn(i).bits.paddr
      paddrModule.io.wlineflag(i) := io.storeIn(i).bits.wlineflag
      paddrModule.io.wen(i) := true.B

      vaddrModule.io.waddr(i) := stWbIndex
      vaddrModule.io.wdata(i) := io.storeIn(i).bits.vaddr
      vaddrModule.io.wlineflag(i) := io.storeIn(i).bits.wlineflag
      vaddrModule.io.wen(i) := true.B

      debug_paddr(paddrModule.io.waddr(i)) := paddrModule.io.wdata(i)

      // mmio(stWbIndex) := io.storeIn(i).bits.mmio

      uop(stWbIndex).debugInfo := io.storeIn(i).bits.uop.debugInfo
      XSInfo("store addr write to sq idx %d pc 0x%x vaddr %x paddr %x mmio %x\n",
        io.storeIn(i).bits.uop.sqIdx.value,
        io.storeIn(i).bits.uop.cf.pc,
        io.storeIn(i).bits.vaddr,
        io.storeIn(i).bits.paddr,
        io.storeIn(i).bits.mmio
      )
    }

    // re-replinish mmio, for pma/pmp will get mmio one cycle later
    val storeInFireReg = RegNext(io.storeIn(i).fire())
    val stWbIndexReg = RegNext(stWbIndex)
    when (storeInFireReg) {
      pending(stWbIndexReg) := io.storeInRe(i).mmio
      mmio(stWbIndexReg) := io.storeInRe(i).mmio
    }

    when(vaddrModule.io.wen(i)){
      debug_vaddr(vaddrModule.io.waddr(i)) := vaddrModule.io.wdata(i)
    }
  }

  // Write data to sq
  for (i <- 0 until StorePipelineWidth) {
    dataModule.io.data.wen(i) := false.B
    io.rob.storeDataRobWb(i).valid := false.B
    io.rob.storeDataRobWb(i).bits := DontCare
    val stWbIndex = io.storeDataIn(i).bits.uop.sqIdx.value
    when (io.storeDataIn(i).fire()) {
      datavalid(stWbIndex) := true.B

      dataModule.io.data.waddr(i) := stWbIndex
      dataModule.io.data.wdata(i) := Mux(io.storeDataIn(i).bits.uop.ctrl.fuOpType === LSUOpType.cbo_zero,
        0.U,
        genWdata(io.storeDataIn(i).bits.data, io.storeDataIn(i).bits.uop.ctrl.fuOpType(1,0))
      )
      dataModule.io.data.wen(i) := true.B

      debug_data(dataModule.io.data.waddr(i)) := dataModule.io.data.wdata(i)

      io.rob.storeDataRobWb(i).valid := true.B
      io.rob.storeDataRobWb(i).bits := io.storeDataIn(i).bits.uop.robIdx

      XSInfo("store data write to sq idx %d pc 0x%x data %x -> %x\n",
        io.storeDataIn(i).bits.uop.sqIdx.value,
        io.storeDataIn(i).bits.uop.cf.pc,
        io.storeDataIn(i).bits.data,
        dataModule.io.data.wdata(i)
      )
    }
  }

  /**
    * load forward query
    *
    * Check store queue for instructions that is older than the load.
    * The response will be valid at the next cycle after req.
    */
  // check over all lq entries and forward data from the first matched store
  for (i <- 0 until LoadPipelineWidth) {
    // Compare deqPtr (deqPtr) and forward.sqIdx, we have two cases:
    // (1) if they have the same flag, we need to check range(tail, sqIdx)
    // (2) if they have different flags, we need to check range(tail, LoadQueueSize) and range(0, sqIdx)
    // Forward1: Mux(same_flag, range(tail, sqIdx), range(tail, LoadQueueSize))
    // Forward2: Mux(same_flag, 0.U,                   range(0, sqIdx)    )
    // i.e. forward1 is the target entries with the same flag bits and forward2 otherwise
    val differentFlag = deqPtrExt(0).flag =/= io.forward(i).sqIdx.flag
    val forwardMask = io.forward(i).sqIdxMask
    // all addrvalid terms need to be checked
    val addrValidVec = WireInit(VecInit((0 until StoreQueueSize).map(i => addrvalid(i) && allocated(i))))
    val dataValidVec = WireInit(VecInit((0 until StoreQueueSize).map(i => datavalid(i))))
    val allValidVec = WireInit(VecInit((0 until StoreQueueSize).map(i => addrvalid(i) && datavalid(i) && allocated(i))))
    val canForward1 = Mux(differentFlag, ~deqMask, deqMask ^ forwardMask) & allValidVec.asUInt
    val canForward2 = Mux(differentFlag, forwardMask, 0.U(StoreQueueSize.W)) & allValidVec.asUInt
    val needForward = Mux(differentFlag, ~deqMask | forwardMask, deqMask ^ forwardMask)

    XSDebug(p"$i f1 ${Binary(canForward1)} f2 ${Binary(canForward2)} " +
      p"sqIdx ${io.forward(i).sqIdx} pa ${Hexadecimal(io.forward(i).paddr)}\n"
    )

    // do real fwd query (cam lookup in load_s1)
    dataModule.io.needForward(i)(0) := canForward1 & vaddrModule.io.forwardMmask(i).asUInt
    dataModule.io.needForward(i)(1) := canForward2 & vaddrModule.io.forwardMmask(i).asUInt

    vaddrModule.io.forwardMdata(i) := io.forward(i).vaddr
    paddrModule.io.forwardMdata(i) := io.forward(i).paddr

    // vaddr cam result does not equal to paddr cam result
    // replay needed
    // val vpmaskNotEqual = ((paddrModule.io.forwardMmask(i).asUInt ^ vaddrModule.io.forwardMmask(i).asUInt) & needForward) =/= 0.U
    // val vaddrMatchFailed = vpmaskNotEqual && io.forward(i).valid
    val vpmaskNotEqual = ((RegNext(paddrModule.io.forwardMmask(i).asUInt) ^ RegNext(vaddrModule.io.forwardMmask(i).asUInt)) & RegNext(needForward)) =/= 0.U
    val vaddrMatchFailed = vpmaskNotEqual && RegNext(io.forward(i).valid)
    when (vaddrMatchFailed) {
      XSInfo("vaddrMatchFailed: pc %x pmask %x vmask %x\n",
        RegNext(io.forward(i).uop.cf.pc),
        RegNext(needForward & paddrModule.io.forwardMmask(i).asUInt),
        RegNext(needForward & vaddrModule.io.forwardMmask(i).asUInt)
      );
    }
    XSPerfAccumulate("vaddr_match_failed", vpmaskNotEqual)
    XSPerfAccumulate("vaddr_match_really_failed", vaddrMatchFailed)

    // Fast forward mask will be generated immediately (load_s1)
    io.forward(i).forwardMaskFast := dataModule.io.forwardMaskFast(i)

    // Forward result will be generated 1 cycle later (load_s2)
    io.forward(i).forwardMask := dataModule.io.forwardMask(i)
    io.forward(i).forwardData := dataModule.io.forwardData(i)

    // If addr match, data not ready, mark it as dataInvalid
    // load_s1: generate dataInvalid in load_s1 to set fastUop
    io.forward(i).dataInvalidFast := (addrValidVec.asUInt & ~dataValidVec.asUInt & vaddrModule.io.forwardMmask(i).asUInt & needForward).orR
    val dataInvalidSqIdxReg = RegNext(OHToUInt(addrValidVec.asUInt & ~dataValidVec.asUInt & vaddrModule.io.forwardMmask(i).asUInt & needForward))
    // load_s2
    io.forward(i).dataInvalid := RegNext(io.forward(i).dataInvalidFast)

    // load_s2
    // check if vaddr forward mismatched
    io.forward(i).matchInvalid := vaddrMatchFailed
    io.forward(i).dataInvalidSqIdx := dataInvalidSqIdxReg
  }

  /**
    * Memory mapped IO / other uncached operations
    *
    * States:
    * (1) writeback from store units: mark as pending
    * (2) when they reach ROB's head, they can be sent to uncache channel
    * (3) response from uncache channel: mark as datavalidmask.wen
    * (4) writeback to ROB (and other units): mark as writebacked
    * (5) ROB commits the instruction: same as normal instructions
    */
  //(2) when they reach ROB's head, they can be sent to uncache channel
  val s_idle :: s_req :: s_resp :: s_wb :: s_wait :: Nil = Enum(5)
  val uncacheState = RegInit(s_idle)
  switch(uncacheState) {
    is(s_idle) {
      when(io.rob.pendingst && pending(deqPtr) && allocated(deqPtr) && datavalid(deqPtr) && addrvalid(deqPtr)) {
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
      when(commitCount > 0.U) {
        uncacheState := s_idle // ready for next mmio
      }
    }
  }
  io.uncache.req.valid := uncacheState === s_req

  io.uncache.req.bits.cmd  := MemoryOpConstants.M_XWR
  io.uncache.req.bits.addr := paddrModule.io.rdata(0) // data(deqPtr) -> rdata(0)
  io.uncache.req.bits.data := dataModule.io.rdata(0).data
  io.uncache.req.bits.mask := dataModule.io.rdata(0).mask

  // CBO op type check can be delayed for 1 cycle,
  // as uncache op will not start in s_idle
  val cbo_mmio_addr = paddrModule.io.rdata(0) >> 2 << 2 // clear lowest 2 bits for op
  val cbo_mmio_op = 0.U //TODO
  val cbo_mmio_data = cbo_mmio_addr | cbo_mmio_op
  when(RegNext(LSUOpType.isCbo(uop(deqPtr).ctrl.fuOpType))){
    io.uncache.req.bits.addr := DontCare // TODO
    io.uncache.req.bits.data := paddrModule.io.rdata(0)
    io.uncache.req.bits.mask := DontCare // TODO
  }

  io.uncache.req.bits.id   := DontCare
  io.uncache.req.bits.instrtype   := DontCare

  when(io.uncache.req.fire()){
    // mmio store should not be committed until uncache req is sent
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

  // (4) writeback to ROB (and other units): mark as writebacked
  io.mmioStout.valid := uncacheState === s_wb
  io.mmioStout.bits.uop := uop(deqPtr)
  io.mmioStout.bits.uop.sqIdx := deqPtrExt(0)
  io.mmioStout.bits.data := dataModule.io.rdata(0).data // dataModule.io.rdata.read(deqPtr)
  io.mmioStout.bits.redirectValid := false.B
  io.mmioStout.bits.redirect := DontCare
  io.mmioStout.bits.debug.isMMIO := true.B
  io.mmioStout.bits.debug.paddr := DontCare
  io.mmioStout.bits.debug.isPerfCnt := false.B
  io.mmioStout.bits.fflags := DontCare
  // Remove MMIO inst from store queue after MMIO request is being sent
  // That inst will be traced by uncache state machine
  when (io.mmioStout.fire()) {
    allocated(deqPtr) := false.B
  }

  /**
    * ROB commits store instructions (mark them as commited)
    *
    * (1) When store commits, mark it as commited.
    * (2) They will not be cancelled and can be sent to lower level.
    */
  XSError(uncacheState =/= s_idle && uncacheState =/= s_wait && commitCount > 0.U,
   "should not commit instruction when MMIO has not been finished\n")
  for (i <- 0 until CommitWidth) {
    when (commitCount > i.U) { // MMIO inst is not in progress
      if(i == 0){
        // MMIO inst should not update commited flag
        // Note that commit count has been delayed for 1 cycle
        when(uncacheState === s_idle){
          commited(cmtPtrExt(0).value) := true.B
        }
      } else {
        commited(cmtPtrExt(i).value) := true.B
      }
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
    // Note that store data/addr should both be valid after store's commit
    assert(!io.sbuffer(i).valid || allvalid(ptr))
    // Write line request should have all 1 mask
    assert(!(io.sbuffer(i).valid && io.sbuffer(i).bits.wline && !io.sbuffer(i).bits.mask.andR))
    io.sbuffer(i).bits.cmd   := MemoryOpConstants.M_XWR
    io.sbuffer(i).bits.addr  := paddrModule.io.rdata(i)
    io.sbuffer(i).bits.vaddr := vaddrModule.io.rdata(i)
    io.sbuffer(i).bits.data  := dataModule.io.rdata(i).data
    io.sbuffer(i).bits.mask  := dataModule.io.rdata(i).mask
    io.sbuffer(i).bits.wline := paddrModule.io.rlineflag(i)
    io.sbuffer(i).bits.id    := DontCare
    io.sbuffer(i).bits.instrtype    := DontCare

    when (io.sbuffer(i).fire()) {
      allocated(ptr) := false.B
      XSDebug("sbuffer "+i+" fire: ptr %d\n", ptr)
    }
  }
  when (io.sbuffer(1).fire()) {
    assert(io.sbuffer(0).fire())
  }
  if (coreParams.dcacheParametersOpt.isEmpty) {
    for (i <- 0 until StorePipelineWidth) {
      val ptr = deqPtrExt(i).value
      val fakeRAM = Module(new RAMHelper(64L * 1024 * 1024 * 1024))
      fakeRAM.clk   := clock
      fakeRAM.en    := allocated(ptr) && commited(ptr) && !mmio(ptr)
      fakeRAM.rIdx  := 0.U
      fakeRAM.wIdx  := (paddrModule.io.rdata(i) - "h80000000".U) >> 3
      fakeRAM.wdata := dataModule.io.rdata(i).data
      fakeRAM.wmask := MaskExpand(dataModule.io.rdata(i).mask)
      fakeRAM.wen   := allocated(ptr) && commited(ptr) && !mmio(ptr)
    }
  }

  if (!env.FPGAPlatform) {
    for (i <- 0 until StorePipelineWidth) {
      val storeCommit = io.sbuffer(i).fire()
      val waddr = SignExt(io.sbuffer(i).bits.addr, 64)
      val wdata = io.sbuffer(i).bits.data & MaskExpand(io.sbuffer(i).bits.mask)
      val wmask = io.sbuffer(i).bits.mask

      val difftest = Module(new DifftestStoreEvent)
      difftest.io.clock       := clock
      difftest.io.coreid      := hardId.U
      difftest.io.index       := i.U
      difftest.io.valid       := storeCommit
      difftest.io.storeAddr   := waddr
      difftest.io.storeData   := wdata
      difftest.io.storeMask   := wmask
    }
  }

  // Read vaddr for mem exception
  io.exceptionAddr.vaddr := vaddrModule.io.rdata(StorePipelineWidth)

  // misprediction recovery / exception redirect
  // invalidate sq term using robIdx
  val needCancel = Wire(Vec(StoreQueueSize, Bool()))
  for (i <- 0 until StoreQueueSize) {
    needCancel(i) := uop(i).robIdx.needFlush(io.brqRedirect) && allocated(i) && !commited(i)
    when (needCancel(i)) {
        allocated(i) := false.B
    }
  }

  /**
    * update pointers
    */
  val lastCycleRedirect = RegNext(io.brqRedirect.valid)
  val lastCycleCancelCount = PopCount(RegNext(needCancel))
  // when io.brqRedirect.valid, we don't allow eneuque even though it may fire.
  val enqNumber = Mux(io.enq.canAccept && io.enq.lqCanAccept && !io.brqRedirect.valid, PopCount(io.enq.req.map(_.valid)), 0.U)
  when (lastCycleRedirect) {
    // we recover the pointers in the next cycle after redirect
    enqPtrExt := VecInit(enqPtrExt.map(_ - lastCycleCancelCount))
  }.otherwise {
    enqPtrExt := VecInit(enqPtrExt.map(_ + enqNumber))
  }

  deqPtrExt := deqPtrExtNext

  val dequeueCount = Mux(io.sbuffer(1).fire(), 2.U, Mux(io.sbuffer(0).fire() || io.mmioStout.fire(), 1.U, 0.U))
  val validCount = distanceBetween(enqPtrExt(0), deqPtrExt(0))

  allowEnqueue := validCount + enqNumber <= (StoreQueueSize - io.enq.req.length).U

  // io.sqempty will be used by sbuffer
  // We delay it for 1 cycle for better timing
  // When sbuffer need to check if it is empty, the pipeline is blocked, which means delay io.sqempty
  // for 1 cycle will also promise that sq is empty in that cycle
  io.sqempty := RegNext(enqPtrExt(0).value === deqPtrExt(0).value && enqPtrExt(0).flag === deqPtrExt(0).flag)

  // perf counter
  QueuePerf(StoreQueueSize, validCount, !allowEnqueue)
  io.sqFull := !allowEnqueue
  XSPerfAccumulate("mmioCycle", uncacheState =/= s_idle) // lq is busy dealing with uncache req
  XSPerfAccumulate("mmioCnt", io.uncache.req.fire())
  XSPerfAccumulate("mmio_wb_success", io.mmioStout.fire())
  XSPerfAccumulate("mmio_wb_blocked", io.mmioStout.valid && !io.mmioStout.ready)
  XSPerfAccumulate("validEntryCnt", distanceBetween(enqPtrExt(0), deqPtrExt(0)))
  XSPerfAccumulate("cmtEntryCnt", distanceBetween(cmtPtrExt(0), deqPtrExt(0)))
  XSPerfAccumulate("nCmtEntryCnt", distanceBetween(enqPtrExt(0), cmtPtrExt(0)))

  val perfinfo = IO(new Bundle(){
    val perfEvents = Output(new PerfEventsBundle(8))
  })
  val perfEvents = Seq(
    ("mmioCycle         ", uncacheState =/= s_idle                                                                                                                             ),
    ("mmioCnt           ", io.uncache.req.fire()                                                                                                                               ),
    ("mmio_wb_success   ", io.mmioStout.fire()                                                                                                                                 ),
    ("mmio_wb_blocked   ", io.mmioStout.valid && !io.mmioStout.ready                                                                                                           ),
    ("stq_1/4_valid     ", (distanceBetween(enqPtrExt(0), deqPtrExt(0)) < (StoreQueueSize.U/4.U))                                                                              ),
    ("stq_2/4_valid     ", (distanceBetween(enqPtrExt(0), deqPtrExt(0)) > (StoreQueueSize.U/4.U)) & (distanceBetween(enqPtrExt(0), deqPtrExt(0)) <= (StoreQueueSize.U/2.U))    ),
    ("stq_3/4_valid     ", (distanceBetween(enqPtrExt(0), deqPtrExt(0)) > (StoreQueueSize.U/2.U)) & (distanceBetween(enqPtrExt(0), deqPtrExt(0)) <= (StoreQueueSize.U*3.U/4.U))),
    ("stq_4/4_valid     ", (distanceBetween(enqPtrExt(0), deqPtrExt(0)) > (StoreQueueSize.U*3.U/4.U))                                                                          ),
  )

  for (((perf_out,(perf_name,perf)),i) <- perfinfo.perfEvents.perf_events.zip(perfEvents).zipWithIndex) {
    perf_out.incr_step := RegNext(perf)
  }
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
    XSDebug(i + ": pc %x va %x pa %x data %x ",
      uop(i).cf.pc,
      debug_vaddr(i),
      debug_paddr(i),
      debug_data(i)
    )
    PrintFlag(allocated(i), "a")
    PrintFlag(allocated(i) && addrvalid(i), "a")
    PrintFlag(allocated(i) && datavalid(i), "d")
    PrintFlag(allocated(i) && commited(i), "c")
    PrintFlag(allocated(i) && pending(i), "p")
    PrintFlag(allocated(i) && mmio(i), "m")
    XSDebug(false, true.B, "\n")
  }

}
