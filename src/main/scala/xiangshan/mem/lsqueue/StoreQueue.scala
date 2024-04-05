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

import chisel3._
import chisel3.util._
import difftest._
import difftest.common.DifftestMem
import org.chipsalliance.cde.config.Parameters
import utility._
import utils._
import xiangshan._
import xiangshan.cache._
import xiangshan.cache.{DCacheLineIO, DCacheWordIO, MemoryOpConstants}
import xiangshan.backend._
import xiangshan.backend.rob.{RobLsqIO, RobPtr}
import xiangshan.backend.Bundles.{DynInst, MemExuOutput}
import xiangshan.backend.decode.isa.bitfield.{Riscv32BitInst, XSInstBitFields}
import xiangshan.backend.fu.FuConfig._

class SqPtr(implicit p: Parameters) extends CircularQueuePtr[SqPtr](
  p => p(XSCoreParamsKey).StoreQueueSize
){
}

object SqPtr {
  def apply(f: Bool, v: UInt)(implicit p: Parameters): SqPtr = {
    val ptr = Wire(new SqPtr)
    ptr.flag := f
    ptr.value := v
    ptr
  }
}

class SqEnqIO(implicit p: Parameters) extends MemBlockBundle {
  val canAccept = Output(Bool())
  val lqCanAccept = Input(Bool())
  val needAlloc = Vec(LSQEnqWidth, Input(Bool()))
  val req = Vec(LSQEnqWidth, Flipped(ValidIO(new DynInst)))
  val resp = Vec(LSQEnqWidth, Output(new SqPtr))
}

class DataBufferEntry (implicit p: Parameters)  extends DCacheBundle {
  val addr   = UInt(PAddrBits.W)
  val vaddr  = UInt(VAddrBits.W)
  val data   = UInt(VLEN.W)
  val mask   = UInt((VLEN/8).W)
  val wline = Bool()
  val sqPtr  = new SqPtr
  val prefetch = Bool()
  val vecValid = Bool()
}

class StoreExceptionBuffer(implicit p: Parameters) extends XSModule with HasCircularQueuePtrHelper {
  val io = IO(new Bundle() {
    val redirect = Flipped(ValidIO(new Redirect))
    val storeAddrIn = Vec(StorePipelineWidth + 1, Flipped(ValidIO(new LsPipelineBundle())))
    val exceptionAddr = new ExceptionAddrIO
  })

  val req_valid = RegInit(false.B)
  val req = Reg(new LsPipelineBundle())

  // enqueue
  // S1:
  val s1_req = VecInit(io.storeAddrIn.map(_.bits))
  val s1_valid = VecInit(io.storeAddrIn.map(_.valid))

  // S2: delay 1 cycle
  val s2_req = RegNext(s1_req)
  val s2_valid = (0 until StorePipelineWidth + 1).map(i =>
    RegNext(s1_valid(i)) &&
      !s2_req(i).uop.robIdx.needFlush(RegNext(io.redirect)) &&
      !s2_req(i).uop.robIdx.needFlush(io.redirect)
  )
  val s2_has_exception = s2_req.map(x => ExceptionNO.selectByFu(x.uop.exceptionVec, StaCfg).asUInt.orR)

  val s2_enqueue = Wire(Vec(StorePipelineWidth + 1, Bool()))
  for (w <- 0 until StorePipelineWidth + 1) {
    s2_enqueue(w) := s2_valid(w) && s2_has_exception(w)
  }

  when (req_valid && req.uop.robIdx.needFlush(io.redirect)) {
    req_valid := s2_enqueue.asUInt.orR
  }.elsewhen (s2_enqueue.asUInt.orR) {
    req_valid := req_valid || true.B
  }

  def selectOldest[T <: LsPipelineBundle](valid: Seq[Bool], bits: Seq[T]): (Seq[Bool], Seq[T]) = {
    assert(valid.length == bits.length)
    if (valid.length == 0 || valid.length == 1) {
      (valid, bits)
    } else if (valid.length == 2) {
      val res = Seq.fill(2)(Wire(Valid(chiselTypeOf(bits(0)))))
      for (i <- res.indices) {
        res(i).valid := valid(i)
        res(i).bits := bits(i)
      }
      val oldest = Mux(valid(0) && valid(1),
        Mux(isAfter(bits(0).uop.robIdx, bits(1).uop.robIdx) ||
          (isNotBefore(bits(0).uop.robIdx, bits(1).uop.robIdx) && bits(0).uop.uopIdx > bits(1).uop.uopIdx), res(1), res(0)),
        Mux(valid(0) && !valid(1), res(0), res(1)))
      (Seq(oldest.valid), Seq(oldest.bits))
    } else {
      val left = selectOldest(valid.take(valid.length / 2), bits.take(bits.length / 2))
      val right = selectOldest(valid.takeRight(valid.length - (valid.length / 2)), bits.takeRight(bits.length - (bits.length / 2)))
      selectOldest(left._1 ++ right._1, left._2 ++ right._2)
    }
  }

  val reqSel = selectOldest(s2_enqueue, s2_req)

  when (req_valid) {
    req := Mux(reqSel._1(0) && isAfter(req.uop.robIdx, reqSel._2(0).uop.robIdx) ||
      (isNotBefore(req.uop.robIdx, reqSel._2(0).uop.robIdx) && req.uop.uopIdx > reqSel._2(0).uop.uopIdx), reqSel._2(0), req)
  } .elsewhen (s2_enqueue.asUInt.orR) {
    req := reqSel._2(0)
  }

  io.exceptionAddr.vaddr := req.vaddr
}

// Store Queue
class StoreQueue(implicit p: Parameters) extends XSModule
  with HasDCacheParameters
  with HasCircularQueuePtrHelper
  with HasPerfEvents
  with HasVLSUParameters {
  val io = IO(new Bundle() {
    val hartId = Input(UInt(8.W))
    val enq = new SqEnqIO
    val brqRedirect = Flipped(ValidIO(new Redirect))
    val vecFeedback = Flipped(ValidIO(new FeedbackToLsqIO))
    val storeAddrIn = Vec(StorePipelineWidth, Flipped(Valid(new LsPipelineBundle))) // store addr, data is not included
    val storeAddrInRe = Vec(StorePipelineWidth, Input(new LsPipelineBundle())) // store more mmio and exception
    val storeDataIn = Vec(StorePipelineWidth, Flipped(Valid(new MemExuOutput(isVector = true)))) // store data, send to sq from rs
    val storeMaskIn = Vec(StorePipelineWidth, Flipped(Valid(new StoreMaskBundle))) // store mask, send to sq from rs
    val sbuffer = Vec(EnsbufferWidth, Decoupled(new DCacheWordReqWithVaddrAndPfFlag)) // write committed store to sbuffer
    val uncacheOutstanding = Input(Bool())
    val mmioStout = DecoupledIO(new MemExuOutput) // writeback uncached store
    val vecmmioStout = DecoupledIO(new MemExuOutput(isVector = true))
    val forward = Vec(LoadPipelineWidth, Flipped(new PipeLoadForwardQueryIO))
    // TODO: scommit is only for scalar store
    val rob = Flipped(new RobLsqIO)
    val uncache = new UncacheWordIO
    // val refill = Flipped(Valid(new DCacheLineReq ))
    val exceptionAddr = new ExceptionAddrIO
    val sqEmpty = Output(Bool())
    val stAddrReadySqPtr = Output(new SqPtr)
    val stAddrReadyVec = Output(Vec(StoreQueueSize, Bool()))
    val stDataReadySqPtr = Output(new SqPtr)
    val stDataReadyVec = Output(Vec(StoreQueueSize, Bool()))
    val stIssuePtr = Output(new SqPtr)
    val sqDeqPtr = Output(new SqPtr)
    val sqFull = Output(Bool())
    val sqCancelCnt = Output(UInt(log2Up(StoreQueueSize + 1).W))
    val sqDeq = Output(UInt(log2Ceil(EnsbufferWidth + 1).W))
    val force_write = Output(Bool())
  })

  println("StoreQueue: size:" + StoreQueueSize)

  // data modules
  val uop = Reg(Vec(StoreQueueSize, new DynInst))
  // val data = Reg(Vec(StoreQueueSize, new LsqEntry))
  val dataModule = Module(new SQDataModule(
    numEntries = StoreQueueSize,
    numRead = EnsbufferWidth,
    numWrite = StorePipelineWidth,
    numForward = LoadPipelineWidth
  ))
  dataModule.io := DontCare
  val paddrModule = Module(new SQAddrModule(
    dataWidth = PAddrBits,
    numEntries = StoreQueueSize,
    numRead = EnsbufferWidth,
    numWrite = StorePipelineWidth,
    numForward = LoadPipelineWidth
  ))
  paddrModule.io := DontCare
  val vaddrModule = Module(new SQAddrModule(
    dataWidth = VAddrBits,
    numEntries = StoreQueueSize,
    numRead = EnsbufferWidth, // sbuffer; badvaddr will be sent from exceptionBuffer
    numWrite = StorePipelineWidth,
    numForward = LoadPipelineWidth
  ))
  vaddrModule.io := DontCare
  val dataBuffer = Module(new DatamoduleResultBuffer(new DataBufferEntry))
  val exceptionBuffer = Module(new StoreExceptionBuffer)
  exceptionBuffer.io.redirect := io.brqRedirect
  exceptionBuffer.io.exceptionAddr.isStore := DontCare
  // TODO: implement it!
  exceptionBuffer.io.storeAddrIn(StorePipelineWidth) := DontCare

  val debug_paddr = Reg(Vec(StoreQueueSize, UInt((PAddrBits).W)))
  val debug_vaddr = Reg(Vec(StoreQueueSize, UInt((VAddrBits).W)))
  val debug_data = Reg(Vec(StoreQueueSize, UInt((XLEN).W)))

  // state & misc
  val allocated = RegInit(VecInit(List.fill(StoreQueueSize)(false.B))) // sq entry has been allocated
  val addrvalid = RegInit(VecInit(List.fill(StoreQueueSize)(false.B))) // non-mmio addr is valid
  val datavalid = RegInit(VecInit(List.fill(StoreQueueSize)(false.B))) // non-mmio data is valid
  val allvalid  = VecInit((0 until StoreQueueSize).map(i => addrvalid(i) && datavalid(i))) // non-mmio data & addr is valid
  val committed = RegInit(VecInit(List.fill(StoreQueueSize)(false.B))) // inst has been committed by rob
  val pending = RegInit(VecInit(List.fill(StoreQueueSize)(false.B))) // mmio pending: inst is an mmio inst, it will not be executed until it reachs the end of rob
  val mmio = RegInit(VecInit(List.fill(StoreQueueSize)(false.B))) // mmio: inst is an mmio inst
  val atomic = RegInit(VecInit(List.fill(StoreQueueSize)(false.B)))
  val prefetch = RegInit(VecInit(List.fill(StoreQueueSize)(false.B))) // need prefetch when committing this store to sbuffer?
  val isVec = RegInit(VecInit(List.fill(StoreQueueSize)(false.B))) // vector store instruction
  //val vec_lastuop = Reg(Vec(StoreQueueSize, Bool())) // last uop of vector store instruction
  val vecMbCommit = RegInit(VecInit(List.fill(StoreQueueSize)(false.B))) // vector store committed from merge buffer to rob
  val vecDataValid = RegInit(VecInit(List.fill(StoreQueueSize)(false.B))) // vector store need write to sbuffer
  // val vec_robCommit = Reg(Vec(StoreQueueSize, Bool())) // vector store committed by rob
  // val vec_secondInv = RegInit(VecInit(List.fill(StoreQueueSize)(false.B))) // Vector unit-stride, second entry is invalid

  // ptr
  val enqPtrExt = RegInit(VecInit((0 until io.enq.req.length).map(_.U.asTypeOf(new SqPtr))))
  val rdataPtrExt = RegInit(VecInit((0 until EnsbufferWidth).map(_.U.asTypeOf(new SqPtr))))
  val deqPtrExt = RegInit(VecInit((0 until EnsbufferWidth).map(_.U.asTypeOf(new SqPtr))))
  val cmtPtrExt = RegInit(VecInit((0 until CommitWidth).map(_.U.asTypeOf(new SqPtr))))
  val addrReadyPtrExt = RegInit(0.U.asTypeOf(new SqPtr))
  val dataReadyPtrExt = RegInit(0.U.asTypeOf(new SqPtr))

  val enqPtr = enqPtrExt(0).value
  val deqPtr = deqPtrExt(0).value
  val cmtPtr = cmtPtrExt(0).value

  val validCount = distanceBetween(enqPtrExt(0), deqPtrExt(0))
  val allowEnqueue = validCount <= (StoreQueueSize - LSQStEnqWidth).U

  val deqMask = UIntToMask(deqPtr, StoreQueueSize)
  val enqMask = UIntToMask(enqPtr, StoreQueueSize)

  // TODO: count commit numbers for scalar / vector store separately
  val scalarCommitCount = RegInit(0.U(log2Ceil(StoreQueueSize + 1).W))
  val scalarCommitted = WireInit(0.U(log2Ceil(CommitWidth + 1).W))
  val vecCommitted = WireInit(0.U(log2Ceil(CommitWidth + 1).W))
  val commitCount = WireInit(0.U(log2Ceil(CommitWidth + 1).W))
  val scommit = RegNext(io.rob.scommit)

  scalarCommitCount := scalarCommitCount + scommit - scalarCommitted

  // store can be committed by ROB
  io.rob.mmio := DontCare
  io.rob.uop := DontCare

  // Read dataModule
  assert(EnsbufferWidth <= 2)
  // rdataPtrExtNext and rdataPtrExtNext+1 entry will be read from dataModule
  val rdataPtrExtNext = WireInit(Mux(dataBuffer.io.enq(1).fire,
    VecInit(rdataPtrExt.map(_ + 2.U)),
    Mux(dataBuffer.io.enq(0).fire || io.mmioStout.fire || io.vecmmioStout.fire,
      VecInit(rdataPtrExt.map(_ + 1.U)),
      rdataPtrExt
    )
  ))

  // deqPtrExtNext traces which inst is about to leave store queue
  //
  // io.sbuffer(i).fire is RegNexted, as sbuffer data write takes 2 cycles.
  // Before data write finish, sbuffer is unable to provide store to load
  // forward data. As an workaround, deqPtrExt and allocated flag update
  // is delayed so that load can get the right data from store queue.
  //
  // Modify deqPtrExtNext and io.sqDeq with care!
  val deqPtrExtNext = Mux(RegNext(io.sbuffer(1).fire),
    VecInit(deqPtrExt.map(_ + 2.U)),
    Mux((RegNext(io.sbuffer(0).fire)) || io.mmioStout.fire || io.vecmmioStout.fire,
      VecInit(deqPtrExt.map(_ + 1.U)),
      deqPtrExt
    )
  )
  io.sqDeq := RegNext(Mux(RegNext(io.sbuffer(1).fire), 2.U,
    Mux((RegNext(io.sbuffer(0).fire)) || io.mmioStout.fire || io.vecmmioStout.fire, 1.U, 0.U)
  ))
  assert(!RegNext(RegNext(io.sbuffer(0).fire) && (io.mmioStout.fire || io.vecmmioStout.fire)))

  for (i <- 0 until EnsbufferWidth) {
    dataModule.io.raddr(i) := rdataPtrExtNext(i).value
    paddrModule.io.raddr(i) := rdataPtrExtNext(i).value
    vaddrModule.io.raddr(i) := rdataPtrExtNext(i).value
  }

  /**
    * Enqueue at dispatch
    *
    * Currently, StoreQueue only allows enqueue when #emptyEntries > EnqWidth
    */
  io.enq.canAccept := allowEnqueue
  val canEnqueue = io.enq.req.map(_.valid)
  val enqCancel = io.enq.req.map(_.bits.robIdx.needFlush(io.brqRedirect))
  val vStoreFlow = io.enq.req.map(_.bits.numLsElem)
  val validVStoreFlow = vStoreFlow.zipWithIndex.map{case (vLoadFlowNum_Item, index) => Mux(!RegNext(io.brqRedirect.valid) && io.enq.canAccept && io.enq.lqCanAccept && canEnqueue(index), vLoadFlowNum_Item, 0.U)}
  val validVStoreOffset = vStoreFlow.zip(io.enq.needAlloc).map{case (flow, needAlloc_Item) => Mux(needAlloc_Item, flow, 0.U)}
  val validVStoreOffsetRShift = 0.U +: validVStoreOffset.take(vStoreFlow.length - 1)

  for (i <- 0 until io.enq.req.length) {
    val sqIdx = enqPtrExt(0) + validVStoreOffsetRShift.take(i + 1).reduce(_ + _)
    val index = io.enq.req(i).bits.sqIdx.value
    val enqInstr = io.enq.req(i).bits.instr.asTypeOf(new XSInstBitFields)
    when (canEnqueue(i) && !enqCancel(i)) {
      for (j <- 0 until VecMemDispatchMaxNumber) {
        when (j.U < validVStoreOffset(i)) {
          uop(index + j.U) := io.enq.req(i).bits
          // NOTE: the index will be used when replay
          uop(index + j.U).sqIdx := sqIdx + j.U
          allocated(index + j.U) := true.B
          datavalid(index + j.U) := false.B
          addrvalid(index + j.U) := false.B
          committed(index + j.U) := false.B
          pending(index + j.U) := false.B
          prefetch(index + j.U) := false.B
          mmio(index + j.U) := false.B
          isVec(index + j.U) := enqInstr.isVecStore // check vector store by the encoding of inst
          vecDataValid(index + j.U) := false.B
          XSError(!io.enq.canAccept || !io.enq.lqCanAccept, s"must accept $i\n")
          XSError(index =/= sqIdx.value, s"must be the same entry $i\n")
        }
      }
    }
    io.enq.resp(i) := sqIdx
  }
  XSDebug(p"(ready, valid): ${io.enq.canAccept}, ${Binary(Cat(io.enq.req.map(_.valid)))}\n")

  /**
    * Update addr/dataReadyPtr when issue from rs
    */
  // update issuePtr
  val IssuePtrMoveStride = 4
  require(IssuePtrMoveStride >= 2)

  val addrReadyLookupVec = (0 until IssuePtrMoveStride).map(addrReadyPtrExt + _.U)
  val addrReadyLookup = addrReadyLookupVec.map(ptr => allocated(ptr.value) &&
   (mmio(ptr.value) || addrvalid(ptr.value) || vecMbCommit(ptr.value))
    && ptr =/= enqPtrExt(0))
  val nextAddrReadyPtr = addrReadyPtrExt + PriorityEncoder(VecInit(addrReadyLookup.map(!_) :+ true.B))
  addrReadyPtrExt := nextAddrReadyPtr

  (0 until StoreQueueSize).map(i => {
    io.stAddrReadyVec(i) := RegNext(allocated(i) && (mmio(i) || addrvalid(i)))
  })

  when (io.brqRedirect.valid) {
    addrReadyPtrExt := Mux(
      isAfter(cmtPtrExt(0), deqPtrExt(0)),
      cmtPtrExt(0),
      deqPtrExtNext(0) // for mmio insts, deqPtr may be ahead of cmtPtr
    )
  }

  io.stAddrReadySqPtr := addrReadyPtrExt

  // update
  val dataReadyLookupVec = (0 until IssuePtrMoveStride).map(dataReadyPtrExt + _.U)
  val dataReadyLookup = dataReadyLookupVec.map(ptr => allocated(ptr.value) &&
   (mmio(ptr.value) || datavalid(ptr.value) || vecMbCommit(ptr.value))
    && ptr =/= enqPtrExt(0))
  val nextDataReadyPtr = dataReadyPtrExt + PriorityEncoder(VecInit(dataReadyLookup.map(!_) :+ true.B))
  dataReadyPtrExt := nextDataReadyPtr

  (0 until StoreQueueSize).map(i => {
    io.stDataReadyVec(i) := RegNext(allocated(i) && (mmio(i) || datavalid(i)))
  })

  when (io.brqRedirect.valid) {
    dataReadyPtrExt := Mux(
      isAfter(cmtPtrExt(0), deqPtrExt(0)),
      cmtPtrExt(0),
      deqPtrExtNext(0) // for mmio insts, deqPtr may be ahead of cmtPtr
    )
  }

  io.stDataReadySqPtr := dataReadyPtrExt
  io.stIssuePtr := enqPtrExt(0)
  io.sqDeqPtr := deqPtrExt(0)

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
    val stWbIndex = io.storeAddrIn(i).bits.uop.sqIdx.value
    exceptionBuffer.io.storeAddrIn(i).valid := io.storeAddrIn(i).fire && !io.storeAddrIn(i).bits.miss && !io.storeAddrIn(i).bits.isvec
    exceptionBuffer.io.storeAddrIn(i).bits := io.storeAddrIn(i).bits

    when (io.storeAddrIn(i).fire) {
      val addr_valid = !io.storeAddrIn(i).bits.miss
      addrvalid(stWbIndex) := addr_valid //!io.storeAddrIn(i).bits.mmio
      // pending(stWbIndex) := io.storeAddrIn(i).bits.mmio

      paddrModule.io.waddr(i) := stWbIndex
      paddrModule.io.wdata(i) := io.storeAddrIn(i).bits.paddr
      paddrModule.io.wmask(i) := io.storeAddrIn(i).bits.mask
      paddrModule.io.wlineflag(i) := io.storeAddrIn(i).bits.wlineflag
      paddrModule.io.wen(i) := true.B

      vaddrModule.io.waddr(i) := stWbIndex
      vaddrModule.io.wdata(i) := io.storeAddrIn(i).bits.vaddr
      vaddrModule.io.wmask(i) := io.storeAddrIn(i).bits.mask
      vaddrModule.io.wlineflag(i) := io.storeAddrIn(i).bits.wlineflag
      vaddrModule.io.wen(i) := true.B

      debug_paddr(paddrModule.io.waddr(i)) := paddrModule.io.wdata(i)

      // mmio(stWbIndex) := io.storeAddrIn(i).bits.mmio

      uop(stWbIndex) := io.storeAddrIn(i).bits.uop
      uop(stWbIndex).debugInfo := io.storeAddrIn(i).bits.uop.debugInfo

      vecDataValid(stWbIndex) := io.storeAddrIn(i).bits.isvec

      XSInfo("store addr write to sq idx %d pc 0x%x miss:%d vaddr %x paddr %x mmio %x isvec %x\n",
        io.storeAddrIn(i).bits.uop.sqIdx.value,
        io.storeAddrIn(i).bits.uop.pc,
        io.storeAddrIn(i).bits.miss,
        io.storeAddrIn(i).bits.vaddr,
        io.storeAddrIn(i).bits.paddr,
        io.storeAddrIn(i).bits.mmio,
        io.storeAddrIn(i).bits.isvec
      )
    }

    // re-replinish mmio, for pma/pmp will get mmio one cycle later
    val storeAddrInFireReg = RegNext(io.storeAddrIn(i).fire && !io.storeAddrIn(i).bits.miss)
    val stWbIndexReg = RegNext(stWbIndex)
    when (storeAddrInFireReg) {
      pending(stWbIndexReg) := io.storeAddrInRe(i).mmio
      mmio(stWbIndexReg) := io.storeAddrInRe(i).mmio
      atomic(stWbIndexReg) := io.storeAddrInRe(i).atomic
    }
    // dcache miss info (one cycle later than storeIn)
    // if dcache report a miss in sta pipeline, this store will trigger a prefetch when committing to sbuffer (if EnableAtCommitMissTrigger)
    when (storeAddrInFireReg) {
      prefetch(stWbIndexReg) := io.storeAddrInRe(i).miss
    }

    when(vaddrModule.io.wen(i)){
      debug_vaddr(vaddrModule.io.waddr(i)) := vaddrModule.io.wdata(i)
    }
  }

  // Write data to sq
  // Now store data pipeline is actually 2 stages
  for (i <- 0 until StorePipelineWidth) {
    dataModule.io.data.wen(i) := false.B
    val stWbIndex = io.storeDataIn(i).bits.uop.sqIdx.value
    // sq data write takes 2 cycles:
    // sq data write s0
    when (io.storeDataIn(i).fire) {
      // send data write req to data module
      dataModule.io.data.waddr(i) := stWbIndex
      dataModule.io.data.wdata(i) := Mux(io.storeDataIn(i).bits.uop.fuOpType === LSUOpType.cbo_zero,
        0.U,
        genVWdata(io.storeDataIn(i).bits.data, io.storeDataIn(i).bits.uop.fuOpType(2,0))
      )
      dataModule.io.data.wen(i) := true.B

      debug_data(dataModule.io.data.waddr(i)) := dataModule.io.data.wdata(i)

      XSInfo("store data write to sq idx %d pc 0x%x data %x -> %x\n",
        io.storeDataIn(i).bits.uop.sqIdx.value,
        io.storeDataIn(i).bits.uop.pc,
        io.storeDataIn(i).bits.data,
        dataModule.io.data.wdata(i)
      )
    }
    // sq data write s1
    when (
      RegNext(io.storeDataIn(i).fire)
      // && !RegNext(io.storeDataIn(i).bits.uop).robIdx.needFlush(io.brqRedirect)
    ) {
      datavalid(RegNext(stWbIndex)) := true.B
    }
  }

  // Write mask to sq
  for (i <- 0 until StorePipelineWidth) {
    // sq mask write s0
    when (io.storeMaskIn(i).fire) {
      // send data write req to data module
      dataModule.io.mask.waddr(i) := io.storeMaskIn(i).bits.sqIdx.value
      dataModule.io.mask.wdata(i) := io.storeMaskIn(i).bits.mask
      dataModule.io.mask.wen(i) := true.B
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
    // (2) if they have different flags, we need to check range(tail, VirtualLoadQueueSize) and range(0, sqIdx)
    // Forward1: Mux(same_flag, range(tail, sqIdx), range(tail, VirtualLoadQueueSize))
    // Forward2: Mux(same_flag, 0.U,                   range(0, sqIdx)    )
    // i.e. forward1 is the target entries with the same flag bits and forward2 otherwise
    val differentFlag = deqPtrExt(0).flag =/= io.forward(i).sqIdx.flag
    val forwardMask = io.forward(i).sqIdxMask
    // all addrvalid terms need to be checked
    // Real Vaild: all scalar stores, and vector store with (!inactive && !secondInvalid)
    val addrRealValidVec = WireInit(VecInit((0 until StoreQueueSize).map(j => addrvalid(j) && allocated(j))))
    // vector store will consider all inactive || secondInvalid flows as valid
    val addrValidVec = WireInit(VecInit((0 until StoreQueueSize).map(j => addrvalid(j) && allocated(j))))
    val dataValidVec = WireInit(VecInit((0 until StoreQueueSize).map(j => datavalid(j))))
    val allValidVec  = WireInit(VecInit((0 until StoreQueueSize).map(j => addrvalid(j) && datavalid(j) && allocated(j))))

    val lfstEnable = Constantin.createRecord("LFSTEnable", LFSTEnable.B).orR
    val storeSetHitVec = Mux(lfstEnable,
      WireInit(VecInit((0 until StoreQueueSize).map(j => io.forward(i).uop.loadWaitBit && uop(j).robIdx === io.forward(i).uop.waitForRobIdx))),
      WireInit(VecInit((0 until StoreQueueSize).map(j => uop(j).storeSetHit && uop(j).ssid === io.forward(i).uop.ssid)))
    )

    val forwardMask1 = Mux(differentFlag, ~deqMask, deqMask ^ forwardMask)
    val forwardMask2 = Mux(differentFlag, forwardMask, 0.U(StoreQueueSize.W))
    val canForward1 = forwardMask1 & allValidVec.asUInt
    val canForward2 = forwardMask2 & allValidVec.asUInt
    val needForward = Mux(differentFlag, ~deqMask | forwardMask, deqMask ^ forwardMask)

    XSDebug(p"$i f1 ${Binary(canForward1)} f2 ${Binary(canForward2)} " +
      p"sqIdx ${io.forward(i).sqIdx} pa ${Hexadecimal(io.forward(i).paddr)}\n"
    )

    // do real fwd query (cam lookup in load_s1)
    dataModule.io.needForward(i)(0) := canForward1 & vaddrModule.io.forwardMmask(i).asUInt
    dataModule.io.needForward(i)(1) := canForward2 & vaddrModule.io.forwardMmask(i).asUInt

    vaddrModule.io.forwardMdata(i) := io.forward(i).vaddr
    vaddrModule.io.forwardDataMask(i) := io.forward(i).mask
    paddrModule.io.forwardMdata(i) := io.forward(i).paddr
    paddrModule.io.forwardDataMask(i) := io.forward(i).mask


    // vaddr cam result does not equal to paddr cam result
    // replay needed
    // val vpmaskNotEqual = ((paddrModule.io.forwardMmask(i).asUInt ^ vaddrModule.io.forwardMmask(i).asUInt) & needForward) =/= 0.U
    // val vaddrMatchFailed = vpmaskNotEqual && io.forward(i).valid
    val vpmaskNotEqual = (
      (RegNext(paddrModule.io.forwardMmask(i).asUInt) ^ RegNext(vaddrModule.io.forwardMmask(i).asUInt)) &
      RegNext(needForward) &
      RegNext(addrRealValidVec.asUInt)
    ) =/= 0.U
    val vaddrMatchFailed = vpmaskNotEqual && RegNext(io.forward(i).valid)
    when (vaddrMatchFailed) {
      XSInfo("vaddrMatchFailed: pc %x pmask %x vmask %x\n",
        RegNext(io.forward(i).uop.pc),
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
    val dataInvalidMask1 = (addrValidVec.asUInt & ~dataValidVec.asUInt & vaddrModule.io.forwardMmask(i).asUInt & forwardMask1.asUInt)
    val dataInvalidMask2 = (addrValidVec.asUInt & ~dataValidVec.asUInt & vaddrModule.io.forwardMmask(i).asUInt & forwardMask2.asUInt)
    val dataInvalidMask = dataInvalidMask1 | dataInvalidMask2
    io.forward(i).dataInvalidFast := dataInvalidMask.orR

    // make chisel happy
    val dataInvalidMask1Reg = Wire(UInt(StoreQueueSize.W))
    dataInvalidMask1Reg := RegNext(dataInvalidMask1)
    // make chisel happy
    val dataInvalidMask2Reg = Wire(UInt(StoreQueueSize.W))
    dataInvalidMask2Reg := RegNext(dataInvalidMask2)
    val dataInvalidMaskReg = dataInvalidMask1Reg | dataInvalidMask2Reg

    // If SSID match, address not ready, mark it as addrInvalid
    // load_s2: generate addrInvalid
    val addrInvalidMask1 = (~addrValidVec.asUInt & storeSetHitVec.asUInt & forwardMask1.asUInt)
    val addrInvalidMask2 = (~addrValidVec.asUInt & storeSetHitVec.asUInt & forwardMask2.asUInt)
    // make chisel happy
    val addrInvalidMask1Reg = Wire(UInt(StoreQueueSize.W))
    addrInvalidMask1Reg := RegNext(addrInvalidMask1)
    // make chisel happy
    val addrInvalidMask2Reg = Wire(UInt(StoreQueueSize.W))
    addrInvalidMask2Reg := RegNext(addrInvalidMask2)
    val addrInvalidMaskReg = addrInvalidMask1Reg | addrInvalidMask2Reg

    // load_s2
    io.forward(i).dataInvalid := RegNext(io.forward(i).dataInvalidFast)
    // check if vaddr forward mismatched
    io.forward(i).matchInvalid := vaddrMatchFailed

    // data invalid sq index
    // check whether false fail
    // check flag
    val s2_differentFlag = RegNext(differentFlag)
    val s2_enqPtrExt = RegNext(enqPtrExt(0))
    val s2_deqPtrExt = RegNext(deqPtrExt(0))

    // addr invalid sq index
    // make chisel happy
    val addrInvalidMaskRegWire = Wire(UInt(StoreQueueSize.W))
    addrInvalidMaskRegWire := addrInvalidMaskReg
    val addrInvalidFlag = addrInvalidMaskRegWire.orR
    val hasInvalidAddr = (~addrValidVec.asUInt & needForward).orR

    val addrInvalidSqIdx1 = OHToUInt(Reverse(PriorityEncoderOH(Reverse(addrInvalidMask1Reg))))
    val addrInvalidSqIdx2 = OHToUInt(Reverse(PriorityEncoderOH(Reverse(addrInvalidMask2Reg))))
    val addrInvalidSqIdx = Mux(addrInvalidMask2Reg.orR, addrInvalidSqIdx2, addrInvalidSqIdx1)

    // store-set content management
    //                +-----------------------+
    //                | Search a SSID for the |
    //                |    load operation     |
    //                +-----------------------+
    //                           |
    //                           V
    //                 +-------------------+
    //                 | load wait strict? |
    //                 +-------------------+
    //                           |
    //                           V
    //               +----------------------+
    //            Set|                      |Clean
    //               V                      V
    //  +------------------------+   +------------------------------+
    //  | Waiting for all older  |   | Wait until the corresponding |
    //  |   stores operations    |   | older store operations       |
    //  +------------------------+   +------------------------------+



    when (RegNext(io.forward(i).uop.loadWaitStrict)) {
      io.forward(i).addrInvalidSqIdx := RegNext(io.forward(i).uop.sqIdx - 1.U)
    } .elsewhen (addrInvalidFlag) {
      io.forward(i).addrInvalidSqIdx.flag := Mux(!s2_differentFlag || addrInvalidSqIdx >= s2_deqPtrExt.value, s2_deqPtrExt.flag, s2_enqPtrExt.flag)
      io.forward(i).addrInvalidSqIdx.value := addrInvalidSqIdx
    } .otherwise {
      // may be store inst has been written to sbuffer already.
      io.forward(i).addrInvalidSqIdx := RegNext(io.forward(i).uop.sqIdx)
    }
    io.forward(i).addrInvalid := Mux(RegNext(io.forward(i).uop.loadWaitStrict), RegNext(hasInvalidAddr), addrInvalidFlag)

    // data invalid sq index
    // make chisel happy
    val dataInvalidMaskRegWire = Wire(UInt(StoreQueueSize.W))
    dataInvalidMaskRegWire := dataInvalidMaskReg
    val dataInvalidFlag = dataInvalidMaskRegWire.orR

    val dataInvalidSqIdx1 = OHToUInt(Reverse(PriorityEncoderOH(Reverse(dataInvalidMask1Reg))))
    val dataInvalidSqIdx2 = OHToUInt(Reverse(PriorityEncoderOH(Reverse(dataInvalidMask2Reg))))
    val dataInvalidSqIdx = Mux(dataInvalidMask2Reg.orR, dataInvalidSqIdx2, dataInvalidSqIdx1)

    when (dataInvalidFlag) {
      io.forward(i).dataInvalidSqIdx.flag := Mux(!s2_differentFlag || dataInvalidSqIdx >= s2_deqPtrExt.value, s2_deqPtrExt.flag, s2_enqPtrExt.flag)
      io.forward(i).dataInvalidSqIdx.value := dataInvalidSqIdx
    } .otherwise {
      // may be store inst has been written to sbuffer already.
      io.forward(i).dataInvalidSqIdx := RegNext(io.forward(i).uop.sqIdx)
    }
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
  // TODO: CAN NOT deal with vector mmio now!
  val s_idle :: s_req :: s_resp :: s_wb :: s_wait :: Nil = Enum(5)
  val uncacheState = RegInit(s_idle)
  switch(uncacheState) {
    is(s_idle) {
      when(RegNext(io.rob.pendingst && pending(deqPtr) && allocated(deqPtr) && datavalid(deqPtr) && addrvalid(deqPtr))) {
        uncacheState := s_req
      }
    }
    is(s_req) {
      when (io.uncache.req.fire) {
        when (io.uncacheOutstanding) {
          uncacheState := s_wb
        } .otherwise {
          uncacheState := s_resp
        }
      }
    }
    is(s_resp) {
      when(io.uncache.resp.fire) {
        uncacheState := s_wb
      }
    }
    is(s_wb) {
      when (io.mmioStout.fire || io.vecmmioStout.fire) {
        uncacheState := s_wait
      }
    }
    is(s_wait) {
      // A MMIO store can always move cmtPtrExt as it must be ROB head
      when(scommit > 0.U) {
        uncacheState := s_idle // ready for next mmio
      }
    }
  }
  io.uncache.req.valid := uncacheState === s_req

  io.uncache.req.bits := DontCare
  io.uncache.req.bits.cmd  := MemoryOpConstants.M_XWR
  io.uncache.req.bits.addr := paddrModule.io.rdata(0) // data(deqPtr) -> rdata(0)
  io.uncache.req.bits.data := shiftDataToLow(paddrModule.io.rdata(0), dataModule.io.rdata(0).data)
  io.uncache.req.bits.mask := shiftMaskToLow(paddrModule.io.rdata(0), dataModule.io.rdata(0).mask)

  // CBO op type check can be delayed for 1 cycle,
  // as uncache op will not start in s_idle
  val cbo_mmio_addr = paddrModule.io.rdata(0) >> 2 << 2 // clear lowest 2 bits for op
  val cbo_mmio_op = 0.U //TODO
  val cbo_mmio_data = cbo_mmio_addr | cbo_mmio_op
  when(RegNext(LSUOpType.isCbo(uop(deqPtr).fuOpType))){
    io.uncache.req.bits.addr := DontCare // TODO
    io.uncache.req.bits.data := paddrModule.io.rdata(0)
    io.uncache.req.bits.mask := DontCare // TODO
  }

  io.uncache.req.bits.atomic := atomic(RegNext(rdataPtrExtNext(0)).value)

  when(io.uncache.req.fire){
    // mmio store should not be committed until uncache req is sent
    pending(deqPtr) := false.B

    XSDebug(
      p"uncache req: pc ${Hexadecimal(uop(deqPtr).pc)} " +
      p"addr ${Hexadecimal(io.uncache.req.bits.addr)} " +
      p"data ${Hexadecimal(io.uncache.req.bits.data)} " +
      p"op ${Hexadecimal(io.uncache.req.bits.cmd)} " +
      p"mask ${Hexadecimal(io.uncache.req.bits.mask)}\n"
    )
  }

  // (3) response from uncache channel: mark as datavalid
  io.uncache.resp.ready := true.B

  // (4) scalar store: writeback to ROB (and other units): mark as writebacked
  io.mmioStout.valid := uncacheState === s_wb && !isVec(deqPtr)
  io.mmioStout.bits.uop := uop(deqPtr)
  io.mmioStout.bits.uop.sqIdx := deqPtrExt(0)
  io.mmioStout.bits.data := shiftDataToLow(paddrModule.io.rdata(0), dataModule.io.rdata(0).data) // dataModule.io.rdata.read(deqPtr)
  io.mmioStout.bits.debug.isMMIO := true.B
  io.mmioStout.bits.debug.paddr := DontCare
  io.mmioStout.bits.debug.isPerfCnt := false.B
  io.mmioStout.bits.debug.vaddr := DontCare
  // Remove MMIO inst from store queue after MMIO request is being sent
  // That inst will be traced by uncache state machine
  when (io.mmioStout.fire) {
    allocated(deqPtr) := false.B
  }

  // (4) or vector store:
  // TODO: implement it!
  io.vecmmioStout := DontCare
  io.vecmmioStout.valid := uncacheState === s_wb && isVec(deqPtr)
  io.vecmmioStout.bits.uop := uop(deqPtr)
  io.vecmmioStout.bits.uop.sqIdx := deqPtrExt(0)
  io.vecmmioStout.bits.data := shiftDataToLow(paddrModule.io.rdata(0), dataModule.io.rdata(0).data) // dataModule.io.rdata.read(deqPtr)
  io.vecmmioStout.bits.debug.isMMIO := true.B
  io.vecmmioStout.bits.debug.paddr := DontCare
  io.vecmmioStout.bits.debug.isPerfCnt := false.B
  io.vecmmioStout.bits.debug.vaddr := DontCare
  // Remove MMIO inst from store queue after MMIO request is being sent
  // That inst will be traced by uncache state machine
  when (io.vecmmioStout.fire) {
    allocated(deqPtr) := false.B
  }

  /**
    * ROB commits store instructions (mark them as committed)
    *
    * (1) When store commits, mark it as committed.
    * (2) They will not be cancelled and can be sent to lower level.
    */
  XSError(uncacheState =/= s_idle && uncacheState =/= s_wait && commitCount > 0.U,
   "should not commit instruction when MMIO has not been finished\n")

  val scalarcommitVec = WireInit(VecInit(Seq.fill(CommitWidth)(false.B)))
  val veccommitVec = WireInit(VecInit(Seq.fill(CommitWidth)(false.B)))
  // TODO: Deal with vector store mmio
  for (i <- 0 until CommitWidth) {
    val veccount = PopCount(veccommitVec.take(i))
    when (isVec(cmtPtrExt(i).value) && isNotAfter(uop(cmtPtrExt(i).value).robIdx, io.rob.pendingPtr) && vecMbCommit(cmtPtrExt(i).value)) {
      if (i == 0){
        // TODO: fixme for vector mmio
        when ((uncacheState === s_idle) || (uncacheState === s_wait && scommit > 0.U)){
          committed(cmtPtrExt(0).value) := true.B
          veccommitVec(i) := true.B
        }
      } else {
        committed(cmtPtrExt(i).value) := true.B
        veccommitVec(i) := true.B
      }
    } .elsewhen (scalarCommitCount > i.U - veccount) {
      if (i == 0){
        when ((uncacheState === s_idle) || (uncacheState === s_wait && scommit > 0.U)){
          committed(cmtPtrExt(0).value) := true.B
          scalarcommitVec(i) := true.B
        }
      } else {
        committed(cmtPtrExt(i).value) := true.B
        scalarcommitVec(i) := true.B
      }
    }
  }

  scalarCommitted := PopCount(scalarcommitVec)
  vecCommitted := PopCount(veccommitVec)
  commitCount := scalarCommitted + vecCommitted

  cmtPtrExt := cmtPtrExt.map(_ + commitCount)

  // committed stores will not be cancelled and can be sent to lower level.
  // remove retired insts from sq, add retired store to sbuffer

  // Read data from data module
  // As store queue grows larger and larger, time needed to read data from data
  // module keeps growing higher. Now we give data read a whole cycle.
  val mmioStall = mmio(rdataPtrExt(0).value)
  for (i <- 0 until EnsbufferWidth) {
    val ptr = rdataPtrExt(i).value
    dataBuffer.io.enq(i).valid := allocated(ptr) && committed(ptr) && (!isVec(ptr) || vecMbCommit(ptr)) && !mmioStall
    // Note that store data/addr should both be valid after store's commit
    assert(!dataBuffer.io.enq(i).valid || allvalid(ptr) || (allocated(ptr) && vecMbCommit(ptr)))
    dataBuffer.io.enq(i).bits.addr     := paddrModule.io.rdata(i)
    dataBuffer.io.enq(i).bits.vaddr    := vaddrModule.io.rdata(i)
    dataBuffer.io.enq(i).bits.data     := dataModule.io.rdata(i).data
    dataBuffer.io.enq(i).bits.mask     := dataModule.io.rdata(i).mask
    dataBuffer.io.enq(i).bits.wline    := paddrModule.io.rlineflag(i)
    dataBuffer.io.enq(i).bits.sqPtr    := rdataPtrExt(i)
    dataBuffer.io.enq(i).bits.prefetch := prefetch(ptr)
    dataBuffer.io.enq(i).bits.vecValid := !isVec(ptr) || vecDataValid(ptr) // scalar is always valid
  }

  // Send data stored in sbufferReqBitsReg to sbuffer
  for (i <- 0 until EnsbufferWidth) {
    io.sbuffer(i).valid := dataBuffer.io.deq(i).valid
    dataBuffer.io.deq(i).ready := io.sbuffer(i).ready
    // Write line request should have all 1 mask
    assert(!(io.sbuffer(i).valid && io.sbuffer(i).bits.wline && io.sbuffer(i).bits.vecValid && !io.sbuffer(i).bits.mask.andR))
    io.sbuffer(i).bits := DontCare
    io.sbuffer(i).bits.cmd   := MemoryOpConstants.M_XWR
    io.sbuffer(i).bits.addr  := dataBuffer.io.deq(i).bits.addr
    io.sbuffer(i).bits.vaddr := dataBuffer.io.deq(i).bits.vaddr
    io.sbuffer(i).bits.data  := dataBuffer.io.deq(i).bits.data
    io.sbuffer(i).bits.mask  := dataBuffer.io.deq(i).bits.mask
    io.sbuffer(i).bits.wline := dataBuffer.io.deq(i).bits.wline
    io.sbuffer(i).bits.prefetch := dataBuffer.io.deq(i).bits.prefetch
    io.sbuffer(i).bits.vecValid := dataBuffer.io.deq(i).bits.vecValid

    // io.sbuffer(i).fire is RegNexted, as sbuffer data write takes 2 cycles.
    // Before data write finish, sbuffer is unable to provide store to load
    // forward data. As an workaround, deqPtrExt and allocated flag update
    // is delayed so that load can get the right data from store queue.
    val ptr = dataBuffer.io.deq(i).bits.sqPtr.value
    when (RegNext(io.sbuffer(i).fire)) {
      allocated(RegEnable(ptr, io.sbuffer(i).fire)) := false.B
      XSDebug("sbuffer "+i+" fire: ptr %d\n", ptr)
    }
  }
  (1 until EnsbufferWidth).foreach(i => when(io.sbuffer(i).fire) { assert(io.sbuffer(i - 1).fire) })
  if (coreParams.dcacheParametersOpt.isEmpty) {
    for (i <- 0 until EnsbufferWidth) {
      val ptr = deqPtrExt(i).value
      val ram = DifftestMem(64L * 1024 * 1024 * 1024, 8)
      val wen = allocated(ptr) && committed(ptr) && !mmio(ptr)
      val waddr = ((paddrModule.io.rdata(i) - "h80000000".U) >> 3).asUInt
      val wdata = Mux(paddrModule.io.rdata(i)(3), dataModule.io.rdata(i).data(127, 64), dataModule.io.rdata(i).data(63, 0))
      val wmask = Mux(paddrModule.io.rdata(i)(3), dataModule.io.rdata(i).mask(15, 8), dataModule.io.rdata(i).mask(7, 0))
      when (wen) {
        ram.write(waddr, wdata.asTypeOf(Vec(8, UInt(8.W))), wmask.asBools)
      }
    }
  }

  // Read vaddr for mem exception
  io.exceptionAddr.vaddr := exceptionBuffer.io.exceptionAddr.vaddr

  // vector commit or replay from
  val vecCommit = Wire(Vec(StoreQueueSize, Bool()))
  for (i <- 0 until StoreQueueSize) {
    val fbk = io.vecFeedback
    vecCommit(i) := fbk.valid && fbk.bits.isCommit && uop(i).robIdx === fbk.bits.robidx && uop(i).uopIdx === fbk.bits.uopidx
    when (vecCommit(i)) {
      vecMbCommit(i) := true.B
    }
  }

  // misprediction recovery / exception redirect
  // invalidate sq term using robIdx
  val needCancel = Wire(Vec(StoreQueueSize, Bool()))
  for (i <- 0 until StoreQueueSize) {
    needCancel(i) := uop(i).robIdx.needFlush(io.brqRedirect) && allocated(i) && !committed(i)
    when (needCancel(i)) {
      allocated(i) := false.B
    }
  }

 /**
* update pointers
**/
  val lastEnqCancel = PopCount(RegNext(VecInit(canEnqueue.zip(enqCancel).map(x => x._1 && x._2)))) // 1 cycle after redirect
  val lastCycleCancelCount = PopCount(RegNext(needCancel)) // 1 cycle after redirect
  val lastCycleRedirect = RegNext(io.brqRedirect.valid) // 1 cycle after redirect
  val enqNumber = validVStoreFlow.reduce(_ + _)

  val lastlastCycleRedirect=RegNext(lastCycleRedirect)// 2 cycle after redirect
  val redirectCancelCount = RegEnable(lastCycleCancelCount + lastEnqCancel, lastCycleRedirect) // 2 cycle after redirect

  when (lastlastCycleRedirect) {
    // we recover the pointers in 2 cycle after redirect for better timing
    enqPtrExt := VecInit(enqPtrExt.map(_ - redirectCancelCount))
  }.otherwise {
    // lastCycleRedirect.valid or nornal case
    // when lastCycleRedirect.valid, enqNumber === 0.U, enqPtrExt will not change
    enqPtrExt := VecInit(enqPtrExt.map(_ + enqNumber))
  }
  assert(!(lastCycleRedirect && enqNumber =/= 0.U))

  deqPtrExt := deqPtrExtNext
  rdataPtrExt := rdataPtrExtNext

  // val dequeueCount = Mux(io.sbuffer(1).fire, 2.U, Mux(io.sbuffer(0).fire || io.mmioStout.fire, 1.U, 0.U))

  // If redirect at T0, sqCancelCnt is at T2
  io.sqCancelCnt := redirectCancelCount
  val ForceWriteUpper = Wire(UInt(log2Up(StoreQueueSize + 1).W))
  ForceWriteUpper := Constantin.createRecord("ForceWriteUpper_"+p(XSCoreParamsKey).HartId.toString(), initValue = 60.U)
  val ForceWriteLower = Wire(UInt(log2Up(StoreQueueSize + 1).W))
  ForceWriteLower := Constantin.createRecord("ForceWriteLower_"+p(XSCoreParamsKey).HartId.toString(), initValue = 55.U)

  val valid_cnt = PopCount(allocated)
  io.force_write := RegNext(Mux(valid_cnt >= ForceWriteUpper, true.B, valid_cnt >= ForceWriteLower && io.force_write), init = false.B)

  // io.sqempty will be used by sbuffer
  // We delay it for 1 cycle for better timing
  // When sbuffer need to check if it is empty, the pipeline is blocked, which means delay io.sqempty
  // for 1 cycle will also promise that sq is empty in that cycle
  io.sqEmpty := RegNext(
    enqPtrExt(0).value === deqPtrExt(0).value &&
    enqPtrExt(0).flag === deqPtrExt(0).flag
  )
  // perf counter
  QueuePerf(StoreQueueSize, validCount, !allowEnqueue)
  io.sqFull := !allowEnqueue
  XSPerfAccumulate("mmioCycle", uncacheState =/= s_idle) // lq is busy dealing with uncache req
  XSPerfAccumulate("mmioCnt", io.uncache.req.fire)
  XSPerfAccumulate("mmio_wb_success", io.mmioStout.fire || io.vecmmioStout.fire)
  XSPerfAccumulate("mmio_wb_blocked", (io.mmioStout.valid && !io.mmioStout.ready) || (io.vecmmioStout.valid && !io.vecmmioStout.ready))
  XSPerfAccumulate("validEntryCnt", distanceBetween(enqPtrExt(0), deqPtrExt(0)))
  XSPerfAccumulate("cmtEntryCnt", distanceBetween(cmtPtrExt(0), deqPtrExt(0)))
  XSPerfAccumulate("nCmtEntryCnt", distanceBetween(enqPtrExt(0), cmtPtrExt(0)))

  val perfValidCount = distanceBetween(enqPtrExt(0), deqPtrExt(0))
  val perfEvents = Seq(
    ("mmioCycle      ", uncacheState =/= s_idle),
    ("mmioCnt        ", io.uncache.req.fire),
    ("mmio_wb_success", io.mmioStout.fire || io.vecmmioStout.fire),
    ("mmio_wb_blocked", (io.mmioStout.valid && !io.mmioStout.ready) || (io.vecmmioStout.valid && !io.vecmmioStout.ready)),
    ("stq_1_4_valid  ", (perfValidCount < (StoreQueueSize.U/4.U))),
    ("stq_2_4_valid  ", (perfValidCount > (StoreQueueSize.U/4.U)) & (perfValidCount <= (StoreQueueSize.U/2.U))),
    ("stq_3_4_valid  ", (perfValidCount > (StoreQueueSize.U/2.U)) & (perfValidCount <= (StoreQueueSize.U*3.U/4.U))),
    ("stq_4_4_valid  ", (perfValidCount > (StoreQueueSize.U*3.U/4.U))),
  )
  generatePerfEvent()

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
      uop(i).pc,
      debug_vaddr(i),
      debug_paddr(i),
      debug_data(i)
    )
    PrintFlag(allocated(i), "a")
    PrintFlag(allocated(i) && addrvalid(i), "a")
    PrintFlag(allocated(i) && datavalid(i), "d")
    PrintFlag(allocated(i) && committed(i), "c")
    PrintFlag(allocated(i) && pending(i), "p")
    PrintFlag(allocated(i) && mmio(i), "m")
    XSDebug(false, true.B, "\n")
  }

}
