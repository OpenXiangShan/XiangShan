/***************************************************************************************
* Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
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

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utility._
import utils._
import xiangshan._
import xiangshan.ExceptionNO._
import xiangshan.backend._
import xiangshan.backend.rob.{RobLsqIO, RobPtr}
import xiangshan.backend.Bundles.{DynInst, MemExuOutput, UopIdx}
import xiangshan.backend.decode.isa.bitfield.{Riscv32BitInst, XSInstBitFields}
import xiangshan.backend.fu.FuConfig._
import xiangshan.backend.fu.FuType
import xiangshan.mem.Bundles._
import xiangshan.cache._
import xiangshan.cache.{CMOReq, CMOResp, DCacheLineIO, DCacheWordIO, MemoryOpConstants}
import difftest._
import difftest.common.DifftestMem

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
  val sqNeedDeq = Bool()
}

class StoreExceptionBuffer(implicit p: Parameters) extends XSModule with HasCircularQueuePtrHelper {
  // The 1st StorePipelineWidth ports: sta exception generated at s1, except for af
  // The 2nd StorePipelineWidth ports: sta af generated at s2
  // The following VecStorePipelineWidth ports: vector st exception
  // The last port: non-data error generated in SoC
  val enqPortNum = StorePipelineWidth * 2 + VecStorePipelineWidth + 1

  val io = IO(new Bundle() {
    val redirect = Flipped(ValidIO(new Redirect))
    val storeAddrIn = Vec(enqPortNum, Flipped(ValidIO(new LsPipelineBundle())))
    val exceptionAddr = new ExceptionAddrIO
  })

  val req_valid = RegInit(false.B)
  val req = Reg(new LsPipelineBundle())

  // enqueue
  // S1:
  val s1_req = VecInit(io.storeAddrIn.map(_.bits))
  val s1_valid = VecInit(io.storeAddrIn.map(x =>
      x.valid && !x.bits.uop.robIdx.needFlush(io.redirect) && ExceptionNO.selectByFu(x.bits.uop.exceptionVec, StaCfg).asUInt.orR
  ))

  // S2: delay 1 cycle
  val s2_req = (0 until enqPortNum).map(i =>
    RegEnable(s1_req(i), s1_valid(i)))
  val s2_valid = (0 until enqPortNum).map(i =>
    RegNext(s1_valid(i)) && !s2_req(i).uop.robIdx.needFlush(io.redirect)
  )

  val s2_enqueue = Wire(Vec(enqPortNum, Bool()))
  for (w <- 0 until enqPortNum) {
    s2_enqueue(w) := s2_valid(w)
  }

  when (req_valid && req.uop.robIdx.needFlush(io.redirect)) {
    req_valid := s2_enqueue.asUInt.orR
  }.elsewhen (s2_enqueue.asUInt.orR) {
    req_valid := true.B
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
          (bits(0).uop.robIdx === bits(1).uop.robIdx && bits(0).uop.uopIdx > bits(1).uop.uopIdx), res(1), res(0)),
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
    req := Mux(
      reqSel._1(0) && (isAfter(req.uop.robIdx, reqSel._2(0).uop.robIdx) || (isNotBefore(req.uop.robIdx, reqSel._2(0).uop.robIdx) && req.uop.uopIdx > reqSel._2(0).uop.uopIdx)),
      reqSel._2(0),
      req)
  } .elsewhen (s2_enqueue.asUInt.orR) {
    req := reqSel._2(0)
  }

  io.exceptionAddr.vaddr     := req.fullva
  io.exceptionAddr.vaNeedExt := req.vaNeedExt
  io.exceptionAddr.isHyper   := req.isHyper
  io.exceptionAddr.gpaddr    := req.gpaddr
  io.exceptionAddr.vstart    := req.uop.vpu.vstart
  io.exceptionAddr.vl        := req.uop.vpu.vl
  io.exceptionAddr.isForVSnonLeafPTE := req.isForVSnonLeafPTE

}

class GenerateInfoFromSBuffer extends Bundle{
  val diffStoreEventCount = UInt(64.W)
}

// Store Queue
class StoreQueue(implicit p: Parameters) extends XSModule
  with HasDCacheParameters
  with HasCircularQueuePtrHelper
  with HasPerfEvents
  with HasVLSUParameters {
  val io = IO(new Bundle() {
    val hartId = Input(UInt(hartIdLen.W))
    val enq = new SqEnqIO
    val brqRedirect = Flipped(ValidIO(new Redirect))
    val vecFeedback = Vec(VecLoadPipelineWidth, Flipped(ValidIO(new FeedbackToLsqIO)))
    val storeAddrIn = Vec(StorePipelineWidth, Flipped(Valid(new LsPipelineBundle))) // store addr, data is not included
    val storeAddrInRe = Vec(StorePipelineWidth, Input(new LsPipelineBundle())) // store more mmio and exception
    val storeDataIn = Vec(StorePipelineWidth, Flipped(Valid(new MemExuOutput(isVector = true)))) // store data, send to sq from rs
    val storeMaskIn = Vec(StorePipelineWidth, Flipped(Valid(new StoreMaskBundle))) // store mask, send to sq from rs
    val sbuffer = Vec(EnsbufferWidth, Decoupled(new DCacheWordReqWithVaddrAndPfFlag)) // write committed store to sbuffer
    val uncacheOutstanding = Input(Bool())
    val cmoOpReq  = DecoupledIO(new CMOReq)
    val cmoOpResp = Flipped(DecoupledIO(new CMOResp))
    val cboZeroStout = DecoupledIO(new MemExuOutput)
    val mmioStout = DecoupledIO(new MemExuOutput) // writeback uncached store
    val vecmmioStout = DecoupledIO(new MemExuOutput(isVector = true))
    val forward = Vec(LoadPipelineWidth, Flipped(new PipeLoadForwardQueryIO))
    // TODO: scommit is only for scalar store
    val rob = Flipped(new RobLsqIO)
    val uncache = new UncacheWordIO
    // val refill = Flipped(Valid(new DCacheLineReq ))
    val exceptionAddr = new ExceptionAddrIO
    val flushSbuffer = new SbufferFlushBundle
    val sqEmpty = Output(Bool())
    val stAddrReadySqPtr = Output(new SqPtr)
    val stAddrReadyVec = Output(Vec(StoreQueueSize, Bool()))
    val stDataReadySqPtr = Output(new SqPtr)
    val stDataReadyVec = Output(Vec(StoreQueueSize, Bool()))
    val stIssuePtr = Output(new SqPtr)
    val sqDeqPtr = Output(new SqPtr)
    val sqDeqUopIdx = Output(UopIdx())
    val sqDeqRobIdx = Output(new RobPtr)
    val sqFull = Output(Bool())
    val sqCancelCnt = Output(UInt(log2Up(StoreQueueSize + 1).W))
    val sqDeq = Output(UInt(log2Ceil(EnsbufferWidth + 1).W))
    val force_write = Output(Bool())
    val maControl   = Flipped(new StoreMaBufToSqControlIO)
    val wfi = Flipped(new WfiReqBundle)
    val diffStore = Flipped(new DiffStoreIO)
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
  // vlsu exception!
  for (i <- 0 until VecStorePipelineWidth) {
    exceptionBuffer.io.storeAddrIn(StorePipelineWidth * 2 + i).valid               := io.vecFeedback(i).valid && io.vecFeedback(i).bits.feedback(VecFeedbacks.FLUSH) // have exception
    exceptionBuffer.io.storeAddrIn(StorePipelineWidth * 2 + i).bits                := DontCare
    exceptionBuffer.io.storeAddrIn(StorePipelineWidth * 2 + i).bits.fullva         := io.vecFeedback(i).bits.vaddr
    exceptionBuffer.io.storeAddrIn(StorePipelineWidth * 2 + i).bits.vaNeedExt      := io.vecFeedback(i).bits.vaNeedExt
    exceptionBuffer.io.storeAddrIn(StorePipelineWidth * 2 + i).bits.gpaddr         := io.vecFeedback(i).bits.gpaddr
    exceptionBuffer.io.storeAddrIn(StorePipelineWidth * 2 + i).bits.uop.uopIdx     := io.vecFeedback(i).bits.uopidx
    exceptionBuffer.io.storeAddrIn(StorePipelineWidth * 2 + i).bits.uop.robIdx     := io.vecFeedback(i).bits.robidx
    exceptionBuffer.io.storeAddrIn(StorePipelineWidth * 2 + i).bits.uop.vpu.vstart := io.vecFeedback(i).bits.vstart
    exceptionBuffer.io.storeAddrIn(StorePipelineWidth * 2 + i).bits.uop.vpu.vl     := io.vecFeedback(i).bits.vl
    exceptionBuffer.io.storeAddrIn(StorePipelineWidth * 2 + i).bits.isForVSnonLeafPTE := io.vecFeedback(i).bits.isForVSnonLeafPTE
    exceptionBuffer.io.storeAddrIn(StorePipelineWidth * 2 + i).bits.uop.exceptionVec  := io.vecFeedback(i).bits.exceptionVec
  }


  val debug_paddr = Reg(Vec(StoreQueueSize, UInt((PAddrBits).W)))
  val debug_vaddr = Reg(Vec(StoreQueueSize, UInt((VAddrBits).W)))
  val debug_data = Reg(Vec(StoreQueueSize, UInt((XLEN).W)))
  val debug_vec_unaligned_start = Reg(Vec(StoreQueueSize, UInt((log2Up(XLEN)).W))) // only use for unit-stride difftest
  val debug_vec_unaligned_offset = Reg(Vec(StoreQueueSize, UInt((log2Up(XLEN)).W))) // only use for unit-stride difftest

  // state & misc
  val allocated = RegInit(VecInit(List.fill(StoreQueueSize)(false.B))) // sq entry has been allocated
  val completed = RegInit(VecInit(List.fill(StoreQueueSize)(false.B)))
  val addrvalid = RegInit(VecInit(List.fill(StoreQueueSize)(false.B)))
  val datavalid = RegInit(VecInit(List.fill(StoreQueueSize)(false.B)))
  val allvalid  = VecInit((0 until StoreQueueSize).map(i => addrvalid(i) && datavalid(i)))
  val committed = RegInit(VecInit(List.fill(StoreQueueSize)(false.B))) // inst has been committed by rob
  val unaligned = RegInit(VecInit(List.fill(StoreQueueSize)(false.B))) // unaligned store
  val cross16Byte = RegInit(VecInit(List.fill(StoreQueueSize)(false.B))) // unaligned cross 16Byte boundary
  val pending = RegInit(VecInit(List.fill(StoreQueueSize)(false.B))) // mmio pending: inst is an mmio inst, it will not be executed until it reachs the end of rob
  val nc = RegInit(VecInit(List.fill(StoreQueueSize)(false.B))) // nc: inst is a nc inst
  val mmio = RegInit(VecInit(List.fill(StoreQueueSize)(false.B))) // mmio: inst is an mmio inst
  val memBackTypeMM = RegInit(VecInit(List.fill(StoreQueueSize)(false.B)))
  val prefetch = RegInit(VecInit(List.fill(StoreQueueSize)(false.B))) // need prefetch when committing this store to sbuffer?
  val isVec = RegInit(VecInit(List.fill(StoreQueueSize)(false.B))) // vector store instruction
  val vecLastFlow = RegInit(VecInit(List.fill(StoreQueueSize)(false.B))) // last uop the last flow of vector store instruction
  val vecMbCommit = RegInit(VecInit(List.fill(StoreQueueSize)(false.B))) // vector store committed from merge buffer to rob
  val hasException = RegInit(VecInit(List.fill(StoreQueueSize)(false.B))) // store has exception, should deq but not write sbuffer
  val waitStoreS2 = RegInit(VecInit(List.fill(StoreQueueSize)(false.B))) // wait for mmio and exception result until store_s2
  // val vec_robCommit = Reg(Vec(StoreQueueSize, Bool())) // vector store committed by rob
  // val vec_secondInv = RegInit(VecInit(List.fill(StoreQueueSize)(false.B))) // Vector unit-stride, second entry is invalid
  val vecExceptionFlag = RegInit(0.U.asTypeOf(Valid(new DynInst)))
  val noPending = RegInit(true.B)

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
  val rdPtr = rdataPtrExt(0).value

  val validCount = distanceBetween(enqPtrExt(0), deqPtrExt(0))
  val allowEnqueue = validCount <= (StoreQueueSize - LSQStEnqWidth).U

  val deqMask = UIntToMask(deqPtr, StoreQueueSize)
  val enqMask = UIntToMask(enqPtr, StoreQueueSize)

  val commitCount = WireInit(0.U(log2Ceil(CommitWidth + 1).W))
  val scommit = GatedRegNext(io.rob.scommit)
  val mmioReq = Wire(chiselTypeOf(io.uncache.req))
  val ncWaitRespPtrReg = RegInit(0.U(uncacheIdxBits.W)) // it's valid only in non-outstanding situation
  val ncReq = Wire(chiselTypeOf(io.uncache.req))
  val ncResp = Wire(chiselTypeOf(io.uncache.resp))
  val ncDoReq = Wire(Bool())
  val ncSlaveAck = Wire(Bool())
  val ncSlaveAckMid = Wire(UInt(uncacheIdxBits.W))
  val ncDoResp = Wire(Bool())
  val ncReadNextTrigger = Mux(io.uncacheOutstanding, ncSlaveAck, ncDoResp)
  val ncDeqTrigger = Mux(io.uncacheOutstanding, ncSlaveAck, ncDoResp)
  val ncPtr = Mux(io.uncacheOutstanding, ncSlaveAckMid, ncWaitRespPtrReg)

  // store can be committed by ROB
  io.rob.mmio := DontCare
  io.rob.uop := DontCare

  // Read dataModule
  assert(EnsbufferWidth <= 2)
  // rdataPtrExtNext and rdataPtrExtNext+1 entry will be read from dataModule
  val rdataPtrExtNext = Wire(Vec(EnsbufferWidth, new SqPtr))
  val sqReadCnt = WireInit(0.U(log2Ceil(EnsbufferWidth + 1).W))
  val readyReadGoVec = WireInit(VecInit((0 until EnsbufferWidth).map(i =>
    if(i == 0) {
      dataBuffer.io.enq(i).fire && dataBuffer.io.enq(i).bits.sqNeedDeq ||
      allocated(rdataPtrExt(i).value) && completed(rdataPtrExt(i).value) && nc(rdataPtrExt(i).value) ||
      io.mmioStout.fire || io.vecmmioStout.fire
    } else {
      dataBuffer.io.enq(i).fire && dataBuffer.io.enq(i).bits.sqNeedDeq ||
      allocated(rdataPtrExt(i).value) && completed(rdataPtrExt(i).value) && nc(rdataPtrExt(i).value)
    }
  )))
  for (i <- 0 until EnsbufferWidth) {
    when(readyReadGoVec.take(i + 1).reduce(_ && _)) {
      sqReadCnt := (i + 1).U // increase one by one
    }
  }
  rdataPtrExtNext := rdataPtrExt.map(_ + sqReadCnt)

  // deqPtrExtNext traces which inst is about to leave store queue
  val deqPtrExtNext = Wire(Vec(EnsbufferWidth, new SqPtr))
  val sqDeqCnt = WireInit(0.U(log2Ceil(EnsbufferWidth + 1).W))
  val readyDeqVec = WireInit(VecInit((0 until EnsbufferWidth).map(i =>
    allocated(deqPtrExt(i).value) && completed(deqPtrExt(i).value)
  )))
  for (i <- 0 until EnsbufferWidth) {
    val ptr = deqPtrExt(i).value
    when(readyDeqVec.take(i + 1).reduce(_ && _)) {
      sqDeqCnt := (i + 1).U
      allocated(ptr) := false.B
      completed(ptr) := false.B
    }
  }
  deqPtrExtNext := deqPtrExt.map(_ + sqDeqCnt)
  io.sqDeq := RegNext(sqDeqCnt)

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
    * Dynamic enq based on numLsElem number
    */
  io.enq.canAccept := allowEnqueue
  val canEnqueue = io.enq.req.map(_.valid)
  val enqCancel = io.enq.req.map(_.bits.robIdx.needFlush(io.brqRedirect))
  val vStoreFlow = io.enq.req.map(_.bits.numLsElem.asTypeOf(UInt(elemIdxBits.W)))
  val validVStoreFlow = vStoreFlow.zipWithIndex.map{case (vStoreFlowNumItem, index) => Mux(!RegNext(io.brqRedirect.valid) && canEnqueue(index), vStoreFlowNumItem, 0.U)}
  val validVStoreOffset = vStoreFlow.zip(io.enq.needAlloc).map{case (flow, needAllocItem) => Mux(needAllocItem, flow, 0.U)}
  val validVStoreOffsetRShift = 0.U +: validVStoreOffset.take(vStoreFlow.length - 1)

  val enqLowBound = io.enq.req.map(_.bits.sqIdx)
  val enqUpBound  = io.enq.req.map(x => x.bits.sqIdx + x.bits.numLsElem)
  val enqCrossLoop = enqLowBound.zip(enqUpBound).map{case (low, up) => low.flag =/= up.flag}

  for(i <- 0 until StoreQueueSize) {
    val entryCanEnqSeq = (0 until io.enq.req.length).map { j =>
      val entryHitBound = Mux(
        enqCrossLoop(j),
        enqLowBound(j).value <= i.U || i.U < enqUpBound(j).value,
        enqLowBound(j).value <= i.U && i.U < enqUpBound(j).value
      )
      canEnqueue(j) && !enqCancel(j) && entryHitBound
    }

    val entryCanEnq = entryCanEnqSeq.reduce(_ || _)
    val selectBits = ParallelPriorityMux(entryCanEnqSeq, io.enq.req.map(_.bits))
    val selectUpBound = ParallelPriorityMux(entryCanEnqSeq, enqUpBound)
    when (entryCanEnq) {
      uop(i) := selectBits
      if (i + 1 == StoreQueueSize)
        vecLastFlow(i) := Mux(0.U === selectUpBound.value, selectBits.lastUop, false.B) else
        vecLastFlow(i) := Mux((i + 1).U === selectUpBound.value, selectBits.lastUop, false.B)
      allocated(i) := true.B
      completed(i) := false.B
      datavalid(i) := false.B
      addrvalid(i) := false.B
      unaligned(i) := false.B
      cross16Byte(i) := false.B
      committed(i) := false.B
      pending(i) := false.B
      prefetch(i) := false.B
      nc(i) := false.B
      mmio(i) := false.B
      isVec(i) :=  FuType.isVStore(selectBits.fuType)
      vecMbCommit(i) := false.B
      hasException(i) := false.B
      waitStoreS2(i) := true.B
    }
  }

  for (i <- 0 until io.enq.req.length) {
    val sqIdx = enqPtrExt(0) + validVStoreOffsetRShift.take(i + 1).reduce(_ + _)
    val index = io.enq.req(i).bits.sqIdx
    XSError(canEnqueue(i) && !enqCancel(i) && (!io.enq.canAccept || !io.enq.lqCanAccept), s"must accept $i\n")
    XSError(canEnqueue(i) && !enqCancel(i) && index.value =/= sqIdx.value, s"must be the same entry $i\n")
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

  val stAddrReadyVecReg = Wire(Vec(StoreQueueSize, Bool()))
  (0 until StoreQueueSize).map(i => {
    stAddrReadyVecReg(i) := allocated(i) && (mmio(i) || addrvalid(i) || (isVec(i) && vecMbCommit(i)))
  })
  io.stAddrReadyVec := GatedValidRegNext(stAddrReadyVecReg)

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
  val dataReadyLookup = dataReadyLookupVec.map(ptr =>
    allocated(ptr.value) &&
    (addrvalid(ptr.value) && (mmio(ptr.value) || datavalid(ptr.value)) || vecMbCommit(ptr.value)) &&
    !unaligned(ptr.value) &&
    ptr =/= enqPtrExt(0)
  )
  val nextDataReadyPtr = dataReadyPtrExt + PriorityEncoder(VecInit(dataReadyLookup.map(!_) :+ true.B))
  dataReadyPtrExt := nextDataReadyPtr

  // move unalign ptr
  val deqGroupHasUnalign = deqPtrExt.map { case ptr => unaligned(ptr.value) }.reduce(_|_)
  val dataPtrInDeqGroupRangeVec = VecInit(deqPtrExt.zipWithIndex.map { case (ptr, i) =>
    dataReadyPtrExt === ptr && sqDeqCnt > i.U
  })
  val unalignedCanMove = deqGroupHasUnalign && dataPtrInDeqGroupRangeVec.asUInt.orR
  when (unalignedCanMove) {
    val step = sqDeqCnt - PriorityEncoder(dataPtrInDeqGroupRangeVec)
    dataReadyPtrExt := dataReadyPtrExt + step
  }

  val stDataReadyVecReg = Wire(Vec(StoreQueueSize, Bool()))
  (0 until StoreQueueSize).map(i => {
    stDataReadyVecReg(i) := allocated(i) &&
      (addrvalid(i) && (mmio(i) || datavalid(i)) || (isVec(i) && vecMbCommit(i))) && !unaligned(i)
  })
  io.stDataReadyVec := GatedValidRegNext(stDataReadyVecReg)

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
  io.sqDeqUopIdx := uop(deqPtrExt(0).value).uopIdx
  io.sqDeqRobIdx := uop(deqPtrExt(0).value).robIdx

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
    // will re-enter exceptionbuffer at store_s2
    exceptionBuffer.io.storeAddrIn(StorePipelineWidth + i).valid := false.B
    exceptionBuffer.io.storeAddrIn(StorePipelineWidth + i).bits := 0.U.asTypeOf(new LsPipelineBundle)

    when (io.storeAddrIn(i).fire && io.storeAddrIn(i).bits.updateAddrValid) {
      val addr_valid = !io.storeAddrIn(i).bits.miss
      addrvalid(stWbIndex) := addr_valid //!io.storeAddrIn(i).bits.mmio
      nc(stWbIndex) := io.storeAddrIn(i).bits.nc

    }
    when (io.storeAddrIn(i).fire && !io.storeAddrIn(i).bits.isFrmMisAlignBuf) {
      // pending(stWbIndex) := io.storeAddrIn(i).bits.mmio
      unaligned(stWbIndex) := io.storeAddrIn(i).bits.isMisalign
      cross16Byte(stWbIndex) := io.storeAddrIn(i).bits.isMisalign && !io.storeAddrIn(i).bits.misalignWith16Byte

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
    }
    when (io.storeAddrIn(i).fire) {
      uop(stWbIndex) := io.storeAddrIn(i).bits.uop
      uop(stWbIndex).debugInfo := io.storeAddrIn(i).bits.uop.debugInfo
      uop(stWbIndex).debug_seqNum := io.storeAddrIn(i).bits.uop.debug_seqNum
    }
    XSInfo(io.storeAddrIn(i).fire && !io.storeAddrIn(i).bits.isFrmMisAlignBuf,
      "store addr write to sq idx %d pc 0x%x miss:%d vaddr %x paddr %x mmio %x isvec %x\n",
      io.storeAddrIn(i).bits.uop.sqIdx.value,
      io.storeAddrIn(i).bits.uop.pc,
      io.storeAddrIn(i).bits.miss,
      io.storeAddrIn(i).bits.vaddr,
      io.storeAddrIn(i).bits.paddr,
      io.storeAddrIn(i).bits.mmio,
      io.storeAddrIn(i).bits.isvec
    )

    // re-replinish mmio, for pma/pmp will get mmio one cycle later
    val storeAddrInFireReg = RegNext(io.storeAddrIn(i).fire && !io.storeAddrIn(i).bits.miss) && io.storeAddrInRe(i).updateAddrValid
    //val stWbIndexReg = RegNext(stWbIndex)
    val stWbIndexReg = RegEnable(stWbIndex, io.storeAddrIn(i).fire)
    when (storeAddrInFireReg) {
      pending(stWbIndexReg) := io.storeAddrInRe(i).mmio
      mmio(stWbIndexReg) := io.storeAddrInRe(i).mmio
      memBackTypeMM(stWbIndexReg) := io.storeAddrInRe(i).memBackTypeMM
      hasException(stWbIndexReg) := io.storeAddrInRe(i).hasException
      addrvalid(stWbIndexReg) := addrvalid(stWbIndexReg) || io.storeAddrInRe(i).hasException
      waitStoreS2(stWbIndexReg) := false.B
    }
    // dcache miss info (one cycle later than storeIn)
    // if dcache report a miss in sta pipeline, this store will trigger a prefetch when committing to sbuffer (if EnableAtCommitMissTrigger)
    when (storeAddrInFireReg) {
      prefetch(stWbIndexReg) := io.storeAddrInRe(i).miss
    }
    // enter exceptionbuffer again
    when (storeAddrInFireReg) {
      exceptionBuffer.io.storeAddrIn(StorePipelineWidth + i).valid := io.storeAddrInRe(i).hasException && !io.storeAddrInRe(i).isvec
      exceptionBuffer.io.storeAddrIn(StorePipelineWidth + i).bits := io.storeAddrInRe(i)
      exceptionBuffer.io.storeAddrIn(StorePipelineWidth + i).bits.uop.exceptionVec(storeAccessFault) := io.storeAddrInRe(i).af
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
    val isVec     = FuType.isVStore(io.storeDataIn(i).bits.uop.fuType)
    // sq data write takes 2 cycles:
    // sq data write s0
    when (io.storeDataIn(i).fire) {
      // send data write req to data module
      dataModule.io.data.waddr(i) := stWbIndex
      dataModule.io.data.wdata(i) := Mux(io.storeDataIn(i).bits.uop.fuOpType === LSUOpType.cbo_zero,
        0.U,
        Mux(isVec,
          io.storeDataIn(i).bits.data,
          genVWdata(io.storeDataIn(i).bits.data, io.storeDataIn(i).bits.uop.fuOpType(2,0)))
      )
      dataModule.io.data.wen(i) := true.B

      debug_data(dataModule.io.data.waddr(i)) := dataModule.io.data.wdata(i)
      debug_vec_unaligned_start(dataModule.io.data.waddr(i)) := io.storeDataIn(i).bits.vecDebug.get.start
      debug_vec_unaligned_offset(dataModule.io.data.waddr(i)) := io.storeDataIn(i).bits.vecDebug.get.offset
    }
    XSInfo(io.storeDataIn(i).fire,
      "store data write to sq idx %d pc 0x%x data %x -> %x\n",
      io.storeDataIn(i).bits.uop.sqIdx.value,
      io.storeDataIn(i).bits.uop.pc,
      io.storeDataIn(i).bits.data,
      dataModule.io.data.wdata(i)
    )
    // sq data write s1
    val lastStWbIndex = RegEnable(stWbIndex, io.storeDataIn(i).fire)
    when (
      RegNext(io.storeDataIn(i).fire) && allocated(lastStWbIndex)
      // && !RegNext(io.storeDataIn(i).bits.uop).robIdx.needFlush(io.brqRedirect)
    ) {
      datavalid(lastStWbIndex) := true.B
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

    val lfstEnable = Constantin.createRecord("LFSTEnable", LFSTEnable)
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
      (RegEnable(paddrModule.io.forwardMmask(i).asUInt, io.forward(i).valid) ^ RegEnable(vaddrModule.io.forwardMmask(i).asUInt, io.forward(i).valid)) &
      RegNext(needForward) &
      GatedRegNext(addrRealValidVec.asUInt)
    ) =/= 0.U
    val vaddrMatchFailed = vpmaskNotEqual && RegNext(io.forward(i).valid)
    XSInfo(vaddrMatchFailed,
      "vaddrMatchFailed: pc %x pmask %x vmask %x\n",
      RegEnable(io.forward(i).uop.pc, io.forward(i).valid),
      RegEnable(needForward & paddrModule.io.forwardMmask(i).asUInt, io.forward(i).valid),
      RegEnable(needForward & vaddrModule.io.forwardMmask(i).asUInt, io.forward(i).valid)
    );
    XSPerfAccumulate("vaddr_match_failed", vpmaskNotEqual)
    XSPerfAccumulate("vaddr_match_really_failed", vaddrMatchFailed)

    // Fast forward mask will be generated immediately (load_s1)
    io.forward(i).forwardMaskFast := dataModule.io.forwardMaskFast(i)

    // Forward result will be generated 1 cycle later (load_s2)
    io.forward(i).forwardMask := dataModule.io.forwardMask(i)
    io.forward(i).forwardData := dataModule.io.forwardData(i)

    //TODO If the previous store appears out of alignment, then simply FF, this is a very unreasonable way to do it.
    //TODO But for the time being, this is the way to ensure correctness. Such a suitable opportunity to support unaligned forward.
    // If addr match, data not ready, mark it as dataInvalid
    // load_s1: generate dataInvalid in load_s1 to set fastUop
    val dataInvalidMask1 = ((addrValidVec.asUInt & ~dataValidVec.asUInt & vaddrModule.io.forwardMmask(i).asUInt) | unaligned.asUInt & allocated.asUInt) & forwardMask1.asUInt
    val dataInvalidMask2 = ((addrValidVec.asUInt & ~dataValidVec.asUInt & vaddrModule.io.forwardMmask(i).asUInt) | unaligned.asUInt & allocated.asUInt) & forwardMask2.asUInt
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



    when (RegEnable(io.forward(i).uop.loadWaitStrict, io.forward(i).valid)) {
      io.forward(i).addrInvalidSqIdx := RegEnable((io.forward(i).uop.sqIdx - 1.U), io.forward(i).valid)
    } .elsewhen (addrInvalidFlag) {
      io.forward(i).addrInvalidSqIdx.flag := Mux(!s2_differentFlag || addrInvalidSqIdx >= s2_deqPtrExt.value, s2_deqPtrExt.flag, s2_enqPtrExt.flag)
      io.forward(i).addrInvalidSqIdx.value := addrInvalidSqIdx
    } .otherwise {
      // may be store inst has been written to sbuffer already.
      io.forward(i).addrInvalidSqIdx := RegEnable(io.forward(i).uop.sqIdx, io.forward(i).valid)
    }
    io.forward(i).addrInvalid := Mux(RegEnable(io.forward(i).uop.loadWaitStrict, io.forward(i).valid), RegNext(hasInvalidAddr), addrInvalidFlag)

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
      io.forward(i).dataInvalidSqIdx := RegEnable(io.forward(i).uop.sqIdx, io.forward(i).valid)
    }
  }

  /**
    * Memory mapped IO / other uncached operations / CMO
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
  val mmioState = RegInit(s_idle)
  val uncacheUop = Reg(new DynInst)
  val cboFlushedSb = RegInit(false.B)
  val cmoOpCode = uncacheUop.fuOpType(1, 0)
  val mmioDoReq = io.uncache.req.fire && !io.uncache.req.bits.nc
  val cboMmioPAddr = Reg(UInt(PAddrBits.W))
  switch(mmioState) {
    is(s_idle) {
      when(RegNext(io.rob.pendingst && uop(deqPtr).robIdx === io.rob.pendingPtr && pending(deqPtr) && allocated(deqPtr) && datavalid(deqPtr) && addrvalid(deqPtr) && !hasException(deqPtr))) {
        mmioState := s_req
        uncacheUop := uop(deqPtr)
        uncacheUop.exceptionVec := 0.U.asTypeOf(ExceptionVec())
        uncacheUop.trigger := 0.U.asTypeOf(TriggerAction())
        cboFlushedSb := false.B
        cboMmioPAddr := paddrModule.io.rdata(0)
      }
    }
    is(s_req) {
      when (mmioDoReq) {
        noPending := false.B
        mmioState := s_resp
      }
    }
    is(s_resp) {
      when(io.uncache.resp.fire && !io.uncache.resp.bits.nc) {
        noPending := true.B
        mmioState := s_wb

        when (io.uncache.resp.bits.nderr || io.cmoOpResp.bits.nderr) {
          uncacheUop.exceptionVec(hardwareError) := true.B
        }
      }
    }
    is(s_wb) {
      when (io.mmioStout.fire || io.vecmmioStout.fire) {
        when (uncacheUop.exceptionVec(hardwareError)) {
          mmioState := s_idle
        }.otherwise {
          mmioState := s_wait
        }
      }
    }
    is(s_wait) {
      // A MMIO store can always move cmtPtrExt as it must be ROB head
      when(scommit > 0.U) {
        mmioState := s_idle // ready for next mmio
      }
    }
  }

  mmioReq.valid := mmioState === s_req && !LSUOpType.isCbo(uop(deqPtr).fuOpType) && !io.wfi.wfiReq
  mmioReq.bits := DontCare
  mmioReq.bits.cmd  := MemoryOpConstants.M_XWR
  mmioReq.bits.addr := paddrModule.io.rdata(0) // data(deqPtr) -> rdata(0)
  mmioReq.bits.vaddr:= vaddrModule.io.rdata(0)
  mmioReq.bits.data := shiftDataToLow(paddrModule.io.rdata(0), dataModule.io.rdata(0).data)
  mmioReq.bits.mask := shiftMaskToLow(paddrModule.io.rdata(0), dataModule.io.rdata(0).mask)
  mmioReq.bits.robIdx := uop(GatedRegNext(rdataPtrExtNext(0)).value).robIdx
  mmioReq.bits.memBackTypeMM := memBackTypeMM(GatedRegNext(rdataPtrExtNext(0)).value)
  mmioReq.bits.nc := false.B
  mmioReq.bits.id := rdataPtrExt(0).value

  /**
    * NC Store
    * (1) req: when it has been commited, it can be sent to lower level.
    * (2) resp: because SQ data forward is required, it can only be deq when ncResp is received
    *
    * NOTE: nc_req_ack is used to make sure that the request is written by the ubuffer and
    * the ubuffer can forward the required data
    */
  // TODO: CAN NOT deal with vector nc now!
  val nc_idle :: nc_req :: nc_req_ack :: nc_resp :: Nil = Enum(4)
  val ncState = RegInit(nc_idle)
  val rptr0 = rdataPtrExt(0).value
  switch(ncState){
    is(nc_idle) {
      when(
        nc(rptr0) && allocated(rptr0) && !completed(rptr0) && committed(rptr0) &&
        allvalid(rptr0) && !isVec(rptr0) && !hasException(rptr0) && !mmio(rptr0)
      ) {
        ncState := nc_req
        ncWaitRespPtrReg := rptr0
      }
    }
    is(nc_req) {
      when(ncDoReq) {
        ncState := nc_req_ack
      }
    }
    is(nc_req_ack) {
      when(ncSlaveAck) {
        when(io.uncacheOutstanding) {
          ncState := nc_idle
        }.otherwise{
          ncState := nc_resp
        }
      }
    }
    is(nc_resp) {
      when(ncResp.fire) {
        ncState := nc_idle
      }
    }
  }

  ncDoReq := io.uncache.req.fire && io.uncache.req.bits.nc
  ncDoResp := ncResp.fire
  ncSlaveAck := io.uncache.idResp.valid && io.uncache.idResp.bits.nc
  ncSlaveAckMid := io.uncache.idResp.bits.mid

  ncReq.valid := ncState === nc_req && !io.wfi.wfiReq
  ncReq.bits := DontCare
  ncReq.bits.cmd  := MemoryOpConstants.M_XWR
  ncReq.bits.addr := paddrModule.io.rdata(0)
  ncReq.bits.vaddr:= vaddrModule.io.rdata(0)
  ncReq.bits.data := shiftDataToLow(paddrModule.io.rdata(0), dataModule.io.rdata(0).data)
  ncReq.bits.mask := shiftMaskToLow(paddrModule.io.rdata(0), dataModule.io.rdata(0).mask)
  ncReq.bits.robIdx := uop(GatedRegNext(rdataPtrExtNext(0)).value).robIdx
  ncReq.bits.memBackTypeMM := memBackTypeMM(GatedRegNext(rdataPtrExtNext(0)).value)
  ncReq.bits.nc := true.B
  ncReq.bits.id := rptr0

  ncResp.ready := io.uncache.resp.ready
  ncResp.valid := io.uncache.resp.fire && io.uncache.resp.bits.nc
  ncResp.bits <> io.uncache.resp.bits
  when (ncDeqTrigger) {
    completed(ncPtr) := true.B
  }
  XSDebug(ncDeqTrigger,"nc fire: ptr %d\n", ncPtr)

  mmioReq.ready := io.uncache.req.ready
  ncReq.ready := io.uncache.req.ready && !mmioReq.valid
  io.uncache.req.valid := mmioReq.valid || ncReq.valid
  io.uncache.req.bits := Mux(mmioReq.valid, mmioReq.bits, ncReq.bits)

  // CBO op type check can be delayed for 1 cycle,
  // as uncache op will not start in s_idle
  val cboMmioAddr = get_block_addr(cboMmioPAddr)
  val deqCanDoCbo = GatedRegNext(LSUOpType.isCbo(uop(deqPtr).fuOpType) && allocated(deqPtr) && addrvalid(deqPtr) && !hasException(deqPtr))

  val isCboZeroToSbVec = (0 until EnsbufferWidth).map{ i =>
    io.sbuffer(i).fire && io.sbuffer(i).bits.vecValid && io.sbuffer(i).bits.wline && allocated(dataBuffer.io.deq(i).bits.sqPtr.value)
  }
  val cboZeroToSb        = isCboZeroToSbVec.reduce(_ || _)
  val cboZeroFlushSb     = GatedRegNext(cboZeroToSb)

  val cboZeroUop         = RegEnable(PriorityMux(isCboZeroToSbVec, dataBuffer.io.deq.map(x=>uop(x.bits.sqPtr.value))), cboZeroToSb)
  val cboZeroSqIdx       = RegEnable(PriorityMux(isCboZeroToSbVec, dataBuffer.io.deq.map(_.bits.sqPtr)), cboZeroToSb)
  val cboZeroValid       = RegInit(false.B)
  val cboZeroWaitFlushSb = RegInit(false.B)

  assert(!(PopCount(isCboZeroToSbVec) > 1.U), "Multiple cbo zero instructions cannot be executed at the same time")

  when (cboZeroToSb) {
    cboZeroValid       := true.B
    cboZeroWaitFlushSb := true.B
  }

  when (deqCanDoCbo) {
    // disable uncache channel
    io.uncache.req.valid := false.B

    when (io.cmoOpReq.fire) {
      noPending := false.B
      mmioState := s_resp
    }

    when (mmioState === s_resp) {
      when (io.cmoOpResp.fire) {
        noPending := true.B
        mmioState := s_wb
      }
    }
  }

  io.cmoOpReq.valid := deqCanDoCbo && cboFlushedSb && (mmioState === s_req) && !io.wfi.wfiReq
  io.cmoOpReq.bits.opcode  := cmoOpCode
  io.cmoOpReq.bits.address := cboMmioAddr

  io.cmoOpResp.ready := deqCanDoCbo && (mmioState === s_resp)

  io.wfi.wfiSafe := GatedValidRegNext(noPending && io.wfi.wfiReq)

  io.flushSbuffer.valid := deqCanDoCbo && !cboFlushedSb && (mmioState === s_req) && !io.flushSbuffer.empty || cboZeroFlushSb

  when(deqCanDoCbo && !cboFlushedSb && (mmioState === s_req) && io.flushSbuffer.empty) {
    cboFlushedSb := true.B
  }

  when(mmioDoReq){
    // mmio store should not be committed until uncache req is sent
    pending(deqPtr) := false.B
  }
  XSDebug(
    mmioDoReq,
    p"uncache req: pc ${Hexadecimal(uop(deqPtr).pc)} " +
    p"addr ${Hexadecimal(io.uncache.req.bits.addr)} " +
    p"data ${Hexadecimal(io.uncache.req.bits.data)} " +
    p"op ${Hexadecimal(io.uncache.req.bits.cmd)} " +
    p"mask ${Hexadecimal(io.uncache.req.bits.mask)}\n"
  )

  // (3) response from uncache channel: mark as datavalid
  io.uncache.resp.ready := true.B

  // (4) scalar store: writeback to ROB (and other units): mark as writebacked
  io.mmioStout.valid := mmioState === s_wb && !isVec(deqPtr)
  io.mmioStout.bits.uop := uncacheUop
  io.mmioStout.bits.uop.exceptionVec := ExceptionNO.selectByFu(uncacheUop.exceptionVec, StaCfg)
  io.mmioStout.bits.uop.sqIdx := deqPtrExt(0)
  io.mmioStout.bits.uop.flushPipe := deqCanDoCbo // flush Pipeline to keep order in CMO
  io.mmioStout.bits.data := shiftDataToLow(paddrModule.io.rdata(0), dataModule.io.rdata(0).data) // dataModule.io.rdata.read(deqPtr)
  io.mmioStout.bits.isFromLoadUnit := DontCare
  io.mmioStout.bits.debug.isMMIO := true.B
  io.mmioStout.bits.debug.isNCIO := false.B
  io.mmioStout.bits.debug.paddr := DontCare
  io.mmioStout.bits.debug.isPerfCnt := false.B
  io.mmioStout.bits.debug.vaddr := DontCare
  // Remove MMIO inst from store queue after MMIO request is being sent
  // That inst will be traced by uncache state machine
  when (io.mmioStout.fire) {
    completed(deqPtr) := true.B
  }

  // cbo Zero writeback to ROB
  io.cboZeroStout.valid                := cboZeroValid && !cboZeroWaitFlushSb
  io.cboZeroStout.bits.uop             := cboZeroUop
  io.cboZeroStout.bits.uop.sqIdx       := cboZeroSqIdx
  io.cboZeroStout.bits.data            := DontCare
  io.cboZeroStout.bits.isFromLoadUnit  := DontCare
  io.cboZeroStout.bits.debug.isMMIO    := false.B
  io.cboZeroStout.bits.debug.isNCIO      := false.B
  io.cboZeroStout.bits.debug.paddr     := DontCare
  io.cboZeroStout.bits.debug.isPerfCnt := false.B
  io.cboZeroStout.bits.debug.vaddr     := DontCare

  when (cboZeroWaitFlushSb && io.flushSbuffer.empty) {
    cboZeroWaitFlushSb    := false.B
  }
  when (io.cboZeroStout.fire) {
    completed(cboZeroSqIdx.value) := true.B
    cboZeroValid := false.B
  }

  exceptionBuffer.io.storeAddrIn.last.valid := io.mmioStout.fire
  exceptionBuffer.io.storeAddrIn.last.bits := DontCare
  exceptionBuffer.io.storeAddrIn.last.bits.fullva := vaddrModule.io.rdata.head
  exceptionBuffer.io.storeAddrIn.last.bits.vaNeedExt := true.B
  exceptionBuffer.io.storeAddrIn.last.bits.uop := uncacheUop

  // (4) or vector store:
  // TODO: implement it!
  io.vecmmioStout := DontCare
  io.vecmmioStout.valid := false.B //mmioState === s_wb && isVec(deqPtr)
  io.vecmmioStout.bits.uop := uop(deqPtr)
  io.vecmmioStout.bits.uop.sqIdx := deqPtrExt(0)
  io.vecmmioStout.bits.data := shiftDataToLow(paddrModule.io.rdata(0), dataModule.io.rdata(0).data) // dataModule.io.rdata.read(deqPtr)
  io.vecmmioStout.bits.debug.isMMIO := true.B
  io.vecmmioStout.bits.debug.isNCIO   := false.B
  io.vecmmioStout.bits.debug.paddr := DontCare
  io.vecmmioStout.bits.debug.isPerfCnt := false.B
  io.vecmmioStout.bits.debug.vaddr := DontCare
  // Remove MMIO inst from store queue after MMIO request is being sent
  // That inst will be traced by uncache state machine
  when (io.vecmmioStout.fire) {
    completed(deqPtr) := true.B
  }

  /**
    * ROB commits store instructions (mark them as committed)
    *
    * (1) When store commits, mark it as committed.
    * (2) They will not be cancelled and can be sent to lower level.
    */
  XSError(mmioState =/= s_idle && mmioState =/= s_wait && commitCount > 0.U,
   "should not commit instruction when MMIO has not been finished\n")

  val commitVec = WireInit(VecInit(Seq.fill(CommitWidth)(false.B)))
  val needCancel = Wire(Vec(StoreQueueSize, Bool())) // Will be assigned later

  if (backendParams.debugEn){ dontTouch(commitVec) }

  // TODO: Deal with vector store mmio
  for (i <- 0 until CommitWidth) {
    // don't mark misalign store as committed
    val ptr = cmtPtrExt(i).value
    val isCommit = WireInit(false.B)
    when (
      allocated(ptr) &&
      isNotAfter(uop(ptr).robIdx, GatedRegNext(io.rob.pendingPtr)) &&
      !needCancel(ptr) &&
      (!waitStoreS2(ptr) || isVec(ptr))) {
      if (i == 0){
        // TODO: fixme for vector mmio
        when ((mmioState === s_idle) || (mmioState === s_wait && scommit > 0.U)){
          when ((isVec(ptr) && vecMbCommit(ptr)) || !isVec(ptr)) {
            isCommit := true.B
            committed(ptr) := true.B
            commitVec(0) := true.B
          }
        }
      } else {
        when ((isVec(ptr) && vecMbCommit(ptr)) || !isVec(ptr)) {
          isCommit := commitVec(i - 1) || committed(ptr)
          committed(ptr) := commitVec(i - 1) || committed(ptr)
          commitVec(i) := commitVec(i - 1)
        }
      }
    }
    when(isCommit && nc(ptr) && hasException(ptr)) {
      completed(ptr) := true.B
    }
  }

  commitCount := PopCount(commitVec)
  cmtPtrExt := cmtPtrExt.map(_ + commitCount)

  /**
   * committed stores will not be cancelled and can be sent to lower level.
   *
   * 1. Store NC: Read data to uncache
   *    implement as above
   *
   * 2. Store Cache: Read data from data module
   *    remove retired insts from sq, add retired store to sbuffer.
   *    as store queue grows larger and larger, time needed to read data from data
   *    module keeps growing higher. Now we give data read a whole cycle.
   */

  //TODO An unaligned command can only be sent out if the databuffer can enter more than two.
  //TODO For now, hardcode the number of ENQs for the databuffer.
  val canDeqMisaligned = dataBuffer.io.enq(0).ready && dataBuffer.io.enq(1).ready
  val firstWithMisalign = unaligned(rdataPtrExt(0).value)
  val firstWithCross16Byte = cross16Byte(rdataPtrExt(0).value)

  val isCross4KPage = io.maControl.toStoreQueue.crossPageWithHit
  val isCross4KPageCanDeq = io.maControl.toStoreQueue.crossPageCanDeq
  // When encountering a cross page store, a request needs to be sent to storeMisalignBuffer for the high page table's paddr.
  io.maControl.toStoreMisalignBuffer.sqPtr := rdataPtrExt(0)
  io.maControl.toStoreMisalignBuffer.doDeq := isCross4KPage && isCross4KPageCanDeq && dataBuffer.io.enq(0).fire
  io.maControl.toStoreMisalignBuffer.uop := uop(rdataPtrExt(0).value)
  for (i <- 0 until EnsbufferWidth) {
    val ptr = rdataPtrExt(i).value
    val mmioStall = if(i == 0) mmio(rdataPtrExt(0).value) else (mmio(rdataPtrExt(i).value) || mmio(rdataPtrExt(i-1).value))
    val ncStall = if(i == 0) nc(rdataPtrExt(0).value) else (nc(rdataPtrExt(i).value) || nc(rdataPtrExt(i-1).value))
    val exceptionValid = if(i == 0) hasException(rdataPtrExt(0).value) else {
      hasException(rdataPtrExt(i).value) || (hasException(rdataPtrExt(i-1).value) && uop(rdataPtrExt(i).value).robIdx === uop(rdataPtrExt(i-1).value).robIdx)
    }
    val vecNotAllMask = dataModule.io.rdata(i).mask.orR
    // Vector instructions that prevent triggered exceptions from being written to the 'databuffer'.
    val vecHasExceptionFlagValid = vecExceptionFlag.valid && isVec(ptr) && vecExceptionFlag.bits.robIdx === uop(ptr).robIdx

    val misalignToDataBufferValid = allocated(rdataPtrExt(0).value) && committed(rdataPtrExt(0).value) &&
                                    (!isVec(rdataPtrExt(0).value) && allvalid(rdataPtrExt(0).value) || vecMbCommit(rdataPtrExt(0).value)) &&
                                    canDeqMisaligned && (!isCross4KPage || isCross4KPageCanDeq || hasException(rdataPtrExt(0).value))
    // Only the first interface can write unaligned directives.
    // Simplified design, even if the two ports have exceptions, but still only one unaligned dequeue.
    val assert_flag = WireInit(false.B)
    when(firstWithMisalign && firstWithCross16Byte) {
      dataBuffer.io.enq(i).valid := misalignToDataBufferValid
      assert_flag := dataBuffer.io.enq(1).valid
    }.otherwise {
      dataBuffer.io.enq(i).valid := (
        allocated(ptr) && committed(ptr)
          && ((!isVec(ptr) && (allvalid(ptr) || hasException(ptr))) || vecMbCommit(ptr))
          && !mmioStall && !ncStall
          && (!unaligned(ptr) || !cross16Byte(ptr) && (allvalid(ptr) || hasException(ptr)))
        )
    }

    val misalignAddrLow = vaddrModule.io.rdata(0)(2, 0)
    val cross16ByteAddrLow4bit = vaddrModule.io.rdata(0)(3, 0)
    val addrLow4bit = vaddrModule.io.rdata(i)(3, 0)

    // For unaligned, we need to generate a base-aligned mask in storeunit and then do a shift split in StoreQueue.
    val Cross16ByteMask = Wire(UInt(32.W))
    val Cross16ByteData = Wire(UInt(256.W))
    Cross16ByteMask := dataModule.io.rdata(0).mask << cross16ByteAddrLow4bit
    Cross16ByteData := dataModule.io.rdata(0).data << (cross16ByteAddrLow4bit << 3)

    val paddrLow  = Cat(paddrModule.io.rdata(0)(paddrModule.io.rdata(0).getWidth - 1, 3), 0.U(3.W))
    val paddrHigh = Cat(paddrModule.io.rdata(0)(paddrModule.io.rdata(0).getWidth - 1, 3), 0.U(3.W)) + 8.U

    val vaddrLow  = Cat(vaddrModule.io.rdata(0)(vaddrModule.io.rdata(0).getWidth - 1, 3), 0.U(3.W))
    val vaddrHigh = Cat(vaddrModule.io.rdata(0)(vaddrModule.io.rdata(0).getWidth - 1, 3), 0.U(3.W)) + 8.U

    val maskLow   = Cross16ByteMask(15, 0)
    val maskHigh  = Cross16ByteMask(31, 16)

    val dataLow   = Cross16ByteData(127, 0)
    val dataHigh  = Cross16ByteData(255, 128)

    val toSbufferVecValid = (!isVec(ptr) || (vecMbCommit(ptr) && allvalid(ptr) && vecNotAllMask)) && !exceptionValid && !vecHasExceptionFlagValid
    when(canDeqMisaligned && firstWithMisalign && firstWithCross16Byte) {
      when(isCross4KPage && isCross4KPageCanDeq) {
        if (i == 0) {
          dataBuffer.io.enq(i).bits.addr      := paddrLow
          dataBuffer.io.enq(i).bits.vaddr     := vaddrLow
          dataBuffer.io.enq(i).bits.data      := dataLow
          dataBuffer.io.enq(i).bits.mask      := maskLow
          dataBuffer.io.enq(i).bits.wline     := false.B
          dataBuffer.io.enq(i).bits.sqPtr     := rdataPtrExt(0)
          dataBuffer.io.enq(i).bits.prefetch  := false.B
          dataBuffer.io.enq(i).bits.sqNeedDeq := true.B
          dataBuffer.io.enq(i).bits.vecValid  := toSbufferVecValid
        }
        else {
          dataBuffer.io.enq(i).bits.addr      := io.maControl.toStoreQueue.paddr
          dataBuffer.io.enq(i).bits.vaddr     := vaddrHigh
          dataBuffer.io.enq(i).bits.data      := dataHigh
          dataBuffer.io.enq(i).bits.mask      := maskHigh
          dataBuffer.io.enq(i).bits.wline     := false.B
          dataBuffer.io.enq(i).bits.sqPtr     := rdataPtrExt(0)
          dataBuffer.io.enq(i).bits.prefetch  := false.B
          dataBuffer.io.enq(i).bits.sqNeedDeq := false.B
          dataBuffer.io.enq(i).bits.vecValid  := dataBuffer.io.enq(0).bits.vecValid
        }
      } .otherwise {
        if (i == 0) {
          dataBuffer.io.enq(i).bits.addr      := paddrLow
          dataBuffer.io.enq(i).bits.vaddr     := vaddrLow
          dataBuffer.io.enq(i).bits.data      := dataLow
          dataBuffer.io.enq(i).bits.mask      := maskLow
          dataBuffer.io.enq(i).bits.wline     := false.B
          dataBuffer.io.enq(i).bits.sqPtr     := rdataPtrExt(0)
          dataBuffer.io.enq(i).bits.prefetch  := false.B
          dataBuffer.io.enq(i).bits.sqNeedDeq  := true.B
          dataBuffer.io.enq(i).bits.vecValid  := toSbufferVecValid
        }
        else {
          dataBuffer.io.enq(i).bits.addr      := paddrHigh
          dataBuffer.io.enq(i).bits.vaddr     := vaddrHigh
          dataBuffer.io.enq(i).bits.data      := dataHigh
          dataBuffer.io.enq(i).bits.mask      := maskHigh
          dataBuffer.io.enq(i).bits.wline     := false.B
          dataBuffer.io.enq(i).bits.sqPtr     := rdataPtrExt(0)
          dataBuffer.io.enq(i).bits.prefetch  := false.B
          dataBuffer.io.enq(i).bits.sqNeedDeq  := false.B
          dataBuffer.io.enq(i).bits.vecValid  := dataBuffer.io.enq(0).bits.vecValid
        }
      }


    }.elsewhen(!cross16Byte(ptr) && unaligned(ptr)) {
      dataBuffer.io.enq(i).bits.addr     := Cat(paddrModule.io.rdata(i)(PAddrBits - 1, 4), 0.U(4.W))
      dataBuffer.io.enq(i).bits.vaddr    := Cat(vaddrModule.io.rdata(i)(VAddrBits - 1, 4), 0.U(4.W))
      dataBuffer.io.enq(i).bits.data     := dataModule.io.rdata(i).data << (addrLow4bit << 3)
      dataBuffer.io.enq(i).bits.mask     := dataModule.io.rdata(i).mask
      dataBuffer.io.enq(i).bits.wline    := paddrModule.io.rlineflag(i)
      dataBuffer.io.enq(i).bits.sqPtr    := rdataPtrExt(i)
      dataBuffer.io.enq(i).bits.prefetch := prefetch(ptr)
      dataBuffer.io.enq(i).bits.sqNeedDeq := true.B
      // when scalar has exception, will also not write into sbuffer
      dataBuffer.io.enq(i).bits.vecValid := toSbufferVecValid
    }.otherwise {
      dataBuffer.io.enq(i).bits.addr     := paddrModule.io.rdata(i)
      dataBuffer.io.enq(i).bits.vaddr    := vaddrModule.io.rdata(i)
      dataBuffer.io.enq(i).bits.data     := dataModule.io.rdata(i).data
      dataBuffer.io.enq(i).bits.mask     := dataModule.io.rdata(i).mask
      dataBuffer.io.enq(i).bits.wline    := paddrModule.io.rlineflag(i)
      dataBuffer.io.enq(i).bits.sqPtr    := rdataPtrExt(i)
      dataBuffer.io.enq(i).bits.prefetch := prefetch(ptr)
      dataBuffer.io.enq(i).bits.sqNeedDeq := true.B
      // when scalar has exception, will also not write into sbuffer
      dataBuffer.io.enq(i).bits.vecValid := toSbufferVecValid

    }

    // Note that store data/addr should both be valid after store's commit
    assert(!dataBuffer.io.enq(i).valid || allvalid(ptr) || hasException(ptr) || (allocated(ptr) && vecMbCommit(ptr)) || assert_flag)
  }

  // Send data stored in sbufferReqBitsReg to sbuffer
  for (i <- 0 until EnsbufferWidth) {
    io.sbuffer(i).valid := dataBuffer.io.deq(i).valid
    dataBuffer.io.deq(i).ready := io.sbuffer(i).ready
    io.sbuffer(i).bits.fromDataBufferEntry(dataBuffer.io.deq(i).bits, MemoryOpConstants.M_XWR)
    // io.sbuffer(i).fire is RegNexted, as sbuffer data write takes 2 cycles.
    // Before data write finish, sbuffer is unable to provide store to load
    // forward data. As an workaround, deqPtrExt and allocated flag update
    // is delayed so that load can get the right data from store queue.
    // ---
    // Only sqNeedDeq can move the ptr.
    // ---
    // however, `completed` is register, when it turn true, the data has already been written to sbuffer
    // Besides, we should not have cbozero completed. (wline is currently only for cbozero)
    val ptr = dataBuffer.io.deq(i).bits.sqPtr.value
    when (io.sbuffer(i).fire && io.sbuffer(i).bits.sqNeedDeq && !io.sbuffer(i).bits.wline) {

      completed(ptr) := true.B
    }
    XSDebug(RegNext(io.sbuffer(i).fire && io.sbuffer(i).bits.sqNeedDeq), "sbuffer "+i+" fire: ptr %d\n", ptr)
  }

  // All vector instruction uop normally dequeue, but the Uop after the exception is raised does not write to the 'sbuffer'.
  // Flags are used to record whether there are any exceptions when the queue is displayed.
  // This is determined each time a write is made to the 'databuffer', prevent subsequent uop of the same instruction from writing to the 'dataBuffer'.
  val vecCommitHasException = (0 until EnsbufferWidth).map{ i =>
    val ptr = rdataPtrExt(i).value
    val mmioStall = if(i == 0) mmio(rdataPtrExt(0).value) else (mmio(rdataPtrExt(i).value) || mmio(rdataPtrExt(i-1).value))
    val ncStall = if(i == 0) nc(rdataPtrExt(0).value) else (nc(rdataPtrExt(i).value) || nc(rdataPtrExt(i-1).value))
    val exceptionVliad      = isVec(ptr) && hasException(ptr) && dataBuffer.io.enq(i).fire && dataBuffer.io.enq(i).bits.sqNeedDeq
    (exceptionVliad, uop(ptr), vecLastFlow(ptr))
  }

  val vecCommitHasExceptionValid      = vecCommitHasException.map(_._1)
  val vecCommitHasExceptionUop        = vecCommitHasException.map(_._2)
  val vecCommitHasExceptionLastFlow   = vecCommitHasException.map(_._3)
  val vecCommitHasExceptionValidOR    = vecCommitHasExceptionValid.reduce(_ || _)
  // Just select the last Uop tah has an exception.
  val vecCommitHasExceptionSelectUop  = ParallelPosteriorityMux(vecCommitHasExceptionValid, vecCommitHasExceptionUop)
  // If the last flow with an exception is the LastFlow of this instruction, the flag is not set.
  // compare robidx to select the last flow
  require(EnsbufferWidth == 2, "The vector store exception handle process only support EnsbufferWidth == 2 yet.")
  val robidxEQ = dataBuffer.io.enq(0).fire && dataBuffer.io.enq(1).fire &&
    uop(rdataPtrExt(0).value).robIdx === uop(rdataPtrExt(1).value).robIdx
  val robidxNE = dataBuffer.io.enq(0).fire && dataBuffer.io.enq(1).fire && (
    uop(rdataPtrExt(0).value).robIdx =/= uop(rdataPtrExt(1).value).robIdx
  )
  val onlyCommit0 = dataBuffer.io.enq(0).fire && !dataBuffer.io.enq(1).fire

  val vecCommitLastFlow =
    // robidx equal => check if 1 is last flow
    robidxEQ && vecCommitHasExceptionLastFlow(1) ||
    // robidx not equal => 0 must be the last flow, just check if 1 is last flow when 1 has exception
    robidxNE && (vecCommitHasExceptionValid(1) && vecCommitHasExceptionLastFlow(1) || !vecCommitHasExceptionValid(1)) ||
    onlyCommit0 && vecCommitHasExceptionLastFlow(0)


  val vecExceptionFlagCancel  = (0 until EnsbufferWidth).map{ i =>
    val ptr = rdataPtrExt(i).value
    val vecLastFlowCommit = vecLastFlow(ptr) && (uop(ptr).robIdx === vecExceptionFlag.bits.robIdx) &&
                            dataBuffer.io.enq(i).fire && dataBuffer.io.enq(i).bits.sqNeedDeq
    vecLastFlowCommit
  }.reduce(_ || _)

  // When a LastFlow with an exception instruction is commited, clear the flag.
  when(!vecExceptionFlag.valid && vecCommitHasExceptionValidOR && !vecCommitLastFlow) {
    vecExceptionFlag.valid  := true.B
    vecExceptionFlag.bits   := vecCommitHasExceptionSelectUop
  }.elsewhen(vecExceptionFlag.valid && vecExceptionFlagCancel) {
    vecExceptionFlag.valid  := false.B
    vecExceptionFlag.bits   := 0.U.asTypeOf(new DynInst)
  }

  // A dumb defensive code. The flag should not be placed for a long period of time.
  // A relatively large timeout period, not have any special meaning.
  // If an assert appears and you confirm that it is not a Bug: Increase the timeout or remove the assert.
  TimeOutAssert(vecExceptionFlag.valid, 3000, "vecExceptionFlag timeout, Plase check for bugs or add timeouts.")

  /* difftest */
  // Initialize when unenabled difftest.
  io.diffStore := DontCare
  // Consistent with the logic above.
  // Only the vector store difftest required signal is separated from the rtl code.
  if (env.EnableDifftest) {
    // commit cbo.inval to difftest
    val cmoInvalEvent = DifftestModule(new DiffCMOInvalEvent)
    cmoInvalEvent.coreid := io.hartId
    cmoInvalEvent.valid := io.mmioStout.fire && deqCanDoCbo && LSUOpType.isCboInval(uop(deqPtr).fuOpType)
    cmoInvalEvent.addr := cboMmioAddr

    // DiffStoreEvent happens when rdataPtr moves.
    // That is, pmsStore enter dataBuffer or ncStore enter Ubuffer
    (0 until EnsbufferWidth).foreach { i =>
      // when i = 0, the sqPtr is rdataPtr(0), which is rdataPtrExt(0), so it applies to NC as well.
      val ptr = dataBuffer.io.enq(i).bits.sqPtr.value
      io.diffStore.diffInfo(i).uop := uop(ptr)
      io.diffStore.diffInfo(i).start := debug_vec_unaligned_start(ptr)
      io.diffStore.diffInfo(i).offset := debug_vec_unaligned_offset(ptr)
      io.diffStore.pmaStore(i).valid := dataBuffer.io.enq(i).fire
      io.diffStore.pmaStore(i).bits.fromDataBufferEntry(dataBuffer.io.enq(i).bits, MemoryOpConstants.M_XWR)
    }
    io.diffStore.ncStore.valid := ncReq.fire && ncReq.bits.memBackTypeMM
    io.diffStore.ncStore.bits := ncReq.bits
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
  io.exceptionAddr.vaddr     := exceptionBuffer.io.exceptionAddr.vaddr
  io.exceptionAddr.vaNeedExt := exceptionBuffer.io.exceptionAddr.vaNeedExt
  io.exceptionAddr.isHyper   := exceptionBuffer.io.exceptionAddr.isHyper
  io.exceptionAddr.gpaddr    := exceptionBuffer.io.exceptionAddr.gpaddr
  io.exceptionAddr.vstart    := exceptionBuffer.io.exceptionAddr.vstart
  io.exceptionAddr.vl        := exceptionBuffer.io.exceptionAddr.vl
  io.exceptionAddr.isForVSnonLeafPTE := exceptionBuffer.io.exceptionAddr.isForVSnonLeafPTE

  // vector commit or replay from
  val vecCommittmp = Wire(Vec(StoreQueueSize, Vec(VecStorePipelineWidth, Bool())))
  val vecCommit = Wire(Vec(StoreQueueSize, Bool()))
  for (i <- 0 until StoreQueueSize) {
    val fbk = io.vecFeedback
    for (j <- 0 until VecStorePipelineWidth) {
      vecCommittmp(i)(j) := fbk(j).valid && (fbk(j).bits.isCommit || fbk(j).bits.isFlush) &&
        uop(i).robIdx === fbk(j).bits.robidx && uop(i).uopIdx === fbk(j).bits.uopidx && allocated(i)
    }
    vecCommit(i) := vecCommittmp(i).reduce(_ || _)

    when (vecCommit(i)) {
      vecMbCommit(i) := true.B
    }
  }

  // For vector, when there is a store across pages with the same uop in storeMisalignBuffer, storequeue needs to mark this item as committed.
  // TODO FIXME Can vecMbCommit be removed?
  when(io.maControl.toStoreQueue.withSameUop && allvalid(rdataPtrExt(0).value)) {
    vecMbCommit(rdataPtrExt(0).value) := true.B
  }

  // misprediction recovery / exception redirect
  // invalidate sq term using robIdx
  for (i <- 0 until StoreQueueSize) {
    needCancel(i) := allocated(i) && !committed(i) && Mux(
        vecExceptionFlag.valid,
        isAfter(uop(i).robIdx, io.brqRedirect.bits.robIdx) && io.brqRedirect.valid,
        uop(i).robIdx.needFlush(io.brqRedirect)
      )
    when (needCancel(i)) {
      allocated(i) := false.B
      completed(i) := false.B
    }
  }

 /**
* update pointers
**/
  val enqCancelValid = canEnqueue.zip(io.enq.req).map{case (v , x) =>
    v && x.bits.robIdx.needFlush(io.brqRedirect)
  }
  val enqCancelNum = enqCancelValid.zip(vStoreFlow).map{case (v, flow) =>
    Mux(v, flow, 0.U)
  }
  val lastEnqCancel = RegEnable(enqCancelNum.reduce(_ + _), io.brqRedirect.valid) // 1 cycle after redirect

  val lastCycleCancelCount = PopCount(RegEnable(needCancel, io.brqRedirect.valid)) // 1 cycle after redirect
  val lastCycleRedirect = RegNext(io.brqRedirect.valid) // 1 cycle after redirect
  val enqNumber = validVStoreFlow.reduce(_ + _)

  val lastlastCycleRedirect=RegNext(lastCycleRedirect)// 2 cycle after redirect
  val redirectCancelCount = RegEnable(lastCycleCancelCount + lastEnqCancel, 0.U, lastCycleRedirect) // 2 cycle after redirect

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
  ForceWriteUpper := Constantin.createRecord(s"ForceWriteUpper_${p(XSCoreParamsKey).HartId}", initValue = StoreQueueForceWriteSbufferUpper)
  val ForceWriteLower = Wire(UInt(log2Up(StoreQueueSize + 1).W))
  ForceWriteLower := Constantin.createRecord(s"ForceWriteLower_${p(XSCoreParamsKey).HartId}", initValue = StoreQueueForceWriteSbufferLower)

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
  val vecValidVec = WireInit(VecInit((0 until StoreQueueSize).map(i => allocated(i) && isVec(i))))
  QueuePerf(StoreQueueSize, PopCount(vecValidVec), !allowEnqueue)
  io.sqFull := !allowEnqueue
  XSPerfAccumulate("mmioCycle", mmioState =/= s_idle) // lq is busy dealing with uncache req
  XSPerfAccumulate("mmioCnt", mmioDoReq)
  XSPerfAccumulate("mmio_wb_success", io.mmioStout.fire || io.vecmmioStout.fire)
  XSPerfAccumulate("mmio_wb_blocked", (io.mmioStout.valid && !io.mmioStout.ready) || (io.vecmmioStout.valid && !io.vecmmioStout.ready))
  XSPerfAccumulate("validEntryCnt", distanceBetween(enqPtrExt(0), deqPtrExt(0)))
  XSPerfAccumulate("cmtEntryCnt", distanceBetween(cmtPtrExt(0), deqPtrExt(0)))
  XSPerfAccumulate("nCmtEntryCnt", distanceBetween(enqPtrExt(0), cmtPtrExt(0)))

  val perfValidCount = distanceBetween(enqPtrExt(0), deqPtrExt(0))
  val perfEvents = Seq(
    ("mmioCycle      ", mmioState =/= s_idle),
    ("mmioCnt        ", mmioDoReq),
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
    XSDebug(false, flag, name) // when(flag)
    XSDebug(false, !flag, " ") // otherwirse
  }

  for (i <- 0 until StoreQueueSize) {
    XSDebug(s"$i: pc %x va %x pa %x data %x ",
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
