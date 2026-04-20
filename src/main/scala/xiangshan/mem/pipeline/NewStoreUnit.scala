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

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utility._
import xiangshan._
import xiangshan.backend.Bundles.{ExuInput, NewExuOutput, StoreUnitToLFST, connectSamePort}
import xiangshan.backend.ctrlblock.DebugLsInfoBundle
import xiangshan.backend.exu.ExeUnitParams
import xiangshan.backend.fu.PMPRespBundle
import xiangshan.backend.fu.NewCSR._
import xiangshan.cache._
import xiangshan.cache.mmu._
import xiangshan.mem.Bundles._
import xiangshan.mem.StoreStage._

class StoreUnitS0(param: ExeUnitParams)(
  implicit p: Parameters,
  override implicit val s: StoreStage.StoreStage = StoreS0()
) extends StoreUnitStage(param) {
  val io = IO(new Bundle {
    val redirect = Flipped(ValidIO(new Redirect))
    /**
      * Request sources
      */
    val unalignTail = Flipped(DecoupledIO(new StoreStageIO))
    val stin = Flipped(Decoupled(new ExuInput(param, hasCopySrc = true)))
    val vecstin = Flipped(Decoupled(new VectorStoreIn))
    val prefetchReq = Flipped(DecoupledIO(new StorePrefetchReq))
    
    // Tlb request
    val tlbReq = DecoupledIO(new TlbReq)
    val tlbReqKill = Output(Bool())
    
    // DCache request
    val dcacheReq = DecoupledIO(new DCacheWordReq)

    // Store mask
    val toSqMask = Valid(new StoreMaskBundle)
  })

  /**
    * Request sources arbitration, in order of priority:
    * 
    * 0. unalign tail from s1
    * 1. vector elements splited by VSplit
    * 2. store issued from IQ
    * 3. prefetch req
    */
  val unalignTail,
    vectorIssue,
    scalarIssue,
    prefetchReq = Wire(DecoupledIO(new StoreStageIO))
  
  val sources = Seq(
    unalignTail,
    vectorIssue,
    scalarIssue,
    prefetchReq
  )
  val sink = Wire(DecoupledIO(new StoreStageIO))

  // 0. unalign tail from s1
  unalignTail <> io.unalignTail

  // 1. vector elements splited by VSplit
  vectorIssue.valid := io.vecstin.valid
  connectSamePort(vectorIssue.bits, io.vecstin.bits)
  vectorIssue.bits.DontCareUnalign()

  // 2. store issued from IQ
  val stin = io.stin.bits
  val stinUop = stin.toDynInst()
  val stinVAddr = stin.src(0) + SignExt(stin.imm(11,0), VAddrBits)
  val stinFullva = stin.src(0) + SignExt(stin.imm(11,0), XLEN)
  val stinSize = Cat(0.U, LSUOpType.size(stinUop.fuOpType))
  scalarIssue.valid := io.stin.valid
  scalarIssue.bits.entrance := StoreEntrance.scalarIssue.U
  scalarIssue.bits.uop := stinUop
  scalarIssue.bits.vaddr := stinVAddr
  scalarIssue.bits.fullva := stinFullva
  scalarIssue.bits.size := stinSize
  scalarIssue.bits.mask := Mux(
    LSUOpType.isCboAll(stinUop.fuOpType),
    Fill(VLEN/8, 1.U(1.W)),
    genVWmask128(stinVAddr, stinSize)
  )
  scalarIssue.bits.isFirstIssue := stin.isFirstIssue
  scalarIssue.bits.DontCareUnalign()
  scalarIssue.bits.DontCareVectorFields()

  // 3. prefetch req
  prefetchReq.valid := io.prefetchReq.valid
  prefetchReq.bits.entrance := StoreEntrance.prefetch.U
  prefetchReq.bits.vaddr := io.prefetchReq.bits.vaddr
  prefetchReq.bits.fullva := io.prefetchReq.bits.vaddr
  prefetchReq.bits.DontCareUnalign()
  prefetchReq.bits.DontCareVectorFields()
  prefetchReq.bits.uop := 0.U.asTypeOf(new DynInst())
  prefetchReq.bits.size := DontCare // TODO: prefetch req size/uop/mask
  prefetchReq.bits.mask := Fill(VLEN/8, 1.U(1.W))
  prefetchReq.bits.isFirstIssue := true.B

  // sources arbitration
  arbiter(sources, sink, Some("RequestSources"))

  // alias for arbitration result
  val uop = sink.bits.uop
  val kill = uop.robIdx.needFlush(io.redirect)
  val entrance = sink.bits.entrance
  val isUnalignTail = StoreEntrance.isUnalignTail(entrance)
  val isVector = StoreEntrance.isVectorIssue(entrance)
  val isScalar = StoreEntrance.isScalarIssue(entrance)
  val isPrefetch = StoreEntrance.isHWPrefetch(entrance)
  val isCbo = isScalar && LSUOpType.isCboAll(uop.fuOpType)
  val isCboNoZero = isScalar && LSUOpType.isCbo(uop.fuOpType)

  /**
    * Pipeline connect
    * 
    * esp. s0 needs to use the result of sources arbitration as the pipe in
    */
  val pipeIn = sink
  val pipeOut = io_pipeOut.get
  val pipeOutValid = RegInit(false.B)
  val pipeOutBits = RegEnable(pipeIn.bits, pipeIn.fire)
  when (kill) { pipeOutValid := false.B }
  .elsewhen (pipeIn.fire) { pipeOutValid := true.B }
  .elsewhen (pipeOut.fire) { pipeOutValid := false.B }
  pipeIn.ready := !pipeOutValid || pipeOut.ready

  /**
    * Unalign handling
    * 
    * 1. Align
    *   Check if the address is aligned, which is used to detect misalign exception in later stages.
    *   For prefetch req, we set align to true to avoid unnecessary exception check in later stages.
    * 
    * 2. Cross16Byte
    *   For unaligned requests that cross a 16-byte boundary but do not cross a 4K page boundary, 
    *     the StoreQueue is responsible for splitting them into two writes to the store buffer.
    *   Prefetch and unalign tail must be within 16 bytes.
    * 
    * 3. Cross4KPage
    *   Check whether this address crosses an 4K page boundary, which is used to inject 
    *     an unalign tail in the next stage.
    * 
    * Some terminology explanations:
    * - **align** indicates whether the addr is aligned with the operation size. `!align` does not necessary mean
    *   splitting is required, but is only used for determining exception in subsequent stages.
    * - **unalign** indicates that under the condition of align, the operation range exceeds aligned 16B bank boundary,
    *   requiring splitting into 2 operations on DCache.
    * - **misalign** is used specifically to denote misalign exception.
    */
  val needAlignCheckSources = Seq(vectorIssue, scalarIssue)
  val needAlignCheckValids = needAlignCheckSources.map(_.valid)
  val needAlignCheck = needAlignCheckValids.orR
  val alignCheckResults = needAlignCheckSources.map(s => alignCheck(s.bits.vaddr, s.bits.size, s.valid)).unzip3
  val align = ParallelPriorityMux(needAlignCheckValids, alignCheckResults._1)
  val cross16Byte = ParallelPriorityMux(needAlignCheckValids, alignCheckResults._2)
  val cross4KPage = ParallelPriorityMux(needAlignCheckValids, alignCheckResults._3)
  
  sink.bits.align := Mux(needAlignCheck, align, Mux(isPrefetch, true.B, false.B))
  sink.bits.unalignHead := Mux(needAlignCheck, cross4KPage, false.B)
  sink.bits.cross16Byte := Mux(needAlignCheck, cross16Byte, false.B)
  
  def alignCheck(vaddr: UInt, size: UInt, valid: Bool): (Bool, Bool, Bool) = {
    require(size.getWidth == MemorySize.Size.width)
    // 1.1 Align check
    val align = LookupTree(size, List(
      MemorySize.B.U -> true.B,
      MemorySize.H.U -> (vaddr.take(1) === 0.U),
      MemorySize.W.U -> (vaddr.take(2) === 0.U),
      MemorySize.D.U -> (vaddr.take(3) === 0.U),
      MemorySize.Q.U -> (vaddr.take(4) === 0.U)
    ))
    // vector store sends 128-bit requests, its address must be 128-aligned
    assert(!(size === MemorySize.Q.U && !align && valid))
    // 1.2 cross16Bytes check
    // 1.3 cross4KPage check
    val lowAddr = vaddr(12, 0)
    val upAddr = LookupTree(size, List(
      MemorySize.B.U -> 0.U,
      MemorySize.H.U -> 1.U,
      MemorySize.W.U -> 3.U,
      MemorySize.D.U -> 7.U,
      MemorySize.Q.U -> 15.U
    )) + lowAddr
    val cross16Byte = upAddr(4) =/= lowAddr(4)
    val cross4KPage = upAddr(12) =/= lowAddr(12)
    (align, cross16Byte, cross4KPage)
  }

  /**
    * IO assignment
    */
  pipeOut.get.valid := pipeOutValid
  pipeOut.get.bits := pipeOutBits

  io.tlbReq.valid := sink.valid
  io.tlbReq.bits.vaddr := sink.bits.vaddr
  io.tlbReq.bits.fullva := sink.bits.fullva
  io.tlbReq.bits.checkfullva := isVector || isScalar
  io.tlbReq.bits.cmd := Mux(isCboNoZero, TlbCmd.read, TlbCmd.write)
  io.tlbReq.bits.hyperinst := LSUOpType.isHlv(uop.fuOpType)
  io.tlbReq.bits.hlvx := LSUOpType.isHlvx(uop.fuOpType)
  io.tlbReq.bits.isPrefetch := isPrefetch
  io.tlbReq.bits.size := sink.bits.size
  io.tlbReq.bits.kill := false.B
  io.tlbReq.bits.memidx.is_ld := false
  io.tlbReq.bits.memidx.is_st := true.B
  io.tlbReq.bits.memidx.idx := uop.sqIdx.value
  io.tlbReq.bits.no_translate := false.B
  io.tlbReq.bits.pmp_addr := DontCare // TODO: move this outside of TlbReq
  io.tlbReq.bits.debug.pc := uop.pc
  io.tlbReq.bits.debug.robIdx := uop.robIdx
  io.tlbReq.bits.debug.isFirstIssue := sink.bits.isFirstIssue
  io.tlbReqKill := false.B

  io.dcacheReq.valid := pipeIn.fire
  io.dcacheReq.bits.cmd := MemoryOpConstants.M_PFW
  io.dcacheReq.bits.vaddr := sink.bits.vaddr
  io.dcacheReq.bits.instrtype := Mux(isPrefetch, DCACHE_PREFETCH_SOURCE.U, STORE_SOURCE.U)

  io.toSqMask.valid := isScalar || isVector
  io.toSqMask.bits.mask := sink.bits.mask
  io.toSqMask.bits.sqIdx := uop.sqIdx

  /**
    *  Perf counters
    */
  val fire = pipeIn.fire && !kill
  XSPerfAccumulate("s0_valid", pipeIn.valid)
  XSPerfAccumulate("s0_fire", fire)
  XSPerfAccumulate("s0_unalignTail", fire && isUnalignTail)
  XSPerfAccumulate("s0_vector", fire && isVector)
  XSPerfAccumulate("s0_scalar", fire && isScalar)
  XSPerfAccumulate("s0_prefetch", fire && isPrefetch)
  XSPerfAccumulate("s0_isFirstIssue", fire && pipeIn.bits.isFirstIssue)
  XSPerfAccumulate("s0_isCbo", fire && isCbo)
  XSPerfAccumulate("s0_isCboNoZero", fire && isCboNoZero)
  XSPerfAccumulate("s0_unalign", fire && !pipeIn.bits.align)
  XSPerfAccumulate("s0_cross16Byte", fire && pipeIn.bits.cross16Byte)
  XSPerfAccumulate("s0_cross4KPage", fire && pipeIn.bits.unalignHead)
}

class StoreUnitS1(param: ExeUnitParams)(
  implicit p: Parameters,
  override implicit val s: StoreStage.StoreStage = StoreS1()
) extends StoreUnitStage(param) {

}

class StoreUnitS2(param: ExeUnitParams)(
  implicit p: Parameters,
  override implicit val s: StoreStage.StoreStage = StoreS2()
) extends StoreUnitStage(param) {

}

class StoreUnitS3(param: ExeUnitParams)(
  implicit p: Parameters,
  override implicit val s: StoreStage.StoreStage = StoreS3()
) extends StoreUnitStage(param) {

}

class StoreUnitS4(param: ExeUnitParams)(
  implicit p: Parameters,
  override implicit val s: StoreStage.StoreStage = StoreS4()
) extends StoreUnitStage(param) {
  
}

class StoreUnitIO(val param: ExeUnitParams)(implicit p: Parameters) extends XSBundle {
  val redirect = Flipped(ValidIO(new Redirect))
  val csrCtrl = Flipped(new CustomCSRCtrlIO)
  val csrTrigger = Input(new CsrTriggerBundle)
  // Request sources
  val stin = Flipped(Decoupled(new ExuInput(param, hasCopySrc = true)))
  val vecstin = Flipped(Decoupled(new VectorStoreIn))
  val prefetchReq = Flipped(DecoupledIO(new StorePrefetchReq))
  // TLB / PMA / PMP
  val tlb = new TlbRequestIO
  val pmp = Flipped(new PMPRespBundle)
  // DCache
  val dcache = new DCacheStoreIO
  // MDP
  val updateLFST = Valid(new StoreUnitToLFST)
  // Store mask, send to sq in s0
  val toSqMask = Valid(new StoreMaskBundle)
  // Store addr, send to sq in s1
  val toSqAddr = ValidIO(new StoreAddrIO)
  // Exception info and memory type, send to sq in s2
  val toSqAddrRe = Output(new StoreAddrIO)
  // UnalignTail req addr, send to sq in s2
  val toUnalignQueue = DecoupledIO(new UnalignQueueIO)
  // Nuke check req to LoadUnit
  val staNukeQueryReq = ValidIO(new StoreNukeQueryReq)
  // Prefetch Train
  val prefetchTrainHintS1 = Output(Bool())
  val prefetchTrainHintS2 = Output(Bool())
  val prefetchTrain = ValidIO(new LsPrefetchTrainBundle())
  // Feedback to RS in s2, for store issue control
  val feedbackSlow = ValidIO(new RSFeedback)
  // Writeback
  val stout = new NewExuOutput(param)
  val vecstout = DecoupledIO(new VecPipelineFeedbackIO(isVStore = true))
  val exceptionInfo = ValidIO(new MemExceptionInfo)

  val storePipeEmpty = Output(Bool())
  val debugInfo = Output(new DebugLsInfoBundle)
}

class NewStoreUnit(val param: ExeUnitParams)(implicit p: Parameters) extends XSModule {
  val io = IO(new StoreUnitIO(param))
  
  val s0 = Module(new StoreUnitS0(param))
  val s1 = Module(new StoreUnitS1(param))
  val s2 = Module(new StoreUnitS2(param))
  val s3 = Module(new StoreUnitS3(param))
  val s4 = Module(new StoreUnitS4(param))

  // Internal wiring
  // s1 <> s0
  // s2 <> s1
  // s3 <> s2
  // s4 <> s3
  // s0.io.unalignTail <> s1.io.unalignTail

  // IO wiring
  // S0
  s0.io.redirect := io.redirect
  s0.io.stin <> io.stin
  s0.io.vecstin <> io.vecstin
  s0.io.prefetchReq <> io.prefetchReq
  io.tlb.req <> s0.io.tlbReq
  io.tlb.req_kill := s0.io.tlbReqKill
  io.dcache.req <> s0.io.dcacheReq
  io.toSqMask <> s0.io.toSqMask
}

abstract class StoreUnitStage(val param: ExeUnitParams)(
  implicit p: Parameters,
  implicit val s: StoreStage
) extends XSModule with OnStoreStage
  with HasDCacheParameters
  with HasCircularQueuePtrHelper {
  val io_pipeIn = if (afterS1) {
    Some(IO(Flipped(DecoupledIO(new StoreStageIO()(p, prevStage(s))))))
  } else None
  val io_pipeOut = if (!lastStage) {
    Some(IO(DecoupledIO(new StoreStageIO)))
  } else None

  def <>(that: StoreUnitStage): Unit = {
    this.io_pipeIn.foreach(_ <> that.io_pipeOut.get)
  }
}