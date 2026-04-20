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
import xiangshan.backend.Bundles.{DynInst, ExuInput, NewExuOutput, StoreUnitToLFST, connectSamePort}
import xiangshan.backend.ctrlblock.DebugLsInfoBundle
import xiangshan.backend.exu.ExeUnitParams
import xiangshan.backend.fu.PMPRespBundle
import xiangshan.backend.fu.NewCSR._
import xiangshan.cache._
import xiangshan.cache.mmu._
import xiangshan.ExceptionNO._
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
  vectorIssue.bits.DontCareStoreSet()

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
  prefetchReq.bits.DontCareStoreSet()
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
  val isHWPrefetch = StoreEntrance.isHWPrefetch(entrance)
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
    *   Prefetch must be within 16 bytes, unalign tail must be cross16Byte
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
  val needAlignCheck = Cat(needAlignCheckValids).orR
  val alignCheckResults = needAlignCheckSources.map(s => alignCheck(s.bits.vaddr, s.bits.size, s.valid)).unzip3
  val align = ParallelPriorityMux(needAlignCheckValids, alignCheckResults._1)
  val cross16Byte = ParallelPriorityMux(needAlignCheckValids, alignCheckResults._2)
  val cross4KPage = ParallelPriorityMux(needAlignCheckValids, alignCheckResults._3)
  
  sink.bits.align.get := Mux(needAlignCheck, align, Mux(isUnalignTail, false.B, true.B))
  sink.bits.unalignHead.get := Mux(needAlignCheck, cross4KPage, false.B)
  sink.bits.cross16Byte.get := Mux(needAlignCheck, cross16Byte, Mux(isUnalignTail, true.B, false.B))
  
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
  pipeOut.valid := pipeOutValid
  pipeOut.bits := pipeOutBits

  io.tlbReq.valid := sink.valid
  io.tlbReq.bits.vaddr := sink.bits.vaddr
  io.tlbReq.bits.fullva := sink.bits.fullva
  io.tlbReq.bits.checkfullva := isVector || isScalar
  io.tlbReq.bits.cmd := Mux(isCboNoZero, TlbCmd.read, TlbCmd.write)
  io.tlbReq.bits.hyperinst := LSUOpType.isHlv(uop.fuOpType)
  io.tlbReq.bits.hlvx := LSUOpType.isHlvx(uop.fuOpType)
  io.tlbReq.bits.isPrefetch := isHWPrefetch
  io.tlbReq.bits.size := sink.bits.size
  io.tlbReq.bits.kill := false.B
  io.tlbReq.bits.memidx.is_ld := false.B
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
  io.dcacheReq.bits.instrtype := Mux(isHWPrefetch, DCACHE_PREFETCH_SOURCE.U, STORE_SOURCE.U)

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
  XSPerfAccumulate("s0_prefetch", fire && isHWPrefetch)
  XSPerfAccumulate("s0_isFirstIssue", fire && pipeIn.bits.isFirstIssue)
  XSPerfAccumulate("s0_isCbo", fire && isCbo)
  XSPerfAccumulate("s0_isCboNoZero", fire && isCboNoZero)
  XSPerfAccumulate("s0_unalign", fire && !pipeIn.bits.align.get)
  XSPerfAccumulate("s0_cross16Byte", fire && pipeIn.bits.cross16Byte.get)
  XSPerfAccumulate("s0_cross4KPage", fire && pipeIn.bits.unalignHead.get)
}

class StoreUnitS1(param: ExeUnitParams)(
  implicit p: Parameters,
  override implicit val s: StoreStage.StoreStage = StoreS1()
) extends StoreUnitStage(param) {
  val io = IO(new Bundle {
    val redirect = Flipped(ValidIO(new Redirect))
    val csrTrigger = Input(new CsrTriggerBundle)

    // Tlb response
    val tlbResp = Flipped(DecoupledIO(new TlbResp))

    // DCache request: paddr and s1 kill signal
    val dcachePAddr = Output(UInt(PAddrBits.W))
    val dcacheKill = Output(Bool())

    // MDP
    val updateLFST = Valid(new StoreUnitToLFST)

    // Nuke check req to LoadUnit
    val staNukeQueryReq = ValidIO(new StoreNukeQueryReq)

    // Send store addr to StoreQueue
    val toSqAddr = ValidIO(new StoreAddrIO)

    // Unalign tail inject to s0
    val unalignTail = DecoupledIO(new StoreStageIO()(p, prevStage(s)))

    // prefetch train hint
    val prefetchTrainHint = Output(Bool())

    val debugInfo = Output(new DebugLsInfoBundle)
  })

  val pipeIn = io_pipeIn.get
  val pipeOut = io_pipeOut.get
  val in = pipeIn.bits
  
  // alias
  val entrance = in.entrance
  val uop = in.uop
  val robIdx = uop.robIdx
  val fuOpType = uop.fuOpType
  val vaddr = in.vaddr
  val mask = in.mask
  val size = in.size
  val isFirstIssue = in.isFirstIssue
  val ssid = in.ssid.get
  val storeSetHit = in.storeSetHit.get
  val vecActive = in.vecActive.get
  val isUnalignTail = StoreEntrance.isUnalignTail(entrance)
  val isVector = StoreEntrance.isVectorIssue(entrance)
  val isScalar = StoreEntrance.isScalarIssue(entrance)
  val isHWPrefetch = StoreEntrance.isHWPrefetch(entrance)
  val align = in.align.get
  val isUnalignHead = isScalar && in.unalignHead.get
  val cross4KPage = isUnalignTail || isUnalignHead
  val cross16Byte = !align && in.cross16Byte.get
  val vecBaseVaddr = in.vecBaseVaddr.get
  val isCbo = isScalar && LSUOpType.isCboAll(uop.fuOpType)

  val kill = robIdx.needFlush(io.redirect)
  val fire = pipeIn.fire && !kill

  /**
    * Tlb & DCache
    */
  val tlbResp = io.tlbResp
  val tlbHit = tlbResp.valid && !tlbResp.bits.miss
  val tlbMiss = tlbResp.valid && tlbResp.bits.miss
  val vaNeedExt = tlbResp.bits.excp(0).vaNeedExt
  val isHyper = tlbResp.bits.excp(0).isHyper
  val isForVSnonLeafPTE = tlbResp.bits.isForVSnonLeafPTE
  val pbmt = Mux(tlbHit, tlbResp.bits.pbmt.head, Pbmt.pma)
  val paddr = tlbResp.bits.paddr(0)
  val gpaddr = tlbResp.bits.gpaddr(0)
  val fullva = tlbResp.bits.fullva
  val tlbException = tlbResp.bits.excp.head
  // vecActive: 1: vector active element, 0: vector not active element, scalar is 1
  // The reason for considering the ld case is because of CboNoZero
  // pf: page fault, af: access fault, gpf: guest page fault
  val pf = tlbHit && (tlbException.pf.st || tlbException.pf.ld) && vecActive
  val af = tlbHit && (tlbException.af.st || tlbException.af.ld) && vecActive
  val gpf = tlbHit && (tlbException.gpf.st || tlbException.gpf.ld) && vecActive
  val hasException = pf || af || gpf

  val killDCache = kill || tlbMiss || hasException

  assert(!(pipeIn.valid && !tlbResp.valid))

  /**
    * Store trigger
    */
  val storeTrigger = Module(new MemTrigger(MemType.STORE))
  val isVectorUnitStride = isVector && VstuType.isUnitStride(fuOpType)
  storeTrigger.io.fromCsrTrigger.tdataVec := io.csrTrigger.tdataVec
  storeTrigger.io.fromCsrTrigger.tEnableVec := io.csrTrigger.tEnableVec
  storeTrigger.io.fromCsrTrigger.triggerCanRaiseBpExp := io.csrTrigger.triggerCanRaiseBpExp
  storeTrigger.io.fromCsrTrigger.debugMode := io.csrTrigger.debugMode
  storeTrigger.io.fromLoadStore.vaddr := vaddr
  storeTrigger.io.fromLoadStore.mask := mask
  storeTrigger.io.fromLoadStore.isVectorUnitStride := isVectorUnitStride
  storeTrigger.io.isCbo.get := isCbo

  val triggerAction = storeTrigger.io.toLoadStore.triggerAction
  val isDebugMode = TriggerAction.isDmode(triggerAction)
  val isBreakPoint = TriggerAction.isExp(triggerAction)
  
  val vecTriggerMask = Mux(
    isDebugMode || isBreakPoint,
    storeTrigger.io.toLoadStore.triggerVaddr - vecBaseVaddr,
    vaddr + genVFirstUnmask(mask).asUInt - vecBaseVaddr
  )
  val vecVaddrOffset = Mux(
    isDebugMode || isBreakPoint,
    storeTrigger.io.toLoadStore.triggerMask,
    0.U
  )

  /**
    * Unalign tail inject to s0
    */
  val unalignTailInjectValid = fire && isUnalignHead
  val unalignTail = Wire(io.unalignTail.bits.cloneType)
  connectSamePort(unalignTail, in)
  unalignTail.entrance := StoreEntrance.unalignTail.U
  unalignTail.vaddr := ((vaddr >> DCacheVWordOffset) + 1.U) << DCacheVWordOffset
  unalignTail.fullva := ((fullva >> DCacheVWordOffset) + 1.U) << DCacheVWordOffset
  unalignTail.size := MemorySize.Q.U // TODO: unalignTail size
  unalignTail.mask := genVWmask(vaddr, LSUOpType.size(fuOpType)) >> DCacheVWordBytes
  unalignTail.align.get := false.B
  unalignTail.unalignHead.get := false.B
  unalignTail.cross16Byte.get := true.B
  unalignTail.DontCareStoreSet()

  /**
    * Nuke check to LoadUnit
    * 
    * MDP
    */
  val nukeQueryReqValid = fire && tlbHit && !isHWPrefetch
  val nukeQueryReq = Wire(new StoreNukeQueryReq)
  nukeQueryReq.robIdx := robIdx
  nukeQueryReq.paddr := paddr
  nukeQueryReq.mask := mask
  nukeQueryReq.matchType := Mux(
    isCbo,
    StLdNukeMatchType.CacheLine,
    Mux(
      cross16Byte,
      StLdNukeMatchType.OctaWord,
      StLdNukeMatchType.Normal
    )
  )

  val updateLFSTValid = fire && tlbHit && isScalar

  /**
    * To Store Queue:
    * 
    * 0. Basic info: paddr, vaddr, mask, size, uop info
    * 1. Unaligned info: Indicates whether the store is unaligned, 
    *    crosses a 16B boundary or a 4K page boundary, whether it is 
    *    the last request in a split request.
    * 2. Exception info: whether this store has exception
    * 3. Memory type: NC or MMIO
    * 
    * [NOTE]: the normal request is also the last request,
    *         Exception info and memory type will be sent in s2
    */
  val toSqAddrValid = fire && tlbHit && !isHWPrefetch
  val toSqAddr = Wire(io.toSqAddr.bits.cloneType)
  toSqAddr.paddr := paddr
  toSqAddr.vaddr := vaddr
  toSqAddr.tlbMiss := tlbMiss
  toSqAddr.mask := mask
  toSqAddr.size := size
  connectSamePort(toSqAddr.uop, uop)
  toSqAddr.uop.pc.get := uop.pc
  toSqAddr.uop.debugInfo.get := uop.perfDebugInfo
  toSqAddr.uop.debug_seqNum.get := uop.debug_seqNum
  toSqAddr.uop.isFirstIssue := isFirstIssue
  // Unalign info
  toSqAddr.isLastRequest := !isUnalignHead
  toSqAddr.cross4KPage := cross4KPage
  toSqAddr.unalignWithin16Byte := cross16Byte
  toSqAddr.isUnalign := !align
  // The following will be set in stage 2
  toSqAddr.nc := DontCare
  toSqAddr.mmio := DontCare
  toSqAddr.af := DontCare
  toSqAddr.hasException := DontCare
  toSqAddr.memBackTypeMM := DontCare
  toSqAddr.cacheMiss := false.B

  /**
    * Pipeline connect
    */
  val pipeOutValid = RegInit(false.B)
  val pipeOutBits = Reg(new StoreStageIO)
  when (kill) { pipeOutValid := false.B }
  .elsewhen (pipeIn.fire) { pipeOutValid := true.B }
  .elsewhen (pipeOut.fire) { pipeOutValid := false.B }

  val stageInfo = Wire(pipeOut.bits.cloneType)
  connectSamePort(stageInfo, in)
  stageInfo.tlbHit.get := tlbHit
  stageInfo.tlbException.get := tlbException
  stageInfo.pbmt.get := pbmt
  stageInfo.isForVSnonLeafPTE.get := isForVSnonLeafPTE
  stageInfo.paddr.get := paddr
  stageInfo.gpaddr.get := gpaddr
  stageInfo.vecTriggerMask.get := vecTriggerMask
  stageInfo.vecVaddrOffset.get := vecVaddrOffset
  stageInfo.feedBack.get := pipeIn.valid && !kill && isScalar // TODO: why need use s1 redirect
  stageInfo.vecFeedBack.get := pipeIn.valid && !kill && isVector // TODO: s2 add tlb hit or needreplay ?
  stageInfo.hasException.get := hasException || isDebugMode
  stageInfo.ptwBack.get := tlbResp.bits.ptwBack
  stageInfo.uop.flushPipe := false.B
  stageInfo.uop.trigger := triggerAction
  stageInfo.uop.exceptionVec(breakPoint) := isBreakPoint
  stageInfo.uop.exceptionVec(storePageFault) := pf
  stageInfo.uop.exceptionVec(storeGuestPageFault) := gpf
  stageInfo.uop.exceptionVec(storeAccessFault) := af
  stageInfo.uop.perfDebugInfo.tlbRespTime := Mux(
    tlbHit,
    GTimer(),
    Mux(tlbMiss, uop.perfDebugInfo.tlbFirstReqTime, uop.perfDebugInfo.tlbRespTime)
  ) // TODO: why need check is st ???

  when (pipeIn.fire) { pipeOutBits := stageInfo }

  /**
    * IO assignment
    */
  io_pipeOut.get.valid := pipeOutValid
  io_pipeOut.get.bits := pipeOutBits
  io_pipeIn.get.ready := !pipeOutValid || kill || pipeOut.ready

  io.tlbResp.ready := true.B
  io.dcachePAddr := paddr
  io.dcacheKill := killDCache

  io.updateLFST.valid := updateLFSTValid
  io.updateLFST.bits.robIdx := robIdx
  io.updateLFST.bits.ssid := ssid
  io.updateLFST.bits.storeSetHit := storeSetHit
  
  io.staNukeQueryReq.valid := nukeQueryReqValid
  io.staNukeQueryReq.bits := nukeQueryReq

  io.unalignTail.valid := unalignTailInjectValid
  io.unalignTail.bits := unalignTail

  io.toSqAddr.valid := toSqAddrValid
  io.toSqAddr.bits := toSqAddr

  io.prefetchTrainHint := fire && isFirstIssue
  
  io.debugInfo := DontCare
  io.debugInfo.s1_robIdx := robIdx
  io.debugInfo.s1_isTlbFirstMiss := tlbMiss && !isHWPrefetch && isFirstIssue

  /**
    * Perf counters
    */
  XSPerfAccumulate("s1_valid", pipeIn.valid)
  XSPerfAccumulate("s1_fire", fire)
  XSPerfAccumulate("s1_tlbHit", fire && tlbHit)
  XSPerfAccumulate("s1_tlbMiss", fire && tlbMiss)
}

class StoreUnitS2(param: ExeUnitParams)(
  implicit p: Parameters,
  override implicit val s: StoreStage.StoreStage = StoreS2()
) extends StoreUnitStage(param) {
  val io = IO(new Bundle {
    val redirect = Flipped(ValidIO(new Redirect))

    // PMP result
    val pmp = Flipped(new PMPRespBundle)

    // DCache
    val dcacheKill = Output(Bool())
    val dcacheResp = Flipped(DecoupledIO(new DCacheWordResp))
    
    // Exception info and memory type to to Store Queue
    val toSqAddrRe = Output(new StoreAddrIO)

    // UnalignTail req addr to Store Queue
    val toUnalignQueue = DecoupledIO(new UnalignQueueIO)

    // Feedback to RS for store issue control
    val feedbackSlow = ValidIO(new RSFeedback)

    // Prefetch Train
    val prefetchTrainHint = Output(Bool())
    val prefetchTrain = ValidIO(new LsPrefetchTrainBundle())
  })

  val pipeIn = io_pipeIn.get
  val pipeOut = io_pipeOut.get
  val in = pipeIn.bits

  // alias
  val entrance = in.entrance
  val uop = in.uop
  val robIdx = uop.robIdx
  val align = in.align.get
  val vecActive = in.vecActive.get
  val isUnalignTail = StoreEntrance.isUnalignTail(entrance)
  val isVector = StoreEntrance.isVectorIssue(entrance)
  val isScalar = StoreEntrance.isScalarIssue(entrance)
  val isHWPrefetch = StoreEntrance.isHWPrefetch(entrance)
  val isCbo = isScalar && LSUOpType.isCboAll(uop.fuOpType)
  val isCboNoZero = isScalar && LSUOpType.isCbo(uop.fuOpType)
  val tlbException = in.tlbException.get
  val tlbHit = pipeIn.valid && in.tlbHit.get
  val tlbMiss = pipeIn.valid && !in.tlbHit.get
  val isUnalignHead = isScalar && in.unalignHead.get
  val cross4KPage = isUnalignTail || isUnalignHead
  val cross16Byte = !align && in.cross16Byte.get

  val kill = robIdx.needFlush(io.redirect)
  val fire = pipeIn.fire && !kill

  /**
    * PMP result & Exception Handling
    * 
    * The response signal of `pmp/pma` is credible only after the physical address is actually generated.
    * Therefore, the response signals of pmp/pma generated after an address translation has produced an 
    * `access fault` or a `page fault` are completely unreliable.
    * 
    * Therefore:
    * - `tlbAccessible` is used to guard all PMP/PMA-based checks.
    * - Once TLB translation reports an unaccessible condition, we should not trust PMA/PMP attributes
    * 
    * Abbreviations:
    * - af  : access fault
    * - am  : misaligned access exception
    * - pf  : page fault
    * - gpf : guest page fault
    */
  val pmp = io.pmp
  val pbmt = in.pbmt.get
  val pmpInaccessible = tlbHit && (pmp.st || pmp.ld && isCboNoZero) && vecActive
  val tlbInaccessible = uop.exceptionVec(storeAccessFault) ||
    uop.exceptionVec(storePageFault) ||
    uop.exceptionVec(storeGuestPageFault)
  val tlbAccessible = !tlbInaccessible
  val isNC = tlbHit && tlbAccessible && Pbmt.isNC(pbmt)
  val isMMIO = tlbHit && tlbAccessible && (Pbmt.isIO(pbmt) || Pbmt.isPMA(pbmt) && pmp.mmio)
  val isUncache = isNC || isMMIO

  val afInaccessible = uop.exceptionVec(storeAccessFault) || pmpInaccessible
  val afVectorUncache = isVector && isUncache
  val afCboUncache = isCbo && isUncache
  val afUnalignMMIO = isMMIO && !align
  
  val af = afInaccessible || afVectorUncache || afCboUncache || afUnalignMMIO
  val am = !align && isScalar && isNC && !pmpInaccessible
  val hasException = in.hasException.get || af || am

  /**
    * DCache
    */
  val cacheMiss = io.dcacheResp.fire && io.dcacheResp.bits.miss
  val killDCache = kill || hasException || isUncache

  /**
    * Unalign tail handling
    */
  // TODO: toUnalignQueue s1 ???
  // TODO: s2 Backpressure affects the timing of feedback.
  val unalignTailNeedReplay = fire && isUnalignTail && !io.toUnalignQueue.ready
  val feedBackValid = in.feedBack.get || unalignTailNeedReplay
  val feedBackHit = tlbHit && !unalignTailNeedReplay

  /**
    * Pipeline connect
    */
  val pipeOutValid = RegInit(false.B)
  val pipeOutBits = Reg(new StoreStageIO)
  when (kill) { pipeOutValid := false.B }
  .elsewhen (pipeIn.fire) { pipeOutValid := true.B }
  .elsewhen (pipeOut.fire) { pipeOutValid := false.B }

  val stageInfo = Wire(pipeOut.bits.cloneType)
  connectSamePort(stageInfo, in)
  stageInfo.uop.exceptionVec(storeAddrMisaligned) := am
  stageInfo.uop.exceptionVec(storeAccessFault) := af
  stageInfo.pmp.get := pmp
  stageInfo.hasException.get := hasException
  stageInfo.nc.get := isNC
  stageInfo.mmio.get := isMMIO

  when (pipeIn.fire) { pipeOutBits := stageInfo }

  /**
    * IO assignment
    */
  io_pipeOut.get.valid := pipeOutValid
  io_pipeOut.get.bits := pipeOutBits
  io_pipeIn.get.ready := !pipeOutValid || kill || pipeOut.ready

  io.dcacheKill := killDCache
  io.dcacheResp.ready := true.B
  
  io.toSqAddrRe.memBackTypeMM := !pmp.mmio // TODO: isMMIO ?
  io.toSqAddrRe.af := fire && af // TODO: af is useless ???
  // TODO: reuse `mmiostall` logic in sq
  io.toSqAddrRe.mmio := (isMMIO || isCboNoZero) && !hasException
  io.toSqAddrRe.nc := isNC // TODO: isNC need (!hasException) ??
  io.toSqAddrRe.cacheMiss := cacheMiss
  io.toSqAddrRe.hasException := fire && hasException
  io.toSqAddrRe.isLastRequest := !isUnalignHead
  io.toSqAddrRe.cross4KPage := cross4KPage
  io.toSqAddrRe.unalignWithin16Byte := cross16Byte
  io.toSqAddrRe.isUnalign := !align
  io.toSqAddrRe.paddr := DontCare
  io.toSqAddrRe.vaddr := DontCare
  io.toSqAddrRe.tlbMiss := DontCare
  io.toSqAddrRe.mask := DontCare
  io.toSqAddrRe.size := DontCare
  io.toSqAddrRe.uop := DontCare

  io.toUnalignQueue.valid := fire && isUnalignTail
  io.toUnalignQueue.bits.sqIdx := uop.sqIdx
  io.toUnalignQueue.bits.paddr := in.paddr.get
  io.toUnalignQueue.bits.robIdx := robIdx

  io.feedbackSlow.valid := feedBackValid
  io.feedbackSlow.bits.hit := feedBackHit
  io.feedbackSlow.bits.flushState := in.ptwBack.get
  io.feedbackSlow.bits.robIdx := robIdx
  io.feedbackSlow.bits.sourceType := RSFeedbackType.tlbMiss
  io.feedbackSlow.bits.sqIdx := uop.sqIdx
  io.feedbackSlow.bits.lqIdx := uop.lqIdx

  val prefetchTrainValid = io.dcacheResp.fire && tlbHit && tlbAccessible && !hasException && !isUncache
  io.prefetchTrainHint := prefetchTrainValid
  io.prefetchTrain.valid := prefetchTrainValid
  io.prefetchTrain.bits := DontCare
  io.prefetchTrain.bits.uop := uop
  io.prefetchTrain.bits.vaddr := in.vaddr
  io.prefetchTrain.bits.paddr := in.paddr.get
  io.prefetchTrain.bits.miss := cacheMiss
  io.prefetchTrain.bits.isFirstIssue := in.isFirstIssue
  io.prefetchTrain.bits.meta_prefetch := io.dcacheResp.bits.meta_prefetch
  io.prefetchTrain.bits.meta_access := io.dcacheResp.bits.meta_access
  io.prefetchTrain.bits.is_from_hw_pf := isHWPrefetch
  io.prefetchTrain.bits.refillLatency := 0.U // TODO: store not for berti, so there is no refillLatency
  /**
    * Perf counters
    */
  XSPerfAccumulate("s2_valid", pipeIn.valid)
  XSPerfAccumulate("s2_fire", fire)
  XSPerfAccumulate("s2_unalignTailReplay", fire && unalignTailNeedReplay)
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
  s1 <> s0
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
  // s1
  s1.io.redirect := io.redirect
  // s1.csrCtrl := io.csrCtrl // TODO: io.csrCtrl ???
  s1.io.csrTrigger := io.csrTrigger
  s1.io.tlbResp := io.tlb.resp
  io.dcache.s1_paddr := s1.io.dcachePAddr
  io.dcache.s1_kill := s1.io.dcacheKill
  io.updateLFST := s1.io.updateLFST
  io.staNukeQueryReq := s1.io.staNukeQueryReq
  io.toSqAddr := s1.io.toSqAddr
  io.debugInfo := s1.io.debugInfo
  io.prefetchTrainHintS1 := s1.io.prefetchTrainHint
  // s2
  s2.io.redirect := io.redirect
  s2.io.pmp := io.pmp
  s2.io.dcacheResp := io.dcache.resp
  io.dcache.s2_kill := s2.io.dcacheKill
  io.toSqAddrRe := s2.io.toSqAddrRe
  io.toUnalignQueue <> s2.io.toUnalignQueue
  io.feedbackSlow := s2.io.feedbackSlow
  io.prefetchTrainHintS2 := s2.io.prefetchTrainHint
  io.prefetchTrain.valid := RegNext(s2.io.prefetchTrain.valid) // TODO: GatedValidRegNext ?
  io.prefetchTrain.bits := RegEnable(s2.io.prefetchTrain.bits, s2.io.prefetchTrain.valid)
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