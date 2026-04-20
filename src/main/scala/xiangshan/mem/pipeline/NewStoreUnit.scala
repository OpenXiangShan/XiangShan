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
import xiangshan.backend.Bundles.{DynInst, ExuInput, MemWriteBack, StoreUnitToLFST, connectSamePort}
import xiangshan.backend.ctrlblock.DebugLsInfoBundle
import xiangshan.backend.exu.ExeUnitParams
import xiangshan.backend.fu.FuConfig._
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
    val dcacheReq = DecoupledIO(new DcacheStoreRequestIO)
  })

  /**
    * Arbitrate all S0 request sources with the following priority::
    *
    * 0. unaligned tail injected from S1
    * 1. vector store elements produced by VSplit
    * 2. scalar store requests issued from the issue queue
    * 3. hardware prefetch requests
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

  // 0. unaligned tail injected from S1
  unalignTail <> io.unalignTail

  // 1. vector store elements produced by VSplit
  vectorIssue.valid := io.vecstin.valid
  connectSamePort(vectorIssue.bits, io.vecstin.bits)
  vectorIssue.bits.DontCareUnalign()
  vectorIssue.bits.DontCareStoreSet()

  // 2. scalar store requests issued from the issue queue
  val stin = io.stin.bits
  val stinUop = stin.toDynInst()
  val stinVAddr = stin.src(0) + SignExt(stin.imm(11,0), VAddrBits)
  val stinFullva = stin.src(0) + SignExt(stin.imm(11,0), XLEN)
  val stinSize = Cat(0.U, LSUOpType.size(stinUop.fuOpType))
  scalarIssue.valid := io.stin.valid
  scalarIssue.bits.entrance := StoreEntrance.scalarIssue.U
  scalarIssue.bits.accessType.instrType := InstrType.scalar.U
  scalarIssue.bits.accessType.isCbo := LSUOpType.isCboAll(stinUop.fuOpType)
  scalarIssue.bits.accessType.isCboNoZero := LSUOpType.isCbo(stinUop.fuOpType)
  scalarIssue.bits.uop := stinUop
  scalarIssue.bits.vaddr := stinVAddr
  scalarIssue.bits.fullva := stinFullva
  scalarIssue.bits.size := stinSize
  scalarIssue.bits.mask := Mux(
    LSUOpType.isCboAll(stinUop.fuOpType),
    Fill(VLEN/8, 1.U(1.W)),
    genVWmask128(stinVAddr, stinSize)
  ) // CBO operations always cover the whole cache line
  scalarIssue.bits.isFirstIssue := stin.isFirstIssue
  scalarIssue.bits.ssid.get := stinUop.ssid
  scalarIssue.bits.storeSetHit.get := stinUop.storeSetHit
  scalarIssue.bits.DontCareUnalign()
  scalarIssue.bits.DontCareVectorFields()

  // 3. hardware prefetch requests
  prefetchReq.valid := io.prefetchReq.valid && io.dcacheReq.ready
  prefetchReq.bits.entrance := StoreEntrance.prefetch.U
  prefetchReq.bits.accessType.instrType := InstrType.prefetch.U
  prefetchReq.bits.accessType.isCbo := false.B
  prefetchReq.bits.accessType.isCboNoZero := false.B
  prefetchReq.bits.vaddr := io.prefetchReq.bits.vaddr
  prefetchReq.bits.fullva := io.prefetchReq.bits.vaddr
  prefetchReq.bits.DontCareUnalign()
  prefetchReq.bits.DontCareVectorFields()
  prefetchReq.bits.DontCareStoreSet()
  prefetchReq.bits.uop := 0.U.asTypeOf(new DynInst())
  prefetchReq.bits.size := MemorySize.Q.U
  prefetchReq.bits.mask := Fill(VLEN/8, 1.U(1.W))
  prefetchReq.bits.isFirstIssue := true.B

  // sources arbitration
  arbiter(sources, sink, Some("RequestSources"))

  // alias for arbitration result
  val uop = sink.bits.uop
  val kill = uop.robIdx.needFlush(io.redirect)
  val entrance = sink.bits.entrance
  val accessType = sink.bits.accessType
  val isUnalignTail = StoreEntrance.isUnalignTail(entrance)
  val isVector = accessType.isVector()
  val isScalar = accessType.isScalar()
  val isHWPrefetch = accessType.isPrefetch()
  val isCbo = accessType.isCbo
  val isCboNoZero = accessType.isCboNoZero

  /**
    * Pipeline connect
    * Unlike later stages, S0 takes the arbitration result (`sink`) as its pipeline input.
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
    *   CBO instructions may have unaligned addresses, but they operate on a whole cache line, so we also set align to true for them.
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
    * - **misalign** is used specifically to denote misalign exception.
    */
  val alignCheckResults = alignCheck(sink.bits.vaddr, sink.bits.size, sink.valid)
  val align = alignCheckResults._1
  val cross16Byte = alignCheckResults._2
  val cross4KPage = alignCheckResults._3
  sink.bits.align.get := Mux(isCbo || isHWPrefetch, true.B, Mux(isUnalignTail, false.B, align))
  sink.bits.unalignHead.get := Mux(isCbo || isHWPrefetch || isUnalignTail, false.B, cross4KPage)
  sink.bits.cross16Byte.get := Mux(isCbo || isHWPrefetch, false.B, Mux(isUnalignTail, true.B, cross16Byte))

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
    // 1.2 cross16Bytes check
    // 1.3 cross4KPage check
    val lowAddr = vaddr.take(pgIdxBits)
    val upAddr = LookupTree(size, List(
      MemorySize.B.U -> 0.U,
      MemorySize.H.U -> 1.U,
      MemorySize.W.U -> 3.U,
      MemorySize.D.U -> 7.U,
      MemorySize.Q.U -> 15.U
    )) +& lowAddr
    val cross16Byte = upAddr(DCacheVWordOffset) =/= lowAddr(DCacheVWordOffset)
    val cross4KPage = upAddr.head(1).asBool
    (align, cross16Byte, cross4KPage)
  }

  // IO assignment
  pipeOut.valid := pipeOutValid
  pipeOut.bits := pipeOutBits

  io.stin.ready := scalarIssue.ready
  io.vecstin.ready := vectorIssue.ready
  io.prefetchReq.ready := prefetchReq.ready

  io.tlbReq.valid := sink.valid
  io.tlbReq.bits.vaddr := sink.bits.vaddr
  io.tlbReq.bits.fullva := sink.bits.fullva
  io.tlbReq.bits.checkfullva := isVector || isScalar
  io.tlbReq.bits.cmd := Mux(isCboNoZero, TlbCmd.read, TlbCmd.write)
  io.tlbReq.bits.hyperinst := LSUOpType.isHsv(uop.fuOpType)
  io.tlbReq.bits.hlvx := false.B
  io.tlbReq.bits.isPrefetch := isHWPrefetch
  io.tlbReq.bits.size := sink.bits.size
  io.tlbReq.bits.kill := false.B
  io.tlbReq.bits.memidx.is_ld := false.B
  io.tlbReq.bits.memidx.is_st := true.B
  io.tlbReq.bits.memidx.idx := uop.sqIdx.value
  io.tlbReq.bits.no_translate := false.B
  io.tlbReq.bits.pmp_addr := DontCare
  io.tlbReq.bits.debug.pc := uop.pc
  io.tlbReq.bits.debug.robIdx := uop.robIdx
  io.tlbReq.bits.debug.isFirstIssue := sink.bits.isFirstIssue
  io.tlbReqKill := false.B

  io.dcacheReq.valid := pipeIn.fire
  io.dcacheReq.bits.cmd := MemoryOpConstants.M_PFW
  io.dcacheReq.bits.vaddr := sink.bits.vaddr
  io.dcacheReq.bits.instrtype := Mux(isHWPrefetch, DCACHE_PREFETCH_SOURCE.U, STORE_SOURCE.U)

  // Perf counters
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

    // UnalignTail req addr to Store Queue
    val toUnalignQueue = DecoupledIO(new UnalignQueueIO)

    // Feedback to RS for store issue control
    val feedBackSlow = ValidIO(new RSFeedback)

    // unalign head sent tlb info to unalign tail
    val unalignHeadTlbHit = Input(Bool())

    // prefetch train hint
    val prefetchTrainHint = Output(Bool())

    val debugInfo = Output(new DebugLsInfoBundle)
  })

  val pipeIn = io_pipeIn.get
  val pipeOut = io_pipeOut.get
  val in = pipeIn.bits

  // alias
  val entrance = in.entrance
  val accessType = in.accessType
  val uop = in.uop
  val robIdx = uop.robIdx
  val fuOpType = uop.fuOpType
  val vaddr = in.vaddr
  val mask = in.mask
  val size = in.size
  val isFirstIssue = in.isFirstIssue
  val ssid = in.ssid.get
  val storeSetHit = in.storeSetHit.get
  val isUnalignTail = StoreEntrance.isUnalignTail(entrance)
  val isVector = accessType.isVector()
  val isScalar = accessType.isScalar()
  val isHWPrefetch = accessType.isPrefetch()
  val isCbo = accessType.isCbo
  val isCboNoZero = accessType.isCboNoZero
  val align = in.align.get
  val isUnalignHead = in.unalignHead.get
  val cross4KPage = isUnalignTail || isUnalignHead
  val cross16Byte = in.cross16Byte.get
  val vecBaseVaddr = in.vecBaseVaddr.get

  val kill = robIdx.needFlush(io.redirect)
  val fire = pipeIn.fire && !kill

  // Tlb & DCache
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
  // The reason for considering the ld case is because of CboNoZero
  // pf: page fault, af: access fault, gpf: guest page fault
  val pf = tlbHit && (tlbException.pf.st || tlbException.pf.ld)
  val af = tlbHit && (tlbException.af.st || tlbException.af.ld)
  val gpf = tlbHit && (tlbException.gpf.st || tlbException.gpf.ld)
  val hasException = pf || af || gpf

  val killDCache = kill || tlbMiss || hasException

  assert(!(pipeIn.valid && !tlbResp.valid))

  // Store trigger
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

  val vecVaddrOffset = Mux(
    isDebugMode || isBreakPoint,
    storeTrigger.io.toLoadStore.triggerVaddr - vecBaseVaddr,
    vaddr + genVFirstUnmask(mask).asUInt - vecBaseVaddr
  )
  val vecTriggerMask = Mux(
    isDebugMode || isBreakPoint,
    storeTrigger.io.toLoadStore.triggerMask,
    0.U
  )

  // Unalign tail inject to s0
  val unalignTailInjectValid = fire && isUnalignHead
  val unalignTail = Wire(io.unalignTail.bits.cloneType)
  connectSamePort(unalignTail, in)
  unalignTail.entrance := StoreEntrance.unalignTail.U
  unalignTail.vaddr := ((vaddr >> DCacheVWordOffset) + 1.U) << DCacheVWordOffset
  unalignTail.fullva := ((fullva >> DCacheVWordOffset) + 1.U) << DCacheVWordOffset
  unalignTail.size := size
  unalignTail.mask := genVWmask(vaddr, LSUOpType.size(fuOpType)) >> DCacheVWordBytes
  unalignTail.align.get := false.B
  unalignTail.unalignHead.get := false.B
  unalignTail.cross16Byte.get := true.B
  unalignTail.DontCareStoreSet()
  assert(!(unalignTailInjectValid && (isCbo || isCboNoZero)))

  // Nuke check to LoadUnit
  val nukeQueryReqValid = fire && tlbHit && !isHWPrefetch
  val nukeQueryReq = Wire(new StoreNukeQueryReq)
  nukeQueryReq.robIdx := robIdx
  nukeQueryReq.paddr := paddr
  nukeQueryReq.mask := mask
  nukeQueryReq.matchType := Mux(
    isCbo,
    StLdNukeMatchType.CacheLine,
    Mux(
      cross16Byte && !cross4KPage,
      StLdNukeMatchType.OctaWord,
      StLdNukeMatchType.Normal
    )
  )

  val updateLFSTValid = fire && tlbHit && isScalar && !isUnalignTail

  /**
    * Generate replay feedback for the RS.
    * A miss here means the request must be replayed after translation ready.
    */
  val canFeedBack = isScalar && !isUnalignHead // unalign head should not feed back.
  val feedBackValid = fire && canFeedBack
  val unalignTailHit = tlbHit && io.unalignHeadTlbHit && io.toUnalignQueue.ready
  val feedBackHit = Mux(isUnalignTail, unalignTailHit, tlbHit)
  val needRSReplay = feedBackValid && !feedBackHit

  /**
    * Information sent to the StoreQueue:
    *
    * Available in S1:
    * - Basic info: paddr, vaddr, mask, size, uop info
    * - Unaligned info: whether this request is unaligned, crosses 16B/4KiB, and
    *   whether it is the last request in a split request.
    *
    * Filled later in S2:
    * - Exception status
    * - Memory type: NC or MMIO
    *
    * [NOTE]: the normal request is also the last request,
    */
  val toSqAddrValid = fire && !isHWPrefetch
  val toSqAddr = Wire(io.toSqAddr.bits.cloneType)
  def alignVWordAddr(addr: UInt) = {
    Cat(addr(addr.getWidth - 1, DCacheVWordOffset), 0.U(DCacheVWordOffset.W))
  }
  toSqAddr.paddr := Mux(isCbo, alignVWordAddr(paddr), paddr)
  toSqAddr.vaddr := Mux(isCbo, alignVWordAddr(vaddr), vaddr)
  toSqAddr.tlbMiss := !feedBackHit
  toSqAddr.mask := mask
  toSqAddr.size := Mux(isCbo, MemorySize.Q.U, size)
  toSqAddr.wlineflag := isCbo
  connectSamePort(toSqAddr.uop, uop)
  toSqAddr.uop.pc.foreach(_ := uop.pc)
  toSqAddr.uop.debugInfo.foreach(_  := uop.perfDebugInfo)
  toSqAddr.uop.debug_seqNum.foreach(_ := uop.debug_seqNum)
  toSqAddr.uop.isFirstIssue := isFirstIssue
  toSqAddr.isHyper := isHyper
  // Unalign info
  toSqAddr.isLastRequest := !isUnalignHead
  toSqAddr.cross4KPage := cross4KPage
  toSqAddr.unalignWithin16Byte := !align && !cross16Byte
  toSqAddr.isUnalign := !align
  // The following will be set in stage 2
  toSqAddr.nc := DontCare
  toSqAddr.mmio := DontCare
  toSqAddr.hasException := DontCare
  toSqAddr.memBackTypeMM := DontCare
  toSqAddr.cacheMiss := false.B

  // Pipeline connect
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
  stageInfo.needRSReplay.get := needRSReplay
  stageInfo.hasException.get := hasException || isDebugMode || isBreakPoint
  stageInfo.uop.trigger := triggerAction
  stageInfo.uop.exceptionVec(breakPoint) := isBreakPoint
  stageInfo.uop.exceptionVec(storePageFault) := pf
  stageInfo.uop.exceptionVec(storeGuestPageFault) := gpf
  stageInfo.uop.exceptionVec(storeAccessFault) := af
  stageInfo.uop.perfDebugInfo.tlbRespTime := Mux(
    tlbHit,
    GTimer(),
    Mux(tlbMiss, uop.perfDebugInfo.tlbFirstReqTime, uop.perfDebugInfo.tlbRespTime)
  )

  when (pipeIn.fire) { pipeOutBits := stageInfo }

  // IO assignment
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

  io.toUnalignQueue.valid := fire && isUnalignTail
  io.toUnalignQueue.bits.sqIdx := uop.sqIdx
  io.toUnalignQueue.bits.paddr := paddr
  io.toUnalignQueue.bits.robIdx := robIdx

  io.feedBackSlow.valid := feedBackValid
  io.feedBackSlow.bits.hit := feedBackHit
  io.feedBackSlow.bits.flushState := tlbResp.bits.ptwBack
  io.feedBackSlow.bits.robIdx := robIdx
  io.feedBackSlow.bits.sourceType := RSFeedbackType.tlbMiss
  io.feedBackSlow.bits.sqIdx := uop.sqIdx
  io.feedBackSlow.bits.lqIdx := uop.lqIdx
  io.feedBackSlow.bits.dataInvalidSqIdx := DontCare

  io.prefetchTrainHint := fire && isFirstIssue

  io.debugInfo := DontCare
  io.debugInfo.s1_robIdx := robIdx.value
  io.debugInfo.s1_isTlbFirstMiss := tlbMiss && !isHWPrefetch && isFirstIssue

  // Perf counters
  XSPerfAccumulate("s1_valid", pipeIn.valid)
  XSPerfAccumulate("s1_fire", fire)
  XSPerfAccumulate("s1_tlbHit", fire && tlbHit)
  XSPerfAccumulate("s1_tlbMiss", fire && tlbMiss)
  XSPerfAccumulate("s1_needRSReplay", needRSReplay)
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
    val dcachePC = Output(UInt(VAddrBits.W))
    val dcacheResp = Flipped(DecoupledIO(new DcacheStoreRespIO()))

    // Exception info and memory type to to Store Queue
    val toSqAddrRe = Output(new StoreAddrIO)

    // unalign head sent tlb info to unalign tail
    val unalignHeadTlbHit = Output(Bool())

    // Prefetch Train
    val prefetchTrainHint = Output(Bool())
    val prefetchTrain = ValidIO(new LsPrefetchTrainBundle())
  })

  val pipeIn = io_pipeIn.get
  val pipeOut = io_pipeOut.get
  val in = pipeIn.bits

  // alias
  val entrance = in.entrance
  val accessType = in.accessType
  val uop = in.uop
  val robIdx = uop.robIdx
  val align = in.align.get
  val isUnalignTail = StoreEntrance.isUnalignTail(entrance)
  val isVector = accessType.isVector()
  val isScalar = accessType.isScalar()
  val isHWPrefetch = accessType.isPrefetch()
  val isCbo = accessType.isCbo
  val isCboNoZero = accessType.isCboNoZero
  val tlbException = in.tlbException.get
  val tlbHit = pipeIn.valid && in.tlbHit.get
  val tlbMiss = pipeIn.valid && !in.tlbHit.get
  val isUnalignHead = in.unalignHead.get
  val cross4KPage = isUnalignTail || isUnalignHead
  val cross16Byte = in.cross16Byte.get

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
  val pmpInaccessible = tlbHit && (pmp.st || pmp.ld && isCboNoZero)
  val tlbInaccessible = uop.exceptionVec(storeAccessFault) ||
    uop.exceptionVec(storePageFault) ||
    uop.exceptionVec(storeGuestPageFault)
  val tlbAccessible = !tlbInaccessible
  val isNC = tlbHit && tlbAccessible && Pbmt.isNC(pbmt)
  val isMMIO = tlbHit && tlbAccessible && (Pbmt.isIO(pbmt) || Pbmt.isPMA(pbmt) && pmp.mmio)
  val isUncache = isNC || isMMIO
  val memBackTypeMM = !pmp.mmio

  val afInaccessible = uop.exceptionVec(storeAccessFault) || pmpInaccessible
  val afVectorUncache = isVector && isUncache
  val afCboUncache = isCbo && isUncache
  val afUnalignMMIO = isMMIO && !align

  val af = afInaccessible || afVectorUncache || afCboUncache || afUnalignMMIO
  val am = !align && isScalar && isNC && !pmpInaccessible
  val hasException = in.hasException.get || af || am

  // DCache
  val cacheMiss = io.dcacheResp.fire && io.dcacheResp.bits.miss
  val killDCache = kill || hasException || isUncache

  // Pipeline connect
  val pipeOutValid = RegInit(false.B)
  val pipeOutBits = Reg(new StoreStageIO)
  when (kill) { pipeOutValid := false.B }
  .elsewhen (pipeIn.fire) { pipeOutValid := true.B }
  .elsewhen (pipeOut.fire) { pipeOutValid := false.B }

  val stageInfo = Wire(pipeOut.bits.cloneType)
  connectSamePort(stageInfo, in)
  stageInfo.uop.exceptionVec(storeAddrMisaligned) := am
  stageInfo.uop.exceptionVec(storeAccessFault) := af
  stageInfo.uop.vpu.vstart := in.vecVaddrOffset.get >> uop.vpu.veew
  stageInfo.hasException.get := hasException
  stageInfo.nc.get := isNC
  stageInfo.mmio.get := isMMIO
  stageInfo.memBackTypeMM.get := memBackTypeMM

  when (pipeIn.fire) { pipeOutBits := stageInfo }

  // IO assignment
  io_pipeOut.get.valid := pipeOutValid
  io_pipeOut.get.bits := pipeOutBits
  io_pipeIn.get.ready := !pipeOutValid || kill || pipeOut.ready

  io.dcacheKill := killDCache
  io.dcachePC := uop.pc
  io.dcacheResp.ready := true.B

  io.toSqAddrRe.memBackTypeMM := memBackTypeMM
  io.toSqAddrRe.mmio := isMMIO
  io.toSqAddrRe.nc := isNC
  io.toSqAddrRe.cacheMiss := cacheMiss
  io.toSqAddrRe.hasException := fire && hasException
  io.toSqAddrRe.isLastRequest := !isUnalignHead
  io.toSqAddrRe.cross4KPage := cross4KPage
  io.toSqAddrRe.unalignWithin16Byte := !align && !cross16Byte
  io.toSqAddrRe.isUnalign := !align
  io.toSqAddrRe.wlineflag := DontCare
  io.toSqAddrRe.paddr := DontCare
  io.toSqAddrRe.vaddr := DontCare
  io.toSqAddrRe.tlbMiss := DontCare
  io.toSqAddrRe.mask := DontCare
  io.toSqAddrRe.size := DontCare
  io.toSqAddrRe.uop := DontCare
  io.toSqAddrRe.isHyper := DontCare

  val prefetchTrainValid = fire && io.dcacheResp.fire && tlbHit && tlbAccessible && !hasException && !isUncache
  io.prefetchTrainHint := prefetchTrainValid
  io.prefetchTrain.valid := prefetchTrainValid
  io.prefetchTrain.bits := DontCare
  io.prefetchTrain.bits.uop := uop
  io.prefetchTrain.bits.vaddr := in.vaddr
  io.prefetchTrain.bits.paddr := in.paddr.get
  io.prefetchTrain.bits.miss := cacheMiss
  io.prefetchTrain.bits.isFirstIssue := in.isFirstIssue
  io.prefetchTrain.bits.meta_prefetch := false.B
  io.prefetchTrain.bits.meta_access := false.B
  io.prefetchTrain.bits.is_from_hw_pf := isHWPrefetch
  io.prefetchTrain.bits.refillLatency := 0.U // TODO: store not for berti, so there is no refillLatency

  io.unalignHeadTlbHit := fire && isUnalignHead && tlbHit

  // Perf counters
  XSPerfAccumulate("s2_valid", pipeIn.valid)
  XSPerfAccumulate("s2_fire", fire)
  XSPerfAccumulate("s2_cacheHit", fire && !cacheMiss)
  XSPerfAccumulate("s2_cacheMiss", fire && cacheMiss)
}

class StoreUnitS3(param: ExeUnitParams)(
  implicit p: Parameters,
  override implicit val s: StoreStage.StoreStage = StoreS3()
) extends StoreUnitStage(param) {
  val io = IO(new Bundle {
    val redirect = Flipped(ValidIO(new Redirect))

    // Unalign head from S4
    val unalignConcat = Flipped(ValidIO(new StoreStageIO))

    // Writeback
    val stout = new MemWriteBack(param)
    val vecstout = DecoupledIO(new VecPipelineFeedbackIO(isVStore = true))
    val exceptionInfo = ValidIO(new MemExceptionInfo)
  })

  val pipeIn = io_pipeIn.get
  val pipeOut = io_pipeOut.get
  val in = pipeIn.bits

  // alias
  val uop = in.uop
  val robIdx = uop.robIdx
  val entrance = in.entrance
  val accessType = in.accessType
  val isUnalignTail = StoreEntrance.isUnalignTail(entrance)
  val isVector = accessType.isVector()
  val isScalar = accessType.isScalar()
  val isHWPrefetch = accessType.isPrefetch()
  val isCbo = accessType.isCbo
  val isMMIO = in.mmio.get
  val hasException = in.hasException.get
  val isUnalignHead = in.unalignHead.get

  val kill = robIdx.needFlush(io.redirect)
  val fire = pipeIn.fire && !kill
  // Only the unaligned head continues into S4; all other requests terminate in S3
  val endPipe = !isUnalignHead

  // Scalar/vector store write back
  val wbType = !isUnalignHead && !isHWPrefetch && !in.needRSReplay.get
  val wbValid = fire && ((!isMMIO && !isCbo) || hasException) && wbType
  val wbData = Wire(in.cloneType)
  wbData := in

  // Unalign head/tail concatenation
  val headValid = io.unalignConcat.valid
  val head = io.unalignConcat.bits
  val headExceptionVec = head.uop.exceptionVec
  val headHasException = headValid && head.hasException.get

  wbData.vaddr := Mux(headValid, head.vaddr, in.vaddr)
  wbData.mask := Mux(headValid, head.mask, in.mask)
  wbData.fullva := Mux(headHasException, head.fullva, in.fullva)
  wbData.hasException.get := hasException || headHasException
  wbData.uop.exceptionVec := Mux(headHasException, headExceptionVec, in.uop.exceptionVec)
  wbData.uop.trigger := Mux(headHasException, head.uop.trigger, in.uop.trigger)
  wbData.tlbException.get := Mux(headHasException, head.tlbException.get, in.tlbException.get)
  wbData.uop.vpu.vstart := Mux(headHasException, head.uop.vpu.vstart, in.uop.vpu.vstart)
  wbData.vecTriggerMask.get := Mux(headHasException, head.vecTriggerMask.get, in.vecTriggerMask.get)
  wbData.isForVSnonLeafPTE.get := Mux(
    headHasException,
    head.isForVSnonLeafPTE.get,
    in.isForVSnonLeafPTE.get
  )
  wbData.paddr.get := Mux(headHasException, head.paddr.get, in.paddr.get)
  wbData.gpaddr.get := Mux(headHasException, head.gpaddr.get, in.gpaddr.get)

  /**
    * stage x
    * To sync with RAW violation checks and send to the backend, delay by x cycles.
    */
  val delayPipe = Module(new DelayPipeline(
    wbData.cloneType, RAWTotalDelayCycles,
    (data: StoreStageIO, redirect: ValidIO[Redirect]) => data.uop.robIdx.needFlush(redirect)
  ))
  delayPipe.io.in.valid := wbValid
  delayPipe.io.in.bits := wbData
  delayPipe.io.out.ready := true.B
  delayPipe.io.redirect := io.redirect
  val sxValid = delayPipe.io.out.valid
  val sxData = delayPipe.io.out.bits
  val sxVector = sxData.accessType.isVector()

  // Pipeline connect
  val pipeOutValid = RegInit(false.B)
  val pipeOutBits = Reg(new StoreStageIO)
  when (kill || endPipe) { pipeOutValid := false.B }
  .elsewhen (pipeIn.fire) { pipeOutValid := true.B }
  .elsewhen (pipeOut.fire) { pipeOutValid := false.B }

  val stageInfo = Wire(pipeOut.bits.cloneType)
  connectSamePort(stageInfo, in)

  when (pipeIn.fire) { pipeOutBits := stageInfo }

  // IO assignment
  io_pipeOut.get.valid := pipeOutValid
  io_pipeOut.get.bits := pipeOutBits
  io_pipeIn.get.ready := !pipeOutValid || kill || pipeOut.ready

  io.stout.toRob.valid := sxValid && !sxVector
  io.stout.toRob.bits.robIdx := sxData.uop.robIdx
  io.stout.toRob.bits.isRVC.foreach(_ := sxData.uop.isRVC)
  io.stout.toRob.bits.trigger.foreach(_ := sxData.uop.trigger)
  io.stout.toRob.bits.sqIdx.foreach(_ := sxData.uop.sqIdx)
  io.stout.toRob.bits.lqIdx.foreach(_ := sxData.uop.lqIdx)
  io.stout.toRob.bits.exceptionVec.foreach(_ := ExceptionNO.selectByFu(sxData.uop.exceptionVec, StaCfg))
  io.stout.toRob.bits.debugInfo.isMMIO.foreach(_ := sxData.mmio.get)
  io.stout.toRob.bits.debugInfo.isNCIO.foreach(_ := sxData.nc.get && !sxData.memBackTypeMM.get)
  io.stout.toRob.bits.debugInfo.isPerfCnt.foreach(_ := false.B)
  io.stout.toRob.bits.debugInfo.paddr.foreach(_ := sxData.paddr.get)
  io.stout.toRob.bits.debugInfo.vaddr.foreach(_ := sxData.vaddr)
  io.stout.toRob.bits.debugInfo.debug_seqNum.foreach(_ := sxData.uop.debug_seqNum)
  io.stout.toRob.bits.debugInfo.perfDebugInfo.foreach(_ := sxData.uop.perfDebugInfo)

  io.exceptionInfo.valid := sxValid && !sxVector
  io.exceptionInfo.bits.robIdx := sxData.uop.robIdx
  io.exceptionInfo.bits.exceptionVec := ExceptionNO.selectByFu(sxData.uop.exceptionVec, StaCfg)
  io.exceptionInfo.bits.vaddr := sxData.fullva
  io.exceptionInfo.bits.gpaddr := sxData.gpaddr.get
  io.exceptionInfo.bits.isForVSnonLeafPTE := sxData.isForVSnonLeafPTE.get
  io.exceptionInfo.bits.vaNeedExt := sxData.tlbException.get.vaNeedExt
  io.exceptionInfo.bits.isHyper := sxData.tlbException.get.isHyper
  io.exceptionInfo.bits.uopIdx := 0.U.asTypeOf(io.exceptionInfo.bits.uopIdx)
  io.exceptionInfo.bits.vl := 0.U.asTypeOf(io.exceptionInfo.bits.vl)
  io.exceptionInfo.bits.vstart := 0.U.asTypeOf(io.exceptionInfo.bits.vstart)

  io.vecstout.valid := sxValid && sxVector
  io.vecstout.bits.mBIndex := sxData.mbIndex.get
  io.vecstout.bits.hit := sxData.tlbHit.get
  io.vecstout.bits.isvec := sxVector
  io.vecstout.bits.sourceType := RSFeedbackType.tlbMiss
  io.vecstout.bits.flushState := DontCare
  io.vecstout.bits.trigger := sxData.uop.trigger
  io.vecstout.bits.nc := sxData.nc.get
  io.vecstout.bits.mmio := sxData.mmio.get
  io.vecstout.bits.exceptionVec := ExceptionNO.selectByFu(sxData.uop.exceptionVec, VstuCfg)
  io.vecstout.bits.hasException := sxData.hasException.get
  io.vecstout.bits.elemIdx := sxData.elemIdx.get
  io.vecstout.bits.alignedType := sxData.size
  io.vecstout.bits.mask := sxData.mask
  io.vecstout.bits.vaddr := sxData.fullva
  io.vecstout.bits.vaNeedExt := sxData.tlbException.get.vaNeedExt
  io.vecstout.bits.gpaddr := sxData.gpaddr.get
  io.vecstout.bits.isForVSnonLeafPTE := sxData.isForVSnonLeafPTE.get
  io.vecstout.bits.vstart := sxData.uop.vpu.vstart
  io.vecstout.bits.vecTriggerMask := sxData.vecTriggerMask.get
}

class StoreUnitS4(param: ExeUnitParams)(
  implicit p: Parameters,
  override implicit val s: StoreStage.StoreStage = StoreS4()
) extends StoreUnitStage(param) {
  val io = IO(new Bundle() {
    val unalignConcat = ValidIO(new StoreStageIO())
  })
  /**
    * Hold the unaligned-head payload for one extra stage
    * so that S3 can recombine it with the tail request.
    */
  io_pipeIn.get.ready := true.B
  io.unalignConcat.valid := io_pipeIn.get.valid
  io.unalignConcat.bits := io_pipeIn.get.bits
}

class StoreUnitIO(val param: ExeUnitParams)(implicit p: Parameters) extends XSBundle {
  val redirect = Flipped(ValidIO(new Redirect))
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
  val feedBackSlow = ValidIO(new RSFeedback)
  // Writeback
  val stout = new MemWriteBack(param)
  val vecstout = DecoupledIO(new VecPipelineFeedbackIO(isVStore = true))
  val exceptionInfo = ValidIO(new MemExceptionInfo)

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
  s2 <> s1
  s3 <> s2
  s4 <> s3
  s0.io.unalignTail <> s1.io.unalignTail
  s1.io.unalignHeadTlbHit := s2.io.unalignHeadTlbHit
  s3.io.unalignConcat <> s4.io.unalignConcat

  // IO wiring
  // S0
  s0.io.redirect := io.redirect
  s0.io.stin <> io.stin
  s0.io.vecstin <> io.vecstin
  s0.io.prefetchReq <> io.prefetchReq
  io.tlb.req <> s0.io.tlbReq
  io.tlb.req_kill := s0.io.tlbReqKill
  io.dcache.req <> s0.io.dcacheReq
  // s1
  s1.io.redirect := io.redirect
  s1.io.csrTrigger := io.csrTrigger
  s1.io.tlbResp <> io.tlb.resp
  io.dcache.s1_paddr := s1.io.dcachePAddr
  io.dcache.s1_kill := s1.io.dcacheKill
  io.updateLFST := s1.io.updateLFST
  io.staNukeQueryReq := s1.io.staNukeQueryReq
  io.toSqAddr := s1.io.toSqAddr
  io.debugInfo := s1.io.debugInfo
  io.prefetchTrainHintS1 := s1.io.prefetchTrainHint
  io.toUnalignQueue <> s1.io.toUnalignQueue
  io.feedBackSlow.valid := RegNext(s1.io.feedBackSlow.valid) // for better timing
  io.feedBackSlow.bits := RegEnable(s1.io.feedBackSlow.bits, s1.io.feedBackSlow.valid)
  // s2
  s2.io.redirect := io.redirect
  s2.io.pmp := io.pmp
  s2.io.dcacheResp <> io.dcache.resp
  io.dcache.s2_kill := s2.io.dcacheKill
  io.dcache.s2_pc := s2.io.dcachePC
  io.toSqAddrRe := s2.io.toSqAddrRe
  io.prefetchTrainHintS2 := s2.io.prefetchTrainHint
  io.prefetchTrain.valid := RegNext(s2.io.prefetchTrain.valid) // for better timing
  io.prefetchTrain.bits := RegEnable(s2.io.prefetchTrain.bits, s2.io.prefetchTrain.valid)
  // s3
  s3.io.redirect := io.redirect
  io.stout := s3.io.stout
  io.vecstout <> s3.io.vecstout
  io.exceptionInfo := s3.io.exceptionInfo
}

class DelayPipeline[T <: Data](gen: T, numDelays: Int, killFn: (T, ValidIO[Redirect]) => Bool)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(gen))
    val out = DecoupledIO(gen)
    val redirect = Flipped(ValidIO(new Redirect))
  })

  val n = numDelays + 1
  val valids = Wire(Vec(n, Bool()))
  val readys = Wire(Vec(n, Bool()))
  val datas = Wire(Vec(n, gen.cloneType))

  private def kill(data: T): Bool = killFn(data, io.redirect)

  valids(0) := io.in.valid
  datas(0) := io.in.bits
  readys(0) := !io.in.valid || kill(datas(0)) ||
               (if (numDelays == 0) io.out.ready else readys(1))

  for (i <- 1 until n) {
    val isLast = (i == numDelays)
    val curKill = kill(datas(i))
    val curCanGo = if (isLast) io.out.ready else readys(i + 1)
    val curFire = valids(i) && !curKill && curCanGo
    val prevFire = valids(i - 1) && !kill(datas(i - 1)) && readys(i)

    readys(i) := !valids(i) || curKill || curCanGo

    val validUpdate = prevFire || curFire || curKill
    valids(i) := RegEnable(Mux(prevFire, true.B, false.B), false.B, validUpdate)
    datas(i) := RegEnable(datas(i - 1), prevFire)
  }

  io.out.valid := valids(n - 1)
  io.out.bits := datas(n - 1)
  io.in.ready := readys(0)
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
