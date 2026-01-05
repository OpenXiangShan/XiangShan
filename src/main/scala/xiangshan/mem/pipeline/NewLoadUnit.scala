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
import top.{ArgParser, Generator}
import utility._
import xiangshan._
import xiangshan.ExceptionNO._
import xiangshan.backend.Bundles.{ExuInput, ExuOutput, MemWakeUpBundle, UopIdx, connectSamePort}
import xiangshan.backend.fu.PMPRespBundle
import xiangshan.backend.fu.FuConfig._
import xiangshan.backend.fu.fpu.FPU
import xiangshan.backend.ctrlblock.DebugLsInfoBundle
import xiangshan.backend.fu.NewCSR._
import xiangshan.backend.exu.ExeUnitParams
import xiangshan.mem.Bundles._
import xiangshan.mem.LoadReplayCauses._
import xiangshan.mem.LoadStage._
import xiangshan.cache._
import xiangshan.cache.mmu._

class LoadUnitS0(param: ExeUnitParams)(
  implicit p: Parameters,
  override implicit val s: LoadStage = LoadS0()
) extends LoadUnitStage(param)
  with HasL1PrefetchSourceParameter {
  val io = IO(new Bundle() {
    /**
      * Request sources
      */
    val unalignTail = Flipped(DecoupledIO(new LoadStageIO))
    val replay = Flipped(DecoupledIO(new LoadReplayIO))
    val fastReplay = Flipped(DecoupledIO(new FastReplayIO))
    // TODO: canAcceptHigh/LowConfPrefetch
    val prefetchReq = Flipped(DecoupledIO(new L1PrefetchReq))
    val vecldin = Flipped(DecoupledIO(new VectorLoadIn))
    val ldin = Flipped(DecoupledIO(new ExuInput(param, hasCopySrc = true)))

    // Tlb request
    val tlbReq = DecoupledIO(new TlbReq)

    // DCache request
    // TODO: move pf_source, is128Req, debug info like s*_pc into req
    // TODO: remove replacement updated
    val dcacheReq = DecoupledIO(new DCacheWordReq)
    val is128Req = Bool()
    val replacementUpdated = Bool()
    val pfSource = Output(UInt(L1PfSourceBits.W))

    /**
      * Data forward request, including:
      * 1. LSQ / Sbuffer STLF
      * 2. UncacheBuffer
      * 3. MSHR and TileLink-D channel
      */
    val sqSbForwardReq = ValidIO(new StoreForwardReqS0)
    val uncacheForwardReq = ValidIO(new StoreForwardReqS0)
    val mshrForwardReq = ValidIO(new DCacheForwardReqS0)
    val tldForwardReq = ValidIO(new DCacheForwardReqS0)
    val uncacheBypassReq = ValidIO(new UncacheBypassReqS0)

    // IQ wakeup
    val wakeup = ValidIO(new MemWakeUpBundle)

    // Debug info
    val debugInfo = Output(new Bundle() {
      val pc = Output(UInt(VAddrBits.W))
    })
  })

  /**
    * Request sources arbitration, in order of priority:
    * 0. unalign tail inject from s1
    * 1. high-priority replay from LRQ, including NC / MMIO replay
    * 2. fast replay from s3
    * 3. low-priority replay from LRQ
    * 4. high-confidence prefetch
    * 5. vector elements splited by VSplit
    * 6. loads issued from IQ
    * 7. low-confidence prefetch
    */
  val unalignTail,
    replayHiPrio,
    fastReplay,
    replayLoPrio,
    prefetchHiConf,
    vectorIssue,
    scalarIssue,
    prefetchLoConf = Wire(DecoupledIO(new LoadStageIO))

  val sources = Seq(
    unalignTail,
    replayHiPrio,
    fastReplay,
    replayLoPrio,
    prefetchHiConf,
    vectorIssue,
    scalarIssue,
    prefetchLoConf
  )
  val sink = Wire(DecoupledIO(new LoadStageIO))

  // 0. unalign tail inject from s1
  unalignTail <> io.unalignTail

  // 1. high-priority replay from LRQ, including NC / MMIO replay
  val replay = Wire(new LoadStageIO)
  connectSamePort(replay, io.replay.bits)
  replay.noQuery.get := io.replay.bits.uncacheReplay.get
  replay.DontCarePAddr()
  replay.DontCareUnalign() // assign later in sink
  val replayIsHiPrio = io.replay.bits.forwardDChannel.get // TODO: consider uncache replay
  replayHiPrio.valid := io.replay.valid && replayIsHiPrio
  replayHiPrio.bits := replay
  replayHiPrio.bits.entrance := LoadEntrance.replayHiPrio.U

  // 2. fast replay from s3
  fastReplay.valid := io.fastReplay.valid
  connectSamePort(fastReplay.bits, io.fastReplay.bits)
  fastReplay.bits.noQuery.get := true.B
  fastReplay.bits.entrance := io.fastReplay.bits.entrance | LoadEntrance.fastReplay.U
  fastReplay.bits.DontCareUnalign() // assign later in sink

  // 3. low-priority replay from LRQ
  val replayStall = io.ldin.valid && isAfter(io.replay.bits.uop.lqIdx, io.ldin.bits.lqIdx.get) ||
    io.vecldin.valid && isAfter(io.replay.bits.uop.lqIdx, io.vecldin.bits.uop.lqIdx)
  val replayIsLoPrio = !io.replay.bits.forwardDChannel.get && !replayStall
  replayLoPrio.valid := io.replay.valid && replayIsLoPrio
  replayLoPrio.bits := replay
  replayLoPrio.bits.entrance := LoadEntrance.replayLoPrio.U
  
  // 4. high-confidence prefetch
  val prefetch = Wire(new LoadStageIO)
  val prefetchIsHiConf = io.prefetchReq.bits.confidence > 0.U
  prefetch.entrance := 0.U // assign later
  prefetch.accessType.instrType := InstrType.prefetch.U
  prefetch.accessType.pftType := PrefetchType.hwData
  prefetch.accessType.pftCoh := Mux(io.prefetchReq.bits.is_store, PrefetchCoh.write, PrefetchCoh.read)
  prefetch.uop := DontCare
  prefetch.vaddr := io.prefetchReq.bits.getVaddr() // not actual vaddr, but Cat(alias, page offset)
  prefetch.fullva := io.prefetchReq.bits.getVaddr()
  prefetch.size := DontCare
  prefetch.mask := 0.U
  prefetch.DontCarePAddr()
  prefetch.noQuery.get := true.B
  prefetch.DontCareUnalign() // assign later in sink
  prefetch.DontCareReplayFromLRQFields()
  prefetch.DontCareVectorFields()
  prefetch.hasROBEntry := false.B
  prefetch.missDbUpdated := false.B
  prefetchHiConf.valid := io.prefetchReq.valid && prefetchIsHiConf
  prefetchHiConf.bits := prefetch
  prefetchHiConf.bits.entrance := LoadEntrance.prefetchHiConf.U

  // 5. vector elements splited by VSplit
  vectorIssue.valid := io.vecldin.valid
  connectSamePort(vectorIssue.bits, io.vecldin.bits)
  vectorIssue.bits.entrance := LoadEntrance.vectorIssue.U
  vectorIssue.bits.accessType.instrType := InstrType.vector.U
  vectorIssue.bits.accessType.pftType := DontCare
  vectorIssue.bits.accessType.pftCoh := DontCare
  vectorIssue.bits.DontCarePAddr()
  vectorIssue.bits.noQuery.get := false.B
  vectorIssue.bits.DontCareUnalign() // assign later in sink
  vectorIssue.bits.DontCareReplayFromLRQFields()

  // 6. loads issued from IQ
  val ldin = io.ldin.bits
  val ldinVAddr = ldin.src(0) + SignExt(ldin.imm(11, 0), VAddrBits)
  val ldinFullva = ldin.src(0) + SignExt(ldin.imm(11, 0), XLEN)
  val ldinSize = LSUOpType.size(ldin.fuOpType) // B, H, W, D, excluding of Q
  scalarIssue.valid := io.ldin.valid
  scalarIssue.bits.entrance := LoadEntrance.scalarIssue.U
  scalarIssue.bits.accessType.instrType := Mux(
    LSUOpType.isPrefetch(ldin.fuOpType),
    InstrType.prefetch.U, // software prefetch
    InstrType.scalar.U
  )
  scalarIssue.bits.accessType.pftType := Mux( // valid only when instrType is prefetch
    ldin.fuOpType === LSUOpType.prefetch_i,
    PrefetchType.swInstr,
    PrefetchType.swData
  )
  scalarIssue.bits.accessType.pftCoh := Mux( // valid only when pftType is not swInstr
    ldin.fuOpType === LSUOpType.prefetch_w,
    PrefetchCoh.write,
    PrefetchCoh.read
  )
  scalarIssue.bits.uop := ldin.toDynInst()
  scalarIssue.bits.vaddr := ldinVAddr
  scalarIssue.bits.fullva := ldinFullva
  scalarIssue.bits.size := ldinSize
  scalarIssue.bits.mask := Mux(LSUOpType.isPrefetch(ldin.fuOpType), 0.U, genVWmask(ldinVAddr, ldinSize))
  scalarIssue.bits.DontCarePAddr()
  scalarIssue.bits.noQuery.get := ldin.fuOpType === LSUOpType.prefetch_i // swInstr
  scalarIssue.bits.DontCareUnalign() // assign later in sink
  scalarIssue.bits.DontCareReplayFromLRQFields()
  scalarIssue.bits.DontCareVectorFields()
  scalarIssue.bits.hasROBEntry := true.B
  scalarIssue.bits.missDbUpdated := false.B

  // 7. low-confidence prefetch
  prefetchLoConf.valid := io.prefetchReq.valid
  prefetchLoConf.bits := prefetch
  prefetchLoConf.bits.entrance := LoadEntrance.prefetchLoConf.U

  // sources arbitration
  arbiter(sources, sink, Some("RequestSources"))
  val pipeIn = Wire(DecoupledIO(new LoadStageIO))
  pipeIn.valid := sink.valid && io.dcacheReq.ready
  sink.ready := pipeIn.ready && io.dcacheReq.ready
  connectSamePort(pipeIn.bits, sink.bits)

  // alias for arbitration result
  val uop = sink.bits.uop
  val isPrefetch = sink.bits.accessType.isPrefetch()
  val isSwInstrPrefetch = sink.bits.accessType.isInstrPrefetch()
  val isHwPrefetch = sink.bits.accessType.isHwPrefetch()
  val isUncacheReplay = sink.bits.isUncacheReplay()

  /**
    * Tlb access
    * 
    * It should be noted that when sending a request to TLB, `req.valid` does not need to be a strict valid signal. We
    * only need to ensure that `req.valid` is HIGH in all cases requiring TLB translation. Meanwhile the strict signal
    * over whether addr translation is actually performed is controled by `noQuery`.
    */
  val needTlbTransSources = Seq(unalignTail, replayHiPrio, replayLoPrio, vectorIssue, scalarIssue)
  val tlbReqValid = Cat(needTlbTransSources.map(_.valid)).orR && io.dcacheReq.ready
  val tlbFuOpType = ParallelPriorityMux(needTlbTransSources.map(s => (s.valid -> s.bits.uop.fuOpType)))
  val tlbVAddr = ParallelPriorityMux(needTlbTransSources.map(s => (s.valid -> s.bits.vaddr)))
  val tlbFullva = sink.bits.fullva
  val tlbHlv = LSUOpType.isHlv(tlbFuOpType)
  val tlbHlvx = LSUOpType.isHlvx(tlbFuOpType)

  val noQuery = sink.bits.noQuery.get

  val firstIssueSources = Seq(vectorIssue, scalarIssue)
  val firstIssue = Cat(firstIssueSources.map(_.fire)).orR
  val tlbCheckFullva = firstIssue

  /**
    * Unalign handling
    * 
    * 1. For requests that are not unalign tail or prefetch
    *   1.1 Align check: check if the address is aligned, which is used to detect misalign exception in later stages
    *   1.2 Bank bound check: simultaneously check whether this address crosses an aligned 16B bank boundary, which is
    *     used to inject an unalign tail in the next stage
    *   1.3 Word bound check: simultaneously check whether this address crosses an aligned 8B bank boundary. If yes,
    *     read the whole 16B bank when accessing DCache
    * 2. For requests that are unalign tail
    *   Do nothing
    * 
    * Some terminology explanations:
    * - **align** indicates whether the addr is aligned with the operation size. `!align` does not necessary mean
    *   splitting is required, but is only used for determining exception in subsequent stages.
    * - **unalign** indicates that under the condition of align, the operation range exceeds aligned 16B bank boundary,
    *   requiring splitting into 2 operations on DCache.
    * - **misalign** is used specifically to denote misalign exception.
    */
  val needAlignCheckSources = Seq(replayHiPrio, fastReplay, replayLoPrio, vectorIssue, scalarIssue)
  val needAlignCheckValids = needAlignCheckSources.map(_.valid)
  val noAlignCheckSources = sources.filterNot(needAlignCheckSources.contains) // unalign tail, hardware prefetch
  val noAlignCheck = Cat(noAlignCheckSources.map(_.fire)).orR || isPrefetch // unalign tail, hardware & software prefetch
  val needAlignCheck = !noAlignCheck

  val alignCheckResults = needAlignCheckSources.map(s => alignCheck(s.bits.bankOffset(), s.bits.size)).unzip3
  val _align = ParallelPriorityMux(needAlignCheckValids, alignCheckResults._1)
  val _crossWordInsideBank = ParallelPriorityMux(needAlignCheckValids, alignCheckResults._2)
  val _crossBank = ParallelPriorityMux(needAlignCheckValids, alignCheckResults._3)
  val align = noAlignCheck || _align
  val crossWordInsideBank = needAlignCheck && _crossWordInsideBank
  val crossBank = needAlignCheck && _crossBank
  val readWholeBank = unalignTail.valid || crossWordInsideBank

  sink.bits.align.get := align
  sink.bits.unalignHead.get := crossBank
  sink.bits.readWholeBank.get := readWholeBank

  def alignCheck(bankOffset: UInt, size: UInt): (Bool, Bool, Bool) = {
    require(bankOffset.getWidth == DCacheVWordOffset)
    require(size.getWidth == MemorySize.Size.width)
    // 1.1 Align check
    val align = LookupTree(size, List( // TODO: parameterize this
      "b00".U -> true.B,
      "b01".U -> (bankOffset.take(1) === 0.U),
      "b10".U -> (bankOffset.take(2) === 0.U),
      "b11".U -> (bankOffset.take(3) === 0.U)
    ))
    assert(size =/= MemorySize.Q.U || bankOffset === 0.U)
    // 1.2 Bank bound check
    // 1.3 Word bound check
    val upBoundBankOffset = LookupTree(size, List(
      MemorySize.B.U -> 0.U,
      MemorySize.H.U -> 1.U,
      MemorySize.W.U -> 3.U,
      MemorySize.D.U -> 7.U,
      MemorySize.Q.U -> 15.U
    )) +& bankOffset
    val wordIdx = bankOffset(DCacheWordOffset)
    val upBoundWordIdx = upBoundBankOffset(DCacheWordOffset)
    val crossBank = upBoundBankOffset.head(1).asBool
    val crossWordInsideBank = !crossBank && wordIdx === 0.U && upBoundWordIdx =/= 0.U
    (align, crossWordInsideBank, crossBank)
  }

  /**
    * DCache access
    * 
    * Access to an aligned 16B bank is required in the following 2 cases:
    * 1. Unalign tail: For simplicity, we do not calculate the exact # of bytes that an unalignTail needs to access,
    *   but directly access the entire bank
    * 2. Access that is not align, but inside a 16B bank
    * 3. Vector unit-stride
    */
  val dcacheReqValid = sink.valid // all sources need to access DCache
  val dcacheVAddr = sink.bits.vaddr
  val noDCacheAccessSwInstrPrefetch = isSwInstrPrefetch // software instruction prefetch
  val noDCacheAccessUncacheReplay = isUncacheReplay // uncache replay
  val noDCacheAccess = noDCacheAccessSwInstrPrefetch || noDCacheAccessUncacheReplay

  /**
    * Data forward
    */
  val storeForwardReq = Wire(new StoreForwardReqS0)
  storeForwardReq.vaddr := sink.bits.vaddr
  storeForwardReq.sqIdx := uop.sqIdx
  storeForwardReq.size := sink.bits.size
  storeForwardReq.loadWaitBit := uop.loadWaitBit
  storeForwardReq.loadWaitStrict := uop.loadWaitStrict
  storeForwardReq.ssid := uop.ssid
  storeForwardReq.storeSetHit := uop.storeSetHit
  storeForwardReq.waitForRobIdx := uop.waitForRobIdx

  val uncacheForwardReqValid = replayHiPrio.fire && replayHiPrio.bits.isUncacheReplay()

  val dcacheForwardReqValid = replayHiPrio.fire && replayHiPrio.bits.forwardDChannel.get
  val dcacheForwardReq = Wire(new DCacheForwardReqS0)
  dcacheForwardReq.vaddr := sink.bits.vaddr
  dcacheForwardReq.size := sink.bits.size
  dcacheForwardReq.mshrId := sink.bits.mshrId.get

  val uncacheBypassReqValid = uncacheForwardReqValid
  val uncacheBypassReq = Wire(new UncacheBypassReqS0)
  uncacheBypassReq.lqIdx := uop.lqIdx
  uncacheBypassReq.isNCReplay := replayHiPrio.bits.isNCReplay()
  uncacheBypassReq.isMMIOReplay := replayHiPrio.bits.isMMIOReplay()

  /**
    * IQ wakeup
    */
  // Select between 2 options based on timing result:
  // Option 1
  val needWakeupSources = Seq(unalignTail, replayHiPrio, fastReplay, replayLoPrio, scalarIssue)
  val needWakeupValids = needWakeupSources.map(s => s.fire && s.bits.accessType.isScalar()) // exclude vector and prefetch
  // Option 2
  // val needWakeupSources = sources
  // val needWakeupValids = needWakeupSources.map(s => s.valid && sink.ready && s.bits.accessType.isScalar())
  val wakeupValid = Cat(needWakeupValids).orR
  val wakeupSource = ParallelPriorityMux(needWakeupValids, needWakeupSources.map(_.bits))
  val wakeup = Wire(new MemWakeUpBundle)
  connectSamePort(wakeup, wakeupSource.uop)

  /**
    * Pipeline connect
    */
  val pipeOutValid = RegInit(false.B)
  val pipeOutBits = RegEnable(pipeIn.bits, pipeIn.fire)
  when (pipeIn.fire) { pipeOutValid := true.B }
  .elsewhen (io_pipeOut.get.fire) { pipeOutValid := false.B }
  pipeIn.ready := !pipeOutValid || io_pipeOut.get.ready

  /**
    * IO assignment
    */
  io_pipeOut.get.valid := pipeOutValid
  io_pipeOut.get.bits := pipeOutBits

  assert(!sink.ready || unalignTail.ready, "unalignTail should always be ready")
  io.replay.ready := Mux(replayIsHiPrio, replayHiPrio.ready, replayLoPrio.ready)
  io.fastReplay.ready := fastReplay.ready
  io.prefetchReq.ready := Mux(prefetchIsHiConf, prefetchHiConf.ready, prefetchLoConf.ready)
  io.vecldin.ready := vectorIssue.ready
  io.ldin.ready := scalarIssue.ready

  io.tlbReq.valid := tlbReqValid
  io.tlbReq.bits.vaddr := tlbVAddr
  io.tlbReq.bits.fullva := tlbFullva
  io.tlbReq.bits.checkfullva := tlbCheckFullva
  io.tlbReq.bits.cmd := TlbCmd.read
  io.tlbReq.bits.hyperinst := tlbHlv
  io.tlbReq.bits.hlvx := tlbHlvx
  io.tlbReq.bits.size := DontCare
  io.tlbReq.bits.kill := noQuery
  io.tlbReq.bits.memidx.is_ld := true.B
  io.tlbReq.bits.memidx.is_st := false.B
  io.tlbReq.bits.memidx.idx := uop.lqIdx.value
  io.tlbReq.bits.isPrefetch := isPrefetch
  io.tlbReq.bits.no_translate := noQuery
  io.tlbReq.bits.pmp_addr := DontCare // TODO: move this outside of TlbReq
  io.tlbReq.bits.debug.pc := uop.pc
  io.tlbReq.bits.debug.robIdx := uop.robIdx
  io.tlbReq.bits.debug.isFirstIssue := firstIssue

  io.dcacheReq.valid := dcacheReqValid && !noDCacheAccess
  io.dcacheReq.bits.cmd := Mux(isPrefetch, MemoryOpConstants.M_PFR, MemoryOpConstants.M_XRD)
  io.dcacheReq.bits.vaddr := dcacheVAddr
  io.dcacheReq.bits.vaddr_dup := dcacheVAddr
  io.dcacheReq.bits.data := DontCare
  io.dcacheReq.bits.mask := DontCare
  io.dcacheReq.bits.id := DontCare
  io.dcacheReq.bits.instrtype := Mux(isPrefetch, DCACHE_PREFETCH_SOURCE.U, LOAD_SOURCE.U)
  io.dcacheReq.bits.isFirstIssue := firstIssue
  io.dcacheReq.bits.replayCarry := DontCare
  io.dcacheReq.bits.lqIdx := uop.lqIdx
  io.dcacheReq.bits.debug_robIdx := uop.robIdx.value
  io.is128Req := readWholeBank
  io.replacementUpdated := DontCare
  io.pfSource := Mux(isHwPrefetch, io.prefetchReq.bits.pf_source.value, L1_HW_PREFETCH_NULL)

  io.sqSbForwardReq.valid := sink.valid
  io.sqSbForwardReq.bits := storeForwardReq

  io.uncacheForwardReq.valid := uncacheForwardReqValid
  io.uncacheForwardReq.bits := storeForwardReq

  io.mshrForwardReq.valid := dcacheForwardReqValid
  io.mshrForwardReq.bits := dcacheForwardReq
  io.tldForwardReq.valid := dcacheForwardReqValid
  io.tldForwardReq.bits := dcacheForwardReq

  io.uncacheBypassReq.valid := uncacheBypassReqValid
  io.uncacheBypassReq.bits := uncacheBypassReq

  io.wakeup.valid := wakeupValid
  io.wakeup.bits := wakeup

  io.debugInfo.pc := uop.pc

}

class LoadUnitS1(param: ExeUnitParams)(
  implicit p: Parameters,
  override implicit val s: LoadStage = LoadS1()
) extends LoadUnitStage(param)
  with HasVLSUParameters 
  with HasNukePAddrMatch {
  val io = IO(new Bundle() {
    val redirect = Flipped(ValidIO(new Redirect))
    val kill = Input(Bool())

    // Tlb response
    val tlbResp = Flipped(DecoupledIO(new TlbResp(2))) // TODO: parameterize 2
    val tlbReqKill = Output(Bool())
    val tlbPAddr = Output(UInt(PAddrBits.W)) // only used for no_translate

    // DCache request: paddr and s1 kill signal
    val dcachePAddr = Output(UInt(PAddrBits.W))
    val dcacheKill = Output(Bool())

    /**
      * Data forward request and kill
      */
    val storeForwardReq = Output(new StoreForwardReqS1) // including SQ, Sbuffer and Uncache
    val mshrForwardReq = Output(new DCacheForwardReqS1)
    val tldForwardReq = Output(new DCacheForwardReqS1)
    val sqForwardKill = Output(Bool())
    val sbufferForwardKill = Output(Bool())
    val uncacheForwardKill = Output(Bool())
    val mshrForwardKill = Output(Bool())
    val tldForwardKill = Output(Bool())
    // early reponse from SQ, unused for now
    val sqForwardResp = Flipped(ValidIO(new SQForwardRespS1))
    
    val uncacheBypassResp = Flipped(ValidIO(new UncacheBypassRespS1))

    // Data path
    val dataPathMeta = ValidIO(new LoadUnitDataPathMeta)

    // Unalign tail inject to s0
    val unalignTail = DecoupledIO(new LoadStageIO()(p, prevStage(s)))

    // Nuke check with StoreUnit
    val staNukeQueryReq = Flipped(Vec(StorePipelineWidth, ValidIO(new StoreNukeQueryReq)))

    // Software instruction prefetch
    val swInstrPrefetch = ValidIO(new SoftIfetchPrefetchBundle)

    // Load trigger
    val csrTrigger = Input(new CsrTriggerBundle)

    // Debug info
    val debugInfo = Output(new Bundle() {
      val isTlbFirstMiss = Bool()
      val isLoadToLoadForward = Bool()
      val robIdx = UInt(log2Ceil(RobSize).W)
      val vaddr = ValidIO(UInt(VAddrBits.W))
      val pc = Output(UInt(VAddrBits.W))
    })
  })

  val pipeIn = io_pipeIn.get
  val pipeOut = io_pipeOut.get
  val in = pipeIn.bits

  val entrance = in.entrance
  val accessType = in.accessType
  val uop = in.uop
  val robIdx = uop.robIdx
  val fuOpType = uop.fuOpType
  val mop = fuOpType(6, 5) // ?
  val vaddr = in.vaddr
  val mask = in.mask

  val isSwInstrPrefetch = accessType.isSwPrefetch() && accessType.isInstrPrefetch()

  /**
    * Redirect
    */
  val redirect = io.redirect
  val redirectNext = Wire(redirect.cloneType)
  redirectNext.valid := GatedValidRegNext(redirect.valid)
  redirectNext.bits := RegEnable(redirect.bits, redirect.valid)
  val kill = io.kill || isSwInstrPrefetch || robIdx.needFlush(redirect) || robIdx.needFlush(redirectNext)

  /**
    * Tlb & DCache
    */
  val tlbResp = io.tlbResp
  val noQuery = in.noQuery.get
  val tlbHit = tlbResp.valid && !tlbResp.bits.miss && !noQuery
  val tlbMiss = tlbResp.valid && tlbResp.bits.miss
  val paddrEffective = tlbHit || noQuery // hit or noQuery
  val pbmt = Mux(tlbHit, tlbResp.bits.pbmt.head, Pbmt.pma)
  val noQueryPAddr = Mux(io.uncacheBypassResp.valid, io.uncacheBypassResp.bits.paddr, in.paddr.get)
  val paddr = Mux(noQuery, noQueryPAddr, tlbResp.bits.paddr(0))
  val paddrDCache = Mux(noQuery, noQueryPAddr, tlbResp.bits.paddr(1))
  val gpaddr = tlbResp.bits.gpaddr(0)
  val fullva = tlbResp.bits.fullva

  val pf = tlbHit && tlbResp.bits.excp.head.pf.ld
  val af = tlbHit && tlbResp.bits.excp.head.af.ld
  val gpf = tlbHit && tlbResp.bits.excp.head.gpf.ld
  val exception = pf || af || gpf

  val killDCache = kill || tlbMiss || exception

  assert(!(pipeIn.valid && !tlbResp.valid && !noQuery))

  /**
    * Load trigger
    */
  val loadTrigger = Module(new MemTrigger(MemType.LOAD))
  loadTrigger.io.fromCsrTrigger.tdataVec := io.csrTrigger.tdataVec
  loadTrigger.io.fromCsrTrigger.tEnableVec := io.csrTrigger.tEnableVec
  loadTrigger.io.fromCsrTrigger.triggerCanRaiseBpExp := io.csrTrigger.triggerCanRaiseBpExp
  loadTrigger.io.fromCsrTrigger.debugMode := io.csrTrigger.debugMode
  loadTrigger.io.fromLoadStore.vaddr := vaddr
  loadTrigger.io.fromLoadStore.isVectorUnitStride := accessType.isVector() && isUnitStride(mop)
  loadTrigger.io.fromLoadStore.mask := mask
  loadTrigger.io.isPrf.get := accessType.isPrefetch()

  val triggerAction = loadTrigger.io.toLoadStore.triggerAction
  val isDebugMode = TriggerAction.isDmode(triggerAction)
  val bp = TriggerAction.isExp(triggerAction)
  val vecVaddrOffset = Mux(
    isDebugMode || bp,
    loadTrigger.io.toLoadStore.triggerVaddr - in.vecBaseVaddr.get,
    vaddr + genVFirstUnmask(mask).asUInt - in.vecBaseVaddr.get
  )
  val vecTriggerMask = Mux(
    isDebugMode || bp,
    loadTrigger.io.toLoadStore.triggerMask,
    0.U
  )

  /**
    * Unalign tail inject to s0
    */
  val unalignTailInjectValid = pipeIn.valid && in.unalignHead.get
  val unalignTail = Wire(io.unalignTail.bits.cloneType)
  connectSamePort(unalignTail, in)
  unalignTail.entrance := LoadEntrance.unalignTail.U
  unalignTail.vaddr := ((vaddr >> DCacheVWordOffset) + 1.U) << DCacheVWordOffset
  unalignTail.fullva := ((in.fullva >> DCacheVWordOffset) + 1.U) << DCacheVWordOffset
  unalignTail.size := MemorySize.Q.U
  unalignTail.mask := genVWmask(vaddr, LSUOpType.size(fuOpType)) >> DCacheVWordBytes
  unalignTail.align.get := false.B
  unalignTail.unalignHead.get := false.B
  unalignTail.readWholeBank.get := true.B

  /**
    * Nuke check with StoreUnit
    */
  val nukeQueryValids = io.staNukeQueryReq.map(_.valid)
  val nukeQueryReqs = io.staNukeQueryReq.map(_.bits)
  val nukePAddrMatches = nukeQueryReqs.map(req => nukePAddrMatch(req.paddr, req.matchType, paddr))
  val nukeStoreOlders = nukeQueryReqs.map(req => isAfter(robIdx, req.robIdx))
  val nukeMaskMatches = nukeQueryReqs.map(req => (req.mask & in.mask).orR)
  val nuke = Cat((nukeQueryValids lazyZip nukePAddrMatches lazyZip nukeStoreOlders lazyZip nukeMaskMatches).map {
    case (valid, paddrMatch, storeOlder, maskMatch) => valid && paddrMatch && storeOlder && maskMatch
  }).orR && paddrEffective

  /**
    * Pipeline connect
    */
  val pipeOutValid = RegInit(false.B)
  val pipeOutBits = Reg(new LoadStageIO)
  when (kill) { pipeOutValid := false.B }
  .elsewhen (pipeIn.fire) { pipeOutValid := true.B }
  .elsewhen (pipeOut.fire) { pipeOutValid := false.B }

  val stageInfo = Wire(pipeOut.bits.cloneType)
  connectSamePort(stageInfo, in)
  stageInfo.uop.trigger := triggerAction
  stageInfo.uop.exceptionVec(breakPoint) := bp
  stageInfo.uop.exceptionVec(loadPageFault) := pf
  stageInfo.uop.exceptionVec(loadAccessFault) := af
  stageInfo.uop.exceptionVec(loadGuestPageFault) := gpf
  stageInfo.uop.debugInfo.tlbRespTime := Mux(
    pipeIn.valid && paddrEffective,
    GTimer(),
    Mux(pipeIn.valid && tlbMiss, uop.debugInfo.tlbFirstReqTime, uop.debugInfo.tlbRespTime)
  )
  // update tlb response
  stageInfo.fullva := tlbResp.bits.fullva
  stageInfo.paddr.get := paddr
  stageInfo.tlbAccessResult.get := Mux(
    noQuery,
    TlbAccessResult.noQuery.U,
    Mux(tlbHit, TlbAccessResult.hit.U, TlbAccessResult.miss.U)
  )
  stageInfo.tlbException.get := tlbResp.bits.excp.head
  stageInfo.pbmt.get := pbmt
  stageInfo.gpaddr.get := gpaddr
  stageInfo.isForVSnonLeafPTE.get := tlbResp.bits.isForVSnonLeafPTE
  // update replay cause (only nuke is detected in S1)
  stageInfo.cause.get := 0.U.asTypeOf(stageInfo.cause.get)
  stageInfo.cause.get(LoadReplayCauses.C_NK) := nuke
  // update trigger info
  stageInfo.vecVaddrOffset.get := vecVaddrOffset
  stageInfo.vecTriggerMask.get := vecTriggerMask

  when (pipeIn.fire) { pipeOutBits := stageInfo }

  /**
    * IO assignment
    */
  io_pipeOut.get.valid := pipeOutValid
  io_pipeOut.get.bits := pipeOutBits
  io_pipeIn.get.ready := !pipeOutValid || kill || pipeOut.ready
 
  io.tlbResp.ready := true.B
  io.tlbReqKill := kill
  io.tlbPAddr := noQueryPAddr

  io.dcachePAddr := paddrDCache
  io.dcacheKill := killDCache

  // use kill instead of killDCache if timing does not allow it
  io.storeForwardReq.paddr := paddr
  io.mshrForwardReq.paddr := paddr
  io.tldForwardReq.paddr := paddr
  io.sqForwardKill := killDCache
  io.sbufferForwardKill := killDCache
  io.uncacheForwardKill := kill
  io.mshrForwardKill := killDCache
  io.tldForwardKill := killDCache

  io.dataPathMeta.valid := pipeIn.valid
  io.dataPathMeta.bits.bankOffset := paddr.take(DCacheVWordOffset)
  io.dataPathMeta.bits.fuOpType := fuOpType
  io.dataPathMeta.bits.fpWen := uop.fpWen
  io.dataPathMeta.bits.isNCReplay := in.isNCReplay()
  io.dataPathMeta.bits.isMMIOReplay := in.isMMIOReplay()
  io.dataPathMeta.bits.isUnalignHead := in.unalignHead.get

  io.unalignTail.valid := unalignTailInjectValid
  io.unalignTail.bits := unalignTail
  assert(!io.unalignTail.valid || io.unalignTail.ready)

  io.swInstrPrefetch.valid := pipeIn.valid && isSwInstrPrefetch
  io.swInstrPrefetch.bits.vaddr := vaddr

  io.debugInfo.isTlbFirstMiss := pipeIn.valid && tlbMiss && in.isFirstIssue()
  io.debugInfo.isLoadToLoadForward := false.B
  io.debugInfo.robIdx := robIdx.value
  io.debugInfo.vaddr.valid := pipeIn.valid
  io.debugInfo.vaddr.bits := vaddr
  io.debugInfo.pc := uop.pc

  assert(!(pipeIn.valid && in.isUncacheReplay()) || io.uncacheBypassResp.valid, "uncache bypass should always success")
}

class LoadUnitS2(param: ExeUnitParams)(
  implicit p: Parameters,
  override implicit val s: LoadStage = LoadS2()
) extends LoadUnitStage(param)
  with HasNukePAddrMatch {
  val io = IO(new Bundle() {
    val redirect = Flipped(ValidIO(new Redirect))
    val kill = Input(Bool())

    // PMP result
    val pmp = Flipped(new PMPRespBundle)
    // TLB Hint
    val tlbHint = Flipped(new TlbHintReq)

    // DCache request: s2 kill signal
    val dcacheKill = Output(Bool())

    // DCache response
    val dcacheResp = Flipped(DecoupledIO(new DCacheWordResp))
    // TODO: move this inside of dcacheResp
    val dcacheBankConflict = Input(Bool())
    val dcacheMSHRNack = Input(Bool())

    /**
      * Data forward response
      */
    val sqForwardResp = Flipped(ValidIO(new SQForwardRespS2))
    val sbufferForwardResp = Flipped(ValidIO(new SbufferForwardResp))
    val uncacheForwardResp = Flipped(ValidIO(new UncacheForwardResp))
    val mshrForwardResp = Flipped(ValidIO(new DCacheForwardResp))
    val tldForwardResp = Flipped(ValidIO(new DCacheForwardResp))

    val uncacheBypassResp = Flipped(ValidIO(new UncacheBypassRespS2))

    // Nuke query from StoreUnit
    val staNukeQueryReq = Flipped(Vec(StorePipelineWidth, ValidIO(new StoreNukeQueryReq)))
    // Nuke query to LQRAR / LQRAW
    val rarNukeQueryReq = DecoupledIO(new LoadNukeQueryReq)
    val rawNukeQueryReq = DecoupledIO(new LoadNukeQueryReq)

    // Prefetch train
    // TODO: this bundle is tooooooo big, define a smaller one
    val prefetchTrain = ValidIO(new LsPrefetchTrainBundle)

    // CSR control signals
    val csrCtrl = Flipped(new CustomCSRCtrlIO)

    // Debug info
    val debugInfo = Output(new Bundle() {
      val isBankConflict = Bool()
      val isDCacheMiss = Bool()
      val isDCacheFirstMiss = Bool()
      val isForwardFail = Bool()
      val robIdx = UInt(log2Ceil(RobSize).W)
      val paddr = ValidIO(UInt(PAddrBits.W))
      val pc = Output(UInt(VAddrBits.W))
    })
  })

  val pipeIn = io_pipeIn.get
  val pipeOut = io_pipeOut.get
  val in = pipeIn.bits

  val entrance = in.entrance
  val accessType = in.accessType
  val uop = in.uop
  val robIdx = uop.robIdx
  val paddr = in.paddr.get
  val isMMIOReplay = in.isMMIOReplay()
  val isNCReplay = in.isNCReplay()
  val isUncacheReplay = in.isUncacheReplay()
  val isPrefetch = accessType.isPrefetch()
  val isUnalignHead = in.unalignHead.get
  val isUnalignTail = LoadEntrance.isUnalignTail(entrance)
  val isUnalign = isUnalignHead || isUnalignTail

  /**
    * Redirect
    * 
    * Some terminology explanations:
    * Both **kill** and **endPipe** indicate that a request should terminate at the current stage without proceeding to
    * the next pipeline stage. However their difference lies in:
    * - **kill** means the request is canceled due to some reason. A killed request will not produce any side effects
    *   on other modules or micro-arch states, such as not writing back to Backend
    * - **endPipe** means the request is not canceled but completes execution at this stage, without advancing to the
    *   next stage, but it may produce side effects on micro-arch
    */
  val redirect = io.redirect
  val kill = io.kill || robIdx.needFlush(redirect)
  val endPipe = isPrefetch

  /**
    * PMP result & exception handling
    */
  val pmp = io.pmp
  val pbmt = in.pbmt.get
  val tlbAccessResult = in.tlbAccessResult.get
  val tlbHit = TlbAccessResult.isHit(tlbAccessResult)
  val tlbMiss = TlbAccessResult.isMiss(tlbAccessResult)
  val tlbNotMiss = TlbAccessResult.isNotMiss(tlbAccessResult)
  val tlbUnaccessable = uop.exceptionVec(loadAccessFault) ||
    uop.exceptionVec(loadPageFault) ||
    uop.exceptionVec(loadGuestPageFault)
  val tlbAccessable = !tlbUnaccessable
  val pmpUnaccessable = pmp.ld && tlbHit

  val isNC = tlbHit && tlbAccessable && Pbmt.isNC(pbmt)
  val isMMIO = tlbHit && tlbAccessable && (Pbmt.isIO(pbmt) || Pbmt.isPMA(pbmt) && pmp.mmio)
  val isUncache = isNC || isMMIO

  // load access fault
  val afUnaccessable = uop.exceptionVec(loadAccessFault) || pmpUnaccessable
  val afVectorUncache = accessType.isVector() && isUncache
  val afTagError = io.dcacheResp.bits.tag_error && tlbHit && io.csrCtrl.cache_error_enable
  val afForwardDenied = Wire(Bool())
  val afBypassDenied = Wire(Bool())
  val af = afUnaccessable || afVectorUncache || afTagError || afForwardDenied || afBypassDenied
  // load address misaligned
  val am = !in.align.get && accessType.isScalar() && isUncache && !pmpUnaccessable
  // hardware error
  val hweForwardCorrupt = Wire(Bool())
  val hweBypassCorrupt = Wire(Bool())
  val hwe = uop.exceptionVec(hardwareError) || hweForwardCorrupt || hweBypassCorrupt

  val exceptionVec = WireInit(uop.exceptionVec)
  val exception = TriggerAction.isDmode(uop.trigger) || ExceptionNO.selectByFu(exceptionVec, LduCfg).asUInt.orR
  exceptionVec(loadAddrMisaligned) := am
  exceptionVec(loadAccessFault) := af
  exceptionVec(hardwareError) := hwe

  /**
    * Data forward response
    */
  // SQ / Sbuffer / Uncache forward
  val storeForwardMask = io.sqForwardResp.bits.forwardMask.asUInt |
    io.sbufferForwardResp.bits.forwardMask.asUInt |
    io.uncacheForwardResp.bits.forwardMask.asUInt

  val sqAddrInvalid = io.sqForwardResp.valid && io.sqForwardResp.bits.addrInvalid.valid
  val sqAddrInvalidSqIdx = io.sqForwardResp.bits.addrInvalid.bits
  val sqDataInvalid = io.sqForwardResp.valid && io.sqForwardResp.bits.dataInvalid.valid
  val sqDataInvalidSqIdx = io.sqForwardResp.bits.dataInvalid.bits

  val matchInvalid = io.sqForwardResp.valid && io.sqForwardResp.bits.matchInvalid ||
    io.sbufferForwardResp.valid && io.sbufferForwardResp.bits.matchInvalid ||
    io.uncacheForwardResp.valid && io.uncacheForwardResp.bits.matchInvalid

  // MSHR / TileLink-D channel
  val mshrForwardDenied = io.mshrForwardResp.valid && io.mshrForwardResp.bits.denied
  val tldForwardDenied = io.tldForwardResp.valid && io.tldForwardResp.bits.denied
  val mshrForwardCorrupt = io.mshrForwardResp.valid && io.mshrForwardResp.bits.corrupt && !io.mshrForwardResp.bits.denied
  val tldForwardCorrupt = io.tldForwardResp.valid && io.tldForwardResp.bits.corrupt && !io.tldForwardResp.bits.denied
  afForwardDenied := mshrForwardDenied || tldForwardDenied
  hweForwardCorrupt := mshrForwardCorrupt || tldForwardCorrupt

  val dcacheFullForward = io.mshrForwardResp.valid || io.tldForwardResp.valid
  val storeFullForward = (~storeForwardMask & in.mask) === 0.U && !sqDataInvalid
  val needDCacheAccess = !dcacheFullForward && !storeFullForward && !isUncache

  // Uncache bypass
  afBypassDenied := io.uncacheBypassResp.valid && io.uncacheBypassResp.bits.denied
  hweBypassCorrupt := io.uncacheBypassResp.valid && io.uncacheBypassResp.bits.corrupt && !io.uncacheBypassResp.bits.denied

  /**
    * DCache early response
    */
  val dcacheMiss = io.dcacheResp.bits.miss
  val mshrNack = io.dcacheMSHRNack
  val bankConflict = io.dcacheBankConflict

  /**
    * Nuke query from StoreUnit
    */
  val prevStageNuke = in.cause.get(C_NK)
  val nukeQueryValids = io.staNukeQueryReq.map(_.valid)
  val nukeQueryReqs = io.staNukeQueryReq.map(_.bits)
  val nukePAddrMatches = nukeQueryReqs.map(req => nukePAddrMatch(req.paddr, req.matchType, paddr))
  val nukeStoreOlders = nukeQueryReqs.map(req => isAfter(robIdx, req.robIdx))
  val nukeMaskMatches = nukeQueryReqs.map(req => (req.mask & in.mask).orR)
  val nuke = Cat((nukeQueryValids lazyZip nukePAddrMatches lazyZip nukeStoreOlders lazyZip nukeMaskMatches).map {
    case (valid, paddrMatch, storeOlder, maskMatch) => valid && paddrMatch && storeOlder && maskMatch
  }).orR && tlbNotMiss || prevStageNuke

  /**
    * Preliminary assessment of the load exit
    * 
    * We categorize the request exit into one of the following 3 categories:
    * 1. Trouble maker: loads that may need replay and may require RAR / RAW violation check, including exits:
    *   1.1 writeback: no need to replay or fast replay, but need to do violation check
    *   1.2 replay: no need to do violation check (or revoke later)
    *   1.3 fast replay: no need to do violation check (or revoke later). It should be noted that requests marked as
    *     `fastReplay` here are not guaranteed to undergo fast replay. They may fail arbitration at s0, in which case
    *     such requests will enter LRQ
    *     1.3.1 loads that have already fast replay should not fast replay again
    *     1.3.2 loads that are bank conflict, or mshr nacked, or nuked may fast replay, except there are other higher-
    *       priority replay causes
    *     1.3.3 loads that are unaligned should not fast replay for simplicity
    * 2. Always writeback: loads that definitely do not require replay or RAR / RAW violation check, and can be directly
    *   written back to Backend, including:
    *   - exception
    *   - MMIO replay
    * 3. Prefetch: hardware or software prefetch requests, which do not require replay or violation check, but never
    *   write back to Backend
    */
  // 2. Always writeback
  val alwaysWriteback = exception || isMMIOReplay
  // 1. Trouble maker
  val troubleMaker = !isPrefetch && !alwaysWriteback
  // 1.3 fast replay
  val cause = Wire(in.cause.get.cloneType)
  val fastReplayMSHRNack = cause(C_DR) && !hasHigherPriorityCauses(cause, C_DR)
  val fastReplayBankConflict = cause(C_BC) && !hasHigherPriorityCauses(cause, C_BC)
  val fastReplayNuke = cause(C_NK) && !hasHigherPriorityCauses(cause, C_RAR) // TODO: use C_RAR or C_NK?
  val fastReplay = !LoadEntrance.isFastReplay(entrance) && // 1.3.1
    (fastReplayMSHRNack || fastReplayBankConflict || fastReplayNuke) && // 1.3.2
    !isUnalign // 1.3.3

  /**
    * Nuke query to LQRAR / LQRAW
    * 
    * For timing considerations, violation check requests issued in s2 do not need to be accurate. But MUST ensure that
    * accurate `revoke` signals are given in s3 to withdraw requests that do not require violation check.
    */
  val nukeQueryReqValid = troubleMaker && !(prevStageNuke || cause(C_BC))
  val nukeQueryReq = Wire(new LoadNukeQueryReq)
  nukeQueryReq.robIdx := robIdx
  nukeQueryReq.paddr := paddr
  nukeQueryReq.lqIdx := uop.lqIdx
  nukeQueryReq.sqIdx := uop.sqIdx
  nukeQueryReq.nc := isNCReplay
  nukeQueryReq.mask := in.mask
  nukeQueryReq.isRVC := uop.isRVC
  nukeQueryReq.ftqPtr := uop.ftqPtr
  nukeQueryReq.ftqOffset := uop.ftqOffset

  /**
    * Load replay
    */
  val shouldReplay = cause.asUInt.orR // including fast replay
  cause(C_MA) := troubleMaker && uop.storeSetHit && sqAddrInvalid
  cause(C_TM) := troubleMaker && tlbMiss
  cause(C_FF) := troubleMaker && sqDataInvalid
  cause(C_DR) := troubleMaker && needDCacheAccess && mshrNack
  cause(C_DM) := troubleMaker && needDCacheAccess && dcacheMiss
  cause(C_WF) := false.B
  cause(C_BC) := troubleMaker && needDCacheAccess && bankConflict
  cause(C_RAR) := troubleMaker && io.rarNukeQueryReq.valid && !io.rarNukeQueryReq.ready
  cause(C_RAW) := troubleMaker && io.rawNukeQueryReq.valid && !io.rawNukeQueryReq.ready
  cause(C_NK) := troubleMaker && nuke
  cause(C_MF) := false.B

  def hasHigherPriorityCauses(cause: Vec[Bool], index: Int): Bool = {
    if (index == 0) false.B
    else Cat(cause.take(index)).orR
  }

  /**
    * Writeback and wakeup
    *
    * For timing considerations, the control signals required for writeback and wakeup in the S3 stage are pre-computed
    * here. Both signals are asserted only when there is no need for replay and no need to access the uncache path, i.e.
    * when load completed execution. The differences between them are as follows:
    * 1. `wakeup` should not asserted when exception occurs
    * 2. `wakeup` should not asserted in case of a vaddr / paddr mismatch (debatable yet)
    */
  val shouldWakeup = !shouldReplay && !isUncache && !exception
  val shouldWriteback = shouldWakeup || exception || matchInvalid

  /**
    * Pipeline connect
    */
  val pipeOutValid = RegInit(false.B)
  val pipeOutBits = Reg(new LoadStageIO) // TODO
  when (kill || endPipe) { pipeOutValid := false.B }
  .elsewhen (pipeIn.fire) { pipeOutValid := true.B }
  .elsewhen (pipeOut.fire) { pipeOutValid := false.B }

  val stageInfo = Wire(pipeOut.bits.cloneType)
  connectSamePort(stageInfo, in)
  stageInfo.uop.flushPipe := false.B
  stageInfo.uop.exceptionVec := exceptionVec
  stageInfo.uop.vpu.vstart := Mux(
    LoadEntrance.isReplay(entrance) || LoadEntrance.isFastReplay(entrance),
    uop.vpu.vstart,
    in.vecVaddrOffset.get >> uop.vpu.veew
  )
  stageInfo.pmp.get := pmp
  stageInfo.nc.get := isNC
  stageInfo.mmio.get := isMMIO
  stageInfo.mshrId.get := io.dcacheResp.bits.mshr_id
  stageInfo.cause.get := cause
  stageInfo.handledByMSHR.get := io.dcacheResp.bits.handled
  stageInfo.dataInvalidSqIdx.get := sqDataInvalidSqIdx
  stageInfo.addrInvalidSqIdx.get := sqAddrInvalidSqIdx
  stageInfo.tlbId.get := io.tlbHint.id
  stageInfo.tlbFull.get := io.tlbHint.full
  // Pre-process for s3
  stageInfo.troubleMaker.get := troubleMaker
  stageInfo.shouldFastReplay.get := fastReplay
  stageInfo.matchInvalid.get := matchInvalid && troubleMaker
  stageInfo.shouldWakeup.get := shouldWakeup
  stageInfo.shouldWriteback.get := shouldWriteback

  when (pipeIn.fire) { pipeOutBits := stageInfo }

  /**
    * IO assignment
    */
  io_pipeOut.get.valid := pipeOutValid
  io_pipeOut.get.bits := pipeOutBits
  io_pipeIn.get.ready := !pipeOutValid || kill || endPipe || pipeOut.ready

  io.dcacheKill := kill || exception || isUncache
  io.dcacheResp.ready := true.B

  io.rarNukeQueryReq.valid := nukeQueryReqValid && pipeIn.valid
  io.rarNukeQueryReq.bits := nukeQueryReq
  io.rawNukeQueryReq.valid := nukeQueryReqValid && pipeIn.valid
  io.rawNukeQueryReq.bits := nukeQueryReq

  io.prefetchTrain.valid := pipeIn.valid && !exception && !isUncache && in.isFirstIssue()
  io.prefetchTrain.bits := DontCare
  io.prefetchTrain.bits.uop := uop
  io.prefetchTrain.bits.vaddr := in.vaddr
  io.prefetchTrain.bits.paddr := paddr
  io.prefetchTrain.bits.miss := io.dcacheResp.bits.miss
  io.prefetchTrain.bits.isFirstIssue := in.isFirstIssue()
  io.prefetchTrain.bits.meta_prefetch := io.dcacheResp.bits.meta_prefetch
  io.prefetchTrain.bits.meta_access := io.dcacheResp.bits.meta_access
  io.prefetchTrain.bits.is_from_hw_pf := accessType.isHwPrefetch()
  io.prefetchTrain.bits.refillLatency := io.dcacheResp.bits.refill_latency

  io.debugInfo.isBankConflict := pipeIn.valid && !kill && cause(C_BC)
  io.debugInfo.isDCacheMiss := pipeIn.valid && !kill && cause(C_DM)
  io.debugInfo.isDCacheFirstMiss := pipeIn.valid && !kill && cause(C_DM) && in.isFirstIssue()
  io.debugInfo.isForwardFail := pipeIn.valid && !kill && cause(C_FF)
  io.debugInfo.robIdx := robIdx.value
  io.debugInfo.paddr.valid := pipeIn.valid
  io.debugInfo.paddr.bits := paddr
  io.debugInfo.pc := uop.pc
}

class LoadUnitS3(param: ExeUnitParams)(
  implicit p: Parameters,
  override implicit val s: LoadStage = LoadS3()
) extends LoadUnitStage(param) {
  val io = IO(new Bundle() {
    val redirect = Flipped(ValidIO(new Redirect))
    val kill = Input(Bool())

    // DCache response
    val dcacheError = Input(Bool())

    // Unalign head from S4
    val unalignConcat = Flipped(ValidIO(new LoadStageIO))

    // Writeback to Backend / LQ / VLMergeBuffer
    val ldout = DecoupledIO(new ExuOutput(param))
    val lqWrite = DecoupledIO(new LqWriteBundle)
    val vecldout = Decoupled(new VecPipelineFeedbackIO(isVStore = false))

    // Fast replay
    val fastReplay = DecoupledIO(new FastReplayIO)

    // RAR / RAW revoke and RAR response
    val rarNukeQueryResp = Flipped(ValidIO(new LoadNukeQueryResp))
    val revokeLastCycle, revokeLastLastCycle = Output(Bool())

    /**
      * Rollback and re-fetch from IFU, including:
      * 1. RAR violation
      * 2. vaddr-paddr mismatch happens in STLF
      */
    val rollback = ValidIO(new Redirect)

    // Exception info
    val exceptionInfo = ValidIO(new MemExceptionInfo)

    // Load cancel
    val cancel = Output(Bool())

    // Uncache data
    // TODO
    val uncacheData = Input(new LoadDataFromLQBundle)

    // CSR control signals
    val csrCtrl = Flipped(new CustomCSRCtrlIO)

    // Debug info
    val debugInfo = Output(new Bundle() {
      val isReplayFast = Bool()
      val isReplaySlow = Bool()
      val isReplayRS = Bool()
      val isReplay = Bool()
      val replayCause = Vec(LoadReplayCauses.allCauses, Bool())
      val replayCnt = UInt(XLEN.W)
      val robIdx = UInt(log2Ceil(RobSize).W)
    })
  })

  val pipeIn = io_pipeIn.get
  val pipeOut = io_pipeOut.get
  val in = pipeIn.bits

  val entrance = in.entrance
  val accessType = in.accessType
  val uop = in.uop
  val robIdx = uop.robIdx
  val isScalar = accessType.isScalar()
  val isVector = accessType.isVector()
  val isUnalignHead = in.unalignHead.get
  val isUnalignTail = LoadEntrance.isUnalignTail(entrance)
  val troubleMaker = in.troubleMaker.get
  val cause = in.cause.get
  val shouldReplay = cause.asUInt.orR

  assert(!pipeIn.valid || !accessType.isPrefetch(), "Prefetch should be killed in S2")
  assert(!io.ldout.valid || io.ldout.ready, "Writeback to Backend should always be ready")
  assert(!io.vecldout.valid || io.vecldout.ready, "Writeback to VLMergeBuffer should always be ready")

  /**
    * Redirect
    */
  val redirect = io.redirect
  val kill = io.kill || robIdx.needFlush(redirect)
  val endPipe = !isUnalignHead // unalign head will flow to next stage

  /**
    * Unalign concatenation
    *
    * We divide the `shouldWriteback` into two scenarios:
    * 1. **s4HeadAlwaysWriteback**: Head does not need to consider tail – it writes back directly, which applies when
    *   the head is an exception or a matchInvalid case
    * 2. **s4HeadWritebackDependOnTail** Head needs to consider tail – it decides whether to write back, replay, or take
    *   other actions based on the tail. This applies when the head can be written back normally (i.e., no exception or
    *   matchInvalid case)
    */
  val s4HeadValid = io.unalignConcat.valid
  val s4Head = io.unalignConcat.bits
  val s4HeadExceptionVec = s4Head.uop.exceptionVec
  val s4HeadReplayCause = s4Head.cause.get
  val s4HeadMatchInvalid = s4Head.matchInvalid.get
  val s4HeadShouldWakeup = s4Head.shouldWakeup.get
  val s4HeadAlwaysWriteback = s4Head.headAlwaysWriteback.get
  val s4HeadWritebackDependOnTail = s4Head.writebackDependOnTail.get
  val s4HeadHasException = s4Head.hasException.get
  val s4HeadShouldReplay = s4HeadReplayCause.asUInt.orR
  val s4HeadShouldRARViolation = s4Head.shouldRarViolation.get

  /**
    * DCache error handling & exception handling
    * 
    * Noted that exception can affect control signals for wakeup and writeback
    */
  val dcacheError = EnableAccurateLoadError.B && io.csrCtrl.cache_error_enable && troubleMaker && io.dcacheError
  val s3ExceptionVec = WireInit(uop.exceptionVec)
  val s3Exception = ExceptionNO.selectByFu(s3ExceptionVec, LduCfg).asUInt.orR || TriggerAction.isDmode(uop.trigger)
  val exceptionVec = Mux(
    s4HeadValid && s4HeadHasException,
    s4HeadExceptionVec,
    s3ExceptionVec
  )
  val exception = s4HeadValid && s4HeadHasException || s3Exception
  val exceptionFullva = Mux(s4HeadValid && s4HeadHasException, s4Head.fullva, in.fullva)
  val exceptionGpaddr = Mux(s4HeadValid && s4HeadHasException, s4Head.gpaddr.get, in.gpaddr.get)
  val exceptionIsForVSnonLeafPTE = Mux(
    s4HeadValid && s4HeadHasException,
    s4Head.isForVSnonLeafPTE.get,
    in.isForVSnonLeafPTE.get
  )
  val exceptionVaNeedExt = Mux(
    s4HeadValid && s4HeadHasException,
    s4Head.tlbException.get.vaNeedExt,
    in.tlbException.get.vaNeedExt
  )

  val s3ShouldWakeup = in.shouldWakeup.get && !dcacheError
  val s3ShouldWriteback = in.shouldWriteback.get || dcacheError
  val shouldWakeup = s3ShouldWakeup && (!s4HeadValid || s4HeadShouldWakeup)
  val shouldWriteback = Mux(
    s4HeadValid,
    s4HeadAlwaysWriteback || s4HeadWritebackDependOnTail && s3ShouldWriteback,
    s3ShouldWriteback
  )

  s3ExceptionVec(hardwareError) := uop.exceptionVec(hardwareError) || dcacheError

  /**
    * Fast replay
    */
  val shouldFastReplay = in.shouldFastReplay.get
  val allowFastReplay = io.fastReplay.ready
  val doFastReplay = shouldFastReplay && allowFastReplay
  val fastReplay = Wire(new FastReplayIO)
  connectSamePort(fastReplay, in)
  fastReplay.cause.get := 0.U.asTypeOf(fastReplay.cause.get)

  /**
    * RAR / RAW revoke
    */
  val s3RevokeException = s3Exception
  val s3RevokeReplay = in.cause.get.asUInt.orR
  val s3Revoke = s3RevokeException || s3RevokeReplay
  val s4HeadRevoke = s4HeadHasException || s4HeadShouldReplay
  val revokeLastCycle = s3Revoke || s4HeadValid && s4HeadRevoke
  val revokeLastLastCycle = s4HeadValid && !s4HeadRevoke && s3Revoke

  /**
    * Pipeline flush
    * - RAR violation
    * - vaddr / paddr mismatch in STLF
    */
  // RAR violation
  val rarResp = io.rarNukeQueryResp
  val s3RarViolation = rarResp.valid && rarResp.bits.nuke && io.csrCtrl.ldld_vio_check_enable && !s3Exception
  val rarViolation = s3RarViolation || s4HeadValid && s4HeadShouldRARViolation
  // vaddr / paddr mismatch in STLF
  val s3MatchInvalid = in.matchInvalid.get && !s3Exception
  val matchInvalid = s3MatchInvalid || s4HeadValid && s4HeadMatchInvalid

  val rollbackValid = pipeIn.valid && (rarViolation || matchInvalid) && endPipe
  val rollbackLevel = Mux(matchInvalid, RedirectLevel.flush, RedirectLevel.flushAfter)

  /**
    * Load cancel
    */
  // For unaligned head (endPipe = 0), always cancel, because the unaligned head needs to be combined with tail.
  val cancel = pipeIn.valid && (!endPipe || !shouldWakeup && isScalar)

  /**
    * Writeback to Backend / LQ / VLMergeBuffer
    */
  // Writeback to Backend
  val ldoutValid = pipeIn.valid && shouldWriteback && isScalar && endPipe
  val ldout = Wire(new ExuOutput(param))
  ldout.data := DontCare // assign data from LoadUnitDataPath
  ldout.pdest := uop.pdest
  ldout.robIdx := uop.robIdx
  ldout.intWen.get := uop.rfWen
  ldout.fpWen.get := uop.fpWen
  ldout.exceptionVec.get := exceptionVec
  ldout.flushPipe.get := uop.flushPipe
  ldout.replay.get := uop.replayInst
  ldout.lqIdx.get := uop.lqIdx
  ldout.trigger.get := uop.trigger
  ldout.isRVC.get := uop.isRVC
  ldout.isFromLoadUnit.get := true.B
  ldout.debug.isMMIO := in.isMMIOReplay()
  ldout.debug.isNCIO := in.isNCReplay() && in.pmp.get.mmio
  ldout.debug.isPerfCnt := false.B
  ldout.debug.paddr := in.paddr.get
  ldout.debug.vaddr := in.vaddr
  ldout.debugInfo := uop.debugInfo
  ldout.debug_seqNum := uop.debug_seqNum

  // Writeback to LQ
  val lqWriteValid = pipeIn.valid && !doFastReplay && endPipe
  val lqWriteReady = io.lqWrite.ready
  val lqWriteCause = Mux(s4HeadValid && s4HeadShouldReplay, s4HeadReplayCause, in.cause.get)
  val lqWriteCauseOH = PriorityEncoderOH(lqWriteCause)
  val lqWrite = Wire(new LqWriteBundle)
  // TODO: remove useless fields after old LoadUnit is removed
  lqWrite.uop := uop
  lqWrite.uop.exceptionVec := exceptionVec
  lqWrite.vaddr := in.vaddr
  lqWrite.fullva := exceptionFullva
  lqWrite.vaNeedExt := exceptionVaNeedExt
  lqWrite.paddr := in.paddr.get
  lqWrite.gpaddr := exceptionGpaddr
  lqWrite.mask := in.mask
  lqWrite.data := DontCare // TODO: remove this
  lqWrite.wlineflag := false.B // TODO: remove this
  lqWrite.miss := cause(C_DM) // TODO: remove this
  lqWrite.tlbMiss := TlbAccessResult.isMiss(in.tlbAccessResult.get)// TODO: remove this
  lqWrite.ptwBack := false.B // TODO: remove this
  lqWrite.af := exceptionVec(loadAccessFault) // TODO: remove this
  lqWrite.nc := in.nc.get
  lqWrite.mmio := in.mmio.get
  lqWrite.memBackTypeMM := !in.pmp.get.mmio
  lqWrite.hasException := false.B // ?
  lqWrite.isHyper := in.tlbException.get.isHyper
  lqWrite.isForVSnonLeafPTE := exceptionIsForVSnonLeafPTE
  lqWrite.isPrefetch := false.B // TODO: remove this
  lqWrite.isHWPrefetch := false.B // TODO: remove this
  lqWrite.forwardMask := DontCare // TODO: remove this
  lqWrite.forwardData := DontCare // TODO: remove this
  lqWrite.ldCancel := DontCare // TODO: remove this
  lqWrite.isvec := isVector
  lqWrite.isLastElem := DontCare // TODO: remove this
  lqWrite.is128bit := in.size === MemorySize.Q.U
  lqWrite.uop_unit_stride_fof := DontCare // TODO: remove this
  lqWrite.usSecondInv := DontCare // TODO: remove this
  lqWrite.elemIdx := in.elemIdx.get
  lqWrite.alignedType := in.size
  lqWrite.mbIndex := in.mbIndex.get
  lqWrite.reg_offset := in.regOffset.get
  lqWrite.elemIdxInsideVd := in.elemIdxInsideVd.get
  lqWrite.is_first_ele := DontCare // TODO: remove this
  lqWrite.vecBaseVaddr := DontCare
  lqWrite.vecVaddrOffset := DontCare
  lqWrite.vecTriggerMask := DontCare
  lqWrite.vecActive := true.B // TODO: remove this
  lqWrite.isLoadReplay := LoadEntrance.isReplay(entrance)
  lqWrite.isFastPath := DontCare // TODO: remove this
  lqWrite.isFastReplay := DontCare // TODO: remove this
  lqWrite.replayCarry := DontCare // TODO: remove this
  lqWrite.isFirstIssue := DontCare // TODO: remove this
  lqWrite.hasROBEntry := DontCare // TODO: remove this
  lqWrite.mshrid := DontCare // TODO: remove this
  lqWrite.handledByMSHR := in.handledByMSHR.get
  lqWrite.replacementUpdated := DontCare // TODO: remove this
  lqWrite.missDbUpdated := DontCare // TODO: remove this
  lqWrite.forward_tlDchannel := DontCare // TODO: remove this
  lqWrite.dcacheRequireReplay := DontCare // TODO: remove this
  lqWrite.delayedLoadError := DontCare // TODO: remove this
  lqWrite.lateKill := DontCare // TODO: remove this
  lqWrite.feedbacked := DontCare // TODO: remove this
  lqWrite.schedIndex := in.replayQueueIdx.get
  lqWrite.tlbNoQuery := DontCare // TODO: remove this
  lqWrite.isFrmMisAlignBuf := false.B // TODO: remove this
  lqWrite.isMisalign := DontCare // TODO: remove this
  lqWrite.isFinalSplit := DontCare // TODO: remove this
  lqWrite.misalignWith16Byte := DontCare // TODO: remove this
  lqWrite.misalignNeedWakeUp := DontCare // TODO: remove this
  lqWrite.updateAddrValid := ldoutValid
  lqWrite.rep_info.mshr_id := in.mshrId.get
  lqWrite.rep_info.full_fwd := false.B
  lqWrite.rep_info.data_inv_sq_idx := in.dataInvalidSqIdx.get
  lqWrite.rep_info.addr_inv_sq_idx := in.addrInvalidSqIdx.get
  lqWrite.rep_info.rep_carry := DontCare
  lqWrite.rep_info.last_beat := in.paddr.get(log2Up(refillBytes))
  lqWrite.rep_info.cause := lqWriteCauseOH
  lqWrite.rep_info.debug := uop.debugInfo
  lqWrite.rep_info.tlb_id := in.tlbId.get
  lqWrite.rep_info.tlb_full := in.tlbFull.get
  lqWrite.nc_with_data := in.isNCReplay()
  lqWrite.data_wen_dup := DontCare // TODO: remove this

  // Writeback to VLMergeBuffer
  val vecldoutValid = pipeIn.valid && !kill && shouldWriteback && isVector && endPipe
  val vecldout = Wire(new VecPipelineFeedbackIO(isVStore = false))
  vecldout.mBIndex := in.mbIndex.get
  vecldout.hit := !shouldReplay || lqWriteReady
  vecldout.isvec := isVector
  vecldout.flushState := DontCare
  vecldout.sourceType := RSFeedbackType.lrqFull
  vecldout.trigger := uop.trigger
  vecldout.nc := false.B
  vecldout.mmio := false.B
  vecldout.exceptionVec := exceptionVec
  vecldout.hasException := exception
  vecldout.vaddr := exceptionFullva
  vecldout.vaNeedExt := exceptionVaNeedExt
  vecldout.gpaddr := exceptionGpaddr
  vecldout.isForVSnonLeafPTE := exceptionIsForVSnonLeafPTE
  vecldout.vstart := uop.vpu.vstart
  vecldout.vecTriggerMask := in.vecTriggerMask.get
  vecldout.elemIdx := in.elemIdx.get
  vecldout.mask := in.mask
  vecldout.alignedType := in.size
  vecldout.reg_offset.get := in.regOffset.get
  vecldout.elemIdxInsideVd.get := in.elemIdxInsideVd.get
  vecldout.vecdata.get := DontCare // assign data from LoadUnitDataPath

  /**
    * Exception info
    */
  val exceptionInfoValid = ldoutValid && !in.isMMIOReplay() // MMIO replay sends exceptionInfo independently
  val exceptionInfo = Wire(new MemExceptionInfo)
  exceptionInfo.robIdx := robIdx
  exceptionInfo.exceptionVec := exceptionVec
  exceptionInfo.vaddr := exceptionFullva
  exceptionInfo.gpaddr := exceptionGpaddr
  exceptionInfo.isForVSnonLeafPTE := exceptionIsForVSnonLeafPTE
  exceptionInfo.vaNeedExt := exceptionVaNeedExt
  exceptionInfo.isHyper := in.tlbException.get.isHyper
  exceptionInfo.uopIdx := 0.U.asTypeOf(UopIdx())
  exceptionInfo.vl := 0.U
  exceptionInfo.vstart := 0.U

  /**
    * Pipeline connect
    */
  val pipeOutValid = RegInit(false.B)
  val pipeOutBits = Reg(new LoadStageIO)
  when (kill || endPipe) { pipeOutValid := false.B }
  .elsewhen (pipeIn.fire) { pipeOutValid := true.B }
  .elsewhen (pipeOut.fire) { pipeOutValid := false.B }

  // Consider only unalign head
  val stageInfo = Wire(pipeOut.bits.cloneType)
  connectSamePort(stageInfo, in)
  stageInfo.uop.exceptionVec := s3ExceptionVec
  stageInfo.matchInvalid.get := s3MatchInvalid
  stageInfo.shouldWakeup.get := s3ShouldWakeup
  stageInfo.shouldWriteback.get := s3ShouldWriteback
  stageInfo.hasException.get := s3Exception
  stageInfo.headAlwaysWriteback.get := s3Exception || s3MatchInvalid
  stageInfo.writebackDependOnTail.get := s3ShouldWriteback && !s3Exception && !s3MatchInvalid
  stageInfo.shouldRarViolation.get := s3RarViolation

  when (pipeIn.fire) { pipeOutBits := stageInfo }

  /**
    * IO assignment
    */
  io_pipeOut.get.valid := pipeOutValid
  io_pipeOut.get.bits := pipeOutBits
  io_pipeIn.get.ready := !pipeOutValid || kill || endPipe || pipeOut.ready

  io.ldout.valid := ldoutValid
  io.ldout.bits := ldout
  io.lqWrite.valid := lqWriteValid
  io.lqWrite.bits := lqWrite
  io.vecldout.valid := vecldoutValid
  io.vecldout.bits := vecldout

  io.fastReplay.valid := shouldFastReplay
  io.fastReplay.bits := fastReplay

  io.revokeLastCycle := revokeLastCycle
  io.revokeLastLastCycle := revokeLastLastCycle

  io.rollback.valid := rollbackValid
  io.rollback.bits := DontCare
  io.rollback.bits.isRVC := uop.isRVC
  io.rollback.bits.robIdx := robIdx
  io.rollback.bits.ftqIdx := uop.ftqPtr
  io.rollback.bits.ftqOffset := uop.ftqOffset
  io.rollback.bits.level := rollbackLevel
  io.rollback.bits.target := uop.pc
  io.rollback.bits.debug_runahead_checkpoint_id := uop.debugInfo.runahead_checkpoint_id

  io.exceptionInfo.valid := exceptionInfoValid
  io.exceptionInfo.bits := exceptionInfo

  io.cancel := cancel

  io.debugInfo.isReplayFast := pipeIn.valid && doFastReplay
  io.debugInfo.isReplaySlow := lqWriteValid && cause.asUInt.orR
  io.debugInfo.isReplayRS := false.B // load never replays from RS
  io.debugInfo.isReplay := pipeIn.valid && cause.asUInt.orR
  io.debugInfo.replayCause := cause
  io.debugInfo.replayCnt := 1.U
  io.debugInfo.robIdx := robIdx.value
}

class LoadUnitS4(param: ExeUnitParams)(
  implicit p: Parameters,
  override implicit val s: LoadStage = LoadS4()
) extends LoadUnitStage(param) {
  val io = IO(new Bundle() {
    val unalignConcat = ValidIO(new LoadStageIO()(p, LoadS3()))
  })

  io_pipeIn.get.ready := true.B
  io.unalignConcat.valid := io_pipeIn.get.valid
  io.unalignConcat.bits := io_pipeIn.get.bits
}

class LoadUnitDataPathMeta(implicit p: Parameters) extends XSBundle with HasDCacheParameters {
  val bankOffset = UInt(DCacheVWordOffset.W)
  val fuOpType = FuOpType()
  val fpWen = Bool()
  val isNCReplay = Bool()
  val isMMIOReplay = Bool()
  val isUnalignHead = Bool()
}

class LoadUnitDataPath(val param: ExeUnitParams)(implicit p: Parameters) extends XSModule with HadNewLoadHelper {
  val io = IO(new Bundle() {
    val s1Meta = Flipped(ValidIO(new LoadUnitDataPathMeta))
    val s2SqForwardResp = Flipped(ValidIO(new SQForwardRespS2))
    val s2SbufferForwardResp = Flipped(ValidIO(new SbufferForwardResp))
    val s2UncacheForwardResp = Flipped(ValidIO(new UncacheForwardResp))
    val s2MSHRForwardResp = Flipped(ValidIO(new DCacheForwardResp))
    val s2TLDForwardResp = Flipped(ValidIO(new DCacheForwardResp))
    val s2UncacheBypassResp = Flipped(ValidIO(new UncacheBypassRespS2))
    val s2DCacheResp = Flipped(ValidIO(new DCacheWordResp))
    val s3ShiftData = Output(UInt(VLEN.W)) // used by vector writeback
    val s3ShiftAndExtData = Output(UInt(VLEN.W)) // used by scalar writeback
  })

  // S1
  val s1Valid = io.s1Meta.valid
  val s1Meta = io.s1Meta.bits

  // S2
  val s2Valid = RegNext(s1Valid, false.B)
  val s2Meta = RegEnable(s1Meta, s1Valid)
  val bankOffset = s2Meta.bankOffset
  val fuOpType = s2Meta.fuOpType
  val fpWen = s2Meta.fpWen
  val isNCReplay = s2Meta.isNCReplay
  val isMMIOReplay = s2Meta.isMMIOReplay
  val isUncacheReplay = isNCReplay || isMMIOReplay
  val isUnalignHead = s2Meta.isUnalignHead
  val isUnalignTail = RegNext(isUnalignHead && s2Valid, false.B)
  val unalignHeadBankOffset = RegEnable(bankOffset, isUnalignHead && s2Valid) // from the perspective of unalign tail

  val uncacheBypassData = io.s2UncacheBypassResp.bits.data
  val dcacheData = io.s2DCacheResp.bits.data
  val rawData = Mux(isUncacheReplay, uncacheBypassData, dcacheData)

  val sqForwardMask = io.s2SqForwardResp.bits.forwardMask.asUInt
  val sqForwardData = io.s2SqForwardResp.bits.forwardData.asUInt
  val ncForwardMask = io.s2UncacheForwardResp.bits.forwardMask.asUInt & Fill(VLEN / 8, isNCReplay)
  val ncForwardData = io.s2UncacheForwardResp.bits.forwardData.asUInt
  val sbufferForwardMask = io.s2SbufferForwardResp.bits.forwardMask.asUInt
  val sbufferForwardData = io.s2SbufferForwardResp.bits.forwardData.asUInt
  val tldMask = Fill(VLEN / 8, io.s2TLDForwardResp.valid)
  val tldData = io.s2TLDForwardResp.bits.forwardData.asUInt
  val mshrMask = Fill(VLEN / 8, io.s2MSHRForwardResp.valid)
  val mshrData = io.s2MSHRForwardResp.bits.forwardData.asUInt
  val (masks, datas) = Seq(
    // DO NOT change the priority here
    (sqForwardMask, sqForwardData),
    (ncForwardMask, ncForwardData),
    (sbufferForwardMask, sbufferForwardData),
    (tldMask, tldData),
    (mshrMask, mshrData)
  ).unzip

  val s2Data = mergeData(rawData, datas, masks)
  val s2RdataTypeOH = genRdataOH(fuOpType, fpWen)
  val s2RdataSelByOffset = VecInit((0 until VLEN / 8).map(i => bankOffset === i.U))
  // If the load is unaligned, its bank offset must reside in (8, 15]
  val unalignHeadBankOffsetUpperBound = VLEN / 8 - 1 // 15
  val unalignHeadBankOffsetLowerBound = XLEN / 8 // 8
  val s2TailRdataSelByHeadOffset = VecInit(
    (unalignHeadBankOffsetUpperBound until unalignHeadBankOffsetLowerBound by -1).map(i =>
      unalignHeadBankOffset === i.U
    )
  )

  // S3
  val s3Valid = RegNext(s2Valid, false.B)
  val s3Data = RegEnable(s2Data, s2Valid)
  val s3IsUnalignTail = RegEnable(isUnalignTail, s2Valid)
  val s3RdataTypeOH = RegEnable(s2RdataTypeOH, s2Valid)
  val s3RdataSelByOffset = RegEnable(s2RdataSelByOffset, s2Valid)
  val s3TailRdataSelByHeadOffset = RegEnable(s2TailRdataSelByHeadOffset, s2Valid)
  // Data shifting
  val s3ShiftHeadList = (0 until VLEN by 8).map(i => (s3Data >> i))
  val s3ShiftTailList = (1*8 until XLEN by 8).map(i => (s3Data << i).take(VLEN))
  val s3ShiftHead = Mux1H(s3RdataSelByOffset, s3ShiftHeadList)
  val s3ShiftTail = Mux1H(s3TailRdataSelByHeadOffset, s3ShiftTailList)
  val s4ShiftHead = RegEnable(s3ShiftHead, s3Valid)
  val s3ShiftData = Mux(
    s3IsUnalignTail,
    s4ShiftHead | s3ShiftTail,
    s3ShiftHead
  )
  // Sign / Zero extension
  val s3ShiftAndExtData = genRdata(s3RdataTypeOH, s3ShiftData.take(XLEN))

  // IO assignment
  io.s3ShiftData := s3ShiftData
  io.s3ShiftAndExtData := s3ShiftAndExtData

  def mergeData(oldData: UInt, newData: Seq[UInt], mask: Seq[UInt]): UInt = {
    val bytesNum = mask.head.getWidth
    require(oldData.getWidth == newData.head.getWidth)
    require(oldData.getWidth == (bytesNum * 8))
    VecInit((0 until bytesNum).map { case i =>
      val sels = mask.map(_(i).asBool) :+ true.B
      val bytes = newData.map(getByte(_, i)) :+ getByte(oldData, i)
      ParallelPriorityMux(sels, bytes)
    }).asUInt
  }

  def getByte(data: UInt, i: Int): UInt = data((i + 1) * 8 - 1, i * 8)
}

class LoadUnitIO(val param: ExeUnitParams)(implicit p: Parameters) extends XSBundle {
  val redirect = Flipped(ValidIO(new Redirect))
  // Request sources
  val ldin = Flipped(DecoupledIO(new ExuInput(param, hasCopySrc = true)))
  val vecldin = Flipped(DecoupledIO(new VectorLoadIn))
  val replay = Flipped(DecoupledIO(new LoadReplayIO))
  val prefetchReq = Flipped(DecoupledIO(new L1PrefetchReq))
  // Writeback to Backend / LQ / VLMergeBuffer
  val ldout = DecoupledIO(new ExuOutput(param))
  val lqWrite = DecoupledIO(new LqWriteBundle)
  val vecldout = Decoupled(new VecPipelineFeedbackIO(isVStore = false))
  // TLB / PMA / PMP
  val tlb = new TlbRequestIO(2)
  val tlbHint = Flipped(new TlbHintReq)
  val pmp = Flipped(new PMPRespBundle)
  // DCache
  val dcache = new DCacheLoadIO
  // IQ wakeup and load cancel
  val wakeup = ValidIO(new MemWakeUpBundle)
  val cancel = Output(Bool())
  // Data forwarding and bypass
  val sqForward = new SQForward
  val sbufferForward = new SbufferForward
  val uncacheForward = new UncacheForward
  val mshrForward = new DCacheForward
  val tldForward = new DCacheForward
  val uncacheBypass = new UncacheBypass
  // Uncache data
  val uncacheData = Input(new LoadDataFromLQBundle)
  // Nuke check with StoreUnit
  val staNukeQueryReq = Flipped(Vec(StorePipelineWidth, ValidIO(new StoreNukeQueryReq)))
  // Nuke check with RAR / RAW
  val rarNukeQuery = new LoadRARNukeQuery
  val rawNukeQuery = new LoadRAWNukeQuery
  val rollback = ValidIO(new Redirect)
  // Prefetch Train
  val prefetchTrain = ValidIO(new LsPrefetchTrainBundle)
  // Software instruction prefetch
  val swInstrPrefetch = ValidIO(new SoftIfetchPrefetchBundle)
  // CSR control signals and load trigger
  val csrCtrl = Flipped(new CustomCSRCtrlIO)
  val csrTrigger = Input(new CsrTriggerBundle)
  // Debug info
  val debugInfo = Output(new DebugLsInfoBundle)
}

class NewLoadUnit(val param: ExeUnitParams)(implicit p: Parameters) extends XSModule {
  val io = IO(new LoadUnitIO(param))

  val s0 = Module(new LoadUnitS0(param))
  val s1 = Module(new LoadUnitS1(param))
  val s2 = Module(new LoadUnitS2(param))
  val s3 = Module(new LoadUnitS3(param))
  val s4 = Module(new LoadUnitS4(param))
  val dataPath = Module(new LoadUnitDataPath(param))

  // Internal wiring
  s1 <> s0
  s2 <> s1
  s3 <> s2
  s4 <> s3
  s0.io.unalignTail <> s1.io.unalignTail
  s0.io.fastReplay <> s3.io.fastReplay
  s3.io.unalignConcat <> s4.io.unalignConcat
  s1.io.kill := false.B
  s2.io.kill := false.B
  s3.io.kill := false.B
  dataPath.io.s1Meta := s1.io.dataPathMeta

  // IO wiring
  // S0
  s0.io.replay <> io.replay
  s0.io.prefetchReq <> io.prefetchReq
  s0.io.vecldin <> io.vecldin
  s0.io.ldin <> io.ldin
  io.tlb.req <> s0.io.tlbReq
  io.dcache.req <> s0.io.dcacheReq
  io.dcache.is128Req := s0.io.is128Req
  io.dcache.replacementUpdated := s0.io.replacementUpdated
  io.dcache.pf_source := s0.io.pfSource
  io.sqForward.s0Req := s0.io.sqSbForwardReq
  io.sbufferForward.s0Req := s0.io.sqSbForwardReq
  io.uncacheForward.s0Req := s0.io.uncacheForwardReq
  io.mshrForward.s0Req := s0.io.mshrForwardReq
  io.tldForward.s0Req := s0.io.tldForwardReq
  io.uncacheBypass.s0Req := s0.io.uncacheBypassReq
  io.wakeup := s0.io.wakeup

  // S1
  s1.io.redirect := io.redirect
  s1.io.tlbResp <> io.tlb.resp
  io.tlb.req_kill := s1.io.tlbReqKill
  io.tlb.req.bits.pmp_addr := s1.io.tlbPAddr // TODO
  io.dcache.s1_paddr_dup_lsu := s1.io.dcachePAddr
  io.dcache.s1_paddr_dup_dcache := s1.io.dcachePAddr
  io.dcache.s1_kill := s1.io.dcacheKill
  io.sqForward.s1Req := s1.io.storeForwardReq
  io.sqForward.s1Kill := s1.io.sqForwardKill
  s1.io.sqForwardResp := io.sqForward.s1Resp
  io.sbufferForward.s1Req := s1.io.storeForwardReq
  io.sbufferForward.s1Kill := s1.io.sbufferForwardKill
  io.uncacheForward.s1Req := s1.io.storeForwardReq
  io.uncacheForward.s1Kill := s1.io.uncacheForwardKill
  io.mshrForward.s1Req := s1.io.mshrForwardReq
  io.mshrForward.s1Kill := s1.io.mshrForwardKill
  io.tldForward.s1Req := s1.io.tldForwardReq
  io.tldForward.s1Kill := s1.io.tldForwardKill
  s1.io.uncacheBypassResp := io.uncacheBypass.s1Resp
  s1.io.staNukeQueryReq := io.staNukeQueryReq
  io.swInstrPrefetch := s1.io.swInstrPrefetch
  s1.io.csrTrigger := io.csrTrigger

  // S2
  s2.io.redirect := io.redirect
  s2.io.pmp := io.pmp
  s2.io.tlbHint := io.tlbHint
  io.dcache.s2_kill := s2.io.dcacheKill
  s2.io.dcacheResp <> io.dcache.resp
  s2.io.dcacheBankConflict := io.dcache.s2_bank_conflict
  s2.io.dcacheMSHRNack := io.dcache.s2_mq_nack
  s2.io.sqForwardResp := io.sqForward.s2Resp
  s2.io.sbufferForwardResp := io.sbufferForward.s2Resp
  s2.io.uncacheForwardResp := io.uncacheForward.s2Resp
  s2.io.mshrForwardResp := io.mshrForward.s2Resp
  s2.io.tldForwardResp := io.tldForward.s2Resp
  s2.io.uncacheBypassResp := io.uncacheBypass.s2Resp
  s2.io.staNukeQueryReq := io.staNukeQueryReq
  io.rarNukeQuery.req <> s2.io.rarNukeQueryReq
  io.rawNukeQuery.req <> s2.io.rawNukeQueryReq
  io.prefetchTrain.valid := GatedValidRegNext(s2.io.prefetchTrain.valid)
  io.prefetchTrain.bits := RegEnable(s2.io.prefetchTrain.bits, s2.io.prefetchTrain.valid)
  s2.io.csrCtrl := io.csrCtrl

  // S3
  s3.io.redirect := io.redirect
  s3.io.dcacheError := io.dcache.resp.bits.error_delayed
  io.ldout <> s3.io.ldout
  io.lqWrite <> s3.io.lqWrite
  io.vecldout <> s3.io.vecldout
  s3.io.rarNukeQueryResp := io.rarNukeQuery.resp
  io.rarNukeQuery.revokeLastCycle := s3.io.revokeLastCycle
  io.rarNukeQuery.revokeLastLastCycle := s3.io.revokeLastLastCycle
  io.rawNukeQuery.revokeLastCycle := s3.io.revokeLastCycle
  io.rawNukeQuery.revokeLastLastCycle := s3.io.revokeLastLastCycle
  io.rollback := s3.io.rollback
  io.cancel := s3.io.cancel
  s3.io.uncacheData := RegNextN(io.uncacheData, 3) // TODO
  s3.io.csrCtrl := io.csrCtrl

  // Data path
  dataPath.io.s2SqForwardResp := io.sqForward.s2Resp
  dataPath.io.s2SbufferForwardResp := io.sbufferForward.s2Resp
  dataPath.io.s2UncacheForwardResp := io.uncacheForward.s2Resp
  dataPath.io.s2MSHRForwardResp := io.mshrForward.s2Resp
  dataPath.io.s2TLDForwardResp := io.tldForward.s2Resp
  dataPath.io.s2UncacheBypassResp := io.uncacheBypass.s2Resp
  dataPath.io.s2DCacheResp.valid := io.dcache.resp.valid
  dataPath.io.s2DCacheResp.bits := io.dcache.resp.bits
  io.ldout.bits.data := VecInit(Seq.fill(param.wbPathNum)(dataPath.io.s3ShiftAndExtData))
  io.vecldout.bits.vecdata.get := dataPath.io.s3ShiftData

  // Debug info
  io.debugInfo.s1_isTlbFirstMiss := s1.io.debugInfo.isTlbFirstMiss
  io.debugInfo.s1_isLoadToLoadForward := s1.io.debugInfo.isLoadToLoadForward
  io.debugInfo.s2_isBankConflict := s2.io.debugInfo.isBankConflict
  io.debugInfo.s2_isDcacheFirstMiss := s2.io.debugInfo.isDCacheMiss
  io.debugInfo.s2_isForwardFail := s2.io.debugInfo.isForwardFail
  io.debugInfo.s3_isReplayFast := s3.io.debugInfo.isReplayFast
  io.debugInfo.s3_isReplaySlow := s3.io.debugInfo.isReplaySlow
  io.debugInfo.s3_isReplayRS := s3.io.debugInfo.isReplayRS
  io.debugInfo.s3_isReplay := s3.io.debugInfo.isReplay
  io.debugInfo.replayCause := s3.io.debugInfo.replayCause
  io.debugInfo.replayCnt := s3.io.debugInfo.replayCnt
  io.debugInfo.s1_robIdx := s1.io.debugInfo.robIdx
  io.debugInfo.s2_robIdx := s2.io.debugInfo.robIdx
  io.debugInfo.s3_robIdx := s3.io.debugInfo.robIdx

  io.dcache.s0_pc := s0.io.debugInfo.pc
  io.dcache.s1_pc := s1.io.debugInfo.pc
  io.dcache.s2_pc := s2.io.debugInfo.pc
}

abstract class LoadUnitStage(val param: ExeUnitParams)(
  implicit p: Parameters,
  implicit val s: LoadStage
) extends XSModule with OnLoadStage
  with HasDCacheParameters
  with HasCircularQueuePtrHelper {
  val io_pipeIn = if (afterS1) {
    Some(IO(Flipped(DecoupledIO(new LoadStageIO()(p, prevStage(s))))))
  } else None
  val io_pipeOut = if (!lastStage) {
    Some(IO(DecoupledIO(new LoadStageIO)))
  } else None

  def <>(that: LoadUnitStage): Unit = {
    this.io_pipeIn.foreach(_ <> that.io_pipeOut.get)
  }
}

trait HasNukePAddrMatch { this: LoadUnitStage =>
  def nukePAddrMatch(storePAddr: UInt, storeMatchType: UInt, loadPAddr: UInt): Bool = {
    Mux(
      StLdNukeMatchType.isCacheLine(storeMatchType),
      (storePAddr >> blockOffBits) === (loadPAddr >> blockOffBits),
      (storePAddr >> DCacheVWordOffset) === (loadPAddr >> DCacheVWordOffset)
    )
  }
}

case class RdataType(
  selFu: (UInt, Bool) => Bool, // (fuOpType, fpWen) => sel
  dataFu: UInt => UInt
)

trait HadNewLoadHelper { this: XSModule =>
  val LBU = RdataType(
    selFu = (fuOpType, fpWen) => fuOpType === LSUOpType.lbu || fuOpType === LSUOpType.hlvbu,
    dataFu = data => ZeroExt(data(7, 0), XLEN)
  )
  val LHU = RdataType(
    selFu = (fuOpType, fpWen) => fuOpType === LSUOpType.lhu || fuOpType === LSUOpType.hlvhu || fuOpType === LSUOpType.hlvxhu,
    dataFu = data => ZeroExt(data(15, 0), XLEN)
  )
  val LWU = RdataType(
    selFu = (fuOpType, fpWen) => fuOpType === LSUOpType.lwu || fuOpType === LSUOpType.hlvwu || fuOpType === LSUOpType.hlvxwu,
    dataFu = data => ZeroExt(data(31, 0), XLEN)
  )
  val LD = RdataType(
    selFu = (fuOpType, fpWen) => fuOpType === LSUOpType.ld || fuOpType === LSUOpType.hlvd,
    dataFu = data => data(63, 0)
  )
  val LB = RdataType(
    selFu = (fuOpType, fpWen) => fuOpType === LSUOpType.lb || fuOpType === LSUOpType.hlvb,
    dataFu = data => SignExt(data(7, 0) , XLEN)
  )
  val LH = RdataType(
    selFu = (fuOpType, fpWen) => fuOpType === LSUOpType.lh && !fpWen || fuOpType === LSUOpType.hlvh,
    dataFu = data => SignExt(data(15, 0) , XLEN)
  )
  val LW = RdataType(
    selFu = (fuOpType, fpWen) => fuOpType === LSUOpType.lw && !fpWen || fuOpType === LSUOpType.hlvw,
    dataFu = data => SignExt(data(31, 0) , XLEN)
  )
  val LH_FP = RdataType(
    selFu = (fuOpType, fpWen) => fuOpType === LSUOpType.lh && fpWen,
    dataFu = data => FPU.box(data, FPU.H)
  )
  val LW_FP = RdataType(
    selFu = (fuOpType, fpWen) => fuOpType === LSUOpType.lw && fpWen,
    dataFu = data => FPU.box(data, FPU.S)
  )

  val types: Seq[RdataType] = Seq(LBU, LHU, LWU, LD, LB, LH, LW, LH_FP, LW_FP)
  val num = types.length
  def genRdataOH(fuOpType: UInt, fpWen: Bool): Vec[Bool] = VecInit(types.map(_.selFu(fuOpType, fpWen)))
  def genRdata(sel: Vec[Bool], data: UInt): UInt = {
    XSError(PopCount(sel) > 1.U, "data selector must be One-Hot!")
    Mux1H(sel, types.map(_.dataFu(data)))
  }
}

/**
  * Only for compiling the module independently
  */
class NewLoadUnitTop(implicit val p: Parameters) extends Module
  with HasXSParameter
  with HasMemBlockParameters {
  val param = ldaParams.head
  param.bindBackendParam(backendParams)
  val io = IO(new LoadUnitIO(param))
  val ldu = Module(new NewLoadUnit(param))
  io <> ldu.io
}

object NewLoadUnitMain extends App {
  val (config, firrtlOpts, firtoolOpts) = ArgParser.parse(
    args :+ "--disable-always-basic-diff" :+ "--dump-fir" :+ "--fpga-platform" :+ "--target" :+ "verilog")

  val defaultConfig = config.alterPartial({
    // Get XSCoreParams and pass it to the "small module"
    case XSCoreParamsKey => config(XSTileKey).head
  })

  Generator.execute(
    firrtlOpts,
    new NewLoadUnitTop()(defaultConfig),
    firtoolOpts
  )

  println("done")
}