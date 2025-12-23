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
import utils._
import utility._
import xiangshan._
import xiangshan.ExceptionNO._
import xiangshan.backend.Bundles.{DynInst, ExuInput, ExuOutput, MemExuOutput, MemWakeUpBundle, connectSamePort}
import xiangshan.backend.fu.PMPRespBundle
import xiangshan.backend.fu.FuConfig._
import xiangshan.backend.fu.FuType
import xiangshan.backend.ctrlblock.{DebugLsInfoBundle, LsTopdownInfo}
import xiangshan.backend.rob.RobPtr
import xiangshan.backend.ctrlblock.DebugLsInfoBundle
import xiangshan.backend.fu.NewCSR._
import xiangshan.backend.fu.util.SdtrigExt
import xiangshan.backend.exu.ExeUnitParams
import xiangshan.mem.mdp._
import xiangshan.mem.Bundles._
import xiangshan.mem.LoadStage._
import xiangshan.cache._
import xiangshan.cache.wpu.ReplayCarry
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

    // IQ wakeup
    val wakeup = ValidIO(new MemWakeUpBundle)

    // Debug info
    val debugInfo = Output(new Bundle() {
      val pc = Output(UInt(VAddrBits.W))
    })
  })

  // TODO: remove this
  dontTouch(io)
  dontTouch(io_pipeOut.get)

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
  fastReplay.bits.entrance := LoadEntrance.fastReplay.U
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
  val isSwInstrPrefetch = sink.bits.accessType.isInstrPrefetch
  val isHwPrefetch = sink.bits.accessType.isHwPrefetch
  val isUncacheReplay = sink.bits.uncacheReplay.get

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

  val alignCheckResults = needAlignCheckSources.map(s => alignCheck(s.bits.offset, s.bits.size)).unzip3
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

  def alignCheck(offset: UInt, size: UInt) = {
    require(offset.getWidth == DCacheLineOffset)
    require(size.getWidth == MemorySize.Size.width)
    // 1.1 Align check
    val align = LookupTree(size, List( // TODO: parameterize this
      "b00".U -> true.B,
      "b01".U -> (offset.take(1) === 0.U),
      "b10".U -> (offset.take(2) === 0.U),
      "b11".U -> (offset.take(3) === 0.U)
    ))
    assert(size =/= MemorySize.Q.U || offset.take(4) === 0.U)
    // 1.2 Bank bound check
    // 1.3 Word bound check
    val bankOffset = offset.take(bankOffsetWidth)
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
  storeForwardReq.uop := uop

  val uncacheForwardReqValid = replayHiPrio.fire && replayHiPrio.bits.isUncacheReplay

  val dcacheForwardReqValid = replayHiPrio.fire && replayHiPrio.bits.forwardDChannel.get
  val dcacheForwardReq = Wire(new DCacheForwardReqS0)
  dcacheForwardReq.vaddr := sink.bits.vaddr
  dcacheForwardReq.size := sink.bits.size
  dcacheForwardReq.mshrId := sink.bits.mshrId.get

  /**
    * IQ wakeup
    */
  // Select between 2 options based on timing result:
  // Option 1
  val needWakeupSources = Seq(unalignTail, replayHiPrio, fastReplay, replayLoPrio, scalarIssue)
  val needWakeupValids = needWakeupSources.map(s => s.fire && s.bits.accessType.isScalar) // exclude vector and prefetch
  // Option 2
  // val needWakeupSources = sources
  // val needWakeupValids = needWakeupSources.map(s => s.valid && sink.ready && s.bits.accessType.isScalar)
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

  io.wakeup.valid := wakeupValid
  io.wakeup.bits := wakeup

  io.debugInfo.pc := uop.pc

}

class LoadUnitS1(param: ExeUnitParams)(
  implicit p: Parameters,
  override implicit val s: LoadStage = LoadS1()
) extends LoadUnitStage(param) with HasVLSUParameters {
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

  // TODO: remove this
  dontTouch(io)
  dontTouch(io_pipeIn.get)
  dontTouch(io_pipeOut.get)

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

  val isSwInstrPrefetch = accessType.isSwPrefetch && accessType.isInstrPrefetch

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
  val paddr = Mux(noQuery, in.paddr.get, tlbResp.bits.paddr(0))
  val paddrDCache = Mux(noQuery, in.paddr.get, tlbResp.bits.paddr(1))
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
  loadTrigger.io.fromLoadStore.isVectorUnitStride := accessType.isVector && isUnitStride(mop)
  loadTrigger.io.fromLoadStore.mask := mask
  loadTrigger.io.isPrf.get := accessType.isPrefetch

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
  unalignTail.vaddr := ((vaddr >> bankOffsetWidth) + 1.U) << bankOffsetWidth
  unalignTail.fullva := ((in.fullva >> bankOffsetWidth) + 1.U) << bankOffsetWidth
  unalignTail.size := MemorySize.Q.U
  unalignTail.mask := genVWmask(vaddr, LSUOpType.size(fuOpType)) >> bankBytes
  unalignTail.align.get := false.B // align is only used for misalign exception, which unalignTail does not care
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

  def nukePAddrMatch(storePAddr: UInt, storeMatchType: UInt, loadPAddr: UInt): Bool = {
    Mux(
      StLdNukeMatchType.isCacheLine(storeMatchType),
      (storePAddr >> blockOffBits) === (loadPAddr >> blockOffBits),
      (storePAddr >> bankOffsetWidth) === (loadPAddr >> bankOffsetWidth)
    )
  }

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
  io.tlbPAddr := in.paddr.get

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

  io.unalignTail.valid := unalignTailInjectValid
  io.unalignTail.bits := unalignTail
  assert(!io.unalignTail.valid || io.unalignTail.ready)

  io.swInstrPrefetch.valid := pipeIn.valid && isSwInstrPrefetch
  io.swInstrPrefetch.bits.vaddr := vaddr

  io.debugInfo.isTlbFirstMiss := pipeIn.valid && tlbMiss &&
    LoadEntrance.isScalarIssue(entrance) &&
    LoadEntrance.isVectorIssue(entrance)
  io.debugInfo.isLoadToLoadForward := false.B
  io.debugInfo.robIdx := robIdx.value
  io.debugInfo.vaddr.valid := pipeIn.valid
  io.debugInfo.vaddr.bits := vaddr
  io.debugInfo.pc := uop.pc
}

class LoadUnitS2(param: ExeUnitParams)(
  implicit p: Parameters,
  override implicit val s: LoadStage = LoadS2()
) extends LoadUnitStage(param) {
  val io = IO(new Bundle() {
    val redirect = Flipped(ValidIO(new Redirect))

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
    val sqForwardResp = Flipped(ValidIO(new SQForwardResp))
    val sbufferForwardResp = Flipped(ValidIO(new SbufferForwardResp))
    val uncacheForwardResp = Flipped(ValidIO(new UncacheForwardResp))
    val mshrForwardResp = Flipped(ValidIO(new DCacheForwardResp))
    val tldForwardResp = Flipped(ValidIO(new DCacheForwardResp))

    // Nuke query from StoreUnit
    val staNukeQueryReq = Flipped(Vec(StorePipelineWidth, ValidIO(new StoreNukeQueryReq)))
    // Nuke query to LQRAR / LQRAW
    val rarNukeQueryReq = DecoupledIO(new LoadNukeQueryReq)
    val rawNukeQueryReq = DecoupledIO(new LoadNukeQueryReq)

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

  dontTouch(io)
  dontTouch(io_pipeIn.get)
  dontTouch(io_pipeOut.get)
  io <> DontCare
  io_pipeIn.get <> DontCare
  io_pipeOut.get <> DontCare
}

class LoadUnitS3(param: ExeUnitParams)(
  implicit p: Parameters,
  override implicit val s: LoadStage = LoadS3()
) extends LoadUnitStage(param) {
  val io = IO(new Bundle() {
    val redirect = Flipped(ValidIO(new Redirect))

    // DCache response
    val dcacheError = Input(Bool())

    // Writeback to Backend / LQ / VLMergeBuffer
    val ldout = DecoupledIO(new ExuOutput(param))
    val lqWrite = DecoupledIO(new LqWriteBundle)
    val vecldout = Decoupled(new VecPipelineFeedbackIO(isVStore = false))

    // Fast replay
    val fastReplay = DecoupledIO(new FastReplayIO)

    // RAR / RAW revoke and RAR response
    val rarNukeQueryResp = Flipped(ValidIO(new LoadNukeQueryResp))
    val revoke = Output(Bool())

    /**
      * Rollback and re-fetch from IFU, including:
      * 1. RAR violation
      * 2. vaddr-paddr mismatch happens in STLF
      */
    val rollback = ValidIO(new Redirect)

    // Load cancel
    val cancel = Output(Bool())

    // Uncache data
    val uncacheData = Input(new LoadDataFromLQBundle)
    
    // Prefetch train
    // TODO: this bundle is tooooooo big, define a smaller one
    val prefetchTrain = ValidIO(new LsPrefetchTrainBundle)

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

  dontTouch(io)
  dontTouch(io_pipeIn.get)
  io <> DontCare
  io_pipeIn.get <> DontCare
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
  // Data forwarding
  val sqForward = new SQForward
  val sbufferForward = new SbufferForward
  val uncacheForward = new UncacheForward
  val mshrForward = new DCacheForward
  val tldForward = new DCacheForward
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

  // Internal wiring
  s1 <> s0
  s2 <> s1
  s3 <> s2
  s0.io.unalignTail <> s1.io.unalignTail
  s0.io.fastReplay <> s3.io.fastReplay

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
  io.wakeup := s0.io.wakeup

  // S1
  s1.io.redirect := io.redirect
  s1.io.kill := false.B
  s1.io.tlbResp <> io.tlb.resp
  io.tlb.req_kill := s1.io.tlbReqKill
  io.tlb.req.bits.pmp_addr := s1.io.tlbPAddr // TODO
  io.dcache.s1_paddr_dup_lsu := s1.io.dcachePAddr
  io.dcache.s1_paddr_dup_dcache := s1.io.dcachePAddr
  io.dcache.s1_kill := s1.io.dcacheKill
  io.sqForward.s1Req := s1.io.storeForwardReq
  io.sqForward.s1Kill := s1.io.sqForwardKill
  io.sbufferForward.s1Req := s1.io.storeForwardReq
  io.sbufferForward.s1Kill := s1.io.sbufferForwardKill
  io.uncacheForward.s1Req := s1.io.storeForwardReq
  io.uncacheForward.s1Kill := s1.io.uncacheForwardKill
  io.mshrForward.s1Req := s1.io.mshrForwardReq
  io.mshrForward.s1Kill := s1.io.mshrForwardKill
  io.tldForward.s1Req := s1.io.tldForwardReq
  io.tldForward.s1Kill := s1.io.tldForwardKill
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
  s2.io.staNukeQueryReq := io.staNukeQueryReq
  io.rarNukeQuery.req <> s2.io.rarNukeQueryReq
  io.rawNukeQuery.req <> s2.io.rawNukeQueryReq
  s2.io.csrCtrl := io.csrCtrl

  // S3
  s3.io.redirect := io.redirect
  s3.io.dcacheError := io.dcache.resp.bits.error_delayed
  io.ldout <> s3.io.ldout
  io.lqWrite <> s3.io.lqWrite
  io.vecldout <> s3.io.vecldout
  s3.io.rarNukeQueryResp := io.rarNukeQuery.resp
  io.rarNukeQuery.revoke := s3.io.revoke
  io.rawNukeQuery.revoke := s3.io.revoke
  io.rollback := s3.io.rollback
  io.cancel := s3.io.cancel
  s3.io.uncacheData := RegNextN(io.uncacheData, 3) // TODO
  io.prefetchTrain := s3.io.prefetchTrain
  s3.io.csrCtrl := io.csrCtrl

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

  // TODO: parameters these somewhere else
  val bankBytes = 16
  val bankOffsetWidth = log2Up(bankBytes)

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