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
import utils._
import utility._
import xiangshan.ExceptionNO._
import xiangshan._
import xiangshan.backend.Bundles.{DynInst, MemExuInput, MemExuOutput}
import xiangshan.backend.fu.PMPRespBundle
import xiangshan.backend.fu.FuConfig._
import xiangshan.backend.datapath.{NewPipelineConnect, NewPipelineConnectPipe}
import xiangshan.backend.ctrlblock.{DebugLsInfoBundle, LsTopdownInfo}
import xiangshan.backend.rob.RobPtr
import xiangshan.backend.ctrlblock.DebugLsInfoBundle
import xiangshan.backend.fu.NewCSR._
import xiangshan.backend.fu.util.SdtrigExt
import xiangshan.cache._
import xiangshan.cache.wpu.ReplayCarry
import xiangshan.cache.mmu._
import xiangshan.mem.mdp._
import xiangshan.mem.ReplayCauseNo._
import xiangshan.mem.Bundles._

class HybridUnitIO()(implicit p: Parameters, val params: MemUnitParams) extends MemUnitIO {
  // from
  val fromLsq    = OptionWrapper(params.hasLoadExe, new Bundle() {
    val forward  = ValidIO(new LoadForwardRespBundle)
    val rarNuke  = ValidIO(new LoadNukeQueryRespBundle)
    val rawNuke  = ValidIO(new LoadNukeQueryRespBundle)
  })
  val fromSta        = OptionWrapper(params.hasLoadExe, Vec(StorePipelineWidth, Flipped(ValidIO(new StoreNukeQueryBundle))))
  val fromSBuffer    = OptionWrapper(params.hasLoadExe, Flipped(ValidIO(new LoadForwardRespBundle)))
  val fromMissQueue  = OptionWrapper(params.hasLoadExe, Flipped(ValidIO(new MissQueueForwardRespBundle)))
  val fromTLDchannel = OptionWrapper(params.hasLoadExe, Input(new DcacheToLduForwardIO))

  // to
  val toBackend = new Bundle() {
    val issue    = OptionWrapper(params.hasStoreAddrExe, ValidIO(new LsPipelineBundle))
    val wakeup   = OptionWrapper(params.hasLoadExe, ValidIO(new DynInst))
    val ldCancel = OptionWrapper(params.hasLoadExe, Output(new LoadCancelIO()))
    val rollback = OptionWrapper(params.hasLoadExe, ValidIO(new Redirect))
    val iqFeedback = OptionWrapper(params.hasStoreAddrExe, ValidIO(new RSFeedback))
  }
  val toLdu     = new Bundle() {
    val replay    = OptionWrapper(params.hasLoadExe, DecoupledIO(new LsPipelineBundle))
    val nukeQuery = OptionWrapper(params.hasStoreAddrExe, ValidIO(new StoreNukeQueryBundle))
  }
  val toLsq     = new Bundle() {
    val out      = OptionWrapper(params.hasLoadExe, DecoupledIO(new LsPipelineBundle))
    val forward  = OptionWrapper(params.hasLoadExe, ValidIO(new LoadForwardReqBundle))
    val rawNuke  = OptionWrapper(params.hasLoadExe, new Bundle() {
      val req = DecoupledIO(new LoadNukeQueryReqBundle)
      val revoke = Output(Bool())
    })
    val rarNuke  = OptionWrapper(params.hasLoadExe, new Bundle() {
      val req = DecoupledIO(new LoadNukeQueryReqBundle)
      val revoke = Output(Bool())
    })
    val addrUpdate = OptionWrapper(params.hasStoreAddrExe, ValidIO(new LsPipelineBundle))
    val excpUpdate = OptionWrapper(params.hasStoreAddrExe, ValidIO(new LsPipelineBundle))
    val maskOut    = OptionWrapper(params.hasStoreAddrExe, ValidIO(new StoreMaskBundle))
  }
  val toSBuffer   = OptionWrapper(params.hasLoadExe, ValidIO(new LoadForwardReqBundle))
  val toMissQueue = OptionWrapper(params.hasLoadExe, ValidIO(new MissQueueForwardReqBundle))
  val toPrefetch  = new Bundle() {
    val ifetch  = OptionWrapper(params.hasLoadExe, ValidIO(new SoftIfetchPrefetchBundle))
    val train   = OptionWrapper(params.hasPrefetch, new LsPrefetchTrainIO)
    val trainL1 = OptionWrapper(params.hasLoadExe, new LsPrefetchTrainIO)
  }
  val toLoadMisalignBuf  = OptionWrapper(params.hasLoadExe, ValidIO(new LsPipelineBundle))
  val toStoreMisalignBuf = OptionWrapper(params.hasStoreAddrExe, ValidIO(new LsPipelineBundle))

  //
  val correctMissTrain = Input(Bool())

  // perf
  val debugLsInfo   = Output(new DebugLsInfoBundle)
  val lsTopdownInfo = Flipped(Output(new LsTopdownInfo))
}

class HybridUnitImp(override val wrapper: MemUnit)(implicit p: Parameters, params: MemUnitParams)
  extends MemUnitImp(wrapper)
  with HasLoadHelper
  with HasDCacheParameters
  with HasCircularQueuePtrHelper
{
  private val lgSelectGroupSize = log2Ceil(RollbackGroupSize)
  private val TotalSelectCycles = scala.math.ceil(log2Ceil(LoadQueueRAWSize).toFloat / lgSelectGroupSize).toInt + 1
  private val TotalDelayCycles  = TotalSelectCycles - 2

  io.suggestName("none")
  override lazy val io = IO(new HybridUnitIO).suggestName("io")

  protected val toPrefetch  = io.toPrefetch
  protected val toLdu       = io.toLdu
  protected val toLsq       = io.toLsq
  protected val toSBuffer   = io.toSBuffer
  protected val toBackend   = io.toBackend
  protected val toMissQueue = io.toMissQueue
  protected val toStoreMisalignBuf = io.toStoreMisalignBuf
  protected val toLoadMisalignBuf = io.toLoadMisalignBuf

  protected val fromSta        = io.fromSta
  protected val fromLsq        = io.fromLsq
  protected val fromSBuffer    = io.fromSBuffer
  protected val fromMissQueue  = io.fromMissQueue
  protected val fromTLDchannel = io.fromTLDchannel

  def getStoreIqIssue(): Seq[DecoupledIO[LsPipelineBundle]] = {
    fromIssue.zip(params.issueParams).filter(x => x._2.isStore && x._2.isIq).map(_._1)
  }

  def getStoreIqWb(): Seq[DecoupledIO[LsPipelineBundle]] = {
    params.issueParams.filter(_._2.isIq).map(MemWBPortMap.getPort(_.wbPort)).distinct.map(
      toIssue(_)
    )
  }

  def getLoadIqIssue(): Seq[DecoupledIO[LsPipelineBundle]] = {
    fromIssue.zip(params.issueParams).filter(x => x._2.isLoad && x._2.isIq).map(_._1)
  }

  def getLoadIqWb(): Seq[DecoupledIO[LsPipelineBundle]] = {
    params.issueParams.filter(_._2.isIq).map(MemWBPortMap.getPort(_.wbPort)).distinct.map(
      toIssue(_)
    )
  }

  // Pipeline
  // --------------------------------------------------------------------------------
  // stage 0
  // --------------------------------------------------------------------------------
  val s0VAddr = WireInit(0.U(VAddrBits.W))
  val s0FullVA = WireInit(0.U(XLEN.W))
  val s0TlbVAddr = WireInit(0.U(VAddrBits.W))
  val s0PAddr = WireInit(0.U(PAddrBits.W))

  val issueParamPairs = fromIssue.zip(params.issueParams)
  fromIssue.zip(params.issueParams).zipWithIndex.map {
    case ((issue, issueParam), i) =>
      val prioritySelect = !fromIssue.slice(0, i).map(_.valid).reduce(_|_)
      val selected =
          if (i == 0) true.B
          else
            true match {
              case _ if (issueParam.isFastReplay || issueParam.isUncache) => {
                !issueParamPairs.filter(_._2.isLoadReplay).map {
                  case (req, param) =>
                    req.valid && req.bits.forwardTLDchannel
                }.reduce(_|_) && prioritySelect
              }

              case _ if (issueParam.isLoadReplay) => {
                  val loadReplayStall = issueParamPairs.filter(_._2.isIq).map {
                    case (req, param) =>
                      req.valid && isAfter(req.bits.uop.robIdx, issue.bits.uop.robIdx)
                  }.reduce(_|_)
                  val misalignBufIssues = issueParamPairs.take(i).filter(_._2.isMisalignBuf)

                  Mux(
                    issue.bits.forwardTLDchannel,
                    (if (misalignBufIssues.length > 0) !misalignBufIssues.map(_._1.valid).reduce(_|_) else true.B),
                    !loadReplayStall && prioritySelect
                  )
              }

              case _ if (issueParam.isVector || issueParam.isIq) => {
                  !issueParamPairs.filter(_._2.isPrefetch).map {
                    case (req, param) =>
                      req.valid && req.bits.confidence > 0.U
                  }.reduce(_|_) && prioritySelect
              }

              case _ if (issueParam.isPrefetch) => {
                  (issue.bits.confidence > 0.U) && !issueParamPairs.filter(
                    x => !(x._2.isVector || x._2.isIq || x._2.isPrefetch)
                  ).map(_._1.valid).reduce(_|_)
              }

              case _ => prioritySelect
            }

      val fullva =
        if (issueParam.hasAGU)
          issue.bits.src(0) + SignExt(issue.bits.uop.imm(11, 0), XLEN)
        else if (issueParam.isMisalignBuf)
          issue.bits.fullva
        else
          issue.bits.vaddr

      val vaddr = fullva(VAddrBits - 1, 0)
      val mask =
        if (issueParam.hasAGU)
          genVWmask128(vaddr, issue.bits.uop.fuOpType(2, 0))
        else
          issue.bits.mask

      issue.ready := false.B
      when (issue.valid && selected) {
        val wbPort = MemWBPortMap.getPort(issueParam.wbPort)
        if (wbPort >= 0)
          s0WBPort(wbPort) := true.B
        s0ArbOut.valid     := issue.valid
        s0ArbOut.bits      := issue.bits
        s0ArbOut.bits.mask := mask
        s0ArbOut.bits.vecActive := !issue.bits.isVector || issue.bits.vecActive
        // set s0VAddr
        if (!issueParam.isUncache)
          s0VAddr := vaddr
        // set s0PAddr
        if (issueParam.isFastReplay || issueParam.isIq || issueParam.isPrefetch)
          s0PAddr := issue.bits.paddr
        // set s0TlbVAddr
        if (issueParam.isFastReplay || issueParam.isIq || issueParam.isVector || issueParam.isPrefetch)
          s0TlbVAddr := vaddr

        issue.ready   := s0ArbOut.ready && (if (issueParam.hasDCacheQuery) io.toDCache.req.ready else true.B)
      }
  }

  s0Kill := Mux(s0Out.bits.isStore, s0Out.bits.uop.robIdx.needFlush(io.fromCtrl.redirect), false.B)

  // s0Out
  // make dev happy
  val s0TlbNoQuery = s0Out.bits.isPrefetch ||
                     s0Out.bits.isFastReplay ||
                     s0Out.bits.isUncache
  s0Out.bits.tlbNoQuery := s0TlbNoQuery
  s0Out.bits.vaddr  := s0VAddr
  s0Out.bits.paddr  := s0PAddr

  // prefetch related ctrl signal
  if (toPrefetch.train.isDefined) {
    val s0HighConfPrefetch = s0Out.bits.isHWPrefetch && s0Out.bits.confidence > 0.U
    toPrefetch.train.get.canAcceptHighConfPrefetch := s0Out.valid && s0HighConfPrefetch && toDCache.req.ready
    val s0LowConfPrefetch  = s0Out.bits.isHWPrefetch && s0Out.bits.confidence === 0.U
    toPrefetch.train.get.canAcceptLowConfPrefetch := s0Out.valid && s0LowConfPrefetch && toDCache.req.ready
  }
  if (toPrefetch.trainL1.isDefined) {
    toPrefetch.trainL1.get.canAcceptHighConfPrefetch := DontCare
    toPrefetch.trainL1.get.canAcceptLowConfPrefetch := DontCare
  }

  // to tlb
  toTlb.req.valid := s0Out.valid && !s0TlbNoQuery && Mux(s0Out.bits.isStore, true.B, toDCache.req.ready)
  toTlb.req.bits.vaddr := s0TlbVAddr
  toTlb.req.bits.cmd := Mux(
                          s0Out.bits.isHWPrefetch && s0Out.bits.isStore,
                          TlbCmd.write,
                          TlbCmd.read
                        )
  toTlb.req.bits.size := Mux(
                          s0Out.bits.isVector,
                          s0Out.bits.alignedType(2, 0),
                          LSUOpType.size(s0Out.bits.uop.fuOpType)
                        )
  toTlb.req.bits.kill := s0Kill
  toTlb.req.bits.no_translate := s0Out.bits.tlbNoQuery

  // to dcache
  toDCache.req.valid := s0Out.valid && !s0Out.bits.isSWPrefetch
  toDCache.req.bits.cmd := Mux(
                            s0Out.bits.uop.fuOpType === LSUOpType.prefetch_r,
                            MemoryOpConstants.M_PFR,
                            Mux(
                              s0Out.bits.uop.fuOpType === LSUOpType.prefetch_w || s0Out.bits.isStore,
                              MemoryOpConstants.M_PFW,
                              MemoryOpConstants.M_XRD
                            )
                          )
  toDCache.req.bits.instrtype := Mux(
                                  s0Out.bits.isHWPrefetch,
                                  DCACHE_PREFETCH_SOURCE.U,
                                  Mux(s0Out.bits.isStore, STORE_SOURCE.U, LOAD_SOURCE.U)
                                )
  toDCache.pf_source := Mux(
                        s0Out.bits.isHWPrefetch,
                        s0Out.bits.pfSource.value,
                        L1_HW_PREFETCH_NULL
                      )
  // dcache replacement extra info
  // TODO: should prefetch load update replacement?
  toDCache.replacementUpdated := Mux(s0Out.bits.isLoadReplay, s0Out.bits.replacementUpdated, false.B)

  if (env.FPGAPlatform) {
    toDCache.s0_pc := DontCare
  } else {
    toDCache.s0_pc := s0Out.bits.uop.pc
  }

  // to lsq maskout
  if (toLsq.maskOut.isDefined) {
    toLsq.maskOut.get.valid      := s0Out.valid && (s0Out.bits.isIq || s0Out.bits.isVector) && s0Out.bits.isStore
    toLsq.maskOut.get.bits.mask  := s0Out.bits.mask
    toLsq.maskOut.get.bits.sqIdx := s0Out.bits.uop.sqIdx
  }

  // to backend for wakeup
  // load wakeup
  // TODO: vector load wakeup?
  if (toBackend.wakeup.isDefined) {
    val s0NonPfIqWakeup = s0Out.bits.isIq && s0Out.bits.isLoad && !s0Out.bits.isPrefetch
    val s0CanWakeup = s0NonPfIqWakeup ||
                      s0Out.bits.isLoadReplay ||
                      s0Out.bits.isUncache ||
                      s0Out.bits.isFastReplay
    toBackend.wakeup.get.valid := s0Out.fire && s0CanWakeup
    toBackend.wakeup.get.bits  := s0Out.bits.uop
  }

  XSDebug(toDCache.req.fire && s0Out.bits.isLoad,
    p"[DCACHE LOAD REQ] pc ${Hexadecimal(s0Out.bits.uop.pc)}, vaddr ${Hexadecimal(s0Out.bits.vaddr)}\n"
  )
  XSDebug(toDCache.req.fire && s0Out.bits.isStore,
    p"[DCACHE STORE REQ] pc ${Hexadecimal(s0Out.bits.uop.pc)}, vaddr ${Hexadecimal(s0Out.bits.vaddr)}\n"
  )

  XSDebug(s0Out.valid && s0Out.bits.isLoad,
    p"S0: pc ${Hexadecimal(s0Out.bits.uop.pc)}, lId ${Hexadecimal(s0Out.bits.uop.lqIdx.asUInt)}, " +
    p"vaddr ${Hexadecimal(s0Out.bits.vaddr)}, mask ${Hexadecimal(s0Out.bits.mask)}\n"
  )
  XSDebug(s0Out.valid && s0Out.bits.isStore,
    p"S0: pc ${Hexadecimal(s0Out.bits.uop.pc)}, sId ${Hexadecimal(s0Out.bits.uop.sqIdx.asUInt)}, " +
    p"vaddr ${Hexadecimal(s0Out.bits.vaddr)}, mask ${Hexadecimal(s0Out.bits.mask)}\n"
  )

  // make dev happy
  val s0FuOpType = Mux(s0Out.bits.isVector, s0Out.bits.alignedType(1, 0), s0Out.bits.uop.fuOpType(1, 0))
  val s0AddrAligned = LookupTree(s0FuOpType, List(
    "b00".U   -> true.B,              //b
    "b01".U   -> (s0Out.bits.vaddr(0) === 0.U),   //h
    "b10".U   -> (s0Out.bits.vaddr(1,0) === 0.U), //w
    "b11".U   -> (s0Out.bits.vaddr(2,0) === 0.U)  //d
  ))

  // if vector store sends 128-bit requests, its address must be 128-aligned
  XSError(s0Out.valid && s0Out.bits.isVector && s0Out.bits.vaddr(3, 0) =/= 0.U && s0Out.bits.alignedType(2),
         "unit stride 128 bit element is not aligned!")

  // make dev happy
  val s0VectorMisaligned = s0Out.bits.uop.exceptionVec(storeAddrMisaligned)
  val s0NonPrefetchMisAligned = (!s0AddrAligned || s0VectorMisaligned && s0Out.bits.vecActive)
  val s0LoadAddrMisaligned = s0Out.bits.isLoad && !s0Out.bits.isPrefetch && s0NonPrefetchMisAligned
  val s0StoreAddrMisaligned = s0Out.bits.isStore && !s0Out.bits.isPrefetch && s0NonPrefetchMisAligned
  s0Out.bits.uop.exceptionVec(loadAddrMisaligned) := s0LoadAddrMisaligned
  s0Out.bits.uop.exceptionVec(storeAddrMisaligned) := s0StoreAddrMisaligned

  when(s0Out.valid && s0Out.bits.isFirstIssue) {
    s0Out.bits.uop.debugInfo.tlbFirstReqTime := GTimer()
  }

  // perf cnt
  val perf_s0DCacheVAddr = toDCache.req.bits.vaddr
  val perf_s0VAddrHighMatch = perf_s0DCacheVAddr(VAddrBits - 1, 12) === s0Out.bits.src(0)
  val perf_s0IsNotVector = !s0Out.bits.isVector
  XSPerfAccumulate("s0StallDCache", s0Out.valid && !toDCache.req.ready)
  XSPerfAccumulate("s0AddrSpecSuccess", s0Out.fire && perf_s0VAddrHighMatch && perf_s0IsNotVector)
  XSPerfAccumulate("s0AddrSpecFailed", s0Out.fire && !perf_s0VAddrHighMatch && perf_s0IsNotVector)
  XSPerfAccumulate("s0AddrSpecSuccessOnce", s0Out.fire  && perf_s0VAddrHighMatch && perf_s0IsNotVector && s0Out.bits.isFirstIssue)
  XSPerfAccumulate("s0AddrSpecFailedOnce", s0Out.fire && !perf_s0VAddrHighMatch && perf_s0IsNotVector && s0Out.bits.isFirstIssue)
  XSPerfAccumulate("s0VecAddrVlenAligned", s0Out.fire  && s0Out.bits.isVector && perf_s0DCacheVAddr(3, 0) === 0.U)
  XSPerfAccumulate("s0VecAddrVlenUnaligned", s0Out.fire && s0Out.bits.isVector && perf_s0DCacheVAddr(3, 0) =/= 0.U)
  XSPerfAccumulate("s0ForwardTLDchannel", s0Out.fire && s0Out.bits.forwardTLDchannel)
  XSPerfAccumulate("s0HardwarePrefetchFire", s0Out.fire && s0Out.bits.isHWPrefetch)
  XSPerfAccumulate("s0SoftwarePrefetchFire", s0Out.fire && s0Out.bits.isSWPrefetch)
  XSPerfAccumulate("s0HardwarePrefetchBlocked", s0Out.valid && !s0Out.ready && s0Out.bits.isHWPrefetch)
  XSPerfAccumulate("s0HardwarePrefetchTotal",   s0Out.fire && s0Out.bits.isPrefetch)

  // Pipeline
  // --------------------------------------------------------------------------------
  // stage 1
  // --------------------------------------------------------------------------------
  // TLB resp (send paddr to dcache)
  val s1RedirectRegWire = Wire(Valid(new Redirect))
  s1RedirectRegWire.valid := GatedValidRegNext(fromCtrl.redirect.valid)
  s1RedirectRegWire.bits  := RegEnable(io.fromCtrl.redirect.bits, io.fromCtrl.redirect.valid)

  val s1MMIOCbo = s1In.bits.uop.fuOpType === LSUOpType.cbo_clean ||
                  s1In.bits.uop.fuOpType === LSUOpType.cbo_flush ||
                  s1In.bits.uop.fuOpType === LSUOpType.cbo_inval
  s1Out.bits.mmio := s1MMIOCbo
  s1Out.bits.atomic := s1MMIOCbo

  val s1HasException = ExceptionNO.partialSelect(s1Out.bits.uop.exceptionVec, params.exceptionOut).asUInt.orR
  val s1FastReplayDelayedError = s1In.bits.delayedError && s1In.bits.isFastReplay
  s1Out.bits.delayedError := s1FastReplayDelayedError

  s1Kill := s1FastReplayDelayedError ||
            s1In.bits.uop.robIdx.needFlush(io.fromCtrl.redirect) ||
            s1In.bits.uop.robIdx.needFlush(s1RedirectRegWire)

  if (env.FPGAPlatform) {
    toDCache.s1_pc := DontCare
  } else {
    toDCache.s1_pc := s1In.bits.uop.pc
  }

  // from tlb
  val s1PAddrDupLsu = Mux(s1In.bits.tlbNoQuery, s1In.bits.paddr, fromTlb.resp.bits.paddr(0))
  val s1PAddrDupDCache = Mux(s1In.bits.tlbNoQuery, s1In.bits.paddr, fromTlb.resp.bits.paddr(1))
  val s1TlbRealValid = !s1In.bits.tlbNoQuery && fromTlb.resp.valid && fromTlb.resp.bits.miss
  val s1MemIdxMatch = fromTlb.resp.bits.memidx.idx === Mux(s1In.bits.isStore, s1In.bits.uop.sqIdx.value, s1In.bits.uop.lqIdx.value)
  when (s1TlbRealValid && s1MemIdxMatch) {
    s1Out.bits.uop.debugInfo.tlbRespTime := GTimer()
  }

  // to Backend
  if (toBackend.issue.isDefined) {
    toBackend.issue.get.valid := s1In.valid && !fromTlb.resp.bits.miss && s1In.bits.isIq
    toBackend.issue.get.bits  := s1In.bits
  }

  // Send TLB feedback to store issue queue
  // Store feedback is generated in s1, sent to Iq in s2
  val s1IqFeedback = Wire(Valid(new RSFeedback))
  val s1IqFeedbackCanGo = s1IqFeedback.valid && !s1In.bits.isVector && !s1In.bits.isMisalignBuf
  s1IqFeedback.valid           := s1In.valid && s1In.bits.isIq && s1In.bits.isStore
  s1IqFeedback.bits.hit        := fromTlb.resp.bits.miss
  s1IqFeedback.bits.flushState := fromTlb.resp.bits.ptwBack
  s1IqFeedback.bits.robIdx     := s1In.bits.uop.robIdx
  s1IqFeedback.bits.sourceType := RSFeedbackType.tlbMiss
  s1IqFeedback.bits.sqIdx      := s1In.bits.uop.sqIdx
  s1IqFeedback.bits.lqIdx      := s1In.bits.uop.lqIdx
  s1IqFeedback.bits.dataInvalidSqIdx := DontCare

  // to Tlb
  toTlb.req_kill := s1Kill || s1FastReplayDelayedError
  toTlb.req.bits.pmp_addr := s1In.bits.paddr

  // to DCache
  toDCache.s1_paddr_dup_lsu := s1PAddrDupLsu
  toDCache.s1_paddr_dup_dcache := s1PAddrDupDCache
  toDCache.s1_kill := s1Kill
  toDCache.s1_kill_data_read := s1Kill

  // to mshr forward
  if (toMissQueue.isDefined) {
    toMissQueue.get.valid := s1Out.valid && s1Out.bits.forwardTLDchannel
    toMissQueue.get.bits.mshrId := s1Out.bits.mshrId
    toMissQueue.get.bits.paddr := s1Out.bits.paddr
  }

  // to lsq forward
  val s1LsqForwardKill = s1Out.bits.tlbMiss ||
                         s1Kill ||
                         s1In.bits.isPrefetch ||
                         s1HasException
  if (toLsq.forward.isDefined) {
    val lsqForward = toLsq.forward.get
    lsqForward.valid := s1Out.valid && !s1LsqForwardKill
    lsqForward.bits.vaddr := s1Out.bits.vaddr
    lsqForward.bits.paddr := s1Out.bits.paddr
    lsqForward.bits.uop   := s1Out.bits.uop
    lsqForward.bits.sqIdx := s1Out.bits.uop.sqIdx
    lsqForward.bits.mask  := s1In.bits.mask
    lsqForward.bits.pc    := s1In.bits.uop.pc // FIXME: remove it

    // pre-calcuate sqIdx mask in s0, then send it to lsq in s1 for forwarding
    val s1SqIdxMask = RegEnable(UIntToMask(s0Out.bits.uop.sqIdx.value, StoreQueueSize), s0Out.fire)
    // to enable load-load, sqIdxMask must be calculated based on ldin.uop
    // If the timing here is not OK, load-load forwarding has to be disabled.
    // Or we calculate sqIdxMask at RS??
    lsqForward.bits.sqIdxMask := s1SqIdxMask

    // to sbuffer forward
    toSBuffer.get := lsqForward
  }

  // to prefetch
  if (params.hasPrefetch) {
    toPrefetch.train.s1PrefetchSpec := s1Out.fire
    if (toPrefetch.trainL1.isDefined) {
      toPrefetch.trainL1.get.s1PrefetchSpec := s1Out.fire
    }
  } else {
    toPrefetch.train.s1PrefetchSpec := false.B
  }

  // override exceptionVec when s1DelayedError is false
  when (!s1FastReplayDelayedError) {
    // generate load exceptions
    s1Out.bits.uop.exceptionVec(loadPageFault) := fromTlb.resp.bits.excp(0).pf.ld && s1In.bits.vecActive && s1TlbRealValid
    s1Out.bits.uop.exceptionVec(loadGuestPageFault) := fromTlb.resp.bits.excp(0).gpf.ld && s1TlbRealValid
    s1Out.bits.uop.exceptionVec(loadAccessFault) := fromTlb.resp.bits.excp(0).af.ld && s1In.bits.vecActive && s1TlbRealValid
    // make dev happy
    val s1LoadFaultException = s1Out.bits.uop.exceptionVec(loadPageFault) ||
                               s1Out.bits.uop.exceptionVec(loadGuestPageFault) ||
                               s1Out.bits.uop.exceptionVec(loadAccessFault)
    when (!s1In.bits.isVector && RegNext(toTlb.req.bits.checkfullva) && s1LoadFaultException) {
      s1Out.bits.uop.exceptionVec(loadAddrMisaligned) := false.B
    }

    // generate store exceptions
    s1Out.bits.uop.exceptionVec(storePageFault) := fromTlb.resp.bits.excp(0).pf.st && s1In.bits.vecActive
    s1Out.bits.uop.exceptionVec(storeGuestPageFault) := fromTlb.resp.bits.excp(0).gpf.st && s1In.bits.vecActive
    s1Out.bits.uop.exceptionVec(storeAccessFault) := fromTlb.resp.bits.excp(0).af.st && s1In.bits.vecActive
    // make dev happy
    val s1StoreFaultException = s1Out.bits.uop.exceptionVec(storePageFault) ||
                                s1Out.bits.uop.exceptionVec(storeAccessFault) ||
                                s1Out.bits.uop.exceptionVec(storeGuestPageFault)
    when (!s1In.bits.isVector && RegNext(toTlb.req.bits.checkfullva) && s1StoreFaultException) {
      s1Out.bits.uop.exceptionVec(storeAddrMisaligned) := false.B
    }
  } .otherwise {
    s1Out.bits.uop.exceptionVec(loadPageFault) := false.B
    s1Out.bits.uop.exceptionVec(loadGuestPageFault) := false.B
    s1Out.bits.uop.exceptionVec(loadAddrMisaligned) := false.B
    s1Out.bits.uop.exceptionVec(loadAccessFault) := s1In.bits.vecActive
  }

  // st-ld violation query
  // make dev happy
  if (toLdu.nukeQuery.isDefined) {
    val s1NukeQueryCanGo = !fromTlb.resp.bits.miss && s1In.bits.isStore && !s1In.bits.isHWPrefetch && !s1In.bits.isMisalignBuf
    val nukeQuery = toLdu.nukeQuery.get
    nukeQuery.valid := s1In.valid && s1NukeQueryCanGo
    nukeQuery.bits.robIdx := s1In.bits.uop.robIdx
    nukeQuery.bits.paddr  := fromTlb.resp.bits.paddr(0)
    nukeQuery.bits.mask   := s1In.bits.mask
    nukeQuery.bits.matchLine := s1In.bits.isVector && s1In.bits.is128bit
  }

  // scalar store and scalar load nuke check, and also other purposes
  if (toLsq.addrUpdate.isDefined) {
    val addrUpdate = toLsq.addrUpdate.get
    addrUpdate.valid := s1In.valid && !s1In.bits.isHWPrefetch && !s1In.bits.isMisalignBuf
    addrUpdate.bits  := s1Out.bits
    addrUpdate.bits.miss := fromTlb.resp.bits.miss
  }

  // if store unit is 128-bits memory access, need match 128-bit
  if (fromSta.isDefined) {
    val s1NukeVec = Wire(Vec(fromSta.get.length, Bool()))
    fromSta.get.zip(s1NukeVec).zipWithIndex.map {
      case ((query, nuke), i) =>
        val match128Bit = query.bits.matchLine || (s1In.bits.isVector && s1In.bits.is128bit)
        val nukePAddrMatch = Mux(
                                match128Bit,
                                s1PAddrDupLsu(PAddrBits - 1, 4) === query.bits.paddr(PAddrBits - 1, 4),
                                s1PAddrDupLsu(PAddrBits - 1, 3) === query.bits.paddr(PAddrBits - 1, 3)
                              )
        val nukeDataMaskMatch = (s1In.bits.mask & query.bits.mask).orR
        val nukedByOlder = isAfter(s1In.bits.uop.robIdx, query.bits.robIdx)
        nuke := query.valid && nukePAddrMatch && nukeDataMaskMatch && nukedByOlder
    }
    s1Out.bits.causeVec(nuke) := s1NukeVec.asUInt.orR && !s1Out.bits.tlbMiss
  }

  // to StoreMisalignBuffer
  if (toStoreMisalignBuf.isDefined) {
    toStoreMisalignBuf.get.valid := s1In.valid && s1In.bits.isMisalignBuf && s1In.bits.isStore &&
                                    GatedValidRegNext(fromCtrl.csrCtrl.hd_misalign_st_enable)
    toStoreMisalignBuf.get.bits  := s1Out.bits
    toStoreMisalignBuf.get.bits.causeVec(tlbMiss) := s2Out.bits.tlbMiss
  }

  XSDebug(s1Out.valid && s1Out.bits.isLoad,
    p"S1: pc ${Hexadecimal(s1Out.bits.uop.pc)}, lId ${Hexadecimal(s1Out.bits.uop.lqIdx.asUInt)}, tlb_miss ${s1Out.bits.tlbMiss}, " +
    p"paddr ${Hexadecimal(s1Out.bits.paddr)}, mmio ${s1Out.bits.mmio}\n"
  )
  XSDebug(s1Out.valid && s1Out.bits.isStore,
    p"S1: pc ${Hexadecimal(s1Out.bits.uop.pc)}, sId ${Hexadecimal(s1Out.bits.uop.sqIdx.asUInt)}, tlb_miss ${s1Out.bits.tlbMiss}, " +
    p"paddr ${Hexadecimal(s1Out.bits.paddr)}, mmio ${s1Out.bits.mmio}\n"
  )

  io.debugLsInfo.s1_robIdx := s1In.bits.uop.robIdx.value
  io.debugLsInfo.s1_isTlbFirstMiss := s1Out.fire &&
                                      s1TlbRealValid &&
                                      fromTlb.resp.bits.debug.isFirstIssue &&
                                      !s1In.bits.isHWPrefetch
  io.debugLsInfo.s1_isLoadToLoadForward := false.B // not support
  io.lsTopdownInfo.s1.robIdx          := s1Out.bits.uop.robIdx.value
  io.lsTopdownInfo.s1.vaddr_valid     := s1Out.valid && s1Out.bits.hasROBEntry
  io.lsTopdownInfo.s1.vaddr_bits      := s1Out.bits.vaddr

  // perf cnt
  XSPerfAccumulate("s1FastReplayDelayedError", s1Out.fire && s1FastReplayDelayedError)

  // Pipeline
  // --------------------------------------------------------------------------------
  // stage 2
  // --------------------------------------------------------------------------------
  // s2: DCache resp
  val s2TriggerDebugMode = RegEnable(s1TriggerDebugMode, s1Out.fire)

  // override exceptionVec
  when (!s2In.bits.delayedError) {
    s2Out.bits.uop.exceptionVec(loadAccessFault) := false.B
    s2Out.bits.uop.exceptionVec(storeAccessFault) := false.B

    val s2TlbUnRelatedExceps = s2In.bits.uop.exceptionVec(loadAddrMisaligned) ||
                               s2In.bits.uop.exceptionVec(storeAddrMisaligned) ||
                               s2In.bits.uop.exceptionVec(breakPoint)
    when (s2In.bits.isPrefetch || s2In.bits.tlbMiss && !s2TlbUnRelatedExceps) {
      s2Out.bits.uop.exceptionVec := 0.U.asTypeOf(s2Out.bits.uop.exceptionVec.cloneType)
    }
  }
  val s2HasException = s2TriggerDebugMode ||
                       ExceptionNO.partialSelect(s2Out.bits.uop.exceptionVec, params.exceptionOut).asUInt.orR

  // from pmp
  val s2Pbmt = RegEnable(fromTlb.resp.bits.pbmt(0), s1Out.fire)
  val s2RealMmio = s2In.bits.mmio || io.fromPmp.mmio || Pbmt.isUncache(s2Pbmt)
  s2Out.bits.mmio := s2RealMmio
  s2Out.bits.af := s2Out.bits.uop.exceptionVec(storeAccessFault)
  s2Out.bits.atomic := s2In.bits.atomic || io.fromPmp.atomic

  val s2StoreAccessFault = s2In.bits.uop.exceptionVec(storeAccessFault)
  val s2PmpRespFault = fromPmp.st
  val s2VectorMMIOFault = s2In.bits.isVector && fromPmp.mmio && RegNext(s1IqFeedback.valid && s1IqFeedback.bits.hit)
  s2Out.bits.uop.exceptionVec(storeAccessFault) := (s2StoreAccessFault || s2PmpRespFault || s2VectorMMIOFault) && s2In.bits.vecActive

  // so bad inst
  val s2BadInst = !s2HasException &&
                  !s2RealMmio &&
                  !s2In.bits.isPrefetch &&
                  !s2In.bits.delayedError

  // from tlb
  s2Out.bits.tlbHandled := !fromTlb.hint.full
  s2Out.bits.tlbId := fromTlb.hint.id
  s2Out.bits.causeVec(tlbMiss) := s2In.bits.tlbMiss && s2BadInst

  // from lsq forward
  val s2ForwardFail = WireInit(false.B)
  if (toLsq.forward.isDefined) {
    s2ForwardFail := fromLsq.get.forward.bits.dataInvalid && RegNext(lsqForward.valid)
    s2Out.bits.causeVec(forwardFail) := s2ForwardFail && s2BadInst
    s2Out.bits.dataInvalidSqIdx := fromLsq.get.forward.bits.dataInvalidSqIdx
    s2Out.bits.lastBeat := s2In.bits.paddr(log2Up(refillBytes))
  }

  // from forward
  val s2FullForward = WireInit(false.B)
  val s2VPMatchFail = WireInit(false.B)
  if (fromLsq.isDefined && fromSBuffer.isDefined) { // FIXME: add a option for forward from one source (lsq or sbuffer)
    val s2ForwardMask = Wire(Vec(params.dataBits/8, Bool()))
    val s2ForwardData = Wire(Vec(params.dataBits/8, UInt(8.W)))
    // merge forward result
    // lsq has higher priority than sbuffer
    s2FullForward := ((~s2ForwardMask.asUInt).asUInt & s2In.bits.mask)
    // generate dataBits/8 Muxs
    val s2SqForward = fromLsq.get.forward.bits.forwardMask.zip(fromLsq.get.forward.bits.forwardData)
    val s2SBufferForward = fromSBuffer.get.bits.forwardMask.zip(fromSBuffer.get.bits.forwardData)
    s2ForwardMask.zip(s2ForwardData).zip(s2SqForward).zip(s2SBufferForward).map {
      case (((forwardMask, forwardData), (sqForwardMask, sqForwardData)), (sbufferForwardMask, sbufferForwardData)) =>
        forwardMask := sqForwardMask || sbufferForwardMask
        forwardData := Mux(sqForwardMask, sqForwardData, sbufferForwardData)
    }
    XSDebug(s2Out.fire && s2Out.bits.isLoad, "[FWD LOAD RESP] pc %x fwd %x(%b) + %x(%b)\n",
      s2In.bits.uop.pc,
      fromLsq.get.forward.bits.forwardData.asUInt, fromLsq.get.forward.bits.forwardMask.asUInt,
      s2ForwardMask.asUInt, s2ForwardData.asUInt
    )
    s2DataFromCache.forwardMask := s2ForwardMask
    s2DataFromCache.forwardData := s2ForwardData

    s2VPMatchFail := (fromLsq.get.forward.bits.matchInvalid || fromSBuffer.get.bits.matchInvalid) && s2BadInst
  }

  // from tld or mshr
  val s2ForwardFromTLDchannelOrMshr = WireInit(false.B)
  if (fromTLDchannel.isDefined) {
    val (s2ForwardFromDchannel, s2ForwardDataFromDchannel) = fromTLDchannel.get.forward(s1Out.valid && s1Out.bits.forwardTLDchannel, s1Out.bits.mshrId, s1Out.bits.paddr)
    val (s2ForwardDataValid, s2ForwardFromMshr, s2ForwardDataFromMshr) = fromMissQueue.get.bits.forward()
    s2ForwardFromTLDchannelOrMshr := s2ForwardDataValid && (s2ForwardFromMshr || s2ForwardFromDchannel)

    s2DataFromCache.forwardDchannel := s2ForwardFromDchannel
    s2DataFromCache.forwardDataDchannel := s2ForwardDataFromDchannel
    s2DataFromCache.forwardMSHR := s2ForwardFromMshr
    s2DataFromCache.forwardDataMSHR := s2ForwardDataFromMshr
    s2DataFromCache.forwardResultValid := s2ForwardFromTLDchannelOrMshr

    XSPerfAccumulate("s2ForwardReq", s2Out.fire && s2Out.bits.forwardTLDchannel)
    XSPerfAccumulate("s2forwardFromTLDchannel", s2Out.fire && s2ForwardFromDchannel)
    XSPerfAccumulate("s2forwardFromTLDchannelOrMshr", s2Out.fire && s2ForwardFromTLDchannelOrMshr)
    XSPerfAccumulate("s2SuccessfullyForwardTLDchannel", s2Out.fire && s2ForwardFromDchannel && s2ForwardDataValid)
    XSPerfAccumulate("s2SuccessfullyForwardMshr", s2Out.fire && s2ForwardFromMshr && s2ForwardDataValid)
  }

  // from DCache
  val s2DCacheFastReplay = WireInit(false.B)
  if (params.hasDCacheQuery) {
    // missqueue nack check
    val s2MissQueueNAck = fromDCache.s2_mq_nack &&
                          !s2ForwardFromTLDchannelOrMshr &&
                          !s2FullForward
    s2Out.bits.causeVec(dcacheReplay) := s2MissQueueNAck && s2BadInst
    s2Out.bits.mshrHandled := fromDCache.resp.bits.handled
    s2Out.bits.mshrId := fromDCache.resp.bits.mshr_id

    // dcache miss check
    val s2DCacheMiss = fromDCache.resp.bits.miss &&
                       !s2ForwardFromTLDchannelOrMshr &&
                       !s2FullForward
    s2Out.bits.causeVec(dcacheMiss) := s2DCacheMiss && s2BadInst
    s2Out.bits.miss := s2DCacheMiss && s2BadInst
    s2Out.bits.replayCarry := fromDCache.resp.bits.replayCarry

    // dcache dataArray bank conflict
    val s2BankConflict = fromDCache.s2_bank_conflict &&
                         !s2ForwardFromTLDchannelOrMshr &&
                         !s2FullForward
    s2Out.bits.causeVec(bankConflict) := s2BankConflict && s2BadInst

    // wpu predict fail
    val s2WPUPredFail = fromDCache.s2_wpu_pred_fail &&
                        !s2ForwardFromTLDchannelOrMshr &&
                        !s2FullForward
    s2Out.bits.causeVec(wpuPredictFail) := s2WPUPredFail && s2BadInst

    s2DCacheFastReplay := (s2MissQueueNAck || !s2DCacheMiss && (s2BankConflict || s2WPUPredFail))
  }

  // from StoreQueue mdp
  val s2MemAmbiguous = WireInit(false.B)
  if (toLsq.forward.isDefined) {
    s2MemAmbiguous := s2In.bits.uop.storeSetHit &&
                      fromLsq.get.forward.bits.addrInvalid &&
                      RegNext(lsqForward.valid)
    s2Out.bits.causeVec(memoryAmbiguous) := s2MemAmbiguous && s2BadInst
    s2Out.bits.addrInvalidSqIdx := fromLsq.get.forward.bits.addrInvalidSqIdx
  }

 // if store unit is 128-bits memory access, need match 128-bit
  val s2NukeFastReplay = WireInit(false.B)
  if (fromSta.isDefined) {
    val s2NukeVec = Wire(Vec(fromSta.get.length, Bool()))
    fromSta.get.zip(s2NukeVec).zipWithIndex.map {
      case ((query, nuke), i) =>
        val match128Bit = query.bits.matchLine || (s2In.bits.isVector && s2In.bits.is128bit)
        val nukePAddrMatch = Mux(
                                match128Bit,
                                s1PAddrDupLsu(PAddrBits - 1, 4) === query.bits.paddr(PAddrBits - 1, 4),
                                s1PAddrDupLsu(PAddrBits - 1, 3) === query.bits.paddr(PAddrBits - 1, 3)
                              )
        val nukeDataMaskMatch = (s2In.bits.mask & query.bits.mask).orR
        val nukedByOlder = isAfter(s2In.bits.uop.robIdx, query.bits.robIdx)
        nuke := query.valid && nukePAddrMatch && nukeDataMaskMatch && nukedByOlder
    }
    s2Out.bits.causeVec(nuke) := s2NukeVec.asUInt.orR && !s2In.bits.tlbMiss && s2BadInst || s2In.bits.causeVec(nuke)
    s2NukeFastReplay := !s2Out.bits.causeVec.drop(3).dropRight(1).reduce(_&_) && s2Out.bits.causeVec(nuke)
  }


  // to DCache
  if (env.FPGAPlatform) {
    toDCache.s2_pc := DontCare
  } else {
    toDCache.s2_pc := s1In.bits.uop.pc
  }
  toDCache.s2_kill := fromPmp.ld || s2RealMmio || s2Kill

  // to lsq
  if (toLsq.rarNuke.isDefined && toLsq.rawNuke.isDefined) {
    val s2CanQuery = !s2MemAmbiguous &&
                     !s2In.bits.tlbMiss &&
                     !s2ForwardFail &&
                     !s2In.bits.isMisalignBuf &&
                     s2BadInst
    toLsq.rarNuke.get.req.valid := s2In.valid && s2CanQuery
    toLsq.rarNuke.get.req.bits.uop   := s2In.bits.uop
    toLsq.rarNuke.get.req.bits.mask  := s2In.bits.mask
    toLsq.rarNuke.get.req.bits.paddr := s2In.bits.paddr
    toLsq.rarNuke.get.req.bits.dataValid := s2FullForward || s2ForwardFromTLDchannelOrMshr || !fromDCache.resp.bits.miss

    toLsq.rawNuke.get.req.valid := toLsq.rarNuke.get.req.valid
    toLsq.rawNuke.get.req.bits  := toLsq.rarNuke.get.req.bits

    val s2RarNAck = toLsq.rarNuke.get.req.valid &&
                    !toLsq.rarNuke.get.req.ready
    s2Out.bits.causeVec(rarNack) := s2RarNAck && s2BadInst

    val s2RawNAck = toLsq.rawNuke.get.req.valid &&
                    !toLsq.rawNuke.get.req.ready
    s2Out.bits.causeVec(rawNack) := s2RawNAck && s2BadInst
  }

  if (toLsq.excpUpdate.isDefined) {
    toLsq.excpUpdate.get.valid := s2In.valid && s2In.bits.isStore
    toLsq.excpUpdate.get.bits  := s2Out.bits
    toLsq.excpUpdate.get.bits.af := s2Out.bits.af && !s2Kill
    toLsq.excpUpdate.get.bits.miss := fromDCache.resp.fire && fromDCache.resp.bits.miss
  }

  // to prefetch
  val s2PfTrainValid = WireInit(false.B)
  val s2PfTrainL1Valid = WireInit(false.B)
  if (params.hasPrefetch) {
    s2PfTrainValid := s2In.valid && !s2RealMmio && (!s2In.bits.tlbMiss || s2In.bits.isHWPrefetch)
    s2PfTrainL1Valid := s2In.valid && !s2RealMmio

    toPrefetch.train.s2PrefetchSpec := s2PfTrainValid

    if (toPrefetch.trainL1.isDefined) {
      toPrefetch.trainL1.get.s2PrefetchSpec := s2PfTrainValid
    }

    XSPerfAccumulate("s2Prefetch", s2Out.fire && s2Out.bits.isPrefetch)
    XSPerfAccumulate("s2PrefetchIgnored", s2Out.fire && s2Out.bits.isPrefetch && fromDCache.s2_mq_nack) // ignore prefetch for mshr full / miss req port conflict
    XSPerfAccumulate("s2PrefetchMiss", s2Out.fire && s2Out.bits.isPrefetch && fromDCache.resp.bits.miss) // prefetch req miss in l1
    XSPerfAccumulate("s2PrefetchHit",  s2Out.fire && s2Out.bits.isPrefetch && !fromDCache.resp.bits.miss) // prefetch req hit in l1
    XSPerfAccumulate("s2PrefetchAccept", s2Out.fire && s2Out.bits.isPrefetch && fromDCache.resp.bits.miss && !fromDCache.s2_mq_nack) // prefetch a missed line in l1, and l1 accepted it
  }

  // to Backend iqFeedback
  if (toBackend.iqFeedback.isDefined) {
    val iqFeedback = toBackend.iqFeedback.get
    iqFeedback.valid := GatedValidRegNext(s1IqFeedbackCanGo)
    iqFeedback.bits := RegEnable(s1IqFeedback.bits, s1IqFeedbackCanGo)
  }

  //  to MisalignBuf writeback
  toIssue.zip(s2WBPort).map {
    case (wb, select) =>
      wb.valid := s2Out.valid && s2Out.bits.isMisalignBuf && s2Out.bits.isStore
      wb.bits  := s2Out.bits
      when (select && s2Out.bits.isMisalignBuf && s2Out.bits.isStore ) {
        s2Out.ready := wb.ready
      }
  }

  // out valid
  val s2MisalignException = WireInit(false.B)
  val s2Misalign = !s2In.bits.isVector &&
                    s2MisalignException &&
                    !s2Out.bits.uop.exceptionVec(breakPoint) &&
                    !RegEnable(s1TriggerDebugMode, false.B, s1Out.fire)

  when (s2In.bits.isStore) {
    s2MisalignException := s2Out.bits.uop.exceptionVec(storeAddrMisaligned) &&
                           GatedValidRegNext(fromCtrl.csrCtrl.hd_misalign_st_enable)
    s2Out.valid := s2In.valid &&
                   (!s2RealMmio || !s2HasException) &&
                   !s2In.bits.isHWPrefetch &&
                   !s2In.bits.isMisalignBuf &&
                   !s2Misalign
  } .otherwise {
    s2MisalignException := s2Out.bits.uop.exceptionVec(loadAddrMisaligned) &&
                           GatedValidRegNext(fromCtrl.csrCtrl.hd_misalign_ld_enable)
    s2Out.valid := s2In.valid && !s2In.bits.isHWPrefetch
  }

  val s2CanFastReplay = !s2Out.bits.causeVec.take(3).reduce(_&_) &&
                         (s2DCacheFastReplay || s2NukeFastReplay)
  val s2SafeWakeup = !s2Out.bits.needReplay && !s2RealMmio && !s2Misalign && s2HasException

  // out uop
  s2Out.bits.uop.flushPipe := false.B
  when (s2In.bits.isLoadReplay || s2In.bits.isFastReplay) {
    s2Out.bits.uop.vpu.vstart := s2In.bits.uop.vpu.vstart
  }

  // data out
  s2DataFromCache.uop         := s2Out.bits.uop
  s2DataFromCache.addrOffset  := s2Out.bits.paddr(3, 0)

  io.debugLsInfo.s2_robIdx := s2Out.bits.uop.robIdx.value
  io.debugLsInfo.s2_isBankConflict := s2Out.fire
  io.debugLsInfo.s2_isDcacheFirstMiss := s2Out.fire
  io.debugLsInfo.s2_isForwardFail := s2Out.fire

  // Topdown
  io.lsTopdownInfo.s2.robIdx          := s2Out.bits.uop.robIdx.value
  io.lsTopdownInfo.s2.paddr_valid     := s2Out.fire && s2Out.bits.hasROBEntry && !s2Out.bits.tlbMiss
  io.lsTopdownInfo.s2.paddr_bits      := s2Out.bits.paddr
  io.lsTopdownInfo.s2.first_real_miss := fromDCache.resp.bits.miss
  io.lsTopdownInfo.s2.cache_miss_en   := s2Out.fire && s2Out.bits.hasROBEntry && !s2Out.bits.tlbMiss && !s2Out.bits.missDbUpdated

  XSPerfAccumulate("s2DCacheMiss", s2Out.fire && fromDCache.resp.bits.miss)
  XSPerfAccumulate("s2DCacheMissFirstIssue", s2Out.fire && s2Out.bits.miss && s2In.bits.isFirstIssue)
  XSPerfAccumulate("s2DCacheRealMissFirstIssue", s2Out.fire&& fromDCache.resp.bits.miss && s2In.bits.isFirstIssue)
  XSPerfAccumulate("s2FullForward", s2Out.fire && s2FullForward)
  XSPerfAccumulate("s2DCacheMissFullForward", s2Out.fire && fromDCache.resp.bits.miss && s2FullForward)

  // Pipeline
  // --------------------------------------------------------------------------------
  // stage 3
  // --------------------------------------------------------------------------------
  // writeback and update load queue
  val s3BadInst = GatedValidRegNext(s2BadInst)
  val s3HasException = GatedValidRegNext(s2HasException)

  // set delay error
  val s3DelayedError = WireInit(false.B)
  if (params.hasDCacheQuery) {
    s3DelayedError := fromDCache.resp.bits.error_delayed && s3BadInst &&
                      GatedValidRegNext(fromCtrl.csrCtrl.cache_error_enable)
  }

  // to fast replay
  val s3FastReplayCancelled = WireInit(false.B)
  val s3CanFastReplay = RegEnable(s2CanFastReplay, false.B, s2Out.fire)
  if (toLdu.replay.isDefined) {
    val replay = toLdu.replay.get
    replay.valid := s3Out.valid && s3CanFastReplay && !s3Kill
    replay.bits  := s3Out.bits
    replay.bits.delayedError := s3DelayedError

    s3FastReplayCancelled := replay.valid && replay.bits.forwardTLDchannel || !toDCache.req.ready
  }

  // to lsq
  if (toLsq.out.isDefined) {
    val lsqOut = toLsq.out.get
    lsqOut.valid := s3Out.valid &&
                    (!s3CanFastReplay || s3FastReplayCancelled) &&
                    !s3In.bits.feedbacked &&
                    !s3In.bits.isMisalignBuf
    lsqOut.bits := s3Out.bits
    lsqOut.bits.replacementUpdated  := fromDCache.resp.bits.replacementUpdated

    // override replay causeVec
    val s3SelectReplayCauseOH = PriorityEncoderOH(s3Out.bits.causeVec.asUInt)
    when (s3HasException || s3DelayedError || s3VPMatchFail) {
      lsqOut.bits.causeVec := 0.U.asTypeOf(lsqOut.bits.causeVec)
    } .otherwise {
      lsqOut.bits.causeVec := VecInit(s3SelectReplayCauseOH.asBools)
    }

    io.debugLsInfo.s3_isReplay := lsqOut.fire && lsqOut.bits.needReplay
    io.debugLsInfo.replayCause := lsqOut.bits.causeVec
    io.debugLsInfo.replayCnt   := 1.U
  } else {
    io.debugLsInfo.s3_isReplay := DontCare
    io.debugLsInfo.replayCause := DontCare
    io.debugLsInfo.replayCnt   := DontCare
  }

  // to LoadMisalignBuffer
  if (toLoadMisalignBuf.isDefined) {
    toLoadMisalignBuf.get.valid := s3Out.valid &&
                                   (!s3CanFastReplay || s3FastReplayCancelled) &&
                                   !s3In.bits.feedbacked &&
                                   !s3In.bits.isMisalignBuf &&
                                   !s3In.bits.isVector &&
                                   GatedValidRegNext(fromCtrl.csrCtrl.hd_misalign_ld_enable)
    toLoadMisalignBuf.get.bits := s3Out.bits
  }

  // to Backend rollback
  val s3FlushPipe = WireInit(false.B)
  if (fromLsq.isDefined) {
    s3FlushPipe := fromLsq.get.rarNuke.valid &&
                   fromLsq.get.rarNuke.bits.replayFromFetch &&
                   GatedValidRegNext(fromCtrl.csrCtrl.ldld_vio_check_enable)
  }

  val s3VPMatchFail = GatedValidRegNext(s2VPMatchFail)
  if (toBackend.rollback.isDefined) {
    val rollback = toBackend.rollback.get
    rollback.valid := s3Out.valid && (s3VPMatchFail || s3FlushPipe) && s3Out.bits.isLoad && s3HasException
    rollback.bits  := DontCare
    rollback.bits.isRVC     := s3Out.bits.uop.preDecodeInfo.isRVC
    rollback.bits.robIdx    := s3Out.bits.uop.robIdx
    rollback.bits.ftqIdx    := s3Out.bits.uop.ftqPtr
    rollback.bits.ftqOffset := s3Out.bits.uop.ftqOffset
    rollback.bits.level     := Mux(s3VPMatchFail, RedirectLevel.flush, RedirectLevel.flushAfter)
    rollback.bits.cfiUpdate.target := s3Out.bits.uop.pc
    rollback.bits.debug_runahead_checkpoint_id := s3Out.bits.uop.debugInfo.runahead_checkpoint_id
  }

  // to Backend ldCancel
  val s3SafeWakeup = RegEnable(s2SafeWakeup, false.B, s2Out.fire)
  if (toBackend.ldCancel.isDefined) {
    val ldCancel = toBackend.ldCancel.get
    ldCancel.ld1Cancel := false.B
    ldCancel.ld2Cancel := s3Out.valid && s3SafeWakeup && s3Out.bits.isVector && !s3Out.bits.isMisalignBuf && s3Out.bits.isLoad
  }

  // to prefetch
  if (toPrefetch.train.isDefined) {
    val pfTrain = toPrefetch.train.get
    pfTrain.req.valid := GatedValidRegNext(s2PfTrainValid)
    pfTrain.req.bits.fromLsPipelineBundle(s2In.bits, latch = true, enable = s2PfTrainValid)
    pfTrain.req.bits.miss := RegEnable(fromDCache.resp.bits.miss, s2PfTrainValid)
    pfTrain.req.bits.metaPrefetch := RegEnable(fromDCache.resp.bits.meta_prefetch, s2PfTrainValid)
    pfTrain.req.bits.metaAccess := RegEnable(fromDCache.resp.bits.meta_access, s2PfTrainValid)
  }

  if (toPrefetch.trainL1.isDefined) {
    val pfTrainL1 = toPrefetch.trainL1.get
    pfTrainL1.req.valid := GatedValidRegNext(s2PfTrainL1Valid)
    pfTrainL1.req.bits.fromLsPipelineBundle(s2In.bits, latch = true, enable = s2PfTrainL1Valid)
    pfTrainL1.req.bits.miss := RegEnable(fromDCache.resp.bits.miss, s2PfTrainL1Valid)
    pfTrainL1.req.bits.metaPrefetch := RegEnable(fromDCache.resp.bits.meta_prefetch, s2PfTrainL1Valid)
    pfTrainL1.req.bits.metaAccess := RegEnable(fromDCache.resp.bits.meta_access, s2PfTrainL1Valid)
  }

  // generate writeback data
  val s3DataFromSqAndSBuffer = RegEnable(s2DataFromCache, s2Out.valid)
  val s3MergedDataFromTLDchannel = RegEnable(s2DataFromCache.mergeTLData(), s2Out.valid)
  val s3MergedDataFromCache = s3DataFromSqAndSBuffer.mergeLsqFwdData(s3MergedDataFromTLDchannel)

  // truncate forward data and cache data to XLEN width to writeback
  val s3ForwardMaskClip = VecInit(List.fill(LdDataDup)(
    RegEnable(Mux(
      s2Out.bits.paddr(3),
      (s2DataFromCache.forwardMask.asUInt)(VLEN / 8 - 1, 8),
      (s2DataFromCache.forwardMask.asUInt)(7, 0)
    ).asTypeOf(Vec(XLEN / 8, Bool())), s2Out.valid)
  ))
  val s3ForwardDataClip = VecInit(List.fill(LdDataDup)(
    RegEnable(Mux(
      s2Out.bits.paddr(3),
      (s2DataFromCache.forwardData.asUInt)(VLEN - 1, 64),
      (s2DataFromCache.forwardData.asUInt)(63, 0)
    ).asTypeOf(Vec(XLEN / 8, UInt(8.W))), s2Out.valid)
  ))
  val s3MergedDataFromTLDchannelClip = VecInit(List.fill(LdDataDup)(
    RegEnable(Mux(
      s2Out.bits.paddr(3),
      s2DataFromCache.mergeTLData()(VLEN - 1, 64),
      s2DataFromCache.mergeTLData()(63, 0)
    ).asTypeOf(Vec(XLEN / 8, UInt(8.W))), s2Out.valid)
  ))
  val s3MergedDataFromCacheClip = VecInit((0 until LdDataDup).map(i => {
    VecInit((0 until XLEN / 8).map(j =>
      Mux(s3ForwardMaskClip(i)(j), s3ForwardDataClip(i)(j), s3MergedDataFromTLDchannelClip(i)(j))
    )).asUInt
  }))
  val s3DataFromCache = VecInit((0 until LdDataDup).map(i => {
    VecInit(Seq(
      s3MergedDataFromCacheClip(i)(63,    0),
      s3MergedDataFromCacheClip(i)(63,    8),
      s3MergedDataFromCacheClip(i)(63,   16),
      s3MergedDataFromCacheClip(i)(63,   24),
      s3MergedDataFromCacheClip(i)(63,   32),
      s3MergedDataFromCacheClip(i)(63,   40),
      s3MergedDataFromCacheClip(i)(63,   48),
      s3MergedDataFromCacheClip(i)(63,   56),
    ))
  }))
  val s3PickedDataFromCache = VecInit((0 until LdDataDup).map(i => {
    Mux1H(s3DataSelectOffset, s3DataFromCache(i))
  }))
  s3Out.bits.data := newRdataHelper(s3RdataOH, s3PickedDataFromCache(0))

  // writeback
  s3Out.ready := false.B
  toIssue.zip(s3WBPort).map {
    case (wb, select) =>
      wb.valid := s3Out.valid && select && s3Out.bits.isLoad && s3SafeWriteback
      wb.bits := s3Out.bits
      wb.bits.uop.exceptionVec(loadAccessFault) := (s3DelayedError || s3Out.bits.uop.exceptionVec(loadAccessFault))
      wb.bits.uop.flushPipe  := false.B
      wb.bits.uop.replayInst := s3VPMatchFail || s3FlushPipe
      wb.bits.isFromLoadUnit := true.B
      when (select) {
        s3Out.ready := wb.ready
      }
  }

  io.debugLsInfo.s3_robIdx := s3Out.bits.uop.robIdx.value
  io.debugLsInfo.s3_isReplayFast := s3Out.fire && s3Out.bits.isFastReplay
  io.debugLsInfo.s3_isReplayRS := s3Out.fire && s3Out.bits.isIq
  io.debugLsInfo.s3_isReplaySlow := s3Out.fire && s3Out.bits.isLoadReplay

  // Pipeline
  // --------------------------------------------------------------------------------
  // stage x
  // --------------------------------------------------------------------------------
  // delay TotalSelectCycles - 2 cycle(s)
  val pipelineRegs = Seq.fill(TotalDelayCycles)(Module(new NewPipelineConnectPipe(new LsPipelineBundle)))
  val wbPortRegs = DelayN(s3WBPort, TotalDelayCycles)

  val exuOut = Wire(DecoupledIO(new LsPipelineBundle))
  if (pipelineRegs.length > 0) {
    pipelineRegs.head.io.in <> s3Out
    pipelineRegs.head.io.in.valid := s3Out.valid && s3Out.bits.isStore
    pipelineRegs.head.io.isFlush := s3Out.bits.uop.robIdx.needFlush(io.fromCtrl.redirect)
    pipelineRegs.head.io.rightOutFire := pipelineRegs.head.io.out.fire
    pipelineRegs.dropRight(1).zip(pipelineRegs.drop(1)).map {
      case (source, sink) =>
        val isFlush = source.io.out.bits.uop.robIdx.needFlush(io.fromCtrl.redirect)
        sink.io.in <> source.io.out
        sink.io.isFlush := isFlush
        sink.io.rightOutFire := sink.io.out.fire
    }
    exuOut <> pipelineRegs.last.io.out
  } else {
    exuOut <> s3Out
  }

  toIssue.zip(wbPortRegs).map {
    case (wb, select) =>
      wb.valid := exuOut.valid && select
      wb.bits  := exuOut.bits
      wb.bits.data := DontCare
      when (select && exuOut.bits.isStore) {
        exuOut.ready := wb.ready
      }
  }
}
