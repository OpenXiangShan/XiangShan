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
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.diplomacy.{BundleBridgeSource, LazyModule, LazyModuleImp}
import xiangshan._
import xiangshan.ExceptionNO._
import xiangshan.backend.Bundles.{DynInst, MemExuInput, MemExuOutput}
import xiangshan.backend.fu.PMPRespBundle
import xiangshan.backend.fu.NewCSR._
import xiangshan.backend.fu.{FuType}
import xiangshan.backend.ctrlblock.{DebugLsInfoBundle, LsTopdownInfo}
import xiangshan.backend.rob.RobLsqIO
import xiangshan.mem.Bundles._
import xiangshan.cache._
import xiangshan.cache.{HasDCacheParameters, StorePrefetchReq}
import xiangshan.cache.mmu.{TlbReq, TlbResp, TlbCmd, TlbHintReq}
import xiangshan.cache.{DCacheLoadReqIO, DCacheLoadRespIO}

class MemExuBlock(implicit p: Parameters) extends LazyModule
  with HasXSParameter
  with HasMemBlockParameters
{
  val stdUnits = memUnitParams.filter(_.isStoreDataUnit).map(
    params => LazyModule(new MemUnit(params).suggestName(params.name))
  )
  require(stdUnits.length == StdCnt, "The number of STD should be match StdCnt!")

  val staUnits = memUnitParams.filter(_.isStoreAddrUnit).map(
    params => LazyModule(new MemUnit(params).suggestName(params.name))
  )
  require(staUnits.length == StaCnt, "The number of STA should be match StaCnt!")

  val loadUnits = memUnitParams.filter(_.isLoadUnit).map(
    params => LazyModule(new MemUnit(params).suggestName(params.name))
  )
  require(loadUnits.length == LduCnt, "The number of LOAD should be match LduCnt!")

  val hybridUnits = memUnitParams.filter(_.isHybridUnit).map(
    params => LazyModule(new MemUnit(params).suggestName(params.name))
  )
  require(hybridUnits.length == HyuCnt, "The number of HYBRID should be match HyuCnt!")

  val atomicsUnits = memUnitParams.filter(_.isAtomicsUnit).map(
    params => LazyModule(new MemUnit(params).suggestName(params.name))
  )
  require(atomicsUnits.length == 1, "Only one atomicsUnit!")

  lazy val module = new MemExuBlockImp(this)
}

class BackendToMemExuBlockIO(implicit p: Parameters) extends MemBlockBundle {
  val issueLda = MixedVec(Seq.fill(LduCnt)(Flipped(DecoupledIO(new MemExuInput))))
  val issueSta = MixedVec(Seq.fill(StaCnt)(Flipped(DecoupledIO(new MemExuInput))))
  val issueStd = MixedVec(Seq.fill(StdCnt)(Flipped(DecoupledIO(new MemExuInput))))
  val issueHya = MixedVec(Seq.fill(HyuCnt)(Flipped(DecoupledIO(new MemExuInput))))
  val rob      = Flipped(new RobLsqIO)
}

class MemExuBlockToBackendIO(implicit p: Parameters) extends MemBlockBundle {
  val writebackLda = Vec(LduCnt, DecoupledIO(new MemExuOutput))
  val writebackSta = Vec(StaCnt, DecoupledIO(new MemExuOutput))
  val writebackStd = Vec(StdCnt, DecoupledIO(new MemExuOutput))
  val writebackHyuLda = Vec(HyuCnt, DecoupledIO(new MemExuOutput))
  val writebackHyuSta = Vec(HyuCnt, DecoupledIO(new MemExuOutput))

  val ldaIqFeedback  = Vec(LduCnt, new MemRSFeedbackIO)
  val staIqFeedback  = Vec(StaCnt, new MemRSFeedbackIO)
  val hyuIqFeedback  = Vec(HyuCnt, new MemRSFeedbackIO)

  val ldCancel = Vec(LdExuCnt, new LoadCancelIO)
  val wakeup = Vec(LdExuCnt, Valid(new DynInst))
  val rollback = Vec(LdExuCnt, ValidIO(new Redirect))
  val stIssue  = Vec(StAddrCnt, ValidIO(new MemExuInput))
}

class MemExuBlockToLsqIO(implicit p: Parameters) extends MemBlockBundle {
  val out      = Vec(LdExuCnt, DecoupledIO(new LsPipelineBundle))
  val forward  = Vec(LdExuCnt, ValidIO(new LoadForwardReqBundle))
  val rawNuke  = Vec(LdExuCnt, new Bundle() {
    val req = DecoupledIO(new LoadNukeQueryReqBundle)
    val revoke = Output(Bool())
  })
  val rarNuke  = Vec(LdExuCnt, new Bundle() {
    val req = DecoupledIO(new LoadNukeQueryReqBundle)
    val revoke = Output(Bool())
  })
  val addrUpdate = Vec(StAddrCnt, ValidIO(new LsPipelineBundle))
  val excpUpdate = Vec(StAddrCnt, ValidIO(new LsPipelineBundle))
  val maskOut    = Vec(StAddrCnt, ValidIO(new StoreMaskBundle))
  val maControl  = new StoreMaBufToSqCtrlIO
  val flushFromMaBuf = Output(Bool())
}

class LsqToMemExuBlockIO(implicit p: Parameters) extends MemBlockBundle {
  val forward  = Vec(LdExuCnt, ValidIO(new LoadForwardRespBundle))
  val rarNuke  = Vec(LdExuCnt, ValidIO(new LoadNukeQueryRespBundle))
  val rawNuke  = Vec(LdExuCnt, ValidIO(new LoadNukeQueryRespBundle))
  val replay   = Vec(LdExuCnt, DecoupledIO(new LsPipelineBundle))
  val uncache  = Vec(LdExuCnt, DecoupledIO(new LsPipelineBundle))
  val flushFinish = Input(Bool())
}

class MemExuBlockIO(implicit p: Parameters) extends MemBlockBundle {
  // from
  val fromCtrl = new Bundle () {
    val hartId    = Input(UInt(hartIdLen.W))
    val redirect  = Flipped(ValidIO(new Redirect))
    val csrCtrl   = Flipped(new CustomCSRCtrlIO)
    val trigger   = Input(new CsrTriggerBundle)
  }

  val fromBackend = new BackendToMemExuBlockIO
  val fromVecExuBlock = Flipped(new toMemExuBlockIO)
  val fromLsq     = Flipped(new LsqToMemExuBlockIO)
  val fromTlb     = Vec(MemAddrExtCnt, new Bundle() {
    val resp = Flipped(DecoupledIO(new TlbResp(2)))
    val hint = Flipped(new TlbHintReq)
  })
  val fromSBuffer    = Vec(LdExuCnt, Flipped(ValidIO(new LoadForwardRespBundle)))
  val fromMissQueue  = Vec(LdExuCnt, Flipped(ValidIO(new MissQueueForwardRespBundle)))
  val fromTLDchannel = Vec(LdExuCnt, Input(new DcacheToLduForwardIO))
  val fromDCache = Vec(MemAddrExtCnt, new DCacheLoadRespIO)
  val fromPmp  = Vec(MemAddrExtCnt,  Flipped(new PMPRespBundle()))
  val fromPrefetch = Vec(MemAddrExtCnt, Flipped(DecoupledIO(new LsPipelineBundle)))

  // to
  val toBackend   = new MemExuBlockToBackendIO
  val toVecExuBlock = Flipped(new fromMemExuBlockIO)
  val toLsq       = new MemExuBlockToLsqIO
  val toDCache    = Vec(MemAddrExtCnt, new DCacheLoadReqIO)
  val toTlb       = Vec(MemAddrExtCnt, new Bundle() {
    val req = DecoupledIO(new TlbReq)
    val req_kill = Output(Bool())
  })
  val toSBuffer   = Vec(LdExuCnt, ValidIO(new LoadForwardReqBundle))
  val toMissQueue = Vec(LdExuCnt, ValidIO(new MissQueueForwardReqBundle))
  val toPrefetch  = new Bundle() {
    val ifetch  = Vec(LdExuCnt, ValidIO(new SoftIfetchPrefetchBundle))
    val train   = Vec(LdExuCnt + StaCnt, new LsPrefetchTrainIO)
    val trainL1 = Vec(LdExuCnt, new LsPrefetchTrainIO)
  }
  val flushSbuffer = new SbufferFlushBundle
  val amoDCacheIO = new AtomicWordIO
  val atomicsExceptionInfo = ValidIO(new ExceptionAddrIO)
  val misalignExceptionInfo = ValidIO(new ExceptionAddrIO)
  val debugLsInfo = Vec(MemAddrExtCnt, Output(new DebugLsInfoBundle))
  val lsTopdownInfo = Vec(LdExuCnt, Output(new LsTopdownInfo))
}

class MemExuBlockImp(wrapper: MemExuBlock) extends LazyModuleImp(wrapper)
  with HasPerfEvents
  with HasXSParameter
  with HasMemBlockParameters
{
  val io = IO(new MemExuBlockIO)

  private val fromCtrl = io.fromCtrl
  private val fromTLDchannel = io.fromTLDchannel
  private val fromPmp = io.fromPmp
  private val (fromBackend,     toBackend)  = (io.fromBackend, io.toBackend)
  private val (fromLsq,         toLsq)      = (io.fromLsq, io.toLsq)
  private val (fromTlb,         toTlb)      = (io.fromTlb, io.toTlb)
  private val (fromSBuffer,     toSBuffer)  = (io.fromSBuffer, io.toSBuffer)
  private val (fromMissQueue,   toMissQueue) = (io.fromMissQueue, io.toMissQueue)
  private val (fromDCache,      toDCache)   = (io.fromDCache, io.toDCache)
  private val (fromPrefetch,    toPrefetch) = (io.fromPrefetch, io.toPrefetch)

  val stdUnitImps     = wrapper.stdUnits.map(_.module)
  val staUnitImps     = wrapper.staUnits.map(_.module.asInstanceOf[HybridUnitImp])
  val loadUnitImps    = wrapper.loadUnits.map(_.module.asInstanceOf[HybridUnitImp])
  val hybridUnitImps  = wrapper.hybridUnits.map(_.module.asInstanceOf[HybridUnitImp])
  val atomicsUnitImps = wrapper.atomicsUnits.map(_.module)

  val loadMisalignBuffer = Module(new LoadMisalignBuffer)
  val storeMisalignBuffer = Module(new StoreMisalignBuffer)

  private val totalStaUnits  = staUnitImps ++ hybridUnitImps
  private val totalLoadUnits = loadUnitImps ++ hybridUnitImps
  private val totalMemUnits  = loadUnitImps ++ hybridUnitImps ++ staUnitImps
  val hartId = p(XSCoreParamsKey).HartId
  val correctMissTrain = Constantin.createRecord(s"CorrectMissTrain$hartId", initValue = false)

  // stdUnits
  stdUnitImps.zipWithIndex.foreach {
    case (impl: StoreDataUnitImp, i) =>
      impl.io.fromCtrl.redirect <> fromCtrl.redirect
      impl.io.fromCtrl.hartId   <> fromCtrl.hartId
      impl.io.fromCtrl.csrCtrl  <> fromCtrl.csrCtrl
      impl.io.fromTlb    := DontCare
      impl.io.fromDCache := DontCare
      impl.io.fromPmp    := DontCare
      impl.io.toDCache.req.ready := false.B
      impl.io.toTlb.req.ready := false.B
    case _ =>
  }

  // staUnits
  staUnitImps.zipWithIndex.foreach {
    case (impl: HybridUnitImp, i) =>
      impl.io.fromCtrl.redirect <> fromCtrl.redirect
      impl.io.fromCtrl.hartId   <> fromCtrl.hartId
      impl.io.fromCtrl.csrCtrl  <> fromCtrl.csrCtrl
    case _ =>
  }

  // loadUnits
  loadUnitImps.zipWithIndex.foreach {
    case (impl: HybridUnitImp, i) =>
      impl.io.fromCtrl.redirect <> fromCtrl.redirect
      impl.io.fromCtrl.hartId   <> fromCtrl.hartId
      impl.io.fromCtrl.csrCtrl  <> fromCtrl.csrCtrl

      if (impl.io.fromCtrl.trigger.isDefined) {
        impl.io.fromCtrl.trigger.get <> fromCtrl.trigger
      }
    case _ =>
  }

  // hybridUnits
  hybridUnitImps.zipWithIndex.foreach {
    case (impl: HybridUnitImp, i) =>
      impl.io.fromCtrl.redirect <> fromCtrl.redirect
      impl.io.fromCtrl.hartId   <> fromCtrl.hartId
      impl.io.fromCtrl.csrCtrl  <> fromCtrl.csrCtrl

      if (impl.io.fromCtrl.trigger.isDefined) {
        impl.io.fromCtrl.trigger.get <> fromCtrl.trigger
      }
    case _ =>
  }

  atomicsUnitImps.zipWithIndex.foreach {
    case (impl: AtomicsUnitImp, i) =>
      impl.io.fromCtrl.redirect <> fromCtrl.redirect
      impl.io.fromCtrl.hartId   <> fromCtrl.hartId
      impl.io.fromCtrl.csrCtrl  <> fromCtrl.csrCtrl

      if (impl.io.fromCtrl.trigger.isDefined) {
        impl.io.fromCtrl.trigger.get <> fromCtrl.trigger
      }
      impl.io.flushSbuffer <> io.flushSbuffer
    case _ =>
  }

 // misalignBuffer
  loadMisalignBuffer.io.redirect     <> fromCtrl.redirect
  loadMisalignBuffer.io.rob.lcommit          := fromBackend.rob.lcommit
  loadMisalignBuffer.io.rob.scommit          := fromBackend.rob.scommit
  loadMisalignBuffer.io.rob.pendingUncacheld := fromBackend.rob.pendingUncacheld
  loadMisalignBuffer.io.rob.pendingld        := fromBackend.rob.pendingld
  loadMisalignBuffer.io.rob.pendingst        := fromBackend.rob.pendingst
  loadMisalignBuffer.io.rob.pendingVst       := fromBackend.rob.pendingVst
  loadMisalignBuffer.io.rob.commit           := fromBackend.rob.commit
  loadMisalignBuffer.io.rob.pendingPtr       := fromBackend.rob.pendingPtr
  loadMisalignBuffer.io.rob.pendingPtrNext   := fromBackend.rob.pendingPtrNext

  toLsq.flushFromMaBuf := loadMisalignBuffer.io.flushLdExpBuff

  //
  storeMisalignBuffer.io.redirect   <> fromCtrl.redirect
  storeMisalignBuffer.io.rob.lcommit          := fromBackend.rob.lcommit
  storeMisalignBuffer.io.rob.scommit          := fromBackend.rob.scommit
  storeMisalignBuffer.io.rob.pendingUncacheld := fromBackend.rob.pendingUncacheld
  storeMisalignBuffer.io.rob.pendingld        := fromBackend.rob.pendingld
  storeMisalignBuffer.io.rob.pendingst        := fromBackend.rob.pendingst
  storeMisalignBuffer.io.rob.pendingVst       := fromBackend.rob.pendingVst
  storeMisalignBuffer.io.rob.commit           := fromBackend.rob.commit
  storeMisalignBuffer.io.rob.pendingPtr       := fromBackend.rob.pendingPtr
  storeMisalignBuffer.io.rob.pendingPtrNext   := fromBackend.rob.pendingPtrNext

  toLsq.maControl <> storeMisalignBuffer.io.sqControl

  // issue: [[Backend]] -> [[MemUnit]]
  // std issue
  val numMemUnitStdIssues = stdUnitImps.map(_.getStoreDataIssues()).flatten.length
  val numBackendStdIssues = fromBackend.issueStd.length
  require(numMemUnitStdIssues == numBackendStdIssues,
          s"The number of std issues(${numMemUnitStdIssues}) should be match backend std issues(${numBackendStdIssues})!")
  stdUnitImps.map(_.io.fromIssue).flatten.zip(fromBackend.issueStd).map {
    case (sink, source) =>
      sink.valid := source.valid
      sink.bits.fromMemExuInputBundle(source.bits, isStore = true)
      source.ready := sink.ready
  }

  // sta issue
  val backendStaIssues = fromBackend.issueSta ++ fromBackend.issueHya
  val numBackendStaIssues = backendStaIssues.length
  val memUnitStaIssues = totalStaUnits.map(_.getStoreAddrIssues()).flatten
  val numMemUnitStaIssues = memUnitStaIssues.length
  require(numBackendStaIssues == numMemUnitStaIssues,
          s"The number of backend sta issue(${numBackendStaIssues}) should be match memunit sta issues(${numMemUnitStaIssues}!")

  memUnitStaIssues.zip(backendStaIssues).zipWithIndex.map {
    case ((sink, source), i) =>
      sink.valid := source.valid
      sink.bits.fromMemExuInputBundle(source.bits, isStore = true)
      sink.bits.isIq           := true.B
      sink.bits.isMisalignBuf  := false.B
      sink.bits.isVector       := false.B
      sink.bits.isFastReplay   := false.B
      sink.bits.isLoadReplay   := false.B
      sink.bits.isUncache      := false.B
      sink.bits.isAtomic       := false.B
      sink.bits.isHWPrefetch   := false.B
      source.ready := sink.ready
  }

  // lda issue
  val backendLdaIssues = fromBackend.issueLda ++ fromBackend.issueHya
  val memUnitLdaIssues = totalLoadUnits.map(_.getScalarLoadIssues()).flatten
  require(backendLdaIssues.length == memUnitLdaIssues.length,
         "The number of backendLdaIssues should be match memUnitLdaIssues!")

  memUnitLdaIssues.zip(backendLdaIssues).map {
    case (sink, source) =>
      sink.valid := source.valid
      sink.bits.fromMemExuInputBundle(source.bits)
      sink.bits.isIq           := true.B
      sink.bits.isMisalignBuf  := false.B
      sink.bits.isVector       := false.B
      sink.bits.isFastReplay   := false.B
      sink.bits.isLoadReplay   := false.B
      sink.bits.isUncache      := false.B
      sink.bits.isAtomic       := false.B
      sink.bits.isHWPrefetch   := false.B
      source.ready := sink.ready
  }

  // prefetch issue: [[Prefetch]] -> [[MemUnit]]
  val prefetchIssues = fromPrefetch
  val memUnitPrefetchIssues = totalMemUnits.map(_.getPrefetchIssues()).flatten
  require(prefetchIssues.length == memUnitPrefetchIssues.length,
          "The number of prefetchIssues should be match memUnitPrefetchIssues!")

  memUnitPrefetchIssues.zip(prefetchIssues).map {
    case (sink, source) =>
      sink <> source
      sink.bits.isStore        := false.B
      sink.bits.isIq           := false.B
      sink.bits.isMisalignBuf  := false.B
      sink.bits.isVector       := false.B
      sink.bits.isFastReplay   := false.B
      sink.bits.isLoadReplay   := false.B
      sink.bits.isUncache      := false.B
      sink.bits.isAtomic       := false.B
      sink.bits.isHWPrefetch   := true.B
  }

  // prefetch train: [[MemUnit]] -> [[Prefetch]]
  val memUnitPrefetchTrains = totalMemUnits.map(_.io.toPrefetch.train.get)
  val prefetchTrains = toPrefetch.train
  require(memUnitPrefetchTrains.length == prefetchTrains.length,
          "The number of memUnitPrefetchTrains should be match prefetchTrains!")
  prefetchTrains <> memUnitPrefetchTrains

  val memUnitPrefetchTrainL1s = totalLoadUnits.map(_.io.toPrefetch.trainL1.get)
  val prefetchTrainL1s = toPrefetch.trainL1
  require(memUnitPrefetchTrainL1s.length == prefetchTrainL1s.length,
          "The number of memUnitPrefetchTrainL1s should be match prefetchTrainL1s!")
  prefetchTrainL1s <> memUnitPrefetchTrainL1s

  val memUnitPrefetchIFetchs = totalLoadUnits.map(_.io.toPrefetch.ifetch.get)
  val prefetchTrainIFetchs = toPrefetch.ifetch
  require(memUnitPrefetchIFetchs.length == prefetchTrainIFetchs.length,
          "The number of memUnitPrefetchTrainL1s should be match prefetchTrainIFetchs!")
  prefetchTrainIFetchs <> memUnitPrefetchIFetchs


  // misalign issue: [[StoreMisalignBuffer]] -> [[StaUnits]]
  val storeMisalignBufIssue = Seq(storeMisalignBuffer.io.splitStoreReq)
  val staMisalignBufIssue  = totalStaUnits.map(_.getMisalignBufIssues()).flatten
  require(storeMisalignBufIssue.length == staMisalignBufIssue.length,
          "The number of storeMisalignBufIssue should be match staMisalignBufIssue")

  staMisalignBufIssue.zip(storeMisalignBufIssue).map {
    case (sink, source) =>
      sink <> source
      sink.bits.isStore        := true.B
      sink.bits.isIq           := false.B
      sink.bits.isMisalignBuf  := true.B
      sink.bits.isVector       := false.B
      sink.bits.isFastReplay   := false.B
      sink.bits.isLoadReplay   := false.B
      sink.bits.isUncache      := false.B
      sink.bits.isAtomic       := false.B
      sink.bits.isHWPrefetch   := false.B
  }

  // misalign issue: [[LoadMisalignBuffer]] -> [[LoadUnits]]
  val loadMisalignBufIssues = Seq(loadMisalignBuffer.io.splitLoadReq)
  val lduMisalignBufIssues  = totalLoadUnits.map(_.getMisalignBufIssues()).flatten
  require(loadMisalignBufIssues.length == lduMisalignBufIssues.length,
          "The number of loadMisalignBufIssues should be match lduMisalignBufIssues")

  lduMisalignBufIssues.zip(loadMisalignBufIssues).map {
    case (sink, source) =>
      sink <> source
      sink.bits.isStore        := false.B
      sink.bits.isIq           := false.B
      sink.bits.isMisalignBuf  := true.B
      sink.bits.isVector       := false.B
      sink.bits.isFastReplay   := false.B
      sink.bits.isLoadReplay   := false.B
      sink.bits.isUncache      := false.B
      sink.bits.isAtomic       := false.B
      sink.bits.isHWPrefetch   := false.B
  }

  // vector issue: [[Vector]] -> [[MemUnit]]
  val vectorStoreIssues = io.fromVecExuBlock.vectorStoreIssues
  val memUnitVectorStoreIssues = totalStaUnits.map(_.getVectorStoreIssues()).flatten
  require(vectorStoreIssues.length == memUnitVectorStoreIssues.length,
          "The number of vectorStoreIssues must match memUnitVectorStoreIssues!")

  memUnitVectorStoreIssues.zip(vectorStoreIssues).map {
    case (sink, source) =>
      sink <> source
      sink.bits.isStore        := true.B
      sink.bits.isIq           := false.B
      sink.bits.isMisalignBuf  := false.B
      sink.bits.isVector       := true.B
      sink.bits.isFastReplay   := false.B
      sink.bits.isLoadReplay   := false.B
      sink.bits.isUncache      := false.B
      sink.bits.isAtomic       := false.B
      sink.bits.isHWPrefetch   := false.B
  }

  val vectorLoadIssues = io.fromVecExuBlock.vectorLoadIssues
  val memUnitVectorLoadIssues = totalLoadUnits.map(_.getVectorLoadIssues()).flatten
  require(vectorLoadIssues.length == memUnitVectorLoadIssues.length,
          "The number of vectorLoadIssues must match memUnitVectorLoadIssues!")

  memUnitVectorLoadIssues.zip(vectorLoadIssues).map {
    case (sink, source) =>
      sink <> source
      sink.bits.isStore        := false.B
      sink.bits.isIq           := false.B
      sink.bits.isMisalignBuf  := false.B
      sink.bits.isVector       := true.B
      sink.bits.isFastReplay   := false.B
      sink.bits.isLoadReplay   := false.B
      sink.bits.isUncache      := false.B
      sink.bits.isAtomic       := false.B
      sink.bits.isHWPrefetch   := false.B
  }

  // fast replay reqs: [[MemUnit]] -> [[MemUnit]]
  val fastReplayReqs = totalLoadUnits.map(_.io.toLdu.replay.get)
  val memUnitFastReplayReqs = totalLoadUnits.map(_.getFastReplayIssues()).flatten
  require(fastReplayReqs.length == memUnitFastReplayReqs.length,
          "The number of fastReplayReqs should be match memUnitFastReplayReqs!")

  memUnitFastReplayReqs.zip(fastReplayReqs).map {
    case (sink, source) =>
      sink <> source
      sink.bits.isStore        := false.B
      sink.bits.isIq           := false.B
      sink.bits.isMisalignBuf  := false.B
      sink.bits.isVector       := false.B
      sink.bits.isFastReplay   := true.B
      sink.bits.isLoadReplay   := false.B
      sink.bits.isUncache      := false.B
      sink.bits.isAtomic       := false.B
      sink.bits.isHWPrefetch   := false.B
  }

  // uncache reqs: [[Lsq]] -> [[MemUnit]]
  val uncacheIssues = Seq(fromLsq.uncache.head)
  val memUnitUncacheIssues = totalLoadUnits.map(_.getUncacheIssues()).flatten
  require(uncacheIssues.length == memUnitUncacheIssues.length,
          "The number of fastReplayReqs should be match memUnitUncacheIssues!")

  memUnitUncacheIssues.zip(uncacheIssues).foreach{
    case (sink, source) =>
      sink <> source
      sink.bits.isStore        := false.B
      sink.bits.isIq           := false.B
      sink.bits.isMisalignBuf  := false.B
      sink.bits.isVector       := false.B
      sink.bits.isFastReplay   := false.B
      sink.bits.isLoadReplay   := false.B
      sink.bits.isUncache      := true.B
      sink.bits.isAtomic       := false.B
      sink.bits.isHWPrefetch   := false.B
  }

  // lsq replay reqs: [[Lsq]] -> [[MemUnit]]
  val lsqReplayReqs = fromLsq.replay
  val memUnitLsqReplayReqs = totalLoadUnits.map(_.getLoadReplayIssues()).flatten
  require(lsqReplayReqs.length == memUnitLsqReplayReqs.length,
          "The number of lsqReplayReqs should be match memUnitLsqReplayReqs!")

  memUnitLsqReplayReqs.zip(lsqReplayReqs).map {
    case (sink, source) =>
      sink <> source
      sink.bits.isStore        := false.B
      sink.bits.isIq           := false.B
      sink.bits.isMisalignBuf  := false.B
      sink.bits.isVector       := false.B
      sink.bits.isFastReplay   := false.B
      sink.bits.isLoadReplay   := true.B
      sink.bits.isUncache      := false.B
      sink.bits.isAtomic       := false.B
      sink.bits.isHWPrefetch   := false.B
  }

  // feedback: [[MemUnit]] -> [[Backend]]
  val memUnitStaFeedbacks = totalStaUnits.map(_.io.toBackend.iqFeedback.get)
  val backendStaFeedbacks = toBackend.staIqFeedback ++ toBackend.hyuIqFeedback
  require(memUnitStaFeedbacks.length == backendStaFeedbacks.length,
         "The number of memUnitStaFeedbacks should be match backendStaFeedbacks!")

  backendStaFeedbacks.zip(memUnitStaFeedbacks).map {
    case (sink, source) =>
      sink.feedbackFast := DontCare // FIXME: DontCare is right?
      sink.feedbackSlow <> source
  }

  // sta issue feedback: [[StaUnits]] -> [[Backend]]
  val staIssueFeedbacks = totalStaUnits.map(_.io.toBackend.issue.get)
  val backendIssueFeedbacks = toBackend.stIssue
  require(staIssueFeedbacks.length == backendIssueFeedbacks.length,
         "The number of staIssueFeedbacks should be match backendIssueFeedbacks!")

  backendIssueFeedbacks.zip(staIssueFeedbacks).map {
    case (sink, source) =>
      sink <> source
  }

  // misalign writeback: [[StaUnits]] -> [[StoreMisalignBuffer]]
  val staMisalignBufWritebacks = totalStaUnits.map(_.getMisalignBufWritebacks()).flatten
  val storeMisalignBufWritebacks = Seq(storeMisalignBuffer.io.splitStoreResp)
  require(staMisalignBufWritebacks.length == storeMisalignBufWritebacks.length,
         "The number of staMisalignBufWritebacks should be match storeMisalignBufWritebacks!")

  storeMisalignBufWritebacks.zip(staMisalignBufWritebacks).foreach {
    case (sink, source) =>
      sink.valid := source.valid
      sink.bits  := source.bits
      source.ready := true.B
  }

  // misalign req: [[StaUnits]] -> [[StoreMisalignBuffer]]
  val staMisalignBufReqs = totalStaUnits.map(_.io.toStoreMisalignBuf.get)
  val storeMisalignBufReqs = storeMisalignBuffer.io.req
  require(staMisalignBufReqs.length == storeMisalignBufReqs.length,
        "The number of staMisalignBufReqs should be match storeMisalignBufReqs!")

  storeMisalignBufReqs.zip(staMisalignBufReqs).foreach {
    case (sink, source) =>
      sink <> source
  }

  // misalign writeback: [[LoadUnits]] -> [[LoadMisalignBuffer]]
  val lduMisalignBufWritebacks = totalLoadUnits.map(_.getMisalignBufWritebacks()).flatten
  val loadMisalignBufWritebacks = Seq(loadMisalignBuffer.io.splitLoadResp)
  require(lduMisalignBufWritebacks.length == loadMisalignBufWritebacks.length,
         "The number of lduMisalignBufWritebacks should be match loadMisalignBufWritebacks!")

  loadMisalignBufWritebacks.zip(lduMisalignBufWritebacks).foreach {
    case (sink, source) =>
      sink.valid := source.valid
      sink.bits  := source.bits
      source.ready := true.B
  }

  // misalign req: [[LoadUnits]] -> [[LoadMisalignBuffer]]
  val lduMisalignBufReqs = totalLoadUnits.map(_.io.toLoadMisalignBuf.get)
  val loadMisalignBufReqs = loadMisalignBuffer.io.req
  require(lduMisalignBufReqs.length == loadMisalignBufReqs.length,
        "The number of lduMisalignBufReqs should be match loadMisalignBufReqs!")

  loadMisalignBufReqs.zip(lduMisalignBufReqs).foreach {
    case (sink, source) =>
      sink <> source
  }

  // tlb reqs: [[MemUnit]] -> [[TLB]]
  val memUnitTlbReqs = totalMemUnits.map(_.io.toTlb)
  val tlbReqs = io.toTlb
  require(memUnitTlbReqs.length == tlbReqs.length,
          "The number of memUnitTlbReqs should be match tlbReqs!")

  tlbReqs.zip(memUnitTlbReqs).foreach {
    case (sink, source) =>
      sink.req <> source.req
      sink.req_kill := source.req_kill
  }

  // tlb resps: [[TLB]] -> [[MemUnit]]
  val tlbResps = io.fromTlb
  val memUnitTlbResps = totalMemUnits.map(_.io.fromTlb)
  require(tlbResps.length == memUnitTlbResps.length,
          "The number of tlbResps should be match memUnitRlbResps!")
  memUnitTlbResps.zip(tlbResps).map {
    case (sink, source) =>
      sink <> source
  }

  // Pmp resps: [[PMP]] -> [[MemUnit]]
  val pmpResps = io.fromPmp
  val memUnitPmpResps = totalMemUnits.map(_.io.fromPmp)
  require(pmpResps.length == memUnitPmpResps.length,
          "The number of pmpResps should be match memUnitRlbResps!")
  memUnitPmpResps.zip(pmpResps).map {
    case (sink, source) =>
      sink <> source
  }

  // DCache reqs: [[MemUnit]] -> [[DCache]]
  val memUnitDCacheReqs = totalMemUnits.map(_.io.toDCache)
  val dcacheReqs = io.toDCache
  require(memUnitDCacheReqs.length == dcacheReqs.length,
          "The number of memUnitDCacheReqs should be match dcacheReqs!")

  dcacheReqs.zip(memUnitDCacheReqs).map {
    case (sink, source) =>
      sink <> source
  }

  // DCache reqs: [[DCache]] -> [[MemUnit]]
  val dcacheResps = io.fromDCache
  val memUnitDCacheResps = totalMemUnits.map(_.io.fromDCache)
  require(dcacheResps.length == memUnitDCacheResps.length,
          "The number of dcacheResps should be match memUnitDCacheResps!")

  memUnitDCacheResps.zip(dcacheResps).map {
    case (sink, source) =>
      sink <> source
  }

  // wakeup: [[MemUnit]] -> [[Backend]]
  val memUnitWakeups = totalLoadUnits.map(_.io.toBackend.wakeup.get)
  val backendWakeups = toBackend.wakeup
  require(memUnitWakeups.length == backendWakeups.length,
          "The number of memUnitWakeups should be match backendWakeups!")

  backendWakeups.zip(memUnitWakeups).map {
    case (sink, source) =>
      sink <> source
  }

  // ldCancel: [[MemUnit]] -> [[Backend]]
  val memUnitLdCancels = totalLoadUnits.map(_.io.toBackend.ldCancel.get)
  val backendLdCancels = toBackend.ldCancel
  require(memUnitLdCancels.length == backendLdCancels.length,
          "The number of memUnitLdCancels should be match backendLdCancels!")

  backendLdCancels.zip(memUnitLdCancels).map {
    case (sink, source) =>
      sink <> source
  }

  // rollback: [[MemUnit]] -> [[Backend]]
  val memUnitRollbacks = totalLoadUnits.map(_.io.toBackend.rollback.get)
  val backendRollbacks = toBackend.rollback
  require(memUnitRollbacks.length == backendRollbacks.length,
          "The number of memUnitRollbacks should be match backendRollback!")

  backendRollbacks.zip(memUnitRollbacks).map {
    case (sink, source) =>
      sink <> source
  }

  // lsq forward req: [[MemUnit]] -> [[Lsq]]
  val memUnitLsqForwardReqs = totalLoadUnits.map(_.io.toLsq.forward.get)
  val lsqForwardReqs = toLsq.forward
  require(memUnitLsqForwardReqs.length == lsqForwardReqs.length,
          "The number of memUnitLsqForwardReqs should be match lsqForwardReqs!")

  lsqForwardReqs.zip(memUnitLsqForwardReqs).map {
    case (sink, source) =>
      sink <> source
  }

  // lsq forward resp: [[Lsq]] -> [[MemUnit]]
  val lsqForwardResps = fromLsq.forward
  val memUnitLsqForwardResps = totalLoadUnits.map(_.io.fromLsq.get.forward)
  require(lsqForwardResps.length == memUnitLsqForwardResps.length,
          "The number of lsqForwardResps should be match memUnitLsqForwardResps!")

  memUnitLsqForwardResps.zip(lsqForwardResps).map {
    case (sink, source) =>
      sink <> source
  }

  // sbuffer forward reqs: [[MemUnit]] -> [[Sbuffer]]
  val memUnitSbufferForwardReqs = totalLoadUnits.map(_.io.toSBuffer.get)
  val sbufferForwardReqs = toSBuffer
  require(memUnitSbufferForwardReqs.length == sbufferForwardReqs.length,
          "The number of memUnitSbufferForwardReqs should be match sbufferForwardReqs!")

  memUnitSbufferForwardReqs.zip(sbufferForwardReqs).map {
    case (sink, source) =>
      sink <> source
  }

  // sbuffer forward resps: [[Sbuffer]] -> [[MemUnit]]
  val sbufferForwardResps = fromSBuffer
  val memUnitSbufferForwardResps = totalLoadUnits.map(_.io.fromSBuffer.get)
  require(sbufferForwardResps.length == memUnitSbufferForwardResps.length,
          "The number of sbufferForwardResps should be match memUnitSbufferForwardResps!")

  memUnitSbufferForwardResps.zip(sbufferForwardResps).map {
    case (sink, source) =>
      sink <> source
  }

  // MissQueue forward reqs: [[MemUnit]] -> [[MissQueue]]
  val memUnitMissQueueForwardReqs = totalLoadUnits.map(_.io.toMissQueue.get)
  val missQueueForwardReqs = toMissQueue
  require(memUnitMissQueueForwardReqs.length == missQueueForwardReqs.length,
          "The number of memUnitMissQueueForwardReqs should be match missQueueForwardReqs!")

  memUnitMissQueueForwardReqs.zip(missQueueForwardReqs).map {
    case (sink, source) =>
      sink <> source
  }

  // MissQueue forward resps: [[MissQueue]] -> [[MemUnit]]
  val missQueueForwardResps = fromMissQueue
  val memUnitMissQueueForwardResps = totalLoadUnits.map(_.io.fromMissQueue.get)
  require(memUnitMissQueueForwardResps.length == missQueueForwardResps.length,
          "The number of memUnitMissQueueForwardReqs should be match missQueueForwardResps!")

  memUnitMissQueueForwardResps.zip(missQueueForwardResps).map {
    case (sink, source) =>
      sink <> source
  }

  // TL D channel forward: [[TL]] -> [[MemUnit]]
  val tlDchannelForwardResps = fromTLDchannel
  val memUnitTLDchannelResps = totalLoadUnits.map(_.io.fromTLDchannel.get)
  require(tlDchannelForwardResps.length == memUnitTLDchannelResps.length,
          "The number of tlDchannelForwardResps should be match memUnitTLDchannelResps!")

  memUnitTLDchannelResps.zip(tlDchannelForwardResps).map {
    case (sink, source) =>
      sink <> source
  }


  // AtomicsUnit: AtomicsUnit will override other control signials,
  // as atomics insts (LR/SC/AMO) will block the pipeline
  val sNormal +: sAtomics = Enum(StAddrCnt + 1)
  val state = RegInit(sNormal)
  val stAtomics = backendStaIssues.map(issue =>
    issue.valid && FuType.storeIsAMO(issue.bits.uop.fuType)
  )
  val stDataAtomics = stdUnitImps.map(_.getStoreDataWritebacks()).flatten.map(issue =>
    issue.valid && FuType.storeIsAMO(issue.bits.uop.fuType)
  )
  val backendStaFeedback = toBackend.staIqFeedback ++ toBackend.hyuIqFeedback
  val atomicsExceptionInfo = WireInit(VecInit(Seq.fill(atomicsUnitImps.length)(0.U.asTypeOf(ValidIO(new ExceptionAddrIO)))))

  atomicsUnitImps.zipWithIndex.foreach {
    case (impl: AtomicsUnitImp, i) =>
      // Tlb
      impl.io.fromTlb.resp.valid := false.B
      impl.io.fromTlb.resp.bits  := DontCare
      impl.io.toTlb.req.ready := tlbReqs.head.req.ready
      impl.io.fromPmp   <> pmpResps.head

      // DCache
      impl.io.fromDCache := DontCare
      impl.io.toDCache.req.ready := false.B
      impl.io.amoDCacheIO <> io.amoDCacheIO

      // issue sta: [[Backend]] -> [[AtomicsUnit]]
      impl.io.fromIssue.head.valid := stAtomics.reduce(_|_)
      impl.io.fromIssue.head.bits.fromMemExuInputBundle(Mux1H(
        stAtomics.zip(backendStaIssues).map(x => x._1 -> x._2.bits)
      ))
      impl.io.fromIssue.head.bits.isIq := true.B
      impl.io.fromIssue.head.bits.isAtomic := true.B

      // issue std: [[Backend]] -> [[AtomicsUnit]]
      impl.io.fromBackend.storeDataIn.valid := stDataAtomics.reduce(_|_)
      impl.io.fromBackend.storeDataIn.bits := Mux1H(
        stDataAtomics.zip(stdUnitImps.map(_.getStoreDataWritebacks()).flatten).map(wb =>
          wb._1 -> wb._2.bits.toMemExuOutputBundle()
        )
      )

      // feedback: [[AtomicsUnit]] -> [[Backend]]
      backendStaFeedback.map(_.feedbackSlow).zip(memUnitStaFeedbacks).zip(sAtomics).foreach {
        case ((sink, source), amoState) =>
          when(state === amoState) {
            sink <> impl.io.toBackend.iqFeedback
            assert(!source.valid)
          }
      }

      when (state =/= sNormal) {
        val amoLoad = loadUnitImps.head.asInstanceOf[HybridUnitImp]
        // use store wb port instead of load
        amoLoad.getScalarLoadWritebacks().map(_.ready := false.B)
        // use Load_0's TLB
        impl.io.toTlb <> tlbReqs.head
        impl.io.fromTlb <> tlbResps.head
        // hw prefetch should be disabled while executing atomic insts
        amoLoad.getPrefetchIssues().map(_.valid := false.B)
        // make sure there's no in-flight uops in load unit
        assert(!amoLoad.getScalarLoadWritebacks().map(_.valid).reduce(_|_))
      }

      when (impl.io.toIssue.head.valid) {
        assert(sAtomics.map(_ === state).reduce(_ || _))
        state := sNormal
      }

      when (DelayN(fromCtrl.redirect.valid, 10) && atomicsExceptionInfo(i).valid) {
        atomicsExceptionInfo(i).valid := false.B
      } .elsewhen (impl.io.exceptionInfo.valid) {
        atomicsExceptionInfo(i).valid := true.B
        atomicsExceptionInfo(i).bits := impl.io.exceptionInfo.bits
      }

      impl.io.flushSbuffer.empty := fromLsq.flushFinish
      io.atomicsExceptionInfo := DontCare
      io.atomicsExceptionInfo := atomicsExceptionInfo(i)
  }

  memUnitStaIssues.zip(backendStaIssues).zip(stAtomics.zip(sAtomics)).zipWithIndex.foreach {
    case (((sink, source), (atomics, amoState)), i) =>
      when (atomics) {
        source.ready := atomicsUnitImps.head.io.fromIssue.head.ready
        sink.valid := false.B

        state := amoState
        assert(!stAtomics.zipWithIndex.filterNot(_._2 == i).unzip._1.reduce(_ || _))
      }
  }


  val atomicsException = RegInit(false.B)
  when (DelayN(fromCtrl.redirect.valid, 10) && atomicsException) {
    atomicsException := false.B
  }.elsewhen (io.atomicsExceptionInfo.valid) {
    atomicsException := true.B
  }
  XSError(atomicsException && atomicsUnitImps.map(_.getAtomicsIssues()).flatten.map(_.valid).reduce(_|_), "new instruction before exception triggers\n")

  // overwrite misalignbuffer exception
  io.misalignExceptionInfo := Mux(
                                loadMisalignBuffer.io.overwriteExpBuf.valid,
                                loadMisalignBuffer.io.overwriteExpBuf,
                                storeMisalignBuffer.io.overwriteExpBuf
                              )

  // writeback: [[MemUnit]] -> [[Backend]]
  // std
  val memUnitStdWritebacks = stdUnitImps.map(_.getStoreDataWritebacks()).flatten
  val backendStdWritebacks = toBackend.writebackStd
  require(backendStdWritebacks.length == memUnitStdWritebacks.length,
         "The number of memUnitStdWritebacks should be match backendStdWritebacks!")

  backendStdWritebacks.zip(memUnitStdWritebacks).map {
    case (sink, source) =>
      sink.valid := source.valid
      sink.bits  := source.bits.toMemExuOutputBundle()
      sink.bits.mask.map(_ := 0.U)
      sink.bits.vdIdx.map(_ := 0.U)
      sink.bits.vdIdxInField.map(_ := 0.U)
      source.ready := sink.ready
  }

  // sta
  val memUnitStaWritebacks = totalStaUnits.map(_.getStoreAddrWritebacks()).flatten
  val backendStaWritebacks = toBackend.writebackSta ++ toBackend.writebackHyuSta
  require(memUnitStaWritebacks.length == backendStaWritebacks.length,
         "The number of memUnitStaWritebacks should be match backendStaWritebacks!")

  backendStaWritebacks.zip(memUnitStaWritebacks).map {
    case (sink, source) =>
      sink.valid := source.valid
      sink.bits := source.bits.toMemExuOutputBundle()
      source.ready := sink.ready
  }

  // lda
  // overwrite lda first writeback port for atomics/loadMisalignBuffer writeback
  val loadMisalignBufferOut = Wire(DecoupledIO(new LsPipelineBundle))
  loadMisalignBufferOut.valid := loadMisalignBuffer.io.writeBack.valid
  loadMisalignBufferOut.bits.fromMemExuOutputBundle(loadMisalignBuffer.io.writeBack.bits)
  loadMisalignBuffer.io.writeBack.ready := loadMisalignBufferOut.ready

  val ldaOut = Wire(DecoupledIO(new LsPipelineBundle))
  ldaOut.valid := loadMisalignBufferOut.valid ||
                  atomicsUnitImps.head.io.toIssue.head.valid ||
                  loadUnitImps.head.getScalarLoadWritebacks().head.valid
  ldaOut.bits  := Mux(
                    loadMisalignBufferOut.valid,
                    loadMisalignBufferOut.bits,
                    Mux(
                      atomicsUnitImps.head.io.toIssue.head.valid,
                      atomicsUnitImps.head.io.toIssue.head.bits,
                      loadUnitImps.head.getScalarLoadWritebacks().head.bits
                    ),
                  )
  ldaOut.bits.isStore := (loadMisalignBufferOut.valid ||
                          atomicsUnitImps.head.io.toIssue.head.valid)

  val memUnitLdaWritebacks = ldaOut +: totalLoadUnits.map(_.getScalarLoadWritebacks()).flatten.drop(1)
  val backendLdaWritebacks = toBackend.writebackLda ++ toBackend.writebackHyuLda
  require(memUnitLdaWritebacks.length == backendLdaWritebacks.length,
          "The number of memUnitLdaWritebacks should be match backendLdaWritebacks!")

  backendLdaWritebacks.zip(memUnitLdaWritebacks).map {
    case (sink, source) =>
      sink.valid := source.valid
      sink.bits := source.bits.toMemExuOutputBundle()
      source.ready := sink.ready
  }

  // debugLsInfo: [[MemUnit]] -> [[Backend]]
  val memUnitDebugLsInfo = totalMemUnits.map(_.io.debugLsInfo)
  val backendDebugLsInfo = io.debugLsInfo
  require(memUnitDebugLsInfo.length == backendDebugLsInfo.length,
          "The number of memUnitDebugLsInfo should be match backendDebugLsInfo!")

  backendDebugLsInfo.zip(memUnitDebugLsInfo).map {
    case (sink, source) =>
      sink <> source
  }

  // Topdown: [[MemUnit]] -> [[Backend]]
  val memUnitTopdownInfo = totalLoadUnits.map(_.io.lsTopdownInfo)
  val backendTopdownInfo = io.lsTopdownInfo
  require(memUnitTopdownInfo.length == backendTopdownInfo.length,
          "The number of memUnitTopdownInfo should be match backendTopdownInfo!")

  backendTopdownInfo.zip(memUnitTopdownInfo).map {
    case (sink, source) =>
      sink <> source
  }

  // performance events
  val perfEvents = totalMemUnits.map(_.asInstanceOf[MemUnitImp]).flatMap(_.getPerfEvents)
  generatePerfEvent()
}