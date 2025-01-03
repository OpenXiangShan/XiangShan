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
import xiangshan.backend.datapath.NewPipelineConnect
import xiangshan.backend.fu.PMPRespBundle
import xiangshan.backend.fu.NewCSR._
import xiangshan.backend.fu.{FuType}
import xiangshan.backend.ctrlblock.{DebugLsInfoBundle, LsTopdownInfo}
import xiangshan.backend.rob.{RobLsqIO, RobPtr}
import xiangshan.mem.Bundles._
import xiangshan.cache._
import xiangshan.cache.{HasDCacheParameters, StorePrefetchReq}
import xiangshan.cache.mmu.{TlbReq, TlbResp, TlbCmd, TlbHintReq}
import xiangshan.cache.{DCacheLoadReqIO, DCacheLoadRespIO}

class MemExuBlock(implicit p: Parameters) extends LazyModule with HasXSParameter with HasMemBlockParameters {
  val stdUnits = memUnitParams.filter(_.isStd).map(
    params => LazyModule(new MemUnit(params)).suggestName(params.name)
  )
  require(stdUnits.length == StdCnt, "The number of StdUnit should be match StdCnt!")

  val staUnits = memUnitParams.filter(_.isSta).map(
    params => LazyModule(new MemUnit(params)).suggestName(params.name)
  )
  require(staUnits.length == StaCnt, "The number of StaUnit should be match StaCnt!")

  val ldUnits = memUnitParams.filter(_.isLdu).map(
    params => LazyModule(new MemUnit(params)).suggestName(params.name)
  )
  require(ldUnits.length == LduCnt, "The number of LoadUnit should be match LduCnt!")

  val hyUnits = memUnitParams.filter(_.isHyu).map(
    params => LazyModule(new MemUnit(params)).suggestName(params.name)
  )
  require(hyUnits.length == HyuCnt, "The number of Hybrid unit should be match HyuCnt!")

  val amoUnits = memUnitParams.filter(_.isAmo).map(
    params => LazyModule(new MemUnit(params)).suggestName(params.name)
  )
  require(amoUnits.length == 1, "Only one atomicsUnit!")

  lazy val module = new MemExuBlockImp(this)
}

class BackendToMemExuBlockIO(implicit p: Parameters) extends MemBlockBundle {
  val issueLda = MixedVec(Seq.fill(LduCnt)(Flipped(DecoupledIO(new MemExuInput))))
  val issueSta = MixedVec(Seq.fill(StaCnt)(Flipped(DecoupledIO(new MemExuInput))))
  val issueStd = MixedVec(Seq.fill(StdCnt)(Flipped(DecoupledIO(new MemExuInput))))
  val issueHya = MixedVec(Seq.fill(HyuCnt)(Flipped(DecoupledIO(new MemExuInput))))
  val robLsqIO = new Bundle() {
    val lcommit = Input(UInt(log2Up(CommitWidth + 1).W))
    val scommit = Input(UInt(log2Up(CommitWidth + 1).W))
    val pendingMMIOld = Input(Bool())
    val pendingld = Input(Bool())
    val pendingst = Input(Bool())
    val pendingVst = Input(Bool())
    val commit = Input(Bool())
    val pendingPtr = Input(new RobPtr)
    val pendingPtrNext = Input(new RobPtr)
  }
}

class MemExuBlockToBackendIO(implicit p: Parameters) extends MemBlockBundle {
  val writebackLda = Vec(LduCnt, DecoupledIO(new MemExuOutput))
  val writebackSta = Vec(StaCnt, DecoupledIO(new MemExuOutput))
  val writebackStd = Vec(StdCnt, DecoupledIO(new MemExuOutput))
  val writebackHyuLda = Vec(HyuCnt, DecoupledIO(new MemExuOutput))
  val writebackHyuSta = Vec(HyuCnt, DecoupledIO(new MemExuOutput))

  val ldaIqFeedback = Vec(LduCnt, new MemRSFeedbackIO)
  val staIqFeedback = Vec(StaCnt, new MemRSFeedbackIO)
  val hyuIqFeedback = Vec(HyuCnt, new MemRSFeedbackIO)

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
  val excpUpdate = Vec(StAddrCnt, new LsPipelineBundle)
  val maskOut    = Vec(StAddrCnt, ValidIO(new StoreMaskBundle))
  val sqControl  = Output(new StoreMaBufToSqCtrlControlBundle)
  val loadMisalignFull = Output(Bool())
}

class LsqToMemExuBlockIO(implicit p: Parameters) extends MemBlockBundle {
  val forward = Vec(LdExuCnt, new LoadForwardRespBundle)
  val rarNuke = Vec(LdExuCnt, ValidIO(new LoadNukeQueryRespBundle))
  val rawNuke = Vec(LdExuCnt, ValidIO(new LoadNukeQueryRespBundle))
  val replay  = Vec(LdExuCnt, DecoupledIO(new LsPipelineBundle))
  val ncOut   = Vec(LdExuCnt, DecoupledIO(new LsPipelineBundle))
  val mmioLdWriteback = DecoupledIO(new MemExuOutput)
  val mmioLdData = Output(new LoadDataFromLQBundle)
  val mmioStWriteback = DecoupledIO(new MemExuOutput)
  val maControl = Output(new StoreMaBufToSqCtrlStoreInfoBundle)
}

class MemUnitToMisalignBufferIO(implicit p: Parameters, params: MemUnitParams) extends XSBundle {
  val enq = DecoupledIO(new LsPipelineBundle)
  val out = OptionWrapper(params.hasMisalign, DecoupledIO(new LsPipelineBundle))
}

class MemExuBlockIO(implicit p: Parameters) extends MemBlockBundle {
  // from
  val fromCtrl = new Bundle () {
    val hartId = Input(UInt(hartIdLen.W))
    val redirect = Flipped(ValidIO(new Redirect))
    val csr = Flipped(new CustomCSRCtrlIO)
    val trigger  = Input(new CsrTriggerBundle)
  }

  val fromBackend = new BackendToMemExuBlockIO
  val fromVecExuBlock = Flipped(new ToMemExuBlockIO)
  val fromLsq = Flipped(new LsqToMemExuBlockIO)
  val fromTlb = Vec(MemAddrExtCnt, new Bundle() {
    val resp = Flipped(DecoupledIO(new TlbResp(2)))
    val hint = Flipped(new TlbHintReq)
  })
  val fromUncache = Vec(LdExuCnt, Input(new LoadForwardRespBundle))
  val fromSBuffer = Vec(LdExuCnt, Input(new LoadForwardRespBundle))
  val fromMissQueue  = Vec(LdExuCnt, Flipped(new MissQueueForwardRespBundle))
  val fromTL = Vec(LdExuCnt, Input(new DcacheToLduForwardIO))
  val fromDCache = Vec(MemAddrExtCnt, new DCacheLoadRespIO)
  val fromPmp = Vec(MemAddrExtCnt,  Flipped(new PMPRespBundle()))
  val fromPrefetch = Vec(MemAddrExtCnt, Flipped(DecoupledIO(new LsPipelineBundle)))

  // to
  val toBackend = new MemExuBlockToBackendIO
  val toVecExuBlock = Flipped(new FromMemExuBlockIO)
  val toLsq = new MemExuBlockToLsqIO
  val toUncache = Vec(LdExuCnt, ValidIO(new LoadForwardReqBundle))
  val toDCache = Vec(MemAddrExtCnt, new DCacheLoadReqIO)
  val toTlb = Vec(MemAddrExtCnt, new Bundle() {
    val req = DecoupledIO(new TlbReq)
    val req_kill = Output(Bool())
  })
  val toSBuffer = Vec(LdExuCnt, ValidIO(new LoadForwardReqBundle))
  val toMissQueue = Vec(LdExuCnt, ValidIO(new MissQueueForwardReqBundle))
  val toPrefetch = new Bundle() {
    val ifetch = Vec(LdExuCnt, ValidIO(new SoftIfetchPrefetchBundle))
    val train = Vec(LdExuCnt + StaCnt, new LsPrefetchTrainIO)
    val trainL1 = Vec(LdExuCnt, new LsPrefetchTrainIO)
  }
  val amoDCacheIO = new AtomicWordIO
  val flushSbuffer = new SbufferFlushBundle
  val atomicsExceptionInfo = ValidIO(new ExceptionAddrIO)
  val misalignExceptionInfo = ValidIO(new ExceptionAddrIO)
  val debugLsInfo = Vec(MemAddrExtCnt, Output(new DebugLsInfoBundle))
  val lsTopdownInfo = Vec(LdExuCnt, Output(new LsTopdownInfo))
}

class MemExuBlockImp(wrapper: MemExuBlock) extends LazyModuleImp(wrapper)
  with HasPerfEvents
  with HasXSParameter
  with HasMemBlockParameters {
  val io = IO(new MemExuBlockIO)

  private val fromCtrl = io.fromCtrl
  private val fromTL = io.fromTL
  private val fromPmp = io.fromPmp
  private val (fromBackend, toBackend) = (io.fromBackend, io.toBackend)
  private val (fromVecExuBlock, toVecExuBlock) = (io.fromVecExuBlock, io.toVecExuBlock)
  private val (fromLsq, toLsq) = (io.fromLsq, io.toLsq)
  private val (fromUncache, toUncache) = (io.fromUncache, io.toUncache)
  private val (fromTlb, toTlb) = (io.fromTlb, io.toTlb)
  private val (fromSBuffer, toSBuffer) = (io.fromSBuffer, io.toSBuffer)
  private val (fromMissQueue, toMissQueue) = (io.fromMissQueue, io.toMissQueue)
  private val (fromDCache, toDCache) = (io.fromDCache, io.toDCache)
  private val (fromPrefetch, toPrefetch) = (io.fromPrefetch, io.toPrefetch)

  val stdUnitImps = wrapper.stdUnits.map(_.module)
  val staUnitImps = wrapper.staUnits.map(_.module.asInstanceOf[HyuImp])
  val ldUnitImps = wrapper.ldUnits.map(_.module.asInstanceOf[HyuImp])
  val hyUnitImps = wrapper.hyUnits.map(_.module.asInstanceOf[HyuImp])
  val amoUnitImps = wrapper.amoUnits.map(_.module)

  val loadMisalignBuffer = Module(new LoadMisalignBuffer)
  val storeMisalignBuffer = Module(new StoreMisalignBuffer)

  // priority: load unit > hybrid unit > sta unit
  private val totalStaUnits = hyUnitImps ++ staUnitImps
  private val totalLdUnits = ldUnitImps ++ hyUnitImps
  private val totalMemUnits = ldUnitImps ++ hyUnitImps ++ staUnitImps

  val hartId = p(XSCoreParamsKey).HartId
  val correctMissTrain = Constantin.createRecord(s"CorrectMissTrain$hartId", initValue = false)

  // stdUnits
  stdUnitImps.zipWithIndex.foreach {
    case (impl: StdImp, i) =>
      impl.io.fromCtrl.redirect <> fromCtrl.redirect
      impl.io.fromCtrl.hartId <> fromCtrl.hartId
      impl.io.fromCtrl.csr <> fromCtrl.csr
      impl.io.fromTlb := DontCare
      impl.io.fromDCache := DontCare
      impl.io.fromPmp := DontCare
      impl.io.toDCache.req.ready := false.B
      impl.io.toTlb.req.ready := false.B

    case _ =>
  }

  // staUnits
  staUnitImps.zipWithIndex.foreach {
    case (impl: HyuImp, i) =>
      impl.io.fromCtrl.redirect <> fromCtrl.redirect
      impl.io.fromCtrl.hartId <> fromCtrl.hartId
      impl.io.fromCtrl.csr  <> fromCtrl.csr
      if (impl.io.fromCtrl.trigger.isDefined) {
        impl.io.fromCtrl.trigger.get <> fromCtrl.trigger
      }

    case _ =>
  }

  // ldUnits
  ldUnitImps.zipWithIndex.foreach {
    case (impl: HyuImp, i) =>
      impl.io.fromCtrl.redirect <> fromCtrl.redirect
      impl.io.fromCtrl.hartId <> fromCtrl.hartId
      impl.io.fromCtrl.csr <> fromCtrl.csr
      if (impl.io.fromCtrl.trigger.isDefined) {
        impl.io.fromCtrl.trigger.get <> fromCtrl.trigger
      }
      impl.io.correctMissTrain := correctMissTrain

    case _ =>
  }

  // hyUnits
  hyUnitImps.zipWithIndex.foreach {
    case (impl: HyuImp, i) =>
      impl.io.fromCtrl.redirect <> fromCtrl.redirect
      impl.io.fromCtrl.hartId <> fromCtrl.hartId
      impl.io.fromCtrl.csr <> fromCtrl.csr
      if (impl.io.fromCtrl.trigger.isDefined) {
        impl.io.fromCtrl.trigger.get <> fromCtrl.trigger
      }
      impl.io.correctMissTrain := correctMissTrain

    case _ =>
  }

  amoUnitImps.zipWithIndex.foreach {
    case (impl: AmoImp, i) =>
      impl.io.fromCtrl.redirect <> fromCtrl.redirect
      impl.io.fromCtrl.hartId <> fromCtrl.hartId
      impl.io.fromCtrl.csr <> fromCtrl.csr
      if (impl.io.fromCtrl.trigger.isDefined) {
        impl.io.fromCtrl.trigger.get <> fromCtrl.trigger
      }
      impl.io.flushSbuffer <> io.flushSbuffer

    case _ =>
  }

 // misalignBuffer
  loadMisalignBuffer.io.redirect <> fromCtrl.redirect
  loadMisalignBuffer.io.rob.lcommit := fromBackend.robLsqIO.lcommit
  loadMisalignBuffer.io.rob.scommit := fromBackend.robLsqIO.scommit
  loadMisalignBuffer.io.rob.pendingMMIOld := fromBackend.robLsqIO.pendingMMIOld
  loadMisalignBuffer.io.rob.pendingld := fromBackend.robLsqIO.pendingld
  loadMisalignBuffer.io.rob.pendingst := fromBackend.robLsqIO.pendingst
  loadMisalignBuffer.io.rob.pendingVst := fromBackend.robLsqIO.pendingVst
  loadMisalignBuffer.io.rob.commit := fromBackend.robLsqIO.commit
  loadMisalignBuffer.io.rob.pendingPtr := fromBackend.robLsqIO.pendingPtr
  loadMisalignBuffer.io.rob.pendingPtrNext := fromBackend.robLsqIO.pendingPtrNext
  loadMisalignBuffer.io.vecWriteBack <> toVecExuBlock.loadMisalignBuffer.writeback
  toLsq.loadMisalignFull := loadMisalignBuffer.io.loadMisalignFull

  //
  storeMisalignBuffer.io.redirect <> fromCtrl.redirect
  storeMisalignBuffer.io.rob.lcommit := fromBackend.robLsqIO.lcommit
  storeMisalignBuffer.io.rob.scommit := fromBackend.robLsqIO.scommit
  storeMisalignBuffer.io.rob.pendingMMIOld := fromBackend.robLsqIO.pendingMMIOld
  storeMisalignBuffer.io.rob.pendingld := fromBackend.robLsqIO.pendingld
  storeMisalignBuffer.io.rob.pendingst := fromBackend.robLsqIO.pendingst
  storeMisalignBuffer.io.rob.pendingVst := fromBackend.robLsqIO.pendingVst
  storeMisalignBuffer.io.rob.commit := fromBackend.robLsqIO.commit
  storeMisalignBuffer.io.rob.pendingPtr := fromBackend.robLsqIO.pendingPtr
  storeMisalignBuffer.io.rob.pendingPtrNext := fromBackend.robLsqIO.pendingPtrNext
  storeMisalignBuffer.io.toVecStoreMergeBuffer <> toVecExuBlock.storeMisalignBuffer.control
  storeMisalignBuffer.io.vecWriteBack <> toVecExuBlock.storeMisalignBuffer.writeback
  storeMisalignBuffer.io.full <> toVecExuBlock.storeMisalignBuffer.storeMisalignBufferFull

  toLsq.sqControl <> storeMisalignBuffer.io.sqControl.toStoreQueue
  storeMisalignBuffer.io.sqControl.toStoreMisalignBuffer := fromLsq.maControl

  // issue: [[Backend]] -> [[MemUnit]]
  // std issue
  val memUnitStdIssues = stdUnitImps.map(_.getStdIssues()).flatten
  val numMemUnitStdIssues = memUnitStdIssues.length
  val numBackendStdIssues = fromBackend.issueStd.length
  require(numMemUnitStdIssues == numBackendStdIssues,
    s"The number of std issues(${numMemUnitStdIssues}) should be match backend std issues(${numBackendStdIssues})!")

  memUnitStdIssues.zip(fromBackend.issueStd).foreach {
    case (sink, source) =>
      sink.valid := source.valid
      source.ready := sink.ready
      sink.bits.fromMemExuInputBundle(source.bits, isStore = true)
  }

  // sta issue
  val backendStaIssues = fromBackend.issueHya ++ fromBackend.issueSta
  val numBackendStaIssues = backendStaIssues.length
  val memUnitStaIssues = totalStaUnits.map(_.getStaIssues()).flatten
  val numMemUnitStaIssues = memUnitStaIssues.length
  require(numBackendStaIssues == numMemUnitStaIssues,
    s"The number of backend sta issue(${numBackendStaIssues}) should be match memunit sta issues(${numMemUnitStaIssues})!")

  memUnitStaIssues.zip(backendStaIssues).foreach {
    case (sink, source) =>
      sink.valid := source.valid
      source.ready := sink.ready
      sink.bits.fromMemExuInputBundle(source.bits, isStore = true)
  }

  // ldu issue
  val backendLdIssues = fromBackend.issueLda ++ fromBackend.issueHya
  val numBackendLdIssues = backendLdIssues.length
  val memUnitLdIssues = totalLdUnits.map(_.getLdIssues()).flatten
  val numMemUnitLdIssues = memUnitLdIssues.length
  require(numBackendLdIssues == numMemUnitLdIssues,
    s"The number of backend ldu issue(${numBackendLdIssues}) should be match memunit ldu issues(${numMemUnitLdIssues})!")

  memUnitLdIssues.zip(backendLdIssues).foreach {
    case (sink, source) =>
      sink.valid := source.valid
      source.ready := sink.ready
      sink.bits.fromMemExuInputBundle(source.bits)
      sink.bits.isIq := true.B
  }

  // prefetch issue: [[Prefetch]] -> [[MemUnit]]
  val prefetchIssues = fromPrefetch
  val numPrefetchIssue = fromPrefetch.length
  val memUnitPrefetchIssues = totalMemUnits.map(_.getPrefetchIssues()).flatten
  val numMemUnitPrefetchIssue = memUnitPrefetchIssues.length
  require(numPrefetchIssue == numMemUnitPrefetchIssue,
    s"The number of prefetch issue(${numPrefetchIssue}) should be match memunit prefetch issues(${numMemUnitPrefetchIssue})!")

  memUnitPrefetchIssues.zip(prefetchIssues).foreach {
    case (sink, source) =>
      sink <> source
  }

  // prefetch train: [[MemUnit]] -> [[Prefetch]]
  val memUnitPrefetchTrains = totalMemUnits.flatMap { memUnit =>
    // Convert to List if it is an Option or directly return the value if not
    memUnit.io.toPrefetch.train.toList
  }
  val numMemUnitPrefetchTrain = memUnitPrefetchTrains.length
  val prefetchTrains = toPrefetch.train
  val numPrefetchTrain = prefetchTrains.length
  require(numMemUnitPrefetchTrain == numPrefetchTrain,
    s"The number of prefetch train(${numMemUnitPrefetchTrain}) should be match memunit prefetch" +
      s"train(${numPrefetchTrain})!")

  prefetchTrains.zip(memUnitPrefetchTrains).foreach {
    case (sink, source) =>
      sink <> source
  }

  val memUnitPrefetchTrainL1s = totalMemUnits.flatMap { memUnit =>
    // Convert to List if it is an Option or directly return the value if not
    memUnit.io.toPrefetch.trainL1.toList
  }
  val numMemUnitPrefetchTrainL1 = memUnitPrefetchTrainL1s.length
  val prefetchTrainL1s = toPrefetch.trainL1
  val numPrefetchTrainL1 = prefetchTrainL1s.length
  require(numMemUnitPrefetchTrainL1 == numPrefetchTrainL1,
    s"The number of prefetch train l1(${numMemUnitPrefetchTrainL1}) should be match memunit prefetch " +
      s"train l1(${numPrefetchTrainL1})!")

  prefetchTrainL1s.zip(memUnitPrefetchTrainL1s).foreach {
    case (sink, source) =>
      sink <> source
  }

  val memUnitPrefetchIFetchs = totalLdUnits.map(_.io.toPrefetch.ifetch.get)
  val numMemUnitPrefetchIFetch = memUnitPrefetchIFetchs.length
  val prefetchTrainIFetchs = toPrefetch.ifetch
  val numPrefetchTrainIFetch = prefetchTrainIFetchs.length
  require(numMemUnitPrefetchIFetch == numPrefetchTrainIFetch,
    s"The number of prefetch train ifetch(${numMemUnitPrefetchIFetch}) should be match memunit prefetch train ifetch(${numPrefetchTrainIFetch})!")

  prefetchTrainIFetchs.zip(memUnitPrefetchIFetchs).foreach {
    case (sink, source) =>
      sink <> source
  }

  // misalign issue: [[StoreMisalignBuffer]] -> [[StaUnits]]
  val storeMisalignBufIssues = Seq(storeMisalignBuffer.io.splitStoreReq)
  val numStoreMisalignBufIssue = storeMisalignBufIssues.length
  val staMisalignBufIssues = totalStaUnits.map(_.getMisalignIssues()).flatten
  val numStaMisalignBufIssue = staMisalignBufIssues.length
  require(numStoreMisalignBufIssue == numStaMisalignBufIssue,
    s"The number of store misalign buf issue(${numStoreMisalignBufIssue}) should be match memunit store misalign issue(${numStaMisalignBufIssue})!")

  staMisalignBufIssues.zip(storeMisalignBufIssues).foreach {
    case (sink, source) =>
      sink <> source
  }

  // misalign issue: [[LoadMisalignBuffer]] -> [[LdaUnits]]
  val loadMisalignBufIssues = Seq(loadMisalignBuffer.io.splitLoadReq)
  val numLoadMisalignBufIssue = loadMisalignBufIssues.length
  val ldaMisalignBufIssues  = totalLdUnits.map(_.getMisalignIssues()).flatten
  val numLdaMisalignBufIssue = ldaMisalignBufIssues.length
  require(numLoadMisalignBufIssue == numLdaMisalignBufIssue,
    s"The number of load misalign buf issue(${numLoadMisalignBufIssue}) should be match memunit load misalign issue(${numLdaMisalignBufIssue})!")

  ldaMisalignBufIssues.zip(loadMisalignBufIssues).foreach {
    case (sink, source) =>
      sink <> source
  }

  // vector issue: [[Vector]] -> [[MemUnit]]
  val vectorStoreIssues = fromVecExuBlock.vectorStoreIssues
  val numVectorStoreIssue = vectorStoreIssues.length
  val memUnitVectorStoreIssues = totalStaUnits.map(_.getVstIssues()).flatten
  val numMemUnitVectorStoreIssue = memUnitVectorStoreIssues.length
  require(numVectorStoreIssue == numMemUnitVectorStoreIssue,
    s"The number of vector store issue(${numVectorStoreIssue}) should be match memunit vector store issue(${numVectorStoreIssue})!")

  memUnitVectorStoreIssues.zip(vectorStoreIssues).foreach {
    case (sink, source) =>
      sink <> source
  }

  val vectorLoadIssues = fromVecExuBlock.vectorLoadIssues
  val numVectorLoadIssue = vectorLoadIssues.length
  val memUnitVectorLoadIssues = totalLdUnits.map(_.getVldIssues()).flatten
  val numMemUnitVectorLoadIssue = memUnitVectorLoadIssues.length
  require(numVectorLoadIssue == numMemUnitVectorLoadIssue,
    s"The number of vector load issue(${numVectorLoadIssue}) should be match memunit vector load issue(${numMemUnitVectorLoadIssue})!")

  memUnitVectorLoadIssues.zip(vectorLoadIssues).foreach {
    case (sink, source) =>
      sink <> source
  }

  // fast replay reqs: [[MemUnit]] -> [[MemUnit]]
  val fastReplayReqs = totalLdUnits.map(_.io.toLdu.replay.get)
  val numFastReplayReq = fastReplayReqs.length
  val memUnitFastReplayReqs = totalLdUnits.map(_.getFastReplayIssues()).flatten
  val numMemUnitFastReplayReq = memUnitFastReplayReqs.length
  require(numFastReplayReq == numMemUnitFastReplayReq,
    s"The number of fast replay req(${numFastReplayReq}) should be match memunit fast replay req(${numMemUnitFastReplayReq})!")

  memUnitFastReplayReqs.zip(fastReplayReqs).foreach {
    case (sink, source) =>
      sink <> source
  }

  // mmio reqs: [[Lsq]] -> [[MemUnit]]
  val mmioIssues = Seq(fromLsq.mmioLdWriteback)
  val numMmioIssue = mmioIssues.length
  val memUnitMmioIssues = totalLdUnits.map(_.getMmioIssues()).flatten
  val numMemUnitMmioIssues = memUnitMmioIssues.length
  require(numMmioIssue == numMemUnitMmioIssues,
    s"The number of mmio req(${numMmioIssue}) should be match memunit mmio req(${numMemUnitMmioIssues})!")

  memUnitMmioIssues.zip(mmioIssues).foreach {
    case (sink, source) =>
      sink.valid := source.valid
      source.ready := sink.ready
      sink.bits.fromMemExuOutputBundle(source.bits)
  }

  // mmio load data: [[Lsq]] -> [[MemUnit]]
  totalLdUnits.map(_.io.fromLsq).filter(_.isDefined).foreach {
    case sink =>
      Connection.connectValidIO(sink.get.mmioLdWriteback, fromLsq.mmioLdWriteback)
      sink.get.mmioLdData <> fromLsq.mmioLdData
  }

  // nc reqs: [[Lsq]] -> [[MemUnit]]
  val ncIssues = fromLsq.ncOut
  val numNcIssue = ncIssues.length
  val memUnitNcIssues = totalLdUnits.map(_.getUncacheIssues()).flatten
  val numMemUnitNcIssues = memUnitNcIssues.length
  require(numNcIssue == numMemUnitNcIssues,
    s"The number of nc req(${numNcIssue}) should be match memunit nc req(${numMemUnitNcIssues})!")

  memUnitNcIssues.zip(ncIssues).foreach {
    case (sink, source) =>
      sink <> source
  }

  // lsq replay reqs: [[Lsq]] -> [[MemUnit]]
  val lsqReplayReqs = fromLsq.replay
  val numLsqReplayReq = lsqReplayReqs.length
  val memUnitLsqReplayReqs = totalLdUnits.map(_.getReplayIssues()).flatten
  val numMemUnitLsqReplayReqs = memUnitLsqReplayReqs.length
  require(numLsqReplayReq == numMemUnitLsqReplayReqs,
    s"The number of lsq replay req(${numLsqReplayReq}) should be match memunit lsq replay " +
      s"req(${numMemUnitLsqReplayReqs})!")

  memUnitLsqReplayReqs.zip(lsqReplayReqs).foreach {
    case (sink, source) =>
      sink <> source
  }

  // feedback: [[MemUnit]] -> [[Backend]]
  val memUnitStaFeedbacks = totalStaUnits.map(_.io.toBackend.iqFeedback)
  val numMemUnitStaFeedback = memUnitStaFeedbacks.length
  val backendStaFeedbacks = toBackend.hyuIqFeedback ++ toBackend.staIqFeedback
  val numBackendStaFeedback = backendStaFeedbacks.length
  require(numMemUnitStaFeedback == numBackendStaFeedback,
    s"The number of memunit sta feedback(${numMemUnitStaFeedback}) should be match backend sta" +
      s"feedback(${numBackendStaFeedback})!")

  backendStaFeedbacks.zip(memUnitStaFeedbacks).foreach {
    case (sink, source) =>
      sink.feedbackFast <> source.feedbackFast // FIXME: DontCare is right?
      sink.feedbackSlow <> source.feedbackSlow
  }

  // sta issue feedback: [[StaUnits]] -> [[Backend]]
  val staIssueFeedbacks = totalStaUnits.map(_.io.toBackend.stIssue)
  val numStaIssueFeedback = staIssueFeedbacks.length
  val backendIssueFeedbacks = toBackend.stIssue
  val numBackendIssueFeedback = backendIssueFeedbacks.length
  require(numStaIssueFeedback == numBackendIssueFeedback,
    s"The number of sta issue feedback(${numStaIssueFeedback}) should be match backend issue " +
      s"feedback(${numBackendIssueFeedback})!")

  backendIssueFeedbacks.zip(staIssueFeedbacks).foreach {
    case (sink, source) =>
      sink <> source
  }

  // load feedback: [[MemUnits]] -> [[Backend]]
  val memUnitLdFeedbacks = totalLdUnits.map(_.io.toBackend.iqFeedback)
  val numMemUnitLdFeedback = memUnitLdFeedbacks.length
  val backendLdFeedbacks = toBackend.ldaIqFeedback ++ toBackend.hyuIqFeedback
  val numBackendLdFeedback = backendLdFeedbacks.length
  require(numMemUnitLdFeedback == numBackendLdFeedback,
    s"The number of memunit ld feedback(${numMemUnitLdFeedback}) should be match backend ld" +
      s"feedback(${numBackendLdFeedback})!")

  backendLdFeedbacks.zip(memUnitLdFeedbacks).foreach {
    case (sink, source) =>
      sink.feedbackFast <> source.feedbackFast
      sink.feedbackSlow <> source.feedbackSlow
  }

  // misalign out: [[StaUnits]] -> [[StoreMisalignBuffer]]
  val staMisalignBufResps = totalStaUnits.map(_.io.toStoreMisalignBuf).filter(_.isDefined).map(_.get.out)
    .flatten
  val numStaMisalignBufResp = staMisalignBufResps.length
  val storeMisalignBufResps = Seq(storeMisalignBuffer.io.splitStoreResp)
  val numStoreMisalignBufResp = storeMisalignBufResps.length
  require(numStaMisalignBufResp == numStoreMisalignBufResp,
    s"The number of sta misalign buf resp(${numStaMisalignBufResp}) should be match store misalign buf " +
      s"resp(${numStoreMisalignBufResp})!")

  storeMisalignBufResps.zip(staMisalignBufResps).foreach {
    case (sink, source) =>
      sink.valid := source.valid
      sink.bits := source.bits
      source.ready := true.B
  }

  // misalign req: [[StaUnits]] -> [[StoreMisalignBuffer]]
  val staMisalignBufReqs = totalStaUnits.map(_.io.toStoreMisalignBuf).filter(_.isDefined).map(_.get.enq)
  val numStaMisalignBufReq = staMisalignBufReqs.length
  val storeMisalignBufReqs = storeMisalignBuffer.io.req
  val numStoreMisalignBufReq = storeMisalignBufReqs.length
  require(numStaMisalignBufReq == numStoreMisalignBufReq,
    s"The number of sta misalign buf req(${numStaMisalignBufReq}) should be match store misalign buf " +
      s"req(${numStoreMisalignBufReq})!")

  storeMisalignBufReqs.zip(staMisalignBufReqs).foreach {
    case (sink, source) =>
      sink <> source
  }

  // misalign writeback: [[LdaUnits]] -> [[LoadMisalignBuffer]]
  val ldaMisalignBufResps = totalLdUnits.map(_.io.toLoadMisalignBuf).filter(_.isDefined).map(_.get.out)
    .flatten
  val numLdaMisalignBufResp = ldaMisalignBufResps.length
  val loadMisalignBufResps = Seq(loadMisalignBuffer.io.splitLoadResp)
  val numLoadMisalignBufResp = loadMisalignBufResps.length
  require(numLdaMisalignBufResp == numLoadMisalignBufResp,
    s"The number of ldu misalign buf resp(${numLdaMisalignBufResp}) should be match load misalign buf " +
      s"resp(${numLoadMisalignBufResp})!")

  loadMisalignBufResps.zip(ldaMisalignBufResps).foreach {
    case (sink, source) =>
      sink.valid := source.valid
      sink.bits := source.bits
      source.ready := true.B
  }

  // misalign req: [[LdaUnits]] -> [[LoadMisalignBuffer]]
  val ldaMisalignBufReqs = totalLdUnits.map(_.io.toLoadMisalignBuf).filter(_.isDefined).map(_.get.enq)
  val numLdaMisalignBufReq = ldaMisalignBufReqs.length
  val loadMisalignBufReqs = loadMisalignBuffer.io.req
  val numLoadMisalignBufReq = loadMisalignBufReqs.length
  require(numLdaMisalignBufReq == numLoadMisalignBufReq,
    s"The number of ldu misalign buf req(${numLdaMisalignBufReq}) should be match load misalign buf " +
      s"req(${numLoadMisalignBufReq})!")

  loadMisalignBufReqs.zip(ldaMisalignBufReqs).foreach {
    case (sink, source) =>
      sink <> source
  }

  // tlb reqs: [[MemUnit]] -> [[TLB]]
  val memUnitTlbReqs = totalMemUnits.map(_.io.toTlb)
  val numMemUnitTlbReq = memUnitTlbReqs.length
  val tlbReqs = io.toTlb
  val numTlbReq = tlbReqs.length
  require(numMemUnitTlbReq == numTlbReq,
    s"The number of memunit tlb req(${numMemUnitTlbReq}) should be match tlb req(${numTlbReq})!")

  tlbReqs.zip(memUnitTlbReqs).foreach {
    case (sink, source) =>
      sink.req <> source.req
      sink.req_kill := source.req_kill
  }

  // tlb resps: [[TLB]] -> [[MemUnit]]
  val tlbResps = io.fromTlb
  val numTlbResp = tlbResps.length
  val memUnitTlbResps = totalMemUnits.map(_.io.fromTlb)
  val numMemUnitTlbResp = memUnitTlbResps.length
  require(numTlbResp == numMemUnitTlbResp,
    s"The number of tlb resp(${numTlbResp}) should be match memunit tlb resp(${numMemUnitTlbResp})!")

  memUnitTlbResps.zip(tlbResps).foreach {
    case (sink, source) =>
      sink <> source
  }

  // Pmp resps: [[PMP]] -> [[MemUnit]]
  val pmpResps = io.fromPmp
  val numPmpResp = pmpResps.length
  val memUnitPmpResps = totalMemUnits.map(_.io.fromPmp)
  val numMemUnitPmpResp = memUnitPmpResps.length
  require(numPmpResp == numMemUnitPmpResp,
    s"The number of pmp resp(${numPmpResp}) should be match memunit pmp resp(${numMemUnitPmpResp})!")

  memUnitPmpResps.zip(pmpResps).foreach {
    case (sink, source) =>
      sink <> source
  }

  // DCache reqs: [[MemUnit]] -> [[DCache]]
  toDCache.zip(totalMemUnits.map(_.io.toDCache)).foreach {
    case (sink, source) =>
      sink <> source
  }

  // DCache resp: [[DCache]] -> [[MemUnit]]
  totalMemUnits.map(_.io.fromDCache).zip(fromDCache).foreach {
    case (sink, source) =>
      sink <> source
  }

  // to Backend
  totalLdUnits.map(_.io.toBackend.wakeup).zip(toBackend.wakeup).foreach {
    case (sink, source) =>
      sink <> source
  }
  totalLdUnits.map(_.io.toBackend.ldCancel).zip(toBackend.ldCancel).foreach {
    case (sink, source) =>
      sink <> source
  }
  totalLdUnits.map(_.io.toBackend.rollback).zip(toBackend.rollback).foreach {
    case (sink, source) =>
      sink <> source
  }

  // to Lsq
  toLsq.out.zip(totalLdUnits.map(_.io.toLsq.out).filter(_.isDefined)).foreach {
    case (sink, source) =>
      sink <> source.get
  }
  toLsq.forward.zip(totalLdUnits.map(_.io.toLsq.forward).filter(_.isDefined)).foreach {
    case (sink, source) =>
      sink <> source.get
  }
  toLsq.rarNuke.zip(totalLdUnits.map(_.io.toLsq.rarNuke).filter(_.isDefined)).foreach {
    case (sink, source) =>
      sink <> source.get
  }
  toLsq.rawNuke.zip(totalLdUnits.map(_.io.toLsq.rawNuke).filter(_.isDefined)).foreach {
    case (sink, source) =>
      sink <> source.get
  }
  toLsq.addrUpdate.zip(totalStaUnits.map(_.io.toLsq.addrUpdate).filter(_.isDefined)).foreach {
    case (sink, source) =>
      sink <> source.get
  }
  toLsq.excpUpdate.zip(totalStaUnits.map(_.io.toLsq.excpUpdate).filter(_.isDefined)).foreach {
    case (sink, source) =>
      sink <> source.get
  }
  toLsq.maskOut.zip(totalStaUnits.map(_.io.toLsq.maskOut).filter(_.isDefined)).foreach {
    case (sink, source) =>
      sink <> source.get
  }

  // from Lsq
  totalLdUnits.map(_.io.fromLsq).filter(_.isDefined).zip(fromLsq.forward).foreach {
    case (sink, source) =>
      sink.get.forward <> source
  }
  totalLdUnits.map(_.io.fromLsq).filter(_.isDefined).zip(fromLsq.rarNuke).foreach {
    case (sink, source) =>
      sink.get.rarNuke <> source
  }
  totalLdUnits.map(_.io.fromLsq).filter(_.isDefined).zip(fromLsq.rawNuke).foreach {
    case (sink, source) =>
      sink.get.rawNuke <> source
  }

  // uncache forward reqs: [[MemUnit]] -> [[Uncache]]
  val memUnitUncacheForwardReqs = totalLdUnits.map(_.io.toUncache).filter(_.isDefined)
  val numMemUnitUncacheForwardReq = memUnitUncacheForwardReqs.length
  val uncacheForwardReqs = toUncache
  val numUncacheForwardReq = uncacheForwardReqs.length
  require(numMemUnitUncacheForwardReq == numUncacheForwardReq,
    s"The number of memunit uncache forward req(${numMemUnitUncacheForwardReq}) should be match uncache forward " +
      s"req(${numUncacheForwardReq})!")

  uncacheForwardReqs.zip(memUnitUncacheForwardReqs).foreach {
    case (sink, source) =>
      sink <> source.get
  }

  // uncache forward resps: [[Uncache]] -> [[MemUnit]]
  val uncacheForwardResps = fromUncache
  val numUncacheForwardResp = uncacheForwardResps.length
  val memUnitUncacheForwardResps = totalLdUnits.map(_.io.fromUncache).filter(_.isDefined)
  val numMemUnitUncacheForwardResp = memUnitUncacheForwardResps.length
  require(numUncacheForwardResp == numMemUnitUncacheForwardResp,
    s"The number of uncache forward resp(${numUncacheForwardResp}) should be match memunit uncache forward " +
      s"resp(${numMemUnitUncacheForwardResp})!")

  memUnitUncacheForwardResps.zip(uncacheForwardResps).foreach {
    case (sink, source) =>
      sink.get <> source
  }

  // sbuffer forward reqs: [[MemUnit]] -> [[Sbuffer]]
  val memUnitSbufferForwardReqs = totalLdUnits.map(_.io.toSBuffer).filter(_.isDefined)
  val numMemUnitSbufferForwardReq = memUnitSbufferForwardReqs.length
  val sbufferForwardReqs = toSBuffer
  val numSbufferForwardReq = sbufferForwardReqs.length
  require(numMemUnitSbufferForwardReq == numSbufferForwardReq,
    s"The number of memunit sbuffer forward req(${numMemUnitSbufferForwardReq}) should be match sbuffer forward " +
      s"req(${numSbufferForwardReq})!")

  memUnitSbufferForwardReqs.zip(sbufferForwardReqs).foreach {
    case (sink, source) =>
      sink.get <> source
  }

  // sbuffer forward resps: [[Sbuffer]] -> [[MemUnit]]
  val sbufferForwardResps = fromSBuffer
  val numSbufferForwardResp = sbufferForwardResps.length
  val memUnitSbufferForwardResps = totalLdUnits.map(_.io.fromSBuffer.get)
  val numMemUnitSbufferForwardResp = memUnitSbufferForwardResps.length
  require(numSbufferForwardResp == numMemUnitSbufferForwardResp,
    s"The number of sbuffer forward resp(${numSbufferForwardResp}) should be match memunit sbuffer forward " +
      s"resp(${numMemUnitSbufferForwardResp})!")

  memUnitSbufferForwardResps.zip(sbufferForwardResps).foreach {
    case (sink, source) =>
      sink <> source
  }

  // MissQueue forward reqs: [[MemUnit]] -> [[MissQueue]]
  val memUnitMissQueueForwardReqs = totalLdUnits.map(_.io.toMissQueue.get)
  val numMemUnitMissQueueForwardReq = memUnitMissQueueForwardReqs.length
  val missQueueForwardReqs = toMissQueue
  val numMissQueueForwardReq = missQueueForwardReqs.length
  require(numMemUnitMissQueueForwardReq == numMissQueueForwardReq,
    s"The number of memunit missQueue forward req(${numMemUnitMissQueueForwardReq}) should be match missQueue forward " +
      s"req(${numMissQueueForwardReq})!")

  memUnitMissQueueForwardReqs.zip(missQueueForwardReqs).foreach {
    case (sink, source) =>
      sink <> source
  }

  // MissQueue forward resps: [[MissQueue]] -> [[MemUnit]]
  val missQueueForwardResps = fromMissQueue
  val numMissQueueForwardResp = missQueueForwardResps.length
  val memUnitMissQueueForwardResps = totalLdUnits.map(_.io.fromMissQueue.get)
  val numMemUnitMissQueueForwardResp = memUnitMissQueueForwardResps.length
  require(numMissQueueForwardResp == numMemUnitMissQueueForwardResp,
    s"The number of missQueue forward resp(${numMissQueueForwardResp}) should be match memunit missQueue forward " +
      s"resp(${numMemUnitMissQueueForwardResp})!")

  memUnitMissQueueForwardResps.zip(missQueueForwardResps).foreach {
    case (sink, source) =>
      sink <> source
  }

  // TL D channel forward: [[TL]] -> [[MemUnit]]
  val tlDchannelForwardResps = fromTL
  val numTLDchannelForwardResp = tlDchannelForwardResps.length
  val memUnitTLDchannelResps = totalLdUnits.flatMap{ memUnit =>
    // Convert to List if it is an Option or directly return the value if not
    memUnit.io.fromTL.toList
  }
  val numMemUnitTLDchannelResp = memUnitTLDchannelResps.length
  require(numTLDchannelForwardResp == numMemUnitTLDchannelResp,
    s"The number of tlDchannel forward resp(${numTLDchannelForwardResp}) should be match memunit tlDchannel forward " +
      s"resp(${numMemUnitTLDchannelResp})!")

  memUnitTLDchannelResps.zip(tlDchannelForwardResps).foreach {
    case (sink, source) =>
      sink <> source
  }

  // storePipeEmpty: [[MemUnit]] -> [[VecExuBlock]]
  val staStorePipeEmpty = totalStaUnits.map(_.io.commonOut.storePipeEmpty)
  val numStaStorePipeEmtpy = staStorePipeEmpty.length
  val memUnitStorePipeEmpty = toVecExuBlock.storePipeEmpty
  val numMemUnitStorePipeEmpty = memUnitStorePipeEmpty.length
  require(numStaStorePipeEmtpy == numMemUnitStorePipeEmpty,
    s"The number of sta storePipeEmpty(${numStaStorePipeEmtpy}) should be match memunit " +
      s"storePipeEmpty(${numMemUnitStorePipeEmpty})!")

  staStorePipeEmpty.zip(memUnitStorePipeEmpty).foreach {
    case (sink, source) =>
      sink <> source
  }

  // AtomicsUnit: AtomicsUnit will override other control signials,
  // as atomics insts (LR/SC/AMO) will block the pipeline
  val s_normal +: s_atomics = Enum(StAddrCnt + 1)
  val state = RegInit(s_normal)
  val stAtomics = backendStaIssues.map(issue => issue.valid && FuType.storeIsAMO(issue.bits.uop.fuType))
  val stDataAtomics = stdUnitImps.map(_.getStdWritebacks()).flatten.map(issue =>
    issue.valid && FuType.storeIsAMO(issue.bits.uop.fuType)
  )
  val backendStaFeedback = toBackend.hyuIqFeedback ++ toBackend.staIqFeedback
  val atomicsExceptionInfo = WireInit(VecInit(Seq.fill(amoUnitImps.length)(0.U.asTypeOf(ValidIO(new ExceptionAddrIO)))))

  amoUnitImps.zipWithIndex.foreach {
    case (impl: AmoImp, i) =>
      // Tlb
      impl.io.fromTlb.resp.valid := false.B
      impl.io.fromTlb.resp.bits  := DontCare
      impl.io.toTlb.req.ready := tlbReqs.head.req.ready
      impl.io.fromPmp <> pmpResps.head

      // DCache
      impl.io.fromDCache := DontCare
      impl.io.toDCache.req.ready := false.B
      impl.io.amoDCacheIO <> io.amoDCacheIO

      // issue sta: [[Backend]] -> [[AtomicsUnit]]
      impl.io.fromBackend.issue.head.valid := stAtomics.reduce(_|_)
      impl.io.fromBackend.issue.head.bits.fromMemExuInputBundle(Mux1H(
        stAtomics.zip(backendStaIssues).map(x => x._1 -> x._2.bits)
      ))
      impl.io.fromBackend.issue.head.bits.isIq := true.B
      impl.io.fromBackend.issue.head.bits.isAtomic := true.B

      // issue std: [[Backend]] -> [[AtomicsUnit]]
      val stdWritebacks = stdUnitImps.map(_.getStdWritebacks()).flatten
      impl.io.storeDataIn.zipWithIndex.foreach {
        case (storeDataIn, wbIdx) =>
          storeDataIn.valid := stDataAtomics(wbIdx)
          storeDataIn.bits := stdWritebacks(wbIdx).bits.toMemExuOutputBundle()
      }

      // feedback: [[AtomicsUnit]] -> [[Backend]]
      backendStaFeedback.zip(memUnitStaFeedbacks).zip(s_atomics).foreach {
        case ((sink, source), amoState) =>
          when(state === amoState) {
            sink.feedbackFast <> impl.io.toBackend.iqFeedback.feedbackFast
            assert(!source.feedbackFast.valid && !source.feedbackFast.bits.isLoad)
          }
      }

      when (state =/= s_normal) {
        val amoLoad = ldUnitImps.head.asInstanceOf[HyuImp]
        // use store wb port instead of load
        amoLoad.getLdWritebacks().head.ready := false.B
        // use Load_0's TLB
        impl.io.toTlb <> tlbReqs.head
        impl.io.fromTlb <> tlbResps.head
        // hw prefetch should be disabled while executing atomic insts
        amoLoad.getPrefetchIssues().map(_.valid := false.B)
        // make sure there's no in-flight uops in load unit
        assert(!amoLoad.getLdWritebacks().head.valid)
      }

      when (impl.io.toBackend.writeback.head.valid) {
        assert(s_atomics.map(_ === state).reduce(_ || _))
        state := s_normal
      }

      when (DelayN(fromCtrl.redirect.valid, 10) && atomicsExceptionInfo(i).valid) {
        atomicsExceptionInfo(i).valid := false.B
      } .elsewhen (impl.io.exceptionInfo.valid) {
        atomicsExceptionInfo(i).valid := true.B
        atomicsExceptionInfo(i).bits := impl.io.exceptionInfo.bits
      }
      io.atomicsExceptionInfo := DontCare
      io.atomicsExceptionInfo := atomicsExceptionInfo(i)
  }

  memUnitStaIssues.zip(backendStaIssues).zip(stAtomics.zip(s_atomics)).zipWithIndex.foreach {
    case (((sink, source), (atomics, amoState)), i) =>
      when (atomics) {
        source.ready := amoUnitImps.head.io.fromBackend.issue.head.ready
        sink.valid := false.B

        state := amoState
        XSError(stAtomics.zipWithIndex.filterNot(_._2 == i).unzip._1.reduce(_ || _), "atomics issue connect fail!")
      }
  }


  val atomicsException = RegInit(false.B)
  when (DelayN(fromCtrl.redirect.valid, 10) && atomicsException) {
    atomicsException := false.B
  }.elsewhen (io.atomicsExceptionInfo.valid) {
    atomicsException := true.B
  }
  XSError(atomicsException && amoUnitImps.map(_.getAmoIssues()).flatten.map(_.valid).reduce(_|_),
    "new instruction before exception triggers\n")

  // overwrite misalignbuffer exception
  io.misalignExceptionInfo := Mux(
    loadMisalignBuffer.io.overwriteExpBuf.valid,
    loadMisalignBuffer.io.overwriteExpBuf,
    storeMisalignBuffer.io.overwriteExpBuf
  )


  // writeback: [[MemUnit]] -> [[Backend]]
  val memUnitStdWritebacks = stdUnitImps.map(_.getStdWritebacks()).flatten
  val numMemUnitStdWriteback = memUnitStdWritebacks.length
  val backendStdWritebacks = toBackend.writebackStd
  val numBackendStdWriteback = backendStdWritebacks.length
  require(numMemUnitStdWriteback == numBackendStdWriteback,
    s"The number of memunit std writeback(${numMemUnitStdWriteback}) should be match backend " +
      s"std writeback(${numBackendStdWriteback})!")

  backendStdWritebacks.zip(memUnitStdWritebacks).foreach {
    case (sink, source) =>
      sink.valid := source.valid
      sink.bits  := source.bits.toMemExuOutputBundle()
      sink.bits.mask.map(_ := 0.U)
      sink.bits.vdIdx.map(_ := 0.U)
      sink.bits.vdIdxInField.map(_ := 0.U)
      source.ready := sink.ready
  }

  // vector load writeback
  val memUnitVldWritebacks = totalLdUnits.map(_.getVldWritebacks()).flatten
  val numMemUnitVldWriteback = memUnitVldWritebacks.length
  val vecExuBlockVldWritebacks = toVecExuBlock.vldWriteback
  val numVecExuBlockVldWriteback = vecExuBlockVldWritebacks.length
  require(numMemUnitVldWriteback == numVecExuBlockVldWriteback,
    s"The number of memunit vld writeback(${numMemUnitVldWriteback}) should be match vecExuBlock " +
      s"vld writeback(${numVecExuBlockVldWriteback})!")

  vecExuBlockVldWritebacks.zip(memUnitVldWritebacks).foreach {
    case (sink, source) =>
      sink <> source
  }

  // vector store writebacks
  val memUnitVstWritebacks = totalStaUnits.map(_.getVstWritebacks()).flatten
  val numMemUnitVstWriteback = memUnitVstWritebacks.length
  val vecExuBlockVstWritebacks = toVecExuBlock.vstWriteback
  val numVecExuBlockVstWriteback = vecExuBlockVstWritebacks.length
  require(numMemUnitVstWriteback == numVecExuBlockVstWriteback,
    s"The number of memunit vst writeback(${numMemUnitVstWriteback}) should be match vecExuBlock " +
      s"vst writeback(${numVecExuBlockVstWriteback})!")

  vecExuBlockVstWritebacks.zip(memUnitVstWritebacks).foreach {
    case (sink, source) =>
      sink <> source
  }

  // sta writeback
  val memUnitStaWritebacks = totalStaUnits.map(_.getStaWritebacks()).flatten
  val numMemUnitStaWriteback = memUnitStaWritebacks.length
  val backendStaWritebacks = toBackend.writebackHyuSta ++ toBackend.writebackSta
  val numBackendStaWriteback = backendStaWritebacks.length
  require(numMemUnitStaWriteback == numBackendStaWriteback,
    s"The number of memunit sta writeback(${numMemUnitStaWriteback}) should be match backend " +
      s"sta writeback(${numBackendStaWriteback})!")

  // mmio store writeback will use store writeback port 0
  val mmioStWriteback = WireInit(0.U.asTypeOf(fromLsq.mmioStWriteback.cloneType))
  NewPipelineConnect(
    fromLsq.mmioStWriteback, mmioStWriteback, mmioStWriteback.fire,
    false.B,
    Option("mmioStWriteback Connect")
  )
  mmioStWriteback.ready := false.B

  // store misalign buffer will overwrite store writeback port 0
  val storeMisalignCanWriteBack = !mmioStWriteback.valid && !memUnitStaWritebacks.head.valid && !memUnitVstWritebacks.head.valid
  storeMisalignBuffer.io.writeBack.ready := storeMisalignCanWriteBack
  storeMisalignBuffer.io.storeOutValid := memUnitStaWritebacks.head.valid
  storeMisalignBuffer.io.storeVecOutValid := memUnitVstWritebacks.head.valid

  backendStaWritebacks.zip(memUnitStaWritebacks).zipWithIndex.foreach {
    case ((sink, source), i) =>
      sink.valid := source.valid
      sink.bits := source.bits.toMemExuOutputBundle()
      source.ready := sink.ready

      if (i == 0) {
        when (mmioStWriteback.valid && !source.valid) {
          sink <> mmioStWriteback
        } .elsewhen (storeMisalignCanWriteBack) {
          sink <> storeMisalignBuffer.io.writeBack
        }
      }
  }

  // ldu
  val ldaExeWbReqs = Wire(Vec(LduCnt + HyuCnt, Decoupled(new MemExuOutput)))
  val ldWritebacks = totalLdUnits.map(_.getLdWritebacks()).flatten
  val vldWritebacks = totalLdUnits.map(_.getVldWritebacks()).flatten
  // atomicsUnit will overwrite the source from ldu if it is about to writeback
  val ldAtomicWriteback = ldWritebacks(AtomicWBPort)
  val ldAtomicOut = Wire(new MemExuOutput)
  ldAtomicOut := amoUnitImps.head.io.toBackend.writeback.head.bits.toMemExuOutputBundle()

  val atomicWritebackOverride = Mux(
    amoUnitImps.head.io.toBackend.writeback.head.valid,
    ldAtomicOut,
    ldAtomicWriteback.bits.toMemExuOutputBundle()
  )
  ldaExeWbReqs(AtomicWBPort).valid :=  amoUnitImps.head.io.toBackend.writeback.head.valid || ldAtomicWriteback.valid
  ldaExeWbReqs(AtomicWBPort).bits  := atomicWritebackOverride
  amoUnitImps.head.io.toBackend.writeback.head.ready := ldaExeWbReqs(AtomicWBPort).ready
  ldAtomicWriteback.ready := ldaExeWbReqs(AtomicWBPort).ready

  // overwrite ldu writeback port for loadMisalignBuffer writeback
  val ldMisalignWriteback = ldWritebacks(MisalignWBPort)
  val vldMisalignWriteback = vldWritebacks(MisalignWBPort)
  val ldMisalignOut = Wire(new MemExuOutput)
  ldMisalignOut := loadMisalignBuffer.io.writeBack.bits

  val misalignWritebackOverride = Mux(
    ldMisalignWriteback.valid,
    ldMisalignWriteback.bits.toMemExuOutputBundle(),
    ldMisalignOut
  )
  ldaExeWbReqs(MisalignWBPort).valid := loadMisalignBuffer.io.writeBack.valid || ldMisalignWriteback.valid
  ldaExeWbReqs(MisalignWBPort).bits := misalignWritebackOverride
  ldaExeWbReqs(MisalignWBPort).bits.isFromLoadUnit := true.B
  loadMisalignBuffer.io.writeBack.ready := ldaExeWbReqs(MisalignWBPort).ready && !ldMisalignWriteback.valid
  loadMisalignBuffer.io.loadOutValid := ldMisalignWriteback.valid
  loadMisalignBuffer.io.loadVecOutValid := vldMisalignWriteback.valid
  ldMisalignWriteback.ready := ldaExeWbReqs(MisalignWBPort).ready

  // loadUnit will overwrite the source from uncache if it is about to writeback
  val ldUncacheWriteback = ldWritebacks(UncacheWBPort)
  ldaExeWbReqs(UncacheWBPort).valid := ldUncacheWriteback.valid
  ldaExeWbReqs(UncacheWBPort).bits := ldUncacheWriteback.bits.toMemExuOutputBundle()
  ldUncacheWriteback.ready := ldaExeWbReqs(UncacheWBPort).ready

  val memUnitLdaWritebacks = ldaExeWbReqs
  val numMemUnitLdaWriteback = memUnitLdaWritebacks.length
  val backendLdaWritebacks = toBackend.writebackLda ++ toBackend.writebackHyuLda
  val numBackendLdaWriteback = backendLdaWritebacks.length
  require(numMemUnitLdaWriteback == numBackendLdaWriteback,
    s"The number of memunit ldu writeback(${numMemUnitLdaWriteback}) should be match backend " +
      s"ldu writeback(${numBackendLdaWriteback})!")

  backendLdaWritebacks.zip(memUnitLdaWritebacks).foreach {
    case (sink, source) =>
      sink <> source
  }

  // debugLsInfo: [[MemUnit]] -> [[Backend]]
  val memUnitDebugLsInfo = totalMemUnits.map(_.io.debugLsInfo)
  val numMemUnitDebugLsInfo = memUnitDebugLsInfo.length
  val backendDebugLsInfo = io.debugLsInfo
  val numBackendDebugLsInfo = backendDebugLsInfo.length
  require(numMemUnitDebugLsInfo == numBackendDebugLsInfo,
    s"The number of memUnitDebugLsInfo should be match backendDebugLsInfo!")

  backendDebugLsInfo.zip(memUnitDebugLsInfo).foreach {
    case (sink, source) =>
      sink <> source
  }

  // Topdown: [[MemUnit]] -> [[Backend]]
  val memUnitTopdownInfo = totalLdUnits.map(_.io.lsTopdownInfo)
  val numMemUnitTopdownInfo = memUnitTopdownInfo.length
  val backendTopdownInfo = io.lsTopdownInfo
  val numBackendTopdownInfo = backendTopdownInfo.length
  require(numMemUnitTopdownInfo == numBackendTopdownInfo,
    "The number of memUnitTopdownInfo should be match backendTopdownInfo!")

  backendTopdownInfo.zip(memUnitTopdownInfo).foreach {
    case (sink, source) =>
      sink <> source
  }

  // performance events
  val perfEvents = totalMemUnits.map(_.asInstanceOf[MemUnitImp]).flatMap(_.getPerfEvents)
  generatePerfEvent()
}