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
  override def shouldBeInlined: Boolean = false
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
  val fromVecExuBlock = Flipped(new VecExuBlockToMemExuBlockIO)
  val fromLsq = Flipped(new LsqToMemExuBlockIO)
  val fromTlb = Vec(MemAddrExtCnt, new Bundle() {
    val resp = Flipped(DecoupledIO(new TlbResp(2)))
    val hint = Flipped(new TlbHintReq)
  })
  val fromUncache = Vec(LdExuCnt, Input(new LoadForwardRespBundle))
  val fromSBuffer = Vec(LdExuCnt, Input(new LoadForwardRespBundle))
  val fromMissQueue = Vec(LdExuCnt, Flipped(new MissQueueForwardRespBundle))
  val fromTL = Vec(LdExuCnt, Input(new DcacheToLduForwardIO))
  val fromDCache = Vec(MemAddrExtCnt, new DCacheLoadRespIO)
  val fromPmp = Vec(MemAddrExtCnt,  Flipped(new PMPRespBundle()))
  val fromPrefetch = Vec(MemAddrExtCnt, Flipped(DecoupledIO(new LsPipelineBundle)))

  // to
  val toBackend = new MemExuBlockToBackendIO
  val toVecExuBlock = Flipped(new MemExuBlockToVecExuBlockIO)
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
      impl.io.fromCtrl.redirect <> io.fromCtrl.redirect
      impl.io.fromCtrl.hartId <> io.fromCtrl.hartId
      impl.io.fromCtrl.csr <> io.fromCtrl.csr
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
      impl.io.fromCtrl.redirect <> io.fromCtrl.redirect
      impl.io.fromCtrl.hartId <> io.fromCtrl.hartId
      impl.io.fromCtrl.csr  <> io.fromCtrl.csr
      if (impl.io.fromCtrl.trigger.isDefined) {
        impl.io.fromCtrl.trigger.get <> io.fromCtrl.trigger
      }

    case _ =>
  }

  // ldUnits
  ldUnitImps.zipWithIndex.foreach {
    case (impl: HyuImp, i) =>
      impl.io.fromCtrl.redirect <> io.fromCtrl.redirect
      impl.io.fromCtrl.hartId <> io.fromCtrl.hartId
      impl.io.fromCtrl.csr <> io.fromCtrl.csr
      if (impl.io.fromCtrl.trigger.isDefined) {
        impl.io.fromCtrl.trigger.get <> io.fromCtrl.trigger
      }
      if (impl.io.correctMissTrain.isDefined) {
        impl.io.correctMissTrain.get := correctMissTrain
      }

    case _ =>
  }

  // hyUnits
  hyUnitImps.zipWithIndex.foreach {
    case (impl: HyuImp, i) =>
      impl.io.fromCtrl.redirect <> io.fromCtrl.redirect
      impl.io.fromCtrl.hartId <> io.fromCtrl.hartId
      impl.io.fromCtrl.csr <> io.fromCtrl.csr
      if (impl.io.fromCtrl.trigger.isDefined) {
        impl.io.fromCtrl.trigger.get <> io.fromCtrl.trigger
      }
      if (impl.io.correctMissTrain.isDefined) {
        impl.io.correctMissTrain.get := correctMissTrain
      }

    case _ =>
  }

  amoUnitImps.zipWithIndex.foreach {
    case (impl: AmoImp, i) =>
      impl.io.fromCtrl.redirect <> io.fromCtrl.redirect
      impl.io.fromCtrl.hartId <> io.fromCtrl.hartId
      impl.io.fromCtrl.csr <> io.fromCtrl.csr
      if (impl.io.fromCtrl.trigger.isDefined) {
        impl.io.fromCtrl.trigger.get <> io.fromCtrl.trigger
      }
      impl.io.flushSbuffer <> io.flushSbuffer

    case _ =>
  }

 // misalignBuffer
  loadMisalignBuffer.io.redirect <> io.fromCtrl.redirect
  loadMisalignBuffer.io.rob.lcommit := io.fromBackend.robLsqIO.lcommit
  loadMisalignBuffer.io.rob.scommit := io.fromBackend.robLsqIO.scommit
  loadMisalignBuffer.io.rob.pendingMMIOld := io.fromBackend.robLsqIO.pendingMMIOld
  loadMisalignBuffer.io.rob.pendingld := io.fromBackend.robLsqIO.pendingld
  loadMisalignBuffer.io.rob.pendingst := io.fromBackend.robLsqIO.pendingst
  loadMisalignBuffer.io.rob.pendingVst := io.fromBackend.robLsqIO.pendingVst
  loadMisalignBuffer.io.rob.commit := io.fromBackend.robLsqIO.commit
  loadMisalignBuffer.io.rob.pendingPtr := io.fromBackend.robLsqIO.pendingPtr
  loadMisalignBuffer.io.rob.pendingPtrNext := io.fromBackend.robLsqIO.pendingPtrNext
  loadMisalignBuffer.io.vecWriteBack <> io.toVecExuBlock.loadMisalignBuffer.writeback
  io.toLsq.loadMisalignFull := loadMisalignBuffer.io.loadMisalignFull

  //
  storeMisalignBuffer.io.redirect <> io.fromCtrl.redirect
  storeMisalignBuffer.io.rob.lcommit := io.fromBackend.robLsqIO.lcommit
  storeMisalignBuffer.io.rob.scommit := io.fromBackend.robLsqIO.scommit
  storeMisalignBuffer.io.rob.pendingMMIOld := io.fromBackend.robLsqIO.pendingMMIOld
  storeMisalignBuffer.io.rob.pendingld := io.fromBackend.robLsqIO.pendingld
  storeMisalignBuffer.io.rob.pendingst := io.fromBackend.robLsqIO.pendingst
  storeMisalignBuffer.io.rob.pendingVst := io.fromBackend.robLsqIO.pendingVst
  storeMisalignBuffer.io.rob.commit := io.fromBackend.robLsqIO.commit
  storeMisalignBuffer.io.rob.pendingPtr := io.fromBackend.robLsqIO.pendingPtr
  storeMisalignBuffer.io.rob.pendingPtrNext := io.fromBackend.robLsqIO.pendingPtrNext
  storeMisalignBuffer.io.toVecStoreMergeBuffer <> io.toVecExuBlock.storeMisalignBuffer.control
  storeMisalignBuffer.io.vecWriteBack <> io.toVecExuBlock.storeMisalignBuffer.writeback
  storeMisalignBuffer.io.full <> io.toVecExuBlock.storeMisalignBuffer.storeMisalignBufferFull

  io.toLsq.sqControl <> storeMisalignBuffer.io.sqControl.toStoreQueue
  storeMisalignBuffer.io.sqControl.toStoreMisalignBuffer := io.fromLsq.maControl

  // issue: `Backend` -> `MemUnit`
  // std issue: `Backend` -> `StdUnits`
  Connection.connect(
    sinkSeq     = stdUnitImps.map(_.getStdIssues()).flatten,
    sourceSeq   = io.fromBackend.issueStd,
    connectFn   = Some((sink: DecoupledIO[LsPipelineBundle], source: DecoupledIO[MemExuInput]) => {
      sink.valid := source.valid
      source.ready := sink.ready
      sink.bits.fromMemExuInputBundle(source.bits, isStore = true)
      sink.bits.isIq := true.B
      sink.bits.isFirstIssue := true.B
    }),
    connectName = "StdUnits issues"
  )

  // sta issue: `Backend` -> `StaUnits`
  Connection.connect(
    sinkSeq     = totalStaUnits.map(_.getStaIssues()).flatten,
    sourceSeq   = io.fromBackend.issueHya ++ io.fromBackend.issueSta,
    connectFn   = Some((sink: DecoupledIO[LsPipelineBundle], source: DecoupledIO[MemExuInput]) => {
      sink.valid := source.valid
      source.ready := sink.ready
      sink.bits.fromMemExuInputBundle(source.bits, isStore = true)
      sink.bits.isIq := true.B
      sink.bits.isFirstIssue := true.B
    }),
    connectName = "StaUnits issues"
  )

  // raw nuke check: `StaUnits` -> `LdUnits/HyUnits`
  totalLdUnits.foreach {
    case ldu =>
      ldu.io.fromSta.foreach {
        case nukeQuery =>
          Connection.connect(
            sinkSeq     = nukeQuery,
            sourceSeq   = totalStaUnits.map(_.io.toLdu.nukeQuery).filter(_.isDefined).flatten,
            connectName = "nukeQuery"
          )
      }
  }

  // ldu issue: `Backend` -> `LdUnits/HyUnits`
  Connection.connect(
    sinkSeq     = totalLdUnits.map(_.getLdIssues()).flatten,
    sourceSeq   = io.fromBackend.issueLda ++ io.fromBackend.issueHya,
    connectFn   = Some((sink: DecoupledIO[LsPipelineBundle], source: DecoupledIO[MemExuInput]) => {
      sink.valid := source.valid
      source.ready := sink.ready
      sink.bits.fromMemExuInputBundle(source.bits, isStore = false)
      sink.bits.isIq := true.B
      sink.bits.isFirstIssue := true.B
    }),
    connectName = "LdUnits/HyUnits issues"
  )

  // prefetch issue: `Prefetch` -> `MemUnit`
  Connection.connect(
    sinkSeq     = totalMemUnits.map(_.getPrefetchIssues()).flatten,
    sourceSeq   = io.fromPrefetch,
    connectName = "Prefetch issues"
  )

  // prefetch train: `MemUnit` -> `Prefetch`
  val memUnitPrefetchTrains = totalMemUnits.flatMap { memUnit =>
    // Convert to List if it is an Option or directly return the value if not
    memUnit.io.toPrefetch.train.toList
  }
  Connection.connect(
    sinkSeq     = io.toPrefetch.train,
    sourceSeq   = memUnitPrefetchTrains,
    connectName = "Prefetch train"
  )

  // prefetch train l1: `MemUnit` -> `Prefetch`
  val memUnitPrefetchTrainL1s = totalMemUnits.flatMap { memUnit =>
    // Convert to List if it is an Option or directly return the value if not
    memUnit.io.toPrefetch.trainL1.toList
  }
  Connection.connect(
    sinkSeq     = io.toPrefetch.trainL1,
    sourceSeq   = memUnitPrefetchTrainL1s,
    connectName = "Prefetch train l1"
  )

  // prefetch ifetch
  Connection.connect(
    sinkSeq     = io.toPrefetch.ifetch,
    sourceSeq   = totalLdUnits.map(_.io.toPrefetch.ifetch.get),
    connectName = "Prefetch ifetch"
  )

  // misalign issue: `StoreMisalignBuffer` -> `StaUnits`
  Connection.connect(
    sinkSeq     = totalStaUnits.map(_.getMisalignIssues()).flatten,
    sourceSeq   = Seq(storeMisalignBuffer.io.splitStoreReq),
    connectName = "StoreMisalignBuffer issues"
  )

  // misalign issue: `LoadMisalignBuffer` -> `LdaUnits`
  Connection.connect(
    sinkSeq     = totalLdUnits.map(_.getMisalignIssues()).flatten,
    sourceSeq   = Seq(loadMisalignBuffer.io.splitLoadReq),
    connectName = "LoadMisalignBuffer issues"
  )

  // vector issue: `Vector` -> `MemUnit`
  Connection.connect(
    sinkSeq     = totalStaUnits.map(_.getVstIssues()).flatten,
    sourceSeq   = io.fromVecExuBlock.vectorStoreIssues,
    connectName = "Vector store issues"
  )

  Connection.connect(
    sinkSeq     = totalLdUnits.map(_.getVldIssues()).flatten,
    sourceSeq   = io.fromVecExuBlock.vectorLoadIssues,
    connectName = "Vector load issues"
  )

  // fast replay reqs: `MemUnit` -> `MemUnit`
  Connection.connect(
    sinkSeq     = totalLdUnits.map(_.getFastReplayIssues()).flatten,
    sourceSeq   = totalLdUnits.map(_.io.toLdu.replay.get),
    connectName = "Fast replay resp"
  )

  // mmio reqs: `Lsq` -> `MemUnit`
  Connection.connect(
    sinkSeq     = totalLdUnits.map(_.getMmioIssues()).flatten,
    sourceSeq   = Seq(io.fromLsq.mmioLdWriteback),
    connectFn   = Some((sink: DecoupledIO[LsPipelineBundle], source: DecoupledIO[MemExuOutput]) => {
      sink.valid := source.valid
      source.ready := sink.ready
      sink.bits.fromMemExuOutputBundle(source.bits)
    }),
    connectName = "Mmio writeback"
  )

  // mmio load data: `Lsq` -> `MemUnit`
  totalLdUnits.map(_.io.fromLsq).filter(_.isDefined).foreach {
    case sink =>
      Connection.connect(
        sinkSeq     = Seq(sink.get.mmioLdWriteback),
        sourceSeq   = Seq(io.fromLsq.mmioLdWriteback),
        connectName = "MMIO writeback"
      )
      sink.get.mmioLdData <> io.fromLsq.mmioLdData
  }

  // mmio reqs: `Lsq` -> `MemUnit`
  Connection.connect(
    sinkSeq     = totalLdUnits.map(_.getMmioIssues()).flatten,
    sourceSeq   = Seq(io.fromLsq.mmioLdWriteback),
    connectFn   = Some((sink: DecoupledIO[LsPipelineBundle], source: DecoupledIO[MemExuOutput]) => {
      sink.valid := source.valid
      source.ready := sink.ready
      sink.bits.fromMemExuOutputBundle(source.bits)
      sink.bits.clearIssueState()
      sink.bits.mmio := true.B
      sink.bits.isMmio := true.B
    }),
    connectName = "MMIO issue"
  )

  // nc reqs: `Lsq` -> `MemUnit`
  Connection.connect(
    sinkSeq     = totalLdUnits.map(_.getUncacheIssues()).flatten,
    sourceSeq   = io.fromLsq.ncOut,
    connectFn   = Some((sink: DecoupledIO[LsPipelineBundle], source: DecoupledIO[LsPipelineBundle]) => {
      sink <> source
      sink.bits.clearIssueState()
      sink.bits.isNoncacheable := true.B
      sink.bits.nc := true.B
    }),
    connectName = "Noncacheable issues"
  )

  // lsq replay reqs: `Lsq` -> `MemUnit`
  Connection.connect(
    sinkSeq     = totalLdUnits.map(_.getReplayIssues()).flatten,
    sourceSeq   = io.fromLsq.replay,
    connectName = "Lsq replay"
  )

  // feedback: `MemUnit` -> `Backend`
  Connection.connect(
    sinkSeq     = io.toBackend.hyuIqFeedback ++ io.toBackend.staIqFeedback,
    sourceSeq   = totalStaUnits.map(_.io.toBackend.iqFeedback),
    connectName = "StaUnit feedback"
  )

  // sta issue feedback: `StaUnits` -> `Backend`
  Connection.connect(
    sinkSeq     = io.toBackend.stIssue,
    sourceSeq   = totalStaUnits.map(_.io.toBackend.stIssue),
    connectName = "StaUnit issue to backend"
  )

  // load feedback: `MemUnits` -> `Backend`
  Connection.connect(
    sinkSeq     = io.toBackend.ldaIqFeedback ++ io.toBackend.hyuIqFeedback,
    sourceSeq   = totalLdUnits.map(_.io.toBackend.iqFeedback),
    connectName = "LdUnits/HyUnits feedback"
  )

  // misalign req: `StaUnits` -> `StoreMisalignBuffer`
  Connection.connect(
    sinkSeq     = storeMisalignBuffer.io.req,
    sourceSeq   = totalStaUnits.map(_.io.toStoreMisalignBuf).filter(_.isDefined).map(_.get.enq),
    connectName = "StoreMisalignBuffer enq"
  )

  // misalign out: `StaUnits` -> `StoreMisalignBuffer`
  Connection.connect(
    sinkSeq     = Seq(storeMisalignBuffer.io.splitStoreResp),
    sourceSeq   = totalStaUnits.map(_.io.toStoreMisalignBuf).filter(_.isDefined).map(_.get.out).flatten,
    connectFn   = Some((sink: ValidIO[LsPipelineBundle], source: DecoupledIO[LsPipelineBundle]) => {
      sink.valid := source.valid
      sink.bits := source.bits
      source.ready := true.B
    }),
    connectName = "StoreMisalignBuffer out"
  )

  // misalign writeback: `LdaUnits` -> `LoadMisalignBuffer`
  Connection.connect(
    sinkSeq     = Seq(loadMisalignBuffer.io.splitLoadResp),
    sourceSeq   = totalLdUnits.map(_.io.toLoadMisalignBuf).filter(_.isDefined).map(_.get.out).flatten,
    connectFn   = Some((sink: ValidIO[LsPipelineBundle], source: DecoupledIO[LsPipelineBundle]) => {
      sink.valid := source.valid
      sink.bits := source.bits
      source.ready := true.B
    }),
    connectName = "LoadMisalignBuffer writeback"
  )

  // misalign req: `LdaUnits` -> `LoadMisalignBuffer`
  Connection.connect(
    sinkSeq     = loadMisalignBuffer.io.req,
    sourceSeq   = totalLdUnits.map(_.io.toLoadMisalignBuf).filter(_.isDefined).map(_.get.enq),
    connectName = "LoadMisalignBuffer enq"
  )

  // tlb reqs: `MemUnit` -> `TLB`
  io.toTlb.zip(totalMemUnits.map(_.io.toTlb)).foreach {
    case (sink, source) =>
      sink.req <> source.req
      sink.req_kill := source.req_kill
  }

  // tlb resps: `TLB` -> `MemUnit`
  Connection.connect(
    sinkSeq     = totalMemUnits.map(_.io.fromTlb.resp),
    sourceSeq   = io.fromTlb.map(_.resp),
    connectName = "TLB resp"
  )

  Connection.connect(
    sinkSeq     = totalMemUnits.map(_.io.fromTlb.hint),
    sourceSeq   = io.fromTlb.map(_.hint),
    connectName = "TLB resp"
  )

  // Pmp resps: `PMP` -> `MemUnit`
  Connection.connect(
    sinkSeq     = totalMemUnits.map(_.io.fromPmp),
    sourceSeq   = io.fromPmp,
    connectName = "DCache req"
  )

  // DCache reqs: `MemUnit` -> `DCache`
  io.toDCache.zip(totalMemUnits.map(_.io.toDCache)).foreach {
    case (sink, source) =>
      sink <> source
  }

  // DCache resp: `DCache` -> `MemUnit`
  totalMemUnits.map(_.io.fromDCache).zip(io.fromDCache).foreach {
    case (sink, source) =>
      sink <> source
  }

  // to Backend
  Connection.connect(
    sinkSeq     = io.toBackend.wakeup,
    sourceSeq   = totalLdUnits.map(_.io.toBackend.wakeup),
    connectName = "LdUnits/HyUnits wakeup"
  )

  Connection.connect(
    sinkSeq     = io.toBackend.ldCancel,
    sourceSeq   = totalLdUnits.map(_.io.toBackend.ldCancel),
    connectName = "LdUnits/HyUnits Cancel"
  )

  Connection.connect(
    sinkSeq     = io.toBackend.rollback,
    sourceSeq   = totalLdUnits.map(_.io.toBackend.rollback),
    connectName = "LdUnits/HyUnits rollback"
  )

  // to Lsq
  Connection.connect(
    sinkSeq     = io.toLsq.out,
    sourceSeq   = totalLdUnits.map(_.io.toLsq.out).filter(_.isDefined).map(_.get),
    connectName = "LdUnits/HyUnits lsq out"
  )

  Connection.connect(
    sinkSeq     = io.toLsq.forward,
    sourceSeq   = totalLdUnits.map(_.io.toLsq.forward).filter(_.isDefined).map(_.get),
    connectName = "LdUnits/HyUnits sq forward req"
  )

  io.toLsq.rarNuke.zip(totalLdUnits.map(_.io.toLsq.rarNuke).filter(_.isDefined).map(_.get)).foreach {
    case (sink, source) =>
      sink <> source
  }
  io.toLsq.rawNuke.zip(totalLdUnits.map(_.io.toLsq.rawNuke).filter(_.isDefined).map(_.get)).foreach {
    case (sink, source) =>
      sink <> source
  }

  Connection.connect(
    sinkSeq     = io.toLsq.addrUpdate,
    sourceSeq   = totalStaUnits.map(_.io.toLsq.addrUpdate).filter(_.isDefined).map(_.get),
    connectName = "StaUnits/HyUnits addr update"
  )

  Connection.connect(
    sinkSeq     = io.toLsq.excpUpdate,
    sourceSeq   = totalStaUnits.map(_.io.toLsq.excpUpdate).filter(_.isDefined).map(_.get),
    connectName = "StaUnits/HyUnits excp update"
  )

  Connection.connect(
    sinkSeq     = io.toLsq.maskOut,
    sourceSeq   = totalStaUnits.map(_.io.toLsq.maskOut).filter(_.isDefined).map(_.get),
    connectName = "StaUnits/HyUnits mask out"
  )

  // from Lsq
  Connection.connect(
    sinkSeq     = totalLdUnits.map(_.io.fromLsq).filter(_.isDefined).map(_.get.forward),
    sourceSeq   = io.fromLsq.forward,
    connectName = "LdUnits/HyUnits sq forward resp"
  )

  Connection.connect(
    sinkSeq     = totalLdUnits.map(_.io.fromLsq).filter(_.isDefined).map(_.get.rarNuke),
    sourceSeq   = io.fromLsq.rarNuke,
    connectName = "LdUnits/HyUnits rar nuke"
  )

  Connection.connect(
    sinkSeq     = totalLdUnits.map(_.io.fromLsq).filter(_.isDefined).map(_.get.rawNuke),
    sourceSeq   = io.fromLsq.rawNuke,
    connectName = "LdUnits/HyUnits raw nuke"
  )

  // uncache forward reqs: `MemUnit` -> `Uncache`
  Connection.connect(
    sinkSeq     = io.toUncache,
    sourceSeq   = totalLdUnits.map(_.io.toUncache).filter(_.isDefined).map(_.get),
    connectName = "Uncache forward req"
  )

  // uncache forward resps: `Uncache` -> `MemUnit`
  Connection.connect(
    sinkSeq     = totalLdUnits.map(_.io.fromUncache).filter(_.isDefined).map(_.get),
    sourceSeq   = io.fromUncache,
    connectName = "Uncache forward resp"
  )

  // sbuffer forward reqs: `MemUnit` -> `Sbuffer`
  Connection.connect(
    sinkSeq     = io.toSBuffer,
    sourceSeq   = totalLdUnits.map(_.io.toSBuffer).filter(_.isDefined).map(_.get),
    connectName = "Sbuffer forward req"
  )

  // sbuffer forward resps: `Sbuffer` -> `MemUnit`
  Connection.connect(
    sinkSeq     = totalLdUnits.map(_.io.fromSBuffer).filter(_.isDefined).map(_.get),
    sourceSeq   = io.fromSBuffer,
    connectName = "Sbuffer forward resp"
  )

  // MissQueue forward reqs: `MemUnit` -> `MissQueue`
  Connection.connect(
    sinkSeq     = io.toMissQueue,
    sourceSeq   = totalLdUnits.map(_.io.toMissQueue).filter(_.isDefined).map(_.get),
    connectName = "MissQueue forward req"
  )

  // MissQueue forward resps: `MissQueue` -> `MemUnit`
  Connection.connect(
    sinkSeq     = totalLdUnits.map(_.io.fromMissQueue).filter(_.isDefined).map(_.get),
    sourceSeq   = io.fromMissQueue,
    connectName = "MissQueue forward resp"
  )

  // TL D channel forward: `TL` -> `MemUnit`
  val memUnitTLDchannelResps = totalLdUnits.flatMap{ memUnit =>
    // Convert to List if it is an Option or directly return the value if not
    memUnit.io.fromTL.toList
  }
  Connection.connect(
    sinkSeq     = memUnitTLDchannelResps,
    sourceSeq   = io.fromTL,
    connectName = "TL D channel forward"
  )

  // storePipeEmpty: `MemUnit` -> `VecExuBlock`
  io.toVecExuBlock.storePipeEmpty.zip(totalStaUnits.map(_.io.commonOut.storePipeEmpty)).foreach {
    case (sink, source) =>
      sink <> source
  }

  // AtomicsUnit: AtomicsUnit will override other control signials,
  // as atomics insts (LR/SC/AMO) will block the pipeline
  val s_normal +: s_atomics = Enum(StAddrCnt + 1)
  val state = RegInit(s_normal)
  val memUnitStaIssues = totalStaUnits.map(_.getStaIssues()).flatten
  val memUnitStaFeedbacks = totalStaUnits.map(_.io.toBackend.iqFeedback)
  val backendStaIssues = io.fromBackend.issueHya ++ io.fromBackend.issueSta
  val stAtomics = backendStaIssues.map(issue => issue.valid && FuType.storeIsAMO(issue.bits.uop.fuType))
  val stDataAtomics = stdUnitImps.map(_.getStdWritebacks()).flatten.map(issue =>
    issue.valid && FuType.storeIsAMO(issue.bits.uop.fuType)
  )
  val backendStaFeedback = io.toBackend.hyuIqFeedback ++ io.toBackend.staIqFeedback
  val atomicsExceptionInfo = RegInit(VecInit(Seq.fill(amoUnitImps.length)(0.U.asTypeOf(ValidIO(new ExceptionAddrIO)))))
  io.atomicsExceptionInfo := DontCare

  amoUnitImps.zipWithIndex.foreach {
    case (impl: AmoImp, i) =>
      // Tlb
      impl.io.fromTlb.resp.valid := false.B
      impl.io.fromTlb.resp.bits  := DontCare
      impl.io.fromTlb.hint := DontCare
      impl.io.toTlb.req.ready := io.toTlb.head.req.ready
      impl.io.fromPmp <> io.fromPmp.head

      // DCache
      impl.io.fromDCache := DontCare
      impl.io.toDCache.req.ready := false.B
      impl.io.amoDCacheIO <> io.amoDCacheIO

      // issue sta: `Backend` -> `AtomicsUnit`
      impl.io.fromBackend.issue.head.valid := stAtomics.reduce(_|_)
      impl.io.fromBackend.issue.head.bits.fromMemExuInputBundle(Mux1H(
        stAtomics.zip(backendStaIssues).map(x => x._1 -> x._2.bits)
      ))
      impl.io.fromBackend.issue.head.bits.isIq := true.B
      impl.io.fromBackend.issue.head.bits.isAtomic := true.B

      // issue std: `Backend` -> `AtomicsUnit`
      val stdWritebacks = stdUnitImps.map(_.getStdWritebacks()).flatten
      impl.io.storeDataIn.zipWithIndex.foreach {
        case (storeDataIn, wbIdx) =>
          storeDataIn.valid := stDataAtomics(wbIdx)
          storeDataIn.bits := stdWritebacks(wbIdx).bits.toMemExuOutputBundle()
      }

      // feedback: `AtomicsUnit` -> `Backend`
      backendStaFeedback.zip(memUnitStaFeedbacks).zip(s_atomics).foreach {
        case ((sink, source), amoState) =>
          when(state === amoState) {
            sink.feedbackFast <> impl.io.toBackend.iqFeedback.feedbackSlow
            assert(!source.feedbackFast.valid && !source.feedbackFast.bits.isLoad)
          }
      }

      when (state =/= s_normal) {
        val amoLoad = ldUnitImps.head.asInstanceOf[HyuImp]
        // use store wb port instead of load
        amoLoad.getLdWritebacks().head.ready := false.B
        // use Load_0's TLB
        impl.io.toTlb <> io.toTlb.head
        impl.io.fromTlb <> io.fromTlb.head
        // hw prefetch should be disabled while executing atomic insts
        amoLoad.getPrefetchIssues().map(_.valid := false.B)
        // make sure there's no in-flight uops in load unit
        assert(!amoLoad.getLdWritebacks().head.valid)
      }

      when (impl.io.toBackend.writeback.head.valid) {
        assert(s_atomics.map(_ === state).reduce(_ || _))
        state := s_normal
      }

      when (DelayN(io.fromCtrl.redirect.valid, 10) && atomicsExceptionInfo(i).valid) {
        atomicsExceptionInfo(i).valid := false.B
      } .elsewhen (impl.io.exceptionInfo.valid) {
        atomicsExceptionInfo(i).valid := true.B
        atomicsExceptionInfo(i).bits := impl.io.exceptionInfo.bits
      }
      io.atomicsExceptionInfo := atomicsExceptionInfo(i)
  }

  memUnitStaIssues.zip(backendStaIssues).zip(stAtomics.zip(s_atomics)).zipWithIndex.foreach {
    case (((sink, source), (atomics, amoState)), i) =>
      when (atomics) {
        source.ready := amoUnitImps.head.io.fromBackend.issue.head.ready
        sink.valid := false.B

        state := amoState
      }
      XSError(atomics && stAtomics.zipWithIndex.filterNot(_._2 == i).unzip._1.reduce(_ || _),
        "atomics issue connect fail!")
  }

  val atomicsException = RegInit(false.B)
  when (DelayN(io.fromCtrl.redirect.valid, 10) && atomicsException) {
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


  // writeback: `MemUnit` -> `Backend`
  Connection.connect(
    sinkSeq     = io.toBackend.writebackStd,
    sourceSeq   = stdUnitImps.map(_.getStdWritebacks()).flatten,
    connectFn   = Some((sink: DecoupledIO[MemExuOutput], source: DecoupledIO[LsPipelineBundle]) => {
      sink.valid := source.valid
      sink.bits  := source.bits.toMemExuOutputBundle()
      sink.bits.mask.map(_ := 0.U)
      sink.bits.vdIdx.map(_ := 0.U)
      sink.bits.vdIdxInField.map(_ := 0.U)
      source.ready := sink.ready
    }),
    connectName = "StdUnits writeback"
  )

  // vector load writeback
  Connection.connect(
    sinkSeq     = io.toVecExuBlock.vldWriteback,
    sourceSeq   = totalLdUnits.map(_.getVldWritebacks()).flatten,
    connectName = "VldUnits writeback to vector execute block"
  )

  // vector store writebacks
  Connection.connect(
    sinkSeq     = io.toVecExuBlock.vstWriteback,
    sourceSeq   = totalStaUnits.map(_.getVstWritebacks()).flatten,
    connectName = "VstUnits writeback to vector execute block"
  )

  // mmio store writeback will use store writeback port 0
  val mmioStWriteback = WireInit(0.U.asTypeOf(io.fromLsq.mmioStWriteback.cloneType))
  NewPipelineConnect(
    io.fromLsq.mmioStWriteback, mmioStWriteback, mmioStWriteback.fire,
    false.B,
    Option("mmioStWriteback Connect")
  )
  mmioStWriteback.ready := false.B

  // sta writeback
  val memUnitStaWritebacks = totalStaUnits.map(_.getStaWritebacks()).flatten
  val numMemUnitStaWriteback = memUnitStaWritebacks.length
  val backendStaWritebacks = io.toBackend.writebackHyuSta ++ io.toBackend.writebackSta
  val numBackendStaWriteback = backendStaWritebacks.length
  require(numMemUnitStaWriteback == numBackendStaWriteback,
    s"The number of memunit sta writeback(${numMemUnitStaWriteback}) should be match backend " +
      s"sta writeback(${numBackendStaWriteback})!")

  // store misalign buffer will overwrite store writeback port 0
  val memUnitVstWritebacks = totalStaUnits.map(_.getVstWritebacks()).flatten
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
          mmioStWriteback.ready := true.B
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

  Connection.connect(
    sinkSeq     = io.toBackend.writebackLda ++ io.toBackend.writebackHyuLda,
    sourceSeq   = ldaExeWbReqs,
    connectName = "LdUnits/HyUnits writeback"
  )

  // debugLsInfo: `MemUnit` -> `Backend`
  Connection.connect(
    sinkSeq     = io.debugLsInfo,
    sourceSeq   = totalMemUnits.map(_.io.debugLsInfo),
    connectName = "DebugLsInfo"
  )

  // Topdown: `MemUnit` -> `Backend`
  Connection.connect(
    sinkSeq     = io.lsTopdownInfo,
    sourceSeq   = totalLdUnits.map(_.io.lsTopdownInfo),
    connectName = "Topdown"
  )

  // performance events
  val perfEvents = totalMemUnits.map(_.asInstanceOf[MemUnitImp]).flatMap(_.getPerfEvents)
  generatePerfEvent()
}
