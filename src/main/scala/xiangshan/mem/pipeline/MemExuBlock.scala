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
import xiangshan.backend.ctrlblock.LsTopdownInfo
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

  lazy val module = new MemExuBlockImp(this)
}

class BackendToMemExuBlockIO(implicit p: Parameters) extends MemBlockBundle {
  val issueLda = MixedVec(Seq.fill(LduCnt)(Flipped(DecoupledIO(new MemExuInput))))
  val issueSta = MixedVec(Seq.fill(StaCnt)(Flipped(DecoupledIO(new MemExuInput))))
  val issueStd = MixedVec(Seq.fill(StdCnt)(Flipped(DecoupledIO(new MemExuInput))))
  val issueHya = MixedVec(Seq.fill(HyuCnt)(Flipped(DecoupledIO(new MemExuInput))))
  val stIssue  = Vec(StAddrCnt, ValidIO(new MemExuInput))
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

  val lsTopdownInfo = Vec(LdExuCnt, Output(new LsTopdownInfo))
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
  val maControl  = Flipped(new StoreMaBufToSqCtrlIO)
  val flushFrmMaBuf = Input(Bool())
}

class LsqToMemExuBlockIO(implicit p: Parameters) extends MemBlockBundle {
  val forward  = Vec(LdExuCnt, ValidIO(new LoadForwardRespBundle))
  val rarNuke  = Vec(LdExuCnt, ValidIO(new LoadNukeQueryRespBundle))
  val rawNuke  = Vec(LdExuCnt, ValidIO(new LoadNukeQueryRespBundle))
  val replay   = Vec(LdExuCnt, DecoupledIO(new LsPipelineBundle))
  val uncache  = Flipped(DecoupledIO(new LsPipelineBundle))
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
  val fromLsq     = Flipped(new LsqToMemExuBlockIO)
  val fromTlb     = Vec(MemAddrExtCnt, new Bundle() {
    val resp = Flipped(DecoupledIO(new TlbResp(2)))
    val hint = Flipped(new TlbHintReq)
  })
  val fromSBuffer    = Vec(LdExuCnt, Flipped(ValidIO(new LoadForwardRespBundle)))
  val fromMissQueue  = Vec(LdExuCnt, Flipped(ValidIO(new MissQueueForwardRespBundle)))
  val fromTLDchannel = Input(new DcacheToLduForwardIO)
  val fromDCache = Vec(MemAddrExtCnt, new DCacheLoadRespIO)
  val fromPmp  = Vec(MemAddrExtCnt,  Flipped(new PMPRespBundle()))
  val fromPrefetch = Vec(MemAddrExtCnt, DecoupledIO(new LsPipelineBundle))

  // to
  val toBackend   = new MemExuBlockToBackendIO
  val toLsq       = new MemExuBlockToLsqIO
  val toDCache    = Vec(MemAddrExtCnt, new DCacheLoadRespIO)
  val toTlb       = Vec(MemAddrExtCnt, new Bundle() {
    val req = DecoupledIO(new TlbReq)
    val req_kill = Output(Bool())
  })
  val toSBuffer   = Vec(LdExuCnt, ValidIO(new LoadForwardReqBundle))
  val toMissQueue = Vec(LdExuCnt, ValidIO(new MissQueueForwardReqBundle))
  val toPrefetch  = new Bundle() {
    val ifetch  = Vec(LdExuCnt, ValidIO(new SoftIfetchPrefetchBundle))
    val train   = Vec(LdExuCnt, new LsPrefetchTrainIO)
    val trainL1 = Vec(LdExuCnt, new LsPrefetchTrainIO)
  }
  val amoDCacheIO = Flipped(new AtomicWordIO)
  val atomicsExceptionInfo = ValidIO(new ExceptionAddrIO)
  val misalignExceptionInfo = ValidIO(new ExceptionAddrIO)
}

class MemExuBlockImp(wrapper: MemExuBlock) extends LazyModuleImp(wrapper)
  with HasXSParameter
  with HasMemBlockParameters
{
  val io = IO(new MemExuBlockIO)

  private val fromCtrl = io.fromCtrl
  private val fromBackend  = io.fromBackend

  private val toLsq = io.toLsq
  private val toBackend = io.toBackend

  val stdUnitImps     = wrapper.stdUnits.map(_.module)

  // stdunits
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

  require(stdUnitImps.map(_.io.fromIssue).flatten.length == fromBackend.issueStd.length,
          "The number of issueStd does not match!")
  stdUnitImps.map(_.io.fromIssue).flatten.zip(fromBackend.issueStd).map {
    case (sink, source) =>
      sink.valid := source.valid
      sink.bits.fromMemExuInputBundle(source.bits)
      source.ready := sink.ready
  }
  require(stdUnitImps.map(_.io.toIssue).flatten.length == toBackend.writebackStd.length,
          "The number of writebackStd does not match!")
  toBackend.writebackStd.zip(stdUnitImps.map(_.io.toIssue).flatten).map {
    case (sink, source) =>
      sink.valid := source.valid
      sink.bits  := source.bits.toMemExuOutputBundle()
      source.ready := sink.ready
  }
}