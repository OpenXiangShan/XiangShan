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
import freechips.rocketchip.diplomacy.BufferParams
import utils._
import utility._
import xiangshan._
import xiangshan.ExceptionNO._
import xiangshan.backend.Bundles._
import xiangshan.backend.datapath.NewPipelineConnect
import xiangshan.backend.fu.{FuType, PMPRespBundle}
import xiangshan.backend.fu.util.SdtrigExt
import xiangshan.backend.fu.vector.Bundles.{VConfig, VType}
import xiangshan.backend.fu.NewCSR._
import xiangshan.backend.fu.vector.Utils.VecDataToMaskDataVec
import xiangshan.backend.rob.RobPtr
import xiangshan.mem._
import xiangshan.mem.Bundles._
import xiangshan.cache.mmu._
import xiangshan.cache._
import xiangshan.cache.wpu.ReplayCarry


class fromMemExuBlockIO(implicit p: Parameters) extends XSBundle with HasMemBlockParameters {
  val vectorStoreWriteback = Vec(VstuCnt, Flipped(DecoupledIO(new LsPipelineBundle)))
  val vectorLoadWriteback = Vec(VlduCnt, Flipped(DecoupledIO(new LsPipelineBundle)))
}

class toMemExuBlockIO(implicit p: Parameters) extends XSBundle with HasMemBlockParameters {
  val vectorLoadIssues  = Vec(VlduCnt, DecoupledIO(new LsPipelineBundle))
  val vectorStoreIssues = Vec(VstuCnt, DecoupledIO(new LsPipelineBundle))
  val vectorDataOut = Vec(VstuCnt, ValidIO(new MemExuOutput(isVector = true)))
}

class VecExuBlockIO(implicit p: Parameters) extends XSBundle with HasMemBlockParameters {
  // from
  val fromCtrl = new Bundle() {
    val hartId    = Input(UInt(hartIdLen.W))
    val redirect  = Flipped(ValidIO(new Redirect))
    val csrCtrl   = Flipped(new CustomCSRCtrlIO)
    val trigger   = Input(new CsrTriggerBundle)
  }
  val fromBackend = new Bundle() {
    val issueVldu = MixedVec(Seq.fill(VlduCnt)(Flipped(DecoupledIO(new MemExuInput(isVector=true)))))
  }
  val fromMemExuBlock = new fromMemExuBlockIO
  val fromDCache = new DCacheLoadRespIO
  val fromTlb    = Flipped(DecoupledIO(new TlbResp(2)))
  val fromPmp    = Flipped(new PMPRespBundle())

  // to
  val toBackend = new Bundle() {
    val vstuIqFeedback = Vec(VstuCnt, new MemRSFeedbackIO(isVector = true))
    val vlduIqFeedback = Vec(VlduCnt, new MemRSFeedbackIO(isVector = true))
    val writebackVldu = Vec(VlduCnt, DecoupledIO(new MemExuOutput(isVector = true)))
  }
  val toLsq = new Bundle() {
    val ldvecFeedback = Vec(VecLoadPipelineWidth, ValidIO(new FeedbackToLsqIO))
    val stvecFeedback = Vec(VecStorePipelineWidth, ValidIO(new FeedbackToLsqIO))
    val vstd = Vec(VstuCnt, ValidIO(new MemExuOutput(isVector = true)))
  }
  val toMemExuBlock = new toMemExuBlockIO
  val toDCache = new DCacheLoadReqIO
  val toTlb     = new Bundle() {
    val req = DecoupledIO(new TlbReq)
    val req_kill = Output(Bool())
  }
  val exceptionInfo = ValidIO(new FeedbackToLsqIO)
  val sbuffer = Decoupled(new DCacheWordReqWithVaddrAndPfFlag)
  val flushSbuffer = new SbufferFlushBundle
  val vecDifftestInfo = Decoupled(new DynInst)
  val vSegmentFlag = Output(Bool())
}

class VecExuBlock(implicit p: Parameters) extends XSModule
  with HasMemBlockParameters
{

  val io = IO(new VecExuBlockIO)

  private val fromCtrl = io.fromCtrl
  private val (fromBackend, toBackend) = (io.fromBackend, io.toBackend)
  private val (fromMemExuBlock, toMemExuBlock) = (io.fromMemExuBlock, io.toMemExuBlock)
  private val (fromDCache, toDCache) = (io.fromDCache, io.toDCache)
  private val (fromTlb, toTlb) = (io.fromTlb, io.toTlb)
  private val fromPmp = io.fromPmp
  private val toLsq = io.toLsq

  // The number of vector load/store units is decoupled with the number of load/store units
  val vlSplit = Seq.fill(VlduCnt)(Module(new VLSplitImp))
  val vsSplit = Seq.fill(VstuCnt)(Module(new VSSplitImp))
  val vlMergeBuffer = Module(new VLMergeBufferImp)
  val vsMergeBuffer = Seq.fill(VstuCnt)(Module(new VSMergeBufferImp))
  val vSegmentUnit  = Module(new VSegmentUnit)
  val vfofBuffer    = Module(new VfofBuffer)

  val vSegmentFlag = RegInit(false.B)

  // vector
  val vlsuCanAccept = (0 until VlduCnt).map(
    i => vsSplit(i).io.in.ready && vlSplit(i).io.in.ready
  )
  val isSegment     = fromBackend.issueVldu.head.valid && FuType.isVsegls(fromBackend.issueVldu.head.bits.uop.fuType)
  val isFixVlUop    = fromBackend.issueVldu.map{x =>
    x.bits.uop.vpu.isVleff && x.bits.uop.vpu.lastUop && x.valid
  }

  when (vSegmentUnit.io.in.fire) {
    vSegmentFlag := true.B
  } .elsewhen (vSegmentUnit.io.uopwriteback.valid) {
    vSegmentFlag := false.B
  }
  io.vSegmentFlag := vSegmentFlag

  vlMergeBuffer.io.redirect <> fromCtrl.redirect
  vsMergeBuffer.map(_.io.redirect <> fromCtrl.redirect)
  vSegmentUnit.io.redirect <> fromCtrl.redirect
  vSegmentUnit.io.fromCsrTrigger <> fromCtrl.trigger
  vfofBuffer.io.redirect <> fromCtrl.redirect


  // init port
  /**
   * TODO: splited vsMergebuffer maybe remove, if one RS can accept two feedback, or don't need RS replay uop
   * for now:
   *  RS0 -> VsSplit0 -> stu0 -> vsMergebuffer0 -> feedback -> RS0
   *  RS1 -> VsSplit1 -> stu1 -> vsMergebuffer1 -> feedback -> RS1
   *
   * vector load don't need feedback
   *
   *  RS0 -> VlSplit0  -> ldu0 -> |
   *  RS1 -> VlSplit1  -> ldu1 -> |  -> vlMergebuffer
   *        replayIO   -> ldu3 -> |
   * */
  (0 until VstuCnt).foreach{i =>
    vsMergeBuffer(i).io.fromPipeline := DontCare
    vsMergeBuffer(i).io.fromSplit := DontCare
  }
  (0 until VlduCnt).foreach{i =>
    //Subsequent instrction will be blocked
    vfofBuffer.io.in(i).valid := fromBackend.issueVldu(i).valid
    vfofBuffer.io.in(i).bits  := fromBackend.issueVldu(i).bits
  }

  // vector store issue: [[Backend]] -> [[MemExuBlock]]
  vsSplit.zip(toMemExuBlock.vectorStoreIssues).zipWithIndex.foreach {
    case ((impl: VSSplitImp, stu), i) =>
      impl.io.redirect <> fromCtrl.redirect
      impl.io.in <> fromBackend.issueVldu(i)
      impl.io.in.valid := fromBackend.issueVldu(i).valid &&
                          VstuType.isVecSt(fromBackend.issueVldu(i).bits.uop.fuOpType) &&
                          vlsuCanAccept(i) && !isSegment
      impl.io.toMergeBuffer <> vsMergeBuffer(i).io.fromSplit.head
      impl.io.vstd.get := DontCare // TODO: Discuss how to pass vector store data

      val vecStIn = Wire(DecoupledIO(new VecPipeBundle))
      stu.valid := vecStIn.valid
      stu.bits.fromVecPipeBundle(vecStIn.bits, isStore = true)
      vecStIn.ready := stu.ready

      NewPipelineConnect(
        impl.io.out, vecStIn, vecStIn.fire,
        Mux(impl.io.out.fire, impl.io.out.bits.uop.robIdx.needFlush(fromCtrl.redirect), vecStIn.bits.uop.robIdx.needFlush(fromCtrl.redirect)),
        Option("VsSplitConnectStu" + i)
      )
    case _ =>
  }

  // vector load issue: [[Backend]] -> [[MemExuBlock]]
  vlSplit.zip(toMemExuBlock.vectorLoadIssues).zipWithIndex.foreach {
    case ((impl: VLSplitImp, ldu), i) =>
      impl.io.redirect <> fromCtrl.redirect
      impl.io.in <> fromBackend.issueVldu(i)
      impl.io.in.valid := fromBackend.issueVldu(i).valid &&
                          VlduType.isVecLd(fromBackend.issueVldu(i).bits.uop.fuOpType) &&
                          vlsuCanAccept(i) && !isSegment && !isFixVlUop(i)
      impl.io.toMergeBuffer <> vlMergeBuffer.io.fromSplit(i)

      val vecLdIn = Wire(DecoupledIO(new VecPipeBundle))
      ldu.valid := vecLdIn.valid
      ldu.bits.fromVecPipeBundle(vecLdIn.bits)
      vecLdIn.ready := ldu.ready

      NewPipelineConnect(
        impl.io.out, vecLdIn, vecLdIn.fire,
        Mux(impl.io.out.fire, impl.io.out.bits.uop.robIdx.needFlush(fromCtrl.redirect), vecLdIn.bits.uop.robIdx.needFlush(fromCtrl.redirect)),
        Option("VlSplitConnectLdu" + i)
      )
    case _ =>
  }

  // vlsu issue: [[Backend]] -> [[VecExuBlock]]
  fromBackend.issueVldu.zip(vlsuCanAccept).foreach {
    case (sink, source) =>
      sink.ready := source
  }

  // vl writeback: [[MemExuBlock]] -> [[vlMergeBuffer]]
  vlMergeBuffer.io.fromPipeline.zip(fromMemExuBlock.vectorLoadWriteback).foreach {
    case (sink, source) =>
      sink.valid := source.valid
      sink.bits  := source.bits.toVecPipelineFeedbackBundle()
      source.ready := sink.ready
  }

  // vs writeback: [[MemExuBlock]] -> [[vsMergeBuffer]]
  vsMergeBuffer.map(_.io.fromPipeline.head).zip(fromMemExuBlock.vectorStoreWriteback).foreach {
    case (sink, source) =>
      sink.valid := source.valid
      sink.bits  := source.bits.toVecPipelineFeedbackBundle(isVStore = true)
      source.ready := sink.ready
  }

  // vl feedback: [[vlMergeBuffer]] -> [[Lsq]]
  toLsq.ldvecFeedback.zip(vlMergeBuffer.io.toLsq).foreach {
    case (sink, source) =>
      sink <> source
  }

  // vs feedback: [[vsMergeBuffer]] -> [[Lsq]]
  toLsq.stvecFeedback.zip(vsMergeBuffer.map(_.io.toLsq.head)).foreach {
    case (sink, source) =>
      sink <> source
  }

  toLsq.vstd.zip(vsSplit).zipWithIndex.foreach {
    case ((sink, source), i) =>
      sink <> source.io.vstd.get
  }

  // vldu feedback: [[vlMergeBuffer]] -> [[Backend]]
  toBackend.vlduIqFeedback.zip(vlMergeBuffer.io.feedback).foreach {
    case (sink, source) =>
      sink.feedbackFast := DontCare
      sink.feedbackSlow <> source
  }

  // vstu feedback: [[vsMergeBuffer]] -> [[Backend]]
  toBackend.vstuIqFeedback.zip(vsMergeBuffer.map(_.io.feedback.head)).zipWithIndex.foreach {
    case ((sink, source), i) =>
      sink.feedbackFast := DontCare
      if (i == 0) {
        sink.feedbackSlow.valid := source.valid || vSegmentUnit.io.feedback.valid
        sink.feedbackSlow.bits  := Mux1H(Seq(
          vSegmentUnit.io.feedback.valid -> vSegmentUnit.io.feedback.bits,
          source.valid -> source.bits
        ))
      } else {
        sink.feedbackSlow <> source
      }
  }

  // vSegmentUnit issue: [[Backend]] -> [[vSegmentUnit]]
  vSegmentUnit.io.in <> fromBackend.issueVldu.head
  vSegmentUnit.io.in.valid := fromBackend.issueVldu.head.valid && !isSegment

  // dcache: [[vSegmentUnit]] <-> [[DCache]]
  connectSamePort(toDCache.req, vSegmentUnit.io.rdcache)
  connectSamePort(vSegmentUnit.io.rdcache, fromDCache.resp)

  // tlb: [[vSegmentUnit]] <-> [[Tlb]]
  toTlb.req <> vSegmentUnit.io.dtlb.req
  toTlb.req_kill := vSegmentUnit.io.dtlb.req_kill
  fromTlb <> vSegmentUnit.io.dtlb.resp

  // pmp: [[Pmp]] -> [[vSegmentUnit]]
  vSegmentUnit.io.pmpResp <> fromPmp

  //
  io.exceptionInfo <> vSegmentUnit.io.exceptionInfo
  io.sbuffer <> vSegmentUnit.io.sbuffer
  io.flushSbuffer <> vSegmentUnit.io.flush_sbuffer
  io.vecDifftestInfo <> vSegmentUnit.io.vecDifftestInfo

  // vldu writeback: [[vlMergeBuffer]] -> [[Backend]]
  val vlMergeBufferWriteback = vlMergeBuffer.io.uopWriteback
  val vsMergeBufferWriteback = vsMergeBuffer.map(_.io.uopWriteback.head)
  val vfofBufferWriteback    = vfofBuffer.io.mergeUopWriteback

  toBackend.writebackVldu.zip(vlMergeBufferWriteback.zip(vsMergeBufferWriteback).zip(vfofBufferWriteback)).zipWithIndex.foreach {
    case ((sink, ((vl, vs), vfof)), i) =>
      if (i == 0) {
        sink.valid := vl.valid || vs.valid || vSegmentUnit.io.uopwriteback.valid
        sink.bits  := PriorityMux(Seq(
          vSegmentUnit.io.uopwriteback.valid -> vSegmentUnit.io.uopwriteback.bits,
          vl.valid -> vl.bits,
          vs.valid -> vs.bits
        ))
        vSegmentUnit.io.uopwriteback.ready := sink.ready
        vl.ready := sink.ready && !vSegmentUnit.io.uopwriteback.valid
        vs.ready := sink.ready && !vSegmentUnit.io.uopwriteback.valid && !vl.valid
      } else if (i == 1) {
        sink.valid := vl.valid || vs.valid || vfofBuffer.io.uopWriteback.valid
        sink.bits  := PriorityMux(Seq(
          vfofBuffer.io.uopWriteback.valid -> vfofBuffer.io.uopWriteback.bits,
          vl.valid -> vl.bits,
          vs.valid -> vs.bits
        ))
        vfofBuffer.io.uopWriteback.ready := sink.ready
        vl.ready := sink.ready && !vfofBuffer.io.uopWriteback.valid
        vs.ready := sink.ready && !vfofBuffer.io.uopWriteback.valid && !vl.valid
      } else {
        sink.valid := vl.valid || vs.valid
        sink.bits := PriorityMux(Seq(
          vl.valid -> vl.bits,
          vs.valid -> vs.bits
        ))
        vl.ready := sink.ready
        vs.ready := sink.ready && !vl.valid
      }

      vfof.valid := vl.valid
      vfof.bits  := vl.bits
  }

  // vstu data out: [[vsSplit]] -> [[Lsq]]
  toMemExuBlock.vectorDataOut.zip(vsSplit.map(_.io.vstd.get)).foreach {
    case (sink, source) =>
      sink <> source
  }
}