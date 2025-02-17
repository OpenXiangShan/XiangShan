/***************************************************************************************
* Copyright (c) 2025 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2025 Institute of Computing Technology, Chinese Academy of Sciences
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
import xiangshan.cache._
import xiangshan.cache.mmu._
import xiangshan.cache.wpu.ReplayCarry

class FromMemExuBlockIO(implicit p: Parameters) extends XSBundle with HasMemBlockParameters {
  val vlWriteback = Vec(LoadPipelineWidth, Flipped(DecoupledIO(new LsPipelineBundle)))
  val vsWriteback = Vec(StorePipelineWidth, Flipped(DecoupledIO(new LsPipelineBundle)))
  val loadMisalignBuffer = new Bundle() {
    val writeback = Flipped(Decoupled(new VecPipelineFeedbackIO(isVStore = false)))
  }
  val storeMisalignBuffer = new Bundle() {
    val full = Input(Bool())
    val control = Vec(VecStorePipelineWidth, Flipped(new StoreMaBufToVecStoreMergeBufferIO))
    val writeback = Vec(VecStorePipelineWidth, Flipped(Decoupled(new VecPipelineFeedbackIO(isVStore = true))))
  }
  val storePipeEmpty = Input(Vec(HyuCnt + StaCnt, Bool()))
}

class ToMemExuBlockIO(implicit p: Parameters) extends XSBundle with HasMemBlockParameters {
  val vlIssues = Vec(VlduCnt, DecoupledIO(new VecPipeBundle(isVStore = false)))
  val vsIssues = Vec(VstuCnt, DecoupledIO(new VecPipeBundle(isVStore = true)))
}

class VecExuBlockIO(implicit p: Parameters) extends XSBundle with HasMemBlockParameters {
  val fromCtrl = new Bundle() {
    val hartId = Input(UInt(hartIdLen.W))
    val redirect = Flipped(ValidIO(new Redirect))
    val csr = Flipped(new CustomCSRCtrlIO)
    val trigger = Input(new CsrTriggerBundle)
  }
  // from
  val fromBackend = new Bundle() {
    val issueVldu = MixedVec(Seq.fill(VlduCnt)(Flipped(DecoupledIO(new MemExuInput(isVector=true)))))
  }
  val fromTlb = Flipped(DecoupledIO(new TlbResp(2)))
  val fromPmp = Flipped(new PMPRespBundle())
  val fromDCache = new DCacheLoadRespIO
  val fromMemExuBlock = new FromMemExuBlockIO
  val fromLsq = new Bundle() {
    val lqDeqPtr = Input(new LqPtr)
  }
  // to
  val toBackend = new Bundle() {
    val vlIqFeedback = Vec(VlduCnt, new MemRSFeedbackIO(isVector = true))
    val vsIqFeedback = Vec(VstuCnt, new MemRSFeedbackIO(isVector = true))
    val writebackVldu = Vec(VlduCnt, DecoupledIO(new MemExuOutput(isVector = true)))
  }
  val toTlb = new Bundle() {
    val req = DecoupledIO(new TlbReq)
    val req_kill = Output(Bool())
  }
  val toDCache = new DCacheLoadReqIO
  val toLsq = new Bundle() {
    val vlFeedback = Vec(VecLoadPipelineWidth, ValidIO(new FeedbackToLsqIO))
    val vsFeedback = Vec(VecStorePipelineWidth, ValidIO(new FeedbackToLsqIO))
    val vstd = Vec(VstuCnt, ValidIO(new MemExuOutput(isVector = true)))
  }
  val toSbuffer = Decoupled(new DCacheWordReqWithVaddrAndPfFlag)
  val toMemExuBlock = new ToMemExuBlockIO
  //
  val exceptionInfo = ValidIO(new FeedbackToLsqIO)
  val flushSbuffer = new SbufferFlushBundle
  val vecDifftestInfo = Decoupled(new DynInst)
  val vSegmentFlag = Output(Bool())
}

class VecExuBlock(implicit p: Parameters) extends XSModule with HasMemBlockParameters {
  val io = IO(new VecExuBlockIO)

  // The number of vector load/store units is decoupled with the number of load/store units
  val vlSplit = Seq.fill(VlduCnt)(Module(new VLSplitImp))
  val vsSplit = Seq.fill(VstuCnt)(Module(new VSSplitImp))
  val vlMergeBuffer = Module(new VLMergeBufferImp)
  val vsMergeBuffer = Seq.fill(VstuCnt)(Module(new VSMergeBufferImp))
  val vSegmentUnit  = Module(new VSegmentUnit)
  val vfofBuffer    = Module(new VfofBuffer)

  val vSegmentFlag = RegInit(false.B)

  // vector
  val vlCanAccept = (0 until VlduCnt).map(i =>
    vlSplit(i).io.in.ready && VlduType.isVecLd(io.fromBackend.issueVldu(i).bits.uop.fuOpType)
  )
  val vsCanAccept = (0 until VstuCnt).map(i =>
    vsSplit(i).io.in.ready && VstuType.isVecSt(io.fromBackend.issueVldu(i).bits.uop.fuOpType)
  )
  io.fromBackend.issueVldu.zip(vlCanAccept.zip(vsCanAccept)).foreach {
    case (issue, (vlReady, vsReady)) =>
      issue.ready := vlReady || vsReady
  }

  val isSegment = io.fromBackend.issueVldu.head.valid && FuType.isVsegls(io.fromBackend.issueVldu.head.bits.uop.fuType)
  val isFixVlUop = io.fromBackend.issueVldu.map{x =>
    x.bits.uop.vpu.isVleff && x.bits.uop.vpu.lastUop && x.valid
  }

  when (GatedValidRegNext(vSegmentUnit.io.in.fire)) {
    vSegmentFlag := true.B
  } .elsewhen (GatedValidRegNext(vSegmentUnit.io.uopwriteback.valid)) {
    vSegmentFlag := false.B
  }
  io.vSegmentFlag := vSegmentFlag

  vlMergeBuffer.io.redirect <> io.fromCtrl.redirect
  vsMergeBuffer.map(_.io.redirect <> io.fromCtrl.redirect)
  vSegmentUnit.io.redirect <> io.fromCtrl.redirect
  vSegmentUnit.io.fromCsrTrigger <> io.fromCtrl.trigger
  vfofBuffer.io.redirect <> io.fromCtrl.redirect

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
  // vector load
  (0 until VlduCnt).foreach{i =>
    vlSplit(i).io.redirect <> io.fromCtrl.redirect
    vlSplit(i).io.in <> io.fromBackend.issueVldu(i)
    vlSplit(i).io.in.valid := io.fromBackend.issueVldu(i).valid &&
      vlCanAccept(i) && !isSegment && !isFixVlUop(i)
    vlSplit(i).io.toMergeBuffer <> vlMergeBuffer.io.fromSplit(i)
    vlSplit(i).io.threshold.get.valid := vlMergeBuffer.io.toSplit.get.threshold
    vlSplit(i).io.threshold.get.bits  := io.fromLsq.lqDeqPtr

    NewPipelineConnect(
      vlSplit(i).io.out,
      io.toMemExuBlock.vlIssues(i),
      io.toMemExuBlock.vlIssues(i).fire,
      Mux(
        vlSplit(i).io.out.fire,
        vlSplit(i).io.out.bits.uop.robIdx.needFlush(io.fromCtrl.redirect),
        io.toMemExuBlock.vlIssues(i).bits.uop.robIdx.needFlush(io.fromCtrl.redirect)
      ),
      Option("VlSplitConnectLdu_"+i)
    )

    //Subsequent instrction will be blocked
    vfofBuffer.io.in(i).valid := io.fromBackend.issueVldu(i).valid
    vfofBuffer.io.in(i).bits  := io.fromBackend.issueVldu(i).bits
  }

  // vector load merge buffer
  vlMergeBuffer.io.fromPipeline.zip(io.fromMemExuBlock.vlWriteback).zipWithIndex.foreach {
    case ((vlMerge, vlWriteback), i) =>
      vlWriteback.ready := vlMerge.ready
      io.fromMemExuBlock.loadMisalignBuffer.writeback.ready := true.B

      vlMerge <> vlWriteback
      if (i == MisalignWBPort) {
        when (!vlWriteback.valid) {
          vlMerge <> io.fromMemExuBlock.loadMisalignBuffer.writeback
        }
      }
  }

  // vl feedback: [[vlMergeBuffer]] -> [[Lsq]]
  io.toLsq.vlFeedback <> vlMergeBuffer.io.toLsq

  // vl feedback: [[vlMergeBuffer]] -> [[Backend]]
  io.toBackend.vlIqFeedback.zip(vlMergeBuffer.io.feedback).foreach {
    case (sink, source) =>
      sink.feedbackFast := DontCare
      sink.feedbackSlow <> source
  }

  // vector store
  (0 until VstuCnt).foreach{i =>
    vsMergeBuffer(i).io.fromPipeline := DontCare
    vsMergeBuffer(i).io.fromSplit := DontCare
    vsMergeBuffer(i).io.fromMisalignBuffer.get.flush := io.fromMemExuBlock.storeMisalignBuffer.control(i).flush
    vsMergeBuffer(i).io.fromMisalignBuffer.get.mbIndex := io.fromMemExuBlock.storeMisalignBuffer.control(i).mbIndex
  }

  (0 until VstuCnt).foreach{ i =>
    vsSplit(i).io.redirect <> io.fromCtrl.redirect
    vsSplit(i).io.in <> io.fromBackend.issueVldu(i)
    vsSplit(i).io.in.valid := io.fromBackend.issueVldu(i).valid &&
      vsCanAccept(i) && !isSegment
    vsSplit(i).io.toMergeBuffer <> vsMergeBuffer(i).io.fromSplit.head

    NewPipelineConnect(
      vsSplit(i).io.out,
      io.toMemExuBlock.vsIssues(i),
      io.toMemExuBlock.vsIssues(i).fire,
      Mux(
        vsSplit(i).io.out.fire,
        vsSplit(i).io.out.bits.uop.robIdx.needFlush(io.fromCtrl.redirect),
        io.toMemExuBlock.vsIssues(i).bits.uop.robIdx.needFlush(io.fromCtrl.redirect)
      ),
      Option("VsSplitConnectStu_"+i)
    )

    vsSplit(i).io.vstd.get := DontCare // Todo: Discuss how to pass vector store data
    vsSplit(i).io.vstdMisalign.get.storeMisalignBufferEmpty := !io.fromMemExuBlock.storeMisalignBuffer.full
    vsSplit(i).io.vstdMisalign.get.storePipeEmpty := io.fromMemExuBlock.storePipeEmpty
  }

  // vector store merge buffer
  vsMergeBuffer.map(_.io.fromPipeline.head).zip(io.fromMemExuBlock.vsWriteback).zipWithIndex.foreach {
    case ((vsMerge, vsWriteback), i) =>
      if (i < VstuCnt) {
        vsWriteback.ready := true.B
        io.fromMemExuBlock.storeMisalignBuffer.writeback(i).ready := vsMerge.ready

        when (vsWriteback.valid) {
          vsMerge <> vsWriteback
        } .otherwise {
          vsMerge <> io.fromMemExuBlock.storeMisalignBuffer.writeback(i)
        }
      }
  }

  // vs feedback: [[vsMergeBuffer]] -> [[Lsq]]
  io.toLsq.vsFeedback <> VecInit(vsMergeBuffer.map(_.io.toLsq.head))

  // vstu feedback: [[vsMergeBuffer]] -> [[Backend]]
  io.toBackend.vsIqFeedback.zip(vsMergeBuffer.map(_.io.feedback.head)).zipWithIndex.foreach {
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

  // tlb: [[vSegmentUnit]] <-> [[Tlb]]
  io.toTlb.req <> vSegmentUnit.io.dtlb.req
  io.toTlb.req_kill := vSegmentUnit.io.dtlb.req_kill
  vSegmentUnit.io.dtlb.resp <> io.fromTlb

  // pmp: [[Pmp]] -> [[vSegmentUnit]]
  vSegmentUnit.io.pmpResp <> io.fromPmp

  // vSegmentUnit issue: [[Backend]] -> [[vSegmentUnit]]
  vSegmentUnit.io.in <> io.fromBackend.issueVldu.head
  vSegmentUnit.io.in.valid := io.fromBackend.issueVldu.head.valid && isSegment

  // dcache: [[vSegmentUnit]] -> [[DCache]]
  io.toDCache.req <> vSegmentUnit.io.rdcache.req
  io.toDCache.s1_kill := vSegmentUnit.io.rdcache.s1_kill
  io.toDCache.s1_kill_data_read := vSegmentUnit.io.rdcache.s1_kill_data_read
  io.toDCache.s2_kill := vSegmentUnit.io.rdcache.s2_kill
  io.toDCache.s0_pc := vSegmentUnit.io.rdcache.s0_pc
  io.toDCache.s1_pc := vSegmentUnit.io.rdcache.s1_pc
  io.toDCache.s2_pc := vSegmentUnit.io.rdcache.s2_pc
  io.toDCache.is128Req := vSegmentUnit.io.rdcache.is128Req
  io.toDCache.pf_source := vSegmentUnit.io.rdcache.pf_source
  io.toDCache.s1_paddr_dup_dcache := vSegmentUnit.io.rdcache.s1_paddr_dup_dcache
  io.toDCache.s1_paddr_dup_lsu := vSegmentUnit.io.rdcache.s1_paddr_dup_lsu
  io.toDCache.replacementUpdated := vSegmentUnit.io.rdcache.replacementUpdated

  vSegmentUnit.io.rdcache.resp <> io.fromDCache.resp
  vSegmentUnit.io.rdcache.s1_disable_fast_wakeup := io.fromDCache.s1_disable_fast_wakeup
  vSegmentUnit.io.rdcache.s2_hit := io.fromDCache.s2_hit
  vSegmentUnit.io.rdcache.s2_first_hit := io.fromDCache.s2_first_hit
  vSegmentUnit.io.rdcache.s2_bank_conflict := io.fromDCache.s2_bank_conflict
  vSegmentUnit.io.rdcache.s2_wpu_pred_fail := io.fromDCache.s2_wpu_pred_fail
  vSegmentUnit.io.rdcache.s2_mq_nack := io.fromDCache.s2_mq_nack
  vSegmentUnit.io.rdcache.debug_s1_hit_way := io.fromDCache.debug_s1_hit_way
  vSegmentUnit.io.rdcache.debug_s2_pred_way_num := io.fromDCache.debug_s2_pred_way_num
  vSegmentUnit.io.rdcache.debug_s2_dm_way_num := io.fromDCache.debug_s2_dm_way_num
  vSegmentUnit.io.rdcache.debug_s2_real_way_num := io.fromDCache.debug_s2_real_way_num

  io.exceptionInfo <> vSegmentUnit.io.exceptionInfo
  io.toSbuffer <> vSegmentUnit.io.sbuffer
  io.flushSbuffer <> vSegmentUnit.io.flush_sbuffer
  io.vecDifftestInfo <> vSegmentUnit.io.vecDifftestInfo

  // vector writeback: [[vMergeBuffer]] -> [[Backend]]
  val vlMergeBufferWriteback = vlMergeBuffer.io.uopWriteback
  val vsMergeBufferWriteback = vsMergeBuffer.map(_.io.uopWriteback.head)
  val vfofBufferWriteback = vfofBuffer.io.mergeUopWriteback

  io.toBackend.writebackVldu.zip(vlMergeBufferWriteback.zip(vsMergeBufferWriteback).zip(vfofBufferWriteback)).
    zipWithIndex.foreach {
    case ((sink, ((vlMerge, vsMerge), vfof)), i) =>
      if (i == 0) {
        sink.valid := vlMerge.valid || vsMerge.valid || vSegmentUnit.io.uopwriteback.valid
        sink.bits := PriorityMux(Seq(
          vSegmentUnit.io.uopwriteback.valid -> vSegmentUnit.io.uopwriteback.bits,
          vlMerge.valid -> vlMerge.bits,
          vsMerge.valid -> vsMerge.bits
        ))
        vSegmentUnit.io.uopwriteback.ready := sink.ready
        vlMerge.ready := sink.ready && !vSegmentUnit.io.uopwriteback.valid
        vsMerge.ready := sink.ready && !vSegmentUnit.io.uopwriteback.valid && !vlMerge.valid
      } else if (i == 1) {
        sink.valid := vlMerge.valid || vsMerge.valid || vfofBuffer.io.uopWriteback.valid
        sink.bits := PriorityMux(Seq(
          vfofBuffer.io.uopWriteback.valid -> vfofBuffer.io.uopWriteback.bits,
          vlMerge.valid -> vlMerge.bits,
          vsMerge.valid -> vsMerge.bits
        ))
        vfofBuffer.io.uopWriteback.ready := sink.ready
        vlMerge.ready := sink.ready && !vfofBuffer.io.uopWriteback.valid
        vsMerge.ready := sink.ready && !vfofBuffer.io.uopWriteback.valid && !vlMerge.valid
      } else {
        sink.valid := vlMerge.valid || vsMerge.valid
        sink.bits := PriorityMux(Seq(
          vlMerge.valid -> vlMerge.bits,
          vsMerge.valid -> vsMerge.bits
        ))
        vlMerge.ready := sink.ready
        vsMerge.ready := sink.ready && !vlMerge.valid
      }
      vfof.valid := vlMerge.valid
      vfof.bits := vlMerge.bits
  }
}
