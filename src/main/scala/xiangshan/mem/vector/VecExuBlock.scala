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

class MemExuBlockToVecExuBlockIO(implicit p: Parameters) extends XSBundle with HasMemBlockParameters {
  val vldWriteback = Vec(LoadPipelineWidth, Flipped(DecoupledIO(new LsPipelineBundle)))
  val vstWriteback = Vec(StorePipelineWidth, Flipped(DecoupledIO(new LsPipelineBundle)))
  val loadMisalignBuffer = new Bundle() {
    val writeback = Flipped(Decoupled(new VecPipelineFeedbackIO(isVStore = false)))
  }
  val storeMisalignBuffer = new Bundle() {
    val storeMisalignBufferFull = Input(Bool())
    val control = Vec(VecStorePipelineWidth, Flipped(new StoreMaBufToVecStoreMergeBufferIO))
    val writeback = Vec(VecStorePipelineWidth, Flipped(Decoupled(new VecPipelineFeedbackIO(isVStore = true))))
  }
  val storePipeEmpty = Input(Vec(HyuCnt + StaCnt, Bool()))
}

class VecExuBlockToMemExuBlockIO(implicit p: Parameters) extends XSBundle with HasMemBlockParameters {
  val vectorLoadIssues = Vec(VlduCnt, DecoupledIO(new LsPipelineBundle))
  val vectorStoreIssues = Vec(VstuCnt, DecoupledIO(new LsPipelineBundle))
}

class VecExuBlockIO(implicit p: Parameters) extends XSBundle with HasMemBlockParameters {
  // from
  val fromCtrl = new Bundle() {
    val hartId = Input(UInt(hartIdLen.W))
    val redirect = Flipped(ValidIO(new Redirect))
    val csr = Flipped(new CustomCSRCtrlIO)
    val trigger = Input(new CsrTriggerBundle)
  }
  val fromBackend = new Bundle() {
    val issueVldu = MixedVec(Seq.fill(VlduCnt)(Flipped(DecoupledIO(new MemExuInput(isVector=true)))))
  }
  val fromMemExuBlock = new MemExuBlockToVecExuBlockIO
  val fromDCache = new DCacheLoadRespIO
  val fromTlb = Flipped(DecoupledIO(new TlbResp(2)))
  val fromPmp = Flipped(new PMPRespBundle())

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
  val toMemExuBlock = new VecExuBlockToMemExuBlockIO
  val toDCache = new DCacheLoadReqIO
  val toTlb = new Bundle() {
    val req = DecoupledIO(new TlbReq)
    val req_kill = Output(Bool())
  }
  val exceptionInfo = ValidIO(new FeedbackToLsqIO)
  val sbuffer = Decoupled(new DCacheWordReqWithVaddrAndPfFlag)
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
  val vldCanAccept = vlSplit.zip(io.fromBackend.issueVldu).map {
    case (vl, issue) => vl.io.in.ready && VlduType.isVecLd(issue.bits.uop.fuOpType)
  }
  val vstCanAccept = vsSplit.zip(io.fromBackend.issueVldu).map {
    case (vs, issue) => vs.io.in.ready && VstuType.isVecSt(issue.bits.uop.fuOpType)
  }

  io.fromBackend.issueVldu.zip(vldCanAccept.zip(vstCanAccept)).foreach {
    case (issue, (vldAccept, vstAccept)) =>
      issue.ready := vldAccept || vstAccept
  }

  val isSegment = io.fromBackend.issueVldu.head.valid && FuType.isVsegls(io.fromBackend.issueVldu.head.bits.uop.fuType)
  val isFixVlUop = io.fromBackend.issueVldu.map { x =>
    x.bits.uop.vpu.isVleff && x.bits.uop.vpu.lastUop && x.valid
  }

  when (vSegmentUnit.io.in.fire) {
    vSegmentFlag := true.B
  } .elsewhen (vSegmentUnit.io.uopwriteback.valid) {
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
   */
  // init vsMergebuffer
  (0 until VstuCnt).foreach { i =>
    vsMergeBuffer(i).io.fromPipeline := DontCare
    vsMergeBuffer(i).io.fromSplit := DontCare
    vsMergeBuffer(i).io.fromMisalignBuffer.get.flush := io.fromMemExuBlock.storeMisalignBuffer.control(i).flush
    vsMergeBuffer(i).io.fromMisalignBuffer.get.mbIndex := io.fromMemExuBlock.storeMisalignBuffer.control(i).mbIndex
  }

  // init vsSplit
  (0 until VstuCnt).foreach { i =>
    vsSplit(i).io.redirect <> io.fromCtrl.redirect
    vsSplit(i).io.in <> io.fromBackend.issueVldu(i)
    /**
      * Determine whether the validity of the input.
      * Conditions:
      * 1. io.fromBackend.issueVldu(i) is valid
      * 2. vstCanAccept(i) is true (likely indicating if the vector store can accept a new value)
      * 3. isSegment is false (indicating that the store is not a segment store)
      */
    vsSplit(i).io.in.valid := io.fromBackend.issueVldu(i).valid && vstCanAccept(i) && !isSegment
    vsSplit(i).io.toMergeBuffer <> vsMergeBuffer(i).io.fromSplit.head

    val vsSplitOut = Wire(Decoupled(new VecPipeBundle(isVStore = true)))
    NewPipelineConnect(
      vsSplit(i).io.out,
      vsSplitOut,
      vsSplitOut.fire,
      Mux(
        vsSplit(i).io.out.fire,
        vsSplit(i).io.out.bits.uop.robIdx.needFlush(io.fromCtrl.redirect),
        io.toMemExuBlock.vectorStoreIssues(i).bits.uop.robIdx.needFlush(io.fromCtrl.redirect)
      ),
      Option("VsSplitConnectStu")
    )
    io.toMemExuBlock.vectorStoreIssues(i).valid := vsSplitOut.valid
    io.toMemExuBlock.vectorStoreIssues(i).bits.fromVecPipeBundle(vsSplitOut.bits, isStore = true)
    vsSplitOut.ready := io.toMemExuBlock.vectorStoreIssues(i).ready

    vsSplit(i).io.vstd.get := DontCare // Todo: Discuss how to pass vector store data
    vsSplit(i).io.vstdMisalign.get.storeMisalignBufferEmpty :=
      !io.fromMemExuBlock.storeMisalignBuffer.storeMisalignBufferFull
    vsSplit(i).io.vstdMisalign.get.storePipeEmpty := !io.fromMemExuBlock.storePipeEmpty(i)
  }

  // init vlSplit
  (0 until VlduCnt).foreach { i =>
    vlSplit(i).io.redirect <> io.fromCtrl.redirect
    vlSplit(i).io.in <> io.fromBackend.issueVldu(i)
    /**
      * Determine whether the validity of the input for the vector load split:
      * Conditions:
      * 1. io.fromBackend.issueVldu(i).valid must be true (i.e., the backend's issue vector load unit is valid).
      * 2. vldCanAccept(i) must be true, indicating that the vector load can accept a new input.
      * 3. isSegment must be false, meaning that the store is not a segment store.
      * 4. isFixVlUop(i) must be false, indicating that the vector load operation is not a fixed width operation (this condition is added to handle specific cases like fixed vector lengths).
      */
    vlSplit(i).io.in.valid := io.fromBackend.issueVldu(i).valid && vldCanAccept(i) && !isSegment && !isFixVlUop(i)
    vlSplit(i).io.toMergeBuffer <> vlMergeBuffer.io.fromSplit(i)

    val vlSplitOut = Wire(DecoupledIO(new VecPipeBundle()))
    NewPipelineConnect(
      left = vlSplit(i).io.out,
      right = vlSplitOut,
      rightOutFire = vlSplitOut.fire,
      isFlush = Mux(
        vlSplit(i).io.out.fire,
        vlSplit(i).io.out.bits.uop.robIdx.needFlush(io.fromCtrl.redirect),
        io.toMemExuBlock.vectorLoadIssues(i).bits.uop.robIdx.needFlush(io.fromCtrl.redirect)
      ),
      moduleName = Option("VlSplitConnectLdu")
    )
    vlSplitOut.valid := io.toMemExuBlock.vectorLoadIssues(i).valid
    io.toMemExuBlock.vectorLoadIssues(i).bits.fromVecPipeBundle(vlSplitOut.bits)
    vlSplitOut.ready <> io.toMemExuBlock.vectorLoadIssues(i).ready

    // Subsequent instrction will be blocked
    vfofBuffer.io.in(i).valid := io.fromBackend.issueVldu(i).valid
    vfofBuffer.io.in(i).bits  := io.fromBackend.issueVldu(i).bits
  }

  //
  vlMergeBuffer.io.fromPipeline.zip(io.fromMemExuBlock.vldWriteback).zipWithIndex.foreach {
    case ((vl, writeback), i) =>
      writeback.ready := vl.ready
      io.fromMemExuBlock.loadMisalignBuffer.writeback.ready := true.B

      vl.valid := writeback.valid
      vl.bits := writeback.bits.toVecPipelineFeedbackBundle()
      if (i == MisalignWBPort) {
        when (!writeback.valid) {
          Connection.connect(
            sink        = vl,
            source      = io.fromMemExuBlock.loadMisalignBuffer.writeback,
            connectFn   = None,
            connectName = "VecExuBlockConnectMisalignBuffer_"+i
          )
        }
      }
  }

  vsMergeBuffer.map(_.io.fromPipeline.head).zip(io.fromMemExuBlock.vstWriteback).zipWithIndex.foreach {
    case ((vs, writeback), i) =>
      if (i < VstuCnt) {
        writeback.ready := true.B
        io.fromMemExuBlock.storeMisalignBuffer.writeback(i).ready := vs.ready

        when (writeback.valid) {
          vs.valid := writeback.valid
          vs.bits := writeback.bits.toVecPipelineFeedbackBundle(isVStore = true)
        } .otherwise {
          Connection.connect(
            sink        = vs,
            source      = io.fromMemExuBlock.storeMisalignBuffer.writeback(i),
            connectFn   = None,
            connectName = "VecExuBlockConnectMisalignBuffer_"+i
          )
        }
      }
  }

  // vector store issue: [[Backend]] -> [[MemExuBlock]]
  vsSplit.zip(io.toMemExuBlock.vectorStoreIssues).zipWithIndex.foreach {
    case ((vs: VSSplitImp, stu), i) =>
      vs.io.redirect <> io.fromCtrl.redirect
      vs.io.in <> io.fromBackend.issueVldu(i)
      vs.io.in.valid := io.fromBackend.issueVldu(i).valid &&
        VstuType.isVecSt(io.fromBackend.issueVldu(i).bits.uop.fuOpType) && vstCanAccept(i) && !isSegment
      vs.io.toMergeBuffer <> vsMergeBuffer(i).io.fromSplit.head
      vs.io.vstd.get := DontCare // TODO: Discuss how to pass vector store data

      val vecStIn = Wire(DecoupledIO(new VecPipeBundle))
      stu.valid := vecStIn.valid
      stu.bits.fromVecPipeBundle(vecStIn.bits, isStore = true)
      vecStIn.ready := stu.ready

      val flush = Mux(
        vs.io.out.fire,
        vs.io.out.bits.uop.robIdx.needFlush(io.fromCtrl.redirect),
        vecStIn.bits.uop.robIdx.needFlush(io.fromCtrl.redirect)
      )

      NewPipelineConnect(
        left = vs.io.out,
        right = vecStIn,
        rightOutFire = vecStIn.fire,
        isFlush = flush,
        moduleName = Option("VsSplitConnectStu" + i)
      )
    case _ =>
  }

  // vector load issue: [[Backend]] -> [[MemExuBlock]]
  vlSplit.zip(io.toMemExuBlock.vectorLoadIssues).zipWithIndex.foreach {
    case ((vl: VLSplitImp, ldu), i) =>
      vl.io.redirect <> io.fromCtrl.redirect
      vl.io.in <> io.fromBackend.issueVldu(i)
      vl.io.in.valid := io.fromBackend.issueVldu(i).valid && vldCanAccept(i) && !isSegment && !isFixVlUop(i) &&
        VlduType.isVecLd(io.fromBackend.issueVldu(i).bits.uop.fuOpType)

      vl.io.toMergeBuffer <> vlMergeBuffer.io.fromSplit(i)

      val vecLdIn = Wire(DecoupledIO(new VecPipeBundle))
      ldu.valid := vecLdIn.valid
      ldu.bits.fromVecPipeBundle(vecLdIn.bits)
      vecLdIn.ready := ldu.ready

      NewPipelineConnect(
        left = vl.io.out,
        right = vecLdIn,
        rightOutFire = vecLdIn.fire,
        isFlush = Mux(
          vl.io.out.fire,
          vl.io.out.bits.uop.robIdx.needFlush(io.fromCtrl.redirect),
          vecLdIn.bits.uop.robIdx.needFlush(io.fromCtrl.redirect)
        ),
        moduleName = Option("VlSplitConnectLdu" + i)
      )
    case _ =>
  }

  // vl writeback: [[MemExuBlock]] -> [[vlMergeBuffer]]
  vlMergeBuffer.io.fromPipeline.zip(io.fromMemExuBlock.vldWriteback).foreach {
    case (sink, source) =>
      sink.valid := source.valid
      sink.bits  := source.bits.toVecPipelineFeedbackBundle()
      source.ready := sink.ready
  }

  // vs writeback: [[MemExuBlock]] -> [[vsMergeBuffer]]
  vsMergeBuffer.map(_.io.fromPipeline.head).zip(io.fromMemExuBlock.vstWriteback).foreach {
    case (sink, source) =>
      sink.valid := source.valid
      sink.bits  := source.bits.toVecPipelineFeedbackBundle(isVStore = true)
      source.ready := sink.ready
  }

  // vl feedback: [[vlMergeBuffer]] -> [[Lsq]]
  io.toLsq.ldvecFeedback.zip(vlMergeBuffer.io.toLsq).foreach {
    case (sink, source) =>
      sink <> source
  }

  // vs feedback: [[vsMergeBuffer]] -> [[Lsq]]
  io.toLsq.stvecFeedback.zip(vsMergeBuffer.map(_.io.toLsq.head)).foreach {
    case (sink, source) =>
      sink <> source
  }

  // vstd: [[vsSplit]] -> [[Lsq]]
  io.toLsq.vstd.zip(vsSplit).zipWithIndex.foreach {
    case ((sink, source), i) =>
      if (source.io.vstd.isDefined) {
        sink <> source.io.vstd.get
      } else {
        sink := DontCare
      }
  }

  // vldu feedback: [[vlMergeBuffer]] -> [[Backend]]
  io.toBackend.vlduIqFeedback.zip(vlMergeBuffer.io.feedback).foreach {
    case (sink, source) =>
      sink.feedbackFast := DontCare
      sink.feedbackSlow <> source
  }

  // vstu feedback: [[vsMergeBuffer]] -> [[Backend]]
  io.toBackend.vstuIqFeedback.zip(vsMergeBuffer.map(_.io.feedback.head)).zipWithIndex.foreach {
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

  // dcache: [[vSegmentUnit]] <-> [[DCache]]
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

  //
  io.exceptionInfo <> vSegmentUnit.io.exceptionInfo
  io.sbuffer <> vSegmentUnit.io.sbuffer
  io.flushSbuffer <> vSegmentUnit.io.flush_sbuffer
  io.vecDifftestInfo <> vSegmentUnit.io.vecDifftestInfo

  // vector writeback: [[vMergeBuffer]] -> [[Backend]]
  val vlMergeBufferWriteback = vlMergeBuffer.io.uopWriteback
  val vsMergeBufferWriteback = vsMergeBuffer.map(_.io.uopWriteback.head)
  val vfofBufferWriteback    = vfofBuffer.io.mergeUopWriteback

  io.toBackend.writebackVldu.zip(vlMergeBufferWriteback.zip(vsMergeBufferWriteback).zip(vfofBufferWriteback)).
    zipWithIndex.foreach {
    case ((sink, ((vl, vs), vfof)), i) =>
      if (i == 0) {
        sink.valid := vl.valid || vs.valid || vSegmentUnit.io.uopwriteback.valid
        sink.bits := PriorityMux(Seq(
          vSegmentUnit.io.uopwriteback.valid -> vSegmentUnit.io.uopwriteback.bits,
          vl.valid -> vl.bits,
          vs.valid -> vs.bits
        ))
        vSegmentUnit.io.uopwriteback.ready := sink.ready
        vl.ready := sink.ready && !vSegmentUnit.io.uopwriteback.valid
        vs.ready := sink.ready && !vSegmentUnit.io.uopwriteback.valid && !vl.valid
      } else if (i == 1) {
        sink.valid := vl.valid || vs.valid || vfofBuffer.io.uopWriteback.valid
        sink.bits := PriorityMux(Seq(
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
}
