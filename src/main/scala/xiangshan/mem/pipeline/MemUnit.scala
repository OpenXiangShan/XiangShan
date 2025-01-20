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
import xiangshan.backend.ctrlblock.{DebugLsInfoBundle, LsTopdownInfo}
import xiangshan.backend.datapath.{NewPipelineConnect, NewPipelineConnectPipe}
import xiangshan.backend.Bundles.{DynInst, MemExuInput, MemExuOutput}
import xiangshan.backend.fu.PMPRespBundle
import xiangshan.backend.fu.NewCSR._
import xiangshan.mem.Bundles.LsPipelineBundle
import xiangshan.mem.HasLoadHelper
import xiangshan.mem.Bundles._
import xiangshan.cache.{DCacheLoadReqIO, DCacheLoadRespIO, MemoryOpConstants}
import xiangshan.cache.{HasDCacheParameters}
import xiangshan.cache.mmu.{Pbmt, TlbReq, TlbResp, TlbCmd, TlbHintReq}

class MemUnit(val params: MemUnitParams)(implicit p: Parameters) extends LazyModule
  with HasXSParameter {
  override def shouldBeInlined: Boolean = false
  implicit val unitParams: MemUnitParams = params
  lazy val module: MemUnitImp = unitParams.unitType match {
    case Std() => new StdImp(this)
    case Sta() => new HyuImp(this)
    case Ldu() => new HyuImp(this)
    case Hyu() => new HyuImp(this)
    case Amo() => new AmoImp(this)
    case _     => throw new IllegalArgumentException("Unknown unit type")
  }
}

class MemUnitIO()(implicit p: Parameters, params: MemUnitParams) extends XSBundle {
  // from
  val fromCtrl = new Bundle() {
    val redirect = Flipped(ValidIO(new Redirect))
    val hartId = Input(UInt(hartIdLen.W))
    val csr = Flipped(new CustomCSRCtrlIO)
    val trigger = OptionWrapper(params.hasTrigger, Input(new CsrTriggerBundle))
  }
  val fromBackend = new Bundle {
    val issue = params.genLsPipelineSinkDecoupledBundle
  }
  val fromTlb = new Bundle() {
    val resp = Flipped(DecoupledIO(new TlbResp(2)))
    val hint = Flipped(new TlbHintReq)
  }
  val fromDCache = new DCacheLoadRespIO
  val fromPmp = Flipped(new PMPRespBundle())

  // to
  val toBackend = new Bundle() {
    val stIssue = ValidIO(new MemExuInput)
    val wakeup = ValidIO(new DynInst)
    val ldCancel = Output(new LoadCancelIO())
    val rollback = ValidIO(new Redirect)
    val iqFeedback = new MemRSFeedbackIO
    val writeback = params.genLsPipelineSourceDecoupledBundle
  }
  val toDCache = new DCacheLoadReqIO
  val toTlb = new Bundle() {
    val req = DecoupledIO(new TlbReq)
    val req_kill = Output(Bool())
  }
}

class MemUnitImp(override val wrapper: MemUnit)(implicit p: Parameters, val params: MemUnitParams)
  extends LazyModuleImp(wrapper)
  with HasPerfEvents
  with HasXSParameter
  with HasDCacheParameters
  with HasCircularQueuePtrHelper {

  protected def printPipeLine(pipeline: LsPipelineBundle, cond: Bool, name: String): Unit = {
    XSDebug(cond,
      p"$name" + p" pc ${Hexadecimal(pipeline.uop.pc)} " +
        p"addr ${Hexadecimal(pipeline.vaddr)} -> ${Hexadecimal(pipeline.paddr)} " +
        p"op ${Binary(pipeline.uop.fuOpType)} " +
        p"data ${Hexadecimal(pipeline.data)} " +
        p"mask ${Hexadecimal(pipeline.mask)}\n"
    )
  }

  lazy val io = IO(new MemUnitIO())

  private def getIssues(issueType: Int): Seq[DecoupledIO[LsPipelineBundle]] = {
    io.fromBackend.issue.zip(params.issueParams).filter(_._2.issueType == issueType).map(_._1)
  }

  private def getWritebacks(issueType: Int): Seq[DecoupledIO[LsPipelineBundle]] = {
    Seq(params.getWritebackPortByType(issueType)).filter(_.isDefined).map(pIdx => io.toBackend.writeback(pIdx.get))
  }

  def getReplayIssues() = getIssues(MemIssueType.Lqr)

  def getMisalignIssues() = getIssues(MemIssueType.Mb)

  def getStdIssues() = getIssues(MemIssueType.Std)

  def getStdWritebacks() = getWritebacks(MemIssueType.Std)

  def getStaIssues() = getIssues(MemIssueType.Sta)

  def getStaWritebacks() = getWritebacks(MemIssueType.Sta)

  def getLdIssues() = getIssues(MemIssueType.Ld)

  def getLdWritebacks() = getWritebacks(MemIssueType.Ld)

  def getPrefetchIssues() = getIssues(MemIssueType.Pf)

  def getAmoIssues() = getIssues(MemIssueType.Amo)

  def getAmoWritebacks() = getWritebacks(MemIssueType.Amo)

  def getVldIssues() = getIssues(MemIssueType.Vld)

  def getVldWritebacks() = getWritebacks(MemIssueType.Vld)

  def getVstIssues() = getIssues(MemIssueType.Vst)

  def getVstWritebacks() = getWritebacks(MemIssueType.Vst)

  def getFastReplayIssues() = getIssues(MemIssueType.Fr)

  def getMmioIssues() = getIssues(MemIssueType.Mmio)

  def getNcIssues() = getIssues(MemIssueType.Nc)

  private val hasLoadTrigger = params.hasLdExe && params.hasTrigger
  private val hasStoreTrigger = params.hasStaExe && params.hasTrigger
  val loadTrigger = OptionWrapper(hasLoadTrigger, Module(new MemTrigger(MemType.LOAD)))
  val storeTrigger = OptionWrapper(hasStoreTrigger, Module(new MemTrigger(MemType.STORE)))
  val triggers = Seq(loadTrigger, storeTrigger)

  val arbiter = params.arbiter match {
    case "order" => Module(new OrderArbiter(params.issueParams.map(_.issueType)))
    case "priority" => Module(new PriorityArbiter(params.issueParams.map(_.issueType)))
    case _ => throw new IllegalArgumentException("Unknown arbiter type")
  }
  // Pipeline
  // -------------------------------------------------------------------
  // stage 0
  // -------------------------------------------------------------------
  val s0_selOut = Wire(DecoupledIO(new LsPipelineBundle))
  val s0_out = Wire(DecoupledIO(new LsPipelineBundle))
  val s0_kill = WireInit(false.B)
  val s0_wbPort = WireInit(VecInit(Seq.fill(params.numWBPorts)(false.B)))
  val perf_s0SelectPort = WireInit(0.U)

  // select
  arbiter.io.in <> io.fromBackend.issue
  arbiter.io.out <> s0_selOut
  s0_out <> s0_selOut

  io.fromBackend.issue.zip(params.issueParams).zipWithIndex.foreach {
    case ((issue, param), i) =>
      when (issue.valid && arbiter.io.chosen(i)) {
        val wbPort = params.getWritebackPortByType(param.issueType)
        if (wbPort.isDefined) s0_wbPort(wbPort.get) := true.B
        perf_s0SelectPort := i.U
      }
  }

  // to tlb
  io.toTlb.req.valid             := s0_out.valid
  io.toTlb.req.bits.vaddr        := s0_out.bits.vaddr
  io.toTlb.req.bits.fullva       := s0_out.bits.fullva
  io.toTlb.req.bits.checkfullva  := s0_out.bits.isIq || s0_out.bits.isVector
  io.toTlb.req.bits.cmd          := Mux(s0_out.bits.isStore, TlbCmd.write, TlbCmd.read)
  io.toTlb.req.bits.isPrefetch   := s0_out.bits.isPrefetch
  io.toTlb.req.bits.size         := s0_out.bits.uop.fuOpType(2, 0)
  io.toTlb.req.bits.kill         := s0_kill
  io.toTlb.req.bits.memidx.is_ld := !s0_out.bits.isStore
  io.toTlb.req.bits.memidx.is_st := s0_out.bits.isStore
  io.toTlb.req.bits.memidx.idx   := Mux(s0_out.bits.isStore, s0_out.bits.uop.sqIdx.value, s0_out.bits.uop.lqIdx.value)
  io.toTlb.req.bits.no_translate := s0_out.bits.tlbNoQuery
  io.toTlb.req.bits.hyperinst    := LSUOpType.isHlv(s0_out.bits.uop.fuOpType)
  io.toTlb.req.bits.hlvx         := LSUOpType.isHlvx(s0_out.bits.uop.fuOpType)
  io.toTlb.req.bits.pmp_addr     := DontCare
  io.toTlb.req.bits.debug.robIdx := s0_out.bits.uop.robIdx
  io.toTlb.req.bits.debug.pc     := s0_out.bits.uop.pc
  io.toTlb.req.bits.debug.isFirstIssue := s0_out.bits.isFirstIssue

  // to DCache
  io.toDCache.req.valid             := s0_out.valid
  io.toDCache.req.bits.cmd          := MemoryOpConstants.M_XRD // TODO: store ?
  io.toDCache.req.bits.vaddr        := s0_out.bits.vaddr
  io.toDCache.req.bits.mask         := s0_out.bits.mask
  io.toDCache.req.bits.data         := DontCare
  io.toDCache.req.bits.isFirstIssue := s0_out.bits.isFirstIssue
  io.toDCache.req.bits.instrtype    := LOAD_SOURCE.U
  io.toDCache.req.bits.debug_robIdx := s0_out.bits.uop.robIdx.value
  io.toDCache.req.bits.replayCarry  := s0_out.bits.replayCarry
  io.toDCache.req.bits.id           := DontCare // TODO: update cache meta
  io.toDCache.req.bits.lqIdx        := s0_out.bits.uop.lqIdx
  io.toDCache.s0_pc                 := s0_out.bits.uop.pc
  io.toDCache.pf_source             := L1_HW_PREFETCH_NULL
  io.toDCache.is128Req              := s0_out.bits.is128bit
  io.toDCache.replacementUpdated    := false.B
  io.toDCache.s0_pc := DontCare
  io.toDCache.s1_pc := DontCare
  io.toDCache.s2_pc := DontCare

  // TODO: need assign?
  io.toBackend.wakeup.valid := false.B
  io.toBackend.wakeup.bits  := DontCare

  when (io.toTlb.req.valid && s0_out.bits.isFirstIssue) {
    s0_out.bits.uop.debugInfo.tlbFirstReqTime := GTimer()
  }

  s0_kill := s0_out.bits.uop.robIdx.needFlush(io.fromCtrl.redirect)

  val perf_s0SelectIssue = Mux1H(io.fromBackend.issue.zipWithIndex.map {
    case (issue, i) =>
      (perf_s0SelectPort === i.U) -> issue
  })
  XSPerfAccumulate("s0_inValid", perf_s0SelectIssue.valid)
  XSPerfAccumulate("s0_inBlock", perf_s0SelectIssue.valid && !perf_s0SelectIssue.ready)
  XSPerfAccumulate("s0_stallOut", s0_out.valid && !s0_out.ready)

  printPipeLine(s0_out.bits, s0_out.valid, "s0")

  // Pipeline
  // -------------------------------------------------------------------
  // stage 1
  // -------------------------------------------------------------------
  val s1_in = WireInit(0.U.asTypeOf(DecoupledIO(new LsPipelineBundle)))
  val s1_out = WireInit(0.U.asTypeOf(DecoupledIO(new LsPipelineBundle)))
  val s1_kill = WireInit(false.B)
  NewPipelineConnect.connect(s0_out, s1_in, s1_in.fire, s0_kill)
  val s1_wbPort = RegEnable(s0_wbPort, 0.U.asTypeOf(s0_wbPort), s0_out.fire)

  s1_kill := s1_in.bits.uop.robIdx.needFlush(io.fromCtrl.redirect)

  // to tlb
  io.toTlb.req_kill := s1_kill
  io.toTlb.req.bits.pmp_addr := s1_in.bits.paddr
  io.fromTlb.resp.ready := true.B // TODO: why dtlbResp needs a ready?

  // to dcache
  io.toDCache.s1_paddr_dup_lsu := io.fromTlb.resp.bits.paddr(0)
  io.toDCache.s1_paddr_dup_dcache := io.fromTlb.resp.bits.paddr(1)
  io.toDCache.s1_kill := s1_kill
  io.toDCache.s1_kill_data_read := s1_kill
  io.toDCache.s1_pc := s1_in.bits.uop.pc

  val s1_pbmt = Wire(UInt(Pbmt.width.W))
  s1_pbmt := Mux(!io.fromTlb.resp.bits.miss, io.fromTlb.resp.bits.pbmt.head, 0.U(Pbmt.width.W))

  if (hasLoadTrigger) {
    loadTrigger.get.io.fromCsrTrigger.tdataVec             := io.fromCtrl.trigger.get.tdataVec
    loadTrigger.get.io.fromCsrTrigger.tEnableVec           := io.fromCtrl.trigger.get.tEnableVec
    loadTrigger.get.io.fromCsrTrigger.triggerCanRaiseBpExp := io.fromCtrl.trigger.get.triggerCanRaiseBpExp
    loadTrigger.get.io.fromCsrTrigger.debugMode            := io.fromCtrl.trigger.get.debugMode
    loadTrigger.get.io.fromLoadStore.vaddr                 := s1_in.bits.vaddr
    loadTrigger.get.io.fromLoadStore.isVectorUnitStride    := s1_in.bits.isVector && s1_in.bits.is128bit
    loadTrigger.get.io.fromLoadStore.mask                  := s1_in.bits.mask
  }
  if (hasStoreTrigger) {
    storeTrigger.get.io.fromCsrTrigger.tdataVec             := io.fromCtrl.trigger.get.tdataVec
    storeTrigger.get.io.fromCsrTrigger.tEnableVec           := io.fromCtrl.trigger.get.tEnableVec
    storeTrigger.get.io.fromCsrTrigger.triggerCanRaiseBpExp := io.fromCtrl.trigger.get.triggerCanRaiseBpExp
    storeTrigger.get.io.fromCsrTrigger.debugMode            := io.fromCtrl.trigger.get.debugMode
    storeTrigger.get.io.fromLoadStore.vaddr                 := s1_in.bits.vaddr
    storeTrigger.get.io.fromLoadStore.isVectorUnitStride    := s1_in.bits.isVector && s1_in.bits.is128bit
    storeTrigger.get.io.fromLoadStore.mask                  := s1_in.bits.mask
  }

  val s1_triggerAction = true match {
    case _ if (hasLoadTrigger && hasStoreTrigger) => {
      val s1LoadTriggerAction = loadTrigger.get.io.toLoadStore.triggerAction
      val s1StoreTriggerAction = storeTrigger.get.io.toLoadStore.triggerAction
      Mux(s1_in.bits.isAtomic,
          s1LoadTriggerAction | s1StoreTriggerAction,
          Mux(
            s1_in.bits.isStore,
            storeTrigger.get.io.toLoadStore.triggerAction,
            loadTrigger.get.io.toLoadStore.triggerAction
          )
      )
    }
    case _ if (hasLoadTrigger && !hasStoreTrigger) =>
      loadTrigger.get.io.toLoadStore.triggerAction
    case _ if (!hasLoadTrigger && hasStoreTrigger) =>
      storeTrigger.get.io.toLoadStore.triggerAction
    case _ => TriggerAction.None
  }
  val s1_triggerDebugMode = TriggerAction.isDmode(s1_triggerAction)
  val s1_triggerBreakpoint = TriggerAction.isExp(s1_triggerAction)
  s1_out.bits.uop.trigger := s1_triggerAction
  // generate breakpoint exception
  s1_out.bits.uop.exceptionVec(breakPoint) := s1_triggerBreakpoint

  // vector vaddr offset generate
  val s1_triggerVaddr = true match {
    case _ if (hasLoadTrigger && hasStoreTrigger) => Mux(
      s1_in.bits.isStore,
      storeTrigger.get.io.toLoadStore.triggerVaddr,
      loadTrigger.get.io.toLoadStore.triggerVaddr
    )
    case _ if (hasLoadTrigger && !hasStoreTrigger) =>
              loadTrigger.get.io.toLoadStore.triggerVaddr
    case _ if (!hasLoadTrigger && hasStoreTrigger) =>
              storeTrigger.get.io.toLoadStore.triggerVaddr
    case _ => 0.U
  }
  s1_out.bits.vecVaddrOffset := Mux(
    s1_triggerDebugMode || s1_triggerBreakpoint,
    s1_triggerVaddr - s1_in.bits.vecBaseVaddr,
    s1_in.bits.vaddr + genVFirstUnmask(s1_in.bits.mask).asUInt - s1_in.bits.vecBaseVaddr
  )
  // vector mask generate
  val s1_triggerMask = true match {
    case _ if (hasLoadTrigger && hasStoreTrigger) => Mux(
      s1_in.bits.isStore,
      storeTrigger.get.io.toLoadStore.triggerMask,
      loadTrigger.get.io.toLoadStore.triggerMask
    )
    case _ if (hasLoadTrigger && !hasStoreTrigger) =>
              loadTrigger.get.io.toLoadStore.triggerMask
    case _ if (!hasLoadTrigger && hasStoreTrigger) =>
              storeTrigger.get.io.toLoadStore.triggerMask
    case _ => 0.U
  }

  s1_out <> s1_in
  s1_out.bits.vaNeedExt := io.fromTlb.resp.bits.excp(0).vaNeedExt
  s1_out.bits.isHyper   := io.fromTlb.resp.bits.excp(0).isHyper
  s1_out.bits.paddr     := io.fromTlb.resp.bits.paddr(0)
  s1_out.bits.gpaddr    := io.fromTlb.resp.bits.gpaddr(0)
  s1_out.bits.isForVSnonLeafPTE := io.fromTlb.resp.bits.isForVSnonLeafPTE
  s1_out.bits.tlbMiss   := s1_in.valid && io.fromTlb.resp.valid && io.fromTlb.resp.bits.miss
  s1_out.bits.ptwBack   := io.fromTlb.resp.bits.ptwBack
  s1_out.bits.uop.flushPipe := false.B
  s1_out.bits.mmio := LSUOpType.isCbo(s1_in.bits.uop.fuOpType) || Pbmt.isIO(s1_pbmt)
  s1_out.bits.atomic := LSUOpType.isCbo(s1_in.bits.uop.fuOpType) || Pbmt.isIO(s1_pbmt)
  s1_out.bits.isNoncacheable := s1_in.bits.isNoncacheable || Pbmt.isNC(s1_pbmt)
  s1_out.bits.vecTriggerMask := Mux(
    s1_triggerDebugMode || s1_triggerBreakpoint,
    s1_triggerMask,
    0.U
  )

  // make dev happy
  val s1_tlbMemIdx = io.fromTlb.resp.bits.memidx
  val s1_tlbMemIdxHit = Mux(
    s1_in.bits.isStore,
    s1_tlbMemIdx.is_st && s1_in.bits.uop.sqIdx.value === s1_tlbMemIdx.idx,
    s1_tlbMemIdx.is_ld && s1_in.bits.uop.lqIdx.value === s1_tlbMemIdx.idx
  )
  when (io.fromTlb.resp.valid && !io.fromTlb.resp.bits.miss && s1_tlbMemIdxHit) {
    s1_out.bits.uop.debugInfo.tlbRespTime := GTimer()
  }

  // TOOD: need assign?
  io.toBackend.stIssue.valid := false.B
  io.toBackend.stIssue.bits  := DontCare
  io.toBackend.iqFeedback.feedbackFast.valid := false.B
  io.toBackend.iqFeedback.feedbackFast.bits := DontCare

  XSPerfAccumulate("s1_inValid", s1_in.valid)
  XSPerfAccumulate("s1_inFire", s1_in.fire)
  XSPerfAccumulate("s1_firstIssue", s1_in.valid && s1_in.bits.isFirstIssue)
  XSPerfAccumulate("s1_tlbMiss", s1_in.valid && io.fromTlb.resp.valid && io.fromTlb.resp.bits.miss)
  XSPerfAccumulate("s1_tlbMissFirstIssue", s1_in.valid && io.fromTlb.resp.valid && io.fromTlb.resp.bits.miss && s1_in.bits.isFirstIssue)
  XSPerfAccumulate("s1_stallOut", s1_out.valid && !s1_out.ready)

  printPipeLine(s1_out.bits, s1_out.valid, "s1")

  // Pipeline
  // -------------------------------------------------------------------
  // stage 2
  // -------------------------------------------------------------------
  val s2_in = WireInit(0.U.asTypeOf(DecoupledIO(new LsPipelineBundle)))
  val s2_out = WireInit(0.U.asTypeOf(DecoupledIO(new LsPipelineBundle)))
  val s2_kill = WireInit(false.B)
  NewPipelineConnect.connect(s1_out, s2_in, s2_in.fire, s1_kill)
  val s2_wbPort = RegEnable(s1_wbPort, 0.U.asTypeOf(s1_wbPort), s1_out.fire)

  io.toTlb.req_kill := false.B
  io.fromDCache.resp.ready := true.B  // TODO: dcache resp

  // kill dcache write intent request when mmio or exception
  io.toDCache.s2_kill := s2_in.bits.uop.robIdx.needFlush(io.fromCtrl.redirect)
  io.toDCache.s2_pc   := s2_in.bits.uop.pc

  s2_out <> s2_in
  s2_out.bits.uop.vpu.vstart := s2_in.bits.vecVaddrOffset >> s2_in.bits.uop.vpu.veew

  // TODO: need assign?
  io.toBackend.iqFeedback.feedbackSlow.valid := false.B
  io.toBackend.iqFeedback.feedbackSlow.bits  := DontCare

  printPipeLine(s2_out.bits, s2_out.valid, "s2")

  XSPerfAccumulate("s2InValid", s2_in.valid)
  XSPerfAccumulate("s2InFire", s2_in.fire)
  XSPerfAccumulate("s2StallOut", s2_out.valid && !s2_out.ready)

  // Pipeline
  // -------------------------------------------------------------------
  // stage 3
  // -------------------------------------------------------------------
  val s3_in = WireInit(0.U.asTypeOf(DecoupledIO(new LsPipelineBundle)))
  val s3_out = WireInit(0.U.asTypeOf(DecoupledIO(new LsPipelineBundle)))
  val s3_kill = WireInit(false.B)

  NewPipelineConnect.connect(s2_out, s3_in, s3_in.fire, s2_kill)
  val s3_wbPort = RegEnable(s2_wbPort, 0.U.asTypeOf(s2_wbPort), s2_out.fire)

  s3_kill := s3_in.bits.uop.robIdx.needFlush(io.fromCtrl.redirect)

  // TODO: need assign?
  io.toBackend.ldCancel.ld1Cancel := false.B
  io.toBackend.ldCancel.ld2Cancel := false.B
  io.toBackend.rollback.valid := false.B
  io.toBackend.rollback.bits  := DontCare

  s3_out <> s3_in
  s3_out.bits.data := io.fromDCache.resp.bits.data

  s3_out.ready := false.B
  io.toBackend.writeback.zip(s3_wbPort).foreach {
    case (wb, chosen) =>
      wb.valid := s3_out.valid && chosen
      wb.bits := s3_out.bits
      when (chosen) {
        s3_out.ready := wb.ready
      }
  }

  printPipeLine(s3_out.bits, s3_out.valid, "s3")
  // perf cnt
  XSPerfAccumulate("s3_invalid", s3_in.valid)
  XSPerfAccumulate("s3_inFire", s3_in.fire)
  XSPerfAccumulate("s3_stallOut", s3_out.valid && !s3_out.ready)

  // bug lyq: some signals in perfEvents are no longer suitable for the current MemBlock design
  // hardware performance counter
  val perfEvents = Seq(
    ("s0_inFire", s0_out.fire),
    ("stallDCache", s0_out.valid && !io.fromDCache.resp.ready),
    ("s1_inFire", s1_out.fire),
    ("s1_tlbMiss", s1_out.fire && io.fromTlb.resp.bits.miss),
    ("s2_inFire", s2_out.fire),
    ("s2_dcacheMiss", s2_out.fire && io.fromDCache.resp.bits.miss),
  )
  generatePerfEvent()
}
