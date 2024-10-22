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
import xiangshan.backend.fu.PMPRespBundle
import xiangshan.backend.fu.NewCSR._
import xiangshan.mem.Bundles.LsPipelineBundle
import xiangshan.mem.HasLoadHelper
import xiangshan.mem.{StoreDataUnitImp}
import xiangshan.mem.Bundles._
import xiangshan.cache.{DCacheLoadReqIO, DCacheLoadRespIO, MemoryOpConstants}
import xiangshan.cache.{HasDCacheParameters}
import xiangshan.cache.mmu.{TlbReq, TlbResp, TlbCmd, TlbHintReq}

class MemUnit(val params: MemUnitParams)(implicit p: Parameters) extends LazyModule
  with HasXSParameter {
  implicit val unitParams: MemUnitParams = params
  lazy val module: MemUnitImp = unitParams.unitType match {
    case StoreDataUnit() => new StoreDataUnitImp(this)
    case _ => null
  }
}

class MemUnitIO()(implicit p: Parameters, params: MemUnitParams) extends XSBundle {
  // from
  val fromCtrl = new Bundle() {
    val redirect  = Flipped(ValidIO(new Redirect))
    val hartId    = Input(UInt(hartIdLen.W))
    val csrCtrl   = Flipped(new CustomCSRCtrlIO)
    val trigger   = OptionWrapper(params.hasTrigger, Input(new CsrTriggerBundle))
  }
  val fromIssue  = params.genLsPipelineSinkDecoupledBundle
  val fromTlb    = new Bundle() {
    val resp = Flipped(DecoupledIO(new TlbResp(2)))
    val hint = Flipped(new TlbHintReq)
  }
  val fromDCache = new DCacheLoadRespIO
  val fromPmp    = Flipped(new PMPRespBundle())

  // to
  val toIssue   = params.genLsPipelineSourceDecoupledBundle
  val toDCache  = new DCacheLoadReqIO
  val toTlb     = new Bundle() {
    val req = DecoupledIO(new TlbReq)
    val req_kill = Output(Bool())
  }
}

class MemUnitImp(override val wrapper: MemUnit)(implicit p: Parameters, val params: MemUnitParams) extends LazyModuleImp(wrapper)
  with HasPerfEvents
  with HasXSParameter
  with HasDCacheParameters
  with HasLoadHelper
{
  // duplicate reg for ldout and vecldout
  protected val LdDataDup = 3
  require(LdDataDup >= 2)

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

  protected val fromCtrl   = io.fromCtrl
  protected val fromIssue  = io.fromIssue
  protected val fromTlb    = io.fromTlb
  protected val fromPmp    = io.fromPmp
  protected val fromDCache = io.fromDCache
  protected val toIssue    = io.toIssue
  protected val toDCache   = io.toDCache
  protected val toTlb      = io.toTlb

  def getIqIssue(): Seq[DecoupledIO[LsPipelineBundle]] = {
    fromIssue.zip(params.issueParams).filter(_._2.isIq).map(_._1)
  }

  def getIqWb(): Seq[DecoupledIO[LsPipelineBundle]] = {
    params.issueParams.filter(_.isIq).map(_.getPort()).distinct.map(
      toIssue(_)
    )
  }

  def getPrefetchIssue(): Seq[DecoupledIO[LsPipelineBundle]] = {
    fromIssue.zip(params.issueParams).filter(_._2.isPrefetch).map(_._1)
  }

  def getVectorIssue(): Seq[DecoupledIO[LsPipelineBundle]] = {
    fromIssue.zip(params.issueParams).filter(_._2.isVector).map(_._1)
  }

  def getVectorWb(): Seq[DecoupledIO[LsPipelineBundle]] = {
    params.issueParams.filter(_.isVector).map(_.getPort()).distinct.map(
      toIssue(_)
    )
  }

  def getMisalignBufIssue(): Seq[DecoupledIO[LsPipelineBundle]] = {
    fromIssue.zip(params.issueParams).filter(_._2.isMisalignBuf).map(_._1)
  }

  def getMisalignBufWb(): Seq[DecoupledIO[LsPipelineBundle]] = {
    params.issueParams.filter(_.isMisalignBuf).map(_.getPort()).distinct.map(
      toIssue(_)
    )
  }


  private val hasLoadTrigger = params.hasLoadExe
  private val hasStoreTrigger = params.hasStoreAddrExe
  val loadTrigger = OptionWrapper(hasLoadTrigger, Module(new MemTrigger(MemType.LOAD)))
  val storeTrigger = OptionWrapper(hasStoreTrigger, Module(new MemTrigger(MemType.STORE)))
  val triggers = Seq(loadTrigger, storeTrigger)

  // Pipeline
  // --------------------------------------------------------------------------------
  // stage 0
  // --------------------------------------------------------------------------------
  val s0Out    = WireInit(0.U.asTypeOf(DecoupledIO(new LsPipelineBundle)))
  val s0ArbOut = WireInit(0.U.asTypeOf(DecoupledIO(new LsPipelineBundle)))
  val s0Kill   = WireInit(false.B)
  val s0WBPort = WireInit(VecInit(Seq.fill(params.numWBPorts)(false.B)))
  val perf_s0SelectPort = WireInit(0.U)
  // select
  fromIssue.zip(params.issueParams).zipWithIndex.map {
    case ((issue, issueParam), i) =>
      val selected = (if (i == 0) true.B else !fromIssue.slice(0, i).map(_.valid).reduce(_|_))

      // generate vaddr
      val vaddr =
        if (issueParam.hasAGU)
          issue.bits.src(0) + SignExt(issue.bits.uop.imm(11, 0), VAddrBits)
        else if (issueParam.isAtomics)
          issue.bits.src(0)
        else
          issue.bits.vaddr(VAddrBits - 1, 0)

      // generate full vaddr
      val fullva =
        if (issueParam.hasAGU)
          issue.bits.src(0) + SignExt(issue.bits.uop.imm(11, 0), XLEN)
        else if (issueParam.isAtomics)
          issue.bits.src(0)
        else
          issue.bits.vaddr

      // generate mask
      val mask =
        if (issueParam.hasAGU)
          genVWmask128(vaddr, issue.bits.uop.fuOpType(2, 0))
        else
          issue.bits.mask

      issue.ready := false.B
      when (issue.valid && selected) {
        perf_s0SelectPort := i.U

        val wbPort = MemWBPortMap.getPort(issueParam.wbPort)
        if (wbPort >= 0)
          s0WBPort(wbPort) := true.B
        s0ArbOut.valid       := issue.valid
        s0ArbOut.bits        := issue.bits
        s0ArbOut.bits.vaddr  := vaddr
        s0ArbOut.bits.fullva := fullva
        s0ArbOut.bits.mask   := mask
        s0ArbOut.bits.vecActive := !issue.bits.isVector || issue.bits.vecActive
        issue.ready   := s0ArbOut.ready && (if (issueParam.hasDCacheQuery) toDCache.req.ready else true.B)
      }
  }

  s0Out <> s0ArbOut
  s0Kill := s0Out.bits.uop.robIdx.needFlush(fromCtrl.redirect)
  when (toTlb.req.valid && s0Out.bits.isFirstIssue) {
    s0Out.bits.uop.debugInfo.tlbFirstReqTime := GTimer()
  }

  // to tlb
  toTlb.req.valid             := s0Out.valid
  toTlb.req.bits.vaddr        := s0Out.bits.vaddr
  toTlb.req.bits.fullva       := s0Out.bits.fullva
  toTlb.req.bits.checkfullva  := s0Out.bits.isIq || s0Out.bits.isVector
  toTlb.req.bits.cmd          := Mux(s0Out.bits.isStore, TlbCmd.write, TlbCmd.read)
  toTlb.req.bits.isPrefetch   := s0Out.bits.isPrefetch
  toTlb.req.bits.size         := s0Out.bits.uop.fuOpType(2, 0)
  toTlb.req.bits.kill         := s0Kill
  toTlb.req.bits.memidx.is_ld := !s0Out.bits.isStore
  toTlb.req.bits.memidx.is_st := s0Out.bits.isStore
  toTlb.req.bits.memidx.idx   := Mux(
                                    s0Out.bits.isStore,
                                    s0Out.bits.uop.sqIdx.value,
                                    s0Out.bits.uop.lqIdx.value
                                  )
  toTlb.req.bits.no_translate := s0Out.bits.tlbNoQuery
  toTlb.req.bits.hyperinst    := LSUOpType.isHlv(s0Out.bits.uop.fuOpType)
  toTlb.req.bits.hlvx         := LSUOpType.isHlvx(s0Out.bits.uop.fuOpType)
  toTlb.req.bits.pmp_addr     := DontCare
  toTlb.req.bits.debug.robIdx := s0Out.bits.uop.robIdx
  toTlb.req.bits.debug.pc     := s0Out.bits.uop.pc
  toTlb.req.bits.debug.isFirstIssue := s0Out.bits.isFirstIssue

  // to DCache
  toDCache.req.valid             := s0Out.valid
  toDCache.req.bits.cmd          := MemoryOpConstants.M_XRD
  toDCache.req.bits.vaddr        := s0Out.bits.vaddr
  toDCache.req.bits.mask         := s0Out.bits.mask
  toDCache.req.bits.data         := DontCare
  toDCache.req.bits.isFirstIssue := s0Out.bits.isFirstIssue
  toDCache.req.bits.instrtype    := LOAD_SOURCE.U
  toDCache.req.bits.debug_robIdx := s0Out.bits.uop.robIdx.value
  toDCache.req.bits.replayCarry  := s0Out.bits.replayCarry
  toDCache.req.bits.id           := DontCare // TODO: update cache meta
  toDCache.req.bits.lqIdx        := s0Out.bits.uop.lqIdx
  toDCache.s0_pc                 := s0Out.bits.uop.pc
  toDCache.pf_source             := L1_HW_PREFETCH_NULL
  toDCache.is128Req              := s0Out.bits.is128bit
  toDCache.replacementUpdated    := false.B


  val perf_s0SelectIssue = Mux1H(fromIssue.zipWithIndex.map{
    case (fromIssue, i) =>
      (perf_s0SelectPort === i.U) -> fromIssue
  })
  XSPerfAccumulate("s0InValid", perf_s0SelectIssue.valid)
  XSPerfAccumulate("s0InBlock", perf_s0SelectIssue.valid && !perf_s0SelectIssue.ready)
  XSPerfAccumulate("s0StallOut", s0Out.valid && !s0Out.ready)

  printPipeLine(s0Out.bits, s0Out.valid, "s0")
  // Pipeline
  // --------------------------------------------------------------------------------
  // stage 1
  // --------------------------------------------------------------------------------
  val s1In   = WireInit(0.U.asTypeOf(DecoupledIO(new LsPipelineBundle)))
  val s1Out  = WireInit(0.U.asTypeOf(DecoupledIO(new LsPipelineBundle)))
  val s1Kill = WireInit(false.B)
  NewPipelineConnect.connect(s0Out, s1In, s1In.fire, s0Kill)
  val s1WBPort = RegEnable(s0WBPort, 0.U.asTypeOf(s0WBPort), s0Out.fire)

  s1Kill := s1In.bits.uop.robIdx.needFlush(fromCtrl.redirect)

  // to tlb
  toTlb.req_kill := s1Kill
  toTlb.req.bits.pmp_addr := s1In.bits.paddr
  fromTlb.resp.ready := true.B // TODO: why dtlbResp needs a ready?

  // to dcache
  toDCache.s1_paddr_dup_lsu := fromTlb.resp.bits.paddr(0)
  toDCache.s1_paddr_dup_dcache := fromTlb.resp.bits.paddr(1)
  toDCache.s1_kill := s1Kill
  toDCache.s1_kill_data_read := s1Kill
  toDCache.s1_pc := s1In.bits.uop.pc

  s1Out <> s1In
  s1Out.bits.vaNeedExt := fromTlb.resp.bits.excp(0).vaNeedExt
  s1Out.bits.isHyper   := fromTlb.resp.bits.excp(0).isHyper
  s1Out.bits.paddr     := fromTlb.resp.bits.paddr(0)
  s1Out.bits.gpaddr    := fromTlb.resp.bits.gpaddr(0)
  s1Out.bits.isForVSnonLeafPTE := fromTlb.resp.bits.isForVSnonLeafPTE
  s1Out.bits.tlbMiss   := s1In.valid && fromTlb.resp.valid && fromTlb.resp.bits.miss
  s1Out.bits.ptwBack   := fromTlb.resp.bits.ptwBack
  s1Out.bits.uop.flushPipe := false.B

  if (params.hasTrigger) {
    triggers.map(_.get).foreach {
      case module =>
        module.io.fromCsrTrigger.tdataVec             := fromCtrl.trigger.get.tdataVec
        module.io.fromCsrTrigger.tEnableVec           := fromCtrl.trigger.get.tEnableVec
        module.io.fromCsrTrigger.triggerCanRaiseBpExp := fromCtrl.trigger.get.triggerCanRaiseBpExp
        module.io.fromCsrTrigger.debugMode            := fromCtrl.trigger.get.debugMode
        module.io.fromLoadStore.vaddr                 := s1In.bits.vaddr
        module.io.fromLoadStore.isVectorUnitStride    := s1In.bits.isVector && s1In.bits.is128bit
        module.io.fromLoadStore.mask                  := s1In.bits.mask
    }
  }

  val s1TriggerAction = true match {
    case _ if (hasLoadTrigger && hasStoreTrigger) =>
              val s1LoadTriggerAction = loadTrigger.get.io.toLoadStore.triggerAction
              val s1StoreTriggerAction = storeTrigger.get.io.toLoadStore.triggerAction
              Mux(s1In.bits.isAtomic,
                s1LoadTriggerAction | s1StoreTriggerAction,
                Mux(
                  s1In.bits.isStore,
                  storeTrigger.get.io.toLoadStore.triggerAction,
                  loadTrigger.get.io.toLoadStore.triggerAction
                )
              )
    case _ if (hasLoadTrigger && !hasStoreTrigger) =>
              loadTrigger.get.io.toLoadStore.triggerAction
    case _ if (!hasLoadTrigger && hasStoreTrigger) =>
              storeTrigger.get.io.toLoadStore.triggerAction
    case _ => TriggerAction.None
  }
  val s1TriggerDebugMode = TriggerAction.isDmode(s1TriggerAction)
  val s1TriggerBreakpoint = TriggerAction.isExp(s1TriggerAction)
  s1Out.bits.uop.trigger   := s1TriggerAction
  // exception generate
  s1Out.bits.uop.exceptionVec(breakPoint) := s1TriggerBreakpoint
  // vector vaddr offset generate
  val s1TriggerVaddr = true match {
    case _ if (hasLoadTrigger && hasStoreTrigger) =>
              Mux(
                s1In.bits.isStore,
                storeTrigger.get.io.toLoadStore.triggerVaddr,
                loadTrigger.get.io.toLoadStore.triggerVaddr
              )
    case _ if (hasLoadTrigger && !hasStoreTrigger) =>
              loadTrigger.get.io.toLoadStore.triggerVaddr
    case _ if (!hasLoadTrigger && hasStoreTrigger) =>
              storeTrigger.get.io.toLoadStore.triggerVaddr
    case _ => 0.U
  }
  s1Out.bits.vecVaddrOffset := Mux(
    s1TriggerDebugMode || s1TriggerBreakpoint,
    s1TriggerVaddr - s1In.bits.vecBaseVaddr,
    s1In.bits.vaddr + genVFirstUnmask(s1In.bits.mask).asUInt - s1In.bits.vecBaseVaddr
  )
  // vector mask generate
  val s1TriggerMask = true match {
    case _ if (hasLoadTrigger && hasStoreTrigger) =>
              Mux(
                s1In.bits.isStore,
                storeTrigger.get.io.toLoadStore.triggerMask,
                loadTrigger.get.io.toLoadStore.triggerMask
              )
    case _ if (hasLoadTrigger && !hasStoreTrigger) =>
              loadTrigger.get.io.toLoadStore.triggerMask
    case _ if (!hasLoadTrigger && hasStoreTrigger) =>
              storeTrigger.get.io.toLoadStore.triggerMask
    case _ => 0.U
  }
  s1Out.bits.vecTriggerMask := Mux(
    s1TriggerDebugMode || s1TriggerBreakpoint,
    s1TriggerMask,
    0.U
  )

  // make dev happy
  val s1TlbMemIdx = fromTlb.resp.bits.memidx
  val s1TlbMemIdxHit = Mux(
    s1In.bits.isStore,
    s1TlbMemIdx.is_st && s1In.bits.uop.sqIdx.value === s1TlbMemIdx.idx,
    s1TlbMemIdx.is_ld && s1In.bits.uop.lqIdx.value === s1TlbMemIdx.idx
  )
  when (fromTlb.resp.valid && !fromTlb.resp.bits.miss && s1TlbMemIdxHit) {
    s1Out.bits.uop.debugInfo.tlbRespTime := GTimer()
  }

  XSPerfAccumulate("s1InValid", s1In.valid)
  XSPerfAccumulate("s1InFire", s1In.fire)
  XSPerfAccumulate("s1FirstIssue", s1In.valid&& s1In.bits.isFirstIssue)
  XSPerfAccumulate("s1TlbMiss", s1In.valid && fromTlb.resp.valid && fromTlb.resp.bits.miss)
  XSPerfAccumulate("s1TlbMissFirstIssue", s1In.valid && fromTlb.resp.valid && fromTlb.resp.bits.miss && s1In.bits.isFirstIssue)
  XSPerfAccumulate("s1StallOut", s1Out.valid && !s1Out.ready)

  printPipeLine(s1Out.bits, s1Out.valid, "s1")
  // Pipeline
  // --------------------------------------------------------------------------------
  // stage 2
  // --------------------------------------------------------------------------------
  val s2In   = WireInit(0.U.asTypeOf(DecoupledIO(new LsPipelineBundle)))
  val s2Out  = WireInit(0.U.asTypeOf(DecoupledIO(new LsPipelineBundle)))
  val s2Kill = WireInit(false.B)
  NewPipelineConnect.connect(s1Out, s2In, s2In.fire, s1Kill)
  val s2WBPort = RegEnable(s1WBPort, 0.U.asTypeOf(s1WBPort), s1Out.fire)

  toTlb.req_kill := false.B
  // TODO: dcache resp
  fromDCache.resp.ready := true.B
  // kill dcache write intent request when mmio or exception
  toDCache.s2_kill := s2In.bits.uop.robIdx.needFlush(fromCtrl.redirect)
  toDCache.s2_pc   := s2In.bits.uop.pc

  val s2RdataOH = genRdataOH(s2In.bits.uop)
  val s2DataSelectOffset = genDataSelectByOffset(s2In.bits.paddr(2, 0))
  val s2DataFromCache = Wire(new LoadDataFromDcacheBundle)
  s2DataFromCache.respDcacheData      := fromDCache.resp.bits.data
  s2DataFromCache.forwardDchannel     := false.B
  s2DataFromCache.forwardDataDchannel := DontCare
  s2DataFromCache.forwardMSHR         := false.B
  s2DataFromCache.forwardDataMSHR     := DontCare
  s2DataFromCache.forwardResultValid  := false.B

  s2DataFromCache.forwardMask         := DontCare
  s2DataFromCache.forwardData         := DontCare
  s2DataFromCache.uop                 := s2In.bits.uop
  s2DataFromCache.addrOffset          := s2In.bits.paddr(3, 0)

  s2Out <> s2In
  s2Out.bits.uop.vpu.vstart := s2In.bits.vecVaddrOffset >> s2In.bits.uop.vpu.veew

  printPipeLine(s2Out.bits, s2Out.valid, "s2")

  XSPerfAccumulate("s2InValid", s2In.valid)
  XSPerfAccumulate("s2InFire", s2In.fire)
  XSPerfAccumulate("s2StallOut", s2Out.valid && !s2Out.ready)

  // Pipeline
  // --------------------------------------------------------------------------------
  // stage 3
  // --------------------------------------------------------------------------------
  val s3In   = WireInit(0.U.asTypeOf(DecoupledIO(new LsPipelineBundle)))
  val s3Out  = WireInit(0.U.asTypeOf(DecoupledIO(new LsPipelineBundle)))
  val s3Kill = WireInit(false.B)
  NewPipelineConnect.connect(s2Out, s3In, s3In.fire, s2Kill)
  val s3WBPort = RegEnable(s2WBPort, 0.U.asTypeOf(s2WBPort), s2Out.fire)

  s3Kill := s3In.bits.uop.robIdx.needFlush(fromCtrl.redirect)

  val s3MergedTLDataClip = VecInit(List.fill(LdDataDup)(
    RegEnable(Mux(
      s2Out.bits.paddr(3),
      s2DataFromCache.mergeTLData()(VLEN - 1, 64),
      s2DataFromCache.mergeTLData()(63, 0)
    ).asTypeOf(Vec(XLEN / 8, UInt(8.W))), s2Out.fire))
  )
  val s3MergedDataClip = VecInit((0 until LdDataDup).map(i => {
    VecInit((0 until XLEN / 8).map(j =>
      s3MergedTLDataClip(i)(j)
    )).asUInt
  }))
  val s3Data = VecInit((0 until LdDataDup).map(i => {
    VecInit(Seq(
      s3MergedDataClip(i)(63,    0),
      s3MergedDataClip(i)(63,    8),
      s3MergedDataClip(i)(63,   16),
      s3MergedDataClip(i)(63,   24),
      s3MergedDataClip(i)(63,   32),
      s3MergedDataClip(i)(63,   40),
      s3MergedDataClip(i)(63,   48),
      s3MergedDataClip(i)(63,   56),
    ))
  }))

  val s3RdataOH = RegEnable(s2RdataOH, 0.U.asTypeOf(s2RdataOH), s2Out.fire)
  val s3DataSelectOffset = RegEnable(s2DataSelectOffset, 0.U.asTypeOf(s2DataSelectOffset), s2Out.fire)
  val s3PickedData = VecInit((0 until LdDataDup).map(i => {
    Mux1H(genDataSelectByOffset(s3In.bits.paddr(2, 0)), s3Data(i))
  }))
  val s3WBData = newRdataHelper(genRdataOH(s3In.bits.uop), s3PickedData(0))

  s3Out <> s3In
  s3Out.bits.data := s3WBData

  s3Out.ready := false.B
  toIssue.zip(s3WBPort).map {
    case (wb, select) =>
      wb.valid := s3Out.valid && select
      wb.bits := s3Out.bits
      when (select) {
        s3Out.ready := wb.ready
      }
  }

  printPipeLine(s3Out.bits, s3Out.valid, "s3")
  // perf cnt
  XSPerfAccumulate("s3InValid", s3In.valid)
  XSPerfAccumulate("s3InFire", s3In.fire)
  XSPerfAccumulate("s3StallOut", s3Out.valid && !s3Out.ready)

  // bug lyq: some signals in perfEvents are no longer suitable for the current MemBlock design
  // hardware performance counter
  val perfEvents = Seq(
    ("s0InFire", s0Out.fire),
    ("stallDCache", s0Out.valid && !fromDCache.resp.ready),
    ("s1InFire", s1Out.fire),
    ("s1TlbMiss", s1Out.fire && fromTlb.resp.bits.miss),
    ("s2InFire", s2Out.fire),
    ("s2DCacheMiss", s2Out.fire && fromDCache.resp.bits.miss),
  )
  generatePerfEvent()
}
