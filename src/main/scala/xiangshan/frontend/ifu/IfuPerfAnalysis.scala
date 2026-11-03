// Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
// Copyright (c) 2020-2021 Peng Cheng Laboratory
//
// XiangShan is licensed under Mulan PSL v2.
// You can use this software according to the terms and conditions of the Mulan PSL v2.
// You may obtain a copy of Mulan PSL v2 at:
//          https://license.coscl.org.cn/MulanPSL2
//
// THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
// EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
// MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
//
// See the Mulan PSL v2 for more details.

package xiangshan.frontend.ifu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.ChiselDB
import utility.Constantin
import utility.HasPerfEvents
import utility.SignExt
import utility.XSDebug
import utility.XSPerfAccumulate
import utility.XSPerfHistogram
import xiangshan.Redirect
import xiangshan.TopDownCounters
import xiangshan.XSCoreParamsKey
import xiangshan.frontend.FrontendRedirectTopdown
import xiangshan.frontend.FrontendTopDownBundle
import xiangshan.frontend.icache.ICachePerfInfo
import xiangshan.frontend.icache.ICacheTopdownInfo

class IfuPerfAnalysis(implicit p: Parameters) extends IfuModule {
  class PerfAnalysisIO(implicit p: Parameters) extends IfuBundle {
    class IfuCheckPerfInfo(implicit p: Parameters) extends IfuBundle {
      val valid:         Vec[Bool] = Vec(FetchPorts, Bool())
      val perfFaultType: Vec[UInt] = Vec(FetchPorts, PreDecodeFaultType())
      val startVAddr:    Vec[UInt] = Vec(FetchPorts, UInt(VAddrBits.W))
      val taken:         Vec[Bool] = Vec(FetchPorts, Bool())
      val selectBlock:   Bool      = Bool()
      val misPred:       Bool      = Bool()
      val misEndOffset:  UInt      = UInt(log2Ceil(FetchBlockInstNum).W)
      val uncacheBubble: Bool      = Bool()
    }

    class IfuTopdownIn(implicit p: Parameters) extends IfuBundle {
      val icacheTopdown:   ICacheTopdownInfo       = new ICacheTopdownInfo
      val ftqTopdown:      FrontendTopDownBundle   = new FrontendTopDownBundle
      val overrideReasons: FrontendTopDownBundle   = new FrontendTopDownBundle
      val redirectTopdown: FrontendRedirectTopdown = new FrontendRedirectTopdown
    }
    class IfuTopdownOut(implicit p: Parameters) extends IfuBundle {
      val topdown = new FrontendTopDownBundle
    }

    class ToIBufferPerfInfo(implicit p: Parameters) extends IfuBundle {
      val ibufferFire: Bool      = Bool()
      val enqEnable:   UInt      = UInt(IBufferEnqueueWidth.W)
      val startVAddr:  Vec[UInt] = Vec(FetchPorts, UInt(VAddrBits.W))
    }

    class IfuPerfInfo(implicit p: Parameters) extends IfuBundle {
      val icachePerfInfo: ICachePerfInfo    = new ICachePerfInfo
      val checkPerfInfo:  IfuCheckPerfInfo  = new IfuCheckPerfInfo
      val toIBufferInfo:  ToIBufferPerfInfo = new ToIBufferPerfInfo
    }

    class IfuPerfCtrl(implicit p: Parameters) extends IfuBundle {
      val s1Ready:          Bool = Bool()
      val s2Ready:          Bool = Bool()
      val uncacheActive:    Bool = Bool()
      val fromFtqBubble:    Bool = Bool()
      val backendRedirect:  Bool = Bool()
      val ifuWbRedirect:    Bool = Bool()
      val fromBpuFlush:     Bool = Bool()
      val fromICacheBubble: Bool = Bool()
    }

    val ifuPerfCtrl: IfuPerfCtrl   = Input(new IfuPerfCtrl)
    val topdownIn:   IfuTopdownIn  = Input(new IfuTopdownIn)
    val topdownOut:  IfuTopdownOut = Output(new IfuTopdownOut)
    val perfInfo:    IfuPerfInfo   = Input(new IfuPerfInfo)
  }
  val io: PerfAnalysisIO = IO(new PerfAnalysisIO)
  private val icacheTopdown     = io.topdownIn.icacheTopdown
  private val ftqTopdown        = io.topdownIn.ftqTopdown
  private val s3_icachePerfInfo = io.perfInfo.icachePerfInfo
  private val checkPerfInfo     = io.perfInfo.checkPerfInfo
  private val toIBufferInfo     = io.perfInfo.toIBufferInfo

  // FTQ already registers its supply cause at the ICache S1 boundary. Local
  // metadata/TLB waits and S0 flushes need that same edge before entering IFU.
  private val emptyTag = Wire(new FrontendTopDownBundle)
  emptyTag.setReason(TopDownCounters.FtqEmptyBubble)
  private val cacheInputTopdown = RegInit(emptyTag)
  cacheInputTopdown.setReason(TopDownCounters.NoStall)
  when(icacheTopdown.supplyStall)(cacheInputTopdown.setReason(TopDownCounters.ICacheSupplyBubble))
  when(icacheTopdown.itlbMissBubble)(cacheInputTopdown.setReason(TopDownCounters.ITLBMissBubble))
  when(icacheTopdown.bpuFlushS0)(cacheInputTopdown := io.topdownIn.overrideReasons)
  cacheInputTopdown.redirectOverride(io.topdownIn.redirectTopdown)

  private val cacheOutputTopdown = WireInit(ftqTopdown)
  when(cacheInputTopdown.reason =/= TopDownCounters.NoStall.id.U)(cacheOutputTopdown := cacheInputTopdown)
  when(icacheTopdown.iCacheMissBubble)(cacheOutputTopdown.setReason(TopDownCounters.ICacheMissBubble))
  when(icacheTopdown.bpuFlushS1 || io.ifuPerfCtrl.fromBpuFlush) {
    cacheOutputTopdown := io.topdownIn.overrideReasons
  }
  cacheOutputTopdown.redirectOverride(io.topdownIn.redirectTopdown)

  // Reasons move on empty cycles as well as valid transfers. A stage held by
  // downstream backpressure keeps its tag, just like the corresponding data.
  private val s1Topdown = RegInit(emptyTag)
  private val s2Topdown = RegInit(emptyTag)
  when(io.ifuPerfCtrl.s1Ready)(s1Topdown := cacheOutputTopdown)
  when(io.ifuPerfCtrl.s2Ready)(s2Topdown := s1Topdown)
  Seq(s1Topdown, s2Topdown).foreach(_.redirectOverride(io.topdownIn.redirectTopdown))
  io.topdownOut.topdown := s2Topdown
  // Include waiting, successful single-instruction delivery and cross-page resend.
  when(io.ifuPerfCtrl.uncacheActive) {
    io.topdownOut.topdown.setReason(TopDownCounters.UncacheFetchBubble)
  }
  io.topdownOut.topdown.redirectOverride(io.topdownIn.redirectTopdown)

  /** <PERF> f0 fetch bubble */
  XSPerfAccumulate("fetch_bubble_ftq_not_valid", io.ifuPerfCtrl.fromFtqBubble)
  XSPerfAccumulate("fetch_flush_backend_redirect", io.ifuPerfCtrl.backendRedirect)
  XSPerfAccumulate("fetch_flush_wb_redirect", io.ifuPerfCtrl.ifuWbRedirect)
  XSPerfAccumulate("fetch_flush_from_bpu", io.ifuPerfCtrl.fromBpuFlush)
  XSPerfAccumulate("fetch_bubble_icache_not_resp", io.ifuPerfCtrl.fromICacheBubble)

  /* write back flush type */
  private val checkFaultType    = checkPerfInfo.perfFaultType
  private val checkJalFault     = WireDefault(VecInit.fill(FetchPorts)(false.B))
  private val checkJalrFault    = WireDefault(VecInit.fill(FetchPorts)(false.B))
  private val checkRetFault     = WireDefault(VecInit.fill(FetchPorts)(false.B))
  private val checkTargetFault  = WireDefault(VecInit.fill(FetchPorts)(false.B))
  private val checkNotCFIFault  = WireDefault(VecInit.fill(FetchPorts)(false.B))
  private val checkInvalidTaken = WireDefault(VecInit.fill(FetchPorts)(false.B))
  private val checkFetchValid   = WireDefault(VecInit.fill(FetchPorts)(false.B))
  checkFetchValid := checkPerfInfo.valid

  for (i <- 0 until FetchPorts) {
    val validFetch = checkFetchValid(i)
    checkJalFault(i)     := validFetch && (checkFaultType(i) === PreDecodeFaultType.JalFault)
    checkJalrFault(i)    := validFetch && (checkFaultType(i) === PreDecodeFaultType.JalrFault)
    checkRetFault(i)     := validFetch && (checkFaultType(i) === PreDecodeFaultType.RetFault)
    checkNotCFIFault(i)  := validFetch && (checkFaultType(i) === PreDecodeFaultType.NotCfiFault)
    checkInvalidTaken(i) := validFetch && (checkFaultType(i) === PreDecodeFaultType.InvalidTaken)
  }

  for (i <- 0 until FetchPorts) {
    XSPerfAccumulate(f"squashCycles_bpWrong_preDecode_jalFault_fetchPort_${i}", checkJalFault(i))
    XSPerfAccumulate(f"squashCycles_bpWrong_preDecode_jalrFault_fetchPort_${i}", checkJalrFault(i))
    XSPerfAccumulate(f"squashCycles_bpWrong_preDecode_retFault_fetchPort_${i}", checkRetFault(i))
    XSPerfAccumulate(f"squashCycles_bpWrong_preDecode_targetFault_fetchPort_${i}", checkTargetFault(i))
    XSPerfAccumulate(f"squashCycles_bpWrong_preDecode_notCFIFault_fetchPort_${i}", checkNotCFIFault(i))
    XSPerfAccumulate(f"squashCycles_bpWrong_preDecode_invalidTakenFault_fetchPort_${i}", checkInvalidTaken(i))
  }

  /* *** Perf *** */
  private val ibufferFire = toIBufferInfo.ibufferFire
  XSPerfAccumulate("frontendFlush", io.ifuPerfCtrl.ifuWbRedirect)
  XSPerfAccumulate("ifu_req", ibufferFire)
  XSPerfAccumulate("ifu_miss", ibufferFire && !s3_icachePerfInfo.hit)
  XSPerfAccumulate("ifu_req_cacheline_0", ibufferFire)
  XSPerfAccumulate("ifu_req_cacheline_1", ibufferFire && s3_icachePerfInfo.isDoubleLine)
  XSPerfAccumulate("ifu_req_cacheline_0_hit", ibufferFire && s3_icachePerfInfo.hit0)
  XSPerfAccumulate("ifu_req_cacheline_1_hit", ibufferFire && s3_icachePerfInfo.hit1)
  XSPerfAccumulate("only_0_hit", ibufferFire && s3_icachePerfInfo.hit0NoReq1)
  XSPerfAccumulate("only_0_miss", ibufferFire && s3_icachePerfInfo.miss0NoReq1)
  XSPerfAccumulate("hit_0_hit_1", ibufferFire && s3_icachePerfInfo.hit0Hit1)
  XSPerfAccumulate("hit_0_miss_1", ibufferFire && s3_icachePerfInfo.hit0Miss1)
  XSPerfAccumulate("miss_0_hit_1", ibufferFire && s3_icachePerfInfo.miss0Hit1)
  XSPerfAccumulate("miss_0_miss_1", ibufferFire && s3_icachePerfInfo.miss0Miss1)
  XSPerfAccumulate("except_0", ibufferFire && s3_icachePerfInfo.except)
  XSPerfHistogram(
    "ifu2ibuffer_validCnt",
    PopCount(toIBufferInfo.enqEnable),
    ibufferFire,
    0,
    FetchBlockInstNum + 1,
    1
  )

  XSPerfAccumulate("stallCycles_fetch_uncache", io.perfInfo.checkPerfInfo.uncacheBubble)

  // DB
  private val hartId                     = p(XSCoreParamsKey).HartId
  private val isWriteFetchToIBufferTable = Constantin.createRecord(s"isWriteFetchToIBufferTable$hartId")
  private val isWriteIfuWbToFtqTable     = Constantin.createRecord(s"isWriteIfuWbToFtqTable$hartId")
  private val fetchToIBufferTable        = ChiselDB.createTable(s"FetchToIBuffer$hartId", new FetchToIBufferDB)
  private val ifuWbToFtqTable            = ChiselDB.createTable(s"IfuWbToFtq$hartId", new IfuWbToFtqDB)

  private val fetchIBufferDumpData = Wire(new FetchToIBufferDB)
  fetchIBufferDumpData.startAddr(0) := toIBufferInfo.startVAddr(0)
  fetchIBufferDumpData.startAddr(1) := toIBufferInfo.startVAddr(1)
  fetchIBufferDumpData.instrCount   := PopCount(toIBufferInfo.enqEnable)
  fetchIBufferDumpData.exception    := ibufferFire && s3_icachePerfInfo.except
  fetchIBufferDumpData.isCacheHit   := ibufferFire && s3_icachePerfInfo.hit

  private val ifuWbToFtqDumpData = Wire(new IfuWbToFtqDB)
  for (i <- 0 until FetchPorts) {
    ifuWbToFtqDumpData.startAddr(i) := checkPerfInfo.startVAddr(i)
  }
  ifuWbToFtqDumpData.isMispred         := checkPerfInfo.misPred
  ifuWbToFtqDumpData.misId             := checkPerfInfo.selectBlock
  ifuWbToFtqDumpData.misPredOffset     := checkPerfInfo.misEndOffset
  ifuWbToFtqDumpData.checkJalFault     := checkJalFault(0) | checkJalFault(1)
  ifuWbToFtqDumpData.checkJalrFault    := checkJalrFault(0) | checkJalrFault(1)
  ifuWbToFtqDumpData.checkRetFault     := checkRetFault(0) | checkRetFault(1)
  ifuWbToFtqDumpData.checkNotCFIFault  := checkNotCFIFault(0) | checkNotCFIFault(1)
  ifuWbToFtqDumpData.checkInvalidTaken := checkInvalidTaken(0) | checkInvalidTaken(1)
  ifuWbToFtqDumpData.checkTargetFault  := false.B

  fetchToIBufferTable.log(
    data = fetchIBufferDumpData,
    en = isWriteFetchToIBufferTable.orR && ibufferFire,
    site = "IFU" + p(XSCoreParamsKey).HartId.toString,
    clock = clock,
    reset = reset
  )
  ifuWbToFtqTable.log(
    data = ifuWbToFtqDumpData,
    en = isWriteIfuWbToFtqTable.orR && checkFetchValid(0) | checkFetchValid(1),
    site = "IFU" + p(XSCoreParamsKey).HartId.toString,
    clock = clock,
    reset = reset
  )
}
