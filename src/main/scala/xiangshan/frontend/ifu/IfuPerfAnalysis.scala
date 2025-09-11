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
import xiangshan.frontend.FrontendTopDownBundle
import xiangshan.frontend.icache.ICachePerfInfo
import xiangshan.frontend.icache.ICacheTopdownInfo

class IfuPerfAnalysis(implicit p: Parameters) extends IfuModule {
  class PerfAnalysisIO(implicit p: Parameters) extends IfuBundle {
    class IfuCheckPerfInfo(implicit p: Parameters) extends IfuBundle {
      val valid:         Vec[Bool] = Vec(FetchPorts, Bool())
      val perfFaultType: Vec[UInt] = Vec(FetchPorts, PreDecodeFaultType())
      val startVAddr:    Vec[UInt] = Vec(FetchPorts, UInt(VAddrBits.W))
      val target:        Vec[UInt] = Vec(FetchPorts, UInt(VAddrBits.W))
      val taken:         Vec[Bool] = Vec(FetchPorts, Bool())
      val misPred:       Vec[Bool] = Vec(FetchPorts, Bool())
      val misEndOffset:  Vec[UInt] = Vec(FetchPorts, UInt(log2Ceil(FetchBlockInstNum).W))
    }

    class IfuTopdownIn(implicit p: Parameters) extends IfuBundle {
      val icacheTopdown:   ICacheTopdownInfo     = new ICacheTopdownInfo
      val ftqTopdown:      FrontendTopDownBundle = new FrontendTopDownBundle
      val topdownRedirect: Valid[Redirect]       = Valid(new Redirect)
    }
    class IfuTopdownOut(implicit p: Parameters) extends IfuBundle {
      val topdown = new FrontendTopDownBundle
    }

    class ToIBufferPerfInfo(implicit p: Parameters) extends IfuBundle {
      val ibufferValid: Bool      = Bool()
      val ibufferReady: Bool      = Bool()
      val enqEnable:    UInt      = UInt(IBufEnqWidth.W)
      val startVAddr:   Vec[UInt] = Vec(FetchPorts, UInt(VAddrBits.W))
    }

    class IfuPerfInfo(implicit p: Parameters) extends IfuBundle {
      val icachePerfInfo: ICachePerfInfo    = new ICachePerfInfo
      val checkPerfInfo:  IfuCheckPerfInfo  = new IfuCheckPerfInfo
      val toIBufferInfo:  ToIBufferPerfInfo = new ToIBufferPerfInfo
    }

    class IfuPrefCtrl(implicit p: Parameters) extends IfuBundle {
      val ftqReqValid:     Bool      = Bool()
      val ftqReqReady:     Bool      = Bool()
      val backendRedirect: Bool      = Bool()
      val ifuWbRedirect:   Bool      = Bool()
      val ifuS0Flush:      Vec[Bool] = Vec(FetchPorts, Bool())
      val ifuS3Valid:      Bool      = Bool()
      val ifuS2Fire:       Bool      = Bool()
      val ifuS3Fire:       Bool      = Bool()
      val icacheRespValid: Bool      = Bool()
    }

    val ifuPerfCtrl: IfuPrefCtrl   = Input(new IfuPrefCtrl)
    val topdownIn:   IfuTopdownIn  = Input(new IfuTopdownIn)
    val topdownOut:  IfuTopdownOut = Output(new IfuTopdownOut)
    val perfInfo:    IfuPerfInfo   = Input(new IfuPerfInfo)
  }
  val io: PerfAnalysisIO = IO(new PerfAnalysisIO)
  private val icacheTopdown     = io.topdownIn.icacheTopdown
  private val ftqTopdown        = io.topdownIn.ftqTopdown
  private val topdownRedirect   = io.topdownIn.topdownRedirect
  private val s4_icachePerfInfo = io.perfInfo.icachePerfInfo
  private val checkPerfInfo     = io.perfInfo.checkPerfInfo
  private val toIBufferInfo     = io.perfInfo.toIBufferInfo

  private def numOfStage = 3
  require(numOfStage > 1, "Ifu numOfStage must be greater than 1")
  private val topdownStages = RegInit(VecInit(Seq.fill(numOfStage)(0.U.asTypeOf(new FrontendTopDownBundle))))
  // bubble events in IFU, only happen in stage 1
  private val icacheMissBubble = icacheTopdown.iCacheMissBubble
  private val itlbMissBubble   = icacheTopdown.itlbMissBubble

  // only driven by clock, not valid-ready
  topdownStages(0) := ftqTopdown
  for (i <- 1 until numOfStage) {
    topdownStages(i) := topdownStages(i - 1)
  }
  when(icacheMissBubble) {
    topdownStages(1).reasons(TopDownCounters.ICacheMissBubble.id) := true.B
  }
  when(itlbMissBubble) {
    topdownStages(1).reasons(TopDownCounters.ITLBMissBubble.id) := true.B
  }
  when(topdownRedirect.valid) {
    // only redirect from backend, IFU redirect itself is handled elsewhere
    when(topdownRedirect.bits.debugIsCtrl) {}.elsewhen(topdownRedirect.bits.debugIsMemVio) {
      for (i <- 0 until numOfStage) {
        topdownStages(i).reasons(TopDownCounters.MemVioRedirectBubble.id) := true.B
      }
      io.topdownOut.topdown.reasons(TopDownCounters.MemVioRedirectBubble.id) := true.B
    }.otherwise {
      for (i <- 0 until numOfStage) {
        topdownStages(i).reasons(TopDownCounters.OtherRedirectBubble.id) := true.B
      }
      io.topdownOut.topdown.reasons(TopDownCounters.OtherRedirectBubble.id) := true.B
    }
  }

  when(io.ifuPerfCtrl.ifuWbRedirect) {
    for (i <- 0 until numOfStage) {
      topdownStages(i).reasons(TopDownCounters.BTBMissBubble.id) := true.B
    }
  }
  io.topdownOut.topdown := topdownStages(numOfStage - 1)

  /** <PERF> f0 fetch bubble */
  XSPerfAccumulate("fetch_bubble_ftq_not_valid", !io.ifuPerfCtrl.ftqReqValid && io.ifuPerfCtrl.ftqReqReady)
  XSPerfAccumulate("fetch_flush_wb_redirect", io.ifuPerfCtrl.backendRedirect)
  XSPerfAccumulate("fetch_flush_wb_redirect", io.ifuPerfCtrl.ifuWbRedirect)
  XSPerfAccumulate("fetch_flush_s0_flush_from_bpu", io.ifuPerfCtrl.ifuS0Flush(0) | io.ifuPerfCtrl.ifuS0Flush(1))
  XSPerfAccumulate("fetch_bubble_icache_not_resp", io.ifuPerfCtrl.ifuS3Valid && !io.ifuPerfCtrl.icacheRespValid)
  XSPerfAccumulate("fetch_flush_s0_flush_from_bpu", io.ifuPerfCtrl.ifuS0Flush(0) | io.ifuPerfCtrl.ifuS0Flush(1))

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
    XSPerfAccumulate(f"fetch${i}_predecode_flush_jalFault", checkJalFault(i))
    XSPerfAccumulate(f"fetch${i}_predecode_flush_jalrFault", checkJalrFault(i))
    XSPerfAccumulate(f"fetch${i}_predecode_flush_retFault", checkRetFault(i))
    XSPerfAccumulate(f"fetch${i}_predecode_flush_targetFault", checkTargetFault(i))
    XSPerfAccumulate(f"fetch${i}_predecode_flush_notCFIFault", checkNotCFIFault(i))
    XSPerfAccumulate(f"fetch${i}_predecode_flush_invalidTakenFault", checkInvalidTaken(i))

    XSDebug(
      checkRetFault(i),
      "fetch:%x  startAddr:%x  nextStartAddr:%x  taken:%d     takenEndOffset:%d\n",
      i.U,
      checkPerfInfo.startVAddr(i),
      checkPerfInfo.target(i),
      checkPerfInfo.taken(i),
      checkPerfInfo.misEndOffset(i)
    )
  }

  /* *** Perf *** */
  private val ibufferFire = toIBufferInfo.ibufferValid && toIBufferInfo.ibufferReady
  XSPerfAccumulate("frontendFlush", io.ifuPerfCtrl.ifuWbRedirect)
  XSPerfAccumulate("ifu_req", ibufferFire)
  XSPerfAccumulate("ifu_miss", ibufferFire && !s4_icachePerfInfo.hit)
  XSPerfAccumulate("ifu_req_cacheline_0", ibufferFire)
  XSPerfAccumulate("ifu_req_cacheline_1", ibufferFire && s4_icachePerfInfo.isDoubleLine)
  XSPerfAccumulate("ifu_req_cacheline_0_hit", ibufferFire && s4_icachePerfInfo.hit0)
  XSPerfAccumulate("ifu_req_cacheline_1_hit", ibufferFire && s4_icachePerfInfo.hit1)
  XSPerfAccumulate("only_0_hit", ibufferFire && s4_icachePerfInfo.hit0NoReq1)
  XSPerfAccumulate("only_0_miss", ibufferFire && s4_icachePerfInfo.miss0NoReq1)
  XSPerfAccumulate("hit_0_hit_1", ibufferFire && s4_icachePerfInfo.hit0Hit1)
  XSPerfAccumulate("hit_0_miss_1", ibufferFire && s4_icachePerfInfo.hit0Miss1)
  XSPerfAccumulate("miss_0_hit_1", ibufferFire && s4_icachePerfInfo.miss0Hit1)
  XSPerfAccumulate("miss_0_miss_1", ibufferFire && s4_icachePerfInfo.miss0Miss1)
  XSPerfAccumulate("except_0", ibufferFire && s4_icachePerfInfo.except)
  XSPerfHistogram(
    "ifu2ibuffer_validCnt",
    PopCount(toIBufferInfo.enqEnable),
    ibufferFire,
    0,
    FetchBlockInstNum + 1,
    1
  )

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
  fetchIBufferDumpData.exception    := ibufferFire && s4_icachePerfInfo.except
  fetchIBufferDumpData.isCacheHit   := ibufferFire && s4_icachePerfInfo.hit

  private val ifuWbToFtqDumpData = Wire(new IfuWbToFtqDB)
  for (i <- 0 until FetchPorts) {
    ifuWbToFtqDumpData.startAddr(i)      := checkPerfInfo.startVAddr(i)
    ifuWbToFtqDumpData.isMissPred(i)     := checkPerfInfo.misPred(i)
    ifuWbToFtqDumpData.missPredOffset(i) := checkPerfInfo.misEndOffset(i)
  }
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
