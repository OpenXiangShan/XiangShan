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

package xiangshan

import org.chipsalliance.cde.config.{Field, Parameters}
import chisel3._
import chisel3.util._
import huancun._
import system.SoCParamsKey
import xiangshan.backend.datapath.RdConfig._
import xiangshan.backend.datapath.WbConfig._
import xiangshan.backend.dispatch.DispatchParameters
import xiangshan.backend.exu.ExeUnitParams
import xiangshan.backend.fu.FuConfig._
import xiangshan.backend.issue.{IntScheduler, IssueBlockParams, MemScheduler, SchdBlockParams, SchedulerType, VfScheduler}
import xiangshan.backend.regfile.{IntPregParams, PregParams, VfPregParams, FakeIntPregParams}
import xiangshan.backend.BackendParams
import xiangshan.cache.DCacheParameters
import xiangshan.cache.prefetch._
import xiangshan.frontend.{BasePredictor, BranchPredictionResp, FTB, FakePredictor, RAS, Tage, ITTage, Tage_SC, FauFTB}
import xiangshan.frontend.icache.ICacheParameters
import xiangshan.cache.mmu.{L2TLBParameters, TLBParameters}
import xiangshan.frontend._
import xiangshan.frontend.icache.ICacheParameters

import freechips.rocketchip.diplomacy.AddressSet
import freechips.rocketchip.tile.MaxHartIdBits
import system.SoCParamsKey
import huancun._
import huancun.debug._
import xiangshan.cache.wpu.WPUParameters
import coupledL2._
import xiangshan.backend.datapath.WakeUpConfig
import xiangshan.mem.prefetch.{PrefetcherParams, SMSParams}

import scala.math.min

case object XSTileKey extends Field[Seq[XSCoreParameters]]

case object XSCoreParamsKey extends Field[XSCoreParameters]

case class XSCoreParameters
(
  HasPrefetch: Boolean = false,
  HartId: Int = 0,
  XLEN: Int = 64,
  VLEN: Int = 128,
  ELEN: Int = 64,
  HSXLEN: Int = 64,
  HasMExtension: Boolean = true,
  HasCExtension: Boolean = true,
  HasHExtension: Boolean = true,
  HasDiv: Boolean = true,
  HasICache: Boolean = true,
  HasDCache: Boolean = true,
  AddrBits: Int = 64,
  VAddrBits: Int = 39,
  GPAddrBits: Int = 41,
  HasFPU: Boolean = true,
  HasVPU: Boolean = true,
  HasCustomCSRCacheOp: Boolean = true,
  FetchWidth: Int = 8,
  AsidLength: Int = 16,
  VmidLength: Int = 14,
  EnableBPU: Boolean = true,
  EnableBPD: Boolean = true,
  EnableRAS: Boolean = true,
  EnableLB: Boolean = false,
  EnableLoop: Boolean = true,
  EnableSC: Boolean = true,
  EnbaleTlbDebug: Boolean = false,
  EnableClockGate: Boolean = true,
  EnableJal: Boolean = false,
  EnableFauFTB: Boolean = true,
  UbtbGHRLength: Int = 4,
  // HistoryLength: Int = 512,
  EnableGHistDiff: Boolean = true,
  EnableCommitGHistDiff: Boolean = true,
  UbtbSize: Int = 256,
  FtbSize: Int = 2048,
  RasSize: Int = 16,
  RasSpecSize: Int = 32,
  RasCtrSize: Int = 3,
  CacheLineSize: Int = 512,
  FtbWays: Int = 4,
  TageTableInfos: Seq[Tuple3[Int,Int,Int]] =
  //       Sets  Hist   Tag
    // Seq(( 2048,    2,    8),
    //     ( 2048,    9,    8),
    //     ( 2048,   13,    8),
    //     ( 2048,   20,    8),
    //     ( 2048,   26,    8),
    //     ( 2048,   44,    8),
    //     ( 2048,   73,    8),
    //     ( 2048,  256,    8)),
    Seq(( 4096,    8,    8),
        ( 4096,   13,    8),
        ( 4096,   32,    8),
        ( 4096,  119,    8)),
  ITTageTableInfos: Seq[Tuple3[Int,Int,Int]] =
  //      Sets  Hist   Tag
    Seq(( 256,    4,    9),
        ( 256,    8,    9),
        ( 512,   13,    9),
        ( 512,   16,    9),
        ( 512,   32,    9)),
  SCNRows: Int = 512,
  SCNTables: Int = 4,
  SCCtrBits: Int = 6,
  SCHistLens: Seq[Int] = Seq(0, 4, 10, 16),
  numBr: Int = 2,
  branchPredictor: Function2[BranchPredictionResp, Parameters, Tuple2[Seq[BasePredictor], BranchPredictionResp]] =
    ((resp_in: BranchPredictionResp, p: Parameters) => {
      val ftb = Module(new FTB()(p))
      val ubtb =Module(new FauFTB()(p))
      // val bim = Module(new BIM()(p))
      val tage = Module(new Tage_SC()(p))
      val ras = Module(new RAS()(p))
      val ittage = Module(new ITTage()(p))
      val preds = Seq(ubtb, tage, ftb, ittage, ras)
      preds.map(_.io := DontCare)

      // ubtb.io.resp_in(0)  := resp_in
      // bim.io.resp_in(0)   := ubtb.io.resp
      // btb.io.resp_in(0)   := bim.io.resp
      // tage.io.resp_in(0)  := btb.io.resp
      // loop.io.resp_in(0)  := tage.io.resp
      ubtb.io.in.bits.resp_in(0) := resp_in
      tage.io.in.bits.resp_in(0) := ubtb.io.out
      ftb.io.in.bits.resp_in(0)  := tage.io.out
      ittage.io.in.bits.resp_in(0)  := ftb.io.out
      ras.io.in.bits.resp_in(0) := ittage.io.out

      (preds, ras.io.out)
    }),
  ICacheECCForceError: Boolean = false,
  IBufSize: Int = 48,
  IBufNBank: Int = 6, // IBuffer bank amount, should divide IBufSize
  DecodeWidth: Int = 6,
  RenameWidth: Int = 6,
  CommitWidth: Int = 8,
  RobCommitWidth: Int = 8,
  RabCommitWidth: Int = 6,
  MaxUopSize: Int = 65,
  EnableRenameSnapshot: Boolean = true,
  RenameSnapshotNum: Int = 4,
  FtqSize: Int = 64,
  EnableLoadFastWakeUp: Boolean = true, // NOTE: not supported now, make it false
  IntLogicRegs: Int = 32,
  FpLogicRegs: Int = 32 + 1 + 1, // 1: I2F, 1: stride
  VecLogicRegs: Int = 32 + 1 + 15, // 15: tmp, 1: vconfig
  VCONFIG_IDX: Int = 32,
  NRPhyRegs: Int = 192,
  VirtualLoadQueueSize: Int = 72,
  LoadQueueRARSize: Int = 72,
  LoadQueueRAWSize: Int = 64, // NOTE: make sure that LoadQueueRAWSize is power of 2.
  RollbackGroupSize: Int = 8,
  LoadQueueReplaySize: Int = 72,
  LoadUncacheBufferSize: Int = 20,
  LoadQueueNWriteBanks: Int = 8, // NOTE: make sure that LoadQueueRARSize/LoadQueueRAWSize is divided by LoadQueueNWriteBanks
  StoreQueueSize: Int = 64,
  StoreQueueNWriteBanks: Int = 8, // NOTE: make sure that StoreQueueSize is divided by StoreQueueNWriteBanks
  StoreQueueForwardWithMask: Boolean = true,
  VlsQueueSize: Int = 8,
  RobSize: Int = 160,
  RabSize: Int = 256,
  VTypeBufferSize: Int = 64, // used to reorder vtype
  IssueQueueSize: Int = 24,
  IssueQueueCompEntrySize: Int = 16,
  dpParams: DispatchParameters = DispatchParameters(
    IntDqSize = 16,
    FpDqSize = 16,
    LsDqSize = 18,
    IntDqDeqWidth = 8,
    FpDqDeqWidth = 6,
    LsDqDeqWidth = 6,
  ),
  intPreg: PregParams = IntPregParams(
    numEntries = 224,
    numRead = None,
    numWrite = None,
  ),
  vfPreg: VfPregParams = VfPregParams(
    numEntries = 192,
    numRead = None,
    numWrite = None,
  ),
  prefetcher: Option[PrefetcherParams] = Some(SMSParams()),
  LoadPipelineWidth: Int = 3,
  StorePipelineWidth: Int = 2,
  VecLoadPipelineWidth: Int = 2,
  VecStorePipelineWidth: Int = 2,
  VecMemSrcInWidth: Int = 2,
  VecMemInstWbWidth: Int = 1,
  VecMemDispatchWidth: Int = 1,
  StoreBufferSize: Int = 16,
  StoreBufferThreshold: Int = 7,
  EnsbufferWidth: Int = 2,
  LoadDependencyWidth: Int = 2,
  // ============ VLSU ============
  UsQueueSize: Int = 8,
  VlFlowSize: Int = 32,
  VlUopSize: Int = 32,
  VsFlowL1Size: Int = 128,
  VsFlowL2Size: Int = 32,
  VsUopSize: Int = 32,
  // ==============================
  UncacheBufferSize: Int = 4,
  EnableLoadToLoadForward: Boolean = false,
  EnableFastForward: Boolean = true,
  EnableLdVioCheckAfterReset: Boolean = true,
  EnableSoftPrefetchAfterReset: Boolean = true,
  EnableCacheErrorAfterReset: Boolean = true,
  EnableAccurateLoadError: Boolean = true,
  EnableUncacheWriteOutstanding: Boolean = false,
  EnableStorePrefetchAtIssue: Boolean = false,
  EnableStorePrefetchAtCommit: Boolean = false,
  EnableAtCommitMissTrigger: Boolean = true,
  EnableStorePrefetchSMS: Boolean = false,
  EnableStorePrefetchSPB: Boolean = false,
  MMUAsidLen: Int = 16, // max is 16, 0 is not supported now
  MMUVmidLen: Int = 14,
  ReSelectLen: Int = 7, // load replay queue replay select counter len
  iwpuParameters: WPUParameters = WPUParameters(
    enWPU = false,
    algoName = "mmru",
    isICache = true,
  ),
  dwpuParameters: WPUParameters = WPUParameters(
    enWPU = false,
    algoName = "mmru",
    enCfPred = false,
    isICache = false,
  ),
  itlbParameters: TLBParameters = TLBParameters(
    name = "itlb",
    fetchi = true,
    useDmode = false,
    NWays = 48,
  ),
  itlbPortNum: Int = 2 + ICacheParameters().prefetchPipeNum + 1,
  ipmpPortNum: Int = 2 + ICacheParameters().prefetchPipeNum + 1,
  ldtlbParameters: TLBParameters = TLBParameters(
    name = "ldtlb",
    NWays = 48,
    outReplace = false,
    partialStaticPMP = true,
    outsideRecvFlush = true,
    saveLevel = true
  ),
  sttlbParameters: TLBParameters = TLBParameters(
    name = "sttlb",
    NWays = 48,
    outReplace = false,
    partialStaticPMP = true,
    outsideRecvFlush = true,
    saveLevel = true
  ),
  hytlbParameters: TLBParameters = TLBParameters(
    name = "hytlb",
    NWays = 48,
    outReplace = false,
    partialStaticPMP = true,
    outsideRecvFlush = true,
    saveLevel = true
  ),
  pftlbParameters: TLBParameters = TLBParameters(
    name = "pftlb",
    NWays = 48,
    outReplace = false,
    partialStaticPMP = true,
    outsideRecvFlush = true,
    saveLevel = true
  ),
  l2ToL1tlbParameters: TLBParameters = TLBParameters(
    name = "l2tlb",
    NWays = 48,
    outReplace = false,
    partialStaticPMP = true,
    outsideRecvFlush = true,
    saveLevel = true
  ),
  refillBothTlb: Boolean = false,
  btlbParameters: TLBParameters = TLBParameters(
    name = "btlb",
    NWays = 48,
  ),
  l2tlbParameters: L2TLBParameters = L2TLBParameters(),
  NumPerfCounters: Int = 16,
  icacheParameters: ICacheParameters = ICacheParameters(
    tagECC = Some("parity"),
    dataECC = Some("parity"),
    replacer = Some("setplru"),
    nMissEntries = 2,
    nProbeEntries = 2,
    nPrefetchEntries = 12,
    nPrefBufferEntries = 32,
  ),
  dcacheParametersOpt: Option[DCacheParameters] = Some(DCacheParameters(
    tagECC = Some("secded"),
    dataECC = Some("secded"),
    replacer = Some("setplru"),
    nMissEntries = 16,
    nProbeEntries = 8,
    nReleaseEntries = 18,
    nMaxPrefetchEntry = 6,
  )),
  L2CacheParamsOpt: Option[L2Param] = Some(L2Param(
    name = "l2",
    ways = 8,
    sets = 1024, // default 512KB L2
    prefetch = Some(coupledL2.prefetch.PrefetchReceiverParams())
  )),
  L2NBanks: Int = 1,
  usePTWRepeater: Boolean = false,
  softTLB: Boolean = false, // dpi-c l1tlb debug only
  softPTW: Boolean = false, // dpi-c l2tlb debug only
  softPTWDelay: Int = 1
){
  def vlWidth = log2Up(VLEN) + 1

  val allHistLens = SCHistLens ++ ITTageTableInfos.map(_._2) ++ TageTableInfos.map(_._2) :+ UbtbGHRLength
  val HistoryLength = allHistLens.max + numBr * FtqSize + 9 // 256 for the predictor configs now

  val intSchdParams = {
    implicit val schdType: SchedulerType = IntScheduler()
    SchdBlockParams(Seq(
      IssueBlockParams(Seq(
        ExeUnitParams("ALU0", Seq(AluCfg, MulCfg, BkuCfg), Seq(IntWB(port = 0, 0)), Seq(Seq(IntRD(0, 0)), Seq(IntRD(1, 0))), true, 2),
        ExeUnitParams("BJU0", Seq(BrhCfg, JmpCfg), Seq(IntWB(port = 4, 0)), Seq(Seq(IntRD(8, 0)), Seq(IntRD(7, 1))), true, 2),
      ), numEntries = IssueQueueSize, numEnq = 2, numComp = IssueQueueCompEntrySize),
      IssueBlockParams(Seq(
        ExeUnitParams("ALU1", Seq(AluCfg, MulCfg, BkuCfg), Seq(IntWB(port = 1, 0)), Seq(Seq(IntRD(2, 0)), Seq(IntRD(3, 0))), true, 2),
        ExeUnitParams("BJU1", Seq(BrhCfg, JmpCfg), Seq(IntWB(port = 2, 1)), Seq(Seq(IntRD(9, 0)), Seq(IntRD(5, 1))), true, 2),
      ), numEntries = IssueQueueSize, numEnq = 2, numComp = IssueQueueCompEntrySize),
      IssueBlockParams(Seq(
        ExeUnitParams("ALU2", Seq(AluCfg), Seq(IntWB(port = 2, 0)), Seq(Seq(IntRD(4, 0)), Seq(IntRD(5, 0))), true, 2),
        ExeUnitParams("BJU2", Seq(BrhCfg, JmpCfg, I2fCfg, VSetRiWiCfg, VSetRiWvfCfg, CsrCfg, FenceCfg, I2vCfg), Seq(IntWB(port = 3, 1), VfWB(5, 1)), Seq(Seq(IntRD(10, 0)), Seq(IntRD(3, 1)))),
      ), numEntries = IssueQueueSize, numEnq = 2, numComp = IssueQueueCompEntrySize),
      IssueBlockParams(Seq(
        ExeUnitParams("ALU3", Seq(AluCfg), Seq(IntWB(port = 3, 0)), Seq(Seq(IntRD(6, 0)), Seq(IntRD(7, 0))), true, 2),
        ExeUnitParams("BJU3", Seq(DivCfg), Seq(IntWB(port = 4, 1)), Seq(Seq(IntRD(11, 0)), Seq(IntRD(1, 1)))),
      ), numEntries = IssueQueueSize, numEnq = 2, numComp = IssueQueueCompEntrySize),
    ),
      numPregs = intPreg.numEntries,
      numDeqOutside = 0,
      schdType = schdType,
      rfDataWidth = intPreg.dataCfg.dataWidth,
      numUopIn = dpParams.IntDqDeqWidth,
    )
  }
  val vfSchdParams = {
    implicit val schdType: SchedulerType = VfScheduler()
    SchdBlockParams(Seq(
      IssueBlockParams(Seq(
        ExeUnitParams("VFEX0", Seq(VfmaCfg, VialuCfg, VimacCfg, VppuCfg), Seq(VfWB(port = 4, 0)), Seq(Seq(VfRD(0, 0)), Seq(VfRD(1, 0)), Seq(VfRD(2, 0)), Seq(VfRD(3, 0)), Seq(VfRD(4, 0)))),
        ExeUnitParams("VFEX1", Seq(VfaluCfg, VfcvtCfg, VipuCfg, VSetRvfWvfCfg), Seq(VfWB(port = 5, 0), IntWB(port = 2, 2)), Seq(Seq(VfRD(5, 0)), Seq(VfRD(6, 0)), Seq(VfRD(7, 0)), Seq(VfRD(8, 0)), Seq(VfRD(9, 0)))),
      ), numEntries = IssueQueueSize, numEnq = 2, numComp = IssueQueueCompEntrySize),
      IssueBlockParams(Seq(
        ExeUnitParams("VFEX2", Seq(VfmaCfg, VialuCfg, F2vCfg), Seq(VfWB(port = 6, 0)), Seq(Seq(VfRD(7, 1)), Seq(VfRD(8, 1)), Seq(VfRD(9, 1)), Seq(VfRD(5, 1)), Seq(VfRD(6, 1)))),
        ExeUnitParams("VFEX3", Seq(VfaluCfg, VfcvtCfg), Seq(VfWB(port = 7, 0), IntWB(port = 3, 2)), Seq(Seq(VfRD(3, 1)), Seq(VfRD(4, 1)), Seq(VfRD(0, 1)), Seq(VfRD(1, 1)), Seq(VfRD(2, 1)))),
      ), numEntries = IssueQueueSize, numEnq = 2, numComp = IssueQueueCompEntrySize),
      IssueBlockParams(Seq(
        ExeUnitParams("VFEX4", Seq(VfdivCfg, VidivCfg), Seq(VfWB(port = 7, 1)), Seq(Seq(VfRD(3, 2)), Seq(VfRD(4, 2)), Seq(VfRD(0, 2)), Seq(VfRD(1, 2)), Seq(VfRD(2, 2)))),
        ExeUnitParams("VFEX5", Seq(VfdivCfg, VidivCfg), Seq(VfWB(port = 6, 1)), Seq(Seq(VfRD(8, 2)), Seq(VfRD(9, 2)), Seq(VfRD(5, 2)), Seq(VfRD(6, 2)), Seq(VfRD(7, 2)))),
      ), numEntries = IssueQueueSize, numEnq = 2, numComp = IssueQueueCompEntrySize),
    ),
      numPregs = vfPreg.numEntries,
      numDeqOutside = 0,
      schdType = schdType,
      rfDataWidth = vfPreg.dataCfg.dataWidth,
      numUopIn = dpParams.FpDqDeqWidth,
    )
  }

  val memSchdParams = {
    implicit val schdType: SchedulerType = MemScheduler()
    val rfDataWidth = 64

    SchdBlockParams(Seq(
      IssueBlockParams(Seq(
        ExeUnitParams("STA0", Seq(StaCfg, MouCfg), Seq(FakeIntWB()), Seq(Seq(IntRD(8, 1)))),
      ), numEntries = IssueQueueSize, numEnq = 2, numComp = IssueQueueCompEntrySize),
      IssueBlockParams(Seq(
        ExeUnitParams("STA1", Seq(StaCfg, MouCfg), Seq(FakeIntWB()), Seq(Seq(IntRD(9, 1)))),
      ), numEntries = IssueQueueSize, numEnq = 2, numComp = IssueQueueCompEntrySize),
      IssueBlockParams(Seq(
        ExeUnitParams("LDU0", Seq(LduCfg), Seq(IntWB(5, 0), VfWB(0, 0)), Seq(Seq(IntRD(12, 0))), true, 2),
      ), numEntries = IssueQueueSize, numEnq = 2, numComp = IssueQueueCompEntrySize),
      IssueBlockParams(Seq(
        ExeUnitParams("LDU1", Seq(LduCfg), Seq(IntWB(6, 0), VfWB(1, 0)), Seq(Seq(IntRD(13, 0))), true, 2),
      ), numEntries = IssueQueueSize, numEnq = 2, numComp = IssueQueueCompEntrySize),
      IssueBlockParams(Seq(
        ExeUnitParams("LDU2", Seq(LduCfg), Seq(IntWB(7, 0), VfWB(2, 0)), Seq(Seq(IntRD(14, 0))), true, 2),
      ), numEntries = IssueQueueSize, numEnq = 2, numComp = IssueQueueCompEntrySize),
      IssueBlockParams(Seq(
        ExeUnitParams("VLSU0", Seq(VlduCfg, VstuCfg), Seq(VfWB(3, 0)), Seq(Seq(VfRD(10, 0)), Seq(VfRD(11, 0)), Seq(VfRD(12, 0)), Seq(VfRD(13, 0)), Seq(VfRD(14, 0)))),
      ), numEntries = IssueQueueSize, numEnq = 2, numComp = IssueQueueCompEntrySize),
      IssueBlockParams(Seq(
        ExeUnitParams("STD0", Seq(StdCfg, MoudCfg), Seq(), Seq(Seq(IntRD(10, 1), VfRD(12, Int.MaxValue)))),
      ), numEntries = IssueQueueSize, numEnq = 2, numComp = IssueQueueCompEntrySize),
      IssueBlockParams(Seq(
        ExeUnitParams("STD1", Seq(StdCfg, MoudCfg), Seq(), Seq(Seq(IntRD(11, 1), VfRD(13, Int.MaxValue)))),
      ), numEntries = IssueQueueSize, numEnq = 2, numComp = IssueQueueCompEntrySize),
    ),
      numPregs = intPreg.numEntries max vfPreg.numEntries,
      numDeqOutside = 0,
      schdType = schdType,
      rfDataWidth = rfDataWidth,
      numUopIn = dpParams.LsDqDeqWidth,
    )
  }

  def PregIdxWidthMax = intPreg.addrWidth max vfPreg.addrWidth

  def iqWakeUpParams = {
    Seq(
      WakeUpConfig(
        Seq("ALU0", "ALU1", "ALU2", "ALU3", "LDU0", "LDU1", "LDU2") ->
        Seq("ALU0", "BJU0", "ALU1", "BJU1", "ALU2", "BJU2", "ALU3", "BJU3", "LDU0", "LDU1", "LDU2", "STA0", "STA1", "STD0", "STD1")
      ),
      WakeUpConfig(
        Seq("VFEX0", "VFEX1", "VFEX2", "VFEX3", "LDU0", "LDU1", "LDU2") ->
        Seq("VFEX0", "VFEX1", "VFEX2", "VFEX3", "VFEX4", "VFEX5")
      ),
      WakeUpConfig(
        Seq("VFEX0", "VFEX1", "VFEX2", "VFEX3") ->
        Seq("STD0", "STD1")
      ),
    ).flatten
  }

  def fakeIntPreg = FakeIntPregParams(intPreg.numEntries, intPreg.numRead, intPreg.numWrite)

  val backendParams: BackendParams = backend.BackendParams(
    Map(
      IntScheduler() -> intSchdParams,
      VfScheduler() -> vfSchdParams,
      MemScheduler() -> memSchdParams,
    ),
    Seq(
      intPreg,
      vfPreg,
      fakeIntPreg
    ),
    iqWakeUpParams,
  )
}

case object DebugOptionsKey extends Field[DebugOptions]

case class DebugOptions
(
  FPGAPlatform: Boolean = false,
  EnableDifftest: Boolean = false,
  AlwaysBasicDiff: Boolean = true,
  EnableDebug: Boolean = false,
  EnablePerfDebug: Boolean = true,
  UseDRAMSim: Boolean = false,
  EnableConstantin: Boolean = false,
  EnableChiselDB: Boolean = false,
  AlwaysBasicDB: Boolean = true,
  EnableTopDown: Boolean = false,
  EnableRollingDB: Boolean = false
)

trait HasXSParameter {

  implicit val p: Parameters

  val PAddrBits = p(SoCParamsKey).PAddrBits // PAddrBits is Phyical Memory addr bits

  val coreParams = p(XSCoreParamsKey)
  val env = p(DebugOptionsKey)

  val XLEN = coreParams.XLEN
  val VLEN = coreParams.VLEN
  val ELEN = coreParams.ELEN
  val HSXLEN = coreParams.HSXLEN
  val minFLen = 32
  val fLen = 64
  val hartIdLen = p(MaxHartIdBits)
  def xLen = XLEN

  val HasMExtension = coreParams.HasMExtension
  val HasCExtension = coreParams.HasCExtension
  val HasHExtension = coreParams.HasHExtension
  val HasDiv = coreParams.HasDiv
  val HasIcache = coreParams.HasICache
  val HasDcache = coreParams.HasDCache
  val AddrBits = coreParams.AddrBits // AddrBits is used in some cases
  val GPAddrBits = coreParams.GPAddrBits
  val VAddrBits = {
    if(HasHExtension){
      coreParams.GPAddrBits
    }else{
      coreParams.VAddrBits
    }
  } // VAddrBits is Virtual Memory addr bits

  val AsidLength = coreParams.AsidLength
  val VmidLength = coreParams.VmidLength
  val ReSelectLen = coreParams.ReSelectLen
  val AddrBytes = AddrBits / 8 // unused
  val DataBits = XLEN
  val DataBytes = DataBits / 8
  val VDataBytes = VLEN / 8
  val HasFPU = coreParams.HasFPU
  val HasVPU = coreParams.HasVPU
  val HasCustomCSRCacheOp = coreParams.HasCustomCSRCacheOp
  val FetchWidth = coreParams.FetchWidth
  val PredictWidth = FetchWidth * (if (HasCExtension) 2 else 1)
  val EnableBPU = coreParams.EnableBPU
  val EnableBPD = coreParams.EnableBPD // enable backing predictor(like Tage) in BPUStage3
  val EnableRAS = coreParams.EnableRAS
  val EnableLB = coreParams.EnableLB
  val EnableLoop = coreParams.EnableLoop
  val EnableSC = coreParams.EnableSC
  val EnbaleTlbDebug = coreParams.EnbaleTlbDebug
  val HistoryLength = coreParams.HistoryLength
  val EnableGHistDiff = coreParams.EnableGHistDiff
  val EnableCommitGHistDiff = coreParams.EnableCommitGHistDiff
  val EnableClockGate = coreParams.EnableClockGate
  val UbtbGHRLength = coreParams.UbtbGHRLength
  val UbtbSize = coreParams.UbtbSize
  val EnableFauFTB = coreParams.EnableFauFTB
  val FtbSize = coreParams.FtbSize
  val FtbWays = coreParams.FtbWays
  val RasSize = coreParams.RasSize
  val RasSpecSize = coreParams.RasSpecSize
  val RasCtrSize = coreParams.RasCtrSize

  def getBPDComponents(resp_in: BranchPredictionResp, p: Parameters) = {
    coreParams.branchPredictor(resp_in, p)
  }
  val numBr = coreParams.numBr
  val TageTableInfos = coreParams.TageTableInfos
  val TageBanks = coreParams.numBr
  val SCNRows = coreParams.SCNRows
  val SCCtrBits = coreParams.SCCtrBits
  val SCHistLens = coreParams.SCHistLens
  val SCNTables = coreParams.SCNTables

  val SCTableInfos = Seq.fill(SCNTables)((SCNRows, SCCtrBits)) zip SCHistLens map {
    case ((n, cb), h) => (n, cb, h)
  }
  val ITTageTableInfos = coreParams.ITTageTableInfos
  type FoldedHistoryInfo = Tuple2[Int, Int]
  val foldedGHistInfos =
    (TageTableInfos.map{ case (nRows, h, t) =>
      if (h > 0)
        Set((h, min(log2Ceil(nRows/numBr), h)), (h, min(h, t)), (h, min(h, t-1)))
      else
        Set[FoldedHistoryInfo]()
    }.reduce(_++_).toSet ++
    SCTableInfos.map{ case (nRows, _, h) =>
      if (h > 0)
        Set((h, min(log2Ceil(nRows/TageBanks), h)))
      else
        Set[FoldedHistoryInfo]()
    }.reduce(_++_).toSet ++
    ITTageTableInfos.map{ case (nRows, h, t) =>
      if (h > 0)
        Set((h, min(log2Ceil(nRows), h)), (h, min(h, t)), (h, min(h, t-1)))
      else
        Set[FoldedHistoryInfo]()
    }.reduce(_++_) ++
      Set[FoldedHistoryInfo]((UbtbGHRLength, log2Ceil(UbtbSize)))
    ).toList



  val CacheLineSize = coreParams.CacheLineSize
  val CacheLineHalfWord = CacheLineSize / 16
  val ExtHistoryLength = HistoryLength + 64
  val ICacheECCForceError = coreParams.ICacheECCForceError
  val IBufSize = coreParams.IBufSize
  val IBufNBank = coreParams.IBufNBank
  val backendParams: BackendParams = coreParams.backendParams
  val DecodeWidth = coreParams.DecodeWidth
  val RenameWidth = coreParams.RenameWidth
  val CommitWidth = coreParams.CommitWidth
  val RobCommitWidth = coreParams.RobCommitWidth
  val RabCommitWidth = coreParams.RabCommitWidth
  val MaxUopSize = coreParams.MaxUopSize
  val EnableRenameSnapshot = coreParams.EnableRenameSnapshot
  val RenameSnapshotNum = coreParams.RenameSnapshotNum
  val FtqSize = coreParams.FtqSize
  val EnableLoadFastWakeUp = coreParams.EnableLoadFastWakeUp
  val IntLogicRegs = coreParams.IntLogicRegs
  val FpLogicRegs = coreParams.FpLogicRegs
  val VecLogicRegs = coreParams.VecLogicRegs
  val VCONFIG_IDX = coreParams.VCONFIG_IDX
  val IntPhyRegs = coreParams.intPreg.numEntries
  val VfPhyRegs = coreParams.vfPreg.numEntries
  val MaxPhyPregs = IntPhyRegs max VfPhyRegs
  val PhyRegIdxWidth = log2Up(IntPhyRegs) max log2Up(VfPhyRegs)
  val RobSize = coreParams.RobSize
  val RabSize = coreParams.RabSize
  val VTypeBufferSize = coreParams.VTypeBufferSize
  val IntRefCounterWidth = log2Ceil(RobSize)
  val LSQEnqWidth = coreParams.dpParams.LsDqDeqWidth
  val LSQLdEnqWidth = LSQEnqWidth min backendParams.numLoadDp
  val LSQStEnqWidth = LSQEnqWidth min backendParams.numStoreDp
  val VirtualLoadQueueSize = coreParams.VirtualLoadQueueSize
  val LoadQueueRARSize = coreParams.LoadQueueRARSize
  val LoadQueueRAWSize = coreParams.LoadQueueRAWSize
  val RollbackGroupSize = coreParams.RollbackGroupSize
  val LoadQueueReplaySize = coreParams.LoadQueueReplaySize
  val LoadUncacheBufferSize = coreParams.LoadUncacheBufferSize
  val LoadQueueNWriteBanks = coreParams.LoadQueueNWriteBanks
  val StoreQueueSize = coreParams.StoreQueueSize
  val StoreQueueNWriteBanks = coreParams.StoreQueueNWriteBanks
  val StoreQueueForwardWithMask = coreParams.StoreQueueForwardWithMask
  val VlsQueueSize = coreParams.VlsQueueSize
  val dpParams = coreParams.dpParams

  def MemIQSizeMax = backendParams.memSchdParams.get.issueBlockParams.map(_.numEntries).max
  def IQSizeMax = backendParams.allSchdParams.map(_.issueBlockParams.map(_.numEntries).max).max

  val NumRedirect = backendParams.numRedirect
  val BackendRedirectNum = NumRedirect + 2 //2: ldReplay + Exception
  val FtqRedirectAheadNum = NumRedirect
  val LoadPipelineWidth = coreParams.LoadPipelineWidth
  val StorePipelineWidth = coreParams.StorePipelineWidth
  val VecLoadPipelineWidth = coreParams.VecLoadPipelineWidth
  val VecStorePipelineWidth = coreParams.VecStorePipelineWidth
  val VecMemSrcInWidth = coreParams.VecMemSrcInWidth
  val VecMemInstWbWidth = coreParams.VecMemInstWbWidth
  val VecMemDispatchWidth = coreParams.VecMemDispatchWidth
  val StoreBufferSize = coreParams.StoreBufferSize
  val StoreBufferThreshold = coreParams.StoreBufferThreshold
  val EnsbufferWidth = coreParams.EnsbufferWidth
  val LoadDependencyWidth = coreParams.LoadDependencyWidth
  val UsQueueSize = coreParams.UsQueueSize
  val VlFlowSize = coreParams.VlFlowSize
  val VlUopSize = coreParams.VlUopSize
  val VsFlowL1Size = coreParams.VsFlowL1Size
  val VsFlowL2Size = coreParams.VsFlowL2Size
  val VsUopSize = coreParams.VsUopSize
  val UncacheBufferSize = coreParams.UncacheBufferSize
  val EnableLoadToLoadForward = coreParams.EnableLoadToLoadForward
  val EnableFastForward = coreParams.EnableFastForward
  val EnableLdVioCheckAfterReset = coreParams.EnableLdVioCheckAfterReset
  val EnableSoftPrefetchAfterReset = coreParams.EnableSoftPrefetchAfterReset
  val EnableCacheErrorAfterReset = coreParams.EnableCacheErrorAfterReset
  val EnableAccurateLoadError = coreParams.EnableAccurateLoadError
  val EnableUncacheWriteOutstanding = coreParams.EnableUncacheWriteOutstanding
  val EnableStorePrefetchAtIssue = coreParams.EnableStorePrefetchAtIssue
  val EnableStorePrefetchAtCommit = coreParams.EnableStorePrefetchAtCommit
  val EnableAtCommitMissTrigger = coreParams.EnableAtCommitMissTrigger
  val EnableStorePrefetchSMS = coreParams.EnableStorePrefetchSMS
  val EnableStorePrefetchSPB = coreParams.EnableStorePrefetchSPB
  require(LoadPipelineWidth == backendParams.LdExuCnt, "LoadPipelineWidth must be equal exuParameters.LduCnt!")
  require(StorePipelineWidth == backendParams.StaCnt, "StorePipelineWidth must be equal exuParameters.StuCnt!")
  val Enable3Load3Store = (LoadPipelineWidth == 3 && StorePipelineWidth == 3)
  val asidLen = coreParams.MMUAsidLen
  val vmidLen = coreParams.MMUVmidLen
  val BTLBWidth = coreParams.LoadPipelineWidth + coreParams.StorePipelineWidth
  val refillBothTlb = coreParams.refillBothTlb
  val iwpuParam = coreParams.iwpuParameters
  val dwpuParam = coreParams.dwpuParameters
  val itlbParams = coreParams.itlbParameters
  val ldtlbParams = coreParams.ldtlbParameters
  val sttlbParams = coreParams.sttlbParameters
  val hytlbParams = coreParams.hytlbParameters
  val pftlbParams = coreParams.pftlbParameters
  val l2ToL1Params = coreParams.l2ToL1tlbParameters
  val btlbParams = coreParams.btlbParameters
  val l2tlbParams = coreParams.l2tlbParameters
  val NumPerfCounters = coreParams.NumPerfCounters

  val instBytes = if (HasCExtension) 2 else 4
  val instOffsetBits = log2Ceil(instBytes)

  val icacheParameters = coreParams.icacheParameters
  val dcacheParameters = coreParams.dcacheParametersOpt.getOrElse(DCacheParameters())

  // dcache block cacheline when lr for LRSCCycles - LRSCBackOff cycles
  // for constrained LR/SC loop
  val LRSCCycles = 64
  // for lr storm
  val LRSCBackOff = 8

  // cache hierarchy configurations
  val l1BusDataWidth = 256

  // load violation predict
  val ResetTimeMax2Pow = 20 //1078576
  val ResetTimeMin2Pow = 10 //1024
  // wait table parameters
  val WaitTableSize = 1024
  val MemPredPCWidth = log2Up(WaitTableSize)
  val LWTUse2BitCounter = true
  // store set parameters
  val SSITSize = WaitTableSize
  val LFSTSize = 32
  val SSIDWidth = log2Up(LFSTSize)
  val LFSTWidth = 4
  val StoreSetEnable = true // LWT will be disabled if SS is enabled
  val LFSTEnable = true

  val PCntIncrStep: Int = 6
  val numPCntHc: Int = 25
  val numPCntPtw: Int = 19

  val numCSRPCntFrontend = 8
  val numCSRPCntCtrl     = 8
  val numCSRPCntLsu      = 8
  val numCSRPCntHc       = 5
  val printEventCoding   = true

  // Parameters for Sdtrig extension
  protected val TriggerNum = 4
  protected val TriggerChainMaxLength = 2
}
