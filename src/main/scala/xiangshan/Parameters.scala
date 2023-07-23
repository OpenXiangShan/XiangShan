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

import chipsalliance.rocketchip.config.{Field, Parameters}
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
import xiangshan.backend.regfile.{IntPregParams, PregParams, VfPregParams}
import xiangshan.backend.BackendParams
import xiangshan.cache.DCacheParameters
import xiangshan.cache.prefetch._
import xiangshan.frontend.{BasePredictor, BranchPredictionResp, FTB, FakePredictor, RAS, Tage, ITTage, Tage_SC, FauFTB}
import xiangshan.frontend.icache.ICacheParameters
import xiangshan.cache.mmu.{L2TLBParameters, TLBParameters}
import xiangshan.frontend._
import xiangshan.frontend.icache.ICacheParameters

import freechips.rocketchip.diplomacy.AddressSet
import system.SoCParamsKey
import huancun._
import huancun.debug._
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
  HasMExtension: Boolean = true,
  HasCExtension: Boolean = true,
  HasDiv: Boolean = true,
  HasICache: Boolean = true,
  HasDCache: Boolean = true,
  AddrBits: Int = 64,
  VAddrBits: Int = 39,
  HasFPU: Boolean = true,
  HasVPU: Boolean = true,
  HasCustomCSRCacheOp: Boolean = true,
  FetchWidth: Int = 8,
  AsidLength: Int = 16,
  EnableBPU: Boolean = true,
  EnableBPD: Boolean = true,
  EnableRAS: Boolean = true,
  EnableLB: Boolean = false,
  EnableLoop: Boolean = true,
  EnableSC: Boolean = true,
  EnbaleTlbDebug: Boolean = false,
  EnableJal: Boolean = false,
  EnableFauFTB: Boolean = true,
  UbtbGHRLength: Int = 4,
  // HistoryLength: Int = 512,
  EnableGHistDiff: Boolean = true,
  EnableCommitGHistDiff: Boolean = true,
  UbtbSize: Int = 256,
  FtbSize: Int = 2048,
  RasSize: Int = 32,
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
  IBufSize: Int = 48,
  DecodeWidth: Int = 6,
  RenameWidth: Int = 6,
  CommitWidth: Int = 6,
  MaxUopSize: Int = 65,
  FtqSize: Int = 64,
  EnableLoadFastWakeUp: Boolean = true, // NOTE: not supported now, make it false
  IntLogicRegs: Int = 32,
  FpLogicRegs: Int = 33,
  VecLogicRegs: Int = 32 + 1 + 15, // 15: tmp, 1: vconfig
  VCONFIG_IDX: Int = 32,
  NRPhyRegs: Int = 192,
  IntPhyRegs: Int = 192,
  VfPhyRegs: Int = 192,
  VirtualLoadQueueSize: Int = 80,
  LoadQueueRARSize: Int = 80,
  LoadQueueRAWSize: Int = 64, // NOTE: make sure that LoadQueueRAWSize is power of 2.
  RollbackGroupSize: Int = 8,
  LoadQueueReplaySize: Int = 80,
  LoadUncacheBufferSize: Int = 20,
  LoadQueueNWriteBanks: Int = 8, // NOTE: make sure that LoadQueueRARSize/LoadQueueRAWSize is divided by LoadQueueNWriteBanks
  StoreQueueSize: Int = 64,
  StoreQueueNWriteBanks: Int = 8, // NOTE: make sure that StoreQueueSize is divided by StoreQueueNWriteBanks
  StoreQueueForwardWithMask: Boolean = true,
  VlsQueueSize: Int = 8,
  RobSize: Int = 256,
  RabSize: Int = 256,
  dpParams: DispatchParameters = DispatchParameters(
    IntDqSize = 16,
    FpDqSize = 16,
    LsDqSize = 16,
    IntDqDeqWidth = 6,
    FpDqDeqWidth = 6,
    LsDqDeqWidth = 6,
  ),
  intPreg: PregParams = IntPregParams(
    numEntries = 256,
    numRead = 14,
    numWrite = 8,
  ),
  vfPreg: VfPregParams = VfPregParams(
    numEntries = 256,
    numRead = 14,
    numWrite = 8,
  ),
  prefetcher: Option[PrefetcherParams] = Some(SMSParams()),
  LoadPipelineWidth: Int = 2,
  StorePipelineWidth: Int = 2,
  VecMemSrcInWidth: Int = 2,
  VecMemInstWbWidth: Int = 1,
  VecMemDispatchWidth: Int = 1,
  StoreBufferSize: Int = 16,
  StoreBufferThreshold: Int = 7,
  EnsbufferWidth: Int = 2,
  UncacheBufferSize: Int = 4,
  EnableLoadToLoadForward: Boolean = true,
  EnableFastForward: Boolean = false,
  EnableLdVioCheckAfterReset: Boolean = true,
  EnableSoftPrefetchAfterReset: Boolean = true,
  EnableCacheErrorAfterReset: Boolean = true,
  EnableDCacheWPU: Boolean = false,
  EnableAccurateLoadError: Boolean = true,
  EnableUncacheWriteOutstanding: Boolean = false,
  MMUAsidLen: Int = 16, // max is 16, 0 is not supported now
  ReSelectLen: Int = 7, // load replay queue replay select counter len
  itlbParameters: TLBParameters = TLBParameters(
    name = "itlb",
    fetchi = true,
    useDmode = false,
    normalNWays = 32,
    normalReplacer = Some("plru"),
    superNWays = 4,
    superReplacer = Some("plru")
  ),
  itlbPortNum: Int = 2 + ICacheParameters().prefetchPipeNum + 1,
  ipmpPortNum: Int = 2 + ICacheParameters().prefetchPipeNum + 1,
  ldtlbParameters: TLBParameters = TLBParameters(
    name = "ldtlb",
    normalNSets = 64,
    normalNWays = 1,
    normalAssociative = "sa",
    normalReplacer = Some("setplru"),
    superNWays = 16,
    normalAsVictim = true,
    outReplace = false,
    partialStaticPMP = true,
    outsideRecvFlush = true,
    saveLevel = true
  ),
  sttlbParameters: TLBParameters = TLBParameters(
    name = "sttlb",
    normalNSets = 64,
    normalNWays = 1,
    normalAssociative = "sa",
    normalReplacer = Some("setplru"),
    superNWays = 16,
    normalAsVictim = true,
    outReplace = false,
    partialStaticPMP = true,
    outsideRecvFlush = true,
    saveLevel = true
  ),
  pftlbParameters: TLBParameters = TLBParameters(
    name = "pftlb",
    normalNSets = 64,
    normalNWays = 1,
    normalAssociative = "sa",
    normalReplacer = Some("setplru"),
    superNWays = 16,
    normalAsVictim = true,
    outReplace = false,
    partialStaticPMP = true,
    outsideRecvFlush = true,
    saveLevel = true
  ),
  refillBothTlb: Boolean = false,
  btlbParameters: TLBParameters = TLBParameters(
    name = "btlb",
    normalNSets = 1,
    normalNWays = 64,
    superNWays = 4,
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
    nPrefBufferEntries = 64,
    hasPrefetch = true,
  ),
  dcacheParametersOpt: Option[DCacheParameters] = Some(DCacheParameters(
    tagECC = Some("secded"),
    dataECC = Some("secded"),
    replacer = Some("setplru"),
    nMissEntries = 16,
    nProbeEntries = 8,
    nReleaseEntries = 18
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

  def intSchdParams = {
    implicit val schdType: SchedulerType = IntScheduler()
    val pregBits = intPreg.addrWidth
    val numRfRead = intPreg.numRead
    val numRfWrite = intPreg.numWrite
    SchdBlockParams(Seq(
      IssueBlockParams(Seq(
        ExeUnitParams("IEX0", Seq(AluCfg, MulCfg, BkuCfg), Seq(IntWB(port = 0, 0)), Seq(Seq(IntRD(0, 0)), Seq(IntRD(1, 0)))),
        ExeUnitParams("IEX1", Seq(AluCfg, MulCfg, BkuCfg), Seq(IntWB(port = 1, 1)), Seq(Seq(IntRD(2, 1)), Seq(IntRD(3, 1)))),
      ), numEntries = 8, pregBits = pregBits, numWakeupFromWB = numRfWrite, numEnq = 2),
      IssueBlockParams(Seq(
        ExeUnitParams("BJU0", Seq(BrhCfg, JmpCfg, CsrCfg, FenceCfg), Seq(IntWB(port = 2, 0)), Seq(Seq(IntRD(4, 0)), Seq(IntRD(5, 0)))),
        ExeUnitParams("BJU1", Seq(BrhCfg), Seq(), Seq(Seq(IntRD(6, 0)), Seq(IntRD(7, 0)))),
      ), numEntries = 8, pregBits = pregBits, numWakeupFromWB = numRfWrite, numEnq = 2),
      IssueBlockParams(Seq(
        ExeUnitParams("IMISC0", Seq(VSetRiWiCfg), Seq(IntWB(port = 3, 0)), Seq(Seq(IntRD(6, 2)), Seq(IntRD(7, 2)))),
      ), numEntries = 8, pregBits = pregBits, numWakeupFromWB = numRfWrite, numEnq = 2),
      IssueBlockParams(Seq(
        ExeUnitParams("IMISC1", Seq(I2fCfg, VSetRiWvfCfg), Seq(VfWB(port = 4, 0)), Seq(Seq(IntRD(6, 1)), Seq(IntRD(7, 1)))),
      ), numEntries = 8, pregBits = pregBits, numWakeupFromWB = numRfWrite, numEnq = 2),
      IssueBlockParams(Seq(
        ExeUnitParams("IDIV0", Seq(DivCfg), Seq(IntWB(port = 4, 0)), Seq(Seq(IntRD(8, 0)), Seq(IntRD(9, 0)))),
      ), numEntries = 8, pregBits = pregBits, numWakeupFromWB = numRfWrite, numEnq = 2),
    ),
      numPregs = intPreg.numEntries,
      numRfReadWrite = Some((numRfRead, numRfWrite)),
      numDeqOutside = 0,
      schdType = schdType,
      rfDataWidth = intPreg.dataCfg.dataWidth,
      numUopIn = dpParams.IntDqDeqWidth,
    )
  }
  def vfSchdParams = {
    implicit val schdType: SchedulerType = VfScheduler()
    val pregBits = vfPreg.addrWidth
    val numRfRead = vfPreg.numRead
    val numRfWrite = vfPreg.numWrite
    SchdBlockParams(Seq(
//      IssueBlockParams(Seq(
//        ExeUnitParams("VEX0", Seq(VialuCfg), Seq(VfWB(port = 0, 0)), Seq(Seq(VfRD(1, 0)), Seq(VfRD(2, 0)), Seq(VfRD(3, 0)), Seq(VfRD(4, 0)), Seq(VfRD(5, 0)))),
//        ExeUnitParams("VEX1", Seq(VimacCfg), Seq(VfWB(port = 0, 0)), Seq(Seq(VfRD(1, 0)), Seq(VfRD(2, 0)), Seq(VfRD(3, 0)), Seq(VfRD(4, 0)), Seq(VfRD(5, 0)))),
//      ), numEntries = 8, pregBits = pregBits, numWakeupFromWB = numRfWrite, numEnq = 2),
      IssueBlockParams(Seq(
        ExeUnitParams("FEX0", Seq(FmacCfg), Seq(VfWB(port = 0, 0)), Seq(Seq(VfRD(1, 0)), Seq(VfRD(2, 0)), Seq(VfRD(3, 0)))),
        ExeUnitParams("FEX1", Seq(FmacCfg), Seq(VfWB(port = 1, 0)), Seq(Seq(VfRD(4, 0)), Seq(VfRD(5, 0)), Seq(VfRD(6, 0)))),
      ), numEntries = 8, pregBits = pregBits, numWakeupFromWB = numRfWrite, numEnq = 2),
      IssueBlockParams(Seq(
        ExeUnitParams("FEX2", Seq(FDivSqrtCfg), Seq(VfWB(port = 2, 0)), Seq(Seq(VfRD(11, 0)), Seq(VfRD(12, 0)))),
        ExeUnitParams("FEX3", Seq(F2fCfg, F2iCfg, VSetRvfWvfCfg), Seq(VfWB(port = 3, 0), IntWB(port = 5, 0)), Seq(Seq(VfRD(7, 0)), Seq(VfRD(8, 0)))),
      ), numEntries = 8, pregBits = pregBits, numWakeupFromWB = numRfWrite, numEnq = 2),
      IssueBlockParams(Seq(
        ExeUnitParams("VEX8", Seq(VfdivCfg), Seq(VfWB(port = 4, 0)), Seq(Seq(VfRD(1, 0)), Seq(VfRD(2, 0)), Seq(VfRD(3, 0)), Seq(VfRD(4, 0)), Seq(VfRD(5, 0)))),
      ), numEntries = 8, pregBits = pregBits, numWakeupFromWB = numRfWrite, numEnq = 2),
    ),
      numPregs = vfPreg.numEntries,
      numRfReadWrite = Some((numRfRead, numRfWrite)),
      numDeqOutside = 0,
      schdType = schdType,
      rfDataWidth = vfPreg.dataCfg.dataWidth,
      numUopIn = dpParams.FpDqDeqWidth,
    )
  }
  def memSchdParams = {
    implicit val schdType: SchedulerType = MemScheduler()
    val pregBits = vfPreg.addrWidth max intPreg.addrWidth
    val rfDataWidth = 64

    SchdBlockParams(Seq(
      IssueBlockParams(Seq(
        ExeUnitParams("LDU0", Seq(LduCfg), Seq(IntWB(6, 0), VfWB(6, 0)), Seq(Seq(IntRD(10, 0)))),
        ExeUnitParams("LDU1", Seq(LduCfg), Seq(IntWB(7, 0), VfWB(7, 0)), Seq(Seq(IntRD(11, 0)))),
      ), numEntries = 8, pregBits = pregBits, numWakeupFromWB = 16, numEnq = 2),
      IssueBlockParams(Seq(
        ExeUnitParams("STA0", Seq(StaCfg, MouCfg), Seq(IntWB(6, 1)), Seq(Seq(IntRD(12, 0)))),
        ExeUnitParams("STA1", Seq(StaCfg, MouCfg), Seq(IntWB(7, 1)), Seq(Seq(IntRD(13, 0)))),
      ), numEntries = 8, pregBits = pregBits, numWakeupFromWB = 16, numEnq = 2),
      IssueBlockParams(Seq(
        ExeUnitParams("STD0", Seq(StdCfg, MoudCfg), Seq(), Seq(Seq(IntRD(8, Int.MaxValue), VfRD(12, Int.MaxValue)))),
        ExeUnitParams("STD1", Seq(StdCfg, MoudCfg), Seq(), Seq(Seq(IntRD(9, Int.MaxValue), VfRD(13, Int.MaxValue)))),
      ), numEntries = 8, pregBits = pregBits, numWakeupFromWB = 16, numEnq = 2),
      IssueBlockParams(Seq(
        ExeUnitParams("VLDU0", Seq(VlduCfg), Seq(VfWB(5, 1)), Seq(Seq(VfRD(0, 0)), Seq(VfRD(1, 0)), Seq(VfRD(2, 0)), Seq(VfRD(3, 0)), Seq(VfRD(4, 0)))),
        ExeUnitParams("VLDU1", Seq(VlduCfg), Seq(VfWB(6, 1)), Seq(Seq(VfRD(5, 0)), Seq(VfRD(6, 0)), Seq(VfRD(7, 0)), Seq(VfRD(8, 0)), Seq(VfRD(9, 0)))),
      ), numEntries = 8, pregBits = pregBits, numWakeupFromWB = 16, numEnq = 2),
    ),
      numPregs = intPreg.numEntries max vfPreg.numEntries,
      numRfReadWrite = None,
      numDeqOutside = 0,
      schdType = schdType,
      rfDataWidth = rfDataWidth,
      numUopIn = dpParams.LsDqDeqWidth,
    )
  }

  def PregIdxWidthMax = intPreg.addrWidth max vfPreg.addrWidth

  def iqWakeUpParams = {
    Seq(
      WakeUpConfig("IEX0" -> "IEX0"),
      WakeUpConfig("IEX0" -> "IEX1"),
      WakeUpConfig("IEX1" -> "IEX0"),
      WakeUpConfig("IEX1" -> "IEX1"),
      WakeUpConfig("IEX0" -> "BJU0"),
      WakeUpConfig("IEX0" -> "BJU1"),
      WakeUpConfig("IEX1" -> "BJU0"),
      WakeUpConfig("IEX1" -> "BJU1"),
      WakeUpConfig("IEX0" -> "LDU0"),
      WakeUpConfig("IEX0" -> "LDU1"),
      WakeUpConfig("IEX1" -> "LDU0"),
      WakeUpConfig("IEX1" -> "LDU1"),
      WakeUpConfig("IEX0" -> "STA0"),
      WakeUpConfig("IEX0" -> "STA1"),
      WakeUpConfig("IEX1" -> "STA0"),
      WakeUpConfig("IEX1" -> "STA1"),
      WakeUpConfig("IMISC1" -> "FEX0"),
      WakeUpConfig("IMISC1" -> "FEX1"),
      WakeUpConfig("IMISC1" -> "FEX2"),
      WakeUpConfig("IMISC1" -> "FEX3"),
      WakeUpConfig("IMISC1" -> "FEX4"),
      WakeUpConfig("FEX3" -> "FEX0"),
      WakeUpConfig("FEX3" -> "FEX1"),
      WakeUpConfig("FEX3" -> "FEX2"),
      WakeUpConfig("FEX3" -> "FEX3"),
      WakeUpConfig("FEX3" -> "IEX0"),
      WakeUpConfig("FEX3" -> "IEX1"),
      WakeUpConfig("FEX3" -> "BJU0"),
      WakeUpConfig("FEX3" -> "BJU1"),
    )
  }

  def backendParams: BackendParams = backend.BackendParams(
    Map(
      IntScheduler() -> intSchdParams,
      VfScheduler() -> vfSchdParams,
      MemScheduler() -> memSchdParams,
    ),
    Seq(
      intPreg,
      vfPreg,
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
  EnableTopDown: Boolean = false
)

trait HasXSParameter {

  implicit val p: Parameters

  val PAddrBits = p(SoCParamsKey).PAddrBits // PAddrBits is Phyical Memory addr bits

  val coreParams = p(XSCoreParamsKey)
  val env = p(DebugOptionsKey)

  val XLEN = coreParams.XLEN
  val VLEN = coreParams.VLEN
  val ELEN = coreParams.ELEN
  val minFLen = 32
  val fLen = 64
  def xLen = XLEN

  val HasMExtension = coreParams.HasMExtension
  val HasCExtension = coreParams.HasCExtension
  val HasDiv = coreParams.HasDiv
  val HasIcache = coreParams.HasICache
  val HasDcache = coreParams.HasDCache
  val AddrBits = coreParams.AddrBits // AddrBits is used in some cases
  val VAddrBits = coreParams.VAddrBits // VAddrBits is Virtual Memory addr bits
  val AsidLength = coreParams.AsidLength
  val ReSelectLen = coreParams.ReSelectLen
  val AddrBytes = AddrBits / 8 // unused
  val DataBits = XLEN
  val DataBytes = DataBits / 8
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
  val UbtbGHRLength = coreParams.UbtbGHRLength
  val UbtbSize = coreParams.UbtbSize
  val EnableFauFTB = coreParams.EnableFauFTB
  val FtbSize = coreParams.FtbSize
  val FtbWays = coreParams.FtbWays
  val RasSize = coreParams.RasSize

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
  val IBufSize = coreParams.IBufSize
  val DecodeWidth = coreParams.DecodeWidth
  val RenameWidth = coreParams.RenameWidth
  val CommitWidth = coreParams.CommitWidth
  val MaxUopSize = coreParams.MaxUopSize
  val FtqSize = coreParams.FtqSize
  val EnableLoadFastWakeUp = coreParams.EnableLoadFastWakeUp
  val IntLogicRegs = coreParams.IntLogicRegs
  val FpLogicRegs = coreParams.FpLogicRegs
  val VecLogicRegs = coreParams.VecLogicRegs
  val VCONFIG_IDX = coreParams.VCONFIG_IDX
  val NRPhyRegs = coreParams.NRPhyRegs
  val PhyRegIdxWidth = log2Up(NRPhyRegs)
  val IntPhyRegs = coreParams.IntPhyRegs
  val VfPhyRegs = coreParams.VfPhyRegs
  val IntPregIdxWidth = log2Up(IntPhyRegs)
  val VfPregIdxWidth = log2Up(VfPhyRegs)
  val RobSize = coreParams.RobSize
  val RabSize = coreParams.RabSize
  val IntRefCounterWidth = log2Ceil(RobSize)
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

  def backendParams: BackendParams = coreParams.backendParams
  def MemIQSizeMax = backendParams.memSchdParams.get.issueBlockParams.map(_.numEntries).max
  def IQSizeMax = backendParams.allSchdParams.map(_.issueBlockParams.map(_.numEntries).max).max
  val LoadPipelineWidth = coreParams.LoadPipelineWidth
  val StorePipelineWidth = coreParams.StorePipelineWidth
  val VecMemSrcInWidth = coreParams.VecMemSrcInWidth
  val VecMemInstWbWidth = coreParams.VecMemInstWbWidth
  val VecMemDispatchWidth = coreParams.VecMemDispatchWidth
  val StoreBufferSize = coreParams.StoreBufferSize
  val StoreBufferThreshold = coreParams.StoreBufferThreshold
  val EnsbufferWidth = coreParams.EnsbufferWidth
  val UncacheBufferSize = coreParams.UncacheBufferSize
  val EnableLoadToLoadForward = coreParams.EnableLoadToLoadForward
  val EnableFastForward = coreParams.EnableFastForward
  val EnableLdVioCheckAfterReset = coreParams.EnableLdVioCheckAfterReset
  val EnableSoftPrefetchAfterReset = coreParams.EnableSoftPrefetchAfterReset
  val EnableCacheErrorAfterReset = coreParams.EnableCacheErrorAfterReset
  val EnableDCacheWPU = coreParams.EnableDCacheWPU
  val EnableAccurateLoadError = coreParams.EnableAccurateLoadError
  val EnableUncacheWriteOutstanding = coreParams.EnableUncacheWriteOutstanding
  val asidLen = coreParams.MMUAsidLen
  val BTLBWidth = coreParams.LoadPipelineWidth + coreParams.StorePipelineWidth
  val refillBothTlb = coreParams.refillBothTlb
  val itlbParams = coreParams.itlbParameters
  val ldtlbParams = coreParams.ldtlbParameters
  val sttlbParams = coreParams.sttlbParameters
  val pftlbParams = coreParams.pftlbParameters
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
  val LFSTEnable = false

  val PCntIncrStep: Int = 6
  val numPCntHc: Int = 25
  val numPCntPtw: Int = 19

  val numCSRPCntFrontend = 8
  val numCSRPCntCtrl     = 8
  val numCSRPCntLsu      = 8
  val numCSRPCntHc       = 5

  // source stages of cancel signal to issue queues
  val cancelStages = Seq("IS", "OG0", "OG1")
}
