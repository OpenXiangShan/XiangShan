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
import xiangshan.backend.exu._
import xiangshan.backend.dispatch.DispatchParameters
import xiangshan.cache.DCacheParameters
import xiangshan.cache.prefetch._
import xiangshan.frontend.{BasePredictor, BranchPredictionResp, FTB, FakePredictor, RAS, Tage, ITTage, Tage_SC, FauFTB}
import xiangshan.frontend.icache.ICacheParameters
import xiangshan.cache.mmu.{L2TLBParameters, TLBParameters}
import freechips.rocketchip.diplomacy.AddressSet
import system.SoCParamsKey
import huancun._
import huancun.debug._
import coupledL2._
import xiangshan.mem.prefetch.{PrefetcherParams, SMSParams}

import scala.math.min

case object XSTileKey extends Field[Seq[XSCoreParameters]]

case object XSCoreParamsKey extends Field[XSCoreParameters]

case class XSCoreParameters
(
  HasPrefetch: Boolean = false,
  HartId: Int = 0,
  XLEN: Int = 64,
  HasMExtension: Boolean = true,
  HasCExtension: Boolean = true,
  HasDiv: Boolean = true,
  HasICache: Boolean = true,
  HasDCache: Boolean = true,
  AddrBits: Int = 64,
  VAddrBits: Int = 39,
  HasFPU: Boolean = true,
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
  FtqSize: Int = 64,
  EnableLoadFastWakeUp: Boolean = true, // NOTE: not supported now, make it false
  IssQueSize: Int = 16,
  NRPhyRegs: Int = 192,
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
  dpParams: DispatchParameters = DispatchParameters(
    IntDqSize = 16,
    FpDqSize = 16,
    LsDqSize = 16,
    IntDqDeqWidth = 4,
    FpDqDeqWidth = 4,
    LsDqDeqWidth = 4
  ),
  exuParameters: ExuParameters = ExuParameters(
    JmpCnt = 1,
    AluCnt = 4,
    MulCnt = 0,
    MduCnt = 2,
    FmacCnt = 4,
    FmiscCnt = 2,
    FmiscDivSqrtCnt = 0,
    LduCnt = 2,
    StuCnt = 2
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
  EnableStorePrefetchAtIssue: Boolean = true,
  EnableStorePrefetchAtCommit: Boolean = false,
  EnableAtCommitMissTrigger: Boolean = true,
  EnableStorePrefetchSMS: Boolean = true,
  EnableStorePrefetchSPB: Boolean = false,
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
  val allHistLens = SCHistLens ++ ITTageTableInfos.map(_._2) ++ TageTableInfos.map(_._2) :+ UbtbGHRLength
  val HistoryLength = allHistLens.max + numBr * FtqSize + 9 // 256 for the predictor configs now

  val loadExuConfigs = Seq.fill(exuParameters.LduCnt)(LdExeUnitCfg)
  val storeExuConfigs = Seq.fill(exuParameters.StuCnt)(StaExeUnitCfg) ++ Seq.fill(exuParameters.StuCnt)(StdExeUnitCfg)

  val intExuConfigs = (Seq.fill(exuParameters.AluCnt)(AluExeUnitCfg) ++
    Seq.fill(exuParameters.MduCnt)(MulDivExeUnitCfg) :+ JumpCSRExeUnitCfg)

  val fpExuConfigs =
    Seq.fill(exuParameters.FmacCnt)(FmacExeUnitCfg) ++
      Seq.fill(exuParameters.FmiscCnt)(FmiscExeUnitCfg)

  val exuConfigs: Seq[ExuConfig] = intExuConfigs ++ fpExuConfigs ++ loadExuConfigs ++ storeExuConfigs
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
)

trait HasXSParameter {

  implicit val p: Parameters

  val PAddrBits = p(SoCParamsKey).PAddrBits // PAddrBits is Phyical Memory addr bits

  val coreParams = p(XSCoreParamsKey)
  val env = p(DebugOptionsKey)

  val XLEN = coreParams.XLEN
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
  val FtqSize = coreParams.FtqSize
  val IssQueSize = coreParams.IssQueSize
  val EnableLoadFastWakeUp = coreParams.EnableLoadFastWakeUp
  val NRPhyRegs = coreParams.NRPhyRegs
  val PhyRegIdxWidth = log2Up(NRPhyRegs)
  val RobSize = coreParams.RobSize
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
  val exuParameters = coreParams.exuParameters
  val NRMemReadPorts = exuParameters.LduCnt + 2 * exuParameters.StuCnt
  val NRIntReadPorts = 2 * exuParameters.AluCnt + NRMemReadPorts
  val NRIntWritePorts = exuParameters.AluCnt + exuParameters.MduCnt + exuParameters.LduCnt
  val NRFpReadPorts = 3 * exuParameters.FmacCnt + exuParameters.StuCnt
  val NRFpWritePorts = exuParameters.FpExuCnt + exuParameters.LduCnt
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
  val EnableStorePrefetchAtIssue = coreParams.EnableStorePrefetchAtIssue
  val EnableStorePrefetchAtCommit = coreParams.EnableStorePrefetchAtCommit
  val EnableAtCommitMissTrigger = coreParams.EnableAtCommitMissTrigger
  val EnableStorePrefetchSMS = coreParams.EnableStorePrefetchSMS
  val EnableStorePrefetchSPB = coreParams.EnableStorePrefetchSPB
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

  val NumRs = (exuParameters.JmpCnt+1)/2 + (exuParameters.AluCnt+1)/2 + (exuParameters.MulCnt+1)/2 +
              (exuParameters.MduCnt+1)/2 + (exuParameters.FmacCnt+1)/2 +  + (exuParameters.FmiscCnt+1)/2 +
              (exuParameters.FmiscDivSqrtCnt+1)/2 + (exuParameters.LduCnt+1)/2 +
              (exuParameters.StuCnt+1)/2 + (exuParameters.StuCnt+1)/2

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
  val loadExuConfigs = coreParams.loadExuConfigs
  val storeExuConfigs = coreParams.storeExuConfigs

  val intExuConfigs = coreParams.intExuConfigs

  val fpExuConfigs = coreParams.fpExuConfigs

  val exuConfigs = coreParams.exuConfigs

  val PCntIncrStep: Int = 6
  val numPCntHc: Int = 25
  val numPCntPtw: Int = 19

  val numCSRPCntFrontend = 8
  val numCSRPCntCtrl     = 8
  val numCSRPCntLsu      = 8
  val numCSRPCntHc       = 5
}
