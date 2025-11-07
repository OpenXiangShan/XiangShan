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
  LoadQueueSize: Int = 80,
  LoadQueueNWriteBanks: Int = 8,
  StoreQueueSize: Int = 64,
  StoreQueueNWriteBanks: Int = 8,
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
  StoreBufferSize: Int = 16,
  StoreBufferThreshold: Int = 7,
  EnableLoadToLoadForward: Boolean = true,
  EnableFastForward: Boolean = false,
  EnableLdVioCheckAfterReset: Boolean = true,
  EnableSoftPrefetchAfterReset: Boolean = true,
  EnableCacheErrorAfterReset: Boolean = true,
  EnablePTWPreferCache: Boolean = true,
  EnableAccurateLoadError: Boolean = true,
  MMUAsidLen: Int = 16, // max is 16, 0 is not supported now
  itlbParameters: TLBParameters = TLBParameters(
    name = "itlb",
    fetchi = true,
    useDmode = false,
    sameCycle = false,
    missSameCycle = true,
    normalNWays = 32,
    normalReplacer = Some("plru"),
    superNWays = 4,
    superReplacer = Some("plru"),
    shouldBlock = true
  ),
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
    tagECC = None,
    dataECC = None,
    replacer = Some("setplru"),
    nMissEntries = 2,
    nProbeEntries = 2,
    nPrefetchEntries = 2,
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
  L2CacheParamsOpt: Option[HCCacheParameters] = Some(HCCacheParameters(
    name = "l2",
    level = 2,
    ways = 8,
    sets = 1024, // default 512KB L2
    prefetch = Some(huancun.prefetch.PrefetchReceiverParams())
  )),
  L2NBanks: Int = 1,
  usePTWRepeater: Boolean = false,
  softPTW: Boolean = false // dpi-c debug only
){
  def allHistLens = SCHistLens ++ ITTageTableInfos.map(_._2) ++ TageTableInfos.map(_._2) :+ UbtbGHRLength
  def HistoryLength = allHistLens.max + numBr * FtqSize + 9 // 256 for the predictor configs now

  def loadExuConfigs = Seq.fill(exuParameters.LduCnt)(LdExeUnitCfg)
  def storeExuConfigs = Seq.fill(exuParameters.StuCnt)(StaExeUnitCfg) ++ Seq.fill(exuParameters.StuCnt)(StdExeUnitCfg)

  def intExuConfigs = (Seq.fill(exuParameters.AluCnt)(AluExeUnitCfg) ++
    Seq.fill(exuParameters.MduCnt)(MulDivExeUnitCfg) :+ JumpCSRExeUnitCfg)

  def fpExuConfigs =
    Seq.fill(exuParameters.FmacCnt)(FmacExeUnitCfg) ++
      Seq.fill(exuParameters.FmiscCnt)(FmiscExeUnitCfg)

  def exuConfigs: Seq[ExuConfig] = intExuConfigs ++ fpExuConfigs ++ loadExuConfigs ++ storeExuConfigs
}

case object DebugOptionsKey extends Field[DebugOptions]

case class DebugOptions
(
  FPGAPlatform: Boolean = false,
  EnableDifftest: Boolean = false,
  AlwaysBasicDiff: Boolean = true,
  EnableDebug: Boolean = false,
  EnablePerfDebug: Boolean = true,
  UseDRAMSim: Boolean = false
)

trait HasXSParameter {

  implicit val p: Parameters

  def PAddrBits = p(SoCParamsKey).PAddrBits // PAddrBits is Phyical Memory addr bits

  def coreParams = p(XSCoreParamsKey)
  def env = p(DebugOptionsKey)

  def XLEN = coreParams.XLEN
  def minFLen = 32
  def fLen = 64
  def xLen = XLEN

  def HasMExtension = coreParams.HasMExtension
  def HasCExtension = coreParams.HasCExtension
  def HasDiv = coreParams.HasDiv
  def HasIcache = coreParams.HasICache
  def HasDcache = coreParams.HasDCache
  def AddrBits = coreParams.AddrBits // AddrBits is used in some cases
  def VAddrBits = coreParams.VAddrBits // VAddrBits is Virtual Memory addr bits
  def AsidLength = coreParams.AsidLength
  def AddrBytes = AddrBits / 8 // unused
  def DataBits = XLEN
  def DataBytes = DataBits / 8
  def HasFPU = coreParams.HasFPU
  def HasCustomCSRCacheOp = coreParams.HasCustomCSRCacheOp
  def FetchWidth = coreParams.FetchWidth
  def PredictWidth = FetchWidth * (if (HasCExtension) 2 else 1)
  def EnableBPU = coreParams.EnableBPU
  def EnableBPD = coreParams.EnableBPD // enable backing predictor(like Tage) in BPUStage3
  def EnableRAS = coreParams.EnableRAS
  def EnableLB = coreParams.EnableLB
  def EnableLoop = coreParams.EnableLoop
  def EnableSC = coreParams.EnableSC
  def EnbaleTlbDebug = coreParams.EnbaleTlbDebug
  def HistoryLength = coreParams.HistoryLength
  def EnableGHistDiff = coreParams.EnableGHistDiff
  def UbtbGHRLength = coreParams.UbtbGHRLength
  def UbtbSize = coreParams.UbtbSize
  def EnableFauFTB = coreParams.EnableFauFTB
  def FtbSize = coreParams.FtbSize
  def FtbWays = coreParams.FtbWays
  def RasSize = coreParams.RasSize

  def getBPDComponents(resp_in: BranchPredictionResp, p: Parameters) = {
    coreParams.branchPredictor(resp_in, p)
  }
  def numBr = coreParams.numBr
  def TageTableInfos = coreParams.TageTableInfos
  def TageBanks = coreParams.numBr
  def SCNRows = coreParams.SCNRows
  def SCCtrBits = coreParams.SCCtrBits
  def SCHistLens = coreParams.SCHistLens
  def SCNTables = coreParams.SCNTables

  def SCTableInfos = Seq.fill(SCNTables)((SCNRows, SCCtrBits)) zip SCHistLens map {
    case ((n, cb), h) => (n, cb, h)
  }
  def ITTageTableInfos = coreParams.ITTageTableInfos
  type FoldedHistoryInfo = Tuple2[Int, Int]
  def foldedGHistInfos =
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



  def CacheLineSize = coreParams.CacheLineSize
  def CacheLineHalfWord = CacheLineSize / 16
  def ExtHistoryLength = HistoryLength + 64
  def IBufSize = coreParams.IBufSize
  def DecodeWidth = coreParams.DecodeWidth
  def RenameWidth = coreParams.RenameWidth
  def CommitWidth = coreParams.CommitWidth
  def FtqSize = coreParams.FtqSize
  def IssQueSize = coreParams.IssQueSize
  def EnableLoadFastWakeUp = coreParams.EnableLoadFastWakeUp
  def NRPhyRegs = coreParams.NRPhyRegs
  def PhyRegIdxWidth = log2Up(NRPhyRegs)
  def RobSize = coreParams.RobSize
  def IntRefCounterWidth = log2Ceil(RobSize)
  def LoadQueueSize = coreParams.LoadQueueSize
  def LoadQueueNWriteBanks = coreParams.LoadQueueNWriteBanks
  def StoreQueueSize = coreParams.StoreQueueSize
  def StoreQueueNWriteBanks = coreParams.StoreQueueNWriteBanks
  def dpParams = coreParams.dpParams
  def exuParameters = coreParams.exuParameters
  def NRMemReadPorts = exuParameters.LduCnt + 2 * exuParameters.StuCnt
  def NRIntReadPorts = 2 * exuParameters.AluCnt + NRMemReadPorts
  def NRIntWritePorts = exuParameters.AluCnt + exuParameters.MduCnt + exuParameters.LduCnt
  def NRFpReadPorts = 3 * exuParameters.FmacCnt + exuParameters.StuCnt
  def NRFpWritePorts = exuParameters.FpExuCnt + exuParameters.LduCnt
  def LoadPipelineWidth = coreParams.LoadPipelineWidth
  def StorePipelineWidth = coreParams.StorePipelineWidth
  def StoreBufferSize = coreParams.StoreBufferSize
  def StoreBufferThreshold = coreParams.StoreBufferThreshold
  def EnableLoadToLoadForward = coreParams.EnableLoadToLoadForward
  def EnableFastForward = coreParams.EnableFastForward
  def EnableLdVioCheckAfterReset = coreParams.EnableLdVioCheckAfterReset
  def EnableSoftPrefetchAfterReset = coreParams.EnableSoftPrefetchAfterReset
  def EnableCacheErrorAfterReset = coreParams.EnableCacheErrorAfterReset
  def EnablePTWPreferCache = coreParams.EnablePTWPreferCache
  def EnableAccurateLoadError = coreParams.EnableAccurateLoadError
  def asidLen = coreParams.MMUAsidLen
  def BTLBWidth = coreParams.LoadPipelineWidth + coreParams.StorePipelineWidth
  def refillBothTlb = coreParams.refillBothTlb
  def itlbParams = coreParams.itlbParameters
  def ldtlbParams = coreParams.ldtlbParameters
  def ld_tlb_ports = if(coreParams.prefetcher.nonEmpty) 3 else 2
  def sttlbParams = coreParams.sttlbParameters
  def btlbParams = coreParams.btlbParameters
  def l2tlbParams = coreParams.l2tlbParameters
  def NumPerfCounters = coreParams.NumPerfCounters

  def NumRs = (exuParameters.JmpCnt+1)/2 + (exuParameters.AluCnt+1)/2 + (exuParameters.MulCnt+1)/2 +
              (exuParameters.MduCnt+1)/2 + (exuParameters.FmacCnt+1)/2 +  + (exuParameters.FmiscCnt+1)/2 +
              (exuParameters.FmiscDivSqrtCnt+1)/2 + (exuParameters.LduCnt+1)/2 +
              ((exuParameters.StuCnt+1)/2) + ((exuParameters.StuCnt+1)/2)

  def instBytes = if (HasCExtension) 2 else 4
  def instOffsetBits = log2Ceil(instBytes)

  def icacheParameters = coreParams.icacheParameters
  def dcacheParameters = coreParams.dcacheParametersOpt.getOrElse(DCacheParameters())

  // dcache block cacheline when lr for LRSCCycles - LRSCBackOff cycles
  // for constrained LR/SC loop
  def LRSCCycles = 64
  // for lr storm
  def LRSCBackOff = 8

  // cache hierarchy configurations
  def l1BusDataWidth = 256

  // load violation predict
  def ResetTimeMax2Pow = 20 //1078576
  def ResetTimeMin2Pow = 10 //1024
  // wait table parameters
  def WaitTableSize = 1024
  def MemPredPCWidth = log2Up(WaitTableSize)
  def LWTUse2BitCounter = true
  // store set parameters
  def SSITSize = WaitTableSize
  def LFSTSize = 32
  def SSIDWidth = log2Up(LFSTSize)
  def LFSTWidth = 4
  def StoreSetEnable = true // LWT will be disabled if SS is enabled

  def loadExuConfigs = coreParams.loadExuConfigs
  def storeExuConfigs = coreParams.storeExuConfigs

  def intExuConfigs = coreParams.intExuConfigs

  def fpExuConfigs = coreParams.fpExuConfigs

  def exuConfigs = coreParams.exuConfigs

  def PCntIncrStep: Int = 6
  def numPCntHc: Int = 25
  def numPCntPtw: Int = 19

  def numCSRPCntFrontend = 8
  def numCSRPCntCtrl     = 8
  def numCSRPCntLsu      = 8
  def numCSRPCntHc       = 5
  def printEventCoding   = true
  // Parameters for Sdtrig extension
  protected def TriggerNum = 10
  protected def TriggerChainMaxLength = 2
}
