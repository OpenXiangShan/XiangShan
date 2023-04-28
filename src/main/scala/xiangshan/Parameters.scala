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
import xiangshan.cache.mmu.{L2TLBParameters, TLBParameters}
import xiangshan.frontend._
import xiangshan.frontend.icache.ICacheParameters

import scala.math.min

case object XSTileKey extends Field[Seq[XSCoreParameters]]

case object XSCoreParamsKey extends Field[XSCoreParameters]

case class XSCoreParameters
(
  HasPrefetch: Boolean = false,
  HartId: Int = 0,
  XLEN: Int = 64,
  VLEN: Int = 128,
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
  MaxUopSize: Int = 37,
  FtqSize: Int = 64,
  EnableLoadFastWakeUp: Boolean = true, // NOTE: not supported now, make it false
  IntLogicRegs: Int = 33,
  FpLogicRegs: Int = 33,
  VecLogicRegs: Int = 40,
  NRPhyRegs: Int = 192,
  IntPhyRegs: Int = 192,
  VfPhyRegs: Int = 192,
  LoadQueueSize: Int = 80,
  LoadQueueNWriteBanks: Int = 8,
  StoreQueueSize: Int = 64,
  StoreQueueNWriteBanks: Int = 8,
  VlsQueueSize: Int = 8,
  RobSize: Int = 256,
  dpParams: DispatchParameters = DispatchParameters(
    IntDqSize = 16,
    FpDqSize = 16,
    LsDqSize = 16,
    IntDqDeqWidth = 6,
    FpDqDeqWidth = 6,
    LsDqDeqWidth = 6,
  ),
  intPreg: PregParams = IntPregParams(
    numEntries = 64,
    numRead = 14,
    numWrite = 8,
  ),
  vfPreg: VfPregParams = VfPregParams(
    numEntries = 64,
    numRead = 14,
    numWrite = 8,
  ),
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
  EnableUncacheWriteOutstanding: Boolean = true,
  MMUAsidLen: Int = 16, // max is 16, 0 is not supported now
  ReSelectLen: Int = 6, // load replay queue replay select counter len
  itlbParameters: TLBParameters = TLBParameters(
    name = "itlb",
    fetchi = true,
    useDmode = false,
    normalNWays = 32,
    normalReplacer = Some("plru"),
    superNWays = 4,
    superReplacer = Some("plru")
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
    prefetch = Some(huancun.prefetch.BOPParameters())
  )),
  L2NBanks: Int = 1,
  usePTWRepeater: Boolean = false,
  softPTW: Boolean = false, // dpi-c debug only
  softPTWDelay: Int = 1
){
  val allHistLens = SCHistLens ++ ITTageTableInfos.map(_._2) ++ TageTableInfos.map(_._2) :+ UbtbGHRLength
  val HistoryLength = allHistLens.max + numBr * FtqSize + 9 // 256 for the predictor configs now

  def intSchdParams = {
    implicit val schdType: SchedulerType = IntScheduler()
    val pregBits = intPreg.addrWidth
    val numRfRead = intPreg.numRead
    val numRfWrite = intPreg.numWrite
    SchdBlockParams(Seq(
      IssueBlockParams(Seq(
        ExeUnitParams(Seq(AluCfg, MulCfg, BkuCfg), Seq(IntWB(port = 0, 0)), Seq(Seq(IntRD(0, 2)), Seq(IntRD(1, 2)))),
        ExeUnitParams(Seq(AluCfg, MulCfg, BkuCfg), Seq(IntWB(port = 1, 0)), Seq(Seq(IntRD(0, 1)), Seq(IntRD(1, 1)))),
      ), numEntries = 8, pregBits = pregBits, numWakeupFromWB = numRfWrite, numEnq = 2),
      IssueBlockParams(Seq(
        ExeUnitParams(Seq(DivCfg), Seq(IntWB(port = 2, 0)), Seq(Seq(IntRD(4, 0)), Seq(IntRD(5, 0)))),
        ExeUnitParams(Seq(DivCfg), Seq(IntWB(port = 3, 0)), Seq(Seq(IntRD(6, 0)), Seq(IntRD(7, 0)))),
      ), numEntries = 8, pregBits = pregBits, numWakeupFromWB = numRfWrite, numEnq = 2),
      IssueBlockParams(Seq(
        ExeUnitParams(Seq(BrhCfg, JmpCfg, CsrCfg, FenceCfg), Seq(IntWB(port = 4, 0)), Seq(Seq(IntRD(2, 1)), Seq(IntRD(3, 1)))),
        ExeUnitParams(Seq(BrhCfg), Seq(), Seq(Seq(IntRD(6, 1)), Seq(IntRD(4, 1)))),
      ), numEntries = 8, pregBits = pregBits, numWakeupFromWB = numRfWrite, numEnq = 2),
      IssueBlockParams(Seq(
        ExeUnitParams(Seq(I2fCfg, VSetIVLCfg, VSetIVConfigCfg), Seq(VecWB(port = 6, Int.MaxValue), IntWB(port = 7, 0)), Seq(Seq(IntRD(6, 0)), Seq(IntRD(7, 0)))),
      ), numEntries = 8, pregBits = pregBits, numWakeupFromWB = numRfWrite, numEnq = 2)
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
      IssueBlockParams(Seq(
        ExeUnitParams(Seq(FmacCfg), Seq(VecWB(port = 0, 0)), Seq(Seq(VfRD(0, 0)), Seq(VfRD(1, 0)), Seq(VfRD(2, 0)))),
        ExeUnitParams(Seq(FmacCfg), Seq(VecWB(port = 1, 0)), Seq(Seq(VfRD(3, 0)), Seq(VfRD(4, 0)), Seq(VfRD(5, 0)))),
      ), numEntries = 8, pregBits = pregBits, numWakeupFromWB = numRfWrite, numEnq = 4),
      IssueBlockParams(Seq(
        ExeUnitParams(Seq(F2fCfg, F2iCfg, FDivSqrtCfg, VSetFVConfigCfg), Seq(VecWB(port = 2, 0), IntWB(port = 7, 0)), Seq(Seq(VfRD(6, 0)), Seq(VfRD(7, 0)))),
      ), numEntries = 8, pregBits = pregBits, numWakeupFromWB = numRfWrite, numEnq = 4),
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
        ExeUnitParams(Seq(LduCfg), Seq(IntWB(5, 0), VecWB(4, 0)), Seq(Seq(IntRD(8, 0)))),
        ExeUnitParams(Seq(LduCfg), Seq(IntWB(6, 0), VecWB(5, 0)), Seq(Seq(IntRD(9, 0)))),
      ), numEntries = 8, pregBits = pregBits, numWakeupFromWB = 16, numEnq = 2),
      IssueBlockParams(Seq(
        ExeUnitParams(Seq(StaCfg, MouCfg), Seq(IntWB(5, 1)), Seq(Seq(IntRD(10, 0)))),
        ExeUnitParams(Seq(StaCfg, MouCfg), Seq(IntWB(6, 1)), Seq(Seq(IntRD(11, 0)))),
      ), numEntries = 8, pregBits = pregBits, numWakeupFromWB = 16, numEnq = 2),
      IssueBlockParams(Seq(
        ExeUnitParams(Seq(StdCfg, MoudCfg), Seq(), Seq(Seq(IntRD(12, 0), VfRD(12, 0)))),
        ExeUnitParams(Seq(StdCfg, MoudCfg), Seq(), Seq(Seq(IntRD(13, 0), VfRD(13, 0)))),
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

  def backendParams: BackendParams = backend.BackendParams(Map(
    IntScheduler() -> intSchdParams,
    VfScheduler() -> vfSchdParams,
    MemScheduler() -> memSchdParams,
  ), Seq(
    intPreg,
    vfPreg,
  ))
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
  EnableTopDown: Boolean = false
)

trait HasXSParameter {

  implicit val p: Parameters

  val PAddrBits = p(SoCParamsKey).PAddrBits // PAddrBits is Phyical Memory addr bits

  val coreParams = p(XSCoreParamsKey)
  val env = p(DebugOptionsKey)

  val XLEN = coreParams.XLEN
  val VLEN = coreParams.VLEN
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
  val NRPhyRegs = coreParams.NRPhyRegs
  val PhyRegIdxWidth = log2Up(NRPhyRegs)
  val IntPhyRegs = coreParams.IntPhyRegs
  val VfPhyRegs = coreParams.VfPhyRegs
  val IntPregIdxWidth = log2Up(IntPhyRegs)
  val VfPregIdxWidth = log2Up(VfPhyRegs)
  val RobSize = coreParams.RobSize
  val IntRefCounterWidth = log2Ceil(RobSize)
  val LoadQueueSize = coreParams.LoadQueueSize
  val LoadQueueNWriteBanks = coreParams.LoadQueueNWriteBanks
  val StoreQueueSize = coreParams.StoreQueueSize
  val StoreQueueNWriteBanks = coreParams.StoreQueueNWriteBanks
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

  val PCntIncrStep: Int = 6
  val numPCntHc: Int = 25
  val numPCntPtw: Int = 19

  val numCSRPCntFrontend = 8
  val numCSRPCntCtrl     = 8
  val numCSRPCntLsu      = 8
  val numCSRPCntHc       = 5
}
