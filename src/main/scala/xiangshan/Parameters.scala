package xiangshan

import chipsalliance.rocketchip.config.{Field, Parameters}
import chisel3._
import chisel3.util._
import xiangshan.backend.exu._
import xiangshan.backend.fu._
import xiangshan.backend.fu.fpu._
import xiangshan.backend.dispatch.DispatchParameters
import xiangshan.cache.{DCacheParameters, ICacheParameters, L1plusCacheParameters}
import xiangshan.cache.prefetch.{BOPParameters, L1plusPrefetcherParameters, L2PrefetcherParameters, StreamPrefetchParameters}

case object XSCoreParamsKey extends Field[XSCoreParameters]

case class XSCoreParameters
(
  HasL2Cache: Boolean = false,
  HasPrefetch: Boolean = false,
  HartId: Int = 0,
  XLEN: Int = 64,
  HasMExtension: Boolean = true,
  HasCExtension: Boolean = true,
  HasDiv: Boolean = true,
  HasICache: Boolean = true,
  HasDCache: Boolean = true,
  EnableStoreQueue: Boolean = true,
  AddrBits: Int = 64,
  VAddrBits: Int = 39,
  PAddrBits: Int = 40,
  HasFPU: Boolean = true,
  FetchWidth: Int = 8,
  EnableBPU: Boolean = true,
  EnableBPD: Boolean = true,
  EnableRAS: Boolean = true,
  EnableLB: Boolean = false,
  EnableLoop: Boolean = true,
  EnableSC: Boolean = true,
  EnbaleTlbDebug: Boolean = false,
  EnableJal: Boolean = false,
  EnableUBTB: Boolean = true,
  HistoryLength: Int = 64,
  BtbSize: Int = 2048,
  JbtacSize: Int = 1024,
  JbtacBanks: Int = 8,
  RasSize: Int = 16,
  CacheLineSize: Int = 512,
  UBtbWays: Int = 16,
  BtbWays: Int = 2,

  EnableL1plusPrefetcher: Boolean = true,
  IBufSize: Int = 48,
  DecodeWidth: Int = 6,
  RenameWidth: Int = 6,
  CommitWidth: Int = 6,
  BrqSize: Int = 32,
  FtqSize: Int = 48,
  EnableLoadFastWakeUp: Boolean = true, // NOTE: not supported now, make it false
  IssQueSize: Int = 16,
  NRPhyRegs: Int = 160,
  NRIntReadPorts: Int = 14,
  NRIntWritePorts: Int = 8,
  NRFpReadPorts: Int = 14,
  NRFpWritePorts: Int = 8,
  LoadQueueSize: Int = 64,
  StoreQueueSize: Int = 48,
  RoqSize: Int = 192,
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
  LoadPipelineWidth: Int = 2,
  StorePipelineWidth: Int = 2,
  StoreBufferSize: Int = 16,
  RefillSize: Int = 512,
  TlbEntrySize: Int = 32,
  TlbSPEntrySize: Int = 4,
  PtwL3EntrySize: Int = 4096, //(256 * 16) or 512
  PtwSPEntrySize: Int = 16,
  PtwL1EntrySize: Int = 16,
  PtwL2EntrySize: Int = 2048, //(256 * 8)
  NumPerfCounters: Int = 16,
){
  val loadExuConfigs = Seq.fill(exuParameters.LduCnt)(LdExeUnitCfg)
  val storeExuConfigs = Seq.fill(exuParameters.StuCnt)(StExeUnitCfg)

  val intExuConfigs = JumpExeUnitCfg +: (
    Seq.fill(exuParameters.MduCnt)(MulDivExeUnitCfg) ++
      Seq.fill(exuParameters.AluCnt)(AluExeUnitCfg)
    )

  val fpExuConfigs =
    Seq.fill(exuParameters.FmacCnt)(FmacExeUnitCfg) ++
      Seq.fill(exuParameters.FmiscCnt)(FmiscExeUnitCfg)

  val exuConfigs: Seq[ExuConfig] = intExuConfigs ++ fpExuConfigs ++ loadExuConfigs ++ storeExuConfigs
}

case object DebugOptionsKey extends Field[DebugOptions]

case class DebugOptions
(
  FPGAPlatform: Boolean = true,
  EnableDebug: Boolean = false,
  EnablePerfDebug: Boolean = true,
  UseDRAMSim: Boolean = false
)

trait HasXSParameter {

  implicit val p: Parameters

  val coreParams = p(XSCoreParamsKey)
  val env = p(DebugOptionsKey)

  val XLEN = coreParams.XLEN
  val hardId = coreParams.HartId
  val minFLen = 32
  val fLen = 64
  def xLen = XLEN

  val HasMExtension = coreParams.HasMExtension
  val HasCExtension = coreParams.HasCExtension
  val HasDiv = coreParams.HasDiv
  val HasIcache = coreParams.HasICache
  val HasDcache = coreParams.HasDCache
  val EnableStoreQueue = coreParams.EnableStoreQueue
  val AddrBits = coreParams.AddrBits // AddrBits is used in some cases
  val VAddrBits = coreParams.VAddrBits // VAddrBits is Virtual Memory addr bits
  val PAddrBits = coreParams.PAddrBits // PAddrBits is Phyical Memory addr bits
  val AddrBytes = AddrBits / 8 // unused
  val DataBits = XLEN
  val DataBytes = DataBits / 8
  val HasFPU = coreParams.HasFPU
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
  val BtbSize = coreParams.BtbSize
  // val BtbWays = 4
  val BtbBanks = PredictWidth
  // val BtbSets = BtbSize / BtbWays
  val JbtacSize = coreParams.JbtacSize
  val JbtacBanks = coreParams.JbtacBanks
  val RasSize = coreParams.RasSize
  val CacheLineSize = coreParams.CacheLineSize
  val CacheLineHalfWord = CacheLineSize / 16
  val ExtHistoryLength = HistoryLength + 64
  val UBtbWays = coreParams.UBtbWays
  val BtbWays = coreParams.BtbWays
  val EnableL1plusPrefetcher = coreParams.EnableL1plusPrefetcher
  val IBufSize = coreParams.IBufSize
  val DecodeWidth = coreParams.DecodeWidth
  val RenameWidth = coreParams.RenameWidth
  val CommitWidth = coreParams.CommitWidth
  val BrqSize = coreParams.BrqSize
  val FtqSize = coreParams.FtqSize
  val IssQueSize = coreParams.IssQueSize
  val EnableLoadFastWakeUp = coreParams.EnableLoadFastWakeUp
  val BrTagWidth = log2Up(BrqSize)
  val NRPhyRegs = coreParams.NRPhyRegs
  val PhyRegIdxWidth = log2Up(NRPhyRegs)
  val RoqSize = coreParams.RoqSize
  val LoadQueueSize = coreParams.LoadQueueSize
  val StoreQueueSize = coreParams.StoreQueueSize
  val dpParams = coreParams.dpParams
  val exuParameters = coreParams.exuParameters
  val NRIntReadPorts = coreParams.NRIntReadPorts
  val NRIntWritePorts = coreParams.NRIntWritePorts
  val NRMemReadPorts = exuParameters.LduCnt + 2 * exuParameters.StuCnt
  val NRFpReadPorts = coreParams.NRFpReadPorts
  val NRFpWritePorts = coreParams.NRFpWritePorts
  val LoadPipelineWidth = coreParams.LoadPipelineWidth
  val StorePipelineWidth = coreParams.StorePipelineWidth
  val StoreBufferSize = coreParams.StoreBufferSize
  val RefillSize = coreParams.RefillSize
  val DTLBWidth = coreParams.LoadPipelineWidth + coreParams.StorePipelineWidth
  val TlbEntrySize = coreParams.TlbEntrySize
  val TlbSPEntrySize = coreParams.TlbSPEntrySize
  val PtwL3EntrySize = coreParams.PtwL3EntrySize
  val PtwSPEntrySize = coreParams.PtwSPEntrySize
  val PtwL1EntrySize = coreParams.PtwL1EntrySize
  val PtwL2EntrySize = coreParams.PtwL2EntrySize
  val NumPerfCounters = coreParams.NumPerfCounters

  val instBytes = if (HasCExtension) 2 else 4
  val instOffsetBits = log2Ceil(instBytes)

  val icacheParameters = ICacheParameters(
    tagECC = Some("parity"),
    dataECC = Some("parity"),
    replacer = Some("setplru"),
    nMissEntries = 2
  )

  val l1plusCacheParameters = L1plusCacheParameters(
    tagECC = Some("secded"),
    dataECC = Some("secded"),
    replacer = Some("setplru"),
    nMissEntries = 8
  )

  val dcacheParameters = DCacheParameters(
    tagECC = Some("secded"),
    dataECC = Some("secded"),
    replacer = Some("setplru"),
    nMissEntries = 16,
    nProbeEntries = 16,
    nReleaseEntries = 16,
    nStoreReplayEntries = 16
  )

  val LRSCCycles = 100


  // cache hierarchy configurations
  val l1BusDataWidth = 256

  // L2 configurations
  val L1BusWidth = 256
  val L2Size = 512 * 1024 // 512KB
  val L2BlockSize = 64
  val L2NWays = 8
  val L2NSets = L2Size / L2BlockSize / L2NWays

  // L3 configurations
  val L2BusWidth = 256
  
  // icache prefetcher
  val l1plusPrefetcherParameters = L1plusPrefetcherParameters(
    enable = true,
    _type = "stream",
    streamParams = StreamPrefetchParameters(
      streamCnt = 2,
      streamSize = 4,
      ageWidth = 4,
      blockBytes = l1plusCacheParameters.blockBytes,
      reallocStreamOnMissInstantly = true,
      cacheName = "icache"
    )
  )

  // dcache prefetcher
  val l2PrefetcherParameters = L2PrefetcherParameters(
    enable = true,
    _type = "bop", // "stream" or "bop"
    streamParams = StreamPrefetchParameters(
      streamCnt = 4,
      streamSize = 4,
      ageWidth = 4,
      blockBytes = L2BlockSize,
      reallocStreamOnMissInstantly = true,
      cacheName = "dcache"
    ),
    bopParams = BOPParameters(
      rrTableEntries = 256,
      rrTagBits = 12,
      scoreBits = 5,
      roundMax = 50,
      badScore = 1,
      blockBytes = L2BlockSize,
      nEntries = dcacheParameters.nMissEntries * 2 // TODO: this is too large
    ),
  )

  val loadExuConfigs = coreParams.loadExuConfigs 
  val storeExuConfigs = coreParams.storeExuConfigs 

  val intExuConfigs = coreParams.intExuConfigs 

  val fpExuConfigs = coreParams.fpExuConfigs 

  val exuConfigs = coreParams.exuConfigs 
}
