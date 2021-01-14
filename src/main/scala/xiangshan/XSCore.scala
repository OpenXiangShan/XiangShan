package xiangshan

import chisel3._
import chisel3.util._
import top.Parameters
import xiangshan.backend._
import xiangshan.backend.dispatch.DispatchParameters
import xiangshan.backend.exu.ExuParameters
import xiangshan.backend.exu.Exu._
import xiangshan.frontend._
import xiangshan.mem._
import xiangshan.backend.fu.HasExceptionNO
import xiangshan.cache.{DCache, DCacheParameters, ICache, ICacheParameters, L1plusCache, L1plusCacheParameters, PTW, Uncache}
import xiangshan.cache.prefetch._
import chipsalliance.rocketchip.config
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink.{TLBuffer, TLBundleParameters, TLCacheCork, TLClientNode, TLFilter, TLIdentityNode, TLToAXI4, TLWidthWidget, TLXbar}
import freechips.rocketchip.devices.tilelink.{DevNullParams, TLError}
import sifive.blocks.inclusivecache.{CacheParameters, InclusiveCache, InclusiveCacheMicroParameters}
import freechips.rocketchip.amba.axi4.{AXI4Deinterleaver, AXI4Fragmenter, AXI4IdIndexer, AXI4IdentityNode, AXI4ToTL, AXI4UserYanker}
import freechips.rocketchip.tile.HasFPUParameters
import utils._

case class XSCoreParameters
(
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
  FectchWidth: Int = 8,
  EnableBPU: Boolean = true,
  EnableBPD: Boolean = true,
  EnableRAS: Boolean = true,
  EnableLB: Boolean = false,
  EnableLoop: Boolean = true,
  EnableSC: Boolean = false,
  HistoryLength: Int = 64,
  BtbSize: Int = 2048,
  JbtacSize: Int = 1024,
  JbtacBanks: Int = 8,
  RasSize: Int = 16,
  CacheLineSize: Int = 512,
  UBtbWays: Int = 16,
  BtbWays: Int = 2,

  EnableL1plusPrefetcher: Boolean = true,
  IBufSize: Int = 32,
  DecodeWidth: Int = 6,
  RenameWidth: Int = 6,
  CommitWidth: Int = 6,
  BrqSize: Int = 32,
  IssQueSize: Int = 12,
  NRPhyRegs: Int = 160,
  NRIntReadPorts: Int = 14,
  NRIntWritePorts: Int = 8,
  NRFpReadPorts: Int = 14,
  NRFpWritePorts: Int = 8,
  LoadQueueSize: Int = 64,
  StoreQueueSize: Int = 48,
  RoqSize: Int = 192,
  dpParams: DispatchParameters = DispatchParameters(
    IntDqSize = 32,
    FpDqSize = 32,
    LsDqSize = 32,
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
  TlbL2EntrySize: Int = 256, // or 512
  PtwL1EntrySize: Int = 16,
  PtwL2EntrySize: Int = 256,
  NumPerfCounters: Int = 16,
  NrExtIntr: Int = 1
)

trait HasXSParameter {

  val core = Parameters.get.coreParameters
  val env = Parameters.get.envParameters

  val XLEN = 64
  val minFLen = 32
  val fLen = 64
  def xLen = 64
  val HasMExtension = core.HasMExtension
  val HasCExtension = core.HasCExtension
  val HasDiv = core.HasDiv
  val HasIcache = core.HasICache
  val HasDcache = core.HasDCache
  val EnableStoreQueue = core.EnableStoreQueue
  val AddrBits = core.AddrBits // AddrBits is used in some cases
  val VAddrBits = core.VAddrBits // VAddrBits is Virtual Memory addr bits
  val PAddrBits = core.PAddrBits // PAddrBits is Phyical Memory addr bits
  val AddrBytes = AddrBits / 8 // unused
  val DataBits = XLEN
  val DataBytes = DataBits / 8
  val HasFPU = core.HasFPU
  val FetchWidth = core.FectchWidth
  val PredictWidth = FetchWidth * (if (HasCExtension) 2 else 1)
  val EnableBPU = core.EnableBPU
  val EnableBPD = core.EnableBPD // enable backing predictor(like Tage) in BPUStage3
  val EnableRAS = core.EnableRAS
  val EnableLB = core.EnableLB
  val EnableLoop = core.EnableLoop
  val EnableSC = core.EnableSC
  val HistoryLength = core.HistoryLength
  val BtbSize = core.BtbSize
  // val BtbWays = 4
  val BtbBanks = PredictWidth
  // val BtbSets = BtbSize / BtbWays
  val JbtacSize = core.JbtacSize
  val JbtacBanks = core.JbtacBanks
  val RasSize = core.RasSize
  val CacheLineSize = core.CacheLineSize
  val CacheLineHalfWord = CacheLineSize / 16
  val ExtHistoryLength = HistoryLength + 64
  val UBtbWays = core.UBtbWays
  val BtbWays = core.BtbWays
  val EnableL1plusPrefetcher = core.EnableL1plusPrefetcher
  val IBufSize = core.IBufSize
  val DecodeWidth = core.DecodeWidth
  val RenameWidth = core.RenameWidth
  val CommitWidth = core.CommitWidth
  val BrqSize = core.BrqSize
  val IssQueSize = core.IssQueSize
  val BrTagWidth = log2Up(BrqSize)
  val NRPhyRegs = core.NRPhyRegs
  val PhyRegIdxWidth = log2Up(NRPhyRegs)
  val RoqSize = core.RoqSize
  val LoadQueueSize = core.LoadQueueSize
  val StoreQueueSize = core.StoreQueueSize
  val dpParams = core.dpParams
  val exuParameters = core.exuParameters
  val NRIntReadPorts = core.NRIntReadPorts
  val NRIntWritePorts = core.NRIntWritePorts
  val NRMemReadPorts = exuParameters.LduCnt + 2*exuParameters.StuCnt
  val NRFpReadPorts = core.NRFpReadPorts
  val NRFpWritePorts = core.NRFpWritePorts
  val LoadPipelineWidth = core.LoadPipelineWidth
  val StorePipelineWidth = core.StorePipelineWidth
  val StoreBufferSize = core.StoreBufferSize
  val RefillSize = core.RefillSize
  val DTLBWidth = core.LoadPipelineWidth + core.StorePipelineWidth
  val TlbEntrySize = core.TlbEntrySize
  val TlbL2EntrySize = core.TlbL2EntrySize
  val PtwL1EntrySize = core.PtwL1EntrySize
  val PtwL2EntrySize = core.PtwL2EntrySize
  val NumPerfCounters = core.NumPerfCounters
  val NrExtIntr = core.NrExtIntr

  val icacheParameters = ICacheParameters(
    tagECC = Some("parity"),
    dataECC = Some("parity"),
    nMissEntries = 2
  )

  val l1plusCacheParameters = L1plusCacheParameters(
    tagECC = Some("secded"),
    dataECC = Some("secded"),
    nMissEntries = 8
  )

  // icache prefetcher
  val l1plusPrefetcherParameters = L1plusPrefetcherParameters(
    enable = false,
    _type = "stream",
    streamParams = StreamPrefetchParameters(
      streamCnt = 4,
      streamSize = 4,
      ageWidth = 4,
      blockBytes = l1plusCacheParameters.blockBytes,
      reallocStreamOnMissInstantly = true
    )
  )

  // dcache prefetcher
  val l2PrefetcherParameters = L2PrefetcherParameters(
    enable = true,
    _type = "stream",
    streamParams = StreamPrefetchParameters(
      streamCnt = 4,
      streamSize = 4,
      ageWidth = 4,
      blockBytes = L2BlockSize,
      reallocStreamOnMissInstantly = true
    )
  )

  val dcacheParameters = DCacheParameters(
    tagECC = Some("secded"),
    dataECC = Some("secded"),
    nMissEntries = 16,
    nLoadMissEntries = 8,
    nStoreMissEntries = 8
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
  val L3Size = 4 * 1024 * 1024 // 4MB
  val L3BlockSize = 64
  val L3NBanks = 4
  val L3NWays = 8
  val L3NSets = L3Size / L3BlockSize / L3NBanks / L3NWays

  // on chip network configurations
  val L3BusWidth = 256
}

trait HasXSLog { this: RawModule =>
  implicit val moduleName: String = this.name
}

abstract class XSModule extends MultiIOModule
  with HasXSParameter
  with HasExceptionNO
  with HasXSLog
  with HasFPUParameters
{
  def io: Record
}

//remove this trait after impl module logic
trait NeedImpl { this: RawModule =>
  override protected def IO[T <: Data](iodef: T): T = {
    println(s"[Warn]: (${this.name}) please reomve 'NeedImpl' after implement this module")
    val io = chisel3.experimental.IO(iodef)
    io <> DontCare
    io
  }
}

abstract class XSBundle extends Bundle
  with HasXSParameter

case class EnviromentParameters
(
  FPGAPlatform: Boolean = true,
  EnableDebug: Boolean = false
)

object AddressSpace extends HasXSParameter {
  // (start, size)
  // address out of MMIO will be considered as DRAM
  def mmio = List(
    (0x00000000L, 0x40000000L),  // internal devices, such as CLINT and PLIC
    (0x40000000L, 0x40000000L)   // external devices
  )

  def isMMIO(addr: UInt): Bool = mmio.map(range => {
    require(isPow2(range._2))
    val bits = log2Up(range._2)
    (addr ^ range._1.U)(PAddrBits-1, bits) === 0.U
  }).reduce(_ || _)
}



class XSCore()(implicit p: config.Parameters) extends LazyModule
  with HasXSParameter
  with HasExeBlockHelper
{

  // to fast wake up fp, mem rs
  val intBlockFastWakeUpFp = intExuConfigs.filter(fpFastFilter)
  val intBlockSlowWakeUpFp = intExuConfigs.filter(fpSlowFilter)
  val intBlockFastWakeUpInt = intExuConfigs.filter(intFastFilter)
  val intBlockSlowWakeUpInt = intExuConfigs.filter(intSlowFilter)

  val fpBlockFastWakeUpFp = fpExuConfigs.filter(fpFastFilter)
  val fpBlockSlowWakeUpFp = fpExuConfigs.filter(fpSlowFilter)
  val fpBlockFastWakeUpInt = fpExuConfigs.filter(intFastFilter)
  val fpBlockSlowWakeUpInt = fpExuConfigs.filter(intSlowFilter)

  // outer facing nodes
  val l1pluscache = LazyModule(new L1plusCache())
  val ptw = LazyModule(new PTW())
  val l2Prefetcher = LazyModule(new L2Prefetcher())
  val memBlock = LazyModule(new MemBlock(
    fastWakeUpIn = intBlockFastWakeUpInt ++ intBlockFastWakeUpFp ++ fpBlockFastWakeUpInt ++ fpBlockFastWakeUpFp,
    slowWakeUpIn = intBlockSlowWakeUpInt ++ intBlockSlowWakeUpFp ++ fpBlockSlowWakeUpInt ++ fpBlockSlowWakeUpFp,
    fastFpOut = Seq(),
    slowFpOut = loadExuConfigs,
    fastIntOut = Seq(),
    slowIntOut = loadExuConfigs
  ))

  lazy val module = new XSCoreImp(this)
}

class XSCoreImp(outer: XSCore) extends LazyModuleImp(outer)
  with HasXSParameter
  with HasExeBlockHelper
{
  val io = IO(new Bundle {
    val externalInterrupt = new ExternalInterruptIO
  })

  println(s"FPGAPlatform:${env.FPGAPlatform} EnableDebug:${env.EnableDebug}")

  // to fast wake up fp, mem rs
  val intBlockFastWakeUpFp = intExuConfigs.filter(fpFastFilter)
  val intBlockSlowWakeUpFp = intExuConfigs.filter(fpSlowFilter)
  val intBlockFastWakeUpInt = intExuConfigs.filter(intFastFilter)
  val intBlockSlowWakeUpInt = intExuConfigs.filter(intSlowFilter)

  val fpBlockFastWakeUpFp = fpExuConfigs.filter(fpFastFilter)
  val fpBlockSlowWakeUpFp = fpExuConfigs.filter(fpSlowFilter)
  val fpBlockFastWakeUpInt = fpExuConfigs.filter(intFastFilter)
  val fpBlockSlowWakeUpInt = fpExuConfigs.filter(intSlowFilter)

  val frontend = Module(new Frontend)
  val ctrlBlock = Module(new CtrlBlock)
  val integerBlock = Module(new IntegerBlock(
    fastWakeUpIn = fpBlockFastWakeUpInt,
    slowWakeUpIn = fpBlockSlowWakeUpInt ++ loadExuConfigs,
    fastFpOut = intBlockFastWakeUpFp,
    slowFpOut = intBlockSlowWakeUpFp,
    fastIntOut = intBlockFastWakeUpInt,
    slowIntOut = intBlockSlowWakeUpInt
  ))
  val floatBlock = Module(new FloatBlock(
    fastWakeUpIn = intBlockFastWakeUpFp,
    slowWakeUpIn = intBlockSlowWakeUpFp ++ loadExuConfigs,
    fastFpOut = fpBlockFastWakeUpFp,
    slowFpOut = fpBlockSlowWakeUpFp,
    fastIntOut = fpBlockFastWakeUpInt,
    slowIntOut = fpBlockSlowWakeUpInt
  ))

  val memBlock = outer.memBlock.module
  val l1pluscache = outer.l1pluscache.module
  val ptw = outer.ptw.module
  val l2Prefetcher = outer.l2Prefetcher.module

  frontend.io.backend <> ctrlBlock.io.frontend
  frontend.io.sfence <> integerBlock.io.fenceio.sfence
  frontend.io.tlbCsr <> integerBlock.io.csrio.tlb

  frontend.io.icacheMemAcq <> l1pluscache.io.req
  l1pluscache.io.resp <> frontend.io.icacheMemGrant
  l1pluscache.io.flush := frontend.io.l1plusFlush
  frontend.io.fencei := integerBlock.io.fenceio.fencei

  ctrlBlock.io.fromIntBlock <> integerBlock.io.toCtrlBlock
  ctrlBlock.io.fromFpBlock <> floatBlock.io.toCtrlBlock
  ctrlBlock.io.fromLsBlock <> memBlock.io.toCtrlBlock
  ctrlBlock.io.toIntBlock <> integerBlock.io.fromCtrlBlock
  ctrlBlock.io.toFpBlock <> floatBlock.io.fromCtrlBlock
  ctrlBlock.io.toLsBlock <> memBlock.io.fromCtrlBlock

  integerBlock.io.wakeUpIn.fastUops <> floatBlock.io.wakeUpIntOut.fastUops
  integerBlock.io.wakeUpIn.fast <> floatBlock.io.wakeUpIntOut.fast
  integerBlock.io.wakeUpIn.slow <> floatBlock.io.wakeUpIntOut.slow ++ memBlock.io.wakeUpIntOut.slow
  integerBlock.io.toMemBlock <> memBlock.io.fromIntBlock

  floatBlock.io.wakeUpIn.fastUops <> integerBlock.io.wakeUpFpOut.fastUops
  floatBlock.io.wakeUpIn.fast <> integerBlock.io.wakeUpFpOut.fast
  floatBlock.io.wakeUpIn.slow <> integerBlock.io.wakeUpFpOut.slow ++ memBlock.io.wakeUpFpOut.slow
  floatBlock.io.toMemBlock <> memBlock.io.fromFpBlock


  integerBlock.io.wakeUpIntOut.fast.map(_.ready := true.B)
  integerBlock.io.wakeUpIntOut.slow.map(_.ready := true.B)
  floatBlock.io.wakeUpFpOut.fast.map(_.ready := true.B)
  floatBlock.io.wakeUpFpOut.slow.map(_.ready := true.B)

  val wakeUpMem = Seq(
    integerBlock.io.wakeUpIntOut,
    integerBlock.io.wakeUpFpOut,
    floatBlock.io.wakeUpIntOut,
    floatBlock.io.wakeUpFpOut
  )
  memBlock.io.wakeUpIn.fastUops <> wakeUpMem.flatMap(_.fastUops)
  memBlock.io.wakeUpIn.fast <> wakeUpMem.flatMap(w => w.fast.map(f => {
	val raw = WireInit(f)
	raw
  }))
  memBlock.io.wakeUpIn.slow <> wakeUpMem.flatMap(w => w.slow.map(s => {
	val raw = WireInit(s)
	raw
  }))

  integerBlock.io.csrio.fflags <> ctrlBlock.io.roqio.toCSR.fflags
  integerBlock.io.csrio.dirty_fs <> ctrlBlock.io.roqio.toCSR.dirty_fs
  integerBlock.io.csrio.exception <> ctrlBlock.io.roqio.exception
  integerBlock.io.csrio.isInterrupt <> ctrlBlock.io.roqio.isInterrupt
  integerBlock.io.csrio.trapTarget <> ctrlBlock.io.roqio.toCSR.trapTarget
  integerBlock.io.csrio.interrupt <> ctrlBlock.io.roqio.toCSR.intrBitSet
  integerBlock.io.csrio.memExceptionVAddr <> memBlock.io.lsqio.exceptionAddr.vaddr
  integerBlock.io.csrio.externalInterrupt <> io.externalInterrupt
  integerBlock.io.csrio.tlb <> memBlock.io.tlbCsr
  integerBlock.io.fenceio.sfence <> memBlock.io.sfence
  integerBlock.io.fenceio.sbuffer <> memBlock.io.fenceToSbuffer

  floatBlock.io.frm <> integerBlock.io.csrio.frm

  memBlock.io.lsqio.commits <> ctrlBlock.io.roqio.commits
  memBlock.io.lsqio.roqDeqPtr <> ctrlBlock.io.roqio.roqDeqPtr
  memBlock.io.lsqio.exceptionAddr.lsIdx.lqIdx := ctrlBlock.io.roqio.exception.bits.lqIdx
  memBlock.io.lsqio.exceptionAddr.lsIdx.sqIdx := ctrlBlock.io.roqio.exception.bits.sqIdx
  memBlock.io.lsqio.exceptionAddr.isStore := CommitType.lsInstIsStore(ctrlBlock.io.roqio.exception.bits.ctrl.commitType)

  ptw.io.tlb(0) <> memBlock.io.ptw
  ptw.io.tlb(1) <> frontend.io.ptw
  ptw.io.sfence <> integerBlock.io.fenceio.sfence
  ptw.io.csr    <> integerBlock.io.csrio.tlb

  l2Prefetcher.io.in <> memBlock.io.toDCachePrefetch

  if (!env.FPGAPlatform) {
    val debugIntReg, debugFpReg = WireInit(VecInit(Seq.fill(32)(0.U(XLEN.W))))
    ExcitingUtils.addSink(debugIntReg, "DEBUG_INT_ARCH_REG", ExcitingUtils.Debug)
    ExcitingUtils.addSink(debugFpReg, "DEBUG_FP_ARCH_REG", ExcitingUtils.Debug)
    val debugArchReg = WireInit(VecInit(debugIntReg ++ debugFpReg))
    ExcitingUtils.addSource(debugArchReg, "difftestRegs", ExcitingUtils.Debug)
  }

}
