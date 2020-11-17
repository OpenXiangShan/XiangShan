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
import xiangshan.cache.{ICache, DCache, L1plusCache, DCacheParameters, ICacheParameters, L1plusCacheParameters, PTW, Uncache}
import chipsalliance.rocketchip.config
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink.{TLBundleParameters, TLCacheCork, TLBuffer, TLClientNode, TLIdentityNode, TLXbar}
import sifive.blocks.inclusivecache.{CacheParameters, InclusiveCache, InclusiveCacheMicroParameters}
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
  EnableLB: Boolean = true,
  EnableLoop: Boolean = false,
  HistoryLength: Int = 64,
  BtbSize: Int = 2048,
  JbtacSize: Int = 1024,
  JbtacBanks: Int = 8,
  RasSize: Int = 16,
  CacheLineSize: Int = 512,
  UBtbWays: Int = 16,
  BtbWays: Int = 2,
  IBufSize: Int = 64,
  DecodeWidth: Int = 6,
  RenameWidth: Int = 6,
  CommitWidth: Int = 6,
  BrqSize: Int = 12,
  IssQueSize: Int = 8,
  NRPhyRegs: Int = 128,
  NRIntReadPorts: Int = 14,
  NRIntWritePorts: Int = 8,
  NRFpReadPorts: Int = 14,
  NRFpWritePorts: Int = 8, 
  EnableUnifiedLSQ: Boolean = false,
  LsroqSize: Int = 16,
  LoadQueueSize: Int = 12,
  StoreQueueSize: Int = 10,
  RoqSize: Int = 32,
  dpParams: DispatchParameters = DispatchParameters(
    DqEnqWidth = 4,
    IntDqSize = 24,
    FpDqSize = 16,
    LsDqSize = 16,
    IntDqDeqWidth = 4,
    FpDqDeqWidth = 4,
    LsDqDeqWidth = 4,
    IntDqReplayWidth = 4,
    FpDqReplayWidth = 4,
    LsDqReplayWidth = 4
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
  NumPerfCounters: Int = 16
)

trait HasXSParameter {

  val core = Parameters.get.coreParameters
  val env = Parameters.get.envParameters

  val XLEN = core.XLEN
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
  val PredictWidth = FetchWidth * 2
  val EnableBPU = core.EnableBPU
  val EnableBPD = core.EnableBPD // enable backing predictor(like Tage) in BPUStage3
  val EnableRAS = core.EnableRAS
  val EnableLB = core.EnableLB
  val EnableLoop = core.EnableLoop
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
  val EnableUnifiedLSQ = core.EnableUnifiedLSQ
  val LsroqSize = core.LsroqSize // 64
  val InnerLsroqIdxWidth = log2Up(LsroqSize)
  val LsroqIdxWidth = InnerLsroqIdxWidth + 1
  val LoadQueueSize = core.LoadQueueSize
  val StoreQueueSize = core.StoreQueueSize
  val dpParams = core.dpParams
  val ReplayWidth = dpParams.IntDqReplayWidth + dpParams.FpDqReplayWidth + dpParams.LsDqReplayWidth
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

  val l1BusDataWidth = 256

  val icacheParameters = ICacheParameters(
    nMissEntries = 2
  )

  val l1plusCacheParameters = L1plusCacheParameters(
    tagECC = Some("secded"),
    dataECC = Some("secded"),
    nMissEntries = 8
  )

  val dcacheParameters = DCacheParameters(
    tagECC = Some("secded"),
    dataECC = Some("secded"),
    nMissEntries = 16,
    nLoadMissEntries = 8,
    nStoreMissEntries = 8
  )

  val LRSCCycles = 100
}

trait HasXSLog { this: RawModule =>
  implicit val moduleName: String = this.name
}

abstract class XSModule extends MultiIOModule
  with HasXSParameter
  with HasExceptionNO
  with HasXSLog
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
    (0x30000000L, 0x10000000L),  // internal devices, such as CLINT and PLIC
    (0x40000000L, 0x40000000L) // external devices
  )

  def isMMIO(addr: UInt): Bool = mmio.map(range => {
    require(isPow2(range._2))
    val bits = log2Up(range._2)
    (addr ^ range._1.U)(PAddrBits-1, bits) === 0.U
  }).reduce(_ || _)
}



class XSCore()(implicit p: config.Parameters) extends LazyModule {

  val dcache = LazyModule(new DCache())
  val uncache = LazyModule(new Uncache())
  val l1pluscache = LazyModule(new L1plusCache())
  val ptw = LazyModule(new PTW())

  val mem = TLIdentityNode()
  val mmio = uncache.clientNode

  // TODO: refactor these params
  private val l2 = LazyModule(new InclusiveCache(
    CacheParameters(
      level = 2,
      ways = 4,
      sets = 512 * 1024 / (64 * 4),
      blockBytes = 64,
      beatBytes = 32 // beatBytes = l1BusDataWidth / 8
    ),
    InclusiveCacheMicroParameters(
      writeBytes = 8
    )
  ))

  private val xbar = TLXbar()

  xbar := TLBuffer() := DebugIdentityNode() := dcache.clientNode
  xbar := TLBuffer() := DebugIdentityNode() := l1pluscache.clientNode
  xbar := TLBuffer() := DebugIdentityNode() := ptw.node

  l2.node := xbar

  mem := TLBuffer() := TLCacheCork() := TLBuffer() := l2.node

  lazy val module = new XSCoreImp(this)
}

class XSCoreImp(outer: XSCore) extends LazyModuleImp(outer)
  with HasXSParameter
  with HasExeBlockHelper
{
  val io = IO(new Bundle {
    val externalInterrupt = new ExternalInterruptIO
  })

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
  val memBlock = Module(new MemBlock(
    fastWakeUpIn = intBlockFastWakeUpInt ++ intBlockFastWakeUpFp ++ fpBlockFastWakeUpInt ++ fpBlockFastWakeUpFp,
    slowWakeUpIn = intBlockSlowWakeUpInt ++ intBlockSlowWakeUpFp ++ fpBlockSlowWakeUpInt ++ fpBlockSlowWakeUpFp,
    fastFpOut = Seq(),
    slowFpOut = loadExuConfigs,
    fastIntOut = Seq(),
    slowIntOut = loadExuConfigs
  ))

  val dcache = outer.dcache.module
  val uncache = outer.uncache.module
  val l1pluscache = outer.l1pluscache.module
  val ptw = outer.ptw.module
  val icache = Module(new ICache)

  //TODO: remove following code
  memBlock.io <> DontCare
  integerBlock.io <> DontCare
  floatBlock.io <> DontCare


  frontend.io.backend <> ctrlBlock.io.frontend
  frontend.io.icacheResp <> icache.io.resp
  frontend.io.icacheToTlb <> icache.io.tlb
  icache.io.req <> frontend.io.icacheReq
  icache.io.flush <> frontend.io.icacheFlush

  icache.io.mem_acquire <> l1pluscache.io.req
  l1pluscache.io.resp <> icache.io.mem_grant
  l1pluscache.io.flush := icache.io.l1plusflush
  icache.io.fencei := integerBlock.io.fencei

  ctrlBlock.io.fromIntBlock <> integerBlock.io.toCtrlBlock
  ctrlBlock.io.fromFpBlock <> floatBlock.io.toCtrlBlock
  ctrlBlock.io.fromLsBlock <> memBlock.io.toCtrlBlock
  ctrlBlock.io.toIntBlock <> integerBlock.io.fromCtrlBlock
  ctrlBlock.io.toFpBlock <> floatBlock.io.fromCtrlBlock
  ctrlBlock.io.toLsBlock <> memBlock.io.fromCtrlBlock

  integerBlock.io.wakeUpIn.fast <> floatBlock.io.wakeUpIntOut.fast
  integerBlock.io.wakeUpIn.slow <> floatBlock.io.wakeUpIntOut.slow ++ memBlock.io.wakeUpIntOut.slow

  floatBlock.io.wakeUpIn.fast <> integerBlock.io.wakeUpFpOut.fast
  floatBlock.io.wakeUpIn.slow <> integerBlock.io.wakeUpFpOut.slow ++ memBlock.io.wakeUpFpOut.slow

  memBlock.io.wakeUpIn.fast <> integerBlock.io.wakeUpIntOut.fast ++
    integerBlock.io.wakeUpFpOut.fast ++
    floatBlock.io.wakeUpIntOut.fast ++
    floatBlock.io.wakeUpFpOut.fast

  memBlock.io.wakeUpIn.slow <> integerBlock.io.wakeUpIntOut.slow ++
    integerBlock.io.wakeUpFpOut.slow ++
    floatBlock.io.wakeUpIntOut.slow ++
    floatBlock.io.wakeUpFpOut.slow

  integerBlock.io.csrOnly.memExceptionVAddr := memBlock.io.csr.exceptionAddr.vaddr
  integerBlock.io.csrOnly.externalInterrupt := io.externalInterrupt
  integerBlock.io.csrOnly.isInterrupt := DontCare //TODO: fix it


  io.externalInterrupt <> integerBlock.io.externalInterrupt

  ptw.io.tlb(0) <> memBlock.io.ptw
  ptw.io.tlb(1) <> frontend.io.ptw
  ptw.io.sfence <> integerBlock.io.sfence
  ptw.io.csr <> integerBlock.io.tlbCsrIO

  dcache.io.lsu.load    <> memBlock.io.dcache.loadUnitToDcacheVec
  dcache.io.lsu.lsroq   <> memBlock.io.dcache.loadMiss
  dcache.io.lsu.atomics <> memBlock.io.dcache.atomics
  dcache.io.lsu.store   <> memBlock.io.dcache.sbufferToDcache
  uncache.io.lsroq      <> memBlock.io.dcache.uncache

  val debugIntReg, debugFpReg = WireInit(VecInit(Seq.fill(32)(0.U(XLEN.W))))
  ExcitingUtils.addSink(debugIntReg, "DEBUG_INT_ARCH_REG", ExcitingUtils.Debug)
  ExcitingUtils.addSink(debugFpReg, "DEBUG_FP_ARCH_REG", ExcitingUtils.Debug)
  val debugArchReg = WireInit(VecInit(debugIntReg ++ debugFpReg))
  if (!env.FPGAPlatform) {
    ExcitingUtils.addSource(debugArchReg, "difftestRegs", ExcitingUtils.Debug)
  }


}
