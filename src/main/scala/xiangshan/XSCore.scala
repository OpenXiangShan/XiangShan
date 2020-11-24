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
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp, AddressSet}
import freechips.rocketchip.tilelink.{TLBundleParameters, TLCacheCork, TLBuffer, TLClientNode, TLIdentityNode, TLXbar, TLWidthWidget, TLFilter, TLToAXI4}
import freechips.rocketchip.devices.tilelink.{TLError, DevNullParams}
import sifive.blocks.inclusivecache.{CacheParameters, InclusiveCache, InclusiveCacheMicroParameters}
import freechips.rocketchip.amba.axi4.{AXI4ToTL, AXI4IdentityNode, AXI4UserYanker, AXI4Fragmenter, AXI4IdIndexer, AXI4Deinterleaver}
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



class XSCore()(implicit p: config.Parameters) extends LazyModule with HasXSParameter {

  // inner nodes
  val dcache = LazyModule(new DCache())
  val uncache = LazyModule(new Uncache())
  val l1pluscache = LazyModule(new L1plusCache())
  val ptw = LazyModule(new PTW())

  // out facing nodes
  val mem = TLIdentityNode()
  val mmio = uncache.clientNode

  // L1 to L2 network
  // -------------------------------------------------
  private val l2_xbar = TLXbar()

  private val l2 = LazyModule(new InclusiveCache(
    CacheParameters(
      level = 2,
      ways = L2NWays,
      sets = L2NSets,
      blockBytes = L2BlockSize,
      beatBytes = L1BusWidth / 8, // beatBytes = l1BusDataWidth / 8
      cacheName = s"L2"
    ),
    InclusiveCacheMicroParameters(
      writeBytes = 8
    )
  ))

  l2_xbar := TLBuffer() := DebugIdentityNode() := dcache.clientNode
  l2_xbar := TLBuffer() := DebugIdentityNode() := l1pluscache.clientNode
  l2_xbar := TLBuffer() := DebugIdentityNode() := ptw.node
  l2.node := TLBuffer() := DebugIdentityNode() := l2_xbar

  mem := l2.node

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

  frontend.io.backend <> ctrlBlock.io.frontend
  frontend.io.icacheResp <> icache.io.resp
  frontend.io.icacheToTlb <> icache.io.tlb
  icache.io.req <> frontend.io.icacheReq
  icache.io.flush <> frontend.io.icacheFlush
  frontend.io.sfence <> integerBlock.io.fenceio.sfence
  frontend.io.tlbCsr <> integerBlock.io.csrio.tlb

  icache.io.mem_acquire <> l1pluscache.io.req
  l1pluscache.io.resp <> icache.io.mem_grant
  l1pluscache.io.flush := icache.io.l1plusflush
  icache.io.fencei := integerBlock.io.fenceio.fencei

  ctrlBlock.io.fromIntBlock <> integerBlock.io.toCtrlBlock
  ctrlBlock.io.fromFpBlock <> floatBlock.io.toCtrlBlock
  ctrlBlock.io.fromLsBlock <> memBlock.io.toCtrlBlock
  ctrlBlock.io.toIntBlock <> integerBlock.io.fromCtrlBlock
  ctrlBlock.io.toFpBlock <> floatBlock.io.fromCtrlBlock
  ctrlBlock.io.toLsBlock <> memBlock.io.fromCtrlBlock

  integerBlock.io.wakeUpIn.fastUops <> floatBlock.io.wakeUpIntOut.fastUops
  integerBlock.io.wakeUpIn.fast <> floatBlock.io.wakeUpIntOut.fast
  integerBlock.io.wakeUpIn.slow <> floatBlock.io.wakeUpIntOut.slow ++ memBlock.io.wakeUpIntOut.slow

  floatBlock.io.wakeUpIn.fastUops <> integerBlock.io.wakeUpFpOut.fastUops
  floatBlock.io.wakeUpIn.fast <> integerBlock.io.wakeUpFpOut.fast
  floatBlock.io.wakeUpIn.slow <> integerBlock.io.wakeUpFpOut.slow ++ memBlock.io.wakeUpFpOut.slow


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
  memBlock.io.lsqio.oldestStore <> ctrlBlock.io.oldestStore
  memBlock.io.lsqio.exceptionAddr.lsIdx.lqIdx := ctrlBlock.io.roqio.exception.bits.lqIdx
  memBlock.io.lsqio.exceptionAddr.lsIdx.sqIdx := ctrlBlock.io.roqio.exception.bits.sqIdx
  memBlock.io.lsqio.exceptionAddr.isStore := CommitType.lsInstIsStore(ctrlBlock.io.roqio.exception.bits.ctrl.commitType)

  ptw.io.tlb(0) <> memBlock.io.ptw
  ptw.io.tlb(1) <> frontend.io.ptw
  ptw.io.sfence <> integerBlock.io.fenceio.sfence
  ptw.io.csr <> integerBlock.io.csrio.tlb

  dcache.io.lsu.load    <> memBlock.io.dcache.loadUnitToDcacheVec
  dcache.io.lsu.lsq   <> memBlock.io.dcache.loadMiss
  dcache.io.lsu.atomics <> memBlock.io.dcache.atomics
  dcache.io.lsu.store   <> memBlock.io.dcache.sbufferToDcache
  uncache.io.lsq      <> memBlock.io.dcache.uncache

  if (!env.FPGAPlatform) {
    val debugIntReg, debugFpReg = WireInit(VecInit(Seq.fill(32)(0.U(XLEN.W))))
    ExcitingUtils.addSink(debugIntReg, "DEBUG_INT_ARCH_REG", ExcitingUtils.Debug)
    ExcitingUtils.addSink(debugFpReg, "DEBUG_FP_ARCH_REG", ExcitingUtils.Debug)
    val debugArchReg = WireInit(VecInit(debugIntReg ++ debugFpReg))
    ExcitingUtils.addSource(debugArchReg, "difftestRegs", ExcitingUtils.Debug)
  }

}
