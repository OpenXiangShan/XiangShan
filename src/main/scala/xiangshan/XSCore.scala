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
import xiangshan.cache.{DCacheParameters, ICacheParameters, L1plusCache, L1plusCacheParameters, PTW, PTWRepeater}
import xiangshan.cache.prefetch._
import chipsalliance.rocketchip.config
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.tile.HasFPUParameters
import system.L1CacheErrorInfo
import utils._

object hartIdCore extends (() => Int) {
  var x = 0

  def apply(): Int = {
    x = x + 1
    x - 1
  }
}

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
  IBufSize: Int = 32,
  DecodeWidth: Int = 6,
  RenameWidth: Int = 6,
  CommitWidth: Int = 6,
  BrqSize: Int = 32,
  FtqSize: Int = 48,
  EnableLoadFastWakeUp: Boolean = true, // NOTE: not supported now, make it false
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
  NrExtIntr: Int = 150
)

trait HasXSParameter {

  val coreParams = Parameters.get.coreParameters
  val env = Parameters.get.envParameters

  val XLEN = 64
  val minFLen = 32
  val fLen = 64

  def xLen = 64

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
  val NrExtIntr = coreParams.NrExtIntr

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
  val L3Size = 4 * 1024 * 1024 // 4MB
  val L3BlockSize = 64
  val L3NBanks = 4
  val L3NWays = 8
  val L3NSets = L3Size / L3BlockSize / L3NBanks / L3NWays

  // on chip network configurations
  val L3BusWidth = 256

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
}

trait HasXSLog {
  this: RawModule =>
  implicit val moduleName: String = this.name
}

abstract class XSModule extends MultiIOModule
  with HasXSParameter
  with HasExceptionNO
  with HasXSLog
  with HasFPUParameters {
  def io: Record
}

//remove this trait after impl module logic
trait NeedImpl {
  this: RawModule =>
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
  EnableDebug: Boolean = false,
  EnablePerfDebug: Boolean = true,
  DualCore: Boolean = false
)

class XSCore()(implicit p: config.Parameters) extends LazyModule
  with HasXSParameter
  with HasExeBlockHelper {
  // outer facing nodes
  val frontend = LazyModule(new Frontend())
  val l1pluscache = LazyModule(new L1plusCache())
  val ptw = LazyModule(new PTW())
  val memBlock = LazyModule(new MemBlock(
    fastWakeUpIn = intExuConfigs.filter(_.hasCertainLatency),
    slowWakeUpIn = intExuConfigs.filter(_.hasUncertainlatency) ++ fpExuConfigs,
    fastWakeUpOut = Seq(),
    slowWakeUpOut = loadExuConfigs,
    numIntWakeUpFp = intExuConfigs.count(_.writeFpRf)
  ))

  lazy val module = new XSCoreImp(this)
}

class XSCoreImp(outer: XSCore) extends LazyModuleImp(outer)
  with HasXSParameter
  with HasExeBlockHelper {
  val io = IO(new Bundle {
    val hartId = Input(UInt(64.W))
    val externalInterrupt = new ExternalInterruptIO
    val l2_pf_enable = Output(Bool())
    val l1plus_error, icache_error, dcache_error = Output(new L1CacheErrorInfo)
  })

  val difftestIO = IO(new DifftestBundle())
  difftestIO <> DontCare

  val trapIO = IO(new TrapIO())
  trapIO <> DontCare

  println(s"FPGAPlatform:${env.FPGAPlatform} EnableDebug:${env.EnableDebug}")
  AddressSpace.checkMemmap()
  AddressSpace.printMemmap()

  // to fast wake up fp, mem rs
  val intBlockFastWakeUp = intExuConfigs.filter(_.hasCertainLatency)
  val intBlockSlowWakeUp = intExuConfigs.filter(_.hasUncertainlatency)

  val ctrlBlock = Module(new CtrlBlock)
  val integerBlock = Module(new IntegerBlock(
    fastWakeUpIn = Seq(),
    slowWakeUpIn = fpExuConfigs.filter(_.writeIntRf) ++ loadExuConfigs,
    memFastWakeUpIn  = loadExuConfigs,
    fastWakeUpOut = intBlockFastWakeUp,
    slowWakeUpOut = intBlockSlowWakeUp
  ))
  val floatBlock = Module(new FloatBlock(
    intSlowWakeUpIn = intExuConfigs.filter(_.writeFpRf),
    memSlowWakeUpIn = loadExuConfigs,
    fastWakeUpOut = Seq(),
    slowWakeUpOut = fpExuConfigs
  ))

  val frontend = outer.frontend.module
  val memBlock = outer.memBlock.module
  val l1pluscache = outer.l1pluscache.module
  val ptw = outer.ptw.module

  io.l1plus_error <> l1pluscache.io.error
  io.icache_error <> frontend.io.error
  io.dcache_error <> memBlock.io.error

  frontend.io.backend <> ctrlBlock.io.frontend
  frontend.io.sfence <> integerBlock.io.fenceio.sfence
  frontend.io.tlbCsr <> integerBlock.io.csrio.tlb
  frontend.io.csrCtrl <> integerBlock.io.csrio.customCtrl

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
  ctrlBlock.io.csrCtrl <> integerBlock.io.csrio.customCtrl

  val memBlockWakeUpInt = memBlock.io.wakeUpOutInt.slow.map(WireInit(_))
  val memBlockWakeUpFp = memBlock.io.wakeUpOutFp.slow.map(WireInit(_))
  memBlock.io.wakeUpOutInt.slow.foreach(_.ready := true.B)
  memBlock.io.wakeUpOutFp.slow.foreach(_.ready := true.B)

  fpExuConfigs.zip(floatBlock.io.wakeUpOut.slow).filterNot(_._1.writeIntRf).map(_._2.ready := true.B)
  val fpBlockWakeUpInt = fpExuConfigs
    .zip(floatBlock.io.wakeUpOut.slow)
    .filter(_._1.writeIntRf)
    .map(_._2)

  intExuConfigs.zip(integerBlock.io.wakeUpOut.slow).filterNot(_._1.writeFpRf).map(_._2.ready := true.B)
  val intBlockWakeUpFp = intExuConfigs.filter(_.hasUncertainlatency)
    .zip(integerBlock.io.wakeUpOut.slow)
    .filter(_._1.writeFpRf)
    .map(_._2)

  integerBlock.io.wakeUpIn.slow <> fpBlockWakeUpInt ++ memBlockWakeUpInt
  integerBlock.io.toMemBlock <> memBlock.io.fromIntBlock
  integerBlock.io.memFastWakeUp <> memBlock.io.ldFastWakeUpInt

  floatBlock.io.intWakeUpFp <> intBlockWakeUpFp
  floatBlock.io.memWakeUpFp <> memBlockWakeUpFp
  floatBlock.io.toMemBlock <> memBlock.io.fromFpBlock

  val wakeUpMem = Seq(
    integerBlock.io.wakeUpOut,
    floatBlock.io.wakeUpOut,
  )
  memBlock.io.wakeUpIn.fastUops <> wakeUpMem.flatMap(_.fastUops)
  memBlock.io.wakeUpIn.fast <> wakeUpMem.flatMap(_.fast)
  // Note: 'WireInit' is used to block 'ready's from memBlock,
  // we don't need 'ready's from memBlock
  memBlock.io.wakeUpIn.slow <> wakeUpMem.flatMap(_.slow.map(x => WireInit(x)))
  memBlock.io.intWakeUpFp <> floatBlock.io.intWakeUpOut

  integerBlock.io.csrio.hartId <> io.hartId
  integerBlock.io.csrio.perf <> DontCare
  integerBlock.io.csrio.perf.retiredInstr <> ctrlBlock.io.roqio.toCSR.perfinfo.retiredInstr
  integerBlock.io.csrio.fpu.fflags <> ctrlBlock.io.roqio.toCSR.fflags
  integerBlock.io.csrio.fpu.isIllegal := false.B
  integerBlock.io.csrio.fpu.dirty_fs <> ctrlBlock.io.roqio.toCSR.dirty_fs
  integerBlock.io.csrio.fpu.frm <> floatBlock.io.frm
  integerBlock.io.csrio.exception <> ctrlBlock.io.roqio.exception
  integerBlock.io.csrio.isXRet <> ctrlBlock.io.roqio.toCSR.isXRet
  integerBlock.io.csrio.trapTarget <> ctrlBlock.io.roqio.toCSR.trapTarget
  integerBlock.io.csrio.interrupt <> ctrlBlock.io.roqio.toCSR.intrBitSet
  integerBlock.io.csrio.memExceptionVAddr <> memBlock.io.lsqio.exceptionAddr.vaddr
  integerBlock.io.csrio.externalInterrupt <> io.externalInterrupt

  integerBlock.io.fenceio.sfence <> memBlock.io.sfence
  integerBlock.io.fenceio.sbuffer <> memBlock.io.fenceToSbuffer

  memBlock.io.csrCtrl <> integerBlock.io.csrio.customCtrl
  memBlock.io.tlbCsr <> integerBlock.io.csrio.tlb
  memBlock.io.lsqio.roq <> ctrlBlock.io.roqio.lsq
  memBlock.io.lsqio.exceptionAddr.lsIdx.lqIdx := ctrlBlock.io.roqio.exception.bits.uop.lqIdx
  memBlock.io.lsqio.exceptionAddr.lsIdx.sqIdx := ctrlBlock.io.roqio.exception.bits.uop.sqIdx
  memBlock.io.lsqio.exceptionAddr.isStore := CommitType.lsInstIsStore(ctrlBlock.io.roqio.exception.bits.uop.ctrl.commitType)

  val itlbRepeater = Module(new PTWRepeater())
  val dtlbRepeater = Module(new PTWRepeater())
  itlbRepeater.io.tlb <> frontend.io.ptw
  dtlbRepeater.io.tlb <> memBlock.io.ptw
  itlbRepeater.io.sfence <> integerBlock.io.fenceio.sfence
  dtlbRepeater.io.sfence <> integerBlock.io.fenceio.sfence
  ptw.io.tlb(0) <> dtlbRepeater.io.ptw
  ptw.io.tlb(1) <> itlbRepeater.io.ptw
  ptw.io.sfence <> integerBlock.io.fenceio.sfence
  ptw.io.csr <> integerBlock.io.csrio.tlb

  // if l2 prefetcher use stream prefetch, it should be placed in XSCore
  assert(l2PrefetcherParameters._type == "bop")
  io.l2_pf_enable := integerBlock.io.csrio.customCtrl.l2_pf_enable

  if (!env.FPGAPlatform) {
    val id = hartIdCore()
    difftestIO.fromSbuffer <> memBlock.difftestIO.fromSbuffer
    difftestIO.fromSQ <> memBlock.difftestIO.fromSQ
    difftestIO.fromCSR <> integerBlock.difftestIO.fromCSR
    difftestIO.fromRoq <> ctrlBlock.difftestIO.fromRoq
    difftestIO.fromAtomic <> memBlock.difftestIO.fromAtomic
    difftestIO.fromPtw <> ptw.difftestIO
    trapIO <> ctrlBlock.trapIO

    val debugIntReg, debugFpReg = WireInit(VecInit(Seq.fill(32)(0.U(XLEN.W))))
    ExcitingUtils.addSink(debugIntReg, s"DEBUG_INT_ARCH_REG$id", ExcitingUtils.Debug)
    ExcitingUtils.addSink(debugFpReg, s"DEBUG_FP_ARCH_REG$id", ExcitingUtils.Debug)
    val debugArchReg = WireInit(VecInit(debugIntReg ++ debugFpReg))
    difftestIO.fromXSCore.r := debugArchReg
  }

}
