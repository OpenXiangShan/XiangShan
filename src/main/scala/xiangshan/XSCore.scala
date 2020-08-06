package xiangshan

import chisel3._
import chisel3.util._
import bus.simplebus._
import noop.{Cache, CacheConfig, HasExceptionNO, TLB, TLBConfig}
import top.Parameters
import xiangshan.backend._
import xiangshan.backend.dispatch.DispatchParameters
import xiangshan.backend.exu.ExuParameters
import xiangshan.frontend._
import xiangshan.mem._
import utils._
import xiangshan.mem.cache.DcacheUserBundle

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
  EnableBPD: Boolean = false,
  EnableRAS: Boolean = false,
  EnableLB: Boolean = false,
  HistoryLength: Int = 64,
  BtbSize: Int = 256,
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
  BrqSize: Int = 16,
  IssQueSize: Int = 8,
  NRPhyRegs: Int = 128,
  NRIntReadPorts: Int = 8,
  NRIntWritePorts: Int = 8,
  NRFpReadPorts: Int = 14,
  NRFpWritePorts: Int = 8,
  MoqSize: Int = 16,
  RoqSize: Int = 32,
  IntDqDeqWidth: Int = 4,
  FpDqDeqWidth: Int = 4,
  LsDqDeqWidth: Int = 4,
  dpParams: DispatchParameters = DispatchParameters(
    DqEnqWidth = 4,
    IntDqSize = 64,
    FpDqSize = 64,
    LsDqSize = 64,
    IntDqDeqWidth = 4,
    FpDqDeqWidth = 4,
    LsDqDeqWidth = 4
  ),
  exuParameters: ExuParameters = ExuParameters(
    JmpCnt = 1,
    AluCnt = 4,
    MulCnt = 0,
    MduCnt = 2,
    FmacCnt = 0,
    FmiscCnt = 0,
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
  PtwL2EntrySize: Int = 256
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
  val ExtHistoryLength = HistoryLength * 2
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
  val MoqSize = core.MoqSize // 64
  val RoqSize = core.RoqSize
  val InnerRoqIdxWidth = log2Up(RoqSize)
  val RoqIdxWidth = InnerRoqIdxWidth + 1
  val InnerMoqIdxWidth = log2Up(MoqSize)
  val MoqIdxWidth = InnerMoqIdxWidth + 1
  val IntDqDeqWidth = core.IntDqDeqWidth
  val FpDqDeqWidth = core.FpDqDeqWidth
  val LsDqDeqWidth = core.LsDqDeqWidth
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
}

trait HasXSLog { this: Module =>
  implicit val moduleName: String = this.name
}

abstract class XSModule extends Module
  with HasXSParameter
  with HasExceptionNO
  with HasXSLog

//remove this trait after impl module logic
trait NeedImpl { this: Module =>
  override protected def IO[T <: Data](iodef: T): T = {
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


class XSCore extends XSModule {
  val io = IO(new Bundle {
    val imem = new SimpleBusC
    val dmem = new SimpleBusC
    val mmio = new SimpleBusUC
    val frontend = Flipped(new SimpleBusUC())
  })

  io.imem <> DontCare

  val DcacheUserBundleWidth = (new DcacheUserBundle).getWidth

  val dmemXbar = Module(new SimpleBusCrossbarNto1(n = 2, userBits = DcacheUserBundleWidth))
  
  val front = Module(new Frontend)
  val backend = Module(new Backend)
  val mem = Module(new Memend)

  front.io.backend <> backend.io.frontend
  mem.io.backend   <> backend.io.mem
  mem.io.pmem <> DontCare // NOTE: Ptw mem -> TileLink
  // front.io.mem <> mem.io.frontend // ptw of itlb

  backend.io.memMMU.imem <> DontCare

  val dtlb = TLB(
    in = mem.io.dmem,
    mem = dmemXbar.io.in(1),
    flush = false.B,
    csrMMU = backend.io.memMMU.dmem
  )(TLBConfig(name = "dtlb", totalEntry = 64, userBits = DcacheUserBundleWidth))
  dmemXbar.io.in(0) <> dtlb.io.out
  // dmemXbar.io.in(1) <> io.frontend

  io.frontend <> DontCare

  io.dmem <> Cache(
    in = dmemXbar.io.out,
    mmio = Seq(io.mmio),
    flush = "b00".U,
    empty = dtlb.io.cacheEmpty,
    enable = HasDcache
  )(CacheConfig(name = "dcache", userBits = DcacheUserBundleWidth))

}
