package xiangshan

import chisel3._
import chisel3.util._
import bus.simplebus._
import noop.{Cache, CacheConfig, HasExceptionNO, TLB, TLBConfig}
import top.Parameters
import xiangshan.backend._
import xiangshan.backend.dispatch.DP1Parameters
import xiangshan.backend.exu.ExuParameters
import xiangshan.frontend._
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
  PAddrBits: Int = 32,
  HasFPU: Boolean = true,
  FectchWidth: Int = 8,
  EnableBPU: Boolean = true,
  EnableBPD: Boolean = true,
  EnableRAS: Boolean = true,
  EnableLB: Boolean = false,
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
  BrqSize: Int = 16,
  IssQueSize: Int = 8,
  NRPhyRegs: Int = 128,
  NRReadPorts: Int = 14,
  NRWritePorts: Int = 8,
  RoqSize: Int = 32,
  IntDqDeqWidth: Int = 4,
  FpDqDeqWidth: Int = 4,
  LsDqDeqWidth: Int = 4,
  dp1Paremeters: DP1Parameters = DP1Parameters(
    IntDqSize = 16,
    FpDqSize = 16,
    LsDqSize = 16
  ),
  exuParameters: ExuParameters = ExuParameters(
    JmpCnt = 1,
    AluCnt = 4,
    MulCnt = 1,
    MduCnt = 1,
    FmacCnt = 0,
    FmiscCnt = 0,
    FmiscDivSqrtCnt = 0,
    LduCnt = 0,
    StuCnt = 1
  )
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
  val BtbBanks = PredictWidth
  val JbtacSize = core.JbtacSize
  val JbtacBanks = core.JbtacBanks
  val RasSize = core.RasSize
  val IBufSize = core.IBufSize
  val DecodeWidth = core.DecodeWidth
  val RenameWidth = core.RenameWidth
  val CommitWidth = core.CommitWidth
  val BrqSize = core.BrqSize
  val IssQueSize = core.IssQueSize
  val BrTagWidth = log2Up(BrqSize)
  val NRPhyRegs = core.NRPhyRegs
  val PhyRegIdxWidth = log2Up(NRPhyRegs)
  val NRReadPorts = core.NRReadPorts
  val NRWritePorts = core.NRWritePorts
  val RoqSize = core.RoqSize
  val InnerRoqIdxWidth = log2Up(RoqSize)
  val RoqIdxWidth = InnerRoqIdxWidth + 1
  val IntDqDeqWidth = core.IntDqDeqWidth
  val FpDqDeqWidth = core.FpDqDeqWidth
  val LsDqDeqWidth = core.LsDqDeqWidth
  val dp1Paremeters = core.dp1Paremeters
  val exuParameters = core.exuParameters
  val CacheLineSize = core.CacheLineSize
  val CacheLineHalfWord = CacheLineSize / 16
  val ExtHistoryLength = HistoryLength * 2
  val UBtbWays = core.UBtbWays
  val BtbWays = core.BtbWays
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

  val dmemXbar = Module(new SimpleBusCrossbarNto1(3))

  val front = Module(new Frontend)
  val backend = Module(new Backend)

  front.io.backend <> backend.io.frontend

  backend.io.memMMU.imem <> DontCare

  val dtlb = TLB(
    in = backend.io.dmem,
    mem = dmemXbar.io.in(1),
    flush = false.B,
    csrMMU = backend.io.memMMU.dmem
  )(TLBConfig(name = "dtlb", totalEntry = 64))
  dmemXbar.io.in(0) <> dtlb.io.out
  dmemXbar.io.in(2) <> io.frontend

  io.dmem <> Cache(
    in = dmemXbar.io.out,
    mmio = Seq(io.mmio),
    flush = "b00".U,
    empty = dtlb.io.cacheEmpty,
    enable = HasDcache
  )(CacheConfig(name = "dcache"))

  XSDebug("(req valid, ready | resp valid, ready) \n")
  XSDebug("c-mem(%x %x %x| %x %x) c-coh(%x %x %x| %x %x) cache (%x %x %x| %x %x) tlb (%x %x %x| %x %x)\n",
    io.dmem.mem.req.valid,
    io.dmem.mem.req.ready,
    io.dmem.mem.req.bits.addr,
    io.dmem.mem.resp.valid,
    io.dmem.mem.resp.ready,
    io.dmem.coh.req.valid,
    io.dmem.coh.req.ready,
    io.dmem.coh.req.bits.addr,
    io.dmem.coh.resp.valid,
    io.dmem.coh.resp.ready,
    dmemXbar.io.out.req.valid,
    dmemXbar.io.out.req.ready,
    dmemXbar.io.out.req.bits.addr,
    dmemXbar.io.out.resp.valid,
    dmemXbar.io.out.resp.ready,
    backend.io.dmem.req.valid,
    backend.io.dmem.req.ready,
    backend.io.dmem.req.bits.addr,
    backend.io.dmem.resp.valid,
    backend.io.dmem.resp.ready
  )
}
