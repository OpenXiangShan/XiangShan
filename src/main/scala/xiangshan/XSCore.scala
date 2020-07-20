package xiangshan

import chisel3._
import chisel3.util._
import bus.simplebus._
import noop.{Cache, CacheConfig, HasExceptionNO, TLB, TLBConfig}
import xiangshan.backend._
import xiangshan.backend.dispatch.DP1Parameters
import xiangshan.backend.exu.ExuParameters
import xiangshan.frontend._
import utils._

trait HasXSParameter {
  val XLEN = 64
  val HasMExtension = true
  val HasCExtension = true
  val HasDiv = true
  val HasIcache = true
  val HasDcache = true
  val EnableStoreQueue = false
  val AddrBits = 64 // AddrBits is used in some cases
  val VAddrBits = 39 // VAddrBits is Virtual Memory addr bits
  val PAddrBits = 32 // PAddrBits is Phyical Memory addr bits
  val AddrBytes = AddrBits / 8 // unused
  val DataBits = XLEN
  val DataBytes = DataBits / 8
  val HasFPU = true
  val FetchWidth = 8
  val PredictWidth = FetchWidth * 2
  val EnableBPU = true
  val EnableBPD = true // enable backing predictor(like Tage) in BPUStage3
  val EnableRAS = false
  val HistoryLength = 64
  val BtbSize = 256
  // val BtbWays = 4
  val BtbBanks = PredictWidth
  // val BtbSets = BtbSize / BtbWays
  val JbtacSize = 1024
  val JbtacBanks = 8
  val RasSize = 16
  val IBufSize = 64
  val DecodeWidth = 6
  val RenameWidth = 6
  val CommitWidth = 6
  val BrqSize = 16
  val IssQueSize = 8
  val BrTagWidth = log2Up(BrqSize)
  val NRPhyRegs = 128
  val PhyRegIdxWidth = log2Up(NRPhyRegs)
  val NRReadPorts = 14
  val NRWritePorts = 8
  val RoqSize = 32
  val InnerRoqIdxWidth = log2Up(RoqSize)
  val RoqIdxWidth = InnerRoqIdxWidth + 1
  val IntDqDeqWidth = 4
  val FpDqDeqWidth = 4
  val LsDqDeqWidth = 4
  val dp1Paremeters = DP1Parameters(
    IntDqSize = 16,
    FpDqSize = 16,
    LsDqSize = 16
  )
  val exuParameters = ExuParameters(
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
  with HasTageParameter

case class XSConfig
(
  FPGAPlatform: Boolean = true,
  EnableDebug: Boolean = true
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


class XSCore(implicit p: XSConfig) extends XSModule {
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
