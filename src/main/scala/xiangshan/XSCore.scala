package xiangshan

import chisel3._
import chisel3.util._
import bus.simplebus._
import noop.{Cache, CacheConfig, HasExceptionNO, TLB, TLBConfig}
import xiangshan.backend._
import xiangshan.backend.dispatch.DP1Config
import xiangshan.backend.exu.ExuConfig
import xiangshan.frontend.Frontend
import xiangshan.utils._

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
  val IBufSize = 64
  val DecodeWidth = 8
  val DecBufSize = 8
  val RenameWidth = 6
  val CommitWidth = 6
  val BrqSize = 16
  val BrTagWidth = log2Up(BrqSize)
  val NRPhyRegs = 128
  val PhyRegIdxWidth = log2Up(NRPhyRegs)
  val NRReadPorts = 14
  val NRWritePorts = 8
  val RoqSize = 32
  val RoqIdxWidth = log2Up(RoqSize)
  val ExtendedRoqIdxWidth = RoqIdxWidth + 1
  val IntDqDeqWidth = 4
  val FpDqDeqWidth = 4
  val LsDqDeqWidth = 4
  val dp1Config = DP1Config(
    IntDqSize = 16,
    FpDqSize = 16,
    LsDqSize = 16
  )
  val exuConfig = ExuConfig(
    AluCnt = 4,
    BruCnt = 1,
    MulCnt = 1,
    MduCnt = 1,
    FmacCnt = 4,
    FmiscCnt = 1,
    FmiscDivSqrtCnt = 1,
    LduCnt = 1,
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

case class XSConfig
(
  FPGAPlatform: Boolean = true,
  EnableDebug: Boolean = true
)

class XSCore(implicit val p: XSConfig) extends XSModule {
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
