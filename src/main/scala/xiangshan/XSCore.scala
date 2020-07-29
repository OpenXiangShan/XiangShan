package xiangshan

import chisel3._
import chisel3.util._
import bus.simplebus._
import noop.{Cache, CacheConfig, HasExceptionNO, TLB, TLBConfig}
import xiangshan.backend._
import xiangshan.backend.dispatch.DP1Parameters
import xiangshan.backend.exu.ExuParameters
import xiangshan.frontend._
import xiangshan.mem._
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
  val CacheLineSize = 512
  val SbufferSize = 16
  val HasFPU = true
  val FetchWidth = 8
  val PredictWidth = FetchWidth * 2
  val EnableBPU = true
  val EnableBPD = false // enable backing predictor(like Tage) in BPUStage3
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
  val exuParameters = ExuParameters(
    JmpCnt = 1,
    AluCnt = 4,
    MulCnt = 1,
    MduCnt = 1,
    FmacCnt = 0,
    FmiscCnt = 0,
    FmiscDivSqrtCnt = 0,
    LduCnt = 2,
    StuCnt = 2
  )
  val NRIntReadPorts = 8
  val NRIntWritePorts = 8
  val NRMemReadPorts = exuParameters.LduCnt + 2*exuParameters.StuCnt
  val NRFpReadPorts = 14
  val NRFpWritePorts = 8
  val MoqSize = 16 // 64
  val RoqSize = 32
  val InnerRoqIdxWidth = log2Up(RoqSize)
  val RoqIdxWidth = InnerRoqIdxWidth + 1
  val InnerMoqIdxWidth = log2Up(MoqSize)
  val MoqIdxWidth = InnerMoqIdxWidth + 1
  val IntDqDeqWidth = 4
  val FpDqDeqWidth = 4
  val LsDqDeqWidth = 4
  val dp1Paremeters = DP1Parameters(
    IntDqSize = 16,
    FpDqSize = 16,
    LsDqSize = 16
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


class XSCore(implicit p: XSConfig) extends XSModule with HasMEMConst {
  val io = IO(new Bundle {
    val imem = new SimpleBusC
    val dmem = new SimpleBusC
    val mmio = new SimpleBusUC
    val frontend = Flipped(new SimpleBusUC())
  })

  io.imem <> DontCare

  val dmemXbar = Module(new SimpleBusCrossbarNto1(n = 2, userBits = DcacheUserBundleWidth))
  
  val front = Module(new Frontend)
  val backend = Module(new Backend)
  val mem = Module(new Memend)

  front.io.backend <> backend.io.frontend
  mem.io.backend   <> backend.io.mem

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
