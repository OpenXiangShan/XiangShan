package xiangshan.cache

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import top.DefaultConfig
import utility.{LogUtilsOptions, LogUtilsOptionsKey, PerfCounterOptions, PerfCounterOptionsKey, XSPerfLevel}
import xiangshan.{DFTOptions, DFTOptionsKey, XSCoreParamsKey, XSTileKey}

class DCacheWrapperElabLazy(implicit p: Parameters) extends LazyModule {
  val cache = LazyModule(new DCacheWrapper())
  private val transfer = TransferSizes(cache.cfg.blockBytes)
  val managers = Seq.fill(cache.cfg.numMemChannels)(TLManagerNode(Seq(
    TLSlavePortParameters.v1(
      managers = Seq(TLSlaveParameters.v1(
        address = Seq(AddressSet(0, 0xffffffffffffL)),
        regionType = RegionType.CACHED,
        supportsAcquireT = transfer,
        supportsAcquireB = transfer,
        supportsGet = transfer,
        supportsPutFull = transfer,
        supportsPutPartial = transfer)),
      beatBytes = cache.l1BusDataWidth / 8,
      endSinkId = 1))))

  managers.zip(cache.clientNodes).foreach { case (manager, client) =>
    manager := client
  }

  class DCacheWrapperElabImp extends LazyModuleImp(this) {
    val io = IO(new Bundle {
      val dcache = new DCacheIO
    })
    val cacheImp = cache.module.asInstanceOf[cache.DCacheWrapperImp]
    io.dcache <> cacheImp.io
  }

  lazy val module = new DCacheWrapperElabImp
}

class DCacheWrapperElabModule(implicit p: Parameters) extends Module {
  private val lazyDcache = LazyModule(new DCacheWrapperElabLazy())
  private val dut = Module(lazyDcache.module)
  val io = IO(new Bundle {
    val dcache = new DCacheIO
  })
  io.dcache <> dut.io.dcache
}

class DCacheWrapperElabTest extends AnyFlatSpec with ChiselSim {
  private def config = {
    val base = new DefaultConfig
    base.alterPartial({ case XSCoreParamsKey => base(XSTileKey).head.copy() }).alterPartial({
      case LogUtilsOptionsKey => LogUtilsOptions(false, false, false, false)
      case PerfCounterOptionsKey => PerfCounterOptions(false, false, XSPerfLevel.VERBOSE, 0)
      case DFTOptionsKey => DFTOptions(EnableMbist = false, EnableSramCtl = false)
    })
  }

  behavior of "DCacheWrapper ECC integration"

  it should "elaborate the complete wrapper boundary" in {
    implicit val p = config
    simulate(new DCacheWrapperElabModule) { d =>
      d.io.dcache := DontCare
      d.reset.poke(true.B)
      d.clock.step(2)
      d.reset.poke(false.B)
      d.clock.step()
    }
  }
}
