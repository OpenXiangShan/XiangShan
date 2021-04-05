package top

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import system._
import chisel3.stage.ChiselGeneratorAnnotation
import chipsalliance.rocketchip.config
import chipsalliance.rocketchip.config.Config
import device.{AXI4Plic, TLTimer}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.devices.tilelink.{DevNullParams, TLError}
import freechips.rocketchip.diplomaticobjectmodel.logicaltree.GenericLogicalTreeNode
import freechips.rocketchip.interrupts.{IntSinkNode, IntSinkPortSimple}
import freechips.rocketchip.tile.{BusErrorUnit, BusErrorUnitParams, XLen}
import sifive.blocks.inclusivecache.{InclusiveCache, InclusiveCacheMicroParameters, CacheParameters}
import xiangshan.cache.prefetch.L2Prefetcher


class XSCoreWithL2()(implicit p: config.Parameters) extends LazyModule
  with HasXSParameter {
  private val core = LazyModule(new XSCore())
  private val l2prefetcher = LazyModule(new L2Prefetcher())
  private val l2xbar = TLXbar()

  val l2cache = LazyModule(new InclusiveCache(
    CacheParameters(
      level = 2,
      ways = L2NWays,
      sets = L2NSets,
      blockBytes = L2BlockSize,
      beatBytes = L1BusWidth / 8, // beatBytes = l1BusDataWidth / 8
      cacheName = s"L2",
      uncachedGet = true,
      enablePerf = false
    ),
    InclusiveCacheMicroParameters(
      memCycles = 25,
      writeBytes = 32
    ),
    fpga = env.FPGAPlatform
  ))
  val uncache = TLXbar()

  l2xbar := TLBuffer() := core.memBlock.dcache.clientNode
  l2xbar := TLBuffer() := core.l1pluscache.clientNode
  l2xbar := TLBuffer() := core.ptw.node
  l2xbar := TLBuffer() := l2prefetcher.clientNode
  l2cache.node := TLBuffer() := l2xbar

  uncache := TLBuffer() := core.frontend.instrUncache.clientNode
  uncache := TLBuffer() := core.memBlock.uncache.clientNode

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val hartId = Input(UInt(64.W))
      val externalInterrupt = new ExternalInterruptIO
      val l1plus_error, icache_error, dcache_error = new L1CacheErrorInfo
    })

    core.module.io.hartId := io.hartId
    core.module.io.externalInterrupt := io.externalInterrupt
    l2prefetcher.module.io.enable := core.module.io.l2_pf_enable
    l2prefetcher.module.io.in <> l2cache.module.io
    io.l1plus_error <> core.module.io.l1plus_error
    io.icache_error <> core.module.io.icache_error
    io.dcache_error <> core.module.io.dcache_error

    val core_reset_gen = Module(new ResetGen())
    core.module.reset := core_reset_gen.io.out

    val l2_reset_gen = Module(new ResetGen())
    l2prefetcher.module.reset := l2_reset_gen.io.out
    l2cache.module.reset := l2_reset_gen.io.out
  }
}

abstract class BaseXSSoc()(implicit p: config.Parameters) extends LazyModule with HasSoCParameter {
  val bankedNode = BankBinder(L3NBanks, L3BlockSize)
  val peripheralXbar = TLXbar()
  val l3_xbar = TLXbar()
}

// We adapt the following three traits from rocket-chip.
// Source: rocket-chip/src/main/scala/subsystem/Ports.scala
trait HaveSlaveAXI4Port {
  this: BaseXSSoc =>

  val idBits = 16

  val l3FrontendAXI4Node = AXI4MasterNode(Seq(AXI4MasterPortParameters(
    Seq(AXI4MasterParameters(
      name = "dma",
      id = IdRange(0, 1 << idBits)
    ))
  )))
  private val errorDevice = LazyModule(new TLError(
    params = DevNullParams(
      address = Seq(AddressSet(0x0, 0x7fffffffL)),
      maxAtomic = 8,
      maxTransfer = 64),
    beatBytes = L2BusWidth / 8
  ))
  private val error_xbar = TLXbar()

  error_xbar :=
    AXI4ToTL() :=
    AXI4UserYanker(Some(1)) :=
    AXI4Fragmenter() :=
    AXI4IdIndexer(1) :=
    l3FrontendAXI4Node
  errorDevice.node := error_xbar
  l3_xbar :=
    TLBuffer() :=
    error_xbar

  val dma = InModuleBody {
    l3FrontendAXI4Node.makeIOs()
  }
}

trait HaveAXI4MemPort {
  this: BaseXSSoc =>
  // 40-bit physical address
  val memRange = AddressSet(0x00000000L, 0xffffffffffL).subtract(AddressSet(0x0L, 0x7fffffffL))
  val memAXI4SlaveNode = AXI4SlaveNode(Seq(
    AXI4SlavePortParameters(
      slaves = Seq(
        AXI4SlaveParameters(
          address = memRange,
          regionType = RegionType.UNCACHED,
          executable = true,
          supportsRead = TransferSizes(1, L3BlockSize),
          supportsWrite = TransferSizes(1, L3BlockSize),
          interleavedId = Some(0)
        )
      ),
      beatBytes = L3BusWidth / 8
    )
  ))

  val mem_xbar = TLXbar()
  mem_xbar :=* TLBuffer() :=* TLCacheCork() :=* bankedNode
  memAXI4SlaveNode :=
    AXI4UserYanker() :=
    AXI4Deinterleaver(L3BlockSize) :=
    TLToAXI4() :=
    TLWidthWidget(L3BusWidth / 8) :=
    mem_xbar

  val memory = InModuleBody {
    memAXI4SlaveNode.makeIOs()
  }
}


trait HaveAXI4PeripheralPort { this: BaseXSSoc =>
  // on-chip devices: 0x3800_000 - 0x3fff_ffff
  val onChipPeripheralRange = AddressSet(0x38000000L, 0x07ffffffL)
  val peripheralRange = AddressSet(0x0, 0x7fffffff).subtract(onChipPeripheralRange)
  val peripheralNode = AXI4SlaveNode(Seq(AXI4SlavePortParameters(
    Seq(AXI4SlaveParameters(
      address = peripheralRange,
      regionType = RegionType.UNCACHED,
      supportsRead = TransferSizes(1, 8),
      supportsWrite = TransferSizes(1, 8),
      interleavedId = Some(0)
    )),
    beatBytes = 8
  )))

  peripheralNode :=
    AXI4UserYanker() :=
    AXI4Deinterleaver(8) :=
    TLToAXI4() :=
    peripheralXbar

  val peripheral = InModuleBody {
    peripheralNode.makeIOs()
  }

}


class XSTop()(implicit p: config.Parameters) extends BaseXSSoc()
  with HaveAXI4MemPort
  with HaveAXI4PeripheralPort
  with HaveSlaveAXI4Port
{

  println(s"FPGASoC cores: $NumCores banks: $L3NBanks block size: $L3BlockSize bus size: $L3BusWidth")

  val core_with_l2 = Seq.fill(NumCores)(LazyModule(new XSCoreWithL2))

  for (i <- 0 until NumCores) {
    peripheralXbar := TLBuffer() := core_with_l2(i).uncache
    l3_xbar := TLBuffer() := core_with_l2(i).l2cache.node
  }

  private val clint = LazyModule(new TLTimer(
    Seq(AddressSet(0x38000000L, 0x0000ffffL)),
    sim = !env.FPGAPlatform
  ))
  clint.node := peripheralXbar

  val fakeTreeNode = new GenericLogicalTreeNode
  val beu = LazyModule(
    new BusErrorUnit(new XSL1BusErrors(NumCores), BusErrorUnitParams(0x38010000), fakeTreeNode))
  beu.node := peripheralXbar

  class BeuSinkNode()(implicit p: config.Parameters) extends LazyModule {
    val intSinkNode = IntSinkNode(IntSinkPortSimple())
    lazy val module = new LazyModuleImp(this){
      val interrupt = IO(Output(Bool()))
      interrupt := intSinkNode.in.head._1.head
    }
  }
  val beuSink = LazyModule(new BeuSinkNode())
  beuSink.intSinkNode := beu.intNode

  val plic = LazyModule(new AXI4Plic(
    Seq(AddressSet(0x3c000000L, 0x03ffffffL)),
    sim = !env.FPGAPlatform
  ))
  plic.node := AXI4IdentityNode() := AXI4UserYanker() := TLToAXI4() := peripheralXbar

  val l3cache = LazyModule(new InclusiveCache(
    CacheParameters(
      level = 3,
      ways = L3NWays,
      sets = L3NSets,
      blockBytes = L3BlockSize,
      beatBytes = L2BusWidth / 8,
      cacheName = "L3",
      uncachedGet = false,
      enablePerf = false
    ),
    InclusiveCacheMicroParameters(
      memCycles = 25,
      writeBytes = 32
    ),
    fpga = env.FPGAPlatform
  ))

  bankedNode :*= l3cache.node :*= TLBuffer() :*= l3_xbar

  lazy val module = new LazyRawModuleImp(this) {
    val io = IO(new Bundle {
      val clock = Input(Bool())
      val reset = Input(Bool())
      val extIntrs = Input(UInt(NrExtIntr.W))
      // val meip = Input(Vec(NumCores, Bool()))
      val ila = if(env.FPGAPlatform && EnableILA) Some(Output(new ILABundle)) else None
    })
    childClock := io.clock.asClock()

    withClockAndReset(childClock, io.reset) {
      val resetGen = Module(new ResetGen())
      resetGen.suggestName("top_reset_gen")
      childReset := resetGen.io.out
    }

    withClockAndReset(childClock, childReset) {
      plic.module.io.extra.get.intrVec <> Cat(beuSink.module.interrupt, io.extIntrs)
      require(io.extIntrs.getWidth + beuSink.module.interrupt.getWidth == NrPlicIntr)

      for (i <- 0 until NumCores) {
        val core_reset_gen = Module(new ResetGen())
        core_reset_gen.suggestName(s"core_${i}_reset_gen")
        core_with_l2(i).module.reset := core_reset_gen.io.out
        core_with_l2(i).module.io.hartId := i.U
        core_with_l2(i).module.io.externalInterrupt.mtip := clint.module.io.mtip(i)
        core_with_l2(i).module.io.externalInterrupt.msip := clint.module.io.msip(i)
        core_with_l2(i).module.io.externalInterrupt.meip := plic.module.io.extra.get.meip(i)
        beu.module.io.errors.l1plus(i) := core_with_l2(i).module.io.l1plus_error
        beu.module.io.errors.icache(i) := core_with_l2(i).module.io.icache_error
        beu.module.io.errors.dcache(i) := core_with_l2(i).module.io.dcache_error
      }

      val l3_reset_gen = Module(new ResetGen())
      l3_reset_gen.suggestName("l3_reset_gen")
      l3cache.module.reset := l3_reset_gen.io.out
    }
  }
}

object TopMain extends App {
  override def main(args: Array[String]): Unit = {
    Parameters.set(
      args.contains("--dual-core") match {
        case false => Parameters()
        case true  => Parameters.dualCoreParameters
      }
    )
    val otherArgs = args.filterNot(_ == "--dual-core")
    implicit val p = new Config((_, _, _) => {
      case XLen => 64
    })
    XiangShanStage.execute(otherArgs, Seq(
      ChiselGeneratorAnnotation(() => {
        val soc = LazyModule(new XSTop())
        soc.module
      })
    ))
  }
}
