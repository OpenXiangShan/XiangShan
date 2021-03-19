package top

import chisel3._
import chisel3.util._
import xiangshan._
import system._
import chisel3.stage.ChiselGeneratorAnnotation
import chipsalliance.rocketchip.config
import device.{TLTimer, AXI4Plic}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.devices.tilelink.{DevNullParams, TLError}
import sifive.blocks.inclusivecache._
import xiangshan.cache.prefetch.L2Prefetcher


class XSCoreWithL2()(implicit p: config.Parameters) extends LazyModule
  with HasXSParameter {
  val core = LazyModule(new XSCore())
  val l2prefetcher = LazyModule(new L2Prefetcher())
  val l2cache = LazyModule(new InclusiveCache(
    CacheParameters(
      level = 2,
      ways = L2NWays,
      sets = L2NSets,
      blockBytes = L2BlockSize,
      beatBytes = L1BusWidth / 8, // beatBytes = l1BusDataWidth / 8
      cacheName = s"L2"
    ),
    InclusiveCacheMicroParameters(
      writeBytes = 32
    )
  ))
  private val l2xbar = TLXbar()

  l2xbar := TLBuffer() := core.memBlock.dcache.clientNode
  l2xbar := TLBuffer() := core.l1pluscache.clientNode
  l2xbar := TLBuffer() := core.ptw.node
  l2xbar := TLBuffer() := l2prefetcher.clientNode
  l2cache.node := TLBuffer() := l2xbar

  lazy val module = new XSCoreWithL2Imp(this)
}

class XSCoreWithL2Imp(outer: XSCoreWithL2) extends LazyModuleImp(outer)
  with HasXSParameter {
  val io = IO(new Bundle {
    val hartId = Input(UInt(64.W))
    val externalInterrupt = new ExternalInterruptIO
  })

  outer.core.module.io.hartId := io.hartId
  outer.core.module.io.externalInterrupt := io.externalInterrupt
  outer.l2prefetcher.module.io.enable := RegNext(outer.core.module.io.l2_pf_enable)
  outer.l2prefetcher.module.io.in <> outer.l2cache.module.io
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
  val memAXI4SlaveNode = AXI4SlaveNode(Seq.tabulate(L3NBanks) { i =>
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
  })

  memAXI4SlaveNode :=*
    AXI4UserYanker() :=*
    AXI4Deinterleaver(L3BlockSize) :=*
    TLToAXI4() :=*
    TLWidthWidget(L3BusWidth / 8) :=*
    TLCacheCork() :=*
    bankedNode

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
    peripheralXbar := TLBuffer() := core_with_l2(i).core.frontend.instrUncache.clientNode
    peripheralXbar := TLBuffer() := core_with_l2(i).core.memBlock.uncache.clientNode
    l3_xbar := TLBuffer() := core_with_l2(i).l2cache.node
  }

  private val clint = LazyModule(new TLTimer(
    Seq(AddressSet(0x38000000L, 0x0000ffffL)),
    sim = !env.FPGAPlatform
  ))
  clint.node := peripheralXbar

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
      cacheName = "L3"
    ),
    InclusiveCacheMicroParameters(
      writeBytes = 32
    )
  )).node

  bankedNode :*= l3cache :*= TLBuffer() :*= l3_xbar

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val extIntrs = Input(UInt(NrExtIntr.W))
      // val meip = Input(Vec(NumCores, Bool()))
      val ila = if(env.FPGAPlatform && EnableILA) Some(Output(new ILABundle)) else None
    })

    plic.module.io.extra.get.intrVec <> RegNext(RegNext(io.extIntrs))

    for (i <- 0 until NumCores) {
      core_with_l2(i).module.io.hartId := i.U
      core_with_l2(i).module.io.externalInterrupt.mtip := clint.module.io.mtip(i)
      core_with_l2(i).module.io.externalInterrupt.msip := clint.module.io.msip(i)
      core_with_l2(i).module.io.externalInterrupt.meip := plic.module.io.extra.get.meip(i)
    }

    dontTouch(io.extIntrs)
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
    implicit val p = config.Parameters.empty
    XiangShanStage.execute(otherArgs, Seq(
      ChiselGeneratorAnnotation(() => {
        val soc = LazyModule(new XSTop())
        soc.module
      })
    ))
  }
}
