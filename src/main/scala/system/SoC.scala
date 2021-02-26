package system

import chipsalliance.rocketchip.config.Parameters
import device.{AXI4Timer, TLTimer, AXI4Plic}
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink.{BankBinder, TLBuffer, TLBundleParameters, TLCacheCork, TLClientNode, TLFilter, TLFuzzer, TLIdentityNode, TLToAXI4, TLWidthWidget, TLXbar}
import utils.{DebugIdentityNode, DataDontCareNode}
import utils.XSInfo
import xiangshan.{HasXSParameter, XSCore, HasXSLog, DifftestBundle}
import xiangshan.cache.prefetch._
import sifive.blocks.inclusivecache.{CacheParameters, InclusiveCache, InclusiveCacheMicroParameters}
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyModuleImp}
import freechips.rocketchip.devices.tilelink.{DevNullParams, TLError}
import freechips.rocketchip.amba.axi4.{AXI4Deinterleaver, AXI4Fragmenter, AXI4IdIndexer, AXI4IdentityNode, AXI4ToTL, AXI4UserYanker}

case class SoCParameters
(
  NumCores: Integer = 1,
  EnableILA: Boolean = false,
  HasL2Cache: Boolean = false,
  HasPrefetch: Boolean = false
)

trait HasSoCParameter extends HasXSParameter{
  val soc = top.Parameters.get.socParameters
  val NumCores = soc.NumCores
  val EnableILA = soc.EnableILA
  val HasL2cache = soc.HasL2Cache
  val HasPrefetch = soc.HasPrefetch
}

class ILABundle extends Bundle {}


class DummyCore()(implicit p: Parameters) extends LazyModule {
  val mem = TLFuzzer(nOperations = 10)
  val mmio = TLFuzzer(nOperations = 10)

  lazy val module = new LazyModuleImp(this){

  }
}


class XSSoc()(implicit p: Parameters) extends LazyModule with HasSoCParameter {
  // CPU Cores
  private val xs_core = Seq.fill(NumCores)(LazyModule(new XSCore()))

  // L1 to L2 network
  // -------------------------------------------------
  private val l2_xbar = Seq.fill(NumCores)(TLXbar())

  private val l2cache = Seq.fill(NumCores)(LazyModule(new InclusiveCache(
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
  )))

  private val l2prefetcher = Seq.fill(NumCores)(LazyModule(new L2Prefetcher()))

  // L2 to L3 network
  // -------------------------------------------------
  private val l3_xbar = TLXbar()

  private val l3_node = LazyModule(new InclusiveCache(
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

  // L3 to memory network
  // -------------------------------------------------
  private val memory_xbar = TLXbar()
  private val mmioXbar = TLXbar()

  // only mem, dma and extDev are visible externally
  val mem = Seq.fill(L3NBanks)(AXI4IdentityNode())
  val dma = AXI4IdentityNode()
  val extDev = AXI4IdentityNode()

  // connections
  // -------------------------------------------------
  for (i <- 0 until NumCores) {
    l2_xbar(i) := TLBuffer() := DebugIdentityNode() := xs_core(i).memBlock.dcache.clientNode
    l2_xbar(i) := TLBuffer() := DebugIdentityNode() := xs_core(i).l1pluscache.clientNode
    l2_xbar(i) := TLBuffer() := DebugIdentityNode() := xs_core(i).ptw.node
    l2_xbar(i) := TLBuffer() := DebugIdentityNode() := l2prefetcher(i).clientNode

    mmioXbar   := TLBuffer() := DebugIdentityNode() := xs_core(i).memBlock.uncache.clientNode
    mmioXbar   := TLBuffer() := DebugIdentityNode() := xs_core(i).frontend.instrUncache.clientNode
    l2cache(i).node := DataDontCareNode(a = true, b = true) := TLBuffer() := DebugIdentityNode() := l2_xbar(i)
    l3_xbar := TLBuffer() := DebugIdentityNode() := l2cache(i).node
  }

  // DMA should not go to MMIO
  val mmioRange = AddressSet(base = 0x0000000000L, mask = 0x007fffffffL)
  // AXI4ToTL needs a TLError device to route error requests,
  // add one here to make it happy.
  val tlErrorParams = DevNullParams(
    address = Seq(mmioRange),
    maxAtomic = 8,
    maxTransfer = 64)
  val tlError = LazyModule(new TLError(params = tlErrorParams, beatBytes = L2BusWidth / 8))
  private val tlError_xbar = TLXbar()
  tlError_xbar :=
    AXI4ToTL() :=
    AXI4UserYanker(Some(1)) :=
    AXI4Fragmenter() :=
    AXI4IdIndexer(1) :=
    dma
  tlError.node := tlError_xbar

  l3_xbar :=
    TLBuffer() :=
    DebugIdentityNode() :=
    tlError_xbar

  val bankedNode =
    BankBinder(L3NBanks, L3BlockSize) :*= l3_node :*= TLBuffer() :*= DebugIdentityNode() :*= l3_xbar

  for(i <- 0 until L3NBanks) {
    mem(i) :=
      AXI4UserYanker() :=
      TLToAXI4() :=
      TLWidthWidget(L3BusWidth / 8) :=
      TLCacheCork() :=
      bankedNode
  }

  private val clint = LazyModule(new TLTimer(
    Seq(AddressSet(0x38000000L, 0x0000ffffL)),
    sim = !env.FPGAPlatform
  ))

  clint.node := mmioXbar
  extDev := AXI4UserYanker() := TLToAXI4() := mmioXbar

  val plic = LazyModule(new AXI4Plic(
    Seq(AddressSet(0x3c000000L, 0x03ffffffL)),
    sim = !env.FPGAPlatform
  ))
  val plicIdentity = AXI4IdentityNode()
  plic.node := plicIdentity := AXI4UserYanker() := TLToAXI4() := mmioXbar

  lazy val module = new LazyModuleImp(this){
    val io = IO(new Bundle{
      val extIntrs = Input(UInt(NrExtIntr.W))
      // val meip = Input(Vec(NumCores, Bool()))
      val ila = if(env.FPGAPlatform && EnableILA) Some(Output(new ILABundle)) else None
    })
    val difftestIO0 = IO(new DifftestBundle())
    val difftestIO1 = IO(new DifftestBundle())
    val difftestIO = Seq(difftestIO0, difftestIO1)

    val trapIO0 = IO(new xiangshan.TrapIO())
    val trapIO1 = IO(new xiangshan.TrapIO())
    val trapIO = Seq(trapIO0, trapIO1)

    plic.module.io.extra.get.intrVec <> RegNext(RegNext(io.extIntrs))

    for (i <- 0 until NumCores) {
      xs_core(i).module.io.hartId := i.U
      xs_core(i).module.io.externalInterrupt.mtip := clint.module.io.mtip(i)
      xs_core(i).module.io.externalInterrupt.msip := clint.module.io.msip(i)
      // xs_core(i).module.io.externalInterrupt.meip := RegNext(RegNext(io.meip(i)))
      xs_core(i).module.io.externalInterrupt.meip := plic.module.io.extra.get.meip(i)
      l2prefetcher(i).module.io.enable := xs_core(i).module.io.l2_pf_enable
      l2prefetcher(i).module.io.in <> l2cache(i).module.io
    }

    difftestIO0 <> xs_core(0).module.difftestIO
    difftestIO1 <> DontCare
    trapIO0 <> xs_core(0).module.trapIO
    trapIO1 <> DontCare
    
    if (env.DualCore) {
      difftestIO1 <> xs_core(1).module.difftestIO
      trapIO1 <> xs_core(1).module.trapIO
    }
    // do not let dma AXI signals optimized out
    dontTouch(dma.out.head._1)
    dontTouch(extDev.out.head._1)
    dontTouch(io.extIntrs)
  }

}
