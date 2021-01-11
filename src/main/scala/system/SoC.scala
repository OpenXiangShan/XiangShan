package system

import chipsalliance.rocketchip.config.Parameters
import device.{AXI4Timer, TLTimer}
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink.{TLBuffer, TLFuzzer, TLIdentityNode, TLXbar}
import utils.DebugIdentityNode
import xiangshan.{HasXSParameter, XSCore}
import sifive.blocks.inclusivecache.{CacheParameters, InclusiveCache, InclusiveCacheMicroParameters}
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp, AddressSet}
import freechips.rocketchip.tilelink.{TLBundleParameters, TLCacheCork, TLBuffer, TLClientNode, TLIdentityNode, TLXbar, TLWidthWidget, TLFilter, TLToAXI4}
import freechips.rocketchip.devices.tilelink.{TLError, DevNullParams}
import freechips.rocketchip.amba.axi4.{AXI4ToTL, AXI4IdentityNode, AXI4UserYanker, AXI4Fragmenter, AXI4IdIndexer, AXI4Deinterleaver}

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
      writeBytes = 8
    )
  )))

  // L2 to L3 network
  // -------------------------------------------------
  private val l3_xbar = TLXbar()

  private val l3_banks = (0 until L3NBanks) map (i =>
      LazyModule(new InclusiveCache(
        CacheParameters(
          level = 3,
          ways = L3NWays,
          sets = L3NSets,
          blockBytes = L3BlockSize,
          beatBytes = L2BusWidth / 8,
          cacheName = s"L3_$i"
        ),
      InclusiveCacheMicroParameters(
        writeBytes = 8
      )
    )))

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
    mmioXbar   := TLBuffer() := DebugIdentityNode() := xs_core(i).memBlock.uncache.clientNode
    l2cache(i).node := TLBuffer() := DebugIdentityNode() := l2_xbar(i)
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

  def bankFilter(bank: Int) = AddressSet(
    base = bank * L3BlockSize,
    mask = ~BigInt((L3NBanks -1) * L3BlockSize))

  for(i <- 0 until L3NBanks) {
    val filter = TLFilter(TLFilter.mSelectIntersect(bankFilter(i)))
    l3_banks(i).node := TLBuffer() := DebugIdentityNode() := filter := l3_xbar
  }

  for(i <- 0 until L3NBanks) {
    mem(i) :=
      AXI4UserYanker() :=
      TLToAXI4() :=
      TLWidthWidget(L3BusWidth / 8) :=
      TLCacheCork() :=
      l3_banks(i).node
  }

  private val clint = LazyModule(new TLTimer(
    Seq(AddressSet(0x38000000L, 0x0000ffffL)),
    sim = !env.FPGAPlatform
  ))

  clint.node := mmioXbar
  extDev := AXI4UserYanker() := TLToAXI4() := mmioXbar

  lazy val module = new LazyModuleImp(this){
    val io = IO(new Bundle{
      val meip = Input(Bool())
      val ila = if(env.FPGAPlatform && EnableILA) Some(Output(new ILABundle)) else None
    })
    for (i <- 0 until NumCores) {
      xs_core(i).module.io.externalInterrupt.mtip := clint.module.io.mtip
      xs_core(i).module.io.externalInterrupt.msip := clint.module.io.msip
      xs_core(i).module.io.externalInterrupt.meip := RegNext(RegNext(io.meip))
    }
    // do not let dma AXI signals optimized out
    chisel3.dontTouch(dma.out.head._1)
    chisel3.dontTouch(extDev.out.head._1)
  }

}
