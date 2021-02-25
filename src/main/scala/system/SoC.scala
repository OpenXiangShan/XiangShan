package system

import chipsalliance.rocketchip.config.Parameters
import device.{AXI4Timer, TLTimer, AXI4Plic}
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink.{BankBinder, TLBuffer, TLBundleParameters, TLCacheCork, TLClientNode, TLFilter, TLFuzzer, TLIdentityNode, TLToAXI4, TLWidthWidget, TLXbar}
import utils.{DebugIdentityNode, DataDontCareNode}
import utils.XSInfo
import xiangshan.{HasXSParameter, XSCore, HasXSLog}
import sifive.blocks.inclusivecache.{CacheParameters, InclusiveCache, InclusiveCacheMicroParameters}
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyModuleImp}
import freechips.rocketchip.devices.tilelink.{DevNullParams, TLError}
import freechips.rocketchip.amba.axi4.{AXI4Deinterleaver, AXI4Fragmenter, AXI4IdIndexer, AXI4IdentityNode, AXI4ToTL, AXI4UserYanker}
import devices.debug.{TLDebugModule, DebugIO, ResetCtrlIO, SystemJTAGIO, DebugTransportModuleJTAG, DebugModuleKey}
import freechips.rocketchip.jtag.JTAGIO
import chisel3.experimental.{IntParam, noPrefix}

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
      writeBytes = 8
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
    l2_xbar(i) := TLBuffer() := DebugIdentityNode() := xs_core(i).l2Prefetcher.clientNode
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

  // DM
  val DM = LazyModule(new TLDebugModule(beatBytes = 8))
  DM.node := mmioXbar
  
  lazy val module = new LazyModuleImp(this){
    val io = IO(new Bundle{
      val extIntrs = Input(Vec(NrExtIntr, Bool()))
      // val meip = Input(Vec(NumCores, Bool()))
      val ila = if(env.FPGAPlatform && EnableILA) Some(Output(new ILABundle)) else None
      // debug io is below, both are options
    })

    plic.module.io.extra.get.intrVec <> RegNext(RegNext(Cat(io.extIntrs)))

    for (i <- 0 until NumCores) {
      xs_core(i).module.io.externalInterrupt.mtip := clint.module.io.mtip(i)
      xs_core(i).module.io.externalInterrupt.msip := clint.module.io.msip(i)
      // xs_core(i).module.io.externalInterrupt.meip := RegNext(RegNext(io.meip(i)))
      xs_core(i).module.io.externalInterrupt.meip := plic.module.io.extra.get.meip(i)
      xs_core(i).module.io.externalInterrupt.debug_int := DM.module.io.debug_int(i)
    }

    val resetctrl = Some(DM).map { outerdebug =>
      outerdebug.module.io.tl_reset := reset
      outerdebug.module.io.tl_clock := clock
      val resetctrl = IO(new ResetCtrlIO(p(DebugModuleKey).get.nComponents))
      outerdebug.module.io.hartIsInReset := resetctrl.hartIsInReset
      resetctrl.hartResetReq.foreach { rcio => outerdebug.module.io.hartResetReq.foreach { rcdm => rcio := rcdm }}
      resetctrl
    }

    // noPrefix is workaround https://github.com/freechipsproject/chisel3/issues/1603
    val debug = noPrefix(Some(DM).map { outerdebug =>
      val debug = IO(new DebugIO)

      require(!(debug.clockeddmi.isDefined && debug.systemjtag.isDefined),
        "You cannot have both DMI and JTAG interface in HasPeripheryDebugModuleImp")

      require(!(debug.clockeddmi.isDefined && debug.apb.isDefined),
        "You cannot have both DMI and APB interface in HasPeripheryDebugModuleImp")

      require(!(debug.systemjtag.isDefined && debug.apb.isDefined),
        "You cannot have both APB and JTAG interface in HasPeripheryDebugModuleImp")

      debug.clockeddmi.foreach { dbg => outerdebug.module.io.dmi.get <> dbg }

      /*(debug.apb
        zip outer.apbDebugNodeOpt
        zip outerdebug.module.io.apb_clock
        zip outerdebug.module.io.apb_reset).foreach {
        case (((io, apb), c ), r) =>
          apb.out(0)._1 <> io
          c:= io.clock
          r:= io.reset
      }*/

      outerdebug.module.io.debug_reset := debug.reset
      outerdebug.module.io.debug_clock := debug.clock

      debug.ndreset := outerdebug.module.io.ctrl.ndreset
      debug.dmactive := outerdebug.module.io.ctrl.dmactive
      outerdebug.module.io.ctrl.dmactiveAck := debug.dmactiveAck
      debug.extTrigger.foreach { x => outerdebug.module.io.extTrigger.foreach {y => x <> y}}

      // TODO in inheriting traits: Set this to something meaningful, e.g. "component is in reset or powered down"
      outerdebug.module.io.ctrl.debugUnavail.foreach { _ := false.B }

      debug
    })

    val dtm = debug.flatMap(_.systemjtag.map(instantiateJtagDTM(_)))

    def instantiateJtagDTM(sj: SystemJTAGIO): DebugTransportModuleJTAG = {

      val dtm = Module(new DebugTransportModuleJTAG(p(DebugModuleKey).get.nDMIAddrSize)) //, p(JtagDTMKey)))
      dtm.io.jtag <> sj.jtag

      debug.map(_.disableDebug.foreach { x => dtm.io.jtag.TMS := sj.jtag.TMS | x })  // force TMS high when debug is disabled

      dtm.io.jtag_clock  := sj.jtag.TCK
      dtm.io.jtag_reset  := sj.reset
      dtm.io.jtag_mfr_id := sj.mfr_id
      dtm.io.jtag_part_number := sj.part_number
      dtm.io.jtag_version := sj.version
      dtm.rf_reset := sj.reset

      Some(DM).map { outerdebug => 
        outerdebug.module.io.dmi.get.dmi <> dtm.io.dmi
        outerdebug.module.io.dmi.get.dmiClock := sj.jtag.TCK
        outerdebug.module.io.dmi.get.dmiReset := sj.reset
      }
      dtm
    }

    // do not let dma AXI signals optimized out
    chisel3.dontTouch(dma.out.head._1)
    chisel3.dontTouch(extDev.out.head._1)
  }

}
