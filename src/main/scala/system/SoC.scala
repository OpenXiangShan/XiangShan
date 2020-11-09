package system

import chipsalliance.rocketchip.config.Parameters
import device.{AXI4Timer, TLTimer}
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink.{TLBuffer, TLFuzzer, TLIdentityNode, TLXbar}
import utils.DebugIdentityNode
import xiangshan.{HasXSParameter, XSCore}


case class SoCParameters
(
  EnableILA: Boolean = false,
  HasL2Cache: Boolean = false,
  HasPrefetch: Boolean = false
)

trait HasSoCParameter extends HasXSParameter{
  val soc = top.Parameters.get.socParameters
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

  private val xsCore = LazyModule(new XSCore())

  // only mem and extDev visible externally
  val mem = xsCore.mem
  val extDev = TLIdentityNode()

  private val mmioXbar = TLXbar()
  private val clint = LazyModule(new TLTimer(
    Seq(AddressSet(0x38000000L, 0x0000ffffL)),
    sim = !env.FPGAPlatform
  ))

  mmioXbar :=
    TLBuffer() :=
    DebugIdentityNode() :=
    xsCore.mmio

  clint.node :=
    mmioXbar

  extDev :=
    mmioXbar

  lazy val module = new LazyModuleImp(this){
    val io = IO(new Bundle{
      val meip = Input(Bool())
      val ila = if(env.FPGAPlatform && EnableILA) Some(Output(new ILABundle)) else None
    })
    xsCore.module.io.externalInterrupt.mtip := clint.module.io.mtip
    xsCore.module.io.externalInterrupt.msip := clint.module.io.msip
    xsCore.module.io.externalInterrupt.meip := RegNext(RegNext(io.meip))
  }

}


//class XSSoc extends Module with HasSoCParameter {
//  val io = IO(new Bundle{
//    val mem = new TLCached(l1BusParams)
//    val mmio = new TLCached(l1BusParams)
//    val frontend = Flipped(new AXI4) //TODO: do we need it ?
//    val meip = Input(Bool())
//    val ila = if (env.FPGAPlatform && EnableILA) Some(Output(new ILABundle)) else None
//  })
//
//  val xsCore = Module(new XSCore)
//
//  io.frontend <> DontCare
//
//  io.mem <> xsCore.io.mem
//
//  val addrSpace = List(
//    (0x40000000L, 0x40000000L), // external devices
//    (0x38000000L, 0x00010000L)  // CLINT
//  )
//  val mmioXbar = Module(new NaiveTL1toN(addrSpace, xsCore.io.mem.params))
//  mmioXbar.io.in <> xsCore.io.mmio
//
//  val extDev = mmioXbar.io.out(0)
//  val clint = Module(new AXI4Timer(sim = !env.FPGAPlatform))
//  clint.io.in <> AXI4ToAXI4Lite(MMIOTLToAXI4(mmioXbar.io.out(1)))
//
//  io.mmio <> extDev
//
//  val mtipSync = clint.io.extra.get.mtip
//  val meipSync = RegNext(RegNext(io.meip))
//  ExcitingUtils.addSource(mtipSync, "mtip")
//  ExcitingUtils.addSource(meipSync, "meip")
//}
