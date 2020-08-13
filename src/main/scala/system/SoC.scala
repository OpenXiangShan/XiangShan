package system

import noop.{Cache, CacheConfig}
import bus.axi4.{AXI4, AXI4Lite, AXI4ToAXI4Lite}
import bus.simplebus._
import bus.tilelink.{NaiveTL1toN, MMIOTLToAXI4, TLCached}
import device.AXI4Timer
import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import top.Parameters
import xiangshan.{HasXSParameter, XSCore}


case class SoCParameters
(
  EnableILA: Boolean = false,
  HasL2Cache: Boolean = false,
  HasPrefetch: Boolean = false
)

trait HasSoCParameter extends HasXSParameter{
  val soc = Parameters.get.socParameters
  val EnableILA = soc.EnableILA
  val HasL2cache = soc.HasL2Cache
  val HasPrefetch = soc.HasPrefetch
}

class ILABundle extends Bundle {}

class XSSoc extends Module with HasSoCParameter {
  val io = IO(new Bundle{
    val mem = new TLCached(l1BusParams)
    val mmio = new TLCached(l1BusParams)
    val frontend = Flipped(new AXI4) //TODO: do we need it ?
    val meip = Input(Bool())
    val ila = if (env.FPGAPlatform && EnableILA) Some(Output(new ILABundle)) else None
  })

  val xsCore = Module(new XSCore)

  io.frontend <> DontCare

  io.mem <> xsCore.io.mem

  val addrSpace = List(
    (0x40000000L, 0x40000000L), // external devices
    (0x38000000L, 0x00010000L)  // CLINT
  )
  val mmioXbar = Module(new NaiveTL1toN(addrSpace, xsCore.io.mem.params))
  mmioXbar.io.in <> xsCore.io.mmio

  val extDev = mmioXbar.io.out(0)
  val clint = Module(new AXI4Timer(sim = !env.FPGAPlatform))
  clint.io.in <> AXI4ToAXI4Lite(MMIOTLToAXI4(mmioXbar.io.out(1)))

  io.mmio <> extDev

  val mtipSync = clint.io.extra.get.mtip
  val meipSync = RegNext(RegNext(io.meip))
  ExcitingUtils.addSource(mtipSync, "mtip")
  ExcitingUtils.addSource(meipSync, "meip")
}
