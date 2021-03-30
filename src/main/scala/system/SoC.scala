/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package system

import org.chipsalliance.cde.config.Parameters
import device.{AXI4Plic, AXI4Timer, TLTimer}
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink.{BankBinder, TLBuffer, TLBundleParameters, TLCacheCork, TLClientNode, TLFilter, TLFuzzer, TLIdentityNode, TLToAXI4, TLWidthWidget, TLXbar}
import utils.{DataDontCareNode, DebugIdentityNode}
import utils.XSInfo
import xiangshan.{HasXSParameter, XSBundle, XSCore}
import xiangshan.cache.prefetch._
import sifive.blocks.inclusivecache.{CacheParameters, InclusiveCache, InclusiveCacheMicroParameters}
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyModuleImp}
import freechips.rocketchip.devices.tilelink.{DevNullParams, TLError}
import freechips.rocketchip.amba.axi4.{AXI4Deinterleaver, AXI4Fragmenter, AXI4IdIndexer, AXI4IdentityNode, AXI4ToTL, AXI4UserYanker}
import freechips.rocketchip.interrupts.{IntSinkNode, IntSinkParameters, IntSinkPortParameters, IntSinkPortSimple}
import freechips.rocketchip.tile.{BusErrorUnit, BusErrorUnitParams, BusErrors, L1BusErrors}

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


class L1CacheErrorInfo extends XSBundle{
  val paddr = Valid(UInt(PAddrBits.W))
  // for now, we only detect ecc
  val ecc_error = Valid(Bool())
}

class XSL1BusErrors(val nCores: Int) extends  BusErrors {
  val icache = Vec(nCores, new L1CacheErrorInfo)
  val l1plus = Vec(nCores, new L1CacheErrorInfo)
  val dcache = Vec(nCores, new L1CacheErrorInfo)

  override def toErrorList: List[Option[(ValidIO[UInt], String, String)]] =
    List.tabulate(nCores){i =>
      List(
        Some(icache(i).paddr, s"IBUS_$i", s"Icache_$i bus error"),
        Some(icache(i).ecc_error, s"I_ECC_$i", s"Icache_$i ecc error"),
        Some(l1plus(i).paddr, s"L1PLUS_$i", s"L1PLUS_$i bus error"),
        Some(l1plus(i).ecc_error, s"L1PLUS_ECC_$i", s"L1PLUS_$i ecc error"),
        Some(dcache(i).paddr, s"DBUS_$i", s"Dcache_$i bus error"),
        Some(dcache(i).ecc_error, s"D_ECC_$i", s"Dcache_$i ecc error")
      )
    }.flatten
}

class XSSoc()(implicit p: Parameters) extends LazyModule with HasSoCParameter {
  // CPU Cores
  private val xs_core = Seq.fill(NumCores)(LazyModule(new XSCore()))

  private val l2prefetcher = LazyModule(new L2Prefetcher())

  // L2 to L3 network
  // -------------------------------------------------
  private val l3_xbar = TLXbar()

  private val l3_cache = LazyModule(new InclusiveCache(
    CacheParameters(
      level = 3,
      ways = L3NWays,
      sets = L3NSets,
      blockBytes = L3BlockSize,
      beatBytes = L2BusWidth / 8,
      replacement = "plru",
      cacheName = "L3",
      uncachedGet = false,
      enablePerf = env.EnablePerfDebug && !env.FPGAPlatform
    ),
    InclusiveCacheMicroParameters(
      writeBytes = 32
    ),
    fpga = env.FPGAPlatform
  ))

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
    l3_xbar := TLBuffer() := DebugIdentityNode() := xs_core(i).memBlock.dcache.clientNode
    l3_xbar := TLBuffer() := DebugIdentityNode() := xs_core(i).l1pluscache.clientNode
    l3_xbar := TLBuffer() := DebugIdentityNode() := xs_core(i).ptw.node
    mmioXbar   := TLBuffer() := DebugIdentityNode() := xs_core(i).memBlock.uncache.clientNode
    mmioXbar   := TLBuffer() := DebugIdentityNode() := xs_core(i).frontend.instrUncache.clientNode
  }
  l3_xbar := TLBuffer() := DebugIdentityNode() := l2prefetcher.clientNode

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
    BankBinder(L3NBanks, L3BlockSize) :*= l3_cache.node :*= TLBuffer() :*= DebugIdentityNode() :*= l3_xbar

  for(i <- 0 until L3NBanks) {
    mem(i) :=
      AXI4UserYanker() :=
      TLToAXI4() :=
      TLWidthWidget(L3BusWidth / 8) :=
      TLBuffer() :=
      TLCacheCork() :=
      bankedNode
  }

  private val clint = LazyModule(new TLTimer(
    Seq(AddressSet(0x38000000L, 0x0000ffffL)),
    sim = !env.FPGAPlatform
  ))

  clint.node := mmioXbar
  extDev := AXI4UserYanker() := TLToAXI4() := mmioXbar

  val beu = LazyModule(
    new BusErrorUnit(new XSL1BusErrors(NumCores), BusErrorUnitParams(0x38010000)))
  beu.node := mmioXbar

  class BeuSinkNode()(implicit p: Parameters) extends LazyModule {
    val intSinkNode = IntSinkNode(IntSinkPortSimple())
    lazy val module = new Impl
    class Impl extends LazyModuleImp(this){
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
  plic.node := AXI4UserYanker() := TLToAXI4() := mmioXbar

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this){
    val io = IO(new Bundle{
      val extIntrs = Input(UInt(NrExtIntr.W))
      // val meip = Input(Vec(NumCores, Bool()))
      val ila = if(env.FPGAPlatform && EnableILA) Some(Output(new ILABundle)) else None
    })

    plic.module.io.extra.get.intrVec <> RegNext(beuSink.module.interrupt)

    for (i <- 0 until NumCores) {
      xs_core(i).module.io.hartId := i.U
      xs_core(i).module.io.externalInterrupt.mtip := clint.module.io.mtip(i)
      xs_core(i).module.io.externalInterrupt.msip := clint.module.io.msip(i)
      beu.module.io.errors.l1plus(i) := RegNext(xs_core(i).module.io.l1plus_error)
      beu.module.io.errors.icache(i) := RegNext(xs_core(i).module.io.icache_error)
      beu.module.io.errors.dcache(i) := RegNext(xs_core(i).module.io.dcache_error)
      // xs_core(i).module.io.externalInterrupt.meip := RegNext(RegNext(io.meip(i)))
      xs_core(i).module.io.externalInterrupt.meip := plic.module.io.extra.get.meip(i)
      l2prefetcher.module.io.enable := RegNext(xs_core(i).module.io.l2_pf_enable)
    }
    l2prefetcher.module.io.in <> l3_cache.module.io

    // do not let dma AXI signals optimized out
    dontTouch(dma.out.head._1)
    dontTouch(extDev.out.head._1)
    dontTouch(io.extIntrs)
  }

}
