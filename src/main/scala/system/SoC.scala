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

import chipsalliance.rocketchip.config.{Field, Parameters}
import chisel3._
import chisel3.util._
import device.DebugModule
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy.{AddressSet, IdRange, InModuleBody, LazyModule, LazyModuleImp, MemoryDevice, RegionType, TransferSizes}
import freechips.rocketchip.interrupts.{IntSourceNode, IntSourcePortSimple}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.{FastToSlow, SlowToFast}
import top.BusPerfMonitor
import utils.TLEdgeBuffer
import huancun._
import huancun.debug.TLLogger
import huancun.utils.{ClockSync3, DFTResetGen, ResetGen}
import xiangshan.backend.fu.PMAConst
import xiangshan.{DebugOptionsKey, XSTileKey}

case object SoCParamsKey extends Field[SoCParameters]

case class SoCParameters
(
  EnableILA: Boolean = false,
  PAddrBits: Int = 38,
  extIntrs: Int = 256,
  L3NBanks: Int = 4,
  L3CacheParamsOpt: Option[HCCacheParameters] = Some(HCCacheParameters(
    name = "l3",
    level = 3,
    ways = 8,
    sets = 2048 // 1MB per bank
  ))
){
  // L3 configurations
  val L3InnerBusWidth = 256
  val L3BlockSize = 64
  // on chip network configurations
  val L3OuterBusWidth = 256
}

trait HasSoCParameter {
  implicit val p: Parameters

  val soc = p(SoCParamsKey)
  val debugOpts = p(DebugOptionsKey)
  val tiles = p(XSTileKey)

  val NumCores = tiles.size
  val EnableILA = soc.EnableILA

  // L3 configurations
  val L3InnerBusWidth = soc.L3InnerBusWidth
  val L3BlockSize = soc.L3BlockSize
  val L3NBanks = soc.L3NBanks

  // on chip network configurations
  val L3OuterBusWidth = soc.L3OuterBusWidth

  val NrExtIntr = soc.extIntrs
}

class ILABundle extends Bundle {}


abstract class BaseSoC()(implicit p: Parameters) extends LazyModule with HasSoCParameter {
  val bankedNode = BankBinder(L3NBanks, L3BlockSize)
  val peripheralXbar = TLXbar()
  val l3_xbar = TLXbar()
  val l3_banked_xbar = TLXbar()

  val address_map = Map(
    "peripheral"     -> (0x0080000000L, 0x1fffffffffL),
    "cpu_peripheral" -> (0x1f10000000L, 0x1f1fffffffL),
    "memory"         -> (0x2000000000L, 0x23ffffffffL),
  )
  def getAddressSet(name: String): Seq[AddressSet] = {
    // AddressSet(base, mask)
    val (low, high) = address_map(name)
    // case 1: low is x0000, high is xffff
    val is_case_1 = isPow2(low) && isPow2(high + 1L)
    // case 2: low = base, mask = high - low
    val is_case_2 = ((high - low) & low) == 0
    if (is_case_1) {
      AddressSet(0, high).subtract(AddressSet(0, low - 1))
    }
    else if (is_case_2) {
      Seq(AddressSet(low, high - low))
    }
    else {
      require(false, s"cannot generate address set for ($low, $high)")
      Seq(AddressSet(0, 0))
    }
  }
  val paddrRange = AddressSet(0x00000000L, (1L << soc.PAddrBits) - 1)
}

// We adapt the following three traits from rocket-chip.
// Source: rocket-chip/src/main/scala/subsystem/Ports.scala
trait HaveSlaveAXI4Port {
  this: BaseSoC =>

  val dmaIdBits = 8
  val l3FrontendAXI4Node = AXI4MasterNode(Seq(AXI4MasterPortParameters(
    Seq(AXI4MasterParameters(
      name = "dma",
      id = IdRange(0, 1 << dmaIdBits)
    ))
  )))
  private val errorDevice = LazyModule(new TLError(
    params = DevNullParams(
      // requests to address below memory will be granted with erros
      address = paddrRange.subtract(getAddressSet("memory")),
      maxAtomic = 8,
      maxTransfer = 64),
    beatBytes = L3InnerBusWidth / 8
  ))

  class DMAPortClockDivDomain()(implicit p: Parameters) extends LazyModule {
    val dmaNode = AXI4IdentityNode()
    val rationalNode = TLRationalIdentityNode()

    rationalNode :=
      TLRationalCrossingSource() :=
      TLFIFOFixer() :=
      TLWidthWidget(32) :=
      AXI4ToTL() :=
      AXI4UserYanker(Some(1)) :=
      AXI4Fragmenter() :=
      AXI4Buffer() :=
      AXI4IdIndexer(5) :=
      dmaNode

    lazy val module = new LazyModuleImp(this) {
      override def desiredName: String = "DMAPortClockDivDomain"
    }
  }
  val dmaClkDiv2Domain = LazyModule(new DMAPortClockDivDomain)
  dmaClkDiv2Domain.dmaNode := l3FrontendAXI4Node

  l3_xbar :=
    TLRationalCrossingSink(SlowToFast) :=
    dmaClkDiv2Domain.rationalNode
  errorDevice.node := l3_xbar

  val dma = InModuleBody {
    l3FrontendAXI4Node.makeIOs()
  }
}

trait HaveAXI4MemPort {
  this: BaseSoC =>
  val device = new MemoryDevice
  val memRange = getAddressSet("memory")
  val memAXI4SlaveNode = AXI4SlaveNode(Seq(
    AXI4SlavePortParameters(
      slaves = Seq(
        AXI4SlaveParameters(
          address = memRange,
          regionType = RegionType.UNCACHED,
          executable = true,
          supportsRead = TransferSizes(1, L3BlockSize),
          supportsWrite = TransferSizes(1, L3BlockSize),
          interleavedId = Some(0),
          resources = device.reg("mem")
        )
      ),
      beatBytes = L3OuterBusWidth / 8
    )
  ))

  val mem_xbar = TLXbar()
  mem_xbar :=*
    TLXbar() :=*
    TLBuffer.chainNode(2) :=*
    TLCacheCork() :=*
    bankedNode

  mem_xbar :=
    TLWidthWidget(8) :=
    TLBuffer.chainNode(3, name = Some("PeripheralXbar_to_MemXbar_buffer")) :=
    peripheralXbar

  class MemPortClockDivDomain()(implicit p: Parameters) extends LazyModule {
    val memoryNode = AXI4IdentityNode()
    val rationalNode = TLRationalIdentityNode()

    memoryNode :=
      AXI4Buffer() :=
      AXI4IdIndexer(idBits = 6) :=
      AXI4UserYanker() :=
      AXI4Deinterleaver(L3BlockSize) :=
      TLToAXI4() :=
      TLSourceShrinker(64) :=
      TLWidthWidget(L3OuterBusWidth / 8) :=
      TLRationalCrossingSink(FastToSlow) :=
      rationalNode

    lazy val module = new LazyModuleImp(this) {
      override def desiredName: String = "MemPortClockDivDomain"
    }
  }

  val memClkDiv2Domain = LazyModule(new MemPortClockDivDomain)

  memClkDiv2Domain.rationalNode :=
    TLRationalCrossingSource() :=
    mem_xbar

  memAXI4SlaveNode := memClkDiv2Domain.memoryNode

  val memory = InModuleBody {
    memAXI4SlaveNode.makeIOs()
  }
}

trait HaveAXI4PeripheralPort { this: BaseSoC =>
  val peripheralBusWidth = 32
  val peripheralRange = getAddressSet("peripheral").flatMap(_.subtract(getAddressSet("cpu_peripheral")))
  val peripheralNode = AXI4SlaveNode(Seq(AXI4SlavePortParameters(
    Seq(AXI4SlaveParameters(
      address = peripheralRange,
      regionType = RegionType.UNCACHED,
      supportsRead = TransferSizes(1, peripheralBusWidth),
      supportsWrite = TransferSizes(1, peripheralBusWidth),
      interleavedId = Some(0)
    )),
    beatBytes = peripheralBusWidth
  )))

  class PeriPortClockDivDomain()(implicit p: Parameters) extends LazyModule {
    val outNode = AXI4IdentityNode()
    val rationalNode = TLRationalIdentityNode()

    outNode :=
      AXI4IdIndexer(idBits = 5) :=
      AXI4Buffer() :=
      AXI4Buffer() :=
      AXI4UserYanker() :=
      AXI4Deinterleaver(8) :=
      TLToAXI4() :=
      TLWidthWidget(8) :=
      TLRationalCrossingSink(FastToSlow) :=
      rationalNode

    lazy val module = new LazyModuleImp(this) {
      override def desiredName: String = "PeriPortClockDivDomain"
    }
  }
  val periClkDiv2Domain = LazyModule(new PeriPortClockDivDomain)
  periClkDiv2Domain.rationalNode :=
    TLRationalCrossingSource() :=
    peripheralXbar

  peripheralNode := periClkDiv2Domain.outNode

  val peripheral = InModuleBody {
    peripheralNode.makeIOs()
  }

}

class SoCMisc()(implicit p: Parameters) extends BaseSoC
  with HaveAXI4MemPort
  with HaveAXI4PeripheralPort
  with PMAConst
  with HaveSlaveAXI4Port
{
  val peripheral_ports = Array.fill(NumCores) { TLTempNode() }
  val core_to_l3_ports = Array.fill(NumCores) { TLTempNode() }

  val l3_in = TLTempNode()
  val l3_out = TLTempNode()
  val l3_mem_pmu = BusPerfMonitor(enable = !debugOpts.FPGAPlatform)

  l3_in :*= TLEdgeBuffer(_ => true, Some("L3_in_buffer")) :*= l3_banked_xbar
  bankedNode :*= TLLogger("MEM_L3", !debugOpts.FPGAPlatform) :*= l3_mem_pmu :*= l3_out

  if(soc.L3CacheParamsOpt.isEmpty){
    l3_out :*= l3_in
  }

  for(port <- peripheral_ports) {
    peripheralXbar := TLBuffer.chainNode(2, Some("L2_to_L3_peripheral_buffer")) := port
  }

  for ((core_out, i) <- core_to_l3_ports.zipWithIndex){
    l3_banked_xbar :=*
      TLLogger(s"L3_L2_$i", !debugOpts.FPGAPlatform) :=*
      TLBuffer() :=
      core_out
  }
  l3_banked_xbar := TLBuffer.chainNode(2) := l3_xbar

  val clint = LazyModule(new CLINT(CLINTParams(0x1f10000000L), 8))
  clint.node := peripheralXbar

  class IntSourceNodeToModule(val num: Int)(implicit p: Parameters) extends LazyModule {
    val sourceNode = IntSourceNode(IntSourcePortSimple(num, ports = 1, sources = 1))
    lazy val module = new LazyModuleImp(this){
      override def desiredName: String = "IntSourceNodeToModule"
      val in = IO(Input(Vec(num, Bool())))
      in.zip(sourceNode.out.head._1).foreach{ case (i, s) => s := i }
    }
  }

  val plic = LazyModule(new TLPLIC(PLICParams(0x1f1c000000L), 8))
  val plicSource = LazyModule(new IntSourceNodeToModule(NrExtIntr))

  plic.intnode := plicSource.sourceNode
  plic.node := peripheralXbar

  val debugModule = LazyModule(new DebugModule(NumCores)(p))
  debugModule.debug.node := peripheralXbar
  debugModule.debug.dmInner.dmInner.sb2tlOpt.foreach { sb2tl  =>
    l3_xbar := TLBuffer() := sb2tl.node
  }

  lazy val module = new LazyModuleImp(this) {
    val debug_module_io = IO(chiselTypeOf(debugModule.module.io))
    val ext_intrs = IO(Input(UInt(NrExtIntr.W)))
    val rtc_clock = IO(Input(Bool()))
    val clock_div2 = IO(Input(Clock()))
    val reset_no_sync = IO(Input(Reset()))
    val dfx_reset = IO(Input(new DFTResetGen))

    val reset_sync_div2 = ResetGen(clock_div2, reset_no_sync, 2, Some(dfx_reset))
    memClkDiv2Domain.module.clock := clock_div2
    memClkDiv2Domain.module.reset := reset_sync_div2
    periClkDiv2Domain.module.clock := clock_div2
    periClkDiv2Domain.module.reset := reset_sync_div2
    dmaClkDiv2Domain.module.clock := clock_div2
    dmaClkDiv2Domain.module.reset := reset_sync_div2

    debugModule.module.io <> debug_module_io

    // sync external interrupts
    require(plicSource.module.in.length == ext_intrs.getWidth)
    for ((plic_in, interrupt) <- plicSource.module.in.zip(ext_intrs.asBools)) {
      plic_in := ClockSync3(interrupt)
    }

    // positive edge sampling of the lower-speed rtc_clock
    val rtcTick = RegInit(0.U(3.W))
    rtcTick := Cat(rtcTick(1, 0), rtc_clock)
    clint.module.io.rtcTick := rtcTick(1) && !rtcTick(2)

  }
}
