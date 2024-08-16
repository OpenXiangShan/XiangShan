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

import org.chipsalliance.cde.config.{Field, Parameters}
import chisel3._
import chisel3.util._
import device.{DebugModule, TLPMA, TLPMAIO}
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy.{AddressSet, IdRange, InModuleBody, LazyModule, LazyModuleImp, MemoryDevice, RegionType, SimpleDevice, TransferSizes}
import freechips.rocketchip.interrupts.{IntSourceNode, IntSourcePortSimple}
import freechips.rocketchip.regmapper.{RegField, RegFieldDesc, RegFieldGroup}
import freechips.rocketchip.tilelink._
import huancun._
import top.BusPerfMonitor
import utility.{ReqSourceKey, TLClientsMerger, TLEdgeBuffer, TLLogger}
import xiangshan.backend.fu.PMAConst
import xiangshan.{DebugOptionsKey, XSTileKey}
import coupledL2.EnableCHI
import coupledL2.tl2chi.CHIIssue

case object SoCParamsKey extends Field[SoCParameters]

case class SoCParameters
(
  EnableILA: Boolean = false,
  PAddrBits: Int = 36,
  extIntrs: Int = 64,
  L3NBanks: Int = 4,
  L3CacheParamsOpt: Option[HCCacheParameters] = Some(HCCacheParameters(
    name = "L3",
    level = 3,
    ways = 8,
    sets = 2048 // 1MB per bank
  )),
  XSTopPrefix: Option[String] = None,
  NodeIDWidthList: Map[String, Int] = Map(
    "B" -> 7,
    "E.b" -> 11
  ),
  NumHart: Int = 64,
  NumIRFiles: Int = 7,
  NumIRSrc: Int = 256,
  UseXSNoCTop: Boolean = false,
  IMSICUseTL: Boolean = false,
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
  val enableCHI = p(EnableCHI)
  val issue = p(CHIIssue)

  val NumCores = tiles.size
  val EnableILA = soc.EnableILA

  // L3 configurations
  val L3InnerBusWidth = soc.L3InnerBusWidth
  val L3BlockSize = soc.L3BlockSize
  val L3NBanks = soc.L3NBanks

  // on chip network configurations
  val L3OuterBusWidth = soc.L3OuterBusWidth

  val NrExtIntr = soc.extIntrs

  val SetIpNumValidSize = soc.NumHart * soc.NumIRFiles

  val NumIRSrc = soc.NumIRSrc
}

class ILABundle extends Bundle {}


abstract class BaseSoC()(implicit p: Parameters) extends LazyModule with HasSoCParameter {
  val bankedNode = Option.when(!enableCHI)(BankBinder(L3NBanks, L3BlockSize))
  val peripheralXbar = Option.when(!enableCHI)(TLXbar())
  val l3_xbar = Option.when(!enableCHI)(TLXbar())
  val l3_banked_xbar = Option.when(!enableCHI)(TLXbar())

  val soc_xbar = Option.when(enableCHI)(AXI4Xbar())
}

// We adapt the following three traits from rocket-chip.
// Source: rocket-chip/src/main/scala/subsystem/Ports.scala
trait HaveSlaveAXI4Port {
  this: BaseSoC =>

  val idBits = 14

  val l3FrontendAXI4Node = AXI4MasterNode(Seq(AXI4MasterPortParameters(
    Seq(AXI4MasterParameters(
      name = "dma",
      id = IdRange(0, 1 << idBits)
    ))
  )))

  if (l3_xbar.isDefined) {
    val errorDevice = LazyModule(new TLError(
      params = DevNullParams(
        address = Seq(AddressSet(0x0, 0x7fffffffL)),
        maxAtomic = 8,
        maxTransfer = 64),
      beatBytes = L3InnerBusWidth / 8
    ))
    errorDevice.node :=
      l3_xbar.get :=
      TLFIFOFixer() :=
      TLWidthWidget(32) :=
      AXI4ToTL() :=
      AXI4UserYanker(Some(1)) :=
      AXI4Fragmenter() :=
      AXI4Buffer() :=
      AXI4Buffer() :=
      AXI4IdIndexer(1) :=
      l3FrontendAXI4Node
  }

  val dma = InModuleBody {
    l3FrontendAXI4Node.makeIOs()
  }
}

trait HaveAXI4MemPort {
  this: BaseSoC =>
  val device = new MemoryDevice
  // 36-bit physical address
  val memRange = AddressSet(0x00000000L, 0xfffffffffL).subtract(AddressSet(0x0L, 0x7fffffffL))
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
      beatBytes = L3OuterBusWidth / 8,
      requestKeys = if (debugOpts.FPGAPlatform) Seq() else Seq(ReqSourceKey),
    )
  ))

  val mem_xbar = TLXbar()
  val l3_mem_pmu = BusPerfMonitor(name = "L3_Mem", enable = !debugOpts.FPGAPlatform && !enableCHI, stat_latency = true)
  val axi4mem_node = AXI4IdentityNode()

  if (enableCHI) {
    axi4mem_node :=
      soc_xbar.get
  } else {
    mem_xbar :=*
      TLBuffer.chainNode(2) :=
      TLCacheCork() :=
      l3_mem_pmu :=
      TLClientsMerger() :=
      TLXbar() :=*
      bankedNode.get

    mem_xbar :=
      TLWidthWidget(8) :=
      TLBuffer.chainNode(3, name = Some("PeripheralXbar_to_MemXbar_buffer")) :=
      peripheralXbar.get

    axi4mem_node :=
      TLToAXI4() :=
      TLSourceShrinker(64) :=
      TLWidthWidget(L3OuterBusWidth / 8) :=
      TLBuffer.chainNode(2) :=
      mem_xbar
  }

  memAXI4SlaveNode :=
    AXI4Buffer() :=
    AXI4Buffer() :=
    AXI4Buffer() :=
    AXI4IdIndexer(idBits = 14) :=
    AXI4UserYanker() :=
    AXI4Deinterleaver(L3BlockSize) :=
    axi4mem_node

  val memory = InModuleBody {
    memAXI4SlaveNode.makeIOs()
  }
}

trait HaveAXI4PeripheralPort { this: BaseSoC =>
  // on-chip devices: 0x3800_0000 - 0x3fff_ffff 0x0000_0000 - 0x0000_0fff
  val onChipPeripheralRange = AddressSet(0x38000000L, 0x07ffffffL)
  val uartRange = AddressSet(0x40600000, 0x3f)
  val uartDevice = new SimpleDevice("serial", Seq("xilinx,uartlite"))
  val uartParams = AXI4SlaveParameters(
    address = Seq(uartRange),
    regionType = RegionType.UNCACHED,
    supportsRead = TransferSizes(1, 32),
    supportsWrite = TransferSizes(1, 32),
    resources = uartDevice.reg
  )
  val peripheralRange = AddressSet(
    0x0, 0x7fffffff
  ).subtract(onChipPeripheralRange).flatMap(x => x.subtract(uartRange))
  val peripheralNode = AXI4SlaveNode(Seq(AXI4SlavePortParameters(
    Seq(AXI4SlaveParameters(
      address = peripheralRange,
      regionType = RegionType.UNCACHED,
      supportsRead = TransferSizes(1, 32),
      supportsWrite = TransferSizes(1, 32),
      interleavedId = Some(0)
    ), uartParams),
    beatBytes = 8
  )))

  val axi4peripheral_node = AXI4IdentityNode()
  val error_xbar = Option.when(enableCHI)(TLXbar())

  peripheralNode :=
    AXI4UserYanker() :=
    AXI4IdIndexer(idBits = 2) :=
    AXI4Buffer() :=
    AXI4Buffer() :=
    AXI4Buffer() :=
    AXI4Buffer() :=
    AXI4UserYanker() :=
    // AXI4Deinterleaver(8) :=
    axi4peripheral_node

  if (enableCHI) {
    val error = LazyModule(new TLError(
      params = DevNullParams(
        address = Seq(AddressSet(0x1000000000L, 0xfffffffffL)),
        maxAtomic = 8,
        maxTransfer = 64),
      beatBytes = 8
    ))
    error.node := error_xbar.get
    axi4peripheral_node :=
      AXI4Deinterleaver(8) :=
      TLToAXI4() :=
      error_xbar.get :=
      TLBuffer.chainNode(2, Some("llc_to_peripheral_buffer")) :=
      TLFIFOFixer() :=
      TLWidthWidget(L3OuterBusWidth / 8) :=
      AXI4ToTL() :=
      AXI4UserYanker() :=
      soc_xbar.get
  } else {
    axi4peripheral_node :=
      AXI4Deinterleaver(8) :=
      TLToAXI4() :=
      TLBuffer.chainNode(3) :=
      peripheralXbar.get
  }

  val peripheral = InModuleBody {
    peripheralNode.makeIOs()
  }

}

class MemMisc()(implicit p: Parameters) extends BaseSoC
  with HaveAXI4MemPort
  with PMAConst
  with HaveAXI4PeripheralPort
{

  val peripheral_ports = Option.when(!enableCHI)(Array.fill(NumCores) { TLTempNode() })
  val core_to_l3_ports = Option.when(!enableCHI)(Array.fill(NumCores) { TLTempNode() })

  val l3_in = TLTempNode()
  val l3_out = TLTempNode()

  val device_xbar = Option.when(enableCHI)(TLXbar())
  device_xbar.foreach(_ := error_xbar.get)

  if (l3_banked_xbar.isDefined) {
    l3_in :*= TLEdgeBuffer(_ => true, Some("L3_in_buffer")) :*= l3_banked_xbar.get
    l3_banked_xbar.get := TLBuffer.chainNode(2) := l3_xbar.get
  }
  bankedNode match {
    case Some(bankBinder) =>
      bankBinder :*= TLLogger("MEM_L3", !debugOpts.FPGAPlatform && debugOpts.AlwaysBasicDB) :*= l3_out
    case None =>
  }

  if(soc.L3CacheParamsOpt.isEmpty){
    l3_out :*= l3_in
  }

  if (!enableCHI) {
    for (port <- peripheral_ports.get) {
      peripheralXbar.get := TLBuffer.chainNode(2, Some("L2_to_L3_peripheral_buffer")) := port
    }
  }

  core_to_l3_ports.foreach { case _ =>
    for ((core_out, i) <- core_to_l3_ports.get.zipWithIndex){
      l3_banked_xbar.get :=*
        TLLogger(s"L3_L2_$i", !debugOpts.FPGAPlatform && debugOpts.AlwaysBasicDB) :=*
        TLBuffer() :=
        core_out
    }
  }

  val clint = LazyModule(new CLINT(CLINTParams(0x38000000L), 8))
  if (enableCHI) { clint.node := device_xbar.get }
  else { clint.node := peripheralXbar.get }

  class IntSourceNodeToModule(val num: Int)(implicit p: Parameters) extends LazyModule {
    val sourceNode = IntSourceNode(IntSourcePortSimple(num, ports = 1, sources = 1))
    class IntSourceNodeToModuleImp(wrapper: LazyModule) extends LazyModuleImp(wrapper) {
      val in = IO(Input(Vec(num, Bool())))
      in.zip(sourceNode.out.head._1).foreach{ case (i, s) => s := i }
    }
    lazy val module = new IntSourceNodeToModuleImp(this)
  }

  val plic = LazyModule(new TLPLIC(PLICParams(0x3c000000L), 8))
  val plicSource = LazyModule(new IntSourceNodeToModule(NrExtIntr))

  plic.intnode := plicSource.sourceNode
  if (enableCHI) { plic.node := device_xbar.get }
  else { plic.node := peripheralXbar.get }

  val pll_node = TLRegisterNode(
    address = Seq(AddressSet(0x3a000000L, 0xfff)),
    device = new SimpleDevice("pll_ctrl", Seq()),
    beatBytes = 8,
    concurrency = 1
  )
  if (enableCHI) { pll_node := device_xbar.get }
  else { pll_node := peripheralXbar.get }

  val debugModule = LazyModule(new DebugModule(NumCores)(p))
  if (enableCHI) {
    debugModule.debug.node := device_xbar.get
    // TODO: l3_xbar
    debugModule.debug.dmInner.dmInner.sb2tlOpt.foreach { sb2tl =>
      error_xbar.get := sb2tl.node
    }
  } else {
    debugModule.debug.node := peripheralXbar.get
    debugModule.debug.dmInner.dmInner.sb2tlOpt.foreach { sb2tl  =>
      l3_xbar.get := TLBuffer() := sb2tl.node
    }
  }

  val pma = LazyModule(new TLPMA)
  if (enableCHI) {
    pma.node := TLBuffer.chainNode(4) := device_xbar.get
  } else {
    pma.node := TLBuffer.chainNode(4) := peripheralXbar.get
  }

  class SoCMiscImp(wrapper: LazyModule) extends LazyModuleImp(wrapper) {

    val debug_module_io = IO(new debugModule.DebugModuleIO)
    val ext_intrs = IO(Input(UInt(NrExtIntr.W)))
    val rtc_clock = IO(Input(Bool()))
    val pll0_lock = IO(Input(Bool()))
    val pll0_ctrl = IO(Output(Vec(6, UInt(32.W))))
    val cacheable_check = IO(new TLPMAIO)
    val clintTime = IO(Output(ValidIO(UInt(64.W))))

    debugModule.module.io <> debug_module_io

    // sync external interrupts
    require(plicSource.module.in.length == ext_intrs.getWidth)
    for ((plic_in, interrupt) <- plicSource.module.in.zip(ext_intrs.asBools)) {
      val ext_intr_sync = RegInit(0.U(3.W))
      ext_intr_sync := Cat(ext_intr_sync(1, 0), interrupt)
      plic_in := ext_intr_sync(2)
    }

    pma.module.io <> cacheable_check

    // positive edge sampling of the lower-speed rtc_clock
    val rtcTick = RegInit(0.U(3.W))
    rtcTick := Cat(rtcTick(1, 0), rtc_clock)
    clint.module.io.rtcTick := rtcTick(1) && !rtcTick(2)

    val pll_ctrl_regs = Seq.fill(6){ RegInit(0.U(32.W)) }
    val pll_lock = RegNext(next = pll0_lock, init = false.B)

    clintTime := clint.module.io.time

    pll0_ctrl <> VecInit(pll_ctrl_regs)

    pll_node.regmap(
      0x000 -> RegFieldGroup(
        "Pll", Some("PLL ctrl regs"),
        pll_ctrl_regs.zipWithIndex.map{
          case (r, i) => RegField(32, r, RegFieldDesc(
            s"PLL_ctrl_$i",
            desc = s"PLL ctrl register #$i"
          ))
        } :+ RegField.r(32, Cat(0.U(31.W), pll_lock), RegFieldDesc(
          "PLL_lock",
          "PLL lock register"
        ))
      )
    )
  }

  lazy val module = new SoCMiscImp(this)
}

class SoCMisc()(implicit p: Parameters) extends MemMisc
  with HaveSlaveAXI4Port

