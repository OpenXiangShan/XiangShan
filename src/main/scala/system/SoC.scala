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
import device.{DebugModule, TLPMA, TLPMAIO}
import freechips.rocketchip.devices.tilelink.{CLINT, CLINTParams, DevNullParams, PLICParams, TLError, TLPLIC}
import freechips.rocketchip.diplomacy.{AddressSet, IdRange, InModuleBody, LazyModule, LazyModuleImp, MemoryDevice, RegionType, SimpleDevice, TransferSizes}
import freechips.rocketchip.interrupts.{IntSourceNode, IntSourcePortSimple}
import freechips.rocketchip.regmapper.{RegField, RegFieldAccessType, RegFieldDesc, RegFieldGroup}
import utils.{BinaryArbiter, TLEdgeBuffer}
import xiangshan.{DebugOptionsKey, HasXSParameter, XSBundle, XSCore, XSCoreParameters, XSTileKey}
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.tilelink._
import top.BusPerfMonitor
import xiangshan.backend.fu.PMAConst
import huancun._
import huancun.debug.TLLogger

case object SoCParamsKey extends Field[SoCParameters]

case class SoCParameters
(
  EnableILA: Boolean = false,
  PAddrBits: Int = 36,
  extIntrs: Int = 64,
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
  private val errorDevice = LazyModule(new TLError(
    params = DevNullParams(
      address = Seq(AddressSet(0x0, 0x7fffffffL)),
      maxAtomic = 8,
      maxTransfer = 64),
    beatBytes = L3InnerBusWidth / 8
  ))
  private val error_xbar = TLXbar()

  l3_xbar :=
    TLFIFOFixer() :=
    TLWidthWidget(32) :=
    AXI4ToTL() :=
    AXI4UserYanker(Some(1)) :=
    AXI4Fragmenter() :=
    AXI4Buffer() :=
    AXI4Buffer() :=
    AXI4IdIndexer(1) :=
    l3FrontendAXI4Node
  errorDevice.node := l3_xbar

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

  val buffers = Seq.fill(125){ TLBuffer() }
  (buffers.init zip buffers.tail) foreach { case (curr, succ) =>
    curr := succ
  }
  println(s"add dummy delay ${buffers.size * 2}")

  memAXI4SlaveNode :=
    AXI4Buffer() :=
    AXI4Buffer() :=
    AXI4Buffer() :=
    AXI4IdIndexer(idBits = 14) :=
    AXI4UserYanker() :=
    AXI4Deinterleaver(L3BlockSize) :=
    TLToAXI4() :=
    buffers.head

  buffers.last :=
    TLSourceShrinker(64) :=
    TLWidthWidget(L3OuterBusWidth / 8) :=
    TLBuffer.chainNode(2) :=
    mem_xbar

  val memory = InModuleBody {
    memAXI4SlaveNode.makeIOs()
  }
}

trait HaveAXI4PeripheralPort { this: BaseSoC =>
  // on-chip devices: 0x3800_0000 - 0x3fff_ffff 0x0000_0000 - 0x0000_0fff
  val onChipPeripheralRange = AddressSet(0x38000000L, 0x07ffffffL)
  val uartRange = AddressSet(0x40600000, 0xf)
  val uartDevice = new SimpleDevice("serial", Seq("xilinx,uartlite"))
  val uartParams = AXI4SlaveParameters(
    address = Seq(uartRange),
    regionType = RegionType.UNCACHED,
    supportsRead = TransferSizes(1, 8),
    supportsWrite = TransferSizes(1, 8),
    resources = uartDevice.reg
  )
  val peripheralRange = AddressSet(
    0x0, 0x7fffffff
  ).subtract(onChipPeripheralRange).flatMap(x => x.subtract(uartRange))
  val peripheralNode = AXI4SlaveNode(Seq(AXI4SlavePortParameters(
    Seq(AXI4SlaveParameters(
      address = peripheralRange,
      regionType = RegionType.UNCACHED,
      supportsRead = TransferSizes(1, 8),
      supportsWrite = TransferSizes(1, 8),
      interleavedId = Some(0)
    ), uartParams),
    beatBytes = 8
  )))

  peripheralNode :=
    AXI4IdIndexer(idBits = 2) :=
    AXI4Buffer() :=
    AXI4Buffer() :=
    AXI4Buffer() :=
    AXI4Buffer() :=
    AXI4UserYanker() :=
    AXI4Deinterleaver(8) :=
    TLToAXI4() :=
    TLBuffer.chainNode(3) :=
    peripheralXbar

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

  val clint = LazyModule(new CLINT(CLINTParams(0x38000000L), 8))
  clint.node := peripheralXbar

  class IntSourceNodeToModule(val num: Int)(implicit p: Parameters) extends LazyModule {
    val sourceNode = IntSourceNode(IntSourcePortSimple(num, ports = 1, sources = 1))
    lazy val module = new LazyModuleImp(this){
      val in = IO(Input(Vec(num, Bool())))
      in.zip(sourceNode.out.head._1).foreach{ case (i, s) => s := i }
    }
  }

  val plic = LazyModule(new TLPLIC(PLICParams(0x3c000000L), 8))
  val plicSource = LazyModule(new IntSourceNodeToModule(NrExtIntr))

  plic.intnode := plicSource.sourceNode
  plic.node := peripheralXbar

  val pll_node = TLRegisterNode(
    address = Seq(AddressSet(0x3a000000L, 0xfff)),
    device = new SimpleDevice("pll_ctrl", Seq()),
    beatBytes = 8,
    concurrency = 1
  )
  pll_node := peripheralXbar

  val debugModule = LazyModule(new DebugModule(NumCores)(p))
  debugModule.debug.node := peripheralXbar
  debugModule.debug.dmInner.dmInner.sb2tlOpt.foreach { sb2tl  =>
    l3_xbar := TLBuffer() := sb2tl.node
  }

  val pma = LazyModule(new TLPMA)
  pma.node := 
    TLBuffer.chainNode(4) :=
    peripheralXbar

  lazy val module = new LazyModuleImp(this){

    val debug_module_io = IO(chiselTypeOf(debugModule.module.io))
    val ext_intrs = IO(Input(UInt(NrExtIntr.W)))
    val pll0_lock = IO(Input(Bool()))
    val pll0_ctrl = IO(Output(Vec(6, UInt(32.W))))
    val cacheable_check = IO(new TLPMAIO)

    val ext_intrs_sync = RegNext(RegNext(RegNext(ext_intrs)))
    val ext_intrs_wire = Wire(UInt(NrExtIntr.W))
    ext_intrs_wire := ext_intrs_sync
    debugModule.module.io <> debug_module_io
    plicSource.module.in := ext_intrs_wire.asBools
    pma.module.io <> cacheable_check

    val freq = 100
    val cnt = RegInit(freq.U)
    val tick = cnt === 0.U
    cnt := Mux(tick, freq.U, cnt - 1.U)
    clint.module.io.rtcTick := tick

    val pll_ctrl_regs = Seq.fill(6){ RegInit(0.U(32.W)) }
    val pll_lock = RegNext(next = pll0_lock, init = false.B)

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
}
