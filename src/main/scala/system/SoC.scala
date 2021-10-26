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
import freechips.rocketchip.amba.axi4.{AXI4Buffer, AXI4Deinterleaver, AXI4Fragmenter, AXI4IdIndexer, AXI4MasterNode, AXI4MasterParameters, AXI4MasterPortParameters, AXI4SlaveNode, AXI4SlaveParameters, AXI4SlavePortParameters, AXI4ToTL, AXI4UserYanker}
import freechips.rocketchip.devices.tilelink.{CLINT, CLINTParams, DevNullParams, PLICParams, TLError, TLPLIC}
import freechips.rocketchip.diplomacy.{AddressSet, IdRange, InModuleBody, LazyModule, LazyModuleImp, MemoryDevice, RegionType, SimpleDevice, TransferSizes}
import freechips.rocketchip.interrupts.{IntSourceNode, IntSourcePortSimple}
import xiangshan.{DebugOptionsKey, HasXSParameter, XSBundle, XSCore, XSCoreParameters}
import freechips.rocketchip.tile.{BusErrorUnit, BusErrorUnitParams, BusErrors, L1BusErrors}
import freechips.rocketchip.tilelink.{BankBinder, TLBuffer, TLCacheCork, TLFIFOFixer, TLTempNode, TLToAXI4, TLWidthWidget, TLXbar}
import huancun.debug.TLLogger
import huancun.{CacheParameters, HCCacheParameters, BankedXbar}
import top.BusPerfMonitor

case object SoCParamsKey extends Field[SoCParameters]

case class SoCParameters
(
  cores: List[XSCoreParameters],
  EnableILA: Boolean = false,
  extIntrs: Int = 150,
  L3NBanks: Int = 4,
  L3CacheParamsOpt: Option[HCCacheParameters] = Some(HCCacheParameters(
    name = "l3",
    level = 3,
    ways = 8,
    sets = 2048 // 1MB per bank
  ))
){
  val PAddrBits = cores.map(_.PAddrBits).reduce((x, y) => if(x > y) x else y)
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
  val NumCores = soc.cores.size
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
  val l3_banked_xbar = BankedXbar(soc.cores.head.L2NBanks)
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

  error_xbar :=
    TLFIFOFixer() :=
    TLWidthWidget(16) :=
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
  this: BaseSoC =>
  val device = new MemoryDevice
  // 40-bit physical address
  val memRange = AddressSet(0x00000000L, 0xffffffffffL).subtract(AddressSet(0x0L, 0x7fffffffL))
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

  def mem_buffN(n: Int) = {
    val buffers = (0 until n).map(_ => AXI4Buffer())
    buffers.reduce((l, r) => l := r)
    (buffers.head, buffers.last)
  }
  val mem_xbar = TLXbar()
  mem_xbar :=* TLCacheCork() :=* bankedNode
  val (buf_l, buf_r) = mem_buffN(5)
  memAXI4SlaveNode := buf_l
  buf_r :=
    AXI4UserYanker() :=
    AXI4Deinterleaver(L3BlockSize) :=
    TLToAXI4() :=
    TLWidthWidget(L3OuterBusWidth / 8) :=
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
    AXI4UserYanker() :=
    AXI4Deinterleaver(8) :=
    TLToAXI4() :=
    peripheralXbar

  val peripheral = InModuleBody {
    peripheralNode.makeIOs()
  }

}

class SoCMisc()(implicit p: Parameters) extends BaseSoC
  with HaveAXI4MemPort
  with HaveAXI4PeripheralPort
  with HaveSlaveAXI4Port
{
  val peripheral_ports = Array.fill(NumCores) { TLTempNode() }
  val core_to_l3_ports = Array.fill(NumCores) { TLTempNode() }

  val l3_in = TLTempNode()
  val l3_out = TLTempNode()
  val l3_mem_pmu = BusPerfMonitor(enable = !debugOpts.FPGAPlatform)

  l3_in :*= l3_banked_xbar
  bankedNode :*= TLLogger("MEM_L3", !debugOpts.FPGAPlatform) :*= l3_mem_pmu :*= l3_out

  if(soc.L3CacheParamsOpt.isEmpty){
    l3_out :*= l3_in
  }

  for(port <- peripheral_ports) {
    peripheralXbar := port
  }

  for ((core_out, i) <- core_to_l3_ports.zipWithIndex){
    l3_banked_xbar :=* TLLogger(s"L3_L2_$i", !debugOpts.FPGAPlatform) :=* core_out
  }
  l3_banked_xbar :=* BankBinder(soc.cores.head.L2NBanks, L3BlockSize) :*= l3_xbar

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

  val debugModule = LazyModule(new DebugModule(NumCores)(p))
  debugModule.debug.node := peripheralXbar
  debugModule.debug.dmInner.dmInner.sb2tlOpt.foreach { sb2tl  =>
    l3_xbar := TLBuffer() := TLWidthWidget(1) := sb2tl.node
  }

  lazy val module = new LazyModuleImp(this){

    val debug_module_io = IO(chiselTypeOf(debugModule.module.io))
    val ext_intrs = IO(Input(UInt(NrExtIntr.W)))

    debugModule.module.io <> debug_module_io
    plicSource.module.in := ext_intrs.asBools

    val freq = 100
    val cnt = RegInit(freq.U)
    val tick = cnt === 0.U
    cnt := Mux(tick, freq.U, cnt - 1.U)
    clint.module.io.rtcTick := tick
  }
}
