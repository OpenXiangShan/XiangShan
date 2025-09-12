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
import device.{AXI4MemEncrypt, DebugModule, SYSCNT, SYSCNTConsts, SYSCNTParams, TIMER, TIMERConsts, TIMERParams, TLPMA, TLPMAIO}


import huancun._
import utility.{ReqSourceKey, TLClientsMerger, TLEdgeBuffer, TLLogger}
import coupledL2.{EnableCHI, L2Param}
import coupledL2.tl2chi.CHIIssue
import openLLC.OpenLLCParam
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.devices.debug.DebugModuleKey
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy.{AddressSet, IdRange, InModuleBody, LazyModule, LazyModuleImp, MemoryDevice, RegionType, SimpleDevice, TransferSizes}
import freechips.rocketchip.interrupts.{IntSourceNode, IntSourcePortSimple}
import freechips.rocketchip.regmapper.{RegField, RegFieldDesc, RegFieldGroup}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.{AsyncQueueParams}
import top.BusPerfMonitor
import xiangshan.backend.fu.{MemoryRange, PMAConfigEntry, PMAConst}
import xiangshan.{DebugOptionsKey, PMParameKey, XSTileKey}
import device.SYSCNTConsts.timeWidth

case object SoCParamsKey extends Field[SoCParameters]
case object CVMParamsKey extends Field[CVMParameters]

case class CVMParameters
(
  MEMENCRange: AddressSet = AddressSet(0x38030000L, 0xfff),
  KeyIDBits: Int = 0,
  MemencPipes: Int = 4,
  HasMEMencryption: Boolean = false,
  HasDelayNoencryption: Boolean = false, // Test specific
)

case class SoCParameters
(
  EnableILA: Boolean = false,
  PAddrBits: Int = 48,
  PmemRanges: Seq[MemoryRange] = Seq(MemoryRange(0x80000000L, 0x80000000000L)),
  PMAConfigs: Seq[PMAConfigEntry] = Seq(
    PMAConfigEntry(0x0L, range = 0x1000000000000L, a = 3),
    PMAConfigEntry(0x80000000000L, c = true, atomic = true, a = 1, x = true, w = true, r = true),
    PMAConfigEntry(0x80000000L, a = 1, w = true, r = true),
    PMAConfigEntry(0x3A000000L, a = 1),
    PMAConfigEntry(0x39002000L, a = 1, w = true, r = true),
    PMAConfigEntry(0x39000000L, a = 1, w = true, r = true),
    PMAConfigEntry(0x38022000L, a = 1, w = true, r = true),
    PMAConfigEntry(0x38021000L, a = 1, x = true, w = true, r = true),
    PMAConfigEntry(0x38020000L, a = 1, w = true, r = true),
    PMAConfigEntry(0x30050000L, a = 1, w = true, r = true), // FIXME: GPU space is cacheable?
    PMAConfigEntry(0x30010000L, a = 1, w = true, r = true),
    PMAConfigEntry(0x20000000L, a = 1, x = true, w = true, r = true),
    PMAConfigEntry(0x10000000L, a = 1, w = true, r = true),
    PMAConfigEntry(0)
  ),
  TIMERRange: AddressSet = AddressSet(0x38000000L, TIMERConsts.size - 1),
  SYSCNTRange: AddressSet = AddressSet(0x3a010000L, SYSCNTConsts.size - 1),
  BEURange: AddressSet = AddressSet(0x38010000L, 0xfff),
  PLICRange: AddressSet = AddressSet(0x3c000000L, PLICConsts.size(PLICConsts.maxMaxHarts) - 1),
  PLLRange: AddressSet = AddressSet(0x3a000000L, 0xfff),
  UARTLiteForDTS: Boolean = true, // should be false in SimMMIO
  extIntrs: Int = 64,
  L3NBanks: Int = 4,
  L3CacheParamsOpt: Option[HCCacheParameters] = Some(HCCacheParameters(
    name = "L3",
    level = 3,
    ways = 8,
    sets = 2048 // 1MB per bank
  )),
  OpenLLCParamsOpt: Option[OpenLLCParam] = None,
  XSTopPrefix: Option[String] = None,
  NodeIDWidthList: Map[String, Int] = Map(
    "B" -> 7,
    "C" -> 9,
    "E.b" -> 11
  ),
  NumHart: Int = 64,
  NumIRFiles: Int = 7,
  NumIRSrc: Int = 256,
  UseXSNoCTop: Boolean = false,
  UseXSNoCDiffTop: Boolean = false,
  UseXSTileDiffTop: Boolean = false,
  IMSICUseTL: Boolean = false,
  SeperateTLBus: Boolean = false,
  SeperateDM: Boolean = false, // for non-XSNoCTop only, should work with SeperateTLBus
  SeperateTLBusRanges: Seq[AddressSet] = Seq(),
  IMSICBusType: device.IMSICBusType.Value = device.IMSICBusType.AXI,
  IMSICParams: aia.IMSICParams = aia.IMSICParams(
    imsicIntSrcWidth = 8,
    mAddr = 0x3A800000,
    sgAddr = 0x3B000000,
    geilen = 5,
    vgeinWidth = 6,
    iselectWidth = 12,
    EnableImsicAsyncBridge = true,
    HasTEEIMSIC = false
  ),
  EnableCHIAsyncBridge: Option[AsyncQueueParams] = Some(AsyncQueueParams(depth = 16, sync = 3, safe = false)),
  EnableClintAsyncBridge: Option[AsyncQueueParams] = Some(AsyncQueueParams(depth = 8, sync = 3, safe = false)),
  SeperateTLAsyncBridge: Option[AsyncQueueParams] = Some(AsyncQueueParams(depth = 1, sync = 3, safe = false)),
  WFIClockGate: Boolean = false,
  EnablePowerDown: Boolean = false
){
  require(
    L3CacheParamsOpt.isDefined ^ OpenLLCParamsOpt.isDefined || L3CacheParamsOpt.isEmpty && OpenLLCParamsOpt.isEmpty,
    "Atmost one of L3CacheParamsOpt and OpenLLCParamsOpt should be defined"
  )
  // L3 configurations
  val L3InnerBusWidth = 256
  val L3BlockSize = 64
  // on chip network configurations
  val L3OuterBusWidth = 256
  val UARTLiteRange = AddressSet(0x40600000, if (UARTLiteForDTS) 0x3f else 0xf)
}

trait HasSoCParameter {
  implicit val p: Parameters

  val soc = p(SoCParamsKey)
  val cvm = p(CVMParamsKey)
  val debugOpts = p(DebugOptionsKey)
  val tiles = p(XSTileKey)
  val enableCHI = p(EnableCHI)
  val issue = p(CHIIssue)

  val NumCores = tiles.size
  val EnableILA = soc.EnableILA

  // Parameters for trace extension
  val TraceTraceGroupNum          = tiles.head.traceParams.TraceGroupNum
  val TraceCauseWidth             = tiles.head.XLEN
  val TraceTvalWidth              = tiles.head.traceParams.IaddrWidth
  val TracePrivWidth              = tiles.head.traceParams.PrivWidth
  val TraceIaddrWidth             = tiles.head.traceParams.IaddrWidth
  val TraceItypeWidth             = tiles.head.traceParams.ItypeWidth
  val TraceIretireWidthCompressed = log2Up(tiles.head.RenameWidth * tiles.head.CommitWidth * 2 + 1)
  val TraceIlastsizeWidth         = tiles.head.traceParams.IlastsizeWidth

  // L3 configurations
  val L3InnerBusWidth = soc.L3InnerBusWidth
  val L3BlockSize = soc.L3BlockSize
  val L3NBanks = soc.L3NBanks

  // on chip network configurations
  val L3OuterBusWidth = soc.L3OuterBusWidth

  val NrExtIntr = soc.extIntrs

  val SetIpNumValidSize = soc.NumHart * soc.NumIRFiles

  val NumIRSrc = soc.NumIRSrc

  val SeperateDM = soc.SeperateDM
  val SeperateTLBus = soc.SeperateTLBus
  val SeperateTLBusRanges = soc.SeperateTLBusRanges

  val EnableCHIAsyncBridge = if (enableCHI && soc.EnableCHIAsyncBridge.isDefined)
    soc.EnableCHIAsyncBridge else None
  val EnableClintAsyncBridge = soc.EnableClintAsyncBridge
  val SeperateTLAsyncBridge = if (SeperateTLBus && soc.SeperateTLAsyncBridge.isDefined)
    soc.SeperateTLAsyncBridge else None

  // seperate TL bus
  val EnableSeperateTLAsync = SeperateTLAsyncBridge.isDefined

  val WFIClockGate = soc.WFIClockGate
  val EnablePowerDown = soc.EnablePowerDown

  def HasMEMencryption = cvm.HasMEMencryption
  require((cvm.HasMEMencryption && (cvm.KeyIDBits > 0)) || (!cvm.HasMEMencryption),
    "HasMEMencryption most set with KeyIDBits > 0")
}

trait HasPeripheralRanges {
  implicit val p: Parameters

  private def cvm = p(CVMParamsKey)
  private def soc = p(SoCParamsKey)
  private def dm = p(DebugModuleKey)
  private def pmParams = p(PMParameKey)

  private def mmpma = pmParams.mmpma

  def onChipPeripheralRanges: Map[String, AddressSet] = Map(
    "TIMER" -> soc.TIMERRange,
    "SYSCNT" -> soc.SYSCNTRange,
    "BEU"   -> soc.BEURange,
    "PLIC"  -> soc.PLICRange,
    "PLL"   -> soc.PLLRange,
    "UART"  -> soc.UARTLiteRange,
    "DEBUG" -> dm.get.address,
    "MMPMA" -> AddressSet(mmpma.address, mmpma.mask)
  ) ++ (
    if (soc.L3CacheParamsOpt.map(_.ctrl.isDefined).getOrElse(false))
      Map("L3CTL" -> AddressSet(soc.L3CacheParamsOpt.get.ctrl.get.address, 0xffff))
    else
      Map()
  ) ++ (
    if (cvm.HasMEMencryption)
      Map("MEMENC"  -> cvm.MEMENCRange)
    else
      Map()
  )

  def peripheralRange = onChipPeripheralRanges.values.foldLeft(Seq(AddressSet(0x0, 0x7fffffffL))) { (acc, x) =>
    acc.flatMap(_.subtract(x))
  }
}

class ILABundle extends Bundle {}


abstract class BaseSoC()(implicit p: Parameters) extends LazyModule with HasSoCParameter with HasPeripheralRanges {
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
  // 48-bit physical address
  val memRange = AddressSet(0x00000000L, 0xffffffffffffL).subtract(AddressSet(0x0L, 0x7fffffffL))
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
  val axi4memencrpty = Option.when(HasMEMencryption)(LazyModule(new AXI4MemEncrypt(cvm.MEMENCRange)))
  if (HasMEMencryption) {
    memAXI4SlaveNode :=
      AXI4Buffer() :=
      AXI4Buffer() :=
      AXI4Buffer() :=
      AXI4IdIndexer(idBits = 14) :=
      AXI4UserYanker() :=
      axi4memencrpty.get.node

    axi4memencrpty.get.node :=
      AXI4Deinterleaver(L3BlockSize) :=
      axi4mem_node
  } else {
    memAXI4SlaveNode :=
      AXI4Buffer() :=
      AXI4Buffer() :=
      AXI4Buffer() :=
      AXI4IdIndexer(idBits = 14) :=
      AXI4UserYanker() :=
      AXI4Deinterleaver(L3BlockSize) :=
      axi4mem_node
  }


  val memory = InModuleBody {
    memAXI4SlaveNode.makeIOs()
  }
}

trait HaveAXI4PeripheralPort { this: BaseSoC =>
  val uartDevice = new SimpleDevice("serial", Seq("xilinx,uartlite"))
  val uartParams = AXI4SlaveParameters(
    address = Seq(soc.UARTLiteRange),
    regionType = RegionType.UNCACHED,
    supportsRead = TransferSizes(1, 32),
    supportsWrite = TransferSizes(1, 32),
    resources = uartDevice.reg
  )
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
        address = Seq(AddressSet(0x1000000000000L, 0xffffffffffffL)),
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

  // instant syscnt
  val syscnt = (LazyModule(new SYSCNT(SYSCNTParams(soc.SYSCNTRange.base), 8)))
  //  val clint = LazyModule(new CLINT(CLINTParams(soc.CLINTRange.base), 8))

  if (enableCHI) { syscnt.node := device_xbar.get }
  else { syscnt.node := peripheralXbar.get }

//  if (enableCHI) { clint.node := device_xbar.get }
//  else { clint.node := peripheralXbar.get }

  class IntSourceNodeToModule(val num: Int)(implicit p: Parameters) extends LazyModule {
    val sourceNode = IntSourceNode(IntSourcePortSimple(num, ports = 1, sources = 1))
    class IntSourceNodeToModuleImp(wrapper: LazyModule) extends LazyModuleImp(wrapper) {
      val in = IO(Input(Vec(num, Bool())))
      in.zip(sourceNode.out.head._1).foreach{ case (i, s) => s := i }
    }
    lazy val module = new IntSourceNodeToModuleImp(this)
  }

  val plic = LazyModule(new TLPLIC(PLICParams(soc.PLICRange.base), 8))
  val plicSource = LazyModule(new IntSourceNodeToModule(NrExtIntr))

  plic.intnode := plicSource.sourceNode
  if (enableCHI) { plic.node := device_xbar.get }
  else { plic.node := peripheralXbar.get }

  val pll_node = TLRegisterNode(
    address = Seq(soc.PLLRange),
    device = new SimpleDevice("pll_ctrl", Seq()),
    beatBytes = 8,
    concurrency = 1
  )
  if (enableCHI) { pll_node := device_xbar.get }
  else { pll_node := peripheralXbar.get }

  // instance timer
  val timer = LazyModule(new TIMER(TIMERParams(IsSelfTest = true, soc.TIMERRange.base), 8))
  val debugModule = LazyModule(new DebugModule(NumCores)(p))
  val SepTLXbarOpt = Option.when(SeperateTLBus)(TLXbar())
  if (enableCHI) {
    if (SeperateDM) {
      debugModule.debug.node := SepTLXbarOpt.get
    } else {
      debugModule.debug.node := device_xbar.get
    }
    debugModule.debug.dmInner.dmInner.sb2tlOpt.foreach { sb2tl =>
      error_xbar.get := sb2tl.node
    }
    if(SeperateTLBus){
      timer.node := SepTLXbarOpt.get
    } else{
      timer.node := device_xbar.get
    }
  } else {
    if (SeperateDM) {
      debugModule.debug.node := SepTLXbarOpt.get
    } else {
      debugModule.debug.node := peripheralXbar.get
    }
    debugModule.debug.dmInner.dmInner.sb2tlOpt.foreach { sb2tl  =>
      l3_xbar.get := TLBuffer() := TLWidthWidget(1) := sb2tl.node
    }
    if(SeperateTLBus){
      timer.node := SepTLXbarOpt.get
    } else{
      timer.node := peripheralXbar.get
    }
  }

  val pma = LazyModule(new TLPMA)
  if (enableCHI) {
    pma.node := TLBuffer.chainNode(4) := device_xbar.get
    if (HasMEMencryption) {
      axi4memencrpty.get.ctrl_node := TLToAPB() := device_xbar.get
    }
  } else {
    pma.node := TLBuffer.chainNode(4) := peripheralXbar.get
    if (HasMEMencryption) {
      axi4memencrpty.get.ctrl_node := TLToAPB() := peripheralXbar.get
    }
  }

  class SoCMiscImp(wrapper: LazyModule) extends LazyModuleImp(wrapper) {

    val debug_module_io = IO(new debugModule.DebugModuleIO)
    val ext_intrs = IO(Input(UInt(NrExtIntr.W)))
    val rtc_clock = IO(Input(Clock()))
    val rtc_reset = IO(Input(Reset()))
    val bus_clock = IO(Input(Clock()))
    val bus_reset = IO(Input(Reset()))
    val pll0_lock = IO(Input(Bool()))
    val pll0_ctrl = IO(Output(Vec(6, UInt(32.W))))
    val cacheable_check = IO(new TLPMAIO)
    val clintTime = IO(Output(ValidIO(UInt(64.W))))
    val scntIO = IO(new Bundle {
      val update_en = Input(Bool())
      val update_value = Input(UInt(timeWidth.W))
      val stop_en = Input(Bool())
    })
    debugModule.module.io <> debug_module_io

    // sync external interrupts
    require(plicSource.module.in.length == ext_intrs.getWidth)
    for ((plic_in, interrupt) <- plicSource.module.in.zip(ext_intrs.asBools)) {
      val ext_intr_sync = RegInit(0.U(3.W))
      ext_intr_sync := Cat(ext_intr_sync(1, 0), interrupt)
      plic_in := ext_intr_sync(2)
    }

    pma.module.io <> cacheable_check

    if (HasMEMencryption) {
      val cnt = Counter(true.B, 8)._1
      axi4memencrpty.get.module.io.random_val := axi4memencrpty.get.module.io.random_req && cnt(2).asBool
      axi4memencrpty.get.module.io.random_data := cnt(0).asBool
    }
    // positive edge sampling of the lower-speed rtc_clock
//    val rtcTick = RegInit(0.U(3.W))
//    rtcTick := Cat(rtcTick(1, 0), rtc_clock)
//    clint.module.io.rtcTick := rtcTick(1) && !rtcTick(2)

    val pll_ctrl_regs = Seq.fill(6){ RegInit(0.U(32.W)) }
    val pll_lock = RegNext(next = pll0_lock, init = false.B)

    // timer instance
    clintTime :=   syscnt.module.io.time // syscnt ->timeasync
    timer.module.io.time <> syscnt.module.io.time
    timer.module.io.hartId := 0.U

    // instance syscnt
    syscnt.module.rtc_clock := rtc_clock
    syscnt.module.rtc_reset := rtc_reset
    syscnt.module.bus_clock := bus_clock
    syscnt.module.bus_reset := bus_reset
    syscnt.module.io.update_en := scntIO.update_en
    syscnt.module.io.update_value := scntIO.update_value
    syscnt.module.io.stop_en := scntIO.stop_en

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

