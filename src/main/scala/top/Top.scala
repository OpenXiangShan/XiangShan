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

package top

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import system._
import chisel3.stage.ChiselGeneratorAnnotation
import chipsalliance.rocketchip.config._
import device.{AXI4Plic, TLTimer}
import firrtl.stage.RunFirrtlTransformAnnotation
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomaticobjectmodel.logicaltree.GenericLogicalTreeNode
import freechips.rocketchip.interrupts._
import freechips.rocketchip.stage.phases.GenerateArtefacts
import freechips.rocketchip.tile.{BusErrorUnit, BusErrorUnitParams, XLen}
import freechips.rocketchip.util.{ElaborationArtefacts, HasRocketChipStageUtils}
import sifive.blocks.inclusivecache.{CacheParameters, InclusiveCache, InclusiveCacheMicroParameters}
import xiangshan.cache.prefetch.L2Prefetcher


class XSCoreWithL2()(implicit p: Parameters) extends LazyModule
  with HasXSParameter with HasSoCParameter {
  private val core = LazyModule(new XSCore)
  private val l2prefetcher = LazyModule(new L2Prefetcher())
  private val l2xbar = TLXbar()
  private val l2cache = if (useFakeL2Cache) null else LazyModule(new InclusiveCache(
    CacheParameters(
      level = 2,
      ways = L2NWays,
      sets = L2NSets,
      blockBytes = L2BlockSize,
      beatBytes = L1BusWidth / 8, // beatBytes = l1BusDataWidth / 8
      cacheName = s"L2",
      uncachedGet = true,
      enablePerf = false
    ),
    InclusiveCacheMicroParameters(
      memCycles = 25,
      writeBytes = 32
    ),
    fpga = debugOpts.FPGAPlatform
  ))
  if(!useFakeL2Cache) {
    ResourceBinding {
      Resource(l2cache.device, "reg").bind(ResourceAddress(hardId))
    }
  }

  val memory_port = TLIdentityNode()
  val uncache = TLXbar()

  if (!useFakeDCache) {
    l2xbar := TLBuffer() := core.memBlock.dcache.clientNode
  }
  //if (!useFakeL1plusCache) {
    l2xbar := TLBuffer() := core.frontend.icache.clientNode
  //}
  if (!useFakePTW) {
    l2xbar := TLBuffer() := core.ptw.node
  }
  l2xbar := TLBuffer() := l2prefetcher.clientNode
  if (useFakeL2Cache) {
    memory_port := l2xbar
  }
  else {
    l2cache.node := TLBuffer() := l2xbar
    memory_port := l2cache.node
  }

  uncache := TLBuffer() := core.frontend.instrUncache.clientNode
  uncache := TLBuffer() := core.memBlock.uncache.clientNode

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val hartId = Input(UInt(64.W))
      val externalInterrupt = new ExternalInterruptIO
      val l1plus_error, icache_error, dcache_error = new L1CacheErrorInfo
    })

    core.module.io.hartId := io.hartId
    core.module.io.externalInterrupt := io.externalInterrupt
    l2prefetcher.module.io.enable := core.module.io.l2_pf_enable
    if (useFakeL2Cache) {
      l2prefetcher.module.io.in := DontCare
    }
    else {
      l2prefetcher.module.io.in <> l2cache.module.io
    }
    io.l1plus_error <> core.module.io.l1plus_error
    io.icache_error <> core.module.io.icache_error
    io.dcache_error <> core.module.io.dcache_error

    val core_reset_gen = Module(new ResetGen(1, !debugOpts.FPGAPlatform))
    core.module.reset := core_reset_gen.io.out

    val l2_reset_gen = Module(new ResetGen(1, !debugOpts.FPGAPlatform))
    l2prefetcher.module.reset := l2_reset_gen.io.out
    if (!useFakeL2Cache) {
      l2cache.module.reset := l2_reset_gen.io.out
    }
  }
}

abstract class BaseXSSoc()(implicit p: Parameters) extends LazyModule
  with HasSoCParameter
  with BindingScope
{
  val bankedNode = BankBinder(L3NBanks, L3BlockSize)
  val peripheralXbar = TLXbar()
  val l3_xbar = TLXbar()
  lazy val dts = DTS(bindingTree)
}

// We adapt the following three traits from rocket-chip.
// Source: rocket-chip/src/main/scala/subsystem/Ports.scala
trait HaveSlaveAXI4Port {
  this: BaseXSSoc =>

  val idBits = 16

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
  this: BaseXSSoc =>
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

  val mem_xbar = TLXbar()
  mem_xbar :=* TLBuffer() :=* TLCacheCork() :=* bankedNode
  memAXI4SlaveNode :=
    AXI4UserYanker() :=
    AXI4Deinterleaver(L3BlockSize) :=
    TLToAXI4() :=
    TLWidthWidget(L3OuterBusWidth / 8) :=
    mem_xbar

  val memory = InModuleBody {
    memAXI4SlaveNode.makeIOs()
  }
}


trait HaveAXI4PeripheralPort { this: BaseXSSoc =>
  // on-chip devices: 0x3800_000 - 0x3fff_ffff
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

class XSTop()(implicit p: Parameters) extends XSTopWithoutDMA
  with HaveSlaveAXI4Port

class XSTopWithoutDMA()(implicit p: Parameters) extends BaseXSSoc()
  with HaveAXI4MemPort
  with HaveAXI4PeripheralPort
{
  ResourceBinding {
    val width = ResourceInt(2)
    val model = "freechips,rocketchip-unknown"
    Resource(ResourceAnchors.root, "model").bind(ResourceString(model))
    Resource(ResourceAnchors.root, "compat").bind(ResourceString(model + "-dev"))
    Resource(ResourceAnchors.soc, "compat").bind(ResourceString(model + "-soc"))
    Resource(ResourceAnchors.root, "width").bind(width)
    Resource(ResourceAnchors.soc, "width").bind(width)
    Resource(ResourceAnchors.cpus, "width").bind(ResourceInt(1))
    def bindManagers(xbar: TLNexusNode) = {
      ManagerUnification(xbar.edges.in.head.manager.managers).foreach{ manager =>
        manager.resources.foreach(r => r.bind(manager.toResource))
      }
    }
    bindManagers(l3_xbar.asInstanceOf[TLNexusNode])
    bindManagers(peripheralXbar.asInstanceOf[TLNexusNode])
  }

  println(s"FPGASoC cores: $NumCores banks: $L3NBanks block size: $L3BlockSize bus size: $L3OuterBusWidth")

  val core_with_l2 = soc.cores.map(coreParams =>
    LazyModule(new XSCoreWithL2()(p.alterPartial({
      case XSCoreParamsKey => coreParams
    })))
  )

  for (i <- 0 until NumCores) {
    peripheralXbar := TLBuffer() := core_with_l2(i).uncache
    l3_xbar := TLBuffer() := core_with_l2(i).memory_port
  }

  val clint = LazyModule(new CLINT(CLINTParams(0x38000000L), 8))
  clint.node := peripheralXbar

  val clintIntSinks = Array.fill(NumCores){
    val clintSink = LazyModule(new IntSinkNodeToModule(2))
    clintSink.sinkNode := clint.intnode
    clintSink
  }

  val fakeTreeNode = new GenericLogicalTreeNode
  val beu = LazyModule(
    new BusErrorUnit(new XSL1BusErrors(NumCores), BusErrorUnitParams(0x38010000), fakeTreeNode))
  beu.node := peripheralXbar

  class IntSinkNodeToModule(val sinks: Int)(implicit p: Parameters) extends LazyModule {
    val sinkNode = IntSinkNode(IntSinkPortSimple(1, sinks))
    lazy val module = new LazyModuleImp(this){
      val out = IO(Output(Vec(sinks, Bool())))
      out.zip(sinkNode.in.head._1).foreach{ case (o, i) => o := i }
    }
  }

  class IntSourceNodeToModule(val num: Int)(implicit p: Parameters) extends LazyModule {
    val sourceNode = IntSourceNode(IntSourcePortSimple(num, ports = 1, sources = 1))
    lazy val module = new LazyModuleImp(this){
      val in = IO(Input(Vec(num, Bool())))
      in.zip(sourceNode.out.head._1).foreach{ case (i, s) => s := i }
    }
  }

  val plic = LazyModule(new TLPLIC(PLICParams(0x3c000000L), 8))
  val plicSource = LazyModule(new IntSourceNodeToModule(NrExtIntr))
  val plicIntSinks = Array.fill(NumCores){
    val plicSink = LazyModule(new IntSinkNodeToModule(1))
    plicSink.sinkNode := plic.intnode
    plicSink
  }
  plic.intnode := beu.intNode
  plic.intnode := plicSource.sourceNode
  
  plic.node := peripheralXbar

  val l3cache = if (useFakeL3Cache) null else LazyModule(new InclusiveCache(
    CacheParameters(
      level = 3,
      ways = L3NWays,
      sets = L3NSets,
      blockBytes = L3BlockSize,
      beatBytes = L3InnerBusWidth / 8,
      cacheName = "L3",
      uncachedGet = false,
      enablePerf = false
    ),
    InclusiveCacheMicroParameters(
      memCycles = 25,
      writeBytes = 32
    ),
    fpga = debugOpts.FPGAPlatform
  ))
  if(!useFakeL3Cache){
    ResourceBinding{
      Resource(l3cache.device, "reg").bind(ResourceAddress(0))
    }
  }
  val l3Ignore = if (useFakeL3Cache) TLIgnoreNode() else null

  if (useFakeL3Cache) {
    bankedNode :*= l3Ignore :*= l3_xbar
  }
  else {
    bankedNode :*= l3cache.node :*= TLBuffer() :*= l3_xbar
  }

  lazy val module = new LazyRawModuleImp(this) {
    ElaborationArtefacts.add("dts", dts)
    val io = IO(new Bundle {
      val clock = Input(Bool())
      val reset = Input(Bool())
      val extIntrs = Input(UInt(NrExtIntr.W))
      // val meip = Input(Vec(NumCores, Bool()))
      val ila = if(debugOpts.FPGAPlatform && EnableILA) Some(Output(new ILABundle)) else None
    })
    childClock := io.clock.asClock()

    withClockAndReset(childClock, io.reset) {
      val resetGen = Module(new ResetGen(1, !debugOpts.FPGAPlatform))
      resetGen.suggestName("top_reset_gen")
      childReset := resetGen.io.out
    }

    withClockAndReset(childClock, childReset) {
      plicSource.module.in := io.extIntrs.asBools()

      for (i <- 0 until NumCores) {
        val core_reset_gen = Module(new ResetGen(1, !debugOpts.FPGAPlatform))
        core_reset_gen.suggestName(s"core_${i}_reset_gen")
        core_with_l2(i).module.reset := core_reset_gen.io.out
        core_with_l2(i).module.io.hartId := i.U
        core_with_l2(i).module.io.externalInterrupt.msip := clintIntSinks(i).module.out(0)
        core_with_l2(i).module.io.externalInterrupt.mtip := clintIntSinks(i).module.out(1)
        core_with_l2(i).module.io.externalInterrupt.meip := plicIntSinks(i).module.out(0)
        beu.module.io.errors.l1plus(i) := core_with_l2(i).module.io.l1plus_error
        beu.module.io.errors.icache(i) := core_with_l2(i).module.io.icache_error
        beu.module.io.errors.dcache(i) := core_with_l2(i).module.io.dcache_error
      }

      if (!useFakeL3Cache) {
        val l3_reset_gen = Module(new ResetGen(1, !debugOpts.FPGAPlatform))
        l3_reset_gen.suggestName("l3_reset_gen")
        l3cache.module.reset := l3_reset_gen.io.out
      }
      // TODO: wrap this in a module
      val freq = 100
      val cnt = RegInit(freq.U)
      val tick = cnt === 0.U
      cnt := Mux(tick, freq.U, cnt - 1.U)
      clint.module.io.rtcTick := tick
    }
  }
}

object TopMain extends App with HasRocketChipStageUtils {
  override def main(args: Array[String]): Unit = {
    val (config, firrtlOpts) = ArgParser.parse(args)
    XiangShanStage.execute(firrtlOpts, Seq(
      ChiselGeneratorAnnotation(() => {
        val soc = LazyModule(new XSTop()(config))
        soc.module
      })
    ))
    ElaborationArtefacts.files.foreach{ case (extension, contents) =>
      writeOutputFile("./build", s"XSTop.${extension}", contents())
    }
  }
}
