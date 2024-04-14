/***************************************************************************************
* Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
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
import chisel3.experimental.dataview._
import difftest.DifftestModule
import xiangshan._
import utils._
import huancun.{HCCacheParameters, HCCacheParamsKey, HuanCun, PrefetchRecv, TPmetaResp}
import coupledL2.EnableCHI
import openLLC.DummyLLC
import utility._
import system._
import device._
import chisel3.stage.ChiselGeneratorAnnotation
import org.chipsalliance.cde.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.jtag.JTAGIO
import chisel3.experimental.{annotate, ChiselAnnotation}
import sifive.enterprise.firrtl.NestedPrefixModulesAnnotation

abstract class BaseXSSoc()(implicit p: Parameters) extends LazyModule
  with BindingScope
{
  // val misc = LazyModule(new SoCMisc())
  lazy val dts = DTS(bindingTree)
  lazy val json = JSON(bindingTree)
}

class XSTop()(implicit p: Parameters) extends BaseXSSoc() with HasSoCParameter
{
  val nocMisc = if (enableCHI) Some(LazyModule(new MemMisc())) else None
  val socMisc = if (!enableCHI) Some(LazyModule(new SoCMisc())) else None
  val misc: MemMisc = if (enableCHI) nocMisc.get else socMisc.get

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
    if (!enableCHI) {
      bindManagers(misc.l3_xbar.get.asInstanceOf[TLNexusNode])
      bindManagers(misc.peripheralXbar.get.asInstanceOf[TLNexusNode])
    }
  }

  println(s"FPGASoC cores: $NumCores banks: $L3NBanks block size: $L3BlockSize bus size: $L3OuterBusWidth")

  val core_with_l2 = tiles.map(coreParams =>
    LazyModule(new XSTile()(p.alter((site, here, up) => {
      case XSCoreParamsKey => coreParams
      case PerfCounterOptionsKey => up(PerfCounterOptionsKey).copy(perfDBHartID = coreParams.HartId)
    })))
  )

  val l3cacheOpt = soc.L3CacheParamsOpt.map(l3param =>
    LazyModule(new HuanCun()(new Config((_, _, _) => {
      case HCCacheParamsKey => l3param.copy(
        hartIds = tiles.map(_.HartId),
        FPGAPlatform = debugOpts.FPGAPlatform
      )
      case MaxHartIdBits => p(MaxHartIdBits)
      case LogUtilsOptionsKey => p(LogUtilsOptionsKey)
      case PerfCounterOptionsKey => p(PerfCounterOptionsKey)
    })))
  )

  val chi_dummyllc_opt = Option.when(enableCHI)(LazyModule(new DummyLLC(numRNs = NumCores)(p)))

  // receive all prefetch req from cores
  val memblock_pf_recv_nodes: Seq[Option[BundleBridgeSink[PrefetchRecv]]] = core_with_l2.map(_.core_l3_pf_port).map{
    x => x.map(_ => BundleBridgeSink(Some(() => new PrefetchRecv)))
  }

  val l3_pf_sender_opt = soc.L3CacheParamsOpt.getOrElse(HCCacheParameters()).prefetch match {
    case Some(pf) => Some(BundleBridgeSource(() => new PrefetchRecv))
    case None => None
  }

  for (i <- 0 until NumCores) {
    core_with_l2(i).clint_int_node := misc.clint.intnode
    core_with_l2(i).plic_int_node :*= misc.plic.intnode
    core_with_l2(i).debug_int_node := misc.debugModule.debug.dmOuter.dmOuter.intnode
    misc.plic.intnode := IntBuffer() := core_with_l2(i).beu_int_source
    if (!enableCHI) {
      misc.peripheral_ports.get(i) := core_with_l2(i).tl_uncache
    }
    core_with_l2(i).memory_port.foreach(port => (misc.core_to_l3_ports.get)(i) :=* port)
    memblock_pf_recv_nodes(i).map(recv => {
      println(s"Connecting Core_${i}'s L1 pf source to L3!")
      recv := core_with_l2(i).core_l3_pf_port.get
    })
  }

  l3cacheOpt.map(_.ctlnode.map(_ := misc.peripheralXbar.get))
  l3cacheOpt.map(_.intnode.map(int => {
    misc.plic.intnode := IntBuffer() := int
  }))

  val core_rst_nodes = if(l3cacheOpt.nonEmpty && l3cacheOpt.get.rst_nodes.nonEmpty){
    l3cacheOpt.get.rst_nodes.get
  } else {
    core_with_l2.map(_ => BundleBridgeSource(() => Reset()))
  }

  core_rst_nodes.zip(core_with_l2.map(_.core_reset_sink)).foreach({
    case (source, sink) =>  sink := source
  })

  l3cacheOpt match {
    case Some(l3) =>
      misc.l3_out :*= l3.node :*= misc.l3_banked_xbar.get
      l3.pf_recv_node.map(recv => {
        println("Connecting L1 prefetcher to L3!")
        recv := l3_pf_sender_opt.get
      })
      l3.tpmeta_recv_node.foreach(recv => {
        for ((core, i) <- core_with_l2.zipWithIndex) {
          println(s"Connecting core_$i\'s L2 TPmeta request to L3!")
          recv := core.core_l3_tpmeta_source_port.get
        }
      })
      l3.tpmeta_send_node.foreach(send => {
        val broadcast = LazyModule(new ValidIOBroadcast[TPmetaResp]())
        broadcast.node := send
        for ((core, i) <- core_with_l2.zipWithIndex) {
          println(s"Connecting core_$i\'s L2 TPmeta response to L3!")
          core.core_l3_tpmeta_sink_port.get := broadcast.node
        }
      })
    case None =>
  }

  chi_dummyllc_opt match {
    case Some(llc) =>
      misc.soc_xbar.get := llc.axi4node
    case None =>
  }

  class XSTopImp(wrapper: LazyModule) extends LazyRawModuleImp(wrapper) {
    soc.XSTopPrefix.foreach { prefix =>
      val mod = this.toNamed
      annotate(new ChiselAnnotation {
        def toFirrtl = NestedPrefixModulesAnnotation(mod, prefix, true)
      })
    }

    FileRegisters.add("dts", dts)
    FileRegisters.add("graphml", graphML)
    FileRegisters.add("json", json)
    FileRegisters.add("plusArgs", freechips.rocketchip.util.PlusArgArtefacts.serialize_cHeader())

    val dma = socMisc.map(m => IO(Flipped(new VerilogAXI4Record(m.dma.elts.head.params))))
    val peripheral = IO(new VerilogAXI4Record(misc.peripheral.elts.head.params))
    val memory = IO(new VerilogAXI4Record(misc.memory.elts.head.params))

    socMisc match {
      case Some(m) =>
        m.dma.elements.head._2 <> dma.get.viewAs[AXI4Bundle]
        dontTouch(dma.get)
      case None =>
    }

    memory.viewAs[AXI4Bundle] <> misc.memory.elements.head._2
    peripheral.viewAs[AXI4Bundle] <> misc.peripheral.elements.head._2

    val io = IO(new Bundle {
      val clock = Input(Bool())
      val reset = Input(AsyncReset())
      val sram_config = Input(UInt(16.W))
      val extIntrs = Input(UInt(NrExtIntr.W))
      val pll0_lock = Input(Bool())
      val pll0_ctrl = Output(Vec(6, UInt(32.W)))
      val systemjtag = new Bundle {
        val jtag = Flipped(new JTAGIO(hasTRSTn = false))
        val reset = Input(AsyncReset()) // No reset allowed on top
        val mfr_id = Input(UInt(11.W))
        val part_number = Input(UInt(16.W))
        val version = Input(UInt(4.W))
      }
      val debug_reset = Output(Bool())
      val rtc_clock = Input(Bool())
      val cacheable_check = new TLPMAIO()
      val riscv_halt = Output(Vec(NumCores, Bool()))
      val riscv_rst_vec = Input(Vec(NumCores, UInt(38.W)))
    })

    val reset_sync = withClockAndReset(io.clock.asClock, io.reset) { ResetGen() }
    val jtag_reset_sync = withClockAndReset(io.systemjtag.jtag.TCK, io.systemjtag.reset) { ResetGen() }

    // override LazyRawModuleImp's clock and reset
    childClock := io.clock.asClock
    childReset := reset_sync

    // output
    io.debug_reset := misc.module.debug_module_io.debugIO.ndreset

    // input
    dontTouch(io)
    dontTouch(memory)
    misc.module.ext_intrs := io.extIntrs
    misc.module.rtc_clock := io.rtc_clock
    misc.module.pll0_lock := io.pll0_lock
    misc.module.cacheable_check <> io.cacheable_check

    io.pll0_ctrl <> misc.module.pll0_ctrl

    val SetIpNumValidVec2 = WireInit(0.U(SetIpNumValidSize.W))
    val setIpNum = WireInit(0.U(log2Up(NumIRSrc).W))

    for ((core, i) <- core_with_l2.zipWithIndex) {
      core.module.io.hartId := i.U
      core.module.io.setIpNumValidVec2 := SetIpNumValidVec2
      core.module.io.setIpNum := setIpNum
      io.riscv_halt(i) := core.module.io.cpu_halt
      core.module.io.reset_vector := io.riscv_rst_vec(i)
      chi_dummyllc_opt.foreach { case llc =>
        llc.module.io.rn(i) <> core.module.io.chi.get
        core.module.io.nodeID.get := i.U // TODO
      }
    }

    if(l3cacheOpt.isEmpty || l3cacheOpt.get.rst_nodes.isEmpty){
      // tie off core soft reset
      for(node <- core_rst_nodes){
        node.out.head._1 := false.B.asAsyncReset
      }
    }

    l3cacheOpt match {
      case Some(l3) =>
        l3.pf_recv_node match {
          case Some(recv) =>
            l3_pf_sender_opt.get.out.head._1.addr_valid := VecInit(memblock_pf_recv_nodes.map(_.get.in.head._1.addr_valid)).asUInt.orR
            for (i <- 0 until NumCores) {
              when(memblock_pf_recv_nodes(i).get.in.head._1.addr_valid) {
                l3_pf_sender_opt.get.out.head._1.addr := memblock_pf_recv_nodes(i).get.in.head._1.addr
                l3_pf_sender_opt.get.out.head._1.l2_pf_en := memblock_pf_recv_nodes(i).get.in.head._1.l2_pf_en
              }
            }
          case None =>
        }
        l3.module.io.debugTopDown.robHeadPaddr := core_with_l2.map(_.module.io.debugTopDown.robHeadPaddr)
        core_with_l2.zip(l3.module.io.debugTopDown.addrMatch).foreach { case (tile, l3Match) => tile.module.io.debugTopDown.l3MissMatch := l3Match }
      case None => core_with_l2.foreach(_.module.io.debugTopDown.l3MissMatch := false.B)
    }

    core_with_l2.foreach { case tile =>
      tile.module.io.nodeID.foreach { case nodeID =>
        nodeID := DontCare
        dontTouch(nodeID)
      }
    }

    misc.module.debug_module_io.resetCtrl.hartIsInReset := core_with_l2.map(_.module.reset.asBool)
    misc.module.debug_module_io.clock := io.clock
    misc.module.debug_module_io.reset := reset_sync

    misc.module.debug_module_io.debugIO.reset := misc.module.reset
    misc.module.debug_module_io.debugIO.clock := io.clock.asClock
    // TODO: delay 3 cycles?
    misc.module.debug_module_io.debugIO.dmactiveAck := misc.module.debug_module_io.debugIO.dmactive
    // jtag connector
    misc.module.debug_module_io.debugIO.systemjtag.foreach { x =>
      x.jtag        <> io.systemjtag.jtag
      x.reset       := jtag_reset_sync
      x.mfr_id      := io.systemjtag.mfr_id
      x.part_number := io.systemjtag.part_number
      x.version     := io.systemjtag.version
    }

    withClockAndReset(io.clock.asClock, reset_sync) {
      // Modules are reset one by one
      // reset ----> SYNC --> {SoCMisc, L3 Cache, Cores}
      val resetChain = Seq(Seq(misc.module) ++ l3cacheOpt.map(_.module) ++ core_with_l2.map(_.module))
      ResetGen(resetChain, reset_sync, !debugOpts.ResetGen)
    }

  }

  lazy val module = new XSTopImp(this)
}

object TopMain extends App {
  val (config, firrtlOpts, firtoolOpts) = ArgParser.parse(args)

  // tools: init to close dpi-c when in fpga
  val envInFPGA = config(DebugOptionsKey).FPGAPlatform
  val enableDifftest = config(DebugOptionsKey).EnableDifftest
  val enableChiselDB = config(DebugOptionsKey).EnableChiselDB
  val enableConstantin = config(DebugOptionsKey).EnableConstantin
  Constantin.init(enableConstantin && !envInFPGA)
  ChiselDB.init(enableChiselDB && !envInFPGA)

  val soc = if (config(SoCParamsKey).UseXSNoCTop)
    DisableMonitors(p => LazyModule(new XSNoCTop()(p)))(config)
  else
    DisableMonitors(p => LazyModule(new XSTop()(p)))(config)

  Generator.execute(firrtlOpts, soc.module, firtoolOpts)

  // generate difftest bundles (w/o DifftestTopIO)
  if (enableDifftest) {
    DifftestModule.finish("XiangShan", false)
  }

  FileRegisters.write(fileDir = "./build", filePrefix = "XSTop.")
}
