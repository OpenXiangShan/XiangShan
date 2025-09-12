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
import utility._
import utility.sram.SramBroadcastBundle
import huancun.{HCCacheParameters, HCCacheParamsKey, HuanCun, PrefetchRecv, TPmetaResp}
import coupledL2.EnableCHI
import coupledL2.tl2chi.CHILogger
import openLLC.{OpenLLC, OpenLLCParamKey, OpenNCB}
import openLLC.TargetBinder._
import cc.xiangshan.openncb._
import system._
import device._
import chisel3.stage.ChiselGeneratorAnnotation
import org.chipsalliance.cde.config._
import freechips.rocketchip.devices.debug.DebugModuleKey
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.jtag.JTAGIO
import chisel3.experimental.{annotate, ChiselAnnotation}
import sifive.enterprise.firrtl.NestedPrefixModulesAnnotation
import scala.collection.mutable.{Map}

import difftest.common.DifftestWiring
import difftest.util.Profile

abstract class BaseXSSoc()(implicit p: Parameters) extends LazyModule
  with HasSoCParameter
  with BindingScope
{
  lazy val tlManagers: List[TLNexusNode] = List()
  lazy val dts = DTS(bindingTree)
  lazy val json = JSON(bindingTree)

  // collect info for DTS
  ResourceBinding {
    val width = ResourceInt(2)
    val model = "xiangshan," + os.read(os.resource / "publishVersion")
    val compatible = "freechips,rocketchip-unknown"
    Resource(ResourceAnchors.root, "model").bind(ResourceString(model))
    Resource(ResourceAnchors.root, "compat").bind(ResourceString(compatible + "-dev"))
    Resource(ResourceAnchors.soc, "compat").bind(ResourceString(compatible + "-soc"))
    Resource(ResourceAnchors.root, "width").bind(width)
    Resource(ResourceAnchors.soc, "width").bind(width)
    Resource(ResourceAnchors.cpus, "width").bind(ResourceInt(1))
    def bindManagers(xbar: TLNexusNode) = {
      ManagerUnification(xbar.edges.in.head.manager.managers).foreach{ manager =>
        manager.resources.foreach(r => r.bind(manager.toResource))
      }
    }
    tlManagers.foreach(xbar => bindManagers(xbar))
  }
}

trait HasDTSImp[+L <: BaseXSSoc] { this: LazyRawModuleImp =>
  val dtsLM = wrapper.asInstanceOf[L]
  FileRegisters.add("dts", dtsLM.dts)
  FileRegisters.add("graphml", dtsLM.graphML)
  FileRegisters.add("json", dtsLM.json)
  FileRegisters.add("plusArgs", freechips.rocketchip.util.PlusArgArtefacts.serialize_cHeader())
}

class XSTop()(implicit p: Parameters) extends BaseXSSoc()
{
  val nocMisc = if (enableCHI) Some(LazyModule(new MemMisc())) else None
  val socMisc = if (!enableCHI) Some(LazyModule(new SoCMisc())) else None
  val misc: MemMisc = if (enableCHI) nocMisc.get else socMisc.get

  override lazy val tlManagers = List(
    misc.l3_xbar.map(_.asInstanceOf[TLNexusNode]),
    misc.peripheralXbar.map(_.asInstanceOf[TLNexusNode])
  ).flatten

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

  val chi_llcBridge_opt = Option.when(enableCHI)(
    LazyModule(new OpenNCB()(p.alter((site, here, up) => {
      case NCBParametersKey => new NCBParameters(
        outstandingDepth    = 64,
        axiMasterOrder      = EnumAXIMasterOrder.WriteAddress,
        readCompDMT         = false,
        writeCancelable     = false,
        writeNoError        = true,
        axiBurstAlwaysIncr  = true,
        chiDataCheck        = EnumCHIDataCheck.OddParity
      )
    })))
  )

  val chi_mmioBridge_opt = Seq.fill(NumCores)(Option.when(enableCHI)(
    LazyModule(new OpenNCB()(p.alter((site, here, up) => {
      case NCBParametersKey => new NCBParameters(
        outstandingDepth            = 32,
        axiMasterOrder              = EnumAXIMasterOrder.None,
        readCompDMT                 = false,
        writeCancelable             = false,
        writeNoError                = true,
        asEndpoint                  = false,
        acceptOrderEndpoint         = true,
        acceptMemAttrDevice         = true,
        readReceiptAfterAcception   = true,
        axiBurstAlwaysIncr          = true,
        chiDataCheck                = EnumCHIDataCheck.OddParity
      )
    })))
  ))

  // receive all prefetch req from cores
  val memblock_pf_recv_nodes: Seq[Option[BundleBridgeSink[PrefetchRecv]]] = core_with_l2.map(_.core_l3_pf_port).map{
    x => x.map(_ => BundleBridgeSink(Some(() => new PrefetchRecv)))
  }

  val l3_pf_sender_opt = soc.L3CacheParamsOpt.getOrElse(HCCacheParameters()).prefetch match {
    case Some(pf) => Some(BundleBridgeSource(() => new PrefetchRecv))
    case None => None
  }
  val nmiIntNode = IntSourceNode(IntSourcePortSimple(1, NumCores, (new NonmaskableInterruptIO).elements.size))
  val nmi = InModuleBody(nmiIntNode.makeIOs())

  for (i <- 0 until NumCores) {
    core_with_l2(i).clint_int_node := misc.timer.intnode
    core_with_l2(i).plic_int_node :*= misc.plic.intnode
    core_with_l2(i).debug_int_node := misc.debugModule.debug.dmOuter.dmOuter.intnode
    core_with_l2(i).nmi_int_node := nmiIntNode
    misc.plic.intnode := IntBuffer() := core_with_l2(i).beu_int_source
    if (!enableCHI) {
      misc.peripheral_ports.get(i) := core_with_l2(i).tl_uncache
    }
    core_with_l2(i).memory_port.foreach(port => (misc.core_to_l3_ports.get)(i) :=* port)
    memblock_pf_recv_nodes(i).map(recv => {
      println(s"Connecting Core_${i}'s L1 pf source to L3!")
      recv := core_with_l2(i).core_l3_pf_port.get
    })
    misc.SepTLXbarOpt.foreach { SepTLXbarOpt =>
      // SeperateTlBus can only be connected to DebugModule now in non-XSNoCTop environment
      println(s"SeparateDM: ${SeperateDM}")
      println(s"misc.SepTLXbarOpt: ${misc.SepTLXbarOpt}")
      require(core_with_l2(i).sep_tl_opt.isDefined)
      require(SeperateTLBusRanges.size >= 1)
      require(SeperateTLBusRanges.head.base <= p(DebugModuleKey).get.address.base)
      require(SeperateTLBusRanges.head.base <= p(SoCParamsKey).TIMERRange.base)
      SepTLXbarOpt := core_with_l2(i).sep_tl_opt.get
    }
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

  chi_llcBridge_opt match {
    case Some(ncb) =>
      misc.soc_xbar.get := ncb.axi4node
    case None =>
  }

  chi_mmioBridge_opt.foreach { e =>
    e match {
      case Some(ncb) =>
        misc.soc_xbar.get := ncb.axi4node
      case None =>
    }
  }

  class XSTopImp(wrapper: XSTop) extends LazyRawModuleImp(wrapper)
    with HasDTSImp[XSTop]
  {
    soc.XSTopPrefix.foreach { prefix =>
      val mod = this.toNamed
      annotate(new ChiselAnnotation {
        def toFirrtl = NestedPrefixModulesAnnotation(mod, prefix, true)
      })
    }

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
      val clock = Input(Clock())
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
      val rtc_clock = Input(Clock())
      val cacheable_check = new TLPMAIO()
      val riscv_halt = Output(Vec(NumCores, Bool()))
      val riscv_critical_error = Output(Vec(NumCores, Bool()))
      val riscv_rst_vec = Input(Vec(NumCores, UInt(soc.PAddrBits.W)))
      val traceCoreInterface = Vec(NumCores, new Bundle {
        val fromEncoder = Input(new Bundle {
          val enable = Bool()
          val stall  = Bool()
        })
        val toEncoder   = Output(new Bundle {
          val cause     = UInt(TraceCauseWidth.W)
          val tval      = UInt(TraceTvalWidth.W)
          val priv      = UInt(TracePrivWidth.W)
          val iaddr     = UInt((TraceTraceGroupNum * TraceIaddrWidth).W)
          val itype     = UInt((TraceTraceGroupNum * TraceItypeWidth).W)
          val iretire   = UInt((TraceTraceGroupNum * TraceIretireWidthCompressed).W)
          val ilastsize = UInt((TraceTraceGroupNum * TraceIlastsizeWidth).W)
        })
      })
    })

    val reset_sync = withClockAndReset(io.clock, io.reset) { ResetGen() }
    val jtag_reset_sync = withClockAndReset(io.systemjtag.jtag.TCK, io.systemjtag.reset) { ResetGen() }
    val chi_openllc_opt = Option.when(enableCHI)(
      withClockAndReset(io.clock, io.reset) {
        Module(new OpenLLC()(p.alter((site, here, up) => {
          case OpenLLCParamKey => soc.OpenLLCParamsOpt.get.copy(
            hartIds = tiles.map(_.HartId),
            FPGAPlatform = debugOpts.FPGAPlatform
          )
        })))
      }
    )

    // override LazyRawModuleImp's clock and reset
    childClock := io.clock
    childReset := reset_sync

    // output
    io.debug_reset := misc.module.debug_module_io.debugIO.ndreset

    // input
    dontTouch(io)
    dontTouch(memory)
    misc.module.ext_intrs := io.extIntrs
    misc.module.pll0_lock := io.pll0_lock
    misc.module.cacheable_check <> io.cacheable_check

    io.pll0_ctrl <> misc.module.pll0_ctrl

    val msiInfo = WireInit(0.U.asTypeOf(ValidIO(UInt(soc.IMSICParams.MSI_INFO_WIDTH.W))))
    // syscnt io input descrip
    val ref_reset_sync = withClockAndReset(io.rtc_clock, io.reset) { ResetGen() }
    misc.module.scntIO.update_en := false.B
    misc.module.scntIO.update_value := 0.U
    misc.module.scntIO.stop_en := false.B
    misc.module.rtc_clock := io.rtc_clock // syscnt clock
    misc.module.rtc_reset := ref_reset_sync.asAsyncReset
    misc.module.bus_clock := io.clock
    misc.module.bus_reset := io.reset



    for ((core, i) <- core_with_l2.zipWithIndex) {
      core.module.io.hartId := i.U
      core.module.io.msiInfo := msiInfo
      core.module.io.clintTime := misc.module.clintTime
      io.riscv_halt(i) := core.module.io.cpu_halt
      io.riscv_critical_error(i) := core.module.io.cpu_crtical_error
      // trace Interface
      val traceInterface = core.module.io.traceCoreInterface
      traceInterface.fromEncoder := io.traceCoreInterface(i).fromEncoder
      io.traceCoreInterface(i).toEncoder.priv := traceInterface.toEncoder.priv
      io.traceCoreInterface(i).toEncoder.cause := traceInterface.toEncoder.trap.cause
      io.traceCoreInterface(i).toEncoder.tval := traceInterface.toEncoder.trap.tval
      io.traceCoreInterface(i).toEncoder.iaddr := VecInit(traceInterface.toEncoder.groups.map(_.bits.iaddr)).asUInt
      io.traceCoreInterface(i).toEncoder.itype := VecInit(traceInterface.toEncoder.groups.map(_.bits.itype)).asUInt
      io.traceCoreInterface(i).toEncoder.iretire := VecInit(traceInterface.toEncoder.groups.map(_.bits.iretire)).asUInt
      io.traceCoreInterface(i).toEncoder.ilastsize := VecInit(traceInterface.toEncoder.groups.map(_.bits.ilastsize)).asUInt

      core.module.io.dft.foreach(dontTouch(_) := DontCare)
      core.module.io.dft_reset.foreach(dontTouch(_) := DontCare)
      core.module.io.reset_vector := io.riscv_rst_vec(i)
    }

    withClockAndReset(io.clock, io.reset) {
      Option.when(enableCHI)(true.B).foreach { _ =>
        for ((core, i) <- core_with_l2.zipWithIndex) {
          val mmioLogger = CHILogger(s"L2[${i}]_MMIO", true)
          val llcLogger = CHILogger(s"L2[${i}]_LLC", true)
          dontTouch(core.module.io.chi.get)
          bind(
            route(
              core.module.io.chi.get, Map((AddressSet(0x0L, 0x00007fffffffL), NumCores + i)) ++ AddressSet(0x0L,
              0xffffffffffffL).subtract(AddressSet(0x0L, 0x00007fffffffL)).map(addr => (addr, NumCores * 2)).toMap
            ),
            Map((NumCores + i) -> mmioLogger.io.up, (NumCores * 2) -> llcLogger.io.up)
          )
          chi_mmioBridge_opt(i).get.module.io.chi.connect(mmioLogger.io.down)
          chi_openllc_opt.get.io.rn(i) <> llcLogger.io.down
          require(core.module.io.chi.get.getWidth == llcLogger.io.up.getWidth)
          require(llcLogger.io.down.getWidth == chi_openllc_opt.get.io.rn(i).getWidth)
        }
        val memLogger = CHILogger(s"LLC_MEM", true)
        chi_openllc_opt.get.io.sn.connect(memLogger.io.up)
        chi_llcBridge_opt.get.module.io.chi.connect(memLogger.io.down)
        chi_openllc_opt.get.io.nodeID := (NumCores * 2).U
        chi_openllc_opt.foreach { l3 =>
          l3.io.debugTopDown.robHeadPaddr := core_with_l2.map(_.module.io.debugTopDown.robHeadPaddr)
        }
        core_with_l2.zip(chi_openllc_opt.get.io.debugTopDown.addrMatch).foreach { case (tile, l3Match) =>
          tile.module.io.debugTopDown.l3MissMatch := l3Match
        }
        core_with_l2.map(_.module.io.l3Miss := (if (chi_openllc_opt.nonEmpty) chi_openllc_opt.get.io.l3Miss else false.B))
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
        core_with_l2.foreach(_.module.io.l3Miss := l3.module.io.l3Miss)
      case None =>
    }

    (chi_openllc_opt, l3cacheOpt) match {
      case (None, None) =>
        core_with_l2.foreach(_.module.io.debugTopDown.l3MissMatch := false.B)
        core_with_l2.foreach(_.module.io.l3Miss := false.B)
      case _ =>
    }

    core_with_l2.zipWithIndex.foreach { case (tile, i) =>
      tile.module.io.nodeID.foreach { case nodeID =>
        nodeID := i.U
        dontTouch(nodeID)
      }
    }

    misc.module.debug_module_io.resetCtrl.hartIsInReset := core_with_l2.map(_.module.io.hartIsInReset)
    misc.module.debug_module_io.clock := io.clock
    misc.module.debug_module_io.reset := reset_sync

    misc.module.debug_module_io.debugIO.reset := misc.module.reset
    misc.module.debug_module_io.debugIO.clock := io.clock
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

    withClockAndReset(io.clock, reset_sync) {
      // Modules are reset one by one
      // reset ----> SYNC --> {SoCMisc, L3 Cache, Cores}
      val resetChain = Seq(Seq(misc.module) ++ l3cacheOpt.map(_.module))
      ResetGen(resetChain, reset_sync, !debugOpts.ResetGen)
      // Ensure that cores could be reset when DM disable `hartReset` or l3cacheOpt.isEmpty.
      val dmResetReqVec = misc.module.debug_module_io.resetCtrl.hartResetReq.getOrElse(0.U.asTypeOf(Vec(core_with_l2.map(_.module).length, Bool())))
      val syncResetCores = if(l3cacheOpt.nonEmpty) l3cacheOpt.map(_.module).get.reset.asBool else misc.module.reset.asBool
      (core_with_l2.map(_.module)).zip(dmResetReqVec).map { case(core, dmResetReq) =>
        ResetGen(Seq(Seq(core)), (syncResetCores || dmResetReq).asAsyncReset, !debugOpts.ResetGen)
      }
    }

  }

  lazy val module = new XSTopImp(this)
}

class XSTileDiffTop(implicit p: Parameters) extends XSTop {
  //TODO: need to keep the same module name as XSNoCDiffTop
  override lazy val desiredName: String = "XSTop"
  class XSTileDiffTopImp(wrapper: XSTop) extends XSTopImp(wrapper) {
    DifftestWiring.createAndConnectExtraIOs()
    Profile.generateJson("XiangShan")
  }

  override lazy val module = new XSTileDiffTopImp(this)
}

object TopMain extends App {
  val (config, firrtlOpts, firtoolOpts) = ArgParser.parse(args)

  // tools: init to close dpi-c when in fpga
  val envInFPGA = config(DebugOptionsKey).FPGAPlatform
  val enableDifftest = config(DebugOptionsKey).EnableDifftest || config(DebugOptionsKey).AlwaysBasicDiff
  val enableChiselDB = config(DebugOptionsKey).EnableChiselDB
  val enableConstantin = config(DebugOptionsKey).EnableConstantin
  Constantin.init(enableConstantin && !envInFPGA)
  ChiselDB.init(enableChiselDB && !envInFPGA)

  if (config(SoCParamsKey).UseXSNoCDiffTop) {
    val soc = DisableMonitors(p => LazyModule(new XSNoCDiffTop()(p)))(config)
    Generator.execute(firrtlOpts, soc.module, firtoolOpts)
  } else if (config(SoCParamsKey).UseXSTileDiffTop) {
    val soc = DisableMonitors(p => LazyModule(new XSTileDiffTop()(p)))(config)
    Generator.execute(firrtlOpts, soc.module, firtoolOpts)
  } else {
    val soc = if (config(SoCParamsKey).UseXSNoCTop)
      DisableMonitors(p => LazyModule(new XSNoCTop()(p)))(config)
    else
      DisableMonitors(p => LazyModule(new XSTop()(p)))(config)

    Generator.execute(firrtlOpts, soc.module, firtoolOpts)

    // generate difftest bundles (w/o DifftestTopIO)
    if (enableDifftest) {
      DifftestModule.finish("XiangShan", false)
    }
  }

  FileRegisters.write(fileDir = "./build", filePrefix = "XSTop.")
}
