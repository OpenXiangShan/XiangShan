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

import chipsalliance.rocketchip.config._
import chisel3._
import chisel3.stage.ChiselGeneratorAnnotation
import chisel3.util.experimental.BoringUtils
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.jtag.JTAGIO
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.{ElaborationArtefacts, HasRocketChipStageUtils}
import huancun.mbist.{FUSEInterface, MBISTController, MBISTInterface, MbitsExtraInterface, Repair, Ultiscan}
import huancun.utils.{DFTResetGen, ResetGen, SRAMTemplate}
import huancun.{HCCacheParamsKey, HuanCun}
import system._
import utils._
import xiangshan._
import xstransforms.ModulePrefixAnnotation

abstract class BaseXSSoc()(implicit p: Parameters) extends LazyModule
  with BindingScope
{
  val misc = LazyModule(new SoCMisc())
  lazy val dts = DTS(bindingTree)
  lazy val json = JSON(bindingTree)
}

class XSTop()(implicit p: Parameters) extends BaseXSSoc() with HasSoCParameter
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
    bindManagers(misc.l3_xbar.asInstanceOf[TLNexusNode])
    bindManagers(misc.peripheralXbar.asInstanceOf[TLNexusNode])
  }

  println(s"FPGASoC cores: $NumCores banks: $L3NBanks block size: $L3BlockSize bus size: $L3OuterBusWidth")

  val core_with_l2 = tiles.zipWithIndex.map({
    case(coreParams,idx) =>
      LazyModule(new XSTile(s"XSTop_CoreWithL2_${idx}_")(p.alterPartial({
        case XSCoreParamsKey => coreParams
      })))
  }
  )

  val l3cacheOpt = soc.L3CacheParamsOpt.map(l3param =>
    LazyModule(new HuanCun("XSTop_L3_")(new Config((_, _, _) => {
      case HCCacheParamsKey => l3param
    })))
  )

  for (i <- 0 until NumCores) {
    core_with_l2(i).clint_int_sink := misc.clint.intnode
    core_with_l2(i).plic_int_sink :*= misc.plic.intnode
    core_with_l2(i).debug_int_sink := misc.debugModule.debug.dmOuter.dmOuter.intnode
    misc.plic.intnode := IntBuffer() := core_with_l2(i).beu_int_source
    misc.peripheral_ports(i) := core_with_l2(i).uncache
    misc.core_to_l3_ports(i) :=* core_with_l2(i).memory_port
  }

  l3cacheOpt.map(_.ctlnode.map(_ := misc.peripheralXbar))
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
      misc.l3_out :*= l3.node :*= TLBuffer.chainNode(2) :*= misc.l3_banked_xbar
    case None =>
  }

  lazy val module = new LazyRawModuleImp(this) {
    ElaborationArtefacts.add("dts", dts)
    ElaborationArtefacts.add("graphml", graphML)
    ElaborationArtefacts.add("json", json)
    ElaborationArtefacts.add("plusArgs", freechips.rocketchip.util.PlusArgArtefacts.serialize_cHeader())

    val dma = IO(Flipped(misc.dma.cloneType))
    val peripheral = IO(misc.peripheral.cloneType)
    val memory = IO(misc.memory.cloneType)

    misc.dma <> dma
    peripheral <> misc.peripheral
    memory <> misc.memory

    val io = IO(new Bundle {
      val clock = Input(Clock())
      val reset = Input(AsyncReset())
      val extIntrs = Input(UInt(NrExtIntr.W))
      val systemjtag = new Bundle {
        val jtag = Flipped(new JTAGIO(hasTRSTn = false))
        val reset = Input(AsyncReset()) // No reset allowed on top
        val mfr_id = Input(UInt(11.W))
        val part_number = Input(UInt(16.W))
        val version = Input(UInt(4.W))
      }
      val debug_reset = Output(Bool())
      val rtc_clock = Input(Bool())
      val riscv_halt = Output(Vec(NumCores, Bool()))
      val riscv_rst_vec = Input(Vec(NumCores, UInt(38.W)))
    })
    
    val xsl2_fscan = IO(new Bundle{
      val mode = Input(Bool())
      val mode_atspeed = Input(Bool())
      val state = Input(Bool())

      val byplatrst_b = Input(Bool())
      val byprst_b = Input(Bool())
      val clkgenctrl = Input(UInt(2.W))
      val clkgenctrlen = Input(UInt(2.W))
      val clkungate = Input(Bool())
      val clkungate_syn = Input(Bool())
      val rstbypen = Input(Bool())
      val shiften = Input(Bool())

      val ram = new Bundle () {
        val bypsel = Input(Bool())
        val hold = Input(Bool())
        val init_en = Input(Bool())
        val init_val = Input(Bool())
        val mcp = Input(Bool())
        val odis_b = Input(Bool())
        val rddis_b = Input(Bool())
        val wrdis_b = Input(Bool())
      }
      val scanchains_so_end = Output(UInt((1100 - 2).W))
      val scanchains_si_bgn = Input(UInt((1100 - 2).W))
      val dftclken = Input(Bool())
      val core_clock_postclk = Input(Clock())

      def toResetGen: DFTResetGen = {
        val top_scan = Wire(new DFTResetGen)
        top_scan.scan_mode := rstbypen
        top_scan.dft_reset := !byprst_b
        top_scan.dft_mode := rstbypen
        top_scan
      }
    })

    val dfx_reset = Some(xsl2_fscan.toResetGen)
    val reset_sync = withClockAndReset(io.clock, io.reset) { ResetGen(2, dfx_reset) }
    val jtag_reset_sync = withClockAndReset(io.systemjtag.jtag.TCK, io.systemjtag.reset) { ResetGen(2, dfx_reset) }

    // override LazyRawModuleImp's clock and reset
    childClock := xsl2_fscan.core_clock_postclk
    childReset := reset_sync

    // core_with_l2 still use io_clock and io_reset
    core_with_l2.foreach(_.module.io.clock := io.clock)
    core_with_l2.foreach(_.module.io.reset := io.reset)

    // output
    io.debug_reset := misc.module.debug_module_io.debugIO.ndreset

    // input
    dontTouch(dma)
    dontTouch(io)
    dontTouch(peripheral)
    dontTouch(memory)
    misc.module.ext_intrs := io.extIntrs
    misc.module.rtc_clock := io.rtc_clock

    for ((core, i) <- core_with_l2.zipWithIndex) {
      core.module.io.hartId := i.U
      core.module.io.reset_vector := io.riscv_rst_vec(i)
      io.riscv_halt(i) := core.module.io.cpu_halt
    }

    if(l3cacheOpt.isEmpty || l3cacheOpt.get.rst_nodes.isEmpty){
      // tie off core soft reset
      for(node <- core_rst_nodes){
        node.out.head._1 := false.B.asAsyncReset
      }
    }

    // MBIST Interface Implementation begins

    val xsl2_ultiscan_ijtag = IO(core_with_l2.head.module.ultiscan_ijtag.cloneType)
    val xsl2_ultiscan_uscan = IO(core_with_l2.head.module.ultiscan_uscan.cloneType)
    dontTouch(xsl2_ultiscan_ijtag)
    dontTouch(xsl2_ultiscan_uscan)

    core_with_l2.head.module.ultiscan_ijtag <> xsl2_ultiscan_ijtag
    core_with_l2.head.module.ultiscan_uscan <> xsl2_ultiscan_uscan

    core_with_l2.head.module.ultiscanToControllerL2.bypsel := xsl2_fscan.ram.bypsel
    core_with_l2.head.module.ultiscanToControllerL2.wdis_b := xsl2_fscan.ram.wrdis_b
    core_with_l2.head.module.ultiscanToControllerL2.rdis_b := xsl2_fscan.ram.rddis_b
    core_with_l2.head.module.ultiscanToControllerL2.init_en := xsl2_fscan.ram.init_en
    core_with_l2.head.module.ultiscanToControllerL2.init_val := xsl2_fscan.ram.init_val

    val mbistExtra = IO(new MbitsExtraInterface)
    dontTouch(mbistExtra)
    core_with_l2.head.module.mbist_extra <> mbistExtra
    
    val mbistInterfacesL3SRAM = {
      if (l3cacheOpt.nonEmpty) {
        if(l3cacheOpt.get.module.mbist_sram.isDefined) {
          Some(
            l3cacheOpt.get.module.mbist_sram.get.zipWithIndex.map({
              case (port, idx) =>
                val intfName = f"MBIST_SRAM_L3_Slice_${idx}_intf"
                val intf = Module(new MBISTInterface(port.params, intfName))
                intf.toPipeline <> port
                intf.extra <> mbistExtra
                intf
            })
          )
        }else{
          None
        }
      } else {
          None
      }
    }


    val hd2prf_in = IO(new FUSEInterface)
    dontTouch(hd2prf_in)
    core_with_l2.head.module.hd2prf_in <> hd2prf_in
    val hsuspsr_in = IO(new FUSEInterface)
    dontTouch(hsuspsr_in)
    core_with_l2.head.module.hsuspsr_in <> hsuspsr_in
    val mbistControllersL3 = {
      if (l3cacheOpt.nonEmpty){
        val repairNodesList = Repair.globalRepairNode
        val mbistInterfacesL3 = mbistInterfacesL3SRAM.get
        require(repairNodesList.length % mbistInterfacesL3.length == 0)
        val repairNodesForEveyController = mbistInterfacesL3.indices.map({
          idx =>
            repairNodesList.filter(_.prefix.contains(s"slice${idx}"))
        })
        Some(
          mbistInterfacesL3.zip(repairNodesForEveyController).map({
            case (intf,nodes) =>
              val prefix = f"L3"
              val ctrl = Module(new MBISTController
              (
                Seq(intf.mbist.params),
                2,
                Seq(prefix),
                Some(nodes)
              ))
              dontTouch(ctrl.io)
              ctrl.repairPort.get.foreach(dontTouch(_))
              MBISTController.connectRepair(ctrl.repairPort.get,nodes)
              ctrl.io.mbist.head <> intf.mbist
              ctrl.io.fscan_ram.head <> intf.fscan_ram
              ctrl.io.static.head <> intf.static
              ctrl.io.fscan_clkungate := xsl2_fscan.clkungate
              ctrl.io.clock := childClock
              ctrl.io.hd2prf_in := hd2prf_in
              ctrl.io.hsuspsr_in := hsuspsr_in
              ctrl.io.fscan_in(0).bypsel := xsl2_fscan.ram.bypsel
              ctrl.io.fscan_in(0).wdis_b := xsl2_fscan.ram.wrdis_b
              ctrl.io.fscan_in(0).rdis_b := xsl2_fscan.ram.rddis_b
              ctrl.io.fscan_in(0).init_en := xsl2_fscan.ram.init_en
              ctrl.io.fscan_in(0).init_val := xsl2_fscan.ram.init_val
              ctrl.io.fscan_in(1) <> core_with_l2.head.module.ultiscanToControllerL3
              ctrl
          })
        )
      }
      else{
        None
      }
    }

    val l1l2_mbist_sram_jtag = IO(core_with_l2.head.module.mbist_ijtag.cloneType)
    dontTouch(l1l2_mbist_sram_jtag)
    core_with_l2.head.module.mbist_ijtag <> l1l2_mbist_sram_jtag

    val l3_sram_mbist = IO(new Bundle {
      val ijtag = {
        if(l3cacheOpt.nonEmpty) {
          val sramPortsNum = mbistInterfacesL3SRAM.get.length
          Some(Vec(sramPortsNum, mbistControllersL3.get.head.io.mbist_ijtag.cloneType))
        } else {
          None
        }
      }
    })
    if (l3_sram_mbist.ijtag.isDefined) {
      dontTouch(l3_sram_mbist.ijtag.get)
    }

    if(l3cacheOpt.nonEmpty){
      l3_sram_mbist.ijtag.get.zip(mbistControllersL3.get).foreach({
        case(port,ctrl) =>
          port <> ctrl.io.mbist_ijtag
      })
    }

    val PWR_MGMT_IN = IO(Input(UInt(6.W)))
    val PWR_MGMT_OUT = IO(Output(UInt(6.W)))
    BoringUtils.addSource(PWR_MGMT_IN, "SRAM0_PWR_MGMT")
    BoringUtils.addSink(PWR_MGMT_OUT, s"SRAM${SRAMTemplate.uniqueID}_PWR_MGMT")
    //MBIST Interface Implementation ends


    misc.module.debug_module_io.resetCtrl.hartIsInReset := core_with_l2.map(_.module.io.reset.asBool)
    misc.module.debug_module_io.clock := childClock.asBool
    misc.module.debug_module_io.reset := childReset

    // TODO: use synchronizer?
    misc.module.debug_module_io.debugIO.reset := jtag_reset_sync
    misc.module.debug_module_io.debugIO.clock := childClock
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

    withClockAndReset(childClock, childReset) {
      // Modules are reset one by one
      // reset ----> SYNC --> {SoCMisc, L3 Cache, Cores}
      val resetChain = Seq((Seq(misc.module) ++ l3cacheOpt.map(_.module)).map(_.reset) ++ core_with_l2.map(_.module.io.reset))
      ResetGen(resetChain, reset_sync, !debugOpts.FPGAPlatform, dfx_reset)
    }

  }
}

object TopMain extends App with HasRocketChipStageUtils {
  override def main(args: Array[String]): Unit = {
    val (config, firrtlOpts) = ArgParser.parse(args)
    val soc = DisableMonitors(p => LazyModule(new XSTop()(p)))(config)
    XiangShanStage.execute(firrtlOpts, Seq(
      ModulePrefixAnnotation("bosc_"),
      ChiselGeneratorAnnotation(() => {
        soc.module
      })
    ))
    ElaborationArtefacts.files.foreach{ case (extension, contents) =>
      writeOutputFile("./build", s"XSTop.${extension}", contents())
    }
  }
}
