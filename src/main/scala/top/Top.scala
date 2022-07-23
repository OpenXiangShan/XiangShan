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
import huancun.mbist._
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
      val clock_div2 = Input(Clock())
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
      val in_spare = Input(UInt(10.W))
      val out_spare = Output(UInt(10.W))
    })

    val xsx_fscan = IO(new UltiscanExternalInterface)

    val mem = IO(new Bundle{
      val core_sram       = new MbitsExtraFullInterface
      val core_rf         = new MbitsExtraFullInterface
      val l2_sram         = new MbitsExtraFullInterface
      val l2_rf           = new MbitsExtraFullInterface
      val l3_banks        = Vec(4,new MbitsExtraFullInterface)
      val l3_dir          = Vec(4,new MbitsExtraFullInterface)
    })
    mem := DontCare
    val hd2prf_in = IO(new MbitsFuseInterface(isSRAM = false))
    val hsuspsr_in = IO(new MbitsFuseInterface(isSRAM = true))
    val uhdusplr_in = IO(new MbitsFuseInterface(isSRAM = true))
    val hduspsr_in = IO(new MbitsFuseInterface(isSRAM = true))

    val L3_BISR = if (l3cacheOpt.nonEmpty) Some(IO(Vec(4,new BISRInputInterface))) else None
    val bisr_mem_chain_select = if (l3cacheOpt.nonEmpty) Some(IO(Input(UInt(1.W)))) else None

    val dfx_reset = Some(xsx_fscan.toResetGen)
    val reset_sync = withClockAndReset(io.clock, io.reset) { ResetGen(2, dfx_reset) }
    val jtag_reset_sync = withClockAndReset(io.systemjtag.jtag.TCK, io.systemjtag.reset) { ResetGen(2, dfx_reset) }

    // override LazyRawModuleImp's clock and reset
    childClock := io.clock
    childReset := reset_sync

    // core_with_l2 still use io_clock and io_reset
    core_with_l2.foreach(_.module.io.clock := io.clock)
    core_with_l2.foreach(_.module.io.reset := io.reset)

    io.debug_reset := misc.module.debug_module_io.debugIO.ndreset
    io.out_spare := DontCare

    dontTouch(hd2prf_in)
    dontTouch(hsuspsr_in)
    dontTouch(uhdusplr_in)
    dontTouch(hduspsr_in)
    dontTouch(L3_BISR.get)
    dontTouch(dma)
    dontTouch(io)
    dontTouch(peripheral)
    dontTouch(memory)
    dontTouch(mem)
    dontTouch(xsx_fscan)
    misc.module.ext_intrs := io.extIntrs
    misc.module.rtc_clock := io.rtc_clock
    misc.module.clock_div2 := io.clock_div2
    misc.module.reset_no_sync := io.reset
    misc.module.dfx_reset := dfx_reset.get

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

    val xsl2_ultiscan = IO(core_with_l2.head.module.ultiscanIO.cloneType)
    dontTouch(xsl2_ultiscan)

    core_with_l2.head.module.ultiscanIO <> xsl2_ultiscan
    core_with_l2.head.module.xsx_ultiscan_in.bypsel := xsx_fscan.ram.bypsel
    core_with_l2.head.module.xsx_ultiscan_in.wdis_b := xsx_fscan.ram.wrdis_b
    core_with_l2.head.module.xsx_ultiscan_in.rdis_b := xsx_fscan.ram.rddis_b
    core_with_l2.head.module.xsx_ultiscan_in.init_en := xsx_fscan.ram.init_en
    core_with_l2.head.module.xsx_ultiscan_in.init_val := xsx_fscan.ram.init_val

    core_with_l2.head.module.mbist_extra_core_sram <> mem.core_sram
    core_with_l2.head.module.mbist_extra_core_rf <> mem.core_rf
    core_with_l2.head.module.mbist_extra_l2_sram <> mem.l2_sram
    core_with_l2.head.module.mbist_extra_l2_rf <> mem.l2_rf

    core_with_l2.head.module.hd2prf_in <> hd2prf_in
    core_with_l2.head.module.hsuspsr_in <> hsuspsr_in
    core_with_l2.head.module.uhdusplr_in <> uhdusplr_in
    core_with_l2.head.module.hduspsr_in <> hduspsr_in


    val l1l2_mbist_sram_jtag = IO(core_with_l2.head.module.mbist_ijtag.cloneType)
    dontTouch(l1l2_mbist_sram_jtag)
    core_with_l2.head.module.mbist_ijtag <> l1l2_mbist_sram_jtag

    val l3SliceNum = l3cacheOpt.get.module.mbist_jtag.get.length
    val l3_sram_mbist = if(l3cacheOpt.nonEmpty) Some(IO(Vec(l3SliceNum, new JTAGInterface))) else None
    if(l3cacheOpt.nonEmpty){
      val l3Module = l3cacheOpt.get.module
      l3Module.io.dfx_reset := dfx_reset.get
      mem.l3_dir.zip(l3Module.mbist_extra_dirs.get).foreach({ case(memIO,cacheIO) => memIO <> cacheIO})
      mem.l3_banks.zip(l3Module.mbist_extra_banks.get).foreach({ case(memIO,cacheIO) => memIO <> cacheIO})
      l3Module.fscan_clkungate.get := xsx_fscan.clkungate
      l3Module.xsx_ultiscan.get.bypsel <> xsx_fscan.ram.bypsel
      l3Module.xsx_ultiscan.get.wdis_b <> xsx_fscan.ram.wrdis_b
      l3Module.xsx_ultiscan.get.rdis_b <> xsx_fscan.ram.rddis_b
      l3Module.xsx_ultiscan.get.init_en <> xsx_fscan.ram.init_en
      l3Module.xsx_ultiscan.get.init_val <> xsx_fscan.ram.init_val
      l3Module.xsl2_ultiscan.get <> core_with_l2.head.module.xsl2_ultiscan_out
      l3Module.hd2prf_in.get <> hd2prf_in
      l3Module.hsuspsr_in.get <> hsuspsr_in
      l3Module.uhdusplr_in.get <> uhdusplr_in
      l3Module.hduspsr_in.get <> hduspsr_in
      l3Module.bisr.get.zip(L3_BISR.get).foreach({ case(extIO,cacheIO) => extIO <> cacheIO})
      l3Module.mbist_jtag.get.zip(l3_sram_mbist.get).foreach({ case(extIO,cacheIO) => extIO <> cacheIO})
      l3Module.bisr_mem_chain_select.get := bisr_mem_chain_select.get
    }
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
