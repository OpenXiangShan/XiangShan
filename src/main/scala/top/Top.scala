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
import device._
import chisel3.stage.ChiselGeneratorAnnotation
import chipsalliance.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.jtag.JTAGIO
import freechips.rocketchip.util.{ElaborationArtefacts, HasRocketChipStageUtils, UIntToOH1}
import huancun.{HCCacheParamsKey, HuanCun}

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

  val core_with_l2 = tiles.map(coreParams =>
    LazyModule(new XSTile()(p.alterPartial({
      case XSCoreParamsKey => coreParams
    })))
  )

  val l3cacheOpt = soc.L3CacheParamsOpt.map(l3param =>
    LazyModule(new HuanCun()(new Config((_, _, _) => {
      case HCCacheParamsKey => l3param.copy(enableTopDown = debugOpts.EnableTopDown)
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
      val clock = Input(Bool())
      val reset = Input(Bool())
      val sram_config = Input(UInt(16.W))
      val extIntrs = Input(UInt(NrExtIntr.W))
      val pll0_lock = Input(Bool())
      val pll0_ctrl = Output(Vec(6, UInt(32.W)))
      val systemjtag = new Bundle {
        val jtag = Flipped(new JTAGIO(hasTRSTn = false))
        val reset = Input(Bool()) // No reset allowed on top
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
    // override LazyRawModuleImp's clock and reset
    childClock := io.clock.asClock
    childReset := io.reset

    // output
    io.debug_reset := misc.module.debug_module_io.debugIO.ndreset

    // input
    dontTouch(dma)
    dontTouch(io)
    dontTouch(peripheral)
    dontTouch(memory)
    misc.module.ext_intrs := io.extIntrs
    misc.module.rtc_clock := io.rtc_clock
    misc.module.pll0_lock := io.pll0_lock
    misc.module.cacheable_check <> io.cacheable_check

    io.pll0_ctrl <> misc.module.pll0_ctrl

    for ((core, i) <- core_with_l2.zipWithIndex) {
      core.module.io.hartId := i.U
      io.riscv_halt(i) := core.module.io.cpu_halt
      core.module.io.reset_vector := io.riscv_rst_vec(i)
    }

    if(l3cacheOpt.isEmpty || l3cacheOpt.get.rst_nodes.isEmpty){
      // tie off core soft reset
      for(node <- core_rst_nodes){
        node.out.head._1 := false.B
      }
    }

    misc.module.debug_module_io.resetCtrl.hartIsInReset := core_with_l2.map(_.module.reset.asBool)
    misc.module.debug_module_io.clock := io.clock
    misc.module.debug_module_io.reset := io.reset

    // TODO: use synchronizer?
    misc.module.debug_module_io.debugIO.reset := io.systemjtag.reset
    misc.module.debug_module_io.debugIO.clock := io.clock.asClock
    // TODO: delay 3 cycles?
    misc.module.debug_module_io.debugIO.dmactiveAck := misc.module.debug_module_io.debugIO.dmactive
    // jtag connector
    misc.module.debug_module_io.debugIO.systemjtag.foreach { x =>
      x.jtag        <> io.systemjtag.jtag
      x.reset       := io.systemjtag.reset
      x.mfr_id      := io.systemjtag.mfr_id
      x.part_number := io.systemjtag.part_number
      x.version     := io.systemjtag.version
    }

    withClockAndReset(io.clock.asClock, io.reset) {
      // Modules are reset one by one
      // reset ----> SYNC --> {SoCMisc, L3 Cache, Cores}
      val resetChain = Seq(Seq(misc.module) ++ l3cacheOpt.map(_.module) ++ core_with_l2.map(_.module))
      ResetGen(resetChain, io.reset, !debugOpts.FPGAPlatform)
    }

  }
}

object TopMain extends App with HasRocketChipStageUtils {
  override def main(args: Array[String]): Unit = {
    val (config, firrtlOpts, firrtlComplier) = ArgParser.parse(args)
    val soc = DisableMonitors(p => LazyModule(new XSTop()(p)))(config)
    Generator.execute(firrtlOpts, soc.module, firrtlComplier)
    ElaborationArtefacts.files.foreach{ case (extension, contents) =>
      writeOutputFile("./build", s"XSTop.${extension}", contents())
    }
  }
}
