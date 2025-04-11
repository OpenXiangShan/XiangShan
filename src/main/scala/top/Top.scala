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
import org.chipsalliance.cde.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.jtag.JTAGIO
import huancun.{HCCacheParamsKey, HuanCun}
import huancun.utils.ResetGen
import freechips.rocketchip.devices.debug.{DebugIO, ResetCtrlIO}
import utility.RegNextN

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

  // dse ctrl regs
  val dseCtrl = LazyModule(new DSECtrlUnit(DSEParams())(p.alter((site, here, up) => {
    case XSCoreParamsKey => tiles.head
  })))

  dseCtrl.ctrlnode := misc.peripheralXbar


  class XSTopImp(wrapper: LazyModule) extends LazyRawModuleImp(wrapper) {
    FileRegisters.add("dts", dts)
    FileRegisters.add("graphml", graphML)
    FileRegisters.add("json", json)
    FileRegisters.add("plusArgs", freechips.rocketchip.util.PlusArgArtefacts.serialize_cHeader())

    val dma = IO(Flipped(misc.dma.cloneType))
    val peripheral = IO(misc.peripheral.cloneType)
    val memory = IO(misc.memory.cloneType)

    misc.dma <> dma
    peripheral <> misc.peripheral
    memory <> misc.memory

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
      val cacheable_check = new TLPMAIO()
      val riscv_halt = Output(Vec(NumCores, Bool()))
      val instrCnt = Output(UInt(64.W))
      val dse_rst = Input(Reset())
      val reset_vector = Input(UInt(36.W))
      val dse_reset_valid = Output(Bool())
      val dse_reset_vec = Output(UInt(36.W))
      val dse_max_epoch = Output(UInt(64.W))
      val dse_epoch = Output(UInt(64.W))
      val dse_max_instr = Output(UInt(64.W))
    })

    val reset_sync = withClockAndReset(io.clock.asClock, io.reset.asBool.asAsyncReset) { ResetGen() }
    val dse_core_reset = withClockAndReset(io.clock.asClock, dseCtrl.module.io.core_reset.asAsyncReset) { ResetGen() }
    val dse_core_reset_delayed = withClock(io.clock.asClock) { RegNext(dseCtrl.module.io.core_reset) }
    val dse_reset_sync = withClockAndReset(io.clock.asClock, dse_core_reset_delayed.asAsyncReset) { ResetGen() }
    val true_reset_sync = (reset_sync.asBool || dse_reset_sync.asBool).asAsyncReset
    val jtag_reset_sync = withClockAndReset(io.systemjtag.jtag.TCK, io.systemjtag.reset) { ResetGen() }

    // override LazyRawModuleImp's clock and reset
    childClock := io.clock.asClock
    childReset := `true_reset_sync`

    // output
    io.debug_reset := misc.module.debug_module_io.debugIO.ndreset
    io.dse_reset_valid := dse_core_reset.asBool
    io.dse_reset_vec := dseCtrl.module.io.reset_vector
    io.dse_max_epoch := dseCtrl.module.io.max_epoch
    io.dse_epoch := dseCtrl.module.io.epoch
    io.dse_max_instr := dseCtrl.module.io.max_instr_cnt

    lazy val (io_perf, nr_perf, deg_valids_vec, deg_data, nrStructCnt) = HardenXSPerfAccumulate.reclaim(tiles.head.CommitWidth)
    lazy val param = HardenPerfConfig(tiles.head.CommitWidth)
    val deg_out = IO(Output(new BatchOutput(param)))
    val perf_out = IO(Output(new IOPerfOutput(nr_perf * (new PerfEventBundle).getWidth)))
    val deg_valids = IO(Output(UInt(tiles.head.CommitWidth.W)))
    withClockAndReset(io.clock.asClock, io.reset.asBool.asAsyncReset) {
      lazy val out = Batch(deg_data, deg_valids_vec, io_perf, nr_perf, param, nrStructCnt)
      deg_out := out._1
      perf_out := out._2
      deg_valids := RegNext(Cat(deg_valids_vec.reverse))
    }

    // input
    dontTouch(dma)
    dontTouch(io)
    dontTouch(peripheral)
    dontTouch(memory)
    dontTouch(perf_out)
    dontTouch(deg_valids)
    dontTouch(deg_out)
    misc.module.ext_intrs := io.extIntrs
    misc.module.pll0_lock := io.pll0_lock
    misc.module.cacheable_check <> io.cacheable_check

    io.pll0_ctrl <> misc.module.pll0_ctrl

//    val true_reset_vector = Mux(true_reset_sync.asBool, io.reset_vector, dseCtrl.module.io.reset_vector)
    val true_reset_vector = withClock(io.clock.asClock) {
      // RegEnable( Mux(reset_sync.asBool, io.reset_vector, dseCtrl.module.io.reset_vector),
      // enable = true_reset_sync.asBool)
      // RegNext(RegNext(Mux(reset_sync.asBool, io.reset_vector, dseCtrl.module.io.reset_vector)))
      RegNextN(Mux(reset_sync.asBool, io.reset_vector, dseCtrl.module.io.reset_vector), 20)
    }

    for ((core, i) <- core_with_l2.zipWithIndex) {
      core.module.io.hartId := i.U
      io.riscv_halt(i) := core.module.io.cpu_halt
      core.module.io.reset_vector := true_reset_vector
    }

    if(l3cacheOpt.isEmpty || l3cacheOpt.get.rst_nodes.isEmpty){
      // tie off core soft reset
      for(node <- core_rst_nodes){
        node.out.head._1 := false.B.asAsyncReset
      }
    }
    val pL3MSHRs = WireInit(0.U(64.W))
    ExcitingUtils.addSink(pL3MSHRs, "DSE_L3MSHRS")
    val pL3Sets = WireInit(0.U(64.W))
    ExcitingUtils.addSink(pL3Sets, "DSE_L3SETS")
    if(l3cacheOpt.isDefined){
      l3cacheOpt.get.module.io.mshrs := pL3MSHRs
      l3cacheOpt.get.module.io.sets := pL3Sets
    }


    misc.module.debug_module_io.resetCtrl.hartIsInReset := core_with_l2.map(_.module.reset.asBool)
    misc.module.debug_module_io.clock := io.clock
    misc.module.debug_module_io.reset := misc.module.reset

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

    withClockAndReset(io.clock.asClock, true_reset_sync) {
      // Modules are reset one by one
      // reset ----> SYNC --> {SoCMisc, L3 Cache, Cores}
      val resetChain = Seq(Seq(misc.module) ++ l3cacheOpt.map(_.module) ++ core_with_l2.map(_.module))
      ResetGen(resetChain, true_reset_sync, !debugOpts.FPGAPlatform)
    }

    dseCtrl.module.io.clk := io.clock.asClock
    dseCtrl.module.io.rst := io.dse_rst
//    core_with_l2.head.module.io.robSize := dseCtrl.module.io.robSize
    dseCtrl.module.io.instrCnt := io.instrCnt

    val instrCnt_sink = Wire(UInt(64.W))
    ExcitingUtils.addSink(instrCnt_sink, "DSE_INSTRCNT")
    io.instrCnt := instrCnt_sink

  }

  lazy val module = new XSTopImp(this)
}

object TopMain extends App {
  val (config, firrtlOpts, firtoolOpts) = ArgParser.parse(args)

  // tools: init to close dpi-c when in fpga
  val envInFPGA = config(DebugOptionsKey).FPGAPlatform

  val soc = DisableMonitors(p => LazyModule(new XSTop()(p)))(config)
  Generator.execute(firrtlOpts, soc.module, firtoolOpts)
  FileRegisters.write(fileDir = "./build", filePrefix = "XSTop.")
}
