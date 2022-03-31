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

import chipsalliance.rocketchip.config.{Config, Parameters}
import chisel3.stage.ChiselGeneratorAnnotation
import chisel3._
import device.{AXI4RAMWrapper, SimJTAG}
import freechips.rocketchip.diplomacy.{DisableMonitors, LazyModule, LazyModuleImp}
import utils.GTimer
import xiangshan.{DebugOptions, DebugOptionsKey}
import chipsalliance.rocketchip.config._
import freechips.rocketchip.devices.debug._
import difftest._
import freechips.rocketchip.util.ElaborationArtefacts
import top.TopMain.writeOutputFile

class SimTop(implicit p: Parameters) extends Module {
  val debugOpts = p(DebugOptionsKey)
  val useDRAMSim = debugOpts.UseDRAMSim

  val l_soc = LazyModule(new XSTop())
  val soc = Module(l_soc.module)
  // Don't allow the top-level signals to be optimized out,
  // so that we can re-use this SimTop for any generated Verilog RTL.
  dontTouch(soc.io)

  l_soc.module.dma <> 0.U.asTypeOf(l_soc.module.dma)

  val l_simMMIO = LazyModule(new SimMMIO(l_soc.misc.peripheralNode.in.head._2))
  val simMMIO = Module(l_simMMIO.module)
  l_simMMIO.io_axi4 <> soc.peripheral

  if(!useDRAMSim){
    val l_simAXIMem = LazyModule(new AXI4RAMWrapper(
      l_soc.misc.memAXI4SlaveNode, 8L * 1024 * 1024 * 1024, useBlackBox = true
    ))
    val simAXIMem = Module(l_simAXIMem.module)
    l_simAXIMem.io_axi4 <> soc.memory
  }

  soc.io.clock := clock.asBool
  soc.io.reset := reset.asBool
  soc.io.extIntrs := simMMIO.io.interrupt.intrVec
  soc.io.sram_config := 0.U
  soc.io.pll0_lock := true.B
  soc.io.cacheable_check := DontCare

  val success = Wire(Bool())
  val jtag = Module(new SimJTAG(tickDelay=3)(p)).connect(soc.io.systemjtag.jtag, clock, reset.asBool, !reset.asBool, success)
  soc.io.systemjtag.reset := reset
  soc.io.systemjtag.mfr_id := 0.U(11.W)
  soc.io.systemjtag.part_number := 0.U(16.W)
  soc.io.systemjtag.version := 0.U(4.W)

  val io = IO(new Bundle(){
    val logCtrl = new LogCtrlIO
    val perfInfo = new PerfInfoIO
    val uart = new UARTIO
    val memAXI = if(useDRAMSim) soc.memory.cloneType else null
  })

  simMMIO.io.uart <> io.uart

  if(useDRAMSim){
    io.memAXI <> soc.memory
  }

  if (!debugOpts.FPGAPlatform && (debugOpts.EnableDebug || debugOpts.EnablePerfDebug)) {
    val timer = GTimer()
    val logEnable = (timer >= io.logCtrl.log_begin) && (timer < io.logCtrl.log_end)
    ExcitingUtils.addSource(logEnable, "DISPLAY_LOG_ENABLE")
    ExcitingUtils.addSource(timer, "logTimestamp")
  }

  if (!debugOpts.FPGAPlatform && debugOpts.EnablePerfDebug) {
    val clean = io.perfInfo.clean
    val dump = io.perfInfo.dump
    ExcitingUtils.addSource(clean, "XSPERF_CLEAN")
    ExcitingUtils.addSource(dump, "XSPERF_DUMP")
  }

  // Check and dispaly all source and sink connections
  ExcitingUtils.fixConnections()
  ExcitingUtils.checkAndDisplay()
}

object SimTop extends App {
  override def main(args: Array[String]): Unit = {
    // Keep this the same as TopMain except that SimTop is used here instead of XSTop
    val (config, firrtlOpts, firrtlComplier) = ArgParser.parse(args)
    Generator.execute(
      firrtlOpts,
      DisableMonitors(p => new SimTop()(p))(config),
      firrtlComplier
    )
    ElaborationArtefacts.files.foreach{ case (extension, contents) =>
      writeOutputFile("./build", s"XSTop.${extension}", contents())
    }
  }
}
